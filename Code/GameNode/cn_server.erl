%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% No longer starts the gn_servers. 
%%% Instead links to them in a similar manner to the graphics setup
%%% @end
%%% Created : 13. Jul 2025 23:41
%%%-------------------------------------------------------------------
-module(cn_server).
-author("dolev").

-behaviour(gen_server).

% API
-export([start_link/1]).
-import(gn_server, [generate_atom_table_names/2]). % to not have to specify the import everytime

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include_lib("mnesia_records.hrl").
%% Linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%-include_lib("src/clean-repo/Code/Objects/object_records.hrl").
%% Windows compatible - Fixed relative paths
-include("../Objects/object_records.hrl").
-include("../common_parameters.hrl").

% Required module for QLC
-include_lib("stdlib/include/qlc.hrl").

-record(gn_data, {
    pid,
    tiles,
    bombs,
    powerups,
    players
    }).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Initialization of cn_server. *Registers globally* as "cn_server", maximal priority on CN node
start_link(GN_playmode_list) ->
    % * GN_playmode_list = [{1, true}, {2, false}, ... ]
    gen_server:start_link({global, ?MODULE}, ?MODULE, [GN_playmode_list], [{priority, max}]). 

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc Initializes all 4 gn servers, assuring proper creation ({Ok, Pid}) and monitoring them (timeout after 20 sec as backup),
%% The data stored is a list of records, each record contains the names (atoms) of the mnesia tables the CN shares with him
%% Accessing the name can be in 2 ways:
%% "Nameless": lists:nth(2, CN_data)#gn_data.players OR
%% "named" (records in the list are named, [Gn1_names, Gn2_names, ...] ): Gn2_names#gn_data.players
init([GN_playmode_list]) -> % [ {GN_number, Answer, NodeID} , {..} ]
    process_flag(trap_exit, true), % set to trap exits of GNs
    %% Attempt to monitor the processes right after initialization
    erlang:send_after(0, self(), {monitor_GNs, GN_playmode_list}),

    CN_data = none, %! initialize WITHOUT any data
    {ok, CN_data}.

%%%================== handle call ==================
%% @doc 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%%================== handle cast ==================

%% @doc Handling forwarding requests
handle_cast({forward_request, Destination, Request}, State) ->
    %% * forward requests look like {forward_request, Destination_GN_name, Request={..} }
    %% * Sends the new message as {forwarded, Request={..}}
    gen_server:cast(Destination, {forwarded, Request}),
    {noreply, State};

%% @doc Handling checks to switch GNs
handle_cast({query_request, AskingGN, Request}, State) ->
    io:format("🔄 CN SERVER: Received query_request from ~p: ~p~n", [AskingGN, Request]),
    %% * this below is the Request's contents
    case Request of
        {move_request_out_of_bounds, player, PlayerNum, [X,Y]=Destination_coord, Direction} ->
        %% * finds which GN oversees the coordinate, extract from Player's mnesia table the relevant buffs
        %% * pass the appropriate GN the message:
        %% * {forwarded, {move_request_out_of_bounds, player, {playerNum, Destination_coord, Direction, [relevant buffs], AskingGN}
            TargetGN = req_player_move:get_managing_node_by_coord(X, Y),
            
            %% Get the asking GN's data to read the player record (player belongs to asking GN, not target GN)
            AskingGNIndex = req_player_move:node_name_to_number(AskingGN),
            AskingGNData = lists:nth(AskingGNIndex, State),
            Players_table = AskingGNData#gn_data.players,
            Player_record = req_player_move:read_player_from_table(PlayerNum, Players_table),
            case erlang:is_record(Player_record, mnesia_players) of
                true -> 
                    io:format("🔄 CN SERVER: Forwarding player ~p move request from ~p to ~p at coordinates ~p~n", [PlayerNum, AskingGN, TargetGN, Destination_coord]),
                    gn_server:cast_message(TargetGN,
                        {move_request_out_of_bounds, player,
                            {PlayerNum, Destination_coord, Direction, Player_record#mnesia_players.special_abilities, AskingGN}
                    });   
                false ->
                    io:format("❌ CN SERVER: Player ~p not found in asking GN ~p table, Player_record: ~p~n", [PlayerNum, AskingGN, Player_record]),
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State};

        {handle_bomb_explosion, Coord, Radius} ->
            %% handled in a side function
            %% Calculates affected coordinates, then sends damage_taken messages to all objects impacted
            io:format("💣 CN SERVER: Processing bomb explosion at ~p with radius ~p~n", [Coord, Radius]),
            bomb_explosion_handler(Coord, Radius),
            {noreply, State};

        {ignite_bomb_request, PlayerNum} ->
            RemoteBombs = bomb_helper_functions:find_remote_bombs_for_player(player_fsm:get_player_pid(PlayerNum)),
            lists:foreach(fun(BombRecord) ->
                % Send ignite message to each remote bomb
                case BombRecord#mnesia_bombs.pid of
                    Pid when is_pid(Pid) ->
                        bomb_as_fsm:ignite_bomb(Pid);
                    _ ->
                        io:format("Warning: Invalid bomb PID for remote bomb at ~p~n", [BombRecord#mnesia_bombs.position])
                end
            end, RemoteBombs),
            %% Notify Player FSM about the ignition
            case RemoteBombs of
                [] -> player_fsm:gn_response(PlayerNum, {ignite_result, denied});
                _  -> player_fsm:gn_response(PlayerNum, {ignite_result, accepted})
            end,
            {noreply, State}
    end;

%% * handles a player transfer from one GN to another
handle_cast({transfer_records, player, PlayerNum, Current_GN, New_GN}, State) ->
    Current_GNIndex = req_player_move:node_name_to_number(Current_GN),
    New_GNIndex = req_player_move:node_name_to_number(New_GN),
    Current_GNData = lists:nth(Current_GNIndex, State),
    New_GNData = lists:nth(New_GNIndex, State),
    Current_GN_players_table = Current_GNData#gn_data.players,
    New_GN_players_table = New_GNData#gn_data.players,
    case transfer_player_records(PlayerNum, Current_GN_players_table, New_GN_players_table) of
        {error, not_found} -> erlang:error(transfer_player_failed, [node(), PlayerNum]);
        ok -> 
            %% Message the new GN to check for collisions
            gn_server:cast_message(New_GN,{incoming_player, PlayerNum})
    end,
    {noreply, State};

%% * Update active bombs in mnesia table and notify controlling player_fsm
handle_cast({player_bomb_exploded, PlayerPid}, State) ->
    update_player_active_bombs(PlayerPid),
    {noreply, State};


%% @doc General cast messages - as of now ignored.
handle_cast(_Msg, State) ->
    {noreply, State}.


%%%================== handle info ==================
%% @doc Initialization of GN_data - sets up the links to all gn_servers
handle_info({monitor_GNs, GN_playmode_list}, IrreleventState) ->
    GN_pids_list = link_GNs_loop(lists:seq(1, length(GN_playmode_list))),
    CN_data = lists:map(
        fun(Index) -> 
            Individual_table_names = generate_table_names(Index),
            PidA = lists:nth(Index, GN_pids_list),
            [TilesTable, BombsTable, PowerupsTable, PlayersTable] = Individual_table_names,
            #gn_data{
                pid = PidA,
                tiles = TilesTable,
                bombs = BombsTable,
                powerups = PowerupsTable,
                players = PlayersTable
            }
        end, lists:seq(1,4)),
    io:format("Successfully linked to all gn_servers ~w~n", [GN_pids_list]),
    %% Store the CN data in the state
    {noreply, CN_data};

%% @doc Receiving ready message from cn_server_graphics
handle_info({graphics_ready, _GraphicsPid}, State) ->
    io:format("**CN_SERVER: Graphics server is ready~n"),
    %% Notify all GN servers to start the game
    lists:foreach(fun(#gn_data{pid = Pid}) ->
        io:format("**CN_SERVER: Sending start_game to GN server: ~p~n", [Pid]),
        Pid ! start_game
    end, State),
    {noreply, State};

%% @doc Handles failure messages from the monitored processes
handle_info({'DOWN', Ref, process, Pid, Reason} , Data=[GN1=#gn_data{}, GN2=#gn_data{}, GN3=#gn_data{}, GN4=#gn_data{}]) -> 
    %% todo: placeholder - deal with failing nodes/processes
    io:format("*CN: monitored process ~w with ref ~w failed, reason:~w~n",[Pid,Ref,Reason]),
    {noreply, Data};

%% @doc General messages received as info - as of now ignored.
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc 
terminate(_Reason, _State) ->
    ok.

%% @doc 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_table_names(GN) ->
    [generate_atom_table_names(GN, "_tiles"), generate_atom_table_names(GN, "_bombs"),
        generate_atom_table_names(GN, "_powerups"), generate_atom_table_names(GN, "_players")].



%% ? I used this in earlier iteration, but changed the code where it was needed. Remove this comment if its used after-all
find_pid_by_node(TargetNode, GNList) ->
    case lists:filter(
        fun(#gn_data{pid = Pid}) -> node(Pid) =:= TargetNode end, GNList) of
        
        [#gn_data{pid = Pid}] -> Pid;
        _ -> pid_not_found
    end.


%% @doc Transfers a player's mnesia table from one GN to another
transfer_player_records(PlayerNum, Current_GN_table, New_GN_table) ->
    Fun = fun() ->
        %% Read entry from current GN
        case mnesia:read(Current_GN_table, PlayerNum, read) of
            [Record] ->
                %% delete from table from the GN we are leaving
                ok = mnesia:delete(Current_GN_table, Record, write),
                %% Write the data to the new GN's table
                mnesia:write(New_GN_table, Record, write);
            [] ->
                {error, not_found}
        end
    end,
    mnesia:activity(transaction, Fun).

bomb_explosion_handler(Coord, Radius) ->
    ResultList = case calculate_explosion_reach(Coord, Radius) of
        {atomic, Result} -> 
            io:format("💥 EXPLOSION_HANDLER: Got atomic result with ~p coordinates~n", [length(lists:flatten(Result))]),
            Result;
        Result when is_list(Result) -> 
            io:format("💥 EXPLOSION_HANDLER: Got list result with ~p coordinates~nFull list is:~p~n", [length(Result), Result]),
            Result;
        Other -> 
            io:format("ERROR: Unexpected result from calculate_explosion_reach: ~p~n", [Other]),
            throw({unexpected_explosion_result, Other})
    end,
    %% * ResultList looks like [ [X,Y], [X,Y], [X,Y] ] with duplicates on origin coordinate
    %% ResultList can be passed to the graphics server so it knows where to show an explosion
    FlattenedCoords = lists:usort(ResultList),
    io:format("💥 EXPLOSION_HANDLER: Sending ~p coordinates to graphics: ~p~n", [length(FlattenedCoords), FlattenedCoords]),
    cn_server_graphics:show_explosion(FlattenedCoords),
    %% Sends inflict_damage messages to all objects affected by the explosion
    notify_affected_objects(ResultList). % TODO: something doesnt work here

calculate_explosion_reach([X, Y], Max_range) ->
    Fun = fun() -> calculate_affected([X, Y], Max_range) end,
	mnesia:activity(transaction, Fun).


-spec calculate_affected(Center::list(), Radius::integer()) -> list().
calculate_affected([X,Y] = Center, Radius) ->
	North = {0, 1}, South = {0, -1}, East = {1, 0}, West = {-1, 0},
	
	EmptyResults = {[], [], [], []},
	
	{CenterIndex, _} = req_player_move:get_gn_number_by_coord(X,Y),
	IntermedResults = erlang:setelement(CenterIndex, EmptyResults, [Center]),
	
	NorthResults = trace_ray(Center, North, Radius, IntermedResults),
	SouthResults = trace_ray(Center, South, Radius, IntermedResults),
	EastResults = trace_ray(Center, East, Radius, IntermedResults),
	WestResults = trace_ray(Center, West, Radius, IntermedResults),
	
	merge_results(NorthResults, SouthResults, EastResults, WestResults).
	
	
merge_results({List1a, List1b, List1c, List1d}, {List2a, List2b, List2c, List2d}, 
            {List3a, List3b, List3c, List3d}, {List4a, List4b, List4c, List4d}) ->
	[List1a ++ List2a ++ List3a ++ List4a, 
    List1b ++ List2b ++ List3b ++ List4b, 
    List1c ++ List2c ++ List3c ++ List4c, 
    List1d ++ List2d ++ List3d ++ List4d].
	

%% end of recursion - reverse the lists.
trace_ray(_CurCoord, _Direction, 0, {List1, List2, List3, List4}) -> 
	{lists:reverse(List1), lists:reverse(List2), lists:reverse(List3), lists:reverse(List4)};

trace_ray([X, Y], {PlusX, PlusY}=Direction, StepsLeft, Accums) ->
	[NextX, NextY] = [X + PlusX, Y + PlusY],
	{TableIndex, TableName} = req_player_move:get_gn_number_by_coord(NextX, NextY),
	
	case mnesia:read(TableName, [NextX, NextY]) of
		[] -> % no tile found
			UpdatedList = erlang:element(TableIndex, Accums),
			NewAccums = erlang:setelement(TableIndex, Accums, [ [NextX, NextY] | UpdatedList]),
			trace_ray([NextX, NextY], Direction, StepsLeft-1, NewAccums);
		[_] -> % found a tile
			UpdatedList = erlang:element(TableIndex, Accums),
			NewAccums = erlang:setelement(TableIndex, Accums, [ [NextX, NextY] | UpdatedList]),
			trace_ray([NextX, NextY], Direction, 0, NewAccums) % go to the end of the recursion
	end.

%% Handler for letting all objects be affected by the explosion in the affected coordinates list
notify_affected_objects(ResultList) ->
    lists:foreach(fun(Coord) -> spawn(process_affected_objects(Coord)) end, ResultList).

process_affected_objects(Coords) ->
    Fun = fun() -> process_single_coord(Coords) end,
    mnesia:activity(read_only, Fun).

process_single_coord(Coord) ->
    %% using QLC queries to make this faster
    TilesPids = qlc:e(
        qlc:append([
            qlc:q([T#mnesia_tiles.pid || T <- mnesia:table(gn1_tiles), T#mnesia_tiles.position == Coord]),
            qlc:q([T#mnesia_tiles.pid || T <- mnesia:table(gn2_tiles), T#mnesia_tiles.position == Coord]),
            qlc:q([T#mnesia_tiles.pid || T <- mnesia:table(gn3_tiles), T#mnesia_tiles.position == Coord]),
            qlc:q([T#mnesia_tiles.pid || T <- mnesia:table(gn4_tiles), T#mnesia_tiles.position == Coord])
        ])),
    BombsPids = qlc:e(
        qlc:append([
            qlc:q([B#mnesia_bombs.pid || B <- mnesia:table(gn1_bombs), B#mnesia_bombs.position == Coord]),
            qlc:q([B#mnesia_bombs.pid || B <- mnesia:table(gn2_bombs), B#mnesia_bombs.position == Coord]),
            qlc:q([B#mnesia_bombs.pid || B <- mnesia:table(gn3_bombs), B#mnesia_bombs.position == Coord]),
            qlc:q([B#mnesia_bombs.pid || B <- mnesia:table(gn4_bombs), B#mnesia_bombs.position == Coord])
        ])),
    PlayersPids = qlc:e(
        qlc:append([
            qlc:q([P#mnesia_players.pid || P <- mnesia:table(gn1_players), P#mnesia_players.position == Coord]),
            qlc:q([P#mnesia_players.pid || P <- mnesia:table(gn2_players), P#mnesia_players.position == Coord]),
            qlc:q([P#mnesia_players.pid || P <- mnesia:table(gn3_players), P#mnesia_players.position == Coord]),
            qlc:q([P#mnesia_players.pid || P <- mnesia:table(gn4_players), P#mnesia_players.position == Coord])
        ])),

    % Send 'inflict damage' message to all affected objects, based on their type (bomb/player/tile)
    % io print of the affected objects
    io:format("💥 EXPLOSION at ~p affects ~p tiles, ~p bombs, and ~p players.~n",
              [Coord, length(TilesPids), length(BombsPids), length(PlayersPids)]),
    inflict_damage_handler(TilesPids, tile, damage_taken),
    inflict_damage_handler(BombsPids, bomb_as_fsm, damage_taken),
    inflict_damage_handler(PlayersPids, player_fsm, inflict_damage),
    ok.


inflict_damage_handler(PidsList, Module, Function) ->
    lists:foreach(fun(Pid) ->
        try
            _ = apply(Module, Function, [Pid]),
            io:format("💥 Sent damage to ~p: ~p:~p(~p)~n", [Pid, Module, Function, Pid])
        catch
            Class:Reason ->
                io:format(standard_error, "Error calling ~p:~p(~p). Class: ~p, Reason: ~p~n", [Module, Function, Pid, Class, Reason])
        end
    end, PidsList),
    ok.


update_player_active_bombs(PlayerNumber) ->
    Tables = [gn1_players, gn2_players, gn3_players, gn4_players],
    Fun = fun() -> find_in_player_tables_by_number(PlayerNumber, Tables) end,
    Result = case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end,
    case Result of
        not_found ->
            erlang:error(player_not_found, [PlayerNumber]);
        {table_updated, Player_record} ->
            %% Notify player_fsm of this change directly
            player_fsm:bomb_exploded(Player_record#mnesia_players.pid)
    end.


find_in_player_tables_by_number(_PlayerNumber, []) -> not_found;
find_in_player_tables_by_number(PlayerNumber, [Table| T]) ->
    case mnesia:index_read(Table, PlayerNumber, player_number) of
        [Record] ->  % update active bombs in mnesia table
            UpdatedRecord = Record#mnesia_players{bombs_placed = Record#mnesia_players.bombs_placed - 1},
            mnesia:write(Table, UpdatedRecord, write),
            {table_updated, Record};
        [] -> find_in_player_tables_by_number(PlayerNumber, T)
    end.


%% Functions used to link the game nodes
link_GNs_loop(NodeNumbers) ->
    io:format("Attempt to link to all gn_servers..~n"),
    Pids = lists:map(fun(NodeNum) ->
       GN_server_name = list_to_atom("GN" ++ integer_to_list(NodeNum) ++ "_server"),
       link_with_retry(GN_server_name, 0)
    end, NodeNumbers),
    Pids. % return list of linked PIDs

link_with_retry(GN_server_name, RetryCount) when RetryCount > 8 ->
    erlang:error({link_failed_after_retries, GN_server_name, RetryCount});
link_with_retry(GN_server_name, RetryCount) ->
    Pid = global:whereis_name(GN_server_name),
    case Pid of
        undefined ->
            io:format("Process ~p not found globally, attempt ~w/8, retrying...~n", [GN_server_name, RetryCount + 1]),
            timer:sleep(500),
            link_with_retry(GN_server_name, RetryCount + 1);
        _ ->
            try
                link(Pid),
                io:format("Successfully linked to ~p~n", [GN_server_name]),
                Pid
            catch
                _:_ ->
                    io:format("Failed to link to ~p, attempt ~w/8, retrying...~n", [GN_server_name, RetryCount + 1]),
                    timer:sleep(500),
                    link_with_retry(GN_server_name, RetryCount + 1)
            end
    end.
