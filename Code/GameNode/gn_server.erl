%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2025 11:19
%%%-------------------------------------------------------------------
-module(gn_server).
-author("dolev").

-behaviour(gen_server).

%% API
-export([start_link/1, generate_atom_table_names/2, cast_message/2]).

-export([get_registered_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include_lib("mnesia_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
%%% Linux compatible
%-include_lib("src/clean-repo/Code/Objects/object_records.hrl").
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%%% Windows compatible - Fixed relative paths
-include("../Objects/object_records.hrl").
%% common_parameters.hrl is already included via mnesia_records.hrl

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers name globally as 'GNx_server', priority set to high 
-spec(start_link({GN_number::integer(), IsBot::boolean()}) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({GN_number, IsBot}) ->
    GN_name = list_to_atom("GN" ++ integer_to_list(GN_number) ++ "_server"),
    gen_server:start_link({global, GN_name}, ?MODULE, [GN_number, IsBot], [{priority, high}]).

%% Sending a message to GN using API call
cast_message(GN_Name, Message) ->
    case global:whereis_name(GN_Name) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Message);
        _ ->
            io:format("**##** GN ~p: Not found**##**~n", [GN_Name])
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initializes the server
-spec(init(list()) ->
    {ok, State :: #gn_state{}} | {ok, State :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([GN_number, PlayerType]) ->
    Data = #gn_state{
        tiles_table_name = generate_atom_table_names(GN_number, "_tiles"),
        bombs_table_name = generate_atom_table_names(GN_number, "_bombs"),
        powerups_table_name = generate_atom_table_names(GN_number, "_powerups"),
        players_table_name = generate_atom_table_names(GN_number, "_players")},
    %% ! HOTFIX: check if tables are created before trying to access them
    %% Wait for all tables to be created before proceeding
    AllTableNames = [
        Data#gn_state.tiles_table_name,
        Data#gn_state.bombs_table_name,
        Data#gn_state.powerups_table_name,
        Data#gn_state.players_table_name
    ],
    io:format("CN: Waiting for tables to be ready: ~p~n", [AllTableNames]),
    case mnesia:wait_for_tables(AllTableNames, 10000) of
        ok -> 
            io:format("✅ All mnesia tables created successfully~n");
        {timeout, BadTabs} -> 
            io:format("❌ Timeout waiting for tables: ~p~n", [BadTabs]),
            error({mnesia_tables_timeout, BadTabs});
        {error, Reason} -> 
            io:format("❌ Error waiting for tables: ~p~n", [Reason]),
            error({mnesia_tables_error, Reason})
    end,
    %% Initialize tiles and players from the generated map
    initialize_tiles(Data#gn_state.tiles_table_name),
    initialize_players(Data#gn_state.players_table_name, PlayerType, GN_number),
    {ok, Data}.

%%% ============== Handle call ==============

%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gn_state{}) ->
    {reply, Reply :: term(), NewState :: #gn_state{}} |
    {reply, Reply :: term(), NewState :: #gn_state{}, timeout() | hibernate} |
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #gn_state{}} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
handle_call({set_player_pid, PlayerPid}, _From, State) ->
    % Handle player PID registration
    io:format("Setting player PID: ~p~n", [PlayerPid]),
    {reply, ok, State};

handle_call(_Request, _From, State = #gn_state{}) ->
    {reply, ok, State}.

%%% ============== Handle cast ==============

-spec(handle_cast(Request :: term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
%% @doc Handle player requests
handle_cast({player_message, Request}, State = #gn_state{}) ->
    ThisGN = get_registered_name(self()),
    io:format("**##** GN ~p: Received player message ~p**##**~n", [ThisGN, Request]),
    case Request of
        %% * Player movement request mechanism
        {move_request, PlayerNum, ThisGN , Direction} -> % move request from a player in our quarter
            io:format("DEBUG GN_SERVER: Processing move request for Player ~p, Direction ~p~n", [PlayerNum, Direction]),
            Move_verdict = req_player_move:handle_player_movement(PlayerNum, Direction, State),
            io:format("DEBUG GN_SERVER: Move verdict: ~p~n", [Move_verdict]),
            case Move_verdict of
                can_move ->
                    io:format("DEBUG GN_SERVER: Player can move, updating movement~n"),
                    req_player_move:insert_player_movement(PlayerNum, State#gn_state.players_table_name, Direction),
                    player_fsm:gn_response(PlayerNum, {move_result, Move_verdict});
                cant_move ->
                    io:format("DEBUG GN_SERVER: Player can't move, updating direction to none~n"),
                    req_player_move:update_player_direction(PlayerNum, State#gn_state.players_table_name, none),
                    player_fsm:gn_response(PlayerNum, {move_result, Move_verdict});
                dest_not_here ->
                    io:format("DEBUG GN_SERVER: Destination not here, forwarding to CN~n"),
                    %% extracts the player's record from mnesia's player table
                    Player = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
                    %% Calculate destination coordinates
                    Destination_coord = req_player_move:calc_new_coordinates(Player#mnesia_players.position, Direction),
                    gn_server:cast_message(cn_server,
                        {query_request, get_registered_name(self()), 
                            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}})
            end,
            {noreply, State};
        {move_request, PlayerNum, TargetGN, Direction} -> % move request from a player outside my quarter
            gn_server:cast_message(cn_server, {forward_request, TargetGN, {move_request, player, PlayerNum, Direction}}),
            {noreply, State};

        %% * Player requesting to place bombs mechanism
        {place_bomb_request, PlayerNum, ThisGN} -> % place bomb request from a player in our quarter
            case bomb_helper_functions:place_bomb(PlayerNum, State#gn_state.players_table_name, State#gn_state.bombs_table_name) of
                bomb_placed ->
                    %% bomb was placed successfully, added into mnesia table by helper function
                    %% Notify player FSM of successful placement
                    player_fsm:gn_response(PlayerNum, {bomb_result, accepted});
                Error ->
                    %% Notify player FSM of failed placement
                    %% Error logging for debugging
                    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
                    io:format("[~2..0B:~2..0B:~2..0B]: Failed to place bomb for player ~p: ~p~n", [Hour, Min, Sec, PlayerNum, Error]),
                    error_logger:info_msg("Failed to place bomb for player ~p: ~p", [PlayerNum, Error]),
                    player_fsm:gn_response(PlayerNum, {bomb_result, denied})
            end,
            {noreply, State};
        {place_bomb_request, PlayerNum, TargetGN} -> % place bomb request from a player outside my quarter
            gn_server:cast_message(cn_server, {forward_request, TargetGN, {place_bomb_request, PlayerNum, ThisGN}}),
            {noreply, State};

        %% * Player requesting to ignite remote bombs
        %% Must check if any of his bombs are 'remote' type, and if any of them are - ignite them
        %% This takes place within cn_server, as the bombs can be across all quarters on theory
        {ignite_bomb_request, PlayerNum} ->
            gn_server:cast_message(cn_server, {query_request, self(), {ignite_bomb_request, PlayerNum}}),
            {noreply, State};

        %% * Cooldown updates
        {cooldown_update, ThisGN, UpdateContent} ->
            req_player_move:update_player_cooldowns(UpdateContent, State#gn_state.players_table_name),
            {noreply, State};
        {cooldown_update, TargetGN, UpdateContent} ->
            gn_server:cast_message(cn_server, {forward_request, TargetGN, {cooldown_update, TargetGN, UpdateContent}}),
            {noreply, State}
    end;

handle_cast({forwarded, Request}, State = #gn_state{}) ->
    %% * handles forwarded messages
    io:format("DEBUG FORWARDED: Received forwarded message: ~p~n", [Request]),
    case Request of
        {move_request, player, PlayerNum, Direction} ->
            %% *  handles a move request of a player inside my quarter whose FSM is on another node
            %% extracts the player's record from mnesia's player table
            Player = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            %% Calculate destination coordinates
            Destination_coord = req_player_move:calc_new_coordinates(Player#mnesia_players.position, Direction),

            case req_player_move:handle_player_movement(PlayerNum, Direction, State) of
                can_move -> 
                    %% move is possible. Update data, open halfway timer, respond to player FSM
                    req_player_move:insert_player_movement(PlayerNum, State#gn_state.players_table_name),
                    %% respond to the player FSM via CN->hosting GN
                    gn_server:cast_message(cn_server,
                        {forward_request, Player#mnesia_players.local_gn, 
                            {gn_answer, {move_result, player, PlayerNum, accepted}}
                        });
                cant_move -> % can't move, obstacle blocking
                    req_player_move:update_player_direction(PlayerNum, State#gn_state.players_table_name, none),
                    gn_server:cast_message(cn_server,
                        {forward_request, Player#mnesia_players.local_gn, 
                            {gn_answer, {move_result, player, PlayerNum, denied}}
                        });
                dest_not_here -> % destination coordinate is overseen by another GN
                    gn_server:cast_message(cn_server,
                        {query_request, get_registered_name(self()), 
                            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}})
            end,
            {noreply, State};
        
        %% * A GN who hosts a player (physically) receives a response for his movement request
        {gn_answer, {move_result, player, PlayerNum, Answer}} ->
            %% Pass the message to the Player FSM
            %% Look for the player in your own records (to find his Pid)
            Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            case erlang:is_record(Player_record, mnesia_players) of 
                true -> 
                    %% Everything as normal (found the record), pass the message
                    player_fsm:gn_response(PlayerNum, {move_result, Answer});
                false -> % crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State};

        %% * A GN got a respond for a movement request of a player/bomb to another quarter (separate pattern-matching) - this is the forwarded reply handler
        %% * This deals with situations where the Player FSM is on the same node as the relevant GN as well as when its on another
        {movement_clearance, player, PlayerNum, Answer, Direction} ->
            io:format("DEBUG ASKING GN: Received movement_clearance for player ~p: ~p with direction ~p~n", [PlayerNum, Answer, Direction]),
            req_player_move:handle_player_movement_clearance(PlayerNum, Answer, Direction, State#gn_state.players_table_name),
            {noreply, State};

        {movement_clearance, bomb, BombIdentifier, Answer} -> % todo: implement bomb movement clearance
            req_player_move:handle_bomb_movement_clearance(BombIdentifier, Answer, State#gn_state.bombs_table_name),
            {noreply, State};

        %% * A player has changed coordinates, resulting in a target GN change.
        %% * this message is sent by the previous GN to let the player FSM update his target
        {new_target_gn, player, PlayerNum, New_GN} ->
            player_fsm:update_target_gn(PlayerNum, New_GN);

        {cooldown_update, _ThisGN, UpdateContent} ->
            req_player_move:update_player_cooldowns(UpdateContent, State#gn_state.players_table_name),
            {noreply, State};

         %% * Player requesting to place bombs
        {place_bomb_request, PlayerNum, AskingGN} ->
            case bomb_helper_functions:place_bomb(PlayerNum, State#gn_state.players_table_name, State#gn_state.bombs_table_name) of
                bomb_placed ->
                    %% bomb was placed successfully, added into mnesia table by helper function
                    %% Player is NOT physically no our machine - forward the response through cn server to local gn server
                    gn_server:cast_message(cn_server, {forward_request, AskingGN, {bomb_result, accepted}});
                Error ->
                    %% Notify player FSM of failed placement
                    %% Error logging for debugging
                    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
                    io:format("[~2..0B:~2..0B:~2..0B]: Failed to place bomb for player ~p: ~p~n", [Hour, Min, Sec, PlayerNum, Error]),
                    error_logger:info_msg("Failed to place bomb for player ~p: ~p", [PlayerNum, Error]),
                    %% Player is NOT physically no our machine - forward the response through cn server to local gn server
                    gn_server:cast_message(cn_server, {forward_request, AskingGN, {PlayerNum, bomb_result, denied}})
            end,
            {noreply, State};

        %% * An answer from target GN for a bomb placement was forwarded back to us (local GN) - notify player FSM
        {PlayerNum, bomb_result, Answer} ->
            player_fsm:gn_response(PlayerNum, {bomb_result, Answer}),
            {noreply, State}
            
    end;


%% * received a move request to the quarter of this GN from another GN - differntiate bomb from player
handle_cast({move_request_out_of_bounds, EntityType, ActualRequest}, State) ->
    case EntityType of
        player -> % a player wants to pass to this GN
            {PlayerNum, Destination_coord, Direction, BuffsList, AskingGN} = ActualRequest,
            io:format("DEBUG TARGET GN: Received move request for player ~p to coordinate ~p from ~p~n", [PlayerNum, Destination_coord, AskingGN]),
            %% checks destination coordinate for obstacles (if the move is possible),
            %% this should also "kickstart" any action caused by this attempted movement
            Move_result = req_player_move:check_for_obstacles(Destination_coord, BuffsList, Direction, State),
            io:format("DEBUG TARGET GN: Move result for player ~p: ~p~n", [PlayerNum, Move_result]),
            gn_server:cast_message(cn_server,
                {forward_request, AskingGN,
                    {movement_clearance, player, PlayerNum, Move_result, Direction}});
        bomb -> % a bomb wants to pass to this GN
            placeholder %! not yet implemented
    end,
    {noreply, State};


%% * A player came into my quarter of the map - open a timer, let the player FSM know
handle_cast({incoming_player, PlayerNum}, State) ->
    Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
    req_player_move:check_entered_coord(Player_record, State),
    {noreply, State};

%% * A tile updates his status (broken/transition to one_hit)
handle_cast({tile_update, Message, Position}, State = #gn_state{}) ->
    case Message of
        tile_breaking -> % tile broke from explosion, remove from table
            tile_helper_functions:break_tile(Position, State#gn_state.tiles_table_name);
        one_hit -> % tile was hit, but is of two_hit type - update in mnesia table
            ok = tile_helper_functions:update_to_one_hit(Position, State#gn_state.tiles_table_name) % ! should crash if tile not found
    end,
    {noreply, State};

%% * A bomb sent a message (Currently supporting timer update)
handle_cast({bomb_message, Message}, State = #gn_state{}) ->
    case Message of
        {update_timer, BombPid, NewTime} ->
            bomb_helper_functions:update_bomb_timer(BombPid, NewTime, State#gn_state.bombs_table_name),
            io:format("GN DEBUG: Updated bomb timer for ~p to ~p~n", [BombPid, NewTime])
    end,
    {noreply, State};

%% * this is a catch-all&ignore clause
handle_cast(_Request, State = #gn_state{}) -> 
    io:format("DEBUG CATCH-ALL: Unhandled cast message: ~p~n", [_Request]),
    {noreply, State}.

%%% ============== Handle info ==============

%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).


%% * Timer/counter for player movement has finished
%% * Checks if the player stays in current GNs boundaries:
%% *    If it changes GNs, transfer the players table entry to the new GN, and update the Player FSM of that change
%% * Regardless, checks destination for power-ups/explosions
handle_info({update_coord, player, PlayerNum}, State = #gn_state{}) ->
        case req_player_move:read_and_update_coord(player, PlayerNum, State#gn_state.players_table_name) of
            {retain_gn, Player_record} ->
                req_player_move:check_entered_coord(Player_record, State); %% Player FSM is updated through here

            {switch_gn, Current_GN, New_GN} ->
                %% transfer records to new GN
                gn_server:cast_message(cn_server,
                    {transfer_records, player, PlayerNum, Current_GN, New_GN}),
                %% Let player FSM know of the GN change
                player_fsm:update_target_gn(PlayerNum, New_GN);

            _ -> % ! got an error somewhere, crash the process. this is mostly for debugging as of now
                erlang:error(failure_when_updating_record, [node(), PlayerNum])
        end,
        {noreply, State};

%% * handle bomb explosions
handle_info({'DOWN', _Ref, process, Pid, {shutdown, exploded}}, State = #gn_state{}) ->
    %% Read and remove bomb from mnesia table. Pass record to cn_server to process explosion
    Record = case req_player_move:read_and_remove_bomb(Pid, State#gn_state.bombs_table_name) of
        R when is_record(R, mnesia_bombs) -> 
            R;
        Other -> 
            io:format("ERROR: read_and_remove_bomb returned unexpected value: ~p~n", [Other]),
            throw({unexpected_return, Other})
    end,
    gn_server:cast_message(cn_server, 
        {query_request, get_registered_name(self()), 
            {handle_bomb_explosion, Record#mnesia_bombs.position, Record#mnesia_bombs.radius}}),
    %% Update player's active bombs count, let playerFSM know.
    notify_owner_of_bomb_explosion(Record#mnesia_bombs.owner, State),
    {noreply, State};

%% * handle item creation request - from blown-up tiles
handle_info({create_item, Coord, ItemType}, State = #gn_state{}) ->
    %% Handle item creation logic here
    io:format("**GN SERVER: Received request to create item at coordinate ~p. Item type: ~p~n", [Coord, ItemType]),
    case tile_helper_functions:create_item(Coord, ItemType, State#gn_state.powerups_table_name) of
        {ok, Pid} ->
            io:format("**GN SERVER: Successfully created item of type ~p at ~p with PID ~p~n", [ItemType, Coord, Pid]),
            ok;
        {error, Reason} ->
            io:format("**GN SERVER ERROR: Failed to create item at ~p. Reason: ~p~n", [Coord, Reason])
    end,
    {noreply, State};

%% * Handle start-of-game message from CN - pass it to player_fsm to "unlock" it
handle_info(start_game, State) ->
    PlayerNumber = req_player_move:node_name_to_number(node()),
    io:format("**GN SERVER: received 'start_game' from CN - Trying to start game for player ~p~n", [PlayerNumber]),

    % Get the actual PID of the player FSM using its global name
    PlayerName = list_to_atom("player_" ++ integer_to_list(PlayerNumber)),
    case global:whereis_name(PlayerName) of
        undefined ->
            io:format("**GN SERVER ERROR: Player FSM ~p not found globally!~n", [PlayerName]);
        PlayerPid ->
            io:format("**GN SERVER: Sending game_start to player FSM ~p (PID: ~p)~n", [PlayerName, PlayerPid]),
            player_fsm:start_signal(PlayerPid)
    end,
    {noreply, State};

%% * default, catch-all and ignore
handle_info(_Info, State = #gn_state{}) ->
{noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gn_state{}) -> term()).
terminate(_Reason, _State = #gn_state{}) ->
ok.

%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gn_state{},
Extra :: term()) ->
{ok, NewState :: #gn_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gn_state{}, _Extra) ->
{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc returns the registered name of a Pid
get_registered_name(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        [] ->
            % Process is not locally registered, check global registry
            case global:registered_names() of
                Names ->
                    case lists:keyfind(Pid, 2, [{Name, global:whereis_name(Name)} || Name <- Names]) of
                        {Name, Pid} -> Name;
                        false -> undefined
                    end
            end;
        undefined -> undefined
    end.

%% @doc helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

initialize_tiles(TableName) ->
    Fun = fun() ->
        AllRecords = mnesia:match_object(TableName, #mnesia_tiles{_ = '_'}, read),
        lists:foreach(
            fun(Tile) ->
            % spawn a tile gen_server process for each tile in the table
                [Pos_x, Pos_y] = Tile#mnesia_tiles.position,
                [Type, Contains] = [Tile#mnesia_tiles.type, Tile#mnesia_tiles.contains],
                {ok, Pid} = tile:start_link(Pos_x, Pos_y, Type, Contains),
                UpdatedRecord = Tile#mnesia_tiles{pid = Pid},
                mnesia:write(TableName, UpdatedRecord, write)
                end,
            AllRecords), ok
        end,
    mnesia:activity(transaction, Fun).


-spec initialize_players(TableName:: atom(), PlayerIsBot:: boolean(), GN_number::1|2|3|4) -> term().
initialize_players(TableName, PlayerIsBot, GN_number) ->
    %% start io_handler gen_server
    {ok, IO_pid} = case PlayerIsBot of
        true -> % bot
            %% ? FOR NOW, DIFFICULTY IS SET TO 'EASY' BY DEFAULT. CAN BE RANDOMIZED?
            bot_handler:start_link(GN_number, easy);
        false -> % player
            io_handler:start_link(GN_number)
    end,
    %% Initialize player_fsm process based on data within mnesia table (as initialized whe map was created)
    Fun = fun() ->
        [PlayerRecord = #mnesia_players{}] = mnesia:read(TableName, GN_number),
        {ok, FSM_pid} = player_fsm:start_link(GN_number, self(), PlayerIsBot, IO_pid),
        %% Update mnesia record
        UpdatedRecord = PlayerRecord#mnesia_players{
            local_gn = self(),
            target_gn = self(), % by default starts at his own GN's quarter
            io_handler_pid = IO_pid,
            pid = FSM_pid,
            bot = PlayerIsBot},
        mnesia:write(TableName, UpdatedRecord, write)
        end,
    mnesia:activity(transaction, Fun).


notify_owner_of_bomb_explosion(OwnerID, State) ->
    %% Notifies owner for the bomb's explosion, as well as updates players' mnesia table for bombs_placed
    case OwnerID of
        none -> %default bomb, no player owner
            ok;
        Pid -> % there's a real player's Pid on the bomb
            Fun = fun() -> 
                Result = qlc:eval(qlc:q(
                    [ {player, P} || P <- mnesia:table(State#gn_state.players_table_name), P#mnesia_players.pid == Pid]
                )),
                case Result of
                    [{player, PlayerRecord}] -> %% update active bomb count in mnesia table
                        mnesia:write(State#gn_state.players_table_name, PlayerRecord#mnesia_players{bombs_placed = (PlayerRecord#mnesia_players.bombs_placed - 1)}, write);
                    [] -> ok % no player found
                end,
                Result % return list back from function
            end, % fun()'s "end"
            Result = case mnesia:activity(transaction, Fun) of
                {atomic, R} -> R;
                R -> R
            end,
            case Result of
                [{player, MatchingPlayerRecord}] -> % player found within GN
                    player_fsm:bomb_exploded(MatchingPlayerRecord#mnesia_players.pid);
                [] -> % player not found within GN, forward request to CN
                    %% send cn_server a request to update active bomb in mnesia, and also notify relevant fsm_player
                    gn_server:cast_message(cn_server,
                        {player_bomb_exploded, Pid});
                _ReturnValue -> % ! shouldn't happen - error out, this is for debugging
                    erlang:error(bad_return_value, [Pid, Result])
            end,
            ok
    end.
