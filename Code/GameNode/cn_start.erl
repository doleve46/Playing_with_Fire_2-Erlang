%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% New file holding all the CN startup logic and functionality
%%% @end
%%% Created : 25. Aug 2025 13:32
%%%-------------------------------------------------------------------
-module(cn_start).
-author("dolev").

%% API
-export([cn_bootstrap/1, discover_GNs/1, discover_GNs_improved/1]).
-export([start/1, initial_mnesia_load/1]).

%% ! NOTE: when terminating, need to use application:stop(mnesia)
%% ! NOTE: for super-massive debugging use sys:trace(Pid, true).

-define(INITIAL_STARTUP, cn_initial_startup).
-define(CN_STARTUP, cn_startup).
-include("mnesia_records.hrl").

%% --------------------------------------------------------------
%%%                 CN initial startup process
%%% This section contains all the code for the cn_initial_startup process
%% --------------------------------------------------------------
cn_bootstrap(IP_prefix) ->
    %% Discover all GNs in the local network
    GN_list = discover_GNs(IP_prefix),
    io:format("Discovered GN nodes: ~p~n", [GN_list]),
    spawn(cn_start, start, [GN_list]). % spawn cn_start process

%% @doc Attempting to communicate with all possible IPs in the local network, returning a the GNs in the network (nodes called GNx@192.168.1.Y )
discover_GNs(IP_prefix) ->
    Looking_for = ["GN1@", "GN2@", "GN3@", "GN4@"],
    lists:flatten(
        [
            case net_adm:names(IP_prefix ++ integer_to_list(X)) of % asks each IP for all nodes he operates
                {ok, Names} -> % IP responded with a lists of his erlang nodes
                    %% filter only nodes of our GNs
                    [NodeName || {Name, _Port} <- Names, NodeNameStr = Name ++ "@" ++ "Prefix" ++ integer_to_list(X), % reconstruct the node name
                lists:any(fun(Y) -> lists:prefix(Y, NodeNameStr) end, Looking_for),
                NodeName = list_to_atom(NodeNameStr),
                NodeName =/= node(), % not my own node
                net_adm:ping(NodeName) =:= pong % ping node
            ];
            _ -> [] % IP didn't respond, ignore it (probably doesn't hold our GNs)
            end || X <- lists:seq(3,253)]
        ).

%% Better network discovery using multicast or broadcast
discover_GNs_improved(IP_prefix) ->
    %% Option 1: Use epmd to query known nodes on specific IPs
    PossibleIPs = generate_ip_range(IP_prefix, 3, 253), % skip .1 (usually router)
    
    %% Parallel ping with timeout
    Self = self(),
    Pids = [spawn(fun() -> ping_ip_for_gns(IP, Self) end) || IP <- PossibleIPs],
    
    %% Collect results with timeout
    collect_gn_discoveries([], length(Pids), 3000). % 3 second timeout

ping_ip_for_gns(IP, ParentPid) ->
    case net_adm:names(IP) of
        {ok, Names} ->
            GNNodes = [list_to_atom(Name ++ "@" ++ IP) || 
                      {Name, _Port} <- Names,
                      lists:prefix("GN", Name),
                      list_to_atom(Name ++ "@" ++ IP) =/= node()],
            
            %% Test connectivity
            ConnectedGNs = [Node || Node <- GNNodes, net_adm:ping(Node) =:= pong],
            ParentPid ! {gn_discovery, ConnectedGNs};
        _ ->
            ParentPid ! {gn_discovery, []}
    end.

collect_gn_discoveries(Acc, 0, _Timeout) -> lists:flatten(Acc);
collect_gn_discoveries(Acc, Remaining, Timeout) ->
    receive
        {gn_discovery, Nodes} ->
            collect_gn_discoveries([Nodes | Acc], Remaining - 1, Timeout)
    after Timeout ->
        lists:flatten(Acc)
    end.

generate_ip_range(Prefix, Start, End) ->
    [Prefix ++ integer_to_list(X) || X <- lists:seq(Start, End)].
%% --------------------------------------------------------------
%%%                     CN startup process
%%% This section contains all the code for the cn_startup process
%% --------------------------------------------------------------
start(_GN_list) -> % currently GN_list is unsued, might be used later on.
    %% register process globally
    global:register_name(cn_start, self()),
    %% Enter loop for initial connections from GNs
    ConnectedNodeNames = initial_connections_loop(0, []),
    io:format("All GN nodes (gn_start) connected successfully: ~p~n", [ConnectedNodeNames]),
    %% Send message to all GN's gn_start to choose playmode (bot/human)
    lists:foreach(fun(NodeName) ->
        {gn_start, NodeName} ! {cn_start, {choose_playmode, are_you_bot}} end,
    ConnectedNodeNames),
    %% * From this point until the game actually starts, the GNs shouldn't be able to disconnect/crash.
    %% Initialize mnesia
    AllNodes = node() ++ ConnectedNodeNames,
    application:set_env(mnesia, dir, "/home/dolev/Documents/mnesia_files"), % ! Change directory based on PC running on, critical for CN
    mnesia:create_schema(AllNodes), % mnesia start-up requirement
    rpc:multicall(AllNodes, application, start, [mnesia]), % Starts mnesia on all nodes
    TableNamesList = lists:map(fun(X) ->
            create_tables(lists:nth(X, ConnectedNodeNames), node(), X)
        end, lists:seq(1,length(ConnectedNodeNames))),
    %% Create map
    Map = map_generator:test_generation(), % ! this is a temporary call - should be something else
    %% Load map into mnesia - in parallel processes
    Mnesia_loading_pid = spawn_link(?MODULE, initial_mnesia_load, [TableNamesList, Map]),
    %% Await GNs decisions to play as bot or human
    GNs_decisions = await_players_decisions(4, [], ConnectedNodeNames),
    io:format("GNs decisions received: ~p~n", [GNs_decisions]),
    %% Verify mnesia loading process has finished
    case erlang:is_process_alive(Mnesia_loading_pid) of
        true ->
            io:format("Mnesia loading process still alive, wait 5 more seconds...~n"),
            timer:sleep(5000),
            false = erlang:is_process_alive(Mnesia_loading_pid), % crash if not ready
            io:format("Mnesia loading process finished.~n");
        false ->
            io:format("Mnesia loading process already finished.~n")
    end,
    %% Open cn_server and cn_graphics_server
    {ok, _Pid_cn_server} = cn_server:start_link(GNs_decisions),
    {ok, _Pid_cn_graphics_server} = cn_server_graphics:start_link(ConnectedNodeNames),
    ok. % Startup process ends gracefully

%% =================== Helper Functions ======================

%% @doc Collects GN connection requests. Returns when 4 nodes are connected
%% Monitors the requesting process.
%% After every change in connection (incoming connection/incoming disconnect)
%% Broadcasts a notification containing current number of connected nodes to all
%% connected processes.
initial_connections_loop(4, ListOfNodeNames) ->
    io:format("All 4 GNs connected - broadcasting to GNs: ~w~n", [ListOfNodeNames]),
    %% broadcast update of connection status to all connected processes.
    broadcast_current_connections(4, ListOfNodeNames),
    ListOfNodeNames;

initial_connections_loop(Count, ListOfNodeNames) ->
    io:format("Current connected GNs: ~w~n", [ListOfNodeNames]),
    receive
        {_From, NodeName, connect_request} ->
            io:format("GN connected: ~p~n", [NodeName]),
            _Ref = erlang:monitor(process, _From), % monitor the requesting process
            NewList = [NodeName | ListOfNodeNames],
            broadcast_current_connections(Count + 1, NewList),
            initial_connections_loop(Count + 1, NewList);
        {_From, NodeName, disconnect_request} ->
            io:format("GN disconnected: ~p~n", [NodeName]),
            %% No monitor reference to demonitor since we don't store it
            NewList = lists:delete(NodeName, ListOfNodeNames),
            broadcast_current_connections(Count+1, NewList),
            initial_connections_loop(Count+1, NewList);
        {'DOWN', _Ref, process, Pid, _Reason} = Msg ->  % Monitored process closed unexpectedly
            io:format("Received DOWN from process ~p, hosted on node:~w~n
                        Full message:~w~n", [Pid, node(Pid), Msg]),
            case lists:member(node(Pid), ListOfNodeNames) of
                false ->
                    %% caught a 'DOWN' from someone who already disconnected from me, ignore it
                    initial_connections_loop(Count, ListOfNodeNames);
                true -> % process that we monitored within our list
                    NewList = lists:delete(node(Pid), ListOfNodeNames),
                    broadcast_current_connections(Count-1, NewList),
                    initial_connections_loop(Count-1, NewList)
            end
    end.

broadcast_current_connections(Counter, ListOfNodeNames) ->
    %% Helper function that sends updates about connected count to all connected nodes
    lists:foreach(fun(NodeName) -> {gn_start, NodeName} ! {cn_start, {connected_count, Counter}} end, ListOfNodeNames),
    ok.

%% @doc recieve-block that catches all decisions from GNs and returns a sorted list of tuples {1, true, Node_1_ID}, {2, false, Node_1_ID} ...
await_players_decisions(0, Acc, _GN_list) -> lists:sort(fun({A,_,_}, {B,_,_}) -> A =< B end, Acc);
await_players_decisions(N, Acc, GN_list) ->
    receive
        {Pid, playmode, Answer} when is_pid(Pid), is_boolean(Answer) ->
            case lists:member(node(Pid),GN_list) of
                true ->
                    GN_number = list_to_integer([lists:nth(3, atom_to_list(node(Pid)))]),
                    await_players_decisions(N-1, [{GN_number, Answer, node(Pid)} | Acc ], GN_list);
                false -> % an error - shouldn't catch such messages
                    %% print this to the screen, and for now continue as normal
                    io:format("Unexpected message from ~p: {~p, ~p}~n", [Pid, playmode, Answer]),
                    await_players_decisions(N, Acc, GN_list)
            end
    end.



%% helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

create_tables(GN_node, CN_node, Node_number) ->
    Mnesia_tiles_name = generate_atom_table_names(Node_number, "_tiles"),
    Mnesia_bombs_name = generate_atom_table_names(Node_number, "_bombs"),
    Mnesia_powerups_name = generate_atom_table_names(Node_number, "_powerups"),
    Mnesia_players_name = generate_atom_table_names(Node_number, "_players"),
    %% initialize all mnesia tables, per game node
    Debug1 = mnesia:create_table(Mnesia_tiles_name, [
        {attributes, record_info(fields, mnesia_tiles)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_tiles},
        {type, set}
        ]),
    Debug2 = mnesia:create_table(Mnesia_bombs_name, [
        {attributes, record_info(fields, mnesia_bombs)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_bombs},
        {type, set}
    ]),
    Debug3 = mnesia:create_table(Mnesia_powerups_name, [
        {attributes, record_info(fields, mnesia_powerups)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_powerups},
        {type, set}
    ]),
    Debug4 = mnesia:create_table(Mnesia_players_name, [
        {attributes, record_info(fields, mnesia_players)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_players},
        {type, set},
        {index, [pid]} % allows indexing (searching more easily) by pid field
    ]),
    io:format("CN: full printout of create_tables for node number #~w:~ntiles: ~w~nbombs: ~w~npowerups: ~w~nplayers: ~w~n", 
        [Node_number,Debug1 ,Debug2, Debug3, Debug4]).


%% @doc awaits mnesia table's finalized setup, then inserts the generated map-state to the tables
initial_mnesia_load(TableNamesList, Map) ->
    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % ? timeout is 5000ms for now
    insert_map_to_database(Map),
    io:format("*Initial map state loaded successfully to mnesia tables~n").

insert_map_to_database(Map) ->
    % full map size - 16x16 [0->15][0->15]
    lists:foreach(fun(X) ->
            lists:foreach(fun(Y) ->
                {TileType, PowerupType, _BombType, PlayerID} = get_tile_content(X,Y, Map),
                if
                    TileType == player_start -> % player at this location
                        init_player([X,Y], PlayerID);
                    TileType =/= free -> % tile "exists"
                        insert_tile([X,Y], TileType, PowerupType);
                    true -> ok % empty tiles aren't stored in database
                end
            end,
            lists:seq(0,15)) end,
        lists:seq(0,15)),
    ok.

%% lessen my suffering in getting content of [X,Y]
get_tile_content(Pos_x, Pos_y, Map) ->
    array:get(Pos_y, array:get(Pos_x, Map)).

%% inserts the tile to its appropriate position
insert_tile(Position=[X,Y], Type, Contains) ->
%% inserts tile to appropriate table - synchronously
    %% Map partitioning:
    %% __________
    %%| GN1| GN2|
    %% ---------
    %%| GN3| GN4|
    %%-----------
    F = fun() ->
        Inserted_record = #mnesia_tiles{position = Position, type = Type, contains = Contains},
        case true of
            _ when X >= 0, X =< 7 , Y > 7 , Y =< 15 -> % GN1
                mnesia:write(gn1_tiles, Inserted_record, write);
            _ when X > 7, X =< 15 , Y > 7 , Y =< 15 -> % GN2
                mnesia:write(gn2_tiles, Inserted_record, write);
            _ when X >= 0 , X =< 7 , Y >= 0 , Y =< 7 -> % GN3
                mnesia:write(gn3_tiles, Inserted_record, write);
            _ when X > 7 , X =< 15 , Y >= 0 , Y =< 7 -> % GN4
                mnesia:write(gn4_tiles, Inserted_record, write)
        end end,
        mnesia:activity(transaction, F).

%% Initialize a player in the appropriate mnesia player table
init_player([X,Y], PlayerID) ->
    Fun = fun() ->
        Init_player_record = #mnesia_players{
            player_number = list_to_integer([lists:nth(8, atom_to_list(PlayerID))]),
            position = [X,Y],
            direction = none,
            movement = false
        },
        case PlayerID of
            'player_1' ->
                mnesia:write(gn1_players, Init_player_record, write);
            'player_2' ->
                mnesia:write(gn2_players, Init_player_record, write);
            'player_3' ->
                mnesia:write(gn3_players, Init_player_record, write);
            'player_4' ->
                mnesia:write(gn4_players, Init_player_record, write)
        end end,
    io:format("CN: initialized a player entry ~w at location ~w~n",[PlayerID, [X,Y]]),
    mnesia:activity(transaction, Fun).

%% --------------------------------------------------------------
