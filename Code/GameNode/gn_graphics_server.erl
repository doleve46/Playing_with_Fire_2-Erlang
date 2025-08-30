-module(gn_graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../common_parameters.hrl").

%% Socket configuration
-define(PYTHON_SOCKET_PORT_BASE, 8100). % GN1: 8101, GN2: 8102, etc.
-define(SOCKET_TIMEOUT, 5000).
-define(SOCKET_BACKLOG, 5).

-record(state, {
    cn_node,                    % Central node
    python_socket,              % Socket to local Python visualizer
    python_socket_pid,          % Socket handler PID
    listen_socket,              % Listen socket
    update_counter = 0,         % Update counter
    current_map_state,          % Current enhanced map state received from CN
    dead_players = #{},         % Track dead players received from CN
    last_update_time = 0,       % Track when we last received an update
    local_gn_name,              % This GN's identifier (gn1, gn2, gn3, gn4)
    backend_timing = #{},       % Backend timing constants received from CN
    local_player_ids = [],      % Player IDs that belong to this GN
    active_explosions = #{},    % Track active explosions received from CN
    socket_acceptor             % Socket acceptor process PID
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the enhanced GN graphics server
-spec start_link(node()) -> {ok, pid()} | ignore | {error, term()}.
start_link(CNNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CNNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the enhanced GN graphics server
init([CNNode]) ->
    LocalGN = determine_local_gn(),
    LocalPlayerIDs = get_local_player_ids(LocalGN),
    
    io:format("🎮 Enhanced GN Graphics Server starting on ~w (Local GN: ~w, Players: ~w)~n", 
              [node(), LocalGN, LocalPlayerIDs]),
    
    State = #state{
        cn_node = CNNode,
        local_gn_name = LocalGN,
        local_player_ids = LocalPlayerIDs
    },
    
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    
    % Start socket server
    erlang:send_after(50, self(), start_socket_server),
    
    io:format("✅ Enhanced GN Graphics Server initialized (waiting for CN updates)~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(get_dead_players, _From, State) ->
    {reply, State#state.dead_players, State};

handle_call(get_active_explosions, _From, State) ->
    {reply, State#state.active_explosions, State};

handle_call(get_local_info, _From, State) ->
    LocalInfo = #{
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        backend_timing => State#state.backend_timing,
        update_counter => State#state.update_counter,
        active_explosions_count => maps:size(State#state.active_explosions)
    },
    {reply, LocalInfo, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, EnhancedMapState}, State) ->
    % io:format("🗺️ GN Graphics Server received map update!~n"),
    % io:format("🔌 Python socket PID: ~p~n", [State#state.python_socket_pid]),  %debug
    CurrentTime = erlang:system_time(millisecond),
    
    {ActualMapState, DeadPlayers, BackendTiming, ActiveExplosions} = case EnhancedMapState of
        #{map := GridData, dead_players := DeadPlayersMap, backend_timing := Timing, active_explosions := Explosions} ->
            ExplosionCount = maps:size(Explosions),
            % io:format("🗺️ GN received full enhanced map update from CN (#~w) with ~w explosions~n", 
                      % [State#state.update_counter + 1, ExplosionCount]),
            
            NewDeaths = maps:filter(fun(PlayerID, _DeathInfo) ->
                not maps:is_key(PlayerID, State#state.dead_players)
            end, DeadPlayersMap),
            
            if map_size(NewDeaths) > 0 ->
                NewDeathList = maps:to_list(NewDeaths),
                io:format("💀 New deaths detected by GN: ~p~n", [NewDeathList]),
                
                lists:foreach(fun({PlayerID, {DeathTime, _LastState, LocalGNAtom}}) ->
                    if LocalGNAtom =:= State#state.local_gn_name ->
                        io:format("🩸 LOCAL PLAYER ~w DIED on this GN! (Death time: ~w)~n", [PlayerID, DeathTime]);
                    true ->
                        io:format("💀 Remote player ~w died on ~w~n", [PlayerID, LocalGNAtom])
                    end
                end, NewDeathList);
            true -> ok
            end,
            
            PreviousExplosions = maps:size(State#state.active_explosions),
            if ExplosionCount > PreviousExplosions ->
                NewExplosionCount = ExplosionCount - PreviousExplosions,
                io:format("💥 ~w new explosions received from CN~n", [NewExplosionCount]);
            ExplosionCount < PreviousExplosions ->
                ExpiredCount = PreviousExplosions - ExplosionCount,
                io:format("💨 ~w explosions expired~n", [ExpiredCount]);
            true -> ok
            end,
            
            {GridData, DeadPlayersMap, Timing, Explosions};
            
        #{map := GridData, dead_players := DeadPlayersMap, backend_timing := Timing} ->
            io:format("🗺️ GN received enhanced map update from CN (#~w) with timing & death info~n", 
                      [State#state.update_counter + 1]),
            {GridData, DeadPlayersMap, Timing, State#state.active_explosions};
            
        #{map := GridData, dead_players := DeadPlayersMap} ->
            io:format("🗺️ GN received map update from CN (#~w) with death info~n", 
                      [State#state.update_counter + 1]),
            {GridData, DeadPlayersMap, State#state.backend_timing, State#state.active_explosions};
            
        _ ->
            io:format("🗺️ GN received basic map update from CN (#~w)~n", 
                      [State#state.update_counter + 1]),
            {EnhancedMapState, State#state.dead_players, State#state.backend_timing, State#state.active_explosions}
    end,

    % If this is the first real map update, signal the menu to start the game
    case State#state.current_map_state of
        undefined ->
            % First map update - signal menu process to start the actual game
            case whereis(menu) of
                undefined -> ok;
                MenuPid -> MenuPid ! {start_actual_game}
            end;
        _ -> ok
    end,
    
    LocalEnhancedMapData = #{
        map => ActualMapState,
        dead_players => DeadPlayers,
        update_time => CurrentTime,
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        backend_timing => BackendTiming,
        active_explosions => ActiveExplosions
    },
    
    send_enhanced_map_to_socket(State#state.python_socket_pid, LocalEnhancedMapData),
    % io:format("📤 Attempted to send map to socket~n"),
            
    NewState = State#state{
        current_map_state = LocalEnhancedMapData,
        dead_players = DeadPlayers,
        backend_timing = BackendTiming,
        active_explosions = ActiveExplosions,
        update_counter = State#state.update_counter + 1,
        last_update_time = CurrentTime
    },
    {noreply, NewState};

handle_cast({movement_confirmation, ConfirmationData}, State) ->
    send_movement_confirmation_to_socket(State#state.python_socket_pid, ConfirmationData),
    {noreply, State};

handle_cast({timer_update, TimerData}, State) ->
    send_timer_update_to_socket(State#state.python_socket_pid, TimerData),
    {noreply, State};

handle_cast({fsm_update, FSMData}, State) ->
    send_fsm_update_to_socket(State#state.python_socket_pid, FSMData),
    {noreply, State};

handle_cast(force_update, State) ->
    case State#state.current_map_state of
        undefined ->
            io:format("⚠️ No enhanced map state available for force update~n");
        MapState ->
            ExplosionCount = maps:size(State#state.active_explosions),
            io:format("🔄 Force updating Socket with current enhanced map state (~w explosions)~n", [ExplosionCount]),
            send_enhanced_map_to_socket(State#state.python_socket_pid, MapState)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(start_socket_server, State) ->
    LocalGN = State#state.local_gn_name,
    SocketPort = get_gn_socket_port(LocalGN),
    
    io:format("🔌 Starting socket server for ~w on port ~w...~n", [LocalGN, SocketPort]),
    
   case start_gn_socket_listener(SocketPort) of
        {ok, {ListenSocket, AcceptorPid}} ->
            io:format("✅ GN Socket server started successfully on port ~w~n", [SocketPort]),
            UpdatedState = State#state{
                listen_socket = ListenSocket,
                socket_acceptor = AcceptorPid
            },
            
            case State#state.current_map_state of
                undefined ->
                    io:format("✅ Socket server ready, waiting for first CN update~n");
                MapState ->
                    io:format("✅ Socket server ready and current map state will be sent when client connects~n")
            end,
            
            % Start the Python socket client after 2 seconds
            erlang:send_after(2000, self(), start_python_socket_client),
            
            {noreply, UpdatedState};
        {error, Reason} ->
            io:format("❌ Failed to start GN socket server: ~p~n", [Reason]),
            % Continue without socket server
            {noreply, State}
    end;

handle_info(start_socket_server, State) ->
    LocalGN = State#state.local_gn_name,
    SocketPort = get_gn_socket_port(LocalGN),
    
    io:format("🔌 Starting socket server for ~w on port ~w...~n", [LocalGN, SocketPort]),
    
    case start_gn_socket_listener(SocketPort) of
        {ok, {ListenSocket, AcceptorPid}} ->
            io:format("✅ GN Socket server started successfully on port ~w~n", [SocketPort]),
            UpdatedState = State#state{
                listen_socket = ListenSocket,
                socket_acceptor = AcceptorPid
            },
            
            % Give socket server more time to be ready before starting Python
            erlang:send_after(3000, self(), start_python_socket_client),  % Changed from 2000 to 3000
            
            {noreply, UpdatedState};
        {error, Reason} ->
            io:format("❌ Failed to start GN socket server: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_info({socket_connected, ClientSocket, ClientPid}, State) ->
    io:format("🔗 Python client connected to GN ~w via socket~n", [State#state.local_gn_name]),
    NewState = State#state{
        python_socket = ClientSocket,
        python_socket_pid = ClientPid
    },
    
    % Send current state if available
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> 
            send_enhanced_map_to_socket(ClientPid, MapState),
            io:format("✅ Current map state sent to newly connected client~n")
    end,
    {noreply, NewState};

handle_info({socket_disconnected, ClientPid}, State) 
    when ClientPid == State#state.python_socket_pid ->
    io:format("⚠️ GN Python socket disconnected~n"),
    NewState = State#state{
        python_socket = undefined,
        python_socket_pid = undefined
    },
    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, State) when Pid == State#state.socket_acceptor ->
    io:format("❌ GN Socket acceptor crashed: ~p. Restarting...~n", [Reason]),
    LocalGN = State#state.local_gn_name,
    SocketPort = get_gn_socket_port(LocalGN),
    case start_gn_socket_listener(SocketPort) of
        {ok, {ListenSocket, AcceptorPid}} ->
            io:format("✅ GN Socket acceptor restarted~n"),
            NewState = State#state{
                listen_socket = ListenSocket,
                socket_acceptor = AcceptorPid
            },
            {noreply, NewState};
        {error, RestartReason} ->
            io:format("❌ Failed to restart GN socket acceptor: ~p~n", [RestartReason]),
            {noreply, State}
    end;

handle_info({'EXIT', Pid, Reason}, State) when Pid == State#state.python_socket_pid ->
    io:format("⚠️ GN Python socket handler crashed: ~p~n", [Reason]),
    NewState = State#state{
        python_socket = undefined,
        python_socket_pid = undefined
    },
    {noreply, NewState};

handle_info({nodedown, Node}, State) when Node == State#state.cn_node ->
    io:format("⚠️ CN node ~w went down~n", [Node]),
    {noreply, State};

handle_info({nodeup, Node}, State) when Node == State#state.cn_node ->
    io:format("✅ CN node ~w came back up~n", [Node]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("ℹ️ Unexpected message: ~p~n", [Info]),
    {noreply, State}.

%% Cleanup on termination
terminate(_Reason, State) ->
    io:format("🛑 Enhanced GN Graphics Server terminating~n"),
    
    if State#state.python_socket =/= undefined ->
        gen_tcp:close(State#state.python_socket);
    true -> ok
    end,
    
    if State#state.listen_socket =/= undefined ->
        gen_tcp:close(State#state.listen_socket);
    true -> ok
    end,
    
    if State#state.socket_acceptor =/= undefined ->
        exit(State#state.socket_acceptor, shutdown);
    true -> ok
    end,
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% GN Identification Functions
%%%===================================================================

determine_local_gn() ->
    case os:getenv("GN_ID") of
        false ->
            NodeName = atom_to_list(node()),
            case NodeName of
                "gn1" ++ _ -> gn1;
                "gn2" ++ _ -> gn2;
                "gn3" ++ _ -> gn3;
                "gn4" ++ _ -> gn4;
                "GN1" ++ _ -> gn1;  % Add uppercase patterns
                "GN2" ++ _ -> gn2;
                "GN3" ++ _ -> gn3;
                "GN4" ++ _ -> gn4;
                _ ->
                    % Try regex approach for more flexible matching
                    case re:run(NodeName, "[Gg][Nn]([1-4])", [{capture, all_but_first, list}]) of
                        {match, [GNNum]} ->
                            case GNNum of
                                "1" -> gn1;
                                "2" -> gn2;
                                "3" -> gn3;
                                "4" -> gn4;
                                _ -> gn1
                            end;
                        nomatch ->
                            io:format("⚠️ Could not determine GN ID from node name ~p, defaulting to gn1~n", [NodeName]),
                            gn1
                    end
            end;
        GNStr ->
            list_to_atom(GNStr)
    end.

get_local_player_ids(LocalGN) ->
    GNToPlayers = #{
        gn1 => [1],  % Player 1 is managed by GN1
        gn2 => [2],  % Player 2 is managed by GN2
        gn3 => [3],  % Player 3 is managed by GN3
        gn4 => [4]   % Player 4 is managed by GN4
    },
    maps:get(LocalGN, GNToPlayers, []).

%%%===================================================================
%%% Socket Server Functions
%%%===================================================================

get_gn_socket_port(GNName) ->
    case GNName of
        gn1 -> ?PYTHON_SOCKET_PORT_BASE + 1;  % 8101
        gn2 -> ?PYTHON_SOCKET_PORT_BASE + 2;  % 8102
        gn3 -> ?PYTHON_SOCKET_PORT_BASE + 3;  % 8103
        gn4 -> ?PYTHON_SOCKET_PORT_BASE + 4   % 8104
    end.

start_gn_socket_listener(Port) ->
    case gen_tcp:listen(Port, [
        binary, 
        {packet, 0}, 
        {active, false}, 
        {reuseaddr, true},
        {nodelay, true},
        {keepalive, true},
        {backlog, ?SOCKET_BACKLOG}
    ]) of
        {ok, ListenSocket} ->
            MainProcess = self(),
            AcceptorPid = spawn_link(fun() -> 
                gn_socket_acceptor_loop(ListenSocket, MainProcess, Port) 
            end),
            {ok, {ListenSocket, AcceptorPid}};
        {error, Reason} ->
            {error, Reason}
    end.

gn_socket_acceptor_loop(ListenSocket, MainProcess, Port) ->
    io:format("🔌 GN Socket acceptor listening on port ~w~n", [Port]),
    accept_gn_connections(ListenSocket, MainProcess).

accept_gn_connections(ListenSocket, MainProcess) ->
    case gen_tcp:accept(ListenSocket, infinity) of
        {ok, ClientSocket} ->
            io:format("🔗 New GN client connected~n"),
            
            % Set socket options
            inet:setopts(ClientSocket, [
                binary,
                {active, false},
                {nodelay, true},
                {keepalive, true}
            ]),
            
            % Spawn client handler
            ClientPid = spawn_link(fun() -> 
                handle_gn_socket_client(ClientSocket, MainProcess) 
            end),
            
            % Transfer socket control to client handler
            gen_tcp:controlling_process(ClientSocket, ClientPid),
            
            % Notify main process
            MainProcess ! {socket_connected, ClientSocket, ClientPid},
            
            % Continue accepting
            accept_gn_connections(ListenSocket, MainProcess);
        {error, Reason} ->
            io:format("❌ GN Accept failed: ~p~n", [Reason]),
            timer:sleep(1000),
            accept_gn_connections(ListenSocket, MainProcess)
    end.

handle_gn_socket_client(Socket, MainProcess) ->
    inet:setopts(Socket, [{active, once}]),
    gn_socket_client_loop(Socket, MainProcess).

gn_socket_client_loop(Socket, MainProcess) ->
    receive
        {tcp, Socket, Data} ->
            io:format("📨 GN received data from client: ~p bytes~n", [byte_size(Data)]),
            inet:setopts(Socket, [{active, once}]),
            gn_socket_client_loop(Socket, MainProcess);
        {tcp_closed, Socket} ->
            io:format("🔌 GN client disconnected~n"),
            MainProcess ! {socket_disconnected, self()},
            gen_tcp:close(Socket);
        {tcp_error, Socket, Reason} ->
            io:format("❌ GN socket error: ~p~n", [Reason]),
            MainProcess ! {socket_disconnected, self()},
            gen_tcp:close(Socket);
        {send_data, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("❌ GN failed to send data: ~p~n", [Reason]),
                    MainProcess ! {socket_disconnected, self()},
                    gen_tcp:close(Socket)
            end,
            gn_socket_client_loop(Socket, MainProcess);
        shutdown ->
            gen_tcp:close(Socket);
        Other ->
            io:format("⚠️ GN socket client received unexpected message: ~p~n", [Other]),
            gn_socket_client_loop(Socket, MainProcess)
    after 30000 ->
        % 30 second keepalive
        case gen_tcp:send(Socket, <<>>) of
            ok ->
                gn_socket_client_loop(Socket, MainProcess);
            {error, _} ->
                io:format("🔌 GN client keepalive failed, disconnecting~n"),
                MainProcess ! {socket_disconnected, self()},
                gen_tcp:close(Socket)
        end
    end.

%%%===================================================================
%%% Socket Communication Functions
%%%===================================================================

send_enhanced_map_to_socket(undefined, _MapState) ->
    % io:format("⚠️ No socket client connected to send map~n"),
    ok;
send_enhanced_map_to_socket(ClientPid, MapState) ->
    io:format("📡 Sending map via socket to PID: ~p~n", [ClientPid]),
    try
        % Create simpler JSON structure
        SimpleMapData = create_simple_map_data(MapState),
        
        JsonMessage = #{
            <<"type">> => <<"map_update">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => SimpleMapData
        },
        
        JsonBinary = jsx:encode(JsonMessage),
        DataLength = byte_size(JsonBinary),
        LengthPrefix = <<DataLength:32/big>>,
        Message = <<LengthPrefix/binary, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        io:format("✅ Map data sent via socket (~w bytes)~n", [DataLength])
        
    catch
        Error:Reason ->
            io:format("❌ Error sending map via socket: ~p:~p~n", [Error, Reason])
    end.

send_movement_confirmation_to_socket(undefined, _ConfirmationData) ->
    ok;
send_movement_confirmation_to_socket(ClientPid, ConfirmationData) ->
    try
        JsonMessage = #{
            <<"type">> => <<"movement_confirmation">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => convert_for_json(ConfirmationData)
        },
        
        JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
        DataLength = byte_size(JsonBinary),
        Message = <<DataLength:32/big, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        case ConfirmationData of
            #{entity_type := player, entity_data := #{player_id := PlayerID}} ->
                io:format("🏃 JSON movement confirmation forwarded for player ~w~n", [PlayerID]);
            #{entity_type := bomb, entity_data := #{from_pos := Pos}} ->
                io:format("💣 JSON movement confirmation forwarded for bomb at ~w~n", [Pos]);
            _ ->
                io:format("📤 JSON movement confirmation forwarded~n")
        end
    catch
        _:Error ->
            io:format("❌ Error sending JSON movement confirmation via socket: ~p~n", [Error])
    end.

send_timer_update_to_socket(undefined, _TimerData) ->
    ok;
send_timer_update_to_socket(ClientPid, TimerData) ->
    try
        JsonMessage = #{
            <<"type">> => <<"timer_update">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => convert_for_json(TimerData)
        },
        
        JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
        DataLength = byte_size(JsonBinary),
        Message = <<DataLength:32/big, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        io:format("⏱️ JSON timer update forwarded via socket~n")
    catch
        _:Error ->
            io:format("❌ Error sending JSON timer update via socket: ~p~n", [Error])
    end.

send_fsm_update_to_socket(undefined, _FSMData) ->
    ok;
send_fsm_update_to_socket(ClientPid, FSMData) ->
    try
        JsonMessage = #{
            <<"type">> => <<"fsm_update">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => convert_for_json(FSMData)
        },
        
        JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
        DataLength = byte_size(JsonBinary),
        Message = <<DataLength:32/big, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        io:format("🎰 JSON FSM update forwarded via socket~n")
    catch
        _:Error ->
            io:format("❌ Error sending JSON FSM update via socket: ~p~n", [Error])
    end.

%%%===================================================================
%%% JSON Conversion Functions
%%%===================================================================

convert_for_json(#{} = Map) ->
    maps:fold(fun(K, V, Acc) ->
        NewKey = case is_atom(K) of
            true -> atom_to_utf8_binary(K);
            false -> K
        end,
        Acc#{NewKey => convert_for_json(V)}
    end, #{}, Map);

convert_for_json(List) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true -> unicode:characters_to_binary(List, utf8);
        false -> [convert_for_json(Item) || Item <- List]
    end;

convert_for_json(Atom) when is_atom(Atom) ->
    atom_to_utf8_binary(Atom);

convert_for_json(Tuple) when is_tuple(Tuple) ->
    convert_for_json(tuple_to_list(Tuple));

convert_for_json(Other) ->
    Other.

atom_to_utf8_binary(Atom) when is_atom(Atom) ->
    unicode:characters_to_binary(atom_to_list(Atom), latin1, utf8);
atom_to_utf8_binary(Other) ->
    Other.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_simple_map_data(MapState) ->
    #{
        <<"map">> => convert_map_for_gn_socket(maps:get(map, MapState, [])),
        <<"dead_players">> => #{},
        <<"update_time">> => erlang:system_time(millisecond),
        <<"local_gn">> => maps:get(local_gn, MapState, gn1),
        <<"active_explosions">> => #{},
        <<"backend_timing">> => #{
            <<"tick_delay">> => 50,
            <<"tile_move">> => 1200
        }
    }.

convert_map_for_gn_socket([]) ->
    % Create empty 16x16 grid
    EmptyCell = [<<"free">>, <<"none">>, <<"none">>, <<"none">>],
    EmptyRow = [EmptyCell || _ <- lists:seq(1, 16)],
    [EmptyRow || _ <- lists:seq(1, 16)];
convert_map_for_gn_socket(Map) when is_list(Map) ->
    try
        lists:map(fun(Row) when is_list(Row) ->
            lists:map(fun(Cell) ->
                convert_gn_cell_safely(Cell)
            end, Row);
        (_) -> 
            []
        end, Map)
    catch
        _:_ ->
            % Return empty grid if conversion fails
            EmptyCell = [<<"free">>, <<"none">>, <<"none">>, <<"none">>],
            EmptyRow = [EmptyCell || _ <- lists:seq(1, 16)],
            [EmptyRow || _ <- lists:seq(1, 16)]
    end.

convert_gn_cell_safely({Tile, Powerup, Bomb, Player, Explosion, Special}) ->
    [
        safe_atom_to_binary_gn(Tile),
        safe_atom_to_binary_gn(Powerup),
        convert_bomb_safely_gn(Bomb),
        convert_player_safely_gn(Player)
    ];
convert_gn_cell_safely({Tile, Powerup, Bomb, Player}) ->
    [
        safe_atom_to_binary_gn(Tile),
        safe_atom_to_binary_gn(Powerup),
        convert_bomb_safely_gn(Bomb),
        convert_player_safely_gn(Player)
    ];
convert_gn_cell_safely(_) ->
    [<<"free">>, <<"none">>, <<"none">>, <<"none">>].

safe_atom_to_binary_gn(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
safe_atom_to_binary_gn(Other) when is_binary(Other) ->
    Other;
safe_atom_to_binary_gn(Other) when is_list(Other) ->
    list_to_binary(Other);
safe_atom_to_binary_gn(_) ->
    <<"unknown">>.

convert_bomb_safely_gn(none) ->
    <<"none">>;
convert_bomb_safely_gn({Type, Ignited, Status, Radius, Owner, Movement, Direction}) ->
    [
        safe_atom_to_binary_gn(Type),
        Ignited,
        safe_atom_to_binary_gn(Status),
        Radius,
        Owner,
        Movement,
        safe_atom_to_binary_gn(Direction)
    ];
convert_bomb_safely_gn(_) ->
    <<"none">>.

convert_player_safely_gn(none) ->
    <<"none">>;
convert_player_safely_gn({PlayerID, Life, Speed, Direction, Movement, MovementTimer, ImmunityTimer, RequestTimer}) ->
    [
        PlayerID,
        Life,
        Speed,
        safe_atom_to_binary_gn(Direction),
        Movement,
        MovementTimer,
        ImmunityTimer,
        RequestTimer
    ];
convert_player_safely_gn(_) ->
    <<"none">>.
