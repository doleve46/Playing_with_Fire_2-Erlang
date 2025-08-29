-module(cn_server_graphics).
-behaviour(gen_server).

%% API
-export([start_link/1, get_current_map/0, show_explosion/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("mnesia_records.hrl").
-include("../common_parameters.hrl").

-define(MAP_SIZE, 16).
-define(DEATH_DISPLAY_TIME, 10000). % Show dead players for 10 seconds
-define(EXPLOSION_DISPLAY_TIME, 1000). % Show explosions for 1 second

%% Socket configuration
-define(PYTHON_SOCKET_PORT, 8080).
-define(SOCKET_TIMEOUT, 5000).
-define(SOCKET_BACKLOG, 5).

-record(state, {
    gn_graphics_servers = [],     % List of {Node, Pid} for GN graphics servers
    python_socket,                % Socket to Python visualizer
    python_socket_pid,            % Socket handler PID
    listen_socket,                % Listen socket
    current_map_state,            % Current unified map state
    gn_nodes,                     % List of GN nodes
    subscribed_tables = [],       % List of tables subscribed to
    update_counter = 0,           % Counter for updates (debugging)
    movement_states = #{},        % Track active player movements with real timing
    bomb_movements = #{},         % Track active bomb movements
    dead_players = #{},           % Track recently deceased players
    last_known_players = #{},     % Track last known player states for death detection
    timer_subscribers = #{},      % Track timer update subscriptions
    active_explosions = #{},      % Track active explosions: Coord => ExpiryTime
    socket_acceptor               % Socket acceptor process PID
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the central graphics server
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(GNNodes) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [GNNodes], []).

%% Get current map state
-spec get_current_map() -> term().
get_current_map() ->
    gen_server:call(?MODULE, get_current_map).

%% Direct function to show explosions
-spec show_explosion(list()) -> ok.
show_explosion(Coordinates) ->
    io:format("💥 Direct explosion call: ~w coordinates will display for ~wms~n", 
              [length(Coordinates), ?EXPLOSION_DISPLAY_TIME]),
    gen_server:cast(?MODULE, {add_explosions_direct, Coordinates}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the graphics server
init([GNNodes]) ->
    io:format("🎨 Enhanced CN Graphics Server starting with Socket communication...~n"),
   
    % Create initial state
    State = #state{gn_nodes = GNNodes},
   
    % Set up mnesia subscriptions
    erlang:send(self(), setup_subscriptions),
   
    % Monitoring of gn graphics servers
    erlang:send_after(?TICK_DELAY, self(), monitor_gn_graphics_servers),
   
    % Start socket server
    erlang:send(self(), start_socket_server),
   
    % Start periodic updates
    erlang:send_after(2*?TICK_DELAY, self(), periodic_update),
   
    % Clean up dead players and explosions periodically
    erlang:send_after(5000, self(), cleanup_expired_elements),
    
    process_flag(trap_exit, true),

    % Start Python visualizer
    start_python_visualizer(),
    
    io:format("✅ Enhanced CN Graphics Server initialized with Socket communication~n"),
    {ok, State}.

%%%===================================================================
%%% Python Visualizer Startup
%%%===================================================================

start_python_visualizer() ->
    spawn(fun() ->
        timer:sleep(2000), % Wait a bit for socket server to be ready
        {ok, Cwd} = file:get_cwd(),
        PythonScript = filename:join([Cwd, "src", "Graphics", "map_live_port.py"]),
        case filelib:is_file(PythonScript) of
            true ->
                io:format("🚀 Starting CN Python visualizer...~n"),
                Port = open_port({spawn, "python3 " ++ PythonScript}, 
                    [{cd, filename:dirname(PythonScript)}, binary, exit_status]),
                io:format("✅ CN Python visualizer started~n");
            false ->
                io:format("❌ Python script not found: ~s~n", [PythonScript])
        end
    end).

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast(force_update, State) ->
    io:format("🔄 Updating enhanced map state with Socket...~n"),
    NewMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState};

handle_cast({add_explosions_direct, Coordinates}, State) ->
    Timestamp = erlang:system_time(millisecond),
    ExpiryTime = Timestamp + ?EXPLOSION_DISPLAY_TIME,
    NewExplosions = lists:foldl(fun(Coord, AccMap) ->
        maps:put(Coord, ExpiryTime, AccMap)
    end, State#state.active_explosions, Coordinates),
    
    NewState = State#state{active_explosions = NewExplosions},
    UpdatedMapState = create_enhanced_map_state(NewState),
    FinalState = NewState#state{current_map_state = UpdatedMapState},
    send_map_to_all_targets(FinalState),
    
    send_explosion_event_to_socket(FinalState#state.python_socket_pid, #{
        coordinates => Coordinates,
        explosion_type => direct,
        timestamp => Timestamp,
        display_time => ?EXPLOSION_DISPLAY_TIME
    }),
    
    {noreply, FinalState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(start_socket_server, State) ->
    io:format("🔌 Starting CN socket server on port ~w...~n", [?PYTHON_SOCKET_PORT]),
    case start_cn_socket_listener() of
        {ok, {ListenSocket, AcceptorPid}} ->
            io:format("✅ CN Socket server started successfully~n"),
            InitialMapState = create_enhanced_map_state(State),
            UpdatedState = State#state{
                listen_socket = ListenSocket,
                socket_acceptor = AcceptorPid,
                current_map_state = InitialMapState
            },
            
            % Wait for CN server to be available
            wait_for_cn_server_and_notify(),
            
            {noreply, UpdatedState};
        {error, Reason} ->
            io:format("❌ Failed to start CN socket server: ~p~n", [Reason]),
            {stop, {socket_server_failed, Reason}, State}
    end;

handle_info({socket_connected, ClientSocket, ClientPid}, State) ->
    io:format("🔗 Python client connected to CN via socket~n"),
    NewState = State#state{
        python_socket = ClientSocket,
        python_socket_pid = ClientPid
    },
    % Send initial map state
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> send_map_to_socket(ClientPid, MapState)
    end,
    {noreply, NewState};

handle_info({socket_disconnected, ClientPid}, State) 
    when ClientPid == State#state.python_socket_pid ->
    io:format("⚠️ CN Python socket disconnected~n"),
    NewState = State#state{
        python_socket = undefined,
        python_socket_pid = undefined
    },
    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, State) when Pid == State#state.socket_acceptor ->
    io:format("❌ Socket acceptor crashed: ~p. Restarting...~n", [Reason]),
    case start_cn_socket_listener() of
        {ok, {ListenSocket, AcceptorPid}} ->
            io:format("✅ Socket acceptor restarted~n"),
            NewState = State#state{
                listen_socket = ListenSocket,
                socket_acceptor = AcceptorPid
            },
            {noreply, NewState};
        {error, RestartReason} ->
            io:format("❌ Failed to restart socket acceptor: ~p~n", [RestartReason]),
            {stop, {socket_restart_failed, RestartReason}, State}
    end;

handle_info({'EXIT', Pid, Reason}, State) when Pid == State#state.python_socket_pid ->
    io:format("⚠️ Python socket handler crashed: ~p~n", [Reason]),
    NewState = State#state{
        python_socket = undefined,
        python_socket_pid = undefined
    },
    {noreply, NewState};

handle_info(setup_subscriptions, State) ->
    io:format("📡 Setting up enhanced mnesia subscriptions...~n"),
    Tables = get_all_tables(),
    SubscribedTables = setup_mnesia_subscriptions(Tables),
    io:format("✅ Subscribed to tables: ~p~n", [SubscribedTables]),
    {noreply, State#state{subscribed_tables = SubscribedTables}};

handle_info(monitor_gn_graphics_servers, State) ->
    io:format("🚀 Attempting to monitor all GN graphics servers...~n"),
    ReferencesList = monitor_gn_graphics_servers(State#state.gn_nodes),
    io:format("✅ Monitoring was successful!: ~p~n", [length(ReferencesList)]),
    {noreply, State#state{gn_graphics_servers = ReferencesList}};

handle_info(periodic_update, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    io:format("📡 Periodic update - GN servers list: ~p~n", [State#state.gn_graphics_servers]),
    
    % Clean up expired explosions
    NewExplosions = maps:filter(fun(_Coord, ExpiryTime) ->
        CurrentTime < ExpiryTime
    end, State#state.active_explosions),
    
    CleanedExplosions = maps:size(State#state.active_explosions) - maps:size(NewExplosions),
    if CleanedExplosions > 0 ->
        io:format("💨 Cleaned up ~w expired explosions~n", [CleanedExplosions]);
    true -> ok
    end,
    
    NewMapState = create_enhanced_map_state(State#state{active_explosions = NewExplosions}),
    UpdatedState = State#state{
        current_map_state = NewMapState,
        update_counter = State#state.update_counter + 1,
        active_explosions = NewExplosions
    },
   
    ShouldSend = (NewMapState =/= State#state.current_map_state) orelse
                 (State#state.update_counter rem 2 == 0),
   
    if ShouldSend ->
        send_map_to_all_targets(UpdatedState),
        % io:format("📤 Sent map to all targets including ~w GN servers~n", [length(UpdatedState#state.gn_graphics_servers)]),
        case State#state.update_counter rem 500 == 0 of
            true ->
                io:format("🔄 Enhanced periodic Socket update #~w sent~n", [UpdatedState#state.update_counter]);
            false -> ok
        end;
    true -> ok
    end,
   
    erlang:send_after(?TICK_DELAY, self(), periodic_update),
    {noreply, UpdatedState};

handle_info(cleanup_expired_elements, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    NewDeadPlayers = maps:filter(fun(_PlayerID, {DeathTime, _LastState, _LocalGN}) ->
        CurrentTime - DeathTime < ?DEATH_DISPLAY_TIME
    end, State#state.dead_players),
    
    NewExplosions = maps:filter(fun(_Coord, ExpiryTime) ->
        CurrentTime < ExpiryTime
    end, State#state.active_explosions),
    
    CleanedDeaths = maps:size(State#state.dead_players) - maps:size(NewDeadPlayers),
    CleanedExplosions = maps:size(State#state.active_explosions) - maps:size(NewExplosions),
    
    if CleanedDeaths > 0 ->
        io:format("🧹 Cleaned up ~w expired dead players~n", [CleanedDeaths]);
    true -> ok
    end,
    
    if CleanedExplosions > 0 ->
        io:format("💨 Cleaned up ~w expired explosions~n", [CleanedExplosions]);
    true -> ok
    end,
    
    erlang:send_after(5000, self(), cleanup_expired_elements),
    {noreply, State#state{dead_players = NewDeadPlayers, active_explosions = NewExplosions}};

% Enhanced mnesia table event handling
handle_info({mnesia_table_event, {write, Table, Record, _ActivityId}}, State) ->
    NewState = case Record of
        #mnesia_players{} ->
            PlayerID = Record#mnesia_players.player_number,
            NewLastKnown = maps:put(PlayerID, Record, State#state.last_known_players),
            
            case detect_enhanced_player_movement_change(Record, State#state.current_map_state) of
                {movement_started, PlayerData} ->
                    send_movement_confirmation_to_socket(State, player, PlayerData),
                    State#state{last_known_players = NewLastKnown};
                {timer_update, TimerData} ->
                    send_timer_update_to_socket(State, player, TimerData),
                    State#state{last_known_players = NewLastKnown};
                no_movement_change ->
                    State#state{last_known_players = NewLastKnown}
            end;
        #mnesia_bombs{} ->
            case detect_enhanced_bomb_movement_change(Record, State#state.current_map_state) of
                {movement_started, BombData} ->
                    send_movement_confirmation_to_socket(State, bomb, BombData),
                    State;
                {fsm_state_change, FSMData} ->
                    send_fsm_update_to_socket(State, bomb, FSMData),
                    State;
                no_movement_change ->
                    State
            end;
        _ ->
            State
    end,
    handle_mnesia_update(NewState);

handle_info({mnesia_table_event, {delete, Table, Key, _ActivityId}}, State) ->
    NewState = case Table of
        TableName when TableName == gn1_players; TableName == gn2_players; 
                       TableName == gn3_players; TableName == gn4_players ->
            case extract_player_id_from_key(Key) of
                {ok, PlayerID} ->
                    handle_enhanced_player_death(PlayerID, Table, State);
                error ->
                    io:format("⚠️ Could not extract player ID from key: ~p~n", [Key]),
                    State
            end;
        _ ->
            State
    end,
    handle_mnesia_update(NewState);

handle_info({mnesia_table_event, _Event}, State) ->
    handle_mnesia_update(State);

handle_info({'DOWN', MonitorRef, process, RemotePid, noconnection}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("ℹ️ Unexpected message: ~p~n", [Info]),
    {noreply, State}.

%% Cleanup on termination
terminate(Reason, State) ->
    io:format("🛑 Enhanced CN Graphics Server terminating: ~p~n", [Reason]),
   
    % Close sockets
    if State#state.python_socket =/= undefined ->
        gen_tcp:close(State#state.python_socket);
    true -> ok
    end,
    
    if State#state.listen_socket =/= undefined ->
        gen_tcp:close(State#state.listen_socket);
    true -> ok
    end,
   
    % Stop socket acceptor
    if State#state.socket_acceptor =/= undefined ->
        exit(State#state.socket_acceptor, shutdown);
    true -> ok
    end,
   
    % Terminate GN graphics servers
    lists:foreach(fun({_Node, Pid}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                exit(Pid, shutdown);
            false -> ok
        end
    end, State#state.gn_graphics_servers),
   
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Socket Server Functions
%%%===================================================================

start_cn_socket_listener() ->
    case gen_tcp:listen(?PYTHON_SOCKET_PORT, [
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
                cn_socket_acceptor_loop(ListenSocket, MainProcess) 
            end),
            {ok, {ListenSocket, AcceptorPid}};
        {error, Reason} ->
            {error, Reason}
    end.

cn_socket_acceptor_loop(ListenSocket, MainProcess) ->
    io:format("🔌 CN Socket acceptor listening on port ~w~n", [?PYTHON_SOCKET_PORT]),
    accept_cn_connections(ListenSocket, MainProcess).

accept_cn_connections(ListenSocket, MainProcess) ->
    case gen_tcp:accept(ListenSocket, infinity) of
        {ok, ClientSocket} ->
            io:format("🔗 New CN client connected~n"),
            
            % Set socket options
            inet:setopts(ClientSocket, [
                binary,
                {active, false},
                {nodelay, true},
                {keepalive, true}
            ]),
            
            % Spawn client handler
            ClientPid = spawn_link(fun() -> 
                handle_cn_socket_client(ClientSocket, MainProcess) 
            end),
            
            % Transfer socket control to client handler
            gen_tcp:controlling_process(ClientSocket, ClientPid),
            
            % Notify main process
            MainProcess ! {socket_connected, ClientSocket, ClientPid},
            
            % Continue accepting
            accept_cn_connections(ListenSocket, MainProcess);
        {error, Reason} ->
            io:format("❌ CN Accept failed: ~p~n", [Reason]),
            timer:sleep(1000),
            accept_cn_connections(ListenSocket, MainProcess)
    end.

handle_cn_socket_client(Socket, MainProcess) ->
    inet:setopts(Socket, [{active, once}]),
    cn_socket_client_loop(Socket, MainProcess).

cn_socket_client_loop(Socket, MainProcess) ->
    receive
        {tcp, Socket, Data} ->
            io:format("📨 CN received data from client: ~p bytes~n", [byte_size(Data)]),
            inet:setopts(Socket, [{active, once}]),
            cn_socket_client_loop(Socket, MainProcess);
        {tcp_closed, Socket} ->
            io:format("🔌 CN client disconnected~n"),
            MainProcess ! {socket_disconnected, self()},
            gen_tcp:close(Socket);
        {tcp_error, Socket, Reason} ->
            io:format("❌ CN socket error: ~p~n", [Reason]),
            MainProcess ! {socket_disconnected, self()},
            gen_tcp:close(Socket);
        {send_data, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("❌ CN failed to send data: ~p~n", [Reason]),
                    MainProcess ! {socket_disconnected, self()},
                    gen_tcp:close(Socket)
            end,
            cn_socket_client_loop(Socket, MainProcess);
        shutdown ->
            gen_tcp:close(Socket);
        Other ->
            io:format("⚠️ CN socket client received unexpected message: ~p~n", [Other]),
            cn_socket_client_loop(Socket, MainProcess)
    after 30000 ->
        % 30 second keepalive
        case gen_tcp:send(Socket, <<>>) of
            ok ->
                cn_socket_client_loop(Socket, MainProcess);
            {error, _} ->
                io:format("🔌 CN client keepalive failed, disconnecting~n"),
                MainProcess ! {socket_disconnected, self()},
                gen_tcp:close(Socket)
        end
    end.

%%%===================================================================
%%% Socket Communication Functions
%%%===================================================================

send_map_to_socket(undefined, _MapState) ->
    ok;
send_map_to_socket(ClientPid, MapState) ->
    try
        % Create simple JSON-compatible data structure
        SimpleMessage = #{
            type => <<"map_update">>,
            timestamp => erlang:system_time(millisecond),
            data => create_simple_json_data(MapState)
        },
        
        % Encode to JSON
        JsonBinary = jsx:encode(SimpleMessage),
        
        % Send length prefix + data
        DataLength = byte_size(JsonBinary),
        LengthPrefix = <<DataLength:32/big>>,
        Message = <<LengthPrefix/binary, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        % Simple logging
        case get(cn_log_counter) of
            undefined -> put(cn_log_counter, 1);
            Counter when Counter >= 40 ->
                io:format("🗺️ JSON map sent via socket~n"),
                put(cn_log_counter, 1);
            Counter ->
                put(cn_log_counter, Counter + 1)
        end
    catch
        Error:Reason ->
            io:format("❌ Error sending JSON data: ~p:~p~n", [Error, Reason])
    end.


%% Helper function to create JSON-safe data
create_simple_json_data(MapState) ->
    #{
        map => convert_map_safely(maps:get(map, MapState, [])),
        dead_players => #{},
        update_time => erlang:system_time(millisecond),
        active_explosions => #{},
        backend_timing => #{
            tick_delay => 50,
            tile_move => 1200,
            ms_reduction => 200,
            immunity_time => 3000
        }
    }.

%% Convert map to JSON-safe format
convert_map_safely([]) ->
    create_empty_16x16_grid();
convert_map_safely(Map) when is_list(Map) ->
    try
        lists:map(fun(Row) when is_list(Row) ->
            lists:map(fun(Cell) ->
                convert_cell_safely(Cell)
            end, Row);
        (_) -> 
            []
        end, Map)
    catch
        _:_ ->
            % Return empty 16x16 grid if conversion fails
            create_empty_16x16_grid()
    end;
convert_map_safely(_) ->
    % Return empty 16x16 grid for invalid input
    create_empty_16x16_grid().

%% Helper function to create a proper 16x16 grid
create_empty_16x16_grid() ->
    EmptyCell = [<<"free">>, <<"none">>, <<"none">>, <<"none">>],
    EmptyRow = [EmptyCell || _ <- lists:seq(1, 16)],
    [EmptyRow || _ <- lists:seq(1, 16)].
%% Convert individual cell to JSON-safe format
convert_cell_safely({Tile, Powerup, Bomb, Player, Explosion, Special}) ->
    [
        safe_atom_to_binary(Tile),
        safe_atom_to_binary(Powerup),
        convert_bomb_safely(Bomb),
        convert_player_safely(Player)
    ];
convert_cell_safely(_) ->
    [<<"free">>, <<"none">>, <<"none">>, <<"none">>].

%% Safe atom to binary conversion
safe_atom_to_binary(Atom) when is_atom(Atom) ->
    try
        atom_to_binary(Atom, utf8)
    catch
        _:_ -> <<"unknown">>
    end;
safe_atom_to_binary(Other) when is_binary(Other) ->
    Other;
safe_atom_to_binary(Other) when is_list(Other) ->
    try
        list_to_binary(Other)
    catch
        _:_ -> <<"unknown">>
    end;
safe_atom_to_binary(_) ->
    <<"unknown">>.

%% Convert bomb info safely
convert_bomb_safely(none) ->
    <<"none">>;
convert_bomb_safely({Type, Ignited, Status, Radius, Owner, Movement, Direction}) ->
    [
        safe_atom_to_binary(Type),
        Ignited,
        safe_atom_to_binary(Status),
        ensure_integer(Radius),
        ensure_integer(Owner),
        Movement,
        safe_atom_to_binary(Direction)
    ];
convert_bomb_safely(_) ->
    <<"none">>.

%% Convert player info safely
convert_player_safely(none) ->
    <<"none">>;
convert_player_safely({PlayerID, Life, Speed, Direction, Movement, MovementTimer, ImmunityTimer, RequestTimer}) ->
    [
        ensure_integer(PlayerID),
        ensure_integer(Life),
        ensure_integer(Speed),
        safe_atom_to_binary(Direction),
        Movement,
        ensure_integer(MovementTimer),
        ensure_integer(ImmunityTimer),
        ensure_integer(RequestTimer)
    ];
convert_player_safely(_) ->
    <<"none">>.

%% Ensure value is an integer
ensure_integer(Val) when is_integer(Val) -> Val;
ensure_integer(Val) when is_list(Val) ->
    try
        list_to_integer(Val)
    catch
        _:_ -> 0
    end;
ensure_integer(Val) when is_binary(Val) ->
    try
        binary_to_integer(Val)
    catch
        _:_ -> 0
    end;
ensure_integer(_) -> 0.

send_movement_confirmation_to_socket(State, EntityType, EntityData) ->
    if State#state.python_socket_pid =/= undefined ->
        try
            JsonMessage = #{
                <<"type">> => <<"movement_confirmation">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"data">> => #{
                    <<"entity_type">> => atom_to_utf8_binary(EntityType),
                    <<"entity_data">> => convert_for_json(EntityData)
                }
            },
            
            JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
            DataLength = byte_size(JsonBinary),
            Message = <<DataLength:32/big, JsonBinary/binary>>,
            
            State#state.python_socket_pid ! {send_data, Message},
            
            case EntityType of
                player ->
                    PlayerID = maps:get(player_id, EntityData),
                    Duration = maps:get(total_duration, EntityData),
                    io:format("🏃 JSON player movement confirmation sent for player ~w (duration: ~wms)~n",
                             [PlayerID, Duration]);
                bomb ->
                    Pos = maps:get(from_pos, EntityData),
                    io:format("💣 JSON bomb movement confirmation sent for bomb at ~w~n", [Pos])
            end
        catch
            _:Error ->
                io:format("❌ Error sending movement confirmation via socket: ~p~n", [Error])
        end;
    true ->
        ok
    end.

send_timer_update_to_socket(State, EntityType, TimerData) ->
    if State#state.python_socket_pid =/= undefined ->
        try
            JsonMessage = #{
                <<"type">> => <<"timer_update">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"data">> => #{
                    <<"entity_type">> => atom_to_utf8_binary(EntityType),
                    <<"timer_data">> => convert_for_json(TimerData)
                }
            },
            
            JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
            DataLength = byte_size(JsonBinary),
            Message = <<DataLength:32/big, JsonBinary/binary>>,
            
            State#state.python_socket_pid ! {send_data, Message}
        catch
            _:Error ->
                io:format("❌ Error sending timer update via socket: ~p~n", [Error])
        end;
    true ->
        ok
    end.

send_fsm_update_to_socket(State, EntityType, FSMData) ->
    if State#state.python_socket_pid =/= undefined ->
        try
            JsonMessage = #{
                <<"type">> => <<"fsm_update">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"data">> => #{
                    <<"entity_type">> => atom_to_utf8_binary(EntityType),
                    <<"fsm_data">> => convert_for_json(FSMData)
                }
            },
            
            JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
            DataLength = byte_size(JsonBinary),
            Message = <<DataLength:32/big, JsonBinary/binary>>,
            
            State#state.python_socket_pid ! {send_data, Message}
        catch
            _:Error ->
                io:format("❌ Error sending FSM update via socket: ~p~n", [Error])
        end;
    true ->
        ok
    end.

send_explosion_event_to_socket(undefined, _ExplosionData) ->
    ok;
send_explosion_event_to_socket(ClientPid, ExplosionData) ->
    try
        JsonMessage = #{
            <<"type">> => <<"explosion_event">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => convert_for_json(ExplosionData)
        },
        
        JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
        DataLength = byte_size(JsonBinary),
        Message = <<DataLength:32/big, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        Coordinates = maps:get(coordinates, ExplosionData, []),
        io:format("💥 JSON explosion event sent via socket: ~w coordinates~n", [length(Coordinates)])
    catch
        _:Error ->
            io:format("❌ Error sending explosion event via socket: ~p~n", [Error])
    end.

send_death_event_to_socket(undefined, _DeathData) ->
    ok;
send_death_event_to_socket(ClientPid, DeathData) ->
    try
        JsonMessage = #{
            <<"type">> => <<"death_event">>,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"data">> => convert_for_json(DeathData)
        },
        
        JsonBinary = jsx:encode(JsonMessage, [return_maps, strict, {encoding, utf8}]),
        DataLength = byte_size(JsonBinary),
        Message = <<DataLength:32/big, JsonBinary/binary>>,
        
        ClientPid ! {send_data, Message},
        
        PlayerID = maps:get(player_id, DeathData),
        LocalGN = maps:get(local_gn, DeathData),
        io:format("💀 JSON death event sent via socket for player ~w on ~w~n", [PlayerID, LocalGN])
    catch
        _:Error ->
            io:format("❌ Error sending death event via socket: ~p~n", [Error])
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
%%% Helper Functions (Enhanced Death Detection)
%%%===================================================================

extract_player_id_from_key(Key) ->
    try
        case Key of
            PlayerID when is_integer(PlayerID) -> {ok, PlayerID};
            {PlayerID} when is_integer(PlayerID) -> {ok, PlayerID};
            {PlayerID, _} when is_integer(PlayerID) -> {ok, PlayerID};
            _ -> error
        end
    catch
        _:_ -> error
    end.

handle_enhanced_player_death(PlayerID, Table, State) ->
    CurrentTime = erlang:system_time(millisecond),
    LastKnownState = maps:get(PlayerID, State#state.last_known_players, undefined),
    
    LocalGN = case Table of
        gn1_players -> gn1;
        gn2_players -> gn2;
        gn3_players -> gn3;
        gn4_players -> gn4;
        _ -> unknown
    end,
    
    DeathRecord = {CurrentTime, LastKnownState, LocalGN},
    NewDeadPlayers = maps:put(PlayerID, DeathRecord, State#state.dead_players),
    NewLastKnown = maps:remove(PlayerID, State#state.last_known_players),
    
    if LastKnownState =/= undefined ->
        #mnesia_players{
            position = Position,
            life = Life,
            speed = Speed,
            immunity_timer = ImmunityTimer
        } = LastKnownState,
        io:format("💀 Player ~w died! (was on ~w at ~w with ~w life, speed ~w, immunity ~w)~n", 
                  [PlayerID, LocalGN, Position, Life, Speed, ImmunityTimer]);
    true ->
        io:format("💀 Player ~w died! (was on ~w, no last known state)~n", [PlayerID, LocalGN])
    end,
    
    send_death_event_to_socket(State#state.python_socket_pid, #{
        player_id => PlayerID,
        death_time => CurrentTime,
        local_gn => LocalGN,
        last_known_state => case LastKnownState of
            undefined -> null;
            _ -> convert_for_json(#{
                position => LastKnownState#mnesia_players.position,
                life => LastKnownState#mnesia_players.life,
                speed => LastKnownState#mnesia_players.speed,
                immunity_timer => LastKnownState#mnesia_players.immunity_timer
            })
        end
    }),
    
    State#state{
        dead_players = NewDeadPlayers,
        last_known_players = NewLastKnown
    }.

detect_enhanced_player_movement_change(NewRecord, CurrentMapState) ->
    #mnesia_players{
        player_number = PlayerNum,
        position = [X, Y],
        direction = Direction,
        movement = Movement,
        speed = Speed,
        movement_timer = MovementTimer,
        immunity_timer = ImmunityTimer,
        request_timer = RequestTimer
    } = NewRecord,
   
    case Movement of
        true when Direction =/= none, MovementTimer > 0 ->
            TotalDuration = ?TILE_MOVE - (Speed - 1) * ?MS_REDUCTION,
            Destination = calculate_destination([X, Y], Direction),
            PlayerData = #{
                player_id => PlayerNum,
                from_pos => [X, Y],
                to_pos => Destination,
                direction => Direction,
                speed => Speed,
                movement_timer => MovementTimer,
                total_duration => TotalDuration,
                immunity_timer => ImmunityTimer,
                request_timer => RequestTimer,
                movement_confirmed => true
            },
            {movement_started, PlayerData};
        _ ->
            if MovementTimer > 0 orelse ImmunityTimer > 0 orelse RequestTimer > 0 ->
                TimerData = #{
                    player_id => PlayerNum,
                    movement_timer => MovementTimer,
                    immunity_timer => ImmunityTimer,
                    request_timer => RequestTimer,
                    position => [X, Y],
                    speed => Speed
                },
                {timer_update, TimerData};
            true ->
                no_movement_change
            end
    end.

detect_enhanced_bomb_movement_change(NewRecord, CurrentMapState) ->
    #mnesia_bombs{
        position = [X, Y],
        movement = Movement,
        direction = Direction,
        type = Type,
        owner = Owner,
        radius = Radius,
        status = Status,
        ignited = Ignited
    } = NewRecord,
   
    case Movement of
        true when Direction =/= none ->
            Destination = calculate_destination([X, Y], Direction),
            BombData = #{
                bomb_id => [X, Y],
                from_pos => [X, Y],
                to_pos => Destination,
                direction => Direction,
                type => Type,
                owner => Owner,
                radius => Radius,
                status => Status,
                ignited => Ignited,
                movement_confirmed => true
            },
            {movement_started, BombData};
        _ ->
            FSMData = #{
                bomb_id => [X, Y],
                position => [X, Y],
                type => Type,
                status => Status,
                ignited => Ignited,
                owner => Owner,
                radius => Radius
            },
            {fsm_state_change, FSMData}
    end.

calculate_destination([X, Y], Direction) ->
    case Direction of
        up -> [X, Y-1];
        down -> [X, Y+1];
        left -> [X-1, Y];
        right -> [X+1, Y]
    end.

handle_mnesia_update(State) ->
    NewMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState}.

send_map_to_all_targets(State) ->
    send_map_to_socket(State#state.python_socket_pid, State#state.current_map_state),
    send_enhanced_map_to_gn_servers(State).

send_enhanced_map_to_gn_servers(State) ->
    lists:foreach(fun(ServerInfo) ->
        case ServerInfo of
            {_Ref, Node, Pid} when is_pid(Pid) ->
                % We have the actual PID
                try
                    gen_server:cast(Pid, {map_update, State#state.current_map_state})
                    % io:format("✅ Sent map to ~w via PID~n", [Node])
                catch
                    _:Error ->
                        io:format("❌ PID cast failed to ~w: ~p~n", [Node, Error])
                end;
            {_Ref, Node} ->
                % Fall back to registered name
                try
                    gen_server:cast({gn_graphics_server, Node}, {map_update, State#state.current_map_state}),
                    io:format("✅ Sent map to ~w via registered name~n", [Node])
                catch
                    _:Error ->
                        io:format("❌ Registered name cast failed to ~w: ~p~n", [Node, Error])
                end
        end
    end, State#state.gn_graphics_servers).

%%%===================================================================
%%% Map Creation Functions (Same as before)
%%%===================================================================

create_enhanced_map_state(State) ->
    try
        EmptyMap = create_empty_map(),
        MapWithTiles = add_tiles_to_map(EmptyMap),
        MapWithPowerups = add_powerups_to_map(MapWithTiles),
        MapWithBombs = add_enhanced_bombs_to_map(MapWithPowerups),
        MapWithPlayers = add_enhanced_players_to_map(MapWithBombs),
        MapWithExplosions = add_explosions_to_map(MapWithPlayers, State#state.active_explosions),
       
        #{
            map => MapWithExplosions,
            dead_players => State#state.dead_players,
            update_time => erlang:system_time(millisecond),
            active_explosions => State#state.active_explosions,
            backend_timing => #{
                tile_move => ?TILE_MOVE,
                ms_reduction => ?MS_REDUCTION,
                immunity_time => ?IMMUNITY_TIME,
                request_cooldown => ?REQUEST_COOLDOWN,
                tick_delay => ?TICK_DELAY,
                explosion_display_time => ?EXPLOSION_DISPLAY_TIME
            }
        }
    catch
        _:Error ->
            io:format("❌ Error creating enhanced map state: ~p~n", [Error]),
            #{
                map => create_empty_map(),
                dead_players => #{},
                update_time => erlang:system_time(millisecond),
                active_explosions => #{},
                backend_timing => #{}
            }
    end.

get_all_tables() ->
    [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles,
     gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs,
     gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups,
     gn1_players, gn2_players, gn3_players, gn4_players].

setup_mnesia_subscriptions(Tables) ->
    lists:foldl(fun(Table, Acc) ->
        case mnesia:subscribe({table, Table, simple}) of
            {ok, _} ->
                io:format("✅ Subscribed to table: ~w~n", [Table]),
                [Table | Acc];
            {error, Reason} ->
                io:format("❌ Failed to subscribe to ~w: ~p~n", [Table, Reason]),
                Acc
        end
    end, [], Tables).

monitor_gn_graphics_servers(GNNodes)->
    RefsList = attempt_gn_graphics_monitoring(GNNodes),
    gn_monitoring_receive_loop(RefsList, []).

gn_monitoring_receive_loop(RefsList, ServersNotFound) ->
    receive
        {'DOWN', Ref, process, _Pid, noproc} ->
            case lists:keyfind(Ref, 1, RefsList) of
                false ->
                    io:format("❌ *Unknown monitoring failure message received:~n~w~n",[{'DOWN', Ref, process, _Pid, noproc}]),
                    gn_monitoring_receive_loop(RefsList, ServersNotFound);
                {_, NodeName} ->
                    io:format("❌ *A monitoring request has failed on node ~w. Accumulating before retrying..~n",[NodeName]),
                    gn_monitoring_receive_loop(lists:keydelete(Ref, 1, RefsList), [NodeName | ServersNotFound])
            end;
        AnythingElse ->
            erlang:send_after(5000, self(), AnythingElse),
            gn_monitoring_receive_loop(RefsList, ServersNotFound)
     after 1500 ->
        case length(ServersNotFound) =/= 0 of
            true ->
                io:format("❌ Failed to monitor ~p servers. The following were not monitored:~w~n", [length(ServersNotFound), ServersNotFound]),
                NewRefs = monitor_gn_graphics_servers(ServersNotFound),
                NewRefs ++ RefsList;
            false ->
                io:format("✅ All GN graphics server were monitored successfully!~n"),
                RefsList
         end
    end.

attempt_gn_graphics_monitoring(NodeList) ->
    io:format("🔍 Attempting to monitor GN graphics servers on nodes: ~p~n", [NodeList]),
    
    RefsList = lists:map(fun(Node) ->
        try
            % Try to get the actual PID first
            case rpc:call(Node, erlang, whereis, [gn_graphics_server], 5000) of
                {badrpc, Reason} ->
                    io:format("❌ RPC failed to ~w: ~p~n", [Node, Reason]),
                    MonitorRef = erlang:monitor(process, {gn_graphics_server, Node}),
                    {MonitorRef, Node};
                undefined ->
                    io:format("⚠️ gn_graphics_server not found on ~w~n", [Node]),
                    MonitorRef = erlang:monitor(process, {gn_graphics_server, Node}),
                    {MonitorRef, Node};
                Pid when is_pid(Pid) ->
                    io:format("✅ Found gn_graphics_server on ~w: ~p~n", [Node, Pid]),
                    MonitorRef = erlang:monitor(process, Pid),
                    {MonitorRef, Node, Pid}  % Store the PID too!
            end
        catch
            _:Error ->
                io:format("❌ Error monitoring ~w: ~p~n", [Node, Error]),
                ErrorRef = make_ref(),
                {ErrorRef, Node}
        end
    end, NodeList),
    
    timer:sleep(1000),
    RefsList.

create_empty_map() ->
    [[{free, none, none, none, none, none} || _ <- lists:seq(1, ?MAP_SIZE)]
     || _ <- lists:seq(1, ?MAP_SIZE)].

add_tiles_to_map(Map) ->
    TileTables = [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles],
    lists:foldl(fun(Table, AccMap) ->
        add_tiles_from_table(Table, AccMap)
    end, Map, TileTables).

add_tiles_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_tiles{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        TileRecords when is_list(TileRecords) ->
            lists:foldl(fun(TileRecord, AccMap) ->
                update_map_with_tile(AccMap, TileRecord)
            end, Map, TileRecords);
        Error ->
            io:format("❌ Error reading table ~w: ~p~n", [Table, Error]),
            Map
    end.

update_map_with_tile(Map, TileRecord) ->
    #mnesia_tiles{position = [X, Y], type = Type, contains = Contains} = TileRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_tile(OldCell, Type, Contains),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid tile position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_cell_tile({_, Powerup, Bomb, Player, Explosion, Special}, TileType, Contains) ->
    ActualPowerup = if Contains =/= none -> Contains; true -> Powerup end,
    {TileType, ActualPowerup, Bomb, Player, Explosion, Special}.

replace_list_element(List, Pos, NewElement) ->
    {Before, [_|After]} = lists:split(Pos - 1, List),
    Before ++ [NewElement] ++ After.

add_powerups_to_map(Map) ->
    PowerupTables = [gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups],
    lists:foldl(fun(Table, AccMap) ->
        add_powerups_from_table(Table, AccMap)
    end, Map, PowerupTables).

add_powerups_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_powerups{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        PowerupRecords when is_list(PowerupRecords) ->
            lists:foldl(fun(PowerupRecord, AccMap) ->
                update_map_with_powerup(AccMap, PowerupRecord)
            end, Map, PowerupRecords);
        Error ->
            io:format("❌ Error reading powerup table ~w: ~p~n", [Table, Error]),
            Map
    end.

update_map_with_powerup(Map, PowerupRecord) ->
    #mnesia_powerups{position = [X, Y], type = Type} = PowerupRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_powerup(OldCell, Type),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid powerup position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_cell_powerup({Tile, _, Bomb, Player, Explosion, Special}, PowerupType) ->
    {Tile, PowerupType, Bomb, Player, Explosion, Special}.

add_enhanced_players_to_map(Map) ->
    PlayerTables = [gn1_players, gn2_players, gn3_players, gn4_players],
    lists:foldl(fun(Table, AccMap) ->
        add_enhanced_players_from_table(Table, AccMap)
    end, Map, PlayerTables).

add_enhanced_players_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_players{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        PlayerRecords when is_list(PlayerRecords) ->
            lists:foldl(fun(PlayerRecord, AccMap) ->
                update_map_with_enhanced_player(AccMap, PlayerRecord)
            end, Map, PlayerRecords);
        Error ->
            io:format("❌ Error reading player table ~w: ~p~n", [Table, Error]),
            Map
    end.

update_map_with_enhanced_player(Map, PlayerRecord) ->
    #mnesia_players{
        position = [X, Y], 
        player_number = PlayerID,
        life = Life, 
        speed = Speed,
        direction = Direction,
        movement = Movement,
        movement_timer = MovementTimer,
        immunity_timer = ImmunityTimer,
        request_timer = RequestTimer
    } = PlayerRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        
        EnhancedPlayerInfo = {
            PlayerID, Life, Speed, Direction, Movement, 
            MovementTimer, ImmunityTimer, RequestTimer
        },
        
        NewCell = update_cell_enhanced_player(OldCell, EnhancedPlayerInfo),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid player position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_cell_enhanced_player({Tile, Powerup, Bomb, _, Explosion, Special}, EnhancedPlayerInfo) ->
    {Tile, Powerup, Bomb, EnhancedPlayerInfo, Explosion, Special}.

add_enhanced_bombs_to_map(Map) ->
    BombTables = [gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs],
    lists:foldl(fun(Table, AccMap) ->
        add_enhanced_bombs_from_table(Table, AccMap)
    end, Map, BombTables).

add_enhanced_bombs_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_bombs{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        BombRecords when is_list(BombRecords) ->
            lists:foldl(fun(BombRecord, AccMap) ->
                update_map_with_enhanced_bomb(AccMap, BombRecord)
            end, Map, BombRecords);
        Error ->
            io:format("❌ Error reading bomb table ~w: ~p~n", [Table, Error]),
            Map
    end.

update_map_with_enhanced_bomb(Map, BombRecord) ->
    #mnesia_bombs{
        position = [X, Y], 
        type = Type, 
        ignited = Ignited,
        status = Status, 
        radius = Radius, 
        owner = Owner,
        movement = Movement,
        direction = Direction
    } = BombRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        
        EnhancedBombInfo = {
            Type, Ignited, Status, Radius, Owner, Movement, Direction
        },
        
        NewCell = update_cell_enhanced_bomb(OldCell, EnhancedBombInfo),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid bomb position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_cell_enhanced_bomb({Tile, Powerup, _, Player, Explosion, Special}, EnhancedBombInfo) ->
    {Tile, Powerup, EnhancedBombInfo, Player, Explosion, Special}.

add_explosions_to_map(Map, ActiveExplosions) ->
    maps:fold(fun([X, Y], _ExpiryTime, AccMap) ->
        update_map_with_explosion(AccMap, X, Y)
    end, Map, ActiveExplosions).

update_map_with_explosion(Map, X, Y) ->
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_explosion(OldCell, explosion),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid explosion position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_cell_explosion({Tile, Powerup, Bomb, Player, _, Special}, ExplosionInfo) ->
    {Tile, Powerup, Bomb, Player, ExplosionInfo, Special}.

wait_for_cn_server_and_notify() ->
    spawn(fun() ->
        wait_for_cn_server_loop(50) % Try for 50 attempts (25 seconds max)
    end).

wait_for_cn_server_loop(0) ->
    io:format("❌ CN server not found after maximum retries~n");
wait_for_cn_server_loop(AttemptsLeft) ->
    case global:whereis_name(cn_server) of
        undefined ->
            io:format("⏳ Waiting for CN server... (~w attempts left)~n", [AttemptsLeft]),
            timer:sleep(500), % Wait 500ms between attempts
            wait_for_cn_server_loop(AttemptsLeft - 1);
        Pid ->
            io:format("✅ Found CN server, sending ready message~n"),
            Pid ! {graphics_ready, self()}
    end.
