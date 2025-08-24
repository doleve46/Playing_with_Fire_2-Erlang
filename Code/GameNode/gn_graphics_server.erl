-module(gn_graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common_parameters.hrl").

-record(state, {
    cn_node,                    % Central node
    python_port,                % Port to local Python visualizer
    update_counter = 0,         % Update counter
    current_map_state,          % Current enhanced map state received from CN
    dead_players = #{},         % Track dead players received from CN
    last_update_time = 0,       % Track when we last received an update
    local_gn_name,              % This GN's identifier (gn1, gn2, gn3, gn4)
    backend_timing = #{},       % Backend timing constants received from CN
    local_player_ids = []       % Player IDs that belong to this GN
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
    
    % Monitor the CN node
    monitor_node(CNNode, true),
    
    % Create Python port after a short delay
    erlang:send_after(50, self(), create_python_port),
    
    io:format("✅ Enhanced GN Graphics Server initialized (waiting for CN updates)~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(get_dead_players, _From, State) ->
    {reply, State#state.dead_players, State};

handle_call(get_local_info, _From, State) ->
    LocalInfo = #{
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        backend_timing => State#state.backend_timing,
        update_counter => State#state.update_counter
    },
    {reply, LocalInfo, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, EnhancedMapState}, State) ->
    % Received enhanced map update from CN graphics server
    CurrentTime = erlang:system_time(millisecond),
    
    % Handle enhanced format with timing and death information
    {ActualMapState, DeadPlayers, BackendTiming} = case EnhancedMapState of
        #{map := GridData, dead_players := DeadPlayersMap, backend_timing := Timing} ->
            % New enhanced format with death information and backend timing
            io:format("🗺️ GN received enhanced map update from CN (#~w) with timing & death info~n", 
                      [State#state.update_counter + 1]),
            
            % Check for newly dead players
            NewDeaths = maps:filter(fun(PlayerID, _DeathInfo) ->
                not maps:is_key(PlayerID, State#state.dead_players)
            end, DeadPlayersMap),
            
            if map_size(NewDeaths) > 0 ->
                NewDeathList = maps:to_list(NewDeaths),
                io:format("💀 New deaths detected by GN: ~p~n", [NewDeathList]),
                
                % Check if any deaths are for local players
                lists:foreach(fun({PlayerID, {DeathTime, _LastState, LocalGNAtom}}) ->
                    if LocalGNAtom =:= State#state.local_gn_name ->
                        io:format("🩸 LOCAL PLAYER ~w DIED on this GN! (Death time: ~w)~n", [PlayerID, DeathTime]);
                    true ->
                        io:format("💀 Remote player ~w died on ~w~n", [PlayerID, LocalGNAtom])
                    end
                end, NewDeathList);
            true -> ok
            end,
            
            {GridData, DeadPlayersMap, Timing};
        #{map := GridData, dead_players := DeadPlayersMap} ->
            % Format without backend timing
            io:format("🗺️ GN received map update from CN (#~w) with death info~n", 
                      [State#state.update_counter + 1]),
            {GridData, DeadPlayersMap, State#state.backend_timing};
        _ ->
            % Old format - just the grid
            io:format("🗺️ GN received basic map update from CN (#~w)~n", 
                      [State#state.update_counter + 1]),
            {EnhancedMapState, State#state.dead_players, State#state.backend_timing}
    end,
    
    % Create enhanced data for local Python visualizer with GN-specific information
    LocalEnhancedMapData = #{
        map => ActualMapState,
        dead_players => DeadPlayers,
        update_time => CurrentTime,
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        backend_timing => BackendTiming
    },
    
    send_enhanced_map_to_python(State#state.python_port, LocalEnhancedMapData),
    
    NewState = State#state{
        current_map_state = LocalEnhancedMapData,
        dead_players = DeadPlayers,
        backend_timing = BackendTiming,
        update_counter = State#state.update_counter + 1,
        last_update_time = CurrentTime
    },
    {noreply, NewState};

% Handle movement confirmations from CN
handle_cast({movement_confirmation, ConfirmationData}, State) ->
    % Forward movement confirmations directly to Python
    send_movement_confirmation_to_python(State#state.python_port, ConfirmationData),
    {noreply, State};

% Handle timer updates from CN
handle_cast({timer_update, TimerData}, State) ->
    % Forward timer updates directly to Python
    send_timer_update_to_python(State#state.python_port, TimerData),
    {noreply, State};

% Handle FSM updates from CN
handle_cast({fsm_update, FSMData}, State) ->
    % Forward FSM updates directly to Python
    send_fsm_update_to_python(State#state.python_port, FSMData),
    {noreply, State};

handle_cast(force_update, State) ->
    % Force update - just resend current state if we have it
    case State#state.current_map_state of
        undefined ->
            io:format("⚠️ No enhanced map state available for force update~n");
        MapState ->
            io:format("🔄 Force updating Python with current enhanced map state~n"),
            send_enhanced_map_to_python(State#state.python_port, MapState)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(create_python_port, State) ->
    % Create Python port for enhanced visualizer
    io:format("🐍 Creating enhanced Python visualizer port...~n"),
    Port = create_enhanced_python_port(State#state.local_gn_name),
    
    UpdatedState = State#state{python_port = Port},
    
    % If we already have enhanced map state from CN, send it
    case State#state.current_map_state of
        undefined ->
            io:format("✅ Enhanced Python port created, waiting for first CN update~n");
        MapState ->
            send_enhanced_map_to_python(Port, MapState),
            io:format("✅ Enhanced Python port created and current map state sent~n")
    end,
    
    {noreply, UpdatedState};

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from enhanced Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Enhanced Python port closed, restarting...~n"),
    NewPort = create_enhanced_python_port(State#state.local_gn_name),
    
    % Resend current enhanced map state if available
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> send_enhanced_map_to_python(NewPort, MapState)
    end,
    
    {noreply, State#state{python_port = NewPort}};

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
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Enhanced GN Identification Functions
%%%===================================================================

%% Determine which GN this server is running on
determine_local_gn() ->
    % Method 1: Try environment variable first
    case os:getenv("GN_ID") of
        false ->
            % Method 2: Check node name pattern
            NodeName = atom_to_list(node()),
            case NodeName of
                "gn1" ++ _ -> gn1;
                "gn2" ++ _ -> gn2;
                "gn3" ++ _ -> gn3;
                "gn4" ++ _ -> gn4;
                _ ->
                    % Method 3: Default fallback (should be configured properly in deployment)
                    io:format("⚠️ Could not determine GN ID from node name ~p, defaulting to gn1~n", [NodeName]),
                    gn1
            end;
        GNStr ->
            list_to_atom(GNStr)
    end.

%% Get the player IDs that belong to this GN
get_local_player_ids(LocalGN) ->
    % Map GN nodes to player IDs based on game design
    GNToPlayers = #{
        gn1 => [1],  % Player 1 is managed by GN1
        gn2 => [2],  % Player 2 is managed by GN2
        gn3 => [3],  % Player 3 is managed by GN3
        gn4 => [4]   % Player 4 is managed by GN4
    },
    maps:get(LocalGN, GNToPlayers, []).

%%%===================================================================
%%% Enhanced Python Port Communication
%%%===================================================================

%% Create enhanced Python port for GN-specific visualizer
create_enhanced_python_port(LocalGN) ->
    try
        % Set environment variable to identify which GN this is
        os:putenv("GN_ID", atom_to_list(LocalGN)),
        
        % Use the enhanced Python visualizer for GN nodes with death detection
        Port = open_port({spawn, "python3 enhanced_gn_map_live.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Enhanced Python visualizer port created for ~w~n", [LocalGN]),
        
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create enhanced Python port: ~p~n", [Error]),
            undefined
    end.

%% Send enhanced map data to Python visualizer
send_enhanced_map_to_python(undefined, _MapState) ->
    io:format("⚠️ No enhanced Python port available~n");

send_enhanced_map_to_python(Port, MapState) ->
    try
        % Send enhanced map state as binary term
        MapBinary = term_to_binary(MapState),
        port_command(Port, MapBinary),
        
        % Log details about what we're sending (less frequent to reduce spam)
        case MapState of
            #{dead_players := DeadPlayers, local_gn := LocalGN, backend_timing := Timing} ->
                DeadCount = maps:size(DeadPlayers),
                TimingKeys = maps:keys(Timing),
                % Only log every 40 updates (every 2 seconds at 50ms intervals)
                case get(log_counter) of
                    undefined -> put(log_counter, 1);
                    Counter when Counter >= 40 ->
                        io:format("📤 Enhanced map forwarded to Python visualizer (~w, dead: ~w, timing: ~w)~n", 
                                 [LocalGN, DeadCount, TimingKeys]),
                        put(log_counter, 1);
                    Counter ->
                        put(log_counter, Counter + 1)
                end;
            _ ->
                ok
        end
    catch
        _:Error ->
            io:format("❌ Error sending enhanced data to Python: ~p~n", [Error])
    end.

%% Send movement confirmation to Python visualizer
send_movement_confirmation_to_python(undefined, _ConfirmationData) ->
    io:format("⚠️ No Python port available for movement confirmation~n");

send_movement_confirmation_to_python(Port, ConfirmationData) ->
    try
        ConfirmationMsg = [movement_confirmation, ConfirmationData],
        MsgBinary = term_to_binary(ConfirmationMsg),
        port_command(Port, MsgBinary),
        
        case ConfirmationData of
            #{entity_type := player, entity_data := #{player_id := PlayerID}} ->
                io:format("🏃 Movement confirmation forwarded for player ~w~n", [PlayerID]);
            #{entity_type := bomb, entity_data := #{from_pos := Pos}} ->
                io:format("💣 Movement confirmation forwarded for bomb at ~w~n", [Pos]);
            _ ->
                io:format("📤 Movement confirmation forwarded~n")
        end
    catch
        _:Error ->
            io:format("❌ Error sending movement confirmation to Python: ~p~n", [Error])
    end.

%% Send timer update to Python visualizer
send_timer_update_to_python(undefined, _TimerData) ->
    ok;

send_timer_update_to_python(Port, TimerData) ->
    try
        TimerMsg = [timer_update, TimerData],
        MsgBinary = term_to_binary(TimerMsg),
        port_command(Port, MsgBinary)
    catch
        _:Error ->
            io:format("❌ Error sending timer update to Python: ~p~n", [Error])
    end.

%% Send FSM update to Python visualizer
send_fsm_update_to_python(undefined, _FSMData) ->
    ok;

send_fsm_update_to_python(Port, FSMData) ->
    try
        FSMMsg = [fsm_update, FSMData],
        MsgBinary = term_to_binary(FSMMsg),
        port_command(Port, MsgBinary)
    catch
        _:Error ->
            io:format("❌ Error sending FSM update to Python: ~p~n", [Error])
    end.

%%%===================================================================
%%% Additional Enhanced Helper Functions
%%%===================================================================

%% Get enhanced statistics about current state
get_enhanced_state_statistics(State) ->
    #{
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        update_counter => State#state.update_counter,
        dead_players_count => maps:size(State#state.dead_players),
        last_update_time => State#state.last_update_time,
        has_map_state => State#state.current_map_state =/= undefined,
        has_python_port => State#state.python_port =/= undefined,
        has_backend_timing => maps:size(State#state.backend_timing) > 0,
        backend_timing_keys => maps:keys(State#state.backend_timing)
    }.

%% Enhanced debug function to print current state
debug_enhanced_state(State) ->
    Stats = get_enhanced_state_statistics(State),
    io:format("🔍 Enhanced GN Graphics Server Debug State:~n"),
    io:format("   Local GN: ~w~n", [maps:get(local_gn, Stats)]),
    io:format("   Local Players: ~w~n", [maps:get(local_players, Stats)]),
    io:format("   Update Counter: ~w~n", [maps:get(update_counter, Stats)]),
    io:format("   Dead Players: ~w~n", [maps:get(dead_players_count, Stats)]),
    io:format("   Has Map State: ~w~n", [maps:get(has_map_state, Stats)]),
    io:format("   Has Python Port: ~w~n", [maps:get(has_python_port, Stats)]),
    io:format("   Has Backend Timing: ~w~n", [maps:get(has_backend_timing, Stats)]),
    io:format("   Backend Timing Keys: ~w~n", [maps:get(backend_timing_keys, Stats)]),
    io:format("   Last Update: ~w~n", [maps:get(last_update_time, Stats)]),
    
    % Show dead players details
    if maps:get(dead_players_count, Stats) > 0 ->
        io:format("   💀 Dead Players Details:~n"),
        maps:fold(fun(PlayerID, {DeathTime, _LastState, LocalGNAtom}, _Acc) ->
            IsLocal = lists:member(PlayerID, State#state.local_player_ids),
            LocalStr = if IsLocal -> " (LOCAL)"; true -> "" end,
            io:format("      Player ~w died at ~w on ~w~s~n", [PlayerID, DeathTime, LocalGNAtom, LocalStr])
        end, ok, State#state.dead_players);
    true -> ok
    end,
    
    % Show backend timing if available
    if maps:size(State#state.backend_timing) > 0 ->
        io:format("   ⏱️ Backend Timing Constants:~n"),
        maps:fold(fun(Key, Value, _Acc) ->
            io:format("      ~w: ~w ms~n", [Key, Value])
        end, ok, State#state.backend_timing);
    true -> ok
    end.

%% Check if a player is local to this GN
is_local_player(PlayerID, State) ->
    lists:member(PlayerID, State#state.local_player_ids).

%% Get local dead players only
get_local_dead_players(State) ->
    maps:filter(fun(PlayerID, _DeathInfo) ->
        is_local_player(PlayerID, State)
    end, State#state.dead_players).
