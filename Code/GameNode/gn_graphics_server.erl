-module(gn_graphics_server).
-behaviour(gen_server).
%% TODO: Need to verify that a python port is created through cn_graphics and gn_graphics, and is responsive and works as intended.

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../common_parameters.hrl").

-record(state, {
    cn_node,                    % Central node
    python_port,                % Port to local Python visualizer
    update_counter = 0,         % Update counter
    current_map_state,          % Current enhanced map state received from CN
    dead_players = #{},         % Track dead players received from CN
    last_update_time = 0,       % Track when we last received an update
    local_gn_name,              % This GN's identifier (gn1, gn2, gn3, gn4)
    backend_timing = #{},       % Backend timing constants received from CN
    local_player_ids = [],      % Player IDs that belong to this GN
    active_explosions = #{}     % Track active explosions received from CN
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
    
    %% trap exits
    process_flag(trap_exit, true),
    
    % Monitor nodes for CN reconnection scenarios
    net_kernel:monitor_nodes(true),
    
    % Create Python port after a short delay
    erlang:send_after(50, self(), create_python_port),
    
    % Request full map from CN immediately (assuming CN is up)
    erlang:send_after(100, self(), request_cn_connection),
    
    io:format("✅ Enhanced GN Graphics Server initialized~n"),
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
handle_cast({map_update, CompleteMapState}, State) ->
    io:format("📡 GN Graphics Server received map update from CN~n"),
    
    % Store the complete map state locally (resetting our state)
    CurrentTime = erlang:system_time(millisecond),
    
    % Extract components from the complete map state
    {GridData, DeadPlayers, BackendTiming, ActiveExplosions} = case CompleteMapState of
        #{map := Grid, dead_players := Deaths, backend_timing := Timing, active_explosions := Explosions} ->
            {Grid, Deaths, Timing, Explosions};
        #{map := Grid, dead_players := Deaths, backend_timing := Timing} ->
            {Grid, Deaths, Timing, #{}};
        #{map := Grid, dead_players := Deaths} ->
            {Grid, Deaths, #{}, #{}};
        _ ->
            % Fallback for unexpected format
            {CompleteMapState, #{}, #{}, #{}}
    end,
    
    % Update local state with complete new state
    NewState = State#state{
        current_map_state = CompleteMapState,
        dead_players = DeadPlayers,
        backend_timing = BackendTiming,
        active_explosions = ActiveExplosions,
        update_counter = State#state.update_counter + 1,
        last_update_time = CurrentTime
    },
    
    % Convert to JSON and send to Python visualizer
    send_enhanced_map_to_python(State#state.python_port, CompleteMapState),
    
    % Log updates occasionally
    case NewState#state.update_counter rem 20 == 0 of
        true ->
            ExplosionCount = maps:size(ActiveExplosions),
            DeathCount = maps:size(DeadPlayers),
            io:format("🔄 GN processed update #~w (~w explosions, ~w dead players)~n", 
                      [NewState#state.update_counter, ExplosionCount, DeathCount]);
        false -> ok
    end,
    
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
            ExplosionCount = maps:size(State#state.active_explosions),
            io:format("🔄 Force updating Python with current enhanced map state (~w explosions)~n", [ExplosionCount]),
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

handle_info(request_cn_connection, State) ->
    io:format("🔌 Requesting full map connection from CN graphics server...~n"),
    try
        % Request full map from CN graphics server
        case cn_server_graphics:request_full_map(node(), self()) of
            FullMapState when is_map(FullMapState) ->
                io:format("✅ Received full map state from CN graphics server~n"),
                
                % Reset local state completely and store new state
                NewState = State#state{
                    current_map_state = FullMapState,
                    dead_players = maps:get(dead_players, FullMapState, #{}),
                    backend_timing = maps:get(backend_timing, FullMapState, #{}),
                    active_explosions = maps:get(active_explosions, FullMapState, #{}),
                    update_counter = 0,
                    last_update_time = erlang:system_time(millisecond)
                },
                
                % Send the full map to Python visualizer as JSON
                send_enhanced_map_to_python(State#state.python_port, FullMapState),
                io:format("📡 Full map state sent to local Python visualizer~n"),
                
                {noreply, NewState};
            Error ->
                io:format("❌ Failed to get full map from CN: ~p, retrying in 2 seconds...~n", [Error]),
                erlang:send_after(2000, self(), request_cn_connection),
                {noreply, State}
        end
    catch
        _:RequestError ->
            io:format("❌ Error requesting full map from CN: ~p, retrying in 2 seconds...~n", [RequestError]),
            erlang:send_after(2000, self(), request_cn_connection),
            {noreply, State}
    end;

handle_info({nodedown, Node}, State) when Node == State#state.cn_node ->
    io:format("⚠️ CN node ~w went down~n", [Node]),
    {noreply, State};

handle_info({nodeup, Node}, State) when Node == State#state.cn_node ->
    io:format("✅ CN node ~w came back up, requesting reconnection...~n", [Node]),
    % Request full map to reconnect
    erlang:send_after(100, self(), request_cn_connection),
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
                "GN1" ++ _ -> gn1;
                "GN2" ++ _ -> gn2;
                "GN3" ++ _ -> gn3;
                "GN4" ++ _ -> gn4;
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
        
        % Use Python 3.6 with manually installed dataclasses
        Port = open_port({spawn, "python3 src/Code/Map/gn_map_live.py"}, 
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
        % Convert Erlang map to JSON using jiffy
        JsonData = jiffy:encode(MapState),
        
        % Send JSON data to Python port
        port_command(Port, JsonData),
        
        % Log details about what we're sending (less frequent to reduce spam)
        case MapState of
            #{dead_players := DeadPlayers, backend_timing := Timing, active_explosions := Explosions} ->
                DeadCount = maps:size(DeadPlayers),
                ExplosionCount = maps:size(Explosions),
                TimingKeys = maps:keys(Timing),
                % Only log every 40 updates (every 2 seconds at 50ms intervals)
                case get(log_counter) of
                    undefined -> put(log_counter, 1);
                    Counter when Counter >= 40 ->
                        io:format("📤 JSON map sent to Python visualizer (dead: ~w, explosions: ~w, timing: ~w)~n", 
                                 [DeadCount, ExplosionCount, TimingKeys]),
                        put(log_counter, 1);
                    Counter ->
                        put(log_counter, Counter + 1)
                end;
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
        active_explosions_count => maps:size(State#state.active_explosions),
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
    io:format("   Active Explosions: ~w~n", [maps:get(active_explosions_count, Stats)]),
    io:format("   Has Map State: ~w~n", [maps:get(has_map_state, Stats)]),
    io:format("   Has Python Port: ~w~n", [maps:get(has_python_port, Stats)]),
    io:format("   Has Backend Timing: ~w~n", [maps:get(has_backend_timing, Stats)]),
    io:format("   Backend Timing Keys: ~w~n", [maps:get(backend_timing_keys, Stats)]),
    io:format("   Last Update: ~w~n", [maps:get(last_update_time, Stats)]),
    
    % Show dead players details
    case maps:get(dead_players_count, Stats) > 0 of
        true ->
            io:format("   💀 Dead Players Details:~n"),
            maps:fold(fun(PlayerID, {DeathTime, _LastState, LocalGNAtom}, _Acc) ->
                IsLocal = lists:member(PlayerID, State#state.local_player_ids),
                LocalStr = if IsLocal -> " (LOCAL)"; true -> "" end,
                io:format("      Player ~w died at ~w on ~w~s~n", [PlayerID, DeathTime, LocalGNAtom, LocalStr])
            end, ok, State#state.dead_players);
        false -> ok
    end,
    
    % Show active explosions if any
    case maps:get(active_explosions_count, Stats) > 0 of
        true ->
            io:format("   💥 Active Explosions:~n"),
            maps:fold(fun(Coord, ExpiryTime, _Acc) ->
                TimeLeft = ExpiryTime - erlang:system_time(millisecond),
                io:format("      ~w expires in ~wms~n", [Coord, TimeLeft])
            end, ok, State#state.active_explosions);
        false -> ok
    end,
    
    % Show backend timing if available
    case maps:size(State#state.backend_timing) > 0 of
        true ->
            io:format("   ⏱️ Backend Timing Constants:~n"),
            maps:fold(fun(Key, Value, _Acc) ->
                io:format("      ~w: ~w ms~n", [Key, Value])
            end, ok, State#state.backend_timing);
        false -> ok
    end.

%% Check if a player is local to this GN
is_local_player(PlayerID, State) ->
    lists:member(PlayerID, State#state.local_player_ids).

%% Get local dead players only
get_local_dead_players(State) ->
    maps:filter(fun(PlayerID, _DeathInfo) ->
        is_local_player(PlayerID, State)
    end, State#state.dead_players).

%% Check if there are any active explosions in this GN's area
has_local_explosions(State) ->
    maps:size(State#state.active_explosions) > 0.

%% Get explosion statistics for this GN
get_explosion_stats(State) ->
    CurrentTime = erlang:system_time(millisecond),
    #{
        total_explosions => maps:size(State#state.active_explosions),
        expiring_soon => maps:size(maps:filter(fun(_Coord, ExpiryTime) ->
            (ExpiryTime - CurrentTime) < 200  % Less than 200ms left
        end, State#state.active_explosions))
    }.

