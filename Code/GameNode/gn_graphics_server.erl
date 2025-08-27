-module(gn_graphics_server).
-behaviour(gen_server).

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

start_link(CNNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CNNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CNNode]) ->
    LocalGN = determine_local_gn(),
    LocalPlayerIDs = get_local_player_ids(LocalGN),
    
    io:format("GN Graphics Server starting on ~w (Local GN: ~w, Players: ~w)~n", 
              [node(), LocalGN, LocalPlayerIDs]),
    
    State = #state{
        cn_node = CNNode,
        local_gn_name = LocalGN,
        local_player_ids = LocalPlayerIDs
    },
    
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    
    % Create Python port after a short delay
    erlang:send_after(50, self(), create_python_port),
    
    io:format("GN Graphics Server initialized (waiting for CN updates)~n"),
    {ok, State}.

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

handle_cast({map_update, EnhancedMapState}, State) ->
    % Received enhanced map update from CN graphics server
    CurrentTime = erlang:system_time(millisecond),
    
    % Handle enhanced format with timing, death, and explosion information
    {ActualMapState, DeadPlayers, BackendTiming, ActiveExplosions} = case EnhancedMapState of
        #{map := GridData, dead_players := DeadPlayersMap, backend_timing := Timing, active_explosions := Explosions} ->
            % Full enhanced format with explosions
            check_for_newly_dead_players(DeadPlayersMap, State#state.dead_players, State#state.local_player_ids),
            {GridData, DeadPlayersMap, Timing, Explosions};
            
        #{map := GridData, dead_players := DeadPlayersMap, backend_timing := Timing} ->
            % Enhanced format without explosions
            check_for_newly_dead_players(DeadPlayersMap, State#state.dead_players, State#state.local_player_ids),
            {GridData, DeadPlayersMap, Timing, State#state.active_explosions};
            
        #{map := GridData, dead_players := DeadPlayersMap} ->
            % Format without backend timing or explosions
            check_for_newly_dead_players(DeadPlayersMap, State#state.dead_players, State#state.local_player_ids),
            {GridData, DeadPlayersMap, State#state.backend_timing, State#state.active_explosions};
            
        _ ->
            % Old format - just the grid
            {EnhancedMapState, State#state.dead_players, State#state.backend_timing, State#state.active_explosions}
    end,
    
    % Create enhanced data for local Python visualizer with GN-specific information
    LocalEnhancedMapData = #{
        map => ActualMapState,
        dead_players => DeadPlayers,
        update_time => CurrentTime,
        local_gn => State#state.local_gn_name,
        local_players => State#state.local_player_ids,
        backend_timing => BackendTiming,
        active_explosions => ActiveExplosions
    },
    
    send_enhanced_map_to_python(State#state.python_port, LocalEnhancedMapData),
    
    NewState = State#state{
        current_map_state = LocalEnhancedMapData,
        dead_players = DeadPlayers,
        backend_timing = BackendTiming,
        active_explosions = ActiveExplosions,
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
        undefined -> ok;
        MapState -> send_enhanced_map_to_python(State#state.python_port, MapState)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(create_python_port, State) ->
    % Create Python port for enhanced visualizer
    Port = create_enhanced_python_port(State#state.local_gn_name),
    
    UpdatedState = State#state{python_port = Port},
    
    % If we already have enhanced map state from CN, send it
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> send_enhanced_map_to_python(Port, MapState)
    end,
    
    {noreply, UpdatedState};

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("Python port closed, restarting...~n"),
    NewPort = create_enhanced_python_port(State#state.local_gn_name),
    
    % Resend current enhanced map state if available
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> send_enhanced_map_to_python(NewPort, MapState)
    end,
    
    {noreply, State#state{python_port = NewPort}};

handle_info({nodedown, Node}, State) when Node == State#state.cn_node ->
    io:format("CN node ~w went down~n", [Node]),
    {noreply, State};

handle_info({nodeup, Node}, State) when Node == State#state.cn_node ->
    io:format("CN node ~w came back up~n", [Node]),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Enhanced GN Identification Functions
%%%===================================================================

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
                    io:format("Could not determine GN ID from node name ~p, defaulting to gn1~n", [NodeName]),
                    gn1
            end;
        GNStr ->
            list_to_atom(GNStr)
    end.

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

create_enhanced_python_port(LocalGN) ->
    try
        % Set environment variable to identify which GN this is
        os:putenv("GN_ID", atom_to_list(LocalGN)),
        
        % FIXED: Use the correct Python file name that exists
        Port = open_port({spawn, "python3 gn_map_live.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("Python visualizer port created for ~w~n", [LocalGN]),
        
        Port
    catch
        _:Error ->
            io:format("Failed to create Python port: ~p~n", [Error]),
            undefined
    end.

send_enhanced_map_to_python(undefined, _MapState) ->
    ok;

send_enhanced_map_to_python(Port, MapState) ->
    try
        % Send enhanced map state as binary term
        MapBinary = term_to_binary(MapState),
        port_command(Port, MapBinary)
    catch
        _:Error ->
            ok
    end.

send_movement_confirmation_to_python(undefined, _ConfirmationData) ->
    ok;

send_movement_confirmation_to_python(Port, ConfirmationData) ->
    try
        ConfirmationMsg = [movement_confirmation, ConfirmationData],
        MsgBinary = term_to_binary(ConfirmationMsg),
        port_command(Port, MsgBinary)
    catch
        _:Error ->
            ok
    end.

send_timer_update_to_python(undefined, _TimerData) ->
    ok;

send_timer_update_to_python(Port, TimerData) ->
    try
        TimerMsg = [timer_update, TimerData],
        MsgBinary = term_to_binary(TimerMsg),
        port_command(Port, MsgBinary)
    catch
        _:Error ->
            ok
    end.

send_fsm_update_to_python(undefined, _FSMData) ->
    ok;

send_fsm_update_to_python(Port, FSMData) ->
    try
        FSMMsg = [fsm_update, FSMData],
        MsgBinary = term_to_binary(FSMMsg),
        port_command(Port, MsgBinary)
    catch
        _:Error ->
            ok
    end.

%%%===================================================================
%%% Enhanced Helper Functions
%%%===================================================================

check_for_newly_dead_players(NewDeadPlayers, OldDeadPlayers, LocalPlayerIDs) ->
    % Check for newly dead players
    NewDeaths = maps:filter(fun(PlayerID, _DeathInfo) ->
        not maps:is_key(PlayerID, OldDeadPlayers)
    end, NewDeadPlayers),
    
    if map_size(NewDeaths) > 0 ->
        NewDeathList = maps:to_list(NewDeaths),
        
        % Check if any deaths are for local players
        lists:foreach(fun({PlayerID, {DeathTime, _LastState, LocalGNAtom}}) ->
            IsLocal = lists:member(PlayerID, LocalPlayerIDs),
            if IsLocal ->
                io:format("LOCAL PLAYER ~w DIED on this GN! (Death time: ~w)~n", [PlayerID, DeathTime]);
            true ->
                io:format("Remote player ~w died on ~w~n", [PlayerID, LocalGNAtom])
            end
        end, NewDeathList);
    true -> ok
    end.

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

debug_enhanced_state(State) ->
    Stats = get_enhanced_state_statistics(State),
    io:format("Enhanced GN Graphics Server Debug State:~n"),
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
    
    % Show dead players details if any
    case maps:get(dead_players_count, Stats) > 0 of
        true ->
            io:format("   Dead Players Details:~n"),
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
            io:format("   Active Explosions:~n"),
            maps:fold(fun(Coord, ExpiryTime, _Acc) ->
                TimeLeft = ExpiryTime - erlang:system_time(millisecond),
                io:format("      ~w expires in ~wms~n", [Coord, TimeLeft])
            end, ok, State#state.active_explosions);
        false -> ok
    end,
    
    % Show backend timing if available
    case maps:size(State#state.backend_timing) > 0 of
        true ->
            io:format("   Backend Timing Constants:~n"),
            maps:fold(fun(Key, Value, _Acc) ->
                io:format("      ~w: ~w ms~n", [Key, Value])
            end, ok, State#state.backend_timing);
        false -> ok
    end.

is_local_player(PlayerID, State) ->
    lists:member(PlayerID, State#state.local_player_ids).

get_local_dead_players(State) ->
    maps:filter(fun(PlayerID, _DeathInfo) ->
        is_local_player(PlayerID, State)
    end, State#state.dead_players).

has_local_explosions(State) ->
    maps:size(State#state.active_explosions) > 0.

get_explosion_stats(State) ->
    CurrentTime = erlang:system_time(millisecond),
    #{
        total_explosions => maps:size(State#state.active_explosions),
        expiring_soon => maps:size(maps:filter(fun(_Coord, ExpiryTime) ->
            (ExpiryTime - CurrentTime) < 200  % Less than 200ms left
        end, State#state.active_explosions))
    }.
