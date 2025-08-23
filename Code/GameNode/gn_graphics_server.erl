-module(gn_graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    cn_node,                    % Central node
    python_port,                % Port to local Python visualizer
    update_counter = 0,         % Update counter
    current_map_state,          % Current map state received from CN
    dead_players = #{},         % Track dead players received from CN
    last_update_time = 0        % Track when we last received an update
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the GN graphics server
-spec start_link(node()) -> {ok, pid()} | ignore | {error, term()}.
start_link(CNNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CNNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the GN graphics server
init([CNNode]) ->
    io:format("🎮 Enhanced GN Graphics Server starting on ~w~n", [node()]),
    
    State = #state{cn_node = CNNode},
    
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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, MapState}, State) ->
    % Received enhanced map update from CN graphics server
    CurrentTime = erlang:system_time(millisecond),
    
    % Handle both old format (direct grid) and new enhanced format
    {ActualMapState, DeadPlayers} = case MapState of
        #{map := GridData, dead_players := DeadPlayersMap} ->
            % New enhanced format with death information
            io:format("🗺️ GN received enhanced map update from CN (#~w) with death info~n", 
                      [State#state.update_counter + 1]),
            
            % Check for newly dead players
            NewDeaths = maps:filter(fun(PlayerID, _DeathInfo) ->
                not maps:is_key(PlayerID, State#state.dead_players)
            end, DeadPlayersMap),
            
            if map_size(NewDeaths) > 0 ->
                NewDeathList = maps:to_list(NewDeaths),
                io:format("💀 New deaths detected by GN: ~p~n", [NewDeathList]),
                
                % Check if any deaths are for local players
                LocalGN = get_local_gn_name(),
                lists:foreach(fun({PlayerID, {_DeathTime, _LastState, LocalGNAtom}}) ->
                    if LocalGNAtom =:= LocalGN ->
                        io:format("🩸 LOCAL PLAYER ~w DIED on this GN!~n", [PlayerID]);
                    true ->
                        io:format("💀 Remote player ~w died on ~w~n", [PlayerID, LocalGNAtom])
                    end
                end, NewDeathList);
            true -> ok
            end,
            
            {GridData, DeadPlayersMap};
        _ ->
            % Old format - just the grid
            io:format("🗺️ GN received map update from CN (#~w)~n", 
                      [State#state.update_counter + 1]),
            {MapState, State#state.dead_players}
    end,
    
    % Send enhanced data to local Python visualizer
    EnhancedMapData = #{
        map => ActualMapState,
        dead_players => DeadPlayers,
        update_time => CurrentTime,
        local_gn => get_local_gn_name()
    },
    
    send_map_to_python(State#state.python_port, EnhancedMapData),
    
    NewState = State#state{
        current_map_state = EnhancedMapData,
        dead_players = DeadPlayers,
        update_counter = State#state.update_counter + 1,
        last_update_time = CurrentTime
    },
    {noreply, NewState};

handle_cast(force_update, State) ->
    % Force update - just resend current state if we have it
    case State#state.current_map_state of
        undefined ->
            io:format("⚠️ No enhanced map state available for force update~n");
        MapState ->
            io:format("🔄 Force updating Python with current enhanced map state~n"),
            send_map_to_python(State#state.python_port, MapState)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(create_python_port, State) ->
    % Create Python port for enhanced visualizer
    io:format("🐍 Creating enhanced Python visualizer port...~n"),
    Port = create_python_port(),
    
    UpdatedState = State#state{python_port = Port},
    
    % If we already have enhanced map state from CN, send it
    case State#state.current_map_state of
        undefined ->
            io:format("✅ Enhanced Python port created, waiting for first CN update~n");
        MapState ->
            send_map_to_python(Port, MapState),
            io:format("✅ Enhanced Python port created and current map state sent~n")
    end,
    
    {noreply, UpdatedState};

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from enhanced Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Enhanced Python port closed, restarting...~n"),
    NewPort = create_python_port(),
    
    % Resend current enhanced map state if available
    case State#state.current_map_state of
        undefined -> ok;
        MapState -> send_map_to_python(NewPort, MapState)
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
%%% Enhanced Python Port Communication
%%%===================================================================

%% Get the local GN name for this node
get_local_gn_name() ->
    % Determine which GN this server is running on
    % This could be based on node name, environment variables, etc.
    NodeName = atom_to_list(node()),
    case NodeName of
        "gn1" ++ _ -> gn1;
        "gn2" ++ _ -> gn2;
        "gn3" ++ _ -> gn3;
        "gn4" ++ _ -> gn4;
        _ ->
            % Fallback: check environment or use default
            case os:getenv("GN_ID") of
                false -> gn1;  % Default fallback
                GNStr -> list_to_atom(GNStr)
            end
    end.

%% Create Python port for enhanced visualizer
create_python_port() ->
    try
        % Use the enhanced Python visualizer for GN nodes with death detection
        Port = open_port({spawn, "python3 gn_map_live_enhanced.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Enhanced Python visualizer port created~n"),
        
        % Set environment variable to identify which GN this is
        LocalGN = get_local_gn_name(),
        os:putenv("GN_ID", atom_to_list(LocalGN)),
        
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create enhanced Python port: ~p~n", [Error]),
            undefined
    end.

%% Send enhanced map data to Python visualizer
send_map_to_python(undefined, _MapState) ->
    io:format("⚠️ No enhanced Python port available~n");

send_map_to_python(Port, MapState) ->
    try
        % Send enhanced map state as binary term
        MapBinary = term_to_binary(MapState),
        port_command(Port, MapBinary),
        
        % Log details about what we're sending
        DeadCount = case MapState of
            #{dead_players := DeadPlayers} -> maps:size(DeadPlayers);
            _ -> 0
        end,
        
        io:format("📤 Enhanced map forwarded to Python visualizer (dead players: ~w)~n", [DeadCount])
    catch
        _:Error ->
            io:format("❌ Error sending enhanced data to Python: ~p~n", [Error])
    end.

%%%===================================================================
%%% Additional Helper Functions
%%%===================================================================

%% Get statistics about current state
get_state_statistics(State) ->
    #{
        update_counter => State#state.update_counter,
        dead_players_count => maps:size(State#state.dead_players),
        last_update_time => State#state.last_update_time,
        has_map_state => State#state.current_map_state =/= undefined,
        has_python_port => State#state.python_port =/= undefined,
        local_gn => get_local_gn_name()
    }.

%% Debug function to print current state
debug_state(State) ->
    Stats = get_state_statistics(State),
    io:format("🔍 GN Graphics Server Debug State:~n"),
    io:format("   Local GN: ~w~n", [maps:get(local_gn, Stats)]),
    io:format("   Update Counter: ~w~n", [maps:get(update_counter, Stats)]),
    io:format("   Dead Players: ~w~n", [maps:get(dead_players_count, Stats)]),
    io:format("   Has Map State: ~w~n", [maps:get(has_map_state, Stats)]),
    io:format("   Has Python Port: ~w~n", [maps:get(has_python_port, Stats)]),
    io:format("   Last Update: ~w~n", [maps:get(last_update_time, Stats)]),
    
    if maps:get(dead_players_count, Stats) > 0 ->
        io:format("   💀 Dead Players Details:~n"),
        maps:fold(fun(PlayerID, {DeathTime, _LastState, LocalGN}, _Acc) ->
            io:format("      Player ~w died at ~w on ~w~n", [PlayerID, DeathTime, LocalGN])
        end, ok, State#state.dead_players);
    true -> ok
    end.