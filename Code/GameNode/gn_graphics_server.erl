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
    current_map_state           % Current map state received from CN
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
    io:format("🎮 Simplified GN Graphics Server starting on ~w~n", [node()]),
    
    State = #state{cn_node = CNNode},
    
    % Create Python port after a short delay
    erlang:send_after(50, self(), create_python_port),
    
    io:format("✅ Simplified GN Graphics Server initialized (waiting for CN updates)~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, MapState}, State) ->
    % Received map update from CN graphics server - send to Python
    io:format("🗺️ GN received map update from CN (#~w), forwarding to Python~n", 
              [State#state.update_counter + 1]),
    
    % Send to local Python visualizer
    send_map_to_python(State#state.python_port, MapState),
    
    NewState = State#state{
        current_map_state = MapState,
        update_counter = State#state.update_counter + 1
    },
    {noreply, NewState};

handle_cast(force_update, State) ->
    % Force update - just resend current state if we have it
    case State#state.current_map_state of
        undefined ->
            io:format("⚠️ No map state available for force update~n");
        MapState ->
            io:format("🔄 Force updating Python with current map state~n"),
            send_map_to_python(State#state.python_port, MapState)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(create_python_port, State) ->
    % Create Python port for visualizer
    io:format("🐍 Creating Python visualizer port...~n"),
    Port = create_python_port(),
    
    UpdatedState = State#state{python_port = Port},
    
    % If we already have a map state from CN, send it
    case State#state.current_map_state of
        undefined ->
            io:format("✅ Python port created, waiting for first CN update~n");
        MapState ->
            send_map_to_python(Port, MapState),
            io:format("✅ Python port created and current map state sent~n")
    end,
    
    {noreply, UpdatedState};

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Python port closed, restarting...~n"),
    NewPort = create_python_port(),
    
    % Resend current map state if available
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
    io:format("🛑 Simplified GN Graphics Server terminating~n"),
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Python Port Communication
%%%===================================================================

%% Create Python port for visualizer
create_python_port() ->
    try
        % Use the Python visualizer for GN nodes
        Port = open_port({spawn, "python3 gn_map_live_enhanced.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Python visualizer port created~n"),
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create Python port: ~p~n", [Error]),
            undefined
    end.

%% Send map data to Python visualizer
send_map_to_python(undefined, _MapState) ->
    io:format("⚠️ No Python port available~n");

send_map_to_python(Port, MapState) ->
    try
        % Send as binary term (same format as CN system)
        MapBinary = term_to_binary(MapState),
        port_command(Port, MapBinary),
        io:format("📤 Map forwarded to Python visualizer~n")
    catch
        _:Error ->
            io:format("❌ Error sending to Python: ~p~n", [Error])
    end.
