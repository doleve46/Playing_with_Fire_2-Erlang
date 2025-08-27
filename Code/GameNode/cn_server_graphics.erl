-module(cn_server_graphics).
-behaviour(gen_server).
%%% ------------------------------------------------------------------------------------------------------
%%% DOLEV - CHANGES MADE:
%% The CN server graphics does NOT spawn the GN graphics servers anymore.
%% They spawn from within their respective node, and have a locally-registered name "cn_server_graphics".
%% This Process tries to monitor al 4 of them - when he is successful he sends a 'ready' 
%% message to cn_server, and proceed to work as before.
%% Added explosion visualization support with direct function call interface.
%% TODO: Need to verify that a python port is created through cn_graphics and gn_graphics, and is responsive and works as intended.
%%% ------------------------------------------------------------------------------------------------------
%% API
-export([start_link/1, get_current_map/0, show_explosion/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("mnesia_records.hrl").
-include("../common_parameters.hrl").

-define(MAP_SIZE, 16).
-define(DEATH_DISPLAY_TIME, 10000). % Show dead players for 10 seconds
-define(EXPLOSION_DISPLAY_TIME, 1000). % Show explosions for 1 second

-record(state, {
    gn_graphics_servers = [],     % List of {Node, Pid} for GN graphics servers
    python_port,                  % Port to Python visualizer
    current_map_state,            % Current unified map state
    gn_nodes,                     % List of GN nodes
    subscribed_tables = [],       % List of tables subscribed to
    update_counter = 0,           % Counter for updates (debugging)
    movement_states = #{},        % Track active player movements with real timing
    bomb_movements = #{},         % Track active bomb movements
    dead_players = #{},           % Track recently deceased players: PlayerID => {DeathTime, LastKnownState, LocalGN}
    last_known_players = #{},     % Track last known player states for death detection
    timer_subscribers = #{},      % Track timer update subscriptions
    active_explosions = #{}       % Track active explosions: Coord => ExpiryTime
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

%% Direct function to show explosions - called directly from cn_server
-spec show_explosion(list()) -> ok.
show_explosion(Coordinates) ->
    
    io:format("💥 Direct explosion call: ~w coordinates will display for ~wms~n", 
              [length(Coordinates), ?EXPLOSION_DISPLAY_TIME]),
    
    %% Add explosions directly to the server state
    gen_server:cast(?MODULE, {add_explosions_direct, Coordinates}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the graphics server
init([GNNodes]) ->
    io:format("🎨 Enhanced CN Graphics Server starting...~n"),
   
    % Create initial state
    State = #state{gn_nodes = GNNodes},
   
    % Set up mnesia subscriptions
    erlang:send(self(), setup_subscriptions),
   
    % Monitoring of gn graphics servers
    erlang:send_after(?TICK_DELAY, self(), monitor_gn_graphics_servers),
   
    % Create Python port
    erlang:send(self(), create_python_port),
   
    % Start periodic updates (faster for better timer sync)
    erlang:send_after(2*?TICK_DELAY, self(), periodic_update),
   
    % Clean up dead players and explosions periodically
    erlang:send_after(5000, self(), cleanup_expired_elements),
    
    %% trap exits
    process_flag(trap_exit, true),
    
    io:format("✅ Enhanced CN Graphics Server initialized~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast(force_update, State) ->
    io:format("🔄 Updating enhanced map state...~n"),
    NewMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState};

%% Handle direct explosion additions
handle_cast({add_explosions_direct, Coordinates}, State) ->
    %% Add all explosion coordinates with expiration time
    Timestamp = erlang:system_time(millisecond),
    ExpiryTime = Timestamp + ?EXPLOSION_DISPLAY_TIME,
    NewExplosions = lists:foldl(fun(Coord, AccMap) ->
        maps:put(Coord, ExpiryTime, AccMap)
    end, State#state.active_explosions, Coordinates),
    
    %% Force immediate map update to show explosions
    NewState = State#state{active_explosions = NewExplosions},
    UpdatedMapState = create_enhanced_map_state(NewState),
    FinalState = NewState#state{current_map_state = UpdatedMapState},
    send_map_to_all_targets(FinalState),
    
    {noreply, FinalState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
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

handle_info(create_python_port, State) ->
    io:format("🐍 Creating enhanced Python port...~n"),
    Port = create_python_visualizer_port(),
    % Create initial enhanced map state
    InitialMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{
        python_port = Port,
        current_map_state = InitialMapState
    },
    % Send initial state
    send_map_to_all_targets(UpdatedState),
    io:format("✅ Enhanced Python port created and initial map sent~n"),
    %% Send ready message to cn_server
    case global:whereis_name(cn_server) of
        Pid when is_pid(Pid) ->
            Pid ! {graphics_ready, self()},
            io:format("✅ Graphics ready message sent to CN server~n");
        undefined ->
            io:format("⚠️ CN server not found when sending graphics_ready message~n")
    end,
    {noreply, UpdatedState};

handle_info(periodic_update, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    % Clean up expired explosions
    NewExplosions = maps:filter(fun(_Coord, ExpiryTime) ->
        CurrentTime < ExpiryTime
    end, State#state.active_explosions),
    
    CleanedExplosions = maps:size(State#state.active_explosions) - maps:size(NewExplosions),
    if CleanedExplosions > 0 ->
        io:format("💨 Cleaned up ~w expired explosions~n", [CleanedExplosions]);
    true -> ok
    end,
    
    % Enhanced periodic update every TICK_DELAY milliseconds for better timer sync
    NewMapState = create_enhanced_map_state(State#state{active_explosions = NewExplosions}),
    UpdatedState = State#state{
        current_map_state = NewMapState,
        update_counter = State#state.update_counter + 1,
        active_explosions = NewExplosions
    },
   
    % Send more frequently for real-time timer updates
    ShouldSend = (NewMapState =/= State#state.current_map_state) orelse
                 (State#state.update_counter rem 2 == 0), % Every 2nd update = every 100ms
   
    if ShouldSend ->
        send_map_to_all_targets(UpdatedState),
        % Only log every 20th update to reduce spam
        case State#state.update_counter rem 20 == 0 of
            true ->
                io:format("🔄 Enhanced periodic update #~w sent~n", [UpdatedState#state.update_counter]);
            false -> ok
        end;
    true -> ok
    end,
   
    % Schedule next update using actual backend timing
    erlang:send_after(?TICK_DELAY, self(), periodic_update),
    {noreply, UpdatedState};

handle_info(cleanup_expired_elements, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    % Remove dead players that have been shown long enough
    NewDeadPlayers = maps:filter(fun(_PlayerID, {DeathTime, _LastState, _LocalGN}) ->
        CurrentTime - DeathTime < ?DEATH_DISPLAY_TIME
    end, State#state.dead_players),
    
    % Remove expired explosions
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
    
    % Schedule next cleanup
    erlang:send_after(5000, self(), cleanup_expired_elements),
    {noreply, State#state{dead_players = NewDeadPlayers, active_explosions = NewExplosions}};

% Enhanced mnesia table event handling with real-time timer tracking
handle_info({mnesia_table_event, {write, Table, Record, _ActivityId}}, State) ->
    NewState = case Record of
        #mnesia_players{} ->
            % Enhanced player state tracking with all timers
            PlayerID = Record#mnesia_players.player_number,
            NewLastKnown = maps:put(PlayerID, Record, State#state.last_known_players),
            
            % Check for movement changes with real timing
            case detect_enhanced_player_movement_change(Record, State#state.current_map_state) of
                {movement_started, PlayerData} ->
                    send_movement_confirmation_to_python(State, player, PlayerData),
                    State#state{last_known_players = NewLastKnown};
                {timer_update, TimerData} ->
                    send_timer_update_to_python(State, player, TimerData),
                    State#state{last_known_players = NewLastKnown};
                no_movement_change ->
                    State#state{last_known_players = NewLastKnown}
            end;
        #mnesia_bombs{} ->
            % Enhanced bomb state tracking with FSM information
            case detect_enhanced_bomb_movement_change(Record, State#state.current_map_state) of
                {movement_started, BombData} ->
                    send_movement_confirmation_to_python(State, bomb, BombData),
                    State;
                {fsm_state_change, FSMData} ->
                    send_fsm_update_to_python(State, bomb, FSMData),
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
            % Enhanced player death detection
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
    % Other mnesia events
    handle_mnesia_update(State);

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from enhanced Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Enhanced Python port closed, attempting to restart...~n"),
    NewPort = create_python_visualizer_port(),
    {noreply, State#state{python_port = NewPort}};

handle_info({'DOWN', MonitorRef, process, RemotePid, noconnection}, State) ->
    %% TODO: deal with GN graphics servers disconnecting.
    {noreply, State};

handle_info(Info, State) ->
    io:format("ℹ️ Unexpected message: ~p~n", [Info]),
    {noreply, State}.

%% Cleanup on termination
terminate(Reason, State) ->
    io:format("🛑 Enhanced CN Graphics Server terminating: ~p~n", [Reason]),
   
    % Close Python port
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
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
%%% Enhanced Death Detection Functions
%%%===================================================================

%% Extract player ID from mnesia delete key
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

%% Enhanced player death handling with more details
handle_enhanced_player_death(PlayerID, Table, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    % Get last known state with all timer information
    LastKnownState = maps:get(PlayerID, State#state.last_known_players, undefined),
    
    % Determine which GN this player belonged to based on table name
    LocalGN = case Table of
        gn1_players -> gn1;
        gn2_players -> gn2;
        gn3_players -> gn3;
        gn4_players -> gn4;
        _ -> unknown
    end,
    
    % Create enhanced death record with more information
    DeathRecord = {CurrentTime, LastKnownState, LocalGN},
    NewDeadPlayers = maps:put(PlayerID, DeathRecord, State#state.dead_players),
    
    % Remove from last known players since they're now dead
    NewLastKnown = maps:remove(PlayerID, State#state.last_known_players),
    
    % Enhanced death logging
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
    
    io:format("🕐 Death recorded at ~w~n", [CurrentTime]),
    
    State#state{
        dead_players = NewDeadPlayers,
        last_known_players = NewLastKnown
    }.

%%%===================================================================
%%% Enhanced Movement and Timer Detection
%%%===================================================================

%% Enhanced player movement detection with real backend timing
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
   
    % Check if movement field changed from false to true with timer
    case Movement of
        true when Direction =/= none, MovementTimer > 0 ->
            % Movement just started - calculate real duration using backend constants
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
            % Check for timer updates without movement
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

%% Enhanced bomb movement detection
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
   
    % Check if movement field changed from false to true
    case Movement of
        true when Direction =/= none ->
            % Bomb movement just started (kicked!)
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
            % Check for FSM state changes
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

%%%===================================================================
%%% Enhanced Map Creation with Full Backend State and Explosions
%%%===================================================================

%% Create enhanced unified map state with all backend information including explosions
create_enhanced_map_state(State) ->
    try
        % Initialize empty map
        EmptyMap = create_empty_map(),
       
        % Add tiles from all GN tables
        MapWithTiles = add_tiles_to_map(EmptyMap),
       
        % Add powerups
        MapWithPowerups = add_powerups_to_map(MapWithTiles),
       
        % Add bombs with enhanced FSM information
        MapWithBombs = add_enhanced_bombs_to_map(MapWithPowerups),
       
        % Add players with full timer information
        MapWithPlayers = add_enhanced_players_to_map(MapWithBombs),
        
        % Add explosions to map
        MapWithExplosions = add_explosions_to_map(MapWithPlayers, State#state.active_explosions),
       
        % Create enhanced map state with all backend data
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

%% Add explosions to the map
add_explosions_to_map(Map, ActiveExplosions) ->
    maps:fold(fun([X, Y], _ExpiryTime, AccMap) ->
        update_map_with_explosion(AccMap, X, Y)
    end, Map, ActiveExplosions).

%% Update map cell with explosion
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

%% Add players with enhanced timer and state information
add_enhanced_players_to_map(Map) ->
    PlayerTables = [gn1_players, gn2_players, gn3_players, gn4_players],
    lists:foldl(fun(Table, AccMap) ->
        add_enhanced_players_from_table(Table, AccMap)
    end, Map, PlayerTables).

%% Add enhanced players from a specific table
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

%% Update map with enhanced player information including all timers
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
        
        % Enhanced player info with all backend state
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

%% Add bombs with enhanced FSM state information
add_enhanced_bombs_to_map(Map) ->
    BombTables = [gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs],
    lists:foldl(fun(Table, AccMap) ->
        add_enhanced_bombs_from_table(Table, AccMap)
    end, Map, BombTables).

%% Add enhanced bombs from a specific table
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

%% Update map with enhanced bomb information including FSM state
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
        
        % Enhanced bomb info with FSM state and movement information
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

%%%===================================================================
%%% Enhanced Communication Functions
%%%===================================================================

%% Send enhanced movement confirmation to Python with real timing
send_movement_confirmation_to_python(State, EntityType, EntityData) ->
    if State#state.python_port =/= undefined ->
        try
            ConfirmationMsg = [movement_confirmation, #{
                entity_type => EntityType,
                entity_data => EntityData
            }],
            MsgBinary = term_to_binary(ConfirmationMsg),
            port_command(State#state.python_port, MsgBinary),
            case EntityType of
                player ->
                    PlayerID = maps:get(player_id, EntityData),
                    Duration = maps:get(total_duration, EntityData),
                    io:format("🏃 Enhanced player movement confirmation sent for player ~w (duration: ~wms)~n",
                             [PlayerID, Duration]);
                bomb ->
                    Pos = maps:get(from_pos, EntityData),
                    io:format("💣 Enhanced bomb movement confirmation sent for bomb at ~w~n", [Pos])
            end
        catch
            _:Error ->
                io:format("❌ Error sending enhanced movement confirmation: ~p~n", [Error])
        end;
    true ->
        ok
    end.

%% Send timer updates to Python
send_timer_update_to_python(State, EntityType, TimerData) ->
    if State#state.python_port =/= undefined ->
        try
            TimerMsg = [timer_update, #{
                entity_type => EntityType,
                timer_data => TimerData
            }],
            MsgBinary = term_to_binary(TimerMsg),
            port_command(State#state.python_port, MsgBinary)
        catch
            _:Error ->
                io:format("❌ Error sending timer update: ~p~n", [Error])
        end;
    true ->
        ok
    end.

%% Send FSM state updates to Python
send_fsm_update_to_python(State, EntityType, FSMData) ->
    if State#state.python_port =/= undefined ->
        try
            FSMMsg = [fsm_update, #{
                entity_type => EntityType,
                fsm_data => FSMData
            }],
            MsgBinary = term_to_binary(FSMMsg),
            port_command(State#state.python_port, MsgBinary)
        catch
            _:Error ->
                io:format("❌ Error sending FSM update: ~p~n", [Error])
        end;
    true ->
        ok
    end.

%%%===================================================================
%%% Enhanced Cell Update Functions
%%%===================================================================

%% Update cell with enhanced player information
update_cell_enhanced_player({Tile, Powerup, Bomb, _, Explosion, Special}, EnhancedPlayerInfo) ->
    {Tile, Powerup, Bomb, EnhancedPlayerInfo, Explosion, Special}.

%% Update cell with enhanced bomb information
update_cell_enhanced_bomb({Tile, Powerup, _, Player, Explosion, Special}, EnhancedBombInfo) ->
    {Tile, Powerup, EnhancedBombInfo, Player, Explosion, Special}.

%% Update cell with explosion information
update_cell_explosion({Tile, Powerup, Bomb, Player, _, Special}, ExplosionInfo) ->
    {Tile, Powerup, Bomb, Player, ExplosionInfo, Special}.

%%%===================================================================
%%% Existing Helper Functions (Enhanced)
%%%===================================================================

%% Calculate destination position
calculate_destination([X, Y], Direction) ->
    case Direction of
        up -> [X, Y-1];
        down -> [X, Y+1];
        left -> [X-1, Y];
        right -> [X+1, Y]
    end.

%% Handle mnesia updates by recreating enhanced map state
handle_mnesia_update(State) ->
    NewMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState}.

%% Get all table names that we need to monitor
get_all_tables() ->
    [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles,
     gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs,
     gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups,
     gn1_players, gn2_players, gn3_players, gn4_players].

%% Set up mnesia subscriptions for all relevant tables
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

%% Monitor enhanced graphics servers on all GN nodes
-spec monitor_gn_graphics_servers(GNNodes::list()) -> [{reference(), atom()}].
monitor_gn_graphics_servers(GNNodes)->
    RefsList = attempt_gn_graphics_monitoring(GNNodes),
    gn_monitoring_receive_loop(RefsList, []).

gn_monitoring_receive_loop(RefsList, ServersNotFound) ->
    receive
        {'DOWN', Ref, process, _Pid, noproc} ->
            case lists:keyfind(Ref, 1, RefsList) of
                false -> % unknown message - unsure what to do with it
                    io:format("❌ *Unknown monitoring failure message received:~n~w~n",[{'DOWN', Ref, process, _Pid, noproc}]),
                    gn_monitoring_receive_loop(RefsList, ServersNotFound);
                {_, NodeName} -> % a monitoring to NodeName has failed - add to failed servers
                    io:format("❌ *A monitoring request has failed on node ~w. Accumulating before retrying..~n",[NodeName]),
                    gn_monitoring_receive_loop(lists:keydelete(Ref, 1, RefsList), [NodeName | ServersNotFound])
            end;
        AnythingElse -> % re-send to self in 5 seconds
            erlang:send_after(5000, self(), AnythingElse),
            gn_monitoring_receive_loop(RefsList, ServersNotFound)
     after 1500 -> % timeout is 1.5sec
        case length(ServersNotFound) =/= 0 of
            true ->
                io:format("❌ Failed to monitor ~p servers. The following were not monitored:~w~n", [length(ServersNotFound), ServersNotFound]),
                NewRefs = monitor_gn_graphics_servers(ServersNotFound),
                NewRefs ++ RefsList;
            false -> % every server was successfully monitored
                io:format("✅ All GN graphics server were monitored successfully!~n"),
                RefsList
         end
    end.

attempt_gn_graphics_monitoring(NodeList) ->
    RefsList = lists:map(fun(Node) ->
        Ref = erlang:monitor(process, {gn_graphics_server, Node}),
        io:format("Sent request to monitor process ~w~n", [Node]),
        {Ref, Node} end, NodeList),
    timer:sleep(1000), % wait for 1 second before looking at the messages we received
    RefsList.

%% Create enhanced Python visualizer port
create_python_visualizer_port() ->
    try
        Port = open_port({spawn, "python3 Graphics/map_live.py"},
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Enhanced Python port created: ~p~n", [Port]),
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create enhanced Python port: ~p~n", [Error]),
            undefined
    end.

%% Create empty 16x16 map with free tiles (enhanced format)
create_empty_map() ->
    [[{free, none, none, none, none, none} || _ <- lists:seq(1, ?MAP_SIZE)]
     || _ <- lists:seq(1, ?MAP_SIZE)].

%% Add tiles from all GN tables to the map
add_tiles_to_map(Map) ->
    TileTables = [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles],
    lists:foldl(fun(Table, AccMap) ->
        add_tiles_from_table(Table, AccMap)
    end, Map, TileTables).

%% Add tiles from a specific table
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

%% Update map with tile information
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

%% Add powerups from all tables
add_powerups_to_map(Map) ->
    PowerupTables = [gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups],
    lists:foldl(fun(Table, AccMap) ->
        add_powerups_from_table(Table, AccMap)
    end, Map, PowerupTables).

%% Add powerups from a specific table
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

%% Update map with powerup information
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

%% Update cell with tile information
update_cell_tile({_, Powerup, Bomb, Player, Explosion, Special}, TileType, Contains) ->
    % If tile contains a powerup, update powerup field
    ActualPowerup = if Contains =/= none -> Contains; true -> Powerup end,
    {TileType, ActualPowerup, Bomb, Player, Explosion, Special}.

%% Update cell with powerup information
update_cell_powerup({Tile, _, Bomb, Player, Explosion, Special}, PowerupType) ->
    {Tile, PowerupType, Bomb, Player, Explosion, Special}.

%% Replace element in list at specific position (1-indexed)
replace_list_element(List, Pos, NewElement) ->
    {Before, [_|After]} = lists:split(Pos - 1, List),
    Before ++ [NewElement] ++ After.

%% Send enhanced map to all targets (Python and GN servers)
send_map_to_all_targets(State) ->
    % Send to Python visualizer
    send_enhanced_map_to_python(State),
   
    % Send to GN graphics servers
    send_enhanced_map_to_gn_servers(State).

%% Send enhanced map to Python visualizer
send_enhanced_map_to_python(State) ->
    if State#state.python_port =/= undefined andalso
       State#state.current_map_state =/= undefined ->
        try
            % Convert enhanced map state to binary Erlang term
            MapBinary = term_to_binary(State#state.current_map_state),
            port_command(State#state.python_port, MapBinary),
            if State#state.update_counter rem 40 == 0 ->  % Log every 2 seconds
                ExplosionCount = maps:size(State#state.active_explosions),
                io:format("🗺️ Enhanced map (with timers, FSM state & ~w explosions) sent to Python visualizer~n", [ExplosionCount]);
            true -> ok
            end
        catch
            _:Error ->
                io:format("❌ Error sending enhanced data to Python: ~p~n", [Error])
        end;
    true ->
        io:format("⚠️ Enhanced Python port or map state not ready~n")
    end.

%% Send enhanced map to all GN graphics servers
send_enhanced_map_to_gn_servers(State) ->
    GNCount = length(State#state.gn_graphics_servers),
    io:format("🔄 CN attempting to send map update to ~w GN graphics servers~n", [GNCount]),
    lists:foreach(fun({Node, Pid}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                try
                    % Send enhanced map state with full backend information
                    gen_server:cast(Pid, {map_update, State#state.current_map_state}),
                    io:format("📡 Map update sent to GN server on ~w~n", [Node]),
                    case State#state.update_counter rem 40 == 0 of  % Log every 2 seconds
                        true ->
                            ExplosionCount = maps:size(State#state.active_explosions),
                            io:format("📡 Enhanced map (with timers, FSM state & ~w explosions) sent to GN server on ~w~n", [ExplosionCount, Node]);
                        false -> ok
                    end
                catch
                    _:Error ->
                        io:format("❌ Error sending enhanced data to GN server on ~w: ~p~n", [Node, Error])
                end;
            false ->
                io:format("⚠️ GN server on ~w not alive or not found (Pid: ~p)~n", [Node, Pid])
        end
    end, State#state.gn_graphics_servers).
