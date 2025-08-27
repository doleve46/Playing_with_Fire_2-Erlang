-module(cn_server_graphics).
-behaviour(gen_server).

%% API
-export([start_link/1, get_current_map/0, show_explosion/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("mnesia_records.hrl").
-include("../common_parameters.hrl").

-define(MAP_SIZE, 16).
-define(DEATH_DISPLAY_TIME, 10000).
-define(EXPLOSION_DISPLAY_TIME, 1000).

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

start_link(GNNodes) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [GNNodes], []).

get_current_map() ->
    gen_server:call(?MODULE, get_current_map).

show_explosion(Coordinates) ->
    gen_server:cast(?MODULE, {add_explosions_direct, Coordinates}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GNNodes]) ->
    io:format("CN Graphics Server starting...~n"),
   
    State = #state{gn_nodes = GNNodes},
   
    erlang:send(self(), setup_subscriptions),
    erlang:send_after(?TICK_DELAY, self(), monitor_gn_graphics_servers),
    erlang:send(self(), create_python_port),
    erlang:send_after(2*?TICK_DELAY, self(), periodic_update),
    erlang:send_after(5000, self(), cleanup_expired_elements),
    
    process_flag(trap_exit, true),
    
    io:format("CN Graphics Server initialized~n"),
    {ok, State}.

handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(force_update, State) ->
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
    
    {noreply, FinalState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup_subscriptions, State) ->
    Tables = get_all_tables(),
    SubscribedTables = setup_mnesia_subscriptions(Tables),
    {noreply, State#state{subscribed_tables = SubscribedTables}};

handle_info(monitor_gn_graphics_servers, State) ->
    GNServers = monitor_gn_graphics_servers(State#state.gn_nodes),
    io:format("Monitoring ~w GN graphics servers~n", [length(GNServers)]),
    {noreply, State#state{gn_graphics_servers = GNServers}};

handle_info(create_python_port, State) ->
    Port = create_python_visualizer_port(),
    InitialMapState = create_enhanced_map_state(State),
    UpdatedState = State#state{
        python_port = Port,
        current_map_state = InitialMapState
    },
    send_map_to_all_targets(UpdatedState),
    cn_server ! {graphics_ready, self()},
    {noreply, UpdatedState};

handle_info(periodic_update, State) ->
    CurrentTime = erlang:system_time(millisecond),
    
    NewExplosions = maps:filter(fun(_Coord, ExpiryTime) ->
        CurrentTime < ExpiryTime
    end, State#state.active_explosions),
    
    NewMapState = create_enhanced_map_state(State#state{active_explosions = NewExplosions}),
    UpdatedState = State#state{
        current_map_state = NewMapState,
        update_counter = State#state.update_counter + 1,
        active_explosions = NewExplosions
    },
   
    ShouldSend = (NewMapState =/= State#state.current_map_state) orelse
                 (State#state.update_counter rem 2 == 0),
   
    if ShouldSend ->
        send_map_to_all_targets(UpdatedState);
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
    
    erlang:send_after(5000, self(), cleanup_expired_elements),
    {noreply, State#state{dead_players = NewDeadPlayers, active_explosions = NewExplosions}};

handle_info({mnesia_table_event, {write, Table, Record, _ActivityId}}, State) ->
    NewState = case Record of
        #mnesia_players{} ->
            PlayerID = Record#mnesia_players.player_number,
            NewLastKnown = maps:put(PlayerID, Record, State#state.last_known_players),
            
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
            case extract_player_id_from_key(Key) of
                {ok, PlayerID} ->
                    handle_enhanced_player_death(PlayerID, Table, State);
                error ->
                    State
            end;
        _ ->
            State
    end,
    handle_mnesia_update(NewState);

handle_info({mnesia_table_event, _Event}, State) ->
    handle_mnesia_update(State);

handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    NewPort = create_python_visualizer_port(),
    {noreply, State#state{python_port = NewPort}};

handle_info({'DOWN', MonitorRef, process, RemotePid, noconnection}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
   
    lists:foreach(fun({_Node, Pid}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                exit(Pid, shutdown);
            false -> ok
        end
    end, State#state.gn_graphics_servers),
   
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Fixed Monitoring Functions
%%%===================================================================

%% FIXED: Simple monitoring that returns {Node, Pid} pairs
monitor_gn_graphics_servers(GNNodes) ->
    lists:filtermap(fun(Node) ->
        case rpc:call(Node, erlang, whereis, [gn_graphics_server]) of
            Pid when is_pid(Pid) ->
                erlang:monitor(process, Pid),
                {true, {Node, Pid}};
            _ -> false
        end
    end, GNNodes).

%%%===================================================================
%%% Enhanced Death Detection Functions
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
    
    io:format("Player ~w died on ~w~n", [PlayerID, LocalGN]),
    
    State#state{
        dead_players = NewDeadPlayers,
        last_known_players = NewLastKnown
    }.

%%%===================================================================
%%% Enhanced Movement and Timer Detection
%%%===================================================================

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

%%%===================================================================
%%% Enhanced Map Creation with Full Backend State and Explosions
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
            #{
                map => create_empty_map(),
                dead_players => #{},
                update_time => erlang:system_time(millisecond),
                active_explosions => #{},
                backend_timing => #{}
            }
    end.

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
        Map
    end.

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
        Map
    end.

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
        Map
    end.

%%%===================================================================
%%% Enhanced Communication Functions
%%%===================================================================

send_movement_confirmation_to_python(State, EntityType, EntityData) ->
    if State#state.python_port =/= undefined ->
        try
            ConfirmationMsg = [movement_confirmation, #{
                entity_type => EntityType,
                entity_data => EntityData
            }],
            MsgBinary = term_to_binary(ConfirmationMsg),
            port_command(State#state.python_port, MsgBinary)
        catch
            _:Error -> ok
        end;
    true ->
        ok
    end.

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
            _:Error -> ok
        end;
    true ->
        ok
    end.

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
            _:Error -> ok
        end;
    true ->
        ok
    end.

%%%===================================================================
%%% Enhanced Cell Update Functions
%%%===================================================================

update_cell_enhanced_player({Tile, Powerup, Bomb, _, Explosion, Special}, EnhancedPlayerInfo) ->
    {Tile, Powerup, Bomb, EnhancedPlayerInfo, Explosion, Special}.

update_cell_enhanced_bomb({Tile, Powerup, _, Player, Explosion, Special}, EnhancedBombInfo) ->
    {Tile, Powerup, EnhancedBombInfo, Player, Explosion, Special}.

update_cell_explosion({Tile, Powerup, Bomb, Player, _, Special}, ExplosionInfo) ->
    {Tile, Powerup, Bomb, Player, ExplosionInfo, Special}.

%%%===================================================================
%%% Existing Helper Functions
%%%===================================================================

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

get_all_tables() ->
    [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles,
     gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs,
     gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups,
     gn1_players, gn2_players, gn3_players, gn4_players].

setup_mnesia_subscriptions(Tables) ->
    lists:foldl(fun(Table, Acc) ->
        case mnesia:subscribe({table, Table, simple}) of
            {ok, _} -> [Table | Acc];
            {error, Reason} -> Acc
        end
    end, [], Tables).

create_python_visualizer_port() ->
    try
        Port = open_port({spawn, "python3 map_live_port.py"},
                        [binary, exit_status, {packet, 4}]),
        Port
    catch
        _:Error ->
            undefined
    end.

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
        Map
    end.

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
        Map
    end.

update_cell_tile({_, Powerup, Bomb, Player, Explosion, Special}, TileType, Contains) ->
    ActualPowerup = if Contains =/= none -> Contains; true -> Powerup end,
    {TileType, ActualPowerup, Bomb, Player, Explosion, Special}.

update_cell_powerup({Tile, _, Bomb, Player, Explosion, Special}, PowerupType) ->
    {Tile, PowerupType, Bomb, Player, Explosion, Special}.

replace_list_element(List, Pos, NewElement) ->
    {Before, [_|After]} = lists:split(Pos - 1, List),
    Before ++ [NewElement] ++ After.

send_map_to_all_targets(State) ->
    send_enhanced_map_to_python(State),
    send_enhanced_map_to_gn_servers(State).

send_enhanced_map_to_python(State) ->
    if State#state.python_port =/= undefined andalso
       State#state.current_map_state =/= undefined ->
        try
            MapBinary = term_to_binary(State#state.current_map_state),
            port_command(State#state.python_port, MapBinary)
        catch
            _:Error -> ok
        end;
    true ->
        ok
    end.

%% FIXED: Use the correct data structure that monitoring function provides
send_enhanced_map_to_gn_servers(State) ->
    lists:foreach(fun({Node, Pid}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                try
                    gen_server:cast(Pid, {map_update, State#state.current_map_state})
                catch
                    _:Error -> ok
                end;
            false -> ok
        end
    end, State#state.gn_graphics_servers).
