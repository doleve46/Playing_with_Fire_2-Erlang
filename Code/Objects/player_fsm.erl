%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Player FSM Process for Playing with Fire 2
%%% Flow: I/O -> Player -> GN -> CN -> GN -> Player -> I/O
%%% 
%%% **Update (19/8):**
%%% The entire process is under rework. Current version merged Roi's additions
%%% into existing frame + re-did the record stored here & supressed/fixed most errors.
%%% -----------------
%%% **Update (20/8):**
%%% - Removed bot decision making from player_fsm, now handled by bot_handler
%%% - adapted init/1 to account for changes
%%% -----------------
%%% **Update (21/8):**
%%% - Overhauled entire state machine
%%% - Fixed internal and helper functions
%%% **Update (23/8):**
%%% - Implemented apply_powerup/2
%%% @end
%%% Created : 06. Jul 2025
%%%-------------------------------------------------------------------
-module(player_fsm).
-behaviour(gen_statem).

-export([start_link/4]).
-export([start_signal/1, send_killswitch/1, input_command/2, gn_response/2, inflict_damage/1, bomb_exploded/1, 
        update_target_gn/2, notify_power_up/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([startup/3, idle/3, waiting_for_response/3, immunity_idle/3, immunity_waiting_for_response/3, dead/3]).

%% linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%% Windows compatible
-include("../common_parameters.hrl").

-record(player_data, {
    %% ! irrelevant stats are held in the appropriate mnesia table.
    %% * Identifier
    player_number,      % 1/2/3/4
    %% movement related
    direction = none, % none|up|down|left|right ; timer is sort-of held by movement_cooldown?
    %% Communication
    local_gn = default,
    target_gn = default,
    io_handler_pid,
    %% Bot related/dependent
    bot = false,         % true/false - is this a bot player
    %% Important stats
    life = 3,
    speed = 1,  % movement speed
    bombs = 1,  % max bombs (concurrently)
    bombs_placed = 0, % bombs currently active by player
    %% Timers
    immunity_timer = 0,
    request_cooldown = 0,
    movement_cooldown = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the player FSM with appropriate I/O handler
-spec start_link(PlayerNumber::integer(), GN_Pid::pid(), IsBot::boolean(), IO_pid::pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(PlayerNumber, GN_Pid, IsBot, IO_pid) ->
    % Spawn player FSM
    ServerName = list_to_atom("player_" ++ integer_to_list(PlayerNumber)),
    gen_statem:start_link({global, ServerName}, ?MODULE, [PlayerNumber, GN_Pid, IsBot, IO_pid], []).

%% @doc Start the game for the player
start_signal(PlayerPid) ->
    gen_statem:cast(PlayerPid, {game_start}).

%% @doc Send killswitch to the player
send_killswitch(PlayerPid) ->
    gen_statem:cast(PlayerPid, {killswitch}).

%% @doc Send input command from I/O handler
input_command(PlayerPid, Command) ->
    gen_statem:cast(PlayerPid, {input_command, Command}).

%% @doc Send response from GN
gn_response(PlayerNum, Response) ->
    gen_statem:cast(list_to_atom("player_" ++ integer_to_list(PlayerNum)), {gn_response, Response}).

%% @doc Inflict damage on player (from explosion)
inflict_damage(PlayerPid) ->
    gen_statem:cast(PlayerPid, inflict_damage).

%% @doc Notify player of one of his own bombs exploding
bomb_exploded(PlayerPid) ->
    gen_statem:cast(PlayerPid, bomb_exploded).

%% @doc Update target_GN (due to switch coordinates)
update_target_gn(PlayerPid, NewTargetGN) ->
    gen_statem:cast(PlayerPid, {update_target_gn, NewTargetGN}).

%% @doc notify player FSM about relevant power-ups gathered (those we keep track of within)
notify_power_up(PlayerPid, PowerUp) -> % content should be {movespeed, X} | {bombs, X} | {life, X}
    gen_statem:cast(PlayerPid, {notify_power_up, PowerUp}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([PlayerNumber, GN_Pid, IsBot, IOHandlerPid]) ->
    % Get GN name - handle both local and global registration
    GN_registered_name = case process_info(GN_Pid, registered_name) of
        {registered_name, LocalName} -> 
            LocalName;  % locally registered
        [] -> 
            % Not locally registered, check if it's globally registered
            % For GN servers, construct the expected global name
            list_to_atom("GN" ++ integer_to_list(PlayerNumber) ++ "_server")
    end,
    Data = #player_data{
        player_number = PlayerNumber,
        local_gn = GN_registered_name,
        target_gn = GN_registered_name, % starting at own GN's quarter
        bot = IsBot,
        io_handler_pid = IOHandlerPid
    },

    %% set player PID in I/O handler
    ok = if
        IsBot == false -> 
            io_handler:set_player_pid(IOHandlerPid, self());
        true -> 
            bot_handler:set_player_pid(IOHandlerPid, self())
    end,
    io:format("**##**PLAYER FSM FINISHED INIT **##**~n"),
    %% move to start-up state - leave when given a message that the game starts
    {ok, startup, Data}.

%%%===================================================================
%%% State Functions
%%%===================================================================

%%% -------------------------------------------------------------------
%%%                         Start-up State
%%% -------------------------------------------------------------------
%% @doc Start-up state - awaiting starting signal before allowing inputs.
%% when receiving said start signal, updates io/bot handler they can start
startup(cast, {game_start}, Data) -> %% TODO: this message does not arrive - check why
    %% Notify I/O and bot handlers that the game has started
    io:format("**## PLAYER FSM RECEIVED 'game_start'~n"),
    ok = if
        Data#player_data.bot == false -> 
            io_handler:game_start(Data#player_data.io_handler_pid); %% TODO: these functions do NOT EXIST
        true -> 
            bot_handler:game_start(Data#player_data.io_handler_pid)
    end,
    %% Begin timer ticks
    erlang:send_after(2*?TICK_DELAY, self(), timer_tick),
    {next_state, idle, Data};

startup(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

%% Handle tick messages in startup state (ignore them)
startup(info, timer_tick, Data) ->
    {keep_state, Data};

%% Ignore all other inputs until game starts
startup(_Type, _Event, Data) ->
    {keep_state, Data}.


%%% -------------------------------------------------------------------
%%%                         Idle State
%%% -------------------------------------------------------------------
%% @doc Idle state - ready to receive commands, not waiting for a response

idle(info, timer_tick, Data) ->
    handle_tick(idle, Data);

idle(cast, {input_command, Command}, Data) ->
    %% return value is determined in the handle function
    handle_input_command(Command, Data);

idle(cast, {gn_response, _Response}, Data) ->
    %% Unexpected GN response in idle state - ignore and log
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unexpected GN response in idle state: ~p~n", [Hour, Min, Sec, {gn_response, _Response}]),
    error_logger:info_msg("Unexpected GN response in idle state: ~p", [{gn_response, _Response}]),
    {keep_state, Data};

idle(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

idle(cast, inflict_damage, Data) ->
    case Data#player_data.life > 1 of
        true ->
            %% Take damage, notify GN, switch to 'immunity idle' state
            NewData = Data#player_data{
                life = Data#player_data.life - 1,
                immunity_timer = ?IMMUNITY_TIME
            },            
            {next_state, immunity_idle, NewData};
        false ->
            NewData = Data#player_data{life = 0},
            %% Player dies
            %% notify io/bot handler (to stop sending things, can also terminate)
            send_io_ack(player_died, NewData),
            %% notify GN - to remove player from mnesia table
            gen_server:cast(Data#player_data.local_gn, {player_died, NewData#player_data.player_number}),
            %% Switch to 'dead' state - process doesn't terminate, only when receiving 'killswitch'
            {next_state, dead, NewData}
    end;

idle(cast, bomb_exploded, Data) ->
    %% Bomb placed by the player exploded - update active bombs count
    NewData = Data#player_data{
        bombs_placed = max(0, Data#player_data.bombs_placed - 1)
    },
    {keep_state, NewData};

idle(cast, {update_target_gn, NewGN}, Data) ->
    %% Update target GN due to switch coordinates
    NewData = Data#player_data{target_gn = NewGN},
    {keep_state, NewData};

idle(cast, {notify_power_up, PowerUp}, Data) ->
    %% Notify player FSM about relevant power-ups gathered
    NewData = apply_powerup(PowerUp, Data),
    {keep_state, NewData};

idle(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%% -------------------------------------------------------------------
%%%                     Waiting_for_response State
%%% -------------------------------------------------------------------
%%% @doc Waiting for response from GN - this state is entered after sending a request to GN and waiting for its response
waiting_for_response(info, timer_tick, Data) ->
    handle_tick(waiting_for_response, Data);

waiting_for_response(cast, {input_command, _Command}, Data) ->
    %% Unexpected input command in waiting_for_response state - ignore and log
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unexpected input command in waiting_for_response state: ~p~n", [Hour, Min, Sec, {input_command, _Command}]),
    error_logger:info_msg("Unexpected input command in waiting_for_response state: ~p", [{input_command, _Command}]),
    {keep_state, Data};

waiting_for_response(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

waiting_for_response(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

waiting_for_response(cast, inflict_damage, Data) ->
    % Can still take damage while waiting for GN response
    case Data#player_data.life > 1 of
        true ->
            %% Take damage, notify GN, switch to 'immunity_waiting_for_response' state
            NewData = Data#player_data{
                life = Data#player_data.life - 1,
                immunity_timer = ?IMMUNITY_TIME
            },
            {next_state, immunity_waiting_for_response, NewData};
        false ->
            NewData = Data#player_data{life = 0},
            %% Player dies
            %% notify io/bot handler (to stop sending things, can also terminate)
            send_io_ack(player_died, NewData),
            %% notify GN - to remove player from mnesia table
            gen_server:cast(Data#player_data.local_gn, {player_died, NewData#player_data.player_number}),
            %% Switch to 'dead' state - process doesn't terminate, only when receiving 'killswitch'
            {next_state, dead, NewData}
    end;

waiting_for_response(cast, bomb_exploded, Data) ->
    %% Bomb placed by the player exploded - update active bombs count
    NewData = Data#player_data{
        bombs_placed = max(0, Data#player_data.bombs_placed - 1)
    },
    {keep_state, NewData};

waiting_for_response(cast, {update_target_gn, NewGN}, Data) ->
    %% Update target GN due to switch coordinates
    NewData = Data#player_data{target_gn = NewGN},
    {keep_state, NewData};

waiting_for_response(cast, {notify_power_up, PowerUp}, Data) ->
    %% Notify player FSM about relevant power-ups gathered
    NewData = apply_powerup(PowerUp, Data),
    {keep_state, NewData};

waiting_for_response(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%% -------------------------------------------------------------------
%%%                     Immunity_idle State
%%% -------------------------------------------------------------------
%% @doc Immunity idle state - cannot take damage
immunity_idle(info, timer_tick, Data) ->
    handle_tick(immunity_idle, Data);

immunity_idle(cast, {input_command, Command}, Data) ->
    %% return value is determined in the handle function
    handle_input_command(Command, Data);

immunity_idle(cast, {gn_response, _Response}, Data) ->
    %% Unexpected GN response in immunity_idle state - ignore and log
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unexpected GN response in immunity_idle state: ~p~n", [Hour, Min, Sec, {gn_response, _Response}]),
    error_logger:info_msg("Unexpected GN response in immunity_idle state: ~p", [{gn_response, _Response}]),
    {keep_state, Data};

immunity_idle(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

immunity_idle(cast, inflict_damage, Data) ->
    %% cannot take damage while in immunity state
    {keep_state, Data};

immunity_idle(cast, bomb_exploded, Data) ->
    %% Bomb placed by the player exploded - update active bombs count
    NewData = Data#player_data{
        bombs_placed = max(0, Data#player_data.bombs_placed - 1)
    },
    {keep_state, NewData};

immunity_idle(cast, {update_target_gn, NewGN}, Data) ->
    %% Update target GN due to switch coordinates
    NewData = Data#player_data{target_gn = NewGN},
    {keep_state, NewData};

immunity_idle(cast, {notify_power_up, PowerUp}, Data) ->
    %% Notify player FSM about relevant power-ups gathered
    NewData = apply_powerup(PowerUp, Data),
    {keep_state, NewData};

immunity_idle(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%% -------------------------------------------------------------------
%%%             Immunity_waiting_for_response State
%%% -------------------------------------------------------------------
%% @doc Immunity wait for response state - player is immune and waiting for GN response
immunity_waiting_for_response(info, timer_tick, Data) ->
    handle_tick(immunity_waiting_for_response, Data);

immunity_waiting_for_response(cast, {input_command, _Command}, Data) ->
    %% Unexpected input command in immunity_waiting_for_response state - ignore and log
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unexpected input command in immunity_waiting_for_response state: ~p~n", [Hour, Min, Sec, {input_command, _Command}]),
    error_logger:info_msg("Unexpected input command in immunity_waiting_for_response state: ~p", [{input_command, _Command}]),
    {keep_state, Data};

immunity_waiting_for_response(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

immunity_waiting_for_response(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

immunity_waiting_for_response(cast, inflict_damage, Data) ->
    %% cannot take damage while in immunity state
    {keep_state, Data};

immunity_waiting_for_response(cast, bomb_exploded, Data) ->
    %% Bomb placed by the player exploded - update active bombs count
    NewData = Data#player_data{
        bombs_placed = max(0, Data#player_data.bombs_placed - 1)
    },
    {keep_state, NewData};

immunity_waiting_for_response(cast, {update_target_gn, NewGN}, Data) ->
    %% Update target GN due to switch coordinates
    NewData = Data#player_data{target_gn = NewGN},
    {keep_state, NewData};

immunity_waiting_for_response(cast, {notify_power_up, PowerUp}, Data) ->
    %% Notify player FSM about relevant power-ups gathered
    NewData = apply_powerup(PowerUp, Data),
    {keep_state, NewData};

immunity_waiting_for_response(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%% -------------------------------------------------------------------
%%%                     Dead State
%%% -------------------------------------------------------------------
%% @doc Dead state - player is dead. Process dies when receiving killswitch message
dead(cast, {killswitch}, Data) ->
    %% Killswitch received - stop the process. Also stop io/bot handler
    send_io_ack(killswitch_received, Data),

    ok = if
        Data#player_data.bot == false -> 
            io_handler:send_killswitch(Data#player_data.io_handler_pid);
        true -> 
            bot_handler:send_killswitch(Data#player_data.io_handler_pid)
    end,
    {stop, killswitch, Data};

dead(cast, _, Data) ->
    % Dead players can't do anything
    {keep_state, Data};

dead(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%%===================================================================
%%%                     Internal Functions                          |
%%%===================================================================
handle_input_command(Command, Data) ->
    case can_send_request(Command, Data) of
        true ->
            case process_command(Command, Data) of
                {ok, Request, NewData} -> % 
                    % Request was pre-checked, send to local GN
                    gen_server:cast(Data#player_data.local_gn, 
                        {player_message, Request}),
                    case Data#player_data.immunity_timer of
                        0 -> %% we were at idle state, no immunity
                            {next_state, waiting_for_response, NewData};
                        _ -> %% we were in immunity_idle state
                            {next_state, immunity_waiting_for_response, NewData}
                    end;
                {not_ok, Reason} ->
                    %% Operation blocked - send ACK to io/bot handler, keep state, set request cooldown timer
                    send_io_ack({request_denied_by_player_fsm, Reason}, Data),
                    NewData = Data#player_data{request_cooldown = ?REQUEST_COOLDOWN},
                    {keep_state, NewData};
                {error, Reason} ->
                    % Invalid command
                    send_io_ack({error, Reason}, Data),
                    {keep_state, Data}
            end;
        false -> % Still in cooldown
            %% do nothing with the request, send an ACK to the io/bot handler
            send_io_ack({error, cooldown}, Data),
            {keep_state, Data}
    end.

process_command(Command, Data) ->
    case Command of
        {move, Direction} ->
            %% * Request Form = {move_request, "WhoAmI", "TargetGN", "Direction"}
            Request = {move_request, Data#player_data.player_number, Data#player_data.target_gn, Direction},
            %% Update Data - set direction to the requested one, DOES NOT SET COUNTERS (only after confirmation)
            NewData = Data#player_data{direction = Direction},
            {ok, Request, NewData};

        drop_bomb ->
            case can_drop_bomb(Data) of % Active bombs < max_bombs
                true ->
                    %% * Request form = {place_bomb_request, "WhoAmI", "TargetGN"}
                    Request = {place_bomb_request, Data#player_data.player_number, Data#player_data.target_gn}, % create bomb request
                    %% No data to update - when ACK received we will update bombs_placed and set timers
                    {ok, Request, Data};
                false ->
                    {not_ok, no_bombs_available} % no bombs available
            end;
            
        ignite_remote ->
            %% player_fsm doesn't hold information about bomb type, immediately send this to GN to handle there
            %% * Request Form = {ignite_bomb_request, "WhoAmI", "TargetGN"}
            Request = {ignite_bomb_request, Data#player_data.player_number},
            {ok, Request, Data};
            
        _ ->
            {error, {unknown_command, Command}}
    end.

handle_gn_response(Response, Data) ->
    case Response of
         {move_result, accepted} ->
            %% Move accepted - player is now moving, cannot move again for X seconds
            %% but can still drop bombs or use other abilities
            NewData = Data#player_data{
                movement_cooldown = ?TILE_MOVE - ((Data#player_data.speed - 1) * ?MS_REDUCTION)},
            send_io_ack({move_accepted}, NewData), % "release" the io/bot handler to send more requests
            if
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        {move_result, denied} ->
            %% Move denied - short cooldown before any request
            NewData = Data#player_data{request_cooldown = ?REQUEST_COOLDOWN},
            send_io_ack({move_denied}, NewData), %% "release" the io/bot handler to send more requests
            if 
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        {bomb_result, accepted} ->
            %% Bomb placed
            NewData = Data#player_data{
                request_cooldown = ?REQUEST_COOLDOWN,
                bombs_placed = Data#player_data.bombs_placed + 1
            },
            send_io_ack(bomb_placed, NewData), % "release" the io/bot handler to send more requests
            if 
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        {bomb_result, denied} ->
            %% Bomb drop failed - restore bomb count
            NewData = Data#player_data{request_cooldown = ?REQUEST_COOLDOWN},
            send_io_ack({bomb_not_placed}, NewData),
            if 
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        {ignite_result, accepted} ->
            %% Remote bombs ignited
            NewData = Data#player_data{request_cooldown = ?REQUEST_COOLDOWN},
            send_io_ack({ignited_bombs}, NewData),
            if 
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        {ignite_result, denied} ->
            %% Remote ignition failed
            %% ? This will be sent when we we don't have remote bombs to blow
            NewData = Data#player_data{
                request_cooldown = 200     % short cooldown before retry
            },
            send_io_ack({ignite_failed}, NewData),
            if 
                Data#player_data.immunity_timer == 0 ->
                    {next_state, idle, NewData};
                true ->
                    {next_state, immunity_idle, NewData}
            end;

        _Unknown ->
            %% Unknown response - log it, do nothing (to not break the game)
            {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
            io:format("[~2..0B:~2..0B:~2..0B]: Unexpected GN response command: ~p~n",
                [Hour, Min, Sec, {_Unknown}]),
            error_logger:info_msg("Unexpected input command in immunity_waiting_for_response state: ~p", [{_Unknown}]),
            send_io_ack({error, unknown_response, {_Unknown}}, Data),
            {keep_state, Data}
    end.


handle_tick(CurrentState, Data) ->
    %% Reduce cooldowns, notify GN of significant changes (every 1 sec), switch states if necessary
    %% Fetch Current counters
    {Immunity_cd, Request_cd, Move_cd} = {Data#player_data.immunity_timer, 
                                         Data#player_data.request_cooldown, 
                                         Data#player_data.movement_cooldown},
    %% Update counters
    Updated_immunityTimer = max(0, Immunity_cd - ?TICK_DELAY),
    Updated_requestCooldown = max(0, Request_cd - ?TICK_DELAY),
    Updated_movementCooldown = max(0, Move_cd - ?TICK_DELAY),

    %% general request cooldown handling
    case Updated_requestCooldown of
        Immunity_cd -> % was at 0, nothing to report or change
            ok;
        0 -> % cooldown just ended, notify GN
            gen_server:cast(Data#player_data.local_gn, {player_message,
                {cooldown_update, Data#player_data.target_gn, 
                    {request_cooldown_update, Data#player_data.player_number, Updated_requestCooldown}
                }
            });
        _ -> % still in cooldown (if we want more checkpoints to send we can add them here)
            ok
    end,
    %% Movement request cooldown handling
    case (Updated_movementCooldown rem 200) of
        Move_cd -> % was at 0, nothing to report or change
            ok;
        0 -> % Report movement cooldown changes every 200 ms to GN
            gen_server:cast(Data#player_data.local_gn, {player_message,
                {cooldown_update, Data#player_data.target_gn,
                    {movement_cooldown_update, Data#player_data.player_number, Updated_movementCooldown}
                }
            });
        _ -> % still in cooldown (if we want more checkpoints to send we can add them here)
            ok
    end,
    %% Update State to accomodate counter/timer updates
    NewData = Data#player_data{
        immunity_timer = Updated_immunityTimer,
        request_cooldown = Updated_requestCooldown,
        movement_cooldown = Updated_movementCooldown
    },
    %% immunity handling - this timer is the only one responsible for state changes
    if
        Updated_immunityTimer == Immunity_cd -> % was at 0, nothing to report
            erlang:send_after(?TICK_DELAY, self(), timer_tick), % Schedule next tick
            {keep_state, NewData};
        Updated_immunityTimer == 0 -> % immunity ended, notify GN
            gen_server:cast(Data#player_data.local_gn, {player_message,
                {cooldown_update, Data#player_data.target_gn,
                    {immunity_update, Data#player_data.player_number, Updated_immunityTimer}}}),
            erlang:send_after(?TICK_DELAY, self(), timer_tick), % Schedule next tick
            case CurrentState of
                immunity_idle -> {next_state, idle, NewData};
                immunity_waiting_for_response -> {next_state, waiting_for_response, NewData}
            end;
        Updated_immunityTimer == 1000 -> % 1sec immunity left, report to GN
            gen_server:cast(Data#player_data.local_gn, {player_message,
                {cooldown_update, Data#player_data.target_gn,
                    {immunity_update, Data#player_data.player_number, Updated_immunityTimer}}}),
            erlang:send_after(?TICK_DELAY, self(), timer_tick), % Schedule next tick
            {keep_state, NewData};
        Updated_immunityTimer == 2000 -> % 2sec immunity left, report to GN
            gen_server:cast(Data#player_data.local_gn, {player_message,
                {cooldown_update, Data#player_data.target_gn,
                    {immunity_update, Data#player_data.player_number, Updated_immunityTimer}}}),
            erlang:send_after(?TICK_DELAY, self(), timer_tick), % Schedule next tick
            {keep_state, NewData};
        true -> % still in immunity or was already at 0
            erlang:send_after(?TICK_DELAY, self(), timer_tick), % Schedule next tick
            {keep_state, NewData}
    end.

handle_common_events(Type, Event, Data) ->
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unhandled event: ~p~n", [Hour, Min, Sec, {Type, Event}]),
    error_logger:info_msg("Unhandled event (State|Event): ~p~n", [{Type, Event}]),
    {keep_state, Data}. % default handler for unexpected events


%%%===================================================================
%%%                         Helper Functions                         |
%%%===================================================================

-spec can_send_request(Command::term(), Data::#player_data{}) -> boolean().
can_send_request(Command, Data) ->
    %% helper function that returns 'true' if we can send this current command (based on timers)
    case Command of
        {move, _Direction} ->
            % Movement requests needs both movement cooldown and request cooldown to be 0
            Data#player_data.movement_cooldown =< 0 andalso Data#player_data.request_cooldown =< 0;
        _ ->
            % Non-movement requests only need general cooldown to be 0
            Data#player_data.request_cooldown =< 0
    end.

-spec can_drop_bomb(Data::#player_data{}) -> boolean().
can_drop_bomb(Data) ->
    Data#player_data.bombs_placed < Data#player_data.bombs. % can drop bomb if placed bombs < max bombs

-spec apply_powerup(Powerup::tuple(), Data::#player_data{}) -> ok.
apply_powerup(Powerup, Data) ->
    case Powerup of
        {movespeed, X} ->
            Data#player_data{speed = X};
        {bombs, X} ->
            Data#player_data{bombs = X};
        {life, X} ->
            Data#player_data{life = Data#player_data.life + X};
        _ -> % powerup not stored on player fsm process
            Data
    end.


send_io_ack(Response, Data) ->
    gen_server:cast(Data#player_data.io_handler_pid, {player_ack, Response}).

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
