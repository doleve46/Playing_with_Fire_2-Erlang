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
%%% 
%%% @end
%%% Created : 06. Jul 2025
%%%-------------------------------------------------------------------
-module(player_fsm).
-behaviour(gen_statem).

-export([start_link/4, input_command/2, gn_response/2, inflict_damage/1, bomb_exploded/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([idle/3, waiting_gn_response/3, immunity/3, dead/3, disconnected/3]).

%% linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%% Windows compatible
-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/common_parameters.hrl").

-record(player_data, {
    %% ! irrelevant stats are held in the appropriate mnesia table.
    %% * Identifier
    player_number,      % 1/2/3/4
    %% movement related
    movement = false, % false |{true,TimerRef}
    % todo: changed from next_position to movement&direction, verify consistency in the code
    %% Communication
    local_gn = default,
    target_gn = default,
    io_handler_pid,

    %% Bot related/dependent
    disconnected = 0,     % counter to 60 (seconds), then kill process
    bot = false,         % true/false - is this a bot player

    %% Important stats
    life = 3,
    speed = 1,  % movement speed
    bombs = 1,  % max bombs (concurrently)
    explosion_radius = 1, % radius of explosion (besides epicenter)
    bombs_placed = 0, % bombs currently active by player
    %% Timers
    immunity_timer = 0,
    request_cooldown = 0,
    movement_cooldown = 0,
    % ! last_request_time is a weird logical decision
    last_request_time = 0 % timestamp of last GN request
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
    gen_statem:start_link({local, ServerName}, ?MODULE, [PlayerNumber, GN_Pid, IsBot, IO_pid], []).


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
    %% TODO: implement support for this function


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

-spec init([integer(), pid(), boolean(), pid()]) ->
    {ok, state, #player_data{}}.
init([PlayerNumber, GN_Pid, IsBot, IOHandlerPid]) ->
    {_, GN_registered_name} = process_info(GN_Pid, registered_name),
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


    %% Start cooldown timer
    erlang:send_after(?TICK_DELAY, self(), tick),
    
    {ok, idle, Data}.

%%%===================================================================
%%% State Functions
%%%===================================================================

%% @doc Idle state - ready to receive commands
idle(cast, {input_command, Command}, Data) ->
    handle_input_command(Command, Data);

idle(cast, {gn_response, _Response}, Data) ->
    % Unexpected GN response in idle state - ignore
    {keep_state, Data};

idle(cast, inflict_damage, Data) ->
    case Data#player_data.life > 1 of
        true ->
            % Take damage, enter immunity
            NewLife = Data#player_data.life - 1,
            NewData = Data#player_data{life = NewLife},
            TimerRef = erlang:send_after(?IMMUNITY_TIME, self(), immunity_end),
            ImmuneData = NewData#player_data{immunity_timer = TimerRef},
            
            % Send ack to I/O if available
            send_io_ack({damage_taken, NewLife}, ImmuneData),
            {next_state, immunity, ImmuneData};
        false ->
            % Player dies
            NewData = Data#player_data{life = 0},
            send_io_ack(player_died, NewData),
            {next_state, dead, NewData}
    end;

idle(info, tick, Data) ->
    handle_tick(Data);

idle(info, disconnect_check, Data) ->
    handle_disconnect_check(Data);

idle(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Waiting for GN response state
waiting_gn_response(cast, {input_command, _Command}, Data) ->
    % Ignore new inputs while waiting for GN response
    {keep_state, Data};

waiting_gn_response(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

waiting_gn_response(cast, inflict_damage, Data) ->
    % Can still take damage while waiting for GN response
    idle(cast, inflict_damage, Data);

waiting_gn_response(info, tick, Data) ->
    handle_tick(Data);

waiting_gn_response(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Immunity state - cannot take damage
immunity(cast, {input_command, Command}, Data) ->
    handle_input_command(Command, Data);

immunity(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

immunity(cast, inflict_damage, Data) ->
    % Immune to damage - ignore damage
    {keep_state, Data};

immunity(info, immunity_end, Data) ->
    % Immunity period ended
    NewData = Data#player_data{immunity_timer = none},
    {next_state, idle, NewData};

immunity(info, tick, Data) ->
    handle_tick(Data);

immunity(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Dead state - player is dead
dead(cast, {input_command, _Command}, Data) ->
    % Dead players can't do anything
    send_io_ack(player_dead, Data),
    {keep_state, Data};

dead(cast, {gn_response, _Response}, Data) ->
    {keep_state, Data};

dead(cast, inflict_damage, Data) ->
    % Already dead
    {keep_state, Data};

dead(info, respawn, Data) ->
    % Respawn logic (if implemented)
    NewData = Data#player_data{life = 3},
    {next_state, idle, NewData};

dead(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Disconnected state - player disconnected
disconnected(info, disconnect_timeout, Data) ->
    % 60 seconds passed, kill process
    {stop, disconnect_timeout, Data};

disconnected(cast, {input_command, _Command}, Data) ->
    % Player reconnected
    NewData = Data#player_data{disconnected = 0},
    {next_state, idle, NewData};

disconnected(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

handle_input_command(Command, Data) ->
    case can_send_request(Command, Data) of
        true ->
            case process_command(Command, Data) of
                {ok, Request, NewData} ->
                    % Send request to local GN
                    gen_server:cast(Data#player_data.local_gn, 
                        {player_message, Request}), % ? removed self() from the tuple, relevant data already in Request
                    
                    % Update cooldown
                    CooldownData = NewData#player_data{
                        last_request_time = erlang:convert_time_unit(erlang:system_time(), native, millisecond)
                    },
                    {next_state, waiting_gn_response, CooldownData};
                {error, Reason} ->
                    % Invalid command
                    send_io_ack({error, Reason}, Data),
                    {keep_state, Data}
            end;
        false ->
            % Still in cooldown
            send_io_ack({error, cooldown}, Data),
            {keep_state, Data}
    end.

process_command(Command, Data) ->
    ok.
    %% ? ENTIRE FUNCTION IS A MESS - COMMENTED OUT TO NOT SEE ERRORS FROM IT
%    case Command of
%        {move, Direction} ->
%            %% * Request = {move_request, "WhoAmI", "TargetGN", "Direction"}
%            %% ! CHANGE TO: 
%            Request = {move_request, Data#player_data.player_number, Data#player_data.target_gn, Direction},
%            %//NextPos = calculate_next_position(Data#player_data.position, Direction),    % calculate next position
%            %//Request = {move_request, Data#player_data.position, NextPos, 
%            %//          Data#player_data.special_abilities},  % create move request
%            NewData = Data#player_data{direction = Direction}, % register request in 'direction'
%            {ok, Request, NewData};
%            
%        drop_bomb ->
%            case can_drop_bomb(Data) of
%                true ->
%                    BombType = get_bomb_type(Data#player_data.special_abilities),   % determine bomb type
%                    Request = {drop_bomb_request, Data#player_data.position, 
%                              BombType, Data#player_data.explosion_radius}, % create bomb request
%                    NewData = Data#player_data{bombs_placed = Data#player_data.bombs_placed + 1},   % increment placed bombs
%                    {ok, Request, NewData};
%                false ->
%                    {error, no_bombs_available} % no bombs available
%            end;
%            
%        ignite_remote ->
%            case lists:member(remote_bomb, Data#player_data.special_abilities) of
%                true ->
%                    Request = {ignite_remote_request},  % create ignite request
%                    {ok, Request, Data};    
%                false ->
%                    {error, no_remote_ability}  % no remote bomb ability
%            end;
%            
%        _ ->
%            {error, unknown_command}    
%    end.

handle_gn_response(Response, Data) ->
    case Response of
         {move_result, accepted} ->
            % Move accepted - player is now moving, cannot move again for X seconds
            % but can still drop bombs or use other abilities
            NewData = Data#player_data{
                movement_cooldown = 2000,  % 2 seconds until next movement allowed
                request_cooldown = 0       % can send non-movement requests immediately
            },
            send_io_ack({move_accepted}, NewData),
            {next_state, idle, NewData};

        {move_result, denied, Reason} ->
            % Move denied - short cooldown before any request
            NewData = Data#player_data{
                request_cooldown = 200     % 200ms before next request
            },
            send_io_ack({move_denied, Reason}, NewData),
            {next_state, idle, NewData};

        {move_result, accepted, switch_gn, NewTargetGN} ->
            % Move accepted AND player is switching to new GN territory
            NewData = Data#player_data{
                movement_cooldown = 2000,  % 2 seconds until next movement
                request_cooldown = 0,      % can send non-movement requests
                target_gn = NewTargetGN    % update target GN
            },
            send_io_ack({move_accepted, gn_switched, NewTargetGN}, NewData),
            {next_state, idle, NewData};

        {bomb_result, success} ->
            % Bomb dropped successfully
            NewData = Data#player_data{
                request_cooldown = 100     % short cooldown before next request
            },
            send_io_ack(bomb_dropped, NewData),
            {next_state, idle, NewData};

        {bomb_result, failed, Reason} ->
            % Bomb drop failed - restore bomb count
            NewData = Data#player_data{
                bombs_placed = Data#player_data.bombs_placed - 1,
                request_cooldown = 200     % short cooldown before retry
            },
            send_io_ack({bomb_failed, Reason}, NewData),
            {next_state, idle, NewData};

        {bomb_exploded} ->
            % One of player's bombs exploded - restore bomb count
            NewData = Data#player_data{bombs_placed = Data#player_data.bombs_placed - 1},
            {keep_state, NewData};  % no ack needed for explosion

        {ignite_result, success, Count} ->
            % Remote bombs ignited
            NewData = Data#player_data{
                request_cooldown = 300     % cooldown before next request
            },
            send_io_ack({ignited_bombs, Count}, NewData),
            {next_state, idle, NewData};

        {ignite_result, failed, Reason} ->
            % Remote ignition failed
            NewData = Data#player_data{
                request_cooldown = 200     % short cooldown before retry
            },
            send_io_ack({ignite_failed, Reason}, NewData),
            {next_state, idle, NewData};

        _ ->
            % Unknown response
            send_io_ack({error, unknown_response}, Data),
            {next_state, idle, Data}
    end.


handle_tick(Data) ->
    % Reduce cooldowns
    NewRequestCooldown = max(0, Data#player_data.request_cooldown - ?TICK_DELAY),
    NewMovementCooldown = max(0, Data#player_data.movement_cooldown - ?TICK_DELAY),
    % Reduce bot bomb cooldown
    NewBombCooldown = max(0, Data#player_data.bot_bomb_cooldown - ?TICK_DELAY),
    NewData = Data#player_data{
        request_cooldown = NewRequestCooldown,
        movement_cooldown = NewMovementCooldown,
        bot_bomb_cooldown = NewBombCooldown
    }, 

    % Schedule next tick
    erlang:send_after(?TICK_DELAY, self(), tick),
    {keep_state, NewData}.

handle_disconnect_check(Data) ->
    case Data#player_data.disconnected of
        Count when Count >= 60 ->
            {stop, disconnect_timeout, Data};   % 60 seconds passed, stop the process
        Count ->
            NewData = Data#player_data{disconnected = Count + 1},
            erlang:send_after(1000, self(), disconnect_check),
            {keep_state, NewData}   % increment disconnect counter
    end.

handle_common_events(_Type, _Event, Data) ->
    io:format("Unhandled event: ~p~n", [{_Type, _Event}]),
    error_logger:info_msg("Unhandled event: ~p~n", [{_Type, _Event}]),
    {keep_state, Data}. % default handler for unexpected events

%% Handle bomb placement request
playing(cast, place_bomb, StateData) ->
    % ? NOT THE PROPER DESIGN CHOICE - NEED TO PASS THROUGH LOCAL GN
    %% Get current position from your state data
    CurrentPosition = StateData#player_data.position, % Adjust this field name to match your record

    %% Send request to target GN  
    gen_server:cast(StateData#player_data.target_gn,  % Adjust this field name to match your record
        {player_message, {place_bomb, StateData#player_data.player_number, CurrentPosition}}),
    {keep_state_and_data};

%% Handle bomb placement responses from GN
playing(info, {gn_response, {bomb_placed, Result}}, StateData) ->
    case Result of
        ok -> 
            io:format("Player ~p: Bomb placed successfully~n", [StateData#player_data.player_number]);
        {error, Reason} -> 
            io:format("Player ~p: Failed to place bomb: ~p~n", 
                     [StateData#player_data.player_number, Reason])
    end,
    {keep_state_and_data};

%% Handle damage taken from explosions
playing(info, {gn_response, {damage_taken, NewLife}}, StateData) ->
    UpdatedStateData = StateData#player_data{life = NewLife}, % Adjust field name to match your record
    case NewLife =< 0 of
        true ->
            io:format("Player ~p died!~n", [StateData#player_data.player_number]),
            {next_state, dead, UpdatedStateData};
        false ->
            io:format("Player ~p damaged, life remaining: ~p~n", 
                     [StateData#player_data.player_number, NewLife]),
            {keep_state, UpdatedStateData}
    end;

%% Handle powerup collection
playing(info, {gn_response, {powerup_collected, PowerupType}}, StateData) ->
    io:format("Player ~p collected powerup: ~p~n", [StateData#player_data.player_number, PowerupType]),
    {keep_state_and_data}.

%% In your io_handler.erl, when player presses bomb key:
handle_bomb_key_press(PlayerPid) ->
    player_fsm:place_bomb(PlayerPid).


%%%===================================================================
%%% Helper Functions
%%%===================================================================

can_send_request(Command, Data) ->
    case Command of
        {move, _Direction} ->
            % Movement requests need both general and movement cooldowns to be 0
            Data#player_data.request_cooldown =< 0 andalso 
            Data#player_data.movement_cooldown =< 0;
        _ ->
            % Non-movement requests only need general cooldown to be 0
            Data#player_data.request_cooldown =< 0
    end.

-spec can_drop_bomb(Data::#player_data{}) -> boolean().
can_drop_bomb(Data) ->
    Data#player_data.bombs_placed < Data#player_data.bombs. % can drop bomb if placed bombs < max bombs


% ? APPLY POWER UP SHOULD BE OVERHAULED - JUST RECEIVE MESSAGES FROM GN WITH BUFFS STORED IN THIS PROCESS
apply_powerup(none, Data) ->
    Data;
apply_powerup(Powerup, Data) ->
    case Powerup of
        movespeed ->
            Data#player_data{speed = Data#player_data.speed + 1};
        more_bombs ->
            Data#player_data{bombs = Data#player_data.bombs + 1};
        range ->
            Data#player_data{explosion_radius = Data#player_data.explosion_radius + 1};
        extra_life ->
            Data#player_data{life = Data#player_data.life + 1};
        _ -> % powerup not stored on player fsm process
            ok
    end.

%% Determine bomb type based on abilities
get_bomb_type(Abilities) ->
    case lists:member(remote_bomb, Abilities) of
        true -> remote; 
        false ->
            case lists:member(repeating_bomb, Abilities) of
                true -> repeating;
                false -> regular
            end
    end.


send_io_ack(Response, Data) ->
    gen_server:cast(Data#player_data.io_handler_pid, {player_ack, Response}).

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% Need to add kick, freeze?