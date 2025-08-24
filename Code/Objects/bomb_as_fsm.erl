%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% ** UPDATE(24/08): **
%%% The bomb movement is not currently under work.
%%% Anything related to it is postponed
%%% @end
%%% Created : 10. Jul 2025 15:41
%%%-------------------------------------------------------------------
-module(bomb_as_fsm).
-author("dolev").

-behaviour(gen_statem).

%% API
-export([start_monitor/4,
    freeze_bomb/1, kick_bomb/2, answer_move_req/2, damage_taken/1, ignite_bomb/1]).

%% gen_statem callbacks
-export([init/1, format_status/1, terminate/3,
    code_change/4, callback_mode/0]).

%% States function
-export([remote_idle/3, armed/3, active_movement/3, delayed_explosion_state/3,
    remote_armed/3, remote_idle_movement/3, remote_armed_frozen_movement/3]).

%% Parameters Definitions
-include("object_records.hrl").
%% Linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%% Windows compatible
-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/common_parameters.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_monitor(Pos_x, Pos_y, Type, Optional) ->
    %% optional is a list containing [Player_ID, Radius]
    %% bomb is nameless - identified based on Pid
    gen_statem:start_monitor(?MODULE, [[Pos_x, Pos_y], Type, self(), Optional], []).

%% @doc send freeze message to bomb
freeze_bomb(BombPid) ->
    gen_statem:cast(BombPid, freeze).

%% @doc return value to this is the reply, {reply, From, {request_movement, Direction}}
%% @doc Sends a message from the overlord GN to 'kick' the bomb. If a movement is requested 
kick_bomb(BombPid, Direction) ->
    gen_statem:call(BombPid, {kick, Direction}).

%% @doc GN answering to move request
%% can be: 'approved', {'approved_switch_gn', NewGN}, 'denied'
answer_move_req(BombPid, Answer) ->
    gen_statem:cast(BombPid, {reply_move_req, Answer}).

damage_taken(BombPid) ->
    gen_statem:cast(BombPid, damage_taken).

%% @doc send ignite message to remote bomb
ignite_bomb(BombPid) ->
    gen_statem:cast(BombPid, ignite).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Position, Type, Gn_Pid, Optional]) ->
    StateData = case Optional of
        [] -> % no owner, default radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = none};
        [Player_ID] -> % stated owner, default radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = Player_ID };
        [Player_ID, Radius] -> % stated owner, custom radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = Player_ID,
                        radius = Radius}
    end,
    case StateData#bomb_state.type of
        regular ->
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {ok, armed, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]};
        remote -> 
            {ok, remote_idle, StateData}; % TODO: complete implementation
        repeating -> % repeating bomb, w.i.p
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {ok, armed, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]}
    end.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    [state_functions, state_enter].

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status({_StateName, _State}) ->
    some_term.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(normal, _CurrentState, _StateData) ->
    %% tell local GN the bomb exploded - goes to handle_info
    % StateData#bomb_state.gn_pid ! {bomb_exploded, self()},
    ok;

terminate(_Reason, _StateName, _State = #bomb_state{}) ->
    ok.



%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #bomb_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% state functions
%%%===================================================================
%% ~~~~~~~~~ State = armed ~~~~~~~~~
%% Syntax for state enter actions - not used here, but syntax is valid
%armed(enter, _OldState, StateData = #bomb_state{}) ->
%    UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
%    {keep_state, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]};

armed(cast, freeze, StateData = #bomb_state{}) ->
    %% if already frozen - do nothing.
    %% else - add FREEZE_DELAY to the timeout timer (fetch old time, calc. leftover + FREEZE_DELAY)
    if
        StateData#bomb_state.status == frozen -> {keep_state_and_data};
        true ->
            UpdatedData = StateData#bomb_state{status=frozen},
            TempTime = calc_new_explode_delay(StateData),
            {keep_state, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]}
    end;

armed({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with direction change (to be updated in mnesia table) 
    %% request movement in said direction, sent to local GN
    %% remain in the same state until answered, retain state_timeout
    gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
    {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, Direction}]};

armed(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            TempTime = calc_new_explode_delay(StateData),
            {next_state, active_movement, StateData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

armed(cast, damage_taken, StateData = #bomb_state{}) ->
    %% explodes after one time TICK
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

armed(cast, ignite, StateData = #bomb_state{}) ->
    %% wrong bomb type, ignore it
    {keep_state, StateData};

armed(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, exploded, StateData};


armed(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(armed, _Type, _Message),
    {keep_state, StateData}.

%% unknown messages - stop the process with an error
%armed(cast, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData};

%armed({call, GN}, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.

%% ~~~~~~~~~ State = active_movement ~~~~~~~~~

active_movement(enter, _OldState, StateData = #bomb_state{}) ->
    %% entering to this state from any other state
    UpdatedData = StateData#bomb_state{movement = true},
    {keep_state, UpdatedData};

active_movement(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, exploded, StateData};

active_movement(cast, freeze, StateData = #bomb_state{}) ->
    %% if already frozen - do nothing.
    %% else - add FREEZE_DELAY to the timeout timer (fetch old time, calc. leftover + FREEZE_DELAY)
    if
    StateData#bomb_state.status == frozen -> {keep_state_and_data};
    true ->
    UpdatedData = StateData#bomb_state{status=frozen},
    TempTime = calc_new_explode_delay(StateData),
    {keep_state, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]}
    end;

active_movement({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% If the new direction is opposite to current direction - stop movement
    %% else - stop current movement and issue request based on new direction
    Opposite_direction = calc_opposite_direction(StateData#bomb_state.direction),
    %% stop current movement, returns to 'armed' state.
    UpdatedData = stop_active_movement(StateData),
    TempTime = calc_new_explode_delay(StateData),
    if 
        Direction == Opposite_direction ->
            %% opposite direction - just cancel current movement timer (if exists)
            {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}], {reply, GN, {none, false}}};
        true ->
            %% any other direction - send request to move at new direction
            %% Check if there's enough time to request the movement before the bomb explodes
            TimeLeft = TempTime - erlang:system_time(millisecond),
            if
                TimeLeft > ?MIN_MOVE_REQ_TIME -> % enough time
                    gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
                    {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode},
                        {reply, GN, {UpdatedData#bomb_state{direction=Direction}, false}}]
                    };
                true -> % not enough time - do not make request
                    {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode},
                        {reply, GN, {none, false}}]}
            end
    end;


active_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, move to 'armed' state, cancel movement timer
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    TempTime = calc_new_explode_delay(StateData),
    {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};

active_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = true
            },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

active_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = true},
            {keep_state, UpdatedData};
        denied -> % movement denied
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            TempTime = calc_new_explode_delay(StateData),
            {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

active_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

active_movement(cast, ignite, StateData = #bomb_state{}) ->
    %% wrong bomb type, ignore it
    {keep_state, StateData};

active_movement(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(active_movement, _Type, _Message),
    {keep_state, StateData}.

%active_movement(cast, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData};

%active_movement({call, GN}, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.

%% ~~~~~~~~~ State = delayed_explosion_state ~~~~~~~~~
%% Throwaway state to ignore everything but state_timeout to explode

delayed_explosion_state(info, _AnyMessage, StateData = #bomb_state{}) ->
    {keep_state, StateData};

delayed_explosion_state(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, exploded, StateData};

delayed_explosion_state(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(delayed_explosion_state, _Type, _Message),
    {keep_state, StateData}.
%% ~~~~~~~~~ State = remote_idle ~~~~~~~~~

remote_idle(cast, freeze, StateData = #bomb_state{}) ->
        {keep_state, StateData#bomb_state{status=frozen}};

remote_idle({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with request for movement in said direction: {request_movement, Direction}
    %% remain in the same state until answered, retain state_timeout
    gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
    {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, Direction}]};

remote_idle(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            {next_state, remote_idle_movement, StateData};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_idle(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_idle(cast, ignite, StateData = #bomb_state{}) ->
    if
        StateData#bomb_state.status == frozen -> % bomb is frozen and armed,
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {next_state, remote_armed, UpdatedData, [{state_timeout, ?FREEZE_DELAY, explode}]};
        true ->
            {stop, exploded, StateData}
    end;

remote_idle(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(remote_idle, _Type, _Message),
    {keep_state, StateData}.
%% ~~~~~~~~~ State = remote_idle ~~~~~~~~~

remote_armed(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, exploded, StateData};

remote_armed(cast, freeze, StateData = #bomb_state{}) ->
    %% Already frozen, ignore this
    {keep_state, StateData};

remote_armed({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with request for movement in said direction, as long as there's enough time before explosion
    %% remain in the same state until answered, retain state_timeout
    TimeLeft = calc_new_explode_delay(StateData) - erlang:system_time(millisecond),
    if
        TimeLeft > ?MIN_MOVE_REQ_TIME -> % enough time
            gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
            {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, Direction}]};
        true -> % not enough time - do not make request
            {keep_state, StateData, [{reply, GN, StateData#bomb_state.direction}]}
    end;


remote_armed(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            TempTime = calc_new_explode_delay(StateData),
            {next_state, remote_armed_frozen_movement, StateData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_armed(cast, damage_taken, StateData = #bomb_state{}) ->
    %% explodes after one time TICK
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_armed(cast, ignite, StateData = #bomb_state{}) ->
    %% Already armed, ignore
    {keep_state, StateData};


remote_armed(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(remote_armed, _Type, _Message),
    {keep_state, StateData}.

%% unknown messages - stop the process with an error
%remote_armed(cast, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData};

%remote_armed({call, GN}, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.


%% ~~~~~~~~~ State = remote_idle_movement ~~~~~~~~~

remote_idle_movement(enter, _OldState, StateData = #bomb_state{}) ->
    %% entering to this state from any other state
    UpdatedData = StateData#bomb_state{movement = true},
    {keep_state, UpdatedData};

remote_idle_movement(cast, freeze, StateData = #bomb_state{}) ->
    {keep_state, StateData#bomb_state{status = frozen}};

remote_idle_movement({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% stop current movement. if the new direction is not opposite to current - request new movement
    Opposite_direction = calc_opposite_direction(StateData#bomb_state.direction),
    UpdatedData = stop_active_movement(StateData),
    if 
        Direction == Opposite_direction ->
            %% opposite direction - just cancel current movement timer (if exists)        
            {next_state, remote_idle, UpdatedData, [{reply, GN, {none, false}}]};
        true ->
            %% any other direction - send request to move at new direction
            gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
            {next_state, remote_idle, UpdatedData, [{reply, GN, {none, false}}]}
    end;

remote_idle_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, stop movement timer
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    {next_state, remote_idle, UpdatedData};

remote_idle_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = true
            },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

remote_idle_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = true},
            {keep_state, UpdatedData};
        denied -> % movement denied
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            {next_state, remote_idle, UpdatedData};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_idle_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_idle_movement(cast, ignite, StateData = #bomb_state{}) ->
    %% if status is normal - explode immediately
    %% else (frozen) - update record, start state_timeout timer & switch to remote_armed_frozen_movement
    case StateData#bomb_state.status of
        normal -> % not frozen, explode immediately
            {stop, exploded, StateData};
        frozen -> % frozen, update record, start timer, switch states
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {next_state, remote_armed_frozen_movement, UpdatedData, [{state_timeout, ?FREEZE_DELAY, explode}]}
    end;

%% unknown messages - stop the process with an error
remote_idle_movement(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(remote_idle_movement, _Type, _Message),
    {keep_state, StateData}.


%remote_idle_movement(cast, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData};

%remote_idle_movement({call, GN}, _Message, StateData = #bomb_state{}) ->
%    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.

%% ~~~~~~~~~ State = remote_armed_frozen_movement ~~~~~~~~~

remote_armed_frozen_movement(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, exploded, StateData};

remote_armed_frozen_movement(cast, freeze, StateData = #bomb_state{}) ->
    %% already frozen, change nothing
    {keep_state, StateData};

remote_armed_frozen_movement({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% stop current movement. if the new direction is not opposite to current - request new movement
    Opposite_direction = calc_opposite_direction(StateData#bomb_state.direction),
    UpdatedData = stop_active_movement(StateData),
    TempTime = calc_new_explode_delay(StateData),
    if 
        Direction == Opposite_direction ->
            %% opposite direction - just cancel current movement timer (if exists)
            {next_state, remote_armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}, {reply, GN, {none, false}}]};
        true ->
            %% any other direction - send request to move at new direction
            %% Only sends request if there's enough time
            TimeLeft = TempTime - erlang:system_time(millisecond),
            if
                TimeLeft > ?MIN_MOVE_REQ_TIME -> % enough time
                    gen_server:cast(StateData#bomb_state.gn_pid, {bomb_message, {move_request, self(), GN, Direction}}),
                    {next_state, remote_armed, UpdatedData#bomb_state{direction = Direction},
                        [{state_timeout, TempTime - erlang:system_time(millisecond), explode},
                        {reply, GN, {Direction, false}}]};
                true -> % not enough time - do not make request
                    {keep_state, UpdatedData, [{reply, GN, {none, false}}]}
            end
    end;

remote_armed_frozen_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, move to remote_armed state, stop movement timer, pass state_timeout along
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    TempTime = calc_new_explode_delay(StateData),
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    {next_state, remote_armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};

remote_armed_frozen_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = true
            },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

remote_armed_frozen_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = true},
            {keep_state, UpdatedData};
        denied -> % movement denied, stop manual timers, pass along left-over state_timeout
            case StateData#bomb_state.movement of
                {true, TimerRef} -> erlang:cancel_timer(TimerRef);
                false -> ok
            end,
            TempTime = calc_new_explode_delay(StateData),
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            {next_state, remote_armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_armed_frozen_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_armed_frozen_movement(cast, ignite, _StateData) ->
    %% already armed, ignore this
    {keep_state_and_data};

remote_armed_frozen_movement(_Type, _Message, StateData = #bomb_state{}) ->
    log_unexpected_message(remote_armed_frozen_movement, _Type, _Message),
    {keep_state, StateData}.
    %{stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}; %% * SHOULD BREAK POST-DEBUG

%remote_armed_frozen_movement(_Any, _Message, StateData = #bomb_state{}) ->
%    {keep_state, StateData}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
calc_new_explode_delay(State = #bomb_state{}) ->
    %% Was updated to be used globally - not only for frozen states
    {_, InitialIgnitionTime} = State#bomb_state.ignited,
    case State#bomb_state.status of
        frozen -> ?EXPLODE_DELAY + ?FREEZE_DELAY + InitialIgnitionTime;
        normal -> ?EXPLODE_DELAY + InitialIgnitionTime
    end.

new_position(Old_position, Direction) ->
    [X,Y] = Old_position,
    case Direction of
        up -> [X,Y+1];
        down -> [X,Y-1];
        left -> [X-1,Y];
        right -> [X+1,Y]
    end.


stop_active_movement(State = #bomb_state{}) ->
    %% stops movement when in active_movement state
    %% returns updated state
    case State#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    State#bomb_state{movement = false, direction = none}.


calc_opposite_direction(Direction) ->
    %% Returns the opposite direction to the one given
    case Direction of
        up -> down;
        down -> up;
        right -> left;
        left -> right
    end.
    
%%%===================================================================
%% Functions existing for debugging

log_unexpected_message(StateName, MsgType, Message) ->
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io:format("[~2..0B:~2..0B:~2..0B]: Unexpected incoming message at state ~p response type ~p command: ~p~n",
        [Hour, Min, Sec, StateName, MsgType, Message]),
    error_logger:info_msg("Unexpected incoming message in state ~p: ~p", [StateName, Message]),
    ok.