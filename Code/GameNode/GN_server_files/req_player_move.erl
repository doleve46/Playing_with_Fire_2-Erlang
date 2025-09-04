%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2025 08:42
%%%-------------------------------------------------------------------

-module(req_player_move).
-author("dolev").

%% ? Because the gn_server file became insanely cluttered, I'm splitting it into different files
%% ? based on functionality. This file will include all functions relevant to a player requesting movement
%% * it might also include a bomb requesting movement later on
-export([read_player_from_table/2, calc_new_coordinates/2,
        update_player_direction/3, handle_player_movement_clearance/3, handle_bomb_movement_clearance/3,
        get_managing_node_by_coord/2, node_name_to_number/1,
        get_records_at_location/2, interact_with_entity/4,
        handle_player_movement/3, insert_player_movement/2, insert_player_movement/3, check_for_obstacles/4,
        read_and_remove_bomb/2, get_gn_number_by_coord/2,
        read_and_update_coord/3, check_entered_coord/2, update_player_cooldowns/2]).

-import(gn_server, [get_registered_name/1]).


%%% ===========================================================================
%% ? Imports for windows:
-include("../../common_parameters.hrl").
-include("../mnesia_records.hrl").
-include("../../Objects/object_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% ? imports for linux:
%-include_lib("src/clean-repo/Code/Objects/object_records.hrl"). %% This should work for compiling under rebar3.
%-include_lib("src/clean-repo/Code/GameNode/mnesia_records.hrl").
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%%% ===========================================================================

%% @doc Updates coordinate after a movement timer expires.
%% If new coord. is within the same GN - update position, direction and movement
%%      then check for collisions in the new coordinate. (*not in this function)
%% else - Send message to player FSM to update his targetGN, update position, direction and movement in record, 
%%      and request CN to tranfer the player entry to the new GN's table, who at last asks the new GN to check for collisions (*not in this function)
read_and_update_coord(player, PlayerNum, Table) ->
    Current_gn_name = get_registered_name(self()),
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, write) of
            [Player_record = #mnesia_players{}] -> 
                [New_x, New_y] = calc_new_coordinates(Player_record#mnesia_players.position, Player_record#mnesia_players.direction),
                %% check if new coordinate fall within current managing GN
                case get_managing_node_by_coord(New_x,New_y) of
                    Current_gn_name -> % Player is not about to leave current GN's quarter
                    %% update position, reset direction and movement
                        Updated_record = Player_record#mnesia_players{
                            position = [New_x, New_y],
                            movement = false,
                            direction = none
                        },
                        mnesia:write(Table, Updated_record, write),
                        {retain_gn, Player_record}; %% return value to calling function
                    Other_name -> %% destination coordinate is managed by another GN (=Other_name)
                    %% update position, target_gn name, reset movement and direction
                    %% ask CN to transfer entry between tables
                        Updated_record = Player_record#mnesia_players{
                            position = [New_x, New_y],
                            target_gn = Other_name,
                            movement = false,
                            direction = none
                        },
                        mnesia:write(Table, Updated_record, write),
                        {switch_gn, Current_gn_name, Other_name} %% return value
                    end;
            [] -> not_found % should cause an error
        end
    end,
    mnesia:activity(transaction, Fun).


-spec read_player_from_table(PlayerNum::integer(), Table::atom()) -> #mnesia_players{} | not_found.
read_player_from_table(PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum) of
            [PlayerRecord = #mnesia_players{}] -> PlayerRecord;

            [] -> not_found % should cause an error
        end
    end,
    mnesia:activity(transaction, Fun).


-spec calc_new_coordinates(Position::list(), Direction::up|down|left|right) -> list().
calc_new_coordinates(Position, Direction) ->
    [X,Y] = Position,
    case Direction of
        up -> [X, Y+1];
        down -> [X, Y-1];
        left -> [X-1, Y];
        right -> [X+1,Y]
    end.
%% =====================================================================
%% ? solving the clusterfuck for managing the movement check

%% @doc returns can_move/cant_move/dest_not_here
handle_player_movement(PlayerNum, Direction, State = #gn_state{}) ->
    io:format("DEBUG: handle_player_movement called for Player ~p, Direction ~p~n", [PlayerNum, Direction]),
    %% ? calculate the destination coordinate, find out if the coordinate is within limits of current GN (the one initiating the function call)
    Player_record = read_player_from_table(PlayerNum, State#gn_state.players_table_name),
    io:format("DEBUG: Player record: ~p~n", [Player_record]),
    Destination = calc_new_coordinates(Player_record#mnesia_players.position, Direction),
    io:format("DEBUG: Calculated destination: ~p~n", [Destination]),
    Current_gn_name = get_registered_name(self()),
    io:format("DEBUG: Current GN name: ~p~n", [Current_gn_name]),
    
    ManagingNode = get_managing_node_by_coord(hd(Destination),lists:last(Destination)),
    io:format("DEBUG: Managing node for destination: ~p~n", [ManagingNode]),
    
    case ManagingNode of
        out_of_map_bounds -> % player tried to move outside the map
            io:format("DEBUG: Movement result: cant_move (out of bounds)~n"),
            cant_move;
        Current_gn_name -> % destination coordinate is managed by this GN
            io:format("DEBUG: Destination is within current GN, checking obstacles~n"),
            %% ? Checks for obstacles in the target coordinate, kickstarting any movements caused by this attempt
            Result = check_for_obstacles(Destination, Player_record#mnesia_players.special_abilities, Direction, State),
            io:format("DEBUG: check_for_obstacles result: ~p~n", [Result]),
            Result;
        _Other_name ->
            io:format("DEBUG: Movement result: dest_not_here (managed by ~p)~n", [ManagingNode]),
            dest_not_here
    end.


check_for_obstacles(Coordinate, BuffsList, Initiator_Direction, State = #gn_state{}) -> 
    %% ? Fetch every entity in that coordinate using QLC
    Entities_at_coord = get_records_at_location(Coordinate, State),
    %% ? Deal with possible interactions - returns can_move/cant_move
    interact_with_entity(Entities_at_coord, BuffsList, Initiator_Direction, State).


%% @doc Update movement to 'true', set time remaining based on movespeed, and set direction
%% time remaining = <Base_time> - (Player_speed - 1) * <MS_REDUCTION>
insert_player_movement(PlayerNum, Table, Direction) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, sticky_write) of
            [Player_record = #mnesia_players{}] ->
                Updated_record = Player_record#mnesia_players{
                    movement = true,
                    direction = Direction,
                    movement_timer = ?TILE_MOVE - (Player_record#mnesia_players.speed -1)*?MS_REDUCTION % * Set counter based on movespeed
                    },
                %% Insert updated record into the correct table (not inferred from record name)
                io:format("DEBUG: insert_player_movement - player num ~p movement timer is ~p, direction is ~p~n", [Updated_record#mnesia_players.player_number, Updated_record#mnesia_players.movement_timer, Direction]),
                mnesia:write(Table, Updated_record, sticky_write),
                ok;
            [] -> % didn't find 
                not_found
        end
    end,
    Result = mnesia:activity(transaction, Fun),
    io:format("DEBUG: insert_player_movement result: ~p~n", [Result]),
    Result.

%% @doc Legacy function for backward compatibility - update movement only
insert_player_movement(PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, sticky_write) of
            [Player_record = #mnesia_players{}] ->
                Updated_record = Player_record#mnesia_players{
                    movement = true,
                    movement_timer = ?TILE_MOVE - (Player_record#mnesia_players.speed -1)*?MS_REDUCTION % * Set counter based on movespeed
                    },
                %% Insert updated record into the correct table (not inferred from record name)
                io:format("DEBUG: insert_player_movement - player num ~p movement timer is ~p~n", [Updated_record#mnesia_players.player_number, Updated_record#mnesia_players.movement_timer]),
                mnesia:write(Table, Updated_record, sticky_write),
                ok;
            [] -> % didn't find 
                not_found
        end
    end,
    Result = mnesia:activity(transaction, Fun),
    io:format("DEBUG: insert_player_movement result: ~p~n", [Result]),
    Result.



get_records_at_location(Coordinate, State = #gn_state{}) ->
    Fun = fun() ->
        Tiles = qlc:eval(qlc:q([ {tile, T} || T <- mnesia:table(State#gn_state.tiles_table_name), T#mnesia_tiles.position == Coordinate])),
        Bombs = qlc:eval(qlc:q([ {bomb, B} || B <- mnesia:table(State#gn_state.bombs_table_name), B#mnesia_bombs.position == Coordinate])),
        Players = qlc:eval(qlc:q([ {player, P} || P <- mnesia:table(State#gn_state.players_table_name), P#mnesia_players.position == Coordinate])),
        Tiles ++ Bombs ++ Players
    end,
    mnesia:activity(sync_dirty, Fun).


-spec interact_with_entity(list(), list(), up|down|left|right, #gn_state{}) -> can_move|cant_move.
interact_with_entity(ListOfEntities, BuffsList, Direction, State) ->
    %% this is the function called upon. rest are the recursion
    interact_with_entity(ListOfEntities, BuffsList, Direction, State, can_move).

-spec interact_with_entity(list(), list(), up|down|left|right, State::#gn_state{}, MoveStatus:: can_move|cant_move) 
    -> can_move|cant_move.
interact_with_entity([], _BuffsList, _Direction, _State, MoveStatus) -> MoveStatus;
interact_with_entity([H|T], BuffsList, Direction, State, MoveStatus) ->
    case H of
        {tile, _Tile} ->
            %% no buffs help with running into a wall, movement request is denied
            interact_with_entity(T, BuffsList, Direction, State, cant_move);
        {bomb, Bomb} ->
            %% check if can kick bombs, freeze them or phased movement, act accordingly
            Relevant_buffs = [Buff || Buff <- BuffsList, lists:member(Buff, [?KICK_BOMB, ?PHASED, ?FREEZE_BOMB])],
            case Relevant_buffs of
                [] -> 
                    %% no special buffs, can't push bomb, movement is denied
                    interact_with_entity(T, BuffsList, Direction, State, cant_move);
                [?KICK_BOMB] ->
                    %% kick bomb special buff, tries to initiate a move for the bomb in the movement direction of the player
                    %% ? send message to bomb, prompting it to initiate appropriate movement based on it's own state
                    update_bomb_direction_movement(
                        Bomb, State#gn_state.bombs_table_name, bomb_as_fsm:kick_bomb(Bomb#mnesia_bombs.pid, Direction)
                    ),
                    interact_with_entity(T, BuffsList, Direction, State, cant_move);
                [?PHASED] ->
                    %% can move through bombs. does not cause the bomb to move, able to keep moving
                    interact_with_entity(T, BuffsList, Direction, State, MoveStatus);
                [?FREEZE_BOMB] ->
                    %% freezes the bomb, cannot move through it
                    %% let the bomb know
                    bomb_as_fsm:freeze_bomb(Bomb#mnesia_bombs.pid),
                    %% update the mnesia table 
                    update_bomb_status(Bomb, State#gn_state.bombs_table_name),
                    interact_with_entity(T, BuffsList, Direction, State, cant_move)
            end;
        {player, _Other_player} ->
            %% design decision: cannot move through other players - same interaction as with a tile
            %% ! need to check if this isn't the player who initiated this - or is it fine 
            interact_with_entity(T, BuffsList, Direction, State, cant_move)
    end.


update_bomb_status(Bomb, Bombs_table) ->
    %% re-read the bomb and update status to frozen on mnesia table
    BombKey = Bomb#mnesia_bombs.position,
    Fun = fun() ->
        [CurrentRecord] = mnesia:wread({Bombs_table, BombKey}),
        mnesia:write(Bombs_table, CurrentRecord#mnesia_bombs{status = frozen}, write)
    end,
    mnesia:activity(transaction, Fun).


update_bomb_direction_movement(Bomb, Bombs_table, ToUpdate) ->
    BombKey = Bomb#mnesia_bombs.position,
    %% Separate handling if only updating direction or direction&movement
    Fun = case ToUpdate of
        {DirectionVal, MovementVal} ->
            %% Update both direction and movement
            fun() ->
                [CurrentRecord] = mnesia:wread({Bombs_table, BombKey}),
                mnesia:write(Bombs_table, CurrentRecord#mnesia_bombs{direction = DirectionVal, movement = MovementVal}, write)
            end;
        DirectionVal ->
            %% Only direction needs to be updated
            fun() -> 
                [CurrentRecord] = mnesia:wread({Bombs_table, BombKey}),
                mnesia:write(Bombs_table, CurrentRecord#mnesia_bombs{direction = DirectionVal}, write)
            end
    end,
    mnesia:activity(transaction, Fun).

read_and_remove_bomb(BombPid, Bombs_table) ->
    Fun = fun() ->
        [Record] = mnesia:match_object(Bombs_table, #mnesia_bombs{pid = BombPid, _ = '_'}, read),
        mnesia:delete_object(Record),
        Record
    end,
    mnesia:activity(transaction, Fun).
%% =====================================================================

-spec get_managing_node_by_coord(X::integer(), Y::integer()) -> atom().
get_managing_node_by_coord(X,Y) when X > 0, X =< 7, Y > 7, Y < 15 -> 'GN1_server';
get_managing_node_by_coord(X,Y) when X > 7, X < 15 , Y > 7 , Y < 15 -> 'GN2_server';
get_managing_node_by_coord(X,Y) when X > 0 , X =< 7 , Y > 0 , Y =< 7 -> 'GN3_server';
get_managing_node_by_coord(X,Y) when X > 7 , X < 15 , Y > 0 , Y =< 7 -> 'GN4_server';
get_managing_node_by_coord(_X, _Y) -> out_of_map_bounds.


node_name_to_number(Name) ->
    list_to_integer([lists:nth(3, atom_to_list(Name))]).


-spec get_gn_number_by_coord(X:: integer(), Y::integer()) -> {Num::integer(), Tile_table_name::atom()}.
get_gn_number_by_coord(X,Y) when X>=0, X=<7, Y>7, Y=<15 -> {1, gn1_tiles};
get_gn_number_by_coord(X,Y) when X > 7, X =< 15 , Y > 7 , Y =< 15 -> {2, gn2_tiles};
get_gn_number_by_coord(X,Y) when X >= 0 , X =< 7 , Y >= 0 , Y =< 7 -> {3, gn3_tiles};
get_gn_number_by_coord(X,Y) when X > 7 , X =< 15 , Y >= 0 , Y =< 7 -> {4, gn4_tiles}.


-spec update_player_direction(PlayerNum::integer(), atom(), atom()) -> term().
update_player_direction(PlayerNum, Table, NewValue) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, sticky_write) of
            [Player_record = #mnesia_players{}] ->
                Updated_record = Player_record#mnesia_players{direction = NewValue},
                %% Insert updated record into table
                mnesia:write(Table, Updated_record, sticky_write),
                Updated_record;
            [] -> % didn't find 
                not_found
        end
    end,
    mnesia:activity(transaction, Fun).


%% @doc handles the operations needed to be done after given a reply for a movement clearance request to another GN for a player
handle_player_movement_clearance(PlayerNum, Answer, Table_name) ->
    %% ? For debugging purposes ONLY, we are letting the player FSM know of approved move requests. Later on should be only if they are denied
    %% both options (can_move/cant_move) send a reply to the player FSM based on his location - this is done first,
    %% Then the database update occurs (different for both)
    Player_record = read_player_from_table(PlayerNum, Table_name),
    %% respond to the player FSM
    if
        Player_record#mnesia_players.target_gn == Player_record#mnesia_players.local_gn ->
            %% Player FSM is on this node, send message directly
            player_fsm:gn_response(PlayerNum, {move_result, Answer}); 
        true ->
            %% Player FSM is on another machine, forward through CN->local GN
            gn_server:cast_message(cn_server,
                {forward_request, Player_record#mnesia_players.local_gn,
                    {gn_answer, {move_result, player, PlayerNum, Answer}}
                })
    end,
    case Answer of
        can_move ->
            %% move is possible. Update data, open movement timer
            insert_player_movement(PlayerNum, Table_name);
        cant_move -> % cannot move to the other node
            %% Update direction to none
            case erlang:is_record(update_player_direction(PlayerNum, Table_name, 'none'), mnesia_players) of
                true -> ok;
                _ -> 
                    %% couldn't find the record, crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end
    end.


handle_bomb_movement_clearance(_BombNum, Answer, _Table_name) -> % todo: implement bomb movement clearance
    %% ! BombNum and Table_name are "unused" for now to remove warnings until I finish this function
    case Answer of
        can_move->
            placeholder;
        cant_move ->
            placeholder
    end.

-spec check_entered_coord(#mnesia_players{}, State::#gn_state{}) -> ok.
check_entered_coord(Player_record, State) ->
    %% Check for powerups in new position, if any are found - add their effect to the player's mnesia table entry
    %% When a powerup is taken it is sent a a 'pickup(Pid)' command to stop & terminate it.
    %% This powerup entry is removed from the mnesia table - triggered by the termination msg from the powerup process.
    %% Player FSM is notified of the following powerup changes: max bombs, speed, lives
    Fun = fun() ->
        case mnesia:read(State#gn_state.powerups_table_name, Player_record#mnesia_players.position, write) of
            %% reads any powerups in the position of the player
            [] -> ?NO_POWERUP;
            [?NO_POWERUP] -> ?NO_POWERUP;
            [Found_powerup] -> % a powerup is present at the new position of the player
                %% remove current powerup from table, send msg to process to terminate
                mnesia:delete(State#gn_state.powerups_table_name, Player_record#mnesia_players.position, write), % remove powerup from table
                powerup:pickup(Found_powerup#mnesia_powerups.pid), % send msg to terminate process
                Found_powerup#mnesia_powerups.type % returns the powerup (from the transaction)
        end
        end,
        Powerup = mnesia:activity(transaction, Fun),
        if
            Powerup == ?NO_POWERUP -> ok; % no powerup found in position
            true -> % consume power-up into player, notify player for selected powerups
                consume_powerup(Powerup, Player_record, State#gn_state.players_table_name)
        end.



consume_powerup(Powerup, Player_record, Players_table) ->
    %% @doc Based on current player's powerups, change/update his power in the mnesia table.
    Updated_record = case Powerup of
        %% ---- Powerups that the player FSM should be notified about ----
        ?MOVE_SPEED -> % movespeed buff
            player_fsm:notify_power_up(Player_record#mnesia_players.pid, {movespeed, Player_record#mnesia_players.speed + 1}),
            Player_record#mnesia_players{speed = Player_record#mnesia_players.speed + 1};
        ?PLUS_LIFE -> % extra life
            player_fsm:notify_power_up(Player_record#mnesia_players.pid, {life, Player_record#mnesia_players.life + 1}),
            Player_record#mnesia_players{life = Player_record#mnesia_players.life + 1};
        ?PLUS_BOMBS -> % increase max bombs
            player_fsm:notify_power_up(Player_record#mnesia_players.pid, {bombs, Player_record#mnesia_players.bombs + 1}),
            Player_record#mnesia_players{bombs = Player_record#mnesia_players.bombs + 1};

        %% ---- General powerups - no conflict when consuming them ----
        ?BIGGER_EXPLOSION -> % increase explosion radius
            Player_record#mnesia_players{explosion_radius = Player_record#mnesia_players.explosion_radius + 1};

        %% Bomb interaction - replaces conflicting buffs present in the player
        ?KICK_BOMB -> % kicking a bomb upon colliding with them
            Updated_buffs = remove_other_buffs([?KICK_BOMB, ?FREEZE_BOMB, ?PHASED], Player_record#mnesia_players.special_abilities, ?KICK_BOMB),
            Player_record#mnesia_players{special_abilities = Updated_buffs};
        ?FREEZE_BOMB -> % freezing a bomb upon colliding with them
            Updated_buffs = remove_other_buffs([?KICK_BOMB, ?FREEZE_BOMB, ?PHASED], Player_record#mnesia_players.special_abilities, ?FREEZE_BOMB),
            Player_record#mnesia_players{special_abilities = Updated_buffs};
        ?PHASED -> % passing through a bomb upon colliding with them
            Updated_buffs = remove_other_buffs([?KICK_BOMB, ?FREEZE_BOMB, ?PHASED], Player_record#mnesia_players.special_abilities, ?PHASED),
            Player_record#mnesia_players{special_abilities = Updated_buffs};
        %% Bomb type - replaces other types with new one
        ?REMOTE_IGNITION -> % remote bombs
            Updated_buffs = remove_other_buffs([?REMOTE_IGNITION, ?REPEAT_BOMBS, ?REGULAR_BOMB], Player_record#mnesia_players.special_abilities, ?REMOTE_IGNITION),
            Player_record#mnesia_players{special_abilities = Updated_buffs};
        ?REPEAT_BOMBS -> % repeating bombs
            Updated_buffs = remove_other_buffs([?REMOTE_IGNITION, ?REPEAT_BOMBS, ?REGULAR_BOMB], Player_record#mnesia_players.special_abilities, ?REPEAT_BOMBS),
            Player_record#mnesia_players{special_abilities = Updated_buffs}

    end,
    Fun = fun() ->
        mnesia:write(Players_table, Updated_record, write) end,
    mnesia:activity(transaction, Fun).



remove_other_buffs(ToRemove, BuffList, NewBuff) ->
    [NewBuff | lists:filter(fun(Buff) -> not lists:member(Buff, ToRemove) end, BuffList)].
    


-spec update_player_cooldowns(Message::tuple(), Players_table::atom()) -> ok.
update_player_cooldowns(Message, Players_table) ->
    %% Unpack message content, update accordingly
    {WhatToUpdate, PlayerNumber, NewValue} = Message,
    %% * Update the player's cooldowns in the mnesia table.
    Fun = fun() ->
        case mnesia:read(Players_table, PlayerNumber) of
            [PlayerRecord = #mnesia_players{}] ->
                %% Update the relevant stat
                UpdatedRecord = case WhatToUpdate of
                    movement_cooldown_update ->
                        PlayerRecord#mnesia_players{movement_timer = NewValue};
                    request_cooldown_update ->
                        PlayerRecord#mnesia_players{request_timer = NewValue};
                    immunity_update ->
                        PlayerRecord#mnesia_players{immunity_timer = NewValue}
                end,
                mnesia:write(Players_table, UpdatedRecord, write);
            [] -> not_found % should never happen
        end
    end,
    mnesia:activity(transaction, Fun),
    if
        NewValue == 0, WhatToUpdate == movement_cooldown_update ->
            %% A movement just finished. Trigger event for updating coordinate and all that entails
            self() ! {update_coord, player, PlayerNumber};
        true ->
            ok
    end,
    ok.
