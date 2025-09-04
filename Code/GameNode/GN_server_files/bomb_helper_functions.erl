-module(bomb_helper_functions).

-export([place_bomb/3, find_remote_bombs_for_player/1]).

-include("../../common_parameters.hrl").
-include("../mnesia_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @doc Places a bomb for a player at their current position
%% @param PlayerNum - The player number/ID placing the bomb
%% @param PlayersTableName - The name of the mnesia table containing player data
%% @returns bomb_placed | {error, Reason}
place_bomb(PlayerNum, PlayersTableName, BombsTableName) ->
    %% Look up player in mnesia table
    Fun = fun() ->
        case mnesia:read(PlayersTableName, PlayerNum, read) of
            [PlayerRecord] -> 
                {ok, PlayerRecord};
            [] -> {error, player_not_found};
            _Multiple -> {error, multiple_players_found}
        end
    end,
    {atomic, Result} = mnesia:transaction(Fun),

    case Result of
        {ok, PlayerRecord} ->
            %% Get player position and bomb settings
            [X,Y] = PlayerRecord#mnesia_players.position,
            BombType = get_bomb_type(PlayerRecord#mnesia_players.special_abilities),
            BombRadius = PlayerRecord#mnesia_players.explosion_radius,
            
            %% Create bomb process using FSM
            case bomb_as_fsm:start_monitor(X, Y, BombType, [PlayerRecord#mnesia_players.pid, BombRadius]) of
                {ok, {BombPid, _MonitorRef}} ->
                    %% Update player's active bomb within mnesia
                    TestReturnVal = update_player_bomb_count(PlayerNum, PlayersTableName, 1), %% TODO: figure out which of these is correct
                    io:format("DEBUG: update_player_bomb_count returned: ~p~n", [TestReturnVal]),
                    case TestReturnVal of
                        {atomic, _} -> ok;
                        ok -> ok;
                        Error1 -> 
                            io:format("ERROR: update_player_bomb_count failed: ~p~n", [Error1]),
                            {error, {failed_to_update_bomb_count, Error1}}
                    end,
                    %% Generate bomb record for table
                    BombRecord = #mnesia_bombs{
                        position = [X,Y],
                        type = BombType,
                        radius = BombRadius,
                        owner = PlayerRecord#mnesia_players.player_number,
                        gn_pid = PlayerRecord#mnesia_players.target_gn,
                        pid = BombPid},
                        TestReturnVal2 = add_bomb_to_table(BombRecord, BombsTableName),
                        io:format("DEBUG: add_bomb_to_table returned: ~p~n", [TestReturnVal2]),
                        case TestReturnVal2 of
                            {atomic, _} -> 
                                io:format("DEBUG: Bomb added to table successfully~n"),
                                bomb_placed;
                            ok -> 
                                io:format("DEBUG: Bomb added to table (got ok instead of {atomic, ok})~n"),
                                bomb_placed;
                            Error2 -> 
                                io:format("ERROR: add_bomb_to_table failed: ~p~n", [Error2]),
                                {error, {failed_to_add_bomb_to_table, Error2}}
                        end;
                Error ->
                    {error, {failed_to_create_bomb, Error}}
            end;

        {error, player_not_found} -> {error, player_not_found};
        {error, multiple_players_found} -> {error, multiple_players_found}
    end.

%% Helper function to determine bomb type based on player buffs
get_bomb_type(BuffList) ->
    case lists:member(?REMOTE_IGNITION, BuffList) of
        true -> ?REMOTE_IGNITION;
        false ->
            case lists:member(?REPEAT_BOMBS, BuffList) of
                true -> ?REPEAT_BOMBS;
                false -> ?NORMAL_BOMB
            end
    end.


%% Helper function to update player's active bomb count 
update_player_bomb_count(PlayerNum, PlayersTableName, Increment) ->
    Fun = fun() ->
        [PlayerRecord] = mnesia:read(PlayersTableName, PlayerNum, write),
        UpdatedRecord = PlayerRecord#mnesia_players{
            bombs_placed = PlayerRecord#mnesia_players.bombs_placed + Increment
        },
        mnesia:write(PlayersTableName, UpdatedRecord, write)
    end,
    mnesia:activity(transaction, Fun).

%% Helper function to add bomb to bombs mnesia table
add_bomb_to_table(Record, BombsTableName) ->
    Fun = fun() ->
        mnesia:write(BombsTableName, Record, write)
    end,
    mnesia:activity(transaction, Fun).

%% @doc Searches all 4 bomb tables for not-ignited remote bombs owned by PlayerNum
%% @returns List of remote bomb records owned by the player
find_remote_bombs_for_player(PlayerNum) ->
    BombTables = [gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs],
    Fun = fun() ->
        lists:foldl(
            fun(TableName, Acc) ->
                Query = qlc:q([
                    Bomb || Bomb <- mnesia:table(TableName),
                    Bomb#mnesia_bombs.owner =:= PlayerNum,
                    Bomb#mnesia_bombs.type =:= ?REMOTE_BOMB,
                    Bomb#mnesia_bombs.ignited =:= false
                ]),
                qlc:e(Query) ++ Acc
            end,
            [], % Initial accumulator
            BombTables)
    end,
    {atomic, RemoteBombs} = mnesia:activity(transaction, Fun),
    RemoteBombs.
