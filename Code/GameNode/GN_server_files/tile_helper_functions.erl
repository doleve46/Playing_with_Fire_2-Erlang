%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2025 17:28
%%%-------------------------------------------------------------------

-module(tile_helper_functions).
-author("dolev").

%% * it might also include a bomb requesting movement later on
-export([break_tile/2, update_to_one_hit/2, create_item/3, store_powerup_in_mnesia/4]).


-import(gn_server, [get_registered_name/1]).


%%% ===========================================================================
%% ? Imports for windows:
-include("../../common_parameters.hrl").
-include("../mnesia_records.hrl").
-include("../../Objects/object_records.hrl"). %% windows fix

%% ? imports for linux:
%-include_lib("src/clean-repo/Code/Objects/object_records.hrl"). %% This should work for compiling under rebar3.
%-include_lib("src/clean-repo/Code/GameNode/mnesia_records.hrl").
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%%% ===========================================================================
break_tile(Position, TableName) ->
    Fun = fun() ->
        case mnesia:read(TableName, Position, sticky_write) of
            [Tile = #mnesia_tiles{}] ->
                % Check if tile contains an item and create it if needed
                case Tile#mnesia_tiles.contains of
                    undefined -> ok;
                    empty -> ok;
                    Item when Item =/= undefined, Item =/= empty ->
                        % Create item at the tile's position
                        erlang:send_after(1000, self(), {create_item, Position, Item})
                        %TODO: this should send a self-message with a 1second delay or so to the GN server to spawn this item
                end,
                io:format("Tile at position ~p is being broken. Type: ~p~n", [Position, Tile#mnesia_tiles.type]),
                ok = mnesia:delete(TableName, Position, write);
            [] -> 
                io:format("ERROR: break_tile: No tile found at position ~p in table ~p~n", [Position, TableName]),
                not_found
        end
    end,
    ReturnVal = case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end,
    ReturnVal.


update_to_one_hit(Position, TableName) ->
    Fun = fun() ->
        case mnesia:read(TableName, Position, sticky_write) of
            [Tile = #mnesia_tiles{}] ->
                Updated_Tile = Tile#mnesia_tiles{type = one_hit},
                mnesia:write(TableName, Updated_Tile, write),
                ok;
            [] -> not_found
        end
    end,
    ReturnVal = case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end,
    ReturnVal.

create_item([X,Y] = Coord, ItemType, PowerupTable) ->
    %% Create the powerup process, store it in the mnesia table
    case powerup:start_link(X, Y, ItemType) of
        {ok, Pid} ->
            io:format("** Created item of type ~p at ~p with PID ~p~n", [ItemType, Coord, Pid]),
            store_powerup_in_mnesia(Coord, ItemType, PowerupTable, Pid),
            {ok, Pid};
        {error, Reason} ->
            io:format("** ERROR: Failed to create item of type ~p at ~p. Reason: ~p~n", [ItemType, Coord, Reason]),
            {error, Reason}
    end.


store_powerup_in_mnesia(Coord, ItemType, PowerupTable, Pid) ->
    Powerup_Record = #mnesia_powerups{
        position = Coord,
        type = ItemType,
        gn_pid = self(),
        pid = Pid
    },
    Fun = fun() ->
        case mnesia:write(PowerupTable, Powerup_Record, write) of
            ok ->
                io:format("** Stored powerup of type ~p at ~p in Mnesia table ~p~n", [ItemType, Coord, PowerupTable]),
                ok;
            {error, Reason} ->
                io:format("** ERROR: Failed to store powerup of type ~p at ~p in Mnesia table ~p. Reason: ~p~n", [ItemType, Coord, PowerupTable, Reason]),
                {error, Reason};
            Anythingelse ->
                io:format("** ERROR: Unexpected result when storing powerup of type ~p at coordinates~p in Mnesia table ~p. Return value: ~p~n", [ItemType, Coord, PowerupTable, Anythingelse]),
                {error, unexpected_result}
        end
    end,
    case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end.