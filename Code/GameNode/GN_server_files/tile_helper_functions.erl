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
-export([break_tile/2, update_to_one_hit/2]).


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