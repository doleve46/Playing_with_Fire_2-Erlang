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
        ok = mnesia:delete(TableName, Position, write)
    end,
    mnesia:activity(transaction, Fun),
    ok.

update_to_one_hit(Position, TableName) ->
    Fun = fun() ->
        case mnesia:read(TableName, Position, sticky_write) of
            [Tile = #mnesia_tiles{}] ->
                Updated_Tile = Tile#mnesia_tiles{type = one_hit},
                mnesia:write(Updated_Tile),
                ok;
            [] -> not_found
        end
    end,
    {atomic, ReturnVal} = mnesia:activity(transaction, Fun),
    ReturnVal.