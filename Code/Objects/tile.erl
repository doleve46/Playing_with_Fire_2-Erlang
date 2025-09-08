%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2025 14:13
%%%-------------------------------------------------------------------
-module(tile).
-author("dolev").
-behaviour(gen_server).

%% API
-export([start_link/4, damage_taken/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

%-define(SERVER, ?MODULE).
%% linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%% Windows compatible
-include("../common_parameters.hrl").

-include("object_records.hrl").


%%%===================================================================
%%% API
%%%===================================================================


%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Pos_x::integer, Pos_y::integer,
    Type:: 'unbreakable'|'breakable'|?STRONG|'one_hit',
    Contains::any() ) -> % todo: update later when finalized
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Pos_x, Pos_y, Type, Contains) ->
    Server_name = list_to_atom("tile_" ++ integer_to_list(Pos_x) ++ "_" ++ integer_to_list(Pos_y)),
    % registers *locally* as atom called 'tile_X_Y' (X,Y - numbers indicating location)
    % ? maybe there's no need to register, but just hold at the GN a database of the position, type and Pid of tiles
    gen_server:start_link({global, Server_name}, ?MODULE, [[Pos_x, Pos_y], Type, Contains], []).

damage_taken(TilePid) ->
    gen_server:cast(TilePid, inflict_damage).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server

-spec(init(Args :: term()) ->
    {ok, State :: #tile_state{}} | {ok, State :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init([Position, Type, Contains]) ->
    State = #tile_state{position=Position, type=Type, contains=Contains},
    erlang:send_after(0, self(), hibernate),
    {ok, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #tile_state{}) ->
    {reply, Reply :: term(), NewState :: #tile_state{}} |
    {reply, Reply :: term(), NewState :: #tile_state{}, timeout() | hibernate} |
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #tile_state{}} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

handle_call(_Request, _From, State = #tile_state{}) ->
    %% no handle call is impelemented, default setting
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages - async. messaging
-spec(handle_cast(Request :: term(), State :: #tile_state{}) ->
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

handle_cast(inflict_damage, State = #tile_state{}) ->
    %% React to being hit by an explosion based on tile type
    io:format("🔥 TILE DEBUG: Tile at ~p (type: ~p) taking damage~n", [State#tile_state.position, State#tile_state.type]),
    case State#tile_state.type of
        unbreakable -> % damage to unbreakable tile does nothing
            io:format("🔥 TILE DEBUG: Unbreakable tile - no damage~n"),
            {noreply, State};
        breakable -> % damage to breakable tile breaks it, handled under terminate/2
            io:format("🔥 TILE DEBUG: Breakable tile - will terminate and create powerup~n"),
            {stop, normal, State};
        ?STRONG -> % moves to 2nd phase of breaking, notify "rulling" GN
            io:format("🔥 TILE DEBUG: Strong tile - transitioning to one_hit~n"),
            New_State = State#tile_state{type = one_hit},
            notify_gn(New_State, one_hit),
            io:format("🔥 TILE DEBUG: Strong tile - notified GN, continuing as one_hit~n"),
            {noreply, New_State};
        one_hit -> % being hit again - breaks the tile
            io:format("🔥 TILE DEBUG: One-hit tile - will terminate and create powerup~n"),
            {stop, normal, State}
    end;

handle_cast(_Request, State = #tile_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #tile_state{}) ->
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

handle_info(Info, State = #tile_state{}) ->
    case Info of
        hibernate ->
            %% ? added hibernation support - maybe sends a message at the start to all tiles to sleep?
            {noreply, State, hibernate};
        _ ->
            {noreply, State}
    end.



%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #tile_state{}) -> term()).

terminate(normal, State = #tile_state{}) ->
    io:format("🔥 TILE DEBUG: Tile at ~p (type: ~p) terminating normally - will create powerup~n", [State#tile_state.position, State#tile_state.type]),
    notify_gn(State, tile_breaking);

terminate(Reason, State = #tile_state{}) -> 
    io:format("🔥 TILE DEBUG: Tile at ~p (type: ~p) terminating abnormally - reason: ~p~n", [State#tile_state.position, State#tile_state.type, Reason]),
    ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #tile_state{},
    Extra :: term()) ->
    {ok, NewState :: #tile_state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State = #tile_state{}, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
notify_gn(State = #tile_state{}, Message) ->
    %% Messages supported: tile_breaking, one_hit
    [X,Y] = State#tile_state.position,
    GN_name = req_player_move:get_managing_node_by_coord(X, Y),
    gn_server:cast_message(GN_name, {tile_update, Message, State#tile_state.position}),
    ok.