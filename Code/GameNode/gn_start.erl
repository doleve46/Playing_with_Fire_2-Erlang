%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% New file holding all the CN startup logic and functionality
%%% @end
%%% Created : 25. Aug 2025 15:24
%%%-------------------------------------------------------------------
-module(gn_start).
-author("dolev").

%% API
-export([start/1]).

%% ! NOTE: when terminating, need to use application:stop(mnesia)
%% ! NOTE: for super-massive debugging use sys:trace(Pid, true).


%% --------------------------------------------------------------
%%%                 GN initial startup process
%% --------------------------------------------------------------
start() ->
    io:format("🚀 Starting GN initial startup process...~n"),
    %% Register the process locally
    register(gn_start),
    %% trap exits
    process_flag(trap_exit, true),
    %% Spawn the menu process
    Menu_Pid = spawn_link(menu, start, []),

    %% Enter receive loop
    IsBot = gn_receive_loop(Menu_Pid),
    NodeName = atom_to_list(node()),
    GNNumber = list_to_integer([lists:nth(3, atom_to_list(Name))])
    %% Left the loop after sending response to playmode (bot/human)
    %% Spawn gn_server and gn_server_graphics
    {ok, _Pid_gn_server} = gn_server:start_link({GNNumber, IsBot}),
    {ok, _Pid_gn_server_graphics} = gn_server_graphics:start_link(NodeName).

gn_receive_loop(Menu_Pid) ->
    receive
        %% Handle messages coming from Menu_Pid
        {Menu_Pid, Request} -> 
            handle_menu_request(Request, Menu_Pid);
        %% Handle messages from cn_start
        {cn_start, Request} ->
            handle_cn_start_request(Request, Menu_Pid),
            gn_receive_loop(Menu_Pid);
        %% ignore other messages and log them to console
        Unknown ->
            io:format("GN_start received unknown message: ~p~n", [Unknown]),
            gn_receive_loop(Menu_Pid)
    end.


handle_menu_request({play_clicked}, Menu_Pid) ->
    io:format("GN_start received play_clicked request~n"),
    %% Forward the request to the cn_server
    {cn_start} ! {self(), node(), connect_request},
    gn_receive_loop(Menu_Pid);
handle_menu_request({exit_clicked}, Menu_Pid) ->
    io:format("GN_start received exit_clicked request~n"),
    %% Forward the request to the cn_server
    {cn_start} ! {self(), node(), disconnect_request};
    gn_receive_loop(Menu_Pid);
handle_menu_request({play_as_human}, Menu_Pid) ->
    io:format("GN_start received play_as_human request~n"),
    %% Forward the request to the cn_server
    {cn_start} ! {self(), playmode, false},
    false; % leave the loop
handle_menu_request({play_as_bot}, Menu_Pid) ->
    io:format("GN_start received play_as_bot request~n"),
    %% Forward the request to the cn_server
    {cn_start} ! {self(), playmode, true},
    true; % leave the loop
handle_menu_request(Unknown, Menu_Pid) ->
    io:format("GN_start received unknown menu request: ~p~n", [Unknown]),
    gn_receive_loop(Menu_Pid).


handle_cn_start_request({connected_count, Count}, Menu_Pid) ->
    io:format("GN_start received connected_count update: ~p~n", [Count]),
    %% Forward the update to the menu process
    Menu_Pid ! {cn_start, connected_count, Count};
handle_cn_start_request({choose_playmode, are_you_bot}, Menu_Pid) ->
    io:format("GN_start received choose_playmode request~n"),
    %% Forward the request to the menu process
    Menu_Pid ! {cn_start, choose_playmode, are_you_bot}.