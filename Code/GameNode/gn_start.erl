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
-export([start/0]).

%% ! NOTE: when terminating, need to use application:stop(mnesia)
%% ! NOTE: for super-massive debugging use sys:trace(Pid, true).


%% --------------------------------------------------------------
%%%                 GN initial startup process
%% --------------------------------------------------------------
start() ->
    io:format("🚀 Starting GN initial startup process...~n"),
    %% Register the process locally
    register(gn_start, self()),
    %% trap exits
    process_flag(trap_exit, true),
    %% Spawn the menu process
    Menu_Pid = spawn_link(menu, start, []),

    %% Enter receive loop
    IsBot = gn_receive_loop(Menu_Pid),
    NodeName = atom_to_list(node()),
    GNNumber = list_to_integer([lists:nth(3, NodeName)]),
    %% Left the loop after sending response to playmode (bot/human)
    %% Spawn gn_server and gn_graphics_server
    {ok, _Pid_gn_server} = gn_server:start_link({GNNumber, IsBot}),
    %% Get the CN node name from the global registry to pass to graphics server
    CNNodeName = case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) -> node(Pid);
        undefined -> 
            io:format("⚠️ Warning: CN node not found, crashing the system~n"),
            exit({cn_node_not_found, self()})
    end,
    {ok, _Pid_gn_graphics_server} = gn_graphics_server:start_link(CNNodeName).

gn_receive_loop(Menu_Pid) ->
    receive
        %% Handle messages coming from Menu_Pid
        {Menu_Pid, Request} -> 
            handle_menu_request(Request, Menu_Pid);
        %% Handle messages from cn_start
        {cn_start, Request} ->
            handle_cn_start_request(Request, Menu_Pid),
            gn_receive_loop(Menu_Pid);
        %% Handle EXIT messages from crashed processes (like menu timeout)
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited with reason: ~p~n", [Pid, Reason]),
            if 
                Pid == Menu_Pid, Reason == badsig ->
                    io:format("Menu process crashed due to timeout, defaulting to bot mode~n"),
                    % Send bot decision to CN and exit loop
                    case global:whereis_name(cn_start) of
                        CNPid when is_pid(CNPid) ->
                            io:format("Found CN process, sending timeout bot decision~n"),
                            CNPid ! {self(), playmode, true};
                        undefined ->
                            io:format("CN process not found during timeout handling~n")
                    end,
                    true;  % exit the loop to continue startup
                true ->
                    gn_receive_loop(Menu_Pid)
            end;
        %% ignore other messages and log them to console
        Unknown ->
            io:format("GN_start received unknown message: ~p~n", [Unknown]),
            gn_receive_loop(Menu_Pid)
    end.


handle_menu_request({play_clicked}, Menu_Pid) ->
    io:format("GN_start received play_clicked request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending connect request~n"),
            Pid ! {self(), node(), connect_request};
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    gn_receive_loop(Menu_Pid);
handle_menu_request({exit_clicked}, _Menu_Pid) ->
    io:format("GN_start received exit_clicked request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending disconnect request~n"),
            Pid ! {self(), node(), disconnect_request};
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    gn_receive_loop(_Menu_Pid);
handle_menu_request({play_as_human}, _Menu_Pid) ->
    io:format("GN_start received play_as_human request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending playmode request~n"),
            Pid ! {self(), playmode, false};
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    false; % leave the loop
handle_menu_request({play_as_bot}, _Menu_Pid) ->
    io:format("GN_start received play_as_bot request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending playmode request~n"),
            Pid ! {self(), playmode, true};
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    false; % leave the loop (same as play_as_human)
handle_menu_request(Unknown, Menu_Pid) ->
    io:format("GN_start received unknown menu request: ~p~n", [Unknown]),
    gn_receive_loop(Menu_Pid).


handle_cn_start_request({connected_count, Count}, Menu_Pid) ->
    io:format("GN_start received connected_count update: ~p~n", [Count]),
    %% Forward the update to the menu process
    Menu_Pid ! {cn_start, connection_count, Count};
handle_cn_start_request({choose_playmode, are_you_bot}, Menu_Pid) ->
    io:format("GN_start received choose_playmode request~n"),
    %% Forward the request to the menu process
    Menu_Pid ! {cn_start, choose_playmode, are_you_bot}.

handle_menu_request({play_as_human}, _Menu_Pid) ->
    io:format("GN_start received play_as_human request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending playmode request (human)~n"),
            Pid ! {self(), playmode, false},
            io:format("Playmode message sent successfully~n");
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    false; % leave the loop

handle_menu_request({play_as_bot}, _Menu_Pid) ->
    io:format("GN_start received play_as_bot request~n"),
    %% Forward the request to the cn_server using global name
    case global:whereis_name(cn_start) of
        Pid when is_pid(Pid) ->
            io:format("Found CN process, sending playmode request (bot)~n"),
            Pid ! {self(), playmode, true},
            io:format("Playmode message sent successfully~n");
        undefined ->
            io:format("CN process not found in global registry. Connected nodes: ~p~n", [nodes()])
    end,
    true. % leave the loop (return bot flag)
