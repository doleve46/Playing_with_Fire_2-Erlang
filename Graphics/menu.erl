-module(menu).
-export([start/0, send/2, send_to_gn_start/1]).

start() ->
    % Get the current directory
    {ok, Cwd} = file:get_cwd(),
    GuiPath = filename:join([Cwd, "src", "Graphics", "PWF2_GN_GUI.py"]),
    Port = open_port({spawn, "python3 " ++ GuiPath}, [{cd, filename:dirname(GuiPath)}, binary, exit_status]),
    send(Port, "show_main_menu"),
    loop(Port, idle).

send(Port, Command) ->
    Port ! {self(), {command, list_to_binary(Command ++ "\n")}}.

send_to_gn_start(Request) ->
    gn_start ! {self(), Request}.

loop(Port, Status) ->
    receive
        {Port, {data, Data}} ->
            Message = binary_to_list(Data),
            io:format("GUI replied: ~s~n", [Message]),
            NewStatus = handle_gui_event(Port, string:trim(Message), Status),
            case NewStatus of
                terminate -> ok; % Process should end
                _ -> loop(Port, NewStatus) % Continue with new status
            end;

        {Port, {exit_status, StatusMsg}} ->
            io:format("GUI exited with status ~p~n", [StatusMsg]),
            send_to_gn_start({exit_clicked}); % simulating crash

        {cn_start, connection_count, Count} when Status==connecting ->
            io:format("Received connection count update: ~p~n", [Count]),
            send(Port, "update_connection_count:" ++ integer_to_list(Count)),
            loop(Port, connecting);

        {cn_start, choose_playmode, are_you_bot} ->
            io:format("All 4 GNs connected! Moving to player choice~n"),
            send(Port, "show_player_choice"),
            start_choice_timer(Port),
            loop(Port, player_choice)

    end.

handle_gui_event(Port, "play_clicked", _Status) ->
    io:format("User clicked Play — waiting for GN connections...~n"),
    send(Port, "show_waiting:0"),
    send_to_gn_start({play_clicked}),
    connecting;
    
handle_gui_event(Port, "exit_clicked", _Status) ->
    io:format("User clicked Exit — exiting~n"),
    send_to_gn_start({exit_clicked}),
    port_close(Port),
    terminate;

handle_gui_event(Port, "retry_clicked", _Status) ->
    io:format("Retrying — waiting for GN connections again~n"),
    send(Port, "show_waiting:0"),
    send_to_gn_start({play_clicked}),
    connecting;

handle_gui_event(Port, "return_to_menu", _Status) ->
    send(Port, "show_main_menu"),
    idle;

handle_gui_event(Port, "play_game_clicked", _Status) ->
    io:format("User chose to play the game~n"),
    send(Port, "show_game_setup"),
    send_to_gn_start({play_as_human}),
    % Close the menu GUI immediately after selection
    send(Port, "close_gui"),
    timer:sleep(500), % Brief delay for GUI to process close command
    %% Close port and process after playmode selection
    spawn(fun() ->
        timer:sleep(500),
        try 
            port_close(Port)
        catch 
            _:_ -> ok  % Ignore port close errors
        end,
        exit(normal)
    end),
    terminate;

handle_gui_event(Port, "bot_clicked", _Status) ->
    io:format("User chose bot mode~n"),
    send(Port, "show_game_setup"),
    send_to_gn_start({play_as_bot}),
    % Close the menu GUI immediately after selection
    send(Port, "close_gui"),
    timer:sleep(500), % Brief delay for GUI to process close command
    %% Close port and process after playmode selection
    spawn(fun() ->
        timer:sleep(500),
        try 
            port_close(Port)
        catch 
            _:_ -> ok  % Ignore port close errors
        end,
        exit(normal)
    end),
    terminate;

handle_gui_event(Port, "choice_timeout", _Status) ->
    io:format("Player choice timed out — defaulting to bot~n"),
    send(Port, "show_game_setup"),
    send_to_gn_start({play_as_bot}),
    timer:sleep(3000),
    %% Close port and process after timeout
    spawn(fun() ->
        timer:sleep(1000),
        try 
            port_close(Port)
        catch 
            _:_ -> ok  % Ignore port close errors
        end,
        exit(normal)
    end),
    terminate;

handle_gui_event(_, Unknown, Status) ->
    io:format("Unhandled message: ~p~n", [Unknown]),
    Status. % Return current status unchanged


%% Start 20-second timer for player choice
start_choice_timer(Port) ->
    spawn(fun() ->
        timer:sleep(20000), % 20 seconds
        send(Port, "choice_timeout")
    end).


