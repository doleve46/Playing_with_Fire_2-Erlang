-module(menu).
-export([start/0, send/2]).

start() ->
    Port = open_port({spawn, "python3 PWF2_GN_GUI.py"}, [binary, exit_status]),
    send(Port, "show_main_menu"),
    loop(Port).

send(Port, Command) ->
    Port ! {self(), {command, list_to_binary(Command ++ "\n")}}.

loop(Port) ->
    receive
        {Port, {data, Data}} ->
            Message = binary_to_list(Data),
            io:format("GUI replied: ~s~n", [Message]),
            handle_gui_event(Port, string:trim(Message)),
            loop(Port);
        {Port, {exit_status, Status}} ->
            io:format("GUI exited with status ~p~n", [Status])
    end.

handle_gui_event(Port, "play_clicked") ->
    io:format("User clicked Play — waiting for GN connections...~n"),
    send(Port, "show_loading"),
    wait_for_gn_connections(Port);
    
handle_gui_event(Port, "exit_clicked") ->
    io:format("User clicked Exit — exiting~n"),
    port_close(Port),
    ok;

handle_gui_event(Port, "retry_clicked") ->
    io:format("Retrying — waiting for GN connections again~n"),
    send(Port, "show_loading"),
    wait_for_gn_connections(Port);

handle_gui_event(Port, "return_to_menu") ->
    send(Port, "show_main_menu");

handle_gui_event(Port, "play_game_clicked") ->
    io:format("User chose to play the game~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000), % Simulate game setup time
    start_game(Port);

handle_gui_event(Port, "bot_clicked") ->
    io:format("User chose bot mode~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000), % Simulate game setup time
    start_game(Port);

handle_gui_event(Port, "choice_timeout") ->
    io:format("Player choice timed out — defaulting to bot~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000),
    start_game(Port);

handle_gui_event(_, Unknown) ->
    io:format("Unhandled message: ~p~n", [Unknown]).

%% Wait for all GN connections (CN already started)
wait_for_gn_connections(Port) ->
    spawn(fun() ->
        try
            % Wait for all GN connections (CN is already running)
            % This will block until all 4 GNs are connected
            ok = cn_initial_startup:await_initial_connections(0, []),
            % All 4 GNs connected successfully
            io:format("All servers connected! Moving to player choice~n"),
            send(Port, "show_player_choice"),
            start_choice_timer(Port)
        catch
            Class:Reason:Stacktrace ->
                io:format("Error waiting for connections: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
                send(Port, "show_error")
        end
    end).

%% Start 60-second timer for player choice
start_choice_timer(Port) ->
    spawn(fun() ->
        timer:sleep(60000), % 60 seconds
        send(Port, "choice_timeout")
    end).

%% Simulate starting the actual game
start_game(Port) ->
    io:format("Starting game...~n"),
    send(Port, "start_game"),
    %% TO DO: Implement actual game logic here
    %% For now, we'll just exit the menu
    timer:sleep(2000),
    port_close(Port).
