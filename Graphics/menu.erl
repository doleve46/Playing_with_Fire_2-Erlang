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
    start_game(Port, false); % false = human player

handle_gui_event(Port, "bot_clicked") ->
    io:format("User chose bot mode~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000), % Simulate game setup time
    start_game(Port, true); % true = bot mode

handle_gui_event(Port, "choice_timeout") ->
    io:format("Player choice timed out — defaulting to bot~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000),
    start_game(Port, true); % default to bot

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

%% Enhanced game startup with graphics servers
start_game(Port, IsBotMode) ->
    io:format("Starting game (Bot mode: ~p)...~n", [IsBotMode]),
    send(Port, "start_game"),
    
    %% Start graphics servers and close menu
    spawn(fun() ->
        timer:sleep(2000), % Give the user a moment to see the "starting" message
        
        %% Close the menu
        io:format("Closing menu and starting graphics servers...~n"),
        port_close(Port),
        
        %% Start graphics servers after menu closes
        start_graphics_servers(IsBotMode)
    end).

%% Start graphics system - CN graphics server will spawn GN graphics servers
start_graphics_servers(IsBotMode) ->
    io:format("🎨 Starting graphics subsystem...~n"),
    
    %% Get list of GN nodes - these should already be connected
    GNNodes = get_connected_gn_nodes(),
    
    if length(GNNodes) < 4 ->
        io:format("⚠️ Warning: Only ~w GN nodes connected, expected 4~n", [length(GNNodes)]);
    true ->
        io:format("✅ Found ~w GN nodes: ~p~n", [length(GNNodes), GNNodes])
    end,
    
    %% Start ONLY the CN graphics server - it will spawn the GN graphics servers
    case start_cn_graphics_server(GNNodes) of
        {ok, CNGraphicsPid} ->
            io:format("✅ CN Graphics Server started: ~p~n", [CNGraphicsPid]),
            io:format("   CN will now spawn GN graphics servers automatically~n"),
            
            %% Wait for CN graphics to fully initialize and spawn GN servers
            timer:sleep(2000),
            
            %% Force initial update to get graphics going
            gen_server:cast(cn_server_graphics, force_update),
            
            io:format("🎮 Game graphics system fully initialized!~n"),
            io:format("🖥️  Check for Python visualization windows~n");
        {error, Reason} ->
            io:format("❌ Failed to start CN Graphics Server: ~p~n", [Reason])
    end.

%% Get currently connected GN nodes
get_connected_gn_nodes() ->
    AllNodes = nodes(),
    lists:filter(fun(Node) ->
        NodeStr = atom_to_list(Node),
        string:prefix(NodeStr, "gn") =/= nomatch
    end, AllNodes).

%% Start the central graphics server
start_cn_graphics_server(GNNodes) ->
    try
        cn_server_graphics:start_link(GNNodes)
    catch
        Error:Reason ->
            io:format("❌ Exception starting CN graphics server: ~p:~p~n", [Error, Reason]),
            {error, {Error, Reason}}
    end.
