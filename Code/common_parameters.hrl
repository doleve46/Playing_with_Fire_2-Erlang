%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc Common parameters used that are used in several files.
%%% All reside here for consistency and ease of change
%%%
%%% @end
%%% Created : 11. Jul 2025 23:25
%%%-------------------------------------------------------------------
-author("dolev").

%% Timing related definitions
-define(EXPLODE_DELAY, 5000). % time for normal explosion to occur
-define(FREEZE_DELAY, 2000). % time added to the counter when a bomb is frozen
-define(TICK_DELAY, 50). % a small delay, used for very short bomb interactions and io handler polling intervals
-define(TILE_MOVE, 1200). % time to complete a tile movement in normal movespeed (=1)
-define(MS_REDUCTION, 200). % time reduces by any additional movespeed buff
-define(MIN_MOVE_REQ_TIME, 800). % minimal overhead time for requesting movement (should be higher than movement itself)

%% Player FSM related definitions
-define(REQUEST_COOLDOWN, 150). % cooldown between requests to GN (MAYBE TO CHANGE)
-define(IMMUNITY_TIME, 2000). % 2 seconds immunity after damage
-define(DISCONNECT_TIMEOUT, 60000). % 60 seconds until process kill
%% Bot-specific constants
-define(MIN_ACTION_DELAY, 300).  % Minimum delay between bot actions (ms)
-define(MAX_ACTION_DELAY, 800).  % Maximum delay between bot actions (ms)
-define(BOMB_PROBABILITY, 0.15). % Base probability of dropping bomb vs moving

%%% ================== Naming Conventions ==================
%% Tile type definitions
-define(FREE, free).
-define(BREAKABLE, breakable).
-define(UNBREAKABLE, unbreakable).
-define(STRONG, strong).
-define(PLAYER_START, player_start).

%% Naming convention for powerups
-define(NO_POWERUP, none).
-define(MOVE_SPEED, move_speed).
-define(REMOTE_IGNITION, remote_ignition).
-define(REPEAT_BOMBS, repeat_bombs).
-define(KICK_BOMB, kick_bomb).
-define(PHASED, phased).
-define(PLUS_BOMBS, plus_bombs).
-define(BIGGER_EXPLOSION, bigger_explosion).
-define(PLUS_LIFE, plus_life).
-define(FREEZE_BOMB, freeze_bomb).
-define(REGULAR_BOMB, normal_bomb).

%% Bomb type definitions
-define(NO_BOMB, none).
-define(NORMAL_BOMB, normal_bomb).
-define(REMOTE_BOMB, remote_bomb).
-define(REPEATING_BOMB, repeating_bomb).

%% Player ID definitions
-define(NO_PLAYER, none).
-define(PLAYER_1, player_1).
-define(PLAYER_2, player_2).
-define(PLAYER_3, player_3).
-define(PLAYER_4, player_4).
