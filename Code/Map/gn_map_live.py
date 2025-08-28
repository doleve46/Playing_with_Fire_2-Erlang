import pygame
import sys
import math
import random
import os
import time
import struct
import select
import json  # Add this import
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

# Initialize Pygame
pygame.init()

# Constants - Synced with common_parameters.hrl
TILE_SIZE = 40
MAP_SIZE = 16
PLAYER_PANEL_WIDTH = 250
POWERUP_PANEL_HEIGHT = 160
TIMER_PANEL_WIDTH = 180
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + TIMER_PANEL_WIDTH + 30
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 30
MIN_WINDOW_WIDTH = 1000
MIN_WINDOW_HEIGHT = 800
FPS = 60

# Backend Constants (from common_parameters.hrl)
TILE_MOVE_BASE = 1200
MS_REDUCTION = 200
IMMUNITY_TIME = 3000
REQUEST_COOLDOWN = 1000
TICK_DELAY = 50
EXPLOSION_DISPLAY_TIME = 1000
DEATH_DISPLAY_TIME = 10000

# Layout offsets
MAP_OFFSET_X = PLAYER_PANEL_WIDTH + 10
MAP_OFFSET_Y = 10
POWERUP_OFFSET_Y = MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE + 10
TIMER_OFFSET_X = MAP_OFFSET_X + MAP_SIZE * TILE_SIZE + 10

# Color Palette
COLORS = {
    'BACKGROUND': (25, 35, 45),
    'UI_BACKGROUND': (35, 45, 55),
    'PANEL_BG': (45, 55, 65),
    'PANEL_BORDER': (65, 75, 85),
    'FLOOR_LIGHT': (245, 235, 205),
    'FLOOR_MID': (230, 220, 190),
    'FLOOR_DARK': (215, 205, 175),
    'FLOOR_SHADOW': (200, 190, 160),
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GOLD': (255, 215, 0),
    'TEXT_SHADOW': (0, 0, 0),
    'TEXT_CYAN': (100, 255, 255),
    'TEXT_ORANGE': (255, 165, 0),
    'TEXT_GREY': (120, 120, 120),
    'TEXT_RED': (200, 50, 50),
    'TEXT_GREEN': (100, 255, 100),
    'TEXT_PURPLE': (200, 100, 255),
    'BRICK_TOP': (180, 90, 45),
    'BRICK_MID': (160, 80, 40),
    'BRICK_DARK': (140, 70, 35),
    'BRICK_SHADOW': (120, 60, 30),
    'MORTAR': (100, 50, 25),
    'WOOD_LIGHT': (200, 140, 90),
    'WOOD_MID': (180, 120, 70),
    'WOOD_DARK': (160, 100, 50),
    'WOOD_SHADOW': (140, 80, 30),
    'WOOD_HIGHLIGHT': (220, 160, 110),
    'WOOD_BAND': (100, 60, 30),
    'METAL_LIGHT': (160, 165, 170),
    'METAL_MID': (130, 135, 140),
    'METAL_DARK': (100, 105, 110),
    'METAL_SHADOW': (70, 75, 80),
    'METAL_SHINE': (200, 205, 210),
    'METAL_BAND': (60, 65, 70),
    'PLAYER_1': (80, 150, 255),
    'PLAYER_2': (255, 80, 100),
    'PLAYER_3': (80, 220, 120),
    'PLAYER_4': (255, 200, 80),
    'PLAYER_1_DEAD': (60, 80, 120),
    'PLAYER_2_DEAD': (120, 60, 70),
    'PLAYER_3_DEAD': (60, 100, 80),
    'PLAYER_4_DEAD': (120, 100, 60),
    'SKIN': (255, 220, 180),
    'SKIN_SHADOW': (230, 195, 155),
    'SKIN_DEAD': (150, 130, 110),
    'SKIN_SHADOW_DEAD': (130, 110, 90),
    'IMMUNITY_GLOW': (100, 255, 255),
    'STUN_COLOR': (255, 255, 100),
    'FREEZE_COLOR': (150, 200, 255),
    'SPEED_BOOST_COLOR': (255, 255, 100),
    'POWERUP_GLOW': (255, 255, 150),
    'POWERUP_CORE': (255, 215, 0),
    'POWERUP_PULSE': (255, 255, 100),
    'BOMB_BLACK': (40, 40, 40),
    'BOMB_FUSE': (255, 100, 0),
    'BOMB_FROZEN': (150, 200, 255),
    'EXPLOSION_CORE': (255, 255, 200),
    'EXPLOSION_MIDDLE': (255, 150, 50),
    'EXPLOSION_OUTER': (255, 50, 50),
    'EXPLOSION_SPARK': (255, 255, 255),
    'DEATH_RED': (150, 0, 0),
    'DEATH_DARK_RED': (100, 0, 0),
    'BLOOD_RED': (180, 20, 20),
    'DEATH_TEXT': (255, 50, 50),
    'SHADOW': (0, 0, 0, 60),
    'HIGHLIGHT': (255, 255, 255, 100),
    'SELECTION': (255, 255, 0, 150),
    'GRID_LINE': (0, 0, 0, 40),
    'TIMER_BAR_BG': (50, 50, 50),
    'TIMER_BAR_FILL': (100, 255, 100),
    'TIMER_BAR_DANGER': (255, 100, 100),
}

@dataclass
class PlayerTimers:
    movement_timer: int = 0
    immunity_timer: int = 0
    request_timer: int = 0

@dataclass
class PlayerState:
    player_id: int
    x: int
    y: int
    health: int
    speed: int
    direction: str
    movement: bool
    timers: PlayerTimers
    status: str = 'alive'

@dataclass
class BombState:
    x: int
    y: int
    bomb_type: str
    timer: int
    owner: int
    radius: int
    status: str
    ignited: bool
    movement: bool
    direction: str

@dataclass
class ExplosionState:
    x: int
    y: int
    explosion_type: str
    intensity: float
    remaining_time: float

class GameState:
    def __init__(self):
        self.tiles = [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        self.powerups = [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        self.players: Dict[int, PlayerState] = {}
        self.bombs: Dict[tuple, BombState] = {}
        self.explosions: List[ExplosionState] = []
        self.dead_players: Dict[int, dict] = {}
        self.backend_timing: Dict[str, int] = {}
        self.active_explosions: Dict[tuple, int] = {}

class CleanGNVisualizer:
    def __init__(self):
        # Window setup
        self.screen = pygame.display.set_mode((min(WINDOW_WIDTH, 1200), min(WINDOW_HEIGHT, 900)), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Clean GN Visualizer - Live Data Only")
        self.clock = pygame.time.Clock()

        # Current window dimensions and scaling
        self.current_width = self.screen.get_width()
        self.current_height = self.screen.get_height()
        self.scale_factor = min(self.current_width / WINDOW_WIDTH, self.current_height / WINDOW_HEIGHT)

        # Font system
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        self.mini_font = pygame.font.Font(None, 14)
        self.death_font = pygame.font.Font(None, 72)
        self.death_subtitle_font = pygame.font.Font(None, 36)

        # Animation and timing system
        self.time = 0.0
        self.powerup_pulse = 0.0
        self.camera_shake = 0.0

        # GN identification
        self.local_gn = self.determine_local_gn()
        self.local_gn_player_ids = self.get_local_player_ids()

        # Death tracking
        self.you_died_display = None
        self.death_screen_start_time = None

        # Backend constants
        self.backend_constants = {
            'tile_move': TILE_MOVE_BASE,
            'ms_reduction': MS_REDUCTION,
            'immunity_time': IMMUNITY_TIME,
            'request_cooldown': REQUEST_COOLDOWN,
            'tick_delay': TICK_DELAY,
            'explosion_display_time': EXPLOSION_DISPLAY_TIME
        }

        # Tile and powerup mappings
        self.tile_mapping = {
            'free': 0, 'breakable': 1, 'unbreakable': 2, 'strong': 3, 'player_start': 4
        }
        self.powerup_mapping = {
            'none': 'none', 'move_speed': 'move_speed', 'remote_ignition': 'remote_ignition',
            'repeat_bombs': 'repeat_bombs', 'kick_bomb': 'kick_bomb', 'phased': 'phased',
            'plus_bombs': 'plus_bombs', 'bigger_explosion': 'bigger_explosion',
            'plus_life': 'plus_life', 'freeze_bomb': 'freeze_bomb'
        }

        # Port communication - ONLY from Erlang port
        self.port_buffer = b''
        self.game_state = GameState()
        self.map_initialized = False

        # Animation systems
        self.player_animations: Dict[int, dict] = {}
        self.bomb_animations: Dict[tuple, dict] = {}
        self.explosion_animations: List[dict] = []
        self.game_effects: List[dict] = []
        self.status_effects: Dict[int, dict] = {}

        # Timer tracking
        self.movement_timers: Dict[int, int] = {}
        self.immunity_timers: Dict[int, int] = {}
        self.request_timers: Dict[int, int] = {}

        # Performance tracking
        self.fps_counter = 0
        self.last_fps_time = time.time()
        self.current_fps = 0

        # Rendering surfaces
        self.virtual_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))

        print(f"🎮 Clean GN Visualizer initialized (Local GN: {self.local_gn})")
        print(f"👥 Local players: {self.local_gn_player_ids}")
        print("⏳ Waiting for live data from GN graphics server...")

    def determine_local_gn(self):
        """Determine which GN this visualizer is running on"""
        gn_id = os.environ.get('GN_ID', 'gn1')
        print(f"🏠 Detected local GN: {gn_id}")
        return gn_id

    def get_local_player_ids(self):
        """Get the player IDs that belong to this GN"""
        gn_to_players = {
            'gn1': [1], 'gn2': [2], 'gn3': [3], 'gn4': [4]
        }
        return gn_to_players.get(self.local_gn, [])

    def read_port_data(self) -> Optional[bytes]:
        """Read data from Erlang port - NO FALLBACKS"""
        try:
            ready, _, _ = select.select([sys.stdin], [], [], 0)
            if not ready:
                return None

            data = sys.stdin.buffer.read()
            if not data:
                return None

            self.port_buffer += data

            # Process complete packets (4-byte length prefix + data)
            if len(self.port_buffer) >= 4:
                packet_length = struct.unpack('>I', self.port_buffer[:4])[0]
                if len(self.port_buffer) >= 4 + packet_length:
                    packet_data = self.port_buffer[4:4 + packet_length]
                    self.port_buffer = self.port_buffer[4 + packet_length:]
                    return packet_data

            return None
        except Exception as e:
            # This is normal when no data is available
            return None

    def parse_json_data(self, binary_data: bytes):
        """Parse JSON data from Erlang - CLEAN AND SIMPLE"""
        try:
            text = binary_data.decode('utf-8')
            return json.loads(text)
        except json.JSONDecodeError as e:
            print(f"❌ JSON decode error: {e}")
            print(f"📄 Data received: {binary_data[:200]}...")
            return None
        except UnicodeDecodeError as e:
            print(f"❌ UTF-8 decode error: {e}")
            return None
        except Exception as e:
            print(f"❌ Unexpected parsing error: {e}")
            return None

def handle_port_data(self, data: bytes):
    """Handle incoming JSON data from Erlang port"""
    parsed_data = self.parse_json_data(data)
    if parsed_data is None:
        return

    print(f"📨 Received JSON data: {type(parsed_data)}")

    # Handle JSON message format
    if isinstance(parsed_data, dict):
        message_type = parsed_data.get('type', 'unknown')
        message_data = parsed_data.get('data', {})
        timestamp = parsed_data.get('timestamp', 0)
        
        print(f"📩 JSON Message: {message_type} at {timestamp}")
        
        if message_type == 'map_update':
            self.process_map_update(message_data)
        elif message_type == 'movement_confirmation':
            self.handle_movement_confirmation(message_data)
        elif message_type == 'timer_update':
            self.handle_timer_update(message_data)
        elif message_type == 'fsm_update':
            self.handle_fsm_update(message_data)
        elif message_type == 'explosion_event':
            self.handle_explosion_event(message_data)
        else:
            print(f"❓ Unknown message type: {message_type}")
    else:
        print(f"❌ Expected JSON dict, got: {type(parsed_data)}")

    def process_map_update(self, update_data):
        """Process map update from GN graphics server"""
        try:
            if isinstance(update_data, dict):
                # Enhanced format with complete backend information
                if 'map' in update_data:
                    print(f"🗺️ Processing enhanced map update")
                    grid_data = update_data['map']
                    self.game_state.dead_players = update_data.get('dead_players', {})
                    self.game_state.backend_timing = update_data.get('backend_timing', {})
                    self.game_state.active_explosions = update_data.get('active_explosions', {})
                    
                    # Update backend constants
                    if self.game_state.backend_timing:
                        self.backend_constants.update(self.game_state.backend_timing)
                        print(f"⚙️ Updated backend constants: {self.game_state.backend_timing}")
                    
                    # Check for local player death
                    self.check_for_local_player_death()
                    
                    self.parse_game_state(grid_data)
                    self.map_initialized = True
                else:
                    # Just grid data
                    print(f"🗺️ Processing basic grid data")
                    self.parse_game_state(update_data)
                    self.map_initialized = True
            elif isinstance(update_data, list):
                # Direct grid format
                print(f"🗺️ Processing direct grid format")
                self.parse_game_state(update_data)
                self.map_initialized = True
            else:
                print(f"⚠️ Unexpected data format: {type(update_data)}")
        except Exception as e:
            print(f"❌ Error processing map update: {e}")
            import traceback
            traceback.print_exc()

    def check_for_local_player_death(self):
        """Check if any local players have died and trigger YOU DIED screen"""
        for player_id in self.local_gn_player_ids:
            if (player_id in self.game_state.dead_players and 
                self.you_died_display is None):
                
                print(f"💀 LOCAL PLAYER {player_id} DIED! Triggering death screen...")
                
                self.you_died_display = {
                    'player_id': player_id,
                    'start_time': self.time
                }
                self.death_screen_start_time = self.time
                self.camera_shake = 2.0
                break

    def parse_game_state(self, erlang_grid) -> bool:
        """Parse game state from Erlang grid"""
        if not erlang_grid or not isinstance(erlang_grid, list):
            print(f"⚠️ Invalid grid data: {type(erlang_grid)}")
            return False

        print(f"🔍 Parsing grid: {len(erlang_grid)} rows")

        # Reset current state
        new_players = {}
        new_bombs = {}
        new_explosions = []

        for row_idx in range(min(len(erlang_grid), MAP_SIZE)):
            for col_idx in range(min(len(erlang_grid[row_idx]), MAP_SIZE)):
                cell = erlang_grid[row_idx][col_idx]

                # Handle cell format (6-tuple with complete information)
                if len(cell) >= 4:
                    tile_type, powerup_type, bomb_info, player_info = cell[:4]
                    explosion_info = cell[4] if len(cell) > 4 else 'none'
                else:
                    continue

                # Transpose coordinates for display
                x, y = col_idx, row_idx

                # Update tiles and powerups
                if x < MAP_SIZE and y < MAP_SIZE:
                    self.game_state.tiles[x][y] = self.tile_mapping.get(tile_type, 0)
                    self.game_state.powerups[x][y] = self.powerup_mapping.get(powerup_type, 'none')

                # Parse player information
                if player_info != 'none':
                    player_data = self.parse_player_info(player_info, x, y)
                    if player_data:
                        new_players[player_data.player_id] = player_data

                # Parse bomb information
                if bomb_info != 'none':
                    bomb_data = self.parse_bomb_info(bomb_info, x, y)
                    if bomb_data:
                        new_bombs[(x, y)] = bomb_data

                # Parse explosion information
                if explosion_info != 'none':
                    explosion_data = self.parse_explosion_info(explosion_info, x, y)
                    if explosion_data:
                        new_explosions.append(explosion_data)

        # Update game state
        self.game_state.players = new_players
        self.game_state.bombs = new_bombs
        self.game_state.explosions = new_explosions

        print(f"✅ Game state parsed: {len(new_players)} players, {len(new_bombs)} bombs, {len(new_explosions)} explosions")
        return True

    def parse_player_info(self, player_info, x: int, y: int) -> Optional[PlayerState]:
        """Parse player information"""
        try:
            if isinstance(player_info, tuple) and len(player_info) >= 8:
                # Enhanced format: (player_id, life, speed, direction, movement, movement_timer, immunity_timer, request_timer)
                player_id = player_info[0]
                health = int(player_info[1]) if str(player_info[1]).isdigit() else 3
                speed = int(player_info[2]) if str(player_info[2]).isdigit() else 1
                direction = str(player_info[3])
                movement = bool(player_info[4])
                movement_timer = int(player_info[5]) if str(player_info[5]).isdigit() else 0
                immunity_timer = int(player_info[6]) if str(player_info[6]).isdigit() else 0
                request_timer = int(player_info[7]) if str(player_info[7]).isdigit() else 0

                if isinstance(player_id, str) and 'player_' in player_id:
                    player_num = int(player_id.split('_')[1])
                elif isinstance(player_id, int):
                    player_num = player_id
                elif str(player_id).isdigit():
                    player_num = int(player_id)
                else:
                    print(f"⚠️ Could not parse player_id: {player_id}")
                    return None
                
                # Update timer tracking
                self.movement_timers[player_num] = movement_timer
                self.immunity_timers[player_num] = immunity_timer
                self.request_timers[player_num] = request_timer
                
                return PlayerState(
                    player_id=player_num, x=x, y=y, health=health, speed=speed,
                    direction=direction, movement=movement,
                    timers=PlayerTimers(movement_timer, immunity_timer, request_timer),
                    status='alive'
                )
            elif isinstance(player_info, str) and 'player_' in player_info:
                player_num = int(player_info.split('_')[1])
                return PlayerState(
                    player_id=player_num, x=x, y=y, health=3, speed=1,
                    direction='north', movement=False,
                    timers=PlayerTimers(), status='alive'
                )
        except (ValueError, TypeError, IndexError) as e:
            print(f"⚠️ Error parsing player info: {e} - data: {player_info}")
        return None

    def parse_bomb_info(self, bomb_info, x: int, y: int) -> Optional[BombState]:
        """Parse bomb information"""
        try:
            if isinstance(bomb_info, tuple) and len(bomb_info) >= 7:
                # Enhanced format: (type, ignited, status, radius, owner, movement, direction)
                bomb_type = str(bomb_info[0])
                ignited = bool(bomb_info[1])
                status = str(bomb_info[2])
                radius = int(bomb_info[3]) if str(bomb_info[3]).isdigit() else 2
                owner = int(bomb_info[4]) if str(bomb_info[4]).isdigit() else 1
                movement = bool(bomb_info[5])
                direction = str(bomb_info[6])
                
                # Calculate timer based on FSM state
                timer = 3000  # Default 3 seconds
                if status == 'remote_idle':
                    timer = -1
                elif status == 'frozen':
                    timer = -2
                
                return BombState(
                    x=x, y=y, bomb_type=bomb_type, timer=timer, owner=owner,
                    radius=radius, status=status, ignited=ignited, movement=movement,
                    direction=direction
                )
            elif isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return BombState(
                    x=x, y=y, bomb_type='normal_bomb', timer=3000, owner=1,
                    radius=2, status='armed', ignited=False, movement=False,
                    direction='none'
                )
        except (ValueError, TypeError, IndexError) as e:
            print(f"⚠️ Error parsing bomb info: {e} - data: {bomb_info}")
        return None

    def parse_explosion_info(self, explosion_info, x: int, y: int) -> Optional[ExplosionState]:
        """Parse explosion information"""
        try:
            if isinstance(explosion_info, tuple) and len(explosion_info) >= 3:
                exp_type = str(explosion_info[0])
                intensity = float(explosion_info[1]) if str(explosion_info[1]).replace('.', '').isdigit() else 1.0
                remaining = float(explosion_info[2]) if str(explosion_info[2]).replace('.', '').isdigit() else 0.5
                
                return ExplosionState(
                    x=x, y=y, explosion_type=exp_type, intensity=intensity,
                    remaining_time=remaining
                )
            elif isinstance(explosion_info, str) and 'explosion' in explosion_info.lower():
                return ExplosionState(
                    x=x, y=y, explosion_type='blast_center', intensity=1.0,
                    remaining_time=0.5
                )
        except (ValueError, TypeError) as e:
            print(f"⚠️ Error parsing explosion info: {e} - data: {explosion_info}")
        return None

    def handle_movement_confirmation(self, confirmation_data: dict):
        """Handle movement confirmation from GN"""
        print(f"🏃 Movement confirmation: {confirmation_data}")
        
        if 'entity_type' in confirmation_data:
            entity_type = confirmation_data['entity_type']
            entity_data = confirmation_data.get('entity_data', {})
            
            if entity_type == 'player':
                player_id = entity_data.get('player_id', 0)
                print(f"👤 Player {player_id} movement confirmed")
            elif entity_type == 'bomb':
                bomb_pos = entity_data.get('from_pos', [0, 0])
                print(f"💣 Bomb at {bomb_pos} movement confirmed")

    def handle_timer_update(self, timer_data: dict):
        """Handle timer update from GN"""
        print(f"⏱️ Timer update: {timer_data}")
        
        if 'entity_type' in timer_data:
            entity_type = timer_data['entity_type']
            if entity_type == 'player':
                player_id = timer_data.get('player_id', 0)
                movement_timer = timer_data.get('movement_timer', 0)
                immunity_timer = timer_data.get('immunity_timer', 0)
                request_timer = timer_data.get('request_timer', 0)
                
                # Update timer tracking
                self.movement_timers[player_id] = movement_timer
                self.immunity_timers[player_id] = immunity_timer
                self.request_timers[player_id] = request_timer
                
                print(f"👤 Player {player_id} timers updated: M={movement_timer}, I={immunity_timer}, R={request_timer}")

    def handle_fsm_update(self, fsm_data: dict):
        """Handle FSM update from GN"""
        print(f"🎰 FSM update: {fsm_data}")

    def handle_explosion_event(self, explosion_data: dict):
        """Handle explosion event from GN"""
        print(f"💥 Explosion event: {explosion_data}")
        coordinates = explosion_data.get('coordinates', [])
        explosion_type = explosion_data.get('type', 'standard')
        
        # Create explosion animations
        for coord in coordinates:
            if len(coord) >= 2:
                self.explosion_animations.append({
                    'x': coord[0], 'y': coord[1],
                    'start_time': self.time,
                    'duration': 1.0,
                    'type': explosion_type
                })

    def update_animations(self):
        """Update all animations"""
        current_time = self.time
        
        # Update player animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            elapsed = current_time - anim['start_time']
            anim['progress'] = min(1.0, elapsed / anim['duration'])
            
            if anim['progress'] >= 1.0:
                del self.player_animations[player_id]

        # Update bomb animations
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
            elapsed = current_time - anim['start_time']
            
            if elapsed >= anim.get('duration', 30):
                del self.bomb_animations[pos]

        # Update explosion animations
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update game effects
        self.game_effects = [
            effect for effect in self.game_effects
            if current_time - effect['start_time'] < effect['duration']
        ]

        # Update camera shake
        if self.camera_shake > 0:
            self.camera_shake -= 2.0 / FPS
            if self.camera_shake < 0:
                self.camera_shake = 0

    def draw_you_died_screen(self):
        """Draw the YOU DIED screen overlay for local players"""
        if not self.you_died_display:
            return
            
        elapsed = self.time - self.death_screen_start_time
        
        # Death screen lasts for 8 seconds
        if elapsed > 8.0:
            self.you_died_display = None
            self.death_screen_start_time = None
            return
        
        # Create dark overlay
        overlay = pygame.Surface((self.current_width, self.current_height), pygame.SRCALPHA)
        
        # Fade in dark red overlay
        fade_progress = min(elapsed / 1.0, 1.0)
        overlay_alpha = int(180 * fade_progress)
        
        # Gradient from dark red to black
        for y in range(self.current_height):
            ratio = y / self.current_height if self.current_height > 0 else 0
            red_intensity = int((1 - ratio * 0.7) * 150 * fade_progress)
            color = (red_intensity, 0, 0, overlay_alpha)
            pygame.draw.line(overlay, color, (0, y), (self.current_width, y))
        
        self.screen.blit(overlay, (0, 0))
        
        # "YOU DIED" text
        if elapsed > 0.5:
            text_elapsed = elapsed - 0.5
            text_alpha = min(text_elapsed / 0.5, 1.0)
            text_scale = 0.5 + 0.5 * min(text_elapsed / 0.3, 1.0)
            
            # Pulsing effect
            pulse = 1.0 + 0.1 * math.sin(text_elapsed * 3)
            final_scale = text_scale * pulse
            
            # Create "YOU DIED" text
            death_text = "YOU DIED"
            base_size = int(72 * final_scale)
            
            # Glow layers
            for i in range(5, 0, -1):
                glow_font = pygame.font.Font(None, base_size + i * 6)
                glow_surface = glow_font.render(death_text, True, 
                    (int(255 * text_alpha * 0.6), 0, 0))
                glow_rect = glow_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 50))
                self.screen.blit(glow_surface, glow_rect)
            
            # Main text
            main_font = pygame.font.Font(None, base_size)
            main_surface = main_font.render(death_text, True, 
                (int(255 * text_alpha), int(50 * text_alpha), int(50 * text_alpha)))
            main_rect = main_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 50))
            self.screen.blit(main_surface, main_rect)
        
        # Subtitle text
        if elapsed > 1.5:
            subtitle_elapsed = elapsed - 1.5
            subtitle_alpha = min(subtitle_elapsed / 0.5, 1.0)
            
            player_id = self.you_died_display['player_id']
            subtitle_text = f"Player {player_id} has fallen"
            
            subtitle_surface = self.death_subtitle_font.render(subtitle_text, True, 
                (int(200 * subtitle_alpha), int(100 * subtitle_alpha), int(100 * subtitle_alpha)))
            subtitle_rect = subtitle_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 20))
            self.screen.blit(subtitle_surface, subtitle_rect)
        
        # Instructions to continue
        if elapsed > 5.0:
            instruction_elapsed = elapsed - 5.0
            instruction_alpha = min(instruction_elapsed / 0.5, 1.0)
            
            instruction_text = "Press SPACE to continue watching..."
            instruction_surface = self.small_font.render(instruction_text, True, 
                (int(150 * instruction_alpha), int(150 * instruction_alpha), int(150 * instruction_alpha)))
            instruction_rect = instruction_surface.get_rect(center=(self.current_width // 2, self.current_height - 80))
            self.screen.blit(instruction_surface, instruction_rect)

    def draw_gradient_rect(self, surface, color1, color2, rect, vertical=True):
        """Draw gradient rectangle"""
        if vertical:
            for y in range(rect.height):
                ratio = y / rect.height if rect.height > 0 else 0
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                               (rect.x, rect.y + y), (rect.x + rect.width, rect.y + y))
        else:
            for x in range(rect.width):
                ratio = x / rect.width if rect.width > 0 else 0
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                               (rect.x + x, rect.y), (rect.x + x, rect.y + rect.height))

    def draw_map(self):
        """Draw the game map"""
        # Apply camera shake
        shake_x = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Draw tiles
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = x * TILE_SIZE + shake_y

                tile_type = self.game_state.tiles[x][y]
                powerup = self.game_state.powerups[x][y]
                has_powerup = powerup != "none"

                # Draw floor
                if tile_type != 2:
                    self.draw_floor(self.map_surface, pixel_x, pixel_y)

                # Draw objects
                if tile_type == 1:  # BREAKABLE
                    self.draw_wooden_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 2:  # UNBREAKABLE
                    self.draw_brick_wall(self.map_surface, pixel_x, pixel_y)
                elif tile_type == 3:  # STRONG
                    self.draw_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)

        # Draw bombs
        for pos, bomb in self.game_state.bombs.items():
            pixel_x = bomb.y * TILE_SIZE + shake_x
            pixel_y = bomb.x * TILE_SIZE + shake_y
            self.draw_bomb(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw players
        for player_id, player in self.game_state.players.items():
            pixel_x = player.y * TILE_SIZE + shake_x
            pixel_y = player.x * TILE_SIZE + shake_y
            is_local = player_id in self.local_gn_player_ids
            self.draw_player(self.map_surface, pixel_x, pixel_y, player, is_local)

        # Draw explosions
        for explosion in self.game_state.explosions:
            pixel_x = explosion.y * TILE_SIZE + shake_x
            pixel_y = explosion.x * TILE_SIZE + shake_y
            self.draw_explosion(self.map_surface, pixel_x, pixel_y, explosion)

        # Draw animated explosions
        for explosion_anim in self.explosion_animations:
            pixel_x = explosion_anim['y'] * TILE_SIZE + shake_x
            pixel_y = explosion_anim['x'] * TILE_SIZE + shake_y
            self.draw_animated_explosion(self.map_surface, pixel_x, pixel_y, explosion_anim)

        # Blit map to virtual surface
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    def draw_floor(self, surface, x, y):
        """Draw floor tile"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['FLOOR_LIGHT'], COLORS['FLOOR_DARK'], rect)
        pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], rect, 1)

    def draw_brick_wall(self, surface, x, y):
        """Draw brick wall"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['BRICK_TOP'], COLORS['BRICK_DARK'], rect)
        
        # Brick pattern
        brick_height = TILE_SIZE // 5
        for row in range(5):
            brick_y = y + row * brick_height
            pygame.draw.line(surface, COLORS['MORTAR'], (x, brick_y), (x + TILE_SIZE, brick_y), 2)

    def draw_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Draw wooden barrel"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        # Barrel body
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.3 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 8) * curve_factor)
            
            ratio = i / TILE_SIZE if TILE_SIZE > 0 else 0
            r = int(COLORS['WOOD_LIGHT'][0] * (1 - ratio) + COLORS['WOOD_DARK'][0] * ratio)
            g = int(COLORS['WOOD_LIGHT'][1] * (1 - ratio) + COLORS['WOOD_DARK'][1] * ratio)
            b = int(COLORS['WOOD_LIGHT'][2] * (1 - ratio) + COLORS['WOOD_DARK'][2] * ratio)
            
            pygame.draw.line(surface, (r, g, b), (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)
        
        # Metal bands
        band_positions = [0.15, 0.4, 0.6, 0.85]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 6) * (1.0 + 0.3 * math.sin(band_ratio * math.pi)))
            pygame.draw.rect(surface, COLORS['WOOD_BAND'], (center_x - band_width // 2, band_y - 2, band_width, 5))

        # Powerup glow
        if has_powerup:
            glow_size = int(25 + 10 * math.sin(self.powerup_pulse * 4))
            glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], 40), (glow_size, glow_size), glow_size)
            surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

    def draw_metal_barrel(self, surface, x, y, has_powerup=False):
        """Draw metal barrel"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        # Barrel body with metallic gradient
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.25 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 10) * curve_factor)
            
            ratio = i / TILE_SIZE if TILE_SIZE > 0 else 0
            reflection_factor = 1.0 + 0.4 * math.sin(ratio * math.pi * 3)
            
            r = int(COLORS['METAL_LIGHT'][0] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][0] * ratio)
            g = int(COLORS['METAL_LIGHT'][1] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][1] * ratio)
            b = int(COLORS['METAL_LIGHT'][2] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][2] * ratio)
            
            r = max(0, min(255, r))
            g = max(0, min(255, g))
            b = max(0, min(255, b))
            
            pygame.draw.line(surface, (r, g, b), (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Powerup glow
        if has_powerup:
            glow_size = int(25 + 10 * math.sin(self.powerup_pulse * 4))
            glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], 40), (glow_size, glow_size), glow_size)
            surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

    def draw_bomb(self, surface, x, y, bomb: BombState):
        """Draw bomb"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        pulse = 0.8 + 0.2 * math.sin(self.time * 4)
        bomb_size = int(16 * pulse)
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (80, 80, 80), (center_x, center_y), bomb_size, 2)
        
        # Fuse
        fuse_end_x = center_x - bomb_size // 2
        fuse_end_y = center_y - bomb_size
        pygame.draw.line(surface, COLORS['BOMB_FUSE'], (center_x, center_y - bomb_size), (fuse_end_x, fuse_end_y), 3)
        
        # Sparking fuse tip
        spark_intensity = 0.5 + 0.5 * math.sin(self.time * 12)
        spark_size = int(5 * spark_intensity)
        if spark_size > 0:
            pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (fuse_end_x, fuse_end_y), spark_size)

        # Status indicator
        status_colors = {
            'armed': COLORS['TEXT_ORANGE'],
            'remote_idle': COLORS['TEXT_CYAN'],
            'frozen': COLORS['FREEZE_COLOR'],
            'moving': COLORS['TEXT_PURPLE']
        }
        
        status_color = status_colors.get(bomb.status, COLORS['TEXT_WHITE'])
        status_text = bomb.status.upper().replace('_', ' ')
        text_surface = self.mini_font.render(status_text, True, status_color)
        text_rect = text_surface.get_rect(center=(center_x, center_y + bomb_size + 15))
        
        # Background for text
        bg_rect = text_rect.inflate(4, 2)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(text_surface, text_rect)

    def draw_player(self, surface, x, y, player: PlayerState, is_local: bool):
        """Draw player"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        player_id = player.player_id

        # Player colors
        if player_id in self.game_state.dead_players:
            player_colors = {
                1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']
            }
            skin_color = COLORS['SKIN_DEAD']
            skin_shadow_color = COLORS['SKIN_SHADOW_DEAD']
        else:
            player_colors = {
                1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
            }
            skin_color = COLORS['SKIN']
            skin_shadow_color = COLORS['SKIN_SHADOW']

        base_color = player_colors.get(player_id, COLORS['PLAYER_1'])
        
        # Add glow for local players
        if is_local and player_id not in self.game_state.dead_players:
            glow_size = 35
            glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            glow_intensity = 0.6 + 0.4 * math.sin(self.time * 4)
            pygame.draw.circle(glow_surf, (*COLORS['TEXT_CYAN'], int(80 * glow_intensity)), 
                             (glow_size, glow_size), glow_size)
            surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

        # Body
        body_rect = pygame.Rect(center_x - 10, center_y - 4, 20, 24)
        self.draw_gradient_rect(surface, base_color, tuple(max(0, c - 50) for c in base_color), body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 70) for c in base_color), body_rect, 2)

        # Head
        head_y = center_y - 15
        pygame.draw.circle(surface, skin_shadow_color, (center_x + 1, head_y + 1), 12)
        pygame.draw.circle(surface, skin_color, (center_x, head_y), 12)

        # Eyes
        if player_id in self.game_state.dead_players:
            # X eyes for dead players
            eye_size = 2
            pygame.draw.line(surface, (100, 100, 100), 
                           (center_x - 5, head_y - 5), (center_x - 3, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (center_x - 3, head_y - 5), (center_x - 5, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (center_x + 3, head_y - 5), (center_x + 5, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (center_x + 5, head_y - 5), (center_x + 3, head_y - 3), eye_size)
        else:
            # Normal eyes
            pygame.draw.circle(surface, (0, 0, 0), (center_x - 4, head_y - 2), 2)
            pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)

        # Player number badge
        badge_width = 30 if is_local else 20
        badge_text = str(player_id)
        if is_local:
            badge_text += "★"
        
        badge_surface = self.small_font.render(badge_text, True, COLORS['TEXT_WHITE'])
        badge_rect = pygame.Rect(center_x - badge_width//2, center_y + 25, badge_width, 12)
        
        # Badge color based on local status
        badge_color = COLORS['TEXT_CYAN'] if is_local else base_color
        pygame.draw.rect(surface, badge_color, badge_rect)
        pygame.draw.rect(surface, COLORS['TEXT_WHITE'], badge_rect, 1)
        surface.blit(badge_surface, (center_x - badge_surface.get_width()//2, center_y + 26))

    def draw_explosion(self, surface, x, y, explosion: ExplosionState):
        """Draw explosion"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        explosion_size = int(30 * explosion.intensity)
        if explosion_size > 0:
            alpha = int(200 * explosion.intensity)
            explosion_surf = pygame.Surface((explosion_size * 2, explosion_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_MIDDLE'], alpha),
                             (explosion_size, explosion_size), explosion_size)
            surface.blit(explosion_surf, (center_x - explosion_size, center_y - explosion_size))

    def draw_animated_explosion(self, surface, x, y, explosion_anim):
        """Draw animated explosion"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        elapsed = self.time - explosion_anim['start_time']
        progress = elapsed / explosion_anim['duration']
        
        if progress >= 1.0:
            return
            
        explosion_size = int(40 * (1 - progress))
        if explosion_size > 0:
            alpha = int(255 * (1 - progress))
            explosion_surf = pygame.Surface((explosion_size * 2, explosion_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_OUTER'], alpha),
                             (explosion_size, explosion_size), explosion_size)
            surface.blit(explosion_surf, (center_x - explosion_size, center_y - explosion_size))

    def draw_player_panel(self):
        """Draw player statistics panel"""
        panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Panel border
        pygame.draw.rect(panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Title
        title_text = f"GN PLAYERS ({self.local_gn.upper()})"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        panel_surface.blit(title_surface, (10, 10))

        # Draw player stats
        start_y = 50
        player_height = (MAP_SIZE * TILE_SIZE - 60) // 4

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            player_data = self.game_state.players.get(player_id)
            is_local = player_id in self.local_gn_player_ids
            is_dead = player_id in self.game_state.dead_players
            
            # Background
            bg_rect = pygame.Rect(5, y_pos, PLAYER_PANEL_WIDTH - 10, player_height - 5)
            
            if is_local:
                pygame.draw.rect(panel_surface, (*COLORS['TEXT_CYAN'], 30), bg_rect)
                pygame.draw.rect(panel_surface, COLORS['TEXT_CYAN'], bg_rect, 2)
            elif is_dead:
                pygame.draw.rect(panel_surface, (*COLORS['TEXT_RED'], 30), bg_rect)
                pygame.draw.rect(panel_surface, COLORS['TEXT_RED'], bg_rect, 1)
            else:
                pygame.draw.rect(panel_surface, (*COLORS['PANEL_BG'], 100), bg_rect)
                pygame.draw.rect(panel_surface, COLORS['PANEL_BORDER'], bg_rect, 1)

            # Player info
            player_text = f"Player {player_id}"
            if is_local:
                player_text += " (LOCAL)"
            if is_dead:
                player_text += " - DEAD"
            
            color = COLORS['TEXT_CYAN'] if is_local else COLORS['TEXT_RED'] if is_dead else COLORS['TEXT_WHITE']
            player_surface = self.font.render(player_text, True, color)
            panel_surface.blit(player_surface, (15, y_pos + 10))

            # Player stats
            if player_data:
                stats_y = y_pos + 35
                stats = [
                    f"Position: ({player_data.x}, {player_data.y})",
                    f"Health: {player_data.health}",
                    f"Speed: {player_data.speed}",
                    f"Direction: {player_data.direction}"
                ]
                
                for i, stat in enumerate(stats):
                    stat_surface = self.small_font.render(stat, True, COLORS['TEXT_WHITE'])
                    panel_surface.blit(stat_surface, (15, stats_y + i * 15))

                # Timer info
                if any([player_data.timers.movement_timer, player_data.timers.immunity_timer, player_data.timers.request_timer]):
                    timers_y = stats_y + len(stats) * 15 + 5
                    if player_data.timers.movement_timer > 0:
                        timer_text = f"Moving: {player_data.timers.movement_timer}ms"
                        timer_surface = self.mini_font.render(timer_text, True, COLORS['TEXT_CYAN'])
                        panel_surface.blit(timer_surface, (15, timers_y))
                        timers_y += 12
                    
                    if player_data.timers.immunity_timer > 0:
                        timer_text = f"Immune: {player_data.timers.immunity_timer}ms"
                        timer_surface = self.mini_font.render(timer_text, True, COLORS['IMMUNITY_GLOW'])
                        panel_surface.blit(timer_surface, (15, timers_y))

        self.virtual_surface.blit(panel_surface, (0, MAP_OFFSET_Y))

    def draw_timer_panel(self):
        """Draw timer information panel"""
        panel_surface = pygame.Surface((TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Panel border
        pygame.draw.rect(panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Title
        title_text = f"GN TIMERS ({self.local_gn.upper()})"
        title_surface = self.font.render(title_text, True, COLORS['TEXT_GOLD'])
        panel_surface.blit(title_surface, (10, 10))

        # Backend info
        info_y = 40
        backend_info = [
            f"Tick Rate: {self.backend_constants.get('tick_delay', TICK_DELAY)}ms",
            f"Base Move: {self.backend_constants.get('tile_move', TILE_MOVE_BASE)}ms",
            f"Local Players: {self.local_gn_player_ids}",
        ]
        
        for i, info in enumerate(backend_info):
            info_surface = self.mini_font.render(info, True, COLORS['TEXT_CYAN'])
            panel_surface.blit(info_surface, (10, info_y + i * 15))

        # Movement timers
        timer_start_y = info_y + len(backend_info) * 15 + 20
        movement_title = "MOVEMENT TIMERS"
        movement_surface = self.small_font.render(movement_title, True, COLORS['TEXT_WHITE'])
        panel_surface.blit(movement_surface, (10, timer_start_y))
        
        current_y = timer_start_y + 25
        for player_id, timer_ms in self.movement_timers.items():
            if timer_ms > 0:
                color = COLORS['TEXT_CYAN'] if player_id in self.local_gn_player_ids else COLORS['TEXT_WHITE']
                timer_text = f"P{player_id}: {timer_ms}ms"
                if player_id in self.local_gn_player_ids:
                    timer_text += " (LOCAL)"
                
                timer_surface = self.mini_font.render(timer_text, True, color)
                panel_surface.blit(timer_surface, (15, current_y))
                current_y += 20

        # Performance info
        performance_y = MAP_SIZE * TILE_SIZE - 60
        perf_title = f"GN PERFORMANCE"
        perf_surface = self.small_font.render(perf_title, True, COLORS['TEXT_WHITE'])
        panel_surface.blit(perf_surface, (10, performance_y))
        
        fps_text = f"FPS: {self.current_fps:.1f}"
        fps_color = COLORS['TEXT_GREEN'] if self.current_fps > 50 else COLORS['TEXT_ORANGE'] if self.current_fps > 30 else COLORS['TEXT_RED']
        fps_surface = self.mini_font.render(fps_text, True, fps_color)
        panel_surface.blit(fps_surface, (15, performance_y + 20))
        
        # Data source indicator
        data_source_text = "Data: Live Port Only"
        data_surface = self.mini_font.render(data_source_text, True, COLORS['TEXT_GREEN'])
        panel_surface.blit(data_surface, (15, performance_y + 35))

        self.virtual_surface.blit(panel_surface, (TIMER_OFFSET_X, MAP_OFFSET_Y))

    def draw_powerup_panel(self):
        """Draw powerup information panel"""
        panel_surface = pygame.Surface((WINDOW_WIDTH - 20, POWERUP_PANEL_HEIGHT))
        panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Panel border
        pygame.draw.rect(panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, WINDOW_WIDTH - 20, POWERUP_PANEL_HEIGHT), 2)

        # Title
        title_text = f"POWERUPS & GAME STATUS ({self.local_gn.upper()})"
        title_surface = self.font.render(title_text, True, COLORS['TEXT_GOLD'])
        panel_surface.blit(title_surface, (10, 10))

        # Game statistics
        stats_y = 40
        game_stats = [
            f"Map Initialized: {'✅' if self.map_initialized else '❌'}",
            f"Active Players: {len(self.game_state.players)}",
            f"Active Bombs: {len(self.game_state.bombs)}",
            f"Active Explosions: {len(self.game_state.explosions)}",
            f"Dead Players: {len(self.game_state.dead_players)}"
        ]
        
        for i, stat in enumerate(game_stats):
            stat_surface = self.small_font.render(stat, True, COLORS['TEXT_WHITE'])
            panel_surface.blit(stat_surface, (10, stats_y + i * 18))

        # Powerup locations
        powerup_x = 300
        powerup_title = "POWERUP LOCATIONS"
        powerup_surface = self.small_font.render(powerup_title, True, COLORS['TEXT_WHITE'])
        panel_surface.blit(powerup_surface, (powerup_x, 40))
        
        powerup_count = 0
        powerup_y = 60
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                powerup = self.game_state.powerups[x][y]
                if powerup != 'none':
                    powerup_text = f"({x},{y}): {powerup.replace('_', ' ').title()}"
                    powerup_surf = self.mini_font.render(powerup_text, True, COLORS['POWERUP_CORE'])
                    panel_surface.blit(powerup_surf, (powerup_x, powerup_y))
                    powerup_y += 15
                    powerup_count += 1
                    
                    if powerup_count >= 8:  # Limit display
                        break
            if powerup_count >= 8:
                break

        # Connection status
        connection_x = 600
        connection_title = "CONNECTION STATUS"
        connection_surface = self.small_font.render(connection_title, True, COLORS['TEXT_WHITE'])
        panel_surface.blit(connection_surface, (connection_x, 40))
        
        connection_stats = [
            f"Port Buffer: {len(self.port_buffer)} bytes",
            f"Last Update: {'Live' if self.map_initialized else 'Waiting'}",
            f"Local GN: {self.local_gn}",
            f"Local Players: {self.local_gn_player_ids}"
        ]
        
        for i, stat in enumerate(connection_stats):
            color = COLORS['TEXT_GREEN'] if 'Live' in stat else COLORS['TEXT_CYAN']
            stat_surface = self.mini_font.render(stat, True, color)
            panel_surface.blit(stat_surface, (connection_x, 60 + i * 15))

        self.virtual_surface.blit(panel_surface, (10, POWERUP_OFFSET_Y))

    def handle_events(self):
        """Handle pygame events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.VIDEORESIZE:
                self.current_width = event.w
                self.current_height = event.h
                self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)
                self.scale_factor = min(self.current_width / WINDOW_WIDTH, self.current_height / WINDOW_HEIGHT)
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE and self.you_died_display:
                    # Clear death screen
                    self.you_died_display = None
                    self.death_screen_start_time = None
                elif event.key == pygame.K_ESCAPE:
                    return False
                elif event.key == pygame.K_F1:
                    # Toggle fullscreen
                    pygame.display.toggle_fullscreen()
        
        return True

    def update_performance_metrics(self):
        """Update performance tracking"""
        self.fps_counter += 1
        current_time = time.time()
        
        if current_time - self.last_fps_time >= 1.0:
            self.current_fps = self.fps_counter / (current_time - self.last_fps_time)
            self.fps_counter = 0
            self.last_fps_time = current_time

    def render_frame(self):
        """Render complete frame"""
        # Clear virtual surface
        self.virtual_surface.fill(COLORS['BACKGROUND'])

        # Draw all components
        self.draw_map()
        self.draw_player_panel()
        self.draw_timer_panel()
        self.draw_powerup_panel()

        # Scale and blit to screen
        if self.scale_factor != 1.0:
            scaled_width = int(WINDOW_WIDTH * self.scale_factor)
            scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
            scaled_surface = pygame.transform.scale(self.virtual_surface, (scaled_width, scaled_height))
            
            # Center the scaled surface
            offset_x = (self.current_width - scaled_width) // 2
            offset_y = (self.current_height - scaled_height) // 2
            
            self.screen.fill(COLORS['BACKGROUND'])
            self.screen.blit(scaled_surface, (offset_x, offset_y))
        else:
            self.screen.blit(self.virtual_surface, (0, 0))

        # Draw death screen overlay (always on top)
        self.draw_you_died_screen()

        # Draw status overlay
        self.draw_status_overlay()

        pygame.display.flip()

    def draw_status_overlay(self):
        """Draw status information overlay"""
        overlay_y = 10
        
        # Connection status
        if not self.map_initialized:
            status_text = "⏳ Waiting for data from GN graphics server..."
            status_color = COLORS['TEXT_ORANGE']
        else:
            status_text = f"✅ Live data from {self.local_gn.upper()}"
            status_color = COLORS['TEXT_GREEN']
        
        status_surface = self.font.render(status_text, True, status_color)
        
        # Background for status text
        bg_rect = status_surface.get_rect()
        bg_rect.x = 10
        bg_rect.y = overlay_y
        bg_rect.inflate_ip(10, 4)
        
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        self.screen.blit(bg_surf, bg_rect.topleft)
        self.screen.blit(status_surface, (15, overlay_y))

        # Instructions
        instruction_y = self.current_height - 30
        instructions = "ESC: Exit | F1: Fullscreen | SPACE: Clear death screen"
        instruction_surface = self.small_font.render(instructions, True, COLORS['TEXT_GREY'])
        
        instr_bg_rect = instruction_surface.get_rect()
        instr_bg_rect.x = 10
        instr_bg_rect.y = instruction_y
        instr_bg_rect.inflate_ip(8, 2)
        
        instr_bg_surf = pygame.Surface((instr_bg_rect.width, instr_bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(instr_bg_surf, (0, 0, 0, 120), (0, 0, instr_bg_rect.width, instr_bg_rect.height))
        self.screen.blit(instr_bg_surf, instr_bg_rect.topleft)
        self.screen.blit(instruction_surface, (14, instruction_y))

    def run(self):
        """Main game loop - CLEAN AND SIMPLE"""
        print("🚀 Starting Clean GN Visualizer main loop...")
        running = True
        
        while running:
            frame_start = time.time()
            
            # Handle events
            running = self.handle_events()
            if not running:
                break

            # Read data from port (NO FALLBACKS)
            port_data = self.read_port_data()
            if port_data:
                self.handle_port_data(port_data)

            # Update timing and animations
            self.time = time.time()
            self.powerup_pulse = self.time * 2
            self.update_animations()
            self.update_performance_metrics()

            # Render frame
            self.render_frame()
            
            # Maintain target FPS
            self.clock.tick(FPS)

        print("🛑 Clean GN Visualizer shutting down...")
        pygame.quit()

def main():
    """Main entry point"""
    print("🎮 Clean GN Visualizer Starting...")
    print("📡 ONLY reads from Erlang port (stdin)")
    print("🚫 NO file fallbacks, NO compatibility layers")
    print("✨ Clean, debuggable, single data path")
    
    try:
        visualizer = CleanGNVisualizer()
        visualizer.run()
    except KeyboardInterrupt:
        print("\n⚠️ Interrupted by user")
    except Exception as e:
        print(f"❌ Fatal error: {e}")
        import traceback
        traceback.print_exc()
    finally:
        pygame.quit()
        sys.exit(0)

if __name__ == "__main__":
    main()
