import pygame
import sys
import math
import random
import time
import struct
import select
import json
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
from enum import Enum

# Initialize Pygame
pygame.init()

# Enhanced Constants - Synced with common_parameters.hrl
TILE_SIZE = 40  # Size of each tile in pixels
MAP_SIZE = 16  # Size of the map in tiles (16x16)
PLAYER_PANEL_WIDTH = 250  # Enhanced left panel for player stats
POWERUP_PANEL_HEIGHT = 160  # Enhanced bottom panel for power-ups
TIMER_PANEL_WIDTH = 180  # Right panel for timer information
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + TIMER_PANEL_WIDTH + 30  # Total width
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 30  # Total height
MIN_WINDOW_WIDTH = 1000  # Minimum width for the window
MIN_WINDOW_HEIGHT = 800  # Minimum height for the window
FPS = 60  # Frames per second for the game loop

# Backend Constants (from common_parameters.hrl)
TILE_MOVE_BASE = 1200  # Base movement time in milliseconds
MS_REDUCTION = 200     # Speed reduction per speed level
IMMUNITY_TIME = 3000   # Immunity duration in milliseconds
REQUEST_COOLDOWN = 1000 # Request cooldown in milliseconds
TICK_DELAY = 50        # Backend tick delay in milliseconds
EXPLOSION_DISPLAY_TIME = 1000  # Explosion display time in milliseconds
DEATH_DISPLAY_TIME = 10000     # Death display time in milliseconds

# Layout offsets
MAP_OFFSET_X = PLAYER_PANEL_WIDTH + 10  # Map starts after player panel
MAP_OFFSET_Y = 10  # Small top margin
POWERUP_OFFSET_Y = MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE + 10  # Power-ups below map
TIMER_OFFSET_X = MAP_OFFSET_X + MAP_SIZE * TILE_SIZE + 10   # Timer panel right of map

# Enhanced Color Palette
COLORS = {
    # Enhanced backgrounds with more depth
    'BACKGROUND': (25, 35, 45),
    'UI_BACKGROUND': (35, 45, 55),
    'PANEL_BG': (45, 55, 65),
    'PANEL_BORDER': (65, 75, 85),
    
    # Floor with realistic gradient effect
    'FLOOR_LIGHT': (245, 235, 205),
    'FLOOR_MID': (230, 220, 190),
    'FLOOR_DARK': (215, 205, 175),
    'FLOOR_SHADOW': (200, 190, 160),

    # Enhanced text colors with better contrast
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GOLD': (255, 215, 0),
    'TEXT_SHADOW': (0, 0, 0),
    'TEXT_CYAN': (100, 255, 255),
    'TEXT_ORANGE': (255, 165, 0),
    'TEXT_GREY': (120, 120, 120),
    'TEXT_RED': (200, 50, 50),
    'TEXT_GREEN': (100, 255, 100),
    'TEXT_PURPLE': (200, 100, 255),

    # Enhanced brick walls with realistic texture
    'BRICK_TOP': (180, 90, 45),
    'BRICK_MID': (160, 80, 40),
    'BRICK_DARK': (140, 70, 35),
    'BRICK_SHADOW': (120, 60, 30),
    'MORTAR': (100, 50, 25),

    # Beautiful wooden barrels with wood grain
    'WOOD_LIGHT': (200, 140, 90),
    'WOOD_MID': (180, 120, 70),
    'WOOD_DARK': (160, 100, 50),
    'WOOD_SHADOW': (140, 80, 30),
    'WOOD_HIGHLIGHT': (220, 160, 110),
    'WOOD_BAND': (100, 60, 30),

    # Shiny metal barrels with reflections
    'METAL_LIGHT': (160, 165, 170),
    'METAL_MID': (130, 135, 140),
    'METAL_DARK': (100, 105, 110),
    'METAL_SHADOW': (70, 75, 80),
    'METAL_SHINE': (200, 205, 210),
    'METAL_BAND': (60, 65, 70),

    # Enhanced player colors with better contrast
    'PLAYER_1': (80, 150, 255),  # Bright Blue
    'PLAYER_2': (255, 80, 100),  # Bright Red
    'PLAYER_3': (80, 220, 120),  # Bright Green
    'PLAYER_4': (255, 200, 80),  # Bright Yellow
    'SKIN': (255, 220, 180),
    'SKIN_SHADOW': (230, 195, 155),

    # Dead player colors (greyed out versions)
    'PLAYER_1_DEAD': (60, 80, 120),
    'PLAYER_2_DEAD': (120, 60, 70),
    'PLAYER_3_DEAD': (60, 100, 80),
    'PLAYER_4_DEAD': (120, 100, 60),
    'SKIN_DEAD': (150, 130, 110),
    'SKIN_SHADOW_DEAD': (130, 110, 90),

    # Status effect colors
    'IMMUNITY_GLOW': (100, 255, 255),
    'STUN_COLOR': (255, 255, 100),
    'FREEZE_COLOR': (150, 200, 255),
    'SPEED_BOOST_COLOR': (255, 255, 100),
    
    # Glowing power-ups with animation
    'POWERUP_GLOW': (255, 255, 150),
    'POWERUP_CORE': (255, 215, 0),
    'POWERUP_PULSE': (255, 255, 100),

    # Enhanced bomb and explosion colors
    'BOMB_BLACK': (40, 40, 40),
    'BOMB_FUSE': (255, 100, 0),
    'BOMB_FROZEN': (150, 200, 255),
    'EXPLOSION_CORE': (255, 255, 200),
    'EXPLOSION_MIDDLE': (255, 150, 50),
    'EXPLOSION_OUTER': (255, 50, 50),
    'EXPLOSION_SPARK': (255, 255, 255),

    # Enhanced special effects
    'SHADOW': (0, 0, 0, 60),
    'HIGHLIGHT': (255, 255, 255, 100),
    'SELECTION': (255, 255, 0, 150),
    'GRID_LINE': (0, 0, 0, 40),
    'TIMER_BAR_BG': (50, 50, 50),
    'TIMER_BAR_FILL': (100, 255, 100),
    'TIMER_BAR_DANGER': (255, 100, 100),
}

# Enhanced Data Classes
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
    last_update: float = 0.0

@dataclass
class BombState:
    x: int
    y: int
    bomb_type: str
    timer: int
    owner: int
    radius: int
    status: str  # FSM state: armed, remote_idle, frozen, moving, etc.
    ignited: bool
    movement: bool
    direction: str
    last_update: float = 0.0

@dataclass
class ExplosionState:
    x: int
    y: int
    explosion_type: str
    intensity: float
    remaining_time: float
    start_time: float

class GameState:
    def __init__(self):
        self.tiles = [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        self.powerups = [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        self.players: Dict[int, PlayerState] = {}
        self.bombs: Dict[tuple, BombState] = {}
        self.explosions: List[ExplosionState] = []
        self.dead_players: Dict[int, dict] = {}
        self.backend_timing: Dict[str, int] = {}
        self.update_time: float = 0.0

class EnhancedGameVisualizer:
    def __init__(self):
        # Enhanced window setup
        initial_width = min(WINDOW_WIDTH, 1200)
        initial_height = min(WINDOW_HEIGHT, 900)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Playing with Fire 2 - Enhanced JSON Real-Time Backend Integration")
        self.clock = pygame.time.Clock()

        # Current window dimensions and scaling
        self.current_width = initial_width
        self.current_height = initial_height
        self.scale_factor = min(initial_width / WINDOW_WIDTH, initial_height / WINDOW_HEIGHT)

        # Enhanced font system
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        self.mini_font = pygame.font.Font(None, 14)
        self.powerup_font = pygame.font.Font(None, 20)

        # Enhanced animation and timing system
        self.time = 0.0
        self.backend_time = 0.0
        self.powerup_pulse = 0.0
        self.camera_shake = 0.0
        self.selected_tile = None

        # Backend constants (will be updated from server)
        self.backend_constants = {
            'tile_move': TILE_MOVE_BASE,
            'ms_reduction': MS_REDUCTION,
            'immunity_time': IMMUNITY_TIME,
            'request_cooldown': REQUEST_COOLDOWN,
            'tick_delay': TICK_DELAY,
            'explosion_display_time': EXPLOSION_DISPLAY_TIME
        }

        # Enhanced mapping dictionaries
        self.tile_mapping = {
            'free': 0, 'breakable': 1, 'unbreakable': 2, 'strong': 3, 'player_start': 4
        }
        self.powerup_mapping = {
            'none': 'none', 'move_speed': 'move_speed', 'remote_ignition': 'remote_ignition',
            'repeat_bombs': 'repeat_bombs', 'kick_bomb': 'kick_bomb', 'phased': 'phased',
            'plus_bombs': 'plus_bombs', 'bigger_explosion': 'bigger_explosion',
            'plus_life': 'plus_life', 'freeze_bomb': 'freeze_bomb'
        }

        # Port communication setup
        self.port_buffer = b''
        self.map_initialized = False
        self.waiting_for_initial_map = True

        # Enhanced game state tracking
        self.previous_game_state = None
        self.current_game_state = GameState()
        
        # Enhanced animation systems
        self.player_animations: Dict[int, dict] = {}
        self.bomb_animations: Dict[tuple, dict] = {}
        self.explosion_animations: List[dict] = []
        self.powerup_animations: List[dict] = []
        self.game_effects: List[dict] = []
        self.status_effects: Dict[int, dict] = {}
        self.timer_animations: Dict[int, dict] = {}

        # Enhanced timer tracking
        self.movement_timers: Dict[int, int] = {}
        self.immunity_timers: Dict[int, int] = {}
        self.request_timers: Dict[int, int] = {}
        self.bomb_timers: Dict[tuple, int] = {}

        # Real-time event tracking
        self.last_timer_update = time.time()
        self.timer_update_frequency = TICK_DELAY / 1000.0  # Convert to seconds

        # Enhanced surfaces for optimized rendering
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
        self.player_panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.powerup_panel_surface = pygame.Surface((WINDOW_WIDTH, POWERUP_PANEL_HEIGHT))
        self.timer_panel_surface = pygame.Surface((TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.virtual_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))

        # Performance tracking
        self.fps_counter = 0
        self.last_fps_time = time.time()
        self.current_fps = 0

        print("🎮 Enhanced JSON Game Visualizer initialized with complete backend integration")
        print("⏳ Waiting for JSON map data from enhanced CN server...")

    def read_port_data(self) -> Optional[List[bytes]]:
        """Enhanced port data reading with JSON packet handling"""
        try:
            # Check if data is available on stdin
            ready, _, _ = select.select([sys.stdin], [], [], 0)
            if not ready:
                return None

            # Read available data
            data = sys.stdin.buffer.read()
            if not data:
                return None

            self.port_buffer += data

            # Process complete packets (4-byte length prefix + JSON data)
            packets = []
            while len(self.port_buffer) >= 4:
                # Read packet length (big-endian 32-bit)
                packet_length = struct.unpack('>I', self.port_buffer[:4])[0]

                if len(self.port_buffer) >= 4 + packet_length:
                    # Complete packet available
                    packet_data = self.port_buffer[4:4 + packet_length]
                    self.port_buffer = self.port_buffer[4 + packet_length:]
                    packets.append(packet_data)
                else:
                    # Incomplete packet, wait for more data
                    break

            return packets if packets else None

        except Exception as e:
            # This is normal when no data is available
            return None

    def decode_json_data(self, binary_data: bytes) -> Optional[dict]:
        """Decode JSON data from binary packet"""
        try:
            # Decode UTF-8 JSON string
            json_string = binary_data.decode('utf-8')
            
            # Parse JSON
            json_data = json.loads(json_string)
            
            return json_data
            
        except json.JSONDecodeError as e:
            print(f"❌ JSON decode error: {e}")
            print(f"   Raw data: {binary_data[:100]}...")  # First 100 bytes for debugging
            return None
        except UnicodeDecodeError as e:
            print(f"❌ UTF-8 decode error: {e}")
            return None
        except Exception as e:
            print(f"❌ Error decoding JSON data: {e}")
            return None

    def handle_port_data(self, packets: List[bytes]):
        """Enhanced JSON packet handling with real-time event processing"""
        for packet in packets:
            json_data = self.decode_json_data(packet)
            if json_data:
                # Handle different JSON message types
                message_type = json_data.get('type', 'unknown')
                message_data = json_data.get('data', {})
                timestamp = json_data.get('timestamp', 0)
                
                print(f"📨 Received JSON message: {message_type} at {timestamp}")
                
                if message_type == 'map_update':
                    print("🗺️ Processing JSON map update")
                    self.process_json_map_update(message_data)
                elif message_type == 'movement_confirmation':
                    print("🏃 Received JSON movement confirmation")
                    self.handle_json_movement_confirmation(message_data)
                elif message_type == 'timer_update':
                    print("⏱️ Received JSON timer update")
                    self.handle_json_timer_update(message_data)
                elif message_type == 'fsm_update':
                    print("🎰 Received JSON FSM state update")
                    self.handle_json_fsm_update(message_data)
                elif message_type == 'explosion_event':
                    print("💥 Received JSON explosion event")
                    self.handle_json_explosion_event(message_data)
                elif message_type == 'death_event':
                    print("💀 Received JSON death event")
                    self.handle_json_death_event(message_data)
                else:
                    print(f"⚠️ Unknown JSON message type: {message_type}")
            else:
                # Try to handle as legacy format if JSON parsing fails
                print("⚠️ Failed to parse as JSON, trying legacy format")

    def process_json_map_update(self, map_data: dict) -> bool:
        """Process JSON map update with enhanced backend timing information"""
        try:
            # Store previous state for animation detection
            self.previous_game_state = self.copy_game_state(self.current_game_state)

            # Extract enhanced map information
            grid_data = map_data.get('map', [])
            new_dead_players = map_data.get('dead_players', {})
            new_backend_timing = map_data.get('backend_timing', {})
            new_update_time = map_data.get('update_time', time.time() * 1000)
            new_active_explosions = map_data.get('active_explosions', {})
            
            # Update backend constants if provided
            if new_backend_timing:
                self.backend_constants.update(new_backend_timing)
                self.timer_update_frequency = self.backend_constants.get('tick_delay', TICK_DELAY) / 1000.0
                print(f"📊 Updated backend timing: {new_backend_timing}")

            # Check for newly dead players
            for player_id, death_info in new_dead_players.items():
                player_id_int = int(player_id)
                if player_id_int not in self.current_game_state.dead_players:
                    print(f"💀 New death detected: Player {player_id_int}")
                    self.create_enhanced_death_animation(player_id_int, death_info)
            
            # Update game state
            self.current_game_state.dead_players = {int(k): v for k, v in new_dead_players.items()}
            self.current_game_state.backend_timing = new_backend_timing
            self.current_game_state.update_time = time.time()

            # Parse the enhanced map data
            success = self.parse_json_game_state(grid_data)
            if success:
                # Set map as initialized if this is first successful parse
                if self.waiting_for_initial_map:
                    self.waiting_for_initial_map = False
                    self.map_initialized = True
                    print("✅ Initial JSON map loaded! Now listening for real-time updates...")

                # Detect changes for enhanced animations
                if self.previous_game_state:
                    self.detect_enhanced_game_changes(self.previous_game_state, self.current_game_state)

                return True
            return False
            
        except Exception as e:
            print(f"❌ Error processing JSON map update: {e}")
            import traceback
            traceback.print_exc()
            return False

    def handle_json_movement_confirmation(self, confirmation_data: dict):
        """Handle JSON movement confirmation with real backend timing"""
        entity_type = confirmation_data.get('entity_type', 'unknown')
        entity_data = confirmation_data.get('entity_data', {})

        if entity_type == 'player':
            self.handle_json_player_movement_confirmation(entity_data)
        elif entity_type == 'bomb':
            self.handle_json_bomb_movement_confirmation(entity_data)

    def handle_json_player_movement_confirmation(self, player_data: dict):
        """Handle JSON player movement with real backend timing"""
        player_id = int(player_data.get('player_id', 0))
        from_pos = player_data.get('from_pos', [0, 0])
        to_pos = player_data.get('to_pos', [0, 0])
        direction = player_data.get('direction', 'north')
        speed = int(player_data.get('speed', 1))
        movement_timer = int(player_data.get('movement_timer', 0))
        total_duration = int(player_data.get('total_duration', 0))
        immunity_timer = int(player_data.get('immunity_timer', 0))
        request_timer = int(player_data.get('request_timer', 0))

        # Calculate real movement duration using backend constants
        if total_duration == 0:
            total_duration = self.backend_constants['tile_move'] - (speed - 1) * self.backend_constants['ms_reduction']
        
        actual_duration = total_duration / 1000.0  # Convert to seconds
        remaining_duration = movement_timer / 1000.0 if movement_timer > 0 else actual_duration

        # Create enhanced animation with real timing
        self.player_animations[player_id] = {
            'type': 'confirmed_walking',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': actual_duration,
            'remaining_duration': remaining_duration,
            'speed': speed,
            'movement_timer': movement_timer,
            'total_duration': total_duration,
            'confirmed': True,
            'active': True
        }

        # Update timer tracking
        self.movement_timers[player_id] = movement_timer
        self.immunity_timers[player_id] = immunity_timer
        self.request_timers[player_id] = request_timer

        # Add enhanced speed effects
        if speed > 1:
            self.create_enhanced_speed_boost_effect(player_id, from_pos[0], from_pos[1], speed, direction, immunity_timer)

        print(f"🏃 JSON player {player_id} movement confirmed:")
        print(f"   {from_pos} -> {to_pos} (speed: {speed}x)")
        print(f"   Duration: {actual_duration:.2f}s, Remaining: {remaining_duration:.2f}s")
        print(f"   Timers - Movement: {movement_timer}ms, Immunity: {immunity_timer}ms, Request: {request_timer}ms")

    def handle_json_bomb_movement_confirmation(self, bomb_data: dict):
        """Handle JSON bomb movement with FSM state information"""
        bomb_id = tuple(bomb_data.get('bomb_id', [0, 0]))
        from_pos = bomb_data.get('from_pos', [0, 0])
        to_pos = bomb_data.get('to_pos', [0, 0])
        direction = bomb_data.get('direction', 'north')
        bomb_type = bomb_data.get('type', 'normal_bomb')
        owner = int(bomb_data.get('owner', 1))
        radius = int(bomb_data.get('radius', 2))
        status = bomb_data.get('status', 'armed')
        ignited = bool(bomb_data.get('ignited', False))

        # Bomb movement duration (typically faster than players)
        bomb_movement_duration = 0.25  # 250ms for bomb kicks

        # Create enhanced bomb movement animation
        self.bomb_animations[bomb_id] = {
            'type': 'moving',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': bomb_movement_duration,
            'bomb_type': bomb_type,
            'owner': owner,
            'radius': radius,
            'status': status,
            'ignited': ignited,
            'confirmed': True,
            'active': True
        }

        # Add enhanced kick effect
        self.create_enhanced_bomb_kick_effect(from_pos[0], from_pos[1], direction, owner, bomb_type)

        print(f"💣 JSON bomb movement confirmed:")
        print(f"   {from_pos} -> {to_pos} (kicked by player {owner})")
        print(f"   Type: {bomb_type}, Status: {status}, Ignited: {ignited}")

    def handle_json_timer_update(self, timer_data: dict):
        """Handle real-time JSON timer updates from backend"""
        entity_type = timer_data.get('entity_type', 'unknown')
        
        if entity_type == 'player':
            player_id = int(timer_data.get('player_id', 0))
            movement_timer = int(timer_data.get('movement_timer', 0))
            immunity_timer = int(timer_data.get('immunity_timer', 0))
            request_timer = int(timer_data.get('request_timer', 0))
            position = timer_data.get('position', [0, 0])
            speed = int(timer_data.get('speed', 1))
            
            # Update timer tracking
            self.movement_timers[player_id] = movement_timer
            self.immunity_timers[player_id] = immunity_timer
            self.request_timers[player_id] = request_timer
            
            # Update existing animations with new timer info
            if player_id in self.player_animations:
                anim = self.player_animations[player_id]
                anim['movement_timer'] = movement_timer
                
                # Recalculate progress based on actual timer
                if anim.get('total_duration', 0) > 0:
                    elapsed_ms = anim['total_duration'] - movement_timer
                    anim['progress'] = min(1.0, elapsed_ms / anim['total_duration'])
            
            # Create timer update effects
            if immunity_timer > 0:
                self.create_immunity_effect(player_id, position[0], position[1], immunity_timer)
            
            print(f"⏱️ JSON timer update for player {player_id}: Movement={movement_timer}, Immunity={immunity_timer}, Request={request_timer}")

    def handle_json_fsm_update(self, fsm_data: dict):
        """Handle JSON FSM state changes for bombs and players"""
        entity_type = fsm_data.get('entity_type', 'unknown')
        
        if entity_type == 'bomb':
            bomb_id = tuple(fsm_data.get('bomb_id', [0, 0]))
            position = fsm_data.get('position', [0, 0])
            bomb_type = fsm_data.get('type', 'normal_bomb')
            status = fsm_data.get('status', 'armed')
            ignited = bool(fsm_data.get('ignited', False))
            owner = int(fsm_data.get('owner', 1))
            radius = int(fsm_data.get('radius', 2))
            
            # Update bomb state
            if bomb_id in self.current_game_state.bombs:
                bomb = self.current_game_state.bombs[bomb_id]
                old_status = bomb.status
                bomb.status = status
                bomb.ignited = ignited
                
                # Create FSM transition effects
                if old_status != status:
                    self.create_bomb_fsm_transition_effect(position[0], position[1], old_status, status, bomb_type)
            
            print(f"🎰 JSON Bomb FSM update at {position}: {status} (ignited: {ignited})")

    def handle_json_explosion_event(self, explosion_data: dict):
        """Handle real-time JSON explosion events"""
        coordinates = explosion_data.get('coordinates', [])
        explosion_type = explosion_data.get('explosion_type', 'standard')
        timestamp = explosion_data.get('timestamp', time.time() * 1000)
        display_time = explosion_data.get('display_time', EXPLOSION_DISPLAY_TIME)
        
        # Create enhanced explosion sequence
        self.create_json_explosion_sequence(coordinates, explosion_type, timestamp, display_time)
        
        print(f"💥 JSON explosion event: {len(coordinates)} coordinates, type: {explosion_type}")

    def handle_json_death_event(self, death_data: dict):
        """Handle JSON death events"""
        player_id = int(death_data.get('player_id', 0))
        death_time = death_data.get('death_time', time.time() * 1000)
        local_gn = death_data.get('local_gn', 'unknown')
        last_known_state = death_data.get('last_known_state')
        
        # Create death animation
        death_info = (death_time, last_known_state, local_gn)
        self.create_enhanced_death_animation(player_id, death_info)
        
        # Add to dead players
        self.current_game_state.dead_players[player_id] = death_info
        
        print(f"💀 JSON death event for player {player_id} on {local_gn}")

    def parse_json_game_state(self, json_grid: List) -> bool:
        """Parse complete game state from JSON grid data"""
        if not json_grid or not isinstance(json_grid, list):
            print("⚠️ Invalid JSON grid data")
            return False

        # Reset current state
        new_players = {}
        new_bombs = {}
        new_explosions = []

        for row_idx in range(min(len(json_grid), MAP_SIZE)):
            row_data = json_grid[row_idx]
            if not isinstance(row_data, list):
                continue
                
            for col_idx in range(min(len(row_data), MAP_SIZE)):
                cell = row_data[col_idx]

                # Handle JSON cell format
                if not isinstance(cell, list) or len(cell) < 4:
                    continue
                    
                tile_type = cell[0] if len(cell) > 0 else 'free'
                powerup_type = cell[1] if len(cell) > 1 else 'none'
                bomb_info = cell[2] if len(cell) > 2 else 'none'
                player_info = cell[3] if len(cell) > 3 else 'none'
                explosion_info = cell[4] if len(cell) > 4 else 'none'
                special_info = cell[5] if len(cell) > 5 else 'none'

                # Transpose coordinates for display
                x, y = col_idx, row_idx

                # Update tiles and powerups
                if x < MAP_SIZE and y < MAP_SIZE:
                    self.current_game_state.tiles[x][y] = self.tile_mapping.get(str(tile_type), 0)
                    self.current_game_state.powerups[x][y] = self.powerup_mapping.get(str(powerup_type), 'none')

                # Parse JSON player information
                if player_info != 'none':
                    player_data = self.parse_json_player_info(player_info, x, y)
                    if player_data:
                        new_players[player_data.player_id] = player_data

                # Parse JSON bomb information
                if bomb_info != 'none':
                    bomb_data = self.parse_json_bomb_info(bomb_info, x, y)
                    if bomb_data:
                        new_bombs[(x, y)] = bomb_data

                # Parse JSON explosion information
                if explosion_info != 'none':
                    explosion_data = self.parse_json_explosion_info(explosion_info, x, y)
                    if explosion_data:
                        new_explosions.append(explosion_data)

        # Update game state
        self.current_game_state.players = new_players
        self.current_game_state.bombs = new_bombs
        self.current_game_state.explosions = new_explosions

        print(f"✅ JSON game state loaded:")
        print(f"   Players: {len(new_players)} (Dead: {len(self.current_game_state.dead_players)})")
        print(f"   Bombs: {len(new_bombs)}, Explosions: {len(new_explosions)}")
        return True

    def parse_json_player_info(self, player_info, x: int, y: int) -> Optional[PlayerState]:
        """Parse JSON player information"""
        try:
            # Handle different JSON formats
            if isinstance(player_info, str):
                if 'player_' in player_info or player_info.isdigit():
                    if 'player_' in player_info:
                        player_num = int(player_info.split('_')[1])
                    else:
                        player_num = int(player_info)
                    return PlayerState(
                        player_id=player_num, x=x, y=y, health=3, speed=1,
                        direction='north', movement=False,
                        timers=PlayerTimers(), status='alive',
                        last_update=self.time
                    )
            elif isinstance(player_info, (list, tuple)) and len(player_info) >= 8:
                # Enhanced JSON format: [player_id, life, speed, direction, movement, movement_timer, immunity_timer, request_timer]
                player_id = player_info[0]
                health = int(player_info[1]) if str(player_info[1]).replace('-', '').isdigit() else 3
                speed = int(player_info[2]) if str(player_info[2]).replace('-', '').isdigit() else 1
                direction = str(player_info[3])
                movement = bool(player_info[4])
                movement_timer = int(player_info[5]) if str(player_info[5]).replace('-', '').isdigit() else 0
                immunity_timer = int(player_info[6]) if str(player_info[6]).replace('-', '').isdigit() else 0
                request_timer = int(player_info[7]) if str(player_info[7]).replace('-', '').isdigit() else 0

                # Extract player number
                if isinstance(player_id, str) and 'player_' in player_id:
                    player_num = int(player_id.split('_')[1])
                elif str(player_id).replace('-', '').isdigit():
                    player_num = int(player_id)
                else:
                    return None
                
                # Update timer tracking
                self.movement_timers[player_num] = movement_timer
                self.immunity_timers[player_num] = immunity_timer
                self.request_timers[player_num] = request_timer
                
                return PlayerState(
                    player_id=player_num, x=x, y=y, health=health, speed=speed,
                    direction=direction, movement=movement,
                    timers=PlayerTimers(movement_timer, immunity_timer, request_timer),
                    status='alive', last_update=self.time
                )
        except (ValueError, TypeError, IndexError) as e:
            print(f"⚠️ Error parsing JSON player info: {e}")
        return None

    def parse_json_bomb_info(self, bomb_info, x: int, y: int) -> Optional[BombState]:
        """Parse JSON bomb information"""
        try:
            if isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return BombState(
                    x=x, y=y, bomb_type='normal_bomb', timer=3000, owner=1,
                    radius=2, status='armed', ignited=False, movement=False,
                    direction='none', last_update=self.time
                )
            elif isinstance(bomb_info, (list, tuple)) and len(bomb_info) >= 7:
                # JSON format: [type, ignited, status, radius, owner, movement, direction]
                bomb_type = str(bomb_info[0])
                ignited = bool(bomb_info[1])
                status = str(bomb_info[2])
                radius = int(bomb_info[3]) if str(bomb_info[3]).replace('-', '').isdigit() else 2
                owner = int(bomb_info[4]) if str(bomb_info[4]).replace('-', '').isdigit() else 1
                movement = bool(bomb_info[5])
                direction = str(bomb_info[6])
                
                # Calculate timer based on FSM state
                timer = 3000  # Default 3 seconds
                if status == 'remote_idle':
                    timer = -1  # Remote bombs don't count down automatically
                elif status == 'frozen':
                    timer = -2  # Frozen bombs are paused
                
                return BombState(
                    x=x, y=y, bomb_type=bomb_type, timer=timer, owner=owner,
                    radius=radius, status=status, ignited=ignited, movement=movement,
                    direction=direction, last_update=self.time
                )
        except (ValueError, TypeError, IndexError) as e:
            print(f"⚠️ Error parsing JSON bomb info: {e}")
        return None

    def parse_json_explosion_info(self, explosion_info, x: int, y: int) -> Optional[ExplosionState]:
        """Parse JSON explosion information"""
        try:
            if isinstance(explosion_info, str) and 'explosion' in explosion_info.lower():
                return ExplosionState(
                    x=x, y=y, explosion_type='blast_center', intensity=1.0,
                    remaining_time=0.5, start_time=self.time
                )
            elif isinstance(explosion_info, (list, tuple)) and len(explosion_info) >= 3:
                exp_type = str(explosion_info[0])
                intensity = float(explosion_info[1]) if str(explosion_info[1]).replace('.', '').replace('-', '').isdigit() else 1.0
                remaining = float(explosion_info[2]) if str(explosion_info[2]).replace('.', '').replace('-', '').isdigit() else 0.5
                
                return ExplosionState(
                    x=x, y=y, explosion_type=exp_type, intensity=intensity,
                    remaining_time=remaining, start_time=self.time
                )
        except (ValueError, TypeError) as e:
            print(f"⚠️ Error parsing JSON explosion info: {e}")
        return None

    def copy_game_state(self, game_state: GameState) -> GameState:
        """Create a deep copy of game state for comparison"""
        new_state = GameState()
        new_state.tiles = [row[:] for row in game_state.tiles]
        new_state.powerups = [row[:] for row in game_state.powerups]
        new_state.players = {k: v for k, v in game_state.players.items()}
        new_state.bombs = {k: v for k, v in game_state.bombs.items()}
        new_state.explosions = game_state.explosions[:]
        new_state.dead_players = {k: v for k, v in game_state.dead_players.items()}
        new_state.backend_timing = {k: v for k, v in game_state.backend_timing.items()}
        new_state.update_time = game_state.update_time
        return new_state

    def detect_enhanced_game_changes(self, old_state: GameState, new_state: GameState):
        """Comprehensive change detection with enhanced backend information"""
        # Detect enhanced player changes with complete timer information
        self.detect_enhanced_player_changes(old_state.players, new_state.players)

        # Detect enhanced bomb lifecycle with FSM state changes
        self.detect_enhanced_bomb_lifecycle(old_state.bombs, new_state.bombs)

        # Detect enhanced explosion evolution
        self.detect_enhanced_explosion_changes(old_state.explosions, new_state.explosions)

        # Detect tile changes (walls destroyed/created)
        self.detect_tile_changes(old_state.tiles, new_state.tiles)

        # Detect power-up changes
        self.detect_powerup_changes(old_state.powerups, new_state.powerups)

    def detect_enhanced_player_changes(self, old_players: Dict[int, PlayerState], new_players: Dict[int, PlayerState]):
        """Detect enhanced player changes with complete timer information"""
        for player_id, new_player in new_players.items():
            if player_id in old_players:
                old_player = old_players[player_id]

                # Position change - only create animation if not already confirmed
                if ((old_player.x, old_player.y) != (new_player.x, new_player.y) and
                        player_id not in self.player_animations):
                    self.create_enhanced_walking_animation(
                        player_id, (old_player.x, old_player.y), (new_player.x, new_player.y),
                        new_player.direction, new_player.speed, new_player.timers
                    )

                # Health change
                if old_player.health != new_player.health:
                    if new_player.health < old_player.health:
                        self.create_enhanced_damage_effect(player_id, new_player.x, new_player.y, 
                                                         old_player.health - new_player.health)
                    else:
                        self.create_enhanced_healing_effect(player_id, new_player.x, new_player.y,
                                                          new_player.health - old_player.health)

                # Timer changes
                if old_player.timers.immunity_timer != new_player.timers.immunity_timer:
                    if new_player.timers.immunity_timer > 0:
                        self.create_immunity_effect(player_id, new_player.x, new_player.y, 
                                                   new_player.timers.immunity_timer)

                # Speed changes
                if old_player.speed != new_player.speed:
                    if new_player.speed > old_player.speed:
                        self.create_speed_change_effect(player_id, new_player.x, new_player.y, new_player.speed)

                # Status changes
                if old_player.status != new_player.status:
                    self.create_enhanced_status_change_effect(player_id, new_player.x, new_player.y,
                                                            old_player.status, new_player.status)
            else:
                # New player appeared (respawn)
                self.create_enhanced_player_spawn_effect(player_id, new_player.x, new_player.y)

    def detect_enhanced_bomb_lifecycle(self, old_bombs: Dict[tuple, BombState], new_bombs: Dict[tuple, BombState]):
        """Detect enhanced bomb lifecycle with FSM state information"""
        # New bombs placed
        for pos, bomb in new_bombs.items():
            if pos not in old_bombs:
                self.create_enhanced_bomb_placement_animation(bomb.x, bomb.y, bomb)

        # Bombs that exploded or disappeared
        for pos, bomb in old_bombs.items():
            if pos not in new_bombs:
                self.create_enhanced_bomb_explosion_sequence(bomb.x, bomb.y, bomb)

        # Bomb state changes
        for pos in old_bombs:
            if pos in new_bombs:
                old_bomb = old_bombs[pos]
                new_bomb = new_bombs[pos]
                
                # FSM state changes
                if old_bomb.status != new_bomb.status:
                    self.create_bomb_fsm_transition_effect(new_bomb.x, new_bomb.y, 
                                                         old_bomb.status, new_bomb.status, new_bomb.bomb_type)
                
                # Ignition state changes
                if old_bomb.ignited != new_bomb.ignited:
                    if new_bomb.ignited:
                        self.create_bomb_ignition_effect(new_bomb.x, new_bomb.y, new_bomb.bomb_type)

    def detect_enhanced_explosion_changes(self, old_explosions: List[ExplosionState], new_explosions: List[ExplosionState]):
        """Detect enhanced explosion evolution"""
        old_explosion_positions = {(e.x, e.y): e for e in old_explosions}
        new_explosion_positions = {(e.x, e.y): e for e in new_explosions}

        # New explosions
        for pos, explosion in new_explosion_positions.items():
            if pos not in old_explosion_positions:
                self.create_enhanced_live_explosion_effect(explosion.x, explosion.y, explosion)

    def detect_tile_changes(self, old_tiles: List[List[int]], new_tiles: List[List[int]]):
        """Detect tile changes (walls destroyed)"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_tile = old_tiles[x][y]
                new_tile = new_tiles[x][y]

                if old_tile != new_tile:
                    self.create_enhanced_tile_change_effect(x, y, old_tile, new_tile)

    def detect_powerup_changes(self, old_powerups: List[List[str]], new_powerups: List[List[str]]):
        """Detect power-up pickup and spawn"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_powerup = old_powerups[x][y]
                new_powerup = new_powerups[x][y]

                if old_powerup != new_powerup:
                    if old_powerup != 'none' and new_powerup == 'none':
                        self.create_enhanced_powerup_pickup_animation(x, y, old_powerup)
                    elif old_powerup == 'none' and new_powerup != 'none':
                        self.create_enhanced_powerup_spawn_animation(x, y, new_powerup)

    # Enhanced Animation Creation Methods
    def create_enhanced_walking_animation(self, player_id: int, old_pos: tuple, new_pos: tuple, 
                                        direction: str, speed: int, timers: PlayerTimers):
        """Enhanced walking animation with real backend timing"""
        # Don't overwrite confirmed animations
        if (player_id in self.player_animations and 
                self.player_animations[player_id].get('confirmed', False)):
            return

        # Calculate real duration using backend constants
        total_duration = self.backend_constants['tile_move'] - (speed - 1) * self.backend_constants['ms_reduction']
        actual_duration = total_duration / 1000.0  # Convert to seconds

        self.player_animations[player_id] = {
            'type': 'walking',
            'start_pos': old_pos,
            'end_pos': new_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': actual_duration,
            'speed': speed,
            'movement_timer': timers.movement_timer,
            'immunity_timer': timers.immunity_timer,
            'request_timer': timers.request_timer,
            'total_duration': total_duration,
            'confirmed': False,
            'active': True
        }

        # Add enhanced dust cloud effect
        self.create_enhanced_dust_cloud_effect(old_pos[0], old_pos[1], direction, speed)

    def create_enhanced_bomb_placement_animation(self, x: int, y: int, bomb_data: BombState):
        """Enhanced animation for bomb placement with FSM state"""
        self.bomb_animations[(x, y)] = {
            'type': 'placement',
            'timer': bomb_data.timer,
            'max_timer': bomb_data.timer,
            'owner': bomb_data.owner,
            'radius': bomb_data.radius,
            'bomb_type': bomb_data.bomb_type,
            'status': bomb_data.status,
            'ignited': bomb_data.ignited,
            'start_time': self.time,
            'pulse_phase': 0,
            'active': True
        }

        # Enhanced screen shake based on bomb type
        shake_intensity = 0.2 if bomb_data.bomb_type == 'remote_bomb' else 0.1
        self.camera_shake = shake_intensity

        # Create placement effect
        self.create_bomb_placement_effect(x, y, bomb_data.owner, bomb_data.bomb_type)

    def create_enhanced_bomb_explosion_sequence(self, x: int, y: int, bomb_data: BombState):
        """Enhanced explosion sequence with realistic physics"""
        # Central explosion with enhanced effects
        self.explosion_animations.append({
            'type': 'bomb_center_enhanced',
            'x': x, 'y': y,
            'radius': bomb_data.radius,
            'bomb_type': bomb_data.bomb_type,
            'owner': bomb_data.owner,
            'start_time': self.time,
            'duration': 2.0,
            'active': True
        })

        # Enhanced explosion rays with realistic propagation
        for direction in ['north', 'south', 'east', 'west']:
            self.create_explosion_ray_sequence(x, y, direction, bomb_data.radius, bomb_data.bomb_type)

        # Enhanced camera shake based on distance and bomb type
        shake_intensity = min(1.0, bomb_data.radius * 0.3)
        if bomb_data.bomb_type == 'remote_bomb':
            shake_intensity *= 1.5
        self.camera_shake = shake_intensity

        # Screen flash effect
        self.create_screen_flash_effect(0.8, (255, 255, 200))

    def create_explosion_ray_sequence(self, origin_x: int, origin_y: int, direction: str, radius: int, bomb_type: str):
        """Create realistic explosion ray sequence"""
        directions = {
            'north': (0, -1), 'south': (0, 1),
            'east': (1, 0), 'west': (-1, 0)
        }
        dx, dy = directions[direction]
        
        for distance in range(1, radius + 1):
            ray_x = origin_x + dx * distance
            ray_y = origin_y + dy * distance
            
            if 0 <= ray_x < MAP_SIZE and 0 <= ray_y < MAP_SIZE:
                # Check for obstacles that would stop the explosion
                tile_type = self.current_game_state.tiles[ray_x][ray_y]
                
                self.explosion_animations.append({
                    'type': 'explosion_ray_enhanced',
                    'x': ray_x, 'y': ray_y,
                    'direction': direction,
                    'distance': distance,
                    'bomb_type': bomb_type,
                    'start_time': self.time + distance * 0.03,  # Realistic propagation delay
                    'duration': 1.2,
                    'intensity': max(0.3, 1.0 - distance * 0.2),
                    'active': True
                })
                
                # Stop at unbreakable walls
                if tile_type == 2:  # unbreakable
                    break

    def create_json_explosion_sequence(self, coordinates: List[tuple], explosion_type: str, timestamp: float, display_time: int):
        """Create JSON explosion sequence for multiple coordinates"""
        for i, coord in enumerate(coordinates):
            if isinstance(coord, list) and len(coord) >= 2:
                x, y = coord[0], coord[1]
                self.explosion_animations.append({
                    'type': 'json_coordinate_explosion',
                    'x': x, 'y': y,
                    'explosion_type': explosion_type,
                    'timestamp': timestamp,
                    'display_time': display_time,
                    'start_time': self.time + i * 0.02,  # Staggered timing
                    'duration': display_time / 1000.0,
                    'active': True
                })

    def create_enhanced_speed_boost_effect(self, player_id: int, x: int, y: int, speed: int, 
                                         direction: str, immunity_timer: int):
        """Enhanced speed boost effect with immunity integration"""
        self.game_effects.append({
            'type': 'speed_boost_enhanced',
            'player_id': player_id,
            'x': x, 'y': y,
            'speed': speed,
            'direction': direction,
            'immunity_timer': immunity_timer,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

        # Add speed trail particles
        for i in range(speed * 3):
            self.game_effects.append({
                'type': 'speed_particle',
                'player_id': player_id,
                'x': x, 'y': y,
                'direction': direction,
                'particle_id': i,
                'start_time': self.time + i * 0.05,
                'duration': 0.8,
                'active': True
            })

    def create_enhanced_bomb_kick_effect(self, x: int, y: int, direction: str, kicker: int, bomb_type: str):
        """Enhanced bomb kick effect with type-specific visuals"""
        self.game_effects.append({
            'type': 'bomb_kick_enhanced',
            'x': x, 'y': y,
            'direction': direction,
            'kicker': kicker,
            'bomb_type': bomb_type,
            'start_time': self.time,
            'duration': 0.6,
            'active': True
        })

        # Add impact particles
        for i in range(8):
            angle = i * 45
            self.game_effects.append({
                'type': 'kick_particle',
                'x': x, 'y': y,
                'angle': angle,
                'bomb_type': bomb_type,
                'start_time': self.time + i * 0.02,
                'duration': 0.5,
                'active': True
            })

    def create_immunity_effect(self, player_id: int, x: int, y: int, immunity_timer: int):
        """Create immunity effect with timer-based intensity"""
        immunity_duration = immunity_timer / 1000.0  # Convert to seconds
        
        self.status_effects[player_id] = {
            'type': 'immunity',
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': immunity_duration,
            'timer': immunity_timer,
            'intensity': min(1.0, immunity_timer / self.backend_constants['immunity_time'])
        }

    def create_enhanced_death_animation(self, player_id: int, death_info: tuple):
        """Enhanced death animation with detailed information"""
        death_time, last_known_state, local_gn = death_info
        
        # Create comprehensive death effect
        if last_known_state:
            if isinstance(last_known_state, dict):
                pos = last_known_state.get('position', [0, 0])
            else:
                pos = getattr(last_known_state, 'position', [0, 0])
                
            x, y = pos if isinstance(pos, list) and len(pos) >= 2 else [0, 0]
            
            self.game_effects.append({
                'type': 'player_death_enhanced',
                'player_id': player_id,
                'x': x, 'y': y,
                'death_time': death_time,
                'local_gn': local_gn,
                'start_time': self.time,
                'duration': 3.0,
                'last_known_state': last_known_state,
                'active': True
            })
            
            # Add death particles
            for i in range(12):
                angle = i * 30
                self.game_effects.append({
                    'type': 'death_particle',
                    'player_id': player_id,
                    'x': x, 'y': y,
                    'angle': angle,
                    'start_time': self.time + i * 0.1,
                    'duration': 2.0,
                    'active': True
                })

    def create_enhanced_damage_effect(self, player_id: int, x: int, y: int, damage: int):
        """Enhanced damage effect with damage amount"""
        self.game_effects.append({
            'type': 'damage_enhanced',
            'player_id': player_id,
            'x': x, 'y': y,
            'damage': damage,
            'start_time': self.time,
            'duration': 1.0,
            'flash_color': (255, 0, 0),
            'active': True
        })

    def create_enhanced_healing_effect(self, player_id: int, x: int, y: int, heal_amount: int):
        """Enhanced healing effect with heal amount"""
        self.game_effects.append({
            'type': 'healing_enhanced',
            'player_id': player_id,
            'x': x, 'y': y,
            'heal_amount': heal_amount,
            'start_time': self.time,
            'duration': 1.2,
            'active': True
        })

    def create_speed_change_effect(self, player_id: int, x: int, y: int, new_speed: int):
        """Create effect for speed changes"""
        self.game_effects.append({
            'type': 'speed_change',
            'player_id': player_id,
            'x': x, 'y': y,
            'new_speed': new_speed,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_bomb_fsm_transition_effect(self, x: int, y: int, old_status: str, new_status: str, bomb_type: str):
        """Create effect for bomb FSM state transitions"""
        self.game_effects.append({
            'type': 'bomb_fsm_transition',
            'x': x, 'y': y,
            'old_status': old_status,
            'new_status': new_status,
            'bomb_type': bomb_type,
            'start_time': self.time,
            'duration': 0.8,
            'active': True
        })

    def create_bomb_ignition_effect(self, x: int, y: int, bomb_type: str):
        """Create effect for bomb ignition"""
        self.game_effects.append({
            'type': 'bomb_ignition',
            'x': x, 'y': y,
            'bomb_type': bomb_type,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_enhanced_dust_cloud_effect(self, x, y, direction, speed):
        """Create enhanced dust cloud based on movement"""
        self.game_effects.append({
            'type': 'dust_cloud_enhanced',
            'x': x, 'y': y,
            'direction': direction,
            'speed': speed,
            'start_time': self.time,
            'duration': 0.4,
            'active': True
        })

    def create_screen_flash_effect(self, intensity, color):
        """Create screen flash effect for explosions"""
        self.game_effects.append({
            'type': 'screen_flash',
            'intensity': intensity,
            'color': color,
            'start_time': self.time,
            'duration': 0.3,
            'active': True
        })

    def create_bomb_placement_effect(self, x, y, owner, bomb_type):
        """Create bomb placement effect"""
        self.game_effects.append({
            'type': 'bomb_placement',
            'x': x, 'y': y,
            'owner': owner,
            'bomb_type': bomb_type,
            'start_time': self.time,
            'duration': 0.5,
            'active': True
        })

    def create_enhanced_powerup_pickup_animation(self, x, y, powerup_type):
        """Create enhanced power-up pickup animation"""
        self.powerup_animations.append({
            'type': 'pickup_enhanced',
            'x': x, 'y': y,
            'powerup': powerup_type,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_enhanced_powerup_spawn_animation(self, x, y, powerup_type):
        """Create enhanced power-up spawn animation"""
        self.powerup_animations.append({
            'type': 'spawn_enhanced',
            'x': x, 'y': y,
            'powerup': powerup_type,
            'start_time': self.time,
            'duration': 0.8,
            'active': True
        })

    def create_enhanced_tile_change_effect(self, x, y, old_tile, new_tile):
        """Create enhanced tile change effect"""
        self.game_effects.append({
            'type': 'tile_change_enhanced',
            'x': x, 'y': y,
            'old_tile': old_tile,
            'new_tile': new_tile,
            'start_time': self.time,
            'duration': 0.6,
            'active': True
        })

    def create_enhanced_status_change_effect(self, player_id, x, y, old_status, new_status):
        """Create enhanced status change effect"""
        self.game_effects.append({
            'type': 'status_change_enhanced',
            'player_id': player_id,
            'x': x, 'y': y,
            'old_status': old_status,
            'new_status': new_status,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_enhanced_player_spawn_effect(self, player_id, x, y):
        """Create enhanced player spawn effect"""
        self.game_effects.append({
            'type': 'player_spawn_enhanced',
            'player_id': player_id,
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': 2.0,
            'active': True
        })

    def create_enhanced_live_explosion_effect(self, x, y, explosion):
        """Create enhanced live explosion effect"""
        self.explosion_animations.append({
            'type': 'live_explosion_enhanced',
            'x': x, 'y': y,
            'explosion_type': explosion.explosion_type,
            'intensity': explosion.intensity,
            'start_time': self.time,
            'duration': explosion.remaining_time,
            'active': True
        })

    # Animation Update System
    def update_all_enhanced_animations(self):
        """Update all enhanced animations with real-time backend integration"""
        current_time = self.time

        # Update player animations with timer synchronization
        self.update_player_animations(current_time)
        
        # Update bomb animations with FSM integration
        self.update_bomb_animations(current_time)
        
        # Update explosion animations with realistic physics
        self.update_explosion_animations(current_time)
        
        # Update game effects
        self.update_game_effects(current_time)
        
        # Update status effects with timer integration
        self.update_status_effects(current_time)
        
        # Update camera effects
        self.update_camera_effects()

    def update_player_animations(self, current_time: float):
        """Update player animations with real-time timer synchronization"""
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                
                # Update progress based on actual backend timer if available
                if anim.get('movement_timer', 0) > 0 and anim.get('total_duration', 0) > 0:
                    # Real backend timer progress
                    remaining_ms = self.movement_timers.get(player_id, anim['movement_timer'])
                    progress = 1.0 - (remaining_ms / anim['total_duration'])
                    anim['progress'] = min(1.0, max(0.0, progress))
                else:
                    # Fallback to time-based progress
                    anim['progress'] = min(1.0, elapsed / anim['duration'])
                
                # Remove completed animations
                if anim['progress'] >= 1.0 or elapsed >= anim['duration']:
                    del self.player_animations[player_id]

    def update_bomb_animations(self, current_time: float):
        """Update bomb animations with FSM state integration"""
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
            elapsed = current_time - anim['start_time']
            
            if anim.get('type') == 'moving':
                # Moving bomb animation
                if elapsed >= anim['duration']:
                    del self.bomb_animations[pos]
            else:
                # Regular bomb countdown with FSM state
                bomb_state = self.current_game_state.bombs.get(pos)
                if bomb_state:
                    anim['status'] = bomb_state.status
                    anim['ignited'] = bomb_state.ignited
                    
                    # Update timer based on FSM state
                    if bomb_state.status == 'frozen':
                        anim['timer'] = -2  # Frozen indicator
                    elif bomb_state.status == 'remote_idle':
                        anim['timer'] = -1  # Remote indicator
                    else:
                        anim['timer'] = max(0, bomb_state.timer)
                
                # Safety timeout
                if elapsed > 30:  # 30 seconds max
                    del self.bomb_animations[pos]

    def update_explosion_animations(self, current_time: float):
        """Update explosion animations with realistic effects"""
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

    def update_game_effects(self, current_time: float):
        """Update all game effects with enhanced processing"""
        self.game_effects = [
            effect for effect in self.game_effects
            if current_time - effect['start_time'] < effect['duration']
        ]

    def update_status_effects(self, current_time: float):
        """Update status effects with real-time timer integration"""
        for player_id in list(self.status_effects.keys()):
            effect = self.status_effects[player_id]
            elapsed = current_time - effect['start_time']
            
            # Update based on actual backend timer if available
            if effect['type'] == 'immunity':
                remaining_timer = self.immunity_timers.get(player_id, 0)
                if remaining_timer <= 0 or elapsed > effect['duration']:
                    del self.status_effects[player_id]
                else:
                    # Update intensity based on remaining time
                    effect['intensity'] = remaining_timer / self.backend_constants['immunity_time']

    def update_camera_effects(self):
        """Update camera shake and other screen effects"""
        if self.camera_shake > 0:
            self.camera_shake -= 2.0 / FPS  # Decay rate
            if self.camera_shake < 0:
                self.camera_shake = 0

    def update_enhanced_death_info(self):
        """Update enhanced death animations and tracking"""
        current_time = self.time
        
        # Clean up expired death effects
        expired_effects = []
        for i, effect in enumerate(self.game_effects):
            if (effect.get('type') == 'player_death_enhanced' and 
                current_time - effect['start_time'] > effect['duration']):
                expired_effects.append(i)
        
        # Remove expired effects in reverse order
        for i in reversed(expired_effects):
            del self.game_effects[i]

    # Enhanced Drawing System
    def draw_enhanced_map(self):
        """Draw the complete enhanced map with all animations and real-time effects"""
        # Apply enhanced camera shake
        shake_x = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update timing and animations
        self.time += 1 / FPS
        self.backend_time += self.timer_update_frequency
        self.powerup_pulse += 1 / FPS
        self.update_all_enhanced_animations()
        self.update_enhanced_death_info()

        # Draw enhanced tiles with shake offset
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = x * TILE_SIZE + shake_y

                tile_type = self.current_game_state.tiles[x][y]
                powerup = self.current_game_state.powerups[x][y]
                has_powerup = powerup != "none"

                # Draw enhanced floor
                if tile_type != 2:
                    self.draw_enhanced_floor(self.map_surface, pixel_x, pixel_y)

                # Draw enhanced objects
                if tile_type == 1:  # BREAKABLE
                    self.draw_enhanced_wooden_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 2:  # UNBREAKABLE
                    self.draw_enhanced_brick_wall(self.map_surface, pixel_x, pixel_y)
                elif tile_type == 3:  # STRONG
                    self.draw_enhanced_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)

                # Enhanced selection highlight
                if self.selected_tile == (x, y):
                    self.draw_enhanced_selection_highlight(self.map_surface, pixel_x, pixel_y)

        # Draw enhanced bombs with FSM state visualization
        for pos, bomb in self.current_game_state.bombs.items():
            pixel_x = bomb.y * TILE_SIZE + shake_x
            pixel_y = bomb.x * TILE_SIZE + shake_y
            self.draw_enhanced_bomb_with_fsm_state(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw enhanced players with complete status effects
        for player_id, player in self.current_game_state.players.items():
            pixel_x = player.y * TILE_SIZE + shake_x
            pixel_y = player.x * TILE_SIZE + shake_y
            self.draw_enhanced_player_with_complete_effects(self.map_surface, pixel_x, pixel_y, player)

        # Draw all enhanced explosions
        for explosion in self.explosion_animations:
            self.draw_enhanced_explosion_effect(self.map_surface, explosion)

        # Draw all enhanced game effects
        self.draw_all_enhanced_game_effects(self.map_surface)

        # Draw enhanced power-up animations
        for powerup_anim in self.powerup_animations:
            self.draw_enhanced_powerup_animation(self.map_surface, powerup_anim)

        # Blit map to virtual surface
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    def draw_enhanced_selection_highlight(self, surface, x, y):
        """Draw enhanced selection highlight with animation"""
        pulse = 0.7 + 0.3 * math.sin(self.time * 6)
        highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
        alpha = int(150 * pulse)
        pygame.draw.rect(highlight_surf, (*COLORS['SELECTION'][:3], alpha), (0, 0, TILE_SIZE, TILE_SIZE))
        pygame.draw.rect(highlight_surf, COLORS['TEXT_GOLD'], (0, 0, TILE_SIZE, TILE_SIZE), 3)
        surface.blit(highlight_surf, (x, y))

    def draw_enhanced_bomb_with_fsm_state(self, surface, x, y, bomb_data: BombState):
        """Draw enhanced bomb with complete FSM state visualization"""
        # Get bomb position for animation lookup
        bomb_id = (bomb_data.x, bomb_data.y)
        actual_x, actual_y = x, y

        # Check for movement animation
        if bomb_id in self.bomb_animations:
            anim = self.bomb_animations[bomb_id]
            if anim.get('confirmed', False) and anim.get('type') == 'moving':
                elapsed = self.time - anim['start_time']
                progress = min(elapsed / anim['duration'], 1.0)

                # Interpolate position with easing
                start_x, start_y = anim['start_pos']
                end_x, end_y = anim['end_pos']
                
                # Smooth easing function
                eased_progress = self.ease_out_quad(progress)
                current_x = start_x + (end_x - start_x) * eased_progress
                current_y = start_y + (end_y - start_y) * eased_progress

                # Convert to screen coordinates
                actual_x = current_y * TILE_SIZE
                actual_y = current_x * TILE_SIZE

                # Add enhanced movement trail
                self.draw_enhanced_bomb_movement_trail(surface, anim, progress)

        center_x = actual_x + TILE_SIZE // 2
        center_y = actual_y + TILE_SIZE // 2

        # FSM state-based visual effects
        if bomb_data.status == 'frozen':
            self.draw_frozen_bomb(surface, center_x, center_y, bomb_data)
        elif bomb_data.status == 'remote_idle':
            self.draw_remote_bomb(surface, center_x, center_y, bomb_data)
        elif bomb_data.ignited:
            self.draw_ignited_bomb(surface, center_x, center_y, bomb_data)
        else:
            self.draw_standard_bomb(surface, center_x, center_y, bomb_data)

        # Draw FSM state indicator
        self.draw_bomb_fsm_indicator(surface, center_x, center_y + TILE_SIZE//2 + 15, bomb_data.status)

        # Draw enhanced timer display
        if bomb_data.timer > 0:
            timer_seconds = bomb_data.timer / 1000.0
            self.draw_enhanced_timer_display(surface, center_x, center_y + TILE_SIZE//2 + 25, timer_seconds)
        elif bomb_data.status == 'remote_idle':
            remote_text = "REMOTE"
            text_surface = self.mini_font.render(remote_text, True, COLORS['TEXT_CYAN'])
            text_rect = text_surface.get_rect(center=(center_x, center_y + TILE_SIZE//2 + 25))
            surface.blit(text_surface, text_rect)
        elif bomb_data.status == 'frozen':
            frozen_text = "FROZEN"
            text_surface = self.mini_font.render(frozen_text, True, COLORS['FREEZE_COLOR'])
            text_rect = text_surface.get_rect(center=(center_x, center_y + TILE_SIZE//2 + 25))
            surface.blit(text_surface, text_rect)

    def ease_out_quad(self, t: float) -> float:
        """Quadratic ease-out function for smooth animations"""
        return 1 - (1 - t) * (1 - t)

    def draw_frozen_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in frozen state"""
        # Ice crystal effect
        pulse = 0.8 + 0.2 * math.sin(self.time * 4)
        bomb_size = int(16 * pulse)
        
        # Ice glow
        ice_size = int(bomb_size * 1.8)
        ice_surf = pygame.Surface((ice_size * 2, ice_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(ice_surf, (*COLORS['FREEZE_COLOR'], 120), (ice_size, ice_size), ice_size)
        surface.blit(ice_surf, (center_x - ice_size, center_y - ice_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, COLORS['FREEZE_COLOR'], (center_x, center_y), bomb_size, 3)
        
        # Ice crystals
        for i in range(6):
            angle = i * 60 + self.time * 30
            crystal_x = center_x + int(bomb_size * 0.7 * math.cos(math.radians(angle)))
            crystal_y = center_y + int(bomb_size * 0.7 * math.sin(math.radians(angle)))
            self.draw_ice_crystal(surface, crystal_x, crystal_y, 4)

    def draw_remote_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in remote_idle state"""
        # Pulsing remote indicator
        pulse = 0.6 + 0.4 * math.sin(self.time * 3)
        bomb_size = int(16 * pulse)
        
        # Remote glow
        remote_size = int(bomb_size * 1.5)
        remote_surf = pygame.Surface((remote_size * 2, remote_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(remote_surf, (*COLORS['TEXT_CYAN'], 100), (remote_size, remote_size), remote_size)
        surface.blit(remote_surf, (center_x - remote_size, center_y - remote_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, COLORS['TEXT_CYAN'], (center_x, center_y), bomb_size, 2)
        
        # Remote control indicator
        pygame.draw.circle(surface, COLORS['TEXT_CYAN'], (center_x, center_y - bomb_size + 4), 3)
        pygame.draw.line(surface, COLORS['TEXT_CYAN'], 
                        (center_x, center_y - bomb_size + 1), 
                        (center_x, center_y - bomb_size - 8), 2)

    def draw_ignited_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in ignited state"""
        # Intense pulsing
        pulse = 0.9 + 0.1 * math.sin(self.time * 10)
        bomb_size = int(18 * pulse)
        
        # Danger glow
        danger_size = int(bomb_size * 2)
        danger_surf = pygame.Surface((danger_size * 2, danger_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(danger_surf, (*COLORS['BOMB_FUSE'], 150), (danger_size, danger_size), danger_size)
        surface.blit(danger_surf, (center_x - danger_size, center_y - danger_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (center_x, center_y), bomb_size, 3)
        
        # Sparking effects
        for i in range(8):
            if random.random() < 0.7:  # 70% chance for each spark
                angle = random.random() * 360
                distance = bomb_size + random.randint(5, 15)
                spark_x = center_x + int(distance * math.cos(math.radians(angle)))
                spark_y = center_y + int(distance * math.sin(math.radians(angle)))
                spark_size = random.randint(2, 4)
                pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], (spark_x, spark_y), spark_size)

    def draw_standard_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in standard armed state"""
        # Get animation data
        bomb_anim = self.bomb_animations.get((bomb_data.x, bomb_data.y))
        
        # Timer-based pulsing
        if bomb_data.timer > 0:
            timer_ratio = bomb_data.timer / 3000.0  # Assume 3 second default
            pulse_speed = 2.0 + (1.0 - timer_ratio) * 6.0  # Faster as timer decreases
        else:
            pulse_speed = 8.0
            
        pulse = 0.8 + 0.2 * math.sin(self.time * pulse_speed)
        bomb_size = int(16 * pulse)

        # Danger glow based on timer
        if bomb_data.timer <= 1000:  # Last second
            danger_intensity = 1.0
        elif bomb_data.timer <= 2000:  # Last 2 seconds
            danger_intensity = 0.7
        else:
            danger_intensity = 0.3

        # Draw danger glow
        glow_size = int(bomb_size * 2 * danger_intensity)
        if glow_size > 0:
            glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (*COLORS['BOMB_FUSE'], int(80 * danger_intensity)),
                             (glow_size, glow_size), glow_size)
            surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

        # Drop shadow
        shadow_surf = pygame.Surface((bomb_size * 2 + 4, bomb_size * 2 + 4), pygame.SRCALPHA)
        pygame.draw.circle(shadow_surf, COLORS['SHADOW'], (bomb_size + 2, bomb_size + 2), bomb_size)
        surface.blit(shadow_surf, (center_x - bomb_size - 2, center_y - bomb_size - 2))

        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (80, 80, 80), (center_x, center_y), bomb_size, 2)

        # Highlight
        pygame.draw.circle(surface, (120, 120, 120), 
                         (center_x - bomb_size // 3, center_y - bomb_size // 3), bomb_size // 4)

        # Enhanced fuse with sparks
        fuse_length = bomb_size // 2
        fuse_end_x = center_x - fuse_length
        fuse_end_y = center_y - bomb_size
        pygame.draw.line(surface, COLORS['BOMB_FUSE'], 
                        (center_x, center_y - bomb_size), (fuse_end_x, fuse_end_y), 3)

        # Sparking fuse tip with particles
        spark_intensity = 0.5 + 0.5 * math.sin(self.time * 12)
        spark_size = int(5 * spark_intensity)
        if spark_size > 0:
            pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (fuse_end_x, fuse_end_y), spark_size)
            pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], (fuse_end_x, fuse_end_y), spark_size // 2)
            
            # Add spark particles
            for i in range(3):
                if random.random() < 0.8:
                    particle_x = fuse_end_x + random.randint(-8, 8)
                    particle_y = fuse_end_y + random.randint(-8, 8)
                    pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], (particle_x, particle_y), 1)

    def draw_ice_crystal(self, surface, x, y, size):
        """Draw small ice crystal"""
        points = []
        for i in range(6):
            angle = i * 60
            point_x = x + int(size * math.cos(math.radians(angle)))
            point_y = y + int(size * math.sin(math.radians(angle)))
            points.append((point_x, point_y))
        
        if len(points) >= 3:
            pygame.draw.polygon(surface, COLORS['FREEZE_COLOR'], points)
            pygame.draw.polygon(surface, (255, 255, 255), points, 1)

    def draw_bomb_fsm_indicator(self, surface, x, y, status):
        """Draw FSM state indicator"""
        status_colors = {
            'armed': COLORS['TEXT_ORANGE'],
            'remote_idle': COLORS['TEXT_CYAN'],
            'frozen': COLORS['FREEZE_COLOR'],
            'moving': COLORS['TEXT_PURPLE'],
            'ignited': COLORS['BOMB_FUSE']
        }
        
        color = status_colors.get(status, COLORS['TEXT_WHITE'])
        status_text = status.upper().replace('_', ' ')
        text_surface = self.mini_font.render(status_text, True, color)
        text_rect = text_surface.get_rect(center=(x, y))
        
        # Background
        bg_rect = text_rect.inflate(4, 2)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(text_surface, text_rect)

    def draw_enhanced_timer_display(self, surface, x, y, timer_seconds):
        """Draw enhanced timer display with color coding"""
        timer_text = f"{timer_seconds:.1f}"
        
        # Color based on time remaining
        if timer_seconds <= 1.0:
            color = COLORS['TEXT_RED']
        elif timer_seconds <= 2.0:
            color = COLORS['TEXT_ORANGE']
        else:
            color = COLORS['TEXT_WHITE']
            
        timer_surface = self.font.render(timer_text, True, color)
        timer_rect = timer_surface.get_rect(center=(x, y))

        # Enhanced background with pulse effect
        pulse = 0.8 + 0.2 * math.sin(self.time * 8) if timer_seconds <= 1.0 else 1.0
        bg_rect = timer_rect.inflate(int(8 * pulse), int(4 * pulse))
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, int(180 * pulse)), (0, 0, bg_rect.width, bg_rect.height))
        pygame.draw.rect(bg_surf, (*color, int(100 * pulse)), (0, 0, bg_rect.width, bg_rect.height), 1)
        
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(timer_surface, timer_rect)

    def draw_enhanced_bomb_movement_trail(self, surface, anim, progress):
        """Draw enhanced trail effect for moving bomb"""
        start_x, start_y = anim['start_pos']
        direction = anim['direction']
        bomb_type = anim.get('bomb_type', 'normal_bomb')

        # Enhanced motion blur with type-specific colors
        trail_colors = {
            'normal_bomb': (60, 60, 60),
            'remote_bomb': (100, 150, 255),
            'freeze_bomb': COLORS['FREEZE_COLOR']
        }
        trail_color = trail_colors.get(bomb_type, (60, 60, 60))

        trail_length = 8
        for i in range(trail_length):
            trail_progress = max(0, progress - i * 0.08)
            if trail_progress <= 0:
                continue

            trail_x = start_x + (anim['end_pos'][0] - start_x) * trail_progress
            trail_y = start_y + (anim['end_pos'][1] - start_y) * trail_progress

            screen_x = trail_y * TILE_SIZE + TILE_SIZE // 2
            screen_y = trail_x * TILE_SIZE + TILE_SIZE // 2

            alpha = int(120 * (1 - i / trail_length) * (1 - progress))
            if alpha > 0:
                trail_size = max(1, 15 - i * 2)
                trail_surf = pygame.Surface((trail_size * 2, trail_size * 2), pygame.SRCALPHA)
                pygame.draw.circle(trail_surf, (*trail_color, alpha),
                                 (trail_size, trail_size), trail_size)
                surface.blit(trail_surf, (screen_x - trail_size, screen_y - trail_size))

    def draw_enhanced_player_with_complete_effects(self, surface, x, y, player: PlayerState):
        """Draw player with complete status effects and timer visualization"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        player_id = player.player_id
        speed = player.speed

        # Get enhanced player colors
        if player_id in self.current_game_state.dead_players:
            # Dead player colors
            player_colors = {
                1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']
            }
            skin_color = COLORS['SKIN_DEAD']
            skin_shadow_color = COLORS['SKIN_SHADOW_DEAD']
        else:
            # Alive player colors with speed enhancement
            player_colors = {
                1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
            }
            skin_color = COLORS['SKIN']
            skin_shadow_color = COLORS['SKIN_SHADOW']

        base_color = player_colors.get(player_id, COLORS['PLAYER_1'])

        # Enhance color based on speed and status effects
        enhanced_color = base_color
        if speed > 1:
            glow_intensity = min(speed * 0.2, 0.8)
            enhanced_color = tuple(min(255, int(c * (1 + glow_intensity))) for c in base_color)

        # Handle walking animation with enhanced interpolation
        char_x, char_y = x, y
        if player_id in self.player_animations:
            anim = self.player_animations[player_id]
            progress = anim.get('progress', 0.0)

            # Enhanced easing
            eased_progress = self.ease_out_quad(progress)
            start_x, start_y = anim['start_pos']
            end_x, end_y = anim['end_pos']

            current_x = start_x + (end_x - start_x) * eased_progress
            current_y = start_y + (end_y - start_y) * eased_progress

            # Convert to screen coordinates
            char_x = current_y * TILE_SIZE
            char_y = current_x * TILE_SIZE
            center_x = char_x + TILE_SIZE // 2
            center_y = char_y + TILE_SIZE // 2

            # Enhanced walking animation
            walk_frequency = 8 + speed * 2
            walk_bounce = math.sin(progress * math.pi * walk_frequency) * (2 + speed * 0.5)
            center_y -= walk_bounce

        # Draw enhanced status effects
        self.draw_enhanced_status_effects(surface, center_x, center_y, player_id)

        # Draw the player character
        self.draw_enhanced_player_character(surface, char_x, char_y, player_id, enhanced_color, skin_color, skin_shadow_color)

        # Draw timer visualizations
        self.draw_player_timer_visualizations(surface, center_x, center_y + 35, player_id, player.timers)

        # Draw speed indicator
        if speed > 1:
            self.draw_enhanced_speed_indicator(surface, center_x, center_y + 50, speed)

    def draw_enhanced_status_effects(self, surface, center_x, center_y, player_id):
        """Draw all enhanced status effects around player"""
        # Immunity effect
        if player_id in self.status_effects and self.status_effects[player_id]['type'] == 'immunity':
            effect = self.status_effects[player_id]
            intensity = effect.get('intensity', 1.0)
            
            # Pulsing immunity glow
            pulse = 0.6 + 0.4 * math.sin(self.time * 8)
            glow_size = int(35 * intensity * pulse)
            
            if glow_size > 0:
                immunity_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
                alpha = int(120 * intensity * pulse)
                pygame.draw.circle(immunity_surf, (*COLORS['IMMUNITY_GLOW'], alpha),
                                 (glow_size, glow_size), glow_size)
                surface.blit(immunity_surf, (center_x - glow_size, center_y - glow_size))

        # Stun effect with orbiting stars
        if player_id in self.status_effects and self.status_effects[player_id]['type'] == 'stun':
            for i in range(4):
                angle = (self.time * 4 + i * 1.57) % (2 * math.pi)  # 90 degrees apart
                star_x = center_x + int(30 * math.cos(angle))
                star_y = center_y + int(30 * math.sin(angle)) - 25
                self.draw_enhanced_stun_star(surface, star_x, star_y)

        # Movement trail for fast players
        if player_id in self.player_animations:
            anim = self.player_animations[player_id]
            speed = anim.get('speed', 1)
            if speed > 1:
                self.draw_player_movement_trail(surface, center_x, center_y, anim, speed)

    def draw_player_movement_trail(self, surface, center_x, center_y, anim, speed):
        """Draw movement trail for fast players"""
        direction = anim.get('direction', 'north')
        progress = anim.get('progress', 0.0)
        
        # Direction vectors
        directions = {
            'north': (0, 1), 'south': (0, -1),
            'east': (-1, 0), 'west': (1, 0)
        }
        dx, dy = directions.get(direction, (0, 0))
        
        # Draw trail particles
        trail_length = speed * 3
        for i in range(trail_length):
            trail_offset = i * 8
            trail_x = center_x + dx * trail_offset
            trail_y = center_y + dy * trail_offset
            
            alpha = int(80 * (1 - i / trail_length) * (1 - progress))
            if alpha > 0:
                particle_size = max(1, 8 - i // 2)
                speed_color = COLORS['SPEED_BOOST_COLOR']
                
                particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                pygame.draw.circle(particle_surf, (*speed_color, alpha),
                                 (particle_size, particle_size), particle_size)
                surface.blit(particle_surf, (trail_x - particle_size, trail_y - particle_size))

    def draw_player_timer_visualizations(self, surface, x, y, player_id, timers: PlayerTimers):
        """Draw visual representations of all player timers"""
        timer_y = y
        
        # Movement timer bar
        if timers.movement_timer > 0:
            total_duration = self.backend_constants['tile_move']  # Base duration
            progress = 1.0 - (timers.movement_timer / total_duration)
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['TEXT_CYAN'], "MOVE")
            timer_y += 8
        
        # Immunity timer bar
        if timers.immunity_timer > 0:
            progress = timers.immunity_timer / self.backend_constants['immunity_time']
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['IMMUNITY_GLOW'], "IMM")
            timer_y += 8
        
        # Request cooldown timer bar
        if timers.request_timer > 0:
            progress = timers.request_timer / self.backend_constants['request_cooldown']
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['TEXT_ORANGE'], "REQ")

    def draw_timer_bar(self, surface, x, y, width, height, progress, color, label):
        """Draw individual timer bar with label"""
        # Background
        pygame.draw.rect(surface, COLORS['TIMER_BAR_BG'], (x, y, width, height))
        
        # Fill based on progress
        if progress > 0:
            fill_width = int(width * progress)
            pygame.draw.rect(surface, color, (x, y, fill_width, height))
        
        # Border
        pygame.draw.rect(surface, COLORS['TEXT_WHITE'], (x, y, width, height), 1)
        
        # Label
        if label:
            label_surface = self.mini_font.render(label, True, color)
            label_rect = label_surface.get_rect()
            label_rect.midright = (x - 2, y + height // 2)
            surface.blit(label_surface, label_rect)

    def draw_enhanced_speed_indicator(self, surface, x, y, speed):
        """Draw enhanced speed boost indicator"""
        # Animated speed arrows with glow
        glow_intensity = 0.8 + 0.2 * math.sin(self.time * 6)
        
        for i in range(min(speed - 1, 4)):  # Max 4 arrows
            arrow_x = x + (i - (speed - 2) / 2) * 10
            arrow_offset = math.sin(self.time * 8 + i * 0.5) * 2
            arrow_y = y + arrow_offset
            
            # Glow effect
            glow_surf = pygame.Surface((12, 12), pygame.SRCALPHA)
            glow_alpha = int(100 * glow_intensity)
            pygame.draw.circle(glow_surf, (*COLORS['SPEED_BOOST_COLOR'], glow_alpha), (6, 6), 6)
            surface.blit(glow_surf, (arrow_x - 6, arrow_y - 6))
            
            # Arrow
            arrow_points = [
                (arrow_x - 4, arrow_y + 3),
                (arrow_x, arrow_y - 3),
                (arrow_x + 4, arrow_y + 3),
                (arrow_x, arrow_y + 1)
            ]
            pygame.draw.polygon(surface, COLORS['SPEED_BOOST_COLOR'], arrow_points)
            pygame.draw.polygon(surface, (255, 255, 255), arrow_points, 1)

    def draw_enhanced_stun_star(self, surface, x, y):
        """Draw enhanced stun star with glow"""
        star_size = 8
        pulse = 0.8 + 0.2 * math.sin(self.time * 10)
        actual_size = int(star_size * pulse)
        
        # Glow effect
        glow_surf = pygame.Surface((actual_size * 3, actual_size * 3), pygame.SRCALPHA)
        pygame.draw.circle(glow_surf, (*COLORS['STUN_COLOR'], 100), 
                         (actual_size * 3 // 2, actual_size * 3 // 2), actual_size * 3 // 2)
        surface.blit(glow_surf, (x - actual_size * 3 // 2, y - actual_size * 3 // 2))
        
        # Star shape
        points = []
        for i in range(10):  # 5-pointed star = 10 points
            angle = i * math.pi / 5 + self.time * 2  # Rotating
            if i % 2 == 0:
                # Outer points
                px = x + actual_size * math.cos(angle)
                py = y + actual_size * math.sin(angle)
            else:
                # Inner points
                px = x + (actual_size // 2) * math.cos(angle)
                py = y + (actual_size // 2) * math.sin(angle)
            points.append((px, py))

        pygame.draw.polygon(surface, COLORS['STUN_COLOR'], points)
        pygame.draw.polygon(surface, (255, 255, 255), points, 1)

    def draw_enhanced_player_character(self, surface, x, y, player_id, outfit_color, skin_color, skin_shadow_color):
        """Draw enhanced player character with improved details"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Gentle bobbing with player-specific phase
        bob_offset = math.sin(self.time * 4 + player_id * 1.5) * 2
        char_y = center_y + bob_offset

        # Enhanced drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 4, 16), pygame.SRCALPHA)
        shadow_alpha = 80 if player_id not in self.current_game_state.dead_players else 40
        pygame.draw.ellipse(shadow_surf, (*COLORS['SHADOW'][:3], shadow_alpha), (2, 0, TILE_SIZE, 16))
        surface.blit(shadow_surf, (x - 2, y + TILE_SIZE - 12))

        # Enhanced body with better gradient
        body_rect = pygame.Rect(center_x - 10, char_y - 4, 20, 24)
        self.draw_gradient_rect(surface, outfit_color, 
                               tuple(max(0, c - 50) for c in outfit_color), body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 70) for c in outfit_color), body_rect, 2)

        # Enhanced head with better shading
        head_y = char_y - 15
        pygame.draw.circle(surface, skin_shadow_color, (center_x + 1, head_y + 1), 12)
        pygame.draw.circle(surface, skin_color, (center_x, head_y), 12)
        pygame.draw.circle(surface, tuple(max(0, c - 40) for c in skin_color), (center_x, head_y), 12, 1)

        # Enhanced facial features
        # Eyes with better detail
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x - 7, head_y - 5, 7, 5))
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 5, 7, 5))
        
        # Pupils with reflection
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1)
        pygame.draw.circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1)

        # Enhanced eyebrows
        pygame.draw.arc(surface, (101, 67, 33), (center_x - 7, head_y - 8, 6, 5), 0, math.pi, 2)
        pygame.draw.arc(surface, (101, 67, 33), (center_x + 2, head_y - 8, 6, 5), 0, math.pi, 2)

        # Enhanced nose and mouth
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in skin_color), (center_x, head_y), 2)
        pygame.draw.arc(surface, (150, 100, 80), (center_x - 4, head_y + 2, 8, 5), 0, math.pi, 2)

        # Enhanced arms with animation
        arm_swing = math.sin(self.time * 6 + player_id * 0.5) * 4
        
        # Left arm with hand
        left_arm_x = center_x - 14 + arm_swing
        left_arm_y = char_y + 8
        pygame.draw.line(surface, outfit_color, (center_x - 8, char_y + 6), (left_arm_x, left_arm_y), 6)
        pygame.draw.circle(surface, skin_color, (left_arm_x, left_arm_y), 4)
        pygame.draw.circle(surface, skin_shadow_color, (left_arm_x, left_arm_y), 4, 1)

        # Right arm with hand
        right_arm_x = center_x + 14 - arm_swing
        pygame.draw.line(surface, outfit_color, (center_x + 8, char_y + 6), (right_arm_x, left_arm_y), 6)
        pygame.draw.circle(surface, skin_color, (right_arm_x, left_arm_y), 4)
        pygame.draw.circle(surface, skin_shadow_color, (right_arm_x, left_arm_y), 4, 1)

        # Enhanced legs with walking animation
        leg_offset = math.sin(self.time * 10 + player_id * 0.3) * 3

        # Left leg with shoe
        left_leg_rect = pygame.Rect(center_x - 8 + leg_offset, char_y + 20, 5, 12)
        pygame.draw.rect(surface, outfit_color, left_leg_rect)
        pygame.draw.rect(surface, (60, 60, 60), (center_x - 9 + leg_offset, char_y + 30, 7, 5))
        pygame.draw.rect(surface, (40, 40, 40), (center_x - 9 + leg_offset, char_y + 30, 7, 5), 1)

        # Right leg with shoe
        right_leg_rect = pygame.Rect(center_x + 3 - leg_offset, char_y + 20, 5, 12)
        pygame.draw.rect(surface, outfit_color, right_leg_rect)
        pygame.draw.rect(surface, (60, 60, 60), (center_x + 2 - leg_offset, char_y + 30, 7, 5))
        pygame.draw.rect(surface, (40, 40, 40), (center_x + 2 - leg_offset, char_y + 30, 7, 5), 1)

        # Enhanced player number badge
        badge_surf = pygame.Surface((20, 12), pygame.SRCALPHA)
        pygame.draw.rect(badge_surf, (255, 255, 255, 220), (0, 0, 20, 12))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 20, 12), 1)
        pygame.draw.rect(badge_surf, outfit_color, (1, 1, 18, 10), 1)

        num_text = self.small_font.render(str(player_id), True, (0, 0, 0))
        badge_surf.blit(num_text, (7, -1))

        # Badge glow effect
        glow_surf = pygame.Surface((24, 16), pygame.SRCALPHA)
        pygame.draw.rect(glow_surf, (*COLORS['POWERUP_CORE'], 120), (0, 0, 24, 16))
        surface.blit(glow_surf, (center_x - 12, char_y + 28))
        surface.blit(badge_surf, (center_x - 10, char_y + 30))

    # Drawing utility methods with enhancements
    def draw_gradient_rect(self, surface, color1, color2, rect, vertical=True):
        """Enhanced gradient rectangle with smooth blending"""
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

    def draw_enhanced_floor(self, surface, x, y):
        """Enhanced floor tile with realistic texture"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['FLOOR_LIGHT'], COLORS['FLOOR_DARK'], rect)

        # Subtle texture pattern
        for i in range(0, TILE_SIZE, 8):
            for j in range(0, TILE_SIZE, 8):
                if (i + j) % 16 == 0:
                    pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], (x + i, y + j, 4, 4))

        # Enhanced border
        pygame.draw.rect(surface, COLORS['FLOOR_LIGHT'], rect, 2)
        pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], rect, 1)

    def draw_enhanced_brick_wall(self, surface, x, y):
        """Enhanced brick wall with realistic depth and texture"""
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 6, TILE_SIZE + 6), pygame.SRCALPHA)
        pygame.draw.rect(shadow_surf, COLORS['SHADOW'], (0, 0, TILE_SIZE + 6, TILE_SIZE + 6))
        surface.blit(shadow_surf, (x - 3, y - 3))

        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['BRICK_TOP'], COLORS['BRICK_DARK'], rect)

        # Enhanced brick pattern
        brick_height = TILE_SIZE // 5
        for row in range(5):
            brick_y = y + row * brick_height
            pygame.draw.line(surface, COLORS['MORTAR'],
                           (x, brick_y), (x + TILE_SIZE, brick_y), 2)

            # Alternating brick pattern
            offset = (TILE_SIZE // 3) if row % 2 == 0 else TILE_SIZE // 6
            for i in range(4):
                brick_x = x + offset + i * (TILE_SIZE // 4)
                if x <= brick_x < x + TILE_SIZE:
                    pygame.draw.line(surface, COLORS['MORTAR'],
                                   (brick_x, brick_y), (brick_x, brick_y + brick_height), 2)

        # Enhanced 3D effect
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x + TILE_SIZE, y), 3)
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x, y + TILE_SIZE), 3)
        pygame.draw.line(surface, COLORS['BRICK_SHADOW'], (x + TILE_SIZE - 1, y), 
                        (x + TILE_SIZE - 1, y + TILE_SIZE), 2)
        pygame.draw.line(surface, COLORS['BRICK_SHADOW'], (x, y + TILE_SIZE - 1), 
                        (x + TILE_SIZE, y + TILE_SIZE - 1), 2)

    def draw_enhanced_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced wooden barrel with realistic wood grain and metal bands"""
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 8, TILE_SIZE + 8), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow_surf, COLORS['SHADOW'], (0, 0, TILE_SIZE + 8, TILE_SIZE + 8))
        surface.blit(shadow_surf, (x - 4, y - 4))

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Enhanced barrel body with realistic curvature
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.3 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 8) * curve_factor)

            # Enhanced wood coloring with grain
            ratio = i / TILE_SIZE
            grain_noise = math.sin(i * 0.8) * 0.1
            adjusted_ratio = max(0, min(1, ratio + grain_noise))
            
            r = int(COLORS['WOOD_LIGHT'][0] * (1 - adjusted_ratio) + COLORS['WOOD_DARK'][0] * adjusted_ratio)
            g = int(COLORS['WOOD_LIGHT'][1] * (1 - adjusted_ratio) + COLORS['WOOD_DARK'][1] * adjusted_ratio)
            b = int(COLORS['WOOD_LIGHT'][2] * (1 - adjusted_ratio) + COLORS['WOOD_DARK'][2] * adjusted_ratio)

            pygame.draw.line(surface, (r, g, b),
                           (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Enhanced metal bands with 3D effect
        band_positions = [0.15, 0.4, 0.6, 0.85]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 6) * (1.0 + 0.3 * math.sin(band_ratio * math.pi)))

            # Band shadow
            pygame.draw.rect(surface, COLORS['WOOD_SHADOW'],
                           (center_x - band_width // 2, band_y - 1, band_width, 6))
            # Main band
            pygame.draw.rect(surface, COLORS['WOOD_BAND'],
                           (center_x - band_width // 2, band_y - 2, band_width, 5))
            # Band highlight
            pygame.draw.rect(surface, COLORS['WOOD_HIGHLIGHT'],
                           (center_x - band_width // 2, band_y - 2, band_width, 1))

        # Enhanced wood grain texture
        for i in range(8):
            grain_x = x + 6 + i * 4
            if grain_x < x + TILE_SIZE - 6:
                grain_intensity = 0.7 + 0.3 * math.sin(i * 1.2)
                grain_color = tuple(int(c * grain_intensity) for c in COLORS['WOOD_SHADOW'])
                pygame.draw.line(surface, grain_color,
                               (grain_x, y + 4), (grain_x, y + TILE_SIZE - 4), 1)

        # Enhanced highlight
        pygame.draw.line(surface, COLORS['WOOD_HIGHLIGHT'],
                        (x + 4, y + 2), (x + 4, y + TILE_SIZE - 2), 3)

        # Enhanced powerup glow
        if has_powerup:
            self.draw_enhanced_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced metal barrel with realistic metallic reflections"""
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 8, TILE_SIZE + 8), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow_surf, COLORS['SHADOW'], (0, 0, TILE_SIZE + 8, TILE_SIZE + 8))
        surface.blit(shadow_surf, (x - 4, y - 4))

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Enhanced barrel body with metallic gradient
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.25 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 10) * curve_factor)

            # Enhanced metallic coloring with reflections
            ratio = i / TILE_SIZE
            reflection_factor = 1.0 + 0.4 * math.sin(ratio * math.pi * 3)
            
            r = int(COLORS['METAL_LIGHT'][0] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][0] * ratio)
            g = int(COLORS['METAL_LIGHT'][1] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][1] * ratio)
            b = int(COLORS['METAL_LIGHT'][2] * (1 - ratio) * reflection_factor + COLORS['METAL_DARK'][2] * ratio)
            
            # Clamp values
            r = max(0, min(255, r))
            g = max(0, min(255, g))
            b = max(0, min(255, b))

            pygame.draw.line(surface, (r, g, b),
                           (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Enhanced metal bands with reflective surfaces
        band_positions = [0.2, 0.8]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 6) * (1.0 + 0.25 * math.sin(band_ratio * math.pi)))

            # Band shadow
            pygame.draw.rect(surface, COLORS['METAL_SHADOW'],
                           (center_x - band_width // 2, band_y - 1, band_width, 5))
            # Main band
            pygame.draw.rect(surface, COLORS['METAL_BAND'],
                           (center_x - band_width // 2, band_y - 2, band_width, 4))
            # Metallic shine
            pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                           (center_x - band_width // 2, band_y - 2, band_width, 1))

        # Enhanced metallic shine strips
        shine_positions = [0.25, 0.5, 0.75]
        for shine_ratio in shine_positions:
            shine_x = x + int(TILE_SIZE * shine_ratio)
            shine_intensity = 0.6 + 0.4 * math.sin(self.time * 2 + shine_ratio * 10)
            shine_alpha = int(150 * shine_intensity)
            
            if shine_alpha > 0:
                shine_surf = pygame.Surface((3, TILE_SIZE - 8), pygame.SRCALPHA)
                pygame.draw.rect(shine_surf, (*COLORS['METAL_SHINE'], shine_alpha), (0, 0, 3, TILE_SIZE - 8))
                surface.blit(shine_surf, (shine_x - 1, y + 4))

        # Enhanced powerup glow
        if has_powerup:
            self.draw_enhanced_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_powerup_glow(self, surface, center_x, center_y):
        """Enhanced power-up glow with pulsing and particles"""
        glow_intensity = 0.8 + 0.2 * math.sin(self.powerup_pulse * 5)
        glow_size = int(25 + 10 * math.sin(self.powerup_pulse * 4))

        # Multi-layered glow
        for layer in range(3):
            radius = glow_size - layer * 6
            if radius > 0:
                alpha = int(40 * glow_intensity * (1 - layer * 0.3))
                glow_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                glow_color = COLORS['POWERUP_GLOW'] if layer == 0 else COLORS['POWERUP_CORE']
                pygame.draw.circle(glow_surf, (*glow_color, alpha), (radius, radius), radius)
                surface.blit(glow_surf, (center_x - radius, center_y - radius))

        # Floating particles
        for i in range(6):
            angle = (self.time * 2 + i * 60) % 360
            particle_x = center_x + int(20 * math.cos(math.radians(angle)))
            particle_y = center_y + int(20 * math.sin(math.radians(angle))) + int(3 * math.sin(self.time * 3 + i))
            
            particle_size = 2 + int(2 * math.sin(self.time * 4 + i))
            if particle_size > 0:
                pygame.draw.circle(surface, COLORS['POWERUP_PULSE'], (particle_x, particle_y), particle_size)

    # Enhanced explosion effects
    def draw_enhanced_explosion_effect(self, surface, explosion):
        """Draw enhanced explosion effects with realistic physics"""
        elapsed = self.time - explosion['start_time']
        progress = elapsed / explosion['duration']

        if progress >= 1.0:
            return

        explosion_type = explosion['type']
        
        if explosion_type == 'bomb_center_enhanced':
            self.draw_enhanced_bomb_center_explosion(surface, explosion, progress)
        elif explosion_type == 'explosion_ray_enhanced':
            self.draw_enhanced_explosion_ray(surface, explosion, progress)
        elif explosion_type == 'json_coordinate_explosion':
            self.draw_json_coordinate_explosion(surface, explosion, progress)
        else:
            self.draw_standard_explosion(surface, explosion, progress)

    def draw_enhanced_bomb_center_explosion(self, surface, explosion, progress):
        """Draw enhanced central bomb explosion with multiple phases"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2
        radius = explosion.get('radius', 2)
        bomb_type = explosion.get('bomb_type', 'normal_bomb')

        if progress < 0.2:  # Initial blast wave
            blast_radius = int(progress * 120 * radius / 2)
            intensity = 1.0 - progress * 3
            
            # Multi-layered explosion
            for layer in range(4):
                layer_radius = max(1, blast_radius - layer * 15)
                layer_alpha = int(255 * intensity * (1 - layer * 0.2))
                
                if layer == 0:
                    color = COLORS['EXPLOSION_CORE']
                elif layer == 1:
                    color = COLORS['EXPLOSION_MIDDLE']
                else:
                    color = COLORS['EXPLOSION_OUTER']
                
                explosion_surf = pygame.Surface((layer_radius * 2, layer_radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(explosion_surf, (*color, layer_alpha), 
                                 (layer_radius, layer_radius), layer_radius)
                surface.blit(explosion_surf, (center_x - layer_radius, center_y - layer_radius))
                
        elif progress < 0.6:  # Fire and debris phase
            fire_progress = (progress - 0.2) / 0.4
            
            # Fire particles
            for i in range(20 + radius * 5):
                angle = random.random() * 2 * math.pi
                distance = random.random() * 60 * radius / 2
                particle_x = center_x + int(math.cos(angle) * distance)
                particle_y = center_y + int(math.sin(angle) * distance) - int(fire_progress * 20)
                
                particle_size = random.randint(2, 8)
                fire_intensity = 1.0 - fire_progress
                
                # Color varies from white-hot to red
                if fire_intensity > 0.7:
                    color = (255, 255, int(200 + 55 * fire_intensity))
                elif fire_intensity > 0.4:
                    color = (255, int(100 + 155 * fire_intensity), 0)
                else:
                    color = (int(100 + 155 * fire_intensity), 0, 0)
                
                alpha = int(255 * fire_intensity)
                if alpha > 0:
                    particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                    pygame.draw.circle(particle_surf, (*color, alpha), 
                                     (particle_size, particle_size), particle_size)
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))
                    
        else:  # Smoke phase
            smoke_progress = (progress - 0.6) / 0.4
            smoke_intensity = 1.0 - smoke_progress
            
            # Rising smoke particles
            for i in range(10):
                angle = random.random() * 2 * math.pi
                base_distance = random.random() * 30
                particle_x = center_x + int(math.cos(angle) * base_distance)
                particle_y = center_y - int(smoke_progress * 80) + random.randint(-10, 10)
                
                smoke_size = random.randint(8, 20)
                smoke_alpha = int(120 * smoke_intensity * random.random())
                
                if smoke_alpha > 0:
                    smoke_color = (60, 60, 60)
                    smoke_surf = pygame.Surface((smoke_size * 2, smoke_size * 2), pygame.SRCALPHA)
                    pygame.draw.circle(smoke_surf, (*smoke_color, smoke_alpha),
                                     (smoke_size, smoke_size), smoke_size)
                    surface.blit(smoke_surf, (particle_x - smoke_size, particle_y - smoke_size))

    def draw_enhanced_explosion_ray(self, surface, explosion, progress):
        """Draw enhanced explosion ray with realistic propagation"""
        if progress > 1.0:
            return

        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2
        distance = explosion.get('distance', 1)
        intensity = explosion.get('intensity', 1.0)
        bomb_type = explosion.get('bomb_type', 'normal_bomb')

        # Enhanced ray visualization
        ray_intensity = intensity * (1.0 - progress * 0.7)
        ray_size = int(TILE_SIZE * 0.9 * ray_intensity)

        if ray_size > 0:
            # Type-specific effects
            if bomb_type == 'remote_bomb':
                base_color = COLORS['TEXT_CYAN']
            elif bomb_type == 'freeze_bomb':
                base_color = COLORS['FREEZE_COLOR']
            else:
                base_color = COLORS['EXPLOSION_MIDDLE']

            # Multi-layer explosion ray
            for layer in range(3):
                layer_size = max(1, ray_size - layer * 6)
                layer_alpha = int(200 * ray_intensity * (1 - layer * 0.3))
                
                if layer == 0:
                    color = COLORS['EXPLOSION_CORE']
                elif layer == 1:
                    color = base_color
                else:
                    color = COLORS['EXPLOSION_OUTER']

                ray_surf = pygame.Surface((layer_size * 2, layer_size * 2), pygame.SRCALPHA)
                pygame.draw.circle(ray_surf, (*color, layer_alpha), 
                                 (layer_size, layer_size), layer_size)
                surface.blit(ray_surf, (center_x - layer_size, center_y - layer_size))

            # Sparks and debris
            if progress < 0.5:
                for i in range(distance * 2):
                    if random.random() < 0.6:
                        spark_angle = random.random() * 2 * math.pi
                        spark_distance = random.randint(ray_size, ray_size + 15)
                        spark_x = center_x + int(math.cos(spark_angle) * spark_distance)
                        spark_y = center_y + int(math.sin(spark_angle) * spark_distance)
                        
                        spark_size = random.randint(1, 3)
                        pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], 
                                         (spark_x, spark_y), spark_size)

    def draw_json_coordinate_explosion(self, surface, explosion, progress):
        """Draw explosion from JSON coordinate event"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2
        explosion_type = explosion.get('explosion_type', 'standard')

        # Pulsing explosion with type-specific effects
        pulse_size = int(30 * (1 - progress) * math.sin(progress * math.pi))
        
        if pulse_size > 0:
            # Color based on explosion type
            if explosion_type == 'ice':
                color = COLORS['FREEZE_COLOR']
            elif explosion_type == 'remote':
                color = COLORS['TEXT_CYAN']
            else:
                color = COLORS['EXPLOSION_MIDDLE']

            explosion_surf = pygame.Surface((pulse_size * 2, pulse_size * 2), pygame.SRCALPHA)
            alpha = int(200 * (1 - progress))
            pygame.draw.circle(explosion_surf, (*color, alpha), 
                             (pulse_size, pulse_size), pulse_size)
            surface.blit(explosion_surf, (center_x - pulse_size, center_y - pulse_size))

    def draw_standard_explosion(self, surface, explosion, progress):
        """Draw standard explosion effect"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2
        intensity = explosion.get('intensity', 1.0)

        explosion_size = int(25 * intensity * (1 - progress))
        if explosion_size > 0:
            alpha = int(150 * (1 - progress))
            explosion_surf = pygame.Surface((explosion_size * 2, explosion_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_MIDDLE'], alpha),
                             (explosion_size, explosion_size), explosion_size)
            surface.blit(explosion_surf, (center_x - explosion_size, center_y - explosion_size))

    # Enhanced game effects drawing
    def draw_all_enhanced_game_effects(self, surface):
        """Draw all enhanced game effects with improved visuals"""
        for effect in self.game_effects:
            effect_type = effect.get('type', 'unknown')
            
            if effect_type == 'bomb_kick_enhanced':
                self.draw_enhanced_bomb_kick_effect(surface, effect)
            elif effect_type == 'speed_boost_enhanced':
                self.draw_enhanced_speed_boost_effect(surface, effect)
            elif effect_type == 'speed_particle':
                self.draw_speed_particle_effect(surface, effect)
            elif effect_type == 'kick_particle':
                self.draw_kick_particle_effect(surface, effect)
            elif effect_type == 'player_death_enhanced':
                self.draw_enhanced_player_death_effect(surface, effect)
            elif effect_type == 'death_particle':
                self.draw_death_particle_effect(surface, effect)
            elif effect_type == 'damage_enhanced':
                self.draw_enhanced_damage_effect(surface, effect)
            elif effect_type == 'healing_enhanced':
                self.draw_enhanced_healing_effect(surface, effect)
            elif effect_type == 'screen_flash':
                self.draw_screen_flash_effect(surface, effect)

    def draw_enhanced_bomb_kick_effect(self, surface, effect):
        """Draw enhanced bomb kick effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        bomb_type = effect.get('bomb_type', 'normal_bomb')

        # Impact burst with type-specific colors
        burst_size = int(35 * progress)
        alpha = int(150 * (1 - progress))

        if burst_size > 0 and alpha > 0:
            # Type-specific colors
            if bomb_type == 'remote_bomb':
                burst_color = COLORS['TEXT_CYAN']
            elif bomb_type == 'freeze_bomb':
                burst_color = COLORS['FREEZE_COLOR']
            else:
                burst_color = (255, 200, 100)

            # Starburst pattern
            for angle in range(0, 360, 30):
                end_x = center_x + int(burst_size * math.cos(math.radians(angle)))
                end_y = center_y + int(burst_size * math.sin(math.radians(angle)))
                
                line_width = max(1, int(4 * (1 - progress)))
                pygame.draw.line(surface, burst_color, (center_x, center_y), (end_x, end_y), line_width)

    def draw_enhanced_speed_boost_effect(self, surface, effect):
        """Draw enhanced speed boost effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        speed = effect.get('speed', 1)

        # Speed aura
        aura_size = int(40 * (1 - progress) * (speed / 4))
        
        if aura_size > 0:
            aura_alpha = int(120 * (1 - progress))
            speed_color = COLORS['SPEED_BOOST_COLOR']

            aura_surf = pygame.Surface((aura_size * 2, aura_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(aura_surf, (*speed_color, aura_alpha), 
                             (aura_size, aura_size), aura_size)
            surface.blit(aura_surf, (center_x - aura_size, center_y - aura_size))

    def draw_speed_particle_effect(self, surface, effect):
        """Draw speed particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        direction = effect.get('direction', 'north')
        
        # Direction vectors
        directions = {'north': (0, -1), 'south': (0, 1), 'east': (1, 0), 'west': (-1, 0)}
        dx, dy = directions.get(direction, (0, 0))
        
        # Particle trail
        particle_distance = int(progress * 50)
        particle_x = center_x + dx * particle_distance
        particle_y = center_y + dy * particle_distance
        
        particle_size = max(1, int(5 * (1 - progress)))
        alpha = int(200 * (1 - progress))
        
        if alpha > 0:
            particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(particle_surf, (*COLORS['SPEED_BOOST_COLOR'], alpha),
                             (particle_size, particle_size), particle_size)
            surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_kick_particle_effect(self, surface, effect):
        """Draw kick particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        angle = effect.get('angle', 0)
        
        # Particle movement
        distance = int(progress * 30)
        particle_x = center_x + int(distance * math.cos(math.radians(angle)))
        particle_y = center_y + int(distance * math.sin(math.radians(angle)))
        
        particle_size = max(1, int(4 * (1 - progress)))
        alpha = int(150 * (1 - progress))
        
        if alpha > 0:
            pygame.draw.circle(surface, (*COLORS['TEXT_ORANGE'], alpha), 
                             (particle_x, particle_y), particle_size)

    def draw_enhanced_player_death_effect(self, surface, effect):
        """Draw enhanced player death effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        player_id = effect.get('player_id', 1)

        # Death colors
        dead_colors = {1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                      3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']}
        death_color = dead_colors.get(player_id, COLORS['PLAYER_1_DEAD'])

        # Death spiral effect
        if progress < 0.7:
            spiral_progress = progress / 0.7
            spiral_size = int(50 * spiral_progress)
            
            for i in range(8):
                angle = (spiral_progress * 720 + i * 45) % 360
                particle_x = center_x + int(spiral_size * math.cos(math.radians(angle)))
                particle_y = center_y + int(spiral_size * math.sin(math.radians(angle)))
                
                particle_alpha = int(200 * (1 - spiral_progress))
                particle_size = max(1, int(8 * (1 - spiral_progress)))
                
                if particle_alpha > 0:
                    particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                    pygame.draw.circle(particle_surf, (*death_color, particle_alpha),
                                     (particle_size, particle_size), particle_size)
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_death_particle_effect(self, surface, effect):
        """Draw death particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        angle = effect.get('angle', 0)
        player_id = effect.get('player_id', 1)

        # Death colors
        dead_colors = {1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                      3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']}
        death_color = dead_colors.get(player_id, COLORS['PLAYER_1_DEAD'])

        # Particle movement with gravity
        distance = int(progress * 40)
        gravity_offset = int(progress * progress * 20)  # Quadratic fall
        
        particle_x = center_x + int(distance * math.cos(math.radians(angle)))
        particle_y = center_y + int(distance * math.sin(math.radians(angle))) + gravity_offset
        
        particle_size = max(1, int(6 * (1 - progress)))
        alpha = int(180 * (1 - progress))
        
        if alpha > 0:
            particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(particle_surf, (*death_color, alpha),
                             (particle_size, particle_size), particle_size)
            surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_enhanced_damage_effect(self, surface, effect):
        """Draw enhanced damage effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        damage = effect.get('damage', 1)

        # Damage flash
        flash_intensity = math.sin(progress * math.pi * 8) * (1 - progress)
        if flash_intensity > 0:
            flash_size = int(TILE_SIZE * 1.2)
            flash_alpha = int(150 * flash_intensity)
            
            flash_surf = pygame.Surface((flash_size, flash_size), pygame.SRCALPHA)
            pygame.draw.rect(flash_surf, (*COLORS['TEXT_RED'], flash_alpha), 
                           (0, 0, flash_size, flash_size))
            surface.blit(flash_surf, (center_x - flash_size//2, center_y - flash_size//2))

        # Damage number
        if progress < 0.5:
            damage_text = f"-{damage}"
            text_y = center_y - int(progress * 40)
            text_alpha = int(255 * (1 - progress * 2))
            
            damage_surface = self.font.render(damage_text, True, (*COLORS['TEXT_RED'], text_alpha))
            text_rect = damage_surface.get_rect(center=(center_x, text_y))
            surface.blit(damage_surface, text_rect)

    def draw_enhanced_healing_effect(self, surface, effect):
        """Draw enhanced healing effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2
        heal_amount = effect.get('heal_amount', 1)

        # Healing glow
        glow_size = int(40 * (1 - progress))
        if glow_size > 0:
            glow_alpha = int(100 * (1 - progress))
            glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (*COLORS['TEXT_GREEN'], glow_alpha), 
                             (glow_size, glow_size), glow_size)
            surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

        # Healing number
        if progress < 0.6:
            heal_text = f"+{heal_amount}"
            text_y = center_y - int(progress * 50)
            text_alpha = int(255 * (1 - progress / 0.6))
            
            heal_surface = self.font.render(heal_text, True, (*COLORS['TEXT_GREEN'], text_alpha))
            text_rect = heal_surface.get_rect(center=(center_x, text_y))
            surface.blit(heal_surface, text_rect)

    def draw_screen_flash_effect(self, surface, effect):
        """Draw screen flash effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        intensity = effect['intensity'] * (1 - progress)
        color = effect['color']
        alpha = int(255 * intensity)
        
        if alpha > 0:
            flash_surf = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE), pygame.SRCALPHA)
            flash_color = (*color, alpha)
            pygame.draw.rect(flash_surf, flash_color, (0, 0, MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
            surface.blit(flash_surf, (0, 0))

    def draw_enhanced_powerup_animation(self, surface, animation):
        """Draw enhanced power-up animations"""
        elapsed = self.time - animation['start_time']
        progress = elapsed / animation['duration']
        
        if progress >= 1.0:
            return

        center_x = animation['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = animation['x'] * TILE_SIZE + TILE_SIZE // 2
        powerup_type = animation['powerup']

        if animation['type'] == 'pickup_enhanced':
            # Rising sparkles
            powerup_colors = {
                'move_speed': COLORS['TEXT_CYAN'],
                'remote_ignition': COLORS['TEXT_ORANGE'],
                'kick_bomb': (255, 100, 255)
            }
            sparkle_color = powerup_colors.get(powerup_type, COLORS['POWERUP_CORE'])
            
            for i in range(12):
                sparkle_y = center_y - int(progress * 100) - i * 8
                sparkle_x = center_x + int(math.sin(self.time * 6 + i) * 25)
                
                if sparkle_y > center_y - 120:
                    alpha = int(255 * (1 - progress))
                    sparkle_size = max(1, int(5 * (1 - progress)))
                    
                    sparkle_surf = pygame.Surface((sparkle_size * 2, sparkle_size * 2), pygame.SRCALPHA)
                    pygame.draw.circle(sparkle_surf, (*sparkle_color, alpha), 
                                     (sparkle_size, sparkle_size), sparkle_size)
                    surface.blit(sparkle_surf, (sparkle_x - sparkle_size, sparkle_y - sparkle_size))
        
        elif animation['type'] == 'spawn_enhanced':
            # Expanding ring
            ring_size = int(progress * 40)
            ring_alpha = int(200 * (1 - progress))
            
            if ring_size > 0 and ring_alpha > 0:
                ring_surf = pygame.Surface((ring_size * 2, ring_size * 2), pygame.SRCALPHA)
                pygame.draw.circle(ring_surf, (*COLORS['POWERUP_GLOW'], ring_alpha), 
                                 (ring_size, ring_size), ring_size, 3)
                surface.blit(ring_surf, (center_x - ring_size, center_y - ring_size))

    # Panel drawing methods
    def draw_enhanced_player_stats_panel(self):
        """Draw enhanced player statistics panel"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel border
        pygame.draw.rect(self.player_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Panel title
        title_text = "PLAYERS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.player_panel_surface.blit(title_surface, (12, 12))

        # Backend timing info
        timing_y = 45
        if self.current_game_state.backend_timing:
            timing_text = f"Backend Sync: {self.backend_constants.get('tick_delay', TICK_DELAY)}ms"
            timing_surface = self.mini_font.render(timing_text, True, COLORS['TEXT_CYAN'])
            self.player_panel_surface.blit(timing_surface, (12, timing_y))

        # Draw each player's stats
        start_y = 70
        player_height = (MAP_SIZE * TILE_SIZE - 90) // 4

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            player_data = self.current_game_state.players.get(player_id)
            self.draw_enhanced_single_player_stats(self.player_panel_surface, player_id, y_pos, player_height, player_data)

        # Blit to virtual surface
        self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))

    def draw_enhanced_single_player_stats(self, surface, player_id, y_pos, height, player_data):
        """Draw individual player statistics"""
        # Determine if player is dead
        is_dead = player_id in self.current_game_state.dead_players
        
        # Choose colors based on death state
        if is_dead:
            player_colors = {1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                           3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']}
            text_color = COLORS['TEXT_GREY']
            status_text = "💀 DEAD"
            status_color = COLORS['TEXT_RED']
        else:
            player_colors = {1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                           3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']}
            text_color = COLORS['TEXT_WHITE']
            if player_data:
                status_text = "✅ ALIVE"
                status_color = COLORS['TEXT_GREEN']
                status_text += f" at ({player_data.x}, {player_data.y})"
            else:
                status_text = "⏳ WAITING"
                status_color = COLORS['TEXT_ORANGE']

        player_color = player_colors.get(player_id, COLORS['PLAYER_1'])

        # Background
        bg_rect = pygame.Rect(10, y_pos, PLAYER_PANEL_WIDTH - 20, height - 10)
        bg_alpha = 30 if is_dead else 60
        self.draw_gradient_rect(surface, (*player_color, bg_alpha), (*player_color, bg_alpha // 2), bg_rect)
        pygame.draw.rect(surface, player_color, bg_rect, 2)

        # Player avatar
        avatar_x, avatar_y = 25, y_pos + 15
        self.draw_enhanced_mini_player(surface, avatar_x, avatar_y, player_id, is_dead=is_dead)

        # Player info
        player_text = f"PLAYER {player_id}"
        player_surface = self.font.render(player_text, True, text_color)
        surface.blit(player_surface, (avatar_x + 35, avatar_y + 5))
        
        status_surface = self.small_font.render(status_text, True, status_color)
        surface.blit(status_surface, (avatar_x + 35, avatar_y + 25))

        # Statistics
        stats_y = y_pos + 50
        
        # Health
        health = player_data.health if player_data else 0 if is_dead else 3
        health_text = "Health:"
        health_surface = self.small_font.render(health_text, True, COLORS['TEXT_RED'] if not is_dead else COLORS['TEXT_GREY'])
        surface.blit(health_surface, (avatar_x + 35, stats_y))
        
        # Draw hearts
        for i in range(max(health, 0)):
            heart_x = avatar_x + 90 + i * 15
            self.draw_enhanced_mini_heart(surface, heart_x, stats_y + 6, COLORS['TEXT_RED'], is_dead)

        # Speed
        if player_data:
            speed_text = f"Speed: {player_data.speed}"
            speed_color = COLORS['TEXT_CYAN'] if not is_dead else COLORS['TEXT_GREY']
            speed_surface = self.small_font.render(speed_text, True, speed_color)
            surface.blit(speed_surface, (avatar_x + 35, stats_y + 20))

        # Timers
        if player_data and not is_dead:
            timer_y = stats_y + 40
            if player_data.timers.movement_timer > 0:
                timer_text = f"Moving: {player_data.timers.movement_timer}ms"
                timer_surface = self.mini_font.render(timer_text, True, COLORS['TEXT_CYAN'])
                surface.blit(timer_surface, (avatar_x + 35, timer_y))
                timer_y += 12
            
            if player_data.timers.immunity_timer > 0:
                timer_text = f"Immune: {player_data.timers.immunity_timer}ms"
                timer_surface = self.mini_font.render(timer_text, True, COLORS['IMMUNITY_GLOW'])
                surface.blit(timer_surface, (avatar_x + 35, timer_y))

    def draw_enhanced_mini_player(self, surface, x, y, player_num, scale=1.0, is_dead=False):
        """Draw mini player avatar"""
        if is_dead:
            player_colors = {1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                           3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']}
            skin_color = COLORS['SKIN_DEAD']
        else:
            player_colors = {1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                           3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']}
            skin_color = COLORS['SKIN']

        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])
        size = int(18 * scale)

        # Body
        body_rect = pygame.Rect(x - size // 2, y, size, int(size * 1.3))
        pygame.draw.rect(surface, outfit_color, body_rect)

        # Head
        head_y = y - size // 2
        pygame.draw.circle(surface, skin_color, (x, head_y), size // 2)

        # Eyes
        if is_dead:
            # X eyes for dead players
            pygame.draw.line(surface, (100, 100, 100), (x - 5, head_y - 3), (x - 3, head_y - 1), 2)
            pygame.draw.line(surface, (100, 100, 100), (x - 3, head_y - 3), (x - 5, head_y - 1), 2)
            pygame.draw.line(surface, (100, 100, 100), (x + 3, head_y - 3), (x + 5, head_y - 1), 2)
            pygame.draw.line(surface, (100, 100, 100), (x + 5, head_y - 3), (x + 3, head_y - 1), 2)
        else:
            # Normal eyes
            pygame.draw.circle(surface, (0, 0, 0), (x - 3, head_y - 2), 1)
            pygame.draw.circle(surface, (0, 0, 0), (x + 3, head_y - 2), 1)

        # Player number badge
        badge_text = str(player_num)
        badge_surface = self.mini_font.render(badge_text, True, (0, 0, 0))
        badge_rect = pygame.Rect(x - 8, y + int(size * 1.3) + 3, 16, 10)
        pygame.draw.rect(surface, (255, 255, 255), badge_rect)
        pygame.draw.rect(surface, (0, 0, 0), badge_rect, 1)
        surface.blit(badge_surface, (x - 4, y + int(size * 1.3) + 1))

    def draw_enhanced_mini_heart(self, surface, x, y, color, is_dead=False):
        """Draw mini heart"""
        size = 5 if is_dead else 7
        alpha = 120 if is_dead else 255
        
        # Heart shape
        pygame.draw.circle(surface, (*color[:3], alpha), (x - 2, y - 1), 2)
        pygame.draw.circle(surface, (*color[:3], alpha), (x + 2, y - 1), 2)
        points = [(x - 3, y), (x + 3, y), (x, y + 5)]
        if len(points) >= 3:
            pygame.draw.polygon(surface, (*color[:3], alpha), points)

    def draw_enhanced_timer_panel(self):
        """Draw timer information panel"""
        self.timer_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel border
        pygame.draw.rect(self.timer_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Title
        title_text = "TIMERS"
        title_surface = self.font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.timer_panel_surface.blit(title_surface, (10, 10))

        # Backend info
        sync_y = 40
        backend_info = [
            f"Tick: {self.backend_constants.get('tick_delay', TICK_DELAY)}ms",
            f"Move: {self.backend_constants.get('tile_move', TILE_MOVE_BASE)}ms"
        ]
        
        for i, info in enumerate(backend_info):
            info_surface = self.mini_font.render(info, True, COLORS['TEXT_CYAN'])
            self.timer_panel_surface.blit(info_surface, (10, sync_y + i * 15))

        # Timer displays
        timer_y = sync_y + 40
        
        # Movement timers
        for player_id, timer_ms in self.movement_timers.items():
            if timer_ms > 0:
                timer_text = f"P{player_id} Move: {timer_ms}ms"
                timer_surface = self.mini_font.render(timer_text, True, COLORS['TEXT_CYAN'])
                self.timer_panel_surface.blit(timer_surface, (10, timer_y))
                timer_y += 15

        # Immunity timers
        for player_id, timer_ms in self.immunity_timers.items():
            if timer_ms > 0:
                timer_text = f"P{player_id} Immune: {timer_ms}ms"
                timer_surface = self.mini_font.render(timer_text, True, COLORS['IMMUNITY_GLOW'])
                self.timer_panel_surface.blit(timer_surface, (10, timer_y))
                timer_y += 15

        # Performance info
        perf_y = MAP_SIZE * TILE_SIZE - 60
        fps_text = f"FPS: {self.current_fps:.1f}"
        fps_surface = self.mini_font.render(fps_text, True, COLORS['TEXT_GREEN'])
        self.timer_panel_surface.blit(fps_surface, (10, perf_y))

        # Blit to virtual surface
        self.virtual_surface.blit(self.timer_panel_surface, (TIMER_OFFSET_X, MAP_OFFSET_Y))

    def draw_enhanced_powerups_panel(self):
        """Draw power-ups information panel"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel border
        pygame.draw.rect(self.powerup_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, WINDOW_WIDTH, POWERUP_PANEL_HEIGHT), 2)

        # Title
        title_text = "POWER-UPS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Power-up info
        powerups = [
            ("⚡", "SPEED BOOST", COLORS['TEXT_CYAN']),
            ("📡", "REMOTE", COLORS['TEXT_ORANGE']),
            ("🏭", "FACTORY", COLORS['TEXT_GOLD']),
            ("👢", "KICK", (255, 100, 255)),
            ("👻", "GHOST", (200, 200, 255)),
            ("💣", "BOMBS", (255, 150, 100)),
            ("💥", "BLAST", (255, 100, 100)),
            ("❤️", "LIFE", (255, 100, 150)),
            ("🧊", "FREEZE", (150, 200, 255))
        ]

        start_x = 30
        start_y = 55
        item_width = (WINDOW_WIDTH - 60) // 3
        
        for i, (icon, name, color) in enumerate(powerups):
            row = i // 3
            col = i % 3
            x = start_x + col * item_width
            y = start_y + row * 25
            
            if y < POWERUP_PANEL_HEIGHT - 20:
                # Icon and name
                icon_surface = self.font.render(icon, True, color)
                name_surface = self.small_font.render(name, True, color)
                
                self.powerup_panel_surface.blit(icon_surface, (x, y))
                self.powerup_panel_surface.blit(name_surface, (x + 30, y + 3))

        # Blit to virtual surface
        self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

    # Event handling system
    def handle_enhanced_events(self):
        """Enhanced event handling"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
                elif event.key == pygame.K_r:
                    print("🔄 Manual refresh requested")
                elif event.key == pygame.K_h:
                    print("🔧 JSON Game Visualizer Controls:")
                    print("   ESC - Exit")
                    print("   R - Request refresh")
                    print("   H - This help")
                    print("   Mouse - Click tiles to inspect")
                    
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    self.handle_enhanced_mouse_click(event.pos)
        return True

    def handle_window_resize(self, new_width, new_height):
        """Handle window resizing"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)
        self.scale_factor = min(self.current_width / WINDOW_WIDTH, self.current_height / WINDOW_HEIGHT)
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)
        print(f"🔄 Window resized to {self.current_width}x{self.current_height}")

    def handle_enhanced_mouse_click(self, mouse_pos):
        """Handle mouse clicks for tile inspection"""
        mouse_x, mouse_y = mouse_pos

        # Calculate virtual coordinates
        scaled_width = int(WINDOW_WIDTH * self.scale_factor)
        scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
        x_offset = (self.current_width - scaled_width) // 2
        y_offset = (self.current_height - scaled_height) // 2

        virtual_x = (mouse_x - max(0, x_offset)) / self.scale_factor
        virtual_y = (mouse_y - max(0, y_offset)) / self.scale_factor

        # Check if click is within map area
        if (MAP_OFFSET_X <= virtual_x < MAP_OFFSET_X + MAP_SIZE * TILE_SIZE and
                MAP_OFFSET_Y <= virtual_y < MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE):

            map_mouse_x = virtual_x - MAP_OFFSET_X
            map_mouse_y = virtual_y - MAP_OFFSET_Y

            tile_x = int(map_mouse_y // TILE_SIZE)
            tile_y = int(map_mouse_x // TILE_SIZE)

            if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                self.selected_tile = (tile_x, tile_y)
                self.inspect_enhanced_tile(tile_x, tile_y)

    def inspect_enhanced_tile(self, tile_x, tile_y):
        """Enhanced tile inspection"""
        tile_type = self.current_game_state.tiles[tile_x][tile_y]
        powerup = self.current_game_state.powerups[tile_x][tile_y]

        tile_names = {0: 'FREE_SPACE', 1: 'WOODEN_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL'}
        tile_name = tile_names.get(tile_type, f'UNKNOWN_{tile_type}')

        print(f"\n🎯 JSON Tile Inspection at ({tile_x}, {tile_y}):")
        print(f"   Tile: {tile_name}")
        print(f"   Power-up: {powerup}")

        # Check for players
        players_here = [p for p in self.current_game_state.players.values() 
                       if p.x == tile_x and p.y == tile_y]
        for player in players_here:
            print(f"   Player {player.player_id}: Health={player.health}, Speed={player.speed}")

        # Check for bombs
        bombs_here = [b for b in self.current_game_state.bombs.values() 
                     if b.x == tile_x and b.y == tile_y]
        for bomb in bombs_here:
            print(f"   Bomb: Type={bomb.bomb_type}, Status={bomb.status}, Timer={bomb.timer}")

    def update_performance_tracking(self):
        """Update FPS tracking"""
        self.fps_counter += 1
        current_time = time.time()
        
        if current_time - self.last_fps_time >= 1.0:
            self.current_fps = self.fps_counter / (current_time - self.last_fps_time)
            self.fps_counter = 0
            self.last_fps_time = current_time

    def draw_complete_enhanced_visualization(self):
        """Draw complete visualization"""
        if not self.map_initialized:
            return

        # Clear virtual surface
        self.virtual_surface.fill(COLORS['BACKGROUND'])

        # Draw all components
        self.draw_enhanced_map()
        self.draw_enhanced_player_stats_panel()
        self.draw_enhanced_timer_panel()
        self.draw_enhanced_powerups_panel()

        # Scale and display
        if self.scale_factor != 1.0:
            scaled_width = int(WINDOW_WIDTH * self.scale_factor)
            scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
            scaled_surface = pygame.transform.smoothscale(self.virtual_surface, (scaled_width, scaled_height))
        else:
            scaled_surface = self.virtual_surface

        # Center and display
        self.screen.fill(COLORS['BACKGROUND'])
        x_offset = (self.current_width - scaled_surface.get_width()) // 2
        y_offset = (self.current_height - scaled_surface.get_height()) // 2
        self.screen.blit(scaled_surface, (max(0, x_offset), max(0, y_offset)))

        # Status display
        self.draw_enhanced_status_display()

    def draw_enhanced_status_display(self):
        """Draw status information"""
        status_y = 10
        
        if self.waiting_for_initial_map:
            status_text = "⏳ Waiting for JSON map data..."
            color = COLORS['TEXT_ORANGE']
        else:
            dead_count = len(self.current_game_state.dead_players)
            active_animations = len(self.player_animations) + len(self.bomb_animations) + len(self.explosion_animations)
            
            status_text = f"🔄 JSON Live Updates | Dead: {dead_count} | Animations: {active_animations} | FPS: {self.current_fps:.1f}"
            color = COLORS['TEXT_CYAN']

        status_surface = self.small_font.render(status_text, True, color)
        status_rect = status_surface.get_rect(topleft=(10, status_y))
        
        # Background
        bg_rect = status_rect.inflate(8, 4)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        
        self.screen.blit(bg_surf, bg_rect)
        self.screen.blit(status_surface, status_rect)

    def run_enhanced_game_loop(self):
        """Main JSON game loop"""
        print("🎮 Enhanced JSON Playing with Fire 2 Visualizer Started!")
        print("📡 JSON Features:")
        print("   🔄 Real-time JSON communication with Erlang backend")
        print("   ⏱️ Complete timer visualization and synchronization")
        print("   🎰 Full FSM state tracking for bombs and players")
        print("   💀 Enhanced death detection and visualization")
        print("   💥 Real-time explosion events with coordinate tracking")
        print("   🎬 Advanced animation system with JSON integration")
        print("   📊 Performance monitoring and debugging tools")
        print("   🖱️ Interactive tile inspection")
        print("\nWaiting for JSON data from Erlang backend...")

        running = True
        while running:
            # Handle events
            running = self.handle_enhanced_events()

            # Read JSON data
            packets = self.read_port_data()
            if packets:
                self.handle_port_data(packets)

            # Update performance
            self.update_performance_tracking()

            # Draw visualization
            if self.map_initialized:
                self.draw_complete_enhanced_visualization()
            else:
                # Waiting screen
                self.screen.fill(COLORS['BACKGROUND'])
                
                waiting_text = "⏳ Waiting for JSON Map Data..."
                waiting_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                waiting_rect = waiting_surface.get_rect(center=(self.current_width // 2, self.current_height // 2))
                self.screen.blit(waiting_surface, waiting_rect)

                sub_text = "Enhanced JSON CN server will send complete backend state"
                sub_surface = self.small_font.render(sub_text, True, COLORS['TEXT_CYAN'])
                sub_rect = sub_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 30))
                self.screen.blit(sub_surface, sub_rect)

            # Update display
            pygame.display.flip()
            self.clock.tick(FPS)

        # Cleanup
        print("\n🛑 JSON Game Visualizer shutting down...")
        print(f"📊 Final Statistics:")
        print(f"   FPS: {self.current_fps:.1f}")
        print(f"   Dead players tracked: {len(self.current_game_state.dead_players)}")
        
        pygame.quit()
        sys.exit()


# Main execution
if __name__ == "__main__":
    try:
        print("🚀 Initializing Enhanced JSON Playing with Fire 2 Visualizer...")
        print("🔧 Complete JSON Backend Integration with Real-time Synchronization")
        print("=" * 80)
        
        visualizer = EnhancedGameVisualizer()
        visualizer.run_enhanced_game_loop()
        
    except KeyboardInterrupt:
        print("\n⏹️ Game interrupted by user")
        pygame.quit()
        sys.exit(0)
    except Exception as e:
        print(f"\n❌ Fatal error: {e}")
        import traceback
        traceback.print_exc()
        pygame.quit()
        sys.exit(1)
