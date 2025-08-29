import pygame
import sys
import math
import random
import time
import struct
import socket
import json
import threading
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

# Socket Configuration - Must match Erlang CN server
CN_SERVER_HOST = 'localhost'
CN_SERVER_PORT = 8080
SOCKET_TIMEOUT = 5.0
RECONNECT_DELAY = 2.0
MAX_RECONNECT_ATTEMPTS = 10

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

class SocketManager:
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.socket: Optional[socket.socket] = None
        self.connected = False
        self.running = True
        self.receive_buffer = b''
        self.lock = threading.Lock()
        self.message_queue = []
        self.connection_attempts = 0
        self.last_connect_time = 0

    def connect(self) -> bool:
        """Establish connection to CN server"""
        try:
            if self.socket:
                self.close()
                
            print(f"🔌 Connecting to CN server at {self.host}:{self.port}...")
            
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(SOCKET_TIMEOUT)
            
            # Enable keepalive
            self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            
            # Disable Nagle's algorithm for lower latency
            self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            
            self.socket.connect((self.host, self.port))
            self.connected = True
            self.connection_attempts = 0
            
            print("✅ Connected to CN server successfully!")
            return True
            
        except socket.timeout:
            print("❌ Connection timeout - CN server may not be running")
            self.connected = False
            return False
        except ConnectionRefused:
            print("❌ Connection refused - CN server not accepting connections")
            self.connected = False
            return False
        except Exception as e:
            print(f"❌ Connection failed: {e}")
            self.connected = False
            return False

    def close(self):
        """Close socket connection"""
        if self.socket:
            try:
                self.socket.close()
            except:
                pass
            self.socket = None
        self.connected = False

    def receive_messages(self):
        """Receive and parse messages in background thread"""
        while self.running and self.connected:
            try:
                if not self.socket:
                    break
                    
                # Receive data
                data = self.socket.recv(4096)
                if not data:
                    print("⚠️ CN server disconnected")
                    self.connected = False
                    break
                    
                self.receive_buffer += data
                
                # Process complete packets
                while len(self.receive_buffer) >= 4:
                    # Read packet length (big-endian 32-bit)
                    packet_length = struct.unpack('>I', self.receive_buffer[:4])[0]
                    
                    if len(self.receive_buffer) >= 4 + packet_length:
                        # Complete packet available
                        packet_data = self.receive_buffer[4:4 + packet_length]
                        self.receive_buffer = self.receive_buffer[4 + packet_length:]
                        
                        # Parse JSON message
                        try:
                            json_string = packet_data.decode('utf-8')
                            message = json.loads(json_string)
                            
                            # Add to queue thread-safely
                            with self.lock:
                                self.message_queue.append(message)
                                
                        except (UnicodeDecodeError, json.JSONDecodeError) as e:
                            print(f"❌ Failed to parse message: {e}")
                    else:
                        # Wait for more data
                        break
                        
            except socket.timeout:
                # Normal timeout, continue
                continue
            except Exception as e:
                print(f"❌ Socket error: {e}")
                self.connected = False
                break

    def get_messages(self) -> List[dict]:
        """Get all pending messages"""
        with self.lock:
            messages = self.message_queue.copy()
            self.message_queue.clear()
            return messages

    def send_message(self, message: dict) -> bool:
        """Send JSON message to server"""
        if not self.connected or not self.socket:
            return False
            
        try:
            json_data = json.dumps(message, ensure_ascii=False)
            json_bytes = json_data.encode('utf-8')
            
            # Send length prefix + data
            length_prefix = struct.pack('>I', len(json_bytes))
            self.socket.sendall(length_prefix + json_bytes)
            return True
            
        except Exception as e:
            print(f"❌ Failed to send message: {e}")
            self.connected = False
            return False

    def attempt_reconnect(self) -> bool:
        """Attempt to reconnect with exponential backoff"""
        current_time = time.time()
        
        if (current_time - self.last_connect_time < RECONNECT_DELAY or 
            self.connection_attempts >= MAX_RECONNECT_ATTEMPTS):
            return False
            
        self.last_connect_time = current_time
        self.connection_attempts += 1
        
        print(f"🔄 Reconnection attempt {self.connection_attempts}/{MAX_RECONNECT_ATTEMPTS}")
        
        if self.connect():
            return True
            
        # Exponential backoff
        time.sleep(min(RECONNECT_DELAY * (2 ** (self.connection_attempts - 1)), 30))
        return False

class EnhancedSocketGameVisualizer:
    def __init__(self):
        # Enhanced window setup
        initial_width = min(WINDOW_WIDTH, 1200)
        initial_height = min(WINDOW_HEIGHT, 900)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Playing with Fire 2 - Socket Communication with CN Server")
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

        # Socket management
        self.socket_manager = SocketManager(CN_SERVER_HOST, CN_SERVER_PORT)
        self.receive_thread = None
        
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

        # Game state
        self.map_initialized = False
        self.waiting_for_initial_map = True
        self.connection_status = "Disconnected"
        
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
        self.last_message_time = 0
        self.message_count = 0

        print("🎮 Enhanced Socket Game Visualizer initialized")
        print(f"🔗 Target CN server: {CN_SERVER_HOST}:{CN_SERVER_PORT}")

    def connect_to_server(self) -> bool:
        """Connect to CN server and start receiving thread"""
        if self.socket_manager.connect():
            self.connection_status = "Connected"
            
            # Start receiving thread
            if self.receive_thread and self.receive_thread.is_alive():
                self.socket_manager.running = False
                self.receive_thread.join(timeout=1.0)
                
            self.socket_manager.running = True
            self.receive_thread = threading.Thread(target=self.socket_manager.receive_messages, daemon=True)
            self.receive_thread.start()
            
            # Send initial connection message
            self.socket_manager.send_message({
                "type": "client_connected",
                "client_type": "python_visualizer",
                "version": "1.0",
                "timestamp": int(time.time() * 1000)
            })
            
            return True
        else:
            self.connection_status = "Connection Failed"
            return False

    def handle_socket_messages(self):
        """Process all pending socket messages"""
        messages = self.socket_manager.get_messages()
        
        for message in messages:
            self.message_count += 1
            self.last_message_time = time.time()
            
            message_type = message.get('type', 'unknown')
            message_data = message.get('data', {})
            timestamp = message.get('timestamp', 0)
            
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
            elif message_type == 'death_event':
                self.handle_death_event(message_data)
            else:
                print(f"⚠️ Unknown message type: {message_type}")

    def process_map_update(self, map_data: dict) -> bool:
        """Process map update with enhanced backend timing information"""
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

            # Check for newly dead players
            for player_id, death_info in new_dead_players.items():
                player_id_int = int(player_id)
                if player_id_int not in self.current_game_state.dead_players:
                    self.create_enhanced_death_animation(player_id_int, death_info)
            
            # Update game state
            self.current_game_state.dead_players = {int(k): v for k, v in new_dead_players.items()}
            self.current_game_state.backend_timing = new_backend_timing
            self.current_game_state.update_time = time.time()

            # Parse the enhanced map data
            success = self.parse_game_state(grid_data)
            if success:
                # Set map as initialized if this is first successful parse
                if self.waiting_for_initial_map:
                    self.waiting_for_initial_map = False
                    self.map_initialized = True
                    print("✅ Initial map loaded! Now receiving real-time updates...")

                # Detect changes for enhanced animations
                if self.previous_game_state:
                    self.detect_game_changes(self.previous_game_state, self.current_game_state)

                return True
            return False
            
        except Exception as e:
            print(f"❌ Error processing map update: {e}")
            import traceback
            traceback.print_exc()
            return False

    def handle_movement_confirmation(self, confirmation_data: dict):
        """Handle movement confirmation with real backend timing"""
        entity_type = confirmation_data.get('entity_type', 'unknown')
        entity_data = confirmation_data.get('entity_data', {})

        if entity_type == 'player':
            self.handle_player_movement_confirmation(entity_data)
        elif entity_type == 'bomb':
            self.handle_bomb_movement_confirmation(entity_data)

    def handle_player_movement_confirmation(self, player_data: dict):
        """Handle player movement with real backend timing"""
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

    def handle_bomb_movement_confirmation(self, bomb_data: dict):
        """Handle bomb movement with FSM state information"""
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

    def handle_timer_update(self, timer_data: dict):
        """Handle real-time timer updates from backend"""
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

    def handle_fsm_update(self, fsm_data: dict):
        """Handle FSM state changes for bombs and players"""
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

    def handle_explosion_event(self, explosion_data: dict):
        """Handle real-time explosion events"""
        coordinates = explosion_data.get('coordinates', [])
        explosion_type = explosion_data.get('explosion_type', 'standard')
        timestamp = explosion_data.get('timestamp', time.time() * 1000)
        display_time = explosion_data.get('display_time', EXPLOSION_DISPLAY_TIME)
        
        # Create enhanced explosion sequence
        self.create_explosion_sequence(coordinates, explosion_type, timestamp, display_time)

    def handle_death_event(self, death_data: dict):
        """Handle death events"""
        player_id = int(death_data.get('player_id', 0))
        death_time = death_data.get('death_time', time.time() * 1000)
        local_gn = death_data.get('local_gn', 'unknown')
        last_known_state = death_data.get('last_known_state')
        
        # Create death animation
        death_info = (death_time, last_known_state, local_gn)
        self.create_enhanced_death_animation(player_id, death_info)
        
        # Add to dead players
        self.current_game_state.dead_players[player_id] = death_info

    def parse_game_state(self, json_grid: List) -> bool:
        """Parse complete game state from JSON grid data"""
        if not json_grid or not isinstance(json_grid, list):
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

                # Handle cell format
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
        self.current_game_state.players = new_players
        self.current_game_state.bombs = new_bombs
        self.current_game_state.explosions = new_explosions

        return True

    def parse_player_info(self, player_info, x: int, y: int) -> Optional[PlayerState]:
        """Parse player information"""
        try:
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
                # Enhanced format: [player_id, life, speed, direction, movement, movement_timer, immunity_timer, request_timer]
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
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_bomb_info(self, bomb_info, x: int, y: int) -> Optional[BombState]:
        """Parse bomb information"""
        try:
            if isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return BombState(
                    x=x, y=y, bomb_type='normal_bomb', timer=3000, owner=1,
                    radius=2, status='armed', ignited=False, movement=False,
                    direction='none', last_update=self.time
                )
            elif isinstance(bomb_info, (list, tuple)) and len(bomb_info) >= 7:
                # Format: [type, ignited, status, radius, owner, movement, direction]
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
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_explosion_info(self, explosion_info, x: int, y: int) -> Optional[ExplosionState]:
        """Parse explosion information"""
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
        except (ValueError, TypeError):
            pass
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

    def detect_game_changes(self, old_state: GameState, new_state: GameState):
        """Detect game changes and create appropriate animations"""
        # Detect player changes
        self.detect_player_changes(old_state.players, new_state.players)

        # Detect bomb lifecycle
        self.detect_bomb_lifecycle(old_state.bombs, new_state.bombs)

        # Detect explosion changes
        self.detect_explosion_changes(old_state.explosions, new_state.explosions)

    def detect_player_changes(self, old_players: Dict[int, PlayerState], new_players: Dict[int, PlayerState]):
        """Detect player changes"""
        for player_id, new_player in new_players.items():
            if player_id in old_players:
                old_player = old_players[player_id]

                # Position change - only create animation if not already confirmed
                if ((old_player.x, old_player.y) != (new_player.x, new_player.y) and
                        player_id not in self.player_animations):
                    self.create_walking_animation(
                        player_id, (old_player.x, old_player.y), (new_player.x, new_player.y),
                        new_player.direction, new_player.speed, new_player.timers
                    )

                # Health change
                if old_player.health != new_player.health:
                    if new_player.health < old_player.health:
                        self.create_damage_effect(player_id, new_player.x, new_player.y, 
                                                 old_player.health - new_player.health)

    def detect_bomb_lifecycle(self, old_bombs: Dict[tuple, BombState], new_bombs: Dict[tuple, BombState]):
        """Detect bomb lifecycle changes"""
        # New bombs placed
        for pos, bomb in new_bombs.items():
            if pos not in old_bombs:
                self.create_bomb_placement_animation(bomb.x, bomb.y, bomb)

        # Bombs that exploded or disappeared
        for pos, bomb in old_bombs.items():
            if pos not in new_bombs:
                self.create_bomb_explosion_sequence(bomb.x, bomb.y, bomb)

    def detect_explosion_changes(self, old_explosions: List[ExplosionState], new_explosions: List[ExplosionState]):
        """Detect explosion changes"""
        old_explosion_positions = {(e.x, e.y): e for e in old_explosions}
        new_explosion_positions = {(e.x, e.y): e for e in new_explosions}

        # New explosions
        for pos, explosion in new_explosion_positions.items():
            if pos not in old_explosion_positions:
                self.create_live_explosion_effect(explosion.x, explosion.y, explosion)

    # Animation Creation Methods
    def create_walking_animation(self, player_id: int, old_pos: tuple, new_pos: tuple, 
                               direction: str, speed: int, timers: PlayerTimers):
        """Create walking animation with backend timing"""
        if (player_id in self.player_animations and 
                self.player_animations[player_id].get('confirmed', False)):
            return

        # Calculate duration using backend constants
        total_duration = self.backend_constants['tile_move'] - (speed - 1) * self.backend_constants['ms_reduction']
        actual_duration = total_duration / 1000.0

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

    def create_bomb_placement_animation(self, x: int, y: int, bomb_data: BombState):
        """Create bomb placement animation"""
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

        self.camera_shake = 0.1

    def create_bomb_explosion_sequence(self, x: int, y: int, bomb_data: BombState):
        """Create bomb explosion sequence"""
        # Central explosion
        self.explosion_animations.append({
            'type': 'bomb_center',
            'x': x, 'y': y,
            'radius': bomb_data.radius,
            'bomb_type': bomb_data.bomb_type,
            'owner': bomb_data.owner,
            'start_time': self.time,
            'duration': 2.0,
            'active': True
        })

        # Camera shake
        shake_intensity = min(1.0, bomb_data.radius * 0.3)
        self.camera_shake = shake_intensity

    def create_explosion_sequence(self, coordinates: List[tuple], explosion_type: str, timestamp: float, display_time: int):
        """Create explosion sequence for multiple coordinates"""
        for i, coord in enumerate(coordinates):
            if isinstance(coord, list) and len(coord) >= 2:
                x, y = coord[0], coord[1]
                self.explosion_animations.append({
                    'type': 'coordinate_explosion',
                    'x': x, 'y': y,
                    'explosion_type': explosion_type,
                    'timestamp': timestamp,
                    'display_time': display_time,
                    'start_time': self.time + i * 0.02,
                    'duration': display_time / 1000.0,
                    'active': True
                })

    def create_enhanced_speed_boost_effect(self, player_id: int, x: int, y: int, speed: int, 
                                         direction: str, immunity_timer: int):
        """Create speed boost effect"""
        self.game_effects.append({
            'type': 'speed_boost',
            'player_id': player_id,
            'x': x, 'y': y,
            'speed': speed,
            'direction': direction,
            'immunity_timer': immunity_timer,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_enhanced_bomb_kick_effect(self, x: int, y: int, direction: str, kicker: int, bomb_type: str):
        """Create bomb kick effect"""
        self.game_effects.append({
            'type': 'bomb_kick',
            'x': x, 'y': y,
            'direction': direction,
            'kicker': kicker,
            'bomb_type': bomb_type,
            'start_time': self.time,
            'duration': 0.6,
            'active': True
        })

    def create_immunity_effect(self, player_id: int, x: int, y: int, immunity_timer: int):
        """Create immunity effect"""
        immunity_duration = immunity_timer / 1000.0
        
        self.status_effects[player_id] = {
            'type': 'immunity',
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': immunity_duration,
            'timer': immunity_timer,
            'intensity': min(1.0, immunity_timer / self.backend_constants['immunity_time'])
        }

    def create_enhanced_death_animation(self, player_id: int, death_info: tuple):
        """Create death animation"""
        death_time, last_known_state, local_gn = death_info
        
        if last_known_state:
            if isinstance(last_known_state, dict):
                pos = last_known_state.get('position', [0, 0])
            else:
                pos = getattr(last_known_state, 'position', [0, 0])
                
            x, y = pos if isinstance(pos, list) and len(pos) >= 2 else [0, 0]
            
            self.game_effects.append({
                'type': 'player_death',
                'player_id': player_id,
                'x': x, 'y': y,
                'death_time': death_time,
                'local_gn': local_gn,
                'start_time': self.time,
                'duration': 3.0,
                'last_known_state': last_known_state,
                'active': True
            })

    def create_damage_effect(self, player_id: int, x: int, y: int, damage: int):
        """Create damage effect"""
        self.game_effects.append({
            'type': 'damage',
            'player_id': player_id,
            'x': x, 'y': y,
            'damage': damage,
            'start_time': self.time,
            'duration': 1.0,
            'flash_color': (255, 0, 0),
            'active': True
        })

    def create_bomb_fsm_transition_effect(self, x: int, y: int, old_status: str, new_status: str, bomb_type: str):
        """Create FSM transition effect"""
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

    def create_live_explosion_effect(self, x: int, y: int, explosion: ExplosionState):
        """Create live explosion effect"""
        self.explosion_animations.append({
            'type': 'live_explosion',
            'x': x, 'y': y,
            'explosion_type': explosion.explosion_type,
            'intensity': explosion.intensity,
            'start_time': self.time,
            'duration': explosion.remaining_time,
            'active': True
        })

    # Animation Update System
    def update_all_animations(self):
        """Update all animations"""
        current_time = self.time

        # Update player animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                
                # Update progress based on actual backend timer if available
                if anim.get('movement_timer', 0) > 0 and anim.get('total_duration', 0) > 0:
                    remaining_ms = self.movement_timers.get(player_id, anim['movement_timer'])
                    progress = 1.0 - (remaining_ms / anim['total_duration'])
                    anim['progress'] = min(1.0, max(0.0, progress))
                else:
                    anim['progress'] = min(1.0, elapsed / anim['duration'])
                
                if anim['progress'] >= 1.0 or elapsed >= anim['duration']:
                    del self.player_animations[player_id]

        # Update bomb animations
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
            elapsed = current_time - anim['start_time']
            
            if anim.get('type') == 'moving':
                if elapsed >= anim['duration']:
                    del self.bomb_animations[pos]
            else:
                bomb_state = self.current_game_state.bombs.get(pos)
                if bomb_state:
                    anim['status'] = bomb_state.status
                    anim['ignited'] = bomb_state.ignited
                
                if elapsed > 30:  # Safety timeout
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

        # Update status effects
        for player_id in list(self.status_effects.keys()):
            effect = self.status_effects[player_id]
            elapsed = current_time - effect['start_time']
            
            if effect['type'] == 'immunity':
                remaining_timer = self.immunity_timers.get(player_id, 0)
                if remaining_timer <= 0 or elapsed > effect['duration']:
                    del self.status_effects[player_id]
                else:
                    effect['intensity'] = remaining_timer / self.backend_constants['immunity_time']

        # Update camera effects
        if self.camera_shake > 0:
            self.camera_shake -= 2.0 / FPS
            if self.camera_shake < 0:
                self.camera_shake = 0

    # Drawing System
    def draw_map(self):
        """Draw the game map with all entities"""
        # Apply camera shake
        shake_x = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update timing and animations
        self.time += 1 / FPS
        self.powerup_pulse += 1 / FPS
        self.update_all_animations()

        # Draw tiles
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = x * TILE_SIZE + shake_y

                tile_type = self.current_game_state.tiles[x][y]
                powerup = self.current_game_state.powerups[x][y]
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
        for pos, bomb in self.current_game_state.bombs.items():
            pixel_x = bomb.y * TILE_SIZE + shake_x
            pixel_y = bomb.x * TILE_SIZE + shake_y
            self.draw_bomb(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw players
        for player_id, player in self.current_game_state.players.items():
            pixel_x = player.y * TILE_SIZE + shake_x
            pixel_y = player.x * TILE_SIZE + shake_y
            self.draw_player(self.map_surface, pixel_x, pixel_y, player)

        # Draw explosions
        for explosion in self.explosion_animations:
            self.draw_explosion_effect(self.map_surface, explosion)

        # Blit map to virtual surface
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    def draw_floor(self, surface, x, y):
        """Draw floor tile"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        pygame.draw.rect(surface, COLORS['FLOOR_LIGHT'], rect)
        pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], rect, 1)

    def draw_brick_wall(self, surface, x, y):
        """Draw brick wall"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        pygame.draw.rect(surface, COLORS['BRICK_MID'], rect)
        pygame.draw.rect(surface, COLORS['BRICK_SHADOW'], rect, 2)

    def draw_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Draw wooden barrel"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Barrel body
        pygame.draw.circle(surface, COLORS['WOOD_MID'], (center_x, center_y), TILE_SIZE // 3)
        pygame.draw.circle(surface, COLORS['WOOD_SHADOW'], (center_x, center_y), TILE_SIZE // 3, 2)

        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_metal_barrel(self, surface, x, y, has_powerup=False):
        """Draw metal barrel"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Barrel body
        pygame.draw.circle(surface, COLORS['METAL_MID'], (center_x, center_y), TILE_SIZE // 3)
        pygame.draw.circle(surface, COLORS['METAL_SHADOW'], (center_x, center_y), TILE_SIZE // 3, 2)

        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_powerup_glow(self, surface, center_x, center_y):
        """Draw power-up glow"""
        glow_intensity = 0.8 + 0.2 * math.sin(self.powerup_pulse * 5)
        glow_size = int(20 + 8 * math.sin(self.powerup_pulse * 4))

        glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
        alpha = int(80 * glow_intensity)
        pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], alpha), (glow_size, glow_size), glow_size)
        surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

    def draw_bomb(self, surface, x, y, bomb_data: BombState):
        """Draw bomb with FSM state visualization"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
    
        # Calculate size based on timer and status
        if bomb_data.timer > 0:
            timer_ratio = bomb_data.timer / 3000.0
            pulse_speed = 2.0 + (1.0 - timer_ratio) * 6.0
        else:
            pulse_speed = 8.0
            
        pulse = 0.8 + 0.2 * math.sin(self.time * pulse_speed)
        bomb_size = int(16 * pulse)
    
        # Draw bomb based on status
        if bomb_data.status == 'frozen':
            self.draw_frozen_bomb(surface, center_x, center_y, bomb_size)
        elif bomb_data.status == 'remote_idle':
            self.draw_remote_bomb(surface, center_x, center_y, bomb_size)
        elif bomb_data.ignited:
            self.draw_ignited_bomb(surface, center_x, center_y, bomb_size)
        else:
            self.draw_standard_bomb(surface, center_x, center_y, bomb_data)
    
        # Draw FSM status indicator
        self.draw_bomb_status_indicator(surface, center_x, center_y + 25, bomb_data.status)
        
        # Draw timer display
        if bomb_data.timer > 0:
            timer_seconds = bomb_data.timer / 1000.0
            self.draw_bomb_timer(surface, center_x, center_y + 35, timer_seconds)
        elif bomb_data.status == 'remote_idle':
            remote_text = "REMOTE"
            text_surface = self.mini_font.render(remote_text, True, COLORS['TEXT_CYAN'])
            text_rect = text_surface.get_rect(center=(center_x, center_y + 35))
            surface.blit(text_surface, text_rect)
        elif bomb_data.status == 'frozen':
            frozen_text = "FROZEN"
            text_surface = self.mini_font.render(frozen_text, True, COLORS['FREEZE_COLOR'])
            text_rect = text_surface.get_rect(center=(center_x, center_y + 35))
            surface.blit(text_surface, text_rect)
    
    def draw_ignited_bomb(self, surface, center_x, center_y, bomb_size):
        """Draw ignited bomb with intense effects"""
        # Intense pulsing
        pulse = 0.9 + 0.1 * math.sin(self.time * 15)
        actual_size = int(bomb_size * pulse)
        
        # Danger glow
        danger_size = int(actual_size * 2)
        danger_surf = pygame.Surface((danger_size * 2, danger_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(danger_surf, (*COLORS['BOMB_FUSE'], 180), (danger_size, danger_size), danger_size)
        surface.blit(danger_surf, (center_x - danger_size, center_y - danger_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), actual_size)
        pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (center_x, center_y), actual_size, 3)
        
        # Sparking effects
        for i in range(12):
            if random.random() < 0.8:  # 80% chance for each spark
                angle = random.random() * 360
                distance = actual_size + random.randint(8, 20)
                spark_x = center_x + int(distance * math.cos(math.radians(angle)))
                spark_y = center_y + int(distance * math.sin(math.radians(angle)))
                spark_size = random.randint(2, 5)
                pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], (spark_x, spark_y), spark_size)
    
    def draw_bomb_status_indicator(self, surface, x, y, status):
        """Draw FSM status indicator"""
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
        bg_rect = text_rect.inflate(6, 3)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 180), (0, 0, bg_rect.width, bg_rect.height))
        pygame.draw.rect(bg_surf, (*color, 100), (0, 0, bg_rect.width, bg_rect.height), 1)
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(text_surface, text_rect)
    
    def draw_bomb_timer(self, surface, x, y, timer_seconds):
        """Draw bomb timer with color coding"""
        timer_text = f"{timer_seconds:.1f}s"
        
        # Color based on time remaining
        if timer_seconds <= 1.0:
            color = COLORS['TEXT_RED']
            pulse = 0.7 + 0.3 * math.sin(self.time * 12)
        elif timer_seconds <= 2.0:
            color = COLORS['TEXT_ORANGE']
            pulse = 0.9 + 0.1 * math.sin(self.time * 6)
        else:
            color = COLORS['TEXT_WHITE']
            pulse = 1.0
            
        timer_surface = self.font.render(timer_text, True, color)
        timer_rect = timer_surface.get_rect(center=(x, y))
    
        # Enhanced background with pulse effect for danger
        bg_rect = timer_rect.inflate(int(8 * pulse), int(4 * pulse))
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        bg_alpha = int(200 * pulse)
        pygame.draw.rect(bg_surf, (0, 0, 0, bg_alpha), (0, 0, bg_rect.width, bg_rect.height))
        pygame.draw.rect(bg_surf, (*color, int(120 * pulse)), (0, 0, bg_rect.width, bg_rect.height), 2)
        
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(timer_surface, timer_rect)

    def draw_frozen_bomb(self, surface, center_x, center_y, bomb_size):
        """Draw frozen bomb with ice effects"""
        # Ice glow effect
        ice_glow_size = bomb_size + 8
        ice_surf = pygame.Surface((ice_glow_size * 2, ice_glow_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(ice_surf, (*COLORS['FREEZE_COLOR'], 100), (ice_glow_size, ice_glow_size), ice_glow_size)
        surface.blit(ice_surf, (center_x - ice_glow_size, center_y - ice_glow_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, COLORS['FREEZE_COLOR'], (center_x, center_y), bomb_size, 3)
        
        # Ice crystals
        for i in range(6):
            angle = i * 60 + self.time * 30
            crystal_x = center_x + int((bomb_size - 3) * math.cos(math.radians(angle)))
            crystal_y = center_y + int((bomb_size - 3) * math.sin(math.radians(angle)))
            pygame.draw.circle(surface, (200, 230, 255), (crystal_x, crystal_y), 2)

    def draw_remote_bomb(self, surface, center_x, center_y, bomb_size):
        """Draw remote bomb with pulsing indicator"""
        # Remote glow
        remote_glow = 0.6 + 0.4 * math.sin(self.time * 4)
        glow_size = int(bomb_size * 1.3 * remote_glow)
        remote_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(remote_surf, (*COLORS['TEXT_CYAN'], 80), (glow_size, glow_size), glow_size)
        surface.blit(remote_surf, (center_x - glow_size, center_y - glow_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, COLORS['TEXT_CYAN'], (center_x, center_y), bomb_size, 2)
        
        # Remote indicator
        indicator_y = center_y - bomb_size - 5
        pygame.draw.circle(surface, COLORS['TEXT_CYAN'], (center_x, indicator_y), 3)
        pygame.draw.line(surface, COLORS['TEXT_CYAN'], (center_x, indicator_y), (center_x, indicator_y - 8), 2)

    def draw_standard_bomb(self, surface, center_x, center_y, bomb_data: BombState):
        """Draw standard bomb with timer-based effects"""
        bomb_size = int(16 * (0.8 + 0.2 * math.sin(self.time * 8)))
        
        # Danger glow based on timer
        if bomb_data.timer > 0:
            danger_ratio = 1.0 - (bomb_data.timer / 3000.0)
            glow_intensity = 0.3 + danger_ratio * 0.7
            glow_size = int(bomb_size * 1.5 * glow_intensity)
            
            danger_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            alpha = int(120 * glow_intensity)
            pygame.draw.circle(danger_surf, (*COLORS['BOMB_FUSE'], alpha), (glow_size, glow_size), glow_size)
            surface.blit(danger_surf, (center_x - glow_size, center_y - glow_size))
        
        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (80, 80, 80), (center_x, center_y), bomb_size, 2)
        
        # Highlight
        pygame.draw.circle(surface, (120, 120, 120), 
                         (center_x - bomb_size//3, center_y - bomb_size//3), bomb_size//3)
        
        # Fuse with sparks
        fuse_end_x = center_x - bomb_size//2
        fuse_end_y = center_y - bomb_size
        pygame.draw.line(surface, COLORS['BOMB_FUSE'], 
                        (center_x, center_y - bomb_size), (fuse_end_x, fuse_end_y), 3)
        
        # Sparking fuse tip
        spark_intensity = 0.5 + 0.5 * math.sin(self.time * 12)
        spark_size = int(5 * spark_intensity)
        if spark_size > 0:
            pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (fuse_end_x, fuse_end_y), spark_size)
            pygame.draw.circle(surface, COLORS['EXPLOSION_SPARK'], (fuse_end_x, fuse_end_y), spark_size // 2)

    def draw_player(self, surface, x, y, player: PlayerState):
        """Draw player with complete status effects and animations"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        player_id = player.player_id
        
        # Get player colors
        if player_id in self.current_game_state.dead_players:
            player_colors = {
                1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']
            }
            skin_color = COLORS['SKIN_DEAD']
        else:
            player_colors = {
                1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
            }
            skin_color = COLORS['SKIN']

        base_color = player_colors.get(player_id, COLORS['PLAYER_1'])

        # Handle walking animation
        char_x, char_y = x, y
        if player_id in self.player_animations:
            anim = self.player_animations[player_id]
            progress = anim.get('progress', 0.0)
            
            start_x, start_y = anim['start_pos']
            end_x, end_y = anim['end_pos']
            
            current_x = start_x + (end_x - start_x) * progress
            current_y = start_y + (end_y - start_y) * progress
            
            char_x = current_y * TILE_SIZE
            char_y = current_x * TILE_SIZE
            center_x = char_x + TILE_SIZE // 2
            center_y = char_y + TILE_SIZE // 2
            
            # Walking bounce
            walk_bounce = math.sin(progress * math.pi * 6) * 3
            center_y -= walk_bounce

        # Draw status effects
        self.draw_status_effects(surface, center_x, center_y, player_id)

        # Draw player character
        self.draw_player_character(surface, char_x, char_y, player_id, base_color, skin_color)
        
        # Draw timer bars
        self.draw_player_timers(surface, center_x, center_y + 25, player_id, player.timers)

    def draw_status_effects(self, surface, center_x, center_y, player_id):
        """Draw status effects around player"""
        # Immunity effect
        if player_id in self.status_effects and self.status_effects[player_id]['type'] == 'immunity':
            effect = self.status_effects[player_id]
            intensity = effect.get('intensity', 1.0)
            
            pulse = 0.6 + 0.4 * math.sin(self.time * 8)
            glow_size = int(30 * intensity * pulse)
            
            immunity_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
            alpha = int(100 * intensity * pulse)
            pygame.draw.circle(immunity_surf, (*COLORS['IMMUNITY_GLOW'], alpha),
                             (glow_size, glow_size), glow_size)
            surface.blit(immunity_surf, (center_x - glow_size, center_y - glow_size))

    def draw_player_character(self, surface, x, y, player_id, outfit_color, skin_color):
        """Draw player character sprite"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Body
        body_rect = pygame.Rect(center_x - 8, center_y, 16, 20)
        pygame.draw.rect(surface, outfit_color, body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 50) for c in outfit_color), body_rect, 1)

        # Head
        head_y = center_y - 10
        pygame.draw.circle(surface, skin_color, (center_x, head_y), 10)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in skin_color), (center_x, head_y), 10, 1)

        # Eyes
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 1)
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 3, head_y - 2), 1)

        # Arms
        arm_swing = math.sin(self.time * 6 + player_id * 0.5) * 3
        pygame.draw.line(surface, outfit_color, (center_x - 8, center_y + 5), (center_x - 12 + arm_swing, center_y + 12), 4)
        pygame.draw.line(surface, outfit_color, (center_x + 8, center_y + 5), (center_x + 12 - arm_swing, center_y + 12), 4)

        # Legs
        leg_offset = math.sin(self.time * 8 + player_id * 0.3) * 2
        pygame.draw.rect(surface, outfit_color, (center_x - 6 + leg_offset, center_y + 16, 4, 10))
        pygame.draw.rect(surface, outfit_color, (center_x + 2 - leg_offset, center_y + 16, 4, 10))

        # Player number badge
        badge_text = str(player_id)
        badge_surface = self.mini_font.render(badge_text, True, (0, 0, 0))
        badge_rect = pygame.Rect(center_x - 8, center_y + 28, 16, 10)
        pygame.draw.rect(surface, (255, 255, 255), badge_rect)
        pygame.draw.rect(surface, (0, 0, 0), badge_rect, 1)
        surface.blit(badge_surface, (center_x - 4, center_y + 26))

    def draw_player_timers(self, surface, x, y, player_id, timers: PlayerTimers):
        """Draw player timer bars"""
        timer_y = y
        
        # Movement timer
        if timers.movement_timer > 0:
            progress = timers.movement_timer / self.backend_constants['tile_move']
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['TEXT_CYAN'])
            timer_y += 6
        
        # Immunity timer
        if timers.immunity_timer > 0:
            progress = timers.immunity_timer / self.backend_constants['immunity_time']
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['IMMUNITY_GLOW'])
            timer_y += 6
        
        # Request cooldown
        if timers.request_timer > 0:
            progress = timers.request_timer / self.backend_constants['request_cooldown']
            self.draw_timer_bar(surface, x - 15, timer_y, 30, 4, progress, COLORS['TEXT_ORANGE'])

    def draw_timer_bar(self, surface, x, y, width, height, progress, color):
        """Draw individual timer bar"""
        # Background
        pygame.draw.rect(surface, COLORS['TIMER_BAR_BG'], (x, y, width, height))
        
        # Fill
        if progress > 0:
            fill_width = int(width * progress)
            pygame.draw.rect(surface, color, (x, y, fill_width, height))
        
        # Border
        pygame.draw.rect(surface, COLORS['TEXT_WHITE'], (x, y, width, height), 1)

    def draw_explosion_effect(self, surface, explosion):
        """Draw explosion effects"""
        elapsed = self.time - explosion['start_time']
        progress = elapsed / explosion['duration']
        
        if progress >= 1.0:
            return

        explosion_type = explosion['type']
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2

        if explosion_type == 'bomb_center':
            self.draw_bomb_center_explosion(surface, explosion, progress, center_x, center_y)
        elif explosion_type == 'coordinate_explosion':
            self.draw_coordinate_explosion(surface, explosion, progress, center_x, center_y)
        else:
            self.draw_standard_explosion(surface, explosion, progress, center_x, center_y)

    def draw_bomb_center_explosion(self, surface, explosion, progress, center_x, center_y):
        """Draw central bomb explosion"""
        radius = explosion.get('radius', 2)
        
        if progress < 0.3:  # Initial blast
            blast_radius = int(progress * 80 * radius / 2)
            intensity = 1.0 - progress * 2
            
            for layer in range(3):
                layer_radius = max(1, blast_radius - layer * 10)
                layer_alpha = int(200 * intensity * (1 - layer * 0.2))
                
                colors = [COLORS['EXPLOSION_CORE'], COLORS['EXPLOSION_MIDDLE'], COLORS['EXPLOSION_OUTER']]
                color = colors[min(layer, 2)]
                
                explosion_surf = pygame.Surface((layer_radius * 2, layer_radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(explosion_surf, (*color, layer_alpha), 
                                 (layer_radius, layer_radius), layer_radius)
                surface.blit(explosion_surf, (center_x - layer_radius, center_y - layer_radius))
                
        elif progress < 0.7:  # Fire phase
            fire_progress = (progress - 0.3) / 0.4
            
            for i in range(15):
                angle = random.random() * 2 * math.pi
                distance = random.random() * 40
                particle_x = center_x + int(math.cos(angle) * distance)
                particle_y = center_y + int(math.sin(angle) * distance) - int(fire_progress * 15)
                
                particle_size = random.randint(3, 8)
                fire_intensity = 1.0 - fire_progress
                
                color = (255, int(100 + 155 * fire_intensity), 0) if fire_intensity > 0.5 else (int(100 + 155 * fire_intensity), 0, 0)
                alpha = int(200 * fire_intensity)
                
                if alpha > 0:
                    particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                    pygame.draw.circle(particle_surf, (*color, alpha), 
                                     (particle_size, particle_size), particle_size)
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_coordinate_explosion(self, surface, explosion, progress, center_x, center_y):
        """Draw explosion from coordinate event"""
        explosion_type = explosion.get('explosion_type', 'standard')
        
        pulse_size = int(25 * (1 - progress) * math.sin(progress * math.pi))
        
        if pulse_size > 0:
            if explosion_type == 'ice':
                color = COLORS['FREEZE_COLOR']
            elif explosion_type == 'remote':
                color = COLORS['TEXT_CYAN']
            else:
                color = COLORS['EXPLOSION_MIDDLE']

            explosion_surf = pygame.Surface((pulse_size * 2, pulse_size * 2), pygame.SRCALPHA)
            alpha = int(180 * (1 - progress))
            pygame.draw.circle(explosion_surf, (*color, alpha), 
                             (pulse_size, pulse_size), pulse_size)
            surface.blit(explosion_surf, (center_x - pulse_size, center_y - pulse_size))

    def draw_standard_explosion(self, surface, explosion, progress, center_x, center_y):
        """Draw standard explosion"""
        intensity = explosion.get('intensity', 1.0)
        explosion_size = int(20 * intensity * (1 - progress))
        
        if explosion_size > 0:
            alpha = int(150 * (1 - progress))
            explosion_surf = pygame.Surface((explosion_size * 2, explosion_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_MIDDLE'], alpha),
                             (explosion_size, explosion_size), explosion_size)
            surface.blit(explosion_surf, (center_x - explosion_size, center_y - explosion_size))

    # UI Panel Drawing Methods
    def draw_player_stats_panel(self):
        """Draw player statistics panel"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Panel border
        pygame.draw.rect(self.player_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Title
        title_text = "PLAYERS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.player_panel_surface.blit(title_surface, (10, 10))

        # Connection status
        status_y = 40
        status_text = f"Status: {self.connection_status}"
        status_color = COLORS['TEXT_GREEN'] if self.connection_status == "Connected" else COLORS['TEXT_RED']
        status_surface = self.small_font.render(status_text, True, status_color)
        self.player_panel_surface.blit(status_surface, (10, status_y))

        # Draw each player
        start_y = 70
        player_height = (MAP_SIZE * TILE_SIZE - 90) // 4

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            player_data = self.current_game_state.players.get(player_id)
            is_dead = player_id in self.current_game_state.dead_players
            
            self.draw_single_player_stats(self.player_panel_surface, player_id, y_pos, player_height, player_data, is_dead)

        # Blit to virtual surface
        self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))

    def draw_single_player_stats(self, surface, player_id, y_pos, height, player_data, is_dead):
        """Draw individual player statistics"""
        # Player colors
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
                status_text = f"✅ ALIVE at ({player_data.x}, {player_data.y})"
                status_color = COLORS['TEXT_GREEN']
            else:
                status_text = "⏳ WAITING"
                status_color = COLORS['TEXT_ORANGE']

        player_color = player_colors.get(player_id, COLORS['PLAYER_1'])

        # Background
        bg_rect = pygame.Rect(5, y_pos, PLAYER_PANEL_WIDTH - 10, height - 5)
        pygame.draw.rect(surface, (*player_color, 30), bg_rect)
        pygame.draw.rect(surface, player_color, bg_rect, 2)

        # Player info
        player_text = f"PLAYER {player_id}"
        player_surface = self.font.render(player_text, True, text_color)
        surface.blit(player_surface, (15, y_pos + 5))
        
        status_surface = self.small_font.render(status_text, True, status_color)
        surface.blit(status_surface, (15, y_pos + 25))

        # Health
        if player_data and not is_dead:
            health_text = f"Health: {player_data.health}"
            health_surface = self.small_font.render(health_text, True, COLORS['TEXT_RED'])
            surface.blit(health_surface, (15, y_pos + 45))
            
            speed_text = f"Speed: {player_data.speed}"
            speed_surface = self.small_font.render(speed_text, True, COLORS['TEXT_CYAN'])
            surface.blit(speed_surface, (15, y_pos + 60))

    def draw_timer_panel(self):
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

        # Performance info
        perf_y = MAP_SIZE * TILE_SIZE - 60
        fps_text = f"FPS: {self.current_fps:.1f}"
        fps_surface = self.mini_font.render(fps_text, True, COLORS['TEXT_GREEN'])
        self.timer_panel_surface.blit(fps_surface, (10, perf_y))
        
        msg_text = f"Messages: {self.message_count}"
        msg_surface = self.mini_font.render(msg_text, True, COLORS['TEXT_WHITE'])
        self.timer_panel_surface.blit(msg_surface, (10, perf_y + 15))

        # Blit to virtual surface
        self.virtual_surface.blit(self.timer_panel_surface, (TIMER_OFFSET_X, MAP_OFFSET_Y))

    def draw_powerups_panel(self):
        """Draw power-ups information panel"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Panel border
        pygame.draw.rect(self.powerup_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, WINDOW_WIDTH, POWERUP_PANEL_HEIGHT), 2)

        # Title
        title_text = "POWER-UPS & CONTROLS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Controls info
        controls = [
            "ESC - Exit | R - Refresh | H - Help | Click tiles to inspect",
            "Socket connection status and real-time backend synchronization"
        ]
        
        for i, control in enumerate(controls):
            control_surface = self.small_font.render(control, True, COLORS['TEXT_WHITE'])
            self.powerup_panel_surface.blit(control_surface, (20, 50 + i * 20))

        # Power-up legend
        powerups = [
            ("⚡", "SPEED", COLORS['TEXT_CYAN']),
            ("📡", "REMOTE", COLORS['TEXT_ORANGE']),
            ("💣", "BOMBS", COLORS['TEXT_GOLD']),
            ("💥", "BLAST", COLORS['TEXT_RED']),
            ("❤️", "LIFE", COLORS['TEXT_GREEN']),
            ("🧊", "FREEZE", COLORS['FREEZE_COLOR'])
        ]

        start_x = 20
        start_y = 100
        for i, (icon, name, color) in enumerate(powerups):
            x = start_x + (i % 6) * 120
            y = start_y + (i // 6) * 25
            
            icon_surface = self.font.render(icon, True, color)
            name_surface = self.small_font.render(name, True, color)
            
            self.powerup_panel_surface.blit(icon_surface, (x, y))
            self.powerup_panel_surface.blit(name_surface, (x + 25, y + 3))

        # Blit to virtual surface
        self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

    # Event Handling
    def handle_events(self):
        """Handle pygame events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
                elif event.key == pygame.K_r:
                    print("🔄 Manual refresh requested")
                    if self.socket_manager.connected:
                        self.socket_manager.send_message({
                            "type": "refresh_request",
                            "timestamp": int(time.time() * 1000)
                        })
                elif event.key == pygame.K_h:
                    print("🔧 Socket Game Visualizer Controls:")
                    print("   ESC - Exit")
                    print("   R - Request refresh from server")
                    print("   H - This help")
                    print("   Mouse - Click tiles to inspect")
                    
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    self.handle_mouse_click(event.pos)
        return True

    def handle_window_resize(self, new_width, new_height):
        """Handle window resizing"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)
        self.scale_factor = min(self.current_width / WINDOW_WIDTH, self.current_height / WINDOW_HEIGHT)
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)

    def handle_mouse_click(self, mouse_pos):
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
                self.inspect_tile(tile_x, tile_y)

    def inspect_tile(self, tile_x, tile_y):
        """Inspect tile contents"""
        tile_type = self.current_game_state.tiles[tile_x][tile_y]
        powerup = self.current_game_state.powerups[tile_x][tile_y]

        tile_names = {0: 'FREE', 1: 'WOODEN_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL'}
        tile_name = tile_names.get(tile_type, f'UNKNOWN_{tile_type}')

        print(f"\n🎯 Tile Inspection at ({tile_x}, {tile_y}):")
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
        """Update FPS and performance tracking"""
        self.fps_counter += 1
        current_time = time.time()
        
        if current_time - self.last_fps_time >= 1.0:
            self.current_fps = self.fps_counter / (current_time - self.last_fps_time)
            self.fps_counter = 0
            self.last_fps_time = current_time

    def draw_complete_visualization(self):
        """Draw complete game visualization"""
        if not self.map_initialized:
            return

        # Clear virtual surface
        self.virtual_surface.fill(COLORS['BACKGROUND'])

        # Draw all components
        self.draw_map()
        self.draw_player_stats_panel()
        self.draw_timer_panel()
        self.draw_powerups_panel()

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
        self.draw_status_display()

    def draw_status_display(self):
        """Draw connection and game status"""
        status_y = 10
        
        if self.waiting_for_initial_map:
            status_text = f"⏳ Waiting for data from {CN_SERVER_HOST}:{CN_SERVER_PORT}..."
            color = COLORS['TEXT_ORANGE']
        elif not self.socket_manager.connected:
            status_text = f"❌ Disconnected from {CN_SERVER_HOST}:{CN_SERVER_PORT}"
            color = COLORS['TEXT_RED']
        else:
            dead_count = len(self.current_game_state.dead_players)
            active_animations = len(self.player_animations) + len(self.bomb_animations) + len(self.explosion_animations)
            
            status_text = f"🔗 Connected to CN server | Dead: {dead_count} | Animations: {active_animations} | FPS: {self.current_fps:.1f}"
            color = COLORS['TEXT_GREEN']

        status_surface = self.small_font.render(status_text, True, color)
        status_rect = status_surface.get_rect(topleft=(10, status_y))
        
        # Background
        bg_rect = status_rect.inflate(8, 4)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        
        self.screen.blit(bg_surf, bg_rect)
        self.screen.blit(status_surface, status_rect)

    def run_game_loop(self):
        """Main game loop with socket communication"""
        print("🚀 Starting Socket Game Visualizer...")
        print(f"🔗 Connecting to CN server at {CN_SERVER_HOST}:{CN_SERVER_PORT}")
        print("📡 Features: Real-time socket communication, complete game visualization, FSM states")
        
        # Initial connection attempt
        if not self.connect_to_server():
            print("❌ Failed to connect to server. Will retry automatically...")

        running = True
        last_reconnect_attempt = 0
        
        while running:
            current_time = time.time()
            
            # Handle events
            running = self.handle_events()

            # Handle socket messages
            if self.socket_manager.connected:
                self.handle_socket_messages()
            elif current_time - last_reconnect_attempt > RECONNECT_DELAY:
                # Attempt reconnection
                if self.socket_manager.attempt_reconnect():
                    print("✅ Reconnected to CN server!")
                    self.connection_status = "Connected"
                else:
                    self.connection_status = "Reconnecting..."
                last_reconnect_attempt = current_time

            # Update performance
            self.update_performance_tracking()

            # Draw visualization
            if self.map_initialized:
                self.draw_complete_visualization()
            else:
                # Waiting screen
                self.screen.fill(COLORS['BACKGROUND'])
                
                waiting_text = f"⏳ Connecting to CN server at {CN_SERVER_HOST}:{CN_SERVER_PORT}..."
                waiting_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                waiting_rect = waiting_surface.get_rect(center=(self.current_width // 2, self.current_height // 2))
                self.screen.blit(waiting_surface, waiting_rect)

                sub_text = "Socket-based communication with complete game state synchronization"
                sub_surface = self.small_font.render(sub_text, True, COLORS['TEXT_CYAN'])
                sub_rect = sub_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 30))
                self.screen.blit(sub_surface, sub_rect)

            # Update display
            pygame.display.flip()
            self.clock.tick(FPS)

        # Cleanup
        print("\n🛑 Shutting down Socket Game Visualizer...")
        print(f"📊 Statistics: FPS: {self.current_fps:.1f}, Messages: {self.message_count}")
        
        self.socket_manager.running = False
        self.socket_manager.close()
        
        if self.receive_thread and self.receive_thread.is_alive():
            self.receive_thread.join(timeout=1.0)
        
        pygame.quit()
        sys.exit()


# Main execution
if __name__ == "__main__":
    try:
        print("🚀 Initializing Socket-based Playing with Fire 2 Visualizer...")
        print("🔌 Complete socket communication with Erlang CN server")
        print("=" * 70)
        
        visualizer = EnhancedSocketGameVisualizer()
        visualizer.run_game_loop()
        
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
