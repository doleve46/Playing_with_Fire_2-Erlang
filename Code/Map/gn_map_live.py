import pygame
import sys
import math
import random
import time
import struct
import socket
import json
import threading
import os
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

# GN Socket Configuration - Must match Erlang GN server
GN_SOCKET_PORT_BASE = 8100  # GN1: 8101, GN2: 8102, etc.
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
    
    # YOU DIED overlay colors
    'DEATH_OVERLAY_BG': (0, 0, 0, 180),
    'DEATH_TEXT': (255, 50, 50),
    'DEATH_SHADOW': (100, 0, 0),
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

class GNSocketManager:
    def __init__(self, gn_id: str):
        self.gn_id = gn_id
        self.gn_number = int(gn_id[-1])  # Extract number from gn1, gn2, etc.
        self.host = 'localhost'
        self.port = GN_SOCKET_PORT_BASE + self.gn_number
        self.socket: Optional[socket.socket] = None
        self.connected = False
        self.running = True
        self.receive_buffer = b''
        self.lock = threading.Lock()
        self.message_queue = []
        self.connection_attempts = 0
        self.last_connect_time = 0

def read_node_id():
    """Read the GN ID from node_id.txt file"""
    try:
        # Get the directory where this script is located
        script_dir = os.path.dirname(os.path.abspath(__file__))
        node_id_file = os.path.join(script_dir, "node_id.txt")
        
        print(f"🔍 Looking for node ID file: {node_id_file}")
        
        with open(node_id_file, 'r') as f:
            gn_id = f.read().strip()
            
        print(f"✅ Read GN ID from file: '{gn_id}'")
        
        # Validate format
        if not gn_id.startswith('gn') or len(gn_id) != 3:
            raise ValueError(f"Invalid GN ID format: '{gn_id}'")
            
        return gn_id
        
    except FileNotFoundError:
        print("❌ ERROR: node_id.txt file not found!")
        print("Make sure the Erlang GN graphics server creates this file first.")
        sys.exit(1)
    except Exception as e:
        print(f"❌ ERROR reading node ID file: {e}")
        sys.exit(1)
    
    def connect(self) -> bool:
        """Establish connection to GN server"""
        try:
            if self.socket:
                self.close()
                
            print(f"🔌 Connecting to GN {self.gn_id} server at {self.host}:{self.port}...")
            
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(SOCKET_TIMEOUT)
            
            # Enable keepalive
            self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            
            # Disable Nagle's algorithm for lower latency
            self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            
            self.socket.connect((self.host, self.port))
            self.connected = True
            self.connection_attempts = 0
            
            print(f"✅ Connected to GN {self.gn_id} server successfully!")
            return True
            
        except socket.timeout:
            print(f"❌ Connection timeout - GN {self.gn_id} server may not be running")
            self.connected = False
            return False
        except ConnectionRefused:
            print(f"❌ Connection refused - GN {self.gn_id} server not accepting connections")
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
                    print(f"⚠️ GN {self.gn_id} server disconnected")
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

class GNGameVisualizer:
    def __init__(self, gn_id: str):
        # Read GN ID from file instead of command line
        self.gn_id = read_node_id()
        self.gn_number = int(self.gn_id[-1])  # Extract number from gn1, gn2, etc.
        self.host = 'localhost'
        self.port = GN_SOCKET_PORT_BASE + self.gn_number
        
        print(f"🎮 GN Map Visualizer starting for {self.gn_id}")
        print(f"🔌 Will connect to {self.host}:{self.port}")
        self.local_player_id = int(gn_id[-1])  # GN1 -> Player 1, etc.
        
        # Enhanced window setup
        initial_width = min(WINDOW_WIDTH, 1200)
        initial_height = min(WINDOW_HEIGHT, 900)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption(f"🎮 Playing with Fire 2 - GN {self.gn_id.upper()} View")
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
        self.death_font = pygame.font.Font(None, 72)  # Large font for death message

        # Socket management
        self.socket_manager = GNSocketManager(gn_id)
        self.receive_thread = None
        
        # Enhanced animation and timing system
        self.time = 0.0
        self.backend_time = 0.0
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
        self.local_player_dead = False  # Track if local player is dead
        self.death_message_start_time = 0.0  # When the death message started
        
        # Enhanced game state tracking
        self.previous_game_state = None
        self.current_game_state = GameState()
        
        # Enhanced animation systems
        self.player_animations: Dict[int, dict] = {}
        self.bomb_animations: Dict[tuple, dict] = {}
        self.explosion_animations: List[dict] = []
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

        print(f"🎮 GN Game Visualizer initialized for {gn_id.upper()}")
        print(f"🔗 Target GN server: localhost:{GN_SOCKET_PORT_BASE + int(gn_id[-1])}")
        print(f"👤 Local player: Player {self.local_player_id}")

def connect_to_server(self) -> bool:
    """Connect to GN server and start receiving thread"""
    print(f"🔌 Attempting to connect to GN {self.gn_id} server...")
    
    # Try a few times in case server isn't ready yet
    for attempt in range(5):
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
                "client_type": "gn_python_visualizer",
                "gn_id": self.gn_id,
                "local_player": self.local_player_id,
                "version": "1.0",
                "timestamp": int(time.time() * 1000)
            })
            
            return True
        else:
            print(f"⏳ Attempt {attempt + 1}/5 failed, retrying in 1 second...")
            time.sleep(1)
    
    print("❌ Failed to connect after 5 attempts")
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

            # Check if local player died
            for player_id, death_info in new_dead_players.items():
                player_id_int = int(player_id)
                if (player_id_int == self.local_player_id and 
                    player_id_int not in self.current_game_state.dead_players):
                    print(f"💀 LOCAL PLAYER {self.local_player_id} DIED!")
                    self.local_player_dead = True
                    self.death_message_start_time = time.time()
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

    # [Include all the same handler methods as in map_live_port.py but simplified versions]
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
                bomb.status = status
                bomb.ignited = ignited

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
        
        # Check if this is our local player
        if player_id == self.local_player_id:
            print(f"💀 LOCAL PLAYER {self.local_player_id} DIED!")
            self.local_player_dead = True
            self.death_message_start_time = time.time()
        
        # Create death animation
        death_info = (death_time, last_known_state, local_gn)
        self.create_enhanced_death_animation(player_id, death_info)
        
        # Add to dead players
        self.current_game_state.dead_players[player_id] = death_info

    # [Include simplified versions of all the parsing and animation methods from map_live_port.py]
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
        # Simplified version - just detect player changes
        for player_id, new_player in new_state.players.items():
            if player_id in old_state.players:
                old_player = old_state.players[player_id]
                if ((old_player.x, old_player.y) != (new_player.x, new_player.y) and
                        player_id not in self.player_animations):
                    self.create_walking_animation(
                        player_id, (old_player.x, old_player.y), (new_player.x, new_player.y),
                        new_player.direction, new_player.speed, new_player.timers
                    )

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

        # Update camera effects
        if self.camera_shake > 0:
            self.camera_shake -= 2.0 / FPS
            if self.camera_shake < 0:
                self.camera_shake = 0

    def ease_out_quad(self, t: float) -> float:
        """Quadratic ease-out function for smooth animations"""
        return 1 - (1 - t) * (1 - t)

    # [Include simplified drawing methods - same visual style but without powerup glow]
    def draw_enhanced_map(self):
        """Draw the complete enhanced map with all animations and real-time effects"""
        # Apply enhanced camera shake
        shake_x = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update timing and animations
        self.time += 1 / FPS
        self.backend_time += self.timer_update_frequency
        self.update_all_animations()

        # Draw enhanced tiles with shake offset (same as CN version)
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

        # Draw enhanced bombs
        for pos, bomb in self.current_game_state.bombs.items():
            pixel_x = bomb.y * TILE_SIZE + shake_x
            pixel_y = bomb.x * TILE_SIZE + shake_y
            self.draw_enhanced_bomb_with_fsm_state(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw enhanced players
        for player_id, player in self.current_game_state.players.items():
            pixel_x = player.y * TILE_SIZE + shake_x
            pixel_y = player.x * TILE_SIZE + shake_y
            self.draw_enhanced_player_with_complete_effects(self.map_surface, pixel_x, pixel_y, player)

        # Draw all enhanced explosions
        for explosion in self.explosion_animations:
            self.draw_enhanced_explosion_effect(self.map_surface, explosion)

        # Draw all enhanced game effects
        self.draw_all_enhanced_game_effects(self.map_surface)

        # Blit map to virtual surface
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    # [Include all the same drawing methods as map_live_port.py but WITHOUT powerup glow]
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
        """Enhanced wooden barrel WITHOUT powerup glow"""
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

        # Enhanced metal bands
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

        # NO POWERUP GLOW - this is the main difference from CN version

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced metal barrel WITHOUT powerup glow"""
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

        # Enhanced metal bands
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

        # NO POWERUP GLOW - this is the main difference from CN version

    def draw_enhanced_selection_highlight(self, surface, x, y):
        """Draw enhanced selection highlight with animation"""
        pulse = 0.7 + 0.3 * math.sin(self.time * 6)
        highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
        alpha = int(150 * pulse)
        pygame.draw.rect(highlight_surf, (*COLORS['SELECTION'][:3], alpha), (0, 0, TILE_SIZE, TILE_SIZE))
        pygame.draw.rect(highlight_surf, COLORS['TEXT_GOLD'], (0, 0, TILE_SIZE, TILE_SIZE), 3)
        surface.blit(highlight_surf, (x, y))

    def draw_enhanced_bomb_with_fsm_state(self, surface, x, y, bomb_data: BombState):
        """Draw enhanced bomb with FSM state visualization"""
        # Same implementation as CN version
        bomb_id = (bomb_data.x, bomb_data.y)
        actual_x, actual_y = x, y

        # Check for movement animation
        if bomb_id in self.bomb_animations:
            anim = self.bomb_animations[bomb_id]
            if anim.get('confirmed', False) and anim.get('type') == 'moving':
                elapsed = self.time - anim['start_time']
                progress = min(elapsed / anim['duration'], 1.0)

                start_x, start_y = anim['start_pos']
                end_x, end_y = anim['end_pos']
                
                eased_progress = self.ease_out_quad(progress)
                current_x = start_x + (end_x - start_x) * eased_progress
                current_y = start_y + (end_y - start_y) * eased_progress

                actual_x = current_y * TILE_SIZE
                actual_y = current_x * TILE_SIZE

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

    def draw_frozen_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in frozen state"""
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

    def draw_remote_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in remote_idle state"""
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

    def draw_ignited_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in ignited state"""
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

    def draw_standard_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in standard armed state"""
        pulse = 0.8 + 0.2 * math.sin(self.time * 8)
        bomb_size = int(16 * pulse)

        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (80, 80, 80), (center_x, center_y), bomb_size, 2)

        # Highlight
        pygame.draw.circle(surface, (120, 120, 120), 
                         (center_x - bomb_size // 3, center_y - bomb_size // 3), bomb_size // 4)

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

        # Handle walking animation with enhanced interpolation
        char_x, char_y = x, y
        if player_id in self.player_animations:
            anim = self.player_animations[player_id]
            progress = anim.get('progress', 0.0)

            eased_progress = self.ease_out_quad(progress)
            start_x, start_y = anim['start_pos']
            end_x, end_y = anim['end_pos']

            current_x = start_x + (end_x - start_x) * eased_progress
            current_y = start_y + (end_y - start_y) * eased_progress

            char_x = current_y * TILE_SIZE
            char_y = current_x * TILE_SIZE
            center_x = char_x + TILE_SIZE // 2
            center_y = char_y + TILE_SIZE // 2

        # Draw the player character
        self.draw_enhanced_player_character(surface, char_x, char_y, player_id, base_color, skin_color, skin_shadow_color)

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
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x - 7, head_y - 5, 7, 5))
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 5, 7, 5))
        
        # Pupils with reflection
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1)
        pygame.draw.circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1)

        # Player number badge
        badge_surf = pygame.Surface((20, 12), pygame.SRCALPHA)
        pygame.draw.rect(badge_surf, (255, 255, 255, 220), (0, 0, 20, 12))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 20, 12), 1)
        pygame.draw.rect(badge_surf, outfit_color, (1, 1, 18, 10), 1)

        num_text = self.small_font.render(str(player_id), True, (0, 0, 0))
        badge_surf.blit(num_text, (7, -1))
        surface.blit(badge_surf, (center_x - 10, char_y + 30))

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

    def draw_enhanced_explosion_effect(self, surface, explosion):
        """Draw enhanced explosion effects"""
        elapsed = self.time - explosion['start_time']
        progress = elapsed / explosion['duration']

        if progress >= 1.0:
            return

        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2
        explosion_type = explosion.get('explosion_type', 'standard')
        
        # Pulsing explosion
        pulse_size = int(30 * (1 - progress) * math.sin(progress * math.pi))
        
        if pulse_size > 0:
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

    def draw_all_enhanced_game_effects(self, surface):
        """Draw all enhanced game effects"""
        for effect in self.game_effects:
            effect_type = effect.get('type', 'unknown')
            
            if effect_type == 'player_death_enhanced':
                self.draw_enhanced_player_death_effect(surface, effect)

    def draw_enhanced_player_death_effect(self, surface, effect):
        """Draw enhanced player death effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2

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
                    pygame.draw.circle(particle_surf, (*COLORS['TEXT_RED'], particle_alpha),
                                     (particle_size, particle_size), particle_size)
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_enhanced_player_stats_panel(self):
        """Draw enhanced player statistics panel"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Enhanced panel border
        pygame.draw.rect(self.player_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Panel title with glow
        title_text = f"GN {self.gn_id.upper()} PLAYERS"
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])
        title_main = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])

        # Multi-layer title effect
        for offset in [(2, 2), (1, 1), (0, 0)]:
            if offset == (0, 0):
                self.player_panel_surface.blit(title_main, (12 + offset[0], 12 + offset[1]))
            else:
                self.player_panel_surface.blit(title_shadow, (12 + offset[0], 12 + offset[1]))

        # Connection status
        status_y = 45
        status_text = f"Socket: {self.connection_status}"
        status_color = COLORS['TEXT_GREEN'] if self.connection_status == "Connected" else COLORS['TEXT_RED']
        status_surface = self.mini_font.render(status_text, True, status_color)
        self.player_panel_surface.blit(status_surface, (12, status_y))

        # Local player indicator
        local_text = f"Local Player: {self.local_player_id}"
        local_color = COLORS['TEXT_CYAN']
        if self.local_player_dead:
            local_color = COLORS['TEXT_RED']
            local_text += " (DEAD)"
        local_surface = self.mini_font.render(local_text, True, local_color)
        self.player_panel_surface.blit(local_surface, (12, status_y + 15))

        # Draw each player's enhanced stats
        start_y = 85
        player_height = (MAP_SIZE * TILE_SIZE - 105) // 4

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            player_data = self.current_game_state.players.get(player_id)
            is_dead = player_id in self.current_game_state.dead_players
            is_local = player_id == self.local_player_id
            self.draw_enhanced_single_player_stats(self.player_panel_surface, player_id, y_pos, player_height, player_data, is_dead, is_local)

        # Blit to virtual surface
        self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))

    def draw_enhanced_single_player_stats(self, surface, player_id, y_pos, height, player_data, is_dead, is_local):
        """Draw enhanced individual player statistics"""
        # Choose colors and status based on death state
        if is_dead:
            player_colors = {
                1: COLORS['PLAYER_1_DEAD'], 2: COLORS['PLAYER_2_DEAD'],
                3: COLORS['PLAYER_3_DEAD'], 4: COLORS['PLAYER_4_DEAD']
            }
            text_color = COLORS['TEXT_GREY']
            status_text = "💀 DEAD"
            status_color = COLORS['TEXT_RED']
        else:
            player_colors = {
                1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
                3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
            }
            text_color = COLORS['TEXT_WHITE']
            if player_data:
                status_text = f"✅ ALIVE at ({player_data.x}, {player_data.y})"
                status_color = COLORS['TEXT_GREEN']
            else:
                status_text = "⏳ WAITING"
                status_color = COLORS['TEXT_ORANGE']

        player_color = player_colors.get(player_id, COLORS['PLAYER_1'])

        # Enhanced background with animated border for local player
        bg_rect = pygame.Rect(10, y_pos, PLAYER_PANEL_WIDTH - 20, height - 10)
        border_pulse = 0.7 + 0.3 * math.sin(self.time * 2 + player_id)
        
        if is_local:
            border_pulse = 1.0 + 0.5 * math.sin(self.time * 4)  # More pronounced for local player
        
        # Background gradient
        bg_alpha = int((30 if is_dead else 60) + 20 * border_pulse)
        if is_local and not is_dead:
            bg_alpha = int(80 + 40 * border_pulse)  # Brighter for local player
            
        self.draw_gradient_rect(surface, (*player_color, bg_alpha), (*player_color, bg_alpha // 2), bg_rect)
        
        # Animated border
        border_color = tuple(int(c * border_pulse) for c in player_color)
        border_width = 3 if is_local else 2
        pygame.draw.rect(surface, border_color, bg_rect, border_width)

        # Local player indicator
        if is_local:
            local_indicator = "👤 YOU"
            local_surface = self.mini_font.render(local_indicator, True, COLORS['TEXT_GOLD'])
            surface.blit(local_surface, (15, y_pos + 5))

        # Enhanced player avatar
        avatar_x = 25
        avatar_y = y_pos + 15
        death_scale = 0.6 if is_dead else 1.0
        self.draw_enhanced_mini_player(surface, avatar_x, avatar_y, player_id, scale=death_scale, is_dead=is_dead)

        # Player ID and status
        player_text = f"PLAYER {player_id}"
        player_surface = self.font.render(player_text, True, text_color)
        surface.blit(player_surface, (avatar_x + 35, avatar_y + 5))
        
        status_surface = self.small_font.render(status_text, True, status_color)
        surface.blit(status_surface, (avatar_x + 35, avatar_y + 25))

        # Enhanced statistics
        stats_start_y = y_pos + 15
        stat_height = 20

        # Health with visual hearts
        current_health = 0 if is_dead else (player_data.health if player_data else 3)
        health_text = "Health:"
        health_color = COLORS['TEXT_GREY'] if is_dead else COLORS['TEXT_RED']
        health_surface = self.small_font.render(health_text, True, health_color)
        surface.blit(health_surface, (avatar_x + 35, stats_start_y + stat_height * 2))

        # Draw hearts
        heart_start_x = avatar_x + 90
        for i in range(max(current_health, 0)):
            heart_x = heart_start_x + i * 15
            self.draw_enhanced_mini_heart(surface, heart_x, stats_start_y + stat_height * 2 + 6, health_color, is_dead)

        # Speed with visual indicator
        if player_data and not is_dead:
            current_speed = player_data.speed
            speed_text = f"Speed: {current_speed}"
            
            if current_speed > 3:
                speed_color = COLORS['TEXT_PURPLE']
            elif current_speed > 1:
                speed_color = COLORS['TEXT_GREEN']
            else:
                speed_color = COLORS['TEXT_CYAN']
                
            speed_surface = self.small_font.render(speed_text, True, speed_color)
            surface.blit(speed_surface, (avatar_x + 35, stats_start_y + stat_height * 3))

    def draw_enhanced_mini_player(self, surface, x, y, player_num, scale=1.0, is_dead=False):
        """Draw enhanced mini player"""
        if is_dead:
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

        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])
        size = int(18 * scale)

        # Enhanced body
        body_rect = pygame.Rect(x - size // 2, y, size, int(size * 1.3))
        self.draw_gradient_rect(surface, outfit_color, tuple(max(0, c - 40) for c in outfit_color), body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 60) for c in outfit_color), body_rect, 1)

        # Enhanced head
        head_y = y - size // 2
        pygame.draw.circle(surface, skin_color, (x, head_y), size // 2)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in skin_color), (x, head_y), size // 2, 1)

        # Face
        if is_dead:
            # X eyes for dead players
            eye_size = 2
            pygame.draw.line(surface, (100, 100, 100), 
                           (x - 5, head_y - 5), (x - 3, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (x - 3, head_y - 5), (x - 5, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (x + 3, head_y - 5), (x + 5, head_y - 3), eye_size)
            pygame.draw.line(surface, (100, 100, 100), 
                           (x + 5, head_y - 5), (x + 3, head_y - 3), eye_size)
        else:
            # Normal eyes
            pygame.draw.circle(surface, (0, 0, 0), (x - size // 4, head_y - 2), 1)
            pygame.draw.circle(surface, (0, 0, 0), (x + size // 4, head_y - 2), 1)

        # Player number badge
        badge_surf = pygame.Surface((16, 10), pygame.SRCALPHA)
        badge_alpha = int(220 * scale) if not is_dead else 120
        pygame.draw.rect(badge_surf, (255, 255, 255, badge_alpha), (0, 0, 16, 10))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 16, 10), 1)

        num_text = self.mini_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (5, -1))
        surface.blit(badge_surf, (x - 8, y + int(size * 1.3) + 3))

    def draw_enhanced_mini_heart(self, surface, x, y, color, is_dead=False):
        """Draw enhanced mini heart"""
        size = 7 if not is_dead else 5
        alpha = 255 if not is_dead else 120
        
        # Heart shape
        pygame.draw.circle(surface, (*color[:3], alpha), (x - 2, y - 1), 2)
        pygame.draw.circle(surface, (*color[:3], alpha), (x + 2, y - 1), 2)
        points = [(x - 3, y), (x + 3, y), (x, y + 5)]
        if len(points) >= 3:
            pygame.draw.polygon(surface, (*color[:3], alpha), points)

    def draw_enhanced_timer_panel(self):
        """Draw enhanced timer panel"""
        self.timer_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        pygame.draw.rect(self.timer_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2)

        # Title
        title_text = "GN TIMERS"
        title_surface = self.font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.timer_panel_surface.blit(title_surface, (10, 10))

        # GN Information
        gn_info_y = 40
        gn_info = [
            f"GN: {self.gn_id.upper()}",
            f"Port: {self.socket_manager.port}",
            f"Local Player: {self.local_player_id}"
        ]
        
        for i, info in enumerate(gn_info):
            color = COLORS['TEXT_CYAN']
            if i == 2 and self.local_player_dead:
                color = COLORS['TEXT_RED']
            info_surface = self.mini_font.render(info, True, color)
            self.timer_panel_surface.blit(info_surface, (10, gn_info_y + i * 15))

        # Performance info
        perf_y = MAP_SIZE * TILE_SIZE - 80
        fps_text = f"FPS: {self.current_fps:.1f}"
        fps_color = COLORS['TEXT_GREEN'] if self.current_fps > 50 else COLORS['TEXT_ORANGE']
        fps_surface = self.mini_font.render(fps_text, True, fps_color)
        self.timer_panel_surface.blit(fps_surface, (10, perf_y))
        
        msg_text = f"Messages: {self.message_count}"
        msg_surface = self.mini_font.render(msg_text, True, COLORS['TEXT_WHITE'])
        self.timer_panel_surface.blit(msg_surface, (10, perf_y + 15))

        # Blit to virtual surface
        self.virtual_surface.blit(self.timer_panel_surface, (TIMER_OFFSET_X, MAP_OFFSET_Y))

    def draw_enhanced_powerups_panel(self):
        """Draw enhanced power-ups panel"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        pygame.draw.rect(self.powerup_panel_surface, COLORS['PANEL_BORDER'], 
                        (0, 0, WINDOW_WIDTH, POWERUP_PANEL_HEIGHT), 2)

        # Title
        title_text = f"GN {self.gn_id.upper()} CONTROLS & POWER-UPS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Controls info
        controls = [
            "ESC - Exit | R - Request refresh | H - Help | Click tiles to inspect",
            f"GN Socket connection to localhost:{self.socket_manager.port} with real-time updates"
        ]
        
        for i, control in enumerate(controls):
            control_surface = self.small_font.render(control, True, COLORS['TEXT_WHITE'])
            self.powerup_panel_surface.blit(control_surface, (20, 50 + i * 20))

        # Enhanced power-up legend (same as CN version)
        powerups = [
            ("⚡", "SPEED", COLORS['TEXT_CYAN']),
            ("📡", "REMOTE", COLORS['TEXT_ORANGE']),
            ("💣", "BOMBS", COLORS['TEXT_GOLD']),
            ("💥", "BLAST", COLORS['TEXT_RED']),
            ("❤️", "LIFE", COLORS['TEXT_GREEN']),
            ("🧊", "FREEZE", COLORS['FREEZE_COLOR']),
            ("👻", "GHOST", COLORS['TEXT_PURPLE']),
            ("🦵", "KICK", (255, 100, 255))
        ]

        start_x = 20
        start_y = 100
        for i, (icon, name, color) in enumerate(powerups):
            x = start_x + (i % 8) * 100
            y = start_y + (i // 8) * 25
            
            # Animated glow
            glow_intensity = 0.7 + 0.3 * math.sin(self.time * 3 + i * 0.5)
            
            icon_surface = self.font.render(icon, True, tuple(int(c * glow_intensity) for c in color))
            name_surface = self.small_font.render(name, True, color)
            
            self.powerup_panel_surface.blit(icon_surface, (x, y))
            self.powerup_panel_surface.blit(name_surface, (x + 25, y + 3))

        # Blit to virtual surface
        self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

    def draw_death_overlay(self):
        """Draw the YOU DIED overlay when local player dies"""
        if not self.local_player_dead:
            return

        # Create overlay surface
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        overlay.fill(COLORS['DEATH_OVERLAY_BG'])
        
        # Pulsing effect for dramatic impact
        pulse = 0.8 + 0.2 * math.sin(self.time * 4)
        
        # Main "YOU DIED" text
        death_text = "YOU DIED"
        text_shadow = self.death_font.render(death_text, True, COLORS['DEATH_SHADOW'])
        text_main = self.death_font.render(death_text, True, tuple(int(c * pulse) for c in COLORS['DEATH_TEXT']))
        
        # Center the text
        text_rect = text_main.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
        shadow_rect = text_shadow.get_rect(center=(WINDOW_WIDTH // 2 + 3, WINDOW_HEIGHT // 2 + 3))
        
        # Multiple shadow layers for depth
        for offset in [(6, 6), (3, 3), (0, 0)]:
            if offset == (0, 0):
                overlay.blit(text_main, text_rect)
            else:
                shadow_pos = (text_rect.x + offset[0], text_rect.y + offset[1])
                overlay.blit(text_shadow, shadow_pos)

        # Additional message
        time_since_death = time.time() - self.death_message_start_time
        sub_text = f"Game continues... ({time_since_death:.1f}s)"
        sub_surface = self.font.render(sub_text, True, COLORS['TEXT_WHITE'])
        sub_rect = sub_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 80))
        overlay.blit(sub_surface, sub_rect)

        # Instructions
        instruction_text = "Press ESC to exit"
        instruction_surface = self.small_font.render(instruction_text, True, COLORS['TEXT_GREY'])
        instruction_rect = instruction_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 110))
        overlay.blit(instruction_surface, instruction_rect)

        # Blit overlay to virtual surface
        self.virtual_surface.blit(overlay, (0, 0))

    # Event Handling
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
                    if self.socket_manager.connected:
                        self.socket_manager.send_message({
                            "type": "refresh_request",
                            "timestamp": int(time.time() * 1000)
                        })
                elif event.key == pygame.K_h:
                    print(f"🔧 GN {self.gn_id.upper()} Game Visualizer Controls:")
                    print("   ESC - Exit")
                    print("   R - Request refresh from server")
                    print("   H - This help")
                    print("   Click tiles - Inspect with enhanced details")
                    
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:
                    self.handle_enhanced_mouse_click(event.pos)
        return True

    def handle_window_resize(self, new_width, new_height):
        """Handle enhanced window resizing"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)
        self.scale_factor = min(self.current_width / WINDOW_WIDTH, self.current_height / WINDOW_HEIGHT)
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)

    def handle_enhanced_mouse_click(self, mouse_pos):
        """Handle enhanced mouse clicks"""
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

        print(f"\n🎯 GN {self.gn_id.upper()} Tile Inspection at ({tile_x}, {tile_y}):")
        print(f"   📍 Tile Type: {tile_name}")
        print(f"   🎁 Power-up: {powerup}")

        # Check for players
        players_here = [p for p in self.current_game_state.players.values() 
                       if p.x == tile_x and p.y == tile_y]
        for player in players_here:
            local_indicator = " (YOU!)" if player.player_id == self.local_player_id else ""
            print(f"   👤 Player {player.player_id}{local_indicator}:")
            print(f"      💖 Health: {player.health}")
            print(f"      ⚡ Speed: {player.speed}")
            print(f"      🧭 Direction: {player.direction}")
            print(f"      ⏱️ Timers: Move={player.timers.movement_timer}ms, Immunity={player.timers.immunity_timer}ms")

        # Check for bombs
        bombs_here = [b for b in self.current_game_state.bombs.values() 
                     if b.x == tile_x and b.y == tile_y]
        for bomb in bombs_here:
            owner_indicator = " (YOUR BOMB!)" if bomb.owner == self.local_player_id else ""
            print(f"   💣 Bomb{owner_indicator}:")
            print(f"      🎯 Type: {bomb.bomb_type}")
            print(f"      ⏰ Timer: {bomb.timer}ms")
            print(f"      👤 Owner: Player {bomb.owner}")
            print(f"      🎰 FSM State: {bomb.status}")

        print()

    def update_performance_tracking(self):
        """Update performance tracking"""
        self.fps_counter += 1
        current_time = time.time()
        
        if current_time - self.last_fps_time >= 1.0:
            self.current_fps = self.fps_counter / (current_time - self.last_fps_time)
            self.fps_counter = 0
            self.last_fps_time = current_time

    def draw_complete_enhanced_visualization(self):
        """Draw complete enhanced visualization"""
        if not self.map_initialized:
            return

        # Clear virtual surface with enhanced background
        bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
        self.draw_gradient_rect(self.virtual_surface, COLORS['BACKGROUND'], COLORS['PANEL_BG'], bg_rect)

        # Draw all enhanced components
        self.draw_enhanced_map()
        self.draw_enhanced_player_stats_panel()
        self.draw_enhanced_timer_panel()
        self.draw_enhanced_powerups_panel()

        # Draw death overlay if local player is dead
        if self.local_player_dead:
            self.draw_death_overlay()

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

        # Enhanced status display
        self.draw_enhanced_status_display()

    def draw_enhanced_status_display(self):
        """Draw enhanced status display"""
        status_y = 10
        
        if self.waiting_for_initial_map:
            status_text = f"⏳ Waiting for game to start on GN {self.gn_id.upper()}..."
            color = COLORS['TEXT_ORANGE']
        elif not self.socket_manager.connected:
            status_text = f"❌ Disconnected from GN {self.gn_id.upper()} server"
            color = COLORS['TEXT_RED']
        else:
            dead_count = len(self.current_game_state.dead_players)
            active_animations = (len(self.player_animations) + len(self.bomb_animations) + 
                               len(self.explosion_animations) + len(self.game_effects))
            
            death_indicator = " | YOU ARE DEAD" if self.local_player_dead else ""
            status_text = (f"🔗 GN {self.gn_id.upper()} Connected | Player {self.local_player_id} | "
                         f"Dead: {dead_count} | Animations: {active_animations} | "
                         f"FPS: {self.current_fps:.1f}{death_indicator}")
            color = COLORS['TEXT_RED'] if self.local_player_dead else COLORS['TEXT_GREEN']

        status_surface = self.small_font.render(status_text, True, color)
        status_rect = status_surface.get_rect(topleft=(10, status_y))
        
        # Enhanced background
        bg_rect = status_rect.inflate(8, 4)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        pygame.draw.rect(bg_surf, color, (0, 0, bg_rect.width, bg_rect.height), 1)
        
        self.screen.blit(bg_surf, bg_rect)
        self.screen.blit(status_surface, status_rect)

    def run_enhanced_game_loop(self):
        """Main enhanced game loop with GN socket communication"""
        print(f"🚀 Starting GN {self.gn_id.upper()} Game Visualizer...")
        print(f"🔗 Connecting to GN server at localhost:{self.socket_manager.port}")
        print(f"👤 Local player: Player {self.local_player_id}")
        print("🎨 Features: Enhanced graphics, GN socket communication, FSM states, real-time effects")
        
        # Initial connection attempt
        if not self.connect_to_server():
            print("❌ Failed to connect to GN server. Will retry automatically...")

        running = True
        last_reconnect_attempt = 0
        
        while running:
            current_time = time.time()
            
            # Handle events
            running = self.handle_enhanced_events()

            # Handle socket messages
            if self.socket_manager.connected:
                self.handle_socket_messages()
            elif current_time - last_reconnect_attempt > RECONNECT_DELAY:
                # Attempt reconnection
                if self.socket_manager.attempt_reconnect():
                    print(f"✅ Reconnected to GN {self.gn_id.upper()} server!")
                    self.connection_status = "Connected"
                else:
                    self.connection_status = "Reconnecting..."
                last_reconnect_attempt = current_time

            # Update performance
            self.update_performance_tracking()

            # Draw visualization
            if self.map_initialized:
                self.draw_complete_enhanced_visualization()
            else:
                # Enhanced waiting screen
                self.screen.fill(COLORS['BACKGROUND'])
                
                waiting_pulse = 0.8 + 0.2 * math.sin(self.time * 3)
                
                main_text = f"⏳ Waiting for game to start..."
                main_surface = self.font.render(main_text, True, 
                                              tuple(int(c * waiting_pulse) for c in COLORS['TEXT_WHITE']))
                main_rect = main_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 30))
                self.screen.blit(main_surface, main_rect)

                sub_text = f"GN {self.gn_id.upper()} - Player {self.local_player_id} - Port {self.socket_manager.port}"
                sub_surface = self.small_font.render(sub_text, True, COLORS['TEXT_CYAN'])
                sub_rect = sub_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 10))
                self.screen.blit(sub_surface, sub_rect)
                
                features_text = "Enhanced Graphics | Real-time Effects | FSM States | GN Socket Communication"
                features_surface = self.mini_font.render(features_text, True, COLORS['TEXT_GOLD'])
                features_rect = features_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 40))
                self.screen.blit(features_surface, features_rect)

            # Update display
            pygame.display.flip()
            self.clock.tick(FPS)

        # Cleanup
        print(f"\n🛑 GN {self.gn_id.upper()} Game Visualizer shutting down...")
        print(f"📊 Final Statistics: FPS: {self.current_fps:.1f}, Messages: {self.message_count}")
        
        self.socket_manager.running = False
        self.socket_manager.close()
        
        if self.receive_thread and self.receive_thread.is_alive():
            self.receive_thread.join(timeout=1.0)
        
        pygame.quit()
        sys.exit()


# def determine_gn_id():
#     """Determine GN ID from command line argument passed by Erlang"""
#     if len(sys.argv) > 1:
#         arg = sys.argv[1].lower()
#         if arg in ['gn1', 'gn2', 'gn3', 'gn4']:
#             print(f"✅ GN ID received from Erlang: {arg}")
#             return arg
    
#     # This should never happen if called properly by Erlang
#     print("❌ No GN ID provided as command line argument!")
#     print("   This script should be started by gn_graphics_server.erl")
#     print("   Falling back to gn1...")
#     return 'gn1'


# Main execution
# Main execution
if __name__ == "__main__":
    try:
        print("🐍 GN Python Visualizer Starting...")
        
        # Get the GN ID first
        gn_id = read_node_id()
        
        print(f"🎯 GN ID: {gn_id.upper()}")
        print(f"👤 Local Player: Player {int(gn_id[-1])}")
        
        # Create visualizer once with correct arguments
        visualizer = GNGameVisualizer(gn_id)
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
