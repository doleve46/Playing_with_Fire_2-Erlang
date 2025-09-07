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
POWERUP_PANEL_HEIGHT = 210  # Enhanced bottom panel for power-ups (increased to fit 2 rows)
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
TILE_MOVE_BASE = 800  # Base movement time in milliseconds  (800 instead of 1200)
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

# Enhanced Color Palette - All RGB format to avoid issues
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
    'DEATH_TEXT': (255, 100, 100),

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

    # Enhanced special effects - converted to RGB
    'SHADOW_RGB': (0, 0, 0),
    'HIGHLIGHT_RGB': (255, 255, 255),
    'SELECTION_RGB': (255, 255, 0),
    'GRID_LINE_RGB': (0, 0, 0),
    'TIMER_BAR_BG': (50, 50, 50),
    'TIMER_BAR_FILL': (100, 255, 100),
    'TIMER_BAR_DANGER': (255, 100, 100),
}

# ============================================================================
# COLOR VALIDATION AND SAFE DRAWING FUNCTIONS
# ============================================================================

def validate_color(color, context="unknown"):
    """Validate and fix color values to ensure pygame compatibility"""
    try:
        if color is None:
            print(f"WARNING: None color passed to {context}, using white")
            return (255, 255, 255)
        
        if not isinstance(color, (tuple, list)):
            print(f"WARNING: Invalid color type {type(color)} in {context}, using white")
            return (255, 255, 255)
        
        if len(color) < 3:
            print(f"WARNING: Color {color} has < 3 components in {context}, using white")
            return (255, 255, 255)
        
        # Take only RGB components and clamp to valid range
        rgb = []
        for i, component in enumerate(color[:3]):
            if not isinstance(component, (int, float)):
                print(f"WARNING: Non-numeric color component {component} at index {i} in {context}")
                rgb.append(255)
            else:
                # Clamp to 0-255 range
                clamped = max(0, min(255, int(component)))
                if clamped != int(component):
                    print(f"WARNING: Color component {component} clamped to {clamped} in {context}")
                rgb.append(clamped)
        
        return tuple(rgb)
        
    except Exception as e:
        print(f"ERROR validating color {color} in {context}: {e}")
        return (255, 255, 255)  # White fallback

def safe_get_color(color_name, context="unknown"):
    """Safely get color from COLORS dictionary with validation"""
    if color_name not in COLORS:
        print(f"WARNING: Color '{color_name}' not found in COLORS dictionary in {context}")
        return (255, 0, 255)  # Bright magenta as error indicator
    
    color = COLORS[color_name]
    return validate_color(color, f"{context}:{color_name}")

def create_rgba_color(rgb_color, alpha, context="unknown"):
    """Safely create RGBA color with alpha channel"""
    validated_rgb = validate_color(rgb_color, context)
    alpha_clamped = max(0, min(255, int(alpha)))
    return (*validated_rgb, alpha_clamped)

def safe_pygame_draw_circle(surface, color, center, radius, width=0, context="circle"):
    """Safely draw circle with color validation"""
    try:
        validated_color = validate_color(color, f"{context}_circle")
        pygame.draw.circle(surface, validated_color, center, radius, width)
    except Exception as e:
        print(f"ERROR in pygame.draw.circle at {context}: {e}")
        print(f"  Color: {color} -> Validated: {validate_color(color, context)}")
        print(f"  Center: {center}, Radius: {radius}, Width: {width}")
        # Draw error indicator
        pygame.draw.circle(surface, (255, 0, 255), center, radius, width)

def safe_pygame_draw_rect(surface, color, rect, width=0, context="rect"):
    """Safely draw rectangle with color validation"""
    try:
        validated_color = validate_color(color, f"{context}_rect")
        pygame.draw.rect(surface, validated_color, rect, width)
    except Exception as e:
        print(f"ERROR in pygame.draw.rect at {context}: {e}")
        print(f"  Color: {color} -> Validated: {validate_color(color, context)}")
        print(f"  Rect: {rect}, Width: {width}")
        # Draw error indicator
        pygame.draw.rect(surface, (255, 0, 255), rect, width)

def safe_pygame_draw_line(surface, color, start_pos, end_pos, width=1, context="line"):
    """Safely draw line with color validation"""
    try:
        validated_color = validate_color(color, f"{context}_line")
        pygame.draw.line(surface, validated_color, start_pos, end_pos, width)
    except Exception as e:
        print(f"ERROR in pygame.draw.line at {context}: {e}")
        print(f"  Color: {color} -> Validated: {validate_color(color, context)}")
        print(f"  Start: {start_pos}, End: {end_pos}, Width: {width}")
        # Draw error indicator
        pygame.draw.line(surface, (255, 0, 255), start_pos, end_pos, width)

def safe_pygame_draw_polygon(surface, color, points, width=0, context="polygon"):
    """Safely draw polygon with color validation"""
    try:
        validated_color = validate_color(color, f"{context}_polygon")
        if len(points) >= 3:
            pygame.draw.polygon(surface, validated_color, points, width)
    except Exception as e:
        print(f"ERROR in pygame.draw.polygon at {context}: {e}")
        print(f"  Color: {color} -> Validated: {validate_color(color, context)}")
        print(f"  Points: {points}, Width: {width}")
        # Draw error indicator
        if len(points) >= 3:
            pygame.draw.polygon(surface, (255, 0, 255), points, width)

def safe_pygame_draw_ellipse(surface, color, rect, width=0, context="ellipse"):
    """Safely draw ellipse with color validation"""
    try:
        validated_color = validate_color(color, f"{context}_ellipse")
        pygame.draw.ellipse(surface, validated_color, rect, width)
    except Exception as e:
        print(f"ERROR in pygame.draw.ellipse at {context}: {e}")
        print(f"  Color: {color} -> Validated: {validate_color(color, context)}")
        print(f"  Rect: {rect}, Width: {width}")
        # Draw error indicator
        pygame.draw.ellipse(surface, (255, 0, 255), rect, width)

# ============================================================================
# DATA CLASSES
# ============================================================================

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
        
    def send_normal_message(self, Message):
        """Send a normal message to the GN server"""
        if self.connected:
            try:
                self.socket.sendall(Message.encode())
                print(f"Sent Keypress message to GN {self.gn_id}: {Message}")
            except Exception as e:
                print(f"Failed to send Keypress message to GN {self.gn_id}: {e}")

    def connect(self) -> bool:
        """Establish connection to GN server"""
        try:
            if self.socket:
                self.close()
                
            print(f"Connecting to GN {self.gn_id} server at {self.host}:{self.port}...")
            
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(SOCKET_TIMEOUT)
            
            # Enable keepalive
            self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            
            # Disable Nagle's algorithm for lower latency
            self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            
            self.socket.connect((self.host, self.port))
            self.connected = True
            self.connection_attempts = 0
            
            print(f"Connected to GN {self.gn_id} server successfully!")
            return True
            
        except socket.timeout:
            print(f"Connection timeout - GN {self.gn_id} server may not be running")
            self.connected = False
            return False
        except ConnectionRefusedError:
            print(f"Connection refused - GN {self.gn_id} server not accepting connections")
            self.connected = False
            return False
        except Exception as e:
            print(f"Connection failed: {e}")
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
                    print(f"GN {self.gn_id} server disconnected")
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
                            print(f"Failed to parse message: {e}")
                    else:
                        # Wait for more data
                        break
                        
            except socket.timeout:
                # Normal timeout, continue
                continue
            except Exception as e:
                print(f"Socket error: {e}")
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
            print(f"Failed to send message: {e}")
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
        
        print(f"Reconnection attempt {self.connection_attempts}/{MAX_RECONNECT_ATTEMPTS}")
        
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
        
        print(f"GN Map Visualizer starting for {self.gn_id}")
        print(f"Will connect to {self.host}:{self.port}")
        self.local_player_id = int(gn_id[-1])  # GN1 -> Player 1, etc.
        
        # Enhanced window setup
        initial_width = min(WINDOW_WIDTH, 1200)
        initial_height = min(WINDOW_HEIGHT, 900)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption(f"Playing with Fire 2 - GN {self.gn_id.upper()} View")
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
            'free': 0, 'breakable': 1, 'unbreakable': 2, 'strong': 3, 'player_start': 4, 'one_hit': 5
        }
        self.powerup_mapping = {
            'none': 'none', 'move_speed': 'move_speed', 'remote_ignition': 'remote_ignition',
            'repeat_bombs': 'repeat_bombs', 'kick_bomb': 'kick_bomb', 'phased': 'phased',
            'plus_bombs': 'plus_bombs', 'bigger_explosion': 'bigger_explosion',
            'plus_life': 'plus_life', 'freeze_bomb': 'freeze_bomb'
        }

        # Load powerup icons
        self.powerup_icons = {}
        self.powerup_panel_icons = {}  # Smaller icons for the panel
        self.powerup_icon_mapping = {
            'move_speed': 'movespeed.png',
            'remote_ignition': 'remote bomb.png',
            'repeat_bombs': 'cool weird bomb icon.png',
            'kick_bomb': 'kick bomb.png',
            'phased': 'phase v2.png',
            'plus_bombs': 'extra bomb.png',
            'bigger_explosion': 'increased radius.png',
            'plus_life': 'extra life.png',
            'freeze_bomb': 'freeze bomb.png'
        }
        self.load_powerup_icons()

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

        print(f"GN Game Visualizer initialized for {gn_id.upper()}")
        print(f"Target GN server: localhost:{GN_SOCKET_PORT_BASE + int(gn_id[-1])}")
        print(f"Local player: Player {self.local_player_id}")

    def load_powerup_icons(self):
        """Load and scale powerup icons from the assets folder"""
        # Get the absolute path to the script directory
        script_dir = os.path.dirname(os.path.abspath(__file__))
        icons_path = os.path.join(script_dir, "..", "..", "Graphics", "assets", "powerup icons")
        
        # Calculate icon sizes
        map_icon_size = int(TILE_SIZE * 1.0)  # 40px for 40px tiles (full tile size for map display)
        panel_icon_size = 50  # Proper 50px size for panel display
        
        if not os.path.exists(icons_path):
            print(f"WARNING: Powerup icons directory not found at {icons_path}")
            print("Creating fallback icons...")
            self.create_fallback_icons(map_icon_size, panel_icon_size)
            return
        
        for powerup_type, filename in self.powerup_icon_mapping.items():
            icon_path = os.path.join(icons_path, filename)
            
            try:
                if os.path.exists(icon_path):
                    # Load original image
                    original_icon = pygame.image.load(icon_path).convert_alpha()
                    
                    # Scale for map display (40px)
                    map_icon = pygame.transform.smoothscale(original_icon, (map_icon_size, map_icon_size))
                    self.powerup_icons[powerup_type] = map_icon
                    
                    # Scale for panel display (50px)
                    panel_icon = pygame.transform.smoothscale(original_icon, (panel_icon_size, panel_icon_size))
                    self.powerup_panel_icons[powerup_type] = panel_icon
                    
                    print(f"✓ Loaded icon for {powerup_type}: {filename}")
                else:
                    print(f"⚠ Icon file not found: {icon_path}")
                    # Create fallback for this specific powerup
                    self.powerup_icons[powerup_type] = self.create_fallback_icon(powerup_type, map_icon_size)
                    self.powerup_panel_icons[powerup_type] = self.create_fallback_icon(powerup_type, panel_icon_size)
                    
            except Exception as e:
                print(f"ERROR loading icon for {powerup_type}: {e}")
                # Create fallback for this specific powerup
                self.powerup_icons[powerup_type] = self.create_fallback_icon(powerup_type, map_icon_size)
                self.powerup_panel_icons[powerup_type] = self.create_fallback_icon(powerup_type, panel_icon_size)

    def create_fallback_icon(self, powerup_type, icon_size):
        """Create a fallback icon for a powerup type"""
        fallback_icon = pygame.Surface((icon_size, icon_size), pygame.SRCALPHA)
        
        # Use powerup-specific colors
        powerup_colors = {
            'move_speed': COLORS.get('TEXT_CYAN', (100, 255, 255)),
            'remote_ignition': COLORS.get('TEXT_ORANGE', (255, 165, 0)),
            'repeat_bombs': COLORS.get('TEXT_GOLD', (255, 215, 0)),
            'kick_bomb': (255, 100, 255),
            'phased': COLORS.get('TEXT_PURPLE', (200, 100, 255)),
            'plus_bombs': COLORS.get('TEXT_GOLD', (255, 215, 0)),
            'bigger_explosion': COLORS.get('TEXT_RED', (200, 50, 50)),
            'plus_life': COLORS.get('TEXT_GREEN', (100, 255, 100)),
            'freeze_bomb': COLORS.get('FREEZE_COLOR', (150, 200, 255))
        }
        
        color = powerup_colors.get(powerup_type, COLORS.get('POWERUP_CORE', (255, 215, 0)))
        
        # Draw a simple colored rectangle with border
        pygame.draw.rect(fallback_icon, color, (0, 0, icon_size, icon_size))
        pygame.draw.rect(fallback_icon, (0, 0, 0), (0, 0, icon_size, icon_size), 2)
        
        # Add simple text identifier
        if icon_size >= 20:
            font = pygame.font.Font(None, max(12, icon_size // 4))
            text = powerup_type[:3].upper()  # First 3 characters
            text_surface = font.render(text, True, (0, 0, 0))
            text_rect = text_surface.get_rect(center=(icon_size//2, icon_size//2))
            fallback_icon.blit(text_surface, text_rect)
        
        return fallback_icon

    def create_fallback_icons(self, map_icon_size, panel_icon_size):
        """Create all fallback icons when directory not found"""
        for powerup_type in self.powerup_icon_mapping.keys():
            self.powerup_icons[powerup_type] = self.create_fallback_icon(powerup_type, map_icon_size)
            self.powerup_panel_icons[powerup_type] = self.create_fallback_icon(powerup_type, panel_icon_size)

    def connect_to_server(self) -> bool:
        """Connect to GN server and start receiving thread"""
        print(f"Attempting to connect to GN {self.gn_id} server...")
        
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
                print(f"Attempt {attempt + 1}/5 failed, retrying in 1 second...")
                time.sleep(1)
        
        print("Failed to connect after 5 attempts")
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
                print(f"Unknown message type: {message_type}")

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
                    print(f"LOCAL PLAYER {self.local_player_id} DIED!")
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
                    print("Initial map loaded! Now receiving real-time updates...")

                # Detect changes for enhanced animations
                if self.previous_game_state:
                    self.detect_game_changes(self.previous_game_state, self.current_game_state)

                return True
            return False
            
        except Exception as e:
            print(f"Error processing map update: {e}")
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
        """Handle player movement with immediate start"""
        player_id = int(player_data.get('player_id', 0))
        from_pos = player_data.get('from_pos', [0, 0])
        to_pos = player_data.get('to_pos', [0, 0])
        direction = player_data.get('direction', 'north')
        speed = int(player_data.get('speed', 1))
        movement_timer = int(player_data.get('movement_timer', 0))
        total_duration = int(player_data.get('total_duration', 0))
        immunity_timer = int(player_data.get('immunity_timer', 0))
        request_timer = int(player_data.get('request_timer', 0))
    
        if total_duration <= 0:
            base_duration = self.backend_constants.get('tile_move', TILE_MOVE_BASE)
            ms_reduction = self.backend_constants.get('ms_reduction', MS_REDUCTION)
            total_duration = base_duration - (speed - 1) * ms_reduction
    
        actual_duration = total_duration / 1000.0
    
        # Start animation immediately
        self.player_animations[player_id] = {
            'type': 'confirmed_walking',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': actual_duration,
            'speed': speed,
            'movement_timer': movement_timer,
            'total_duration': total_duration,
            'confirmed': True,
            'active': True
        }
    
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
            
            self.movement_timers[player_id] = movement_timer
            self.immunity_timers[player_id] = immunity_timer
            self.request_timers[player_id] = request_timer

    def ease_out_quad(self, t: float) -> float:
        """Quadratic ease-out function for smooth animation transitions"""
        return 1 - (1 - t) * (1 - t)

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
            print(f"LOCAL PLAYER {self.local_player_id} DIED!")
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
        """Create walking animation immediately when movement detected"""
        # Don't create if we already have a confirmed animation
        if (player_id in self.player_animations and 
                self.player_animations[player_id].get('confirmed', False)):
            return
    
        base_duration = self.backend_constants.get('tile_move', TILE_MOVE_BASE)
        ms_reduction = self.backend_constants.get('ms_reduction', MS_REDUCTION)
        total_duration = base_duration - (speed - 1) * ms_reduction
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
                    'x': y, 'y': x,    # changed from 'x': x, 'y': y
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
        """Update all animations with server timer sync"""
        current_time = self.time
    
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                # Get current server timer
                server_timer = self.movement_timers.get(player_id, 0)
                
                if anim.get('total_duration', 0) > 0 and server_timer >= 0:
                    # Calculate progress from server timer
                    elapsed_ms = anim['total_duration'] - server_timer
                    progress = elapsed_ms / anim['total_duration']
                    anim['progress'] = max(0.0, min(1.0, progress))
                    
                    # End when server timer reaches 0
                    if server_timer <= 0:
                        anim['progress'] = 1.0
                else:
                    # Fallback to time-based
                    elapsed = current_time - anim['start_time']
                    anim['progress'] = min(1.0, elapsed / anim['duration'])
                
                if anim['progress'] >= 1.0:
                    del self.player_animations[player_id]
    
        # Update other animations...
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]
    
        self.game_effects = [
            effect for effect in self.game_effects
            if current_time - effect['start_time'] < effect['duration']
        ]
    
        if self.camera_shake > 0:
            self.camera_shake -= 2.0 / FPS
            if self.camera_shake < 0:
                self.camera_shake = 0

    # ENHANCED DRAWING METHODS WITH SAFE COLOR HANDLING
    def draw_enhanced_map(self):
        """Draw the complete enhanced map with all animations and real-time effects"""
        # Apply enhanced camera shake
        shake_x = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 12) if self.camera_shake > 0 else 0

        self.map_surface.fill(safe_get_color('BACKGROUND', 'map_background'))

        # Update timing and animations
        self.time += 1 / FPS
        self.backend_time += self.timer_update_frequency
        self.update_all_animations()

        # Draw enhanced tiles with shake offset (same as CN version)
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = (MAP_SIZE - 1 - x) * TILE_SIZE + shake_y

                tile_type = self.current_game_state.tiles[x][y]
                powerup = self.current_game_state.powerups[x][y]
                has_powerup = powerup != "none"

                # Draw enhanced floor
                if tile_type != 2:
                    self.draw_enhanced_floor(self.map_surface, pixel_x, pixel_y)

                # Draw enhanced objects
                # ADDED: tile_type == 0
                if tile_type == 0 and has_powerup:  # Free floor with powerup
                    self.draw_standalone_powerup(self.map_surface, pixel_x, pixel_y, powerup)
                elif tile_type == 1:  # BREAKABLE
                    self.draw_enhanced_wooden_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 2:  # UNBREAKABLE
                    self.draw_enhanced_brick_wall(self.map_surface, pixel_x, pixel_y)
                elif tile_type == 3:  # STRONG
                    self.draw_enhanced_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 5:  # ONE_HIT (damaged strong barrel)
                    self.draw_enhanced_damaged_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)

                # Enhanced selection highlight
                if self.selected_tile == (x, y):
                    self.draw_enhanced_selection_highlight(self.map_surface, pixel_x, pixel_y)

        # Draw enhanced bombs
        for pos, bomb in self.current_game_state.bombs.items():
            pixel_x = bomb.y * TILE_SIZE + shake_x
            pixel_y = (MAP_SIZE - 1 - bomb.x) * TILE_SIZE + shake_y
            self.draw_enhanced_bomb_with_fsm_state(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw enhanced players
        for player_id, player in self.current_game_state.players.items():
            pixel_x = player.y * TILE_SIZE + shake_x
            pixel_y = (MAP_SIZE - 1 - player.x) * TILE_SIZE + shake_y
            self.draw_enhanced_player_with_complete_effects(self.map_surface, pixel_x, pixel_y, player)

        # Draw all enhanced explosions
        for explosion in self.explosion_animations:
            self.draw_enhanced_explosion_effect(self.map_surface, explosion)

        # Draw all enhanced game effects
        self.draw_all_enhanced_game_effects(self.map_surface)

        # Blit map to virtual surface
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    def draw_enhanced_floor(self, surface, x, y):
        """Enhanced floor tile with realistic texture"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        floor_light = safe_get_color('FLOOR_LIGHT', 'floor_tile')
        floor_dark = safe_get_color('FLOOR_DARK', 'floor_tile')
        floor_shadow = safe_get_color('FLOOR_SHADOW', 'floor_tile')
        
        self.draw_gradient_rect(surface, floor_light, floor_dark, rect)

        # Subtle texture pattern
        for i in range(0, TILE_SIZE, 8):
            for j in range(0, TILE_SIZE, 8):
                if (i + j) % 16 == 0:
                    safe_pygame_draw_rect(surface, floor_shadow, (x + i, y + j, 4, 4), context="floor_texture")

        # Enhanced border
        safe_pygame_draw_rect(surface, floor_light, rect, 2, context="floor_border_light")
        safe_pygame_draw_rect(surface, floor_shadow, rect, 1, context="floor_border_shadow")

    def draw_enhanced_brick_wall(self, surface, x, y):
        """Enhanced brick wall with realistic depth and texture"""
        # Get colors safely
        shadow_color = safe_get_color('SHADOW_RGB', 'brick_shadow')
        brick_top = safe_get_color('BRICK_TOP', 'brick_wall')
        brick_dark = safe_get_color('BRICK_DARK', 'brick_wall')
        mortar_color = safe_get_color('MORTAR', 'brick_wall')
        brick_shadow = safe_get_color('BRICK_SHADOW', 'brick_wall')
        
        # Drop shadow with alpha surface
        shadow_surf = pygame.Surface((TILE_SIZE + 6, TILE_SIZE + 6), pygame.SRCALPHA)
        shadow_rgba = create_rgba_color(shadow_color, 60, 'brick_shadow')
        safe_pygame_draw_rect(shadow_surf, shadow_rgba, (0, 0, TILE_SIZE + 6, TILE_SIZE + 6), context="brick_shadow")
        surface.blit(shadow_surf, (x - 3, y - 3))

        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, brick_top, brick_dark, rect)

        # Enhanced brick pattern
        brick_height = TILE_SIZE // 5
        for row in range(5):
            brick_y = y + row * brick_height
            safe_pygame_draw_line(surface, mortar_color,
                           (x, brick_y), (x + TILE_SIZE, brick_y), 2, context="brick_mortar_h")

            # Alternating brick pattern
            offset = (TILE_SIZE // 3) if row % 2 == 0 else TILE_SIZE // 6
            for i in range(4):
                brick_x = x + offset + i * (TILE_SIZE // 4)
                if x <= brick_x < x + TILE_SIZE:
                    safe_pygame_draw_line(surface, mortar_color,
                                   (brick_x, brick_y), (brick_x, brick_y + brick_height), 2, context="brick_mortar_v")

        # Enhanced 3D effect
        safe_pygame_draw_line(surface, brick_top, (x, y), (x + TILE_SIZE, y), 3, context="brick_highlight_top")
        safe_pygame_draw_line(surface, brick_top, (x, y), (x, y + TILE_SIZE), 3, context="brick_highlight_left")
        safe_pygame_draw_line(surface, brick_shadow, (x + TILE_SIZE - 1, y), 
                        (x + TILE_SIZE - 1, y + TILE_SIZE), 2, context="brick_shadow_right")
        safe_pygame_draw_line(surface, brick_shadow, (x, y + TILE_SIZE - 1), 
                        (x + TILE_SIZE, y + TILE_SIZE - 1), 2, context="brick_shadow_bottom")

    def draw_enhanced_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced wooden barrel WITHOUT powerup glow"""
        # Get colors safely
        shadow_color = safe_get_color('SHADOW_RGB', 'wood_shadow')
        wood_light = safe_get_color('WOOD_LIGHT', 'wood_barrel')
        wood_dark = safe_get_color('WOOD_DARK', 'wood_barrel')
        wood_shadow = safe_get_color('WOOD_SHADOW', 'wood_barrel')
        wood_highlight = safe_get_color('WOOD_HIGHLIGHT', 'wood_barrel')
        wood_band = safe_get_color('WOOD_BAND', 'wood_barrel')
        
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 8, TILE_SIZE + 8), pygame.SRCALPHA)
        shadow_rgba = create_rgba_color(shadow_color, 60, 'wood_barrel_shadow')
        safe_pygame_draw_ellipse(shadow_surf, shadow_rgba, (0, 0, TILE_SIZE + 8, TILE_SIZE + 8), context="wood_shadow")
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
            
            # Safe color interpolation
            r = int(wood_light[0] * (1 - adjusted_ratio) + wood_dark[0] * adjusted_ratio)
            g = int(wood_light[1] * (1 - adjusted_ratio) + wood_dark[1] * adjusted_ratio)
            b = int(wood_light[2] * (1 - adjusted_ratio) + wood_dark[2] * adjusted_ratio)
            
            grain_color = validate_color((r, g, b), 'wood_grain')
            safe_pygame_draw_line(surface, grain_color,
                           (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1, context="wood_grain")

        # Enhanced metal bands
        band_positions = [0.15, 0.4, 0.6, 0.85]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 6) * (1.0 + 0.3 * math.sin(band_ratio * math.pi)))

            # Band shadow
            safe_pygame_draw_rect(surface, wood_shadow,
                           (center_x - band_width // 2, band_y - 1, band_width, 6), context="wood_band_shadow")
            # Main band
            safe_pygame_draw_rect(surface, wood_band,
                           (center_x - band_width // 2, band_y - 2, band_width, 5), context="wood_band_main")
            # Band highlight
            safe_pygame_draw_rect(surface, wood_highlight,
                           (center_x - band_width // 2, band_y - 2, band_width, 1), context="wood_band_highlight")

        # Enhanced wood grain texture
        for i in range(8):
            grain_x = x + 6 + i * 4
            if grain_x < x + TILE_SIZE - 6:
                grain_intensity = 0.7 + 0.3 * math.sin(i * 1.2)
                grain_color = tuple(int(c * grain_intensity) for c in wood_shadow)
                grain_color = validate_color(grain_color, 'wood_grain_texture')
                safe_pygame_draw_line(surface, grain_color,
                               (grain_x, y + 4), (grain_x, y + TILE_SIZE - 4), 1, context="wood_grain_line")

        # Enhanced highlight
        safe_pygame_draw_line(surface, wood_highlight,
                        (x + 4, y + 2), (x + 4, y + TILE_SIZE - 2), 3, context="wood_highlight_line")

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced metal barrel WITHOUT powerup glow"""
        # Get colors safely
        shadow_color = safe_get_color('SHADOW_RGB', 'metal_shadow')
        metal_light = safe_get_color('METAL_LIGHT', 'metal_barrel')
        metal_dark = safe_get_color('METAL_DARK', 'metal_barrel')
        metal_shadow = safe_get_color('METAL_SHADOW', 'metal_barrel')
        metal_shine = safe_get_color('METAL_SHINE', 'metal_barrel')
        metal_band = safe_get_color('METAL_BAND', 'metal_barrel')
        
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 8, TILE_SIZE + 8), pygame.SRCALPHA)
        shadow_rgba = create_rgba_color(shadow_color, 60, 'metal_barrel_shadow')
        safe_pygame_draw_ellipse(shadow_surf, shadow_rgba, (0, 0, TILE_SIZE + 8, TILE_SIZE + 8), context="metal_shadow")
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
            
            r = int(metal_light[0] * (1 - ratio) * reflection_factor + metal_dark[0] * ratio)
            g = int(metal_light[1] * (1 - ratio) * reflection_factor + metal_dark[1] * ratio)
            b = int(metal_light[2] * (1 - ratio) * reflection_factor + metal_dark[2] * ratio)
            
            # Clamp and validate color
            metal_color = validate_color((r, g, b), 'metal_gradient')
            safe_pygame_draw_line(surface, metal_color,
                           (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1, context="metal_gradient")

        # Enhanced metal bands
        band_positions = [0.2, 0.8]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 6) * (1.0 + 0.25 * math.sin(band_ratio * math.pi)))

            # Band shadow
            safe_pygame_draw_rect(surface, metal_shadow,
                           (center_x - band_width // 2, band_y - 1, band_width, 5), context="metal_band_shadow")
            # Main band
            safe_pygame_draw_rect(surface, metal_band,
                           (center_x - band_width // 2, band_y - 2, band_width, 4), context="metal_band_main")
            # Metallic shine
            safe_pygame_draw_rect(surface, metal_shine,
                           (center_x - band_width // 2, band_y - 2, band_width, 1), context="metal_band_shine")

        # Enhanced metallic shine strips
        shine_positions = [0.25, 0.5, 0.75]
        for shine_ratio in shine_positions:
            shine_x = x + int(TILE_SIZE * shine_ratio)
            shine_intensity = 0.6 + 0.4 * math.sin(self.time * 2 + shine_ratio * 10)
            shine_alpha = int(150 * shine_intensity)
            
            if shine_alpha > 0:
                shine_surf = pygame.Surface((3, TILE_SIZE - 8), pygame.SRCALPHA)
                shine_rgba = create_rgba_color(metal_shine, shine_alpha, 'metal_shine_strip')
                safe_pygame_draw_rect(shine_surf, shine_rgba, (0, 0, 3, TILE_SIZE - 8), context="metal_shine_rect")
                surface.blit(shine_surf, (shine_x - 1, y + 4))
    
    # ADDED: draw power ups not in tile
    def draw_standalone_powerup(self, surface, x, y, powerup_type):
        """Draw standalone powerup on free floor tile"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        # Try to use the loaded icon first
        powerup_icon = self.powerup_icons.get(powerup_type)
        if powerup_icon:
            # Draw the icon with floating animation
            float_offset = int(3 * math.sin(self.time * 3))
            icon_x = center_x - powerup_icon.get_width() // 2
            icon_y = center_y - powerup_icon.get_height() // 2 + float_offset
            
            # Add a subtle glow behind the icon
            glow_pulse = 0.7 + 0.3 * math.sin(self.time * 4)
            glow_size = int(25 * glow_pulse)
            
            if glow_size > 0:
                glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
                glow_color = safe_get_color('POWERUP_GLOW', 'powerup_icon_glow')
                glow_alpha = int(100 * glow_pulse)
                glow_rgba = create_rgba_color(glow_color, glow_alpha, 'powerup_icon_glow')
                safe_pygame_draw_circle(glow_surf, glow_rgba, (glow_size, glow_size), glow_size, context="powerup_icon_glow")
                surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size + float_offset))
            
            surface.blit(powerup_icon, (icon_x, icon_y))
            return
        
        # Fallback to legacy drawing if icon not available
        self.draw_legacy_powerup(surface, x, y, powerup_type)

    def draw_legacy_powerup(self, surface, x, y, powerup_type):
        """Legacy powerup drawing for fallback"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2
        
        # Powerup colors based on type
        powerup_colors = {
            'move_speed': safe_get_color('TEXT_CYAN', 'speed_powerup'),
            'remote_ignition': safe_get_color('TEXT_ORANGE', 'remote_powerup'),
            'repeat_bombs': safe_get_color('TEXT_GOLD', 'repeat_powerup'),
            'kick_bomb': validate_color((255, 100, 255), 'kick_powerup'),
            'phased': safe_get_color('TEXT_PURPLE', 'phased_powerup'),
            'plus_bombs': safe_get_color('TEXT_GOLD', 'plus_bombs_powerup'),
            'bigger_explosion': safe_get_color('TEXT_RED', 'bigger_explosion_powerup'),
            'plus_life': safe_get_color('TEXT_GREEN', 'plus_life_powerup'),
            'freeze_bomb': safe_get_color('FREEZE_COLOR', 'freeze_powerup')
        }
        
        color = powerup_colors.get(powerup_type, safe_get_color('POWERUP_CORE', 'default_powerup'))
        
        # Animated pulsing
        pulse = 0.8 + 0.2 * math.sin(self.time * 5)
        glow_size = int(20 * pulse)
        
        # Multi-layered powerup display
        for layer in range(3):
            radius = glow_size - layer * 4
            if radius > 0:
                alpha = int(150 * pulse * (1 - layer * 0.3))
                
                if alpha > 0:
                    powerup_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                    powerup_rgba = create_rgba_color(color, alpha, f'powerup_layer_{layer}')
                    safe_pygame_draw_circle(powerup_surf, powerup_rgba, 
                                     (radius, radius), radius, context=f"powerup_{powerup_type}")
                    surface.blit(powerup_surf, (center_x - radius, center_y - radius))
        
        # Central core
        core_size = max(1, int(8 * pulse))
        safe_pygame_draw_circle(surface, color, (center_x, center_y), core_size, context=f"powerup_core_{powerup_type}")
        
        # Floating particles around powerup
        for i in range(4):
            angle = (self.time * 2 + i * 90) % 360
            particle_x = center_x + int(15 * math.cos(math.radians(angle)))
            particle_y = center_y + int(15 * math.sin(math.radians(angle))) + int(2 * math.sin(self.time * 4 + i))
            
            particle_size = max(1, int(3 * pulse))
            safe_pygame_draw_circle(surface, color, (particle_x, particle_y), particle_size, context=f"powerup_particle_{i}")

    def draw_enhanced_damaged_metal_barrel(self, surface, x, y, has_powerup=False):
        """Enhanced damaged metal barrel (one_hit state) with visible damage"""
        # Get colors safely
        shadow_color = safe_get_color('SHADOW_RGB', 'damaged_metal_shadow')
        metal_light = safe_get_color('METAL_LIGHT', 'damaged_metal_barrel')
        metal_dark = safe_get_color('METAL_DARK', 'damaged_metal_barrel')
        metal_shadow = safe_get_color('METAL_SHADOW', 'damaged_metal_barrel')
        metal_shine = safe_get_color('METAL_SHINE', 'damaged_metal_barrel')
        metal_band = safe_get_color('METAL_BAND', 'damaged_metal_barrel')
        
        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE + 8, TILE_SIZE + 8), pygame.SRCALPHA)
        shadow_rgba = create_rgba_color(shadow_color, 60, 'damaged_metal_barrel_shadow')
        safe_pygame_draw_ellipse(shadow_surf, shadow_rgba, (0, 0, TILE_SIZE + 8, TILE_SIZE + 8), context="damaged_metal_shadow")
        surface.blit(shadow_surf, (x - 4, y - 4))

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Enhanced barrel body with metallic gradient (more dented)
        for i in range(TILE_SIZE):
            y_pos = y + i
            # More dramatic denting for damaged barrel
            curve_factor = 1.0 + 0.4 * math.sin((i / TILE_SIZE) * math.pi) + 0.1 * math.sin((i / TILE_SIZE) * math.pi * 3)
            width = int((TILE_SIZE - 12) * curve_factor)  # Slightly smaller due to damage

            # Darker metallic coloring due to damage
            ratio = i / TILE_SIZE
            damage_factor = 0.7  # Make it darker to show damage
            
            r = int((metal_light[0] * (1 - ratio) + metal_dark[0] * ratio) * damage_factor)
            g = int((metal_light[1] * (1 - ratio) + metal_dark[1] * ratio) * damage_factor)
            b = int((metal_light[2] * (1 - ratio) + metal_dark[2] * ratio) * damage_factor)
            
            # Clamp and validate color
            metal_color = validate_color((r, g, b), 'damaged_metal_gradient')
            safe_pygame_draw_line(surface, metal_color,
                           (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1, context="damaged_metal_gradient")

        # Damaged metal bands (fewer and more worn)
        band_positions = [0.3, 0.7]  # Only 2 bands, positioned differently
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.3 * math.sin(band_ratio * math.pi)))

            # More worn band appearance
            worn_band_color = validate_color((metal_band[0] * 0.6, metal_band[1] * 0.6, metal_band[2] * 0.6), 'worn_band')
            safe_pygame_draw_rect(surface, worn_band_color,
                           (center_x - band_width // 2, band_y - 1, band_width, 3), context="worn_metal_band")

        # Add visible damage effects - cracks and dents (static, no animation)
        damage_color = validate_color((40, 30, 25), 'damage_marks')
        
        # Static cracks
        crack_positions = [
            (center_x + 5, y + 8, center_x + 12, y + 20),
            (center_x - 8, y + 15, center_x - 3, y + 25),
            (center_x + 3, y + 28, center_x + 10, y + 35)
        ]
        
        for crack in crack_positions:
            safe_pygame_draw_line(surface, damage_color, (crack[0], crack[1]), (crack[2], crack[3]), 2, context="damage_crack")

        # Static dent marks
        dent_positions = [(center_x + 12, center_y + 10), (center_x - 12, center_y + 22), (center_x + 6, center_y + 30)]
        for dent_x, dent_y in dent_positions:
            safe_pygame_draw_circle(surface, damage_color, (int(dent_x), int(dent_y)), 3, context="damage_dent")

        # Reduced metallic shine (damaged metal doesn't shine as much)
        shine_positions = [0.4, 0.6]  # Fewer shine positions
        for shine_ratio in shine_positions:
            shine_x = x + int(TILE_SIZE * shine_ratio)
            shine_alpha = 60  # Much reduced shine
            
            shine_surf = pygame.Surface((2, TILE_SIZE - 12), pygame.SRCALPHA)
            shine_rgba = create_rgba_color(metal_shine, shine_alpha, 'damaged_metal_shine_strip')
            safe_pygame_draw_rect(shine_surf, shine_rgba, (0, 0, 2, TILE_SIZE - 12), context="damaged_metal_shine_rect")
            surface.blit(shine_surf, (shine_x, y + 6))

        # Show powerup glow if present
        if has_powerup:
            self.draw_enhanced_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_powerup_glow(self, surface, center_x, center_y):
        """Enhanced power-up glow with pulsing and particles"""
        glow_intensity = 0.8 + 0.2 * math.sin(self.time * 5)
        glow_size = int(25 + 10 * math.sin(self.time * 4))

        # Multi-layered glow
        for layer in range(3):
            radius = glow_size - layer * 6
            if radius > 0:
                alpha = int(40 * glow_intensity * (1 - layer * 0.3))
                glow_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                glow_color = safe_get_color('POWERUP_GLOW', 'powerup_glow') if layer == 0 else safe_get_color('POWERUP_CORE', 'powerup_core')
                glow_rgba = create_rgba_color(glow_color, alpha, f'powerup_glow_layer_{layer}')
                safe_pygame_draw_circle(glow_surf, glow_rgba, (radius, radius), radius, context=f"powerup_glow_{layer}")
                surface.blit(glow_surf, (center_x - radius, center_y - radius))

        # Floating particles
        for i in range(6):
            angle = (self.time * 2 + i * 60) % 360
            particle_x = center_x + int(20 * math.cos(math.radians(angle)))
            particle_y = center_y + int(20 * math.sin(math.radians(angle))) + int(3 * math.sin(self.time * 3 + i))
            
            particle_size = 2 + int(2 * math.sin(self.time * 4 + i))
            if particle_size > 0:
                pulse_color = safe_get_color('POWERUP_PULSE', 'powerup_pulse_particle')
                safe_pygame_draw_circle(surface, pulse_color, (particle_x, particle_y), particle_size, context=f"powerup_particle_{i}")

    def draw_enhanced_selection_highlight(self, surface, x, y):
        """Draw enhanced selection highlight with animation"""
        pulse = 0.7 + 0.3 * math.sin(self.time * 6)
        highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
        
        selection_color = safe_get_color('SELECTION_RGB', 'selection_highlight')
        gold_color = safe_get_color('TEXT_GOLD', 'selection_border')
        
        alpha = int(150 * pulse)
        selection_rgba = create_rgba_color(selection_color, alpha, 'selection_fill')
        
        safe_pygame_draw_rect(highlight_surf, selection_rgba, (0, 0, TILE_SIZE, TILE_SIZE), context="selection_fill")
        safe_pygame_draw_rect(highlight_surf, gold_color, (0, 0, TILE_SIZE, TILE_SIZE), 3, context="selection_border")
        surface.blit(highlight_surf, (x, y))

    def draw_enhanced_bomb_with_fsm_state(self, surface, x, y, bomb_data: BombState):
        """Draw enhanced bomb with FSM state visualization"""
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
                actual_y = (MAP_SIZE - 1 - current_x) * TILE_SIZE

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
        
        freeze_color = safe_get_color('FREEZE_COLOR', 'frozen_bomb')
        bomb_black = safe_get_color('BOMB_BLACK', 'bomb_body')
        
        # Ice glow
        ice_size = int(bomb_size * 1.8)
        ice_surf = pygame.Surface((ice_size * 2, ice_size * 2), pygame.SRCALPHA)
        ice_rgba = create_rgba_color(freeze_color, 120, 'ice_glow')
        safe_pygame_draw_circle(ice_surf, ice_rgba, (ice_size, ice_size), ice_size, context="ice_glow")
        surface.blit(ice_surf, (center_x - ice_size, center_y - ice_size))
        
        # Main bomb body
        safe_pygame_draw_circle(surface, bomb_black, (center_x, center_y), bomb_size, context="frozen_bomb_body")
        safe_pygame_draw_circle(surface, freeze_color, (center_x, center_y), bomb_size, 3, context="frozen_bomb_border")

    def draw_remote_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in remote_idle state"""
        pulse = 0.6 + 0.4 * math.sin(self.time * 3)
        bomb_size = int(16 * pulse)
        
        cyan_color = safe_get_color('TEXT_CYAN', 'remote_bomb')
        bomb_black = safe_get_color('BOMB_BLACK', 'bomb_body')
        
        # Remote glow
        remote_size = int(bomb_size * 1.5)
        remote_surf = pygame.Surface((remote_size * 2, remote_size * 2), pygame.SRCALPHA)
        remote_rgba = create_rgba_color(cyan_color, 100, 'remote_glow')
        safe_pygame_draw_circle(remote_surf, remote_rgba, (remote_size, remote_size), remote_size, context="remote_glow")
        surface.blit(remote_surf, (center_x - remote_size, center_y - remote_size))
        
        # Main bomb body
        safe_pygame_draw_circle(surface, bomb_black, (center_x, center_y), bomb_size, context="remote_bomb_body")
        safe_pygame_draw_circle(surface, cyan_color, (center_x, center_y), bomb_size, 2, context="remote_bomb_border")

    def draw_ignited_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in ignited state"""
        pulse = 0.9 + 0.1 * math.sin(self.time * 10)
        bomb_size = int(18 * pulse)
        
        fuse_color = safe_get_color('BOMB_FUSE', 'ignited_bomb')
        bomb_black = safe_get_color('BOMB_BLACK', 'bomb_body')
        
        # Danger glow
        danger_size = int(bomb_size * 2)
        danger_surf = pygame.Surface((danger_size * 2, danger_size * 2), pygame.SRCALPHA)
        danger_rgba = create_rgba_color(fuse_color, 150, 'danger_glow')
        safe_pygame_draw_circle(danger_surf, danger_rgba, (danger_size, danger_size), danger_size, context="danger_glow")
        surface.blit(danger_surf, (center_x - danger_size, center_y - danger_size))
        
        # Main bomb body
        safe_pygame_draw_circle(surface, bomb_black, (center_x, center_y), bomb_size, context="ignited_bomb_body")
        safe_pygame_draw_circle(surface, fuse_color, (center_x, center_y), bomb_size, 3, context="ignited_bomb_border")

    def draw_standard_bomb(self, surface, center_x, center_y, bomb_data):
        """Draw bomb in standard armed state"""
        pulse = 0.8 + 0.2 * math.sin(self.time * 8)
        bomb_size = int(16 * pulse)

        bomb_black = safe_get_color('BOMB_BLACK', 'bomb_body')
        shadow_color = safe_get_color('SHADOW_RGB', 'bomb_shadow')

        # Drop shadow
        shadow_surf = pygame.Surface((bomb_size * 2 + 4, bomb_size * 2 + 4), pygame.SRCALPHA)
        shadow_rgba = create_rgba_color(shadow_color, 60, 'bomb_shadow')
        safe_pygame_draw_circle(shadow_surf, shadow_rgba, (bomb_size + 2, bomb_size + 2), bomb_size, context="bomb_shadow")
        surface.blit(shadow_surf, (center_x - bomb_size - 2, center_y - bomb_size - 2))

        # Main bomb body
        safe_pygame_draw_circle(surface, bomb_black, (center_x, center_y), bomb_size, context="bomb_body")
        safe_pygame_draw_circle(surface, (80, 80, 80), (center_x, center_y), bomb_size, 2, context="bomb_border")

        # Highlight
        safe_pygame_draw_circle(surface, (120, 120, 120), 
                         (center_x - bomb_size // 3, center_y - bomb_size // 3), bomb_size // 4, context="bomb_highlight")

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
                1: safe_get_color('PLAYER_1_DEAD', 'dead_player'), 
                2: safe_get_color('PLAYER_2_DEAD', 'dead_player'),
                3: safe_get_color('PLAYER_3_DEAD', 'dead_player'), 
                4: safe_get_color('PLAYER_4_DEAD', 'dead_player')
            }
            skin_color = safe_get_color('SKIN_DEAD', 'dead_skin')
            skin_shadow_color = safe_get_color('SKIN_SHADOW_DEAD', 'dead_skin_shadow')
        else:
            # Alive player colors with speed enhancement
            player_colors = {
                1: safe_get_color('PLAYER_1', 'alive_player'), 
                2: safe_get_color('PLAYER_2', 'alive_player'),
                3: safe_get_color('PLAYER_3', 'alive_player'), 
                4: safe_get_color('PLAYER_4', 'alive_player')
            }
            skin_color = safe_get_color('SKIN', 'alive_skin')
            skin_shadow_color = safe_get_color('SKIN_SHADOW', 'alive_skin_shadow')

        base_color = player_colors.get(player_id, safe_get_color('PLAYER_1', 'default_player'))

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
            char_y = (MAP_SIZE - 1 - current_x) * TILE_SIZE
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
        shadow_color = safe_get_color('SHADOW_RGB', 'player_shadow')
        shadow_rgba = create_rgba_color(shadow_color, shadow_alpha, 'player_shadow')
        safe_pygame_draw_ellipse(shadow_surf, shadow_rgba, (2, 0, TILE_SIZE, 16), context="player_shadow")
        surface.blit(shadow_surf, (x - 2, y + TILE_SIZE - 12))

        # Enhanced body with better gradient
        body_rect = pygame.Rect(center_x - 10, char_y - 4, 20, 24)
        body_dark_color = tuple(max(0, c - 50) for c in outfit_color)
        body_dark_color = validate_color(body_dark_color, 'player_body_dark')
        
        self.draw_gradient_rect(surface, outfit_color, body_dark_color, body_rect)
        
        body_border_color = tuple(max(0, c - 70) for c in outfit_color)
        body_border_color = validate_color(body_border_color, 'player_body_border')
        safe_pygame_draw_rect(surface, body_border_color, body_rect, 2, context="player_body_border")

        # Enhanced head with better shading
        head_y = char_y - 15
        safe_pygame_draw_circle(surface, skin_shadow_color, (center_x + 1, head_y + 1), 12, context="player_head_shadow")
        safe_pygame_draw_circle(surface, skin_color, (center_x, head_y), 12, context="player_head")
        
        head_border_color = tuple(max(0, c - 40) for c in skin_color)
        head_border_color = validate_color(head_border_color, 'player_head_border')
        safe_pygame_draw_circle(surface, head_border_color, (center_x, head_y), 12, 1, context="player_head_border")

        # Enhanced facial features
        safe_pygame_draw_ellipse(surface, (255, 255, 255), (center_x - 7, head_y - 5, 7, 5), context="player_left_eye")
        safe_pygame_draw_ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 5, 7, 5), context="player_right_eye")
        
        # Pupils with reflection
        safe_pygame_draw_circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2, context="player_left_pupil")
        safe_pygame_draw_circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2, context="player_right_pupil")
        safe_pygame_draw_circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1, context="player_left_reflection")
        safe_pygame_draw_circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1, context="player_right_reflection")

        # Player number badge
        badge_surf = pygame.Surface((20, 12), pygame.SRCALPHA)
        badge_bg_rgba = create_rgba_color((255, 255, 255), 220, 'player_badge_bg')
        safe_pygame_draw_rect(badge_surf, badge_bg_rgba, (0, 0, 20, 12), context="player_badge_bg")
        safe_pygame_draw_rect(badge_surf, (0, 0, 0), (0, 0, 20, 12), 1, context="player_badge_border1")
        safe_pygame_draw_rect(badge_surf, outfit_color, (1, 1, 18, 10), 1, context="player_badge_border2")

        num_text = self.small_font.render(str(player_id), True, (0, 0, 0))
        badge_surf.blit(num_text, (7, -1))
        surface.blit(badge_surf, (center_x - 10, char_y + 30))

    def draw_gradient_rect(self, surface, color1, color2, rect, vertical=True):
        """Enhanced gradient rectangle with smooth blending and safe colors"""
        # Validate colors
        color1 = validate_color(color1, 'gradient_color1')
        color2 = validate_color(color2, 'gradient_color2')
        
        if vertical:
            for y in range(rect.height):
                ratio = y / rect.height if rect.height > 0 else 0
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                
                gradient_color = validate_color((r, g, b), 'gradient_line')
                safe_pygame_draw_line(surface, gradient_color,
                               (rect.x, rect.y + y), (rect.x + rect.width, rect.y + y), context="gradient_line")

    def draw_enhanced_explosion_effect(self, surface, explosion):
        """Draw enhanced explosion effects"""
        elapsed = self.time - explosion['start_time']
        progress = elapsed / explosion['duration']

        if progress >= 1.0:
            return

        explosion_type = explosion['type']
        
        if explosion_type == 'bomb_center_enhanced':
            self.draw_enhanced_bomb_center_explosion(surface, explosion, progress)
        elif explosion_type == 'explosion_ray_enhanced':
            self.draw_enhanced_explosion_ray(surface, explosion, progress)
        elif explosion_type == 'coordinate_explosion':
            self.draw_coordinate_explosion(surface, explosion, progress)
        else:
            self.draw_standard_explosion(surface, explosion, progress)

    def draw_enhanced_bomb_center_explosion(self, surface, explosion, progress):
        """Draw enhanced central bomb explosion with multiple phases"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - explosion['x']) * TILE_SIZE + TILE_SIZE // 2
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
                    color = safe_get_color('EXPLOSION_CORE', 'bomb_explosion_core')
                elif layer == 1:
                    color = safe_get_color('EXPLOSION_MIDDLE', 'bomb_explosion_middle')
                else:
                    color = safe_get_color('EXPLOSION_OUTER', 'bomb_explosion_outer')
                
                explosion_surf = pygame.Surface((layer_radius * 2, layer_radius * 2), pygame.SRCALPHA)
                explosion_rgba = create_rgba_color(color, layer_alpha, f'bomb_explosion_layer_{layer}')
                safe_pygame_draw_circle(explosion_surf, explosion_rgba, 
                                 (layer_radius, layer_radius), layer_radius, context=f"bomb_explosion_{layer}")
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
                    color = validate_color((255, 255, int(200 + 55 * fire_intensity)), 'fire_white_hot')
                elif fire_intensity > 0.4:
                    color = validate_color((255, int(100 + 155 * fire_intensity), 0), 'fire_orange')
                else:
                    color = validate_color((int(100 + 155 * fire_intensity), 0, 0), 'fire_red')
                
                alpha = int(255 * fire_intensity)
                if alpha > 0:
                    particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                    particle_rgba = create_rgba_color(color, alpha, 'fire_particle')
                    safe_pygame_draw_circle(particle_surf, particle_rgba, 
                                     (particle_size, particle_size), particle_size, context="fire_particle")
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_enhanced_explosion_ray(self, surface, explosion, progress):
        """Draw enhanced explosion ray with realistic propagation"""
        if progress > 1.0:
            return

        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - explosion['x']) * TILE_SIZE + TILE_SIZE // 2
        distance = explosion.get('distance', 1)
        intensity = explosion.get('intensity', 1.0)
        bomb_type = explosion.get('bomb_type', 'normal_bomb')

        # Enhanced ray visualization
        ray_intensity = intensity * (1.0 - progress * 0.7)
        ray_size = int(TILE_SIZE * 0.9 * ray_intensity)

        if ray_size > 0:
            # Type-specific effects
            if bomb_type == 'remote_ignition':
                base_color = safe_get_color('TEXT_CYAN', 'remote_explosion_ray')
            elif bomb_type == 'freeze_bomb':
                base_color = safe_get_color('FREEZE_COLOR', 'freeze_explosion_ray')
            else:
                base_color = safe_get_color('EXPLOSION_MIDDLE', 'standard_explosion_ray')

            # Multi-layer explosion ray
            for layer in range(3):
                layer_size = max(1, ray_size - layer * 6)
                layer_alpha = int(200 * ray_intensity * (1 - layer * 0.3))
                
                if layer == 0:
                    color = safe_get_color('EXPLOSION_CORE', 'explosion_ray_core')
                elif layer == 1:
                    color = base_color
                else:
                    color = safe_get_color('EXPLOSION_OUTER', 'explosion_ray_outer')

                ray_surf = pygame.Surface((layer_size * 2, layer_size * 2), pygame.SRCALPHA)
                ray_rgba = create_rgba_color(color, layer_alpha, f'explosion_ray_layer_{layer}')
                safe_pygame_draw_circle(ray_surf, ray_rgba, 
                                 (layer_size, layer_size), layer_size, context=f"explosion_ray_{layer}")
                surface.blit(ray_surf, (center_x - layer_size, center_y - layer_size))

            # Sparks and debris
            if progress < 0.5:
                spark_color = safe_get_color('EXPLOSION_SPARK', 'explosion_sparks')
                for i in range(distance * 2):
                    if random.random() < 0.6:
                        spark_angle = random.random() * 2 * math.pi
                        spark_distance = random.randint(ray_size, ray_size + 15)
                        spark_x = center_x + int(math.cos(spark_angle) * spark_distance)
                        spark_y = center_y + int(math.sin(spark_angle) * spark_distance)
                        
                        spark_size = random.randint(1, 3)
                        safe_pygame_draw_circle(surface, spark_color, (spark_x, spark_y), spark_size, context="explosion_spark")

    def draw_coordinate_explosion(self, surface, explosion, progress):
        """Draw explosion at specific coordinates from explosion event"""
        # Apply coordinate transformation like bombs and players
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - explosion['x']) * TILE_SIZE + TILE_SIZE // 2
        explosion_type = explosion.get('explosion_type', 'standard')
        
        # Pulsing explosion with type-specific effects
        pulse_size = int(30 * (1 - progress) * math.sin(progress * math.pi))
        
        if pulse_size > 0:
            # Color based on explosion type
            if explosion_type == 'ice':
                color = safe_get_color('FREEZE_COLOR', 'ice_explosion')
            elif explosion_type == 'remote':
                color = safe_get_color('TEXT_CYAN', 'remote_explosion')
            else:
                color = safe_get_color('EXPLOSION_MIDDLE', 'standard_explosion')

            explosion_surf = pygame.Surface((pulse_size * 2, pulse_size * 2), pygame.SRCALPHA)
            alpha = int(200 * (1 - progress))
            explosion_rgba = create_rgba_color(color, alpha, 'coordinate_explosion')
            safe_pygame_draw_circle(explosion_surf, explosion_rgba, 
                             (pulse_size, pulse_size), pulse_size, context="coordinate_explosion")
            surface.blit(explosion_surf, (center_x - pulse_size, center_y - pulse_size))

    def draw_standard_explosion(self, surface, explosion, progress):
        """Draw standard explosion effect"""
        # Apply coordinate transformation like bombs and players
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - explosion['x']) * TILE_SIZE + TILE_SIZE // 2
        intensity = explosion.get('intensity', 1.0)

        explosion_size = int(25 * intensity * (1 - progress))
        if explosion_size > 0:
            alpha = int(150 * (1 - progress))
            explosion_surf = pygame.Surface((explosion_size * 2, explosion_size * 2), pygame.SRCALPHA)
            explosion_color = safe_get_color('EXPLOSION_MIDDLE', 'standard_explosion')
            explosion_rgba = create_rgba_color(explosion_color, alpha, 'standard_explosion')
            safe_pygame_draw_circle(explosion_surf, explosion_rgba,
                             (explosion_size, explosion_size), explosion_size, context="standard_explosion")
            surface.blit(explosion_surf, (center_x - explosion_size, center_y - explosion_size))

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
            elif effect_type == 'screen_flash':
                self.draw_screen_flash_effect(surface, effect)

    def draw_enhanced_bomb_kick_effect(self, surface, effect):
        """Draw enhanced bomb kick effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2
        bomb_type = effect.get('bomb_type', 'normal_bomb')

        # Impact burst with type-specific colors
        burst_size = int(35 * progress)
        alpha = int(150 * (1 - progress))

        if burst_size > 0 and alpha > 0:
            # Type-specific colors
            if bomb_type == 'remote_ignition':
                burst_color = safe_get_color('TEXT_CYAN', 'kick_remote_burst')
            elif bomb_type == 'freeze_bomb':
                burst_color = safe_get_color('FREEZE_COLOR', 'kick_freeze_burst')
            else:
                burst_color = validate_color((255, 200, 100), 'kick_normal_burst')

            # Starburst pattern
            for angle in range(0, 360, 30):
                end_x = center_x + int(burst_size * math.cos(math.radians(angle)))
                end_y = center_y + int(burst_size * math.sin(math.radians(angle)))
                
                line_width = max(1, int(4 * (1 - progress)))
                burst_rgba = create_rgba_color(burst_color, alpha, 'kick_burst_line')
                safe_pygame_draw_line(surface, burst_rgba, (center_x, center_y), (end_x, end_y), line_width, context="kick_burst")

    def draw_enhanced_speed_boost_effect(self, surface, effect):
        """Draw enhanced speed boost effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2
        speed = effect.get('speed', 1)

        # Speed aura
        aura_size = int(40 * (1 - progress) * (speed / 4))
        
        if aura_size > 0:
            aura_alpha = int(120 * (1 - progress))
            speed_color = safe_get_color('SPEED_BOOST_COLOR', 'speed_boost_aura')

            aura_surf = pygame.Surface((aura_size * 2, aura_size * 2), pygame.SRCALPHA)
            aura_rgba = create_rgba_color(speed_color, aura_alpha, 'speed_boost_aura')
            safe_pygame_draw_circle(aura_surf, aura_rgba, 
                             (aura_size, aura_size), aura_size, context="speed_boost_aura")
            surface.blit(aura_surf, (center_x - aura_size, center_y - aura_size))

    def draw_speed_particle_effect(self, surface, effect):
        """Draw speed particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2
        direction = effect.get('direction', 'north')
        particle_id = effect.get('particle_id', 0)

        # Direction vectors
        directions = {'north': (0, -1), 'south': (0, 1), 'east': (1, 0), 'west': (-1, 0)}
        dx, dy = directions.get(direction, (0, 0))
        
        # Particle position
        distance = progress * 30 + particle_id * 5
        particle_x = center_x + dx * distance
        particle_y = center_y + dy * distance
        
        # Particle properties
        particle_size = max(1, int(6 * (1 - progress)))
        alpha = int(180 * (1 - progress))
        
        if alpha > 0 and particle_size > 0:
            particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
            speed_color = safe_get_color('SPEED_BOOST_COLOR', 'speed_particle')
            particle_rgba = create_rgba_color(speed_color, alpha, 'speed_particle')
            safe_pygame_draw_circle(particle_surf, particle_rgba,
                             (particle_size, particle_size), particle_size, context="speed_particle")
            surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_kick_particle_effect(self, surface, effect):
        """Draw kick particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2
        angle = effect.get('angle', 0)

        # Particle trajectory
        distance = progress * 40
        particle_x = center_x + int(math.cos(math.radians(angle)) * distance)
        particle_y = center_y + int(math.sin(math.radians(angle)) * distance)
        
        particle_size = max(1, int(4 * (1 - progress)))
        alpha = int(150 * (1 - progress))
        
        if alpha > 0:
            kick_color = safe_get_color('TEXT_ORANGE', 'kick_particle')
            kick_rgba = create_rgba_color(kick_color, alpha, 'kick_particle')
            safe_pygame_draw_circle(surface, kick_rgba, (particle_x, particle_y), particle_size, context="kick_particle")

    def draw_death_particle_effect(self, surface, effect):
        """Draw death particle effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2
        particle_id = effect.get('particle_id', 0)

        # Random particle movement
        angle = (particle_id * 45 + progress * 180) % 360
        distance = progress * 25
        particle_x = center_x + int(math.cos(math.radians(angle)) * distance)
        particle_y = center_y + int(math.sin(math.radians(angle)) * distance) - int(progress * 15)
        
        particle_size = max(1, int(5 * (1 - progress)))
        alpha = int(180 * (1 - progress))
        
        if alpha > 0:
            death_color = safe_get_color('TEXT_RED', 'death_particle')
            death_rgba = create_rgba_color(death_color, alpha, 'death_particle')
            safe_pygame_draw_circle(surface, death_rgba, (particle_x, particle_y), particle_size, context="death_particle")

    def draw_enhanced_damage_effect(self, surface, effect):
        """Draw enhanced damage effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2 - int(progress * 20)
        damage = effect.get('damage', 1)

        # Damage text effect
        alpha = int(255 * (1 - progress))
        if alpha > 0:
            font_size = max(14, int(18 + damage * 4))
            damage_font = pygame.font.Font(None, font_size)
            damage_text = f"-{damage}"
            
            damage_color = safe_get_color('TEXT_RED', 'damage_text')
            damage_rgba = create_rgba_color(damage_color, alpha, 'damage_text')
            
            # Create text surface with alpha
            text_surf = damage_font.render(damage_text, True, damage_color)
            alpha_surf = pygame.Surface(text_surf.get_size(), pygame.SRCALPHA)
            alpha_surf.fill((*damage_color, alpha))
            text_surf.blit(alpha_surf, (0, 0), special_flags=pygame.BLEND_RGBA_MULT)
            
            surface.blit(text_surf, (center_x - text_surf.get_width() // 2, center_y))

    def draw_screen_flash_effect(self, surface, effect):
        """Draw screen flash effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        intensity = effect.get('intensity', 0.5)
        color = effect.get('color', (255, 255, 255))
        
        flash_alpha = int(255 * intensity * (1 - progress))
        if flash_alpha > 0:
            flash_surf = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE), pygame.SRCALPHA)
            flash_color = validate_color(color, 'screen_flash')
            flash_rgba = create_rgba_color(flash_color, flash_alpha, 'screen_flash')
            flash_surf.fill(flash_rgba)
            surface.blit(flash_surf, (0, 0))

    def draw_enhanced_player_death_effect(self, surface, effect):
        """Draw enhanced player death effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']
        
        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = (MAP_SIZE - 1 - effect['x']) * TILE_SIZE + TILE_SIZE // 2

        # Death spiral effect
        if progress < 0.7:
            spiral_progress = progress / 0.7
            spiral_size = int(50 * spiral_progress)
            
            text_red = safe_get_color('TEXT_RED', 'death_effect')
            
            for i in range(8):
                angle = (spiral_progress * 720 + i * 45) % 360
                particle_x = center_x + int(spiral_size * math.cos(math.radians(angle)))
                particle_y = center_y + int(spiral_size * math.sin(math.radians(angle)))
                
                particle_alpha = int(200 * (1 - spiral_progress))
                particle_size = max(1, int(8 * (1 - spiral_progress)))
                
                if particle_alpha > 0:
                    particle_surf = pygame.Surface((particle_size * 2, particle_size * 2), pygame.SRCALPHA)
                    particle_rgba = create_rgba_color(text_red, particle_alpha, 'death_particle')
                    safe_pygame_draw_circle(particle_surf, particle_rgba,
                                     (particle_size, particle_size), particle_size, context="death_particle")
                    surface.blit(particle_surf, (particle_x - particle_size, particle_y - particle_size))

    def draw_enhanced_player_stats_panel(self):
        """Draw enhanced player statistics panel"""
        self.player_panel_surface.fill(safe_get_color('UI_BACKGROUND', 'player_panel_bg'))

        # Enhanced panel border
        panel_border_color = safe_get_color('PANEL_BORDER', 'player_panel_border')
        safe_pygame_draw_rect(self.player_panel_surface, panel_border_color, 
                        (0, 0, PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2, context="player_panel_border")

        # Panel title with glow
        title_text = f"GN {self.gn_id.upper()} PLAYERS"
        title_shadow_color = safe_get_color('TEXT_SHADOW', 'panel_title_shadow')
        title_main_color = safe_get_color('TEXT_GOLD', 'panel_title_main')
        
        title_shadow = self.title_font.render(title_text, True, title_shadow_color)
        title_main = self.title_font.render(title_text, True, title_main_color)

        # Multi-layer title effect
        for offset in [(2, 2), (1, 1), (0, 0)]:
            if offset == (0, 0):
                self.player_panel_surface.blit(title_main, (12 + offset[0], 12 + offset[1]))
            else:
                self.player_panel_surface.blit(title_shadow, (12 + offset[0], 12 + offset[1]))

        # Connection status
        status_y = 45
        status_text = f"Socket: {self.connection_status}"
        status_color = safe_get_color('TEXT_GREEN', 'status_ok') if self.connection_status == "Connected" else safe_get_color('TEXT_RED', 'status_error')
        status_surface = self.mini_font.render(status_text, True, status_color)
        self.player_panel_surface.blit(status_surface, (12, status_y))

        # Local player indicator
        local_text = f"Local Player: {self.local_player_id}"
        local_color = safe_get_color('TEXT_CYAN', 'local_player_ok')
        if self.local_player_dead:
            local_color = safe_get_color('TEXT_RED', 'local_player_dead')
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
                1: safe_get_color('PLAYER_1_DEAD', 'dead_player_1'), 
                2: safe_get_color('PLAYER_2_DEAD', 'dead_player_2'),
                3: safe_get_color('PLAYER_3_DEAD', 'dead_player_3'), 
                4: safe_get_color('PLAYER_4_DEAD', 'dead_player_4')
            }
            text_color = safe_get_color('TEXT_GREY', 'dead_text')
            status_text = "DEAD"
            status_color = safe_get_color('TEXT_RED', 'dead_status')
        else:
            player_colors = {
                1: safe_get_color('PLAYER_1', 'alive_player_1'), 
                2: safe_get_color('PLAYER_2', 'alive_player_2'),
                3: safe_get_color('PLAYER_3', 'alive_player_3'), 
                4: safe_get_color('PLAYER_4', 'alive_player_4')
            }
            text_color = safe_get_color('TEXT_WHITE', 'alive_text')
            if player_data:
                status_text = f"ALIVE at ({player_data.x}, {player_data.y})"
                status_color = safe_get_color('TEXT_GREEN', 'alive_status')
            else:
                status_text = "WAITING"
                status_color = safe_get_color('TEXT_ORANGE', 'waiting_status')

        player_color = player_colors.get(player_id, safe_get_color('PLAYER_1', 'default_player'))

        # Enhanced background with animated border for local player
        bg_rect = pygame.Rect(10, y_pos, PLAYER_PANEL_WIDTH - 20, height - 10)
        border_pulse = 0.7 + 0.3 * math.sin(self.time * 2 + player_id)
        
        if is_local:
            border_pulse = 1.0 + 0.5 * math.sin(self.time * 4)  # More pronounced for local player
        
        # Background gradient with safe colors
        bg_alpha = int((30 if is_dead else 60) + 20 * border_pulse)
        if is_local and not is_dead:
            bg_alpha = int(80 + 40 * border_pulse)  # Brighter for local player
        
        # Create safe background colors
        bg_color1 = player_color
        bg_alpha_clamped = max(0, min(255, bg_alpha))
        bg_color2 = tuple(max(0, min(255, int(c * 0.5))) for c in bg_color1)
        
        self.draw_gradient_rect(surface, bg_color1, bg_color2, bg_rect)
        
        # Animated border with safe color
        border_intensity = max(0.3, min(1.5, border_pulse))
        border_color = tuple(max(0, min(255, int(c * border_intensity))) for c in player_color)
        border_color = validate_color(border_color, 'player_stats_border')
        border_width = 3 if is_local else 2
        safe_pygame_draw_rect(surface, border_color, bg_rect, border_width, context="player_stats_border")

        # Local player indicator
        if is_local:
            local_indicator = "YOU"
            local_gold = safe_get_color('TEXT_GOLD', 'local_indicator')
            local_surface = self.mini_font.render(local_indicator, True, local_gold)
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
        health_color = safe_get_color('TEXT_GREY', 'health_dead') if is_dead else safe_get_color('TEXT_RED', 'health_alive')
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
                speed_color = safe_get_color('TEXT_PURPLE', 'speed_high')
            elif current_speed > 1:
                speed_color = safe_get_color('TEXT_GREEN', 'speed_medium')
            else:
                speed_color = safe_get_color('TEXT_CYAN', 'speed_low')
                
            speed_surface = self.small_font.render(speed_text, True, speed_color)
            surface.blit(speed_surface, (avatar_x + 35, stats_start_y + stat_height * 3))

    def draw_enhanced_mini_player(self, surface, x, y, player_num, scale=1.0, is_dead=False):
        """Draw enhanced mini player"""
        if is_dead:
            player_colors = {
                1: safe_get_color('PLAYER_1_DEAD', 'mini_dead_1'), 
                2: safe_get_color('PLAYER_2_DEAD', 'mini_dead_2'),
                3: safe_get_color('PLAYER_3_DEAD', 'mini_dead_3'), 
                4: safe_get_color('PLAYER_4_DEAD', 'mini_dead_4')
            }
            skin_color = safe_get_color('SKIN_DEAD', 'mini_dead_skin')
        else:
            player_colors = {
                1: safe_get_color('PLAYER_1', 'mini_alive_1'), 
                2: safe_get_color('PLAYER_2', 'mini_alive_2'),
                3: safe_get_color('PLAYER_3', 'mini_alive_3'), 
                4: safe_get_color('PLAYER_4', 'mini_alive_4')
            }
            skin_color = safe_get_color('SKIN', 'mini_alive_skin')

        outfit_color = player_colors.get(player_num, safe_get_color('PLAYER_1', 'mini_default'))
        size = int(18 * scale)

        # Enhanced body
        body_rect = pygame.Rect(x - size // 2, y, size, int(size * 1.3))
        body_dark_color = tuple(max(0, c - 40) for c in outfit_color)
        body_dark_color = validate_color(body_dark_color, 'mini_body_dark')
        
        self.draw_gradient_rect(surface, outfit_color, body_dark_color, body_rect)
        
        body_border_color = tuple(max(0, c - 60) for c in outfit_color)
        body_border_color = validate_color(body_border_color, 'mini_body_border')
        safe_pygame_draw_rect(surface, body_border_color, body_rect, 1, context="mini_body_border")

        # Enhanced head
        head_y = y - size // 2
        safe_pygame_draw_circle(surface, skin_color, (x, head_y), size // 2, context="mini_head")
        
        head_border_color = tuple(max(0, c - 30) for c in skin_color)
        head_border_color = validate_color(head_border_color, 'mini_head_border')
        safe_pygame_draw_circle(surface, head_border_color, (x, head_y), size // 2, 1, context="mini_head_border")

        # Face
        if is_dead:
            # X eyes for dead players
            eye_color = validate_color((100, 100, 100), 'dead_eyes')
            eye_size = 2
            safe_pygame_draw_line(surface, eye_color, 
                           (x - 5, head_y - 5), (x - 3, head_y - 3), eye_size, context="dead_eye_x1")
            safe_pygame_draw_line(surface, eye_color, 
                           (x - 3, head_y - 5), (x - 5, head_y - 3), eye_size, context="dead_eye_x2")
            safe_pygame_draw_line(surface, eye_color, 
                           (x + 3, head_y - 5), (x + 5, head_y - 3), eye_size, context="dead_eye_x3")
            safe_pygame_draw_line(surface, eye_color, 
                           (x + 5, head_y - 5), (x + 3, head_y - 3), eye_size, context="dead_eye_x4")
        else:
            # Normal eyes
            safe_pygame_draw_circle(surface, (0, 0, 0), (x - size // 4, head_y - 2), 1, context="mini_left_eye")
            safe_pygame_draw_circle(surface, (0, 0, 0), (x + size // 4, head_y - 2), 1, context="mini_right_eye")

        # Player number badge
        badge_surf = pygame.Surface((16, 10), pygame.SRCALPHA)
        badge_alpha = int(220 * scale) if not is_dead else 120
        badge_bg_rgba = create_rgba_color((255, 255, 255), badge_alpha, 'mini_badge_bg')
        safe_pygame_draw_rect(badge_surf, badge_bg_rgba, (0, 0, 16, 10), context="mini_badge_bg")
        safe_pygame_draw_rect(badge_surf, (0, 0, 0), (0, 0, 16, 10), 1, context="mini_badge_border")

        num_text = self.mini_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (5, -1))
        surface.blit(badge_surf, (x - 8, y + int(size * 1.3) + 3))

    def draw_enhanced_mini_heart(self, surface, x, y, color, is_dead=False):
        """Draw enhanced mini heart"""
        size = 7 if not is_dead else 5
        alpha = 255 if not is_dead else 120
        
        # Validate color
        heart_color = validate_color(color, 'mini_heart')
        
        # Heart shape with alpha
        heart_rgba = create_rgba_color(heart_color, alpha, 'mini_heart_shape')
        
        # Create temporary surface for heart
        heart_surf = pygame.Surface((14, 12), pygame.SRCALPHA)
        safe_pygame_draw_circle(heart_surf, heart_rgba, (x - 2 - (x-7), y - 1 - (y-6)), 2, context="heart_left")
        safe_pygame_draw_circle(heart_surf, heart_rgba, (x + 2 - (x-7), y - 1 - (y-6)), 2, context="heart_right")
        
        points = [(x - 3 - (x-7), y - (y-6)), (x + 3 - (x-7), y - (y-6)), (x - (x-7), y + 5 - (y-6))]
        if len(points) >= 3:
            safe_pygame_draw_polygon(heart_surf, heart_rgba, points, context="heart_triangle")
        
        surface.blit(heart_surf, (x-7, y-6))

    def draw_enhanced_timer_panel(self):
        """Draw enhanced timer panel"""
        self.timer_panel_surface.fill(safe_get_color('UI_BACKGROUND', 'timer_panel_bg'))
        
        panel_border_color = safe_get_color('PANEL_BORDER', 'timer_panel_border')
        safe_pygame_draw_rect(self.timer_panel_surface, panel_border_color, 
                        (0, 0, TIMER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE), 2, context="timer_panel_border")

        # Title
        title_text = "GN TIMERS"
        title_color = safe_get_color('TEXT_GOLD', 'timer_panel_title')
        title_surface = self.font.render(title_text, True, title_color)
        self.timer_panel_surface.blit(title_surface, (10, 10))

        # GN Information
        gn_info_y = 40
        gn_info = [
            f"GN: {self.gn_id.upper()}",
            f"Port: {self.socket_manager.port}",
            f"Local Player: {self.local_player_id}"
        ]
        
        for i, info in enumerate(gn_info):
            info_color = safe_get_color('TEXT_CYAN', 'gn_info')
            if i == 2 and self.local_player_dead:
                info_color = safe_get_color('TEXT_RED', 'gn_info_dead')
            info_surface = self.mini_font.render(info, True, info_color)
            self.timer_panel_surface.blit(info_surface, (10, gn_info_y + i * 15))

        # Performance info
        perf_y = MAP_SIZE * TILE_SIZE - 80
        fps_text = f"FPS: {self.current_fps:.1f}"
        fps_color = safe_get_color('TEXT_GREEN', 'fps_good') if self.current_fps > 50 else safe_get_color('TEXT_ORANGE', 'fps_low')
        fps_surface = self.mini_font.render(fps_text, True, fps_color)
        self.timer_panel_surface.blit(fps_surface, (10, perf_y))
        
        msg_text = f"Messages: {self.message_count}"
        msg_color = safe_get_color('TEXT_WHITE', 'msg_count')
        msg_surface = self.mini_font.render(msg_text, True, msg_color)
        self.timer_panel_surface.blit(msg_surface, (10, perf_y + 15))

        # Blit to virtual surface
        self.virtual_surface.blit(self.timer_panel_surface, (TIMER_OFFSET_X, MAP_OFFSET_Y))

    def draw_enhanced_powerups_panel(self):
        """Draw enhanced power-ups panel"""
        self.powerup_panel_surface.fill(safe_get_color('UI_BACKGROUND', 'powerup_panel_bg'))
        
        panel_border_color = safe_get_color('PANEL_BORDER', 'powerup_panel_border')
        safe_pygame_draw_rect(self.powerup_panel_surface, panel_border_color, 
                        (0, 0, WINDOW_WIDTH, POWERUP_PANEL_HEIGHT), 2, context="powerup_panel_border")

        # Title
        title_text = f"{self.gn_id.upper()} CONTROLS & POWER-UPS"
        title_color = safe_get_color('TEXT_GOLD', 'powerup_panel_title')
        title_surface = self.title_font.render(title_text, True, title_color)
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Controls info
        controls = [
            "W - Move Up | A - Move Left | S - Move Down | D - Move Right | SPACE - Place bomb | Q - Ignite Bomb (remote only) | Click tiles to inspect",
            f"GN Socket connection to localhost:{self.socket_manager.port}, receiving updates from CN Graphics"
        ]
        
        control_color = safe_get_color('TEXT_WHITE', 'control_text')
        for i, control in enumerate(controls):
            control_surface = self.small_font.render(control, True, control_color)
            self.powerup_panel_surface.blit(control_surface, (20, 50 + i * 20))

        # Enhanced power-up legend using actual icons
        powerup_types = [
            ('move_speed', "SPEED", safe_get_color('TEXT_CYAN', 'powerup_speed')),
            ('remote_ignition', "REMOTE BOMBS", safe_get_color('TEXT_ORANGE', 'powerup_remote')),
            ('plus_bombs', "EXTRA BOMBS", safe_get_color('TEXT_GOLD', 'powerup_bombs')),
            ('repeat_bombs', "REPEATING BOMBS", safe_get_color('TEXT_GOLD', 'powerup_repeat')),
            ('bigger_explosion', "BLAST RADIUS", safe_get_color('TEXT_RED', 'powerup_blast')),
            ('plus_life', "EXTRA LIFE", safe_get_color('TEXT_GREEN', 'powerup_life')),
            ('freeze_bomb', "FREEZE UPON TOUCH", safe_get_color('FREEZE_COLOR', 'powerup_freeze')),
            ('phased', "PHASED MOVEMENT", safe_get_color('TEXT_PURPLE', 'powerup_ghost')),
            ('kick_bomb', "KICK BOMB", validate_color((255, 100, 255), 'powerup_kick'))
        ]

        start_x = 20
        start_y = 100
        panel_icon_size = 50  # 50x50 pixel icons
        
        for i, (powerup_type, name, color) in enumerate(powerup_types):
            x = start_x + (i % 5) * 200  # 5 powerups per row with 200px spacing
            y = start_y + (i // 5) * 60   # Rows with 60px vertical spacing
            
            # Animated glow
            glow_intensity = 0.7 + 0.3 * math.sin(self.time * 3 + i * 0.5)
            
            # Try to use the loaded panel icon (now properly sized at 50px)
            panel_icon = self.powerup_panel_icons.get(powerup_type)
            if panel_icon:
                self.powerup_panel_surface.blit(panel_icon, (x, y))
            else:
                # Fallback to colored square if icon not available
                fallback_color = tuple(int(c * glow_intensity) for c in color)
                fallback_color = validate_color(fallback_color, 'powerup_fallback')
                safe_pygame_draw_rect(self.powerup_panel_surface, fallback_color, (x, y, panel_icon_size, panel_icon_size), context="powerup_fallback")
                white_color = safe_get_color('TEXT_WHITE', 'powerup_border')
                safe_pygame_draw_rect(self.powerup_panel_surface, white_color, (x, y, panel_icon_size, panel_icon_size), 1, context="powerup_border")
            
            name_surface = self.small_font.render(name, True, color)
            self.powerup_panel_surface.blit(name_surface, (x + panel_icon_size + 10, y + 18))  # Centered text positioning

        # Blit to virtual surface
        self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

    def draw_death_overlay(self):
        """Draw the YOU DIED overlay - full black screen when local player dies"""
        if not self.local_player_dead:
            return
    
        # Create full black screen overlay that covers everything
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.fill((0, 0, 0))  # Pure black background
        
        # Pulsing effect for dramatic impact
        pulse = 0.8 + 0.2 * math.sin(self.time * 4)
        
        # Main "YOU DIED" text in red
        death_text = "YOU DIED"
        death_color = safe_get_color('DEATH_TEXT', 'death_overlay')
        pulsed_death_color = tuple(int(c * pulse) for c in death_color)
        pulsed_death_color = validate_color(pulsed_death_color, 'death_text_pulsed')
        
        text_main = self.death_font.render(death_text, True, pulsed_death_color)
        
        # Center the text
        text_rect = text_main.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
        
        # Draw the text on the black overlay
        overlay.blit(text_main, text_rect)
    
        # Additional message
        time_since_death = time.time() - self.death_message_start_time
        sub_text = f"Game continues... ({time_since_death:.1f}s)"
        sub_color = safe_get_color('TEXT_WHITE', 'death_sub_text')
        sub_surface = self.font.render(sub_text, True, sub_color)
        sub_rect = sub_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 80))
        overlay.blit(sub_surface, sub_rect)
    
        # Instructions
        instruction_text = "Press ESC to exit"
        instruction_color = safe_get_color('TEXT_GREY', 'death_instruction')
        instruction_surface = self.small_font.render(instruction_text, True, instruction_color)
        instruction_rect = instruction_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 110))
        overlay.blit(instruction_surface, instruction_rect)
    
        # Blit overlay to virtual surface - this will cover everything
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
                    print("Manual refresh requested")
                    if self.socket_manager.connected:
                        self.socket_manager.send_message({
                            "type": "refresh_request",
                            "timestamp": int(time.time() * 1000)
                        })
                elif event.key == pygame.K_h:
                    print(f"GN {self.gn_id.upper()} Game Visualizer Controls:")
                    print("   ESC - Exit")
                    print("   R - Request refresh from server")
                    print("   H - This help")
                    print("   Click tiles - Inspect with enhanced details")
                elif event.key in (pygame.K_w, pygame.K_a, pygame.K_s, pygame.K_d,
                         pygame.K_SPACE, pygame.K_q):
                    self.socket_manager.send_normal_message(pygame.key.name(event.key))
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

            # Convert screen coordinates to game coordinates
            # pixel_x = y * TILE_SIZE, so y = pixel_x // TILE_SIZE
            # pixel_y = (MAP_SIZE - 1 - x) * TILE_SIZE, so x = MAP_SIZE - 1 - (pixel_y // TILE_SIZE)
            tile_y = int(map_mouse_x // TILE_SIZE)
            tile_x = MAP_SIZE - 1 - int(map_mouse_y // TILE_SIZE)

            if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                self.selected_tile = (tile_x, tile_y)
                self.inspect_enhanced_tile(tile_x, tile_y)

    def inspect_enhanced_tile(self, tile_x, tile_y):
        """Enhanced tile inspection"""
        tile_type = self.current_game_state.tiles[tile_x][tile_y]
        powerup = self.current_game_state.powerups[tile_x][tile_y]

        tile_names = {0: 'FREE_SPACE', 1: 'WOODEN_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL'}
        tile_name = tile_names.get(tile_type, f'UNKNOWN_{tile_type}')

        print(f"\nGN {self.gn_id.upper()} Tile Inspection at ({tile_x}, {tile_y}):")
        print(f"   Tile Type: {tile_name}")
        print(f"   Power-up: {powerup}")

        # Check for players
        players_here = [p for p in self.current_game_state.players.values() 
                       if p.x == tile_x and p.y == tile_y]
        for player in players_here:
            local_indicator = " (YOU!)" if player.player_id == self.local_player_id else ""
            print(f"   Player {player.player_id}{local_indicator}:")
            print(f"      Health: {player.health}")
            print(f"      Speed: {player.speed}")
            print(f"      Direction: {player.direction}")
            print(f"      Timers: Move={player.timers.movement_timer}ms, Immunity={player.timers.immunity_timer}ms")

        # Check for bombs
        bombs_here = [b for b in self.current_game_state.bombs.values() 
                     if b.x == tile_x and b.y == tile_y]
        for bomb in bombs_here:
            owner_indicator = " (YOUR BOMB!)" if bomb.owner == self.local_player_id else ""
            print(f"   Bomb{owner_indicator}:")
            print(f"      Type: {bomb.bomb_type}")
            print(f"      Timer: {bomb.timer}ms")
            print(f"      Owner: Player {bomb.owner}")
            print(f"      FSM State: {bomb.status}")

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
        bg_color = safe_get_color('BACKGROUND', 'main_bg')
        panel_bg_color = safe_get_color('PANEL_BG', 'main_panel_bg')
        
        bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
        self.draw_gradient_rect(self.virtual_surface, bg_color, panel_bg_color, bg_rect)

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
        main_bg_color = safe_get_color('BACKGROUND', 'screen_bg')
        self.screen.fill(main_bg_color)
        x_offset = (self.current_width - scaled_surface.get_width()) // 2
        y_offset = (self.current_height - scaled_surface.get_height()) // 2
        self.screen.blit(scaled_surface, (max(0, x_offset), max(0, y_offset)))

        # Enhanced status display - but only if player is alive
        if not self.local_player_dead:
            self.draw_enhanced_status_display()

    def draw_enhanced_status_display(self):
        """Draw enhanced status display"""
        status_y = 10
        
        if self.waiting_for_initial_map:
            status_text = f"Waiting for game to start on GN {self.gn_id.upper()}..."
            color = safe_get_color('TEXT_ORANGE', 'status_waiting')
        elif not self.socket_manager.connected:
            status_text = f"Disconnected from GN {self.gn_id.upper()} server"
            color = safe_get_color('TEXT_RED', 'status_disconnected')
        else:
            dead_count = len(self.current_game_state.dead_players)
            active_animations = (len(self.player_animations) + len(self.bomb_animations) + 
                               len(self.explosion_animations) + len(self.game_effects))
            
            death_indicator = " | YOU ARE DEAD" if self.local_player_dead else ""
            status_text = (f"GN {self.gn_id.upper()} Connected | Player {self.local_player_id} | "
                         f"Dead: {dead_count} | Animations: {active_animations} | "
                         f"FPS: {self.current_fps:.1f}{death_indicator}")
            color = safe_get_color('TEXT_RED', 'status_dead') if self.local_player_dead else safe_get_color('TEXT_GREEN', 'status_connected')

        status_surface = self.small_font.render(status_text, True, color)
        status_rect = status_surface.get_rect(topleft=(10, status_y))
        
        # Enhanced background
        bg_rect = status_rect.inflate(8, 4)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        bg_rgba = create_rgba_color((0, 0, 0), 150, 'status_bg')
        safe_pygame_draw_rect(bg_surf, bg_rgba, (0, 0, bg_rect.width, bg_rect.height), context="status_bg")
        safe_pygame_draw_rect(bg_surf, color, (0, 0, bg_rect.width, bg_rect.height), 1, context="status_border")
        
        self.screen.blit(bg_surf, bg_rect)
        self.screen.blit(status_surface, status_rect)

    def run_enhanced_game_loop(self):
        """Main enhanced game loop with GN socket communication"""
        print(f"Starting GN {self.gn_id.upper()} Game Visualizer...")
        print(f"Connecting to GN server at localhost:{self.socket_manager.port}")
        print(f"Local player: Player {self.local_player_id}")
        print("Features: Enhanced graphics, GN socket communication, FSM states, real-time effects")
        
        # Initial connection attempt
        if not self.connect_to_server():
            print("Failed to connect to GN server. Will retry automatically...")

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
                    print(f"Reconnected to GN {self.gn_id.upper()} server!")
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
                main_bg_color = safe_get_color('BACKGROUND', 'waiting_bg')
                self.screen.fill(main_bg_color)
                
                waiting_pulse = 0.8 + 0.2 * math.sin(self.time * 3)
                
                main_text = f"Waiting for game to start..."
                text_color = safe_get_color('TEXT_WHITE', 'waiting_text')
                pulsed_color = tuple(int(c * waiting_pulse) for c in text_color)
                pulsed_color = validate_color(pulsed_color, 'waiting_text_pulsed')
                
                main_surface = self.font.render(main_text, True, pulsed_color)
                main_rect = main_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 30))
                self.screen.blit(main_surface, main_rect)

                sub_text = f"GN {self.gn_id.upper()} - Player {self.local_player_id} - Port {self.socket_manager.port}"
                sub_color = safe_get_color('TEXT_CYAN', 'waiting_sub_text')
                sub_surface = self.small_font.render(sub_text, True, sub_color)
                sub_rect = sub_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 10))
                self.screen.blit(sub_surface, sub_rect)
                
                features_text = "Enhanced Graphics | Real-time Effects | FSM States | GN Socket Communication"
                features_color = safe_get_color('TEXT_GOLD', 'waiting_features')
                features_surface = self.mini_font.render(features_text, True, features_color)
                features_rect = features_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 40))
                self.screen.blit(features_surface, features_rect)

            # Update display
            pygame.display.flip()
            self.clock.tick(FPS)

        # Cleanup
        print(f"\nGN {self.gn_id.upper()} Game Visualizer shutting down...")
        print(f"Final Statistics: FPS: {self.current_fps:.1f}, Messages: {self.message_count}")
        
        self.socket_manager.running = False
        self.socket_manager.close()
        
        if self.receive_thread and self.receive_thread.is_alive():
            self.receive_thread.join(timeout=1.0)
        
        pygame.quit()
        sys.exit()


def read_node_id():
    """Read the GN ID from node_id.txt file"""
    try:
        # Get the directory where this script is located
        script_dir = os.path.dirname(os.path.abspath(__file__))
        node_id_file = os.path.join(script_dir, "node_id.txt")
        
        print(f"Looking for node ID file: {node_id_file}")
        
        with open(node_id_file, 'r') as f:
            gn_id = f.read().strip()
            
        print(f"Read GN ID from file: '{gn_id}'")
        
        # Validate format
        if not gn_id.startswith('gn') or len(gn_id) != 3:
            raise ValueError(f"Invalid GN ID format: '{gn_id}'")
            
        return gn_id
        
    except FileNotFoundError:
        print("ERROR: node_id.txt file not found!")
        print("Make sure the Erlang GN graphics server creates this file first.")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR reading node ID file: {e}")
        sys.exit(1)

# Main execution
if __name__ == "__main__":
    try:
        print("GN Python Visualizer Starting...")
        
        # Get the GN ID first
        gn_id = read_node_id()
        
        print(f"GN ID: {gn_id.upper()}")
        print(f"Local Player: Player {int(gn_id[-1])}")
        
        # Create visualizer once with correct arguments
        visualizer = GNGameVisualizer(gn_id)
        visualizer.run_enhanced_game_loop()
        
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
        pygame.quit()
        sys.exit(0)
    except Exception as e:
        print(f"\nFatal error: {e}")
        import traceback
        traceback.print_exc()
        pygame.quit()
        sys.exit(1)
