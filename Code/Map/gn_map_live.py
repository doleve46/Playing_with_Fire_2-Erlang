import pygame
import sys
import math
import random
import struct
import select
import os

# Initialize Pygame
pygame.init()

TILE_SIZE = 40  # Size of each tile in pixels
MAP_SIZE = 16  # Size of the map in tiles (16x16)
PLAYER_PANEL_WIDTH = 220  # Left panel for player stats
POWERUP_PANEL_HEIGHT = 140  # Bottom panel for power-ups
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + 20  # Player panel + map + margin
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 20  # Map + power-ups + margin
MIN_WINDOW_WIDTH = 800  # Minimum width for the window
MIN_WINDOW_HEIGHT = 600  # Minimum height for the window
FPS = 60  # Frames per second for the game loop

# Layout offsets
MAP_OFFSET_X = PLAYER_PANEL_WIDTH + 10  # Map starts after player panel
MAP_OFFSET_Y = 10  # Small top margin
POWERUP_OFFSET_Y = MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE + 10  # Power-ups below map

COLORS = {
    # Floor with gradient effect
    'FLOOR_LIGHT': (245, 235, 205),
    'FLOOR_MID': (230, 220, 190),
    'FLOOR_DARK': (215, 205, 175),
    'FLOOR_SHADOW': (200, 190, 160),

    # Enhanced backgrounds
    'BACKGROUND': (45, 55, 65),
    'UI_BACKGROUND': (35, 45, 55),
    'PANEL_BG': (25, 35, 45),

    # Text with glow
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GOLD': (255, 215, 0),
    'TEXT_SHADOW': (0, 0, 0),
    'TEXT_CYAN': (100, 255, 255),
    'TEXT_ORANGE': (255, 165, 0),
    'TEXT_GREY': (120, 120, 120),  # New color for dead players
    'TEXT_RED': (200, 50, 50),     # New color for death indicators

    # Enhanced brick walls
    'BRICK_TOP': (180, 90, 45),
    'BRICK_MID': (160, 80, 40),
    'BRICK_DARK': (140, 70, 35),
    'BRICK_SHADOW': (120, 60, 30),
    'MORTAR': (100, 50, 25),

    # Beautiful wooden barrels
    'WOOD_LIGHT': (200, 140, 90),
    'WOOD_MID': (180, 120, 70),
    'WOOD_DARK': (160, 100, 50),
    'WOOD_SHADOW': (140, 80, 30),
    'WOOD_HIGHLIGHT': (220, 160, 110),
    'WOOD_BAND': (100, 60, 30),

    # Shiny metal barrels
    'METAL_LIGHT': (160, 165, 170),
    'METAL_MID': (130, 135, 140),
    'METAL_DARK': (100, 105, 110),
    'METAL_SHADOW': (70, 75, 80),
    'METAL_SHINE': (200, 205, 210),
    'METAL_BAND': (60, 65, 70),

    # Vibrant player colors
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

    # Glowing power-ups
    'POWERUP_GLOW': (255, 255, 150),
    'POWERUP_CORE': (255, 215, 0),
    'POWERUP_PULSE': (255, 255, 100),

    # Special effects
    'SHADOW': (0, 0, 0, 60),
    'HIGHLIGHT': (255, 255, 255, 100),
    'SELECTION': (255, 255, 0, 150),
    'GRID_LINE': (0, 0, 0, 40),

    # Bomb and explosion colors
    'BOMB_BLACK': (40, 40, 40),
    'BOMB_FUSE': (255, 100, 0),
    'EXPLOSION_CORE': (255, 255, 200),
    'EXPLOSION_MIDDLE': (255, 150, 50),
    'EXPLOSION_OUTER': (255, 50, 50),

    # Death screen colors
    'DEATH_RED': (150, 0, 0),
    'DEATH_DARK_RED': (100, 0, 0),
    'BLOOD_RED': (180, 20, 20),
    'DEATH_TEXT': (255, 50, 50),
}


class EnhancedGNGameVisualizer:
    def __init__(self):
        # Make window resizable and start with smaller size
        initial_width = min(WINDOW_WIDTH, 900)
        initial_height = min(WINDOW_HEIGHT, 700)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Enhanced GN Game Visualizer - Death Detection")
        self.clock = pygame.time.Clock()

        # Current window dimensions
        self.current_width = initial_width
        self.current_height = initial_height
        self.scale_factor = min(initial_width / WINDOW_WIDTH, initial_height / WINDOW_HEIGHT)

        # Fonts
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        self.powerup_font = pygame.font.Font(None, 20)
        
        # Special fonts for death screen
        self.death_font = pygame.font.Font(None, 72)  # Large font for "YOU DIED"
        self.death_subtitle_font = pygame.font.Font(None, 36)

        # Animation variables
        self.time = 0
        self.powerup_pulse = 0
        self.camera_shake = 0
        self.selected_tile = None

        # GN identification - we need to determine which GN this is
        self.local_gn = self.determine_local_gn()  # gn1, gn2, gn3, or gn4
        self.local_gn_player_ids = self.get_local_player_ids()  # Which players belong to this GN

        # Death tracking
        self.dead_players = {}  # PlayerID -> {death_time, last_known_state, local_gn}
        self.death_animations = {}  # PlayerID -> animation data
        self.you_died_display = None  # Track if we should show "YOU DIED"
        self.death_screen_start_time = None

        # Mapping dictionaries
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

        # Complete game state tracking for animations
        self.previous_game_state = None
        self.current_game_state = self.get_fallback_game_state()

        # Enhanced Animation systems (same as CN system)
        self.player_animations = {}  # Player movement animations
        self.bomb_animations = {}  # Bomb countdown and placement animations
        self.explosion_animations = []  # Active explosion effects
        self.powerup_animations = []  # Power-up pickup/spawn effects
        self.game_effects = []  # General effect animations (damage, status, etc.)
        self.status_effects = {}  # Player status effects (stun, invincibility, etc.)

        # Load initial game state
        self.player_stats = self.load_player_stats()

        # Create surfaces for smooth rendering
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
        self.player_panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.powerup_panel_surface = pygame.Surface((WINDOW_WIDTH, POWERUP_PANEL_HEIGHT))

        # Virtual surface for full layout
        self.virtual_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))

        print(f"🎮 Enhanced GN Game Visualizer initialized (Local GN: {self.local_gn})")
        print(f"👥 Local players: {self.local_gn_player_ids}")
        print("⏳ Waiting for enhanced map data from GN graphics server...")

    def determine_local_gn(self):
        """Determine which GN this visualizer is running on"""
        # In a real deployment, this would check the hostname, environment variables,
        # or command line arguments to determine which GN node this is
        # For now, we'll use a simple method or default
        gn_id = os.environ.get('GN_ID', 'gn1')  # Default to gn1 if not specified
        print(f"🏠 Detected local GN: {gn_id}")
        return gn_id

    def get_local_player_ids(self):
        """Get the player IDs that belong to this GN"""
        # Map GN nodes to player IDs (this would be configured based on your system)
        gn_to_players = {
            'gn1': [1],  # Player 1 is on GN1
            'gn2': [2],  # Player 2 is on GN2
            'gn3': [3],  # Player 3 is on GN3
            'gn4': [4]   # Player 4 is on GN4
        }
        return gn_to_players.get(self.local_gn, [])

    def read_port_data(self):
        """Read data from Enhanced GN graphics server via stdin"""
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

            # Process complete packets (4-byte length prefix + data)
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

            return packets

        except Exception as e:
            # This is normal when no data is available
            return None

    def decode_erlang_data(self, binary_data):
        """Decode Erlang binary term (enhanced)"""
        try:
            # For development, assume it's a string representation of Erlang terms
            text = binary_data.decode('utf-8', errors='ignore')
            if text.startswith('[') and (text.endswith(']') or text.endswith('].')):
                # Remove trailing period if present
                if text.endswith('].'):
                    text = text[:-1]
                # Use eval for development (not secure, but works for testing)
                return eval(text)
            return None
        except Exception as e:
            print(f"❌ Error decoding Erlang data: {e}")
            return None

    def handle_port_data(self, packets):
        """Handle received packets from Enhanced GN graphics server"""
        for packet in packets:
            try:
                # Try to decode as binary term first (like CN system)
                decoded_data = eval(packet.decode('utf-8'))
            except:
                decoded_data = self.decode_erlang_data(packet)
            
            if decoded_data:
                # Check if this is a movement confirmation
                if (isinstance(decoded_data, list) and len(decoded_data) == 2 and
                        decoded_data[0] == 'movement_confirmation'):

                    print("🏃 Received movement confirmation from GN")
                    self.handle_movement_confirmation(decoded_data[1])

                elif self.waiting_for_initial_map:
                    # First data should be initial map
                    print("🗺️ Received initial enhanced map from GN graphics server")
                    success = self.process_initial_map(decoded_data)
                    if success:
                        self.waiting_for_initial_map = False
                        self.map_initialized = True
                        print("✅ Initial enhanced map loaded! Now listening for GN updates...")
                else:
                    # Subsequent data updates
                    print("🔄 Received enhanced update from GN graphics server")
                    self.process_map_update(decoded_data)

    def process_initial_map(self, map_data):
        """Process initial map from Enhanced GN graphics server"""
        try:
            # Handle both old format (direct grid) and new format (enhanced structure)
            if isinstance(map_data, dict) and 'map' in map_data:
                # New enhanced format with death information
                grid_data = map_data['map']
                self.dead_players = map_data.get('dead_players', {})
                print(f"📊 Enhanced map data received with {len(self.dead_players)} dead players")
                self.check_for_local_player_death()
            else:
                # Old format - just the grid
                grid_data = map_data
                self.dead_players = {}
            
            # Parse the map data into game state
            game_state = self.parse_complete_game_state(grid_data)
            if game_state:
                self.current_game_state = game_state
                return True
            return False
        except Exception as e:
            print(f"❌ Error processing initial map: {e}")
            return False

    def process_map_update(self, update_data):
        """Process real-time update from Enhanced GN graphics server"""
        try:
            # Store previous state for animation detection
            self.previous_game_state = self.current_game_state.copy() if self.current_game_state else None

            # Handle enhanced data structure
            if isinstance(update_data, dict) and 'map' in update_data:
                # New enhanced format with death information
                grid_data = update_data['map']
                new_dead_players = update_data.get('dead_players', {})
                
                # Check for newly dead players
                for player_id, death_info in new_dead_players.items():
                    if player_id not in self.dead_players:
                        print(f"💀 New death detected: Player {player_id}")
                        self.create_death_animation(player_id, death_info)
                
                self.dead_players = new_dead_players
                self.check_for_local_player_death()
            else:
                # Old format - just the grid
                grid_data = update_data

            # Parse new state
            new_game_state = self.parse_complete_game_state(grid_data)
            if new_game_state:
                self.current_game_state = new_game_state

                # Detect changes for animations
                if self.previous_game_state:
                    self.detect_complete_game_changes(self.previous_game_state, new_game_state)

                return True
            return False
        except Exception as e:
            print(f"❌ Error processing map update: {e}")
            return False

    def check_for_local_player_death(self):
        """Check if any local players have died and trigger YOU DIED screen"""
        for player_id in self.local_gn_player_ids:
            if player_id in self.dead_players and self.you_died_display is None:
                # Local player died! Show death screen
                death_time, last_known_state, local_gn = self.dead_players[player_id]
                
                print(f"💀 LOCAL PLAYER {player_id} DIED! Triggering death screen...")
                
                self.you_died_display = {
                    'player_id': player_id,
                    'death_time': death_time,
                    'start_time': self.time,
                    'last_known_state': last_known_state,
                    'confirmed_local_gn': local_gn
                }
                self.death_screen_start_time = self.time
                
                # Massive camera shake for local death
                self.camera_shake = 2.0
                
                print(f"🩸 YOU DIED screen activated for Player {player_id}")
                break

    def create_death_animation(self, player_id, death_info):
        """Create death animation for a player"""
        death_time, last_known_state, local_gn = death_info
        
        # Create death effect
        self.death_animations[player_id] = {
            'start_time': self.time,
            'duration': 3.0,  # 3 second death animation
            'death_time': death_time,
            'last_known_state': last_known_state,
            'local_gn': local_gn,
            'active': True
        }
        
        # Add visual effects
        if last_known_state:
            # Extract position from last known state
            pos = getattr(last_known_state, 'position', [0, 0])
            x, y = pos if isinstance(pos, list) and len(pos) >= 2 else [0, 0]
            self.game_effects.append({
                'type': 'player_death',
                'player_id': player_id,
                'x': x, 'y': y,
                'start_time': self.time,
                'duration': 2.0,
                'active': True
            })

    def draw_you_died_screen(self):
        """Draw the dramatic YOU DIED screen overlay"""
        if not self.you_died_display:
            return
            
        elapsed = self.time - self.death_screen_start_time
        
        # Death screen lasts for 8 seconds
        if elapsed > 8.0:
            self.you_died_display = None
            self.death_screen_start_time = None
            return
        
        # Create dark overlay with blood effect
        overlay = pygame.Surface((self.current_width, self.current_height), pygame.SRCALPHA)
        
        # Fade in dark red overlay
        fade_progress = min(elapsed / 1.0, 1.0)  # Fade in over 1 second
        overlay_alpha = int(180 * fade_progress)
        
        # Gradient from dark red to black
        for y in range(self.current_height):
            ratio = y / self.current_height
            red_intensity = int((1 - ratio * 0.7) * 150 * fade_progress)
            color = (red_intensity, 0, 0, overlay_alpha)
            pygame.draw.line(overlay, color, (0, y), (self.current_width, y))
        
        self.screen.blit(overlay, (0, 0))
        
        # "YOU DIED" text with dramatic effects
        if elapsed > 0.5:  # Text appears after 0.5 seconds
            text_elapsed = elapsed - 0.5
            
            # Text fade in and scale effect
            text_alpha = min(text_elapsed / 0.5, 1.0)
            text_scale = 0.5 + 0.5 * min(text_elapsed / 0.3, 1.0)
            
            # Pulsing effect
            pulse = 1.0 + 0.1 * math.sin(text_elapsed * 3)
            final_scale = text_scale * pulse
            
            # Create "YOU DIED" text
            death_text = "YOU DIED"
            
            # Create multiple text surfaces for glow effect
            base_size = int(72 * final_scale)
            
            # Glow layers (multiple red outlines)
            for i in range(5, 0, -1):
                glow_font = pygame.font.Font(None, base_size + i * 6)
                glow_surface = glow_font.render(death_text, True, 
                    (int(255 * text_alpha * 0.6), 0, 0))
                glow_rect = glow_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 50))
                self.screen.blit(glow_surface, glow_rect)
            
            # Main text (bright red)
            main_font = pygame.font.Font(None, base_size)
            main_surface = main_font.render(death_text, True, 
                (int(255 * text_alpha), int(50 * text_alpha), int(50 * text_alpha)))
            main_rect = main_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 - 50))
            self.screen.blit(main_surface, main_rect)
            
            # Highlight on text
            highlight_surface = main_font.render(death_text, True, 
                (int(255 * text_alpha), int(200 * text_alpha), int(200 * text_alpha)))
            highlight_rect = highlight_surface.get_rect(center=(self.current_width // 2 - 2, self.current_height // 2 - 52))
            self.screen.blit(highlight_surface, highlight_rect)
        
        # Subtitle text
        if elapsed > 1.5:  # Subtitle appears after 1.5 seconds
            subtitle_elapsed = elapsed - 1.5
            subtitle_alpha = min(subtitle_elapsed / 0.5, 1.0)
            
            player_id = self.you_died_display['player_id']
            subtitle_text = f"Player {player_id} has fallen"
            
            subtitle_surface = self.death_subtitle_font.render(subtitle_text, True, 
                (int(200 * subtitle_alpha), int(100 * subtitle_alpha), int(100 * subtitle_alpha)))
            subtitle_rect = subtitle_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 20))
            self.screen.blit(subtitle_surface, subtitle_rect)
        
        # Blood drip effects
        if elapsed > 2.0:
            drip_elapsed = elapsed - 2.0
            num_drips = min(int(drip_elapsed * 5), 20)
            
            for i in range(num_drips):
                drip_x = (self.current_width // 4) + (i * self.current_width // 20)
                drip_length = min(drip_elapsed * 100, self.current_height // 2)
                drip_alpha = max(0, 150 - i * 7)
                
                if drip_alpha > 0:
                    pygame.draw.line(self.screen, (180, 20, 20),
                        (drip_x, 0), (drip_x, drip_length), 3)
        
        # Instructions to continue
        if elapsed > 5.0:  # Instructions appear after 5 seconds
            instruction_elapsed = elapsed - 5.0
            instruction_alpha = min(instruction_elapsed / 0.5, 1.0)
            
            instruction_text = "Press SPACE to continue watching..."
            instruction_surface = self.small_font.render(instruction_text, True, 
                (int(150 * instruction_alpha), int(150 * instruction_alpha), int(150 * instruction_alpha)))
            instruction_rect = instruction_surface.get_rect(center=(self.current_width // 2, self.current_height - 80))
            self.screen.blit(instruction_surface, instruction_rect)

    def handle_movement_confirmation(self, confirmation_data):
        """Handle immediate movement confirmation for players and bombs (same as CN)"""
        entity_type = confirmation_data['entity_type']
        entity_data = confirmation_data['entity_data']

        if entity_type == 'player':
            self.handle_player_movement_confirmation(entity_data)
        elif entity_type == 'bomb':
            self.handle_bomb_movement_confirmation(entity_data)

    def handle_player_movement_confirmation(self, player_data):
        """Handle immediate player movement confirmation (same as CN)"""
        player_id = player_data['player_id']
        from_pos = player_data['from_pos']
        to_pos = player_data['to_pos']
        direction = player_data['direction']
        speed = player_data['speed']

        # Calculate speed-aware animation duration
        base_duration = 0.4  # Base duration
        speed_multiplier = max(speed, 1)
        actual_duration = base_duration / speed_multiplier

        # Start immediate animation
        self.player_animations[player_id] = {
            'type': 'confirmed_walking',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': actual_duration,
            'speed': speed,
            'confirmed': True,
            'active': True
        }

        # Add speed effects if player is fast
        if speed > 1:
            self.create_speed_boost_effect(player_id, from_pos[0], from_pos[1], speed, direction)

        print(
            f"🏃 GN Player {player_id} movement confirmed: {from_pos} -> {to_pos} (speed: {speed}x, duration: {actual_duration:.2f}s)")

    def handle_bomb_movement_confirmation(self, bomb_data):
        """Handle immediate bomb movement confirmation (kicked bomb) (same as CN)"""
        bomb_id = tuple(bomb_data['bomb_id'])  # Convert to tuple for dict key
        from_pos = bomb_data['from_pos']
        to_pos = bomb_data['to_pos']
        direction = bomb_data['direction']
        bomb_type = bomb_data['type']
        owner = bomb_data['owner']

        # Bombs move at a fixed speed
        bomb_movement_duration = 0.3

        # Start immediate bomb movement animation
        self.bomb_animations[bomb_id] = {
            'type': 'moving',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': bomb_movement_duration,
            'bomb_type': bomb_type,
            'owner': owner,
            'confirmed': True,
            'active': True
        }

        # Add kick effect at the starting position
        self.create_bomb_kick_effect(from_pos[0], from_pos[1], direction, owner)

        print(f"💣 GN Bomb movement confirmed: {from_pos} -> {to_pos} (kicked by {owner})")

    def create_speed_boost_effect(self, player_id, x, y, speed, direction):
        """Create visual effect for speed boost (same as CN)"""
        self.game_effects.append({
            'type': 'speed_boost',
            'player_id': player_id,
            'x': x, 'y': y,
            'speed': speed,
            'direction': direction,
            'start_time': self.time,
            'duration': 0.8,
            'active': True
        })

    def create_bomb_kick_effect(self, x, y, direction, kicker):
        """Create visual effect when bomb is kicked (same as CN)"""
        self.game_effects.append({
            'type': 'bomb_kick',
            'x': x, 'y': y,
            'direction': direction,
            'kicker': kicker,
            'start_time': self.time,
            'duration': 0.5,
            'active': True
        })

    def parse_complete_game_state(self, erlang_grid):
        """Parse complete game state including bombs, players, explosions (same as CN)"""
        game_state = {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],  # List of active bombs with timers
            'players': [],  # Current player positions and status
            'explosions': [],  # Active explosion areas
            'game_info': {'time': 0, 'round': 1, 'status': 'playing'}
        }

        if not erlang_grid or not isinstance(erlang_grid, list):
            print("⚠️ Invalid grid data, using fallback")
            return self.get_fallback_game_state()

        for row_idx in range(len(erlang_grid)):
            if row_idx >= MAP_SIZE:
                break
            for col_idx in range(len(erlang_grid[row_idx])):
                if col_idx >= MAP_SIZE:
                    break

                cell = erlang_grid[row_idx][col_idx]

                # Handle both 4-tuple and 6-tuple formats
                if len(cell) >= 4:
                    tile_type, powerup_type, bomb_info, player_info = cell[:4]
                    explosion_info = cell[4] if len(cell) > 4 else 'none'
                    special_info = cell[5] if len(cell) > 5 else 'none'

                    # Transpose coordinates for display
                    x, y = col_idx, row_idx

                    # Parse tile
                    game_state['tiles'][x][y] = self.tile_mapping.get(tile_type, 0)
                    game_state['powerups'][x][y] = self.powerup_mapping.get(powerup_type, 'none')

                    # Parse bomb information
                    if bomb_info != 'none':
                        bomb_data = self.parse_bomb_info(bomb_info, x, y)
                        if bomb_data:
                            game_state['bombs'].append(bomb_data)

                    # Parse player information - include speed
                    if player_info != 'none':
                        player_data = self.parse_player_info(player_info, x, y)
                        if player_data:
                            game_state['players'].append(player_data)

                    # Parse explosion information
                    if explosion_info != 'none':
                        explosion_data = self.parse_explosion_info(explosion_info, x, y)
                        if explosion_data:
                            game_state['explosions'].append(explosion_data)

        print(
            f"✅ GN Game state loaded - Players: {len(game_state['players'])}, Bombs: {len(game_state['bombs'])}, Explosions: {len(game_state['explosions'])}, Dead: {len(self.dead_players)}")
        return game_state

    def parse_bomb_info(self, bomb_info, x, y):
        """Parse bomb information from Erlang data (same as CN)"""
        try:
            # Handle simple bomb atom like 'bomb' or complex tuple
            if isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return {
                    'x': x, 'y': y,
                    'type': 'normal_bomb',
                    'timer': 3,  # Default timer
                    'owner': 'unknown',
                    'power': 2,  # Default power
                    'animation_start': self.time
                }
            elif isinstance(bomb_info, tuple) and len(bomb_info) >= 2:
                # Complex bomb info: (bomb_type, timer, owner, power)
                bomb_type = bomb_info[0] if len(bomb_info) > 0 else 'normal_bomb'
                timer = int(bomb_info[1]) if len(bomb_info) > 1 and str(bomb_info[1]).isdigit() else 3
                owner = bomb_info[2] if len(bomb_info) > 2 else 'unknown'
                power = int(bomb_info[3]) if len(bomb_info) > 3 and str(bomb_info[3]).isdigit() else 2

                return {
                    'x': x, 'y': y,
                    'type': str(bomb_type),
                    'timer': timer,
                    'owner': str(owner),
                    'power': power,
                    'animation_start': self.time
                }
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_player_info(self, player_info, x, y):
        """Parse current player position and status with speed (same as CN)"""
        try:
            # Handle simple player atom like 'player_1'
            if isinstance(player_info, str) and 'player_' in player_info:
                player_num = int(player_info.split('_')[1])
                return {
                    'player': player_num,
                    'x': x, 'y': y,
                    'health': 3,  # Default health
                    'speed': 1,  # Default speed
                    'status': 'alive',
                    'direction': 'north',
                    'last_update': self.time
                }
            elif isinstance(player_info, tuple) and len(player_info) >= 2:
                # Complex player info: (player_id, health, speed)
                player_id = player_info[0]
                health = int(player_info[1]) if len(player_info) > 1 and str(player_info[1]).isdigit() else 3
                speed = int(player_info[2]) if len(player_info) > 2 and str(player_info[2]).isdigit() else 1
                status = player_info[3] if len(player_info) > 3 else 'alive'
                direction = player_info[4] if len(player_info) > 4 else 'north'

                if 'player_' in str(player_id):
                    player_num = int(str(player_id).split('_')[1])
                    return {
                        'player': player_num,
                        'x': x, 'y': y,
                        'health': health,
                        'speed': speed,  # Includes actual speed
                        'status': str(status),
                        'direction': str(direction),
                        'last_update': self.time
                    }
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_explosion_info(self, explosion_info, x, y):
        """Parse explosion state information (same as CN)"""
        try:
            # Handle simple explosion atom
            if isinstance(explosion_info, str) and 'explosion' in explosion_info.lower():
                return {
                    'x': x, 'y': y,
                    'type': 'blast_center',
                    'intensity': 1.0,
                    'remaining_time': 0.5,
                    'start_time': self.time
                }
            elif isinstance(explosion_info, tuple) and len(explosion_info) >= 2:
                # Complex explosion info: (explosion_type, intensity, remaining_time)
                exp_type = explosion_info[0] if len(explosion_info) > 0 else 'blast_center'
                intensity = float(explosion_info[1]) if len(explosion_info) > 1 and str(explosion_info[1]).replace('.',
                                                                                                                   '').isdigit() else 1.0
                remaining = float(explosion_info[2]) if len(explosion_info) > 2 and str(explosion_info[2]).replace('.',
                                                                                                                   '').isdigit() else 0.5

                return {
                    'x': x, 'y': y,
                    'type': str(exp_type),
                    'intensity': intensity,
                    'remaining_time': remaining,
                    'start_time': self.time
                }
        except (ValueError, TypeError):
            pass
        return None

    def detect_complete_game_changes(self, old_state, new_state):
        """Comprehensive change detection for all game elements (same as CN)"""

        # Detect player movements with detailed tracking
        self.detect_detailed_player_changes(old_state.get('players', []),
                                            new_state.get('players', []))

        # Detect bomb lifecycle (placement, ticking, explosion)
        self.detect_bomb_lifecycle(old_state.get('bombs', []),
                                   new_state.get('bombs', []))

        # Detect explosion evolution
        self.detect_explosion_changes(old_state.get('explosions', []),
                                      new_state.get('explosions', []))

        # Detect tile changes (walls destroyed/created)
        self.detect_tile_changes(old_state['tiles'], new_state['tiles'])

        # Detect power-up changes
        self.detect_powerup_changes(old_state['powerups'], new_state['powerups'])

    def detect_detailed_player_changes(self, old_players, new_players):
        """Detect player movements, direction changes, status changes (same as CN)"""
        old_player_dict = {p['player']: p for p in old_players}
        new_player_dict = {p['player']: p for p in new_players}

        for player_id, new_player in new_player_dict.items():
            if player_id in old_player_dict:
                old_player = old_player_dict[player_id]

                # Position change - only create animation if not already confirmed
                if ((old_player['x'], old_player['y']) != (new_player['x'], new_player['y']) and
                        player_id not in self.player_animations):
                    self.create_detailed_walking_animation(
                        player_id,
                        (old_player['x'], old_player['y']),
                        (new_player['x'], new_player['y']),
                        new_player.get('direction', 'north'),
                        new_player.get('speed', 1)  # Include speed
                    )
                    print(
                        f"🚶 GN Player {player_id} moved from ({old_player['x']}, {old_player['y']}) to ({new_player['x']}, {new_player['y']})")

                # Health change
                if old_player['health'] != new_player['health']:
                    if new_player['health'] < old_player['health']:
                        self.create_damage_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💔 GN Player {player_id} took damage: {old_player['health']} -> {new_player['health']}")
                    else:
                        self.create_healing_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💚 GN Player {player_id} healed: {old_player['health']} -> {new_player['health']}")

                # Status change (death, stun, etc.)
                if old_player['status'] != new_player['status']:
                    self.create_status_change_effect(player_id, new_player['x'], new_player['y'],
                                                     old_player['status'], new_player['status'])
                    print(f"⚡ GN Player {player_id} status change: {old_player['status']} -> {new_player['status']}")
            else:
                # New player appeared (respawn)
                print(f"✨ GN Player {player_id} spawned at ({new_player['x']}, {new_player['y']})")
                self.create_player_spawn_effect(player_id, new_player['x'], new_player['y'])

    # Include the rest of the animation and drawing methods from the CN system
    # (These would be the same as in the complete map_live_port.py)
    
    def update_all_animations(self):
        """Update all active animations (same as CN)"""
        current_time = self.time

        # Update player walking animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    # Animation finished
                    del self.player_animations[player_id]

        # Update bomb animations
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
            if anim.get('type') == 'moving':
                # Moving bomb animation
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    del self.bomb_animations[pos]
            else:
                # Regular bomb countdown - keep active until explicitly removed
                elapsed = current_time - anim['start_time']
                if elapsed > 10:  # Safety timeout
                    del self.bomb_animations[pos]

        # Update explosion animations
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update power-up animations
        self.powerup_animations = [
            anim for anim in self.powerup_animations
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
            if current_time - effect['start_time'] > effect['duration']:
                del self.status_effects[player_id]

        # Update camera shake
        if self.camera_shake > 0:
            self.camera_shake -= 1 / FPS

        # Update death animations
        current_time = self.time
        expired_deaths = []
        for player_id, anim in self.death_animations.items():
            if current_time - anim['start_time'] > anim['duration']:
                expired_deaths.append(player_id)
        
        for player_id in expired_deaths:
            del self.death_animations[player_id]

    def draw_player_stats_panel(self):
        """Draw player statistics panel on the left side with death detection (enhanced for GN)"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel title
        title_text = f"GN PLAYERS ({self.local_gn.upper()})"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.player_panel_surface.blit(title_surface, (10, 10))

        # Draw each player's stats 
        current_players = {p['player']: p for p in self.current_game_state['players']}
        
        for player_id in range(1, 5):
            y_pos = 60 + (player_id - 1) * 80
            player_data = current_players.get(player_id)
            
            # Check if player is dead
            is_dead = player_id in self.dead_players
            is_local_player = player_id in self.local_gn_player_ids
            
            # Choose colors and status based on death state
            if is_dead:
                if is_local_player:
                    status_text = f"Player {player_id}: YOU DIED"
                    status_color = COLORS['DEATH_TEXT']
                else:
                    status_text = f"Player {player_id}: DEAD"
                    status_color = COLORS['TEXT_RED']
                text_color = COLORS['TEXT_GREY']
            else:
                if player_data:
                    status_text = f"Player {player_id}: ACTIVE"
                    if is_local_player:
                        status_color = COLORS['TEXT_CYAN']  # Highlight local player
                    else:
                        status_color = COLORS['TEXT_WHITE']
                else:
                    status_text = f"Player {player_id}: WAITING"
                    status_color = COLORS['TEXT_ORANGE']
                text_color = COLORS['TEXT_WHITE']
            
            # Get health and speed (from current data or last known if dead)
            if is_dead and player_id in self.dead_players:
                death_time, last_known_state, local_gn = self.dead_players[player_id]
                if last_known_state:
                    health = getattr(last_known_state, 'life', 0)
                    speed = getattr(last_known_state, 'speed', 1)
                else:
                    health = 0
                    speed = 1
            else:
                health = player_data.get('health', 3) if player_data else 3
                speed = player_data.get('speed', 1) if player_data else 1
            
            health_text = f"Health: {health}"
            speed_text = f"Speed: {speed}"
            
            if speed > 1 and not is_dead:
                speed_color = COLORS['TEXT_CYAN']
            else:
                speed_color = text_color
            
            status_surface = self.small_font.render(status_text, True, status_color)
            health_surface = self.small_font.render(health_text, True, text_color)
            speed_surface = self.small_font.render(speed_text, True, speed_color) 
            
            self.player_panel_surface.blit(status_surface, (20, y_pos))
            self.player_panel_surface.blit(health_surface, (20, y_pos + 20))
            self.player_panel_surface.blit(speed_surface, (20, y_pos + 40))

    def draw_info_panel(self):
        """Draw info panel with death statistics (enhanced for GN)"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Title
        title_text = f"GN GAME INFO ({self.local_gn.upper()})"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Stats including death info
        bomb_count = len(self.current_game_state['bombs'])
        player_count = len(self.current_game_state['players'])
        dead_count = len(self.dead_players)

        stats_text = [
            f"Active Players: {player_count}",
            f"Active Bombs: {bomb_count}",
            f"Dead Players: {dead_count}",
            f"Status: {'ENHANCED LIVE UPDATES' if self.map_initialized else 'WAITING FOR DATA'}"
        ]

        for i, text in enumerate(stats_text):
            if i == 2 and dead_count > 0:  # Highlight dead players count
                color = COLORS['TEXT_RED']
            elif i == 3 and self.map_initialized:
                color = COLORS['TEXT_CYAN']
            else:
                color = COLORS['TEXT_WHITE']
            text_surface = self.small_font.render(text, True, color)
            self.powerup_panel_surface.blit(text_surface, (20, 60 + i * 25))

    def load_player_stats(self):
        """Load player statistics (same as CN)"""
        return {
            1: {
                'life': 3, 'speed': 1, 'bombs': 3, 'explosion_radius': 2,
                'special_abilities': [], 'color': COLORS['PLAYER_1']
            },
            2: {
                'life': 2, 'speed': 2, 'bombs': 4, 'explosion_radius': 1,
                'special_abilities': ['kick_bomb'], 'color': COLORS['PLAYER_2']
            },
            3: {
                'life': 4, 'speed': 1, 'bombs': 2, 'explosion_radius': 3,
                'special_abilities': [], 'color': COLORS['PLAYER_3']
            },
            4: {
                'life': 1, 'speed': 3, 'bombs': 5, 'explosion_radius': 1,
                'special_abilities': ['plus_bombs', 'phased', 'freeze_bomb'], 'color': COLORS['PLAYER_4']
            }
        }

    def get_fallback_game_state(self):
        """Fallback game state if no data received (same as CN)"""
        return {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],
            'players': [],
            'explosions': [],
            'game_info': {'time': 0, 'round': 1, 'status': 'waiting'}
        }

    # Include stub methods for the animation functions that would be copied from CN system
    def detect_bomb_lifecycle(self, old_bombs, new_bombs): pass
    def detect_explosion_changes(self, old_explosions, new_explosions): pass
    def detect_tile_changes(self, old_tiles, new_tiles): pass
    def detect_powerup_changes(self, old_powerups, new_powerups): pass
    def create_detailed_walking_animation(self, *args): pass
    def create_damage_effect(self, *args): pass
    def create_healing_effect(self, *args): pass
    def create_status_change_effect(self, *args): pass
    def create_player_spawn_effect(self, *args): pass

    def handle_events(self):
        """Handle pygame events (enhanced with death screen interaction)"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
                elif event.key == pygame.K_SPACE:
                    # Space key dismisses death screen
                    if self.you_died_display:
                        print("⏩ Death screen dismissed by user")
                        self.you_died_display = None
                        self.death_screen_start_time = None
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
        return True

    def handle_window_resize(self, new_width, new_height):
        """Handle window resizing (same as CN)"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)

        # Update scale factor to maintain aspect ratio
        self.scale_factor = min(
            self.current_width / WINDOW_WIDTH,
            self.current_height / WINDOW_HEIGHT
        )

        # Recreate screen surface
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)

    def run(self):
        """Main game loop with Enhanced GN port communication and death detection"""
        print("🎮 Enhanced GN Game Visualizer Started with Death Detection!")
        print(f"🏠 Local GN: {self.local_gn}")
        print(f"👥 Monitoring local players: {self.local_gn_player_ids}")
        print("📡 Reading enhanced data from GN graphics server:")
        print("   ✨ Enhanced with movement confirmations")
        print("   🏃 Smooth speed-aware animations")
        print("   💣 Bomb movement tracking")
        print("   🎨 Advanced visual effects")
        print("   💀 Death detection and display")
        print("   🩸 YOU DIED screen for local players")
        print("🖱️ Click tiles to inspect | SPACE dismisses death screen | ESC to exit")

        running = True
        while running:
            running = self.handle_events()

            # Read data from Enhanced GN graphics server
            packets = self.read_port_data()
            if packets:
                self.handle_port_data(packets)

            # Update time and animations
            self.time += 1 / FPS
            self.update_all_animations()

            # Only draw if we have a map
            if self.map_initialized and self.current_game_state:
                # Clear virtual surface with gradient background
                self.virtual_surface.fill(COLORS['BACKGROUND'])

                # Draw complete enhanced game visualization
                self.draw_player_stats_panel()
                self.draw_info_panel()

                # Blit surfaces to virtual surface
                self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))
                self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

                # Scale virtual surface to actual screen
                if self.scale_factor != 1.0:
                    scaled_width = int(WINDOW_WIDTH * self.scale_factor)
                    scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
                    scaled_surface = pygame.transform.scale(self.virtual_surface, (scaled_width, scaled_height))
                else:
                    scaled_surface = self.virtual_surface

                # Clear screen and center the scaled content
                self.screen.fill(COLORS['BACKGROUND'])

                # Center the content on screen
                x_offset = (self.current_width - scaled_surface.get_width()) // 2
                y_offset = (self.current_height - scaled_surface.get_height()) // 2
                self.screen.blit(scaled_surface, (max(0, x_offset), max(0, y_offset)))

                # Display connection status
                status_text = f"🔄 Enhanced GN live updates active | Local: {self.local_gn.upper()}"
                color = COLORS['TEXT_CYAN']

                status_surface = self.small_font.render(status_text, True, color)
                self.screen.blit(status_surface, (10, 10))

                # Draw death screen overlay if active - THIS IS THE KEY FEATURE!
                self.draw_you_died_screen()

            else:
                # Show waiting screen
                self.screen.fill(COLORS['BACKGROUND'])

                waiting_text = "⏳ Waiting for enhanced map data from GN graphics server..."
                text_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                text_rect = text_surface.get_rect(center=(self.current_width // 2, self.current_height // 2))
                self.screen.blit(text_surface, text_rect)

                instruction_text = f"Enhanced GN server will send data automatically (Local: {self.local_gn})"
                inst_surface = self.small_font.render(instruction_text, True, COLORS['TEXT_CYAN'])
                inst_rect = inst_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 30))
                self.screen.blit(inst_surface, inst_rect)

            pygame.display.flip()
            self.clock.tick(FPS)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = EnhancedGNGameVisualizer()
    visualizer.run()