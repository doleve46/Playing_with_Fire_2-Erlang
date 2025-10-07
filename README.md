# Playing with Fire 2 - Erlang

## 🎮 Game Overview

Playing with Fire 2 is a classic game where players navigate a grid-based arena, place bombs to destroy obstacles and opponents, and collect power-ups to enhance their abilities. The game is built as a distributed system with:

- **Central Node (CN)**: Coordinates the game and manages connections
- **Game Nodes (GN1-GN4)**: Handle individual player logic and a quarter of the game map
- **Python Graphics**: Provides the visual interface and user interaction

## 📋 Prerequisites

### Software Requirements
- **Erlang/OTP** (version 24 or higher)
- **Rebar3** (Erlang build tool)
- **Python 3.x**
- **Pygame** library
- **jsx** library (used for Python-Erlang communication)

### Erlang Dependencies

```bash
Within rebar.config:
...
{deps, [{jsx, "~> 3.0"}]}
...
```

### Python Dependencies
```bash
pip install pygame
```
**For Python versions older than 3.7:**
```bash
pip install pygame dataclasses
```

### Network Configuration
- All nodes must be on the same local network or have proper firewall configurations
- Default cookie: `12345` (configured in shell scripts)

## 🚀 Installation

1. **Create a rebar3 app:**
   ```bash
   rebar3 new app playing_with_fire
   ```
2. **Clone the repository:**
   ```bash
   git clone https://github.com/roialus/Playing_with_Fire_2-Earlang.git
   cd Playing_with_Fire_2-Earlang
   ```

3. **Update network configuration:**
   - Edit `shell scripts/node names.txt` with your IP addresses
   - Update the IP addresses in the shell scripts if needed


## 🎯 How to Run

### Starting the Game
**Note:** before using the shell scripts, update the project's path (where you cloned the repo) within the scripts

1.**Start Game Nodes (on each player machine):**
   ```bash
   # For Game Node 1
   chmod +x "shell scripts/start_gn1.sh"
   ./shell scripts/start_gn1.sh
   
   # Similarly for GN2, GN3, GN4 (use respective scripts)
   ```
  - Wait till the all the menus are open.

2. **Start the Central Node (CN):**
   ```bash
   chmod +x "shell scripts/start_cn.sh"
   ./shell scripts/start_cn.sh
   ```
 
4. **Connect and Play:**
   - Wait till CN will discover all GNs
   - Click "Play" to wait for player connections (in each GN)
   - Game nodes will automatically connect to the CN
   - Once all players are connected, choose to play as human or let AI play

### 🎮 How to Play

#### Game Controls:
- **w, up, arrow_up**:        Move player upward
- **s, down, arrow_down**:    Move player downward  
- **a, left, arrow_left**:    Move player left
- **d, right, arrow_right**:  Move player right
- **space, e**:               Place a bomb at current location
- **q**:                      Ignite remote bombs
