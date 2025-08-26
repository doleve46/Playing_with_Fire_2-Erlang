#!/bin/bash

# Get the current IP address (using the default route interface)
IP_ADDRESS=$(hostname -I | awk '{print $1}')

# Alternative method if the above doesn't work:
# IP_ADDRESS=$(ip route get 8.8.8.8 | awk -F"src " 'NR==1{split($2,a," ");print a[1]}')

echo "Detected IP Address: $IP_ADDRESS"

# Navigate to the specified folder
PROJECT_PATH="$HOME/Desktop/dolev_roi/playing_with_fire"

if [ ! -d "$PROJECT_PATH" ]; then
    echo "Error: Directory $PROJECT_PATH does not exist!"
    exit 1
fi

echo "Navigating to: $PROJECT_PATH"
cd "$PROJECT_PATH" || {
    echo "Error: Failed to navigate to $PROJECT_PATH"
    exit 1
}

echo "Running rebar3 clean..."
rebar3 clean
if [ $? -ne 0 ]; then
    echo "Error: rebar3 clean failed!"
    exit 1
fi

echo "Running rebar3 compile..."
rebar3 compile
if [ $? -ne 0 ]; then
    echo "Error: rebar3 compile failed!"
    exit 1
fi

echo "Starting Erlang shell with name GN1@$IP_ADDRESS..."
echo "Will automatically run gn_start:start() after shell starts..."

# Create a temporary script file
cat > /tmp/erlang_commands.txt << 'EOF'
gn_start:start().
EOF

echo "Commands will be executed automatically. Press Ctrl+C to exit when done."
sleep 1

# Use timeout to give the shell time to start, then send the command
(sleep 1.5; cat /tmp/erlang_commands.txt; cat) | rebar3 shell --name "GN3@$IP_ADDRESS" --setcookie 12345

# Clean up
rm -f /tmp/erlang_commands.txt