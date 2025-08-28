#!/bin/bash

# Get the current IP address (using the default route interface)
IP_ADDRESS=$(hostname -I | awk '{print $1}')

echo "*Detected IP Address: $IP_ADDRESS"

PROJECT_PATH="$HOME/Desktop/dolev_roi/playing_with_fire"
if [ ! -d "$PROJECT_PATH" ]; then
    echo "Error: Directory $PROJECT_PATH does not exist!"
    exit 1
fi

echo "*Navigating to $PROJECT_PATH"
cd "$PROJECT_PATH" || {
	echo "Error: Failed to navigate to $PROJECT_PATH"
	exit 1
}

echo "*Running rebar3 clean.."
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

IP_PREFIX =$(echo "$IP_ADDRESS" | cut -d. -f1-3)

echo "Starting Erlang shell with name CN@$IP_ADDRESS..."
echo "Will automatically run cn_start:cn_bootstrap($IP_PREFIX) after shell starts..."
echo "Using IP prefix: $IP_PREFIX"

# Create a temporary script file
cat > /tmp/erlang_commands.txt << EOF
cn_start:cn_bootstrap("$IP_PREFIX.").
EOF

(sleep 1.0; cat /tmp/erlang_commands.txt; cat) | rebar3 shell --name "CN@$IP_ADDRESS" --setcookie 12345

# clean up
rm -f \tmp/erlang_commands.txt

# VERSION THAT WORKS FOR THE LAB
#rebar3 shell --name "CN@$IP_ADDRESS" --setcookie 12345 --eval 'cn_start:cn_bootstrap("132.72.81.").'