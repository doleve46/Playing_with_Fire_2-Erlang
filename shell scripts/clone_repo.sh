#!/bin/bash

# Target directory (must already exist)
TARGET_DIR="/home/csestudent/Desktop/dolev_roi/playing_with_fire/src"

# Repo URL
REPO_URL="https://github.com/roialus/Playing_with_Fire_2-Earlang.git"

# Enter the directory
cd "$TARGET_DIR" || { echo "Directory $TARGET_DIR not found."; exit 1; }

# Clone the repo into this directory
git clone "$REPO_URL" .

# Print result
if [ $? -eq 0 ]; then
    echo "Repository successfully cloned into $TARGET_DIR"
else
    echo "Failed to clone repository."
fi
