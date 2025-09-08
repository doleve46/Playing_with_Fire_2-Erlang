#!/bin/bash

# Target directory (your rebar3 project folder)
TARGET_DIR="/home/csestudent/Desktop/dolev_roi/playing_with_fire/src"

# Repo URL
REPO_URL="https://github.com/roialus/Playing_with_Fire_2-Earlang.git"

# Branch to clone
# BRANCH="linux_temp_master"   # <<< change this to the branch you want
BRANCH="changes_roi"   # <<< change this to the branch you want

# Temporary directory
TMP_DIR=$(mktemp -d)

# Clone specific branch into temp dir
git clone -b "$BRANCH" --single-branch "$REPO_URL" "$TMP_DIR" || { echo "Failed to clone repository."; exit 1; }

# Copy contents (excluding .git) into target dir
shopt -s dotglob
cp -r "$TMP_DIR"/* "$TARGET_DIR"/
shopt -u dotglob

# Cleanup
rm -rf "$TMP_DIR"

echo "Repository branch '$BRANCH' merged into $TARGET_DIR"
