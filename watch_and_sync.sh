#!/bin/bash

# Basic file watcher using shell commands
# This script checks for changes every 30 seconds and syncs with GitHub if changes are detected

echo "Starting file watcher to sync with GitHub..."
echo "Press Ctrl+C to stop"

# Run in an infinite loop
while true; do
  # Run the sync script (which already checks for changes)
  ./sync_to_github.sh
  
  # Wait for 30 seconds
  echo "Waiting 30 seconds before checking again..."
  sleep 30
done 