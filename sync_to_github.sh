#!/bin/bash

# Get current timestamp
TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")

# Add all changes
git add .

# Check if there are changes to commit
if ! git diff-index --quiet HEAD --; then
    # Commit with timestamp
    git commit -m "Auto-sync changes at $TIMESTAMP"
    
    # Push to GitHub
    git push origin main
    
    echo "Changes pushed to GitHub at $TIMESTAMP"
else
    echo "No changes to commit at $TIMESTAMP"
fi 