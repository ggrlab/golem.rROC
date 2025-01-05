#!/bin/bash

# Path to the R folder
WATCH_DIR="R/"

# Command to run the R script

R_SCRIPT="./dev/app.R"

# Function to start the R script
restart_r_script() {
    # ps aux | grep "$R_SCRIPT"
    # ps aux | grep "$R_SCRIPT" | awk '{print $2}' 
    # ps aux | grep "$R_SCRIPT" | awk '{print $2}' | xargs -r kill # Kill any existing instances
    ps aux | grep "$R_SCRIPT"
    pkill -f "$R_SCRIPT" 2>/dev/null  # Kill any existing instances
    Rscript $R_SCRIPT &  # Start the script in the background
    # echo "R script started with PID $!"
}

# Initial start of the R script
restart_r_script

# Monitor the folder for changes
inotifywait -m -e close_write "$WATCH_DIR" | while read -r path event file; do
    echo "Detected change in $file. Restarting R script..."
    restart_r_script
done