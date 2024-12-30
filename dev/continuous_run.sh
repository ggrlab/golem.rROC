#!/usr/bin/env bash

set -e # exit on errors
# kill all processes where grep finds "run_dev.R"
app_name="app.R"
killing_processes=$(ps aux | grep $app_name)
echo "Killing processes: $killing_processes"
kill $(echo $killing_processes | awk '{print $2}') || echo "No processes to kill"


# Batch changes every 1second
next_allowed_run=$(date +%s)
batch_window_seconds=1
current_shiny_pid="EMPTY"
inotifywait \
    --monitor ./R \
    --recursive \
    --event=create \
    --event=modify \
    --event=attrib \
    --event=delete \
    --event=move \
    --format='%T' \
    --timefmt='%s' |
    while read event_time; do
        echo ${file_path}${file_name} event: ${file_event}
        # If events arrive before the next allowed command-run, just skip them.
        if [[ $event_time -ge $next_allowed_run ]]; then
            echo "    DO THE EVENT!"
            if [[ $current_shiny_pid != "EMPTY" ]]; then
                echo "    KILLING SHINY SERVER: $current_shiny_pid"
                kill $current_shiny_pid || echo "Process was not running"
            fi
            Rscript -e "source('./dev/$app_name')" &
            current_shiny_pid=$!
            next_allowed_run=$(date --date="${batch_window_seconds}sec" +%s)
            sleep $batch_window_seconds # Wait for additional changes
        fi
    done
