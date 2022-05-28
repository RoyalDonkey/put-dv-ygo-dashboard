#!/bin/sh

# This script creates the "decks.json" file out of *.jsonl files scattered
# across the decks directory. The "decks.json" file can then be read by R.

DECKSDIR=decks
OUTPUT="$DECKSDIR/decks.json"

# Open the output JSON list
printf '[\n' >"$OUTPUT"

# Filter and concatenate everything into the output file
first=1
find "$DECKSDIR" -type f -name '*.jsonl' | while read -r fname; do
    all=0
    admitted=0
    while read -r deck; do
        echo "$deck" | grep -q '"main":\s*\[\]' || {
            if [ -n "$first" ]; then
                printf "\t%s" "$deck" >>"$OUTPUT"
                unset first
            else
                printf ",\n\t%s" "$deck" >>"$OUTPUT"
            fi
            admitted=$((admitted + 1))
        }
        all=$((all + 1))
        printf '\r%s: %s/%s filtering...' "$fname" "$admitted" "$all"
    done <"$fname"
    printf '\r%s: %s/%s done.       \n' "$fname" "$admitted" "$all"
done

# Close the output JSON list
printf '\n]' >>"$OUTPUT"
