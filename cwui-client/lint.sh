#/usr/bin/bash

defined=$(grep -oE '^[a-zA-Z]* :' "$1" | sed 's/://')

for d in $defined; do
    used=$(git grep -c "$d")
    n_files=$(echo "$used" | wc -l)
    if [ "$n_files" -eq 1 ]; then
        echo "$d"
        echo "$used"
    fi
done
