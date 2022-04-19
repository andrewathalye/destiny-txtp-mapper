#!/bin/sh
echo "$0 [matches_new]" 1>&2
head -n150 "$1" | sed -e 's/^ *//g' | cut -d' ' -f2 | xargs -I '{}' echo '+ {} UNIDENTIFIED'

