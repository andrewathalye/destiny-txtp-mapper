#!/bin/sh
echo "$0 [matches_new]" 1>&2
head -n150 "$1" | cut -d' ' -f2 | xargs -I '{}' echo '+ {} UNIDENTIFIED'

