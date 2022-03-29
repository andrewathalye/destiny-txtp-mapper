#!/bin/sh
echo "$0 [matches_newclean]" 1>&2
head -n150 "$1" | cut -d' ' -f2 | sort | uniq | xargs -I '{}' echo '+ {} UNIDENTIFIED'

