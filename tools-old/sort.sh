#!/bin/sh
echo "$0 [unsorted matches]: Sort matches for cleaning of duplicate lines." 1>&2
awk '{print $2 " " $1}' "$1" | sort | grep -v "generic"
