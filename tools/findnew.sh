#!/bin/sh
echo "$0 [clean matches file] [tracks file]: find banks not yet seen" 1>&2
while read -n14 id # Read bank ID
do
	read num # Finish line
	if [[ -z $( grep $id "$2" ) ]]
	then
		echo "$num" "$id"
	fi
	
done <  "$1"

