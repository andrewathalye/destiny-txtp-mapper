#!/bin/sh
./tools-ada/search/search | grep -vE '^( )0' | awk '{print $2 " " $1}' | LC_ALL=C sort | grep -v "generic" | ./tools-ada/cleaner/cleaner | sort -rg | awk '{ print $2 " " $1}'
