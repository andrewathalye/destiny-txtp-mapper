#!/bin/sh
./tools-ada/find/find "$1" | awk '{ print $2 " " $1 }' | sort -rg
