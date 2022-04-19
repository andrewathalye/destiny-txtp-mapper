#!/bin/sh
./tools-ada/search/search | grep -vE '^( )0|generic' | sort -rg
