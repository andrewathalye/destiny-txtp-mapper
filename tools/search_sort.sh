#!/bin/sh
./src/search/search | grep -vE '^( )0|generic' | sort -rg | cut -b 2-
