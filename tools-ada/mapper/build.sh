#!/bin/sh
gnatmake-10.3.1 -gnatva -gnatwa -O2 -D obj src/main.adb -bargs -shared -margs -o mapper
