#!/bin/sh
# TODO - turn this into a fully functional make file

gnatmake wordsxml -O3 -o ../dist/Linux_x86-gcc4/wordsxml -bargs -static
strip ../dist/Linux_x86-gcc4/wordsxml 
gnatmake makedict -O3 -o ../tools/Linux_x86-gcc4/makedict -bargs -static
gnatmake makestem -O3 ../tools/Linux_x86-gcc4/makestem -bargs -static
gnatmake makeewds -O3 -o ../tools/Linux_x86-gcc4/makeewds -bargs -static
gnatmake makeefil -O3 -o ../tools/Linux_x86-gcc4/makeefil -bargs -static
gnatmake makeinfl -O3 -o ../tools/Linux_x86-gcc4/makeinfl -bargs -static
strip ../tools/Linux_x86-gcc4/make*

