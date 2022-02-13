#!/bin/sh
# TODO - turn this into a fully functional make file

gnatmake wordsxml -O3 -largs -lgcc_s.1
strip wordsxml makedict makestem makeewds makeefil makeinfl
gnatmake makedict -O3 -largs -lgcc_s.1
gnatmake makestem -O3 -largs -lgcc_s.1
gnatmake makeewds -O3 -largs -lgcc_s.1
gnatmake makeefil -O3 -largs -lgcc_s.1
gnatmake makeinfl -O3 -largs -lgcc_s.1

cp wordsxml ../dist/Darwin_ppc-gcc4
cp makedict ../tools/Darwin_ppc-gcc4
cp makestem ../tools/Darwin_ppc-gcc4
cp makeewds ../tools/Darwin_ppc-gcc4
cp makeefil ../tools/Darwin_ppc-gcc4
cp makeinfl ../tools/Darwin_ppc-gcc4

echo G | ./makedict
cp DICTFILE.GEN ../dist/dict_ppc
echo G | ./makestem
cp STEMFILE.GEN ../dist/dict_ppc
cp INDXFILE.GEN ../dist/dict_ppc
./makeefil 
cp EWDSFILE.GEN ../dist/dict_ppc
./makeinfl
cp INFLECTS.SEC ../dist/dict_ppc
cp ADDONS.LAT ../dist/dict_ppc
cp UNIQUES.LAT ../dist/dict_ppc
