; TODO - turn this into a fully functional make file

gnatmake -O3 wordsxml
gnatmake -O3 makedict
gnatmake -O3 makestem
gnatmake -O3 makeewds
gnatmake -O3 makeefil
gnatmake -O3 makeinfl
strip wordsxml.exe make*.exe

copy wordsxml.exe ..\dist\WINNT_x86-gcc3
copy makedict.exe ..\tools\WINNT_x86-gcc3
copy makestem.exe ..\tools\WINNT_x86-gcc3
copy makeewds.exe ..\tools\WINNT_x86-gcc3
copy makeefil.exe ..\tools\WINNT_x86-gcc3
copy makeinfl.exe ..\tools\WINNT_x86-gcc3

echo G | .\makedict  
copy DICTFILE.GEN ..\dist\dict_x86
echo G | .\makestem 
copy STEMFILE.GEN ..\dist\dict_x86
copy INDXFILE.GEN ..\dist\dict_x86
.\makeefil 
copy EWDSFILE.GEN ..\dist\dict_x86
.\makeinfl
copy INFLECTS.SEC ..\dist\dict_x86
copy ADDONS.LAT ..\dist\dict_x86
copy UNIQUES.LAT ..\dist\dict_x86
