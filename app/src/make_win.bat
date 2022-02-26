del *.o
del *.exe

gnatmake -O3 wordsxml
gnatmake -O3 makedict
gnatmake -O3 makestem
gnatmake -O3 makeewds
gnatmake -O3 makeefil
gnatmake -O3 makeinfl
strip wordsxml.exe make*.exe

echo G | .\makedict  
echo G | .\makestem 
.\makeefil 
.\makeinfl


