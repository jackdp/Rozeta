@echo off

rem --------------- common -----------------
set AppName=Rozeta
set AppVer=3.0
set AppFullName=%AppName% %AppVer%
set AppName_=Rozeta
set AppUrl=https://www.pazera-software.com/products/rozeta/
set CompressionLevel=7
set CommonFiles=-xr!vx.json lang\* presets\*.json presets\*.rozt 180.roz 180_symmetry.roz analiza.roz example1.roz example2.roz example3_polygon.roz big_10000.rozx README.txt
set DocFiles=Readme.md Rozeta_description_EN.md Rozeta_description_PL.md doc_images\*.png

::del presets\_LastSession.json
ren presets\_LastSession.json _LastSession.tmp

rem ----------------- 32 bit ---------------------
set AppExe32Compiled=Rozeta32.exe
set PortableFileZip32=%AppName_%_32bit_PORTABLE.zip
set CreatePortableZip32=7z a -tzip -mx=%CompressionLevel% %PortableFileZip32% %AppExe32Compiled% %CommonFiles% %DocFiles%


rem ----------------- 64 bit ---------------------
set AppExe64Compiled=Rozeta64.exe
set PortableFileZip64=%AppName_%_64bit_PORTABLE.zip
set CreatePortableZip64=7z a -tzip -mx=%CompressionLevel% %PortableFileZip64% %AppExe64Compiled% %CommonFiles% %DocFiles%




if exist %PortableFileZip32% del %PortableFileZip32%
%CreatePortableZip32%

if exist %PortableFileZip64% del %PortableFileZip64%
%CreatePortableZip64%



ren presets\_LastSession.tmp _LastSession.json
