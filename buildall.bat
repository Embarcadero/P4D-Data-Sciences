@echo off
call rsvars.bat
call get-depends.bat
msbuild /p:BuildGroup=RunTime P4DWrapStack.groupproj
python.exe convert-depends-to-sbom.py
