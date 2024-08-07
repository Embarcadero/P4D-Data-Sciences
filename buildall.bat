@echo off
call rsvars.bat
call get-depends.bat
msbuild /bl /p:BuildGroup=RunTime P4DDataSciencesComponentSuite.groupproj
python.exe convert-depends-to-sbom.py
