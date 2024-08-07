@echo off
setlocal enabledelayedexpansion

:: Path to the shared file listing the repositories
set REPO_FILE=depends.txt

:: Loop through each line in the file
for /f "tokens=1,2 delims=," %%i in (%REPO_FILE%) do (
    set "REPO_URL=%%i"
    set "TARGET_DIR=%%j"

    if exist "!TARGET_DIR!" (
        echo Updating !TARGET_DIR!...
        cd /d "!TARGET_DIR!"
        git pull
        cd /d %~dp0
    ) else (
        echo Cloning !REPO_URL! into !TARGET_DIR!...
        git clone "!REPO_URL!" "!TARGET_DIR!"
    )
)

endlocal
