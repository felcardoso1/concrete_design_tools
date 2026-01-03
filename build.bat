@echo off
REM Build script for Concrete Design Tools (Windows)
REM Compiles all Pascal source files and places binaries in AGENT\ folder
REM
REM Uses smart linking (-XX -CX) to produce ~121KB binaries instead of ~536KB.
REM See docs/BINARY_SIZE_OPTIMIZATION.md for details.

setlocal enabledelayedexpansion

echo Building Concrete Design Tools...
echo Source: src\
echo Output: AGENT\
echo.

REM Ensure AGENT directory exists
if not exist AGENT mkdir AGENT

set COUNT=0
set FAILED=0

REM Compile each .pas file
for %%f in (src\*.pas) do (
    set /a COUNT+=1
    set "filename=%%~nf"
    echo [!COUNT!] Compiling !filename!...

    fpc -O2 -Xs -XX -CX -oAGENT\!filename!.exe "%%f" >nul 2>&1
    if !errorlevel! equ 0 (
        echo          -^> AGENT\!filename!.exe
    ) else (
        echo          -^> FAILED
        set /a FAILED+=1
    )
)

echo.
echo Build complete!
set /a SUCCESS=COUNT-FAILED
echo   Compiled: !SUCCESS!/!COUNT!
if !FAILED! gtr 0 (
    echo   Failed: !FAILED!
    exit /b 1
)

REM Clean up object files
del /q AGENT\*.o 2>nul
del /q src\*.o src\*.ppu 2>nul

echo.
echo Binaries are in: AGENT\
echo To use: cd AGENT ^&^& flexao.exe --help

endlocal
