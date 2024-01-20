:: Simple run.cs/exe tests
:: Look for possible FAILURE word in batch output

@echo off
set "framework=v4.0.30319"

:: compile test
c:\windows\Microsoft.NET\Framework64\%framework%\csc run.cs 
if not errorlevel 1 (echo SUCCESS) else (echo FAILURE)

:: standard way
echo. & run -log run_cs_log.txt  ping google.com -n 1
if not errorlevel 1 (echo SUCCESS) else (echo FAILURE)

:: appending
echo. & run -alog run_cs_log.txt ping google.com -n 1
if not errorlevel 1 (echo SUCCESS) else (echo FAILURE)

:: no log
echo. & run -nolog ping google.com -n 1
if not errorlevel 1 (echo SUCCESS) else (echo FAILURE)

:: not in path exe (trashed long output)
echo. & run -nolog c:\windows\Microsoft.NET\Framework64\%framework%\csc /help > NUL
if not errorlevel 1 (echo SUCCESS) else (echo FAILURE)

echo. & pause
