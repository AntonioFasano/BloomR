
:: BRemacs Environment Diagnostic
:: ==============================

@Echo off
SETLOCAL

:: Up from bloomR\main\ed\
cd /D %~dp0\..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\main\bremacs"
Echo Temp HOME env var is (reset in site-start.el):
Echo %HOME%

Echo Temp work dir set to (reset in site-start.el):
Echo %CD%

Echo Start main\bremacs\bin\runemacs.exe -q --no-splash
     Start main\bremacs\bin\runemacs.exe -q --no-splash
Pause

