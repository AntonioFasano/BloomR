
:: BloomR Environment Diagnostic
:: =============================

@Echo off
SETLOCAL

:: Up from bloomR\main\ed\
cd /D %~dp0\..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\mybloomr"
Echo HOME env var is:
Echo %HOME%

Echo Current dir set to:
cd   %HOME%
Echo %CD%

Echo Start ..\main\R\bin\x64\Rgui.exe  LANGUAGE=en
     Start ..\main\R\bin\x64\Rgui.exe  LANGUAGE=en
Pause


