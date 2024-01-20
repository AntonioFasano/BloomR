
:: BloomR Environment Diagnostic
:: =============================

@Echo off
SETLOCAL

:: Up from bloomR\apps\ed\
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

Set "bloomr_branch=core"
Echo BloomR edition is %bloomr_branch%

Echo Start ..\apps\R\bin\x64\Rgui.exe LANGUAGE=en
     Start ..\apps\R\bin\x64\Rgui.exe LANGUAGE=en
Pause
