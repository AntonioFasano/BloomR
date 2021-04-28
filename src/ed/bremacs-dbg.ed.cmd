
:: BRemacs Environment Diagnostic (debug)
:: ======================================

@Echo off
SETLOCAL

:: Up from bloomR\main\ed\
cd /D %~dp0\..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\main"
Echo Temp HOME env var is (reset in site-start.el):
Echo %HOME%

Echo Temp work dir set to (reset in site-start.el):
Echo %CD%

:: 1 causes to load br-init-dbg.el insetead of br-init-dbg.el
set "BREMACSDBG=1"

:: Breamcs site-start.el calls br-init(-dbg)
Echo Start main\bremacs\bin\runemacs.exe -q --no-splash %%*
     Start main\bremacs\bin\runemacs.exe -q --no-splash %*
Pause

:: Call directly br-init.el, removing site-start.el
:: ..\main\bremacs\bin\runemacs.exe  -q --load %~dp0share\emacs\site-lisp\br-init-dbg.el %*


