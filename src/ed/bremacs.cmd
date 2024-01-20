
:: BRemacs Environment Diagnostic
:: ==============================

@Echo off
SETLOCAL

:: Up from bloomR\apps\ed\
cd /D %~dp0\..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%


Set "HOME=%BLOOMR%\mybloomr"
If Exist "%HOME%\" (
   Echo HOME env var is:
   Echo %HOME%
) Else (
   Echo Unable to find HOME directory:
   Echo %HOME%
   Pause
   Exit /b 1
)

If Exist "%BLOOMR%\apps\bremacs\.emacs.d\" (
   Echo BR/Emacs user directory:
   Echo %BLOOMR%\apps\bremacs\.emacs.d\
) Else (
   Echo Unable to find BR/Emacs user directory:
   Echo %BLOOMR%\apps\bremacs\.emacs.d\
   Pause
   Exit /b 1
)

If Exist "%BLOOMR%\apps\bremacs\.emacs.d\early-init.el" (
    Echo BR/Emacs init file is:
    Echo %BLOOMR%\apps\bremacs\.emacs.d\early-init.el
) Else (
    Echo Unable to find BR/Emacs init file is:
    Echo %BLOOMR%\apps\bremacs\.emacs.d\early-init.el
    Pause
    Exit /b 1
)

Echo Temp work dir is set to (init files will change it to HOME):
Echo %CD%

Set "bloomr_branch=bremacs"
Echo BloomR edition is %bloomr_branch%


:: Only emacs.exe can print message() on the cmd console
If "%BREMACSDBG%"=="1" (
   Set "bremacs_path=apps\bremacs\bin\emacs.exe"
) Else (
   Set "bremacs_path=apps\bremacs\bin\runemacs.exe"
)

Echo %bremacs_path% --init-dir apps\bremacs\.emacs.d
     %bremacs_path% --init-dir apps\bremacs\.emacs.d

:: Note: Only emacs.exe can detect error level
If ERRORLEVEL 1 (echo: &Echo There was a non-zero exit code.)


If "%bremacs_path%"=="apps\bremacs\bin\runemacs.exe" (
   echo: &Echo For early stage errors, debug with bremacs-dbg.cmd
)

Pause
