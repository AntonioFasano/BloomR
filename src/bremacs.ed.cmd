
:: Bremacs Environment Diagnostic
:: =============================

@Echo Current directory is:
cd 
Set HOME=%BLOOMR%mybloomr
Set JAVA_HOME=%BLOOMR%main\openjdk\jre
REM Set PATH=%JAVA_HOME%\bin;%PATH%
Start main\bremacs\bin\runemacs.exe -q --no-splash
Pause

