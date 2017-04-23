
:: BRemacs Environment Diagnostic
:: ==============================

@Echo off

:: Up from bloomR\main\ed\
cd ..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\main\bremacs"
Echo Temp HOME env var is:
Echo %HOME%

Set "JAVA_HOME=%BLOOMR%\main\openjdk\jre"
Echo JAVA_HOME env var is:
Echo %JAVA_HOME%

Echo Temp work dir set to:
Echo %CD%


REM Set PATH=%JAVA_HOME%\bin;%PATH%
Echo Start main\bremacs\bin\runemacs.exe -q --no-splash
     Start main\bremacs\bin\runemacs.exe -q --no-splash
Pause

