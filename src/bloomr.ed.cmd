
:: BloomR Environment Diagnostic
:: =============================

@Echo Current directory is:
cd 
Set HOME=%BLOOMR%mybloomr
Set JAVA_HOME=%BLOOMR%main\openjdk\jre
REM Set PATH=%JAVA_HOME%\bin;%PATH%
Start main\bin\x64\Rgui.exe   --internet2 LANGUAGE=en
Pause


