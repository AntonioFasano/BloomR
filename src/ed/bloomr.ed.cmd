
:: BloomR Environment Diagnostic
:: =============================

@Echo off

:: Up from bloomR\main\ed\
cd ..\..

Set "BLOOMR=%CD%"
Echo BLOOMR env var is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\mybloomr"
Echo HOME env var is:
Echo %HOME%

Set "JAVA_HOME=%BLOOMR%\main\openjdk\jre"
Echo JAVA_HOME env var is:
Echo %JAVA_HOME%

Echo Current dir set to:
cd   %HOME%
Echo %CD%

REM Set PATH=%JAVA_HOME%\bin;%PATH%

Echo Start ..\main\R\bin\x64\Rgui.exe   --internet2 LANGUAGE=en
     Start ..\main\R\bin\x64\Rgui.exe   --internet2 LANGUAGE=en
Pause


