
:: BloomR Environment Diagnostic
:: =============================

@Echo off

:: bloomR\main\ed\
cd ..\..

Set "BLOOMR=%CD%"
Echo BLOOMR environment directory is:
Echo %BLOOMR%

Set "HOME=%BLOOMR%\mybloomr"
Echo HOME environment directory is:
Echo %HOME%

Set "JAVA_HOME=%BLOOMR%\main\openjdk\jre"
Echo JAVA_HOME environment directory is:
Echo %JAVA_HOME%

Echo Current dir set to:
Echo %HOME%
cd  %HOME%

REM Set PATH=%JAVA_HOME%\bin;%PATH%

Start ..\main\R\bin\x64\Rgui.exe   --internet2 LANGUAGE=en
Pause


