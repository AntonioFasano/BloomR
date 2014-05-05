EnvSet, HOME,      %A_ScriptDir%\mybloomr
EnvSet, JAVA_HOME, %A_ScriptDir%\main\openjdk
EnvSet, PATH,      %A_ScriptDir%\main\openjdk\bin;%path%
Run, main\bin\x64\Rgui.exe
