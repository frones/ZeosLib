@ECHO OFF

:: Set your values here
set Database=%~dp0%\..\..\build\testenv\zeoslib.fdb
set User=SYSDBA
set Password=masterkey
set fb_home=C:\Coding\DB\Firebird\bin

del "%Database%" 2> nul
echo CREATE DATABASE "%Database%" USER "%User%" PAGE_SIZE=4096; exit; | "%fb_home%\isql" -u %User% -p %Password% -s 3 || goto :Err

"%fb_home%\isql" "%Database%" -u %User% -p %Password% -s 3 < ..\create_interbase.sql || goto :Err
"%fb_home%\isql" "%Database%" -u %User% -p %Password% -s 3 < ..\create_interbase_bugreport.sql || goto :Err
"%fb_home%\isql" "%Database%" -u %User% -p %Password% -s 3 < ..\populate_any.sql || goto :Err
"%fb_home%\isql" "%Database%" -u %User% -p %Password% -s 3 < ..\populate_interbase.sql || goto :Err

echo Database created and filled successfully
timeout /t 3
goto :EOF

:Err
pause