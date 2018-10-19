@ECHO OFF

:: Set your values here
set Database=%~dp0%\..\..\build\testenv\zeoslib.db
set SQLite_home=C:\Coding\DB\SQLite

del %Database% 2> nul

echo .exit | "%SQLite_home%\sqlite3" -init ..\create_sqlite3.sql "%Database%" || goto :Err
echo .exit | "%SQLite_home%\sqlite3" -init ..\populate_any.sql "%Database%" || goto :Err

echo Database created and filled successfully
timeout /t 3
goto :EOF

:Err
pause