@ECHO OFF

:: Set your values here
set Host=localhost
set Database=zeoslib
set User=root
set Password=root
set MySQLBin=C:\Coding\DB\MySQL\bin

echo drop database %Database% | "%MySQLBin%\mysql" -h %Host% -u %User% -p%Password%
echo create database %Database% | "%MySQLBin%\mysql" -h %Host% -u %User% -p%Password% || goto :Err

"%MySQLBin%\mysql" -h %Host% -u %User% -p%Password% "%Database%" < ..\create_mysql.sql || goto :Err
"%MySQLBin%\mysql" -h %Host% -u %User% -p%Password% "%Database%" < ..\populate_any.sql || goto :Err
"%MySQLBin%\mysql" -h %Host% -u %User% -p%Password% "%Database%" < ..\populate_mysql.sql || goto :Err
"%MySQLBin%\mysql" -h %Host% -u %User% -p%Password% "%Database%" < ..\create_mysql_bugreport.sql || goto :Err

echo Database created and filled successfully
timeout /t 3
goto :EOF

:Err
pause