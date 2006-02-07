set Host=localhost
set Database=zeoslib
set user=seeger
set Password=seeger
set pg_home=d:\sqlserverfarm\postgre\81\bin

%pg_home%\dropdb -U %User% %Database%
%pg_home%\createdb -U %User% %Database%

%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\create_postgresql.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\populate_any.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\populate_postgresql.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\create_postgresql_bugreport.sql
