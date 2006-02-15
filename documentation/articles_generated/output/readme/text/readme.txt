
This document was created using the >e-novative> DocBook Environment (eDE)






ZeosDBO Technical Info
======================





ZeosLib Development Group
=========================




09 July 2004



===========================================================================


ZeosDBO is a database middleware components for Borland development
tools, including Delphi, C++ Builder and Kylix.

The following compilers are supported:


* Delphi 5 - 7 and 9
* Lazarus (FreePascal)
* C++ Builder 5 - 6
* Kylix 2 - 3


ZeosDBO supports direct connectivity to the following databases using the
vendor provided, native interface:


* MySQL 3.20 - 4.1
* PostgreSQL 6.5 - 7.4
* Firebird 1.0 - 1.5
* Interbase 5.0 - 7.5
* Microsoft SQL Server 7, 2000
* Sybase ASE 12.0, 12.5
* Oracle 9i
* SQLite 2.8


For other databases we propose to use implemented Active Data Objects
(ADO) Bridge.

Advantages of using ZeosDBO:


* Platform independance. The ZeosDBO is highly generic. Applications
  written in ZeosDBO can be migrated across databases without major
  changes.
* ZeosDBO is open source, written for usability and extensibility.
* ZeosDBO leverages the amazing power of the Delphi development
  environment without relying on a performance killing middleware.
* ZeosDBO is an extremely thin abstraction layer, unlike 'thick'
  layered protocols like ADO and BDE.


Package contents:


1. ZCore - Core classes and interfaces. Contains Java style objects
   and collections as well as compatibility types and functions.
2. ZParseSql - SQL specific for syntax and lexical analysis.
3. ZPlain - Native plain API to supported SQL servers.
4. ZDbc - Port of Java Database Connectivity API (JDBC 2.0). DBC API
   acts as intermediate layer between Plain API and highlevel TDataset or
   DBExpress components
5. ZComponent - visual components descended from TDataset.


Installed components:


1. TZConnection: This component encapsulates the database connection
   and transaction management.
2. TZReadOnlyQuery: TDataset component to execute SQL queries and
   process data in read-only mode.
3. TZQuery: TDataset component which allows data modifications in
   regular and cached mode.
4. TZUpdateSQL: Analog of standard TUpdateSQL component to explicite
   definition of Insert/Update/Delete SQL statements for TDataset
   modifications.
5. TZStoredProc: The component to execute SQL stored procedures.
6. TZSQLProcessor: The component to execute SQL scripts for different
   SQL and various delimiter types.
7. TZSQLMonitor: The component to monitor all outgoing SQL queries and
   other logging information.
8. TZSQLMetadata: Specialized TDataset component which provides an
   access to database metadata such as tables, columns, indices, etc.


The project home page is here (for news, links and other project info):
http://www.zeoslib.net

The sourceforge development site is located here (for technical resources
and anonymous web based cvs access):
http://www.sourceforge.net/projects/zeoslib

Thank you for using our software,

The ZeosLib Development Group


This document was created using the >e-novative> DocBook Environment (eDE)

