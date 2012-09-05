unit ZTestBugReports;

{$I ZBugReport.inc}

interface

uses
{$IFDEF ENABLE_MYSQL}
  ZTestBugDbcMySql, ZTestBugCompMySql,
{$ENDIF}
{$IFDEF ENABLE_ORACLE}
  ZTestBugDbcOracle, ZTestBugCompOracle,
{$ENDIF}
{$IFDEF ENABLE_POSTGRESQL}
  ZTestBugDbcPostgreSql, ZTestBugCompPostgreSql,
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
  ZTestBugDbcInterbase, ZTestBugCompInterbase,
{$ENDIF}
{$IFDEF ENABLE_DBLIB}
  {ludob empty test ZTestBugDbcDbLib,}
  {ludob empty test ZTestBugCompDbLib, }
  ZTestBugCompMSSql,
{$ENDIF}
{$IFDEF ENABLE_SQLITE}
  ZTestBugCompSQLite,
{$ENDIF}
  {ludob empty test ZTestBugDbcCore,}
  ZTestBugCompCore;

implementation

end.

