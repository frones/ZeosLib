unit ZTestBugReports;

{$I ZBugReport.inc}

interface

uses
{$IFDEF ENABLE_ASA}
  ZTestBugDbcASA, ZTestCompASABugReport,
{$ENDIF}
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
  ZTestBugDbcDbLib,
  ZTestBugCompDbLib,
  ZTestBugCompMSSql,
{$ENDIF}
{$IFDEF ENABLE_SQLITE}
  ZTestBugCompSQLite,
{$ENDIF}
  {ludob empty test ZTestBugDbcCore,}
  ZTestBugCompCore;

implementation

end.

