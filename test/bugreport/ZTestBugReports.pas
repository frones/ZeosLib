unit ZTestBugReports;

{$I ZBugReport.inc}

interface

uses
{$IFDEF ENABLE_ADO}
  ZTestBugCompADO,
{$ENDIF}
{$IFDEF ENABLE_ASA}
  ZTestBugDbcASA, ZTestBugCompASA,
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
  ZTestBugDbcCore,
  ZTestBugCompCore;

implementation

end.

