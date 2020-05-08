unit ZTestDbc;

{$I ZDbc.inc}

interface

uses
  ZTestDbcDriver, ZTestDbcResultSet, ZTestDbcUtils, ZTestDbcCache,
  ZTestDbcCachedResultSet, ZTestDbcMetadata, ZTestDbcResultSetMetadata, ZTestDbcResolver,
  {$IFDEF ENABLE_ADO}
  ZTestDbcADO,
  {$ENDIF}
  {$IFDEF ENABLE_SQLITE}
  ZTestDbcSqLite,
  {$ENDIF}
  {$IFDEF ENABLE_POSTGRESQL}
  ZTestDbcPostgreSqlMetadata,  ZTestDbcPostgreSql,
  {$ENDIF}
  {$IFDEF ENABLE_ORACLE}
  ZTestDbcOracle,
  {$ENDIF}
  {$IFDEF ENABLE_MYSQL}
  ZTestDbcMySqlMetadata, ZTestDbcMySql,
  {$ENDIF}
  {$IFDEF ENABLE_DBLIB}
  ZTestDbcMsSql,
  {$ENDIF}
  {$IF defined(ENABLE_INTERBASE) or defined(ENABLE_FIREBIRD)}
  ZTestDbcInterbaseMetadata, ZTestDbcInterbase,
  {$IFEND}
  {$IFDEF ENABLE_ASA}
  ZTestDbcASA, ZTestDbcASAMetadata,
  {$ENDIF}
  {$IFDEF ENABLE_ODBC}
  ZTestDbcODBc,
  {$ENDIF}
  ZTestDbcGeneric
  ;

implementation

end.

