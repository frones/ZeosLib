unit ZTestParseSql;

{$I ZParseSql.inc}

interface

uses
  {$IFDEF ENABLE_DBLIB}
    ZTestSybaseToken,
  {$ENDIF}
  {$IFDEF ENABLE_MYSQL}
    ZTestMySqlToken,
  {$ENDIF}
  {$IFDEF ENABLE_POSTGRESQL}
    ZTestPostgreSqlToken,
  {$ENDIF}
  {$IFDEF ENABLE_INTERBASE}
    ZTestInterbaseToken,
  {$ENDIF}
  {$IFDEF ENABLE_ORACLE}
    ZTestOracleToken,
  {$ENDIF}
  {$IFDEF ENABLE_SQLITE}
    ZTestSqLiteToken,
  {$ENDIF}
  ZTestSqlAnalyser, ZTestScriptParser
  ;
implementation

end.

