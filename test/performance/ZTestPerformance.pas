unit ZTestPerformance;

{$I ZPerformance.inc}

interface

uses
{$IFDEF WIN64}
  {$DEFINE EXCLUDE_BDE_TEST}
{$ENDIF}
{$IFNDEF EXCLUDE_BDE_TEST}
  ZTestBdePerformance,
{$ENDIF}
{$IFNDEF EXCLUDE_DBX_TEST}
  ZTestDbxPerformance,
{$ENDIF}
{$IFNDEF EXCLUDE_IBX_TEST}
  ZTestIBXPerformance,
{$ENDIF}
{$IFNDEF EXCLUDE_OLD_ZEOS_TEST}
  ZTestOldZeosPerformance,
{$ENDIF}
  ZTestDbcPerformance,
  ZTestDatasetPerformance;


implementation

end.
