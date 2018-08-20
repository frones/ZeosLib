{$IFDEF LINUX}{$DEFINE UNIX}{$ENDIF} // Kylix legacy.

{$IFNDEF UNIX}
  {$I ..\..\src\Zeos.inc}
  {$I ..\ZTest.inc}
{$ELSE}
  {$I ../../src/Zeos.inc}
  {$I ../ZTest.inc}
{$ENDIF}