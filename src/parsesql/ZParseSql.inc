{$IFDEF FREEBSD}
  {$DEFINE UNIX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

{$IFNDEF UNIX}
{$I ..\Zeos.inc}
{$ELSE}
{$I ../Zeos.inc}
{$ENDIF}
