unit zplainplatform;

interface

{$I zpackages.inc}

{$warn 5023 off : no warning about unused units}

uses 
{$IF defined(ENABLE_ADO) or defined(ENABLE_OLEDB)}
  ZOleDB,
{$IFEND}

{$IFDEF ENABLE_OLEDB}
  ZPlainOleDBDriver,
{$ENDIF}

{$IFDEF ENABLE_ADO}
  ZPlainAdoDriver,
  ZPlainAdo,
{$ENDIF}
  ZCompatibility;  // just to have one unit used always

implementation

end.
