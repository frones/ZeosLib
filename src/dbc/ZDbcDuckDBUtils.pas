unit ZDbcDuckDBUtils;

{$I zdbc.inc}

interface

uses
  Classes, SysUtils;

{$IFNDEF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit

const
  {$IFNDEF FPC}
  DuckDBMicrosecondsPerDay: Double = Double(24) * 60 *60 *1000 * 1000;
  {$ELSE}
  DuckDBMicrosecondsPerDay = 24 * 60 *60 *1000 * 1000;
  {$ENDIF}

var
  //the encoded date of 1970-01-01
  DuckDBDateShift: Integer;

{$ENDIF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit

uses
  DateUtils;

initialization
  DuckDBDateShift := Trunc(EncodeDate(1970, 1, 1));
  //DuckDBMicrosecondsPerDay = 24 * 60 *60 *1000 * 1000;

{$ENDIF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit

end.

