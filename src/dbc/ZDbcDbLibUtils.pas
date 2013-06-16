{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                DBLib Utility Functions                  }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDbLibUtils;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZVariant, ZDbcIntfs, ZPlainDBLibDriver, ZCompatibility;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt; CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertDBLibToSqlType(FieldType: SmallInt; CtrlsCPType: TZControlsCodePage): TZSQLType;
function ConvertFreeTDSToSqlType(FieldType: SmallInt; CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(Value: string): TZSQLType;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibType(FieldType: TZSQLType): Integer;
function ConvertSqlTypeToFreeTDSType(FieldType: TZSQLType): Integer;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSQLType): string;
function ConvertSqlTypeToFreeTDSTypeName(FieldType: TZSQLType): string;

{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSQLParameter(Value: TZVariant; ParamType: TZSQLType;
  ConSettings: PZConSettings; PlainDriver: IZDBLibPlainDriver): ZAnsiString;

implementation

uses Types, ZSysUtils, ZPlainDbLibConstants, ZEncoding, ZDbcUtils
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    1, 12, -8, -9: Result := stString;
    -7: Result := stBoolean;
//Bug #889223, bug with tinyint on mssql
//    -6: Result := stByte;
    -5: Result := stLong;
    -6: Result := stShort;
    5: Result := stShort;
    4: Result := stInteger;
    2, 3, 6, 7, 8: Result := stDouble;
    11, 93: Result := stTimestamp;
    -1, -10: Result := stAsciiStream;
    -3, -4, -11: Result := stBinaryStream;
    -2: Result := stBytes;
  else
    Result := stUnknown;
  end;
  if CtrlsCPType = cCP_UTF16 then
  case Result of
    stString: Result := stUnicodeString;
    stAsciiStream: Result := stUnicodeStream;
  end;
end;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertDBLibToSqlType(FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    DBLIBSQLCHAR: Result := stString;
    DBLIBSQLBIT: Result := stBoolean;
//Bug #889223, bug with tinyint on mssql
//    DBLIBSQLINT1: Result := stByte;
    DBLIBSQLINT1: Result := stShort;
    DBLIBSQLINT2: Result := stShort;
    DBLIBSQLINT4: Result := stInteger;
    DBLIBSQLFLT4: Result := stDouble;
    DBLIBSQLFLT8: Result := stDouble;
    DBLIBSQLMONEY4: Result := stDouble;
    DBLIBSQLMONEY: Result := stDouble;
    DBLIBSQLDATETIM4: Result := stTimestamp;
    DBLIBSQLDATETIME: Result := stTimestamp;
    DBLIBSQLTEXT: Result := stAsciiStream;
    DBLIBSQLIMAGE: Result := stBinaryStream;
    DBLIBSQLBINARY: Result := stBinaryStream;
  else
    Result := stUnknown;
  end;
  if CtrlsCPType = cCP_UTF16 then
  case Result of
    stString: Result := stUnicodeString;
    stAsciiStream: Result := stUnicodeStream;
  end;
end;

{**
  Converts a FreeTDS native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertFreeTDSToSqlType(FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    SYBCHAR, SYBVARCHAR, XSYBCHAR, XSYBVARCHAR: Result := stString;
    SYBINTN, SYBINT4:                           Result := stInteger;
    SYBINT8:                                    Result := stLong;
    SYBNUMERIC:                                 Result := stBigDecimal;
    SYBINT1, SYBINT2:                           Result := stShort;
    SYBFLT8, SYBFLTN, SYBREAL, SYBDECIMAL:      Result := stDouble;
    SYBDATETIME, SYBDATETIME4, SYBDATETIMN:     Result := stTimestamp;
    SYBBIT, SYBBITN:                            Result := stBoolean;
    SYBTEXT:                                    Result := stAsciiStream;
    SYBNTEXT:                                   Result := stUnicodeStream;
    SYBIMAGE, SYBBINARY, SYBVARBINARY,
    XSYBBINARY, XSYBVARBINARY:                  Result := stBinaryStream;
    SYBMONEY4, SYBMONEY, SYBMONEYN:             Result := stDouble;
    SYBVOID:                                    Result := stUnknown;
    SYBNVARCHAR, XSYBNCHAR, XSYBNVARCHAR:       Result := stUnicodeString;
    SYBMSXML:                                   Result := stBinaryStream;
    SYBUNIQUE:                                  Result := stString;
    SYBVARIANT:                                 Result := stString;
    SYBMSUDT:                                   Result := stString;
    else
      Result := stUnknown;
  end;
  if CtrlsCPType = cCP_UTF16 then
  case Result of
    stString: Result := stUnicodeString;
    stAsciiStream: Result := stUnicodeStream;
  end;
end;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(Value: string): TZSQLType;
begin
  Result := stUnknown;
end;

{**
  Converts ZDBC SQL types into DBLib native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibType(FieldType: TZSQLType): Integer;
begin
  Result := -1;
  case FieldType of
    stBoolean: Result := DBLIBSQLBIT;
    stByte: Result := DBLIBSQLINT1;
    stShort: Result := DBLIBSQLINT2;
    stInteger: Result := DBLIBSQLINT4;
    stLong: Result := DBLIBSQLFLT8;
    stFloat: Result := DBLIBSQLFLT8;
    stDouble: Result := DBLIBSQLFLT8;
    stBigDecimal: Result := DBLIBSQLFLT8;
    stString: Result := DBLIBSQLCHAR;
    stBytes: Result := DBLIBSQLBINARY;
    stDate: Result := DBLIBSQLDATETIME;
    stTime: Result := DBLIBSQLDATETIME;
    stTimestamp: Result := DBLIBSQLDATETIME;
    stAsciiStream: Result := DBLIBSQLTEXT;
    stUnicodeStream: Result := DBLIBSQLIMAGE;
    stBinaryStream: Result := DBLIBSQLIMAGE;
  end;
end;

{**
  Converts ZDBC SQL types into DBLib native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSQLType): string;
begin
  Result := '';
  case FieldType of
    stBoolean: Result := 'bit';
    stByte: Result := 'tinyint';
    stShort: Result := 'smallint';
    stInteger: Result := 'int';
    stLong: Result := 'bigint';
    stFloat: Result := 'float(24)';
    stDouble: Result := 'float(53)';
    stBigDecimal: Result := 'float(53)';
    stString: Result := 'varchar(8000)';
    stBytes: Result := 'varbinary(8000)';
    stDate: Result := 'datetime';
    stTime: Result := 'datetime';
    stTimestamp: Result := 'datetime';
    stAsciiStream: Result := 'text';
    stUnicodeStream: Result := 'ntext';
    stBinaryStream: Result := 'image';
  end;
end;

{**
  Converts ZDBC SQL types into FreeTDS native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToFreeTDSType(FieldType: TZSQLType): Integer;
begin
  Result := -1;
  case FieldType of
    stBoolean: Result := SYBBIT;
    stByte: Result := SYBINT1;
    stShort: Result := SYBINT2;
    stInteger: Result := SYBINT4;
    stLong: Result := SYBFLT8;
    stFloat: Result := SYBFLT8;
    stDouble: Result := SYBFLT8;
    stBigDecimal: Result := SYBFLT8;
    stString: Result := SYBCHAR;
    stUnicodeString: Result := SYBNVARCHAR;
    stBytes: Result := SYBBINARY;
    stDate: Result := SYBDATETIME;
    stTime: Result := SYBDATETIME;
    stTimestamp: Result := SYBDATETIME;
    stAsciiStream: Result := SYBTEXT;
    stUnicodeStream: Result := SYBNTEXT;
    stBinaryStream: Result := SYBIMAGE;
  end;
end;

{**
  Converts ZDBC SQL types into FreeTDS native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToFreeTDSTypeName(FieldType: TZSQLType): string;
begin
  Result := '';
  case FieldType of
    stBoolean: Result := 'bit';
    stByte: Result := 'tinyint';
    stShort: Result := 'smallint';
    stInteger: Result := 'int';
    stLong: Result := 'bigint';
    stFloat: Result := 'float(24)';
    stDouble: Result := 'float(53)';
    stBigDecimal: Result := 'float(53)';
    stString: Result := 'varchar(8000)';
    stUnicodeString: Result := 'nvarchar(4000)';
    stBytes: Result := 'varbinary(8000)';
    stDate: Result := 'datetime';
    stTime: Result := 'datetime';
    stTimestamp: Result := 'datetime';
    stAsciiStream: Result := 'text';
    stUnicodeStream: Result := 'ntext';
    stBinaryStream: Result := 'image';
  end;
end;


{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;
const
  Nullability: array[0..2] of TZColumnNullableType =
    (ntNoNulls, ntNullable, ntNullableUnknown);
begin
  Result := Nullability[DBLibNullability];
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSQLParameter(Value: TZVariant; ParamType: TZSQLType;
  ConSettings: PZConSettings; PlainDriver: IZDBLibPlainDriver): ZAnsiString;
var
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
begin
  TempBytes := nil;

  if DefVarManager.IsNull(Value) then
    Result := 'NULL'
  else
  begin
    case ParamType of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then
          Result := '1'
        else
          Result := '0';
      stByte, stShort, stInteger, stLong, stFloat, stDouble, stBigDecimal:
        Result := ZAnsiString(SoftVarManager.GetAsString(Value));
      stString:
        Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(PlainDriver.ZPlainString(SoftVarManager.GetAsString(Value), ConSettings), '''');
      stUnicodeString:
        if ConSettings.ClientCodePage.Encoding = ceUTF8 then
          Result := 'N'+{$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(UTF8Encode(SoftVarManager.GetAsUnicodeString(Value)),'''')
        else
          Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(PlainDriver.ZPlainString(SoftVarManager.GetAsUnicodeString(Value), ConSettings),'''');
      stBytes:
        begin
          TempBytes := StrToBytes(AnsiString(SoftVarManager.GetAsString(Value)));
          if Length(TempBytes) = 0 then
            Result := 'NULL'
          else
            Result := GetSQLHexAnsiString(PAnsiChar(TempBytes), Length(TempBytes), True);
        end;
      stDate:
        Result := ZAnsiString('''' + FormatDateTime(ConSettings^.DateFormat,
          SoftVarManager.GetAsDateTime(Value)) + '''');
      stTime:
        Result := ZAnsiString('''' + FormatDateTime('hh":"mm":"ss":"zzz',
          SoftVarManager.GetAsDateTime(Value)) + '''');
      stTimestamp:
        Result := ZAnsiString('''' + FormatDateTime(ConSettings^.DateFormat+' hh":"mm":"ss":"zzz',
          SoftVarManager.GetAsDateTime(Value)) + '''');
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if ParamType = stBinaryStream then
              Result := GetSQLHexAnsiString(PAnsiChar(TempBlob.GetBuffer), TempBlob.Length, True)
            else
              if ParamType = stUnicodeStream then
              {$IFDEF WITH_UNITANSISTRINGS}
                Result := 'N'+AnsiStrings.AnsiQuotedStr(AnsiStrings.StringReplace(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings), #0, '', [rfReplaceAll]), '''')
              else
                Result := AnsiStrings.AnsiQuotedStr(AnsiStrings.StringReplace(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings), #0, '', [rfReplaceAll]), '''')
              {$ELSE}
                Result := 'N'+AnsiQuotedStr(StringReplace(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings), #0, '', [rfReplaceAll]), '''')
              else
                Result := AnsiQuotedStr(StringReplace(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings), #0, '', [rfReplaceAll]), '''')
              {$ENDIF}
          end
          else
            Result := 'NULL';
          TempBlob := nil;
        end;
      else
        Result := 'NULL';
    end;
  end;
end;

end.
