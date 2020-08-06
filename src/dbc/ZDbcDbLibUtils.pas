{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                DBLib Utility Functions                  }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses Classes, SysUtils,
  ZVariant, ZDbcIntfs, ZPlainDBLibDriver, ZCompatibility;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt; Precision, Scale: Integer): TZSQLType;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertTDSTypeToSqlType(FieldType: TTDSType; Precision, Scale: Integer): TZSQLType;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToTDSType(FieldType: TZSQLType): TTDSType;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses ZSysUtils, ZEncoding, ZDbcUtils, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt; Precision, Scale: Integer): TZSQLType;
begin
  case FieldType of
    1{char}, 12{varchar}, -8{nchar}, -9{nvarchar}: Result := stString;
    -7{bit}: Result := stBoolean;
//Bug #889223, bug with tinyint on mssql
    -6: Result := stByte;
    -5: Result := stLong;
//    -6: Result := stSmall;
    5: Result := stSmall;
    4: Result := stInteger;
    2{SQL_NUMERIC}, 3{SQL_DECIMAL}:
      if (Scale <= 4) and (Precision < sAlignCurrencyScale2Precision[Scale])
      then Result := stCurrency
      else Result := stBigDecimal;
    6, 7, 8: Result := stDouble;
    11, 93: Result := stTimestamp;
    -1{text}: Result := stAsciiStream;
    -10: Result{ntext} := stAsciiStream;
    -4{image}: Result := stBinaryStream;
    -2{binary},-3{varbinary}: Result := stBytes;
    -11{uniqueidentifier}: Result := stGUID;
  else
    Result := stUnknown;
  end;
end;

{**
  Converts a tabular data stream native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertTDSTypeToSqlType(FieldType: TTDSType; Precision, Scale: Integer): TZSQLType;
begin
  case FieldType of
    tdsVoid, tdsUDT:
      Result := stUnknown; //Null columns
    tdsImage:
      Result := stBinaryStream;
    tdsText: Result := stAsciiStream; //currently we have no national encoding..
    tdsNText, tdsMSXML: Result := stUnicodeStream;
    tdsUnique:
      Result := stGUID;
    tdsBinary, tdsVarBinary:
      Result := stBytes;
    tdsBigBinary, tdsBigVarBinary:
      Result := stBinaryStream;
    tdsIntN:
      Result := stInteger;
    tdsChar, tdsVarchar, tdsBigChar, tdsBigVarChar: Result := stString;
    tdsBigNChar, tdsNVarChar, tdsBigNVarChar: Result := stUnicodeString;
    tdsInt1:
      Result := stByte;
    tdsBit, tdsBitN:
      Result := stBoolean;
    tdsInt2:
      Result := stSmall;
    tdsInt4:
      Result := stInteger;
    tdsDateTime, tdsDateTimeN, tdsDateTime4:
      Result := stTimeStamp;
    tdsFlt4, tdsFltN:
      Result := stFloat;
    tdsMoney, tdsMoney4, tdsMoneyN:
      Result := stCurrency;
    tdsFlt8:
      Result := stDouble;
    tdsDecimal, tdsNumeric:
      if (Scale <= 4) and (Precision < sAlignCurrencyScale2Precision[Scale])
      then Result := stCurrency
      else Result := stBigDecimal;
    //tdsVariant: {from tds.h -> sybase only -> na't test it}
    tdsInt8:
      Result := stLong;
    else
      Result := stUnknown;
  end;
end;

{**
  Converts ZDBC SQL types into FreeTDS native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToTDSType(FieldType: TZSQLType): TTDSType;
begin
  case FieldType of
    stBoolean: Result := tdsBit;
    stByte, stShort: Result := tdsInt1;
    stWord, stSmall: Result := tdsInt2;
    stLongWord, stInteger: Result := tdsInt4;
    stLong, stUlong: Result := tdsInt8;
    stFloat: Result := tdsFlt4;
    stDouble, stBigDecimal: Result := tdsFlt8;
    stString, stUnicodeString: Result := tdsVarChar;
    stBytes: Result := tdsVarBinary;
    stGUID: Result := tdsUnique;
    stDate, stTime, stTimestamp: Result := tdsDateTime;
    stAsciiStream, stUnicodeStream: Result := tdsText;
    stBinaryStream: Result := tdsImage;
    else Result := tdsVoid;
  end;
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
