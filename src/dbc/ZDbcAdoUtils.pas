{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Specific Utilities                  }
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

unit ZDbcAdoUtils;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  Types,
  ZDbcIntfs, ZCompatibility, ZPlainAdo, ZDbcAdo, ZVariant, ZOleDB, ZDbcOleDBUtils,
  ZDbcStatement;

type
  PDirectionTypes = ^TDirectionTypes;
  TDirectionTypes = array of TOleEnum;

const ZProcedureColumnType2AdoType: array[TZProcedureColumnType] of  TOleEnum =
  (adParamUnknown{pctUnknown}, adParamInput{pctIn}, adParamInputOutput{pctInOut},
    adParamOutput{pctOut}, adParamReturnValue{pctReturn}, adParamReturnValue{pctResultSet});

{**
  Converts an ADO native types into string related.
  @param FieldType dblibc native field type.
  @return a string data type name.
}
function ConvertAdoToTypeName(FieldType: SmallInt): string;

{**
  Converts a Ado native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertAdoToSqlType(const FieldType, Precision, Scale: SmallInt;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Converts a Zeos type into ADO types.
  @param FieldType zeos field type.
  @return a ADO datatype.
}
function ConvertSqlTypeToAdo(FieldType: TZSQLType): Integer;

{**
  Converts a Variant type into ADO types.
  @param VT Variant datatype.
  @return a ADO datatype.
}
function ConvertVariantToAdo(VT: TVarType): Integer;

{**
  Converts a TZResultSetType type into ADO cursor type.
  @param ResultSetType.
  @return a ADO cursor type.
}
function ConvertResultSetTypeToAdo(ResultSetType: TZResultSetType): Integer;

{**
  Converts a TZResultSetConcurrency type into ADO lock type.
  @param ResultSetConcurrency.
  @return a ADO lock type.
}
function ConvertResultSetConcurrencyToAdo(ResultSetConcurrency: TZResultSetConcurrency): Integer;

{**
  Converts a OLEDB schema guid into ADO schema ID usable with OpenSchema.
  @param OleDBSchema schema guid.
  @return a ADO schema id.
}
function ConvertOleDBToAdoSchema(OleDBSchema: TGUID): Integer;

function GetCurrentResultSet(const AdoRecordSet: ZPlainAdo.RecordSet;
  const Connection: IZAdoConnection; const Statement: IZStatement; Const SQL: String;
  ConSettings: PZConSettings;
  const ResultSetConcurrency: TZResultSetConcurrency): IZResultSet;

function IsSelect(const SQL: string): Boolean;

{**
  Sets a variant value into specified parameter.
  @param AdoCommand the ole command
  @param Connection the Connection interface
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure ADOSetInParam(const AdoCommand: ZPlainAdo.Command; const Connection: IZConnection;
  ParamCount: Integer; const ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value: TZVariant;
  const ParamDirection: ParameterDirectionEnum);

function ADOBindArrayParams(const AdoCommand: ZPlainAdo.Command; const Connection: IZConnection;
  ConSettings: PZConSettings; const InParamValues: TZVariantDynArray;
  ParamDirection: ParameterDirectionEnum{note: should be an array later!!};
  ArrayCount: Integer): Integer;

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;

procedure SetOleCommandProperties(const Command: ICommandText; TimeOut: SmallInt;
  Provider: TZServerProvider; SupportsMARSConnection, Prepare: Boolean);

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; const ClientVarManager: IZClientVariantManager;
  SupportsMilliSeconds: Boolean = True);

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray;
  const SupportsMilliseconds: Boolean = True);

procedure RefreshParameters(const AdoCommand: ZPlainAdo.Command; DirectionTypes: PDirectionTypes = nil);

var
{**
  Required to free memory allocated by oledb
}
  ZAdoMalloc: IMalloc;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF}, Variants, Math,
  ZSysUtils, ZDbcAdoResultSet, ZDbcCachedResultSet, ZDbcResultSet, ZDbcUtils,
  ZMessages, ZEncoding, ZFastCode, ZClasses;

{**
  Converts an ADO native types into string related.
  @param FieldType dblibc native field type.
  @return a string data type name.
}
function ConvertAdoToTypeName(FieldType: SmallInt): string;
begin
  case FieldType of
    adChar             : Result := 'Char';
    adVarChar          : Result := 'VarChar';
    adBSTR             : Result := 'BSTR';
    adWChar            : Result := 'WChar';
    adVarWChar         : Result := 'VarWChar';
    adBoolean          : Result := 'Boolean';
    adTinyInt          : Result := 'TinyInt';
    adUnsignedTinyInt  : Result := 'UnsignedTinyInt';
    adSmallInt         : Result := 'SmallInt';
    adUnsignedSmallInt : Result := 'UnsignedSmallInt';
    adInteger          : Result := 'Integer';
    adUnsignedInt      : Result := 'UnsignedInt';
    adBigInt           : Result := 'BigInt';
    adUnsignedBigInt   : Result := 'UnsignedBigInt';
    adSingle           : Result := 'Single';
    adDouble           : Result := 'Double';
    adDecimal          : Result := 'Decimal';
    adNumeric          : Result := 'Numeric';
    adVarNumeric       : Result := 'VarNumeric';
    adCurrency         : Result := 'Currency';
    adDBDate           : Result := 'DBDate';
    adDBTime           : Result := 'DBTime';
    adDate             : Result := 'Date';
    adDBTimeStamp      : Result := 'DBTimeStamp';
    adFileTime         : Result := 'FileTime';
    adLongVarChar      : Result := 'LongVarChar';
    adLongVarWChar     : Result := 'LongVarWChar';
    adBinary           : Result := 'Binary';
    adVarBinary        : Result := 'VarBinary';
    adLongVarBinary    : Result := 'LongVarBinary';
    adGUID             : Result := 'GUID';
    adEmpty            : Result := 'Empty';
    adError            : Result := 'Error';
    adArray            : Result := 'Array';
    adChapter          : Result := 'Chapter';
    adIDispatch        : Result := 'IDispatch';
    adIUnknown         : Result := 'IUnknown';
    adPropVariant      : Result := 'PropVariant';
    adUserDefined      : Result := 'UserDefined';
    adVariant          : Result := 'Variant';
  else
    Result := 'Unknown';
  end;
end;

{**
  Converts a Ado native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertAdoToSqlType(const FieldType, Precision, Scale: SmallInt;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  //http://msdn.microsoft.com/en-us/library/windows/desktop/ms675318%28v=vs.85%29.aspx
  case FieldType of
    adChar, adVarChar,
    adWChar, adVarWChar, adBSTR: Result := stString;
    adBoolean: Result := stBoolean;
    adTinyInt: Result := stShort;
    adUnsignedTinyInt: Result := stByte;
    adSmallInt: Result := stSmall;
    adUnsignedSmallInt: Result := stWord;
    adInteger, adError{Indicates a 32-bit error code}: Result := stInteger;
    adUnsignedInt: Result := stLongWord;
    adBigInt: Result := stLong;
    adUnsignedBigInt: Result := stULong;
    adSingle: Result := stFloat;
    adDouble: Result := stDouble;
    adDecimal, adNumeric, adVarNumeric:
        if (Scale >= 0) and (Scale <= 4) and (Precision > 0) and (Precision < sAlignCurrencyScale2Precision[Scale])
        then Result := stCurrency
        else Result := stBigDecimal;
    adCurrency: Result := stCurrency;
    adDBDate: Result := stDate;
    adDBTime: Result := stTime;
    adDate : Result := stDate;
    adDBTimeStamp, adFileTime: Result := stTimestamp;
    adLongVarChar: Result := stAsciiStream;
    adLongVarWChar: Result := stAsciiStream;
    adBinary, adVarBinary: Result := stBytes;
    adLongVarBinary: Result := stBinaryStream;
    adGUID: Result := stGUID;
    adEmpty, AdArray, adChapter,
    adPropVariant, adUserDefined: Result := stString;
    adVariant: Result := stString;
  else
    {adIDispatch, adIUnknown: reserved, nut used tpyes}Result := stUnknown
  end;
  if CtrlsCPType = cCP_UTF16 then
    case Result of
      stString: Result := stUnicodeString;
      stAsciiStream: Result := stUnicodeStream;
    end;
end;

{**
  Converts a Zeos type into ADO types.
  @param FieldType zeos field type.
  @return a ADO datatype.
}
function ConvertSqlTypeToAdo(FieldType: TZSQLType): Integer;
begin
  case FieldType of
    stString, stUnicodeString: Result := adVarWChar;
    stBoolean: Result := adBoolean;
    stByte: Result := adUnsignedTinyInt;
    stShort: Result := adTinyInt;
    stWord: Result := adUnsignedSmallInt;
    stSmall: Result := adSmallInt;
    stLongWord: Result := adUnsignedInt;
    stInteger: Result := adInteger;
    stULong: Result := adUnsignedBigInt;
    stLong: Result := adBigInt;
    stCurrency: Result := adCurrency;
    stFloat: Result := adSingle;
    stDouble, stBigDecimal: Result := adDouble;
    stDate, stTime, stTimestamp: Result := adDate;
    stBytes: Result := adVarBinary;
    stGUID: Result := adGUID;
    stAsciiStream, stUnicodeStream: Result := adLongVarWChar;
    stBinaryStream: Result := adLongVarBinary;
  else
    Result := adEmpty;
  end;
end;

{**
  Converts a Variant type into ADO types.
  @param VT Variant datatype.
  @return a ADO datatype.
}
function ConvertVariantToAdo(VT: TVarType): Integer;
begin
  case VT and varTypeMask of
    varEmpty: Result := adEmpty;
    varNull: Result := adVarChar;
    varSmallint: Result := adSmallInt;
    varInteger: Result := adInteger;
    varSingle: Result := adSingle;
    varDouble: Result := adDouble;
    varCurrency: Result := adCurrency;
    varDate: Result := adDate;
    varOleStr: Result := adVarWChar;
    varDispatch: Result := adIDispatch;
    varError: Result := adError;
    varBoolean: Result := adBoolean;
    varVariant: Result := adVariant;
    varUnknown: Result := adIUnknown ;
{$IFNDEF FPC}
    varShortInt: Result := adTinyInt;
{$ENDIF}
    varByte: if (VT and varArray) <> 0 then Result := adLongVarBinary else Result := adUnsignedTinyInt;
{$IFNDEF FPC}
    varWord: Result := adUnsignedSmallInt;
    varLongWord: Result := adUnsignedInt;
    varInt64: Result := adBigInt;
{$ENDIF}
    varStrArg: Result := adWChar;
    varString: Result := adVarChar;
{$IFDEF UNICODE}
    varUString: Result := adVarChar;
{$ENDIF}
    varAny: Result := adEmpty;
  else
    Result := adEmpty;
  end;
end;


{**
  Converts a TZResultSetType type into ADO cursor type.
  @param ResultSetType.
  @return a ADO cursor type.
}
function ConvertResultSetTypeToAdo(ResultSetType: TZResultSetType): Integer;
begin
  case ResultSetType of
    rtForwardOnly: Result := adOpenForwardOnly;
    rtScrollInsensitive: Result := adOpenStatic;
    rtScrollSensitive: Result := adOpenDynamic;
  else
    Result := -1;//adOpenUnspecified;
  end
end;

{**
  Converts a TZResultSetConcurrency type into ADO lock type.
  @param ResultSetConcurrency.
  @return a ADO lock type.
}
function ConvertResultSetConcurrencyToAdo(ResultSetConcurrency: TZResultSetConcurrency): Integer;
begin
  case ResultSetConcurrency of
    rcReadOnly: Result := adLockReadOnly;
    rcUpdatable: Result := adLockOptimistic;
  else
    Result := -1;//adLockUnspecified;
  end
end;

{**
  Converts a OLEDB schema guid into ADO schema ID usable with OpenSchema.
  @param OleDBSchema schema guid.
  @return a ADO schema id.
}
function ConvertOleDBToAdoSchema(OleDBSchema: TGUID): Integer;
begin
  Result := -1;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_ASSERTIONS) then Result := 0;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CATALOGS) then Result := 1;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CHARACTER_SETS) then Result := 2;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLLATIONS) then Result := 3;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMNS) then Result := 4;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CHECK_CONSTRAINTS) then Result := 5;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CONSTRAINT_COLUMN_USAGE) then Result := 6;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CONSTRAINT_TABLE_USAGE) then Result := 7;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_KEY_COLUMN_USAGE) then Result := 8;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_REFERENTIAL_CONSTRAINTS) then Result := 9;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLE_CONSTRAINTS) then Result := 10;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMN_DOMAIN_USAGE) then Result := 11;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_INDEXES) then Result := 12;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMN_PRIVILEGES) then Result := 13;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLE_PRIVILEGES) then Result := 14;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_USAGE_PRIVILEGES) then Result := 15;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURES) then Result := 16;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_SCHEMATA) then Result := 17;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_SQL_LANGUAGES) then Result := 18;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_STATISTICS) then Result := 19;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLES) then Result := 20;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TRANSLATIONS) then Result := 21;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROVIDER_TYPES) then Result := 22;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEWS) then Result := 23;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEW_COLUMN_USAGE) then Result := 24;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEW_TABLE_USAGE) then Result := 25;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURE_PARAMETERS) then Result := 26;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_FOREIGN_KEYS) then Result := 27;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PRIMARY_KEYS) then Result := 28;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURE_COLUMNS) then Result := 29;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_CUBES) then Result := 32;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_DIMENSIONS) then Result := 33;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_HIERARCHIES) then Result := 34;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_LEVELS) then Result := 35;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_MEASURES) then Result := 36;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_PROPERTIES) then Result := 37;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_MEMBERS) then Result := 38;
  if IsEqualGuid(OleDBSchema, DBPROPSET_TRUSTEE) then Result := 39;
end;

function GetCurrentResultSet(const AdoRecordSet: ZPlainAdo.RecordSet;
  const Connection: IZAdoConnection; const Statement: IZStatement; Const SQL: String; ConSettings: PZConSettings;
  const ResultSetConcurrency: TZResultSetConcurrency): IZResultSet;
var
  NativeResultSet: IZResultSet;
begin
  Result := nil;
  if Assigned(AdoRecordset) then
    if (AdoRecordSet.State and adStateOpen) = adStateOpen then
    begin
      NativeResultSet := TZAdoResultSet.Create(Statement, SQL, AdoRecordSet);
      if ResultSetConcurrency = rcUpdatable then
        Result := TZCachedResultSet.Create(NativeResultSet, SQL,
          TZAdoCachedResolver.Create(Connection.GetAdoConnection,
          Statement, NativeResultSet.GetMetaData), ConSettings)
      else
        Result := NativeResultSet;
    end;
end;

function IsSelect(const SQL: string): Boolean;
begin
  Result := Uppercase(Copy(TrimLeft(Sql), 1, 6)) = 'SELECT';
end;

{**
  Sets a variant value into specified parameter.
  @param AdoCommand the ole command
  @param Connection the Connection interface
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure ADOSetInParam(const AdoCommand: ZPlainAdo.Command; const Connection: IZConnection;
  ParamCount: Integer; const ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value: TZVariant;
  const ParamDirection: ParameterDirectionEnum);
var
  S: Integer;
  B: IZBlob;
  V: OleVariant;
  T: Integer;
  P: ZPlainAdo.Parameter;
  RetValue: TZVariant;
  TmpSQLType: TZSQLType;
begin
  RetValue:= Value;
  TmpSQLType := SQLType;
  if not (RetValue.VType = vtNull) and (RetValue.VType = vtInterface) and
    (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
  begin
    B := SoftVarManager.GetAsInterface(Value) as IZBlob;
    if B.IsEmpty then
      RetValue := NullVariant
    else
      case SQLType of
        stAsciiStream, stUnicodeStream:
          if B.IsClob then
          begin
            SoftVarManager.SetAsUnicodeString(RetValue, B.GetUnicodeString);
            TmpSQLType := stUnicodeString;
          end
          else
          begin
            if SQLType = stAsciiStream then
            begin
              {$IFDEF UNICODE}
              SoftVarManager.SetAsString(RetValue, String(B.GetString));
              {$ELSE}
              SoftVarManager.SetAsString(RetValue, GetValidatedAnsiStringFromBuffer(B.GetBuffer, B.Length, Connection.GetConSettings));
              {$ENDIF}
              TmpSQLType := stString;
            end
            else
            begin
              B := TZAbstractClob.CreateWithStream(GetValidatedUnicodeStream(B.GetBuffer, B.Length, Connection.GetConSettings, False), 1200{zCP_UTF16}, Connection.GetConSettings);
              SoftVarManager.SetAsUnicodeString(RetValue, B.GetUnicodeString);
              TmpSQLType := stUnicodeString;
            end;
          end;
        stBinaryStream:
          begin
            if Assigned(B) then
              SoftVarManager.SetAsBytes(RetValue, B.GetBytes);
            TmpSQLType := stBytes;
          end;
      end;
  end;

  case RetValue.VType of
    vtNull: V := Null;
    vtBoolean: V := SoftVarManager.GetAsBoolean(RetValue);
    vtBytes: V := SoftVarManager.GetAsBytes(RetValue);
    vtInteger: //V := SoftVarManager.GetAsInteger(RetValue);
      if ParameterIndex <= ParamCount then
      begin //Hacking the IDE variant: Not all IDE's support
        P := AdoCommand.Parameters.Item[ParameterIndex - 1];
        P.Direction := ParamDirection;
        P.Type_ :=  adBigInt;
        P.Value := SoftVarManager.GetAsInteger(RetValue);
        Exit;
      end
      else
        AdoCommand.Parameters.Append(AdoCommand.CreateParameter(
          'P' + ZFastCode.IntToUnicode(ParameterIndex), adBigInt, ParamDirection,
            0, SoftVarManager.GetAsInteger(RetValue)));
    vtUInteger: //V := SoftVarManager.GetAsInteger(RetValue);
      if ParameterIndex <= ParamCount then
      begin //Hacking the IDE variant: Not all IDE's support
        P.Direction := ParamDirection;
        P.Type_ :=  adUnsignedBigInt;
        P := AdoCommand.Parameters.Item[ParameterIndex - 1];
        P.Value := SoftVarManager.GetAsUInteger(RetValue);
        Exit;
      end
      else
        AdoCommand.Parameters.Append(AdoCommand.CreateParameter(
          'P' + ZFastCode.IntToUnicode(ParameterIndex), adUnsignedBigInt,
            ParamDirection, 0, SoftVarManager.GetAsUInteger(RetValue)));
    {$IFDEF BCD_TEST}
    vtCurrency: V := SoftVarManager.GetAsCurrency(RetValue);
    vtDouble: V := SoftVarManager.GetAsDouble(RetValue);
    vtBigDecimal: V := SoftVarManager.GetAsDouble(RetValue);
    {$ELSE}
    vtFloat: V := SoftVarManager.GetAsFloat(RetValue);
    {$ENDIF}
    vtUnicodeString, vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtCharRec:
    begin
      RetValue.VUnicodeString := Connection.GetClientVariantManager.GetAsUnicodeString(RetValue);
      V := WideString(RetValue.VUnicodeString);
    end;
    vtDateTime: V := TDateTime(SoftVarManager.GetAsDateTime(RetValue));
  end;

  S := 0; //init val
  case TmpSQLType of
    stString, stUnicodeString:
      begin
        S := Length(RetValue.VUnicodeString) shl 1; //strange! Need size in bytes!!
        if S = 0 then S := 1;
        //V := Null; patch by zx - see http://zeos.firmos.at/viewtopic.php?t=1255
      end;
    stBytes:
      begin
        if (VarType(V) and varArray) <> 0 then
          S := VarArrayHighBound(V, 1) + 1;
        if S = 0 then V := Null;
      end;
  end;

  if VarIsNull(V) or (SQLType = stBytes) then
    T := ConvertSqlTypeToAdo(TmpSQLType)
  else
    T := ConvertVariantToAdo(VarType(V));

  if ParameterIndex <= ParamCount then
  begin
    P := AdoCommand.Parameters.Item[ParameterIndex - 1];
    P.Direction := ParamDirection; //set ParamDirection! Bidirection is requires for callables f.e.
    if not VarIsNull(V) then //align new size and type
    begin
      P.Type_ := T;
      P.Size := S;
    end;
    //if VarIsClear(P.Value) or (P.Value <> V) or (TmpSQLType = stBytes) then //Check if Param is cleared, unasigned or different
      P.Value := V;
  end
  else
    AdoCommand.Parameters.Append(AdoCommand.CreateParameter(
      'P' + ZFastCode.IntToUnicode(ParameterIndex), T, ParamDirection, S, V));
end;


function ADOBindArrayParams(const AdoCommand: ZPlainAdo.Command; const Connection: IZConnection;
  ConSettings: PZConSettings; const InParamValues: TZVariantDynArray;
  ParamDirection: ParameterDirectionEnum{note: should be an array later!!};
  ArrayCount: Integer): Integer;
var
  P: ZPlainAdo.Parameter;
  I: Integer;
  J: Cardinal;
  TempBlob: IZBlob;
  UniTemp: WideString;
  IsNull: Boolean;
  RC: OleVariant;
  SQLType: TZSQLType;

  { array DML bindings }
  ZData: Pointer; //array entry
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;
label ProcString;

  function IsNullFromIndicator: Boolean;
  begin
    case TZSQLType(InParamValues[I].VArray.VIsNullArrayType) of
      stBoolean: Result := ZBooleanArray[J];
      stByte: Result := ZByteArray[J] <> 0;
      stShort: Result := ZShortIntArray[J] <> 0;
      stWord: Result := ZWordArray[J] <> 0;
      stSmall: Result := ZSmallIntArray[J] <> 0;
      stLongWord: Result := ZLongWordArray[J] <> 0;
      stInteger: Result := ZIntegerArray[J] <> 0;
      stLong: Result := ZInt64Array[J] <> 0;
      stULong: Result := ZUInt64Array[J] <> 0;
      stFloat: Result := ZSingleArray[J] <> 0;
      stDouble: Result := ZDoubleArray[J] <> 0;
      stCurrency: Result := ZCurrencyArray[J] <> 0;
      stBigDecimal: Result := ZExtendedArray[J] <> 0;
      stGUID:
        Result := True;
      stString, stUnicodeString:
        begin
          case InParamValues[i].VArray.VIsNullArrayVariantType of
            vtString: Result := StrToBoolEx(ZStringArray[j]);
            vtAnsiString: Result := StrToBoolEx(ZAnsiStringArray[j]);
            vtUTF8String: Result := StrToBoolEx(ZUTF8StringArray[j]);
            vtRawByteString: Result := StrToBoolEx(ZRawByteStringArray[j]);
            vtUnicodeString: Result := StrToBoolEx(ZUnicodeStringArray[j]);
            vtCharRec:
              if ZCompatibleCodePages(ZCharRecArray[j].CP, zCP_UTF16) then
                Result := StrToBoolEx(PWideChar(ZCharRecArray[j].P))
              else
                Result := StrToBoolEx(PAnsiChar(ZCharRecArray[j].P));
            vtNull: Result := True;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        end;
      stBytes:
        Result := ZBytesArray[j] = nil;
      stDate, stTime, stTimestamp:
        Result := ZDateTimeArray[j] <> 0;
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        Result := ZInterfaceArray[j] = nil;
      else
        raise EZSQLException.Create(SUnsupportedParameterType);
    end;
  end;

begin
  {EH: slight cut down version with OleVatiants}
  Result := 0;
  for J := 0 to ArrayCount-1 do
  begin
    for i := 0 to High(InParamValues) do
    begin
      P := AdoCommand.Parameters.Item[i];
      P.Direction := ParamDirection;
      ZData := InParamValues[I].VArray.VIsNullArray;
      if (ZData = nil) then
        IsNull := True
      else
        IsNull := IsNullFromIndicator;

      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        P.Value := null
      else
      begin
        SQLType := TZSQLType(InParamValues[I].VArray.VArrayType);
        P.Type_ := ConvertSQLTypeToADO(SQLType);
        case SQLType of
          stBoolean:    P.Value := ZBooleanArray[J];
          stByte:       P.Value := ZByteArray[J];
          stShort:      P.Value := ZShortIntArray[J];
          stWord:       P.Value := ZWordArray[J];
          stSmall:      P.Value := ZSmallIntArray[J];
          stLongWord:   P.Value := ZLongWordArray[J];
          stInteger:    P.Value := ZIntegerArray[J];
          stLong:       P.Value := ZInt64Array[J];
          stULong:      P.Value := ZUInt64Array[J];
          stFloat:      P.Value := ZSingleArray[J];
          stDouble:     P.Value := ZDoubleArray[J];
          stCurrency:   P.Value := ZCurrencyArray[J];
          stBigDecimal: P.Value := ZExtendedArray[J];
          stGUID:
            begin
              P.Type_ := adGUID;
              P.Size := 38;
              P.Value := GUIDToUnicode(ZGUIDArray[j]);
            end;
          stString, stUnicodeString:
            begin
              case InParamValues[i].VArray.VArrayVariantType of
                vtString:
                    {$IFDEF UNICODE}
                    UniTemp := ZStringArray[j];
                    {$ELSE}
                    UniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[j], ConSettings^.CTRL_CP);
                    {$ENDIF}
                vtAnsiString: UniTemp := ZWideString(ZAnsiStringArray[j]);
                vtUTF8String: UniTemp := PRawToUnicode(Pointer(ZUTF8StringArray[j]), Length(ZUTF8StringArray[j]), zCP_UTF8);
                vtRawByteString: UniTemp := ConSettings^.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[j], ConSettings^.CTRL_CP);
                vtUnicodeString: UniTemp := ZUnicodeStringArray[j];
                vtCharRec:
                  if ZCompatibleCodePages(ZCharRecArray[j].CP, zCP_UTF16) then
                    SetString(UniTemp, PWideChar(ZCharRecArray[j].P), ZCharRecArray[j].Len)
                  else
                    UniTemp := PRawToUnicode(ZCharRecArray[j].P, ZCharRecArray[j].Len, ZCharRecArray[j].CP);
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
              P.Precision := Max(P.Precision, Length(UniTemp));
              P.Size := Max(1, Length(UniTemp) shl 1);
              P.Value := UniTemp;
            end;
          stBytes:
            begin
              P.Size := Length(ZBytesArray[j]);
              P.Value := ZBytesArray[j];
            end;
          stDate, stTime, stTimestamp: P.Value := ZDateTimeArray[j];
          stAsciiStream,
          stUnicodeStream:
            begin
              TempBlob := ZInterfaceArray[j] as IZBlob;
              if TempBlob.IsEmpty then
                P.Value := null
              else
                if TempBlob.IsClob then
                begin
ProcString:         UniTemp := TempBlob.GetUnicodeString;
                  P.Size := Max(1, Length(UniTemp) shl 1);
                  P.Value := UniTemp;
                end
                else
                begin
                  TempBlob := TZAbstractClob.CreateWithStream(GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, Connection.GetConSettings, False), zCP_UTF16, Connection.GetConSettings);
                  goto ProcString;
                end;
            end;
          stBinaryStream:
            begin
              TempBlob := ZInterfaceArray[j] as IZBlob;
              if TempBlob.IsEmpty then
                P.Value := null
              else
              begin
                P.Size := TempBlob.Length;
                P.Value := TempBlob.GetBytes;
              end;
            end
          else
            raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
        end;
      end;
    end;
    if J < Cardinal(ArrayCount -1) then
    begin
      AdoCommand.Execute(RC, EmptyParam, adExecuteNoRecords); {left space for last execution}
      Result := Result + RC;
    end;
  end;
end;

procedure RefreshParameters(const AdoCommand: ZPlainAdo.Command;
  DirectionTypes: PDirectionTypes = nil);
  procedure RefreshFromOleDB;
  var
    I: Integer;
    ParamCount: NativeUInt;
    ParamInfo: PDBParamInfoArray;
    NamesBuffer: PPOleStr;
    Name: WideString;
    Parameter: _Parameter;
    Direction: ParameterDirectionEnum;
    OLEDBCommand: ICommand;
    OLEDBParameters: ICommandWithParameters;
    CommandPrepare: ICommandPrepare;
  begin
    OLEDBCommand := (AdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand;
    OLEDBCommand.QueryInterface(ICommandWithParameters, OLEDBParameters);
    OLEDBParameters.SetParameterInfo(0, nil, nil);
    if Assigned(OLEDBParameters) then
    begin
      ParamInfo := nil;
      NamesBuffer := nil;
      try
        OLEDBCommand.QueryInterface(ICommandPrepare, CommandPrepare);
        if Assigned(CommandPrepare) then CommandPrepare.Prepare(0);
        if OLEDBParameters.GetParameterInfo(ParamCount{%H-}, PDBPARAMINFO(ParamInfo), NamesBuffer) = S_OK then
          for I := 0 to ParamCount - 1 do
            with ParamInfo[I] do
            begin
              { When no default name, fabricate one like ADO does }
              if pwszName = nil then
                Name := 'Param' + ZFastCode.IntToUnicode(I+1) else { Do not localize }
                Name := pwszName;
              { ADO maps DBTYPE_BYTES to adVarBinary }
              if wType = DBTYPE_BYTES then wType := adVarBinary;
              { ADO maps DBTYPE_STR to adVarChar }
              if wType = DBTYPE_STR then wType := adVarChar;
              { ADO maps DBTYPE_WSTR to adVarWChar }
              if wType = DBTYPE_WSTR then wType := adVarWChar;
              Direction := dwFlags and $F;
              { Verify that the Direction is initialized }
              if Assigned(DirectionTypes) then
                Parameter := AdoCommand.CreateParameter(Name, wType, DirectionTypes^[i], ulParamSize, EmptyParam)
              else
              begin
                if Direction = adParamUnknown then Direction := adParamInput;
                Parameter := AdoCommand.CreateParameter(Name, wType, Direction, ulParamSize, EmptyParam);
              end;
              Parameter.Precision := bPrecision;
              Parameter.NumericScale := ParamInfo[I].bScale;
              Parameter.Attributes := dwFlags and $FFFFFFF0; { Mask out Input/Output flags }
            end;
      finally
        if Assigned(CommandPrepare) then CommandPrepare.Unprepare;
        if (ParamInfo <> nil) then ZAdoMalloc.Free(ParamInfo);
        if (NamesBuffer <> nil) then ZAdoMalloc.Free(NamesBuffer);
      end;
    end;
  end;

  procedure RefreshFromADO;
  var
    I: Integer;
    Parameter: _Parameter;
  begin
    with AdoCommand do
    try
      Parameters.Refresh;
      for I := 0 to Parameters.Count - 1 do
        with Parameters[I] do
        begin
        { We can't use the instance of the parameter in the ADO collection because
          it will be freed when the connection is closed even though we have a
          reference to it.  So instead we create our own and copy the settings }
          if Assigned(DirectionTypes) and (Length(DirectionTypes^) > I) then
            Parameter := CreateParameter(Name, Type_, DirectionTypes^[i], Size, EmptyParam)
          else
            Parameter := CreateParameter(Name, Type_, Direction, Size, EmptyParam);
          Parameter.Precision := Precision;
          Parameter.NumericScale := NumericScale;
          Parameter.Attributes := Attributes;
        end;
    except
      { do nothing }
    end;
  end;
begin
  if ( AdoCommand.CommandType = adCmdText ) then
    RefreshFromOLEDB else
    RefreshFromADO;
end;

procedure ProcessUnicode(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; CodePoints: Integer);
begin
  PLen^ := CodePoints*SizeOf(WideChar);
  if ByRef then
    if (Src = nil) or (CodePoints = 0) then
      PPointer(Data)^ := PEmptyUnicodeString
    else
      PPointer(Data)^ := Src
  else
  begin
    {set Reference Pointer first! see: PrepareOleDBBindings comment}
    PNativeUInt(Data)^ := Data+SizeOf(Pointer);
    if (Src = nil) or (CodePoints = 0) then
      PWideChar(Data + SizeOf(Pointer))^ := #0
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^+SizeOf(WideChar));
  end;
end;

procedure ProcessAnsi(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; Len: Integer);
begin
  PLen^ := Len*SizeOf(AnsiChar);
  if ByRef then
    if (Src = nil) or (Len = 0) then
      PPointer(Data)^ := PEmptyAnsiString
    else
      PPointer(Data)^ := Src
  else
  begin
    {set Reference Pointer first! see: PrepareOleDBBindings comment}
    PNativeUInt(Data)^ := Data +SizeOf(Pointer);
    if (Src = nil) or (Len = 0) then
      PAnsiChar(Data + SizeOf(Pointer))^ := #0
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^+SizeOf(AnsiChar));
  end;
end;

procedure ProcessBinary(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; Len: Cardinal);
begin
  PLen^ := Len;
  if ByRef then
    if (Src = nil) or (Len = 0) then
      PPointer(Data)^ := nil
    else
      PPointer(Data)^ := Src
  else
  begin
    PNativeUInt(Data)^ := Data+SizeOf(Pointer);
    if (Src = nil) or (Len = 0) then
      PPointer(Data + SizeOf(Pointer))^ := nil
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^);
  end;
end;

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; const ClientVarManager: IZClientVariantManager;
  SupportsMilliSeconds: Boolean = True);
var
  Year, MilliSecond: Word;
  I: Integer;
  TempBlob: IZBlob;
  TmpStream: TStream;
  GUID: TGUID;
  Data: NativeUInt;
  PLen: PDBLENGTH;
begin
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to High(InParamValues) do
  begin
    if (InParamValues[I].VType = vtNull)  then
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL
    else
    begin
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_OK;
      Data := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue;
      //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
      PLen := PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength);
      case DBBindingArray[i].wType of
        DBTYPE_NULL:      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
        DBTYPE_I2:        PSmallInt(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_I4:        PInteger(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        {$IFDEF BCD_TEST}
        DBTYPE_R4:        PSingle(Data)^ := ClientVarManager.GetAsDouble(InParamValues[i]);
        DBTYPE_R8:        PDouble(Data)^ := ClientVarManager.GetAsDouble(InParamValues[i]);
        DBTYPE_CY:        PCurrency(Data)^ := ClientVarManager.GetAsCurrency(InParamValues[i]);
        {$ELSE}
        DBTYPE_R4:        PSingle(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_R8:        PDouble(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_CY:        PCurrency(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        {$ENDIF}
        DBTYPE_DATE:      PDateTime(Data)^ := ClientVarManager.GetAsDateTime(InParamValues[i]);
        //DBTYPE_IDISPATCH	= 9;
        //DBTYPE_ERROR	= 10;
        DBTYPE_BOOL:      PWordBool(Data)^ := ClientVarManager.GetAsBoolean(InParamValues[i]);
        //DBTYPE_VARIANT	= 12;
        //DBTYPE_IUNKNOWN	= 13;
        DBTYPE_UI1:       PByte(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I1:        PShortInt(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI2:       PWord(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_UI4:       PLongWord(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I8:        PInt64(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI8:       PUInt64(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_GUID:// or DBTYPE_BYREF:
          if InParamValues[i].vType = vtBytes then
            ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16)
          else
            if InParamValues[i].vType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString] then
            begin
              GUID := StringToGUID(ClientVarManager.GetAsString(InParamValues[i]));
              ProcessBinary(Data, PLen, False, @GUID.D1, 16)
            end
            else
              raise EZSQLException.Create(IntToStr(Ord(InParamTypes[i]))+' '+SUnsupportedParameterType);
        DBTYPE_BYTES or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(Data, PLen, True, Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(Data, PLen, True,
                  Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes));
              end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                TempBlob.GetBuffer, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(InParamValues[i].vBytes))))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, NativeUInt(Length(InParamValues[i].vBytes))));
              end;
        DBTYPE_STR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //Ansi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, ConSettings);
                ProcessAnsi(Data, PLen, True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(Data, PLen, True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                    {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, TempBlob.Length));
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(
                  TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].VRawByteString),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VRawByteString),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
            end;
        DBTYPE_WSTR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //Unicode lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(Data, PLen, True, Pointer(InParamValues[i].VUnicodeString), Length(InParamValues[i].VUnicodeString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VUnicodeString),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(InParamValues[i].VUnicodeString)));
            end;
        DBTYPE_DBDATE:
          begin
            DecodeDate(ClientVarManager.GetAsDateTime(InParamValues[i]), Year,
              PDBDate(Data)^.month, PDBDate(Data)^.day);
            PDBDate(Data)^.year := Year;
          end;
        DBTYPE_DBTIME:
          DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
            PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second,
            MilliSecond);
        DBTYPE_DBTIME2:
          begin
            DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
              PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second,
              MilliSecond);
              PDBTIME2(Data)^.fraction := Millisecond * 1000000;
          end;
        DBTYPE_DBTIMESTAMP:
          begin
            DecodeDate(ClientVarManager.GetAsDateTime(InParamValues[i]), Year,
              PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
            PDBTimeStamp(Data)^.year := Year;
            DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
              PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute,
              PDBTimeStamp(Data)^.second, MilliSecond);
            if SupportsMilliSeconds then
              PDBTimeStamp(Data)^.fraction := MilliSecond*1000000
            else
              PDBTimeStamp(Data)^.fraction := 0;
          end;
       else
          raise EZSQLException.Create(ZFastCode.IntToStr(DBBindingArray[i].wType)+' '+SUnsupportedParameterType);
        //DBTYPE_UDT: ;
        //DBTYPE_HCHAPTER:;
        //DBTYPE_PROPVARIANT:;
        //DBTYPE_VARNUMERIC:;


      end;
    end;
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray;
  const SupportsMilliseconds: Boolean = True);
var
  I, TempLobOffSet: Integer;
  Year, MilliSecond: Word;
  J, BuffOffSet: DB_UPARAMS;
  TempBlob: IZBlob;
  UniTemp: ZWideString;
  AnsiTemp: AnsiString;
  DateTimeTemp: TDateTime;
  IsNull: Boolean;
  SQLType: TZSQLType;
  TmpStream: TStream;
  GUID: TGUID;

  { array DML bindings }
  ZData: Pointer; //array entry
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;

  Data: NativeUInt;
  PLen: PDBLENGTH;

  function IsNullFromIndicator: Boolean;
  begin
    case TZSQLType(InParamValues[I].VArray.VIsNullArrayType) of
      stBoolean: Result := ZBooleanArray[ArrayOffSet];
      stByte: Result := ZByteArray[ArrayOffSet] <> 0;
      stShort: Result := ZShortIntArray[ArrayOffSet] <> 0;
      stWord: Result := ZWordArray[ArrayOffSet] <> 0;
      stSmall: Result := ZSmallIntArray[ArrayOffSet] <> 0;
      stLongWord: Result := ZLongWordArray[ArrayOffSet] <> 0;
      stInteger: Result := ZIntegerArray[ArrayOffSet] <> 0;
      stLong: Result := ZInt64Array[ArrayOffSet] <> 0;
      stULong: Result := ZUInt64Array[ArrayOffSet] <> 0;
      stFloat: Result := ZSingleArray[ArrayOffSet] <> 0;
      stDouble: Result := ZDoubleArray[ArrayOffSet] <> 0;
      stCurrency: Result := ZCurrencyArray[ArrayOffSet] <> 0;
      stBigDecimal: Result := ZExtendedArray[ArrayOffSet] <> 0;
      stGUID:
        Result := True;
      stString, stUnicodeString:
        begin
          case InParamValues[i].VArray.VIsNullArrayVariantType of
            vtString: Result := StrToBoolEx(ZStringArray[ArrayOffSet]);
            vtAnsiString: Result := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
            vtUTF8String: Result := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
            vtRawByteString: Result := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
            vtUnicodeString: Result := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
            vtCharRec:
              if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                Result := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
              else
                Result := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
            vtNull: Result := True;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        end;
      stBytes:
        Result := ZBytesArray[ArrayOffSet] = nil;
      stDate, stTime, stTimestamp:
        Result := ZDateTimeArray[ArrayOffSet] <> 0;
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        Result := ZInterfaceArray[ArrayOffSet] = nil;
      else
        raise EZSQLException.Create(SUnsupportedParameterType);
    end;
  end;
begin
  BuffOffSet := 0;
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for J := 0 to DBParams.cParamSets-1 do
  begin
    TempLobOffSet := 0;
    for i := 0 to High(InParamValues) do
    begin
      ZData := InParamValues[I].VArray.VIsNullArray;
      if (ZData = nil) then
        IsNull := False
      else
        IsNull := IsNullFromIndicator;
      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL
      else
      begin
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
        SQLType := TZSQLType(InParamValues[I].VArray.VArrayType);
        Data := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet);
        //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
        PLen := PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet));
        case DBBindingArray[i].wType of
          DBTYPE_NULL:  PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL; //Shouldn't happen
          DBTYPE_I2:
            case SQLType of
              stBoolean:    PSmallInt(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSmallInt(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PSmallInt(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSmallInt(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PSmallInt(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSmallInt(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSmallInt(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSmallInt(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PSmallInt(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSmallInt(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PSmallInt(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PSmallInt(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PSmallInt(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PSmallInt(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PSmallInt(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PSmallInt(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PSmallInt(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PSmallInt(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSmallInt(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PSmallInt(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSmallInt(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I4:
            case SQLType of
              stBoolean:    PInteger(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInteger(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PInteger(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInteger(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PInteger(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInteger(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInteger(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInteger(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PInteger(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInteger(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInteger(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInteger(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInteger(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInteger(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInteger(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInteger(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInteger(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInteger(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInteger(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInteger(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInteger(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R4:
            case SQLType of
              stBoolean:    PSingle(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSingle(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PSingle(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSingle(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PSingle(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSingle(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSingle(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSingle(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PSingle(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSingle(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PSingle(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PSingle(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PSingle(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PSingle(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PSingle(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PSingle(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PSingle(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSingle(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R8:
            case SQLType of
              stBoolean:    PDouble(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDouble(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PDouble(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDouble(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PDouble(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDouble(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDouble(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDouble(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PDouble(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDouble(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDouble(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDouble(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDouble(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PDouble(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PDouble(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PDouble(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PDouble(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDouble(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_CY:
            case SQLType of
              stBoolean:    PCurrency(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PCurrency(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PCurrency(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PCurrency(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PCurrency(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PCurrency(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PCurrency(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PCurrency(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PCurrency(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PCurrency(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PCurrency(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PCurrency(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PCurrency(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PCurrency(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PCurrency(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PCurrency(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PCurrency(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PCurrency(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_DATE:
            case SQLType of
              stBoolean:    PDateTime(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDateTime(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PDateTime(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDateTime(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PDateTime(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDateTime(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDateTime(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDateTime(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PDateTime(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDateTime(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDateTime(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDateTime(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDateTime(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                  vtAnsiString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                  vtUTF8String:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                  vtRawByteString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                  vtUnicodeString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDateTime(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_BOOL:
            case SQLType of
              stBoolean:    PWordBool(Data)^ := ZBooleanArray[ArrayOffSet];
              stByte:       PWordBool(Data)^ := ZByteArray[ArrayOffSet] <> 0;
              stShort:      PWordBool(Data)^ := ZShortIntArray[ArrayOffSet] <> 0;
              stWord:       PWordBool(Data)^ := ZWordArray[ArrayOffSet] <> 0;
              stSmall:      PWordBool(Data)^ := ZSmallIntArray[ArrayOffSet] <> 0;
              stLongWord:   PWordBool(Data)^ := ZLongWordArray[ArrayOffSet] <> 0;
              stInteger:    PWordBool(Data)^ := ZIntegerArray[ArrayOffSet] <> 0;
              stLong:       PWordBool(Data)^ := ZInt64Array[ArrayOffSet] <> 0;
              stULong:      PWordBool(Data)^ := ZUInt64Array[ArrayOffSet] <> 0;
              stFloat:      PWordBool(Data)^ := ZSingleArray[ArrayOffSet] <> 0;
              stDouble:     PWordBool(Data)^ := ZDoubleArray[ArrayOffSet] <> 0;
              stCurrency:   PWordBool(Data)^ := ZCurrencyArray[ArrayOffSet] <> 0;
              stBigDecimal: PWordBool(Data)^ := ZExtendedArray[ArrayOffSet] <> 0;
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PWordBool(Data)^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ELSE}
                  vtString:         PWordBool(Data)^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ENDIF}
                  vtAnsiString:     PWordBool(Data)^ := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
                  vtUTF8String:     PWordBool(Data)^ := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
                  vtRawByteString:  PWordBool(Data)^ := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
                  vtUnicodeString:  PWordBool(Data)^ := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWordBool(Data)^ := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
                    else
                      PWordBool(Data)^ := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWordBool(Data)^ := ZDateTimeArray[ArrayOffSet] <> 0;
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI1:
            case SQLType of
              stBoolean:    PByte(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PByte(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PByte(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PByte(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PByte(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PByte(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PByte(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PByte(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PByte(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PByte(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PByte(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PByte(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PByte(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PByte(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PByte(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PByte(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PByte(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PByte(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PByte(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PByte(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PByte(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI2:
            case SQLType of
              stBoolean:    PWord(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PWord(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PWord(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PWord(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PWord(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PWord(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PWord(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PWord(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PWord(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PWord(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PWord(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PWord(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PWord(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PWord(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PWord(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PWord(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PWord(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PWord(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWord(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PWord(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWord(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI4:
            case SQLType of
              stBoolean:    PLongWord(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PLongWord(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PLongWord(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PLongWord(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PLongWord(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PLongWord(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PLongWord(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PLongWord(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PLongWord(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PLongWord(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PLongWord(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PLongWord(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PLongWord(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PLongWord(Data)^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PLongWord(Data)^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PLongWord(Data)^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PLongWord(Data)^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PLongWord(Data)^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PLongWord(Data)^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PLongWord(Data)^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PLongWord(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I8:
            case SQLType of
              stBoolean:    PInt64(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInt64(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PInt64(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInt64(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PInt64(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInt64(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInt64(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInt64(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PInt64(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInt64(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInt64(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInt64(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInt64(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInt64(Data)^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInt64(Data)^ := RawToInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInt64(Data)^ := RawToInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInt64(Data)^ := RawToInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInt64(Data)^ := UnicodeToInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInt64(Data)^ := UnicodeToInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInt64(Data)^ := RawToInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInt64(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI8:
            case SQLType of
              stBoolean:    PUInt64(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PUInt64(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PUInt64(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PUInt64(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PUInt64(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PUInt64(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PUInt64(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PUInt64(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PUInt64(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PUInt64(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PUInt64(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PUInt64(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PUInt64(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PUInt64(Data)^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PUInt64(Data)^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PUInt64(Data)^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PUInt64(Data)^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PUInt64(Data)^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PUInt64(Data)^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PUInt64(Data)^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PUInt64(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_GUID or DBTYPE_BYREF: //GUID
            case SQLType of
              stGUID:
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), @ZGUIDArray[ArrayOffSet].D1, 16);
              stBytes:
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16);
              stString, stUnicodeString:
                begin
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: GUID := StringToGUID(ZStringArray[ArrayOffSet]);
                    vtAnsiString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet])));
                    vtUTF8String:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet])));
                    vtRawByteString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet])));
                    vtUnicodeString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet])));
                    vtCharRec:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeCharRec(ZCharRecArray[ArrayOffSet])));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                  ProcessBinary(Data, PLen, False, @GUID.D1, 16)
                end;
              else
                raise EZSQLException.Create('Unsupported GUID Variant');
            end;
          DBTYPE_BYTES or DBTYPE_BYREF:
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
                  end;
                stBytes:
                  ProcessBinary(Data, PLen, True, Pointer(ZBytesArray[ArrayOffSet]), Length(ZBytesArray[ArrayOffSet]));
                stGUID:
                  ProcessBinary(Data, PLen, True, @ZGUIDArray[ArrayOffSet].D1, 16);
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end
            else
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetBuffer, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                  end;
                stBytes:
                  ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    Pointer(ZBytesArray[ArrayOffSet]),
                    {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(ZBytesArray[ArrayOffSet]))));
                stGUID:
                  ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    @ZGUIDArray[ArrayOffSet].D1, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, 16));
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end;
          DBTYPE_STR or DBTYPE_BYREF: //just prepared case!
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            begin
              case SQLType of
                stBoolean:      AnsiTemp := BoolToRawEx(ZBooleanArray[ArrayOffSet]);
                stByte:         AnsiTemp := IntToRaw(ZByteArray[ArrayOffSet]);
                stShort:        AnsiTemp := IntToRaw(ZShortIntArray[ArrayOffSet]);
                stWord:         AnsiTemp := IntToRaw(ZWordArray[ArrayOffSet]);
                stSmall:        AnsiTemp := IntToRaw(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     AnsiTemp := IntToRaw(ZLongWordArray[ArrayOffSet]);
                stInteger:      AnsiTemp := IntToRaw(ZIntegerArray[ArrayOffSet]);
                stULong:        AnsiTemp := IntToRaw(ZUInt64Array[ArrayOffSet]);
                stLong:         AnsiTemp := IntToRaw(ZInt64Array[ArrayOffSet]);
                stFloat:        AnsiTemp := FloatToRaw(ZSingleArray[ArrayOffSet]);
                stDouble:       AnsiTemp := FloatToRaw(ZDoubleArray[ArrayOffSet]);
                stCurrency:     AnsiTemp := FloatToRaw(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   AnsiTemp := FloatToRaw(ZExtendedArray[ArrayOffSet]);
                stTime:         AnsiTemp := DateTimeToRawSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         AnsiTemp := DateTimeToRawSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    AnsiTemp := DateTimeToRawSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: AnsiTemp := ConSettings^.ConvFuncs.ZStringToAnsi(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                    vtAnsiString: AnsiTemp := ZAnsiStringArray[ArrayOffSet];
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString: AnsiTemp := ZRawByteStringArray[ArrayOffSet];
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, ZOSCodePage) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(Data)^ := ZCharRecArray[ArrayOffSet].P;
                        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
                        continue;
                      end
                      else
                        if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                          AnsiTemp := PUnicodeToRaw(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, GetACP)
                        else
                        begin
                          UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                          AnsiTemp := AnsiString(UniTemp);
                        end
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                    begin
                      TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //
                      ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PAnsiChar(AnsiTemp), Length(AnsiTemp), GetAcp, ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessAnsi(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
              Inc(TempLobOffSet);
            end
            else
            begin
              case SQLType of
                stBoolean:      AnsiTemp := BoolToRawEx(ZBooleanArray[ArrayOffSet]);
                stByte:         AnsiTemp := IntToRaw(ZByteArray[ArrayOffSet]);
                stShort:        AnsiTemp := IntToRaw(ZShortIntArray[ArrayOffSet]);
                stWord:         AnsiTemp := IntToRaw(ZWordArray[ArrayOffSet]);
                stSmall:        AnsiTemp := IntToRaw(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     AnsiTemp := IntToRaw(ZLongWordArray[ArrayOffSet]);
                stInteger:      AnsiTemp := IntToRaw(ZIntegerArray[ArrayOffSet]);
                stULong:        AnsiTemp := IntToRaw(ZUInt64Array[ArrayOffSet]);
                stLong:         AnsiTemp := IntToRaw(ZInt64Array[ArrayOffSet]);
                stFloat:        AnsiTemp := FloatToRaw(ZSingleArray[ArrayOffSet]);
                stDouble:       AnsiTemp := FloatToRaw(ZDoubleArray[ArrayOffSet]);
                stCurrency:     AnsiTemp := FloatToRaw(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   AnsiTemp := FloatToRaw(ZExtendedArray[ArrayOffSet]);
                stTime:         AnsiTemp := DateTimeToRawSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         AnsiTemp := DateTimeToRawSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    AnsiTemp := DateTimeToRawSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: AnsiTemp := ConSettings^.ConvFuncs.ZStringToAnsi(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                    vtAnsiString:
                      begin
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZAnsiStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(ZAnsiStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString:
                      begin
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZRawByteStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(ZRawByteStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, GetACP) then
                      begin
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, ZCharRecArray[ArrayOffSet].Len));
                        continue;
                      end
                      else
                        if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                          AnsiTemp := PUnicodeToRaw(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, GetACP)
                        else
                        begin
                          UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                          AnsiTemp := AnsiString(UniTemp);
                        end
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                    begin
                      TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //make internal conversion first
                      ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessAnsi(Data, PLen, False, Pointer(AnsiTemp), //converted values can't be referenced
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(AnsiTemp)));
            end;
          DBTYPE_WSTR or DBTYPE_BYREF:
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            begin
              case SQLType of
                stBoolean:      UniTemp := BoolToUnicodeEx(ZBooleanArray[ArrayOffSet]);
                stByte:         UniTemp := IntToUnicode(ZByteArray[ArrayOffSet]);
                stShort:        UniTemp := IntToUnicode(ZShortIntArray[ArrayOffSet]);
                stWord:         UniTemp := IntToUnicode(ZWordArray[ArrayOffSet]);
                stSmall:        UniTemp := IntToUnicode(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     UniTemp := IntToUnicode(ZLongWordArray[ArrayOffSet]);
                stInteger:      UniTemp := IntToUnicode(ZIntegerArray[ArrayOffSet]);
                stULong:        UniTemp := IntToUnicode(ZUInt64Array[ArrayOffSet]);
                stLong:         UniTemp := IntToUnicode(ZInt64Array[ArrayOffSet]);
                stFloat:        UniTemp := FloatToUnicode(ZSingleArray[ArrayOffSet]);
                stDouble:       UniTemp := FloatToUnicode(ZDoubleArray[ArrayOffSet]);
                stCurrency:     UniTemp := FloatToUnicode(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   UniTemp := FloatToUnicode(ZExtendedArray[ArrayOffSet]);
                stTime:         UniTemp := DateTimeToUnicodeSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         UniTemp := DateTimeToUnicodeSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    UniTemp := DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      {$IFDEF UNICODE}
                      UniTemp := ZStringArray[ArrayOffSet];
                      {$ELSE}
                      UniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                      {$ENDIF}
                    vtAnsiString: UniTemp := PRawToUnicode(Pointer(ZAnsiStringArray[ArrayOffSet]), Length(ZAnsiStringArray[ArrayOffSet]), GetACP);
                    vtUTF8String: UniTemp := PRawToUnicode(Pointer(ZUTF8StringArray[ArrayOffSet]), Length(ZUTF8StringArray[ArrayOffSet]), zCP_UTF8);
                    vtRawByteString: UniTemp := ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[ArrayOffSet], ConSettings^.ClientCodePage^.CP);
                    vtUnicodeString: UniTemp := ZUnicodeStringArray[ArrayOffSet];
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(Data)^ := ZCharRecArray[ArrayOffSet].P;
                        PLen^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
                        continue;
                      end
                      else
                        UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP)
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                      TempBlob.GetPWideChar //make conversion first
                    else
                    begin
                      TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                      TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                      InParamValues[i].vInterface := TempBlob; //keep mem alive!
                      TmpStream.Free;
                    end;
                    ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PWideChar(UniTemp), Length(UniTemp), ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              Inc(TempLobOffSet);
            end
            else
            begin
              case SQLType of
                stBoolean:      UniTemp := BoolToUnicodeEx(ZBooleanArray[ArrayOffSet]);
                stByte:         UniTemp := IntToUnicode(ZByteArray[ArrayOffSet]);
                stShort:        UniTemp := IntToUnicode(ZShortIntArray[ArrayOffSet]);
                stWord:         UniTemp := IntToUnicode(ZWordArray[ArrayOffSet]);
                stSmall:        UniTemp := IntToUnicode(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     UniTemp := IntToUnicode(ZLongWordArray[ArrayOffSet]);
                stInteger:      UniTemp := IntToUnicode(ZIntegerArray[ArrayOffSet]);
                stULong:        UniTemp := IntToUnicode(ZUInt64Array[ArrayOffSet]);
                stLong:         UniTemp := IntToUnicode(ZInt64Array[ArrayOffSet]);
                stFloat:        UniTemp := FloatToUnicode(ZSingleArray[ArrayOffSet]);
                stDouble:       UniTemp := FloatToUnicode(ZDoubleArray[ArrayOffSet]);
                stCurrency:     UniTemp := FloatToUnicode(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   UniTemp := FloatToUnicode(ZExtendedArray[ArrayOffSet]);
                stTime:         UniTemp := DateTimeToUnicodeSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         UniTemp := DateTimeToUnicodeSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    UniTemp := DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      {$IFDEF UNICODE}
                      begin
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZStringArray[ArrayOffSet])));
                        continue;
                      end;
                      {$ELSE}
                      UniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                      {$ENDIF}
                    vtAnsiString: UniTemp := PRawToUnicode(Pointer(ZAnsiStringArray[ArrayOffSet]), Length(ZAnsiStringArray[ArrayOffSet]), GetACP);
                    vtUTF8String: UniTemp := PRawToUnicode(Pointer(ZUTF8StringArray[ArrayOffSet]), Length(ZUTF8StringArray[ArrayOffSet]), zCP_UTF8);
                    vtRawByteString: UniTemp := ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[ArrayOffSet], ConSettings^.ClientCodePage^.CP);
                    vtUnicodeString:
                      begin
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZUnicodeStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZUnicodeStringArray[ArrayOffSet])));
                        continue;
                      end;
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, ZCharRecArray[ArrayOffSet].Len));
                        continue;
                      end
                      else
                        UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                      TempBlob.GetPWideChar //make internal conversion first
                    else
                    begin
                      TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                      TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                      InParamValues[i].vInterface := TempBlob; //keep mem alive!
                      TmpStream.Free;
                    end;
                    ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetPWideChar,
                      {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, TempBlob.Length shr 1));
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessUnicode(Data, PLen, False, Pointer(UniTemp),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(UniTemp)));
            end;
          DBTYPE_DBDATE:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeDate(DateTimeTemp, Year, PDBDate(Data)^.month, PDBDate(Data)^.day);
              PDBDate(Data)^.year := Year;
            end;
          DBTYPE_DBTIME:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeTime(DateTimeTemp, PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, MilliSecond);
            end;
          DBTYPE_DBTIME2:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeTime(DateTimeTemp,
                PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second,
                MilliSecond);
                PDBTIME2(Data)^.fraction := Millisecond * 1000000;
            end;
          DBTYPE_DBTIMESTAMP:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeDate(DateTimeTemp, Year, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
              PDBTimeStamp(Data)^.year := Year;
              DecodeTime(DateTimeTemp, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MilliSecond);
              if SupportsMilliseconds then
                PDBTimeStamp(Data)^.fraction := MilliSecond * 1000*1000
              else
                PDBTimeStamp(Data)^.fraction := 0;
            end;
          else
            raise EZSQLException.Create('OleType: '+ZFastCode.IntToStr(DBBindingArray[i].wType)+'/SQLType: '+ZFastCode.IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
          //DBTYPE_UDT: ;
          //DBTYPE_HCHAPTER:;
          //DBTYPE_PROPVARIANT:;
          //DBTYPE_VARNUMERIC:;
        end;
      end;
    end;
    Inc(ArrayOffSet);
    Inc(BuffOffSet, RowSize);
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
{$HINTS ON}

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;
var
  I: Integer;
  LobBufCount: Integer;
  Procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := ParamInfoArray^[Index].iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(ParamInfoArray^[Index].wType, ParamInfoArray^[Index].bPrecision, ParamInfoArray^[Index].bScale);
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //lob's
    begin
      { cbMaxLen returns max allowed bytes for Lob's which depends to server settings.
       So rowsize could have a overflow. In all cases we need to use references
       OR introduce DBTYPE_IUNKNOWN by using a IPersistStream/ISequentialStream/IStream see:
       http://msdn.microsoft.com/en-us/library/windows/desktop/ms709690%28v=vs.85%29.aspx }
      DBBindingArray[Index].cbMaxLen := SizeOf(Pointer);
      { now let's decide if we can use direct references or need space in buffer
        and a reference or if we need a external object for lob's}
      if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISOUTPUT <> 0) then
        raise Exception.Create('OUT/INOUT Parameter for LOB''s are currently not supported!');
      if not (InParamTypes[Index] in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
        Inc(LobBufCount);
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
    end
    else
    begin
      { all other types propably fit into one RowSize-Buffer }
      if DBBindingArray[Index].wType in [DBTYPE_GUID, DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR] then
        {for all these types we reserve a pointer and the buffer-memory, if we need it or not!
         this catches possible conversion later on. So we can either directly address or
         point to the buffer after the pointer where a converted value was moved in (:
         This may waste mem but makes everything flexible like a charm!}
      begin
         //all these types including GUID need a reference pointer except we do not play with multiple row binding
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        if DBBindingArray[Index].wType = DBTYPE_STR then
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+ParamInfoArray^[Index].ulParamSize +1
        else if DBBindingArray[Index].wType = DBTYPE_WSTR then
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+((ParamInfoArray^[Index].ulParamSize +1) shl 1)
        else
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+ParamInfoArray^[Index].ulParamSize;
          DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      end
      else
      begin { fixed types do not need a length indicator }
        DBBindingArray[Index].cbMaxLen := ParamInfoArray[Index].ulParamSize;
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength;
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_STATUS;
      end;
    end;
    DBBindingArray[Index].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    { let's check param directions and set IO modes}
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISINPUT <> 0) then //input found
      if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISOUTPUT <> 0) then //output found too
        DBBindingArray[Index].eParamIO := DBPARAMIO_INPUT or DBPARAMIO_OUTPUT
      else
        DBBindingArray[Index].eParamIO := DBPARAMIO_INPUT
    else
      DBBindingArray[Index].eParamIO := DBPARAMIO_OUTPUT;
    DBBindingArray[Index].dwFlags :=  ParamInfoArray^[Index].dwFlags; //set found flags to indicate long types too
    DBBindingArray[Index].bPrecision := ParamInfoArray^[Index].bPrecision;
    DBBindingArray[Index].bScale := ParamInfoArray^[Index].bScale;
  end;
begin
  LobBufCount := 0;
  SetLength(DBBindingArray, DBUPARAMS);

  DBBindingArray[0].obStatus := 0;
  SetDBBindingProps(0);
  for i := 1 to DBUPARAMS -1 do
  begin
    DBBindingArray[i].obStatus := DBBindingArray[i-1].obValue  + DBBindingArray[i-1].cbMaxLen;
    SetDBBindingProps(I);
  end;
  Result := DBBindingArray[DBUPARAMS -1].obValue + DBBindingArray[DBUPARAMS -1].cbMaxLen;
  SetLength(TempLobs, LobBufCount);
end;

procedure SetOleCommandProperties(const Command: ICommandText; TimeOut: SmallInt;
  Provider: TZServerProvider; SupportsMARSConnection: Boolean; Prepare: Boolean);
var
  FCmdProps: ICommandProperties;
  rgCommonProperties: array[0..20] of TDBProp;
  rgProviderProperties: TDBProp;
  rgPropertySets: array[0..1] of TDBPROPSET;

  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
begin
  FCmdProps := nil; //init
  if Succeeded(Command.QueryInterface(IID_ICommandProperties, FCmdProps)) then
  begin
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
    rgPropertySets[0].cProperties     := 0; //init
    rgPropertySets[0].guidPropertySet := DBPROPSET_ROWSET;
    rgPropertySets[0].rgProperties    := @rgCommonProperties[0];
    rgPropertySets[1].cProperties     := 0;
    case Provider of
      spMSSQL: rgPropertySets[1].guidPropertySet := DBPROPSET_SQLSERVERROWSET
      else rgPropertySets[1].guidPropertySet := DBPROPSET_ROWSET;
    end;
    rgPropertySets[1].rgProperties    := @rgProviderProperties;

    SetProp(rgPropertySets[0], DBPROP_COMMANDTIMEOUT,    Max(0, TimeOut)); //Set command time_out static!
    SetProp(rgPropertySets[0], DBPROP_SERVERCURSOR,      VARIANT_TRUE); //force a server side cursor
    if (Provider = spMSSQL) then
    begin
      //turn off deferred prepare -> raise exception on Prepare if command can't be executed!
      //http://msdn.microsoft.com/de-de/library/ms130779.aspx
      if Prepare then
        SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_FALSE)
      else
        SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_TRUE);
    end else begin
      //to avoid http://support.microsoft.com/kb/272358/de we need a
      //FAST_FORWARD(RO) server cursor
      {common sets which are NOT default: according the cursor models of
      http://msdn.microsoft.com/de-de/library/ms130840.aspx }
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,        VARIANT_FALSE);
      if SupportsMARSConnection then begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_FALSE);
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_FALSE);
      end else begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
      end;
      SetProp(rgPropertySets[0], DBPROP_OTHERINSERT,       VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_OTHERUPDATEDELETE, VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,         VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANFETCHBACKWARDS,  VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANSCROLLBACKWARDS, VARIANT_FALSE);
    end;
    try
      OleDBCheck(FCmdProps.SetProperties(2,@rgPropertySets[0]), '', nil);
    finally
      FCmdProps := nil;
    end;
  end;
end;

initialization
  OleCheck(CoGetMalloc(1, ZAdoMalloc));
finalization
  ZAdoMalloc := nil;
{$ENDIF ZEOS_DISABLE_ADO}
end.
