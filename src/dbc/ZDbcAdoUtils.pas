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
{$IFDEF ENABLE_ADO}

uses Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  Types,
  ZDbcIntfs, ZCompatibility, ZPlainAdo, ZDbcAdo, ZVariant, ZDbcStatement, ZOleDB;

type
  PDirectionTypes = ^TDirectionTypes;
  TDirectionTypes = array of TOleEnum;

  TInterfacesDynArray = array of TInterfaceDynArray;

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
function ConvertAdoToSqlType(const FieldType: SmallInt;
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

{**
  Brings up the ADO connection string builder dialog.
}
function PromptDataSource(Handle: THandle; InitialString: WideString): WideString;

function GetCurrentResultSet(AdoRecordSet: ZPlainAdo.RecordSet;
  Connection: IZAdoConnection; Statement: IZStatement; Const SQL: String;
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
procedure ADOSetInParam(AdoCommand: ZPlainAdo.Command; Connection: IZConnection;
  ParamCount: Integer; const ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value: TZVariant;
  const ParamDirection: ParameterDirectionEnum);

function ADOBindArrayParams(AdoCommand: ZPlainAdo.Command; Connection: IZConnection;
  ConSettings: PZConSettings; const InParamValues: TZVariantDynArray;
  ParamDirection: ParameterDirectionEnum{note: should be an array later!!};
  ArrayCount: Integer): Integer;

function PrepareOleDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; ClientVarManager: IZClientVariantManager);

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray);

procedure RefreshParameters(AdoCommand: ZPlainAdo.Command; DirectionTypes: PDirectionTypes = nil);

var
{**
  Required to free memory allocated by oledb
}
  ZAdoMalloc: IMalloc;

{$ENDIF ENABLE_ADO}
implementation
{$IFDEF ENABLE_ADO}

uses
  ComObj, Variants, Math,
  ZSysUtils, ZDbcAdoResultSet, ZDbcCachedResultSet, ZDbcResultSet, ZDbcUtils,
  ZMessages, ZEncoding, ZFastCode;

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
function ConvertAdoToSqlType(const FieldType: SmallInt;
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
    adDecimal: Result := stBigDecimal;
    adNumeric, adVarNumeric: Result := stBigDecimal;
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

{**
  Brings up the ADO connection string builder dialog.
}
function PromptDataSource(Handle: THandle; InitialString: WideString): WideString;
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
begin
  Result := InitialString;
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if InitialString <> '' then
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(InitialString), IUnknown, DataSource{%H-});
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;
  if Succeeded(DBPrompt.PromptDataSource(nil, Handle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource)) then
  begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    Result := InitStr;
  end;
end;

function GetCurrentResultSet(AdoRecordSet: ZPlainAdo.RecordSet;
  Connection: IZAdoConnection; Statement: IZStatement; Const SQL: String; ConSettings: PZConSettings;
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
procedure ADOSetInParam(AdoCommand: ZPlainAdo.Command; Connection: IZConnection;
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
    vtFloat: V := SoftVarManager.GetAsFloat(RetValue);
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


function ADOBindArrayParams(AdoCommand: ZPlainAdo.Command; Connection: IZConnection;
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
              P.Value := {$IFNDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(GUIDToString(ZGUIDArray[j]));
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

{$HINTS OFF}
procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; ClientVarManager: IZClientVariantManager);
var
  I: Integer;
  TempBlob: IZBlob;
  TmpStream: TStream;
  GUID: TGUID;

  procedure ProcessUnicode(ByRef: Boolean; Src: Pointer; CodePoints: Integer);
  begin
    if ByRef then
      if (Src = nil) or (CodePoints = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := PEmptyUnicodeString;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := (CodePoints) shl 1;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer);
      if (Src = nil) or (CodePoints = 0) then
      begin
        PWideChar(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer))^, (CodePoints+1) shl 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := (CodePoints) shl 1;
      end;
    end;
  end;
  procedure ProcessAnsi(ByRef: Boolean; Src: Pointer; Len: Integer);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := PEmptyAnsiString;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer);
      if (Src = nil) or (Len = 0) then
      begin
        PAnsiChar(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^, Len + 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end;
    end;
  end;
  procedure ProcessBinary(ByRef: Boolean; Src: Pointer; Len: Cardinal);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end
    else
    begin
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+SizeOf(Pointer);
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+ SizeOf(Pointer))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+ SizeOf(Pointer))^, Len);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end;
    end;
  end;
begin
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to High(InParamValues) do
  begin
    if (InParamValues[I].VType = vtNull)  then
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL
    else
    begin
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_OK;
      case DBBindingArray[i].wType of
        DBTYPE_NULL:      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
        DBTYPE_I2:        PSmallInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_I4:        PInteger(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_R4:        PSingle(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_R8:        PDouble(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_CY:        PCurrency(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_DATE:      PDateTime(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsDateTime(InParamValues[i]);
        //DBTYPE_IDISPATCH	= 9;
        //DBTYPE_ERROR	= 10;
        DBTYPE_BOOL:      PWordBool(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsBoolean(InParamValues[i]);
        //DBTYPE_VARIANT	= 12;
        //DBTYPE_IUNKNOWN	= 13;
        DBTYPE_UI1:       PByte(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I1:        PShortInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI2:       PWord(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_UI4:       PLongWord(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I8:        PInt64(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI8:       PUInt64(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_GUID or DBTYPE_BYREF:
          if InParamValues[i].vType = vtBytes then
            ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16)
          else
            if InParamValues[i].vType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString] then
            begin
              GUID := StringToGUID(ClientVarManager.GetAsString(InParamValues[i]));
              ProcessBinary(False, @GUID.D1, 16)
            end
            else
              raise EZSQLException.Create(IntToStr(Ord(InParamTypes[i]))+' '+SUnsupportedParameterType);
        DBTYPE_BYTES or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(True, TempBlob.GetBuffer, TempBlob.Length);
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(True, Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(True,
                  Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes));
              end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                TempBlob.GetBuffer, Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(InParamValues[i].vBytes))))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  Min(DBBindingArray[I].cbMaxLen, NativeUInt(Length(InParamValues[i].vBytes))));
              end;
        DBTYPE_STR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, ConSettings);
                ProcessAnsi(True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                    Min(DBBindingArray[I].cbMaxLen-1, TempBlob.Length));
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(
                  TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].VRawByteString),
                  Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VRawByteString),
                Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
            end;
        DBTYPE_WSTR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length  shr 1);
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(True, Pointer(InParamValues[i].VUnicodeString), Length(InParamValues[i].VUnicodeString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VUnicodeString),
                Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(InParamValues[i].VUnicodeString)));
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

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray);
var
  I, TempLobOffSet: Integer;
  J, BuffOffSet: DB_UPARAMS;
  TempBlob: IZBlob;
  UniTemp: ZWideString;
  AnsiTemp: AnsiString;
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

  procedure ProcessUnicode(ByRef: Boolean; Src: Pointer; CodePoints: Integer);
  begin
    if ByRef then
      if (Src = nil) or (CodePoints = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := PEmptyUnicodeString;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := (CodePoints) shl 1;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (CodePoints = 0) then
      begin
        PWideChar(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet + SizeOf(Pointer)))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer)))^, (CodePoints+1) shl 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := (CodePoints) shl 1;
      end;
    end;
  end;
  procedure ProcessAnsi(ByRef: Boolean; Src: Pointer; Len: Integer);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := PEmptyAnsiString;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (Len = 0) then
      begin
        PAnsiChar(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet + SizeOf(Pointer)))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer)))^, Len + 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end;
    end;
  end;
  procedure ProcessBinary(ByRef: Boolean; Src: Pointer; Len: Cardinal);
  begin
    if ByRef then // bind by ref is
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^, Len);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end;
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
        IsNull := True
      else
        IsNull := IsNullFromIndicator;
      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL
      else
      begin
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
        SQLType := TZSQLType(InParamValues[I].VArray.VArrayType);
        case DBBindingArray[i].wType of
          DBTYPE_NULL:  PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL; //Shouldn't happen
          DBTYPE_I2:
            case SQLType of
              stBoolean:    PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I4:
            case SQLType of
              stBoolean:    PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R4:
            case SQLType of
              stBoolean:    PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R8:
            case SQLType of
              stBoolean:    PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_CY:
            case SQLType of
              stBoolean:    PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_DATE:
            case SQLType of
              stBoolean:    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                  vtAnsiString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                  vtUTF8String:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                  vtRawByteString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                  vtUnicodeString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_BOOL:
            case SQLType of
              stBoolean:    PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZBooleanArray[ArrayOffSet];
              stByte:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet] <> 0;
              stShort:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet] <> 0;
              stWord:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet] <> 0;
              stSmall:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet] <> 0;
              stLongWord:   PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet] <> 0;
              stInteger:    PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet] <> 0;
              stLong:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet] <> 0;
              stULong:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet] <> 0;
              stFloat:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet] <> 0;
              stDouble:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet] <> 0;
              stCurrency:   PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet] <> 0;
              stBigDecimal: PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet] <> 0;
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ELSE}
                  vtString:         PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ENDIF}
                  vtAnsiString:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
                  vtUTF8String:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
                  vtRawByteString:  PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
                  vtUnicodeString:  PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
                    else
                      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet] <> 0;
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI1:
            case SQLType of
              stBoolean:    PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI2:
            case SQLType of
              stBoolean:    PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI4:
            case SQLType of
              stBoolean:    PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I8:
            case SQLType of
              stBoolean:    PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI8:
            case SQLType of
              stBoolean:    PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_GUID or DBTYPE_BYREF: //GUID
            case SQLType of
              stGUID:
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), @ZGUIDArray[ArrayOffSet].D1, 16);
              stBytes:
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16);
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
                  ProcessBinary(False, @GUID.D1, 16)
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
                    ProcessBinary(True, TempBlob.GetBuffer, TempBlob.Length);
                  end;
                stBytes:
                  ProcessBinary(True, Pointer(ZBytesArray[ArrayOffSet]), Length(ZBytesArray[ArrayOffSet]));
                stGUID:
                  ProcessBinary(True, @ZGUIDArray[ArrayOffSet].D1, 16);
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end
            else
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetBuffer, Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                  end;
                stBytes:
                  ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    Pointer(ZBytesArray[ArrayOffSet]),
                    Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(ZBytesArray[ArrayOffSet]))));
                stGUID:
                  ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    @ZGUIDArray[ArrayOffSet].D1, Min(DBBindingArray[I].cbMaxLen, 16));
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
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, ZDefaultSystemCodePage) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].P;
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
                      ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PAnsiChar(AnsiTemp), Length(AnsiTemp), GetAcp, ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessAnsi(True, TempBlob.GetBuffer, TempBlob.Length);
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
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZAnsiStringArray[ArrayOffSet]),
                          Min(DBBindingArray[I].cbMaxLen-1, Length(ZAnsiStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString:
                      begin
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZRawByteStringArray[ArrayOffSet]),
                          Min(DBBindingArray[I].cbMaxLen-1, Length(ZRawByteStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, GetACP) then
                      begin
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          Min(DBBindingArray[I].cbMaxLen-1, ZCharRecArray[ArrayOffSet].Len));
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
                      ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessAnsi(False, Pointer(AnsiTemp), //converted values can't be referenced
                Min(DBBindingArray[I].cbMaxLen-1, Length(AnsiTemp)));
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
                        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].P;
                        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
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
                    ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PWideChar(UniTemp), Length(UniTemp), ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
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
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZStringArray[ArrayOffSet]),
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZStringArray[ArrayOffSet])));
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
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZUnicodeStringArray[ArrayOffSet]),
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZUnicodeStringArray[ArrayOffSet])));
                        continue;
                      end;
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, ZCharRecArray[ArrayOffSet].Len));
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
                    ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetPWideChar,
                      Min((DBBindingArray[I].cbMaxLen shr 1) -1, TempBlob.Length shr 1));
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessUnicode(False, Pointer(UniTemp),
                Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(UniTemp)));
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
    Inc(ArrayOffSet);
    Inc(BuffOffSet, RowSize);
  end;
end;
{$HINTS ON}
function MapOleTypesToZeos(DBType: TOleEnum): DBTYPE;
begin
  //ole type mappings:
  //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
  { we only map types to Zeos simple types here}
  Result := DBType;
  case DBType of
    { all commented enums are types i've no idea about. All droped are supported as is }
    DBTYPE_BSTR: Result := DBTYPE_WSTR;
    //DBTYPE_IDISPATCH	= 9;
    //DBTYPE_ERROR: 	= 10;
    //DBTYPE_VARIANT	= 12;
    //DBTYPE_IUNKNOWN	= 13;
    DBTYPE_DECIMAL: Result := DBTYPE_R8;
    DBTYPE_STR: Result := DBTYPE_WSTR;  //if we would know the server-codepage ... we could decrease mem
    DBTYPE_NUMERIC: Result := DBTYPE_R8;
    //DBTYPE_UDT	= 132;
    DBTYPE_DBDATE: Result := DBTYPE_DATE;
    DBTYPE_DBTIME: Result := DBTYPE_DATE;
    DBTYPE_DBTIMESTAMP: Result := DBTYPE_DATE;
    //DBTYPE_HCHAPTER	= 136;
    DBTYPE_FILETIME: Result := DBTYPE_DATE;
    //DBTYPE_PROPVARIANT	= 138;
    DBTYPE_VARNUMERIC: Result := DBTYPE_R8;
  end;
end;

function PrepareOleDBBindings(DBUPARAMS: DB_UPARAMS;
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
    DBBindingArray[Index].wType := MapOleTypesToZeos(ParamInfoArray^[Index].wType);
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //lob's
    begin
      { cbMaxLen returns max allowed bytes for Lob's which depends to server settings.
       So rowsize could have a overflow. In all cases we need to use references
       OR introduce DBTYPE_IUNKNOWN by using a IPersistStream see:
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
        DBBindingArray[Index].cbMaxLen := ParamInfoArray^[Index].ulParamSize;
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
    DBBindingArray[Index].bPrecision := ParamInfoArray^[Index].bPrecision; //looks nice ... but do we need it?
    DBBindingArray[Index].bScale := ParamInfoArray^[Index].bScale; //looks nice ... but do we need it?
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

procedure RefreshParameters(AdoCommand: ZPlainAdo.Command;
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
          if Assigned(DirectionTypes) then
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

initialization
  OleCheck(CoGetMalloc(1, ZAdoMalloc));
finalization
  ZAdoMalloc := nil;

{$ENDIF ENABLE_ADO}
end.



