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

uses Windows, Classes, SysUtils, ActiveX,
  ZDbcIntfs, ZCompatibility, ZPlainAdo, ZDbcAdo, ZVariant, ZDbcStatement;

type
  PDirectionTypes = ^TDirectionTypes;
  TDirectionTypes = array of TOleEnum;

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
  ComObj, {$IFDEF FPC}PL_CT_OleDB{$ELSE}OleDB{$ENDIF}, Variants,
  ZSysUtils, ZDbcAdoResultSet, ZDbcCachedResultSet, ZDbcResultSet, ZDbcUtils;

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
  else
    {adIDispatch, adIUnknown, adVariant: reserved, nut used tpyes}Result := stUnknown
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
    stString: Result := adVarChar;
    stUnicodeString: Result := adVarWChar;
    stBoolean: Result := adBoolean;
    stByte: Result := adTinyInt;
    stSmall: Result := adSmallInt;
    stInteger: Result := adInteger;
    stLong: Result := adBigInt;
    stBigDecimal: Result := adDecimal;
    stFloat: Result := adSingle;
    stDouble: Result := adDouble;
    stDate: Result := adDBDate;
    stTime: Result := adDBTime;
    stTimestamp: Result := adDBTimeStamp;
    stBytes: Result := adVarBinary;
    stGUID: Result := adGUID;
    stAsciiStream: Result := adLongVarChar;
    stUnicodeStream: Result := adLongVarWChar;
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
      PWideChar(InitialString), IUnknown, DataSource);
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
  V: Variant;
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
    vtInteger: V := Integer(SoftVarManager.GetAsInteger(RetValue));
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
    if VarIsClear(P.Value) or (P.Value <> V) or (TmpSQLType = stBytes) then //Check if Param is cleared, unasigned or different
      P.Value := V;
  end
  else
    AdoCommand.Parameters.Append(AdoCommand.CreateParameter(
      'P' + IntToStr(ParameterIndex), T, ParamDirection, S, V));
end;

procedure RefreshParameters(AdoCommand: ZPlainAdo.Command;
  DirectionTypes: PDirectionTypes = nil);
  procedure RefreshFromOleDB;
  var
    I: Integer;
    ParamCount: NativeUInt;
    ParamInfo: PDBParamInfoArray;
    NamesBuffer: POleStr;
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
        if OLEDBParameters.GetParameterInfo(ParamCount, PDBPARAMINFO(ParamInfo), @NamesBuffer) = S_OK then
          for I := 0 to ParamCount - 1 do
            with ParamInfo[I] do
            begin
              { When no default name, fabricate one like ADO does }
              if pwszName = nil then
                Name := 'Param' + IntToStr(I+1) else { Do not localize }
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


