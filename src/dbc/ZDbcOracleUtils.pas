{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZDbcOracleUtils;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZVariant, ZPlainOracleDriver, ZDbcLogging,
  ZCompatibility, ZPlainOracleConstants;

const
  MAX_SQLVAR_LIMIT = 1024;

type
  {** Declares SQL Object }

  POCIObject = ^TOCIObject;
  TObjFields = array of POCIObject;
  TOCIObject = Record                 // embedded object or table will work recursively
    type_name:      String;           //object's name (TDO)
    type_schema:    String;           //object's schema name (TDO)
    parmdp:         POCIParam;        //Describe attributes of the object OCI_DTYPE_PARAM
    parmap:         POCIParam;        //Describe attributes of the object OCI_ATTR_COLLECTION_ELEMENT OCI_ATTR_PARAM
    tdo:            POCIType;         //object's TDO handle
    typecode:       OCITypeCode;      //object's OCI_ATTR_TYPECODE
    col_typecode:   OCITypeCode;      //if collection this is its OCI_ATTR_COLLECTION_TYPECODE
    elem_typecode:  OCITypeCode;      //if collection this is its element's OCI_ATTR_TYPECODE
    obj_ref:        POCIRef;          //if an embeded object this is ref handle to its TDO
    obj_ind:        POCIInd;          //Null indictator for object
    obj_value:      POCIComplexObject;//the actual value from the DB
    obj_type:       POCIType;         //if an embeded object this is the  OCIType returned by a OCIObjectPin
    is_final_type:  ub1;              //object's OCI_ATTR_IS_FINAL_TYPE
    fields:         TObjFields;       //one object for each field/property
    field_count:    ub2;              //The number of fields Not really needed but nice to have
    next_subtype:   POCIObject;       //There is strored information about subtypes for inteherited objects
    stmt_handle:    POCIStmt;         //the Statement-Handle
    Level:          Integer;          //the instance level
    Pinned:         Boolean;          //did we pin the obj on decribe?
  end;

  PZSQLVar = ^TZSQLVar;
  TZSQLVar = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    Handle:    POCIHandle;
    Define:    POCIHandle;
    BindHandle: POCIBind;
    Data:      Pointer;
    DupData:   Pointer;
    DataType:  ub2;
    DataSize:  ub2;
    Length:    Integer;
    Precision: Integer;
    Scale:     Integer;
    ColType:   TZSQLType;
    TypeCode:  ub2;
    Indicator: sb2;
    Blob:      IZBlob;
    _Obj:      POCIObject;
  end;

  TZSQLVars = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    AllocNum:  ub4;
    ActualNum: ub4;
    Variables: array[1..MAX_SQLVAR_LIMIT] of TZSQLVar;
  end;
  PZSQLVars = ^TZSQLVars;

  TZOracleParam = Record
    pName:string;
    pSQLType:Integer;
    pValue: TZVariant;
    pTypeName: String;
    pType: ShortInt;
    pProcIndex: Integer;
    pParamIndex: Integer;
    pOutIndex: Integer;
  End;
  TZOracleParams = array of TZOracleParam;

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Handle: POCIEnv; const ErrorHandle: POCIError;
  const ConSettings: PZConSettings);

{**
  Allocates in memory and initializes the Oracle variable.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variable an Oracle variable holder.
  @param DataType a DBC data type.
  @param OracleType a correspondent Oracle type.
  @param DataSize a length for string variables.
}
procedure InitializeOracleVar(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Variable: PZSQLVar;
  DataType: TZSQLType; OracleType: ub2; DataSize: Integer);

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; ErrorHandle: POCIError; Variables: PZSQLVars;
  Values: TZVariantDynArray; ChunkSize: Integer);

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(Variables: PZSQLVars);

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Converts Oracle internal date into TDateTime
  @param Value a pointer to Oracle internal date.
  @return a decoded TDateTime value.
}
function OraDateToDateTime(Value: PAnsiChar): TDateTime;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(PlainDriver: IZOraclePlainDriver;
  ErrorHandle: POCIError; Status: Integer; LogCategory: TZLoggingCategory;
  LogMessage: string);

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; LogSQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError): IZResultSet; overload;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; LogSQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError; OutVars: PZSQLVars;
  Const OracleParams: TZOracleParams): IZResultSet; overload;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError);

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(PlainDriver: IZOraclePlainDriver;
  SQL: RawByteString; LogSQL: String; Handle: POCIStmt; ErrorHandle: POCIError;
  PrefetchCount: ub4; ConSettings: PZConSettings);

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure ExecuteOracleStatement(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; LogSQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError);

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(PlainDriver: IZOraclePlainDriver;
  Handle: POCIStmt; ErrorHandle: POCIError): ub4;

function DescribeObject(PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  ParamHandle: POCIParam; stmt_handle: POCIHandle; Level: ub2): POCIObject;

implementation

uses ZMessages, ZDbcOracle, ZDbcOracleResultSet, ZDbcCachedResultSet,
  ZDbcGenericResolver, ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Calculates size of SQLVars record.
  @param Count a number of variable.
  @returns a record size.
}
function CalculateSQLVarsSize(Count: Integer): Integer;
begin
  Result := SizeOf(TZSQLVars) + Count * SizeOf(TZSQLVar);
end;

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);
var
  Size: Integer;
begin
  if Variables <> nil then
    FreeMem(Variables);

  Size := CalculateSQLVarsSize(Count);
  GetMem(Variables, Size);
  FillChar(Variables^, Size, 0);
  Variables^.AllocNum := Count;
  Variables^.ActualNum := 0;
end;

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Handle: POCIEnv; const ErrorHandle: POCIError;
  const ConSettings: PZConSettings);
var
  I: Integer;
  CurrentVar: PZSQLVar;

  procedure DisposeObject(var Obj: POCIObject);
  var
    I: Integer;
  begin
    for i := 0 to High(Obj.fields) do
      DisposeObject(Obj.fields[i]);
    SetLength(Obj.fields, 0);
    if Assigned(Obj.next_subtype) then
    begin
      DisposeObject(Obj.next_subtype);
      Obj.next_subtype := nil;
    end;
    if Obj.Pinned then
      {Unpin tdo}
      //CheckOracleError(PlainDriver, ErrorHandle, //debug
        PlainDriver.ObjectUnpin(Handle,ErrorHandle, CurrentVar^._Obj.tdo)
        ;//debug, lcOther, 'OCIObjectUnpin', ConSettings);
    if (Obj.Level = 0) and assigned(Obj.tdo) then
      {Free Object}
      //debugCheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.ObjectFree(Handle,ErrorHandle, CurrentVar^._Obj.tdo, 0)
      ;//debug, lcOther, 'OCIObjectFree', ConSettings);
    Dispose(Obj);
    Obj := nil;
  end;

begin
  if Variables <> nil then
  begin
    { Frees allocated memory for output variables }
    for I := 1 to Variables.ActualNum do
    begin
      CurrentVar := @Variables.Variables[I];
      if Assigned(CurrentVar._Obj) then
        DisposeObject(CurrentVar^._Obj);
      if CurrentVar.Data <> nil then
      begin
        if CurrentVar.TypeCode in [SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE] then
        begin
          PlainDriver.DescriptorFree(PPOCIDescriptor(CurrentVar.Data)^,
            OCI_DTYPE_LOB);
        end
        else if CurrentVar.TypeCode = SQLT_TIMESTAMP then
        begin
          PlainDriver.DescriptorFree(PPOCIDescriptor(CurrentVar.Data)^,
            OCI_DTYPE_TIMESTAMP);
        end;
        FreeMem(CurrentVar.Data);
        CurrentVar.Data := nil;
      end;
    end;

    FreeMem(Variables);
  end;
  Variables := nil;
end;

{**
  Allocates in memory and initializes the Oracle variable.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variable an Oracle variable holder.
  @param DataType a DBC data type.
  @param OracleType a correspondent Oracle type.
  @param DataSize a length for string variables.
}

procedure InitializeOracleVar(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Variable: PZSQLVar;
  DataType: TZSQLType; OracleType: ub2; DataSize: Integer);
var
  Length: Integer;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  Variable.ColType := DataType;
  Variable.TypeCode := OracleType;
  Variable.DataSize := DataSize;
  Length := 0;
  case Variable.ColType of
    stByte, stShort, stInteger:
      begin
        Variable.TypeCode := SQLT_INT;
        Length := SizeOf(LongInt);
      end;
    stFloat, stDouble, stLong:
      begin
        Variable.TypeCode := SQLT_FLT;
        Length := SizeOf(Double);
      end;
    stDate, stTime, stTimestamp:
      begin
        Variable.TypeCode := SQLT_TIMESTAMP;
        Length := SizeOf(POCIDateTime);
      end;
    stString, stUnicodeString:
      begin
        Variable.TypeCode := SQLT_STR;
        Length := Variable.DataSize + 1;
      end;
    stAsciiStream, stUnicodeStream, stBinaryStream, stBytes:
      begin
        if not (Variable.TypeCode in [SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE,SQLT_NTY]) then
        begin
          if Variable.ColType = stAsciiStream then
            Variable.TypeCode := SQLT_LVC
          else
            Variable.TypeCode := SQLT_LVB;
          if Variable.DataSize = 0 then
            Length := 128 * 1024 + SizeOf(Integer)
          else
            Length := Variable.DataSize + SizeOf(Integer);
        end
        else
          Length := SizeOf(POCILobLocator);
      end;
    stDataSet: ; //Do nothing here!
    stUnknown:
      Exit;
  end;

  Variable.Length := Length;
  GetMem(Variable.Data, Variable.Length);
  if Variable.TypeCode in [SQLT_BIN, SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE] then
  begin
    PlainDriver.DescriptorAlloc(OracleConnection.GetConnectionHandle,
      PPOCIDescriptor(Variable.Data)^, OCI_DTYPE_LOB, 0, nil);
  end
  else
    if Variable.TypeCode = SQLT_TIMESTAMP then
    begin
      PlainDriver.DescriptorAlloc(OracleConnection.GetConnectionHandle,
        PPOCIDescriptor(Variable.Data)^, OCI_DTYPE_TIMESTAMP, 0, nil);
    end;
end;

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; ErrorHandle: POCIError; Variables: PZSQLVars;
  Values: TZVariantDynArray; ChunkSize: Integer);
var
  I, Len: Integer;
  Status: Integer;
  CurrentVar: PZSQLVar;
  TempDate: TDateTime;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  WriteTempBlob: IZOracleBlob;
  TempStream: TStream;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  for I := 0 to Variables.ActualNum - 1 do
  begin
    CurrentVar := @Variables.Variables[I + 1];
    CurrentVar.DupData := CurrentVar.Data;
    if (high(Values)<I) or DefVarManager.IsNull(Values[I]) then
    begin
      CurrentVar.Indicator := -1;
      //CurrentVar.Data := nil;
    end
    else
    begin
      CurrentVar.Indicator := 0;
      case CurrentVar.TypeCode of
        SQLT_INT:
          begin
            PLongInt(CurrentVar.Data)^ :=
              DefVarManager.GetAsInteger(Values[I]);
          end;
        SQLT_FLT:
          begin
            PDouble(CurrentVar.Data)^ :=
              DefVarManager.GetAsFloat(Values[I]);
          end;
        SQLT_STR:
          begin
            case Values[i].VType of
              vtUnicodeString:
                {$IFDEF WITH_STRLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(CurrentVar.Data),
                  PAnsiChar(PlainDriver.ZPlainString(SoftVarManager.GetAsUnicodeString(Values[I]), Connection.GetConSettings)), 1024);
              else
                {$IFDEF WITH_STRLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(CurrentVar.Data),
                  PAnsiChar(PlainDriver.ZPlainString(SoftVarManager.GetAsString(Values[I]), Connection.GetConSettings)), 1024);
            end;
          end;
        SQLT_VST:
          begin
            {$IFDEF WITH_STRLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(CurrentVar.Data), PAnsiChar(UTF8Encode(DefVarManager.GetAsUnicodeString(Values[I]))), 1024);
          end;
        SQLT_TIMESTAMP:
          begin
            TempDate := DefVarManager.GetAsDateTime(Values[I]);
            DecodeDate(TempDate, Year, Month, Day);
            DecodeTime(TempDate, Hour, Min, Sec, MSec);
            Status := PlainDriver.DateTimeConstruct(
              OracleConnection.GetConnectionHandle,
              ErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
              Year, Month, Day, Hour, Min, Sec, MSec * 1000000, nil, 0);
            CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, '');
          end;
        SQLT_BLOB, SQLT_CLOB:
          begin
            if Values[I].VType = vtBytes then
            begin
              TempBytes := DefVarManager.GetAsBytes(Values[I]);
              Len := Length(TempBytes);
              TempStream := TMemoryStream.Create;
              TempStream.Size := Len;
              System.Move(Pointer(TempBytes)^, TMemoryStream(TempStream).Memory^, Len);
            end
            else
            begin
              TempBlob := DefVarManager.GetAsInterface(Values[I]) as IZBlob;
              if not TempBlob.IsEmpty then
              begin
                if (CurrentVar.TypeCode = SQLT_CLOB) then
                  TempStream := TStringStream.Create(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                      TempBlob.Length, TempBlob.WasDecoded, Connection.GetConSettings))
                else
                  TempStream := TempBlob.GetStream;
              end
              else TempStream := TMemoryStream.Create;
            end;
            try
              WriteTempBlob := TZOracleBlob.Create(PlainDriver,
                TMemoryStream(TempStream).Memory, TempStream.Size, Connection, PPOCIDescriptor(CurrentVar.Data)^,
                CurrentVar.ColType, ChunkSize);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteBlob;
              CurrentVar.Blob := WriteTempBlob;
            finally
              WriteTempBlob := nil;
              TempStream.Free;
            end;
          end;
      end;
    end;
  end;
end;

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(Variables: PZSQLVars);
var
  I: Integer;
  CurrentVar: PZSQLVar;
begin
  for I := 1 to Variables.ActualNum do
  begin
    CurrentVar := @Variables.Variables[I];
    CurrentVar.Blob := nil;
    CurrentVar.Data := CurrentVar.DupData;
  end;
end;

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  TypeName := UpperCase(TypeName);
  Result := stUnknown;

  if (TypeName = 'CHAR') or (TypeName = 'VARCHAR2') then
    Result := stString
  else if (TypeName = 'NCHAR') or (TypeName = 'NVARCHAR2') then
    Result := stString
  else if (TypeName = 'FLOAT') or (TypeName = 'BINARY_FLOAT') or (TypeName = 'BINARY_DOUBLE') then
    Result := stDouble
  else if TypeName = 'DATE' then  {precission - 1 sec, so Timestamp}
    Result := stTimestamp
  else if TypeName = 'BLOB' then
    Result := stBinaryStream
  else if (TypeName = 'RAW') then
    Result := stBytes
  else if (TypeName = 'LONG RAW') then
    Result := stBinaryStream
  else if TypeName = 'CLOB' then
    Result := stAsciiStream
  else if TypeName = 'NCLOB' then
    Result := stAsciiStream
  else if TypeName = 'LONG' then
    Result := stAsciiStream
  else if StartsWith(TypeName, 'TIMESTAMP') then
    Result := stTimestamp
  else if TypeName = 'BFILE' then
    Result := stBinaryStream else
  if TypeName = 'NUMBER' then
  begin
    Result := stDouble;  { default for number types}
    if (Scale = 0) and (Precision <> 0) then
    begin
      if Precision <= 2 then
        Result := stByte
      else if Precision <= 4 then
        Result := stShort
      else if Precision <= 9 then
        Result := stInteger
      else if Precision <= 19 then
        Result := stLong  {!!in fact, unusable}
    end;
  end;
  if ( CtrlsCPType = cCP_UTF16 ) then
    case result of
      stString: Result := stUnicodeString;
      stAsciiStream: if not (TypeName = 'LONG') then Result := stUnicodeStream; //fix: http://zeos.firmos.at/viewtopic.php?t=3530
    end;
end;

{**
  Converts Oracle internal date into TDateTime
  @param Value a pointer to Oracle internal date.
  @return a decoded TDateTime value.
}
function OraDateToDateTime(Value: PAnsiChar): TDateTime;
type
  TOraDate = array[1..7] of Byte;
  POraDate = ^TOraDate;
var
  Ptr: POraDate;
begin
  Ptr := POraDate(Value);
  Result := EncodeDate((Ptr[1] - 100) * 100 + Ptr[2] - 100, Ptr[3], Ptr[4]) +
    EncodeTime(Ptr[5]-1, Ptr[6]-1, Ptr[7]-1, 0);
end;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(PlainDriver: IZOraclePlainDriver;
  ErrorHandle: POCIError; Status: Integer; LogCategory: TZLoggingCategory;
  LogMessage: string);
var
  ErrorMessage: string;
  ErrorBuffer: array[0..255] of AnsiChar;
  ErrorCode: SB4;
begin
  ErrorMessage := '';
  ErrorCode := Status;

  case Status of
    OCI_SUCCESS:
      Exit;
    OCI_SUCCESS_WITH_INFO:
      begin
        PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, ErrorBuffer, 255,
          OCI_HTYPE_ERROR);
        ErrorMessage := 'OCI_SUCCESS_WITH_INFO: ' + String(ErrorBuffer);
      end;
    OCI_NEED_DATA:
      ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, ErrorBuffer, 255,
          OCI_HTYPE_ERROR);
        ErrorMessage := 'OCI_ERROR: ' + String(ErrorBuffer);
      end;
    OCI_INVALID_HANDLE:
      ErrorMessage := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      ErrorMessage := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      ErrorMessage := 'OCI_CONTINUE';
  end;

  if (Status <> OCI_SUCCESS) and (Status <> OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
  begin
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
        ErrorCode, ErrorMessage);
    if not ( ( LogCategory = lcDisconnect ) and ( ErrorCode = 3314 ) ) then //patch for disconnected Server
      //on the other hand we can't close the connction  MantisBT: #0000227
      raise EZSQLException.CreateWithCode(ErrorCode,
        Format(SSQLError1, [ErrorMessage]));
  end;
  if (Status = OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogMessage(LogCategory, PlainDriver.GetProtocol, ErrorMessage);
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; LogSQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError): IZResultSet;
var
  NativeResultSet: TZOracleResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleResultSet.Create(PlainDriver, Statement,
    LogSQL, Handle, ErrorHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (Statement.GetResultSetConcurrency = rcUpdatable)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, LogSQL, nil,
      Statement.GetConnection.GetConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZOracleCachedResolver.Create(
      Statement, NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
      Statement: IZStatement; LogSQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; OutVars: PZSQLVars;
      Const OracleParams: TZOracleParams): IZResultSet;
var
  NativeResultSet: TZOracleCallableResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleCallableResultSet.Create(PlainDriver, Statement,
    LogSQL, StmtHandle, ErrorHandle, OutVars, OracleParams);
  NativeResultSet.SetConcurrency(rcReadOnly);
  CachedResultSet := TZCachedResultSet.Create(NativeResultSet, LogSQL, nil,
    Statement.GetConnection.GetConSettings);
  CachedResultSet.SetConcurrency(rcReadOnly);
  CachedResultSet.SetResolver(TZOracleCachedResolver.Create(
    Statement, NativeResultSet.GetMetadata));
  CachedResultSet.Last;
  CachedResultSet.BeforeFirst;
  Result := CachedResultSet;
end;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError);
var
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  ErrorHandle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    ErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  Handle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    Handle, OCI_HTYPE_STMT, 0, nil);
end;

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);
begin
  if ErrorHandle <> nil then
  begin
    PlainDriver.HandleFree(ErrorHandle, OCI_HTYPE_ERROR);
    ErrorHandle := nil;
  end;
  if Handle <> nil then
  begin
    PlainDriver.HandleFree(Handle, OCI_HTYPE_STMT);
    Handle := nil;
  end;
end;

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(PlainDriver: IZOraclePlainDriver;
  SQL: RawByteString; LogSQL: String; Handle: POCIStmt;
  ErrorHandle: POCIError; PrefetchCount: ub4; ConSettings: PZConSettings);
var
  Status: Integer;
begin
  PlainDriver.AttrSet(Handle, OCI_HTYPE_STMT, @PrefetchCount, SizeOf(ub4),
    OCI_ATTR_PREFETCH_ROWS, ErrorHandle);
  Status := PlainDriver.StmtPrepare(Handle, ErrorHandle, PAnsiChar(SQL),
    Length(SQL)+1, OCI_NTV_SYNTAX, OCI_DEFAULT);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcExecute, LogSQL);
end;

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure ExecuteOracleStatement(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; LogSQL: string; Handle: POCIStmt; ErrorHandle: POCIError);
var
  Status: Integer;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  Status := PlainDriver.StmtExecute(OracleConnection.GetContextHandle,
    Handle, ErrorHandle, 1, 0, nil, nil, OCI_DEFAULT);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcExecute, LogSQL);
end;

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(PlainDriver: IZOraclePlainDriver;
  Handle: POCIStmt; ErrorHandle: POCIError): ub4;
begin
  Result := 0;
  PlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @Result, nil,
    OCI_ATTR_ROW_COUNT, ErrorHandle);
end;

{**
  recurses down the field's TDOs and saves the little bits it need for later
  use on a fetch SQLVar._obj
}
function DescribeObject(PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  ParamHandle: POCIParam; stmt_handle: POCIHandle; Level: ub2): POCIObject;
var
  type_ref: POCIRef;

  function AllocateObject: POCIObject;
  begin
    Result := New(POCIObject);
    FillChar(Result^, SizeOf(TOCIObject), 0);
  end;

  procedure DescribeObjectByTDO(PlainDriver: IZOraclePlainDriver;
    Connection: IZConnection; var obj: POCIObject);
  var
    FConnection: IZOracleConnection;
    list_attibutes: POCIParam;
    name: PAnsiChar;
    temp: RawByteString;
    len: ub4;
    I: ub2;
    Fld: POCIObject;
  begin
    FConnection := Connection as IZOracleConnection;

    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.DescribeAny(FConnection.GetContextHandle,
        FConnection.GetErrorHandle, obj.tdo, 0, OCI_OTYPE_PTR, OCI_DEFAULT,
        OCI_PTYPE_TYPE, FConnection.GetDescribeHandle),
      lcOther, 'OCIDescribeAny(OCI_PTYPE_TYPE) of OCI_OTYPE_PTR');

    //we have the Actual TDO  so lets see what it is made up of by a describe
    Len := 0;  //and we store it in the object's paramdp for now
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(FConnection.GetDescribeHandle, OCI_HTYPE_DESCRIBE,
        @obj.parmdp, @Len, OCI_ATTR_PARAM, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_HTYPE_DESCRIBE) of OCI_ATTR_PARAM');

    //Get the SchemaName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_SCHEMA_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_SCHEMA_NAME) of OCI_DTYPE_PARAM');

    SetLength(temp, len+1);
    temp := {$IFDEF WITH_STRPLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrPLCopy(PAnsiChar(temp), name, len);
    Obj.type_schema := PlainDriver.ZDbcString(temp, Connection.GetConSettings);

    //Get the TypeName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM');

    SetLength(temp, len+1);
    temp := {$IFDEF WITH_STRPLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrPLCopy(PAnsiChar(temp), name, len);
    Obj.type_name := PlainDriver.ZDbcString(temp, Connection.GetConSettings);

    //Get the TypeCode of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @Obj.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM');

    if (obj.typecode = OCI_TYPECODE_OBJECT ) or ( obj.typecode = OCI_TYPECODE_OPAQUE) then
    begin
      //we will need a reff to the TDO for the pin operation
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.obj_ref, nil, OCI_ATTR_REF_TDO, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_REF_TDO) of OCI_DTYPE_PARAM');

      //now we'll pin the object
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.ObjectPin(FConnection.GetConnectionHandle, FConnection.GetErrorHandle,
          Obj.obj_ref, nil, OCI_PIN_LATEST, OCI_DURATION_SESSION, pub2(OCI_LOCK_NONE),
          @obj.obj_type),
        lcOther, 'OCIObjectPin(OCI_PIN_LATEST, OCI_DURATION_SESSION, OCI_LOCK_NONE)');
      Obj.Pinned := True;

      //is the object the final type or an type-descriptor?
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.is_final_type, nil, OCI_ATTR_IS_FINAL_TYPE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_IS_FINAL_TYPE) of OCI_DTYPE_PARAM(SubType)');

      //Get the FieldCount
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.field_count, nil, OCI_ATTR_NUM_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_NUM_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)');

      //now get the differnt fields of this object add one field object for property
      SetLength(Obj.fields, Obj.field_count);

      //a field is just another instance of an obj not a new struct
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @list_attibutes, nil, OCI_ATTR_LIST_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_LIST_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)');

      if obj.field_count > 0 then
      for I := 0 to obj.field_count-1 do
      begin
        Fld := AllocateObject;  //allocate a new object
          Obj.fields[i] := Fld;  //assign the object to the field-list

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.ParamGet(list_attibutes, OCI_DTYPE_PARAM,
            FConnection.GetErrorHandle, Fld.parmdp, I+1),
          lcOther, 'OCIParamGet(OCI_DTYPE_PARAM) of OCI_DTYPE_PARAM(Element)');

        // get the name of the attribute
        len := 0;
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
            @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM(Element)');

        SetLength(temp, len+1);
        temp := {$IFDEF WITH_STRPLCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrPLCopy(PAnsiChar(temp), name, len);
        Fld.type_name := PlainDriver.ZDbcString(temp, Connection.GetConSettings);

        // get the typeCode of the attribute
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
            @Fld.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM(Element)');

        if (fld.typecode = OCI_TYPECODE_OBJECT) or
           (fld.typecode = OCI_TYPECODE_VARRAY) or
           (fld.typecode = OCI_TYPECODE_TABLE) or
           (fld.typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
          //this is some sort of object or collection so lets drill down some more
          fld.next_subtype := DescribeObject(PlainDriver, Connection, fld.parmdp,
              obj.stmt_handle, obj.Level+1);
      end;
    end
    else
    begin
      //this is an embedded table or varray of some form so find out what is in it*/

      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.col_typecode, nil, OCI_ATTR_COLLECTION_TYPECODE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_TYPECODE) of OCI_DTYPE_PARAM');

      //first get what sort of collection it is by coll typecode
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.parmap, nil, OCI_ATTR_COLLECTION_ELEMENT, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_ELEMENT) of OCI_DTYPE_PARAM');

      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.elem_typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE of Element) of OCI_DTYPE_PARAM');

      if (obj.elem_typecode = OCI_TYPECODE_OBJECT) or
         (obj.elem_typecode = OCI_TYPECODE_VARRAY) or
         (obj.elem_typecode = OCI_TYPECODE_TABLE) or
         (obj.elem_typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
        //this is some sort of object or collection so lets drill down some more
        obj.next_subtype := DescribeObject(PlainDriver, Connection, obj.parmap,
            obj.stmt_handle, obj.Level+1);
    end;
  end;
begin
    Result := AllocateObject;

  //Describe the field (OCIParm) we know it is a object or a collection

  //Get the Actual TDO
  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.AttrGet(ParamHandle, OCI_DTYPE_PARAM, @type_ref, nil,
      OCI_ATTR_REF_TDO, (Connection as IZOracleConnection).GetErrorHandle),
    lcOther, 'OCIAttrGet OCI_ATTR_REF_TDO of OCI_DTYPE_PARAM');

  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.TypeByRef((Connection as IZOracleConnection).GetConnectionHandle,
      (Connection as IZOracleConnection).GetErrorHandle, type_ref,
      OCI_DURATION_TRANS, OCI_TYPEGET_ALL, @Result.tdo),
    lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO');
  Result^.Level := Level;
  DescribeObjectByTDO(PlainDriver, Connection, Result);
end;

end.

