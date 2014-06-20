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
  Max_OCI_String_Size = 1024*32;


type
  {** Declares SQL Object }

  POCIObject = ^TOCIObject;
  TObjFields = array of POCIObject;
  PObjFields = ^TObjFields;
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
    fields:         PObjFields;       //one object for each field/property
    field_count:    ub2;              //The number of fields Not really needed but nice to have
    next_subtype:   POCIObject;       //There is strored information about subtypes for inteherited objects
    stmt_handle:    POCIStmt;         //the Statement-Handle
    Level:          Integer;          //the instance level
  end;

  PUB2Array = ^TUB2Array;
  TUB2Array = array[0..0] of ub2;
  PSB2Array = ^TSB2Array;
  TSB2Array = array[0..0] of sb2;

  PZSQLVar = ^TZSQLVar;
  TZSQLVar = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    {OCI Handles}
    Handle:    POCIHandle;
    Define:    POCIHandle;
    BindHandle: POCIBind;
    {binding values}
    Data:      Pointer;
    oDataType:  ub2;
    oDataSize:  ub2;
    oIndicatorArray: PSB2Array; //indicates NULL ...
    oDataSizeArray: PUB2Array; //value length for strings/bytes
    _Obj:      POCIObject;
    {Zeos proceesing values}
    TypeCode:  ub2;
    Length:    NativeUInt; //indicate size of Data
    Precision: Integer; //field.precision
    Scale:     Integer; //field.scale
    ColType:   TZSQLType; //Zeos SQLType
    Blob:      IZBlob; //temporary interface
    CodePage:  Word; //ColumnCodePage
    AllocMem:   Boolean; //isn't it selve explainatory?
  end;

  TZSQLVars = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    AllocNum:  ub4;
    Variables: array[0..MAX_SQLVAR_LIMIT] of TZSQLVar;
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

  TDesciptorRec = record
    htype: sb4; //Indicate C/BLOB or TimeStamps
    Descriptors: TPointerDynArray; //allocated descriptors
    Lobs: TInterfaceDynArray; //PlaceHolder to keep lob refcount > 0
  end;
  TDesciptorRecArray = array of TDesciptorRec;

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
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const ConSettings: PZConSettings);

procedure DefineOracleVarTypes(var Variable: PZSQLVar; DataType: TZSQLType;
  DataSize: Integer; OracleType: ub2; DoAllocMem: Boolean = True);

procedure SetVariableDataEntrys(var BufferEntry: PAnsiChar; var Variable: PZSQLVar;
  Iteration: LongWord);

procedure AllocDesriptors(const PlainDriver: IZOraclePlainDriver;
  ConnectionHandle: POCIEnv; var Variable: PZSQLVar; Iteration: Integer);

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; const ErrorHandle: POCIError;
  Variables: PZSQLVars; const Values: TZVariantDynArray; const ChunkSize: Integer);

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders or array of TDesciptorRec.
  @param ArrayCount count of bound arrays
}
procedure UnloadOracleVars(var Variables; const ArrayCount: Integer);

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
procedure CheckOracleError(const PlainDriver: IZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const LogSQL: string; const Handle: POCIStmt;
  const ErrorHandle: POCIError; ZBufferSize: Integer): IZResultSet; overload;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; LogSQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError; const Params: PZSQLVars;
  Const OracleParams: TZOracleParams): IZResultSet; overload;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError;
  UserServerCachedStmt: Boolean = False);

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
procedure PrepareOracleStatement(const PlainDriver: IZOraclePlainDriver;
  ContextHandle: POCISvcCtx; const SQL: RawByteString; var Handle: POCIStmt;
  const ErrorHandle: POCIError; PrefetchMemory: ub4; ServerCachedStmtHandle: Boolean;
  const ConSettings: PZConSettings);

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param ContectHandle the OCI ContextHandle.
  @param SQL an SQL query to be logged.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @param ConSettings the connection settings record.
  @param AutoCommit the commit each execution?.
}
procedure ExecuteOracleStatement(const PlainDriver: IZOraclePlainDriver;
  const ContextHandle: POCISvcCtx; const LogSQL: RawByteString;
  const Handle: POCIStmt; const ErrorHandle: POCIError;
  const ConSettings: PZConSettings; const AutoCommit: Boolean;
  const Iters: Integer);

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(const PlainDriver: IZOraclePlainDriver;
  const Handle: POCIStmt; const ErrorHandle: POCIError): ub4;

function DescribeObject(PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  ParamHandle: POCIParam; stmt_handle: POCIHandle; obj: POCIObject; Level: ub2): POCIObject;

procedure OraWriteLob(const PlainDriver: IZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);

implementation

uses Math, ZMessages, ZDbcOracle, ZDbcOracleResultSet, ZDbcCachedResultSet,
  ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

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

  Size := SizeOf(TZSQLVars) + Count * SizeOf(TZSQLVar);
  GetMem(Variables, Size);
  FillChar(Variables^, Size, 0);
  Variables^.AllocNum := Count;
end;

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const ConSettings: PZConSettings);
var
  I: Integer;
  J: Word;
  CurrentVar: PZSQLVar;

  procedure DisposeObject(Obj: POCIObject);
  var
    I: Integer;
  begin
    if Assigned(Obj.fields) then
    begin
      for i := 0 to Obj.field_count-1 do
        DisposeObject(Obj.fields^[i]);
      SetLength(Obj.fields^, 0);
      Dispose(Obj.fields);
      Obj.fields := nil;
    end;
    if Assigned(Obj.next_subtype) then
    begin
      DisposeObject(Obj.next_subtype);
      Obj.next_subtype := nil;
    end;
    if Obj.Level = 0 then
    begin
      {Unpin tdo}
      CheckOracleError(PlainDriver, ErrorHandle, PlainDriver.ObjectUnpin(Handle,ErrorHandle, CurrentVar^._Obj.tdo), lcOther, 'OCIObjectUnpin', ConSettings);
      CheckOracleError(PlainDriver, ErrorHandle, PlainDriver.ObjectFree(Handle,ErrorHandle, CurrentVar^._Obj.tdo, 0), lcOther, 'OCIObjectFree', ConSettings);;
    end;

    Dispose(Obj);
  end;

begin
  if Variables <> nil then
  begin
    { Frees allocated memory for output variables }
    for I := 0 to Variables.AllocNum-1 do
    begin
      CurrentVar := @Variables.Variables[I];
      if Assigned(CurrentVar^._Obj) then
      begin
        DisposeObject(CurrentVar^._Obj);
        CurrentVar^._Obj := nil;
      end;
      if CurrentVar^.Data <> nil then
      begin
        if CurrentVar^.TypeCode in [SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE] then
          for J := 0 to Iteration-1 do
            PlainDriver.DescriptorFree(PPOCIDescriptor(NativeUInt(CurrentVar^.Data)+(J*SizeOf(Pointer)))^,
              OCI_DTYPE_LOB)
        else
          if CurrentVar^.TypeCode = SQLT_TIMESTAMP then
            for J := 0 to Iteration-1 do
              PlainDriver.DescriptorFree(PPOCIDescriptor(NativeUInt(CurrentVar^.Data)+(J*SizeOf(Pointer)))^,
                OCI_DTYPE_TIMESTAMP);
        CurrentVar^.Data := nil;
      end;
      CurrentVar^.oIndicatorArray := nil;
      CurrentVar^.oDataSizeArray := nil;
    end;
    FreeMem(Variables);
    Variables := nil;
  end;
end;

procedure DefineOracleVarTypes(var Variable: PZSQLVar; DataType: TZSQLType;
  DataSize: Integer; OracleType: ub2; DoAllocMem: Boolean = True);
begin
  with Variable^ do
  begin
    ColType := DataType;
    TypeCode := OracleType;
    oDataSize := DataSize;
    AllocMem := DoAllocMem;
    case ColType of
      stByte, stShort, stWord, stSmall, stInteger:
        begin
          TypeCode := SQLT_INT;
          Length := SizeOf(LongInt);
        end;
      stUlong:
        begin
          TypeCode := SQLT_STR;
          oDataSize := 22;
          Length := oDataSize +1; //for trailing #0
        end;
      stLong, stFloat, stDouble, stLongWord, stCurrency, stBigDecimal:
        begin
          TypeCode := SQLT_FLT;
          Length := SizeOf(Double);
        end;
      stDate, stTime, stTimestamp:
        begin
          TypeCode := SQLT_TIMESTAMP;
          Length := SizeOf(POCIDateTime);
        end;
      stString, stUnicodeString:
        if OracleType = SQLT_AFC then
          Length := oDataSize
        else
        begin
          TypeCode := SQLT_STR;
          Length := oDataSize + 1;
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream, stBytes:
        if (TypeCode in [SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE,SQLT_NTY]) then
          Length := SizeOf(POCILobLocator)
        else
        begin
          if ColType = stAsciiStream then
            TypeCode := SQLT_LVC
          else
            TypeCode := SQLT_LVB;
          if oDataSize = 0 then
            Length := 128 * 1024 + SizeOf(Integer)
          else
            Length := oDataSize + SizeOf(Integer);
        end;
      stDataSet: ; //Do nothing here!
      stUnknown:
        Exit;
    end;
  end;
end;

procedure SetVariableDataEntrys(var BufferEntry: PAnsiChar; var Variable: PZSQLVar;
  Iteration: LongWord);
begin
  with Variable^ do
  begin
  {now let's set binding entrys}
  //step one: set null indicators
    oIndicatorArray := Pointer(BufferEntry);
    Inc(BufferEntry, SizeOf(sb2)*Iteration);
  //Step two: set Length Indicators if required
    oDataSizeArray := Pointer(NativeUInt(BufferEntry) * Byte((TypeCode=SQLT_STR) and (oDataSize > 0))); //either nil or valid address
    Inc(BufferEntry, Byte((TypeCode=SQLT_STR) and (oDataSize > 0))*SizeOf(ub2)*Iteration); //inc 0 or SizeOf(ub2)
  //Step three: set data entrys if required
    Data := Pointer(NativeUInt(BufferEntry) * Byte(AllocMem)); //either nil or valid address
    Inc(BufferEntry, Byte(AllocMem)*Length*Iteration); //inc 0 or Length
  end;
end;

procedure AllocDesriptors(const PlainDriver: IZOraclePlainDriver;
  ConnectionHandle: POCIEnv; var Variable: PZSQLVar; Iteration: Integer);
var
  i: LongWord;
  hType: Integer;
begin
  case Variable^.TypeCode of
    SQLT_TIMESTAMP:
      hType := OCI_DTYPE_TIMESTAMP;
    SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
      hType := OCI_DTYPE_LOB;
    else
      Exit;
  end;
  for i := 0 to Iteration -1 do
    PlainDriver.DescriptorAlloc(ConnectionHandle,
      PPOCIDescriptor(NativeUInt(Variable^.Data)+(I*SizeOf(POCIDescriptor)))^, hType, 0, nil)
end;

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; const ErrorHandle: POCIError;
  Variables: PZSQLVars; const Values: TZVariantDynArray; const ChunkSize: Integer);
var
  I, Len: Integer;
  CurrentVar: PZSQLVar;
  TempDate: TDateTime;
  TempBytes: TBytes;
  TempBlob: IZBlob;
  WriteTempBlob: IZOracleBlob;

  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  OracleConnection: IZOracleConnection;
  ClientVarManager: IZClientVariantManager;
  Buffer: Pointer;
  AnsiTemp: RawByteString;
  ConSettings: PZConSettings;
  CharRec: TZCharRec;
begin
  OracleConnection := Connection as IZOracleConnection;
  ClientVarManager := Connection.GetClientVariantManager;
  ConSettings := Connection.GetConSettings;
  for I := 0 to Variables.AllocNum - 1 do
  begin
    CurrentVar := @Variables.Variables[I];
    if (high(Values)<I) or ClientVarManager.IsNull(Values[I]) then
      CurrentVar^.oIndicatorArray^[0] := -1
    else
    begin
      CurrentVar^.oIndicatorArray^[0] := 0;
      case CurrentVar^.TypeCode of
        SQLT_INT:
          PLongInt(CurrentVar^.Data)^ := ClientVarManager.GetAsInteger(Values[I]);
        SQLT_FLT:
          PDouble(CurrentVar^.Data)^ := ClientVarManager.GetAsFloat(Values[I]);
        SQLT_STR:
          if CurrentVar^.AllocMem then //no mem is allocated. data is just a pointer to a locale string of TZCharRec
          begin
            CharRec := ClientVarManager.GetAsCharRec(Values[i], ConSettings^.ClientCodePage^.CP);
            CurrentVar^.oDataSizeArray^[0] := Math.Min(CharRec.Len, Max_OCI_String_Size)+1; //need the leading $0, because oracle expects it
            System.Move(CharRec.P^, CurrentVar^.Data^, CurrentVar^.oDataSizeArray^[0]);
            (PAnsiChar(CurrentVar^.Data)+CurrentVar^.oDataSizeArray^[0]-1)^ := #0; //improve  StrLCopy...
          end;
        SQLT_TIMESTAMP:
          begin
            TempDate := ClientVarManager.GetAsDateTime(Values[I]);
            DecodeDate(TempDate, Year, Month, Day);
            DecodeTime(TempDate, Hour, Min, Sec, MSec);
            CheckOracleError(PlainDriver, ErrorHandle,
              PlainDriver.DateTimeConstruct(
                OracleConnection.GetConnectionHandle,
                ErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
                Year, Month, Day, Hour, Min, Sec, MSec * 1000000, nil, 0),
              lcOther, '', ConSettings);
          end;
        SQLT_BLOB:
          begin
            if Values[I].VType = vtBytes then
            begin
              TempBytes := ClientVarManager.GetAsBytes(Values[I]);
              Len := Length(TempBytes);
              if Len > 0 then
                Buffer := @TempBytes[0]
              else
                Buffer := nil;
            end
            else
            begin
              TempBlob := ClientVarManager.GetAsInterface(Values[I]) as IZBlob;
              if TempBlob.IsEmpty then
              begin
                Buffer := nil;
                Len := 0;
              end
              else
              begin
                Buffer := TempBlob.GetBuffer;
                Len := TempBlob.Length;
              end;
            end;
            try
              WriteTempBlob := TZOracleBlob.Create(PlainDriver, nil, 0,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                PPOCIDescriptor(CurrentVar^.Data)^, ChunkSize, ConSettings);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteLobFromBuffer(Buffer, Len);
              CurrentVar^.Blob := WriteTempBlob;
            finally
              WriteTempBlob := nil;
            end;
          end;
        SQLT_CLOB:
          try
            TempBlob := ClientVarManager.GetAsInterface(Values[I]) as IZBlob;
            if TempBlob.IsClob then
            begin
              WriteTempBlob := TZOracleClob.Create(PlainDriver,
                nil, 0, OracleConnection.GetConnectionHandle,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                PPOCIDescriptor(CurrentVar^.Data)^, ChunkSize, ConSettings,
                ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.CreateBlob;
              Buffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.WriteLobFromBuffer(Buffer, TempBlob.Length);
            end
            else
            begin
              if not TempBlob.IsEmpty then
              begin
                AnsiTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, Connection.GetConSettings);
                Len := Length(AnsiTemp);
                if Len = 0 then
                  Buffer := nil
                else
                  Buffer := @AnsiTemp[1];
              end
              else
              begin
                Buffer := nil;
                Len := 0;
              end;
              WriteTempBlob := TZOracleClob.Create(PlainDriver, nil, 0,
                OracleConnection.GetConnectionHandle, OracleConnection.GetContextHandle,
                OracleConnection.GetErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
                ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteLobFromBuffer(Buffer, Len);
            end;
            CurrentVar^.Blob := WriteTempBlob;
          finally
            WriteTempBlob := nil;
          end
      end;
    end;
  end;
end;

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(var Variables; const ArrayCount: Integer);
var
  I, J: Integer;
  SQLVars: PZSQLVars absolute Variables;
  DescriptorsArray: TDesciptorRecArray absolute Variables;
begin
  if ArrayCount = 0 then
    for I := 0 to SQLVars.AllocNum -1 do
      SQLVars.Variables[i].Blob := nil
  else
    for i := 0 to High(DescriptorsArray) do
      if (DescriptorsArray[i].htype = OCI_DTYPE_LOB) then
        for j := 0 to ArrayCount -1 do
          DescriptorsArray[i].Lobs[j] := nil;
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
        Result := stSmall
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
procedure CheckOracleError(const PlainDriver: IZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);
var
  ErrorMessage: RawByteString;
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
        ErrorMessage := 'OCI_SUCCESS_WITH_INFO: ' + RawByteString(ErrorBuffer);
      end;
    OCI_NEED_DATA:
      ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        if PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, ErrorBuffer, 255,
          OCI_HTYPE_ERROR) = 100 then
          ErrorMessage := 'OCI_ERROR: Unkown(OCI_NO_DATA)'
        else
          ErrorMessage := 'OCI_ERROR: ' + RawByteString(ErrorBuffer);
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
      DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
        ErrorCode, ErrorMessage);
    if not ( ( LogCategory = lcDisconnect ) and ( ErrorCode = 3314 ) ) then //patch for disconnected Server
      //on the other hand we can't close the connction  MantisBT: #0000227
      raise EZSQLException.CreateWithCode(ErrorCode,
        Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
  if (Status = OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogMessage(LogCategory, ConSettings^.Protocol, ErrorMessage);
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const LogSQL: string; const Handle: POCIStmt;
  const ErrorHandle: POCIError; ZBufferSize: Integer): IZResultSet;
var
  NativeResultSet: TZOracleResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleResultSet.Create(PlainDriver, Statement,
    LogSQL, Handle, ErrorHandle, ZBufferSize);
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
      ErrorHandle: POCIError; const Params: PZSQLVars;
      Const OracleParams: TZOracleParams): IZResultSet;
var
  NativeResultSet: TZOracleCallableResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleCallableResultSet.Create(PlainDriver, Statement,
    LogSQL, StmtHandle, ErrorHandle, Params, OracleParams);
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
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError;
  UserServerCachedStmt: Boolean = False);
var
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  ErrorHandle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    ErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  Handle := nil;
  if not UserServerCachedStmt then
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
procedure PrepareOracleStatement(const PlainDriver: IZOraclePlainDriver;
  ContextHandle: POCISvcCtx; const SQL: RawByteString; var Handle: POCIStmt;
  const ErrorHandle: POCIError; PrefetchMemory: ub4; ServerCachedStmtHandle: Boolean;
  const ConSettings: PZConSettings);
var
  PrefetchCount: ub4;
begin
  PrefetchCount := 0;
  if ServerCachedStmtHandle then
  begin
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.StmtPrepare2(ContextHandle, Handle, ErrorHandle,
        Pointer(SQL), Length(SQL),nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT),
      lcExecute, SQL, ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
     PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT, @PrefetchCount ,0, OCI_ATTR_PREFETCH_ROWS,ErrorHandle),
        lcOther, 'Prefetch_Count', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT,@PrefetchMemory,0,OCI_ATTR_PREFETCH_MEMORY,ErrorHandle),
        lcOther, 'Prefetch_Memory', ConSettings);
  end
  else
  begin
    CheckOracleError(PlainDriver, ErrorHandle,
     PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT, @PrefetchCount ,0, OCI_ATTR_PREFETCH_ROWS,ErrorHandle),
        lcOther, 'Prefetch_Count', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT,@PrefetchMemory,0,OCI_ATTR_PREFETCH_MEMORY,ErrorHandle),
        lcOther, 'Prefetch_Memory', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle, PlainDriver.StmtPrepare(Handle,
      ErrorHandle, Pointer(SQL), Length(SQL), OCI_NTV_SYNTAX, OCI_DEFAULT),
      lcExecute, SQL, ConSettings);
  end;
end;

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param ContectHandle the OCI ContextHandle.
  @param SQL an SQL query to be logged.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @param ConSettings the connection settings record.
  @param AutoCommit the commit each execution?.
}
procedure ExecuteOracleStatement(const PlainDriver: IZOraclePlainDriver;
  const ContextHandle: POCISvcCtx; const LogSQL: RawByteString;
  const Handle: POCIStmt; const ErrorHandle: POCIError;
  const ConSettings: PZConSettings; const AutoCommit: Boolean;
  const Iters: Integer);
begin
  if AutoCommit then
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.StmtExecute(ContextHandle,
        Handle, ErrorHandle, Iters, 0, nil, nil, OCI_COMMIT_ON_SUCCESS),
      lcExecute, LogSQL, ConSettings)
  else
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.StmtExecute(ContextHandle,
        Handle, ErrorHandle, Iters, 0, nil, nil, OCI_DEFAULT),
      lcExecute, LogSQL, ConSettings);
end;

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(const PlainDriver: IZOraclePlainDriver;
  const Handle: POCIStmt; const ErrorHandle: POCIError): ub4;
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
  ParamHandle: POCIParam; stmt_handle: POCIHandle; obj: POCIObject; Level: ub2): POCIObject;
var
  type_ref: POCIRef;
  ConSettings: PZConSettings;

  function AllocateObject: POCIObject;
  begin
    Result := New(POCIObject);
    Result.parmdp := nil;
    Result.parmap := nil;
    Result.tdo := nil;
    Result.typecode := 0;
    Result.col_typecode := 0;
    Result.elem_typecode := 0;
    Result.obj_ref := nil;
    Result.obj_ind := nil;
    Result.obj_value := nil;
    Result.obj_type := nil;
    Result.fields := nil;
    Result.field_count := 0;
    Result.next_subtype := nil;
    Result.is_final_type := ub1(0);
    Result.stmt_handle := stmt_handle;
    Result.Level := Level;
  end;

  procedure DescribeObjectByTDO(PlainDriver: IZOraclePlainDriver;
    Connection: IZConnection; obj: POCIObject);
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
      lcOther, 'OCIDescribeAny(OCI_PTYPE_TYPE) of OCI_OTYPE_PTR', ConSettings);

    //we have the Actual TDO  so lets see what it is made up of by a describe
    Len := 0;  //and we store it in the object's paramdp for now
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(FConnection.GetDescribeHandle, OCI_HTYPE_DESCRIBE,
        @obj.parmdp, @Len, OCI_ATTR_PARAM, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_HTYPE_DESCRIBE) of OCI_ATTR_PARAM', ConSettings);

    //Get the SchemaName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_SCHEMA_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_SCHEMA_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp{%H-});
    Obj.type_schema := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp);
    Obj.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeCode of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @Obj.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

    if (obj.typecode = OCI_TYPECODE_OBJECT ) or ( obj.typecode = OCI_TYPECODE_OPAQUE) then
    begin
      //we will need a reff to the TDO for the pin operation
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.obj_ref, nil, OCI_ATTR_REF_TDO, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_REF_TDO) of OCI_DTYPE_PARAM', ConSettings);

      //now we'll pin the object
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.ObjectPin(FConnection.GetConnectionHandle, FConnection.GetErrorHandle,
          Obj.obj_ref, nil, OCI_PIN_LATEST, OCI_DURATION_SESSION, pub2(OCI_LOCK_NONE),
          @obj.obj_type),
        lcOther, 'OCIObjectPin(OCI_PIN_LATEST, OCI_DURATION_SESSION, OCI_LOCK_NONE)', ConSettings);

      //is the object the final type or an type-descriptor?
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.is_final_type, nil, OCI_ATTR_IS_FINAL_TYPE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_IS_FINAL_TYPE) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //Get the FieldCount
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.field_count, nil, OCI_ATTR_NUM_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_NUM_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //now get the differnt fields of this object add one field object for property
      Obj.fields := New(PObjFields);
      SetLength(Obj.fields^, Obj.field_count);

      //a field is just another instance of an obj not a new struct
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @list_attibutes, nil, OCI_ATTR_LIST_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_LIST_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      for I := 0 to obj.field_count-1 do
      begin
        Fld := AllocateObject;  //allocate a new object
        Obj.fields^[i] := Fld;  //assign the object to the field-list

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.ParamGet(list_attibutes, OCI_DTYPE_PARAM,
            FConnection.GetErrorHandle, Fld.parmdp, I+1),
          lcOther, 'OCIParamGet(OCI_DTYPE_PARAM) of OCI_DTYPE_PARAM(Element)', ConSettings);

        // get the name of the attribute
        len := 0;
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
            @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM(Element)', ConSettings);

        ZSetString(name, len, temp);
        Fld.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

        // get the typeCode of the attribute
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
            @Fld.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM(Element)', ConSettings);

        if (fld.typecode = OCI_TYPECODE_OBJECT) or
           (fld.typecode = OCI_TYPECODE_VARRAY) or
           (fld.typecode = OCI_TYPECODE_TABLE) or
           (fld.typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
          //this is some sort of object or collection so lets drill down some more
          fld.next_subtype := DescribeObject(PlainDriver, Connection, fld.parmdp,
            obj.stmt_handle, Fld, obj.Level+1);
      end;
    end
    else
    begin
      //this is an embedded table or varray of some form so find out what is in it*/

      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.col_typecode, nil, OCI_ATTR_COLLECTION_TYPECODE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

      //first get what sort of collection it is by coll typecode
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.parmap, nil, OCI_ATTR_COLLECTION_ELEMENT, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_ELEMENT) of OCI_DTYPE_PARAM', ConSettings);

      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @obj.elem_typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE of Element) of OCI_DTYPE_PARAM', ConSettings);

      if (obj.elem_typecode = OCI_TYPECODE_OBJECT) or
         (obj.elem_typecode = OCI_TYPECODE_VARRAY) or
         (obj.elem_typecode = OCI_TYPECODE_TABLE) or
         (obj.elem_typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
        //this is some sort of object or collection so lets drill down some more
        obj.next_subtype := DescribeObject(PlainDriver, Connection, obj.parmap,
          obj.stmt_handle, nil, obj.Level+1);
    end;
  end;
begin
  ConSettings := Connection.GetConSettings;

  if Assigned(obj) then
    Result := obj
  else
    Result := AllocateObject;

  //Describe the field (OCIParm) we know it is a object or a collection

  //Get the Actual TDO
  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.AttrGet(ParamHandle, OCI_DTYPE_PARAM, @type_ref, nil,
      OCI_ATTR_REF_TDO, (Connection as IZOracleConnection).GetErrorHandle),
    lcOther, 'OCIAttrGet OCI_ATTR_REF_TDO of OCI_DTYPE_PARAM', ConSettings);

  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.TypeByRef((Connection as IZOracleConnection).GetConnectionHandle,
      (Connection as IZOracleConnection).GetErrorHandle, type_ref,
      OCI_DURATION_TRANS, OCI_TYPEGET_ALL, @Result.tdo),
    lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
  DescribeObjectByTDO(PlainDriver, Connection, Result);
end;

procedure OraWriteLob(const PlainDriver: IZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);
var
  Status: sword;
  ContentSize, OffSet: ub4;

  function DoWrite(AOffSet: ub4; AChunkSize: ub4; APiece: ub1): sword;
  var
    AContentSize: ub4;
  begin
    if BinaryLob then
    begin
      AContentSize := ContentSize;
      Result := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, 0, SQLCS_IMPLICIT);
    end
    else
    begin
      if ContentSize > 0 then
        AContentSize := ConSettings^.ClientCodePage^.CharWidth
      else
      begin
        AContentSize := ContentSize;
        AChunkSize := ConSettings^.ClientCodePage^.CharWidth;
      end;

      Result := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, ConSettings^.ClientCodePage^.ID, SQLCS_IMPLICIT);
    end;
    ContentSize := AContentSize;
    inc(OffSet, AChunkSize);
  end;
begin

  { Opens a large object or file for read. }
  Status := PlainDriver.LobOpen(ContextHandle, ErrorHandle, LobLocator, OCI_LOB_READWRITE);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Open Large Object', ConSettings);

  { Checks for empty blob.}
  { This test doesn't use IsEmpty because that function does allow for zero length blobs}
  if (BlobSize > 0) then
  begin
    if not BinaryLob then
      BlobSize := BlobSize-1;
    if BlobSize > ChunkSize then
    begin
      OffSet := 0;
      ContentSize := 0;

      Status := DoWrite(1, ChunkSize, OCI_FIRST_PIECE);
      if Status <> OCI_NEED_DATA then
        CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);

      if (BlobSize - OffSet) > ChunkSize then
        while (BlobSize - OffSet) > ChunkSize do //take care there is room left for LastPiece
        begin
          Status := DoWrite(offset, ChunkSize, OCI_NEXT_PIECE);
          if Status <> OCI_NEED_DATA then
            CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);
        end;
      Status := DoWrite(offset, BlobSize - OffSet, OCI_LAST_PIECE);
    end
    else
    begin
      ContentSize := BlobSize;
      Status := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        ContentSize, 1, BlobData, BlobSize, OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
    end;
  end
  else
    Status := PlainDriver.LobTrim(ContextHandle, ErrorHandle, LobLocator, 0);

  CheckOracleError(PlainDriver, ErrorHandle,
    Status, lcOther, 'Write Large Object', ConSettings);

  { Closes large object or file. }
  Status := PlainDriver.LobClose(ContextHandle, ErrorHandle, LobLocator);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Close Large Object', ConSettings);
end;


end.
