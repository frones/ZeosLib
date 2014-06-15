{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Oracle Database Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcOracleResultSet;

interface

{$I ZDbc.inc}

uses
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcOracle, ZDbcResultSet, ZPlainOracleDriver,
  ZDbcResultSetMetadata, ZDbcLogging, ZCompatibility, ZDbcOracleUtils,
  ZPlainOracleConstants;

type
  {** Implements Oracle ResultSet. }
  TZOracleAbstractResultSet = class(TZAbstractResultSet)
  private
    FStmtHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FConnection: IZOracleConnection;
    FOutVars: PZSQLVars;
    FChunkSize: Integer;
    function GetSQLVarHolder(ColumnIndex: Integer): PZSQLVar; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetAsStringValue(ColumnIndex: Integer;
      SQLVarHolder: PZSQLVar): RawByteString;
    function GetAsDateTimeValue(const SQLVarHolder: PZSQLVar): TDateTime;
    function GetFinalObject(Obj: POCIObject): POCIObject;
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError);

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetAnsiRec(ColumnIndex: Integer): TZAnsiRec; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetDataSet(ColumnIndex: Integer): IZDataSet; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
  end;

  TZOracleResultSet = class(TZOracleAbstractResultSet)
  protected
    procedure Open; override;
  public
    procedure Close; override;
    function Next: Boolean; override;
  end;

  TZOracleCallableResultSet = Class(TZOracleAbstractResultSet)
  private
    FFieldNames: TStringDynArray;
    function PrepareOracleOutVars(InVars: PZSQLVars;
      const OracleParams: TZOracleParams): PZSQLVars;
  protected
    procedure Open; override;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; OutVars: PZSQLVars; const OracleParams: TZOracleParams);
    procedure Close; override;
    function Next: Boolean; override;
  End;

  {** Represents an interface, specific for Oracle blobs. }
  IZOracleBlob = interface(IZBlob)
    ['{3D861AAC-B263-42F1-B359-2A188D1D986A}']
    procedure CreateBlob;
    procedure ReadLob;
    procedure WriteLob;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

  {** Implements external blob wrapper object for Oracle. }
  TZOracleBlob = class(TZAbstractUnCachedBlob, IZOracleBlob)
  private
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FPlainDriver: IZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
    FConSettings: PZConSettings;
  protected
    procedure InternalSetData(AData: Pointer; ASize: Integer);
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Data: Pointer; const Size: Int64; const ContextHandle: POCISvcCtx;
      const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
      const ChunkSize: Integer; const ConSettings: PZConSettings);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; override;
    procedure WriteLob; override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);

    function Clone(Empty: Boolean = False): IZBlob; override;
  end;

  {** Implements external blob wrapper object for Oracle. }
  TZOracleClob = class(TZAbstractUnCachedCLob, IZOracleBlob)
  private
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FConnectionHandle: POCIEnv;
    FPlainDriver: IZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
      const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
      const LobLocator: POCILobLocator; const ChunkSize: Integer;
      const ConSettings: PZConSettings; const CodePage: Word);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; override;
    procedure WriteLob; override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);

    function Clone(Empty: Boolean = False): IZBlob; override;
  end;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} ZFastCode,
  ZMessages, ZDbcUtils, ZEncoding;

{ TZOracleAbstractResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a Oracle plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a Oracle specific query handle.
}
constructor TZOracleAbstractResultSet.Create(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);

  FStmtHandle := StmtHandle;
  FErrorHandle := ErrorHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FConnection := Statement.GetConnection as IZOracleConnection;
  FChunkSize := Statement.GetChunkSize;

  Open;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZOracleAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
var
  CurrentVar: PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  if (ColumnIndex <= InvalidDbcIndex) or (ColumnIndex > FOutVars.AllocNum{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
{$ENDIF}

  CurrentVar := @FOutVars.Variables[ColumnIndex];
  Result := (CurrentVar.Indicator < 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetAnsiRec(ColumnIndex: Integer): TZAnsiRec;
var
  SQLVarHolder: PZSQLVar;
  Len: Integer;
begin
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  Result.Len := 0;
  Result.P := nil;
  if not LastWasNull then
    with SQLVarHolder^ do
    case TypeCode of
      SQLT_INT:
        begin
          FRawTemp := IntToRaw(PLongInt(Data)^);
          Result.P := Pointer(FRawTemp);
          Result.Len := {$IFDEF WITH_INLINE}System.Length(FRawTemp){$ELSE}{%H-}PLongInt(NativeUInt(FRawTemp) - 4)^{$ENDIF};
        end;
      SQLT_FLT:
        begin
          FRawTemp := FloatToSQLRaw(PDouble(Data)^);
          Result.P := Pointer(FRawTemp);
          Result.Len := {$IFDEF WITH_INLINE}System.Length(FRawTemp){$ELSE}{%H-}PLongInt(NativeUInt(FRawTemp) - 4)^{$ENDIF};
        end;
      SQLT_STR:
        begin
          Len := DataSize;
          if DataType = SQLT_AFC then //Ansi fixed char
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
          Result.P := SQLVarHolder.Data;
          Result.Len := Len;
        end;
      SQLT_LVB, SQLT_LVC, SQLT_BIN:
        begin
          Result.P := PAnsiChar(SQLVarHolder.Data) + SizeOf(Integer);
          Result.Len := PInteger(SQLVarHolder.Data)^;
        end;
      SQLT_DAT, SQLT_TIMESTAMP:
        begin
          FRawTemp := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
            ConSettings^.ReadFormatSettings, False);
          Result.P := Pointer(FRawTemp);
          Result.Len := {$IFDEF WITH_INLINE}System.Length(FRawTemp){$ELSE}{%H-}PLongInt(NativeUInt(FRawTemp) - 4)^{$ENDIF};
        end;
      SQLT_BLOB, SQLT_CLOB:
        begin
          Blob := GetBlob(ColumnIndex);
          Result.P := Blob.GetBuffer;
          Result.Len := Blob.Length;
        end;
      else
        raise Exception.Create('Missing OCI Type?');
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  SQLVarHolder: PZSQLVar;
  AnsiRec: TZAnsiRec;
begin
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
    with SQLVarHolder^ do
    case TypeCode of
      SQLT_INT: Result := IntToRaw(PLongInt(Data)^);
      SQLT_UIN: Result := IntToRaw(PLongWord(Data)^);
      SQLT_FLT: Result := FloatToSQLRaw(PDouble(Data)^);
      SQLT_STR:
        begin
          AnsiRec.Len := DataSize;
          AnsiRec.P := Data;
          if DataType = SQLT_AFC then //Ansi fixed char
            while (PAnsiChar(Data)+AnsiRec.Len-1)^ = ' ' do Dec(AnsiRec.Len); //omit trailing spaces
          Result := ConSettings^.ConvFuncs.ZAnsiRecToUTF8(AnsiRec, ConSettings^.ClientCodePage^.CP)
        end;
      SQLT_LVB, SQLT_LVC, SQLT_BIN:
        ZSetString(PAnsiChar(Data), DataSize, Result);
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
          ConSettings^.ReadFormatSettings, False);
      SQLT_BLOB:
        begin
          Blob := GetBlob(ColumnIndex);
          ZSetString(PAnsiChar(Blob.GetBuffer), Blob.Length, Result);
          Blob := nil;
        end;
      SQLT_CLOB:
        GetBlob(ColumnIndex).GetUTF8String;
      else
        raise Exception.Create('Missing OCI Type?');
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := GetAnsiRec(ColumnIndex).P;
end;

{**
  Gets a holder for SQL output variable.
  @param ColumnIndex an index of the column to read.
  @returns an output variable holder or <code>nil</code> if column is empty.
}
function TZOracleAbstractResultSet.GetSQLVarHolder(ColumnIndex: Integer): PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Result := @FOutVars.Variables[ColumnIndex];
  LastWasNull := (Result.Indicator < 0) or (Result.Data = nil);
  if LastWasNull then
    Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>String</code>.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @param SQLVarHolder a reference to SQL variable holder or <code>nil</code>
    to force retrieving the variable.
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetAsStringValue(ColumnIndex: Integer;
  SQLVarHolder: PZSQLVar): RawByteString;
var
  Blob: IZBlob;
begin
  if SQLVarHolder = nil then
    SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if SQLVarHolder <> nil then
    case SQLVarHolder.TypeCode of
      SQLT_INT:
        Result := IntToRaw(PLongInt(SQLVarHolder.Data)^);
      SQLT_FLT:
        Result := FloatToSQLRaw(PDouble(SQLVarHolder.Data)^);
      SQLT_STR:
        ZSetString(PAnsiChar(SQLVarHolder.Data), SQLVarHolder.DataSize, Result);
      SQLT_LVB, SQLT_LVC, SQLT_BIN:
        ZSetString(PAnsiChar(SQLVarHolder.Data) + SizeOf(Integer),
          PInteger(SQLVarHolder.Data)^, Result);
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
          ConSettings^.ReadFormatSettings, False);
      SQLT_BLOB, SQLT_CLOB:
        begin
          Blob := GetBlob(ColumnIndex);
          Result := Blob.GetString;
        end;
    end
  else
    Result := '';
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>DateTime</code>.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @param SQLVarHolder a reference to SQL variable holder or <code>nil</code>
    to force retrieving the variable.
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetAsDateTimeValue(const SQLVarHolder: PZSQLVar): TDateTime;
var
  Status: Integer;
  Year: SmallInt;
  Month, Day: Byte;
  Hour, Minute, Second: Byte;
  Millis: Integer;
  Connection: IZOracleConnection;
begin
  if SQLVarHolder <> nil then
    if SQLVarHolder.TypeCode = SQLT_DAT then
      Result := OraDateToDateTime(SQLVarHolder.Data)
    else
    begin
      Connection := GetStatement.GetConnection as IZOracleConnection;
      if SQLVarHolder.ColType in [stDate, stTimestamp] then
      begin
        Status := FPlainDriver.DateTimeGetDate(
          Connection.GetConnectionHandle,
          FErrorHandle, PPOCIDescriptor(SQLVarHolder.Data)^,
          Year{%H-}, Month{%H-}, Day{%H-});
        // attention : this code handles all timestamps on 01/01/0001 as a pure time value
        // reason : oracle doesn't have a pure time datatype so all time comparisons compare
        //          TDateTime values on 30 Dec 1899 against oracle timestamps on 01 januari 0001 (negative TDateTime)
        if (Status = OCI_SUCCESS) then
          if (Year and Month and Day = 1) then
            Result := 0
          else
            Result := EncodeDate(Year, Month, Day)
        else
          Result := 0;
      end
      else
        Result := 0;
      if SQLVarHolder.ColType in [stTime, stTimestamp] then
      begin
        Status := FPlainDriver.DateTimeGetTime(
          Connection.GetConnectionHandle,
          FErrorHandle, PPOCIDescriptor(SQLVarHolder.Data)^,
          Hour{%H-}, Minute{%H-}, Second{%H-}, Millis{%H-});
        if Status = OCI_SUCCESS then
          Result := Result + EncodeTime(
            Hour, Minute, Second, Millis div 1000000);
      end;
    end
  else
    Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := GetAsStringValue(ColumnIndex, nil);
end;

{**
  Gets the final object of a type/named-collection/nested-table,array

  @param obj the parent-object
  @return the Object which contains the final object descriptor
}
function TZOracleAbstractResultSet.GetFinalObject(Obj: POCIObject): POCIObject;
begin
  if Obj.is_final_type = 1 then
    Result := Obj
  else
    Result := GetFinalObject(Obj.next_subtype); //recursive call
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZOracleAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := False
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^ <> 0;
        SQLT_UIN:
          Result := PLongWord(Data)^ <> 0;
        SQLT_FLT:
          Result := Trunc(PDouble(Data)^) <> 0;
        SQLT_STR:
          Result := StrToBoolEx(PAnsiChar(Data), True, DataType = SQLT_AFC);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := False;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]) <> 0;
        SQLT_BLOB, SQLT_CLOB:
          Result := StrToBoolEx(PAnsiChar(GetBlob(ColumnIndex).GetBuffer));
      else
        Result := False;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := Trunc(PDouble(Data)^);
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            ZSetString(PAnsiChar(Data), Len, FRawTemp);
            Result := RawToIntDef(FRawTemp, 0);
          end
          else //#0 terminated string / no trailing spaces
            Result := RawToIntDef(Data, 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]));
        SQLT_BLOB, SQLT_CLOB:
          Result := RawToIntDef(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := Trunc(PDouble(Data)^);
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            ZSetString(PAnsiChar(Data), Len, FRawTemp);
            Result := RawToInt64Def(FRawTemp, 0);
          end
          else //#0 terminated string / no trailing spaces
            Result := RawToInt64Def(Data, 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]));
        SQLT_BLOB, SQLT_CLOB:
          Result := RawToInt64Def(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetULong(ColumnIndex: Integer): UInt64;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := Trunc(PDouble(Data)^);
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            ZSetString(PAnsiChar(Data), Len, FRawTemp);
            Result := RawToUInt64Def(FRawTemp, 0);
          end
          else //#0 terminated string / no trailing spaces
            Result := RawToUInt64Def(Data, 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]));
        SQLT_BLOB, SQLT_CLOB:
            Result := RawToUInt64Def(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := PDouble(Data)^;
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, Len);
          end
          else //#0 terminated string / no trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, DataSize);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_BLOB, SQLT_CLOB:
          Result := SqlStrToFloatDef(GetBlob(ColumnIndex).GetString, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := PDouble(Data)^;
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, Len);
          end
          else //#0 terminated string / no trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, DataSize);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_BLOB, SQLT_CLOB:
          Result := SqlStrToFloatDef(GetBlob(ColumnIndex).GetString, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var Len: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := PDouble(Data)^;
        SQLT_STR:
          if DataType = SQLT_AFC then //Ansi fixed char
          begin
            Len := DataSize;
            while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, Len);
          end
          else //#0 terminated string / no trailing spaces
            Result := SqlStrToFloatDef(PAnsiChar(Data), 0, DataSize);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_BLOB, SQLT_CLOB:
          Result := SqlStrToFloatDef(GetBlob(ColumnIndex).GetString, 0);
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(GetAsStringValue(ColumnIndex, nil));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var 
  Len: Cardinal;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(Data)^);
        SQLT_STR:
          begin
            Len := DataSize;
            if DataType = SQLT_AFC then //Ansi fixed char
              while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
              Result := RawSQLDateToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed{%H-}));
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]));
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
              RawSQLTimeStampToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var 
  Len: Integer;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := Frac(PDouble(Data)^);
        SQLT_STR:
          begin
            Len := DataSize;
            if DataType = SQLT_AFC then //Ansi fixed char
              while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            if (PAnsiChar(Data)+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := Frac(RawSQLTimeStampToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed{%H-}));
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT:
          Result := 0;
        SQLT_TIMESTAMP:
          Result := Frac(GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]));
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            if (PAnsiChar(Blob.GetBuffer)+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := Frac(RawSQLTimeStampToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed{%H-}));
            LastWasNull := Result = 0;
          end;
      else
        Result := 0;
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZOracleAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var 
  Len: Cardinal;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  with FOutVars.Variables[ColumnIndex] do
  begin
    LastWasNull := (Indicator < 0) or (Data = nil);
    if LastWasNull then
      Result := 0
    else
      case TypeCode of
        SQLT_INT:
          Result := PLongInt(Data)^;
        SQLT_UIN:
          Result := PLongWord(Data)^;
        SQLT_FLT:
          Result := PDouble(Data)^;
        SQLT_STR:
          begin
            Len := DataSize;
            if DataType = SQLT_AFC then //Ansi fixed char
              while (PAnsiChar(Data)+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            if (PAnsiChar(Data)+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
                Result := RawSQLTimeStampToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(Data, Len, ConSettings^.ReadFormatSettings, Failed);
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(@FOutVars.Variables[ColumnIndex]);
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            if (PAnsiChar(Blob.GetBuffer)+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - LongWord(Blob.Length)) <= 4 then
                Result := RawSQLTimeStampToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(Blob.GetBuffer, Blob.Length, ConSettings^.ReadFormatSettings, Failed);
            LastWasNull := Result = 0;
          end;
      else
        Result := 0;
      end;
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>IZResultSet</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>IZResultSet</code> object representing the SQL
    <code>IZResultSet</code> value in the specified column
}
function TZOracleAbstractResultSet.GetDataSet(ColumnIndex: Integer): IZDataSet;
var
  CurrentVar: PZSQLVar;
  type_Ref: POCIRef;
  //tdo: POCIType;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  GetSQLVarHolder(ColumnIndex);
  CurrentVar := @FOutVars.Variables[ColumnIndex];
  Result := nil;
  if CurrentVar.TypeCode = SQLT_NTY then
    if CurrentVar.Indicator >= 0 then
    begin
      if CurrentVar._Obj.is_final_type = 1 then
        // here we've the final object lets's read it to test it
        // later we only need the reference-pointers to create a new dataset
      else
      begin
         //http://cpansearch.perl.org/src/TIMB/DBD-Oracle-1.26/oci8.c

        //create a temporary object
        type_ref := nil;
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectNew(FConnection.GetConnectionHandle,
            FConnection.GetErrorHandle, FConnection.GetContextHandle,
            OCI_TYPECODE_REF, nil, nil, OCI_DURATION_DEFAULT, TRUE, @type_ref),
          lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
        //Get the type reference
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectGetTypeRef(FConnection.GetConnectionHandle,
            FConnection.GetErrorHandle, CurrentVar._Obj.obj_value, type_Ref),
          lcOther, 'OCIObjectGetTypeRef(obj_value)', ConSettings);

        //Now let's get the new tdo
        //Excptions????????
        {CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.TypeByRef(FConnection.GetConnectionHandle,
            FConnection.GetErrorHandle, type_ref, OCI_DURATION_DEFAULT,
            OCI_TYPEGET_ALL, @tdo),
          lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);}
        //free the temporary object
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectFree(FConnection.GetConnectionHandle,
            FConnection.GetErrorHandle, type_ref, ub2(0)),
          lcOther, 'ObjectFree()', ConSettings);
      end;


      {CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.ResultSetToStmt(CurrentVar._Object,
          FErrorHandle), lcOther, 'Nested Table to Stmt handle', ConSettings);
      Result := CreateOracleResultSet(FPlainDriver, GetStatement,
        'Fetch Nested Table', CurrentVar._Object, FErrorHandle);}
    end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZOracleAbstractResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  CurrentVar: PZSQLVar;
  LobLocator: POCILobLocator;
  RawTemp: RawByteString;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  GetSQLVarHolder(ColumnIndex);
  CurrentVar := @FOutVars.Variables[ColumnIndex];
  if CurrentVar.TypeCode in [SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE] then
  begin
    if CurrentVar.Indicator >= 0 then
      LobLocator := PPOCIDescriptor(CurrentVar.Data)^
    else
      LobLocator := nil;

    if CurrentVar.TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
      Result := TZOracleBlob.Create(FPlainDriver, nil, 0, FConnection.GetContextHandle,
        FConnection.GetErrorHandle, LobLocator, FChunkSize, ConSettings)
    else
      Result := TZOracleClob.Create(FPlainDriver, nil, 0,
        FConnection.GetConnectionHandle, FConnection.GetContextHandle,
        FConnection.GetErrorHandle, LobLocator, FChunkSize, ConSettings,
        ConSettings^.ClientCodePage^.CP);
    (Result as IZOracleBlob).ReadLob; //nasty: we've got only one descriptor if we fetch the rows. Loading on demand isn't possible

  end
  else
    if CurrentVar.TypeCode=SQLT_NTY then
      Result := TZAbstractBlob.CreateWithStream(nil)
    else
    begin
      if CurrentVar.Indicator >= 0 then
      begin
        try
          if CurrentVar.TypeCode in [SQLT_LVB, SQLT_LVC, SQLT_BIN] then
            Result := TZAbstractBlob.CreateWithData(PAnsiChar(CurrentVar.Data) + SizeOf(Integer), PInteger(CurrentVar.Data)^)
          else
          begin
            RawTemp := GetAsStringValue(ColumnIndex, CurrentVar);
            Result := TZAbstractClob.CreateWithData(PAnsiChar(RawTemp),
              Length(RawTemp), ConSettings^.ClientCodePage^.CP, ConSettings);
          end;
        finally
        end;
      end
      else
        Result := TZAbstractBlob.CreateWithStream(nil);
    end;
end;

{ TZOracleResultSet }

{**
  Opens this recordset.
}
procedure TZOracleResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  Connection: IZOracleConnection;
  CurrentVar: PZSQLVar;
  ColumnCount: ub4;
  TempColumnName: PAnsiChar;
  TempColumnNameLen, CSForm: Integer;
  FConnectionHandle: POCIEnv;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FStmtHandle) or not Assigned(FErrorHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  Connection := GetStatement.GetConnection as IZOracleConnection;
  FConnectionHandle := Connection.GetConnectionHandle;

  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.StmtExecute(Connection.GetContextHandle, FStmtHandle,
    FErrorHandle, 1, 0, nil, nil, OCI_DESCRIBE_ONLY), lcExecute, 'OCIStmtExecute', ConSettings);

  { Resize SQLVERS structure if needed }
  FPlainDriver.AttrGet(FStmtHandle, OCI_HTYPE_STMT, @ColumnCount, nil,
    OCI_ATTR_PARAM_COUNT, FErrorHandle);
  AllocateOracleSQLVars(FOutVars, ColumnCount);

  { Allocates memory for result set }
  for I := 1 to FOutVars.AllocNum do
  begin
    CurrentVar := @FOutVars.Variables[I{$IFDEF GENERIC_INDEX}-1{$ENDIF}];
    CurrentVar.Handle := nil;

    FPlainDriver.ParamGet(FStmtHandle, OCI_HTYPE_STMT, FErrorHandle,
      CurrentVar.Handle, I);
    FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
      @CurrentVar.DataSize, nil, OCI_ATTR_DATA_SIZE, FErrorHandle);
    FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
      @CurrentVar.DataType, nil, OCI_ATTR_DATA_TYPE, FErrorHandle);
    CurrentVar.Scale := 0;
    CurrentVar.Precision := 0;

    case CurrentVar.DataType of
      SQLT_CHR, SQLT_VCS, SQLT_AFC, SQLT_AVC, SQLT_STR, SQLT_VST:
        CurrentVar.ColType := stString;
      SQLT_NUM: //unsigned char[21] see: http://docs.oracle.com/cd/B19306_01/appdev.102/b14250/oci03typ.htm
        begin
          FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
            @CurrentVar.Precision, nil, OCI_ATTR_PRECISION, FErrorHandle);
          FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
            @CurrentVar.Scale, nil, OCI_ATTR_SCALE, FErrorHandle);

          {by default convert number to double}
          CurrentVar.ColType := stDouble;
          if (CurrentVar.Scale = 0) and (CurrentVar.Precision <> 0) then
          begin //No digits found, but possible signed or not/overrun of converiosn? No way to find this out -> just use a "save" type
            case CurrentVar.Precision of
              0..2: CurrentVar.ColType := stShort; // -128..127
              3..4: CurrentVar.ColType := stSmall; // -32768..32767
              5..9: CurrentVar.ColType := stInteger; // -2147483648..2147484647
              10..19: CurrentVar.ColType := stLong; // -9223372036854775808..9223372036854775807
              //skip 20 can be UInt64 or Int64  assume Double values instead
              21: CurrentVar.ColType := stULong; //0..18446744073709551615
            end;
          end
          else if (CurrentVar.Scale <= 4) and (CurrentVar.Precision > 0) and
            (CurrentVar.Precision <= 19) then
            CurrentVar.ColType := stCurrency;
        end;
      SQLT_BFLOAT, SQLT_BDOUBLE, SQLT_IBFLOAT, SQLT_IBDOUBLE:
        CurrentVar.ColType := stDouble;
      SQLT_INT, _SQLT_PLI:
        CurrentVar.ColType := stInteger;
      SQLT_LNG, SQLT_LVC:
        CurrentVar.ColType := stAsciiStream;
      SQLT_RID, SQLT_RDD:
        begin
          CurrentVar.ColType := stString;
          CurrentVar.DataSize := 20;
        end;
      SQLT_DAT, SQLT_DATE:
        { oracle DATE precission - 1 second}
        CurrentVar.ColType := stTimestamp;
      SQLT_TIME, SQLT_TIME_TZ:
        CurrentVar.ColType := stTime;
      SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ:
        CurrentVar.ColType := stTimestamp;
      SQLT_BIN, SQLT_LBI:
        begin
          if CurrentVar.DataSize = 0 then
            CurrentVar.ColType := stBinaryStream
          else
            CurrentVar.ColType := stBytes;
        end;
      SQLT_CLOB:
        begin
          CurrentVar.ColType := stAsciiStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
        end;
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          CurrentVar.ColType := stBinaryStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
        end;
      SQLT_NTY:
        begin
          CurrentVar.ColType := stDataSet;
          CurrentVar.TypeCode := CurrentVar.DataType;

          CurrentVar._Obj := DescribeObject(FplainDriver, FConnection,
            CurrentVar.Handle, FStmtHandle, nil, 0);
          if CurrentVar._Obj.col_typecode = OCI_TYPECODE_TABLE then
            CurrentVar.ColType := stDataSet
          else if CurrentVar._Obj.col_typecode = OCI_TYPECODE_VARRAY then
            CurrentVar.ColType := stArray
          else //more possible types
            CurrentVar.ColType := stBinaryStream;
        end;
      else
        CurrentVar.ColType := stUnknown;
    end;

    if CurrentVar.ColType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
    begin
      CurrentVar.CodePage := ConSettings^.ClientCodePage^.CP;
      if (ConSettings.CPType = cCP_UTF16) then
        case CurrentVar.ColType of
          stString: CurrentVar.ColType := stUnicodeString;
          stAsciiStream: if not ( CurrentVar.DataType in [SQLT_LNG]) then
            CurrentVar.ColType := stUnicodeStream;
        end;
    end
    else
      CurrentVar.CodePage := High(Word);


    {now reserve mem and override OCI given typecodes for bindings }
    InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
      CurrentVar.ColType, CurrentVar.TypeCode, CurrentVar.DataSize);

    if CurrentVar.ColType in [stString, stUnicodeString] then
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineByPos(FStmtHandle, CurrentVar.Define,
        FErrorHandle, I, CurrentVar.Data, CurrentVar.Length, CurrentVar.TypeCode,
        @CurrentVar.Indicator, @CurrentVar.DataSize, nil, OCI_DEFAULT), lcExecute, 'OCIDefineByPos', ConSettings)
    else
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineByPos(FStmtHandle, CurrentVar.Define,
        FErrorHandle, I, CurrentVar.Data, CurrentVar.Length, CurrentVar.TypeCode,
        @CurrentVar.Indicator, nil, nil, OCI_DEFAULT), lcExecute, 'OCIDefineByPos', ConSettings);
    if CurrentVar.DataType=SQLT_NTY then
    begin
      //second step: http://www.csee.umbc.edu/portal/help/oracle8/server.815/a67846/obj_bind.htm
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineObject(CurrentVar.Define, FErrorHandle, CurrentVar._Obj.tdo,
           @CurrentVar._Obj.obj_value, nil, nil, nil), lcExecute, 'OCIDefineObject', ConSettings);
    end;
  end;

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := FirstDbcIndex to FOutVars.AllocNum{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    CurrentVar := @FOutVars.Variables[I];
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';
      ColumnCodePage := CurrentVar.CodePage;
      TempColumnName := nil;
      FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
        @TempColumnName, @TempColumnNameLen, OCI_ATTR_NAME, FErrorHandle);
      if TempColumnName <> nil then
        ColumnLabel := BufferToStr(TempColumnName, TempColumnNameLen);

      ColumnDisplaySize := 0;
      AutoIncrement := False;
      Signed := True;
      Nullable := ntNullable;

      ColumnType := CurrentVar.ColType;
      Scale := CurrentVar.Scale;
      if (ColumnType in [stString, stUnicodeString]) then
      begin
        FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
          @ColumnDisplaySize, nil, OCI_ATTR_DISP_SIZE, FErrorHandle);
        FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
          @CSForm, nil, OCI_ATTR_CHARSET_FORM, FErrorHandle);
        if CSForm = SQLCS_NCHAR then //AL16UTF16 or AL16UTF16LE?? We should determine the NCHAR set on connect
          ColumnDisplaySize := ColumnDisplaySize div 2;
        Precision := GetFieldSize(ColumnType, ConSettings, ColumnDisplaySize,
          ConSettings.ClientCodePage^.CharWidth);
      end
      else
        if (ColumnType = stBytes ) then
          Precision := CurrentVar.DataSize
        else
          Precision := CurrentVar.Precision;
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZOracleResultSet.Close;
begin
  if assigned(FOutVars) then // else no statement anyways
    FreeOracleSQLVars(FPlainDriver, FOutVars, FConnection.GetConnectionHandle, FErrorHandle, ConSettings);
  { prepared statement own handles, so dont free them }
  FStmtHandle := nil;
  inherited Close;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZOracleResultSet.Next: Boolean;
var
  Status: Integer;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (FStmtHandle = nil) then
    Exit;


  {if Self.ResultSetType = rtForwardOnly then}
    if RowNo = 0 then
      Status := FPlainDriver.StmtExecute(FConnection.GetContextHandle, FStmtHandle,
        FErrorHandle, 1, 0, nil, nil, OCI_DEFAULT)
    else
      Status := FPlainDriver.StmtFetch2(FStmtHandle, FErrorHandle,
        1, OCI_FETCH_NEXT, 0, OCI_DEFAULT)
  ;{else}
{ http://docs.oracle.com/cd/B10501_01/appdev.920/a96584/oci04sql.htm#420200
Increasing Scrollable Cursor Performance
Response time is improved if you use OCI client-side prefetch buffers.
After calling OCIStmtExecute() for a scrollable cursor, call OCIStmtFetch2()
using OCI_FETCH_LAST to obtain the size of the result set.
Then set OCI_ATTR_PREFETCH_ROWS to about 20% of that size, and set OCI_PREFETCH_MEMORY
if the result set uses a large amount of memory.}
    {if RowNo = 0 then
    begin
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.StmtExecute(FConnection.GetContextHandle, FStmtHandle,
          FErrorHandle, 1, 0, nil, nil, OCI_STMT_SCROLLABLE_READONLY),
            lcOther, 'FETCH ROW', ConSettings);
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.StmtFetch2(FStmtHandle, FErrorHandle,
          1, OCI_FETCH_LAST, 0, OCI_DEFAULT), lcOther, 'FETCH ROW', ConSettings);
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.AttrGet(FStmtHandle, OCI_HTYPE_STMT, @Status, nil,
          OCI_ATTR_CURRENT_POSITION, FErrorHandle), lcOther, 'FETCH ROW', ConSettings);
      if Status > 0 then
      begin
        MaxRows := Status;
        Status := Max(100, Status div 4); //setting 25%
        FPlainDriver.AttrSet(FStmtHandle, OCI_HTYPE_STMT, @Status, SizeOf(ub4),
          OCI_ATTR_PREFETCH_ROWS, FErrorHandle);
        Status := FPlainDriver.StmtFetch2(FStmtHandle, FErrorHandle,
          1, OCI_FETCH_FIRST, Status, OCI_DEFAULT);
      end;
    end
    else
      Status := FPlainDriver.StmtFetch2(FStmtHandle, FErrorHandle,
        1, OCI_FETCH_NEXT, 0, OCI_DEFAULT);}

  if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'FETCH ROW', ConSettings);

  if Status in [OCI_SUCCESS, OCI_SUCCESS_WITH_INFO] then
  begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end
  else
  begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;
end;

{ TZOracleCallableResultSet }
function TZOracleCallableResultSet.PrepareOracleOutVars(InVars: PZSQLVars;
  const OracleParams: TZOracleParams): PZSQLVars;
var
  I, J: Integer;
begin
  J := 0;
  for i := 0 to High(OracleParams) do
    if OracleParams[I].pType in [2,3,4] then
      Inc(J);

  Result := nil;
  AllocateOracleSQLVars(Result, J);
  SetLength(FFieldNames, J);

  {$IFDEF GENERIC_INDEX}
  for I := 0 to High(OracleParams) do
  {$ELSE}
  for I := 1 to Length(OracleParams) do
  {$ENDIF}
  begin
    J := OracleParams[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pOutIndex;
    if OracleParams[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pType in [2,3,4] then //ptInOut, ptOut, ptResult
    begin
      Result.Variables[J].ColType := InVars.Variables[I].ColType;
      Result.Variables[J].TypeCode := InVars.Variables[I].TypeCode;
      Result.Variables[J].DataSize := InVars.Variables[I].DataSize;
      Result.Variables[J].Length := InVars.Variables[I].Length;
      GetMem(Result.Variables[J].Data, InVars.Variables[I].Length);
      Move(InVars.Variables[I].Data^, Result.Variables[J].Data^, InVars.Variables[I].Length);
      FFieldNames[J{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := OracleParams[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pName;
    end;
  end;
end;

procedure TZOracleCallableResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
begin
  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := FirstDbcIndex to FOutVars.AllocNum {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    CurrentVar := @FOutVars.Variables[I];
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';

      ColumnLabel := FFieldNames[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
      ColumnDisplaySize := 0;
      AutoIncrement := False;
      Signed := True;
      Nullable := ntNullable;

      ColumnType := CurrentVar.ColType;
      Scale := CurrentVar.Scale;

      {Reset the column type which can be changed by user before}
      if CurrentVar.ColType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        if (ColumnType = stUnicodeStream) and not ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stAsciiStream;
        if (ColumnType = stAsciiStream) and ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stUnicodeStream;
        if (ColumnType = stUnicodeString) and not ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stString;
        if (ColumnType = stString) and ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stUnicodeString;
      end
      else
        ColumnInfo.ColumnCodePage := High(Word);

      if ( ColumnType in [stString, stUnicodeString] ) then
      begin
        ColumnDisplaySize := CurrentVar.DataSize;
        Precision := GetFieldSize(ColumnType, ConSettings, CurrentVar.DataSize,
          ConSettings.ClientCodePage^.CharWidth);
      end
      else
        Precision := CurrentVar.Precision;
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a Oracle plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a Oracle specific query handle.
}
constructor TZOracleCallableResultSet.Create(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError; OutVars: PZSQLVars; const OracleParams: TZOracleParams);
begin
  FOutVars := PrepareOracleOutVars(OutVars, OracleParams);
  inherited Create(PlainDriver, Statement, SQL, StmtHandle, ErrorHandle);
  FConnection := Statement.GetConnection as IZOracleConnection;
  MaxRows := 1;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZOracleCallableResultSet.Close;
var
  I: Integer;
  CurrentVar: PZSQLVar;
begin
  if FOutVars <> nil then
  begin
    { Frees allocated memory for output variables }
    for I := 1 to FOutVars.AllocNum do
    begin
      CurrentVar := @FOutVars.Variables[I];
      if CurrentVar.Data <> nil then
      begin
        FreeMem(CurrentVar.Data);
        CurrentVar.Data := nil;
      end;
    end;
    FreeMem(FOutVars);
  end;
  FOutVars := nil;
  inherited Close;
end;

function TZOracleCallableResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo >= MaxRows) then
    Exit;
  RowNo := LastRowNo + 1;
  Result := True;
end;

{ TZOracleBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a Oracle connection reference.
  @param LobLocator an Oracle lob locator reference.
  @param BlobType a blob type.
}
constructor TZOracleBlob.Create(const PlainDriver: IZOraclePlainDriver;
  const Data: Pointer; const Size: Int64; const ContextHandle: POCISvcCtx;
  const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
  const ChunkSize: Integer; const ConSettings: PZConSettings);
begin
  inherited CreateWithData(Data, Size);
  FContextHandle := ContextHandle;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FTemporary := False;
  FChunkSize := ChunkSize;
  FConSettings := ConSettings;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleBlob.Destroy;
begin
  if FTemporary then
    FPlainDriver.LobFreeTemporary(FContextHandle, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleBlob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.LobCreateTemporary(FContextHandle, FErrorHandle,
      FLobLocator, OCI_DEFAULT, OCI_DEFAULT, OCI_TEMP_BLOB, False,
      OCI_DURATION_DEFAULT),
    lcOther, 'Create Large Object', FConSettings);

  FTemporary := True;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZOracleBlob.ReadLob;
const
  MemDelta = 1 shl 12;  // read page (2^...)
var
  Status: Integer;
  Buf: PByteArray;
  ReadNumBytes, Offset, Cap: ub4;
begin
  if not Updated and (FLobLocator <> nil)
    and (BlobData = nil) and (not FTemporary) then
  begin
    { Opens a large object or file for read. }
    Status := FPlainDriver.LobOpen(FContextHandle, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Open Large Object', FConSettings);
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        repeat
          {Calc new progressive by 1/8 and aligned by MemDelta capacity for buffer}
          Cap := (Offset + (Offset shr 3) + 2 * MemDelta - 1) and not (MemDelta - 1);
          ReallocMem(Buf, Cap);
          ReadNumBytes := Cap - Offset;

          Status := FPlainDriver.LobRead(FContextHandle, FErrorHandle,
            FLobLocator, ReadNumBytes, Offset + 1, @Buf[Offset], ReadNumBytes,
            nil, nil, 0, SQLCS_IMPLICIT);
          CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Read Large Object', FConSettings);
          if ReadNumBytes > 0 then
            Inc(Offset, ReadNumBytes);
        until Offset < Cap;
      except
        FreeMem(Buf);
        Buf := nil;
        raise;
      end;
    finally
      { Closes large object or file. }
      Status := FPlainDriver.LobClose(FContextHandle,FErrorHandle, FLobLocator);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
    end;
    { Assigns data }
    InternalSetData(Buf, Offset);
  end;
  inherited ReadLob;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZOracleBlob.WriteLob;
begin
  OraWriteLob(FPlainDriver, BlobData, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, True, nil);
end;

procedure TZOracleBlob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, Len, True, nil);
end;

{**
  Replace data in blob by AData without copy (keep ref of AData)
}
procedure TZOracleBlob.InternalSetData(AData: Pointer; ASize: Integer);
begin
  InternalClear;
  BlobData := AData;
  BlobSize := ASize;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZOracleBlob.Clone(Empty: Boolean = False): IZBlob;
begin
  if Empty then
    Result := TZOracleBlob.Create(FPlainDriver, nil, 0, FContextHandle,
      FErrorHandle, FLobLocator, FChunkSize, FConSettings)
  else
    Result := TZOracleBlob.Create(FPlainDriver, BlobData, Length, FContextHandle,
      FErrorHandle, FLobLocator, FChunkSize, FConSettings);
end;

{ TZOracleClob }

constructor TZOracleClob.Create(const PlainDriver: IZOraclePlainDriver;
  const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  const ConSettings: PZConSettings; const CodePage: Word);
begin
  if ZCompatibleCodePages(CodePage, zCP_UTF16) then
    inherited CreateWithData(Data, Size div 2, ConSettings)
  else
    inherited CreateWithData(Data, Size, CodePage, ConSettings);
  FContextHandle := ContextHandle;
  FConnectionHandle := ConnectionHandle;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FTemporary := False;
  FChunkSize := ChunkSize;
end;

destructor TZOracleClob.Destroy;
begin
  if FTemporary then
    FPlainDriver.LobFreeTemporary(FContextHandle, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleClob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.LobCreateTemporary(FContextHandle, FErrorHandle, FLobLocator,
      OCI_DEFAULT, OCI_DEFAULT, OCI_TEMP_CLOB, False, OCI_DURATION_DEFAULT),
    lcOther, 'Create Large Object', FConSettings);

  FTemporary := True;
end;

procedure TZOracleClob.ReadLob;
var
  Status: Integer;
  Buf: PByteArray;
  ReadNumChars, Offset: ub4;
  csfrm: ub1;

  procedure DoRead(const csid: ub2; const csfrm: ub1);
  begin
    ReadNumChars := 0;
    Status := FPlainDriver.LobRead(FContextHandle,FErrorHandle, FLobLocator,
      ReadNumChars, Offset + 1, Buf, FChunkSize, nil, nil, csid, csfrm);
    if ReadNumChars > 0 then
    begin
      Inc(Offset, ReadNumChars);
      ReallocMem(FBlobData, Offset+1);
      System.Move(Buf^, (PAnsiChar(FBlobData)+NativeUInt(OffSet-ReadNumChars))^, ReadNumChars);
    end;
  end;
begin
  if not Updated and (FLobLocator <> nil) and (BlobData = nil) and (not FTemporary) then
  begin
    { Opens a large object or file for read. }
    Status := FPlainDriver.LobOpen(FContextHandle, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Open Large Object', FConSettings);
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        GetMem(Buf, FChunkSize+1);
        Offset := 0;
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.LobCharSetForm(FConnectionHandle, FErrorHandle,
            FLobLocator, @csfrm),
          lcOther, 'Determine LOB SCFORM', FConSettings); //need to determine proper CharSet-Form
        DoRead(FConSettings^.ClientCodePage^.ID, csfrm);
        while Status = OCI_NEED_DATA do
          DoRead(FConSettings^.ClientCodePage^.ID, csfrm);
        CheckOracleError(FPlainDriver, FErrorHandle,
          Status, lcOther, 'Read Large Object', FConSettings);
        BlobSize := OffSet+1; //oracle includes #0 terminator
        if OffSet = 0 then ReallocMem(FBlobData, 1);
        (PAnsiChar(FBlobData)+NativeUInt(OffSet))^ := #0;
      except
        FreeMem(Buf);
        Buf := nil;
        raise;
      end;
    finally
      { Closes large object or file. }
      Status := FPlainDriver.LobClose(FContextHandle, FErrorHandle, FLobLocator);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
      if Buf <> nil then
        FreeMem(Buf);
    end;
  end;
  inherited ReadLob;
end;

procedure TZOracleClob.WriteLob;
begin
  GetPAnsiChar(FConSettings^.ClientCodePage^.CP); //convert if required
  OraWriteLob(FPlainDriver, BlobData, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, False, FConSettings);
end;

procedure TZOracleClob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  if Buffer = nil then
    OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
      FChunkSize, Len, False, FConSettings)
  else
    OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
      FChunkSize, Len+1, False, FConSettings);
end;

function TZOracleClob.Clone(Empty: Boolean = False): IZBlob;
begin
  if Empty then
    Result := TZOracleClob.Create(FPlainDriver, nil, 0,
      FConnectionHandle, FContextHandle, FErrorHandle, FLobLocator, FChunkSize,
      FConSettings, CurrentCodePage)
  else
    Result := TZOracleClob.Create(FPlainDriver, BlobData, Length,
      FConnectionHandle, FContextHandle, FErrorHandle, FLobLocator, FChunkSize,
      FConSettings, CurrentCodePage);
end;

end.
