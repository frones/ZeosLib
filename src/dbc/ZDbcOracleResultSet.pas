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
  Classes, SysUtils, Types, ZSysUtils, ZDbcIntfs,
  ZDbcResultSet, ZPlainOracleDriver, ZDbcResultSetMetadata, ZDbcLogging,
  ZCompatibility, ZDbcOracleUtils, ZPlainOracleConstants;

type

  {** Implements Oracle ResultSet. }
  TZOracleAbstractResultSet = class(TZAbstractResultSet)
  private
    FSQL: string;
    FStmtHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FOutVars: PZSQLVars;
  protected
    function GetSQLVarHolder(ColumnIndex: Integer): PZSQLVar;
    function GetAsStringValue(ColumnIndex: Integer;
      SQLVarHolder: PZSQLVar): ZAnsiString;
    function GetAsLongIntValue(ColumnIndex: Integer;
      SQLVarHolder: PZSQLVar): LongInt;
    function GetAsDoubleValue(ColumnIndex: Integer;
      SQLVarHolder: PZSQLVar): Double;
    function GetAsDateTimeValue(ColumnIndex: Integer;
      SQLVarHolder: PZSQLVar): TDateTime;
    function InternalGetString(ColumnIndex: Integer): ZAnsiString; override;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError);

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): ShortInt; override;
    function GetShort(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
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
    function PrepareOracleOutVars(Statement: IZStatement; InVars: PZSQLVars;
      FieldNames: TStringDynArray; ParamTypes,
      FFunctionResultOffsets: array of shortInt): PZSQLVars;
  protected
    procedure Open; override;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Statement: IZStatement; SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; OutVars: PZSQLVars; FieldNames: TStringDynArray;
      ParamTypes, FunctionResultOffsets: array of shortInt);
    procedure Close; override;
    function Next: Boolean; override;
  End;

  {** Represents an interface, specific for Oracle blobs. }
  IZOracleBlob = interface(IZBlob)
    ['{3D861AAC-B263-42F1-B359-2A188D1D986A}']
    function GetLobLocator: POCILobLocator;
    procedure CreateBlob;
    procedure ReadBlob;
    procedure WriteBlob;
  end;

  {** Implements external blob wrapper object for Oracle. }
  TZOracleBlob = class(TZAbstractBlob, IZOracleBlob)
  private
    FHandle: IZConnection;
    FLobLocator: POCILobLocator;
    FPlainDriver: IZOraclePlainDriver;
    FBlobType: TZSQLType;
    FTemporary: Boolean;
    FChunkSize: Integer;
  protected
    procedure InternalSetData(AData: Pointer; ASize: Integer);
  public
    constructor Create(PlainDriver: IZOraclePlainDriver; Data: Pointer;
      Size: Integer; Handle: IZConnection; LobLocator: POCILobLocator;
      BlobType: TZSQLType; ChunkSize: Integer);
    destructor Destroy; override;

    function GetLobLocator: POCILobLocator;
    procedure CreateBlob;
    procedure ReadBlob;
    procedure WriteBlob;

    function Length: LongInt; override;
    function IsEmpty: Boolean; override;
    function Clone: IZBlob; override;

    function GetString: ZAnsiString; override;
    function GetBytes: TByteDynArray; override;
    function GetStream: TStream; override;
  end;

implementation

uses
  Math, ZMessages, ZDbcOracle, ZDbcUtils, ZEncoding;

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

  FSQL := SQL;
  FStmtHandle := StmtHandle;
  FErrorHandle := ErrorHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;

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
  if (ColumnIndex <=0) or (ColumnIndex > FOutVars.ActualNum) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;
{$ENDIF}

  CurrentVar := @FOutVars.Variables[ColumnIndex];
  Result := (CurrentVar.Indicator < 0);
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
  SQLVarHolder: PZSQLVar): ZAnsiString;
var
  OldSeparator: Char;
  Blob: IZBlob;
begin
  if SQLVarHolder = nil then
    SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if SQLVarHolder <> nil then
  begin
    case SQLVarHolder.TypeCode of
      SQLT_INT:
        Result := AnsiString(IntToStr(PLongInt(SQLVarHolder.Data)^));
      SQLT_FLT:
        begin
          OldSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
          {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
          Result := AnsiString(FloatToSqlStr(PDouble(SQLVarHolder.Data)^));
          {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldSeparator;
        end;
      SQLT_STR:
        Result := StrPas(PAnsiChar(SQLVarHolder.Data));
      SQLT_LVB, SQLT_LVC:
        begin
          Result := AnsiString(BufferToStr(PAnsiChar(SQLVarHolder.Data) + SizeOf(Integer),
            PInteger(SQLVarHolder.Data)^));
        end;
      SQLT_DAT, SQLT_TIMESTAMP:
        begin
          Result := AnsiString(DateTimeToAnsiSQLDate(
            GetAsDateTimeValue(ColumnIndex, SQLVarHolder)));
        end;
      SQLT_BLOB, SQLT_CLOB:
        begin
          Blob := GetBlob(ColumnIndex);
          Result := Blob.GetString;
        end;
    end;
  end
  else
    Result := '';
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>LongInt</code>.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @param SQLVarHolder a reference to SQL variable holder or <code>nil</code>
    to force retrieving the variable.
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetAsLongIntValue(ColumnIndex: Integer;
  SQLVarHolder: PZSQLVar): LongInt;
begin
  if SQLVarHolder = nil then
    SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if SQLVarHolder <> nil then
  begin
    case SQLVarHolder.TypeCode of
      SQLT_INT:
        Result := PLongInt(SQLVarHolder.Data)^;
      SQLT_FLT:
        Result := Trunc(PDouble(SQLVarHolder.Data)^);
      else
      begin
        Result := Trunc(SqlStrToFloatDef(
          GetAsStringValue(ColumnIndex, SQLVarHolder), 0));
      end;
    end;
  end
  else
    Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Double</code>.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @param SQLVarHolder a reference to SQL variable holder or <code>nil</code>
    to force retrieving the variable.
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0.0</code>
}
function TZOracleAbstractResultSet.GetAsDoubleValue(ColumnIndex: Integer;
  SQLVarHolder: PZSQLVar): Double;
begin
  if SQLVarHolder = nil then
    SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if SQLVarHolder <> nil then
  begin
    case SQLVarHolder.TypeCode of
      SQLT_INT:
        Result := PLongInt(SQLVarHolder.Data)^;
      SQLT_FLT:
        Result := PDouble(SQLVarHolder.Data)^;
      else
      begin
        Result := SqlStrToFloatDef(
          GetAsStringValue(ColumnIndex, SQLVarHolder), 0);
      end;
    end;
  end
  else
    Result := 0;
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
function TZOracleAbstractResultSet.GetAsDateTimeValue(ColumnIndex: Integer;
  SQLVarHolder: PZSQLVar): TDateTime;
var
  Status: Integer;
  Year: SmallInt;
  Month, Day: Byte;
  Hour, Minute, Second: Byte;
  Millis: Integer;
  Connection: IZOracleConnection;
begin
  if SQLVarHolder = nil then
    SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if SQLVarHolder <> nil then
  begin
    case SQLVarHolder.TypeCode of
      SQLT_DAT:
        Result := OraDateToDateTime(SQLVarHolder.Data);
      SQLT_TIMESTAMP:
        begin
          Connection := GetStatement.GetConnection as IZOracleConnection;
          if SQLVarHolder.ColType in [stDate, stTimestamp] then
          begin
            Status := FPlainDriver.DateTimeGetDate(
              Connection.GetConnectionHandle,
              FErrorHandle, PPOCIDescriptor(SQLVarHolder.Data)^,
              Year, Month, Day);
            // attention : this code handles all timestamps on 01/01/0001 as a pure time value
            // reason : oracle doesn't have a pure time datatype so all time comparisons compare
            //          TDateTime values on 30 Dec 1899 against oracle timestamps on 01 januari 0001 (negative TDateTime)
            if (Status = OCI_SUCCESS) and
               ((Year <> 1) or (Month <> 1) or (Day <> 1)) then
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
              Hour, Minute, Second, Millis);
            if Status = OCI_SUCCESS then
            begin
              Millis := Round(Millis / 1000000);
              if Millis >= 1000 then Millis := 999;
                Result := Result + EncodeTime(
                  Hour, Minute, Second, Millis);
            end;
          end;
        end;
      else
      begin
        Result := AnsiSQLDateToDateTime(
          String(GetAsStringValue(ColumnIndex, SQLVarHolder)));
      end;
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
function TZOracleAbstractResultSet.InternalGetString(ColumnIndex: Integer): ZAnsiString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := GetAsStringValue(ColumnIndex, nil);
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
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Temp := String(GetAsStringValue(ColumnIndex, nil));
  Result := (StrToIntDef(Temp, 0) <> 0) or StrToBoolEx(Temp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := ShortInt(GetAsLongIntValue(ColumnIndex, nil));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := SmallInt(GetAsLongIntValue(ColumnIndex, nil));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := Integer(GetAsLongIntValue(ColumnIndex, nil));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := Trunc(GetAsDoubleValue(ColumnIndex, nil));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := GetAsDoubleValue(ColumnIndex, nil);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := GetAsDoubleValue(ColumnIndex, nil);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := GetAsDoubleValue(ColumnIndex, nil);
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
function TZOracleAbstractResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := Trunc(GetAsDateTimeValue(ColumnIndex, nil));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := Frac(GetAsDateTimeValue(ColumnIndex, nil));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := GetAsDateTimeValue(ColumnIndex, nil);
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
  Connection: IZOracleConnection;
  CurrentVar: PZSQLVar;
  LobLocator: POCILobLocator;
  Stream: TStream;
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

    Connection := GetStatement.GetConnection as IZOracleConnection;
    Result := TZOracleBlob.Create(FPlainDriver, nil, 0, Connection, LobLocator,
      CurrentVar.ColType, GetStatement.GetChunkSize);
    (Result as IZOracleBlob).ReadBlob;

  end
  else
    if CurrentVar.TypeCode=SQLT_NTY then
      Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection)
    else
    begin
      if CurrentVar.Indicator >= 0 then
      begin
        Stream := nil;
        try
          Stream := TStringStream.Create(
            GetAsStringValue(ColumnIndex, CurrentVar));
          Result := TZAbstractBlob.CreateWithStream(Stream, GetStatement.GetConnection);
        finally
          if Assigned(Stream) then
            Stream.Free;
        end;
      end
      else
        Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection);
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
  Status: Integer;
  Connection: IZOracleConnection;
  CurrentVar: PZSQLVar;
  ColumnCount: ub4;
  TempColumnName: PAnsiChar;
  TempColumnNameLen: Integer;
  ptype,addr_tdo:pointer;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FStmtHandle) or not Assigned(FErrorHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  Connection := GetStatement.GetConnection as IZOracleConnection;

  Status := FPlainDriver.StmtExecute(Connection.GetContextHandle, FStmtHandle,
    FErrorHandle, 1, 0, nil, nil, OCI_DESCRIBE_ONLY);
  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, FSQL);

  { Resize SQLVERS structure if needed }
  FPlainDriver.AttrGet(FStmtHandle, OCI_HTYPE_STMT, @ColumnCount, nil,
    OCI_ATTR_PARAM_COUNT, FErrorHandle);
  AllocateOracleSQLVars(FOutVars, ColumnCount);
  FOutVars.ActualNum := ColumnCount;

  { Allocates memory for result set }
  for I := 1 to FOutVars.ActualNum do
  begin
    CurrentVar := @FOutVars.Variables[I];
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
        begin
          CurrentVar.ColType := stString
        end;
      SQLT_NUM:
        begin
          FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
            @CurrentVar.Precision, nil, OCI_ATTR_PRECISION, FErrorHandle);
          FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
            @CurrentVar.Scale, nil, OCI_ATTR_SCALE, FErrorHandle);

          {by default convert number to double}
          CurrentVar.ColType := stDouble;
          if (CurrentVar.Scale = 0) and (CurrentVar.Precision <> 0) then
          begin
            if CurrentVar.Precision <= 2 then
              CurrentVar.ColType := stByte
            else if CurrentVar.Precision <= 4 then
              CurrentVar.ColType := stShort
            else if CurrentVar.Precision <= 9 then
              CurrentVar.ColType := stInteger
            else if CurrentVar.Precision <= 19 then
              CurrentVar.ColType := stLong;
          end
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
        CurrentVar.ColType := stBinaryStream;
      SQLT_CLOB:
        begin
          CurrentVar.ColType := stAsciiStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
        end;
      SQLT_BLOB:
        begin
          CurrentVar.ColType := stBinaryStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
        end;
      SQLT_BFILEE, SQLT_CFILEE:
        begin
          CurrentVar.ColType := stBinaryStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
        end;
      SQLT_NTY:
        begin
          CurrentVar.ColType := stBinaryStream;
          CurrentVar.TypeCode := CurrentVar.DataType;
          CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.AttrGet(CurrentVar.Handle, OCI_DTYPE_PARAM,
                 @ptype, nil, OCI_ATTR_REF_TDO, FErrorHandle)
            ,lcExecute, FSQL);
          CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.ObjectPin(Connection.GetConnectionHandle, FErrorHandle,
              ptype, nil, OCI_PIN_ANY, OCI_DURATION_SESSION, pub2(OCI_LOCK_NONE),
                @addr_tdo) ,lcExecute, FSQL);
        end;
      else
        CurrentVar.ColType := stUnknown;
    end;

    if (ConSettings.CPType = cCP_UTF16) then
      case CurrentVar.ColType of
        stString: CurrentVar.ColType := stUnicodeString;
        stAsciiStream: if not ( CurrentVar.DataType in [SQLT_LNG]) then
          CurrentVar.ColType := stUnicodeStream;
      end;


    InitializeOracleVar(FPlainDriver, Connection, CurrentVar,
      CurrentVar.ColType, CurrentVar.TypeCode, CurrentVar.DataSize);

    Status := FPlainDriver.DefineByPos(FStmtHandle, CurrentVar.Define,
      FErrorHandle, I, CurrentVar.Data, CurrentVar.Length, CurrentVar.TypeCode,
      @CurrentVar.Indicator, nil, nil, OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, FSQL);
    if CurrentVar.DataType=SQLT_NTY then
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineObject(CurrentVar.Define, FErrorHandle, addr_tdo,
           @CurrentVar._Object,nil,nil,nil), lcExecute, FSQL);
  end;

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 1 to FOutVars.ActualNum do
  begin
    CurrentVar := @FOutVars.Variables[I];
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';

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
var
  ps: IZPreparedStatement;
begin
  if assigned(FOutVars) then // else no statement anyways
    FreeOracleSQLVars(FPlainDriver, FOutVars, (GetStatement.GetConnection as IZOracleConnection).GetConnectionHandle, FErrorHandle);
  { prepared statement own handles, so dont free them }
  if not Supports(GetStatement, IZPreparedStatement, ps) then
     FreeOracleStatementHandles(FPlainDriver, FStmtHandle, FErrorHandle);
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
  Connection: IZOracleConnection;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) then
    Exit;

  if RowNo = 0 then
  begin
    Connection := GetStatement.GetConnection as IZOracleConnection;
    Status := FPlainDriver.StmtExecute(Connection.GetContextHandle, FStmtHandle,
      FErrorHandle, 1, 0, nil, nil, OCI_DEFAULT);
  end
  else
  begin
    Status := FPlainDriver.StmtFetch(FStmtHandle, FErrorHandle,
      1, OCI_FETCH_NEXT, OCI_DEFAULT);
  end;
  if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'FETCH ROW');

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
function TZOracleCallableResultSet.PrepareOracleOutVars(Statement: IZStatement;
  InVars: PZSQLVars; FieldNames: TStringDynArray;
  ParamTypes, FFunctionResultOffsets: array of shortInt): PZSQLVars;
var
  I, J, K, OutParamCount, ResultOffSet: Integer;
  Connection: IZConnection;

  procedure PrepareVar;
  begin
    Result.Variables[J].ColType := InVars.Variables[K].ColType;
    Result.Variables[J].TypeCode := InVars.Variables[K].TypeCode;
    Result.Variables[J].DataSize := InVars.Variables[K].DataSize;
    Result.Variables[J].Length := InVars.Variables[K].Length;
    GetMem(Result.Variables[J].Data, InVars.Variables[K].Length);
    Move(InVars.Variables[K].Data^, Result.Variables[J].Data^, InVars.Variables[K].Length);
    SetLength(FFieldNames, J);
    FFieldNames[j-1] := FieldNames[I-1];
  end;
begin
  Connection := Statement.GetConnection;

  OutParamCount := 0;
  for i := 0 to InVars.ActualNum-1 do
    if ParamTypes[I] in [2,3,4] then
      Inc(OutParamCount);

  Result := nil;
  AllocateOracleSQLVars(Result, OutParamCount);
  Result.ActualNum := OutParamCount;

  J := 0;
  K := 0;
  ResultOffSet := 0;
  for i := 1 to InVars.ActualNum do
  begin
    Inc(K);
    if ( Length(FFunctionResultOffsets) > 0 ) and  (FFunctionResultOffsets[ResultOffSet] = i -1) then
      Inc(K);
    if ParamTypes[i-1] in [2,3] then
    begin
      inc(J);
      PrepareVar;
    end
    else
      if ParamTypes[i-1] = 4 then //ptResult
      begin
        inc(J);
        K := FFunctionResultOffsets[ResultOffSet]+1;
        PrepareVar;
        Inc(ResultOffSet);
      end;
  end;
end;

procedure TZOracleCallableResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
  Connection: IZConnection;
begin
  Connection := GetStatement.GetConnection;
  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 1 to FOutVars.ActualNum do
  begin
    CurrentVar := @FOutVars.Variables[I];
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';

      ColumnLabel := FFieldNames[i-1];
      ColumnDisplaySize := 0;
      AutoIncrement := False;
      Signed := True;
      Nullable := ntNullable;

      ColumnType := CurrentVar.ColType;
      Scale := CurrentVar.Scale;

      {Reset the column type which can be changed by user before}
      if (ColumnType = stUnicodeStream) and not ( Connection.GetConSettings.CPType = cCP_UTF16) then
        ColumnType := stAsciiStream;
      if (ColumnType = stAsciiStream) and ( Connection.GetConSettings.CPType = cCP_UTF16) then
        ColumnType := stUnicodeStream;
      if (ColumnType = stUnicodeString) and not ( Connection.GetConSettings.CPType = cCP_UTF16) then
        ColumnType := stString;
      if (ColumnType = stString) and ( Connection.GetConSettings.CPType = cCP_UTF16) then
        ColumnType := stUnicodeString;

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
  ErrorHandle: POCIError; OutVars: PZSQLVars; FieldNames: TStringDynArray;
  ParamTypes, FunctionResultOffsets: array of shortInt);
begin
  FOutVars := PrepareOracleOutVars(Statement, OutVars, FieldNames, ParamTypes,
                FunctionResultOffsets);
  inherited Create(PlainDriver, Statement, SQL, StmtHandle, ErrorHandle);
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
    for I := 1 to FOutVars.ActualNum do
    begin
      CurrentVar := @FOutVars.Variables[I];
      if CurrentVar.Data <> nil then
      begin
        CurrentVar.DupData := nil;
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
constructor TZOracleBlob.Create(PlainDriver: IZOraclePlainDriver;
  Data: Pointer; Size: Integer; Handle: IZConnection;
  LobLocator: POCILobLocator; BlobType: TZSQLType; ChunkSize: Integer);
begin
  inherited CreateWithData(Data, Size, Handle);
  FHandle := Handle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FTemporary := False;
  FBlobType := BlobType;
  FChunkSize := ChunkSize;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleBlob.Destroy;
var
  Connection: IZOracleConnection;
begin
  if FTemporary then
  begin
    Connection := FHandle as IZOracleConnection;
    FPlainDriver.LobFreeTemporary(Connection.GetContextHandle,
      Connection.GetErrorHandle, FLobLocator);
  end;

  inherited Destroy;
end;

{**
  Gets the lob locator reference.
  @return the lob locator reference.
}
function TZOracleBlob.GetLobLocator: POCILobLocator;
begin
  Result := FLobLocator;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleBlob.CreateBlob;
var
  Status: Integer;
  Connection: IZOracleConnection;
  TempBlobType: ub1;
begin
  Connection := FHandle as IZOracleConnection;

  if FBlobType = stBinaryStream then
    TempBlobType := OCI_TEMP_BLOB
  else
    TempBlobType := OCI_TEMP_CLOB;

  Status := FPlainDriver.LobCreateTemporary(Connection.GetContextHandle,
    Connection.GetErrorHandle, FLobLocator, OCI_DEFAULT, OCI_DEFAULT,
    TempBlobType, False, OCI_DURATION_DEFAULT);
  CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
    Status, lcOther, 'Create Large Object');

  FTemporary := True;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZOracleBlob.ReadBlob;
const
  MemDelta = 1 shl 12;  // read page (2^...)
var
  Status: Integer;
  Buf: PByteArray;
  ReadNumBytes, ReadNumChars, Offset, Cap: ub4;
  Connection: IZOracleConnection;
  AnsiTemp: ZAnsiString;
  Stream: TStream;

  procedure DoRead;
  begin
    FillChar(Buf^, FChunkSize+1, #0);
    ReadNumChars := 0;
    Status := FPlainDriver.LobRead(Connection.GetContextHandle,
      Connection.GetErrorHandle, FLobLocator, ReadNumChars, Offset + 1,
      Buf, FChunkSize, nil, nil, Connection.GetClientCodePageInformations^.ID, SQLCS_IMPLICIT);
    if ReadNumChars > 0 then
    begin
      Inc(Offset, ReadNumChars);
      AnsiTemp := AnsiTemp+PAnsiChar(Buf);
    end;
  end;
begin
  if not Updated and (FLobLocator <> nil)
    and (BlobData = nil) and (not FTemporary) then
  begin
    Connection := FHandle as IZOracleConnection;

    { Opens a large object or file for read. }
    Status := FPlainDriver.LobOpen(Connection.GetContextHandle,
      Connection.GetErrorHandle, FLobLocator, OCI_LOB_READONLY);
    CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
      Status, lcOther, 'Open Large Object');
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        case FBlobType of
          stBinaryStream:
            repeat //BLOB
              {Calc new progressive by 1/8 and aligned by MemDelta capacity for buffer}
              Cap := (Offset + (Offset shr 3) + 2 * MemDelta - 1) and not (MemDelta - 1);
              ReallocMem(Buf, Cap);
              ReadNumBytes := Cap - Offset;

              Status := FPlainDriver.LobRead(Connection.GetContextHandle,
                Connection.GetErrorHandle, FLobLocator, ReadNumBytes, Offset + 1,
                @Buf[Offset], ReadNumBytes, nil, nil, 0, SQLCS_IMPLICIT);
              CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
                Status, lcOther, 'Read Large Object');
              if ReadNumBytes > 0 then
                Inc(Offset, ReadNumBytes);
            until Offset < Cap;
          else //CLob
          begin
            GetMem(Buf, FChunkSize+1);
            AnsiTemp := '';
            Offset := 0;
            DoRead;
            if Status = OCI_NEED_DATA then
              while Status = OCI_NEED_DATA do
                DoRead;
            CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
              Status, lcOther, 'Read Large Object');
            if FBlobType = stUnicodeStream then
              Stream := ZEncoding.GetValidatedUnicodeStream(AnsiTemp, Connection.GetConSettings, True)
            else
              Stream := TStringStream.Create(GetValidatedAnsiString(AnsiTemp, Connection.GetConSettings, True));
            ReallocMem(Buf, Stream.Size);
            Move(TMemoryStream(Stream).Memory^, PAnsichar(Buf)^, Stream.Size);
            OffSet := Stream.Size;
            Stream.Free;
            FDecoded := FBlobType = stUnicodeStream;
          end;
        end;
      except
        FreeMem(Buf);
        raise;
      end;
    finally
      { Closes large object or file. }
      Status := FPlainDriver.LobClose(Connection.GetContextHandle,
        Connection.GetErrorHandle, FLobLocator);
      CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
        Status, lcOther, 'Close Large Object');
    end;
    { Assigns data }
    InternalSetData(Buf, Offset);
  end;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZOracleBlob.WriteBlob;
var
  Status: sword;
  Connection: IZOracleConnection;
  ContentSize, OffSet: ub4;

  function DoWrite(AOffSet: ub4; AChunkSize: ub4; APiece: ub1): sword;
  var
    AContentSize: ub4;
  begin
    if Self.FBlobType = stBinaryStream then
    begin
      AContentSize := ContentSize;
      Result := FPlainDriver.LobWrite(Connection.GetContextHandle,
        Connection.GetErrorHandle, FLobLocator, AContentSize, AOffSet,
        (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece, nil, nil, 0, SQLCS_IMPLICIT);
    end
    else
    begin
      if ContentSize > 0 then
        AContentSize := AChunkSize div Connection.GetClientCodePageInformations^.CharWidth
      else
      begin
        AContentSize := ContentSize;
        AChunkSize := Connection.GetClientCodePageInformations^.CharWidth;
      end;

      Result := FPlainDriver.LobWrite(Connection.GetContextHandle,
        Connection.GetErrorHandle, FLobLocator, AContentSize, AOffSet,
        (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece, nil, nil, Connection.GetClientCodePageInformations^.ID, SQLCS_IMPLICIT);
    end;
    ContentSize := AContentSize;
    inc(OffSet, AChunkSize);
  end;
begin
  Connection := FHandle as IZOracleConnection;

  { Opens a large object or file for read. }
  Status := FPlainDriver.LobOpen(Connection.GetContextHandle,
    Connection.GetErrorHandle, FLobLocator, OCI_LOB_READWRITE);
  CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
    Status, lcOther, 'Open Large Object');

  { Checks for empty blob.}
  { This test doesn't use IsEmpty because that function does allow for zero length blobs}
  if (BlobSize > 0) then
  begin
    if BlobSize > FChunkSize then
    begin
      OffSet := 0;
      ContentSize := 0;

      Status := DoWrite(1, FChunkSize, OCI_FIRST_PIECE);
      if Status <> OCI_NEED_DATA then
        CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
          Status, lcOther, 'Write Large Object');

      if (BlobSize - OffSet) > FChunkSize then
        while (BlobSize - OffSet) > FChunkSize do //take care there is room left for LastPiece
        begin
          Status := DoWrite(offset, FChunkSize, OCI_NEXT_PIECE);
          if Status <> OCI_NEED_DATA then
            CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
              Status, lcOther, 'Write Large Object');

        end;
      Status := DoWrite(offset, BlobSize - OffSet, OCI_LAST_PIECE);
    end
    else
    begin
      ContentSize := BlobSize;
      Status := FPlainDriver.LobWrite(Connection.GetContextHandle,
        Connection.GetErrorHandle, FLobLocator, ContentSize, 1,
        BlobData, BlobSize, OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
    end;
  end
  else
  begin
    Status := FPlainDriver.LobTrim(Connection.GetContextHandle,
      Connection.GetErrorHandle, FLobLocator, 0);
  end;
  CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
    Status, lcOther, 'Write Large Object');

  { Closes large object or file. }
  Status := FPlainDriver.LobClose(Connection.GetContextHandle,
    Connection.GetErrorHandle, FLobLocator);
  CheckOracleError(FPlainDriver, Connection.GetErrorHandle,
    Status, lcOther, 'Close Large Object');
end;

{**
  Replace data in blob by AData without copy (keep ref of AData)
}
procedure TZOracleBlob.InternalSetData(AData: Pointer; ASize: Integer);
begin
  Clear;
  BlobData := AData;
  BlobSize := ASize;
end;

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZOracleBlob.IsEmpty: Boolean;
begin
  ReadBlob;
  Result := inherited IsEmpty;
end;

function TZOracleBlob.Length: LongInt;
begin
  ReadBlob;
  Result := inherited Length;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZOracleBlob.Clone: IZBlob;
begin
  Result := TZOracleBlob.Create(FPlainDriver, BlobData, BlobSize,
    FHandle, FLobLocator, FBlobType, FChunkSize);
end;

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZOracleBlob.GetStream: TStream;
begin
  ReadBlob;
  Result := inherited GetStream;
end;

{**
  Gets the string from the stored data.
  @return a string which contains the stored data.
}
function TZOracleBlob.GetString: ZAnsiString;
begin
  ReadBlob;
  Result := inherited GetString;
end;

{**
  Gets the byte buffer from the stored data.
  @return a byte buffer which contains the stored data.
}
function TZOracleBlob.GetBytes: TByteDynArray;
begin
  ReadBlob;
  Result := inherited GetBytes;
end;

end.
