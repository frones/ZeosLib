{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZDbcBeginnerStatement;

{$INCLUDE ZDbc.inc}

interface

uses Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} FmtBCD, SysUtils,
  ZCompatibility, ZVariant,
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZDbcUtils;

type
  TZAbstractBeginnerPreparedStatement = class(TZAbstractStatement, IZPreparedStatement)
  private
    FInParamValues: TZVariantDynArray;
    FInParamTypes: TZSQLTypeArray;
    FInParamDefaultValues: TStringDynArray;
    FInitialArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FClientVariantManger: IZClientVariantManager;
  protected
    FInParamCount: Integer;
    FCachedQueryRaw: TRawByteStringDynArray;
    FCachedQueryUni: TUnicodeStringDynArray;
    FNCharDetected: TBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    FIsPraparable: Boolean;
    function GetClientVariantManger: IZClientVariantManager;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;

    procedure SetInParamCount(const NewParamCount: Integer); virtual;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); virtual;
    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: String = EmptyRaw);
    function GetInParamLogValue(Value: TZVariant): RawByteString;
    function GetOmitComments: Boolean; virtual;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; virtual;

    property InParamValues: TZVariantDynArray read FInParamValues write FInParamValues;
    property InParamTypes: TZSQLTypeArray read FInParamTypes write FInParamTypes;
    property InParamDefaultValues: TStringDynArray
      read FInParamDefaultValues write FInParamDefaultValues;
    property InParamCount: Integer read FInParamCount write FInParamCount;
    property ClientVarManager: IZClientVariantManager read FClientVariantManger;
    property CachedQueryRaw: TRawByteStringDynArray read FCachedQueryRaw;
    property CachedQueryUni: TUnicodeStringDynArray read FCachedQueryUni;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
    property IsNCharIndex: TBooleanDynArray read FNCharDetected;
    property IsPreparable: Boolean read FIsPraparable;
    property ArrayCount: ArrayLenInt read FInitialArrayCount;
    procedure RaiseUnsupportedException;
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; virtual;
    function ExecuteUpdatePrepared: Integer; virtual;
    function ExecutePrepared: Boolean; virtual;

    procedure BeforeClose; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string);

    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType); virtual;
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean); virtual;
    procedure SetByte(ParameterIndex: Integer; Value: Byte); virtual;
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt); virtual;
    procedure SetWord(ParameterIndex: Integer; Value: Word); virtual;
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt); virtual;
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal); virtual;
    procedure SetInt(ParameterIndex: Integer; Value: Integer); virtual;
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64); virtual;
    procedure SetLong(ParameterIndex: Integer; const Value: Int64); virtual;
    procedure SetFloat(ParameterIndex: Integer; Value: Single); virtual;
    procedure SetDouble(ParameterIndex: Integer; const Value: Double); virtual;
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency); virtual;
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);  virtual; //AVZ
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); overload; virtual;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); overload; virtual;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); overload; virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); overload; virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); overload; virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); overload; virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); overload; virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); overload; virtual;
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant); virtual;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); virtual;
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); virtual;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      {%H-}Scale: LengthInt = 0);

    //======================================================================
    // Methods for accessing out parameters by index
    //======================================================================
    function IsNull(Index: Integer): Boolean;
    function GetBoolean(ParameterIndex: Integer): Boolean;
    function GetByte(ParameterIndex: Integer): Byte;
    function GetShort(ParameterIndex: Integer): ShortInt;
    function GetWord(ParameterIndex: Integer): Word;
    function GetSmall(ParameterIndex: Integer): SmallInt;
    function GetUInt(ParameterIndex: Integer): Cardinal;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetULong(ParameterIndex: Integer): UInt64;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double;
    function GetCurrency(ParameterIndex: Integer): Currency;
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
    procedure GetGUID(Index: Integer; var Result: TGUID);
    function GetBytes(ParameterIndex: Integer): TBytes; overload;
    function GetDate(ParameterIndex: Integer): TDateTime; overload;
    procedure GetDate(ParameterIndex: Integer; Var Result: TZDate); overload;
    function GetTime(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTime(ParameterIndex: Integer; Var Result: TZTime); overload;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTimeStamp(Index: Integer; var Result: TZTimeStamp); overload;
    function GetValue(ParameterIndex: Integer): TZVariant;

    function GetString(ParameterIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString;

    function GetBLob(ParameterIndex: Integer): IZBlob;
    function GetCLob(ParameterIndex: Integer): IZClob;

    procedure ClearParameters; virtual;

    procedure AddBatchPrepared; virtual;
    function GetMetaData: IZResultSetMetaData; virtual;
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

implementation

uses ZMessages, ZFastCode, ZSysUtils, ZEncoding,
  ZDbcResultSet;
{------------------------------------------------------------------------------}

{ TZAbstractBeginnerPreparedStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractBeginnerPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FClientVariantManger := Connection.GetClientVariantManager;
  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := SQL;
  SetInParamCount(0);
  FPrepared := False;
  FInitialArrayCount := 0;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractBeginnerPreparedStatement.Destroy;
begin
  Unprepare;
  inherited Destroy;
  ClearParameters;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractBeginnerPreparedStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractBeginnerPreparedStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractBeginnerPreparedStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractBeginnerPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractBeginnerPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractBeginnerPreparedStatement.Execute(const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Return a VariantManager which supports client encoded RawByteStrings
  @returns IZClientVariantManager
}
function TZAbstractBeginnerPreparedStatement.GetClientVariantManger: IZClientVariantManager;
begin
  Result := FClientVariantManger;
end;

function TZAbstractBeginnerPreparedStatement.GetCLob(
  ParameterIndex: Integer): IZClob;
var
  Idx: Integer;
  TempBlob: IZClob;
begin
  Idx := ParameterIndex - FirstDbcIndex;
  if (Idx) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  if (InParamValues[Idx].VType = vtInterface) and (Supports(InParamValues[Idx].VInterface, IZClob, TempBlob)) then begin
    Result := TempBlob;
  end else begin
    EZSQLException.Create('Paramter is not an IZBlob');
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractBeginnerPreparedStatement.PrepareInParameters;
begin
end;

{**
  Binds the input parameters
}
procedure TZAbstractBeginnerPreparedStatement.BindInParameters;
begin
  DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractBeginnerPreparedStatement.UnPrepareInParameters;
begin
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractBeginnerPreparedStatement.SetInParamCount(const NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FInParamValues, NewParamCount);
  SetLength(FInParamTypes, NewParamCount);
  SetLength(FInParamDefaultValues, NewParamCount);
  for I := FInParamCount to NewParamCount - 1 do
  begin
    FInParamValues[I] := NullVariant;
    FInParamTypes[I] := stUnknown;

    FInParamDefaultValues[I] := '';
  end;
  FInParamCount := NewParamCount;
end;

{**
  Sets a variant value into specified parameter.
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure TZAbstractBeginnerPreparedStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
begin
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} >= FInParamCount then
    SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamTypes[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SQLType;
  FInParamValues[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Logs a message about prepared statement event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZAbstractBeginnerPreparedStatement.LogPrepStmtMessage(Category: TZLoggingCategory;
  const Msg: String = EmptyRaw);
begin
  if DriverManager.HasLoggingListener then
    if msg <> EmptyRaw then
      DriverManager.LogMessage(Category, Connection.GetIZPlainDriver.GetProtocol, 'Statement '+{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(FStatementId)+' : '+Msg)
    else
      DriverManager.LogMessage(Category, Connection.GetIZPlainDriver.GetProtocol, 'Statement '+{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(FStatementId));
end;


function TZAbstractBeginnerPreparedStatement.GetInParamLogValue(Value: TZVariant): RawByteString;
begin
  With Value do
    case VType of
      vtNull : result := '(NULL)';
      vtBoolean : if VBoolean then result := '(TRUE)' else result := '(FALSE)';
      vtBytes : Result := GetSQLHexAnsiString(Pointer(VRawByteString), Length(VRawByteString), False);
      vtInteger : result := IntToRaw(VInteger);
      vtDouble : result := FloatToRaw(VDouble);
      vtString,
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString,
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String,
      {$ENDIF}
      vtRawByteString,
      vtUnicodeString,
      vtCharRec: result := #39 + ClientVarManager.GetAsRawByteString(Value) + #39;
      vtDateTime : result := ClientVarManager.GetAsRawByteString(Value);
      vtPointer : result := '(POINTER)';
      vtInterface : result := '(INTERFACE)';
    else
      result := '(UNKNOWN TYPE)'
    end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$IFDEF FPC}
  {$PUSH} {$WARN 5033 off : Function result does not seem to be set} // base class - result not returned intentionally
{$ENDIF}
function TZAbstractBeginnerPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAbstractBeginnerPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
procedure TZAbstractBeginnerPreparedStatement.SetDefaultValue(
  ParameterIndex: Integer; const Value: string);
begin
 if ParameterIndex >= FInParamCount then
   SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamDefaultValues[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractBeginnerPreparedStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  SetInParam(ParameterIndex, SQLType, NullVariant);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  SetInParam(ParameterIndex, stBoolean, EncodeBoolean(Value));
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  SetInParam(ParameterIndex, stByte, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  SetInParam(ParameterIndex, stShort, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  SetInParam(ParameterIndex, stWord, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  SetInParam(ParameterIndex, stSmall, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>uint</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  SetInParam(ParameterIndex, stLongWord, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetInt(ParameterIndex: Integer;
  Value: Integer);
begin
  SetInParam(ParameterIndex, stInteger, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>ulong</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  SetInParam(ParameterIndex, stULong, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  SetInParam(ParameterIndex, stLong, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  SetInParam(ParameterIndex, stFloat, EncodeDouble(Value));
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  SetInParam(ParameterIndex, stDouble, EncodeDouble(Value));
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  SetInParam(ParameterIndex, stCurrency, EncodeDouble(Value));
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetBigDecimal(
  ParameterIndex: Integer; const Value: TBCD);
begin
  SetInParam(ParameterIndex, stBigDecimal, EncodeBigDecimal(Value));
end;

{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  SetInParam(ParameterIndex, stString, EncodeCharRec(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetString(ParameterIndex: Integer;
   const Value: String);
begin
  SetInParam(ParameterIndex, stString, EncodeString(Value));
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractBeginnerPreparedStatement.SetAnsiString(ParameterIndex: Integer;
   const Value: AnsiString);
begin
  SetInParam(ParameterIndex, stString, EncodeAnsiString(Value));
end;
{$ENDIF}

{**
  Sets the designated parameter to a Java <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractBeginnerPreparedStatement.SetUTF8String(ParameterIndex: Integer;
   const Value: UTF8String);
begin
  SetInParam(ParameterIndex, stString, EncodeUTF8String(Value));
end;
{$ENDIF}
{**
  Sets the designated parameter to a Java <code>RawByteString</code> value.
  The driver dosn't converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetRawByteString(ParameterIndex: Integer;
   const Value: RawByteString);
begin
  SetInParam(ParameterIndex, stString, EncodeRawByteString(Value));
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractBeginnerPreparedStatement.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  if Value <> ResultSetConcurrency then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetConcurrency(Value);
  end;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractBeginnerPreparedStatement.SetResultSetType(Value: TZResultSetType);
begin
  if Value <> ResultSetType then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetType(Value);
  end;
end;

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetUnicodeString(ParameterIndex: Integer;
  const Value: ZWideString);
begin
  SetInParam(ParameterIndex, stUnicodeString, EncodeUnicodeString(Value));
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  SetInParam(ParameterIndex, stBytes, EncodeBytes(Value));
end;

procedure TZAbstractBeginnerPreparedStatement.SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Len);
  Move(Value^, Bytes[0], Len);
  SetInParam(ParameterIndex, stBytes, EncodeBytes(Bytes));
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetGUID(ParameterIndex: Integer; const Value: TGUID);
begin
  SetInParam(ParameterIndex, stGUID, EncodeGUID(Value));
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stDate, EncodeDateTime(Value));
end;

procedure TZAbstractBeginnerPreparedStatement.SetDate(ParameterIndex: Integer; const Value: TZDate);
begin
  SetInParam(ParameterIndex, stDate, EncodeZDate(Value));
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTime, EncodeDateTime(Value));
end;

procedure TZAbstractBeginnerPreparedStatement.SetTime(ParameterIndex: Integer; const Value: TZTime);
begin
  SetInParam(ParameterIndex, stTime, EncodeZTime(Value));
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTimestamp, EncodeDateTime(Value));
end;

procedure TZAbstractBeginnerPreparedStatement.SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp);
begin
  SetInParam(ParameterIndex, stTimestamp, EncodeZTimeStamp(Value));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large ASCII value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code>. Data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from ASCII to the database char format.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the Java input stream that contains the ASCII parameter value
  @param length the number of bytes in the stream
}
procedure TZAbstractBeginnerPreparedStatement.SetAsciiStream(
  ParameterIndex: Integer; const Value: TStream);
var
  MyMemoryStream: TMemoryStream;
  NeedToRelease: Boolean;
  OriginalPos: Int64;
begin
  NeedToRelease := False;
  if (Value is TMemoryStream) then begin
    MyMemoryStream := Value as TMemoryStream;
  end else begin
    MyMemoryStream := TMemoryStream.Create;
    NeedToRelease := true;
  end;
  try
    if NeedToRelease then begin
      OriginalPos := Value.Position;
      Value.Seek(0, soBeginning);
      MyMemoryStream.CopyFrom(Value, Value.Size);
      Value.Seek(OriginalPos, soBeginning);
    end;

    if MyMemoryStream.Memory = nil
    then SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(PEmptyAnsiString, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings))
    else
      if ConSettings^.ClientCodePage^.CP = zCP_UTF16 then
        SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(MyMemoryStream.Memory, Value.Size, DefaultSystemCodePage, ConSettings))
      else
        SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(MyMemoryStream.Memory, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings));
  finally
    if NeedToRelease then
      FreeAndNil(MyMemoryStream);
  end;
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large UNICODE value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from UNICODE to the database char format.
  The byte format of the Unicode stream must be Java UTF-8, as
  defined in the Java Virtual Machine Specification.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the UNICODE parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetUnicodeStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(PEmptyUnicodeString, Value.Size, ConSettings))
  else SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_UTF16, ConSettings));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetBinaryStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  SetBlob(ParameterIndex, stBinaryStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets a blob object for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the java blob object.
}
procedure TZAbstractBeginnerPreparedStatement.SetBlob(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  if not (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
    raise EZSQLException.Create(SWrongTypeForBlobParameter);
  SetInParam(ParameterIndex, SQLType, EncodeInterface(Value));
end;

{**
  Sets a variant value for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the variant value.
}
procedure TZAbstractBeginnerPreparedStatement.SetValue(ParameterIndex: Integer;
  const Value: TZVariant);
var
  SQLType: TZSQLType;
  TempBlob: IZBlob;
begin
  case Value.VType of
    vtBoolean: SQLType := stBoolean;
    vtInteger: SQLType := stLong;
    vtUInteger: SQLType := stULong;
    vtDouble: SQLType := stDouble;
    vtBigDecimal: SQLType := stBigDecimal;
    vtUnicodeString: SQLType := stUnicodeString;
    vtDateTime: SQLType := stTimestamp;
    vtBytes: SQLType := stBytes;
    vtArray: SQLType := TZSQLType(Value.VArray.VArrayType);
    vtInterface:
      if Supports(Value.VInterface, IZBlob, TempBlob) then
        if TempBlob.IsClob then
          SQLType := stAsciiStream
        else
          SQLType := stBinaryStream
      else
        SQLType := stString; //???
  else
    SQLType := stString;
  end;
  SetInParam(ParameterIndex, SQLType, Value);
end;

{**
  Sets the designated parameter to a <code>T???DynArray</code> value.
  The driver converts this to an SQL <code>Array of X</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the TZSQLType
  @param x the parameter value
}
procedure TZAbstractBeginnerPreparedStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
begin
  if InParamCount < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    raise EZSQLException.Create('Set Array-Value first');
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex -1;
  {$ENDIF}
  if InParamValues[ParameterIndex].VType <> vtArray then
    raise EZSQLException.Create('No Array bound before!');
  InParamValues[ParameterIndex].VArray.VIsNullArray := Pointer(Value);
  InParamValues[ParameterIndex].VArray.VIsNullArrayType := Ord(SQLType);
  InParamValues[ParameterIndex].VArray.VIsNullArrayVariantType := VariantType;
end;

{**
  Sets the designated parameter to a <code>Array of ???</code> value.
  The driver converts this to an SQL <code>Array of </code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param A dynamic array of X.
  @param SQLType the TZSQLType of the value
  @param VariantType the TZVariantType SubType of the value
}
procedure TZAbstractBeginnerPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
var
  V: TZVariant;
  {using mem entry of ZData is faster then casting and save imbelievable many codelines for all possible types!}
  ZArray: Pointer absolute Value;

  procedure AssertLength;
  var Len: ArrayLenInt;
  begin
    Len := {%H-}PArrayLenInt({%H-}NativeUInt(ZArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
    if (ParameterIndex = FirstDbcIndex) or ((ParameterIndex > FirstDbcIndex) and
       (InParamValues[ParameterIndex{$IFNDEF GENERIC_INDEX} - 2{$ELSE}-1{$ENDIF}].VArray.VArray = nil))  then
      FInitialArrayCount := Len
    else
      if Len <> FInitialArrayCount then
        raise EZSQLException.Create('Array count does not equal with initial count!');
  end;
begin
  if Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings then
  begin
    if ZArray <> nil then
      case SQLType of
        stUnknown: raise EZSQLException.Create('Invalid SQLType for Array binding!');
        stBoolean, stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong,
        stLong, stFloat, stDouble, stCurrency, stBigDecimal, stBytes, stGUID, stDate,
        stTime, stTimestamp, stAsciiStream, stUnicodeStream, stBinaryStream:
          AssertLength;
        stString:
          case VariantType of
            vtString, {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}vtUTF8String, vtRawByteString, vtCharRec:
              AssertLength
            else
              raise EZSQLException.Create('Invalid Variant-Type for String-Array binding!');
          end;
        stUnicodeString:
          case VariantType of
            vtUnicodeString, vtCharRec:
              AssertLength
            else
              raise EZSQLException.Create('Invalid Variant-Type for String-Array binding!');
          end;
        stArray:          raise EZSQLException.Create('Invalid SQL-Type for Array binding!');
        stResultSet: ;
      end;
    V.VType := vtArray;
    V.VArray.VArray := Pointer(Value);
    V.VArray.VArrayVariantType := VariantType;
    V.VArray.VArrayType := Ord(SQLType);
    V.VArray.VIsNullArray := nil;
    V.VArray.VIsNullArrayType := 0;
    V.VArray.VIsNullArrayVariantType := vtNull;
    SetInParam(ParameterIndex, SQLType, V);
  end
  else
    raise EZSQLException.Create('ArrayBindings are not supported!');
end;

// Note:
// I think this whole RegisterParameter-Stuff is plain wrong. Agreed - we need
// to know the parameter count. But why should it be the duty of the user to specify
// data types here? The DBMS should know what parameter types it returns and it
// should tell us these types. If it doesn't tell us, we still can ask GetProcedureColulmns
// to tell us.
// Why introduce another problem source here? I am going to register nulls for now
// to have an implementation.
procedure TZAbstractBeginnerPreparedStatement.RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
  ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0; Scale: LengthInt = 0);
begin
  SetInParam(ParameterIndex + FirstDbcIndex, SQLType, EncodeNull);
end;

function TZAbstractBeginnerPreparedStatement.IsNull(Index: Integer): Boolean;
begin
  if (Index - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.IsNull(InParamValues[Index]);
end;

function TZAbstractBeginnerPreparedStatement.GetBoolean(ParameterIndex: Integer): Boolean;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsBoolean(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetByte(ParameterIndex: Integer): Byte;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetShort(ParameterIndex: Integer): ShortInt;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetWord(ParameterIndex: Integer): Word;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetSmall(ParameterIndex: Integer): SmallInt;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetUInt(ParameterIndex: Integer): Cardinal;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsUInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetInt(ParameterIndex: Integer): Integer;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetULong(ParameterIndex: Integer): UInt64;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsUInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetLong(ParameterIndex: Integer): Int64;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsInteger(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetFloat(ParameterIndex: Integer): Single;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsDouble(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetDouble(ParameterIndex: Integer): Double;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsDouble(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetCurrency(ParameterIndex: Integer): Currency;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsCurrency(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

procedure TZAbstractBeginnerPreparedStatement.GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsBigDecimal(InParamValues[ParameterIndex - FirstDbcIndex], Result);
end;

procedure TZAbstractBeginnerPreparedStatement.GetGUID(Index: Integer; var Result: TGUID);
begin
  if (Index - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsGUID(InParamValues[Index - FirstDbcIndex], Result);
end;

function TZAbstractBeginnerPreparedStatement.GetBytes(ParameterIndex: Integer): TBytes;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsBytes(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetDate(ParameterIndex: Integer): TDateTime;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsDateTime(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

procedure TZAbstractBeginnerPreparedStatement.GetDate(ParameterIndex: Integer; Var Result: TZDate);
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsDate(InParamValues[ParameterIndex - FirstDbcIndex], Result);
end;

function TZAbstractBeginnerPreparedStatement.GetTime(ParameterIndex: Integer): TDateTime;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsDateTime(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

procedure TZAbstractBeginnerPreparedStatement.GetTime(ParameterIndex: Integer; Var Result: TZTime);
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsTime(InParamValues[ParameterIndex - FirstDbcIndex], Result);
end;

function TZAbstractBeginnerPreparedStatement.GetTimestamp(ParameterIndex: Integer): TDateTime;
var
  Res: TZTime;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsTime(InParamValues[ParameterIndex - FirstDbcIndex], Res);
  if not TryTimeToDateTime(Res, Result) then
    raise EZSQLException.Create('Could not convert TZTime to TDateTime');
end;

procedure TZAbstractBeginnerPreparedStatement.GetTimeStamp(Index: Integer; var Result: TZTimeStamp);
begin
  if (Index - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  SoftVarManager.GetAsTimeStamp(InParamValues[Index - FirstDbcIndex], Result);
end;

function TZAbstractBeginnerPreparedStatement.GetValue(ParameterIndex: Integer): TZVariant;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := InParamValues[ParameterIndex - FirstDbcIndex];
end;

function TZAbstractBeginnerPreparedStatement.GetString(ParameterIndex: Integer): String;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsString(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractBeginnerPreparedStatement.GetAnsiString(ParameterIndex: Integer): AnsiString;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsAnsiString(InParamValues[ParameterIndex - FirstDbcIndex]);
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZAbstractBeginnerPreparedStatement.GetUTF8String(ParameterIndex: Integer): UTF8String;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsUTF8String(InParamValues[ParameterIndex - FirstDbcIndex]);
end;
{$ENDIF}

function TZAbstractBeginnerPreparedStatement.GetRawByteString(ParameterIndex: Integer): RawByteString;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsRawByteString(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetUnicodeString(ParameterIndex: Integer): ZWideString;
begin
  if (ParameterIndex - FirstDbcIndex) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  Result := SoftVarManager.GetAsUnicodeString(InParamValues[ParameterIndex - FirstDbcIndex]);
end;

function TZAbstractBeginnerPreparedStatement.GetBLob(ParameterIndex: Integer): IZBlob;
var
  Idx: Integer;
  TempBlob: IZBlob;
begin
  Idx := ParameterIndex - FirstDbcIndex;
  if (Idx) >= Length(InParamValues) then
    raise EZSQLException.Create('Paramter index exceeds parameter count.');
  if (InParamValues[Idx].VType = vtInterface) and (Supports(InParamValues[Idx].VInterface, IZBlob, TempBlob)) then begin
    Result := TempBlob;
  end else begin
    EZSQLException.Create('Paramter is not an IZBlob');
  end;
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractBeginnerPreparedStatement.ClearParameters;
var
  I: Integer;
begin
  for I := FirstDbcIndex to FInParamCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    SetInParam(I, stUnknown, NullVariant);
    SetDefaultValue(I, '');
  end;
  SetInParamCount(0);
  FInitialArrayCount := 0;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractBeginnerPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

procedure TZAbstractBeginnerPreparedStatement.BeforeClose;
begin
  inherited BeforeClose;
    if Prepared then
      Unprepare;
end;

function TZAbstractBeginnerPreparedStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

procedure TZAbstractBeginnerPreparedStatement.Prepare;
begin
  DriverManager.LogMessage(lcPrepStmt,Self);
  PrepareInParameters;
  FPrepared := True;
end;

procedure TZAbstractBeginnerPreparedStatement.Unprepare;
begin
  if Assigned(FOpenResultSet) then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  LastResultSet := nil;
  UnPrepareInParameters;
  FPrepared := False;
  Self.FInitialArrayCount := 0;
  SetLength(FCachedQueryRaw, 0);
  SetLength(FCachedQueryUni, 0);
end;

function TZAbstractBeginnerPreparedStatement.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{**
  Adds a set of parameters to this <code>PreparedStatement</code>
  object's batch of commands.
  @see Statement#addBatch
}
procedure TZAbstractBeginnerPreparedStatement.AddBatchPrepared;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZAbstractBeginnerPreparedStatement.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
  raise EZSQLException.Create(SUnsupportedOperation);
end;

function TZAbstractBeginnerPreparedStatement.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var
  I : integer;
  LogString : String;
begin
  LogString := '';
  case Category of
    lcBindPrepStmt:
        if InParamCount = 0 then
          result := nil
        else
          begin { Prepare Log Output}
            For I := 0 to InParamCount - 1 do
              LogString := LogString + String(GetInParamLogValue(InParamValues[I])+',');
            result := CreateStmtLogEvent(Category, Logstring);
          end;
  else
    result := inherited CreatelogEvent(Category);
  end;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractBeginnerPreparedStatement.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

procedure TZAbstractBeginnerPreparedStatement.SetASQL(const Value: RawByteString);
begin
  if ( ASQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetASQL(Value);
  end;
end;

procedure TZAbstractBeginnerPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if ( WSQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetWSQL(Value);
  end;
end;

function TZAbstractBeginnerPreparedStatement.GetOmitComments: Boolean;
begin
  Result := False;
end;

function TZAbstractBeginnerPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
  Result := nil;
end;

end.
