{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Statement Classes                   }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcAdoStatement;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}
{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX, FmtBCD,
  Windows,
  ZClasses, //inlined Get method of TZCustomElementList
  ZCompatibility, ZSysUtils,
  ZDbcIntfs, ZDbcStatement, ZDbcAdo, ZPlainAdo, ZVariant, ZDbcAdoUtils,
  ZDbcOleDBStatement, ZDbcUtils;

{$IFDEF WITH_NOT_INLINED_WARNING}{$WARN 6058 off : Call to subroutine "operator:=(const source:OleVariant):AnsiString" marked as inline is not inlined}{$ENDIF}
type
  {** Implements Prepared ADO Statement. }
  TZAbstractAdoStatement = Class(TZUTF16ParamCountPreparedStatement)
  private
    FAdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FIsSelectSQL: Boolean;
    FCommandType: CommandTypeEnum;
    FRC: OleVariant;
    FByteBuffer: PByteBuffer;
    fDEFERPREPARE: Boolean;
  protected
    function CreateResultSet: IZResultSet; virtual;
    /// <summary>Removes the current connection reference from this object.</summary>
    /// <remarks>This method will be called only if the object is garbage.</remarks>
    procedure ReleaseConnection; override;
    /// <summary>Adds the parameter value to the SQLStringWriter as a log value</summary>
    /// <param>"Index" The index of the parameter. First index is 0, second is 1..</param>
    /// <param>"SQLWriter" the buffered writer which composes the log string.</param>
    /// <param>"Result" a reference to the result string the SQLWriter flushes the buffer.</param>
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
  public
    constructor CreateWithCommandType(const Connection: IZConnection; const SQL: string;
      const Info: TStrings; CommandType: CommandTypeEnum);
  public
    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
  end;

  TZAdoPreparedStatement = class(TZAbstractAdoStatement, IZPreparedStatement)
  private
    FRefreshParamsFailed, FEmulatedParams: Boolean;
  protected
    function CheckParameterIndex(Index: Integer; SQLType: TZSQLType): TDataTypeEnum; reintroduce;
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    function CreateResultSet: IZResultSet; override;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
  public //setters
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual

    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(Index: Integer; {%H-}SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; AValue: Boolean); reintroduce;
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(Index: Integer; AValue: Byte);
    procedure SetShort(Index: Integer; AValue: ShortInt);
    procedure SetWord(Index: Integer; AValue: Word); reintroduce;
    procedure SetSmall(Index: Integer; AValue: SmallInt); reintroduce;
    procedure SetUInt(Index: Integer; AValue: Cardinal); reintroduce;
    procedure SetInt(Index: Integer; AValue: Integer); reintroduce;
    procedure SetULong(Index: Integer; const AValue: UInt64); reintroduce;
    procedure SetLong(Index: Integer; const AValue: Int64); reintroduce;
    procedure SetFloat(Index: Integer; AValue: Single); reintroduce;
    procedure SetDouble(Index: Integer; const AValue: Double); reintroduce;
    procedure SetCurrency(Index: Integer; const AValue: Currency); reintroduce;
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TBCD); reintroduce;

    procedure SetPWideChar(Index: Word; Value: PWideChar; Len: Cardinal);
    procedure SetPBytes(Index: Word; AValue: PByte; Len: Cardinal);

    procedure SetCharRec(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZCharRec); reintroduce;
    procedure SetString(Index: Integer; const AValue: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(Index: Integer; const AValue: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(Index: Integer; const AValue: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const AValue: RawByteString); reintroduce;
    procedure SetUnicodeString(Index: Integer; const AValue: UnicodeString); reintroduce;

    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZDate); overload;
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZTime); overload;
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZTimeStamp); overload;

    procedure SetBytes(Index: Integer; const AValue: TBytes); reintroduce; overload;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGUID(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TGUID); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const AValue: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZAdoStatement = class(TZAbstractAdoStatement)
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  TZAdoCallableStatement2 = class(TZAbstractCallableStatement_W, IZCallableStatement)
  protected
    /// <summary>creates an exceution Statement. Which wraps the call.</summary>
    /// <param>"StoredProcName" the name of the stored procedure or function to
    ///  be called.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, Math,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  ZEncoding, ZDbcLogging, ZDbcResultSet, ZFastCode, ZPlainOleDBDriver,
  ZDbcCachedResultSet, ZDbcResultSetMetadata, ZDbcAdoResultSet,
  ZMessages, ZDbcProperties;

var DefaultPreparableTokens: TPreparablePrefixTokens;

const cParamIOs: array[TZProcedureColumnType] of ParameterDirectionEnum = (
  {pctUnknown,} adParamInput,
  {pctIn,}adParamInput,
  {pctInOut,} adParamInputOutput,
  {pctOut,} adParamOutput,
  {pctReturn,} adParamReturnValue,
  {pctResultSet} adParamUnknown
  );

{ TZAbstractAdoStatement }

procedure TZAbstractAdoStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZSQLStringWriter; var Result: SQLString);
var V: OleVariant;
  vt: Word;
  ValueAddr: Pointer;
begin
  with FAdoCommand.Parameters.Item[ParamIndex] do begin
    V := Get_Value;
    vt := tagVariant(V).vt;
    if vt and VT_BYREF = VT_BYREF then begin
      vt := vt xor VT_BYREF;
      ValueAddr := tagVariant(V).unkVal;
    end else if vt = VT_DECIMAL
      then ValueAddr := @V
      else if (vt = VT_BSTR)
        then ValueAddr := tagVariant(V).bstrVal
        else ValueAddr := @tagVariant(V).bVal;
    case vt of
      VT_NULL, VT_EMPTY: SQLWriter.AddText('(NULL)', Result);
      VT_BOOL:          if PWordBool(ValueAddr)^
                        then SQLWriter.AddText('(TRUE)', Result)
                        else SQLWriter.AddText('(FALSE)', Result);
      VT_UI1:           SQLWriter.AddOrd(PByte(ValueAddr)^, Result);
      VT_UI2:           SQLWriter.AddOrd(PWord(ValueAddr)^, Result);
      VT_UI4:           SQLWriter.AddOrd(PCardinal(ValueAddr)^, Result);
      VT_UINT:          SQLWriter.AddOrd(PLongWord(ValueAddr)^, Result);
      VT_I1:            SQLWriter.AddOrd(PShortInt(ValueAddr)^, Result);
      VT_I2:            SQLWriter.AddOrd(PSmallInt(ValueAddr)^, Result);
      VT_ERROR,
      VT_I4:            SQLWriter.AddOrd(PInteger(ValueAddr)^, Result);
      VT_INT:           SQLWriter.AddOrd(PLongInt(ValueAddr)^, Result);
      VT_HRESULT:       SQLWriter.AddOrd(PHResult(ValueAddr)^, Result);
      VT_UI8:           SQLWriter.AddOrd(PUInt64(ValueAddr)^, Result);
      VT_I8:            SQLWriter.AddOrd(PInt64(ValueAddr)^, Result);
      VT_CY:            SQLWriter.AddDecimal(PCurrency(ValueAddr)^, Result);
      VT_DECIMAL:     begin
                        if PDecimal(ValueAddr).scale > 0 then begin
                          ScaledOrdinal2Bcd(UInt64(PDecimal(ValueAddr).Lo64), PDecimal(ValueAddr).scale, PBCD(FByteBuffer)^, PDecimal(ValueAddr).sign > 0);
                          SQLWriter.AddDecimal(PBCD(FByteBuffer)^, Result);
                        end else if PDecimal(ValueAddr).sign > 0 then
                          SQLWriter.AddOrd(Int64(-UInt64(PDecimal(ValueAddr).Lo64)), Result)
                        else
                          SQLWriter.AddOrd(UInt64(PDecimal(ValueAddr).Lo64), Result);
                      end;
      VT_R4:          SQLWriter.AddFloat(PSingle(ValueAddr)^, Result);
      VT_R8:          SQLWriter.AddFloat(PDouble(ValueAddr)^, Result);
    else case Type_ of {ADO uses its own DataType-mapping different to System tagVariant type mapping}
        adGUID:      begin
                       SQLWriter.AddChar(#39, Result);
                       SQLWriter.{$IFNDEF UNICODE}AddAscii7UTF16Text{$ELSE}AddText{$ENDIF}(PWideChar(ValueAddr), 38, Result);
                       SQLWriter.AddChar(#39, Result);
                     end;
        adDBTime,
        adDate,
        adDBDate,
        adDBTimeStamp: SQLWriter.AddDateTime(PDateTime(ValueAddr)^, ConSettings.WriteFormatSettings.DateTimeFormat, Result);
        adChar,
        adWChar,
        adVarChar,
        adVarWChar:     begin
                          {$IFNDEF UNICODE}
                          PUnicodeToRaw(PWideChar(ValueAddr), Length(WideString(ValueAddr)), zCP_UTF8, fRawTemp);
                          SQLWriter.AddTextQuoted(fRawTemp, #39, Result);
                          fRawTemp := '';
                          {$ELSE}
                          SQLWriter.AddTextQuoted(PWideChar(ValueAddr), Length(WideString(ValueAddr)), #39, Result);
                          {$ENDIF}
                        end;
        adLongVarChar:  SQLWriter.AddText('(CLOB)', Result);
        adLongVarWChar: SQLWriter.AddText('(NCLOB)', Result);
        adLongVarBinary:SQLWriter.AddText('(BLOB)', Result);
        adBinary,
        adVarBinary:    SQLWriter.AddHexBinary(tagVariant(V).parray.pvData, tagVariant(V).parray.cbElements, True,  Result);
        else            SQLWriter.AddText('(UNKNOWN)', Result);
      end;
    end;
  end;
end;

function TZAbstractAdoStatement.CreateResultSet: IZResultSet;
var NativeResultSet: IZResultSet;
begin
  if Assigned(FAdoRecordset) and ((FAdoRecordSet.State and adStateOpen) = adStateOpen) then begin
    NativeResultSet := TZAdoResultSet.Create(Self, SQL, FAdoRecordSet);
    if ResultSetConcurrency = rcUpdatable then
      Result := TZADOCachedResultSet.Create(NativeResultSet, SQL,
        TZAdoCachedResolver.Create(FAdoConnection.GetAdoConnection, Self,
          NativeResultSet.GetMetaData), ConSettings)
    else Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
  end else
    Result := nil;
end;

constructor TZAbstractAdoStatement.CreateWithCommandType(const Connection: IZConnection;
  const SQL: string; const Info: TStrings; CommandType: CommandTypeEnum);
begin
  inherited Create(Connection, SQL, Info);
  FCommandType := CommandType;
  FAdoConnection := Connection as IZAdoConnection;
  FByteBuffer := FAdoConnection.GetByteBufferAddress;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractAdoStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;

  Prepare;
  BindInParameters;
  try
    RestartTimer;
    FAdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, adOptionUnspecified);
    LastResultSet := CreateResultSet;
    LastUpdateCount := {%H-}RC;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt, Self);
  except
    on E: Exception do
      FAdoConnection.HandleErrorOrWarning(lcExecPrepStmt, E, Self, SQL);
  end;
  Result := Assigned(LastResultSet);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractAdoStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close; //Note keep track we close the RS and DO NOT Try to resync them!
  FOpenResultSet := nil;
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  RestartTimer;
  try
    if FIsSelectSQL then begin
      if (FAdoRecordSet = nil) or (FAdoRecordSet.MaxRecords <> MaxRows) then begin
        FAdoRecordSet := CoRecordSet.Create;
        FAdoRecordSet.MaxRecords := MaxRows;
        //handle a MSAccess issue: https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=127118
        if Self.FAdoConnection.GetServerProvider <> spMSJet then begin
          FAdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
          if (GetResultSetType = rtForwardOnly) or (GetResultSetConcurrency = rcUpdatable)
          then FAdoRecordSet.CursorLocation := adUseServer
          else FAdoRecordSet.CursorLocation := adUseClient;
          FAdoRecordSet.CacheSize := 128*1024;
          FAdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockReadOnly, adOptionUnspecified);
        end else
          FAdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
      end else
        FAdoRecordSet.Requery(adOptionUnspecified);
    end else
      FAdoRecordSet := FAdoCommand.Execute(FRC, EmptyParam, adOptionUnspecified);
    Result := CreateResultSet;
    LastUpdateCount := FRC;
    if not Assigned(Result) then begin
      while (GetMoreResults) and (LastUpdateCount > -1) do ;
      Result := GetResultSet;
    end;
    FOpenResultSet := Pointer(Result);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt, Self);
  except
    on E: Exception do
      FAdoConnection.HandleErrorOrWarning(lcExecPrepStmt, E, Self, SQL);
  end
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAbstractAdoStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  try
    RestartTimer;
    FAdoRecordSet := FAdoCommand.Execute(FRC, EmptyParam, adExecuteNoRecords);
    if BindList.HasOutOrInOutOrResultParam then
      LastResultSet := CreateResultSet;
    LastUpdateCount := FRC;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt, Self);
  except
    on E: Exception do
      FAdoConnection.HandleErrorOrWarning(lcExecPrepStmt, E, Self, SQL);
  end;
  Result := LastUpdateCount;
end;

function TZAbstractAdoStatement.GetMoreResults: Boolean;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(FAdoRecordSet) then begin
    FAdoRecordSet := FAdoRecordSet.NextRecordset(FRC);
    LastResultSet := CreateResultSet;
    Result := Assigned(LastResultSet);
    LastUpdateCount := FRC;
  end;
end;

procedure TZAbstractAdoStatement.Prepare;
var S: String;
begin
  if FAdoCommand = nil then begin
    FAdoCommand := CoCommand.Create;
    FAdoCommand.CommandText := WSQL;
    FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  end;
  if Not Prepared then begin//prevent PrepareInParameters
    FIsSelectSQL := IsSelect(SQL);
    FAdoCommand.CommandText := WSQL;
    FAdoCommand.CommandType := FCommandType;
    if (FWeakIZPreparedStatementPtr <> nil) and (FTokenMatchIndex > -1) then begin
      S := GetParameters.Values[DSProps_DeferPrepare];
      if S = '' then
        S := Connection.GetParameters.Values[DSProps_DeferPrepare];
      if S = '' then begin
        S := DefineStatementParameter(Self, DSProps_PreferPrepared, StrTrue);
        fDEFERPREPARE := not StrToBoolEx(S);
      // FAdoCommand.Properties['Defer Prepare'].Value := False;
      end else
        fDEFERPREPARE := StrToBoolEx(S);
      if (not fDEFERPREPARE and (BindList.Capacity = 0)) or (FCommandType = adCmdStoredProc) then begin
        RestartTimer;
        FAdoCommand.Prepared := True;
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcPrepStmt,Self);
      end;
    end;
    inherited Prepare;
  end;
end;

procedure TZAbstractAdoStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FAdoConnection := nil;
end;

procedure TZAbstractAdoStatement.Unprepare;
begin
  RestartTimer;
  FAdoRecordSet := nil;
  if Assigned(FAdoCommand) and FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  FAdoCommand := nil;
  inherited Unprepare;
end;

{ TZAdoPreparedStatement }

function TZAdoPreparedStatement.CheckParameterIndex(Index: Integer;
  SQLType: TZSQLType): TDataTypeEnum;
var I: Integer;
  procedure AddParam(Number: Integer);
  var Param: _Parameter;
      W: WideString;
  begin
    W := 'Param'+IntToUnicode(Number);
    Param := fAdoCommand.CreateParameter(W, adVariant, cParamIOs[BindList[i].ParamType], SizeOf(OleVariant), EmptyParam);
    fAdoCommand.Parameters.Append(Param);
  end;
begin
  if not Prepared then Prepare;
  if not FEmulatedParams and FRefreshParamsFailed and (BindList.Count < (Index+1)) and (BindList.Capacity >= (Index+1)) then
    for i := BindList.Count to Index do
      AddParam(I+1);
  if (FEmulatedParams or FRefreshParamsFailed) and (BindList.Capacity < (Index+1)) then
    raise CreateBindVarOutOfRangeError(Index);
  inherited CheckParameterIndex(Index);
  if FEmulatedParams
  then Result := adBSTR
  else if FRefreshParamsFailed
    then Result := ZSQLTypeToAdoType[SQLType]
    else Result := fAdoCommand.Parameters[Index].Type_;
end;

constructor TZAdoPreparedStatement.Create(
  const Connection: IZConnection; const SQL: string; const Info: TStrings);
begin
  inherited CreateWithCommandType(Connection, SQL, Info, adCmdText);
end;

function TZAdoPreparedStatement.CreateResultSet: IZResultSet;
  procedure CreateOutParamResultSet;
  var
    I, j: Integer;
    ColumnInfo: TZColumnInfo;
    ColumnsInfo: TObjectList;
    RS: TZVirtualResultSet;
    P: Pointer;
    Blob: IZBlob;
    Temp: OleVariant;
    BD: TBCD;
    PD: PDecimal;
    L: NativeUInt;
    AdType: TDataTypeEnum;
    {$IFNDEF UNICODE}
    Name: WideString;
    {$ENDIF}
  begin
    ColumnsInfo := TObjectList.Create;
    try
      for I := 0 to FAdoCommand.Parameters.Count -1 do
        if FAdoCommand.Parameters.Item[i].Direction in [adParamOutput,
          adParamInputOutput, adParamReturnValue] then begin
        ColumnInfo := TZColumnInfo.Create;
        with ColumnInfo do begin
          {$IFNDEF UNICODE}
          Name := FAdoCommand.Parameters.Item[i].Name;
          ColumnLabel := PUnicodeToRaw(Pointer(Name), Length(Name), ZCP_UTF8);
          {$ELSE}
          ColumnLabel := FAdoCommand.Parameters.Item[i].Name;
          {$ENDIF}
          AdType := FAdoCommand.Parameters.Item[I].Type_;
          case AdType of
            adChar, adVarChar, adLongVarChar,
            adWChar, adVarWChar, adBSTR, adLongVarWChar,
            adBinary, adVarBinary, adLongVarBinary:
                Precision := FAdoCommand.Parameters.Item[I].Size
            else FAdoCommand.Parameters.Item[I].Precision;
          end;
          ColumnType := ConvertAdoToSqlType(AdType,
            Precision, FAdoCommand.Parameters.Item[I].NumericScale);
        end;
        ColumnsInfo.Add(ColumnInfo);
      end;

      RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
      with RS do begin
        SetType(rtScrollInsensitive);
        SetConcurrency(rcUpdatable);
        RS.MoveToInsertRow;
        J := 0;
        for i := 0 to FAdoCommand.Parameters.Count -1 do begin
          with FAdoCommand.Parameters.Item[i] do begin
            if Direction in [adParamOutput, adParamInputOutput, adParamReturnValue] then
            with TZColumnInfo(ColumnsInfo[J]) do begin
              Temp := Value;
              case tagVariant(Temp).vt of
                VT_NULL, VT_EMPTY: ;
                VT_BOOL:        RS.UpdateBoolean(J+FirstDbcIndex, tagVARIANT(Temp).vbool);
                VT_UI1:         RS.UpdateByte(J+FirstDbcIndex, tagVARIANT(Temp).bVal);
                VT_UI2:         RS.UpdateWord(J+FirstDbcIndex, tagVARIANT(Temp).uiVal);
                VT_UI4:         RS.UpdateUInt(J+FirstDbcIndex, tagVARIANT(Temp).ulVal);
                VT_UINT:        RS.UpdateUInt(J+FirstDbcIndex, tagVARIANT(Temp).uintVal);
                VT_I1:          RS.UpdateShort(J+FirstDbcIndex, ShortInt(tagVARIANT(Temp).cVal));
                VT_I2:          RS.UpdateSmall(J+FirstDbcIndex, tagVARIANT(Temp).iVal);
                VT_I4:          RS.UpdateInt(J+FirstDbcIndex, tagVARIANT(Temp).lVal);
                VT_INT,
                VT_HRESULT:     RS.UpdateInt(J+FirstDbcIndex, tagVARIANT(Temp).intVal);
                VT_ERROR:       RS.UpdateInt(J+FirstDbcIndex, tagVARIANT(Temp).scode);
                VT_I8:          RS.UpdateLong(J+FirstDbcIndex, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).llVal{$ELSE}PInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
                VT_UI8:         RS.UpdateULong(J+FirstDbcIndex, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).ullVal{$ELSE}PUInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
                VT_R4:          RS.UpdateFloat(J+FirstDbcIndex, tagVARIANT(Temp).fltVal);
                VT_R8:          RS.UpdateDouble(J+FirstDbcIndex, tagVARIANT(Temp).dblVal);
                VT_CY:          RS.UpdateCurrency(J+FirstDbcIndex, tagVARIANT(Temp).cyVal);
                VT_DATE:        RS.UpdateTimeStamp(J+FirstDbcIndex, tagVARIANT(Temp).date);
                VT_BSTR:        begin
                                  L := PLongInt(PAnsiChar(tagVARIANT(Temp).bstrVal)-SizeOf(LongInt))^ shr 1; //fpc has no SysStringLen -> improve
                                  RS.UpdatePWideChar(J+FirstDbcIndex, tagVARIANT(Temp).bstrVal, L);
                                end;
                VT_DECIMAL:     begin
                                  PD := @Temp;
                                  if Ord(ColumnType) <= Ord(stULong) then
                                    if PD.Sign > 0
                                    then RS.UpdateLong(J+FirstDbcIndex, -UInt64(PD.Lo64))
                                    else RS.UpdateULong(J+FirstDbcIndex, UInt64(PD.Lo64))
                                  else begin
                                    ScaledOrdinal2Bcd(UInt64(PD.Lo64), PD.Scale, BD{%H-}, PD.Sign > 0);
                                    RS.UpdateBigDecimal(J+FirstDbcIndex, BD);
                                  end;
                                end;
                else            case Type_ of
                                  adBinary,
                                  adVarBinary: RS.UpdateBytes(J+FirstDbcIndex, VarToBytes(Temp));
                                  adLongVarBinary:
                                      if VarIsArray(Temp) then begin
                                        P := VarArrayLock(Temp);
                                        try
                                          Blob := TZLocalMemBLob.CreateWithData(P, VarArrayHighBound(Temp, 1)+1);
                                          RS.UpdateLob(J+FirstDbcIndex, Blob);
                                        finally
                                          VarArrayUnLock(Temp);
                                        end;
                                      end;
                                  else UpdateNull(J+FirstDbcIndex); //debug -> not required
                                end;
              end;
              Inc(J);
            end;
          end;
        end;
        InsertRow;
        SetConcurrency(rcReadOnly);
      end;
      fOutParamResultSet := RS;
      fOpenResultSet := Pointer(Result);
    finally
      ColumnsInfo.Free;
    end;
  end;
begin
  if (Bindlist.HasOutOrInOutOrResultParam) then CreateOutParamResultSet;
  Result := inherited CreateResultSet;
  if Result = nil then
    Result := fOutParamResultSet;
end;

function TZAdoPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := @DefaultPreparableTokens;
end;

procedure TZAdoPreparedStatement.PrepareInParameters;
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
  if (FCommandType = adCmdStoredProc) or (Bindlist.Capacity = 0) then Exit;
  OLEDBCommand := (FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand;
  OLEDBCommand.QueryInterface(ICommandWithParameters, OLEDBParameters);
  if Assigned(OLEDBParameters) and not fDEFERPREPARE then begin
    OLEDBParameters.SetParameterInfo(0, nil, nil);
    ParamInfo := nil;
    NamesBuffer := nil;
    try
      OLEDBCommand.QueryInterface(ICommandPrepare, CommandPrepare);
      if Assigned(CommandPrepare) then CommandPrepare.Prepare(0);
      if OLEDBParameters.GetParameterInfo(ParamCount{%H-}, PDBPARAMINFO(ParamInfo), NamesBuffer) = S_OK then begin
        for I := 0 to ParamCount - 1 do
          with ParamInfo[I] do begin
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
            if Direction = adParamUnknown then
              Direction := cParamIOs[BindList[i].ParamType];
            Parameter := FAdoCommand.CreateParameter(Name, wType, Direction, ulParamSize, EmptyParam);
            Parameter.Precision := bPrecision;
            Parameter.NumericScale := ParamInfo[I].bScale;
            if (dwFlags and $FFFFFFF0) <= (adParamSigned or adParamNullable or adParamLong) then
              Parameter.Attributes := dwFlags and $FFFFFFF0; { Mask out Input/Output flags }
          end;
        FRefreshParamsFailed := False;
      end else FRefreshParamsFailed := True;
    finally
      if Assigned(CommandPrepare) then CommandPrepare.Unprepare;
      if (ParamInfo <> nil) then ZAdoMalloc.Free(ParamInfo);
      if (NamesBuffer <> nil) then ZAdoMalloc.Free(NamesBuffer);
    end;
  end;
end;

procedure TZAdoPreparedStatement.SetAnsiString(Index: Integer;
  const AValue: AnsiString);
begin
  FUniTemp := PRawToUnicode(Pointer(AValue), Length(aValue), ZOSCodePage);
  SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp));
end;

procedure TZAdoPreparedStatement.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TBCD);
var V: OleVariant;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBigDecimal) of
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := BCDCompare(AValue, NullBCD) <> 0;
                  goto set_var;
                end;
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  BCDToCurr(AValue, tagVariant(V).cyVal);
                  goto set_var;
                end;
    adTinyInt, adSmallInt, adInteger, adBigInt: SetLong(Index, BCD2Int64(AValue));
    adUnsignedTinyInt, adUnsignedSmallInt, adUnsignedInt,
    adUnsignedBigInt: SetULong(Index, BCD2UInt64(AValue));
    adSingle, adDouble: SetDouble(Index, BCDToDouble(AValue));
    adDecimal,
    adNumeric:  begin
                  tagVariant(V).vt := VT_DECIMAL;
                  BCD2Decimal(AValue, @V);
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        SetPWideChar(Index, PWideChar(FByteBuffer), ZSysUtils.BcdToUni(AValue,
                  PWideChar(FByteBuffer), '.'));
  end;
end;

procedure TZAdoPreparedStatement.SetBlob(Index: Integer;
  SQLType: TZSQLType; const AValue: IZBlob);
var P: Pointer;
  Lob: IZBlob;
  Len: NativeUint;
begin
  Lob := AValue; //inc refcnt else FPC leaks many memory
  if (AValue = nil) or (aValue.IsEmpty) then
    SetNull(Index, SQLType)
  else case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType) of
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: if Lob.IsClob then begin
                      Lob.SetCodePageTo(zCP_UTF16);
                      P := Lob.GetPWideChar(FUniTemp, Len);
                      SetPWideChar(Index, P, Len);
                    end else begin
                      P := Lob.GetBuffer(FRawTemp, Len);
                      fUniTemp := ZDbcUtils.GetSQLHexWideString(P, Len, True);
                      SetPWideChar(Index, Pointer(fUniTemp), Length(fUniTemp));
                    end;
    adBinary,
    adVarBinary,
    adLongVarBinary: begin
                       P := Lob.GetBuffer(FRawTemp, Len);
                       SetPBytes(Index, P, Len);
                     end;
    else raise CreateConversionError(Index, SQLType, stUnknown)
  end;
end;

procedure TZAdoPreparedStatement.SetBoolean(Index: Integer;
  AValue: Boolean);
var V: OleVariant;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBoolean) of
    adBoolean: begin
        tagVariant(V).vt := VT_BOOL;
        tagVariant(V).vbool := AValue;
        FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
      end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: if AValue
                    then SetPWideChar(Index, Pointer(BoolStrsUpW[True]), 4)
                    else SetPWideChar(Index, Pointer(BoolStrsUpW[False]), 5);
    else SetInt(Index, Ord(AValue));
  end;
end;

procedure TZAdoPreparedStatement.SetByte(Index: Integer; AValue: Byte);
var V: OleVariant;
begin
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte) = adUnsignedTinyInt then begin
    tagVariant(V).vt := VT_UI1;
    tagVariant(V).bVal := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetUInt(Index, AValue);
end;

{**
  Sets the designated parameter to a Java array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZAdoPreparedStatement.SetBytes(ParameterIndex: Integer; Value: PByte;
  Len: NativeUInt);
begin
  if (Value = nil) or (Len = 0) then
    SetNull(ParameterIndex, stBytes)
  else case CheckParameterIndex(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBytes) of
    adBinary,
    adVarBinary,
    adLongVarBinary: SetPBytes(ParameterIndex, Value, Len);
    else raise CreateConversionError(ParameterIndex, stBytes, stUnknown)
  end;
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAdoPreparedStatement.SetBytes(Index: Integer;
  const AValue: TBytes);
var P: Pointer;
  L: LengthInt;
  UID: PGUID absolute P;
begin
  P := Pointer(AValue);
  if P = nil
  then SetNull(Index, stBytes)
  else begin
    L := Length(AValue);
    case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBytes) of
      adGUID: begin
                CheckError(L=SizeOf(TGUID), 'UID-Size missmatch');
                GUIDToBuffer(@UID.D1, PWideChar(fByteBuffer), [guidWithBrackets, guidSet0Term]);
                SetPWideChar(Index, PWideChar(fByteBuffer), 38); //ad GUID is a BSTR?
              end;
      adBSTR: begin
                fUniTemp := ZDbcUtils.GetSQLHexWideString(P, Length(AValue), True);
                SetPWideChar(Index, Pointer(fUniTemp), L);
              end;
      adBinary,
      adVarBinary,
      adLongVarBinary: SetPBytes(Index, P, L);
    end;
  end;
end;

procedure TZAdoPreparedStatement.SetCharRec(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZCharRec);
begin
  if AValue.CP = zCP_UTF16 then
    SetPWidechar(Index, AValue.P, AValue.Len)
  else begin
    fUniTemp := PRawToUnicode(AValue.P, AValue.Len, AValue.CP);
    SetPWidechar(Index, Pointer(fUniTemp), Length(fUniTemp));
  end;
end;

procedure TZAdoPreparedStatement.SetCurrency(Index: Integer;
  const AValue: Currency);
var V: OleVariant;
  PD: PDecimal;
  I64: Int64 absolute AValue;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stCurrency) of
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  tagVariant(V).cyVal := AValue;
                  goto set_var;
                end;
    adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt,
    adUnsignedSmallInt, adUnsignedInt, adUnsignedBigInt:
                  SetLong(Index, I64 div 10000);
    adSingle:   begin
                  tagVariant(V).vt := VT_R4;
                  tagVariant(V).fltVal := AValue;
                  goto set_var;
                end;
    adDouble:   begin
                  tagVariant(V).vt := VT_R8;
                  tagVariant(V).dblVal := AValue;
                  goto set_var;
                end;
    adDate, adDBDate, adDBTime, adDBTimeStamp: begin
                  tagVariant(V).vt := VT_DATE;
                  tagVariant(V).date := AValue;
                  goto set_var;
                end;
    adDecimal,
    adNumeric:  begin
                  tagVariant(V).vt := VT_DECIMAL;
                  PD := @V;
                  PD.scale := 4;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                  if AValue < 0 then begin
                    PD.sign := 1;
                    PUInt64(@PD.Lo64)^ := -i64;
                  end else begin
                    PD.sign := 0;
                    PD.Lo64 := i64;
                  end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
                  goto set_var;
                end;
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        begin
                  CurrToUnicode(AValue, '.', PWideChar(fByteBuffer), @PD);
                  SetPWideChar(Index, PWideChar(fByteBuffer), PWideChar(PD) - PWideChar(fByteBuffer));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZDate);
var V: OleVariant;
label jmp_assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDate) of
    adDBDate, adDBTime, adDBTimeStamp:
                with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
                  if Size <> 8 then //size is initializesd to ole record sizes are not 8
                    Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
                  goto jmp_Assign;
                end;
    adDate:     begin
jmp_Assign:       V := null;
                  tagVariant(V).vt := VT_DATE;
                  TryDateToDateTime(AValue, PDateTime(@tagVariant(V).date)^);
                  FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, PWideChar(fByteBuffer),
      DateToUni(AValue.Year, AValue.Month, AValue.Day, PWideChar(fByteBuffer),
      ConSettings.WriteFormatSettings.DateFormat, False, AValue.IsNegative));
    else           begin
                      TryDateToDateTime(AValue, PDateTime(fByteBuffer)^);
                      SetDouble(Index, PDouble(fByteBuffer)^);
                    end;
  end;
end;

procedure TZAdoPreparedStatement.SetDouble(Index: Integer;
  const AValue: Double);
var V: OleVariant;
  BCD: TBCD;
label set_var;
begin
  V := null;
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDouble) of
    adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt,
    adUnsignedSmallInt, adUnsignedInt, adUnsignedBigInt:
                  SetLong(Index, Trunc(AValue));
    adSingle:   begin
                  tagVariant(V).vt := VT_R4;
                  tagVariant(V).fltVal := AValue;
                  goto set_var;
                end;
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  tagVariant(V).cyVal := AValue;
                  goto set_var;
                end;
    adDecimal,
    adNumeric:  begin
                  if FRefreshParamsFailed
                  then Double2BCD(AValue, BCD{%H-})
                  else Double2BCD(RoundTo(AValue, -FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].NumericScale), BCD);
                  SetBigDecimal(Index, BCD);
                end;
    adDate, adDBDate, adDBTime, adDBTimeStamp: begin
                  tagVariant(V).vt := VT_DATE;
                  tagVariant(V).date := AValue;
                  goto set_var;
                end;
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adDouble:   begin
                  tagVariant(V).vt := VT_R8;
                  tagVariant(V).dblVal := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        SetPWideChar(Index, PWideChar(fByteBuffer), ZSysUtils.FloatToUnicode(AValue, PWideChar(fByteBuffer)));
  end;
end;

procedure TZAdoPreparedStatement.SetFloat(Index: Integer;
  AValue: Single);
var V: OleVariant;
  BCD: TBCD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stFloat) of
    adTinyInt, adSmallInt, adInteger, adBigInt, adUnsignedTinyInt,
    adUnsignedSmallInt, adUnsignedInt, adUnsignedBigInt:
                  SetLong(Index, Trunc(AValue));
    adDouble:   begin
                  tagVariant(V).vt := VT_R8;
                  tagVariant(V).dblVal := AValue;
                  goto set_var;
                end;
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  tagVariant(V).cyVal := AValue;
                  goto set_var;
                end;
    adDecimal,
    adNumeric:  begin
                  if FRefreshParamsFailed
                  then Double2BCD(AValue, BCD{%H-})
                  else Double2BCD(RoundTo(AValue, -FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].NumericScale), BCD);
                  SetBigDecimal(Index, BCD);
                end;
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adDate, adDBDate, adDBTime, adDBTimeStamp: begin
                  tagVariant(V).vt := VT_DATE;
                  tagVariant(V).date := AValue;
                  goto set_var;
                end;
    adSingle:   begin
                  tagVariant(V).vt := VT_R4;
                  tagVariant(V).fltVal := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        SetPWideChar(Index, PWideChar(fByteBuffer), ZSysUtils.FloatToUnicode(AValue, PWideChar(fByteBuffer)));
  end;
end;

procedure TZAdoPreparedStatement.SetGUID(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TGUID);
begin
  GUIDToBuffer(@AValue.D1, PWideChar(fByteBuffer), [guidWithBrackets, guidSet0Term]);
  SetPWideChar(Index, PWideChar(fByteBuffer), 38); //ad GUID is a BSTR?
end;

procedure TZAdoPreparedStatement.SetInt(Index, AValue: Integer);
var V: OleVariant;
  PD: PDecimal;
  P: PWideChar absolute PD;
label set_var;
begin
  V := null;
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stInteger) of
    adTinyInt:  begin
                  tagVariant(V).vt := VT_I1;
                  PShortInt(@tagVariant(V).cVal)^ := AValue;
                  goto set_var;
                end;
    adSmallInt: begin
                  tagVariant(V).vt := VT_I2;
                  tagVariant(V).iVal := AValue;
                  goto set_var;
                end;
    adBigInt:   begin
                  tagVariant(V).vt := VT_I8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).llVal{$ELSE}PInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
                  goto set_var;
                end;
    adUnsignedTinyInt: begin
                  tagVariant(V).vt := VT_UI1;
                  tagVariant(V).bVal := AValue;
                  goto set_var;
                end;
    adUnsignedSmallInt: begin
                  tagVariant(V).vt := VT_UI2;
                  tagVariant(V).uiVal := AValue;
                  goto set_var;
                end;
    adUnsignedInt: begin
                  tagVariant(V).vt := VT_UI4;
                  tagVariant(V).ulVal := AValue;
                  goto set_var;
                end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    adUnsignedBigInt: begin
                  tagVariant(V).vt := VT_UI8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).ullVal{$ELSE}PUInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
                  goto set_var;
                end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    adDate, adDBDate, adDBTime, adDBTimeStamp,
    adSingle, adDouble, adCurrency, adDecimal,
    adNumeric:  SetCurrency(Index, AValue);
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adInteger:  begin
                  tagVariant(V).vt := VT_I4;
                  tagVariant(V).lVal := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        begin
                  IntToUnicode(AValue, PWideChar(fByteBuffer), @P);
                  SetPWideChar(Index, PWideChar(fByteBuffer), P - PWideChar(fByteBuffer));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetLong(Index: Integer;
  const AValue: Int64);
var V: OleVariant;
  PD: PDecimal;
  P: PWideChar absolute PD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong) of
    adTinyInt:  begin
                  tagVariant(V).vt := VT_I1;
                  PShortInt(@tagVariant(V).cVal)^ := AValue;
                  goto set_var;
                end;
    adSmallInt: begin
                  tagVariant(V).vt := VT_I2;
                  tagVariant(V).iVal := AValue;
                  goto set_var;
                end;
    adInteger:  begin
                  tagVariant(V).vt := VT_I4;
                  tagVariant(V).lVal := AValue;
                  goto set_var;
                end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    adUnsignedTinyInt, adUnsignedSmallInt, adUnsignedInt,
    adUnsignedBigInt: SetULong(Index, AValue);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    adDate, adDBDate, adDBTime, adDBTimeStamp,
    adSingle, adDouble: SetDouble(Index, AValue);
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  tagVariant(V).cyVal := AValue;
                  goto set_var;
                end;
    adDecimal,
    adNumeric:  begin
                  tagVariant(V).vt := VT_DECIMAL;
                  PD := @V;
                  PD.scale := 0;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                 if AValue < 0 then begin
                    PUint64(@PD.Lo64)^ := -AValue;
                    PD.sign := 1;
                  end else begin
                    PUint64(@PD.Lo64)^ := Uint64(AValue);
                    PD.sign := 0;
                  end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
                  goto set_var;
                end;
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adBigInt:   begin
                  tagVariant(V).vt := VT_I8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).llVal{$ELSE}PInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        begin
                  IntToUnicode(AValue, PWideChar(fByteBuffer), @P);
                  SetPWideChar(Index, PWideChar(fByteBuffer), P - PWideChar(fByteBuffer));
                  Exit;
                end;
  end;
end;

const cNullUni = UnicodeString('null');
procedure TZAdoPreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1{$ENDIF};
  CheckParameterIndex(Index, SQLType);
  if FEmulatedParams
  then BindList.Put(Index, SQLType, cNullUni)
  else FAdoCommand.Parameters[Index].Value := null;
end;

procedure TZAdoPreparedStatement.SetPBytes(Index: Word; AValue: PByte;
  Len: Cardinal);
var V: OleVariant;
begin
  if AValue = nil then
    SetNull(Index, stBytes)
  else case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBytes) of
    adBinary,
    adVarBinary,
    adLongVarBinary: if FEmulatedParams then begin
        FUniTemp := GetSQLHexWideString(PAnsiChar(AValue), Len, True);
        BindList.Put(Index, BindList[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].SQLType, fUniTemp);
      end else begin
        V := VarArrayCreate([0, Len - 1], varByte);
        Move(AValue^, TVarData(V).VArray.Data^, Len);
        FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
        FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Size := Len;
      end;
    else raise CreateConversionError(Index, stBytes, stUnknown)
  end;
end;

procedure TZAdoPreparedStatement.SetPWideChar(Index: Word;
  Value: PWideChar; Len: Cardinal);
var V: OleVariant;
  BCD: TBCD;
  D: Double absolute BCD;
  DT: TDatetime absolute BCD;
  C: Currency absolute BCD;
  procedure SetEmulatedValue;
  begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    case BindList[Index].SQLType of
      stString, stUnicodeString, stAsciiStream, stUnicodeStream:
        BindList.Put(Index, BindList[Index].SQLType, ZSysUtils.SQLQuotedStr(Value, Len, WideChar(#39)));
      else begin
        if Value <> Pointer(fUniTemp)
        then System.SetString(fUniTemp, Value, Len);
        BindList.Put(Index, BindList[Index].SQLType, fUniTemp);
      end;
    end;
  end;
label set_BSTR;
begin
  if FEmulatedParams then
    SetEmulatedValue
  else case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stUnicodeString) of
    adTinyInt, adSmallInt, adInteger, adUnsignedTinyInt, adUnsignedSmallInt:
      SetInt(Index, UniCodeToIntDef(Value, Value+Len, 0));
    adBigInt: SetLong(Index, UniCodeToInt64Def(Value, Value+Len, 0));
    adUnsignedInt, adUnsignedBigInt: SetULong(Index, UniCodeToUInt64Def(Value, Value+Len, 0));
    adSingle, adDouble: begin
                          SQLStrToFloatDef(Value, 0, D, Len);
                          SetDouble(Index, D);
                        end;
    adCurrency:         begin
                          SQLStrToFloatDef(Value, 0, C, Len);
                          SetCurrency(Index, C);
                        end;
    adDecimal,
    adNumeric:          begin
                          if not TryUniToBCD(Value, Len, BCD, '.') then
                            BCD := NullBCD;
                          SetBigDecimal(Index, BCD);
                        end;
    adBoolean:          SetBoolean(Index, StrToBoolEx(Value, Value+Len, True, False));
    adDBTimeStamp,
    adDate,
    adDBDate,
    adDBTime:           begin
                          TryPCharToDateTime(Value, Len, Consettings.WriteFormatSettings, DT);
                          SetDouble(Index, D);
                        end;
    adGUID,
    adBSTR,
    adChar,
    adVarChar,
    adLongVarChar,
    adWChar,
    adVarWChar,
    adLongVarWChar: begin
set_BSTR:               if Value = nil then
                          Value := PEmptyUnicodeString;
                        tagVariant(V).vt := VT_BSTR;
                        System.SetString(PWideString(@tagVariant(V).bstrVal)^, Value, Len);
                        FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                      end;
  end;
end;

procedure TZAdoPreparedStatement.SetRawByteString(Index: Integer;
  const AValue: RawByteString);
var L: LengthInt;
begin
  L := Length(AValue);
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stString) of
    adBinary, adVarBinary, adLongVarBinary:
      SetPBytes(Index, Pointer(AValue), L);
    else begin
      fUniTemp := PRawToUnicode(Pointer(AValue), L, FClientCP);
      SetPWideChar(Index, Pointer(fUniTemp), Length(fUniTemp));
    end;
  end;
end;

procedure TZAdoPreparedStatement.SetShort(Index: Integer;
  AValue: ShortInt);
var V: OleVariant;
begin
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stShort) = adTinyInt then begin
    tagVariant(V).vt := VT_UI1;
    PShortInt(@tagVariant(V).cVal)^ := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetInt(Index, AValue);
end;

procedure TZAdoPreparedStatement.SetSmall(Index: Integer;
  AValue: SmallInt);
var V: OleVariant;
begin
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall) = adSmallInt then begin
    tagVariant(V).vt := VT_I2;
    tagVariant(V).iVal := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetInt(Index, AValue);
end;

procedure TZAdoPreparedStatement.SetString(Index: Integer;
  const AValue: String);
var PW: PWideChar;
  L: NativeUInt;
begin
  {$IFDEF UNICODE}
  L := Length(AValue);
  if L = 0
  then PW := PEmptyUnicodeString
  else PW := Pointer(AValue);
  {$ELSE}
  fUniTemp := ZRawToUnicode(AValue, GetW2A2WConversionCodePage(ConSettings));
  L := Length(fUniTemp);
  if L = 0
  then PW := PEmptyUnicodeString
  else PW := Pointer(fUniTemp);
  {$ENDIF}
  SetPWideChar(Index, PW, L);
end;

procedure TZAdoPreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZTime);
var V: OleVariant;
label jmp_assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stTime) of
    adDBDate, adDBTime, adDBTimeStamp:
                with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
                  if Size <> 8 then //size is initializesd to ole record sizes which are not 8
                    Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
                  goto jmp_Assign;
                end;
    adDate:     begin
jmp_assign:       V := null;
                  tagVariant(V).vt := VT_DATE;
                  TryTimeToDateTime(AValue, PDateTime(@tagVariant(V).date)^);
                  FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, PWideChar(fByteBuffer), TimeToUni(AValue.Hour,
        AValue.Minute, AValue.Second, AValue.Fractions, PWideChar(fByteBuffer),
        ConSettings.WriteFormatSettings.TimeFormat, False, AValue.IsNegative));
    else           begin
                      TryTimeToDateTime(AValue, PDateTime(fByteBuffer)^);
                      SetDouble(Index, PDouble(fByteBuffer)^);
                    end;
  end;
end;

procedure TZAdoPreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} AValue: TZTimeStamp);
var V: OleVariant;
label jmp_Assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stTimeStamp) of
    adDBTimeStamp, adDBDate, adDBTime:
            with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
              if Size <> 8 then //size is initializesd to ole record sizes are not 8
                Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
              goto jmp_Assign;
            end;
    adDate: begin
jmp_Assign:   V := null;
              tagVariant(V).vt := VT_DATE;
              TryTimeStampToDateTime(AValue, PDateTime(@tagVariant(V).date)^);
              FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
            end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, PWideChar(fByteBuffer), DateTimeToUni(AValue.Hour,
        AValue.Minute, AValue.Second, AValue.Fractions, AValue.Hour, AValue.Minute,
        AValue.Fractions, PWideChar(fByteBuffer),
        ConSettings.WriteFormatSettings.TimeFormat, False, AValue.IsNegative));
    else           begin
                      TryTimeStampToDateTime(AValue, PDateTime(fByteBuffer)^);
                      SetDouble(Index, PDouble(fByteBuffer)^);
                    end;
  end;
end;

procedure TZAdoPreparedStatement.SetUInt(Index: Integer;
  AValue: Cardinal);
var V: OleVariant;
  PD: PDecimal;
  P: PWideChar absolute PD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord) of
    adTinyInt:  begin
                  tagVariant(V).vt := VT_I1;
                  PShortInt(@tagVariant(V).cVal)^ := AValue;
                  goto set_var;
                end;
    adSmallInt: begin
                  tagVariant(V).vt := VT_I2;
                  tagVariant(V).iVal := AValue;
                  goto set_var;
                end;
    adInteger:  begin
                  tagVariant(V).vt := VT_I4;
                  tagVariant(V).lVal := AValue;
                  goto set_var;
                end;
    adBigInt:   begin
                  tagVariant(V).vt := VT_I8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).llVal{$ELSE}PInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
                  goto set_var;
                end;
    adUnsignedTinyInt: begin
                  tagVariant(V).vt := VT_UI1;
                  tagVariant(V).bVal := AValue;
                  goto set_var;
                end;
    adUnsignedSmallInt: begin
                  tagVariant(V).vt := VT_UI2;
                  tagVariant(V).uiVal := AValue;
                  goto set_var;
                end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    adUnsignedBigInt: begin
                  tagVariant(V).vt := VT_UI8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).ullVal{$ELSE}PUInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
                  goto set_var;
                end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    adDate, adDBDate, adDBTime, adDBTimeStamp,
    adSingle, adDouble, adCurrency, adDecimal,
    adNumeric:  SetCurrency(Index, AValue);
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adUnsignedInt: begin
                  tagVariant(V).vt := VT_UI4;
                  tagVariant(V).ulVal := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        begin
                  IntToUnicode(AValue, PWideChar(fByteBuffer), @P);
                  SetPWideChar(Index, PWideChar(fByteBuffer), P - PWideChar(fByteBuffer));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetULong(Index: Integer;
  const AValue: UInt64);
var V: OleVariant;
  BCD: TBCD;
  PD: PDecimal absolute BCD;
  P: PWideChar absolute PD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong) of
    adTinyInt, adSmallInt, adInteger, adBigInt: SetLong(Index, AValue);
    adUnsignedTinyInt: begin
                  tagVariant(V).vt := VT_UI1;
                  tagVariant(V).bVal := AValue;
                  goto set_var;
                end;
    adUnsignedSmallInt: begin
                  tagVariant(V).vt := VT_UI2;
                  tagVariant(V).uiVal := AValue;
                  goto set_var;
                end;
    adUnsignedInt: begin
                  tagVariant(V).vt := VT_UI4;
                  tagVariant(V).ulVal := AValue;
                  goto set_var;
                end;
    adDate, adDBDate, adDBTime, adDBTimeStamp,
    adSingle, adDouble: SetDouble(Index, AValue);
    adCurrency: begin
                  tagVariant(V).vt := VT_CY;
                  tagVariant(V).cyVal := AValue;
                  goto set_var;
                end;
    adDecimal,
    adNumeric:  begin
                  tagVariant(V).vt := VT_DECIMAL;
                  PD := @V;
                  PD.scale := 0;
                  PUint64(@PD.Lo64)^ := AValue;
                  PD.sign := 0;
                  goto set_var;
                end;
    adBoolean:  begin
                  tagVariant(V).vt := VT_BOOL;
                  tagVariant(V).vbool := AValue <> 0;
                  goto set_var;
                end;
    adUnsignedBigInt: begin
                  tagVariant(V).vt := VT_UI8;
                  {$IFDEF WITH_tagVARIANT_UINT64}tagVariant(V).ullVal{$ELSE}PUInt64(@tagVariant(V).cyVal)^{$ENDIF} := AValue;
set_var:          FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    else        begin
                  IntToUnicode(AValue, PWideChar(fByteBuffer), @P);
                  SetPWideChar(Index, PWideChar(fByteBuffer), P - PWideChar(fByteBuffer));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetUnicodeString(Index: Integer;
  const AValue: UnicodeString);
begin
  SetPWideChar(Index, Pointer(AValue), Length(AValue));
end;

procedure TZAdoPreparedStatement.SetUTF8String(Index: Integer;
  const AValue: UTF8String);
begin
  FUniTemp := PRawToUnicode(Pointer(AValue), Length(aValue), zCP_UTF8);
  SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp));
end;

procedure TZAdoPreparedStatement.SetWord(Index: Integer; AValue: Word);
var V: OleVariant;
begin
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord) = adUnsignedSmallInt then begin
    tagVariant(V).vt := VT_UI2;
    tagVariant(V).uiVal := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetUInt(Index, AValue);
end;

{ TZAdoStatement }

constructor TZAdoStatement.Create(const Connection: IZConnection;
  const Info: TStrings);
begin
  inherited CreateWithCommandType(Connection, '', Info, adCmdText);
end;

{ TZAdoCallableStatement2 }

function TZAdoCallableStatement2.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
begin
  Result := TZAdoPreparedStatement.CreateWithCommandType(Connection, StoredProcName, Info, adCmdStoredProc);
  TZAdoPreparedStatement(Result).Prepare;
end;

initialization

SetLength(DefaultPreparableTokens, 6);
DefaultPreparableTokens[0].MatchingGroup := 'DELETE';
DefaultPreparableTokens[1].MatchingGroup := 'INSERT';
DefaultPreparableTokens[2].MatchingGroup := 'UPDATE';
DefaultPreparableTokens[3].MatchingGroup := 'SELECT';
DefaultPreparableTokens[4].MatchingGroup := 'CALL';
DefaultPreparableTokens[5].MatchingGroup := 'SET';
{$ENDIF ZEOS_DISABLE_ADO}
end.
