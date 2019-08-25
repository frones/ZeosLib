{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Statement Classes                   }
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

unit ZDbcAdoStatement;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}
{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  ZCompatibility, ZSysUtils, ZOleDB, FmtBCD,
  ZDbcIntfs, ZDbcStatement, ZDbcAdo, ZPlainAdo, ZVariant, ZDbcAdoUtils,
  ZDbcOleDBUtils, ZDbcOleDBStatement;

type
  {** Implements Prepared ADO Statement. }
  TZAbstractAdoStatement = Class(TZUTF16PreparedStatement)
  private
    FAdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FIsSelectSQL: Boolean;
    FCommandType: CommandTypeEnum;
    FRC: OleVariant;
  protected
    function CreateResultSet: IZResultSet; virtual;
    procedure ReleaseConnection; override;
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
    //FBatchStmt: TZOleDBPreparedStatement;
  protected
    function CheckParameterIndex(Index, ASize: Integer; SQLType: TZSQLType): DataTypeEnum; reintroduce;
    procedure PrepareInParameters; override;
    function CreateResultSet: IZResultSet; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
  public //setters
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
    procedure SetNull(Index: Integer; {%H-}SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; AValue: Boolean); reintroduce;
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
    procedure SetBigDecimal(Index: Integer; const AValue: TBCD); reintroduce;

    procedure SetPWideChar(Index: Word; Value: PWideChar; Len: Cardinal);
    procedure SetPBytes(Index: Word; AValue: PByte; Len: Cardinal);

    procedure SetCharRec(Index: Integer; const AValue: TZCharRec); reintroduce;
    procedure SetString(Index: Integer; const AValue: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(Index: Integer; const AValue: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(Index: Integer; const AValue: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const AValue: RawByteString); reintroduce;
    procedure SetUnicodeString(Index: Integer; const AValue: ZWideString); reintroduce;

    procedure SetDate(Index: Integer; const AValue: TDateTime); reintroduce;
    procedure SetTime(Index: Integer; const AValue: TDateTime); reintroduce;
    procedure SetTimestamp(Index: Integer; const AValue: TDateTime); reintroduce;

    procedure SetBytes(Index: Integer; const AValue: TBytes); reintroduce;
    procedure SetGUID(Index: Integer; const AValue: TGUID); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const AValue: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZAdoStatement = class(TZAbstractAdoStatement)
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  TZAdoCallableStatement2 = class(TZAbstractCallableStatement_W, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, Math, {$IFNDEF FPC}Windows{inline},{$ENDIF}
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  ZEncoding, ZDbcLogging, ZDbcCachedResultSet, ZDbcResultSet, ZFastCode,
  ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils, ZDbcAdoResultSet,
  ZMessages, ZDbcProperties;

{ TZAbstractAdoStatement }

function TZAbstractAdoStatement.CreateResultSet: IZResultSet;
var NativeResultSet: IZResultSet;
begin
  if Assigned(FAdoRecordset) and ((FAdoRecordSet.State and adStateOpen) = adStateOpen) then begin
    NativeResultSet := TZAdoResultSet.Create(Self, SQL, FAdoRecordSet);
    if ResultSetConcurrency = rcUpdatable then
      Result := TZCachedResultSet.Create(NativeResultSet, SQL,
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
    FAdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, adOptionUnspecified);
    LastResultSet := CreateResultSet;
    LastUpdateCount := {%H-}RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
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
  try
    if FIsSelectSQL then begin
      if (FAdoRecordSet = nil) or (FAdoRecordSet.MaxRecords <> MaxRows) then begin
        FAdoRecordSet := CoRecordSet.Create;
        FAdoRecordSet.MaxRecords := MaxRows;
        FAdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
        if (GetResultSetType = rtForwardOnly) or (GetResultSetConcurrency = rcUpdatable)
        then FAdoRecordSet.CursorLocation := adUseServer
        else FAdoRecordSet.CursorLocation := adUseClient;
        FAdoRecordSet.CacheSize := 128*1024;
        FAdoRecordSet.MaxRecords := MaxRows;
        FAdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockReadOnly, adOptionUnspecified);
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
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
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
    FAdoRecordSet := FAdoCommand.Execute(FRC, EmptyParam, adExecuteNoRecords);
    if BindList.HasOutOrInOutOrResultParam then
      LastResultSet := CreateResultSet;
    LastUpdateCount := FRC;
    Result := LastUpdateCount;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
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
begin
  if FAdoCommand = nil then begin
    FAdoCommand := CoCommand.Create;
    FAdoCommand.CommandText := WSQL;
    FAdoConnection := Connection as IZAdoConnection;
    FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  end;
  if Not Prepared then begin//prevent PrepareInParameters
    FIsSelectSQL := IsSelect(SQL);
    FAdoCommand.CommandText := WSQL;
    FAdoCommand.CommandType := FCommandType;
   // FAdoCommand.Properties['Defer Prepare'].Value := False;
    inherited Prepare;
    FAdoCommand.Prepared := True;
  end;
end;

procedure TZAbstractAdoStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FAdoConnection := nil;
end;

procedure TZAbstractAdoStatement.Unprepare;
begin
  FAdoRecordSet := nil;
  if Assigned(FAdoCommand) and FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  FAdoCommand := nil;
  inherited Unprepare;
end;

{ TZAdoPreparedStatement }

function TZAdoPreparedStatement.CheckParameterIndex(Index, ASize: Integer;
  SQLType: TZSQLType): DataTypeEnum;
var ParamDirection: ParameterDirectionEnum;
  I: Integer;
  W: WideString;
  V: OleVariant;
begin
  if not Prepared then Prepare;
  inherited CheckParameterIndex(Index);
  if FEmulatedParams then
    Result := adBSTR
  else if FRefreshParamsFailed then begin
    Result := ZSQLTypeToAdoType[SQLType];
    if (fAdoCommand.Parameters.Count < Index -1) then begin
      if BindList[Index].ParamType = pctUnknown
      then ParamDirection := adParamInput
      else ParamDirection := ZProcedureColumnType2AdoType[BindList[Index].ParamType];
      for I := fAdoCommand.Parameters.Count to Index do begin
        V := null;
        W := 'Param'+IntToUnicode(I+1);
        fAdoCommand.Parameters.Append(fAdoCommand.CreateParameter(W, adVariant, ParamDirection, SizeOf(OleVariant), V));
      end;
      fAdoCommand.Parameters.Append(fAdoCommand.CreateParameter('Param'+IntToUnicode(Index+1),
         ConvertSqlTypeToAdo(SQLType), ParamDirection, ASize, null));
    end else with fAdoCommand.Parameters[Index] do begin
      Type_ := Result;
      Size := ASize
    end
  end else begin
    if (Index <0) or (fAdoCommand.Parameters.Count < Index +1) then
      raise Exception.Create('Index out of bounds');
    Result := fAdoCommand.Parameters[Index].Type_;
  end;
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
  begin
    ColumnsInfo := TObjectList.Create;
    try
      for I := 0 to FAdoCommand.Parameters.Count -1 do
        if FAdoCommand.Parameters.Item[i].Direction in [adParamOutput,
          adParamInputOutput, adParamReturnValue] then begin
        ColumnInfo := TZColumnInfo.Create;
        with ColumnInfo do begin
          {$IFNDEF UNICODE}
          ColumnLabel := PUnicodeToString(Pointer(FAdoCommand.Parameters.Item[i].Name), Length(FAdoCommand.Parameters.Item[i].Name), ConSettings^.CTRL_CP);
          {$ELSE}
          ColumnLabel := FAdoCommand.Parameters.Item[i].Name;
          {$ENDIF}
          ColumnType := ConvertAdoToSqlType(FAdoCommand.Parameters.Item[I].Type_,
            FAdoCommand.Parameters.Item[I].Precision, FAdoCommand.Parameters.Item[I].NumericScale, ConSettings.CPType);
          Precision := FAdoCommand.Parameters.Item[I].Precision;
        end;
        ColumnsInfo.Add(ColumnInfo);
      end;

      RS := TZVirtualResultSet.CreateWithColumns(Self, ColumnsInfo, '', ConSettings);
      with RS do begin
        SetType(rtScrollInsensitive);
        SetConcurrency(rcReadOnly);
        RS.MoveToInsertRow;
        J := 0;
        for i := 0 to FAdoCommand.Parameters.Count -1 do begin
          with FAdoCommand.Parameters.Item[i],
               TZColumnInfo(ColumnsInfo[J]) do begin
            if Direction in [adParamOutput, adParamInputOutput, adParamReturnValue] then begin
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
                                          Blob := TZAbstractBlob.CreateWithData(P, VarArrayHighBound(Temp, 1)+1);
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
        RS.InsertRow;
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

procedure TZAdoPreparedStatement.PrepareInParameters;
var i: Integer;
begin
  { test if we can access the parameter collection }
  try
    if BindList.Count <> FAdoCommand.Parameters.Count then //this could cause an AV
      BindList.Count := FAdoCommand.Parameters.Count;
    FRefreshParamsFailed := False;
    SetLength(FInParamDefaultValues, BindList.Count);
    for I := 0 to BindList.Count -1 do
      with FAdoCommand.Parameters[i] do
        BindList.SetParamTypes(I, ConvertAdoToSqlType(Get_Type_, Get_Precision,
          Get_NumericScale, ConSettings.CPType), AdoType2ZProcedureColumnType[Get_Direction]);
  except { do not handle the exception
      tag ADO did fail to compute the paramter info's instead!
      an example: Insert into Foo Values (?,?),(?,?),(?,?) crash with ado but native oledb succeeds !
      So we add a parameter}
    FAdoCommand.Parameters.Append(FAdoCommand.CreateParameter('DummyParam', adVariant, adParamInput, SizeOf(OleVariant), null));
    FAdoCommand.Parameters.Delete('DummyParam');
    FRefreshParamsFailed := True;
  end;
end;

procedure TZAdoPreparedStatement.SetAnsiString(Index: Integer;
  const AValue: AnsiString);
begin
  FUniTemp := PRawToUnicode(Pointer(AValue), Length(aValue), ZOSCodePage);
  SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp));
end;

procedure TZAdoPreparedStatement.SetBigDecimal(Index: Integer;
  const AValue: TBCD);
var V: OleVariant;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(tagDec), stBigDecimal) of
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
    else        SetPWideChar(Index, @fWBuffer[0], ZSysUtils.BcdToUni(AValue, @fWBuffer[0], '.'));
  end;
end;

procedure TZAdoPreparedStatement.SetBlob(Index: Integer;
  SQLType: TZSQLType; const AValue: IZBlob);
var P: Pointer;
  Lob: IZBlob;
begin
  Lob := AValue; //inc refcnt else FPC leaks many memory
  if (AValue = nil) or (aValue.IsEmpty) then
    SetNull(Index, SQLType)
  else case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, 0, SQLType) of
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: if Lob.IsClob then begin
                      P := Lob.GetPWideChar;
                      SetPWideChar(Index, P, Lob.Length shr 1);
                    end else begin
                      fUniTemp := ZDbcUtils.GetSQLHexWideString(Lob.GetBuffer, Lob.Length, True);
                      SetPWideChar(Index, Pointer(fUniTemp), Length(fUniTemp));
                    end;
    adBinary,
    adVarBinary,
    adLongVarBinary: SetPBytes(Index, Lob.GetBuffer, Lob.Length);
  end;
end;

procedure TZAdoPreparedStatement.SetBoolean(Index: Integer;
  AValue: Boolean);
var V: OleVariant;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(WordBool), stBoolean) of
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
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Byte), stByte) = adUnsignedTinyInt then begin
    tagVariant(V).vt := VT_UI1;
    tagVariant(V).bVal := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetUInt(Index, AValue);
end;

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
    case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, stBytes) of
      adGUID: begin
                Assert(L=SizeOf(TGUID), 'UID-Size missmatch');
                GUIDToBuffer(@UID.D1, PWideChar(@fWBuffer[0]), [guidWithBrackets, guidSet0Term]);
                SetPWideChar(Index, @fWBuffer[0], 38); //ad GUID is a BSTR?
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
  const AValue: TZCharRec);
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
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Currency), stCurrency) of
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
                  CurrToUnicode(AValue, @fWBuffer[0], @PD);
                  SetPWideChar(Index, @fWBuffer[0], PWideChar(PD) - PWideChar(@fWBuffer[0]));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetDate(Index: Integer;
  const AValue: TDateTime);
var V: OleVariant;
label jmp_assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Double), stDate) of
    adDBDate, adDBTime, adDBTimeStamp:
                with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
                  if Size <> 8 then //size is initializesd to ole record sizes are not 8
                    Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
                  goto jmp_Assign;
                end;
    adDate:     begin
jmp_Assign:       V := null;
                  tagVariant(V).vt := VT_DATE;
                  tagVariant(V).date := AValue;
                  FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, @fWBuffer[0], DateTimeToUnicodeSQLDate(AValue, @fWBuffer[0], ConSettings.WriteFormatSettings, False));
    else            SetDouble(Index, AValue);
  end;
end;

procedure TZAdoPreparedStatement.SetDouble(Index: Integer;
  const AValue: Double);
var V: OleVariant;
  BCD: TBCD;
label set_var;
begin
  V := null;
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Double), stDouble) of
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
    else        SetPWideChar(Index, @fWBuffer[0], ZSysUtils.FloatToUnicode(AValue, @fWBuffer[0]));
  end;
end;

procedure TZAdoPreparedStatement.SetFloat(Index: Integer;
  AValue: Single);
var V: OleVariant;
  BCD: TBCD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Single), stFloat) of
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
    else        SetPWideChar(Index, @fWBuffer[0], ZSysUtils.FloatToUnicode(AValue, @fWBuffer[0]));
  end;
end;

procedure TZAdoPreparedStatement.SetGUID(Index: Integer;
  const AValue: TGUID);
begin
  GUIDToBuffer(@AValue.D1, PWideChar(@fWBuffer[0]), [guidWithBrackets, guidSet0Term]);
  SetPWideChar(Index, @fWBuffer[0], 38); //ad GUID is a BSTR?
end;

procedure TZAdoPreparedStatement.SetInt(Index, AValue: Integer);
var V: OleVariant;
  PD: PDecimal;
  P: PWideChar absolute PD;
label set_var;
begin
  V := null;
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Integer), stInteger) of
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
                  IntToUnicode(AValue, @fWBuffer[0], @P);
                  SetPWideChar(Index, @fWBuffer[0], P - PWideChar(@fWBuffer[0]));
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
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Int64), stLong) of
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
                  IntToUnicode(AValue, @fWBuffer[0], @P);
                  SetPWideChar(Index, @fWBuffer[0], P - PWideChar(@fWBuffer[0]));
                  Exit;
                end;
  end;
end;

const cNullUni = ZWideString('null');
procedure TZAdoPreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1{$ENDIF};
  CheckParameterIndex(Index, ZSQLTypeToAdoParamSize[SQLType], SQLType);
  if FEmulatedParams
  then BindList.Put(Index, SQLType, cNullUni)
  else FAdoCommand.Parameters[Index].Value := null;
end;

procedure TZAdoPreparedStatement.SetPBytes(Index: Word; AValue: PByte;
  Len: Cardinal);
var V: OleVariant;
begin
  V := VarArrayCreate([0, Len - 1], varByte);
  Move(AValue^, TVarData(V).VArray.Data^, Len);
  FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
end;

procedure TZAdoPreparedStatement.SetPWideChar(Index: Word;
  Value: PWideChar; Len: Cardinal);
var V: OleVariant;
  BCD: TBCD;
  D: Double absolute BCD;
  C: Currency absolute BCD;
  Failed: Boolean;
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
label set_dbl, set_BSTR;
begin
  if FEmulatedParams then
    SetEmulatedValue
  else case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Len, stUnicodeString) of
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
    adDate:             begin
                          D := UnicodeSQLTimeStampToDateTime(Value, Len, Consettings.WriteFormatSettings, Failed);
                          goto set_dbl;
                        end;
    adDBDate:           begin
                          D := UnicodeSQLDateToDateTime(Value, Len, Consettings.WriteFormatSettings, Failed);
                          goto set_dbl;
                        end;
    adDBTime:           begin
                          D := UnicodeSQLTimeToDateTime(Value, Len, Consettings.WriteFormatSettings, Failed);
set_dbl:                  if Failed then D := 0;
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
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, stString) of
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
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(ShortInt), stShort) = adTinyInt then begin
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
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(SmallInt), stSmall) = adSmallInt then begin
    tagVariant(V).vt := VT_I2;
    tagVariant(V).iVal := AValue;
    FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
  end else
    SetInt(Index, AValue);
end;

procedure TZAdoPreparedStatement.SetString(Index: Integer;
  const AValue: String);
begin
  {$IFDEF UNICODE}
  SetPWideChar(Index, Pointer(AValue), Length(AValue));
  {$ELSE}
  if Consettings.AutoEncode
  then fUniTemp := ConSettings.ConvFuncs.ZStringToUnicode(AValue, ConSettings^.CTRL_CP)
  else fUniTemp := PRawToUnicode(Pointer(AValue), Length(AValue), FClientCP);
  SetPWideChar(Index, Pointer(fUniTemp), Length(fUniTemp));
  {$ENDIF}
end;

procedure TZAdoPreparedStatement.SetTime(Index: Integer;
  const AValue: TDateTime);
var V: OleVariant;
label jmp_assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Double), stTime) of
    adDBDate, adDBTime, adDBTimeStamp:
                with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
                  if Size <> 8 then //size is initializesd to ole record sizes which are not 8
                    Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
                  goto jmp_Assign;
                end;
    adDate:     begin
jmp_assign:       V := null;
                  tagVariant(V).vt := VT_DATE;
                  tagVariant(V).date := AValue;
                  FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
                end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, @fWBuffer[0], DateTimeToUnicodeSQLTime(AValue, @fWBuffer[0], ConSettings.WriteFormatSettings, False));
    else            SetDouble(Index, AValue);
  end;
end;

procedure TZAdoPreparedStatement.SetTimestamp(Index: Integer;
  const AValue: TDateTime);
var V: OleVariant;
label jmp_Assign;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Double), stTimeStamp) of
    adDBTimeStamp, adDBDate, adDBTime:
            with FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
              if Size <> 8 then //size is initializesd to ole record sizes are not 8
                Size := 8; // but we can't assign it using the OleVariants -> set to Dbl size
              goto jmp_Assign;
            end;
    adDate: begin
jmp_Assign:   V := null;
              tagVariant(V).vt := VT_DATE;
              tagVariant(V).date := AValue;
              FAdoCommand.Parameters[Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value := V;
            end;
    adBSTR, adChar, adVarChar, adLongVarChar, adWChar, adVarWChar,
    adLongVarWChar: SetPWideChar(Index, @fWBuffer[0], DateTimeToUnicodeSQLTimeStamp(AValue, @fWBuffer[0], ConSettings.WriteFormatSettings, False));
    else            SetDouble(Index, AValue);
  end;
end;

procedure TZAdoPreparedStatement.SetUInt(Index: Integer;
  AValue: Cardinal);
var V: OleVariant;
  PD: PDecimal;
  P: PWideChar absolute PD;
label set_var;
begin
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Cardinal), stLongWord) of
    adTinyInt:  begin
                  tagVariant(V).vt := VT_I1;
                  PShortInt(@tagVariant(V).cVal)^ := AValue;
                  goto set_var;
                end;
    adSmallInt: begin
                  tagVariant(V).vt := VT_I2;
                  tagVariant(V).iVal := AValue;
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
                  IntToUnicode(AValue, @fWBuffer[0], @P);
                  SetPWideChar(Index, @fWBuffer[0], P - PWideChar(@fWBuffer[0]));
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
  case CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Uint64), stULong) of
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
                  IntToUnicode(AValue, @fWBuffer[0], @P);
                  SetPWideChar(Index, @fWBuffer[0], P - PWideChar(@fWBuffer[0]));
                end;
  end;
end;

procedure TZAdoPreparedStatement.SetUnicodeString(Index: Integer;
  const AValue: ZWideString);
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
  if CheckParameterIndex(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SizeOf(Word), stWord) = adUnsignedSmallInt then begin
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

{$ENDIF ZEOS_DISABLE_ADO}
end.
