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
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZSysUtils, ZDbcIntfs, ZDbcOracle, ZDbcResultSet, ZPlainOracleDriver,
  ZDbcResultSetMetadata, ZDbcLogging, ZCompatibility, ZDbcOracleUtils,
  ZPlainOracleConstants, ZPlainDriver, ZDbcStatement;

type
  {** Implements Oracle ResultSet. }
  TZOracleAbstractResultSet_A = class(TZAbstractReadOnlyResultSet_A)
  private
    FStmtHandle: POCIStmt;
    FErrorHandle: POCIError;
    FConnectionHandle: POCIEnv;
    FOCISvcCtx: POCISvcCtx;
    FPlainDriver: TZOraclePlainDriver;
    FConnection: IZOracleConnection;
    FColumns: PZSQLVars;
    FChunkSize: Integer;
    FIteration: Integer; //Max count of rows which fit into BufferSize <= FZBufferSize
    FCurrentRowBufIndex: Cardinal; //The current row in buffer! NOT the current row of RS
    FZBufferSize: Integer; //max size for multiple rows. If Row > Value ignore it!
    FRowsBuffer: TByteDynArray; //Buffer for multiple rows if possible which is reallocated or freed by IDE -> mem leak save!
    FTempLob: IZBlob;
    FClientCP: Word;
    Fbufsize: UB4; //a temporary variable uses for Number2Text
    fStatus: Sword;
    FvnuInfo: TZvnuInfo;
    function GetFinalObject(Obj: POCIObject): POCIObject;
  public
    constructor Create(
      const Statement: IZStatement; const SQL: string;
      const StmtHandle: POCIStmt; const ErrorHandle: POCIError;
      const ZBufferSize: Integer);
    procedure BeforeClose; override;
    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    {$IFDEF BCD_TEST}
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    {$ELSE}
    function GetBigDecimal(ColumnIndex: Integer): Extended;
    {$ENDIF}
    function GetBytes(ColumnIndex: Integer): TBytes;
    function GetDate(ColumnIndex: Integer): TDateTime;
    function GetTime(ColumnIndex: Integer): TDateTime;
    function GetTimestamp(ColumnIndex: Integer): TDateTime;
    function GetDataSet(ColumnIndex: Integer): IZDataSet; override;
    function GetBlob(ColumnIndex: Integer): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions); reintroduce;
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZOracleResultSet_A = class(TZOracleAbstractResultSet_A, IZResultSet)
  private
    FMaxBufIndex: Integer;
  protected
    procedure Open; override;
  public
    procedure BeforeClose; override;
    function Next: Boolean; reintroduce;
  end;

  TZOracleCallableResultSet_A = Class(TZOracleAbstractResultSet_A, IZResultSet)
  private
    FFieldNames: TStringDynArray;
  public
    constructor Create(
      const Statement: IZStatement; const SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; OraVariables: PZOCIParamBinds;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}BindList: TZBindList);
  protected
    procedure Open; override;
  public
    function Next: Boolean; reintroduce;
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
  TZOracleBlob = class(TZAbstractBlob, IZOracleBlob)
  private
    FOCISvcCtx: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FNativePlainDriver: IZPlainDriver; //keep alive
    FPlainDriver: TZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
    FConSettings: PZConSettings;
  protected
    procedure InternalSetData(AData: Pointer; ASize: Integer);
  public
    constructor Create(const PlainDriver: TZOraclePlainDriver;
      const Data: Pointer; const Size: Int64; const OCISvcCtx: POCISvcCtx;
      const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
      const ChunkSize: Integer; const ConSettings: PZConSettings);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; //override;
    procedure WriteLob; //override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

  {EH: my current uncached implementation doesn't work here since we've no
   scrollable RS}
  {** Implements external blob wrapper object for Oracle. }
  TZOracleClob = class(TZAbstractCLob, IZOracleBlob)
  private
    FOCISvcCtx: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FConnectionHandle: POCIEnv;
    FNativePlainDriver: IZPlainDriver; //keep alive
    FPlainDriver: TZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
  public
    constructor Create(const PlainDriver: TZOraclePlainDriver;
      const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
      const OCISvcCtx: POCISvcCtx; const ErrorHandle: POCIError;
      const LobLocator: POCILobLocator; const ChunkSize: Integer;
      const ConSettings: PZConSettings; const CodePage: Word);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; //override;
    procedure WriteLob; //override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} ZFastCode,
  ZMessages, ZEncoding, ZClasses, FmtBCD, ZDbcUtils;

const sql_fmt: PAnsiChar = ('TM9');
const sql_fmt_length: ub4 = 3;      //000000000000009.9000
const sql_nls_params: PAnsiChar = ('NLS_NUMERIC_CHARACTERS=''.,''');
const sql_nls_p_length: ub4 = 27;

{ TZOracleAbstractResultSet_A }

{$IFDEF USE_SYNCOMMONS}
procedure TZOracleAbstractResultSet_A.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var
    P: PAnsiChar;
    C, H, I: SmallInt;
  Month, Day: Byte;
  Hour, Minute, Second: Byte;
  Year: SmallInt;
  Millis: Cardinal;
  procedure AddJSONEscape(P: PAnsichar; Len: LengthInt);
  begin
    JSONWriter.Add('"');
    if FClientCP = zCP_UTF8 then
      JSONWriter.AddJSONEscape(P, Len)
    else begin
      FUniTemp := PRawToUnicode(P, Len, FClientCP);
      JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), System.Length(FUniTemp));
    end;
    JSONWriter.Add('"');
  end;
begin
  //init
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    {$R-}
    with FColumns^.Variables[C] do
    if (valuep = nil) or (indp^[FCurrentRowBufIndex] < 0) then begin
      if not (jcsSkipNulls in JSONComposeOptions) then begin
        if JSONWriter.Expand then
          JSONWriter.AddString(JSONWriter.ColNames[I]);
        JSONWriter.AddShort('null,')
      end;
    end else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      P := valuep+(FCurrentRowBufIndex*value_sz);
      case dty of
        { the ordinals we support }
        SQLT_INT        : case value_sz of
                            SizeOf(Int64): JSONWriter.Add(PInt64(P)^);
                            SizeOf(Integer): JSONWriter.Add(PInteger(P)^);
                            SizeOf(SmallInt): JSONWriter.Add(PSmallInt(P)^);
                            else JSONWriter.Add(PShortInt(P)^);
                          end;
        SQLT_UIN        : case value_sz of
                            SizeOf(UInt64): JSONWriter.Add(PUInt64(P)^);
                            SizeOf(Cardinal): JSONWriter.Add(PCardinal(P)^);
                            SizeOf(Word): JSONWriter.Add(PWord(P)^);
                            else JSONWriter.Add(PByte(P)^);
                          end;
        { the FPU floting values we support }
        SQLT_FLT        : if value_sz = SizeOf(Double)
                          then JSONWriter.AddDouble(PDouble(P)^)
                          else JSONWriter.AddSingle(PSingle(P)^);
        SQLT_BDOUBLE    : JSONWriter.AddDouble(PDouble(P)^);
        SQLT_BFLOAT     : JSONWriter.AddSingle(PSingle(P)^);
        { the oracle soft decimal }
        SQLT_VNU        : case ZDbcOracleUtils.nvuKind(POCINumber(P), FvnuInfo) of
                            nvu0: JSONWriter.Add('0');
                            vnuNegInt: JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], NegOrdNVU2Raw(POCINumber(P), FvnuInfo, @FTinyBuffer[0]));
                            vnuPosInt: JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PosOrdNVU2Raw(POCINumber(P), FvnuInfo, @FTinyBuffer[0]));
                            vnuPosCurr: JSONWriter.AddCurr64(PosNvu2Curr(POCINumber(P), FvnuInfo));
                            //vnuPosCurr: JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PosNVUCurr2Raw(POCINumber(P), FvnuInfo, @FTinyBuffer[0]));
                            vnuNegCurr: JSONWriter.AddCurr64(NegNvu2Curr(POCINumber(P), FvnuInfo));
                            //vnuNegCurr: JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], NegNVUCurr2Raw(POCINumber(P), FvnuInfo, @FTinyBuffer[0]));
                            nvuNegInf: JSONWriter.AddShort('"-Infinity"');
                            nvuPosInf: JSONWriter.AddShort('"Infinity"');
                            else begin
                              FStatus:= FPlainDriver.OCINumberToReal(FErrorHandle, POCINumber(P), SizeOf(Double), @FTinyBuffer[0]);
                              if FStatus = OCI_Success
                              then JSONWriter.AddDouble(PDouble(@FTinyBuffer[0])^)
                              else CheckOracleError(FPLainDriver, FErrorHandle, FStatus, lcOther, 'OCINumberToReal', ConSettings);
                            end;
                          end;
        { the charter types we support }
        SQLT_VCS        : AddJSONEscape(@POCIVary(P).Data[0], POCIVary(P).Len);
        SQLT_LVC        : AddJSONEscape(@POCILong(P).Data[0], POCILong(P).Len);
        SQLT_VST        : AddJSONEscape(@PPOCILong(P)^.data[0], PPOCILong(P)^.Len);
        { fixed char right ' ' padded }
        SQLT_AFC        : AddJSONEscape(P, GetAbsorbedTrailingSpacesLen(P, Value_sz));
        { the binary raw we support }
        SQLT_LVB        : JSONWriter.WrBase64(@POCILong(P).data[0], POCILong(P).Len, True);
        SQLT_BIN        : JSONWriter.WrBase64(P, value_sz, True);
        SQLT_VBI        : JSONWriter.WrBase64(@POCIVary(P).data[0], POCIVary(P).Len, True);
        { date /+ time we do support }
        SQLT_DAT        : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            if POraDate(P)^.Cent < 100 then
                              JSONWriter.Add('-');
                            if ColType <> stTime then begin
                              DateToIso8601PChar(@FTinyBuffer[0], True, (POraDate(P)^.Cent-100)*100+POraDate(P)^.Year-100,
                                POraDate(P)^.month, POraDate(P)^.day);
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],10);
                            end else if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('0000-00-00');
                            if (ColType <> stDate) then begin
                              TimeToIso8601PChar(@FTinyBuffer[0], True, POraDate(P)^.Hour-1,
                                POraDate(P)^.Min-1,POraDate(P)^.Sec-1, 0, 'T', jcoMilliseconds in JSONComposeOptions);
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],8 + (4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            end;
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
        SQLT_TIMESTAMP: begin
                          if jcoMongoISODate in JSONComposeOptions then
                            JSONWriter.AddShort('ISODate("')
                          else if jcoDATETIME_MAGIC in JSONComposeOptions then
                            JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                          else
                            JSONWriter.Add('"');
                          if (ColType <> stTime) and (FPlainDriver.OCIDateTimeGetDate(FConnectionHandle,
                             FErrorHandle, PPOCIDescriptor(P)^, Year{%H-}, Month{%H-}, Day{%H-}) = OCI_SUCCESS) and
                             (not ((Year=1) and (Month=1) and (Day=1))) then begin
                          // attention : this code handles all timestamps on 01/01/0001 as a pure time value
                          // reason : oracle doesn't have a pure time datatype so all time comparisons compare
                          //          TDateTime values on 30 Dec 1899 against oracle timestamps on 01 januari 0001 (negative TDateTime)
                            DateToIso8601PChar(@FTinyBuffer[0], True, Abs(Year), Month, Day);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],10);
                          end else if jcoMongoISODate in JSONComposeOptions then
                            JSONWriter.AddShort('0000-00-00');
                          if (ColType <> stDate) and (FPlainDriver.OCIDateTimeGetTime(FConnectionHandle,
                             FErrorHandle, {%H-}PPOCIDescriptor(P)^, Hour{%H-}, Minute{%H-}, Second{%H-}, Millis{%H-}) = OCI_SUCCESS) then begin
                            TimeToIso8601PChar(@FTinyBuffer[0], True, Hour, Minute, Second,
                              Millis div 1000000, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],8 + (4*Ord(jcoMilliseconds in JSONComposeOptions)));
                          end;
                          if jcoMongoISODate in JSONComposeOptions
                          then JSONWriter.AddShort('Z)"')
                          else JSONWriter.Add('"');
                        end;
        SQLT_INTERVAL_DS,
        SQLT_INTERVAL_YM: begin
                            JSONWriter.Add('"');
                            JSONWriter.AddDateTime(GetTimeStamp(C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}));
                            JSONWriter.Add('"');
                          end;
        { large object support }
        SQLT_BLOB,
        SQLT_BFILEE,
        SQLT_CFILEE     : begin
                            FTempLob := GetBlob(C+{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                            JSONWriter.WrBase64(FTempLob.GetBuffer, FTempLob.Length, True);
                            FTempLob := nil;
                          end;
        SQLT_CLOB       : begin
                            JSONWriter.Add('"');
                            FTempLob := GetBlob(C{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                            P := FTempLob.GetPAnsiChar(zCP_UTF8);
                            JSONWriter.AddJSONEscape(P, FTempLob.Length);
                            FTempLob := nil;
                            JSONWriter.Add('"');
                          end;
        else
          raise Exception.Create('Missing OCI Type: '+IntToStr(dty));
      end;
      JSONWriter.Add(',');
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a Oracle plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a Oracle specific query handle.
}
procedure TZOracleAbstractResultSet_A.BeforeClose;
begin
  FreeOracleSQLVars(FPlainDriver, FColumns, FIteration, FConnectionHandle,
    FErrorHandle, ConSettings);
  inherited BeforeClose;
end;

constructor TZOracleAbstractResultSet_A.Create(
  const Statement: IZStatement;
  const SQL: string; const StmtHandle: POCIStmt; const ErrorHandle: POCIError;
  const ZBufferSize: Integer);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);

  FStmtHandle := StmtHandle;
  FErrorHandle := ErrorHandle;
  FPlainDriver := TZOraclePlainDriver(Statement.GetConnection.GetIZPlainDriver.GetInstance);
  ResultSetConcurrency := rcReadOnly;
  FConnection := Statement.GetConnection as IZOracleConnection;
  FConnectionHandle := FConnection.GetConnectionHandle;
  FOCISvcCtx := FConnection.GetServiceContextHandle;
  FChunkSize := Statement.GetChunkSize;
  FIteration := 1;
  FCurrentRowBufIndex := 0;
  FZBufferSize := ZBufferSize;
  FClientCP := ConSettings^.ClientCodePage^.CP;
  Open;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZOracleAbstractResultSet_A.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > Integer(FColumns^.AllocNum{$IFDEF GENERIC_INDEX}-1{$ENDIF})) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
{$ENDIF}
  {$R-}
  Result := (FColumns^.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].valuep = nil) or
            (FColumns^.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].indp^[FCurrentRowBufIndex] < 0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
const rInfinity: RawbyteString = 'Infinity';
const rNegInfinity: RawbyteString = '-Infinity';
function TZOracleAbstractResultSet_A.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUint): PAnsiChar;
var SQLVarHolder: PZSQLVar;
label dbl, sin, set_Result;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := nil;
    Len := 0;
  end else begin
    Result := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: Len := GetAbsorbedTrailingSpacesLen(Result, SQLVarHolder.Value_sz);
      SQLT_VST: begin
                  Len := PPOCILong(Result)^.Len;
                  Result := @PPOCILong(Result)^.data[0];
                end;
      SQLT_VCS: begin
                  Len := POCIVary(Result).Len;
                  Result := PAnsiChar(@POCIVary(Result).data[0]);
                end;
      SQLT_LVC: begin
                  Len := POCILong(Result).Len;
                  Result := PAnsiChar(@POCILong(Result).data[0]);
                end;
      { the oracle soft decimal }
      SQLT_VNU:
        case nvuKind(POCINumber(Result), FvnuInfo) of
          nvu0: begin
              FTinyBuffer[0] := Ord('0');
              Len := 1;
              Result := @FTinyBuffer[0];
            end;
          nvuNegInf: begin
              Result := Pointer(rNegInfinity);
              Len := 9;
            end;
          nvuPosInf: begin
              Result := Pointer(rInfinity);
              Len := 8;
            end;
          vnuNegInt: begin
              Len := NegOrdNVU2Raw(POCINumber(Result), FvnuInfo, @FTinyBuffer[0]);
              Result := @FTinyBuffer[0];
            end;
          vnuPosInt: begin
              Len := PosOrdNVU2Raw(POCINumber(Result), FvnuInfo, @FTinyBuffer[0]);
              Result := @FTinyBuffer[0];
            end;
          vnuPosCurr: begin
              CurrToRaw(PosNvu2Curr(POCINumber(Result), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          vnuNegCurr: begin
              CurrToRaw(NegNvu2Curr(POCINumber(Result), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          else begin
              Fbufsize := SizeOf(FTinyBuffer);
              fStatus := FplainDriver.OCINumberToText(FErrorHandle, POCINumber(Result),
                sql_fmt, sql_fmt_length, sql_nls_params, sql_nls_p_length, @Fbufsize, @FTinyBuffer[0]);
              if fStatus <> OCI_SUCCESS then
                CheckOracleError(FPlainDriver, FErrorHandle, fStatus, lcOther,
                      'OCINumberToText', ConSettings);
              Len := Fbufsize;
              Result := @FTinyBuffer[0];
            end;
        end;
      { the ordinals we yet do support }
      SQLT_INT: begin
                  case SQLVarHolder.value_sz of
                    SizeOf(Int64): IntToRaw(PInt64(Result)^, @FTinyBuffer[0], @Result);
                    SizeOf(Integer): IntToRaw(PInteger(Result)^, @FTinyBuffer[0], @Result);
                    SizeOf(SmallInt): IntToRaw(PSmallInt(Result)^, @FTinyBuffer[0], @Result);
                    else IntToRaw(PShortInt(Result)^, @FTinyBuffer, @Result[0]);
                  end;
                  goto set_Result;
                end;
      SQLT_UIN: begin
                  case SQLVarHolder.value_sz of
                    SizeOf(UInt64): IntToRaw(PUInt64(Result)^, @FTinyBuffer[0], @Result);
                    SizeOf(Cardinal): IntToRaw(PCardinal(Result)^, @FTinyBuffer[0], @Result);
                    SizeOf(SmallInt): IntToRaw(Cardinal(PWord(Result)^), @FTinyBuffer[0], @Result);
                    else IntToRaw(Cardinal(PByte(Result)^), @FTinyBuffer, @Result[0]);
                  end;
set_Result:       Len := Result - @FTinyBuffer[0];
                  Result := @FTinyBuffer[0];
                end;
      { the FPU floats we do support }
      SQLT_BFLOAT:  goto sin;
      SQLT_BDOUBLE: goto dbl;
      SQLT_FLT: begin
                  if SQLVarHolder^.value_sz = SizeOf(Double) then
      dbl:          Len := FloatToSQLRaw(PDouble(Result)^, @FTinyBuffer)
                  else
      sin:          Len := FloatToSQLRaw(PSingle(Result)^, @FTinyBuffer);
                  Result := @FTinyBuffer[0];
                end;
      { the binary raw we support }
      SQLT_VBI: begin
                  Len := POCIVary(Result)^.Len;
                  Result := @POCIVary(Result)^.data[0];
                end;
      SQLT_LVB: begin
                  Len := POCILong(Result)^.Len;
                  Result := @POCILong(Result)^.data[0];
                end;
      { the date/time types we support }
      SQLT_DAT,
      SQLT_INTERVAL_DS,
      SQLT_INTERVAL_YM,
      SQLT_TIMESTAMP_TZ,
      SQLT_TIMESTAMP_LTZ,
      SQLT_TIMESTAMP: begin
                  ZSysUtils.DateTimeToRawSQLTimeStamp(GetTimeStamp(ColumnIndex),
                    @FTinyBuffer[0], ConSettings^.ReadFormatSettings, False);
                  Len := ConSettings^.ReadFormatSettings.DateTimeFormatLen;
                end;
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          FTempLob  := GetBlob(ColumnIndex);
          Result    := FTempLob.GetBuffer;
          Len       := FTempLob.Length;
        end;
      SQLT_CLOB:
        begin
          FTempLob  := GetBlob(ColumnIndex);
          Result    := FTempLob.GetPAnsiChar(FClientCP);
          Len       := FTempLob.Length;
        end;
      else
        raise Exception.Create('Unsupported OCI Type? '+ZFastCode.IntToStr(SQLVarHolder^.dty));
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of UCS2 string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
const
  wNegInfinity: ZWideString = '-Infinity';
  wInfinity: ZWideString = 'Infinity';

function TZOracleAbstractResultSet_A.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  D: Double;
label dbl, sin, set_from_tmp, set_Result;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := nil;
    Len := 0;
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: begin
                  FUniTemp := PRawToUnicode(P, GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz), FClientCP);
                  goto set_from_tmp;
                end;
      SQLT_VST: begin
                  FUniTemp := PRawToUnicode(@PPOCILong(P)^.data[0], PPOCILong(P)^.Len, FClientCP);
                  goto set_from_tmp;
                end;
      SQLT_VCS: begin
                  FUniTemp := PRawToUnicode(@POCIVary(P)^.data[0], POCIVary(P)^.Len, FClientCP);
                  goto set_from_tmp;
                end;
      SQLT_LVC: begin
                  FUniTemp := PRawToUnicode(@POCILong(P)^.data[0], POCILong(P)^.Len, FClientCP);
                  goto set_from_tmp;
                end;
      { the oracle soft decimal }
      SQLT_VNU:
        case nvuKind(POCINumber(P), FvnuInfo) of
          nvu0: begin
              PWord(@FTinyBuffer[0])^ := Ord('0');
              Len := 1;
              Result := @FTinyBuffer[0];
            end;
          nvuNegInf: begin
              Result := Pointer(wNegInfinity);
              Len := 9;
            end;
          nvuPosInf: begin
              Result := Pointer(wInfinity);
              Len := 8;
            end;
          vnuNegInt: begin
              IntToUnicode(NegNvu2Int(POCINumber(P), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          vnuPosInt: begin
              IntToUnicode(PosNvu2Int(POCINumber(P), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          vnuPosCurr: begin
              CurrToUnicode(PosNvu2Curr(POCINumber(P), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          vnuNegCurr: begin
              CurrToUnicode(NegNvu2Curr(POCINumber(P), FvnuInfo), @FTinyBuffer[0], @Result);
              goto set_Result;
            end;
          else begin
              Result := @FTinyBuffer[0];
              fStatus := FplainDriver.OCINumberToReal(FErrorHandle, POCINumber(P), SizeOf(Double), @D);
              if fStatus <> OCI_SUCCESS then
                CheckOracleError(FPlainDriver, FErrorHandle, fStatus, lcOther, 'OCINumberToReal', ConSettings);
              Len := FloatToSQLUnicode(D, @FTinyBuffer)
            end;
        end;
      { the ordinals we yet do support }
      SQLT_INT: begin
                  case SQLVarHolder.value_sz of
                    SizeOf(Int64): IntToUnicode(PInt64(P)^, @FTinyBuffer[0], @Result);
                    SizeOf(Integer): IntToUnicode(PInteger(P)^, @FTinyBuffer[0], @Result);
                    SizeOf(SmallInt): IntToUnicode(PSmallInt(P)^, @FTinyBuffer[0], @Result);
                    else IntToUnicode(PShortInt(P)^, @FTinyBuffer, @Result[0]);
                  end;
                  goto set_Result;
                end;
      SQLT_UIN: begin
                  case SQLVarHolder.value_sz of
                    SizeOf(UInt64): IntToUnicode(PUInt64(P)^, @FTinyBuffer[0], @Result);
                    SizeOf(Cardinal): IntToUnicode(PCardinal(P)^, @FTinyBuffer[0], @Result);
                    SizeOf(Word): IntToUnicode(Cardinal(PWord(P)^), @FTinyBuffer[0], @Result);
                    else IntToUnicode(Cardinal(PByte(P)^), @FTinyBuffer, @Result[0]);
                  end;
set_Result:       Len := Result - PWideChar(@FTinyBuffer[0]);
                  Result := @FTinyBuffer[0];
                end;
      { the FPU floats we du support }
      SQLT_BFLOAT:  goto sin;
      SQLT_BDOUBLE: goto dbl;
      SQLT_FLT: begin
                  if SQLVarHolder^.value_sz = SizeOf(Double) then
      dbl:          Len := FloatToSQLUnicode(PDouble(P)^, @FTinyBuffer)
                  else
      sin:          Len := FloatToSQLUnicode(PSingle(P)^, @FTinyBuffer);
                  Result := @FTinyBuffer
                end;
      { the binary raw we support }
      SQLT_VBI: begin
                  Len := POCIVary(P)^.Len;
                  Result := @POCIVary(P)^.data[0];
                end;
      SQLT_LVB: begin
                  Len := POCILong(P)^.Len;
                  Result := @POCILong(P)^.data[0];
                end;
      { the date/time types we support }
      SQLT_DAT,
      SQLT_INTERVAL_DS,
      SQLT_INTERVAL_YM,
      SQLT_TIMESTAMP_TZ,
      SQLT_TIMESTAMP_LTZ,
      SQLT_TIMESTAMP: begin
                  ZSysUtils.DateTimeToUnicodeSQLTimeStamp(GetTimeStamp(ColumnIndex),
                    @FTinyBuffer, ConSettings^.ReadFormatSettings, False);
                  Result := @FTinyBuffer;
                  Len := ConSettings^.ReadFormatSettings.DateTimeFormatLen;
                end;
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          FTempLob  := GetBlob(ColumnIndex);
          FUniTemp := Ascii7ToUnicodeString(FTempLob.GetBuffer, FTempLob.Length);
set_from_tmp:
          Len := Length(FUniTemp);
          if Len = 0
          then Result := PEmptyUnicodeString
          else Result := Pointer(FUniTemp);
        end;
      SQLT_CLOB:
        begin
          FTempLob  := GetBlob(ColumnIndex); //localize
          Result    := FTempLob.GetPWideChar;
          Len       := FTempLob.Length;
        end;
      else
        raise Exception.Create('Unsupported OCI Type? '+ZFastCode.IntToStr(SQLVarHolder^.dty));
    end;
  end;
end;

{**
  Gets the final object of a type/named-collection/nested-table,array

  @param obj the parent-object
  @return the Object which contains the final object descriptor
}
function TZOracleAbstractResultSet_A.GetFinalObject(Obj: POCIObject): POCIObject;
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
function TZOracleAbstractResultSet_A.GetBoolean(ColumnIndex: Integer): Boolean;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := False
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: Result := StrToBoolEx(P, P+GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz), True, False);
      SQLT_VST: Result := StrToBoolEx(PAnsiChar(@PPOCILong(P)^.data[0]), PAnsiChar(@PPOCILong(P)^.data[0])+PPOCILong(P)^.Len, True, False);
      SQLT_VCS: Result := StrToBoolEx(PAnsiChar(@POCIVary(P).data[0]), PAnsiChar(@POCIVary(P).data[0])+POCIVary(P)^.Len, True, False);
      SQLT_LVC: Result := StrToBoolEx(PAnsiChar(@POCILong(P).data[0]), PAnsiChar(@POCILong(P).data[0])+POCILong(P)^.Len, True, False);
      { the oracle soft decimal }
      SQLT_VNU: Result := nvuKind(POCINumber(P), FvnuInfo) <> nvu0;
      { the ordinals we yet do support }
      SQLT_INT: case SQLVarHolder.value_sz of
                  SizeOf(Int64):    Result := PInt64(P)^ <> 0;
                  SizeOf(Integer):  Result := PInteger(P)^ <> 0;
                  SizeOf(SmallInt): Result := PSmallInt(P)^ <> 0;
                  else              Result := PShortInt(P)^ <> 0;
                end;
      SQLT_UIN: case SQLVarHolder.value_sz of
                  SizeOf(UInt64):   Result := PUInt64(P)^ <> 0;
                  SizeOf(Cardinal): Result := PCardinal(P)^ <> 0;
                  SizeOf(Word):     Result := PWord(P)^ <> 0;
                  else              Result := PByte(P)^ <> 0;
                end;
      { the FPU floats we do support }
      SQLT_FLT: if SQLVarHolder^.value_sz = SizeOf(Double)
                then Result := PDouble(P)^ <> 0
                else Result := PSingle(P)^ <> 0;
      SQLT_BFLOAT:  Result := PSingle(P)^ <> 0;
      SQLT_BDOUBLE: Result := PDouble(P)^ <> 0;
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := GetTimeStamp(ColumnIndex) <> 0;
      //SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
      SQLT_CLOB: Result := StrToBoolEx(GetBlob(ColumnIndex).GetPAnsiChar(FClientCP));
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
function TZOracleAbstractResultSet_A.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetLong(ColumnIndex: Integer): Int64;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Status: sword;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: Result := RawToInt64Def(P, P+GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz), 0);
      SQLT_VST: Result := RawToInt64Def(PAnsiChar(@PPOCILong(P)^.data[0]), PAnsiChar(@PPOCILong(P)^.data[0])+PPOCILong(P)^.Len, 0);
      SQLT_VCS: Result := RawToInt64Def(PAnsiChar(@POCIVary(P).data[0]), PAnsiChar(@POCIVary(P).data[0])+POCIVary(P)^.Len, 0);
      SQLT_LVC: Result := RawToInt64Def(PAnsiChar(@POCILong(P).data[0]), PAnsiChar(@POCILong(P).data[0])+POCILong(P)^.Len, 0);
      { the oracle soft decimal }
      SQLT_VNU: case nvuKind(POCINumber(P), FvnuInfo) of
                  nvu0,
                  nvuNegInf,
                  nvuPosInf:  Result := 0;
                  vnuNegInt:  Result := NegNvu2Int(POCINumber(P), FvnuInfo);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                  vnuPosInt:  Result := PosNvu2Int(POCINumber(P), FvnuInfo);
                  vnuPosCurr: Result := PosNvu2Int(POCINumber(P), FvnuInfo);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
                  vnuNegCurr: Result := NegNvu2Int(POCINumber(P), FvnuInfo);
                  else begin
                      Status := FplainDriver.OCINumberToReal(FErrorHandle, POCINumber(P),
                        SizeOf(Double), @FTinyBuffer[0]);
                      if Status <> OCI_SUCCESS then
                        CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther,
                              'OCINumberToReal', ConSettings);
                      Result := Trunc(PDouble(@FTinyBuffer[0])^);
                    end;
                end;
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
        case SQLVarHolder.value_sz of
          SizeOf(UInt64):   Result := Int64(PUInt64(P)^);
          SizeOf(Cardinal): Result := PCardinal(P)^;
          SizeOf(Word):     Result := PSmallInt(P)^;
          else              Result := PByte(P)^;
        end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := Trunc(PDouble(P)^)
                    else Result := Trunc(PSingle(P)^);
      SQLT_BFLOAT:  Result := Trunc(PSingle(P)^);
      SQLT_BDOUBLE: Result := Trunc(PDouble(P)^);
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetTimeStamp(ColumnIndex));
      SQLT_BLOB, SQLT_CLOB: begin
          FTempLob := GetBlob(ColumnIndex);
          Result := RawToInt64Def(FTempLob.GetBuffer, 0);
        end;
    else
      Result := 0;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZOracleAbstractResultSet_A.GetULong(ColumnIndex: Integer): UInt64;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Status: sword;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
{$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: Result := RawToUInt64Def(P, P+GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz), 0);
      SQLT_VST: Result := RawToUInt64Def(PAnsiChar(@PPOCILong(P)^.data[0]), PAnsiChar(@PPOCILong(P)^.data[0])+PPOCILong(P)^.Len, 0);
      SQLT_VCS: Result := RawToUInt64Def(PAnsiChar(@POCIVary(P).data[0]), PAnsiChar(@POCIVary(P).data[0])+POCIVary(P)^.Len, 0);
      SQLT_LVC: Result := RawToUInt64Def(PAnsiChar(@POCILong(P).data[0]), PAnsiChar(@POCILong(P).data[0])+POCILong(P)^.Len, 0);
      { the oracle soft decimal }
      SQLT_VNU: case nvuKind(POCINumber(P), FvnuInfo) of
                  nvu0,
                  nvuNegInf,
                  nvuPosInf:  Result := 0;
                  vnuNegInt:  Result := NegNvu2Int(POCINumber(P), FvnuInfo);
                  vnuPosInt:  Result := PosNvu2Int(POCINumber(P), FvnuInfo);
                  vnuPosCurr: Result := PosNvu2Int(POCINumber(P), FvnuInfo);
                  vnuNegCurr: Result := NegNvu2Int(POCINumber(P), FvnuInfo);
                  else begin
                      Status := FplainDriver.OCINumberToReal(FErrorHandle, POCINumber(P),
                        SizeOf(Double), @FTinyBuffer[0]);
                      if Status <> OCI_SUCCESS then
                        CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther,
                              'OCINumberToReal', ConSettings);
                      if PDouble(@FTinyBuffer[0])^ < 0
                      then Result := 0
                      else Result := Trunc(PDouble(@FTinyBuffer[0])^);
                    end;
                end;
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
        case SQLVarHolder.value_sz of
          SizeOf(UInt64):   Result := PUInt64(P)^;
          SizeOf(Cardinal): Result := PCardinal(P)^;
          SizeOf(Word):     Result := PWord(P)^;
          else              Result := PByte(P)^;
        end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := Trunc(PDouble(P)^)
                    else Result := Trunc(PSingle(P)^);
      SQLT_BFLOAT:  Result := Trunc(PSingle(P)^);
      SQLT_BDOUBLE: Result := Trunc(PDouble(P)^);
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetTimeStamp(ColumnIndex));
      SQLT_BLOB, SQLT_CLOB: begin
          FTempLob := GetBlob(ColumnIndex);
          Result := RawToUInt64Def(FTempLob.GetBuffer, 0);
        end;
    else
      Result := 0;
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := GetDouble(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetDouble(ColumnIndex: Integer): Double;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Status: sword;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder^.valuep+(FCurrentRowBufIndex*SQLVarHolder^.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: SqlStrToFloatDef(P,0,Result, GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz));
      SQLT_VST: SqlStrToFloatDef(PAnsiChar(@PPOCILong(P)^.data[0]),0,Result, PPOCILong(P)^.Len);
      SQLT_VCS: SqlStrToFloatDef(PAnsiChar(@POCIVary(P).data[0]), 0, Result, POCIVary(P)^.Len);
      SQLT_LVC: SqlStrToFloatDef(PAnsiChar(@POCILong(P).data[0]), 0, Result, POCILong(P)^.Len);
      { the oracle soft decimal }
      SQLT_VNU: begin
          Result := 0;
          Status:= FPlainDriver.OCINumberToReal(FErrorHandle, POCINumber(P), SizeOf(Double), @Result);
          if Status <> OCI_Success then
            CheckOracleError(FPLainDriver, FErrorHandle, Status, lcOther, 'OCINumberToReal', ConSettings);
        end;
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
          case SQLVarHolder.value_sz of
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
            SizeOf(UInt64):   Result := Int64(PUInt64(P)^);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
            SizeOf(Cardinal): Result := PCardinal(P)^;
            SizeOf(Word):     Result := PSmallInt(P)^;
            else              Result := PByte(P)^;
          end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := PDouble(P)^
                    else Result := PSingle(P)^;
      SQLT_BFLOAT:  Result := PSingle(P)^;
      SQLT_BDOUBLE: Result := PDouble(P)^;
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := GetTimeStamp(ColumnIndex);
      SQLT_BLOB, SQLT_CLOB:
        SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
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
{$IFDEF BCD_TEST}
procedure TZOracleAbstractResultSet_A.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
{$ELSE}
function TZOracleAbstractResultSet_A.GetBigDecimal(ColumnIndex: Integer): Extended;
{$ENDIF}
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Status: SWord;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: SqlStrToFloatDef(P,0,Result, GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz));
      SQLT_VST: SqlStrToFloatDef(PAnsiChar(@PPOCILong(P)^.data[0]),0,Result, PPOCILong(P)^.Len);
      SQLT_VCS: SqlStrToFloatDef(PAnsiChar(@POCIVary(P).data[0]), 0, Result, POCIVary(P)^.Len);
      SQLT_LVC: SqlStrToFloatDef(PAnsiChar(@POCILong(P).data[0]), 0, Result, POCILong(P)^.Len);
      { the oracle soft decimal }
      SQLT_VNU:
        {$IFDEF BCD_TEST}
        Result := Nvu2BCD(POCINumber(P), Result);
        {$ELSE}
        begin
          case nvuKind(POCINumber(P), FvnuInfo) of
            nvu0: Result := 0;
            {$R-} {$Q-}//fix a pre fpc 3.0 comiler bug
            nvuNegInf: Result := NegInfinity;
            nvuPosInf: Result := Infinity;
            {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
            {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
            vnuNegInt: Result := NegNvu2Int(POCINumber(P), FvnuInfo);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
            vnuPosInt: Result := PosNvu2Int(POCINumber(P), FvnuInfo);
            //vnuPosInt: Result := PosNvu2Int(FvnuInfo.Exponent, POCINumber(P));
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
            vnuPosCurr: Result := PosNvu2Curr(POCINumber(P), FvnuInfo);
            vnuNegCurr: Result := NegNvu2Curr(POCINumber(P), FvnuInfo);
            else begin
                Status := FplainDriver.OCINumberToReal(FErrorHandle, POCINumber(P),
                  SizeOf(Double), @FTinyBuffer[0]);
                if Status <> OCI_SUCCESS then
                  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther,
                        'OCINumberToReal', ConSettings);
                Result := PDouble(@FTinyBuffer[0])^;
              end;
          end;
        end;
        {$ENDIF}
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
          case SQLVarHolder.value_sz of
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
            SizeOf(UInt64):   Result := Int64(PUInt64(P)^);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
            SizeOf(Cardinal): Result := PCardinal(P)^;
            SizeOf(Word):     Result := PSmallInt(P)^;
            else              Result := PByte(P)^;
          end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := PDouble(P)^
                    else Result := PSingle(P)^;
      SQLT_BFLOAT:  Result := PSingle(P)^;
      SQLT_BDOUBLE: Result := PDouble(P)^;
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := GetTimeStamp(ColumnIndex);
      SQLT_BLOB, SQLT_CLOB:
        SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
      else
        Result := 0;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte array</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetBytes(ColumnIndex: Integer): TBytes;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := nil
  end else begin
    P := SQLVarHolder^.valuep+(FCurrentRowBufIndex*SQLVarHolder^.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported character/raw binary types we use }
      SQLT_VST: Result := BufferToBytes(@PPOCILong(P)^.data[0], PPOCILong(P)^.Len);
      SQLT_VCS, SQLT_VBI: Result := BufferToBytes(@POCIVary(P).data[0], POCIVary(P).Len);
      SQLT_LVC, SQLT_LVB: Result := BufferToBytes(@POCILong(P).data[0], POCILong(P).Len);
      { the supported large object types types we use }
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE, SQLT_CLOB:
        Result := GetBlob(ColumnIndex).GetBytes;
      else
        Result := BufferToBytes(P, SQLVarHolder.value_sz);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet_A.GetCurrency(ColumnIndex: Integer): Currency;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Status: sword;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: SqlStrToFloatDef(P,0,Result, GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz));
      SQLT_VST: SqlStrToFloatDef(PAnsiChar(@PPOCILong(P)^.data[0]),0,Result, PPOCILong(P)^.Len);
      SQLT_VCS: SqlStrToFloatDef(PAnsiChar(@POCIVary(P).data[0]), 0, Result, POCIVary(P)^.Len);
      SQLT_LVC: SqlStrToFloatDef(PAnsiChar(@POCILong(P).data[0]), 0, Result, POCILong(P)^.Len);
      { the oracle soft decimal }
      SQLT_VNU:
          case nvuKind(POCINumber(P), FvnuInfo) of
            nvu0, nvuNegInf, nvuPosInf: Result := 0;
            vnuNegInt: Result := NegNvu2Int(POCINumber(P), FvnuInfo);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
            vnuPosInt: Result := PosNvu2Int(POCINumber(P), FvnuInfo);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
            vnuPosCurr: Result := PosNvu2Curr(POCINumber(P), FvnuInfo);
            vnuNegCurr: Result := NegNvu2Curr(POCINumber(P), FvnuInfo);
            else begin
                Status := FplainDriver.OCINumberToReal(FErrorHandle, POCINumber(P),
                  SizeOf(Double), @FTinyBuffer[0]);
                if Status <> OCI_SUCCESS then
                  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther,
                        'OCINumberToReal', ConSettings);
                Result := PDouble(@FTinyBuffer[0])^;
              end;
        end;
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
          case SQLVarHolder.value_sz of
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
            SizeOf(UInt64):   Result := Int64(PUInt64(P)^);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
            SizeOf(Cardinal): Result := PCardinal(P)^;
            SizeOf(Word):     Result := PSmallInt(P)^;
            else              Result := PByte(P)^;
          end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := PDouble(P)^
                    else Result := PSingle(P)^;
      SQLT_BFLOAT:  Result := PSingle(P)^;
      SQLT_BDOUBLE: Result := PDouble(P)^;
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB:
      { the date/time types we support }
      SQLT_DAT, SQLT_TIMESTAMP:
        Result := GetTimeStamp(ColumnIndex);
      SQLT_BLOB, SQLT_CLOB:
        SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
    else
      Result := 0;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet_A.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := Int(GetTimeStamp(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet_A.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := Frac(GetTimeStamp(ColumnIndex));
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
function TZOracleAbstractResultSet_A.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
  Blob: IZBlob;
  Status: sword;
  Year: SmallInt;
  yr, mnth, dy, hr, mm, ss, fsec: sb4;
  Month, Day: Byte;
  Hour, Minute, Second: Byte;
  Millis: ub4;
  Ptr: POraDate absolute P;
label ConvFromString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := 0
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: begin { fixed char right ' ' padded }
                  Len := GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz);
                  goto ConvFromString;
                end;
      SQLT_VST: begin
                  Len := PPOCILong(P)^.Len;
                  P := @PPOCILong(P)^.data[0];
                  goto ConvFromString;
                end;
      SQLT_VCS: begin
                  Len := POCIVary(P).Len;
                  P := PAnsiChar(@POCIVary(P).data[0]);
                  goto ConvFromString;
                end;
      SQLT_LVC: begin
                  Len := POCILong(P).Len;
                  P := PAnsiChar(@POCILong(P).data[0]);
ConvFromString:   if (P+2)^ = ':' then //possible date if Len = 10 then
                    Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
                  else if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
                    Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
                  else
                    Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
                  LastWasNull := (Result = 0) or Failed;
                end;
      { the ordinals we yet do support }
      SQLT_INT:
        case SQLVarHolder.value_sz of
          SizeOf(Int64):    Result := PInt64(P)^;
          SizeOf(Integer):  Result := PInteger(P)^;
          SizeOf(SmallInt): Result := PSmallInt(P)^;
          else              Result := PShortInt(P)^;
        end;
      SQLT_UIN:
          case SQLVarHolder.value_sz of
            SizeOf(UInt64):   Result := Int64(PUInt64(P)^);
            SizeOf(Cardinal): Result := PCardinal(P)^;
            SizeOf(Word):     Result := PSmallInt(P)^;
            else              Result := PByte(P)^;
          end;
      { the FPU floats we do support }
      SQLT_FLT:     if SQLVarHolder.value_sz = SizeOf(Double)
                    then Result := PDouble(P)^
                    else Result := PSingle(P)^;
      SQLT_BFLOAT:  Result := PSingle(P)^;
      SQLT_BDOUBLE: Result := PDouble(P)^;
      { the binary raw we support }
      //SQLT_VBI, SQLT_LVB: Result := 0;
      { the date/time types we support }
      SQLT_DAT: begin
            if Ptr^.Cent <= 100  // avoid TDateTime values < 0 (generates wrong DecodeTime) //thanks to ab of synopse
            then result := 0
            else result := EncodeDate((Ptr^.Cent-100)*100+Ptr^.Year-100,Ptr^.Month,Ptr^.Day);
            if (Ptr^.Hour<>0) or (Ptr^.Min<>0) or (Ptr^.Sec<>0) then
              result := result + EncodeTime(Ptr^.Hour-1,Ptr^.Min-1,Ptr^.Sec-1,0);
          end;
      SQLT_INTERVAL_DS:
        begin
          Status := FPlainDriver.OCIIntervalGetDaySecond(FOCISvcCtx, FErrorHandle,
            @dy, @hr, @mm, @ss, @fsec, PPOCIDescriptor(P)^);
          if (Status = OCI_SUCCESS)
          then Result := EncodeTime(hr, mm, ss, fsec div 100000)
          else Result := 0;
        end;
      SQLT_INTERVAL_YM:
        begin
          Status := FPlainDriver.OCIIntervalGetYearMonth(FOCISvcCtx, FErrorHandle, @yr, @mnth, PPOCIDescriptor(P)^);
          if (Status = OCI_SUCCESS) and (not (yr and mnth = 1))
          then Result := EncodeDate(yr, mnth, 1)
          else Result := 0;
        end;
      SQLT_TIMESTAMP_TZ,
      SQLT_TIMESTAMP_LTZ,
      SQLT_TIMESTAMP: begin
          Status := FPlainDriver.OCIDateTimeGetDate(FConnectionHandle, FErrorHandle,
            PPOCIDescriptor(P)^, Year{%H-}, Month{%H-}, Day{%H-});
        // attention : this code handles all timestamps on 01/01/0001 as a pure time value
        // reason : oracle doesn't have a pure time datatype so all time comparisons compare
        //          TDateTime values on 30 Dec 1899 against oracle timestamps on 01 januari 0001 (negative TDateTime)
          if (Status = OCI_SUCCESS) and (not ((Year=1) and (Month=1) and (Day=1)))
          then Result := EncodeDate(Year, Month, Day)
          else Result := 0;
          Status := FPlainDriver.OCIDateTimeGetTime(FConnectionHandle, FErrorHandle,
            PPOCIDescriptor(P)^, Hour{%H-}, Minute{%H-}, Second{%H-}, Millis{%H-});
          if Status = OCI_SUCCESS then
            Result := Result + EncodeTime(Hour, Minute, Second, Millis div 1000000);
        end;
      SQLT_BLOB,
      SQLT_CLOB: begin
          Blob := Getblob(ColumnIndex);
          P := Blob.GetBuffer;
          Len := Blob.Length;
          goto ConvFromString;
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
function TZOracleAbstractResultSet_A.GetDataSet(ColumnIndex: Integer): IZDataSet;
var
  SQLVarHolder: PZSQLVar;
  type_Ref: POCIRef;
  //tdo: POCIType;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := nil
  end else begin
    LastWasNull := False;
    Result := nil;
    if SQLVarHolder.dty = SQLT_NTY then
      {$R-}
      if SQLVarHolder.indp[FCurrentRowBufIndex] >= 0 then begin
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        if SQLVarHolder._Obj.is_final_type = 1 then
          // here we've the final object lets's read it to test it
          // later we only need the reference-pointers to create a new dataset
        else
        begin
           //http://cpansearch.perl.org/src/TIMB/DBD-Oracle-1.26/oci8.c

          //create a temporary object
          type_ref := nil;
          CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.OCIObjectNew(FConnectionHandle,
              FConnection.GetErrorHandle, FOCISvcCtx, OCI_TYPECODE_REF,
                nil, nil, OCI_DURATION_DEFAULT, TRUE, @type_ref),
            lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
          //Get the type reference
          CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.OCIObjectGetTypeRef(FConnectionHandle,
              FConnection.GetErrorHandle, SQLVarHolder._Obj.obj_value, type_Ref),
            lcOther, 'OCIObjectGetTypeRef(obj_value)', ConSettings);

          //Now let's get the new tdo
          //Excptions????????
          {CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.TypeByRef(FConnectionHandle,
              FConnection.GetErrorHandle, type_ref, OCI_DURATION_DEFAULT,
              OCI_TYPEGET_ALL, @tdo),
            lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);}
          //free the temporary object
          CheckOracleError(FPlainDriver, FErrorHandle,
            FPlainDriver.OCIObjectFree(FConnectionHandle,
              FConnection.GetErrorHandle, type_ref, ub2(0)),
            lcOther, 'ObjectFree()', ConSettings);
        end;


        {CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ResultSetToStmt(SQLVarHolder^._Obj.obj_ind,
            FErrorHandle), lcOther, 'Nested Table to Stmt handle', ConSettings);
        Result := CreateOracleResultSet(FPlainDriver, GetStatement,
          'Fetch Nested Table', SQLVarHolder^._Obj.obj_ref, FErrorHandle)};
      end;
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
function TZOracleAbstractResultSet_A.GetBlob(ColumnIndex: Integer): IZBlob;
var
  SQLVarHolder: PZSQLVar;
  Len: NativeUInt;
  P: PAnsiChar;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  {$R-}
  SQLVarHolder := @FColumns.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if (SQLVarHolder.valuep = nil) or (SQLVarHolder.indp[FCurrentRowBufIndex] < 0) then begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    LastWasNull := True;
    Result := nil
  end else begin
    P := SQLVarHolder.valuep+(FCurrentRowBufIndex*SQLVarHolder.value_sz);
    LastWasNull := False;
    case SQLVarHolder.dty of
      { the supported String types we use }
      SQLT_AFC: Result := TZAbstractClob.CreateWithData(P, GetAbsorbedTrailingSpacesLen(P, SQLVarHolder.Value_sz), FClientCP, ConSettings);
      SQLT_VST: Result := TZAbstractClob.CreateWithData(PAnsiChar(@PPOCILong(P)^.data[0]),
        PPOCILong(P)^.Len, FClientCP, ConSettings);
      SQLT_VCS: Result := TZAbstractClob.CreateWithData(PAnsiChar(@POCIVary(P).data[0]),
        POCIVary(P)^.Len, FClientCP, ConSettings);
      SQLT_LVC: Result := TZAbstractClob.CreateWithData(PAnsiChar(@POCILong(P).data[0]),
        POCILong(P)^.Len, FClientCP, ConSettings);
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE: begin
          Result := TZOracleBlob.Create(FPlainDriver, nil, 0, FOCISvcCtx,
            FConnection.GetErrorHandle, PPOCIDescriptor(P)^, FChunkSize, ConSettings);
          (Result as IZOracleBlob).ReadLob; //nasty: we've got only one descriptor if we fetch the rows. Loading on demand isn't possible
        end;
      SQLT_CLOB: begin
          Result := TZOracleClob.Create(FPlainDriver, nil, 0,
            FConnectionHandle, FOCISvcCtx,
            FConnection.GetErrorHandle, PPOCIDescriptor(P)^, FChunkSize, ConSettings,
            FClientCP);
          (Result as IZOracleBlob).ReadLob; //nasty: we've got only one descriptor if we fetch the rows. Loading on demand isn't possible
        end;
      SQLT_NTY: Result := TZAbstractBlob.CreateWithStream(nil);
      { the binary raw we support }
      SQLT_VBI: Result := TZAbstractBlob.CreateWithData(PAnsiChar(@POCIVary(P)^.data[0]), POCIVary(P)^.Len);
      SQLT_LVB: Result := TZAbstractBlob.CreateWithData(PAnsiChar(@POCILong(P)^.data[0]), POCILong(P)^.Len);
      else begin
          P := GetPAnsiChar(ColumnIndex, Len);
          Result := TZAbstractClob.CreateWithData(P, Len, FClientCP, ConSettings);
        end;
    end;
  end;
end;

{ TZOracleResultSet_A }

{**
  Opens this recordset.
}
procedure TZOracleResultSet_A.Open;
var
//  char_semantics: ub1;
  I, J: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
  ColumnCount: ub4;
  TempColumnNameLen, CSForm: Integer;
  P: PAnsiChar;
  DescriptorColumnCount,SubObjectColumnCount: Integer;
  //CanBindInt64: Boolean;
  paramdpp: Pointer;
  RowSize: Integer;
  defn_or_bindpp:     POCIHandle;
  function AttributeToString(var P: PAnsiChar; Len: Integer): string;
  begin
    if P <> nil then
      {$IFDEF UNICODE}
      Result := ZEncoding.PRawToUnicode(P, Len, FClientCP)
      {$ELSE}
      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(FClientCP, ConSettings^.CTRL_CP) then
        Result := BufferToStr(P, Len)
      else
        Result := ZUnicodeToString(PRawToUnicode(P, Len, FClientCP), ConSettings^.CTRL_CP)
      {$ENDIF}
    else
      Result := '';
    P := nil;
  end;
begin
  //CanBindInt64 := FConnection.GetClientVersion >= 11002000;
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FStmtHandle) or not Assigned(FErrorHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.OCIStmtExecute(FOCISvcCtx, FStmtHandle, FErrorHandle, 1, 0,
      nil, nil, OCI_DESCRIBE_ONLY),
      lcExecute, 'OCIStmtExecute', ConSettings);

  { Resize SQLVARS structure if needed }
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.OCIAttrGet(FStmtHandle, OCI_HTYPE_STMT, @ColumnCount, nil,
      OCI_ATTR_PARAM_COUNT, FErrorHandle),
      lcExecute, 'OCIStmtExecute', ConSettings);

  AllocateOracleSQLVars(FColumns, ColumnCount);
  DescriptorColumnCount := 0; SubObjectColumnCount := 0;
  ColumnsInfo.Clear;
  ColumnsInfo.Capacity := ColumnCount; //alloc space once
  RowSize := 0;
  { collect informations for result set columns }
  for I := 1 to ColumnCount do begin
    {$R-}
    CurrentVar := @FColumns.Variables[I-1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}

    ColumnInfo := TZColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);

    paramdpp := nil; //init
    FPlainDriver.OCIParamGet(FStmtHandle, OCI_HTYPE_STMT, FErrorHandle, paramdpp, I);
    (*CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @char_semantics, nil, OCI_ATTR_CHAR_USED, FErrorHandle),
      lcExecute, 'OCI_ATTR_CHAR_USED', ConSettings);
    if Boolean(char_semantics) then
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @CurrentVar^.value_sz, nil, OCI_ATTR_MAXCHAR_SIZE, FErrorHandle)
    else*)
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @CurrentVar^.value_sz, nil, OCI_ATTR_DATA_SIZE, FErrorHandle);
    CurrentVar^.value_sz := PUB2(@CurrentVar^.value_sz)^; //full init of all 4 Bytes -> is a ub2
    FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
      @CurrentVar^.dty, nil, OCI_ATTR_DATA_TYPE, FErrorHandle);
    if CurrentVar^.dty in [SQLT_NUM, SQLT_VNU] then begin //unsigned char[21](binary) see: http://docs.oracle.com/cd/B19306_01/appdev.102/b14250/oci03typ.htm
      {11g bug: returns Precision 38 for Ordinal values }
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @CurrentVar^.Precision, nil, OCI_ATTR_PRECISION, FErrorHandle);
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @CurrentVar^.Scale, nil, OCI_ATTR_SCALE, FErrorHandle);
      ColumnInfo.Precision := CurrentVar.Precision;
      if CurrentVar.Scale > 0 then
        ColumnInfo.Scale := CurrentVar.Scale;
    end else begin
      CurrentVar^.Scale := 0;
      CurrentVar^.Precision := 0;
    end;
    P := nil; //init
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
      @P, @TempColumnNameLen, OCI_ATTR_NAME, FErrorHandle),
      lcExecute, 'OCI_ATTR_NAME', ConSettings);
    ColumnInfo.ColumnLabel := AttributeToString(P, TempColumnNameLen);

    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
      @P, @TempColumnNameLen, OCI_ATTR_SCHEMA_NAME, FErrorHandle),
      lcExecute, 'OCI_ATTR_SCHEMA_NAME', ConSettings);
    ColumnInfo.SchemaName := AttributeToString(P, TempColumnNameLen);
    ColumnInfo.CharOctedLength := CurrentVar^.value_sz;

    CurrentVar^.ColType := NormalizeOracleTypeToSQLType(CurrentVar.dty,
      CurrentVar.value_sz, CurrentVar^.DescriptorType,
      CurrentVar.Precision, CurrentVar.Scale, ConSettings, OCI_TYPEPARAM_IN);
    inc(DescriptorColumnCount, Ord(CurrentVar^.DescriptorType > 0));
    ColumnInfo.Signed := True;
    ColumnInfo.Nullable := ntNullable;

    ColumnInfo.ColumnType := CurrentVar^.ColType;
    ColumnInfo.Scale := CurrentVar^.Scale;
    if (ColumnInfo.ColumnType in [stString, stUnicodeString]) then begin
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @ColumnInfo.Precision, nil, OCI_ATTR_DISP_SIZE, FErrorHandle);
      FPlainDriver.OCIAttrGet(paramdpp, OCI_DTYPE_PARAM,
        @CSForm, nil, OCI_ATTR_CHARSET_FORM, FErrorHandle);
      if CSForm = SQLCS_NCHAR then //We should determine the NCHAR set on connect
        ColumnInfo.ColumnDisplaySize := ColumnInfo.ColumnDisplaySize shr 1; //shr 1 = div 2 but faster
      //ColumnInfo.Precision := ColumnInfo.ColumnDisplaySize;
      ColumnInfo.CharOctedLength := CurrentVar^.value_sz;
      if ColumnInfo.ColumnType = stString then begin
        ColumnInfo.CharOctedLength := ColumnInfo.Precision * ConSettings^.ClientCodePage^.CharWidth;
      end else begin
        ColumnInfo.CharOctedLength := ColumnInfo.Precision shl 1;
      end;
    end else if (ColumnInfo.ColumnType = stBytes ) then
      ColumnInfo.Precision := CurrentVar^.value_sz
    else
      ColumnInfo.Precision := CurrentVar^.Precision;
    if CurrentVar.dty = SQLT_NTY  then begin
      Inc(SubObjectColumnCount);
      CurrentVar^.value_sz := SizeOf(PPOCIDescriptor);
      CurrentVar^.ColType := stDataSet;

      CurrentVar^._Obj := DescribeObject(FplainDriver, FConnection,
        paramdpp, FStmtHandle, 0);
      if CurrentVar^._Obj.col_typecode = OCI_TYPECODE_TABLE then
        CurrentVar^.ColType := stDataSet
      else if CurrentVar^._Obj.col_typecode = OCI_TYPECODE_VARRAY then
        CurrentVar^.ColType := stArray
      else //more possible types
        CurrentVar^.ColType := stBinaryStream;
    end;
    if CurrentVar^.ColType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]
    then ColumnInfo.ColumnCodePage := FClientCP
    else ColumnInfo.ColumnCodePage := High(Word);
    {calc required size of field}

    if CurrentVar^.value_sz > 0 then
      if (CurrentVar^.dty = SQLT_VST) or (CurrentVar^.DescriptorType > 0)
      then Inc(RowSize, SizeOf(Pointer)+SizeOf(sb2){NullIndicator})
      else Inc(RowSize, Integer(CurrentVar^.value_sz+SizeOf(sb2)){NullIndicator});
  end;
  {in case all cols are null we need min 1 defined col-variable to exec the stmt }
  if (RowSize = 0 ) then begin
    FIteration := 1;
    FColumns.Variables[0].value_sz := 8;
    RowSize := 8 +SizeOf(sb2);
  end else if (RowSize > FZBufferSize) { now let's calc the iters we can use }
    then FIteration := 1
    else FIteration := FZBufferSize div RowSize;
  if ( DescriptorColumnCount > 0 ) and (DescriptorColumnCount * FIteration > 1000) then //take care we do not create too much descriptors
    FIteration := 1000 div DescriptorColumnCount;
  if (SubObjectColumnCount > 0) then
    FIteration := 1; //EH: current code isn't prepared -> Bugfix required

  SetLength(FRowsBuffer, RowSize * FIteration); //Alloc mem we need for multiple rows
  {give our Vars it's addressation in RowsBuffer}
  P := Pointer(FRowsBuffer);
  { Bind handle and Fills the column info. }
  for I := 1 to FColumns.AllocNum do begin
    {$R-}
    CurrentVar := @FColumns.Variables[I-1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if (CurrentVar^.value_sz = 0) then
      continue;
    CurrentVar.indp := Pointer(P);
    Inc(P, SizeOf(sb2)*FIteration);
    CurrentVar.valuep := P;
    if CurrentVar^.ColType = stUnknown then
      continue;
    if CurrentVar^.DescriptorType <> NO_DTYPE then
      for J := 0 to FIteration -1 do begin
        FStatus := FPlainDriver.OCIDescriptorAlloc(FConnectionHandle, PPOCIDescriptor(P)^, CurrentVar^.DescriptorType, 0, nil);
        if FStatus <> OCI_SUCCESS then
          CheckOracleError(FPlainDriver, FErrorHandle, Fstatus, lcOther, 'OCIDescriptorAlloc', ConSettings);
        Inc(P, SizeOf(PPOCIDescriptor));
      end
    else if CurrentVar^.dty = SQLT_VST then
      for J := 0 to FIteration -1 do begin
        FStatus := FPlainDriver.OCIStringResize(FConnectionHandle, FErrorHandle, CurrentVar^.value_sz, PPOCIString(P));
        if FStatus <> OCI_SUCCESS then
          CheckOracleError(FPlainDriver, FErrorHandle, Fstatus, lcOther, 'OCIStringResize', ConSettings);
        Inc(P, SizeOf(PPOCIString));
      end
    else
      Inc(P, CurrentVar^.value_sz*Cardinal(FIteration));
    defn_or_bindpp := nil;
    FStatus := FPlainDriver.OCIDefineByPos(FStmtHandle, defn_or_bindpp,
      FErrorHandle, I, CurrentVar^.valuep, CurrentVar^.value_sz, CurrentVar^.dty,
      CurrentVar^.indp, CurrentVar^.alenp, nil, OCI_DEFAULT);
    if FStatus <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, FStatus, lcExecute, 'OCIDefineByPos', ConSettings);
    if CurrentVar^.dty=SQLT_NTY then
      //second step: http://www.csee.umbc.edu/portal/help/oracle8/server.815/a67846/obj_bind.htm
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.OCIDefineObject(defn_or_bindpp, FErrorHandle, CurrentVar^._Obj.tdo,
           @CurrentVar^._Obj.obj_value, nil, nil, nil),
        lcExecute, 'OCIDefineObject', ConSettings);
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
procedure TZOracleResultSet_A.BeforeClose;
begin
  inherited BeforeClose;
  SetLength(Self.FRowsBuffer, 0);
  { prepared statement own handles, so dont free them }
  FStmtHandle := nil;
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
function TZOracleResultSet_A.Next: Boolean;
var
  Status: Integer;
  FetchedRows: LongWord;
label Success;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (FStmtHandle = nil) then
    Exit;

  if RowNo = 0 then begin//fetch Iteration count of rows
    Status := FPlainDriver.OCIStmtExecute(FOCISvcCtx, FStmtHandle,
      FErrorHandle, FIteration, 0, nil, nil, OCI_DEFAULT);
    if Status = OCI_SUCCESS then begin
      FMaxBufIndex := FIteration -1; //FFetchedRows is an index [0...?] / FIteration is Count 1...?
      goto success; //skip next if's
    end;
  end else if Integer(FCurrentRowBufIndex) < FMaxBufIndex then begin
    Inc(FCurrentRowBufIndex);
    goto Success; //skip next if's
  end else if FMaxBufIndex+1 < FIteration then begin
    RowNo := RowNo + 1;
    Exit;
  end else begin //fetch Iteration count of rows
    Status := FPlainDriver.OCIStmtFetch2(FStmtHandle, FErrorHandle,
      FIteration, OCI_FETCH_NEXT, 0, OCI_DEFAULT);
    FCurrentRowBufIndex := 0; //reset
    if Status = OCI_SUCCESS then begin
      FMaxBufIndex := FIteration -1;
      goto success;
    end;
  end;

  if Status = OCI_NO_DATA then begin
    FPlainDriver.OCIAttrGet(FStmtHandle,OCI_HTYPE_STMT,@FetchedRows,nil,OCI_ATTR_ROWS_FETCHED,FErrorHandle);
    LastRowNo := RowNo+Integer(FetchedRows);  //this makes Exit out in first check on next fetch
    FMaxBufIndex := Integer(FetchedRows)-1;
    RowNo := RowNo + 1;
    //did we retrieve a row or is table empty?
    if FetchedRows > 0 then
      Result := True;
    Exit;
  end;

  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'FETCH ROW', ConSettings);

  if Status in [OCI_SUCCESS, OCI_SUCCESS_WITH_INFO] then begin
Success:
    RowNo := RowNo + 1;
    if FMaxBufIndex+1 = FIteration then
      LastRowNo := RowNo;
    Result := True;
  end;
end;

{ TZOracleCallableResultSet_A }

constructor TZOracleCallableResultSet_A.Create(const Statement: IZStatement;
  const SQL: string; StmtHandle: POCIStmt; ErrorHandle: POCIError;
  OraVariables: PZOCIParamBinds; {$IFDEF AUTOREFCOUNT}const{$ENDIF} BindList: TZBindList);
var I, N: Integer;
  BindValue: PZBindValue;
  ParamValue: PZOCIParamBind;
  CurrentVar: PZSQLVar;
begin
  N := 0;
  for I := 0 to BindList.Count -1 do
    if Ord(BindList[i].ParamType) > ord(pctIn) then
      Inc(N);
  AllocateOracleSQLVars(FColumns, N);
  SetLength(FFieldNames, N);

  N := 0;
  for I := 0 to BindList.Count -1 do begin
    {$R-}
    BindValue := BindList[i];
    if Ord(BindValue.ParamType) <= ord(pctIn) then
      Continue;
    ParamValue := @OraVariables[i];
    CurrentVar := @FColumns.Variables[N];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.valuep := ParamValue.valuep;
    CurrentVar.dty := ParamValue.dty;
    CurrentVar.value_sz := ParamValue.value_sz;
    CurrentVar.indp := ParamValue.indp;
    CurrentVar.DescriptorType := ParamValue.DescriptorType;
    CurrentVar.ColType := BindValue.SQLType;
    FFieldNames[N] := ParamValue.ParamName;
    Inc(N);
  end;
  inherited Create(Statement, SQL, StmtHandle, ErrorHandle, 0);
  LastRowNo := 1;
end;

function TZOracleCallableResultSet_A.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo = 1) then
    Exit;
  RowNo := 1;
  Result := True;
end;

procedure TZOracleCallableResultSet_A.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
begin
  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FColumns.AllocNum -1 do
  begin
    {$R-}
    CurrentVar := @FColumns.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';

      ColumnLabel := FFieldNames[i];
      ColumnDisplaySize := 0;
      AutoIncrement := False;
      Signed := True;
      Nullable := ntNullable;

      ColumnType := CurrentVar^.ColType;
      Scale := CurrentVar^.Scale;

      {Reset the column type which can be changed by user before}
      if CurrentVar^.ColType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnInfo.ColumnCodePage := FClientCP;
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
        ColumnDisplaySize := CurrentVar.value_sz;
        Precision := CurrentVar.value_sz;
      end
      else
        Precision := CurrentVar.Precision;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
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
constructor TZOracleBlob.Create(const PlainDriver: TZOraclePlainDriver;
  const Data: Pointer; const Size: Int64; const OCISvcCtx: POCISvcCtx;
  const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
  const ChunkSize: Integer; const ConSettings: PZConSettings);
begin
  inherited CreateWithData(Data, Size);
  FOCISvcCtx := OCISvcCtx;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FNativePlainDriver := PlainDriver;
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
    FPlainDriver.OCILobFreeTemporary(FOCISvcCtx, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleBlob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.OCILobCreateTemporary(FOCISvcCtx, FErrorHandle,
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
    Status := FPlainDriver.OCILobOpen(FOCISvcCtx, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    if Status <> OCI_SUCCESS then
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

          Status := FPlainDriver.OCILobRead(FOCISvcCtx, FErrorHandle,
            FLobLocator, ReadNumBytes, Offset + 1, {$R-}@Buf[Offset]{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}, ReadNumBytes,
            nil, nil, 0, SQLCS_IMPLICIT);
          if Status <> OCI_SUCCESS then
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
      Status := FPlainDriver.OCILobClose(FOCISvcCtx,FErrorHandle, FLobLocator);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
    end;
    { Assigns data }
    InternalSetData(Buf, Offset);
  end;
  //inherited ReadLob;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZOracleBlob.WriteLob;
begin
  OraWriteLob(FPlainDriver, BlobData, FOCISvcCtx, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, True, nil);
end;

procedure TZOracleBlob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  OraWriteLob(FPlainDriver, Buffer, FOCISvcCtx, FErrorHandle, FLobLocator,
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

{ TZOracleClob }

constructor TZOracleClob.Create(const PlainDriver: TZOraclePlainDriver;
  const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
  const OCISvcCtx: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  const ConSettings: PZConSettings; const CodePage: Word);
begin
  if ZCompatibleCodePages(CodePage, zCP_UTF16) then
    inherited CreateWithData(Data, Size shr 1, ConSettings) //shr 1 = div 2 but faster
  else
    inherited CreateWithData(Data, Size, CodePage, ConSettings);
  FOCISvcCtx := OCISvcCtx;
  FConnectionHandle := ConnectionHandle;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FNativePlainDriver := PlainDriver;
  FTemporary := False;
  FChunkSize := ChunkSize;
end;

destructor TZOracleClob.Destroy;
begin
  if FTemporary then
    FPlainDriver.OCILobFreeTemporary(FOCISvcCtx, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleClob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.OCILobCreateTemporary(FOCISvcCtx, FErrorHandle, FLobLocator,
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
    Status := FPlainDriver.OCILobRead(FOCISvcCtx,FErrorHandle, FLobLocator,
      ReadNumChars, Offset + 1, Buf, FChunkSize, nil, nil, csid, csfrm);
    if ReadNumChars > 0 then
    begin
      Inc(Offset, ReadNumChars);
      ReallocMem(FBlobData, Offset+1);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, (PAnsiChar(FBlobData)+NativeUInt(OffSet-ReadNumChars))^, ReadNumChars);
    end;
  end;
begin
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  if not Updated and (FLobLocator <> nil) and (BlobData = nil) and (not FTemporary) then
  begin
    { Opens a large object or file for read. }
    Status := FPlainDriver.OCILobOpen(FOCISvcCtx, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Open Large Object', FConSettings);
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        GetMem(Buf, FChunkSize+1);
        Offset := 0;
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.OCILobCharSetForm(FConnectionHandle, FErrorHandle,
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
      Status := FPlainDriver.OCILobClose(FOCISvcCtx, FErrorHandle, FLobLocator);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
      if Buf <> nil then
        FreeMem(Buf);
    end;
  end;
  //inherited ReadLob;
end;

procedure TZOracleClob.WriteLob;
begin
  GetPAnsiChar(FConSettings^.ClientCodePage^.CP); //convert if required
  OraWriteLob(FPlainDriver, BlobData, FOCISvcCtx, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, False, FConSettings);
end;

procedure TZOracleClob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  if Buffer = nil then
    OraWriteLob(FPlainDriver, Buffer, FOCISvcCtx, FErrorHandle, FLobLocator,
      FChunkSize, Len, False, FConSettings)
  else
    {%H-}OraWriteLob(FPlainDriver, Buffer, FOCISvcCtx, FErrorHandle, FLobLocator,
      FChunkSize, Int64(Len)+1, False, FConSettings);
end;

end.
