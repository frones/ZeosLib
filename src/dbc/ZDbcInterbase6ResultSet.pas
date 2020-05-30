{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6ResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6,
  ZPlainFirebirdInterbaseDriver, ZCompatibility, ZDbcResultSetMetadata, ZMessages,
  ZPlainDriver, ZDbcInterbase6Utils, ZDbcUtils, ZClasses,
  ZDbcCache, ZDbcCachedResultSet, ZDbcFirebirdInterbase;

type
  IZInterbaseResultSet = Interface(IZResultSet)
    ['{1CFF9886-0B1A-47E1-BD52-2D58ABC2B3CF}']
    function GetConnection: IZInterbase6Connection;
  End;

  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractInterbaseFirebirdResultSet,
    IZResultSet, IZInterbaseResultSet)
  private
    FStmtHandle: TISC_STMT_HANDLE;
    FStmtHandleAddr: PISC_STMT_HANDLE;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FPISC_DB_HANDLE: PISC_DB_HANDLE;
    FPlainDriver: TZInterbasePlainDriver;
    FDialect: Word;
    FStmtType: TZIbSqlStatementType;
    FISC_TR_HANDLE: TISC_TR_HANDLE;
    FIBConnection: IZInterbase6Connection;
    FIBTransaction: IZIBTransaction;
    procedure RegisterCursor;
    procedure DeRegisterCursor;
  public //implement IZInterbaseResultSet
    function GetConnection: IZInterbase6Connection;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StmtHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      StmtType: TZIbSqlStatementType);

    procedure AfterClose; override;
    procedure ResetCursor; override;

    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    function Next: Boolean; reintroduce;
  end;

  TZInterbase6Lob = class;

  {** EH: implements a sequential Firebird/Interbase large object stream }
  TZInterbaseLobStream = class(TZAbstractInterbaseFirebirdLobStream)
  private
    FDB_HANDLE: PISC_DB_HANDLE;
    FTransactionHandle: PISC_TR_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FBlobHandle: TISC_BLOB_HANDLE;
    FOwnerLob: TZInterbase6Lob;
  protected
    procedure FillBlobInfo;
    function GetSize: Int64; override;
  public
    constructor Create(const OwnerLob: TZInterbase6Lob);
    destructor Destroy; override;
  public
    procedure OpenLob;
    procedure CloseLob;
    procedure CreateLob;
    procedure CancelLob;
  public //TStream overrides
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
  end;

  { TZInterbase6Lob }

  TZInterbase6Lob = Class(TZAbstractStreamedLob, IZLob, IZBlob,
    IImmediatelyReleasable, IZInterbaseFirebirdLob)
  private
    FLobStream: TZInterbaseLobStream;
    FPlainDriver: TZInterbasePlainDriver;
    FBlobId: TISC_QUAD;
    FIBConnection: IZInterbase6Connection;
    FIBTransaction: IZIBTransaction;
    FReleased: Boolean;
    FBlobInfo: TIbBlobInfo;
    FBlobInfoFilled: Boolean;
    FIsTemporary: Boolean;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    function GetConSettings: PZConSettings;
  public
    function GetBlobId: TISC_QUAD;
  public
    function Clone(LobStreamMode: TZLobStreamMode): IZBlob;
    function IsEmpty: Boolean; override;
    procedure Clear; override;
  public //obsolate
    function Length: Integer; override;
  public
    constructor Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  End;

  TZInterbase6Clob = Class(TZInterbase6Lob, IZCLob)
  public
    constructor Create(const Connection: IZInterbase6Connection;
      BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
      ColumnCodePage: Word; const OpenLobStreams: TZSortedList);
  End;

  TZInterbase6Blob = Class(TZInterbase6Lob)
  public
    constructor Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
  End;

  {**
    Implements Firebird cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZInterbaseCachedResultSet = Class(TZCachedResultset)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob{IZLob}; override;
  End;

  TZInterbaseRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils, ZDbcMetadata, ZDbcLogging, ZVariant,
  ZDbcProperties;

{ TZInterbase6XSQLDAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase statement type
}
constructor TZInterbase6XSQLDAResultSet.Create(const Statement: IZStatement;
  const SQL: string; StmtHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
  StmtType: TZIbSqlStatementType);
var
  I: Word;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZInterbaseFirebirdColumnInfo;
  ZCodePageInfo: PZCodePage;
  CPID: Word;
  XSQLVAR: PXSQLVAR;
label jmpLen;
begin
  inherited Create(Statement, SQL);
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  FPISC_DB_HANDLE := FIBConnection.GetDBHandle;
  FISC_TR_HANDLE := FIBConnection.GetTrHandle^;
  FPlainDriver := FIBConnection.GetPlainDriver;
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc

  FStmtHandleAddr := StmtHandleAddr;
  FStmtHandle := StmtHandleAddr^;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  if (FStmtHandle=0) or (FXSQLDA.sqld = 0) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := FXSQLDA.sqld;
  if FXSQLDA.sqld > 0 then begin //keep track we have a column to avoid range issues see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=10595
    ColumnsInfo.Capacity := FXSQLDA.sqld;
    for I := 0 to FXSQLDA.sqld - 1 do begin
      {$R-}
      XSQLVAR := @FXSQLDA.sqlvar[i];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      ColumnInfo := TZInterbaseFirebirdColumnInfo.Create;
      with ColumnInfo do begin
        TableName := FIZSQLDA.GetFieldRelationName(I);
        if TableName <> '' then
          ColumnName := FIZSQLDA.GetFieldSqlName(I);
        ColumnLabel := FIZSQLDA.GetFieldAliasName(I);
        FieldSqlType := FIZSQLDA.GetFieldSqlType(I);
        sqltype := XSQLVAR.sqltype and not (1);
        sqlsubtype := XSQLVAR.sqlsubtype;
        sqlscale := XSQLVAR.sqlscale;
        sqldata := XSQLVAR.sqldata;
        sqlind := XSQLVAR.sqlind;
        if FGUIDProps.ColumnIsGUID(FieldSqlType, XSQLVAR.sqllen, ColumnName) then
          FieldSqlType := stGUID;
        ColumnType := FieldSqlType;

        case FieldSqlType of
          stString, stUnicodeString, stGUID: begin
              //see test Bug#886194, we retrieve 565 as CP... the modula get returns the FBID of CP
              CPID := XSQLVAR.sqlsubtype and 255;
              //see: http://sourceforge.net/p/zeoslib/tickets/97/
              if (CPID = ConSettings^.ClientCodePage^.ID)
              then ZCodePageInfo := ConSettings^.ClientCodePage
              else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID); //get column CodePage info}
              ColumnCodePage := ZCodePageInfo.CP;
              if (FieldSqlType = stGUID) or (ConSettings^.ClientCodePage^.ID = CS_NONE) then begin
jmpLen:         Precision := XSQLVAR.sqllen;
                CharOctedLength := Precision;
              end else begin
                CharOctedLength := XSQLVAR.sqllen;
                Precision := XSQLVAR.sqllen div ZCodePageInfo^.CharWidth;
              end;
              Signed := sqltype = SQL_TEXT;
            end;
          stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
            then if FIsMetadataResultSet
              then ColumnCodePage := zCP_UTF8
              else begin //connected with CS_NONE no transliterions are made by FB
                CPID := FIBConnection.GetSubTypeTextCharSetID(TableName,ColumnName);
                if CPID = CS_NONE
                then ZCodePageInfo := ConSettings^.ClientCodePage
                else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID);
                ColumnCodePage := ZCodePageInfo.CP;
              end else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          stBytes: begin
              ColumnCodePage := zCP_Binary;
              goto jmpLen;
            end;
          stBinaryStream: ColumnCodePage := zCP_Binary;
          else begin
            ColumnCodePage := zCP_NONE;
            case FieldSqlType of
              stShort, stSmall, stInteger, stLong: Signed := True;
              stCurrency, stBigDecimal: begin
                Signed  := True;
                Scale   := -XSQLVAR.sqlscale;
                //first digit does not count because of overflow (FB does not allow this)
                case XSQLVAR.sqltype and not (1) of
                  SQL_SHORT:  Precision := 4;
                  SQL_LONG:   Precision := 9;
                  SQL_INT64:  Precision := 18;
                end;
              end;
              stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of milli second fractions
              {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //nothing todo
            end;
          end;
        end;
        ReadOnly := (TableName = '') or (ColumnName = '') or
          (ColumnName = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);
        Writable := not ReadOnly;
        Nullable := TZColumnNullableType(Ord(FIZSQLDA.IsNullable(I)));
        Scale := -sqlscale;
        CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;
  end;
  Open;
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
procedure TZInterbase6XSQLDAResultSet.AfterClose;
begin
  { Free output allocated memory }
  FXSQLDA := nil;
  FIZSQLDA := nil;
  FStmtHandle := 0; //don't forget!
  inherited AfterClose;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>IZBLob</code> object.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  BlobId: TISC_QUAD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (sqlind <> nil) and (sqlind^ = ISC_NULL) then begin
      LastWasNull := True;
      Result := nil;
    end else begin
      LastWasNull := False;
      case sqltype of
        SQL_QUAD,
        SQL_BLOB: begin
            BlobId := PISC_QUAD(sqldata)^;
            if ColumnType = stBinaryStream
            then Result := TZInterbase6BLob.Create(FIBConnection, BlobId,
              lsmRead, FOpenLobStreams)
            else Result := TZInterbase6Clob.Create(FIBConnection, BlobId,
              lsmRead, ColumnCodePage, FOpenLobStreams);
          end
        else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
      end;
    end;
  end;
end;

function TZInterbase6XSQLDAResultSet.GetConnection: IZInterbase6Connection;
begin
  Result := FIBConnection;
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
function TZInterbase6XSQLDAResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
  Status: ISC_STATUS;
label CheckE;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandleAddr^ = 0)) then
    Exit;

  { Fetch row. }
  if (FStmtType <> stExecProc) then begin //AVZ - Test for ExecProc - this is for multiple rows
    if (RowNo = 0) then
      FStmtHandle := FStmtHandleAddr^;
    Status := FPlainDriver.isc_dsql_fetch(@StatusVector,
      @FStmtHandle, FDialect, FXSQLDA);
    if Status = 0 then begin
      if (RowNo = 0) then RegisterCursor;
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end else if Status = 100  then begin
      {no error occoured -> notify IsAfterLast and close the recordset}
      RowNo := RowNo + 1;
      if FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
        goto CheckE;
      FStmtHandle := 0;
      if (FIBTransaction <> nil) then
        DeRegisterCursor;
    end else
CheckE:CheckInterbase6Error(FPlainDriver, StatusVector, Self);
  end else if RowNo = 0 then begin
    Result := True;
    RowNo := 1;
    LastRowNo := 1;
  end else if RowNo = 1 then
    RowNo := 2; //notify AfterLast
end;

procedure TZInterbase6XSQLDAResultSet.RegisterCursor;
begin
  FIBTransaction := FIBConnection.GetActiveTransaction;
  FIBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

procedure TZInterbase6XSQLDAResultSet.ResetCursor;
var StatusVector: TARRAY_ISC_STATUS;
begin
  if not Closed then begin
    if (FStmtHandle <> 0) then begin
      if (FStmtType <> stExecProc) then begin
         if (FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0) then
          CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcOther, 'isc_dsql_free_statement');
        FStmtHandle := 0;
        if FIBTransaction <> nil then
          DeRegisterCursor;
      end else
        FStmtHandle := 0;
    end;
    inherited ResetCursor;
  end;
end;

procedure TZInterbase6XSQLDAResultSet.DeRegisterCursor;
begin
  FIBTransaction.DeRegisterOpencursor(IZResultSet(TransactionResultSet));
  FIBTransaction := nil;
end;

{ TZInterbaseLobStream }

procedure TZInterbaseLobStream.CancelLob;
begin
  if not FReleased then begin
    Assert(Updated);
    Assert(FLobIsOpen);
    try
      if FPlainDriver.isc_cancel_blob(@FStatusVector, @FBlobHandle) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
    finally
      FLobIsOpen := False;
      Updated := False;
      FPosition := 0;
      PInt64(@BlobId)^ := 0;
      PInt64(@FOwnerLob.FBlobId)^ := 0;
      BlobInfo.TotalSize := 0;
    end;
  end;
end;

procedure TZInterbaseLobStream.CloseLob;
begin
  Assert(FLobIsOpen);
  if FPlainDriver.isc_close_blob(@FStatusVector, @FBlobHandle) <> 0 then
    CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  FLobIsOpen := False;
  FPosition := 0;
end;

constructor TZInterbaseLobStream.Create(const OwnerLob: TZInterbase6Lob);
begin
  inherited Create(OwnerLob, OwnerLob, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  BlobId := OwnerLob.FBlobId;
  FPlainDriver := OwnerLob.FPlainDriver;
  FDB_HANDLE := OwnerLob.FIBConnection.GetDBHandle;
  FTransactionHandle := OwnerLob.FIBConnection.GetTrHandle;
  BlobInfo :=  @FOwnerLob.FBlobInfo;
end;

procedure TZInterbaseLobStream.CreateLob;
begin
  { create blob handle }
  if FPlainDriver.isc_create_blob2(@FStatusVector, FDB_HANDLE, FTransactionHandle,
     @FBlobHandle, @BlobId, 0, nil) <> 0 then //EH: what about BPB
    CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
  FOwnerLob.FBlobId := BlobId; //write back to descriptor
  Updated := True;
  FLobIsOpen := True;
  FOwnerLob.FIsTemporary := True;
  FOwnerLob.FBlobInfoFilled := True;
  BlobInfo.NumSegments := 0;
  BlobInfo.TotalSize := 0;
  BlobInfo.BlobType := Byte(FOwnerLob.IsClob);
  BlobInfo.MaxSegmentSize := High(Word);
end;

destructor TZInterbaseLobStream.Destroy;
begin
  try
    if not FReleased and FLobIsOpen then
  {    close blob handle }
      if FPlainDriver.isc_close_blob(@FStatusVector, @FBlobHandle) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  finally
    FOwnerLob.FLobStream := nil;
    FOwnerLob.FIsUpdated := Updated;
  end;
  inherited;
end;

procedure TZInterbaseLobStream.FillBlobInfo;
var
  Items: array[0..3] of Byte;
  Results: array[0..99] of AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Item, ItemVal: Integer;
  StatusVector: TARRAY_ISC_STATUS;
begin
  Items[0] := isc_info_blob_num_segments;
  Items[1] := isc_info_blob_max_segment;
  Items[2] := isc_info_blob_total_length;
  Items[3] := isc_info_blob_type;

  if FPlainDriver.isc_blob_info(@StatusVector, @FBlobHandle, 4, @items[0],
      SizeOf(Results), @Results[0]) <> 0 then
    CheckInterbase6Error(FPlainDriver, StatusVector, Self);
  pBufStart := @Results[0];
  pBuf := pBufStart;
  while pBuf - pBufStart <= SizeOf(Results) do
  begin
    Item := Byte(pBuf^);
    if Item = isc_info_end then
      Break;

    Inc(pBuf);
    ItemVal := ReadInterbase6NumberWithInc(FPlainDriver, pBuf);

    case Item of
      isc_info_blob_num_segments:
        FOwnerLob.FBlobInfo.NumSegments := ItemVal;
      isc_info_blob_max_segment:
        FOwnerLob.FBlobInfo.MaxSegmentSize := ItemVal;
      isc_info_blob_total_length:
        FOwnerLob.FBlobInfo.TotalSize := ItemVal;
      isc_info_blob_type:
        FOwnerLob.FBlobInfo.BlobType := ItemVal;
    end;
  end;
  FOwnerLob.FBlobInfoFilled := True;
end;

function TZInterbaseLobStream.GetSize: Int64;
begin
  if Int64(BlobID) = 0 then
    Result := 0
  else begin
    if not FLobIsOpen then
      OpenLob;
    if FReleased
    then Result := 0
    else Result := FOwnerLob.FBlobInfo.TotalSize;
  end;
end;

procedure TZInterbaseLobStream.OpenLob;
begin
  if not FLobIsOpen then begin
    if (Int64(BlobID) <> 0) then begin
       if FPlainDriver.isc_open_blob2(@FStatusVector, FDB_HANDLE,
       FTransactionHandle, @FBlobHandle, @BlobId, 0 , nil) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
      FillBlobInfo;
    end else
      CreateLob;
    FLobIsOpen := True;
  end;
  //isc_blob_gen_bpb2
end;

function TZInterbaseLobStream.Read(var Buffer; Count: Longint): Longint;
var BytesRead, SegLen: ISC_USHORT;
  Status: ISC_STATUS;
var PBuf: PAnsiChar;
begin
  Result := 0;
  if not FReleased then begin
    if FOwnerLob.FLobStreamMode = lsmWrite then
      raise CreateWriteOnlyException;
    if not FLobIsOpen then
      OpenLob;

    PBuf := @Buffer;
    while Count > 0 do begin
      if Count > LongInt(FOwnerLob.FBlobInfo.MaxSegmentSize)
      then SegLen := FOwnerLob.FBlobInfo.MaxSegmentSize
      else SegLen := Word(Count);
      Status := FPlainDriver.isc_get_segment(@FStatusVector, @FBlobHandle,
             @BytesRead, SegLen, PBuf);
      case Status of
        0, isc_segment: begin
            Inc(Result, BytesRead);
            Dec(Count, BytesRead);
            Inc(PBuf, BytesRead);
          end;
        isc_segstr_eof: begin
           Inc(Result, BytesRead);
           Break;
          end
        else CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
      end;
    end;
  end;
  FPosition := FPosition + Result;
end;

function TZInterbaseLobStream.Seek(Offset: Longint; Origin: Word): Longint;
var P: Pointer;
begin
  if Origin = soFromEnd then
    Result := FOwnerLob.FBlobInfo.TotalSize - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else begin
    Result := OffSet;
    if (Result = 0) and (FPosition > 0) then //seek to bos ?
      CloseLob;
  end;
  if FPosition > Result //backward seeking is not supported
  then raise EZSQLException.Create(SOperationIsNotAllowed1)
  else if Result > FPosition then //seek forward?
    if FOwnerLob.FLobStreamMode = lsmRead then begin //allowed on reading mode only
      GetMem(P, FOwnerLob.FBlobInfo.MaxSegmentSize);
      try
        while Result > FPosition do begin
          OffSet := Result - FPosition;
          if OffSet > LongInt(FOwnerLob.FBlobInfo.MaxSegmentSize)
          then Origin := FOwnerLob.FBlobInfo.MaxSegmentSize
          else Origin := Word(OffSet);
          Inc(FPosition, Read(P^, Origin));
        end;
      finally
        FreeMem(P);
      end;
    end
  else raise CreateWriteOnlyException;
  FPosition := Result;
end;

function TZInterbaseLobStream.Write(const Buffer; Count: Longint): Longint;
var
  SegLen: Integer;
  TempBuffer: PAnsiChar;
begin
  Result := 0;
  if FReleased then Exit;
  if FOwnerLob.FLobStreamMode = lsmRead then
    raise EZSQLException.Create(SOperationIsNotAllowed2);
  if (FPosition = 0) and FOwnerLob.FIsTemporary then begin
    if FLobIsOpen and Updated
    then CancelLob
    else CreateLob;
  end;
  if not FLobIsOpen then
    OpenLob;
  { put data to blob }
  TempBuffer := @Buffer;
  while (Count > 0) do begin
    if Count > LongInt(BlobInfo.MaxSegmentSize)
    then SegLen := BlobInfo.MaxSegmentSize
    else SegLen := Count;
    if FPlainDriver.isc_put_segment(@FStatusVector, @FBlobHandle, SegLen, TempBuffer) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
    Inc(Result, SegLen);
    Inc(TempBuffer, SegLen);
    Dec(Count, SegLen);
  end;
  { in write mode we always have a new LOB }
  BlobInfo.TotalSize := BlobInfo.TotalSize + Result;
  Updated := True;
  FPosition := FPosition + Result;
end;

{ TZInterbaseCachedResultSet }

function TZInterbaseCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  InterbaseResultSet: IZInterbaseResultSet;
  IBConnection: IZInterbase6Connection;
  BlobID: TISC_Quad;
  i64: Int64 absolute BlobID;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if ResultSet.QueryInterface(IZInterbaseResultSet, InterbaseResultSet) = S_OK then begin
    {$IFNDEF GENERIC_INDEX}Dec(ColumnIndex);{$ENDIF}
    SQLType := TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType;
    if (Byte(SQLType) >= Byte(stAsciiStream)) and (Byte(SQLType) <= Byte(stBinaryStream)) then begin
      IBConnection := InterbaseResultSet.GetConnection;
      i64 := 0;
      if (SQLType = stBinaryStream)
      then Result := TZInterbase6Blob.Create(IBConnection, BlobID, LobStreamMode, fOpenLobStreams)
      else Result := TZInterbase6Clob.Create(IBConnection, BlobID, LobStreamMode,
        TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, FOpenLobStreams);
      UpdateLob(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, Result);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, SQLType);
  end else
    Result := nil;
end;

class function TZInterbaseCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZInterbaseRowAccessor;
end;

{ TZInterbase6Lob }

procedure TZInterbase6Lob.AfterConstruction;
begin
  FIBTransaction := FIBConnection.GetActiveTransaction;
  FIBTransaction.RegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZInterbase6Lob.BeforeDestruction;
begin
  if FIBTransaction <> nil then begin
    FIBTransaction.DeRegisterOpenUnCachedLob(Self);
    FIBTransaction := nil;
  end;
  inherited;
end;

procedure TZInterbase6Lob.Clear;
begin
  if PInt64(@FBlobID)^ <> 0 then try
    if FIsTemporary and (FLobStream <> nil) and FLobStream.FLobIsOpen then begin
      FLobStream.CancelLob;
      FreeAndNil(FLobStream);
    end;
  finally
    FIsTemporary := False;
    PInt64(@FBlobID)^ := 0;
    FIsUpdated := True;
  end;
end;

function TZInterbase6Lob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var Lob: TZInterbase6Lob;
    ALobID: TISC_QUAD;
    P: Pointer;
    SegmentSize, Count: LongInt;
    ReadStream, WriteStream: TStream;
begin
  PInt64(@ALobID)^ := 0;
  if FColumnCodePage = zCP_Binary
  then Lob := TZInterbase6BLob.Create(FIBConnection, ALobID, lsmWrite, FOpenLobStreams)
  else Lob := TZInterbase6Clob.Create(FIBConnection, ALobID, lsmWrite, FColumnCodePage, FOpenLobStreams);
  Result := Lob;
  if LobStreamMode <> lsmWrite then begin
    ReadStream := CreateLobStream(FColumnCodePage, lsmRead);
    Lob.Open(lsmWrite); //create a lob descriptor
    WriteStream := Lob.CreateLobStream(FColumnCodePage, lsmWrite);
    P := nil;
    try
      if FBlobInfo.TotalSize > 0 then begin
        segmentsize := FBlobInfo.MaxSegmentSize;
        GetMem(P, SegmentSize);
        while true do begin
          Count := ReadStream.Read(P^, SegmentSize);
          WriteStream.Write(P^, Count);
          if (Count < SegmentSize) or
            ((Count = SegmentSize) and (FBlobInfo.MaxSegmentSize = FBlobInfo.TotalSize)) then
            Break;
        end;
      end;
    finally
      if P <> nil then
        FreeMem(P);
      FreeAndNil(ReadStream);
      FreeAndNil(WriteStream);
    end;
  end;
  Lob.FLobStreamMode := LobStreamMode;
end;

constructor TZInterbase6Lob.Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
  LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  Assert(LobStreamMode <> lsmReadWrite);
  FLobStreamMode := LobStreamMode;
  FPlainDriver := Connection.GetPlainDriver;
  FIBConnection := Connection;
  FBlobId := BlobId;
end;

function TZInterbase6Lob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  FLobStreamMode := LobStreamMode;
  FLobStream := TZInterbaseLobStream.Create(Self);
  Result := FLobStream;
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

function TZInterbase6Lob.GetBlobId: TISC_QUAD;
begin
  Result := FBlobId;
end;

function TZInterbase6Lob.GetConSettings: PZConSettings;
begin
  if FIBConnection <> nil
  then Result := FIBConnection.GetConSettings
  else Result := nil;
end;

function TZInterbase6Lob.IsEmpty: Boolean;
begin
  Result := Int64(FBlobId) = 0;
end;

function TZInterbase6Lob.Length: Integer;
var Stream: TStream;
begin
  if FReleased or (PInt64(@FBlobID)^ = 0)
  then Result := 0
  else begin
    if not FBlobInfoFilled then begin
      Stream := CreateLobStream(FColumnCodePage, lsmRead);
      if Stream <> nil then
        FLobStream.FillBlobInfo;
    end;
    Result := FBlobInfo.TotalSize
  end;
end;

procedure TZInterbase6Lob.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FIBConnection <> nil) and (FIBConnection.GetActiveTransaction <> nil) and
     (FIBConnection.GetActiveTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then begin
    FIBTransaction.DeRegisterOpenUnCachedLob(Self);
    FIBTransaction := nil;
    FIBConnection := nil;
    Imm.ReleaseImmediat(Sender, AError);
    if FlobStream <> nil then begin
      FlobStream.FReleased := True;
      FreeAndNil(FlobStream);
    end;
  end;
  FReleased := true;
end;

{ TZInterbase6Clob }

constructor TZInterbase6Clob.Create(const Connection: IZInterbase6Connection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
end;

{ TZInterbase6Blob }

constructor TZInterbase6Blob.Create(const Connection: IZInterbase6Connection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, zCP_Binary, OpenLobStreams);
end;

{ TZInterbaseRowAccessor }

constructor TZInterbaseRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  CachedLobs: WordBool);
var TempColumns: TObjectList;
  I: Integer;
  Current: TZColumnInfo;
begin
  {EH: usually this code is NOT nessecary if we would handle the types as the
  providers are able to. But in current state we just copy all the incompatibilities
  from the DataSets into dbc... grumble.}
  TempColumns := TObjectList.Create(True);
  CopyColumnsInfo(ColumnsInfo, TempColumns);
  for I := 0 to TempColumns.Count -1 do begin
    Current := TZColumnInfo(TempColumns[i]);
    if Current.ColumnType in [stUnicodeString, stUnicodeStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no streams 4 sqlite
    if Current.ColumnType in [stBytes, stBinaryStream] then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, CachedLobs);
  TempColumns.Free;
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
