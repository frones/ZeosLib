{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
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

unit ZDbcFirebirdResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses FmtBCD, {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  Firebird, ZCompatibility, ZClasses,
  ZDbcIntfs, ZDbcResultSet, ZDbcFirebirdStatement, ZDbcInterbase6Utils,
  ZDbcFirebird, ZDbcCachedResultSet, ZDbcCache, ZDbcResultSetMetadata,
  ZPlainFirebirdInterbaseConstants, ZPlainFirebirdInterbaseDriver,
  ZPlainFirebirdDriver, ZDbcFirebirdInterbase, ZDbcLogging;

type
  IZFirebirdResultSet = Interface(IZResultSet)
    ['{44E775F4-4E7D-4F92-9B97-5C5E504019F9}']
    function GetConnection: IZFirebirdConnection;
  End;

  PIResultSet = ^IResultSet;

  TZAbstractFirebirdResultSet = Class(TZAbstractInterbaseFirebirdResultSet,
    IZResultSet, IZFirebirdResultSet)
  private
    FFBStatement: IStatement;
    FStatus: IStatus;
    FMessageMetadata: IMessageMetadata;
    FDataBuffer: Pointer;
    FFBConnection: IZFirebirdConnection;
    FFBTransaction: IZFirebirdTransaction;
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FFirstRow: Boolean;
    procedure DeRegisterCursor;
  public //implement IZFirebirdResultSet
    function GetConnection: IZFirebirdConnection;
  public
    procedure RegisterCursor;
  protected
    procedure Open; override;
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      FBStmt: IStatement; MessageMetadata: IMessageMetadata; Status: IStatus);
    procedure AfterClose; override;
  public
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  end;

  TZFirebirdResultSet = class(TZAbstractFirebirdResultSet)
  private
    FResultset: IResultset;
    FResultSetAddr: PIResultSet;
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      FBStmt: IStatement; MessageMetadata: IMessageMetadata; Status: IStatus;
      ResultSet: PIResultSet);
    procedure AfterClose; override;
    procedure ResetCursor; override;

  public { Traversal/Positioning }
    function IsBeforeFirst: Boolean; override;
    function IsAfterLast: Boolean; override;
    function First: Boolean; override;
    function Last: Boolean; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
  end;

  TZFirebirdOutParamResultSet = class(TZAbstractFirebirdResultSet)
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      FBStmt: IStatement; MessageMetadata: IMessageMetadata; Status: IStatus;
      DataBuffer: Pointer);
  public { Traversal/Positioning }
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  TZFirebirdLob = class;

  {** EH: implements a sequential Firebird large object stream }
  TZFirebirdLobStream = class(TZImmediatelyReleasableLobStream, IImmediatelyReleasable)
  private
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FAttachment: IAttachment;
    FFBTransaction: ITransaction;
    FStatus: IStatus;
    FBlob: IBlob;
    FLobIsOpen: Boolean;
    FPosition: Integer;
    FOwnerLob: TZFirebirdLob;
  protected
    procedure FillBlobInfo;
    function GetSize: Int64; override;
  public
    constructor Create(const OwnerLob: TZFirebirdLob);
    destructor Destroy; override;
  public
    BlobId: TISC_QUAD;
    Updated: Boolean;
    BlobInfo: PIbBlobInfo;
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

  IZInterbaseLob = interface(IZLob)
    ['{85E3AA45-07A5-476E-83A7-15D40C4DEFCE}']
    function GetBlobId: TISC_QUAD;
  end;

  { TZFirebirdLob }

  TZFirebirdLob = Class(TZAbstractStreamedLob, IZLob, IZBlob,
    IImmediatelyReleasable, IZInterbaseLob)
  private
    FLobStream: TZFirebirdLobStream;
    FPlainDriver: TZFirebird3UpPlainDriver;
    FBlobId: TISC_QUAD;
    FFBConnection: IZFirebirdConnection;
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
    constructor Create(const Connection: IZFirebirdConnection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  End;

  TZFirebirdClob = Class(TZFirebirdLob, IZCLob)
  public
    constructor Create(const Connection: IZFirebirdConnection;
      BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
      ColumnCodePage: Word; const OpenLobStreams: TZSortedList);
  End;

  TZFirebirdBLob = Class(TZFirebirdLob)
  public
    constructor Create(const Connection: IZFirebirdConnection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
  End;

  {**
    Implements Firebird cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZFirebirdCachedResultSet = Class(TZCachedResultset)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob{IZLob}; override;
  End;

  TZFirebirdRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;

  IZFirebirdLob = interface(IZLob)
    ['{A6AFDB99-1472-4FCD-86A5-79506532FE68}']
    function GetBlobId: TISC_QUAD;
  end;

  function ConvertIB_FBType2SQLType(AType, ASubType: Cardinal; Scale: Integer): TZSQLType;

{$ENDIF ZEOS_DISABLE_FIREBIRD}
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD}

uses SysUtils, ZDbcUtils, ZSysUtils, ZFastCode, ZEncoding, ZMessages;

function ConvertIB_FBType2SQLType(AType, ASubType: Cardinal; Scale: Integer): TZSQLType;
begin
  case AType of
    SQL_VARYING, SQL_TEXT:
      if ASubType = CS_BINARY {Octets}
      then Result := stBytes
      else Result := stString;
    SQL_LONG:
        if Scale = 0 then
          Result := stInteger
        else if Scale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_SHORT:
        if Scale = 0 then
          Result := stSmall
        else if Scale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_FLOAT:
      Result := stFloat;
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_BOOLEAN, SQL_BOOLEAN_FB:
      Result := stBoolean;
    SQL_DATE: Result := stTimestamp;
    SQL_TYPE_TIME: Result := stTime;
    SQL_TYPE_DATE: Result := stDate;
    SQL_INT64:
        //https://firebirdsql.org/file/documentation/reference_manuals/fblangref25-en/html/fblangref25-datatypes-fixedtypes.html
        if Scale = 0 then
          Result := stLong
        else if Scale = -4 then //EH firebird supports a max precision of 18 only
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_QUAD, SQL_BLOB:
        if ASubType = isc_blob_text
        then Result := stAsciiStream
        else Result := stBinaryStream;
    SQL_ARRAY: Result := stArray;
    else  Result := TZSQLType.stUnknown;
  end;
end;
{ TZAbstractFirebirdResultSet }

procedure TZAbstractFirebirdResultSet.AfterClose;
begin
  inherited;
  if FMessageMetadata <> nil then begin
    FMessageMetadata.release;
    FMessageMetadata := nil;
  end;
  if FFBStatement <> nil then begin
    FFBStatement.release;
    FFBStatement := nil;
  end;
end;

constructor TZAbstractFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; FBStmt: IStatement; MessageMetadata: IMessageMetadata;
  Status: IStatus);
begin
  inherited Create(Statement, SQL);
  FFBStatement := FBStmt;
  FFBStatement.addRef;
  FMessageMetadata := MessageMetadata;
  FMessageMetadata.addRef;
  FStatus := Status;
  FFBConnection := Statement.GetConnection as IZFirebirdConnection;
  FPlainDriver := FFBConnection.GetPlainDriver;
  FFirstRow := True;
  Open;
end;

procedure TZAbstractFirebirdResultSet.DeRegisterCursor;
begin
  FFBTransaction.DeRegisterOpencursor(IZResultSet(TransactionResultSet));
  FFBTransaction := nil;
end;

function TZAbstractFirebirdResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var
  BlobId: TISC_QUAD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (sqlind <> nil) and (sqlind^ = ISC_NULL)
    then Result := nil
    else if sqltype = SQL_BLOB then begin
      BlobId := PISC_QUAD(sqldata)^;
      if ColumnType = stBinaryStream
      then Result := TZFirebirdBLob.Create(FFBConnection, BlobId,
        lsmRead, FOpenLobStreams)
      else Result := TZFirebirdClob.Create(FFBConnection, BlobId,
        lsmRead, ColumnCodePage, FOpenLobStreams);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
  end;
  LastWasNull := Result = nil;
end;

function TZAbstractFirebirdResultSet.GetConnection: IZFirebirdConnection;
begin
  Result := FFBConnection;
end;

{**
  Opens this recordset.
}
procedure TZAbstractFirebirdResultSet.Open;
var I, Len: Cardinal;
  CP_ID: Word;
  ColumnInfo: TZInterbaseFirebirdColumnInfo;
  P: PAnsiChar;
  ZCodePageInfo: PZCodePage;
label jmpLen;
begin
  I := FMessageMetadata.getCount(FStatus);
  if i = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  if FDataBuffer = nil then begin
    Len := FMessageMetadata.getMessageLength(FStatus);
    GetMem(FDataBuffer, Len);
  end;
  ColumnsInfo.Capacity := I;
  for i := 0 to I -1 do begin
    if ConSettings.ClientCodePage.ID = CS_NONE
    then CP_ID := zCP_UTF8
    else CP_ID := ConSettings.ClientCodePage.CP;

    ColumnInfo := TZInterbaseFirebirdColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);
    with ColumnInfo do begin
      sqltype := FMessageMetadata.getType(FStatus, I);
      sqlsubtype := FMessageMetadata.getSubType(FStatus, I);
      sqlscale := FMessageMetadata.getScale(FStatus, I);
      P := FMessageMetadata.getRelation(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      TableName := PRawToUnicode(P, Len, CP_ID);
      {$ELSE}
      System.SetString(TableName, P, Len);
      {$ENDIF}
      if TableName <> '' then begin //firebird does not corectly clear the name
        //see TestColumnTypeAndTableDetermination we get a 'ADD' in buffer back
        P := FMessageMetadata.getField(FStatus, I);
        Len := ZFastCode.StrLen(P);
        {$IFDEF UNICODE}
        ColumnName := PRawToUnicode(P, Len, CP_ID);
        {$ELSE}
        System.SetString(ColumnName, P, Len);
        {$ENDIF}
      end;
      P := FMessageMetadata.getAlias(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      ColumnLabel := PRawToUnicode(P, Len, CP_ID);
      {$ELSE}
      System.SetString(ColumnLabel, P, Len);
      {$ENDIF}
      sqltype := FMessageMetadata.getType(FStatus, I);
      sqlsubType := FMessageMetadata.getSubType(FStatus, I);
      Len := FMessageMetadata.getLength(FStatus, I);
      sqlscale := FMessageMetadata.getScale(FStatus, I);
      Scale := -sqlscale;
      ColumnType := ConvertIB_FBType2SQLType(sqltype, sqlsubtype, sqlscale);
      if FGUIDProps.ColumnIsGUID(ColumnType, len, ColumnName) then
        ColumnType := stGUID;
      if FMessageMetadata.isNullable(FStatus, I) then begin
        sqlind := PISC_SHORT(PAnsiChar(FDataBuffer)+FMessageMetadata.getNullOffset(FStatus, I));
        Nullable := ntNullable;
      end;
      sqldata := PAnsiChar(FDataBuffer)+FMessageMetadata.getOffset(FStatus, I);
      case ColumnType of
        stString: begin
            //see test Bug#886194, we retrieve 565 as CP... the modula returns the FBID of CP
            CP_ID := Word(FMessageMetadata.getCharSet(FStatus, I)) and 255;
            //see: http://sourceforge.net/p/zeoslib/tickets/97/
            if (CP_ID = ConSettings^.ClientCodePage^.ID)
            then ZCodePageInfo := ConSettings^.ClientCodePage
            else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP_ID); //get column CodePage info}
            ColumnCodePage := ZCodePageInfo.CP;
            if ConSettings^.ClientCodePage^.ID = CS_NONE then begin
  jmpLen:     Precision := Len;
              CharOctedLength := Precision;
            end else begin
              CharOctedLength := len;
              Precision := len div Cardinal(ZCodePageInfo^.CharWidth);
            end;
            Signed := sqltype = SQL_TEXT;
          end;
        stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
          then if FIsMetadataResultSet
            then ColumnCodePage := zCP_UTF8
            else begin //connected with CS_NONE no transliterions are made by FB
              CP_ID := FFBConnection.GetSubTypeTextCharSetID(TableName,ColumnName);
              if CP_ID = CS_NONE
              then ZCodePageInfo := ConSettings^.ClientCodePage
              else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP_ID);
              ColumnCodePage := ZCodePageInfo.CP;
            end else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        stBytes: begin
            ColumnCodePage := zCP_Binary;
            goto jmpLen;
          end;
        stBinaryStream: ColumnCodePage := zCP_Binary;
        else begin
          ColumnCodePage := zCP_NONE;
          case ColumnType of
            stShort, stSmall, stInteger, stLong: Signed := True;
            stCurrency, stBigDecimal: begin
              Signed  := True;
              Scale   := Scale;
              //first digit does not count because of overflow (FB does not allow this)
              case sqltype of
                SQL_SHORT:  Precision := 4;
                SQL_LONG:   Precision := 9;
                SQL_INT64:  Precision := 18;
              end;
            end;
            stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of milli second fractions
          end;
        end;
      end;
      ReadOnly := (TableName = '') or (ColumnName = '') or
        (ColumnName = 'RDB$DB_KEY') or (ColumnType = ZDbcIntfs.stUnknown);
      Writable := not ReadOnly;
      CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
    end;
  end;
  inherited Open;
end;

procedure TZAbstractFirebirdResultSet.RegisterCursor;
begin
  FFBTransaction := FFBConnection.GetActiveTransaction;
  FFBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

{ TZFirebirdResultSet }

procedure TZFirebirdResultSet.AfterClose;
begin
  if FResultset <> nil then begin
    FResultset.release;
    FResultset := nil;
  end;
  if FDataBuffer <> nil then begin
    FreeMem(FDataBuffer);
    FDataBuffer := nil;
  end;
  inherited AfterClose;
end;

constructor TZFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; FBStmt: IStatement; MessageMetadata: IMessageMetadata;
  Status: IStatus; ResultSet: PIResultSet);
begin
  inherited Create(Statement, SQL, FBStmt, MessageMetadata, Status);
  FResultset := ResultSet^;
  FResultSetAddr := ResultSet;
end;

{**
  Moves the cursor to the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
  <code>false</code> if there are no rows in the result set
}
function TZFirebirdResultSet.First: Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchFirst(FStatus, FDataBuffer);
    Result := Status = IStatus.RESULT_OK;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        LastRowNo := 0;
        RowNo := 1; //set AfterLast
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchFirst', Self, lcExecute);
    end else begin
      RowNo := 1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else Result := False;
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZFirebirdResultSet.IsAfterLast: Boolean;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Result := FResultset.isEof(FStatus)
  end else Result := True;
end;

{**
  Indicates whether the cursor is before the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is before the first row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZFirebirdResultSet.IsBeforeFirst: Boolean;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Result := FResultset.isBof(FStatus)
  end else Result := True;
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZFirebirdResultSet.Last: Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchLast(FStatus, FDataBuffer);
    Result := Status = IStatus.RESULT_OK;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        if RowNo = 0 then
          RowNo := 1; //else ?? which row do we have now?
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchLast', Self, lcExecute);
    end else begin
      //how to know a rowno now?
    end;
  end else Result := False;
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZFirebirdResultSet.MoveAbsolute(Row: Integer): Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchAbsolute(FStatus, Row, FDataBuffer);
    Result := Status = IStatus.RESULT_OK;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        RowNo := Row;
        if LastRowNo >= Row then
          LastRowNo := Row -1;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchAbsolute', Self, lcExecute);
    end else begin
      RowNo := Row;
      if LastRowNo < Row then
        LastRowNo := Row;
    end;
  end else
    Result := False;
end;

{**
  Moves the cursor a relative number of rows, either positive or negative.
  Attempting to move beyond the first/last row in the
  result set positions the cursor before/after the
  the first/last row. Calling <code>relative(0)</code> is valid, but does
  not change the cursor position.

  <p>Note: Calling the method <code>relative(1)</code>
  is different from calling the method <code>next()</code>
  because is makes sense to call <code>next()</code> when there
  is no current row,
  for example, when the cursor is positioned before the first row
  or after the last row of the result set.

  @return <code>true</code> if the cursor is on a row;
    <code>false</code> otherwise
}
function TZFirebirdResultSet.MoveRelative(Rows: Integer): Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchRelative(FStatus, Rows, FDataBuffer);
    RowNo := RowNo + Rows;
    Result := Status = IStatus.RESULT_OK;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        if LastRowNo >= RowNo then
          LastRowNo := RowNo -1;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchAbsolute', Self, lcExecute);
    end else begin
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else
    Result := False;
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
function TZFirebirdResultSet.Next: Boolean;
var Status: Integer;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FResultSetAddr^ = nil)) then
    Exit;
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchNext(FStatus, FDataBuffer);
    Result := Status = IStatus.RESULT_OK;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        if LastRowNo < RowNo then
          LastRowNo := RowNo;
        RowNo := RowNo +1; //set AfterLast
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchNext', Self, lcExecute);
    end else begin
      RowNo := RowNo +1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else Result := False;
end;

{**
  Moves the cursor to the previous row in this
  <code>ResultSet</code> object.

  <p><B>Note:</B> Calling the method <code>previous()</code> is not the same as
  calling the method <code>relative(-1)</code> because it
  makes sense to call</code>previous()</code> when there is no current row.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if it is off the result set
}
function TZFirebirdResultSet.Previous: Boolean;
var Status: Integer;
begin
  if not Closed and (RowNo > 0) then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchPrior(FStatus, FDataBuffer);
    Result := Status = IStatus.RESULT_OK;
    RowNo := RowNo -1;
    if not Result then begin
      if Status = IStatus.RESULT_NO_DATA then begin
        if LastRowNo < RowNo then
          LastRowNo := RowNo;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchPrior', Self, lcExecute);
    end;
  end else Result := False;
end;

procedure TZFirebirdResultSet.ResetCursor;
begin
  inherited;
  if FResultSet <> nil then begin
    FResultSet.close(FStatus);
    if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
      FFBConnection.HandleError(FStatus, 'IResultSet.close', Self, lcOther);
    FResultSet.release;
    FResultSet := nil;
  end;
  if FFBTransaction <> nil then
    DeRegisterCursor;
end;

{ TZFirebirdOutParamResultSet }

constructor TZFirebirdOutParamResultSet.Create(const Statement: IZStatement;
  const SQL: String; FBStmt: IStatement; MessageMetadata: IMessageMetadata;
  Status: IStatus; DataBuffer: Pointer);
begin
  FDataBuffer := DataBuffer;
  inherited Create(Statement, SQL, FBStmt, MessageMetadata, Status);
  LastRowNo := 1;
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZFirebirdOutParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  RowNo := Row;
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
function TZFirebirdOutParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 1
end;

{ TZFirebirdLobStream }

procedure TZFirebirdLobStream.CancelLob;
begin
  if not FReleased then begin
    Assert(Updated);
    Assert(FLobIsOpen);
    try
      FBlob.cancel(FStatus);
      if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      FOwnerLob.FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(FOwnerLob);
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

procedure TZFirebirdLobStream.CloseLob;
begin
  Assert(FLobIsOpen);
  FBlob.close(FStatus);
  if ((Fstatus.getState and IStatus.STATE_ERRORS) <> 0) then
    FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.close', Self, lcOther);
  FLobIsOpen := False;
  FPosition := 0;
end;

constructor TZFirebirdLobStream.Create(const OwnerLob: TZFirebirdLob);
begin
  inherited Create(OwnerLob, OwnerLob, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  BlobId := OwnerLob.FBlobId;
  FPlainDriver := OwnerLob.FPlainDriver;
  FFBTransaction := OwnerLob.FFBConnection.GetActiveTransaction.GetTransaction;
  FFBTransaction.AddRef;
  BlobInfo :=  @FOwnerLob.FBlobInfo;
  FStatus := OwnerLob.FFBConnection.GetStatus;
  FAttachment := OwnerLob.FFBConnection.GetAttachment;
  FAttachment.addRef;
end;

procedure TZFirebirdLobStream.CreateLob;
var WasRegistered: Boolean;
begin
  if FBlob <> nil then
    FBlob.release;
  WasRegistered := Int64(BlobId) <> 0;
  { create blob handle }
  FBlob := FAttachment.createBlob(Fstatus, FFBTransaction, @BlobId, 0, nil);
  if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
    FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
  if not WasRegistered then
    FOwnerLob.FFBConnection.GetActiveTransaction.RegisterOpenUnCachedLob(FOwnerLob);
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

destructor TZFirebirdLobStream.Destroy;
begin
  try
    if not FReleased then begin
      if (FBlob <> nil) then begin
        if FLobIsOpen then { close blob handle }
          FBlob.close(FStatus);
        if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
          FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.close', Self, lcOther);
        FBlob.release;
        if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
          FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.release', Self, lcOther);
        FBlob := nil;
      end;
    end;
  finally
    FOwnerLob.FLobStream := nil;
    FOwnerLob.FIsUpdated := Updated;
    FAttachment.release;
    FFBTransaction.release
  end;
  inherited;
end;

procedure TZFirebirdLobStream.FillBlobInfo;
var
  Items: array[0..3] of Byte;
  Results: array[0..99] of AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Item, ItemVal: Integer;
begin
  Items[0] := isc_info_blob_num_segments;
  Items[1] := isc_info_blob_max_segment;
  Items[2] := isc_info_blob_total_length;
  Items[3] := isc_info_blob_type;

  FBlob.getInfo(FStatus, 4, @Items[0], SizeOf(Results), @Results[0]);
  if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
     FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
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

function TZFirebirdLobStream.GetSize: Int64;
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

procedure TZFirebirdLobStream.OpenLob;
begin
  if not FLobIsOpen then begin
    if (Int64(BlobID) <> 0) then begin
      FBlob := FAttachment.openBlob(FStatus, FFBTransaction, @BlobID, 0, nil);
      if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      FillBlobInfo;
    end else
      CreateLob;
    FLobIsOpen := True;
  end;
  //isc_blob_gen_bpb2
end;

function TZFirebirdLobStream.Read(var Buffer; Count: Longint): Longint;
var BytesRead, SegLen: Cardinal;
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
      Status := FBlob.getSegment(FStatus, SegLen, PBuf, @BytesRead);
      if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      case Status of
        IStatus.RESULT_OK, IStatus.RESULT_SEGMENT: begin
            Inc(Result, BytesRead);
            Dec(Count, BytesRead);
            Inc(PBuf, BytesRead);
          end;
        IStatus.RESULT_NO_DATA: begin
           Inc(Result, BytesRead);
           Break;
          end
        else FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.getSegment', Self, lcOther);
      end;
    end;
  end;
  FPosition := FPosition + Result;
end;

function TZFirebirdLobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Origin = soFromEnd then
    Result := FOwnerLob.FBlobInfo.TotalSize - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else
    Result := OffSet;
  if (Result <> 0) then begin
    Result := FBlob.seek(FStatus, Origin, Offset);
    if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
      FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
  end;
  FPosition := Result;
end;

function TZFirebirdLobStream.Write(const Buffer; Count: Longint): Longint;
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
    FBlob.putSegment(FStatus, SegLen, TempBuffer);
    if (FStatus.getState and FStatus.STATE_ERRORS) <> 0 then
      FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
    Inc(Result, SegLen);
    Inc(TempBuffer, SegLen);
    Dec(Count, SegLen);
  end;
  { in write mode we always have a new LOB }
  BlobInfo.TotalSize := BlobInfo.TotalSize + Result;
  Updated := True;
  FPosition := FPosition + Result;
end;

{ TZFirebirdCachedResultSet }

function TZFirebirdCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  FirebirdResultSet: IZFirebirdResultSet;
  FBConnection: IZFirebirdConnection;
  BlobID: TISC_Quad;
  i64: Int64 absolute BlobID;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if ResultSet.QueryInterface(IZFirebirdResultSet, FirebirdResultSet) = S_OK then begin
    {$IFNDEF GENERIC_INDEX}Dec(ColumnIndex);{$ENDIF}
    SQLType := TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType;
    if (Byte(SQLType) >= Byte(stAsciiStream)) and (Byte(SQLType) <= Byte(stBinaryStream)) then begin
      FBConnection := FirebirdResultSet.GetConnection;
      i64 := 0;
      if (SQLType = stBinaryStream)
      then Result := TZFirebirdBlob.Create(FBConnection, BlobID, LobStreamMode, fOpenLobStreams)
      else Result := TZFirebirdClob.Create(FBConnection, BlobID, LobStreamMode,
        TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, FOpenLobStreams);
      UpdateLob(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, Result);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, SQLType);
  end;
end;

class function TZFirebirdCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZFirebirdRowAccessor;
end;

{ TZFirebirdRowAccessor }

constructor TZFirebirdRowAccessor.Create(ColumnsInfo: TObjectList;
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
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no National streams 4 IB/FB
    if Current.ColumnType in [stBytes, stBinaryStream] then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, CachedLobs);
  TempColumns.Free;
end;

{ TZFirebirdClob }

constructor TZFirebirdClob.Create(const Connection: IZFirebirdConnection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
end;

{ TZFirebirdBLob }

constructor TZFirebirdBLob.Create(const Connection: IZFirebirdConnection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, zCP_Binary, OpenLobStreams);
end;

{ TZFirebirdLob }

procedure TZFirebirdLob.AfterConstruction;
begin
  if Int64(FBlobID) <> 0 then
    FFBConnection.GetActiveTransaction.RegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZFirebirdLob.BeforeDestruction;
begin
  if Int64(FBlobID) <> 0 then
    FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZFirebirdLob.Clear;
begin
  if Int64(FBlobID) <> 0 then try
    if FIsTemporary and (FLobStream <> nil) and FLobStream.FLobIsOpen then begin
      FLobStream.CancelLob;
      FreeAndNil(FLobStream);
    end;
  finally
    FIsTemporary := False;
    PInt64(@FBlobID)^ := 0;
    FIsUpdated := True;
    FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
  end;
end;

function TZFirebirdLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var Lob: TZFirebirdLob;
    ALobID: TISC_QUAD;
    P: Pointer;
    SegmentSize, Count: LongInt;
    ReadStream, WriteStream: TStream;
begin
  PInt64(@ALobID)^ := 0;
  if FColumnCodePage = zCP_Binary
  then Lob := TZFirebirdBLob.Create(FFBConnection, ALobID, lsmWrite, FOpenLobStreams)
  else Lob := TZFirebirdClob.Create(FFBConnection, ALobID, lsmWrite, FColumnCodePage, FOpenLobStreams);
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

constructor TZFirebirdLob.Create(const Connection: IZFirebirdConnection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  Assert(LobStreamMode <> lsmReadWrite);
  FLobStreamMode := LobStreamMode;
  FPlainDriver := Connection.GetPlainDriver;
  FFBConnection := Connection;
  FBlobId := BlobId;
end;

function TZFirebirdLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  FLobStreamMode := LobStreamMode;
  FLobStream := TZFirebirdLobStream.Create(Self);
  Result := FLobStream;
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

function TZFirebirdLob.GetBlobId: TISC_QUAD;
begin
  Result := FBlobId;
end;

function TZFirebirdLob.GetConSettings: PZConSettings;
begin
  if FFBConnection <> nil
  then Result := FFBConnection.GetConSettings
  else Result := nil;
end;

function TZFirebirdLob.IsEmpty: Boolean;
begin
  Result := Int64(FBlobId) = 0;
end;

function TZFirebirdLob.Length: Integer;
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

procedure TZFirebirdLob.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FFBConnection <> nil) and (FFBConnection.GetActiveTransaction <> nil) and
     (FFBConnection.GetActiveTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then begin
    FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
    Imm.ReleaseImmediat(Sender, AError);
    if FlobStream <> nil then begin
      FlobStream.FReleased := True;
      FreeAndNil(FlobStream);
    end;
  end;
  FReleased := true;
end;

initialization
{$ENDIF ZEOS_DISABLE_FIREBIRD}
end.
