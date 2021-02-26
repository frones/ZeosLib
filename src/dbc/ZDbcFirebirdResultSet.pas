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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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

uses
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZPlainFirebird, ZCompatibility, ZClasses,
  ZDbcResultSet, ZDbcInterbase6Utils,
  ZDbcFirebird, ZDbcCachedResultSet, ZDbcCache, ZDbcResultSetMetadata,
  ZPlainFirebirdInterbaseDriver,
  ZDbcFirebirdInterbase, ZDbcLogging, ZDbcIntfs;

type
  IZFirebirdResultSet = Interface(IZResultSet)
    ['{44E775F4-4E7D-4F92-9B97-5C5E504019F9}']
    function GetConnection: IZFirebirdConnection;
  End;

  PIResultSet = ^IResultSet;

  TZAbstractFirebirdResultSet = Class(TZAbstractInterbaseFirebirdResultSet,
    IZResultSet, IZFirebirdResultSet)
  private
    FStatus: IStatus;
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
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; OrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
      Status: IStatus; DataBuffer: Pointer);
    procedure ResetCursor; override;
  public
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  end;

  TZFirebirdResultSet = class(TZAbstractFirebirdResultSet)
  private
    FResultset: IResultset;
    FResultSetAddr: PIResultSet;
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; OrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
      Status: IStatus; DataBuffer: Pointer; ResultSet: PIResultSet);
  public
    /// <summary>Resets the Cursor position to Row 0, and releases servver
    ///  and client resources.</summary>
    procedure ResetCursor; override;
  public { Traversal/Positioning }
    /// <summary>Indicates whether the cursor is before the first row in this
    ///  <c>ResultSet</c> object.</summary>
    /// <returns><c>true</c> if the cursor is before the first row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsBeforeFirst: Boolean; override;
    /// <summary>Indicates whether the cursor is after the last row in this
    ///  <c>ResultSet</c> object.
    /// <returns><c>true</c> if the cursor is after the last row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsAfterLast: Boolean; override;
    /// <summary>Moves the cursor to the first row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the resultset</returns>
    function First: Boolean; override;
    /// <summary>Moves the cursor to the last row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the result set </returns>
    function Last: Boolean; override;
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;
    /// <summary>Moves the cursor a relative number of rows, either positive
    ///  or negative. Attempting to move beyond the first/last row in the
    ///  result set positions the cursor before/after the the first/last row.
    ///  Calling <c>relative(0)</c> is valid, but does not change the cursor
    ///  position. Note: Calling the method <c>relative(1)</c> is different
    ///  from calling the method <c>next()</c> because is makes sense to call
    ///  <c>next()</c> when there is no current row, for example, when the
    ///  cursor is positioned before the first row or after the last row of the
    ///  result set. </summary>
    /// <param>"Rows" the relative number of rows to move the cursor.</param>
    /// <returns><c>true</c> if the cursor is on a row;<c>false</c> otherwise
    /// </returns>
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
  end;

  TZFirebirdOutParamResultSet = class(TZAbstractFirebirdResultSet)
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; OrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
      Status: IStatus; DataBuffer: Pointer);
  public { Traversal/Positioning }
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  TZFirebirdLob = class;

  {** EH: implements a Firebird large object stream }
  TZFirebirdLobStream = class(TZAbstractInterbaseFirebirdLobStream)
  private
    FAttachment: IAttachment;
    FFBTransaction: ITransaction;
    FStatus: IStatus;
    FBlob: IBlob;
    FOwnerLob: TZFirebirdLob;
  protected
    procedure FillBlobInfo;
    function GetSize: Int64; override;
  public
    constructor Create(const OwnerLob: TZFirebirdLob);
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

  { TZFirebirdLob }

  TZFirebirdLob = Class(TZAbstractStreamedLob, IZLob, IZBlob,
    IImmediatelyReleasable, IZInterbaseFirebirdLob)
  private
    FLobStream: TZFirebirdLobStream;
    FPlainDriver: TZFirebirdPlainDriver;
    FBlobId: TISC_QUAD;
    FFBConnection: IZFirebirdConnection;
    FFBTransaction: IZFirebirdTransaction;
    FReleased: Boolean;
    FBlobInfo: TIbBlobInfo;
    FBlobInfoFilled: Boolean;
    FIsTemporary: Boolean;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //IImmediatelyReleasable
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
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
    constructor Create(const Connection: IZFirebirdConnection; const BlobId: TISC_QUAD;
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
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
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
    SQL_DEC16, SQL_DEC34,
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_BOOLEAN, SQL_BOOLEAN_FB:
      Result := stBoolean;
    SQL_DATE, SQL_TIMESTAMP_TZ, SQL_TIMESTAMP_TZ_EX: Result := stTimestamp;
    SQL_TYPE_TIME, SQL_TIME_TZ, SQL_TIME_TZ_EX: Result := stTime;
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
    SQL_INT128, SQL_DEC_FIXED: Result := stBigDecimal;
    else  Result := stUnknown;
  end;
end;
{ TZAbstractFirebirdResultSet }

constructor TZAbstractFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata;
  OrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
  Status: IStatus; DataBuffer: Pointer);
var I, Len, OffSet: Cardinal;
  CP_ID: Word;
  ColumnInfo: TZInterbaseFirebirdColumnInfo;
  P: PAnsiChar;
  ZCodePageInfo: PZCodePage;
label jmpLen;
begin
  inherited Create(Statement, SQL);
  FStatus := Status;
  FFBConnection := Statement.GetConnection as IZFirebirdConnection;
  FPlainDriver := FFBConnection.GetPlainDriver;
  FFirstRow := True;
  FDataBuffer := DataBuffer;

  I := MessageMetadata.getCount(FStatus);
  if i = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := I;
  for i := 0 to I -1 do begin
    {$IFDEF UNICODE}
    if ConSettings.ClientCodePage.ID = CS_NONE
    then CP_ID := zCP_UTF8
    else CP_ID := ConSettings.ClientCodePage.CP;
    {$ENDIF UNICODE}
    ColumnInfo := TZInterbaseFirebirdColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);
    with ColumnInfo do begin
      P := MessageMetadata.getRelation(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      PRawToUnicode(P, Len, CP_ID, TableName);
      {$ELSE}
      System.SetString(TableName, P, Len);
      {$ENDIF}
      if TableName <> '' then begin //firebird does not corectly clear the name
        //see TestColumnTypeAndTableDetermination we get a 'ADD' in buffer back
        P := MessageMetadata.getField(FStatus, I);
        Len := ZFastCode.StrLen(P);
        {$IFDEF UNICODE}
        PRawToUnicode(P, Len, CP_ID, ColumnName);
        {$ELSE}
        System.SetString(ColumnName, P, Len);
        {$ENDIF}
      end;
      P := MessageMetadata.getAlias(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      PRawToUnicode(P, Len, CP_ID, ColumnLabel);
      {$ELSE}
      System.SetString(ColumnLabel, P, Len);
      {$ENDIF}
      sqltype := PZIBFBOrgSqlTypeAndScale(OrgTypeList[i]).sqltype;
      sqlsubType := MessageMetadata.getSubType(FStatus, I);
      sqlscale := PZIBFBOrgSqlTypeAndScale(OrgTypeList[i]).scale;
      Scale := -sqlscale;
      if (sqltype = SQL_TEXT) or (sqltype = SQL_VARYING) then begin //SQL_BLOB
        CP_ID := Word(MessageMetadata.getCharSet(FStatus, I)) and 255;
        ColumnType := ConvertIB_FBType2SQLType(sqltype, CP_ID, sqlscale);
      end else
        ColumnType := ConvertIB_FBType2SQLType(sqltype, sqlsubtype, sqlscale);
      //if (sqltype <> SQL_INT128) and (sqltype <> SQL_DEC_FIXED) then
        sqltype := MessageMetadata.getType(FStatus, I);
      sqlsubType := MessageMetadata.getSubType(FStatus, I);
      sqlscale := MessageMetadata.getScale(FStatus, I);
      Len := MessageMetadata.getLength(FStatus, I);
      if FGUIDProps.ColumnIsGUID(ColumnType, len, ColumnName) then
        ColumnType := stGUID;
      //if MessageMetadata.isNullable(FStatus, I) then begin EH: bug in 4.0, Nullable isn't copied and not setable with builder
      if PZIBFBOrgSqlTypeAndScale(OrgTypeList[i]).Nullable then begin
        OffSet := MessageMetadata.getNullOffset(FStatus, I);
        sqlind := PISC_SHORT(PAnsiChar(FDataBuffer)+OffSet);
        Nullable := ntNullable;
      end;
      OffSet := MessageMetadata.getOffset(FStatus, I);
      sqldata := PAnsiChar(FDataBuffer)+OffSet;
      if sqlind = sqldata then
        sqlind := nil;
      case ColumnType of
        stString, stGUID: begin
            //see test Bug#886194, we retrieve 565 as CP... the modula returns the FBID of CP
            CP_ID := Word(MessageMetadata.getCharSet(FStatus, I)) and 255;
            //see: http://sourceforge.net/p/zeoslib/tickets/97/
            if (CP_ID = ConSettings^.ClientCodePage^.ID)
            then ZCodePageInfo := ConSettings^.ClientCodePage
            else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP_ID); //get column CodePage info}
            ColumnCodePage := ZCodePageInfo.CP;
            if (ColumnType = stGUID) or (ConSettings^.ClientCodePage^.ID = CS_NONE) then begin
  jmpLen:     Precision := Len;
              CharOctedLength := Precision;
            end else begin
              CharOctedLength := len;
              Precision := len div Cardinal(ZCodePageInfo^.CharWidth);
            end;
            if sqltype = SQL_TEXT then
              Scale := Precision;
          end;
        stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
          then if FIsMetadataResultSet
            then ColumnCodePage := zCP_UTF8
            else begin //connected with CS_NONE no transliterations are made by FB
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
                //SQL_DEC16:  Precision := 16;
                //SQL_DEC34:  Precision := 34;
                SQL_INT128,
                SQL_DEC_FIXED: Precision := 38;
                {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //nothing todo
              end;
            end;
            stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of millisecond fractions
            {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //nothing todo
          end;
        end;
      end;
      ReadOnly := (TableName = '') or (ColumnName = '') or
        (ColumnName = 'RDB$DB_KEY') or (ColumnType = ZDbcIntfs.stUnknown);
      Writable := not ReadOnly;
      CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
    end;
  end;
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

procedure TZAbstractFirebirdResultSet.RegisterCursor;
begin
  FFBTransaction := FFBConnection.GetActiveTransaction;
  FFBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

procedure TZAbstractFirebirdResultSet.ResetCursor;
begin
  inherited ResetCursor;
  if FFBTransaction <> nil then
    DeRegisterCursor;
end;

{ TZFirebirdResultSet }

constructor TZFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata;
  OrgTypeList: TZIBFBOrgSqlTypeAndScaleList; Status: IStatus;
  DataBuffer: Pointer; ResultSet: PIResultSet);
begin
  inherited Create(Statement, SQL, MessageMetadata, OrgTypeList, Status, DataBuffer);
  FResultset := ResultSet^;
  FResultSetAddr := ResultSet;
  FCursorLocation := rctServer;
end;

function TZFirebirdResultSet.First: Boolean;
var Status: Integer;
begin
  Result := False;
  if ResultSetType = rtForwardOnly then
    if RowNo = 0
    then Result := Next
    else raise EZSQLException.Create(SOperationIsNotAllowed1)
  else if not Closed and (FResultSetAddr^ <> nil) then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchFirst(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        LastRowNo := 0;
        RowNo := 1; //set AfterLast
      end else
        FFBConnection.HandleErrorOrWarning(lcExecute, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchFirst', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end else begin
      RowNo := 1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end;
end;

function TZFirebirdResultSet.IsAfterLast: Boolean;
begin
  if ResultSetType = rtForwardOnly then
    Result := (RowNo > LastRowNo)
  else begin
    Result := True;
    if not Closed and (FResultSetAddr^ <> nil) then begin
      if (FResultSet = nil) then begin
        FResultSet := FResultSetAddr^;
        RegisterCursor;
      end;
      Result := FResultset.isEof(FStatus)
    end;
  end;
end;

function TZFirebirdResultSet.IsBeforeFirst: Boolean;
begin
  if ResultSetType = rtForwardOnly then
    Result :=  (FRowNo = 0)
  else begin
    Result := True;
    if not Closed and (FResultSetAddr^ <> nil) then begin
      if (FResultSet = nil) then begin
        FResultSet := FResultSetAddr^;
        RegisterCursor;
      end;
      Result := FResultset.isBof(FStatus)
    end;
  end;
end;

function TZFirebirdResultSet.Last: Boolean;
var Status: Integer;
begin
  Result := False;
  if ResultSetType = rtForwardOnly then
    raise EZSQLException.Create(SOperationIsNotAllowed1);
  if not Closed and (FResultSetAddr^ <> nil) then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchLast(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if RowNo = 0 then
          RowNo := 1; //else ?? which row do we have now?
      end else
        FFBConnection.HandleErrorOrWarning(lcExecute, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchLast', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end else begin
      //how to know a rowno now?
    end;
  end;
end;

function TZFirebirdResultSet.MoveAbsolute(Row: Integer): Boolean;
var Status: Integer;
begin
  Result := False;
  if ResultSetType = rtForwardOnly then
    if Row = RowNo+1
    then Result := Next
    else raise EZSQLException.Create(SOperationIsNotAllowed1);
  if not Closed and (FResultSetAddr^ <> nil) then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchAbsolute(FStatus, Row, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        RowNo := Row;
        if LastRowNo >= Row then
          LastRowNo := Row -1;
      end else
        FFBConnection.HandleErrorOrWarning(lcExecute, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchAbsolute', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end else begin
      RowNo := Row;
      if LastRowNo < Row then
        LastRowNo := Row;
    end;
  end;
end;

function TZFirebirdResultSet.MoveRelative(Rows: Integer): Boolean;
var Status: Integer;
begin
  Result := False;
  if ResultSetType = rtForwardOnly then
    if Rows = 1
    then Result := Next
    else raise EZSQLException.Create(SOperationIsNotAllowed1);
  if not Closed and (FResultSetAddr^ <> nil) then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchRelative(FStatus, Rows, FDataBuffer);
    RowNo := RowNo + Rows;
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if LastRowNo >= RowNo then
          LastRowNo := RowNo -1;
      end else
        FFBConnection.HandleErrorOrWarning(lcExecute, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchAbsolute', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end else begin
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end;
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
  if Closed or (FResultSetAddr^ = nil) or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
    Exit;
  if not Closed then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchNext(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then try
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if GetType = rtForwardOnly then begin
          FResultSet.Close(FStatus); //dereister cursor from Txn
          if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
            FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.close', Self)
          else // Close() releases intf on success
            FResultSet:= nil;
        end;
      end else
        FFBConnection.HandleErrorOrWarning(lcFetch, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchNext', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    finally
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
      RowNo := RowNo+1; //tag as after last keep this, else the FPC grids are getting viny nilly
      //if statement is prepared but a syntax error did happen on execute only
      //example TestSF443
      if Assigned(FResultSet) then
        FResultSet.release;
      FResultSet := nil;
      FResultSetAddr^ := nil;
      if (FFBTransaction <> nil) then
        DeRegisterCursor;
    end else begin
      RowNo := RowNo +1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end;
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
  Result := False;
  if ResultSetType = rtForwardOnly then
    raise EZSQLException.Create(SOperationIsNotAllowed1);
  if not Closed and (FResultSetAddr^ <> nil) then begin
    if (FResultSet = nil) then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchPrior(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    RowNo := RowNo -1;
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if LastRowNo < RowNo then
          LastRowNo := RowNo;
      end else
        FFBConnection.HandleErrorOrWarning(lcExecute, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.fetchPrior', Self);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end;
  end;
end;

procedure TZFirebirdResultSet.ResetCursor;
begin
  try
    if FResultSet <> nil then begin
      FResultSet.close(FStatus);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IResultSet.close', Self)
      else begin // Close() releases intf on success
        FResultSet:= nil;
        FResultSetAddr^:= nil;
      end;
    end;
  finally
    if FResultSet <> nil then begin
      FResultSet.release;
      FResultSet := nil;
      FResultSetAddr^ := nil;
    end;
    inherited ResetCursor;
  end;
end;

{ TZFirebirdOutParamResultSet }

constructor TZFirebirdOutParamResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata; OrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
  Status: IStatus; DataBuffer: Pointer);
begin
  inherited Create(Statement, SQL, MessageMetadata, OrgTypeList, Status, DataBuffer);
  LastRowNo := 1;
  FCursorLocation := rctClient;
end;

function TZFirebirdOutParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  if (Row >= 0) and (Row <= 2) then
    RowNo := Row;
end;

function TZFirebirdOutParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 2; //set AfterLast
end;

{ TZFirebirdLobStream }

procedure TZFirebirdLobStream.CancelLob;
begin
  if not FReleased then begin
    Assert(Updated);
    Assert(FLobIsOpen);
    try
      FBlob.cancel(FStatus);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.cancel', Self);
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
  try
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.close', Self);
  finally
    FBlob.release;
  end;
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
  if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
    FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IAttachment.createBlob', Self);
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
    if not FReleased and (FBlob <> nil) and FLobIsOpen then
      CloseLob;
  finally
    FOwnerLob.FLobStream := nil;
    FOwnerLob.FIsUpdated := Updated;
    FAttachment.release;
    FFBTransaction.release;
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
  if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
    FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.GetInfo', Self);
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
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IAttachment.openBlob', Self);
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
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.getSegment', Self);
      case Status of
        {$IFDEF WITH_CLASS_CONST}
        IStatus.RESULT_OK, IStatus.RESULT_SEGMENT
        {$ELSE}
        IStatus_RESULT_OK, IStatus_RESULT_SEGMENT
        {$ENDIF}: begin
            Inc(Result, Integer(BytesRead));
            Dec(Count, BytesRead);
            Inc(PBuf, BytesRead);
          end;
        {$IFDEF WITH_CLASS_CONST}
        IStatus.RESULT_NO_DATA
        {$ELSE}
        IStatus_RESULT_NO_DATA
        {$ENDIF}: begin
           Inc(Result, Integer(BytesRead));
           Break;
          end
        else FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.getSegment', Self);
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
  else begin
    Result := OffSet;
    if (Result = 0) and (FPosition > 0) then //seek to bos ?
      CloseLob;
  end;
  if (Result <> FPosition) then begin
    Result := FBlob.seek(FStatus, Origin, Offset);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
       FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.seek', Self);
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
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FOwnerLob.FFBConnection.HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.putSegment', Self);
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
  end else
    Result := nil;
end;

class function TZFirebirdCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZFirebirdRowAccessor;
end;

{ TZFirebirdRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ConSettings, ColumnCodePage" not used} {$ENDIF}
class function TZFirebirdRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stUnicodeString, stUnicodeStream] then
    Result := TZSQLType(Byte(Result)-1); // no national chars 4 IB/FB
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
  FFBTransaction := FFBConnection.GetActiveTransaction;
  FFBTransaction.RegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZFirebirdLob.BeforeDestruction;
begin
  if FFBTransaction <> nil then begin
    FFBTransaction.DeRegisterOpenUnCachedLob(Self);
    FFBTransaction := nil;
  end;
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
  const BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
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
    FFBTransaction.DeRegisterOpenUnCachedLob(Self);
    FFBTransaction := nil;
    FFBConnection := nil;
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
