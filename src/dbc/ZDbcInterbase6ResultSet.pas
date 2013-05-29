{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6ResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, Types, ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6,
  ZPlainFirebirdInterbaseConstants,
  ZCompatibility, ZDbcResultSetMetadata, ZDbcInterbase6Utils, ZMessages;

type

  {** Implements Interbase ResultSet. }
  TZInterbase6ResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FCursorName: AnsiString;
    FStmtHandle: TISC_STMT_HANDLE;
    FSqlData: IZResultSQLDA;
    FParamsSqlData: IZParamsSQLDA;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure Open; override;
    function GetFieldValue(ColumnIndex: Integer): Variant;
    function InternalGetString(ColumnIndex: Integer): ZAnsiString; override;
  public
    constructor Create(Statement: IZStatement; SQL: string;
      var StatementHandle: TISC_STMT_HANDLE; CursorName: AnsiString;
      SqlData: IZResultSQLDA; ParamsSqlData: IZParamsSQLDA;
      CachedBlob: boolean);
    destructor Destroy; override;

    procedure Close; override;

    function GetCursorName: AnsiString; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetUnicodeString(ColumnIndex: Integer): WideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
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

    function MoveAbsolute(Row: Integer): Boolean; override;
    function Next: Boolean; override;
  end;

  {** Implements external blob wrapper object for Intebase/Firbird. }
  TZInterbase6Blob = class(TZAbstractBlob)
  private
    FBlobId: TISC_QUAD;
    FBlobRead: Boolean;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure ReadBlob;
  public
    constructor Create(IBConnection: IZInterbase6Connection;
      var BlobId: TISC_QUAD);

    function IsEmpty: Boolean; override;
    function Clone: IZBlob; override;
    function GetStream: TStream; override;
    function GetString: ZAnsiString; override;
    function GetUnicodeString: WideString; override;
    function GetBytes: TByteDynArray; override;
  end;

implementation

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  SysUtils, ZDbcUtils, ZEncoding;

{ TZInterbase6ResultSet }

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
procedure TZInterbase6ResultSet.Close;
begin
  if FStmtHandle <> 0 then
  begin
    { Free output allocated memory }
    FSqlData := nil;
    FParamsSqlData := nil;
    { Free allocate sql statement }
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE); //AVZ
  end;
  inherited Close;
end;

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZInterbase6ResultSet.Create(Statement: IZStatement; SQL: string;
  var StatementHandle: TISC_STMT_HANDLE; CursorName: AnsiString;
  SqlData: IZResultSQLDA; ParamsSqlData: IZParamsSQLDA; CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, nil,
    Statement.GetConnection.GetConSettings);
  
  FFetchStat := 0;
  FSqlData := SqlData;
  FCursorName := CursorName;
  FCachedBlob := CachedBlob;
  FIBConnection := Statement.GetConnection as IZInterbase6Connection;

  FParamsSqlData := ParamsSqlData;
  FStmtHandle := StatementHandle;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;

  Open;
end;

{**
   Free memory and destriy component
}
destructor TZInterbase6ResultSet.Destroy;
begin
  if not Closed then
    Close;
  inherited Destroy;
end;

{**
   Return field value by it index
   @param the index column 0 first, 1 second ...
   @return the field value as variant type
}
function TZInterbase6ResultSet.GetFieldValue(ColumnIndex: Integer): Variant;
begin
  CheckClosed;
  Result := FSqlData.GetValue(ColumnIndex);
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
function TZInterbase6ResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := FSqlData.GetBigDecimal(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function TZInterbase6ResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Size: Integer;
  Buffer: Pointer;
  BlobId: TISC_QUAD;
  TempStream: TStream;
begin
  Result := nil;
  CheckClosed;
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  if FCachedBlob then
  begin
    try
      BlobId := FSqlData.GetQuad(ColumnIndex - 1);
      with FIBConnection do
        ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle,
          BlobId, Size, Buffer);

      case GetMetaData.GetColumnType(ColumnIndex) of
        stBinaryStream:
          Result := TZAbstractBlob.CreateWithData(Buffer, Size, FIBConnection);
        stAsciiStream:
          begin
            Result := TZAbstractBlob.CreateWithData(Buffer, Size, FIBConnection);
            TempStream := TStringStream.Create(GetValidatedAnsiString(Result.GetString, Consettings, True));
            if Assigned(TempStream) then
            begin
              Result.SetStream(TempStream);
              TempStream.Free;
            end;
          end;
        else
          begin
            TempStream := GetValidatedUnicodeStream(Buffer, Size, ConSettings, True);
            Result := TZAbstractBlob.CreateWithStream(TempStream, FIBConnection, True);
            if Assigned(TempStream) then
              TempStream.Free;
          end;
      end;
    finally
      FreeMem(Buffer, Size);
    end;
  end
  else
  begin
    BlobId := FSqlData.GetQuad(ColumnIndex - 1);
    Result := TZInterbase6Blob.Create(FIBConnection, BlobId);
  end;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZInterbase6ResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := FSqlData.GetBoolean(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := FSqlData.GetByte(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
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
function TZInterbase6ResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := FSqlData.GetBytes(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := FSqlData.GetDate(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := FSqlData.GetDouble(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := FSqlData.GetFloat(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := FSqlData.GetInt(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := FSqlData.GetLong(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := FSqlData.GetShort(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.InternalGetString(ColumnIndex: Integer): ZAnsiString;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := FSqlData.GetString(ColumnIndex - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := FSqlData.GetTime(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
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
function TZInterbase6ResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := FSqlData.GetTimestamp(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZInterbase6ResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  Result := FSqlData.IsNull(ColumnIndex - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetString(ColumnIndex: Integer): String;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if ( ConSettings.ClientCodePage.ID = CS_NONE ) then //CharacterSet 'NONE' doesn't convert anything! Data as is!
    case FSqlData.GetIbSqlType(ColumnIndex -1) of
      SQL_VARYING, SQL_TEXT:
        if FSqlData.GetIbSqlSubType(ColumnIndex -1) = CS_NONE then
          Result := ZDbcString(FSqlData.GetString(ColumnIndex - 1))
        else
          Result := ZDbcString(FSqlData.GetString(ColumnIndex - 1),
            FIBConnection.GetPlainDriver.ValidateCharEncoding(FSqlData.GetIbSqlSubType(ColumnIndex -1)).CP);
      else
        Result := ZDbcString(FSqlData.GetString(ColumnIndex - 1));
    end
  else
    Result := ZDbcString(FSqlData.GetString(ColumnIndex - 1));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetUnicodeString(ColumnIndex: Integer): WideString;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if ( ConSettings.ClientCodePage.ID = CS_NONE ) then //CharacterSet 'NONE' doesn't convert anything! Data as is!
    case FSqlData.GetIbSqlType(ColumnIndex -1) of
      SQL_VARYING, SQL_TEXT:
        if FSqlData.GetIbSqlSubType(ColumnIndex -1) = CS_NONE then
          Result := ZDbcUnicodeString(FSqlData.GetString(ColumnIndex - 1))
        else
          Result := ZDbcUnicodeString(FSqlData.GetString(ColumnIndex - 1),
            FIBConnection.GetPlainDriver.ValidateCharEncoding(FSqlData.GetIbSqlSubType(ColumnIndex -1)).CP);
      else
        Result := ZDbcUnicodeString(FSqlData.GetString(ColumnIndex - 1));
    end
  else
    Result := ZDbcUnicodeString(FSqlData.GetString(ColumnIndex - 1));
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
function TZInterbase6ResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  RaiseForwardOnlyException;
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
function TZInterbase6ResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (LastRowNo >= MaxRows) then
    Exit;

  { Fetch row. }
  if (ResultSetType = rtForwardOnly) and (FFetchStat = 0) then
  begin
    with FIBConnection do
    begin
      if (FCursorName = '') then  //AVZ - Test for ExecProc - this is for multiple rows
      begin
        FFetchStat := GetPlainDriver.isc_dsql_fetch(@StatusVector,
          @FStmtHandle, GetDialect, FSqlData.GetData);
      end
      else
      begin     //AVZ - Cursor name has a value therefore the result set already exists
        FFetchStat := 1;
        Result := True;
        //FStmtHandle := nil;  //AVZ TEST
      end;
    end;

    if FFetchStat = 0 then
    begin
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end;
  end;
end;

{**
  Opens this recordset.
}
procedure TZInterbase6ResultSet.Open;
var
  I: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if FStmtHandle=0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  for I := 0 to FSqlData.GetFieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      ColumnName := GetFieldSqlName(I);
      TableName := GetFieldRelationName(I);
      ColumnLabel := GetFieldAliasName(I);
      FieldSqlType := GetFieldSqlType(I);
      ColumnType := FieldSqlType;

      if FieldSqlType in [stBytes, stString, stUnicodeString] then
      begin
        MaxLenghtBytes := GetFieldLength(I);
        if (FSqlData.GetIbSqlType(I) = SQL_TEXT) or ( FieldSQLType = stBytes ) then
        begin
          ColumnDisplaySize := MaxLenghtBytes;
          Precision := MaxLenghtBytes;
        end
        else
          if ConSettings.ClientCodePage^.ID = CS_NONE then
            Precision := GetFieldSize(ColumnType, ConSettings, MaxLenghtBytes,
              FIBConnection.GetPlainDriver.ValidateCharEncoding(GetIbSqlSubType(I)).CharWidth, @ColumnDisplaySize, True)
          else
            Precision := GetFieldSize(ColumnType, ConSettings, MaxLenghtBytes,
              ConSettings.ClientCodePage^.CharWidth, @ColumnDisplaySize, True);
      end;

      ReadOnly := (GetFieldRelationName(I) = '') or (GetFieldSqlName(I) = '')
        or (GetFieldSqlName(I) = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);

      if IsNullable(I) then
        Nullable := ntNullable
      else
        Nullable := ntNoNulls;

      Scale := GetFieldScale(I);
      AutoIncrement := False;
      //Signed := False;
      //CaseSensitive := True;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  inherited Open;
end;

function TZInterbase6ResultSet.GetCursorName: AnsiString;
begin
  Result := FCursorName;
end;

{ TZInterbase6Blob }

function TZInterbase6Blob.Clone: IZBlob;
begin
  Result := TZInterbase6Blob.Create(FIBConnection, FBlobId);
end;

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZInterbase6Blob.Create(IBConnection: IZInterbase6Connection;
  var BlobId: TISC_QUAD);
begin
  FBlobId := BlobId;
  FBlobRead := False;
  FIBConnection := IBConnection;
end;

function TZInterbase6Blob.GetBytes: TByteDynArray;
begin
  ReadBlob;
  Result := inherited GetBytes;
end;

function TZInterbase6Blob.GetStream: TStream;
begin
  ReadBlob;
  Result := inherited GetStream;
end;

function TZInterbase6Blob.GetString: ZAnsiString;
begin
  ReadBlob;
  Result := inherited GetString;
end;

function TZInterbase6Blob.GetUnicodeString: WideString;
begin
  ReadBlob;
  Result := inherited GetUnicodeString;
end;

function TZInterbase6Blob.IsEmpty: Boolean;
begin
  ReadBlob;
  Result := inherited IsEmpty;
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure TZInterbase6Blob.ReadBlob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  if FBlobRead then
   Exit;
   
  with FIBConnection do
    ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle, FBlobId, Size, Buffer);
  BlobSize := Size;
  BlobData := Buffer;
  FBlobRead := True;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

end.