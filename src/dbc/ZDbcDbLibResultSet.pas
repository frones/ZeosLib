{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         DBLib Resultset common functionality            }
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

unit ZDbcDbLibResultSet;

interface

{$I ZDbc.inc}

uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZDbcIntfs, ZDbcResultSet, ZCompatibility, ZDbcResultsetMetadata,
  ZDbcGenericResolver, ZDbcCachedResultSet, ZDbcCache, ZDbcDBLib,
  ZPlainDbLibConstants, ZPlainDBLibDriver;

type
  {** Implements DBLib ResultSet. }
  TZDBLibResultSet = class(TZAbstractResultSet)
  private
    FSQL: string;
    FCheckDBDead: Boolean;
    FHandle: PDBPROCESS;
    DBLibColTypeCache: Array of TTDSType;
    DBLibColumnCount: Integer;
    FUserEncoding: TZCharEncoding;
    procedure CheckColumnIndex(ColumnIndex: Integer);
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      UserEncoding: TZCharEncoding = ceDefault);

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with mssql and sybase specific functionality. }
  TZDBLibCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

implementation

uses ZMessages, ZDbcLogging, ZDbcDBLibUtils, ZEncoding, ZSysUtils, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
;

{ TZDBLibResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param Handle a DBLib specific query handle.
}
constructor TZDBLibResultSet.Create(const Statement: IZStatement; const SQL: string;
  UserEncoding: TZCharEncoding);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);
  Statement.GetConnection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  FSQL := SQL;
  FCheckDBDead := FPlainDriver.GetProtocol = 'mssql';
  FUserEncoding := UserEncoding;

  Open;
end;

{**
  Opens this recordset.
}
procedure TZDBLibResultSet.Open;
var
  ColumnInfo: TZColumnInfo;
  I: Integer;
  ColInfo: DBCOL;
  tdsColInfo: TTDSDBCOL;
label AssignGeneric;
  procedure AssignGenericColumnInfoFromZDBCOL(ArrayIndex: Integer; ColInfo: ZDBCOL);
  begin
    DBLibColTypeCache[ArrayIndex] := TTDSType(ColInfo.Typ);
    ColumnInfo.ColumnType := ConvertTDSTypeToSqlType(DBLibColTypeCache[ArrayIndex], ConSettings.CPType);
    if DBLibColTypeCache[ArrayIndex] in [tdsNumeric, tdsDecimal] then
      ColumnInfo.Scale := ColInfo.Scale
    else
      ColumnInfo.Scale := 0;
    ColumnInfo.Precision := ColInfo.MaxLength;
    ColumnInfo.CaseSensitive := ColInfo.CaseSensitive = 1;
    ColumnInfo.Nullable := TZColumnNullableType(ColInfo.Null);
    ColumnInfo.ReadOnly := not ColInfo.Updatable = 1;
    ColumnInfo.Writable := ColInfo.Updatable = 1;
    ColumnInfo.AutoIncrement := ColInfo.Identity;
    ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong];
  end;
begin
//Check if the current statement can return rows
  if FPlainDriver.dbCmdRow(FHandle) <> DBSUCCEED then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info }
  ColumnsInfo.Clear;
  DBLibColumnCount := FPlainDriver.dbnumcols(FHandle);
  SetLength(DBLibColTypeCache, DBLibColumnCount);
  for i := 1 to DBLibColumnCount do
  begin
    ColumnInfo := TZColumnInfo.Create;
    if FDBLibConnection.FreeTDS then
    begin
      tdsColInfo.SizeOfStruct := SizeOf(TTDSDBCOL);
      FillChar(tdsColInfo.Name[0], tdsColInfo.SizeOfStruct- SizeOf(DBInt), #0);
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @tdsColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      {$IFDEF UNICODE}
      ColumnInfo.ColumnName := PRawToUnicode(@tdsColInfo.Name[0],
        ZFastCode.StrLen(tdsColInfo.Name), ConSettings^.ClientCodePage^.CP);
      {$ELSE}
      ColumnInfo.ColumnName := ConSettings^.ConvFuncs.ZRawToString(tdsColInfo.Name,
        ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
      {$ENDIF}
      if tdsColInfo.ActualName[0] = #0 then
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else
      {$IFDEF UNICODE}
        ColumnInfo.ColumnLabel := PRawToUnicode(@tdsColInfo.ActualName[0],
          ZFastCode.StrLen(tdsColInfo.ActualName), ConSettings^.ClientCodePage^.CP);
      {$ELSE}
        ColumnInfo.ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(tdsColInfo.ActualName,
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
      {$ENDIF}
      if tdsColInfo.TableName[0] <> #0 then
      {$IFDEF UNICODE}
        ColumnInfo.TableName := PRawToUnicode(@tdsColInfo.TableName[0],
          ZFastCode.StrLen(tdsColInfo.TableName), ConSettings^.ClientCodePage^.CP);
      {$ELSE}
        ColumnInfo.TableName := ConSettings^.ConvFuncs.ZRawToString(tdsColInfo.TableName,
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
      {$ENDIF}
      AssignGenericColumnInfoFromZDBCOL(I-1, tdsColInfo.ColInfo);
    end
    else
      if FDBLibConnection.GetProvider = dpMsSQL then
      begin
        ColInfo.SizeOfStruct := SizeOf(DBCOL); //before execute dbcolinfo we need to set the record size -> 122 Byte or we fail
        if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @ColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
          goto AssignGeneric;
        {$IFDEF UNICODE}
        ColumnInfo.ColumnName := PRawToUnicode(@ColInfo.Name[0],
          ZFastCode.StrLen(ColInfo.Name), ConSettings^.ClientCodePage^.CP);
        {$ELSE}
        ColumnInfo.ColumnName := ConSettings^.ConvFuncs.ZRawToString(ColInfo.Name,
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        {$ENDIF}
        if ColInfo.ActualName[0] = #0 then
          ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
        else
        {$IFDEF UNICODE}
          ColumnInfo.ColumnLabel := PRawToUnicode(@ColInfo.ActualName[0],
            ZFastCode.StrLen(ColInfo.ActualName), ConSettings^.ClientCodePage^.CP);
        {$ELSE}
          ColumnInfo.ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(ColInfo.ActualName,
            ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        {$ENDIF}
        if (ColInfo.TableName[0] <> #0) then
        {$IFDEF UNICODE}
          ColumnInfo.TableName := PRawToUnicode(@ColInfo.TableName[0],
            ZFastCode.StrLen(ColInfo.TableName), ConSettings^.ClientCodePage^.CP);
        {$ELSE}
          ColumnInfo.TableName := ConSettings^.ConvFuncs.ZRawToString(ColInfo.TableName,
            ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        {$ENDIF}
        AssignGenericColumnInfoFromZDBCOL(I-1, ColInfo.ColInfo);
      end
      else
      begin
AssignGeneric:  {this is the old way we did determine the ColumnInformations}
        ColumnInfo.ColumnName := ConSettings^.ConvFuncs.ZRawToString(FPlainDriver.dbColSource(FHandle, I),
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        ColumnInfo.ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(FPlainDriver.dbColName(FHandle, I),
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        DBLibColTypeCache[I-1] := TTDSType(FPlainDriver.dbColtype(FHandle, I));
        ColumnInfo.ColumnType := ConvertTDSTypeToSqlType(DBLibColTypeCache[I-1], ConSettings.CPType);
        ColumnInfo.Currency := DBLibColTypeCache[I-1] in [tdsMoney, tdsMoney4, tdsMoneyN];
        ColumnInfo.Precision := FPlainDriver.dbCollen(FHandle, I);
        ColumnInfo.Scale := 0;
        ColumnInfo.Signed := not (DBLibColTypeCache[I-1] = tdsInt1);
      end;
    if (ColumnInfo.ColumnType in [stString, stUnicodeString]) then begin
      if (ConSettings.ClientCodepage^.IsStringFieldCPConsistent) then
        ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP
      else if (FUserEncoding = ceUTF8) then
        ColumnInfo.ColumnCodePage := zCP_UTF8;
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
procedure TZDBLibResultSet.Close;
begin
{ TODO -ofjanos -cGeneral : Maybe it needs a dbcanquery here. }
//  if Assigned(FHandle) then
//    if not FPlainDriver.dbDead(FHandle) then
//      if FPlainDriver.dbCanQuery(FHandle) <> DBSUCCEED then
//        FDBLibConnection.CheckDBLibError(lcDisconnect, 'CLOSE QUERY');
  FHandle := nil;
  inherited Close;
end;

{**
  Checks if the columnindex is in the proper range.
  An exception is generated if somthing is not ok.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZDBLibResultSet.CheckColumnIndex(ColumnIndex: Integer);
begin
  if (ColumnIndex > DBLibColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) or
     (ColumnIndex < FirstDbcIndex) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZDBLibResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FPlainDriver.dbData(FHandle, ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}) = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the string in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  DT: TTDSType;
  label Convert{, DecLenByTrailingSpaces, AssignFromFRawTemp};
begin
  Len := 0;
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  ColumnIndex := ColumnIndex +1;
  {$ENDIF}
  Result := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Result = nil;
  if not LastWasNull then
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      Len := NativeUInt(FPlainDriver.dbDatLen(FHandle, ColumnIndex));
      while (Len > 0) and ((Result+Len -1)^ = ' ') do Dec(Len);
      if TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage = zCP_NONE then
        case ZDetectUTF8Encoding(Result, Len) of
          etUTF8: TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage := zCP_UTF8;
          etAnsi: TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          else
            ;
        end;
    end
    else
    if DT = tdsUnique then
    begin
      FRawTemp := GUIDToRaw(Result, 16);
      Result := Pointer(FRawTemp);
      Len := 38;
    end
    else
    if (DT = tdsImage) then
      Len := NativeUInt(FPlainDriver.dbDatLen(FHandle, ColumnIndex))
    else
    begin
      Convert:
      SetLength(FRawTemp, 4001);
      Len := FPlainDriver.dbconvert(FHandle, Ord(DT), Pointer(Result),
        Len, Ord(tdsChar), Pointer(FRawTemp), 4001);
      while (Len > 0) and ((Result+Len -1)^ = ' ') do Dec(Len);
      Result := Pointer(FRawTemp);
    end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$WARNINGS OFF} //result might not be undefined
function TZDBLibResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var
  DT: TTDSType;
  Tmp: RawByteString;
  P: PAnsiChar;
  Len: LengthInt;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  Len := FPlainDriver.dbDatLen(FHandle, ColumnIndex+1); //hint DBLib isn't #0 terminated @all
  P := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex+1));
  {$ELSE}
  Len := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  P := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := P = nil;
  if LastWasNull then
    Result := ''
  else
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      while (Len > 0) and ((P+Len -1)^ = ' ') do Dec(Len);
      if Len = 0 then
        Result := ''
      else
      {TDS protocol issue: we dont't know if UTF8(NCHAR) or ANSI(CHAR) fields are coming in: no idea about encoding..
       So selt's test encoding until we know it -----> bad for ASCII7 only }
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stAsciiStream, stUnicodeStream:  //DBlib doesn't convert NTEXT so in all cases we've ansi-Encoding
            Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP);
          else//stString, stUnicodeString:
            if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage = zCP_NONE then
              case ZDetectUTF8Encoding(P, Len) of
                etUTF8:
                  begin
                    TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := zCP_UTF8;
                    Result := PRawToUnicode(P, Len, zCP_UTF8);
                  end;
                etAnsi:
                  begin
                    TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
                    Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP);
                  end;
                else //ASCII7
                  Result := USASCII7ToUnicodeString(P, Len);
              end
            else
              Result := PRawToUnicode(P, Len, TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage)
        end
    end else
    if DT = tdsUnique then
      Result := GUIDToUnicode(P, 16)
    else if (DT = tdsImage) then
      ZSetString(P, Len, Result)
    else
    begin
      SetLength(Tmp, 4001);
      Len := FPlainDriver.dbconvert(FHandle, Ord(DT), Pointer(P), Len,
        Ord(tdsChar), Pointer(tmp), 4001);
      while (Len > 0) and (tmp[Len] = ' ') do Dec(Len);
      Result := USASCII7ToUnicodeString(Pointer(tmp), Len);
    end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETSTRING');
end;
{$WARNINGS ON} //result might not be undefined

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex+1); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex+1));
  {$ELSE}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;
  if LastWasNull then
    Result := ''
  else
  begin
    Result := '';
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      while (DL > 0) and ({%H-}PAnsiChar({%H-}NativeUint(Data) + NativeUint(DL - 1))^ = ' ') do
              Dec(DL);
      if DL > 0 then
      begin
        SetLength(Result, DL);
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Data^, Pointer(Result)^, DL);
      end;
    end else
    if DT = tdsUnique then
      FRawTemp := GUIDToRaw(Data, 16)
    else
    if (DT = tdsImage) then
      ZSetString(Data, DL, Result)
    else
    begin
      SetLength(Result, 4001);
      DL := FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL,
        Ord(tdsChar), Pointer(Result), Length(Result));
      while (DL > 0) and (Result[DL] = ' ') do
          Dec(DL);
      SetLength(Result, DL);
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETSTRING');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZDBLibResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := False;
  if Data <> nil then
  begin
    if DT = tdsBit then
      Result := System.PBoolean(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsBit),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBOOLEAN');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetByte(ColumnIndex: Integer): Byte;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));

  LastWasNull := Data = nil;
  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsInt1 then
      Result := PByte(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt1),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTE');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));

  LastWasNull := Data = nil;
  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsInt2 then
      Result := PSmallInt(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt2),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GetSmall');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
    if DT = tdsInt4 then
      Result := PLongint(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt4),
        @Result, SizeOf(Result));
  FDBLibConnection.CheckDBLibError(lcOther, 'GETINT');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
    if DT = tdsInt8 then //sybase only
      Result := PInt64(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt8),
        @Result, SizeOf(Int64));
  FDBLibConnection.CheckDBLibError(lcOther, 'GETLONG');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsFlt4 then
      Result := PSingle(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsFlt4),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETFLOAT');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsFlt8 then
      Result := PDouble(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsFlt8),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETDOUBLE');
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
function TZDBLibResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := GetDouble(ColumnIndex);
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
function TZDBLibResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  DL: Integer;
  Data: Pointer;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex+1); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex+1));
  {$ELSE}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  {$ENDIF}
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTES');
  LastWasNull := Data = nil;

  Result := BufferToBytes(Data, DL);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := System.Int(GetTimestamp(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := Frac(GetTimestamp(ColumnIndex));
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
function TZDBLibResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
  TempDate: DBDATETIME;
  tdsTempDate: TTDSDBDATETIME;
  Failed: Boolean;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
  begin
    //Perfect conversion no need to crack and reencode the date.
    if DT = tdsDateTime then
      if FDBLibConnection.FreeTDS then //type diff
        Result := PTDSDBDATETIME(Data)^.dtdays + 2 + (PTDSDBDATETIME(Data)^.dttime / 25920000)
      else
        Result := PDBDATETIME(Data)^.dtdays + 2 + (PDBDATETIME(Data)^.dttime / 25920000)
    else if (DT in [tdsNText, tdsNVarChar, tdsText, tdsVarchar, tdsChar]) then
      if (PAnsiChar(Data)+2)^ = ':' then
        Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed{%H-})
      else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Word(DL)) <= 4 then
          Result := RawSQLTimeStampToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
      else
        Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
    else
      if FDBLibConnection.FreeTDS then //type diff
      begin
        FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsDateTime),
          @tdsTempDate, SizeOf(tdsTempDate));
        Result := tdsTempDate.dtdays + 2 + (tdsTempDate.dttime / 25920000);
      end
      else
      begin
        FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsDateTime),
          @TempDate, SizeOf(TempDate));
        Result := TempDate.dtdays + 2 + (TempDate.dttime / 25920000);
      end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETTIMESTAMP');
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZDBLibResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  DL: Integer;
  Data: Pointer;
  TempAnsi: RawByteString;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex+1); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex+1));
  {$ELSE}
  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex); //hint DBLib isn't #0 terminated @all
  Data := Pointer(FPlainDriver.dbdata(FHandle, ColumnIndex));
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := Data = nil;

  Result := nil;
  if not LastWasNull then
    case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
      stBytes, stBinaryStream:
        Result := TZAbstractBlob.CreateWithData(Data, DL);
      stAsciiStream, stUnicodeStream:
        begin
          if (DL = 1) and (PAnsiChar(Data)^ = ' ') then DL := 0; //improve empty lobs, where len = 1 but string should be ''
          Result := TZAbstractClob.CreateWithData(Data, DL,
            FDBLibConnection.GetServerAnsiCodePage, ConSettings);
        end;
      stString, stUnicodeString:
        if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage = zCP_NONE then
          case ZDetectUTF8Encoding(Data, DL) of
            etUTF8:
              begin
                TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := zCP_UTF8;
                Result := TZAbstractClob.CreateWithData(Data, DL, zCP_UTF8, ConSettings);
              end;
            etAnsi:
              begin
                TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
                Result := TZAbstractClob.CreateWithData(Data, DL, ConSettings^.ClientCodePage^.CP, ConSettings);
              end;
            else //ASCII7
              Result := TZAbstractClob.CreateWithData(Data, DL, zCP_us_ascii, ConSettings);
          end
        else
          Result := TZAbstractClob.CreateWithData(Data, DL,
            TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, ConSettings);
      else
      begin
        TempAnsi := InternalGetString(ColumnIndex);
        Result := TZAbstractClob.CreateWithData(PAnsiChar(TempAnsi),
          Length(TempAnsi), ConSettings^.ClientCodePage^.CP, ConSettings);
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
function TZDBLibResultSet.Next: Boolean;
begin
  Result := False;
  if FCheckDBDead then
    if FPlainDriver.dbDead(FHandle) then
      Exit;
//!!! maybe an error message other than dbconnection is dead should be raised
  case FPlainDriver.dbnextrow(FHandle) of
    REG_ROW: Result := True;
    NO_MORE_ROWS: ;
    DBFAIL: FDBLibConnection.CheckDBLibError(lcOther, 'NEXT');
    BUF_FULL: ;//should not happen because we are not using dblibc buffering.
  else
   // If a compute row is read, the computeid of the row is returned
    Result := False;
  end;
end;


{ TZDBLibCachedResolver }

{**
  Creates a DBLib specific cached resolver object.
  @param PlainDriver a native DBLib plain driver.
  @param Handle a DBLib specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZDBLibCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := -1;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZDBLibCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  I: Integer;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  { Defines an index of autoincrement field. }
  if FAutoColumnIndex = -1 then
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if Metadata.IsAutoIncrement(I) then
      begin
        FAutoColumnIndex := I;
        Break;
      end;

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Statement := Connection.CreateStatement;
    ResultSet := Statement.ExecuteQuery('SELECT @@IDENTITY');
    try
      if ResultSet.Next then
        NewRowAccessor.SetLong(FAutoColumnIndex, ResultSet.GetLong(FirstDbcIndex));
    finally
      ResultSet.Close;
      Statement.Close;
    end;
  end;
end;

end.

