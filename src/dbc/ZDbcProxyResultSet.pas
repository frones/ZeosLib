{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcProxyResultSet;

interface

{$I ZDbc.inc}

{$IFDEF ENABLE_PROXY} //if set we have an empty unit
uses
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$IFNDEF NO_UNIT_CONTNRS}, Contnrs{$ENDIF}{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZPlainProxyDriverIntf, ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcLogging,{$IFDEF ZEOS73UP}FmtBCD, ZVariant, {$ENDIF}
  ZDbcResultSetMetadata, ZCompatibility, {$IFDEF FPC}ZXmlCompat{$ELSE} XmlDoc, XmlIntf{$ENDIF};

type
  {** Implements DBC Layer Proxy ResultSet. }
  TZDbcProxyResultSet = class({$IFDEF ZEOS73UP}TZAbstractReadOnlyResultSet, IZResultSet{$ELSE}TZAbstractResultSet{$ENDIF})
  private
    FXmlDocument: IXMLDocument;
    FCurrentRowNode: IXMLNode;
    FResultSetNode: IXMLNode;
    FMetadataNode: IXmlNode;
    FRowsNode: IXMLNode;
    FFormatSettings: TFormatSettings;
  protected
    {$IFNDEF NEXTGEN}
    FAnsiBuffer: AnsiString;
    {$ENDIF}
    FWideBuffer: ZWideString;
    FStringBuffer: String;
    {$IFNDEF ZEOS73UP}
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    {$ENDIF}
    /// <summary>
    ///  Opens this recordset.
    /// </summary>
    procedure Open; override;
  public
    /// <summary>
    ///  Constructs this object, assignes main properties and
    ///  opens the record set.
    /// </summary>
    /// <param name="Connection">
    ///  The DBC Proxy connection interface that returned the result set data.
    /// </param>
    /// <param name="SQL">
    ///  The SQL String that generated the result set.
    /// </param>
    /// <param name="ResultStr">
    ///  A string containing the XML exncoded result set.
    /// </param>
    constructor Create(const Connection: IZConnection; const SQL: string; const ResultStr: WideString);
    /// <summary>
    ///  Indicates if the value of the designated column in the current row
    ///  of this ResultSet object is Null.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  if the value is SQL NULL, the
    ///  value returned is true. false otherwise.
    /// </returns>
    function IsNull(ColumnIndex: Integer): Boolean; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row
    ///  of this ResultSet object as
    ///  a PChar in the Delphi programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value; if the value is SQL NULL, the
    ///  value returned is null
    /// </returns>
    {$IFNDEF ZEOS73UP}
    function GetPChar(ColumnIndex: Integer): PChar; override;
    {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>PAnsiChar</c> in the Delphi
    ///   programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    {$IFNDEF ZEOS73UP}
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>PAnsiChar</c> in the Delphi
    ///   programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <param name="Len">
    ///  the Length of the PAnsiChar String
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; {$IFNDEF ZEOS73UP} override; {$ELSE} overload;{$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>PWideChar</c>.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>
    ///  Also <c>LastWasNull</c> is set accordingly.
    /// </returns>
    {$IFNDEF ZEOS73UP}
    function GetPWideChar(ColumnIndex: Integer): PWidechar; override;
    {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a PWideChar.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <param name="Len">
    ///  the Length of the PWideChar String in Words.
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>.
    ///  Also <c>LastWasNull</c> is set accordingly.
    /// </returns>
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; {$IFNDEF ZEOS73UP} override; {$ELSE} overload;{$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>String</c>.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetString(ColumnIndex: Integer): String; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as an <c>AnsiString</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetAnsiString(ColumnIndex: Integer): AnsiString; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>UTF8String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetUTF8String(ColumnIndex: Integer): UTF8String; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>UTF8String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetRawByteString(ColumnIndex: Integer): RawByteString; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetBinaryString(ColumnIndex: Integer): RawByteString; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this <c>ResultSet</c> object as a <c>UnicodeString</c> in the Delphi programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    /// <summary>
    /// Gets the value of the designated column in the current row of this
    /// <c>ResultSet</c> object as a <c>Boolean</c>.
    /// </summary>
    /// <param name="ColumnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>false</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetBoolean(ColumnIndex: Integer): Boolean; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetInt(ColumnIndex: Integer): Integer; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetLong(ColumnIndex: Integer): Int64; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetULong(ColumnIndex: Integer): UInt64; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetFloat(ColumnIndex: Integer): Single; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetDouble(ColumnIndex: Integer): Double; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    function GetCurrency(ColumnIndex: Integer): Currency; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    {$IFNDEF ZEOS73UP}
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    {$ELSE}
    function GetBigDecimal(ColumnIndex: Integer): TBCD; overload;
    {$ENDIF}
    function GetBytes(ColumnIndex: Integer): TBytes; {$IFNDEF ZEOS73UP}override{$ELSE}overload{$ENDIF};
    function GetDate(ColumnIndex: Integer): TDateTime; {$IFNDEF ZEOS73UP}override{$ELSE}overload{$ENDIF};
    function GetTime(ColumnIndex: Integer): TDateTime; {$IFNDEF ZEOS73UP}override{$ELSE}overload{$ENDIF};
    function GetTimestamp(ColumnIndex: Integer): TDateTime; {$IFNDEF ZEOS73UP}override{$ELSE}overload{$ENDIF};
    {$IFNDEF ZEOS73UP}
    function GetBlob(ColumnIndex: Integer): IZBlob; {$IFNDEF ZEOS73UP} override; {$ENDIF}
    {$ELSE}
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$ENDIF}

    {$IFDEF ZEOS73UP}
    function GetUInt(ColumnIndex: Integer): Cardinal;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD); overload;
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;
    {$ENDIF ZEOS73UP}
    function MoveAbsolute(Row: Integer): Boolean; override;
    /// <summary>
    /// Gets the number of updated rows in the database.
    /// </summary>
    /// <returns>
    ///  The number of rows that were updated during the execution of the query.
    /// </returns>
    function GetUpdateCount: Integer;
  end;

  TZDbcProxyResultSetMetadata = Class(TZAbstractResultSetMetadata)
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
  End;

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
implementation
{$IFDEF ENABLE_PROXY} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math,
  ZMessages, ZEncoding, ZFastCode, ZDbcMetadata, ZClasses,
  TypInfo, Variants, ZBase64, ZExceptions {$IFNDEF FPC},xmldom{$ENDIF} {$IFDEF WITH_OMNIXML}, Xml.omnixmldom{$ENDIF};

const
  ValueAttr = 'value';

function BoolToInt(Value: Boolean): Integer;
begin
  if Value then Result := 1 else Result := 0;
end;

constructor TZDbcProxyResultSetMetadata.Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
begin
  inherited;
  Loaded := true;
end;

{ TZDbcProxyResultSet }

constructor TZDbcProxyResultSet.Create(const Connection: IZConnection; const SQL: string; const ResultStr: WideString);
var
  Stream: TStream;
  ConSettings: PZConSettings;
  Metadata: IZDatabaseMetadata;
  xmldoc: {$IFDEF FPC}TZXMLDocument{$ELSE}TXMLDocument{$ENDIF};

  {$IFNDEF FPC}DomVendor: TDOMVendor;{$ENDIF}

  procedure addBomToSTream;
  const
    {$IFDEF FPC}
    BOM: AnsiString = #$FF#$FE;
    {$ELSE}
    BOM: WideString = #$FEFF;
    {$ENDIF}
  begin
    Stream.Write(BOM[1], 2);
  end;

begin
  ConSettings := Connection.GetConSettings;
  Metadata := Connection.GetMetadata;

  inherited Create(Statement, SQL,
    TZDbcProxyResultSetMetadata.Create(Metadata, SQL, Self), ConSettings);
  {$IFDEF FPC}
  xmldoc := TZXmlDocument.Create;
  {$ELSE}
  xmldoc := TXMLDocument.Create(nil);
  // OmiXml preserves the Carriage Return in Strings -> This solves a problem
  // where CRLF gets converted to LF wit MSXML
  DomVendor := DOMVendors.Find('Omni XML');
  if Assigned(DomVendor) then
    xmldoc.DOMImplementation := DomVendor.DOMImplementation;
  {$ENDIF}
  FXmlDocument := xmldoc as IXMLDocument;

  Stream := TMemoryStream.Create;
  try
    addBomToSTream; // so the XML stuff knows that it is UTF16 encoded.
    Stream.Write(ResultStr[1], Length(ResultStr) * 2);
    Stream.Position := 0;
    FXmlDocument.LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;

  FFormatSettings.DateSeparator := '-';
  FFormatSettings.LongDateFormat := 'YYYY/MM/DD';
  FFormatSettings.ShortDateFormat := 'YYYY/MM/DD';
  FFormatSettings.LongTimeFormat := 'HH:NN:SS.ZZZ';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS.ZZZ';
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.ThousandSeparator := ',';

  ResultSetType := rtScrollInsensitive;

  Open;
end;

procedure TZDbcProxyResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldCount: Integer;
  ColumnNode: IXMLNode;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  FResultSetNode := FXmlDocument.ChildNodes.Get(0);
  FMetadataNode := FResultSetNode.ChildNodes.FindNode('metadata');
  FRowsNode := FResultSetNode.ChildNodes.FindNode('rows');

  if Assigned(FRowsNode) then
    LastRowNo := FRowsNode.ChildNodes.Count
  else
    LastRowNo := 0;

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldCount := FMetadataNode.ChildNodes.Count;

  for I := 0 to FieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    ColumnNode := FMetadataNode.ChildNodes.Get(I);
    with ColumnInfo do
    begin
//      Precision := StrToInt(ColumnNode.Attributes['precision']);
//      {$IFDEF UNICODE}
//      ColumnLabel := PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP);
//      {$ELSE}
//      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
//        ColumnLabel := BufferToStr(P, Precision)
//      else
//        ColumnLabel := ZUnicodeToString(PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
//      {$ENDIF}

//    addProperty('codepage', IntToStr(MD.GetColumnCodePage(x)));  // is this needed? All data is unicode in the end?
//    h�? codepage und hasdefaultvalue gibts nicht am columntype?

      // todo: kl�ren, was hier vonwegen der oben auskommentierten Unicodegeschichten rein mu�...
      CatalogName := ColumnNode.Attributes['catalogname'];
      {$IFNDEF ZEOS73UP}
      ColumnDisplaySize := StrToInt(ColumnNode.Attributes['displaysize']);
      {$ENDIF}
      ColumnLabel := ColumnNode.Attributes['label'];
      ColumnName := ColumnNode.Attributes['name'];
      ColumnType := TZSQLType(GetEnumValue(TypeInfo(TZSQLType), ColumnNode.Attributes['type']));
      case ColumnType of
        stString, stUnicodeString:
          {$IFNDEF ZEOS73UP}if GetConSettings.CPType = cCP_UTF16 then {$ENDIF ZEOS73UP} begin
            ColumnType := stUnicodeString;
            ColumnCodePage := zCP_UTF16;
          {$IFNDEF ZEOS73UP}end else begin
            ColumnType := stString;
            ColumnCodePage := zCP_UTF8;
          {$ENDIF ZEOS73UP}
          end;
        stAsciiStream, stUnicodeStream:
          {$IFNDEF ZEOS73UP}if GetConSettings.CPType = cCP_UTF16 then {$ENDIF ZEOS73UP} begin
            ColumnType := stUnicodeStream;
            ColumnCodePage := zCP_UTF16;
          {$IFNDEF ZEOS73UP}
          end else begin
            ColumnType := stAsciiStream;
            ColumnCodePage := zCP_UTF8
          {$ENDIF ZEOS73UP}
          end;
      end;
      DefaultValue := ColumnNode.Attributes['defaultvalue'];
      Precision := StrToInt(ColumnNode.Attributes['precision']);
      Scale := StrToInt(ColumnNode.Attributes['scale']);
      SchemaName := ColumnNode.Attributes['schemaname'];
      TableName := ColumnNode.Attributes['tablename'];
      AutoIncrement := StrToBool(ColumnNode.Attributes['isautoincrement']);
      CaseSensitive := StrToBool(ColumnNode.Attributes['iscasesensitive']);
      Currency := StrToBool(ColumnNode.Attributes['iscurrency']);
      DefinitelyWritable := StrToBool(ColumnNode.Attributes['isdefinitlywritable']);
      Nullable := TZColumnNullableType(GetEnumValue(TypeInfo(TZColumnNullableType), ColumnNode.Attributes['isnullable']));
      ReadOnly := StrToBool(ColumnNode.Attributes['isreadonly']);
      Searchable := StrToBool(ColumnNode.Attributes['issearchable']);
      Signed := StrToBool(ColumnNode.Attributes['issigned']);
      Writable := StrToBool(ColumnNode.Attributes['iswritable']);
      if ColumnType = stString then
        ColumnType := stUnicodeString;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  FCurrentRowNode := nil;
  RowNo := 0;

  inherited Open;
end;

function TZDbcProxyResultSet.IsNull(ColumnIndex: Integer): Boolean;
var
  ValueNode: IXMLNode;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if not Assigned(FCurrentRowNode) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  ValueNode := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex);
  Result := StrToBoolDef(VarToStrDef(ValueNode.Attributes['isnull'], 'False'), false);
end;

{$IFNDEF ZEOS73UP}
function TZDbcProxyResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
begin
  raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$ENDIF}

{$IFNDEF ZEOS73UP}
function TZDbcProxyResultSet.GetPChar(ColumnIndex: Integer): PChar;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
    FStringBuffer := VarToStrDef(Val, '');
  end;

  if (FStringBuffer = '') or (LastWasNull) then begin
    Result := nil;
  end else begin
    Result := @FStringBuffer[Low(FStringBuffer)];
  end;
end;

function TZDbcProxyResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
end;
{$ENDIF ZEOS73UP}

function TZDbcProxyResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
{$IFNDEF NEXTGEN}
var
  Val: OleVariant;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
    FAnsiBuffer := AnsiString(VarToStrDef(Val, ''));
    Len := Length(FAnsiBuffer);
    if Len = 0
    then Result := PEmptyAnsiString
    else Result := Pointer(FAnsiBuffer);
  end else begin
    Result := nil;
    Len := 0
  end;
{$ELSE}
  raise EZSQLException.Create('GetPAnsiChar is not supported on Nextgen.');
{$ENDIF}
end;

function TZDbcProxyResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
    FWideBuffer := VarToStrDef(Val, '');
    Len := Length(FWideBuffer);
    if Len = 0
    then Result := PEmptyUnicodeString
    else Result := Pointer(FWideBuffer);
  end else begin
    Result := nil;
    Len := 0
  end;
end;

{$IFNDEF ZEOS73UP}
function TZDbcProxyResultSet.GetPWideChar(ColumnIndex: Integer): PWidechar;
var
  Len: NativeUInt;
begin
  Result := GetPWideChar(ColumnIndex, Len);
end;
{$ENDIF ZEOS73UP}

function TZDbcProxyResultSet.GetString(ColumnIndex: Integer): String;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Result := Val;
end;

{$IFNDEF NO_ANSISTRING}
function TZDbcProxyResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Result := AnsiString(Val);
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZDbcProxyResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  Val: OleVariant;
  Val2: ZWideString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Val2 := VarToStr(Val);
  Result := UTF8Encode(Val2);
end;
{$ENDIF}

function TZDbcProxyResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
var
  Val: OleVariant;
  Val2: String;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Val2 := VarToStr(Val);
  Result := UTF8Encode(Val2);
end;

function TZDbcProxyResultSet.GetBinaryString(ColumnIndex: Integer): RawByteString;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Result := RawByteString(VarToStrDef(Val, ''));
end;

function TZDbcProxyResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var
  Val: OleVariant;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Val := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
  Result := Val;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZDbcProxyResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Str: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then begin
    Str := FCurrentRowNode.ChildNodes.Get(ColumnIndex - FirstDbcIndex).Attributes[ValueAttr];
    Result := StrToBool(Str);
  end else begin
    Result := false;
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
function TZDbcProxyResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := StrToInt(Val);
    stLong:
      Result := StrToInt(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := Trunc(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToInt(Val);
    stDate:
      Result := Trunc(StrToDate(Val, FFormatSettings));
    stTime:
      Result := 0;
    stTimestamp:
      Result := Trunc(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcProxyResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt64(Val);
    stULong:
      Result := StrToInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := Trunc(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToInt64(Val);
    stDate:
      Result := Trunc(StrToDate(Val, FFormatSettings));
    stTime:
      Result := 0;
    stTimestamp:
      Result := Trunc(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcProxyResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := UnicodeToUInt64(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := Trunc(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := UnicodeToUInt64(Val);
    stDate:
      Result := Trunc(StrToDate(Val, FFormatSettings));
    stTime:
      Result := 0;
    stTimestamp:
      Result := Trunc(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcProxyResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := UnicodeToUInt64(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := StrToFloat(Val, FFormatSettings);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToFloat(Val, FFormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := StrToDateTime(Val, FFormatSettings);
    else
      Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcProxyResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := UnicodeToUInt64(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := StrToFloat(Val, FFormatSettings);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToFloat(Val, FFormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := StrToDateTime(Val, FFormatSettings);
    else
      Result := 0;
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
{$IFNDEF ZEOS73UP}
function TZDbcProxyResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := StrToUInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := StrToFloat(Val, FFormatSettings);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToFloat(Val, FormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := StrToDateTime(Val, FFormatSettings);
    else
      Result := 0;
  end;
end;
{$ELSE}
function TZDbcProxyResultSet.GetBigDecimal(ColumnIndex: Integer): TBcd;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result :=  IntegerToBcd(0);
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := IntegerToBcd(BoolToInt(StrToBool(Val)));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := IntegerToBcd(StrToInt(Val));
    stULong:
      ScaledOrdinal2Bcd(UnicodeToUInt64(Val), 0, Result, False);
    stLong:
      ScaledOrdinal2Bcd(StrToInt64(Val), 0, Result);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := UniToBcd(Val);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := UniToBcd(Val);
    else
      Result := IntegerToBcd(0);
  end;
end;
{$ENDIF}
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDbcProxyResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
  ColInfo: TZColumnInfo;
begin
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    //Result := '';
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColInfo := TZColumnInfo(ColumnsInfo.Items[Idx]);
  ColType := ColInfo.ColumnType;

  case ColType of
    stBytes, stBinaryStream:
      Result := ZDecodeBase64(Val);
    else begin
      raise EZSQLException.Create('GetBytes is not supported for ' + ColInfo.GetColumnTypeName + ' (yet). Column: ' + ColInfo.ColumnLabel);
    end;
  end;
end;

function TZDbcProxyResultSet.GetCurrency(
  ColumnIndex: Integer): Currency;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stCurrency:
      Result := StrToCurr(Val, FFormatSettings);
    stFloat, stDouble, stBigDecimal:
      Result := StrToFloat(Val, FFormatSettings);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToCurr(Val, FFormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := StrToDateTime(Val, FFormatSettings);
    else
      Result := 0;
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
function TZDbcProxyResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stFloat, stDouble, stBigDecimal, stCurrency:
      Result := Trunc(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToDate(Val, FFormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := 0;
    stTimestamp:
      Result := Trunc(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDbcProxyResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := 0;
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := 0;
    stULong:
      Result := 0;
    stLong:
      Result := 0;
    stFloat, stDouble, stBigDecimal, stCurrency:
      Result := Frac(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToTime(Val, FFormatSettings);
    stDate:
      Result := 0;
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := Frac(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
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
function TZDbcProxyResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := StrToInt(Val);
    stULong:
      Result := UnicodeToUInt64(Val);
    stLong:
      Result := StrToInt64(Val);
    stFloat, stDouble, stBigDecimal, stCurrency:
      Result := StrToFloat(Val, FFormatSettings);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := StrToDateTime(Val, FFormatSettings);
    stDate:
      Result := StrToDate(Val, FFormatSettings);
    stTime:
      Result := StrToTime(Val, FFormatSettings);
    stTimestamp:
      Result := StrToDateTime(Val, FFormatSettings);
    else
      Result := 0;
  end;
end;

{$IFNDEF ZEOS73UP}

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZDbcProxyResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
  AnsiVal: AnsiString;
  Bytes: TBytes;
  ColInfo: TZColumnInfo;
begin
  {$IFNDEF DISABLE_CHECKING}
    CheckColumnConvertion(ColumnIndex, stInteger);
  {$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := nil;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColInfo := TZColumnInfo(ColumnsInfo.Items[Idx]);
  ColType := ColInfo.ColumnType;
  case ColType of
    stBinaryStream, stBytes: begin
      Bytes := DecodeBase64(AnsiString(Val));
      Result := TZAbstractBlob.CreateWithData(@Bytes[0], Length(Bytes)) as IZBlob;
    end;
    stAsciiStream, stUnicodeStream: begin
      if Val <> '' then
         Result := TZAbstractCLob.CreateWithData(@Val[Low(Val)], Length(Val), GetConSettings) as IZBlob
       else
         Result := TZAbstractCLob.CreateWithData(nil, 0, GetConSettings) as IZBlob;
    end;
    else begin
      raise EZSQLException.Create('GetBlob is not supported for ' + ColInfo.GetColumnTypeName + ' (yet). Column: ' + ColInfo.ColumnLabel);
    end;
  end;
end;

{$ELSE}

function TZDbcProxyResultSet.GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
  ValVariant: Variant;
  //AnsiVal: {$IFDEF NEXTGEN}RawByteString{$ELSE}AnsiString{$ENDIF};
  Bytes: TBytes;
  ColInfo: TZColumnInfo;
begin
  if LobStreamMode <> lsmRead then
    raise EZSQLException.Create('No lob stream mode besides lsmRead is supported.');

  {$IFNDEF DISABLE_CHECKING}
    CheckColumnConvertion(ColumnIndex, stInteger);
  {$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := nil;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  ValVariant := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  Val := VarToStr(ValVariant);
  ColInfo := TZColumnInfo(ColumnsInfo.Items[Idx]);
  ColType := ColInfo.ColumnType;
  case ColType of
    stBinaryStream: begin
      if Val = '' then
        Result := TZAbstractBLob.CreateWithData(nil, 0)
      else begin
        {$IFDEF NO_ANSISTRING}
        Bytes := ZDecodeBase64(Val);
        {$ELSE}
        Bytes := ZDecodeBase64(AnsiString(Val));
        {$ENDIF}
        Result := TZAbstractBlob.CreateWithData(@Bytes[0], Length(Bytes)) as IZBlob;
      end;
    end;
    stAsciiStream, stUnicodeStream: begin
      if Val <> '' then
         {$IFDEF WITH_ZEROBASEDSTRINGS}
         Result := TZAbstractCLob.CreateWithData(@Val[Low(Val)], Length(Val), GetConSettings) as IZBlob
         {$ELSE}
         Result := TZAbstractCLob.CreateWithData(@Val[1], Length(Val), GetConSettings) as IZBlob
         {$ENDIF}
       else
         Result := TZAbstractCLob.CreateWithData(nil, 0, GetConSettings) as IZBlob;
    end;
    else begin
      raise EZSQLException.Create('GetBlob is not supported for ' + ColInfo.GetColumnTypeName + ' (yet). Column: ' + ColInfo.ColumnLabel);
    end;
  end;
end;
{$ENDIF}


{$IFDEF ZEOS73UP}
function TZDbcProxyResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;
  case ColType of
    stBoolean:
      Result := BoolToInt(StrToBool(Val));
    stByte, stShort, stWord, stSmall, stLongWord, stInteger:
      Result := UnicodeToUInt32(Val);
    stULong:
      Result := UnicodeToUInt32(Val);
    stLong:
      Result := UnicodeToUInt32(Val);
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := Trunc(StrToFloat(Val, FFormatSettings));
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      Result := UnicodeToUInt32(Val);
    stDate:
      Result := Trunc(StrToDate(Val, FFormatSettings));
    stTime:
      Result := 0;
    stTimestamp:
      Result := Trunc(StrToDateTime(Val, FFormatSettings));
    else
      Result := 0;
  end;
end;

procedure TZDbcProxyResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
begin
  Result := GetBigDecimal(ColumnIndex)
end;

procedure TZDbcProxyResultSet.GetGUID(ColumnIndex: Integer; var Result: TGUID);
var
  ColType: TZSQLType;
  Idx: Integer;
  Val: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
    exit;
  end;

  Idx := ColumnIndex - FirstDbcIndex;
  Val := FCurrentRowNode.ChildNodes.Get(Idx).Attributes[ValueAttr];
  ColType := TZColumnInfo(ColumnsInfo.Items[Idx]).ColumnType;

  case ColType of
    stGUID:
      Result := StringToGUID(Val);
    else
      raise EZUnsupportedException.Create(SUnsupportedOperation);
  end;
end;

function TZDbcProxyResultSet.GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte;
begin
  raise EZSQLException.Create('GetBytes is not supported (yet)');
end;

procedure TZDbcProxyResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
begin
  DecodeDateTimeToDate(GetDate(ColumnIndex), Result);
end;

procedure TZDbcProxyResultSet.GetTime(ColumnIndex: Integer; Var Result: TZTime);
begin
  DecodeDateTimeToTime(GetTime(ColumnIndex), Result);
end;

procedure TZDbcProxyResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
begin
  DecodeDateTimeToTimeStamp(GetTimestamp(ColumnIndex), Result);
end;
{$ENDIF}

function TZDbcProxyResultSet.GetUpdateCount: Integer;
var
  TempStr: String;
begin
  TempStr := FResultSetNode.Attributes['updatecount'];
  Result := StrToIntDef(TempStr, 0);
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
function TZDbcProxyResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  { Checks for maximum row. }
{$IFDEF FPC} // I suppose FPC compiler needs this initial assignment...?
   Result := False;
{$ENDIF}
  { Processes negative rows. }
  if Row < 0 then begin
    Row := LastRowNo + Row + 1;
    if Row < 0 then Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then begin
    if ResultSetType = rtForwardOnly then begin
      while RowNo < Row do begin
        if (RowNo <> 0) and (FRowsNode.ChildNodes.Count > 0) then
          FRowsNode.ChildNodes.Delete(0);
        RowNo := RowNo + 1;
      end;
      if FRowsNode.ChildNodes.Count > 0 then begin
        Result := True;
        FCurrentRowNode := FRowsNode.ChildNodes.Get(0);
      end else begin
        Result := False;
        Row := Min(Row, LastRowNo + 1);
        FCurrentRowNode := nil;
      end;
    end else begin
      if (0 < Row) and (Row <= LastRowNo) then begin
        Result := True;
        FCurrentRowNode := FRowsNode.ChildNodes.Get(Row - 1)
      end else begin
        Result := False;
        Row := Min(Row, LastRowNo + 1);
        FCurrentRowNode := nil;
      end;
    end;
    RowNo := Row;
  end else begin
    raise EZSQLException.Create('This resultset is forward only.');
  end;
end;

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
end.
