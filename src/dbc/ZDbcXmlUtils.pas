unit ZDbcXmlUtils;

{$I ZDbc.inc}

interface

uses SysUtils, Classes, ZDbcIntfs;

function ZXmlEncodeResultSet(const RS: IZResultSet; const MaxRows: LongWord = 0; const UpdateCount: Integer = 0): String;
function ZXmlEncodeResultSetMetaData(const MD: IZResultSetMetadata): String;
function ZXmlEncodeResultSetRows(const RS: IZResultSet; const MaxRows: Integer): String;
function XMLEncode(Input: String): String;

var
  ZXmlProxyFormatSettings: TFormatSettings;

implementation

uses typinfo, FMTBCD, {$IFDEF WITH_TBYTES}ZBase64,{$ENDIF} ZExceptions, ZCompatibility;

{$IFNDEF FPC}
const
  LineEnding = sLineBreak;
{$ENDIF}


function ZXmlEncodeResultSet(const RS: IZResultSet; const MaxRows: LongWord = 0; const UpdateCount: Integer = 0): String;
var
  MD: IZResultSetMetadata;
begin
  MD := RS.GetMetadata;
  Result := '<resultset updatecount="' + IntToStr(UpdateCount) + '">' + LineEnding
          + ZXmlEncodeResultSetMetaData(MD) + LineEnding
          + ZXmlEncodeResultSetRows(RS, MaxRows) + LineEnding
          + '</resultset>';
end;

{--------------- metadata encoding --------------------------------------------}

function ZXmlEncodeResultSetMetaData(const MD: IZResultSetMetadata): String;
const
  MetadataStart = '<metadata>';
  MetadataEnd = '</metadata>';
  ColumnStart = '<column';
  ColumnEnd = ' />';
var
  x: Integer;
  Line: String;

  procedure addProperty(const Name, Value: String); overload;
  begin
    Line := Line + ' ' + Name + '="' + Value +'"';
  end;

  procedure addProperty(const Name: String; Const Value: Integer); overload;
  begin
    Line := Line + ' ' + Name + '="' + IntToStr(Value) +'"';
  end;

  procedure addProperty(const Name: String; const Value: TZSQLType); overload;
  var
    TypeName: String;
  begin
    TypeName := GetEnumName(TypeInfo({$IFDEF FPC}Value{$ELSE}TZSQLType{$ENDIF}), Ord(Value));
    Line := Line + ' ' + Name + '="' + TypeName +'"';
  end;

  procedure addProperty(const Name: String; const Value: TZColumnNullableType); overload;
  var
    TypeName: String;
  begin
    TypeName := GetEnumName(TypeInfo({$IFDEF FPC}Value{$ELSE}TZColumnNullableType{$ENDIF}), Ord(Value));
    Line := Line + ' ' + Name + '="' + TypeName +'"';
  end;

  procedure addProperty(const Name: String; const Value: Boolean); overload;
  begin
      Line := Line + ' ' + Name + '="' + BoolToStr(Value, True) + '"';
  end;

begin
  Result := MetadataStart + LineEnding;
  // todo: adapt to the current start and end of Zeos enumerations
  for x := FirstDbcIndex to MD.GetColumnCount - 1 + FirstDbcIndex do begin
    Line := '';
    addProperty('catalogname', MD.GetCatalogName(x));
    addProperty('codepage', IntToStr(MD.GetColumnCodePage(x)));  // is this needed? All data is unicode in the end?
    {$IFNDEF ZEOS73UP}
    addProperty('displaysize', MD.GetColumnDisplaySize(x));
    {$ENDIF}
    addProperty('label', MD.GetColumnLabel(x));
    addProperty('name', MD.GetColumnName(x));
    addProperty('type', MD.GetColumnType(x));
    addProperty('defaultvalue', MD.GetDefaultValue(x));
    addProperty('precision', MD.GetPrecision(x));
    addproperty('scale', MD.GetScale(x));
    addproperty('schemaname', MD.GetSchemaName(x));
    addProperty('tablename', MD.GetTableName(x));
    addproperty('hasdefaultvalue', MD.HasDefaultValue(x));
    addproperty('isautoincrement', MD.IsAutoIncrement(x));
    addProperty('iscasesensitive', MD.IsCaseSensitive(x));
    addProperty('iscurrency', MD.IsCurrency(x));
    addProperty('isdefinitlywritable', MD.IsDefinitelyWritable(x));
    addProperty('isnullable', MD.IsNullable(x));
    addProperty('isreadonly', MD.IsReadOnly(x));
    addProperty('issearchable', MD.IsSearchable(x));
    addproperty('issigned', MD.IsSigned(x));
    addProperty('iswritable', MD.IsWritable(x));
    Result := Result + ColumnStart + Line + ColumnEnd + LineEnding;
  end;
  Result := Result + MetadataEnd;
end;

{------------------ record set encoding ---------------------------------------}

function ConvertBool(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + BoolToStr(RS.GetBoolean(Idx), True) + '" />';
end;

function ConvertInt(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + IntToStr(RS.GetInt(Idx)) + '" />';
end;

function ConvertInt64(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + IntToStr(RS.GetLong(Idx)) + '" />';
end;

function ConvertSingle(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetFloat(Idx), ZXmlProxyFormatSettings) + '" />';
end;

function ConvertDouble(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetDouble(Idx), ZXmlProxyFormatSettings) + '" />';
end;

function ConvertCurrency(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + CurrToStr(RS.GetCurrency(Idx), ZXmlProxyFormatSettings) + '" />';
end;

{$IFNDEF ZEOS73UP}
function ConvertExtended(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetBigDecimal(Idx), ZXmlProxyFormatSettings) + '" />';
end;
{$ELSE}
function ConvertBcd(const RS: IZResultSet; Const Idx: Integer): String;
var
  BCD: TBCD;
begin
  RS.GetBigDecimal(Idx, BCD);
  {$IFDEF BCDTOSTR_WITH_FORMATSETTINGS}
  Result := '<field value="' + BCDToStr(BCD, ZXmlProxyFormatSettings) + '" />';
  {$ELSE}
  Result := ConvertDouble(RS, Idx);
  {$ENDIF}
end;
{$ENDIF}

function ConvertString(const RS: IZResultSet; Const Idx: Integer): String;
var
  Value: ZWideString;
begin
  Value := RS.GetUnicodeString(Idx);
  Result := '<field value="' + XMLEncode(Utf8Encode(Value)) + '" />';
end;

function ConvertBinaryStream(const RS: IZResultSet; Const Idx: Integer): String;
(*
var
  DbStream: TStream;
  EncodingStream: TBase64EncodingStream;
  StringStream: TStringStream;
*)
begin
(*
  StringStream := TStringStream.Create('');
  try
    EncodingStream := TBase64EncodingStream.Create(StringStream);
    try
      DbStream := RS.GetBinaryStream(Idx);
      try
        EncodingStream.CopyFrom(DbStream, DbStream.Size);
      finally
        FreeAndNil(DbStream);
      end;
      EncodingStream.Flush;
    finally
      FreeAndNil(EncodingStream);
    end;
    Result := '<field value="' + XMLEncode(StringStream.DataString) + '" />';
  finally
    FreeAndNil(StringStream);
  end;
*)
  {$IFDEF WITH_TBYTES}
  Result := '<field value="' + ZEncodeBase64(RS.GetBytes(Idx)) + '" />';
  {$ELSE}
  raise EZSQLException.Create('Encoding Binary is not supported with this compiler.');
  {$ENDIF}
end;

function ConvertBytes(const RS: IZResultSet; Const Idx: Integer): String;
(*
var
  DbValue: TBytes;
  EncodingStream: TBase64EncodingStream;
  StringStream: TStringStream;
*)
begin
(*
  StringStream := TStringStream.Create('');
  try
    EncodingStream := TBase64EncodingStream.Create(StringStream);
    try
      DbValue := RS.GetBytes(Idx);
      EncodingStream.Write(DbValue[0], Length(DbValue));
      EncodingStream.Flush;
    finally
      FreeAndNil(EncodingStream);
    end;
    Result := '<field value="' + XMLEncode(StringStream.DataString) + '" />';
  finally
    FreeAndNil(StringStream);
  end;
*)
  {$IFDEF WITH_TBYTES}
  Result := '<field value="' + ZEncodeBase64(RS.GetBytes(Idx)) + '" />';
  {$ELSE}
  raise EZSQLException.Create('Encoding Binary is not supported with this compiler.');
  {$ENDIF}
end;

function ConvertDate(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + DateToStr(RS.GetDate(Idx), ZXmlProxyFormatSettings) + '" />';
end;

function ConvertTime(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + TimeToStr(RS.GetTime(Idx), ZXmlProxyFormatSettings) + '" />';
end;

function ConvertDateTime(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + DateTimeToStr(RS.GetTimestamp(Idx), ZXmlProxyFormatSettings) + '" />';
end;

function ConvertNull: String;
begin
  Result := '<field isnull="True" />';
end;

function ZXmlEncodeResultSetRows(const RS: IZResultSet; const MaxRows: Integer): String;
type
  TRSConversionProc = function(const RS: IZResultSet; Const Idx: Integer): String;
var
  Idx: Integer;
  CF: Array of TRSConversionProc;
  Line: String;
  MD: IZResultSetMetadata;
  Rows: TStringList;
begin
  if RS.GetType <> rtForwardOnly then
    RS.MoveAbsolute(0);
  if not RS.IsAfterLast then begin
    MD := RS.GetMetadata;
    SetLength(CF, MD.GetColumnCount);
    for Idx := FirstDbcIndex to MD.GetColumnCount - 1 + FirstDbcIndex do begin
      case MD.GetColumnType(Idx) of
        stBoolean: CF[Idx - FirstDbcIndex] := ConvertBool;
        stByte, stShort, stWord, stSmall, stLongWord, stInteger: CF[Idx - FirstDbcIndex] := ConvertInt;
        stULong, stLong: CF[Idx - FirstDbcIndex] := ConvertInt64;
        stFloat: CF[Idx - FirstDbcIndex] := ConvertSingle;
        stDouble: CF[Idx - FirstDbcIndex] := ConvertDouble;
        stCurrency: CF[Idx - FirstDbcIndex] := ConvertCurrency;
        stBigDecimal: CF[Idx - FirstDbcIndex] := {$IFNDEF ZEOS73UP}ConvertExtended{$ELSE}ConvertBcd{$ENDIF};
        stString, stUnicodeString: CF[Idx - FirstDbcIndex] := ConvertString;
        stDate: CF[Idx - FirstDbcIndex] := ConvertDate;
        stTime: CF[Idx - FirstDbcIndex] := ConvertTime;
        stTimestamp: CF[Idx - FirstDbcIndex] := ConvertDateTime;
        stAsciiStream, stUnicodeStream: CF[Idx - FirstDbcIndex] := ConvertString;
        stBinaryStream: CF[Idx - FirstDbcIndex] := ConvertBinaryStream;
        stBytes: CF[Idx-FirstDbcIndex] := ConvertBytes;
        else raise EZSQLException.Create('Conversion of type ' + MD.GetColumnTypeName(Idx) + ' is not supported (yet).');
      end;
    end;

    Rows := TStringList.Create;
    try
      while RS.Next do begin
        if (MaxRows <> 0) then
          if (Rows.Count >= MaxRows) then Break;
        Line := '<row>';
        for idx := FirstDbcIndex to MD.GetColumnCount - 1 + FirstDbcIndex do begin
          if RS.IsNull(idx) then
            Line := Line + ConvertNull
          else
            Line := Line + CF[Idx - FirstDbcIndex](RS, Idx);
        end;
        Rows.Add(Line + '</row>');
      end;
      Line := Rows.Text;
    finally
      FreeAndNil(Rows);
    end;
    Result := '<rows>' + LineEnding + Line + '</rows>';
  end;
end;

function XMLEncode(Input: String): String;
var
  x: Integer;
  Position: Integer;

  procedure CutAndInsert(Replacement: String);
  begin
    if Position < x then Result := Result + Copy(Input, Position, x - Position);
    Result := Result + Replacement;
    Position := x + 1;
  end;
begin
  Position := 1;
  Result := '';
  for x := 1 to Length(Input) do begin
    case Input[x] of
      #00..#31, '%': CutAndInsert('&#' + IntToStr(Ord(Input[x])) + ';');
      '<': CutAndInsert('&lt;');
      '>': CutAndInsert('&gt;');
      '&': CutAndInsert('&amp;');
      '''': CutAndInsert('&apos;');
      '"': CutAndInsert('&quot;');
    end;
  end;
  if Position <= Length(Input) then Result := Result + Copy(Input, Position, Length(Input));
end;

initialization
  ZXmlProxyFormatSettings.DateSeparator := '-';
  ZXmlProxyFormatSettings.LongDateFormat := 'YYYY/MM/DD';
  ZXmlProxyFormatSettings.ShortDateFormat := 'YYYY/MM/DD';
  ZXmlProxyFormatSettings.LongTimeFormat := 'HH:NN:SS.ZZZ';
  ZXmlProxyFormatSettings.ShortTimeFormat := 'HH:NN:SS.ZZZ';
  ZXmlProxyFormatSettings.DecimalSeparator := '.';
  ZXmlProxyFormatSettings.TimeSeparator := ':';
  ZXmlProxyFormatSettings.ThousandSeparator := ',';

end.
