{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Connectivity Functions              }
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

unit ZDbcUtils;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Contnrs,
  ZCompatibility, ZDbcIntfs, ZDbcResultSetMetadata, ZTokenizer, ZVariant;

type
  TPreparablePrefixToken = Record
    MatchingGroup: String;
    ChildMatches: TStringDynArray;
  end;
  TPreparablePrefixTokens = array of TPreparablePrefixToken;

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SuupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(Url: string;
  SupportedProtocols: TStringDynArray): string;

{**
  Resolves a database URL and fills the database connection parameters.
  @param Url an initial database URL.
  @param Info an initial info parameters.
  @param HostName a name of the database host.
  @param Port a port number.
  @param Database a database name.
  @param UserName a name of the database user.
  @param Password a user's password.
  @param ResutlInfo a result info parameters.
}
procedure ResolveDatabaseUrl(const Url: string; Info: TStrings;
  var HostName: string; var Port: Integer; var Database: string;
  var UserName: string; var Password: string; ResultInfo: TStrings);

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(Statement: IZStatement; const ParamName: string;
  const Default: string): string;

{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Len then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}
function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;

{**
  Returns a FieldSize in Bytes dependend to the FieldType and CharWidth
  @param <code>TZSQLType</code> the Zeos FieldType
  @param <code>Integer</code> the Current given FieldLength
  @param <code>Integer</code> the Current CountOfByte/Char
  @param <code>Boolean</code> does the Driver returns the FullSizeInBytes
  @returns <code>Integer</code> the count of AnsiChars for Field.Size * SizeOf(Char)
}
function GetFieldSize(const SQLType: TZSQLType;ConSettings: PZConSettings;
  const Precision, CharWidth: Integer; DisplaySize: PInteger = nil;
    SizeInBytes: Boolean = False): Integer;

function WideStringStream(const AString: WideString): TStream;

function TokenizeSQLQueryRaw(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TRawByteStringDynArray;

function TokenizeSQLQueryUni(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TUnicodeStringDynArray;

{$IF defined(ENABLE_MYSQL) or defined(ENABLE_POSTGRESQL) or defined(ENABLE_INTERBASE)}
procedure AssignOutParamValuesFromResultSet(const ResultSet: IZResultSet;
  OutParamValues: TZVariantDynArray; const OutParamCount: Integer;
  const PAramTypes: array of ShortInt);
{$IFEND}

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString; overload;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString; overload;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Buffer the pointer to the Data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

function GetValidatedUnicodeStream(const Ansi: RawByteString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

implementation

uses ZMessages, ZSysUtils, ZEncoding, ZFastCode;

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(Url: string;
  SupportedProtocols: TStringDynArray): string;
var
  I: Integer;
  Protocol: string;
  Index: Integer;
begin
  Result := '';

  Index := FirstDelimiter(':', Url);
  if Index > 0 then
    Protocol := Copy(Url, Index + 1, Length(Url) - Index)
  else
    Protocol := '';
  Index := FirstDelimiter(':', Protocol);
  if Index > 1 then
    Protocol := Copy(Protocol, 1, Index - 1)
  else
    Protocol := '';

  if Protocol = '' then
    raise EZSQLException.Create(Format(SIncorrectConnectionURL, [Url]));

  for I := Low(SupportedProtocols) to High(SupportedProtocols) do
  begin
    if SupportedProtocols[I] = Protocol then
    begin
      Result := Protocol;
      Break;
    end;
  end;

  if Result = '' then
    raise EZSQLException.Create(Format(SUnsupportedProtocol, [Protocol]));
end;

{**
  Resolves a database URL and fills the database connection parameters.
  @param Url an initial database URL.
  @param Info an initial info parameters.
  @param HostName a name of the database host.
  @param Port a port number.
  @param Database a database name.
  @param UserName a name of the database user.
  @param Password a user's password.
  @param ResutlInfo a result info parameters.
}
procedure ResolveDatabaseUrl(const Url: string; Info: TStrings;
  var HostName: string; var Port: Integer; var Database: string;
  var UserName: string; var Password: string; ResultInfo: TStrings);
var
  Temp: string;
begin
   { assign URL first -> define all out out params }
   {A correct builded URL exports all these Params if they are expected!}
  DriverManager.ResolveDatabaseUrl(URL, HostName, Port, DataBase, UserName, Password, ResultInfo);

  { Retrieves non special-escaped-parameters }
  Temp := Url;
  while FirstDelimiter('?', Temp) > 0 do //Get all aditional Parameters
    Temp := Copy(Temp, FirstDelimiter('?', Temp)+1, Length(Temp));
  PutSplitString(ResultInfo, Temp, ';'); //overrides all Strings
  ResultInfo.Text := StringReplace(ResultInfo.Text, #9, ';', [rfReplaceAll]); //unescape the #9 char

  if Assigned(Info) then //isn't that strange? (Shouldn't we pick out double-values?)
    Resultinfo.AddStrings(Info);//All possible PWD/Password and UID/UserName are aviable now, but for what? And the can also be doubled!

  { Redefines user name if not avialble in the URL}
  if UserName = '' then //Priority 1: URL.UserName
  begin
    UserName := ResultInfo.Values['UID']; //Priority 2: Info-UID
    if UserName = '' then
      UserName := ResultInfo.Values['username']; //Priority 3: Info-username
  end;

  { Redefines user password if not avialble in the URL }
  if Password = '' then //Priority 1: URL.Password
  begin
    Password := ResultInfo.Values['PWD']; //Priority 2: Info-PWD
    if Password = '' then
      Password := ResultInfo.Values['password']; //Priority 3: Info-password
  end;
end;

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;
begin
  case ResultType of
    stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stCurrency, stBigDecimal:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString];
    stDouble:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString,
        stTime, stDate, stTimeStamp];
    stString, stUnicodeString:
      Result := True;
    stBytes:
      Result := InitialType in [stString, stUnicodeString, stBytes, stGUID,
        stAsciiStream, stUnicodeStream, stBinaryStream];
    stTimestamp:
      Result := InitialType in [stString, stUnicodeString, stDate, stTime, stTimestamp, stDouble];
    stDate:
      Result := InitialType in [stString, stUnicodeString, stDate, stTimestamp, stDouble];
    stTime:
      Result := InitialType in [stString, stUnicodeString, stTime, stTimestamp, stDouble];
    stBinaryStream:
      Result := (InitialType in [stBinaryStream, stBytes]) and (InitialType <> stUnknown);
    else
      Result := (ResultType = InitialType) and (InitialType <> stUnknown);
  end;
end;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;
begin
  case ColumnType of
    stBoolean:        Result := 'Boolean';
    stByte:           Result := 'Byte';
    stShort:          Result := 'Short';
    stWord:           Result := 'Word';
    stSmall:          Result := 'Small';
    stLongWord:       Result := 'LongWord';
    stInteger:        Result := 'Integer';
    stULong:          Result := 'ULong';
    stLong:           Result := 'Long';
    stFloat:          Result := 'Float';
    stDouble:         Result := 'Double';
    stCurrency:       Result := 'Currency';
    stBigDecimal:     Result := 'BigDecimal';
    stString:         Result := 'String';
    stUnicodeString:  Result := 'UnicodeString';
    stBytes:          Result := 'Bytes';
    stGUID:           Result := 'GUID';
    stDate:           Result := 'Date';
    stTime:           Result := 'Time';
    stTimestamp:      Result := 'Timestamp';
    stAsciiStream:    Result := 'AsciiStream';
    stUnicodeStream:  Result := 'UnicodeStream';
    stBinaryStream:   Result := 'BinaryStream';
    stArray:          Result := 'Array';
    stDataSet:        Result := 'DataSet';
    else
      Result := 'Unknown';
  end;
end;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);
begin
  if E is EZSQLException then
  begin
    raise EZSQLException.CreateClone(EZSQLException(E));
  end
  else
  begin
    raise EZSQLException.Create(E.Message);
  end;
end;

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);
var
  I: Integer;
  Current: TZColumnInfo;
  ColumnInfo: TZColumnInfo;
begin
  for I := 0 to FromList.Count - 1 do
  begin
    Current := TZColumnInfo(FromList[I]);
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.AutoIncrement := Current.AutoIncrement;
    ColumnInfo.CaseSensitive := Current.CaseSensitive;
    ColumnInfo.Searchable := Current.Searchable;
    ColumnInfo.Currency := Current.Currency;
    ColumnInfo.Nullable := Current.Nullable;
    ColumnInfo.Signed := Current.Signed;
    ColumnInfo.ColumnDisplaySize := Current.ColumnDisplaySize;
    ColumnInfo.ColumnLabel := Current.ColumnLabel;
    ColumnInfo.ColumnName := Current.ColumnName;
    ColumnInfo.SchemaName := Current.SchemaName;
    ColumnInfo.Precision := Current.Precision;
    ColumnInfo.Scale := Current.Scale;
    ColumnInfo.TableName := Current.TableName;
    ColumnInfo.CatalogName := Current.CatalogName;
    ColumnInfo.ColumnType := Current.ColumnType;
    ColumnInfo.ReadOnly := Current.ReadOnly;
    ColumnInfo.Writable := Current.Writable;
    ColumnInfo.DefinitelyWritable := Current.DefinitelyWritable;
    ColumnInfo.ColumnCodePage := Current.ColumnCodePage;

    ToList.Add(ColumnInfo);
  end;
end;

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(Statement: IZStatement; const ParamName: string;
  const Default: string): string;
begin
  Result := Statement.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Statement.GetConnection.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Default;
end;

{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;
begin
  if Value = '' then
    Result := '%'
  else
    Result := Value;
end;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Length then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}

function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
var P: PWideChar;
begin
  Result := ''; //init speeds setlength x2
  if ODBC then
  begin
    SetLength(Result,(Len shl 1)+2); //shl 1 = * 2 but faster
    P := Pointer(Result);
    P^ := '0';
    Inc(P);
    P^ := 'x';
    Inc(P);
    ZBinToHex(Value, P, Len);
  end
  else
  begin
    SetLength(Result, (Len shl 1)+3); //shl 1 = * 2 but faster
    P := Pointer(Result);
    P^ := 'x';
    Inc(P);
    P^ := #39;
    Inc(P);
    ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    P^ := #39;
  end;
end;

function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
var P: PAnsiChar;
begin
  Result := ''; //init speeds setlength x2
  if ODBC then
  begin
    System.SetLength(Result,(Len shl 1)+2);//shl 1 = * 2 but faster
    P := Pointer(Result);
    P^ := '0';
    Inc(P);
    P^ := 'x';
    Inc(P);
    ZBinToHex(Value, P, Len);
  end
  else
  begin
    SetLength(Result, (Len shl 1)+3); //shl 1 = * 2 but faster
    P := Pointer(Result);
    P^ := 'x';
    Inc(P);
    P^ := #39;
    Inc(P);
    ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    P^ := #39;
  end;
end;

function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;
begin
  {$IFDEF UNICODE}
  Result := GetSQLHexWideString(Value, Len, ODBC);
  {$ELSE}
  Result := GetSQLHexAnsiString(Value, Len, ODBC);
  {$ENDIF}
end;

{**
  Returns a FieldSize in Bytes dependend to the FieldType and CharWidth
  @param <code>TZSQLType</code> the Zeos FieldType
  @param <code>Integer</code> the Current given FieldLength
  @param <code>Integer</code> the Current CountOfByte/Char
  @param <code>Boolean</code> does the Driver returns the FullSizeInBytes
  @returns <code>Integer</code> the count of AnsiChars for Field.Size * SizeOf(Char)
}
function GetFieldSize(const SQLType: TZSQLType; ConSettings: PZConSettings;
  const Precision, CharWidth: Integer; DisplaySize: PInteger = nil;
    SizeInBytes: Boolean = False): Integer;
var
  TempPrecision: Integer;
begin
  if ( SQLType in [stString, stUnicodeString] ) and ( Precision <> 0 )then
  begin
    if SizeInBytes then
      TempPrecision := Precision div CharWidth
    else
      TempPrecision := Precision;

    if Assigned(DisplaySize) then
      DisplaySize^ := TempPrecision;

    if SQLType = stString then
      //the RowAccessor assumes SizeOf(Char)*Precision+SizeOf(Char)
      //the Field assumes Precision*SizeOf(Char)
      {$IFDEF UNICODE}
      if ConSettings^.ClientCodePage^.CharWidth >= 2 then //All others > 3 are UTF8
        Result := TempPrecision shl 1 //add more mem for a reserved thirt byte
      else //two and one byte AnsiChars are one WideChar
        Result := TempPrecision
      {$ELSE}
        if ( ConSettings^.CPType = cCP_UTF8 ) or (ConSettings^.CTRL_CP = zCP_UTF8) then
          Result := TempPrecision * shl 4
        else
          Result := TempPrecision * CharWidth
      {$ENDIF}
    else //stUnicodeString
      //UTF8 can pickup LittleEndian/BigEndian 4 Byte Chars
      //the RowAccessor assumes 2*Precision+2!
      //the Field assumes 2*Precision ??Does it?
      if CharWidth > 2 then
        Result := TempPrecision shl 1
      else
        Result := TempPrecision;
  end
  else
    Result := Precision;
end;

function WideStringStream(const AString: WideString): TStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(PWideChar(AString)^, Length(AString)*2);
  Result.Position := 0;
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryRaw(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TRawByteStringDynArray;
var
  I, C, N: Integer;
  Temp: RawByteString;
  NextIsNChar, ParamFound: Boolean;
  Tokens: TZTokenDynArray;

  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    SetLength(IsNCharIndex, Length(Result));
    if Param and NextIsNChar then
    begin
      IsNCharIndex[High(IsNCharIndex)] := True;
      NextIsNChar := False;
    end
    else
      IsNCharIndex[High(IsNCharIndex)] := False;
  end;
begin
  ParamFound := ({$IFDEF USE_FAST_CHARPOS}ZFastCode.CharPos{$ELSe}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound or ConSettings^.AutoEncode or Assigned(ComparePrefixTokens) then
  begin
    Tokens := Tokenizer.TokenizeBuffer(SQL, [toUnifyWhitespaces]);
    Temp := '';
    SQL := '';
    NextIsNChar := False;
    N := -1;
    CompareSuccess^ := False;
    for I := 0 to High(Tokens) do
    begin
      {check if we've a preparable statement. If ComparePrefixTokens = nil then
        comparing is not required or already done }
      if (Tokens[I].TokenType = ttWord) and Assigned(ComparePrefixTokens) then
        if N = -1 then
        begin
          for C := 0 to high(ComparePrefixTokens) do
            if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then
            begin
              if Length(ComparePrefixTokens[C].ChildMatches) = 0 then
                CompareSuccess^ := True
              else
                N := C; //save group
              Break;
            end;
          if N = -1 then //no sub-tokens ?
            ComparePrefixTokens := nil; //stop compare sequence
        end
        else
        begin //we already got a group
          for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
            if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then
            begin
              CompareSuccess^ := True;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
      SQL := SQL + Tokens[I].Value;
      if ParamFound and (Tokens[I].Value = '?') then
      begin
        Add(Temp);
        Add('?', True);
        Temp := '';
      end
      else
        if ParamFound and NeedNCharDetection and (Tokens[I].Value = 'N') and
          (Length(Tokens) > i) and (Tokens[i+1].Value = '?') then
        begin
          Add(Temp);
          Add('N');
          Temp := '';
          NextIsNChar := True;
        end
        else
          case (Tokens[i].TokenType) of
            ttEscape:
              Temp := Temp +
                {$IFDEF UNICODE}
                ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value,
                  ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
                {$ELSE}
                Tokens[i].Value;
                {$ENDIF}
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword:
              Temp := Temp + ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
            else
              Temp := Temp + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[i].Value);
          end;
    end;
    if (Temp <> '') then
      Add(Temp);
  end
  else
    Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryUni(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TUnicodeStringDynArray;
var
  I, C, N: Integer;
  Tokens: TZTokenDynArray;
  Temp: ZWideString;
  NextIsNChar, ParamFound: Boolean;
  procedure Add(const Value: ZWideString; Const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    SetLength(IsNCharIndex, Length(Result));
    if Param and NextIsNChar then
    begin
      IsNCharIndex[High(IsNCharIndex)] := True;
      NextIsNChar := False;
    end
    else
      IsNCharIndex[High(IsNCharIndex)] := False;
  end;
begin
  ParamFound := ({$IFDEF USE_FAST_CHARPOS}ZFastCode.CharPos{$ELSe}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound or ConSettings^.AutoEncode or Assigned(ComparePrefixTokens) then
  begin
    Tokens := Tokenizer.TokenizeBuffer(SQL, [toUnifyWhitespaces]);

    Temp := '';
    SQL := '';
    NextIsNChar := False;
    N := -1;
    for I := 0 to High(Tokens) do
    begin
      {check if we've a preparable statement. If ComparePrefixTokens = nil then
        comparing is not required or already done }
      if (Tokens[I].TokenType = ttWord) and Assigned(ComparePrefixTokens) then
        if N = -1 then
        begin
          for C := 0 to high(ComparePrefixTokens) do
            if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then
            begin
              if Length(ComparePrefixTokens[C].ChildMatches) = 0 then
                CompareSuccess^ := True
              else
                N := C; //save group
              Break;
            end;
          if N = -1 then //no sub-tokens ?
            ComparePrefixTokens := nil; //stop compare sequence
        end
        else
        begin //we already got a group
          for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
            if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then
            begin
              CompareSuccess^ := True;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
      SQL := SQL + Tokens[I].Value;
      if ParamFound and (Tokens[I].Value = '?') then
      begin
        Add(Temp);
        Add('?', True);
        Temp := '';
      end
      else
        if ParamFound and NeedNCharDetection and (Tokens[I].Value = 'N') and
          (Length(Tokens) > i) and (Tokens[i+1].Value = '?') then
        begin
          Add(Temp);
          Add('N');
          Temp := '';
          NextIsNChar := True;
        end
        else
          {$IFDEF UNICODE}
          Temp := Temp + Tokens[i].Value;
          {$ELSE}
          case (Tokens[i].TokenType) of
            ttEscape, ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword:
              Temp := Temp + ConSettings^.ConvFuncs.ZStringToUnicode(Tokens[i].Value, ConSettings^.CTRL_CP)
            else
              Temp := Temp + ASCII7ToUnicodeString(Tokens[i].Value);
          end;
          {$ENDIF}
    end;
    if (Temp <> '') then
      Add(Temp);
  end
  else
    {$IFDEF UNICODE}
    Add(SQL);
    {$ELSE}
    Add(ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings^.CTRL_CP));
    {$ENDIF}
end;

{$IF defined(ENABLE_MYSQL) or defined(ENABLE_POSTGRESQL) or defined(ENABLE_INTERBASE)}
procedure AssignOutParamValuesFromResultSet(const ResultSet: IZResultSet;
  OutParamValues: TZVariantDynArray; const OutParamCount: Integer;
  const ParamTypes: array of ShortInt);
var
  ParamIndex, I: Integer;
  HasRows: Boolean;
  SupportsMoveAbsolute: Boolean;
  Meta: IZResultSetMetadata;
begin
  SupportsMoveAbsolute := ResultSet.GetType <> rtForwardOnly;
  if SupportsMoveAbsolute then ResultSet.BeforeFirst;
  HasRows := ResultSet.Next;

  I := FirstDbcIndex;
  Meta := ResultSet.GetMetadata;
  for ParamIndex := 0 to OutParamCount - 1 do
  begin
    if not (ParamTypes[ParamIndex] in [2, 3, 4]) then // ptOutput, ptInputOutput, ptResult
      Continue;
    if I > Meta.GetColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} then
      Break;

    if (not HasRows) or (ResultSet.IsNull(I)) then
      OutParamValues[ParamIndex] := NullVariant
    else
      case Meta.GetColumnType(I) of
        stBoolean:
          OutParamValues[ParamIndex] := EncodeBoolean(ResultSet.GetBoolean(I));
        stByte:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetByte(I));
        stShort:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetShort(I));
        stWord:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetWord(I));
        stSmall:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetSmall(I));
        stLongword:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetUInt(I));
        stInteger:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetInt(I));
        stULong:
          OutParamValues[ParamIndex] := EncodeUInteger(ResultSet.GetULong(I));
        stLong:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetLong(I));
        stBytes:
          OutParamValues[ParamIndex] := EncodeBytes(ResultSet.GetBytes(I));
        stFloat:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetFloat(I));
        stDouble:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetDouble(I));
        stCurrency:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetCurrency(I));
        stBigDecimal:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetBigDecimal(I));
        stString, stAsciiStream:
          OutParamValues[ParamIndex] := EncodeString(ResultSet.GetString(I));
        stUnicodeString, stUnicodeStream:
          OutParamValues[ParamIndex] := EncodeUnicodeString(ResultSet.GetUnicodeString(I));
        stDate:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetDate(I));
        stTime:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetTime(I));
        stTimestamp:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetTimestamp(I));
        stBinaryStream:
          OutParamValues[ParamIndex] := EncodeInterface(ResultSet.GetBlob(I));
        else
          OutParamValues[ParamIndex] := EncodeString(ResultSet.GetString(I));
      end;
    Inc(I);
  end;
  if SupportsMoveAbsolute then ResultSet.BeforeFirst;
end;
{$IFEND}

function TestEncoding(const Bytes: TByteDynArray; const Size: Cardinal;
  const ConSettings: PZConSettings): TZCharEncoding;
begin
  Result := ceDefault;
  {EgonHugeist:
    Step one: Findout, wh at's comming in! To avoid User-Bugs as good as possible
      it is possible that a PAnsiChar OR a PWideChar was written into
      the Stream!!!  And these chars could be trunced with changing the
      Stream.Size.
      I know this can lead to pain with two byte ansi chars, but what else can i do?
    step two: detect the encoding }

  if (Size mod 2 = 0) and ( ZFastCode.StrLen(PAnsiChar(Bytes)) {%H-}< Size ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16
  else
    if ConSettings.AutoEncode then
      case ZDetectUTF8Encoding(Pointer(Bytes), Size) of
        etUSASCII: Result := ceDefault; //Exact!
        etAnsi:
          { Sure this isn't right in all cases!
            Two/four byte WideChars causing the same result!
            Leads to pain! Is there a way to get a better test?
            I've to start from the premise the function which calls this func
            should decide wether ansi or unicode}
          Result := ceAnsi;
        etUTF8: Result := ceUTF8; //Exact!
      end
    else
      Result := ceDefault
end;

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
{$WARNINGS OFF}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString;
var
  US: ZWideString; //possible com base widestring -> prevent overflow
  Bytes: TByteDynArray;
begin
  if Size = 0 then
    Result := ''
  else
  begin
    SetLength(Bytes, Size +2);
    System.move(Buffer^, Pointer(Bytes)^, Size);
    case TestEncoding(Bytes, Size, ConSettings) of
      ceDefault: ZSetString(Buffer, Size, Result);
      ceAnsi:
        if ConSettings.ClientCodePage.Encoding = ceAnsi then
          if ( ConSettings.CTRL_CP = zCP_UTF8) or (ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP) then //second test avoids encode the string twice
            ZSetString(Buffer, Size, Result)  //should be exact
          else
          begin
            US := PRawToUnicode(Pointer(Bytes), Size, ConSettings.CTRL_CP);
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
          end
        else  //Database expects UTF8
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            if ZDefaultSystemCodePage = zCP_UTF8 then
              Result := AnsiToUTF8(String(PAnsiChar(Bytes))) //Can't localize the ansi CP
            else
            begin
              US := PRawToUnicode(Pointer(Bytes), Size, ZDefaultSystemCodePage);
              Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP);
            end
          else
          begin
            US := PRawToUnicode(Pointer(Bytes), Size, ConSettings.CTRL_CP);
            Result := UTF8Encode(US);
          end;
      ceUTF8:
        if ConSettings.ClientCodePage.Encoding = ceAnsi then //ansi expected
          {$IFDEF WITH_LCONVENCODING}
          Result := Consettings.PlainConvertFunc(String(PAnsiChar(Bytes)))
          {$ELSE}
          Result := ZUnicodeToRaw(UTF8ToString(PAnsiChar(Bytes)), ConSettings.ClientCodePage.CP)
          {$ENDIF}
         else //UTF8 Expected
           ZSetString(Buffer, Size, Result);  //should be exact
      ceUTF16:
        begin
          SetLength(US, Size shr 1);
          System.Move(Bytes[0], US[1], Size);
          if ConSettings.ClientCodePage.Encoding = ceAnsi then
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(UTF8Encode(US))
            {$ELSE}
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
            {$ENDIF}
          else
            Result := UTF8Encode(US);
        end;
      else
        Result := '';
    end;
  end;
end;
{$WARNINGS ON}

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString;
var DB_CP: Word;
begin
  DB_CP := ConSettings.ClientCodePage.CP;
  ConSettings.ClientCodePage.CP := ToCP;
  Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
  ConSettings.ClientCodePage.CP := DB_CP;
end;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString;
begin
  if FromDB then
    if ( ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP ) or not ConSettings.AutoEncode then
      Result := Ansi
    else
      {$IFDEF WITH_LCONVENCODING}
      Result := Consettings.DbcConvertFunc(Ansi)
      {$ELSE}
      Result := ZUnicodeToRaw(ZRawToUnicode(Ansi, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP)
      {$ENDIF}
  else
    Result := ''; // not done yet  and not needed. Makes the compiler happy
end;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Stream the Stream with the unknown format and data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  Len: Integer;
  US: ZWideString;
  Bytes: TByteDynArray;
begin
  Result := nil;
  US := '';
  if Assigned(Buffer) and ( Size > 0 ) then
  begin
    SetLength(Bytes, Size +2);
    System.move(Buffer^, Pointer(Bytes)^, Size);
    if FromDB then //do not check encoding twice
      Result := GetValidatedUnicodeStream(PAnsiChar(Bytes), ConSettings, FromDB)
    else
      case TestEncoding(Bytes, Size, ConSettings) of
        ceDefault: US := USASCII7ToUnicodeString(Buffer, Size);
        ceAnsi: //We've to start from the premisse we've got a Unicode string in here ):
          begin
            SetLength(US, Size shr 1);
            System.Move(Pointer(Bytes)^, Pointer(US)^, Size);
          end;
        ceUTF8: US := PRawToUnicode(Buffer, size, zCP_UTF8);
        ceUTF16:
          begin
            SetLength(US, Size shr 1);
            System.Move(Pointer(Bytes)^, Pointer(US)^, Size);
          end;
      end;

    Len := Length(US) shl 1;
    if not Assigned(Result) and (Len > 0) then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(Pointer(US)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
  end;
end;

function GetValidatedUnicodeStream(const Ansi: RawByteString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  Len: Integer;
  US: ZWideString;
begin
  Result := nil;
  if Ansi <> '' then
  begin
    if FromDB then
      {$IFDEF WITH_LCONVENCODING}
      US := UTF8ToString(Consettings.DbcConvertFunc(Ansi))
      {$ELSE}
      US := ZRawToUnicode(Ansi, ConSettings.ClientCodePage.CP)
      {$ENDIF}
    else
      case ZDetectUTF8Encoding(Ansi) of
        etUSASCII: US := USASCII7ToUnicodeString(Ansi);
        etUTF8: US := PRawToUnicode(Pointer(Ansi), Length(Ansi), zCP_UTF8);
        etAnsi:
          {$IFDEF WITH_LCONVENCODING}
          US := ZWideString(Ansi); //random success
          {$ELSE}
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            US := ZWideString(Ansi) //random success
          else
            US := ZRawToUnicode(Ansi, ConSettings.CTRL_CP);
         {$ENDIF}
      end;

    Len := Length(US)*2;
    if Len > 0 then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(Pointer(US)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
  end;
end;

end.

