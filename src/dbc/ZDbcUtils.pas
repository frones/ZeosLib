{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Connectivity Functions              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcUtils;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, Contnrs, ZCompatibility, ZDbcIntfs, ZDbcResultSetMetadata;

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
  AnsiQuotedStr or NullText
  @param S the string
  @param NullText the "NULL"-Text
  @param QuoteChar the char that is used for quotation
  @return 'null' if S is '', otherwise AnsiQuotedStr(S)
}
function AQSNullText(const Value, NullText: string; QuoteChar: Char = ''''): string;

{**
  AnsiQuotedStr or Null
  @param S the string
  @return 'null' if S is '', otherwise AnsiQuotedStr(S)
}
function AQSNull(const Value: string; QuoteChar: Char = ''''): string;

{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;

{**
  PrepareUnicodeStream checks the incoming Stream for his given Memory and
  returns a valid UTF8 StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedUnicodeStream(const Stream: TStream): TStream;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Len then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}
function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZAnsiString;
function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;

{**
  Returns a FieldSize in Bytes dependend to the FieldType and CharWidth
  @param <code>TZSQLType</code> the Zeos FieldType
  @param <code>Integer</code> the Current given FieldLength
  @param <code>Integer</code> the Current CountOfByte/Char
  @param <code>Boolean</code> does the Driver returns the FullSizeInBytes
  @returns <code>Integer</code> the count of AnsiChars for Field.Size * SizeOf(Char)
}
function GetFieldSize(const SQLType: TZSQLType;
  const Precision, CharWidth: Integer; SizeInBytes: Boolean = False): Integer;

implementation

uses ZMessages, ZSysUtils{$IFDEF WITH_WIDESTRUTILS},WideStrUtils{$ENDIF};

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
    stBoolean, stByte, stShort, stInteger,
    stLong, stFloat, stDouble, stBigDecimal:
      Result := InitialType in [stBoolean, stByte, stShort, stInteger,
        stLong, stFloat, stDouble, stBigDecimal, stString, stUnicodeString];
    stString, stUnicodeString:
      Result := True;
    stBytes:
      Result := InitialType in [stString, stUnicodeString, stBytes,
        stAsciiStream, stUnicodeStream, stBinaryStream];
    stTimestamp:
      Result := InitialType in [stString, stUnicodeString, stDate, stTime, stTimestamp];
    stDate:
      Result := InitialType in [stString, stUnicodeString, stDate, stTimestamp];
    stTime:
      Result := InitialType in [stString, stUnicodeString, stTime, stTimestamp];
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
    stBoolean:
      Result := 'Boolean';
    stByte:
      Result := 'Byte';
    stShort:
      Result := 'Short';
    stInteger:
      Result := 'Integer';
    stLong:
      Result := 'Long';
    stFloat:
      Result := 'Float';
    stDouble:
      Result := 'Double';
    stBigDecimal:
      Result := 'BigDecimal';
    stString:
      Result := 'String';
    stUnicodeString:
      Result := 'UnicodeString';
    stBytes:
      Result := 'Bytes';
    stDate:
      Result := 'Date';
    stTime:
      Result := 'Time';
    stTimestamp:
      Result := 'Timestamp';
    stAsciiStream:
      Result := 'AsciiStream';
    stUnicodeStream:
      Result := 'UnicodeStream';
    stBinaryStream:
      Result := 'BinaryStream';
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
  AnsiQuotedStr or NullText
  @param S the string
  @param NullText the "NULL"-Text
  @param QuoteChar the char that is used for quotation
  @return 'null' if S is '', otherwise AnsiQuotedStr(S)
}
function AQSNullText(const Value, NullText: string; QuoteChar: Char): string;
begin
  if Value = '' then
    Result := NullText
  else
    Result := AnsiQuotedStr(Value, QuoteChar);
end;

{**
  AnsiQuotedStr or Null
  @param S the string
  @param QuoteChar the char that is used for quotation
  @return 'null' if S is '', otherwise AnsiQuotedStr(S)
}
function AQSNull(const Value: string; QuoteChar: Char): string;
begin
  Result := AQSNullText(Value, 'null', QuoteChar);
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

function GetValidatedUnicodeStream(const Stream: TStream): TStream;
var
  Ansi: ZAnsiString;
  Len: Integer;
  WS: ZWideString;
  Bytes: TByteDynArray;
begin
  {EgonHugeist: TempBuffer the WideString, }
  //Step one: Findout, wat's comming in! To avoid User-Bugs
    //it is possible that a PAnsiChar OR a PWideChar was written into
    //the Stream!!!  And these chars could be trunced with changing the
    //Stream.Size.
  if Assigned(Stream) then
  begin
    SetLength(Bytes, Stream.Size +2);
    System.move(TMemoryStream(Stream).Memory^, Pointer(Bytes)^, Stream.Size);
    if {$IFDEF DELPHI14_UP}StrLen{$ELSE}Length{$ENDIF}(PWideChar(Bytes)) = Stream.Size then
    begin
      if StrLen(PAnsiChar(Bytes)) >= Stream.Size then  //Hack!! If no #0 is witten then the PAnsiChar could be oversized
      begin
        if DetectUTF8Encoding(PAnsiChar(Bytes)) = etAnsi then
          Ansi := AnsiToUTF8(String(PAnsiChar(Bytes)))
        else
          Ansi := PAnsiChar(Bytes);
      end
      else
      begin
        WS := PWideChar(TMemoryStream(Stream).Memory);
        SetLength(WS, Stream.Size div 2);
        Ansi := UTF8Encode(WS);
      end;
    end
    else
      if StrLen(PAnsiChar(Bytes)) < Stream.Size then //PWideChar written
      begin
        SetLength(WS, Stream.Size div 2);
        System.Move(PWideString(TMemoryStream(Stream).Memory)^,
          PWideChar(WS)^, Stream.Size);
        Ansi := UTF8Encode(WS);
      end
      else
        if StrLen(PAnsiChar(Bytes)) = Stream.Size then
        begin
          if DetectUTF8Encoding(PAnsiChar(Bytes)) = etAnsi then
            Ansi := AnsiToUTF8(String(PAnsiChar(Bytes)))
          else
            Ansi := PAnsiChar(PAnsiChar(Bytes));
        end
        else
        begin
          Ansi := PAnsiChar(Bytes);
          if DetectUTF8Encoding(Ansi) = etAnsi then
            Ansi := AnsiToUTF8(String(Ansi));
        end;
    Len := Length(Ansi);
    Result := TMemoryStream.Create;
    Result.Size := Len;
    System.Move(PAnsiChar(Ansi)^, TMemoryStream(Result).Memory^, Len);
    Result.Position := 0;
    SetLength(Bytes, 0);
  end
  else
    Result := nil;
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
var
  HexVal: AnsiString;
begin
  SetLength(HexVal,Len * 2 );
  BinToHex(Value, PAnsiChar(HexVal), Len);

  if ODBC then
    Result := '0x'+WideString(HexVal)
  else
    Result := 'x'#39+WideString(HexVal)+#39;
end;

function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZAnsiString;
var
  HexVal: AnsiString;
begin
  SetLength(HexVal,Len * 2 );
  BinToHex(Value, PAnsiChar(HexVal), Len);

  if ODBC then
    Result := '0x'+HexVal
  else
    Result := 'x'#39+HexVal+#39;
end;

function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;
begin
  {$IFDEF DELPHI12_UP}
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
function GetFieldSize(const SQLType: TZSQLType;
  const Precision, CharWidth: Integer; SizeInBytes: Boolean = False): Integer;
var
  TempPrecision: Integer;
begin
  if ( SQLType in [stString, stUnicodeString] ) and ( Precision <> 0 )then
  begin
    if SizeInBytes then
      TempPrecision := (Precision div CharWidth) + (Precision mod CharWidth)
    else
      TempPrecision := Precision;

    if SQLType = stString then
      //the RowAccessor assumes SizeOf(Char)*Precision+SizeOf(Char)
      //the Field assumes Precision*SizeOf(Char)
      {$IFDEF DELPHI12_UP}
      if CharWidth = 3 then //All others > 3 are UTF8
        Result := Trunc(TempPrecision * 1.5) //add more mem for a reserved thirt byte
      else //two and one byte AnsiChars are one WideChar
        Result := TempPrecision
      {$ELSE}
      Result := TempPrecision * CharWidth
      {$ENDIF}
    else //stUnicodeString
      //UTF8 can pickup LittleEndian/BigEndian 4 Byte Chars
      //the RowAccessor assumes 2*Precision+2
      //the Field assumes 2*Precision
      Result := TempPrecision * 2;
  end
  else
    Result := Precision;
end;

end.

