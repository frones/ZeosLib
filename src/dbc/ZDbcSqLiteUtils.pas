{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
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

unit ZDbcSqLiteUtils;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZPlainSqLiteDriver, ZDbcLogging, ZCompatibility;

{**
  Convert string SQLite field type to SQLType
  @param string field type value
  @param Precision the column precision or size
  @param Decimals the column position after decimal point
  @result the SQLType field type value
}
function ConvertSQLiteTypeToSQLType(TypeName: string; var Precision: Integer;
  var Decimals: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Checks for possible sql errors.
  @param PlainDriver a SQLite plain driver.
  @param ErrorCode an error code.
  @param ErrorMessage an error message.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckSQLiteError(PlainDriver: IZSQLitePlainDriver;
  ErrorCode: Integer; ErrorMessage: PAnsiChar;
  LogCategory: TZLoggingCategory; LogMessage: string);

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeString(Value: Zansistring): Zansistring;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(Value: ansistring): ansistring;

{**
  Decodes a SQLite Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param SQLiteVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertSQLiteVersionToSQLVersion( const SQLiteVersion: PAnsiChar ): Integer;


implementation

uses ZMessages{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Convert string SQLite field type to SQLType
  @param string field type value
  @param Precision the column precision or size
  @param Decimals the column position after decimal point
  @result the SQLType field type value
}
function ConvertSQLiteTypeToSQLType(TypeName: string; var Precision: Integer;
  var Decimals: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;
var
  P1, P2: Integer;
  Temp: string;
begin
  TypeName := UpperCase(TypeName);
  Result := stString;
  Precision := 0;
  Decimals := 0;

  P1 := Pos('(', TypeName);
  P2 := Pos(')', TypeName);
  if (P1 > 0) and (P2 > 0) then
  begin
    Temp := Copy(TypeName, P1 + 1, P2 - P1 - 1);
    TypeName := Copy(TypeName, 1, P1 - 1);
    P1 := Pos(',', Temp);
    if P1 > 0 then
    begin
      Precision := StrToIntDef(Copy(Temp, 1, P1 - 1), 0);
      Decimals := StrToIntDef(Copy(Temp, P1 + 1, Length(Temp) - P1), 0);
    end
    else
      Precision := StrToIntDef(Temp, 0);
  end;

  if StartsWith(TypeName, 'BOOL') then
    Result := stBoolean
  else if TypeName = 'TINYINT' then
    Result := stByte
  else if TypeName = 'SMALLINT' then
    Result := stShort
  else if TypeName = 'MEDIUMINT' then
    Result := stInteger
  else if StartsWith(TypeName, 'INT') then
    Result := stInteger
  else if TypeName = 'BIGINT' then
    Result := stLong
  else if StartsWith(TypeName, 'REAL') then
    Result := stDouble
  else if StartsWith(TypeName, 'FLOAT') then
    Result := stDouble
  else if (TypeName = 'NUMERIC') or (TypeName = 'DECIMAL')
    or (TypeName = 'NUMBER') then
  begin
   { if Decimals = 0 then
      Result := stInteger
    else} Result := stDouble;
  end
  else if StartsWith(TypeName, 'DOUB') then
    Result := stDouble
  else if TypeName = 'MONEY' then
    Result := stBigDecimal
  else if StartsWith(TypeName, 'CHAR') then
    Result := stString
  else if TypeName = 'VARCHAR' then
    Result := stString
  else if TypeName = 'VARBINARY' then
    Result := stBytes
  else if TypeName = 'BINARY' then
    Result := stBytes
  else if TypeName = 'DATE' then
    Result := stDate
  else if TypeName = 'TIME' then
    Result := stTime
  else if TypeName = 'TIMESTAMP' then
    Result := stTimestamp
  else if TypeName = 'DATETIME' then
    Result := stTimestamp
  else if Pos('BLOB', TypeName) > 0 then
    Result := stBinaryStream
  else if Pos('CLOB', TypeName) > 0 then
    Result := stAsciiStream
  else if Pos('TEXT', TypeName) > 0 then
    Result := stAsciiStream;

  if (Result = stInteger) and (Precision <> 0) then
  begin
    if Precision <= 2 then
      Result := stByte
    else if Precision <= 4 then
      Result := stShort
    else if Precision <= 9 then
      Result := stInteger
    else
      Result := stLong;
  end;

  if ( CtrlsCPType = cCP_UTF16 ) then
    case Result of
      stString:  Result := stUnicodeString;
      stAsciiStream: Result := stUnicodeStream;
    end;

  if (Result = stString) then
    if (Precision = 0) then
      Precision := 255 *{$IFDEF UNICODE}2{$ELSE}4{$ENDIF}//UTF8 assumes 4Byte/Char
    else
      Precision := Precision*{$IFDEF UNICODE}2{$ELSE}4{$ENDIF};//UTF8 assumes 4Byte/Char

  if (Result = stUnicodeString) then
    if (Precision = 0) then
      Precision := 255 * 2 //UTF8 assumes 4Byte/Char -> 2 * UnicodeChar
    else
      Precision := Precision * 2;//UTF8 assumes 4Byte/Char

end;

{**
  Checks for possible sql errors.
  @param PlainDriver a SQLite plain driver.
  @param ErrorCode an error code.
  @param ErrorMessage an error message.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckSQLiteError(PlainDriver: IZSQLitePlainDriver;
  ErrorCode: Integer; ErrorMessage: PAnsiChar;
  LogCategory: TZLoggingCategory; LogMessage: string);
var
  Error: string;
begin
  if ErrorMessage <> nil then
  begin
  {$IFDEF DELPHI12_UP}
    Error := trim(UTF8ToUnicodeString(ErrorMessage));
  {$ELSE}
    {$IFNDEF FPC}
    Error := Trim(UTF8ToAnsi(StrPas(ErrorMessage)));
    {$ELSE}
    Error := Trim(StrPas(ErrorMessage));
    {$ENDIF}
  {$ENDIF}
    PlainDriver.FreeMem(ErrorMessage);
  end
  else
    Error := '';
  if not (ErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
  begin
    if Error = '' then
      Error := String(StrPas(PlainDriver.ErrorString(ErrorCode)));

    DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
      ErrorCode, Error);
    raise EZSQLException.CreateWithCode(ErrorCode, Format(SSQLError1, [Error]));
  end;
end;


function NewEncodeString(Value: Zansistring): Zansistring;
var
  I: Integer;
  SrcLength: Integer;
  SrcBuffer: PAnsiChar;
  ihx : integer;
  shx : ansistring;
begin
  SrcLength := Length(Value);
  SrcBuffer := PAnsiChar(Value);
  SetLength( Result,3 + SrcLength * 2 );
  Result[1] := 'x'; // set x
  Result[2] := ''''; // set Open Quote
  ihx := 3; // set 1st hex location
  for I := 1 to SrcLength do
  begin
    shx := AnsiString(IntToHex( ord(SrcBuffer^),2 )); // eg. '3E'
    result[ihx] := shx[1]; Inc( ihx,1 ); // copy '3'
    result[ihx] := shx[2]; Inc( ihx,1 ); // copy 'E'
    Inc( SrcBuffer,1 ); // next byte source location
  end;
  result[ihx] := ''''; // set Close Quote
end;

function NewDecodeString(Value:ansistring):ansistring;
  var
    i : integer;
    srcbuffer : PAnsichar;
  begin
    value := copy(value,3,length(value)-4);
    value := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiLowercase(value);
    i := length(value) div 2;
    srcbuffer := PAnsiChar(value);
    setlength(result,i);
    HexToBin(PAnsiChar(srcbuffer),PAnsiChar(result),i);
end;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeString(Value: Zansistring): Zansistring;
begin
  result := NewEncodeString(Value);
end;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(Value: ansistring): ansistring;
var
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PAnsiChar;
begin
  if pos('x''',String(value))= 1 then
    result := NewDecodeString(value)
  else
  begin
    SrcLength := Length(Value);
    SrcBuffer := PAnsiChar(Value);
    SetLength(Result, SrcLength);
    DestLength := 0;
    DestBuffer := PAnsiChar(Result);

    while SrcLength > 0 do
    begin
      if SrcBuffer^ = '%' then
      begin
        Inc(SrcBuffer);
        if SrcBuffer^ <> '0' then
          DestBuffer^ := SrcBuffer^
        else
          DestBuffer^ := #0;
        Inc(SrcBuffer);
        Dec(SrcLength, 2);
      end
      else
      begin
        DestBuffer^ := SrcBuffer^;
        Inc(SrcBuffer);
        Dec(SrcLength);
      end;
      Inc(DestBuffer);
      Inc(DestLength);
    end;
    SetLength(Result, DestLength);
  end;
end;

{**
  Decodes a SQLite Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param SQLiteVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertSQLiteVersionToSQLVersion( const SQLiteVersion: PAnsiChar ): Integer;
var
  MajorVersion, MinorVersion, SubVersion: Integer;
  s:string;
begin
  s:=String(SQLiteVersion);
  MajorVersion:=StrToIntDef(copy(s,1,pos('.',s)-1),0);
  delete(s,1,pos('.',s));
  MinorVersion:=StrToIntDef(copy(s,1,pos('.',s)-1),0);
  delete(s,1,pos('.',s));
  SubVersion:=StrToIntDef(s,0);
  Result := EncodeSQLVersioning(MajorVersion,MinorVersion,SubVersion);
end;

end.
