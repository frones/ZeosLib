{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcMySqlUtils;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZPlainMySqlDriver, ZDbcLogging;

const
  MAXBUF = 65535;

type
  {** Silent exception }
  EZMySQLSilentException = class(EAbort);

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(PlainDriver: IZMySQLPlainDriver;
  FieldHandle: PZMySQLField; FieldFlags: Integer): TZSQLType;

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string): TZSQLType;

{**
  Converts MySQL Timestamp to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function MySQLTimestampToDateTime(const Value: string): TDateTime;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; const LogMessage: string);
{$IFDEF MYSQL_USE_PREPARE}
procedure CheckMySQLPrepStmtError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; const LogMessage: string);
{$ENDIF}

procedure EnterSilentMySQLError;
procedure LeaveSilentMySQLError;

{**
  Decodes a MySQL Version Value encoded with format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the MySQL Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeMySQLVersioning(const MySQLVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);

{**
  Encodes major, minor and subversion (revision) values in MySQL format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  For example, 4.1.12 is returned as 40112.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeMySQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;

{**
  Decodes a MySQL Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertMySQLVersionToSQLVersion( const MySQLVersion: Integer ): Integer;

{$IFDEF MYSQL_USE_PREPARE}
function getMySQLFieldSize (field_type: Byte; field_size: LongWord): LongWord;
{$ENDIF}

implementation

uses ZMessages, Math, ZPlainMySqlConstants;

threadvar
  SilentMySQLError: Integer;

procedure EnterSilentMySQLError;
begin
  Inc(SilentMySQLError);
end;

procedure LeaveSilentMySQLError;
begin
  Dec(SilentMySQLError);
end;

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags a field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(PlainDriver: IZMySQLPlainDriver;
  FieldHandle: PZMySQLField; FieldFlags: Integer): TZSQLType;
begin
  case PlainDriver.GetFieldType(FieldHandle) of
    FIELD_TYPE_TINY:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stByte
        else Result := stShort;
      end;
    FIELD_TYPE_YEAR, FIELD_TYPE_SHORT:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stShort
        else Result := stInteger;
      end;
    FIELD_TYPE_INT24, FIELD_TYPE_LONG:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stInteger
        else Result := stLong;
      end;
    FIELD_TYPE_LONGLONG:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stLong
        else Result := stBigDecimal;
      end;
    FIELD_TYPE_FLOAT:
//      Result := stFloat;
      Result := stDouble;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL: {ADDED FIELD_TYPE_NEWDECIMAL by fduenas 20-06-2006}
      begin
        if PlainDriver.GetFieldDecimals(FieldHandle) = 0 then
        begin
          if PlainDriver.GetFieldLength(FieldHandle) < 11 then
            Result := stInteger
          else Result := stLong;
        end else
          Result := stDouble;
      end;
    FIELD_TYPE_DOUBLE:
      Result := stDouble;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
      Result := stDate;
    FIELD_TYPE_TIME:
      Result := stTime;
    FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP:
      Result := stTimestamp;
    FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB:
      if (FieldFlags and BINARY_FLAG) = 0 then
        Result := stAsciiStream
      else Result := stBinaryStream;
    else
      Result := stString;
  end;
end;

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string): TZSQLType;
var
  IsUnsigned: Boolean;
  Posi, Len: Integer;
  Spec: string;
begin
  TypeName := UpperCase(TypeName);
  TypeNameFull := UpperCase(TypeNameFull);
  Result := stUnknown;

  Posi := FirstDelimiter(' ', TypeName);
  if Posi > 0 then
    TypeName := Copy(TypeName, 1, Posi - 1);

  Spec := '';
  Posi := FirstDelimiter(' ', TypeNameFull);
  if Posi > 0 then
    Spec := Copy(TypeNameFull, Posi + 1, Length(TypeNameFull)-Posi);

  IsUnsigned := Pos('UNSIGNED', Spec) > 0;

  if TypeName = 'TINYINT' then
  begin
    if IsUnsigned then
      Result := stShort
    else Result := stByte;
  end
  else if TypeName = 'YEAR' then
    Result := stShort
  else if TypeName = 'SMALLINT' then
  begin
    if IsUnsigned then
      Result := stInteger
    else Result := stShort;
  end
  else if TypeName = 'MEDIUMINT' then
    Result := stInteger
  else if TypeName = 'INT' then
  begin
    if StartsWith(TypeNameFull, 'INT(10)') or
      StartsWith(TypeNameFull, 'INT(11)') then
    begin
      if IsUnsigned then
        Result := stLong
      else Result := stInteger;
    end else
      Result := stInteger;
  end
  else if TypeName = 'INTEGER' then
  begin
    if IsUnsigned then
      Result := stLong
    else Result := stInteger;
  end
  else if TypeName = 'BIGINT' then
    Result := stLong
  else if TypeName = 'INT24' then
    Result := stLong
  else if TypeName = 'REAL' then
  begin
    if IsUnsigned then
      Result := stDouble
    else Result := stFloat;
  end
  else if TypeName = 'FLOAT' then
  begin
//    if IsUnsigned then
      Result := stDouble
//    else Result := stFloat;
  end
  else if TypeName = 'DECIMAL' then
  begin
    if EndsWith(TypeNameFull, ',0)') then
    begin
      Len := StrToInt(Copy(TypeNameFull, 9, Length(TypeNameFull) - 11));
      if Len < 10 then
        Result := stInteger
      else Result := stLong;
    end else
      Result := stDouble;
  end
  else if TypeName = 'DOUBLE' then
    Result := stDouble
  else if TypeName = 'CHAR' then
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
  else if TypeName = 'TINYBLOB' then
    Result := stBinaryStream
  else if TypeName = 'BLOB' then
    Result := stBinaryStream
  else if TypeName = 'MEDIUMBLOB' then
    Result := stBinaryStream
  else if TypeName = 'LONGBLOB' then
    Result := stBinaryStream
  else if TypeName = 'TINYTEXT' then
    Result := stAsciiStream
  else if TypeName = 'TEXT' then
    Result := stAsciiStream
  else if TypeName = 'MEDIUMTEXT' then
    Result := stAsciiStream
  else if TypeName = 'LONGTEXT' then
    Result := stAsciiStream
  else if TypeName = 'ENUM' then
  begin
    if (TypeNameFull = 'ENUM(''Y'',''N'')')
      or (TypeNameFull = 'ENUM(''N'',''Y'')') then
      Result := stBoolean
    else Result := stString;
  end
  else if TypeName = 'SET' then
    Result := stString;
end;

{**
  Converts MySQL Timestamp to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function MySQLTimestampToDateTime(const Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Integer;
  StrLength, StrPos: Integer;
begin
  Month := 0;
  Day := 0;
  Hour := 0;
  Min := 0;
  Sec := 0;
  Result := 0;

  StrLength := Length(Value);
  if (StrLength = 14) or (StrLength = 8) then
  begin
    StrPos := 5;
    Year := StrToIntDef(Copy(Value, 1, 4), 0);
  end
  else
  begin
    StrPos := 3;
    Year := StrToIntDef(Copy(Value, 1, 2), 0);
  end;

  if StrLength > 2 then  {Add Month}
  begin
    Month := StrToIntDef(Copy(Value, StrPos, 2), 0);
    if StrLength > 4 then {Add Day}
    begin
      Day := StrToIntDef(Copy(Value, StrPos + 2, 2), 0);
      if StrLength > 6 then {Add Hour}
      begin
        Hour := StrToIntDef(Copy(Value, StrPos + 4, 2), 0);
        if StrLength > 8 then {Add Minute}
        begin
          Min := StrToIntDef(Copy(Value, StrPos + 6, 2), 0);
          if StrLength > 10 then {Add Second}
            Sec := StrToIntDef(Copy(Value, StrPos + 8, 2), 0);
       end;
     end;
   end;
  end;

  if (Year <> 0) and (Month <> 0) and (Day <> 0) then
  begin
    try
      Result := EncodeDate(Year, Month, Day)
    except
    end;
  end;

  try
    Result := Result + EncodeTime(Hour, Min, Sec, 0);
  except
  end;
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; const LogMessage: string);
var
  ErrorMessage: string;
  ErrorCode: Integer;
begin
  ErrorMessage := Trim(StrPas(PlainDriver.GetLastError(Handle)));
  ErrorCode := PlainDriver.GetLastErrorCode(Handle);
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ErrorMessage]));
  end;
end;

{$IFDEF MYSQL_USE_PREPARE}
procedure CheckMySQLPrepStmtError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; const LogMessage: string);
var
  ErrorMessage: string;
  ErrorCode: Integer;
begin
  ErrorMessage := Trim(PlainDriver.GetLastPreparedError(Handle));
  ErrorCode := PlainDriver.GetLastPreparedErrorCode(Handle);
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory,PlainDriver.GetProtocol,LogMessage,ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ErrorMessage]));
  end;
end;
{$ENDIF}

{**
  Decodes a MySQL Version Value encoded with format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the MySQL Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeMySQLVersioning(const MySQLVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);
begin
 MajorVersion := Trunc(MySQLVersion/10000);
 MinorVersion := Trunc((MySQLVersion-(MajorVersion*10000))/100);
 SubVersion   := Trunc((MySQLVersion-(MajorVersion*10000)-(MinorVersion*100)));
end;

{**
  Encodes major, minor and subversion (revision) values in MySQL format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  For example, 4.1.12 is returned as 40112.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeMySQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;
begin
 Result := (MajorVersion * 10000) + (MinorVersion * 100) + SubVersion;
end;

{**
  Decodes a MySQL Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  So it transforms a version in format XYYZZ to XYYYZZZ where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param MySQLVersion an integer containing the Full MySQL Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertMySQLVersionToSQLVersion( const MySQLVersion: Integer ): integer;
var MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeMySQLVersioning(MySQLVersion,MajorVersion,MinorVersion,SubVersion);
 Result := EncodeSQLVersioning(MajorVersion,MinorVersion,SubVersion);
end;

{$IFDEF MYSQL_USE_PREPARE}
function getMySQLFieldSize (field_type: Byte; field_size: LongWord): LongWord;
var
    MaxBlobSize: LongWord;
    SmallBLOB: Word;
Begin
    MaxBlobSize := 255; // mdaems : temporary solution (See PDO PDOPlainMysqlDriver)
//    MaxBlobSize := FPlainDriver.GetMaximumBLOBSize;
    if (MaxBlobSize > 65535) then
        SmallBLOB := 65535
    else
        SmallBLOB := MaxBlobSize;

    case field_type of
        FIELD_TYPE_TINY:        Result := 1;
        FIELD_TYPE_SHORT:       Result := 2;
        FIELD_TYPE_LONG:        Result := 3;
        FIELD_TYPE_LONGLONG:    Result := 8;
        FIELD_TYPE_FLOAT:       Result := 4;
        FIELD_TYPE_DOUBLE:      Result := 8;
        FIELD_TYPE_TIMESTAMP:   Result := sizeOf(MYSQL_TIME);
        FIELD_TYPE_DATE:        Result := sizeOf(MYSQL_TIME);
        FIELD_TYPE_TIME:        Result := sizeOf(MYSQL_TIME);
        FIELD_TYPE_DATETIME:    Result := sizeOf(MYSQL_TIME);
        FIELD_TYPE_TINY_BLOB:   Result := 255;
        FIELD_TYPE_MEDIUM_BLOB: Result := SmallBLOB;
        FIELD_TYPE_LONG_BLOB:   Result := MaxBlobSize;
        FIELD_TYPE_BLOB:        Result := MaxBlobSize;
        FIELD_TYPE_STRING:      Result := MaxBlobSize;
        FIELD_TYPE_VAR_STRING:  Result := field_size; {mysql 5.0.3 moved limit to 2^16}
    else
{  FIELD_TYPE_DECIMAL   = 0;
  FIELD_TYPE_NULL      = 6;
  FIELD_TYPE_INT24     = 9;
  FIELD_TYPE_YEAR      = 13;
  FIELD_TYPE_NEWDATE   = 14;
  FIELD_TYPE_VARCHAR   = 15; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_BIT       = 16; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_NEWDECIMAL = 246; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_ENUM      = 247;
  FIELD_TYPE_SET       = 248;
  FIELD_TYPE_VAR_STRING = 253;
  FIELD_TYPE_GEOMETRY  = 255;
}        Result := 255;  {unknown ??}
    end;
end;
{$ENDIF}

end.
