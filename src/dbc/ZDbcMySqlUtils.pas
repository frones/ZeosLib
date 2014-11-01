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

unit ZDbcMySqlUtils;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZPlainMySqlDriver, ZPlainMySqlConstants, ZDbcLogging,
  ZCompatibility, ZDbcResultSetMetadata, ZVariant;

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
function ConvertMySQLHandleToSQLType(FieldHandle: PZMySQLField;
  CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(const PlainDriver: IZMySQLPlainDriver;
  const Handle: PZMySQLConnect; const LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; Const ConSettings: PZConSettings);
procedure CheckMySQLPrepStmtError(const PlainDriver: IZMySQLPlainDriver;
  const Handle: PZMySQLConnect; const LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; const ConSettings: PZConSettings;
  ErrorIsIgnored: PBoolean = nil; const IgnoreErrorCode: Integer = 0);

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

function getMySQLFieldSize (field_type: TMysqlFieldTypes; field_size: LongWord): LongWord;

{**
  Returns a valid TZColumnInfo from a FieldHandle
  @param PlainDriver the MySQL PlainDriver interface
  @param FieldHandle the handle of the fetched field
  @returns a new TZColumnInfo
}
function GetMySQLColumnInfoFromFieldHandle(FieldHandle: PZMySQLField;
  ConSettings: PZConSettings; bUseResult:boolean): TZColumnInfo;

procedure ConvertMySQLColumnInfoFromString(const TypeInfo: String;
  ConSettings: PZConSettings; out TypeName, TypeInfoSecond: String;
  out FieldType: TZSQLType; out ColumnSize: Integer; out Precision: Integer);

function MySQLPrepareAnsiSQLParam(Handle: PZMySQLConnect; Value: TZVariant;
  const DefaultValue: String; ClientVarManager: IZClientVariantManager;
  PlainDriver: IZMySQLPlainDriver; const InParamType: TZSQLType;
  const UseDefaults: Boolean; ConSettings: PZConSettings): RawByteString;

implementation

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} Math,
  ZMessages, ZDbcUtils, ZFastCode{$IFDEF UNICODE}, ZEncoding{$ENDIF};

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
function ConvertMySQLHandleToSQLType(FieldHandle: PZMySQLField;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
    case PMYSQL_FIELD(FieldHandle)^._type of
    FIELD_TYPE_TINY:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
         Result := stShort
      else
         Result := stByte;
    FIELD_TYPE_YEAR:
      Result := stWord;
    FIELD_TYPE_SHORT:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
         Result := stSmall
      else
         Result := stWord;
    FIELD_TYPE_INT24, FIELD_TYPE_LONG:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
         Result := stInteger
      else
         Result := stLongWord;
    FIELD_TYPE_LONGLONG:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
         Result := stLong
      else
        Result := stULong;
    FIELD_TYPE_FLOAT:
      Result := stFloat;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL: {ADDED FIELD_TYPE_NEWDECIMAL by fduenas 20-06-2006}
      if PMYSQL_FIELD(FieldHandle)^.decimals = 0 then
        if PMYSQL_FIELD(FieldHandle)^.length < 11 then
          if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
            Result := stInteger
          else
            Result := stLongWord
        else
          if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
             Result := stLong
          else
            Result := stULong
      else
        Result := stDouble;
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
      if (PMYSQL_FIELD(FieldHandle)^.flags and BINARY_FLAG) = 0 then
        If ( CtrlsCPType = cCP_UTF16) then
          Result := stUnicodeStream
        else
          Result := stAsciiStream
      else
        Result := stBinaryStream;
    FIELD_TYPE_BIT:
      Result := stByte;
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_VAR_STRING,
    FIELD_TYPE_STRING:
      if (PMYSQL_FIELD(FieldHandle)^.flags and BINARY_FLAG) = 0 then
        if ( CtrlsCPType = cCP_UTF16) then
          Result := stUnicodeString
        else
          Result := stString
      else
        Result := stBytes;
    FIELD_TYPE_ENUM:
      Result := stString;
    FIELD_TYPE_SET:
      Result := stString;
    FIELD_TYPE_NULL:
      // Example: SELECT NULL FROM DUAL
      Result := stString;
   FIELD_TYPE_GEOMETRY:
      // Todo: Would be nice to show as WKT.
      Result := stBinaryStream;
   else
      raise Exception.Create('Unknown MySQL data type!');
   end;
end;

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
const
  GeoTypes: array[0..7] of string = (
   'POINT','LINESTRING','POLYGON','GEOMETRY',
   'MULTIPOINT','MULTILINESTRING','MULTIPOLYGON','GEOMETRYCOLLECTION'
  );
var
  IsUnsigned: Boolean;
  Posi, Len, i: Integer;
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

  IsUnsigned := ZFastCode.Pos('UNSIGNED', Spec) > 0;

  if TypeName = 'TINYINT' then
  begin
    if IsUnsigned then
      Result := stByte
    else
      Result := stSmall;
  end
  else if TypeName = 'YEAR' then
    Result := stWord  //1901 to 2155, and 0000 in the 4 year format and 1970-2069 if you use the 2 digit format (70-69).
  else if TypeName = 'SMALLINT' then
  begin
    if IsUnsigned then
      Result := stWord  //0 - 65535
    else
      Result := stSmall; //-32768 - 32767
  end
  else if TypeName = 'MEDIUMINT' then
    if IsUnsigned then  //0 - 16777215
       Result := stLongWord
    else
       Result := stInteger //-8388608 - 8388607
  else if (TypeName = 'INT') or (TypeName = 'INTEGER') or (TypeName = 'INT24') then
    if IsUnsigned then
       Result := stLongWord //0 - 4294967295
    else
       Result := stInteger //-2147483648 - 2147483647
  else if TypeName = 'BIGINT' then
    if IsUnsigned then
       Result := stULong //0 - 18446744073709551615
    else
       Result := stLong // -9223372036854775808 - 9223372036854775807
  //else if TypeName = 'INT24' then  //no docs?
    //Result := stLong
  else if TypeName = 'REAL' then
  begin
    if IsUnsigned then
      Result := stDouble
    else
      Result := stFloat;
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
      else
        Result := stLong;
    end
    else
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
    else
      Result := stString;
  end
  else if TypeName = 'SET' then
    Result := stString
  else if TypeName = 'BIT' then
    Result := stSmall
  else
      for i := 0 to Length(GeoTypes) - 1 do
         if GeoTypes[i] = TypeName then
            Result := stBinaryStream;

  if ( CtrlsCPType = cCP_UTF16) then
  case result of
    stString: Result := stUnicodeString;
    stAsciiStream: Result := stUnicodeStream;
  end;

  if Result = stUnknown then
     raise Exception.Create('Unknown MySQL data type!');
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(const PlainDriver: IZMySQLPlainDriver;
  const Handle: PZMySQLConnect; const LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; Const ConSettings: PZConSettings);
var
  ErrorMessage: RawByteString;
  ErrorCode: Integer;
begin
  ErrorMessage := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(PlainDriver.GetLastError(Handle));
  ErrorCode := PlainDriver.GetLastErrorCode(Handle);
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory, ConSettings.Protocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(
        ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
end;

procedure CheckMySQLPrepStmtError(const PlainDriver: IZMySQLPlainDriver;
  const Handle: PZMySQLConnect; const LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; const ConSettings: PZConSettings;
  ErrorIsIgnored: PBoolean = nil; const IgnoreErrorCode: Integer = 0);
var
  ErrorMessage: RawByteString;
  ErrorCode: Integer;
begin
  ErrorCode := PlainDriver.GetLastPreparedErrorCode(Handle);
  if Assigned(ErrorIsIgnored) then
    if (IgnoreErrorCode = ErrorCode) then
    begin
      ErrorIsIgnored^ := True;
      Exit;
    end
    else
      ErrorIsIgnored^ := False;
  ErrorMessage := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(PlainDriver.GetLastPreparedError(Handle));
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(
        ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
end;

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
  MajorVersion := MySQLVersion div 10000;
  MinorVersion := (MySQLVersion - (MajorVersion * 10000)) div 100;
  SubVersion   := MySQLVersion-(MajorVersion*10000)-(MinorVersion*100);
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
var
   MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeMySQLVersioning(MySQLVersion,MajorVersion,MinorVersion,SubVersion);
 Result := EncodeSQLVersioning(MajorVersion,MinorVersion,SubVersion);
end;

function getMySQLFieldSize(field_type: TMysqlFieldTypes; field_size: LongWord): LongWord;
begin
  case field_type of
    FIELD_TYPE_ENUM:        Result := 1;
    FIELD_TYPE_TINY:        Result := 1;
    FIELD_TYPE_SHORT:       Result := 2;
    FIELD_TYPE_LONG:        Result := 4;
    FIELD_TYPE_LONGLONG:    Result := 8;
    FIELD_TYPE_FLOAT:       Result := 4;
    FIELD_TYPE_DOUBLE:      Result := 8;
    FIELD_TYPE_DATE:        Result := sizeOf(MYSQL_TIME);
    FIELD_TYPE_TIME:        Result := sizeOf(MYSQL_TIME);
    FIELD_TYPE_DATETIME:    Result := sizeOf(MYSQL_TIME);
    FIELD_TYPE_TINY_BLOB:   Result := field_size; //stBytes
    FIELD_TYPE_BLOB:        Result := field_size;
    FIELD_TYPE_STRING:      Result := field_size;
  else
    Result := 255;  {unknown ??}
  end;
end;

{**
  Returns a valid TZColumnInfo from a FieldHandle
  @param PlainDriver the MySQL PlainDriver interface
  @param FieldHandle the handle of the fetched field
  @returns a new TZColumnInfo
}
function GetMySQLColumnInfoFromFieldHandle(FieldHandle: PZMySQLField;
  ConSettings: PZConSettings; bUseResult:boolean): TZColumnInfo;
var
  FieldLength: ULong;
begin
  if Assigned(FieldHandle) then
  begin
    Result := TZColumnInfo.Create;
    {$IFDEF UNICODE}
    Result.ColumnLabel := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.name,
      PMYSQL_FIELD(FieldHandle)^.name_length, ConSettings^.ClientCodePage^.CP);
    Result.ColumnName := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.org_name,
      PMYSQL_FIELD(FieldHandle)^.org_name_length, ConSettings^.ClientCodePage^.CP);
    Result.TableName := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.org_table,
      PMYSQL_FIELD(FieldHandle)^.org_table_length, ConSettings^.ClientCodePage^.CP);
    Result.SchemaName := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.db,
      PMYSQL_FIELD(FieldHandle)^.db_length, ConSettings^.ClientCodePage^.CP);
    Result.CatalogName := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.catalog,
      PMYSQL_FIELD(FieldHandle)^.catalog_length, ConSettings^.ClientCodePage^.CP);
    //Result.DefaultValue := PRawToUnicode(PMYSQL_FIELD(FieldHandle)^.def,
      //PMYSQL_FIELD(FieldHandle)^.def_length, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    Result.ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.name, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    Result.ColumnName := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.org_name, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    Result.TableName := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.org_table, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    Result.SchemaName := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.db, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    Result.CatalogName := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.catalog, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    //Result.DefaultValue := ConSettings^.ConvFuncs.ZRawToString(PMYSQL_FIELD(FieldHandle)^.def, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    {$ENDIF}
    Result.ReadOnly := (PMYSQL_FIELD(FieldHandle)^.table = nil);
    Result.Writable := not Result.ReadOnly;
    Result.ColumnType := ConvertMySQLHandleToSQLType(FieldHandle, ConSettings.CPType);
    FieldLength := PMYSQL_FIELD(FieldHandle)^.length;
    //EgonHugeist: arrange the MBCS field DisplayWidth to a proper count of Chars

    if Result.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      Result.ColumnCodePage := ConSettings^.ClientCodePage^.CP
    else
      Result.ColumnCodePage := High(Word);

    if Result.ColumnType in [stString, stUnicodeString] then
       case PMYSQL_FIELD(FieldHandle)^.charsetnr of
        1, 84, {Big5}
        95, 96, {cp932 japanese}
        19, 85, {euckr}
        24, 86, {gb2312}
        38, 87, {gbk}
        13, 88, {sjis}
        35, 90, 128..151:  {ucs2}
          begin
            Result.ColumnDisplaySize := (FieldLength div 4);
            Result.Precision := GetFieldSize(Result.ColumnType, ConSettings,
              Result.ColumnDisplaySize, 2, nil);
          end;
        33, 83, 192..215, { utf8 }
        97, 98, { eucjpms}
        12, 91: {ujis}
          begin
            Result.ColumnDisplaySize := (FieldLength div 3);
            Result.Precision := GetFieldSize(Result.ColumnType,
              ConSettings, Result.ColumnDisplaySize, 3, nil);
          end;
        54, 55, 101..124, {utf16}
        56, 62, {utf16le}
        60, 61, 160..183, {utf32}
        45, 46, 224..247: {utf8mb4}
          begin
            Result.ColumnDisplaySize := (FieldLength div 4);
            Result.Precision := GetFieldSize(Result.ColumnType,
              ConSettings, Result.ColumnDisplaySize, 4, nil);
          end;
        else //1-Byte charsets
        begin
          Result.ColumnDisplaySize := FieldLength;
          Result.Precision := GetFieldSize(Result.ColumnType,
            ConSettings, Result.ColumnDisplaySize, 1, nil);
        end;
      end
    else
      Result.Precision := min(MaxBlobSize,FieldLength);

    if PMYSQL_FIELD(FieldHandle)^._type in [FIELD_TYPE_BLOB, FIELD_TYPE_MEDIUM_BLOB,
       FIELD_TYPE_LONG_BLOB,FIELD_TYPE_STRING, FIELD_TYPE_VAR_STRING] then
      if bUseResult then  //PMYSQL_FIELD(Field)^.max_length not valid
        Result.MaxLenghtBytes := Result.Precision
      else
        Result.MaxLenghtBytes := PMYSQL_FIELD(FieldHandle)^.max_length
    else
      Result.MaxLenghtBytes := FieldLength;
    Result.Scale := PMYSQL_FIELD(FieldHandle)^.decimals;
    Result.AutoIncrement := (AUTO_INCREMENT_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0) or
      (TIMESTAMP_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0);
    Result.Signed := (UNSIGNED_FLAG and PMYSQL_FIELD(FieldHandle)^.flags) = 0;
    if NOT_NULL_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0 then
      Result.Nullable := ntNoNulls
    else
      Result.Nullable := ntNullable;
    // Properties not set via query results here will be fetched from table metadata.
  end
  else
    Result := nil;
end;

procedure ConvertMySQLColumnInfoFromString(const TypeInfo: String;
  ConSettings: PZConSettings; out TypeName, TypeInfoSecond:
  String; out FieldType: TZSQLType; out ColumnSize: Integer; out Precision: Integer);
var
  TypeInfoList: TStrings;
  TypeInfoFirst: String;
  J, TempPos: Integer;
begin
  TypeInfoList := TStringList.Create;
  TypeInfoFirst := '';
  TypeInfoSecond := '';
  Precision := 0;
  ColumnSize := 0;

  if StrPos(PChar(TypeInfo), '(') <> nil then
  begin
    PutSplitString(TypeInfoList, TypeInfo, '()');
    TypeInfoFirst := TypeInfoList.Strings[0];
    TypeInfoSecond := TypeInfoList.Strings[1];
  end
  else
    TypeInfoFirst := TypeInfo;

  TypeInfoFirst := LowerCase(TypeInfoFirst);
  TypeName := TypeInfoFirst;

  FieldType := ConvertMySQLTypeToSQLType(TypeInfoFirst, TypeInfo, Consettings.CPType);
  { the column type is ENUM}
  if TypeInfoFirst = 'enum' then
  begin
    PutSplitString(TypeInfoList, TypeInfoSecond, ',');
    for J := 0 to TypeInfoList.Count-1 do
      ColumnSize := Max(ColumnSize, Length(TypeInfoList.Strings[J]));
  end
  else
    { the column type is decimal }
    if ( ZFastCode.Pos(',', TypeInfoSecond) > 0 ) and not ( TypeInfoFirst = 'set' ) then
    begin
      TempPos := FirstDelimiter(',', TypeInfoSecond);
      ColumnSize := StrToIntDef(Copy(TypeInfoSecond, 1, TempPos - 1), 0);
      Precision := StrToIntDef(Copy(TypeInfoSecond, TempPos + 1,
        Length(TypeInfoSecond) - TempPos), 0);
    end
    else
    begin
      { the column type is other }
       if (TypeInfoSecond <> '') and not (TypeInfoFirst = 'set') then
          ColumnSize := StrToIntDef(TypeInfoSecond, 0)
       else if TypeInfoFirst = 'tinyint' then
          ColumnSize := 1
       else if TypeInfoFirst = 'smallint' then
          ColumnSize := 6
       else if TypeInfoFirst = 'mediumint' then
          ColumnSize := 6
       else if TypeInfoFirst = 'int' then
          ColumnSize := 11
       else if TypeInfoFirst = 'integer' then
          ColumnSize := 11
       else if TypeInfoFirst = 'bigint' then
          ColumnSize := 25
       else if TypeInfoFirst = 'int24' then
          ColumnSize := 25
       else if TypeInfoFirst = 'real' then
          ColumnSize := 12
       else if TypeInfoFirst = 'float' then
          ColumnSize := 12
       else if TypeInfoFirst = 'decimal' then
          ColumnSize := 12
       else if TypeInfoFirst = 'numeric' then
          ColumnSize := 12
       else if TypeInfoFirst = 'double' then
          ColumnSize := 22
       else if TypeInfoFirst = 'char' then
          ColumnSize := 1
       else if TypeInfoFirst = 'varchar' then
          ColumnSize := 255
       else if TypeInfoFirst = 'date' then
          ColumnSize := 10
       else if TypeInfoFirst = 'time' then
          ColumnSize := 8
       else if TypeInfoFirst = 'timestamp' then
          ColumnSize := 19
       else if TypeInfoFirst = 'datetime' then
          ColumnSize := 19
       else if TypeInfoFirst = 'tinyblob' then
          ColumnSize := 255
       else if TypeInfoFirst = 'blob' then
          ColumnSize := MAXBUF
       else if TypeInfoFirst = 'mediumblob' then
          ColumnSize := 16277215//may be 65535
       else if TypeInfoFirst = 'longblob' then
          ColumnSize := High(Integer)//2147483657//may be 65535
       else if TypeInfoFirst = 'tinytext' then
          ColumnSize := 255
       else if TypeInfoFirst = 'text' then
          ColumnSize := 65535
       else if TypeInfoFirst = 'mediumtext' then
          ColumnSize := 16277215 //may be 65535
       else if TypeInfoFirst = 'enum' then
          ColumnSize := 255
       else if TypeInfoFirst = 'set' then
          ColumnSize := 255;
    end;
    if FieldType in [stString, stUnicodeString] then
      ColumnSize := GetFieldSize(FieldType, consettings, ColumnSize,
        ConSettings.ClientCodePage.CharWidth, nil);

  FreeAndNil(TypeInfoList);
end;

function MySQLPrepareAnsiSQLParam(Handle: PZMySQLConnect; Value: TZVariant;
  const DefaultValue: String; ClientVarManager: IZClientVariantManager;
  PlainDriver: IZMySQLPlainDriver; const InParamType: TZSQLType;
  const UseDefaults: Boolean; ConSettings: PZConSettings): RawByteString;
var
  TempBytes: TBytes;
  TempBlob: IZBlob;
begin
  if ClientVarManager.IsNull(Value) then
    if UseDefaults and (DefaultValue <> '') then
      Result := ConSettings^.ConvFuncs.ZStringToRaw(DefaultValue,
        ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
    else
      Result := 'NULL'
  else
  begin
    case InParamType of
      stBoolean:
        if ClientVarManager.GetAsBoolean(Value) then
           Result := '''Y'''
        else
           Result := '''N''';
      stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
      stFloat, stDouble, stCurrency, stBigDecimal:
        Result := ClientVarManager.GetAsRawByteString(Value);
      stBytes:
        begin
          TempBytes := ClientVarManager.GetAsBytes(Value);
          Result := GetSQLHexAnsiString(PAnsiChar(TempBytes), Length(TempBytes));
        end;
      stString, stUnicodeString:
        Result := PlainDriver.EscapeString(Handle, ClientVarManager.GetAsRawByteString(Value), ConSettings, True);
      stDate:
        Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stTime:
        Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stTimestamp:
        Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            case InParamType of
              stBinaryStream:
                Result := GetSQLHexAnsiString(PAnsichar(TempBlob.GetBuffer), TempBlob.Length);
              else
                if TempBlob.IsClob then
                  Result := PlainDriver.EscapeString(Handle,
                    TempBlob.GetRawByteString, ConSettings, True)
                else
                  Result := PlainDriver.EscapeString(Handle,
                    GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                      TempBlob.Length, ConSettings), ConSettings, True);
            end;
          end
          else
            Result := 'NULL';
        end;
    end;
  end;
end;


end.
