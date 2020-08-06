{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Sybase SQL Anywhere Connectivity Classes         }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcASAUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, FmtBCD,
  ZSysUtils, ZVariant, ZCompatibility,
  ZPlainASADriver, ZDbcIntfs, ZDbcLogging, ZDbcASA;

const
  StdVars = 20;
  MinBLOBSize = 256;
  BlockSize = 20;

type
  { ASA Error Class}
  EZASAConvertError = class(Exception);

  { Base interface for sqlda }
  IZASASQLDA = interface
    ['{7606E8EB-9FC8-4F76-8D91-E23AB96409E1}']
    function CreateException(const Msg: string): EZSQLException;
    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure ReadBlobToString(const Index: Word; out str: RawByteString);
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }
  TZASASQLDA = class (TInterfacedObject, IZASASQLDA)
  private
    FConSettings: PZConSettings;
    FSQLDA: PASASQLDA;
    FPlainDriver: TZASAPlainDriver;
    FHandle: PZASASQLCA;
    FCursorName: PAnsiChar;
    FConnection: IZASAConnection;
    function CreateException(const  Msg: string): EZSQLException;
    procedure CheckIndex(const Index: Word);
    procedure CheckRange(const Index: Word);
    procedure SetFieldType(const Index: Word; ASAType: Smallint; Len: LongWord); overload;
    procedure SetFieldType(ToSQLDA: PASASQLDA; const Index: Word; ASAType: Smallint; Len: LongWord); overload;
  protected
    procedure ReadBlob(const Index: Word; var Buffer: Pointer; Length: LongWord);
  public
    constructor Create(const Connection: IZASAConnection;
      CursorName: PAnsiChar; NumVars: Word = StdVars);
    destructor Destroy; override;

    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): Integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure ReadBlobToString(const Index: Word; out str: RawByteString);
  end;

{**
  Converts a ASA native type into ZDBC SQL types.
  @param FieldHandle a handler to field description structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType(const SQLType: SmallInt): TZSQLType;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;

function ConvertASAJDBCToSqlType(const FieldType: SmallInt): TZSQLType;

const SQLType2ASATypeMap: array[TZSQLType] of SmallInt =
  (DT_NOTYPE, //
    //fixed size DataTypes first
    DT_BIT,
    DT_TINYINT, DT_SMALLINT{no signed tiny?}, DT_UNSSMALLINT, DT_SMALLINT, DT_UNSINT, DT_INT, DT_UNSBIGINT, DT_BIGINT,  //ordinals
    DT_FLOAT, DT_DOUBLE, DT_DECIMAL, DT_DECIMAL, //floats
    DT_TIMESTAMP_STRUCT, DT_TIMESTAMP_STRUCT, DT_TIMESTAMP_STRUCT,
    DT_FIXCHAR, //SQLDataType UNIQUEIDENTIFIERSTR
    //now varying size types in equal order
    DT_VARCHAR, DT_NVARCHAR, DT_BINARY,
    DT_LONGVARCHAR, DT_LONGNVARCHAR, DT_LONGBINARY,
    //finally the object types
    DT_NOTYPE, DT_NOTYPE);
const SQLType2ASASizeMap: array[TZSQLType] of SmallInt =
  (0, //
    //fixed size DataTypes first
    SizeOf(Byte),
    SizeOf(Byte), SizeOf(SmallInt){no signed tiny?}, SizeOf(Word), SizeOf(SmallInt), SizeOf(Integer), SizeOf(Cardinal), SizeOf(UInt64), SizeOf(Int64),  //ordinals
    SizeOf(Single), SizeOf(Double), 16, 16, //float's/BCD's
    SizeOf(TZASASQLDateTime), SizeOf(TZASASQLDateTime), SizeOf(TZASASQLDateTime),
    36, //SQLDataType UNIQUEIDENTIFIERSTR
    //now varying size types in equal order
    1, 1, 1,
    SizeOf(TZASABlobStruct)-1, SizeOf(TZASABlobStruct)-1, SizeOf(TZASABlobStruct)-1,
    //finally the object types
    0, SizeOf(TZASABlobStruct)-1);

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses Variants, Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZMessages, ZEncoding, ZDbcUtils;

{ TZASASQLDA }

function TZASASQLDA.CreateException(const Msg: string): EZSQLException;
begin
  DriverManager.LogError(lcOther, 'ASA', '', -1, Msg);
  Result := EZSQLException.Create(Format( SSQLError1, [Msg]));
end;

{**
   Check range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZASASQLDA.CheckIndex(const Index: Word);
begin
  Assert( Assigned( FSQLDA), 'SQLDA not initialized.');
  Assert( Index < Word(FSQLDA.sqld), 'Out of Range.');
end;

procedure TZASASQLDA.CheckRange(const Index: Word);
begin
  CheckIndex( Index);
  Assert( Assigned( FSQLDA.sqlVar[ Index].sqlData),
    'No memory for variable in SQLDA.');
end;

procedure TZASASQLDA.SetFieldType(ToSQLDA: PASASQLDA; const Index: Word;
  ASAType: Smallint; Len: LongWord);
begin
  CheckIndex(Index);
  with ToSQLDA.sqlvar[Index] do
  begin
    if ( ASAType and $FFFE = DT_LONGBINARY) or
       ( ASAType and $FFFE = DT_LONGNVARCHAR) or
       ( ASAType and $FFFE = DT_LONGVARCHAR) then
    begin
      if Assigned( sqlData) then
        ReallocMem( sqlData, SizeOf(TZASABlobStruct)+Len)
      else
        GetMem( sqlData, SizeOf( TZASABlobStruct)+Len);
      PZASABlobStruct( sqlData).array_len := Len;
      PZASABlobStruct( sqlData).stored_len := 0;
      PZASABlobStruct( sqlData).untrunc_len := 0;
      PZASABlobStruct( sqlData).arr[0] := AnsiChar(#0);
      Len := SizeOf( TZASABlobStruct)-1;
      //Inc( Len, SizeOf( TZASABlobStruct)-1);
    end
    else
    begin
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        Inc( Len, SizeOf( TZASASQLSTRING));
      if Assigned( sqlData) then
        ReallocMem( sqlData, Len)
      else
        GetMem( sqlData, Len);
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        PZASASQLSTRING( sqlData).length := 0;
    end;
    sqlType := ASAType;
    sqllen := Len;
  end;
end;

procedure TZASASQLDA.SetFieldType(const Index: Word; ASAType: Smallint;
  Len: LongWord);
begin
  SetFieldType(FSQLDA, Index, ASAType, Len);
end;

constructor TZASASQLDA.Create(const Connection: IZASAConnection;
   CursorName: PAnsiChar; NumVars: Word = StdVars);
begin
  FConnection := Connection;
  FPlainDriver := Connection.GetPlainDriver;
  FHandle := Connection.GetDBHandle;
  FCursorName := CursorName;
  AllocateSQLDA(NumVars);
  FConSettings := Connection.GetConSettings;
  inherited Create;
end;

destructor TZASASQLDA.Destroy;
begin
  FreeSQLDA;
  inherited;
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
procedure TZASASQLDA.AllocateSQLDA( NumVars: Word);
begin
  FreeSQLDA;
  FSQLDA := FPlainDriver.alloc_sqlda( NumVars);
  if not Assigned( FSQLDA) then
    raise CreateException( 'Not enough memory for SQLDA');
end;

{**
   Allocate memory for SQLVar in SQLDA structure for every
   fields by it length.
}
procedure TZASASQLDA.InitFields;
var
  i: Integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqld-1 do
    begin
      case FSQLDA.sqlVar[i].sqlType and $FFFE of
        DT_DATE,
        DT_TIME,
        DT_TIMESTAMP:
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_TIMESTAMP_STRUCT +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := SizeOf( TZASASQLDateTime);
                        end;
        DT_DECIMAL: //it's recommended by SYB to use strings instead of the packed undefined decimal value
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_VARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := FmtBCD.MaxFMTBcdFractionSize+2;//sign and dot
                        end;
        DT_STRING,
        DT_FIXCHAR,
        DT_VARCHAR:     FSQLDA.sqlVar[i].sqlType := DT_VARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
        DT_LONGVARCHAR: begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
        DT_BINARY:      FSQLDA.sqlVar[i].sqlType := DT_BINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
        DT_LONGBINARY:  begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGBINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
        DT_NSTRING,
        DT_NFIXCHAR,
        DT_NVARCHAR:    FSQLDA.sqlVar[i].sqlType := DT_NVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
        DT_LONGNVARCHAR:begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGNVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
      end;
      SetFieldType( i, FSQLDA.sqlVar[i].sqlType, FSQLDA.sqlVar[i].sqlLen);
    end;
  end;
end;

{**
   Clear allocated data for SQLDA parameters
}
procedure TZASASQLDA.FreeSQLDA;
var
  i: integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqln-1 do
    begin
      FSQLDA.sqlVar[i].sqlInd := nil;
      if Assigned( FSQLDA.sqlVar[i].sqlData) then
      begin
        FreeMem( FSQLDA.sqlVar[i].sqlData);
        FSQLDA.sqlVar[i].sqlData := nil;
      end;
    end;
    FPlainDriver.free_sqlda( FSQLDA);
    FSQLDA := nil;
  end;
end;

{**
   Return pointer to SQLDA structure
}
function TZASASQLDA.GetData: PASASQLDA;
begin
  Result := FSQLDA;
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZASASQLDA.IsBlob(const Index: Word): boolean;
begin
  Result := GetFieldSqlType( Index) in
    [ stAsciiStream, stUnicodeStream, stBinaryStream];
end;

{**
   Indicate nullable field
   @param Index the index fields
   @return true if field nullable overwise false
}
function TZASASQLDA.IsNullable(const Index: Word): boolean;
begin
  CheckIndex(Index);
  Result := FSQLDA.sqlvar[Index].sqlType and 1 = 1
end;

{**
   Get fields count not allocated.
   @return fields count
}
function TZASASQLDA.GetFieldCount: Integer;
begin
  if Assigned( FSQLDA) then
    Result := FSQLDA.sqld
  else
    Result := 0;
end;

{**
   Return Name for field
   @param Index the index fields
   @return the name
}
function TZASASQLDA.GetFieldName(const Index: Word): String;
begin
  CheckIndex(Index);
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FSQLDA.sqlvar[Index].sqlname.data[0],
    FSQLDA.sqlvar[Index].sqlname.length-1, FConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not FConSettings^.AutoEncode) or (FConSettings^.ClientCodePage^.CP = FConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FSQLDA.sqlvar[Index].sqlname.data[0]), FSQLDA.sqlvar[Index].sqlname.length-1)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FSQLDA.sqlvar[Index].sqlname.data[0],
        FSQLDA.sqlvar[Index].sqlname.length-1, FConSettings^.ClientCodePage^.CP), FConSettings^.CTRL_CP);
  {$ENDIF}
end;

{**
   Return field index by it name
   @param Index the index fields
   @return the index field
}
function TZASASQLDA.GetFieldIndex(const Name: String): Word;
var FieldName: String;
  P1, P2: PChar;
begin
  for Result := 0 to FSQLDA.sqld - 1 do begin
    FieldName := GetFieldName(Result);
    P1 := Pointer(Name);
    P2 := Pointer(FieldName);
    if Length(FieldName) = Length(name) then
      if StrLIComp(P1, P2, Length(name)) = 0 then
        Exit;
  end;
  raise CreateException( Format( SFieldNotFound1, [name]));
  Result := 0; // satisfy compiler
end;

{**
   Return field length
   @param Index the index fields
   @return the field lenth
}
function TZASASQLDA.GetFieldLength(const Index: Word): Word;
begin
  CheckIndex( Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := FSQLDA.sqlvar[Index].sqlLen
  else
    Result := (FSQLDA.sqlvar[Index].sqlLen and $FF);//EH commented no idea for what shr 1 + 1; //shr 1 = div 2 but faster
end;

{**
   Return field scale
   @param Index the index fields
   @return the field scale
}
function TZASASQLDA.GetFieldScale(const Index: Word): integer;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := 0
  else
    Result := FSQLDA.sqlvar[Index].sqlLen div 256;
end;

{**
   Convert ASA sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZASASQLDA.GetFieldSqlType(const Index: Word): TZSQLType;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_TIMESTAMP_STRUCT then
    Result := ConvertASATypeToSQLType(FSQLDA.sqlvar[Index].sqlType)
  else
    Result := ConvertASATypeToSQLType(PSmallInt(PAnsiChar(FSQLDA.sqlvar[Index].sqlData)+SizeOf(TZASASQLDateTime))^)
end;

procedure TZASASQLDA.ReadBlob(const Index: Word; var Buffer: Pointer;
  Length: LongWord);
var
  TempSQLDA: PASASQLDA;
  Offs, Rd: LongWord;
const
  BlockSize = 32700;
begin
  with FSQLDA.sqlvar[Index] do
  begin
    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGNVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) and
       ( PZASABlobStruct( sqlData).array_len > 0) then
    begin
      Assert( PZASABlobStruct( sqlData).array_len = PZASABlobStruct( sqlData).untrunc_len,
        'Blob Record is not correctly initialized');
      if PZASABlobStruct( sqlData).array_len <> Length then
        raise CreateException( 'Could''nt complete BLOB-Read');
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move( PZASABlobStruct( sqlData).arr[0], Buffer, PZASABlobStruct( sqlData).array_len);
    end
    else
    begin
      TempSQLDA := FPlainDriver.alloc_sqlda( 1);
      if not Assigned( TempSQLDA) then
        raise CreateException( 'Not enough memory for SQLDA');
      try
        with TempSQLDA.sqlvar[ 0] do
        begin
          case Self.GetFieldSqlType(Index) of
            stAsciiStream:
              SetFieldType(TempSQLDA, 0, DT_LONGVARCHAR, Min( Int64(BlockSize), Int64(Length)));
            stUnicodeStream:
              SetFieldType(TempSQLDA, 0, DT_LONGNVARCHAR, Min( Int64(BlockSize), Int64(Length)));
            stBinaryStream:
              SetFieldType(TempSQLDA, 0, DT_LONGBINARY, Min( Int64(BlockSize), Int64(Length)));
            else
              sqlType := DT_FIXCHAR;
          end;
          sqlname.length := 0;
          sqlname.data[0] := AnsiChar(#0);
          TempSQLDA.sqld := TempSQLDA.sqln;

          Offs := 0;
          Rd := 0;

          while True do begin
            FPlainDriver.dbpp_get_data(FHandle, FCursorName, Index + 1, Offs, TempSQLDA, 0);
            if FHandle.sqlCode <> SQLE_NOERROR then
               FConnection.HandleErrorOrWarning(lcOther, 'dbpp_get_data', FConnection);
            if ( sqlind^ < 0 ) then
              break;
            Inc( Rd, PZASABlobStruct( sqlData)^.stored_len);
            if Offs = 0 then ReallocMem(Buffer, PZASABlobStruct( sqlData)^.untrunc_len+Byte(ORd(sqlType and $FFFE <> DT_LONGBINARY))); //keep 1 byte for trailing #0 term
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move((PZASABlobStruct( sqlData)^.arr[0]), (PAnsiChar(Buffer)+Offs)^, PZASABlobStruct( sqlData)^.stored_len);
            if ( sqlind^ = 0 ) or ( RD = Length) then
              break;
            Inc( Offs, PZASABlobStruct( sqlData)^.stored_len);
            sqllen := Min( Int64(BlockSize), Int64(Length-Rd));
          end;
          if Rd <> Length then
            raise CreateException( 'Could''nt complete BLOB-Read');
          FreeMem(sqlData);
          FPlainDriver.free_sqlda( TempSQLDA);
          TempSQLDA := nil;
        end;
      except
        if Assigned( TempSQLDA) then
          FPlainDriver.free_sqlda( TempSQLDA);
        raise;
      end;
    end;
  end;
end;

{**
   Read blob data to string
   @param Index an filed index
   @param Str destination string
}
procedure TZASASQLDA.ReadBlobToString(const Index: Word; out Str: RawByteString);
var Buffer: Pointer;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Str := EmptyRaw;
    if (sqlind^ < 0) then
       Exit;

    case sqlType and $FFFE of
      DT_LONGBINARY, DT_LONGVARCHAR, DT_LONGNVARCHAR:
      begin
        GetMem(Buffer, PZASABlobStruct( sqlData).untrunc_len);
        SetLength( Str, PZASABlobStruct( sqlData).untrunc_len);
        ReadBlob(Index, Buffer, PZASABlobStruct( sqlData).untrunc_len);
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Str)^, PZASABlobStruct( sqlData).untrunc_len);
        FreeMem(buffer);
      end else
        raise CreateException( Format( SErrorConvertionField,
          [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
  Converts a ASA native types into ZDBC SQL types.
  @param SQLType Field of TASASQLVar structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType(const SQLType: SmallInt): TZSQLType;
begin
  case SQLType and $FFFE of
    DT_NOTYPE:
      Result := stUnknown;
    DT_SMALLINT:
      Result := stSmall;
    DT_INT:
      Result := stInteger;
    DT_DECIMAL:
      Result := stBigDecimal;
    DT_FLOAT:
      Result := stFloat;
    DT_DOUBLE:
      Result := stDouble;
    DT_DATE:
      Result := stDate;
    DT_VARIABLE, DT_STRING, DT_FIXCHAR, DT_VARCHAR:
      Result := stString;
    DT_NSTRING, DT_NFIXCHAR, DT_NVARCHAR:
                      Result := stUnicodeString; //just tag it
    DT_LONGNVARCHAR:  Result := stUnicodeStream; //just tag it
    DT_LONGVARCHAR:   Result := stAsciiStream;
    DT_TIME:
      Result := stTime;
    DT_TIMESTAMP:
      Result := stTimestamp;
    DT_TIMESTAMP_STRUCT:
      Result := stTimestamp;
    DT_BINARY:
      Result := stBytes;
    DT_LONGBINARY:
      Result := stBinaryStream;
    DT_TINYINT:
      Result := stByte;
    DT_BIGINT:
      Result := stLong;
    DT_UNSINT:
      Result := stInteger;
    DT_UNSSMALLINT:
      Result := stSmall;
    DT_UNSBIGINT:
      Result := stLong;
    DT_BIT:
      Result := stBoolean;
  else
    Result := stUnknown;
  end;
end;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;
begin
  case SQLType and $FFFE of
    DT_SMALLINT:
      Result := 'DT_SMALLINT';
    DT_INT:
      Result := 'DT_INT';
    DT_DECIMAL:
      Result := 'DT_DECIMAL'; //BCD Fields not supported
    DT_FLOAT:
      Result := 'DT_FLOAT';
    DT_DOUBLE:
      Result := 'DT_DOUBLE';
    DT_DATE:
      Result := 'DT_DATE';
    DT_VARIABLE:
      Result := 'DT_VARIABLE';
    DT_STRING:
      Result := 'DT_STRING';
    DT_FIXCHAR:
      Result := 'DT_FIXCHAR';
    DT_VARCHAR:
      Result := 'DT_VARCHAR';
    DT_LONGVARCHAR:
      Result := 'DT_LONGVARCHAR';
    DT_TIME:
      Result := 'DT_TIME';
    DT_TIMESTAMP:
      Result := 'DT_TIMESTAMP';
    DT_TIMESTAMP_STRUCT:
      Result := 'DT_TIMESTAMP_STRUCT';
    DT_BINARY:
      Result := 'DT_BINARY';
    DT_LONGBINARY:
      Result := 'DT_LONGBINARY';
    DT_TINYINT:
      Result := 'DT_TINYINT';
    DT_BIGINT:
      Result := 'DT_BIGINT';
    DT_UNSINT:
      Result := 'DT_UNSINT';
    DT_UNSSMALLINT:
      Result := 'DT_UNSSMALLINT';
    DT_UNSBIGINT:
      Result := 'DT_UNSBIGINT';
    DT_BIT:
      Result := 'DT_BIT';
    DT_NSTRING:
      Result := 'DT_NSTRING';
    DT_NFIXCHAR:
      Result := 'DT_NFIXCHAR';
    DT_NVARCHAR:
      Result := 'DT_NVARCHAR';
    DT_LONGNVARCHAR:
      Result := 'DT_LONGNVARCHAR';
  else
    Result := 'Unknown';
  end;
end;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertASAJDBCToSqlType(const FieldType: SmallInt): TZSQLType;
begin
  case FieldType of
    1, 12, -8, -9:
      Result := stString;
    -7: Result := stBoolean;
    -6: Result := stByte;
    5: Result := stSmall;
    4: Result := stInteger;
    -5 : Result := stLong;
    6, 7, 8: Result := stDouble;
    2, 3: Result := stDouble;  //BCD Feld
    11, 93: Result := stTimestamp;
    -1: Result := stAsciiStream;
    -10: Result := stAsciiStream;//stUnicodeStream;
    -4, -11, 1111: Result := stBinaryStream;
    -3, -2: Result := stBytes;
    92: Result := stTime;
    91: Result := stDate;
  else
    Result := stUnknown;
  end;
end;

{$ENDIF ZEOS_DISABLE_ASA}
end.

