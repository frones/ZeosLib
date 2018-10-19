{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for Oracle Database Connectivity Classes    }
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

unit ZTestDbcOracle;

interface
{$I ZDbc.inc}

{.$DEFINE EGONHUGEIST}
uses Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZSqlTestCase, ZDbcOracle,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcOracleCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
    function GetSupportedProtocols: string; override;
    //disabled tests:
    procedure TestStatement;
  published
    procedure TestConnection;
    procedure TestResultSet;
    procedure TestLongObjects;
    procedure TestPreparedStatement;
    procedure TestEmptyBlob;
    procedure TestNumbers;
    procedure TestLargeBlob;
    procedure TestDateWithTime;
    procedure TestFKError;
(*
    procedure TestDefaultValues;
*)
    {$IFDEF EGONHUGEIST}
    procedure TestVNU;
    {$ENDIF}
  end;


implementation

uses ZTestConsts, ZTestCase, ZVariant, ZSysUtils
  {$IFDEF EGONHUGEIST}, ZPLainOracleDriver, ZPlainOracleConstants,
  ZDbcOracleUtils, ZFastCode, ZDbcLogging{$ENDIF};

{ TZTestDbcOracleCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcOracleCase.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

{**
  Runs a test for Oracle database connection.
}
procedure TZTestDbcOracleCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Runs a test for regular Oracle DBC Statement.
}
procedure TZTestDbcOracleCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  try
    Statement.ExecuteUpdate('SELECT * FROM equipment');
    Fail('Incorrect ExecuteUpdate behaviour');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
  Statement.Close;
end;
{$IFDEF EGONHUGEIST}
procedure TZTestDbcOracleCase.TestVNU;
var
  SI1, SI2: Int64;
  UI1, UDI2: UInt64;
  D: Double;
  C1: Currency absolute Si1;
  C2: Currency absolute Si2;
  FPlainDriver: TZOraclePlainDriver;
  OCINumber: TOCINumber;
  FvnuInfo: TZvnuInfo;
  FErrorHandle: POCIError;
  Status: sword;
  fTinyBuffer: array[byte] of ansiChar;
  tmp: RawByteString;
  function NegNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo; const I2: Int64): Int64;
  var i: Byte;
  begin
    {$R-} {$Q-}
    { initialize with first negative base-100-digit }
    Result := -(101 - num[2]); //init
    { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
    for i := 3 to vnuInfo.Len do
      Result := Result * 100 - (101 - num[i]);
    I := (vnuInfo.Len-1)*2;
    if I <= vnuInfo.Precision then
      Result := Result * sPosScaleFaktor[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
    if Result <> I2 then
      Result := 0;
  end;
  function PosNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo; const I2: UInt64): UInt64; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  var i: Byte;
  begin
    {$R-} {$Q-}
    { initialize with first positive base-100-digit }
    Result := (num[2] - 1);
    { skip len, exponent and first base-100-digit -> start with 3}
    for i := 3 to vnuInfo.Len do
      Result := Result * 100 + Byte(num[i] - 1);
    I := (vnuInfo.Len-1)*2;
    if I <= vnuInfo.Precision then
      {$IFNDEF WITH_UINT64_C1118_ERROR}
      Result := Result * uPosScaleFaktor[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
      {$ELSE}
      Result := Result * UInt64(sPosScaleFaktor[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)]);
      {$ENDIF}
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
  end;
  function PosNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo; const C: Currency): Currency; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  var I64: Int64 absolute Result;
    i: ShortInt;
  begin
    {$R-} {$Q-}
    { initialize with first positive base-100-digit }
    I64 := (num[2] - 1);
    { skip len, exponent and first base-100-digit -> start with 3}
    for i := 3 to num[0] do
      i64 := i64 * 100 + Byte(num[i] - 1);
    I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
    if Result <> c  then
      Result := 0;
  end;
  function NegNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo; const C: Currency): Currency; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  var I64: Int64 absolute Result;
    i: ShortInt;
  begin
    {$R-} {$Q-}
    i64 := -(101 - num[2]); //init
    { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
    for i := 3 to vnuInfo.Len do
      i64 := i64 * 100 - (101 - num[i]);
    I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
    if Result <> c  then
      Result := 0;
  end;
  procedure Curr2Nvu2(num: POCINumber; const C: Currency; num2: TOCINumber);
  var
    d: ShortInt;
    l: byte;
    I: Int64 absolute C;
    Exp: Byte;
  begin
    { a currency allways fits into a 38 precision oracle number }
    {$R-} {$Q-}
    l := 0;
    Exp := 0 ;
    if C < 0 then begin
      while i <> 0 do begin
        d := I mod 100;
      //  i := i div 100;
      end;
      if (Exp = 0) and (d = 0) then
    end else begin

    end;
  end;
function PosNVUCurr2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
var i: Byte;
  PStart: PAnsiChar;
begin
  //FillChar(Buf^, 200, #0);
  PStart := Buf;
  //fill trailing zero
  case (vnuInfo.Scale-vnuInfo.Precision) of
    3:  begin
          PWord(Buf)^ := 12336;
          PWord(Buf+2)^ := 12336;
          Inc(Buf,4);
        end;
    2:  begin
          PByte(Buf)^ := Ord('0');
          PWord(Buf+2)^ := 12336;
          Inc(Buf,3);
        end;
    1:  begin
          PWord(Buf)^ := 12336;
          Inc(Buf,2);
        end;
    0: begin
          PByte(Buf+1)^ := Ord('0');
          Inc(Buf);
       end;
  end;
  if vnuInfo.FirstBase100DigitDiv10Was0
  then PByte(Buf)^ := Ord('0')+Byte(num[2] - 1)
  else PWord(Buf)^ := Word(TwoDigitLookupRaw[Byte(num[2] - 1)]);
  Inc(Buf, 1+Ord(vnuInfo.FirstBase100DigitDiv10Was0));
  for I := 3 to vnuInfo.Len do begin
    PWord(Buf)^ := Word(TwoDigitLookupRaw[Byte(num[i] - 1)]);
    Inc(Buf,2);
  end;
  I := Max(vnuInfo.Precision, vnuInfo.Scale)+Ord(vnuInfo.LastBase100DigitMod10Was0);
  {move the scaled digits one positon to right }
  PCardinal(Buf+1-i)^ := PCardinal(Buf-i)^;
  PByte(Buf-i)^ := Ord('.');
  if not (vnuInfo.LastBase100DigitMod10Was0 and (PByte(Buf-1)^ <> Ord('.'))) then
    Inc(Buf);
  Result := Buf - PStart{sign and decimal digit};
end;
  function NegNVUCurr2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
  var i: Byte;
    PStart: PAnsiChar;
  begin
    FillChar(Buf^, 200, #0);
    PStart := Buf;
    PByte(Buf)^ := Ord('-');
    //fill trailing zero's of required
    case (vnuInfo.Scale-vnuInfo.Precision) of
      3:  begin
            PWord(Buf+1)^ := 12336;
            PWord(Buf+3)^ := 12336;
            Inc(Buf,5);
          end;
      2:  begin
            PByte(Buf+1)^ := Ord('0');
            PWord(Buf+3)^ := 12336;
            Inc(Buf,4);
          end;
      1:  begin
            PWord(Buf+1)^ := 12336;
            Inc(Buf,3);
          end;
      0: begin
            PByte(Buf+1)^ := Ord('0');
            Inc(Buf,2);
         end;
      else Inc(Buf);
    end;
    if vnuInfo.FirstBase100DigitDiv10Was0 then begin
      PByte(Buf)^ := Ord('0')+Byte(101 - num[2]);
      Inc(Buf);
    end else begin
      PWord(Buf)^ := Word(TwoDigitLookupRaw[Byte(101 - num[2])]);
      Inc(Buf,2);
    end;
    for I := 3 to vnuInfo.Len do begin
      PWord(Buf)^ := Word(TwoDigitLookupRaw[Byte(101 - num[i])]);
      Inc(Buf,2);
    end;
    I := Max(vnuInfo.Precision, vnuInfo.Scale)+Ord(vnuInfo.LastBase100DigitMod10Was0);
    {move the scaled digits one positon to right }
    PCardinal(Buf+1-i)^ := PCardinal(Buf-i)^;
    PByte(Buf-i)^ := Ord('.');
    if not (vnuInfo.LastBase100DigitMod10Was0 and (PByte(Buf-1)^ <> Ord('.'))) then
      Inc(Buf);
    Result := Buf - PStart{sign and decimal digit};
  end;
begin
  Connection.Open;
  FErrorHandle := (Connection as IZOracleConnection).GetErrorHandle;
  FplainDriver := TZOraclePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  for SI1 := -1001{-99900} to High(Int64) do begin
    CheckOracleError(FPLainDriver, FErrorHandle,
      FPlainDriver.OCINumberFromInt(FErrorHandle, @SI1,
        SizeOf(INT64),OCI_NUMBER_SIGNED, POCINumber(@OCINumber)),
        lcOther, '', Connection.GetConSettings);
    if SI1 < 0 then begin
      Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuNegInt);
        SI2 := NegNvu2Int(POCINumber(@OCINumber), FvnuInfo, SI1);
        ZSetString(PAnsiChar(@FTinyBuffer[0]), NegOrdNVU2Raw(POCINumber(@OCINumber),FvnuInfo, @FTinyBuffer[0]), tmp);
        if tmp <> intToRaw(Si2) then begin
          NegOrdNVU2Raw(POCINumber(@OCINumber),FvnuInfo, @FTinyBuffer[0]);
          Assert(tmp = intToRaw(Si2), InttoUnicode(SI1));
        end;
      if Si1 <> si2 then begin
        Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuNegInt);
        SI2 := NegNvu2Int(POCINumber(@OCINumber), FvnuInfo, SI1);
        Assert(Si2 = SI1, InttoUnicode(SI1));
      end;
    end else if SI1 = 0 then
      Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = nvu0)
    else begin
        Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuPosInt);
        SI2 := PosNvu2Int(POCINumber(@OCINumber), FvnuInfo, SI1);
        Assert(SI2 = SI1, InttoUnicode(SI1));
        ZSetString(PAnsiChar(@FTinyBuffer[0]), PosOrdNVU2Raw(POCINumber(@OCINumber),FvnuInfo, @FTinyBuffer[0]), tmp);
        if tmp <> intToRaw(Si2) then begin
          NegOrdNVU2Raw(POCINumber(@OCINumber),FvnuInfo, @FTinyBuffer[0]);
          Assert(tmp = intToRaw(Si2), InttoUnicode(SI1));
        end;
      end;

    if not (SI1 mod 10000 = 0) then begin
      D := C1;
    CheckOracleError(FPLainDriver, FErrorHandle,
      FPlainDriver.OCINumberFromReal(FErrorHandle, @D,
        SizeOf(Double), POCINumber(@OCINumber)),
        lcOther, '', Connection.GetConSettings);
      if C1 < 0 then begin
        if nvuKind(POCINumber(@OCINumber), FvnuInfo) <> vnuNegCurr then
          Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuNegCurr);
            C2 := NegNvu2Curr(POCINumber(@OCINumber), FvnuInfo, c1);
        if C2 <> C1 then begin
          C2 := 0;
        Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuNegCurr);
          C2 := NegNvu2Curr(POCINumber(@OCINumber), FvnuInfo, c1);
          Assert(C2 = C1, 'Expect '+CurrtoUnicode(C1)+' was '+CurrtoUnicode(C2));
        end;
        NegNVUCurr2Raw(POCINumber(@OCINumber), FvnuInfo, @FTinyBuffer[0]);
      end else if C1 = 0 then
        Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = nvu0)
      else begin
        Assert(nvuKind(POCINumber(@OCINumber), FvnuInfo) = vnuPosCurr);
        C2 := PosNvu2Curr(POCINumber(@OCINumber), FvnuInfo, C1);
        if C2 <> C1 then begin
          C2 := 0;
          C2 := PosNvu2Curr(POCINumber(@OCINumber), FvnuInfo, C1);
          Assert(C2 = C1, 'Expect '+CurrtoUnicode(C1)+' was '+CurrtoUnicode(C2));
        end;
      end;
      //Curr2Nvu2(@OCINumber, C1, OCINumber);
    end;
  end;
end;
{$ENDIF EGONHUGEIST}

{**
  Runs a test for Oracle DBC ResultSet with stored results.
}
procedure TZTestDbcOracleCase.TestResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZDatabaseMetadata;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Metadata := Connection.GetMetadata;
  ResultSet := Metadata.GetPrimaryKeys('', 'ZEOSLIB', '');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM people');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment2');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM cargo');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery(
    'SELECT b_id, b_long, b_clob, b_blob FROM blob_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM binary_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for Oracle DBC PreparedStatement.
}
procedure TZTestDbcOracleCase.TestPreparedStatement;
const
  department_dep_id_Index = FirstDbcIndex;
  department_dep_name_Index = FirstDbcIndex+1;
  department_dep_shname_Index = FirstDbcIndex+2;
  department_dep_address_Index = FirstDbcIndex+3;
  people_p_id_Index = FirstDbcIndex;
  people_p_begin_work_Index = FirstDbcIndex+1;
var
  Statement: IZPreparedStatement;
  Statement1: IZPreparedStatement;
  ResultSet: IZResultSet;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  CheckNotNull(Statement);
  try
    Statement.SetInt(department_dep_id_Index, TEST_ROW_ID);
    Statement.ExecuteUpdatePrepared;

    Statement1 := Connection.PrepareStatement(
      'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
      + ' VALUES(?,?,?,?)');
    CheckNotNull(Statement1);
    try
      Statement1.SetInt(department_dep_id_Index, TEST_ROW_ID);
      Statement1.SetString(department_dep_name_Index, 'xyz');
      Statement1.SetNull(department_dep_shname_Index, stString);
      Stream := TStringStream.Create('abc'#10'def'#13'hgi');
      try
        Statement1.SetAsciiStream(department_dep_address_Index, Stream);
      finally
        Stream.Free;
      end;
      CheckEquals(1, Statement1.ExecuteUpdatePrepared);
    finally
      Statement1.Close;
    end;

    Statement.SetInt(department_dep_id_Index, TEST_ROW_ID);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
    Statement.ExecutePrepared;
    CheckEquals(0, Statement.GetUpdateCount);

    Statement1 := Connection.PrepareStatement(
      'SELECT count(*) FROM people WHERE p_id<>? AND p_begin_work<>?');
    CheckNotNull(Statement1);
    try
      Statement1.SetNull(people_p_id_Index, stInteger);
      Statement1.SetNull(people_p_begin_work_Index, stTimestamp);
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
//        CheckEquals(5, ResultSet.GetInt(people_count_Index));
        Check(not ResultSet.Next);
      finally
        ResultSet.Close;
      end;
    finally
      Statement1.Close;
    end;

    Statement1 := Connection.PrepareStatement('UPDATE people SET p_resume=?');
    CheckNotNull(Statement1);
    try
      Statement1.SetNull(FirstDbcIndex, stAsciiStream);
      CheckEquals(5, Statement1.ExecuteUpdatePrepared);
    finally
      Statement1.Close;
    end;
  finally
    Statement.Close;
  end;
end;

{**
  Special blob case: blob is not null, but its empty by length
}

procedure TZTestDbcOracleCase.TestEmptyBlob;
const
  update_blob_values_b_blob_Index = FirstDbcIndex;
  select_blob_values_b_blob_Index = FirstDbcIndex+1;
var
  Statement: IZPreparedStatement;
  Statement1: IZPreparedStatement;
  ResultSet: IZResultSet;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'update blob_values set b_blob=? where b_id = 1');
  CheckNotNull(Statement);
  Stream := TMemoryStream.Create; // empty stream
  try
    Statement.SetBinaryStream(update_blob_values_b_blob_Index, Stream);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);

    Statement1 := Connection.PrepareStatement(
      'select b_id, b_blob from blob_values order by b_id');
    CheckNotNull(Statement1);
    try
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
        Check(not ResultSet.IsNull(select_blob_values_b_blob_Index));
        CheckEquals(0, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length');
        Check(ResultSet.Next);
        CheckEquals(20, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length (2)');
      finally
        ResultSet.Close;
      end;
    finally
      Statement1.Close;
    end;
  finally
    Statement.Close;
    Stream.Free;
  end;
end;



(*
{**
  Runs a test for Oracle default values.
}
procedure TZTestDbcOracleCase.TestDefaultValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from default_values');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(5), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(6), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(7), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

*)

{**
  Runs a test for Oracle long objects.
}
procedure TZTestDbcOracleCase.TestLongObjects;
const
  blob_values_b_id_Index = FirstDbcIndex;
  blob_values_b_long_Index = FirstDbcIndex+1;
  blob_values_b_clob_Index = FirstDbcIndex+2;
  blob_values_b_blob_Index = FirstDbcIndex+3;
  binary_values_n_id_Index = FirstDbcIndex;
  binary_values_n_raw_Index = FirstDbcIndex+1;
  binary_values_n_longraw_Index = FirstDbcIndex+2;
  binary_values_n_blob_Index = FirstDbcIndex+3;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
//  Statement.SetResultSetType(rtScrollInsensitive);
//  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery(
    'SELECT b_id, b_long, b_clob, b_blob FROM blob_values ORDER BY b_id');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  CheckEquals(1, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_long_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_clob_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_blob_Index));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(blob_values_b_long_Index).GetString);
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(blob_values_b_clob_Index).GetString);
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 = String(ResultSet.GetBlob(blob_values_b_blob_Index).GetString), 'Comparision of binary strings failed.');

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(blob_values_b_long_Index).GetString));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(blob_values_b_clob_Index).GetString));
  Check(ResultSet.IsNull(blob_values_b_blob_Index));

  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery(
    'SELECT n_id, n_raw, n_longraw, n_blob FROM binary_values ORDER BY n_id');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  CheckEquals(1, ResultSet.GetInt(binary_values_n_id_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_raw_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_longraw_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_blob_Index));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(binary_values_n_id_Index));
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 = {$IFDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(BytesToStr(ResultSet.GetBytes(binary_values_n_raw_Index))), 'Second comparision of binary strings failed');
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 = String(ResultSet.GetBlob(binary_values_n_longraw_Index).GetString), 'Third comparision of binary strings failed');
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 = String(ResultSet.GetBlob(binary_values_n_blob_Index).GetString), 'Fourth comparision of binary strings failed');

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(binary_values_n_id_Index));
  Check(ResultSet.IsNull(binary_values_n_raw_Index));
  Check(ResultSet.IsNull(binary_values_n_longraw_Index));
  Check(ResultSet.IsNull(binary_values_n_blob_Index));

  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Test number datatype reading
}

procedure TZTestDbcOracleCase.TestNumbers;
const
  number_values_n_id_Index = FirstDbcIndex;
  number_values_n_tint_Index = FirstDbcIndex+1;
  number_values_n_sint_Index = FirstDbcIndex+2;
  number_values_n_int_Index = FirstDbcIndex+3;
  number_values_n_bdecimal_Index = FirstDbcIndex+4;
  number_values_n_numeric_Index = FirstDbcIndex+5;
  number_values_n_float_Index = FirstDbcIndex+6;
  number_values_n_real_Index = FirstDbcIndex+7;
  number_values_n_dprecision_Index = FirstDbcIndex+8;
  number_values_n_money_Index = FirstDbcIndex+9;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery(
    'SELECT * FROM number_values order by 1');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  // 1, -128,-32768,-2147483648,-9223372036854775808, -99999.9999
  // -3.402823466E+38, -3.402823466E+38, -1.7976931348623157E+38, -21474836.48
  CheckEquals(1, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(-128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(-32768, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(Low(LongInt), ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(Low(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(Low(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(-99999.9999, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-99999.9999, ResultSet.GetCurrency(number_values_n_numeric_Index));
  {EH: oracle uses the number here, so we can't compare any other way except using a string or simply forget it
    it's officially documented the number is not accurate for FPU floats}
  CheckEquals('-3.402823466E38', ResultSet.GetString(number_values_n_float_Index));
  CheckEquals('-3.402823466E38', ResultSet.GetString(number_values_n_real_Index));
  //CheckEquals(-1.7976931348623157E38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-21474836.48, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //2,-128,-32768,-2147483648,-9223372036854775808, -11111.1111,
	//-1.175494351E-38, -1.175494351E-38, -2.2250738585072014E-38, 21474836.47
  CheckEquals(2, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(-128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(-32768, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(Low(LongInt), ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(Low(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(Low(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(-11111.1111, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-11111.1111, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(-1.175494351E-38, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(-1.175494351E-38, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(-2.2250738585072014E-38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(21474836.47, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //3, 0, 0, 0, 0, 0, 0, 0, 0, '0'
  CheckEquals(3, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(0, ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(0, ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(0, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(0, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(0, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(0, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(0, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(0, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //4, 128, 32767, 2147483647, 9223372036854775807, 11111.1111,
	//3.402823466E+38, 3.402823466E+38, 1.7976931348623157E+38, -922337203685477.5808
  CheckEquals(4, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(32767, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(2147483647, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(High(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(High(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(11111.1111, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(11111.1111, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals('3.402823466E38', ResultSet.GetString(number_values_n_float_Index));
  CheckEquals('3.402823466E38', ResultSet.GetString(number_values_n_real_Index));
  //CheckEquals(1.7976931348623157E+38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-922337203685477.5808, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //5, 128, 32767, 147483647, 9223372036854775807,  99999.9999,
	//1.175494351E-38, 1.175494351E-38, 2.2250738585072014E-38, 922337203685477.5807
  CheckEquals(5, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(32767, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(147483647, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(High(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(High(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(99999.9999, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(99999.9999, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(1.175494351E-38, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(1.175494351E-38, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(2.2250738585072014E-38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(922337203685477.5807, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(not ResultSet.Next);
end;

{**
  Test the large amount data in blob
}

procedure TZTestDbcOracleCase.TestLargeBlob;
const
  insert_blob_values_b_blob_Index = FirstDbcIndex;
  select_blob_values_b_blob_Index = FirstDbcIndex+1;
var
  InStm: TMemoryStream;
  OutBytes: TBytes;
  i, TestSize: Integer;
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  InStm := TMemoryStream.Create;
  try
    TestSize := 1050 * 1024 + Random(100000); // relative big random size
    InStm.SetSize(TestSize);
    // randomizing content
    i := 0;
    while i < TestSize do begin
      {$R-} //EH range check does overrun the defined TByteArray = array[0..32767] of Byte range -> turn off!
      PByteArray(InStm.Memory)[i] := Random(256);
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      Inc(i, Random(1000));
    end;
    // inserting
    PStatement := Connection.PrepareStatement(
      Format('insert into blob_values(b_id, b_blob) values (%d, ?)', [TEST_ROW_ID]));
    CheckNotNull(PStatement);
    PStatement.SetBinaryStream(insert_blob_values_b_blob_Index, InStm);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');
    PStatement.Close;

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);

    ResultSet := Statement.ExecuteQuery(
      'SELECT b_id, b_blob FROM blob_values where b_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEquals(TestSize, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length');
    OutBytes := ResultSet.GetBytes(select_blob_values_b_blob_Index);
    CheckEquals(TestSize, Length(OutBytes), 'Wrong blob bytes length');
    CheckEqualsMem(InStm.Memory, @OutBytes[0], TestSize, 'Wrong blob content (byte array)');
  finally
    InStm.Free;

    PStatement := Connection.PrepareStatement(
    'DELETE FROM blob_values WHERE b_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{**
  Test oracle DATE type precission is 1 second
}

procedure TZTestDbcOracleCase.TestDateWithTime;
const
  param_d_date_Index = FirstDbcIndex;
  field_d_date_Index = FirstDbcIndex+1;
var
  TestDate: TDateTime;
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  TestDate := EncodeDate(2009, 12, 20) + EncodeTime(20, 09, 11, 0);
  try
    // inserting
    PStatement := Connection.PrepareStatement(
      Format('insert into date_values(d_id, d_date) values(%d, ?)', [TEST_ROW_ID]));
    CheckNotNull(PStatement);
    PStatement.SetTimestamp(param_d_date_Index, TestDate);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');
    PStatement.Close;

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    ResultSet := Statement.ExecuteQuery(
      'SELECT d_id, d_date FROM date_values where d_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEqualsDate(TestDate, ResultSet.GetTimestamp(field_d_date_Index), [dpYear..dpSec], 'DATE type must have 1 sec precission');
  finally
    PStatement := Connection.PrepareStatement(
    'DELETE FROM date_values WHERE d_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{**
  Test PK-error and possible prepared statement corruption after it
}

procedure TZTestDbcOracleCase.TestFKError;
const
  TestStr = 'The source code of the ZEOS Libraries and packages are distributed under the Library GNU General Public License';
  s_id_Index = FirstDbcIndex;
  s_varchar_Index = FirstDbcIndex+1;
var
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  // inserting
  PStatement := Connection.PrepareStatement(
    'insert into string_values(s_id, s_varchar) values(?, ?)');
  CheckNotNull(PStatement);
  // making PK error
  PStatement.SetInt(s_id_Index, 1);
  PStatement.SetNull(s_varchar_Index, stString);  // null clears variable memory ref
  try
    PStatement.ExecuteUpdatePrepared;
    Fail('Primary key violation expected');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  // rerun with new value (and check, that prev error dont corrupt PStatement)
  try
    PStatement.SetInt(s_id_Index, TEST_ROW_ID);
    PStatement.SetString(s_varchar_Index, TestStr);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    ResultSet := Statement.ExecuteQuery(
      'SELECT s_id, s_varchar FROM string_values where s_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEquals(TestStr, ResultSet.GetString(s_varchar_Index));
  finally
    PStatement := Connection.PrepareStatement(
    'DELETE FROM string_values WHERE s_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcOracleCase.Suite);
end.
