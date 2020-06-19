{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for Interbase Database Connectivity Classes   }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZTestDbcInterbase;

interface
{$I ZDbc.inc}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcInterbase6, ZSqlTestCase,
  ZCompatibility, DateUtils, Types;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcInterbaseCase = class(TZAbstractDbcSQLTestCase)
  private
    hl_idArray: TIntegerDynArray;
    stBooleanArray: TBooleanDynArray;
    stByteArray: TByteDynArray;
    stShortArray: TShortIntDynArray;
    stLongArray: TInt64DynArray;
    stIntegerArray: TIntegerDynArray;
    stFloatArray: TSingleDynArray;
    stDoubleArray: TDoubleDynArray;
    stBigDecimalArray: TExtendedDynArray;
    stStringArray: TRawByteStringDynArray;
    stUnicodeStringArray: TUnicodeStringDynArray;
    stBytesArray: TBytesDynArray;
    stDateArray: TDateTimeDynArray;
    stTimeArray: TDateTimeDynArray;
    stTimeStampArray: TDateTimeDynArray;
    stGUIDArray: TGUIDDynArray;
    stAsciiStreamArray: TZCharRecDynArray;
    stUnicodeStreamArray: TUTF8StringDynArray;
    stBinaryStreamArray: TInterfaceDynArray;
    stBooleanNullArray: array of TBooleanDynArray;
    stByteNullArray: array of TByteDynArray;
    stShortNullArray: array of TShortIntDynArray;
    stWordNullArray: array of TWordDynArray;
    stSmallNullArray: array of TSmallIntDynArray;
    stLongWordNullArray: array of TLongWordDynArray;
    stIntegerNullArray: array of TIntegerDynArray;
    stULongNullArray: array of TUInt64DynArray;
    stLongNullArray: array of TInt64DynArray;
    stFloatNullArray: array of TSingleDynArray;
    stDoubleNullArray: array of TDoubleDynArray;
    stCurrencyNullArray: array of TCurrencyDynArray;
    stBigDecimalNullArray: array of TExtendedDynArray;
    stStringNullArray: array of TRawByteStringDynArray;
    stUnicodeStringNullArray: array of TUnicodeStringDynArray;
  protected
    function GetSupportedProtocols: string; override;
    procedure InternalTestArrayBinding(PStatement: IZPreparedStatement;
      FirstID, ArrayLen: Integer);
  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestRegularResultSet;
    procedure TestBlobs;
    procedure TestUpdateBlobs;
    procedure TestCaseSensitive;
    procedure TestDefaultValues;
    procedure TestDomainValues;
    procedure TestStoredprocedures;
    procedure TestMsec;
    procedure TestEmptyStrings;
    procedure TestArrayBindings;
    procedure TestClientVersionNumber;
    procedure TestDefaultReadCommittedMode;
    procedure FB_TestUpdateCounts;
    procedure FB_TestUpdateCounts_Returning;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_A;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_B;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_C;
    procedure TestLongStatements;
  end;

implementation

uses SysUtils, ZTestConsts, ZTestCase, ZDbcResultSet, ZVariant, ZMessages,
  ZDbcInterbase6Metadata, DB;

{ TZTestDbcInterbaseCase }

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_A;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_A');
    //Check(Stmt.GetUpdateCount = 0, 'updatecount is not equal');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_B;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_B');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    //Check(Stmt.GetUpdateCount = Cnt, 'updatecount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_C;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_B');
//    Check(Stmt.GetUpdateCount = Cnt, 'updatecount is not equal');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_Returning;
var Stmt: IZStatement;
  Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  try
    CheckNotNull(Stmt);
    Cnt := Stmt.ExecuteUpdate('update people set p_id = p_id where p_id = 5 returning p_id'); //fb does not support multiple rows yet
    Check(Cnt = 1, 'updatecount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
  end;
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcInterbaseCase.GetSupportedProtocols: string;
begin
  Result := pl_all_interbase;
end;

const
  hl_id_Index           = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  stBooleanArray_Index  = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  stByte_Index          = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  stShort_Index         = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  stInteger_Index       = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
  stLong_Index          = {$IFDEF GENERIC_INDEX}5{$ELSE}6{$ENDIF};
  stFloat_Index         = {$IFDEF GENERIC_INDEX}6{$ELSE}7{$ENDIF};
  stDouble_Index        = {$IFDEF GENERIC_INDEX}7{$ELSE}8{$ENDIF};
  stBigDecimal_Index    = {$IFDEF GENERIC_INDEX}8{$ELSE}9{$ENDIF};
  stString_Index        = {$IFDEF GENERIC_INDEX}9{$ELSE}10{$ENDIF};
  stUnicode_Index       = {$IFDEF GENERIC_INDEX}10{$ELSE}11{$ENDIF};
  stBytes_Index         = {$IFDEF GENERIC_INDEX}11{$ELSE}12{$ENDIF};
  stDate_Index          = {$IFDEF GENERIC_INDEX}12{$ELSE}13{$ENDIF};
  stTime_Index          = {$IFDEF GENERIC_INDEX}13{$ELSE}14{$ENDIF};
  stTimeStamp_Index     = {$IFDEF GENERIC_INDEX}14{$ELSE}15{$ENDIF};
  stGUID_Index          = {$IFDEF GENERIC_INDEX}15{$ELSE}16{$ENDIF};
  stAsciiStream_Index   = {$IFDEF GENERIC_INDEX}16{$ELSE}17{$ENDIF};
  stUnicodeStream_Index = {$IFDEF GENERIC_INDEX}17{$ELSE}18{$ENDIF};
  stBinaryStream_Index  = {$IFDEF GENERIC_INDEX}18{$ELSE}19{$ENDIF};
{$WARNINGS OFF}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZTestDbcInterbaseCase.InternalTestArrayBinding(
  PStatement: IZPreparedStatement; FirstID, ArrayLen: Integer);
var
  I, J: Integer;

  procedure PrepareSomeData;
  var I: Integer;
  begin
    SetLength(hl_idArray, ArrayLen);
    SetLength(stBooleanArray, ArrayLen);
    SetLength(stByteArray, ArrayLen);
    SetLength(stShortArray, ArrayLen);
    SetLength(stLongArray, ArrayLen);
    SetLength(stIntegerArray, ArrayLen);
    SetLength(stFloatArray, ArrayLen);
    SetLength(stDoubleArray, ArrayLen);
    SetLength(stBigDecimalArray, ArrayLen);
    SetLength(stStringArray, ArrayLen);
    SetLength(stUnicodeStringArray, ArrayLen);
    SetLength(stBytesArray, ArrayLen);
    SetLength(stDateArray, ArrayLen);
    SetLength(stTimeArray, ArrayLen);
    SetLength(stTimeStampArray, ArrayLen);
    SetLength(stGUIDArray, ArrayLen);
    SetLength(stAsciiStreamArray, ArrayLen);
    SetLength(stUnicodeStreamArray, ArrayLen);
    SetLength(stBinaryStreamArray, ArrayLen);
    for i := 0 to ArrayLen-1 do
    begin
      hl_idArray[i] := FirstID+I;
      stBooleanArray[i] := Boolean(Random(1));
      stByteArray[i] := Random(255);
      stShortArray[i] := I;
      stLongArray[I] := I;
      stIntegerArray[I] := I;
      stFloatArray[i] := RandomFloat(-5000, 5000);
      stDoubleArray[i] := RandomFloat(-5000, 5000);
      stBigDecimalArray[i] := RandomFloat(-5000, 5000);
      stStringArray[i] := RandomStr(Random(99)+1);
      stUnicodeStringArray[i] := RandomStr(Random(254+1));
      stBytesArray[i] := RandomBts(ArrayLen);
      stDateArray[i] := Trunc(Now);
      stTimeArray[i] := Frac(Now);
      stTimeStampArray[i] := Now;
      stGUIDArray[i] := RandomGUID;
      stAsciiStreamArray[i].Len := Length(stStringArray[i]);
      stAsciiStreamArray[i].P := Pointer(stStringArray[i]);
      stAsciiStreamArray[i].CP := Connection.GetConSettings^.ClientCodePage^.CP; {safe we're passing ASCII7 only to the raws}
      stUnicodeStreamArray[i] := RandomStr(MaxPerformanceLobSize);
      stBinaryStreamArray[i] := TZAbstractBlob.Create;
      (stBinaryStreamArray[i] as IZBlob).SetBytes(RandomBts(MaxPerformanceLobSize));
    end;
  end;
begin
  CheckNotNull(PStatement);
  PrepareSomeData;
  PStatement.SetDataArray(hl_id_Index, hl_idArray, stInteger);
  PStatement.SetDataArray(stBooleanArray_Index, stBooleanArray, stBoolean);
  PStatement.SetDataArray(stByte_Index, stByteArray, stByte);
  PStatement.SetDataArray(stShort_Index, stShortArray, stShort);
  PStatement.SetDataArray(stInteger_Index, stIntegerArray, stInteger);
  PStatement.SetDataArray(stLong_Index, stLongArray, stLong);
  PStatement.SetDataArray(stFloat_Index, stFloatArray, stFloat);
  PStatement.SetDataArray(stDouble_Index, stDoubleArray, stDouble);
  PStatement.SetDataArray(stBigDecimal_Index, stBigDecimalArray, stBigDecimal);
  PStatement.SetDataArray(stString_Index, stStringArray, stString, vtRawByteString);
  PStatement.SetDataArray(stUnicode_Index, stUnicodeStringArray, stUnicodeString, vtUnicodeString);
  PStatement.SetDataArray(stBytes_Index, stBytesArray, stBytes);
  PStatement.SetDataArray(stDate_Index, stDateArray, stDate);
  PStatement.SetDataArray(stTime_Index, stTimeArray, stTime);
  PStatement.SetDataArray(stTimeStamp_Index, stTimeStampArray, stTimeStamp);
  PStatement.SetDataArray(stGUID_Index, stGUIDArray, stGUID);
  PStatement.SetDataArray(stAsciiStream_Index, stAsciiStreamArray, stString, vtCharRec);
  PStatement.SetDataArray(stUnicodeStream_Index, stUnicodeStreamArray, stString, vtUTF8String);
  PStatement.SetDataArray(stBinaryStream_Index, stBinaryStreamArray, stBinaryStream);

  for i := FirstDbcIndex to 19{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    case TZSQLType(Random(14)+1) of
      stBoolean:
        begin
          SetLength(stBooleanNullArray, Length(stBooleanNullArray) +1);
          SetLength(stBooleanNullArray[High(stBooleanNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stBooleanNullArray[High(stBooleanNullArray)][J] := False
            else
              stBooleanNullArray[High(stBooleanNullArray)][J] := Boolean(Random(1));
          PStatement.SetNullArray(I, stBoolean, stBooleanNullArray[High(stBooleanNullArray)]);
        end;
      stByte:
        begin
          SetLength(stByteNullArray, Length(stByteNullArray)+1);
          SetLength(stByteNullArray[High(stByteNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stByteNullArray[High(stByteNullArray)][J] := Ord(False)
            else
              stByteNullArray[High(stByteNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stByte, stByteNullArray[High(stByteNullArray)]);
        end;
      stShort:
        begin
          SetLength(stShortNullArray, Length(stShortNullArray)+1);
          SetLength(stShortNullArray[High(stShortNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stShortNullArray[High(stShortNullArray)][J] := 0
            else
              stShortNullArray[High(stShortNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stShort, stShortNullArray[High(stShortNullArray)]);
        end;
      stWord:
        begin
          SetLength(stWordNullArray, Length(stWordNullArray)+1);
          SetLength(stWordNullArray[High(stWordNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stWordNullArray[High(stWordNullArray)][j] := 0
            else
              stWordNullArray[High(stWordNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stWord, stWordNullArray[High(stWordNullArray)]);
        end;
      stSmall:
        begin
          SetLength(stSmallNullArray, Length(stSmallNullArray)+1);
          SetLength(stSmallNullArray[High(stSmallNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stSmallNullArray[High(stSmallNullArray)][J] := 0
            else
              stSmallNullArray[High(stSmallNullArray)][J] := -Random(2);
          PStatement.SetNullArray(I, stSmall, stSmallNullArray[High(stSmallNullArray)]);
        end;
      stLongWord:
        begin
          SetLength(stLongWordNullArray, Length(stLongWordNullArray)+1);
          SetLength(stLongWordNullArray[High(stLongWordNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stLongWordNullArray[High(stLongWordNullArray)][J] := 0
            else
              stLongWordNullArray[High(stLongWordNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stLongWord, stLongWordNullArray[High(stLongWordNullArray)]);
        end;
      stInteger:
        begin
          SetLength(stIntegerNullArray, Length(stIntegerNullArray)+1);
          SetLength(stIntegerNullArray[High(stIntegerNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stIntegerNullArray[High(stIntegerNullArray)][J] := 0
            else
              stIntegerNullArray[High(stIntegerNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stInteger, stIntegerNullArray[High(stIntegerNullArray)]);
        end;
      stULong:
        begin
          SetLength(stULongNullArray, Length(stULongNullArray)+1);
          SetLength(stULongNullArray[High(stULongNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stULongNullArray[High(stULongNullArray)][J] := 0
            else
              stULongNullArray[High(stULongNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stULong, stULongNullArray[High(stULongNullArray)]);
        end;
      stLong:
        begin
          SetLength(stLongNullArray, Length(stLongNullArray) +1);
          SetLength(stLongNullArray[High(stLongNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stLongNullArray[High(stLongNullArray)][J] := 0
            else
              stLongNullArray[High(stLongNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stLong, stLongNullArray[High(stLongNullArray)]);
        end;
      stFloat:
        begin
          SetLength(stFloatNullArray, Length(stFloatNullArray)+1);
          SetLength(stFloatNullArray[High(stFloatNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stFloatNullArray[High(stFloatNullArray)][J] := 0
            else
              stFloatNullArray[High(stFloatNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stFloat, stFloatNullArray[High(stFloatNullArray)]);
        end;
      stDouble:
        begin
          SetLength(stDoubleNullArray, Length(stDoubleNullArray)+1);
          SetLength(stDoubleNullArray[high(stDoubleNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stDoubleNullArray[high(stDoubleNullArray)][J] := 0
            else
              stDoubleNullArray[high(stDoubleNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stDouble, stDoubleNullArray[high(stDoubleNullArray)]);
        end;
      stCurrency:
        begin
          SetLength(stCurrencyNullArray, Length(stCurrencyNullArray)+1);
          SetLength(stCurrencyNullArray[High(stCurrencyNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stCurrencyNullArray[High(stCurrencyNullArray)][J] := 0
            else
              stCurrencyNullArray[High(stCurrencyNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stCurrency, stCurrencyNullArray[High(stCurrencyNullArray)]);
        end;
      stBigDecimal:
        begin
          SetLength(stBigDecimalNullArray, Length(stBigDecimalNullArray)+1);
          SetLength(stBigDecimalNullArray[High(stBigDecimalNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stBigDecimalNullArray[High(stBigDecimalNullArray)][J] := 0
            else
              stBigDecimalNullArray[High(stBigDecimalNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stBigDecimal, stBigDecimalNullArray[High(stBigDecimalNullArray)]);
        end;
      {stString:
        begin
          SetLength(stStringNullArray, Length(stStringNullArray)+1);
          SetLength(stStringNullArray[High(stStringNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
              else
                stStringNullArray[High(stStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stString, stStringNullArray[High(stStringNullArray)], vtRawByteString);
        end;}
      stUnicodeString:
        begin
          SetLength(stUnicodeStringNullArray, Length(stUnicodeStringNullArray)+1);
          SetLength(stUnicodeStringNullArray[High(stUnicodeStringNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'FALSE'
              else
                stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stUnicodeString, stUnicodeStringNullArray[High(stUnicodeStringNullArray)], vtUnicodeString);
        end;
      else
        begin
          SetLength(stStringNullArray, Length(stStringNullArray)+1);
          SetLength(stStringNullArray[High(stStringNullArray)], ArrayLen);
          for J := 0 to ArrayLen-1 do
            if I = FirstDbcIndex then
              stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
              else
                stStringNullArray[High(stStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stString, stStringNullArray[High(stStringNullArray)], vtRawByteString);
        end;
      {stBytes:
      stGUID:
      stDate:
      stTime:
      stTimestamp:
      stArray:
      stDataSet:
      stAsciiStream:
      stUnicodeStream:
      stBinaryStream:}
    end;
  PStatement.ExecuteUpdatePrepared;
  //SetLength(stShortNullArray, 0);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcInterbaseCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  if ConnectionConfig.Transport = traNative then
    CheckEquals(3, (Connection as IZInterbase6Connection).GetDialect);

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(cSInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiSerializable);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(cSInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.SetAutoCommit(False);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.CreateStatement;
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.Commit;
  Connection.Rollback;
  Connection.SetAutoCommit(True);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(cSInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Connection.SetTransactionIsolation(tiReadCommitted);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.CreateStatement;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

procedure TZTestDbcInterbaseCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
  Statement.close;
end;

procedure TZTestDbcInterbaseCase.TestRegularResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM DEPARTMENT');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
end;

procedure TZTestDbcInterbaseCase.TestBlobs;
const
  B_ID_Index = FirstDbcIndex;
  B_TEXT_Index = FirstDbcIndex+1;
  B_IMAGE_Index = FirstDbcIndex+2;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := CreateDbcConnection;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM BLOB_VALUES WHERE B_ID='
    + IntToStr(TEST_ROW_ID));

  TempStream := nil;
  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/zapotec.bmp'));
  try
    PreparedStatement := Connection.PrepareStatement(
      'INSERT INTO BLOB_VALUES (B_ID, B_TEXT, B_IMAGE) VALUES(?,?,?)');
    PreparedStatement.SetInt(B_ID_Index, TEST_ROW_ID);
    PreparedStatement.SetAsciiStream(B_TEXT_Index, TextStream);
    PreparedStatement.SetBinaryStream(B_IMAGE_Index, ImageStream);
    CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
      + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);
    CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
    TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
    CheckEquals(TextStream, TempStream);
    TempStream.Free;
    TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
    CheckEquals(ImageStream, TempStream);
  finally
    FreeAndNil(TempStream);
    ResultSet.Close;

    TextStream.Free;
    ImageStream.Free;

    Statement.Close;
  end;
end;

procedure TZTestDbcInterbaseCase.TestUpdateBlobs;
const
  insert_B_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  insert_B_TEXT_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  insert_B_IMAGE_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  update_B_ID_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  update_B_TEXT_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  update_B_IMAGE_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := CreateDbcConnection;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM BLOB_VALUES WHERE B_ID='
    + IntToStr(TEST_ROW_ID));

  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/zapotec.bmp'));

  PreparedStatement := Connection.PrepareStatement(
    'INSERT INTO BLOB_VALUES (B_ID, B_TEXT, B_IMAGE) VALUES(?,?,?)');
  PreparedStatement.SetInt(insert_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(insert_B_TEXT_Index, TextStream);
  PreparedStatement.SetBinaryStream(insert_B_IMAGE_Index, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;

// Update blob
  TextStream.Free;
  ImageStream.Free;
  TextStream := TStringStream.Create('GFEDCBA');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/dogs.jpg'));

  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(update_B_TEXT_Index, TextStream);
  PreparedStatement.SetBinaryStream(update_B_IMAGE_Index, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;

// Update null binary blob
  TextStream.Free;
  TextStream := TStringStream.Create('GFEDCBA');

  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(update_B_TEXT_Index, TextStream);
  PreparedStatement.SetNull(update_B_IMAGE_Index,stBinaryStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet.Close;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  CheckNull(ResultSet.GetBinaryStreamByName('B_IMAGE'));
  TempStream.Free;

// Update null ascii blob

  ResultSet.Close;
  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetNull(update_B_TEXT_Index,stAsciiStream);
  PreparedStatement.SetNull(update_B_IMAGE_Index,stBinaryStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  CheckNull(ResultSet.GetAsciiStreamByName('B_TEXT'));
  CheckNull(ResultSet.GetBinaryStreamByName('B_IMAGE'));

  ResultSet.Close;

  TextStream.Free;
  ImageStream.Free;

  Statement.Close;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);

    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    I := Stmt.ExecuteUpdate('Update people set p_id = p_id');
    Check(I = Cnt, 'updatecount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.TestCaseSensitive;
const
  CS_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  Cs_Data1_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  cs_data1_Index1 = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  cs_data1_Index2 = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM "Case_Sensitive"');
  CheckNotNull(ResultSet);
  Metadata := ResultSet.GetMetadata;
  CheckNotNull(Metadata);

  CheckEquals('CS_ID', Metadata.GetColumnName(CS_ID_Index));
  CheckEquals(False, Metadata.IsCaseSensitive(CS_ID_Index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(CS_ID_Index));

  CheckEquals('Cs_Data1', Metadata.GetColumnName(Cs_Data1_Index));
  CheckEquals(True, Metadata.IsCaseSensitive(Cs_Data1_Index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(Cs_Data1_Index));

  CheckEquals('cs_data1', Metadata.GetColumnName(cs_data1_Index1));
  CheckEquals(True, Metadata.IsCaseSensitive(cs_data1_Index1));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_Index1));

  CheckEquals('cs data1', Metadata.GetColumnName(cs_data1_Index2));
  CheckEquals(True, Metadata.IsCaseSensitive(cs_data1_Index2));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_Index2));

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase default values.
}
procedure TZTestDbcInterbaseCase.TestDefaultValues;
const
  D_ID = FirstDbcIndex;
  D_FLD1 = FirstDbcIndex +1;
  D_FLD2 = FirstDbcIndex +2;
  D_FLD3 = FirstDbcIndex +3;
  D_FLD4 = FirstDbcIndex +4;
  D_FLD5 = FirstDbcIndex +5;
  D_FLD6 = FirstDbcIndex +6;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DEFAULT_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT D_ID,D_FLD1,D_FLD2,D_FLD3,D_FLD4,D_FLD5,D_FLD6 FROM DEFAULT_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(D_FLD4), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(D_FLD5), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(D_FLD6), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase domain fields.
}
procedure TZTestDbcInterbaseCase.TestDomainValues;
const
  D_ID = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  D_FLD1 = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  D_FLD2 = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  D_FLD3 = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DOMAIN_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));

  ResultSet.Close;
  ResultSet := nil;

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.Next;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase stored procedures.
}
procedure TZTestDbcInterbaseCase.TestStoredprocedures;
var
  ResultSet: IZResultSet;
  CallableStatement: IZCallableStatement;
begin
  // Doesn't run with ExecutePrepared. RegisterOutParameter does also not work.
  // Has to be called with an ExecuteQueryPrepared, then has to be fetched and
  // afterwards the Resultes have to be retrieved via result set columns.
  // Resultset must only have one(!) line.
  CallableStatement := Connection.PrepareCallWithParams(
    'PROCEDURE1', nil);
  with CallableStatement do begin
    SetInt(FirstDbcIndex, 12345);
    ResultSet := ExecuteQueryPrepared;
    with ResultSet do begin
      CheckEquals(True, Next);
      CheckEquals(True, (IsFirst() and IsLast()));
      CheckEquals(12346, GetInt(FirstDbcIndex));
    end;
  end;
  CallableStatement.Close;

  CallableStatement := Connection.PrepareCallWithParams(
    'PROCEDURE2', nil);
  ResultSet := CallableStatement.ExecuteQueryPrepared;
  with ResultSet do begin
    CheckEquals(True, Next);
    CheckEquals('Computer', GetString(FirstDbcIndex));
    CheckEquals(True, Next);
    CheckEquals('Laboratoy', GetString(FirstDbcIndex));
    CheckEquals(True, Next);
    CheckEquals('Radiostation', GetString(FirstDbcIndex));
    CheckEquals(True, Next);
    CheckEquals('Volvo', GetString(FirstDbcIndex));
    Close;
  end;
  CallableStatement.Close;
end;

type
    TZTimeStamp = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Fractions: Cardinal;
  end;

procedure TZTestDbcInterbaseCase.TestMsec;
const
  D_ID = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  D_DATE = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  D_TIME = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  D_DATETIME = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  D_TIMESTAMP = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  ThisTime : TDateTime;
  oldTimeFormat: string;
  TS1, TS2: TZTimeStamp;
  procedure ToTS(const InValue: TDateTime; var ToVal:TZTimeStamp);
  begin
    ToVal.Fractions := 0;
    DecodeDateTime(InValue, ToVal.Year, ToVal.Month, ToVal.Day, ToVal.Hour,
      ToVal.Minute, ToVal.Second, PWord(@ToVal.Fractions)^);
  end;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.ExecuteUpdate('delete from DATE_VALUES where D_ID=4');
  ResultSet := Statement.ExecuteQuery('select D_ID, D_DATE, D_TIME, D_DATETIME, D_TIMESTAMP from DATE_VALUES');
  CheckNotNull(ResultSet);
  OldTimeFormat := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat := 'hh:mm:ss.zzz';
  ThisTime := DateUtils.EncodeDateTime(18,8,2, 13, 13, 13, 999);
  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 4);
  ResultSet.UpdateDate(D_DATE,ThisTime);
  ResultSet.UpdateTime(D_TIME,ThisTime);
  ResultSet.UpdateTimestamp(D_DATETIME,ThisTime);
  ResultSet.UpdateTimestamp(D_TIMESTAMP,ThisTime);
  ResultSet.InsertRow;
//  ResultSet.Last; // why do we do this in this test?
  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(Trunc(ThisTime), ResultSet.GetDate(D_DATE),'Failure field 2');
  ToTS(Frac(ThisTime), TS1);
  ToTS(ResultSet.GetTime(D_TIME), Ts2);
  CheckEquals(EncodeTime(ts1.Hour, ts1.Minute, ts1.Second, 0), EncodeTime(ts2.Hour, ts2.Minute, ts2.Second, 0), 'time without fractions');
  CheckEquals(ts1.Fractions, ts2.Fractions, 'fractions');
  CheckEquals(ThisTime, ResultSet.GetTimeStamp(D_DATETIME),'Failure field 4');
  CheckEquals(ThisTime, ResultSet.GetTimeStamp(D_TIMESTAMP),'Failure field 5');
  ResultSet.DeleteRow;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat := OldTimeFormat;
  ResultSet.Close;
  Statement.Close;
end;

// There should be no
// SQL Error:  Dynamic SQL Error SQL error code = -804 Incorrect values within SQLDA structure.
procedure TZTestDbcInterbaseCase.TestEmptyStrings;
const
  CSQLd = 'delete from department where dep_id in (4,5)';
  CSQLi = 'insert into department (dep_id, dep_shname) values (?,?)';
  dep_id = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  dep_shname = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  PreparedStatement: IZPreparedStatement;
begin
  PreparedStatement := Connection.PrepareStatement(CSQLd);
  CheckNotNull(PreparedStatement);
  PreparedStatement.ExecutePrepared;
  PreparedStatement.Close;
  PreparedStatement := Connection.PrepareStatement(CSQLi);
  CheckNotNull(PreparedStatement);
  PreparedStatement.SetInt(dep_id, 4);
  PreparedStatement.SetString(dep_shname, '');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.SetInt(dep_id, 5);
  PreparedStatement.SetString(dep_shname, '');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.Close;
end;

{$WARNINGS OFF} //implizit string conversion of...
procedure TZTestDbcInterbaseCase.TestArrayBindings;
var PStatement: IZPreparedStatement;
begin
  Connection.PrepareStatement('delete from high_load').ExecutePrepared;
  PStatement := Connection.PrepareStatement(
  'insert into high_load(hl_id, stBoolean, stByte, stShort, stInteger, stLong, '+
    'stFloat, stDouble, stBigDecimal, stString, stUnicodeString, stBytes,'+
    'stDate, stTime, stTimestamp, stGUID, stAsciiStream, stUnicodeStream, '+
    'stBinaryStream) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)');
  CheckNotNull(PStatement);
  InternalTestArrayBinding(PStatement, 0, 50);
  InternalTestArrayBinding(PStatement, 50, 20);
  InternalTestArrayBinding(PStatement, 70, 10);
  PStatement.ClearParameters;
  PStatement.SetInt(hl_id_Index, 81);
  PStatement.SetBoolean(stBooleanArray_Index, stBooleanArray[Random(9)]);
  PStatement.SetByte(stByte_Index, stByteArray[Random(9)]);
  PStatement.SetShort(stShort_Index, stShortArray[Random(9)]);
  PStatement.SetInt(stInteger_Index, stIntegerArray[Random(9)]);
  PStatement.SetLong(stLong_Index, stLongArray[Random(9)]);
  PStatement.SetFloat(stFloat_Index, stFloatArray[Random(9)]);
  PStatement.SetDouble(stDouble_Index, stDoubleArray[Random(9)]);
  PStatement.SetBigDecimal(stBigDecimal_Index, stBigDecimalArray[Random(9)]);
  PStatement.SetRawByteString(stString_Index, stStringArray[Random(9)]);
  PStatement.SetUnicodeString(stUnicode_Index, stUnicodeStringArray[Random(9)]);
  PStatement.SetBytes(stBytes_Index, stBytesArray[Random(9)]);
  PStatement.SetDate(stDate_Index, stDateArray[Random(9)]);
  PStatement.SetTime(stTime_Index, stTimeArray[Random(9)]);
  PStatement.SetTimestamp(stTimeStamp_Index, stTimeStampArray[Random(9)]);
  PStatement.SetNull(stGUID_Index, stString);
  PStatement.SetCharRec(stAsciiStream_Index, stAsciiStreamArray[Random(9)]);
  PStatement.SetUTF8String(stUnicodeStream_Index, stUnicodeStreamArray[Random(9)]);
  PStatement.SetBlob(stBinaryStream_Index, stBinaryStream, stBinaryStreamArray[Random(9)] as IZBlob);
  PStatement.ExecuteUpdatePrepared;
  PStatement.ClearParameters;
  with PStatement.ExecuteQuery('select Count(*) from high_load') do
  begin
    Next;
    CheckEquals(81, GetInt(FirstDbcIndex), 'Blokinsertiation Count');
  end;
end;
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcInterbaseCase.TestClientVersionNumber;
var
  Version: Integer;
begin
  Version := Connection.GetClientVersion;

  CheckNotEquals(0, Version, 'Expected a client library version of anything but 0.');
end;

procedure TZTestDbcInterbaseCase.TestDefaultReadCommittedMode;
const
  IDX = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  IsFirebird: Boolean;
  ServerVersion: Integer;
  IsolationMode: Integer;
begin
  Connection.Close;
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.Open;

  if ConnectionConfig.Transport <> traNative then
    Exit;

  with (Connection.GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo) do begin
    IsFirebird := HostIsFireBird;
    ServerVersion := GetHostVersion;
  end;

  if (not IsFirebird) or (ServerVersion < 2001000) then begin
    Check(true, 'This is a fake and cannot fail because this test can only be executed on Firebird 2.1+');
  end else begin
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement, 'Couldn''t get a valid statement.');
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcReadOnly);

    ResultSet := Statement.ExecuteQuery('select T.MON$ISOLATION_MODE from MON$TRANSACTIONS T where T.MON$TRANSACTION_ID = CURRENT_TRANSACTION');
    CheckNotNull(ResultSet);

    Check(ResultSet.Next, 'Couldn''t move to the first result row.');

    IsolationMode := ResultSet.GetInt(IDX);

    ResultSet.Close;
    Statement.Close;

    if ServerVersion >= 4000000 then
      CheckEquals(4, IsolationMode, 'Expected Isolation mode to be READ COMMITTED READ CONSISTENCY (4) but got something else.')
    else
      CheckEquals(2, IsolationMode, 'Expected Isolation mode to be READ COMMITTED RECORD VERSION (2) but got something else.');
  end;
end;


/// <summary>
///   This test tests if statements longer than 64 KB work as expected.
/// </summary>
procedure TZTestDbcInterbaseCase.TestLongStatements;
const
  IDX = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;

  SQL: String;
  Ctr: Integer;
  MinLen: Integer;
begin
  MinLen := Integer(High(Word)) + 1;
  if Connection.GetMetadata.GetDatabaseInfo.GetMaxStatementLength > MinLen then begin
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement, 'Couldn''t get a valid statement.');
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcReadOnly);

    //Build SQL:
    Ctr := FirstDbcIndex + 1;
    SQL := 'select cast(' + IntToStr(FirstDbcIndex) + ' as integer) as Field' + IntToStr(FirstDbcIndex);
    while Length(SQL) < MinLen do begin
      SQL := SQL + ', cast(' + IntToStr(Ctr) + ' as integer) as Field' + IntToStr(Ctr);
      Inc(Ctr);
    end;

    SQL := SQL + ' from RDB$DATABASE';

    ResultSet := Statement.ExecuteQuery(SQL);
    CheckNotNull(ResultSet);

    Check(ResultSet.Next, 'Couldn''t move to the first result row.');

    for Ctr := FirstDbcIndex to ResultSet.GetMetadata.GetColumnCount - FirstDbcIndex do begin
      CheckEquals(Ctr, ResultSet.GetInt(Ctr), 'Expected the field to have its index number as its value.');
    end;

    ResultSet.Close;
    Statement.Close;
  end else begin
    Check(true, 'This is a fake and cannot fail because this test can only be executed on Firebird 3.0+');
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcInterbaseCase.Suite);
end.
