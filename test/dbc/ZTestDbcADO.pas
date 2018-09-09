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

unit ZTestDbcADO;

interface

{$I ZDbc.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZSqlTestCase,
  Types, ZCompatibility, SysUtils, ZDbcResultSet, ZVariant;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcADOCase = class(TZAbstractDbcSQLTestCase)
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
    procedure TestArrayBindings;
    procedure TestFetchSequenceValue;
  end;

implementation

{ TZTestDbcADOCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZTestDbcADOCase.GetSupportedProtocols: string;
begin
  Result := 'ADO';
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
procedure TZTestDbcADOCase.InternalTestArrayBinding(
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
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcADOCase.TestArrayBindings;
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
  PStatement.SetNull(stGUID_Index, stGuid);
  PStatement.SetCharRec(stAsciiStream_Index, stAsciiStreamArray[Random(9)]);
  PStatement.SetUTF8String(stUnicodeStream_Index, stUnicodeStreamArray[Random(9)]);
  PStatement.SetBlob(stBinaryStream_Index, stBinaryStream, stBinaryStreamArray[Random(9)] as IZBlob);
  PStatement.ExecuteUpdatePrepared;
  PStatement.ClearParameters;
  with PStatement.ExecuteQuery('select Count(*) from high_load') do
  begin
    Next;
    CheckEquals(81, GetInt(FirstDbcIndex), 'Blokinsertiation Count');
    Close;
  end;
  PStatement.Close;
  Connection.Commit;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcADOCase.TestFetchSequenceValue;
var PStatement: IZPreparedStatement;
begin
  PStatement := Connection.PrepareStatement('SELECT current_value FROM sys.sequences WHERE name = ''generate_id''');
  try
    with PStatement.ExecuteQueryPrepared do
    begin
      Next;
      CheckEquals(90000250, GetLong(FirstDbcIndex), 'current_value');
      Close;
    end;
  finally
    PStatement.Close;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcADOCase.Suite);
end.
