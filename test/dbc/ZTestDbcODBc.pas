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

unit ZTestDbcODBc;

interface
{$I ZDbc.inc}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcInterbase6, ZSqlTestCase,
  ZCompatibility, DateUtils, Math, Types;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcODBCCase = class(TZAbstractDbcSQLTestCase)
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
  protected
    function GetSupportedProtocols: string; override;
    procedure InternalTestArrayBinding(PStatement: IZPreparedStatement;
      FirstID, ArrayLen: Integer);
  published
    procedure TestArrayBindings;
    procedure TestFetchSequenceValue;
  end;

implementation

uses SysUtils, ZTestConsts, ZTestCase, ZDbcResultSet, ZVariant;

{ TZTestDbcODBCCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcODBCCase.GetSupportedProtocols: string;
begin
  Result := 'odbc_w,odbc_a';
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
procedure TZTestDbcODBCCase.InternalTestArrayBinding(
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

  for i := FirstDbcIndex to 19{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
    SetLength(stBooleanNullArray, Length(stBooleanNullArray) +1);
    SetLength(stBooleanNullArray[High(stBooleanNullArray)], ArrayLen);
    for J := 0 to ArrayLen-1 do
      if I = FirstDbcIndex then
        stBooleanNullArray[High(stBooleanNullArray)][J] := False
      else
        stBooleanNullArray[High(stBooleanNullArray)][J] := Boolean(Random(1));
    PStatement.SetNullArray(I, stBoolean, stBooleanNullArray[High(stBooleanNullArray)]);
  end;
  PStatement.ExecuteUpdatePrepared;
  //SetLength(stShortNullArray, 0);
end;
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcODBCCase.TestArrayBindings;
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
{$WARNINGS ON} //implizit string conversion of...

procedure TZTestDbcODBCCase.TestFetchSequenceValue;
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
  RegisterTest('dbc',TZTestDbcODBCCase.Suite);
end.
