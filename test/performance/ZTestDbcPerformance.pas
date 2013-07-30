{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for ZDBC API Performance           }
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

unit ZTestDbcPerformance;

interface

{$I ZPerformance.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, Classes,
  ZPerformanceTestCase, ZDbcIntfs, ZCompatibility
  {$IFDEF ENABLE_MYSQL}
    ,ZDbcMySql
  {$ENDIF}
  ;

type

  {** Implements a performance test case for Native DBC API. }
  TZNativeDbcPerformanceTestCase = class(TZPerformanceSQLTestCase)
  protected
    FConnection: IZConnection;
  protected
    property Connection: IZConnection read FConnection write FConnection;

    function GetImplementedAPI: string; override;
    function CreateResultSet(Query: string): IZResultSet; virtual;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
  end;

  {** Implements a performance test case for Native DBC API. }
  TZCachedDbcPerformanceTestCase = class (TZNativeDbcPerformanceTestCase)
  protected
    function GetImplementedAPI: string; override;
    function CreateResultSet(Query: string): IZResultSet; override;

    procedure RunTestInsert; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
  end;

implementation

uses ZTestCase, ZTestConsts, ZSysUtils, ZDbcResultSet, Types;

{ TZNativeDbcPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZNativeDbcPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbc';
end;

{**
  Creates a specific for this test result set.
  @param Query a SQL query string.
  @return a created Result Set for the SQL query.
}
function TZNativeDbcPerformanceTestCase.CreateResultSet(
  Query: string): IZResultSet;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  Statement.SetFetchDirection(fdForward);
  Statement.SetResultSetConcurrency(rcReadOnly);
  Statement.SetResultSetType(rtForwardOnly);
  Result := Statement.ExecuteQuery(Query);
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZNativeDbcPerformanceTestCase.DefaultSetUpTest;
begin
  Connection := CreateDbcConnection;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZNativeDbcPerformanceTestCase.DefaultTearDownTest;
begin
  if Connection <> nil then
  begin
    Connection.Close;
    Connection := nil;
  end;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZNativeDbcPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestConnect;
begin
  if SkipForReason(srNoPerformance) then Exit;
  Connection := CreateDbcConnection;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs an insert test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestInsert;
var
  I,N: Integer;
  Statement: IZPreparedStatement;
  SQL: String;
  Ansi: RawByteString;
  Uni: ZWideString;
  Bts: TByteDynArray;
begin
  if SkipForReason(srNoPerformance) then Exit;

  SQL := 'INSERT INTO '+PerformanceTable+' VALUES (';
  for i := 0 to high(Self.ResultSetTypes) do
    if i = 0 then SQL := SQL+'?'
    else SQL := SQL+',?';
  SQL := SQL+')';
  Statement := Connection.PrepareStatement( SQL);
  for I := 1 to GetRecordCount do
  begin
    for N := 1 to high(ResultSetTypes)+1 do
      case ResultSetTypes[N-1] of
        stBoolean: Statement.SetBoolean(N, Random(1) = 1);
        stByte:    Statement.SetByte(N, Ord(Random(255)));
        stShort,
        stInteger,
        stLong:    Statement.SetInt(N, I);
        stFloat,
        stDouble,
        stBigDecimal: Statement.SetFloat(N, RandomFloat(-100, 100));
        stString: Statement.SetString(N, RandomStr(FieldSizes[N-1]));
        stUnicodeString: Statement.SetUnicodeString(N, ZWideString(RandomStr(FieldSizes[N-1])));
        stDate:    Statement.SetDate(N, Now);
        stTime:    Statement.SetTime(N, Now);
        stTimestamp: Statement.SetTimestamp(N, now);
        stBytes, stGUID: Statement.SetBytes(N, RandomBts(16));
        stAsciiStream:
          begin
            Ansi := RawByteString(RandomStr(GetRecordCount*100));
            Statement.SetBlob(N, stAsciiStream, TZAbstractBlob.CreateWithData(PAnsiChar(Ansi), GetRecordCount*100, Connection, False));
          end;
        stUnicodeStream:
          begin
            Uni := ZWideString(RandomStr(GetRecordCount*100));
            Statement.SetBlob(N, stUnicodeStream, TZAbstractBlob.CreateWithData(PWideChar(Uni), GetRecordCount*100*2, Connection, True));
          end;
        stBinaryStream:
          begin
            Bts := RandomBts(GetRecordCount*100);
            Statement.SetBlob(N, stUnicodeStream, TZAbstractBlob.CreateWithData(Pointer(Bts), GetRecordCount*100, Connection, False));
          end;
      end;
    Statement.ExecuteUpdatePrepared;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs an open test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestOpen;
begin
  if SkipForReason(srNoPerformance) then Exit;

  CreateResultSet('SELECT * FROM '+PerformanceTable);
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
   Performs a fetch data
}
procedure TZNativeDbcPerformanceTestCase.RunTestFetch;
var
  ResultSet: IZResultSet;
begin
  if SkipForReason(srNoPerformance) then Exit;

  ResultSet := CreateResultSet('SELECT * FROM '+PerformanceTable);
  while ResultSet.Next do
  begin
//    ResultSet.GetPChar(1);
//    ResultSet.GetPChar(2);
//    ResultSet.GetPChar(3);
    ResultSet.GetInt(1);
    ResultSet.GetFloat(2);
    ResultSet.GetString(3);
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs an update test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestUpdate;
var
  I, N: Integer;
  SQL: String;
  Statement: IZPreparedStatement;
  Ansi: RawByteString;
  Uni: ZWideString;
  Bts: TByteDynArray;
begin
  if SkipForReason(srNoPerformance) then Exit;

  SQL := 'UPDATE '+PerformanceTable+' SET';
  for i := 1 to high(FieldNames) do
    if I = 1 then
      SQL := SQL + ' '+FieldNames[I]+'=?'
    else
      SQL := SQL + ', '+FieldNames[I]+'=?';
  SQL := SQL + ' WHERE '+PerformancePrimaryKey+'=?';
  Statement := Connection.PrepareStatement(SQL);
  for I := 1 to GetRecordCount do
  begin
    for N := 1 to high(ResultSetTypes) do
      case ResultSetTypes[N] of
        stBoolean: Statement.SetBoolean(N, Random(1) = 1);
        stByte:    Statement.SetByte(N, Ord(Random(255)));
        stShort,
        stInteger,
        stLong:    Statement.SetInt(N, I);
        stFloat,
        stDouble,
        stBigDecimal: Statement.SetFloat(N, RandomFloat(-100, 100));
        stString: Statement.SetString(N, RandomStr(FieldSizes[N]));
        stUnicodeString: Statement.SetUnicodeString(N, ZWideString(RandomStr(FieldSizes[N])));
        stDate:    Statement.SetDate(N, Now);
        stTime:    Statement.SetTime(N, Now);
        stTimestamp: Statement.SetTimestamp(N, now);
        stBytes, stGUID: Statement.SetBytes(N, RandomBts(16));
        stAsciiStream:
          begin
            Ansi := RawByteString(RandomStr(GetRecordCount*100));
            Statement.SetBlob(N, stAsciiStream, TZAbstractBlob.CreateWithData(PAnsiChar(Ansi), GetRecordCount*100, Connection, False));
          end;
        stUnicodeStream:
          begin
            Uni := ZWideString(RandomStr(GetRecordCount*100));
            Statement.SetBlob(N, stUnicodeStream, TZAbstractBlob.CreateWithData(PWideChar(Uni), GetRecordCount*100*2, Connection, True));
          end;
        stBinaryStream:
          begin
            Bts := RandomBts(GetRecordCount*100);
            Statement.SetBlob(N, stUnicodeStream, TZAbstractBlob.CreateWithData(Pointer(Bts), GetRecordCount*100, Connection, False));
          end;
      end;
    Statement.SetInt(High(ResultSetTypes)+1, I);
    Statement.ExecuteUpdatePrepared;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs a delete test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestDelete;
var
  I: Integer;
  Statement: IZPreparedStatement;
begin
  if SkipForReason(srNoPerformance) then Exit;

  Statement := Connection.PrepareStatement(
    'DELETE from '+PerformanceTable+' WHERE '+PerformancePrimaryKey+'=?');
  for I := 1 to GetRecordCount do
  begin
    Statement.SetInt(1, I);
    Statement.ExecutePrepared;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs a direct update test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestDirectUpdate;
var
  I: Integer;
  Statement: IZStatement;
begin
  if SkipForReason(srNoPerformance) then Exit;

  Statement := Connection.CreateStatement;
  for I := 1 to GetRecordCount do
  begin
    Statement.ExecuteUpdate(Format('UPDATE '+PerformanceTable+' SET data1=%s, data2=''%s'''
      + ' WHERE '+PerformancePrimaryKey+' = %d', [FloatToSqlStr(RandomFloat(-100, 100)),
      RandomStr(10), I]));
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{ TZCachedDbcPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZCachedDbcPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbc-cached';
end;

{**
  Creates a specific for this test result set.
  @param Query a SQL query string.
  @return a created Result Set for the SQL query.
}
function TZCachedDbcPerformanceTestCase.CreateResultSet(
  Query: string): IZResultSet;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  Statement.SetFetchDirection(fdForward);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetResultSetType(rtScrollInsensitive);
  Result := Statement.ExecuteQuery(Query);
end;

{**
  Performs an insert test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestInsert;
var
  I: Integer;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srNoPerformance) then Exit;

  ResultSet := CreateResultSet('SELECT * '+PerformanceTable);
  for I := 1 to GetRecordCount do
  begin
    ResultSet.MoveToInsertRow;
    ResultSet.UpdateInt(1, I);
    ResultSet.UpdateFloat(2, RandomFloat(-100, 100));
    ResultSet.UpdateString(3, RandomStr(10));
    ResultSet.InsertRow;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs an update test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestUpdate;
var
  ResultSet: IZResultSet;
begin
  if SkipForReason(srNoPerformance) then Exit;

  ResultSet := CreateResultSet('SELECT * from '+PerformanceTable);
  while ResultSet.Next do
  begin
    ResultSet.UpdateFloat(2, RandomFloat(-100, 100));
    ResultSet.UpdateString(3, RandomStr(10));
    ResultSet.UpdateRow;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

{**
  Performs a delete test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestDelete;
var
  ResultSet: IZResultSet;
begin
  if SkipForReason(srNoPerformance) then Exit;

  ResultSet := CreateResultSet('SELECT * from '+PerformanceTable);
  while ResultSet.Next do
    ResultSet.DeleteRow;
  if not SkipPerformanceTransactionMode then Connection.Commit;
end;

initialization
  RegisterTest('performance', TZNativeDbcPerformanceTestCase.Suite);
  RegisterTest('performance', TZCachedDbcPerformanceTestCase.Suite);
end.

