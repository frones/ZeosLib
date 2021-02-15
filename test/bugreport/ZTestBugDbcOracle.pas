{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for Interbase DBC Bug Reports         }
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

unit ZTestBugDbcOracle;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_ORACLE}
uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZCompatibility,
  ZDbcOracle, ZSqlTestCase;

type

  {** Implements a DBC bug report test case for Oracle }
  TZTestDbcOracleBugReport = class(TZAbstractDbcSQLTestCase)
  private
    FConnLostError: EZSQLConnectionLost;
    procedure FOnConnectionLost(var AError: EZSQLConnectionLost);
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestNum1;
    procedure TestBlobValues;
    procedure TestTicket437;
    procedure TestConnectionLossTicket452;
    procedure TestTicket455;
    procedure TestDuplicateParamNames;
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses ZTestCase, ZDbcLogging;

{ TZTestDbcOracleBugReport }

procedure TZTestDbcOracleBugReport.FOnConnectionLost(
  var AError: EZSQLConnectionLost);
begin
  FConnLostError := AError;
  AError := nil;
end;

function TZTestDbcOracleBugReport.GetSupportedProtocols: string;
begin
  Result := 'oracle';
end;

{**
  NUMBER must be froat
}
procedure TZTestDbcOracleBugReport.TestNum1;
const
  col_id_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  col_num_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM Table_Num1');
  with ResultSet do
  begin
    with GetMetadata do
    begin
      CheckEquals(ord(stInteger), Ord(GetColumnType(col_id_Index)), 'id column type');
      CheckEquals(ord(stBigDecimal), Ord(GetColumnType(col_num_Index)), 'Num column type');
    end;
    CheckEquals(True, Next, 'ResultSet.Next');
    CheckEquals(1, GetInt(col_id_Index), 'id value');
    CheckEquals(54321.0123456789, GetDouble(col_num_Index), 1E-11, 'Num value');
    Close;
  end;
end;

procedure TZTestDbcOracleBugReport.TestTicket437;
const
  col_id_Index      = FirstDbcIndex;
  col_text_Index    = col_id_Index +1;
  col_clobek_index  = col_text_Index +1;
  col_longek_index  = col_clobek_index +1;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  LongLobRow1, LongLobRow2, LongLobRow3: IZBlob;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('select * from table_ticket437 order by id');
  with ResultSet do try
    with GetMetadata do
    begin
      CheckEquals(stInteger, GetColumnType(col_id_Index), 'id column type');
      CheckEquals(stString, GetColumnType(col_text_Index), 'text column type');
      CheckEquals(stAsciiStream, GetColumnType(col_clobek_index), 'clobek column type');
      CheckEquals(stAsciiStream, GetColumnType(col_longek_index), 'longek column type');
    end;
    Check(Next, 'ResultSet.Next');
    CheckEquals('ASD', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('ASD', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('ASD', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow1 := GetBlob(col_longek_index);
    Check(Next, 'ResultSet.Next');
    CheckEquals('ASDF', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('ASDF', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('ASDF', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow2 := GetBlob(col_longek_index);
    Check(Next, 'ResultSet.Next');
    CheckEquals('QWERT', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('QWERT', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('QWERT', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow3 := GetBlob(col_longek_index);

    CheckEquals('ASD', LongLobRow1.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString(Connection.GetConSettings.ClientCodePage.CP){$ENDIF}, 'the value of longek row 1');
    CheckEquals('ASDF', LongLobRow2.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString(Connection.GetConSettings.ClientCodePage.CP){$ENDIF}, 'the value of longek row 2');
    CheckEquals('QWERT', LongLobRow3.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString(Connection.GetConSettings.ClientCodePage.CP){$ENDIF}, 'the value of longek row 3');
  finally
    LongLobRow1 := nil;
    LongLobRow2 := nil;
    LongLobRow3 := nil;
    Close;
  end;
end;

(* If TZStoredProc StoredProcName is set to a name that doesn't exist then
executing it will give an access violation. Error occurs in BuildFunction where
it refers to Descriptor.Args[0], but args is nil.*)
procedure TZTestDbcOracleBugReport.TestTicket455;
var Stmt: IZCallableStatement;
begin
  Stmt := Connection.PrepareCall('TestTicket455');
  Check(Stmt <> nil);
  try
    Stmt.ExecuteUpdatePrepared;
    Fail('This test can''t pass');
  Except
    on E:Exception do
      Check(E is EZSQLException, 'We expect an SQLException here');
  end;
end;

procedure TZTestDbcOracleBugReport.TestBlobValues;
begin
  if SkipForReason(srClosedBug) then Exit;

  with Connection.CreateStatement.ExecuteQuery('select * from blob_values') do
  begin
    CheckEquals(6, GetMetadata.GetColumnCount);
    Check(next);
    Close;
  end;
end;

procedure TZTestDbcOracleBugReport.TestConnectionLossTicket452;
var CL_Connection: IZConnection;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  SQL: String;
  NCLOB, longLob: IZBlob;
  Stream: TStream;
begin
  CL_Connection := DriverManager.GetConnection(Connection.GetURL);
  Check(CL_Connection <> nil);
  Stream := nil;
  try
    CL_Connection.SetOnConnectionLostErrorHandler(FOnConnectionLost);
    Statement := CL_Connection.CreateStatement;
    ResultSet := Statement.ExecuteQuery('SELECT SID, SERIAL# FROM V$SESSION WHERE AUDSID = Sys_Context(''USERENV'', ''SESSIONID'')');
    try
      ResultSet.Next;
    except
      Fail('To get this test runinng use SQLPLUS, login as SYSDBA and EXECUTE: "grant select on SYS.V_$SESSION to [My_USERNAME]"');
    end;
    SQL := 'ALTER SYSTEM DISCONNECT SESSION ''' + ResultSet.GetString(FirstDbcIndex)+ ',' + ResultSet.GetString(FirstDbcIndex+1) + ''' IMMEDIATE';
    ResultSet.Close;
    ResultSet := Statement.ExecuteQuery('select * from blob_values');
    Check(ResultSet.Next);
    Check(ResultSet.Next);
    longLob := ResultSet.GetBlob(FirstDbcIndex+1);
    longLob.Open(lsmRead);
    NCLOB := ResultSet.GetBlob(FirstDbcIndex+2);
    NCLOB.Open(lsmRead);
    //now kill connection
    Connection.ExecuteImmediat(SQL, lcDisconnect);
    Stream := NCLOB.GetStream;
    CheckEquals(0, Stream.Size); //this shcould now also trigger the connlost error
    Check(ResultSet.IsClosed, 'the resultset should be closed');
    //now try to do "something" to get a communication error 3113
    //the FOnConnectionLost handler should hide the exception
    CheckFalse(ResultSet.Next);
    Check(NCLOB.IsEmpty, 'no more data after connection lost error');
    //Check(longLob.IsEmpty, 'no more data after connection lost error');
    Check(FConnLostError <> nil, 'There is a connection lost error!');
    Check(ResultSet.IsClosed, 'the resultset should be closed');
    Check(Statement.IsClosed, 'the statement should be closed');
    Check(CL_Connection.IsClosed);
  finally
    FreeAndnil(Stream);
    FreeAndNil(FConnLostError);
  end;
end;

{ see https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=132398}
procedure TZTestDbcOracleBugReport.TestDuplicateParamNames;
var Statement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  Check(Connection <> nil);
  try
    Statement := Connection.PrepareStatement('SELECT :SID, :SID, :SID+:AID FROM DUAL Where 1 = :SID or 2 = :SID');
    Statement.SetInt(FirstDbcIndex, 1);
    Statement.SetInt(FirstDbcIndex+1, 1);
    Statement.SetInt(FirstDbcIndex+2, 1);
    Statement.SetInt(FirstDbcIndex+3, 1);
    Statement.SetInt(FirstDbcIndex+4, 1);
    Statement.SetInt(FirstDbcIndex+5, 1);
    ResultSet := Statement.ExecuteQueryPrepared;
    Check(ResultSet.Next);
    CheckEquals(3, ResultSet.GetColumnCount);
    CheckFalse(ResultSet.IsNull(FirstDbcIndex));
    CheckFalse(ResultSet.IsNull(FirstDbcIndex+1));
    CheckFalse(ResultSet.IsNull(FirstDbcIndex+2));
  finally

  end;
end;

initialization
  RegisterTest('bugreport',TZTestDbcOracleBugReport.Suite);
{$ENDIF ZEOS_DISABLE_ORACLE}
end.
