{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for the Transaction Component        }
{                                                         }
{            Originally written by EgonHugeist            }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestTransaction;

{$I ZComponent.inc}

interface

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, SysUtils,
  ZGenericSqlToken, ZSqlTestCase, ZConnection, ZDataset;

type
  {** Implements a generic test case for class TZStoredProc. }
  TZAbstactTransactionTestCase = class(TZAbstractCompSQLTestCase)
  public
    function CreateTransaction: TZTransaction;
  end;

  TZGenericecTransactionTestCase = Class(TZAbstactTransactionTestCase)
  published
    procedure TestQueryAssignTransaction;
    procedure TestQueryTransaction;
  End;

implementation

uses ZTestConsts, Variants;

{ TZAbstactTransactionTestCase }

function TZAbstactTransactionTestCase.CreateTransaction: TZTransaction;
begin
  Result := TZTransaction.Create(nil);
  Result.Connection := Connection;
end;

{ TZGenericecTransactionTestCase }

procedure TZGenericecTransactionTestCase.TestQueryAssignTransaction;
var Query: TZQuery;
    Transaction: TZTransaction;
begin
  Query := CreateQuery;
  Check(Query <> nil);
  Transaction := CreateTransaction;
  try
    Query.Transaction := Transaction;
    FreeAndNil(Transaction);
    Check(Query.Transaction = nil, 'The Txn should be unregistered');
  finally
    FreeAndNil(Query);
    FreeAndNil(Transaction);
  end;
end;

procedure TZGenericecTransactionTestCase.TestQueryTransaction;
var Query: TZQuery;
    Transaction: TZTransaction;
    Succeeded: Boolean;
begin
  Query := CreateQuery;
  Check(Query <> nil);
  Transaction := CreateTransaction;
  try
    Check(Transaction.AutoCommit, 'the txn should be in autocommit mode');
    Connection.Connect;
    Transaction.TransactIsolationLevel := Connection.DbcConnection.GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation;
    Query.Transaction := Transaction;
    //this test should pass if the txn is in autocommit mode
    Query.SQL.Text := 'DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
    Query.ExecSQL;
    Transaction.StartTransaction;
    Succeeded := False;
    with Query do try
      CheckFalse(Transaction.AutoCommit);
      { Create prepared statement for equipment table }
      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
      CheckEquals(6, Params.Count);

      Params[5].DataType := ftDate;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsString := '\xyz\'+#13;
      Params[2].AsInteger := 7;
      Params[3].AsFloat := 1234.567;
      Params[4].AsDateTime := EncodeDate(1999, 8, 5);
      Params[5].Value := Null;
      ExecSQL;

      CheckEquals(1, RowsAffected);
      Succeeded := True;
    finally
      if Succeeded
      then Transaction.Commit
      else Transaction.Rollback;
    end;
  finally
    FreeAndNil(Transaction);
    FreeAndNil(Query);
    Connection.ExecuteDirect('DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID));
  end;
end;

initialization
  RegisterTest('component',TZGenericecTransactionTestCase.Suite);
end.
