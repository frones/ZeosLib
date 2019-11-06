{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for SQL String Classes             }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestSqlStrings;

interface
{$I ZComponent.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZTestCase, ZSqlStrings, ZGenericSqlToken;

type

  {** Implements a test case for class TZSqlStrings. }
  TZTestSQLStringsCase = class(TZAbstractTestCase)
  private
    SQLStrings: TZSQLStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStatements;
    procedure TestParams;
    procedure TestParamChar;
    procedure TestUncompleted;
    procedure TestTicket384;
 end;

implementation

uses Classes;

{ TZTestSqlStringsCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLStringsCase.SetUp;
begin
  SQLStrings := TZSQLStrings.Create;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLStringsCase.TearDown;
begin
  SQLStrings.Free;
  SQLStrings := nil;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestSQLStringsCase.TestParams;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(4, SQLStrings.ParamCount);
  CheckEquals('ID', SQLStrings.ParamNames[0]);
  CheckEquals('NAME', SQLStrings.ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.ParamNames[2]);
  CheckEquals('NEW_NAME', SQLStrings.ParamNames[3]);

  SQLScript := 'INSERT INTO department VALUES (:NOT, :ORDER, :LIKE);'
    + #10 + 'UPDATE department SET dep_name=:"quoted", dep_address=:LIKE'
    + ' WHERE id_dep=:NOT AND dep_name=:"quoted identifier";';
  SQLStrings.Text := SQLScript;

  CheckEquals(5, SQLStrings.ParamCount);
  CheckEquals('NOT', SQLStrings.ParamNames[0]);
  CheckEquals('ORDER', SQLStrings.ParamNames[1]);
  CheckEquals('LIKE', SQLStrings.ParamNames[2]);
  CheckEquals('quoted', SQLStrings.ParamNames[3]);
  CheckEquals('quoted identifier', SQLStrings.ParamNames[4]);


  SQLStrings.Clear;
  CheckEquals(0, SQLStrings.ParamCount);

  //see http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
  SQLScript := 'select IsNull(B.X||B.y, '+QuotedStr('')+') as X_Y_Z from "My_Table" as B '+
    'where 1=1 and :user_ID = B.x and ((B.nazwa iLike :nazwaFiltr) or (B.skrot iLike :nazwaFiltr))';
  SQLStrings.Text := SQLScript;
  CheckEquals(2, SQLStrings.ParamCount);
  CheckEquals('user_ID', SQLStrings.ParamNames[0]);
  CheckEquals('nazwaFiltr', SQLStrings.ParamNames[1]);
  SQLStrings.Clear;

  //See http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=98333
  SQLScript := 'SELECT * FROM sys_users WHERE login = :login';
  SQLStrings.Text := SQLScript;
  CheckEquals(1, SQLStrings.ParamCount);
  CheckEquals('login', SQLStrings.ParamNames[0]);
  SQLStrings.Clear;

  SQLScript := 'SELECT * FROM users WHERE username = :username;';
  SQLStrings.Text := SQLScript;
  CheckEquals(1, SQLStrings.ParamCount);
  CheckEquals('username', SQLStrings.ParamNames[0]);
  SQLStrings.Clear;

  //See: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
  SQLScript := 'SELECT * FROM sys_users WHERE login = :user_id';
  SQLStrings.Text := SQLScript;
  CheckEquals(1, SQLStrings.ParamCount);
  CheckEquals('user_id', SQLStrings.ParamNames[0]);
  SQLStrings.Clear;
end;

{**
  Runs a test for SQL parameters delimited by non standard parameter marker.
}
procedure TZTestSQLStringsCase.TestParamChar;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;
  CheckEquals(4, SQLStrings.ParamCount);
  SQLStrings.ParamChar := '&';
  CheckEquals(0, SQLStrings.ParamCount);

  SQLScript := 'INSERT INTO department VALUES (&ID, &NAME, &NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=&NEW_NAME, dep_address=&NEW_ADDRESS'
    + ' WHERE id_dep=&Id AND dep_name=&Name;';
  SQLStrings.Text := SQLScript;
  CheckEquals(4, SQLStrings.ParamCount);
  CheckEquals('ID', SQLStrings.ParamNames[0]);
  CheckEquals('NAME', SQLStrings.ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.ParamNames[2]);
  CheckEquals('NEW_NAME', SQLStrings.ParamNames[3]);

  try
    // Failure expected when ParamChar isn't seen as a Symbol by the Tokenizer
    // U is interpreted as the start of a normal word by all tokenizers
    SQLStrings.ParamChar := 'U';
    Fail('Wrong behaviour when setting ParamChar to U');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
end;

{**
  Runs a test for SQL statements.
}
procedure TZTestSQLStringsCase.TestStatements;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('INSERT INTO department VALUES (?, ?, ?)',
    SQLStrings.Statements[0].SQL);
  CheckEquals(3, SQLStrings.Statements[0].ParamCount);
  CheckEquals('ID', SQLStrings.Statements[0].ParamNames[0]);
  CheckEquals('NAME', SQLStrings.Statements[0].ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[0].ParamNames[2]);

  CheckEquals('UPDATE department SET dep_name=?, dep_address=?'
    + ' WHERE id_dep=? AND dep_name=?', SQLStrings.Statements[1].SQL);
  CheckEquals(4, SQLStrings.Statements[1].ParamCount);
  CheckEquals('NEW_NAME', SQLStrings.Statements[1].ParamNames[0]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[1].ParamNames[1]);
  CheckEquals('ID', SQLStrings.Statements[1].ParamNames[2]);
  CheckEquals('NAME', SQLStrings.Statements[1].ParamNames[3]);
end;

(*
Hello,
today I have discovered a regression in 7.3alpha (since revision 4629).
The TZSQLStrings does not correctly unescape colons in SQL
(e.g. EXECUTE BLOCK with variables). Attached is a test and a patch.
Best regards, Joe
*)
procedure TZTestSQLStringsCase.TestTicket384;
begin
  SQLStrings.Text :=
      'EXECUTE BLOCK AS '#10+
      'DECLARE CNT INT; '#10+
      'BEGIN'#10+
      'SELECT COUNT(*) FROM table1 WHERE key=1 INTO ::CNT;'#10+
      'IF (CNT=0) THEN'#10+
      'INSERT INTO table1 (key,value) VALUES (1, ''bug'');'#10+
      'END';
  Check((SQLStrings.ParamCount <> 0) or (SQLStrings.StatementCount <> 1)
    or (SQLStrings.Statements[0].SQL <> 'EXECUTE BLOCK AS DECLARE CNT INT; BEGIN SELECT COUNT(*) FROM table1 WHERE key=1 INTO :CNT; IF (CNT=0) THEN INSERT INTO table1 (key,value) VALUES (1, ''bug''); END'),
      'BUGCHECK! Ticked384');
end;

{**
  Runs a test for uncompleted SQL statements.
}
procedure TZTestSQLStringsCase.TestUncompleted;
var
  SQLScript: string;
begin
  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);

  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);
end;

initialization
  RegisterTest('component',TZTestSQLStringsCase.Suite);
end.
