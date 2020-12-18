{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                   Component Examples                    }
{                                                         }
{                    by EgonHugeist                       }
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

unit ZQueryBatchDML;

interface

uses ZConnection, ZDataSet;

procedure ExecuteQueryBatchInsert;

implementation

uses SysUtils;

procedure ExecuteQueryBatchInsert;
var Connection: TZConnection;
    Query: TZQuery;
    DMLidx: Integer;
    Succeeded: Boolean;
begin
  Connection := TZConnection.Create(nil);
  {assign everything to connect}
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Connection.Connect;
  try
    Connection.ExecuteDirect('CREATE TABLE DML_INSERT_DEMO('+
      'ID INTEGER NOT NULL,'+
      'NUM VARCHAR(32),'+
      'INSERT_TIMESTAMP DATETIME)');
    Connection.StartTransaction;
    Succeeded := False;
    try
      Query.SQL.Text := 'INSERT INTO DML_INSERT_DEMO VALUES (:ID,:NUM,:INSERT_TIMESTAMP)';
      Query.Params.BatchDMLCount := 10;
      for DMLidx := 0 to 9 do begin
        Query.Params[0].AsIntegers[DMLidx] := DMLidx +1;
        Query.Params[1].AsStrings[DMLidx] := IntToStr(DMLidx);
        Query.Params[2].AsDateTimes[DMLidx] := now;
      end;
      Query.ExecSQL;
      Assert(Query.RowsAffected = 10);
      Succeeded := True;
    finally
      if Succeeded
      then Connection.Commit
      else Connection.Rollback;
    end;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

end.
