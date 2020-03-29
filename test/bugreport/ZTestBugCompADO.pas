{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for DbLib Component Bug Reports        }
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

unit ZTestBugCompADO;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataSet, DB, ZDbcIntfs, ZSqlTestCase, ZCompatibility;

type

  {** Implements a bug report test case for DbLib components. }
  ZTestCompADOBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
    procedure TestTrailingSpaces; //EH y2020 commented out see: https://www.sqlservercentral.com/forums/topic/trailing-spaces-being-automatically-trimmed
  published
    procedure TestNotNullValues;
    procedure TestADQA_All_types;
  end;

implementation

uses SysUtils, ZTestCase;

{ ZTestCompDbLibBugReport }

function ZTestCompADOBugReport.GetSupportedProtocols: string;
begin
  Result := 'ado;odbc_w;odbc_a;OleDB;mssql;sybase';
end;

procedure ZTestCompADOBugReport.TestTrailingSpaces;
const
  RowID = 500;
  TestString: String = 'TrailingSpaces    ';
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'select * from people';
    Query.Open;
    CheckEquals(ord(ftSmallInt), ord(Query.Fields[0].DataType));
    CheckEquals(ord(ftSmallInt), ord(Query.Fields[1].DataType));
    CheckStringFieldType(Query.Fields[2].DataType, Connection.ControlsCodePage);
    Check(Query.Fields[3].DataType in [ftDateTime, ftTime]); //create scripts?
    Check(Query.Fields[4].DataType in [ftDateTime, ftTime]); //create scripts?
    CheckEquals(ord(ftBlob), ord(Query.Fields[5].DataType));
    CheckMemoFieldType(Query.Fields[6].DataType, Connection.ControlsCodePage);
    {$IFDEF WITH_FTBYTE}
    CheckEquals(ord(ftByte), ord(Query.Fields[7].DataType));
    {$ELSE}
    CheckEquals(ord(ftWord), ord(Query.Fields[7].DataType));
    {$ENDIF}
    CheckEquals('Vasia Pupkin', Query.Fields[2].AsString);

    Query.Append;
    Query.Fields[0].AsInteger := RowID;
    Query.Fields[2].AsString  := TestString;
    Query.Post;
    Query.Close;
    Query.SQL.Text := 'select * from people where p_id = '+IntToStr(RowID);
    Query.Open;
    CheckEquals(TestString, Query.Fields[2].AsString);
  finally
    Query.SQL.Text := 'delete from people where p_id = '+IntToStr(RowID);
    Query.ExecSQL;
    Query.Free;
  end;
end;

procedure ZTestCompADOBugReport.TestADQA_All_types;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'select * from ADQA_All_types';
    Query.Open;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompADOBugReport.TestNotNullValues;
const
  RowID = 0;
  TestString = String('NullValues');
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'select * from people';
    Query.Open;
    CheckEquals(ord(ftSmallInt), ord(Query.Fields[0].DataType));
    CheckEquals(ord(ftSmallInt), ord(Query.Fields[1].DataType));
    CheckStringFieldType(Query.Fields[2].DataType, Connection.ControlsCodePage);
    Check(Query.Fields[3].DataType in [ftDateTime, ftTime]); //create scripts?
    Check(Query.Fields[4].DataType in [ftDateTime, ftTime]); //create scripts?
    CheckEquals(ord(ftBlob), ord(Query.Fields[5].DataType));
    CheckMemoFieldType(Query.Fields[6].DataType, Connection.ControlsCodePage);
    {$IFDEF WITH_FTBYTE}
    CheckEquals(ord(ftByte), ord(Query.Fields[7].DataType));
    {$ELSE}
    CheckEquals(ord(ftWord), ord(Query.Fields[7].DataType));
    {$ENDIF}
    CheckEquals('Vasia Pupkin', Query.Fields[2].AsString);

    Query.Append;
    Query.Fields[0].AsInteger := RowID;
    Query.Fields[2].AsString  := TestString;
    Query.Post;
    Query.Close;
    Query.Open;
    Query.First;
    CheckEquals(TestString, Query.Fields[2].AsString);
  finally
    Query.SQL.Text := 'delete from people where p_id = '+IntToStr(RowID);
    Query.ExecSQL;
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompADOBugReport.Suite);
end.


