{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Case for SQL Data Types               }
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

unit ZTestSqlTypes;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, SysUtils,
  ZGenericSqlToken, ZDataset, ZSqlTestCase;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestSQLTypesCase = class(TZAbstractCompSQLTestCase)
  private
    Query: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDateTypes;
  end;

implementation

uses Classes, ZDbcIntfs, ZCompatibility, ZAbstractRODataSet, DateUtils,
  ZTestConsts, ZSysUtils, ZTestCase;

{ TZTestSQLTypesCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLTypesCase.SetUp;
begin
  inherited SetUp;
  Query := CreateQuery;
  Query.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLTypesCase.TearDown;
begin
  Query.Close;
  Query.Free;
  inherited TearDown;
end;

{**
  Runs a test for Date, Time and DateTime SQL types.
}
procedure TZTestSQLTypesCase.TestDateTypes;
var
  NowDate: TDateTime;
  F: TField;
  TS: TZTimeStamp;
begin
  NowDate := Now();

  Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;

  // Query.RequestLive := True;
  Query.SQL.Text := 'SELECT * FROM date_values WHERE d_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.Open;

  CheckEquals(0, Query.RecordCount);
  Query.Insert;

  Query.FieldByName('d_id').AsInteger := TEST_ROW_ID;

  if (Connection.DbcConnection.GetServerProvider in [spOracle, spASA, spASE]) then begin
    //those do not have a native date or time field? Oracle for sure
    CheckEquals(Ord(ftDateTime), Ord(Query.FieldByName('d_date').DataType));
    CheckEquals(Ord(ftDateTime), Ord(Query.FieldByName('d_time').DataType));
  end else if (Connection.DbcConnection.GetServerProvider = spMSSQL) then begin //depends to serverversion + createskripts
    Check(Query.FieldByName('d_date').DataType in [ftDate, ftDateTime], 'MSSQL d_date fieldType');
    Check(Query.FieldByName('d_time').DataType in [ftTime, ftDateTime], 'MSSQL d_time fieldType');
    if Query.FieldByName('d_time').DataType <> ftTime then begin //depends to our create script
      DecodeDateTimeToTimeStamp(NowDate, TS); //sqlserver: Rounded to increments of .000, .003, or .007 seconds for Type DATETIME not DATETIME2
      TS.Fractions := 0; //keeps random fails away
      Check(TryTimeStampToDateTime(TS, NowDate));
    end;
  end else begin //those support all type
    CheckEquals(Ord(ftDate), Ord(Query.FieldByName('d_date').DataType));
    CheckEquals(Ord(ftTime), Ord(Query.FieldByName('d_time').DataType));
  end;
  CheckEquals(Ord(ftDateTime), Ord(Query.FieldByName('d_datetime').DataType));
  CheckEquals(Ord(ftDateTime), Ord(Query.FieldByName('d_timestamp').DataType));

  Query.FieldByName('d_date').AsDateTime := NowDate;
  Query.FieldByName('d_time').AsDateTime := NowDate;
  Query.FieldByName('d_datetime').AsDateTime := NowDate;
  Query.FieldByName('d_timestamp').AsDateTime := NowDate;

  f := Query.FieldByName('d_date'); //some provider do not have a datefield
  if F.DataType = ftDate then
    CheckEquals(Int(NowDate), Query.FieldByName('d_date').AsDateTime, 1e-10)
  else if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
    else CheckEquals(NowDate, F.AsDateTime, 1e-10);

  {$IFNDEF WITH_FPC_FTTIME_BUG}
  f := Query.FieldByName('d_time'); //some provider do not have a timefield
  if F.DataType = ftTime then
    if F.InheritsFrom(TZTimeField) and (TZTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(Frac(NowDate), F.AsDateTime, [dpYear..dpSec])
    else CheckEquals(Frac(NowDate), F.AsDateTime, 1e-10)
  else if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
    else CheckEquals(NowDate, F.AsDateTime, 1e-10);
  {$ENDIF}

  f := Query.FieldByName('d_datetime');
  if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
  then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
  else CheckEquals(NowDate, F.AsDateTime, 1e-10);

  f := Query.FieldByName('d_timestamp');
  if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
  then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
  else CheckEquals(NowDate, F.AsDateTime, 1e-10);
  Query.Post;

  Query.Close;
  Query.Open;

  CheckEquals(1, Query.RecordCount);
  f := Query.FieldByName('d_date'); //some provider do not have a datefield
  if F.DataType = ftDate then
    CheckEquals(Int(NowDate), Query.FieldByName('d_date').AsDateTime, 1e-10)
  else if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
    else CheckEquals(NowDate, F.AsDateTime, 1e-10);

  {$IFNDEF WITH_FPC_FTTIME_BUG}
  f := Query.FieldByName('d_time'); //some provider do not have a timefield
  if F.DataType = ftTime then
    if F.InheritsFrom(TZTimeField) and (TZTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(Frac(NowDate), F.AsDateTime, [dpYear..dpSec])
    else CheckEquals(Frac(NowDate), F.AsDateTime, 1e-10)
  else if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
    then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
    else if True then

      CheckEquals(NowDate, F.AsDateTime, 1e-10);
  {$ENDIF}

  f := Query.FieldByName('d_datetime');
  if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
  then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
  else CheckEquals(NowDate, F.AsDateTime, 1e-10);

  f := Query.FieldByName('d_timestamp');
  if F.InheritsFrom(TZDateTimeField) and (TZDateTimeField(F).SecondFractionsScale < 3)
  then CheckEqualsDate(NowDate, F.AsDateTime, [dpYear..dpSec])
  else CheckEquals(NowDate, F.AsDateTime, 1e-10);
  Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:Id';
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;
  CheckEquals(1, Query.RowsAffected);
end;

initialization
  RegisterTest('component',TZTestSQLTypesCase.Suite);
end.
