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

unit ZTestBugCompDbLib;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZDbcIntfs, ZSqlTestCase, ZCompatibility, ZDbcProperties;

type

  {** Implements a bug report test case for DbLib components. }
  ZTestCompDbLibBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    {$IFNDEF FPC}
    procedure Test_NChar_Values;
    {$ENDIF}
    procedure BlankTest;
  end;

implementation

{ ZTestCompDbLibBugReport }

uses ZAbstractRODataset, SysUtils;

procedure ZTestCompDbLibBugReport.BlankTest;
begin
  Check(True);
end;

function ZTestCompDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,OleDB,ado,odbc_w,odbc_a';
end;

{$IFNDEF FPC}
procedure ZTestCompDbLibBugReport.Test_NChar_Values;
var
  Query: TZQuery;
  TStr, Str3, Str4, Str5: String;
begin
  TStr := Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199)+
          Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223)+
          Chr(200)+Chr(201)+Chr(202)+Chr(203)+Chr(204)+Chr(205)+Chr(206)+Chr(207)+
          Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215);
  Str3 := Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199);
  Str4 := Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215);
  Str5 := Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223);

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select n_id, s_nchar, s_nvarchar from national_char_values';
    Query.Open;
    CheckEquals(0, Query.RecordCount, 'national_char_values RecordCount');
    Query.Close;
    Query.SQL.Text := 'insert into national_char_values values(:n_id, :s_nchar, :s_nvarchar, :b_ntext, :s_char, :s_varchar, :b_text)';
    Query.ParamByName('n_id').AsInteger := 1;
    {$IFDEF WITH_FTWIDESTRING}
    Query.ParamByName('s_nchar').AsWideString := Str3;
    Query.ParamByName('s_nvarchar').AsWideString := Str3;
    Query.ParamByName('b_ntext').AsWideString := TStr+TStr+TStr;
    Query.ParamByName('s_char').AsWideString := Str3;
    Query.ParamByName('s_varchar').AsWideString := Str3;
    Query.ParamByName('b_text').AsWideString := TStr+TStr+TStr;
    {$ELSE}
    Query.ParamByName('s_nchar').AsString := GetDBTestString(Str3, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_nvarchar').AsString := GetDBTestString(Str3, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_ntext').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_char').AsString := GetDBTestString(Str3, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_varchar').AsString := GetDBTestString(Str3, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_text').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    {$ENDIF}
    Query.ExecSQL;
    Query.ParamByName('n_id').AsInteger := 2;
    {$IFDEF WITH_FTWIDESTRING}
    Query.ParamByName('s_nchar').AsWideString := Str4;
    Query.ParamByName('s_nvarchar').AsWideString := Str4;
    Query.ParamByName('b_ntext').AsWideString := TStr+TStr+TStr;
    Query.ParamByName('s_char').AsWideString := Str4;
    Query.ParamByName('s_varchar').AsWideString := Str4;
    Query.ParamByName('b_text').AsWideString := TStr+TStr+TStr;
    {$ELSE}
    Query.ParamByName('s_nchar').AsString := GetDBTestString(Str4, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_nvarchar').AsString := GetDBTestString(Str4, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_ntext').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_char').AsString := GetDBTestString(Str4, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_varchar').AsString := GetDBTestString(Str4, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_text').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    {$ENDIF}
    Query.ExecSQL;
    Query.ParamByName('n_id').AsInteger := 3;
    {$IFDEF WITH_FTWIDESTRING}
    Query.ParamByName('s_nchar').AsWideString := Str5;
    Query.ParamByName('s_nvarchar').AsWideString := Str5;
    Query.ParamByName('b_ntext').AsWideString := TStr+TStr+TStr;
    Query.ParamByName('s_char').AsWideString := Str5;
    Query.ParamByName('s_varchar').AsWideString := Str5;
    Query.ParamByName('b_text').AsWideString := TStr+TStr+TStr;
    {$ELSE}
    Query.ParamByName('s_nchar').AsString := GetDBTestString(Str5, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_nvarchar').AsString := GetDBTestString(Str5, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_ntext').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_char').AsString := GetDBTestString(Str5, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('s_varchar').AsString := GetDBTestString(Str5, Query.Connection.DbcConnection.GetConSettings);
    Query.ParamByName('b_text').AsString := GetDBTestString(TStr+TStr+TStr, Query.Connection.DbcConnection.GetConSettings);
    {$ENDIF}
    Query.ExecSQL;
    Query.SQL.Text := 'select n_id, s_nchar, s_nvarchar, s_char, s_varchar from national_char_values';
    Query.Open;
    CheckEquals(5, Query.FieldCount, 'The SQL >' + Query.SQL.Text + '< returned less fields than were expected.');
    {$IFNDEF UNICODE}
    if Connection.ControlsCodePage = cCP_UTF8 then
    begin
      CheckEquals(UTF8Encode(Str3), Query.FieldByName('s_nchar').AsString, 's_nchar value');
      CheckEquals(UTF8Encode(Str3), Query.FieldByName('s_nvarchar').AsString, 's_nvarchar value');
      CheckEquals(UTF8Encode(Str3), Query.FieldByName('s_char').AsString, 's_char value');
      CheckEquals(UTF8Encode(Str3), Query.FieldByName('s_varchar').AsString, 's_varchar value');
      Query.Next;
      CheckEquals(UTF8Encode(Str4), Query.FieldByName('s_nchar').AsString, 's_nchar value');
      CheckEquals(UTF8Encode(Str4), Query.FieldByName('s_nvarchar').AsString, 's_nvarchar value');
      CheckEquals(UTF8Encode(Str4), Query.FieldByName('s_char').AsString, 's_char value');
      CheckEquals(UTF8Encode(Str4), Query.FieldByName('s_varchar').AsString, 's_varchar value');
      Query.Next;
      CheckEquals(UTF8Encode(Str5), Query.FieldByName('s_nchar').AsString, 's_nchar value');
      CheckEquals(UTF8Encode(Str5), Query.FieldByName('s_nvarchar').AsString, 's_nvarchar value');
      CheckEquals(UTF8Encode(Str5), Query.FieldByName('s_char').AsString, 's_char value');
      CheckEquals(UTF8Encode(Str5), Query.FieldByName('s_varchar').AsString, 's_varchar value');
    end
    else
    {$ENDIF}
    begin
      CheckEquals(Str3, Query.FieldByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nchar value');
      CheckEquals(Str3, Query.FieldByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nvarchar value');
      CheckEquals(Str3, Query.FieldByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_char value');
      CheckEquals(Str3, Query.FieldByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_varchar value');
      Query.Next;
      CheckEquals(Str4, Query.FieldByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nchar value');
      CheckEquals(Str4, Query.FieldByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nvarchar value');
      CheckEquals(Str4, Query.FieldByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_char value');
      CheckEquals(Str4, Query.FieldByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_varchar value');
      Query.Next;
      CheckEquals(Str5, Query.FieldByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nchar value');
      CheckEquals(Str5, Query.FieldByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_nvarchar value');
      CheckEquals(Str5, Query.FieldByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_char value');
      CheckEquals(Str5, Query.FieldByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF}, 's_varchar value');
    end;
  finally
    Query.Properties.Values[DSProps_ValidateUpdateCount] := '-1';
    Query.SQL.Text := 'delete from national_char_values';
    Query.ExecSQL;
    Query.Free;
  end;
end;
{$ENDIF}

initialization
  RegisterTest('bugreport',ZTestCompDbLibBugReport.Suite);
end.
