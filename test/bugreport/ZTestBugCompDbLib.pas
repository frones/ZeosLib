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
    procedure Test_NChar_Values;
  end;

implementation

{ ZTestCompDbLibBugReport }

function ZTestCompDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,FreeTDS_MsSQL<=6.5,FreeTDS_MsSQL-7.0,FreeTDS_MsSQL-2000,FreeTDS_MsSQL>=2005,FreeTDS_Sybase<10,FreeTDS_Sybase-10+';
end;

const
  TStr = ZWideString('ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...');
  Str3 = ZWideString('ќдной из наиболее');
  Str4 = ZWideString('тривиальных задач');
  Str5 = ZWideString('решаемых многими');

procedure ZTestCompDbLibBugReport.Test_NChar_Values;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select n_id, s_nchar, s_nvarchar from national_char_values';
    Query.Open;
    CheckEquals(0, Query.RecordCount, 'national_char_values RecordCount');
    Query.Close;
    Query.SQL.Text := 'insert into national_char_values values(:n_id, N:s_nchar, N:s_nvarchar, N:b_ntext, :s_char, :s_varchar, :b_text)';
    Query.ParamByName('n_id').AsInteger := 1;
    Query.ParamByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str3;
    Query.ParamByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str3;
    Query.ParamByName('b_ntext').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
    Query.ParamByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str3;
    Query.ParamByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str3;
    Query.ParamByName('b_text').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
    Query.ExecSQL;
    Query.ParamByName('n_id').AsInteger := 2;
    Query.ParamByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str4;
    Query.ParamByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str4;
    Query.ParamByName('b_ntext').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
    Query.ParamByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str4;
    Query.ParamByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str4;
    Query.ParamByName('b_text').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
    Query.ExecSQL;
    Query.ParamByName('n_id').AsInteger := 3;
    Query.ParamByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str5;
    Query.ParamByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str5;
    Query.ParamByName('b_ntext').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
    Query.ParamByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str5;
    Query.ParamByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := Str5;
    Query.ParamByName('b_text').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := TStr+TStr+TStr;
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

initialization
  RegisterTest('bugreport',ZTestCompDbLibBugReport.Suite);
end.
