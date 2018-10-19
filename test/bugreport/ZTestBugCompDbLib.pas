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

{$IFNDEF FPC}
type

  {** Implements a bug report test case for DbLib components. }
  ZTestCompDbLibBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test_NChar_Values;
  end;
  {$ENDIF}

implementation

{ ZTestCompDbLibBugReport }

{$IFNDEF FPC}
function ZTestCompDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,FreeTDS_MsSQL<=6.5,FreeTDS_MsSQL-7.0,FreeTDS_MsSQL-2000,FreeTDS_MsSQL>=2005,FreeTDS_Sybase<10,FreeTDS_Sybase-10+';
end;

//const
//  TStr = ZWideString('ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...');
//  Str3 = ZWideString('ќдной из наиболее');
//  Str4 = ZWideString('тривиальных задач');
//  Str5 = ZWideString('решаемых многими');
//  TStr = ZWideString(#$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430#$0438#$0431#$043E#$043B#$0435#$0435#$0020#$0442#$0440#$0438#$0432#$0438#$0430#$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434#$0430#$0447#$002C#$0020#$0440#$0435#$0448#$0430#$0435#$043C#$044B#$0445#$0020#$043C#$043D#$043E#$0433#$0438#$043C#$0438#$0020#$043A#$043E#$043B#$043B#$0435#$043A#$0442#$0438#$0432#$0430#$043C#$0438#$0020#$043F#$0440#$043E#$0433#$0440#$0430#$043C#$043C#$0438#$0441#$0442#$043E#$0432#$002C#$0020#$044F#$0432#$043B#$044F#$0435#$0442#$0441#$044F#$0020#$043F#$043E#$0441#$0442#$0440#$043E#$0435#$043D#$0438#$0435#$0020#$0438#$043D#$0444#$043E#$0440#$043C#$0430#$0446#$0438#$043E#$043D#$043D#$043E#$0439#$0020#$0441#$0438#$0441#$0442#$0435#$043C#$044B#$0020#$0434#$043B#$044F#$0020#$0430#$0432#$0442#$043E#$043C#$0430#$0442#$0438#$0437#$0430#$0446#$0438#$0438#$0020#$0431#$0438#$0437#$043D#$0435#$0441#$002D#$0434#$0435#$044F#$0442#$0435#$043B#$044C#$043D#$043E#$0441#$0442#$0438#$0020#$043F#$0440#$0435#$0434#$043F#$0440#$0438#$044F#$0442#$0438#$044F#$002E#$0020#$0412#$0441#$0435#$0020#$0430#$0440#$0445#$0438#$0442#$0435#$043A#$0442#$0443#$0440#$043D#$044B#$0435#$0020#$043A#$043E#$043C#$043F#$043E#$043D#$0435#$043D#$0442#$044B#$0020#$0028#$0431#$0430#$0437#$044B#$0020#$0434#$0430#$043D#$043D#$044B#$0445#$002C#$0020#$0441#$0435#$0440#$0432#$0435#$0440#$0430#$0020#$043F#$0440#$0438#$043B#$043E#$0436#$0435#$043D#$0438#$0439#$002C#$0020#$043A#$043B#$0438#$0435#$043D#$0442#$0441#$043A#$043E#$0435#$0020#$002E#$002E#$002E);
//  Str3 = ZWideString(#$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430#$0438#$0431#$043E#$043B#$0435#$0435);
//  Str4 = ZWideString(#$0442#$0440#$0438#$0432#$0438#$0430#$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434#$0430#$0447);
//  Str5 = ZWideString(#$0440#$0435#$0448#$0430#$0435#$043C#$044B#$0445#$0020#$043C#$043D#$043E#$0433#$0438#$043C#$0438);

procedure ZTestCompDbLibBugReport.Test_NChar_Values;
var
  Query: TZQuery;
  TStr, Str3, Str4, Str5: String;
begin
  TStr := Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199)+ Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223) +Chr(200)+Chr(201)+Chr(202)+Chr(203)+Chr(204)+Chr(205)+Chr(206)+Chr(207)+Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215);
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
{$ENDIF}

initialization
{$IFNDEF FPC}
  RegisterTest('bugreport',ZTestCompDbLibBugReport.Suite);
{$ENDIF}
end.
