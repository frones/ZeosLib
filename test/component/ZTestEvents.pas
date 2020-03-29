{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for EventAlerter Components        }
{                                                         }
{          Originally written by Zeos Team                }
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

unit ZTestEvents;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  Classes,
  {$IFDEF ENABLE_INTERBASE}
  ZIBEventAlerter,
  {$ENDIF}
  ZSysUtils, ZSqlTestCase;

{$IFDEF ENABLE_INTERBASE}
type
  {** Implements a test case for class TZStoredProc. }
  TZTestInterbaseEventAlert = class(TZAbstractCompSQLTestCase)
  protected
    Events: array of record
      EventName: string;
      EventCount: Integer;
    end;
//    function GetSupportedProtocols: string; override;
    function SupportsConfig(Config: TZConnectionConfig): Boolean; override;
    procedure ZIBEventAlerterEventAlert(Sender: TObject; EventName: string;
      EventCount: Integer; var CancelAlerts: Boolean);
  published
    procedure TestIBAllerter;
  end;
{$ENDIF}

implementation

{$IFDEF ENABLE_INTERBASE}

uses ZdbcIntfs;

{ TZTestInterbaseEventAlert }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
//function TZTestInterbaseEventAlert.GetSupportedProtocols: string;
//begin
//  Result := pl_all_interbase;
//end;

function TZTestInterbaseEventAlert.SupportsConfig(Config: TZConnectionConfig): Boolean;
begin
  Result := (Config.Transport = traNative) and (Config.Provider = spIB_FB);
end;

procedure TZTestInterbaseEventAlert.ZIBEventAlerterEventAlert(Sender: TObject; EventName: string;
  EventCount: Integer; var CancelAlerts: Boolean);
begin
  SetLength(Events, Length(Events) + 1);
  Events[High(Events)].EventName := EventName;
  Events[High(Events)].EventCount := EventCount;
end;

procedure TZTestInterbaseEventAlert.TestIBAllerter;

  procedure PostEvents(const EventsToSend: array of string);
  var
    sql: string;
    i: Integer;
  begin
    sql := 'EXECUTE BLOCK AS BEGIN ';
    for i := Low(EventsToSend) to High(EventsToSend) do
      sql := sql + 'POST_EVENT '''+EventsToSend[i]+''';';
    sql := sql + 'END';
    Connection.ExecuteDirect(sql);
  end;

  function JoinArr(const Arr: array of string): string; overload;
  var i: Integer;
  begin
    for i := Low(Arr) to High(Arr) do
      AppendSepString(Result, Arr[i], ';');
  end;

  function JoinArr(const Arr: array of Integer): string; overload;
  var i: Integer;
  begin
    for i := Low(Arr) to High(Arr) do
      AppendSepString(Result, IntToStr(Arr[i]), ';');
  end;

  procedure TestEvents(
    const EventsToSend: array of string;
    const EventNamesExpect: array of string;
    const EventCountsExpect: array of Integer);
  var
    StartTicks: Cardinal;
    i: Integer;
    Descr: string;
  const
    TimeToWait = 500; // [ms]
  begin
    // prepare
    SetLength(Events, 0);
    StartTicks := GetTickCount;
    // post
    PostEvents(EventsToSend);
    // We can't realize when posted events arrive completely so just wait some time
    while GetTickCount - StartTicks <= TimeToWait do
      CheckSynchronize(100);
    // check
    Descr := Format('Posted events: [%s], expect [%s]',
      [JoinArr(EventsToSend), JoinArr(EventNamesExpect)]);
    CheckEquals(Length(EventNamesExpect), Length(Events), Descr + ' lengths differ');
    for i := Low(Events) to High(Events) do
    begin
      CheckEquals(EventNamesExpect[i], Events[i].EventName, Descr + ' differs event #'+IntToStr(i));
      CheckEquals(EventCountsExpect[i], Events[i].EventCount, Descr + ' differs event #'+IntToStr(i));
    end;
  end;

var
  IBEvents: TZIBEventAlerter;
begin
  Connection.Connect;
  IBEvents := TZIBEventAlerter.Create(nil);
  try
    // init
    IBEvents.Connection := Connection;
    IBEvents.OnEventAlert := ZIBEventAlerterEventAlert;
    IBEvents.Events.Add('ev1');
    IBEvents.Events.Add('ev2');
    IBEvents.Events.Add('ev3');
    IBEvents.Events.Add('ev4');
    IBEvents.RegisterEvents;
    CheckSynchronize(1000); // !

    // do tests
    TestEvents(['ev1'], ['ev1'], [1]);
    TestEvents(['ev1', 'ev2', 'ev3', 'ev4'], ['ev1', 'ev2', 'ev3', 'ev4'], [1, 1, 1, 1]);
    TestEvents(['ev1', 'ev2', 'ev2', 'ev4'], ['ev1', 'ev2', 'ev4'], [1, 2, 1]);
    TestEvents(['ev4', 'ev2', 'ev1', 'ev3'], ['ev1', 'ev2', 'ev3', 'ev4'], [1, 1, 1, 1]);
  finally
    IBEvents.Free;
  end;
end;

{$ENDIF}

initialization
  {$IFDEF ENABLE_INTERBASE}
  RegisterTest('component',TZTestInterbaseEventAlert.Suite);
  {$ENDIF}
end.
