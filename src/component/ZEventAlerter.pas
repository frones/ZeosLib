{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                TZEventAlerter Classes                   }
{                                                         }
{                Written by EgonHugeist                   }
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
{                                 Zeos Development Group. }
{********************************************************@}

unit ZEventAlerter;

interface

{$I ZComponent.inc}

uses ZAbstractConnection, Classes, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZDbcIntfs;

type
  TZOnEventAlert = procedure(Sender: TObject; Data: TZEventOrNotification) of object;

  TZEventAlerter = Class(TAbstractActiveConnectionLinkedComponent)
  private
    FEventNames: TStrings;
    FReceivedEvents: TStrings;
    FProperties: TStrings;
    FAlerter: IZEventAlerter;
    FCS: TCriticalSection;
    FCloneConnection: Boolean;
    FEventName: TStrings;
    FLockedList: TStrings;
    FOnEventAlert: TZOnEventAlert;
    FEventDisplayFormat: String;
    procedure SetEventNames(const Value: TStrings);
    procedure SetCloneConnection(const Value: Boolean);
    procedure HandleEvents(var Event: TZEventOrNotification);
    function GetReceivedEvents: TStrings; //make threadsave copies of the received events
  protected
    procedure SetActive(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EventNames: TStrings read GetReceivedEvents write SetEventNames;
    property CloneConnection: Boolean read FCloneConnection write SetCloneConnection;
    property ReceivedEvents: TStrings read FReceivedEvents;
    property OnEventAlert: TZOnEventAlert read FOnEventAlert write FOnEventAlert;
  End;

implementation

uses SysUtils,
  ZMessages;

{ TZEventAlerter }

constructor TZEventAlerter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloneConnection := True;
  FEventNames := TStringList.Create(True);
  TStringList(FEventNames).Sorted := True;
  TStringList(FEventNames).Duplicates := dupIgnore;
  FReceivedEvents := TStringList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TZEventAlerter.Destroy;
begin
  if Active then
    SetActive(False);
  FreeAndNil(FEventNames);
  FreeAndNil(FReceivedEvents);
  FreeAndNil(FCS);
  inherited;
end;

function TZEventAlerter.GetReceivedEvents: TStrings;
var I: Integer;
begin
  FCS.Enter;
  try
    for i := FLockedList.Count -1 downto 0 do begin
      FReceivedEvents.AddObject(FlockedList[i], FlockedList.Objects[i]);
      FlockedList.Delete(I);
    end;
  finally
    FCS.Leave;
  end;
  Result := FReceivedEvents;
end;

procedure TZEventAlerter.HandleEvents(var Event: TZEventOrNotification);
begin
  FCS.Enter;
  try
    if Assigned(OnEventAlert) then
      OnEventAlert(Self, Event);
    if (Event <> nil) then
      FLockedList.AddObject(Event.ToString, Event);
    Event := nil;
  finally
    FCS.Leave;
  end;
end;

procedure TZEventAlerter.SetActive(Value: Boolean);
begin
  if FActive <> Value then try
    if FAlerter = nil then begin
      FAlerter := FConnection.DbcConnection.GetEventAlerter(HandleEvents, FCloneConnection, FProperties);
      FAlerter.Listen(FEventNames, HandleEvents);
    end else FAlerter.GetConnection.CloseEventAlerter(FAlerter);
  finally
    FActive := Value;
  end;
end;

procedure TZEventAlerter.SetCloneConnection(const Value: Boolean);
begin
  if FCloneConnection <> Value then begin
    if FActive then
      raise EZSQLException.Create(Format(SOperationIsNotAllowed3, ['Active']));
    FCloneConnection := Value;
  end;
end;

procedure TZEventAlerter.SetEventNames(const Value: TStrings);
begin
  if FActive then
    FAlerter.Unlisten;
  FEventNames.Assign(Value);
  if FActive then
    FAlerter.Listen(FEventName, HandleEvents);
end;

end.
