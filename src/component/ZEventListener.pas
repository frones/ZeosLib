{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                TZEventListener Classes                  }
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

unit ZEventListener;

interface

{$I ZComponent.inc}

uses ZAbstractConnection, Classes, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZDbcIntfs;

type
  TZOnEventAlert = procedure(Sender: TObject; Data: TZEventData;
    Var AddReceivedEvent: Boolean) of object;

  TZAbstractEventListener = Class(TAbstractActiveConnectionLinkedComponent)
  protected
    FCS: TCriticalSection;
    FEventNames: TStrings;
    FReceivedEvents: TStrings;
    FProperties: TStrings;
    FListener: IZEventListener;
    FCloneConnection: Boolean;
    FLockedList: TStrings;
    FOnEventAlert: TZOnEventAlert;
    procedure SetEventNames(const Value: TStrings);
    procedure SetCloneConnection(const Value: Boolean);
    procedure HandleEvents(var Event: TZEventData);
    function GetReceivedEvents: TStrings; //make threadsave copies of the received events
    procedure SetActive(Value: Boolean); override;
    procedure SetConnection(Value: TZAbstractConnection); override;
    procedure SetProperties(const Value: TStrings); virtual;
  protected
    property EventNames: TStrings read FEventNames write SetEventNames;
    property CloneConnection: Boolean read FCloneConnection write SetCloneConnection;
    property ReceivedEvents: TStrings read GetReceivedEvents;
    property OnEventAlert: TZOnEventAlert read FOnEventAlert write FOnEventAlert;
    property Properties: TStrings read FProperties write SetProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property CriticalSection: TCriticalSection read FCS;
  End;

  TZEventListener = Class(TZAbstractEventListener)
  public
    property ReceivedEvents;
  published
    property EventNames;
    property CloneConnection;
    property OnEventAlert;
    property Connection;
    property Properties;
  End;

implementation

uses SysUtils,
  ZMessages,
  ZAbstractRODataset;

{ TZAbstractEventListener }

constructor TZAbstractEventListener.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloneConnection := True;
  FEventNames := TStringList.Create;
  TStringList(FEventNames).Sorted := True; // dupIgnore only works when the TStringList is sorted
  TStringList(FEventNames).Duplicates := dupIgnore; // don't allow duplicate events
  FReceivedEvents := TStringList.Create;
  FProperties := TStringList.Create;
  FCS := TCriticalSection.Create;
end;

destructor TZAbstractEventListener.Destroy;
begin
  if Active then try
    SetActive(False);
  except end;
  FreeAndNil(FEventNames);
  FreeAndNil(FReceivedEvents);
  FreeAndNil(FCS);
  FreeAndNil(FProperties);
  inherited;
end;

function TZAbstractEventListener.GetReceivedEvents: TStrings;
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

procedure TZAbstractEventListener.HandleEvents(var Event: TZEventData);
var AddReceivedEvent: Boolean;
begin
  FCS.Enter;
  try
    AddReceivedEvent := True;
    if Assigned(OnEventAlert) then
      OnEventAlert(Self, Event, AddReceivedEvent);
    if (Event <> nil) then
      if AddReceivedEvent then begin
        FLockedList.AddObject(Event.ToString, Event);
        Event := nil;
      end else FreeAndNil(Event);
  finally
    FCS.Leave;
  end;
end;

procedure TZAbstractEventListener.SetActive(Value: Boolean);
begin
  if Value and (FConnection = nil) then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);
  if FActive <> Value then try
    if FListener = nil then begin
      if not FConnection.Connected then //may be oversized (yet) but the
        //conenction "may" create and login dialog... OTH it would be easy to create a cloned connection
        FConnection.Connect;
      FListener := FConnection.DbcConnection.GetEventListener(HandleEvents, FCloneConnection, FProperties);
      FListener.Listen(FEventNames, HandleEvents);
    end else FListener.GetConnection.CloseEventListener(FListener);
  finally
    FActive := Value;
  end;
end;

procedure TZAbstractEventListener.SetCloneConnection(const Value: Boolean);
begin
  if FCloneConnection <> Value then begin
    if FActive then
      raise EZDatabaseError.Create(Format(SOperationIsNotAllowed3, ['Active']));
    FCloneConnection := Value;
  end;
end;

procedure TZAbstractEventListener.SetConnection(Value: TZAbstractConnection);
Var WasListening: boolean;
Begin
  If (Value <> FConnection) Then Begin
    If (csDesigning in ComponentState) Then
      FConnection := Value
    Else Begin
      WasListening := FActive;
      If FActive Then
        SetActive(False);
      FConnection := Value;
      If WasListening and (Value <> nil) Then
        SetActive(True);
    End;
  End;
end;

procedure TZAbstractEventListener.SetEventNames(const Value: TStrings);
begin
  if FActive then
    FListener.Unlisten;
  FEventNames.Assign(Value);
  if FActive then
    FListener.Listen(FEventNames, HandleEvents);
end;

procedure TZAbstractEventListener.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

end.
