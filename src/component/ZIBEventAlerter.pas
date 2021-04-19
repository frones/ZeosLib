{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{            Written by Sergey Merkuriev                  }
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

{ Constributors:
  MS (Michael Seeger),
  NB
  EgonHugeist (replace all code and use anchestor class TZAbstractEventListener instead)
}

unit ZIBEventAlerter;

{$I ZComponent.inc}

interface

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
uses
  SysUtils, Classes,
  ZDbcFirebirdInterbase, ZEventListener, ZAbstractConnection;

type

  TEventAlert = TZFirebirdInterbaseEventAlert;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: integer) of object;

  TZIBEventAlerter = class(TZAbstractEventListener)
  private
    FOnEventAlert: TEventAlert;
    FOnError: TErrorEvent;
    FAutoRegister: boolean;
    FAlerter: IZFirebirdInterbaseEventAlerter;
  protected
    { Protected declarations }
    procedure EventChange(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AfterListenerAssigned; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure RegisterEvents; deprecated {$IFDEF WITH_DEPRECATED_MESSAGE}'Use Active property instead'{$ENDIF};
    procedure UnRegisterEvents; virtual; deprecated {$IFDEF WITH_DEPRECATED_MESSAGE}'Use Active property instead'{$ENDIF};
    procedure SetAutoRegister(const Value: boolean);
  published
    { Published declarations }
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister;
    property CloneConnection;
    property Connection;
    property Active;
    property Events;
    property Registered: boolean read FActive write SetActive;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit

uses ZPlainFirebirdInterbaseDriver;

{ TZIBEventAlerter }

procedure TZIBEventAlerter.AfterListenerAssigned;
begin
  FAlerter := FListener as IZFirebirdInterbaseEventAlerter;
  if Assigned(FOnEventAlert) then
    FAlerter.SetOnEventAlert(FOnEventAlert);
end;

constructor TZIBEventAlerter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TStringList(FEventNames).OnChange := EventChange; // assign the routine which validates the event lenghts
end;


{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Sender" not used} {$ENDIF}
procedure TZIBEventAlerter.EventChange(Sender: TObject);
var
  i: integer;
  WasActive: boolean;
begin
  WasActive := FActive;
  try
    if WasActive then
      SetActive(False);
    TStringList(FEventNames).OnChange := nil;
    try
      for i := (FEventNames.Count - 1) downto 0 do
        if (FEventNames[i] = EmptyStr) then
          FEventNames.Delete(i)
        else if (Length(FEventNames[i]) > (IB_MAX_EVENT_LENGTH - 1)) then
          FEventNames[i] := Copy(FEventNames[i], 1, (IB_MAX_EVENT_LENGTH - 1));
    finally
      TStringList(FEventNames).OnChange := EventChange;
    end;
  finally
    if WasActive and (FEventNames.Count > 0) then
      SetActive(True);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZIBEventAlerter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then begin
    if FActive then
      SetActive(False);
    FConnection := nil;
  end;
end;

Procedure TZIBEventAlerter.RegisterEvents;
Begin
  If (not (csDesigning in ComponentState)) and (Assigned(FConnection)) then
    SetActive(True);
End;


procedure TZIBEventAlerter.SetAutoRegister(const Value: boolean);
begin
  if FAutoRegister <> Value then begin
    FAutoRegister := Value;
    if FAutoRegister and (not FActive)and
      Assigned(FConnection) and FConnection.Connected then
      SetActive(True)
  end;
end;

procedure TZIBEventAlerter.UnregisterEvents;
begin
  if not (csDesigning in ComponentState) then
    SetActive(False);
end;

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
end.
