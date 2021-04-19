{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Postgres Database Connectivity Classes         }
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

{*********************************************************}
{                                                         }
{ TZPgEventAlerter, Asynchronous notifying.               }
{   By Ivan Rog - 2010                                    }
{                                                         }
{ Contributors:                                           }
{   EgonHugeist replace all code by using the             }
{     TZAbstractEventAllerter base class                  }
{   Silvio Clecio - http://silvioprog.com.br              }
{                                                         }
{*********************************************************}

unit ZPgEventAlerter;

interface
{$I ZComponent.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, ZEventListener, ZDbcPostgreSql;

type
  TZPgNotifyEvent = procedure(Sender: TObject; Event: string;
    ProcessID: Integer; Payload: string; var CancelListening: Boolean) of object;

  { TZPgEventAlerter }

  TZPgEventAlerter = class (TZAbstractEventListener)
  private
    FPGListener: IZPostgresEventListener;
    FNotifyFired: TZPgNotifyEvent;
    FInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
  protected
    procedure AfterListenerAssigned; override;
    procedure HandlePGNotification(const Event: string; ProcessID: Integer; Payload: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CloneConnection default True;
    property Interval: Cardinal read FInterval write SetInterval default 250;
    property OnNotify: TZPgNotifyEvent read FNotifyFired write FNotifyFired;
    property Events;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit


uses ZMessages, ZAbstractRODataset;

{ TZPgEventAlerter }

procedure TZPgEventAlerter.AfterListenerAssigned;
begin
  FPGListener := FListener as IZPostgresEventListener;
  if Assigned(FNotifyFired) then
    FPGListener.SetOnPgNotifyEvent(HandlePGNotification);
  FPGListener.SetListenerInterval(FInterval);
end;

constructor TZPgEventAlerter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 250;
  FCloneConnection := True;
end;

procedure TZPgEventAlerter.HandlePGNotification(const Event: string;
  ProcessID: Integer; Payload: string);
var CancelListening: Boolean;
begin
  FCS.Enter;
  try
    CancelListening := False;
    if Assigned(FNotifyFired)  then
      FNotifyFired(Self, Event, ProcessID, PayLoad, CancelListening);
    if CancelListening then
      SetActive(False);
  finally
    FCS.Leave
  end;
end;

procedure TZPgEventAlerter.SetInterval(Value: Cardinal);
begin
  if Value = 0 then
    FInterval := 250;
  if Value <> FInterval then begin
    FInterval := Value;
    if FPGListener <> nil then
      FPGListener.SetListenerInterval(Value);
  end;
end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
