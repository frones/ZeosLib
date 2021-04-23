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

uses Classes, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZAbstractConnection, ZDbcIntfs;

type
  TZOnEventAlert = procedure(Sender: TObject; Data: TZEventData;
    Var CancelListening: Boolean) of object;

  TZAbstractEventListener = Class(TAbstractActiveConnectionLinkedComponent)
  protected
    FCS: TCriticalSection;
    FEventNames: TStrings;
    FProperties: TStrings;
    FListener: IZEventListener;
    FCloneConnection: Boolean;
    FLockedList: TStrings;
    FOnEventAlert: TZOnEventAlert;
    procedure SetEventNames(const Value: TStrings);
    procedure SetCloneConnection(const Value: Boolean);
    procedure HandleEvents(var Event: TZEventData);
    procedure SetActive(Value: Boolean); override;
    procedure AfterListenerAssigned; virtual;
    procedure SetConnection(Value: TZAbstractConnection); override;
    procedure SetProperties(const Value: TStrings); virtual;
    function GetClonedDbcConnection: IZConnection;
  protected
    {$IF defined(ENABLE_INTERBASE) or defined(ENABLE_FIREBIRD) or defined(ENABLE_POSTGRESQL)}
    property Events: TStrings read FEventNames write SetEventNames;
    {$IFEND}
    property EventNames: TStrings read FEventNames write SetEventNames;
    property OnEventAlert: TZOnEventAlert read FOnEventAlert write FOnEventAlert;
    property Properties: TStrings read FProperties write SetProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property CriticalSection: TCriticalSection read FCS;
    property Active;
  published
    property Connection;
    property CloneConnection: Boolean read FCloneConnection write SetCloneConnection;
  End;

  TZEventListener = Class(TZAbstractEventListener)
  published
    property EventNames;
    property OnEventAlert;
    property Properties;
  End;

implementation

uses SysUtils,
  ZMessages,
  ZAbstractRODataset;

{ TZAbstractEventListener }

procedure TZAbstractEventListener.AfterListenerAssigned;
begin
  //just a dummy for descendants
end;

constructor TZAbstractEventListener.Create(AOwner: TComponent);
var I: Integer;
begin
  inherited Create(AOwner);
  FCloneConnection := True;
  FEventNames := TStringList.Create;
  TStringList(FEventNames).Sorted := True; // dupIgnore only works when the TStringList is sorted
  TStringList(FEventNames).Duplicates := dupIgnore; // don't allow duplicate events
  FProperties := TStringList.Create;
  FCS := TCriticalSection.Create;
  if (csDesigning in ComponentState) and Assigned(AOwner) then
    for I := AOwner.ComponentCount - 1 downto 0 do
      if AOwner.Components[I] is TZAbstractConnection then begin
        FConnection := AOwner.Components[I] as TZAbstractConnection;
        Break;
      end;
end;

destructor TZAbstractEventListener.Destroy;
begin
  if Active then try
    SetActive(False);
  except end;
  FreeAndNil(FEventNames);
  FreeAndNil(FCS);
  FreeAndNil(FProperties);
  inherited;
end;

type TZProtectedConenction = Class(TZAbstractConnection);
function TZAbstractEventListener.GetClonedDbcConnection: IZConnection;
begin
  //EH: Set the attachment charsset, AuotEncode, and ControlsCP again
  //if the user did clear the properties then this info is lost
  //See https://sourceforge.net/p/zeoslib/tickets/329/
  Result := DriverManager.GetConnection(TZProtectedConenction(FConnection).ConstructURL(FConnection.User, FConnection.Password));
  FConnection.ShowSqlHourGlass;
  try
    with Result do begin
      SetReadOnly(True);
      SetCatalog(FConnection.Catalog);
      SetUseMetadata(False);
      SetAddLogMsgToExceptionOrWarningMsg(FConnection.AddLogMsgToExceptionOrWarningMsg);
      SetRaiseWarnings(FConnection.RaiseWarningMessages);
      Open;
    end;
  finally
    if Assigned(Result) And Result.IsClosed then
      Result := nil;
    FConnection.HideSQLHourGlass;
  end;
end;

procedure TZAbstractEventListener.HandleEvents(var Event: TZEventData);
var CancelListening: Boolean;
begin
  FCS.Enter;
  try
    CancelListening := False;
    if Assigned(OnEventAlert) then
      OnEventAlert(Self, Event, CancelListening);
    FreeAndNil(Event);
    if CancelListening then
      SetActive(False);
  finally
    FCS.Leave;
  end;
end;

procedure TZAbstractEventListener.SetActive(Value: Boolean);
var Con: IZConnection;
begin
  if Value and (FConnection = nil) then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);
  Con := nil;
  if FActive <> Value then try
    if FListener = nil then begin
      if FCloneConnection
      then Con := GetClonedDbcConnection
      else begin
        if not FConnection.Connected then
          FConnection.Connected;
        Con := FConnection.DbcConnection;
      end;
      if Con.IsClosed then
        Con.open;
      FListener := Con.GetEventListener(HandleEvents, False, FProperties);
      AfterListenerAssigned;
      FListener.Listen(FEventNames, HandleEvents);
    end else begin
      Con := FListener.GetConnection;
      Con.CloseEventListener(FListener);
    end;
    FActive := Value;
  finally
    Con := nil;
  end;
end;

procedure TZAbstractEventListener.SetCloneConnection(const Value: Boolean);
begin
  if FCloneConnection <> Value then begin
    if FActive then
      raise EZDatabaseError.Create(SConnectionIsOpened);
    FCloneConnection := Value;
  end;
end;

procedure TZAbstractEventListener.SetConnection(Value: TZAbstractConnection);
Var WasListening: boolean;
Begin
  If (Value <> FConnection) then begin
    If (csDesigning in ComponentState) Then
      FConnection := Value
    else begin
      WasListening := FActive;
      if FActive then
        SetActive(False);
      FConnection := Value;
      if WasListening and (Value <> nil) then
        SetActive(True);
    end;
  end;
end;

procedure TZAbstractEventListener.SetEventNames(const Value: TStrings);
var I: Integer;
begin
  if FActive then
    FListener.Unlisten;
  FEventNames.Clear;
  for i := 0 to Value.Count-1 do
    FEventNames.AddObject(Trim(Value[i]), Value.Objects[i]);
  if FActive then
    FListener.Listen(FEventNames, HandleEvents);
end;

procedure TZAbstractEventListener.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

end.
