unit ZDbcProxyManagement;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ZDbcIntfs, SyncObjs;

type
  TDbcProxyConnection = class
  protected
    FID: String;
    FZeosConnection: IZConnection;
    FLastAccessTime: TDateTime;
    FCriticalSection: TCriticalSection;
  public
    constructor Create(AConnection: IZConnection); virtual;
    destructor Destroy; override;
    property ZeosConnection: IZConnection read FZeosConnection;
    property ID: String read FID;
    property LastAccessTime: TDateTime read FLastAccessTime;
    procedure Lock;
    procedure Unlock;
  end;

procedure RaiseNotImplemented(FunctionName: String);

implementation

procedure RaiseNotImplemented(FunctionName: String);
begin
  raise Exception.Create('Function ' + FunctionName + ' is not implemented yet!');
end;

constructor TDbcProxyConnection.Create(AConnection: IZConnection);
var
  UUID: TGuid;
begin
  FCriticalSection := TCriticalSection.Create;
  CreateGUID(UUID);
  FID := GUIDToString(UUID);
  FZeosConnection := AConnection;
  FLastAccessTime := Now;
end;

destructor TDbcProxyConnection.Destroy;
begin
  if Assigned(FCriticalSection) then
    FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TDbcProxyConnection.Lock;
begin
  FLastAccessTime := Now;
  FCriticalSection.Enter;
end;

procedure TDbcProxyConnection.Unlock;
begin
  FLastAccessTime := Now;
  FCriticalSection.Leave;
end;

end.

