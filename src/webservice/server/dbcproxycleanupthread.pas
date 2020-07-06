unit dbcproxycleanupthread;

{$mode delphiunicode}{$H+}

interface

uses
  Classes, SysUtils, DbcProxyConnectionManager;

type
  TDbcProxyCleanupThread = class(TThread)
    private
      FShouldExit: Boolean;
      FConnManager: TDbcProxyConnectionManager;
    protected
      procedure Execute; override;
    public
      constructor Create(ConnManager: TDbcProxyConnectionManager);
  end;

implementation

uses
  DateUtils, ZDbcProxyManagement;

constructor TDbcProxyCleanupThread.Create(ConnManager: TDbcProxyConnectionManager);
begin
  inherited Create(True);
  FShouldExit := false;
  FConnManager := ConnManager;
end;

procedure TDbcProxyCleanupThread.Execute;
var
  MaxTime: TDateTime;
  x: SizeInt;
  Conn: TDbcProxyConnection;
  ConnID: String;
  y: Integer;
begin
  y := 0;
  while not FShouldExit do begin
    y := y mod 60;
    if y = 0 then begin
      MaxTime := IncMinute(Now, 24 * 60);
      for X := FConnManager.GetConnectionCount - 1 downto 0 do begin
        Conn := FConnManager.LockConnection(x);
        try
          if Conn.LastAccessTime <= MaxTime then begin
            ConnID := Conn.ID;
            Conn.ZeosConnection.Close;
          end else begin
            ConnID := '';
          end;
        finally
          Conn.Unlock;
        end;
        if ConnID <> '' then
          FConnManager.RemoveConnection(ConnID);
      end;
    end;
    inc(y);
    Sleep(1000);
  end;
end;

end.

