unit DbcProxyFileLogger;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}Classes, SysUtils, SyncObjs;

type
  TDbcProxyLogger = class
    public
      procedure Error(MessageStr: String); virtual; abstract;
      procedure Warning(MessageStr: String); virtual; abstract;
      procedure Info(MessageStr: String); virtual; abstract;
      procedure Debug(MessageStr: String); virtual; abstract;
  end;

  TDbcProxyWritelnLogger = class(TDbcProxyLogger)
    protected
      procedure Log(MessageStr: String); virtual; abstract;
    public
      procedure Error(MessageStr: String); override;
      procedure Warning(MessageStr: String); override;
      procedure Info(MessageStr: String); override;
      procedure Debug(MessageStr: String); override;
  end;

  TDbcProxyFileLogger = class(TDbcProxyWritelnLogger)
    protected
      FFileOpened: Boolean;
      FLogFile: TextFile;
      FFileLock: TCriticalSection;
      procedure Log(MessageStr: String); override;
    public
      constructor Create(LogFileName: String); virtual;
      destructor Destroy; override;
  end;

  TDbcProxyConsoleLogger = class(TDbcProxyWritelnLogger)
    protected
      FLock: TCriticalSection;
      FConsoleAvailable: Boolean;
      procedure Log(MessageStr: String); override;
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

procedure TDbcProxyWritelnLogger.Error(MessageStr: String);
begin
  Log('Error: ' + MessageStr);
end;

procedure TDbcProxyWritelnLogger.Warning(MessageStr: String);
begin
  Log('Warning: ' + MessageStr);
end;

procedure TDbcProxyWritelnLogger.Info(MessageStr: String);
begin
  Log('Info: ' + MessageStr);
end;

procedure TDbcProxyWritelnLogger.Debug(MessageStr: String);
begin
  Log('Debug: ' + MessageStr);
end;

{------------------------------------------------------------------------------}

constructor TDbcProxyFileLogger.Create(LogFileName: String);
begin
  FFileOpened := False;
  AssignFile(FLogFile, LogFileName);
  if FileExists(LogFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
  FFileOpened := True;
  FFileLock := TCriticalSection.Create;
end;

destructor TDbcProxyFileLogger.Destroy;
begin
  if Assigned(FFileLock) then
    FreeAndNil(FFileLock);
  if FFileOpened then
    CloseFile(FLogFile);
end;

procedure TDbcProxyFileLogger.Log(MessageStr: String);
begin
  FFileLock.Enter;
  try
    MessageStr := FormatDateTime('YYYY-MM-DD HH:NN:SS', Now) + ' ' +  MessageStr;
    WriteLn(FLogFile, MessageStr);
    Flush(FLogFile);
  finally
    FFileLock.Leave;
  end;
end;

{------------------------------------------------------------------------------}

constructor TDbcProxyConsoleLogger.Create;
{$IFDEF WINDOWS}
var
  StdOutHandle: HANDLE;
{$ENDIF}
begin
  FLock := TCriticalSection.Create;
  {$IFDEF WINDOWS}
  StdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  FConsoleAvailable := StdOutHandle <> INVALID_HANDLE_VALUE;
  {$ELSE}
  FConsoleAvailable := True;
  {$ENDIF}
end;

destructor TDbcProxyConsoleLogger.Destroy;
begin
  if Assigned(FLock) then
    FreeAndNil(FLock);
end;

procedure TDbcProxyConsoleLogger.Log(MessageStr: String);
begin
  if FConsoleAvailable then begin;
    FLock.Enter;
    try
      MessageStr := DateTimeToStr(Now, True) + ' ' +  MessageStr;
      WriteLn(MessageStr);
      Flush(StdOut);
    finally
      FLock.Leave;
    end;
  end;
end;

end.

