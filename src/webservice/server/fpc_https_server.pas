{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Note: This is a modified version of the fpc_http_server unit. It enables the
        use of SSL with FPC versions > 3.2.0
}

unit fpc_https_server;

interface

{$IF Defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 30200) }
  {$DEFINE FPC_IS_SSLENABLED}
  {$IF FPC_FULLVERSION >= 30202}
    {$DEFINE FPC_CAN_CREATE_CERTIFICATE}
  {$IFEND}
{$IFEND}
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, httpdefs, fphttpserver, server_listener, wst_types;

type

  IObjectRef = interface
    ['{B62EC733-999D-4DEC-A69F-B7546A16F661}']
    function GetObject() : TObject;
  end;

  { TFPWorkerObject }

  TFPWorkerObject = class(TInterfacedObject,IObjectRef)
  private
    FHTTPServerObject: TFPHTTPServer;
    FRootAddress : string;
    FServerSoftware : String;
    FOnNotifyMessage : TListnerNotifyMessage;

    {$IFDEF FPC_IS_SSLENABLED}
    FUseSSL: Boolean;
    FKeyFile: String;
    FKeyPasswod: String;
    FCertificateFileName: String;
    FHostName: String;
    {$ENDIF}
  private
    function GetHandleRequestInThread : Boolean;
    function GetListeningPort : Integer;
    procedure SetHandleRequestInThread(const AValue : Boolean);
    procedure SetListeningPort(const AValue : Integer);

    {$IFDEF FPC_IS_SSLENABLED}
    procedure SetUseSSL(const AValue: Boolean);
    procedure SetKeyFile(const AValue: String);
    procedure SetKeyPasswod(const AValue: String);
    procedure SetCertificateFileName(const AValue: String);
    procedure SetHostName(const AValue: String);
    {$ENDIF}

    procedure ProcessWSDLRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
    procedure ProcessServiceRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
  private
    procedure RequestHandler(
          Sender    : TObject;
      Var ARequest  : TFPHTTPConnectionRequest;
      Var AResponse : TFPHTTPConnectionResponse
    );
  protected
    function GetObject() : TObject;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Start();
    procedure Stop();
    function IsActive : Boolean;

    property RootAddress : string read FRootAddress write FRootAddress;
    property ServerSoftware : string read FServerSoftware write FServerSoftware;
    property ListeningPort : Integer read GetListeningPort write SetListeningPort;
    property OnNotifyMessage : TListnerNotifyMessage read FOnNotifyMessage write FOnNotifyMessage;
    property HandleRequestInThread : Boolean read GetHandleRequestInThread write SetHandleRequestInThread;

    {$IFDEF FPC_IS_SSLENABLED}
    property UseSSL: Boolean read FUseSSL write SetUseSSL;
    property KeyFile: String read FKeyFile write SetKeyFile;
    property KeyPasswod: String read FKeyPasswod write SetKeyPasswod;
    property CertificateFileName: String read FCertificateFileName write SetCertificateFileName;
    property HostName: String read FHostName write SetHostName;
    {$ENDIF}
  end;

  { TServerListnerThread }

  TServerListnerThread = class(TThread)
  private
    FWorkerObject : IObjectRef;
    FOnNotifyMessage : TListnerNotifyMessage;
  public
    constructor Create(AWorkerObject : TFPWorkerObject; OnNotifyMessage: TListnerNotifyMessage = nil);
    procedure Execute(); override;
  end;

  TListenerOption = (loExecuteInThread, loHandleRequestInThread);
  TListenerOptions = set of TListenerOption;

  { TwstFPHttpListener }

  TwstFPHttpsListener = class(TwstListener)
  private
    FOptions : TListenerOptions;
    FWorkerObjectRef : IObjectRef;
    FWorkerObject : TFPWorkerObject;

    {$IFDEF FPC_IS_SSLENABLED}
    procedure SetUseSSL(const AValue: Boolean);
    procedure SetKeyFile(const AValue: String);
    procedure SetKeyPasswod(const AValue: String);
    procedure SetCertificateFileName(const AValue: String);
    procedure SetHostName(const AValue: String);
    function GetUseSSL: Boolean;
    function GetKeyFile: String;
    function GetKeyPasswod: String;
    function GetCertificateFileName: String;
    function GetHostName: String;
    {$ENDIF}
  protected
    procedure SetOnNotifyMessage(const AValue : TListnerNotifyMessage);override;
  public
    constructor Create(
      const AServerIpAddress   : string  = '127.0.0.1';
      const AListningPort      : Integer = 8000;
      const ADefaultClientPort : Integer = 25000;
      const AServerSoftware    : string  = 'Web Service Toolkit Application'
    );
    destructor Destroy(); override;
    class function GetDescription() : string;override;
    procedure Start();override;
    procedure Stop();override;
    function IsActive : Boolean; override;

    property Options : TListenerOptions read FOptions write FOptions;
    {$IFDEF FPC_IS_SSLENABLED}
    property UseSSL: Boolean read GetUseSSL write SetUseSSL;
    property KeyFile: String read GetKeyFile write SetKeyFile;
    property KeyPasswod: String read GetKeyPasswod write SetKeyPasswod;
    property CertificateFileName: String read GetCertificateFileName write SetCertificateFileName;
    property HostName: String read GetHostName write SetHostName;
    {$ENDIF}
  end;

implementation
uses
  wst_consts,
  base_service_intf, server_service_intf, server_service_imputils, metadata_wsdl;

{$IFDEF WST_DBG}
procedure Display(const AMsg : string);
begin
  if IsConsole then
    WriteLn(AMsg);
end;
{$ENDIF}

function ExtractNextPathElement(var AFullPath : string):string;
var
  i : SizeInt;
begin
  Result := '';
  if ( Length(AFullPath) > 0 ) then begin
    while ( Length(AFullPath) > 0 ) and ( AFullPath[1] = sSEPARATOR ) do begin
      Delete(AFullPath,1,1);
    end;
    i := Pos(sSEPARATOR,AFullPath);
    if ( i < 1 ) then begin
      Result := AFullPath;
      AFullPath := '';
    end else begin
      Result := Copy(AFullPath,1,Pred(i));
      Delete(AFullPath,1,i);
    end;
  end;
end;

{ TServerListnerThread }

constructor TServerListnerThread.Create(AWorkerObject : TFPWorkerObject; OnNotifyMessage: TListnerNotifyMessage = nil);
begin
  FreeOnTerminate := True;
  FWorkerObject := AWorkerObject;
  FOnNotifyMessage := OnNotifyMessage;
  inherited Create(False);
end;

procedure TServerListnerThread.Execute();
var
  locObject : TFPWorkerObject;
begin
  try
    locObject := TFPWorkerObject(FWorkerObject.GetObject());
    locObject.Start();
  except
    on e : Exception do begin
      if Assigned(FOnNotifyMessage) then
        FOnNotifyMessage(Self,'Start()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

{ TFPWorkerObject }

procedure TFPWorkerObject.ProcessWSDLRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GenerateWSDL(locRepName,FRootAddress);
  i:=Length(strBuff);
  if (StrBuff<>'') then
    begin
    AResponse.ContentType := 'text/xml';
    AResponse.Content:=strBuff;
    end
  else
    begin
    AResponse.ContentType := 'text/html';
    AResponse.Content := GenerateWSDLHtmlTable();
    end;
  if AResponse.ContentLength=0 then
    AResponse.ContentLength:=Length(AResponse.Content);
end;

procedure TFPWorkerObject.ProcessServiceRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  inStream : TStringStream;
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then
    begin
    ProcessWSDLRequest(ARequest,AResponse,APath);
    Exit;
    end;
  inStream := nil;
  try
    inStream := TStringStream.Create(ARequest.Content);
    try
      AResponse.ContentStream := TMemoryStream.Create();
      ctntyp := ARequest.ContentType;
      AResponse.ContentType := ctntyp;
      frmt := Trim(ARequest.QueryFields.Values['format']);
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponse.ContentStream,frmt);
      rqst.GetPropertyManager().SetProperty(sREMOTE_IP,ARequest.RemoteAddress);
      HandleServiceRequest(rqst);
      AResponse.ContentLength:=AResponse.ContentStream.Size;
    finally
      inStream.Free();
    end;
  except
    on e : Exception do begin
      if Assigned(FOnNotifyMessage) then
        FOnNotifyMessage(Self,'ProcessData()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

function TFPWorkerObject.GetHandleRequestInThread : Boolean;
begin
  Result := FHTTPServerObject.Threaded;
end;

function TFPWorkerObject.GetListeningPort : Integer;
begin
  Result := FHTTPServerObject.Port;
end;

procedure TFPWorkerObject.RequestHandler(
      Sender    : TObject;
  var ARequest  : TFPHTTPConnectionRequest;
  var AResponse : TFPHTTPConnectionResponse
);
var
{$IFDEF WST_DBG}
  s : string;
  j : SizeInt;
{$ENDIF}
  locPath, locPathPart : string;
begin
  AResponse.Server:=FServerSoftware;
  locPath := ARequest.URL;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then
    ProcessServiceRequest(ARequest,AResponse,locPath)
  else
    ProcessWSDLRequest(ARequest,AResponse,locPath);
  try
    AResponse.SendContent;
  finally
    if Assigned(AResponse.ContentStream) then begin
      AResponse.ContentStream.Free();
      AResponse.ContentStream := nil;
    end;
  end;
end;

function TFPWorkerObject.GetObject : TObject;
begin
  Result := Self;
end;

procedure TFPWorkerObject.SetHandleRequestInThread(const AValue : Boolean);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetHandleRequestInThread']);
  FHTTPServerObject.Threaded := AValue;
end;

procedure TFPWorkerObject.SetListeningPort(const AValue : Integer);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetListeningPort']);
  FHTTPServerObject.Port := AValue;
end;

{$IFDEF FPC_IS_SSLENABLED}
procedure TFPWorkerObject.SetUseSSL(const AValue: Boolean);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetUseSSL']);
  FHTTPServerObject.UseSSL := AValue;
end;

procedure TFPWorkerObject.SetKeyFile(const AValue: String);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetKeyFile']);
  FHTTPServerObject.CertificateData.PrivateKey.FileName := AValue;
end;

procedure TFPWorkerObject.SetKeyPasswod(const AValue: String);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetKeyPasswod']);
  FHTTPServerObject.CertificateData.KeyPassword := AValue;
end;

procedure TFPWorkerObject.SetCertificateFileName(const AValue: String);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetCertificateFileName']);
  FHTTPServerObject.CertificateData.Certificate.FileName := AValue;
end;

procedure TFPWorkerObject.SetHostName(const AValue: String);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetHostName']);
  FHTTPServerObject.CertificateData.HostName := AValue;
end;
{$ENDIF}

constructor TFPWorkerObject.Create();
begin
  inherited Create();
  FHTTPServerObject := TFPHTTPServer.Create(nil);
  FHTTPServerObject.OnRequest := @RequestHandler;
end;

destructor TFPWorkerObject.Destroy();
begin
  if (FHTTPServerObject <> nil) then
    FHTTPServerObject.Active := False;
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TFPWorkerObject.Start();
begin
  if not FHTTPServerObject.Active then
    FHTTPServerObject.Active := True;
end;

procedure TFPWorkerObject.Stop();
begin
  if FHTTPServerObject.Active then
    FHTTPServerObject.Active := False;
end;

function TFPWorkerObject.IsActive : Boolean;
begin
  Result := FHTTPServerObject.Active;
end;

{ TwstFPHttpListener }

{$IFDEF FPC_IS_SSLENABLED}
procedure TwstFPHttpsListener.SetUseSSL(const AValue: Boolean);
begin
  FWorkerObject.UseSSL := AValue;
end;

procedure TwstFPHttpsListener.SetKeyFile(const AValue: String);
begin
  FWorkerObject.KeyFile := AValue;
end;

procedure TwstFPHttpsListener.SetKeyPasswod(const AValue: String);
begin
  FWorkerObject.KeyPasswod := AValue;
end;

procedure TwstFPHttpsListener.SetCertificateFileName(const AValue: String);
begin
  FWorkerObject.CertificateFileName := AValue;
end;

procedure TwstFPHttpsListener.SetHostName(const AValue: String);
begin
  FWorkerObject.HostName := AValue;
end;

function TwstFPHttpsListener.GetUseSSL: Boolean;
begin
  Result := FWorkerObject.UseSSL;
end;

function TwstFPHttpsListener.GetKeyFile: String;
begin
  Result := FWorkerObject.KeyFile;
end;

function TwstFPHttpsListener.GetKeyPasswod: String;
begin
  Result := FWorkerObject.KeyPasswod;
end;

function TwstFPHttpsListener.GetCertificateFileName: String;
begin
  Result := FWorkerObject.CertificateFileName;
end;

function TwstFPHttpsListener.GetHostName: String;
begin
  Result := FWorkerObject.HostName;
end;
{$ENDIF}

procedure TwstFPHttpsListener.SetOnNotifyMessage(const AValue : TListnerNotifyMessage);
begin
  inherited SetOnNotifyMessage(AValue);
  if (FWorkerObject <> nil) then
    FWorkerObject.OnNotifyMessage := AValue;
end;

constructor TwstFPHttpsListener.Create(
      const AServerIpAddress   : string;
      const AListningPort      : Integer;
      const ADefaultClientPort : Integer;
      const AServerSoftware    : string
);

begin
  inherited Create();
  FWorkerObjectRef := TFPWorkerObject.Create();
  FWorkerObject := TFPWorkerObject(FWorkerObjectRef.GetObject());
  FWorkerObject.RootAddress := AServerIpAddress;
  FWorkerObject.ServerSoftware := AServerSoftware;
  FWorkerObject.ListeningPort := AListningPort;
end;

destructor TwstFPHttpsListener.Destroy();
begin
  if (FWorkerObject <> nil) then
    Stop();
  FWorkerObjectRef := nil;
  inherited Destroy();
end;

procedure TwstFPHttpsListener.Start();
begin
  if not FWorkerObject.IsActive() then begin
    FWorkerObject.HandleRequestInThread := (loHandleRequestInThread in Options);
    if (loExecuteInThread in Options) then begin
      // The thread is create with "FreeOnTerminate := True"
      TServerListnerThread.Create(FWorkerObject, OnNotifyMessage);
    end else begin
      FWorkerObject.Start();
    end;
  end;
end;

procedure TwstFPHttpsListener.Stop();
begin
  if FWorkerObject.IsActive() then begin
    //In case of the thread(loExecuteInThread in Options),
    //this will make the thread exit and free itself as "FreeOnTerminate := True"
    FWorkerObject.Stop();
  end;
end;

class function TwstFPHttpsListener.GetDescription() : string;
begin
  Result := 'WST FP HTTPS Listener';
end;

function TwstFPHttpsListener.IsActive: Boolean;
begin
  Result := FWorkerObject.IsActive();
end;

initialization


end.
