unit ZDbcInterbaseBackup;

{$I zdbc.inc}

interface

uses
  Classes, SysUtils, ZDbcIntfs, ZCompatibility;

type
  TZFirebirdBackup = class(TInterfacedObject, IZBackup)
    protected
      FHostName: ZWideString;
      FDatabase: ZWideString;
      FPort: Word;
      FOnVerbose: TZVerboseCallback;
      FLibLocation: ZWideString;
      FUserName: ZWideString;
      FPassword: ZWideString;
      FBackupFileName: ZWideString;
    public
      procedure SetHostName(HostName: ZWideString);
      function GetHostName: ZWideString;
      procedure SetDatabase(Database: ZWideString);
      function GetDatabase: ZWideString;
      procedure SetPort(Port: Word);
      function GetPort: Word;
      procedure SetOnVerbose(Callback: TZVerboseCallback);
      function GetOnVerbose: TZVerboseCallback;
      procedure SetLibLocation(LibLocation: ZWideString);
      function GetLibLocation: ZWideString;
      procedure SetUserName(UserName: ZWideString);
      function GetUserName: ZWideString;
      procedure SetPassword(Password: ZWideString);
      function GetPassword: ZWideString;
      procedure SetBackupFileName(FileName: ZWideString);
      function GetBackupFileName: ZWideString;
      procedure Backup;
  end;

implementation

uses ZPlainFirebirdInterbaseDriver, ZExceptions, ZDbcInterbase6Utils, ZPlainDriver, ZEncoding;

const
  ServiceManagerParams: array [0..8] of TZIbParam =
    (
      (Name: 'isc_spb_current_version';       ValueType: pvtNone;    Number: isc_spb_current_version),
      (Name: 'isc_spb_user_name';             ValueType: pvtString;  Number: isc_spb_user_name),
      (Name: 'isc_spb_password';              ValueType: pvtString;  Number: isc_spb_password),
      (Name: 'isc_spb_utf8_filename';         ValueType: pvtString;    Number: isc_spb_utf8_filename),

      (Name: 'isc_spb_dbname';                ValueType: pvtLongString;  Number: isc_spb_dbname),
      (Name: 'isc_spb_expected_db';           ValueType: pvtString;      Number: isc_spb_expected_db),
      (Name: 'isc_spb_bkp_file';              ValueType: pvtLongString;  Number: isc_spb_bkp_file),
      (Name: 'isc_spb_verbose';               ValueType: pvtNone;        Number: isc_spb_verbose),
      (Name: 'isc_spb_nullbyte';              ValueType: pvtNone;        Number: 0)
    );

procedure GetFirebirdVersion(const ProductStr: UTF8String; out IsFirebird: Boolean; out FbVersion: Integer);
var
  FBPos: Integer;
  MajorStr: AnsiString;
  MinorStr: AnsiString;
begin
  FBPos := Pos('Firebird', ProductStr);
  if FBPos = 0 then begin
    FbVersion := 0;
    IsFirebird := False;
  end else begin
    IsFirebird := True;
    MajorStr := Copy(ProductStr, FBPos + 9, Length(ProductStr));
    FbPos := Pos('.', MajorStr);
    if FBPos > 0 then begin
      MinorStr := Copy(MajorStr, FBPos + 1, Length(MajorStr));
      Delete(MajorStr, FBPos, Length(MajorStr));
    end;
    FbVersion := StrToIntDef(MajorStr, 0) * 1000000 + StrToIntDef(MinorStr, 0) * 1000;
  end;
end;

procedure HandleIbError(PlainDriver: TZInterbasePlainDriver; const StatusVector: TARRAY_ISC_STATUS);
var
  Error: String;
  PTempStatus: PISC_STATUS;
begin
  Error := '';
  SetLength(Error, 1024);
  PTempStatus := @StatusVector[0];
  if not assigned(PlainDriver.fb_interpret) then
    raise EZSQLException.Create('fb_interpret is not assigned.')
  else begin
    PlainDriver.fb_interpret(@Error[1], Length(Error), @PTempStatus);
    SetLength(Error, StrLen(PAnsiChar(Error)));
    raise EZSQLException.Create(Error);
  end;
end;

procedure TZFirebirdBackup.SetHostName(HostName: ZWideString);
begin
  FHostName := HostName;
end;

function TZFirebirdBackup.GetHostName: ZWideString;
begin
  Result := FHostName;
end;

procedure TZFirebirdBackup.SetDatabase(Database: ZWideString);
begin
  FDatabase := Database;
end;

function TZFirebirdBackup.GetDatabase: ZWideString;
begin
  Result := FDatabase;
end;

procedure TZFirebirdBackup.SetPort(Port: Word);
begin
  FPort := Port;
end;

function TZFirebirdBackup.GetPort: Word;
begin
  Result := FPort;
end;

procedure TZFirebirdBackup.SetOnVerbose(Callback: TZVerboseCallback);
begin
  FOnVerbose := Callback;
end;

function TZFirebirdBackup.GetOnVerbose: TZVerboseCallback;
begin
  Result := FOnVerbose;
end;

procedure TZFirebirdBackup.SetLibLocation(LibLocation: ZWideString);
begin
  FLibLocation := LibLocation;
end;

function TZFirebirdBackup.GetLibLocation: ZWideString;
begin
  Result := FLibLocation;
end;

procedure TZFirebirdBackup.SetUserName(UserName: ZWideString);
begin
  FUserName := UserName;
end;

function TZFirebirdBackup.GetUserName: ZWideString;
begin
  Result := FUserName;
end;

procedure TZFirebirdBackup.SetPassword(Password: ZWideString);
begin
  FPassword := Password;
end;

function TZFirebirdBackup.GetPassword: ZWideString;
begin
  Result := FPassword;
end;

procedure TZFirebirdBackup.SetBackupFileName(FileName: ZWideString);
begin
  FBackupFileName := FileName;
end;

function TZFirebirdBackup.GetBackupFileName: ZWideString;
begin
  Result := FBackupFileName;
end;

procedure TZFirebirdBackup.Backup;
var
  ZDbcDriver: IZDriver;
  ZPlainDriver: IZPlainDriver;
  ZURL: TZURL;
  IBPlainDriver: TZInterbasePlainDriver;
  StatusVector: TARRAY_ISC_STATUS;
  ServiceHandle: TISC_SVC_HANDLE;
  Status: ISC_STATUS;
  URL: String;
  LineBuffer: RawByteString;
  IsFirebird: Boolean;
  FirebirdVersion: Integer;
  ServiceName: UTF8String;
  Info: TStringList;
  SPB: RawByteString;
  APB: RawByteString;
  TPB: RawByteString;
  LineLen: Word;
begin
  URL := 'zdbc:interbase://' + UTF8Encode(FHostName) + '/ServiceMgr';
  if FLibLocation <> '' then
    URL := URL + '?LibLocation=' + UTF8Encode(FLibLocation);

  ZURL := TZURL.Create(url);

  ZDbcDriver := ZDbcIntfs.DriverManager.GetDriver(url);
  if not Assigned(ZDbcDriver) then
    raise EZSQLException.Create('No driver found.');

  ZPlainDriver := ZDbcDriver.GetPlainDriver(ZURL);
  if not Assigned(ZPlainDriver) then
    raise EZSQLException.Create('Could not load the plain driver.');

  IBPlainDriver := ZPlainDriver.GetInstance as TZInterbasePlainDriver;
  if not Assigned(IBPlainDriver) then
    raise EZSQLException.Create('Could not get the plain driver instance');

  // check the library version
  LineBuffer := '';
  SetLength(LineBuffer, 50);
  IBPlainDriver.isc_get_client_version(@Linebuffer[1]);
  SetLength(LineBuffer, StrLen(PAnsiChar(LineBuffer)));
  GetFirebirdVersion(LineBuffer, IsFirebird, FirebirdVersion);

  if not IsFirebird or (FirebirdVersion < 3000000) then
    raise EZSQLException.Create('Only Firebird client library version 3.0 or higher is supported.');

  ServiceHandle := nil;
  try
    // connect to service manager
    Info := TStringList.Create;
    Info.Add('isc_spb_user_name=' + UTF8Encode(FUserName));
    Info.Add('isc_spb_password=' + UTF8Encode(FPassword));
    Info.Add('isc_spb_utf8_filename');
    Info.Add('isc_spb_expected_db=' + UTF8Encode(FDatabase));
    SPB := BuildPB(IBPlainDriver, Info, isc_spb_version1, 'isc_spb_', ServiceManagerParams{$IFDEF UNICODE}, zCP_UTF8{$ENDIF});

    if FHostName = '' then
      ServiceName := 'service_mgr'
    else if FHostName = '.' then
      ServiceName := '\\.\service_mgr'
    else begin
      ServiceName := UTF8Encode(FHostName);
      if FPort <> 0 then
        ServiceName := ServiceName + '/' + IntToStr(FPort);
      ServiceName := ServiceName + ':service_mgr';
    end;

    Status := IBPlainDriver.isc_service_attach(@StatusVector, 0, @ServiceName[1], @ServiceHandle, Length(SPB), @SPB[1]);

    if Status <> 0 then
      HandleIbError(IBPlainDriver, StatusVector);

    // create the service task
    Info.Clear;
    Info.Add('isc_spb_dbname=' + UTF8Encode(FDatabase));
    Info.Add('isc_spb_bkp_file=' + UTF8Encode(FBackupFileName));
    Info.Add('isc_spb_verbose');
    TPB := BuildPB(IBPlainDriver, Info, isc_action_svc_backup, 'isc_spb_', ServiceManagerParams {$IFDEF UNICODE}, zCP_UTF8{$ENDIF});

    FillChar(StatusVector, SizeOf(StatusVector), 0);
    Status := IBPlainDriver.isc_service_start(@StatusVector , @ServiceHandle, nil, Length(TPB), {PAnsiChar(TPB)} PISC_SCHAR(@TPB[1]));

    if Status <> 0 then
      HandleIbError(IBPlainDriver, StatusVector);

    // query the service for information in a loop
    // the loop ends when the server has no more data
    Info.Clear;
    APB := BuildPB(IBPlainDriver, Info, isc_info_svc_line, 'isc_spb_', ServiceManagerParams {$IFDEF UNICODE}, zCP_UTF8{$ENDIF});

    while true do begin
      SetLength(LineBuffer, 1024);
      FillChar(LineBuffer[1], Length(LineBuffer), #0);
      Status := IBPlainDriver.isc_service_query(@StatusVector, @ServiceHandle, nil, 0, PISC_SCHAR(PEmptyAnsiString), Length(APB), PISC_SCHAR(@APB[1]), Length(LineBuffer), @LineBuffer[1]);
      if Status <> 0 then
        HandleIbError(IBPlainDriver, StatusVector);
      if (Byte(LineBuffer[1]) <> isc_info_svc_line) then
        raise EZSQLException.Create('unexpected API result');
      LineLen := PWord(@LineBuffer[2])^;
      if LineLen <> 0 then begin
        if Assigned(FOnVerbose) then
          FOnVerbose(UTF8Decode(Copy(LineBuffer, 4, LineLen)));
      end else
        break;
    end;

    Status := IBPlainDriver.isc_service_detach(@StatusVector, @ServiceHandle);
    if Status <> 0 then
      HandleIbError(IBPlainDriver, StatusVector);
  finally
    if Assigned(ZURL) then
      FreeAndNil(ZURL);
    if Assigned(Info) then
      FreeAndNil(Info);
  end;
end;


end.

