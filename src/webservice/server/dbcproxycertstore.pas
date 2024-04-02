{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                WebService Proxy Server                  }
{                                                         }
{         Originally written by Jan Baumgarten            }
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

unit dbcproxycertstore;

{$I dbcproxy.inc}

interface

{$IFDEF ENABLE_TOFU_CERTIFICATES}

uses
  Classes, SysUtils, generics.collections, SyncObjs, Types, IniFiles;

type
  TCertificateInfo = record
    CertificateFile: String;
    KeyFile: String;
    PublicKey: String;
    ValidFrom: TDate;
    ValidTo: TDate;
    SectionName: String;
  end;

  TCertificateList = TList<TCertificateInfo>;

  TDbcProxyCertificateStore = class
    protected
      FCriticalSection: TCriticalSection;
      FCertificatesPath: String;
      FCertificates: TCertificateList;
      FCertificatesInfoFile: String;
      FHostName: String;
      FIniFileOptions: TIniFileOptions;
      procedure LoadCertificates;
      procedure ValidateCertificateInfo;
      procedure DeleteCertificate(Index: Integer);
      procedure AddNewCertificate;
      procedure MaintainStore;
    public
      property CertificatesPath: String read FCertificatesPath;
      property HostName: String read FHostName;
      constructor Create;
      destructor Destroy; override;
      procedure DoMaintenance;
      function GetValidPublicKeys: TStringDynArray;
      procedure GetCurrentCertificate(out CertificateFile, KeyFile: String);
  end;

var
  TofuCertStore: TDbcProxyCertificateStore;

{$ENDIF}

implementation

{$IFDEF ENABLE_TOFU_CERTIFICATES}

uses {$IFDEF WINDOWS}Windows, ShlObj, {$ELSE}unix, {$IFEND}zeosproxy_imp, openssl,
  base64, dateutils, math;

const
  ValidDays = 365;


{$IFDEF WINDOWS}
const
  CSIDL_LOCAL_APPDATA = $001C;

//von http://www.delphipraxis.net/153680-csidl-windows-7-a.html
//function getWinSpecialFolder(CSIDLFolder : integer) : string;
function GetSpecialFolder(hWindow: HWND; Folder: Integer): String;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(hWindow, PChar(Result), Folder, false);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then Result := IncludeTrailingPathDelimiter(Result);
end;
{$ENDIF}

// Some stuff to extend the existing functionality of the openssl unit
type
  PASN1_PCTX = SslPtr;
  TEvpPkeyPrintPublic = function(outvar: PBIO; pkey: PEVP_PKEY; indent: Integer;
    pctx: PASN1_PCTX): Integer; cdecl;

var
  _EvpPkeyPrintPublic: TEvpPkeyPrintPublic = nil;

function InitSSLInterface: Boolean;
begin
  Result := openssl.InitSSLInterface;
  if Result then begin
    if not Assigned(_EvpPkeyPrintPublic) then begin
      _EvpPkeyPrintPublic := GetProcAddress(SSLUtilHandle, 'EVP_PKEY_print_public');
      Result := Assigned(_EvpPkeyPrintPublic);
    end;
  end;
end;

function EvpPkeyPrintPublic(outvar: PBIO; pkey: PEVP_PKEY; indent: Integer;
  pctx: PASN1_PCTX): Integer;
begin
  if InitSSLInterface and Assigned(_EvpPkeyPrintPublic) then
    Result := _EvpPkeyPrintPublic(outvar, pkey, indent, pctx)
  else
    Result := 0;
end;

// The function to create self signed certificates
function CreateSelfSignedCert(const HostName, Country: String; const ValidDays: Integer; out Certificate, PrivateKey: RawBytestring; out PublicKey: AnsiString): Boolean;
var
  pk: PEVP_PKEY;
  x: PX509;
  rsa: PRSA;
  t: PASN1_UTCTIME;
  name: PX509_NAME;
  b: PBIO;
  xn, y: integer;
  s: AnsiString;
begin
  Result := True;
  pk := EvpPkeynew;
  x := X509New;
  try
    rsa := RsaGenerateKey(3072, $10001, nil, nil);
    EvpPkeyAssign(pk, EVP_PKEY_RSA, rsa);
    X509SetVersion(x, 2);
//    Asn1IntegerSet(X509getSerialNumber(x), 0);
    Asn1IntegerSet(X509getSerialNumber(x), GetTickCount);
    t := Asn1UtctimeNew;
    try
      X509GmtimeAdj(t, -60 * 60 *24);
      X509SetNotBefore(x, t);
      X509GmtimeAdj(t, ValidDays * 60 * 60 *24);
      X509SetNotAfter(x, t);
    finally
      Asn1UtctimeFree(t);
    end;
    X509SetPubkey(x, pk);
    Name := X509GetSubjectName(x);
    if Country <> '' then
      X509NameAddEntryByTxt(Name, 'C', $1001, Country, -1, -1, 0);
    X509NameAddEntryByTxt(Name, 'CN', $1001, HostName, -1, -1, 0);
    //X509AddExtension(x, NID_ext_key_usage, 'serverAuth,clientAuth');
    //X509AddExtension(x, NID_subject_alt_name, 'DNS:' + HostName);
    //X509AddExtension(x, NID_basic_constraints, 'CA:true');

    x509SetIssuerName(x, Name);
    x509Sign(x, pk, EvpGetDigestByName('SHA256'));
    b := BioNew(BioSMem);
    try
      i2dX509Bio(b, x);
      xn := bioctrlpending(b);
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
    finally
      BioFreeAll(b);
    end;
    Certificate := s;
    b := BioNew(BioSMem);
    try
      i2dPrivatekeyBio(b, pk);
      xn := bioctrlpending(b);
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
    finally
      BioFreeAll(b);
    end;
    PrivateKey := s;

    b := BioNew(BioSMem);
    try
      EvpPkeyPrintPublic(b, pk, 0, nil);
      xn := BioCtrlPending(b);
      SetLength(s, xn);
      y := BioRead(b, s, xn);
      if y > 0 then
        SetLength(s, y);
    finally
      BioFreeAll(b);
    end;
    PublicKey := s;
    // clean up the private key, because it is prepared to be printed in a pretty way for humans and not for machines:
    y := pos('Modulus:', PublicKey);
    Delete(PublicKey, 1, y + 7);
    y := pos('Exponent:', PublicKey);
    Delete(PublicKey, y, length(PublicKey));
    for y := length(PublicKey) downto 1 do begin
      case PublicKey[y] of
        '0'..'9', 'a'..'f': ;// do nothing
        else Delete(PublicKey, y, 1);
      end;
    end;
    if copy(PublicKey, 1, 2) = '00' then
      Delete(PublicKey, 1, 2);
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

function GetCertificatesPath: String;
begin
  {$IFDEF WINDOWS}
    Result := GetSpecialFolder(0, CSIDL_LOCAL_APPDATA) + 'ZeosProxy\Certificates';
  {$ELSE}
    {$IFDEF ENABLE_DEBUG_SETTINGS}
    Result := ExtractFilePath(ParamStr(0)) + '.zeosproxy/certificates';
    {$ELSE}
    Result := '/var/lib/zeosproxy/certificates';
    {$IFEND}
  {$IFEND}
end;

// The Windows portion is taken from here:
// https://www.delphipraxis.net/107832-post3.html
// The Unix portion is taken from here:
// https://forum.lazarus.freepascal.org/index.php/topic,30885.msg196955.html#msg196955
function GetComputerName: String;
{$IFDEF WINDOWS}
var
  Size: DWORD;
{$IFEND}
begin
  {$IFDEF WINDOWS}
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, Size);
  if Windows.GetComputerName(PChar(Result), Size) then
    SetLength(Result, Size)
  else
    Result := '';
  {$ELSE}
  Result := GetHostName;
  {$IFEND}
end;

procedure ExportCertData(Data: RawByteString; Header, Footer: String; FileName: String);
var
  Base64Data: String;
  Lines: TStringList;
begin
  Base64Data := EncodeStringBase64(Data);

  Lines := TStringList.Create;
  try
    Lines.Add(Header);
    while Base64Data <> '' do begin
      Lines.Add(Copy(Base64Data, 1, 80));
      Delete(Base64Data, 1, 80);
    end;
    Lines.Add(Footer);
    Lines.SaveToFile(FileName);
  finally
    FreeAndNil(Lines);
  end;
end;

constructor TDbcProxyCertificateStore.Create;
begin
  inherited;
  FIniFileOptions := [ifoWriteStringBoolean];
  FCertificatesPath := GetCertificatesPath;
  FCertificates := TCertificateList.Create;
  FCertificatesInfoFile := FCertificatesPath + DirectorySeparator + 'certinfo.ini';
  FCriticalSection := SyncObjs.TCriticalSection.Create();
  FHostName := LowerCase(GetComputerName) + '.local';

  LoadCertificates;
  ValidateCertificateInfo;
  MaintainStore;
end;

destructor TDbcProxyCertificateStore.Destroy;
begin
  if Assigned(FCriticalSection)
    then FreeAndNil(FCriticalSection);
  if Assigned(FCertificates) then
    FreeAndNil(FCertificates);
  inherited;
end;

procedure TDbcProxyCertificateStore.LoadCertificates;
var
  CertInfo: TIniFile;
  Sections: TStringList;
  x: Integer;
  Certificate: TCertificateInfo;
  SectionName: String;
  TempDate: String;
begin
  if not DirectoryExists(FCertificatesPath)
    then ForceDirectories(FCertificatesPath);

  try
    CertInfo := TIniFile.Create(FCertificatesInfoFile, FIniFileOptions);
    Sections := TStringList.Create;
    CertInfo.ReadSections(Sections);
    for
      x := 0 to Sections.Count - 1 do begin
        SectionName := Sections.Strings[x];
        Certificate.SectionName := SectionName;
        Certificate.CertificateFile := CertInfo.ReadString(SectionName, 'CertificateFile', '');
        Certificate.KeyFile := CertInfo.ReadString(SectionName, 'KeyFile', '');
        Certificate.PublicKey := CertInfo.ReadString(SectionName, 'PublicKey', '');
        TempDate := CertInfo.ReadString(SectionName, 'ValidFrom', '');
        Certificate.ValidFrom := StrToDate(TempDate);
        TempDate := CertInfo.ReadString(SectionName, 'ValidTo', '');
        Certificate.ValidTo := StrToDate(TempDate);
        FCertificates.Add(Certificate);
      end;
  finally
    if Assigned(CertInfo) then
      FreeAndNil(CertInfo);
    if Assigned(Sections) then
      FreeAndNil(Sections);
  end;
end;

procedure TDbcProxyCertificateStore.ValidateCertificateInfo;
var
  Certificate: TCertificateInfo;
  x: Integer;
  IsValid: Boolean;
begin
  for x := FCertificates.Count - 1 downto 0 do begin
    Certificate := FCertificates[x];
    IsValid := Certificate.ValidTo > Date;
    IsValid := IsValid and (Certificate.PublicKey <> '');
    IsValid := IsValid and (Certificate.KeyFile <> '');
    IsValid := IsValid and FileExists(FCertificatesPath + DirectorySeparator + Certificate.KeyFile);
    IsValid := IsValid and (Certificate.CertificateFile <> '');
    IsValid := IsValid and FileExists(FCertificatesPath + DirectorySeparator + Certificate.CertificateFile);
    if not IsValid then
      DeleteCertificate(x);
  end;
end;

procedure TDbcProxyCertificateStore.DeleteCertificate(Index: Integer);
var
  Certificate: TCertificateInfo;
  CertInfo: TIniFile;
  CertFile, KeyFile: String;
begin
  Certificate := FCertificates.Items[Index];
  FCertificates.Delete(Index);
  CertFile := FCertificatesPath + PathDelim + Certificate.CertificateFile;
  KeyFile := FCertificatesPath + PathDelim + Certificate.KeyFile;
  If (Certificate.KeyFile <> '') and FileExists(KeyFile) then
    SysUtils.DeleteFile(KeyFile);
  If (Certificate.CertificateFile <> '') and FileExists(CertFile) then
    SysUtils.DeleteFile(CertFile);
  CertInfo := TIniFile.Create(FCertificatesInfoFile, FIniFileOptions);
  try
    CertInfo.EraseSection(Certificate.SectionName);
  finally
    FreeAndNil(CertInfo);
  end;
end;

procedure TDbcProxyCertificateStore.AddNewCertificate;
var
  Certificate, PrivateKey: RawByteString;
  PublicKey: String;
  UUID: TGuid;
  UUIDStr: String;
  CertFile, KeyFile: String;
  ValidFrom, ValidTo: TDate;
  CertInfo: TIniFile;
  CertRecord: TCertificateInfo;
begin
  if CreateSelfSignedCert(GetComputerName + '.local', '', ValidDays, Certificate, PrivateKey, PublicKey) then begin
    CreateGUID(UUID);
    UUIDStr := GUIDToString(UUID);
    CertFile := UUIDStr + '.crt';
    KeyFile := UUIDStr + '.key';
    ExportCertData(Certificate, '-----BEGIN CERTIFICATE-----', '-----END CERTIFICATE-----', FCertificatesPath + DirectorySeparator + CertFile);
    ExportCertData(PrivateKey, '-----BEGIN RSA PRIVATE KEY-----', '-----END RSA PRIVATE KEY-----', FCertificatesPath + DirectorySeparator + KeyFile);
    ValidFrom := Date;
    ValidTo := ValidFrom;
    ValidTo := IncDay(ValidTo, ValidDays - 1);

    CertInfo := TIniFile.Create(FCertificatesInfoFile, FIniFileOptions);
    try
      CertInfo.WriteString(UUIDStr, 'CertificateFile', CertFile);
      CertInfo.WriteString(UUIDStr, 'KeyFile', KeyFile);
      CertInfo.WriteString(UUIDStr, 'PublicKey', PublicKey);
      CertInfo.WriteDate(UUIDStr, 'ValidFrom', ValidFrom);
      CertInfo.WriteDate(UUIDStr, 'ValidTo', ValidTo);
    finally
      FreeAndNil(CertInfo);
    end;

    CertRecord.CertificateFile := CertFile;
    CertRecord.KeyFile := KeyFile;
    CertRecord.PublicKey := PublicKey;
    CertRecord.ValidFrom := ValidFrom;
    CertRecord.ValidTo := ValidTo;
    FCertificates.Add(CertRecord);
  end;
end;

procedure TDbcProxyCertificateStore.MaintainStore;
var
  MaxValidity: TDate;
  x: Integer;
begin
  MaxValidity := 0;
  for x := FCertificates.Count - 1 downto 0 do begin
    if FCertificates[x].ValidTo < Date then
      DeleteCertificate(x)
    else
      MaxValidity := Max(MaxValidity, FCertificates[x].ValidTo);
  end;

  if FCertificates.Count = 0 then
    AddNewCertificate
  else if DaysBetween(Date, MaxValidity) < (ValidDays div 2) then
    AddNewCertificate;
end;

procedure TDbcProxyCertificateStore.DoMaintenance;
begin
  FCriticalSection.Enter;
  try
    MaintainStore;
  finally
    FCriticalSection.Leave;
  end;
end;

function TDbcProxyCertificateStore.GetValidPublicKeys: TStringDynArray;
var
  x: Integer;
  PublicKey: String;
begin
  FCriticalSection.Enter;
  try
    SetLength(Result, FCertificates.Count);
    for x := 0 to FCertificates.Count - 1 do begin
      PublicKey := FCertificates[x].PublicKey;
      Result[x] := Copy(PublicKey, 1, Length(PublicKey));
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TDbcProxyCertificateStore.GetCurrentCertificate(out CertificateFile, KeyFile: String);
var
  x: Integer;
  ResultIdx: Integer;
  MinValid: TDate;
begin
  FCriticalSection.Enter;
  try
    // Find the certificate with the lowest ValidFrom date
    ResultIdx := 0;
    MinValid := FCertificates.Items[0].ValidFrom;
    for x := 1 to FCertificates.Count - 1 do begin
      if FCertificates[x].ValidFrom < MinValid then begin
        ResultIdx := x;
        MinValid := FCertificates[x].ValidFrom;
      end;
    end;

    //Return certificate data
    CertificateFile := FCertificatesPath + DirectorySeparator + FCertificates[ResultIdx].CertificateFile;
    KeyFile := FCertificatesPath + DirectorySeparator + FCertificates[ResultIdx].KeyFile;
  finally
    FCriticalSection.Leave;
  end;
end;

{$ENDIF}

end.

