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

uses
  Classes, SysUtils, generics.collections;

type
  TCertificateInfo = record
    CertificateFile: String;
    KeyFile: String;
    PublicKey: String;
    ValidFrom: TDate;
    ValidUntil: TDate;
    SectionName: String;
  end;

  TCertificateList = TList<TCertificateInfo>;

  TDbcProxyCertificateStore = class
    protected
      FCertificatesPath: String;
      FCertificates: TCertificateList;
      FCertificatesInfoFile: String;
      procedure LoadCertificates;
      procedure ValidateCertificateInfo;
      procedure DeleteCertificate(Index: Integer);
      procedure AddNewCertificate;
      //procedure MaintainCertificates;
    public
      property CertificatesPath: String read FCertificatesPath;
      constructor Create;
  end;

implementation

uses {$IFDEF WINDOWS}Windows, ShlObj, {$IFEND}zeosproxy_imp, inifiles, ssl_openssl_lib,
  synautil, base64;

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

function CreateSelfSignedCert(const HostName, Country: String; const ValidDays: Integer; out Certificate, Key: RawBytestring): Boolean;
var
  pk: EVP_PKEY;
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
    rsa := RsaGenerateKey(2048, $10001, nil, nil);
    EvpPkeyAssign(pk, EVP_PKEY_RSA, rsa);
    X509SetVersion(x, 2);
//    Asn1IntegerSet(X509getSerialNumber(x), 0);
    Asn1IntegerSet(X509getSerialNumber(x), GetTick);
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
    Key := s;
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

function GetCertificatesPath: String;
begin
  {$IFDEF WINDOWS}
    Result := GetSpecialFolder(0, CSIDL_LOCAL_APPDATA) + '\ZeosProxy\Certificates';
  {$ELSE}
    Result := '/var/lib/zeosproxy/certificates';
  {$IFEND}
end;

constructor TDbcProxyCertificateStore.Create;
begin
  inherited;
  FCertificatesPath := GetCertificatesPath;
  FCertificates := TCertificateList.Create;
  FCertificatesInfoFile := FCertificatesPath + DirectorySeparator + 'certinfo.ini';
end;

procedure TDbcProxyCertificateStore.LoadCertificates;
var
  CertInfo: TIniFile;
  Sections: TStringList;
  x: Integer;
  Certificate: TCertificateInfo;
  SectionName: String;
begin
  if not DirectoryExists(FCertificatesPath)
    then ForceDirectories(FCertificatesPath);

  try
    CertInfo := TIniFile.Create(FCertificatesInfoFile);
    Sections := TStringList.Create;
    CertInfo.ReadSections(Sections);
    for
      x := 0 to Sections.Count - 1 do begin
        SectionName := Sections.Strings[x];
        Certificate.SectionName := SectionName;
        Certificate.CertificateFile := CertInfo.ReadString(SectionName, 'CertificateFile', '');
        Certificate.KeyFile := CertInfo.ReadString(SectionName, 'KeyFile', '');
        Certificate.PublicKey := CertInfo.ReadString(SectionName, 'PublicKey', '');
        Certificate.ValidFrom := CertInfo.ReadDate(SectionName, 'ValidFrom', 0);
        Certificate.ValidUntil := CertInfo.ReadDate(SectionName, 'ValidUntil', 0);
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
    IsValid := Certificate.ValidUntil > Date;
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
begin
  Certificate := FCertificates.Items[Index];
  FCertificates.Delete(Index);
  If (Certificate.KeyFile <> '') and FileExists(Certificate.KeyFile) then
    SysUtils.DeleteFile(Certificate.KeyFile);
  If (Certificate.CertificateFile <> '') and FileExists(Certificate.CertificateFile) then
    SysUtils.DeleteFile(Certificate.CertificateFile);
  CertInfo := TIniFile.Create(FCertificatesInfoFile);
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
begin
  if CreateSelfSignedCert('zeos', '', 365, Certificate, PrivateKey) then begin

  end;
end;

(*
procedure TDbcProxyCertificateStore.MaintainStore;
var
  Certificate: TCertificateInfo;
begin


end;
*)
end.

