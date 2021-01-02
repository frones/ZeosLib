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

unit YubiOtp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, HMAC;

type
  TYubiOtpStatus = (yosUnknown, yosOk, yosBadOtp, yosReplayedOtp, yosBadSignature, yosMissingParameter, yosNoSuchClient, yosOperationNotAllowed, yosBackendError, yosNotEnoughAnswers, yosReplayedRequest);

/// <summary>
///  This function encodes a THMACSHA1Digest as a Base64 string.
/// </summary>
/// <param name="Digest">
///  The digest that is to be encoded.
/// </param>
/// <returns>
///  The Base64 encoded HMAC-SHA1 digest.
/// </returns>
function EncodeHmacSha1DigestBase64(const Digest: THMACSHA1Digest):String;
/// <summary>
///  This function converts a textual status as retuned by a YubiKey Validation Server to a TYubiOtpStatus.
/// </summary>
/// <param name="Status">
///  The status to be converted.
/// </param>
/// <returns>
///  The status. yosUnknown, if the status cannot be converted.
/// </returns>
function StringToYubiOtpStatus(const Status: String): TYubiOtpStatus;
/// <summary>
///  This procedure checks a Yubikey OTP status. If it is not yosOk, an exception will be raised.
/// </summary>
/// <param name="Status">
///  The status to be checked.
/// </param>
procedure RaiseYubiOtpError(Status: TYubiOtpStatus);
/// <summary>
///  This function checks a yubikey OTP against the given YubiKey Validation Server.
/// </summary>
/// <param name="BaseUrl">
///  This is the base URL for the validation server.
///  Usually it is https://<servername>/wsapi/2.0/verify.
///  In case of the yubicloud it should be 'https://api.yubico.com/wsapi/2.0/verify'.
/// </param>
/// <param name="OTP">
///  This is the OTP to be checked by the validation server.
/// </param>
/// <param name="RemainingPassword">
///  If there is a user password before the yubikey OTP, it will be retuned in this parameter.
/// </param>
/// <param name="ClientId">
///  The client id to be used for identifying ourselves to the validation server.
/// </param>
/// <param name="SecretKey">
///  The secret key to be used to verify the authentication server signature.
///  If no secret key is given, the signature will not be checked.
/// </param>
/// <returns>
///  The status retuned by the validation server. yosOk means the password is ok.
/// </returns>
function VerifyYubiOtp(const BaseUrl: String; OTP: String; out RemainingPassword: String; const ClientId: Integer; const SecretKey: String = ''): TYubiOtpStatus;

implementation

uses
  fphttpclient, opensslsockets, base64;

function EncodeHmacSha1DigestBase64(const Digest: THMACSHA1Digest):String;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
  Size: Integer;
begin
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Size := SizeOf(Digest);
      Encoder.Write(Digest, Size);
    finally
      Encoder.Free;
    end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

function StringToYubiOtpStatus(const Status: String): TYubiOtpStatus;
begin
  if Status = 'OK' then Result := yosOk
  else if Status = 'BAD_OTP' then Result := yosBadOtp
  else if Status = 'REPLAYED_OTP' then Result := yosReplayedOtp
  else if Status = 'BAD_SIGNATURE' then Result := yosBadSignature
  else if Status = 'MISSING_PARAMETER' then Result := yosMissingParameter
  else if Status = 'NO_SUCH_CLIENT' then Result := yosNoSuchClient
  else if Status = 'OPERATION_NOT_ALLOWED' then Result := yosOperationNotAllowed
  else if Status = 'BACKEND_ERROR' then Result := yosBackendError
  else if Status = 'NOT_ENOUGH_ANSWERS' then Result := yosNotEnoughAnswers
  else if Status = 'REPLAYED_REQUEST' then Result := yosReplayedRequest
  else Result := yosUnknown;
end;

procedure RaiseYubiOtpError(Status: TYubiOtpStatus);
begin
  case Status of
    yosUnknown: raise Exception.Create('The server returned an unknown or empty status value.');
    yosBadOtp: raise Exception.Create('The OTP is invalid format.');
    yosReplayedOtp: raise Exception.Create('The OTP has already been seen by the service.');
    yosBadSignature: raise Exception.Create('The HMAC signature verification failed.');
    yosMissingParameter: raise Exception.Create('The request lacks a parameter.');
    yosNoSuchClient: raise Exception.Create('The request id does not exist.');
    yosOperationNotAllowed: raise Exception.Create('The request id is not allowed to verify OTPs.');
    yosBackendError: raise Exception.Create('Unexpected error in our server. Please contact us if you see this error.');
    yosNotEnoughAnswers: raise Exception.Create('Server could not get requested number of syncs during before timeout');
    yosReplayedRequest: raise Exception.Create('Server has seen the OTP/Nonce combination before');
  end;
end;

function VerifyYubiOtp(const BaseUrl: String; OTP: String; out RemainingPassword: String; const ClientId: Integer; const SecretKey: String = ''): TYubiOtpStatus;
const
  DefaultOtpLen = 44; // a Yubikey OTP usually is 44 characters long
var
  OtpLen: Integer;
  URL: String;
  Client: TFPHTTPClient;
  NonceI: Int64;
  Nonce: String;
  Lines: TStringList;
  Line: String;
  Signature1, Signature2: String;
  x: integer;
  Digest: THMACSHA1Digest;
  Key: RawByteString;
  Status: String;
begin
  // get the real OTP, return the remaining string, if any
  OtpLen := Length(OTP);
  if OtpLen < DefaultOtpLen then raise Exception.Create('The OTP needs to have a minimum of 44 characters') else begin
    if OtpLen > DefaultOtpLen then begin
      RemainingPassword := Copy(OTP, 1, OtpLen - DefaultOtpLen);
      Delete(OTP, 1, OtpLen - DefaultOtpLen);
    end else begin
      RemainingPassword := '';
    end;
  end;

  // generate a nonce
  NonceI := High(Int64);
  NonceI := Random(NonceI);
  Nonce := IntToHex(NonceI, 16);

  // create the URL. This could be better...
  URL := BaseUrl;
  URL := URL + '?id=' + IntToStr(ClientId);
  URL := URL + '&otp=' + OTP;
  URL := URL + '&nonce=' + Nonce;

  Lines := TStringList.Create;
  try
    // send the request to the server
    Client := TFPHTTPClient.Create(nil);
    try
      Client.Get(URL, Lines);
    finally
      FreeAndNil(Client);
    end;

    // check OTP and extract STATUS
    Status := Lines.Values['otp'];
    if Status <> OTP then
      raise Exception.Create('The OTP in the reply doesn''t match the otp that was given to the service.');
    Status := Lines.Values['status'];
    Result := StringToYubiOtpStatus(Status);

    // check the signature of the result, if possible
    if SecretKey <> '' then begin
      for x := Lines.Count - 1 downto 0 do
        if trim(Lines[x]) = '' then Lines.Delete(x);

      Signature1 := Lines.Values['h'];
      Lines.Delete(Lines.IndexOfName('h'));
      Lines.Sort;
      Line := '';
      for x := 0 to Lines.Count - 1 do begin
        if x > 0 then Line := Line + '&';
        Line := Line + Lines[x];
      end;

      Key := DecodeStringBase64(SecretKey);

      Digest := HMACSHA1Digest(Key, Line);
      Signature2 := EncodeHmacSha1DigestBase64(digest);

      if Signature1 <> Signature2 then
        raise Exception.Create('The signature of the service reply is invalid.');
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

initialization
  Randomize;

end.

