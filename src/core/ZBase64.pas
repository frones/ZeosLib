unit ZBase64;

{$I ZCore.inc}

interface

{$IFDEF WITH_NETENCODING}

uses
  Classes, SysUtils, System.NetEncoding;

function DecodeBase64(const InStr: AnsiString): TBytes;
function EncodeBase64(const InValue: TBytes): String;

{$ENDIF}

implementation

{$IFDEF WITH_NETENCODING}

function DecodeBase64(const InStr: AnsiString): TBytes;
begin
  Result := TNetEncoding.Base64.DecodeStringToBytes(InStr);
end;

function EncodeBase64(const InValue: TBytes): String;
begin
  Result := String(TNetEncoding.Base64.EncodeBytesToString(InValue));
end;

{$ENDIF}

end.

