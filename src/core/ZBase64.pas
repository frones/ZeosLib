unit ZBase64;

{$I ZCore.inc}

interface

{$IFDEF WITH_TBYTES}

uses
  Classes, SysUtils;

function ZDecodeBase64(const InStr: {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF}): TBytes;
function ZEncodeBase64(const InValue: TBytes): {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF};

{$ENDIF}

implementation

{$IFDEF WITH_TBYTES}

uses {$IFDEF WITH_NETENCODING}
     System.NetEncoding
     {$ELSE}
       {$IFDEF FPC}
       Base64
       {$ELSE}
       EncdDecd
       {$ENDIF}
     {$ENDIF};

function ZDecodeBase64(const InStr: {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF}): TBytes;
{$IFNDEF WITH_NETENCODING}
var
  {$IFDEF FPC}
  InStream: TStringStream;
  DecodingStream: TBase64DecodingStream;
  OutStream: TBytesStream;
  {$ELSE}
  InStream: TMemoryStream;
  OutStream: TBytesStream;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF WITH_NETENCODING}
  Result := TNetEncoding.Base64.DecodeStringToBytes(String(InStr));
  {$ELSE}
    {$IFDEF FPC}
    try
      InStream := TStringStream.Create(InStr);
      DecodingStream := TBase64DecodingStream.Create(InStream);
      OutStream := TBytesStream.Create;

      OutStream.CopyFrom(DecodingStream, DecodingStream.Size);
      Result := OutStream.Bytes;
    finally
      if Assigned(OutStream) then
        FreeAndNil(OutStream);
      if Assigned(DecodingStream) then
        FreeAndNil(DecodingStream);
      if Assigned(InStream) then
        FreeAndNil(InStream);
    end;
    {$ELSE}
    try
      InStream := TMemoryStream.Create;
      OutStream := TBytesStream.Create;

      InStream.Write(InStr[1], Length(InStr));
      InStream.Position := 0;

      DecodeStream(InStream, OutStream);

      Result := OutStream.Bytes;
    finally
      if Assigned(InStream) then
        FreeAndNil(InStream);
      if Assigned(OutStream) then
        FreeAndNil(OutStream);
    end;
    {$ENDIF}
  {$ENDIF}
end;

function ZEncodeBase64(const InValue: TBytes): {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF};
{$IFNDEF WITH_NETENCODING}
var
  {$IFDEF FPC}
  InStream: TBytesStream;
  EncodingStream: TBase64EncodingStream;
  OutStream: TStringStream;
  {$ELSE}
  InStream: TBytesStream;
  OutStream: TMemoryStream;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF WITH_NETENCODING}
  Result := {$IFNDEF NEXTGEN}AnsiString{$ENDIF}(TNetEncoding.Base64.EncodeBytesToString(InValue));
  {$ELSE}
    {$IFDEF FPC}
    try
      OutStream := TStringStream.Create('');
      EncodingStream := TBase64EncodingStream.Create(OutStream);
      InStream := TBytesStream.Create(InValue);

      EncodingStream.CopyFrom(InStream, InStream.Size);
      EncodingStream.Flush;
      Result := OutStream.DataString;
    finally
      if Assigned(OutStream) then
        FreeAndNil(OutStream);
      if Assigned(EncodingStream) then
        FreeAndNil(EncodingStream);
      if Assigned(InStream) then
        FreeAndNil(InStream);
    end;
    {$ELSE}
    try
      InStream := TBytesStream.Create(InValue);
      OutStream := TMemoryStream.Create;

      EncodeStream(InStream, OutStream);

      SetLength(Result, OutStream.Size);
      Move(OutStream.Memory^, Result[1], OutStream.Size);
    finally
      if Assigned(InStream) then
        FreeAndNil(InStream);
      if Assigned(OutStream) then
        FreeAndNil(OutStream);
    end;
    {$ENDIF}
  {$ENDIF}
end;

{$ENDIF}

end.

