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
  OutStream: {$IF DECLARED(TBytesStream)}TBytesStream{$ELSE}TMemoryStream{$IFEND};
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
      OutStream := {$IF DECLARED(TBytesStream)}TBytesStream{$ELSE}TMemoryStream{$IFEND}.Create;

      InStream.Write(InStr[1], Length(InStr));
      InStream.Position := 0;

      DecodeStream(InStream, OutStream);

      {$IF DECLARED(TBytesStream)}
      Result := OutStream.Bytes;
      {$ELSE}
      SetLength(Result, OutStream.Size);
      OutStream.Position := 0;
      OutStream.Read(Result[0], OutStream.Size);
      {$IFEND}
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
  InStream: {$IF DECLARED(TBytesStream)}TBytesStream{$ELSE}TMemoryStream{$IFEND};
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
      {$IF DECLARED(TBytesStream)}
      InStream := TBytesStream.Create(InValue);
      {ELSE}
      InStream := TMemoryStream.Create;
      InStream.Write(InValue[0], Length[InValue]);
      InStream.Position := 0;
      {$IFEND}
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

