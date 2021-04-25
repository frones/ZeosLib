{###############################################################################
                      https://github.com/wendelb/DelphiOTP
###############################################################################}
unit GoogleOTP;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}System.{$ENDIF}SysUtils, {$IFNDEF FPC}System.{$ENDIF}Math, Base32U, {$IFNDEF FPC}System.{$ENDIF}DateUtils
  {$IFNDEF FPC}
    , IdGlobal, IdHMACSHA1
  {$ELSE}
    , HMAC
  {$IFEND};

(*

Test Case for the CalculateOTP function
---------------------------------------

Init key: AAAAAAAAAAAAAAAAAAAA
Timestamp: 1
BinCounter: 0000000000000001 (HEX-Representation)
Hash: eeb00b0bcc864679ff2d8dd30bec495cb5f2ee9e (HEX-Representation)
Offset: 14
Part 1: 73
Part 2: 92
Part 3: 181
Part 4: 242
One time password: 812658

Easy Display: Format('%.6d', [CalculateOTP(SECRET)]);
*)

function CalculateOTP(const Secret: String; const Counter: Integer = -1): Integer;
function ValidateTOPT(const Secret: String; const Token: Integer; const WindowSize: Integer = 4): Boolean;
function GenerateOTPSecret(len: Integer = -1): String;

implementation

Type
  {$IFNDEF FPC}
  OTPBytes = TIdBytes;
  {$ELSE}
  OTPBytes = TBytes;
  {$IFEND}

const
  otpLength = 6;
  keyRegeneration = 30;
  SecretLengthDef = 20;

{$IFDEF FPC}
function BytesToStringRaw(const InValue: TBytes): RawByteString;
begin
  SetString(Result, PAnsiChar(Pointer(InValue)), length(InValue));
end;

function RawByteStringToBytes(const InValue: RawByteString): TBytes;
begin
  Result := [];
  SetLength(Result, Length(InValue));
  Move(InValue[1], Result[0], Length(InValue));
end;

function ToBytes(const InValue: Int64): TBytes;
begin
  Result := [];
  SetLength(Result, SizeOf(Int64));
  Move(InValue, Result[0], SizeOf(Int64));
end;
{$ENDIF}

/// <summary>
///   Sign the Buffer with the given Key
/// </summary>
function HMACSHA1(const _Key: OTPBytes; const Buffer: OTPBytes): OTPBytes;
begin
  {$IFNDEF FPC}
  with TIdHMACSHA1.Create do
  begin
    Key := _Key;
    Result := HashValue(Buffer);
    Free;
  end;
  {$ELSE}
    Result := HMAC.HMACSHA1Digest(BytesToStringRaw(_Key), BytesToStringRaw(Buffer));
  {$IFEND}
end;

/// <summary>
///   Reverses TIdBytes (from low->high to high->low)
/// </summary>
function ReverseIdBytes(const inBytes: OTPBytes): OTPBytes;
var
  i: Integer;
begin
  {$IFDEF FPC}Result := [];{$IFEND}
  SetLength(Result, Length(inBytes));
  for i := Low(inBytes) to High(inBytes) do
    Result[High(inBytes) - i] := inBytes[i];
end;

/// <summary>
///   My own ToBytes function. Something in the original one isn't working as expected.
/// </summary>
function StrToIdBytes(const inString: String): OTPBytes;
var
  ch: Char;
  i: Integer;
begin
  {$IFDEF FPC}Result := [];{$ENDIF}
  SetLength(Result, Length(inString));

  i := 0;
  for ch in inString do
  begin
    Result[i] := Ord(ch);
    inc(i);
  end;
end;

function CalculateOTP(const Secret: String; const Counter: Integer = -1): Integer;
var
  BinSecret: String;
  Hash: String;
  Offset: Integer;
  Part1, Part2, Part3, Part4: Integer;
  Key: Integer;
  Time: Integer;
begin

  if Counter <> -1 then
    Time := Counter
  else
    Time := DateTimeToUnix(Now, False) div keyRegeneration;

  BinSecret := Base32.Decode(Secret);
  Hash := BytesToStringRaw(HMACSHA1(StrToIdBytes(BinSecret), ReverseIdBytes(ToBytes(Int64(Time)))));

  Offset := (ord(Hash[20]) AND $0F) + 1;
  Part1 := (ord(Hash[Offset+0]) AND $7F);
  Part2 := (ord(Hash[Offset+1]) AND $FF);
  Part3 := (ord(Hash[Offset+2]) AND $FF);
  Part4 := (ord(Hash[Offset+3]) AND $FF);

  Key := (Part1 shl 24) OR (Part2 shl 16) OR (Part3 shl 8) OR (Part4);
  Result := Key mod Trunc(IntPower(10, otpLength));
end;

function ValidateTOPT(const Secret: String; const Token: Integer; const WindowSize: Integer = 4): Boolean;
var
  TimeStamp: Integer;
  TestValue: Integer;
begin
  Result := false;

  TimeStamp := DateTimeToUnix(Now, False) div keyRegeneration;
  for TestValue := Timestamp - WindowSize to TimeStamp + WindowSize do
  begin
    if (CalculateOTP(Secret, TestValue) = Token) then
      Result := true;
  end;
end;

function GenerateOTPSecret(len: Integer = -1): String;
var
  i : integer;
  ValCharLen : integer;
begin
  Result := '';
  ValCharLen := Length(Base32U.ValidChars);

  if (len < 1) then
    len := SecretLengthDef;

  for i := 1 to len do
  begin
    Result := Result + copy(Base32U.ValidChars, Random(ValCharLen) + 1, 1);
  end;
end;

end.
