unit md5crypt;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

// Note: The md5a unit is the MD5 unit as released by Assarbad.
uses {$IFNDEF FPC}md5A{$ELSE}md5{$IFEND};

function crypt_md5(pw: RawByteString; salt: ANSIString): AnsiString;
function crypt_md5pg(pw: UTF8String; UserName: UTF8String): AnsiString;

function md5salt: ANSIString;

implementation

const
  a64: array[0..63] of AnsiChar = './0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

function crypt_md5(pw: RawByteString; salt: AnsiString): AnsiString;
// Found at: http://www.delphipraxis.net/topic126233_md5+salt+shadowfile+aus+windows.html&highlight=md5+crypt
const
  magic: array[0..2] of AnsiChar = '$1$';
var
  ctx, ctx1: {$IFNDEF FPC}TMD5_CTX{$ELSE}TMD5Context{$IFEND};
  final: TMD5Digest;
  i: integer;
  l: longint;

  function to64(l: longint; n: integer): string;
  var
    s: string;
  begin
    s := '';
    while n>0 do begin
      s := s+a64[l and $3f];
      l := l shr 6;
      dec(n);
    end;
    to64 := s;
  end;

begin
  if copy(salt, 1, length(magic)) = magic then salt := copy(salt, length(magic) + 1, length(salt));
  l := Pos('$', salt);
  if l = 0 then l := length(salt) else l := l - 1;
  if l > 8 then l := 8;
  salt := copy(salt, 1, l);

  MD5Init(ctx);
  MD5Update(ctx, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw));
  MD5Update(ctx, {$IFNDEF FPC}@magic{$ELSE}magic{$IFEND}, 3);
  MD5Update(ctx, {$IFNDEF FPC}@salt[1]{$ELSE}salt[1]{$IFEND}, length(salt));

  MD5Init(ctx1);
  MD5Update(ctx1, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw));
  MD5Update(ctx1, {$IFNDEF FPC}@salt[1]{$ELSE}salt[1]{$IFEND}, length(salt));
  MD5Update(ctx1, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw));
  MD5Final(ctx1, final);
  i := length(pw);
  while i>0 do begin
    if i>16 then MD5Update(ctx, {$IFNDEF FPC}@final{$ELSE}final{$IFEND}, 16)
    else MD5Update(ctx, {$IFNDEF FPC}@final{$ELSE}final{$IFEND}, i);
    dec(i,16);
  end;
  fillchar(final,sizeof(final),0);
  i :=  length(pw);
  while i<>0 do begin
    if odd(i) then MD5Update(ctx, {$IFNDEF FPC}@final{$ELSE}final{$IFEND}, 1)
    else MD5Update(ctx, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, 1);
    i := i shr 1;
  end;
  MD5Final(ctx, final);
  for i:=0 to 999 do begin
    MD5Init(ctx1);
    if (i and 1) <>0 then MD5Update(ctx1, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw))
    else MD5Update(ctx1, {$IFNDEF FPC}@final{$ELSE}final{$IFEND}, 16);
    if i mod 3 <> 0 then MD5Update(ctx1, {$IFNDEF FPC}@salt[1]{$ELSE}salt[1]{$IFEND}, length(salt));
    if i mod 7 <> 0 then MD5Update(ctx1, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw));
    if (i and 1) <>0 then MD5Update(ctx1, {$IFNDEF FPC}@final{$ELSE}final{$IFEND}, 16)
    else MD5Update(ctx1, {$IFNDEF FPC}@pw[1]{$ELSE}pw[1]{$IFEND}, length(pw));
    MD5Final(ctx1, final);
  end;

  result := magic + salt + '$';
  l := (longint(final[ 0]) shl 16) or (longint(final[ 6]) shl 8) or final[12]; result := result + to64(l,4);
  l := (longint(final[ 1]) shl 16) or (longint(final[ 7]) shl 8) or final[13]; result := result + to64(l,4);
  l := (longint(final[ 2]) shl 16) or (longint(final[ 8]) shl 8) or final[14]; result := result + to64(l,4);
  l := (longint(final[ 3]) shl 16) or (longint(final[ 9]) shl 8) or final[15]; result := result + to64(l,4);
  l := (longint(final[ 4]) shl 16) or (longint(final[10]) shl 8) or final[ 5]; result := result + to64(l,4);
  l := final[11]; result := result + to64(l,2);
end;

function md5salt: AnsiString;
var
  lauf: Integer;
begin
  Result := '';
  Randomize;
  for lauf := 1 to 8 do Result := Result + a64[trunc(Random(64))]
end;

function crypt_md5pg(pw: UTF8String; UserName: UTF8String): AnsiString;
var
  Digest: TMD5Digest;
begin
  Digest := MD5String(pw+UserName);
  Result := 'md5' + MD5Print(Digest);
end;

end.
