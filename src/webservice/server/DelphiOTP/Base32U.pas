{###############################################################################
                      https://github.com/wendelb/DelphiOTP
###############################################################################}
unit Base32U;

{

================================================================================
If you found this Unit as your were looking for a delphi Base32 Implementation,
that is also unicode ready, please see the Readme!
================================================================================

}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}System.{$ENDIF}SysUtils;  // For UpperCase (Base32Decode)

type
  Base32 = class
  public
    /// <param name="inString">
    ///   Base32-String (Attention: No validity checks)
    /// </param>
    /// <summary>
    ///   Decodes a Base32-String
    /// </summary>
    /// <returns>
    ///   Unicode String containing the ANSI-Data from that Base32-Input
    /// </returns>
    class function Decode(const inString: String): String;
  end;

// As the FromBase32String Function doesn't has the result I'm looking for, here
// is my version of that function. This is converted from a PHP function, which
// can be found here: https://www.idontplaydarts.com/2011/07/google-totp-two-factor-authentication-for-php/
const
  ValidChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';

implementation

{$REGION 'Base32Functions'}


function Base32Decode(const source: String): String;
var
  UpperSource: String;
  p, i, l, n, j: Integer;
begin
  UpperSource := UpperCase(source);

  l := Length(source);
  n := 0; j := 0;
  Result := '';

  for i := 1 to l do
  begin
    n := n shl 5; 				// Move buffer left by 5 to make room

    p := Pos(UpperSource[i], ValidChars);
    if p >= 0 then
      n := n + (p - 1);         // Add value into buffer

		j := j + 5;				// Keep track of number of bits in buffer

    if (j >= 8) then
    begin
      j := j - 8;
      Result := Result + chr((n AND ($FF shl j)) shr j);
    end;
  end;
end;

{$ENDREGION}

{ Base32 }

class function Base32.Decode(const inString: String): String;
begin
  Result := Base32Decode(inString);
end;

end.
