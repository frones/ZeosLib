{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Regular Expressions                    }
{                                                         }
{            Originally written by Sergey Seroukhov       }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZMatchPattern;
{
  Author: Kevin Boylan
  Ported By: Sergey Seroukhov

  This code is meant to allow wildcard pattern matches.
  It is VERY useful for matching filename wildcard patterns.
  It allows unix grep-like pattern comparisons, for instance:

	?	   	Matches any single characer
	*	   	Matches any contiguous characters
	[abc]  	Matches a or b or c at that position
	[^abc]	Matches anything but a or b or c at that position
	[!abc]	Ditto
	[a-e]  	Matches a through e at that position

	'ma?ch.*'	-Would match match.exe, mavch.dat, march.on, etc
	'this [e-n]s a [!zy]est' - Would match 'this is a test',
                               but would not match 'this as a yest'

  This is a Delphi VCL translation from C code that was downloaded from CIS.
  C code was written by J. Kerceval and released to public domain 02/20/1991.
  This code is ofcourse also public domain. I would appreciate it if you would
  let me know if you find any bugs.  I would also appreciate any notes sent my
  way letting me know if you find it useful.
}

{$I ZCore.inc}

interface

uses SysUtils;

{ Check if Text equal to pattern }
function IsMatch(const Pattern, Text: string): Boolean;

implementation

const
{ Match defines }
  MATCH_PATTERN	  = 6;
  MATCH_LITERAL	  = 5;
  MATCH_RANGE	  = 4;
  MATCH_ABORT	  = 3;
  MATCH_END	  = 2;
  MATCH_VALID	  = 1;
  MATCH_PROCESSING = 0;
{ Pattern defines }
{  PATTERN_VALID	  =  0;
  PATTERN_ESC	  = -1;
  PATTERN_RANGE	  = -2;
  PATTERN_CLOSE	  = -3;
  PATTERN_EMPTY	  = -4;
}{ Character defines }
  MATCH_CHAR_SINGLE	        = '?';
  MATCH_CHAR_KLEENE_CLOSURE     = '*';
  MATCH_CHAR_RANGE_OPEN	        = '[';
  MATCH_CHAR_RANGE	        = '-';
  MATCH_CHAR_RANGE_CLOSE        = ']';
  MATCH_CHAR_CARET_NEGATE       = '^';
  MATCH_CHAR_EXCLAMATION_NEGATE	= '!';

function Matche(P, PEnd, T, TEnd: PChar): Integer; forward;
function MatchAfterStar(P, PEnd, T, TEnd: PChar): Integer; forward;

function IsMatch(const Pattern, Text: string): Boolean;
var aPattern, aText: string;
  P, PEnd, T, TEnd: PChar;
begin
  {EH: Why is the (Ansi)LowerCase() required? We use it for the filter expressins only and the
    Strings(non Unicode) my have a different encoding!
    I would start from the premisse we match case-sensitive
    otherwise a explicit Lower() or Upper() using the filters should be done, IMHO
    The matche-method wasn't made for }
  aPattern := AnsiLowerCase(Pattern);
  aText    := AnsiLowerCase(Text);
  P := Pointer(aPattern);
  T := Pointer(aText);
  if (P = nil) or (T = nil) then begin
    Result := False;
    Exit;
  end;
  PEnd := P + Length(aPattern) -1;
  TEnd := T + Length(aText) -1;
  Result := (Matche(P, PEnd, T, TEnd) = MATCH_VALID);
end;

function Matche(P, PEnd, T, TEnd: PChar): Integer;
var
  RangeStart, RangeEnd: PChar;
  Invert, MemberMatch, Loop: Boolean;
Label MP;
begin
  If (P^ = '*') and (P = PEnd) then begin { EH: a single '*' matches everything }
    Result := MATCH_VALID;
    exit;
  end;
  Result := MATCH_PROCESSING;
  while ((Result = MATCH_PROCESSING) and (P <= PEnd)) do begin
    if T > TEnd then begin
      if (P^ = MATCH_CHAR_KLEENE_CLOSURE) and (P+1 > PEnd)
      then Result := MATCH_VALID
      else Result := MATCH_ABORT;
      Exit;
    end else
      case (P^) of
        MATCH_CHAR_KLEENE_CLOSURE:
          Result := MatchAfterStar(P,PEnd, T,TEnd);
        MATCH_CHAR_RANGE_OPEN:
          begin
            Inc(P);
            Invert := False;
            if (p > PEnd) then goto MP;
            if (P^ = MATCH_CHAR_EXCLAMATION_NEGATE) or
               (P^ = MATCH_CHAR_CARET_NEGATE) then begin
              Invert := True;
              Inc(P);
              if (p > PEnd) then goto MP;
            end;
            if (P^ = MATCH_CHAR_RANGE_CLOSE) then goto MP;
            MemberMatch := False;
            Loop := True;
            while (Loop and (P^ <> MATCH_CHAR_RANGE_CLOSE)) do begin
              RangeStart := P;
              RangeEnd := P;
              Inc(P);
              if P > PEnd then goto MP;
              if P^ = MATCH_CHAR_RANGE then begin
                Inc(P);
                RangeEnd := P;
                if (P > PEnd) or (RangeEnd^ = MATCH_CHAR_RANGE_CLOSE) then goto MP;
                Inc(P);
              end;
              if P > PEnd then goto MP;
              if RangeStart < RangeEnd then begin
                if (T^ >= RangeStart^) and
                   (T^ <= RangeEnd^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end else begin
                if (T^ >= RangeEnd^) and
                   (T^ <= RangeStart^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end;
            end;
            if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then begin
              Result := MATCH_RANGE;
              Exit;
            end;
            if MemberMatch then
              while (P <= PEnd) and (P^ <> MATCH_CHAR_RANGE_CLOSE) do
                Inc(P);
              if P > PEnd then begin
MP:             Result := MATCH_PATTERN;
                Exit;
              end;
          end;
        else if (P^ <> MATCH_CHAR_SINGLE) then
          if (P^ <> T^) then
            Result := MATCH_LITERAL;
      end;
    Inc(P);
    Inc(T);
  end;
  if Result = MATCH_PROCESSING then
    if T <= TEnd
    then Result := MATCH_END
    else Result := MATCH_VALID;
end;

function MatchAfterStar(P, PEnd, T, TEnd: PChar): Integer;
label MV, MA;
begin
  Result := MATCH_PROCESSING;
  While (( T <= TEnd ) and (P <= PEnd)) and
     (Ord(P^) in [Ord(MATCH_CHAR_SINGLE), Ord(MATCH_CHAR_KLEENE_CLOSURE)]) do begin
    if P^ = MATCH_CHAR_SINGLE then
      Inc(T);
    Inc(P);
  end;
  If (T > TEnd) then goto MA;
  If (p > PEnd) then goto MV;
  repeat
    If (PEnd >= P) and ((P^ = T^) or (P^ = MATCH_CHAR_RANGE_OPEN)) then begin
      Result  := Matche(P, PEnd, T, TEnd);
      if Result <> MATCH_VALID then
        Result := MATCH_PROCESSING;//retry until end of Text, (check below) or Result valid
    end;
    if (T > TEnd) or (P > PEnd) then begin
MA:   Result := MATCH_ABORT;
      Exit;
    end;
    Inc(T);
  //until Result <> MATCH_PROCESSING
  Until (Result = MATCH_VALID) or (t > TEnd);
  if (p > PEnd) and (t > TEnd) then
MV: Result := MATCH_VALID;
end;

end.
