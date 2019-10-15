{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctionsStrings;

interface

{$I ZCore.inc}

uses
  SysUtils, ZFunctions, ZExpression, ZVariant;

{**  String functions}

type
  {** Implements a abstract string function. }
  TZAbstractStringFunction = class(TZAbstractFunction)
  protected
    function IsUnicodeVar(var Value: TZVariant;
      const VariantManager: IZVariantManager): Boolean;
  end;

  {** Implements a CONCAT function. }
  TZConcatFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SUBSTR function. }
  TZSubStrFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LEFT function. }
  TZLeftFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RIGHT function. }
  TZRightFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a STRPOS function. }
  TZStrPosFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LENGTH function. }
  TZLengthFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a UPPER function. }
  TZUpperFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOWER function. }
  TZLowerFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CAPITALIZE function. }
  TZCapitalizeFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TRIM function. }
  TZTrimFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LTRIM function. }
  TZLTrimFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RTRIM function. }
  TZRTrimFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SOUNDEX function. }
  TZSoundexFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LEVENSHTEINDIST function. }
  TZLevenshteinDistanceFunction = class (TZAbstractStringFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

Function Capitalize(const s: string; const Delims : string = '') : string;
Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;
procedure AddStringFunctions(Functions : TZFunctionsList);

implementation

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} Math, StrUtils,
  ZSysUtils, ZMessages, ZCompatibility, ZFastCode;

{$IFDEF WITH_TSYSCHARSET_DEPRECATED}
var
  sStdWordDelims: String;
{$ENDIF}

Function Capitalize(const s: string; const Delims : string = '') : string;
var
  {$IFDEF WITH_TSYSCHARSET_DEPRECATED}
  sDelims : String;
  {$ELSE}
  sDelims : TSysCharSet;
  i : integer;
  P: PChar;
  {$ENDIF}
begin
  if Delims = '' then
    sDelims := {$IFDEF WITH_TSYSCHARSET_DEPRECATED}sStdWordDelims{$ELSE}StdWordDelims{$ENDIF}
  else begin
    {$IFDEF WITH_TSYSCHARSET_DEPRECATED}
    sDelims := Delims;
    {$ELSE}
    sDelims := [];
    P := Pointer(Delims);
    for i:=0 to Length(Delims)-1 do
      Include(sDelims,AnsiChar((P+i)^))
    {$ENDIF}
  end;
  Result := AnsiProperCase(s, sDelims);
end;

Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;

var
  d      : array of array of Integer;
  s,t    : string;
  Start,
  Len1,
  Len2,
  i, j,
  Cost : Integer;

begin
  Len1 := Length(s1);
  Len2 := Length(s2);

  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;
  if DoUpcase then
  begin
    s := Uppercase(s1);
    t := Uppercase(s2);
  end
  else
  begin
    s := s1;
    t := s2;
  end;
  start := 1;
//  trim off the matching items at the beginning
  while (start <= Len1) and (start <= Len2) and (s[start] = t[start]) do
      INC(start);
//  trim off the matching items at the end
  while (start <= Len1) and (start <= Len2) and (s[Len1] = t[Len2]) do
  begin
    DEC(Len1);
    DEC(Len2);
  end;

  DEC(Start);

  DEC(Len1, Start);
  DEC(Len2, Start);

  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;

  setlength(d, Len1 + 1, Len2 + 1);
  for i := 0 to Len1 do
    d[i, 0] := i;
  for j := 0 to Len2 do
    d[0, j] := j;

//  only loop over the items that are different
  for i := 1 to Len1 do
  begin
    for j := 1 to Len2 do
    begin
      Cost := ABS(ORD(s[i+start] <> t[j+start]));
      d[i, j] := Min(
                     Min(d[i-1,j]+1,          // deletion
                         d[i,j-1]+1),         // insertion
                         d[i-1,j-1]+Cost);    // substitution
    end;
  end;
  Result := d[Len1, Len2];
end;

{****  This is the original not optimized version
Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;

var
  d      : array of array of Integer;
  s,t    : string;
  Len1,
  Len2,
  i, j,
  Cost : Integer;
begin
  Len1 := Length(s1);
  Len2 := Length(s2);
  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;
  if DoUpcase then
  begin
    s := Upcase(s1);
    t := Upcase(s2);
  end
  else
  begin
    s := s1;
    t := s2;
  end;
  setlength(d, Len1 + 1, Len2 + 1);
  for i := 0 to Len1 do
    d[i, 0] := i;
  for j := 0 to Len2 do
    d[0, j] := j;
  for i := 1 to Len1 do
  begin
    for j := 1 to Len2 do
    begin
      Cost := ABS(ORD(s[i] <> t[j]));
      d[i, j] := Min(
                     Min(d[i-1,j]+1,
                         d[i,j-1]+1),
                         d[i-1,j-1]+Cost);
    end;
  end;
  Result := d[Len1, Len2];
end;
******}

{ TZConcatFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZConcatFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Value1, Value2: TZVariant;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);
  Value1 := Stack.GetParameter(ParamsCount);
  VariantManager.Assign(Value1, Result); //init vtType
  if IsUnicodeVar(Value1, VariantManager) then begin
    for I := ParamsCount - 1 downto 1 do begin
      Value2 := Stack.GetParameter(I);
      Result.VUnicodeString := Result.VUnicodeString + VariantManager.GetAsUnicodeString(Value2);
    end;
  end else begin
    for I := ParamsCount - 1 downto 1 do begin
      Value2 := Stack.GetParameter(I);
      if Value2.VType in [{$IFNDEF UNICODE}vtString,{$ENDIF}
        {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}vtRawByteString]
      {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
      then Result.VRawByteString := Result.VRawByteString + Value2.VRawByteString
      else Result.VRawByteString := Result.VRawByteString + VariantManager.GetAsRawByteString(Value2);
      {$ELSE}
      then Result.VRawByteString := RawConcat([Result.VRawByteString, Value2.VRawByteString])
      else Result.VRawByteString := RawConcat([Result.VRawByteString, VariantManager.GetAsRawByteString(Value2)]);
      {$ENDIF}
    end;
  end;
end;

{ TZSubStrFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSubStrFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2, Value3: TZVariant;
begin
  CheckParamsCount(Stack, 3);
  Value1 := Stack.GetParameter(1);
  Value2 := Stack.GetParameter(2);
  Value3 := Stack.GetParameter(3);

  VariantManager.Assign(Value3, Result);
  if IsUnicodeVar(Value3, VariantManager)
  then Result.VUnicodeString := Copy(Result.VUnicodeString, VariantManager.GetAsInteger(Value2), VariantManager.GetAsInteger(Value1))
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  else Result.VRawByteString := Copy(Result.VRawByteString, VariantManager.GetAsInteger(Value2), VariantManager.GetAsInteger(Value1));
  {$ELSE}
  else begin
    Result.VRawByteString := Copy(Result.VRawByteString, VariantManager.GetAsInteger(Value2)-1, VariantManager.GetAsInteger(Value1)+1);
    Result.VRawByteString[High(Result.VRawByteString)] := 0;
  end;
  {$ENDIF}
end;

{ TZLeftFunction }
function TZLeftFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2: TZVariant;
begin
  CheckParamsCount(Stack, 2);
  Value1 := Stack.GetParameter(2);
  Value2 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager)
  then Result.VUnicodeString := Copy(Result.VUnicodeString, 1, Value2.VInteger)
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  else begin
    Result.VRawByteString := Copy(Result.VRawByteString, 0, Value2.VInteger +1);
    Result.VRawByteString[Value2.VInteger] := 0;
  end;
  {$ELSE}
  else Result.VRawByteString := Copy(Result.VRawByteString, 1, Value2.VInteger);
  {$ENDIF}
end;

{ TZRightFunction }
function TZRightFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2: TZVariant;
begin
  CheckParamsCount(Stack, 2);
  Value1 := Stack.GetParameter(2);
  Value2 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager)
  then Result.VUnicodeString := Copy(Value1.VUnicodeString, Length(Value1.VUnicodeString) + 1 - Value2.VInteger, Value2.VInteger)
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  else begin
    Result.VRawByteString := Copy(Value1.VRawByteString, Length(Value1.VRawByteString) - Value2.VInteger, Value2.VInteger+1);
    Result.VRawByteString[High(Result.VRawByteString)] := 0;
  end;
  {$ELSE}
  else Result.VRawByteString := Copy(Value1.VRawByteString, Length(Value1.VRawByteString) + 1 - Value2.VInteger, Value2.VInteger);
  {$ENDIF}
end;

{ TZStrPosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZStrPosFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2: TZVariant;
  iPos: Integer;
  UniTmp: UnicodeString;
begin
  CheckParamsCount(Stack, 2);
  Value1 := Stack.GetParameter(1);
  Value2 := Stack.GetParameter(2);
  if IsUnicodeVar(Value1, VariantManager) then begin
    UniTmp := VariantManager.GetAsUnicodeString(Value2);
    iPos := ZFastCode.Pos(UniTmp, Value1.VUnicodeString);
  end else begin
    if Value2.VType <> Value1.VType then
      Value2 := VariantManager.Convert(Value2, Value1.VType);
    iPos := ZFastCode.Pos(Value2.VRawByteString, Value1.VRawByteString);
  end;
  VariantManager.SetAsInteger(Result, iPos);
end;

{ TZLengthFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLengthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
  L: LengthInt;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  if IsUnicodeVar(Value1, VariantManager)
  then L := Length(Value1.VUnicodeString)
  else L := Length(Value1.VRawByteString){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  VariantManager.SetAsInteger(Result, L);
end;

{ TZLowerFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLowerFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager)
  then Result.VUnicodeString := {$IFDEF UNICODE}AnsiLowerCase{$ELSE}WideLowerCase{$ENDIF}(Result.VUnicodeString)
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  else Result.VRawByteString := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiLowerCase(Result.VRawByteString);
  {$ELSE}
  else Result.VRawByteString := ZSysUtils.LowerCase(Result.VRawByteString);
  {$ENDIF}
end;

{ TZUpperFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZUpperFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager)
  then Result.VUnicodeString := {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(Result.VUnicodeString)
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  else Result.VRawByteString := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(Result.VRawByteString);
  {$ELSE}
  else Result.VRawByteString := ZSysUtils.UpperCase(Result.VRawByteString);
  {$ENDIF}
end;

{ TZCapitalizeFunction }

function TZCapitalizeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 1) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 2) then
    VariantManager.SetAsString(Result, Capitalize(
      VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsString(Result, Capitalize(
      VariantManager.GetAsString(Stack.GetParameter(2)),
      VariantManager.GetAsString(Stack.GetParameter(1))))
end;

{ TZTrimFunction }

function TZTrimFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
  P: Pointer;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager) then begin
    P := Pointer(Result.VUnicodeString);
    Result.VUnicodeString := ZSysUtils.Trim(PWidechar(P), Length(Result.VUnicodeString));
  end else begin
    P := Pointer(Result.VRawByteString);
    Result.VRawByteString := ZSysUtils.Trim(PAnsichar(P), Length(Result.VRawByteString){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
  end;
end;

{ TZLTrimFunction }

function TZLTrimFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
  P: Pointer;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager) then begin
    P := Pointer(Result.VUnicodeString);
    Result.VUnicodeString := ZSysUtils.LTrim(PWidechar(P), Length(Result.VUnicodeString));
  end else begin
    P := Pointer(Result.VRawByteString);
    Result.VRawByteString := ZSysUtils.LTrim(PAnsichar(P), Length(Result.VRawByteString){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
  end;
end;

{ TZRTrimFunction }

function TZRTrimFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value1: TZVariant;
  P: Pointer;
begin
  CheckParamsCount(Stack, 1);
  Value1 := Stack.GetParameter(1);
  VariantManager.Assign(Value1, Result);
  if IsUnicodeVar(Value1, VariantManager) then begin
    P := Pointer(Result.VUnicodeString);
    Result.VUnicodeString := ZSysUtils.RTrim(PWidechar(P), Length(Result.VUnicodeString));
  end else begin
    P := Pointer(Result.VRawByteString);
    Result.VRawByteString := ZSysUtils.RTrim(PAnsichar(P), Length(Result.VRawByteString){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
  end;
end;

{ TZSoundexFunction }

function TZSoundexFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 1) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 2) then
    VariantManager.SetAsString(Result, Soundex(
      VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsString(Result, Soundex(
      VariantManager.GetAsString(Stack.GetParameter(2)),
      VariantManager.GetAsInteger(Stack.GetParameter(1))))
end;

function TZLevenshteinDistanceFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;

var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 3) then
    VariantManager.SetAsInteger(Result,
                                  LevenshteinDistance(
                                    VariantManager.GetAsString(Stack.GetParameter(2)),
                                    VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsInteger(Result,
                                LevenshteinDistance(
                                  VariantManager.GetAsString(Stack.GetParameter(3)),
                                  VariantManager.GetAsString(Stack.GetParameter(2)),
                                  VariantManager.GetAsBoolean(Stack.GetParameter(1))))
end;

procedure AddStringFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZConcatFunction.Create('CONCAT'));
  Functions.Add(TZSubStrFunction.Create('SUBSTR'));
  Functions.Add(TZLeftFunction.Create('LEFT'));
  Functions.Add(TZRightFunction.Create('RIGHT'));
  Functions.Add(TZStrPosFunction.Create('STRPOS'));
  Functions.Add(TZLengthFunction.Create('LENGTH'));

  Functions.Add(TZUpperFunction.Create('UPPER'));
  Functions.Add(TZLowerFunction.Create('LOWER'));
  Functions.Add(TZCapitalizeFunction.Create('CAP'));
  Functions.Add(TZCapitalizeFunction.Create('CAPITALIZE'));

  Functions.Add(TZTrimFunction.Create('TRIM'));
  Functions.Add(TZLTrimFunction.Create('LTRIM'));
  Functions.Add(TZRTrimFunction.Create('RTRIM'));

  Functions.Add(TZSoundexFunction.Create('SOUNDEX'));
  Functions.Add(TZLevenshteinDistanceFunction.Create('LEVDIST'));
  Functions.Add(TZLevenshteinDistanceFunction.Create('LEVENSHTEINDISTANCE'));
end;

{ TZAbstractStringFunction }

function TZAbstractStringFunction.IsUnicodeVar(var Value: TZVariant;
  const VariantManager: IZVariantManager): Boolean;
begin
  if Value.VType in [{$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString] then
    Result := True
  else if Value.VType in [{$IFNDEF UNICODE}vtString,{$ENDIF}
        {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}vtRawByteString]
    then Result := False
    else begin
      Value := VariantManager.Convert(Value, vtString);
      Result := {$IFDEF UNICODE}True{$ELSE}False{$ENDIF};
    end;
end;

{$IFDEF WITH_TSYSCHARSET_DEPRECATED}

{$WARNINGS OFF}
procedure sStdWordDelimsFiller;
var C: Char;
  P: PChar;
  I: Integer;
begin
  I := 0;
  for C := #0 to Char(High(Byte)) do
    Inc(I, Ord(C in StdWordDelims));
  SetLength(sStdWordDelims, I);
  P := Pointer(sStdWordDelims);
  for C := #0 to Char(High(Byte)) do
    if C in StdWordDelims then begin
      P^ := C;
      Inc(P);
    end;
end;
{$WARNINGS ON}

initialization

sStdWordDelimsFiller;

{$ENDIF}

end.

