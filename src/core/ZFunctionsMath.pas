{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctionsMath;

interface

{$I ZCore.inc}

uses
  SysUtils, ZFunctions, ZExpression, ZVariant, ZFastCode;

{**  Math functions }

type
  {** Implements a E function. }
  TZEFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a PI function. }
  TZPIFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RND function. }
  TZRndFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ABS function. }
  TZAbsFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Trigonometric }
  {** Implements a COS function. }
  TZCosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a COT function. }
  TZCotFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SIN function. }
  TZSinFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TAN function. }
  TZTanFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ACOS function. }
  TZAcosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ASIN function. }
  TZAsinFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ATAN function. }
  TZAtanFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Rounding }
  {** Implements a ROUND function. }
  TZRoundFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TRUNC function. }
  TZTruncFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a INT function. }
  TZIntFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a FRAC function. }
  TZFracFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CEIL function. }
  TZCeilFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a FLOOR function. }
  TZFloorFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Logarithmic }
  {** Implements a EXP function. }
  TZExpFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG function. }
  TZLogFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG10 function. }
  TZLog10Function = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SQR function. }
  TZSqrFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant; override;
  end;

procedure AddMathFunctions(Functions : TZFunctionsList);

implementation

uses
  Math, FmtBCD;

{ TZEFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} //VariantManager is not used
function TZEFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  Result := EncodeDouble(Exp(1));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZPIFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} //VariantManager is not used
function TZPIFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  Result := EncodeDouble(PI);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZRndFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} //VariantManager is not used
function TZRndFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  Result := EncodeDouble(Random);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZAbsFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} //VariantManager is not used
function TZAbsFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  if Value.VType = vtInteger then
    Result := EncodeInteger(Abs(Value.VInteger))
  else if Value.VType = vtDouble then
    Result := EncodeDouble(Abs(Value.VDouble))
  else if Value.VType = vtCurrency then
    Result := EncodeCurrency(Abs(Value.VCurrency))
  else if (Value.VType = vtBigDecimal) then begin
    InitializeVariant(Result, vtBigDecimal);
    if IsBcdNegative(Value.VBigDecimal)
    then Result.VBigDecimal := Value.VBigDecimal
    else BcdMultiply(Value.VBigDecimal, StrToBCD('-1'), Result.VBigDecimal);
  end else
    Result := Value;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZExpFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZExpFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Exp(
    VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZLogFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLogFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Ln(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZLog10Function }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLog10Function.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Log10(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZCosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCosFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Cos(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZCotFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCotFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Cotan(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZSinFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSinFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Sin(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZTanFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTanFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Tan(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZAcosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAcosFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(ArcCos(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZAsinFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAsinFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(ArcSin(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZAtanFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAtanFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(ArcTan(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZCeilFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCeilFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeInteger(Ceil(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZFloorFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZFloorFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeInteger(Floor(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZRoundFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZRoundFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeInteger(Round(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZTruncFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTruncFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeInteger({$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZIntFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZIntFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Int(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZFracFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZFracFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Frac(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

{ TZSqrFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSqrFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Result := EncodeDouble(Sqrt(VariantManager.GetAsDouble(Stack.GetParameter(1))));
end;

procedure AddMathFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZEFunction.Create('E'));
  Functions.Add(TZPIFunction.Create('PI'));
  Functions.Add(TZRndFunction.Create('RND'));
  Functions.Add(TZAbsFunction.Create('ABS'));
  Functions.Add(TZExpFunction.Create('EXP'));
  Functions.Add(TZLogFunction.Create('LOG'));
  Functions.Add(TZLog10Function.Create('LOG10'));
  Functions.Add(TZCosFunction.Create('COS'));
  Functions.Add(TZSinFunction.Create('SIN'));
  Functions.Add(TZTanFunction.Create('TAN'));
  Functions.Add(TZCotFunction.Create('COT'));
  Functions.Add(TZAcosFunction.Create('ACOS'));
  Functions.Add(TZAsinFunction.Create('ASIN'));
  Functions.Add(TZAtanFunction.Create('ATAN'));
  Functions.Add(TZRoundFunction.Create('ROUND'));
  Functions.Add(TZCeilFunction.Create('CEIL'));
  Functions.Add(TZFloorFunction.Create('FLOOR'));
  Functions.Add(TZIntFunction.Create('INT'));
  Functions.Add(TZTruncFunction.Create('TRUNC'));
  Functions.Add(TZFracFunction.Create('FRAC'));
  Functions.Add(TZSqrFunction.Create('SQR'));
  Functions.Add(TZSqrFunction.Create('SQRT'));
end;

end.

