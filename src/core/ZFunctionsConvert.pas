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

unit ZFunctionsConvert;

interface

{$I ZCore.inc}

uses
  SysUtils, ZFunctions, ZExpression, ZVariant;

{**  Conversion functions }

type

{**  Str <> Float}
  {** Implements a VAL function. }
  TZValFunction = class (TZAbstractFunction, IZFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{**  Str <> Date}
  {** Implements a CTOD function. }
  TZCtodFunction = class (TZAbstractFunction, IZFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  {** Implements a DTOS function. }
  TZDtosFunction = class (TZAbstractFunction, IZFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  {** Implements a DTOS function. }
  TZFormatDateTimeFunction = class (TZAbstractFunction, IZFunction)
  public
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

procedure AddConvertFunctions(Functions : TZFunctionsList);

implementation

var
  InternalDefaultFormatSettings : TFormatSettings;

 { TZValFunction }

function TZValFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsDouble(Result, StrToFloatDef(Stack.GetParameter(1).{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 0, InternalDefaultFormatSettings));
end;

{ TZCtodFunction }

function TZCtodFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsDateTime(Result, StrToDateDef(Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 0));
end;

{ TZDtosFunction }

function TZDtosFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, FormatDateTime('yyyymmdd', Value.VDateTime));
end;

{ TZFormatDateTimeFunction }

function TZFormatDateTimeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsString(Result, FormatDateTime(Stack.GetParameter(2).{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, Stack.GetParameter(1).VDateTime));
end;

procedure AddConvertFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZValFunction.Create('VAL'));
  Functions.Add(TZDtosFunction.Create('DTOS'));
  Functions.Add(TZCtodFunction.Create('CTOD'));
  Functions.Add(TZFormatDateTimeFunction.Create('FORMATDATETIME'));
end;

initialization
  InternalDefaultFormatSettings.ThousandSeparator   := ',';
  InternalDefaultFormatSettings.DecimalSeparator    := '.';
end.

