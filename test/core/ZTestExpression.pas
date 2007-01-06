{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Expression Classes             }
{                                                         }
{          Originally written by Sergey Seroukhov         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestExpression;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  TestFramework, ZTestDefinitions, SysUtils, Classes, ZExpression, ZVariables;

type

  {** Implements a test case for Utilities. }
  TZTestExpressionCase = class(TZCoreGenericTestCase)
  published
    procedure TestRegularExpressions;
    procedure TestArrays;
    procedure TestFunctions;
    procedure TestVariables;
    procedure TestPerformance;
  end;

implementation

uses ZVariant, ZCompatibility;

const
  RUN_COUNT = 100000;

{ TZTestExpressionCase }

{**
  Runs a test for arrays processing.
}
procedure TZTestExpressionCase.TestArrays;
var
  Expression: IZExpression;
begin
  Expression := TZExpression.Create;
(*
  Variant[] values1 = new Variant[10];
  Integer[] values2 = new Integer[10];
  for (int i = 0; i < 10; i++) {
    values1[i] = expression.createVariant();
    values1[i].setInt(i + 1);
    values2[i] = new Integer(i + 1);
  }
  Variant n = expression.createVariant();
  Variant a = expression.createVariant();
  expression.setExpression("IN(N, A)");

  n.setInt(3);
  expression.getDefaultVariables().set("N", n);
  a.setArray(values1);
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  n.setInt(-100);
  expression.getDefaultVariables().set("N", n);
  assertTrue(!expression.evaluate().getBoolean());

  n.setInt(3);
  expression.getDefaultVariables().set("N", n);
  a.setObjectArray(values2);
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  n.setInt(-100);
  expression.getDefaultVariables().set("N", n);
  assertTrue(!expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(A, 3)");
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(A, -100)");
  expression.getDefaultVariables().set("A", a);
  assertTrue(!expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(0.0, '0')");
  assertTrue(expression.evaluate().getBoolean());
*)
end;

{**
  Runs a test for regular (simple) expressions.
}
procedure TZTestExpressionCase.TestRegularExpressions;
var
  Expression: IZExpression;
begin
  Expression := TZExpression.Create;

  Expression.Expression := '2+2';
  CheckEquals(4, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(4, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '3-1';
  CheckEquals(2, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(2, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '2+2*2+2';
  CheckEquals(8, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(8, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '(2+2)*(2+2)';
  CheckEquals(16, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(16, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '''ABBA'' LIKE ''A*B?''';
  CheckEquals(True, DefVarManager.GetAsBoolean(Expression.Evaluate));

  Expression.Expression := '''ABC'' + 123';
  CheckEquals('ABC123', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := '''ABC'' + (100 + 23)';
  CheckEquals('ABC123', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := '''Result='' + (23.5 + 11 / 23 * 2^2 - 3) + True';
  CheckEquals('Result=20.5TRUE', DefVarManager.GetAsString(Expression.Evaluate));
end;

{**
  Runs a test for build-in functions.
}
procedure TZTestExpressionCase.TestFunctions;
var
  Expression: IZExpression;
begin
  Expression := TZExpression.Create;

  Expression.Expression := 'Sum(''A'',''B'',3)';
  CheckEquals('AB3', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'Sum(''A'',''B'',''3'') + (28 + 2)';
  CheckEquals('AB330', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'Sum(''A'',''B'',''3'') + (Max(10, -33, 28) + Min(2, 100, 5))';
  CheckEquals('AB330', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'TIME()';
  Check(DefVarManager.GetAsDateTime(Expression.Evaluate) <> 0);
end;

{**
  Runs a test for variables.
}
procedure TZTestExpressionCase.TestVariables;
var
  Expression: IZExpression;
  Variables: IZVariablesList;
  Temp: TZVariant;
begin
  Expression := TZExpression.Create;

  DefVarManager.SetAsInteger(Temp, 100);
  Expression.DefaultVariables.Add('a', Temp);
  DefVarManager.SetAsInteger(Temp, 23);
  Expression.DefaultVariables.Add('B C', Temp);
  Expression.Expression := 'A + "B C"';
  CheckEquals(123, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '"B C" LIKE ''2*''';
  Check(DefVarManager.GetAsBoolean(Expression.Evaluate));

  Expression.Expression := '"B C" NOT LIKE ''2*''';
  CheckEquals(False, DefVarManager.GetAsBoolean(Expression.Evaluate));

  DefVarManager.SetAsInteger(Temp, 123);
  Expression.DefaultVariables.Add('A_B', Temp);
  Expression.Expression := 'A_B + 321';
  CheckEquals(444, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Clear;
  Expression.AutoVariables := True;
  Expression.Expression := 'A = 123';
  CheckEquals(0, Expression.DefaultVariables.FindByName('a'));
  DefVarManager.SetAsInteger(Temp, 123);
  Expression.DefaultVariables.Values[0] := Temp;
  Check(DefVarManager.GetAsBoolean(Expression.Evaluate));

  try
    Expression.AutoVariables := False;
    Expression.Clear;
    Expression.Expression := 'B + 321';
    Expression.Evaluate;
    Fail('Wrong behaviour with unknown variable.');
  except
  end;

  Expression.Expression := 'A + B';
  Variables := TZVariablesList.Create;
  DefVarManager.SetAsInteger(Temp, 123);
  Variables.Add('a', Temp);
  DefVarManager.SetAsString(Temp, '321');
  Variables.Add('B', Temp);
  CheckEquals(444, DefVarManager.GetAsInteger(Expression.Evaluate2(Variables)));

  Expression.Expression := 'A=1 OR A=2';
  Variables := TZVariablesList.Create;
  DefVarManager.SetAsInteger(Temp, 3);
  Variables.Add('a', Temp);
  CheckEquals(False, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));

  Variables := TZVariablesList.Create;
  DefVarManager.SetNull(Temp);
  Variables.Add('a', Temp);

  Expression.Expression := 'A IS NULL';
  CheckEquals(True, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));

  Expression.Expression := 'A IS NOT NULL';
  CheckEquals(False, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));

  Expression.Expression := 'A Is Null Or a Not Like ''AB*''';
  CheckEquals(True, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));

  Expression.Expression := 'A Is Not Null or a Like ''C?''';
  CheckEquals(False, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));

  Expression.Expression := 'Upper(''Abc'')';
  CheckEquals('ABC', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'Lower(''Abc'')';
  CheckEquals('abc', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'Concat(''Ab'', ''cd'', ''efG'')';
  CheckEquals('AbcdefG', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'SubStr(''AbcdefG'', 3, 3)';
  CheckEquals('cde', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := 'StrPos(''AbcdefG'', ''cde'')';
  CheckEquals(3, DefVarManager.GetAsInteger(Expression.Evaluate));

  Variables := TZVariablesList.Create;
  DefVarManager.SetAsString(Temp, '');
  Variables.Add('a', Temp);
  Expression.Expression := 'A = ''''';
  CheckEquals(True, DefVarManager.GetAsBoolean(Expression.Evaluate2(Variables)));
end;

{**
  Run tests for expression performance.
}
procedure TZTestExpressionCase.TestPerformance;
var
  I: Integer;
  StartTicks: Cardinal;
  Expression: IZExpression;
  Stack: TZExecutionStack;
  Temp: TZVariant;
  OldDecimalSeparator: Char;
begin
  { Tests speed of repeatable calculations with variables. }
  Expression := TZExpression.CreateWithExpression('A-100>100');
  Stack := TZExecutionStack.Create;
  try
    StartTicks := GetTickCount;
    for I := 0 to RUN_COUNT - 1 do
    begin
      DefVarManager.SetAsInteger(Temp, I);
      Expression.DefaultVariables.Values[0] := Temp;
      Expression.Evaluate4(Expression.DefaultVariables,
        Expression.DefaultFunctions, Stack);
    end;
    PrintLn(Format('Evaluating expression, Time: %d',
      [GetTickCount - StartTicks]));
  finally
    Stack.Free;
  end;

  { Tests comparison of float values. }
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := ',';
  Expression := TZExpression.CreateWithExpression('A=123.4567890123456');
  Stack := TZExecutionStack.Create;
  try
    DefVarManager.SetAsFloat(Temp, 123.4567890123456);
    Expression.DefaultVariables.Values[0] := Temp;
    Temp := Expression.Evaluate4(Expression.DefaultVariables,
      Expression.DefaultFunctions, Stack);
    CheckEquals(Ord(vtBoolean), Ord(Temp.VType));
    CheckEquals(True, Temp.VBoolean);
  finally
    DecimalSeparator := OldDecimalSeparator;
    Stack.Free;
  end;

  { Tests alternative comparison of float values. }
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := ',';
  Expression := TZExpression.CreateWithExpression(
    'Abs(A - 123.4567890123456)<0.001');
  Stack := TZExecutionStack.Create;
  try
    DefVarManager.SetAsFloat(Temp, 123.4567890123456);
    Expression.DefaultVariables.Values[0] := Temp;
    Temp := Expression.Evaluate4(Expression.DefaultVariables,
      Expression.DefaultFunctions, Stack);
    CheckEquals(Ord(vtBoolean), Ord(Temp.VType));
    CheckEquals(True, Temp.VBoolean);
  finally
    DecimalSeparator := OldDecimalSeparator;
    Stack.Free;
  end;
end;

initialization
  TestFramework.RegisterTest(TZTestExpressionCase.Suite);
end.
