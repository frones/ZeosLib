{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for Interbase Database Connectivity Classes   }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZTestDbcODBc;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC}
uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZSqlTestCase;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcODBCCase = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestFetchSequenceValue;
  end;

{$ENDIF ZEOS_DISABLE_ODBC}
implementation
{$IFNDEF ZEOS_DISABLE_ODBC}

{ TZTestDbcODBCCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcODBCCase.GetSupportedProtocols: string;
begin
  Result := 'odbc_w,odbc_a';
end;

procedure TZTestDbcODBCCase.TestFetchSequenceValue;
var PStatement: IZPreparedStatement;
begin
  if Connection.GetServerProvider in [spMSSQL, spASE] then begin
    PStatement := Connection.PrepareStatement('SELECT current_value FROM sys.sequences WHERE name = ''generate_id''');
    try
      with PStatement.ExecuteQueryPrepared do
      begin
        Next;
        CheckEquals(90000250, GetLong(FirstDbcIndex), 'current_value');
        Close;
      end;
    finally
      PStatement.Close;
    end;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcODBCCase.Suite);
{$ENDIF ZEOS_DISABLE_ODBC}

end.
