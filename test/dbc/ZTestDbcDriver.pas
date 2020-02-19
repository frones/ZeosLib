{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for driver manager, plain driver            }
{   and driver classes                                    }
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

unit ZTestDbcDriver;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZDbcIntfs, ZSqlTestCase, ZPlainDriver, ZDbcConnection;

type
  TZFooPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver)
  public
    function GetProtocol: string; override;
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
    function GetDescription: string; override;
  end;

  TZFooDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
  end;

  TZTestDriver = class(TZAbstractDbcSQLTestCase)
  published
    procedure TestPlainDriver;
  end;

implementation

{ TZFooPlainDriver }

function TZFooPlainDriver.GetProtocol: string;
begin
  Result := 'FOO';
end;

function TZFooPlainDriver.Clone: IZPlainDriver;
begin
  Result := Self;
end;

procedure TZFooPlainDriver.LoadCodePages;
begin
end;

function TZFooPlainDriver.GetDescription: string;
begin
  Result := '';
end;

{ TZFooDriver }

constructor TZFooDriver.Create;
begin
  inherited;
  AddSupportedProtocol(AddPlainDriverToCache(TZFooPlainDriver.Create));
end;

{ TZTestDriver }

procedure TZTestDriver.TestPlainDriver;
var
  FooDriver: IZDriver;
  Url: TZURL;
const
  SUrl = 'zdbc:FOO://foo';
begin
  // set up
  Url := TZURL.Create(SUrl);
  Url.LibLocation := 'foo.dll';
  FooDriver := TZFooDriver.Create;
  DriverManager.RegisterDriver(FooDriver);

  // tests
  try
    CheckNotNull(DriverManager.GetDriver(SUrl), 'GetDriver case-sensitive');
    CheckNotNull(DriverManager.GetDriver(LowerCase(SUrl)), 'GetDriver case-insensitive');
    CheckNotNull(FooDriver.GetPlainDriver(Url), 'GetPlainDriver case-sensitive');
    Url.Protocol := LowerCase(Url.Protocol);
    CheckNotNull(FooDriver.GetPlainDriver(Url), 'GetPlainDriver case-insensitive');
  finally
    // tear down
    FreeAndNil(Url);
    DriverManager.DeregisterDriver(FooDriver);
  end;
end;

initialization
  RegisterTest('dbc', TZTestDriver.Suite);
end.

