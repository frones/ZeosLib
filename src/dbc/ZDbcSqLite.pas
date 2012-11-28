{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcSqLite;

interface

{$I ZDbc.inc}

uses
  Types, ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainSqLiteDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser, ZURL,
  ZPlainDriver;

type

  {** Implements SQLite Database Driver. }
  {$WARNINGS OFF}
  TZSQLiteDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;
  {$WARNINGS ON}

  {** Represents a SQLite specific connection interface. }
  IZSQLiteConnection = interface (IZConnection)
    ['{A4B797A9-7CF7-4DE9-A5BB-693DD32D07D2}']
    function UseOldBlobEncoding: Boolean;
    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;
  end;

  {** Implements SQLite Database Connection. }
  TZSQLiteConnection = class(TZAbstractConnection, IZSQLiteConnection)
  private
    FCatalog: string;
    FHandle: Psqlite;
    function UseOldBlobEncoding: Boolean;
  protected
    procedure InternalCreate; override;
    procedure StartTransactionSupport;

  public
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;

    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;

    function ReKey(const Key: string): Integer;
    function Key(const Key: string): Integer;
    function GetBinaryEscapeString(const Value: ZAnsiString): String; override;
  end;

var
  {** The common driver manager object. }
  SQLiteDriver: IZDriver;

implementation

uses
  ZSysUtils, ZDbcUtils, ZDbcSqLiteStatement, ZSqLiteToken,
  ZDbcSqLiteUtils, ZDbcSqLiteMetadata, ZSqLiteAnalyser;

{ TZSQLiteDriver }

{**
  Constructs this object with default properties.
}
constructor TZSQLiteDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create, 'sqlite'));
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
{$WARNINGS OFF}
function TZSQLiteDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZSQLiteConnection.Create(Url);
end;
{$WARNINGS ON}

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZSQLiteDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZSQLiteDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZSQLiteDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZSQLiteTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZSQLiteDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZSQLiteStatementAnalyser.Create;
  Result := Analyser;
end;

{ TZSQLiteConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZSQLiteConnection.InternalCreate;
begin
  FMetadata := TZSQLiteDatabaseMetadata.Create(Self, Url);
  AutoCommit := True;
  TransactIsolationLevel := tiNone;
  CheckCharEncoding('UTF-8');
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLiteConnection.Destroy;
begin
  inherited Destroy;
end;

function TZSQLiteConnection.UseOldBlobEncoding: Boolean;
begin
  Result := Url.Properties.Values['OldBlobEncoding'] = 'True';
end;

{**
  Set encryption key for a database
  @param Key the key used to encrypt your database.
  @return error code from SQLite Key function.
}
function TZSQLiteConnection.Key(const Key: string):Integer;
var
  ErrorCode: Integer;
begin
  {$IFDEF DELPHI12_UP}
  ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(UTF8String(Key)), StrLen(PAnsiChar(UTF8String(Key))));
  {$ELSE}
  ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(Key), StrLen(PAnsiChar(Key)));
  {$ENDIF}
  Result := ErrorCode;
end;

{**
  Reencrypt a database with a new key. The old/current key needs to be
  set before calling this function.
  @param Key the new key used to encrypt your database.
  @return error code from SQLite ReKey function.
}
function TZSQLiteConnection.ReKey(const Key: string):Integer;
var
  ErrorCode: Integer;
begin
  {$IFDEF DELPHI12_UP}
  ErrorCode := GetPlainDriver.ReKey(FHandle, PAnsiChar(UTF8String(Key)), StrLen(PAnsiChar(UTF8String(Key))));
  {$ELSE}
  ErrorCode := GetPlainDriver.ReKey(FHandle, PAnsiChar(Key), StrLen(PAnsiChar(Key)));
  {$ENDIF}
  Result := ErrorCode;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZSQLiteConnection.Open;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  LogMessage: string;
  SQL: AnsiString;
  Timeout_ms: Integer;
begin
  if not Closed then
    Exit;
  ErrorMessage := '';

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

{$IFDEF DELPHI12_UP}
  FHandle := GetPlainDriver.Open(PAnsiChar(AnsiString(UTF8Encode(Database))), 0, ErrorMessage);
{$ELSE}
  FHandle := GetPlainDriver.Open(PAnsiChar(Database), 0, ErrorMessage);
{$ENDIF}

  if FHandle = nil then
  begin
    CheckSQLiteError(GetPlainDriver, SQLITE_ERROR, ErrorMessage,
      lcConnect, LogMessage);
  end;
  DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol, LogMessage);

  { Turn on encryption if requested }
  if StrToBoolEx(Info.Values['encrypted']) then
  begin
    {$IFDEF DELPHI12_UP}
    ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(UTF8String(Password)), StrLen(PAnsiChar(UTF8String(Password))));
    {$ELSE}
    ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(Password), StrLen(PAnsiChar(Password)));
    {$ENDIF}
    CheckSQLiteError(GetPlainDriver, ErrorCode, nil, lcConnect, 'SQLite.Key');
  end;

  { Set busy timeout if requested } 
  Timeout_ms := StrToIntDef(Info.Values['busytimeout'], -1); 
  if Timeout_ms >= 0 then 
  begin 
    GetPlainDriver.BusyTimeout(FHandle, Timeout_ms);
  end; 

  try
    if ( FClientCodePage <> '' ) then
    begin
        SQL := 'PRAGMA encoding = '''+AnsiString(FClientCodePage)+'''';
        ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(SQL),
          nil, nil, ErrorMessage);
        CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, String(SQL));
    end;

    SQL := 'PRAGMA show_datatypes = ON';
    ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(SQL),
      nil, nil, ErrorMessage);
    CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, String(SQL));

    StartTransactionSupport;
  except
    GetPlainDriver.Close(FHandle);
    FHandle := nil;
    raise;
  end;

  inherited Open;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @param Info a statement parameters.
  @return a new Statement object
}
function TZSQLiteConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZSQLiteStatement.Create(GetPlainDriver, Self, Info, FHandle);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZSQLiteConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZSQLitePreparedStatement.Create(GetPlainDriver, Self, SQL,
    Info, FHandle);
end;

{**
  Starts a transaction support.
}
procedure TZSQLiteConnection.StartTransactionSupport;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQL: String;
begin
  if TransactIsolationLevel <> tiNone then
  begin
    ErrorMessage := '';
    SQL := 'BEGIN TRANSACTION';
    ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(AnsiString(SQL)), nil, nil,
      ErrorMessage);
    CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, SQL);
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Commit;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQL: PAnsiChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    ErrorMessage := '';
    SQL := 'COMMIT TRANSACTION';
    ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(SQL), nil, nil,
      ErrorMessage);
    CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, String(SQL));
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, String(SQL));

    StartTransactionSupport;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Rollback;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQL: String;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    ErrorMessage := '';
    SQL := 'ROLLBACK TRANSACTION';
    ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(AnsiString(SQL)), nil, nil,
      ErrorMessage);
    CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, SQL);

    StartTransactionSupport;
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZSQLiteConnection.Close;
var
  LogMessage: string;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;

  GetPlainDriver.Close(FHandle);
  FHandle := nil;
  LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
  if Assigned(DriverManager) then
    DriverManager.LogMessage(lcDisconnect, PlainDriver.GetProtocol, LogMessage);
  inherited Close;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZSQLiteConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

function TZSQLiteConnection.GetClientVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(GetPlainDriver.LibVersion);
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZSQLiteConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZSQLiteConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQL: String;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    ErrorMessage := '';
    SQL := 'ROLLBACK TRANSACTION';
    ErrorCode := GetPlainDriver.Execute(FHandle, PAnsiChar(AnsiString(SQL)), nil, nil,
      ErrorMessage);
    CheckSQLiteError(GetPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, SQL);
  end;

  inherited SetTransactionIsolation(Level);

  if not Closed then
    StartTransactionSupport;
end;

{**
  Gets a reference to SQLite connection handle.
  @return a reference to SQLite connection handle.
}
function TZSQLiteConnection.GetConnectionHandle: Psqlite;
begin
  Result := FHandle;
end;

{**
  Gets a SQLite plain driver interface.
  @return a SQLite plain driver interface.
}
function TZSQLiteConnection.GetPlainDriver: IZSQLitePlainDriver;
begin
  Result := PlainDriver as IZSQLitePlainDriver;
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result := BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZSQLiteConnection.GetBinaryEscapeString(const Value: ZAnsiString): String;
begin
  if GetAutoEncodeStrings then
    Result := GetDriver.GetTokenizer.AnsiGetEscapeString(ZDbcSqLiteUtils.EncodeString(Value))
  else
    Result := String(ZDbcSqLiteUtils.EncodeString(Value));
end;

function TZSQLiteConnection.GetHostVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(GetPlainDriver.LibVersion);
end;

initialization
  SQLiteDriver := TZSQLiteDriver.Create;
  DriverManager.RegisterDriver(SQLiteDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(SQLiteDriver);
  SQLiteDriver := nil;
end.

