{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASA;

interface

{$I ZDbc.inc}

uses
  ZCompatibility, Classes, Contnrs, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainASADriver, ZTokenizer, ZDbcGenericResolver, ZURL,
  ZPlainDriver, ZGenericSqlAnalyser, ZPlainASAConstants;

type
  {** Implements a ASA Database Driver. }
  {$WARNINGS OFF}
  TZASADriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;
  {$WARNINGS ON}

  {** Represents a ASA specific connection interface. }
  IZASAConnection = interface (IZConnection)
    ['{FAAAFCE0-F550-4098-96C6-580145813EBF}']
    function GetDBHandle: PZASASQLCA;
    function GetPlainDriver: IZASAPlainDriver;
//    procedure CreateNewDatabase(SQL: String);
  end;

  {** Implements ASA Database Connection. }
  TZASAConnection = class(TZAbstractConnection, IZASAConnection)
  private
    FSQLCA: TZASASQLCA;
    FHandle: PZASASQLCA;
  private
    procedure StartTransaction; virtual;
  protected
    procedure InternalCreate; override;
  public
    destructor Destroy; override;

    function GetDBHandle: PZASASQLCA;
    function GetPlainDriver: IZASAPlainDriver;
//    procedure CreateNewDatabase(SQL: String);

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure Commit; override;
    procedure Rollback; override;
    procedure SetOption(Temporary: Integer; User: PAnsiChar; const Option: string;
      const Value: string);

    procedure Open; override;
    procedure Close; override;
  end;

  {** Implements a specialized cached resolver for ASA. }
  TZASACachedResolver = class(TZGenericCachedResolver)
  public
    function FormCalculateStatement(Columns: TObjectList): string; override;
  end;


var
  {** The common driver manager object. }
  ASADriver: IZDriver;

implementation

uses
  ZDbcASAMetadata, ZDbcASAStatement, ZDbcASAUtils, ZSybaseToken,
  ZSybaseAnalyser, ZDbcLogging;

{ TZASADriver }

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
function TZASADriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZASAConnection.Create(Url);
end;
{$WARNINGS ON}

{**
  Constructs this object with default properties.
}
constructor TZASADriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZASA7PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZASA8PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZASA9PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZASA12PlainDriver.Create));
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZASADriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZASADriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZASADriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZSybaseTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZASADriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZSybaseStatementAnalyser.Create;
  Result := Analyser;
end;

{ TZASAConnection }

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZASAConnection.Close;
begin
  if Closed or (not Assigned(PlainDriver))then
     Exit;

  if AutoCommit then
    Commit
  else
    Rollback;

  GetPlainDriver.db_string_disconnect( FHandle, nil);
  CheckASAError( GetPlainDriver, FHandle, lcDisconnect);

  FHandle := nil;
  if GetPlainDriver.db_fini( @FSQLCA) = 0 then
  begin
    DriverManager.LogError( lcConnect, PlainDriver.GetProtocol, 'Inititalizing SQLCA',
      0, 'Error closing SQLCA');
    raise EZSQLException.CreateWithCode( 0,
      'Error closing SQLCA');
  end;

  DriverManager.LogMessage(lcDisconnect, PlainDriver.GetProtocol,
      Format('DISCONNECT FROM "%s"', [Database]));

  inherited Close;
end;

{**
   Commit current transaction
}
procedure TZASAConnection.Commit;
begin
  if Closed or AutoCommit then
     Exit;

  if FHandle <> nil then
  begin
    GetPlainDriver.db_commit( FHandle, 0);
    CheckASAError( GetPlainDriver, FHandle, lcTransaction);
    DriverManager.LogMessage(lcTransaction,
      PlainDriver.GetProtocol, 'TRANSACTION COMMIT');
  end;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZASAConnection.InternalCreate;
begin
  Self.FMetadata := TZASADatabaseMetadata.Create(Self, URL);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZASAConnection.CreateCallableStatement(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZASACallableStatement.Create(Self, SQL, Info);
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
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZASAConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZASAPreparedStatement.Create(Self, SQL, Info);
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
function TZASAConnection.CreateRegularStatement(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZASAStatement.Create(Self, Info);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZASAConnection.Destroy;
begin
  if not Closed then
    Close;

  inherited;
end;

{**
   Get database connection handle.
   @return database handle
}
function TZASAConnection.GetDBHandle: PZASASQLCA;
begin
  Result := FHandle;
end;

{**
   Return native interbase plain driver
   @return plain driver
}
function TZASAConnection.GetPlainDriver: IZASAPlainDriver;
begin
  Result := PlainDriver as IZASAPlainDriver;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZASAConnection.Open;
var
  ConnectionString, Links: string;
begin
  if not Closed then
     Exit;

  FHandle := nil;
  ConnectionString := '';
  try
    if GetPlainDriver.db_init( @FSQLCA) = 0 then
    begin
      DriverManager.LogError( lcConnect, PlainDriver.GetProtocol, 'Inititalizing SQLCA',
        0, 'Error initializing SQLCA');
      raise EZSQLException.CreateWithCode( 0,
        'Error initializing SQLCA');
    end;
    FHandle := @FSQLCA;

    if HostName <> '' then
      ConnectionString := ConnectionString + 'ENG="' + HostName + '"; ';
    if User <> '' then
      ConnectionString := ConnectionString + 'UID="' + User + '"; ';
    if Password <> '' then
      ConnectionString := ConnectionString + 'PWD="' + Password + '"; ';
    if Database <> '' then
    begin
      if CompareText( ExtractFileExt( Database), '.db') = 0 then
        ConnectionString := ConnectionString + 'DBF="' + Database + '"; '
      else
        ConnectionString := ConnectionString + 'DBN="' + Database + '"; ';
    end;

    Links := '';
    if Info.Values['CommLinks'] <> ''
      then Links := 'CommLinks=' + Info.Values['CommLinks'];
    if Info.Values['LINKS'] <> ''
      then Links := 'LINKS=' + Info.Values['LINKS'];
    if (Links = '') and (Port <> 0)
      then Links := 'LINKS=tcpip(PORT=' + IntToStr(Port) + ')';
    if Links <> ''
      then ConnectionString := ConnectionString + Links + '; ';

    GetPlainDriver.db_string_connect(FHandle, PAnsiChar(AnsiString(ConnectionString)));
    CheckASAError( GetPlainDriver, FHandle, lcConnect);

    DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol,
      Format('CONNECT TO "%s" AS USER "%s"', [Database, User]));

    if ( FClientCodePage <> '' ) then
      if ( GetPlainDriver.db_change_char_charset(FHandle, PAnsiChar(AnsiString(FClientCodePage))) = 0 ) or
         ( GetPlainDriver.db_change_nchar_charset(FHandle, PAnsiChar(AnsiString(FClientCodePage))) = 0 ) then
        CheckASAError( GetPlainDriver, FHandle, lcOther, 'Set client CharacterSet failed.');

    StartTransaction;

    //SetConnOptions     RowCount;

  except
    on E: Exception do
    begin
      if Assigned( FHandle) then
        GetPlainDriver.db_fini( FHandle);
      FHandle := nil;
      raise;
    end;
  end;

  inherited Open;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZASAConnection.Rollback;
begin
  if Closed or AutoCommit then
     Exit;

  if Assigned( FHandle) then
  begin
    GetPlainDriver.db_rollback( FHandle, 0);
    CheckASAError( GetPlainDriver, FHandle, lcTransaction);
    DriverManager.LogMessage(lcTransaction,
      PlainDriver.GetProtocol, 'TRANSACTION ROLLBACK');
  end;
end;

procedure TZASAConnection.SetOption(Temporary: Integer; User: PAnsiChar;
  const Option: string; const Value: string);
var
  SQLDA: PASASQLDA;
  Sz: Integer;
  S: string;
begin
  if Assigned( FHandle) then
  begin
    Sz := SizeOf( TASASQLDA) - 32767 * SizeOf( TZASASQLVAR);
    SQLDA := AllocMem( Sz);
    try
      StrPLCopy( SQLDA.sqldaid, 'SQLDA   ', 8);
      SQLDA.sqldabc := Sz;
      SQLDA.sqln := 1;
      SQLDA.sqld := 1;
      SQLDA.sqlVar[0].sqlType := DT_STRING;
      SQLDA.sqlVar[0].sqlLen := Length( Value)+1;
      SQLDA.sqlVar[0].sqlData := PAnsiChar(AnsiString(Value));
      GetPlainDriver.db_setoption(FHandle, Temporary, User, PAnsiChar(AnsiString(Option)), SQLDA);

      CheckASAError( GetPlainDriver, FHandle, lcOther);
      S := String(User);
      DriverManager.LogMessage( lcOther, PlainDriver.GetProtocol,
        Format( 'SET OPTION %s.%s = %s', [ S, Option, Value]));
    finally
      FreeMem( SQLDA);
    end;
  end;
end;

{**
   Start transaction
}
procedure TZASAConnection.StartTransaction;
var
  ASATL: integer;
begin
  if AutoCommit then
    SetOption( 1, nil, 'CHAINED', 'OFF')
  else
    SetOption( 1, nil, 'CHAINED', 'ON');
  ASATL := Ord( TransactIsolationLevel);
  if ASATL > 1 then
    ASATL := ASATL - 1;
  SetOption( 1, nil, 'ISOLATION_LEVEL', IntToStr( ASATL));
end;

{ TZASACachedResolver }

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZASACachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
var
  I: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  if Columns.Count = 0 then
     Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    Current := TZResolverParameter(Columns[I]);
    if Result <> '' then
      Result := Result + ',';
    if Current.DefaultValue <> '' then
      Result := Result + Current.DefaultValue
    else
      Result := Result + 'NULL';
  end;
  Result := 'SELECT ' + Result;
end;

initialization
  ASADriver := TZASADriver.Create;
  DriverManager.RegisterDriver(ASADriver);

finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(ASADriver);
  ASADriver := nil;
end.
