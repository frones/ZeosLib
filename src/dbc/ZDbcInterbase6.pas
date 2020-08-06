{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcInterbase6;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZPlainFirebirdInterbaseDriver, ZCompatibility, ZDbcUtils, ZDbcIntfs, ZDbcConnection,
  ZSysUtils, ZDbcLogging, ZDbcInterbase6Utils,
  ZClasses, ZDbcFirebirdInterbase;

type

  {** Implements Interbase6 Database Driver. }
  TZInterbase6Driver = class(TZInterbaseFirebirdDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
  end;

  IZIBTransaction = interface(IZInterbaseFirebirdTransaction)
    ['{FACB5CA2-4400-470E-A1DC-ECE29CDE4E6F}']
    function GetTrHandle: PISC_TR_HANDLE;
  end;

  {** Represents a Interbase specific connection interface. }
  IZInterbase6Connection = interface (IZInterbaseFirebirdConnection)
    ['{E870E4FE-21EB-4725-B5D8-38B8A2B12D0B}']
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetActiveTransaction: IZIBTransaction;
    function GetPlainDriver: TZInterbasePlainDriver;
    function GetDBIntegerInfo(isc_info: Byte; const Sender: IImmediatelyReleasable): Integer;
    function GetDBStringInfo(isc_info: Byte; const Sender: IImmediatelyReleasable): String;
  end;

  {** Implements Interbase6 Database Connection. }

  { TZInterbase6Connection }
  TZInterbase6Connection = class(TZInterbaseFirebirdConnection, IZConnection,
    IZInterbase6Connection, IZTransactionManager, IZInterbaseFirebirdConnection)
  private
    FHandle: TISC_DB_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FPlainDriver: TZInterbasePlainDriver;
    function ConstructConnectionString: String;
  protected
    procedure BeforeUrlAssign; override;
    procedure InternalCreate; override;
    procedure InternalClose; override;
  public
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure ExecuteImmediat(const SQL: RawByteString; ISC_TR_HANDLE: PISC_TR_HANDLE; LoggingCategory: TZLoggingCategory); overload;

  public
    function IsFirebirdLib: Boolean; override;
    function IsInterbaseLib: Boolean; override;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetActiveTransaction: IZIBTransaction;
    function GetPlainDriver: TZInterbasePlainDriver;
    function GetDBIntegerInfo(isc_info: Byte; const Sender: IImmediatelyReleasable): Integer;
    function GetDBStringInfo(isc_info: Byte; const Sender: IImmediatelyReleasable): String;
  public { IZTransactionManager }
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;

    function PingServer: Integer; override;
    function AbortOperation: Integer; override;

    procedure Open; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  {** EH: implements a IB/FB transaction }
  TZIBTransaction = class(TZInterbaseFirebirdTransaction,
    IZTransaction, IZIBTransaction, IZInterbaseFirebirdTransaction)
  private
    FTrHandle: TISC_TR_HANDLE;
    FTEB: TISC_TEB;
  protected
    function TxnIsStarted: Boolean; override;
    function TestCachedResultsAndForceFetchAll: Boolean; override;
  public { IZTransaction }
    procedure Commit;
    procedure Close;
    function GetConnection: IZConnection;
    function IsClosed: Boolean;
    procedure Rollback;
    function StartTransaction: Integer;
  public { IZIBTransaction }
    procedure DoStartTransaction;
    function GetTrHandle: PISC_TR_HANDLE;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); override;
  end;

var
  {** The common driver manager object. }
  Interbase6Driver: IZDriver;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses ZFastCode, ZEncoding, ZMessages,
  ZDbcInterbase6Statement, ZDbcInterbaseFirebirdMetadata, ZDbcProperties,
  Math
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES},System.Types{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZInterbase6Driver }

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
function TZInterbase6Driver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZInterbase6Connection.Create(Url);
end;

{**
  Constructs this object with default properties.
}
constructor TZInterbase6Driver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZInterbasePlainDriver.Create));
  {$IFDEF ZEOS_DISABLE_FIREBIRD}
  AddSupportedProtocol(AddPlainDriverToCache( TZFirebirdPlainDriver.Create));
  {$ENDIF}
end;

{ TZInterbase6Connection }

Function TZInterbase6Connection.AbortOperation: Integer;
Begin
 // https://github.com/FirebirdSQL/firebird/blob/master/doc/README.fb_cancel_operation
 if assigned(FPlainDriver.fb_cancel_operation) then begin
   Result := 0;
   {If  }FPlainDriver.fb_cancel_operation(@FStatusVector, @FHandle, fb_cancel_raise){ <> 0 Then
     commented by EH: if nothing was cancel, propably because FB is ready
     inbetween we receive an exception...
     HandleErrorOrWarning(lcOther, @FStatusVector, 'cancel operation', Self);}
 end else Result := 1 //abort opertion is not supported by the current client library
End;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZInterbase6Connection.InternalClose;
var Status: ISC_Status;
begin
  FLogMessage := 'DISCONNECT FROM "'+URL.DataBase+'"';
  try
    inherited InternalClose;
  finally
    if Assigned(DriverManager) and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    if FHandle <> 0 then begin
      Status := FPlainDriver.isc_detach_database(@FStatusVector, @FHandle);
      FHandle := 0;
      if Status <> 0 then
        HandleErrorOrWarning(lcDisconnect, @FStatusVector, FLogMessage, Self);
    end;
  end;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZInterbase6Connection.InternalCreate;
begin
  FPlainDriver := TZInterbasePlainDriver(PlainDriver.GetInstance);
  FHandle := 0;
  inherited InternalCreate;
  FXSQLDAMaxSize := 64*1024; //64KB by default
end;

function TZInterbase6Connection.GetPlainDriver: TZInterbasePlainDriver;
begin
  Result := FPlainDriver;
end;

procedure TZInterbase6Connection.ExecuteImmediat(const SQL: RawByteString;
  ISC_TR_HANDLE: PISC_TR_HANDLE; LoggingCategory: TZLoggingCategory);
var Status: ISC_STATUS;
begin
  if SQL = '' then
    Exit;
  Status := FPlainDriver.isc_dsql_execute_immediate(@FStatusVector, @FHandle,
      ISC_TR_HANDLE, Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF},
      Pointer(SQL), GetDialect, nil);
  {$IFDEF UNCICODE}
  if (Status <> 0) or (FStatusVector[2] = isc_arg_warning) or DriverManager.HasLoggingListener then
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ENDIF}
    if (Status <> 0) or (FStatusVector[2] = isc_arg_warning) then
      HandleErrorOrWarning(LoggingCategory, @FStatusVector, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self);
  DriverManager.LogMessage(LoggingCategory, URL.Protocol, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF});
end;

procedure TZInterbase6Connection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
begin
  if LoggingCategory <> lcExecute
  then ExecuteImmediat(SQL, GetActiveTransaction.GetTrHandle, LoggingCategory)
  else CreateStatement.ExecuteUpdate(SQL)
end;

{**
  Determines wether the client library is Firebird. Works for Firebird 1.5+
  Note that this Function cannot reliably determine wether you are on interbase.
  Use IsInterbaseLib for that.
}
function TZInterbase6Connection.IsFirebirdLib: Boolean;
begin
  if FClientVersion = -1 then DetermineClientTypeAndVersion;
  Result := FIsFirebirdLib;
end;

{**
  Determines wether the client library is Firebird. Works for Interbase 7.0+
  Note that this Function cannot reliably determine wether you are on interbase.
  Use IsInterbaseLib for that.
}
function TZInterbase6Connection.IsInterbaseLib: Boolean;
begin
  if FClientVersion = -1 then DetermineClientTypeAndVersion;
  Result := FIsInterbaseLib;
end;

{**
   Get database connection handle.
   @return database handle
}
function TZInterbase6Connection.GetDBHandle: PISC_DB_HANDLE;
begin
  Result := @FHandle;
end;

{**
   Return interbase server integer
   @param isc_info a ISC_INFO_XXX number
   @param sender the calling object
   @return ISC_INFO Integer
}
function TZInterbase6Connection.GetDBIntegerInfo(isc_info: Byte;
  const Sender: IImmediatelyReleasable): Integer;
begin
  if FPlainDriver.isc_database_info(@FStatusVector, @FHandle, 1, @isc_info,
      SizeOf(TByteBuffer), @FByteBuffer[0]) <> 0 then
    HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_database_info', Sender);
  { Buffer:
      0     - type of info
      1..2  - number length
      3..N  - number
      N+1   - #1 }
  if FByteBuffer[0] = isc_info
  then Result := ReadInterbase6Number(FPlainDriver, FByteBuffer[1])
  else Result := -1;
end;

{**
   Return interbase server string
   @param isc_info a ISC_INFO_XXX number
   @param sender the calling object
   @return ISC_INFO string
}
function TZInterbase6Connection.GetDBStringInfo(isc_info: Byte;
  const Sender: IImmediatelyReleasable): String;
begin
  if FPlainDriver.isc_database_info(@FStatusVector, @FHandle, 1, @isc_info,
      SizeOf(TByteBuffer), @FByteBuffer[0]) <> 0 then
    HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_database_info', Sender);

  { Buffer:
      0     - type of info
      1..2  - total data length
      3     - #1
      4     - string length
      5..N  - string
      N+1   - #1 }
  if FByteBuffer[0] = isc_info
  then Result := ConvertConnRawToString(ConSettings, @FByteBuffer[5], Integer(FByteBuffer[4]))
  else Result := '';
end;

{**
   Get Interbase transaction handle
   @return transaction handle
}
function TZInterbase6Connection.GetTrHandle: PISC_TR_HANDLE;
begin
  if not Closed
  then Result := GetActiveTransaction.GetTrHandle
  else Result := nil;
end;

procedure TZInterbase6Connection.BeforeUrlAssign;
begin
  inherited;
  FIsFirebirdLib := false;
  FIsInterbaseLib := false;
end;

{**
  Constructs the connection string for the current connection
}
function TZInterbase6Connection.ConstructConnectionString: String;
var
  Protocol: String;
  ConnectionString: String;
begin
  Protocol := LowerCase(Info.Values[ConnProps_FBProtocol]);

  if ((Protocol = 'inet') or (Protocol = 'wnet') or (Protocol = 'xnet') or (Protocol = 'local')) then begin
    if (GetClientVersion >= 3000000) and IsFirebirdLib then begin
      if protocol = 'inet' then begin
        if Port <> 0
        then ConnectionString := 'inet://' + HostName + ':' + ZFastCode.IntToStr(Port) + '/' + Database
        else ConnectionString := 'inet://' + HostName + '/' + Database;
      end else if Protocol = 'wnet' then begin
        if HostName <> ''
        then ConnectionString := 'wnet://' + HostName + '/' + Database
        else ConnectionString := 'wnet://' + Database
      end else if Protocol = 'xnet' then begin
        ConnectionString := 'xnet://' + Database;
      end else begin
        ConnectionString := Database;
      end;
    end else begin
      if protocol = 'inet' then begin
        if HostName = ''
        then ConnectionString := 'localhost'
        else ConnectionString := HostName;
        if Port <> 0 then begin
          ConnectionString := ConnectionString + '/' + ZFastCode.IntToStr(Port);
        end;
        ConnectionString := ConnectionString + ':';
        ConnectionString := ConnectionString + Database;
      end else if Protocol = 'wnet' then begin
        if HostName = ''
        then ConnectionString := '\\.'
        else ConnectionString := '\\' + HostName;
        if Port <> 0 then begin
          ConnectionString := ConnectionString + '@' + ZFastCode.IntToStr(Port);
        end;
        ConnectionString := ConnectionString + '\' + Database;
      end else begin
        ConnectionString := Database;
      end;
    end;
  end else begin
    if HostName <> '' then
      if Port <> 0 then
        ConnectionString := HostName + '/' + ZFastCode.IntToStr(Port) + ':' + Database
      else
        ConnectionString := HostName + ':' + Database
    else
      ConnectionString := Database;
  end;

  Result := ConnectionString;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZInterbase6Connection.Open;
var
  DPB: RawByteString;
  DBName: array[0..512] of AnsiChar;
  ConnectionString, CSNoneCP, DBCP, CreateDB: String;
  ti: IZIBTransaction;
  Statement: IZStatement;
  I: Integer;
  P, PEnd: PChar;
  TrHandle: TISC_TR_HANDLE;
  DBCreated: Boolean;
  procedure PrepareDPB;
  var
    R: RawByteString;
    P: PAnsiChar;
    L: LengthInt;
    CP: Word;
  begin
    if (Info.IndexOf('isc_dpb_utf8_filename') = -1)
    then CP := zOSCodePage
    else CP := zCP_UTF8;
    {$IFDEF UNICODE}
    R := ZUnicodeToRaw(ConnectionString, CP);
    {$ELSE}
    R := ZConvertStringToRawWithAutoEncode(ConnectionString, ConSettings^.CTRL_CP, CP);
    {$ENDIF}
    DPB := GenerateDPB(FPlainDriver, Info, ConSettings, CP);
    P := Pointer(R);
    L := Min(SizeOf(DBName)-1, Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
    if P <> nil then
      Move(P^, DBName[0], L);
    AnsiChar((PAnsiChar(@DBName[0])+L)^) := AnsiChar(#0);
  end;
label reconnect;
begin
  if not Closed then
    Exit;
  DBCP := '';
  if TransactIsolationLevel = tiReadUncommitted then
    raise EZSQLException.Create('Isolation level do not capable');
  if ConSettings^.ClientCodePage = nil then
    CheckCharEncoding(FClientCodePage, True);

  AssignISC_Parameters;
  ConnectionString := ConstructConnectionString;
  CSNoneCP := Info.Values[ConnProps_Charset_NONE_Alias];

  FHandle := 0;
  DBCreated := False;
  { Create new db if needed }
  if (Info.Values[ConnProps_CreateNewDatabase] <> '') then begin
    CreateDB := Info.Values[ConnProps_CreateNewDatabase];
    if (GetClientVersion >= 2005000) and IsFirebirdLib and (Length(CreateDB)<=4) and StrToBoolEx(CreateDB, False) then begin
      if (Info.Values[ConnProps_isc_dpb_lc_ctype] <> '') and (Info.Values[ConnProps_isc_dpb_set_db_charset] = '') then
        Info.Values[ConnProps_isc_dpb_set_db_charset] := Info.Values[ConnProps_isc_dpb_lc_ctype];
      DBCP := Info.Values[ConnProps_isc_dpb_set_db_charset];
      PrepareDPB;
      FLogMessage := 'CREATE DATABASE "'+URL.Database+'" AS USER "'+ URL.UserName+'"';
      if FPlainDriver.isc_create_database(@FStatusVector, SmallInt(StrLen(@DBName[0])),
          @DBName[0], @FHandle, Smallint(Length(DPB)),Pointer(DPB), 0) <> 0 then
        Self.HandleErrorOrWarning(lcOther, @FStatusVector, FLogMessage, Self);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    end else begin
      {$IFDEF UNICODE}
      DPB := ZUnicodeToRaw(CreateDB, zOSCodePage);
      {$ELSE}
      DPB := CreateDB;
      {$ENDIF}
      CreateDB := UpperCase(CreateDB);
      I := PosEx('CHARACTER', CreateDB);
      if I > 0 then begin
        I := PosEx('SET', CreateDB, I);
        P := Pointer(CreateDB);
        Inc(I, 3); Inc(P, I); Inc(I);
        While P^ = ' ' do begin
          Inc(I); Inc(P);
        end;
        PEnd := P;
        While ((Ord(PEnd^) >= Ord('A')) and (Ord(PEnd^) <= Ord('Z'))) or
              ((Ord(PEnd^) >= Ord('0')) and (Ord(PEnd^) <= Ord('9'))) do
          Inc(PEnd);
        DBCP :=  Copy(CreateDB, I, (PEnd-P));
      end else DBCP := sCS_NONE;
      if FPlainDriver.isc_dsql_execute_immediate(@FStatusVector, @FHandle, @TrHandle,
          Length(DPB), Pointer(DPB), FDialect, nil) <> 0 then
        HandleErrorOrWarning(lcOther, @FStatusVector, CreateDB, Self);
      { Logging connection action }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, URL.Protocol, CreateDB);
      //we did create the db and are connected now.
      //we have no dpb so we connect with 'NONE' which is not a problem for the UTF8/NONE charsets
      //because the metainformations are retrieved in UTF8 encoding
      if (DBCP <> FClientCodePage) or ((DBCP = sCS_NONE) and (FClientCodePage <> '') and
         ((FClientCodePage <> 'UTF8') and (FClientCodePage <> sCS_NONE))) then begin
        FLogMessage := 'DISCONNECT FROM "'+URL.DataBase+'"';
        //we need a reconnect with a valid dpb
        if FPlainDriver.isc_detach_database(@FStatusVector, @FHandle) <> 0 then
          HandleErrorOrWarning(lcDisconnect, @FStatusVector, FLogMessage, Self);
        TrHandle := 0;
        FHandle := 0;
      end;
    end;
    Info.Values[ConnProps_CreateNewDatabase] := '';
    DBCreated := True;
  end;
reconnect:
  if FHandle = 0 then begin
    PrepareDPB;
    FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);
    { Connect to Interbase6 database. }
    if FPlainDriver.isc_attach_database(@FStatusVector,
        ZFastCode.StrLen(@DBName[0]), @DBName[0], @FHandle, Length(DPB), Pointer(DPB)) <> 0 then
      HandleErrorOrWarning(lcConnect, @FStatusVector, FLogMessage, Self);

    { Dialect could have changed by isc_dpb_set_db_SQL_dialect command }
    I := GetDBIntegerInfo(isc_info_db_SQL_Dialect, Self);
    if I = -1
    then FDialect := SQL_DIALECT_V5
    else FDialect := Word(I);
    { Logging connection action }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  end;

  inherited SetAutoCommit(AutoCommit or (Info.IndexOf(TxnProps_isc_tpb_autocommit) <> -1));
  FRestartTransaction := not AutoCommit;

  FHardCommit := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
  if (DBCP <> '') and not DBCreated then
    Exit;
  inherited Open;

  with GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo do
  begin
    CollectServerInformations; //keep this one first!
    FHostVersion := GetHostVersion;
    FXSQLDAMaxSize := GetMaxSQLDASize;
  end;

  {Check for ClientCodePage: if empty switch to database-defaults
    and/or check for charset 'NONE' which has a different byte-width
    and no convertions where done except the collumns using collations}
  if not DBCreated then begin
    Statement := CreateStatementWithParams(nil);
    try
      with Statement.ExecuteQuery('SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE') do begin
        if Next then DBCP := GetString(FirstDbcIndex);
        Close;
      end;
    finally
      Statement := nil;
    end;
    ti := GetActiveTransaction;
    try
      ti.Close;
    finally
      ti := nil;
    end;
  end;
  if DBCP = 'NONE' then begin { SPECIAL CASE CHARCTERSET "NONE":
    EH: the server makes !NO! charset conversion if CS_NONE.
    Attaching a CS "NONE" db with a characterset <> CS_NONE has this effect:
    All field codepages are retrieved as the given client-characterset.
    This works nice as long the fields have it's own charset definition.
    But what's the encoding of the CS_NONE fields? The more what about CLOB encoding?

    If we're attaching the db with CS "NONE" all userdefined field CP's are
    returned gracefully. Zeos can convert everything to your Controls-CP.
    Except the CP_NONE fields. And the text lob's where encoding is unknown too.
    For the Unicode-IDE's this case is a nightmare. Jan's suggestion is to use
    the fields as Byte/BlobFields only. My idea is to use such fields with the
    CPWIN1252 Charset which maps each byte to words and vice versa.
    So no information is lost and the data is still readable "somehow".

    Side-note: see: https://firebirdsql.org/rlsnotesh/str-charsets.html
    Any DDL/DML in non ASCII7 range will give a maleformed string if encoding is
    different to UTF8/UNICODE_FSS because the RDB$-Tables have a
    UNICODE_FSS collation}

    {test if charset is not given or is CS_NONE }
    if CSNoneCP = ''
    then CSNoneCP := FClientCodePage
    else if FCLientCodePage <> ''
      then CSNoneCP := FCLientCodePage
      else CSNoneCP := 'WIN1252'; {WIN1252 would be optimal propably, each byte is shifted to a word..}
    ResetCurrentClientCodePage(CSNoneCP, False);
    ConSettings^.ClientCodePage^.ID := 0;
    //Now notify our metadata object all fields are retrieved in utf8 encoding
    (FMetadata as TZInterbase6DatabaseMetadata).SetUTF8CodePageInfo;
    if (FCLientCodePage <> DBCP) then begin
      Info.Values[ConnProps_isc_dpb_lc_ctype] := DBCP;
      InternalClose;
      goto reconnect; //build new TDB and reopen in CS_NONE mode
    end;
  end else if FClientCodePage = '' then
    CheckCharEncoding(DBCP);
end;

{**
  release all handles immeditaely on connection loss
  @param Sender the caller where the connection loss did happen first
    also to be used as comparsion with other IImmediatelyReleasable objects
    to avoid circular calls
}
procedure TZInterbase6Connection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var ImmediatelyReleasable: IImmediatelyReleasable;
begin
  FHandle := 0;
  if (fActiveTransaction <> nil) and
     (fActiveTransaction.QueryInterface(IImmediatelyReleasable, ImmediatelyReleasable) = S_OK) and
      (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
  while fTransactions.Count > 0 do begin
    fTransactions[0].QueryInterface(IImmediatelyReleasable, ImmediatelyReleasable);
    if ImmediatelyReleasable <> Sender then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
  end;
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Checks if a connection is still alive by doing a call to isc_database_info
  It does not matter what info we request, we are not looking at it, as long
  as it is something which should _always_ work if the connection is there.
  We check if the error returned is one of the net_* errors described in the
  firebird client documentation (isc_network_error .. isc_net_write_err).
  Returns 0 if the connection is OK
  Returns non zero if the connection is not OK
}
function TZInterbase6Connection.PingServer: integer;
var
  DatabaseInfoCommand: Char;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
  ErrorCode: ISC_STATUS;
begin
  DatabaseInfoCommand := Char(isc_info_reads);
  ErrorCode := FPlainDriver.isc_database_info(@FStatusVector, @FHandle, 1, @DatabaseInfoCommand,
                           IBLocalBufferLength, @Buffer[0]);
  case ErrorCode of
    isc_network_error..isc_net_write_err:
      Result := -1
    else
      Result := 0;
  end;
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

  @param Name a procedure or function identifier
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZInterbase6Connection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
    Open;
  Result := TZInterbase6CallableStatement.Create(Self, Name, Info);
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
function TZInterbase6Connection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZInterbase6PreparedStatement.Create(Self, SQL, Info);
end;

function TZInterbase6Connection.GetActiveTransaction: IZIBTransaction;
var TA: IZTransaction;
begin
  if FHandle <> 0 then begin
    if fActiveTransaction = nil then begin
      TA := CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction);
    end;
    fActiveTransaction.QueryInterface(IZIBTransaction, Result);
  end else
    Result := nil;
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
function TZInterbase6Connection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZInterbase6Statement.Create(Self, Info);
end;

function TZInterbase6Connection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
begin
  if Params = nil then
    Params := Info;
  Result := TZIBTransaction.Create(Self, AutoCommit, ReadOnly, TransactIsolationLevel, Params);
  fTransactions.Add(Result);
end;

{ TZIBTransaction }

procedure TZIBTransaction.Close;
var Status: ISC_STATUS;
begin
  if FTrHandle > 0 then with TZInterbase6Connection(FOwner) do begin
    if fDoCommit
    then Status := FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle)
    else Status := FPlainDriver.isc_rollback_transaction(@FStatusVector, @FTrHandle);
    FTrHandle := 0;
    fSavepoints.Clear;
    if Status <> 0 then
      FOwner.HandleErrorOrWarning(lcTransaction, @FStatusVector, sCommitMsg, Self);
    FOwner.ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZIBTransaction.Commit;
var Status: ISC_STATUS;
  S: RawByteString;
begin
  with TZInterbase6Connection(FOwner) do
    if fSavepoints.Count > 0 then begin
      S := 'RELEASE SAVEPOINT '+ {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
      FSavePoints.Delete(FSavePoints.Count-1);
    end else if FTrHandle <> 0 then try
      if FHardCommit or
        ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
        ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
      then Status := FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle)
      else begin
        fDoCommit := True;
        fDoLog := False;
        Status := FPlainDriver.isc_commit_retaining(@FStatusVector, @FTrHandle);
        ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
      end;
      if Status <> 0 then
        FOwner.HandleErrorOrWarning(lcTransaction, @FStatusVector, sCommitMsg,
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    finally
      if fDoLog and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
    end;
end;

procedure TZIBTransaction.DoStartTransaction;
begin
  GetTrHandle;
end;

function TZIBTransaction.GetConnection: IZConnection;
begin
  Result := FOwner as TZInterbase6Connection;
  FOwner.SetActiveTransaction(Self);
end;

function TZIBTransaction.GetTrHandle: PISC_TR_HANDLE;
begin
  if FTrHandle = 0 then
    StartTransaction;
  Result := @FTrHandle
end;

function TZIBTransaction.IsClosed: Boolean;
begin
  Result := FTrHandle = 0;
end;

procedure TZIBTransaction.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  FTrHandle := 0;
  inherited;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZIBTransaction.Rollback;
var Status: ISC_STATUS;
  S: RawByteString;
begin
  with TZInterbase6Connection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'ROLLBACK TO SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTrHandle <> 0 then try
    if FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then Status := FPlainDriver.isc_rollback_transaction(@FStatusVector, @FTrHandle)
    else begin
      fDoCommit := False;
      fDoLog := False;
      Status := FPlainDriver.isc_rollback_retaining(@FStatusVector, @FTrHandle);
      ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
    end;
    if Status <> 0 then
      FOwner.HandleErrorOrWarning(lcTransaction, @FStatusVector, sRollbackMsg, Self);
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
  end;
end;

{**
  Starts transaction support or saves the current transaction.
  If the connection is closed, the connection will be opened.
  If a transaction is underway a nested transaction or a savepoint will be
  spawned. While the tranaction(s) is/are underway the AutoCommit property is
  set to False. Ending up the transaction with a commit/rollback the autocommit
  property will be restored if changing the autocommit mode was triggered by a
  starttransaction call.
  @return the current txn-level. 1 means a transaction was started.
  2 means the transaction was saved. 3 means the previous savepoint got saved
  too and so on
}
function TZIBTransaction.StartTransaction: Integer;
var S: String;
begin
  with TZInterbase6Connection(FOwner) do begin
    if FTrHandle = 0 then begin
      if FTPB = EmptyRaw then begin
        FTPB := FOwner.GenerateTPB(FAutoCommit, FReadOnly, FTransactionIsolation, FProperties);
        fTEB.tpb_length := Length(FTPB){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
        fTEB.tpb_address := Pointer(FTPB);
      end;
      Result := Ord(not Self.FAutoCommit);
      fTEB.db_handle := @FHandle;
      if FPlainDriver.isc_start_multiple(@FStatusVector, @FTrHandle, 1, @fTEB) <> 0 then
        FOwner.HandleErrorOrWarning(lcTransaction, @FStatusVector, sStartTxn, Self);
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sStartTxn);
    end else begin
      Result := FSavePoints.Count+2;
      S := 'SP'+ZFastcode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result);
      ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
      Result := FSavePoints.Add(S)+2;
    end;
  end;
end;

function TZIBTransaction.TestCachedResultsAndForceFetchAll: Boolean;
var I, RowNo: Integer;
  P: Pointer;
begin
  Result := False;
  for I := 0 to FOpenCursors.Count -1 do
    if IZResultSet(FOpenCursors[i]).GetType = rtForwardOnly then
      Exit;
  Result := True;
  while FOpenCursors.Count > 0 do begin
    P := FOpenCursors[FOpenCursors.Count-1];
    RowNo := IZResultSet(P).GetRow;
    IZResultSet(P).Last; //now the pointer will be removed from the open cursor list
    IZResultSet(P).MoveAbsolute(RowNo); //restore current position
  end;
end;

function TZIBTransaction.TxnIsStarted: Boolean;
begin
  Result := FTrHandle <> 0
end;

initialization
  Interbase6Driver := TZInterbase6Driver.Create;
  DriverManager.RegisterDriver(Interbase6Driver);

finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(Interbase6Driver);
  Interbase6Driver := nil;
{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
