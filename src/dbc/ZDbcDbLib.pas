{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               DBLib Connectivity Classes                }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLib;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ZSysUtils, ZClasses,
  ZDbcConnection, ZDbcIntfs, ZCompatibility, ZDbcLogging, ZPlainDbLibDriver,
  ZTokenizer, ZGenericSqlAnalyser, ZPlainDriver;

type
  TDBLibProvider = (dpMsSQL, dpSybase);

  {** Implements DBLib Database Driver. }
  TZDBLibDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a DBLib specific connection interface. }
  IZDBLibConnection = interface (IZConnection)
    ['{6B0662A2-FF2A-4415-B6B0-AAC047EA0671}']

    function GetProvider: TDBLibProvider;
    function GetConnectionHandle: PDBPROCESS;
    function GetServerAnsiCodePage: Word;
    procedure CheckDBLibError(LogCategory: TZLoggingCategory;
      const LogMessage: SQLString; const Sender: IImmediatelyReleasable);
    function GetByteBufferAddress: PByteBuffer;
    function GetPlainDriver: TZDBLIBPLainDriver;
  end;

  {** Implements a generic DBLib Connection. }
  TZDBLibConnection = class(TZAbstractSuccedaneousTxnConnection, IZConnection,
    IZDBLibConnection, IZTransaction)
  private
    FSQLErrors: TZDBLibErrorList;
    FSQLMessages: TZDBLibMessageList;
    FProvider: TDBLibProvider;
    FServerAnsiCodePage: Word;
    FPlainDriver: TZDBLIBPLainDriver;
    {$IFDEF TEST_CALLBACK}
    FDBLibErrorHandler: IZDBLibErrorHandler;
    FDBLibMessageHandler: IZDBLibMessageHandler;
    {$ENDIF TEST_CALLBACK}
    FLastWarning: EZSQLWarning;
    function GetProvider: TDBLibProvider;
    procedure InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
    procedure DetermineMSDateFormat;
    function DetermineMSServerCollation: String;
    function DetermineMSServerCodePage(const Collation: String): Word;
    {$IFDEF TEST_CALLBACK}
    function DBMSGHANDLE(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
      Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer;
    function DBERRHANDLE(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
      DbErrStr, OsErrStr: PAnsiChar): Integer;
    {$ENDIF TEST_CALLBACK}
  protected
    FHandle: PDBPROCESS;
    procedure InternalCreate; override;
    procedure InternalLogin;
    function GetConnectionHandle: PDBPROCESS;
    procedure CheckDBLibError(LogCategory: TZLoggingCategory;
      const LogMessage: SQLString; const Sender: IImmediatelyReleasable);
    function GetServerCollation: String;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
  public
    destructor Destroy; override;
  public
    function AbortOperation: Integer; override;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;

    procedure Commit;
    procedure Rollback;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function StartTransaction: Integer;

    procedure Open; override;

    procedure SetReadOnly(Value: Boolean); override;

    procedure SetCatalog(const Value: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;

  public
    function GetServerAnsiCodePage: Word;
    function GetPlainDriver: TZDBLIBPLainDriver;
    function GetServerProvider: TZServerProvider; override;
  end;

var
  {** The common driver manager object. }
  DBLibDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} ZDbcProperties,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZMessages, ZDbcUtils, ZDbcDbLibStatement, ZEncoding, ZFastCode,
  ZDbcDbLibMetadata, ZSybaseToken, ZSybaseAnalyser;

var
  DBLIBCriticalSection: TCriticalSection;

  { TZDBLibDriver }

{**
  Constructs this object with default properties.
}
constructor TZDBLibDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TMSSQLDBLibPLainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TSybaseDBLibPLainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
}
function TZDBLibDriver.Connect(const Url: TZURL): IZConnection;
var iPlainDriver: IZPlainDriver;
    dblibPlainDriver: TZDBLIBPLainDriver;
begin
  DBLIBCriticalSection.Enter;
  Result := nil;
  iPlainDriver := nil;
  try
    iPlainDriver := GetPlainDriver(URL);
    dblibPlainDriver := iPlainDriver.GetInstance as TZDBLIBPLainDriver;
    if dblibPlainDriver.DBLibraryVendorType = lvtMS then
      raise EZSQLException.Create('Old NTWDBLIB.DLL library isn''t supported anymore. Use FreeTDS instead.');
    Result := TZDBLibConnection.Create(Url);
  finally
    iPlainDriver := nil;
    DBLIBCriticalSection.Release;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZDBLibDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZDBLibDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZDBLibDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZSybaseTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZDBLibDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZSybaseStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZDBLibConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZDBLibConnection.InternalCreate;
begin
  FPlainDriver := TZDBLIBPLainDriver(PlainDriver.GetInstance);
  FSQLErrors := TZDBLibErrorList.Create;
  FSQLMessages := TZDBLibMessageList.Create;
  if ZFastCode.Pos('mssql', LowerCase(Url.Protocol)) > 0  then begin
    FMetadata := TZMsSqlDatabaseMetadata.Create(Self, Url);
    FProvider := dpMsSQL;
  end else if ZFastCode.Pos('sybase', LowerCase(Url.Protocol)) > 0 then begin
    FMetadata := TZSybaseDatabaseMetadata.Create(Self, Url);
    FProvider := dpSybase;
  end else
    FMetadata := nil;
  if (FPlainDriver.DBLibraryVendorType = lvtFreeTDS) and (Info.Values[ConnProps_CodePage] = '') then
    {$IF defined(UNICODE) or defined(LCL)}
    Info.Values[ConnProps_CodePage] := 'UTF-8';
    {$ELSE}
    Info.Values[ConnProps_CodePage] := 'ISO-8859-1'; //this is the default CP of free-tds
    {$IFEND}
end;

{**
  Login procedure can be overriden for special settings.
}
procedure TZDBLibConnection.InternalLogin;
var
  Loginrec: PLOGINREC;
  RawTemp: RawByteString;
  lLogFile  : String;
  TDSVersion: DBINT;
  Ret: RETCODE;
  P: PChar;
  E: EZSQLException;
begin
  FLogMessage := 'CONNECT TO "'+HostName+'"';
  LoginRec := FPlainDriver.dbLogin;
  try
    if FPLainDriver.DBLibraryVendorType <> lvtSybase then begin
      //sybase does not support dbsetlversion nor dbsetlname() for TDSversion..
      //they have a dbsetversion which is a one time parameter per dll-load

      //FreeDTS SetVersion just sets a global variable which is deprecated,
      //but they are able to set the tdsversion per login
      //the ms lib always returns DBFAIL with dbsetlname(),  why?
      lLogFile := URL.Properties.Values[ConnProps_TDSVersion];
      TDSVersion := StrToIntDef(lLogFile, TDSDBVERSION_UNKNOWN);
      if TDSVersion = TDSDBVERSION_UNKNOWN then begin
        lLogFile := URL.Properties.Values[ConnProps_TDSVersion];
        if (lLogFile <> '') and (FplainDriver.DBLibraryVendorType <> lvtMS) then begin
          P := Pointer(lLogFile);
          if P^ = Char('5') then
            TDSVersion := TDSDBVERSION_100
          else if P^ = Char('4') then
            TDSVersion := TDSDBVERSION_42
          else if P^ = Char('7') then begin
            Inc(P, 1+Ord(Length(lLogFile) = 3));
            TDSVersion := DBVERSION_70 + (Ord(P^)-Ord('0'));
            if (TDSVersion < TDSDBVERSION_UNKNOWN) or (TDSVersion > DBVERSION_73) then
              TDSVersion := TDSDBVERSION_UNKNOWN;
          end;
        end;
        if TDSVersion = TDSDBVERSION_UNKNOWN then
          //no tds version given.
          //just set the latest versions (sybase10+ / SQLServer 2005+)
          //all others are old!
          case FplainDriver.DBLibraryVendorType of
            lvtMS:      TDSVersion := TDSDBVERSION_42;
            lvtSybase:  TDSVersion := TDSDBVERSION_100;
            lvtFreeTDS: if FProvider = dpMsSQL
                        then TDSVersion := DBVERSION_72
                        else TDSVersion := TDSDBVERSION_100;
          end;
      end;
      if TDSVersion <> TDSDBVERSION_UNKNOWN then begin
        if FPLainDriver.DBLibraryVendorType = lvtFreeTDS
        then Ret := FPlainDriver.dbsetversion(TDSVersion)// and FPlainDriver.dbsetlversion(LoginRec, TDSVersion)
        else Ret := FPlainDriver.dbsetlname(LoginRec, nil, TDSVersion);
        Assert(Ret = DBSUCCEED, 'failed to set the TDS version');
      end;
    end;
//Common parameters
    RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Info.Values[ConnProps_Workstation], ConSettings.CTRL_CP, ZOSCodePage);
    if Pointer(RawTemp) <> nil then
      FPlainDriver.dbSetLHost(LoginRec, Pointer(RawTemp));

    RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Info.Values[ConnProps_AppName], ConSettings.CTRL_CP, ZOSCodePage);
    if Pointer(RawTemp) <> nil then
      FPlainDriver.dbSetLApp(LoginRec, Pointer(RawTemp));

    RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Info.Values[ConnProps_Language], ConSettings.CTRL_CP, ZOSCodePage);
    if Pointer(RawTemp) <> nil then
      FPlainDriver.dbSetLNatLang(LoginRec, Pointer(RawTemp));

    if Info.Values[ConnProps_Timeout] <> '' then
      FPlainDriver.dbSetLoginTime(StrToIntDef(Info.Values[ConnProps_Timeout], 60));

    if (FPlainDriver.DBLibraryVendorType = lvtFreeTDS) then begin
      if StrToBoolEx(Info.Values[ConnProps_Log]) or StrToBoolEx(Info.Values[ConnProps_Logging]) or
         StrToBoolEx(Info.Values[ConnProps_TDSDump]) then begin
        lLogFile := Info.Values[ConnProps_LogFile];
        if lLogFile = '' then
          lLogFile := Info.Values[ConnProps_Log_File];
        if lLogFile = '' then
          lLogFile := Info.Values[ConnProps_TDSDumpFile];
        if lLogFile = '' then
          lLogFile := ChangeFileExt(ParamStr(0), '.tdslog');
        RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(lLogFile, ConSettings.CTRL_CP, ZOSCodePage);
        if RawTemp <> '' then
          FPlainDriver.tdsDump_Open(Pointer(RawTemp));
      end;
    end;


    if ( FProvider = dpMsSQL ) and ( StrToBoolEx(Info.Values[ConnProps_NTAuth]) or StrToBoolEx(Info.Values[ConnProps_Trusted])
      or StrToBoolEx(Info.Values[ConnProps_Secure]) ) and (FPlainDriver.DBLibraryVendorType <> lvtFreeTDS) then
    begin
      FPlainDriver.dbsetlsecure(LoginRec);
      FLogMessage := Format(SConnect2WinAuth,  [URL.HostName]);
    end else begin
      {$IFDEF UNICODE}
      RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(URL.UserName, ConSettings.CTRL_CP, ZOSCodePage);
      {$ELSE}
      RawTemp := URL.UserName;
      {$ENDIF}
      FPlainDriver.dbsetluser(LoginRec, PAnsiChar(RawTemp));
      {$IFDEF UNICODE}
      RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Password, ConSettings.CTRL_CP, ZOSCodePage);
      {$ELSE}
      RawTemp := Password;
      {$ENDIF}
      FPlainDriver.dbsetlpwd(LoginRec, PAnsiChar(RawTemp));
      FLogMessage := Format(SConnect2AsUser,  [URL.HostName, URL.UserName]);
    end;
    if (FPlainDriver.DBLibraryVendorType = lvtFreeTDS) or (FProvider = dpSybase) then begin
      RawTemp := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_CodePage]);
      if Pointer(RawTemp) <> nil then begin
        FPlainDriver.dbSetLCharSet(LoginRec, PAnsiChar(RawTemp));
        CheckCharEncoding(Info.Values[ConnProps_CodePage]);
      end;
    end;
    CheckDBLibError(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    RawTemp := ConSettings^.ConvFuncs.ZStringToRaw(HostName, ConSettings.CTRL_CP, ZOSCodePage);
    // add port number if FreeTDS is used, the port number was specified and no server instance name was given:
    if (FPlainDriver.DBLibraryVendorType = lvtFreeTDS) and (Port <> 0) and (ZFastCode.Pos('\', HostName) = 0)  then
      RawTemp := RawTemp + ':' + ZFastCode.IntToRaw(Port);
    FHandle := FPlainDriver.dbOpen(LoginRec, PAnsiChar(RawTemp));
    CheckDBLibError(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    if not Assigned(FHandle) then begin
      if FLastWarning <> nil then begin
        E := EZSQLException.CreateClone(FLastWarning);
        FreeAndNil(FLastWarning);
      end else E := EZSQLException.Create('The connection to the server failed, no '+
        'proper handle was returned. Insufficient memory, unable to connect for any reason. ');
      raise E;
    end;
    {$IFDEF TEST_CALLBACK}
    FDBLibErrorHandler := FPlainDriver.GetErrorHandler(FHandle, DBERRHANDLE);
    FDBLibMessageHandler := FPlainDriver.GetMessageHandler(FHandle, DBMSGHANDLE);
    {$ENDIF TEST_CALLBACK}
    DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  finally
    FPlainDriver.dbLoginFree(LoginRec);
  end;
end;

function TZDBLibConnection.GetConnectionHandle: PDBPROCESS;
begin
  if FProvider = dpMsSQL then
    if FPlainDriver.dbDead(FHandle) <> 0 then
    begin
      Closed := True;
      Open;
    end;
  Result := FHandle;
end;

function TZDBLibConnection.GetPlainDriver: TZDBLIBPLainDriver;
begin
  Result := FPlainDriver
end;

function TZDBLibConnection.GetProvider: TDBLibProvider;
begin
  Result := FProvider;
end;

function TZDBLibConnection.AbortOperation: Integer;
begin
 // http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.help.ocs_12.5.1.dblib/html/dblib/X57019.htm
 If FPlainDriver.dbcancel(FHandle) = DBSUCCEED Then Result := 0
   Else Result := 1;
end;

procedure TZDBLibConnection.CheckDBLibError(LogCategory: TZLoggingCategory;
  const LogMessage: SQLString; const Sender: IImmediatelyReleasable);
var I: Integer;
  rException, rWarningOrInfo: RawByteString;
  FirstError: Integer;
  FormatStr: String;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
  SQLWriter: TZRawSQLStringWriter;
  {$IFDEF UNICODE}msgCP: Word;{$ENDIF}
  procedure IntToBuf(Value: Integer; var Buf: RawByteString);
  var C: Cardinal;
    Digits: Byte;
    Negative: Boolean;
    IntBuf: Array[0..4] of AnsiChar;
  begin
    Digits := GetOrdinalDigits(Value, C, Negative);
    if Negative then
      SQLWriter.AddChar(AnsiChar('-'), rException);
    PCardinal(@IntBuf[0])^ := Cardinal(808464432);//'0000'
    IntToRaw(C, @IntBuf[5-Digits], Digits);
    SQLWriter.AddText(PAnsiChar(@IntBuf[0]), 5, Buf);
  end;
begin
  {$IFNDEF TEST_CALLBACK}
  FPlainDriver.AssignErrorMessages(FHandle, FSQLErrors, FSQLMessages);
  {$ENDIF}
  if (FSQLErrors.Count = 0) and (FSQLMessages.Count = 0) then
    Exit;
  rException := EmptyRaw;
  rWarningOrInfo := EmptyRaw;
  FirstError := 0;
  SQLWriter := TZRawSQLStringWriter.Create(1024);
  try
    for I := 0 to FSQLErrors.Count -1 do begin
      lErrorEntry := PDBLibError(FSQLErrors[I]);
      if (lErrorEntry <> nil) then begin
        if lErrorEntry^.DbErr > EXINFO then begin
          if FirstError = 0 then
            FirstError := lErrorEntry^.DbErr;
          SQLWriter.AddLineFeedIfNotEmpty(rException);
          SQLWriter.AddText('DBError : [', rException);
          IntToBuf(lErrorEntry^.DbErr, rException);
          SQLWriter.AddText('] : ', rException);
          SQLWriter.AddText(lErrorEntry^.DbErrStr, rException);
        end;
        if lErrorEntry^.OsErr > EXINFO then begin
          if FirstError = 0 then
            FirstError := lErrorEntry^.OsErr;
          SQLWriter.AddLineFeedIfNotEmpty(rException);
          SQLWriter.AddText('OSError : [', rException);
          IntToBuf(lErrorEntry^.OsErr, rException);
          SQLWriter.AddText('] : ', rException);
          SQLWriter.AddText(lErrorEntry^.OsErrStr, rException);
        end;
        Dispose(lErrorEntry);
      end;
    end;
    SQLWriter.Finalize(rException);
    FSQLErrors.Count := 0; //clear the list
    for I := 0 to FSQLMessages.Count -1 do begin
      lMesageEntry := PDBLibMessage(FSQLMessages[I]);
      if (lMesageEntry <> nil) then begin
        if lMesageEntry^.MsgNo <> 5701 then begin
          if FirstError = 0 then
            FirstError := lMesageEntry^.MsgNo;
          SQLWriter.AddLineFeedIfNotEmpty(rWarningOrInfo);
          SQLWriter.AddText('MsgNo : [', rWarningOrInfo);
          IntToBuf(lMesageEntry^.MsgNo, rWarningOrInfo);
          SQLWriter.AddText('] : ', rWarningOrInfo);
          SQLWriter.AddText(lMesageEntry^.MsgText, rWarningOrInfo);
        end;
        Dispose(lMesageEntry);
      end;
    end;
    FSQLMessages.Count := 0; //clear the list
    SQLWriter.Finalize(rWarningOrInfo);
  finally
    FreeAndNil(SQLWriter);
  end;
  {$IFDEF UNICODE}
  if ConSettings.ClientCodePage <> nil
  then msgCP := ConSettings.ClientCodePage.CP
  else msgCP := ZOSCodePage;
  {$ENDIF UNICODE}
  if LogMessage <> '' then
    if LogCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;
  if rException <> EmptyRaw then begin
    {$IFDEF UNICODE}
    FLogMessage := ZRawToUnicode(rException, msgCP);
    {$ELSE}
    FLogMessage := rException;
    {$ENDIF}
    LogError(LogCategory, FirstError, Self, logMessage, FLogMessage);
    if DriverManager.HasLoggingListener then
      DriverManager.LogError(LogCategory, URL.Protocol, LogMessage, 0, FLogMessage);
    if LogMessage <> ''
    then FLogMessage := Format(FormatStr, [FLogMessage, FirstError, LogMessage])
    else FLogMessage := Format(FormatStr, [FLogMessage, FirstError]);
    if fHandle <> nil then
      FPlainDriver.dbcancel(fHandle);
    raise EZSQLException.Create(FLogMessage);
  end else if (rWarningOrInfo <> EmptyRaw) then begin
    {$IFDEF UNICODE}
    FLogMessage := ZRawToUnicode(rWarningOrInfo, msgCP);
    {$ELSE}
    FLogMessage := rWarningOrInfo;
    {$ENDIF}
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LogCategory, URL.Protocol, FLogMessage);
    ClearWarnings;
    if LogMessage <> ''
    then FLogMessage := Format(FormatStr, [FLogMessage, FirstError, LogMessage])
    else FLogMessage := Format(FormatStr, [FLogMessage, FirstError]);
    FLogMessage := '';
    FLastWarning := EZSQLWarning.CreateWithCode(FirstError, FLogMessage);
  end;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZDBLibConnection.ClearWarnings;
begin
  FreeAndNil(FLastWarning);
end;

{**
  Opens a connection to database server with specified parameters.
}
const textlimit: PAnsichar = '2147483647';
procedure TZDBLibConnection.Open;
{$IFDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF}
begin
   if not Closed then
      Exit;

  InternalLogin;
  FLogMessage := 'USE '+ URL.Database;
  {$IFDEF UNICODE}
  Tmp := ZUnicodeToRaw(URL.Database, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if FPlainDriver.dbUse(FHandle, Pointer({$IFDEF UNICODE}Tmp{$ELSE}URL.Database{$ENDIF})) <> DBSUCCEED then
    CheckDBLibError(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);

  FLogMessage := 'set textlimit=2147483647';
  if FPlainDriver.dbsetopt(FHandle, FPlainDriver.GetDBOption(dboptTEXTLIMIT),Pointer(textlimit), -1) <> DBSUCCEED then
    CheckDBLibError(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  if FPlainDriver.dbsetopt(FHandle, FPlainDriver.GetDBOption(dboptTEXTSIZE),Pointer(textlimit), -1) <> DBSUCCEED then
    CheckDBLibError(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  ExecuteImmediat(RawByteString('set quoted_identifier on'), lcOther);

  inherited Open;

  if TransactIsolationLevel = tiNone then
    TransactIsolationLevel := tiReadCommitted; //use default til
  if TransactIsolationLevel <> tiReadCommitted then
    InternalSetTransactionIsolation(GetTransactionIsolation);

  if not AutoCommit then begin
    AutoCommit := True;
    SetAutoCommit(False);
  end;

  (GetMetadata.GetDatabaseInfo as IZDbLibDatabaseInfo).InitIdentifierCase(GetServerCollation);
  if (FProvider = dpMsSQL) and (FPlainDriver.DBLibraryVendorType <> lvtFreeTDS) then
  begin
  {note: this is a hack from a user-request of synopse project!
    Purpose is to notify Zeos all Character columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char(UTF8) encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions, reports!}
    FDisposeCodePage := True;
    ConSettings^.ClientCodePage := New(PZCodePage);
    ConSettings^.ClientCodePage^.CP := ZOSCodePage; //need a tempory CP for the SQL preparation
    ConSettings^.ClientCodePage^.Encoding := ceAnsi;
    ConSettings^.ClientCodePage^.Name := DetermineMSServerCollation;
    FServerAnsiCodePage := DetermineMSServerCodePage(ConSettings^.ClientCodePage^.Name);
    if UpperCase(Info.Values[DSProps_ResetCodePage]) = 'UTF8' then
    begin
      ConSettings^.ClientCodePage^.CP := zCP_UTF8;
      ConSettings^.ClientCodePage^.Encoding := ceUTF8;
      ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := True;
    end else begin
      ConSettings^.ClientCodePage^.CP := FServerAnsiCodePage;
      ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := False;
    end;
    ConSettings^.AutoEncode := True; //Must be set because we can't determine a column-codepage! e.g NCHAR vs. CHAR Fields
    SetConvertFunctions(ConSettings);
  end else begin
    if (FProvider = dpSybase) and (FPlainDriver.DBLibraryVendorType <> lvtFreeTDS)
    then ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := False;
    FServerAnsiCodePage := ConSettings^.ClientCodePage^.CP;
    ConSettings^.ReadFormatSettings.DateFormat := 'yyyy/mm/dd';
    ConSettings^.ReadFormatSettings.DateTimeFormat := ConSettings^.ReadFormatSettings.DateFormat+' '+ConSettings^.ReadFormatSettings.TimeFormat;
  end;
  { EH:
  http://technet.microsoft.com/en-us/library/ms180878%28v=sql.105%29.aspx
   Using DATE and DATETIME in ISO 8601 format is multi-language supported:
   DATE Un-separated
   DATETIME as YYYY-MM-DDTHH:NN:SS }
  if (FProvider = dpMsSQL) then
    DetermineMSDateFormat;
  ConSettings^.WriteFormatSettings.DateFormat := 'YYYYMMDD';
  ConSettings^.WriteFormatSettings.DateTimeFormat := 'YYYY-MM-DDTHH:NN:SS';
  SetDateTimeFormatProperties(False);
  if Info.Values[ConnProps_AnsiPadding] <> '' then
    if StrToBoolEx(Info.Values[ConnProps_AnsiPadding]) then
      ExecuteImmediat(RawByteString('SET ANSI_PADDING ON'), lcOther)
    else begin
      ExecuteImmediat(RawByteString('SET ANSI_DEFAULTS OFF'), lcOther);
      ExecuteImmediat(RawByteString('SET ANSI_PADDING OFF'), lcOther);
    end;
  //ExecuteImmediat(RawByteString('SET NO_BROWSETABLE ON'), lcOther)
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
function TZDBLibConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibCallableStatement.Create(Self, Name, Info);
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
function TZDBLibConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibPreparedStatementEmulated.Create(Self, SQL, Info);
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
function TZDBLibConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibStatement.Create(Self, Info);
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZDBLibConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      ExecuteImmediat(RawByteString('commit'), lcOther);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

const
  DBLibIsolationLevels: array[Boolean, TZTransactIsolationLevel] of RawByteString = ((
   'SET TRANSACTION ISOLATION LEVEL READ COMMITTED',
   'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
   'SET TRANSACTION ISOLATION LEVEL READ COMMITTED',
   'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ',
   'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE'),
   ('1','0','1','2','3'));

procedure TZDBLibConnection.InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  ExecuteImmediat(DBLibIsolationLevels[FProvider = dpSybase, Level], lcTransaction);
end;

{$IFDEF TEST_CALLBACK}
function TZDBLibConnection.DBERRHANDLE(Proc: PDBPROCESS; Severity, DbErr,
  OsErr: Integer; DbErrStr, OsErrStr: PAnsiChar): Integer;
var DBLibError: PDBLibError;
begin
  Assert(Proc = FHandle);
  DBLibError := New(PDBLibError);
  DBLibError.dbProc := Proc;
  DBLibError.Severity := Severity;
  DBLibError.DbErr := DBErr;
  DBLibError.OsErr := OsErr;
  if DbErrStr = nil
  then DBLibError.DbErrStr := EmptyRaw
  else ZSetString(DbErrStr, StrLen(DbErrStr), DBLibError.DbErrStr);
  if OsErrStr = nil
  then DBLibError.OsErrStr := EmptyRaw
  else ZSetString(OsErrStr, StrLen(OsErrStr), DBLibError.OsErrStr);
  FSQLErrors.Add(DBLibError);
  Result := INT_CANCEL
end;

function TZDBLibConnection.DBMSGHANDLE(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
  Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar;
  Line: DBUSMALLINT): Integer;
var DBLibMessage: PDBLibMessage;
begin
  Assert(Proc = FHandle);
  DBLibMessage := New(PDBLibMessage);
  DBLibMessage.dbProc := Proc;
  DBLibMessage.MsgNo := MsgNo;
  DBLibMessage.MsgState := MsgState;
  DBLibMessage.Severity := Severity;
  if MsgText = nil
  then DBLibMessage.MsgText := EmptyRaw
  else ZSetString(MsgText, StrLen(MsgText), DBLibMessage.MsgText);
  if SrvName = nil
  then DBLibMessage.SrvName := EmptyRaw
  else ZSetString(SrvName, StrLen(SrvName), DBLibMessage.SrvName);
  if ProcName = nil
  then DBLibMessage.ProcName := EmptyRaw
  else ZSetString(ProcName, StrLen(ProcName), DBLibMessage.ProcName);
  FSQLMessages.Add(DBLibMessage);
  Result := INT_EXIT
end;
{$ENDIF TEST_CALLBACK}

destructor TZDBLibConnection.Destroy;
begin
  try
    inherited Destroy;
  finally
    FreeAndNil(FSQLErrors);
    FreeAndNil(FSQLMessages);
  end;
end;

procedure TZDBLibConnection.DetermineMSDateFormat;
{$IFDEF UNICODE}
var
  Tmp: RawByteString;
{$ENDIF}
begin
  {$IFDEF UNICODE}Tmp{$ELSE}FLogMessage{$ENDIF} := 'SELECT dateformat FROM master.dbo.syslanguages WHERE name = @@LANGUAGE';
  if (FPlainDriver.dbcmd(FHandle, Pointer({$IFDEF UNICODE}Tmp{$ELSE}FLogMessage{$ENDIF})) <> DBSUCCEED) or
     (FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbnextrow(FHandle) <> REG_ROW) then begin
    {$IFDEF UNICODE}
    FLogMessage := USASCII7ToUnicodeString(Tmp);
    {$ENDIF}
    CheckDBLibError(lcOther, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr))
  end else
    ZSetString(PAnsiChar(FPlainDriver.dbdata(FHandle, 1)),
      FPlainDriver.dbDatLen(FHandle, 1), ConSettings^.ReadFormatSettings.DateFormat);
  FPlainDriver.dbCancel(FHandle);
  if ConSettings^.ReadFormatSettings.DateFormat = 'dmy' then
    ConSettings^.ReadFormatSettings.DateFormat := 'DD/MM/YYYY'
  else if ConSettings^.ReadFormatSettings.DateFormat = 'mdy' then
    ConSettings^.ReadFormatSettings.DateFormat := 'MM/DD/YYYY'
  else
    ConSettings^.ReadFormatSettings.DateFormat := 'YYYY/MM/DD';
  ConSettings^.ReadFormatSettings.DateTimeFormat := ConSettings^.ReadFormatSettings.DateFormat+' HH:NN:SS:ZZZ';
end;

function TZDBLibConnection.DetermineMSServerCollation: String;
{$IFDEF UNICODE}
var
  Tmp: RawByteString;
{$ENDIF UNICODE}
begin
  FLogMessage := 'SELECT DATABASEPROPERTYEX('+
    SQLQuotedStr(URL.Database, #39)+', ''Collation'') as DatabaseCollation';
  {$IFDEF UNICODE}
  Tmp := ZUnicodeToRaw(FLogMessage, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if (FPlainDriver.dbcmd(FHandle, Pointer({$IFDEF UNICODE}Tmp{$ELSE}FLogMessage{$ENDIF})) <> DBSUCCEED) or
     (FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbnextrow(FHandle) <> REG_ROW) then
    CheckDBLibError(lcOther, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr))
  else
    {$IFDEF UNICODE}
    Result := USASCII7ToUnicodeString(PAnsiChar(FPlainDriver.dbdata(FHandle, 1)), FPlainDriver.dbDatLen(FHandle, 1));
    {$ELSE}
    ZSetString(PAnsiChar(FPlainDriver.dbdata(FHandle, 1)), FPlainDriver.dbDatLen(FHandle, 1), Result{%H-});
    {$ENDIF}
  FPlainDriver.dbCancel(FHandle);
end;

{**
  Executes simple statements immediatally.
}
procedure TZDBLibConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
begin
  if Pointer(SQL) = nil then
    Exit;
  FHandle := GetConnectionHandle;
  {$IFDEF UNICODE}
  FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ELSE}
  FLogMessage := SQL;
  {$ENDIF}
  if (FPlainDriver.dbcmd(FHandle, Pointer(SQL)) <> DBSUCCEED) or
     (FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) then
    CheckDBLibError(LoggingCategory, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  repeat
    FPlainDriver.dbresults(FHandle);
    FPlainDriver.dbcanquery(FHandle);
  until FPlainDriver.dbmorecmds(FHandle) = DBFAIL;
  CheckDBLibError(LoggingCategory, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, URL.Protocol, FLogMessage);
end;

function TZDBLibConnection.GetServerCollation: String;
begin
  if FProvider = dpMsSQL
  then Result := DetermineMSServerCollation
  else Result := 'unknown';
end;

function TZDBLibConnection.DetermineMSServerCodePage(const Collation: String): Word;
var P: PAnsiChar;
    L: LengthInt;
{$IFDEF UNICODE}
  Tmp: RawByteString;
{$ENDIF}
begin
  Result := High(Word);
  FLogMessage := 'SELECT COLLATIONPROPERTY('''+Collation+''', ''Codepage'') as Codepage';
  {$IFDEF UNICODE}
  Tmp := UnicodeStringToASCII7(FLogMessage);
  {$ENDIF}
  if (FPlainDriver.dbcmd(FHandle, Pointer({$IFDEF UNICODE}Tmp{$ELSE}FLogMessage{$ENDIF})) <> DBSUCCEED) or
     (FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (FPlainDriver.dbnextrow(FHandle) <> REG_ROW) then
    CheckDBLibError(lcOther, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr))
  else begin
    P := PAnsiChar(FPlainDriver.dbdata(FHandle, 1));
    L := FPlainDriver.dbDatLen(FHandle, 1);
    Result := RawToIntDef(P, P+L, High(Word));
    FPlainDriver.dbCancel(FHandle);
  end;
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZDBLibConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone then
    TransactIsolationLevel := tiReadCommitted; //use default til
  if Level <> TransactIsolationLevel then begin
    if not Closed then begin
      if not AutoCommit then
        raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
      InternalSetTransactionIsolation(Level);
    end;
    TransactIsolationLevel := Level;
  end;
end;

function TZDBLibConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    ExecuteImmediat(RawByteString('begin transaction'), lcTransaction);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    ExecuteImmediat('SAVE TRANSACTION '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Commit;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  if not Closed then
    if FSavePoints.Count > 0
    then FSavePoints.Delete(FSavePoints.Count-1)
    else begin
      ExecuteImmediat(RawByteString('commit'), lcTransaction);
      AutoCommit := True;
      if FRestartTransaction then
        StartTransaction;
    end
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Rollback;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollBack);
  if not Closed then
    if FSavePoints.Count > 0 then begin
      S := 'ROLLBACK TRANSACTION '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
      FSavePoints.Delete(FSavePoints.Count-1);
    end else begin
      ExecuteImmediat(RawByteString('rollback'), lcTransaction);
      AutoCommit := True;
      if FRestartTransaction then
        StartTransaction;
    end
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZDBLibConnection.InternalClose;
begin
  if Closed or not Assigned(PlainDriver) then
    Exit;
  FSavePoints.Clear;
  try
    if not AutoCommit then begin
      AutoCommit := not FRestartTransaction;
      ExecuteImmediat(RawByteString('rollback'), lcTransaction);
    end;
    ClearTransactions;
  finally
    if FHandle <> nil then begin
      FPlainDriver.dbclose(FHandle);
      FHandle := nil;
    end;
    {$IFDEF TEST_CALLBACK}
    FDBLibErrorHandler := nil;
    FDBLibMessageHandler := nil;
    {$ENDIF TEST_CALLBACK}
    if FSQLErrors <> nil then
      FSQLErrors.Clear;
    if FSQLErrors <> nil then
      FSQLErrors.Clear;
    if DriverManager.HasLoggingListener then begin
      FLogMessage := 'CLOSE CONNECTION TO "'+HostName+'" DATABASE "'+URL.Database+'"';
      DriverManager.LogMessage(lcDisconnect, URL.Protocol, FLogMessage);
    end;
  end;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZDBLibConnection.SetReadOnly(Value: Boolean);
begin
  //sql server and sybase do not support RO-Transaction or Sessions
  //all we have is a readonly database ...
  if Value then
    raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZDBLibConnection.SetCatalog(const Value: string);
var
  RawCat: RawByteString;
begin
  if (Value <> '') and not Closed then
  begin
    {$IFNDEF NO_AUTOENCODE}
    RawCat := ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
      {$IFDEF UNICODE}
      RawCat := ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP);
      {$ELSE}
      RawCat := Value;
      {$ENDIF}
    {$ENDIF}
    FLogMessage := 'SET CATALOG '+Value;
    if FPlainDriver.dbUse(FHandle, PAnsiChar(RawCat)) <> DBSUCCEED then
      CheckDBLibError(lcOther, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    DriverManager.LogMessage(lcOther, URL.Protocol, FLogMessage);
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZDBLibConnection.GetCatalog: string;
begin
  Result := String(FPlainDriver.dbName(FHandle));
  CheckDBLibError(lcOther, 'GETCATALOG', IImmediatelyReleasable(FWeakImmediatRelPtr));
end;

function TZDBLibConnection.GetServerAnsiCodePage: Word;
begin
  Result := FServerAnsiCodePage;
end;

function TZDBLibConnection.GetServerProvider: TZServerProvider;
const DBLib2ServerProv: Array[TDBLIBProvider] of TZServerProvider = (spMSSQL, spASE);
begin
  Result := DBLib2ServerProv[FProvider];
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZDBLibConnection.GetWarnings: EZSQLWarning;
begin
  Result := FLastWarning;
end;

initialization
  DBLibDriver := TZDBLibDriver.Create;
  DriverManager.RegisterDriver(DBLibDriver);
  DBLIBCriticalSection := TCriticalSection.Create;
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(DBLibDriver);
  DBLibDriver := nil;
  FreeAndNil(DBLIBCriticalSection);
{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
