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

unit ZDbcInterbase6;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZPlainFirebirdDriver, ZCompatibility, ZDbcUtils, ZDbcIntfs, ZDbcCachedResultSet,
  ZDbcConnection, ZPlainFirebirdInterbaseConstants, ZSysUtils, ZDbcLogging,
  ZDbcInterbase6Utils, ZDbcGenericResolver, ZTokenizer, ZGenericSqlAnalyser,
  ZDbcCache, ZURL, ZClasses, ZCollections;

type

  {** Implements Interbase6 Database Driver. }
  TZInterbase6Driver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  TZInterbase6ConnectionGUIDProps = class;

  IZIBTransaction = interface(IZTransaction)
    ['{FACB5CA2-4400-470E-A1DC-ECE29CDE4E6F}']
    procedure CloseTransaction;
    function GetTrHandle: PISC_TR_HANDLE;
    procedure RegisterOpencursor(const CursorRS: IZResultSet);
    procedure RegisterOpenUnCachedLob(const Lob: IZlob);
    procedure DeRegisterOpenCursor(const CursorRS: IZResultSet);
    procedure DeRegisterOpenUnCachedLob(const Lob: IZlob);
    function GetTransactionLevel: Integer;
    function GetOpenCursorCount: Integer;
    function IsReadOnly: Boolean;
    function IsAutoCommit: Boolean;
    function GetTPB: RawByteString;
    function StartTransaction: Integer;
  end;

  {** Represents a Interbase specific connection interface. }
  IZInterbase6Connection = interface (IZConnection)
    ['{E870E4FE-21EB-4725-B5D8-38B8A2B12D0B}']
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetXSQLDAMaxSize: LongWord;
    function GetGUIDProps: TZInterbase6ConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetActiveTransaction: IZIBTransaction;
    function IsFirebirdLib: Boolean;
    function IsInterbaseLib: Boolean;
    function GetPlainDriver: TZInterbasePlainDriver;
  end;

  TGUIDDetectFlag = (gfByType, gfByDomain, gfByFieldName);
  TGUIDDetectFlags = set of TGUIDDetectFlag;

  {** Implements GUID detection options/properties }

  TZInterbase6AbstractGUIDProps = class
  private
    FDetectFlags: TGUIDDetectFlags;
    FDomains: TStrings;
    FFields: TStrings;
  protected // to access from descendants
    procedure InternalInit(const OptionByType, OptionDomains, OptionFields: string);
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  public
    destructor Destroy; override;
    function ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
  end;

  // Reusable object intended for use on Connection level. Allows re-initialization
  // if Connection properties are changed. Uses Connection properties only
  TZInterbase6ConnectionGUIDProps = class(TZInterbase6AbstractGUIDProps)
  protected // to access from other classes of the unit
    procedure InitFromProps(Properties: TStrings);
  public
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  end;

  // Temporary object intended for use on Statement level. Should be re-created
  // whenever a Statement is opened. Uses Statement & Connection properties.
  // Doesn't consider domain info (there's no domains on Statement level)
  TZInterbase6StatementGUIDProps = class(TZInterbase6AbstractGUIDProps)
  public
    constructor Create(const Statement: IZStatement); overload;
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnName: string): Boolean;
  end;

  {** Implements Interbase6 Database Connection. }

  { TZInterbase6Connection }
  TZInterbase6Connection = class(TZAbstractDbcConnection, IZConnection,
    IZInterbase6Connection, IZTransactionManager)
  private
    FDialect: Word;
    FHandle: TISC_DB_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FHardCommit: boolean;
    FHostVersion: Integer;
    FClientVersion: Integer;
    FIsFirebirdLib: Boolean; // never use this directly, always use IsFirbirdLib
    FIsInterbaseLib: Boolean; // never use this directly, always use IsInterbaseLib
    FPB_CP: Word; //the parameter buffer codepage
    FXSQLDAMaxSize: LongWord;
    FPlainDriver: TZInterbasePlainDriver;
    FProcedureTypesCache: TStrings;
    FGUIDProps: TZInterbase6ConnectionGUIDProps;
    FTPBs: array[Boolean,Boolean,TZTransactIsolationLevel] of RawByteString;
    fTransactions: array[Boolean] of IZCollection; //simultan (not nested) readonly/readwrite transaction container
    fActiveTransaction: array[Boolean] of IZIBTransaction;
    procedure DetermineClientTypeAndVersion;
    procedure AssignISC_Parameters;
    function GenerateTPB(AutoCommit, ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
      Info: TStrings): RawByteString;
    procedure TransactionParameterPufferChanged;
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure OnPropertiesChange({%H-}Sender: TObject); override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure ExecuteImmediat(const SQL: RawByteString; ISC_TR_HANDLE: PISC_TR_HANDLE; LoggingCategory: TZLoggingCategory); overload;
  public
    constructor Create(const ZUrl: TZURL);
    destructor Destroy; override;
    function GetHostVersion: Integer; override;
    function GetClientVersion: Integer; override;
    function IsFirebirdLib: Boolean;
    function IsInterbaseLib: Boolean;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetXSQLDAMaxSize: LongWord;
    function GetGUIDProps: TZInterbase6ConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetActiveTransaction: IZIBTransaction;
    function GetPlainDriver: TZInterbasePlainDriver;
  public { IZTransactionManager }
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    procedure ReleaseTransaction(const Transaction: IZTransaction);
    procedure SetActiveTransaction(const Value: IZTransaction);
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;

    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;

    procedure Commit;
    procedure Rollback;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetAutoCommit(Value: Boolean); override;
    function StartTransaction: Integer;

    function PingServer: Integer; override;
    function AbortOperation: Integer; override;

    function ConstructConnectionString: String;
    procedure Open; override;

    function GetBinaryEscapeString(const Value: RawByteString): String; override;
    function GetBinaryEscapeString(const Value: TBytes): String; override;
    function GetServerProvider: TZServerProvider; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  {** Implements a Interbase6/Firebird sequence. }
  TZInterbase6Sequence = class(TZIdentifierSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
    procedure SetBlockSize(const Value: Integer); override;
  end;

  {** EH: implements a IB/FB transaction }
  TZIBTransaction = class(TZCodePagedObject, IImmediatelyReleasable,
    IZTransaction, IZIBTransaction)
  private
    fSavepoints: TStrings;
    fDoCommit, fDoLog: Boolean;
    FOpenCursors, FOpenUncachedLobs: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FReadOnly, FAutoCommit: Boolean;
    FTrHandle: TISC_TR_HANDLE;
    FTPB: RawByteString;
    FTEB: TISC_TEB;
    FExplicitTransactionCounter: Integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZInterbase6Connection;
    function TestCachedResultsAndForceFetchAll: Boolean;
  public { IZTransaction }
    procedure Commit;
    procedure Rollback;
    function StartTransaction: Integer;
  public { IZIBTransaction }
    procedure CloseTransaction;
    function GetTrHandle: PISC_TR_HANDLE;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    procedure RegisterOpencursor(const CursorRS: IZResultSet);
    procedure RegisterOpenUnCachedLob(const Lob: IZlob);
    procedure DeRegisterOpenCursor(const CursorRS: IZResultSet);
    procedure DeRegisterOpenUnCachedLob(const Lob: IZlob);
    function GetTransactionLevel: Integer;
    function GetOpenCursorCount: Integer;
    function GetTPB: RawByteString;
    function IsReadOnly: Boolean;
    function IsAutoCommit: Boolean;
  public
    constructor Create(const Owner: TZInterbase6Connection; AutoCommit, ReadOnly: Boolean;
      const TPB: RawByteString);
    procedure BeforeDestruction; override;
  end;

  IFB_IB_SavePoint = interface
    ['{96D74A82-A1ED-4190-9CF1-A969BF73A1E9}']
    function GetOwnerTransaction: IZIBTransaction;
  end;

var
  {** The common driver manager object. }
  Interbase6Driver: IZDriver;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses ZFastCode, ZDbcInterbase6Statement, ZDbcInterbase6Metadata, ZEncoding,
  ZInterbaseToken, ZInterbaseAnalyser, ZDbcMetadata, ZMessages,
  ZDbcProperties, Math
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
  AddSupportedProtocol(AddPlainDriverToCache(TZFirebirdPlainDriver.Create));
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZInterbase6Driver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZInterbase6Driver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZInterbase6Driver.GetTokenizer: IZTokenizer;
begin
  Result := TZInterbaseTokenizer.Create;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZInterbase6Driver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZInterbaseStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZInterbase6Connection }

constructor TZInterbase6Connection.Create(const ZUrl: TZURL);
begin
  // ! Create the object before parent's constructor because it is used in
  // TZAbstractDbcConnection.Create > Url.OnPropertiesChange
  FGUIDProps := TZInterbase6ConnectionGUIDProps.Create;
  inherited Create(ZUrl);
  FClientVersion := -1;
  FIsFirebirdLib := false;
  FIsInterbaseLib := false;
end;

destructor TZInterbase6Connection.Destroy;
begin
  FreeAndNil(FProcedureTypesCache);
  FreeAndNil(FGUIDProps);
  inherited Destroy;
end;

Function TZInterbase6Connection.AbortOperation: Integer;
Begin
 {$MESSAGE '.AbortOperation with Firebird is untested and might cause unexpected results!'}
 // https://github.com/Alexpux/firebird-git-svn/blob/master/doc/README.fb_cancel_operation
 if assigned(FPlainDriver.fb_cancel_operation) then begin
   Result := 0;
   If FPlainDriver.fb_cancel_operation(@FStatusVector, @FHandle, fb_cancel_raise) <> 0 Then
     CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
 end else begin
   Result := 1 //abort opertion is not supported by the current client library
 end;
End;

procedure TZInterbase6Connection.AssignISC_Parameters;
var
  RoleName: string;
  ConnectTimeout, Idx: integer;
  WireCompression: Boolean;
begin
  { set default sql dialect it can be overriden }
  FDialect := StrToIntDef(Info.Values[ConnProps_Dialect], SQL_DIALECT_CURRENT);

  Info.BeginUpdate; // Do not call OnPropertiesChange every time a property changes
  { Processes connection properties. }
  if Info.Values['isc_dpb_username'] = '' then
    Info.Values['isc_dpb_username'] := Url.UserName;
  if Info.Values['isc_dpb_password'] = '' then
    Info.Values['isc_dpb_password'] := Url.Password;

  if FClientCodePage = '' then //was set on inherited Create(...)
    if Info.Values['isc_dpb_lc_ctype'] <> '' then //Check if Dev set's it manually
    begin
      FClientCodePage := Info.Values['isc_dpb_lc_ctype'];
      CheckCharEncoding(FClientCodePage, True);
    end;
  Info.Values['isc_dpb_lc_ctype'] := FClientCodePage;

  RoleName := Trim(Info.Values[ConnProps_Rolename]);
  if RoleName <> '' then
    Info.Values['isc_dpb_sql_role_name'] := UpperCase(RoleName);

  ConnectTimeout := StrToIntDef(Info.Values[ConnProps_Timeout], -1);
  if ConnectTimeout >= 0 then
    Info.Values['isc_dpb_connect_timeout'] := ZFastCode.IntToStr(ConnectTimeout);

  WireCompression := StrToBoolEx(Info.Values[ConnProps_WireCompression]);
  if WireCompression then
    Info.Values['isc_dpb_config'] :=
      Info.Values['isc_dpb_config'] + LineEnding + 'WireCompression=true';

  if Info.IndexOf('isc_dpb_sql_dialect') = -1 then
    Info.Values['isc_dpb_sql_dialect'] := IntToStr(FDialect);

  Idx := Info.IndexOf('isc_dpb_utf8_filename');
  if (GetClientVersion >= 2005000) and IsFirebirdLib then begin
    if (Idx = -1) and ((FClientCodePage = 'UTF8') or (FClientCodePage = 'UNICODE_FSS')) then
      Info.Add('isc_dpb_utf8_filename');
    FPB_CP := zCP_UTF8;
  end else if Idx <> -1 then begin
    Info.Delete(Idx);
    FPB_CP := ConSettings.ClientCodePage.CP;
  end;
  Info.EndUpdate;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZInterbase6Connection.InternalClose;
var B: Boolean;
begin
  try
    for B := False to True do begin
      fTransactions[b].Clear;
      fActiveTransaction[b] := nil;
    end;
  finally
    if Assigned(DriverManager) and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
          'DISCONNECT FROM "'+ConSettings^.DataBase+'"');
    if FHandle <> 0 then begin
      FPlainDriver.isc_detach_database(@FStatusVector, @FHandle);
      FHandle := 0;
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcDisconnect);
    end;
  end;
end;

{**
   Commit current transaction
}
procedure TZInterbase6Connection.Commit;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  with GetActiveTransaction do begin
    Commit;
    if (not FRestartTransaction) and (GetTransactionLevel = 0) then
      SetAutoCommit(True)
  end;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZInterbase6Connection.InternalCreate;
begin
  FPlainDriver := TZInterbasePlainDriver(PlainDriver.GetInstance);
  FMetadata := TZInterbase6DatabaseMetadata.Create(Self, Url);
  fTransactions[False] := TZCollection.Create;
  fTransactions[True] := TZCollection.Create;
  { set default sql dialect it can be overriden }
  FDialect := StrToIntDef(Info.Values[ConnProps_Dialect], SQL_DIALECT_CURRENT);

  FXSQLDAMaxSize := 64*1024; //64KB by default
  FHandle := 0;
  FProcedureTypesCache := TStringList.Create;
end;

procedure TZInterbase6Connection.OnPropertiesChange(Sender: TObject);
var
  AC,RO, HC: Boolean;
  TIL: TZTransactIsolationLevel;
begin
  HC := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
  FGUIDProps.InitFromProps(Info);
  for RO := false to true do begin
    for AC := false to true do
      for til := low(TZTransactIsolationLevel) to high(TZTransactIsolationLevel) do
        FTPBs[AC][RO][TIL] := '';
    if FHandle <> 0 then
      FTPBs[AutoCommit][RO][TransactIsolationLevel] := GenerateTPB(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
    if (fActiveTransaction[RO] <> nil) and ((HC <> FHardCommit) or
       (fActiveTransaction[RO].GetTPB <> FTPBs[AutoCommit][RO][TransactIsolationLevel])) then//*** ADDED THIS CHECK by EMartin ***
      TransactionParameterPufferChanged;
  end;
  FHardCommit := HC;
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZInterbase6Connection.GetHostVersion: Integer;
begin
  Result := FHostVersion;
end;

function TZInterbase6Connection.GetPlainDriver: TZInterbasePlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Determines the Client Library vendor and version. Works for Firebird 1.5+ and
  Interbase 7+
}
procedure TZInterbase6Connection.DetermineClientTypeAndVersion;
var
  Major, Minor, Release: Integer;
  VersionStr: String;
  FbPos: Integer;
  DotPos: Integer;
  Buff: array[0..50] of AnsiChar;
begin
  Release := 0;

  if Assigned(FplainDriver.isc_get_client_version) then begin
    FplainDriver.isc_get_client_version(@Buff[0]);
    {$IFDEF UNICODE}
    VersionStr := ZSysUtils.ASCII7ToUnicodeString(@Buff[0], ZFastCode.StrLen(PAnsiChar(@Buff[0])));
    {$ELSE}
    SetString(VersionStr, PAnsiChar(@Buff[0]), ZFastCode.StrLen(PAnsiChar(@Buff[0])));
    {$ENDIF}
  end else
    VersionStr := '';
  FbPos := System.Pos('firebird', LowerCase(VersionStr));
  if FbPos > 0 then begin
    FIsFirebirdLib := true;
    // remove the fake Major version number
    DotPos := System.Pos('.', VersionStr);
    Delete(VersionStr, 1, DotPos);
    // remove the fake Minor version number
    DotPos := System.Pos('.', VersionStr);
    Delete(VersionStr, 1, DotPos);
    // get the release number
    DotPos := System.Pos('.', VersionStr);
    Release := StrToIntDef(Copy(VersionStr, 1, DotPos - 1), 0);
    // remove the Firebird brand including the space
    FbPos := System.Pos('firebird', LowerCase(VersionStr));
    Delete(VersionStr, 1, FbPos + 8);
    // get the major and minor version numbers
    DotPos := System.Pos('.', VersionStr);
    Major := StrToIntDef(Copy(VersionStr, 1, DotPos - 1), 0);
    Minor := StrToIntDef(Copy(VersionStr, DotPos + 1, length(VersionStr)), 0);
  end else begin
    if Assigned(FPlainDriver.isc_get_client_major_version)
    then Major := FPlainDriver.isc_get_client_major_version()
    else Major := 0;
    if Assigned(FPlainDriver.isc_get_client_major_version)
    then Minor := FPlainDriver.isc_get_client_minor_version()
    else Minor := 0;
    FIsInterbaseLib := Major <> 0;
  end;

  FClientVersion := Major * 1000000 + Minor * 1000 + Release;
end;

procedure TZInterbase6Connection.ExecuteImmediat(const SQL: RawByteString;
  ISC_TR_HANDLE: PISC_TR_HANDLE; LoggingCategory: TZLoggingCategory);
begin
  if SQL = '' then
    Exit;
  if FPlainDriver.isc_dsql_execute_immediate(@FStatusVector, @FHandle,
      ISC_TR_HANDLE, Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF},
      Pointer(SQL), GetDialect, nil) <> 0 then
    CheckInterbase6Error(FPlainDriver, FStatusVector, Self, LoggingCategory);
  DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, SQL);
end;

procedure TZInterbase6Connection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
begin
  ExecuteImmediat(SQL, GetActiveTransaction.GetTrHandle, LoggingCategory);
end;

{**
  Gets the client's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZInterbase6Connection.GetClientVersion: Integer;
begin
  if FClientVersion = -1 then DetermineClientTypeAndVersion;
  Result := FClientVersion;
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
   Return Interbase dialect number. Dialect a dialect Interbase SQL
   must be 1 or 2 or 3.
   @return dialect number
}
function TZInterbase6Connection.GetDialect: Word;
begin
  Result := FDialect;
end;

function TZInterbase6Connection.GetXSQLDAMaxSize: LongWord;
begin
  Result := FXSQLDAMaxSize;
end;

function TZInterbase6Connection.GetGUIDProps: TZInterbase6ConnectionGUIDProps;
begin
  Result := FGUIDProps;
end;

function TZInterbase6Connection.GetServerProvider: TZServerProvider;
begin
  Result := spIB_FB;
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
const sCS_NONE = 'NONE';
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
  CSNoneCP := Info.Values[DSProps_ResetCodePage];

  FHandle := 0;
  DBCreated := False;
  { Create new db if needed }
  if (Info.Values[ConnProps_CreateNewDatabase] <> '') then begin
    CreateDB := Info.Values[ConnProps_CreateNewDatabase];
    if (GetClientVersion >= 2005000) and IsFirebirdLib and (Length(CreateDB)<=4) and StrToBoolEx(CreateDB, False) then begin
      if (Info.Values['isc_dpb_lc_ctype'] <> '') and (Info.Values['isc_dpb_set_db_charset'] = '') then
        Info.Values['isc_dpb_set_db_charset'] := Info.Values['isc_dpb_lc_ctype'];
      DBCP := Info.Values['isc_dpb_set_db_charset'];
      PrepareDPB;
      if FPlainDriver.isc_create_database(@FStatusVector, SmallInt(StrLen(@DBName[0])),
          @DBName[0], @FHandle, Smallint(Length(DPB)),Pointer(DPB), 0) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcConnect);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
          'CREATE DATABASE "'+ConSettings.Database+'" AS USER "'+ ConSettings^.User+'"');
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
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, DPB);
      { Logging connection action }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
          DPB+' AS USER "'+ ConSettings^.User+'"');
      //we did create the db and are connected now.
      //we have no dpb so we connect with 'NONE' which is not a problem for the UTF8/NONE charsets
      //because the metainformations are retrieved in UTF8 encoding
      if (DBCP <> FClientCodePage) or ((DBCP = sCS_NONE) and (FClientCodePage <> '') and
         ((FClientCodePage <> 'UTF8') and (FClientCodePage <> sCS_NONE))) then begin
        //we need a reconnect with a valid dpb
        if FPlainDriver.isc_detach_database(@FStatusVector, @FHandle) <> 0 then
          CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, DPB);
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
    { Connect to Interbase6 database. }
    if FPlainDriver.isc_attach_database(@FStatusVector,
        ZFastCode.StrLen(@DBName[0]), @DBName[0], @FHandle, Length(DPB), Pointer(DPB)) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcConnect);

    { Dialect could have changed by isc_dpb_set_db_SQL_dialect command }
    FDialect := GetDBSQLDialect(FPlainDriver, @FHandle, Self);
    { Logging connection action }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
        'CONNECT TO "'+ConSettings^.DataBase+'" AS USER "'+ConSettings^.User+'"');
  end;

  inherited SetAutoCommit(AutoCommit or (Info.IndexOf('isc_tpb_autocommit') <> -1));
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
      ti.CloseTransaction;
      ReleaseTransaction(ti);
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
      else CSNoneCP := 'WIN1252'; {WIN1252 would be optimal propably}
    ResetCurrentClientCodePage(CSNoneCP, False);
    ConSettings^.ClientCodePage^.ID := 0;
    //Now notify our metadata object all fields are retrieved in utf8 encoding
    (FMetadata as TZInterbase6DatabaseMetadata).SetUTF8CodePageInfo;
    if (FCLientCodePage <> DBCP) then begin
      Info.Values['isc_dpb_lc_ctype'] := DBCP;
      InternalClose;
      goto reconnect; //build new TDB and reopen in SC_NONE mode
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
  B: Boolean;
begin
  FHandle := 0;
  for B := False to True do begin
    if (fActiveTransaction[b] <> nil) and
       (fActiveTransaction[b].QueryInterface(IImmediatelyReleasable, ImmediatelyReleasable) = S_OK) and
        (ImmediatelyReleasable <> Sender) then
        ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    while fTransactions[b].Count > 0 do begin
      fTransactions[b][0].QueryInterface(IImmediatelyReleasable, ImmediatelyReleasable);
      if ImmediatelyReleasable <> Sender then
        ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    end;
  end;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZInterbase6Connection.ReleaseTransaction(
  const Transaction: IZTransaction);
var idx: Integer;
  Trans: IZTransaction;
  B: Boolean;
begin
  for B := False to True do begin
    if (fActiveTransaction[B] <> nil) then begin
      fActiveTransaction[B].QueryInterface(IZTransaction, Trans);
      if (Trans = Transaction) then
        fActiveTransaction[B] := nil;
    end;
    Idx := fTransactions[b].IndexOf(Transaction);
    if Idx <> -1 then begin
      fTransactions[b].Delete(Idx);
      Exit;
    end;
  end;
  raise EZSQLException.Create('release an invalid Transaction');
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZInterbase6Connection.Rollback;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  with GetActiveTransaction do begin
    Rollback;
    if (not FRestartTransaction) and (GetTransactionLevel = 0) then
      SetAutoCommit(True);
  end;
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

{**
   Start Interbase transaction
}
function TZInterbase6Connection.StartTransaction: Integer;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    AutoCommit := False;
    TransactionParameterPufferChanged;
  end else GetActiveTransaction.GetTrHandle;
  Result := GetActiveTransaction.StartTransaction;
end;

function TZInterbase6Connection.StoredProcedureIsSelectable(
  const ProcName: String): Boolean;
var I: Integer;
  function AddToCache(const ProcName: String): Boolean;
  var RS: IZResultSet;
    Stmt: IZStatement;
  begin
    Result := False;
    Stmt := CreateStatementWithParams(Info);
    RS := Stmt.ExecuteQuery('SELECT RDB$PROCEDURE_TYPE FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = '+QuotedStr(ProcName));
    if RS <> nil then try
      if RS.Next then begin
        Result := RS.GetShort(FirstDbcIndex)=1; //Procedure type 2 has no suspend
        FProcedureTypesCache.AddObject(ProcName, TObject(Ord(Result)));
      end else
        Raise EZUnsupportedException.Create(SUnsupportedOperation);
    finally
      RS.Close;
      RS := nil;
      Stmt := nil;
    end;
  end;
begin
  I := FProcedureTypesCache.IndexOf(ProcName);
  if I = -1
  then Result := AddToCache(ProcName)
  else Result := FProcedureTypesCache.Objects[I] <> nil;
end;

procedure TZInterbase6Connection.TransactionParameterPufferChanged;
begin
  if (fActiveTransaction[ReadOnly] <> nil) then begin
    fActiveTransaction[ReadOnly].CloseTransaction;
    if (fActiveTransaction[ReadOnly] <> nil) then
      ReleaseTransaction(fActiveTransaction[ReadOnly]);
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
procedure TZInterbase6Connection.SetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  if (Level <> TransactIsolationLevel) then begin
    TransactionParameterPufferChanged;
    TransactIsolationLevel := Level;
    //restart automatically happens on GetTrHandle
  end;
end;

function TZInterbase6Connection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  //http://tracker.firebirdsql.org/browse/CORE-2789
  if (GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsBinaryInSQL then
    if (Length(Value)*2+3) < 32*1024
    then Result := GetSQLHexString(PAnsiChar(Value), Length(Value))
    else raise Exception.Create('Binary data out of range! Use parameters!')
  else raise Exception.Create('Your Firebird-Version does''t support Binary-Data in SQL-Statements! Use parameters!');
end;

const
  Tpb_Access: array[boolean] of String = ('isc_tpb_write','isc_tpb_read');
  tpb_AutoCommit: array[boolean] of String = ('','isc_tpb_autocommit');
function TZInterbase6Connection.GenerateTPB(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel; Info: TStrings): RawByteString;
var
  Params: TStrings;
{ List of parameters that are assigned according to values of properties but
  could be overwritten by user.
  These parameters are all simple flags having no value so no splitting is required. }
type
  TOverwritableParams = (parTIL, parRW, parRecVer, parWait, parAutoCommit);
  TOverwritableParamValues = array[TOverwritableParams] of string;

  { Add all items from Src to Dest except those which define overwritable params.
    Value of these params are returned in OverwritableParams array. }
  procedure AddStrings(Dest, Src: TStrings; var OverwritableParams: TOverwritableParamValues);
  var
    I: Integer;
    SrcPar: string;
  begin
    for I := 0 to Src.Count - 1 do
    begin
      SrcPar := LowerCase(Src[I]);
      if (SrcPar = 'isc_tpb_consistency') or
         (SrcPar = 'isc_tpb_concurrency') or
         (SrcPar = 'isc_tpb_read_committed') then
        OverwritableParams[parTIL] := SrcPar
      else
      if (SrcPar = 'isc_tpb_wait') or
         (SrcPar = 'isc_tpb_nowait') then
        OverwritableParams[parWait] := SrcPar
      else
      if (SrcPar = 'isc_tpb_read') or
         (SrcPar = 'isc_tpb_write') then
        OverwritableParams[parRW] := SrcPar
      else
      if (SrcPar = 'isc_tpb_rec_version') or
         (SrcPar = 'isc_tpb_no_rec_version') then
        OverwritableParams[parRecVer] := SrcPar
      else
      if (SrcPar = 'isc_tpb_autocommit') then
        OverwritableParams[parAutoCommit] := SrcPar
      else if StartsWith(SrcPar, TPBPrefix) then  //skip all non isc_tpb params
        Dest.Add(Src[I]);
    end;
  end;
var
  OverwritableParams: TOverwritableParamValues;
begin
  Params := TStringlist.Create;
  try
    Params.Capacity := Ord(High(TOverwritableParams))+Info.Count;
    //OverwritableParams[parRW] := tpb_Access[ReadOnly]; will be set always below
    OverwritableParams[parAutoCommit] := tpb_AutoCommit[AutoCommit];

    { Set transaction parameters by TransactIsolationLevel }
    case TransactIsolationLevel of
      tiReadCommitted:
        begin
          if GetHostVersion >= 4000000
          then OverwritableParams[parRecVer] := 'isc_tpb_read_consistency'
          else OverwritableParams[parRecVer] := 'isc_tpb_rec_version';
          OverwritableParams[parWait] := 'isc_tpb_nowait';
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := 'isc_tpb_read_committed';
        end;
      tiRepeatableRead:
        begin
          OverwritableParams[parWait] := 'isc_tpb_nowait';
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := 'isc_tpb_concurrency';
        end;
      tiSerializable:
        begin
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := 'isc_tpb_consistency';
        end;
      else begin
        OverwritableParams[parRW] := tpb_Access[ReadOnly]; //eh: why is this done before AddStrings is called?
        { FB default values for non-standard TIL }
        OverwritableParams[parTIL] := 'isc_tpb_concurrency';
        OverwritableParams[parWait] := 'isc_tpb_wait';
        AddStrings(Params, Info, OverwritableParams);
      end;
    end;
    { EH: as long we allow to overwrite the two params we need to sync our settings: }
    if OverwritableParams[parRW] <> tpb_Access[ReadOnly] then //overwrite seems to be allowed for tiNone
      inherited SetReadOnly(not ReadOnly);
    if OverwritableParams[parAutoCommit] <> tpb_AutoCommit[AutoCommit] then //overwrite seems to be allowed allways
      inherited SetAutoCommit(not AutoCommit);


    { Add overwitable parameters to the beginning of list }
    if OverwritableParams[parRW] <> '' then
      Params.Insert(0, OverwritableParams[parRW]);
    if OverwritableParams[parWait] <> '' then
      Params.Insert(0, OverwritableParams[parWait]);
    if OverwritableParams[parRecVer] <> '' then
      Params.Insert(0, OverwritableParams[parRecVer]);
    if OverwritableParams[parTIL] <> '' then
      Params.Insert(0, OverwritableParams[parTIL]);
    if OverwritableParams[parAutoCommit] <> '' then
      Params.Insert(0, OverwritableParams[parAutoCommit]);

    Result := BuildPB(FPlainDriver, Params, isc_tpb_version3, TPBPrefix, TransactionParams, ConSettings, FPB_CP);
  finally
    FreeAndNil(Params);
  end;
end;

function TZInterbase6Connection.GetActiveTransaction: IZIBTransaction;
var TA: IZTransaction;
begin
  if FHandle <> 0 then begin
    if fActiveTransaction[ReadOnly] = nil then begin
      TA := CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZIBTransaction, fActiveTransaction[ReadOnly]);
      fTransactions[ReadOnly].Add(TA);
    end;
    Result := fActiveTransaction[ReadOnly];
  end else
    Result := nil;
end;

function TZInterbase6Connection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  //http://tracker.firebirdsql.org/browse/CORE-2789
  if (GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsBinaryInSQL then
    if (Length(Value)*2+3) < 32*1024
    then Result := GetSQLHexString(PAnsiChar(Value), Length(Value))
    else raise Exception.Create('Binary data out of range! Use parameters!')
  else raise Exception.Create('Your Firebird-Version does''t support Binary-Data in SQL-Statements! Use parameters!');
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZInterbase6Connection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZInterbase6Sequence.Create(Self, Sequence, BlockSize);
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
  if FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel] = EmptyRaw then
    FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel] := GenerateTPB(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
  Result := TZIBTransaction.Create(Self, AutoCommit, ReadOnly, FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel]);
end;

procedure TZInterbase6Connection.SetActiveTransaction(
  const Value: IZTransaction);
var Transaction: IZIBTransaction;
  SavePoint: IFB_IB_SavePoint;
begin
  SavePoint := nil;
  if (Value = nil) or (Value.QueryInterface(IZIBTransaction, Transaction) <> S_OK)
    or (Value.QueryInterface(IFB_IB_SavePoint, SavePoint) <> S_OK) then
    raise EZSQLException.Create('invalid IB/FB transaction');
  if SavePoint <> nil then
    Transaction := SavePoint.GetOwnerTransaction;
  fActiveTransaction[Transaction.IsReadOnly] := Transaction;
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
procedure TZInterbase6Connection.SetAutoCommit(Value: Boolean);
begin
  FRestartTransaction := not Value;
  if (Value <> AutoCommit) then begin
    TransactionParameterPufferChanged;
    AutoCommit := Value;
    //restart automatically happens on GetTrHandle
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
procedure TZInterbase6Connection.SetReadOnly(Value: Boolean);
begin
  if (ReadOnly <> Value) then begin
    ReadOnly := Value;
    //restart automatically happens on GetTrHandle
  end;
end;

{ TZInterbase6AbstractGUIDProps }

destructor TZInterbase6AbstractGUIDProps.Destroy;
begin
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  inherited;
end;

procedure TZInterbase6AbstractGUIDProps.InternalInit(const OptionByType, OptionDomains, OptionFields: string);
begin
  // Cleanup
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  FDetectFlags := [];

  if StrToBoolEx(OptionByType) then
    Include(FDetectFlags, gfByType);
  if OptionDomains <> '' then
  begin
    Include(FDetectFlags, gfByDomain);
    FDomains := ExtractFields(OptionDomains, [';', ',']);
  end;
  if OptionFields <> '' then
  begin
    Include(FDetectFlags, gfByFieldName);
    FFields := ExtractFields(OptionFields, [';', ',']);
  end;
end;

{**
  Determines if a column could have GUID / UUID type (SQLType = stBytes and DataSize = 16)
  @param  SQL column type
  @param  length of column data
  @return True if domain could have GUID type
}
function TZInterbase6AbstractGUIDProps.ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
begin
  Result := (SQLType = stBytes) and (DataSize = 16);
end;

{**
  Determines if a column has GUID / UUID type.
  @param  SQL column type             (used if GUID is determined by type)
  @param  length of column data       (used if GUID is determined by type)
  @param  domain name                 (used if GUID is determined by domain name)
  @param  column name                 (used if GUID is determined by field name)
  @return True if column must have GUID type according to connection properties.
}
function TZInterbase6AbstractGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
begin
  if ColumnCouldBeGUID(SQLType, DataSize) then
  begin
    // Perform checking by descending importance, the first positive result breaks the chain
    if (gfByFieldName in FDetectFlags) and (ColumnName <> '') then
    begin
      Result := (FFields.IndexOf(ColumnName) <> -1);
      if Result then Exit;
    end;
    if (gfByDomain in FDetectFlags) and (ColumnDomain <> '') then
    begin
      Result := (FDomains.IndexOf(ColumnDomain) <> -1);
      if Result then Exit;
    end;
    Result := (gfByType in FDetectFlags);
  end
  else
    Result := False;
end;

{ TZInterbase6ConnectionGUIDProps }

{**
  For use from this unit only.
  Reads GUID-related values from Properties and inits internal fields.
}
procedure TZInterbase6ConnectionGUIDProps.InitFromProps(Properties: TStrings);
begin
  InternalInit(
    Properties.Values[ConnProps_SetGUIDByType],
    Properties.Values[ConnProps_GUIDDomains],
    Properties.Values[DSProps_GUIDFields]
  );
end;

function TZInterbase6ConnectionGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, ColumnDomain, ColumnName);
end;

{ TZInterbase6StatementGUIDProps }

{**
  For use from outside the unit.
  Creates an object based on Statement and Connection properties.
  The object should be re-created every time a Statement is opened.
}
constructor TZInterbase6StatementGUIDProps.Create(const Statement: IZStatement);
begin
  inherited Create;
  InternalInit(
    DefineStatementParameter(Statement, DSProps_SetGUIDByType, ''),
    '', // Domain info is useless when object is created based on Statement
    DefineStatementParameter(Statement, DSProps_GUIDFields, '') );
end;

function TZInterbase6StatementGUIDProps.ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, '', ColumnName);
end;

{ TZIBTransaction }

procedure TZIBTransaction.BeforeDestruction;
begin
  FOpenCursors.Clear;
  FOpenUncachedLobs.Clear;
  fSavepoints.Clear;
  if FTrHandle <> 0 then
    if fDoCommit
    then Commit
    else RollBack;
  FreeAndNil(fSavepoints);
  FreeAndNil(FOpenCursors);
  FreeAndNil(FOpenUncachedLobs);
  inherited BeforeDestruction;
end;

procedure TZIBTransaction.CloseTransaction;
begin
  fSavepoints.Clear;
  if FTrHandle <> 0 then
    if FOwner.AutoCommit
    then Commit
    else RollBack;
end;

procedure TZIBTransaction.Commit;
var Status: ISC_STATUS;
  S: RawByteString;
begin
  if fSavepoints.Count > 0 then begin
    S := 'RELEASE SAVEPOINT '+ {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    FOwner.ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
    FExplicitTransactionCounter := fSavepoints.Count +1;
  end else if FTrHandle <> 0 then with FOwner do try
    if FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then Status := FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle)
    else begin
      fDoCommit := True;
      fDoLog := False;
      Status := FPlainDriver.isc_commit_retaining(@FStatusVector, @FTrHandle);
      ReleaseTransaction(Self);
    end;
    FExplicitTransactionCounter := 0;
    if Status <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION COMMIT');
  end;
end;

constructor TZIBTransaction.Create(const Owner: TZInterbase6Connection;
  AutoCommit, ReadOnly: Boolean; const TPB: RawByteString);
begin
  FOwner := Owner;
  FOpenCursors := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FOpenUncachedLobs := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  fTPB := TPB;
  fTEB.tpb_length := Length(fTPB){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  fTEB.tpb_address := Pointer(fTPB);
  fSavepoints := TStringList.Create;
  fDoLog := True;
  FReadOnly := ReadOnly;
  FAutoCommit := AutoCommit;
  ConSettings := Owner.ConSettings;
end;

procedure TZIBTransaction.DeRegisterOpencursor(const CursorRS: IZResultSet);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenCursors <> nil, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  I := FOpenCursors.IndexOf(Pointer(CursorRS));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  FOpenCursors.Delete(I);
end;

procedure TZIBTransaction.DeRegisterOpenUnCachedLob(const Lob: IZlob);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenUncachedLobs <> nil, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  I := FOpenUncachedLobs.IndexOf(Pointer(Lob));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  FOpenUncachedLobs.Delete(I);
end;

function TZIBTransaction.GetTransactionLevel: Integer;
begin
  Result := FExplicitTransactionCounter;
end;

function TZIBTransaction.GetOpenCursorCount: Integer;
begin
  Result := FOpenCursors.Count;
end;

function TZIBTransaction.GetTPB: RawByteString;
begin
  Result := FTPB;
end;

function TZIBTransaction.GetTrHandle: PISC_TR_HANDLE;
begin
  if FTrHandle = 0 then
    StartTransaction;
  Result := @FTrHandle
end;

function TZIBTransaction.IsAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

function TZIBTransaction.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TZIBTransaction.RegisterOpencursor(const CursorRS: IZResultSet);
begin
  FOpenCursors.Add(Pointer(CursorRS));
end;

procedure TZIBTransaction.RegisterOpenUnCachedLob(const Lob: IZlob);
begin
  FOpenUncachedLobs.Add(Pointer(Lob));
end;

procedure TZIBTransaction.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var I: Integer;
  ImmediatelyReleasable: IImmediatelyReleasable;
begin
  FTrHandle := 0;
  fSavepoints.Clear;
  I := FOwner.FTransactions[FReadOnly].IndexOf(Self);
  if I > -1 then
    FOwner.FTransactions[FReadOnly].Delete(I);
  for i := FOpenUncachedLobs.Count -1 downto 0 do
    if Supports(IZBlob(FOpenUncachedLobs[i]), IImmediatelyReleasable, ImmediatelyReleasable) and
       (Sender <> ImmediatelyReleasable) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
end;

procedure TZIBTransaction.Rollback;
var Status: ISC_STATUS;
  S: RawByteString;
begin
  if fSavepoints.Count > 0 then begin
    S := 'ROLLBACK TO '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    FOwner.ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTrHandle <> 0 then with FOwner do try
    if FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then Status := FPlainDriver.isc_rollback_transaction(@FStatusVector, @FTrHandle)
    else begin
      fDoCommit := False;
      fDoLog := False;
      Status := FPlainDriver.isc_rollback_retaining(@FStatusVector, @FTrHandle);
      ReleaseTransaction(Self);
    end;
    FExplicitTransactionCounter := 0;
    if Status <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION ROLLBACK');
  end;
end;

function TZIBTransaction.StartTransaction: Integer;
var S: String;
begin
  if FTrHandle = 0 then with FOwner do begin
    Result := Ord(not AutoCommit);
    fTEB.db_handle := @FHandle;
    if FPlainDriver.isc_start_multiple(@FStatusVector, @FTrHandle, 1, @fTEB) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner, lcTransaction);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION STARTED.');
  end else begin
    Result := FSavePoints.Count+2;
    S := 'SP'+ZFastcode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result);
    FOwner.ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
  FExplicitTransactionCounter := Result;
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

{ TZInterbase6Sequence }

function TZInterbase6Sequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT GEN_ID('+FName+', 0) FROM RDB$DATABASE';
end;

function TZInterbase6Sequence.GetNextValueSQL: string;
begin
  with Connection.GetMetadata do begin
    if (GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsNextValueFor and (FBlockSize = 1)
    then Result := ' NEXT VALUE FOR '+FName
    else Result := ' GEN_ID('+FName+', '+ZFastcode.IntToStr(FBlockSize)+') ';
    Result := 'SELECT '+Result+' FROM RDB$DATABASE';
  end;
end;

procedure TZInterbase6Sequence.SetBlockSize(const Value: Integer);
begin
  if Value <> fBlockSize then begin
    FlushResults;
    inherited SetBlockSize(Value);
  end;
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
