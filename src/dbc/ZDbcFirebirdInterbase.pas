{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{             Originally written by EgonHugeist           }
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

unit ZDbcFirebirdInterbase;

interface

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZCollections, ZClasses, ZCompatibility,
  ZDbcIntfs, ZDbcConnection, ZTokenizer, ZGenericSqlAnalyser, ZDbcCache,
  ZDbcGenericResolver, ZDbcResultSetMetadata, ZDbcCachedResultSet,
  ZPlainFirebirdInterbaseConstants, ZPlainFirebirdInterbaseDriver,
  ZDbcResultSet;
type
  {** Implements Interbase or Firebird Database Driver. }
  TZInterbaseFirebirdDriver = class(TZAbstractDriver)
  public
    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  TZInterbaseFirebirdConnectionGUIDProps = class;

  IZInterbaseFirebirdTransaction = interface(IZTransaction)
    ['{A30246BA-AFC0-43FF-AB56-AB272281A3C2}']
    procedure CloseTransaction;
    procedure DoStartTransaction;
    procedure RegisterOpencursor(const CursorRS: IZResultSet);
    procedure RegisterOpenUnCachedLob(const Lob: IZlob);
    procedure DeRegisterOpenCursor(const CursorRS: IZResultSet);
    procedure DeRegisterOpenUnCachedLob(const Lob: IZlob);
    function GetTransactionLevel: Integer;
    function GetOpenCursorCount: Integer;
    function GetTPB: RawByteString;
    function IsReadOnly: Boolean;
    function IsAutoCommit: Boolean;
    function StartTransaction: Integer;
  end;

  {** Represents a Interbase specific connection interface. }
  IZInterbaseFirebirdConnection = interface (IZConnection)
    ['{B4B4136F-3692-454A-8F22-6C5EEE247BC0}']
    function GetDialect: Word;
    function GetXSQLDAMaxSize: Cardinal;
    function GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetSubTypeTextCharSetID(const TableName, ColumnName: String): Integer;
    function IsFirebirdLib: Boolean;
    function IsInterbaseLib: Boolean;
    function GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
  end;

  TZInterbaseFirebirdConnection = Class(TZAbstractDbcConnection)
  private
    FWeakTransactionManagerPtr: Pointer;
  protected
    FHardCommit: boolean;
    FDialect: Word;
    FProcedureTypesCache, FSubTypeTestCharIDCache: TStrings;
    FGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    FTPBs: array[Boolean,Boolean,TZTransactIsolationLevel] of RawByteString;
    fTransactions: array[Boolean] of IZCollection; //simultan (not nested) readonly/readwrite transaction container
    fActiveTransaction: array[Boolean] of IZInterbaseFirebirdTransaction;
    FPB_CP: Word; //the parameter buffer codepage
    FClientVersion: Integer;
    FHostVersion: Integer;
    FXSQLDAMaxSize: Cardinal;
    FInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
    procedure BeforeUrlAssign; override;
    procedure DetermineClientTypeAndVersion; virtual; abstract;
    procedure AssignISC_Parameters;
    function GenerateTPB(AutoCommit, ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
      Info: TStrings): RawByteString;
    procedure TransactionParameterPufferChanged;
    function GetActiveTransaction: IZInterbaseFirebirdTransaction;
    procedure OnPropertiesChange({%H-}Sender: TObject); override;
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public { IZTransactionManager }
    procedure ReleaseTransaction(const Transaction: IZTransaction);
    procedure SetActiveTransaction(const Value: IZTransaction);
  public
    function IsFirebirdLib: Boolean; virtual; abstract;
    function IsInterbaseLib: Boolean; virtual; abstract;
    function GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetSubTypeTextCharSetID(const TableName, ColumnName: String): Integer;
    function GetDialect: Word;
    function GetXSQLDAMaxSize: Cardinal;
    function GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
  public
    procedure Commit;
    procedure Rollback;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetAutoCommit(Value: Boolean); override;
    function StartTransaction: Integer;
  public
    function GetServerProvider: TZServerProvider; override;
    function GetBinaryEscapeString(const Value: TBytes): String; override;
    function GetHostVersion: Integer; override;
    function GetClientVersion: Integer; override;
  public
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;
  End;

  {** EH: implements a IB/FB transaction }
  TZInterbaseFirebirdTransaction = class(TZCodePagedObject, IImmediatelyReleasable)
  private
    FWeakIZTransactionPtr: Pointer;
  protected
    fSavepoints: TStrings;
    fDoCommit, fDoLog: Boolean;
    FOpenCursors, FOpenUncachedLobs: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FReadOnly, FAutoCommit: Boolean;
    FTPB: RawByteString;
    FExplicitTransactionCounter: Integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZInterbaseFirebirdConnection;
    function TestCachedResultsAndForceFetchAll: Boolean;
  protected
    function TxnIsStarted: Boolean; virtual; abstract;
  public { IZInterbaseFirebirdTransaction }
    procedure CloseTransaction;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
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
    constructor Create(const Owner: TZInterbaseFirebirdConnection; AutoCommit, ReadOnly: Boolean;
      const TPB: RawByteString);
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

  {** Implements a Interbase6/Firebird sequence. }
  TZInterbaseFirebirdSequence = class(TZIdentifierSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
    procedure SetBlockSize(const Value: Integer); override;
  end;

  {** Implements Interbase ResultSetMetadata object. }
  TZInterbaseFirebirdResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
    procedure SetColumnTypeFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  End;

  {** Implements a specialized cached resolver for Interbase/Firebird. }
  TZInterbaseFirebirdCachedResolver = class(TZGenerateSQLCachedResolver)
  private
    FInsertReturningFields: TStrings;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): string; override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
  end;

  {** Implements a specialized cached resolver for Firebird version 2.0 and up. }
  TZFirebird2upCachedResolver = class(TZInterbaseFirebirdCachedResolver)
  public
    procedure FormWhereClause(const SQLWriter: TZSQLStringWriter;
      const OldRowAccessor: TZRowAccessor; var Result: SQLString); override;
  end;

  IZInterbaseFirebirdSavePoint = interface
    ['{96D74A82-A1ED-4190-9CF1-A969BF73A1E9}']
    function GetOwnerTransaction: IZInterbaseFirebirdTransaction;
  end;

  TZInterbaseFirebirdColumnInfo = Class(TZColumnInfo)
    sqldata: Pointer; //points to data in our buffer
    sqlind: PISC_SHORT; //null indicator, nil if not nullable
    sqltype: Cardinal;
    sqlsubtype: Cardinal;
    sqlscale: Integer;
  public
    function GetPCharFromTextVar(out Len: NativeUInt): PAnsiChar; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  End;

  TZInterbaseFirebirdStatementGUIDProps = class;

  TZAbstractInterbaseFirebirdResultSet = Class(TZAbstractReadOnlyResultSet)
  protected
    FGUIDProps: TZInterbaseFirebirdStatementGUIDProps;
    FIsMetadataResultSet: Boolean;
  public
    //EH: this field is a weak resultset reference
    //it may be an address of a cached resultset which owns this instance.
    //this pointer should be registered as open cursor on the TA
    //aim is: if a transaction commit is called the TA checks if
    //all open resultsets a scollable. If so a fetchall will be done by TA.
    //finally the TA can commit the handle (i.e. changing the AutoCommit mode)
    //which would ususally close all pending cursors
    TransactionResultSet: Pointer;
    procedure AfterClose; override;
    constructor Create(const Statement: IZStatement; const SQL: string);
  public
    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  end;

  TGUIDDetectFlag = (gfByType, gfByDomain, gfByFieldName);
  TGUIDDetectFlags = set of TGUIDDetectFlag;

  {** Implements GUID detection options/properties }

  TZInterbaseFirebirdAbstractGUIDProps = class //constributed by Fr0st
  private
    FDetectFlags: TGUIDDetectFlags;
    FDomains: TStrings;
    FFields: TStrings;
  public // to access from descendants
    procedure InternalInit(const OptionByType, OptionDomains, OptionFields: string);
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  public
    destructor Destroy; override;
    function ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
  end;

  // Reusable object intended for use on Connection level. Allows re-initialization
  // if Connection properties are changed. Uses Connection properties only
  //constributed by Fr0st
  TZInterbaseFirebirdConnectionGUIDProps = class(TZInterbaseFirebirdAbstractGUIDProps)
  public // to access from other classes of the unit
    procedure InitFromProps(Properties: TStrings);
  public
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  end;

  // Temporary object intended for use on Statement level. Should be re-created
  // whenever a Statement is opened. Uses Statement & Connection properties.
  // Doesn't consider domain info (there's no domains on Statement level)
   //constributed by Fr0st
  TZInterbaseFirebirdStatementGUIDProps = class(TZInterbaseFirebirdAbstractGUIDProps)
  public
    constructor Create(const Statement: IZStatement); overload;
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnName: string): Boolean;
  end;

const
  sCS_NONE = 'NONE';
  DS_Props_IsMetadataResultSet = 'IsMetadataResultSet';
  sCommitMsg = RawByteString('TRANSACTION COMMIT');
  sRollbackMsg = RawByteString('TRANSACTION ROLLBACK');

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

uses ZSysUtils, ZFastCode, ZEncoding, ZMessages,
  ZInterbaseToken, ZInterbaseAnalyser,
  ZDbcUtils, ZDbcMetadata, ZDbcProperties, ZDbcInterbase6Utils,
  ZDbcInterbaseFirebirdMetadata;

{ TZInterbaseFirebirdDriver }

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZInterbaseFirebirdDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZInterbaseStatementAnalyser.Create;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZInterbaseFirebirdDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZInterbaseTokenizer.Create;
end;

{ TZInterbaseFirebirdConnection }

procedure TZInterbaseFirebirdConnection.AfterConstruction;
var TAManager: IZTransactionManager;
begin
  QueryInterface(IZTransactionManager, TAManager);
  FWeakTransactionManagerPtr := Pointer(TAManager);
  TAManager := nil;
  inherited AfterConstruction;
end;

procedure TZInterbaseFirebirdConnection.AssignISC_Parameters;
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
    Info.Values['isc_dpb_sql_dialect'] := ZFastCode.IntToStr(FDialect);

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

procedure TZInterbaseFirebirdConnection.BeforeUrlAssign;
begin
  // ! Create the object before parent's constructor because it is used in
  // TZAbstractDbcConnection.Create > Url.OnPropertiesChange
  FGUIDProps := TZInterbaseFirebirdConnectionGUIDProps.Create;
  FClientVersion := -1;
  inherited BeforeUrlAssign;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZInterbaseFirebirdConnection.Commit;
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
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZInterbaseFirebirdConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZInterbaseFirebirdSequence.Create(
    IZConnection(fWeakReferenceOfSelfInterface), Sequence, BlockSize);
end;

destructor TZInterbaseFirebirdConnection.Destroy;
begin
  FreeAndNil(FProcedureTypesCache);
  FreeAndNil(FGUIDProps);
  FreeAndNil(FSubTypeTestCharIDCache);
  inherited Destroy;
end;

const
  Tpb_Access: array[boolean] of String = ('isc_tpb_write','isc_tpb_read');
  tpb_AutoCommit: array[boolean] of String = ('','isc_tpb_autocommit');
function TZInterbaseFirebirdConnection.GenerateTPB(AutoCommit,
  ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
  Info: TStrings): RawByteString;
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

    Result := BuildPB(Params, isc_tpb_version3, TPBPrefix, TransactionParams, ConSettings, FPB_CP);
  finally
    FreeAndNil(Params);
  end;
end;

function TZInterbaseFirebirdConnection.GetActiveTransaction: IZInterbaseFirebirdTransaction;
var TA: IZTransaction;
begin
  if not IsClosed then begin
    if fActiveTransaction[ReadOnly] = nil then begin
      TA := IZTransactionManager(FWeakTransactionManagerPtr).CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction[ReadOnly]);
      fTransactions[ReadOnly].Add(TA);
    end;
    Result := fActiveTransaction[ReadOnly];
  end else
    Result := nil;
end;

function TZInterbaseFirebirdConnection.GetBinaryEscapeString(
  const Value: TBytes): String;
begin
  //http://tracker.firebirdsql.org/browse/CORE-2789
  if (GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsBinaryInSQL
  then Result := GetSQLHexString(PAnsiChar(Value), Length(Value))
  else raise Exception.Create('Your Firebird-Version does''t support Binary-Data in SQL-Statements! Use parameters!');
end;

{**
  Gets the client's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZInterbaseFirebirdConnection.GetClientVersion: Integer;
begin
  if FClientVersion = -1 then DetermineClientTypeAndVersion;
  Result := FClientVersion;
end;

{**
   Return Interbase dialect number. Dialect a dialect Interbase SQL
   must be 1 or 2 or 3.
   @return dialect number
}
function TZInterbaseFirebirdConnection.GetDialect: Word;
begin
  Result := FDialect;
end;

function TZInterbaseFirebirdConnection.GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
begin
  Result := FGUIDProps;
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZInterbaseFirebirdConnection.GetHostVersion: Integer;
begin
  Result := FHostVersion;
end;

function TZInterbaseFirebirdConnection.GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
begin
  Result := FInterbaseFirebirdPlainDriver;
end;

function TZInterbaseFirebirdConnection.GetServerProvider: TZServerProvider;
begin
  Result := spIB_FB;
end;

function TZInterbaseFirebirdConnection.GetSubTypeTextCharSetID(const TableName,
  ColumnName: String): Integer;
var S: String;
  function GetFromMetaData: Integer;
  var Stmt: IZStatement;
    RS: IZResultSet;
  begin
    Stmt := CreateStatement;
    RS := Stmt.ExecuteQuery('SELECT F.RDB$CHARACTER_SET_ID '+LineEnding+
      'FROM RDB$RELATION_FIELDS R'+LineEnding+
      'INNER JOIN RDB$FIELDS F on R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME'+LineEnding+
      'WHERE R.RDB$RELATION_NAME = '+QuotedStr(TableName)+' and R.RDB$FIELD_NAME = '+QuotedStr(ColumnName));
    if RS.Next
    then Result := RS.GetInt(FirstDbcIndex)
    else Result := ConSettings.ClientCodePage.ID;
    RS.Close;
    RS := nil;
    Stmt.Close;
    Stmt := Nil;
  end;
begin
  S := TableName+'/'+ColumnName;
  Result := FSubTypeTestCharIDCache.IndexOf(S);
  if Result < 0 then begin
    Result := GetFromMetaData;
    FSubTypeTestCharIDCache.AddObject(S, TObject(Result));
  end else
    Result := Integer(FSubTypeTestCharIDCache.Objects[Result]);
end;

function TZInterbaseFirebirdConnection.GetXSQLDAMaxSize: Cardinal;
begin
  Result := FXSQLDAMaxSize;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZInterbaseFirebirdConnection.InternalClose;
var B: Boolean;
begin
  for B := False to True do begin
    fTransactions[b].Clear;
    fActiveTransaction[b] := nil;
  end;
end;

procedure TZInterbaseFirebirdConnection.InternalCreate;
begin
  FMetadata := TZInterbase6DatabaseMetadata.Create(Self, Url);
  fTransactions[False] := TZCollection.Create;
  fTransactions[True] := TZCollection.Create;
  { set default sql dialect it can be overriden }
  FDialect := StrToIntDef(Info.Values[ConnProps_Dialect], SQL_DIALECT_CURRENT);
  FProcedureTypesCache := TStringList.Create;
  FSubTypeTestCharIDCache := TStringList.Create;
  FInterbaseFirebirdPlainDriver := TZInterbaseFirebirdPlainDriver(GetIZPlainDriver.GetInstance);
end;

procedure TZInterbaseFirebirdConnection.OnPropertiesChange(Sender: TObject);
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
    If not IsClosed then
      FTPBs[AutoCommit][RO][TransactIsolationLevel] := GenerateTPB(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
    if (fActiveTransaction[RO] <> nil) and ((HC <> FHardCommit) or
       (fActiveTransaction[RO].GetTPB <> FTPBs[AutoCommit][RO][TransactIsolationLevel])) then//*** ADDED THIS CHECK by EMartin ***
      TransactionParameterPufferChanged;
  end;
  FHardCommit := HC;
end;

procedure TZInterbaseFirebirdConnection.ReleaseTransaction(
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
procedure TZInterbaseFirebirdConnection.Rollback;
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

procedure TZInterbaseFirebirdConnection.SetActiveTransaction(
  const Value: IZTransaction);
var Transaction: IZInterbaseFirebirdTransaction;
  SavePoint: IZInterbaseFirebirdSavePoint;
begin
  SavePoint := nil;
  Transaction := nil;
  if (Value = nil) or (Value.QueryInterface(IZInterbaseFirebirdConnection, Transaction) <> S_OK)
    or (Value.QueryInterface(IZInterbaseFirebirdSavePoint, SavePoint) <> S_OK) then
    raise EZSQLException.Create('invalid IB/FB transaction');
  if (SavePoint <> nil) then
    SavePoint.GetOwnerTransaction.QueryInterface(IZInterbaseFirebirdTransaction, Transaction);
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
procedure TZInterbaseFirebirdConnection.SetAutoCommit(Value: Boolean);
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
procedure TZInterbaseFirebirdConnection.SetReadOnly(Value: Boolean);
begin
  if (ReadOnly <> Value) then begin
    TransactionParameterPufferChanged;
    ReadOnly := Value;
    //restart automatically happens on GetTrHandle
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
procedure TZInterbaseFirebirdConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if (Level <> TransactIsolationLevel) then begin
    TransactionParameterPufferChanged;
    TransactIsolationLevel := Level;
    //restart automatically happens on GetTrHandle
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
function TZInterbaseFirebirdConnection.StartTransaction: Integer;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    AutoCommit := False;
    TransactionParameterPufferChanged;
  end else GetActiveTransaction.DoStartTransaction;
  Result := GetActiveTransaction.StartTransaction;
end;

function TZInterbaseFirebirdConnection.StoredProcedureIsSelectable(
  const ProcName: String): Boolean;
var I: Integer;
  function AddToCache(const ProcName: String): Boolean;
  var RS: IZResultSet;
    Stmt: IZStatement;
  begin
    Result := False;
    Stmt := IZConnection(fWeakReferenceOfSelfInterface).CreateStatementWithParams(Info);
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

procedure TZInterbaseFirebirdConnection.TransactionParameterPufferChanged;
begin
  if (fActiveTransaction[ReadOnly] <> nil) then begin
    fActiveTransaction[ReadOnly].CloseTransaction;
    if (fActiveTransaction[ReadOnly] <> nil) then
      ReleaseTransaction(fActiveTransaction[ReadOnly]);
  end;
end;

{ TZInterbaseFirebirdTransaction }

procedure TZInterbaseFirebirdTransaction.AfterConstruction;
var Trans: IZTransaction;
begin
  QueryInterface(IZTransaction, Trans);
  FWeakIZTransactionPtr := Pointer(Trans);
  Trans := nil;
  inherited;
end;

procedure TZInterbaseFirebirdTransaction.BeforeDestruction;
begin
  FOpenCursors.Clear;
  FOpenUncachedLobs.Clear;
  fSavepoints.Clear;
  if TxnIsStarted then
    if fDoCommit
    then IZTransaction(FWeakIZTransactionPtr).Commit
    else IZTransaction(FWeakIZTransactionPtr).RollBack;
  FreeAndNil(fSavepoints);
  FreeAndNil(FOpenCursors);
  FreeAndNil(FOpenUncachedLobs);
  inherited BeforeDestruction;
end;

procedure TZInterbaseFirebirdTransaction.CloseTransaction;
begin
  fSavepoints.Clear;
  if TxnIsStarted then
    if FOwner.AutoCommit
    then IZTransaction(FWeakIZTransactionPtr).Commit
    else IZTransaction(FWeakIZTransactionPtr).RollBack;
end;

constructor TZInterbaseFirebirdTransaction.Create(const Owner: TZInterbaseFirebirdConnection;
  AutoCommit, ReadOnly: Boolean; const TPB: RawByteString);
begin
  FOwner := Owner;
  FOpenCursors := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FOpenUncachedLobs := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  fTPB := TPB;
  fSavepoints := TStringList.Create;
  fDoLog := True;
  FReadOnly := ReadOnly;
  FAutoCommit := AutoCommit;
  ConSettings := Owner.ConSettings;
end;

procedure TZInterbaseFirebirdTransaction.DeRegisterOpencursor(const CursorRS: IZResultSet);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenCursors <> nil, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  I := FOpenCursors.IndexOf(Pointer(CursorRS));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  FOpenCursors.Delete(I);
end;

procedure TZInterbaseFirebirdTransaction.DeRegisterOpenUnCachedLob(const Lob: IZlob);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenUncachedLobs <> nil, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  I := FOpenUncachedLobs.IndexOf(Pointer(Lob));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  FOpenUncachedLobs.Delete(I);
end;

function TZInterbaseFirebirdTransaction.GetTransactionLevel: Integer;
begin
  Result := FExplicitTransactionCounter;
end;

function TZInterbaseFirebirdTransaction.GetOpenCursorCount: Integer;
begin
  Result := FOpenCursors.Count;
end;

function TZInterbaseFirebirdTransaction.GetTPB: RawByteString;
begin
  Result := FTPB;
end;

function TZInterbaseFirebirdTransaction.IsAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

function TZInterbaseFirebirdTransaction.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TZInterbaseFirebirdTransaction.RegisterOpencursor(const CursorRS: IZResultSet);
begin
  FOpenCursors.Add(Pointer(CursorRS));
end;

procedure TZInterbaseFirebirdTransaction.RegisterOpenUnCachedLob(const Lob: IZlob);
begin
  FOpenUncachedLobs.Add(Pointer(Lob));
end;

procedure TZInterbaseFirebirdTransaction.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var I: Integer;
  ImmediatelyReleasable: IImmediatelyReleasable;
begin
  fSavepoints.Clear;
  I := FOwner.FTransactions[FReadOnly].IndexOf(Self);
  if I > -1 then
    FOwner.FTransactions[FReadOnly].Delete(I);
  for i := FOpenUncachedLobs.Count -1 downto 0 do
    if Supports(IZBlob(FOpenUncachedLobs[i]), IImmediatelyReleasable, ImmediatelyReleasable) and
       (Sender <> ImmediatelyReleasable) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
end;

function TZInterbaseFirebirdTransaction.TestCachedResultsAndForceFetchAll: Boolean;
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

{ TZInterbaseFirebirdSequence }

function TZInterbaseFirebirdSequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT GEN_ID('+FName+', 0) FROM RDB$DATABASE';
end;

function TZInterbaseFirebirdSequence.GetNextValueSQL: string;
begin
  with Connection.GetMetadata do begin
    if (GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsNextValueFor and (FBlockSize = 1)
    then Result := ' NEXT VALUE FOR '+FName
    else Result := ' GEN_ID('+FName+', '+ZFastcode.IntToStr(FBlockSize)+') ';
    Result := 'SELECT '+Result+' FROM RDB$DATABASE';
  end;
end;

procedure TZInterbaseFirebirdSequence.SetBlockSize(const Value: Integer);
begin
  if Value <> fBlockSize then begin
    FlushResults;
    inherited SetBlockSize(Value);
  end;
end;

{ TZInterbaseFirebirdResultSetMetadata }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZInterbaseFirebirdResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZInterbaseFirebirdResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZInterbaseFirebirdResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZInterbaseFirebirdResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZInterbaseFirebirdResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbaseFirebirdResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := False; //not supported by FB/IB
end;

{**
  Initializes columns with additional data.
}
procedure TZInterbaseFirebirdResultSetMetadata.LoadColumns;
{$IFNDEF ZEOS_TEST_ONLY}
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
{$ENDIF}
begin
  {$IFDEF ZEOS_TEST_ONLY}
  inherited LoadColumns;
  {$ELSE}
  if Metadata.GetConnection.GetDriver.GetStatementAnalyser.DefineSelectSchemaFromQuery(Metadata.GetConnection.GetDriver.GetTokenizer, SQL) <> nil then
    for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
      Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
      ClearColumn(Current);
      if Current.TableName = '' then
        continue;
      TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(Metadata.GetIdentifierConvertor.Quote(Current.TableName)),'');
      if TableColumns <> nil then begin
        TableColumns.BeforeFirst;
        while TableColumns.Next do
          if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
            FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
            Break;
          end;
      end;
    end;
  Loaded := True;
  {$ENDIF}
end;

procedure TZInterbaseFirebirdResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
end;

procedure TZInterbaseFirebirdResultSetMetadata.SetColumnTypeFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
var Precision: Integer;
begin
  //FB native ResultSet can't give use users choosen precision for the Numeric/Decimal Fields
  //so a ISC_INT64 type with scale smaller then four will always be
  //mapped to stBigDecimal while it could be a stCurrency type. Let's test it!
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then begin
    Precision := TableColumns.GetInt(TableColColumnSizeIndex);
    if (ColumnInfo.ColumnType = stBigDecimal) and (ColumnInfo.Scale <= 4) and
       (Precision < sAlignCurrencyScale2Precision[ColumnInfo.Scale]) then
      ColumnInfo.ColumnType := stCurrency;
  end else
    inherited SetColumnTypeFromGetColumnsRS(ColumnInfo, TableColumns);
end;

{ TZInterbaseFirebirdCachedResolver }

constructor TZInterbaseFirebirdCachedResolver.Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  Fields: string;
begin
  inherited;
  Fields := Statement.GetParameters.Values[DSProps_InsertReturningFields];
  if Fields <> '' then
    FInsertReturningFields := ExtractFields(Fields, [';', ',']);
end;

destructor TZInterbaseFirebirdCachedResolver.Destroy;
begin
  inherited;
  FreeAndNil(FInsertReturningFields);
end;

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZInterbaseFirebirdCachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): string;
// --> ms, 30/10/2005
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(RowAccessor, ColumnsLookup);
  if Result <> '' then begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0
    then Result := copy(Result, 1, iPos+3) + ' RDB$DATABASE'
    else Result := Result + ' FROM RDB$DATABASE';
  end;
// <-- ms
end;

procedure TZInterbaseFirebirdCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) then
    UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
end;

procedure TZInterbaseFirebirdCachedResolver.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; const
  OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
var
  I, ColumnIdx: Integer;
  RS: IZResultSet;
begin
  //inherited;

  RS := InsertStatement.GetResultSet;
  if RS = nil then
    Exit;

  for I := 0 to FInsertReturningFields.Count - 1 do
  begin
    ColumnIdx := Metadata.FindColumn(FInsertReturningFields[I]);
    if ColumnIdx = InvalidDbcIndex then
      raise EZSQLException.Create(Format(SColumnWasNotFound, [FInsertReturningFields[I]]));
    NewRowAccessor.SetValue(ColumnIdx, RS.GetValueByName(FInsertReturningFields[I]));
  end;

  RS.Close; { Without Close RS keeps circular ref to Statement causing mem leak }
end;

{ TZFirebird2upCachedResolver }

procedure TZFirebird2upCachedResolver.FormWhereClause(
  const SQLWriter: TZSQLStringWriter; const OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, idx: Integer;
  Tmp, S: SQLString;
begin
  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do begin
    idx := PZIndexPair(WhereColumns[I]).ColumnIndex;
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    S := MetaData.GetColumnName(idx);
    Tmp := IdentifierConvertor.Quote(S);
    SQLWriter.AddText(Tmp, Result);
    if (Metadata.IsNullable(Idx) = ntNullable)
    then SQLWriter.AddText(' IS NOT DISTINCT FROM ?', Result)
    else SQLWriter.AddText('=?', Result);
  end;
end;

{ TZInterbaseFirebirdColumnInfo }

function TZInterbaseFirebirdColumnInfo.GetPCharFromTextVar(
  out Len: NativeUInt): PAnsiChar;
begin
  if SQLType = SQL_TEXT then begin
    Result := sqldata;
    if ColumnCodePage = zCP_Binary
    then Len := CharOctedLength
    else Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, CharOctedLength);
  end else begin
    Result := @PISC_VARYING(sqldata).str[0];
    Len := PISC_VARYING(sqldata).strlen;
  end;
end;

{ TZAbstractInterbaseFirebirdResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractInterbaseFirebirdResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var L, H, I: Integer;
    P: Pointer;
    C, SQLCode: ISC_SHORT;
    TempDate: TZTimeStamp;//TCTimeStructure;
  procedure WConvert(P: PAnsiChar; L: ISC_SHORT; CP: word); //no _UStrClr in method
  begin
    FUniTemp := PRawToUnicode(P, L, CP);
    JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp))
  end;
  procedure RaiseConvertError; //no _U/LStrClr in method
  begin
    raise EZIBConvertError.Create(Format(SErrorConvertionField,
      [FIZSQLDA.GetFieldAliasName(C), GetNameSqlType(SQLCode)]));
  end;
  procedure ReadUTF8CLob(const BlobId: TISC_QUAD);
  var Stream: TStream;
    IbStream: TZInterbaseLobStream;
    Clob: IZCLob;
    Buf: PAnsiChar;
    L, R, Size: LongInt;
  begin
    CLob := TZInterbase6Clob.Create(FIBConnection, BlobId, lsmRead, zCP_UTF8, FOpenLobStreams);
    Stream := Clob.GetStream(zCP_UTF8);
    IbStream := Stream as TZInterbaseLobStream;
    Buf := nil;
    try
      IbStream.OpenLob;
      Size := IbStream.BlobInfo.TotalSize;
      if Size = 0 then Exit;
      { read chunked as firebird supports it }
      L := IbStream.BlobInfo.MaxSegmentSize;
      GetMem(Buf, L);
      repeat
        R := Stream.Read(Buf^, L);
        JSONWriter.AddJSONEscape(Buf, R); //is not #0 terminated
        Dec(Size, R);
        if L > Size then
          L := Size;
      until (R = 0){should not happen} or
            (Size = 0){if segmentsize < total};
    finally
      Stream.Free;
      FreeMem(Buf);
      Clob := nil;
    end;
  end;
  procedure ReadAsWCLob(const BlobId: TISC_QUAD; ColumnCodePage: Word);
  var Clob: IZCLob;
    PW: Pointer;
    L: NativeUInt;
  begin
    CLob := TZInterbase6Clob.Create(FIBConnection, BlobId, lsmRead, ColumnCodePage, FOpenLobStreams);
    try
      PW := CLob.GetPWideChar(FUniTemp, L);
      JSONWriter.AddJSONEscapeW(PW, L); //is not #0 terminated
      FUniTemp := '';
    finally
      Clob := nil;
    end;
  end;
  procedure ReadBLob(const BlobId: TISC_QUAD);
  var Blob: IZBLob;
    P: Pointer;
    L: NativeUInt;
  begin
    Blob := TZInterbase6Blob.Create(FIBConnection, BlobId, lsmRead, FOpenLobStreams);
    try
      P := Blob.GetBuffer(FRawTemp, L); //base 64 can not be added in chunks ):
      JSONWriter.WrBase64(P, L, True);
      FRawTemp := '';
    finally
      Blob := nil;
    end;
  end;
begin
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    {$R-}
    with TZInterbaseFirebirdColumnInfo(ColumnsInfo[c]) do
      if (sqlind <> nil) and (sqlind^ = ISC_NULL) then
        if JSONWriter.Expand then begin
          if not (jcsSkipNulls in JSONComposeOptions) then begin
            JSONWriter.AddString(JSONWriter.ColNames[I]);
            JSONWriter.AddShort('null,')
          end;
        end else
          JSONWriter.AddShort('null,')
      else begin
        if JSONWriter.Expand then
          JSONWriter.AddString(JSONWriter.ColNames[I]);
        SQLCode := (sqltype and not(1));
        case SQLCode of
          SQL_VARYING   : if sqlsubtype = CS_BINARY {octets} then
                            JSONWriter.WrBase64(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, True)
                          else begin
                            JSONWriter.Add('"');
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen)
                            else WConvert(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_TEXT      : if sqlsubtype = CS_BINARY then
                            JSONWriter.WrBase64(sqldata, CharOctedLength, True)
                          else begin
                            JSONWriter.Add('"');
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(sqldata, L)
                            else WConvert(sqldata, L, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_D_FLOAT,
          SQL_DOUBLE    : JSONWriter.AddDouble(PDouble(sqldata)^);
          SQL_FLOAT     : JSONWriter.AddSingle(PSingle(sqldata)^);
          SQL_SHORT     : if sqlscale = 0 then
                            JSONWriter.Add(PISC_SHORT(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_LONG      : if sqlscale = 0 then
                            JSONWriter.Add(PISC_LONG(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(PISC_LONG(sqldata)^, @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_INT64     : if (sqlscale = 0) then
                            JSONWriter.Add(PISC_INT64(sqldata)^)
                          else if sqlScale = -4 then
                            JSONWriter.AddCurr64(PISC_INT64(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(PISC_INT64(sqldata)^, @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_TIMESTAMP : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                              TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(@FTinyBuffer[0], True, TempDate.Year, TempDate.Month, TempDate.Day);
                            isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                              TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(@FTinyBuffer[10], True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second, TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_QUAD,
          SQL_BLOB      : begin
                            if SqlSubType = isc_blob_text then begin
                              JSONWriter.Add('"');
                              if ColumnCodePage = zCP_UTF8
                              then ReadUTF8CLob(PISC_QUAD(sqldata)^)
                              else ReadAsWCLob(PISC_QUAD(sqldata)^, ColumnCodePage);
                              JSONWriter.Add('"');
                            end else ReadBlob(PISC_QUAD(sqldata)^);
                          end;
          //SQL_ARRAY     : JSONWriter.AddShort('"Array"');
          SQL_TYPE_TIME : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            end else
                              JSONWriter.Add('"');
                            isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                              TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(@FTinyBuffer[0], True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second,  TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],8+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
          SQL_TYPE_DATE : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_DATE(sqldata)^, TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(@FTinyBuffer[0], True, TempDate.Year, TempDate.Month, Tempdate.Day);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_BOOLEAN   : JSONWriter.AddShort(JSONBool[PISC_BOOLEAN(sqldata)^ <> 0]);
          SQL_BOOLEAN_FB: JSONWriter.AddShort(JSONBool[PISC_BOOLEAN_FB(sqldata)^ <> 0]);
          else          RaiseConvertError;
        end;
        JSONWriter.Add(',');
      end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAbstractInterbaseFirebirdResultSet.AfterClose;
begin
  FreeAndNil(FGUIDProps);
  inherited AfterClose;
end;

constructor TZAbstractInterbaseFirebirdResultSet.Create(
  const Statement: IZStatement; const SQL: string);
var Connection: IZConnection;
begin
  Connection := Statement.GetConnection;
  inherited Create(Statement, SQL, TZInterbaseFirebirdResultSetMetadata.Create(
    Connection.GetMetadata, SQL, Self), Connection.GetConSettings);
  FGUIDProps := TZInterbaseFirebirdStatementGUIDProps.Create(Statement);
  FIsMetadataResultSet := (ConSettings.ClientCodePage.ID = CS_NONE) and
    (Statement.GetParameters.Values[DS_Props_IsMetadataResultSet] = 'True');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in operating system encoding.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractInterbaseFirebirdResultSet.GetAnsiString(
  ColumnIndex: Integer): AnsiString;
var
  P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: AnsiString);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := IZResultSet(FWeakIntfPtrOfSelf).GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(ZOSCodePage, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if not LastWasNull then case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := CharOctedLength;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(P), CharOctedLength);
                          if (ColumnCodePage = ZOSCodePage)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
                        if (ColumnCodePage = ZOSCodePage) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, ZOSCodePage, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(ColumnIndex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_ANSISTRING}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>BigDecimal</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      LastWasNull := True;
      PCardinal(@Result.Precision)^ := 0;
    end else case sqltype of
      SQL_BOOLEAN   : ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN(sqldata)^ <> 0), 0, Result);
      SQL_BOOLEAN_FB: ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN_FB(sqldata)^ <> 0), 0, Result);
      SQL_SHORT     : ScaledOrdinal2Bcd(PISC_SHORT(sqldata)^, Byte(Scale), Result);
      SQL_LONG      : ScaledOrdinal2Bcd(PISC_LONG(sqldata)^, Byte(Scale), Result);
      SQL_INT64     : ScaledOrdinal2Bcd(PISC_INT64(sqldata)^, Byte(Scale), Result);
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        LastWasNull := not TryRawToBCD(P, Len, Result, '.');
                      end;
      SQL_D_FLOAT,
      SQL_DOUBLE,
      SQL_FLOAT,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIME : Double2BCD(GetDouble(ColumnIndex), Result);
      else raise CreateConversionError(ColumnIndex, ColumnType, stBigDecimal);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then
      Result := False
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^ <> 0;
      SQL_FLOAT     : Result := PSingle(sqldata)^ <> 0;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^ <> 0;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^ <> 0;
      SQL_LONG      : Result := PISC_LONG(sqldata)^ <> 0;
      SQL_SHORT     : Result := PISC_SHORT(sqldata)^ <> 0;
      SQL_QUAD,
      SQL_INT64     : Result := PISC_INT64(sqldata)^ <> 0;
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := StrToBoolEx(P, P+Len);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stBoolean);
    end;
  end;
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
  function FromLob(ColumnIndex: Integer; out Len: NativeUInt): PByte;
  var Lob: IZBlob;
  begin
    Lob := IZResultSet(FWeakIntfPtrOfSelf).GetBlob(ColumnIndex);
    Result := Lob.GetBuffer(FRawTemp, Len);
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Result := nil;
      Len := 0;
    end else case sqltype of
      SQL_TEXT: begin
          Result := sqldata;
          Len := CharOctedLength;
        end;
      SQL_VARYING: begin
          Result := @PISC_VARYING(sqldata).str[0];
          Len := PISC_VARYING(sqldata).strlen;
        end;
      SQL_QUAD,
      SQL_BLOB: Result := FromLob(ColumnIndex, Len);
      else begin
        Result := PByte(sqldata);
        Len := CharOctedLength;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZDate</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>zero-padded</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var
  Len: NativeUInt;
  P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then PInt64(@Result.Year)^ := 0
    else case sqltype of
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          Result.Year, Result.Month, Result.Day);
                        Result.IsNegative := False;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^, Result.Year, Result.Month, Result.Day);
                        Result.IsNegative := False;
                      end;
      SQL_TYPE_TIME : PInt64(@Result.Year)^ := 0;
      SQL_TEXT,
      SQL_VARYING: begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result)
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stDate);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^    / IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stDate);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  P: PAnsiChar;
  Len: NativeUInt;
  I64: Int64 absolute Result;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = -4 then
                        I64 := PISC_LONG(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_LONG(sqldata)^ * IBScaleDivisor[-4-sqlscale]
                      else
                        I64 := PISC_LONG(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := Ord(PISC_BOOLEAN(sqldata)^ <> 0);
      SQL_BOOLEAN_FB: Result := Ord(PISC_BOOLEAN_FB(sqldata)^ <> 0);
      SQL_SHORT     : if sqlscale = -4 then
                        I64 := PISC_SHORT(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_SHORT(sqldata)^ * IBScaleDivisor[(-4-sqlscale)]
                      else
                        I64 := PISC_SHORT(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_INT64     : if sqlscale = -4 then
                        I64 := PISC_INT64(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_INT64(sqldata)^ * IBScaleDivisor[-4-sqlscale]
                      else
                        I64 := PISC_INT64(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stCurrency);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^    / IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stFloat);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TGUID</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var P: PAnsiChar;
  Len: NativeUint;
label SetFromPChar, Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then FillChar(Result, SizeOf(TGUID), #0)
    else case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodepage = zCP_Binary)
                        then Len := CharOctedLength
                        else Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                        goto SetFromPChar;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
SetFromPChar:           if (ColumnCodepage = zCP_Binary) and (Len = SizeOf(TGUID))
                        then Move(P^, Result.D1, SizeOf(TGUID))
                        else if (ColumnCodepage <> zCP_Binary) and ((Len = 36) or (Len = 38))
                          then ValidGUIDToBinary(P, @Result.D1)
                          else goto Fail;
                      end;
      else
Fail:     raise CreateConversionError(ColumnIndex, ColumnType, stGUID);

    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPCharFromTextVar(Len);
                        Result := RawToIntDef(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stInteger);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_D_FLOAT,
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToInt64Def(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stLong);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTime</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>zero padded</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var
  P: PAnsiChar;
  Len: NativeUInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
Fill: PCardinal(@Result.Hour)^ := 0;
      PInt64(@Result.Second)^ := 0;
    end else case sqltype of
      SQL_TIMESTAMP : begin
            isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TYPE_DATE : goto Fill;
      SQL_TYPE_TIME : begin
            isc_decode_time(PISC_TIME(sqldata)^,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TEXT, SQL_VARYING: begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stTime);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTimestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero padded</code>
  @exception SQLException if a database access error occurs
}
procedure TZAbstractInterbaseFirebirdResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Pint64(@Result.Year)^ := 0;
      PInt64(@Result.Minute)^ := 0;
      PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
    end else case sqltype of
      SQL_TIMESTAMP :
        begin
          isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
            Result.Year, Result.Month, Result.Day);
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TYPE_DATE :
        begin
          PInt64(@Result.Hour)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_date(PISC_DATE(sqldata)^,
            Result.Year, Result.Month, Result.Day);
          Result.IsNegative := False;
        end;
      SQL_TYPE_TIME :
        begin
          PInt64(@Result.Year)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIME(sqldata)^,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stTimeStamp);
    end;
  end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractInterbaseFirebirdResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  Assert((ColumnIndex >= FirstDbcIndex) and (ColumnIndex <= ColumnsInfo.Count {$IFDEF GENERIC_INDEX}-1{$ENDIF}), 'Index out of Range.');
  {$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    Result := (sqlind <> nil) and (sqlind^ = ISC_NULL)
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length in bytes of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractInterbaseFirebirdResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  TempDate: TZTimeStamp;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): Pointer;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := IZResultSet(FWeakIntfPtrOfSelf).GetBlob( ColumnIndex);
    Result := BlobTemp.GetBuffer(fRawTemp, Len);
  end;
  label set_Results;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Len := 0;
      Result := nil;
    end else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Len := FloatToSQLRaw(PDouble(sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_LONG      : if sqlscale = 0 then begin
                        IntToRaw(PISC_LONG(sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_LONG(sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_FLOAT     : begin
                        Len := FloatToSQLRaw(PSingle(sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if sqlscale = 0 then begin
                        IntToRaw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0 then begin
                        IntToRaw(PISC_INT64(sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_INT64(sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
set_Results:            Len := Result - PAnsiChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_TEXT,
      SQL_VARYING   : Result:= GetPCharFromTextVar(Len);
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := DateTimeToRaw(TempDate.Year, TempDate.Month,
                          TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := DateToRaw(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := TimeToRaw(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else ZDbcUtils.CreateConversionError(ColumnIndex, ColumnType, stString);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length in words of UTF16 string buffer
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  label set_Results;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := IZResultSet(FWeakIntfPtrOfSelf).GetBlob(ColumnIndex, lsmRead);
    if BlobTemp.IsClob then begin
      Result := BlobTemp.GetPWideChar(FUniTemp, Len);
    end else begin
      Result := BlobTemp.GetBuffer(FRawTemp, Len);
      FUniTemp := Ascii7ToUnicodeString(Pointer(Result), Len);
      FRawTemp := '';
      Result := Pointer(FUniTemp);
      Len := Length(FUniTemp);
    end;
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Len := 0;
      Result := nil;
    end else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Len := FloatToSQLUnicode(PDouble(sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_FLOAT     : begin
                        Len := FloatToSQLUnicode(PSingle(sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if sqlscale = 0 then begin
                        IntToUnicode(Integer(PISC_SHORT(sqldata)^), PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(Integer(PISC_SHORT(sqldata)^), PWideChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_LONG      : if sqlscale = 0 then begin
                        IntToUnicode(PISC_LONG(sqldata)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_LONG(sqldata)^, PWideChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0 then begin
                        IntToUnicode(PISC_INT64(sqldata)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_INT64(sqldata)^, PWideChar(@FTinyBuffer[0]), @Result, Byte(-sqlscale));
set_Results:            Len := Result - PWideChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPCharFromTextVar(Len);
                        if ColumnCodePage = CS_BINARY
                        then fUniTemp := Ascii7ToUnicodeString(P, Len)
                        else fUniTemp := PRawToUnicode(P, Len, ColumnCodePage);
                        Len := Length(fUniTemp);
                        if Len <> 0
                        then Result := Pointer(fUniTemp)
                        else Result := PEmptyUnicodeString;
                      end;
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := DateTimeToUni(TempDate.Year,
                          TempDate.Month, TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := DateToUni(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := TimeToUni(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 100000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else ZDbcUtils.CreateConversionError(ColumnIndex, ColumnType, stUnicodeString);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>usigned 32bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>unsigned 64bit integer</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractInterbaseFirebirdResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToUInt64Def(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stULong);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractInterbaseFirebirdResultSet.GetUTF8String(
  ColumnIndex: Integer): UTF8String;
var P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: UTF8String);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := IZResultSet(FWeakIntfPtrOfSelf).GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(zCP_UTF8, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := ''
    else case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := CharOctedLength;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                          if (ColumnCodePage = zCP_UTF8)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
                        if (ColumnCodePage = zCP_UTF8) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, zCP_UTF8, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(Columnindex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_UTF8STRING}

{ TZInterbaseFirebirdAbstractGUIDProps }

destructor TZInterbaseFirebirdAbstractGUIDProps.Destroy;
begin
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  inherited;
end;

procedure TZInterbaseFirebirdAbstractGUIDProps.InternalInit(const OptionByType, OptionDomains, OptionFields: string);
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
function TZInterbaseFirebirdAbstractGUIDProps.ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
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
function TZInterbaseFirebirdAbstractGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
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

{ TZInterbaseFirebirdConnectionGUIDProps }

{**
  For use from this unit only.
  Reads GUID-related values from Properties and inits internal fields.
}
procedure TZInterbaseFirebirdConnectionGUIDProps.InitFromProps(Properties: TStrings);
begin
  InternalInit(
    Properties.Values[ConnProps_SetGUIDByType],
    Properties.Values[ConnProps_GUIDDomains],
    Properties.Values[DSProps_GUIDFields]
  );
end;

function TZInterbaseFirebirdConnectionGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, ColumnDomain, ColumnName);
end;

{ TZInterbaseFirebirdStatementGUIDProps }

{**
  For use from outside the unit.
  Creates an object based on Statement and Connection properties.
  The object should be re-created every time a Statement is opened.
}
constructor TZInterbaseFirebirdStatementGUIDProps.Create(const Statement: IZStatement);
begin
  inherited Create;
  InternalInit(
    DefineStatementParameter(Statement, DSProps_SetGUIDByType, ''),
    '', // Domain info is useless when object is created based on Statement
    DefineStatementParameter(Statement, DSProps_GUIDFields, '') );
end;

function TZInterbaseFirebirdStatementGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, '', ColumnName);
end;

initialization
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
end.
