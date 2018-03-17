{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Database Connectivity Interfaces              }
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

unit ZDbcIntfs;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF}, SysUtils,
  ZClasses, ZCollections, ZCompatibility, ZTokenizer, ZSelectSchema,
  ZGenericSqlAnalyser, ZDbcLogging, ZVariant, ZPlainDriver, ZURL;

const
  { generic constant for first column/parameter index }
  FirstDbcIndex = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  { generic constant for invalid column/parameter index }
  InvalidDbcIndex = {$IFDEF GENERIC_INDEX}-1{$ELSE}0{$ENDIF};
const
  { Constants from JDBC DatabaseMetadata }
  TypeSearchable           = 3;
  ProcedureReturnsResult   = 2;

// Exceptions
type

  {** Abstract SQL exception. }
  EZSQLThrowable = class(Exception)
  private
    FErrorCode: Integer;
    FStatusCode: String;
  public
    constructor Create(const Msg: string);
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    constructor CreateClone(const E:EZSQLThrowable);

    property ErrorCode: Integer read FErrorCode;
    property StatusCode: string read FStatuscode; // The "String" Errocode // FirmOS
  end;

  {** Generic SQL exception. }
  EZSQLException = class(EZSQLThrowable);

  {** Generic SQL warning. }
  EZSQLWarning = class(EZSQLThrowable);

// Data types
type
  {** Defines supported SQL types. }
  TZSQLType = (stUnknown, stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stDouble, stCurrency, stBigDecimal,
    stString, stUnicodeString,
    stBytes, stGUID,
    stDate, stTime, stTimestamp,
    stArray, stDataSet,
    stAsciiStream, stUnicodeStream, stBinaryStream);

  {** Defines a transaction isolation level. }
  TZTransactIsolationLevel = (tiNone, tiReadUncommitted, tiReadCommitted,
    tiRepeatableRead, tiSerializable);

  {** Defines a resultset fetch direction. }
  TZFetchDirection = (fdForward, fdReverse, fdUnknown);

  {** Defines a type of result set. }
  TZResultSetType = (rtForwardOnly, rtScrollInsensitive, rtScrollSensitive);

  {** Defines a result set concurrency type. }
  TZResultSetConcurrency = (rcReadOnly, rcUpdatable);

  {** Defines a nullable type for the column. }
  TZColumnNullableType = (ntNoNulls, ntNullable, ntNullableUnknown);

  {** Defines a result type for the procedures. }
  TZProcedureResultType = (prtUnknown, prtNoResult, prtReturnsResult);

  {** Defines a column type for the procedures. }
  TZProcedureColumnType = (pctUnknown, pctIn, pctInOut, pctOut, pctReturn,
    pctResultSet);

  {** Defines a best row identifier. }
  TZBestRowIdentifier = (brUnknown, brNotPseudo, brPseudo);

  {** Defines a scope best row identifier. }
  TZScopeBestRowIdentifier = (sbrTemporary, sbrTransaction, sbrSession);

  {** Defines a version column. }
  TZVersionColumn = (vcUnknown, vcNotPseudo, vcPseudo);

  {**  }
  TZImportedKey = (ikCascade, ikRestrict, ikSetNull, ikNoAction, ikSetDefault,
    ikInitiallyDeferred, ikInitiallyImmediate, ikNotDeferrable);

  TZTableIndex = (tiStatistic, tiClustered, tiHashed, tiOther);

  {** Defines a post update mode. }
  TZPostUpdatesMode = (poColumnsAll, poColumnsChanged);

  {** Defines a locate mode. }
  TZLocateUpdatesMode = (loWhereAll, loWhereChanged, loWhereKeyOnly);

// Interfaces
type

  // Forward declarations
  IZDriverManager = interface;
  IZDriver = interface;
  IZConnection = interface;
  IZDatabaseMetadata = interface;
  IZDatabaseInfo = interface;
  IZStatement = interface;
  IZPreparedStatement = interface;
  IZCallableStatement = interface;
  IZResultSet = interface;
  IZResultSetMetadata = interface;
  IZBlob = interface;
  IZNotification = interface;
  IZSequence = interface;
  IZDataSet = interface;

  {** Driver Manager interface. }
  IZDriverManager = interface(IZInterface)
    ['{8874B9AA-068A-4C0C-AE75-9DB1EA9E3720}']

    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    function GetClientVersion(const Url: string): Integer;
    procedure RegisterDriver(Driver: IZDriver);
    procedure DeregisterDriver(Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetLoginTimeout: Integer;
    procedure SetLoginTimeout(Seconds: Integer);

    procedure AddLoggingListener(Listener: IZLoggingListener);
    procedure RemoveLoggingListener(Listener: IZLoggingListener);
    function HasLoggingListener: Boolean;

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString); overload;
    procedure LogMessage(const Category: TZLoggingCategory; const Sender: IZLoggingObject); overload;
    procedure LogError(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString; ErrorCode: Integer; const Error: RawByteString);
    function ConstructURL(const Protocol, HostName, Database,
      UserName, Password: String; const Port: Integer;
      const Properties: TStrings = nil; const LibLocation: String = ''): String;
    procedure ResolveDatabaseUrl(const Url: string; out HostName: string;
      out Port: Integer; out Database: string; out UserName: string;
      out Password: string; ResultInfo: TStrings = nil); overload;
    procedure ResolveDatabaseUrl(const Url: string; out Database: string); overload;
  end;

  {** Database Driver interface. }
  IZDriver = interface(IZInterface)
    ['{2157710E-FBD8-417C-8541-753B585332E2}']

    function GetSupportedProtocols: TStringDynArray;
    function GetSupportedClientCodePages(const Url: TZURL;
      Const {$IFNDEF UNICODE}AutoEncode,{$ENDIF} SupportedsOnly: Boolean;
      CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
    function Connect(const Url: string; Info: TStrings): IZConnection; overload;
    function Connect(const Url: TZURL): IZConnection; overload;
    function GetClientVersion(const Url: string): Integer;
    function AcceptsURL(const Url: string): Boolean;
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver;

    function GetPropertyInfo(const Url: string; Info: TStrings): TStrings;
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetSubVersion: Integer;
    function GetTokenizer: IZTokenizer;
    function GetStatementAnalyser: IZStatementAnalyser;
  end;

  {** Database Connection interface. }
  IZConnection = interface(IZInterface)
    ['{8EEBBD1A-56D1-4EC0-B3BD-42B60591457F}']
    procedure RegisterStatement(const Value: IZStatement);
    procedure DeregisterStatement(const Statement: IZStatement);

    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;

    function CreateNotification(const Event: string): IZNotification;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;

    function NativeSQL(const SQL: string): string;

    procedure SetAutoCommit(Value: Boolean);
    function GetAutoCommit: Boolean;

    procedure Commit;
    procedure Rollback;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);
    procedure CommitPrepared(const transactionid: string);
    procedure RollbackPrepared(const transactionid: string);


    //Ping Server Support (firmos) 27032006

    function PingServer: Integer;
    function EscapeString(const Value: RawByteString): RawByteString;

    procedure Open;
    procedure Close;
    function IsClosed: Boolean;

    function GetDriver: IZDriver;
    function GetIZPlainDriver: IZPlainDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    function GetClientVersion: Integer;
    function GetHostVersion: Integer;

    procedure SetReadOnly(Value: Boolean);
    function IsReadOnly: Boolean;

    procedure SetCatalog(const Value: string);
    function GetCatalog: string;

    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    function GetTransactionIsolation: TZTransactIsolationLevel;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean);
    //EgonHugeist
    function GetBinaryEscapeString(const Value: RawByteString): String; overload;
    function GetBinaryEscapeString(const Value: TBytes): String; overload;
    function GetEscapeString(const Value: ZWideString): ZWideString; overload;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload;
    function GetClientCodePageInformations: PZCodePage;
    function GetAutoEncodeStrings: Boolean;
    procedure SetAutoEncodeStrings(const Value: Boolean);
    property AutoEncodeStrings: Boolean read GetAutoEncodeStrings write SetAutoEncodeStrings;
    function GetEncoding: TZCharEncoding;
    function GetConSettings: PZConSettings;
    function GetClientVariantManager: IZClientVariantManager;

    {$IFDEF ZEOS_TEST_ONLY}
    function GetTestMode : Byte;
    procedure SetTestMode(Mode: Byte);
    {$ENDIF}
  end;

  {** Database metadata interface. }
  IZDatabaseMetadata = interface(IZInterface)
    ['{FE331C2D-0664-464E-A981-B4F65B85D1A8}']

    function GetURL: string;
    function GetUserName: string;

    function GetDatabaseInfo: IZDatabaseInfo;
    function GetTriggers(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet; //EgonHugeist 30.03.2011
    function GetCollationAndCharSet(const Catalog, Schema, TableName, ColumnName: String): IZResultSet; //EgonHugeist 10.01.2012
    function GetCharacterSets: IZResultSet; //EgonHugeist 19.01.2012
    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet;
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet;

    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet;
    function GetSchemas: IZResultSet;
    function GetCatalogs: IZResultSet;
    function GetTableTypes: IZResultSet;
    function GetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet;
    function GetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet;

    function GetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet;
    function GetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
    function GetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;

    function GetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet;

    function GetTypeInfo: IZResultSet;

    function GetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet;

    function GetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet;

    function GetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;

    function GetConnection: IZConnection;
    function GetIdentifierConvertor: IZIdentifierConvertor;

    procedure ClearCache;overload; 
    procedure ClearCache(const Key: string);overload;

    function AddEscapeCharToWildcards(const Pattern:string): string;
    function NormalizePatternCase(Pattern:String): string;
  end;

  {**
    Database information interface. Used to describe the database as a whole
    (version, capabilities, policies, etc).
  } // technobot 2008-06-24
  IZDatabaseInfo = interface(IZInterface)
    ['{107CA354-F594-48F9-8E08-CD797F151EA0}']

    // database/driver/server info:
    function GetDatabaseProductName: string;
    function GetDatabaseProductVersion: string;
    function GetDriverName: string;
    function GetDriverVersion: string;
    function GetDriverMajorVersion: Integer;
    function GetDriverMinorVersion: Integer;
    function GetServerVersion: string;

    // capabilities (what it can/cannot do):
    function AllProceduresAreCallable: Boolean;
    function AllTablesAreSelectable: Boolean;
    function SupportsMixedCaseIdentifiers: Boolean;
    function SupportsMixedCaseQuotedIdentifiers: Boolean;
    function SupportsAlterTableWithAddColumn: Boolean;
    function SupportsAlterTableWithDropColumn: Boolean;
    function SupportsColumnAliasing: Boolean;
    function SupportsConvert: Boolean;
    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
      Boolean;
    function SupportsTableCorrelationNames: Boolean;
    function SupportsDifferentTableCorrelationNames: Boolean;
    function SupportsExpressionsInOrderBy: Boolean;
    function SupportsOrderByUnrelated: Boolean;
    function SupportsGroupBy: Boolean;
    function SupportsGroupByUnrelated: Boolean;
    function SupportsGroupByBeyondSelect: Boolean;
    function SupportsLikeEscapeClause: Boolean;
    function SupportsMultipleResultSets: Boolean;
    function SupportsMultipleTransactions: Boolean;
    function SupportsNonNullableColumns: Boolean;
    function SupportsMinimumSQLGrammar: Boolean;
    function SupportsCoreSQLGrammar: Boolean;
    function SupportsExtendedSQLGrammar: Boolean;
    function SupportsANSI92EntryLevelSQL: Boolean;
    function SupportsANSI92IntermediateSQL: Boolean;
    function SupportsANSI92FullSQL: Boolean;
    function SupportsIntegrityEnhancementFacility: Boolean;
    function SupportsOuterJoins: Boolean;
    function SupportsFullOuterJoins: Boolean;
    function SupportsLimitedOuterJoins: Boolean;
    function SupportsSchemasInDataManipulation: Boolean;
    function SupportsSchemasInProcedureCalls: Boolean;
    function SupportsSchemasInTableDefinitions: Boolean;
    function SupportsSchemasInIndexDefinitions: Boolean;
    function SupportsSchemasInPrivilegeDefinitions: Boolean;
    function SupportsCatalogsInDataManipulation: Boolean;
    function SupportsCatalogsInProcedureCalls: Boolean;
    function SupportsCatalogsInTableDefinitions: Boolean;
    function SupportsCatalogsInIndexDefinitions: Boolean;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean;
    function SupportsOverloadPrefixInStoredProcedureName: Boolean;
    function SupportsParameterBinding: Boolean;
    function SupportsPositionedDelete: Boolean;
    function SupportsPositionedUpdate: Boolean;
    function SupportsSelectForUpdate: Boolean;
    function SupportsStoredProcedures: Boolean;
    function SupportsSubqueriesInComparisons: Boolean;
    function SupportsSubqueriesInExists: Boolean;
    function SupportsSubqueriesInIns: Boolean;
    function SupportsSubqueriesInQuantifieds: Boolean;
    function SupportsCorrelatedSubqueries: Boolean;
    function SupportsUnion: Boolean;
    function SupportsUnionAll: Boolean;
    function SupportsOpenCursorsAcrossCommit: Boolean;
    function SupportsOpenCursorsAcrossRollback: Boolean;
    function SupportsOpenStatementsAcrossCommit: Boolean;
    function SupportsOpenStatementsAcrossRollback: Boolean;
    function SupportsTransactions: Boolean;
    function SupportsTransactionIsolationLevel(const Level: TZTransactIsolationLevel):
      Boolean;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
    function SupportsDataManipulationTransactionsOnly: Boolean;
    function SupportsResultSetType(const _Type: TZResultSetType): Boolean;
    function SupportsResultSetConcurrency(const _Type: TZResultSetType;
      const Concurrency: TZResultSetConcurrency): Boolean;
    function SupportsBatchUpdates: Boolean;
    function SupportsNonEscapedSearchStrings: Boolean;
    function SupportsMilliseconds: Boolean;
    function SupportsUpdateAutoIncrementFields: Boolean;
    function SupportsArrayBindings: Boolean;

    // maxima:
    function GetMaxBinaryLiteralLength: Integer;
    function GetMaxCharLiteralLength: Integer;
    function GetMaxColumnNameLength: Integer;
    function GetMaxColumnsInGroupBy: Integer;
    function GetMaxColumnsInIndex: Integer;
    function GetMaxColumnsInOrderBy: Integer;
    function GetMaxColumnsInSelect: Integer;
    function GetMaxColumnsInTable: Integer;
    function GetMaxConnections: Integer;
    function GetMaxCursorNameLength: Integer;
    function GetMaxIndexLength: Integer;
    function GetMaxSchemaNameLength: Integer;
    function GetMaxProcedureNameLength: Integer;
    function GetMaxCatalogNameLength: Integer;
    function GetMaxRowSize: Integer;
    function GetMaxStatementLength: Integer;
    function GetMaxStatements: Integer;
    function GetMaxTableNameLength: Integer;
    function GetMaxTablesInSelect: Integer;
    function GetMaxUserNameLength: Integer;

    // policies (how are various data and operations handled):
    function IsReadOnly: Boolean;
    function IsCatalogAtStart: Boolean;
    function DoesMaxRowSizeIncludeBlobs: Boolean;
    function NullsAreSortedHigh: Boolean;
    function NullsAreSortedLow: Boolean;
    function NullsAreSortedAtStart: Boolean;
    function NullsAreSortedAtEnd: Boolean;
    function NullPlusNonNullIsNull: Boolean;
    function UsesLocalFiles: Boolean;
    function UsesLocalFilePerTable: Boolean;
    function StoresUpperCaseIdentifiers: Boolean;
    function StoresLowerCaseIdentifiers: Boolean;
    function StoresMixedCaseIdentifiers: Boolean;
    function StoresUpperCaseQuotedIdentifiers: Boolean;
    function StoresLowerCaseQuotedIdentifiers: Boolean;
    function StoresMixedCaseQuotedIdentifiers: Boolean;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel;
    function DataDefinitionCausesTransactionCommit: Boolean;
    function DataDefinitionIgnoredInTransactions: Boolean;

    // interface details (terms, keywords, etc):
    function GetIdentifierQuoteString: string;
    function GetIdentifierQuoteKeywordsSorted: TStringDynArray;
    function GetSchemaTerm: string;
    function GetProcedureTerm: string;
    function GetCatalogTerm: string;
    function GetCatalogSeparator: string;
    function GetSQLKeywords: string;
    function GetNumericFunctions: string;
    function GetStringFunctions: string;
    function GetSystemFunctions: string;
    function GetTimeDateFunctions: string;
    function GetSearchStringEscape: string;
    function GetExtraNameCharacters: string;
  end;

  {** Generic SQL statement interface. }
  IZStatement = interface(IZInterface)
    ['{22CEFA7E-6A6D-48EC-BB9B-EE66056E90F1}']

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; overload;
    function ExecuteUpdate(const SQL: ZWideString): Integer; overload;
    function Execute(const SQL: ZWideString): Boolean; overload;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload;
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload;
    function Execute(const SQL: RawByteString): Boolean; overload;

    function GetSQL : String;

    procedure Close;

    function GetMaxFieldSize: Integer;
    procedure SetMaxFieldSize(Value: Integer);
    function GetMaxRows: Integer;
    procedure SetMaxRows(Value: Integer);
    procedure SetEscapeProcessing(Value: Boolean);
    function GetQueryTimeout: Integer;
    procedure SetQueryTimeout(Value: Integer);
    procedure Cancel;
    procedure SetCursorName(const Value: AnsiString);

    function GetResultSet: IZResultSet;
    function GetUpdateCount: Integer;
    function GetMoreResults: Boolean;

    procedure SetFetchDirection(Value: TZFetchDirection);
    function GetFetchDirection: TZFetchDirection;
    procedure SetFetchSize(Value: Integer);
    function GetFetchSize: Integer;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency);
    function GetResultSetConcurrency: TZResultSetConcurrency;
    procedure SetResultSetType(Value: TZResultSetType);
    function GetResultSetType: TZResultSetType;

    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    function GetPostUpdates: TZPostUpdatesMode;
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    function GetLocateUpdates: TZLocateUpdatesMode;

    procedure AddBatch(const SQL: string); deprecated;
    procedure AddBatchRequest(const SQL: string);

    procedure ClearBatch;
    function ExecuteBatch: TIntegerDynArray;

    function GetConnection: IZConnection;
    function GetParameters: TStrings;
    function GetChunkSize: Integer;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;
    procedure FreeOpenResultSetReference;
  end;

  {** Prepared SQL statement interface. }
  IZPreparedStatement = interface(IZStatement)
    ['{990B8477-AF11-4090-8821-5B7AFEA9DD70}']

    function ExecuteQueryPrepared: IZResultSet;
    function ExecuteUpdatePrepared: Integer;
    function ExecutePrepared: Boolean;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string);

    procedure SetNull(ParameterIndex: Integer; const SQLType: TZSQLType);
    procedure SetBoolean(ParameterIndex: Integer; const Value: Boolean);
    procedure SetByte(ParameterIndex: Integer; const Value: Byte);
    procedure SetShort(ParameterIndex: Integer; const Value: ShortInt);
    procedure SetWord(ParameterIndex: Integer; const Value: Word);
    procedure SetSmall(ParameterIndex: Integer; const Value: SmallInt);
    procedure SetUInt(ParameterIndex: Integer; const Value: Cardinal);
    procedure SetInt(ParameterIndex: Integer; const Value: Integer);
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; const Value: Single);
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: Extended);
    procedure SetPChar(ParameterIndex: Integer; const Value: PChar);
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); //AVZ
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes);
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID);
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime);
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime);
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime);
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBlob(ParameterIndex: Integer; const SQLType: TZSQLType;
      const Value: IZBlob);
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant);
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);

    procedure ClearParameters;

    procedure AddBatchPrepared;
    function GetMetadata: IZResultSetMetadata;
  end;

  {** Callable SQL statement interface. }
  IZCallableStatement = interface(IZPreparedStatement)
    ['{E6FA6C18-C764-4C05-8FCB-0582BDD1EF40}']
    function IsFunction: Boolean;
    { Multiple ResultSet support API }
    function GetFirstResultSet: IZResultSet;
    function GetPreviousResultSet: IZResultSet;
    function GetNextResultSet: IZResultSet;
    function GetLastResultSet: IZResultSet;
    function BOR: Boolean;
    function EOR: Boolean;
    function GetResultSetByIndex(const Index: Integer): IZResultSet;
    function GetResultSetCount: Integer;

    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer);
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer);
    function WasNull: Boolean;

    function IsNull(ParameterIndex: Integer): Boolean;
    function GetPChar(ParameterIndex: Integer): PChar;
    function GetString(ParameterIndex: Integer): String;
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString;
    function GetBoolean(ParameterIndex: Integer): Boolean;
    function GetByte(ParameterIndex: Integer): Byte;
    function GetShort(ParameterIndex: Integer): ShortInt;
    function GetWord(ParameterIndex: Integer): Word;
    function GetSmall(ParameterIndex: Integer): SmallInt;
    function GetUInt(ParameterIndex: Integer): Cardinal;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetULong(ParameterIndex: Integer): UInt64;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double;
    function GetCurrency(ParameterIndex: Integer): Currency;
    function GetBigDecimal(ParameterIndex: Integer): Extended;
    function GetBytes(ParameterIndex: Integer): TBytes;
    function GetDate(ParameterIndex: Integer): TDateTime;
    function GetTime(ParameterIndex: Integer): TDateTime;
    function GetTimestamp(ParameterIndex: Integer): TDateTime;
    function GetValue(ParameterIndex: Integer): TZVariant;
  end;

  IZParamNamedCallableStatement = interface(IZCallableStatement)
    ['{99882891-81B2-4F3E-A3D7-35B6DCAA7136}']
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
  end;

  {** EH: sort helper procs }
  TCompareFunc = function(const Null1, Null2: Boolean; const V1, V2): Integer;
  TCompareFuncs = Array of TCompareFunc;

  {** Defines Column-Comparison kinds }
  TComparisonKind = (ckAscending{greater than}, ckDescending{less than}, ckEquals);
  TComparisonKindArray = Array of TComparisonKind;

  {** Rows returned by SQL query. }
  IZResultSet = interface(IZInterface)
    ['{8F4C4D10-2425-409E-96A9-7142007CC1B2}']

    function Next: Boolean;
    procedure Close;
    procedure ResetCursor;
    function WasNull: Boolean;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPChar(ColumnIndex: Integer): PChar;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; overload;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetString(ColumnIndex: Integer): String;
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetBinaryString(ColumnIndex: Integer): RawByteString; deprecated;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString;
    function GetPWideChar(ColumnIndex: Integer): PWideChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetByte(ColumnIndex: Integer): Byte;
    function GetShort(ColumnIndex: Integer): ShortInt;
    function GetWord(ColumnIndex: Integer): Word;
    function GetSmall(ColumnIndex: Integer): SmallInt;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    function GetBigDecimal(ColumnIndex: Integer): Extended;
    function GetBytes(ColumnIndex: Integer): TBytes;
    function GetDate(ColumnIndex: Integer): TDateTime;
    function GetTime(ColumnIndex: Integer): TDateTime;
    function GetTimestamp(ColumnIndex: Integer): TDateTime;
    function GetAsciiStream(ColumnIndex: Integer): TStream;
    function GetUnicodeStream(ColumnIndex: Integer): TStream;
    function GetBinaryStream(ColumnIndex: Integer): TStream;
    function GetBlob(ColumnIndex: Integer): IZBlob;
    function GetDataSet(ColumnIndex: Integer): IZDataSet;
    function GetValue(ColumnIndex: Integer): TZVariant;
    function GetDefaultExpression(ColumnIndex: Integer): string;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    function IsNullByName(const ColumnName: string): Boolean;
    function GetPCharByName(const ColumnName: string): PChar;
    function GetPAnsiCharByName(const ColumnName: string): PAnsiChar; overload;
    function GetPAnsiCharByName(const ColumnName: string; out Len: NativeUInt): PAnsiChar; overload;
    function GetStringByName(const ColumnName: string): String;
    function GetAnsiStringByName(const ColumnName: string): AnsiString;
    function GetUTF8StringByName(const ColumnName: string): UTF8String;
    function GetRawByteStringByName(const ColumnName: string): RawByteString;
    function GetBinaryStringByName(const ColumnName: string): RawByteString; deprecated;
    function GetUnicodeStringByName(const ColumnName: string): ZWideString;
    function GetPWideCharByName(const ColumnName: string): PWideChar; overload;
    function GetPWideCharByName(const ColumnName: string; out Len: NativeUInt): PWideChar; overload;
    function GetBooleanByName(const ColumnName: string): Boolean;
    function GetByteByName(const ColumnName: string): Byte;
    function GetShortByName(const ColumnName: string): ShortInt;
    function GetWordByName(const ColumnName: string): Word;
    function GetSmallByName(const ColumnName: string): SmallInt;
    function GetUIntByName(const ColumnName: string): Cardinal;
    function GetIntByName(const ColumnName: string): Integer;
    function GetULongByName(const ColumnName: string): UInt64;
    function GetLongByName(const ColumnName: string): Int64;
    function GetFloatByName(const ColumnName: string): Single;
    function GetDoubleByName(const ColumnName: string): Double;
    function GetCurrencyByName(const ColumnName: string): Currency;
    function GetBigDecimalByName(const ColumnName: string): Extended;
    function GetBytesByName(const ColumnName: string): TBytes;
    function GetDateByName(const ColumnName: string): TDateTime;
    function GetTimeByName(const ColumnName: string): TDateTime;
    function GetTimestampByName(const ColumnName: string): TDateTime;
    function GetAsciiStreamByName(const ColumnName: string): TStream;
    function GetUnicodeStreamByName(const ColumnName: string): TStream;
    function GetBinaryStreamByName(const ColumnName: string): TStream;
    function GetBlobByName(const ColumnName: string): IZBlob;
    function GetDataSetByName(const ColumnName: String): IZDataSet;
    function GetValueByName(const ColumnName: string): TZVariant;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    function GetCursorName: AnsiString;
    function GetMetadata: IZResultSetMetadata;
    function FindColumn(const ColumnName: string): Integer;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function IsBeforeFirst: Boolean;
    function IsAfterLast: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    procedure BeforeFirst;
    procedure AfterLast;
    function First: Boolean;
    function Last: Boolean;
    function GetRow: NativeInt;
    function MoveAbsolute(Row: Integer): Boolean;
    function MoveRelative(Rows: Integer): Boolean;
    function Previous: Boolean;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    procedure SetFetchDirection(Value: TZFetchDirection);
    function GetFetchDirection: TZFetchDirection;

    procedure SetFetchSize(Value: Integer);
    function GetFetchSize: Integer;

    function GetType: TZResultSetType;
    function GetConcurrency: TZResultSetConcurrency;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean;
    function RowInserted: Boolean;
    function RowDeleted: Boolean;

    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; const Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; const Value: Byte);
    procedure UpdateShort(ColumnIndex: Integer; const Value: ShortInt);
    procedure UpdateWord(ColumnIndex: Integer; const Value: Word);
    procedure UpdateSmall(ColumnIndex: Integer; const Value: SmallInt);
    procedure UpdateUInt(ColumnIndex: Integer; const Value: Cardinal);
    procedure UpdateInt(ColumnIndex: Integer; const Value: Integer);
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64);
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; const Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double);
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended);
    procedure UpdatePChar(ColumnIndex: Integer; const Value: PChar);
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar); overload;
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUInt); overload;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar); overload;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; Len: PNativeUInt); overload;
    procedure UpdateString(ColumnIndex: Integer; const Value: String);
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString);
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String);
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateBinaryString(ColumnIndex: Integer; const Value: RawByteString); deprecated;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString);
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes);
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime);
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime);
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime);
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateDataSet(ColumnIndex: Integer; const Value: IZDataSet);
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant);
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob);

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string);
    procedure UpdateBooleanByName(const ColumnName: string; const Value: Boolean);
    procedure UpdateByteByName(const ColumnName: string; const Value: Byte);
    procedure UpdateShortByName(const ColumnName: string; const Value: ShortInt);
    procedure UpdateWordByName(const ColumnName: string; const Value: Word);
    procedure UpdateSmallByName(const ColumnName: string; const Value: SmallInt);
    procedure UpdateUIntByName(const ColumnName: string; const Value: Cardinal);
    procedure UpdateIntByName(const ColumnName: string; const Value: Integer);
    procedure UpdateULongByName(const ColumnName: string; const Value: UInt64);
    procedure UpdateLongByName(const ColumnName: string; const Value: Int64);
    procedure UpdateFloatByName(const ColumnName: string; const Value: Single);
    procedure UpdateCurrencyByName(const ColumnName: string; const Value: Currency);
    procedure UpdateDoubleByName(const ColumnName: string; const Value: Double);
    procedure UpdateBigDecimalByName(const ColumnName: string; const Value: Extended);
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar); overload;
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar; Len: PNativeUInt); overload;
    procedure UpdatePCharByName(const ColumnName: string; const Value: PChar);
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar); overload;
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar; Len: PNativeUInt); overload;
    procedure UpdateStringByName(const ColumnName: string; const Value: String);
    procedure UpdateAnsiStringByName(const ColumnName: string; const Value: AnsiString);
    procedure UpdateUTF8StringByName(const ColumnName: string; const Value: UTF8String);
    procedure UpdateRawByteStringByName(const ColumnName: string; const Value: RawByteString);
    procedure UpdateBinaryStringByName(const ColumnName: string; const Value: RawByteString); deprecated;
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: ZWideString);
    procedure UpdateBytesByName(const ColumnName: string; const Value: TBytes);
    procedure UpdateDateByName(const ColumnName: string; const Value: TDateTime);
    procedure UpdateTimeByName(const ColumnName: string; const Value: TDateTime);
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TDateTime);
    procedure UpdateAsciiStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateUnicodeStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateBinaryStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateDataSetByName(const ColumnName: string; const Value: IZDataSet);
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant);

    procedure InsertRow;
    procedure UpdateRow;
    procedure DeleteRow;
    procedure RefreshRow;
    procedure CancelRowUpdates;
    procedure MoveToInsertRow;
    procedure MoveToCurrentRow;
//    procedure MoveToSearchRow;

//    function Search(CaseInsensitive, PartialKey: Boolean): Boolean;
//    function Compare(Row: Integer; CaseInsensitive, PartialKey: Boolean):
//      Boolean;

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs;

    function GetStatement: IZStatement;
    function GetConSettings: PZConsettings;
  end;

  {** TDataSet interface}
  IZDataSet = interface(IZInterface)
    ['{DBC24011-EF26-4FD8-AC8B-C3E01619494A}']
    function GetDataSet: TDataSet;
    function IsEmpty: Boolean;
  end;

  {** ResultSet metadata interface. }
  IZResultSetMetadata = interface(IZInterface)
    ['{47CA2144-2EA7-42C4-8444-F5154369B2D7}']

    function GetColumnCount: Integer;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean;
    function IsCaseSensitive(ColumnIndex: Integer): Boolean;
    function IsSearchable(ColumnIndex: Integer): Boolean;
    function IsCurrency(ColumnIndex: Integer): Boolean;
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType;

    function IsSigned(ColumnIndex: Integer): Boolean;
    function GetColumnDisplaySize(ColumnIndex: Integer): Integer;
    function GetColumnLabel(ColumnIndex: Integer): string;
    function GetColumnName(ColumnIndex: Integer): string;
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    function GetSchemaName(ColumnIndex: Integer): string;
    function GetPrecision(ColumnIndex: Integer): Integer;
    function GetScale(ColumnIndex: Integer): Integer;
    function GetTableName(ColumnIndex: Integer): string;
    function GetCatalogName(ColumnIndex: Integer): string;
    function GetColumnType(ColumnIndex: Integer): TZSQLType;
    function GetColumnTypeName(ColumnIndex: Integer): string;
    function IsReadOnly(ColumnIndex: Integer): Boolean;
    function IsWritable(ColumnIndex: Integer): Boolean;
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean;
    function GetDefaultValue(ColumnIndex: Integer): string;
    function HasDefaultValue(ColumnIndex: Integer): Boolean;
  end;

  {** External or internal blob wrapper object. }
  PIZLob = ^IZBlob;
  IZBlob = interface(IZInterface)
    ['{47D209F1-D065-49DD-A156-EFD1E523F6BF}']

    function IsEmpty: Boolean;
    function IsUpdated: Boolean;
    function IsClob: Boolean;
    function Length: Integer;

    function GetString: RawByteString;
    procedure SetString(const Value: RawByteString);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetStream: TStream;
    procedure SetStream(const Value: TStream); overload;
    function GetBuffer: Pointer;
    procedure SetBuffer(const Buffer: Pointer; const Length: Integer);
    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    procedure SetBlobData(const Buffer: Pointer; const Len: Cardinal); overload;
    {$ENDIF}

    procedure Clear;
    function Clone(Empty: Boolean = False): IZBlob;

    {Clob operations}
    function GetRawByteString: RawByteString;
    procedure SetRawByteString(Const Value: RawByteString; const CodePage: Word);
    function GetAnsiString: AnsiString;
    procedure SetAnsiString(Const Value: AnsiString);
    function GetUTF8String: UTF8String;
    procedure SetUTF8String(Const Value: UTF8String);
    procedure SetUnicodeString(const Value: ZWideString);
    function GetUnicodeString: ZWideString;
    procedure SetStream(const Value: TStream; const CodePage: Word); overload;
    function GetRawByteStream: TStream;
    function GetAnsiStream: TStream;
    function GetUTF8Stream: TStream;
    function GetUnicodeStream: TStream;
    function GetPAnsiChar(const CodePage: Word): PAnsiChar;
    procedure SetPAnsiChar(const Buffer: PAnsiChar; const CodePage: Word; const Len: Cardinal);
    function GetPWideChar: PWideChar;
    procedure SetPWideChar(const Buffer: PWideChar; const Len: Cardinal);
    function GetBufferAddress: PPointer;
    function GetLengthAddress: PInteger;
    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    procedure SetBlobData(const Buffer: Pointer; const Len: Cardinal; const CodePage: Word); overload;
    {$ENDIF}
  end;
  IZUnCachedLob = interface(IZBlob)
    ['{194F1179-9FFC-4032-B983-5EB3DD2E8B16}']
    procedure FlushBuffer;
  end;

  {** Database notification interface. }
  IZNotification = interface(IZInterface)
    ['{BF785C71-EBE9-4145-8DAE-40674E45EF6F}']

    function GetEvent: string;
    procedure Listen;
    procedure Unlisten;
    procedure DoNotify;
    function CheckEvents: string;

    function GetConnection: IZConnection;
  end;

  {** Database sequence generator interface. }
  IZSequence = interface(IZInterface)
    ['{A9A54FE5-0DBE-492F-8DA6-04AC5FCE779C}']
    function  GetName: string;
    function  GetBlockSize: Integer;
    procedure SetName(const Value: string);
    procedure SetBlockSize(const Value: Integer);
    function  GetCurrentValue: Int64;
    function  GetNextValue: Int64;
    function  GetCurrentValueSQL: string;
    function  GetNextValueSQL: string;
    function  GetConnection: IZConnection;
  end;

var
  {** The common driver manager object. }
  DriverManager: IZDriverManager;

implementation

uses ZMessages,{$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF};

type
  {** Driver Manager interface. }

  { TZDriverManager }

  TZDriverManager = class(TInterfacedObject, IZDriverManager)
  private
    FDrivers: IZCollection;
    FLoginTimeout: Integer;
    FLoggingListeners: IZCollection;
    FHasLoggingListener: Boolean;
    FCriticalSection: TCriticalSection;
    FURL: TZURL;
    procedure LogEvent(const Event: TZLoggingEvent);
  public
    constructor Create;
    destructor Destroy; override;

    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    procedure RegisterDriver(Driver: IZDriver);
    procedure DeregisterDriver(Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetClientVersion(const Url: string): Integer;

    function GetLoginTimeout: Integer;
    procedure SetLoginTimeout(Value: Integer);

    procedure AddLoggingListener(Listener: IZLoggingListener);
    procedure RemoveLoggingListener(Listener: IZLoggingListener);
    function HasLoggingListener: Boolean;

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString); overload;
    procedure LogMessage(const Category: TZLoggingCategory; const Sender: IZLoggingObject); overload;
    procedure LogError(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString; ErrorCode: Integer; const Error: RawByteString);

    function ConstructURL(const Protocol, HostName, Database,
      UserName, Password: String; const Port: Integer;
      const Properties: TStrings = nil; const LibLocation: String = ''): String;
    procedure ResolveDatabaseUrl(const Url: string; out HostName: string;
      out Port: Integer; out Database: string; out UserName: string;
      out Password: string; ResultInfo: TStrings = nil); overload;
    procedure ResolveDatabaseUrl(const Url: string; out Database: string); overload;
  end;

{ TZDriverManager }

{**
  Constructs this object with default properties.
}
constructor TZDriverManager.Create;
begin
  FDrivers := TZCollection.Create;
  FLoginTimeout := 0;
  FLoggingListeners := TZCollection.Create;
  FHasLoggingListener := False;
  FURL := TZURL.Create;
  fCriticalSection := TCriticalSection.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZDriverManager.Destroy;
begin
  FreeAndNil(FURL);
  FDrivers := nil;
  FLoggingListeners := nil;
  FreeAndNil(fCriticalSection);
  inherited Destroy;
end;

{**
  Gets a collection of registered drivers.
  @return an unmodifiable collection with registered drivers.
}
function TZDriverManager.GetDrivers: IZCollection;
begin
  Result := TZUnmodifiableCollection.Create(FDrivers);
end;

{**
  Gets a login timeout value.
  @return a login timeout.
}
function TZDriverManager.GetLoginTimeout: Integer;
begin
  Result := FLoginTimeout;
end;

{**
  Sets a new login timeout value.
  @param Seconds a new login timeout in seconds.
}
procedure TZDriverManager.SetLoginTimeout(Value: Integer);
begin
  FLoginTimeout := Value;
end;

{**
  Registers a driver for specific database.
  @param Driver a driver to be registered.
}
procedure TZDriverManager.RegisterDriver(Driver: IZDriver);
begin
  if not FDrivers.Contains(Driver) then
    FDrivers.Add(Driver);
end;

{**
  Unregisters a driver for specific database.
  @param Driver a driver to be unregistered.
}
procedure TZDriverManager.DeregisterDriver(Driver: IZDriver);
begin
  FDrivers.Remove(Driver);
end;

{**
  Gets a driver which accepts the specified url.
  @param Url a database connection url.
  @return a found driver or <code>null</code> otherwise.
}
function TZDriverManager.GetDriver(const Url: string): IZDriver;
var
  I: Integer;
  Current: IZDriver;
begin
  Result := nil;
  for I := 0 to FDrivers.Count - 1 do
  begin
    Current := FDrivers[I] as IZDriver;
    if Current.AcceptsURL(Url) then
    begin
      Result := Current;
      Break;
    end;
  end;
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @param Info an extra connection parameters.
  @return an opened connection.
}
function TZDriverManager.GetConnectionWithParams(const Url: string; Info: TStrings):
  IZConnection;
var
  Driver: IZDriver;
begin
  Driver := GetDriver(Url);
  if Driver = nil then
    raise EZSQLException.Create(SDriverWasNotFound);
  Result := Driver.Connect(Url, Info);
end;

{**
  Locates a required driver and returns the client library version number.
  @param Url a database connection Url.
  @return client library version number.
}
function TZDriverManager.GetClientVersion(const Url: string): Integer;
var
  Driver: IZDriver;
begin
  Driver := GetDriver(Url);
  if Driver = nil then
    raise EZSQLException.Create(SDriverWasNotFound);
  Result := Driver.GetClientVersion(Url);
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @param User a user's name.
  @param Password a user's password.
  @return an opened connection.
}
function TZDriverManager.GetConnectionWithLogin(const Url: string; const User: string;
  const Password: string): IZConnection;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  try
    Info.Add('username=' + User);
    Info.Add('password=' + Password);
    Result := GetConnectionWithParams(Url, Info);
  finally
    Info.Free;
  end;
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @return an opened connection.
}
function TZDriverManager.GetConnection(const Url: string): IZConnection;
begin
  Result := GetConnectionWithParams(Url, nil);
end;

{**
  Adds a logging listener to log SQL events.
  @param Listener a logging interface to be added.
}
procedure TZDriverManager.AddLoggingListener(Listener: IZLoggingListener);
begin
  fCriticalSection.Enter;
  try
    FLoggingListeners.Add(Listener);
    FHasLoggingListener := True;
  finally
    fCriticalSection.Leave;
  end;
end;

{**
  Removes a logging listener from the list.
  @param Listener a logging interface to be removed.
}
procedure TZDriverManager.RemoveLoggingListener(Listener: IZLoggingListener);
begin
  fCriticalSection.Enter;
  try
    FLoggingListeners.Remove(Listener);
    FHasLoggingListener := (FLoggingListeners.Count>0);
  finally
    fCriticalSection.Leave;
  end;
end;

function TZDriverManager.HasLoggingListener: Boolean;
begin
  result := FHasLoggingListener;
end;

{**
  Logs a message about event with error result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
procedure TZDriverManager.LogError(Category: TZLoggingCategory;
  const Protocol: RawByteString; const Msg: RawByteString; ErrorCode: Integer;
  const Error: RawByteString);
var
  Event: TZLoggingEvent;
begin
  if not FHasLoggingListener then
    Exit;
  Event := TZLoggingEvent.Create(Category, Protocol, Msg, ErrorCode, Error);
  try
    LogEvent(Event);
  finally
    Event.Destroy;
  end;
end;

{**
  Logs a message about event with error result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
procedure TZDriverManager.LogEvent(const Event: TZLoggingEvent);
var
  I: Integer;
  Listener: IZLoggingListener;
begin
  if not FHasLoggingListener then
    Exit;
  fCriticalSection.Enter;
  try
    for I := 0 to FLoggingListeners.Count - 1 do
    begin
      Listener := FLoggingListeners[I] as IZLoggingListener;
      try
        Listener.LogEvent(Event);
      except
      end;
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

{**
  Logs a message about event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZDriverManager.LogMessage(Category: TZLoggingCategory;
  const Protocol: RawByteString; const Msg: RawByteString);
begin
  if not FHasLoggingListener then
      Exit;
  LogError(Category, Protocol, Msg, 0, '');
end;

procedure TZDriverManager.LogMessage(const Category: TZLoggingCategory;
  const Sender: IZLoggingObject);
var
  Event: TZLoggingEvent;
begin
  if not FHasLoggingListener then
    Exit;
  Event := Sender.CreateLogEvent(Category);
  If Assigned(Event) then
  begin
    LogEvent(Event);
    Event.Free;
  end;
end;

{**
  Constructs a valid URL
  @param Protocol the Driver-protocol (must be assigned).
  @param HostName the hostname (could be empty).
  @param Database the connection-database (could be empty).
  @param UserName the username (could be empty).
  @param Password the password(could be empty).
  @param Port the Server-Port (could be 0).
  @param Properties the Database-Properties (could be empty).
}
function TZDriverManager.ConstructURL(const Protocol, HostName, Database,
  UserName, Password: String; const Port: Integer;
  const Properties: TStrings = nil; const LibLocation: String = ''): String;
begin
  FURL.Protocol := Protocol;
  FURL.HostName := HostName;
  FURL.Database := DataBase;
  FURL.UserName := UserName;
  FURL.Password := Password;
  FURL.Port := Port;
  FURL.Properties.Clear;
  if Assigned(Properties) then
    FURL.Properties.AddStrings(Properties);
  FURL.LibLocation := LibLocation;
  Result := FURL.URL;
end;

{**
  Resolves a database URL and fills the database connection parameters.
  @param Url an initial database URL.
  @param HostName a name of the database host.
  @param Port a port number.
  @param Database a database name.
  @param UserName a name of the database user.
  @param Password a user's password.
  @param ResutlInfo a result info parameters.
}
procedure TZDriverManager.ResolveDatabaseUrl(const Url: string; out HostName: string;
  out Port: Integer; out Database: string; out UserName: string;
  out Password: string; ResultInfo: TStrings = nil);
begin
  FURL.URL := Url;
  HostName := FURL.HostName;
  Port := FURL.Port;
  DataBase := FURL.Database;
  UserName := FURL.UserName;
  PassWord := FURL.Password;
  if Assigned(ResultInfo) then
  begin
    ResultInfo.Clear;
    ResultInfo.AddStrings(FURL.Properties);
  end;
end;

{**
  Resolves a database URL and fills the database parameter for MetaData.
  @param Url an initial database URL.
  @param Database a database name.
}
procedure TZDriverManager.ResolveDatabaseUrl(const Url: string; out Database: string);
begin
  FURL.URL := Url;
  DataBase := FURL.Database;
end;

{ EZSQLThrowable }

{**
  Creates an exception with message string.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode:=E.ErrorCode;
  FStatusCode:=E.Statuscode;
end;

constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
  @param ErrorCode a native server error code.
}
constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

initialization
  DriverManager := TZDriverManager.Create;
finalization
  DriverManager := nil;
end.

