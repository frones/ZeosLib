{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{ Originally written by Sergey Merkuriev, Sergey Seroukhov}
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

unit ZSqlTestCase;

interface

{$I ZTestFramework.inc}

uses
  {$IFNDEF VER130BELOW}Types, {$ENDIF} {$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF},
  Classes, SysUtils, DB, Contnrs, FmtBCD,
  ZDataset, ZDatasetUtils, ZAbstractConnection,
  ZCompatibility, ZDbcIntfs, ZConnection, ZTestCase, ZScriptParser, ZDbcLogging;

const
  { protocol lists }
  pl_all_mysql = 'mysql,mariadb';
  pl_all_postgresql = 'postgresql';
  pl_all_sqlite = 'sqlite';
  pl_all_interbase = 'firebird,interbase,firebird3up';
  pl_all_oracle = 'oracle';
  pl_all_dblib = 'mssql,sybase';
  pl_all_asa = 'asa,asa_capi';
  pl_all_transports = 'ado,odbc_a,odbc_w,oledb,webserviceproxy';
  pl_all = pl_all_mysql + ',' + pl_all_postgresql + ',' + pl_all_sqlite + ','
         + pl_all_interbase + ',' + pl_all_oracle + ',' + pl_all_dblib + ',' + pl_all_asa;

  // Protocols needing prefererealprepared option for real prepared statements
  pl_realpreparable = pl_all_mysql;

type
  TZConfigUse = (cuMainConnection, cuNonAscii);
  TZConfigUses = set of TZConfigUse;

  TDataSetTypesDynArray = array of TFieldType;
  TResultSetTypesDynArray = array of TZSQLType;

  // ! List of protocols totally mirrors protocol conditions used throughout the tests.
  // Protocol type is determined by StartsWith(Protocol, ProtocolPrefixes[drv]),
  // so one prefix means exactly one protocol type!
  TProtocolType = (protUnknown, protMySQL, protPostgre, protSQLite, protFirebird, protInterbase,
    protOracle, protASA, protASACAPI, protMSSQL, protOleDB, protADO, protSybase,
    protODBC, protWebServiceProxy);

  TZTransport = (traUnknown, traNative, traADO, traODBC, traOLEDB, traWEBPROXY);

const
  // Strictly in lower-case
  ProtocolPrefixes: array[TProtocolType] of string =
    ('unknown', 'mysql', 'postgresql', 'sqlite', 'firebird', 'interbase',
     'oracle', 'asa', 'asa_capi', 'mssql', 'oledb', 'ado', 'sybase',
     'odbc', 'webserviceproxy');

type
  {** Represents a SQL test database configuration. }
  { TZConnectionConfig }
  TZConnectionConfig = class
  private
    FConfigUses: TZConfigUses;
    FName: string;
    FLibLocation: string;
    FAlias: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FRebuild: Boolean;
    FDelimiter: string;
    FDelimiterType: TZDelimiterType;
    FCreateScripts: TStringDynArray;
    FDropScripts: TStringDynArray;
    FProperties: TStringDynArray;
    FCharacterSets: TStringDynArray;
    FExtendedTest: Boolean;
    FExtended_cGet_ACP: Boolean;
    FExtended_cGet_UTF8: Boolean;
    FExtended_cGet_UTF16: Boolean;
    FExtended_Codepages: Boolean;
    FSkip_Performance: Boolean;
    FTestMode: Byte;
    FPerformanceDataSetTypes: TDataSetTypesDynArray;
    FPerformanceResultSetTypes: TResultSetTypesDynArray;
    FPerformanceFieldSizes: TIntegerDynArray;
    FPerformanceFieldNames: TStringDynArray;
    FPerformanceFieldPropertiesDetermined: Boolean;
    FProvider: TZServerProvider;
    FTransport: TZTransport;
    procedure SetConfigUses(AValue: TZConfigUses);
    function GetConnectionUrl(const Param: String): TZURL;
    function CreateDbcConnection: IZConnection;
    function GetProtocolType: TProtocolType;
    function GetProvider: TZServerProvider;
    function GetTransport: TZTransport;
  public
    constructor Create; overload;
    constructor Create(TemplateConfig: TZConnectionConfig; Suffix: String); overload;
    constructor Create(ConnectionName: String); overload;
    destructor Destroy; override;
    procedure CreateExtendedConfigurations(ConnectionsList: TObjectList);
    property Name: string read FName write FName;
    property LibLocation: string read FLibLocation write FLibLocation;
    property Alias: string read FAlias write FAlias;
    property Protocol: string read FProtocol write FProtocol;
    property HostName: string read FHostName write FHostName;
    property Port: Integer read FPort write FPort;
    property Database: string read FDatabase write FDatabase;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Rebuild: Boolean read FRebuild write FRebuild;
    property Delimiter: string read FDelimiter write FDelimiter;
    property DelimiterType: TZDelimiterType read FDelimiterType
      write FDelimiterType;
    property CreateScripts: TStringDynArray read FCreateScripts
      write FCreateScripts;
    property DropScripts: TStringDynArray read FDropScripts
      write FDropScripts;
    property Properties: TStringDynArray read FProperties
      write FProperties;
    property CharacterSets: TStringDynArray read FCharacterSets
      write FCharacterSets;
    property ExtendedTest: Boolean read FExtendedTest;
    property Include_cGet_ACP: Boolean read FExtended_cGet_ACP;
    property Include_cGet_UTF8: Boolean read FExtended_cGet_UTF8;
    property Include_cGet_UTF16: Boolean read FExtended_cGet_UTF16;
    property Include_Codepages: Boolean read FExtended_Codepages;
    property ConfigUses: TZConfigUses read FConfigUses write SetConfigUses;
    property TestMode: Byte read FTestMode write FTestMode;
    property PerformanceDataSetTypes: TDataSetTypesDynArray read FPerformanceDataSetTypes write FPerformanceDataSetTypes;
    property PerformanceResultSetTypes: TResultSetTypesDynArray read FPerformanceResultSetTypes write FPerformanceResultSetTypes;
    property PerformanceFieldSizes: TIntegerDynArray read FPerformanceFieldSizes write FPerformanceFieldSizes;
    property PerformanceFieldNames: TStringDynArray read FPerformanceFieldNames write FPerformanceFieldNames;
    property PerformanceFieldPropertiesDetermined: Boolean read FPerformanceFieldPropertiesDetermined write FPerformanceFieldPropertiesDetermined;
    property Provider: TZServerProvider read GetProvider;
    property Transport: TZTransport read GetTransport;
  end;

  {** Implements an abstract class for all SQL test cases. }

  { TZAbstractSQLTestCase }

  /// <summary>
  ///   Base class for all Zeos (SQL) Test cases. All tests should be derived
  ///   from this class.
  /// </summary>
  TZAbstractSQLTestCase = class(TZAbstractTestCase, IZLoggingListener)
  private
    FCurrentConnectionConfig : TZConnectionConfig;
    FSkipSetup: boolean;
    FTraceList: TStrings;

    FProvider: TZServerProvider;
    FTransport: TZTransport;

    function GetLibLocation: string;
    function GetAlias: string;
    function GetConnectionName: string;
    function GetCreateScripts: TStringDynArray;
    function GetDatabase: string;
    function GetDropScripts: TStringDynArray;
    function GetHostName: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetProperties: TStringDynArray;
    function GetProtocol : string;
    function GetProtocolType: TProtocolType;
    function GetProvider: TZServerProvider;
    function GetTransport: TZTransport;
    function GetRebuild: Boolean;
    function GetUserName: string;
    procedure SetCurrentConnectionConfig(AValue: TZConnectionConfig);
  protected
    {$IFDEF WITH_CLASS_VARS}class var {$ENDIF} CVConnectionConfigs : TObjectList;
    {$IFDEF WITH_CLASS_VARS}class{$ENDIF} procedure LoadConfigurations;
    {$IFDEF WITH_CLASS_VARS}class{$ENDIF} property ConnectionConfigs: TObjectList read CVConnectionConfigs write CVConnectionConfigs;
    {$IFNDEF WITH_CLASS_VARS}procedure LoadConfiguration; override;{$ENDIF}
    property TraceList: TStrings read FTraceList write FTraceList;

    {$IFNDEF FPC}
    procedure RunWithFixture(TestResult: TTestResult); override;
    {$ELSE}
    procedure {%H-}Run(TestResult: TTestResult); override;
    {$ENDIF}

    function IsProtocolValid(Config: TZConnectionConfig): Boolean; virtual;
    function IsConfigUseValid(Config: TZConnectionConfig): Boolean; virtual;
    function IsNonASCIITest: Boolean; virtual;
    function GetSupportedProtocols: string; virtual;

    { Random values generators. }
    function RandomStr(Length: Integer): string;
    function RandomBts(Length: Integer): TBytes;
    function RandomInt(MinValue, MaxValue: Integer): Integer;
    function RandomFloat(MinValue, MaxValue: Double): Double;
    function RandomGUIDString: String;
    function RandomGUIDBytes: TBytes;
    function RandomGUID: TGUID;

    procedure StartSQLTrace;
    procedure StopSQLTrace;

    /// <summary>
    ///   Determines wether the current test can be run on the provided connection
    ///   configuration. Test cases using this method should return an empty string in
    ///   GetSupportedProtocols.
    /// </summary>
    function SupportsConfig(Config: TZConnectionConfig): Boolean; virtual;
  public
    destructor Destroy; override;

    procedure Fail(Msg: string; ErrorAddr: Pointer = nil);{$IFNDEF FPC}  override; {$ENDIF}
    procedure LogEvent(Event: TZLoggingEvent);

    procedure CheckEquals(Expected, Actual: TBCD;
      const Msg: string = ''); overload;
    { Different convenience methods. }
    function CreateDbcConnection: IZConnection; virtual;
    function CreateDatasetConnection: TZConnection; virtual;
    procedure PrintResultSet(ResultSet: IZResultSet;
      ShowTypes: Boolean; Note: string = '');

    function GetConnectionUrl(const Param: String): TZURL;

    { Properties to access active connection settings. }
    property ConnectionConfig:TZConnectionConfig read FCurrentConnectionConfig write SetCurrentConnectionConfig;
    property ConnectionName: string read GetConnectionName;
    property LibLocation: string read GetLibLocation;
    property Alias: string read GetAlias;
    property Protocol: string read GetProtocol;
    property ProtocolType: TProtocolType read GetProtocolType;
    property Provider: TZServerProvider read GetProvider;
    property Transport: TZTransport read GetTransport;
    property HostName: string read GetHostName;
    property Port: Integer read GetPort;
    property Database: string read GetDatabase;
    property UserName: string read GetUserName;
    property Password: string read GetPassword;
    property Rebuild: Boolean read GetRebuild;
    property CreateScripts: TStringDynArray read GetCreateScripts;
    property DropScripts: TStringDynArray read GetDropScripts;
    property Properties: TStringDynArray read GetProperties;
    property SkipSetup: Boolean read FSkipSetup;
  end;

  {** Implements a dbc test case which runs all active protocols. }

  { TZAbstractDbcSQLTestCase }

  TZAbstractDbcSQLTestCase = class (TZAbstractSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    property Connection: IZConnection read FConnection write FConnection;
  public
    procedure CheckEquals(Expected, Actual: TZSQLType;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: TZSQLType;
      const Msg: string = ''); overload;
  end;

  {** Implements a bug test case which runs all active protocols. }

  { TZAbstractCompSQLTestCase }

  TZAbstractCompSQLTestCase = class (TZAbstractSQLTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateQuery: TZQuery;
    function CreateReadOnlyQuery: TZReadOnlyQuery;
    function CreateTable: TZTable;
    property Connection: TZConnection read FConnection write FConnection;
  public
    function GetDBTestString(const Value: UnicodeString; Target: TZTransliterationType; MaxLen: Integer = -1): SQLString; overload;
    procedure CheckEquals(const OrgStr: UnicodeString; ActualLobStream: TStream;
      Actual: TField; ConSettings: PZConSettings; const Msg: string = ''); overload;
    procedure CheckEquals(const OrgStr: UnicodeString; Actual: TField; const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: TFieldType;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: TFieldType;
      const Msg: string = ''); overload;
    procedure CheckStringFieldType(Actual: TField; ControlsCodePage: TZControlsCodePage);
    procedure CheckStringParamType(Actual: TParam; ControlsCodePage: TZControlsCodePage);
    procedure CheckMemoParamType(Actual: TParam; ControlsCodePage: TZControlsCodePage);
    procedure CheckMemoFieldType(Actual: TField; ControlsCodePage: TZControlsCodePage);
  end;

  {** Implements a bug test case which runs all active protocols with MB-Chars }
  TZAbstractDbcSQLTestCaseMBCs = class (TZAbstractDbcSQLTestCase)
  protected
    function IsNonASCIITest: Boolean; override;
  end;

  {** Implements a bug test case which runs all active protocols with MB-Chars }
  TZAbstractCompSQLTestCaseMBCs = class (TZAbstractCompSQLTestCase)
  protected
    function IsNonASCIITest: Boolean; override;
  end;

{**
  Rebuilds test databases for the active connections
  in the specified test group.
  @param TestGroup a test group name. If the group is not set,
    the test group is taken from TestGroup global variable.
}
procedure RebuildTestDatabases(TestGroup: string = '');

Function ProtocolInProtocols(protocol : string; protocols : string) : boolean;

const
  MaxPerformanceLobSize = 10000;
  CharRange = 126{~}-32{Space};

implementation

uses
  Math, {$IFDEF ENABLE_POOLED}ZDbcPooled,{$ENDIF}
  ZSysUtils, ZEncoding, ZTestConfig, ZSqlProcessor, ZAbstractRODataset, ZDbcProperties,
  TypInfo;

function PropPos(const PropDynArray: TStringDynArray; const AProp: String): Integer; overload;
var
  I: Integer;
begin
  Result := -1;
  for i := 0 to high(PropDynArray) do
    if StartsWith(UpperCase(PropDynArray[i]), UpperCase(AProp)+'=') then
    begin
      Result := i;
      Break;
    end;
end;
function PropPos(const Current: TZConnectionConfig; const AProp: String): Integer; overload;
begin
  Result := PropPos(Current.Properties, AProp);
end;

procedure SetProperty(const Current: TZConnectionConfig; const AProp, NewValue: String);
var
  I: Integer;
begin
  I := PropPos(Current, AProp);
  if I > -1 then
    Current.Properties[i] := AProp+'='+NewValue
  else
  begin
    SetLength(Current.FProperties, Length(Current.FProperties) +1);
    Current.Properties[High(Current.Properties)] := AProp+'='+NewValue;
  end;
end;

function DefineDelimiterType(Value: string): TZDelimiterType;
begin
  if LowerCase(Value) = LowerCase(DEFAULT_DELIMITER) then
    Result := dtDefault
  else if LowerCase(Value) = LowerCase(GO_DELIMITER) then
    Result := dtGo
  else if LowerCase(Value) = LowerCase(SET_TERM_DELIMITER) then
    Result := dtSetTerm
  else if LowerCase(Value) = LowerCase(DELIMITER_DELIMITER) then
    Result := dtDelimiter
  else if LowerCase(Value) = LowerCase(EMPTY_LINE_DELIMITER) then
    Result := dtEmptyLine
  else Result := dtDefault;
end;

Function ProtocolInProtocols(protocol : string; protocols : string) : boolean;
var
  Temp: TStrings;
  TempName : string;
begin
  Temp := SplitString(protocols, LIST_DELIMITERS);
  TempName := Protocol;
  try
    {$IFDEF ENABLE_POOLED}
    If StartsWith(TempName,pooledprefix) then
      TempName := Copy(TempName,Length(PooledPrefix)+1,Length(TempName));
    {$ENDIF}
    Result := (Temp.IndexOf(TempName) >= 0);
  finally
    Temp.Free;
  end;
end;

{ TZConnectionConfig }

procedure TZConnectionConfig.SetConfigUses(AValue: TZConfigUses);
begin
  if FConfigUses=AValue then Exit;
  FConfigUses:=AValue;
end;

function TZConnectionConfig.GetConnectionUrl(const Param: String): TZURL;
var
  I: Integer;
begin
  Result := TZURL.Create;
  Result.Protocol := Protocol;
  Result.HostName := HostName;
  Result.Port := Port;
  Result.Database := Database;
  Result.UserName := UserName;
  Result.Password := Password;
  Result.LibLocation := LibLocation;

  for I := 0 to High(Properties) do
    Result.Properties.Add(Properties[I]);
  Result.Properties.Add(Param);
end;

function TZConnectionConfig.CreateDbcConnection: IZConnection;
var
  TempURL: TZURL;
begin
  TempURL := GetConnectionUrl('');
  try
    Result := DriverManager.GetConnection(TempURL.URL);
    {$IFDEF ZEOS_TEST_ONLY}
    Result.SetTestMode(ConnectionConfig.TestMode);
    {$ENDIF}
  finally
    TempURL.Free;
  end;
end;

function TZConnectionConfig.GetProtocolType: TProtocolType;
var Prot: string;
begin
  Prot := LowerCase(Protocol);
  for Result := Low(TProtocolType) to High(TProtocolType) do
    if StartsWith(Prot, ProtocolPrefixes[Result]) then
      Exit;
  Result := protUnknown;
end;

function TZConnectionConfig.GetProvider: TZServerProvider;
var
  Connection: IZConnection;
begin
  (*if FProvider <> spUnknown
  then Result := FProvider
  else begin*)
    case GetProtocolType of
      protMySQL: FProvider := spMySQL;
      protPostgre: FProvider := spPostgreSQL;
      protSQLite: FProvider :=spSQLite;
      protFirebird, protInterbase: FProvider := spIB_FB;
      protOracle: FProvider := spOracle;
      protASA,
      protASACAPI: FProvider := spASA;
      protMSSQL, protOleDB, protADO, protODBC, protSybase: begin
          Connection := CreateDbcConnection;
          Connection.Open;
          FProvider := Connection.GetServerProvider;
        end;
      else FProvider := spUnknown;
    end;
    Result := FProvider;
  //end;
end;

function TZConnectionConfig.GetTransport: TZTransport;
begin
  if FTransport <> traUnknown then begin
    Result := FTransport;
  end else case GetProtocolType of
    protMySQL, protPostgre, protSQLite, protFirebird, protInterbase, protOracle, protASA, protASACAPI, protMSSQL, protSybase: Result := traNative;
    protOleDB: Result := traOLEDB;
    protADO: Result := traADO;
    protODBC: Result := traODBC;
    protWebServiceProxy: Result := traWEBPROXY;
    else Result := traUnknown;
  end;
end;

constructor TZConnectionConfig.Create;
begin
  inherited Create;
  FExtendedTest := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_TEST_KEY, FALSE_VALUE));
  FExtended_cGet_ACP := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_CGET_ACP_KEY, FALSE_VALUE));
  FExtended_cGet_UTF8 := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_CCP_UTF8_KEY, FALSE_VALUE));
  FExtended_cGet_UTF16 := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_CCP_UTF16_KEY, FALSE_VALUE));
  FExtended_CodePages := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_CODEPAGES_KEY, FALSE_VALUE));
  FSkip_Performance := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    SKIP_PERFORMANCE_KEY, TRUE_VALUE));
  FTestMode := 0;
  FPerformanceFieldPropertiesDetermined := False;
end;

constructor TZConnectionConfig.Create(TemplateConfig: TZConnectionConfig; Suffix: String);
begin
  Create;
  if Suffix <> '' then
    FName := TemplateConfig.Name+'_'+Suffix
  else
    FName := TemplateConfig.Name;
  FLibLocation := TemplateConfig.LibLocation;
  FAlias := TemplateConfig.Alias;
  FProtocol := TemplateConfig.Protocol;
  FHostName := TemplateConfig.HostName;
  FPort := TemplateConfig.Port;
  FDatabase := TemplateConfig.Database;
  FUserName := TemplateConfig.UserName;
  FPassword := TemplateConfig.Password;
  FRebuild := TemplateConfig.Rebuild;
  FDelimiterType := TemplateConfig.DelimiterType;
  FDelimiter := TemplateConfig.Delimiter;
  FCreateScripts := TemplateConfig.CreateScripts;
  FDropScripts := TemplateConfig.DropScripts;
  FProperties := TemplateConfig.Properties;
  FCharacterSets := TemplateConfig.CharacterSets;
  FConfigUses := TemplateConfig.ConfigUses-[cuMainConnection];
end;

constructor TZConnectionConfig.Create(ConnectionName: String);
begin
  Create;
  FName := ConnectionName;
  FLibLocation := TestConfig.ReadProperty(FName, DATABASE_LIBLOCATION_KEY, '');
  FAlias := TestConfig.ReadProperty(FName, DATABASE_ALIAS_KEY, '');
  FProtocol := TestConfig.ReadProperty(FName, DATABASE_PROTOCOL_KEY, '');
  FHostName := TestConfig.ReadProperty(FName, DATABASE_HOST_KEY,
    DEFAULT_HOST_VALUE);
  FPort := StrToIntDef(TestConfig.ReadProperty(FName,
    DATABASE_PORT_KEY, ''), DEFAULT_PORT_VALUE);
  FDatabase := TestConfig.ReadProperty(FName, DATABASE_NAME_KEY, '');
  FUserName := TestConfig.ReadProperty(FName, DATABASE_USER_KEY, '');
  FPassword := TestConfig.ReadProperty(FName, DATABASE_PASSWORD_KEY, '');
  FRebuild := StrToBoolEx(TestConfig.ReadProperty(FName,
    DATABASE_REBUILD_KEY, FALSE_VALUE));
  FDelimiterType := DefineDelimiterType(
    TestConfig.ReadProperty(FName, DATABASE_DELIMITER_TYPE_KEY, ''));
  FDelimiter := TestConfig.ReadProperty(FName,
    DATABASE_DELIMITER_KEY, '');
  FCreateScripts := SplitStringToArray(TestConfig.ReadProperty(FName,
    DATABASE_CREATE_SCRIPTS_KEY, ''), LIST_DELIMITERS);
  FDropScripts := SplitStringToArray(TestConfig.ReadProperty(FName,
    DATABASE_DROP_SCRIPTS_KEY, ''), LIST_DELIMITERS);
  FProperties := SplitStringToArray(TestConfig.ReadProperty(FName,
    DATABASE_PROPERTIES_KEY, ''), ',;');
  FConfigUses := [cuMainConnection];
end;

destructor TZConnectionConfig.Destroy;
begin
  //writeln('Destroy '+FName);
  inherited Destroy;
end;

{**
  Creates the additional connection configurations for extended tests
}
procedure TZConnectionConfig.CreateExtendedConfigurations(ConnectionsList: TObjectList);
  {$IFDEF ZEOS_TEST_ONLY}
var  MaxTestMode, TestMode: Byte;
  {$ENDIF}

  procedure SetCharacterSets(const Current: TZConnectionConfig);
  var
    iCharacterSets: Integer;
    MyCurrent: TZConnectionConfig;
  begin
    if FExtended_CodePages then
    for iCharacterSets := 0 to high(Current.CharacterSets) do
      begin
        MyCurrent := TZConnectionConfig.Create(Current, Current.CharacterSets[iCharacterSets]);
        //Writeln(MyCurrent.Name);
        SetProperty(MyCurrent, ConnProps_CodePage,Current.CharacterSets[iCharacterSets]);
        ConnectionsList.Add(MyCurrent);
      end;
  end;
  procedure SetCtrlsCPTypes(const Current: TZConnectionConfig);
  var
    MyCurrent: TZConnectionConfig;

    procedure CloneConfig(CPType:String);
    begin
      if CPType = '' then
        MyCurrent := Current
      else
      begin
        MyCurrent := TZConnectionConfig.Create(Current, CPType);
        MyCurrent.ConfigUses:=MyCurrent.ConfigUses+[cuNonAscii];
        //Writeln(MyCurrent.Name);
        SetProperty(MyCurrent, ConnProps_ControlsCP,CPType);
        ConnectionsList.Add(MyCurrent);
      end;
    end;
  begin

    { GET_ACP is supported for all compiler}
    if FExtended_cGet_ACP then
      {$IF defined(DELPHI) and not defined(UNICODE))}
      CloneConfig(''); //GET_ACP is default for Ansi-Delphi -> no clone!
      {$ELSE}
      CloneConfig('GET_ACP');
      {$IFEND}

    { CP_UTF8 is not supported for Unicode compiler }
    {$IFNDEF UNICODE}
    if FExtended_cGet_UTF8 then
      {$IFDEF FPC}
      CloneConfig(''); //CP_UTF8 is FPC/LCL default -> no clone!
      {$ELSE}
      CloneConfig('CP_UTF8');
      {$ENDIF}
    {$ENDIF}

    { CP_UTF16 (Wide-Field) is not supported for D7 and older FPC }
    if FExtended_cGet_UTF16 then
      {$IFDEF UNICODE}
      CloneConfig(''); //CP_UTF16 is default for D12_UP -> no clone!
      {$ELSE}
      CloneConfig('CP_UTF16');
      {$ENDIF}

    if not (FExtended_cGet_ACP or FExtended_cGet_UTF8 or FExtended_cGet_UTF16) then
      CloneConfig('');
  end;
  procedure create_charsets_encodings(ConnectionConfig: TZConnectionConfig);
  var
    TempCharacterSets: TStringDynArray;
  begin
    TempCharacterSets := SplitStringToArray(TestConfig.ReadProperty(Self.Name,
      DATABASE_CHARACTERSETS_KEY, ''), LIST_DELIMITERS);
    if PropPos(ConnectionConfig, ConnProps_CodePage) > -1 then //add a empty dummy value to get the autodetecting running for PG for example
      SetLength(TempCharacterSets, Length(TempCharacterSets)+1);
    ConnectionConfig.CharacterSets := TempCharacterSets;

    SetCtrlsCPTypes(Self);
  end;
begin
  if ExtendedTest then
  begin
    create_charsets_encodings(self);
  end;

  {$IFDEF ZEOS_TEST_ONLY}
  // Test Modes
  MaxTestMode := 0;
  if ProtocolInProtocols(self.Protocol,pl_all_postgresql) then
    MaxTestMode := 1;
  For TestMode := 1 to MaxTestMode do
  begin
    //writeln('create TestMode_'+IntToStr(TestMode));
    TempConfig := TZConnectionConfig.Create(Self, 'test_mode_'+IntToStr(TestMode));
    ConnectionsList.Add(TempConfig);
    TempConfig.TestMode := TestMode;
    if ExtendedTest then
    begin
      create_charsets_encodings(TempConfig);
    end;
  end;
  {$ENDIF}
end;

{ TZAbstractSQLTestCase }

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractSQLTestCase.Destroy;
begin
  {$IFNDEF WITH_CLASS_VARS}
  FreeAndNil(CVConnectionConfigs);
  {$ENDIF}
  FreeAndNil(FTraceList);

  inherited Destroy;
end;

function TZAbstractSQLTestCase.GetProtocol: string;
begin
  Result := FCurrentConnectionConfig.Protocol;
  {$IFDEF ENABLE_POOLED}
  If StartsWith(Result,pooledprefix) then
    Result := Copy(Result,Length(PooledPrefix)+1,Length(Result));
  {$ENDIF}
end;

function TZAbstractSQLTestCase.GetProtocolType: TProtocolType;
var Prot: string;
begin
  Prot := LowerCase(Protocol);
  for Result := Low(TProtocolType) to High(TProtocolType) do
    if StartsWith(Prot, ProtocolPrefixes[Result]) then
      Exit;
  Result := protUnknown;
end;

function TZAbstractSQLTestCase.GetProvider: TZServerProvider;
var
  Connection: IZConnection;
begin
  {if FProvider <> spUnknown
  then Result := FProvider
  else begin}
    case GetProtocolType of
      protMySQL: FProvider := spMySQL;
      protPostgre: FProvider := spPostgreSQL;
      protSQLite: FProvider :=spSQLite;
      protFirebird, protInterbase: FProvider := spIB_FB;
      protOracle: FProvider := spOracle;
      protASA: FProvider := spASA;
      protMSSQL, protOleDB, protADO, protASACAPI, protODBC, protSybase: begin
          Connection := CreateDbcConnection;
          Connection.Open;
          FProvider := Connection.GetServerProvider;
        end;
      else FProvider := spUnknown;
    end;
    Result := FProvider;
  //end;
end;

function TZAbstractSQLTestCase.GetTransport: TZTransport;
begin
  if FTransport <> traUnknown then begin
    Result := FTransport;
  end else case GetProtocolType of
    protMySQL, protPostgre, protSQLite, protFirebird, protInterbase, protOracle, protASA, protASACAPI, protMSSQL, protSybase: Result := traNative;
    protOleDB: Result := traOLEDB;
    protADO: Result := traADO;
    protODBC: Result := traODBC;
    protWebServiceProxy: Result := traWEBPROXY;
    else Result := traUnknown;
  end;
end;

function TZAbstractSQLTestCase.GetRebuild: Boolean;
begin
  Result := FCurrentConnectionConfig.Rebuild;
end;

function TZAbstractSQLTestCase.GetUserName: string;
begin
  Result := FCurrentConnectionConfig.UserName;
end;

procedure TZAbstractSQLTestCase.SetCurrentConnectionConfig(
  AValue: TZConnectionConfig);
begin
  if FCurrentConnectionConfig=AValue then Exit;
  FCurrentConnectionConfig:=AValue;
end;

function TZAbstractSQLTestCase.GetLibLocation: string;
begin
  if FCurrentConnectionConfig.LibLocation = ''
  then Result := FCurrentConnectionConfig.LibLocation
  else Result := TestConfig.PathFromRoot(FCurrentConnectionConfig.LibLocation);
end;

function TZAbstractSQLTestCase.GetAlias: string;
begin
  Result := FCurrentConnectionConfig.Alias;
end;

function TZAbstractSQLTestCase.GetConnectionName: string;
begin
  Result := FCurrentConnectionConfig.Name;
end;

function TZAbstractSQLTestCase.GetCreateScripts: TStringDynArray;
begin
  Result := FCurrentConnectionConfig.CreateScripts;
end;

function TZAbstractSQLTestCase.GetDatabase: string;
begin
  // Database could be defined as alias, absolute path or relative path
  // Consider it a path if a PathDelim is encountered
{ EH@Fr0sT:
  Driver=SQL Server Native Client 11.0;Initial Catalog=zeoslib;Data Source=EGONDEVLAPTOPW7\SQLEXPRESS;Workstation ID=EGONDEVLAPTOPW7
  is also a valid database name... so this code wrongly adds path data..
  if Pos(PathDelim, FCurrentConnectionConfig.Database) > 0 then
    Result := TestConfig.PathFromRoot(FCurrentConnectionConfig.Database)
  else}
    Result := FCurrentConnectionConfig.Database;
end;

function TZAbstractSQLTestCase.GetDropScripts: TStringDynArray;
begin
  Result := FCurrentConnectionConfig.DropScripts;
end;

function TZAbstractSQLTestCase.GetHostName: string;
begin
  Result := FCurrentConnectionConfig.HostName;
end;

function TZAbstractSQLTestCase.GetPassword: string;
begin
  Result := FCurrentConnectionConfig.Password;
end;

function TZAbstractSQLTestCase.GetPort: Integer;
begin
  Result := FCurrentConnectionConfig.Port;
end;

function TZAbstractSQLTestCase.GetProperties: TStringDynArray;
begin
  Result := FCurrentConnectionConfig.Properties;
end;

{**
  Overrides a fail method which prints an error message.
  @param Msg an error message string.
  @param ErrorAddr an address where error happened.
}
procedure TZAbstractSQLTestCase.Fail(Msg: string; ErrorAddr: Pointer = nil);
begin
  {$IFNDEF WITH_TESTCASE_ERROR_ADDRESS}
  inherited Fail(Format('%s/%s: %s', [ConnectionName, Protocol, Msg]));
  {$ELSE}
  inherited Fail(Format('%s/%s: %s', [ConnectionName, Protocol, Msg]),
    ErrorAddr);
  {$ENDIF}
end;

{**
  Handles a new incoming logging event.
  @param Event an incoming logging event.
}
procedure TZAbstractSQLTestCase.LogEvent(Event: TZLoggingEvent);
begin
  if Event.Message <> '' then
    FTraceList.Append(String(Event.Message));
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZAbstractSQLTestCase.IsProtocolValid(Config: TZConnectionConfig): Boolean;
begin
  if GetSupportedProtocols = '' then
    Result := True
  else
    Result := ProtocolInProtocols(Config.Protocol,GetSupportedProtocols);
end;

function TZAbstractSQLTestCase.IsConfigUseValid(
  Config: TZConnectionConfig): Boolean;
var selection, Objection: Boolean;
begin
  Objection := False;
  Selection := (cuMainConnection in Config.ConfigUses);  //main configured connections are always valid
  If (cuNonAscii in Config.ConfigUses) then
  begin
    Selection := True;
    Objection := Objection or Not(isNonASciiTest) //Non Ascii connections are only valid for NonAscii tests
  end;
  If Config.TestMode > 0 then
    Selection := True;
  Result := Selection and not(Objection);
end;

{**
  Is the current test US-ASCII encoded?
  @return True if Test is ASCII encoded
}
function TZAbstractSQLTestCase.IsNonASCIITest: Boolean;
begin
  Result := False;
end;

function TZAbstractSQLTestCase.GetSupportedProtocols: string;
begin
  result := '';
end;

{**
  Generates a random float value between MinValue and MaxValue.
  @param MinValue a minimum limit value.
  @param MaxValue a maximum limit value.
  @return a random generated value.
}
function TZAbstractSQLTestCase.RandomFloat(MinValue,
  MaxValue: Double): Double;
begin
  Result := (MinValue * 100 + Random(Trunc((MaxValue - MinValue) * 100))) / 100;
end;

function TZAbstractSQLTestCase.RandomGUIDString: String;
{$IFDEF WITH_FTGUID}
var GUID: TGUID;
{$ENDIF}
begin
  {$IFDEF WITH_FTGUID}
  if CreateGuid(GUID) = S_OK then
    Result := GUIDToString(GUID)
  else
  {$ENDIF}
    Result := '{BAF24A92-C8CE-4AB4-AEBC-3D4A9BCB0946}';
end;

function TZAbstractSQLTestCase.RandomGUIDBytes: TBytes;
{$IFDEF WITH_FTGUID}
var GUID: TGUID;
{$ENDIF}
begin
  Result := nil;
  SetLength(Result, 16);
  {$IFDEF WITH_FTGUID}
  if CreateGuid(GUID) = S_OK then
    System.Move(Pointer(@GUID)^, Pointer(Result)^, 16)
  else
  {$ENDIF}
    Result := RandomBts(16);
end;

function TZAbstractSQLTestCase.RandomGUID: TGUID;
{$IFDEF FPC}
var Bts: TBytes;
  GUID: TGUID absolute BTS;
{$ENDIF}
begin
  {$IFDEF FPC}
  Bts := RandomGUIDBytes;
  Result := GUID;
  {$ELSE}
  CreateGuid(Result);
  {$ENDIF}
end;

{**
  Generates a random integer value between MinValue and MaxValue.
  @param MinValue a minimum limit value.
  @param MaxValue a maximum limit value.
  @return a random generated value.
}
function TZAbstractSQLTestCase.RandomInt(MinValue,
  MaxValue: Integer): Integer;
begin
  Result := MinValue + Random(MaxValue - MinValue);
end;

{**
  Generates a random string with the specified length.
  @param Length a string length (default is 32).
  @return a random generated value.
}
function TZAbstractSQLTestCase.RandomStr(Length: Integer): string;
var
  I: Integer;
  PResult: PChar;
begin

  Result := '';
  if Length <= 0 then
    Length := 32
  else
    Length := Min(MaxPerformanceLobSize, Length);
  SetLength(Result, Length);
  PResult := PChar(Result);
  for I := 0 to Length-1 do
    (PResult+i)^ := Chr(Random(CharRange)+32{min Space});
end;

{**
  Generates a random binary value with the specified length.
  @param Length a ByteArray length (default is 32).
  @return a random generated value.
}
function TZAbstractSQLTestCase.RandomBts(Length: Integer): TBytes;
var
  I: Integer;
begin
  if Length <= 0 then
    Length := 32
  else
    Length := Min(MaxPerformanceLobSize, Length);
  Result := nil;
  SetLength(Result, Length);
  for I := 1 to Length do
    Result[i-1] := Ord(Random(255));
end;

{**
  Loads all configuration from the configuration file.
}
{$IFDEF WITH_CLASS_VARS}class{$ENDIF}procedure TZAbstractSQLTestCase.LoadConfigurations;

var
  I: Integer;
  Temp: string;
  ActiveConnections: TStringDynArray;
  Current: TZConnectionConfig;
begin
  { Resets a connection configuration list. }
  if not Assigned(CVConnectionConfigs) then
    CVConnectionConfigs := TObjectList.Create
  else CVConnectionConfigs.Clear;

  { Reads a list with active database connections. }
  Temp := ReadInheritProperty(ACTIVE_CONNECTIONS_KEY, NONE_VALUE);
  if UpperCase(Temp) = UpperCase(NONE_VALUE) then
    Temp := '';
  ActiveConnections := SplitStringToArray(Temp, LIST_DELIMITERS);

  for I := 0 to High(ActiveConnections) do
  begin
    Current := TZConnectionConfig.Create(ActiveConnections[I]);
    Current.ConfigUses:=[cuMainConnection];
    //Writeln('Master Connection : '+Current.Name);

    {$IFNDEF ENABLE_ADO}
    if Current.Protocol = 'ado' then
    begin
      Current.Free;
      continue;
    end;
    {$ENDIF}

    CVConnectionConfigs.Add(Current);
    Current.CreateExtendedConfigurations(CVConnectionConfigs);
  end;
end;

{$IFNDEF WITH_CLASS_VARS}
procedure TZAbstractSQLTestCase.LoadConfiguration;
begin
  inherited LoadConfiguration;
  LoadConfigurations;
end;
{$ENDIF}

{**
  Starts logging outgoing SQL statements.
}
procedure TZAbstractSQLTestCase.StartSQLTrace;
begin
  if FTraceList = nil then
    FTraceList := TStringList.Create
  else FTraceList.Clear;
  DriverManager.AddLoggingListener(Self);
end;

{**
  Clean ups test after finish.
}
procedure TZAbstractSQLTestCase.StopSQLTrace;
begin
  DriverManager.RemoveLoggingListener(Self);
end;

function TZAbstractSQLTestCase.SupportsConfig(Config: TZConnectionConfig): Boolean;
begin
  Result := true;
end;


{$IFNDEF FPC}
{**
   Function configure test paramters and start test case
   <b>Note:</b> Configuration file ZSqlConfig.ini should exist and contain
    the appropriate section with settings of the protocol
}
procedure TZAbstractSQLTestCase.RunWithFixture(TestResult: TTestResult);
var
  I: Integer;
begin
  for I := 0 to ConnectionConfigs.Count - 1 do
    if IsProtocolValid(TZConnectionConfig(ConnectionConfigs[I])) and
       IsConfigUseValid(TZConnectionConfig(ConnectionConfigs[I])) and
       SupportsConfig(ConnectionConfigs[I] as TZConnectionConfig) then
    begin
      ConnectionConfig := TZConnectionConfig(ConnectionConfigs[I]);
    //writeln('Using : '+Current.Name);
      inherited RunWithFixture(TestResult);
    end;
end;
{$ELSE}
{**
   Function configure test paramters and start test case
   <b>Note:</b> Configuration file ZSqlConfig.ini should exist and contain
    the appropriate section with settings of the protocol
}
procedure TZAbstractSQLTestCase.Run(TestResult: TTestResult);
var
  I: Integer;
begin
  for I := 0 to ConnectionConfigs.Count - 1 do
    if IsProtocolValid(TZConnectionConfig(ConnectionConfigs[I])) and
       IsConfigUseValid(TZConnectionConfig(ConnectionConfigs[I])) and
       SupportsConfig(ConnectionConfigs[I] as TZConnectionConfig) then
    begin
      ConnectionConfig := TZConnectionConfig(ConnectionConfigs[I]);
      //writeln('Using : '+ConnectionName);
      inherited Run(TestResult);
    end;
end;
{$ENDIF}

{**
  Creates a database ZDBC connection object.
  @return a created database ZDBC connection object.
}
function TZAbstractSQLTestCase.CreateDbcConnection: IZConnection;
begin
  Result := FCurrentConnectionConfig.CreateDbcConnection;
end;

procedure TZAbstractSQLTestCase.CheckEquals(Expected, Actual: TBCD;
  const Msg: string);
begin
  Check(ZSysUtils.ZBCDCompare(Expected, Actual) = 0, Msg);
end;

{**
  Creates a database connection component compatible with TZDataset.
  @return a created database connection component.
}
function TZAbstractSQLTestCase.CreateDatasetConnection: TZConnection;
var
  I: Integer;
begin
  Result := TZConnection.Create(nil);
  Result.LibLocation := LibLocation;
  Result.Protocol := Protocol;
  Result.Port := Port;
  Result.HostName := HostName;
  Result.Database := Database;
  Result.User := UserName;
  Result.Password := Password;
  Result.LoginPrompt := False;
  for I := 0 to High(Properties) do begin
    if StartsWith(Properties[I], ConnProps_ControlsCP) then begin
      if EndsWith(Properties[I], 'GET_ACP')
      then Result.ControlsCodePage := cGET_ACP
      else {$IFNDEF UNICODE}if EndsWith(Properties[I], 'CP_UTF8')
        then Result.ControlsCodePage := cCP_UTF8
        else {$ENDIF}if EndsWith(Properties[I], 'CP_UTF16')
          then Result.ControlsCodePage := cCP_UTF16
          else Result.ControlsCodePage := cDynamic;
    end;
    Result.Properties.Add(Properties[I])
  end;
  {$IFDEF ZEOS_TEST_ONLY}
  Result.TestMode := ConnectionConfig.TestMode;
  {$ENDIF}
end;

{**
  Prints a content of the result set from the first to the last row.
  @param ResultSet a result set object.
  @param ShowTypes a show types flag.
}
procedure TZAbstractSQLTestCase.PrintResultSet(ResultSet: IZResultSet;
  ShowTypes: Boolean; Note: string = '');
var
  I: Integer;
  Metadata: IZResultSetMetadata;
  Stream: TStream;
  Buffer: array[0..100] of Char;
  ReadNum: Integer;
begin
  if not SuppressTestOutput then
  begin
    if Note <> '' then
    begin
      System.Writeln;
      System.Writeln('====================================');
      System.Writeln(Note);
      System.Writeln('====================================');
    end;

    Metadata := ResultSet.GetMetaData;
    for I := 1 to Metadata.GetColumnCount do
    begin
      System.Write(Metadata.GetColumnLabel(I));
      if ShowTypes then
        System.Write(':', Metadata.GetColumnTypeName(I));
      System.Write(' ');
    end;
    System.Writeln;
    System.Writeln('====================================');

    while ResultSet.Next do
    begin
      for I := 1 to Metadata.GetColumnCount do
      begin
        if ResultSet.IsNull(I) then
        begin
          System.Write('NULL ');
          Continue;
        end;

        case Metadata.GetColumnType(I) of
          stAsciiStream:
            begin
              Stream := ResultSet.GetAsciiStream(I);
              if Stream <> nil then
              begin
                try
                  ReadNum := Stream.Read(Buffer{%H-}, 100);
                  System.Write('''' + BufferToStr(Buffer, ReadNum) + '''');
                finally
                  Stream.Free;
                end;
              end else
                System.Write('!ERROR!');
            end;
          stBinaryStream:
            begin
              Stream := ResultSet.GetBinaryStream(I);
              if Stream <> nil then
              begin
                try
                  ReadNum := Stream.Read(Buffer, 10);
                  System.Write('''' + BufferToStr(Buffer, ReadNum) + '...''');
                finally
                  Stream.Free;
                end;
              end else
                System.Write('!ERROR!');
            end;
          else
            System.Write('''' + ResultSet.GetString(I) + '''');
        end;
        System.Write(' ');
      end;
      System.Writeln;
    end;
    ResultSet.BeforeFirst;
    System.Writeln('====================================');
    System.Writeln;
  end;
end;

{**
  Gets a connection URL object.
  @return a built connection URL object.
}
function TZAbstractSQLTestCase.GetConnectionUrl(const Param: String): TZURL;
begin
  Result := FCurrentConnectionConfig.GetConnectionUrl(Param);
end;

type
  {** Implements a supplementary generic SQL test case. }

  { TZSupplementarySQLTestCase }

  TZSupplementarySQLTestCase = class (TZAbstractSQLTestCase)
  private
    FTestGroup: string;
    FSQLProcessor: TZSQLProcessor;
  protected
    procedure SuppressError(Processor: TZSQLProcessor; StatementIndex: Integer;
      E: Exception; var ErrorHandleAction: TZErrorHandleAction);
    procedure RaiseError(Processor: TZSQLProcessor; StatementIndex: Integer;
      E: Exception; var ErrorHandleAction: TZErrorHandleAction);

    procedure ExecuteScripts(FileNames: TStringDynArray;
      RaiseException: Boolean);
  public
    constructor CreateWithGroup(TestGroup: string);
    destructor Destroy; override;

    procedure RebuildDatabases;
  end;


{ TZSupplementarySQLTestCase }

{**
  Constructs this test cases and assignes a group name.
  @param TestGroup a name of the test group.
}
constructor TZSupplementarySQLTestCase.CreateWithGroup(TestGroup: string);
begin
  FTestGroup := TestGroup;
  FSQLProcessor := TZSQLProcessor.Create(nil);
end;

{**
  Destroyes this object and clean ups the memory.
}
destructor TZSupplementarySQLTestCase.Destroy;
begin
  FSQLProcessor.Free;
  inherited Destroy;
end;

{**
  Handles exceptions in SQL script processing and raise them.
}
procedure TZSupplementarySQLTestCase.RaiseError(Processor: TZSQLProcessor;
  StatementIndex: Integer; E: Exception;
  var ErrorHandleAction: TZErrorHandleAction);
begin
  ErrorHandleAction := eaFail;
end;

{**
  Handles exceptions in SQL script processing and suppress them.
}
procedure TZSupplementarySQLTestCase.SuppressError(
  Processor: TZSQLProcessor; StatementIndex: Integer; E: Exception;
  var ErrorHandleAction: TZErrorHandleAction);
begin
  System.WriteLn('Database Rebuild Error: ' + E.Message);
  ErrorHandleAction := eaSkip;
end;

{**
  Executes a list of SQL scripts.
  @param FileNames an array with file names.
  @param RaiseException <code>False</code> to suppress exceptions.
}
procedure TZSupplementarySQLTestCase.ExecuteScripts(
  FileNames: TStringDynArray; RaiseException: Boolean);
var
  I: Integer;
  ScriptPath: string;
  ScriptName: String;
begin
  { Sets the right error event handler. }
  if RaiseException then
    FSQLProcessor.OnError := RaiseError
  else FSQLProcessor.OnError := SuppressError;

  ScriptPath := TestConfig.ScriptPath;

  for I := 0 to High(FileNames) do
  begin
    FSQLProcessor.Script.Clear;
    try
      ScriptName := ScriptPath + FileNames[I];
      Writeln('Executing ' + ScriptName);
      FSQLProcessor.Script.LoadFromFile(ScriptName);
      // To avoid parameter handling while rebuild! Parameters must not be handled!
      FSQLProcessor.ParamCheck := false;
    except
      System.WriteLn(Format('File %s can not be opened and executed.',
        [FileNames[I]]));
      if RaiseException then
        raise;
    end;

    if FSQLProcessor.Script.Count > 0 then
    begin
      try
        FSQLProcessor.Execute;
      except
        on E: Exception do
        begin
          System.WriteLn('Database Rebuild Error: ' + E.Message);
          if RaiseException then
            raise;
        end;
      end;
    end;
  end;
end;

{**
  Rebuilds databases, configured for this test group.
}
procedure TZSupplementarySQLTestCase.RebuildDatabases;
var
  I: Integer;
  Connection: TZConnection;
begin
{$IFNDEF WITH_CLASS_VARS}
  if not Assigned(ConnectionConfigs) or (ConnectionConfigs.Count = 0) then
    LoadConfiguration;
{$ENDIF}
  for I := 0 to ConnectionConfigs.Count - 1 do
    if IsProtocolValid(TZConnectionConfig(ConnectionConfigs[I])) and
       IsConfigUseValid(TZConnectionConfig(ConnectionConfigs[I])) then
    begin
      ConnectionConfig := TZConnectionConfig(ConnectionConfigs[I]);
      //Writeln('Rebuilding '+ConnectionName);
      Connection := CreateDatasetConnection;
      try
        FSQLProcessor.Connection := Connection;
        FSQLProcessor.Delimiter := ConnectionConfig.Delimiter;
        FSQLProcessor.DelimiterType := ConnectionConfig.DelimiterType;

        if Rebuild then
        begin
          ExecuteScripts(DropScripts, False);
          ExecuteScripts(CreateScripts, True);
        end;
      finally
        Connection.Free;
      end;
  end;
end;

{ TZAbstractDbcSQLTestCase}

procedure TZAbstractDbcSQLTestCase.CheckNotEquals(Expected, Actual: TZSQLType;
  const Msg: string);
var E, A: String;
begin
  E := TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Expected));
  A := TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Expected));
  inherited CheckNotEquals(E, A, Msg);
end;

procedure TZAbstractDbcSQLTestCase.SetUp;
begin
  FConnection := CreateDbcConnection;
end;

procedure TZAbstractDbcSQLTestCase.TearDown;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Close;
    FConnection := nil;
  end;
end;

procedure TZAbstractDbcSQLTestCase.CheckEquals(Expected, Actual: TZSQLType;
  const Msg: string = '');
var E, A: String;
begin
  E := TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Expected));
  A := TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Expected));
  inherited CheckEquals(E, A, Msg);
end;

{ TZAbstractCompSQLTestCase }

procedure TZAbstractCompSQLTestCase.SetUp;
begin
  FConnection := CreateDatasetConnection;
end;

procedure TZAbstractCompSQLTestCase.TearDown;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Disconnect;
    FConnection.Free;
    FConnection := nil;
  end;
end;

type THackDataSet = class(TZAbstractRODataSet);
procedure TZAbstractCompSQLTestCase.CheckEquals(const OrgStr: UnicodeString;
  ActualLobStream: TStream; Actual: TField; ConSettings: PZConSettings;
  const Msg: string = '');
var
  StrStream: TMemoryStream;
  procedure SetAnsiStream(const Value: RawByteString);
  begin
    StrStream.Write(PAnsiChar(Value)^, Length(Value));
    StrStream.Position := 0;
  end;
begin
  StrStream := TMemoryStream.Create;
  {$IFDEF WITH_WIDEMEMO}
  if Actual.InheritsFrom(TWideMemoField) then begin
    StrStream.Write(PWideChar(OrgStr)^, Length(OrgStr)*2);
    StrStream.Position := 0;
  end else {$ENDIF}begin
    Check(Actual.InheritsFrom(TMemoField), 'Should be a MemoField');
    if TMemoField(Actual).Transliterate or (ConSettings.ClientCodePage.Encoding = ceUTF16)
    then SetAnsiStream(ZUnicodeToRaw(OrgStr, GetTransliterateCodePage(TZAbstractRODataset(Actual.DataSet).Connection.ControlsCodePage)))
    else SetAnsiStream(ZUnicodeToRaw(OrgStr, ConSettings.ClientCodePage.CP))
  end;
  try
    CheckEquals(StrStream, ActualLobStream, Msg);
  finally
    StrStream.Free;
  end;
end;

{**
   Function compare two strings with depenedent to the ConnectionSettings.
   If strings not equals raise exception.
   @param Expected the first stream for compare
   @param Actual the second stream for compare
   @param ConSettings the Connection given settings
}
procedure TZAbstractCompSQLTestCase.CheckEquals(const OrgStr: UnicodeString;
  Actual: TField; const Msg: string = '');
var ATemp, ATemp2: AnsiString;
    WTmp: UnicodeString;
    Stream: TMemoryStream;
    Idx, Size: Integer;
    ControlsCodePage: TZControlsCodePage;
    ColumnCP, StringCP{, TransliterateCP}: Word;
    ConSettings: PZConSettings;
    SQLType: TZSQLType;
begin
  with THackDataSet(Actual.DataSet) do begin
    Idx := ResultSetMetadata.FindColumn(Actual.FieldName);
    Check(Idx > InvalidDbcIndex, Protocol+': the native column index was not found');
    ConSettings := ResultSet.GetConSettings;
    ControlsCodePage := Connection.ControlsCodePage;
    SQLType := ResultSetMetadata.GetColumnType(Idx);
    Size := ResultSetMetadata.GetPrecision(idx);
    if ConSettings.ClientCodePage.Encoding = ceUTF16 then begin
      ColumnCP := zCP_UTF16;
      StringCP := GetTransliterateCodePage(ControlsCodepage);
    end else begin
      ColumnCP := THackDataSet(Actual.DataSet).ResultSetMetadata.GetColumnCodePage(Idx);
      case THackDataSet(Actual.DataSet).Connection.RawCharacterTransliterateOptions.Encoding of
        encDB_CP: StringCP := ConSettings.ClientCodePage.CP;
        encUTF8: StringCP := zCP_UTF8;
        else StringCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF};
      end;
    end;
  end;
  //TransliterateCP := THackDataSet(Actual.DataSet).Connection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttField);
  if Actual.InheritsFrom(TWideStringField) then begin
    WTmp := TWideStringField(Actual).Value;
    CheckEquals(OrgStr, WTmp, Protocol+': The UTF16 value of Field: '+Actual.FieldName);
    Check((ControlsCodePage = cCP_UTF16) or ((ControlsCodePage = cDynamic) and (ColumnCP = zCP_UTF16)), Protocol+': The FieldType-W type');
    {$IFNDEF UNICODE}
      {$IFDEF WITH_DEFAULSYSTEMCODEPAGE}
    ATemp := ZUnicodeToRaw(OrgStr, StringCP);
      {$ELSE}
    ATemp := AnsiString(OrgStr);
      {$ENDIF}
    CheckEquals(ATemp, Actual.AsString, Protocol+': The raw string of Field '+Actual.FieldName);
    {$ENDIF}
    {$IFNDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}
    if (Size <= 0) or (SQLType in [stAsciiStream, stUnicodeStream]) then begin
      CheckEquals((MaxInt shr 1)-2, Actual.Size, Protocol+': The Size of the Field: '+Actual.FieldName);
      Check((SQLType in [stAsciiStream, stUnicodeStream]) or ((SQLType in [stString, stUnicodeString]) and (Size <= 0)),
        Protocol+': The underlaying SQLType of the Field: '+Actual.FieldName);
      Exit;
    end;{$ENDIF}
    CheckEquals(Size, Actual.Size, Protocol+': The Size of the Field '+Actual.FieldName);
    Check((SQLType in [stString, stUnicodeString]) ,
      Protocol+': The underlaying SQLType of the Field: '+Actual.FieldName);
  end else {$IFDEF WITH_WIDEMEMO}if Actual.InheritsFrom(TWideMemoField) then begin
    CheckEquals(OrgStr, TWideMemoField(Actual).Value, Protocol+': The UTF16 value of Field: '+Actual.FieldName);
    Check((ControlsCodePage = cCP_UTF16) or ((ControlsCodePage = cDynamic) and (ColumnCP = zCP_UTF16)), Protocol+': The FieldType-W type');
    Stream := TMemoryStream.Create;
    try
      TWideMemoField(Actual).SaveToStream(Stream);
      CheckEquals(Length(OrgStr) shl 1, Stream.Size, Protocol+': The Stream Size');
      WTmp := '';
      SetLength(WTmp, Length(OrgStr));
      Stream.Position := 0;
      Stream.Read(Pointer(WTmp)^, Stream.Size);
      CheckEquals(OrgStr, WTmp, Protocol+': The string read back from W-Steam');
    finally
      Stream.Free;
    end;
    {$IFNDEF UNICODE}
    ATemp := ZUnicodeToRaw(OrgStr, StringCP);
    CheckEquals(ATemp, Actual.AsString, Protocol+' The raw string of Field '+Actual.FieldName);
    {$ENDIF}
    Check((SQLType in [stAsciiStream, stUnicodeStream]) or ((SQLType in [stString, stUnicodeString]) and (Size <= 0)),
      Protocol+': The underlaying SQLType of the Field: '+Actual.FieldName);
  end else {$ENDIF} if Actual.InheritsFrom(TStringField) then begin
    if (ColumnCP = zCP_UTF16) or (TStringField(Actual).Transliterate) then
      StringCP := GetTransliterateCodePage(ControlsCodepage);
    ATemp := ZUnicodeToRaw(OrgStr, StringCP);
    ATemp2 := TStringField(Actual).{$IFDEF WITH_ASANSISTRING}AsAnsiString{$ELSE}AsString{$ENDIF};
    CheckEquals(ATemp, Atemp2, Protocol+': The raw value of Field: '+Actual.FieldName);
    Check(ControlsCodePage in [cGET_ACP, {$IFNDEF UNICODE}cCP_UTF8,{$ENDIF}cDynamic], Protocol+': The FieldType-A type');
    CheckEquals(Size, Actual.Size, Protocol+': The Size of the StringField '+Actual.FieldName);
    Check((SQLType in [stString, stUnicodeString]) ,
      Protocol+': The underlaying SQLType of the Field: '+Actual.FieldName);
  end else if Actual.InheritsFrom(TMemoField) then begin
    Stream := TMemoryStream.Create;
    if (ColumnCP = zCP_UTF16) or TMemoField(Actual).Transliterate then begin
      StringCP := GetTransliterateCodePage(ControlsCodepage);
      ATemp := ZUnicodeToRaw(OrgStr, StringCP);
    end else ATemp := ZUnicodeToRaw(OrgStr, ColumnCP);
    try
      TMemoField(Actual).SaveToStream(Stream);
      CheckEquals(Length(ATemp), Stream.Size, Protocol+': The Stream Size');
      SetLength(ATemp2, Length(ATemp));
      Stream.Position := 0;
      Stream.Read(Pointer(ATemp2)^, Stream.Size);
      CheckEquals(ATemp, ATemp2, Protocol+': The raw string read back from A-Stream');
    finally
      Stream.Free;
    end;
    if (ColumnCP = zCP_UTF16) or (TMemoField(Actual).Transliterate and (StringCP <> ColumnCP))
    then ATemp := ZUnicodeToRaw(OrgStr, StringCP)
    else ATemp := ZUnicodeToRaw(OrgStr, ColumnCP);
    ATemp2 := TStringField(Actual).{$IFDEF WITH_ASANSISTRING}AsAnsiString{$ELSE}AsString{$ENDIF};
    CheckEquals(ATemp, Atemp2, Protocol+': The raw value of Field: '+Actual.FieldName);
    Check(ControlsCodePage in [cGET_ACP, {$IFNDEF UNICODE}cCP_UTF8,{$ENDIF}cDynamic], Protocol+' The FieldType-A type');
    Check((SQLType in [stAsciiStream, stUnicodeStream]) or ((SQLType in [stString, stUnicodeString]) and (Size <= 0)),
      Protocol+': The underlaying SQLType of the Field: '+Actual.FieldName);
  end else
    Check(False, Protocol+': wrong overload called');
end;

procedure TZAbstractCompSQLTestCase.CheckNotEquals(Expected, Actual: TFieldType;
  const Msg: string);
var E, A: String;
begin
  E := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(Expected));
  A := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(Expected));
  inherited CheckNotEquals(E, A, Msg);
end;

procedure TZAbstractCompSQLTestCase.CheckStringFieldType(Actual: TField;
  ControlsCodePage: TZControlsCodePage);
var IDX: Integer;
begin
  case ControlsCodePage of
    cGET_ACP{$IFNDEF UNICODE},cCP_UTF8{$ENDIF}: CheckEquals(ftString, Actual.DataType, 'String Field/Parameter-Type');
    cCP_UTF16: CheckEquals(ftWideString, Actual.DataType, 'String Field/Parameter-Type');
    cDynamic: begin
                IDX := THackDataSet(Actual.DataSet).ResultSetMetadata.FindColumn(Actual.FieldName);
                if THackDataSet(Actual.DataSet).ResultSetMetadata.GetColumnType(IDX) = stUnicodeString
                then CheckEquals(ftWideString, Actual.DataType, 'String Field/Parameter-Type')
                else CheckEquals(ftString, Actual.DataType, 'String Field/Parameter-Type')
              end;

  end;
end;

procedure TZAbstractCompSQLTestCase.CheckMemoParamType(Actual: TParam;
  ControlsCodePage: TZControlsCodePage);
begin
  case ControlsCodePage of
    cGET_ACP{$IFNDEF UNICODE},cCP_UTF8{$ENDIF}: CheckEquals(ftMemo, Actual.DataType, 'Memo Parameter-Type');
    cCP_UTF16:  CheckEquals({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}, Actual.DataType, 'Memo Parameter-Type');
    cDynamic:   if Actual.DataType = {$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}
                then CheckEquals({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}, Actual.DataType, 'Memo Parameter-Type')
                else CheckEquals(ftMemo, Actual.DataType, 'Memo Parameter-Type')
  end;
end;

procedure TZAbstractCompSQLTestCase.CheckStringParamType(Actual: TParam;
  ControlsCodePage: TZControlsCodePage);
begin
  case ControlsCodePage of
    cGET_ACP{$IFNDEF UNICODE},cCP_UTF8{$ENDIF}: CheckEquals(ftString, Actual.DataType, 'String Parameter-Type');
    cCP_UTF16:  CheckEquals(ftWideString, Actual.DataType, 'String Parameter-Type');
    cDynamic:   if Actual.DataType = ftWideString
                then CheckEquals(ftWideString, Actual.DataType, 'String Parameter-Type')
                else CheckEquals(ftString, Actual.DataType, 'String Parameter-Type')
  end;
end;

procedure TZAbstractCompSQLTestCase.CheckEquals(Expected, Actual: TFieldType;
  const Msg: string);
var E, A: String;
begin
  E := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(Expected));
  A := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(Expected));
  inherited CheckEquals(E, A, Msg);
end;

procedure TZAbstractCompSQLTestCase.CheckMemoFieldType(Actual: TField;
  ControlsCodePage: TZControlsCodePage);
var IDX: Integer;
begin
  case ControlsCodePage of
    cGET_ACP{$IFNDEF UNICODE}, cCP_UTF8{$ENDIF}: CheckEquals(ftMemo, Actual.DataType, 'Memo-Field/Parmeter-Type');
    cCP_UTF16: CheckEquals({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}, Actual.DataType, 'Memo-FieldParameter-Type');
    cDynamic: begin
                IDX := THackDataSet(Actual.DataSet).ResultSetMetadata.FindColumn(Actual.FieldName);
                if THackDataSet(Actual.DataSet).ResultSetMetadata.GetColumnType(IDX) = stUnicodeStream
                then CheckEquals({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}, Actual.DataType, 'Memo Field/Parameter-Type')
                else CheckEquals(ftMemo, Actual.DataType, 'Memo Field/Parameter-Type')
              end;
  end;
end;

function TZAbstractCompSQLTestCase.CreateQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values[DSProps_PreferPrepared]) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

function TZAbstractCompSQLTestCase.CreateReadOnlyQuery: TZReadOnlyQuery;
begin
  Result := TZReadOnlyQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values[DSProps_PreferPrepared]) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

function TZAbstractCompSQLTestCase.CreateTable: TZTable;
begin
  Result := TZTable.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values[DSProps_PreferPrepared]) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

function TZAbstractCompSQLTestCase.GetDBTestString(const Value: UnicodeString;
  Target: TZTransliterationType; MaxLen: Integer = -1): SQLString;
var CP: Word;
  {$IFDEF UNICODE}
  RawTemp: RawByteString;
  ConSettings: PZConSettings;
  {$ENDIF}
begin
  {$IFDEF UNICODE}
  ConSettings := Connection.DbcConnection.GetConSettings;
  if (ConSettings.ClientCodePage.Encoding in [ceUTF8, ceUTF16])
  then Result := Value
  else begin
    CP := ConSettings.ClientCodePage.CP;
    RawTemp := ZUnicodeToRaw(Value, CP);
    Result := ZRawToUnicode(RawTemp, CP);
  end;
  {$ELSE}
  CP := FConnection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(Target);
  Result := ZUnicodeToRaw(Value, CP);
  {$ENDIF}
  if (MaxLen > 0) and (Length(Result) > MaxLen) then
    SetLength(Result, MaxLen);
end;

{ TZAbstractDbcSQLTestCaseMBCs }

function TZAbstractDbcSQLTestCaseMBCs.IsNonASCIITest: Boolean;
begin
  Result := True;
end;

{ TZAbstractCompSQLTestCaseMBCs }
function TZAbstractCompSQLTestCaseMBCs.IsNonASCIITest: Boolean;
begin
  Result := True;
end;

{**
  Rebuilds databases for the specified test group.
  @param TestGroup a name of the test group. If the name is not set
    test group is takes from TestGroup global variable.
}
procedure RebuildTestDatabases(TestGroup: string = '');
var
  Temp: TZSupplementarySQLTestCase;
begin
  if TestGroup = '' then
    TestGroup := ZTestConfig.TestGroup;

  Temp := TZSupplementarySQLTestCase.CreateWithGroup(TestGroup);
  try
    Temp.RebuildDatabases;
  finally
    Temp.Free;
  end;
end;

{$IFDEF WITH_CLASS_VARS}
initialization
  TZAbstractSQLTestCase.LoadConfigurations;

finalization
  FreeAndNil(TZAbstractSQLTestCase.CVConnectionConfigs);
{$ENDIF}

end.
