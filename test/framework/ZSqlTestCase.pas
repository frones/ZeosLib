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
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  {$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF}, Classes, SysUtils, DB,
  {$IFDEF ENABLE_POOLED}ZClasses,{$ENDIF} ZDataSet,
  ZCompatibility, ZDbcIntfs, ZConnection, Contnrs, ZTestCase, ZScriptParser, ZDbcLogging;

const
  { protocol lists }
  pl_mysql_client_server = 'mysql,mysql-4.1,mysql-5';
  pl_mysql_embedded = 'mysqld-4.1,mysqld-5';
  pl_all_mysql = pl_mysql_client_server + ','+ pl_mysql_embedded;
  pl_all_postgresql = 'postgresql,postgresql-7,postgresql-8,postgresql-9';
  pl_all_sqlite = 'sqlite,sqlite-3';
  pl_interbase_client_server = 'interbase-6,firebird-1.0,firebird-1.5,firebird-2.0,firebird-2.1,firebird-2.5';
  pl_interbase_embedded = 'firebirdd-1.5,firebirdd-2.0,firebirdd-2.1,firebirdd-2.5';
  pl_all_interbase = pl_interbase_client_server + ',' + pl_interbase_embedded;

  // Protocols needing prefererealprepared option for real prepared statements
  pl_realpreparable = pl_all_mysql+','+pl_all_postgresql+','+pl_all_sqlite;

type
  TZConfigUse = (cuMainConnection, cuNonAscii, cuRealPrepared, cuAutoEncoded);
  TZConfigUses = set of TZConfigUse;

  {** Represents a SQL test database configuration. }
  { TZConnectionConfig }
  TZConnectionConfig = class
  private
    FConfigUses: TZConfigUses;
    FName: string;
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
    FExtended_AutoEncoding: Boolean;
    FSkip_RealPrepared: Boolean;
    procedure SetConfigUses(AValue: TZConfigUses);
  public
    constructor Create; overload;
    constructor Create(TemplateConfig: TZConnectionConfig; Suffix: String); overload;
    constructor Create(ConnectionName: String); overload;
    destructor Destroy; override;
    procedure CreateExtendedConfigurations(ConnectionsList: TObjectList);
    property Name: string read FName write FName;
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
    property Include_AutoEncoding: Boolean read FExtended_AutoEncoding;
    property Skip_RealPrepared: Boolean read FSkip_RealPrepared;
    property ConfigUses:TZConfigUses read FConfigUses write SetConfigUses;
  end;

  {** Implements an abstract class for all SQL test cases. }

  { TZAbstractSQLTestCase }

  TZAbstractSQLTestCase = class(TZAbstractTestCase, IZLoggingListener)
  private
    FTraceList: TStrings;

    FConnectionName: string;
    FAlias: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FRebuild: Boolean;
    FCreateScripts: TStringDynArray;
    FDropScripts: TStringDynArray;
    FProperties: TStringDynArray;
    FSkipNonZeosIssues: Boolean;
    FSkipSetup: Boolean;
    function GetProtocol : string;
    function GetSkipNonZeosIssues: Boolean;
  protected
    {$IFDEF WITH_CLASS_VARS}class var {$ENDIF} CVConnectionConfigs : TObjectList;
    {$IFDEF WITH_CLASS_VARS}class{$ENDIF} procedure LoadConfigurations;
    {$IFDEF WITH_CLASS_VARS}class{$ENDIF} property ConnectionConfigs: TObjectList read CVConnectionConfigs write CVConnectionConfigs;
    {$IFNDEF WITH_CLASS_VARS}procedure LoadConfiguration; override;{$ENDIF}
    property TraceList: TStrings read FTraceList write FTraceList;

    procedure SetActiveConnection(Connection: TZConnectionConfig);
    {$IFNDEF FPC}
    procedure RunWithFixture(TestResult: TTestResult); override;
    {$ELSE}
    procedure Run(TestResult: TTestResult); override;
    {$ENDIF}

    function IsProtocolValid(Config: TZConnectionConfig): Boolean; virtual;
    function IsConfigUseValid(Config: TZConnectionConfig): Boolean; virtual;
    function IsNonASCIITest: Boolean; virtual;
    function IsRealPreparableTest: Boolean; virtual;
    function IsAutoEncodableTest: Boolean; virtual;
    function GetSupportedProtocols: string; virtual;
    function SkipClosed: Boolean;

    procedure StartSQLTrace;
    procedure StopSQLTrace;

    procedure CheckStringFieldType(Actual: TFieldType; ConSettings: PZConSettings);
    procedure CheckMemoFieldType(Actual: TFieldType; ConSettings: PZConSettings);

    function GetDBTestString(const Value: String; ConSettings: PZConSettings; IsUTF8Encoded: Boolean = False; MaxLen: Integer = -1): String;
    function GetDBValidString(const Value: String; ConSettings: PZConSettings; IsUTF8Encoded: Boolean = False): String;
    function GetDBTestStream(const Value: String; ConSettings: PZConSettings; IsUTF8Encoded: Boolean = False): TStream;
  public
    destructor Destroy; override;

    procedure Fail(Msg: string; ErrorAddr: Pointer = nil);{$IFNDEF FPC}  override; {$ENDIF}
    procedure LogEvent(Event: TZLoggingEvent);

    { Different convenience methods. }
    function CreateDbcConnection: IZConnection;
    function CreateDatasetConnection: TZConnection;
    procedure PrintResultSet(ResultSet: IZResultSet;
      ShowTypes: Boolean; Note: string = '');

    { Properties to access active connection settings. }
    property ConnectionName: string read FConnectionName;
    property Alias: string read FAlias;
    property Protocol: string read GetProtocol;
    property HostName: string read FHostName;
    property Port: Integer read FPort;
    property Database: string read FDatabase;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property Rebuild: Boolean read FRebuild;
    property CreateScripts: TStringDynArray read FCreateScripts;
    property DropScripts: TStringDynArray read FDropScripts;
    property Properties: TStringDynArray read FProperties;
    property SkipNonZeosIssues: Boolean read GetSkipNonZeosIssues;
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
    function IsRealPreparableTest: Boolean; override;
    function GetConnectionUrl(Param: String): string;

    property Connection: IZConnection read FConnection write FConnection;
  end;

  {** Implements a bug test case which runs all active protocols. }

  { TZAbstractCompSQLTestCase }

  TZAbstractCompSQLTestCase = class (TZAbstractSQLTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function IsRealPreparableTest: Boolean; override;
    function IsAutoEncodableTest: Boolean; override;
    function CreateQuery: TZQuery;
    function CreateReadOnlyQuery: TZReadOnlyQuery;
    function CreateTable: TZTable;

    property Connection: TZConnection read FConnection write FConnection;
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

implementation

uses ZSysUtils, ZTestConsts, ZTestConfig, ZSqlProcessor, ZURL, ZAbstractRODataset;

function PropPos(const Current: TZConnectionConfig; const AProp: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for i := 0 to high(Current.Properties) do
    if StartsWith(UpperCase(Current.Properties[i]), UpperCase(AProp)+'=') then
    begin
      Result := i;
      Break;
    end;
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

{ TZConnectionConfig }

procedure TZConnectionConfig.SetConfigUses(AValue: TZConfigUses);
begin
  if FConfigUses=AValue then Exit;
  FConfigUses:=AValue;
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
  FExtended_AutoEncoding := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    EXTENDED_AUTOENCODING_KEY, FALSE_VALUE));
  FSkip_RealPrepared := StrToBoolEx(TestConfig.ReadProperty(COMMON_GROUP,
    SKIP_REAL_PREPARED_KEY, FALSE_VALUE));
end;

constructor TZConnectionConfig.Create(TemplateConfig: TZConnectionConfig; Suffix: String);
begin
  Create;
  FName := TemplateConfig.Name+'_'+Suffix;
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
    DATABASE_PROPERTIES_KEY, ''), LIST_DELIMITERS);
  FConfigUses := [cuMainConnection];
  FSkip_RealPrepared := StrToBoolEx(TestConfig.ReadProperty(FName,
    SKIP_REAL_PREPARED_KEY, FALSE_VALUE));
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
var
  TempConfig:TZConnectionConfig;

  Function ProtocolIsRealPreparable(protocol : string) : boolean;
  var
    Temp: TStrings;
    TempName : string;
  begin
    Temp := SplitString(pl_realpreparable, LIST_DELIMITERS);
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
        SetProperty(MyCurrent, 'codepage',Current.CharacterSets[iCharacterSets]);
        ConnectionsList.Add(MyCurrent);
      end;
  end;

  procedure SetAutoEncodings(const Current: TZConnectionConfig);
  var MyCurrent: TZConnectionConfig;
  begin
    if Include_AutoEncoding then
    begin
      MyCurrent := TZConnectionConfig.Create(Current, 'AutoEncodeStrings');
      MyCurrent.ConfigUses:=MyCurrent.ConfigUses+[cuAutoEncoded];
      //Writeln(MyCurrent.Name);
      SetProperty(MyCurrent, 'AutoEncodeStrings','ON');
      ConnectionsList.Add(MyCurrent);
      SetCharacterSets(MyCurrent);
      {autoencodings off is default so nothing must be added...}
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
        SetProperty(MyCurrent, 'controls_cp',CPType);
        ConnectionsList.Add(MyCurrent);
      end;
      if (CPType = 'CP_UTF16') then //autoencoding is allways true
        SetCharacterSets(MyCurrent)
      else
        {$IF defined(MSWINDOWS) or defined(WITH_FPC_STRING_CONVERSATION) or defined(WITH_LCONVENCODING) or defined(DELPHI)}
        SetAutoEncodings(MyCurrent); //Allow Autoencoding only if supported!
        {$ELSE}
        SetCharacterSets(MyCurrent); //No Autoencoding available
        {$IFEND}
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
    {$IFDEF WITH_WIDEFIELDS}
    if FExtended_cGet_UTF16 then
      {$IFDEF UNICODE}
      CloneConfig(''); //CP_UTF16 is default for D12_UP -> no clone!
      {$ELSE}
      CloneConfig('CP_UTF16');
      {$ENDIF}
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
    if PropPos(ConnectionConfig, 'codepage') > -1 then //add a empty dummy value to get the autodetecting running for PG for example
      SetLength(TempCharacterSets, Length(TempCharacterSets)+1);
    ConnectionConfig.CharacterSets := TempCharacterSets;

    SetCtrlsCPTypes(Self);
  end;
begin
  if ExtendedTest then
  begin
    create_charsets_encodings(self);
  end;

  if Not Skip_RealPrepared then
  begin
    if ProtocolIsRealPreparable(self.Protocol) then
    begin
      //writeln('create preferprepared');
      TempConfig := TZConnectionConfig.Create(Self, 'preferprepared');
      SetProperty(TempConfig, 'preferprepared', 'True');
      SetProperty(TempConfig, 'preferpreparedresolver', 'True');
      ConnectionsList.Add(TempConfig);
      TempConfig.ConfigUses:=[cuRealPrepared];
      if ExtendedTest then
      begin
        create_charsets_encodings(TempConfig);
      end;
    end;
  end;

end;

{ TZAbstractSQLTestCase }

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractSQLTestCase.Destroy;
begin
  {$IFNDEF WITH_CLASS_VARS}
  if Assigned(CVConnectionConfigs) then
    CVConnectionConfigs.Free;
  {$ENDIF}
  if Assigned(FTraceList) then
    FTraceList.Free;

  inherited Destroy;
end;

function TZAbstractSQLTestCase.GetProtocol: string;
begin
  {$IFDEF ENABLE_POOLED}
  If StartsWith(FProtocol,pooledprefix) then
    Result := Copy(FProtocol,Length(PooledPrefix)+1,Length(FProtocol))
  else
  {$ENDIF}
    Result := FProtocol;
end;

function TZAbstractSQLTestCase.GetSkipNonZeosIssues: boolean;
begin
  Check(True);
  Result := FSkipNonZeosIssues
end;

{**
  Overrides a fail method which prints an error message.
  @param Msg an error message string.
  @param ErrorAddr an address where error happened.
}
procedure TZAbstractSQLTestCase.Fail(Msg: string; ErrorAddr: Pointer = nil);
begin
  inherited Fail(Format('%s/%s: %s', [ConnectionName, Protocol, Msg]),
    ErrorAddr);
end;

{**
  Handles a new incoming logging event.
  @param Event an incoming logging event.
}
procedure TZAbstractSQLTestCase.LogEvent(Event: TZLoggingEvent);
begin
  if Event.Message <> '' then
    FTraceList.Append(Event.Message);
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZAbstractSQLTestCase.IsProtocolValid(Config: TZConnectionConfig): Boolean;
var
  Temp: TStrings;
  TempName : string;
begin
  if GetSupportedProtocols = '' then
    Result := True
  else
    begin
      Temp := SplitString(GetSupportedProtocols, LIST_DELIMITERS);
      TempName := Config.Protocol;
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
  If (cuRealPrepared in Config.ConfigUses) then
  begin
    Selection := True;
    Objection := Objection or Not(isRealPreparableTest) //PreferRealPrepared connections are only usefull for specific tests
  end;
  If (cuAutoEncoded in Config.ConfigUses) then
  begin
    Selection := True;
    Objection := Objection or Not(isAutoEncodableTest) //AutoEncoded connections are only usefull for specific tests
  end;
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

function TZAbstractSQLTestCase.IsRealPreparableTest: Boolean;
begin
  Result := False;
end;

function TZAbstractSQLTestCase.IsAutoEncodableTest: Boolean;
begin
  result := False;
end;

function TZAbstractSQLTestCase.GetSupportedProtocols: string;
begin
  result := '';
end;

function TZAbstractSQLTestCase.SkipClosed: Boolean;
begin
  Check(True);
  Result := StrToBoolEx(ReadInheritProperty(SKIP_CLOSED_KEY, FALSE_VALUE));
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

    {$IFDEF FPC}
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
  Sets an active database connection for the test.
  @param Connection a database connection to be set.
}
procedure TZAbstractSQLTestCase.SetActiveConnection(
  Connection: TZConnectionConfig);
begin
  FConnectionName := Connection.Name;
  FAlias := Connection.Alias;
  FProtocol := Connection.Protocol;
  FHostName := Connection.HostName;
  FPort := Connection.Port;
  FDatabase := Connection.Database;
  FUserName := Connection.UserName;
  FPassword := Connection.Password;
  FRebuild := Connection.Rebuild;
  FCreateScripts := Connection.CreateScripts;
  FDropScripts := Connection.DropScripts;
  FProperties := Connection.Properties;
end;

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

procedure TZAbstractSQLTestCase.CheckStringFieldType(Actual: TFieldType;
  ConSettings: PZConSettings);
begin
  case ConSettings.CPType of
    cGET_ACP, cCP_UTF8{$IFNDEF WITH_WIDEFIELDS},cCP_UTF16{$ENDIF}: CheckEquals(Ord(ftString), Ord(Actual), 'String Field/Parameter-Type');
    {$IFDEF WITH_WIDEFIELDS}cCP_UTF16: CheckEquals(Ord(ftWideString), Ord(Actual), 'String Field/Parameter-Type');{$ENDIF}
  end;
end;

procedure TZAbstractSQLTestCase.CheckMemoFieldType(Actual: TFieldType;
  ConSettings: PZConSettings);
begin
  case ConSettings.CPType of
    cGET_ACP, cCP_UTF8{$IFNDEF WITH_WIDEFIELDS},cCP_UTF16{$ENDIF}: CheckEquals(Ord(ftMemo), Ord(Actual), 'Memo-Field/Parmeter-Type');
    {$IFDEF WITH_WIDEFIELDS}cCP_UTF16: CheckEquals(Ord(ftWideMemo), Ord(Actual), 'Memo-FieldParameter-Type');{$ENDIF}
  end;
end;

{**
  Get a valid String to Test the encoding. If AutoEncodeStrings then the
  Encoding is reverted to get proper test-behavior.
  @param Value a string which should be prepared for the Test.
  @return the right or reverted encoded string to check the behavior.
}
{$IFDEF UNICODE}
  {$WARNINGS OFF}
{$ENDIF}
function TZAbstractSQLTestCase.GetDBTestString(const Value: String;
  ConSettings: PZConSettings; IsUTF8Encoded: Boolean = False;
  MaxLen: Integer = -1): String;
var Temp: {$IFNDEF UNICODE}ZAnsiString{$ELSE}String{$ENDIF};
begin
  Result := Value;
  if ConSettings.CPType = cCP_UTF16 then
    if isUTF8Encoded then
      Temp := UTF8ToString(ZAnsiString(Value))
    else
      Temp := Value
  else
    case ConSettings.ClientCodePage.Encoding of
      ceDefault: Result := Value; //Souldn't be possible
      ceAnsi:
        if ConSettings.AutoEncode then //Revert the expected value to test
          if IsUTF8Encoded then
            Temp := Value
          else
            Temp := UTF8Encode(WideString(Value))
        else  //Return the expected value to test
          if IsUTF8Encoded then
            Temp := UTF8ToAnsi(Value)
          else
            Temp := Value;
      else //ceUTF8, ceUTF16, ceUTF32
        if ConSettings.AutoEncode then //Revert the expected value to test
          if IsUTF8Encoded then
            {$IFDEF UNICODE}
            Temp := UTF8ToString(Value)
            {$ELSE}
            Temp := UTF8ToAnsi(Value)
            {$ENDIF}
          else
            Temp := Value
        else
          if IsUTF8Encoded then
            Temp := Value
          else
            Temp := UTF8Encode(WideString(Value)); //Return the expected value to test
    end;
  if (MaxLen = -1) then
  begin
    SetLength(Result, Length(Temp));
    System.Move(PChar(Temp)^, PChar(Result)^, Length(Temp)*SizeOf(Char));
  end
  else
  begin
    SetLength(Result, MaxLen);
    System.Move(PChar(Temp)^, PChar(Result)^, MaxLen*SizeOf(Char));
  end;
end;

function TZAbstractSQLTestCase.GetDBValidString(const Value: String;
  ConSettings: PZConSettings; IsUTF8Encoded: Boolean = False): String;
var Temp: {$IFNDEF UNICODE}ZAnsiString{$ELSe}String{$ENDIF};
begin
  Result := Value;
  if ConSettings.CPType = cCP_UTF16 then
    if isUTF8Encoded then
      Temp := UTF8ToString(ZAnsiString(Value))
    else
      Temp := Value
  else
    case ConSettings.ClientCodePage.Encoding of
      ceDefault: Result := Value; //Souldn't be possible
      ceAnsi:
          if IsUTF8Encoded then
            Temp := UTF8ToAnsi(Value)
          else
            Temp := Value;
      else //ceUTF8, ceUTF16, ceUTF32
        if IsUTF8Encoded then
          Temp := Value
        else
          Temp := UTF8Encode(WideString(Value)); //Return the expected value to test
    end;
  SetLength(Result, Length(Temp));
  System.Move(PChar(Temp)^, PChar(Result)^, Length(Temp)*SizeOf(Char));
end;

{$IFDEF UNICODE}
  {$WARNINGS ON}
{$ENDIF}

function TZAbstractSQLTestCase.GetDBTestStream(const Value: String; ConSettings:
  PZConSettings; IsUTF8Encoded: Boolean = False): TStream;
var
  WS: ZWideString;
  Ansi: ZAnsiString;
begin
  Result := TMemoryStream.Create;
  if ( ConSettings.CPType = cCP_UTF16 ) then
  begin
    if isUTF8Encoded then
      WS := UTF8ToString(ZAnsiString(Value))
    else
      WS := ZWideString(Value);
    Result.Write(PWideChar(WS)^, Length(WS)*2);
    Result.Position := 0;
  end
  else
  begin
    case ConSettings.ClientCodePage.Encoding of
      ceAnsi:
        if ConSettings.AutoEncode then //Revert the expected value to test
          if IsUTF8Encoded then
            Ansi := ZAnsiString(Value)
          else
            Ansi := UTF8Encode(WideString(Value))
        else  //Return the expected value to test
          if IsUTF8Encoded then
            Ansi := ZAnsiString(UTF8ToAnsi(ZAnsiString(Value)))
          else
            Ansi := ZAnsiString(Value);
      else //ceUTF8, ceUTF16, ceUTF32
        if ConSettings.AutoEncode then //Revert the expected value to test
          if IsUTF8Encoded then
            Ansi := ZAnsiString(UTF8ToAnsi(ZAnsiString(Value)))
          else
            Ansi := ZAnsiString(Value)
        else
          if IsUTF8Encoded then
            Ansi := ZAnsiString(Value)
          else
            Ansi := UTF8Encode(WideString(Value)); //Return the expected value to test
    end;
    Result.Write(PAnsiChar(Ansi)^, Length(Ansi));
    Result.Position := 0;
  end;
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
  Current: TZConnectionConfig;
begin
  for I := 0 to ConnectionConfigs.Count - 1 do
    if IsProtocolValid(TZConnectionConfig(ConnectionConfigs[I])) and
       IsConfigUseValid(TZConnectionConfig(ConnectionConfigs[I])) then
    begin
      Current := TZConnectionConfig(ConnectionConfigs[I]);
    //writeln('Using : '+Current.Name);
      SetActiveConnection(Current);
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
  Current: TZConnectionConfig;
begin
  for I := 0 to ConnectionConfigs.Count - 1 do
    if IsProtocolValid(TZConnectionConfig(ConnectionConfigs[I])) and
       IsConfigUseValid(TZConnectionConfig(ConnectionConfigs[I])) then
    begin
      Current := TZConnectionConfig(ConnectionConfigs[I]);
      //writeln('Using : '+Current.Name);
      SetActiveConnection(Current);
      inherited Run(TestResult);
    end;
end;
{$ENDIF}

{**
  Creates a database ZDBC connection object.
  @return a created database ZDBC connection object.
}
function TZAbstractSQLTestCase.CreateDbcConnection: IZConnection;
var
  URL: string;
  TempProperties :TStrings;
  I: Integer;
begin
  if Port <> 0 then
    URL := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [Protocol, HostName, Port, Database, UserName, Password])
  else URL := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [Protocol, HostName, Database, UserName, Password]);
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  Result := DriverManager.GetConnectionWithParams(URL, TempProperties);
  TempProperties.Free;
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
  Result.Protocol := Protocol;
  Result.Port := Port;
  Result.HostName := HostName;
  Result.Database := Database;
  Result.User := UserName;
  Result.Password := Password;
  Result.LoginPrompt := False;
  for I := 0 to High(Properties) do
  begin
    Result.Properties.Add(Properties[I])
  end;
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
                  ReadNum := Stream.Read(Buffer, 100);
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
begin
  { Sets the right error event handler. }
  if RaiseException then
    FSQLProcessor.OnError := RaiseError
  else FSQLProcessor.OnError := SuppressError;

  ScriptPath := TestConfig.ConfigFilePath;

  for I := 0 to High(FileNames) do
  begin
    FSQLProcessor.Script.Clear;
    try
      FSQLProcessor.Script.LoadFromFile(ScriptPath + FileNames[I]);
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
  Current: TZConnectionConfig;
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
      Current := TZConnectionConfig(ConnectionConfigs[I]);
      //Writeln('Rebuilding '+Current.Name);
      SetActiveConnection(Current);
      Connection := CreateDatasetConnection;
      try
        FSQLProcessor.Connection := Connection;
        FSQLProcessor.Delimiter := Current.Delimiter;
        FSQLProcessor.DelimiterType := Current.DelimiterType;

        if Current.Rebuild then
        begin
          ExecuteScripts(Current.DropScripts, False);
          ExecuteScripts(Current.CreateScripts, True);
        end;
      finally
        Connection.Free;
      end;
  end;
end;

{ TZAbstractDbcSQLTestCase}

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

function TZAbstractDbcSQLTestCase.IsRealPreparableTest: Boolean;
begin
  Result:= True;
end;

function TZAbstractDbcSQLTestCase.GetConnectionUrl(Param: String): string;
var
  TempProperties: TStrings;
  I: Integer;
begin
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  TempProperties.Add(Param);
  Result := DriverManager.ConstructURL(Protocol, HostName, Database,
  UserName, Password, Port, TempProperties);
  TempProperties.Free;
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
  end;
end;

function TZAbstractCompSQLTestCase.IsRealPreparableTest: Boolean;
begin
  Result:= True;
end;

function TZAbstractCompSQLTestCase.IsAutoEncodableTest: Boolean;
begin
  Result:=True;
end;

function TZAbstractCompSQLTestCase.CreateQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
  if StrToBoolEx(FConnection.Properties.Values['preferpreparedresolver']) then
    Result.Options := Result.Options + [doPreferPreparedResolver];
end;

function TZAbstractCompSQLTestCase.CreateReadOnlyQuery: TZReadOnlyQuery;
begin
  Result := TZReadOnlyQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
  if StrToBoolEx(FConnection.Properties.Values['preferpreparedresolver']) then
    Result.Options := Result.Options + [doPreferPreparedResolver];
end;

function TZAbstractCompSQLTestCase.CreateTable: TZTable;
begin
  Result := TZTable.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
  if StrToBoolEx(FConnection.Properties.Values['preferpreparedresolver']) then
    Result.Options := Result.Options + [doPreferPreparedResolver];
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
  if Assigned(TZAbstractSQLTestCase.CVConnectionConfigs) then
    TZAbstractSQLTestCase.CVConnectionConfigs.Free;
{$ENDIF}
end.
