unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, StdCtrls, ZConnection;

const
  { Names of the main test groups }
  COMMON_GROUP           = 'common';
  CORE_TEST_GROUP        = 'core';
  PARSESQL_TEST_GROUP    = 'parsesql';
  PLAIN_TEST_GROUP       = 'plain';
  DBC_TEST_GROUP         = 'dbc';
  COMPONENT_TEST_GROUP   = 'component';
  BUGREPORT_TEST_GROUP   = 'bugreport';
  PERFORMANCE_TEST_GROUP = 'performance';
const
  { Names of the main configuration keys. }
  ENABLE_MEMCHECK_KEY      = 'enable.memcheck';
  MEMCHECK_LOGFILE_KEY     = 'memcheck.logfile';
  MEMCHECK_SHOWRESULT_KEY  = 'memcheck.showresult';
  DECIMAL_SEPARATOR_KEY    = 'decimal.separator';
  SUPPRESS_TEST_OUTPUT_KEY = 'suppress.output';
  ENABLE_KEY               = 'enable';
  SKIP_CLOSED_KEY          = 'skip.closed';
  ACTIVE_CONNECTIONS_KEY   = 'connections';

const
  { Names of the connection configuration keys. }
  DATABASE_ALIAS_KEY          = 'alias';
  DATABASE_PROTOCOL_KEY       = 'protocol';
  DATABASE_HOST_KEY           = 'host';
  DATABASE_PORT_KEY           = 'port';
  DATABASE_NAME_KEY           = 'database';
  DATABASE_USER_KEY           = 'user';
  DATABASE_PASSWORD_KEY       = 'password';
  DATABASE_REBUILD_KEY        = 'rebuild';
  DATABASE_DELIMITER_TYPE_KEY = 'delimiter.type';
  DATABASE_DELIMITER_KEY      = 'delimiter';
  DATABASE_CREATE_SCRIPTS_KEY = 'create.scripts';
  DATABASE_DROP_SCRIPTS_KEY   = 'drop.scripts';
  DATABASE_PROPERTIES_KEY     = 'properties';
  DATABASE_LIBRARY_KEY        = 'LibLocation';
  DATABASE_PREPREPARESQL_KEY  = 'PreprepareSQL';
  DATABASE_CREATE_KEY         = 'createNewDatabase';
  DATABASE_CODEPAGE_KEY       = 'codepage';

const
  { SQL script delimiters }
  DEFAULT_DELIMITER    = 'default';
  SET_TERM_DELIMITER   = 'setterm';
  GO_DELIMITER         = 'go';
  EMPTY_LINE_DELIMITER = 'emptyline';

const
  { Well Known Values. }
  NONE_VALUE      = 'none';
  FALSE_VALUE     = 'false';
  TRUE_VALUE      = 'true';
  LIST_DELIMITERS = ' ,;';

const
  { Default Values. }
  DEFAULT_DECIMAL_SEPARATOR = ',';
  DEFAULT_PORT_VALUE        = 0;
  DEFAULT_HOST_VALUE        = 'localhost';

const
  DefaultFile = {$IFDEF UNIX}'../database/test.properties'{$ELSE}'..\database\test.properties'{$ENDIF};
type
  TPlainConfig = Class(TObject)
  private
    FHasChanged: Boolean;
    FCommon: Boolean;
    FProtocol: string;
    FHostName: string;
    FPort: string;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FLibLocation: String;
    FAnsiCodePage: String;
    FUnicodeCodePage: String;
    FUnicode: Boolean;
    FPreprepareSQL: Boolean;
    FCore: Boolean;
    FDbc: Boolean;
    FParseSQL: Boolean;
    FPlain: Boolean;
    FComponent: Boolean;
    FBugreport: Boolean;
    FPerformance: Boolean;
    FAlias: String;
    FRebuild: Boolean;
    FBuildScripts: String;
    FDropScripts: String;
    FDelimiter: String;
    FDelimiterType: String;
    FCreateDatabase: Boolean;
    FProperties: String;
    procedure SetCommon(const Value: Boolean);
    procedure SetAnsiCodePage(const Value: string);
    procedure SetUnicodeCodePage(const Value: string);
    procedure SetUnicode(const Value: boolean);
    procedure SetPreprepareSQL(const Value: Boolean);
    procedure SetCore(const Value: boolean);
    procedure SetParseSQL(const Value: boolean);
    procedure SetPlain(const Value: boolean);
    procedure SetDbc(const Value: boolean);
    procedure SetComponent(const Value: boolean);
    procedure SetBugreport(const Value: boolean);
    procedure SetPerformance(const Value: boolean);
    procedure SetAlias(const Value: String);
    procedure SetRebuild(const Value: Boolean);
    procedure SetBuildScripts(const Value: string);
    procedure SetDropScripts(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetConnPort(const Value: string);
    procedure SetDatabase(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetLibLocation(const Value: String);
    procedure SetDelimiter(const Value: String);
    procedure SetDelimiterType(const Value: String);
    procedure SetProperties(const Value: String);
    procedure SetCreateDatabase(const Value: Boolean);
  public
    constructor Create(const AProtocol: String);
    procedure PostUpdates;
    property Common: Boolean read FCommon write SetCommon;
    property AnsiCodePage: String read FAnsiCodePage write SetAnsiCodePage;
    property UnicodeCodePage: String read FUnicodeCodePage write SetUnicodeCodePage;
    property Unicode: Boolean read FUnicode write SetUnicode;
    property PreprepareSQL: Boolean read FPreprepareSQL write SetPreprepareSQL;
    property Core: Boolean read FCore write SetCore;
    property ParseSQL: Boolean read FParseSQL write SetParseSQL;
    property Plain: Boolean read FPlain write SetPlain;
    property Dbc: Boolean read FDbc write SetDbc;
    property Component: Boolean read FComponent write SetComponent;
    property Bugreport: Boolean read FBugreport write SetBugreport;
    property Performance: Boolean read FPerformance write SetPerformance;
    property Alias: String read FAlias write SetAlias;
    property Rebuild: Boolean read FRebuild write SetRebuild;
    property BuildScripts: String read FBuildScripts write SetBuildScripts;
    property DropScripts: String read FDropScripts write SetDropScripts;
    property Protocol: string read FProtocol write SetProtocol;
    property HostName: string read FHostName write SetHostName;
    property Port: String read FPort write SetConnPort;
    property Database: string read FDatabase write SetDatabase;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
    property LibLocation: string read FLibLocation write SetLibLocation;
    property Delimiter: string read FDelimiter write SetDelimiter;
    property DelimiterType: String read FDelimiterType write SetDelimiterType;
    property CreateDatabase: Boolean read FCreateDatabase write SetCreateDatabase;
    property Properties: String read FProperties write SetProperties;
    property Changed: Boolean read FHasChanged;
  end;

  TPerformanceConfig = class(TObject)
  private
    FAPIPlain: Boolean;
    FAPIdbc: Boolean;
    FAPIdbcchached: Boolean;
    FAPIdataset: Boolean;
    FAPImidas: Boolean;
    FAPIoldzeos: Boolean;
    FAPIbde: Boolean;
    FAPIado: Boolean;
    FAPIdbx: Boolean;
    FAPIdbxc: Boolean;
    FAPIibx: Boolean;
    FTestConncet: Boolean;
    FTestInsert: Boolean;
    FTestOpen: Boolean;
    FTestFetch: Boolean;
    FTestSort: Boolean;
    FTestFilter: Boolean;
    FTestUpdate: Boolean;
    FTestDelete: Boolean;
    FTestDirectUpdate: Boolean;
    FBaseAPIPlain: Boolean;
    FBaseAPIdbc: Boolean;
    FBaseAPIdbcchached: Boolean;
    FBaseAPIdataset: Boolean;
    FResultOutput: String;
    FRepeat: Integer;
    FCount: Integer;
    FDetails: Boolean;
    FHasChanged: Boolean;
    procedure SetAPIPlain(const Value: Boolean);
    procedure SetAPIdbc(const Value: Boolean);
    procedure SetAPIdbcchached(const Value: Boolean);
    procedure SetAPIdataset(const Value: Boolean);
    procedure SetAPImidas(const Value: Boolean);
    procedure SetAPIoldzeos(const Value: Boolean);
    procedure SetAPIbde(const Value: Boolean);
    procedure SetAPIado(const Value: Boolean);
    procedure SetAPIdbx(const Value: Boolean);
    procedure SetAPIdbxc(const Value: Boolean);
    procedure SetAPIibx(const Value: Boolean);
    procedure SetTestConncet(const Value: Boolean);
    procedure SetTestInsert(const Value: Boolean);
    procedure SetTestOpen(const Value: Boolean);
    procedure SetTestFetch(const Value: Boolean);
    procedure SetTestSort(const Value: Boolean);
    procedure SetTestFilter(const Value: Boolean);
    procedure SetTestUpdate(const Value: Boolean);
    procedure SetTestDelete(const Value: Boolean);
    procedure SetTestDirectUpdate(const Value: Boolean);
    procedure SetBaseAPIPlain(const Value: Boolean);
    procedure SetBaseAPIdbc(const Value: Boolean);
    procedure SetBaseAPIdbcchached(const Value: Boolean);
    procedure SetBaseAPIdataset(const Value: Boolean);
    procedure SetResultOutput(const Value: String);
    procedure SetRepeat(const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetDetails(const Value: Boolean);
  public
    constructor Create;
    procedure PostUpdates;
    property APIPlain: Boolean read FAPIPlain write SetAPIPlain;
    property APIdbc: Boolean read FAPIdbc write SetAPIdbc;
    property APIdbcchached: Boolean read FAPIdbcchached write SetAPIdbcchached;
    property APIdataset: Boolean read FAPIdataset write SetAPIdataset;
    property APImidas: Boolean read FAPImidas write SetAPImidas;
    property APIoldzeos: Boolean read FAPIoldzeos write SetAPIoldzeos;
    property APIbde: Boolean read FAPIbde write SetAPIbde;
    property APIado: Boolean read FAPIado write SetAPIado;
    property APIdbx: Boolean read FAPIdbx write SetAPIdbx;
    property APIdbxc: Boolean read FAPIdbxc write SetAPIdbxc;
    property APIibx: Boolean read FAPIibx write SetAPIibx;
    property TestConncet: Boolean read FTestConncet write SetTestConncet;
    property TestInsert: Boolean read FTestInsert write SetTestInsert;
    property TestOpen: Boolean read FTestOpen write SetTestOpen;
    property TestFetch: Boolean read FTestFetch write SetTestFetch;
    property TestSort: Boolean read FTestSort write SetTestSort;
    property TestFilter: Boolean read FTestFilter write SetTestFilter;
    property TestUpdate: Boolean read FTestUpdate write SetTestUpdate;
    property TestDelete: Boolean read FTestDelete write SetTestDelete;
    property TestDirectUpdate: Boolean read FTestDirectUpdate write SetTestDirectUpdate;
    property BaseAPIPlain: Boolean read FBaseAPIPlain write SetBaseAPIPlain;
    property BaseAPIdbc: Boolean read FBaseAPIdbc write SetBaseAPIdbc;
    property BaseAPIdbcchached: Boolean read FBaseAPIdbcchached write SetBaseAPIdbcchached;
    property BaseAPIdataset: Boolean read FBaseAPIdataset write SetBaseAPIdataset;
    property ResultOutput: String read FResultOutput write SetResultOutput;
    property RepeatCount: Integer read FRepeat write SetRepeat;
    property Records: Integer read FCount write SetCount;
    property Details: Boolean read FDetails write SetDetails;
    property Changed: Boolean read FHasChanged;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    cbgTests: TCheckGroup;
    cbUnicode: TCheckBox;
    cbPreprepareSQL: TCheckBox;
    cbRebuild: TCheckBox;
    cbCreateDB: TCheckBox;
    cbAnsiCP: TComboBox;
    cbUnicodeCP: TComboBox;
    cbDetails: TCheckBox;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    CheckGroup3: TCheckGroup;
    eDatabase: TEdit;
    eDelimiter: TEdit;
    eDelimiterType: TEdit;
    eBuildScripts: TEdit;
    eRecords: TEdit;
    eRepeat: TEdit;
    ePort: TEdit;
    eDropScripts: TEdit;
    eLibLocation: TEdit;
    ePassword: TEdit;
    eUser: TEdit;
    eHost: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lbDrivers: TListBox;
    Properties: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    mProperties: TMemo;
    MenuItem1: TMenuItem;
    miClose: TMenuItem;
    miNew: TMenuItem;
    MenuItem3: TMenuItem;
    miOpen: TMenuItem;
    miPost: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    RadioGroup1: TRadioGroup;
    tsPerformance: TTabSheet;
    tsMain: TTabSheet;
    ZConnection1: TZConnection;
    procedure cbCreateDBEditingDone(Sender: TObject);
    procedure cbgTestsItemClick(Sender: TObject; Index: integer);
    procedure cbPreprepareSQLEditingDone(Sender: TObject);
    procedure cbRebuildEditingDone(Sender: TObject);
    procedure cbUnicodeCPEditingDone(Sender: TObject);
    procedure cbUnicodeEditingDone(Sender: TObject);
    procedure cbAnsiCPEditingDone(Sender: TObject);
    procedure eBuildScriptsEditingDone(Sender: TObject);
    procedure eDatabaseEditingDone(Sender: TObject);
    procedure eDelimiterEditingDone(Sender: TObject);
    procedure eDelimiterTypeEditingDone(Sender: TObject);
    procedure eDropScriptsEditingDone(Sender: TObject);
    procedure eHostEditingDone(Sender: TObject);
    procedure eLibLocationEditingDone(Sender: TObject);
    procedure ePasswordEditingDone(Sender: TObject);
    procedure eUserEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lbDriversSelectionChange(Sender: TObject; User: boolean);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPostClick(Sender: TObject);
    procedure mPropertiesEditingDone(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses IniFiles, ZSysUtils;

var
  Ini: TIniFile;

function ReadProperty(const Group, Key, Default: string): string;
begin
  if Assigned(Ini) then
    Result := Trim(Ini.ReadString(Group, Group + '.' + Key, Default))
  else Result := '';
  if ( Result = '' ) and ( Default <> '' ) then Result := Default;
end;

procedure WriteProperty( const Group, Key, Value: string);
begin
  if Assigned(Ini) then
    Ini.WriteString(Group, Group + '.' + Key, Value);
end;

constructor TPlainConfig.Create(const AProtocol: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  FProtocol := AProtocol;
  if Assigned(Ini) then
  begin

    PutSplitStringEx(SL, ReadProperty(COMMON_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FCommon := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(CORE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FCore := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PARSESQL_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FParseSQL := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PLAIN_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FPlain := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(DBC_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FDbc := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(COMPONENT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FComponent := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(BUGREPORT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FBugreport := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PERFORMANCE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), LIST_DELIMITERS);
    FPerformance := SL.IndexOf(AProtocol) > -1;

    FAlias := ReadProperty(AProtocol, DATABASE_ALIAS_KEY, '');
    FHostName := ReadProperty(AProtocol, DATABASE_HOST_KEY, DEFAULT_HOST_VALUE);
    FPort := ReadProperty(AProtocol, DATABASE_PORT_KEY, '0');
    FDatabase := ReadProperty(AProtocol, DATABASE_NAME_KEY, '');
    FPassword := ReadProperty(AProtocol, DATABASE_PASSWORD_KEY, '');
    FRebuild := StrToBoolEx(ReadProperty(AProtocol, DATABASE_REBUILD_KEY, 'No'));
    if ( Pos('firebird', AProtocol) > 0 ) or (pos('interbase', AProtocol) > 0) then
      FDelimiterType := ReadProperty(AProtocol, DATABASE_DELIMITER_TYPE_KEY, SET_TERM_DELIMITER)
    else if ( Pos('mssql', LowerCase(AProtocol)) > 0 ) or (pos('sybase', LowerCase(AProtocol)) > 0) then
      FDelimiterType := ReadProperty(AProtocol, DATABASE_DELIMITER_TYPE_KEY, GO_DELIMITER)
    else
      FDelimiterType := ReadProperty(AProtocol, DATABASE_DELIMITER_TYPE_KEY, '');
    FDelimiter := ReadProperty(AProtocol, DATABASE_DELIMITER_KEY, DEFAULT_DELIMITER);

    FUnicode := StrToBoolEx(ReadProperty(AProtocol, 'use.unicode.charset', 'Yes'));
    FBuildScripts := ReadProperty(AProtocol, DATABASE_CREATE_SCRIPTS_KEY, '');
    FDropScripts := ReadProperty(AProtocol, DATABASE_DROP_SCRIPTS_KEY, '');
    FLibLocation := ReadProperty(AProtocol, DATABASE_LIBRARY_KEY, '');
    FUnicodeCodePage := ReadProperty(AProtocol, 'unicode.charset', '');
    FAnsiCodePage := ReadProperty(AProtocol, 'ansi.charset', '');
    FPreprepareSQL := StrToBoolEx(ReadProperty(AProtocol, DATABASE_PREPREPARESQL_KEY, 'Yes'));
    FCreateDatabase := StrToBoolEx(ReadProperty(AProtocol, DATABASE_CREATE_KEY, 'No'));
    PutSplitStringEx(SL, ReadProperty(AProtocol, DATABASE_PROPERTIES_KEY, ''), ';');
    {Drop common values}
    if SL.Values[DATABASE_LIBRARY_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_LIBRARY_KEY));
    if SL.Values[DATABASE_CODEPAGE_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_CODEPAGE_KEY));
    if SL.Values[DATABASE_CREATE_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_CREATE_KEY));
    if SL.Values[DATABASE_PREPREPARESQL_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_PREPREPARESQL_KEY));
    FProperties := SL.Text;
  end;
  SL.Free;
end;

procedure TPlainConfig.PostUpdates;
var
  SProperties: String;

  procedure AddPropText(const Ident, Value: String);
  begin
    if Value <> '' then
      if SProperties <> '' then
        if Ident <> '' then
           SProperties := SProperties+';'+Ident+'='+Value
        else
           SProperties := SProperties+';'+Value
      else
        if Ident <> '' then
          SProperties := Ident+'='+Value
        else
          SProperties := Value;
  end;

begin
  if Assigned(Ini) then
  begin
    WriteProperty(FProtocol, DATABASE_PROTOCOL_KEY, FProtocol);
    WriteProperty(FProtocol, DATABASE_ALIAS_KEY, FAlias);
    WriteProperty(FProtocol, DATABASE_HOST_KEY, FHostName);
    WriteProperty(FProtocol, DATABASE_PORT_KEY, FPort);
    WriteProperty(FProtocol, DATABASE_NAME_KEY, FDatabase);
    WriteProperty(FProtocol, DATABASE_PASSWORD_KEY, FPassword);
    if FRebuild then
       WriteProperty(FProtocol, DATABASE_REBUILD_KEY, 'Yes')
    else
      WriteProperty(FProtocol, DATABASE_REBUILD_KEY, 'No');
    WriteProperty(FProtocol, DATABASE_DELIMITER_TYPE_KEY, FDelimiterType);
    WriteProperty(FProtocol, DATABASE_DELIMITER_KEY, FDelimiter);

    if FUnicode then
    begin
       WriteProperty(FProtocol, 'use.unicode.charset', 'Yes');
       AddPropText(DATABASE_CODEPAGE_KEY, FUnicodeCodePage);
    end
    else
    begin
      WriteProperty(FProtocol, 'use.unicode.charset', 'No');
      AddPropText(DATABASE_CODEPAGE_KEY, FAnsiCodePage);
    end;
    WriteProperty(FProtocol, DATABASE_CREATE_SCRIPTS_KEY, FBuildScripts);
    WriteProperty(FProtocol, DATABASE_DROP_SCRIPTS_KEY, FDropScripts);
    WriteProperty(FProtocol, DATABASE_LIBRARY_KEY, FLibLocation);
    AddPropText(DATABASE_LIBRARY_KEY, FLibLocation);
    WriteProperty(FProtocol, 'unicode.charset', FUnicodeCodePage);
    WriteProperty(FProtocol, 'ansi.charset', FAnsiCodePage);
    if FPreprepareSQL then
    begin
      WriteProperty(FProtocol, DATABASE_PREPREPARESQL_KEY, 'Yes');
      AddPropText(DATABASE_PREPREPARESQL_KEY, 'ON');
    end
    else
      WriteProperty(FProtocol, DATABASE_PREPREPARESQL_KEY, 'Yes');
    if FCreateDatabase then
    begin
      WriteProperty(FProtocol, DATABASE_CREATE_KEY, 'Yes');
      AddPropText(DATABASE_CREATE_KEY, FDatabase);
    end
    else
      WriteProperty(FProtocol, DATABASE_CREATE_KEY, 'No');

    AddPropText('', StringReplace(TrimRight(FProperties), LineEnding, ';', [rfReplaceAll]));

    WriteProperty(FProtocol, DATABASE_PROPERTIES_KEY, SProperties);
    FHasChanged := False;
  end;
end;

procedure TPlainConfig.SetCommon(const Value: Boolean);
begin
  FHasChanged := FCommon <> Value;
  FCommon := Value;
end;


procedure TPlainConfig.SetAnsiCodePage(const Value: string);
begin
  FHasChanged := FAnsiCodePage <> Value;
  FAnsiCodePage := Value;
end;

procedure TPlainConfig.SetUnicodeCodePage(const Value: string);
begin
  FHasChanged := FUnicodeCodePage <> Value;
  FUnicodeCodePage := Value;
end;

procedure TPlainConfig.SetUnicode(const Value: boolean);
begin
  FHasChanged := FUnicode <> Value;
  FUnicode := Value;
end;

procedure TPlainConfig.SetPreprepareSQL(const Value: Boolean);
begin
  FHasChanged := FPreprepareSQL <> Value;
  FPreprepareSQL := Value;
end;

procedure TPlainConfig.SetCore(const Value: boolean);
begin
  FHasChanged := FCore <> Value;
  FCore := Value;
end;

procedure TPlainConfig.SetDbc(const Value: boolean);
begin
  FHasChanged := FDbc <> Value;
  FDbc := Value;
end;


procedure TPlainConfig.SetParseSQL(const Value: boolean);
begin
  FHasChanged := FParseSQL <> Value;
  FParseSQL := Value;
end;

procedure TPlainConfig.SetPlain(const Value: boolean);
begin
  FHasChanged := FPlain <> Value;
  FPlain := Value;
end;

procedure TPlainConfig.SetComponent(const Value: boolean);
begin
  FHasChanged := FComponent <> Value;
  FComponent := Value;
end;

procedure TPlainConfig.SetBugreport(const Value: boolean);
begin
  FHasChanged := FBugReport <> Value;
  FBugReport := Value;
end;

procedure TPlainConfig.SetPerformance(const Value: boolean);
begin
  FHasChanged := FPerformance <> Value;
  FPerformance := Value;
end;

procedure TPlainConfig.SetAlias(const Value: String);
begin
  FHasChanged := FAlias <> Value;
  FAlias := Value;
end;

procedure TPlainConfig.SetRebuild(const Value: Boolean);
begin
  FHasChanged := FRebuild <> Value;
  FRebuild := Value;
end;

procedure TPlainConfig.SetBuildScripts(const Value: string);
begin
  FHasChanged := FBuildScripts <> Value;
  FBuildScripts := Value;
end;

procedure TPlainConfig.SetDropScripts(const Value: string);
begin
  FHasChanged := FDropScripts <> Value;
  FDropScripts := Value;
end;

procedure TPlainConfig.SetProtocol(const Value: string);
begin
  FHasChanged := FProtocol <> Value;
  FProtocol := Value;
end;

procedure TPlainConfig.SetHostName(const Value: string);
begin
  FHasChanged := FHostName <> Value;
  FHostName := Value;
end;

procedure TPlainConfig.SetConnPort(const Value: String);
begin
  FHasChanged := FPort <> Value;
  FPort := Value;
end;

procedure TPlainConfig.SetDatabase(const Value: string);
begin
  FHasChanged := FDatabase <> Value;
  FDatabase := Value;
end;

procedure TPlainConfig.SetUserName(const Value: string);
begin
  FHasChanged := FUserName <> Value;
  FUserName := Value;
end;

procedure TPlainConfig.SetPassword(const Value: string);
begin
  FHasChanged := FPassword <> Value;
  FPassword := Value;
end;

procedure TPlainConfig.SetLibLocation(const Value: String);
begin
  FHasChanged := FLibLocation <> Value;
  FLibLocation := Value;
end;
procedure TPlainConfig.SetDelimiter(const Value: String);
begin
  FHasChanged := FDelimiter <> Value;
  FDelimiter := Value;
end;

procedure TPlainConfig.SetDelimiterType(const Value: String);
begin
  FHasChanged := FDelimiterType <> Value;
  FDelimiterType := Value;
end;

procedure TPlainConfig.SetProperties(const Value: String);
begin
  FHasChanged := FProperties <> Value;
  FProperties := Value;
end;

procedure TPlainConfig.SetCreateDatabase(const Value: Boolean);
begin
  FHasChanged := FCreateDatabase <> Value;
  FCreateDatabase := Value;
end;

constructor TPerformanceConfig.Create;
begin

end;

procedure TPerformanceConfig.PostUpdates;
begin

end;

procedure TPerformanceConfig.SetAPIPlain(const Value: Boolean);
begin
  FHasChanged := FAPIPlain <> Value;
  FAPIPlain := Value;
end;

procedure TPerformanceConfig.SetAPIdbc(const Value: Boolean);
begin
  FHasChanged := FAPIdbc <> Value;
  FAPIdbc := Value;
end;

procedure TPerformanceConfig.SetAPIdbcchached(const Value: Boolean);
begin
  FHasChanged := FAPIdbcchached <> Value;
  FAPIdbcchached := Value;
end;

procedure TPerformanceConfig.SetAPIdataset(const Value: Boolean);
begin
  FHasChanged := FAPIdataset <> Value;
  FAPIdataset := Value;
end;

procedure TPerformanceConfig.SetAPImidas(const Value: Boolean);
begin
  FHasChanged := FAPImidas <> Value;
  FAPImidas := Value;
end;

procedure TPerformanceConfig.SetAPIoldzeos(const Value: Boolean);
begin
  FHasChanged := FAPIoldzeos <> Value;
  FAPIoldzeos := Value;
end;

procedure TPerformanceConfig.SetAPIbde(const Value: Boolean);
begin
  FHasChanged := FAPIbde <> Value;
  FAPIbde := Value;
end;

procedure TPerformanceConfig.SetAPIado(const Value: Boolean);
begin
  FHasChanged := FAPIado <> Value;
  FAPIado := Value;
end;

procedure TPerformanceConfig.SetAPIdbx(const Value: Boolean);
begin
  FHasChanged := FAPIdbx <> Value;
  FAPIdbx := Value;
end;

procedure TPerformanceConfig.SetAPIdbxc(const Value: Boolean);
begin
  FHasChanged := FAPIdbxc <> Value;
  FAPIdbxc := Value;
end;

procedure TPerformanceConfig.SetAPIibx(const Value: Boolean);
begin
  FHasChanged := FAPIibx <> Value;
  FAPIibx := Value;
end;

procedure TPerformanceConfig.SetTestConncet(const Value: Boolean);
begin
  FHasChanged := FTestConncet <> Value;
  FTestConncet := Value;
end;

procedure TPerformanceConfig.SetTestInsert(const Value: Boolean);
begin
  FHasChanged := FTestInsert <> Value;
  FTestInsert := Value;
end;

procedure TPerformanceConfig.SetTestOpen(const Value: Boolean);
begin
  FHasChanged := FTestOpen <> Value;
  FTestOpen := Value;
end;

procedure TPerformanceConfig.SetTestFetch(const Value: Boolean);
begin
  FHasChanged := FTestFetch <> Value;
  FTestFetch := Value;
end;

procedure TPerformanceConfig.SetTestSort(const Value: Boolean);
begin
  FHasChanged := FTestSort <> Value;
  FTestSort := Value;
end;

procedure TPerformanceConfig.SetTestFilter(const Value: Boolean);
begin
  FHasChanged := FTestFilter <> Value;
  FTestFilter := Value;
end;

procedure TPerformanceConfig.SetTestUpdate(const Value: Boolean);
begin
  FHasChanged := FTestUpdate <> Value;
  FTestUpdate := Value;
end;

procedure TPerformanceConfig.SetTestDelete(const Value: Boolean);
begin
  FHasChanged := FTestDelete <> Value;
  FTestDelete := Value;
end;

procedure TPerformanceConfig.SetTestDirectUpdate(const Value: Boolean);
begin
  FHasChanged := FTestDirectUpdate <> Value;
  FTestDirectUpdate := Value;
end;

procedure TPerformanceConfig.SetBaseAPIPlain(const Value: Boolean);
begin
  FHasChanged := FBaseAPIPlain <> Value;
  FBaseAPIPlain := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdbc(const Value: Boolean);
begin
  FHasChanged := FBaseAPIdbc <> Value;
  FBaseAPIdbc := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdbcchached(const Value: Boolean);
begin
  FHasChanged := FBaseAPIdbc <> Value;
  FBaseAPIdbc := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdataset(const Value: Boolean);
begin
  FHasChanged := FBaseAPIdataset <> Value;
  FBaseAPIdataset := Value;
end;

procedure TPerformanceConfig.SetResultOutput(const Value: String);
begin
  FHasChanged := FResultOutput <> Value;
  FResultOutput := Value;
end;

procedure TPerformanceConfig.SetRepeat(const Value: Integer);
begin
  FHasChanged := FRepeat <> Value;
  FRepeat := Value;
end;

procedure TPerformanceConfig.SetCount(const Value: Integer);
begin
  FHasChanged := FCount <> Value;
  FCount := Value;
end;

procedure TPerformanceConfig.SetDetails(const Value: Boolean);
begin
  FHasChanged := FDetails <> Value;
  FDetails := Value;
end;

{ TfrmMain }
procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
  SL: TStrings;
begin
  SL := TStringList.Create;
  ZConnection1.GetProtocolNames(SL);
  TStringList(SL).Sort;
  if FileExists(DefaultFile) then
    Ini := TIniFile.Create(DefaultFile)
  else
    if OpenDialog1.Execute then
    begin
      if OpenDialog1.Filename <> '' then
        Ini := TIniFile.Create(OpenDialog1.Filename);
    end;
  if not Assigned(Ini) then
  begin
    Ini := TIniFile.Create('test.properties');
    ShowMessage('New test.properties will be created. '+LineEnding+
      'Copy this file into the Zeos \database folder. Enjoy!');
  end;
  for i := 0 to SL.Count -1 do
    lbDrivers.Items.AddObject(SL[i], TPlainConfig.Create(SL[i]));
  if lbDrivers.Items.Count > 0 then lbDrivers.ItemIndex:=0;
  lbDriversSelectionChange(Sender, True);
end;

procedure TfrmMain.lbDriversSelectionChange(Sender: TObject; User: boolean);
begin
  if User then
  begin
    cbgTests.Checked[0] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Common;
    cbgTests.Checked[1] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Core;
    cbgTests.Checked[2] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).ParseSQL;
    cbgTests.Checked[3] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Plain;
    cbgTests.Checked[4] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Dbc;
    cbgTests.Checked[5] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Component;
    cbgTests.Checked[6] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Bugreport;
    cbgTests.Checked[7] := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Performance;
    eDatabase.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Database;
    eHost.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).HostName;
    ePort.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Port;
    eUser.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).UserName;
    ePassword.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Password;
    eLibLocation.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).LibLocation;
    eDelimiter.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Delimiter;
    cbAnsiCP.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).AnsiCodePage;
    cbUnicodeCP.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).UnicodeCodePage;
    cbUnicode.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Unicode;
    cbPreprepareSQL.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).PreprepareSQL;
    cbRebuild.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Rebuild;
    cbCreateDB.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).CreateDatabase;
    eDelimiterType.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DelimiterType;
    eBuildScripts.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).BuildScripts;
    eDropScripts.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DropScripts;
    mProperties.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Properties;
  end;
end;

procedure TfrmMain.miNewClick(Sender: TObject);
var
  I: Integer;
  FChanged: Boolean;
  procedure OpenIniFile;
  begin
    if OpenDialog1.Execute then
    begin
      Self.lbDrivers.Clear;
      if OpenDialog1.Filename <> '' then
      begin
        if Assigned(Ini) then
          Ini.Free;
        Ini := TIniFile.Create(OpenDialog1.Filename);
      end;
    end;
  end;
begin
  FChanged := False;
  for i := 0 to lbDrivers.items.Count -1 do
    if TPlainConfig(lbDrivers.items.Objects[i]).Changed then
    begin
      FChanged := True;
      Break;
    end;
  if FChanged then
    case MessageDlg('Settings have been changed. Post updates?', mtConfirmation,
      [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        begin
          miPostClick(Sender);
          OpenIniFile;
        end;
      mrNo: OpenIniFile;
      mrAbort:;
    end
  else OpenIniFile;
end;

procedure TfrmMain.miOpenClick(Sender: TObject);
begin
  miNewClick(Sender);
end;

procedure TfrmMain.miPostClick(Sender: TObject);
var
  I: Integer;
  SL: TStrings;
  SCommon, SCore, SParseSQL, SPlain, SDbc, SComponent, SBugreport,
    SPerformance: String;

  procedure AddGroup(var Ident: String; const Value: String; const Add: Boolean);
  begin
    if Add then
      if Ident <> '' then
        Ident := Ident+','+Value
      else
        Ident := Value;
  end;

begin
  SL := TStringList.Create;
  for i := 0 to lbDrivers.items.Count -1 do
  begin
    AddGroup(SCommon, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Common);
    AddGroup(SCore, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Core);
    AddGroup(SParseSQL, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).ParseSQL);
    AddGroup(SPlain, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Plain);
    AddGroup(SDbc, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Dbc);
    AddGroup(SComponent, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Component);
    AddGroup(SBugreport, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Bugreport);
    AddGroup(SPerformance, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Performance);
    if TPlainConfig(lbDrivers.items.Objects[I]).Changed then
      TPlainConfig(lbDrivers.items.Objects[I]).PostUpdates;
  end;

  WriteProperty(COMMON_GROUP, ACTIVE_CONNECTIONS_KEY, SCommon);
  WriteProperty(CORE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SCore);
  WriteProperty(PARSESQL_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SParseSQL);
  WriteProperty(PLAIN_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SPlain);
  WriteProperty(DBC_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SDbc);
  WriteProperty(COMPONENT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SComponent);
  WriteProperty(BUGREPORT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SBugreport);
  WriteProperty(PERFORMANCE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SPerformance);
  SL.Free;
end;

procedure TfrmMain.mPropertiesEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Properties := mProperties.Text;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  I: Integer;
  FChanged: Boolean;
begin
  FChanged := False;
  for i := 0 to lbDrivers.items.Count -1 do
    if TPlainConfig(lbDrivers.items.Objects[i]).Changed then
    begin
      FChanged := True;
      Break;
    end;
  if FChanged then
    case MessageDlg('Settings have been changed. Post updates?', mtConfirmation,
      [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        begin
          miPostClick(Sender);
          CloseAction := caFree;
        end;
      mrNo: CloseAction := caFree;
      mrAbort: CloseAction := caNone;
    end;
end;

procedure TfrmMain.cbgTestsItemClick(Sender: TObject; Index: integer);
begin
  case Index of
    0: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Common := cbgTests.Checked[0];
    1: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Core := cbgTests.Checked[1];
    2: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).ParseSQL := cbgTests.Checked[2];
    3: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Plain := cbgTests.Checked[3];
    4: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Dbc := cbgTests.Checked[4];
    5: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Component := cbgTests.Checked[5];
    6: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Bugreport := cbgTests.Checked[6];
    7: TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Performance := cbgTests.Checked[7];
  end;
end;

procedure TfrmMain.cbCreateDBEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).CreateDatabase := cbCreateDB.Checked;
end;

procedure TfrmMain.cbPreprepareSQLEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).PreprepareSQL := cbPreprepareSQL.Checked;
end;

procedure TfrmMain.cbRebuildEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Rebuild := cbRebuild.Checked;
end;

procedure TfrmMain.cbUnicodeCPEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).UnicodeCodePage := cbUnicodeCP.Text;
end;

procedure TfrmMain.cbUnicodeEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Unicode := cbUnicode.Checked;
end;

procedure TfrmMain.cbAnsiCPEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).AnsiCodePage := cbAnsiCP.Text;
end;

procedure TfrmMain.eBuildScriptsEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).BuildScripts := eBuildScripts.Text;
end;

procedure TfrmMain.eDatabaseEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Database := eDatabase.Text;
end;

procedure TfrmMain.eDelimiterEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Delimiter := eDelimiter.Text;
end;

procedure TfrmMain.eDelimiterTypeEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DelimiterType := eDelimiterType.Text;
end;

procedure TfrmMain.eDropScriptsEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DropScripts := eDropScripts.Text;
end;

procedure TfrmMain.eHostEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).HostName := eHost.Text;
end;

procedure TfrmMain.eLibLocationEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).LibLocation := eLibLocation.Text;
end;

procedure TfrmMain.ePasswordEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Password := ePassword.Text;
end;

procedure TfrmMain.eUserEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).UserName := eUser.Text;
end;

{$R *.lfm}

end.

