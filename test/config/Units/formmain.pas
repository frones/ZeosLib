{*********************************************************}
{                                                         }
{             Zeos test-suite Configurator                }
{                                                         }
{        Originally written by Michael Hiergeist          }
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
  DATABASE_AUTOENCODE_KEY     = 'AutoEncodeStrings';
  DATABASE_CREATE_KEY         = 'createNewDatabase';
  DATABASE_CODEPAGE_KEY       = 'codepage';
  DATABASE_POOLED_KEY         = 'pooled';

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
  { Performance tests values}
  PERFORMANCE_APIS           = 'apis';
  PERFORMANCE_TESTS          = 'tests';
  PERFORMANCE_REPEAT         = 'repeat';
  PERFORMANCE_REOCORDS       = 'records';
  PERFORMANCE_OUTPUT         = 'output';
  PERFORMANCE_BASEAPI        = 'baseapi';
  PERFORMANCE_PRINTDETAILS   = 'printdetails';
const
  { Performance APIs }
  plain         = 'plain';
  dbc           = 'dbc';
  dbc_cached    = 'dbc-cached';
  dataset       = 'dataset';
  midas         = 'midas';
  old_zeos      = 'old-zeos';
  bde           = 'bde';
  ado           = 'ado';
  dbx           = 'dbx';
  dbxc          = 'dbxc';
  ibx           = 'ibx';
const
  { Performance tests }
  test_connect       = 'connect';
  test_insert        = 'insert';
  test_open          = 'open';
  test_fetch         = 'fetch';
  test_sort          = 'sort';
  test_filter        = 'filter';
  test_update        = 'update';
  test_delete        = 'delete';
  test_direct_update = 'direct-update';
const
  { Performance Output }
  out_plain          = plain;
  out_csv            = 'csv';
  out_html           = 'html';
const
  { Performance options }
  opt_repeat         = 'repeat';
  opt_records        = 'records';
  opt_print_details  = 'printdetails';
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
    FNotes: String;
    FPooled: Boolean;
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
    procedure SetNotes(const Value: String);
    procedure SetCreateDatabase(const Value: Boolean);
    procedure SetPooled(const Value: Boolean);
  public
    constructor Create(const AProtocol: String);
    procedure PostUpdates(const Suffix: String = '');
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
    property Notes: String read FNotes write SetNotes;
    property Pooled: Boolean read FPooled write SetPooled;
    property Changed: Boolean read FHasChanged;
  end;

  TPerformanceConfig = class(TObject)
  private
    FAPIPlain: Boolean;
    FAPIdbc: Boolean;
    FAPIdbc_cached: Boolean;
    FAPIdataset: Boolean;
    FAPImidas: Boolean;
    FAPIold_zeos: Boolean;
    FAPIbde: Boolean;
    FAPIado: Boolean;
    FAPIdbx: Boolean;
    FAPIdbxc: Boolean;
    FAPIibx: Boolean;
    FTestConnect: Boolean;
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
    FBaseAPIdbccached: Boolean;
    FBaseAPIdataset: Boolean;
    FResultOutput: String;
    FRepeat: Integer;
    FCount: Integer;
    FDetails: Boolean;
    FHasChanged: Boolean;
    procedure SetAPIPlain(const Value: Boolean);
    procedure SetAPIdbc(const Value: Boolean);
    procedure SetAPIdbccached(const Value: Boolean);
    procedure SetAPIdataset(const Value: Boolean);
    procedure SetAPImidas(const Value: Boolean);
    procedure SetAPIoldzeos(const Value: Boolean);
    procedure SetAPIbde(const Value: Boolean);
    procedure SetAPIado(const Value: Boolean);
    procedure SetAPIdbx(const Value: Boolean);
    procedure SetAPIdbxc(const Value: Boolean);
    procedure SetAPIibx(const Value: Boolean);
    procedure SetTestConnect(const Value: Boolean);
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
    procedure SetBaseAPIdbccached(const Value: Boolean);
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
    property APIdbccached: Boolean read FAPIdbc_cached write SetAPIdbccached;
    property APIdataset: Boolean read FAPIdataset write SetAPIdataset;
    property APImidas: Boolean read FAPImidas write SetAPImidas;
    property APIoldzeos: Boolean read FAPIold_zeos write SetAPIoldzeos;
    property APIbde: Boolean read FAPIbde write SetAPIbde;
    property APIado: Boolean read FAPIado write SetAPIado;
    property APIdbx: Boolean read FAPIdbx write SetAPIdbx;
    property APIdbxc: Boolean read FAPIdbxc write SetAPIdbxc;
    property APIibx: Boolean read FAPIibx write SetAPIibx;
    property TestConnect: Boolean read FTestConnect write SetTestConnect;
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
    property BaseAPIdbccached: Boolean read FBaseAPIdbccached write SetBaseAPIdbccached;
    property BaseAPIdataset: Boolean read FBaseAPIdataset write SetBaseAPIdataset;
    property ResultOutput: String read FResultOutput write SetResultOutput;
    property RepeatCount: Integer read FRepeat write SetRepeat;
    property Records: Integer read FCount write SetCount;
    property Details: Boolean read FDetails write SetDetails;
    property Changed: Boolean read FHasChanged;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    cbAnsiCP: TComboBox;
    cbCreateDB: TCheckBox;
    cbgTests: TCheckGroup;
    cbAutoEncode: TCheckBox;
    cbPrintDetails: TCheckBox;
    cbRebuild: TCheckBox;
    cbUnicode: TCheckBox;
    cbUnicodeCP: TComboBox;
    cgAPIS: TCheckGroup;
    cgTests: TCheckGroup;
    cgBaseAPIS: TCheckGroup;
    cbPooled: TCheckBox;
    eBuildScripts: TEdit;
    eDatabase: TEdit;
    eDelimiter: TEdit;
    eDelimiterType: TEdit;
    eAlias: TEdit;
    eDropScripts: TEdit;
    eHost: TEdit;
    eLibLocation: TEdit;
    ePassword: TEdit;
    ePort: TEdit;
    eRecords: TEdit;
    eRepeat: TEdit;
    eUser: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDrivers: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miClose: TMenuItem;
    miNew: TMenuItem;
    MenuItem3: TMenuItem;
    miOpen: TMenuItem;
    miPost: TMenuItem;
    MenuItem9: TMenuItem;
    mProperties: TMemo;
    mNotes: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    rgOutput: TRadioGroup;
    Splitter1: TSplitter;
    tsNotes: TTabSheet;
    tsProperties: TTabSheet;
    tsPerformance: TTabSheet;
    tsMain: TTabSheet;
    procedure cbCreateDBEditingDone(Sender: TObject);
    procedure cbgTestsItemClick(Sender: TObject; Index: integer);
    procedure cbPooledEditingDone(Sender: TObject);
    procedure cbAutoEncodeEditingDone(Sender: TObject);
    procedure cbPrintDetailsClick(Sender: TObject);
    procedure cbRebuildEditingDone(Sender: TObject);
    procedure cbUnicodeCPEditingDone(Sender: TObject);
    procedure cbUnicodeEditingDone(Sender: TObject);
    procedure cbAnsiCPEditingDone(Sender: TObject);
    procedure cgAPISItemClick(Sender: TObject; Index: integer);
    procedure cgBaseAPISItemClick(Sender: TObject; Index: integer);
    procedure cgTestsItemClick(Sender: TObject; Index: integer);
    procedure eAliasEditingDone(Sender: TObject);
    procedure eBuildScriptsEditingDone(Sender: TObject);
    procedure eDatabaseEditingDone(Sender: TObject);
    procedure eDelimiterEditingDone(Sender: TObject);
    procedure eDelimiterTypeEditingDone(Sender: TObject);
    procedure eDropScriptsEditingDone(Sender: TObject);
    procedure eHostEditingDone(Sender: TObject);
    procedure eLibLocationEditingDone(Sender: TObject);
    procedure ePasswordEditingDone(Sender: TObject);
    procedure ePortEditingDone(Sender: TObject);
    procedure eRecordsEditingDone(Sender: TObject);
    procedure eRepeatEditingDone(Sender: TObject);
    procedure eUserEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lbDriversSelectionChange(Sender: TObject; User: boolean);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPostClick(Sender: TObject);
    procedure mNotesEditingDone(Sender: TObject);
    procedure mPropertiesEditingDone(Sender: TObject);
    procedure rgOutputClick(Sender: TObject);
  private
    { private declarations }
    PerformanceConfig: TPerformanceConfig;
    ZCon: TZConnection;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses IniFiles, ZSysUtils, ZDbcIntfs, ZCompatibility, Types, ZURL;

{$R *.lfm}

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
    PutSplitStringEx(SL, ReadProperty(COMMON_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FCommon := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(CORE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FCore := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PARSESQL_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FParseSQL := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PLAIN_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FPlain := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(DBC_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FDbc := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(COMPONENT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FComponent := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(BUGREPORT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FBugreport := SL.IndexOf(AProtocol) > -1;
    PutSplitStringEx(SL, ReadProperty(PERFORMANCE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, ''), ',');
    FPerformance := SL.IndexOf(AProtocol) > -1;

    FAlias := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_ALIAS_KEY, '');
    FHostName := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_HOST_KEY, DEFAULT_HOST_VALUE);
    FPort := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_PORT_KEY, '');
    FDatabase := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_NAME_KEY, '');
    FUsername := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_USER_KEY, '');
    FPassword := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_PASSWORD_KEY, '');
    FRebuild := StrToBoolEx(ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_REBUILD_KEY, 'No'));
    if ( Pos('firebird', StringReplace(AProtocol, '=', '\', [rfReplaceAll])) > 0 ) or (pos('interbase', StringReplace(AProtocol, '=', '\', [rfReplaceAll])) > 0) then
      FDelimiterType := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_DELIMITER_TYPE_KEY, SET_TERM_DELIMITER)
    else if ( Pos('mssql', LowerCase(StringReplace(AProtocol, '=', '\', [rfReplaceAll]))) > 0 ) or (pos('sybase', LowerCase(StringReplace(AProtocol, '=', '\', [rfReplaceAll]))) > 0) then
      FDelimiterType := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_DELIMITER_TYPE_KEY, GO_DELIMITER)
    else
      FDelimiterType := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_DELIMITER_TYPE_KEY, '');
    FDelimiter := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_DELIMITER_KEY, '');

    FUnicode := StrToBoolEx(ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), 'use.unicode.charset', 'Yes'));
    FBuildScripts := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_CREATE_SCRIPTS_KEY, '');
    FDropScripts := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_DROP_SCRIPTS_KEY, '');
    FLibLocation := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_LIBRARY_KEY, '');
    FUnicodeCodePage := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), 'unicode.charset', '');
    FAnsiCodePage := ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), 'ansi.charset', '');
    FPreprepareSQL := StrToBoolEx(ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_AUTOENCODE_KEY, 'Yes'));
    FCreateDatabase := StrToBoolEx(ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_CREATE_KEY, 'No'));
    PutSplitStringEx(SL, ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_PROPERTIES_KEY, ''), ';');
    {Drop common values}
    if SL.Values[DATABASE_LIBRARY_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_LIBRARY_KEY));
    if SL.Values[DATABASE_CODEPAGE_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_CODEPAGE_KEY));
    if SL.Values[DATABASE_CREATE_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_CREATE_KEY));
    if SL.Values[DATABASE_AUTOENCODE_KEY] <> '' then
      SL.Delete(SL.IndexOfName(DATABASE_AUTOENCODE_KEY));
    FProperties := SL.Text;

    PutSplitStringEx(SL, ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), 'notes', ''), ';');
    FNotes := SL.Text;

    FPooled := StrToBoolEx(ReadProperty(StringReplace(AProtocol, '=', '\', [rfReplaceAll]), DATABASE_POOLED_KEY, 'No'));
  end;
  SL.Free;
end;

procedure TPlainConfig.PostUpdates(const Suffix: String = '');
var
  SProperties: String;
  AProtocol: String;

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
    AProtocol := StringReplace(FProtocol, '=', '\', [rfReplaceAll]);
    WriteProperty(Suffix+AProtocol, DATABASE_PROTOCOL_KEY, FProtocol);
    WriteProperty(Suffix+AProtocol, DATABASE_ALIAS_KEY, FAlias);
    WriteProperty(Suffix+AProtocol, DATABASE_HOST_KEY, FHostName);
    WriteProperty(Suffix+AProtocol, DATABASE_PORT_KEY, FPort);
    WriteProperty(Suffix+AProtocol, DATABASE_NAME_KEY, FDatabase);
    WriteProperty(Suffix+AProtocol, DATABASE_USER_KEY, FUserName);
    WriteProperty(Suffix+AProtocol, DATABASE_PASSWORD_KEY, FPassword);
    if FRebuild then
       WriteProperty(Suffix+AProtocol, DATABASE_REBUILD_KEY, 'Yes')
    else
      WriteProperty(Suffix+AProtocol, DATABASE_REBUILD_KEY, 'No');
    WriteProperty(Suffix+AProtocol, DATABASE_DELIMITER_TYPE_KEY, FDelimiterType);
    WriteProperty(Suffix+AProtocol, DATABASE_DELIMITER_KEY, FDelimiter);

    if FUnicode then
    begin
       WriteProperty(Suffix+AProtocol, 'use.unicode.charset', 'Yes');
       AddPropText(DATABASE_CODEPAGE_KEY, FUnicodeCodePage);
    end
    else
    begin
      WriteProperty(Suffix+AProtocol, 'use.unicode.charset', 'No');
      AddPropText(DATABASE_CODEPAGE_KEY, FAnsiCodePage);
    end;
    WriteProperty(Suffix+AProtocol, DATABASE_CREATE_SCRIPTS_KEY, FBuildScripts);
    WriteProperty(Suffix+AProtocol, DATABASE_DROP_SCRIPTS_KEY, FDropScripts);
    WriteProperty(Suffix+AProtocol, DATABASE_LIBRARY_KEY, FLibLocation);
    AddPropText(DATABASE_LIBRARY_KEY, FLibLocation);
    WriteProperty(Suffix+AProtocol, 'unicode.charset', FUnicodeCodePage);
    WriteProperty(Suffix+AProtocol, 'ansi.charset', FAnsiCodePage);
    if FPreprepareSQL then
    begin
      WriteProperty(Suffix+AProtocol, DATABASE_AUTOENCODE_KEY, 'Yes');
      AddPropText(DATABASE_AUTOENCODE_KEY, 'ON');
    end
    else
      WriteProperty(Suffix+AProtocol, DATABASE_AUTOENCODE_KEY, 'Yes');
    if FCreateDatabase then
    begin
      WriteProperty(Suffix+AProtocol, DATABASE_CREATE_KEY, 'Yes');
      AddPropText(DATABASE_CREATE_KEY, FDatabase);
    end
    else
      WriteProperty(Suffix+AProtocol, DATABASE_CREATE_KEY, 'No');
    if FPooled then
      WriteProperty(Suffix+AProtocol, DATABASE_POOLED_KEY, 'Yes')
    else
      WriteProperty(Suffix+AProtocol, DATABASE_POOLED_KEY, 'No');

    AddPropText('', StringReplace(TrimRight(FProperties), LineEnding, ';', [rfReplaceAll]));

    WriteProperty(Suffix+AProtocol, 'notes', StringReplace(TrimRight(FNotes), LineEnding, ';', [rfReplaceAll]));
    WriteProperty(Suffix+AProtocol, DATABASE_PROPERTIES_KEY, SProperties);
    if not FHasChanged then FHasChanged := False;
  end;
end;

procedure TPlainConfig.SetCommon(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FCommon <> Value;
  FCommon := Value;
end;


procedure TPlainConfig.SetAnsiCodePage(const Value: string);
begin
  if not FHasChanged then FHasChanged := FAnsiCodePage <> Value;
  FAnsiCodePage := Value;
end;

procedure TPlainConfig.SetUnicodeCodePage(const Value: string);
begin
  if not FHasChanged then FHasChanged := FUnicodeCodePage <> Value;
  FUnicodeCodePage := Value;
end;

procedure TPlainConfig.SetUnicode(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FUnicode <> Value;
  FUnicode := Value;
end;

procedure TPlainConfig.SetPreprepareSQL(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FPreprepareSQL <> Value;
  FPreprepareSQL := Value;
end;

procedure TPlainConfig.SetCore(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FCore <> Value;
  FCore := Value;
end;

procedure TPlainConfig.SetDbc(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FDbc <> Value;
  FDbc := Value;
end;


procedure TPlainConfig.SetParseSQL(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FParseSQL <> Value;
  FParseSQL := Value;
end;

procedure TPlainConfig.SetPlain(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FPlain <> Value;
  FPlain := Value;
end;

procedure TPlainConfig.SetComponent(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FComponent <> Value;
  FComponent := Value;
end;

procedure TPlainConfig.SetBugreport(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FBugReport <> Value;
  FBugReport := Value;
end;

procedure TPlainConfig.SetPerformance(const Value: boolean);
begin
  if not FHasChanged then FHasChanged := FPerformance <> Value;
  FPerformance := Value;
end;

procedure TPlainConfig.SetAlias(const Value: String);
begin
  if not FHasChanged then FHasChanged := FAlias <> Value;
  FAlias := Value;
end;

procedure TPlainConfig.SetRebuild(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FRebuild <> Value;
  FRebuild := Value;
end;

procedure TPlainConfig.SetBuildScripts(const Value: string);
begin
  if not FHasChanged then FHasChanged := FBuildScripts <> Value;
  FBuildScripts := Value;
end;

procedure TPlainConfig.SetDropScripts(const Value: string);
begin
  if not FHasChanged then FHasChanged := FDropScripts <> Value;
  FDropScripts := Value;
end;

procedure TPlainConfig.SetProtocol(const Value: string);
begin
  if not FHasChanged then FHasChanged := FProtocol <> Value;
  FProtocol := Value;
end;

procedure TPlainConfig.SetHostName(const Value: string);
begin
  if not FHasChanged then FHasChanged := FHostName <> Value;
  FHostName := Value;
end;

procedure TPlainConfig.SetConnPort(const Value: String);
begin
  if not FHasChanged then FHasChanged := FPort <> Value;
  FPort := Value;
end;

procedure TPlainConfig.SetDatabase(const Value: string);
begin
  if not FHasChanged then FHasChanged := FDatabase <> Value;
  FDatabase := Value;
end;

procedure TPlainConfig.SetUserName(const Value: string);
begin
  if not FHasChanged then FHasChanged := FUserName <> Value;
  FUserName := Value;
end;

procedure TPlainConfig.SetPassword(const Value: string);
begin
  if not FHasChanged then FHasChanged := FPassword <> Value;
  FPassword := Value;
end;

procedure TPlainConfig.SetLibLocation(const Value: String);
begin
  if not FHasChanged then FHasChanged := FLibLocation <> Value;
  FLibLocation := Value;
end;
procedure TPlainConfig.SetDelimiter(const Value: String);
begin
  if not FHasChanged then FHasChanged := FDelimiter <> Value;
  FDelimiter := Value;
end;

procedure TPlainConfig.SetDelimiterType(const Value: String);
begin
  if not FHasChanged then FHasChanged := FDelimiterType <> Value;
  FDelimiterType := Value;
end;

procedure TPlainConfig.SetProperties(const Value: String);
begin
  if not FHasChanged then FHasChanged := FProperties <> Value;
  FProperties := Value;
end;

procedure TPlainConfig.SetNotes(const Value: String);
begin
  if not FHasChanged then FHasChanged := FNotes <> Value;
  FNotes := Value;
end;

procedure TPlainConfig.SetCreateDatabase(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FCreateDatabase <> Value;
  FCreateDatabase := Value;
end;

procedure TPlainConfig.SetPooled(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FPooled <> Value;
  FPooled := Value;
end;

constructor TPerformanceConfig.Create;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  if Assigned(Ini) then
  begin
    PutSplitStringEx(SL, ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_APIS, ''), ',');
    FAPIado := SL.IndexOf(ado) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(ado)] := FAPIado;
    FAPIbde := SL.IndexOf(bde) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(bde)] := FAPIbde;
    FAPIdataset := SL.IndexOf(dataset) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(dataset)] := FAPIdataset;
    FAPIdbc := SL.IndexOf(dbc) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(dbc)] := FAPIdbc;
    FAPIplain := SL.IndexOf(plain) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(plain)] := FAPIplain;
    FAPIdbc_cached := SL.IndexOf(dbc_cached) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(dbc_cached)] := FAPIdbc_cached;
    FAPImidas := SL.IndexOf(midas) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(midas)] := FAPImidas;
    FAPIold_zeos := SL.IndexOf(old_zeos) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(old_zeos)] := FAPIold_zeos;
    FAPIdbx := SL.IndexOf(dbx) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(dbx)] := FAPIdbx;
    FAPIdbxc := SL.IndexOf(dbxc) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(dbxc)] := FAPIdbxc;
    FAPIibx := SL.IndexOf(ibx) > -1;
    frmMain.cgAPIS.Checked[frmMain.cgAPIS.items.IndexOf(ibx)] := FAPIibx;
    { Available tests }
    PutSplitStringEx(SL, ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_TESTS, ''), ',');
    FTestConnect := SL.IndexOf(test_connect) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_connect)] := FTestConnect;
    FTestInsert := SL.IndexOf(test_insert) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_insert)] := FTestInsert;
    FTestInsert := SL.IndexOf(test_insert) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_insert)] := FTestInsert;
    FTestOpen := SL.IndexOf(test_open) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_open)] := FTestOpen;
    FTestFetch := SL.IndexOf(test_fetch) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_fetch)] := FTestFetch;
    FTestSort := SL.IndexOf(test_sort) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_sort)] := FTestSort;
    FTestFilter := SL.IndexOf(test_filter) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_filter)] := FTestFilter;
    FTestUpdate := SL.IndexOf(test_update) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_update)] := FTestUpdate;
    FTestDelete := SL.IndexOf(test_delete) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_delete)] := FTestDelete;
    FTestDirectUpdate := SL.IndexOf(test_delete) > -1;
    frmMain.cgTests.Checked[frmMain.cgTests.items.IndexOf(test_direct_update)] := FTestDirectUpdate;
    { base apis }
    PutSplitStringEx(SL, ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_BASEAPI, ''), ',');
    FBaseAPIPlain := SL.IndexOf(plain) > -1;
    frmMain.cgBaseAPIS.Checked[frmMain.cgBaseAPIS.items.IndexOf(plain)] := FBaseAPIPlain;
    FBaseAPIdbc := SL.IndexOf(dbc) > -1;
    frmMain.cgBaseAPIS.Checked[frmMain.cgBaseAPIS.items.IndexOf(dbc)] := FBaseAPIdbc;
    FBaseAPIdbccached := SL.IndexOf(dbc_cached) > -1;
    frmMain.cgBaseAPIS.Checked[frmMain.cgBaseAPIS.items.IndexOf(dbc_cached)] := FBaseAPIdbccached;
    FBaseAPIdataset := SL.IndexOf(dataset) > -1;
    frmMain.cgBaseAPIS.Checked[frmMain.cgBaseAPIS.items.IndexOf(dataset)] := FBaseAPIdataset;
    { Output }
    FResultOutput := ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_OUTPUT, plain);
    frmMain.rgOutput.ItemIndex := frmMain.rgOutput.items.IndexOf(FResultOutput);
    { Options }
    FRepeat := StrToIntDef(ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_REPEAT, '3'), 3);
    frmMain.eRepeat.Text := IntToStr(FRepeat);
    FCount := StrToIntDef(ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_REOCORDS, '10000'), 10000);
    frmMain.eRecords.Text := IntToStr(FCount);
    FDetails := StrToBoolEx(ReadProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_PRINTDETAILS, 'Yes'));
  end;
  SL.Free;
end;

procedure TPerformanceConfig.PostUpdates;
var
  I: Integer;
  Temp: String;
begin
  Temp := '';
  for I := 0 to frmMain.cgAPIS.Items.count -1 do
    if frmMain.cgAPIS.Checked[i] then
      if Temp = '' then Temp := frmMain.cgAPIS.Items[i]
      else Temp := Temp+','+frmMain.cgAPIS.Items[i];
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_APIS, Temp);
  Temp := '';
  { Available tests }
  for I := 0 to frmMain.cgTests.Items.count -1 do
    if frmMain.cgTests.Checked[i] then
      if Temp = '' then Temp := frmMain.cgTests.Items[i]
      else Temp := Temp+','+frmMain.cgTests.Items[i];
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_TESTS, Temp);
  { base apis }
  Temp := '';
  for I := 0 to frmMain.cgBaseAPIS.Items.count -1 do
    if frmMain.cgBaseAPIS.Checked[i] then
      if Temp = '' then Temp := frmMain.cgBaseAPIS.Items[i]
      else Temp := Temp+','+frmMain.cgBaseAPIS.Items[i];
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_BASEAPI, Temp);
  { Output }
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_OUTPUT, frmMain.rgOutput.items[frmMain.rgOutput.ItemIndex]);
  { Options }
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_REPEAT, frmMain.eRepeat.Text);
  WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_REOCORDS, frmMain.eRecords.Text);
  if frmMain.cbPrintDetails.Checked then
    WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_PRINTDETAILS, 'Yes')
  else
    WriteProperty(PERFORMANCE_TEST_GROUP, PERFORMANCE_PRINTDETAILS, 'No');
end;

procedure TPerformanceConfig.SetAPIPlain(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIPlain <> Value;
  FAPIPlain := Value;
end;

procedure TPerformanceConfig.SetAPIdbc(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIdbc <> Value;
  FAPIdbc := Value;
end;

procedure TPerformanceConfig.SetAPIdbccached(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIdbc_cached <> Value;
  FAPIdbc_cached := Value;
end;

procedure TPerformanceConfig.SetAPIdataset(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIdataset <> Value;
  FAPIdataset := Value;
end;

procedure TPerformanceConfig.SetAPImidas(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPImidas <> Value;
  FAPImidas := Value;
end;

procedure TPerformanceConfig.SetAPIoldzeos(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIold_zeos <> Value;
  FAPIold_zeos := Value;
end;

procedure TPerformanceConfig.SetAPIbde(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIbde <> Value;
  FAPIbde := Value;
end;

procedure TPerformanceConfig.SetAPIado(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIado <> Value;
  FAPIado := Value;
end;

procedure TPerformanceConfig.SetAPIdbx(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIdbx <> Value;
  FAPIdbx := Value;
end;

procedure TPerformanceConfig.SetAPIdbxc(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIdbxc <> Value;
  FAPIdbxc := Value;
end;

procedure TPerformanceConfig.SetAPIibx(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FAPIibx <> Value;
  FAPIibx := Value;
end;

procedure TPerformanceConfig.SetTestConnect(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestConnect <> Value;
  FTestConnect := Value;
end;

procedure TPerformanceConfig.SetTestInsert(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestInsert <> Value;
  FTestInsert := Value;
end;

procedure TPerformanceConfig.SetTestOpen(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestOpen <> Value;
  FTestOpen := Value;
end;

procedure TPerformanceConfig.SetTestFetch(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestFetch <> Value;
  FTestFetch := Value;
end;

procedure TPerformanceConfig.SetTestSort(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestSort <> Value;
  FTestSort := Value;
end;

procedure TPerformanceConfig.SetTestFilter(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestFilter <> Value;
  FTestFilter := Value;
end;

procedure TPerformanceConfig.SetTestUpdate(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestUpdate <> Value;
  FTestUpdate := Value;
end;

procedure TPerformanceConfig.SetTestDelete(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestDelete <> Value;
  FTestDelete := Value;
end;

procedure TPerformanceConfig.SetTestDirectUpdate(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FTestDirectUpdate <> Value;
  FTestDirectUpdate := Value;
end;

procedure TPerformanceConfig.SetBaseAPIPlain(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FBaseAPIPlain <> Value;
  FBaseAPIPlain := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdbc(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FBaseAPIdbc <> Value;
  FBaseAPIdbc := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdbccached(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FBaseAPIdbccached <> Value;
  FBaseAPIdbccached := Value;
end;

procedure TPerformanceConfig.SetBaseAPIdataset(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FBaseAPIdataset <> Value;
  FBaseAPIdataset := Value;
end;

procedure TPerformanceConfig.SetResultOutput(const Value: String);
begin
  if not FHasChanged then FHasChanged := FResultOutput <> Value;
  FResultOutput := Value;
end;

procedure TPerformanceConfig.SetRepeat(const Value: Integer);
begin
  if not FHasChanged then FHasChanged := FRepeat <> Value;
  FRepeat := Value;
end;

procedure TPerformanceConfig.SetCount(const Value: Integer);
begin
  if not FHasChanged then FHasChanged := FCount <> Value;
  FCount := Value;
end;

procedure TPerformanceConfig.SetDetails(const Value: Boolean);
begin
  if not FHasChanged then FHasChanged := FDetails <> Value;
  FDetails := Value;
end;

{ TfrmMain }
procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
  SL: TStrings;
begin
  SL := TStringList.Create;
  ZCon := TZconnection.Create(Self);
  ZCon.GetProtocolNames(SL);
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
  PerformanceConfig := TPerformanceConfig.create;
  cbPrintDetails.Checked := PerformanceConfig.FDetails;
end;

procedure TfrmMain.lbDriversSelectionChange(Sender: TObject; User: boolean);
var
  SDyn: TStringDynArray;
  Url: TZURL;
  I: Integer;
  Driver: IZDriver;
  CP: PZCodePage;
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
    eAlias.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Alias;
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
    cbAutoEncode.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).PreprepareSQL;
    cbRebuild.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Rebuild;
    cbCreateDB.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).CreateDatabase;
    eDelimiterType.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DelimiterType;
    eBuildScripts.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).BuildScripts;
    eDropScripts.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).DropScripts;
    mProperties.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Properties;
    mNotes.Text := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Notes;
    cbPooled.Checked := TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Pooled;

    Url := TZURL.Create;
    Url.Protocol :=  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Protocol;
    Driver := DriverManager.GetDriver(Url.URL);
    SDyn := Driver.GetSupportedClientCodePages(Url, cbAutoEncode.Checked, False);
    cbAnsiCP.Items.Clear;
    cbUnicodeCP.Items.Clear;
    for i := 0 to high(SDyn) do
    begin
      CP := Driver.GetPlainDriver(URL, False).ValidateCharEncoding(SDyn[i]);
      if CP^.Encoding = ceAnsi then
        cbAnsiCP.Items.Add(SDyn[i])
      else
        if CP^.IsSupported then
          cbUnicodeCP.Items.Add(SDyn[i]);
    end;
    Driver := nil;
    Url.Free;
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

  procedure AddGroup(var Ident: String; const Value: String; const Add, Pooled: Boolean);
  var
    FProtocol: String;
  begin
    FProtocol := StringReplace(Value, '=', '\', [rfReplaceAll]);
    if Add then
      if Ident <> '' then
        if Pooled then
          Ident := Ident+','+FProtocol+','+DATABASE_POOLED_KEY+'.'+FProtocol
        else
          Ident := Ident+','+FProtocol
      else
        if Pooled then
          Ident := FProtocol+','+DATABASE_POOLED_KEY+'.'+FProtocol
        else
          Ident := FProtocol;
  end;

begin
  SL := TStringList.Create;
  for i := 0 to lbDrivers.items.Count -1 do
  begin
    AddGroup(SCommon, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Common,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SCore, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Core,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SParseSQL, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).ParseSQL,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SPlain, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Plain,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SDbc, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Dbc,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SComponent, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Component,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SBugreport, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Bugreport,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    AddGroup(SPerformance, TPlainConfig(lbDrivers.items.Objects[I]).Protocol,
                      TPlainConfig(lbDrivers.items.Objects[I]).Performance,
                      TPlainConfig(lbDrivers.items.Objects[I]).Pooled);
    TPlainConfig(lbDrivers.items.Objects[I]).PostUpdates;
    if TPlainConfig(lbDrivers.items.Objects[I]).Pooled then
      TPlainConfig(lbDrivers.items.Objects[I]).PostUpdates(DATABASE_POOLED_KEY+'.');
  end;

  WriteProperty(COMMON_GROUP, ACTIVE_CONNECTIONS_KEY, SCommon);
  WriteProperty(CORE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SCore);
  WriteProperty(PARSESQL_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SParseSQL);
  WriteProperty(PLAIN_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SPlain);
  WriteProperty(DBC_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SDbc);
  WriteProperty(COMPONENT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SComponent);
  WriteProperty(BUGREPORT_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SBugreport);
  WriteProperty(PERFORMANCE_TEST_GROUP, ACTIVE_CONNECTIONS_KEY, SPerformance);
  PerformanceConfig.PostUpdates;
  SL.Free;
end;

procedure TfrmMain.mNotesEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Notes := mNotes.Text;
end;

procedure TfrmMain.mPropertiesEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Properties := mProperties.Text;
end;

procedure TfrmMain.rgOutputClick(Sender: TObject);
begin
  Self.PerformanceConfig.ResultOutput := rgOutput.Items[rgOutput.ItemIndex];
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
  if FChanged or PerformanceConfig.Changed then
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
  PerformanceConfig.Free;
  ZCon.Free;
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

procedure TfrmMain.cbPooledEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Pooled := cbPooled.Checked;
end;

procedure TfrmMain.cbCreateDBEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).CreateDatabase := cbCreateDB.Checked;
end;

procedure TfrmMain.cbAutoEncodeEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).PreprepareSQL := cbAutoEncode.Checked;
end;

procedure TfrmMain.cbPrintDetailsClick(Sender: TObject);
begin
  PerformanceConfig.Details := cbPrintDetails.Checked;
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

procedure TfrmMain.cgAPISItemClick(Sender: TObject; Index: integer);
begin
  case Index of
    0: PerformanceConfig.APIPlain := cgAPIS.Checked[Index];
    1: PerformanceConfig.APIdbc := cgAPIS.Checked[Index];
    2: PerformanceConfig.APIdbccached := cgAPIS.Checked[Index];
    3: PerformanceConfig.APIdataset := cgAPIS.Checked[Index];
    4: PerformanceConfig.APImidas := cgAPIS.Checked[Index];
    5: PerformanceConfig.APIoldzeos := cgAPIS.Checked[Index];
    6: PerformanceConfig.APIbde := cgAPIS.Checked[Index];
    7: PerformanceConfig.APIado := cgAPIS.Checked[Index];
    8: PerformanceConfig.APIdbx := cgAPIS.Checked[Index];
    9: PerformanceConfig.APIdbxc := cgAPIS.Checked[Index];
    10: PerformanceConfig.APIibx := cgAPIS.Checked[Index];
  end;
end;

procedure TfrmMain.cgBaseAPISItemClick(Sender: TObject; Index: integer);
begin
  case Index of
    0: PerformanceConfig.BaseAPIPlain := cgBaseAPIS.Checked[Index];
    1: PerformanceConfig.BaseAPIdbc := cgBaseAPIS.Checked[Index];
    2: PerformanceConfig.BaseAPIdbccached := cgBaseAPIS.Checked[Index];
    3: PerformanceConfig.BaseAPIdataset := cgBaseAPIS.Checked[Index];
  end;
end;

procedure TfrmMain.cgTestsItemClick(Sender: TObject; Index: integer);
begin
  case Index of
    0: PerformanceConfig.TestConnect := cgTests.Checked[Index];
    1: PerformanceConfig.TestInsert := cgTests.Checked[Index];
    2: PerformanceConfig.TestOpen := cgTests.Checked[Index];
    3: PerformanceConfig.TestFetch := cgTests.Checked[Index];
    4: PerformanceConfig.TestSort := cgTests.Checked[Index];
    5: PerformanceConfig.TestFilter := cgTests.Checked[Index];
    6: PerformanceConfig.TestUpdate := cgTests.Checked[Index];
    7: PerformanceConfig.TestDelete := cgTests.Checked[Index];
    8: PerformanceConfig.TestDirectUpdate := cgTests.Checked[Index];
  end;
end;

procedure TfrmMain.eAliasEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Alias := eAlias.Text;
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

procedure TfrmMain.ePortEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).Port := ePort.Text;
end;

procedure TfrmMain.eRecordsEditingDone(Sender: TObject);
begin
  PerformanceConfig.Records := StrToIntDef(eRecords.Text, 10000);
end;

procedure TfrmMain.eRepeatEditingDone(Sender: TObject);
begin
  PerformanceConfig.RepeatCount := StrToIntDef(eRepeat.Text, 3);
end;

procedure TfrmMain.eUserEditingDone(Sender: TObject);
begin
  TPlainConfig(lbDrivers.items.Objects[lbDrivers.ItemIndex]).UserName := eUser.Text;
end;

end.

