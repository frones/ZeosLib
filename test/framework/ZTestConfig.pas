{************************************************f*********}
{                                                         }
{                 Zeos Database Objects                   }
{           Configuration for Testing Framework           }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestConfig;

interface

{$I ZTestFramework.inc}

{$IFDEF VER130}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFDEF USE_MEMCHECK}
  MemCheck,
{$ENDIF}
{$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF},Classes, SysUtils, IniFiles, ZCompatibility;

const
  { Test configuration file }
  CONFIG_CMD_OPTION   = '-c';
  DEFAULT_CONFIG_DIR  = 'database';
  DEFAULT_LOG_DIR     = 'log';
  DEFAULT_CONFIG_FILE = 'test.properties';
{$IFDEF UNIX}
  PATH_DELIMITER      = '/';
{$ELSE}
  PATH_DELIMITER      = '\';
{$ENDIF}

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
  ENABLE_MEMCHECK_KEY          = 'enable.memcheck';
  MEMCHECK_LOGFILE_KEY         = 'memcheck.logfile';
  MEMCHECK_SHOWRESULT_KEY      = 'memcheck.showresult';
  DECIMAL_SEPARATOR_KEY        = 'decimal.separator';
  SUPPRESS_TEST_OUTPUT_KEY     = 'suppress.output';
  ENABLE_KEY                   = 'enable';
  SKIP_CLOSED_KEY              = 'skip.closed';
  SKIP_NON_ZEOS_ISSUES_KEY     = 'skip.non_zeos_issues';
  SKIP_REAL_PREPARED_KEY       = 'skip.real_prepared';
  SKIP_PERFORMANCE_KEY         = 'skip.performance';
  SKIP_PERFORMANCE_TRANS_KEY   = 'skip.performance.transaction.mode';
  PERFORMANCE_TABLE_NAME_KEY   = 'performance.table.name';
  PERFORMANCE_PRIMARYKEY_KEY   = 'performance.primary.key.name';
  PERFORMANCE_LOADLOBS_KEY     = 'performance.loadlobs';
  ACTIVE_CONNECTIONS_KEY       = 'connections';
  EXTENDED_TEST_KEY            = 'extended.test';
  EXTENDED_CGET_ACP_KEY        = 'extended.cget_acp';
  EXTENDED_CCP_UTF8_KEY        = 'extended.ccp_utf8';
  EXTENDED_CCP_UTF16_KEY       = 'extended.ccp_utf16';
  EXTENDED_CODEPAGES_KEY       = 'extended.codepages';
  EXTENDED_AUTOENCODING_KEY    = 'extended.autoencoding';

const
  { Names of the connection configuration keys. }
  DATABASE_LIBLOCATION_KEY    = 'liblocation';
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
  DATABASE_CHARACTERSETS_KEY  = 'charactersets';

const
  { SQL script delimiters }
  DEFAULT_DELIMITER    = 'default';
  SET_TERM_DELIMITER   = 'setterm';
  DELIMITER_DELIMITER  = 'delimiter';
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

type

  {** Implements a wrapper for test configuration file. }
  TZTestConfiguration = class
  private
    FConfigFile: TIniFile;
    FConfigLoaded: Boolean;
    FConfigFileName: string;
    FEnableMemCheck: Boolean;
    FMemCheckLogFile: string;
    FMemCheckShowResult: Boolean;
    FScriptPath: string;

    function GetConfigFileName: string;
  public
    destructor Destroy; override;

    procedure LoadConfig;
    function ReadProperty(const Group, Key, Default: string): string;

    procedure ActivateMemCheck;
    procedure DeactivateMemCheck;

    property ConfigFile: TIniFile read FConfigFile;
    property ConfigFileName: string read FConfigFileName;
    property ConfigLoaded: Boolean read FConfigLoaded;
    property ScriptPath: String read FScriptPath;

    property EnableMemCheck: Boolean read FEnableMemCheck;
    property MemCheckLogFile: string read FMemCheckLogFile;
    property MemCheckShowResult: Boolean read FMemCheckShowResult;
  end;

type
  TCommandLineSwitches = record
    help:            boolean;
    list:            boolean;
    verbose:         boolean;
    runall:          boolean;
    batch:           boolean;
    xml:             boolean;
    xmlfilename:     string;
    norebuild:       boolean;
    suite:           boolean;
    memcheck:        boolean;
    memcheck_file:   String;
    suiteitems:      TStringDynArray;
    suitename:       String;
    sqlmonitor:      boolean;
    sqlmonitorfile:  String;
  end;
var
  CommandLineSwitches: TCommandLineSwitches;


{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringToArray(Value: string; Delimiters: string): TStringDynArray;

var
  {** The default test configuration instance. }
  TestConfig: TZTestConfiguration;

  {** The active test group. }
  TestGroup: string;

  {$IFNDEF FPC}
  function CreateTestSuite:ITestSuite;
  {$ENDIF}
procedure EnableZSQLMonitor;

implementation

uses ZSysUtils, ZFastCode,
  {$IFDEF WITH_VCL_PREFIX}Vcl.Forms{$ELSE}Forms{$ENDIF}
  {$IFDEF FPC}, testregistry{$ENDIF}, ZSqlMonitor, ZDbcLogging;
type

  { TZTestLoggingFormatter }

  TZTestLoggingFormatter = class(TInterfacedObject, IZLoggingFormatter)
  private
    FCounter: Integer;
  public
    function Format(LoggingEvent: TZLoggingEvent) : RawByteString;
  end;


var
  SQLMonitor : TZSQLMonitor;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringToArray(Value: string; Delimiters: string): TStringDynArray;
var
  I: Integer;
  Temp: TStrings;
begin
  Temp := SplitString(Value, Delimiters);
  try
    SetLength(Result, Temp.Count);
    for I := 0 to Temp.Count - 1 do
      Result[I] := Temp[I];
  finally
    Temp.Free;
  end;
end;

{ TZTestLoggingFormatter }

function TZTestLoggingFormatter.Format(LoggingEvent: TZLoggingEvent): RawByteString;
begin
  Inc(FCounter);
  Result := RawByteString(SysUtils.Format('[%10d]', [FCounter])) + ' cat: ';
  case LoggingEvent.Category of
    lcConnect: Result := Result + 'Connect';
    lcDisconnect: Result := Result + 'Disconnect';
    lcTransaction: Result := Result + 'Transaction';
    lcExecute: Result := Result + 'Execute';
    lcPrepStmt: Result := Result + 'Prepare';
    lcBindPrepStmt: Result := Result + 'Bind prepared';
    lcExecPrepStmt: Result := Result + 'Execute prepared';
    lcUnprepStmt: Result := Result + 'Unprepare prepared';
  else
    Result := Result + 'Other';
  end;
  if LoggingEvent.Protocol <> '' then
    Result := Result + ', proto: ' + LoggingEvent.Protocol;
  Result := Result + ', msg: ' + LoggingEvent.Message;
  if (LoggingEvent.ErrorCode <> 0) or (LoggingEvent.Error <> '') then
  begin
    Result := Result + ', errcode: ' + IntToRaw(LoggingEvent.ErrorCode)
      + ', error: ' + LoggingEvent.Error;
  end;
end;

{ TZTestConfiguration }

{**
  Destroys this test configuration and cleanups the memory.
}
destructor TZTestConfiguration.Destroy;
begin
  if Assigned(FConfigFile) then
    ConfigFile.Free;

  inherited Destroy;
end;

{**
  Gets a fully qualified name of the configuration file.
  @return a fully qualified name of the configuration file.
}
function TZTestConfiguration.GetConfigFileName: string;
var
  I: Integer;
  Path: string;
begin
  Result := '';

  { Searches for config file within command line parameters. }
  { Config file must be defined as '-c <file path>' }
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = CONFIG_CMD_OPTION)
      and (I < ParamCount) then
    begin
      Result := ParamStr(I + 1);
      Break;
    end;
  end;

  { Searches for default configuration file. }
  if Result = '' then
  begin
    Path := '.' + PATH_DELIMITER + DEFAULT_CONFIG_DIR
      + PATH_DELIMITER + DEFAULT_CONFIG_FILE;
    for I := 1 to 4 do
    begin
      if FileExists(Path) then
      begin
        Result := Path;
        Break;
      end;
      Path := '.' + PATH_DELIMITER + '.' + Path;
    end;
  end;

  { If config file still is not defined set it by default. }
  if Result = '' then
    Result := ExtractFileDir(ParamStr(0)) + PATH_DELIMITER + Path;

  FConfigFileName := Result;
  Writeln('Config File Name: ' + Result);
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZTestConfiguration.LoadConfig;
var
  ConfigFileName: String;
  ScriptPath: String;
begin
  { Reads a configuration file from the disk. }
  if Assigned(FConfigFile) then
    FConfigFile.Free;
  ConfigFileName := GetConfigFileName;
  if not FileExists(ConfigFileName)
  then raise Exception.Create('Config file doesn''t exist!');

  ConfigFileName := ExpandFileName(ConfigFileName);
  FConfigFile := TIniFile.Create(ConfigFileName);
  ScriptPath := FConfigFile.ReadString('common', 'common.scriptpath', '');

  if ScriptPath <> '' then begin
    if DirectoryExists(ScriptPath)
    then FScriptPath := ExpandFileName(ScriptPath)
    else if DirectoryExists(ExtractFilePath(ParamStr(0)) + ScriptPath)
      then FScriptPath := ExtractFilePath(ParamStr(0)) + ScriptPath
      else if DirectoryExists(ExtractFilePath(ConfigFileName) + ScriptPath)
        then FScriptPath := ExtractFilePath(ConfigFileName) + ScriptPath
        else FScriptPath := ExtractFilePath(FConfigFileName);
  end else begin
    FScriptPath := ExtractFilePath(FConfigFileName);
  end;

  if FScriptPath <> ''
    then if FScriptPath[Length(FScriptPath)] <> PathDelim
      then FScriptPath := FScriptPath + PathDelim;

  { Reads default properties. }
  FEnableMemCheck := StrToBoolEx(ReadProperty(COMMON_GROUP,
    ENABLE_MEMCHECK_KEY, FALSE_VALUE));
  if FEnableMemCheck and not CommandLineSwitches.memcheck then
    CommandLineSwitches.memcheck := True;
  FMemCheckLogFile := ReadProperty(COMMON_GROUP,
    MEMCHECK_LOGFILE_KEY, '');
  if CommandLineSwitches.memcheck and (CommandLineSwitches.memcheck_file = '') then
    CommandLineSwitches.memcheck_file := FMemCheckLogFile;
  FMemCheckShowResult := StrToBoolEx(ReadProperty(COMMON_GROUP,
    MEMCHECK_SHOWRESULT_KEY, FALSE_VALUE));
end;

{**
  Reads a configuration property from loaded config file.
  @param Group a name of group of properties. Serves as a section name
    within the INI-file and format real key as <group>.<key>
  @param Key a property key.
  @param Default a default property value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZTestConfiguration.ReadProperty(
  const Group, Key, Default: string): string;
begin
  if Assigned(FConfigFile) then
    Result := Trim(FConfigFile.ReadString(Group, Group + '.' + Key, Default))
  else Result := '';
end;

{**
  Activates checking for memory leaks using MemCheck.
}
procedure TZTestConfiguration.ActivateMemCheck;
begin
{$IFDEF USE_MEMCHECK}
  if FEnableMemCheck then
  begin
    MemCheck.ShowLogFileWhenUseful := FMemCheckShowResult;
    MemCheck.MemCheckLogFileName := FMemCheckLogFile;
    MemCheck.MemChk;
  end;
{$ENDIF}
end;

{**
  Deactivates checking for memory leaks.
}
procedure TZTestConfiguration.DeactivateMemCheck;
begin
{$IFDEF USE_MEMCHECK}
  MemCheck.UnMemChk;
{$ENDIF}
end;

{**
  Dumb function which makes it possible to find the value passed to a command line switch in Delphi.
  Please replace this by a better solution when available.
  eg when the test program is started using commandline
    ztestall.exe -n -suite "core;parsesql" -b
  this function should return 'core;parsesql'
}
{$IFNDEF FPC}
function GetCommandLineSwitchValue(ShortSwitch: String; LongSwitch: String; IgnoreCasse: Boolean = true):String;
var
  i: integer;
  expectedShortSwitch, expectedLongSwitch, currentSwitch: String;
begin
  Result := '';
  if IgnoreCasse then begin
    expectedLongSwitch := LowerCase(Longswitch);
    expectedShortSwitch := LowerCase(ShortSwitch);
  end else begin
    expectedLongSwitch := Longswitch;
    expectedShortSwitch := ShortSwitch;
  end;

  for i := 1 to ParamCount do // Don't check the last one, as we can't return the next param then!!
  begin
    if IgnoreCasse then currentSwitch := LowerCase(ParamStr(i)) else currentSwitch := ParamStr(i);
    
    If (currentSwitch[1]='-') then
      if ((CompareText(currentSwitch,'-'+expectedShortSwitch) = 0) or
         (CompareText(currentSwitch,'-'+expectedLongSwitch) = 0)) and
         (i < ParamCount) then
        Result := ParamStr(i+1);
    If (currentSwitch[1]='/') then
      if ((CompareText(currentSwitch,'/'+expectedShortSwitch) = 0) or
         (CompareText(currentSwitch,'/'+expectedLongSwitch) = 0)) and
         (i < ParamCount) then
        Result := ParamStr(i+1);
    if StartsWith(currentSwitch, '--'+expectedLongSwitch+'=') then
      Result := Copy(ParamStr(i), Length('--'+expectedLongSwitch+'=') + 1, Length(ParamStr(i)));
  end;
end;
{$ENDIF}

function FindCmdLineSwitch(Switch: String; IgnoreCase: Boolean): boolean;
var
  expectedSwitch, currentSwitch: String;
  x: Integer;
begin
  Result := false;
  if Length(Switch) > 0 then begin
    if IgnoreCase then expectedSwitch := LowerCase(Switch) else expectedSwitch := Switch;

    for x := 1 to ParamCount do begin
      if IgnoreCase then currentSwitch := LowerCase(ParamStr(x)) else currentSwitch := ParamStr(x);
      if (currentSwitch = '--' + expectedSwitch) or
        StartsWith(currentSwitch, '--' + expectedSwitch + '=')
      then begin
        Result := true;
        break;
      end;
    end;
  end;

  if not Result then Result := SysUtils.FindCmdLineSwitch(Switch, IgnoreCase);
end;

{**
  Convert Command Line Switches to aa compiler independent global record
}
procedure GetCommandLineSwitches;
begin
  {$IFDEF FPC}
  CommandLineSwitches.help := Application.HasOption('h', 'help');
  CommandLineSwitches.list := Application.HasOption('l', 'list');
  CommandLineSwitches.verbose := Application.HasOption('v', 'verbose');
  CommandLineSwitches.runall := Application.HasOption('a', 'all');
  CommandLineSwitches.batch := Application.HasOption('b', 'batch');
  CommandLineSwitches.xml := Application.HasOption('x', 'xml');
  if CommandLineSwitches.xml then
    CommandLineSwitches.XmlFileName := Application.GetOptionValue('x', 'xml');
  CommandLineSwitches.norebuild := Application.HasOption('n', 'norebuild');
  CommandLineSwitches.memcheck := Application.HasOption('memcheck');
  if CommandLineSwitches.memcheck then
    CommandLineSwitches.memcheck_file := Application.GetOptionValue('memcheck')
  else
    CommandLineSwitches.memcheck_file := '';
  CommandLineSwitches.suite := Application.HasOption('s', 'suite');
  If CommandLineSwitches.suite then
    CommandLineSwitches.suiteitems := SplitStringToArray(Application.GetOptionValue('suite'),LIST_DELIMITERS);
  CommandLineSwitches.sqlmonitor := Application.HasOption('m','monitor');
  If CommandLineSwitches.sqlmonitor then
    CommandLineSwitches.sqlmonitorfile := Application.GetOptionValue('m', 'monitor');
  if Application.HasOption('suitename') then
    CommandLineSwitches.suitename := Application.GetOptionValue('suitename');
  {$ELSE}
  CommandLineSwitches.help := (FindCmdLineSwitch('H',true) or FindCmdLineSwitch('Help',true));
  CommandLineSwitches.list := (FindCmdLineSwitch('L',true) or FindCmdLineSwitch('List',true));
  CommandLineSwitches.verbose := (FindCmdLineSwitch('V',true) or FindCmdLineSwitch('Verbose',true));
  CommandLineSwitches.runall := (FindCmdLineSwitch('A',true) or FindCmdLineSwitch('All',true));
  CommandLineSwitches.batch := (FindCmdLineSwitch('B',true) or FindCmdLineSwitch('Batch',true));
  CommandLineSwitches.xml := (FindCmdLineSwitch('X',true) or FindCmdLineSwitch('XML',true));
  if CommandLineSwitches.xml then
    CommandLineSwitches.xmlfilename := GetCommandLineSwitchValue('X' ,'XML');
  CommandLineSwitches.norebuild := (FindCmdLineSwitch('N',true) or FindCmdLineSwitch('NoRebuild',true));
  CommandLineSwitches.memcheck := FindCmdLineSwitch('MemCheck',true);
  if CommandLineSwitches.memcheck then
    CommandLineSwitches.memcheck_file := GetCommandLineSwitchValue('MemCheck', 'memcheck')
  else
    CommandLineSwitches.memcheck_file := '';
  CommandLineSwitches.suite := (FindCmdLineSwitch('S',true) or FindCmdLineSwitch('Suite',true));
  If CommandLineSwitches.suite then
    CommandLineSwitches.suiteitems := SplitStringToArray(GetCommandLineSwitchValue('S' ,'Suite'),LIST_DELIMITERS);
  CommandLineSwitches.sqlmonitor := (FindCmdLineSwitch('M',true) or FindCmdLineSwitch('Monitor',true));
  If CommandLineSwitches.sqlmonitor then
    CommandLineSwitches.sqlmonitorfile := GetCommandLineSwitchValue('M' ,'Monitor');
  if FindCmdLineSwitch('SuiteName',true) then
    CommandLineSwitches.suitename := GetCommandLineSwitchValue('' ,'SuiteName');
  {$ENDIF}
end;

{**
  Build a custom test suite from all registered tests, based on command line switches and the
  configuration file
  Unfortunately fpcunit and DUnit need different approaches
}
{$IFNDEF FPC}
function CreateTestSuite:ITestSuite;
var
  I, J: integer;
  RealSuiteName: String;

  procedure CheckTestRegistry (test:ITest; ATestName:string);
  var s, c : string;
      I, p : integer;
  begin
    if Supports(test, ITestSuite) then
      begin
      p := System.pos('.', ATestName);
      if p > 0 then
        begin
        s := copy (ATestName, 1, p-1);
        c := copy (ATestName, p+1, maxint);
        end
      else
        begin
        s := '';
        c := ATestName;
        end;
      if comparetext(c, test.Name) = 0 then
        begin
          Result.AddTest(test);
        end
      else if (CompareText( s, Test.Name) = 0) or (s = '') then
        for I := 0 to test.Tests.Count - 1 do
          CheckTestRegistry (ITest(test.Tests[I]), c)
      end
    else // if test is TTestCase then
      begin
      if comparetext(test.Name, ATestName) = 0 then
        begin
          Result.AddTest(test);
        end;
      end;
  end;
begin
  If CommandLineSwitches.Suite then
    begin
      if CommandLineSwitches.suitename = '' then RealSuiteName := 'Suite' else RealSuiteName := CommandLineSwitches.suitename;
      Result := TTestSuite.Create(RealSuiteName);
      for J := 0 to High(CommandLineSwitches.suiteitems) do
        for I := 0 to RegisteredTests.Tests.count-1 do
          CheckTestRegistry (ITest(RegisteredTests.Tests[I]), CommandLineSwitches.suiteitems[J]);
      RegisteredTests.Tests.Clear;
    end
  else
    Result := RegisteredTests;
end;
{$ENDIF}

procedure EnableZSQLMonitor;
begin
  SQLMonitor := TZSQLMonitor.Create(Application);
  SQLMonitor.FileName := CommandLineSwitches.sqlmonitorfile;
  SQLMonitor.Active := True;
  SQLMonitor.AutoSave := True;
  SQLMonitor.LoggingFormatter := TZTestLoggingFormatter.Create;
end;

initialization
  SQLMonitor := nil;
  GetCommandLineSwitches;

  TestGroup := COMMON_GROUP;

  TestConfig := TZTestConfiguration.Create;
  TestConfig.LoadConfig;
  TestConfig.ActivateMemCheck;
finalization
  if Assigned(TestConfig) then
    TestConfig.Free;
  if Assigned(SQLMonitor) then
    begin
     {$IFNDEF FPC}
     SQLMonitor.Free;
     {$ENDIF}
    end;
end.

