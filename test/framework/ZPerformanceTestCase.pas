{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Abstract Performance Test Cases             }
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

unit ZPerformanceTestCase;

interface

{$I ZTestFramework.inc}

uses Classes, Contnrs, Types, DB, SysUtils,
  ZCompatibility, ZTestCase, ZDataset, ZSqlTestCase, ZConnection, ZDbcIntfs;

type
  {** A method for test set up, run or tear down. }
  TZTestMethod = procedure of object;

  {** Implements a abstract performance test case. }

  { TZPerformanceSQLTestCase }

  TZPerformanceSQLTestCase = class (TZAbstractCompSQLTestCase)
  private
    FSelectedAPIs: TStrings;
    FSelectedTests: TStrings;
    FRecordCount: Integer;
    FRepeatCount: Cardinal;
    FLoadLobs: Boolean;
    FSkipFlag: Boolean;
    FSkipPerformanceTransactionMode: Boolean;
    FPerformanceTable: String;
    FPerformancePrimaryKey: String;

    procedure RunSelectedTest(TestName: string; SetUpMethod: TZTestMethod;
      RunTestMethod: TZTestMethod; TearDownMethod: TZTestMethod);
    procedure DetermineFieldsProperties(ConnectionConfig: TZConnectionConfig);
  protected
    FAsciiStream, FUnicodeStream, FBinaryStream: TStream;
    FDirectSQLTypes: TResultSetTypesDynArray;
    FDirectFieldTypes: TDataSetTypesDynArray;
    FDirectFieldNames: TStringDynArray;
    FDirectFieldSizes: TIntegerDynArray;
    FTrueVal, FFalseVal: String;

    procedure LoadConfiguration; override;
    procedure SetUp; override;
    function GetImplementedAPI: string; virtual; abstract;

    procedure Print(const Msg: string); override;
    procedure PrintLn(const Msg: string); override;

    { Informational methods. }
    function GetRecordCount: Integer;
    procedure SkipTest;

    { Tests table preparation methods. }
    procedure PopulateTable(TableName: string; PrimaryKey: string;
      RecordCount: Integer; ForeignKey: string; ForeignKeyRange: Integer);
    procedure CleanupTable(TableName: string);

    { Implementation of different tests. }
    procedure DefaultSetUpTest; virtual;
    procedure DefaultTearDownTest; virtual;

    procedure SetUpTestConnect; virtual;
    procedure RunTestConnect; virtual;
    procedure TearDownTestConnect; virtual;

    procedure SetUpTestInsert; virtual;
    procedure RunTestInsert; virtual;
    procedure TearDownTestInsert; virtual;

    procedure SetUpTestOpen; virtual;
    procedure RunTestOpen; virtual;
    procedure TearDownTestOpen; virtual;

    procedure SetUpTestFetch; virtual;
    procedure RunTestFetch; virtual;
    procedure TearDownTestFetch; virtual;

    procedure SetUpTestSort; virtual;
    procedure RunTestSort; virtual;
    procedure TearDownTestSort; virtual;

    procedure SetUpTestFilter; virtual;
    procedure RunTestFilter; virtual;
    procedure TearDownTestFilter; virtual;

    procedure SetUpTestUpdate; virtual;
    procedure RunTestUpdate; virtual;
    procedure TearDownTestUpdate; virtual;

    procedure SetUpTestDelete; virtual;
    procedure RunTestDelete; virtual;
    procedure TearDownTestDelete; virtual;

    procedure SetUpTestDirectUpdate; virtual;
    procedure RunTestDirectUpdate; virtual;
    procedure TearDownTestDirectUpdate; virtual;

    procedure SetUpTestLocate; virtual;
    procedure RunTestLocate; virtual;
    procedure TearDownTestLocate; virtual;

    procedure SetUpTestLookup; virtual;
    procedure RunTestLookup; virtual;
    procedure TearDownTestLookup; virtual;

    property SkipPerformanceTransactionMode: Boolean read FSkipPerformanceTransactionMode;
    property PerformanceTable: String read FPerformanceTable;
    property PerformancePrimaryKey: String read FPerformancePrimaryKey;
    property LoadLobs: Boolean read FLoadLobs;
  public
    function CreateDatasetConnection: TZConnection; override;
    function CreateDbcConnection: IZConnection; override;

    destructor Destroy; override;

  published
    { DUnit test methods. }
    { keep this execution order to save setup time !! }
    procedure TestConnect;
    procedure TestInsert;
    procedure TestUpdate;
    procedure TestFetch;
    procedure TestOpen;
    procedure TestSort;
    procedure TestFilter;
    procedure TestLocate;
    procedure TestLookup;
    procedure TestDelete;
    procedure TestDirectUpdate;
  end;

  {** Defines a container for performance test results. }

  { TZPerformanceResultItem }

  TZPerformanceResultItem = class
  private
    FConnectionName: String;
    FProtocol: String;
    FAPIName: string;
    FTestName: string;
    FTryIndex: Integer;
    FMetric: Double;
  public
    constructor Create(const APIName, TestName, Protocol, ConnectionName: string;
      const TryIndex: Integer; const Metric: Double);
    procedure Normalize(BaseMetric: Double);

    property APIName: string read FAPIName write FAPIName;
    property TestName: string read FTestName write FTestName;
    property TryIndex: Integer read FTryIndex write FTryIndex;
    property Metric: Double read FMetric write FMetric;
    property Protocol: String read FProtocol write FProtocol;
    property ConnectionName: String read FConnectionName write FConnectionName;
  end;

  {** Implements a performance result processor. }

  { TZPerformanceResultProcessor }

  TZPerformanceResultProcessor = class
  private
    FResults: TObjectList;
    FSelectedAPIs: TStrings;
    FSelectedTests: TStrings;
    FProtocols: TStrings;
    FRecordCount: Cardinal;
    FRepeatCount: Cardinal;
    FBaseAPIName: string;
    FDetails: Boolean;
    FOutputType: string;
    FSkipPerformance: Boolean;
    Used: Boolean;
  protected
    property Results: TObjectList read FResults write FResults;

    procedure LoadConfiguration;
    function FindResultItem(APIName, TestName: string;
      TryIndex: Integer): TZPerformanceResultItem; overload;
    function FindResultItem(const APIName, TestName, Protocol, ConnectionName: string;
      const TryIndex: Integer): TZPerformanceResultItem; overload;
    procedure CalculateAverages;
    procedure NormalizeResults;

    procedure PrintPlainResults;
    procedure PrintCSVResults;
    procedure PrintHTMLResults;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterResult(const APIName, TestName, Protocol,
      ConnectionName: String; const TryIndex: Integer; const Metric: Double);
    procedure ProcessResults;
    procedure PrintResults;
    procedure ClearResults;

    property SelectedAPIs: TStrings read FSelectedAPIs;
    property SelectedTests: TStrings read FSelectedTests;
    property SelectedProtocols: TStrings read FProtocols;
    property RepeatCount: Cardinal read FRepeatCount write FRepeatCount;
    property RecordCount: Cardinal read FRecordCount write FRecordCount;
    property BaseAPIName: string read FBaseAPIName write FBaseAPIName;
    property Details: Boolean read FDetails write FDetails;
    property OutputType: string read FOutputType write FOutputType;
  end;

var
  PerformanceResultProcessor: TZPerformanceResultProcessor;

implementation

uses
  Math,
  ZSysUtils, ZTestConfig, ZTestConsts, ZDatasetUtils, ZClasses;

{ TZPerformanceSQLTestCase }

{**
  Destroys this object and clean ups the memory.
}
destructor TZPerformanceSQLTestCase.Destroy;
begin
  if Assigned(FSelectedAPIs) then
    FSelectedAPIs.Free;
  if Assigned(FSelectedTests) then
    FSelectedTests.Free;

  inherited Destroy;
end;

{**
  Gets a specified record count for test tables.
  @return a specified record count.
}
function TZPerformanceSQLTestCase.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZPerformanceSQLTestCase.LoadConfiguration;
begin
  inherited LoadConfiguration;

  { Defines a selected APIs }
  if Assigned(FSelectedAPIs) then
    FSelectedAPIs.Free;
  FSelectedAPIs := SplitString(ReadGroupProperty('apis', ''),
    LIST_DELIMITERS);

  { Defines a selected tests }
  if Assigned(FSelectedTests) then
    FSelectedTests.Free;
  FSelectedTests := SplitString(ReadGroupProperty('tests', ''),
    LIST_DELIMITERS);

  { Reads other configuration parameters. }
  FRecordCount := StrToIntDef(ReadGroupProperty('records', ''), 1000);
  FRepeatCount := StrToIntDef(ReadGroupProperty('repeat', ''), 1);
  FSkipPerformanceTransactionMode := StrToBoolEx(ReadInheritProperty(SKIP_PERFORMANCE_TRANS_KEY, FALSE_VALUE));
  FPerformanceTable := ReadInheritProperty(PERFORMANCE_TABLE_NAME_KEY, PERFORMANCE_TABLE_NAME);
  FPerformancePrimaryKey := ReadInheritProperty(PERFORMANCE_PRIMARYKEY_KEY, PERFORMANCE_PRIMARY_KEY);
  FLoadLobs := StrToBoolEx(ReadInheritProperty(PERFORMANCE_LOADLOBS_KEY, TRUE_VALUE));
end;

procedure TZPerformanceSQLTestCase.SetUp;
begin
  inherited SetUp;
  if not (ConnectionConfig.PerformanceFieldPropertiesDetermined) then
    DetermineFieldsProperties(ConnectionConfig);
end;

{**
  Print a string message.
  @param Message a message string.
}
procedure TZPerformanceSQLTestCase.Print(const Msg: string);
begin
//  Status(_Message);
  System.Write(Msg);
end;

{**
  Print a string message on a new line.
  @param Message a message string.
}
procedure TZPerformanceSQLTestCase.PrintLn(const Msg: string);
begin
//  Status(_Message)
  System.Writeln(Msg);
end;

{**
  Removes all existed rows in the specified table.
  @param TableName a name of the table.
}
procedure TZPerformanceSQLTestCase.CleanupTable(TableName: string);
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format('DELETE FROM %s', [TableName]);
    Query.ExecSQL;
    if not SkipPerformanceTransactionMode then Connection.Commit;
  finally
    Query.Free;
  end;
end;

{**
  Populates the data to the specified table.
  @param TableName a name of the table.
  @param PrimaryKey a name of the table primary key
  @param RecordCount a number of records to populate
  @param ForeignKey a name of the foreign key for master-detail relations
    (empty string means no foreign key)
  @param ForeignKeyRange the range of values for the foreign keys - [0..x].
}
procedure TZPerformanceSQLTestCase.PopulateTable(TableName: string;
  PrimaryKey: string; RecordCount: Integer;
  ForeignKey: string; ForeignKeyRange: Integer);
var
  I, Index, Count, CommitCount: Integer;
  Query, Query1: TZQuery;
  CurrentCount: Integer;
  TransactIsolationLevel: TZTransactIsolationLevel;
  AutoCommit: Boolean;
begin
  TransactIsolationLevel := Connection.TransactIsolationLevel;
  AutoCommit := Connection.AutoCommit;
  Connection.AutoCommit := False;
  Connection.TransactIsolationLevel := tiReadCommitted;
  Query := CreateQuery;
  Query1 := CreateQuery;
  try
    Query.ReadOnly := True;

    Query1.Connection := Connection;

    if StartsWith(Protocol, 'sqlite') then
    begin
      Query1.SQL.Text := Format('SELECT COUNT(*), %s FROM %s', [PrimaryKey, TableName]);
      Query1.Open;
      Query1.Last;
      CurrentCount := Query1.Fields[0].AsInteger;
      Index := Query1.Fields[1].AsInteger;
      Query1.Close;
    end
    else
    begin
      Query1.SQL.Text := Format('SELECT COUNT(*), MAX(%s) FROM %s', [PrimaryKey, TableName]);
      Query1.Open;
      CurrentCount := Query1.Fields[0].AsInteger;
      Index := Query1.Fields[1].AsInteger;
      Query1.Close;
    end;

    if RecordCount = CurrentCount then
      Exit;

    Query.SQL.Text := Format('SELECT * FROM %s ORDER BY %s',
      [TableName, PrimaryKey]);
    Query.Open;

    CommitCount := 0;
    if CurrentCount > RecordCount then
    begin
      Count := CurrentCount - RecordCount;
      Query.Last;
      while not Query.Bof and (Count > 0) do
      begin
        Index := Query.FieldByName(PrimaryKey).AsInteger;
        Query1.SQL.Text := Format('DELETE FROM %s WHERE %s=%d', [TableName, PrimaryKey, Index]);
        Query1.ExecSQL;
        Dec(Count);
        Query.Prior;
      end;
    end
    else
    begin
      Count := RecordCount - CurrentCount;
      Query.First;
      Query1.SQL.Text := Format('SELECT * FROM %s WHERE %s > %d', [TableName, PrimaryKey, Index]);
      Query1.Open;
      while Count > 0 do
      begin
        Inc(Index);
        Query1.Insert;
        for I := 0 to High(ConnectionConfig.PerformanceDataSetTypes) do
        begin
          case ConnectionConfig.PerformanceDataSetTypes[i] of
            ftString, ftFixedChar:
              Query1.Fields[i].AsString := RandomStr(Query1.Fields[i].DisplayWidth);
            ftMemo, ftFmtMemo:
              Query1.Fields[i].AsString := RandomStr(RecordCount*100);
            {$IFDEF WITH_WIDEFIELDS}
            ftWideString{$IFNDEF FPC}, ftFixedWideChar{$ENDIF}:
              Query1.Fields[i].AsWideString := WideString(RandomStr(Query1.Fields[i].DisplayWidth));
            ftWideMemo:
              Query1.Fields[i].AsWideString := WideString(RandomStr(RecordCount*100));
            {$ENDIF}
            ftSmallint:
              Query1.Fields[i].AsInteger := Random(255);
            ftInteger, ftWord, ftLargeint:
              if UpperCase(PrimaryKey) = UpperCase(Query.Fields[I].FieldName) then
                Query1.Fields[i].AsInteger := Index
              else if UpperCase(ForeignKey) = UpperCase(Query.Fields[I].FieldName) then
                Query1.Fields[i].AsInteger := RandomInt(1, ForeignKeyRange)
              else
                Query1.Fields[i].AsInteger := RandomInt(-100, 100);
            ftBoolean:
              Query1.Fields[i].AsBoolean := Random(1) = 0;
            ftBCD, ftFMTBcd, ftFloat, ftCurrency{$IFDEF WITH_FTEXTENDED}, ftExtended{$ENDIF}:
              Query1.Fields[i].AsFloat := RandomFloat(-100, 100);
            ftDate, ftTime, ftDateTime, ftTimeStamp:
              Query1.Fields[i].AsFloat := now;
            ftVarBytes, ftBytes:
              Query1.Fields[i].Value := RandomBts(Query1.Fields[i].Size);
            ftBlob:
              Query1.Fields[i].Value := RandomBts(RecordCount*100);
            {$IFDEF WITH_FTGUID}
            ftGuid:
              Query1.Fields[i].AsString := RandomGUIDString;
            {$ENDIF}
            {ftAutoInc, ftGraphic,
            ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
            ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
            ftVariant, ftInterface, ftIDispatch);}
          end;
        end;
        Query1.Post;
        Inc(CommitCount);
        if CommitCount mod 1000 = 0 then
          Connection.Commit;
        Dec(Count);
      end;
    end;
    if not (CommitCount mod 1000 = 0) then
      Connection.Commit;
    Query.Close;
    Query1.Close;
  finally
    Connection.TransactIsolationLevel := TransactIsolationLevel;
    Connection.AutoCommit := AutoCommit;
    Connection.Disconnect;
    Query.Free;
    Query1.Free;
  end;
end;

{**
  Runs a selected test.
  @param TestName a name of the test to be runned.
  @param SetUpMethod the method to initialize the test.
  @param RunTestMethod the method to run the test.
  @param TearDownMethod the method to deinitialize the test.
}
procedure TZPerformanceSQLTestCase.RunSelectedTest(TestName: string;
  SetUpMethod: TZTestMethod; RunTestMethod: TZTestMethod;
  TearDownMethod: TZTestMethod);
var
  I: Integer;
  StartTicks: Cardinal;
  StopTicks: Cardinal;
begin
  BlankCheck;

  { Filter tests by selected API and test name. }
  if FSelectedAPIs.IndexOf(GetImplementedAPI) < 0 then
    Exit;
  if FSelectedTests.IndexOf(TestName) < 0 then
    Exit;

  FSkipFlag := False;

  for I := 1 to FRepeatCount do
  begin
    { Initializes the test. }
    SetUpMethod;

    { Runs the test. }
    try
      StartTicks := GetTickCount;
      RunTestMethod;
      StopTicks := GetTickCount;
    finally
    { Deinitializes the test. }
      TearDownMethod;
    end;

    { Registers a performance test result. }
    if not FSkipFlag then
      PerformanceResultProcessor.RegisterResult( GetImplementedAPI, TestName,
        Protocol, ConnectionName, I, StopTicks - StartTicks)
    else
      Exit;
  end;
end;

procedure TZPerformanceSQLTestCase.DetermineFieldsProperties(ConnectionConfig: TZConnectionConfig);
var
  Query: TZQuery;
  I: Integer;
  DataSetTypes: TDataSetTypesDynArray;
  ResultSetTypes: TResultSetTypesDynArray;
  FieldSizes: TIntegerDynArray;
  FieldNames: TStringDynArray;
begin
  Query := CreateQuery;
  Query.SQL.Text := 'select * from '+PerformanceTable;
  Query.Open;
  SetLength(DataSetTypes, Query.Fields.Count);
  SetLength(ResultSetTypes, Query.Fields.Count);
  SetLength(FieldSizes, Query.Fields.Count);
  SetLength(FieldNames, Query.Fields.Count);
  for i := 0 to Query.Fields.Count -1 do
  begin
    if UpperCase(Query.Fields[i].FieldName) = UpperCase(FPerformancePrimaryKey) then
      DataSetTypes[i] := ftInteger //improve oracle to avoid primary key violation
    else
      DataSetTypes[i] := Query.Fields[i].DataType; //improve oracle to avoid primary key violation
    ResultSetTypes[i] := ConvertDatasetToDbcType(DataSetTypes[i]);
    if ResultSetTypes[i] = stBytes then
      FieldSizes[i] := Query.Fields[i].Size
    else
      FieldSizes[i] := Query.Fields[i].DisplayWidth;
    FieldNames[i] := Query.Fields[i].FieldName;
  end;
  Query.Close;
  Query.Free;
  ConnectionConfig.PerformanceDataSetTypes := DataSetTypes;
  ConnectionConfig.PerformanceResultSetTypes := ResultSetTypes;
  ConnectionConfig.PerformanceFieldSizes := FieldSizes;
  ConnectionConfig.PerformanceFieldNames := FieldNames;
  ConnectionConfig.PerformanceFieldPropertiesDetermined := True;
end;

{**
  Skips the test where the method is called.
}
procedure TZPerformanceSQLTestCase.SkipTest;
begin
  FSkipFlag := True;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZPerformanceSQLTestCase.DefaultSetUpTest;
begin
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZPerformanceSQLTestCase.DefaultTearDownTest;
begin
end;

{**
  The empty Test method for connect test.
}
procedure TZPerformanceSQLTestCase.RunTestConnect;
begin
  SkipTest;
end;

{**
  The empty Test method for delete test.
}
procedure TZPerformanceSQLTestCase.RunTestDelete;
begin
  SkipTest;
end;

{**
  The empty Test method for fetch test.
}
procedure TZPerformanceSQLTestCase.RunTestFetch;
begin
  SkipTest;
end;

{**
  The empty Test method for filter test.
}
procedure TZPerformanceSQLTestCase.RunTestFilter;
begin
  SkipTest;
end;

{**
  The empty Test method for insert test.
}
procedure TZPerformanceSQLTestCase.RunTestInsert;
begin
  SkipTest;
end;

{**
  The empty Test method for open test.
}
procedure TZPerformanceSQLTestCase.RunTestOpen;
begin
  SkipTest;
end;

{**
  The empty Test method for sort test.
}
procedure TZPerformanceSQLTestCase.RunTestSort;
begin
  SkipTest;
end;

{**
  The empty Test method for update test.
}
procedure TZPerformanceSQLTestCase.RunTestUpdate;
begin
  SkipTest;
end;

{**
  The empty Test method for direct update test.
}
procedure TZPerformanceSQLTestCase.RunTestDirectUpdate;
begin
  SkipTest;
end;

{**
  The empty Test method for locate test.
}
procedure TZPerformanceSQLTestCase.RunTestLocate;
begin
  SkipTest;
end;

{**
  The empty Test method for lookup test.
}
procedure TZPerformanceSQLTestCase.RunTestLookup;
begin
  SkipTest;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZPerformanceSQLTestCase.SetUpTestConnect;
begin
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for delete test.
}
procedure TZPerformanceSQLTestCase.SetUpTestDelete;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for fetch test.
}
procedure TZPerformanceSQLTestCase.SetUpTestFetch;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for filter test.
}
procedure TZPerformanceSQLTestCase.SetUpTestFilter;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for insert test.
}
procedure TZPerformanceSQLTestCase.SetUpTestInsert;
var
  Count: Integer;
begin
  CleanupTable(FPerformanceTable);
  DefaultSetUpTest;

  Count := Min(MaxPerformanceLobSize, GetRecordCount);
  FAsciiStream := TStringStream.Create(RawByteString(RandomStr(Count)));
  FUnicodeStream := StreamFromData(ZWideString(RandomStr(Count)));
  FBinaryStream := StreamFromData(RandomBts(Count));
end;

{**
  The empty Set Up method for open test.
}
procedure TZPerformanceSQLTestCase.SetUpTestOpen;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for sort test.
}
procedure TZPerformanceSQLTestCase.SetUpTestSort;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for update test.
}
procedure TZPerformanceSQLTestCase.SetUpTestUpdate;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for direct update test.
}
procedure TZPerformanceSQLTestCase.SetUpTestDirectUpdate;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for locate test.
}
procedure TZPerformanceSQLTestCase.SetUpTestLocate;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Set Up method for lookup test.
}
procedure TZPerformanceSQLTestCase.SetUpTestLookup;
begin
  PopulateTable(FPerformanceTable, FPerformancePrimaryKey,
    FRecordCount, '', 0);
  DefaultSetUpTest;
end;

{**
  The empty Tear Down method for connect test.
}
procedure TZPerformanceSQLTestCase.TearDownTestConnect;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for delete test.
}
procedure TZPerformanceSQLTestCase.TearDownTestDelete;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for fetch test.
}
procedure TZPerformanceSQLTestCase.TearDownTestFetch;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for filter test.
}
procedure TZPerformanceSQLTestCase.TearDownTestFilter;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for insert test.
}
procedure TZPerformanceSQLTestCase.TearDownTestInsert;
begin
  FreeAndNil(FAsciiStream);
  FreeAndNil(FUnicodeStream);
  FreeAndNil(FBinaryStream);
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for open test.
}
procedure TZPerformanceSQLTestCase.TearDownTestOpen;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for sort test.
}
procedure TZPerformanceSQLTestCase.TearDownTestSort;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for update test.
}
procedure TZPerformanceSQLTestCase.TearDownTestUpdate;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for direct update test.
}
procedure TZPerformanceSQLTestCase.TearDownTestDirectUpdate;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for locate test.
}
procedure TZPerformanceSQLTestCase.TearDownTestLocate;
begin
  DefaultTearDownTest;
end;

{**
  The empty Tear Down method for lookup test.
}
procedure TZPerformanceSQLTestCase.TearDownTestLookup;
begin
  DefaultTearDownTest;
end;

function TZPerformanceSQLTestCase.CreateDatasetConnection: TZConnection;
begin
  Result := inherited CreateDatasetConnection;
  if not FSkipPerformanceTransactionMode then
  begin
    Result.AutoCommit := False;
    Result.TransactIsolationLevel := tiReadCommitted;
  end;
end;

function TZPerformanceSQLTestCase.CreateDbcConnection: IZConnection;
begin
  Result := inherited CreateDbcConnection;
  if not FSkipPerformanceTransactionMode then
  begin
    Result.SetAutoCommit(False);
    Result.SetTransactionIsolation(tiReadCommitted);
  end;
end;

{**
  Performs a connect test.
}
procedure TZPerformanceSQLTestCase.TestConnect;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('connect', SetUpTestConnect, RunTestConnect, TearDownTestConnect);
end;

{**
  Performs a delete test.
}
procedure TZPerformanceSQLTestCase.TestDelete;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('delete', SetUpTestDelete, RunTestDelete, TearDownTestDelete);
end;

{**
  Performs a fetch test.
}
procedure TZPerformanceSQLTestCase.TestFetch;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('fetch', SetUpTestFetch, RunTestFetch, TearDownTestFetch);
end;

{**
  Performs a filter test.
}
procedure TZPerformanceSQLTestCase.TestFilter;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('filter', SetUpTestFilter, RunTestFilter, TearDownTestFilter);
end;

{**
  Performs an insert test.
}
procedure TZPerformanceSQLTestCase.TestInsert;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('insert', SetUpTestInsert, RunTestInsert, TearDownTestInsert);
end;

{**
  Performs an open test.
}
procedure TZPerformanceSQLTestCase.TestOpen;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('open', SetUpTestOpen, RunTestOpen, TearDownTestOpen);
end;

{**
  Performs a sort test.
}
procedure TZPerformanceSQLTestCase.TestSort;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('sort', SetUpTestSort, RunTestSort, TearDownTestSort);
end;

{**
  Performs an update test.
}
procedure TZPerformanceSQLTestCase.TestUpdate;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('update', SetUpTestUpdate, RunTestUpdate, TearDownTestUpdate);
end;

{**
  Performs a direct update test.
}
procedure TZPerformanceSQLTestCase.TestDirectUpdate;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('direct-update', SetUpTestDirectUpdate,
    RunTestDirectUpdate, TearDownTestDirectUpdate);
end;

{**
  Performs a locate test.
}
procedure TZPerformanceSQLTestCase.TestLocate;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('locate', SetUpTestLocate, RunTestLocate, TearDownTestLocate);
end;

{**
  Performs a direct update test.
}
procedure TZPerformanceSQLTestCase.TestLookup;
begin
  if SkipForReason(srNoPerformance) then Exit;
  RunSelectedTest('lookup', SetUpTestLookup, RunTestLookup, TearDownTestLookup);
end;

{ TZPerformanceResultItem }

{**
  Constructs a test metric class.
  @param APIName a name of testing API.
  @param TestName a name of specific test.
  @param TryIndex an index of try. 0 is used for average.
  @param Metric a time metric (absolute or relative).
}
constructor TZPerformanceResultItem.Create(const APIName, TestName,
  Protocol, ConnectionName: string;
  const TryIndex: Integer; const Metric: Double);
begin
  FAPIName := APIName;
  FTestName := TestName;
  FTryIndex := TryIndex;
  FProtocol := Protocol;
  FConnectionName := ConnectionName;
  if Metric = 0 then
    FMetric := 0.1
  else
    FMetric := Metric;
end;

{**
  Calculates a normalized time metric (relative time metric).
  @param BaseMetric a time metric which is used as 100%.
}
procedure TZPerformanceResultItem.Normalize(BaseMetric: Double);
begin
  if BaseMetric > 0 then
    FMetric := FMetric * 100 / BaseMetric
  else FMetric := -1;
end;

{ TZPerformanceResultProcessor }

{**
  Creates a performance result processor object.
}
constructor TZPerformanceResultProcessor.Create;
begin
  FResults := TObjectList.Create;
  FSelectedAPIs := TStringList.Create;
  FSelectedTests := TStringList.Create;
  FProtocols := TStringList.Create;
  Used := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPerformanceResultProcessor.Destroy;
var I: Integer;
begin
  if Used then
  begin
    ProcessResults;
    PrintResults;
  end;

  FResults.Free;
  FSelectedAPIs.Free;
  FSelectedTests.Free;
  for i := 0 to FProtocols.Count -1 do
    TStringList(FProtocols.Objects[i]).Free;
  FProtocols.Free;
  inherited Destroy;
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZPerformanceResultProcessor.LoadConfiguration;
begin
  { Defines a selected APIs }
  if Assigned(FSelectedAPIs) then
    FSelectedAPIs.Free;
  FSelectedAPIs := SplitString(TestConfig.ReadProperty(
    TestGroup, 'apis', ''), LIST_DELIMITERS);

  { Defines a selected tests }
  if Assigned(FSelectedTests) then
    FSelectedTests.Free;
  FSelectedTests := SplitString(TestConfig.ReadProperty(
    TestGroup, 'tests', ''), LIST_DELIMITERS);

  { Reads other configuration parameters. }
  FRepeatCount := StrToIntDef(TestConfig.ReadProperty(
    TestGroup, 'repeat', ''), 1);
  FRecordCount := StrToIntDef(TestConfig.ReadProperty(
    TestGroup, 'records', ''), 1000);
  FDetails := StrToBoolEx(TestConfig.ReadProperty(
    TestGroup, 'printdetails', FALSE_VALUE));
  FBaseAPIName := TestConfig.ReadProperty(TestGroup, 'baseapi', '');
  FOutputType := TestConfig.ReadProperty(TestGroup, 'output', 'plain');
  FSkipPerformance := StrToBoolEx(TestConfig.ReadProperty(TestGroup, SKIP_PERFORMANCE_KEY, TRUE_VALUE));
end;

{**
  Finds a performance result item by specified criteria.
  @param APIName a name of API.
  @param TestName a name of specific test.
  @param TryIndex an index of try.
  @returns a found object or <code>nil</code> otherwise.
}
function TZPerformanceResultProcessor.FindResultItem(APIName,
  TestName: string; TryIndex: Integer): TZPerformanceResultItem;
var
  I: Integer;
  Current: TZPerformanceResultItem;
begin
  Result := nil;
  for I := 0 to Results.Count - 1 do
  begin
    Current := TZPerformanceResultItem(Results[I]);
    if (Current.APIName = APIName) and (Current.TestName = TestName)
      and (Current.TryIndex = TryIndex) then
    begin
      Result := Current;
      Break;
    end;
  end;
end;

function TZPerformanceResultProcessor.FindResultItem(const APIName,
  TestName, Protocol, ConnectionName: string;
  const TryIndex: Integer): TZPerformanceResultItem;
var
  I: Integer;
  Current: TZPerformanceResultItem;
begin
  Result := nil;
  for I := 0 to Results.Count - 1 do
  begin
    Current := TZPerformanceResultItem(Results[I]);
    if (Current.APIName = APIName) and (Current.TestName = TestName)
      and (Current.Protocol = Protocol) and (Current.ConnectionName = ConnectionName)
      and (Current.TryIndex = TryIndex) then
    begin
      Result := Current;
      Break;
    end;
  end;
end;

{**
  Calculates average time metrics for test results.
}
procedure TZPerformanceResultProcessor.CalculateAverages;
var
  I, J, M, N, K: Integer;
  CountTotal, CountConnectionName, CountProtocol, CountTests: Integer;
  AverageMetricTests, AverageMetricToal, AverageMetricProtocol, AverageMetricRepeat, AverageMetricConnectionName: Double;
  Current: TZPerformanceResultItem;
begin
  AverageMetricToal := 0;
  CountTotal := 0;
  for I := 0 to SelectedAPIs.Count - 1 do
  begin
    AverageMetricTests := 0;
    CountTests := 0;
    for J := 0 to SelectedTests.Count - 1 do
    begin
      AverageMetricProtocol := 0;
      CountProtocol := 0;
      for M := 0 to FProtocols.Count-1 do
      begin
        AverageMetricConnectionName := 0;
        CountConnectionName := 0;
        for N := 0 to TStringList(FProtocols.Objects[M]).Count -1 do
        begin
          AverageMetricRepeat := 0;
          for K := 1 to RepeatCount do
          begin
            Current := Self.FindResultItem(SelectedAPIs[I], SelectedTests[J], FProtocols[M], TStringList(FProtocols.Objects[M])[N], K);
            if Current <> nil then
            begin
              Inc(CountConnectionName);
              Inc(CountProtocol);
              Inc(CountTests);
              Inc(CountTotal);
              AverageMetricConnectionName := AverageMetricConnectionName + Current.Metric;
              AverageMetricProtocol := AverageMetricProtocol + Current.Metric;
              AverageMetricTests := AverageMetricTests + Current.Metric;
              AverageMetricRepeat := AverageMetricRepeat + Current.Metric;
              AverageMetricToal := AverageMetricToal + Current.Metric;
            end;
          end;
          if RepeatCount > 0 then
            AverageMetricRepeat := AverageMetricRepeat / RepeatCount
          else AverageMetricRepeat := -1;
          RegisterResult(SelectedAPIs[I], SelectedTests[J], FProtocols[M], TStringList(FProtocols.Objects[M])[N], 0, AverageMetricRepeat);
        end; {for N := 0 to TStringList(FProtocols.Objects[M]).Count -1 do}
        if ( CountConnectionName > 0 ) then
          AverageMetricConnectionName := AverageMetricConnectionName / CountConnectionName
        else AverageMetricConnectionName := -1;
        RegisterResult(SelectedAPIs[I], SelectedTests[J], FProtocols[M], 'all connections', 0, AverageMetricConnectionName);
      end; {for M := 0 to FProtocols.Count-1 do}
      if ( CountProtocol > 0 ) then
        AverageMetricProtocol := AverageMetricProtocol / CountProtocol
      else
        AverageMetricProtocol := -1;
      RegisterResult(SelectedAPIs[I], SelectedTests[J], 'all protocols', 'all connections', 0, AverageMetricProtocol);
    end; { for J := 0 to SelectedTests.Count - 1 do }
    if CountTests > 0 then
      AverageMetricTests := AverageMetricTests / CountTests
    else
      AverageMetricTests := -1;
    RegisterResult(SelectedAPIs[I], 'all tests', 'all protocols', 'all connections', 0, AverageMetricTests);
  end;
  if CountTotal > 0 then
    AverageMetricToal := AverageMetricToal / CountTotal
  else
    AverageMetricToal := -1;
  RegisterResult('all apis', 'all tests', 'all protocols', 'all connections', 0, AverageMetricToal);
end;

{**
  Normalizes performance results based on specified API.
}
procedure TZPerformanceResultProcessor.NormalizeResults;
var
  I, J, K: Integer;
  BaseMetric: Double;
  Current: TZPerformanceResultItem;
begin
  if BaseAPIName = '' then Exit;

  for I := 0 to SelectedTests.Count - 1 do
  begin
    Current := FindResultItem(BaseAPIName, SelectedTests[I], 0);
    if Current <> nil then
      BaseMetric := Current.Metric
    else BaseMetric := -1;

    for J := 0 to SelectedAPIs.Count - 1 do
    begin
      for K := 0 to RepeatCount do
      begin
        Current := Self.FindResultItem(SelectedAPIs[J], SelectedTests[I], K);
        if Current <> nil then
          Current.Normalize(BaseMetric);
      end;
    end;
  end;
end;

{**
  Prints performance test results in CSV format.
}
procedure TZPerformanceResultProcessor.PrintCSVResults;
var
  I, J, K: Integer;
  TryIndex: Integer;
  StartTry: Integer;
  Current: TZPerformanceResultItem;
begin
  Write('API');
  if Details then
    Write(',TRY');
  for I := 0 to SelectedTests.Count - 1 do
    Write(',', UpperCase(SelectedTests[I]));
  WriteLn;

  for I := 0 to SelectedAPIs.Count - 1 do
  begin
    if Details then
      StartTry := 1
    else StartTry := RepeatCount + 1;

    for J := StartTry to RepeatCount + 1 do
    begin
      Write(UpperCase(SelectedAPIs[I]));
      if J > Integer(RepeatCount) then
      begin
        if Details then
          Write(',Average');
        TryIndex := 0;
      end
      else
      begin
        if Details then
          Write(',', J);
        TryIndex := J;
      end;

      for K := 0 to SelectedTests.Count - 1 do
      begin
        Current := Self.FindResultItem(SelectedAPIs[I], SelectedTests[K],
          TryIndex);
        if (Current <> nil) and (Current.Metric >= 0) then
          Write(',', Format('%.2f', [Current.Metric]))
        else Write(',');
      end;

      WriteLn;
    end
  end;
end;

{**
  Prints performance test results in HTML format.
}
procedure TZPerformanceResultProcessor.PrintHTMLResults;
var
  I, J, K: Integer;
  TryIndex: Integer;
  StartTry: Integer;
  Current: TZPerformanceResultItem;
  Units: string;
begin
  if BaseAPIName = '' then
    Units := 'ms'
  else Units := '%';

  WriteLn('<table border=1>');
  WriteLn('  <tr>');
  Write('    <th>API</th>');
  if Details then
    Write('<th>TRY</th>');
  for I := 0 to SelectedTests.Count - 1 do
    Write('<th>', UpperCase(SelectedTests[I]), '</th>');
  WriteLn;
  WriteLn('  </tr>');

  for I := 0 to SelectedAPIs.Count - 1 do
  begin
    if Details then
      StartTry := 1
    else StartTry := RepeatCount + 1;

    WriteLn('  <tr>');
    Write('    <td rowspan=', Integer(RepeatCount) + 2 - StartTry , '>',
      UpperCase(SelectedAPIs[I]), '</td>');

    for J := StartTry to RepeatCount + 1 do
    begin
      if J > StartTry then
      begin
        WriteLn('  <tr>');
        Write('    ');
      end;

      if J > Integer(RepeatCount) then
      begin
        if Details then
          Write('<th>Average</th>');
        TryIndex := 0;
      end
      else
      begin
        if Details then
          Write('<td>', J, '</td>');
        TryIndex := J;
      end;

      for K := 0 to SelectedTests.Count - 1 do
      begin
        Current := Self.FindResultItem(SelectedAPIs[I], SelectedTests[K],
          TryIndex);
        if (Current <> nil) and (Current.Metric >= 0) then
        begin
          if TryIndex = 0 then
            Write('<th>', Format('%.2f %s', [Current.Metric, Units]), '</th>')
          else
            Write('<td>', Format('%.2f %s', [Current.Metric, Units]), '</td>');
        end else
          Write('<td>&nbsp;</td>');
      end;

      WriteLn;
      WriteLn('  </tr>');
    end
  end;
  WriteLn('</table>');
end;

{**
  Prints performance test results in plain (regular) format.
}
procedure TZPerformanceResultProcessor.PrintPlainResults;
var
  I, J, N, M: Integer;
  Current: TZPerformanceResultItem;
  Units: string;
begin
  if BaseAPIName = '' then
    Units := 'ms'
  else Units := '%';

  WriteLn('PerformanceTests of: '+ZClasses.ZEOS_VERSION);
  for I := 0 to SelectedAPIs.Count - 1 do
  begin
    for J := 0 to SelectedTests.Count - 1 do
    begin
      WriteLn(Format('    Running API: %s, Test: %s, Records: %d, Repeat: %d',
        [UpperCase(SelectedAPIs[I]), UpperCase(SelectedTests[J]), RecordCount, RepeatCount]));

      for N := 0 to FProtocols.Count -1 do
      begin
        WriteLn('');
        WriteLn('      Used protocol: '+FProtocols[n]);
        if Details then
          for M := 0 to TStringList(FProtocols.Objects[N]).Count -1 do
          begin
            Current := Self.FindResultItem(SelectedAPIs[I], SelectedTests[J], FProtocols[n], TStringList(FProtocols.Objects[N])[M], 0);
            if (Current <> nil) and (Current.Metric >= 0) then
              WriteLn(Format('        Detail Average - %.2f %s (%s)', [Current.Metric, Units, TStringList(FProtocols.Objects[N])[M]]))
            else WriteLn('        Detail Average - absent');
          end;
        Current := Self.FindResultItem(SelectedAPIs[I], SelectedTests[J], FProtocols[n], 'all connections', 0);
        if (Current <> nil) and (Current.Metric >= 0) then
          WriteLn(Format('      Protocol Average - %.2f %s (%s)', [Current.Metric, Units, FProtocols[n]]))
        else WriteLn('      Protocol Average - absent');
      end;
      Current := FindResultItem(SelectedAPIs[I], SelectedTests[J], 'all protocols', 'all connections', 0);
      if (Current <> nil) and (Current.Metric >= 0) then
        WriteLn(Format('    Test Average - %.2f %s (%s)', [Current.Metric, Units, SelectedTests[J]]))
      else WriteLn('    Test Average - absent');
      WriteLn('');
    end;
    Current := FindResultItem(SelectedAPIs[i], 'all tests', 'all protocols', 'all connections', 0);
    if (Current <> nil) and (Current.Metric >= 0) then
      WriteLn(Format('  API Average - %.2f %s (%s)', [Current.Metric, Units, SelectedAPIs[i]]))
    else WriteLn('  API Average - absent');
    WriteLn('');
  end;
  Current := FindResultItem('all apis', 'all tests', 'all protocols', 'all connections', 0);
  if (Current <> nil) and (Current.Metric >= 0) then
    WriteLn(Format('Overall Average - %.2f %s', [Current.Metric, Units]))
  else WriteLn('Overall Average - absent');
  WriteLn('');
end;

{**
  Registers a particular performance test result.
  @param APIName a name of tested API.
  @param TestName a name of specified test.
  @param TryIndex an index of try.
  @param Metric a time metric.
}
procedure TZPerformanceResultProcessor.RegisterResult(const APIName, TestName,
  Protocol, ConnectionName: String; const TryIndex: Integer; const Metric: Double);
var iProt: Integer;
begin
  Used := True;
  Results.Add(TZPerformanceResultItem.Create(APIName, TestName, Protocol,
    ConnectionName, TryIndex, Metric));
  if not (( APIName = 'all apis' ) or ( TestName = 'all tests' ) or
    ( Protocol = 'all protocols' ) or ( ConnectionName = 'all connections' )) then
  begin
    iProt := FProtocols.IndexOf(Protocol);
    if iProt = -1 then
      iProt := FProtocols.AddObject(Protocol, TStringList.Create);
    if TStringList(FProtocols.Objects[iProt]).IndexOf(ConnectionName) = -1 then
      TStringList(FProtocols.Objects[iProt]).Add(ConnectionName);
  end;
end;

{**
  Prints performance test results in specific format.
}
procedure TZPerformanceResultProcessor.PrintResults;
var
  Output: string;
begin
  if FSkipPerformance then Exit;

  Output := UpperCase(OutputType);
  if Output = 'CSV' then
    PrintCSVResults
  else if Output = 'HTML' then
    PrintHTMLResults
  else PrintPlainResults;
end;

{**
  Processes performance results.
}
procedure TZPerformanceResultProcessor.ProcessResults;
begin
  if FSkipPerformance then Exit;

  CalculateAverages;
  if BaseAPIName <> '' then
    NormalizeResults;
end;

{**
  Clears all registered results.
}
procedure TZPerformanceResultProcessor.ClearResults;
begin
  Results.Clear;
end;

initialization
  PerformanceResultProcessor := TZPerformanceResultProcessor.Create;
  PerformanceResultProcessor.LoadConfiguration;
finalization
  if Assigned(PerformanceResultProcessor) then
    PerformanceResultProcessor.Free;
end.

