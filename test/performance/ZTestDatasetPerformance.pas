{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for TDataset Performance          }
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

unit ZTestDatasetPerformance;

interface

{$I ZPerformance.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, Classes,
  ZPerformanceTestCase, ZDataset, ZDbcIntfs, DB;

type

  {** Implements a performance test case for ZeosDBO TDataset API. }
  TZDatasetPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FQuery: TZQuery;
    FLastStringField: String;
  protected
    property Query: TZQuery read FQuery write FQuery;

    function GetImplementedAPI: string; override;
    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
    procedure SetUpTestDirectUpdate; override;
    procedure RunTestDirectUpdate; override;
    procedure SetUpTestLocate; override;
    procedure RunTestLocate; override;
    procedure SetUpTestLookup; override;
    procedure RunTestLookup; override;
    procedure SetUpTestSort; override;
    procedure RunTestSort; override;
  end;

implementation

uses ZSysUtils, ZTestCase, ZSqlTestCase;

{ TZDatasetPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDatasetPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dataset';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZDatasetPerformanceTestCase.SetUp;
begin
  inherited SetUp;
  Query := CreateQuery;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZDatasetPerformanceTestCase.TearDown;
begin
  if (not SkipPerformanceTransactionMode) and
    Query.Connection.Connected then Query.Connection.Commit;
  FreeAndNil(FQuery);
  inherited TearDown;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDatasetPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Connect;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDatasetPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Disconnect;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDatasetPerformanceTestCase.RunTestConnect;
begin
  Connection.Connect;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  The empty Set Up method for insert test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable;
  Query.Open;
end;

{**
  Performs an insert test.
}
procedure TZDatasetPerformanceTestCase.RunTestInsert;
var
  Index, I: Integer;
begin
  for Index := 1 to GetRecordCount do
    with Query do
    begin
      Append;
      for I := 0 to High(ConnectionConfig.PerformanceDataSetTypes) do
        case ConnectionConfig.PerformanceDataSetTypes[i] of
          ftString, ftFixedChar:
            Fields[i].AsString := RandomStr(ConnectionConfig.PerformanceFieldSizes[i]);
          ftMemo, ftFmtMemo:
            (Fields[i] as TBlobField).LoadFromStream(FAsciiStream);
          {$IFDEF WITH_WIDEFIELDS}
          ftWideString{$IFNDEF FPC}, ftFixedWideChar{$ENDIF}:
            Fields[i].AsWideString := WideString(RandomStr(ConnectionConfig.PerformanceFieldSizes[i]));
          ftWideMemo:
            (Fields[i] as TBlobField).LoadFromStream(FUnicodeStream);
          {$ENDIF}
          ftSmallint, ftWord {$IFDEF WITH_FTBYTE}, ftByte {$ENDIF} {$IFDEF WITH_FTSHORTINT}, ftShortint {$ENDIF}:
            Fields[i].AsInteger := Random(127);
          ftInteger, ftLargeint:
            Fields[i].AsInteger := Index;
          ftBoolean:
            Fields[i].AsBoolean := Random(1) = 0;
          ftBCD, ftFMTBcd, ftFloat, ftCurrency{$IFDEF WITH_FTEXTENDED}, ftExtended{$ENDIF}:
            Fields[i].AsFloat := RandomFloat(-100, 100);
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            Fields[i].AsFloat := now;
          ftVarBytes, ftBytes:
            Fields[i].Value := RandomBts(ConnectionConfig.PerformanceFieldSizes[i]);
          ftBlob:
            (Fields[i] as TBlobField).LoadFromStream(FBinaryStream);
          {$IFDEF WITH_FTGUID}
          ftGuid:
            Fields[i].AsString := RandomGUIDString;
          {$ENDIF}
          {ftAutoInc, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
          ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
          ftVariant, ftInterface, ftIDispatch);}
      end;
      Post;
    end;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  Performs an open test.
}
procedure TZDatasetPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable;
  Query.Open;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  The empty Set Up method for fetch test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable;
  Query.ReadOnly := True;
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDatasetPerformanceTestCase.RunTestFetch;
var I: Integer;
begin
  while not Query.EOF do
    with Query do
    begin
      for I := 0 to High(ConnectionConfig.PerformanceDataSetTypes) do
        case ConnectionConfig.PerformanceDataSetTypes[i] of
          ftString,
          ftFixedChar
          {$IFDEF WITH_FTGUID}, ftGuid{$ENDIF}:
            Fields[i].AsString;
          ftMemo,
          ftFmtMemo:
            if LoadLobs then Fields[i].AsString;
          {$IFDEF WITH_WIDEFIELDS}
          ftWideMemo:
            if LoadLobs then Fields[i].AsWideString;
          ftWideString
          {$IFNDEF FPC}, ftFixedWideChar{$ENDIF}:
            Fields[i].AsWideString;
          {$ENDIF}
          ftSmallint, ftWord {$IFDEF WITH_FTBYTE}, ftByte {$ENDIF} {$IFDEF WITH_FTSHORTINT}, ftShortint {$ENDIF},
          ftInteger, ftLargeint:
            Fields[i].AsInteger;
          ftBoolean:
            Fields[i].AsBoolean;
          ftBCD, ftFMTBcd, ftFloat, ftCurrency{$IFDEF WITH_FTEXTENDED}, ftExtended{$ENDIF}:
            Fields[i].AsFloat;
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            Fields[i].AsDateTime;
          ftBlob:
            if LoadLobs then Fields[i].Value;
          ftVarBytes, ftBytes:
            Fields[i].Value;
          {ftAutoInc, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
          ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
          ftVariant, ftInterface, ftIDispatch);}
        end;
      Next;
    end;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  The empty Set Up method for update test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable;
  Query.Open;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  Performs an update test.
}
procedure TZDatasetPerformanceTestCase.RunTestUpdate;
var I: Integer;
begin
  while not Query.EOF do
    with Query do
    begin
      Edit;
      for I := 1 to High(ConnectionConfig.PerformanceDataSetTypes) do
      begin
        case ConnectionConfig.PerformanceDataSetTypes[i] of
          ftString, ftFixedChar:
            Fields[i].AsString := RandomStr(ConnectionConfig.PerformanceFieldSizes[i]);
          ftMemo, ftFmtMemo:
            Fields[i].AsString := RandomStr(RecordCount*100);
          {$IFDEF WITH_WIDEFIELDS}
          ftWideString{$IFNDEF FPC}, ftFixedWideChar{$ENDIF}:
            Fields[i].AsWideString := WideString(RandomStr(ConnectionConfig.PerformanceFieldSizes[i]));
          ftWideMemo:
            Fields[i].AsWideString := WideString(RandomStr(RecordCount*100));
          {$ENDIF}
          ftSmallint, ftWord {$IFDEF WITH_FTBYTE}, ftByte {$ENDIF} {$IFDEF WITH_FTSHORTINT}, ftShortint {$ENDIF}:
            Fields[i].AsInteger := Random(127);
          ftInteger, ftLargeint:
            Fields[i].AsInteger := RandomInt(-100, 100);
          ftBoolean:
            Fields[i].AsBoolean := Random(1) = 0;
          ftBCD, ftFMTBcd, ftFloat, ftCurrency{$IFDEF WITH_FTEXTENDED}, ftExtended{$ENDIF}:
            Fields[i].AsFloat := RandomFloat(-100, 100);
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            Fields[i].AsDateTime := now;
          ftVarBytes, ftBytes:
            Fields[i].Value := RandomBts(ConnectionConfig.PerformanceFieldSizes[i]);
          ftBlob:
            Fields[i].Value := RandomBts(RecordCount*100);
          {$IFDEF WITH_FTGUID}
          ftGuid:
            Fields[i].AsString := RandomGUIDString;
          {$ENDIF}
          {ftAutoInc, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
          ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
          ftVariant, ftInterface, ftIDispatch);}
        end;
      end;
      Post;
      Next;
    end;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;
{**
  The empty Set Up method for delete test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable;
  Query.Open;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  Performs a delete test.
}
procedure TZDatasetPerformanceTestCase.RunTestDelete;
begin
  while not Query.EOF do
    Query.Delete;
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

procedure TZDatasetPerformanceTestCase.SetUpTestDirectUpdate;
var
  I: Integer;
begin
  inherited;
  SetLength(FDirectFieldTypes, 0);
  SetLength(FDirectFieldNames, 0);
  SetLength(FDirectFieldSizes, 0);
  for i := 0 to high(ConnectionConfig.PerformanceResultSetTypes) do
  begin
    { copy predefined values to temporary arrays }
    SetLength(FDirectFieldTypes, Length(FDirectFieldTypes)+1);
    FDirectFieldTypes[High(FDirectFieldTypes)] := ConnectionConfig.PerformanceDataSetTypes[i];
    SetLength(FDirectFieldNames, Length(FDirectFieldNames)+1);
    FDirectFieldNames[High(FDirectFieldNames)] := ConnectionConfig.PerformanceFieldNames[i];
    SetLength(FDirectFieldSizes, Length(FDirectFieldSizes)+1);
    FDirectFieldSizes[High(FDirectFieldSizes)] := ConnectionConfig.PerformanceFieldSizes[i];
    { check types }
    case ConnectionConfig.PerformanceDataSetTypes[i] of
      ftBytes, ftBlob:
        // check if driver can do GetBinaryEscapeString (f.i., firebird below 2.5 doesn't support x'hex' syntax)
        // we do a test run of the method and remove type if it raises exception
        try
          Connection.DbcConnection.GetBinaryEscapeString(RandomBts(5));
        except
          SetLength(FDirectSQLTypes, Length(FDirectSQLTypes)-1); //omit these types to avoid exception
          SetLength(FDirectFieldNames, Length(FDirectFieldNames)-1); //omit these names to avoid exception
          SetLength(FDirectFieldSizes, Length(FDirectFieldSizes)-1); //omit these names to avoid exception
        end;
      ftBoolean:
        case ProtocolType of
          protSQLite, protMySQL:
            begin
              Self.FTrueVal := #39'Y'#39;
              Self.FFalseVal := #39'N'#39;
            end;
          protPostgre:
            begin
              Self.FTrueVal := 'TRUE';
              Self.FFalseVal := 'FALSE';
            end
          else
            begin
              Self.FTrueVal := '1';
              Self.FFalseVal := '0';
            end;
        end;
      ftDate, ftTime, ftDateTime, ftTimeStamp: //session dependend values. This i'll solve later
        begin
          SetLength(FDirectFieldTypes, Length(FDirectFieldTypes)-1); //omit these types to avoid exception
          SetLength(FDirectFieldNames, Length(FDirectFieldNames)-1); //omit these names to avoid exception
          SetLength(FDirectFieldSizes, Length(FDirectFieldSizes)-1); //omit these names to avoid exception
        end;
    end;
  end;
end;
{**
  Performs a direct update test.
}
procedure TZDatasetPerformanceTestCase.RunTestDirectUpdate;
var
  I, N: Integer;
  SQL: String;
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  if SkipForReason(srNoPerformance) then Exit;

  OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  OldThousandSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := ',';
  for I := 1 to GetRecordCount do
  begin
    SQL := 'UPDATE '+PerformanceTable+' SET ';
    for N := 1 to high(FDirectFieldTypes) do
    begin
      case FDirectFieldTypes[n] of
        ftBoolean:
          if Random(1) = 1 then
            SQL := SQL + FDirectFieldNames[N]+'='+ FTrueVal
          else
            SQL := SQL + FDirectFieldNames[N]+'='+ FFalseVal;
        ftSmallint, ftWord {$IFDEF WITH_FTBYTE}, ftByte {$ENDIF} {$IFDEF WITH_FTSHORTINT}, ftShortint {$ENDIF}:
          SQL := SQL + FDirectFieldNames[N]+'='+IntToStr(Random(127));
        ftInteger, ftLargeint:
          SQL := SQL + FDirectFieldNames[N]+'='+IntToStr(Random(I));
        ftBCD, ftFMTBcd, ftFloat, ftCurrency{$IFDEF WITH_FTEXTENDED}, ftExtended{$ENDIF}:
          {$IFNDEF WITH_FORMATSETTINGS}
          SQL := SQL + FDirectFieldNames[N]+'='+StringReplace(FloatToStr(RandomFloat(-100, 100)), ',','.', [rfReplaceAll]);
          {$ELSE}
          SQL := SQL + FDirectFieldNames[N]+'='+FloatToStr(RandomFloat(-100, 100));
          {$ENDIF}
        ftString, ftFixedChar{$IFDEF WITH_WIDEFIELDS}, ftWideString{$IFNDEF FPC}, ftFixedWideChar{$ENDIF}{$ENDIF}:
          SQL := SQL + FDirectFieldNames[N]+'='+Connection.DbcConnection.GetEscapeString(RandomStr(FDirectFieldSizes[N]));
        {$IFDEF WITH_FTGUID}
        ftGuid:
          SQL := SQL + FDirectFieldNames[N]+'='+Connection.DbcConnection.GetEscapeString(RandomGUIDString);
        {$ENDIF}
        ftBytes:
          SQL := SQL + FDirectFieldNames[N]+'='+Connection.DbcConnection.GetBinaryEscapeString(RandomBts(FDirectFieldSizes[N]));
        ftMemo, ftFmtMemo{$IFDEF WITH_WIDEFIELDS}, ftWideMemo{$ENDIF}:
          SQL := SQL + FDirectFieldNames[N]+'='+Connection.DbcConnection.GetEscapeString(RandomStr(GetRecordCount*100));
        ftBlob:
          SQL := SQL + FDirectFieldNames[N]+'='+Connection.DbcConnection.GetBinaryEscapeString(RandomBts(GetRecordCount*100));
        //ftDate, ftTime, ftDateTime, ftTimestamp //session dependend
      end;
      if N = high(FDirectFieldTypes) then
        SQL := SQL + ' WHERE '+ PerformancePrimaryKey+'='+IntToStr(i)
      else
        SQL := SQL + ', ';
    end;
    Query.SQL.Text := SQL;
    Query.ExecSQL;
  end;
  if not SkipPerformanceTransactionMode then Connection.Commit;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldThousandSeparator;
end;

{**
  The empty Set Up method for locate test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestLocate;
var I: Integer;
begin
  inherited SetUpTestLocate;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable+' ORDER BY '+PerformancePrimaryKey;
  Query.Open;
  Query.Last;
  Query.First;
  FLastStringField := '';
  for i := High(ConnectionConfig.PerformanceDataSetTypes) downto 0 do
    if ConnectionConfig.PerformanceDataSetTypes[i] in [ftString{$IFDEF WITH_WIDEFIELDS}, ftWideString{$ENDIF}] then
    begin
      FLastStringField := ConnectionConfig.PerformanceFieldNames[i];
      Break;
    end;
end;

{**
  Performs a locate test.
}
procedure TZDatasetPerformanceTestCase.RunTestLocate;
begin
  Query.Locate(FLastStringField,'AAAAAAAAAA',[]);
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

{**
  The empty Set Up method for lookup test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestLookup;
var I: Integer;
begin
  inherited SetUpTestLookup;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable+' ORDER BY '+PerformancePrimaryKey;
  Query.Open;
  Query.Last;
  Query.First;
  FLastStringField := '';
  for i := High(ConnectionConfig.PerformanceDataSetTypes) downto 0 do
    if ConnectionConfig.PerformanceDataSetTypes[i] in [ftString{$IFDEF WITH_WIDEFIELDS}, ftWideString{$ENDIF}] then
    begin
      FLastStringField := ConnectionConfig.PerformanceFieldNames[i];
      Break;
    end;
end;

{**
  Performs a lookup test.
}
procedure TZDatasetPerformanceTestCase.RunTestLookup;
begin
  Query.Lookup(FLastStringField,'AAAAAAAAAA',PerformancePrimaryKey);
  if not SkipPerformanceTransactionMode then
    Query.Connection.Commit;
end;

procedure TZDatasetPerformanceTestCase.SetUpTestSort;
begin
  inherited;
  Query.SQL.Text := 'SELECT * FROM '+PerformanceTable+' ORDER BY '+PerformancePrimaryKey;
end;

procedure TZDatasetPerformanceTestCase.RunTestSort;
begin
  Query.SortedFields := PerformancePrimaryKey +' Desc';
  Query.Open;
  Query.SortedFields := PerformancePrimaryKey;
  Query.Open;
end;
initialization
  RegisterTest('performance', TZDatasetPerformanceTestCase.Suite);
end.

