{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Abstract StoredProc component              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                            & Janos Fegyverneki          }
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

unit ZStoredProcedure;

interface

{$I ZComponent.inc}

uses
  Types, SysUtils, DB, Classes, ZDbcIntfs,
  ZAbstractDataset, ZCompatibility;

type

  {**
    Abstract dataset to access to stored procedures.
  }
  TZStoredProc = class(TZAbstractDataset)
  private
    FMetaResultSet: IZResultset;
    procedure RetrieveParamValues;
    function GetStoredProcName: string;
    procedure SetStoredProcName(const Value: string);
    function GetParamType(const Value: TZProcedureColumnType): TParamType;
  protected
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    procedure SetStatementParams(Statement: IZPreparedStatement;
      ParamNames: TStringDynArray; Params: TParams;
      DataLink: TDataLink); override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

  protected
  {$IFDEF WITH_IPROVIDER}
    function PSIsSQLBased: Boolean; override;
    procedure PSExecute; override;
    {$IFDEF WITH_IPROVIDERWIDE}
    function PSGetTableNameW: WideString; override;
    {$ELSE}
    function PSGetTableName: string; override;
    {$ENDIF}
    procedure PSSetCommandText(const ACommandText: string); override;
  {$ENDIF}

  public
    procedure ExecProc; virtual;

    procedure FirstResultSet;
    procedure PreviousResultSet;
    procedure NextResultSet;
    procedure LastResultSet;
    procedure SetResultSet(const Index: Integer);
    function ResultSetCount: Integer;
    function BOR: Boolean;
    function EOR: Boolean;
  published
    property Active;
    property ParamCheck;
    property Params;
    property ShowRecordTypes;
    property Options;
    property StoredProcName: string read GetStoredProcName
      write SetStoredProcName;
  end;

implementation

uses
  ZAbstractRODataset, ZMessages, ZDatasetUtils
  {$IFDEF WITH_ASBYTES}, ZSysUtils{$ENDIF}
  {$IFDEF WITH_INLINE_ANSICOMPARETEXT}, Windows{$ENDIF}
  ;

{ TZStoredProc }

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function TZStoredProc.CreateStatement(const SQL: string; Properties: TStrings):
  IZPreparedStatement;
var
  I: Integer;
  CallableStatement: IZCallableStatement;
  Catalog, Schema, ObjectName: string;
begin
  CallableStatement := Connection.DbcConnection.PrepareCallWithParams(
    Trim(SQL), Properties);

  CallableStatement.ClearParameters;

  if Supports(CallableStatement, IZParamNamedCallableStatement) then
    if Assigned(FMetaResultSet) then
      FMetaResultSet.BeforeFirst
    else
    begin //i need allways all types to cast and there names
      SplitQualifiedObjectName(Trim(SQL), Catalog, Schema, ObjectName);
      ObjectName := Connection.DbcConnection.GetMetadata.AddEscapeCharToWildcards(ObjectName);
      FMetaResultSet := Connection.DbcConnection.GetMetadata.GetProcedureColumns(Catalog, Schema, ObjectName, '');
    end;

  for I := 0 to Params.Count - 1 do
  begin
    CallableStatement.RegisterParamType( I+1, ord(Params[I].ParamType));

    if Params[I].ParamType in [ptResult, ptOutput, ptInputOutput] then
      CallableStatement.RegisterOutParameter(I + 1,
        Ord(ConvertDatasetToDbcType(Params[I].DataType)));

    if Supports(CallableStatement, IZParamNamedCallableStatement) and
      Assigned(FMetaResultSet) then
      if FMetaResultSet.Next then
        (CallableStatement as IZParamNamedCallableStatement).RegisterParamTypeAndName(
          I, FMetaResultSet.GetString(7), Params[i].Name, FMetaResultSet.GetInt(8),
          FMetaResultSet.GetInt(9));
  end;
  Result := CallableStatement;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
  @param DataLink a datalink to get parameters.
}
{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure TZStoredProc.SetStatementParams(Statement: IZPreparedStatement;
  ParamNames: TStringDynArray; Params: TParams; DataLink: TDataLink);
var
  I: Integer;
  Param: TParam;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];

    if Params[I].ParamType in [ptResult, ptOutput] then
     Continue;

    SetStatementParam(I+1, Statement, Param);
  end;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

{**
  Retrieves parameter values from callable statement.
}
procedure TZStoredProc.RetrieveParamValues;
var
  I: Integer;
  Param: TParam;
  FCallableStatement: IZCallableStatement;
  TempBlob: IZBlob;
  {$IFDEF WITH_ASBYTES}
  TempBytes: TByteDynArray;
  Bts: TBytes;
  {$ENDIF}
begin
  if Assigned(Statement) then
    Statement.QueryInterface(IZCallableStatement, FCallableStatement);
  if not Assigned(FCallableStatement) then
    Exit;

  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];

    if not (Param.ParamType in [ptResult, ptOutput, ptInputOutput]) then
      Continue;

    if FCallableStatement.IsNull(I + 1) then
      Param.Clear
    else
      case Param.DataType of
        ftBoolean:
          Param.AsBoolean := FCallableStatement.GetBoolean(I + 1);
        ftSmallInt:
          Param.AsSmallInt := FCallableStatement.GetShort(I + 1);
        ftInteger, ftAutoInc:
          Param.AsInteger := FCallableStatement.GetInt(I + 1);
        ftFloat:
          Param.AsFloat := FCallableStatement.GetDouble(I + 1);
        ftLargeInt:
          Param.Value := FCallableStatement.GetLong(I + 1);
        ftString:
          begin
            Param.AsString := FCallableStatement.GetString(I + 1);
            {$IFDEF UNICODE}Param.DataType := ftString;{$ENDIF} //Hack: D12_UP sets ftWideString on assigning a UnicodeString
          end;
        ftWideString:
          {$IFDEF WITH_FTWIDESTRING}Param.AsWideString{$ELSE}Param.Value{$ENDIF} := FCallableStatement.GetUnicodeString(I + 1);
        ftMemo:
          begin
            Param.AsMemo := FCallableStatement.GetString(I + 1);
            {$IFDEF UNICODE}Param.DataType := ftMemo;{$ENDIF} //Hack: D12_UP sets ftWideMemo on assigning a UnicodeString
          end;
        {$IFDEF WITH_WIDEMEMO}
        ftWideMemo:
        begin
          Param.AsWideString := FCallableStatement.GetUnicodeString(I + 1);
          Param.DataType := ftWideMemo;
        end;
        {$ENDIF}
        ftBytes, ftVarBytes:
          begin
            {$IFDEF WITH_ASBYTES}
            TempBytes := StrToBytes(ZAnsiString(FCallableStatement.GetString(I + 1)));
            SetLength(Bts, High(TempBytes)+1);
            Move(PAnsiChar(TempBytes)^, PAnsiChar(Bts)^, High(TempBytes)+1);
            Param.AsBytes := Bts;
            {$ELSE}
            Param.AsString := FCallableStatement.GetString(I + 1);
            {$ENDIF}
          end;
        ftDate:
          Param.AsDate := FCallableStatement.GetDate(I + 1);
        ftTime:
          Param.AsTime := FCallableStatement.GetTime(I + 1);
        ftDateTime:
          Param.AsDateTime := FCallableStatement.GetTimestamp(I + 1);
        ftBlob:
          begin
            TempBlob := FCallableStatement.GetValue(I +1).VInterface as IZBlob;
            if not TempBlob.IsEmpty then
              Param.SetBlobData(TempBlob.GetBuffer, TempBlob.Length);
            TempBlob := nil;
          end
        else
           raise EZDatabaseError.Create(SUnKnownParamDataType);
      end;
  end;
end;

{**
  Performs internal query opening.
}
procedure TZStoredProc.InternalOpen;
begin
  inherited InternalOpen;

  RetrieveParamValues;
end;

{**
  Performs internal query closing.
}
procedure TZStoredProc.InternalClose;
begin
  inherited InternalClose;
end;

function TZStoredProc.GetStoredProcName: string;
begin
  Result := Trim(SQL.Text);
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure TZStoredProc.SetStoredProcName(const Value: string);
var
  OldParams: TParams;
  Catalog, Schema, ObjectName: string;
  ColumnType: Integer;
begin
  CheckConnected;
  if AnsiCompareText(Trim(SQL.Text), Trim(Value)) <> 0 then
  begin
    SQL.Text := Value;
    if ParamCheck and (Value <> '') and not (csLoading in ComponentState) and Assigned(Connection) then
    begin
      CheckConnected;
      Connection.ShowSQLHourGlass;
      try
        SplitQualifiedObjectName(Value, Catalog, Schema, ObjectName);
        ObjectName := Connection.DbcConnection.GetMetadata.AddEscapeCharToWildcards(ObjectName);
        FMetaResultSet := Connection.DbcConnection.GetMetadata.GetProcedureColumns(Catalog, Schema, ObjectName, '');
        OldParams := TParams.Create;
        try
          OldParams.Assign(Params);
          Params.Clear;
          while FMetaResultSet.Next do
          begin
            ColumnType := FMetaResultSet.GetIntByName('COLUMN_TYPE');
            if ColumnType >= 0 then //-1 is result column
              Params.CreateParam(ConvertDbcToDatasetType(TZSqlType(FMetaResultSet.GetIntByName('DATA_TYPE'))),
                FMetaResultSet.GetStringByName('COLUMN_NAME'),
                GetParamType(TZProcedureColumnType(ColumnType)));
          end;
          Params.AssignValues(OldParams);
        finally
          OldParams.Free;
        end;
      finally
        Connection.HideSQLHourGlass;
      end;
    end;
  end;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

procedure TZStoredProc.ExecProc;
begin
  Connection.ShowSQLHourGlass;
  try
    if Active then
      Close;
    ExecSQL;
    RetrieveParamValues;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Procedure the First retrieved resultset if the givens
}
procedure TZStoredProc.FirstResultSet;
begin
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      SetAnotherResultset((Statement as IZCallableStatement).GetFirstResultSet);
end;

{**
  Procedure the Previous retrieved resultset if the givens
}
procedure TZStoredProc.PreviousResultSet;
begin
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      SetAnotherResultset((Statement as IZCallableStatement).GetPreviousResultSet);
end;

{**
  Procedure the Next retrieved resultset if the givens
}
procedure TZStoredProc.NextResultSet;
begin
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      SetAnotherResultset((Statement as IZCallableStatement).GetNextResultSet);
end;

{**
  Procedure the Last retrieved resultset if the givens
}
procedure TZStoredProc.LastResultSet;
begin
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      SetAnotherResultset((Statement as IZCallableStatement).GetLastResultSet);
end;

{**
  Retrieves a ResultSet by his index.
  @param Integer the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
procedure TZStoredProc.SetResultSet(const Index: Integer);
begin
  if Assigned(Statement) then
    if ( Index < 0 ) or ( Index > (Statement as IZCallableStatement).GetResultSetCount -1 ) then
      raise Exception.Create(Format(SListIndexError, [Index]))
    else
      SetAnotherResultset((Statement as IZCallableStatement).GetResultSetByIndex(Index));
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZStoredProc.ResultSetCount: Integer;
begin
  Result := 0;
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      Result := (Statement as IZCallableStatement).GetResultSetCount;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZStoredProc.BOR: Boolean;
begin
  Result := True;
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      Result := (Statement as IZCallableStatement).BOR;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZStoredProc.EOR: Boolean;
begin
  Result := True;
  if Assigned(Statement) then
    if (Statement as IZCallableStatement).HasMoreResultSets then
      Result := (Statement as IZCallableStatement).EOR;
end;

{**
  Converts procedure column type to dataset param type.
  @param Value a initial procedure column type.
  @return a corresponding param type.
}
function TZStoredProc.GetParamType(const Value: TZProcedureColumnType): TParamType;
begin
  case Value of
    pctIn:
      Result := ptInput;
    pctInOut:
      Result := ptInputOutput;
    pctOut:
      Result := ptOutput;
    pctReturn:
      Result := ptResult;
    pctResultSet:
      Result := ptResult;
  else
    Result := ptUnknown;
  end;
end;

{$IFDEF WITH_IPROVIDER}
{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZStoredProc.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

{**
  Gets the name of the stored procedure.
  @returns the name of this stored procedure.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZStoredProc.PSGetTableNameW: WideString;
{$ELSE}
function TZStoredProc.PSGetTableName: string;
{$ENDIF}
begin
  Result := StoredProcName;
end;

{**
  Executes this stored procedure.
}
procedure TZStoredProc.PSExecute;
begin
  ExecProc;
end;

{**
  Assignes a new name for this stored procedure.
  @param ACommandText a new name for this stored procedure.
}
procedure TZStoredProc.PSSetCommandText(const ACommandText: string);
begin
  StoredProcName := ACommandText;
end;
{$ENDIF}

end.
