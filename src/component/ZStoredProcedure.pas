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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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
  Types, SysUtils, Classes, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  ZDbcIntfs, ZAbstractDataset, ZCompatibility;

type

  {**
    Abstract dataset to access to stored procedures.
  }
  TZStoredProc = class(TZAbstractDataset)
  private
    FMetaResultSet: IZResultset;
    function GetStoredProcName: string;
    procedure SetStoredProcName(const Value: string);
  protected
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    procedure SetStatementParams(const Statement: IZPreparedStatement;
      const ParamNames: TStringDynArray; Params: TParams;
      DataLink: TDataLink); override;
    procedure InternalOpen; override;
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
    function NextResultSet: Boolean; override;
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
  ZAbstractRODataset, ZMessages, ZDatasetUtils, ZDbcMetadata
  {$IFDEF WITH_ASBYTES}, ZSysUtils{$ENDIF} ,FmtBCD
  {$IFDEF WITH_INLINE_ANSICOMPARETEXT}, Windows{$ENDIF};

{ TZStoredProc }

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZStoredProc.CreateStatement(const SQL: string; Properties: TStrings):
  IZPreparedStatement;
var
  I: Integer;
  CallableStatement: IZCallableStatement;
begin
  CallableStatement := Connection.DbcConnection.PrepareCallWithParams(
    Trim(SQL), Properties);

  if not Connection.DbcConnection.UseMetadata then
    for I := 0 to Params.Count - 1 do
      with Params[I] do
        CallableStatement.RegisterParameter(I, ConvertDatasetToDbcType(DataType),
        DatasetTypeToProcColDbc[ParamType], Name, Precision, NumericScale);
  Result := CallableStatement;
end;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
  @param DataLink a datalink to get parameters.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ParamNames/DataLink" not used} {$ENDIF}
procedure TZStoredProc.SetStatementParams(const Statement: IZPreparedStatement;
  const ParamNames: TStringDynArray; Params: TParams; DataLink: TDataLink);
var
  I: Integer;
  Param: TParam;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];

    if Params[I].ParamType in [ptResult, ptOutput] then
     Continue;

    SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, Param);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Performs internal query opening.
}
procedure TZStoredProc.InternalOpen;
begin
  inherited InternalOpen;
  if Resultset.GetType <> rtForwardOnly then
    Resultset.BeforeFirst;
end;

function TZStoredProc.GetStoredProcName: string;
begin
  Result := Trim(SQL.Text);
end;

procedure TZStoredProc.SetStoredProcName(const Value: string);
var
  OldParams: TParams;
  Catalog, Schema, ObjectName: string;
  ColumnType: TZProcedureColumnType;
  SQLType: TZSQLType;
  Precision: Integer;
  Metadata: IZDatabaseMetadata;
  DatabaseInfo: IZDatabaseInfo;
begin
  Catalog := Trim(SQL.Text);
  ObjectName := Trim(Value);
  if (AnsiCompareText(Catalog, ObjectName) <> 0) or ((Params.Count = 0) and
     not (csDesigning in ComponentState) and not Active) then begin
    SQL.Text := Value;
    if ParamCheck and (Value <> '') and not (csLoading in ComponentState) and Assigned(Connection) then begin
      CheckConnected;
      Metadata := Connection.DbcConnection.GetMetadata;
      DatabaseInfo := Metadata.GetDatabaseInfo;
      Connection.ShowSQLHourGlass;
      try
        SplitQualifiedObjectName(Value, DatabaseInfo.SupportsCatalogsInProcedureCalls,
          DatabaseInfo.SupportsSchemasInProcedureCalls, Catalog, Schema, ObjectName);
        Schema := Metadata.AddEscapeCharToWildcards(Schema);
        ObjectName := Metadata.AddEscapeCharToWildcards(ObjectName);
        FMetaResultSet := Metadata.GetProcedureColumns(Catalog, Schema, ObjectName, '');
        OldParams := TParams.Create;
        try
          OldParams.Assign(Params);
          Params.Clear;
          while FMetaResultSet.Next do begin
            ColumnType := TZProcedureColumnType(FMetaResultSet.GetInt(ProcColColumnTypeIndex));
            SQLType := TZSqlType(FMetaResultSet.GetInt(ProcColDataTypeIndex));
            if Ord(SQLType) >= Ord(stString)
            then Precision := FMetaResultSet.GetInt(ProcColLengthIndex)
            else Precision := FMetaResultSet.GetInt(ProcColPrecisionIndex);
            Params.CreateParam(ConvertDbcToDatasetType(SQLType,
              Connection.ControlsCodePage, Precision),
                FMetaResultSet.GetString(ProcColColumnNameIndex),
                ProcColDbcToDatasetType[ColumnType]);
          end;
          Params.AssignValues(OldParams);
        finally
          OldParams.Free;
        end;
      finally
        Connection.HideSQLHourGlass;
        DatabaseInfo := nil;
        Metadata := nil;
      end;
    end;
  end;
end;

procedure TZStoredProc.ExecProc;
begin
  ExecSQL;
end;

{**
  Procedure the First retrieved resultset if the givens
}
procedure TZStoredProc.FirstResultSet;
var CallableStmt: IZCallableStatement;
begin
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then
    SetAnotherResultset(CallableStmt.GetFirstResultSet);
end;

{**
  Procedure the Previous retrieved resultset if the givens
}
procedure TZStoredProc.PreviousResultSet;
var CallableStmt: IZCallableStatement;
begin
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then
    SetAnotherResultset(CallableStmt.GetPreviousResultSet);
end;

{**
  Procedure the Next retrieved resultset if the givens
}
function TZStoredProc.NextResultSet: Boolean;
var CallableStmt: IZCallableStatement;
  RS: IZResultSet;
begin
  Result := False;
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then begin
    RS := CallableStmt.GetNextResultSet;
    Result := RS <> nil;
    if Result then
      SetAnotherResultset(RS);
  end;
end;

{**
  Procedure the Last retrieved resultset if the givens
}
procedure TZStoredProc.LastResultSet;
var CallableStmt: IZCallableStatement;
begin
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then
    SetAnotherResultset(CallableStmt.GetLastResultSet)
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
    if Statement.GetMoreResults then
      Result := (Statement as IZCallableStatement).GetResultSetCount;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZStoredProc.BOR: Boolean;
var CallableStmt: IZCallableStatement;
begin
  Result := True;
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then begin
    Result := CallableStmt.BOR;
    CallableStmt := nil;
  end;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZStoredProc.EOR: Boolean;
var CallableStmt: IZCallableStatement;
begin
  Result := True;
  if Assigned(Statement) and Supports(Statement, IZCallableStatement, CallableStmt) then begin
    Result := CallableStmt.EOR;
    CallableStmt := nil;
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

