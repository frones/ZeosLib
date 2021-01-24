{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Abstract MemTable component               }
{                                                         }
{          Originally written by EgonHugeist              }
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

unit ZMemTable;

{$I ZComponent.inc}

interface

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZCompatibility,
  ZDbcIntfs,
  ZAbstractDataset, ZAbstractRODataset;

type
  TZAbstractMemTable = class(TZAbstractDataset)
  protected
    FConSettings: TZConSettings;
    FCharacterSet: TZCodePage;
    function CreateResultSet(const {%H-}SQL: string; MaxRows: Integer):
      IZResultSet; override;
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    procedure CheckSQLQuery; override;
    procedure CheckConnected; override;
    procedure InternalRefresh; override;
    procedure InternalPrepare; override;
  end;

implementation

uses ZMessages, ZEncoding,
  ZDbcStatement, ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils,
  ZDbcCachedResultSet,
  ZDatasetUtils,
  {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF};

type
  TZMemResultSetPreparedStatement = Class(TZBeginnerPreparedStatement,
    IZPreparedStatement)
  private
    FColumnList: TObjectList;
  protected
    procedure CheckParameterIndex(var Value: Integer); override;
  public
    constructor Create(ConSettings: PZConSettings;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}AColumnList: TObjectList;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
    destructor Destroy; override;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  End;

{ TZMemResultSetPreparedStatement }

procedure TZMemResultSetPreparedStatement.CheckParameterIndex(
  var Value: Integer);
begin
  raise EZSQLException.Create(Format(SParametersError, [Value, 0]));
end;

constructor TZMemResultSetPreparedStatement.Create(
  ConSettings: PZConSettings;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}AColumnList: TObjectList;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
begin
  Self.ConSettings := ConSettings;
  FColumnList := TObjectList.Create;
  CopyColumnsInfo(AColumnList, FColumnList);
  Self.ConSettings := ConSettings;
end;

destructor TZMemResultSetPreparedStatement.Destroy;
begin
  inherited;
  FreeAndNil(FColumnList);
end;

function TZMemResultSetPreparedStatement.ExecutePrepared: Boolean;
begin
  LastResultSet := ExecuteQueryPrepared;
  Result := True;
end;

function TZMemResultSetPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var VirtualResultSet: TZVirtualResultSet;
begin
  VirtualResultSet := TZVirtualResultSet.CreateWithColumns(FColumnList, '', ConSettings);
  Result := VirtualResultSet;
  VirtualResultSet.SetType(GetResultSetType);
  VirtualResultSet.SetConcurrency(GetResultSetConcurrency);
end;

function TZMemResultSetPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := 0;
end;

{ TZAbstractMemTable }

procedure TZAbstractMemTable.CheckConnected;
begin
  // NOOP
end;

procedure TZAbstractMemTable.CheckSQLQuery;
begin
  if FieldDefs.Count = 0 then
    raise EZDataBaseError.Create(SQueryIsEmpty);
end;

function TZAbstractMemTable.CreateResultSet(const SQL: string;
  MaxRows: Integer): IZResultSet;
var RS: IZCachedResultSet;
begin
  FCharacterSet.Encoding := {$IFDEF UNICODE}ceUTF16{$ELSE}{$IFDEF FPC}ceUTF8{$ELSE}ceAnsi{$ENDIF}{$ENDIF};
  FCharacterSet.CP := {$IFDEF UNICODE}zCP_UTF16{$ELSE}{$IFDEF FPC}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF};
  Statement := CreateStatement('', Properties);
  if RequestLive then
    Statement.SetResultSetConcurrency(rcUpdatable)
  else
    Statement.SetResultSetConcurrency(rcReadOnly);
  Statement.SetFetchDirection(fdForward);
  if IsUniDirectional then
    Statement.SetResultSetType(rtForwardOnly)
  else
    Statement.SetResultSetType(rtScrollInsensitive);
  if MaxRows > 0 then
    Statement.SetMaxRows(MaxRows);
  Result := Statement.ExecuteQueryPrepared;
  Result.QueryInterface(IZCachedResultSet, RS);
  CachedResultSet := RS;
end;

function TZAbstractMemTable.CreateStatement(const SQL: string;
  Properties: TStrings): IZPreparedStatement;
var ColumnList: TObjectList;
    I: Integer;
    Current: TFieldDef;
    ColumnInfo: TZColumnInfo;
    AConSettings: PZConSettings;
begin
  ColumnList := TObjectList.Create(True);
  try
    for I := 0 to FieldDefs.Count - 1 do begin
      Current := FieldDefs[I];
      if not Current.InternalCalcField then begin
        ColumnInfo := TZColumnInfo.Create;
        ColumnInfo.ColumnType := ConvertDatasetToDbcType(Current.DataType);
        ColumnInfo.ColumnName := Current.Name;
        ColumnInfo.Precision := Current.Size;
        ColumnInfo.Writable := RequestLive;
        ColumnInfo.ReadOnly := not RequestLive;
        if Current.DataType in [ftBCD, ftFmtBCD] then
          ColumnInfo.Scale := Current.Size
        else if Current.DataType in [ftMemo, ftString, ftFixedChar] then
          ColumnInfo.ColumnCodePage := GetTransliterateCodePage(Connection.ControlsCodePage)
        else if Current.DataType in [{$IFDEF WITH_FTWIDEMEMO}ftWideMemo, {$ENDIF}
          ftWideString{$IFDEF WITH_FTFIXEDWIDECHAR}, ftFixedWideChar{$ENDIF}] then
          ColumnInfo.ColumnCodePage := zCP_UTF16;
        ColumnInfo.ColumnLabel := Current.DisplayName;
        ColumnList.Add(ColumnInfo);
      end;
    end;
    if (Connection <> nil) and (Connection.Connected)
    then AConSettings := Connection.DbcConnection.GetConSettings
    else begin
      FConSettings.ClientCodePage := @FCharacterSet;
      AConSettings := @FConSettings;
    end;
    Result := TZMemResultSetPreparedStatement.Create(AConSettings, ColumnList, Properties);
  finally
    FreeAndNil(ColumnList);
  end;
end;

procedure TZAbstractMemTable.InternalPrepare;
begin
  //NOOP
end;

procedure TZAbstractMemTable.InternalRefresh;
begin
  //NOOP
end;

end.
