{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Generic Cached Resolver                  }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcGenericResolver;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} FmtBCD,
  ZVariant, ZDbcIntfs, ZDbcCache, ZDbcCachedResultSet, ZCompatibility,
  ZSelectSchema, ZClasses, ZCollections;

type

  {** Implements a resolver parameter object. }
  TZResolverParameter = class (TObject)
  private
    FColumnIndex: Integer;
    FColumnName: string;
    FColumnType: TZSQLType;
    FNewValue: Boolean;
    FDefaultValue: string;
  public
    constructor Create(ColumnIndex: Integer; const ColumnName: string;
      ColumnType: TZSQLType; NewValue: Boolean; const DefaultValue: string);
    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
    property ColumnName: string read FColumnName write FColumnName;
    property ColumnType: TZSQLType read FColumnType write FColumnType;
    property NewValue: Boolean read FNewValue write FNewValue;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  {**
    Implements a generic cached resolver object which generates
    DML SQL statements and posts resultset updates to database.
  }

  { TZGenericCachedResolver }

  TZGenericCachedResolver = class (TInterfacedObject, IZCachedResolver)
  private
    FConnection: IZConnection;
    FStatement : IZStatement;
    FMetadata: IZResultSetMetadata;
    FDatabaseMetadata: IZDatabaseMetadata;
    FIdentifierConvertor: IZIdentifierConvertor;

    FInsertColumns: TObjectList;
    FUpdateColumns: TObjectList;
    FWhereColumns: TObjectList;

    FInsertParams: TObjectList;
    FUpdateParams: TObjectList;
    FDeleteParams: TObjectList;

    FCalcDefaults: Boolean;
    FWhereAll: Boolean;
    FUpdateAll: Boolean;

    FStatements : TZHashMap;
  protected
    InsertStatement: IZPreparedStatement;
    UpdateStatement: IZPreparedStatement;
    DeleteStatement: IZPreparedStatement;
    RefreshResultSet: IZResultSet;

    procedure CopyResolveParameters({$IFDEF AUTOREFCOUNT}const {$ENDIF}FromList, ToList: TObjectList);
    function ComposeFullTableName(const Catalog, Schema, Table: SQLString;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter): SQLString;
    function DefineTableName: SQLString;

    function CreateResolverStatement(const SQL : String): IZPreparedStatement;
    procedure SetResolverStatementParamters(const Statement: IZStatement;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TStrings); virtual;

    procedure DefineCalcColumns({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}RowAccessor: TZRowAccessor);
    procedure DefineInsertColumns({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList);
    procedure DefineUpdateColumns({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure DefineWhereKeyColumns({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList);
    procedure DefineWhereAllColumns({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      IgnoreKeyColumn: Boolean = False);
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; virtual;

    procedure FillStatement(const Statement: IZPreparedStatement;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor);

    property Connection: IZConnection read FConnection write FConnection;
    property Metadata: IZResultSetMetadata read FMetadata write FMetadata;
    property DatabaseMetadata: IZDatabaseMetadata read FDatabaseMetadata
      write FDatabaseMetadata;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;

    property InsertColumns: TObjectList read FInsertColumns;
    property UpdateColumns: TObjectList read FUpdateColumns;
    property WhereColumns: TObjectList read FWhereColumns;

    property CalcDefaults: Boolean read FCalcDefaults write FCalcDefaults;
    property WhereAll: Boolean read FWhereAll write FWhereAll;
    property UpdateAll: Boolean read FUpdateAll write FUpdateAll;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;

    procedure FormWhereClause({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor: TZRowAccessor; var Result: SQLString); virtual;
    function FormInsertStatement({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}{%H-}NewRowAccessor: TZRowAccessor): SQLString;
    function FormUpdateStatement({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor): SQLString; virtual;
    function FormDeleteStatement(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): SQLString;
    function FormCalculateStatement(Columns: TObjectList): SQLString; virtual;

    procedure CalculateDefaults(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
    procedure PostUpdates(const Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor); virtual;
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); virtual;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor); //FOS+ 07112006
  end;

implementation

uses ZMessages, ZSysUtils, ZDbcMetadata, ZDbcUtils, ZDbcProperties
  {$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

{ TZResolverParameter }

{**
  Constructs this resolver parameter and assignes the main properties.
  @param ColumnIndex a result set column index.
  @param ColumnName a result set column name.
  @param NewValue <code>True</code> for new value and <code>False</code>
    for an old one.
  @param DefaultValue a default column value to evalute on server.
}
constructor TZResolverParameter.Create(ColumnIndex: Integer;
  const ColumnName: string; ColumnType: TZSQLType; NewValue: Boolean; const DefaultValue: string);
begin
  FColumnType := ColumnType;
  FColumnIndex := ColumnIndex;
  FColumnName := ColumnName;
  FNewValue := NewValue;
  FDefaultValue := DefaultValue;
end;

{ TZGenericCachedResolver }

{**
  Creates a cached resolver and assignes the main properties.
  @param ResultSet a related ResultSet object.
}
constructor TZGenericCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  FStatement := Statement;
  FConnection := Statement.GetConnection;
  FMetadata := Metadata;
  FDatabaseMetadata := Statement.GetConnection.GetMetadata;
  FIdentifierConvertor := FDatabaseMetadata.GetIdentifierConvertor;

  FInsertColumns := TObjectList.Create(True);
  FWhereColumns := TObjectList.Create(True);
  FUpdateColumns := TObjectList.Create(True);

  FInsertParams := TObjectList.Create(True);
  FUpdateParams := TObjectList.Create(True);
  FDeleteParams := TObjectList.Create(True);

  FCalcDefaults := StrToBoolEx(DefineStatementParameter(Statement,
    DSProps_Defaults, 'true'));
  FUpdateAll := UpperCase(DefineStatementParameter(Statement,
    DSProps_Update, 'changed')) = 'ALL';
  FWhereAll := UpperCase(DefineStatementParameter(Statement,
    DSProps_Where, 'keyonly')) = 'ALL';
  FStatements := TZHashMap.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZGenericCachedResolver.Destroy;
procedure FlustStmt(var Stmt: IZPreparedStatement);
begin
  if Stmt <> nil then begin
    Stmt.Close;
    Stmt := nil
  end;
end;

begin
  FMetadata := nil;
  FDatabaseMetadata := nil;

  FreeAndNil(FInsertColumns);
  FreeAndNil(FUpdateColumns);
  FreeAndNil(FWhereColumns);

  FreeAndNil(FInsertParams);
  FreeAndNil(FUpdateParams);
  FreeAndNil(FDeleteParams);

  FreeAndNil(FStatements);
  FlustStmt(InsertStatement);
  FlustStmt(UpdateStatement);
  FlustStmt(DeleteStatement);
  if RefreshResultSet <> nil then
     RefreshResultSet.Close;
  inherited Destroy;
end;

{**
  Copies resolver parameters from source list to destination list.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure TZGenericCachedResolver.CopyResolveParameters(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}FromList, ToList: TObjectList);
var
  I: Integer;
  Current: TZResolverParameter;
begin
  for I := 0 to FromList.Count - 1 do
  begin
    Current := TZResolverParameter(FromList[I]);
    if Current.ColumnName <> '' then
      ToList.Add(TZResolverParameter.Create(Current.ColumnIndex,
        Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
  end;
end;

{**
  Composes a fully quilified table name.
  @param Catalog a table catalog name.
  @param Schema a table schema name.
  @param Table a table name.
  @return a fully qualified table name.
}
function TZGenericCachedResolver.ComposeFullTableName(const Catalog, Schema,
  Table: SQLString; {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter): SQLString;
var tmp: SQLString;
begin
  Result := '';
  if Table <> '' then begin
    if (Catalog <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsCatalogsInDataManipulation then begin
      Tmp := IdentifierConvertor.Quote(Catalog);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar('.', Result);
    end;
    if (Schema <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsSchemasInDataManipulation then begin
      Tmp := IdentifierConvertor.Quote(Schema);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar('.', Result);
    end;
    Tmp := IdentifierConvertor.Quote(Table);
    SQLWriter.AddText(Tmp, Result);
    SQLWriter.Finalize(Result);
  end;
end;

{**
  Defines a table name from the select statement.
}
function TZGenericCachedResolver.DefineTableName: SQLString;
var
  I: Integer;
  Temp: string;
  SQLWriter: TZSQLStringWriter;
begin
  Result := '';
  SQLWriter := TZSQLStringWriter.Create(512);
  try
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      Temp := ComposeFullTableName(Metadata.GetCatalogName(I),
        Metadata.GetSchemaName(I), Metadata.GetTableName(I), SQLWriter);
      if (Result = '') and (Temp <> '') then
        Result := Temp
      else if (Result <> '') and (Temp <> '') and (Temp <> Result) then
        raise EZSQLException.Create(SCanNotUpdateComplexQuery);
    end;
    if Result = '' then
      raise EZSQLException.Create(SCanNotUpdateThisQueryType);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

function TZGenericCachedResolver.CreateResolverStatement(const SQL: String): IZPreparedStatement;
var
  Temp : TStrings;
begin
  Temp := TStringList.Create;
  Result := nil;
  try
    SetResolverStatementParamters(FStatement, Temp);
    Result := Connection.PrepareStatementWithParams(SQL, Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Gets a collection of data columns for INSERT statements.
  @param Columns a collection of columns.
}
procedure TZGenericCachedResolver.DefineInsertColumns(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList);
var
  I: Integer;
begin
  { Precache insert parameters. }
  if InsertColumns.Count = 0 then
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and (Metadata.GetColumnName(I) <> '') and Metadata.IsWritable(I)
      then InsertColumns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
  { Use cached insert parameters }
  CopyResolveParameters(InsertColumns, Columns);
end;

{**
  Gets a collection of data columns for UPDATE statements.
  @param Columns a collection of columns.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.DefineUpdateColumns(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor);
var I: Integer;
begin
  { Use precached parameters. }
  if UpdateAll and (UpdateColumns.Count > 0) then begin
    CopyResolveParameters(UpdateColumns, Columns);
    Exit;
  end;

  { Defines parameters for UpdateAll mode. }
  if UpdateAll then begin
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and
         (Metadata.GetColumnName(I) <> '') and Metadata.IsWritable(I) then
        UpdateColumns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
    CopyResolveParameters(UpdateColumns, Columns);
  end else { Defines parameters for UpdateChanged mode. }
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and
         (Metadata.GetColumnName(I) <> '') and Metadata.IsWritable(I) and
         (OldRowAccessor.CompareBuffer(OldRowAccessor.RowBuffer,
          NewRowAccessor.RowBuffer, I, NewRowAccessor.GetCompareFunc(I, ckEquals))  <> 0) then
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
end;

{**
  Gets a collection of where key columns for DELETE or UPDATE DML statements.
  @param Columns a collection of key columns.
}
procedure TZGenericCachedResolver.DefineWhereKeyColumns(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList);

  function AddColumn(const Table, ColumnName: string; WhereColumns: TObjectList): Boolean;
  var
    I: Integer;
  begin
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (ColumnName = Metadata.GetColumnName(I)) and (Table = Metadata.GetTableName(I)) then begin
        WhereColumns.Add(TZResolverParameter.Create(I, ColumnName,
          Metadata.GetColumnType(I), False, ''));
        Result := True;
        Exit;
      end;
    WhereColumns.Clear;
    Result := False;
  end;

var
  I: Integer;
  KeyFields: string;
  Catalog, Schema, Table: string;
  PrimaryKeys: IZResultSet;
  Fields: TStrings;
begin
  { Use precached values. }
  if WhereColumns.Count > 0 then begin
    CopyResolveParameters(WhereColumns, Columns);
    Exit;
  end;
  Table := '';
  { Defines catalog, schema and a table. }
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
    Table := Metadata.GetTableName(I);
    if Table <> '' then begin
      Schema := Metadata.GetSchemaName(I);
      Catalog := Metadata.GetCatalogName(I);
      Break;
    end;
  end;

  { Tryes to define primary keys. }
  if not WhereAll then begin
    KeyFields := FStatement.GetParameters.Values[DSProps_KeyFields];
    { Let user define key fields }
    if KeyFields <> '' then begin
      Fields := ExtractFields(KeyFields, [',', ';']);
      try
        for I := 0 to Fields.Count - 1 do
          if not AddColumn(Table, Fields[I], WhereColumns) then
            Break;
      finally
        Fields.Free;
      end;
    end else begin { Ask DB for key fields }
      {For exact results: quote all identifiers SEE: http://sourceforge.net/p/zeoslib/tickets/81/
      If table names have mixed case ConstructNameCondition will return wrong results
      and we fall back to WhereAll}
      PrimaryKeys := DatabaseMetadata.GetPrimaryKeys(IdentifierConvertor.Quote(Catalog),
        IdentifierConvertor.Quote(Schema), IdentifierConvertor.Quote(Table));
      while PrimaryKeys.Next do
        if not AddColumn(Table, PrimaryKeys.GetString(ColumnNameIndex), WhereColumns) then
          Break;
    end;
  end;

  if WhereColumns.Count > 0
  then CopyResolveParameters(WhereColumns, Columns)
  else DefineWhereAllColumns(Columns);
end;

{**
  Gets a collection of where all columns for DELETE or UPDATE DML statements.
  @param Columns a collection of key columns.
}
procedure TZGenericCachedResolver.DefineWhereAllColumns(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  IgnoreKeyColumn: Boolean = False);
var
  I: Integer;
begin
  { Use precached values. }
  if WhereColumns.Count > 0 then begin
    CopyResolveParameters(WhereColumns, Columns);
    Exit;
  end;

  { Takes a key all non-blob fields. }
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if CheckKeyColumn(I)
    then WhereColumns.Add(TZResolverParameter.Create(I,
        Metadata.GetColumnName(I), Metadata.GetColumnType(I), False, ''))
    else if IgnoreKeyColumn then
      WhereColumns.Add(TZResolverParameter.Create(I,
        Metadata.GetColumnName(I), Metadata.GetColumnType(I), False, ''));
  if ( WhereColumns.Count = 0 ) and ( not IgnoreKeyColumn )
  then DefineWhereAllColumns(Columns, True)
  else CopyResolveParameters(WhereColumns, Columns);
end;

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZGenericCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stAsciiStream, stBinaryStream, stUnicodeStream]);
end;

{**
  Gets a collection of data columns to initialize before INSERT statements.
  @param Columns a collection of columns.
  @param RowAccessor an accessor object to column values.
}
procedure TZGenericCachedResolver.DefineCalcColumns(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}RowAccessor: TZRowAccessor);
var
  I: Integer;
begin
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if RowAccessor.IsNull(I) and (Metadata.GetTableName(I) <> '')
      and ((Metadata.GetDefaultValue(I) <> '') or (RowAccessor.GetColumnDefaultExpression(I) <> '')) then
      // DefaultExpression takes takes precedence on database default value
      if RowAccessor.GetColumnDefaultExpression(I) <> '' then
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I),
          True, RowAccessor.GetColumnDefaultExpression(I)))
      else
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I),
          True, Metadata.GetDefaultValue(I)));
end;

{**
  Fills the specified statement with stored or given parameters.
  @param ResultSet a source result set object.
  @param Statement a DBC statement object.
  @param Config an UpdateStatement configuration.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.FillStatement(const Statement: IZPreparedStatement;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I: Integer;
  ColumnIndex: Integer;
  Current: TZResolverParameter;
  RowAccessor: TZRowAccessor;
  WasNull: Boolean;
  BCD: TBCD; //one val on stack 4 all
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
  G: TGUID absolute BCD;
begin
  WasNull := False;
  for I := 0 to Params.Count - 1 do
  begin
    Current := TZResolverParameter(Params[I]);
    if Current.NewValue then
      RowAccessor := NewRowAccessor
    else
      RowAccessor := OldRowAccessor;
    ColumnIndex := Current.ColumnIndex;

    if RowAccessor.IsNull(ColumnIndex) then
      Statement.SetNull(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Metadata.GetColumnType(ColumnIndex))
    else case Metadata.GetColumnType(ColumnIndex) of
      stBoolean:
        Statement.SetBoolean(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetBoolean(ColumnIndex, WasNull));
      stByte:
        Statement.SetByte(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetByte(ColumnIndex, WasNull));
      stShort:
        Statement.SetShort(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetShort(ColumnIndex, WasNull));
      stWord:
        Statement.SetWord(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetWord(ColumnIndex, WasNull));
      stSmall:
        Statement.SetSmall(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetSmall(ColumnIndex, WasNull));
      stLongWord:
        Statement.SetUInt(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetUInt(ColumnIndex, WasNull));
      stInteger:
        Statement.SetInt(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetInt(ColumnIndex, WasNull));
      stULong:
        Statement.SetULong(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetULong(ColumnIndex, WasNull));
      stLong:
        Statement.SetLong(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetLong(ColumnIndex, WasNull));
      stFloat:
        Statement.SetFloat(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetFloat(ColumnIndex, WasNull));
      stCurrency:
        Statement.SetCurrency(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetCurrency(ColumnIndex, WasNull));
      stDouble:
        Statement.SetDouble(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetDouble(ColumnIndex, WasNull));
      stBigDecimal: begin
                      RowAccessor.GetBigDecimal(ColumnIndex, BCD{%H-}, WasNull);
                      Statement.SetBigDecimal(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BCD);
                    end;
      stString, stUnicodeString:
        Statement.SetCharRec(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetCharRec(ColumnIndex, WasNull));
      stBytes:
        Statement.SetBytes(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetBytes(ColumnIndex, WasNull));
      stGUID: begin
                RowAccessor.GetGUID(ColumnIndex, G{%H-}, WasNull);
                Statement.SetGuid(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, G);
              end;
      stDate: begin
                RowAccessor.GetDate(ColumnIndex, WasNull, D);
                Statement.SetDate(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, D);
              end;
      stTime: begin
                RowAccessor.GetTime(ColumnIndex, WasNull, T);
                Statement.SetTime(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
              end;
      stTimestamp: begin
                RowAccessor.GetTimestamp(ColumnIndex, WasNull, TS);
                Statement.SetTimestamp(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS);
              end;
      stAsciiStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stAsciiStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
      stUnicodeStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnicodeStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
      stBinaryStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBinaryStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
    end;
  end;
end;

{**
  Forms a where clause for UPDATE or DELETE DML statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
procedure TZGenericCachedResolver.FormWhereClause(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, N: Integer;
  Current: TZResolverParameter;
  Condition: SQLString;
begin
  N := Columns.Count - WhereColumns.Count;

  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do begin
    Current := TZResolverParameter(WhereColumns[I]);
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    Condition := IdentifierConvertor.Quote(Current.ColumnName);
    SQLWriter.AddText(Condition, Result);
    if OldRowAccessor.IsNull(Current.ColumnIndex) then begin
      SQLWriter.AddText(' IS NULL', Result);
      Columns.Delete(N);
    end else begin
      SQLWriter.AddText('=?', Result);
      Inc(N);
    end;
  end;
end;

{**
  Forms a INSERT statements.
  @param Columns a collection of key columns.
  @param NewRowAccessor an accessor object to new column values.
}
function TZGenericCachedResolver.FormInsertStatement(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}NewRowAccessor: TZRowAccessor): SQLString;
var
  I: Integer;
  Tmp: SQLString;
  // NB: INSERT..RETURNING is only aclual for several drivers so we must ensure
  // this unit is compilable with all these drivers disabled.
  {$IF DECLARED(DSProps_InsertReturningFields)}
  Fields: TStrings;
  {$IFEND}
  SQLWriter: TZSQLStringWriter;
begin
  SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
  Result := 'INSERT INTO ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    DefineInsertColumns(Columns);
    if Columns.Count = 0 then begin
      Result := '';
      Exit;
    end;

    SQLWriter.AddChar(' ', Result);
    SQLWriter.AddChar('(', Result);
    for I := 0 to Columns.Count - 1 do begin
      if I > 0 then
        SQLWriter.AddChar(',', Result);
      Tmp := IdentifierConvertor.Quote(TZResolverParameter(Columns[I]).ColumnName);
      SQLWriter.AddText(Tmp, Result);
    end;
    SQLWriter.AddText(') VALUES (', Result);
    for I := 0 to Columns.Count - 1 do begin
      if I > 0 then
        SQLWriter.AddChar(',', Result);
      SQLWriter.AddChar('?', Result);
    end;
    SQLWriter.AddChar(')', Result);

    {$IF DECLARED(DSProps_InsertReturningFields)}
    Tmp := FStatement.GetParameters.Values[DSProps_InsertReturningFields];
    if Tmp <> '' then begin
      SQLWriter.AddText(' RETURNING ', Result);
      Fields := ExtractFields(Tmp, [',', ';']);
      for I := 0 to Fields.Count - 1 do begin
        if I > 0 then
          SQLWriter.AddChar(',', Result);
        Tmp := IdentifierConvertor.Quote(Fields[I]);
        SQLWriter.AddText(Tmp, Result);
      end;
      Fields.Free;
    end;
    {$IFEND}
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

{**
  Forms an UPDATE statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
function TZGenericCachedResolver.FormUpdateStatement(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor, NewRowAccessor: TZRowAccessor): SQLString;
var
  I: Integer;
  Current: TZResolverParameter;
  Temp: SQLString;
  SQLWriter: TZSQLStringWriter;
begin
  SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
  Result := 'UPDATE ';
  try
    Temp := DefineTableName;
    DefineUpdateColumns(Columns, OldRowAccessor, NewRowAccessor);
    if Columns.Count = 0 then begin
      Result := '';
      Exit;
    end;
    SQLWriter.AddText(Temp, Result);
    SQLWriter.AddText(' SET ', Result);
    for I := 0 to Columns.Count - 1 do begin
      Current := TZResolverParameter(Columns[I]);
      if I > 0 then
        SQLWriter.AddChar(',', Result);
      Temp := IdentifierConvertor.Quote(Current.ColumnName);
      SQLWriter.AddText(Temp, Result);
      SQLWriter.AddText('=?', Result);
    end;

    DefineWhereKeyColumns(Columns);
    FormWhereClause(Columns, SQLWriter, OldRowAccessor, Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

{**
  Forms a where clause for DELETE statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZGenericCachedResolver.FormDeleteStatement(Columns: TObjectList;
  OldRowAccessor: TZRowAccessor): SQLString;
var
  SQLWriter: TZSQLStringWriter;
  Tmp: SQLString;
begin
  SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
  Result := 'DELETE FROM ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    DefineWhereKeyColumns(Columns);
    FormWhereClause(Columns, SQLWriter, OldRowAccessor, Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
    Tmp := '';
  end;
end;

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZGenericCachedResolver.FormCalculateStatement(
  Columns: TObjectList): SQLString;
var
  I: Integer;
  Current: TZResolverParameter;
  SQLWriter: TZSQLStringWriter;
begin
  if Columns.Count = 0 then
    Result := ''
  else begin
    Result := 'SELECT ';
    SQLWriter := TZSQLStringWriter.Create(512+(Columns.Count shl 5));
    for I := 0 to Columns.Count - 1 do begin
      Current := TZResolverParameter(Columns[I]);
      if I > 0 then
         SQLWriter.AddChar(',', Result);
      if Current.DefaultValue <> ''
      then SQLWriter.AddText(Current.DefaultValue, Result)
      else SQLWriter.AddText('NULL', Result);
    end;
    SQLWriter.Finalize(Result);
    FreeAndNil(SQLWriter);
  end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement            : IZPreparedStatement;
  S                    : string;
  SQL                  : string;
  SQLParams            : TObjectList;
  lUpdateCount         : Integer;
  lValidateUpdateCount : Boolean;
  TempKey              : IZAnyValue;
  SenderStatement      : IZStatement;
  {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
  function CreateInvalidUpdateCountException: EZSQLException; //suppress _U/LStrArrClear
  begin
    Result := EZSQLException.Create(Format(SInvalidUpdateCount, [lUpdateCount]));
  end;
  {$ENDIF WITH_VALIDATE_UPDATE_COUNT}
begin
  if (UpdateType = utDeleted) and (OldRowAccessor.RowBuffer.UpdateType = utInserted) then
    Exit;

  case UpdateType of
    utInserted:
      begin
        if InsertStatement = nil then begin
          SQL := FormInsertStatement(FInsertParams, NewRowAccessor);
          InsertStatement := CreateResolverStatement(SQL);
          Statement := InsertStatement;
        end;
        Statement := InsertStatement;
        SQLParams := FInsertParams;
      end;
    utDeleted:
      begin
        if not FWhereAll then begin
          If DeleteStatement = nil then begin
            SQL := FormDeleteStatement(FDeleteParams, OldRowAccessor);
            DeleteStatement := CreateResolverStatement(SQL);
          end;
          Statement := DeleteStatement;
          SQLParams := FDeleteParams;
        end else begin
          FDeleteParams.Clear;  //EH: where columns propably are cached after 1. call
          SQL := FormDeleteStatement(FDeleteParams, OldRowAccessor);
          if SQL = '' then Exit;
          TempKey := TZAnyValue.CreateWithInteger(Hash(SQL));
          Statement := FStatements.Get(TempKey) as IZPreparedStatement;
          If Statement = nil then begin
            Statement := CreateResolverStatement(SQL);
            FStatements.Put(TempKey, Statement);
          end;
          SQLParams := FDeleteParams;
        end;
      end;
    utModified:
      begin
        FUpdateParams.Clear;  //EH: where columns propably are cached after 1. call
        //now what's faster?: caching stmts too by using a hashmap or recreate always
        //first of all: we need the new command-stmt
        SQL := FormUpdateStatement(FUpdateParams, OldRowAccessor, NewRowAccessor);
        If SQL = '' then exit;// no fields have been changed
        TempKey := TZAnyValue.CreateWithInteger(Hash(SQL));
        UpdateStatement := FStatements.Get(TempKey) as IZPreparedStatement;
        If UpdateStatement = nil then begin
          UpdateStatement := CreateResolverStatement(SQL);
          FStatements.Put(TempKey, UpdateStatement);
        end;
        Statement := UpdateStatement;
        SQLParams := FUpdateParams;
      end;
    else
      Exit;
  end;

  FillStatement(Statement, SQLParams, OldRowAccessor, NewRowAccessor);
  // if Property ValidateUpdateCount isn't set : assume it's true
  SenderStatement := Sender.GetStatement;
  if Assigned(SenderStatement) then begin
    S := SenderStatement.GetParameters.Values[DSProps_ValidateUpdateCount];
    lValidateUpdateCount := (S = '') or StrToBoolEx(S);
  end else begin
    lValidateUpdateCount := true;
  end;

  lUpdateCount := Statement.ExecuteUpdatePrepared;
  {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
  if  (lValidateUpdateCount) and (lUpdateCount <> 1   ) then
    raise CreateInvalidUpdateCountException;
  {$ENDIF}
end;

{$IFDEF FPC} {$PUSH} //rolling eyes
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$ENDIF}
procedure TZGenericCachedResolver.RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
var Stmt: IZPreparedStatement;
    I, upd_idx: Integer;
    Len: NativeUInt;
    BCD: TBCD; //one val on stack 4 all
    UID: TGUID absolute BCD;
    TS: TZTimeStamp absolute BCD;
    D: TZDate absolute BCD;
    T: TZTime absolute BCD;
    SQLType: TZSQLType;
    i32: Integer absolute BCD;
    c32: Cardinal absolute BCD;
    i64: Int64 absolute BCD;
    U64: UInt64 absolute BCD;
    Dbl: Double absolute BCD;
    P: Pointer absolute BCD;
    C: Currency absolute BCD;
    S: Single absolute BCD;
    L: IZBlob absolute BCD;
  procedure InitStmt(out Stmt: IZPreparedStatement);
  var
    I: Integer;
    Current: TZResolverParameter;
    SQL, Temp: SQLString;
    SQLWriter: TZSQLStringWriter;
    Columns: TObjectList;
  begin
    Stmt := nil;
    SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
    Columns := TObjectList.Create(True);
    try
      DefineInsertColumns(Columns);
      if Columns.Count = Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} then //no aggregates or casted columns
        SQL := 'SELECT ';

      for I := 0 to Columns.Count - 1 do begin
        Current := TZResolverParameter(Columns[I]);
        if I > 0 then
          SQLWriter.AddChar(',', SQL);
        Temp := IdentifierConvertor.Quote(Current.ColumnName);
        SQLWriter.AddText(Temp, SQL);
      end;
      SQLWriter.AddText(' FROM ', SQL);
      Temp := DefineTableName;
      SQLWriter.AddText(Temp, SQL);

      DefineWhereKeyColumns(Columns);
      if WhereColumns.Count = 0 then Exit;
      FormWhereClause(Columns, SQLWriter, RowAccessor, SQL);
      SQLWriter.Finalize(SQL);
      Stmt := CreateResolverStatement(SQL);
    finally
      FreeAndNil(SQLWriter);
      FreeAndNil(Columns);
    end;
  end;
begin
  if (RefreshResultSet = nil) or (RefreshResultSet.GetStatement = nil)
  then InitStmt(Stmt)
  else RefreshResultSet.GetStatement.QueryInterface(IZPreparedStatement, Stmt);
  if Stmt = nil then
    raise EZSQLException.Create(SUpdateSQLNoResult)
  else begin
    FillStatement(Stmt, WhereColumns, RowAccessor, RowAccessor);
    RefreshResultSet := Stmt.ExecuteQueryPrepared;
    if (RefreshResultSet = nil) or not RefreshResultSet.Next then
      raise EZSQLException.Create(SUpdateSQLNoResult);
    for I := FirstDbcIndex to FInsertColumns.Count {$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      upd_idx := TZResolverParameter(FInsertColumns[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).FColumnIndex;
      if RefreshResultSet.IsNull(upd_idx)
      then RowAccessor.SetNull(upd_idx)
      else begin
        SQLType := RowAccessor.GetColumnType(upd_idx);
        case SQLType of
          stBoolean: RowAccessor.SetBoolean(upd_idx, RefreshResultSet.GetBoolean(I));
          stByte, stWord, stLongWord: begin
                c32 := RefreshResultSet.GetUInt(I);
                RowAccessor.SetUInt(upd_idx, c32);
              end;
          stShort, stSmall, stInteger: begin
                i32 := RefreshResultSet.GetInt(I);
                RowAccessor.SetInt(upd_idx, i32);
              end;
          stULong: begin
                u64 := RefreshResultSet.GetULong(I);
                RowAccessor.SetULong(upd_idx, u64);
              end;
          stLong: begin
                i64 := RefreshResultSet.GetLong(I);
                RowAccessor.SetLong(upd_idx, i64);
              end;
          stFloat: begin
                S := RefreshResultSet.GetFloat(I);
                RowAccessor.SetFloat(upd_idx, S);
              end;
          stDouble: begin
                Dbl := RefreshResultSet.GetDouble(I);
                RowAccessor.SetDouble(upd_idx, Dbl);
              end;
          stCurrency: begin
                C := RefreshResultSet.GetCurrency(I);
                RowAccessor.SetCurrency(upd_idx, C);
              end;
          stBigDecimal: begin
                RefreshResultSet.GetBigDecimal(I, BCD{%H-});
                RowAccessor.SetBigDecimal(upd_idx, BCD);
              end;
          stGUID: begin
                RefreshResultSet.GetGUID(I, UID);
                RowAccessor.SetGUID(upd_idx, UID);
              end;
          stString, stUnicodeString:
              if RowAccessor.IsRaw then begin
                P := RefreshResultSet.GetPAnsiChar(I, Len);
                RowAccessor.SetPAnsiChar(upd_idx, P, Len);
              end else begin
                P := RefreshResultSet.GetPWideChar(I, Len);
                RowAccessor.SetPWideChar(upd_idx, P, Len);
              end;
          stBytes: begin
                P := RefreshResultSet.GetBytes(I, Len);
                RowAccessor.SetBytes(upd_idx, P, Len);
              end;
          stDate: begin
              RefreshResultSet.GetDate(I, D);
              RowAccessor.SetDate(upd_idx, D);
            end;
          stTime: begin
              RefreshResultSet.GetTime(I, T);
              RowAccessor.SetTime(upd_idx, T);
            end;
          stTimestamp: begin
              RefreshResultSet.GetTimestamp(I, TS);
              RowAccessor.SetTimestamp(upd_idx, TS);
            end;
          stAsciiStream, stUnicodeStream, stBinaryStream: begin
              P := nil; //avoid gpf
              L := RefreshResultSet.GetBlob(I);
              RowAccessor.SetBlob(upd_idx, L);
              L := nil;
            end;
        end;
        if RefreshResultSet.WasNull then //if a convasion failed .. MySQL?
          RowAccessor.SetNull(upd_idx);
      end;
    end;
    RefreshResultSet.ResetCursor; //unlock handles
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZGenericCachedResolver.SetResolverStatementParamters(
  const Statement: IZStatement; {$IFDEF AUTOREFCOUNT}const {$ENDIF} Params: TStrings);
begin
  Params.Assign(Statement.GetParameters);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
 {**
  Calculate default values for the fields.
  @param Sender a cached result set object.
  @param RowAccessor an accessor object to column values.
}
procedure TZGenericCachedResolver.CalculateDefaults(
  const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
var
  I: Integer;
  SQL: string;
  SQLParams: TObjectList;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
  Current: TZResolverParameter;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
  G: TGUID absolute BCD;
  i32: Integer absolute BCD;
  c32: Cardinal absolute BCD;
  i64: Int64 absolute BCD;
  U64: UInt64 absolute BCD;
  Dbl: Double absolute BCD;
  P: Pointer;
  C: Currency absolute BCD;
  S: Single absolute BCD;
begin
  if not FCalcDefaults then
     Exit;

  SQLParams := TObjectList.Create(True);
  try
    DefineCalcColumns(SQLParams, RowAccessor);
    if SQLParams.Count = 0 then
       Exit;
    SQL := FormCalculateStatement(SQLParams);
    if SQL = '' then
       Exit;

    { Executes statement and fills default fields. }
    Statement := Connection.CreateStatement;
    try
      ResultSet := Statement.ExecuteQuery(SQL);
      if ResultSet.Next then
      begin
        Metadata := ResultSet.GetMetadata;
        for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
        begin
          Current := TZResolverParameter(SQLParams[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]);
          try
            case Current.ColumnType of
              stBoolean: RowAccessor.SetBoolean(I, ResultSet.GetBoolean(I));
              stByte, stWord, stLongWord: begin
                                C32 := ResultSet.GetUInt(I);
                                RowAccessor.SetUInt(Current.ColumnIndex, C32);
                              end;
              stShort, stSmall, stInteger: begin
                                I32 := ResultSet.GetInt(I);
                                RowAccessor.SetInt(Current.ColumnIndex, i32);
                              end;
              stULong:        begin
                                u64 := ResultSet.GetULong(I);
                                RowAccessor.SetULong(Current.ColumnIndex, U64);
                              end;
              stLong:         begin
                                i64 := ResultSet.GetLong(I);
                                RowAccessor.SetLong(Current.ColumnIndex, i64);
                              end;
              stFloat:        begin
                                S := ResultSet.GetFloat(I);
                                RowAccessor.SetFloat(Current.ColumnIndex, S);
                              end;
              stDouble:       begin
                                Dbl := ResultSet.GetDouble(I);
                                RowAccessor.SetDouble(Current.ColumnIndex, Dbl);
                              end;
              stCurrency:     begin
                                C := ResultSet.GetCurrency(I);
                                RowAccessor.SetCurrency(Current.ColumnIndex, C);
                              end;
              stGUID:         begin
                                ResultSet.GetGUID(I, G);
                                RowAccessor.SetGUID(Current.ColumnIndex, G);
                              end;
              stBigDecimal:   begin
                                ResultSet.GetBigDecimal(I, BCD);
                                RowAccessor.SetBigDecimal(Current.ColumnIndex, BCD);
                              end;
              stString, stAsciiStream, stUnicodeString, stUnicodeStream:
                if RowAccessor.IsRaw then begin
                  P := ResultSet.GetPAnsiChar(I, Len);
                  RowAccessor.SetPAnsiChar(Current.ColumnIndex, P, Len)
                end else begin
                  P := ResultSet.GetPWideChar(I, Len);
                  RowAccessor.SetPWideChar(Current.ColumnIndex, P, Len);
                end;
              stBytes: RowAccessor.SetBytes(Current.ColumnIndex, ResultSet.GetBytes(I, Len), Len);
              stDate:         begin
                                ResultSet.GetDate(I, D);
                                RowAccessor.SetDate(Current.ColumnIndex, D);
                              end;
              stTime:         begin
                                ResultSet.GetTime(I, T);
                                RowAccessor.SetTime(Current.ColumnIndex, T);
                              end;
              stTimestamp:    begin
                                ResultSet.GetTimestamp(I, TS);
                                RowAccessor.SetTimestamp(Current.ColumnIndex, TS);
                              end;
            end;

            if ResultSet.WasNull then
              RowAccessor.SetNull(Current.ColumnIndex);
          except
            { Supress any errors in default fields. }
          end;
        end;
      end;
      ResultSet.Close;
    finally
      Statement.Close;
    end;
  finally
    FreeAndNil(SQLParams);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract base class - parameters not used intentionally
 procedure TZGenericCachedResolver.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
begin
 //Should be implemented at Specific database Level Cached resolver
end;
{$IFDEF FPC} {$POP} {$ENDIF} // abstract base class - parameters not used intentionally

{END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }

end.

