{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Generic Cached Resolver                  }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcGenericResolver;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZDbcIntfs, ZDbcCache, ZDbcCachedResultSet, ZCompatibility,
  ZSelectSchema, ZClasses, ZSysUtils;

type
  /// <summary>Implements an abstract resolver object</summary>
  TZAbstractCachedResolver = class (TInterfacedObject)
  protected
    Connection: IZConnection;
    Metadata: IZResultSetMetadata;
    RefreshResultSet: IZResultSet;
  end;

  /// <author>EgonHugeist<author>
  /// <summary>Defines a reference of a TZKeyAndPreparedStatmentPair record.</summary>
  PZKeyAndPreparedStatmentPair = ^TZKeyAndPreparedStatmentPair;
  /// <author>EgonHugeist<author>
  /// <summary>Defines a TZKeyAndPreparedStatmentPair record which holds the key
  ///  and statement interface</summary>
  TZKeyAndPreparedStatmentPair = record
    Key: NativeUInt;
    Statement: IZPreparedStatement;
  end;

  /// <author>EgonHugeist<author>
  /// <summary>Implements a list of TZKeyAndPreparedStatmentPairs.</summary>
  ZKeyAndPreparedStatementPairList = class(TZCustomElementList)
  protected
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    /// <summary>Adds an SQL and it's statment into the list</summary>
    /// <param>"Hash" the SQL hash used as a Key to be added.</param>
    /// <param>"Statement" the IZPreparedStatement object to be added.</param>
    procedure Add(Hash: NativeUInt; const Statement: IZPreparedStatement);
    /// <summary>Attampt to find an entry using the given SQL as key</summary>
    /// <param>"Hash" the SQL hash used as a Key to be added.</param>
    /// <returns>The address of the pair or nil if nothing was found.</returns>
    function Get(Hash: NativeUInt): PZKeyAndPreparedStatmentPair;
    /// <summary>Attampt to remove a pair from the list by key</summary>
    /// <param>"Hash" the SQL hash used as a Key to be added.</param>
    procedure Remove(Hash: NativeUInt);
  public
    /// <summary>Creates this object and assignes the main properties.</summary>
    constructor Create;
  end;

  { TZGenerateSQLCachedResolver }

  /// <summary>Implements a generic cached resolver object which generates
  ///  DML SQL statements and posts resultset updates to database.</summary>
  TZGenerateSQLCachedResolver = class (TZAbstractCachedResolver, IZCachedResolver,
    IZGenerateSQLCachedResolver)
  private
    FStatement : IZStatement;
    FTransaction: IZTransaction;
    FDatabaseMetadata: IZDatabaseMetadata;
    FIdentifierConverter: IZIdentifierConverter;

    FUpdateColumns: TZIndexPairList;
    FWhereColumns: TZIndexPairList;
    FCurrentWhereColumns: TZIndexPairList;

    FCalcDefaults: Boolean;
    FWhereAll: Boolean;
    FUpdateAll: Boolean;

  protected
    FUpdateStatements: ZKeyAndPreparedStatementPairList;
    FDeleteStatements: ZKeyAndPreparedStatementPairList;
    FInsertColumns: TZIndexPairList;
    FInsertStatements: ZKeyAndPreparedStatementPairList;
    InsertStatement: IZPreparedStatement;
    /// <summary>Composes a fully quilified table name.</summary>
    /// <param>"Catalog" a table catalog name.</param>
    /// <param>"Schema" a table schema name.</param>
    /// <param>"Table" a table name.</param>
    /// <returns>a fully qualified table name.</returns>
    function ComposeFullTableName(const Catalog, Schema, Table: SQLString;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter): SQLString;
    /// <summary>Defines a table name from the select statement.</summary>
    /// <returns>the table name.</returns>
    function DefineTableName: SQLString;
    /// <summary>Creates a statment for this resolver and assignes the properties.</summary>
    /// <param>"SQL" the SQL to be executed.</param>
    /// <returns>a parameter supported statement object.</returns>
    function CreateResolverStatement(const SQL : String): IZPreparedStatement;
    /// <summary>Assigns the resolver statement parameters from a source statement</summary>
    /// <param>"Statement" the source statement we copy the properties from.</param>
    procedure SetResolverStatementParamters(const Statement: IZStatement;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TStrings); virtual;
    /// <summary>Fills a collection of data columns for INSERT statements.</summary>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    procedure FillInsertColumnsPairList(NewRowAccessor: TZRowAccessor);
    /// <summary>Gets a collection of data columns for UPDATE statements.</summary>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    procedure FillUpdateColumns(const OldRowAccessor,NewRowAccessor: TZRowAccessor);
    /// <summary>Fills the collection of the where key columns for DELETE or
    ///  UPDATE DML statements.</summary>
    /// <param>"IncrementDestIndexBy" to increment the WhereColumnsLookup.</param>
    procedure FillWhereKeyColumns(IncrementDestIndexBy: Integer);
    /// <summary>Fills the collection of the where all columns for DELETE or
    ///  UPDATE DML statements.</summary>
    /// <param>"IncrementDestIndexBy" to increment the WhereColumnsLookup.</param>
    /// <param>"IgnoreKeyColumn" to determine if key columns are ignored</param>
    procedure FillWhereAllColumns(IncrementDestIndexBy: Integer;
      IgnoreKeyColumn: Boolean = False);
    /// <summary>Checks is the specified column can be used in where clause.</summary>
    /// <param>"ColumnIndex" an index of the column.</param>
    /// <returns><c>true</c> if column can be included into where clause.</returns>
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; virtual;
  protected
    /// <summary>Represents the DatabaseMetadata object</summary>
    property DatabaseMetadata: IZDatabaseMetadata read FDatabaseMetadata
      write FDatabaseMetadata;
    /// <summary>Represents the IdentifierConverter object</summary>
    property IdentifierConverter: IZIdentifierConverter
      read FIdentifierConverter write FIdentifierConverter;
    /// <summary>Represents the underlaying statement object</summary>
    property Statement: IZStatement read FStatement;
    /// <summary>Represents a list of current UPDATE columns used for the
    ///  statement bindings</summary>
    property UpdateColumnsLookup: TZIndexPairList read FUpdateColumns;
    /// <summary>Represents a cached list of all determined WhereColumns</summary>
    property WhereColumns: TZIndexPairList read FWhereColumns;
    /// <summary>Represents a list of current WHERE columns used for the
    ///  statement bindings</summary>
    property WhereColumnsLookup: TZIndexPairList read FCurrentWhereColumns;
    /// <summary>Represents if column defaults should be calculated</summary>
    property CalcDefaults: Boolean read FCalcDefaults write FCalcDefaults;
    /// <summary>Represents if all columns should be used in the where clause.</summary>
    property WhereAll: Boolean read FWhereAll write FWhereAll;
    /// <summary>Represents if all columns should be updated.</summary>
    property UpdateAll: Boolean read FUpdateAll write FUpdateAll;
  public
    /// <summary>Creates a cached resolver and assignes the main properties.</summary>
    /// <param>"Statement" the related ResultSet object.</param>
    /// <param>"Metadata" the related ResultSet metadata object.</param>
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>Forms a where clause for UPDATE or DELETE DML statements.</summary>
    /// <param>"SQLWriter" a TZSQLStringWriter object used for buffered writes</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"Result" a reference to the Result String the SQLWriter uses
    ///  for the buffered writes.</param>
    procedure FormWhereClause(const SQLWriter: TZSQLStringWriter;
      const OldRowAccessor: TZRowAccessor; var Result: SQLString); virtual;
    /// <summary>Forms an INSERT statement.</summary>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <returns>the composed insert SQL.</returns>
    function FormInsertStatement(NewRowAccessor: TZRowAccessor): SQLString; virtual;
    /// <summary>Forms an UPDATE statements.</summary>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <returns>the composed UPDATE SQL.</returns>
    function FormUpdateStatement(
      const OldRowAccessor, NewRowAccessor: TZRowAccessor): SQLString;
    /// <summary>Forms a DELETE statement.</summary>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <returns>the composed DELETE SQL.</returns>
    function FormDeleteStatement(const OldRowAccessor: TZRowAccessor): SQLString;
    /// <author>Michael Seeger</author>
    /// <summary>Forms a SELECT statements to calculate default values.</summary>
    /// <param>"RowAccessor" an accessor object to column values.</param>
    /// <param>"ColumnsLookup" an TZIndexPairList which holds the NULL columns.</param>
    /// <returns>the composed SELECT SQL.</returns>
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): SQLString; virtual;
  public //implement IZCachedResolver
    /// <author>Egonhugeist</author>
    /// <summary>Sets the appropriate transaction to this resolver object.</summary>
    /// <param>"Value" the transaction to be used.</param>
    procedure SetTransaction(const Value: IZTransaction); virtual;
    /// <author>Egonhugeist</author>
    /// <summary>Check if the underlaying transaction is in AutoCommit mode.</summary>
    /// <returns><c>True</c> if the transaction is in AutoCommit mode;
    ///  <c>False</c> otherwise.</returns>
    function HasAutoCommitTransaction: Boolean;
    /// <author>Michael Seeger</author>
    /// <summary>Calculate default values for the fields.</summary>
    /// <param>"Sender" a cached result set object.</param>
    /// <param>"RowAccessor" an accessor object to column values.</param>
    procedure CalculateDefaults(const Sender: IZCachedResultSet; const RowAccessor: TZRowAccessor);
    /// <summary>Posts updates to database.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"UpdateType" a type of updates.</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    procedure PostUpdates(const Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor); virtual;
    /// <summary>Posts updates the autoincrement fields.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"UpdateType" a type of updates.</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <param>"Resolver" the resolver object used to load the column data.</param>
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor;
      const Resolver: IZCachedResolver); virtual;
    /// <summary>Refreshes the current row.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"RowAccessor" an accessor object to current column values.</param>
    procedure RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor); //FOS+ 07112006
    /// <summary>Set the readonly state of a field. The value will be ignored
    ///  if the field is not writable.</summary>
    /// <param>"ColumnIndex" the columnnumber of the field.</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the dml's.</param>
    procedure SetReadOnly(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Set the searchable state of a field. The value will be ignored
    ///  if the field is not searchable at all e.g. LOB's.</summary>
    /// <param>"ColumnIndex" the columnnumber of the field.</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the where clause of the dml's.</param>
    procedure SetSearchable(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Set the Calculate null columns defaults.</summary>
    /// <param>"Value" <c>true</c> means calc defaults.</param>
    procedure SetCalcDefaults(Value: Boolean);
    /// <summary>Set the WhereAll state for generating the where clause of the
    ///  dml's. The value will be ignored if no indexfields are defined and
    ///  if no primary key is available. If both conditions are true the
    ///  whereAll mode is always true.</summary>
    /// <param>"Value" <c>true</c> means use all searchable columns. Otherwise
    ///  the primary key will or given index fields are used.</param>
    procedure SetWhereAll(Value: Boolean);
    /// <summary>Set the updateAll state for generating the dml's. <c>true</c>
    ///  means use all updatable columns. Otherwise only changed fields are used
    ///  for updates.</summary>
    /// <param>"Value" the UpdateAll mode should be used.</param>
    procedure SetUpdateAll(Value: Boolean);
  end;
  /// <summary>definines an alias for compatibility</summary>
  TZGenericCachedResolver = TZGenerateSQLCachedResolver;

implementation

uses ZMessages, ZDbcMetadata, ZDbcUtils, ZDbcProperties
  {$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

{ TZGenerateSQLCachedResolver }

constructor TZGenerateSQLCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  FStatement := Statement;
  Connection := Statement.GetConnection;
  Self.Metadata := Metadata;
  FDatabaseMetadata := Statement.GetConnection.GetMetadata;
  FIdentifierConverter := FDatabaseMetadata.GetIdentifierConverter;

  FInsertColumns := TZIndexPairList.Create;

  FUpdateColumns := TZIndexPairList.Create;
  FWhereColumns := TZIndexPairList.Create;
  FCurrentWhereColumns := TZIndexPairList.Create;

  FCalcDefaults := True;
  FUpdateStatements := ZKeyAndPreparedStatementPairList.Create;
  FDeleteStatements := ZKeyAndPreparedStatementPairList.Create;
  FInsertStatements := ZKeyAndPreparedStatementPairList.Create;
end;

destructor TZGenerateSQLCachedResolver.Destroy;
begin
  Metadata := nil;
  FDatabaseMetadata := nil;

  FreeAndNil(FInsertColumns);
  FreeAndNil(FUpdateColumns);
  FreeAndNil(FWhereColumns);
  FreeAndNil(FCurrentWhereColumns);

  FreeAndNil(FUpdateStatements);
  FreeAndNil(FDeleteStatements);
  FreeAndNil(FInsertStatements);

  if InsertStatement <> nil then begin
    InsertStatement.Close;
    InsertStatement := nil;
  end;
  if RefreshResultSet <> nil then begin
    RefreshResultSet.Close;
    RefreshResultSet := nil;
  end;
  inherited Destroy;
end;

function TZGenerateSQLCachedResolver.ComposeFullTableName(const Catalog, Schema,
  Table: SQLString; {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter): SQLString;
var tmp: SQLString;
begin
  Result := '';
  if Table <> '' then begin
    if (Catalog <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsCatalogsInDataManipulation then begin
      Tmp := IdentifierConverter.Quote(Catalog, iqCatalog);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar('.', Result);
    end;
    if (Schema <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsSchemasInDataManipulation then begin
      Tmp := IdentifierConverter.Quote(Schema, iqSchema);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar('.', Result);
    end;
    Tmp := IdentifierConverter.Quote(Table, iqTable);
    SQLWriter.AddText(Tmp, Result);
    SQLWriter.Finalize(Result);
  end;
end;

function TZGenerateSQLCachedResolver.DefineTableName: SQLString;
var
  I: Integer;
  Temp: string;
  SQLWriter: TZSQLStringWriter;
begin
  Result := '';
  SQLWriter := TZSQLStringWriter.Create(512);
  try
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      if not Metadata.IsReadOnly(I) and Metadata.IsWritable(I) then
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

function TZGenerateSQLCachedResolver.CreateResolverStatement(const SQL: String): IZPreparedStatement;
var
  Temp : TStrings;
begin
  Temp := TStringList.Create;
  Result := nil;
  try
    SetResolverStatementParamters(FStatement, Temp);
    if FTransaction <> nil
    then Result := FTransaction.GetConnection.PrepareStatementWithParams(SQL, Temp)
    else Result := Connection.PrepareStatementWithParams(SQL, Temp);
  finally
    Temp.Free;
  end;
end;

procedure TZGenerateSQLCachedResolver.FillUpdateColumns(
  const OldRowAccessor, NewRowAccessor: TZRowAccessor);
var I, j: Integer;
  IP: PZIndexPair;
begin
  FUpdateColumns.Clear;
  { Use precached parameters. }
  if FInsertColumns.Count = 0 then
    FillInsertColumnsPairList(NewRowAccessor);
  { Defines parameters for UpdateAll mode. }
  if UpdateAll then
    FUpdateColumns.Assign(FInsertColumns)
  else begin
    FUpdateColumns.Capacity := FInsertColumns.Count;
    J := FirstDbcIndex;
    for I := 0 to FUpdateColumns.Capacity-1 do begin
      IP := PZIndexPair(FInsertColumns[i]);
      if (OldRowAccessor.CompareBuffer(OldRowAccessor.RowBuffer,
         NewRowAccessor.RowBuffer, IP.ColumnIndex, NewRowAccessor.GetCompareFunc(IP.ColumnIndex, ckEquals))  <> 0) then begin
        FUpdateColumns.Add(J, IP.ColumnIndex);
        Inc(J);
      end;
    end;
  end;
end;

procedure TZGenerateSQLCachedResolver.FillWhereKeyColumns(IncrementDestIndexBy: Integer);

  function AddColumn(const Table, ColumnName: string; WhereColumns: TZIndexPairList): Boolean;
  var
    I: Integer;
  begin
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (ColumnName = Metadata.GetColumnName(I)) and (Table = Metadata.GetTableName(I)) then begin
        WhereColumns.Add(WhereColumns.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF},i);
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
  IndexPair: PZIndexPair;
label CopyParams;
begin
  { Use precached values. }
  WhereColumnsLookup.Clear;
  if (FWhereColumns.Count > 0) then
    goto CopyParams;
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
          if not AddColumn(Table, Fields[I], FWhereColumns) then
            Break;
      finally
        Fields.Free;
      end;
    end else begin { Ask DB for key fields }
      {For exact results: quote all identifiers SEE: http://sourceforge.net/p/zeoslib/tickets/81/
      If table names have mixed case ConstructNameCondition will return wrong results
      and we fall back to WhereAll}
      PrimaryKeys := DatabaseMetadata.GetPrimaryKeys(IdentifierConverter.Quote(Catalog, iqCatalog),
        IdentifierConverter.Quote(Schema, iqSchema), IdentifierConverter.Quote(Table, iqTable));
      while PrimaryKeys.Next do
        if not AddColumn(Table, PrimaryKeys.GetString(ColumnNameIndex), FWhereColumns) then
          Break;
    end;
  end;

  if FWhereColumns.Count = 0 then begin
    WhereAll := True;
    FillWhereAllColumns(IncrementDestIndexBy);
  end else begin
CopyParams:
    WhereColumnsLookup.Capacity := FWhereColumns.Count;
    for I := 0 to FWhereColumns.Count -1 do begin
      IndexPair := FWhereColumns[i];
      WhereColumnsLookup.Add(IndexPair.SrcOrDestIndex+IncrementDestIndexBy, IndexPair.ColumnIndex)
    end;
  end;
end;

procedure TZGenerateSQLCachedResolver.FillWhereAllColumns(
  IncrementDestIndexBy: Integer; IgnoreKeyColumn: Boolean = False);
var
  I: Integer;
  IndexPair: PZIndexPair;
begin
  { Use precached values. }
  if WhereColumns.Count = 0 then
    { Takes a key all non-blob fields. }
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if CheckKeyColumn(I)
      then WhereColumns.Add(FWhereColumns.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, I)
      else if IgnoreKeyColumn then
        WhereColumns.Add(FWhereColumns.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, I);
  if ( WhereColumns.Count = 0 ) and ( not IgnoreKeyColumn )
  then FillWhereAllColumns(IncrementDestIndexBy, True)
  else begin
    WhereColumnsLookup.Capacity := FWhereColumns.Count;
    for I := 0 to FWhereColumns.Count -1 do begin
      IndexPair := FWhereColumns[i];
      WhereColumnsLookup.Add(IndexPair.SrcOrDestIndex+IncrementDestIndexBy, IndexPair.ColumnIndex)
    end;
  end;
end;

function TZGenerateSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stAsciiStream, stBinaryStream, stUnicodeStream]);
end;

procedure TZGenerateSQLCachedResolver.FormWhereClause(
  const SQLWriter: TZSQLStringWriter; const OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, N, IDX: Integer;
  Condition: SQLString;
begin
  N := 0;
  if FWhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to FWhereColumns.Count - 1 do begin
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    IDX := PZIndexPair(FWhereColumns[i]).ColumnIndex;
    Condition := MetaData.GetColumnName(Idx);
    Condition := IdentifierConverter.Quote(Condition, iqColumn);
    SQLWriter.AddText(Condition, Result);
    if OldRowAccessor.IsNull(IDX) then begin
      SQLWriter.AddText(' IS NULL', Result);
      FCurrentWhereColumns.Delete(N);
      for IDX := N to FCurrentWhereColumns.Count -1 do
        Dec(PZIndexPair(FCurrentWhereColumns[IDX]).SrcOrDestIndex);
    end else begin
      SQLWriter.AddText('=?', Result);
      Inc(N);
    end;
  end;
end;

function TZGenerateSQLCachedResolver.HasAutoCommitTransaction: Boolean;
begin
  if FTransaction <> nil
  then Result := FTransaction.GetAutoCommit
  else Result := Connection.GetAutoCommit;
end;

function TZGenerateSQLCachedResolver.FormInsertStatement(
  NewRowAccessor: TZRowAccessor): SQLString;
var
  I, ColumnIndex: Integer;
  Tmp: SQLString;
  // NB: INSERT..RETURNING is only aclual for several drivers so we must ensure
  // this unit is compilable with all these drivers disabled.
  {$IF DECLARED(DSProps_InsertReturningFields)}
  Fields: TStrings;
  {$IFEND}
  SQLWriter: TZSQLStringWriter;
begin
  I := MetaData.GetColumnCount;
  SQLWriter := TZSQLStringWriter.Create(512+(I shl 5));
  Result := 'INSERT INTO ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    SQLWriter.AddChar(' ', Result);
    SQLWriter.AddChar('(', Result);
    if FInsertColumns.Count = 0 then
      FillInsertColumnsPairList(NewRowAccessor);
    if (FInsertColumns.Count = 0) and not
       {test for generated always cols }
       ((Metadata.GetColumnCount > 0) and Metadata.IsAutoIncrement(FirstDbcIndex)) then begin
      Result := '';
      Exit;
    end;
    for I := 0 to FInsertColumns.Count-1 do begin
      ColumnIndex := PZIndexPair(FInsertColumns[i])^.ColumnIndex;
      Tmp := Metadata.GetColumnName(ColumnIndex);
      Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);
    SQLWriter.AddText(' VALUES (', Result);
    for I := 0 to FInsertColumns.Count - 1 do begin
      SQLWriter.AddChar('?', Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);

    {$IF DECLARED(DSProps_InsertReturningFields)}
    Tmp := FStatement.GetParameters.Values[DSProps_InsertReturningFields];
    if Tmp <> '' then begin
      SQLWriter.AddText(' RETURNING ', Result);
      Fields := ExtractFields(Tmp, [',', ';']);
      for I := 0 to Fields.Count - 1 do begin
        if I > 0 then
          SQLWriter.AddChar(',', Result);
        Tmp := IdentifierConverter.Quote(Fields[I], iqColumn);
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

function TZGenerateSQLCachedResolver.FormUpdateStatement(
  const OldRowAccessor, NewRowAccessor: TZRowAccessor): SQLString;
var
  I, ColumnIndex: Integer;
  Temp: SQLString;
  SQLWriter: TZSQLStringWriter;

begin
  SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
  Result := 'UPDATE ';
  try
    Temp := DefineTableName;
    FillUpdateColumns(OldRowAccessor, NewRowAccessor);
    if FUpdateColumns.Count = 0 then begin
      Result := '';
      Exit;
    end;
    SQLWriter.AddText(Temp, Result);
    SQLWriter.AddText(' SET ', Result);
    for I := 0 to FUpdateColumns.Count - 1 do begin
      ColumnIndex := PZIndexPair(FUpdateColumns[i]).ColumnIndex;
      if I > 0 then
        SQLWriter.AddChar(',', Result);
      Temp := MetaData.GetColumnName(ColumnIndex);
      Temp := IdentifierConverter.Quote(Temp, iqColumn);
      SQLWriter.AddText(Temp, Result);
      SQLWriter.AddText('=?', Result);
    end;

    FillWhereKeyColumns(FUpdateColumns.Count);
    FormWhereClause(SQLWriter, OldRowAccessor, Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

function TZGenerateSQLCachedResolver.FormDeleteStatement(
  const OldRowAccessor: TZRowAccessor): SQLString;
var
  SQLWriter: TZSQLStringWriter;
  Tmp: SQLString;
begin
  SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
  Result := 'DELETE FROM ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    FillWhereKeyColumns(0);
    FormWhereClause(SQLWriter, OldRowAccessor, Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
    Tmp := '';
  end;
end;

procedure TZGenerateSQLCachedResolver.FillInsertColumnsPairList(NewRowAccessor: TZRowAccessor);
var I, J: Integer;
  Tmp: String;
begin
  FInsertColumns.Clear;
  FInsertColumns.Capacity := MetaData.GetColumnCount;
  J := FirstDbcIndex;
  for I := FirstDbcIndex to FInsertColumns.Capacity{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
    Tmp := Metadata.GetTableName(I);
    if (Tmp = '') or Metadata.IsReadOnly(I) or  not Metadata.IsWritable(I) or
       (Metadata.IsAutoIncrement(I) and NewRowAccessor.IsNull(I)) then continue;
    Tmp := Metadata.GetColumnName(I);
    if Tmp <> '' then begin
      FInsertColumns.Add(J, I);
      Inc(J);
    end;
  end;
end;

function TZGenerateSQLCachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): SQLString;
var
  I, ColumnIndex: Integer;
  SQLWriter: TZSQLStringWriter;
  S: String;
begin
  Result := 'SELECT ';
  SQLWriter := TZSQLStringWriter.Create(512+(ColumnsLookup.Count shl 5));
  try
    for I := 0 to ColumnsLookup.Count - 1 do begin
      ColumnIndex := PZIndexPair(ColumnsLookup[i]).ColumnIndex;
      S := RowAccessor.GetColumnDefaultExpression(ColumnIndex);
      if S = '' then S := Metadata.GetDefaultValue(ColumnIndex);
      if S = '' then S := 'NULL';
      SQLWriter.AddText(S, Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.CancelLastComma(Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

procedure TZGenerateSQLCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement            : IZPreparedStatement;
  SQL                  : string;
  lUpdateCount         : Integer;
  lValidateUpdateCount : Boolean;
  TempKey              : NativeUInt;
  SenderStatement      : IZStatement;
  Val                  : PZKeyAndPreparedStatmentPair;
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
        if (InsertStatement = nil) or InsertStatement.IsClosed or (FInsertColumns.Count = 0) then begin
          InsertStatement := nil;
          SQL := FormInsertStatement(NewRowAccessor);
          TempKey := Hash(SQL);
          Val := FInsertStatements.Get(TempKey);
          If (Val = nil) or Val.Statement.IsClosed then begin
            if (Val <> nil) then
              FInsertStatements.Remove(TempKey);
            InsertStatement := CreateResolverStatement(SQL);
            FInsertStatements.Add(TempKey, InsertStatement);
          end;
        end;
        Statement := InsertStatement;
        NewRowAccessor.FillStatement(Statement, FInsertColumns, Metadata);
      end;
    utDeleted:
      begin
        if not FWhereAll or (FWhereColumns.Count = 0) then begin
          If (FDeleteStatements.Count = 0) or PZKeyAndPreparedStatmentPair(FDeleteStatements[0]).Statement.IsClosed then begin
            FDeleteStatements.Count := 0;
            SQL := FormDeleteStatement(OldRowAccessor);
            TempKey := Hash(SQL);
            Statement := CreateResolverStatement(SQL);
            FDeleteStatements.Add(TempKey, Statement);
          end else begin
            //refill the FillWhereKeyColumns, the offsets might be changed by a
            //previous row update see: https://sourceforge.net/p/zeoslib/tickets/461/
            if PZIndexPair(WhereColumnsLookup[0]).SrcOrDestIndex <> FirstDbcIndex then
              FillWhereKeyColumns(0);
            Statement := PZKeyAndPreparedStatmentPair(FDeleteStatements[0]).Statement;
          end;
        end else begin
          SQL := FormDeleteStatement(OldRowAccessor);
          if SQL = '' then Exit;
          TempKey := Hash(SQL);
          Val := FInsertStatements.Get(TempKey);
          If (Val = nil) or (Val.Statement.IsClosed) then begin
            if Statement <> nil then
              FUpdateStatements.Remove(TempKey);
            Statement := CreateResolverStatement(SQL);
            FDeleteStatements.Add(TempKey, Statement);
          end else
            Statement := Val.Statement;
        end;
        OldRowAccessor.FillStatement(Statement, FCurrentWhereColumns, Metadata);
      end;
    utModified:
      begin
        //now what's faster?: caching stmts too by using a hashmap or recreate always
        //first of all: we need the new command-stmt
        SQL := FormUpdateStatement(OldRowAccessor, NewRowAccessor);
        If SQL = '' then exit;// no fields have been changed
        TempKey := Hash(SQL);
        Val := FUpdateStatements.Get(TempKey);
        If (Val = nil) or (Val.Statement.IsClosed) then begin
          Statement := CreateResolverStatement(SQL);
          if Statement <> nil then
            FUpdateStatements.Remove(TempKey);
          FUpdateStatements.Add(TempKey, Statement);
        end else
          Statement := Val.Statement;
        OldRowAccessor.FillStatement(Statement, FCurrentWhereColumns, Metadata);
        NewRowAccessor.FillStatement(Statement, FUpdateColumns, Metadata);
      end;
    else
      Exit;
  end;

  // if Property ValidateUpdateCount isn't set : assume it's true
  SenderStatement := Sender.GetStatement;
  if Assigned(SenderStatement) then begin
    SQL := SenderStatement.GetParameters.Values[DSProps_ValidateUpdateCount];
    lValidateUpdateCount := (SQL = '') or StrToBoolEx(SQL);
  end else begin
    lValidateUpdateCount := true;
  end;

  lUpdateCount := Statement.ExecuteUpdatePrepared;
  {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
  if  (lValidateUpdateCount) and (lUpdateCount <> 1   ) then
    raise CreateInvalidUpdateCountException;
  {$ENDIF}
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} //rolling eyes
procedure TZGenerateSQLCachedResolver.RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
var Stmt: IZPreparedStatement;
  procedure InitStmt(out Stmt: IZPreparedStatement);
  var
    I, ColumnIndex: Integer;
    SQL, Tmp: SQLString;
    SQLWriter: TZSQLStringWriter;
  begin
    Stmt := nil;
    SQLWriter := TZSQLStringWriter.Create(512+(MetaData.GetColumnCount shl 5));
    SQL := 'SELECT ';
    try
      if FInsertColumns.Count = 0 then
        FillInsertColumnsPairList(RowAccessor);
      if FInsertColumns.Count = 0 then
        Exit;
      for I := 0 to FInsertColumns.Count-1 do begin
        ColumnIndex := PZIndexPair(FInsertColumns[i])^.ColumnIndex;
        Tmp := Metadata.GetColumnName(ColumnIndex);
        Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
        SQLWriter.AddText(Tmp, SQL);
        SQLWriter.AddChar(',', SQL);
      end;
      SQLWriter.ReplaceOrAddLastChar(',', ' ', SQL);
      SQLWriter.AddText('FROM ', SQL);
      Tmp := DefineTableName;
      SQLWriter.AddText(Tmp, SQL);

      FillWhereKeyColumns(0);
      FormWhereClause(SQLWriter, RowAccessor, SQL);
      SQLWriter.Finalize(SQL);
      Stmt := CreateResolverStatement(SQL);
    finally
      FreeAndNil(SQLWriter);
    end;
  end;
begin
  if (RefreshResultSet = nil) or (RefreshResultSet.GetStatement = nil) or RefreshResultSet.GetStatement.IsClosed
  then InitStmt(Stmt)
  else RefreshResultSet.GetStatement.QueryInterface(IZPreparedStatement, Stmt);
  if Stmt = nil then
    raise EZSQLException.Create(SUpdateSQLNoResult)
  else begin
    RowAccessor.FillStatement(Stmt, FWhereColumns, Metadata);
    RefreshResultSet := Stmt.ExecuteQueryPrepared;
    if (RefreshResultSet = nil) or not RefreshResultSet.Next then
      raise EZSQLException.Create(SUpdateSQLNoResult);
    RowAccessor.FillFromFromResultSet(RefreshResultSet, FInsertColumns);
    RefreshResultSet.ResetCursor; //unlock handles
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZGenerateSQLCachedResolver.SetTransaction(
  const Value: IZTransaction);
var Stmt: IZStatement;
begin
  if FTransaction <> Value then begin
    FTransaction := Value;
    if InsertStatement <> nil
    then Stmt := InsertStatement
    else begin
      if (FUpdateStatements.Count > 0) then
        Stmt := PZKeyAndPreparedStatmentPair(FUpdateStatements[0]).Statement
      else if (FDeleteStatements.Count > 0) then
        Stmt := PZKeyAndPreparedStatmentPair(FDeleteStatements[0]).Statement
      else if (FInsertStatements.Count > 0) then
        Stmt := PZKeyAndPreparedStatmentPair(FInsertStatements[0]).Statement
      else Stmt := nil;
    end;
    { test if statement is part of session -> FB always all others will fail}
    if (Stmt <> nil) and ((Value = nil) or (Stmt.GetConnection <> Value.GetConnection)) then begin
      Stmt.Close;
      InsertStatement := nil;
      FInsertStatements.Count := 0;
      FUpdateStatements.Count := 0;
      FDeleteStatements.Count := 0;
    end;
  end;
end;

procedure TZGenerateSQLCachedResolver.SetCalcDefaults(Value: Boolean);
begin
  FCalcDefaults := Value;
end;

procedure TZGenerateSQLCachedResolver.SetUpdateAll(Value: Boolean);
begin
  FUpdateAll := Value;
end;

procedure TZGenerateSQLCachedResolver.SetWhereAll(Value: Boolean);
begin
  if FWhereAll <> Value then begin
    FWhereAll := Value;
  end;
end;

procedure TZGenerateSQLCachedResolver.SetReadOnly(ColumnIndex: Integer;
  Value: Boolean);
begin
  if Metadata.IsReadOnly(ColumnIndex) <> Value then begin
    Metadata.SetReadOnly(ColumnIndex, Value);
    if Metadata.IsReadOnly(ColumnIndex) = Value then begin
      FInsertColumns.Clear;
      FUpdateColumns.Clear;
    end;
  end;
end;

procedure TZGenerateSQLCachedResolver.SetResolverStatementParamters(
  const Statement: IZStatement; {$IFDEF AUTOREFCOUNT}const {$ENDIF} Params: TStrings);
begin
  Params.Assign(Statement.GetParameters);
end;

procedure TZGenerateSQLCachedResolver.SetSearchable(ColumnIndex: Integer;
  Value: Boolean);
begin
  if Metadata.IsSearchable(ColumnIndex) <> Value then begin
    Metadata.SetSearchable(ColumnIndex, Value);
    if Metadata.IsSearchable(ColumnIndex) = Value then
      FWhereColumns.Clear;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Sender" not used} {$ENDIF}
procedure TZGenerateSQLCachedResolver.CalculateDefaults(
  const Sender: IZCachedResultSet; const RowAccessor: TZRowAccessor);
var
  SQL: string;
  DefaultColumnsLookup: TZIndexPairList;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  I, J: Integer;
begin
  if not FCalcDefaults then
     Exit;
  DefaultColumnsLookup := TZIndexPairList.Create;
  try
    J := FirstDbcIndex;
    DefaultColumnsLookup.Capacity := Metadata.GetColumnCount;
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if RowAccessor.IsNull(I) and (Metadata.GetTableName(I) <> '')
        and ((Metadata.GetDefaultValue(I) <> '') or (RowAccessor.GetColumnDefaultExpression(I) <> '')) then begin
          DefaultColumnsLookup.Add(J, I);
          Inc(J);
        end;
    if J = FirstDbcIndex then
       Exit;
    SQL := FormCalculateStatement(RowAccessor, DefaultColumnsLookup);
    if SQL = '' then
       Exit;
    { Executes statement and fills default fields. }
    Statement := Connection.CreateStatement;
    ResultSet := Statement.ExecuteQuery(SQL);
    if ResultSet.Next then
      try
        RowAccessor.FillFromFromResultSet(ResultSet, DefaultColumnsLookup);
      except
        { Supress any errors in default fields. }
      end;
    ResultSet.Close;
    Statement.Close;
  finally
    FreeAndnil(DefaultColumnsLookup);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract base class - parameters not used intentionally
 procedure TZGenerateSQLCachedResolver.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
  const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
begin
 //Should be implemented at Specific database Level Cached resolver
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ ZKeyAndPreparedStatementPairList }

procedure ZKeyAndPreparedStatementPairList.Add(Hash: NativeUInt;
  const Statement: IZPreparedStatement);
var Index: NativeInt;
  KeyAndPreparedStatmentPair: PZKeyAndPreparedStatmentPair;
begin
  KeyAndPreparedStatmentPair := inherited Add(Index);
  KeyAndPreparedStatmentPair.Key := Hash;
  KeyAndPreparedStatmentPair.Statement := Statement;
end;

constructor ZKeyAndPreparedStatementPairList.Create;
begin
  inherited Create(SizeOf(TZKeyAndPreparedStatmentPair), True);
end;

function ZKeyAndPreparedStatementPairList.Get(
  Hash: NativeUInt): PZKeyAndPreparedStatmentPair;
var I: NativeInt;
begin
  for I := 0 to Count -1 do begin
    Result := inherited Get(I);
    if (Result.Key = Hash) then
      Exit;
  end;
  Result := nil;

end;

procedure ZKeyAndPreparedStatementPairList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if (Action = lnDeleted) and (PZKeyAndPreparedStatmentPair(Ptr).Statement <> nil) then begin
    PZKeyAndPreparedStatmentPair(Ptr).Statement.Close;
    PZKeyAndPreparedStatmentPair(Ptr).Statement := nil;
  end;
end;

procedure ZKeyAndPreparedStatementPairList.Remove(Hash: NativeUInt);
var I: NativeInt;
    Pair: PZKeyAndPreparedStatmentPair;
begin
  for I := 0 to Count -1 do begin
    Pair := inherited Get(I);
    if (Pair.Key = Hash) then begin
      Delete(I);
      Break;
    end;
  end;
end;

end.

