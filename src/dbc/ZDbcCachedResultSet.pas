{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Caching Classes and Interfaces               }
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

{ constributors:
  EgonHugeist
  FOS
  Michael Seeger
  Mark Ford
}
unit ZDbcCachedResultSet;

interface

{$I ZDbc.inc}

uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}FmtBCD,
  ZDbcResultSetMetadata, ZClasses, ZDbcIntfs, ZDbcResultSet, ZDbcCache,
  ZCompatibility, ZExceptions;

type
  // Forward declarations.
  IZCachedResultSet = interface;

  {** Resolver to post updates. }
  IZCachedResolver = interface (IZInterface)
    ['{546ED716-BB88-468C-8CCE-D7111CF5E1EF}']
    /// <author>Michael Seeger</author>
    /// <summary>Calculate default values for the fields.</summary>
    /// <param>"Sender" a cached result set object.</param>
    /// <param>"RowAccessor" an accessor object to column values.</param>
    procedure CalculateDefaults(const Sender: IZCachedResultSet;
      const RowAccessor: TZRowAccessor);
    /// <summary>Posts updates to database.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"UpdateType" a type of updates.</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor);
    /// <summary>Posts updates the autoincrement fields.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"UpdateType" a type of updates.</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <param>"Resolver" the resolver object used to load the column data.</param>
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
    /// <summary>Refreshes the current row.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"RowAccessor" an accessor object to current column values.</param>
    procedure RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor); //FOS+ 07112006
    /// <author>EgonHugeist</author>
    /// <summary>Set a transaction object used for the crud-operations.</summary>
    /// <param>"Value" the IZTransaction object.</param>
    procedure SetTransaction(const Value: IZTransaction);
    /// <author>EgonHugeist</author>
    /// <summary>Test if the resolver-transaction is a autocommit txn.</summary>
    /// <returns><c>True</c> if the transaction object is in autocommit mode;
    ///  <c>False</c> otherwise.</returns>
    function HasAutoCommitTransaction: Boolean;
    /// <author>EgonHugeist</author>
    /// <summary>Flush all cached statements</summary>
    procedure FlushStatementCache;
    /// <summary>Set a new connection.</summary>
    /// <param>"Value" the IZTransaction object.</param>
    procedure SetConnection(const Value: IZConnection);
    /// <author>Egonhugeist</author>
    /// <summary>Gets the correct transaction to use for new objects from this Resolver.
    ///  That is either an explicitly assigned transaction or the currently active transaction
    ///  from the connection.</summary>
    /// <returns>A valid transaction.</returns>
    function GetTransaction: IZTransaction;
  end;

  IZGenerateSQLCachedResolver = interface(IZCachedResolver)
    ['{D2694EF6-F6B6-4A11-BB46-456ED63DCC18}']
    /// <summary>Set the readonly state of a field. The value will be ignored
    ///  if the field is not writable.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the dml's.</param>
    procedure SetReadOnly(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Set the searchable state of a field. The value will be ignored
    ///  if the field is not searchable at all e.g. LOB's.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
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
    /// <summary>Set's a list of parameter properties to this resolver object.</summary>
    /// <param>"Value" the List of parameter.</param>
    procedure SetResolverParameters(Value: TStrings);
    /// <summary>Set a new resultset metadata object</summary>
    /// <param>"Value" the new resultset metadata object to be set.</param>
    procedure SetMetadata(const Value: IZResultSetMetadata);
  end;

  {** Represents a cached result set. }
  IZCachedResultSet = interface (IZResultSet)
    ['{BAF24A92-C8CE-4AB4-AEBC-3D4A9BCB0946}']

    function GetResolver: IZCachedResolver;
    procedure SetResolver(const Resolver: IZCachedResolver);

    {BEGIN PATCH [1214009] Calc Defaults in TZUpdateSQL and Added Methods to GET and SET the database Native Resolver
      will help to implemented feature to Calculate default values in TZUpdateSQL
      comment: add this properties to get the original Resolver
      this will be useful whn using TZUpdateSQL //added by fduenas
    }
    function GetNativeResolver: IZCachedResolver;
   {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}

    /// <summary>Checks if the resultset has late fetched fields linked to
    ///  the server. Such as LOBS or Statements or ResultSets. If this is true
    ///  the resultset is not full memory cached.</summary>
    /// <returns><c>True</ce> if the resultset has server linked fields.</returns>
    function HasServerLinkedColumns: Boolean;
    function IsCachedUpdates: Boolean;
    /// <summary>Checks is the last row was fetched to cache.</summary>
    /// <returns><c>True</ce> if the last row was fetched.</returns>
    function IsLastRowFetched: Boolean;
    function IsPendingUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    procedure ClearStatementLink;
    /// <summary>Posts all saved updates to the server.</summary>
    procedure PostUpdates;
    procedure CancelUpdates;
    procedure PostUpdatesCached;
    procedure DisposeCachedUpdates;
    procedure RevertRecord;
    procedure MoveToInitialRow;
  end;

  {** Implements cached ResultSet. }
  TZAbstractCachedResultSet = class (TZAbstractResultSet, IZResultSet, IZCachedResultSet)
  private
    FCachedUpdates: Boolean;
    FLobCacheMode: TLobCacheMode;
    FRowsList: TZSortedList;
    FInitialRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FCurrentRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FIndexPairList: TZIndexPairList;
    FSelectedRow: PZRowBuffer;
    FUpdatedRow: PZRowBuffer;
    FInsertedRow: PZRowBuffer;
    FEmptyRow: PZRowBuffer; //improve the MoveToInitialRow in insert state
    FRowAccessor: TZRowAccessor;
    FNewRowAccessor: TZRowAccessor;
    FOldRowAccessor: TZRowAccessor;
    FNextRowIndex: Integer;
    FResolver: IZCachedResolver;
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    FNativeResolver: IZCachedResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  protected
    FLastRowFetched: Boolean;
    FLocalConSettings: TZConSettings;
    FLocalCharSet: TZCodePage;
    class function GetRowAccessorClass: TZRowAccessorClass; virtual;
    procedure CheckAvailable;
    procedure CheckUpdatable;
    procedure Open; override;
    function GetNextRowIndex: Integer;

    procedure CalculateRowDefaults(RowAccessor: TZRowAccessor); virtual;
    procedure PostRowUpdates(OldRowAccessor,
      NewRowAccessor: TZRowAccessor); virtual;
    function LocateRow(RowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}; RowIndex: Integer): Integer;
    function AppendRow(Row: PZRowBuffer): PZRowBuffer;
    procedure PrepareRowForUpdates;

    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates;
    property RowsList: TZSortedList read FRowsList write FRowsList;
    property InitialRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF} read FInitialRowsList
      write FInitialRowsList;
    property CurrentRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF} read FCurrentRowsList
      write FCurrentRowsList;
    property SelectedRow: PZRowBuffer read FSelectedRow write FSelectedRow;
    property UpdatedRow: PZRowBuffer read FUpdatedRow write FUpdatedRow;
    property InsertedRow: PZRowBuffer read FInsertedRow write FInsertedRow;
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property OldRowAccessor: TZRowAccessor read FOldRowAccessor
      write FOldRowAccessor;
    property NewRowAccessor: TZRowAccessor read FNewRowAccessor
      write FNewRowAccessor;
    property NextRowIndex: Integer read FNextRowIndex write FNextRowIndex;
    property Resolver: IZCachedResolver read FResolver write FResolver;
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    property NativeResolver: IZCachedResolver read FNativeResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  public
    constructor CreateWithStatement(const SQL: string; const Statement: IZStatement;
      ConSettings: PZConSettings);
    constructor CreateWithColumns(ColumnsInfo: TObjectList; const SQL: string;
      ConSettings: PZConSettings); overload;
    constructor CreateWithColumns(const Statement: IZStatement;
      ColumnsInfo: TObjectList; const SQL: string; ConSettings: PZConSettings); overload;
    destructor Destroy; override;

    procedure AfterClose; override;
    procedure ResetCursor; override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetString(ColumnIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetUnicodeString(ColumnIndex: Integer): UnicodeString;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; Var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    function GetDefaultExpression(ColumnIndex: Integer): string;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; Value: Byte);
    procedure UpdateShort(ColumnIndex: Integer; Value: ShortInt);
    procedure UpdateWord(ColumnIndex: Integer; Value: Word);
    procedure UpdateSmall(ColumnIndex: Integer; Value: SmallInt);
    procedure UpdateUInt(ColumnIndex: Integer; Value: Cardinal);
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer);
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64);
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double);
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: TBCD);
    procedure UpdateGUID(ColumnIndex: Integer; const Value: TGUID);
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; var Len: NativeUint); overload;
    /// <summary>Updates the designated column with a <c>PWideChar</c> buffer
    ///  value. The <c>updateXXX</c> methods are used to update column values in
    ///  the current row or the insert row.  The <c>updateXXX</c> methods do not
    ///  update the underlying database; instead the <c>updateRow</c> or
    ///  <c>insertRow</c> methods are called to update the database.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" an address of the value buffer</param>
    /// <param>"Len" a reference of the buffer Length variable in words.</param>
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; var Len: NativeUint); overload;
    procedure UpdateString(ColumnIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: UnicodeString);
    procedure UpdateBytes(ColumnIndex: Integer; Value: PByte; var Len: NativeUInt); overload;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TZDate); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TZTime); overload;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob);
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure CancelRowUpdates; override;
    procedure RefreshRow; override;// FOS+ 071106

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer; override;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs; override;

    //---------------------------------------------------------------------
    // Cached Updates
    //---------------------------------------------------------------------

    function GetResolver: IZCachedResolver;
    procedure SetResolver(const Resolver: IZCachedResolver);
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    function GetNativeResolver: IZCachedResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    function IsCachedUpdates: Boolean;
    /// <summary>Checks is the last row was fetched to cache.</summary>
    /// <returns><c>True</ce> if the last row was fetched.</returns>
    function IsLastRowFetched: Boolean;
    /// <summary>Checks if the resultset has late fetched fields linked to
    ///  the server. Such as LOBS or Statements or ResultSets. If this is true
    ///  the resultset is not full memory cached.</summary>
    /// <returns><c>True</ce> if the resultset has server linked fields.</returns>
    function HasServerLinkedColumns: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    function IsPendingUpdates: Boolean; virtual;
    /// <summary>Moves the cursor to the insert row.  The current cursor
    ///  position is remembered while the cursor is positioned on the insert
    ///  row.
    ///  The insert row is a special row associated with an updatable result
    ///  set. It is essentially a buffer where a new row may be constructed by
    ///  calling the <c>updateXXX</c> methods prior to inserting the row into
    ///  the result set.
    ///  Only the <c>updateXXX</c>, <c>getXXX</c> and <c>insertRow</c>
    ///  methods may be called when the cursor is on the insert row. All of the
    ///  columns in a result set must be given a value each time this method is
    ///  called before calling <c>insertRow</c>. An <c>updateXXX</c> method must
    ///  be called before a <c>getXXX</c> method can be called on a column value.</summary>
    procedure MoveToInsertRow; override;
    /// <summary>Moves the cursor to the remembered cursor position, usually the
    ///  current row. This method has no effect if the cursor is not on the
    ///  insert row.</summary>
    procedure MoveToCurrentRow; override;
    /// <summary>Posts all saved updates to the server.</summary>
    procedure PostUpdates; virtual;
    procedure CancelUpdates; virtual;
    procedure RevertRecord; virtual;
    procedure MoveToInitialRow; virtual;
    procedure PostUpdatesCached; virtual;
    procedure DisposeCachedUpdates; virtual;
    procedure ClearStatementLink; virtual;
    {$IFDEF WITH_COLUMNS_TO_JSON}
    procedure ColumnsToJSON(ResultsWriter: {$IFDEF MORMOT2}TResultsWriter{$ELSE}TJSONWriter{$ENDIF}; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject, jcoDATETIME_MAGIC]);
    {$ENDIF WITH_COLUMNS_TO_JSON}

    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob; virtual;
  end;

  {**
    Implements Abstract cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZCachedResultSet = class(TZAbstractCachedResultSet)
  private
    FResultSet: IZResultSet;
  protected
    /// <summary>Cycle through the entire result set and cache any uncached
    ///  lobs as Disconnected '[Disc]'. Needed for the TryKeepDataOnDisconnect
    ///  feature and lcmOnAccess lob caching.</summary>
    procedure CacheAllLobs;
    procedure FillColumnsInfo(const ColumnsInfo: TObjectList); virtual;
    procedure Open; override;
    /// <summary>Fetches one row from the wrapped result set object.</summary>
    /// <returns><c>True</c> if row was successfuly fetched or <c>False</c>
    ///  otherwise.</returns>
    function Fetch: Boolean; virtual;
    procedure FetchAll; virtual;

    property ResultSet: IZResultSet read FResultSet write FResultSet;
  public
    constructor Create(const ResultSet: IZResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);
    constructor CreateWithColumns(const ColumnsInfo: TObjectList;
      const ResultSet: IZResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); override;
    procedure AfterClose; override;
    procedure ResetCursor; override;
    function GetMetaData: IZResultSetMetaData; override;
    procedure ClearStatementLink; override;

    function IsLast: Boolean; override;
    procedure AfterLast; override;
    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  {** Represents a Virtual ResultSet interface. }
  IZVirtualResultSet = interface(IZCachedResultSet)
    ['{D84055AC-BCD5-40CD-B408-6F11AF000C96}']
    procedure SetType(Value: TZResultSetType);
    procedure SetConcurrency(Value: TZResultSetConcurrency);
    procedure ChangeRowNo(CurrentRowNo, NewRowNo: NativeInt);
    procedure SortRows(const ColumnIndices: TIntegerDynArray; Descending: Boolean);
    procedure CopyFrom(const Source: IZResultSet; Rows: TZSortedList;
      FieldPairs: TZIndexPairList);
  end;

  {** Implements Virtual ResultSet. }
  TZVirtualResultSet = class(TZAbstractCachedResultSet, IZVirtualResultSet)
  private
    fColumnIndices, FCompareFuncs: Pointer; //ovoid RTTI finalize!
    function ColumnSort(Item1, Item2: Pointer): Integer;
  protected
    procedure CalculateRowDefaults(RowAccessor: TZRowAccessor); override;
    procedure PostRowUpdates(OldRowAccessor, NewRowAccessor: TZRowAccessor);
      override;
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    constructor CreateWithStatement(const SQL: string; const Statement: IZStatement;
      ConSettings: PZConSettings);
    constructor CreateWithColumns(ColumnsInfo: TObjectList; const SQL: string;
      ConSettings: PZConSettings);
    constructor CreateFrom(const Source: IZResultSet; Rows: TZSortedList;
      FieldPairs: TZIndexPairList; ConSettings: PZConSettings);
    constructor CreateCloneFrom(const Source: IZResultSet);
  public
    procedure ChangeRowNo(CurrentRowNo, NewRowNo: NativeInt);
    procedure SortRows(const ColumnIndices: TIntegerDynArray; Descending: Boolean);
    procedure CopyFrom(const Source: IZResultSet; Rows: TZSortedList;
      FieldPairs: TZIndexPairList);
  end;

  { TZVirtualResultSetRowAccessor }

  TZVirtualResultSetRowAccessor = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; LobCacheMode: TLobCacheMode); override;
  end;

implementation

uses ZMessages, ZDbcGenericResolver, ZDbcUtils, ZEncoding, ZDbcProperties,
  ZSysUtils;


{ TZAbstractCachedResultSet }

function TZAbstractCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  DataAddress: Pointer;
  IsNull: Boolean;
  CP: Word;
  CL: Integer;
label Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if ResultSetConcurrency = rcUpdatable then begin
    DataAddress := RowAccessor.GetColumnData(ColumnIndex, IsNull);
    SQLType := RowAccessor.GetColumnType(ColumnIndex);
    CP := RowAccessor.GetColumnCodePage(ColumnIndex);
    CL := RowAccessor.GetColumnLength( ColumnIndex);
    case SQLType of
      stBytes: if (CL <= 0) or (CL = MaxInt)
          then Result := TZRowAccessorBytesLob.CreateWithDataAddess(DataAddress, zCP_Binary, ConSettings, FOpenLobStreams)
          else goto Fail;
      stString, stUnicodeString: if (CL <= 0) or (CL = MaxInt) then
          if CP = zCP_UTF16
          then Result := TZRowAccessorUnicodeStringLob.CreateWithDataAddess(DataAddress, CP, ConSettings, FOpenLobStreams)
          else Result := TZRowAccessorRawByteStringLob.CreateWithDataAddess(DataAddress, CP, ConSettings, FOpenLobStreams)
        else goto Fail;
      stAsciiStream, stUnicodeStream: begin
          Result := TZLocalMemCLob.Create(CP, ConSettings, FOpenLobStreams);
          PIZLob(DataAddress)^ := Result;
          if IsNull then
            PByte(PAnsiChar(DataAddress)-1)^ := 1;
        end;
      stBinaryStream: begin
          Result := TZLocalMemBLob.Create(FOpenLobStreams);
          PIZLob(DataAddress)^ := Result;
          if IsNull then
            PByte(PAnsiChar(DataAddress)-1)^ := 1;
        end
      else
Fail:   raise CreateCanNotAccessBlobRecordException(ColumnIndex, SQLType);
    end;
  end else
    raise CreateReadOnlyException;
  Result.Open(LobStreamMode);
end;

constructor TZAbstractCachedResultSet.CreateWithColumns(
  const Statement: IZStatement; ColumnsInfo: TObjectList; const SQL: string;
  ConSettings: PZConSettings);
begin
  inherited Create(Statement, SQL, nil, ConSettings);
  CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  FCachedUpdates := False;
  Open;
end;

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query.
}
constructor TZAbstractCachedResultSet.CreateWithStatement(const SQL: string;
  const Statement: IZStatement; ConSettings: PZConSettings);
begin
  inherited Create(Statement, SQL, nil, ConSettings);
  FCachedUpdates := False;
end;

{**
  Creates this object and assignes the main properties.
  @param SQL an SQL query.
  @param ColumnsInfo a columns info for cached rows.
}
constructor TZAbstractCachedResultSet.CreateWithColumns(
  ColumnsInfo: TObjectList; const SQL: string; ConSettings: PZConSettings);
begin
  inherited Create(nil, SQL, nil, ConSettings);
  CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  FCachedUpdates := False;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractCachedResultSet.Destroy;
begin
  inherited Destroy;
  FResolver := nil;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := nil;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FreeAndNil(FIndexPairList);
end;

{**
  Checks for availability of the cached buffer.
}
procedure TZAbstractCachedResultSet.CheckAvailable;
begin
  CheckClosed;
  if (FRowAccessor = nil) or (FRowAccessor.RowBuffer = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
end;

{**
  Checks is the cached buffer updatable.
}
procedure TZAbstractCachedResultSet.CheckUpdatable;
begin
  CheckAvailable;
  if ResultSetConcurrency <> rcUpdatable then
    raise CreateReadOnlyException;;
end;

procedure TZAbstractCachedResultSet.ClearStatementLink;
begin
  //noop
end;

{**
  Generates the next row index value.
  @return the new generated row index.
}
function TZAbstractCachedResultSet.GetNextRowIndex: Integer;
begin
  Result := FNextRowIndex;
  Inc(FNextRowIndex);
end;

{**
  Finds a row with specified index among list of rows.
  @param RowsList a list of rows.
  @param Index a row index.
  @return a found row buffer of <code>null</code> otherwise.
}
function TZAbstractCachedResultSet.LocateRow(RowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
  RowIndex: Integer): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to RowsList.Count - 1 do
    if PZRowBuffer(RowsList[I]).Index = RowIndex then begin
      Result := I;
      Break;
    end;
end;

{**
  Appends a row to the list of rows if such row is not exist.
  @param Row a row buffer.
  @return an appended row buffer.
}
function TZAbstractCachedResultSet.AppendRow(Row: PZRowBuffer): PZRowBuffer;
begin
  if LocateRow(FInitialRowsList, Row.Index) < 0 then
  begin
    Result := FRowAccessor.AllocBuffer;
    FRowAccessor.CopyBuffer(Row, Result);
    FInitialRowsList.Add(Result);
    FCurrentRowsList.Add(Row);
  end
  else
    Result := nil;
end;

{**
  Prepares the current selected row for updates.
}
procedure TZAbstractCachedResultSet.PrepareRowForUpdates;
begin
  if (RowAccessor.RowBuffer = FSelectedRow) and (FSelectedRow <> FUpdatedRow) and (FSelectedRow.UpdateType <> utInserted) then begin
    FSelectedRow := FUpdatedRow;
    RowAccessor.RowBuffer := FSelectedRow;
    RowAccessor.CloneFrom(PZRowBuffer(FRowsList[RowNo - 1]));
    FUpdatedRow.Index := FSelectedRow.Index;
    FUpdatedRow.UpdateType := utModified;
  end;
end;

procedure TZAbstractCachedResultSet.CalculateRowDefaults(
  RowAccessor: TZRowAccessor);
begin
{$IFNDEF DISABLE_CHECKING}
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
{$ENDIF}
  Resolver.CalculateDefaults(Self, RowAccessor);
end;

{**
  Post changes to database server.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new or updated
    column values.
}
procedure TZAbstractCachedResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
{$IFNDEF DISABLE_CHECKING}
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
{$ENDIF}
  Resolver.PostUpdates(Self, NewRowAccessor.RowBuffer.UpdateType,
    OldRowAccessor, NewRowAccessor);
end;

{**
  Gets a cached updates resolver object.
  @return a cached updates resolver object.
}
function TZAbstractCachedResultSet.GetResolver: IZCachedResolver;
begin
  Result := FResolver;
end;

class function TZAbstractCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZRowAccessor;
end;

{**
  Sets a new cached updates resolver object.
  @param Resolver a cached updates resolver object.
}
procedure TZAbstractCachedResultSet.SetResolver(const Resolver: IZCachedResolver);
begin
  FResolver := Resolver;
{BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  if FNativeResolver = nil then
     FNativeResolver := Resolver;
{END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
end;
{BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
{**
  Gets a Native cached updates resolver object.
  @return a Native cached updates resolver object.
}
function TZAbstractCachedResultSet.GetNativeResolver: IZCachedResolver;
begin
  Result := FNativeResolver;
end;
{END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}

{**
  Checks is the cached updates mode turned on.
  @return <code>True</code> if the cached updates mode turned on.
}
function TZAbstractCachedResultSet.IsCachedUpdates: Boolean;
begin
  Result := FCachedUpdates;
end;

function TZAbstractCachedResultSet.IsLastRowFetched: Boolean;
begin
  Result := FLastRowFetched;
end;

{**
  Switched the cached updates mode.
  @param Value boolean flag which turns on/off the cached updates mode.
}
procedure TZAbstractCachedResultSet.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates := Value;
    if not FCachedUpdates then
      PostUpdates;
  end;
end;

{**
  Checks is cached updates pending.
  @return <code>True</code> if the cached updates pending.
}
function TZAbstractCachedResultSet.IsPendingUpdates: Boolean;
begin
  //patch by Soner
  Result := Assigned(FInitialRowsList) and (FInitialRowsList.Count > 0);
  //Original:  Result := FInitialRowsList.Count > 0;
  //this can cause error if you Call TZQuery.UpdatesPending at DoBeforeClose;
  //because FInitialRowsList can be nil if nothing inserted/deleted/modified
end;

{**
  Moves to the current row with initial column values.
}
procedure TZAbstractCachedResultSet.MoveToInitialRow;
var
  Index: Integer;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) and (FSelectedRow <> nil) then begin
    Index := LocateRow(FInitialRowsList, FSelectedRow.Index);
    if Index >= 0 then
      FRowAccessor.RowBuffer := FInitialRowsList[Index]//do not set the FSelectedRow
    else if (FUpdatedRow.Index <> -1) and (FUpdatedRow.Index = FSelectedRow.Index) and (FUpdatedRow.UpdateType = utModified) then
      FRowAccessor.RowBuffer := FRowsList[FUpdatedRow.Index]////do not set the FSelectedRow;
    else if FRowAccessor.RowBuffer = FInsertedRow then begin
      if FEmptyRow = nil then
        FEmptyRow := FRowAccessor.AllocBuffer;
      FRowAccessor.RowBuffer := FEmptyRow;
    end;

  end else
    FRowAccessor.RowBuffer := nil;
end;

procedure TZAbstractCachedResultSet.PostUpdates;
begin
  CheckClosed;
  if FInitialRowsList.Count > 0 then begin
    while FInitialRowsList.Count > 0 do begin
      OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[0]);
      NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[0]);

      { Updates default field values. }
      if NewRowAccessor.RowBuffer.UpdateType in [utInserted, utModified] then
        CalculateRowDefaults(NewRowAccessor);

      { Posts row updates and processes the exceptions. }
      PostRowUpdates(OldRowAccessor, NewRowAccessor);

      { If post was Ok - update the row update type. }
      if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then begin
        NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
        if (FSelectedRow <> nil) and
           (FSelectedRow.Index = NewRowAccessor.RowBuffer.Index) then
          FSelectedRow.UpdateType := utUnmodified;
        FUpdatedRow.Index := -1;
      end;

      { Removes cached rows. }
      OldRowAccessor.Dispose;
      FInitialRowsList.Delete(0);
      FCurrentRowsList.Delete(0);
    end;
  end;
end;

{**
  Posts all saved updates to the server but keeps them cached.
}
procedure TZAbstractCachedResultSet.PostUpdatesCached;
var
  i: Integer;
begin
  CheckClosed;
  if FInitialRowsList.Count > 0 then begin
    i := 0;
    while i < FInitialRowsList.Count do begin
      OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[i]);
      NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[i]);
      { Updates default field values. }
      if NewRowAccessor.RowBuffer.UpdateType in [utInserted, utModified] then
        CalculateRowDefaults(NewRowAccessor);
      { Posts row updates. }
      PostRowUpdates(OldRowAccessor, NewRowAccessor);
      if Resolver.HasAutoCommitTransaction then begin
        if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then begin
          NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
          if (FSelectedRow <> nil)
            and (FSelectedRow.Index = NewRowAccessor.RowBuffer.Index) then
              FSelectedRow.UpdateType := utUnmodified;
        end;
        { Remove cached rows. }
        OldRowAccessor.Dispose;
        FInitialRowsList.Delete(i);
        FCurrentRowsList.Delete(i);
      end else
        Inc(i);
    end;
  end;
end;

{**
  Frees the updates and marks records as unmodified. Complements
  PostUpdatesCached.
}
procedure TZAbstractCachedResultSet.DisposeCachedUpdates;
var i: Integer;
begin
  for I := FInitialRowsList.Count -1 downto 0 do begin
    OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[i]);
    NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[i]);

    if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then begin
      NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
      if (FSelectedRow <> nil)
        and (FSelectedRow.Index = NewRowAccessor.RowBuffer.Index) then
          FSelectedRow.UpdateType := utUnmodified;
    end;
    { Remove cached rows. }
    OldRowAccessor.Dispose;
    FInitialRowsList.Delete(i);
    FCurrentRowsList.Delete(i);
  end;
end;

{**
  Cancels updates for all rows.
}
procedure TZAbstractCachedResultSet.CancelUpdates;
var i: Integer;
    InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  for I := FInitialRowsList.Count -1 downto 0 do begin
    InitialRow := PZRowBuffer(FInitialRowsList[i]);
    CurrentRow := PZRowBuffer(FCurrentRowsList[i]);

    if CurrentRow.UpdateType = utInserted then
      InitialRow.UpdateType := utDeleted;

    FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
    if (FSelectedRow = FUpdatedRow) and
       (FSelectedRow.Index = InitialRow.Index) then begin
      FRowAccessor.CopyBuffer(InitialRow, FSelectedRow);
      FRowAccessor.ClearBuffer(FUpdatedRow);
    end;

    FRowAccessor.DisposeBuffer(InitialRow);
    FInitialRowsList.Delete(i);
    FCurrentRowsList.Delete(i);
  end;
end;

{**
  Cancels updates for the current row.
}
procedure TZAbstractCachedResultSet.RefreshRow;
begin
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
  Resolver.RefreshCurrentRow(Self,RowAccessor);
end;

procedure TZAbstractCachedResultSet.RevertRecord;
var
  Index: Integer;
  InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  if (RowNo > 0) and (RowNo <= LastRowNo) then begin
    Index := LocateRow(FInitialRowsList, FSelectedRow.Index);
    if Index >= 0 then begin
      InitialRow := PZRowBuffer(FInitialRowsList[Index]);
      CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);

      if CurrentRow.UpdateType = utInserted then
        InitialRow.UpdateType := utDeleted;
      FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
      if (FSelectedRow = FUpdatedRow) then begin
        FRowAccessor.CopyBuffer(InitialRow, FSelectedRow);
        FUpdatedRow.Index := -1;
        FUpdatedRow.UpdateType := utUnmodified;
      end;

      FRowAccessor.DisposeBuffer(InitialRow);
      FInitialRowsList.Delete(Index);
      FCurrentRowsList.Delete(Index);
    end else if (FSelectedRow = FUpdatedRow) and (FSelectedRow.Index < FRowsList.Count) then begin
      CurrentRow := FRowsList[FSelectedRow.Index];
      FRowAccessor.ClearBuffer(FSelectedRow);
      FSelectedRow := CurrentRow;
    end else if (FRowAccessor.RowBuffer = FInsertedRow) then begin
      FRowAccessor.Clear;
      FRowAccessor.RowBuffer := FSelectedRow;
    end;
  end;
end;

{**
  Opens this recordset.
}
procedure TZAbstractCachedResultSet.Open;
var I: Integer;
begin
  if not Closed then
    raise EZSQLException.Create(SResultsetIsAlreadyOpened);
  //EH: make the ConSetting ptr disconnect imune
  if ConSettings <> @FLocalConSettings then begin
    FLocalConSettings := ConSettings^;
    FLocalCharSet := ConSettings^.ClientCodePage^;
    FLocalConSettings.ClientCodePage := @FLocalCharSet;
    Self.ConSettings := @FLocalConSettings;
  end;

  FRowsList := TZSortedList.Create;
  FInitialRowsList := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FCurrentRowsList := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;

  FRowAccessor := GetRowAccessorClass.Create(ColumnsInfo, ConSettings, FOpenLobStreams, FLobCacheMode);
  FOldRowAccessor := GetRowAccessorClass.Create(ColumnsInfo, ConSettings, FOpenLobStreams, FLobCacheMode);
  FNewRowAccessor := GetRowAccessorClass.Create(ColumnsInfo, ConSettings, FOpenLobStreams, FLobCacheMode);

  FUpdatedRow := FRowAccessor.AllocBuffer;
  FInsertedRow := FRowAccessor.AllocBuffer;
  FSelectedRow := nil;

  FNextRowIndex := 0;

  if (Resolver = nil) and (GetConcurrency = rcUpdatable) then
    Resolver := TZGenerateSQLCachedResolver.Create(GetStatement, GetMetadata);

  inherited Open;
  if FIndexPairList = nil then
    FIndexPairList := TZIndexPairList.Create;
  FIndexPairList.Clear;
  FIndexPairList.Capacity := ColumnsInfo.Count;
  for I := FirstDbcIndex to ColumnsInfo.Count{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    FIndexPairList.Add(I, I);
  FCursorLocation := rctClient;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAbstractCachedResultSet.AfterClose;
var
  I: Integer;
begin
  inherited AfterClose;
  //required for FPC which has a different logic freeing the interfaces..
  //the reolver caches some dml stmts, even if we close the resultset and nil the
  //intf afterwards, FPC holds the rs in memory, thus some "Object is in use"
  //errors may mappen, see TZTestDbcInterbaseCase.Test_GENERATED_BY_DEFAULT_64 f.e.
  FNativeResolver := nil;
  FResolver := nil; //required for FPC
  if Assigned(FRowAccessor) then begin
    for I := 0 to FRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
    for I := 0 to FInitialRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FInitialRowsList[I]));

    FRowAccessor.DisposeBuffer(FUpdatedRow);
    FUpdatedRow := nil;
    FRowAccessor.DisposeBuffer(FInsertedRow);
    FInsertedRow := nil;
    FSelectedRow := nil;

    if FEmptyRow <> nil then begin
      FRowAccessor.DisposeBuffer(FEmptyRow);
      FEmptyRow := nil;
    end;

    FreeAndNil(FRowsList);
    FreeAndNil(FInitialRowsList);
    FreeAndNil(FCurrentRowsList);

    FreeAndNil(FRowAccessor);
    FreeAndNil(FOldRowAccessor);
    FreeAndNil(FNewRowAccessor);
    FreeAndNil(FIndexPairList);
  end;
end;

procedure TZAbstractCachedResultSet.ResetCursor;
var
  I: Integer;
begin
  if Assigned(FRowAccessor) then begin
    for I := 0 to FRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
    for I := 0 to FInitialRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FInitialRowsList[I]));
    FRowsList.Clear;
    FInitialRowsList.Clear;
    FCurrentRowsList.Clear;
  end;
  FNextRowIndex := 0;
  inherited ResetCursor;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractCachedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.IsNull(ColumnIndex);
end;

function TZAbstractCachedResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetPAnsiChar(ColumnIndex, LastWasNull, Len);
end;

function TZAbstractCachedResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetPWideChar(ColumnIndex, LastWasNull, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetString(ColumnIndex: Integer): String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractCachedResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetAnsiString(ColumnIndex, LastWasNull);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractCachedResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUTF8String(ColumnIndex, LastWasNull);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetRawByteString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Widestring</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetUnicodeString(ColumnIndex: Integer): UnicodeString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUnicodeString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractCachedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBoolean(ColumnIndex, LastWasNull);
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBytes(ColumnIndex, LastWasNull, len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetULong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetLong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetFloat(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZAbstractCachedResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  FRowAccessor.GetGUID(ColumnIndex, Result, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetDouble(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetCurrency(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractCachedResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  FRowAccessor.GetBigDecimal(ColumnIndex, Result, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractCachedResultSet.GetDate(ColumnIndex: Integer; Var Result: TZDate);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  FRowAccessor.GetDate(ColumnIndex, LastWasNull, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractCachedResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  FRowAccessor.GetTime(ColumnIndex, LastWasNull, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
procedure TZAbstractCachedResultSet.GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  FRowAccessor.GetTimestamp(ColumnIndex, LastWasNull, Result);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractCachedResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  Current: IZBlob;
  CLob: IZClob;
  Newlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if LobStreamMode <> lsmRead then
    PrepareRowForUpdates;
  Result := FRowAccessor.GetBlob(ColumnIndex, LastWasNull);
  if (FRowAccessor.LobCacheMode = lcmOnAccess) and (Result <> nil) and not Result.IsCached then
  begin
    Current := Result;
    if Current.QueryInterface(IZCLob, Clob) = S_OK
    then Newlob := TZLocalMemCLob.CreateFromClob(Clob, FRowAccessor.GetColumnCodePage(ColumnIndex), ConSettings, FOpenLobStreams)
    else Newlob := TZLocalMemBLob.CreateFromBlob(Current, FOpenLobStreams);
    Result := Newlob;
    FRowAccessor.SetBlob(ColumnIndex, Result);
  end;
  if (Result = nil) and (LobStreamMode <> lsmRead) then
    Result := CreateLob(ColumnIndex, LobStreamMode);
end;

{**
  Gets the DefaultExpression value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the DefaultExpression value
}
function TZAbstractCachedResultSet.GetDefaultExpression(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetColumnDefaultExpression(ColumnIndex);
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractCachedResultSet.UpdateNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetNull(ColumnIndex);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateBoolean(ColumnIndex: Integer;
  Value: Boolean);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBoolean(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateByte(ColumnIndex: Integer;
  Value: Byte);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetByte(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the address of new column value
  @param Len the length of the addressed value
}
procedure TZAbstractCachedResultSet.UpdateBytes(ColumnIndex: Integer;
  Value: PByte; var Len: NativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBytes(ColumnIndex, Value, Len);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateShort(ColumnIndex: Integer;
  Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetShort(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateWord(ColumnIndex: Integer;
  Value: Word);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetWord(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateSmall(ColumnIndex: Integer;
  Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetSmall(ColumnIndex, Value);
end;

{**
  Updates the designated column with an <code>uint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateUInt(ColumnIndex: Integer;
  Value: Cardinal);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUInt(ColumnIndex, Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateInt(ColumnIndex: Integer;
  Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetInt(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>ulong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateULong(ColumnIndex: Integer;
  const Value: UInt64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetULong(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateLong(ColumnIndex: Integer;
  const Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetLong(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateFloat(ColumnIndex: Integer;
  Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetFloat(ColumnIndex, Value);
end;

procedure TZAbstractCachedResultSet.UpdateGUID(ColumnIndex: Integer;
  const Value: TGUID);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetGUID(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateDouble(ColumnIndex: Integer;
  const Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetDouble(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateCurrency(ColumnIndex: Integer;
  const Value: Currency);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetCurrency(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  const Value: TBCD);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBigDecimal(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>TZAnsiRec</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar; var Len: NativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if Value = nil
  then FRowAccessor.SetNull(ColumnIndex)
  else FRowAccessor.SetPAnsiChar(ColumnIndex, Value, Len);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len a pointer to the Length of the value in codepoints
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar; var Len: NativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if Value = nil
  then FRowAccessor.SetNull(ColumnIndex)
  else FRowAccessor.SetPWideChar(ColumnIndex, Value, Len);
end;


{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateString(ColumnIndex: Integer;
  const Value: String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractCachedResultSet.UpdateAnsiString(ColumnIndex: Integer;
  const Value: AnsiString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetAnsiString(ColumnIndex, Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCachedResultSet.UpdateUTF8String(ColumnIndex: Integer;
  const Value: UTF8String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUTF8String(ColumnIndex, Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateRawByteString(ColumnIndex: Integer;
  const Value: RawByteString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetRawByteString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>Widestring</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: UnicodeString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUnicodeString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TZDate);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetDate(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TZTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetTime(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateTimestamp(ColumnIndex: Integer;
  const Value: TZTimeStamp);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetTimestamp(ColumnIndex, Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  const Value: TStream);
var Blob: IZBlob;
    CLob: IZClob;
    CP: Word;
    IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if (Value = nil) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, IsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    if Blob.QueryInterface(IZCLob, Clob) = S_OK then begin
      CP := FRowAccessor.GetColumnCodePage(ColumnIndex);
      if CP = zCP_UTF16 then
        CP := GetW2A2WConversionCodePage(ConSettings);
      Clob.SetStream(Value, CP);
    end else Blob.SetStream(Value);
  end;
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZAbstractCachedResultSet.UpdateBinaryStream(
  ColumnIndex: Integer; const Value: TStream);
var Blob: IZBlob;
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if (Value = nil) or (Value.Size = 0) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, IsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    Blob.SetStream(Value);
  end;
end;

procedure TZAbstractCachedResultSet.UpdateLob(ColumnIndex: Integer;
  const Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBlob(ColumnIndex, Value);
end;

{**
  Updates the designated column with a UTF16 character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateUnicodeStream(
  ColumnIndex: Integer; const Value: TStream);
var Blob: IZBlob;
    CLob: IZClob;
    IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if (Value = nil) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, IsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    if Blob.QueryInterface(IZCLob, Clob) = S_OK
    then Clob.SetStream(Value, zCP_UTF16)
    else Blob.SetStream(Value);
  end;
end;

{**
  Updates the DefaultExpression of the designated column with a <code>String</code> value.
  This changes the behaviour of the RowAccessor used by the Resultset
  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new DefaultExpression value for the column
}
procedure TZAbstractCachedResultSet.UpdateDefaultExpression(ColumnIndex: Integer;
  const Value: string);
begin
  FNewRowAccessor.SetColumnDefaultExpression(ColumnIndex, Value);
end;

//---------------------------------------------------------------------
// Processing methods
//---------------------------------------------------------------------

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZAbstractCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  // 2018-09-16 commented out because it seems to be current policy for other
  // result sets to not raise an exception if the result set is closed but simply
  // return false here. What is our specification?
  //CheckClosed;
  if (ResultSetType = rtForwardOnly) and (Row < RowNo) then
    raise CreateForwardOnlyException;
{$ENDIF}

  if not Closed and (Row >= 0) and (Row <= LastRowNo + 1) then begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then begin
      Result := True;
      FSelectedRow := PZRowBuffer(FRowsList[Row - 1]);
      if (FSelectedRow.Index = FUpdatedRow.Index) and (FUpdatedRow.UpdateType = utModified) then
        FSelectedRow := FUpdatedRow
      else if (FSelectedRow.UpdateType = utInserted) and FCachedUpdates then begin
        Row := LocateRow(FCurrentRowsList, FSelectedRow.Index);
        if (Row >= 0) then
          FSelectedRow := FCurrentRowsList[Row];
      end;
    end else begin
      Result := False;
      FSelectedRow := nil;
    end;
    RowAccessor.RowBuffer := FSelectedRow;
  end else
    Result := False;
end;

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractCachedResultSet.RowUpdated: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utModified;
  end
  else
    Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractCachedResultSet.RowInserted: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utInserted;
  end else
    Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZAbstractCachedResultSet.RowDeleted: Boolean;
var
  UpdateType: TZRowUpdateType;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    UpdateType := PZRowBuffer(FRowsList[RowNo - 1])^.UpdateType;
    Result := UpdateType = utDeleted;
  end
  else
    Result := False;
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> object and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractCachedResultSet.InsertRow;
var TempRow: PZRowBuffer;
  Succeeded: Boolean;
begin
  CheckUpdatable;
  { Creates a new row. }
  TempRow := FRowAccessor.RowBuffer;
  FRowAccessor.Alloc;
  FRowAccessor.CopyFrom(FInsertedRow);
  FRowAccessor.RowBuffer^.UpdateType := utInserted;
  FRowAccessor.RowBuffer^.Index := GetNextRowIndex;

  AppendRow(FRowAccessor.RowBuffer);

  { Posts non-cached updates. }
  if not FCachedUpdates then begin
    Succeeded := False;
    try
      PostUpdates;
      Succeeded := True;
    finally //EH no reraising of an Exception required -> keep original stack frame i.e. MadExcept
      if not Succeeded then begin
        { Restore the previous state. See AppendRow}
        FRowAccessor.DisposeBuffer(FInitialRowsList[FInitialRowsList.Count - 1]);
        FInitialRowsList.Delete(FInitialRowsList.Count - 1);
        FRowAccessor.DisposeBuffer(FCurrentRowsList[FCurrentRowsList.Count - 1]);
        FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);
        FRowAccessor.RowBuffer := TempRow;
      end;
    end;
  end;
  FRowsList.Add(FRowAccessor.RowBuffer);
  FRowAccessor.ClearBuffer(FInsertedRow, True);

  LastRowNo := FRowsList.Count;
  MoveAbsolute(LastRowNo);
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.UpdateRow;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SCanNotUpdateEmptyRow);

  if PZRowBuffer(FRowsList[RowNo - 1]).UpdateType = utDeleted then
    raise EZSQLException.Create(SCanNotUpdateDeletedRow);

  if (FSelectedRow <> FUpdatedRow) then
      Exit;

  AppendRow(FRowsList[RowNo - 1]); //move org row to initiallist

  FSelectedRow := PZRowBuffer(FRowsList[RowNo - 1]);
  FRowAccessor.CopyBuffer(FUpdatedRow, FSelectedRow);
  FRowAccessor.RowBuffer := FSelectedRow;
  if FSelectedRow.UpdateType = utUnmodified then
    FSelectedRow.UpdateType := utModified;

  { Posts non-cached updates. }
  if not FCachedUpdates then
    PostUpdates; //EH: restoring previous state should happen by RevertRecord!
  FUpdatedRow.UpdateType := utUnmodified;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.DeleteRow;
var Succeeded: Boolean;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) or (FSelectedRow = nil) then
    raise EZSQLException.Create(SCanNotDeleteEmptyRow);

  if FSelectedRow^.UpdateType = utInserted
  then RevertRecord
  else begin
    AppendRow(FRowsList[RowNo - 1]); //copies the rows, add to FInitialRowsList and FInitialRowsList -> RevertRecord

    FSelectedRow^.UpdateType := utDeleted;
    if FSelectedRow = FUpdatedRow then
      FRowAccessor.CopyBuffer(FUpdatedRow, FRowsList[RowNo - 1]);

    { Posts non-cached updates. }
    if not FCachedUpdates then begin
      Succeeded := False;
      try
        PostUpdates;
        Succeeded := True;
      finally
        if not Succeeded then begin
          { Restores the previous state. }
          FRowAccessor.DisposeBuffer(FRowsList[RowNo - 1]);
          FRowsList[RowNo - 1] := FInitialRowsList[FInitialRowsList.Count - 1];
          FSelectedRow := FRowsList[RowNo - 1];
          FInitialRowsList.Delete(FInitialRowsList.Count - 1);
          FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);
        end;
      end;
    end;
  end;
end;

{**
  Cancels the updates made to the current row in this
  <code>ResultSet</code> object.
  This method may be called after calling an
  <code>updateXXX</code> method(s) and before calling
  the method <code>updateRow</code> to roll back
  the updates made to a row.  If no updates have been made or
  <code>updateRow</code> has already been called, this method has no
  effect.
}
procedure TZAbstractCachedResultSet.CancelRowUpdates;
begin
  if (FSelectedRow <> nil) and (FUpdatedRow.Index = FSelectedRow.Index) then begin
    FUpdatedRow.Index := -1;
    FUpdatedRow.UpdateType := utUnmodified;
  end;
  MoveAbsolute(RowNo);
end;

procedure TZAbstractCachedResultSet.MoveToInsertRow;
begin
  CheckClosed;
  FRowAccessor.RowBuffer := FInsertedRow;
end;

procedure TZAbstractCachedResultSet.MoveToCurrentRow;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
    if (FSelectedRow.Index = FUpdatedRow.Index) and (FUpdatedRow.UpdateType = utModified) then begin
      FRowAccessor.RowBuffer := FUpdatedRow;
      FSelectedRow := FUpdatedRow;
    end else FRowAccessor.RowBuffer := FSelectedRow
  else FRowAccessor.RowBuffer := nil;
end;

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TZAbstractCachedResultSet.ColumnsToJSON(ResultsWriter: {$IFDEF MORMOT2}TResultsWriter{$ELSE}TJSONWriter{$ENDIF};
  JSONComposeOptions: TZJSONComposeOptions);
begin
  FRowAccessor.ColumnsToJSON(ResultsWriter, JSONComposeOptions)
end;
{$ENDIF WITH_COLUMNS_TO_JSON}

{**
  Compares fields from two row buffers.
  @param Row1 the first row buffer to compare.
  @param Row2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZAbstractCachedResultSet.CompareRows(Row1, Row2: NativeInt;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var
  RowBuffer1, RowBuffer2: PZRowBuffer;
begin
{$IFNDEF DISABLE_CHECKING}
  if ResultSetType = rtForwardOnly then
    raise CreateForwardOnlyException;
{$ENDIF}
  RowBuffer1 := PZRowBuffer(FRowsList[Row1 - 1]);
  RowBuffer2 := PZRowBuffer(FRowsList[Row2 - 1]);
  Result := FRowAccessor.CompareBuffers(RowBuffer1, RowBuffer2,
    ColumnIndices, CompareFuncs);
end;

function TZAbstractCachedResultSet.HasServerLinkedColumns: Boolean;
begin
  Result := FRowAccessor.HasServerLinkedColumns;
end;

function TZAbstractCachedResultSet.GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
  const CompareKinds: TComparisonKindArray): TCompareFuncs;
begin
  Result := FRowAccessor.GetCompareFuncs(ColumnIndices, CompareKinds);
end;

{ TZCachedResultSet }

procedure TZCachedResultSet.CacheAllLobs;
const
  Disc: ZWideString = '[Disc]';
var
  ColumnIndex: Integer;
  SQLType: TZSQLType;
  Current: IZBlob;
  Newlob: IZBlob;
  LastNull: Boolean;
begin
  if ((FRowAccessor.LobCacheMode <> lcmOnLoad) and First) then
  begin
    repeat
      for ColumnIndex := 0 to GetColumnCount - 1 do
      begin
        SQLType := FRowAccessor.GetColumnType(ColumnIndex);
        if SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream] then
        begin
          Current := FRowAccessor.GetBlob(ColumnIndex, LastNull);
          if (Current <> nil) and not Current.IsCached then
          begin
            Newlob := TZLocalMemCLob.CreateWithData(PWideChar(Disc), 6, ConSettings, FOpenLobStreams);
            FRowAccessor.SetBlob(ColumnIndex, NewLob);
          end;
        end;
      end;
    until not Next;
  end;
end;

procedure TZCachedResultSet.ClearStatementLink;
var GenDMLResolver: IZGenerateSQLCachedResolver;
begin
  if ((FRowAccessor.LobCacheMode = lcmOnAccess)) then
    CacheAllLobs;
  if Statement <> nil then
    if not FRowAccessor.HasServerLinkedColumns and IsLastRowFetched and GetMetadata.IsMetadataLoaded then begin
      Statement.FreeOpenResultSetReference(IZResultSet(FWeakIZResultSetPtr));
      if FResultSet <> nil then begin
        FResultSet.GetMetadata.AssignColumnInfosTo(ColumnsInfo);
        TZAbstractResultSetMetadata(Metadata).SetMetadataLoaded(True);
        FResultSet.Close;
        FResultSet := nil;
      end;
      if FResolver.QueryInterface(IZGenerateSQLCachedResolver, GenDMLResolver) = S_OK then
        GenDMLResolver.SetMetadata(TZAbstractResultSetMetadata(Metadata));
      IZStatement(PPointer(@Statement)^) := nil;
    end else raise EZSQLException.Create('Resultset is not loaded or has server linked columns. Can''t unlink from statement');
end;

{**
  Creates this object and assignes the main properties.
  @param ResultSet a wrapped resultset object.
  @param Resolver a cached updates resolver object.
}
constructor TZCachedResultSet.Create(const ResultSet: IZResultSet; const SQL: string;
  const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet.GetStatement, SQL, nil, ConSettings);
  FResultSet := ResultSet;
  FResolver := Resolver;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := Resolver;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  Open;
end;

function TZCachedResultSet.Fetch: Boolean;
var TempRow: PZRowBuffer;
    Succeeded: Boolean;
begin
  if Assigned(FResultSet) and not FLastRowFetched
  then Result := FResultSet.Next
  else Result := False;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then begin
    if (FResultSet <> nil) and not FLastRowFetched then
      FResultSet.ResetCursor; //EH: clear library mem or release servercursor
    FLastRowFetched := True;
    Exit;
  end;

  TempRow := RowAccessor.RowBuffer;
  Succeeded := False;
  try
    RowAccessor.Alloc;
    RowAccessor.RowBuffer.Index := GetNextRowIndex;
    RowAccessor.RowBuffer.UpdateType := utUnmodified;
    RowAccessor.FillFromFromResultSet(FResultSet, FIndexPairList);
    RowsList.Add(RowAccessor.RowBuffer);
    LastRowNo := RowsList.Count;
    Succeeded := True;
  finally
    if not Succeeded then //OutOfMem? FetchError?
      RowAccessor.Dispose;
    RowAccessor.RowBuffer := TempRow;
  end;
end;

{**
  Fetches all of the rest rows from the wrapped result set.
}
procedure TZCachedResultSet.FetchAll;
begin
  while Fetch do;
end;

procedure TZCachedResultSet.FillColumnsInfo(const ColumnsInfo: TObjectList);
var
  I: Integer;
  MetaData: IZResultSetMetaData;
  ColumnInfo: TZColumnInfo;
begin
  MetaData := FResultSet.GetMetadata;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      Currency := Metadata.IsCurrency(I);
      Signed := Metadata.IsSigned(I);
      ColumnLabel := Metadata.GetOrgColumnLabel(I);
      Precision := Metadata.GetPrecision(I);
      Scale := Metadata.GetScale(I);
      ColumnType := Metadata.GetColumnType(I);
      ColumnCodePage := MetaData.GetColumnCodePage(I);

      If ColumnType In [stString, stUnicodeString] Then
        If ColumnCodePage = zCP_UTF16 Then ColumnType := stUnicodeString
          Else ColumnType := stString;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
end;

{**
  Opens this recordset.
}
procedure TZCachedResultSet.Open;
var
  Statement: IZStatement;
  lcmString: String;
begin
  Statement := ResultSet.GetStatement;
  if Assigned(Statement) then
  begin
    lcmString := DefineStatementParameter(Statement, DSProps_LobCacheMode, LcmNoneStr);
    FLobCacheMode := GetLobCacheModeFromString(lcmString, FLobCacheMode);
  end
  else
    FLobCacheMode := lcmNone;
  ColumnsInfo.Clear;
  FillColumnsInfo(ColumnsInfo);
  FLastRowFetched := False;
  inherited Open;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZCachedResultSet.AfterClose;
begin
  inherited AfterClose;
  If Assigned(FResultset) then begin
    FResultset.Close;
    FResultSet := nil;
  end;
end;

constructor TZCachedResultSet.CreateWithColumns(const ColumnsInfo: TObjectList;
  const ResultSet: IZResultSet; const SQL: string;
  const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet.GetStatement, SQL, nil, ConSettings);
  FResultSet := ResultSet;
  FResolver := Resolver;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := Resolver;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  ZDbcUtils.CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  inherited Open;
end;

procedure TZCachedResultSet.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var ImmediatelyReleasable: IImmediatelyReleasable;
    GenDMLResolver: IZGenerateSQLCachedResolver;
begin
  if FLastRowFetched and not HasServerLinkedColumns and GetMetadata.IsMetadataLoaded then begin
    if FResultSet <> nil then begin
      FResultSet.GetMetadata.AssignColumnInfosTo(ColumnsInfo);
      TZAbstractResultSetMetadata(Metadata).SetMetadataLoaded(True);
      if Supports(FResultSet, IImmediatelyReleasable, ImmediatelyReleasable) and
         (ImmediatelyReleasable <> Sender) then
        ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
      FResultSet := nil;
    end;
    if Statement <> nil then begin
      Statement.FreeOpenResultSetReference(IZResultSet(FWeakIZResultSetPtr));
      if Supports(Statement, IImmediatelyReleasable, ImmediatelyReleasable) and
         (ImmediatelyReleasable <> Sender) then
        ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
      IZStatement(PPointer(@Statement)^) := nil;
    end;
    if (FResolver <> nil) then begin
      if FResolver.QueryInterface(IZGenerateSQLCachedResolver, GenDMLResolver) = S_OK then begin
        GenDMLResolver.SetMetadata(TZAbstractResultSetMetadata(Metadata));
        GenDMLResolver.SetConnection(nil);
      end;
      FResolver.SetTransaction(nil);
    end;
  end else inherited ReleaseImmediat(Sender, AError);
end;

procedure TZCachedResultSet.ResetCursor;
begin
  // Mark: The original version reset the LobCacheMode here which seems incorrect.  I've left the code below in case
  //       it turns out I'm wrong on this.
  if not Closed then begin
    If Assigned(FResultset) then begin
      if not FLastRowFetched then
        FResultset.ResetCursor;
      FLastRowFetched := False;
    end;
    inherited ResetCursor;
  end;
end;
(*  // Original version.
procedure TZCachedResultSet.ResetCursor;
var
  Statement: IZStatement;
begin
  if not Closed then begin
    If Assigned(FResultset) then begin
      if not FLastRowFetched then
        FResultset.ResetCursor;
      Statement := ResultSet.GetStatement;
      if Assigned(Statement)
      then FCachedLobs := StrToBoolEx(DefineStatementParameter(Statement, DSProps_CachedLobs, 'false'))
      else FCachedLobs := True;
      FRowAccessor.CachedLobs := FCachedLobs;
      FOldRowAccessor.CachedLobs := FCachedLobs;
      FNewRowAccessor.CachedLobs := FCachedLobs;
      FLastRowFetched := False;
    end;
    inherited ResetCursor;
  end;
end;
*)

{**
  Retrieves the  number, types and properties of
  this <code>ResultSet</code> object's columns.
  @return the description of this <code>ResultSet</code> object's columns
}
function TZCachedResultSet.GetMetadata: IZResultSetMetadata;
begin
  If Assigned(FResultset)
  then Result := ResultSet.GetMetadata
  else Result := inherited GetMetadata;
end;

{**
  Moves the cursor to the end of
  this <code>ResultSet</code> object, just after the
  last row. This method has no effect if the result set contains no rows.
}
procedure TZCachedResultSet.AfterLast;
begin
  FetchAll;
  inherited AfterLast;
end;

{**
  Indicates whether the cursor is on the last row of
  this <code>ResultSet</code> object.
  Note: Calling the method <code>isLast</code> may be expensive
  because the JDBC driver
  might need to fetch ahead one row in order to determine
  whether the current row is the last row in the result set.

  @return <code>true</code> if the cursor is on the last row;
    <code>false</code> otherwise
}
function TZCachedResultSet.IsLast: Boolean;
begin
  FetchAll;
  Result := inherited IsLast;
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZCachedResultSet.Last: Boolean;
begin
  FetchAll;
  Result := inherited Last;
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if ((MaxRows > 0) and (Row > MaxRows)) then
    Exit;

  { Processes negative rows }
  if Row < 0 then begin
    FetchAll;
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end else
  { Processes moving after last row }
    while (LastRowNo < Row) and Fetch do;

  Result := inherited MoveAbsolute(Row);
end;

{ TZVirtualResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
}
constructor TZVirtualResultSet.CreateWithStatement(const SQL: string;
   const Statement: IZStatement; ConSettings: PZConSettings);
begin
  inherited CreateWithStatement(SQL, Statement, ConSettings);
  FLastRowFetched := True;
end;

class function TZVirtualResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZVirtualResultSetRowAccessor;
end;

{**
  Change Order of one Rows in Resultset
  Note: First Row = 1, to get RowNo use IZResultSet.GetRow
  @param CurrentRowNo the curren number of row
  @param NewRowNo the new number of row
}
procedure TZVirtualResultSet.ChangeRowNo(CurrentRowNo, NewRowNo: NativeInt);
var P: Pointer;
begin
  CurrentRowNo := CurrentRowNo -1;
  NewRowNo := NewRowNo -1;
  P := RowsList[CurrentRowNo];
  RowsList.Delete(CurrentRowNo);
  RowsList.Insert(NewRowNo, P);
  P := InitialRowsList[CurrentRowNo];
  InitialRowsList.Delete(CurrentRowNo);
  InitialRowsList.Insert(NewRowNo, P);
end;

function TZVirtualResultSet.ColumnSort(Item1, Item2: Pointer): Integer;
begin
  Result := RowAccessor.CompareBuffers(Item1, Item2,
    TIntegerDynArray(FColumnIndices), TCompareFuncs(FCompareFuncs));
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
procedure TZVirtualResultSet.CopyFrom(const Source: IZResultSet;
  Rows: TZSortedList; FieldPairs: TZIndexPairList);
  procedure CopyRow(RowAccessor: TZRowAccessor; Source: IZResultSet; FieldPairs: TZIndexPairList);
  var Succeeded: Boolean;
  begin
    Succeeded := False;
    try
      RowAccessor.Alloc;
      RowAccessor.RowBuffer.Index := GetNextRowIndex;
      RowAccessor.RowBuffer.UpdateType := utUnmodified;
      RowAccessor.FillFromFromResultSet(Source, FieldPairs);
      RowsList.Add(RowAccessor.RowBuffer);
      LastRowNo := RowsList.Count;
      Succeeded := True;
    finally
      if not Succeeded {Out of mem?} then
        RowAccessor.Dispose;
    end;
  end;
var I: Integer;
  Row: NativeInt;
begin
  Row := Source.GetRow;
  if Rows = nil then begin
    if Source.GetType <> rtForwardOnly then
      Source.First
    else if Source.IsBeforeFirst then
      Source.Next;
    while not Source.IsAfterLast do begin
      CopyRow(RowAccessor, Source, FieldPairs);
      Source.Next;
    end;
  end else for I := 0 to Rows.Count -1 do begin
    Source.MoveAbsolute(NativeInt(Rows[i]));
    CopyRow(RowAccessor, Source, FieldPairs);
  end;
  if Source.GetType <> rtForwardOnly then
    Source.MoveAbsolute(Row);
  BeforeFirst;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

constructor TZVirtualResultSet.CreateCloneFrom(const Source: IZResultSet);
var MetaData: IZResultSetMetadata;
begin
  MetaData := Source.GetMetadata;
  inherited Create(nil, '', nil, Source.GetConSettings);
  Metadata.AssignColumnInfosTo(ColumnsInfo);
  FCachedUpdates := False;
  FLastRowFetched := True;
  Open;
end;

constructor TZVirtualResultSet.CreateFrom(const Source: IZResultSet;
  Rows: TZSortedList; FieldPairs: TZIndexPairList; ConSettings: PZConSettings);
var MetaData: IZResultSetMetadata;
    ColumnInfo: TZColumnInfo;
    C, I: Integer;
begin
  inherited Create(nil, '', nil, ConSettings);
  Metadata := Source.GetMetadata;
  if FieldPairs <> nil
  then ColumnsInfo.Capacity := FieldPairs.Count
  else ColumnsInfo.Capacity := Metadata.GetColumnCount;
  if FieldPairs = nil
  then Metadata.AssignColumnInfosTo(ColumnsInfo)
  else for i := 0 to FieldPairs.Count-1 do begin
    C := PZIndexPair(FieldPairs[i]).SrcOrDestIndex;
    ColumnInfo := TZColumnInfo.Create;
    ColumnInfo.Currency := Metadata.IsCurrency(C);
    ColumnInfo.Signed := Metadata.IsSigned(C);
    ColumnInfo.ColumnLabel := Metadata.GetOrgColumnLabel(C);
    ColumnInfo.Precision := Metadata.GetPrecision(C);
    ColumnInfo.ColumnType := Metadata.GetColumnType(C);
    ColumnInfo.ColumnCodePage := Metadata.GetColumnCodePage(C);
    ColumnInfo.Scale := Metadata.GetScale(C);
    ColumnInfo.Nullable := Metadata.IsNullable(C);
    ColumnsInfo.Add(ColumnInfo);
  end;
  FCachedUpdates := False;
  FLastRowFetched := True;
  Open;
  CopyFrom(Source, Rows, FieldPairs);
end;

{**
  Creates this object and assignes the main properties.
  @param ColumnsInfo a columns info for cached rows.
  @param SQL an SQL query string.
}
constructor TZVirtualResultSet.CreateWithColumns(ColumnsInfo: TObjectList;
  const SQL: string; ConSettings: PZConSettings);
begin
  inherited CreateWithColumns(ColumnsInfo, SQL, ConSettings);
  FCachedUpdates := False;
  FLastRowFetched := True;
end;

{**
  Calculates column default values..
  @param RowAccessor a row accessor which contains new column values.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "NewRowAccessor" not used} {$ENDIF} // empty function - parameter not used intentionally
procedure TZVirtualResultSet.CalculateRowDefaults(RowAccessor: TZRowAccessor);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF} // empty function - parameter not used intentionally

{**
  Post changes to database server.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new or updated
    column values.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "OldRowAccessor,NewRowAccessor" not used} {$ENDIF} // empty function - parameter not used intentionally
procedure TZVirtualResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
  //NewRowAccessor.GetColumnDefaultExpression(FirstDbcIndex) //EH commented what's purpose of the line? There is no testcase!
end;
{$IFDEF FPC} {$POP} {$ENDIF} // empty function - parameter not used intentionally

procedure TZVirtualResultSet.SortRows(const ColumnIndices: TIntegerDynArray;
  Descending: Boolean);
var I: Integer;
    ComparisonKind: TComparisonKind;
begin
  SetLength(TCompareFuncs(FCompareFuncs), Length(ColumnIndices));
  if Descending
  then ComparisonKind := ckDescending
  else ComparisonKind := ckAscending;
  for i := low(ColumnIndices) to high(ColumnIndices) do
    TCompareFuncs(FCompareFuncs)[i] := RowAccessor.GetCompareFunc(ColumnIndices[I], ComparisonKind);
  fColumnIndices := Pointer(ColumnIndices);
  RowsList.Sort(ColumnSort);
  SetLength(TCompareFuncs(FCompareFuncs), 0);
end;

{ TZVirtualResultSetRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobCacheMode" not used} {$ENDIF}
constructor TZVirtualResultSetRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  LobCacheMode: TLobCacheMode);
begin
  inherited Create(ColumnsInfo, ConSettings, OpenLobStreams, lcmNone); //we need no lobs here
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ConSettings, ColumnCodePage" not used} {$ENDIF}
class function TZVirtualResultSetRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stAsciiStream, stUnicodeStream, stBinaryStream] then
    Result := TZSQLType(Byte(Result)-3); // no streams here
end;
{$IFDEF FPC} {$POP} {$ENDIF}

end.
