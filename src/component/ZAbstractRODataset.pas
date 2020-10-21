{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Read/Only Dataset component           }
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

unit ZAbstractRODataset;

interface

{$I ZComponent.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Variants, Types, SysUtils, Classes, FMTBcd, {$IFDEF WITH_SqlTimSt_UNIT}SqlTimSt,{$ENDIF}
  TypInfo, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  ZSysUtils, ZAbstractConnection, ZDbcIntfs, ZSqlStrings, ZCompatibility, ZExpression,
  ZDbcCache, ZDbcCachedResultSet, ZDatasetUtils, ZClasses
  {$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$IFDEF WITH_GENERIC_TLISTTFIELD}, Generics.Collections{$ENDIF};

type
  {$IFDEF xFPC} // fixed in r3943 or earlier 2006-06-25
  TUpdateStatusSet = set of TUpdateStatus;

  EUpdateError = class(EDatabaseError)
  end;
  {$ENDIF}

  {$IF NOT DECLARED(TRecordBuffer)}
  TRecordBuffer = {$IFDEF WITH_TRECBUF_PBYTE}TRecBuf{$ELSE}PChar{$ENDIF};
  {$IFEND}

  TGetCalcFieldsParamType = {$IFDEF WITH_GETCALCFIELDS_TRECBUF}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};

  TSortType = (stAscending, stDescending, stIgnored);   {bangfauzan addition}

  {** Options for dataset. }
  TZDatasetOption = ({$IFNDEF NO_TDATASET_TRANSLATE}doOemTranslate, {$ENDIF}
    doCalcDefaults, doAlwaysDetailResync, doSmartOpen, doPreferPrepared,
    doDontSortOnPost, doUpdateMasterFirst, doCachedLobs);

  {** Set of dataset options. }
  TZDatasetOptions = set of TZDatasetOption;

  // Forward declarations.
  TZAbstractRODataset = class;

  {** Implements a Zeos specific database exception with SQL error code. }
  EZDatabaseError = class(EDatabaseError)
  private
    FErrorCode: Integer;
    FStatusCode: String;
    FSpecificData: TZExceptionSpecificData;
    procedure SetStatusCode(const Value: String);
   public
    constructor Create(const Msg: string);
    constructor CreateFromException(E: EZSQLThrowable);
    destructor Destroy; override;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property StatusCode: String read FStatusCode write SetStatusCode;
    property SpecificData: TZExceptionSpecificData read FSpecificData;
  end;

  {** Dataset Linker class. }
  TZDataLink = class(TMasterDataLink)
  private
    FDataset: TZAbstractRODataset;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADataset: TZAbstractRODataset); {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

  {$IFNDEF WITH_TDATASETFIELD}
  TDataSetField = class;
  {$ENDIF WITH_TDATASETFIELD}
  {** Abstract dataset component optimized for read/only access. }
  {$IFDEF WITH_WIDEDATASET}
  TZAbstractRODataset = class(TWideDataSet)
  {$ELSE}
  TZAbstractRODataset = class(TDataSet)
  {$ENDIF}
  private
{$IFNDEF WITH_FUNIDIRECTIONAL}
    FUniDirectional: Boolean;
{$ENDIF}
    FCurrentRow: Integer;
    FRowAccessor, FFieldsAccessor: TZRowAccessor;
    FResultSet2AccessorIndexList: TZIndexPairList;
    FClientCP: Word;
    FOldRowBuffer: PZRowBuffer;
    FNewRowBuffer: PZRowBuffer;
    FCurrentRows: TZSortedList;
    FFetchCount: Integer;
    FFieldsLookupTable: TZFieldsLookUpDynArray;
    FRowsAffected: Integer;

    FFilterEnabled: Boolean;
    FFilterExpression: IZExpression;
    FFilterStack: TZExecutionStack;
    FFilterFieldRefs: TObjectDynArray;
    FInitFilterFields: Boolean;
    FDisableZFields: Boolean;

    FRequestLive: Boolean;
    FFetchRow: integer;    // added by Patyi

    FSQL: TZSQLStrings;
    FParams: TParams;
    FShowRecordTypes: TUpdateStatusSet;
    FOptions: TZDatasetOptions;
    FProperties: TStrings;
    FConnection: TZAbstractConnection;
    FStatement: IZPreparedStatement;
    FResultSet: IZResultSet;
    FResultSetMetadata: IZResultSetMetadata;
    FOpenLobStreams: TZSortedList;

    FRefreshInProgress: Boolean;
    //for the Date/Time/DateTimeFields: circumvent duplicate conversions
    //TBCDField:
    //circumvent a TClientDataSet BCDField bug.
    //The ClientDataSets do not call the Get/SetData overload with NativeFormat overload
    //so they use the slow TBCD record instead (while we are in Currency range)
    //and convert all values to/from the currency
    //Zitat of DB.TBCDField.GetDataSize: Integer:
    //"SizeOf(TBcd) is used here instead of SizeOf(Currency) because some
    // datasets store the currency data in TBcd format in the record buffer.
    // For these classes (TBDEDataset & TClientDataset) a call to
    // TField.GetData(Buffer, True) will return a TBcd."
    FNativeFormatOverloadCalled: array[ftBCD..ftDateTime] of Boolean;

    {FFieldDefsInitialized: boolean;}  // commented out because this causes SF#286

    FDataLink: TDataLink;
    FMasterLink: TMasterDataLink;
    FLinkedFields: string; {renamed by bangfauzan}
    FIndexFieldNames: String; {bangfauzan addition}
    FUniTemp: UnicodeString;
    {$IFNDEF UNICODE}
    FRawTemp: RawByteString;
    {$ENDIF}
    FCharEncoding: TZCharEncoding;

    FIndexFields: {$IFDEF WITH_GENERIC_TLISTTFIELD}TList<TField>{$ELSE}TList{$ENDIF};
    FCachedLobs: WordBool;
    FSortType : TSortType; {bangfauzan addition}
    FHasOutParams: Boolean;
    FLastRowFetched: Boolean;
    FSortedFields: string;
    FSortedFieldRefs: TObjectDynArray;
    FSortedFieldIndices: TIntegerDynArray;
    FSortedComparsionKinds: TComparisonKindArray;
    FSortedOnlyDataFields: Boolean;
    FCompareFuncs: TCompareFuncs;
    FSortRowBuffer1: PZRowBuffer;
    FSortRowBuffer2: PZRowBuffer;
    FPrepared: Boolean;
    FCursorOpened: Boolean;
    FResultSetWalking: Boolean;
    {$IFNDEF WITH_NESTEDDATASETS}
    FNestedDataSets: TList;
    {$ENDIF}
    {$IFNDEF WITH_NESTEDDATASETCLASS}
    FNestedDatasetClass: TDataSetClass;
    {$ENDIF}
    {$IFNDEF WITH_DATASETFIELD}
    FDataSetField: TDataSetField;
    {$ENDIF}
    {$IFNDEF WITH_OBJECTVIEW}
    FObjectView: Boolean;
    {$ENDIF WITH_OBJECTVIEW}
    {$IFNDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
    FCurrentFieldRefIndex: Integer;
    {$ENDIF}
    {$IFNDEF WITH_SPARSEARRAYS}
    FSparseArrays: Boolean;
    procedure SetSparseArrays(Value: Boolean);
    {$ENDIF WITH_SPARSEARRAYS}

    {$IFNDEF WITH_NESTEDDATASETS}
    function GetNestedDataSets: TList;
    {$ENDIF}
    procedure OnBlobUpdate(AField: NativeInt);
    function GetFieldIndex(AField: TField): Integer;
    procedure SetDisableZFields(Value: Boolean);
    function CreateFieldsLookupTable(out IndexPairList: TZIndexPairList): TZFieldsLookUpDynArray;
  private
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(Value: Boolean);
    function GetParamChar: Char;
    procedure SetParamChar(Value: Char);
    procedure SetParams(Value: TParams);
    function GetShowRecordTypes: TUpdateStatusSet;
    procedure SetShowRecordTypes(Value: TUpdateStatusSet);
    procedure SetConnection(Value: TZAbstractConnection);
    procedure SetDataSource(Value: TDataSource);
    function GetMasterFields: string;
    procedure SetMasterFields(const Value: string);
    function GetMasterDataSource: TDataSource;
    procedure SetMasterDataSource(Value: TDataSource);
    function GetLinkedFields: string; {renamed by bangfauzan}
    procedure SetLinkedFields(const Value: string);  {renamed by bangfauzan}
    function GetIndexFieldNames : String; {bangfauzan addition}
    procedure SetIndexFieldNames(const Value : String); {bangfauzan addition}
    procedure SetOptions(Value: TZDatasetOptions);
    procedure SetSortedFields(const Value: string); {bangfauzan modification}

    function GetSortType : TSortType; {bangfauzan addition}
    Procedure SetSortType(Value : TSortType); {bangfauzan addition}

    procedure UpdateSQLStrings(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

    procedure SetPrepared(Value : Boolean);
    {$IFNDEF WITH_FUNIDIRECTIONAL}
    procedure SetUniDirectional(const Value: boolean);
    {$ENDIF}
    function  GetUniDirectional: boolean;
    procedure SetProperties(const Value: TStrings); virtual;
  protected
    FTransaction: TZAbstractTransaction;
    procedure CheckOpened;
    procedure CheckConnected;
    procedure CheckBiDirectional;
    procedure CheckSQLQuery; virtual;
    procedure RaiseReadOnlyError;

    function FetchOneRow: Boolean;
    function FetchRows(RowCount: Integer): Boolean;
    function FilterRow(RowNo: NativeInt): Boolean;
    function GotoRow(RowNo: NativeInt): Boolean; // added by tohenk
    procedure RereadRows;
    procedure SetStatementParams(const Statement: IZPreparedStatement;
      const ParamNames: TStringDynArray; Params: TParams;
      DataLink: TDataLink); virtual;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure DoOnNewRecord; override;
    procedure RetrieveParamValues;
    function GetDataSource: TDataSource; override;
    procedure Prepare4DataManipulation(Field: TField);
    procedure SetTransaction(Value: TZAbstractTransaction);
  protected { Internal protected properties. }
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; virtual;
    function CreateResultSet(const {%H-}SQL: string; MaxRows: Integer):
      IZResultSet; virtual;
    {$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES} //EH: there is something weired with cirtcular references + FPC and implementation uses! So i added this virtual function to get a IsUpdatable state
    function GetUpdatable: Boolean; virtual;
    property Updatable: Boolean read GetUpdatable;
    {$ENDIF}
    {Notes by EH:
     since 7.3 the Accessor is just widening the fields for userdefined fields
     Also is the Rowbuffer used for the bookmarks
     IF no user defined fields are added the rowbuffer has a size of 9 Bytes per row.
     All Data-Fields are getting it's data from the ResultSets directly.
     So this may be a bit slower for the string-based resultsets like
     postgres or mysql with clientcursors but we do no longer waste memory with duplicate data
     also are loads of moves suppressd, the more we write all MDL changes
     into the IZCachedResultSet directly}
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
    property OldRowBuffer: PZRowBuffer read FOldRowBuffer write FOldRowBuffer;
    property NewRowBuffer: PZRowBuffer read FNewRowBuffer write FNewRowBuffer;
    property CurrentRows: TZSortedList read FCurrentRows write FCurrentRows;
    property FetchCount: Integer read FFetchCount write FFetchCount;
    property FieldsLookupTable: TZFieldsLookUpDynArray read FFieldsLookupTable
      write FFieldsLookupTable;

    property FilterEnabled: Boolean read FFilterEnabled write FFilterEnabled;
    property FilterExpression: IZExpression read FFilterExpression
      write FFilterExpression;
    property FilterStack: TZExecutionStack read FFilterStack write FFilterStack;
    property FilterFieldRefs: TObjectDynArray read FFilterFieldRefs
      write FFilterFieldRefs;
    property InitFilterFields: Boolean read FInitFilterFields
      write FInitFilterFields;

    property Statement: IZPreparedStatement read FStatement write FStatement;
    property ResultSet: IZResultSet read FResultSet write FResultSet;
    property ResultSetMetadata: IZResultSetMetadata read FResultSetMetadata;
    property ResultSetWalking: Boolean read FResultSetWalking;
  protected { External protected properties. }
    property DataLink: TDataLink read FDataLink;
    property MasterLink: TMasterDataLink read FMasterLink;
    property IndexFields: {$IFDEF WITH_GENERIC_TLISTTFIELD}TList<TField>{$ELSE}TList{$ENDIF} read FIndexFields;
    property RequestLive: Boolean read FRequestLive write FRequestLive
      default False;
    property FetchRow: integer read FFetchRow write FFetchRow default 0;  // added by Patyi
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck
      default True;
    property ParamChar: Char read GetParamChar write SetParamChar
      default ':';
    property SQL: TStrings read GetSQL write SetSQL;
    property Params: TParams read FParams write SetParams;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True;
    property ShowRecordTypes: TUpdateStatusSet read GetShowRecordTypes
      write SetShowRecordTypes default [usUnmodified, usModified, usInserted];
    property IsUniDirectional: Boolean read GetUniDirectional
      write SetUniDirectional default False;
    property Properties: TStrings read FProperties write SetProperties;
    property Options: TZDatasetOptions read FOptions write SetOptions;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MasterFields: string read GetMasterFields
      write SetMasterFields;
    property MasterSource: TDataSource read GetMasterDataSource
      write SetMasterDataSource;
    property LinkedFields: string read GetLinkedFields
      write SetLinkedFields; {renamed by bangfauzan}
    property IndexFieldNames:String read GetIndexFieldNames
      write SetIndexFieldNames; {bangfauzan addition}
    {$IFNDEF WITH_NESTEDDATASETS}
    property NestedDataSets: TList read GetNestedDataSets;
    {$ENDIF}
    {$IFNDEF WITH_NESTEDDATASETCLASS}
    property NestedDataSetClass: TDataSetClass read FNestedDataSetClass write FNestedDataSetClass;
    {$ENDIF}
  protected { Abstracts methods }
    {$IFNDEF WITH_InternalAddRecord_TRecBuf}
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    {$ELSE}
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
    {$ENDIF}
    procedure InternalDelete; override;
    procedure InternalPost; override;
    {$IFNDEF FPC}
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}); override;
    {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    function GetActiveBuffer(out RowBuffer: PZRowBuffer): Boolean;
    {$IFNDEF WITH_AllocRecBuf_TRecBuf}
    function AllocRecordBuffer: TRecordBuffer; override;
    {$ELSE}
    function AllocRecBuf: TRecBuf; override;
    {$ENDIF}
    {$IFNDEF WITH_FreeRecBuf_TRecBuf}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    {$ELSE}
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ENDIF}
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; {$IFDEF WITH_FTDATASETSUPPORT}override;{$ENDIF}
    procedure CloseBlob(Field: TField); override;

    procedure CheckFieldCompatibility(Field: TField; AFieldDef: TFieldDef); {$IFDEF WITH_CHECKFIELDCOMPATIBILITY} override;{$ENDIF}
    //procedure CreateFields; override;

    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    {$IFDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; override;
    {$ELSE}
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    {$ENDIF}
    procedure BindFields(Binding: Boolean); {$IFDEF WITH_VIRTUAL_BINDFIELDS}override;{$ENDIF}
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    {$IFDEF WITH_InternalGotoBookmark_TBookmark}
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    {$ELSE}
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    {$ENDIF}
    procedure InternalRefresh; override;
    procedure InternalHandleException; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer;
      Data:{$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF}); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer;
      Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF}); override;
{$IFNDEF WITH_VIRTUAL_DEFCHANGED}
    procedure DefChanged(Sender: TObject); virtual;
{$ENDIF}
    {$IFNDEF WITH_DATASETFIELD}
    procedure SetDataSetField(const Value: TDataSetField); virtual;
    {$ENDIF}
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): LongInt;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    {$IFNDEF WITH_OBJECTVIEW}
    procedure SetObjectView(const Value: Boolean);
    {$ENDIF WITH_OBJECTVIEW}
    procedure SetAnotherResultset(const Value: IZResultSet);
    procedure InternalSort;
    function ClearSort(Item1, Item2: Pointer): Integer;
    function HighLevelSort(Item1, Item2: Pointer): Integer;
    function LowLevelSort(Item1, Item2: Pointer): Integer;

    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure MoveRecNo(Value: Integer);
    procedure SetRecNo(Value: Integer); override;
    function IsCursorOpen: Boolean; override;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure RefreshParams; virtual;

    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
  protected
  {$IFDEF WITH_IPROVIDER}
    procedure PSStartTransaction; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    // Silvio Clecio
    {$IFDEF WITH_IPROVIDERWIDE}
    function PSGetTableNameW: WideString; override;
    function PSGetQuoteCharW: WideString; override;
    function PSGetKeyFieldsW: WideString; override;
    procedure PSSetCommandText(const CommandText: WideString); overload; override;
    procedure PSSetCommandText(const CommandText: string); overload; override;
    //??     function PSGetCommandTextW: WideString; override;
    function PSExecuteStatement(const ASQL: WideString; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
    {$ELSE}
    function PSGetTableName: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetKeyFields: string; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      {$IFDEF WITH_IProviderSupportNG}var ResultSet: TDataSet
      {$ELSE} ResultSet: Pointer = nil{$ENDIF}): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}
    function PSGetUpdateException(E: Exception;
      Prev: EUpdateError): EUpdateError; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind;
      Delta: TDataSet): Boolean; override;
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    procedure PSSetParams(AParams: TParams); override;
    function PSInTransaction: Boolean; override;
  {$ENDIF}
  protected
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF FPC}PtrInt{$ELSE}NativeInt{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FetchAll; virtual;  // added by Patyi
    procedure ExecSQL; virtual;
    function RowsAffected: LongInt;
    function ParamByName(const Value: string): TParam;

    {$IFDEF FPC} // FPC has these methods virtual plainly returning False while on Delphi they use FindRecord
    function FindFirst: Boolean; override;
    function FindLast: Boolean; override;
    function FindNext: Boolean; override;
    function FindPrior: Boolean; override;
    {$ENDIF}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function IsSequenced: Boolean; override;

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
      override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;

    function GetFieldData(Field: TField; {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}): Boolean; override;
    function GetFieldData(Field: TField; {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean): Boolean; override;
    {$IFDEF FPC}
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}); override;
    {$ENDIF}
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
      override;
    function UpdateStatus: TUpdateStatus; override;
    {$IFNDEF NO_TDATASET_TRANSLATE}
    function Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; override;
    {$ENDIF}
    procedure Prepare;
    procedure Unprepare;
  public
    property Active;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property FieldDefs stored False;
    property DbcStatement: IZPreparedStatement read FStatement;
    property DbcResultSet: IZResultSet read FResultSet;
    {$IFNDEF WITH_OBJECTVIEW}
    property ObjectView: Boolean read FObjectView write SetObjectView;
    {$ENDIF WITH_OBJECTVIEW}
    {$IFNDEF WITH_SPARSEARRAYS}
    property SparseArrays: Boolean read FSparseArrays write SetSparseArrays;
    {$ENDIF WITH_SPARSEARRAYS}
    {$IFNDEF WITH_DATASETFIELD}
    property DataSetField: TDataSetField read FDataSetField write SetDataSetField;
    {$ENDIF}
    property LastRowFetched: Boolean read FLastRowFetched;
  published
    property Transaction: TZAbstractTransaction read FTransaction
      write SetTransaction;
    property Connection: TZAbstractConnection read FConnection write SetConnection;
    property SortedFields: string read FSortedFields write SetSortedFields;
    property SortType : TSortType read FSortType write SetSortType
      default stAscending; {bangfauzan addition}
    property DisableZFields: Boolean read FDisableZFields write SetDisableZFields default False;

    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeRefresh;
    property AfterRefresh;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnFilterRecord;
    property Filter;
    property Filtered;
  public
    function NextResultSet: Boolean; virtual;
    function NextRecordSet: Boolean;
    function NextRowsAffected: Boolean;
  end;

  {$IFNDEF WITH_TFIELD_PARENTFIELD}
  TObjectField = class;
  {$ENDIF}

  TZDateField = Class(TDateField) //keep that inherited class to keep InheritsFrom(TDateField) alive
  private
    FLastFormat, FDateFormat: array[Boolean] of String;
    FLastDateSep: Char;
    FSimpleFormat: array[Boolean] of Boolean;
    FBuff: array[0..cMaxDateLen] of Char;
    FFieldIndex: Integer;
    FInvalidText: String;
    FBound: Boolean;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    function IsRowDataAvailable: Boolean;
    procedure SetInvalidText(const Value: String);
  protected
    function GetIsNull: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function FilledValueWasNull(var Value: TZDate): Boolean;
    procedure SetAsDateTime(Value: TDateTime); override;
    function GetAsDate: TZDate;
    procedure SetAsDate(const Value: TZDate);
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    {$IFDEF WITH_TSQLTIMESTAMP_RECORD}
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    {$ENDIF}
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Clear; override;
  public
    property Value: TZDate read GetAsDate write SetAsDate;
  published
    property InvalidDisplayText: String read FInvalidText write SetInvalidText;
  End;

  TZDateTimeField = Class(TDateTimeField) //keep that inherited class to keep InheritsFrom(TDateTimeField) alive
  private
    FLastFormat: array[Boolean] of String;
    FFractionFormat: array[Boolean] of String;
    FFractionLen: array[Boolean] of Integer;
    FSimpleFormat: array[Boolean] of Boolean;
    FBuff: array[0..cMaxTimeStampLen] of Char;
    FFieldIndex, FScale: Integer;
    FAdjSecFracFmt, FBound: Boolean;
    FInvalidText: String;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    procedure SetInvalidText(const Value: String);
    procedure SetAdjSecFracFmt(Value: Boolean);
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    function FilledValueWasNull(Var Value: TZTimeStamp): Boolean;
    function GetAsTimeStamp: TZTimeStamp;
    function GetAsDateTime: TDateTime; override;
    {$IFDEF WITH_TSQLTIMESTAMP_RECORD}
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    {$ENDIF}
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsTimeStamp(const Value: TZTimeStamp);
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Value: TZTimeStamp read GetAsTimeStamp write SetAsTimeStamp;
    property SecondFractionsScale: Integer read FScale;
  public
    procedure Clear; override;
  published
    property InvalidDisplayText: String read FInvalidText write SetInvalidText;
    property AdjustSecondFractionsFormat: Boolean read FAdjSecFracFmt write SetAdjSecFracFmt default True;
  End;

  TZTimeField = Class(TTimeField) //keep that inherited class to keep InheritsFrom(TTimeField) alive
  private
    FLastFormat: array[Boolean] of String;
    FFractionFormat: array[Boolean] of String;
    FFractionLen: array[Boolean] of Integer;
    FSimpleFormat: array[Boolean] of Boolean;
    FLastTimeSep: Char;
    FBuff: array[0..cMaxTimeLen] of Char;
    FFieldIndex, fScale: Integer;
    FAdjSecFracFmt, FBound: Boolean;
    FInvalidText: String;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    procedure SetInvalidText(const Value: String);
    procedure SetAdjSecFracFmt(Value: Boolean);
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function FilledValueWasNull(var Value: TZTime): Boolean;
    function GetAsTime: TZTime;
    procedure SetAsTime(const Value: TZTime);
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    property Value: TZTime read GetAsTime write SetAsTime;
    property SecondFractionsScale: Integer read fScale;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
  published
    property InvalidDisplayText: String read FInvalidText write SetInvalidText;
    property AdjustSecondFractionsFormat: Boolean read FAdjSecFracFmt write SetAdjSecFracFmt default True;
  End;

  TZBooleanField = class(TBooleanField)
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function FilledValueWasNull(var Value: Boolean): Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    function GetAsBoolean: Boolean; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetVarValue(const Value: Variant); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
  end;

  TZSmallIntField = class(TSmallIntField)
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function GetAsSmallInt: SmallInt;
    procedure SetAsSmallInt(Value: SmallInt);
    function IsRowDataAvailable: Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
  public
    procedure Clear; override;
    property Value: SmallInt read GetAsSmallInt write SetAsSmallInt;
  end;

  TZShortIntField = class({$IFDEF WITH_FTSHORTINT}TShortIntField{$ELSE}TZSmallIntField{$ENDIF})
  private
    {$IFDEF WITH_FTSHORTINT}FFieldIndex: Integer;{$ENDIF}
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    {$IFDEF WITH_FTBYTE}FBound: Boolean;{$ENDIF}
    function GetAsShortInt: ShortInt;
    procedure SetAsShortInt(Value: ShortInt);
  {$IFDEF WITH_FTSHORTINT}
    function IsRowDataAvailable: Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  {$ENDIF}
  protected
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
  {$IFDEF WITH_FTSHORTINT}
    function GetIsNull: Boolean; override;
  {$ENDIF WITH_FTSHORTINT}
  public
    {$IFDEF WITH_FTSHORTINT}procedure Clear; override;{$ENDIF}
    property Value: ShortInt read GetAsShortInt write SetAsShortInt;
  end;

  TZWordField = class(TWordField)
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function GetAsWord: Word;
    procedure SetAsWord(Value: Word);
    function IsRowDataAvailable: Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
  public
    procedure Clear; override;
    property Value: Word read GetAsWord write SetAsWord;
  published
    property ColumnIndex: Integer read FFieldIndex; //keep that for persistent fields
  end;

{ TZByteField }
  TZByteField = class({$IFDEF WITH_FTBYTE}TByteField{$ELSE}TZWordField{$ENDIF})
  private
    {$IFDEF WITH_FTBYTE}FFieldIndex: Integer;{$ENDIF}
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    {$IFDEF WITH_FTBYTE}FBound: Boolean;{$ENDIF}
    function GetAsByte: Byte;
    procedure SetAsByte(Value: Byte);
  {$IFDEF WITH_FTBYTE}
    function IsRowDataAvailable: Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  {$ENDIF}
  protected
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
  {$IFDEF WITH_FTBYTE}
    function GetIsNull: Boolean; override;
  {$ENDIF}
  public
    {$IFDEF WITH_FTBYTE}procedure Clear; override;{$ENDIF}
    property Value: Byte read GetAsByte write SetAsByte;
  end;

  { implement a signed 32 bit version ->
    Delphi LongInt on Android/unnix seems to have 8Bytes) }
  TZIntegerField = class(TIntegerField) //keep that inherited class to keep InheritsFrom(TIntegerField) alive
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function GetAsInt: Integer;
    procedure SetAsInt(Value: Integer);
    function IsRowDataAvailable: Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    function FilledValueWasNull(var Value: Integer): Boolean;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;

    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override; //delphi hardly try to convert then variant even if null
  public
    property Value: Integer read GetAsInt write SetAsInt;
  public
    procedure Clear; override;
  end;

  { TZInt64Field }
  TZInt64Field = class(TLargeintField) //keep that inherited class to keep InheritsFrom(TLargIntField) alive
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function IsRowDataAvailable: Boolean;
    function FilledValueWasNull(var Value: Largeint): Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    function GetAsLargeInt: LargeInt; {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ELSE}virtual;{$ENDIF}
    procedure SetAsLargeInt(Value: LargeInt); {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ELSE}virtual;{$ENDIF}
    function GetAsCardinal: Cardinal; virtual;
    procedure SetAsCardinal(Value: Cardinal); virtual;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
    function GetAsString: string; override;
    procedure SetAsString(const Value: String); override;
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
  public
    procedure Clear; override;
    {$IFNDEF TFIELD_HAS_ASLARGEINT}
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
    {$ENDIF}
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
  end;

  TZCardinalField = class({$IFDEF WITH_FTLONGWORD}TLongWordField{$ELSE}TZInt64Field{$ENDIF}) //keep that inherited class to keep InheritsFrom(TLongWordField) alive
  private
    {$IFDEF WITH_FTLONGWORD}FFieldIndex: Integer;{$ENDIF}
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    {$IFDEF WITH_FTLONGWORD}FBound: Boolean;{$ENDIF}
    {$IFDEF WITH_FTLONGWORD}function IsRowDataAvailable: Boolean;{$ENDIF}
  protected
    function GetAsCardinal: Cardinal; {$IFNDEF WITH_FTLONGWORD}override;{$ENDIF}
    procedure SetAsCardinal(Value: Cardinal);{$IFNDEF WITH_FTLONGWORD}override;{$ENDIF}
    {$IFDEF WITH_FTLONGWORD}function GetIsNull: Boolean; override;{$ENDIF}
    function FilledValueWasNull(var Value: Cardinal): Boolean;
    function GetAsFloat: Double; override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
    function GetAsLargeInt: Largeint; {$IF not defined(WITH_FTLONGWORD) or defined(TFIELD_HAS_ASLARGEINT)} override;{$IFEND}
    {$IFDEF WITH_FTLONGWORD}
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetAsLongWord: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}Cardinal{$ELSE}LongWord{$ENDIF}; override;
    {$ENDIF WITH_FTLONGWORD}
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    procedure SetAsLargeInt(Value: Largeint); {$IF not defined(WITH_FTLONGWORD) or defined(TFIELD_HAS_ASLARGEINT)} override;{$IFEND}
    {$IFDEF WITH_FTLONGWORD}
    procedure SetAsLongWord(Value: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}Cardinal{$ELSE}LongWord{$ENDIF}); override;
    {$ENDIF WITH_FTLONGWORD}
    procedure SetAsString(const Value: String); override;
    procedure SetVarValue(const Value: Variant); override;
  {$IFDEF WITH_FTLONGWORD}
  public
    procedure Clear; override;
  {$ENDIF WITH_FTLONGWORD}
  public
    property Value: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
  end;

  { TZUInt64Field }
  TZUInt64Field = class(TZInt64Field) //keep that inherited class to keep InheritsFrom(TLargIntField) alive
  private
    FMinValue: UInt64;
    FMaxValue: UInt64;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    function GetAsUInt64: UInt64;
    procedure SetAsUInt64(Value: UInt64);
    function FilledValueWasNull(var Value: UInt64): Boolean;
  protected
    {$IFDEF WITH_FTSINGLE}
    function GetAsSingle: Single; override;
    {$ENDIF WITH_FTSINGLE}
    function GetAsFloat: Double; override;
    {$IFDEF WITH_FTEXTENDED}
    function GetAsExtended: Extended; override;
    {$ENDIF WITH_FTEXTENDED}
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    {$IFDEF WITH_FTSINGLE}
    procedure SetAsSingle(Value: Single); override;
    {$ENDIF WITH_FTSINGLE}
    procedure SetAsFloat(Value: Double); override;
    {$IFDEF WITH_FTEXTENDED}
    procedure SetAsExtended(Value: Extended); override;
    {$ENDIF WITH_FTEXTENDED}
    procedure SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
    function GetAsCardinal: Cardinal; override;
    procedure SetAsCardinal(Value: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Value: UInt64 read GetAsUInt64 write SetAsUInt64;
    property AsUnsigedLargeInt: UInt64 read GetAsUInt64 write SetAsUInt64;
  published
    property MaxValue: UInt64 read FMaxValue write FMaxValue {$IF NOT(defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR))}default 0{$IFEND};
    property MinValue: UInt64 read FMinValue write FMinValue {$IF NOT(defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR))}default 0{$IFEND};
  end;

  TZDoubleField = Class(TFloatField)
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function IsRowDataAvailable: Boolean;
    function FilledValueWasNull(var Value: Double): Boolean;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat(Value: Double); override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  public
    procedure Clear; override;
  End;

  TZSingleField = Class({$IFDEF WITH_FTSINGLE}TSingleField{$ELSE}TZDoubleField{$ENDIF})
  private
    {$IFDEF WITH_FTSINGLE}FFieldIndex: Integer;{$ENDIF}
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    {$IFDEF WITH_FTSINGLE}FBound: Boolean;{$ENDIF}
    {$IFDEF WITH_FTSINGLE}function IsRowDataAvailable: Boolean;{$ENDIF}
    function FilledValueWasNull(var Value: Single): Boolean;
  protected
  {$IFDEF WITH_FTSINGLE}
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  {$ENDIF WITH_FTSINGLE}
    function GetAsSingle: Single; {$IFDEF WITH_FTSINGLE}override;{$ENDIF}
    procedure SetAsSingle(Value: Single); {$IFDEF WITH_FTSINGLE}override;{$ENDIF}
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  {$IFNDEF WITH_FTSingle}
    function GetAsFloat: Double; override;
    procedure SetAsFloat(Value: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    function GetIsNull: Boolean; override;
    property Value: Single read GetAsSingle write SetAsSingle;
  {$ELSE}
  public
    procedure Clear; override;
  {$ENDIF}
  End;

  TZBCDField = class(TBCDField)
  private
    FFieldIndex: Integer;
    {$IFDEF WITH_TVALUEBUFFER}FValidateBuffer: TValueBuffer; {$ENDIF}
    FBound: Boolean;
    function IsRowDataAvailable: Boolean;
    function FilledValueWasNull(var Value: Currency): Boolean;
    {$IFNDEF TFIELD_HAS_ASLARGEINT}procedure SetAsLargeInt(const Value: LargeInt); {$ENDIF}
  protected
    function GetIsNull: Boolean; override;
    function GetAsBCD: TBcd; override;
    function GetAsCurrency: Currency; override;
    function GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}; override; //delphi/FPC uses Round which fails in CPU64 with precsion loss
    function GetAsLargeInt: Largeint; {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ENDIF} //delphi/FPC uses Round which fails in CPU64 with precsion loss
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsCurrency(Value: Currency); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    {$IFNDEF WIT_ASLARGEINT}
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
    {$ENDIF}
    procedure Clear; override;
  end;

  TZFMTBCDField = class(TFMTBCDField)
  private
    FFieldIndex: Integer;
    FBound: Boolean;
    function FilledValueWasNull(var Value: TBCD): Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsBCD: TBcd; override;
    {$IFDEF WITH_FTSINGLE}function GetAsSingle: Single; override;{$ENDIF}
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsCurrency(Value: Currency); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
  end;

  TZGuidField = class(TGuidField)
  private
    FFieldIndex: Integer;
    FValidateBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Array[0..38] of Ansichar{$ENDIF};
    FBound: Boolean;
    function FilledValueWasNull(var Value: TGUID): Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetAsGuid: TGUID; {$IFDEF WITH_VIRTUAL_TFIELD_GETASGUID}override;{$ENDIF}
    procedure SetAsGuid(const Value: TGUID); {$IFDEF WITH_VIRTUAL_TFIELD_GETASGUID}override;{$ENDIF}
  protected
    function GetIsNull: Boolean; override;
    function GetAsString: String; override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
    property Value: TGUID read GetAsGuid write SetAsGuid;
  end;

  TZRawStringField = class(TStringField)
  private
    FFieldIndex: Integer;
    FBufferSize: NativeUint;
    FColumnCP: Word;
    FValidateBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}RawByteString{$ENDIF};
    FBound: Boolean;
    function IsRowDataAvailable: Boolean;
    function CreateSizeError: EZDatabaseError;
    procedure SetPWideChar(P: Pointer; Len: NativeUint);
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
    function GetIsNull: Boolean; override;
    function GetAsBoolean: Boolean; override;
    function GetDataSize: Integer; override;
    function GetAsString: String; override;
    function GetAsVariant: Variant; override;
    procedure SetAsString(const Value: String); override;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString: AnsiString; {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String: UTF8String; {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF NO_UTF8STRING}
    function GetAsRawByteString: RawByteString;
    procedure SetAsRawByteString(const Value: RawByteString);
    {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
    function GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    procedure SetAsWideString(const Value: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    {$IFEND}
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    function GetAsUnicodeString: UnicodeString; {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    procedure SetAsUnicodeString(const Value: UnicodeString); {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFDEF TFIELD_HAS_ASBYTES}
    function GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF}; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
    {$IFNDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
      property AsUnicodeString: UnicodeString read GetAsWideString write SetAsWideString;
      {$ELSE}
      property AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
      {$ENDIF}
    {$ENDIF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}

    {$IFNDEF NO_UTF8STRING}
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    {$ENDIF NO_UTF8STRING}
    {$IF not defined(NO_ANSISTRING) and not defined(WITH_ASANSISTRING)}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$IFEND}
    property AsRawByteString: RawByteString read GetAsRawByteString write SetAsRawByteString;
  end;

  TZUnicodeStringField = class(TWideStringField)
  private
    FFieldIndex: Integer;
    FColumnCP: Word;
    FValidateBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}UnicodeString{$ENDIF};
    FBound: Boolean;
    function IsRowDataAvailable: Boolean;
    function CreateSizeError: EZDatabaseError;
    procedure SetPWideChar(P: PWideChar; Len: NativeUint);
  protected
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
    function GetIsNull: Boolean; override;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString: AnsiString; {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String: UTF8String; {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF NO_UTF8STRING}
    {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
    function GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    procedure SetAsWideString(const Value: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    {$IFEND}
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    function GetAsUnicodeString: UnicodeString; {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    procedure SetAsUnicodeString(const Value: UnicodeString); {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFDEF TFIELD_HAS_ASBYTES}
    function GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF}; override;
    {$ENDIF}
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    property Value: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
      {$IFNDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      property AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
      {$ENDIF}
    {$ENDIF}
    {$IF not defined(NO_ANSISTRING) and not defined(WITH_ASANSISTRING)}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$IFEND}
    {$IFNDEF NO_UTF8STRING}
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    {$ENDIF NO_UTF8STRING}
  end;

  TZBytesField = class(TBytesField)
  private
    FFieldIndex: Integer;
    fBound: Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
  end;

  TZVarBytesField = class(TVarBytesField)
  private
    FFieldIndex: Integer;
    fBound: Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF}; {$IFDEF TFIELD_HAS_ASBYTES}override;{$ENDIF}
    procedure SetAsBytes(const Value: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF}); {$IFDEF TFIELD_HAS_ASBYTES}override;{$ENDIF}
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
    {$IFNDEF TFIELD_HAS_ASBYTES}
    property Value: TBytes read GetAsBytes write SetAsBytes;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    {$ENDIF}
  end;

  { TZRawCLobField }

  TZRawCLobField = class(TMemoField)
  private
    FFieldIndex: Integer;
    fBound: Boolean;
    FColumnCP: Word;
    function IsRowDataAvailable: Boolean;
    procedure SetPWideChar(P: Pointer; Len: NativeUint);
  public
    function GetIsNull: Boolean; override;
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
    function GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    procedure SetAsWideString(const Value: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    {$IFEND}
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    function GetAsUnicodeString: UnicodeString; {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING} override;{$ENDIF}
    procedure SetAsUnicodeString(const Value: UnicodeString); {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING} override;{$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString: AnsiString; {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String: UTF8String; {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF NO_UTF8STRING}
    function GetAsRawByteString: RawByteString;
    procedure SetAsRawByteString(const Value: RawByteString);
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    property AsUnicodeString: UnicodeString read GetAsWideString write SetAsWideString;
    {$ELSE}
      {$IFNDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      property AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    {$ENDIF NO_UTF8STRING}
    {$IF not defined(NO_ANSISTRING) and not defined(WITH_ASANSISTRING)}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$IFEND}
    procedure Clear; override;
  end;

  { TZUnicodeCLobField }

  TZUnicodeCLobField = class({$IFDEF WITH_WIDEMEMO}TWideMemoField{$ELSE}TZRawCLobField{$ENDIF})
  {$IFDEF WITH_WIDEMEMO}
  private
    FFieldIndex: Integer;
    fBound: Boolean;
    FColumnCP: Word;
    function IsRowDataAvailable: Boolean;
    procedure SetPWideChar(P: Pointer; Len: NativeUint);
  public
    function GetIsNull: Boolean; override;
    function GetAsString: String; override;
    procedure SetAsString(const Value: String); override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
    function GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    procedure SetAsWideString(const Value: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}override;{$ENDIF}
    {$IFEND}
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    function GetAsUnicodeString: UnicodeString; {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    procedure SetAsUnicodeString(const Value: UnicodeString); {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString: AnsiString; {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String: UTF8String; {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF NO_UTF8STRING}
    function GetAsRawByteString: RawByteString;
    procedure SetAsRawByteString(const Value: RawByteString);
  protected
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    property AsUnicodeString: UnicodeString read GetAsWideString write SetAsWideString;
    {$ELSE}
      {$IFNDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      property AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    {$ENDIF NO_UTF8STRING}
    {$IF not defined(NO_ANSISTRING) and not defined(WITH_ASANSISTRING)}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$IFEND}
    procedure Clear; override;
  {$ENDIF WITH_WIDEMEMO}
  end;

  TZBlobField = class(TBlobField)
  private
    FFieldIndex: Integer;
    fBound: Boolean;
    function IsRowDataAvailable: Boolean;
  protected
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure Bind(Binding: Boolean); {$IFDEF WITH_VIRTUAL_TFIELD_BIND}override;{$ENDIF}
  public
    procedure Clear; override;
  end;

  {$IFNDEF WITH_TOBJECTFIELD}
  TObjectField = class(TField)
  private
    FFields: TFields;
    FOwnedFields: TFields;
    FObjectType: string;
    FUnNamed: Boolean;
    procedure DataSetChanged;
    procedure ReadUnNamed(Reader: TReader);
    procedure WriteUnNamed(Writer: TWriter);
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    {$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
    procedure Bind({%H-}Binding: Boolean); virtual;
    {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    function GetFieldCount: Integer;
    function GetFields: TFields; virtual;
    function GetFieldValue(Index: Integer): Variant; virtual;
    function GetHasConstraints: Boolean; //override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
    procedure SetFieldKind(Value: TFieldKind); {$IFDEF WITH_VIRTUAL_SETFIELDKIND}override{$ELSE}virtual{$ENDIF};
    procedure SetFieldValue(Index: Integer; const Value: Variant); virtual;
    procedure SetParentField(AField: TObjectField);// override;
    procedure SetUnNamed(Value: Boolean); inline;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property FieldCount: Integer read GetFieldCount;
    property Fields: TFields read GetFields;
    property FieldValues[Index: Integer]: Variant read GetFieldValue
      write SetFieldValue; default;
    property UnNamed: Boolean read FUnNamed default False;
  published
    property ObjectType: string read FObjectType write FObjectType;
  end;
  {$ENDIF !WITH_TOBJECTFIELD}

{$IFNDEF WITH_TARRAYFIELD}
{ TArrayField }

  TArrayField = class(TObjectField)
  protected
    procedure Bind(Binding: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Size default 10;
  end;
{$ENDIF !WITH_TARRAYFIELD}

{ TDataSetField }
{$IFNDEF WITH_TDATASETFIELD}
  TDataSetField = class(TObjectField)
  private
    FOwnedDataSet: TDataSet;
    FNestedDataSet: TDataSet;
    FIncludeObjectField: Boolean;
    function GetNestedDataSet: TDataSet;
    procedure AssignNestedDataSet(Value: TDataSet);
    procedure SetIncludeObjectField(Value: Boolean);
  protected
    procedure Bind(Binding: Boolean); override;
    function GetCanModify: Boolean; override;
    function GetFields: TFields; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NestedDataSet: TDataSet read GetNestedDataSet;
  published
    property IncludeObjectField: Boolean read FIncludeObjectField write SetIncludeObjectField default False;
  end;
{$ENDIF}
  {$IF not declared(TFieldDefsClass)}
  TFieldDefsClass = class of TFieldDefs;
  {$IFEND}

  TZFieldDef = Class(TFieldDef)
  private
    FSQLType: TZSQLType;
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    FChildDefs: TFieldDefs;
    function GetChildDefs: TFieldDefs;
    procedure SetChildDefs(Value: TFieldDefs);
    {$ENDIF}
    (*function CreateFieldComponent(Owner: TComponent;
      ParentField: TObjectField = nil; FieldName: string = ''): TField;*)
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    function GetChildDefsClass: TFieldDefsClass; virtual;
    {$ENDIF}
  public
    constructor Create(Owner: TFieldDefs; const Name: string; FieldType: TFieldType;
      SQLType: TZSQLType; Size: Integer; Required: Boolean; FieldNo: Integer); reintroduce;
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    destructor Destroy; override;
    function HasChildDefs: Boolean;
    {$ENDIF}
    (*function CreateField(Owner: TComponent; ParentField: TObjectField = nil;
      const FieldName: string = ''; CreateChildren: Boolean = True): TField;*)
  {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
  published
    property ChildDefs: TFieldDefs read GetChildDefs write SetChildDefs stored HasChildDefs;
  {$ENDIF}
  End;

  {$IFNDEF WITH_OBJECTFIELDTYPES}
const
  ObjectFieldTypes = [ftADT, ftArray, ftReference, ftDataSet];
  {$ENDIF}

implementation

uses ZFastCode, Math, ZVariant, ZMessages,
  ZSelectSchema, ZGenericSqlToken, ZTokenizer, ZGenericSqlAnalyser, ZEncoding,
  ZDbcProperties, ZDbcResultSet
  {$IFNDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}, ZAbstractDataset{$ENDIF} //see comment of Updatable property
  {$IFDEF WITH_DBCONSTS}, DBConsts {$ELSE}, DBConst{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ EZDatabaseError }

{**
  Constructs a database exception with a string message.
  @param Msg a string message which describes the error.
}
constructor EZDatabaseError.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{**
  Constructs a database exception from TZSQLThrowable instance.
  @param E an original TZSQLThrowable instance.
}
constructor EZDatabaseError.CreateFromException(E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  ErrorCode := E.ErrorCode;
  Statuscode:= E.StatusCode;
  if E.SpecificData <> nil then
    FSpecificData := E.SpecificData.Clone;
end;

procedure EZDatabaseError.SetStatusCode(const Value: String);
begin
  FStatusCode := value;
end;

destructor EZDatabaseError.Destroy;
begin
  FreeAndNil(FSpecificData);
  inherited;
end;

function CreateFieldConvertionError(const Field: TField): EZDataBaseError;
begin
  Result := EZDataBaseError.Create(Format(SErrorConvertionField, [Field.DisplayName, GetEnumName(TypeInfo(TFieldType), Ord(Field.DataType))]));
end;

function CreateUnBoundError(const Field: TField): EZDatabaseError;
begin
  Result := EZDatabaseError.Create(Format({$IFDEF FPC}SNoDataset{$ELSE}SDataSetMissing{$ENDIF}, [Field.DisplayName]));
end;

{ TZDataLink }

{**
  Creates this dataset link object.
  @param ADataset an owner linked dataset component.
}
constructor TZDataLink.Create(ADataset: TZAbstractRODataset);
begin
  inherited Create(ADataset);
  FDataset := ADataset;
end;

{**
  Processes changes in state of linked dataset.
}
procedure TZDataLink.ActiveChanged;
begin
  if FDataset.Active and not (csDestroying in FDataset.Componentstate) then
    FDataset.RefreshParams;
end;

{**
  Processes changes in fields of the linked dataset.
  @param Field a field which was changed.
}
procedure TZDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataset.Active then
    FDataset.RefreshParams;
end;

{ TZAbstractRODataset }

{**
  Constructs this object and assignes the mail properties.
  @param AOwner a component owner.
}
constructor TZAbstractRODataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpenLobStreams := TZSortedList.Create;

  FSQL := TZSQLStrings.Create;
  TZSQLStrings(FSQL).Dataset := Self;
  TZSQLStrings(FSQL).MultiStatements := False;
  FSQL.OnChange := UpdateSQLStrings;
  FParams := TParams.Create(Self);
  FCurrentRows := TZSortedList.Create;
  BookmarkSize := SizeOf(Integer);
  FShowRecordTypes := [usModified, usInserted, usUnmodified];
  FRequestLive := False;
  FFetchRow := 0;                // added by Patyi
  FOptions := [doCalcDefaults, doPreferPrepared];
  FDisableZFields := False;

  FFilterEnabled := False;
  FProperties := TStringList.Create;
  FFilterExpression := TZExpression.Create;
  FFilterExpression.Tokenizer := CommonTokenizer;
  FFilterStack := TZExecutionStack.Create;

  FDataLink := TZDataLink.Create(Self);
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  {$IFDEF WITH_GENERIC_TLISTTFIELD}
  FIndexFields := TList<TField>.Create;
  {$ELSE}
  FIndexFields := TList.Create;
  {$ENDIF}
  {$IFNDEF WITH_NESTEDDATASETS}
  FNestedDataSets := TList.Create;
  {$ENDIF}
  {$IF defined(ZEOS_TEST_ONLY) and defined(TEST_ZFIELDS)}
  FUseZFields := True;
  {$IFEND}
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractRODataset.Destroy;
begin
  Unprepare;
  if Assigned(Connection) then
  begin
    try
      SetConnection(nil);
    except
    end;
  end;

  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FCurrentRows);
  FreeAndNil(FProperties);
  FreeAndNil(FFilterStack);

  FreeAndNil(FDataLink);
  FreeAndNil(FMasterLink);
  FreeAndNil(FIndexFields);
  {$IFNDEF WITH_NESTEDDATASETS}
  FreeAndNil(FNestedDataSets);
  {$ENDIF}
  FreeAndNil(FOpenLobStreams);
  inherited Destroy;
end;

{**
  Sets database connection object.
  @param Value a database connection object.
}
procedure TZAbstractRODataset.SetConnection(Value: TZAbstractConnection);
begin
  if FConnection <> Value then begin
    if Active then
       Close;
    Unprepare;
    if FConnection <> nil then
      FConnection.UnregisterDataSet(Self);
    FConnection := Value;
    if FConnection <> nil then begin
      FConnection.RegisterDataSet(Self);
      if FSQL.Count > 0 then begin
      {EH: force rebuild all of the SQLStrings ->
        in some case the generic tokenizer fails for several reasons like:
        keyword detection, identifier detection, Field::=x(ParamEsacaping to ":=" ) vs. Field::BIGINT (pg-TypeCasting)
        using persistent components where creation order means the datasets are
        created before the connection+protocol is available the generic
        tokenizer fails in all areas}
        FSQL.BeginUpdate;
        FSQL.EndUpdate;
      end;
    end;
  end;
end;

{**
  Gets the SQL query.
  @return the SQL query strings.
}

function TZAbstractRODataset.GetSQL: TStrings;
begin
  Result := FSQL;
end;

{$IFNDEF WITH_FUNIDIRECTIONAL}
function TZAbstractRODataset.SetUniDirectional(const Value: boolean);
begin
  FUniDirectional := Value;
end;
{$ENDIF}
{**
  Gets unidirectional state of dataset.
  @return the unidirectional flag (delphi).
}
function TZAbstractRODataset.GetUniDirectional: boolean;
begin
  Result := {$IFNDEF WITH_FUNIDIRECTIONAL}FUniDirectional{$ELSE}inherited IsUniDirectional{$ENDIF};
end;

{$IFNDEF WITH_SPARSEARRAYS}
procedure TZAbstractRODataset.SetSparseArrays(Value: Boolean);
begin
  CheckInactive;
  FSparseArrays := Value;
end;
{$ENDIF WITH_SPARSEARRAYS}

{$IFNDEF WITH_NESTEDDATASETS}
function TZAbstractRODataset.GetNestedDataSets: TList;
begin
  if FNestedDataSets = nil then
    FNestedDataSets := TList.Create;
  Result := FNestedDataSets;
end;
{$ENDIF}

type
  THackTransaction = class(TZAbstractTransaction);

procedure TZAbstractRODataset.SetTransaction(Value: TZAbstractTransaction);
begin
  CheckInactive;
  if Value <> FTransaction then begin
    if (FTransaction <> nil) then begin
      if (Statement <> nil) and (THackTransaction(FTransaction).GetIZTransaction.GetConnection <> Statement.GetConnection) then
        Statement.Close;
      FTransaction.UnregisterDataSet(Self);
    end;
    FTransaction := Value;
    if FTransaction <> nil then begin
      FTransaction.RegisterDataSet(Self);
    end;
  end;
end;

{**
  Sets a new SQL query.
  @param Value a new SQL query.
}
procedure TZAbstractRODataset.SetSQL(Value: TStrings);
begin
  FSQL.Assign(Value);
end;

{**
  Gets a parameters check value.
  @return a parameters check value.
}
function TZAbstractRODataset.GetParamCheck: Boolean;
begin
  Result := FSQL.ParamCheck;
end;

{**
  Sets a new parameters check value.
  @param Value a parameters check value.
}
procedure TZAbstractRODataset.SetParamCheck(Value: Boolean);
begin
  if Value <> FSQL.ParamCheck then begin
    FSQL.ParamCheck := Value;
    UpdateSQLStrings(FSQL);
  end;
end;

{**
  Gets a parameters marker.
  @return a parameter marker.
}
function TZAbstractRODataset.GetParamChar: Char;
begin
  Result := FSQL.ParamChar;
end;

{**
  Sets a new parameter marker.
  @param Value a parameter marker.
}
procedure TZAbstractRODataset.SetParamChar(Value: Char);
begin
  if Value <> FSQL.ParamChar then begin
    FSQL.ParamChar := Value;
    UpdateSQLStrings(FSQL);
  end;
end;

{**
  Sets a new set of parameters.
  @param Value a set of parameters.
}
procedure TZAbstractRODataset.SetParams(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{**
  Defines a persistent dataset properties.
  @param Filer a persistent manager object.
}
procedure TZAbstractRODataset.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZAbstractRODataset(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{**
  Reads parameter data from persistent storage.
  @param Reader an input data stream.
}
procedure TZAbstractRODataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{**
  Writes parameter data from persistent storage.
  @param Writer an output data stream.
}
procedure TZAbstractRODataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{**
  Gets a SQL parameter by its name.
  @param Value a parameter name.
  @return a found parameter object.
}
function TZAbstractRODataset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

{**
  Updates parameters from SQL statement.
  @param Sender an event sender object.
}
procedure TZAbstractRODataset.UpdateSQLStrings(Sender: TObject);
var
  I: Integer;
  OldParams: TParams;
begin
  FieldDefs.Clear;
  if Active
  then Close
  else if assigned(Statement) then begin
    Statement.Close;
    Statement := nil;
  end;

  UnPrepare;
  if (csLoading in ComponentState) then
    Exit;
  OldParams := TParams.Create;
  OldParams.Assign(FParams);
  FParams.Clear;

  try
    for I := 0 to TZSQLStrings(Sender).ParamCount - 1 do
      FParams.CreateParam(ftUnknown, TZSQLStrings(Sender).ParamNames[I], ptUnknown);
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

{**
  Gets the ReadOnly property.
  @return <code>True</code> if the opened result set read only.
}
function TZAbstractRODataset.GetReadOnly: Boolean;
begin
  Result := not RequestLive;
end;

{**
  Sets a new ReadOnly property.
  @param Value <code>True</code> to set result set read-only.
}
procedure TZAbstractRODataset.SetReadOnly(Value: Boolean);
begin
  RequestLive := not Value;
end;

{**
  Gets a visible updated records types.
  @param return visible UpdateRecordTypes value.
}
function TZAbstractRODataset.GetShowRecordTypes: TUpdateStatusSet;
begin
  Result := FShowRecordTypes;
end;

{**
  Sets a new visible updated records types.
  @param Value a new visible UpdateRecordTypes value.
}
procedure TZAbstractRODataset.SetShowRecordTypes(Value: TUpdateStatusSet);
begin
  if Value <> FShowRecordTypes then
  begin
    FShowRecordTypes := Value;
    RereadRows;
  end;
end;

{**
  Checks if this dataset is opened.
}
procedure TZAbstractRODataset.CheckOpened;
begin
  if not Active then
    DatabaseError(SOperationIsNotAllowed4);
end;

{**
  Checks if the database connection is assigned
  and tries to connect.
}
procedure TZAbstractRODataset.CheckConnected;
begin
  if Connection = nil then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);
  Connection.Connect;
end;

{**
  Checks is the database has bidirectional access.
}
procedure TZAbstractRODataset.CheckBiDirectional;
begin
  if IsUniDirectional then
    raise EZDatabaseError.Create(SOperationIsNotAllowed1);
end;

{**
  Checks the correct SQL query.
}
procedure TZAbstractRODataset.CheckSQLQuery;
begin
  if FSQL.StatementCount < 1 then
    raise EZDatabaseError.Create(SQueryIsEmpty);
  if FSQL.StatementCount > 1 then
    raise EZDatabaseError.Create(SCanNotExecuteMoreQueries);
end;

{**
  Raises an error 'Operation is not allowed in read-only dataset.
}
procedure TZAbstractRODataset.RaiseReadOnlyError;
begin
  raise EZDatabaseError.Create(SOperationIsNotAllowed2);
end;

{**
  Fetches specified number of records.
  @param RowCount a specified number of rows to be fetched.
  @return <code>True</code> if all required rows were fetched.
}
function TZAbstractRODataset.FetchRows(RowCount: Integer): Boolean;
begin
  if (CurrentRows.Count < RowCount) or (RowCount = 0) then
    if FLastRowFetched
    then Result := CurrentRows.Count >= RowCount
    else begin
      Connection.ShowSQLHourGlass;
      try
        if (RowCount = 0) then begin
          while FetchOneRow do;
          Result := True;
        end else begin
          while (CurrentRows.Count < RowCount) do
            if not FetchOneRow then
              Break;
          Result := CurrentRows.Count >= RowCount;
        end;
      finally
        Connection.HideSQLHourGlass;
      end;
    end
  else Result := True;
end;

{**
  Fetches one row from the result set.
  @return <code>True</code> if record was successfully fetched.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.FetchOneRow: Boolean;
begin
  if Assigned(ResultSet) then
    repeat
      if (FetchCount = 0) or (ResultSet.GetRow = FetchCount) or
          ResultSet.MoveAbsolute(FetchCount)
      then begin
        Result := ResultSet.Next;
        FLastRowFetched := not Result;
      end else Result := False;
      if Result then begin
        Inc(FFetchCount);
        if FilterRow(ResultSet.GetRow) then
          CurrentRows.Add(Pointer(ResultSet.GetRow))
        else
          Continue;
      end;
    until True
  else
    Result := False;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Checks the specified row with the all filters.
  @param RowNo a number of the row.
  @return <code>True</code> if the row sutisfy to all filters.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.FilterRow(RowNo: NativeInt): Boolean;
var
  I: Integer;
  SavedRow: Integer;
  SavedRows: TZSortedList;
  SavedState: TDatasetState;
begin
  Result := True;

  { Locates the result set to the specified row. }
  if ResultSet.GetRow <> RowNo then
  begin
    if not ResultSet.MoveAbsolute(RowNo) then
      Result := False;
  end;
  if not Result then
     Exit;

  { Checks record by ShowRecordType }
  if ResultSet.RowUpdated then
    Result := usModified in ShowRecordTypes
  else if ResultSet.RowInserted then
    Result := usInserted in ShowRecordTypes
  else if ResultSet.RowDeleted then
    Result := usDeleted in ShowRecordTypes
  else
    Result := usUnmodified in ShowRecordTypes;
  if not Result then
     Exit;

  { Check master-detail links }
  if MasterLink.Active then
    for I := 0 to MasterLink.Fields.Count - 1 do begin
      if I < IndexFields.Count then
        Result := CompareKeyFields(TField(IndexFields[I]), ResultSet,
          TField(MasterLink.Fields[I]));
      if not Result then
        Break;
    end;
  if not Result then
     Exit;

  { Checks record by OnFilterRecord event }
  if FilterEnabled and Assigned(OnFilterRecord) then
  begin
    SavedRow := CurrentRow;
    SavedRows := CurrentRows;
    CurrentRows := TZSortedList.Create;

    SavedState := SetTempState(dsNewValue);
    CurrentRows.Add(Pointer(RowNo));
    CurrentRow := 1;

    try
      OnFilterRecord(Self, Result);
    except
      if Assigned(ApplicationHandleException)
      then ApplicationHandleException(Self);
    end;

    CurrentRow := SavedRow;
    {$IFDEF AUTOREFCOUNT}
    CurrentRows := nil;
    {$ELSE}
    CurrentRows.Free;
    {$ENDIF}
    CurrentRows := SavedRows;
    RestoreState(SavedState);

  end;
  if not Result then
     Exit;

  { Check the record by filter expression. }
  if FilterEnabled and (FilterExpression.Expression <> '') then begin
    if not InitFilterFields then begin
      FilterFieldRefs := DefineFilterFields(Self, FilterExpression);
      InitFilterFields := True;
    end;
    CopyDataFieldsToVars(FilterFieldRefs, ResultSet,
      FilterExpression.DefaultVariables);
    Result := FilterExpression.VariantManager.GetAsBoolean(
      FilterExpression.Evaluate4(FilterExpression.DefaultVariables,
      FilterExpression.DefaultFunctions, FilterStack));
  end;
  if not Result then
     Exit;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Go to specified row.
  @param RowNo a number of the row.
  @return <code>True</code> if the row successfully located.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.GotoRow(RowNo: NativeInt): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := CurrentRows.IndexOf(Pointer(RowNo));
  if Index >= 0 then
  begin
    if Index < CurrentRow then
      CheckBiDirectional;
    CurrentRow := Index + 1;
    Result := True;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Rereads all rows and applies a filter.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
procedure TZAbstractRODataset.RereadRows;
var I, RowNo: NativeInt;
begin
  if not (State in [dsInactive]) and not IsUniDirectional then
  begin
    UpdateCursorPos; //see http://sourceforge.net/p/zeoslib/tickets/89/
    if (CurrentRow > 0) and (CurrentRow <= CurrentRows.Count) and
       (CurrentRows.Count > 0) then
      RowNo := NativeInt(CurrentRows[CurrentRow - 1])
    else
      RowNo := -1;
    CurrentRows.Clear;

    for I := 1 to FetchCount do
      if FilterRow(I) then
        CurrentRows.Add(Pointer(I));

    CurrentRow := CurrentRows.IndexOf(Pointer(RowNo)) + 1;
    CurrentRow := Min(Max(1, CurrentRow), CurrentRows.Count);

    if FSortedFields <> '' then
      InternalSort
    else
      Resync([]);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Retrieves parameter values from prepared statement.
}
{$IFDEF FPC} {$PUSH} {$WARN 4057 off : Local variable "BCD" does not seem to be initialized} {$ENDIF}
procedure TZAbstractRODataset.RetrieveParamValues;
var
  I: Integer;
  Param: TParam;
  TempBlob: IZBlob;
  P: Pointer;
  L: NativeUint;
  R: RawByteString;
  BCD: TBCD;
  {$IFDEF WITH_TVALUEBUFFER}
  VB: TValueBuffer;
  {$ENDIF}
begin
  for I := 0 to Params.Count - 1 do begin
    Param := Params[I];

    if not (Param.ParamType in [ptResult, ptOutput, ptInputOutput]) then
      Continue;

    if Statement.IsNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then
      Param.Clear
    else
      case Param.DataType of
        ftBoolean:
          Param.AsBoolean := Statement.GetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$IFDEF WITH_FTBYTE}
        ftByte:
          Param.AsByte := Statement.GetByte(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF WITH_FTBYTE}
        {$IFDEF WITH_FTSHORTINT}
        ftShortInt:
          Param.AsShortInt := Statement.GetShort(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF WITH_FTSHORTINT}
        {$IFDEF WITH_FTSHORTINT}
        ftWord:
          Param.AsWord := Statement.GetWord(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF WITH_FTSHORTINT}
        ftSmallInt:
          Param.AsSmallInt := Statement.GetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$IFDEF WITH_FTLONGWORD}
        ftLongWord:
          Param.AsLongWord := Statement.GetUInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF WITH_FTLONGWORD}
        ftInteger, ftAutoInc:
          Param.AsInteger := Statement.GetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$IFDEF WITH_PARAM_ASLARGEINT}
        ftLargeInt:
          Param.AsLargeInt := Statement.GetLong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF}
        {$IFDEF WITH_FTSINGLE}
        ftSingle:
          Param.AsSingle := Statement.GetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF WITH_FTSINGLE}
        ftCurrency, ftFloat:
          Param.AsFloat := Statement.GetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$IFDEF WITH_FTEXTENDED}
        ftExtended:
          Param.AsFloat := Statement.GetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        {$ENDIF}
        ftBCD:
          Param.AsCurrency := Statement.GetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftFmtBCD: begin
            Statement.GetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BCD{%H-});
            Param.AsFMTBCD := BCD;
          end;
        ftString:
          begin
            Param.AsString := Statement.GetString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
            {$IFDEF UNICODE}Param.DataType := ftString;{$ENDIF} //Hack: D12_UP sets ftWideString on assigning a UnicodeString
          end;
        ftWideString:
          {$IFDEF WITH_PARAM_ASWIDESTRING}Param.AsWideString{$ELSE}Param.Value{$ENDIF} := Statement.GetUnicodeString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftMemo:
          begin
            Param.AsMemo := Statement.GetString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
            {$IFDEF UNICODE}Param.DataType := ftMemo;{$ENDIF} //Hack: D12_UP sets ftWideMemo on assigning a UnicodeString
          end;
        {$IFDEF WITH_WIDEMEMO}
        ftWideMemo:
        begin
          Param.AsWideString := Statement.GetUnicodeString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
          Param.DataType := ftWideMemo;
        end;
        {$ENDIF}
        ftBytes, ftVarBytes, ftGuid:
          Param.Value := Statement.GetBytes(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftDate:
          Param.AsDate := Statement.GetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftTime:
          Param.AsTime := Statement.GetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftDateTime:
          Param.AsDateTime := Statement.GetTimestamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        ftBlob:
          begin
            TempBlob := Statement.GetValue(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).VInterface as IZBlob;
            if not TempBlob.IsEmpty then begin
              R := '';
              P := TempBlob.GetBuffer(R, L);
              {$IFDEF WITH_TVALUEBUFFER}
              SetLength(VB, L);
              Move(P^, Pointer(VB)^, L);
              Param.SetBlobData(VB, L);
              {$ELSE}
              Param.SetBlobData({$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ENDIF}(P), L);
              {$ENDIF}
            end else
            Param.Clear;
            TempBlob := nil;
          end
        else
           raise EZDatabaseError.Create(SUnKnownParamDataType);
      end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
  @param DataLink a datalink to get parameters.
}
procedure TZAbstractRODataset.SetStatementParams(const Statement: IZPreparedStatement;
  const ParamNames: TStringDynArray; Params: TParams; DataLink: TDataLink);
var
  I: Integer;
  TempParam, Param: TParam;
  Dataset: TDataset;
  Field: TField;
begin
  if DataLink.Active then
    Dataset := DataLink.DataSet
  else
    Dataset := nil;

  if (not ParamCheck) and (not Assigned(ParamNames)) and (FParams.Count > 0) then begin
    for I := 0 to Params.Count -1 do begin
      Param := Params[i];
      if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
        Continue;
      SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, Param);
    end;
  end else begin
    TempParam := TParam.Create(nil);
    try
      for I := Low(ParamNames) to High(ParamNames) do
      begin
        if Assigned(Dataset) then
          Field := Dataset.FindField(ParamNames[I])
        else
          Field := nil;

        if Assigned(Field) then
        begin
          TempParam.AssignField(Field);
          Param := TempParam;
        end
        else
        begin
          Param := Params.FindParam(ParamNames[I]);
          if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
            Continue;
        end;

        SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, Param);
      end;
    finally
      TempParam.Free;
    end;
  end;
end;

{**
  Locates a specified record in dataset.
  @param Buffer a record buffer to put the contents of the row.
  @param GetMode a location mode.
  @param DoCheck flag to perform checking.
  @return a location result.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowNo: NativeInt;
begin
  // mad stub for unidirectional (problem in TDataSet.MoveBuffer) - dont know about FPC
  // we always use same TDataSet-level buffer, because we can see only one row
  {$IFNDEF WITH_UNIDIRECTIONALBUG}
  if IsUniDirectional then
    Buffer := TRecordBuffer(Buffers[0]);
  {$ENDIF}

  Result := grOK;
  case GetMode of
    gmNext:
      begin
        if FetchRows(CurrentRow + 1) then
          CurrentRow := CurrentRow + 1
        else
          Result := grEOF;
      end;
    gmPrior:
      begin
        CheckBiDirectional;
        if (CurrentRow > 1) and (CurrentRows.Count > 0) then
          CurrentRow := CurrentRow - 1
        else
          Result := grBOF;
      end;
    gmCurrent:
      begin
        if CurrentRow < CurrentRows.Count then
          CheckBiDirectional;

        if CurrentRow = 0 then
        begin
          if CurrentRows.Count = 0 then
            FetchRows(1);
          CurrentRow := Min(CurrentRows.Count, 1);
        end
        else if not FetchRows(CurrentRow) then
          CurrentRow := Max(1, Min(CurrentRows.Count, CurrentRow));

        if CurrentRows.Count = 0 then
          Result := grError;
      end;
  end;

  if Result = grOK then
  begin
    RowNo := NativeInt(CurrentRows[CurrentRow - 1]);
    if (ResultSet.GetRow <> RowNo) then
      ResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := PZRowBuffer(Buffer);
    RowAccessor.RowBuffer^.Index := RowNo;
    FRowAccessor.RowBuffer^.BookmarkFlag := Byte(bfCurrent);
    GetCalcFields(TGetCalcFieldsParamType(Buffer));
  end;

  if (Result = grError) and DoCheck then
    raise EZDatabaseError.Create(SNoMoreRecords);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the current record buffer depended on the current dataset state.
  @param RowBuffer a reference to the result row buffer.
  @return <code>True</code> if the buffer was defined.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.GetActiveBuffer(
  out RowBuffer: PZRowBuffer): Boolean;
var
  RowNo: NativeInt;
  procedure TryMoveToInitialRow;
  var CachedResultSet: IZCachedResultSet; //keep intf out of main method -> no _IntfClear here
  begin
    if (ResultSet.QueryInterface(IZCachedResultSet, CachedResultSet) = S_OK)
    then CachedResultSet.MoveToInitialRow
    else ResultSet.MoveToCurrentRow;
  end;
begin
  RowBuffer := nil;
  case State of
    dsBrowse,dsBlockRead:
        if not IsEmpty then begin
          RowBuffer := PZRowBuffer(ActiveBuffer);
          if RowBuffer.Index <> FResultSet.GetRow then
            FResultSet.MoveAbsolute(RowBuffer.Index);
        end;
    dsEdit: begin
        RowBuffer := PZRowBuffer(ActiveBuffer);
        if RowBuffer.Index <> FResultSet.GetRow
        then FResultSet.MoveAbsolute(RowBuffer.Index)
        else ResultSet.MoveToCurrentRow; //in case Old/New/CurValue was called before -- quirky grids..
      end;
    dsInsert: begin
        RowBuffer := PZRowBuffer(ActiveBuffer);
        if (RowBuffer.BookmarkFlag in [Byte(bfEOF){append row},Byte(bfInserted)])
        then FResultSet.MoveToInsertRow
        else FResultSet.MoveAbsolute(RowBuffer.Index);
      end;
    dsCalcFields: RowBuffer := PZRowBuffer(CalcBuffer);
    dsOldValue: if not IsEmpty then begin
        RowBuffer := PZRowBuffer(ActiveBuffer);
        if (RowBuffer.BookMarkFlag >= Byte(bfEOF)) then
            //there is no OldValue for an inserted/appended row -> tag no Data
          RowBuffer := nil
        else begin
          RowBuffer := OldRowBuffer;
          RowNo := NativeInt(CurrentRows[CurrentRow - 1]);
          if RowNo <> ResultSet.GetRow then
            CheckBiDirectional;
          if (ResultSet.GetRow = RowNo) or ResultSet.MoveAbsolute(RowNo) then begin
            RowBuffer.Index := RowNo;
            TryMoveToInitialRow;
          end else RowBuffer := nil;
        end;
      end;
    dsNewValue, dsCurValue: begin
        RowBuffer := PZRowBuffer(ActiveBuffer);
        if (RowBuffer.BookMarkFlag >= Byte(bfEOF)) then begin
          RowBuffer := NewRowBuffer;
          ResultSet.MoveToInsertRow;
        end else begin
          RowNo := NativeInt(CurrentRows[CurrentRow - 1]);
          if RowNo <> ResultSet.GetRow then
            CheckBiDirectional;
          RowBuffer := NewRowBuffer;
          if (ResultSet.GetRow = RowNo) or ResultSet.MoveAbsolute(RowNo) then
            ResultSet.MoveToCurrentRow;
          if (RowBuffer.Index <> RowNo) then begin
            RowAccessor.RowBuffer := RowBuffer;
            RowAccessor.Clear;
            RowBuffer.Index := RowNo;
          end;
        end;
      end;
    {$IFDEF FPC}else; {$ENDIF}
  end;
  Result := RowBuffer <> nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
function TZAbstractRODataset.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
{$ELSE}
function TZAbstractRODataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
var FieldDef: TFieldDef;
begin
  if FCurrentFieldRefIndex >= FieldDefs.Count then begin
    { propably a user defined field added to FieldList but not TFieldDefs }
    Result := inherited GetFieldClass(FieldType);
    Exit;
  end;
  FieldDef := FieldDefs[FCurrentFieldRefIndex];
  Inc(FCurrentFieldRefIndex);
{$ENDIF}
  if not FieldDef.InternalCalcField and (FieldDef is TZFieldDef) then begin
    case {$IFDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}FieldDef.DataType{$ELSE}FieldType{$ENDIF} of
      ftBoolean: Result := TZBooleanField;
      {$IFDEF WITH_FTBYTE}
      ftByte: Result := TZByteField;
      {$ENDIF WITH_FTBYTE}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt: Result := TZShortIntField;
      {$ENDIF WITH_FTSHORTINT}
      ftSmallInt: {$IFNDEF WITH_FTSHORTINT}
          if TZFieldDef(FieldDef).FSQLType = stShort
          then Result := TZShortIntField
          else {$ENDIF WITH_FTSHORTINT} Result := TZSmallIntField;
      ftWord: {$IFNDEF WITH_FTSHORTINT}
          if TZFieldDef(FieldDef).FSQLType = stByte
          then Result := TZByteField
          else {$ENDIF WITH_FTSHORTINT} Result := TZWordField;
      ftInteger: Result := TZIntegerField;
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord: Result := TZCardinalField;
      {$ENDIF WITH_FTLONGWORD}
      ftTime: Result := TZTimeField;
      ftDate: Result := TZDateField;
      ftDateTime: Result := TZDateTimeField;
      ftLargeInt: if TZFieldDef(FieldDef).FSQLType = stLong
          then Result := TZInt64Field
          else {$IFNDEF WITH_FTLONGWORD}if TZFieldDef(FieldDef).FSQLType = stLongWord
            then Result := TZCardinalField
            else {$ENDIF WITH_FTLONGWORD}Result := TZUInt64Field;
      {$IFDEF WITH_FTSINGLE}
      ftSingle: Result := TZSingleField;
      {$ENDIF WITH_FTSINGLE}
      ftFloat: {$IFNDEF WITH_FTSINGLE} if TZFieldDef(FieldDef).FSQLType = stFloat
          then Result := TZSingleField
          else {$ENDIF WITH_FTSINGLE}Result := TZDoubleField;
      ftBCD: Result := TZBCDField;
      ftFmtBCD: Result := TZFMTBcdField;
      ftGUID: Result := TZGUIDField;
      ftString: Result := TZRawStringField;
      ftWideString: Result := TZUnicodeStringField;
      ftBytes: Result := TZBytesField;
      ftVarBytes: Result := TZVarBytesField;
      ftMemo: {$IFNDEF WITH_WIDEMEMO} if TZFieldDef(FieldDef).FSQLType = stUnicodeStream
        then Result := TZUnicodeCLobField
        else {$ENDIF WITH_WIDEMEMO}Result := TZRawCLobField;
      {$IFDEF WITH_WIDEMEMO}
      ftWideMemo: Result := TZUnicodeCLobField;
      {$ENDIF WITH_WIDEMEMO}
      else {ftBlob} Result := TZBLobField;
    end;
  end else
    {$IFDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
    Result := inherited GetFieldClass(FieldDef);
    {$ELSE}
    Result := inherited GetFieldClass(FieldType);
    {$ENDIF}
end;

function TZAbstractRODataset.GetFieldData(Field: TField;
  {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer:
  {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
  NativeFormat: Boolean): Boolean;
begin
  if Field.DataType in [ftWideString, ftBCD, ftDate, ftTime, ftDateTime] then begin
    NativeFormat := True;
    if (Field.DataType <> ftWideString) then
      FNativeFormatOverloadCalled[Field.DataType] := True;
  end;
  Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

var D1M1Y1: TDateTime;
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
{**
  Retrieves the column value and stores it into the field buffer.
  @param Field an field object to be retrieved.
  @param Buffer a field value buffer.
  @return <code>True</code> if non-null value was retrieved.
}
function TZAbstractRODataset.GetFieldData(Field: TField;
  {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer:
    {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}): Boolean;
var
  ColumnIndex: Integer;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  S: TTimeStamp absolute TS;
  UID: TGUID absolute TS;
  DT: TDateTime;
  bLen: NativeUInt;
  P: Pointer;
  RowBuffer: PZRowBuffer;
  FieldCP, ColumnCP: Word;
{$IFDEF WITH_TVALUEBUFFER}label jmpReal10RS, jmpReal10RA;{$ENDIF}
  label jmpMoveW, jmpMoveA;
  label jmpMovDate, jmpMovTime, jmpMovDateTime, jmpMovBts, jmpMovVarBts;
begin
  if GetActiveBuffer(RowBuffer) then begin
    ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;
    if Field.FieldKind = fkData then
      if Buffer <> nil then begin
        case Field.DataType of
          ftString: begin
              ColumnCP := FResultSetMetadata.GetColumnCodePage(ColumnIndex);
              if ((ColumnCP = zCP_UTF16) or TStringField(Field).Transliterate) or (FCharEncoding = ceUTF16) then begin
                FieldCP  := GetTransliterateCodePage(Connection.ControlsCodePage);
                P := FResultSet.GetPWideChar(ColumnIndex, blen);
                Result := P <> nil;
                if Result then begin
                  blen := PUnicode2PRawBuf(P, Pointer(Buffer), blen, Field.DataSize-1, FieldCP);
                  PByte(PAnsiChar(Buffer)+blen)^ := 0;
                end else
                  PByte(Buffer)^ := 0;
                Exit;
              end else begin
                P := FResultSet.GetPAnsiChar(ColumnIndex, blen);
                Result := P <> nil;
                goto jmpMoveA;
              end;
            end;
          ftSmallint: PSmallInt(Buffer)^ := FResultSet.GetSmall(ColumnIndex);
          ftInteger, ftAutoInc: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PInteger{$ELSE}PLongInt{$ENDIF}(Buffer)^ := ResultSet.GetInt(ColumnIndex);
          ftBoolean: PWordBool(Buffer)^ := ResultSet.GetBoolean(ColumnIndex);
          ftWord: FResultSet.GetWord(ColumnIndex);
          ftFloat, ftCurrency: PDouble(Buffer)^ := FResultSet.GetDouble(ColumnIndex);
          ftBcd: begin
              if FNativeFormatOverloadCalled[ftBcd] //circumvent the T[BDE/Client]DataSet bug...
              then PCurrency(Buffer)^ := FResultSet.GetCurrency(ColumnIndex)
              else ResultSet.GetBigDecimal(ColumnIndex, PBCD(Buffer)^);
              FNativeFormatOverloadCalled[ftBcd] := False;
            end;
          ftDate: begin
                    FResultSet.GetDate(ColumnIndex, D);
                    Result := FResultSet.WasNull;
                    goto jmpMovDate;
                  end;
          { Processes DateTime fields. }
          ftTime: begin
                    ResultSet.GetTime(ColumnIndex, T{%H-});
                    Result := ResultSet.WasNull;
                    goto jmpMovTime;
                  end;
          ftDateTime: begin
                    ResultSet.GetTimeStamp(ColumnIndex, TS);
                    Result := ResultSet.WasNull;
                    goto jmpMovDateTime;
                  end;
          { Processes binary fields. }
          ftBytes: begin
              P := ResultSet.GetBytes(ColumnIndex, bLen);
              Result := ResultSet.WasNull;
              goto jmpMovBts;
            end;
          ftVarBytes: begin
              P := ResultSet.GetBytes(ColumnIndex, bLen);
              Result := ResultSet.WasNull;
              goto jmpMovVarBts;
            end;
          { Processes String fields. }
          ftWideString: begin
              P := FResultSet.GetPWideChar(ColumnIndex, bLen);
              Result := P <> nil;
              goto jmpMoveW;
            end;
          ftLargeInt: if FResultSetMetadata.GetColumnType(ColumnIndex) = stULong
            then PUInt64(Buffer)^ := ResultSet.GetULong(ColumnIndex)
            else PInt64(Buffer)^ := ResultSet.GetLong(ColumnIndex);
          {$IFDEF WITH_FTGUID}
          ftGUID:
            begin
              ResultSet.GetGUID(ColumnIndex, UID);
              if ResultSet.WasNull
              then PByte(Buffer)^ := 0
              else GUIDToBuffer(@UID.D1, PAnsiChar(Buffer), [guidWithBrackets, guidSet0Term]);
            end;
          {$ENDIF}
          {$IFDEF WITH_FTTIMESTAMP_FIELD}
          ftTimeStamp: begin
                        FResultSet.GetTimeStamp(ColumnIndex, TS);
                        PSQLTimeStamp(Buffer)^ := PSQLTimeStamp(@TS.Year)^
                      end;
          {$ENDIF !WITH_FTTIMESTAMP_FIELD}
          ftFmtBcd: ResultSet.GetBigDecimal(ColumnIndex, PBCD(Buffer)^);
          {$IFDEF WITH_FTLONGWORD}
            ftLongWord: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}PCardinal{$ELSE}PLongWord{$ENDIF}(Buffer)^ := ResultSet.GetUInt(ColumnIndex);
          {$ENDIF WITH_FTLONGWORD}
          {$IFDEF WITH_FTSHORTINT}
            ftShortInt: PShortInt(Buffer)^ := ResultSet.GetShort(ColumnIndex);
          {$ENDIF WITH_FTSHORTINT}
          {$IFDEF WITH_FTBYTE}
            ftByte: PByte(Buffer)^ := ResultSet.GetByte(ColumnIndex);
          {$ENDIF WITH_FTBYTE}
          {$IFDEF WITH_FTEXTENDED}
            ftExtended:
{$IFDEF WITH_TVALUEBUFFER}jmpReal10RS:{$ENDIF WITH_TVALUEBUFFER}
            PExtended(Buffer)^ := ResultSet.GetDouble(ColumnIndex);
          {$ENDIF WITH_FTEXTENDED}
          {$IFDEF WITH_FTTIMESTAMP_OFFSET}
          ftTimeStampOffset: begin
                        FResultSet.GetTimeStamp(ColumnIndex, TS);
                        PSQLTimeStampOffSet(Buffer)^ := PSQLTimeStampOffSet(@TS.Year)^;
                      end;
          {$ENDIF !WITH_FTTIMESTAMP_OFFSET}
          {$IFDEF WITH_FTSINGLE}
            ftSingle: PSingle(Buffer)^ := ResultSet.GetFloat(ColumnIndex);
          {$ENDIF WITH_FTSINGLE}
          { Processes blob fields. }
          //ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}, ftDataSet:
          { Processes all other fields. }
          else begin
            Result := not ResultSet.IsNull(ColumnIndex);
            Exit;
          end;
        end;
        Result := not ResultSet.WasNull;
      {$IFDEF WITH_TVALUEBUFFER} //See: http://sourceforge.net/p/zeoslib/tickets/118/
      end else if Field.DataType = ftExtended then begin // added by KestasL
        SetLength(Buffer, SizeOf(Extended));
        goto jmpReal10RS;
      {$ENDIF WITH_TVALUEBUFFER}
      end else
        Result := not ResultSet.IsNull(ColumnIndex)
    else if Buffer <> nil then begin //Accessor cached fields:
      case Field.DataType of
        ftString: begin
            P := RowAccessor.GetPAnsiChar(ColumnIndex, Result, blen);
            Result := not Result;
jmpMoveA:   if Result then begin
              if blen > NativeUInt(Field.DataSize-1) then
                blen := NativeUInt(Field.DataSize-1);
              Move(P^, Pointer(Buffer)^, blen);
              PByte(PAnsiChar(Buffer)+blen)^ := 0;
            end;
            Exit;
          end;
        ftSmallint: RowAccessor.GetSmall(ColumnIndex, Result);
        ftInteger, ftAutoInc: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PInteger{$ELSE}PLongInt{$ENDIF}(Buffer)^ := RowAccessor.GetInt(ColumnIndex, Result);
        ftBoolean: PWordBool(Buffer)^ := RowAccessor.GetBoolean(ColumnIndex, Result);
        ftWord: PWord(Buffer)^ := RowAccessor.GetWord(ColumnIndex, Result);
        ftFloat, ftCurrency: PDouble(Buffer)^ := RowAccessor.GetDouble(ColumnIndex, Result);
        ftBcd: begin
            if FNativeFormatOverloadCalled[ftBcd] //circumvent the T[BDE/Client]DataSet bug...
            then PCurrency(Buffer)^ := RowAccessor.GetCurrency(ColumnIndex, Result)
            else RowAccessor.GetBigDecimal(ColumnIndex, PBCD(Buffer)^,  Result);
            FNativeFormatOverloadCalled[ftBcd] := False;
          end;
        ftDate: begin
                  RowAccessor.GetDate(ColumnIndex, Result, D);
jmpMovDate:       Result := Result or not TryEncodeDate(D.Year, D.Month, D.Day, DT);
                  {$IFNDEF OLDFPC}if FNativeFormatOverloadCalled[ftDate] then {$ENDIF OLDFPC}
                    if Result
                    then PDateTime(Buffer)^ := 0
                    else PDateTime(Buffer)^ := DT
                  {$IFNDEF OLDFPC}
                  else if Result
                  then PInteger(Buffer)^ := 0
                  else begin
                    PInteger(Buffer)^ := Trunc(DT - D1M1Y1 + 1);
                    if D.IsNegative then
                      PInteger(Buffer)^ := -PInteger(Buffer)^;
                  end;
                  {$ENDIF OLDFPC}
                  FNativeFormatOverloadCalled[ftDate] := False;
               end;
        { Processes DateTime fields. }
        ftTime: begin
                  RowAccessor.GetTime(ColumnIndex, Result, T{%H-});
jmpMovTime:       Result := Result or not TryEncodeTime(T.Hour, T.Minute, T.Second, T.Fractions div NanoSecsPerMSec, DT);
                  {$IFNDEF WITH_FPC_FTTIME_BUG}
                  if FNativeFormatOverloadCalled[ftTime] then
                  {$ENDIF WITH_FPC_FTTIME_BUG}
                    if Result
                    then PDateTime(Buffer)^ := 0
                    else PDateTime(Buffer)^ := DT{$IFDEF WITH_FPC_FTTIME_BUG};{$ENDIF}
                  {$IFNDEF WITH_FPC_FTTIME_BUG}
                  else if Result
                  then PInteger(Buffer)^ := 0
                  else begin
                    PInteger(Buffer)^ := Trunc(DT * MSecsOfDay + 0.1);
                    if T.IsNegative then
                      PInteger(Buffer)^ := -PInteger(Buffer)^;
                  end;
                  {$ENDIF WITH_FPC_FTTIME_BUG}
                  FNativeFormatOverloadCalled[ftTime] := False;
                end;
        ftDateTime: begin
                  RowAccessor.GetTimeStamp(ColumnIndex, Result, TS);
jmpMovDateTime:   Result := Result or not TryTimeStampToDateTime(TS, DT);
                  if Result
                  then PDateTime(Buffer)^ := 0
                  else if FNativeFormatOverloadCalled[ftDateTime]
                    then PDateTime(Buffer)^ := DT
                    else begin
                      S := DateTimeToTimeStamp(DT);
                      PDateTime(Buffer)^ := TimeStampToMSecs(S);
                    end;
                  FNativeFormatOverloadCalled[ftDateTime] := False;
                end;
        { Processes binary fields. }
        ftBytes: begin
            P := RowAccessor.GetBytes(ColumnIndex, Result, bLen);
jmpMovBts:  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Pointer(Buffer)^, Min(Integer(bLen), Field.DataSize));
            FillChar((PAnsiChar(Buffer)+bLen)^, Field.DataSize-Integer(blen), #0);
          end;
        ftVarBytes: begin
            P := RowAccessor.GetBytes(ColumnIndex, Result, bLen);
jmpMovVarBts:PWord(Buffer)^ := bLen;
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^,
              (PAnsiChar(Pointer(Buffer))+SizeOf(Word))^, Min(Integer(bLen), Field.DataSize));
          end;
        { Processes String fields. }
        ftWideString: begin
            P := RowAccessor.GetPWideChar(ColumnIndex, Result, bLen);
            Result := not Result;
jmpMoveW:   if Result then begin
              {$IFDEF TWIDESTRINGFIELD_DATABUFFER_IS_PWIDESTRING}
                System.SetString(PWideString(Buffer)^, PWideChar(P), blen);
              {$ELSE}
              begin
                blen := blen shl 1;
                if blen >= NativeUint(Field.DataSize-2) then
                  blen := NativeUint(Field.DataSize-2);
                Move(P^, Pointer(Buffer)^, blen);
                PWord(PAnsiChar(Buffer)+blen)^ := 0;
              end;
              {$ENDIF}
            end;
            Exit;
          end;
        ftLargeInt: if FResultSetMetadata.GetColumnType(ColumnIndex) = stULong
            then PUInt64(Buffer)^ := RowAccessor.GetULong(ColumnIndex, Result)
            else PInt64(Buffer)^ := RowAccessor.GetLong(ColumnIndex, Result);
        {$IFDEF WITH_FTGUID}
        ftGUID:
          begin
            P := RowAccessor.GetColumnData(ColumnIndex, Result);
            if Result then PByte(Buffer)^ := 0
            else GUIDToBuffer(P, PAnsiChar(Buffer), True, True);
          end;
        {$ENDIF}
        {$IFDEF WITH_FTTIMESTAMP_FIELD}
        ftTimeStamp: begin
                      RowAccessor.GetTimeStamp(ColumnIndex, Result, TS);
                      PSQLTimeStamp(Buffer)^ := PSQLTimeStamp(@TS.Year)^
                    end;
        {$ENDIF !WITH_FTTIMESTAMP_FIELD}
        ftFmtBcd: RowAccessor.GetBigDecimal(ColumnIndex, PBCD(Buffer)^, Result);
        {$IFDEF WITH_FTLONGWORD}
          ftLongWord: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}PCardinal{$ELSE}PLongWord{$ENDIF}(Buffer)^ := RowAccessor.GetUInt(ColumnIndex, Result);
        {$ENDIF WITH_FTLONGWORD}
        {$IFDEF WITH_FTSHORTINT}
          ftShortInt: PShortInt(Buffer)^ := RowAccessor.GetShort(ColumnIndex, Result);
        {$ENDIF WITH_FTSHORTINT}
        {$IFDEF WITH_FTBYTE}
          ftByte: PByte(Buffer)^ := RowAccessor.GetByte(ColumnIndex, Result);
        {$ENDIF WITH_FTBYTE}
        {$IFDEF WITH_FTEXTENDED}
          ftExtended:
{$IFDEF WITH_TVALUEBUFFER}jmpReal10RA:{$ENDIF WITH_TVALUEBUFFER}
          PExtended(Buffer)^ := RowAccessor.GetDouble(ColumnIndex, Result);
        {$ENDIF WITH_FTEXTENDED}
        {$IFDEF WITH_FTTIMESTAMP_OFFSET}
        ftTimeStampOffset: begin
                      RowAccessor.GetTimeStamp(ColumnIndex, Result, TS);
                      PSQLTimeStampOffSet(Buffer)^ := PSQLTimeStampOffSet(@TS.Year)^;
                    end;
        {$ENDIF !WITH_FTTIMESTAMP_OFFSET}
        {$IFDEF WITH_FTSINGLE}
          ftSingle: PSingle(Buffer)^ := RowAccessor.GetFloat(ColumnIndex, Result);
        {$ENDIF WITH_FTSINGLE}
        { Processes blob fields. }
        //ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}
        //{$IFDEF WITH_FTDATASETSUPPORT},ftDataSet{$ENDIF} :
        { Processes all other fields. }
        else Result := not ResultSet.IsNull(ColumnIndex);
      end;
      Result := not Result;
    {$IFDEF WITH_TVALUEBUFFER} //See: http://sourceforge.net/p/zeoslib/tickets/118/
    end else if Field.DataType = ftExtended then begin // added by KestasL
      SetLength(Buffer, SizeOf(Extended));
      goto jmpReal10RA;
    {$ENDIF WITH_TVALUEBUFFER}
    end else
      Result := not RowAccessor.IsNull(ColumnIndex);
  end else //if GetActiveBuffer
    Result := False;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes


function TZAbstractRODataset.GetFieldIndex(AField: TField): Integer;
begin
  if FFieldsLookupTable = nil then
    FFieldsLookupTable := CreateFieldsLookupTable(FResultSet2AccessorIndexList);
  Result := DefineFieldIndex(FieldsLookupTable, AField);
end;

{**
  Support for widestring field
}
procedure TZAbstractRODataset.SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
  NativeFormat: Boolean);
begin
  if Field.DataType in [ftWideString, ftBCD, ftDate, ftTime, ftDateTime] then begin
    NativeFormat := True;
    if (Field.DataType <> ftWideString) then
      FNativeFormatOverloadCalled[Field.DataType] := True;
  end;

  {$IFNDEF VIRTUALSETFIELDDATA}
  inherited SetFieldData(Field, Buffer, NativeFormat);
  {$ELSE}
  SetFieldData(Field, Buffer);
  {$ENDIF}
end;

{**
  Stores the column value from the field buffer.
  @param Field an field object to be stored.
  @param Buffer a field value buffer.
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF} //ill FPC
procedure TZAbstractRODataset.SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF});
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  DT: TDateTime;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  UID: TGUID absolute TS;
  S: TTimeStamp absolute TS;
  FieldCP, ColumnCP: Word;
  PA: PAnsiChar absolute TS;
  PW: PWideChar absolute TS;
  L: NativeUInt;
begin
  if not Active then
    raise EZDatabaseError.Create(SOperationIsNotAllowed4);
  if not RequestLive and (Field.FieldKind = fkData) then
    RaiseReadOnlyError;
  // Check for readonly updates
  // Lookup values are requeried automatically on edit of all fields.
  // Didn't find a way to avoid this...
  if Field.ReadOnly and (Field.FieldKind <> fkLookup)
                    and not (State in [dsSetKey, dsCalcFields, dsFilter, dsBlockRead, dsInternalCalc, dsOpening]) then
    DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  if GetActiveBuffer(RowBuffer) then
  begin

    ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;

    if State in [dsEdit, dsInsert] then
      Field.Validate(Buffer);

    if Field.FieldKind <> fkData then //left over for calculated fields etc
      if Assigned(Buffer) then
        case Field.DataType of
          ftString: begin
                      PA := PAnsichar(Buffer);
                      L := StrLen(PA);
                      RowAccessor.SetPAnsiChar(ColumnIndex, PA, L);
                    end;
          ftSmallint: RowAccessor.SetInt(ColumnIndex, PSmallInt(Buffer)^);
          ftInteger, ftAutoInc: RowAccessor.SetInt(ColumnIndex, {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PInteger{$ELSE}PLongInt{$ENDIF}(Buffer)^);
          ftWord: RowAccessor.SetUInt(ColumnIndex, PWord(Buffer)^);
          ftBoolean: RowAccessor.SetBoolean(ColumnIndex, PWordBool(Buffer)^);
          ftFloat, ftCurrency: RowAccessor.SetDouble(ColumnIndex, PDouble(Buffer)^);
          ftBCD: if FNativeFormatOverloadCalled[ftBCD] then begin
              if (Field.Size < 4) then //right truncation? Using the Tbcd record's behaves equal
                PCurrency(Buffer)^ := RoundCurrTo(PCurrency(Buffer)^, Field.Size);
              RowAccessor.SetCurrency(ColumnIndex, PCurrency(Buffer)^);
              FNativeFormatOverloadCalled[ftBCD] := False;
            end else
              RowAccessor.SetBigDecimal(ColumnIndex, PBCD(Buffer)^);
          ftDate: begin
              if FNativeFormatOverloadCalled[ftDate]
              then DT := PDateTime(Buffer)^
              else DT := PInteger(Buffer)^ - 1 + D1M1Y1;
              DecodeDateTimeToDate(DT, D);
              RowAccessor.SetDate(ColumnIndex, D);
              FNativeFormatOverloadCalled[ftDate] := False;
            end;
          ftTime: begin
              if FNativeFormatOverloadCalled[ftTime]
              then DT := PDateTime(Buffer)^
              else DT := PInteger(Buffer)^ / MSecsOfDay;
              DecodeDateTimeToTime(DT, T);
              if (T.Fractions > 0) and Field.InheritsFrom(TZTimeField) then
                T.Fractions := ZSysUtils.RoundNanoFractionTo(T.Fractions, TZTimeField(Field).fScale);
              RowAccessor.SetTime(ColumnIndex, T);
              FNativeFormatOverloadCalled[ftTime] := False;
            end;
          ftDateTime: begin
              if FNativeFormatOverloadCalled[ftDateTime]
              then DT := PDateTime(Buffer)^
              else begin
                {$IFDEF FPC}
                S := MSecsToTimeStamp(System.Trunc(PDouble(Buffer)^));
                {$ELSE}
                S := MSecsToTimeStamp(PDateTime(Buffer)^);
                {$ENDIF}
                DT := TimeStampToDateTime(S);
              end;
              DecodeDateTimeToTimeStamp(DT, TS);
              if (TS.Fractions > 0) and Field.InheritsFrom(TZDateTimeField) then
                TS.Fractions := ZSysUtils.RoundNanoFractionTo(TS.Fractions, TZDateTimeField(Field).fScale);
              RowAccessor.SetTimeStamp(ColumnIndex, TS);
              FNativeFormatOverloadCalled[ftDateTime] := False;
            end;
          ftBytes: { Processes binary array fields. } begin
              L := Field.Size;
              RowAccessor.SetBytes(ColumnIndex, Pointer(Buffer), L);
            end;
          ftVarBytes: { Processes varbinary fields. } begin
              L := PWord(Buffer)^;
              RowAccessor.SetBytes(ColumnIndex, PByte(PAnsiChar(Buffer)+SizeOf(Word)), L);
            end;
          ftWideString: { Processes widestring fields. }
            {$IFDEF TWIDESTRINGFIELD_DATABUFFER_IS_PWIDESTRING}
            RowAccessor.SetUnicodeString(ColumnIndex, PWideString(Buffer)^);
            {$ELSE}
            begin
              PW := {$IFDEF WITH_TVALUEBUFFER}Pointer(Buffer){$ELSE}Buffer{$ENDIF};
              L := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(PW);
              RowAccessor.SetPWideChar(ColumnIndex, PW, L);
            end;
            {$ENDIF}
          ftLargeInt: if FResultSetMetaData.GetColumnType(ColumnIndex) = stULong
              then RowAccessor.SetULong(ColumnIndex, PUInt64(Buffer)^)
              else RowAccessor.SetLong(ColumnIndex, PInt64(Buffer)^);
          {$IFDEF WITH_FTGUID}
          ftGUID: begin
              ValidGUIDToBinary(PAnsiChar(Buffer), @UID.D1);
              RowAccessor.SetGUID(ColumnIndex, UID);
            end;
          {$ENDIF}
          {$IFDEF WITH_FTTIMESTAMP_FIELD}
          ftTimeStamp: begin
              PInt64(PAnsiChar(@TS.Year)+SizeOf(TZTimeStamp)-SizeOf(Int64))^ := 0;
              PSQLTimeStamp(@TS.Year)^ := PSQLTimeStamp(Buffer)^;
              RowAccessor.SetTimestamp(ColumnIndex, TS);
            end;
          {$ENDIF WITH_FTTIMESTAMP_FIELD}
          ftFmtBCD: RowAccessor.SetBigDecimal(ColumnIndex, PBCD(Buffer)^);
          {$IFDEF WITH_FTLONGWORD}
          ftLongWord: RowAccessor.SetUInt(ColumnIndex, {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PCardinal{$ELSE}PLongWord{$ENDIF}(Buffer)^);
          {$ENDIF WITH_FTLONGWORD}
          {$IFDEF WITH_FTSHORTINT}
          ftShortInt: RowAccessor.SetShort(ColumnIndex, PShortInt(Buffer)^);
          {$ENDIF WITH_FTSHORTINT}
          {$IFDEF WITH_FTBYTE}
          ftByte: RowAccessor.SetByte(ColumnIndex, PByte(Buffer)^);
          {$ENDIF WITH_FTBYTE}
          {$IFDEF WITH_FTEXTENDED}
          ftExtended: RowAccessor.SetDouble(ColumnIndex, PExtended(Buffer)^);
          {$ENDIF}
          {$IFDEF WITH_FTTIMESTAMP_OFFSET}
          ftTimeStampOffset: begin
              TS.IsNegative := False; //not supported here
              PSQLTimeStampOffSet(@TS.Year)^ := PSQLTimeStampOffSet(Buffer)^;
              RowAccessor.SetTimestamp(ColumnIndex, TS);
            end;
          {$ENDIF WITH_FTTIMESTAMP_OFFSET}
          {$IFDEF WITH_FTSINGLE}
          ftSingle: RowAccessor.SetFloat(ColumnIndex, PSingle(Buffer)^);
          {$ENDIF WITH_FTSINGLE}
          else raise CreateFieldConvertionError(Field);
        end
      else RowAccessor.SetNull(ColumnIndex)
    else if Assigned(Buffer) then
      case Field.DataType of
        ftString: begin
            FieldCP  := GetTransliterateCodePage(Connection.ControlsCodePage);
            ColumnCP := FResultSetMetadata.GetColumnCodePage(ColumnIndex);
            PA := PAnsichar(Buffer);
            L := StrLen(PA);
            if (L > 0) and ((ColumnCP = zCP_UTF16) or (FCharEncoding = ceUTF16) or ((FieldCP <> ColumnCP) and TStringField(Field).Transliterate)) then begin
              FUniTemp := PRawToUnicode(PA, L, FieldCP);
              L := Length(FUniTemp);
              if L = 0
              then PW := PEmptyUnicodeString
              else PW := Pointer(FUniTemp);
              FResultSet.UpdatePWideChar(ColumnIndex, PW, L);
              FUniTemp := '';
            end else
              FResultSet.UpdatePAnsiChar(ColumnIndex, PA, L);
          end;
        ftSmallint: FResultSet.UpdateSmall(ColumnIndex, PSmallInt(Buffer)^);
        ftInteger, ftAutoInc: FResultSet.UpdateInt(ColumnIndex, {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PInteger{$ELSE}PLongInt{$ENDIF}(Buffer)^);
        ftWord: FResultSet.UpdateWord(ColumnIndex, PWord(Buffer)^);
        ftBoolean: FResultSet.UpdateBoolean(ColumnIndex, PWordBool(Buffer)^);
        ftFloat, ftCurrency: FResultSet.UpdateDouble(ColumnIndex, PDouble(Buffer)^);
        ftBCD: if FNativeFormatOverloadCalled[ftBCD] then begin
            if (Field.Size < 4) then //right truncation? Using the Tbcd record's behaves equal
              PCurrency(Buffer)^ := RoundCurrTo(PCurrency(Buffer)^, Field.Size);
            FResultSet.UpdateCurrency(ColumnIndex, PCurrency(Buffer)^);
            FNativeFormatOverloadCalled[ftBCD] := False;
          end else
            FResultSet.UpdateBigDecimal(ColumnIndex, PBCD(Buffer)^);
        ftDate: begin
            if FNativeFormatOverloadCalled[ftDate]
            then DT := PDateTime(Buffer)^
            else DT := PInteger(Buffer)^ - 1 + D1M1Y1;
            DecodeDateTimeToDate(DT, D{%H-});
            FResultSet.UpdateDate(ColumnIndex, D);
            FNativeFormatOverloadCalled[ftDate] := False;
          end;
        ftTime: begin
            if FNativeFormatOverloadCalled[ftTime]
            then DT := PDateTime(Buffer)^
            else DT := PInteger(Buffer)^ / MSecsOfDay;
            DecodeDateTimeToTime(DT, T);
            if (T.Fractions > 0) and Field.InheritsFrom(TZTimeField) then
              T.Fractions := ZSysUtils.RoundNanoFractionTo(T.Fractions, TZTimeField(Field).fScale);
            FResultSet.UpdateTime(ColumnIndex, T);
            FNativeFormatOverloadCalled[ftTime] := False;
          end;
        ftDateTime: begin
            if FNativeFormatOverloadCalled[ftDateTime]
            then DT := PDateTime(Buffer)^
            else begin
              {$IFDEF FPC}
              S := MSecsToTimeStamp(System.Trunc(PDouble(Buffer)^));
              {$ELSE}
              S := MSecsToTimeStamp(PDateTime(Buffer)^);
              {$ENDIF}
              DT := TimeStampToDateTime(S);
            end;
            DecodeDateTimeToTimeStamp(DT, TS);
            if (TS.Fractions > 0) and Field.InheritsFrom(TZDateTimeField) then
              TS.Fractions := ZSysUtils.RoundNanoFractionTo(TS.Fractions, TZDateTimeField(Field).fScale);
            FResultSet.UpdateTimeStamp(ColumnIndex, TS);
            FNativeFormatOverloadCalled[ftDateTime] := False;
          end;
        ftBytes: { Processes binary array fields. } begin
            L := Field.Size;
            FResultSet.UpdateBytes(ColumnIndex, Pointer(Buffer), L);
          end;
        ftVarBytes: { Processes varbinary fields. } begin
            L := PWord(Buffer)^;
            FResultSet.UpdateBytes(ColumnIndex, PByte(PAnsiChar(Buffer)+SizeOf(Word)), L);
          end;
        ftWideString: { Processes widestring fields. }
          begin
          {$IFDEF TWIDESTRINGFIELD_DATABUFFER_IS_PWIDESTRING}
            L := Length(PWideString(Buffer)^);
            if L = 0
            then PW := PEmptyUnicodeString
            else PW := Pointer(PWideString(Buffer)^);
          {$ELSE}
            PW := {$IFDEF WITH_TVALUEBUFFER}Pointer(Buffer){$ELSE}Buffer{$ENDIF};
            L := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(PW);
          {$ENDIF}
            FResultSet.UpdatePWideChar(ColumnIndex, PW, L);
          end;
        ftLargeInt: if FResultSetMetaData.GetColumnType(ColumnIndex) = stULong
            then FResultSet.UpdateULong(ColumnIndex, PUInt64(Buffer)^)
            else FResultSet.UpdateLong(ColumnIndex, PInt64(Buffer)^);
        {$IFDEF WITH_FTGUID}
        ftGUID: begin
            ValidGUIDToBinary(PAnsiChar(Buffer), @UID.D1);
            FResultSet.UpdateGUID(ColumnIndex, UID);
          end;
        {$ENDIF}
        {$IFDEF WITH_FTTIMESTAMP_FIELD}
        ftTimeStamp: begin
            PInt64(PAnsiChar(@TS.Year)+SizeOf(TZTimeStamp)-SizeOf(Int64))^ := 0;
            PSQLTimeStamp(@TS.Year)^ := PSQLTimeStamp(Buffer)^;
            FResultSet.UpdateTimeStamp(ColumnIndex, TS);
          end;
        {$ENDIF WITH_FTTIMESTAMP_FIELD}
        ftFmtBCD: FResultSet.UpdateBigDecimal(ColumnIndex, PBCD(Buffer)^);
        {$IFDEF WITH_FTLONGWORD}
        ftLongWord: FResultSet.UpdateUInt(ColumnIndex, {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}PCardinal{$ELSE}PLongWord{$ENDIF}(Buffer)^);
        {$ENDIF WITH_FTLONGWORD}
        {$IFDEF WITH_FTSHORTINT}
        ftShortInt: FResultSet.UpdateShort(ColumnIndex, PShortInt(Buffer)^);
        {$ENDIF WITH_FTSHORTINT}
        {$IFDEF WITH_FTBYTE}
        ftByte: FResultSet.UpdateByte(ColumnIndex, PByte(Buffer)^);
        {$ENDIF WITH_FTBYTE}
        {$IFDEF WITH_FTEXTENDED}
        ftExtended: FResultSet.UpdateDouble(ColumnIndex, PExtended(Buffer)^);
        {$ENDIF}
        {$IFDEF WITH_FTTIMESTAMP_OFFSET}
        ftTimeStampOffset: begin
            TS.IsNegative := False; //not supported here
            PSQLTimeStampOffSet(@TS.Year)^ := PSQLTimeStampOffSet(Buffer)^;
            FResultSet.UpdateTimeStamp(ColumnIndex, TS);
          end;
        {$ENDIF WITH_FTTIMESTAMP_OFFSET}
        {$IFDEF WITH_FTSINGLE}
        ftSingle: FResultSet.UpdateFloat(ColumnIndex, PSingle(Buffer)^);
        {$ENDIF WITH_FTSINGLE}
        else raise CreateFieldConvertionError(Field);
      end
    else FResultSet.UpdateNull(ColumnIndex);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Field));
  end else
    raise EZDatabaseError.Create(SRowDataIsNotAvailable);

  if Field.FieldKind = fkData then begin
    OldRowBuffer.Index := -1;
    NewRowBuffer.Index := -1;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Checks is the cursor opened.
  @return <code>True</code> if the cursor is opened.
}
function TZAbstractRODataset.IsCursorOpen: Boolean;
begin
  Result := (ResultSet <> nil) and FCursorOpened;
end;

{**
  Gets an affected rows by the last executed statement.
  @return a number of last updated rows.
}
function TZAbstractRODataset.RowsAffected: LongInt;
begin
  Result := FRowsAffected;
end;

{**
  Gets the size of the record buffer.
  @return the size of the record buffer.
}
function TZAbstractRODataset.GetRecordSize: Word;
begin
  Result := RowAccessor.RowSize;
end;

{**
  Allocates a buffer for new record.
  @return an allocated record buffer.
}
{$IFNDEF WITH_AllocRecBuf_TRecBuf}
function TZAbstractRODataset.AllocRecordBuffer: TRecordBuffer;
{$ELSE}
function TZAbstractRODataset.AllocRecBuf: TRecBuf;
{$ENDIF}
begin
  {Dev notes:
   This will be called for OldRowBuffer, NewRowBuffer and for count of visible rows
   so NO memory wasting happens here!
  }
  RowAccessor.Alloc;
  {$IFNDEF WITH_AllocRecBuf_TRecBuf}
  Result := TRecordBuffer(RowAccessor.RowBuffer);
  {$ELSE}
  Result := TRecBuf(RowAccessor.RowBuffer);
  {$ENDIF}
end;

{**
  Frees a previously allocated record buffer.
  @param Buffer a previously allocated buffer.
}
{$IFNDEF WITH_FreeRecBuf_TRecBuf}
procedure TZAbstractRODataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  RowAccessor.DisposeBuffer(PZRowBuffer(Buffer));
  Buffer := nil;
end;
{$ELSE}
procedure TZAbstractRODataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  RowAccessor.DisposeBuffer(PZRowBuffer(Buffer));
  Buffer := 0;
end;
{$ENDIF}

{**
  Fetch all records. Added by Patyi
}
procedure TZAbstractRODataset.FetchAll;
begin
  Connection.ShowSQLHourGlass;
  FetchRows(0);
  if Active then
    UpdateCursorPos;
  Connection.HideSQLHourGlass;
end;

{**
  Executes a DML SQL statement.
}
procedure TZAbstractRODataset.ExecSQL;
begin
  if Active then begin
    Connection.ShowSQLHourGlass;
    try
      Close;
    finally
      Connection.HideSQLHourGlass;
    end;
  end;
  if (Statement = nil) or Statement.IsClosed then
    Prepare;

  Connection.ShowSQLHourGlass;
  try
    SetStatementParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams, FDataLink);

    FRowsAffected := Statement.ExecuteUpdatePrepared;
    if FHasOutParams then
      RetrieveParamValues;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Performs an internal initialization of field defiitions.
}
procedure TZAbstractRODataset.InternalInitFieldDefs;
var
  I, J, Size: Integer;
  AutoInit: Boolean;
  FieldType: TFieldType;
  SQLType: TZSQLType;
  ResultSet: IZResultSet;
  FieldName: string;
  FName: string;
  //ConSettings: PZConSettings;
  FieldDef: TFieldDef;
begin
  FieldDefs.Clear;
  ResultSet := Self.ResultSet;
  AutoInit := ResultSet = nil;

  try
    { Opens an internal result set if query is closed. }
    if AutoInit then
    begin
      CheckSQLQuery;
      CheckConnected;
      Prepare;
      ResultSet := CreateResultSet(FSQL.Statements[0].SQL, 0);
      FResultSetMetadata := ResultSet.GetMetadata;
    end;
    if not Assigned(ResultSet) then
      raise Exception.Create(SCanNotOpenResultSet);

    { Reads metadata from resultset. }

    with FResultSetMetadata do begin
    //ConSettings := ResultSet.GetConSettings;
    if GetColumnCount > 0 then
      for I := FirstDbcIndex to GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
        SQLType := GetColumnType(I);
        FieldType := ConvertDbcToDatasetType(SQLType, Connection.ControlsCodePage, GetPrecision(I));
        if (FieldType = ftVarBytes) and IsSigned(I) then
          FieldType := ftBytes;
        (*{$IFDEF WITH_FTTIMESTAMP_FIELD}
        else if (FieldType = ftDateTime) and (GetScale(I) > 3) then
          FieldType := ftTimeStamp
        {$ENDIF WITH_FTTIMESTAMP_FIELD}*);

        if FieldType in [ftBytes, ftVarBytes, ftString, ftWidestring] then begin
          {$IFNDEF WIT_WIDEMEMO}
          if (Connection.ControlsCodePage = cCP_UTF16) and (FieldType = ftWidestring) and (SQLType in [stAsciiStream, stUnicodeStream])
          then Size := (MaxInt shr 1)-2
          else{$ENDIF}
          Size := GetPrecision(I);
        end else {$IFDEF WITH_FTGUID} if FieldType = ftGUID then
          Size := 38
        else {$ENDIF} if FieldType in [ftBCD, ftFmtBCD{, ftTime, ftDateTime}] then
          Size := GetScale(I)
        else
          Size := 0;

        J := 0;
        FieldName := GetColumnLabel(I);
        FName := FieldName;
        while FieldDefs.IndexOf(FName) >= 0 do begin
          Inc(J);
          FName := Format('%s_%d', [FieldName, J]);
        end;
        {$IFNDEF UNICODE}
        if (FCharEncoding = ceUTF16) //dbc internaly stores everything in UTF8
          {$IF defined(WITH_DEFAULTSYSTEMCODEPAGE) or not defined(LCL)}
            and ({$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF} <> zCP_UTF8)
          {$IFEND}then begin
          PRawToRawConvert(Pointer(FName), Length(FName), zCP_UTF8, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF}, FRawTemp);
          FName := FRawTemp;
        end;
        {$ENDIF UNICODE}
        if (SQLType in [stBoolean..stBinaryStream]) and not FDisableZFields
        then FieldDef := TZFieldDef.Create(FieldDefs, FName, FieldType, SQLType, Size, False, I)
        else FieldDef := TFieldDef.Create(FieldDefs, FName, FieldType, Size, False, I);
        with FieldDef do begin
          if not (ReadOnly or IsUniDirectional) then begin
            {$IFNDEF OLDFPC}
            Required := IsWritable(I) and (IsNullable(I) = ntNoNulls);
            {$ENDIF}
            if IsReadOnly(I) then Attributes := Attributes + [faReadonly];
          end else
            Attributes := Attributes + [faReadonly];
          Precision := GetPrecision(I);
          DisplayName := FName;
          if GetOrgColumnLabel(i) <> GetColumnLabel(i) then
             Attributes := Attributes + [faUnNamed];
          //EH: hmm do we miss that or was there a good reason? For me its not relevant..
          //if (SQLType in [stString, stUnicodeString]) and (GetScale(i) = GetPrecision(I)) then
            //Attributes := Attributes + [faFixed];
        end;
      end;
    end;
    {$IFNDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
    FCurrentFieldRefIndex := 0;
    {$ENDIF}
  finally
    { Closes localy opened resultset. }
    if AutoInit then
    begin
      if ResultSet <> nil then begin
        FResultSetMetadata := nil;
        ResultSet.Close;
        ResultSet := nil;
      end;
      UnPrepare;
    end;
    {FFieldDefsInitialized := True;}  // commented out because this caises SF#286
  end;
end;

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZAbstractRODataset.CreateStatement(const SQL: string; Properties: TStrings):
  IZPreparedStatement;
var
  Temp: TStrings;
  Txn: IZTransaction;
  TxnCon: IZConnection;
  {$IFNDEF UNICODE}
  sqlCP, ClientCP: Word;
  NewSQL: RawByteString;
  ConSettings: PZConSettings;
  {$ENDIF}
begin
  Temp := TStringList.Create;
  try
    if Assigned(Properties) then
      Temp.AddStrings(Properties);
    {$IF declared(DSProps_PreferPrepared)}
    Temp.Values[DSProps_PreferPrepared] := BoolStrs[doPreferPrepared in FOptions];
    {$IFEND}
    if FTransaction <> nil
    then Txn := THackTransaction(FTransaction).GetIZTransaction
    else Txn := FConnection.DbcConnection.GetConnectionTransaction;
    TxnCon := Txn.GetConnection; //sets the active txn for IB/FB that is more a hack than i nice idea of me (EH) but make it work..
    {$IFNDEF UNICODE}
    ConSettings := TxnCon.GetConSettings;
    if (Ord(FCharEncoding)  >= Ord(ceUTF8))
    then ClientCP := zCP_UTF8
    else ClientCP := ConSettings.ClientCodePage.CP;
    sqlCP := Connection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttSQL);
    if (clientCP <> sqlCP) then begin
      NewSQL := '';
      PRawToRawConvert(Pointer(SQL), Length(SQL), sqlCP, clientCP, RawByteString(NewSQL));
    end else NewSQL := SQL;
    Result := TxnCon.PrepareStatementWithParams(NewSQL, Temp);
    {$ELSE}
    Result := TxnCon.PrepareStatementWithParams(SQL, Temp);
    {$ENDIF}
  finally
    Temp.Free;
  end;
end;

{**
  Creates a DBC resultset for the query.
  @param SQL an SQL query.
  @param MaxRows a maximum rows number (-1 for all).
  @returns a created DBC resultset.
}
function TZAbstractRODataset.CreateResultSet(const SQL: string;
  MaxRows: Integer): IZResultSet;
begin
  Connection.ShowSQLHourGlass;
  try
    SetStatementParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams, FDataLink);
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

    if doSmartOpen in FOptions
    then if Statement.ExecutePrepared
      then Result := Statement.GetResultSet
      else Result := nil
    else Result := Statement.ExecuteQueryPrepared;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Performs internal query opening.
}
procedure TZAbstractRODataset.InternalOpen;
var
  ColumnList: TObjectList;
  I, Cnt: Integer;
  OldRS: IZResultSet;
  ConSettings: PZConSettings;
begin
  {$IFNDEF FPC}
  If (csDestroying in Componentstate) then
    raise Exception.Create(SCanNotOpenDataSetWhenDestroying);
  {$ENDIF}
  if not FResultSetWalking then Prepare;

  CurrentRow := 0;
  FetchCount := 0;
  CurrentRows.Clear;
  FLastRowFetched := False;

  Connection.ShowSQLHourGlass;
  OldRS := FResultSet;
  try
    { Creates an SQL statement and resultsets }
    if not FResultSetWalking then
      if FSQL.StatementCount> 0
      then ResultSet := CreateResultSet(FSQL.Statements[0].SQL, -1)
      else ResultSet := CreateResultSet('', -1);
      if not Assigned(ResultSet) then
        if not (doSmartOpen in FOptions)
        then raise EZDatabaseError.Create(SCanNotOpenResultSet)
        else Exit;
    ConSettings := ResultSet.GetConSettings;
    FClientCP := ConSettings.ClientCodePage.CP;
    FCharEncoding := ConSettings.ClientCodePage.Encoding;

    FCursorOpened := True;
    FResultSetMetadata := ResultSet.GetMetadata;
    { Initializes field and index defs. }
    if (OldRS <> ResultSet) or FResultSetWalking (*or (not FRefreshInProgress) {and (not FFieldDefsInitialized)*) then  // commented out because this causes SF#286
      InternalInitFieldDefs;

    {$IFDEF WITH_LIFECYCLES}
    if ((FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles)) and not FRefreshInProgress then
    {$ELSE}
    if DefaultFields and not FRefreshInProgress then
    {$ENDIF}
    begin
      CreateFields;
      for i := 0 to Fields.Count -1 do begin
        if Fields[i].DataType = ftString then
          Fields[i].DisplayWidth := FResultSetMetadata.GetPrecision(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
        {$IFDEF WITH_FTGUID}
        else if Fields[i].DataType = ftGUID then Fields[i].DisplayWidth := 40 //looks better in Grid
        {$ENDIF}
        (*else if Fields[i].DataType in [ftTime, ftDateTime] then
          Fields[i].DisplayWidth := Fields[i].DisplayWidth + MetaData.GetScale(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF})*);
        {$IFDEF WITH_TAUTOREFRESHFLAG} //that's forcing loading metainfo's
        //if FResultSetMetadata.IsAutoIncrement({$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then
          //Fields[i].AutoGenerateValue := arAutoInc;
        {$ENDIF !WITH_TAUTOREFRESHFLAG}
      end;
    end;
    BindFields(True);

    if not FRefreshInProgress then begin
      { Initializes accessors and buffers. }
      ColumnList := ConvertFieldsToColumnInfo(Fields, GetTransliterateCodePage(Connection.ControlsCodePage), True);
      Cnt := ColumnList.Count;
      try
        //the RowAccessor wideneds the fieldbuffers for calculated field
        FRowAccessor := TZRowAccessor.Create(ColumnList, ResultSet.GetConSettings, FOpenLobStreams, FCachedLobs)
      finally
        ColumnList.Free;
      end;
      if Cnt > 0 then
        FFieldsAccessor := FRowAccessor;
      if not IsUnidirectional then begin
        {$IFDEF WITH_AllocRecBuf_TRecBuf}
        FOldRowBuffer := PZRowBuffer(AllocRecBuf);
        FNewRowBuffer := PZRowBuffer(AllocRecBuf);
        {$ELSE}
        FOldRowBuffer := PZRowBuffer(AllocRecordBuffer);
        FNewRowBuffer := PZRowBuffer(AllocRecordBuffer);
        {$ENDIF}
      end;

      InitFilterFields := False;

      IndexFields.Clear;
      GetFieldList(IndexFields, FLinkedFields); {renamed by bangfauzan}
    end;

    { Performs sorting. }
    if FSortedFields <> '' then
      InternalSort;
  finally
    Connection.HideSQLHourGlass;
    OldRS := nil;
  end;
  if FHasOutParams then
    RetrieveParamValues;
end;

{**
  Performs internal query closing.
}
procedure TZAbstractRODataset.InternalClose;
begin
  if ResultSet <> nil then
    if not FResultSetWalking then
      ResultSet.ResetCursor;
  FCursorOpened := False;
  FLastRowFetched := False;

  if not FRefreshInProgress then begin
    if (FOldRowBuffer <> nil) then
      {$IFNDEF WITH_FreeRecBuf_TRecBuf}
      FreeRecordBuffer(TRecordBuffer(FOldRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ELSE}
      FreeRecBuf(TRecordBuffer(FOldRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ENDIF}
    FOldRowBuffer := nil;

    if (FNewRowBuffer <> nil) and not FRefreshInProgress then
      {$IFNDEF WITH_FreeRecBuf_TRecBuf}
      FreeRecordBuffer(TRecordBuffer(FNewRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ELSE}
      FreeRecBuf(TRecordBuffer(FNewRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ENDIF}
    FNewRowBuffer := nil;

    if FFieldsAccessor <> RowAccessor then
      FreeAndNil(FFieldsAccessor);
    FreeAndNil(FResultSet2AccessorIndexList);
    FreeAndNil(FRowAccessor);
    { Destroy default fields }
    {$IFDEF WITH_LIFECYCLES}
    if ((FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles))
    {$ELSE}
    if DefaultFields
    {$ENDIF}
    then DestroyFields
    else BindFields(False);

    FieldsLookupTable := nil;
  end;
  {$IFNDEF WITH_GETFIELDCLASS_TFIELDDEF_OVERLOAD}
  FCurrentFieldRefIndex := 0;
  {$ENDIF}
  CurrentRows.Clear;
end;

{**
  Performs internal go to first record.
}
procedure TZAbstractRODataset.InternalFirst;
begin
  if CurrentRow > 0 then
    CheckBiDirectional;
  CurrentRow := 0;
end;

{**
  Performs internal go to last record.
}
procedure TZAbstractRODataset.InternalLast;
begin
  FetchRows(0);
  if CurrentRows.Count > 0 then
    CurrentRow := CurrentRows.Count + 1
  else
    CurrentRow := 0;
end;

{**
  Processes internal exception handling.
}
procedure TZAbstractRODataset.InternalHandleException;
begin
//  Application.HandleException(Self);
end;

{**
  Gets the maximum records count.
  @return the maximum records count.
}
function TZAbstractRODataset.GetRecordCount: Integer;
var RC: Integer;
begin
  CheckActive;
  if not IsUniDirectional and not FLastRowFetched then begin
    RC := FFetchRow;
    if (RC <> 0) and (CurrentRows.Count > FFetchRow) and (CurrentRow = CurrentRows.Count) and
      ((CurrentRows.Count mod FFetchRow) = 0) then
      RC := CurrentRows.Count + FFetchRow; //EH: load data chunked see https://sourceforge.net/p/zeoslib/tickets/399/
    FetchRows(RC);     // the orginal code was FetchRows(0); modifyed by Patyi
  end;
  Result := CurrentRows.Count;
end;

{**
  Gets the current record number.
  @return the current record number.
}
function TZAbstractRODataset.GetRecNo: Integer;
begin
  if Active then
    UpdateCursorPos;
  Result := CurrentRow;
  //EH: load data chunked see https://sourceforge.net/p/zeoslib/tickets/399/
  if not IsUniDirectional and not FLastRowFetched and
    (CurrentRow = CurrentRows.Count) and (FFetchRow > 0) then begin
    FetchRows(CurrentRows.Count+FFetchRow);
    Resync([rmCenter]); //notify we've widened the records
  end;
end;

{**
  Moves current record to the specified record.
  @param Value a new current record number.
}
procedure TZAbstractRODataset.MoveRecNo(Value: Integer);
var
  PreviousCurrentRow: Integer;
begin
  Value := Max(1, Value);
  if Value < CurrentRow then
    CheckBiDirectional;

  if FetchRows(Value) then
    CurrentRow := Value
  else
    CurrentRow := CurrentRows.Count;

  PreviousCurrentRow := CurrentRow;//Resync moves the current row away
  try
    if not (State in [dsInactive]) then
       Resync([]);
  finally
    CurrentRow := PreviousCurrentRow;
  end;
  UpdateCursorPos;
end;

{**
  Sets a new currenct record number.
  @param Value a new current record number.
}
procedure TZAbstractRODataset.SetRecNo(Value: Integer);
begin
  CheckOpened;
  Value := Max(1, Value);
  if Value < CurrentRow then
    CheckBiDirectional;

  DoBeforeScroll;
  MoveRecNo(Value);
  DoAfterScroll;
end;

{**
  Defines is the query editable?
  @return <code>True</code> if the query is editable.
}
function TZAbstractRODataset.GetCanModify: Boolean;
begin
  Result := RequestLive;
end;

{**
  Gets a linked datasource.
  @returns a linked datasource.
}
function TZAbstractRODataset.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

{$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}
function TZAbstractRODataset.GetUpdatable: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{**
  Sets the value of the Prepared property.
  Setting to <code>True</code> prepares the query. Setting to <code>False</code> unprepares.
  @param Value a new value for the Prepared property.
}
procedure TZAbstractRODataset.SetPrepared(Value: Boolean);
begin
  FResultSetWalking := False;
  If Value <> FPrepared then
    begin
      If Value then
        InternalPrepare
      else
        InternalUnprepare;
      FPrepared := Value;
    end;
end;

{**
  Sets a new linked datasource.
  @param Value a new linked datasource.
}
procedure TZAbstractRODataset.SetDataSource(Value: TDataSource);
begin
  {$IFNDEF FPC}
  if IsLinkedTo(Value) then
  {$ELSE}
  if Value.IsLinkedTo(Self) then
  {$ENDIF}
    raise EZDatabaseError.Create(SCircularLink);
  DataLink.DataSource := Value;
end;

procedure TZAbstractRODataset.SetDisableZFields(Value: Boolean);
begin
  if Value <> FDisableZFields then begin
    CheckInactive;
    unprepare;
    FDisableZFields := Value;
  end;
end;

{**
  Gets a master datasource.
  @returns a master datasource.
}
function TZAbstractRODataset.GetMasterDataSource: TDataSource;
begin
  Result := MasterLink.DataSource;
end;

{**
  Sets a new master datasource.
  @param Value a new master datasource.
}
procedure TZAbstractRODataset.SetMasterDataSource(Value: TDataSource);
begin
  {$IFNDEF FPC}
  if IsLinkedTo(Value) then
  {$ELSE}
  if Value.IsLinkedTo(Self) then
  {$ENDIF}
    raise EZDatabaseError.Create(SCircularLink);
  MasterLink.DataSource := Value;
  RereadRows;
end;

{**
  Gets master link fields.
  @returns a list with master fields.
}
function TZAbstractRODataset.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

{**
  Sets master link fields.
  @param Value a new master link fields.
}
procedure TZAbstractRODataset.SetMasterFields(const Value: string);
begin
  if FMasterLink.FieldNames <> Value then
  begin
    FMasterLink.FieldNames := Value;
    RereadRows;
  end;
end;

{**
  Processes change events from the master dataset.
  @param Sender an event sender object.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // TNotifyEvent - parameter not used intentionally
procedure TZAbstractRODataset.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  if (doAlwaysDetailResync in FOptions) or (FMasterLink.DataSet = nil)
    or not (FMasterLink.DataSet.State in [dsEdit, dsInsert]) then
    RereadRows;
end;

{**
  Processes disable events from the master dataset.
  @param Sender an event sender object.
}
procedure TZAbstractRODataset.MasterDisabled(Sender: TObject);
begin
  RereadRows;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Initializes new record with master fields.
}
procedure TZAbstractRODataset.DoOnNewRecord;
var
  I: Integer;
  MasterField, DetailField: TField;
  Temp: Int64;
  P1, P2 : Integer;
begin
  if MasterLink.Active and (MasterLink.Fields.Count > 0) then
  begin
    for I := 0 to MasterLink.Fields.Count - 1 do
    begin
      if I < IndexFields.Count then
      begin
        MasterField := TField(MasterLink.Fields[I]);
        DetailField := TField(IndexFields[I]);
        // Processes LargeInt fields.
        if (MasterField is TLargeIntField)
          or (DetailField is TLargeIntField) then
        begin
          if MasterField is TLargeIntField then
            Temp := TLargeIntField(
              MasterField).{$IFDEF WITH_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF}
          else
            Temp := MasterField.AsInteger;
          if DetailField is TLargeIntField then
            TLargeIntField(DetailField).{$IFDEF WITH_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF} := Temp
          else
            DetailField.AsString := ZFastCode.IntToStr(Temp);
        end
        // Processes all other fields.
        else
          DetailField.Value := MasterField.Value;
      end;
    end;
  end
  else
  begin
    if DataLink.Active and (DataLink.dataset.Fields.Count > 0) then
    begin
      p1 := 1; p2 := 1;
      while (P1 <= Length(LinkedFields)) and (p2 <= Length(MasterFields)) do
      begin
        DetailField := FieldByName(ExtractFieldName(LinkedFields, P1));
        MasterField := DataLink.DataSet.FieldByName (ExtractFieldName(MasterFields, P2));
        DetailField.Assign(MasterField);
      end;
    end;
  end;
  inherited DoOnNewRecord;
end;

{**
  Gets a list of index field names.
  @returns a list of index field names.
}
function TZAbstractRODataset.GetLinkedFields: string; {renamed by bangfauzan}
begin
  Result := FLinkedFields; {renamed by bangfauzan}
end;

{**
  Sets a new list of index field names.
  @param Value a new list of index field names.
}
procedure TZAbstractRODataset.SetLinkedFields(const Value: string); {renamed by bangfauzan}
begin
  if FLinkedFields <> Value then {renamed by bangfauzan}
  begin
    FLinkedFields := Value; {renamed by bangfauzan}
    IndexFields.Clear;
    if State <> dsInactive then
    begin
      GetFieldList(IndexFields, FLinkedFields); {renamed by bangfauzan}
      RereadRows;
    end;
  end;
end;

{**
  Sets a new set of dataset options.
  @param Value a new set of dataset options.
}
procedure TZAbstractRODataset.SetOptions(Value: TZDatasetOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    FCachedLobs := doCachedLobs in FOptions
  end;
end;

{**
  Sets a new sorted fields.
  @param Value a new sorted fields.
}
procedure TZAbstractRODataset.SetSortedFields(const Value: string); {bangfauzan modification}
var aValue: string;
begin
  aValue:=Trim(Value); {bangfauzan addition}
  if (FSortedFields <> aValue) or (FIndexFieldNames <> aValue)then {bangfauzan modification}
  begin
    FIndexFieldNames:=aValue;
    FSortType := GetSortType; {bangfauzan addition}
    {removing ASC or DESC behind space}
    if (FSortType <> stIgnored) then
    begin {pawelsel modification}
      aValue:=StringReplace(aValue,' Desc','',[rfReplaceAll,rfIgnoreCase]);
      aValue:=StringReplace(aValue,' Asc','',[rfReplaceAll,rfIgnoreCase]);
    end;
    FSortedFields := aValue;
    if Active then
      if not ({$IFDEF FPC}Updatable{$ELSE}Self is TZAbstractDataSet{$ENDIF}) then
        InternalSort //enables clearsort which prevents rereading data
      else
        {bangfauzan modification}
        if (FSortedFields = '') then
          InternalRefresh
        else
          InternalSort;
      {end of bangfauzan modification}
  end;
end;

{**
  Refreshes parameters and reopens the dataset.
}
procedure TZAbstractRODataset.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and not (DataSet.State in [dsSetKey, dsEdit]) then
        begin
          Refresh;
        end;
    end;
  finally
    EnableControls;
  end;
end;

{**
  Performs the internal preparation of the query.
}
procedure TZAbstractRODataset.InternalPrepare;
var I: Integer;
begin
  CheckSQLQuery;
  CheckInactive;  //AVZ - Need to check this
  CheckConnected;

  Connection.ShowSQLHourGlass;
  try
    if (FSQL.StatementCount > 0) and((Statement = nil) or (Statement.GetConnection.IsClosed)) then begin
      Statement := CreateStatement(FSQL.Statements[0].SQL, Properties);
      FHasOutParams := False;
      for i := 0 to Params.Count -1 do
        if (Params[I].ParamType <> ptUnknown) then begin
          FHasOutParams := FHasOutParams or (Ord(Params[I].ParamType) >= Ord(ptOutput));
          Statement.RegisterParameter(i, ConvertDatasetToDbcType(Params[I].DataType),
            DatasetTypeToProcColDbc[Params[i].ParamType], Params[i].Name,
            Max(Params[i].Precision, Params[i].Size), Params[i].NumericScale);
        end;
    end else if (Assigned(Statement)) then
      Statement.ClearParameters;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Rolls back the internal preparation of the query.
}
procedure TZAbstractRODataset.InternalUnPrepare;
begin
  if FOpenLobStreams.Count > 0 then
    raise EZSQLException.Create('you can''t close the DataSet while LobStreams are open');
  FResultSetMetadata := nil;
  if FResultSet <> nil then begin
    FResultSet.Close;
    FResultSet := nil;
  end;
  if Statement <> nil then begin
    Statement.Close;
    Statement := nil;
  end;
  {FFieldDefsInitialized := False;} // commented out because this causes SF#286
end;

{**
  Performs internal switch to the specified bookmark.
  @param Bookmark a specified bookmark.
}
{$IFDEF WITH_InternalGotoBookmark_TBookmark}
procedure TZAbstractRODataset.InternalGotoBookmark(Bookmark: TBookmark);
{$ELSE}
procedure TZAbstractRODataset.InternalGotoBookmark(Bookmark: Pointer);
{$ENDIF}
begin
  if not GotoRow(PInteger(Bookmark)^) then
    raise EZDatabaseError.Create(SBookmarkWasNotFound);
end;

{**
  Performs an internal switch to the specified record.
  @param Buffer the specified row buffer.
}

procedure TZAbstractRODataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  GotoRow(PZRowBuffer(Buffer)^.Index);
end;

procedure TZAbstractRODataset.DataEvent(Event: TDataEvent; Info: {$IFDEF FPC}PtrInt{$ELSE}NativeInt{$ENDIF});
var I, j: Integer;
begin
  inherited DataEvent(Event, Info);
  if Event = deLayoutChange then
    for i := 0 to Fields.Count -1 do
      for j := 0 to high(FieldsLookupTable) do
        if (FieldsLookupTable[j].Field = Fields[i]) and (FieldsLookupTable[j].DataSource = dltResultSet) then begin
          FResultSetMetadata.SetReadOnly(FieldsLookupTable[j].Index, Fields[i].ReadOnly or not (pfInUpdate in Fields[i].ProviderFlags));
          FResultSetMetadata.SetSearchable(FieldsLookupTable[j].Index, (pfInWhere in Fields[i].ProviderFlags));
        end;
end;

{$IFNDEF WITH_VIRTUAL_DEFCHANGED}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
procedure TZAbstractRODataset.DefChanged(Sender: TObject);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}
{$ENDIF}

{$IFNDEF WITH_DATASETFIELD}
procedure TZAbstractRODataset.SetDataSetField(const Value: TDataSetField);
begin
  if Value <> FDataSetField then
  begin
    if (Value <> nil) and ((Value.DataSet = Self) or
       ((TZAbstractRODataset(Value.DataSet).GetDataSource <> nil) and
        (TZAbstractRODataset(Value.DataSet).GetDataSource.DataSet = Self))) then
      DatabaseError('Circular DataLink', Self);
    if Assigned(Value) and not InheritsFrom(TZAbstractRODataset(Value.DataSet).NestedDataSetClass) then
      DatabaseErrorFmt('Dataset must inherite from %s', [TZAbstractRODataset(Value.DataSet).NestedDataSetClass.ClassName], Self);
    if Active then Close;
    if Assigned(FDataSetField) then
      FDataSetField.AssignNestedDataSet(nil);
    FDataSetField := Value;
    if Assigned(Value) then
    begin
      Value.AssignNestedDataSet(Self);
      if Value.DataSet.Active then Open;
    end;
  end;
end;
{$ENDIF}

{**
  Performs an internal adding a new record.
  @param Buffer a buffer of the new adding record.
  @param Append <code>True</code> if record should be added to the end
    of the result set.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // empty function - parameter not used intentionally
{$IFNDEF WITH_InternalAddRecord_TRecBuf}
procedure TZAbstractRODataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
{$ELSE}
procedure TZAbstractRODataset.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
{$ENDIF}
begin
  RaiseReadOnlyError;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Performs an internal record removing.
}
procedure TZAbstractRODataset.InternalDelete;
begin
  RaiseReadOnlyError;
end;

{**
  Performs an internal post updates.
}
procedure TZAbstractRODataset.InternalPost;
  procedure Checkrequired;
  var
    I: longint;
    columnindex : integer;
  begin
    For I:=0 to Fields.Count-1 do With Fields[i] do
      if State = dsEdit then begin
        if Required and not ReadOnly and (FieldKind=fkData) and IsNull then
          raise EZDatabaseError.Create(Format(SNeedField,[DisplayName]));
      end else if State = dsInsert then
        if Required and not ReadOnly and (FieldKind=fkData) and IsNull then begin
         // allow autoincrement and defaulted fields to be null;
            columnindex := Resultset.FindColumn(Fields[i].FieldName);
            if (Columnindex = InvalidDbcIndex) or
               (not FResultSetMetadata.HasDefaultValue(columnIndex) and
                not FResultSetMetadata.IsAutoIncrement(columnIndex)) then
              raise EZDatabaseError.Create(Format(SNeedField,[DisplayName]));
          end;
  end;

begin
  if not ({$IFDEF FPC}Updatable{$ELSE}Self is TZAbstractDataSet{$ENDIF}) then
    RaiseReadOnlyError;

  Checkrequired;
end;

{**
  Gets a bookmark flag from the specified record.
  @param Buffer a pointer to the record buffer.
  @return a bookmark flag from the specified record.
}
function TZAbstractRODataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := TBookmarkFlag(PZRowBuffer(Buffer)^.BookmarkFlag);
end;

{**
  Sets a new bookmark flag to the specified record.
  @param Buffer a pointer to the record buffer.
  @param Value a new bookmark flag to the specified record.
}

procedure TZAbstractRODataset.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PZRowBuffer(Buffer)^.BookmarkFlag := Ord(Value);
end;

{**
  Gets bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}

procedure TZAbstractRODataset.GetBookmarkData(
  Buffer: TRecordBuffer;
  Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF});
begin
  PInteger(Data)^ := PZRowBuffer(Buffer)^.Index;
end;

{**
  Sets a new bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}


procedure TZAbstractRODataset.SetBookmarkData(
  Buffer: TRecordBuffer;
  Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF});
begin
  PZRowBuffer(Buffer)^.Index := PInteger(Data)^;
end;

{**
  Compare two specified bookmarks.
  @param Bookmark1 the first bookmark object.
  @param Bookmark2 the second bookmark object.
  @return 0 if bookmarks are equal, -1 if the first bookmark is less,
    1 if the first bookmark is greatter.
}
{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractRODataset.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
var
  Index1, Index2: Integer;
begin
  Result := 0;
  if not Assigned(Bookmark1) or not Assigned(Bookmark2) then
    Exit;

  Index1 := CurrentRows.IndexOf(Pointer(PInteger(Bookmark1)^));
  Index2 := CurrentRows.IndexOf(Pointer(PInteger(Bookmark2)^));

  if Index1 < Index2 then Result := -1
  else if Index1 > Index2 then Result := 1;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{**
  Binds or unbinds the fields of the dataset.
  @param Binding decides if the field is bound or not.
}
procedure TZAbstractRODataset.BindFields(Binding: Boolean);
{$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
var I: Integer;
  Field: TField;
{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
begin
  if Binding then begin
    if FResultSet2AccessorIndexList <> nil then
      FreeAndNil(FResultSet2AccessorIndexList);
    FFieldsLookupTable := CreateFieldsLookupTable(FResultSet2AccessorIndexList);
  end;
  inherited BindFields(Binding);
{$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
  if not FDisableZFields then
    for i := 0 to Fields.Count -1 do begin
      Field := Fields[i];
      if (Field is TZDateField) then
        TZDateField(Field).Bind(Binding)
      else if (Field is TZDateTimeField) then
        TZDateField(TZDateTimeField).Bind(Binding)
      else if (Field is TZTimeField) then
        TZTimeField(Field).Bind(Binding)
      else if (Field is TZBooleanField) then
        TZBooleanField(Field).Bind(Binding)
      else if (Field is TZSmallIntField) then
        TZSmallIntField(Field).Bind(Binding)
      else if (Field is TZShortIntField) then
        TZShortIntField(Field).Bind(Binding)
      else if (Field is TZWordField) then
        TZWordField(Field).Bind(Binding)
      else if (Field is TZByteField) then
        TZByteField(Field).Bind(Binding)
      else if (Field is TZIntegerField) then
        TZIntegerField(Field).Bind(Binding)
      else if (Field is TZInt64Field) then
        TZInt64Field(Field).Bind(Binding)
      else if (Field is TZUInt64Field) then
        TZUInt64Field(Field).Bind(Binding)
      else if (Field is TZDoubleField) then
        TZDoubleField(Field).Bind(Binding)
      else if (Field is TZSingleField) then
        TZSingleField(Field).Bind(Binding)
      else if (Field is TZBCDField) then
        TZBCDField(Field).Bind(Binding)
      else if (Field is TZFMTBCDField) then
        TZFMTBCDField(Field).Bind(Binding)
      else if (Field is TZGuidField) then
        TZGuidField(Field).Bind(Binding)
      else if (Field is TZRawStringField) then
        TZRawStringField(Field).Bind(Binding)
      else if (Field is TZUnicodeStringField) then
        TZUnicodeStringField(Field).Bind(Binding)
      else if (Field is TZBytesField) then
        TZBytesField(Field).Bind(Binding)
      else if (Field is TZVarBytesField) then
        TZVarBytesField(Field).Bind(Binding)
      else if (Field is TZRawCLobField) then
        TZRawCLobField(Field).Bind(Binding)
      else if (Field is TZUnicodeCLobField) then
        TZUnicodeCLobField(Field).Bind(Binding)
      else if (Field is TZUnicodeCLobField) then
        TZUnicodeCLobField(Field).Bind(Binding)
      else if (Field is TZBlobField) then
        TZBlobField(Field).Bind(Binding);
    end;
  {$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

{**
  Checks is the specified bookmark valid.
  @param Bookmark a bookmark object.
  @return <code>True</code> if the bookmark is valid.
}
{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractRODataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := False;
  if Active and Assigned(Bookmark) and (FResultSet <> nil) and (CurrentRows <> nil) then
    Result := CurrentRows.IndexOf(Pointer(PInteger(Bookmark)^)) >= 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Performs an internal initialization of record buffer.
  @param Buffer a record buffer for initialization.
}

procedure TZAbstractRODataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  RowAccessor.ClearBuffer(PZRowBuffer(Buffer));
end;

{**
  Performs an internal refreshing.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
procedure TZAbstractRODataset.InternalRefresh;
var
  RowNo: NativeInt;
  Found: Boolean;
  KeyFields: string;
  Temp: TZVariantDynArray;
  KeyValues: Variant;
  FieldRefs: TObjectDynArray;
  OnlyDataFields: Boolean;
begin
  OnlyDataFields := False;
  FieldRefs := nil;
  if Active then
  begin
    if CurrentRow > 0 then
    begin
      RowNo := NativeInt(CurrentRows[CurrentRow - 1]);
      if ResultSet.GetRow <> RowNo then
        ResultSet.MoveAbsolute(RowNo);

      if Properties.Values[DSProps_KeyFields] <> '' then
        KeyFields := Properties.Values[DSProps_KeyFields]
      else
        KeyFields := DefineKeyFields(Fields, Connection.DbcConnection.GetMetadata.GetIdentifierConverter);
      FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
      {$IFDEF WITH_VAR_INIT_WARNING}Temp := nil;{$ENDIF}
      SetLength(Temp, Length(FieldRefs));
      RetrieveDataFieldsFromResultSet(FieldRefs, ResultSet, Temp);
      if Length(FieldRefs) = 1 then
        KeyValues := EncodeVariant(Temp[0])
      else
        KeyValues := EncodeVariantArray(Temp);
    end
    else
    begin
      KeyFields := '';
      KeyValues := Unassigned;
    end;

    DisableControls;
    try
      try
        FRefreshInProgress := True;
        InternalClose;
        InternalOpen;
      finally
        FRefreshInProgress := False;
      end;

      DoBeforeScroll;
      if KeyFields <> '' then
        Found := Locate(KeyFields, KeyValues, [])
      else
        Found := False;
    finally
      EnableControls;
    end;

    if not Found then
    begin
      DoBeforeScroll;
      DoAfterScroll;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Finds the next record in a filtered query.
  @param Restart a <code>True</code> to find from the start of the query.
  @param GoForward <code>True</code> to navigate in the forward direction.
  @return <code>True</code> if a sutisfied row was found.
}
function TZAbstractRODataset.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Index: Integer;
  SavedFilterEnabled: Boolean;
begin
  { Checks the current state. }
  CheckBrowseMode;
  DoBeforeScroll;
  Result := False;

  { Defines an initial position position. }
  if Restart then
  begin
    if GoForward then
      Index := 1
    else
    begin
      FetchRows(0);
      Index := CurrentRows.Count;
    end
  end
  else
  begin
    Index := CurrentRow;
    if GoForward then
    begin
      Inc(Index);
      if Index > CurrentRows.Count then
        FetchOneRow;
    end
    else
      Dec(Index);
  end;

  { Finds a record. }
  SavedFilterEnabled := FilterEnabled;
  try
    FilterEnabled := True;
    while (Index >= 1) and (Index <= CurrentRows.Count) do
    begin
      if FilterRow(Index) then
      begin
        Result := True;
        Break;
      end;
      if GoForward then
      begin
        Inc(Index);
        if Index > CurrentRows.Count then
          FetchOneRow;
      end
      else
        Dec(Index)
    end
  finally
    FilterEnabled := SavedFilterEnabled;
  end;

  { Sets a new found position. }
  SetFound(Result);
  if Result then
  begin
    MoveRecNo(Index);
    DoAfterScroll;
  end;
end;

{$IFDEF FPC}
function TZAbstractRODataset.FindFirst: Boolean;
begin
  Result := FindRecord(True, True);
end;

function TZAbstractRODataset.FindLast: Boolean;
begin
  Result := FindRecord(True, False);
end;

function TZAbstractRODataset.FindNext: Boolean;
begin
  Result := FindRecord(False, True);
end;

function TZAbstractRODataset.FindPrior: Boolean;
begin
  Result := FindRecord(False, False);
end;
{$ENDIF}

{**
  Sets a filtering control flag.
  @param Value <code>True</code> to turn filtering On.
}
procedure TZAbstractRODataset.SetFiltered(Value: Boolean);
begin
  if Value <> FilterEnabled then
  begin
    FilterEnabled := Value;
    inherited SetFiltered(Value);
    RereadRows;
  end;
end;

{**
  Sets a new filter expression string.
  @param Value a new filter expression.
}
procedure TZAbstractRODataset.SetFilterText(const Value: string);
begin
  inherited SetFilterText(Value);
  FilterExpression.DefaultVariables.Clear;
  FilterExpression.Expression := Value;
  InitFilterFields := False;
  if FilterEnabled then
    RereadRows;
end;

{$IFNDEF WITH_OBJECTVIEW}
procedure TZAbstractRODataset.SetObjectView(const Value: Boolean);
begin
  CheckInactive;
  FObjectView := Value;
end;
{$ENDIF WITH_OBJECTVIEW}
{**
  Checks is the opened resultset sequensed?
  @return <code>True</code> if the opened resultset is sequenced.
}
function TZAbstractRODataset.IsSequenced: Boolean;
begin
  Result := (not FilterEnabled);
end;

function TZAbstractRODataset.NextRecordSet: Boolean;
begin
  Result := NextResultSet;
end;

function TZAbstractRODataset.NextResultSet: Boolean;
begin
  if Assigned(Statement) and Statement.GetMoreResults then begin
    Result := True;
    SetAnotherResultset(Statement.GetResultSet);
  end else
    Result := False;
end;

function TZAbstractRODataset.NextRowsAffected: Boolean;
begin
  if Assigned(Statement) and Statement.GetMoreResults then begin
    Result := True;
    FRowsAffected := Statement.GetUpdateCount;
  end else
    Result := False;
end;

{**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZAbstractRODataset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    Close;
    FConnection := nil;
  end;

  if (Operation = opRemove) and Assigned(FDataLink)
    and (AComponent = FDataLink.Datasource) then
    FDataLink.DataSource := nil;

  if (Operation = opRemove) and Assigned(FMasterLink)
    and (AComponent = FMasterLink.Datasource) then
  begin
    FMasterLink.DataSource := nil;
    RereadRows;
  end;
end;

procedure TZAbstractRODataset.OnBlobUpdate(AField: NativeInt);
begin
  DataEvent(deFieldChange, AField);
end;

{**
  Performs an internal record search.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return an index of found row or -1 if nothing was found.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.InternalLocate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): LongInt;
var
  RowNo: NativeInt;
  I, RowCount: Integer;
  FieldRefs: TObjectDynArray;
  FieldIndices: TZFieldsLookUpDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  DecodedKeyValues: TZVariantDynArray;
  RowValues: TZVariantDynArray;
  PartialKey: Boolean;
  CaseInsensitive: Boolean;
  VariantManager: IZClientVariantManager;
begin
  OnlyDataFields := False;
  CheckBrowseMode;
  Result := -1;
  DecodedKeyValues := nil;

  PartialKey := loPartialKey in Options;
  CaseInsensitive := loCaseInsensitive in Options;

  FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
  FieldIndices := nil;
  if FieldRefs = nil then
     Exit;
  DecodedKeyValues := DecodeVariantArray(KeyValues);

  { Checks for equal field and values number }
  if Length(FieldRefs) <> Length(DecodedKeyValues) then
    raise EZDatabaseError.Create(SIncorrectSearchFieldsNumber);
  {$IFDEF WITH_VAR_INIT_WARNING}RowValues := nil;{$ENDIF}
  SetLength(RowValues, Length(DecodedKeyValues));

  VariantManager := Connection.DbcConnection.GetClientVariantManager;

  if not OnlyDataFields then begin
    { Processes fields if come calculated or lookup fields are involved. }
    {$IFDEF WITH_AllocRecBuf_TRecBuf}
    SearchRowBuffer := PZRowBuffer(AllocRecBuf);
    {$ELSE}
    SearchRowBuffer := PZRowBuffer(AllocRecordBuffer);
    {$ENDIF}
    try
      I := 0;
      FieldIndices := DefineFieldIndices(FieldsLookupTable, FieldRefs);
      RowCount := CurrentRows.Count;
      while True do begin
        while (I >= RowCount) and FetchOneRow do
          RowCount := CurrentRows.Count;
        if I >= RowCount then
          Break;

        RowNo := NativeInt(CurrentRows[I]);
        ResultSet.MoveAbsolute(RowNo);

        RowAccessor.RowBuffer := SearchRowBuffer;
        RowAccessor.RowBuffer^.Index := RowNo;
        GetCalcFields(TGetCalcFieldsParamType(SearchRowBuffer));
        FillDataFieldsFromSourceLookup(FieldIndices, RowAccessor, FResultSet, RowValues);

        if CompareDataFields(DecodedKeyValues, RowValues, VariantManager,
          PartialKey, CaseInsensitive) then begin
          Result := I + 1;
          Break;
        end;

        Inc(I);
      end;
    finally
      if SearchRowBuffer <> nil then
        {$IFNDEF WITH_FreeRecBuf_TRecBuf}
        FreeRecordBuffer(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
        {$ELSE}
        FreeRecBuf(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
        {$ENDIF}
    end;
  end else begin
    PrepareValuesForComparison(FieldRefs, DecodedKeyValues,
      ResultSet, PartialKey, CaseInsensitive, VariantManager);

    { Processes only data fields. }
    I := 0;
    RowCount := CurrentRows.Count;
    while True do begin
      while (I >= RowCount) and FetchOneRow do
        RowCount := CurrentRows.Count;
      if I >= RowCount then
        Break;

      RowNo := NativeInt(CurrentRows[I]);
      ResultSet.MoveAbsolute(RowNo);

      if CompareFieldsFromResultSet(FieldRefs, DecodedKeyValues,
        ResultSet, PartialKey, CaseInsensitive, VariantManager) then begin
        Result := I + 1;
        Break;
      end;

      Inc(I);
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Locates an interested record by specified search criteria.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return <code>True</code> if record was found or <code>False</code> otherwise.
}
function TZAbstractRODataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  Index: Integer;
begin
  DoBeforeScroll;
  if (Active) then //AVZ Check if the dataset is active before performing locate - return false otherwise
  begin
    Index := InternalLocate(KeyFields, KeyValues, Options);
    if Index > 0 then
    begin
      MoveRecNo(Index);
      DoAfterScroll;
      Result := True;
    end
    else
      Result := False;
    SetFound(Result);

  end
    else
  begin
    Result := False;
  end;
end;

{**
  Lookups specified fields from the searched record.
  @param KeyFields a list of field names to search record.
  @param KeyValues an array of field values to search record.
  @param ResultFields a list of field names to return as a result.
  @return an array of requested field values.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var
  RowNo: NativeInt;
  FieldRefs: TObjectDynArray;
  FieldIndices: TZFieldsLookUpDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  ResultValues: TZVariantDynArray;
begin
  OnlyDataFields := False;
  Result := Null;
  RowNo := InternalLocate(KeyFields, KeyValues, []);
  FieldRefs := nil;
  FieldIndices := nil;
  if RowNo < 0 then
     Exit;

  { Fill result array }
  FieldRefs := DefineFields(Self, ResultFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
  FieldIndices := DefineFieldIndices(FieldsLookupTable, FieldRefs);
  {$IFDEF WITH_VAR_INIT_WARNING}ResultValues := nil;{$ENDIF}
  SetLength(ResultValues, Length(FieldRefs));
  {$IFDEF WITH_AllocRecBuf_TRecBuf}
  SearchRowBuffer := PZRowBuffer(AllocRecBuf);
  {$ELSE}
  SearchRowBuffer := PZRowBuffer(AllocRecordBuffer);
  {$ENDIF}
  try
    RowNo := NativeInt(CurrentRows[RowNo - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    RowAccessor.RowBuffer := SearchRowBuffer;
    RowAccessor.RowBuffer^.Index := RowNo;
    GetCalcFields(TGetCalcFieldsParamType(SearchRowBuffer));
    FillDataFieldsFromSourceLookup(FieldIndices, RowAccessor, FResultSet, ResultValues);
  finally
    {$IFNDEF WITH_FreeRecBuf_TRecBuf}
    FreeRecordBuffer(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
    {$ELSE}
    FreeRecBuf(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
    {$ENDIF}
  end;

  if Length(FieldIndices) = 1 then
    Result := EncodeVariant(ResultValues[0])
  else
    Result := EncodeVariantArray(ResultValues);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the updated status for the current row.
  @return the UpdateStatus value for the current row.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.UpdateStatus: TUpdateStatus;
var
  RowNo: NativeInt;
begin
  Result := usUnmodified;
  if (ResultSet <> nil) and (CurrentRows.Count > 0) then
  begin
    RowNo := NativeInt(CurrentRows[CurrentRow - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    if ResultSet.RowInserted then
      Result := usInserted
    else if ResultSet.RowUpdated then
      Result := usModified
    else if ResultSet.RowDeleted then
      Result := usDeleted;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Translates strings between ansi and oem character sets.
}
{$IFNDEF NO_TDATASET_TRANSLATE}
function TZAbstractRODataset.Translate(Src, Dest: PAnsiChar; ToOem: Boolean):
   Integer;
begin
  if (Src <> nil) then
  begin
    Result := ZFastCode.StrLen(Src);
  {$IFDEF MSWINDOWS}
    if doOemTranslate in FOptions then
    begin
      if ToOem then
        CharToOemA(Src, Dest)
      else
        OemToCharA(Src, Dest);
      Dest[Result] := #0;
    end
    else
  {$ENDIF}
    begin
      if (Src <> Dest) then
      {$IFDEF WITH_STRCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrCopy(Dest, Src);
    end;
  end
  else
    Result := 0;
end;
{$ENDIF}
{**
  Prepares the query.
  If this actually does happen at the database connection level depends on the
  specific implementation.
}
procedure TZAbstractRODataset.Prepare;
begin
  Prepared := True;
end;

procedure TZAbstractRODataset.Prepare4DataManipulation(Field: TField);
var RowBuffer: PZRowBuffer;
begin
  if Field.ReadOnly and (Field.FieldKind <> fkLookup) and not (State in
    [dsSetKey, dsCalcFields, dsFilter, dsBlockRead, dsInternalCalc, dsOpening]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Field.DataSet);
  if GetActiveBuffer(RowBuffer)
  then FRowAccessor.RowBuffer := RowBuffer
  else raise EZDatabaseError.Create(SRowDataIsNotAvailable);
end;

{**
  Unprepares the query.
  Before the query gets executed it must be prepared again.
}
procedure TZAbstractRODataset.Unprepare;
begin
  Prepared := False;
end;

{**
  Creates a stream object for specified blob field.
  @param Field an interested field object.
  @param Mode a blob open mode.
  @return a created stream object.
}
function TZAbstractRODataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  Blob: IZBlob;
  CLob: IZCLob;
  ConSettings: PZConSettings;
  CP: Word;
begin
  CheckActive;

  Result := nil;
  if (Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}])
    and GetActiveBuffer(RowBuffer) then begin
    ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;

    Blob := FResultSet.GetBlob(ColumnIndex, TZLobStreamMode(Mode));
    if (Blob <> nil) then
      Blob.Open(TZLobStreamMode(Mode));
    if Blob <> nil then begin
      case Field.DataType of
        {$IFDEF WITH_WIDEMEMO}
        ftWideMemo: begin
            Assert(Blob.QueryInterface(IZCLob, CLob) = S_OK);
            Result := Clob.GetStream(zCP_UTF16);
          end;
        {$ENDIF}
        ftMemo, ftFmtMemo: begin
            ConSettings := FConnection.DbcConnection.GetConSettings;
            CP := GetTransliterateCodePage(Connection.ControlsCodePage);
            if not ((FCharEncoding = ceUTF16) or
      {XE10.3 x64 bug: a ObjectCast of a descendand doesn't work -> use exact class or the "As" operator}
              ((Field as TMemoField).Transliterate and (CP <> ConSettings.ClientCodePage.CP))) then
              CP := ConSettings.ClientCodePage.CP;
            Assert(Blob.QueryInterface(IZCLob, CLob) = S_OK);
            Result := Clob.GetStream(CP);
          end;
        else Result := Blob.GetStream
      end;
      if Mode <> bmRead then
        Blob.SetOnUpdateHandler(OnBlobUpdate, NativeInt(Field));
    end;
  end;
  if Result = nil then
    Result := TMemoryStream.Create;
end;

{**
  Creates a fields lookup table to define fixed position
  of the field in dataset.
  @param IndexPairList reaturns a collection of index pairs.
  @returns a fields lookup table.
}
function TZAbstractRODataset.CreateFieldsLookupTable(
  out IndexPairList: TZIndexPairList): TZFieldsLookUpDynArray;
var I, Idx: Integer;
  a: Integer;
  FieldName: String;
begin
  Result := nil;
  SetLength(Result, Fields.Count);
  IndexPairList := TZIndexPairList.Create;
  IndexPairList.Capacity := Fields.Count;
  a := FirstDbcIndex;
  for I := 0 to Fields.Count - 1 do begin
    Result[i].Field := Fields[I];
    FieldName := Fields[I].FieldName;
    {$IFNDEF UNICODE}
    if (FCharEncoding = ceUTF16) //dbc internaly stores everything in UTF8
      {$IF defined(WITH_DEFAULTSYSTEMCODEPAGE) or not defined(LCL)}
        and ({$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF} <> zCP_UTF8)
      {$IFEND}then begin
      PRawToRawConvert(Pointer(FieldName), Length(FieldName), zCP_UTF8, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF}, FRawTemp);
      FieldName := FRawTemp;
    end;
    {$ENDIF}
    Idx := FResultSetMetadata.FindColumn(FieldName);
    if Idx = InvalidDbcIndex then begin
      Result[i].DataSource := dltAccessor;
      Result[i].Index := a;
      Inc(a);
    end else begin
      Result[i].DataSource := dltResultSet;
      Result[i].Index := Idx;
      IndexPairList.Add(Idx, i);
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // empty function - parameter not used intentionally
function TZAbstractRODataset.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  {$IFDEF WITH_FTDATASETSUPPORT}
  Result := inherited CreateNestedDataSet(DataSetField);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // empty function - parameter not used intentionally
{**
  Closes the specified BLOB field.
  @param a BLOB field object.
}
procedure TZAbstractRODataset.CloseBlob(Field: TField);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Closes the cursor-handles. Releases(not closing) the current resultset
  and opens the cursorhandles. The current statment is used further.
  @param the NewResultSet
}
procedure TZAbstractRODataset.SetAnotherResultset(const Value: IZResultSet);
begin
  {EgonHugeist: I was forced to go this stupid sequence
    first i wanted to exclude parts of InternalOpen/Close but this didn't solve
    the DataSet issues. You can't init the fields as long the Cursor is not
    closed.. Which is equal to cursor open}
  if Assigned(Value) and ( Value <> ResultSet ) then
  begin
    FResultSetWalking := True; //hint for InternalOpen
    SetState(dsInactive);
    CloseCursor; //Calls InternalOpen in his sequence so InternalClose must be prepared
    ResultSet := Value; //Assign the new resultset
    if not ResultSet.IsBeforeFirst and (ResultSet.GetType <> rtForwardOnly) then
      ResultSet.BeforeFirst; //need this. All from dataset buffered resultsets are EOR
    {FFieldDefsInitialized := False;}  // commented out because it causes SF#286
    OpenCursor{$IFDEF FPC}(False){$ENDIF}; //Calls InternalOpen in his sequence so InternalOpen must be prepared
    OpenCursorComplete; //set DataSet to dsActive
    FResultSetWalking := False; //reset hint for InternalOpen
  end;
end;

{**
  Performs sorting of the internal rows.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
procedure TZAbstractRODataset.InternalSort;
var
  I: Integer;
  RowNo: NativeInt;
  SavedAccessor: TZRowAccessor;
  function CreateAllFieldsAccessor: TZRowAccessor;
  var ColumnList: TObjectList;
      I: Integer;
      CP: Word;
  begin
    ColumnList := TObjectList.Create(True);
    try
      for i := low(FFieldsLookupTable) to high(FFieldsLookupTable) do begin
        if FFieldsLookupTable[i].DataSource = dltAccessor
        then CP := GetTransliterateCodePage(Connection.ControlsCodePage)
        else CP := FResultSetMetadata.GetColumnCodePage(FFieldsLookupTable[i].Index);
        ColumnList.Add(ConvertFieldToColumnInfo(TField(FFieldsLookupTable[i].Field), CP))
      end;
      Result := TZRowAccessor.Create(ColumnList, ResultSet.GetConSettings, FOpenLobStreams, FCachedLobs)
    finally
      ColumnList.Free;
    end;
  end;
begin
  //if FIndexFieldNames = '' then exit; {bangfauzan addition}
  if (ResultSet <> nil) and not IsUniDirectional then begin
    FIndexFieldNames := Trim(FIndexFieldNames); {bangfauzan modification}
    DefineSortedFields(Self, {FSortedFields} FIndexFieldNames {bangfauzan modification},
    FSortedFieldRefs, FSortedComparsionKinds, FSortedOnlyDataFields);

    if (CurrentRow <= CurrentRows.Count) and (CurrentRows.Count > 0)
      and (CurrentRow > 0) then
      RowNo := NativeInt(CurrentRows[CurrentRow - 1])
    else
      RowNo := -1;

    { Restores the previous order. }
    if Length(FSortedFieldRefs) = 0
    then CurrentRows.Sort(ClearSort)
    else begin
      FetchRows(0);
      if FSortedOnlyDataFields then begin
        { Converts field objects into field indices. }
        SetLength(FSortedFieldIndices, Length(FSortedFieldRefs));
        for I := 0 to High(FSortedFieldRefs) do
          FSortedFieldIndices[I] := TField(FSortedFieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
        { Performs a sorting. }
        FCompareFuncs := ResultSet.GetCompareFuncs(FSortedFieldIndices, FSortedComparsionKinds);
        CurrentRows.Sort(LowLevelSort);
      end else begin
        SavedAccessor := FFieldsAccessor;
        FFieldsAccessor := CreateAllFieldsAccessor;
        { Sorts using generic highlevel approach. }
        try
          { Allocates buffers for sorting. }
          FSortRowBuffer1 := FFieldsAccessor.AllocBuffer;
          FSortRowBuffer2 := FFieldsAccessor.AllocBuffer;
          { Converts field objects into field indices. }
          SetLength(FSortedFieldIndices, Length(FSortedFieldRefs));
          for I := 0 to High(FSortedFieldRefs) do
            FSortedFieldIndices[I] := DefineFieldIndex(FieldsLookupTable,
              TField(FSortedFieldRefs[I]));
          { Performs sorting. }
          FCompareFuncs := FFieldsAccessor.GetCompareFuncs(FSortedFieldIndices, FSortedComparsionKinds);
          CurrentRows.Sort(HighLevelSort);
        finally
          { Disposed buffers for sorting. }
          FFieldsAccessor.DisposeBuffer(FSortRowBuffer1);
          FFieldsAccessor.DisposeBuffer(FSortRowBuffer2);
          FreeAndNil(FFieldsAccessor);
          FFieldsAccessor := SavedAccessor;
        end;
      end;
    end;

    CurrentRow := CurrentRows.IndexOf(Pointer(RowNo)) + 1;
    CurrentRow := Min(Max(0, CurrentRow), CurrentRows.Count);
    if not (State in [dsInactive]) then
       Resync([]);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Clears list sorting and restores the previous order.
  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
{$IFDEF FPC} {$PUSH}
  {$WARN 4082 off : Converting pointers to signed integers...}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractRODataset.ClearSort(Item1, Item2: Pointer): Integer;
begin
  //no real pointer addresses here, just a Integer represented as Pointer! -> overflow save!
  Result := NativeInt(Item1) - NativeInt(Item2);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sorting list using generic approach which is slow but may be used
  with calculated fields.

  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.HighLevelSort(Item1, Item2: Pointer): Integer;
var
  RowNo: NativeInt;
begin
  { Gets the first row. }
  RowNo := {%H-}NativeInt(Item1);
  FResultSet.MoveAbsolute(RowNo);
  FFieldsAccessor.RowBuffer := FSortRowBuffer1;
  FFieldsAccessor.RowBuffer^.Index := RowNo;
  { fill rowdata }
  FFieldsAccessor.FillFromFromResultSet(FResultSet, FResultSet2AccessorIndexList);
  FFieldsAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
  { fill data from CalcFields }
  GetCalcFields(TGetCalcFieldsParamType(FSortRowBuffer1));

  { Gets the second row. }
  RowNo := NativeInt(Item2);
  ResultSet.MoveAbsolute(RowNo);
  FFieldsAccessor.RowBuffer := FSortRowBuffer2;
  FFieldsAccessor.RowBuffer^.Index := RowNo;
  { fill rowdata }
  FFieldsAccessor.FillFromFromResultSet(FResultSet, FResultSet2AccessorIndexList);
  FFieldsAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
  { fill data from CalcFields }
  GetCalcFields(TGetCalcFieldsParamType(FSortRowBuffer2));

  { Compare both records. }
  Result := FFieldsAccessor.CompareBuffers(FSortRowBuffer1, FSortRowBuffer2,
    FSortedFieldIndices, FCompareFuncs);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sorting list using lowlevel approach which is fast but may not be used
  with calculated fields.

  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.LowLevelSort(Item1, Item2: Pointer): Integer;
begin
  Result := ResultSet.CompareRows(NativeInt(Item1), NativeInt(Item2),
    FSortedFieldIndices, FCompareFuncs);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
   Sets a new dataset properties.
   @param Value a dataset properties.
}
procedure TZAbstractRODataset.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

{$IFDEF WITH_IPROVIDER}

{**
  Starts a new transaction.
}
procedure TZAbstractRODataset.PSStartTransaction;
begin
  if Assigned(FConnection) and not FConnection.AutoCommit then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    FConnection.StartTransaction;
  end;
end;

{**
  Completes previously started transaction.
  @param Commit a commit transaction flag.
}
procedure TZAbstractRODataset.PSEndTransaction(Commit: Boolean);
begin
  if Assigned(FConnection) and FConnection.Connected
    and not FConnection.AutoCommit then
  begin
      if Commit then
         FConnection.Commit
      else
         FConnection.Rollback;
  end;
end;

{**
  Checks if this query is in transaction mode.
  @returns <code>True</code> if query in transaction.
}
function TZAbstractRODataset.PSInTransaction: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected
    and (FConnection.TransactIsolationLevel <> tiNone)
    and not FConnection.AutoCommit;
end;

{**
  Returns a string quote character.
  @retuns a quote character.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetQuoteCharW: WideString;
{$ELSE}
function TZAbstractRODataset.PSGetQuoteChar: string;
{$ENDIF}
begin
  if Assigned(FConnection) then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    Result := FConnection.DbcConnection.GetMetadata.GetDatabaseInfo.GetIdentifierQuoteString;
    if Length(Result) > 1 then
      Result := Copy(Result, 1, 1);
  end
  else
    Result := '"';
end;

{**
  Checks if dataset can execute any commands?
  @returns <code>True</code> if the query can execute any commands.
}
function TZAbstractRODataset.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZAbstractRODataset.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

{**
  Resets this dataset.
}
procedure TZAbstractRODataset.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Refresh;
    First;
  end;
end;

{**
  Execute statement a SQL query.
}
procedure TZAbstractRODataset.PSExecute;
begin
  ExecSQL;
end;

{**
  Gets query parameters.
  @returns parameters of this query.
}
function TZAbstractRODataset.PSGetParams: TParams;
begin
  Result := Params;
end;

{**
  Set new query parameters
  @param AParams new parameters to set into this query.
}
procedure TZAbstractRODataset.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
end;

{**
  Sets a command text for this query to execute.
  @param CommandText a command text for this query.
}

{$IFDEF WITH_IPROVIDERWIDE}
procedure TZAbstractRODataset.PSSetCommandText(const CommandText: string);
begin
  SQL.Text := CommandText;
end;

procedure TZAbstractRODataset.PSSetCommandText(const CommandText: WideString);
{$ELSE}
procedure TZAbstractRODataset.PSSetCommandText(const CommandText: string);
{$ENDIF}
begin
  SQL.Text := CommandText;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // empty function - parameter not used intentionally
{**
  Updates a record in the specified dataset.
  @param UpdateKind a type of the update.
  @param Delta a dataset with updates.
}
function TZAbstractRODataset.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  Result := False;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Generates an EUpdateError object based on another exception object.
  @param E occured exception.
  @param Prev a previous update error.
  @returns a new created update error.
}
function TZAbstractRODataset.PSGetUpdateException(E: Exception;
  Prev: EUpdateError): EUpdateError;
var
  PrevErrorCode: Integer;
begin
  if E is EZSQLException then
  begin
    if Assigned(Prev) then
      PrevErrorCode := Prev.ErrorCode
    else
      PrevErrorCode := 0;

    Result := EUpdateError.Create(E.Message, '',
      EZSQLException(E).ErrorCode, PrevErrorCode, E);
  end
  else
    Result := EUpdateError.Create(E.Message, '', -1, -1, E);
end;

{**
  Gets a table name if table is only one in the SELECT SQL statement.
  @returns a table name or an empty string is SQL query is complex SELECT
    or not SELECT statement.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetTableNameW: WideString;
{$ELSE}
function TZAbstractRODataset.PSGetTableName: string;
{$ENDIF}
var
  Driver: IZDriver;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
begin
  Result := '';
  if FConnection <> nil then
  begin
    Driver := FConnection.DbcDriver;
    Tokenizer := Driver.GetTokenizer;
    StatementAnalyser := Driver.GetStatementAnalyser;
    SelectSchema := StatementAnalyser.DefineSelectSchemaFromQuery(
      Tokenizer, SQL.Text);
    if Assigned(SelectSchema) and (SelectSchema.TableCount = 1) then
      Result := SelectSchema.Tables[0].FullName;
  end;
end;

{**
  Defines a list of query primary key fields.
  @returns a semicolon delimited list of query key fields.
}
// Silvio Clecio
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetKeyFieldsW: WideString;
begin
  Result := inherited PSGetKeyFieldsW;
end;
{$ELSE}
function TZAbstractRODataset.PSGetKeyFields: string;
begin
  Result := inherited PSGetKeyFields;
end;
{$ENDIF}

{**
  Executes a SQL statement with parameters.
  @param ASQL a SQL statement with parameters defined with question marks.
  @param AParams a collection of statement parameters.
  @param ResultSet a supplied result set reference (just ignored).
  @returns a number of updated rows.
}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSExecuteStatement(const ASQL: WideString; AParams: TParams;
  ResultSet: Pointer = nil): Integer;
{$ELSE}
function TZAbstractRODataset.PSExecuteStatement(const ASQL: string;
  AParams: TParams; {$IFDEF WITH_IProviderSupportNG}var ResultSet: TDataSet
      {$ELSE}ResultSet: Pointer = nil{$ENDIF}): Integer;
{$ENDIF}
var
  I: Integer;
  Statement: IZPreparedStatement;
  ParamValue: TParam;
begin
  if Assigned(FConnection) then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    Statement := FConnection.DbcConnection.PrepareStatement(ASQL);
    if (AParams <> nil) and (AParams.Count > 0) then
      for I := 0 to AParams.Count - 1 do
      begin
        ParamValue := AParams[I];
        SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, ParamValue);
      end;
    Result := Statement.ExecuteUpdatePrepared;
  end
  else
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$ENDIF}

// NB: FPC has TField.FieldDef property
procedure TZAbstractRODataset.CheckFieldCompatibility(Field: TField; AFieldDef: TFieldDef);
const
  {EH: hint all commented types are the fields the RowAccessor can't handle -> avoid stack killing moves in Get/SetFieldData()
  this Error trapping is made for User-added fields like calculated's ....}
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    //generic TFieldTypes of FPC and Delphi(since D7, of course):
    ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime, // 5..11
    ftBytes, ftVarBytes, ftInteger{ftAutoInc}, ftBlob, ftMemo, ftBlob{ftGraphic}, ftMemo{ftFmtMemo}, // 12..18
    ftBlob{ftParadoxOle}, ftBlob{ftDBaseOle}, ftBlob{ftTypedBinary}, ftUnknown{ftCursor}, ftString{ftFixedChar}, ftWideString, // 19..24
    ftLargeint, ftUnknown{ftADT}, ftUnknown{ftArray}, ftUnknown{ftReference}, ftDataSet, ftBlob{ftOraBlob}, ftMemo{ftOraClob}, // 25..31
    ftUnknown{ftVariant}, ftUnknown{ftInterface}, ftUnknown{ftIDispatch}, ftGuid, ftTimeStamp, ftFMTBcd // 32..37
{$IFDEF FPC} //addition types for FPC
    , ftWideString{ftFixedWideChar}, ftWideMemo // 38..39
{$ELSE !FPC}
{$IF CompilerVersion >= 18} //additional Types since D2006 and D2007
    , ftWideString{ftFixedWideChar}, ftWideMemo, ftDateTime{ftOraTimeStamp}, ftDateTime{ftOraInterval} // 38..41
{$IF CompilerVersion >= 20} //additional Types since D2009
    , ftLongWord, ftShortint, ftByte, ftExtended, ftUnknown{ftConnection}, ftUnknown{ftParams}, ftBlob{ftStream} //42..48
{$IF CompilerVersion >= 21} //additional Types since D2010
    , ftDateTime{ftTimeStampOffset}, ftUnknown{ftObject}, ftSingle //49..51
{$IFEND CompilerVersion >= 21}
{$IFEND CompilerVersion >= 20}
{$IFEND CompilerVersion >= 18}
{$ENDIF FPC}
  );
  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference, ftFmtBCD];
begin
  with Field do
  begin
    if (BaseFieldTypes[DataType] <> BaseFieldTypes[AFieldDef.DataType]) then
      DatabaseErrorFmt(SFieldTypeMismatch, [DisplayName,
        FieldTypeNames[DataType], FieldTypeNames[AFieldDef.DataType]], Self);
    if (DataType in CheckTypeSizes) and (Size <> AFieldDef.Size) then
        DatabaseErrorFmt(SFieldSizeMismatch, [DisplayName, Size,
          AFieldDef.Size], Self);
  end;
end;

{$IFDEF WITH_IPROVIDERSUPPORT_GUID}
type
  IProviderSupportActual = {$IF DECLARED(IProviderSupportNG)}IProviderSupportNG{$ELSE} IProviderSupport {$IFEND};
{$ENDIF}

{**
  Reset the calculated (includes fkLookup) fields
  @param Buffer
}
procedure TZAbstractRODataset.ClearCalcFields(Buffer: TRecordBuffer);
var
  Index: Integer;
begin
  RowAccessor.RowBuffer := PZRowBuffer(Buffer);
  for Index := 0 to Fields.Count-1 do
    if (Fields[Index].FieldKind in [fkCalculated, fkLookup]) then
      RowAccessor.SetNull(DefineFieldindex(FFieldsLookupTable,Fields[Index]));
end;

{=======================bangfauzan addition========================}
function TZAbstractRODataset.GetSortType: TSortType;
var
  AscCount, DescCount: Integer;
  s, Fragment: String;
begin
  {pawelsel modification}
  AscCount := 0;
  DescCount := 0;
  s := UpperCase(ReplaceChar(';', ',', FIndexFieldNames));
  Fragment := '';
  while s <> '' do
  begin
    BreakString(s, ',', Fragment, s);
    if ZFastCode.Pos(' DESC', Fragment) > 0 then
      Inc(DescCount)
    else
      Inc(AscCount);
  end;
  if (DescCount > 0) and (AscCount > 0) then
    Result := stIgnored
  else if (DescCount > 0) then
    Result := stDescending
  else
    Result := stAscending;
end;

procedure TZAbstractRODataset.SetSortType(Value: TSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    if (FSortType <> stIgnored) then
    begin {pawelsel modification}
      FSortedFields:=StringReplace(FSortedFields,' Desc','',[rfReplaceAll,rfIgnoreCase]);
      FSortedFields:=StringReplace(FSortedFields,' Asc','',[rfReplaceAll,rfIgnoreCase]);
    end;
    FIndexFieldNames:=GetIndexFieldNames;
    if Active then
      if (FSortedFields = '') then
        Self.InternalRefresh
      else
        InternalSort;
  end;
end;

function TZAbstractRODataset.GetIndexFieldNames : String;
begin
  Result:=FSortedFields;
  if Result <> '' then
  begin {pawelsel modification}
    if FSortType = stAscending then
    begin
       Result:=StringReplace(Result,';',' Asc;',[rfReplaceAll]);
       Result:=StringReplace(Result,',',' Asc,',[rfReplaceAll]);
       Result:=Result+' Asc';
    end;
    if FSortType = stDescending then
    begin
       Result:=StringReplace(Result,';',' Desc;',[rfReplaceAll]);
       Result:=StringReplace(Result,',',' Desc,',[rfReplaceAll]);
       Result:=Result+' Desc';
    end;
  end;
end;

procedure TZAbstractRODataset.SetIndexFieldNames(const Value: String);
var aValue: string;
begin
  aValue:=Trim(Value);
  {pawelsel modification}
  aValue:=RemoveChar('[', aValue);
  aValue:=RemoveChar(']', aValue);

  if FIndexFieldNames <> aValue then
  begin
     FIndexFieldNames := aValue;
     FSortType:=GetSortType;
     if (FSortType <> stIgnored) then
     begin {pawelsel modification}
        aValue:=StringReplace(aValue,' Desc','',[rfReplaceAll,rfIgnoreCase]);
        aValue:=StringReplace(aValue,' Asc','',[rfReplaceAll,rfIgnoreCase]);
     end;
     FSortedFields:=aValue;
  end;

  {Perform sorting}
  if Active then
     if (FSortedFields = '') then
        Self.InternalRefresh
     else
        InternalSort;
end;

{====================end of bangfauzan addition====================}

{ TZInt64Field }

function TZInt64Field.GetAsLargeInt: LargeInt;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetLong(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZInt64Field.GetAsString: string;
var L: LargeInt;
begin
  if FilledValueWasNull(L)
  then Result := ''
  else Result := ZFastCode.IntToStr(L)
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZInt64Field.GetAsVariant: Variant;
var L: LargeInt;
begin
  if FilledValueWasNull(L)
  then Result := null
  else Result := L
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZInt64Field.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZInt64Field.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

function TZInt64Field.FilledValueWasNull(var Value: Largeint): Boolean;
begin
  if IsRowDataAvailable then begin
    Value := TZAbstractRODataset(DataSet).FResultSet.GetLong(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

procedure TZInt64Field.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZInt64Field.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

procedure TZInt64Field.SetAsCardinal(Value: Cardinal);
begin
  SetAsLargeInt(Value);
end;

function TZInt64Field.GetAsCardinal: Cardinal;
begin
  Result := Cardinal(GetAsLargeInt);
end;

function TZInt64Field.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}(GetAsLargeInt);
end;

procedure TZInt64Field.SetAsLargeInt(Value: LargeInt);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(LargeInt));
    PInt64(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PInt64(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateLong(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZInt64Field.SetAsString(const Value: String);
begin
  if Value = ''
  then Clear
  else SetAsLargeInt(StrToInt64(Value))
end;

procedure TZInt64Field.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsLargeInt(Value);
end;

{ TZUInt64Field }

procedure TZUInt64Field.SetAsCardinal(Value: Cardinal);
begin
  SetAsUInt64(UInt64(Value));
end;

{$IFDEF WITH_FTEXTENDED}
procedure TZUInt64Field.SetAsExtended(Value: Extended);
begin
  SetAsLargeInt(Trunc(Value));
end;
{$ENDIF WITH_FTEXTENDED}

procedure TZUInt64Field.SetAsFloat(Value: Double);
begin
  SetAsLargeInt(Trunc(Value));
end;

procedure TZUInt64Field.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  SetAsUInt64(UInt64(Value));
end;

procedure TZUInt64Field.SetAsLargeInt(Value: Largeint);
begin
  SetAsUInt64(UInt64(Value));
end;

{$IFDEF WITH_FTSINGLE}
procedure TZUInt64Field.SetAsSingle(Value: Single);
begin
  SetAsLargeInt(Trunc(Value));
end;
{$ENDIF WITH_FTSINGLE}

procedure TZUInt64Field.SetAsString(const Value: string);
begin
  if Value = ''
  then Clear
  else SetAsLargeInt({$IFDEF UNICODE}UnicodeToUInt64{$ELSE}RawToUInt64{$ENDIF}(Value));
end;

procedure TZUInt64Field.SetAsUInt64(Value: UInt64);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(UInt64));
    PUInt64(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PUInt64(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateULong(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

function TZUInt64Field.GetAsCardinal: Cardinal;
begin
  Result := Cardinal(GetAsUInt64);
end;

{$IFDEF WITH_FTEXTENDED}
function TZUInt64Field.GetAsExtended: Extended;
begin
  Result := GetAsUInt64;
end;
{$ENDIF WITH_FTEXTENDED}

function TZUInt64Field.GetAsFloat: Double;
begin
  Result := GetAsUInt64;
end;

function TZUInt64Field.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}(GetAsUInt64);
end;

function TZUInt64Field.GetAsLargeInt: Largeint;
begin
  Result := LargeInt(GetAsUInt64);
end;

{$IFDEF WITH_FTSINGLE}
function TZUInt64Field.GetAsSingle: Single;
begin
  Result := GetAsUInt64;
end;
{$ENDIF WITH_FTSINGLE}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZUInt64Field.GetAsString: string;
var U: UInt64;
begin
  if FilledValueWasNull(U)
  then Result := ''
  else Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(U)
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZUInt64Field.GetAsUInt64: UInt64;
begin
  FilledValueWasNull(Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZUInt64Field.GetAsVariant: Variant;
var U: UInt64;
begin
  if FilledValueWasNull(U)
  then Result := null
  else Result := U;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

procedure TZUInt64Field.SetVarValue(const Value: Variant);
begin
  SetAsUInt64(Value);
end;

function TZUInt64Field.GetDefaultWidth: Integer;
begin
  Result := 22;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZUInt64Field.GetText(var Text: string; DisplayText: Boolean);
var U: UInt64;
    FmtStr: string;
begin
  if FilledValueWasNull(U)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = ''
    then Text := ZFastCode.IntToStr(U)
    else Text := FormatFloat(FmtStr, U);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

function TZUInt64Field.FilledValueWasNull(var Value: UInt64): Boolean;
begin
  if IsRowDataAvailable then begin
    Value := TZAbstractRODataset(DataSet).FResultSet.GetULong(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

constructor TZUInt64Field.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftLargeint); //we do not have a datatype for unsigend longlong ordinals until XE10.3
  ValidChars := ['+', '0'..'9']
end;

{ TZByteField }

{$IFDEF WITH_FTBYTE}
procedure TZByteField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  inherited Bind(Binding);
end;
{$ENDIF WITH_FTBYTE}

function TZByteField.GetAsByte: Byte;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetByte(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

function TZByteField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := GetAsByte;
end;

{$IFDEF WITH_FTBYTE}
function TZByteField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;
{$ENDIF WITH_FTBYTE}

{$IFDEF WITH_FTBYTE}
function TZByteField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;
{$ENDIF WITH_FTBYTE}

procedure TZByteField.SetAsByte(Value: Byte);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Byte));
    PByte(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PByte(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateUInt(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZByteField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  if (Value < Low(Byte)) or (Value > High(Byte)) then
    RangeError(Value, Low(Byte), High(Byte));
  SetAsByte(Byte(Value));
end;

{$IFDEF WITH_FTBYTE}
procedure TZByteField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;
{$ENDIF WITH_FTBYTE}

{ TZShortIntField }

{$IFDEF WITH_FTSHORTINT}
procedure TZShortIntField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  inherited Bind(Binding);
end;
{$ENDIF WITH_FTSHORTINT}

procedure TZShortIntField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if (Value < Low(ShortInt)) or (Value > High(ShortInt)) then
    RangeError(Value, Low(ShortInt), High(ShortInt));
  SetAsShortInt(ShortInt(Value));
end;

procedure TZShortIntField.SetAsShortInt(Value: ShortInt);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(ShortInt));
    PShortInt(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PShortInt(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateShort(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

function TZShortIntField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := GetAsShortInt;
end;

function TZShortIntField.GetAsShortInt: ShortInt;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetShort(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

{$IFDEF WITH_FTSHORTINT}
function TZShortIntField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;
{$ENDIF WITH_FTSHORTINT}

{$IFDEF WITH_FTSHORTINT}
function TZShortIntField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;
{$ENDIF WITH_FTSHORTINT}

{$IFDEF WITH_FTSHORTINT}
procedure TZShortIntField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;
{$ENDIF WITH_FTSHORTINT}

{ TZFieldDef }
{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
function TZFieldDef.GetChildDefs: TFieldDefs;
begin
  //if FChildDefs = nil then
    //FChildDefs := GetChildDefsClass.Create(Self);
  Result := FChildDefs;
end;

procedure TZFieldDef.SetChildDefs(Value: TFieldDefs);
begin
  ChildDefs.Assign(Value);
end;

{$ENDIF TFIELDDEF_HAS_CHILDEFS}
(*
type
  THackObjectField = Class(TObjectField);
*)

{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
function TZFieldDef.GetChildDefsClass: TFieldDefsClass;
begin
  if Assigned(Collection) then
    Result := TFieldDefsClass(Collection.ClassType)
  else
    Result := TFieldDefs;
end;
{$ENDIF TFIELDDEF_HAS_CHILDEFS}

constructor TZFieldDef.Create(Owner: TFieldDefs; const Name: string;
  FieldType: TFieldType; SQLType: TZSQLType; Size: Integer; Required: Boolean; FieldNo: Integer);
begin
  inherited Create(Owner, Name, FieldType, Size, Required, FieldNo);
  FSQLType := SQLType;
end;

{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
destructor TZFieldDef.Destroy;
begin
  FreeAndNil(FChildDefs);
  inherited Destroy;
end;

function TZFieldDef.HasChildDefs: Boolean;
begin
  Result := (FChildDefs <> nil) and (FChildDefs.Count > 0);
end;
{$ENDIF}

{$IFNDEF WITH_TOBJECTFIELD}
{ TObjectField }

constructor TObjectField.Create(AOwner: TComponent);
begin
  FOwnedFields := TFields.Create(nil);
  FFields := FOwnedFields;
  inherited Create(AOwner);
end;

destructor TObjectField.Destroy;
begin
  inherited Destroy;
  FOwnedFields.Free;
end;

procedure TObjectField.ReadUnNamed(Reader: TReader);
begin
  SetUnNamed(Reader.ReadBoolean);
end;

procedure TObjectField.WriteUnNamed(Writer: TWriter);
begin
  Writer.WriteBoolean(UnNamed);
end;

procedure TObjectField.DefineProperties(Filer: TFiler);

  function UnNamedStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := UnNamed <> TObjectField(Filer.Ancestor).UnNamed else
      Result := UnNamed;
  end;

begin
  inherited;
  Filer.DefineProperty('UnNamed', ReadUnNamed, WriteUnNamed, UnNamedStored);
end;

procedure TObjectField.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to FOwnedFields.Count - 1 do
  begin
    Field := FOwnedFields[I];
    if Field.Owner = Root then Proc(Field);
  end;
end;

procedure TObjectField.SetChildOrder(Component: TComponent; Order: Integer);
var
  F: TField;
begin
  F := Component as TField;
  if FFields.IndexOf(F) >= 0 then
    F.Index := Order;
end;

type THackField = class(TField); // get access to protected method
function TObjectField.GetDefaultWidth: Integer;
var
  I: Integer;
begin
  Result := 10;
  if FOwnedFields.Count > 0 then
  begin
    for I := 0 to FOwnedFields.Count - 1 do
      Inc(Result, THackField(FOwnedFields[I]).GetDefaultWidth);
    Result := Result shr 1;
  end;
end;

function TObjectField.GetHasConstraints: Boolean;
var
  I: Integer;
begin
  Result := False;//inherited GetHasConstraints;
  if not Result then
    for I := 0 to FFields.Count - 1 do
    begin
      Result := FFields[I].HasConstraints;
      if Result then Break;
    end;
end;

procedure TObjectField.SetFieldKind(Value: TFieldKind);
var
  I: Integer;
begin
  if FieldKind <> Value then
  begin
    {if (DataSet <> nil) and (DataSet.FDesigner <> nil) then
    with DataSet.Designer do
    begin
      BeginDesign;
      try
        FFieldKind := Value;
        for I := 0 to FFields.Count - 1 do
          FFields[I].FFieldKind := Value;
      finally
        EndDesign;
      end;
    end else}
    begin
      CheckInactive;
      FieldKind := Value;
      for I := 0 to FFields.Count - 1 do
        FFields[I].FieldKind := Value;
    end;
  end;
end;

procedure TObjectField.DataSetChanged;
//var
  //I: Integer;
begin
   { TODO : Check FOwnedFields/FFields (private section) }
  {FOwnedFields.DataSet := DataSet;
  for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].DataSet := DataSet;}
  if (DataSet <> nil) and not TZAbstractRODataset(DataSet).ObjectView then
    TZAbstractRODataset(DataSet).ObjectView := True;
end;

procedure TObjectField.SetDataSet(ADataSet: TDataSet);
begin
  FFields := FOwnedFields;
  inherited SetDataSet(ADataSet);
  DataSetChanged;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TObjectField.SetParentField(AField: TObjectField);
begin
  FFields := FOwnedFields;
  //inherited SetParentField(AField);
  DataSetChanged;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
class procedure TObjectField.CheckTypeSize(Value: Integer);
begin
  { Size is computed, no validation }
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
procedure TObjectField.Bind(Binding: Boolean);
begin
  {if FieldKind = fkLookup then
    if Binding then
    begin
      if LookupCache then
        RefreshLookupList
      else
        ValidateLookupInfo(True);
   end;}
end;
{$ENDIF}

procedure TObjectField.FreeBuffers;
{var
  I: Integer;}
begin
{ TODO : Check TFields.FreeBuffers for FPC how to get TFields overwritten? }
  {for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].FreeBuffers;}
end;

function TObjectField.GetFieldCount: Integer;
begin
  Result := Fields.Count;
end;

function TObjectField.GetFields: TFields;
begin
  Result := FFields;
end;

function TObjectField.GetAsString: string;

  function ValueToStr(const V: Variant): string;
  var
    S: string;
    V2: Variant;
    HighBound, I: Integer;
    Sep: string;
  begin
    Result := '';
    if VarIsArray(V) then
    begin
      HighBound := VarArrayHighBound(V, 1);
      Sep := '';
      for I := 0 to HighBound do
      begin
        V2 := V[I];
        if VarIsArray(V2) then
          S := ValueToStr(V2) else
          S := VarToStr(V2);
        Result := Result + Sep + S;
        if I = 0 then Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ListSeparator + ' ';
      end;
    end else
      Result := VarToStr(V);
    if Result <> '' then
      Result := '('+Result+')';
  end;

begin
  if (FFields = FOwnedFields) and (FFields.Count > 0) then
    Result := ValueToStr(GetAsVariant) else
    Result := inherited GetAsString;
end;

function TObjectField.GetFieldValue(Index: Integer): Variant;
begin
  Result := FFields[Index].Value;
end;

procedure TObjectField.SetFieldValue(Index: Integer; const Value: Variant);
begin
  FFields[Index].Value := Value;
end;

function TObjectField.GetAsVariant: Variant;
var
  I: Integer;
begin
  if IsNull then Result := Null else
  begin
    Result := VarArrayCreate([0, FieldCount - 1], varVariant);
    for I := 0 to FieldCount - 1 do
      Result[I] := GetFieldValue(I);
  end;
end;

procedure TObjectField.SetVarValue(const Value: Variant);
var
  Count, I: Integer;
begin
  Count := VarArrayHighBound(Value, 1) + 1;
  if Count > Size then Count := Size;
  for I := 0 to Count - 1  do
    SetFieldValue(I, Value[I]);
end;

procedure TObjectField.SetUnNamed(Value: Boolean);
begin
  FUnNamed := Value;
end;
{$ENDIF}

{ TArrayField }

{$IFNDEF WITH_TARRAYFIELD}
constructor TArrayField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftArray);
  Size := 10;
end;

procedure TArrayField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
{ TODO : Check how to get TFields.SparseArrays running with FPC? }
  {if TZAbstractRODataset(DataSet).SparseArrays then
    FFields.SparseFields := Size;}
end;

{$ENDIF !WITH_TARRAYFIELD}

{ TDataSetField }

{$IFNDEF WITH_TDATASETFIELD}
constructor TDataSetField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDataSet);
end;

destructor TDataSetField.Destroy;
begin
  AssignNestedDataSet(nil);
  FOwnedDataSet.Free;
  inherited Destroy;
end;

procedure TDataSetField.SetIncludeObjectField(Value: Boolean);
begin
  if Assigned(FNestedDataSet) then
    TZAbstractRODataSet(FNestedDataSet).CheckInactive;
  FIncludeObjectField := Value;
end;

procedure TDataSetField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Assigned(FNestedDataSet) then
  begin
    if Binding then
    begin
      if FNestedDataSet.State = dsInActive then FNestedDataSet.Open;
    end
    else
      FNestedDataSet.Close;
  end;
end;

function TDataSetField.GetFields: TFields;
begin
  if FNestedDataSet = nil then
    GetNestedDataSet;
  Result := inherited GetFields;
end;

function TDataSetField.GetNestedDataSet: TDataSet;
begin
  if (FNestedDataSet = nil) and not (csReading in DataSet.ComponentState) then
    FNestedDataSet := TZAbstractRODataset(DataSet).CreateNestedDataSet(Self);
  Result := FNestedDataSet;
end;

procedure TDataSetField.AssignNestedDataSet(Value: TDataSet);
begin
  if Assigned(FNestedDataSet) then
  begin
    FNestedDataSet.Close;
    TZAbstractRODataset(FNestedDataSet).DataSetField := nil;
    if Assigned(DataSet) then
      TZAbstractRODataset(DataSet).NestedDataSets.Remove(FNestedDataSet);
  end;
  if Assigned(Value) then
  begin
    TZAbstractRODataset(DataSet).NestedDataSets.Add(Value);
    FFields := Value.Fields;
  end else
    FFields := FOwnedFields;
  FNestedDataSet := Value;
end;

function TDataSetField.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and Assigned(NestedDataSet) and
    FNestedDataSet.Active;
end;

procedure TDataSetField.Assign(Source: TPersistent);
var
  I: Integer;
  SourceDataset: TDataset;
  SourceField: TField;
begin
  inherited;
  if (Source is TDataSetField) then
  begin
    SourceDataset := (Source as TDataSetField).NestedDataSet;
    if not Assigned(SourceDataset) or not Assigned(NestedDataSet) then Exit;
    SourceDataset.First;
    while not SourceDataset.Eof do
    begin
      NestedDataset.Append;
      for I := 0 to NestedDataset.Fields.Count - 1 do
      begin
        SourceField := SourceDataset.FieldByName(NestedDataset.Fields[I].FieldName);
        if Assigned(SourceField) then
          NestedDataset.Fields[I].Assign(SourceField);
      end;
      NestedDataset.Post;
      SourceDataset.Next;
    end;
  end
  else
    inherited Assign(Source);
end;
{$ENDIF !WITH_TDATASETFIELD}

{ TZDateField }

procedure TZDateField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF}
end;

procedure TZDateField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

constructor TZDateField.Create(AOwner: TComponent);
begin
  FInvalidText := 'NAD';
  inherited Create(AOwner);
end;

function TZDateField.FilledValueWasNull(var Value: TZDate): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    FResultSet.GetDate(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    Result := FResultSet.WasNull;
  end else Result := True;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5060 off : Function Result does not seem to be initialized} //FPC....
{$ENDIF}
function TZDateField.GetAsDate: TZDate;
begin
  if FilledValueWasNull(Result) then
    PInt64(@Result.Year)^ := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5060 off : Function Result does not seem to be initialized} //FPC....
{$ENDIF}
function TZDateField.GetAsDateTime: TDateTime;
var D: TZDate;
begin
  if FilledValueWasNull(D) or not ZSysUtils.TryDateToDateTime(D, Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDateField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;


{$IFDEF WITH_TSQLTIMESTAMP_RECORD}
function TZDateField.GetAsSQLTimeStamp: TSQLTimeStamp;
var D: TZDate;
begin
  if FilledValueWasNull(D) then begin
    PInt64(@Result.Year)^ := 0;
    PInt64(@Result.Minute)^ := 0;
  end else begin
    PInt64(@Result.Year)^ := PInt64(@D.Year)^;
    PInt64(@Result.Hour)^ := 0;
    Result.Fractions := 0;
  end;
end;
{$ENDIF WITH_TSQLTIMESTAMP_RECORD}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //ill FPC
procedure TZDateField.GetText(var Text: string; DisplayText: Boolean);
var
  Frmt: string;
  DT: TDateTime;
  D: TZDate;
  Delim, Sep: Char;
  b: Boolean;
  Digits: Byte;
  P: PChar;
begin
  if FilledValueWasNull(D)
  then Text := ''
  else begin
    B := DisplayText and (DisplayFormat <> '');
    if B then begin
      Frmt := DisplayFormat;
      Sep := #0;
    end else begin
      Frmt := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
      Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
    end;
    if (Frmt <> FLastFormat[B]) or (not B and (FLastDateSep <> Sep)) then begin
      FLastFormat[B] := Frmt;
      FLastDateSep := Sep;
      if not B and FindFirstDateFormatDelimiter(Frmt, Delim) and (Delim <> Sep) then
        Frmt := ZSysUtils.ReplaceChar(Delim, Sep, Frmt);
      FDateFormat[b] := Frmt;
      FSimpleFormat[b] := IsSimpleDateFormat(Frmt);
    end;
    if FSimpleFormat[b] then begin
      P := @FBuff[0];
      Digits := {$IFDEF UNICODE}DateToUni{$ELSE}DateToRaw{$ENDIF}(D.Year, D.Month,
        D.Day, P, FDateFormat[b], False, D.IsNegative);
      System.SetString(Text, P, Digits);
    end else begin
      if TryEncodeDate(D.Year, D.Month, D.Day, DT)
      //let the compiler do the complex stuff i.e. century/weekdays/monthname and user defined additional tokens
      then DateTimeToString(Text, FDateFormat[b], DT)
      else begin
        if DisplayText
        then Text := FInvalidText
        else Text := '';
        Exit;
      end;
      if D.IsNegative then
        Text := '-'+Text;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDateField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;;
end;

procedure TZDateField.SetAsDate(const Value: TZDate);
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateDate(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "D" does not seem to be initialized} {$ENDIF} //ill FPC
procedure TZDateField.SetAsDateTime(Value: TDateTime);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(TDateTime));
    PDateTime(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PDateTime(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
var D: TZDate;
begin
  if Assigned(OnValidate) then
    DoValidate;
  DecodeDateTimeToDate(Value, D);
  SetAsDate(D);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZDateField.SetInvalidText(const Value: String);
begin
  if Value = '' then
    raise EZDatabaseError.CreateFmt(SNeedField, [DisplayName]);
  FInvalidText := Value;
end;

{ TZTimeField }

procedure TZTimeField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
    fScale := TZAbstractRODataset(DataSet).FResultSetMetadata.GetScale(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZTimeField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

constructor TZTimeField.Create(AOwner: TComponent);
begin
  FInvalidText := 'NAT';
  FAdjSecFracFmt := True;
  inherited Create(AOwner);
end;

function TZTimeField.FilledValueWasNull(var Value: TZTime): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    FResultSet.GetTime(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    Result := FResultSet.WasNull;
  end else Result := True
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5060 off : Function Result does not seem to be initialized}
{$ENDIF} //rolling eyes
function TZTimeField.GetAsDateTime: TDateTime;
var T: TZTime;
begin
  if FilledValueWasNull(T) or not ZSysUtils.TryTimeToDateTime(T, Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5060 off : Function Result does not seem to be initialized} //FPC....
{$ENDIF}
function TZTimeField.GetAsTime: TZTime;
begin
  if FilledValueWasNull(Result) then begin
    {$IFDEF CPU64}PInt64{$ELSE}PCardinal{$ENDIF}(@Result.Hour)^ := 0;
    PInt64(@Result.Second)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZTimeField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;


{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //ill FPC
procedure TZTimeField.GetText(var Text: string; DisplayText: Boolean);
var
  Frmt: string;
  Delim, Sep: Char;
  DT: TDateTime;
  T: TZTime;
  I,J: LengthInt;
  Fraction: Cardinal;
  B: Boolean;
  P: PChar;
  Millis: Word;
begin
  if FilledValueWasNull(T)
  then Text := ''
  else begin
    B := DisplayText and (DisplayFormat <> '');
    if B then begin
      Frmt := DisplayFormat;
      Sep := #0;
    end else begin
      Frmt := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
      Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
    end;
    if (Frmt <> FLastFormat[B]) or (not B and (Sep <> FLastTimeSep)) then begin
      FLastFormat[B] := Frmt;
      if not B then begin
        FLastTimeSep := Sep;
        if FindFirstTimeFormatDelimiter(Frmt, Delim) and (Delim <> Sep) then
          Frmt := ZSysUtils.ReplaceChar(Delim, Sep, Frmt);
      end;
      FSimpleFormat[b] := IsSimpleTimeFormat(Frmt);
      if FAdjSecFracFmt
      then FFractionFormat[b] := ConvertAsFractionFormat(Frmt, FScale, not FSimpleFormat[b], FFractionLen[b])
      else FFractionFormat[b] := Frmt;
    end;
    if FSimpleFormat[b] then begin
      P := @FBuff[0];
      Fraction := t.Fractions;
      if not FAdjSecFracFmt then
        Fraction := RoundNanoFractionTo(Fraction, FScale);
      I := {$IFDEF UNICODE}TimeToUni{$ELSE}TimeToRaw{$ENDIF}(
        T.Hour, T.Minute, T.Second, Fraction, P, FLastFormat[B], False, T.IsNegative);
      System.SetString(Text, P, I);
    end else begin
      if FAdjSecFracFmt
      then Millis := 0
      else Millis := RoundNanoFractionToMillis(T.Fractions);
      if TryEncodeTime(T.Hour, T.Minute, T.Second, Millis, DT) then begin
        //let the compiler do the complex stuff i.e. AM/PM and user defined additional tokens, week days etc.
        DateTimeToString(Text, FFractionFormat[b], DT);
        if  FAdjSecFracFmt then begin
          //if shortformat the position may be variable. no chance to cache that info
          I := ZFastCode.Pos(MilliReplaceUnQuoted[FScale], Text);
          if I > 0 then begin
            P := Pointer(Text);
            Inc(P, I-1);
            Fraction := t.Fractions;
            Fraction := RoundNanoFractionTo(Fraction, FScale);
            Fraction := Fraction div FractionLength2NanoSecondMulTable[FScale];
            {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Fraction, P, Byte(FScale));
            if FScale > FFractionLen[B] then begin
              J := I+FScale;
              P := Pointer(Text);
              Inc(P, j-2);
              Millis := 0;
              while (J>I) and (P^ = ('0')) do begin
                Inc(Millis);
                Dec(J);
                Dec(P);
              end;
              if Millis > 0 then
                Delete(Text, J, Millis);
            end;
          end;
        end;
      end else begin
        if DisplayText
        then Text := FInvalidText
        else Text := '';
        Exit;
      end;
      if T.IsNegative then
        Text := '-'+Text;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //ill FPC

function TZTimeField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZTimeField.SetAdjSecFracFmt(Value: Boolean);
begin
  FLastFormat[True] := '';
  FLastFormat[False] := '';
  FAdjSecFracFmt := Value;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZTimeField.SetAsDateTime(Value: TDateTime);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(TDateTime));
    PDateTime(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PDateTime(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
var T: TZTime;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if Assigned(OnValidate) then
      DoValidate;
    DecodeDateTimeToTime(Value, T);
    if (T.Fractions > 0) then
      T.Fractions := ZSysUtils.RoundNanoFractionTo(T.Fractions, fScale);
    FResultSet.UpdateTime(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

procedure TZTimeField.SetAsTime(const Value: TZTime);
var T: TZTime;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    T := Value;
    if (T.Fractions > 0) then
      T.Fractions := ZSysUtils.RoundNanoFractionTo(T.Fractions, fScale);
    FResultSet.UpdateTime(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZTimeField.SetInvalidText(const Value: String);
begin
  if Value = '' then
    raise EZDatabaseError.CreateFmt(SNeedField, [DisplayName]);
  FInvalidText := Value;
end;

{ TZDateTimeField }

procedure TZDateTimeField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
    fScale := TZAbstractRODataset(DataSet).FResultSetMetadata.GetScale(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZDateTimeField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

{$IFDEF WITH_TSQLTIMESTAMP_RECORD}
function TZDateTimeField.GetAsSQLTimeStamp: TSQLTimeStamp;
var TS: TZTimeStamp;
begin
  if FilledValueWasNull(TS)
  then begin
    PInt64(@Result.Year)^ := 0;
    PInt64(@Result.Minute)^ := 0;
  end else
    Result := PSQLTimeStamp(@TS.Year)^
end;
{$ENDIF WITH_TSQLTIMESTAMP_RECORD}

constructor TZDateTimeField.Create(AOwner: TComponent);
begin
  FAdjSecFracFmt := True;
  FInvalidText := 'NADT';
  inherited;
end;

function TZDateTimeField.FilledValueWasNull(var Value: TZTimeStamp): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    FResultSet.GetTimestamp(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    Result := FResultSet.WasNull;
  end else Result := True;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5060 off : Function Result does not seem to be initialized}
{$ENDIF} //rolling eyes
function TZDateTimeField.GetAsDateTime: TDateTime;
var TS: TZTimeStamp;
begin
  if FilledValueWasNull(TS) or not ZSysUtils.TryTimeStampToDateTime(TS, Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}//rolling eyes
function TZDateTimeField.GetAsTimeStamp: TZTimeStamp;
begin
  if FilledValueWasNull(Result) then begin
    PInt64(@Result.Year)^ := 0;
    PInt64(@Result.Minute)^ := 0;
    PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDateTimeField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZDateTimeField.GetText(var Text: string; DisplayText: Boolean);
var
  Frmt: string;
  DT, D: TDateTime;
  Delim: Char;
  TS: TZTimeStamp;
  I,J: LengthInt;
  Fraction: Cardinal;
  B: Boolean;
  P: PChar;
  Millis: Word;
begin
  if FilledValueWasNull(TS)
  then Text := ''
  else begin
    B := DisplayText and (DisplayFormat <> '');
    if B
    then Frmt := DisplayFormat
    else begin //improve the "C" token of FormatDateTime
      if FindFirstDateFormatDelimiter({$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat, Delim) and
         (Delim <> {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator)
      then Frmt := ZSysUtils.ReplaceChar(Delim, {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator, {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat)
      else Frmt := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
      if (FAdjSecFracFmt and (FScale > 0) and (TS.Fractions > 0) ) or
         (TS.Hour <> 0) or (TS.Minute <> 0) or (TS.Second <> 0) then begin
        Frmt := Frmt + ' ';
        if FindFirstTimeFormatDelimiter({$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat, Delim) and
           (Delim <> {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator)
        then Frmt := Frmt + ZSysUtils.ReplaceChar(Delim, {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator, {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat)
        else Frmt := Frmt + {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
      end;
    end;
    if Frmt <> FLastFormat[B] then begin
      FLastFormat[B] := Frmt;
      FSimpleFormat[b] := IsSimpleDateTimeFormat(Frmt);
      if FAdjSecFracFmt
      then FFractionFormat[b] := ConvertAsFractionFormat(Frmt, FScale, not FSimpleFormat[b], FFractionLen[b])
      else FFractionFormat[b] := Frmt;
    end;
    if FSimpleFormat[b] then begin
      P := @FBuff[0];
      Fraction := ts.Fractions;
      if not FAdjSecFracFmt then
        Fraction := RoundNanoFractionTo(Fraction, FScale);
      I := {$IFDEF UNICODE}DateTimeToUni{$ELSE}DateTimeToRaw{$ENDIF}(
        TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute,
        TS.Second, Fraction, P, FLastFormat[B], False, TS.IsNegative);
      System.SetString(Text, P, I);
    end else begin
      B := False;
      if TryEncodeDate(TS.Year, TS.Month, TS.Day, d) then begin
        if FAdjSecFracFmt
        then Millis := 0
        else Millis := RoundNanoFractionToMillis(TS.Fractions);
        B := TryEncodeTime(TS.Hour, TS.Minute, TS.Second, Millis, DT);
        if B then
          if d < 0
          then DT := D - DT
          else DT := D + DT;
      end;
      if B then begin
        //let the compiler do the complex stuff i.e. AM/PM and user defined additional tokens, week days etc.
        DateTimeToString(Text, FFractionFormat[b], DT);
        if FAdjSecFracFmt then begin
          //if shortformat the position may be variable. no chance to cache that info
          I := ZFastCode.Pos(MilliReplaceUnQuoted[FScale], Text);
          if I > 0 then begin
            P := Pointer(Text);
            Inc(P, I-1);
            Fraction := ts.Fractions;
            Fraction := RoundNanoFractionTo(Fraction, FScale);
            Fraction := Fraction div FractionLength2NanoSecondMulTable[FScale];
            {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Fraction, P, Byte(FScale));
            if FScale > FFractionLen[B] then begin
              J := I+FScale;
              P := Pointer(Text);
              Inc(P, j-2);
              Millis := 0;
              while (J>I) and (P^ = ('0')) do begin
                Inc(Millis);
                Dec(J);
                Dec(P);
              end;
              if Millis > 0 then
                Delete(Text, J, Millis);
            end;
          end;
        end;
      end else begin
        if DisplayText
        then Text := FInvalidText
        else Text := '';
        Exit;
      end;
      if TS.IsNegative then
        Text := '-'+Text;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDateTimeField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZDateTimeField.SetAdjSecFracFmt(Value: Boolean);
begin
  FLastFormat[True] := '';
  FLastFormat[False] := '';
  FAdjSecFracFmt := Value;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$ENDIF} //rolling eyes
procedure TZDateTimeField.SetAsDateTime(Value: TDateTime);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(TDateTime));
    PDateTime(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PDateTime(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
var TS: TZTimeStamp;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if Assigned(OnValidate) then
      DoValidate;
    DecodeDateTimeToTimeStamp(Value, TS);
    if (TS.Fractions > 0) then
      TS.Fractions := ZSysUtils.RoundNanoFractionTo(TS.Fractions, fScale);
    FResultSet.UpdateTimeStamp(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZDateTimeField.SetAsTimeStamp(const Value: TZTimeStamp);
var TS: TZTimeStamp;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    TS := Value; //make a copy might be a non writable const
    if (TS.Fractions > 0) then
      TS.Fractions := ZSysUtils.RoundNanoFractionTo(TS.Fractions, fScale);
    FResultSet.UpdateTimestamp(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZDateTimeField.SetInvalidText(const Value: String);
begin
  if Value = '' then
    raise EZDatabaseError.CreateFmt(SNeedField, [DisplayName]);
  FInvalidText := Value;
end;

{ TZSmallIntField }

procedure TZSmallIntField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZSmallIntField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

function TZSmallIntField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := GetAsSmallInt;
end;

function TZSmallIntField.GetAsSmallInt: SmallInt;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetSmall(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

function TZSmallIntField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZSmallIntField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZSmallIntField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  if (Value < Low(SmallInt)) or (Value > High(SmallInt)) then
    RangeError(Value, Low(SmallInt), High(SmallInt));
  SetAsSmallInt(SmallInt(Value));
end;

procedure TZSmallIntField.SetAsSmallInt(Value: SmallInt);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(SmallInt));
    PSmallInt(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PSmallInt(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateSmall(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZWordField }

procedure TZWordField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZWordField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

function TZWordField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := GetAsWord;
end;

function TZWordField.GetAsWord: Word;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetWord(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

function TZWordField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZWordField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZWordField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  if (Value < Low(Word)) or (Value > High(Word)) then
    RangeError(Value, Low(Word), High(Word));
  SetAsWord(Word(Value));
end;

procedure TZWordField.SetAsWord(Value: Word);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Word));
    PWord(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PWord(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateWord(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZIntegerField }
procedure TZIntegerField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZIntegerField.Clear;
begin
  if FieldKind in [fkData, fkInternalCalc] then begin
    if not FBound then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
        FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end;
    end;
  end;
end;

function TZIntegerField.FilledValueWasNull(var Value: Integer): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    Value := FResultSet.GetInt(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := FResultSet.WasNull;
  end else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZIntegerField.GetAsInt: Integer;
begin
  if FilledValueWasNull(Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZIntegerField.GetAsString: String;
var I: Integer;
begin
  if FilledValueWasNull(I)
  then Result := ''
  else Result := ZFastCode.IntToStr(I)
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZIntegerField.GetAsVariant: Variant;
var I: Integer;
begin
  if FilledValueWasNull(I)
  then Result := null
  else Result := I
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZIntegerField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZIntegerField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZIntegerField.SetAsInt(Value: Integer);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Integer));
    PInteger(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PInteger(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateInt(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZIntegerField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  SetAsInt(Value);
end;

procedure TZIntegerField.SetAsString(const Value: string);
begin
  if Value = ''
  then Clear
  else SetAsInt(ZFastCode.{$IFDEF UNICODE}UnicodeToInt{$ELSE}RawToInt{$ENDIF}(Value));
end;

procedure TZIntegerField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsInt(Value);
end;

{ TZCardinalField }

{$IFDEF WITH_FTLONGWORD}
procedure TZCardinalField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZCardinalField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;
{$ENDIF WITH_FTLONGWORD}

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZCardinalField.GetAsCardinal: Cardinal;
begin
  if FilledValueWasNull(Result) then
    Result := 0;
end;

function TZCardinalField.GetAsFloat: Double;
begin
  Result := GetAsCardinal;
end;

function TZCardinalField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
var C: Cardinal;
begin
  C := GetAsCardinal;
  if C > Cardinal(High({$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF})) then
    RangeError(C, 0, High(LongInt));
  Result := C;
end;

function TZCardinalField.GetAsLargeInt: Largeint;
begin
  Result := GetAsCardinal;
end;

{$IFDEF WITH_FTLONGWORD}
function TZCardinalField.GetAsLongWord: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}Cardinal{$ELSE}LongWord{$ENDIF};
begin
  Result := GetAsCardinal;
end;
{$ENDIF WITH_FTLONGWORD}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZCardinalField.GetAsString: string;
var C: Cardinal;
begin
  if FilledValueWasNull(C)
  then Result := ''
  else Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(C)
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZCardinalField.GetAsVariant: Variant;
var C: Cardinal;
begin
  if FilledValueWasNull(C)
  then Result := null
  else Result := C;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF WITH_FTLONGWORD}
function TZCardinalField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;
{$ENDIF WITH_FTLONGWORD}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZCardinalField.GetText(var Text: string; DisplayText: Boolean);
var C: Cardinal;
    FmtStr: string;
begin
  if FilledValueWasNull(C)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = ''
    then Text := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(C)
    else Text := FormatFloat(FmtStr, C);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF WITH_FTLONGWORD}
function TZCardinalField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;
{$ENDIF WITH_FTLONGWORD}

function TZCardinalField.FilledValueWasNull(var Value: Cardinal): Boolean;
begin
  if IsRowDataAvailable then begin
    Value := TZAbstractRODataset(DataSet).FResultSet.GetUInt(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

procedure TZCardinalField.SetAsCardinal(Value: Cardinal);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Cardinal));
    PCardinal(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PCardinal(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateUInt(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZCardinalField.SetAsFloat(Value: Double);
begin
  SetAsCardinal(Cardinal(Round(Value)));
end;

procedure TZCardinalField.SetAsInteger(Value: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF});
begin
  if (Value < 0) then
    RangeError(Value, 0, High(Cardinal));
  SetAsCardinal(Value);
end;

procedure TZCardinalField.SetAsLargeInt(Value: Largeint);
begin
  if (Value < 0) or (Value > High(Cardinal)) then
    RangeError(Value, 0, High(Cardinal));
  SetAsCardinal(Value);
end;

{$IFDEF WITH_FTLONGWORD}
procedure TZCardinalField.SetAsLongWord(Value: {$IFDEF HAVE_TFIELD_32BIT_ASLONGWORD}Cardinal{$ELSE}LongWord{$ENDIF});
begin
  SetAsCardinal(Value);
end;
{$ENDIF WITH_FTLONGWORD}

procedure TZCardinalField.SetAsString(const Value: String);
begin
  if Value = ''
  then Clear
  else SetAsCardinal(ZFastCode.{$IFDEF UNICODE}UnicodeToUint64{$ELSE}RawToUint64{$ENDIF}(Value));
end;

procedure TZCardinalField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsCardinal(Value);
end;

{ TZSingleField }

{$IFDEF WITH_FTSINGLE}
procedure TZSingleField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;
{$ENDIF WITH_FTSINGLE}

{$IFNDEF WITH_FTSINGLE}
constructor TZSingleField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Precision := 7;
end;
{$ENDIF WITH_FTSINGLE}

{$IFNDEF WITH_FTSINGLE}
function TZSingleField.GetAsFloat: Double;
begin
  Result := GetAsSingle;
end;
{$ENDIF WITH_FTSINGLE}

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZSingleField.GetAsSingle: Single;
begin
  if FilledValueWasNull(Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZSingleField.GetAsString: string;
var S: Single;
begin
  if FilledValueWasNull(S)
  then Result := ''
  else Result := FloatToStr(S);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZSingleField.GetAsVariant: Variant;
var S: Single;
begin
  if FilledValueWasNull(S)
  then Result := null
  else Result := S;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF WITH_FTSINGLE}
procedure TZSingleField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;
{$ENDIF WITH_FTSINGLE}

{$IFNDEF WITH_FTSINGLE}
function TZSingleField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;
{$ENDIF WITH_FTSINGLE}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZSingleField.GetText(var Text: string; DisplayText: Boolean);
var Format: TFloatFormat;
    FmtStr: string;
    Digits: Integer;
    S: Single;
begin
  if FilledValueWasNull(S)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = '' then begin
      if Currency then begin
        if DisplayText
        then Format := ffCurrency
        else Format := ffFixed;
        Digits := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}CurrencyDecimals;
      end else begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(S, Format, Precision, Digits);
    end else
      Text := FormatFloat(FmtStr, S);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF WITH_FTSINGLE}
function TZSingleField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;
{$ENDIF WITH_FTSINGLE}

function TZSingleField.FilledValueWasNull(var Value: Single): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    Value := FResultSet.GetFloat(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := FResultSet.WasNull;
  end else Result := True;
end;

{$IFNDEF WITH_FTSINGLE}
procedure TZSingleField.SetAsFloat(Value: Double);
begin
  SetAsSingle(Value);
end;
{$ENDIF WITH_FTSINGLE}

procedure TZSingleField.SetAsSingle(value: Single);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Single));
    PSingle(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PSingle(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateFloat(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZSingleField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsSingle(Value);
end;

{ TZDoubleField }

procedure TZDoubleField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZDoubleField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZDoubleField.GetAsFloat: Double;
begin
  if FilledValueWasNull(Result) then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZDoubleField.GetAsString: string;
var D: Double;
begin
  if FilledValueWasNull(D)
  then Result := ''
  else Result := FloatToStr(D);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZDoubleField.GetAsVariant: Variant;
var D: Double;
begin
  if FilledValueWasNull(D)
  then Result := null
  else Result := D;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDoubleField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZDoubleField.GetText(var Text: string; DisplayText: Boolean);
var
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
  D: Double;
begin
  if FilledValueWasNull(D)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = '' then begin
      if Currency then begin
        if DisplayText
        then Format := ffCurrency
        else Format := ffFixed;
        Digits := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}CurrencyDecimals;
      end else begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(D, Format, Precision, Digits);
    end else
      Text := FormatFloat(FmtStr, D);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDoubleField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

function TZDoubleField.FilledValueWasNull(var Value: Double): Boolean;
begin
  if IsRowDataAvailable then begin
    Value := TZAbstractRODataset(DataSet).FResultSet.GetDouble(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

procedure TZDoubleField.SetAsFloat(Value: Double);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(Double));
    PDouble(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PDouble(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateFloat(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZDoubleField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsFloat(Value);
end;

{ TZBCDField }

procedure TZBCDField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZBCDField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZBCDField.FilledValueWasNull(var Value: Currency): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    Value := FResultSet.GetCurrency(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := FResultSet.WasNull;
  end else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBCDField.GetAsBCD: TBcd;
var C: System.Currency;
begin
  if FilledValueWasNull(C)
  then PCardinal(@Result.Precision)^ := 0
  else ZSysUtils.Currency2Bcd(C, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBCDField.GetAsCurrency: Currency;
begin
  if FilledValueWasNull(Result{%H-}) then
    Result := 0;
end;

function TZBCDField.GetAsInteger: {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF};
begin
  Result := {$IFDEF HAVE_TFIELD_32BIT_ASINTEGER}Integer{$ELSE}Longint{$ENDIF}(GetAsLargeInt);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBCDField.GetAsLargeInt: Largeint;
var C: System.Currency;
begin
  if FilledValueWasNull(C)
  then Result := 0
  else begin
    C := RoundCurrTo(C, 0);
    Result := PInt64(@C)^ div 10000;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBCDField.GetAsString: string;
var C: System.Currency;
begin
  if FilledValueWasNull(C)
  then Result := ''
  else Result := CurrToStr(C);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBCDField.GetAsVariant: Variant;
var C: System.Currency;
begin
  if FilledValueWasNull(C)
  then Result := null
  else Result := C;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBCDField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZBCDField.GetText(var Text: string; DisplayText: Boolean);
var
  Format: TFloatFormat;
  Digits: Integer;
  FmtStr: string;
  C: System.Currency;
begin
  if FilledValueWasNull(C)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = '' then begin
      if Self.currency then begin
        if DisplayText
        then Format := ffCurrency
        else Format := ffFixed;
        Digits := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}CurrencyDecimals;
      end else begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := CurrToStrF(C, Format, Digits);
    end else
      Text := FormatCurr(FmtStr, C);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBCDField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZBCDField.SetAsCurrency(Value: Currency);
  procedure DoValidate;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(System.Currency));
    PCurrency(FValidateBuffer)^ := Value;
    Validate(FValidateBuffer);
    Value := PCurrency(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@Value);
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if ((MinValue <> 0) or (MaxValue <> 0)) and ((Value < MinValue) or (Value > MaxValue)) then
    RangeError(Value, MinValue, MaxValue);
  if (Size < 4) then
    Value := RoundCurrTo(Value, Size);
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateCurrency(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{$IFNDEF TFIELD_HAS_ASLARGEINT}
procedure TZBCDField.SetAsLargeInt(const Value: LargeInt);
begin
  SetAsCurrency(Value);
end;
{$ENDIF TFIELD_HAS_ASLARGEINT}

{ TZFMTBCDField }

procedure TZFMTBCDField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZFMTBCDField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZFMTBCDField.FilledValueWasNull(var Value: TBCD): Boolean;
begin
  if IsRowDataAvailable then begin
    TZAbstractRODataset(DataSet).FResultSet.GetBigDecimal(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function Result does not seem to be initialized} {$ENDIF}
function TZFMTBCDField.GetAsBCD: TBcd;
var U: {$IFDEF CPU64}UInt64{$ELSE}Cardinal{$ENDIF} absolute Result;
begin
  if FilledValueWasNull(Result) then
    U := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZFMTBCDField.GetAsCurrency: Currency;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetCurrency(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

function TZFMTBCDField.GetAsFloat: Double;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetDouble(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := 0;
end;

{$IFDEF WITH_FTSINGLE}
function TZFMTBCDField.GetAsSingle: Single;
begin
  Result := GetAsFloat;
end;
{$ENDIF WITH_FTSINGLE}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZFMTBCDField.GetAsString: string;
var BCD: TBCD;
begin
  if FilledValueWasNull(BCD)
  then Result := ''
  else Result := BcdToStr(bcd);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZFMTBCDField.GetAsVariant: Variant;
var BCD: TBcd;
begin
  if FilledValueWasNull(BCD)
  then Result := null
  else Result := VarFMTBcdCreate(Bcd);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZFMTBCDField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZFMTBCDField.GetText(var Text: string; DisplayText: Boolean);
var Format: TFloatFormat;
    Digits: Integer;
    FmtStr: string;
    Bcd: TBcd;
begin
  if FilledValueWasNull(BCD)
  then Text := ''
  else begin
    if DisplayText or (EditFormat = '')
    then FmtStr := DisplayFormat
    else FmtStr := EditFormat;
    if FmtStr = '' then begin
      if Currency then begin
        if DisplayText
        then Format := ffCurrency
        else Format := ffFixed;
        Digits := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}CurrencyDecimals;
      end else begin
        Format := ffGeneral;
        Digits := {$IFDEF FPC}Size{$ELSE}0{$ENDIF};
      end;
      Text := BcdToStrF(Bcd, Format, Precision, Digits);
    end else
      Text := FormatBcd(FmtStr, Bcd);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZFMTBCDField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZFMTBCDField.SetAsCurrency(Value: Currency);
var BCD: TBCD;
begin
  Currency2Bcd(Value, BCD);
  SetAsBCD(BCD);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZBooleanField }

procedure TZBooleanField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZBooleanField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZBooleanField.FilledValueWasNull(var Value: Boolean): Boolean;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    Value := FResultSet.GetBoolean(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Result := FResultSet.WasNull;
  end else Result := True;
end;

function TZBooleanField.GetAsBoolean: Boolean;
begin
  if FilledValueWasNull(Result{%H-}) then
    Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBooleanField.GetAsString: string;
var B: Boolean;
begin
  if FilledValueWasNull(B)
  then Result := ''
  else if B
    then Result := {$IFDEF FPC}'TRUE'{$ELSE}STextTrue{$ENDIF}
    else Result := {$IFDEF FPC}'FALSE'{$ELSE}STextFalse{$ENDIF};
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZBooleanField.GetAsVariant: Variant;
var B: Boolean;
begin
  if FilledValueWasNull(B)
  then Result := null
  else Result := B;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBooleanField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZBooleanField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZBooleanField.SetAsBoolean(Value: Boolean);
  procedure DoValidate(W: Word);
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, SizeOf(WordBool));
    PWord(FValidateBuffer)^ := W;
    Validate(FValidateBuffer);
    Value := PWordBool(FValidateBuffer)^;
    {$ELSE WITH_TVALUEBUFFER}
    Validate(@W);
    Value := W <> 0;
    {$ENDIF WITH_TVALUEBUFFER}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if Assigned(OnValidate) then
    DoValidate(Ord(Value));
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateBoolean(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZBooleanField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else SetAsBoolean(Value);
end;

{ TZGuidField }

procedure TZGuidField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZGuidField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZGuidField.FilledValueWasNull(var Value: TGUID): Boolean;
begin
  if IsRowDataAvailable then begin
    TZAbstractRODataset(DataSet).FResultSet.GetGUID(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
    Result := TZAbstractRODataset(DataSet).FResultSet.WasNull;
  end else Result := True;
end;

function TZGuidField.GetAsGuid: TGUID;
begin
  if FilledValueWasNull(Result{%H-}) then begin
    PInt64(@Result.D1)^ := 0;
    PInt64(@Result.D4)^ := 0;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF} //rolling eyes
function TZGuidField.GetAsString: String;
var Value: TGUID;
begin
  if FilledValueWasNull(Value)
  then Result := ''
  else Result := {$IFDEF UNICODE}GUIDToUnicode{$ELSE}GUIDToRaw{$ENDIF}(Value, [guidWithBrackets]);
end;
{$IFDEF FPC} {$POP} {$ENDIF} //rolling eyes

function TZGuidField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZGuidField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZGuidField.SetAsGuid(const Value: TGUID);
var P: PGUID;
  procedure DoValidate;
  var UID: TGUID;
  begin
    {$IFDEF WITH_TVALUEBUFFER}
    if FValidateBuffer = nil then
      SetLength(FValidateBuffer, 38+1);
    {$ENDIF WITH_TVALUEBUFFER}
    P := @FValidateBuffer[0];
    GUIDToBuffer(@Value.D1, PAnsiChar(P), [guidWithBrackets, guidSet0Term]);
    {$IFDEF WITH_TVALUEBUFFER}
    Validate(FValidateBuffer);
    {$ELSE WITH_TVALUEBUFFER}
    Validate(P);
    {$ENDIF WITH_TVALUEBUFFER}
    ValidGUIDToBinary(PAnsiChar(P), @UID.D1);
    P^ := UID;
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := @Value.D1;
  if Assigned(OnValidate) then
    DoValidate;
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateGUID(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P^);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZRawStringField }

procedure TZRawStringField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if (DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
    with TZAbstractRODataset(DataSet) do begin
      if FCharEncoding = ceUTF16
      then FColumnCP := zCP_UTF16
      else FColumnCP := FResultSetMetadata.GetColumnCodePage(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      Transliterate := Transliterate or (FColumnCP = zCP_UTF16) or (
        TZAbstractRODataset(DataSet).Connection.RawCharacterTransliterateOptions.Fields and
        (FColumnCP <>  GetTransliterateCodePage(Connection.ControlsCodePage)));
      if (FColumnCP = zCP_UTF8)
      then FBufferSize := Size shl 2
      else FBufferSize := Size * ZOSCodePageMaxCharSize;
      FBufferSize := FBufferSize +1
    end;
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZRawStringField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

constructor TZRawStringField.Create(AOwner: TComponent);
begin
  inherited;
  Transliterate := False;
end;

{$IFDEF FPC}
const SFieldOutOfRange = 'Value for field '#39'%s'#39' is out of range.';
{$ENDIF}

function TZRawStringField.CreateSizeError: EZDatabaseError;
begin
  Result := EZDatabaseError.Create(Format(SFieldOutOfRange, [DisplayName]));
end;

{$IFNDEF NO_ANSISTRING}
function TZRawStringField.GetAsAnsiString: AnsiString;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetAnsiString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$ENDIF NO_ANSISTRING}

function TZRawStringField.GetAsBoolean: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetBoolean(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := False;
end;

{$IFDEF TFIELD_HAS_ASBYTES}
function TZRawStringField.GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
var TransliterateCP: Word;
    P: Pointer;
    L:  NativeUint;
label jmpSet1, jmpSetL;
begin
  Result := nil;
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    TransliterateCP := GetTransliterateCodePage(Connection.ControlsCodePage);
    if (FColumnCP = zCP_UTF16) then begin
      P := FResultSet.GetPWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      if L > 0 then begin
        SetLength(Result, L shl 2);
        L := ZEncoding.PUnicode2PRawBuf(P, Pointer(Result), L, L shl 2, TransliterateCP);
        goto jmpSetL;
      end else goto jmpSet1;
    end else begin
      P := FResultSet.GetPAnsiChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      if (L > 0) then
        if Transliterate then begin
          SetLength(Result, L shl 2);
          L := PRawToPRawBuf(P, Pointer(Result), L, L shl 2, FColumnCP, TransliterateCP);
jmpSetL:  SetLength(Result, L+1);
        end else begin
          SetLength(Result, L+1);
          Move(P^, Pointer(Result)^, L);
        end
      else
jmpSet1: SetLength(Result, 1);
    end;
    PByte(PAnsiChar(Result)+L)^ := 0;
  end else Result := nil;
end;
{$ENDIF}

function TZRawStringField.GetAsString: String;
{$IFNDEF UNICODE}
var TransliterateCP: Word;
    P: Pointer;
    L:  NativeUint;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    TransliterateCP := GetTransliterateCodePage(Connection.ControlsCodePage);
    if (FColumnCP = zCP_UTF16) then begin
      P := FResultSet.GetPWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      Result := PUnicodeToRaw(P, L, TransliterateCP)
    end else begin
      P := FResultSet.GetPAnsiChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      Result := '';
      if Transliterate
      then PRawToRawConvert(P, L, FColumnCP, TransliterateCP, RawByteString(Result))
      {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
      else begin
        Result := '';
        ZSetString(P, L, RawByteString(Result), FColumnCP);
      end;
      {$ELSE}
      else System.SetString(Result, PAnsiChar(P), L);
      {$ENDIF}
    end
  end else Result := '';
  {$ELSE}
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
  {$ENDIF}
end;

function TZRawStringField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZRawStringField.GetAsRawByteString: RawByteString;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetRawByteString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
function TZRawStringField.GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF};
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
function TZRawStringField.GetAsUnicodeString: UnicodeString;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZRawStringField.GetAsUTF8String: UTF8String;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUTF8String(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$ENDIF NO_UTF8STRING}

function TZRawStringField.GetAsVariant: Variant;
begin
  if IsRowDataAvailable
  then with TZAbstractRODataset(DataSet) do begin
    if (FCharEncoding <> ceUTF16) and (FColumnCP = GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage))
    then Result := FResultSet.GetString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
    else Result := FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    if FResultSet.WasNull then
      Result := null;
  end else Result := null;
end;


function TZRawStringField.GetDataSize: Integer;
begin
  if FBound
  then Result := FBufferSize
  else Result := inherited GetDataSize;
end;

function TZRawStringField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZRawStringField.SetAsAnsiString(const Value: AnsiString);
  procedure SetAsUni;
  var US: UnicodeString;
  begin
    US := ZRawToUnicode(Value, ZOSCodePage);
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    SetAsUnicodeString(US);
    {$ELSE FIELD_ASWIDESTRING_IS_UNICODESTRING}
    SetAsWideString(US);
    {$ENDIF FIELD_ASWIDESTRING_IS_UNICODESTRING}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    if (Value = '') or ((FColumnCP = ZOSCodePage) and (ZOSCodePageMaxCharSize = 1) )
    then SetAsRawByteString(Value)
    else SetAsUni;
  end;
end;
{$ENDIF NO_ANSISTRING}

procedure TZRawStringField.SetAsRawByteString(const Value: RawByteString);
var P: PAnsiChar;
  L: NativeUInt;
  procedure SetAsW;
  var W: UnicodeString;
    CP: Word;
    L: NativeUint;
    P: PWideChar;
  begin
    with TZAbstractRODataset(DataSet) do begin
      if FColumnCP = zCP_UTF16
      then CP := FResultSet.GetConSettings.ClientCodePage.CP
      else CP := FColumnCP;
      if CP = zCP_UTF16 then
        CP := GetTransliterateCodePage(Connection.ControlsCodePage);
      W := ZRawToUnicode(Value, CP);
      L := Length(W);
      if L = 0
      then P := PEmptyUnicodeString
      else P := Pointer(W);
      FResultSet.UpdatePWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P, L);
    end;
  end;
  procedure DoValidate;
  begin
    SetLength(FValidateBuffer, Max(L, FBufferSize){$IFDEF WITH_TVALUEBUFFER}+1{$ENDIF});
    if L > 0 then
      Move(P^, Pointer(FValidateBuffer)^, L);
    P := Pointer(FValidateBuffer);
    PByte(P+L)^ := 0;
    {$IFNDEF NO_TDATASET_TRANSLATE}
    if Transliterate and (doOemTranslate in TZAbstractRODataset(DataSet).Options) then
      DataSet.Translate(P, P, True);
    {$ENDIF NO_TDATASET_TRANSLATE}
    Validate({$IFDEF WITH_TVALUEBUFFER}FValidateBuffer{$ELSE}P{$ENDIF});
    L := ZFastCode.StrLen(P);
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    P := Pointer(Value);
    if P = nil
    then L := 0
    else L := ZFastCode.StrLen(P); //the Delphi/FPC guys did decide to allow no zero byte in middle of a string propably because of Validate(Buffer)
    if Assigned(OnValidate) {$IFNDEF NO_TDATASET_TRANSLATE}or (Transliterate and (doOemTranslate in TZAbstractRODataset(DataSet).Options)) {$ENDIF} then
      DoValidate;
    if L > FBufferSize then
      raise CreateSizeError;
    if (FColumnCP = zCP_UTF16)
    then SetAsW
    else FResultSet.UpdatePAnsiChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P, L);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZRawStringField.SetAsString(const Value: String);
var
{$IFNDEF UNICODE}
    L: LengthInt;
    P: PAnsiChar;
  procedure DoValidate;
  begin
    SetLength(FValidateBuffer, Math.Max(L, Size){$IFDEF WITH_TVALUEBUFFER}+1{$ENDIF});
    if L > 0 then
      Move(P^, Pointer(FValidateBuffer)^, L+1);
    P := Pointer(FValidateBuffer);
    PByte(P+L)^ := 0;
    {$IFDEF WITH_TVALUEBUFFER}
    Validate(FValidateBuffer);
    {$ELSE WITH_TVALUEBUFFER}
    Validate(P);
    {$ENDIF WITH_TVALUEBUFFER}
    if Transliterate then
      DataSet.Translate(P, P, True);
    L := ZFastCode.StrLen(P);
  end;
{$ELSE}
  PW: PWideChar;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  PW := Pointer(Value);
  SetPWideChar(PW, SysUtils.StrLen(PW));
  {$ELSE}
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    P := Pointer(Value);
    if P = nil
    then L := 0
    else L := ZFastCode.StrLen(P);  //the Delphi/FPC guys did decide to allow no zero byte in middle of a string propably because of Validate(Buffer)
    if Transliterate then begin
      FUniTemp := PRawToUnicode(P, L, GetTransliterateCodePage(Connection.ControlsCodePage));
      SetAsUnicodeString(FUniTemp);
    end else SetAsRawByteString(Value);
  end;
  {$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
procedure TZRawStringField.SetAsWideString(const Value: {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}WideString{$ELSE}UnicodeString{$ENDIF});
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
procedure TZRawStringField.SetAsUnicodeString(const Value: UnicodeString);
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZRawStringField.SetAsUTF8String(const Value: UTF8String);
var P: PAnsiChar;
    L: NativeUInt;
  procedure SetW;
  Var US: UnicodeString;
  begin
    US := ZRawToUnicode(Value, zCP_UTF8);
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    SetAsUnicodeString(US);
    {$ELSE FIELD_ASWIDESTRING_IS_UNICODESTRING}
    SetAsWideString(US);
    {$ENDIF FIELD_ASWIDESTRING_IS_UNICODESTRING}
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  if (Value = '') or ((FColumnCP = zCP_UTF8) and (CountOfUtf8Chars(P,L)<=NativeUInt(Size)))
  then SetAsRawByteString(Value)
  else SetW; //no UStrClear here
end;
{$ENDIF NO_UTF8STRING}

procedure TZRawStringField.SetPWideChar(P: Pointer; Len: NativeUint);
var RawCP: Word;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  if Len > NativeUInt(Size) then
    raise CreateSizeError;
  with TZAbstractRODataset(DataSet) do begin
    if Assigned(OnValidate) then begin
      if (FColumnCP = zCP_UTF16)
      then RawCP := GetTransliterateCodePage(Connection.ControlsCodePage)
      else RawCP := FColumnCP;
      SetAsRawByteString(PUnicodeToRaw(P, Len, RawCP))
    end else with TZAbstractRODataset(DataSet) do begin
      Prepare4DataManipulation(Self);
      if P = nil then
        P := PEmptyUnicodeString;
      FResultSet.UpdatePWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P, Len);
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

{ TZUnicodeStringField }

procedure TZUnicodeStringField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      FFieldIndex := GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
      if FCharEncoding = ceUTF16
      then FColumnCP := zCP_UTF16
      else FColumnCP := FResultSetMetadata.GetColumnCodePage(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    end;
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZUnicodeStringField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZUnicodeStringField.CreateSizeError: EZDatabaseError;
begin
  Result := EZDatabaseError.Create(Format(SFieldOutOfRange, [DisplayName]));
end;

{$IFNDEF NO_ANSISTRING}
function TZUnicodeStringField.GetAsAnsiString: AnsiString;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetAnsiString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$ENDIF NO_ANSISTRING}

{$IFDEF TFIELD_HAS_ASBYTES}
function TZUnicodeStringField.GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
var P: Pointer;
    L:  NativeUint;
begin
  Result := nil;
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    P := FResultSet.GetPWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
    L := L shl 1;
    SetLength(Result, L+2);
    Move(P^, Pointer(Result)^, L);
    PWord(PAnsiChar(Result)+L)^ := 0;
  end else Result := nil;
end;
{$ENDIF TFIELD_HAS_ASBYTES}

function TZUnicodeStringField.GetAsString: String;
{$IFNDEF UNICODE}
var TransliterateCP: Word;
    P: Pointer;
    L:  NativeUint;
begin
  if IsRowDataAvailable then with TZAbstractRODataset(DataSet) do begin
    TransliterateCP := GetTransliterateCodePage(Connection.ControlsCodePage);
    if (FColumnCP = zCP_UTF16) then begin
      P := FResultSet.GetPWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      Result := PUnicodeToRaw(P, L, TransliterateCP)
    end else begin
      P := FResultSet.GetPAnsiChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
      if TransliterateCP <> FColumnCP
      then ZEncoding.PRawToRawConvert(P, L, FColumnCP, TransliterateCP, RawByteString(Result))
      {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
      else begin
        Result := '';
        ZSetString(P, L, RawByteString(Result), FColumnCP);
      end;
      {$ELSE}
      else System.SetString(Result, PAnsiChar(P), L);
      {$ENDIF}
    end
  end else Result := '';
{$ELSE}
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
{$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
function TZUnicodeStringField.GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF};
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
function TZUnicodeStringField.GetAsUnicodeString: UnicodeString;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;
{$ENDIF}


function TZUnicodeStringField.GetAsUTF8String: UTF8String;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.GetUTF8String(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := '';
end;

function TZUnicodeStringField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

function TZUnicodeStringField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZUnicodeStringField.SetAsAnsiString(const Value: AnsiString);
Var U: UnicodeString;
begin
  U := ZRawToUnicode(Value, zOSCodePage);
  {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or not defined(HAVE_UNICODESTRING)}
  SetAsWideString(U);
  {$ELSE}
  SetAsUnicodeString(U);
  {$IFEND}
end;
{$ENDIF NO_ANSISTRING}

procedure TZUnicodeStringField.SetAsString(const Value: String);
{$IFDEF UNICODE}
begin
  SetPWideChar(Pointer(Value), Length(Value));
{$ELSE}
var L: NativeUInt;
    P: PAnsiChar;
    StringCP: Word;
  procedure SetW(StrCP: Word);
  var U: UnicodeString;
  begin
    U := PRawToUnicode(P, L, StrCP);
    {$IFNDEF HAVE_UNICODESTRING}
    SetAsWideString(U);
    {$ELSE}
    SetAsUnicodeString(U);
    {$ENDIF}
  end;
label jmpMove;
begin
  //we convert all values to UTF16 for Size control except the value and encoding do fit into
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  if P = nil
  then L := 0
  else L := ZFastCode.StrLen(P);  //the Delphi/FPC guys did decide to allow no zero byte in middle of a string propably because of Validate(Buffer)
  with TZAbstractRODataset(DataSet) do begin
    if (L = 0) then
      if Assigned(OnValidate) or (FColumnCP <> zCP_UTF16)
      then SetW(zCP_WIN1252)
      else goto jmpMove
    else begin
      StringCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF};
      if Assigned(OnValidate) or (StringCP <> FColumnCP) then
        SetW(StringCP)
      else if (StringCP = ZCP_UTF8) then
        if ((L <= NativeUInt(Size)) or (CountOfUtf8Chars(P,L)  <= NativeUInt(Size)))
        then goto jmpMove
        else raise CreateSizeError
      else if (FColumnCP = StringCP) and (L <= NativeUInt(Size*ZOSCodePageMaxCharSize)) then begin
jmpMove:Prepare4DataManipulation(Self);
        FResultSet.UpdatePAnsiChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P, L);
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
          DataEvent(deFieldChange, NativeInt(Self));
      end else
        SetW(StringCP);
    end;
  end;
{$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
procedure TZUnicodeStringField.SetAsWideString(const Value: {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}WideString{$ELSE}UnicodeString{$ENDIF});
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
procedure TZUnicodeStringField.SetAsUnicodeString(const Value: UnicodeString);
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$ENDIF}

procedure TZUnicodeStringField.SetAsUTF8String(const Value: UTF8String);
Var U: UnicodeString;
begin
  U := ZRawToUnicode(Value, zCP_UTF8);
  {$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or not defined(HAVE_UNICODESTRING)}
  SetAsWideString(U);
  {$ELSE}
  SetAsUnicodeString(U);
  {$IFEND}
end;

procedure TZUnicodeStringField.SetPWideChar(P: PWideChar; Len: NativeUint);
  procedure DoValidate;
  begin
    {$IFDEF TWIDESTRINGFIELD_DATABUFFER_IS_PWIDESTRING}
    SetLength(FValidateBuffer, Len);
    {$ELSE}
      {$IFDEF WITH_TVALUEBUFFER}
      SetLength(FValidateBuffer, (Math.Max(Len, Size)+1) shl 1);
      {$ELSE}
      SetLength(FValidateBuffer, Math.Max(Len, Size));
      {$ENDIF}
    {$ENDIF}
    if Len > 0 then
      Move(P^, Pointer(FValidateBuffer)^, (Len+1) shl 1);
    P := Pointer(FValidateBuffer);
    PWord(P + Len)^ := 0;
    {$IFDEF TWIDESTRINGFIELD_DATABUFFER_IS_PWIDESTRING}
    Validate(@FValidateBuffer);
    P := Pointer(FValidateBuffer);
    if P = nil
    then Len := 0 else
    {$ELSE}
    Validate({$IFDEF WITH_TVALUEBUFFER}FValidateBuffer{$ELSE}P{$ENDIF});
    {$ENDIF}
    Len := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(P);
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if Assigned(OnValidate) then
      DoValidate;
    if P = nil then
      P := PEmptyUnicodeString;
    if Len > NativeUInt(Size) then
      raise CreateSizeError;
    FResultSet.UpdatePWideChar(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, P, Len);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZBytesField }

procedure TZBytesField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZBytesField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZBytesField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

const cBytesText: array[Boolean] of String = ('(BYTES)', '(Bytes)');
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZBytesField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsRowDataAvailable
  then Text := cBytesText[TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})]
  else Text := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBytesField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{ TZVarBytesField }

procedure TZVarBytesField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZVarBytesField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZVarBytesField.GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
var P: PByte;
    L: NativeUint;
begin
  Result := nil;
  if IsRowDataAvailable then begin
    P := TZAbstractRODataset(DataSet).FResultSet.GetBytes(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, L);
    if L > 0 then begin
      SetLength(Result, L);
      Move(P^, Pointer(Result)^, L);
    end;
  end;
end;

function TZVarBytesField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

const cVarBytesText: array[Boolean] of String = ('(VARBYTES)', '(VarBytes)');
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZVarBytesField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsRowDataAvailable
  then Text := cVarBytesText[TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})]
  else Text := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZVarBytesField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

procedure TZVarBytesField.SetAsBytes(const Value: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF});
var L: NativeUInt;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  L := Length(Value);
  if L = 0
  then Clear
  else with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    FResultSet.UpdateBytes(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Pointer(Value), L);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZRawCLobField }

procedure TZRawCLobField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      FFieldIndex := GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
      if FCharEncoding = ceUTF16
      then FColumnCP := zCP_UTF16
      else FColumnCP := FResultSetMetadata.GetColumnCodePage(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      Transliterate := Transliterate or (FColumnCP = zCP_UTF16) or (
        TZAbstractRODataset(DataSet).Connection.RawCharacterTransliterateOptions.Fields and
        (FColumnCP <>  GetTransliterateCodePage(Connection.ControlsCodePage)));
    end;
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZRawCLobField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

procedure TZRawCLobField.SetPWideChar(P: Pointer; Len: NativeUint);
var Clob: IZClob;
    R: RawByteString;
    ConSettings: PZConSettings;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    ConSettings := FRowAccessor.ConSettings;
    if (FColumnCP = zCP_UTF16) then begin
       if P = nil then
          P:= PEmptyUnicodeString;
       CLob := TZLocalMemCLob.CreateWithData(nil, 0, ConSettings, FOpenLobStreams);
       Clob.SetPWideChar(P, Len); //notify updated
    end else begin
      R := PUnicodeToRaw(P,Len,FColumnCP);
      Len := Length(R);
      if Len = 0
      then P := PEmptyUnicodeString
      else P := Pointer(R);
      CLob := TZLocalMemCLob.CreateWithData(nil, 0, FColumnCP, ConSettings, FOpenLobStreams);
      CLob.SetPAnsiChar(P, FColumnCP, Len);
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZRawCLobField.SetVarValue(const Value: Variant);
begin
  if TVarData(Value).VType <= 1 //in [varEmpty, varNull]
  then Clear
  else if (TVarData(Value).VType = varOleStr) {$IFDEF WITH_varUString} or (TVarData(Value).VType = varUString){$ENDIF}
    {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    then SetAsUnicodeString(Value)
    {$ELSE FIELD_ASWIDESTRING_IS_UNICODESTRING}
    then SetAsWideString(Value)
    {$ENDIF FIELD_ASWIDESTRING_IS_UNICODESTRING}
    else SetAsString(Value);
end;

{$IFNDEF NO_ANSISTRING}
function TZRawCLobField.GetAsAnsiString: AnsiString;
var Lob: IZBlob;
    Clob: IZClob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := CLob.GetAnsiString
      else Result := '';
    end
  else Result := '';
end;
{$ENDIF NO_ANSISTRING}

function TZRawCLobField.GetAsRawByteString: RawByteString;
var Lob: IZBlob;
    Clob: IZClob;
    CP: Word;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK) then begin
        if (FColumnCP = zCP_UTF16)
        then CP := GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage)
        else CP := FColumnCP;
        Result := Clob.GetRawByteString(CP);
      end else Result := ''
    end
  else Result := '';
end;


function TZRawCLobField.GetAsString: String;
{$IFNDEF UNICODE}
var Clob: IZClob;
    Lob: IZBlob;
    CP: Word;
    R: RawByteString;
    P: PAnsiChar;
    L: NativeUInt;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := GetAsWideString;
  {$ELSE}
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK) then begin
        if (FColumnCP = zCP_UTF16) or Transliterate
        then CP := GetTransliterateCodePage(Connection.ControlsCodePage)
        else CP := FColumnCP;
        R := '';
        P := Clob.GetPAnsiChar(CP, R, L);
        if (L<>0) and (P <> Pointer(R)) then begin
          {$IFDEF FPC}Result := '';{$ENDIF}
          System.SetString(Result, P, L);
        end else Result := R;
      end else Result := ''
    end
  else Result := '';
  {$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
function TZRawCLobField.GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF};
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := CLob.GetUnicodeString
      else Result := ''
    end
  else Result := '';
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
function TZRawCLobField.GetAsUnicodeString: UnicodeString;
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := CLob.GetUnicodeString
      else Result := ''
    end
  else Result := '';
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZRawCLobField.GetAsUTF8String: UTF8String;
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := CLob.GetUTF8String
      else Result := ''
    end
  else Result := '';
end;
{$ENDIF NO_UTF8STRING}

function TZRawCLobField.GetAsVariant: Variant;
begin
  if IsRowDataAvailable
  then with TZAbstractRODataset(DataSet) do begin
    if (FColumnCP = GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage))
    then Result := FResultSet.GetString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
    else Result := FResultSet.GetUnicodeString(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    if FResultSet.WasNull then
      Result := null;
  end else Result := null;
end;


function TZRawCLobField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

const cMemoText: array[Boolean] of String = ('(MEMO)', '(Memo)');
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZRawCLobField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsRowDataAvailable
  then Text := cMemoText[TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})]
  else Text := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZRawCLobField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZRawCLobField.SetAsAnsiString(const Value: AnsiString);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    R: RawByteString;
    U: UnicodeString;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16) then begin
      U := PRawToUnicode(P,L,zOSCodePage);
      P := Pointer(U);
      L := Length(U);
      CLob.SetPWideChar(P, L)
    end else if FColumnCP = zOSCodePage then
      CLob.SetPAnsiChar(P, zOSCodePage, L)
    else begin
      U := PRawToUnicode(P,L,zOSCodePage);
      R := PUnicodeToRaw(P,L,FColumnCP);
      CLob.SetRawByteString(R, FColumnCP);
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, CLob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF NO_ANSISTRING}

procedure TZRawCLobField.SetAsRawByteString(const Value: RawByteString);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    U: UnicodeString;
  procedure SetW(CP: Word);
  begin
    U := PRawToUnicode(P,L,CP);
    P := Pointer(U);
    L := Length(U);
    CLob.SetPWideChar(P, L)
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16)
    then SetW(GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage))
    else CLob.SetPAnsiChar(P, FColumnCP, L);
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;


procedure TZRawCLobField.SetAsString(const Value: String);
{$IFNDEF UNICODE}
var L: LengthInt;
    P: PAnsiChar;
    Blob: IZBlob;
    Clob: IZCLob;
  procedure SetW(StrCP: Word);
  var U: UnicodeString;
  begin
    U := PRawToUnicode(P, L, StrCP);
    SetPWideChar(Pointer(U), Length(U));
  end;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  SetAsWideString(Value);
  {$ELSE}
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    P := Pointer(Value);
    if P = nil then begin
      L := 0;
      P := PEmptyAnsiString
    end else L := ZFastCode.StrLen(P);  //the Delphi/FPC guys did decide to allow no zero byte in middle of a string propably because of Validate(Buffer)
    if (FColumnCP = zCP_UTF16) or Transliterate
    then SetW(GetTransliterateCodePage(Connection.ControlsCodePage)) else begin
      Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
      BLob.QueryInterface(IZCLob, Clob);
      CLob.SetPAnsiChar(P,FColumnCP,L);
      FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    end;
  end;
  {$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
procedure TZRawCLobField.SetAsWideString(const Value: {$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}WideString{$ELSE}UnicodeString{$ENDIF});
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
procedure TZRawCLobField.SetAsUnicodeString(const Value: UnicodeString);
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZRawCLobField.SetAsUTF8String(const Value: UTF8String);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    R: RawByteString;
    U: UnicodeString;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16) then begin
      U := PRawToUnicode(P,L,zCP_UTF8);
      P := Pointer(U);
      L := Length(U);
      CLob.SetPWideChar(P, L)
    end else if FColumnCP = zCP_UTF8 then
      CLob.SetPAnsiChar(P, zCP_UTF8, L)
    else begin
      U := PRawToUnicode(P,L,zCP_UTF8);
      R := PUnicodeToRaw(P,L,FColumnCP);
      CLob.SetRawByteString(R, FColumnCP);
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF NO_UTF8STRING}

{ TZUnicodeCLobField }

{$IFDEF WITH_WIDEMEMO}
procedure TZUnicodeCLobField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    with TZAbstractRODataset(DataSet) do begin
      FFieldIndex := GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
      if FCharEncoding = ceUTF16
      then FColumnCP := zCP_UTF16
      else FColumnCP := FResultSetMetadata.GetColumnCodePage(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    end;
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZUnicodeCLobField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

procedure TZUnicodeCLobField.SetPWideChar(P: Pointer; Len: NativeUint);
var Blob: IZBlob;
    Clob: IZClob;
    R: RawByteString;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16) then begin
       if P = nil then
          P:= PEmptyUnicodeString;
       Clob.SetPWideChar(P, Len); //notify updated
    end else begin
      R := PUnicodeToRaw(P,Len,FColumnCP);
      Len := Length(R);
      if Len = 0
      then P := PEmptyUnicodeString
      else P := Pointer(R);
      CLob.SetPAnsiChar(P, FColumnCP, Len); //notify updated
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{$IFNDEF NO_ANSISTRING}
function TZUnicodeCLobField.GetAsAnsiString: AnsiString;
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := CLob.GetAnsiString
      else Result := ''
    end
  else Result := '';
end;
{$ENDIF NO_ANSISTRING}

function TZUnicodeCLobField.GetAsRawByteString: RawByteString;
var Clob: IZClob;
    Lob: IZBlob;
    CP: Word;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK) then begin
        if FColumnCP = zCP_UTF16
        then CP := GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage)
        else CP := FColumnCP;
        Result := Clob.GetRawByteString(CP);
      end else Result := ''
    end
  else Result := '';
end;


function TZUnicodeCLobField.GetAsString: String;
{$IFNDEF UNICODE}
var Clob: IZClob;
    Lob: IZBlob;
    CP: Word;
    R: RawByteString;
    P: PAnsiChar;
    L: NativeUInt;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := GetAsWideString;
  {$ELSE}
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK) then begin
        CP := GetTransliterateCodePage(Connection.ControlsCodePage);
        R := '';
        P := Clob.GetPAnsiChar(CP, R, L);
        if (L<>0) and (P <> Pointer(R)) then begin
          {$IFDEF FPC}Result := '';{$ENDIF}
          System.SetString(Result, P, L)
        end else Result := R;
      end else Result := ''
    end
  else Result := '';
  {$ENDIF}
end;

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
function TZUnicodeCLobField.GetAsWideString: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF};
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := Clob.GetUnicodeString
      else Result := ''
    end
  else Result := '';
end;
{$IFEND}

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
function TZUnicodeCLobField.GetAsUnicodeString: UnicodeString;
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := Clob.GetUnicodeString
      else Result := ''
    end
  else Result := '';
end;
{$ENDIF FIELD_ASWIDESTRING_IS_UNICODESTRING}

{$IFNDEF NO_UTF8STRING}
function TZUnicodeCLobField.GetAsUTF8String: UTF8String;
var Clob: IZClob;
    Lob: IZBlob;
begin
  if IsRowDataAvailable then
    with TZAbstractRODataset(DataSet) do begin
      Lob := FResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if (Lob <> nil) and (Lob.QueryInterface(IZCLob, Clob) = S_OK)
      then Result := Clob.GetUTF8String
      else Result := ''
    end
  else Result := '';
end;
{$ENDIF NO_UTF8STRING}

function TZUnicodeCLobField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

const cWideMemoText: array[Boolean] of String = ('(WIDEMEMO)', '(WideMemo)');
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZUnicodeCLobField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsRowDataAvailable
  then Text := cWideMemoText[TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})]
  else Text := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IF defined(FIELD_ASWIDESTRING_IS_UNICODESTRING) or defined(WITH_VIRTUAL_TFIELD_ASWIDESTRING)}
procedure TZUnicodeCLobField.SetAsWideString(const Value: {$IFDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}UnicodeString{$ELSE}WideString{$ENDIF});
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$IFEND}

function TZUnicodeCLobField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZUnicodeCLobField.SetAsAnsiString(const Value: AnsiString);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    R: RawByteString;
    U: UnicodeString;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16) then begin
      U := PRawToUnicode(P,L,zOSCodePage);
      P := Pointer(U);
      L := Length(U);
      CLob.SetPWideChar(P, L)
    end else if FColumnCP = zOSCodePage then
      CLob.SetPAnsiChar(P, zOSCodePage, L)
    else begin
      U := PRawToUnicode(P,L,zOSCodePage);
      R := PUnicodeToRaw(P,L,FColumnCP);
      CLob.SetRawByteString(R, FColumnCP);
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF NO_ANSISTRING}

procedure TZUnicodeCLobField.SetAsRawByteString(const Value: RawByteString);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    U: UnicodeString;
  procedure SetW(CP: Word);
  begin
    U := PRawToUnicode(P,L,CP);
    P := Pointer(U);
    L := Length(U);
    CLob.SetPWideChar(P, L)
  end;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    Blob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16)
    then SetW(GetTransliterateCodePage(TZAbstractRODataset(DataSet).Connection.ControlsCodePage))
    else CLob.SetPAnsiChar(P, FColumnCP, L);
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZUnicodeCLobField.SetAsString(const Value: String);
{$IFNDEF UNICODE}
var L: LengthInt;
    P: PAnsiChar;
    Blob: IZBlob;
    Clob: IZCLob;
    StringCP: Word;
  procedure SetW(StrCP: Word);
  var U: UnicodeString;
  begin
    U := PRawToUnicode(P, L, StrCP);
    SetPWideChar(Pointer(U), Length(U));
  end;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  SetPWideChar(Pointer(Value), Length(Value));
  {$ELSE}
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    P := Pointer(Value);
    if P = nil then begin
      L := 0;
      P := PEmptyAnsiString;
    end else L := ZFastCode.StrLen(P);  //the Delphi/FPC guys did decide to allow no zero byte in middle of a string propably because of Validate(Buffer)
    StringCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF};
    if (StringCP <> FColumnCP) then
      SetW(StringCP)
    else begin
      Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
      BLob.QueryInterface(IZCLob, Clob);
      CLob.SetPAnsiChar(P,FColumnCP,L);
      FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    end;
  end;
  {$ENDIF}
end;

{$IFNDEF FIELD_ASWIDESTRING_IS_UNICODESTRING}
procedure TZUnicodeCLobField.SetAsUnicodeString(const Value: UnicodeString);
begin
  SetPWideChar(Pointer(Value), Length(Value));
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZUnicodeCLobField.SetAsUTF8String(const Value: UTF8String);
var Blob: IZBlob;
    Clob: IZClob;
    P: Pointer;
    L: NativeUInt;
    R: RawByteString;
    U: UnicodeString;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  P := Pointer(Value);
  L := Length(Value);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    Blob := ResultSet.GetBlob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmWrite);
    BLob.QueryInterface(IZCLob, Clob);
    if (FColumnCP = zCP_UTF16) then begin
      U := PRawToUnicode(P,L,zCP_UTF8);
      P := Pointer(U);
      L := Length(U);
      CLob.SetPWideChar(P, L)
    end else if FColumnCP = zCP_UTF8 then
      CLob.SetPAnsiChar(P, zCP_UTF8, L)
    else begin
      U := PRawToUnicode(P,L,zCP_UTF8);
      R := PUnicodeToRaw(P,L,FColumnCP);
      CLob.SetRawByteString(R, FColumnCP);
    end;
    FResultSet.UpdateLob(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Clob);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF NO_UTF8STRING}

{$ENDIF WITH_WIDEMEMO}

{ TZBLobField }

procedure TZBLobField.Bind(Binding: Boolean);
begin
  FBound := Binding;
  if Binding then begin
    if ((DataSet = nil) or not DataSet.InheritsFrom(TZAbstractRODataset)) then
      raise CreateUnBoundError(Self);
    FFieldIndex := TZAbstractRODataset(DataSet).GetFieldIndex(Self){$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  end;
  {$IFDEF WITH_VIRTUAL_TFIELD_BIND}inherited Bind(Binding);{$ENDIF WITH_VIRTUAL_TFIELD_BIND}
end;

procedure TZBLobField.Clear;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do begin
    Prepare4DataManipulation(Self);
    if not FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      FResultSet.UpdateNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(Self));
    end;
  end;
end;

function TZBLobField.GetIsNull: Boolean;
begin
  if IsRowDataAvailable
  then Result := TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
  else Result := True;
end;

const cBlobText: array[Boolean] of String = ('(BLOB)', '(Blob)');
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZBlobField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsRowDataAvailable
  then Text := cBlobText[TZAbstractRODataset(DataSet).FResultSet.IsNull(FFieldIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF})]
  else Text := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZBLobField.IsRowDataAvailable: Boolean;
var RowBuffer: PZRowBuffer;
begin
  if not FBound then
    raise CreateUnBoundError(Self);
  with TZAbstractRODataset(DataSet) do
    if GetActiveBuffer(RowBuffer) then begin
      FRowAccessor.RowBuffer := RowBuffer;
      Result := True;
    end else Result := False;
end;

initialization
  D1M1Y1 := EncodeDate(1,1,1);
end.
