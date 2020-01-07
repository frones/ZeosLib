{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Dataset utility functions and classes            }
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

unit ZDatasetUtils;

interface

{$I ZComponent.inc}

uses
  Types, Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}Db{$ENDIF},
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ELSE}ZClasses,{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  ZDbcIntfs, ZDbcCache, ZCompatibility, ZExpression, ZVariant, ZTokenizer,
  ZSelectSchema;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): TObjectList;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(const ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(const ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names or field indices separated by ',' or ';'
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFields(DataSet: TDataset; const FieldNames: string;
  out OnlyDataFields: Boolean; const Tokenizer: IZTokenizer): TObjectDynArray;

{**
  Defins a indices of filter fields.
  @param Dataset a dataset object.
  @param Expression a expression calculator.
  @returns an array with field object references.
}
function DefineFilterFields(DataSet: TDataset;
  const Expression: IZExpression): TObjectDynArray;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param ResultSet an initial result set object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const ResultSet: IZResultSet; const ResultValues: TZVariantDynArray);

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromRowAccessor(const FieldRefs: TObjectDynArray;
  const FieldIndices: TIntegerDynArray; RowAccessor: TZRowAccessor;
  const ResultValues: TZVariantDynArray);

{**
  Copy a set of specified field values to variables.
  @param Fields an array with interested field object references.
  @param ResultSet an initial result set object.
  @param Variables a list of variables.
}
procedure CopyDataFieldsToVars(const Fields: TObjectDynArray;
  const ResultSet: IZResultSet; const Variables: IZVariablesList);

{**
  Prepares values for comparison by CompareFieldsFromResultSet.
  @param FieldRefs an array with interested field object references.
  @param DecodedKeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
}
procedure PrepareValuesForComparison(const FieldRefs: TObjectDynArray;
  const DecodedKeyValues: TZVariantDynArray; const ResultSet: IZResultSet;
  PartialKey: Boolean; CaseInsensitive: Boolean; const VariantManager: IZClientVariantManager);

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareDataFields(const KeyValues, RowValues: TZVariantDynArray;
  Const VariantManager: IZVariantManager; PartialKey, CaseInsensitive: Boolean): Boolean;

{**
  Compares row field values with the given ones.
  @param FieldRefs an array with interested field object references.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const KeyValues: TZVariantDynArray; const ResultSet: IZResultSet; PartialKey: Boolean;
  CaseInsensitive: Boolean; const VariantManager: IZClientVariantManager): Boolean;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @param IZIdentifierConvertor IdentifierConverter for the used database
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields; const IdConverter: IZIdentifierConvertor): string;

{**
  Compare values from two key fields.
  @param Field1 the first field object.
  @param ResultSet the resultset to read the first field value.
  @param Field2 the second field object.
}
function CompareKeyFields(Field1: TField; const ResultSet: IZResultSet;
  Field2: TField): Boolean;

{**
  Defins a indices and directions for sorted fields.
  @param Dataset a dataset object.
  @param SortedFields an encoded fields for sorting in the format
    <Field Name> [ASC | DESC] [, ...]
  @param FieldRefs a decoded field object references.
  @param FieldDirs a decoded field directions.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
procedure DefineSortedFields(DataSet: TDataset;
  const SortedFields: string; out FieldRefs: TObjectDynArray;
  out CompareKinds: TComparisonKindArray; out OnlyDataFields: Boolean);

{**
  Creates a fields lookup table to define fixed position
  of the field in dataset.
  @param Fields a collection of TDataset fields in initial order.
  @returns a fields lookup table.
}
function CreateFieldsLookupTable(Fields: TFields): TPointerDynArray;

{**
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TPointerDynArray;
  Field: TField): Integer;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TPointerDynArray;
  const FieldRefs: TObjectDynArray): TIntegerDynArray;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  out Catalog, Schema, ObjectName: string); overload;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string); overload;

{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetParam(Idex..);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TParam);

const ProcColDbcToDatasetType: array[TZProcedureColumnType] of TParamType =
  (ptUnknown{pctUnknown}, ptInput{pctIn}, ptInputOutput{pctInOut},
   ptOutPut{pctOut}, ptResult{pctReturn}, ptResult{pctResultSet});
const DatasetTypeToProcColDbc: array[TParamType] of TZProcedureColumnType =
  (pctUnknown{ptUnknown}, pctIn{ptInput}, pctOut{ptOutPut},
    pctInOut{ptInputOutput}, pctReturn{ptResult});

function IsSimpleDateTimeFormat(const Format: String): Boolean;
function IsSimpleDateFormat(const Format: String): Boolean;
function IsSimpleTimeFormat(const Format: String): Boolean;
function ConvertAsFractionFormat(const Frmt: String; Scale: Integer;
  ReplaceFractions: Boolean; out FractionLen: Integer): String;
function FindFirstDateFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;
function FindFirstTimeFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;

const
  MilliReplaceQuoted: array[0..9] of String = (
    '""',
    '"'#1'"',
    '"'#1#2'"',
    '"'#1#2#3'"',
    '"'#1#2#3#4'"',
    '"'#1#2#3#4#5'"',
    '"'#1#2#3#4#5#6'"',
    '"'#1#2#3#4#5#6#7'"',
    '"'#1#2#3#4#5#6#7#8'"',
    '"'#1#2#3#4#5#6#7#8#9'"');
  MilliReplaceUnQuoted: array[0..9] of String = (
    '',
    #1,
    #1#2,
    #1#2#3,
    #1#2#3#4,
    #1#2#3#4#5,
    #1#2#3#4#5#6,
    #1#2#3#4#5#6#7,
    #1#2#3#4#5#6#7#8,
    #1#2#3#4#5#6#7#8#9);
  FractionAdjust: array[0..9] of String = (
    '',
    'f',
    'ff',
    'fff',
    'ffff',
    'fffff',
    'ffffff',
    'fffffff',
    'ffffffff',
    'fffffffff');
  {** Common variables. }
var
  CommonTokenizer: IZTokenizer;

implementation

uses
  FmtBCD, Math,
  ZFastCode, ZMessages, ZGenericSqlToken, ZDbcResultSetMetadata, ZAbstractRODataset,
  ZSysUtils, ZDbcResultSet;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;
begin
  case Value of
    stBoolean:
      Result := ftBoolean;
    stByte:
      Result := {$IFDEF WITH_FTBYTE}ftByte{$ELSE}ftWord{$ENDIF}; // ! dangerous - field will get a type with greater size
    stShort:
      Result := {$IFDEF WITH_FTSHORTINT}ftShortint{$ELSE}ftSmallInt{$ENDIF}; // !
    stSmall:
      Result := ftSmallInt;
    stWord:
      Result := ftWord;
    stInteger:
      Result := ftInteger;
    stLongWord:
      Result := {$IFDEF WITH_FTLONGWORD}ftLongWord{$ELSE}ftLargeInt{$ENDIF}; // !
    stLong, stULong:
      Result := ftLargeInt;
    {$IFDEF WITH_FTSINGLE}
    stFloat:
      Result := ftSingle;
    {$ENDIF}
    stBigDecimal:
      Result := ftFmtBCD;
    {$IFNDEF WITH_FTSINGLE}stFloat,{$ENDIF}
    stDouble:
      Result := ftFloat;
    stCurrency:
      //Result := ftCurrency;
      Result := ftBCD;
    stString:
      Result := ftString;
    stBytes{$IFNDEF WITH_FTGUID}, stGUID{$ENDIF}:
      Result := ftBytes;
    {$IFDEF WITH_FTGUID}
    stGUID:
      Result := ftGUID;
    {$ENDIF}
    stDate:
      Result := ftDate;
    stTime:
      Result := ftTime;
    stTimestamp:
      Result := ftDateTime;
    stAsciiStream:
      Result := ftMemo;
    stBinaryStream:
      Result := ftBlob;
    stUnicodeString:
      Result := ftWideString;
    stUnicodeStream:
      Result := {$IFNDEF WITH_WIDEMEMO}ftWideString{$ELSE}ftWideMemo{$ENDIF};
    {$IFDEF WITH_FTDATASETSUPPORT}
    stDataSet:
      Result := ftDataSet;
    {$ENDIF}
    stArray:
      Result := ftArray;
    else
      Result := ftUnknown;
  end;
end;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;
begin
  case Value of
    ftBoolean:
      Result := stBoolean;
    {$IFDEF WITH_FTBYTE}
    ftByte:
      Result := stByte;
    {$ENDIF}
    {$IFDEF WITH_FTSHORTINT}
    ftShortInt:
      Result := stShort;
    {$ENDIF}
    ftWord:
      Result := stWord;
    ftSmallInt:
      Result := stSmall;
    ftInteger, ftAutoInc:
      Result := stInteger;
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord:
      Result := stLongWord;
    {$ENDIF}
    {$IFDEF WITH_FTSINGLE}
    ftSingle:
      Result := stFloat;
    {$ENDIF}
    ftFloat:
      Result := stDouble;
    {$IFDEF WITH_FTEXTENDED}
    ftExtended:
      Result := stBigDecimal;
    {$ENDIF}
    ftLargeInt:
      Result := stLong;
    ftCurrency: Result := stDouble;
    ftBCD:
      Result := stCurrency;
    ftFmtBCD:
      Result := stBigDecimal;
    ftString:
      Result := stString;
    ftBytes, ftVarBytes:
      Result := stBytes;
    ftDate:
      Result := stDate;
    ftTime:
      Result := stTime;
    ftDateTime:
      Result := stTimestamp;
    ftMemo:
      Result := stAsciiStream;
    ftBlob, ftGraphic:
      Result := stBinaryStream;
    ftWideString:
      Result := stUnicodeString;
    {$IFDEF WITH_FTGUID}
    ftGuid:
      Result := stGUID;
    {$ENDIF}
    {$IFDEF WITH_WIDEMEMO}
    ftWideMemo:
      Result := stUnicodeStream;
    {$ENDIF}
    {$IFDEF WITH_FTDATASETSUPPORT}
    ftDataSet:
      Result := stDataSet;
    {$ENDIF}
    ftArray:
      Result := stArray;
    else
      Result := stUnknown;
  end;
end;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): TObjectList;
var
  I: Integer;
  Current: TField;
  ColumnInfo: TZColumnInfo;
begin
  Result := TObjectList.Create(True);
  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.ColumnType := ConvertDatasetToDbcType(Current.DataType);
    ColumnInfo.ColumnName := Current.FieldName;
    ColumnInfo.Precision := Current.Size;
    if Current.DataType in [ftBCD, ftFmtBCD] then
      ColumnInfo.Scale := Current.DataSize;
    ColumnInfo.ColumnLabel := Current.DisplayName;
    ColumnInfo.DefaultExpression := Current.DefaultExpression;
    Result.Add(ColumnInfo);
  end;
end;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(const ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I, FieldIndex: Integer;
  Current: TField;
  ColumnIndex, ColumnCount: Integer;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  G: TGUID absolute BCD;
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
begin
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    if not (Current.FieldKind in [fkData, fkInternalCalc]) then
      Continue;

    ColumnIndex := Current.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    FieldIndex := DefineFieldIndex(FieldsLookupTable, Current);
    if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > ColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
      Continue;

    case Current.DataType of
      ftBoolean:
        RowAccessor.SetBoolean(FieldIndex, ResultSet.GetBoolean(ColumnIndex));
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}ftWord{$IFDEF WITH_FTLONGWORD},ftLongWord{$ENDIF}:
        RowAccessor.SetUInt(FieldIndex, ResultSet.GetUInt(ColumnIndex));
      {$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}ftSmallInt,ftInteger, ftAutoInc:
        RowAccessor.SetInt(FieldIndex, ResultSet.GetInt(ColumnIndex));
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        RowAccessor.SetFloat(FieldIndex, ResultSet.GetFloat(ColumnIndex));
      {$ENDIF}
      ftCurrency, ftFloat:
        RowAccessor.SetDouble(FieldIndex, ResultSet.GetDouble(ColumnIndex));
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        RowAccessor.SetDouble(FieldIndex, ResultSet.GetDouble(ColumnIndex));
      {$ENDIF}
      {$IFDEF WITH_FTGUID}
      ftGUID: begin
          ResultSet.GetGUID(ColumnIndex, G);
          RowAccessor.SetGUID(FieldIndex, G);
        end;
      {$ENDIF}
      ftFmtBCD: begin
          ResultSet.GetBigDecimal(ColumnIndex, BCD);
          RowAccessor.SetBigDecimal(FieldIndex, BCD);
        end;
      ftLargeInt:
        RowAccessor.SetLong(FieldIndex, ResultSet.GetLong(ColumnIndex));
      ftBCD:
        RowAccessor.SetCurrency(FieldIndex, ResultSet.GetCurrency(ColumnIndex));
      ftString, ftWideString:
        if RowAccessor.IsRaw then
          RowAccessor.SetPAnsiChar(FieldIndex, ResultSet.GetPAnsiChar(ColumnIndex, Len), Len)
        else
          RowAccessor.SetPWideChar(FieldIndex, ResultSet.GetPWideChar(ColumnIndex, Len), Len);
      ftBytes, ftVarBytes:
        RowAccessor.SetBytes(FieldIndex, ResultSet.GetBytes(ColumnIndex));
      ftDate: begin
          ResultSet.GetDate(ColumnIndex, D);
          RowAccessor.SetDate(FieldIndex, D);
        end;
      ftTime: begin
          ResultSet.GetTime(ColumnIndex, T);
          RowAccessor.SetTime(FieldIndex, T);
        end;
      ftDateTime: begin
          ResultSet.GetTimestamp(ColumnIndex, TS);
          RowAccessor.SetTimestamp(FieldIndex, TS);
        end;
      ftMemo, ftBlob, ftGraphic {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
        RowAccessor.SetBlob(FieldIndex, ResultSet.GetBlob(ColumnIndex));
      {$IFDEF WITH_FTDATASETSUPPORT}
      ftDataSet:
        RowAccessor.SetDataSet(FieldIndex, ResultSet.GetDataSet(ColumnIndex));
      {$ENDIF}
    end;

    if ResultSet.WasNull then
      RowAccessor.SetNull(FieldIndex);
  end;
end;

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(const ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I, FieldIndex: Integer;
  Current: TField;
  WasNull: Boolean;
  ColumnIndex, ColumnCount: Integer;
  Blob: IZBlob;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  G: TGUID absolute BCD;
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
begin
  WasNull := False;
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    if Current.FieldKind <> fkData then
      Continue;

    ColumnIndex := Current.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    FieldIndex := DefineFieldIndex(FieldsLookupTable, Current);
    if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > ColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
      Continue;

//    if (Current.Required = True) and (WasNull = True) then
//      raise EZDatabaseError.Create(Format(SFieldCanNotBeNull, [Current.FieldName]));
    case Current.DataType of
      ftBoolean:
        ResultSet.UpdateBoolean(ColumnIndex, RowAccessor.GetBoolean(FieldIndex, WasNull));
      {$IFDEF WITH_FTBYTE}
      ftByte:
        ResultSet.UpdateByte(ColumnIndex, RowAccessor.GetByte(FieldIndex, WasNull));
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt:
        ResultSet.UpdateShort(ColumnIndex, RowAccessor.GetShort(FieldIndex, WasNull));
      {$ENDIF}
      ftWord:
        ResultSet.UpdateWord(ColumnIndex, RowAccessor.GetWord(FieldIndex, WasNull));
      ftSmallInt:
        ResultSet.UpdateSmall(ColumnIndex, RowAccessor.GetSmall(FieldIndex, WasNull));
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:
        ResultSet.UpdateUInt(ColumnIndex, RowAccessor.GetUInt(FieldIndex, WasNull));
      {$ENDIF}
      ftInteger, ftAutoInc:
        ResultSet.UpdateInt(ColumnIndex, RowAccessor.GetInt(FieldIndex, WasNull));
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        ResultSet.UpdateFloat(ColumnIndex, RowAccessor.GetFloat(FieldIndex, WasNull));
      {$ENDIF}
      ftFloat:
        ResultSet.UpdateDouble(ColumnIndex, RowAccessor.GetDouble(FieldIndex, WasNull));
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        ResultSet.UpdateDouble(ColumnIndex, RowAccessor.GetDouble(FieldIndex, WasNull));
      {$ENDIF}
      ftFmtBCD: begin
          RowAccessor.GetBigDecimal(ColumnIndex, PBCD(@RowAccessor.TinyBuffer[0])^, WasNull);
          ResultSet.UpdateBigDecimal(FieldIndex, PBCD(@RowAccessor.TinyBuffer[0])^);
        end;
      ftLargeInt:
        ResultSet.UpdateLong(ColumnIndex, RowAccessor.GetLong(FieldIndex, WasNull));
      ftCurrency, ftBCD:
        ResultSet.UpdateCurrency(ColumnIndex,
          RowAccessor.GetCurrency(FieldIndex, WasNull));
      ftString, ftWidestring:
        if RowAccessor.IsRaw then
          ResultSet.UpdatePAnsiChar(ColumnIndex,
            RowAccessor.GetPAnsiChar(FieldIndex, WasNull, Len), Len)
        else
          ResultSet.UpdatePWideChar(ColumnIndex,
            RowAccessor.GetPWideChar(FieldIndex, WasNull, Len), Len);
      {$IFDEF WITH_FTGUID}ftGuid: begin
          RowAccessor.GetGUID(FieldIndex, G, WasNull);
          ResultSet.UpdateGUID(ColumnIndex, G);
        end;
      {$ENDIF}
      ftBytes, ftVarBytes:
        ResultSet.UpdateBytes(ColumnIndex, RowAccessor.GetBytes(FieldIndex, WasNull));
      ftDate: begin
          RowAccessor.GetDate(FieldIndex, WasNull, D);
          ResultSet.UpdateDate(ColumnIndex, D);
        end;
      ftTime: begin
          RowAccessor.GetTime(FieldIndex, WasNull, T);
          ResultSet.UpdateTime(ColumnIndex, T);
        end;
      ftDateTime: begin
          RowAccessor.GetTimestamp(FieldIndex, WasNull, TS);
          ResultSet.UpdateTimestamp(ColumnIndex, TS);
        end;
      {$IFDEF WITH_WIDEMEMO}
      ftWideMemo,
      {$ENDIF}
      ftMemo, ftBlob, ftGraphic:
        begin
          Blob := RowAccessor.GetBlob(FieldIndex, WasNull);
          WasNull := (Blob = nil) or (Blob.IsEmpty); //need a check for IsEmpty too
          ResultSet.UpdateLob(ColumnIndex, Blob);
        end;
      {$IFDEF WITH_FTDATASETSUPPORT}
      ftDataSet: ;
      {$ENDIF}
    end;

    if WasNull then
    begin
      // Performance thing :
      // The default expression will only be set when necessary : if the value really IS null
      Resultset.UpdateDefaultExpression(ColumnIndex, RowAccessor.GetColumnDefaultExpression(FieldIndex));
      ResultSet.UpdateNull(ColumnIndex);
    end;
  end;
end;

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFields(DataSet: TDataset; const FieldNames: string;
  out OnlyDataFields: Boolean; const Tokenizer: IZTokenizer): TObjectDynArray;
var
  I, TokenValueInt: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  Field: TField;
  FieldCount: Integer;
begin
  OnlyDataFields := True;
  FieldCount := 0;
  SetLength(Result, FieldCount);
  Tokens := Tokenizer.TokenizeBufferToList(FieldNames,
    [toSkipEOF, toSkipWhitespaces, toUnifyNumbers]);

  try
    for I := 0 to Tokens.Count - 1 do
    begin
      Token := Tokens[I];
      Field := nil;

      case Token.TokenType of
        ttQuoted, ttQuotedIdentifier, ttWord:
          Field := DataSet.FieldByName(Tokenizer.GetQuoteState.DecodeToken(Token^, Token.P^)); // Will raise exception if field not present
        ttNumber:
          begin
            TokenValueInt := StrToInt(TokenAsString(Token^));
            // Tokenizer always returns numbers > 0
            if TokenValueInt >= Dataset.Fields.Count then
              raise EZDatabaseError.CreateFmt(SFieldNotFound2, [TokenValueInt]);
            Field := Dataset.Fields[TokenValueInt];
          end;
        ttSymbol:
          if not (Tokens.IsEqual(i, Char(',')) or Tokens.IsEqual(i, Char(';'))) then
            raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenAsString(Token^)]);
        else
          raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenAsString(Token^)]);
      end;

      if Field <> nil then
      begin
        OnlyDataFields := OnlyDataFields and (Field.FieldKind = fkData);
        Inc(FieldCount);
        SetLength(Result, FieldCount);
        Result[FieldCount - 1] := Field;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Defins a indices of filter fields.
  @param Dataset a dataset object.
  @param Expression a expression calculator.
  @returns an array with field object references.
}
function DefineFilterFields(DataSet: TDataset;
  const Expression: IZExpression): TObjectDynArray;
var
  I: Integer;
  Current: TField;
begin
  if Expression.Expression <> '' then
  begin
    SetLength(Result, Expression.DefaultVariables.Count);
    for I := 0 to Expression.DefaultVariables.Count - 1 do
    begin
      Current := DataSet.FindField(Expression.DefaultVariables.Names[I]);
      if Current <> nil then
        Result[I] := Current
      else
        Result[I] := nil;
    end;
  end
  else
    SetLength(Result, 0);
end;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param ResultSet an initial result set object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const ResultSet: IZResultSet; const ResultValues: TZVariantDynArray);
var
  I, ColumnIndex: Integer;
begin
  for I := 0 to High(FieldRefs) do
  begin
    ColumnIndex := TField(FieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    if ColumnIndex >= 0 then
    begin
      case TField(FieldRefs[I]).DataType of
        ftString:
          ResultValues[I] := EncodeString(ResultSet.GetString(ColumnIndex));
        ftBoolean:
          ResultValues[I] := EncodeBoolean(ResultSet.GetBoolean(ColumnIndex));
        {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
        ftWord, ftSmallInt, ftInteger, ftAutoInc:
          ResultValues[I] := EncodeInteger(ResultSet.GetInt(ColumnIndex));
        {$IFDEF WITH_FTSINGLE}ftSingle,{$ENDIF}
        {$IFDEF WITH_FTEXTENDED}ftExtended,{$ENDIF}
        ftFloat, ftCurrency: ResultValues[I] := EncodeDouble(ResultSet.GetDouble(ColumnIndex));
        ftBCD: ResultValues[I] := EncodeCurrency(ResultSet.GetCurrency(ColumnIndex));
        ftFmtBCD: begin
                    InitializeVariant(ResultValues[I], vtBigDecimal);
                    ResultSet.GetBigDecimal(ColumnIndex, ResultValues[I].VBigDecimal);
                  end;
        {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
          ResultValues[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
        ftDate, ftTime, ftDateTime:
          ResultValues[I] := EncodeDateTime(ResultSet.GetTimestamp(ColumnIndex));
        ftWidestring{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}:
          ResultValues[I] := EncodeUnicodeString(ResultSet.GetUnicodeString(ColumnIndex));
        ftBytes, ftVarBytes, ftBlob, ftGraphic:
          ResultValues[I] := EncodeBytes(ResultSet.GetBytes(ColumnIndex));
        {$IFDEF WITH_FTGUID}
        ftGUID: begin
                  InitializeVariant(ResultValues[I], vtGUID);
                  ResultSet.GetGUID(ColumnIndex, ResultValues[I].VGUID);
                end;
        {$ENDIF}
        else
          ResultValues[I] := EncodeString(ResultSet.GetString(ColumnIndex));
      end;
      if ResultSet.WasNull then
        ResultValues[I] := NullVariant;
    end
    else
      ResultValues[I] := NullVariant;
  end;
end;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromRowAccessor(const FieldRefs: TObjectDynArray;
  const FieldIndices: TIntegerDynArray; RowAccessor: TZRowAccessor;
  const ResultValues: TZVariantDynArray);
var
  I: Integer;
  ColumnIndex: Integer;
  WasNull: Boolean;
begin
  WasNull := False;
  for I := 0 to High(FieldRefs) do
  begin
    ColumnIndex := FieldIndices[I];
    case TField(FieldRefs[I]).DataType of
      ftString, ftMemo:
        ResultValues[I] := EncodeString(RowAccessor.GetString(ColumnIndex, WasNull));
      ftBoolean:
        ResultValues[I] := EncodeBoolean(RowAccessor.GetBoolean(ColumnIndex, WasNull));
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftWord, ftSmallInt, ftInteger, ftAutoInc:
        ResultValues[I] := EncodeInteger(RowAccessor.GetInt(ColumnIndex, WasNull));
        {$IFDEF WITH_FTSINGLE}ftSingle,{$ENDIF}
        {$IFDEF WITH_FTEXTENDED}ftExtended,{$ENDIF}
        ftFloat, ftCurrency: ResultValues[I] := EncodeDouble(RowAccessor.GetDouble(ColumnIndex, WasNull));
        ftBCD: ResultValues[I] := EncodeCurrency(RowAccessor.GetCurrency(ColumnIndex, WasNull));
        ftFmtBCD: begin
                    InitializeVariant(ResultValues[I], vtBigDecimal);
                    RowAccessor.GetBigDecimal(ColumnIndex, ResultValues[I].VBigDecimal, WasNull);
                  end;
      {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
        ResultValues[I] := EncodeInteger(RowAccessor.GetLong(ColumnIndex, WasNull));
      ftDate, ftTime, ftDateTime:
        ResultValues[I] := EncodeDateTime(RowAccessor.GetDouble(ColumnIndex, WasNull));
      ftWidestring{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}:
        ResultValues[I] := EncodeUnicodeString(RowAccessor.GetUnicodeString(ColumnIndex, WasNull));
      ftBytes, ftVarBytes:
        ResultValues[I] := EncodeBytes(RowAccessor.GetBytes(ColumnIndex, WasNull));
      else
        ResultValues[I] := EncodeString(RowAccessor.GetString(ColumnIndex, WasNull));
    end;
    if WasNull then
      ResultValues[I] := NullVariant;
  end;
end;

{**
  Copy a set of specified field values to variables.
  @param Fields an array with interested field object references.
  @param ResultSet an initial result set object.
  @param Variables a list of variables.
}
procedure CopyDataFieldsToVars(const Fields: TObjectDynArray;
  const ResultSet: IZResultSet; const Variables: IZVariablesList);
var
  I, ColumnIndex: Integer;
  TinyBuffer: array[Byte] of Byte;
  procedure CopyFromField;
  begin
    if TField(Fields[I]).IsNull then
      Variables.Values[I] := NullVariant
    else case TField(Fields[I]).DataType of
      ftBoolean:
        Variables.Values[I] := EncodeBoolean(TField(Fields[I]).AsBoolean);
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftWord, ftSmallInt, ftInteger, ftAutoInc:
        Variables.Values[I] := EncodeInteger(TField(Fields[I]).AsInteger);
      {$IFDEF WITH_FTSINGLE}
      ftSingle,
      {$ENDIF}
      ftCurrency,
      ftFloat:
        Variables.Values[I] := EncodeDouble(TField(Fields[I]).AsFloat);
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        Variables.Values[I] := EncodeDouble(TField(Fields[I]).AsExtended);
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
        Variables.Values[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
      ftBCD:
        Variables.Values[I] := EncodeCurrency(ResultSet.GetCurrency(ColumnIndex));
      ftFmtBCD: Variables.Values[I] := EncodeBigDecimal(TField(Fields[I]).AsBCD);
      ftDate, ftTime, ftDateTime:
        Variables.Values[I] := EncodeDateTime(TField(Fields[I]).AsDateTime);
      //ftString, ftMemo:
        //Variables.Values[I] := EncodeString(TField(Fields[I]).AsString);
    {$IFNDEF UNICODE}
      {$IFDEF WITH_FTWIDESTRING}
      ftWidestring{$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
        Variables.Values[I] := EncodeUnicodeString(TField(Fields[I]).AsWideString);
      {$ENDIF}
    {$ENDIF}
      ftBytes, ftVarBytes:
        {$IFDEF TFIELD_HAS_ASBYTES}
        Variables.Values[I] := EncodeBytes(TField(Fields[I]).AsBytes);
        {$ELSE}
        Variables.Values[I] := EncodeBytes(VarToBytes(TField(Fields[I]).AsVariant));
        {$ENDIF}
      ftArray, ftDataSet: raise EZDatabaseError.Create(SDataTypeDoesNotSupported)
      else Variables.Values[I] := EncodeString(TField(Fields[I]).AsString);
    end;
  end;
begin
  for I := 0 to High(Fields) do begin
    if Fields[I] = nil then
      Continue;

    ColumnIndex := TField(Fields[I]).FieldNo {$IFDEF GENERIC_INDEX}-1{$ENDIF};
    if ColumnIndex = -1 then
      CopyFromField
    else if not ResultSet.IsNull(ColumnIndex) then
      case TField(Fields[I]).DataType of
        ftBoolean:
          Variables.Values[I] := EncodeBoolean(ResultSet.GetBoolean(ColumnIndex));
        {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
        ftWord, ftSmallInt, ftInteger, ftAutoInc:
          Variables.Values[I] := EncodeInteger(ResultSet.GetInt(ColumnIndex));
        {$IFDEF WITH_FTSINGLE}
        ftSingle,
        {$ENDIF}
        ftCurrency, ftFloat:
          Variables.Values[I] := EncodeDouble(ResultSet.GetDouble(ColumnIndex));
        {$IFDEF WITH_FTEXTENDED}
        ftExtended:
          Variables.Values[I] := EncodeDouble(ResultSet.GetDouble(ColumnIndex));
        {$ENDIF}
        ftFmtBCD: begin
                    ResultSet.GetBigDecimal(ColumnIndex, PBCD(@TinyBuffer[0])^);
                    Variables.Values[I] := EncodeBigDecimal(PBCD(@TinyBuffer[0])^);
                  end;
        {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
          Variables.Values[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
        ftBCD:
          Variables.Values[I] := EncodeCurrency(ResultSet.GetCurrency(ColumnIndex));
        ftDate:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetDate(ColumnIndex));
        ftTime:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetTime(ColumnIndex));
        ftDateTime:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetTimestamp(ColumnIndex));
        //ftString, ftMemo:
          //Variables.Values[I] := EncodeString(ResultSet.GetString(ColumnIndex));
        {$IFNDEF UNICODE}
        ftWidestring{$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
          Variables.Values[I] := EncodeUnicodeString(ResultSet.GetUnicodeString(ColumnIndex));
        {$ENDIF}
        ftBytes, ftVarBytes:
          Variables.Values[I] := EncodeBytes(ResultSet.GetBytes(ColumnIndex));
        ftArray,
        ftDataSet: raise EZDatabaseError.Create(SDataTypeDoesNotSupported);
        else
          Variables.Values[I] := EncodeString(ResultSet.GetString(ColumnIndex));
      end
    else
      Variables.Values[I] := NullVariant;
  end;
end;

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareDataFields(const KeyValues, RowValues: TZVariantDynArray;
  Const VariantManager: IZVariantManager; PartialKey, CaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  {$IFNDEF NEXTGEN}
  Value1, Value2: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  {$ENDIF}
  WValue1, WValue2: ZWideString;
begin
  Result := True;
  for I := 0 to High(KeyValues) do begin
    {$IFNDEF NEXTGEN}
    case KeyValues[I].VType of
      vtUnicodeString{$IFDEF UNICODE}, vtString{$ENDIF}:
        begin
    {$ENDIF}
          WValue1 := VariantManager.GetAsUnicodeString(KeyValues[I]);
          WValue2 := VariantManager.GetAsUnicodeString(RowValues[I]);
          if CaseInsensitive then begin
            if PartialKey then begin
              {$IFDEF MSWINDOWS}
                Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                  PWideChar(WValue2), Length(WValue1), PWideChar(WValue1), Length(WValue1)) - 2{CSTR_EQUAL} = 0;
              {$ELSE}
                {$IFDEF UNICODE}
                Result := SysUtils.AnsiStrLComp(PWideChar(WValue2), PWideChar(WValue1), Length(WValue1)) = 0;
                {$ELSE}
                Value1 := AnsiString(WValue1);
                Value2 := AnsiString(WValue2);
                Result := AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0;
                {$ENDIF}
              {$ENDIF}
            end else
              Result := WValue1 = WValue2
          end else if PartialKey then begin
            {$IFDEF MSWINDOWS}
              Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                PWideChar(WValue2), Length(WValue1), PWideChar(WValue1), Length(WValue1)) - 2{CSTR_EQUAL} = 0;
            {$ELSE}
              {$IFDEF UNICODE}
              Result := SysUtils.AnsiStrLComp(PWideChar(WValue2), PWideChar(WValue1), Length(WValue1)) = 0;
              {$ELSE}
              Value1 := AnsiString(WValue1);
              Value2 := AnsiString(WValue2);
              Result := AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0;
              {$ENDIF}
            {$ENDIF}
          end else
            Result := VariantManager.Compare(KeyValues[I], RowValues[I]) = 0;
    {$IFNDEF NEXTGEN}
        end;
      else
      begin
        {$IFDEF NO_ANSISTRING}
        Value1 := VariantManager.GetAsRawByteString(KeyValues[I]);
        Value2 := VariantManager.GetAsRawByteString(RowValues[I]);
        {$ELSE}
        Value1 := VariantManager.GetAsAnsiString(KeyValues[I]);
        Value2 := VariantManager.GetAsAnsiString(RowValues[I]);
        {$ENDIF}
        if CaseInsensitive then begin
          Value1 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(Value1);
          Value2 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(Value2);
          if PartialKey
          then Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0
          else Result := Value1 = Value2
        end else begin
          if PartialKey
          then Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0
          else Result := VariantManager.Compare(KeyValues[I], RowValues[I]) = 0;
        end;
      end;
    end;
    {$ENDIF}
    if not Result then
      Break;
  end;
end;

{**
  Prepares values for comparison by CompareFieldsFromResultSet.
  @param FieldRefs an array with interested field object references.
  @param DecodedKeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
}
procedure PrepareValuesForComparison(const FieldRefs: TObjectDynArray;
  const DecodedKeyValues: TZVariantDynArray; const ResultSet: IZResultSet;
  PartialKey: Boolean; CaseInsensitive: Boolean;
  const VariantManager: IZClientVariantManager);
var
  I: Integer;
  Current: TField;
  CurrentType : TZSQLType;
begin
  { Preprocesses cycle variables. }
  for I := 0 to High(FieldRefs) do
  begin
    Current := TField(FieldRefs[I]);

    if DecodedKeyValues[I].VType = vtNull then
      Continue;
    CurrentType := ResultSet.GetMetadata.GetColumnType(Current.FieldNo{$IFDEF GENERIC_INDEX} -1{$ENDIF});

    if PartialKey then begin
      {$IFNDEF NEXTGEN}
      if (CurrentType in [stUnicodeString, stUnicodeStream]) or
         ((CurrentType in [stString, stAsciiStream]) and (VariantManager.UseWComparsions)) then begin
      {$ENDIF NEXTGEN}
        DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtUnicodeString);
        if CaseInsensitive then
          DecodedKeyValues[I].VUnicodeString :=
              {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(DecodedKeyValues[I].VUnicodeString);
      {$IFNDEF NEXTGEN}
      end else begin
        DecodedKeyValues[I] := SoftVarManager.Convert(DecodedKeyValues[I], vtAnsiString);
        if CaseInsensitive then
          DecodedKeyValues[I].VRawByteString := AnsiUpperCase(DecodedKeyValues[I].VRawByteString);
      end;
      {$ENDIF NEXTGEN}
    end else
      case CurrentType of
        stBoolean:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtBoolean);
        stByte, stWord, stLongWord, stULong:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtUInteger);
        stShort, stSmall, stInteger, stLong:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtInteger);
        stFloat, stDouble:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtDouble);
        stCurrency:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtCurrency);
        stBigDecimal:
          DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtBigDecimal);
        stUnicodeString, stUnicodeStream:
          begin
            if DecodedKeyValues[I].VType <> vtUnicodeString then
              DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtUnicodeString);
            if CaseInsensitive then
              DecodedKeyValues[I].VUnicodeString :=
                {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(DecodedKeyValues[I].VUnicodeString);
          end;
        stGUID: DecodedKeyValues[I] := VariantManager.Convert(
            DecodedKeyValues[I], vtGUID);
        stDate, stTime, stTimestamp:
          DecodedKeyValues[I] := VariantManager.Convert(
            DecodedKeyValues[I], vtDateTime);
        else {$IFNDEF NEXTGEN} if (CurrentType in [stString, stAsciiStream]) then
          if VariantManager.UseWComparsions then {$ENDIF NEXTGEN}begin
            DecodedKeyValues[I] := VariantManager.Convert(DecodedKeyValues[I], vtUnicodeString);
            if CaseInsensitive then
              DecodedKeyValues[I].VUnicodeString :=
                {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(DecodedKeyValues[I].VUnicodeString);
          end {$IFNDEF NEXTGEN} else begin
            DecodedKeyValues[I] := VariantManager.Convert(
              DecodedKeyValues[I], vtRawByteString);
            if CaseInsensitive then
              DecodedKeyValues[I].VRawByteString :=
                {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(DecodedKeyValues[I].VRawByteString);
        end else
          DecodedKeyValues[I] := VariantManager.Convert(
              DecodedKeyValues[I], vtRawByteString);
        {$ENDIF NEXTGEN}
    end;
  end;
end;

{**
  Compares row field values with the given ones.
  @param FieldRefs an array with interested field object references.
  @param KeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const KeyValues: TZVariantDynArray; const ResultSet: IZResultSet; PartialKey: Boolean;
  CaseInsensitive: Boolean; const VariantManager: IZClientVariantManager): Boolean;
var
  I: Integer;
  ColumnIndex: Integer;
  {$IFNDEF NEXTGEN}
  AValue1, AValue2: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF};
  {$ENDIF}
  WValue1, WValue2: ZWideString;
  CurrentType : TZSQLType;
  BCD: TBCD;
  UID: TGUID absolute BCD;
  U64: UInt64 absolute BCD;
  I64: Int64 absolute BCD;
  C: Currency absolute i64;
  D: Double absolute C;
  DT: TDateTime absolute D;
  B: Boolean absolute C;
  P1, P2: Pointer;
  L1, L2: LengthInt;
begin
  Result := True;
  for I := 0 to High(KeyValues) do
  begin
    ColumnIndex := TField(FieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};

    if KeyValues[I].VType = vtNull then begin
      Result := ResultSet.IsNull(ColumnIndex);
      if not Result then
         Break;
      Continue;
    end;

    CurrentType := ResultSet.GetMetadata.GetColumnType(ColumnIndex);

    if PartialKey then begin
      {$IFNDEF NEXTGEN}
      if (CurrentType in [stUnicodeString, stUnicodeStream]) or
         ((CurrentType in [stString, stAsciiStream]) and (VariantManager.UseWComparsions)) then begin
      {$ENDIF}
        {$IFDEF NEXGEN}
        WValue1 := VariantManager.GetAsUnicodeString(KeyValues[I]);
        {$ELSE}
        WValue1 := KeyValues[I].VUnicodeString;
        {$ENDIF}
        WValue2 := ResultSet.GetUnicodeString(ColumnIndex);
        if CaseInsensitive then
          WValue2 := {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(WValue2);

        P1 := Pointer(WValue1);
        if P1 = nil then //if partial value is '' then the evaluatin is always true
          Exit;
        {$IF not (defined(FPC) and not defined(MSWINDOWS))}
        P2 := Pointer(WValue2);
        {$IFEND}
        L1 := Length(WValue1);
        L2 := Length(WValue2);
        if L2 < L1 then begin //if resultset value is shorter than keyvalue the evaluatin is always false
          Result := False;
          Exit;
        end;
        {$IFDEF MSWINDOWS}
        Result := CompareStringW(LOCALE_USER_DEFAULT, 0, P2, L1, P1, L1) = {$IFDEF FPC}2{$ELSE}CSTR_EQUAL{$ENDIF};
        {$ELSE}
          {$IFDEF UNICODE}
          Result := SysUtils.AnsiStrLComp(P2, P1, L1) = 0;
          {$ELSE} //https://www.freepascal.org/docs-html/rtl/sysutils/widecomparestr.html
            if L2 > L1 then
              WValue2 := Copy(WValue2, 1, L2);
            Result := WideCompareStr(WValue1, WValue2) = 0;
          {$ENDIF}
        {$ENDIF}
      {$IFNDEF NEXTGEN}
      end else begin
        AValue1 := KeyValues[I].VRawByteString;
        AValue2 := ResultSet.GetAnsiString(ColumnIndex);
        if CaseInsensitive then
          AValue2 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(AValue2);
        L1 := Length(AValue1);
        L2 := Length(AValue2);
        if L2 < L1 then
          Exit;
        P1 := Pointer(AValue1);
        P2 := Pointer(AValue1);
        Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(P2), PAnsiChar(P1), L1) = 0;
      end;
      {$ENDIF}
    end else
      case CurrentType of
        stBoolean: begin
              B := ResultSet.GetBoolean(ColumnIndex);
              Result := KeyValues[I].VBoolean = B;
            end;
        stByte, stWord, stLongWord, stUlong: begin
              U64 := ResultSet.GetULong(ColumnIndex);
              Result := KeyValues[I].VUInteger = U64;
            end;
        stShort, stSmall, stInteger, stLong: begin
              I64 := ResultSet.GetLong(ColumnIndex);
              Result := KeyValues[I].VInteger = I64;
            end;
        stFloat:  begin
              D := ResultSet.GetDouble(ColumnIndex);
              Result := Abs(KeyValues[I].VDouble - D) < FLOAT_COMPARE_PRECISION_SINGLE;
            end;
        stDouble: begin
              D := ResultSet.GetDouble(ColumnIndex);
              Result := Abs(KeyValues[I].VDouble - D) < FLOAT_COMPARE_PRECISION;
            end;
        stCurrency: begin
              C := ResultSet.GetCurrency(ColumnIndex);
              Result := KeyValues[I].VCurrency = C;
            end;
        stBigDecimal: begin
                        ResultSet.GetBigDecimal(ColumnIndex, BCD);
                        Result := BCDCompare(KeyValues[I].VBigDecimal, BCD) = 0;
                      end;
        stDate,
        stTime,
        stTimestamp:  begin
            DT := ResultSet.GetTimestamp(ColumnIndex);
            Result := ZCompareDateTime(KeyValues[I].VDateTime, DT) = 0;;
          end;
        stGUID: begin
                  ResultSet.GetGUID(ColumnIndex, UID);
                  Result := CompareMem(@KeyValues[I].VGUID.D1, @UID.D1, SizeOf(TGUID));
                end
        else {$IFNDEF NEXTGEN}if (CurrentType in [stUnicodeString, stUnicodeStream]) or
             ((CurrentType in [stString, stAsciiStream]) and (VariantManager.UseWComparsions)) then {$ENDIF NEXTGEN} begin
            WValue2 := ResultSet.GetUnicodeString(ColumnIndex);
            if CaseInsensitive then
              WValue2 := {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(WValue2);
            Result := KeyValues[I].VUnicodeString = WValue2;
        {$IFNDEF NEXTGEN}
          end else begin
            AValue2 := ResultSet.GetAnsiString(ColumnIndex);
            if CaseInsensitive then
              AValue2 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(AValue2);
            Result := KeyValues[I].VRawByteString = AValue2;
        {$ENDIF !NEXTGEN}
          end;
      end;

    Result := Result and not ResultSet.WasNull;
    if not Result then
      Break;
  end;
end;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields; const IdConverter: IZIdentifierConvertor): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Fields.Count - 1 do
  begin
    if (Fields[I].FieldKind = fkData)
      and not (Fields[I].DataType in [ftBlob, ftGraphic, ftMemo, ftBytes, ftVarBytes {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}]) then
      AppendSepString(Result, IdConverter.Quote(Fields[I].FieldName), ',');
  end;
end;

{**
  Compare values from two key fields.
  @param Field1 the first field object.
  @param ResultSet the resultset to read the first field value.
  @param Field2 the second field object.
}
function CompareKeyFields(Field1: TField; const ResultSet: IZResultSet;
  Field2: TField): Boolean;
var
  ColumnIndex: Integer;
  BCD: TBCD;
begin
  Result := False;
  if Field1.FieldNo >= 1 then
  begin
    ColumnIndex := Field1.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    case Field1.DataType of
      ftBoolean:
        Result := ResultSet.GetBoolean(ColumnIndex) = Field2.AsBoolean;
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}
      {$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftSmallInt, ftInteger, ftAutoInc:
        Result := ResultSet.GetInt(ColumnIndex) = Field2.AsInteger;
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        Result := Abs(ResultSet.GetFloat(ColumnIndex)
          - Field2.AsSingle) < FLOAT_COMPARE_PRECISION_SINGLE;
      {$ENDIF}
      ftFloat:
        begin
          Result := Abs(ResultSet.GetDouble(ColumnIndex)
            - Field2.AsFloat) < FLOAT_COMPARE_PRECISION;
        end;
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
          Result := Abs(ResultSet.GetDouble(ColumnIndex)
            - Field2.AsExtended) < FLOAT_COMPARE_PRECISION;
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}
      ftLongword:
        Result := ResultSet.GetULong(ColumnIndex)
          = Field2.{$IFDEF TFIELD_HAS_ASLARGEINT}AsLargeInt{$ELSE}AsInteger{$ENDIF};
      {$ENDIF}
      ftLargeInt:
          if Field2 is TLargeIntField
          then Result := ResultSet.GetLong(ColumnIndex) = TLargeIntField(Field2).AsLargeInt
          else Result := ResultSet.GetInt(ColumnIndex) = Field2.AsInteger;
      ftFmtBCD: begin
          ResultSet.GetBigDecimal(ColumnIndex, BCD);
          Result := BCDCompare(Field2.AsBCD, BCD) = 0;
        end;
      ftBCD: Result := ResultSet.GetCurrency(ColumnIndex) = TBCDField(Field2).AsCurrency;
      ftCurrency: Result := (ResultSet.GetDouble(ColumnIndex) - Field2.AsFloat) < FLOAT_COMPARE_PRECISION;
      ftDate: Result := ResultSet.GetDate(ColumnIndex) = Field2.AsDateTime;
      ftTime: Result := ResultSet.GetTime(ColumnIndex) = Field2.AsDateTime;
      ftDateTime: Result := ResultSet.GetTimestamp(ColumnIndex) = Field2.AsDateTime;
      ftWideString:
        Result := ResultSet.GetUnicodeString(ColumnIndex) =
          Field2.{$IFDEF WITH_ASVARIANT}AsVariant{$ELSE}AsWideString{$ENDIF};
      else //includes ftGUID
        Result := ResultSet.GetString(ColumnIndex) = Field2.AsString;
    end;
  end;
end;

{**
  Defins a indices and directions for sorted fields.
  @param Dataset a dataset object.
  @param SortedFields an encoded fields for sorting in the format
    <Field Name> [ASC | DESC] [, ...]
  @param FieldRefs a decoded field object references.
  @param FieldDirs a decoded field directions.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
procedure DefineSortedFields(DataSet: TDataset;
  const SortedFields: string; out FieldRefs: TObjectDynArray;
  out CompareKinds: TComparisonKindArray; out OnlyDataFields: Boolean);
var
  I, TokenValueInt: Integer;
  Tokens: TZTokenList;
  Field: TField;
  FieldCount: Integer;
  PrevTokenWasField: Boolean;
begin
  OnlyDataFields := True;
  FieldCount := 0;
  PrevTokenWasField := False;
  SetLength(FieldRefs, FieldCount);
  SetLength(CompareKinds, FieldCount);
  Tokens := CommonTokenizer.TokenizeBufferToList(SortedFields,
    [toSkipEOF, toSkipWhitespaces, toUnifyNumbers]);

  try
    for I := 0 to Tokens.Count - 1 do
    begin
      Field := nil;

      case Tokens[i].TokenType of
        ttQuoted, ttQuotedIdentifier, ttWord:
          begin
            // Check if current token is a sort order marker
            // Note that ASC/DESC are valid field identifiers! So we must check
            // if previous token was a field and then set sort order
            // Otherwise - add current token as a field ("Field1 desc, Asc, Field2 desc")

            // Could this be a sort order marker?
            if PrevTokenWasField then
            begin
              if Tokens.IsEqual(i, 'DESC', tcInsensitive) then
                CompareKinds[FieldCount - 1] := ckDescending
              else if Tokens.IsEqual(i, 'ASC', tcInsensitive) then
                CompareKinds[FieldCount - 1] := ckAscending
              else
                raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [Tokens.AsString(I)]);
            end
            else
            // No, this is a field
              Field := DataSet.FieldByName(CommonTokenizer.GetQuoteState.DecodeToken(Tokens[i]^, Tokens[i].P^));  // Will raise exception if field not present
          end;
        ttNumber:
          begin
            TokenValueInt := StrToInt(Tokens.AsString(I));
            // Tokenizer always returns numbers > 0
            if TokenValueInt >= Dataset.Fields.Count then
              raise EZDatabaseError.CreateFmt(SFieldNotFound2, [TokenValueInt]);
            Field := Dataset.Fields[TokenValueInt];
          end;
        ttSymbol:
          if not (Tokens.IsEqual(i, Char(',')) or Tokens.IsEqual(i, Char(';'))) then
            raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [Tokens.AsString(I)]);
        else
          raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [Tokens.AsString(I)]);
      end;

      PrevTokenWasField := (Field <> nil);
      if Field <> nil then
      begin
        OnlyDataFields := OnlyDataFields and (Field.FieldKind = fkData);
        Inc(FieldCount);
        SetLength(FieldRefs, FieldCount);
        SetLength(CompareKinds, FieldCount);
        FieldRefs[FieldCount - 1] := Field;
        CompareKinds[FieldCount - 1] := ckAscending;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Creates a fields lookup table to define fixed position
  of the field in dataset.
  @param Fields a collection of TDataset fields in initial order.
  @returns a fields lookup table.
}
type
  THackZField = Class(TZField); //access protected property
function CreateFieldsLookupTable(Fields: TFields): TPointerDynArray;
var
  I: Integer;
begin
  SetLength(Result, Fields.Count);
  for I := 0 to Fields.Count - 1 do
  begin
    Result[I] := Fields[I];
    if Fields[i] is TZField then
      THackZField(Fields[i]).FieldIndex := I+1;
  end;
end;

{**
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TPointerDynArray;
  Field: TField): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FieldsLookupTable) do
    if FieldsLookupTable[I] = Field then
    begin
      Result := I{$IFNDEF GENERIC_INDEX}+1{$ENDIF};
      Break;
    end;
end;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TPointerDynArray;
  const FieldRefs: TObjectDynArray): TIntegerDynArray;
var
  I: Integer;
begin
  if FieldRefs = nil then
  begin
    Result := nil;
    Exit;
  end;

  SetLength(Result, Length(FieldRefs));
  for I := 0 to High(Result) do
    Result[I] := DefineFieldIndex(FieldsLookupTable, TField(FieldRefs[I]));
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  out Catalog, Schema, ObjectName: string);

{$IFDEF OLDFPC}
function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings): Integer;
var
  Head, Tail: PChar;
  EOS, InQuote: Boolean;
  QuoteChar: Char;
  Item: string;
begin
  Result := 0;
  if (Content = nil) or (Content^ = #0) or (Strings = nil) then
     Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  Strings.BeginUpdate;
  try
    repeat
      while CharInSet(Tail^, WhiteSpace + [#13, #10]) do
        Inc(Tail);
      Head := Tail;
      while True do
      begin
        while (InQuote and not CharInSet(Tail^, [QuoteChar, #0])) or
               not CharInSet(Tail^, Separators + [#0, #13, #10, '''', '"']) do
           Inc(Tail);
        if CharInSet(Tail^, ['''', '"']) then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Tail^) then
            QuoteChar := #0
          else
            QuoteChar := Tail^;
          InQuote := QuoteChar <> #0;
          Inc(Tail);
        end
        else
          Break;
      end;
      EOS := Tail^ = #0;
      if (Head <> Tail) and (Head^ <> #0) then
      begin
        if Strings <> nil then
        begin
          SetString(Item, Head, Tail - Head);
          Strings.Add(Item);
        end;
        Inc(Result);
      end;
      Inc(Tail);
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;
{$ENDIF}

var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    Catalog := '';
    Schema := '';
    ObjectName := QualifiedName;
    ExtractStrings(['.'], [' '], PChar(QualifiedName), SL);
    case SL.Count of
      0, 1: ;
      2: begin
           Schema := SL.Strings[0];
           ObjectName := SL.Strings[1];
         end;
      3: begin
           Catalog := SL.Strings[0];
           Schema := SL.Strings[1];
           ObjectName := SL.Strings[2];
         end;
    else
      begin
        ObjectName := SL.Strings[SL.Count - 1];
        Schema := SL.Strings[SL.Count - 2];
        for I := 0 to SL.Count - 3 do
        begin
          Catalog := Catalog + SL.Strings[I];
          if I < SL.Count - 3 then
            Catalog := Catalog + '.';
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);
var
  SL: TStringList;
  I: Integer;
begin
  if SupportsCatalogs and SupportsSchemas then
    SplitQualifiedObjectName(QualifiedName, Catalog, Schema, ObjectName)
  else
  begin
    SL := TStringList.Create;
    try
      Catalog := '';
      Schema := '';
      ObjectName := QualifiedName;
      ExtractStrings(['.'], [' '], PChar(QualifiedName), SL);
      case SL.Count of
        0, 1: ;
        2:
          begin
            if SupportsCatalogs then
            begin
              Catalog := SL.Strings[0];
              if SupportsSchemas then
                Schema := SL.Strings[1]
              else
                ObjectName := SL.Strings[1];
            end
            else
              if SupportsSchemas then
              begin
                Schema := SL.Strings[0];
                ObjectName := SL.Strings[1];
              end
              else
                ObjectName := SL.Strings[0]+'.'+SL.Strings[1];
          end;
        3:
          if SupportsCatalogs then
          begin
            Catalog := SL.Strings[0];
            if SupportsSchemas then
            begin
              Schema := SL.Strings[1];
              ObjectName := SL.Strings[2]
            end
            else
              ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
          end
          else
            if SupportsSchemas then
            begin
              Schema := SL.Strings[0];
              ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
            end
            else
              ObjectName := SL.Strings[0]+'.'+SL.Strings[1]+'.'+SL.Strings[2];
        else
          if SupportsCatalogs then
          begin
            Catalog := SL.Strings[0];
            if SupportsSchemas then
            begin
              Schema := SL.Strings[1];
              for i := 2 to SL.Count-1 do
                if i = 2 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end
            else
            begin
              ObjectName := '';
              for i := 2 to SL.Count-1 do
                if I = 2 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end;
          end
          else
            if SupportsSchemas then
            begin
              Schema := SL.Strings[0];
              for i := 1 to SL.Count-1 do
                if i = 1 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end
            else
              for i := 0 to SL.Count-1 do
                if I = 0 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
        end;
    finally
      SL.Free;
    end;
  end;
end;

{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetXxxx(ColumnIndex, xxx);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TParam);
var
  Stream: TStream;
  TempBytes: TBytes;
  BlobData: TBlobData;
  {$IFDEF WITH_WIDEMEMO}P: Pointer;
  UniTemp: ZWideString;
  {$ENDIF}
begin
  if Param.IsNull then
    Statement.SetNull(Index, ConvertDatasetToDbcType(Param.DataType))
  else
  begin
    case Param.DataType of
      ftBoolean:
        Statement.SetBoolean(Index, Param.AsBoolean);
      {$IFDEF WITH_FTBYTE}
      ftByte:
        Statement.SetByte(Index, Param.AsByte);
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt:
        Statement.SetShort(Index, Param.AsShortInt);
      {$ENDIF}
      ftWord:
        Statement.SetWord(Index, Param.AsWord);
      ftSmallInt:
        Statement.SetSmall(Index, Param.AsSmallInt);
      ftInteger, ftAutoInc:
        Statement.SetInt(Index, Param.AsInteger);
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        Statement.SetFloat(Index, Param.AsSingle);
      {$ENDIF}
      ftFloat:
        Statement.SetDouble(Index, Param.AsFloat);
      ftFmtBCD:
        Statement.SetBigDecimal(Index, Param.AsFMTBCD);
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        Statement.SetDouble(Index, Param.AsFloat);
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:
        Statement.SetInt(Index, Integer(Param.AsLongWord));
      {$ENDIF}
      ftLargeInt:
        Statement.SetLong(Index, {$IFDEF WITH_PARAM_ASLARGEINT}Param.AsLargeInt{$ELSE}StrToInt64(Param.AsString){$ENDIF});
      ftCurrency, ftBCD:
        Statement.SetCurrency(Index, Param.AsCurrency);
      ftString, ftFixedChar:
        Statement.SetString(Index, Param.AsString);
      {$IFDEF WITH_FTWIDESTRING}
      ftWideString:
        Statement.SetUnicodeString(Index, Param.AsWideString);
      {$ENDIF}
      ftBytes, ftVarBytes:
        begin
          {$IFDEF TPARAM_HAS_ASBYTES}
          Statement.SetBytes(Index, Param.AsBytes);
          {$ELSE}
          Statement.SetBytes(Index, VarToBytes(Param.Value));
          {$ENDIF}
        end;
      {$IFDEF WITH_FTGUID}
      // As of now (on Delphi 10.2) TParam has no support of ftGuid data type.
      // GetData and GetDataSize will raise exception on unsupported data types.
      // But user can assign data type manually and as long as he doesn't call
      // these methods things will be fine.
      // Here we presume the data is stored as TBytes.
      ftGuid:
        begin
          {$IFDEF TPARAM_HAS_ASBYTES}
          TempBytes := Param.AsBytes;
          {$ELSE}
          TempBytes := VarToBytes(Param.Value);
          {$ENDIF}
          Statement.SetGuid(Index, PGUID(TempBytes)^);
        end;
      {$ENDIF}
      ftDate:
        Statement.SetDate(Index, Param.AsDate);
      ftTime:
        Statement.SetTime(Index, Param.AsTime);
      ftDateTime:
        Statement.SetTimestamp(Index, Param.AsDateTime);
      ftMemo:
        begin
          {EgonHugeist: On reading a Param as Memo the Stream reads Byte-wise
            on Changing to stUnicodeString/Delphi12Up a String is from
            Type wide/unicode so we have to give him back as
            Stream!}
            {$IFDEF UNICODE}
            Stream := Param.AsStream;
            {$ELSE}
            Stream := TStringStream.Create(Param.AsMemo);
            {$ENDIF}
          try
            Statement.SetAsciiStream(Index, Stream);
          finally
            Stream.Free;
          end;
        end;
      {$IFDEF WITH_WIDEMEMO}
      ftWideMemo:
        begin
          UniTemp := Param.AsWideString;
          P :=  Pointer(UniTemp);
          if P = nil then
            P := PEmptyUnicodeString;
          Statement.SetBlob(Index, stUnicodeStream, TZAbstractClob.CreateWithData(PWideChar(P), Length(UniTemp), Statement.GetConnection.GetConSettings));
        end;
      {$ENDIF}
      ftBlob, ftGraphic:
        begin
          BlobData := Param.AsBlob;
          Statement.SetBlob(Index, stBinaryStream, TZAbstractBlob.CreateWithData(Pointer(BlobData), Length(BlobData)));
        end;
      else
        raise EZDatabaseError.Create(SUnKnownParamDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(Ord(Param.DataType)));
    end;
  end;
end;

function ConvertAsFractionFormat(const Frmt: String; Scale: Integer;
  ReplaceFractions: Boolean; out FractionLen: Integer): String;
var P, PEnd, MSEnd, QuoteChar, MSStart, SPos, DotPos: PChar;
  L: Integer;
label MoveLast;
begin
  Result := '';
  P := Pointer(Frmt);
  PEnd := P +Length(Frmt);
  MSStart:= nil;
  MSEnd  := nil;
  QuoteChar := nil;
  SPos := nil;
  DotPos := nil;
  while P < PEnd do begin
    if (P^ = #39) or (P^ = '"') then begin
      if QuoteChar = nil then
        QuoteChar := P
      else if QuoteChar^ = P^ then
        QuoteChar := nil;
    end else if (QuoteChar = nil) then begin
      case Ord(P^) or $20 of
        Ord('f'),
        Ord('z'): begin
                    MSEnd := P;
                    if MSStart = nil then
                      MSStart := P;
                  end;
        Ord('.'): DotPos := P;
        Ord('s'): Spos := P;
      end;
    end;
    Inc(P);
  end;
  P := Pointer(Frmt);
  if MSEnd = nil
  then FractionLen := 0
  else begin
    Inc(MSEnd);
    FractionLen := MSEnd - MSStart;
  end;
  L := Length(Frmt);
  if ReplaceFractions then
    Inc(L, 2);
  if MSStart <> nil then begin
    SetLength(Result, L+Scale-FractionLen);
    DotPos := Pointer(Result);
    L := (MSStart - P);
    Move(P^, DotPos^, L * SizeOf(char));
MoveLast:
    Inc(DotPos, L);
    L := Scale;
    if ReplaceFractions then begin
      Inc(L, 2);
      P := Pointer(MilliReplaceQuoted[Scale]);
    end else
      P := Pointer(FractionAdjust[Scale]);
    Move(P^, DotPos^, L * SizeOf(Char));
    Inc(DotPos, L);
    L := (PEnd - MSEnd);
    if L > 0 then
      Move(MSEnd^, DotPos^, L * SizeOf(Char));
  end else begin
    Result := Frmt;
    if DotPos <> nil then begin
      MSEnd := DotPos+1;
      SetLength(Result, L+Scale);
      DotPos := Pointer(Result);
      L := (MSEnd - P);
      Move(P^, DotPos^, L * SizeOf(char));
      goto MoveLast;
    end else if SPos <> nil then begin
      MSEnd := SPos+1;
      SetLength(Result, L+1+Scale);
      DotPos := Pointer(Result);
      L := (SPos - P);
      Move(P^, DotPos^, L * SizeOf(char));
      (DotPos+L+1)^ := '.';
      Inc(L, 2);
      goto MoveLast;
    end;
  end;
end;

function FindFirstDateFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;
var P, PEnd: PChar;
begin
  Delimiter := #0;
  Result := False;
  P := Pointer(Format);
  PEnd := P + Length(Format);
  while P < PEnd do begin
    if (Ord(P^) < Ord('0')) or ((Ord(P^) > Ord('9')) and
       (((Ord(P^) or $20 < Ord('a')) or (Ord(P^) or $20 > Ord('z'))))) then begin
      Delimiter := P^;
      Result := True;
      Break;
    end;
    Inc(P);
  end;
end;

function FindFirstTimeFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;
var P, PEnd: PChar;
begin
  Delimiter := #0;
  Result := False;
  P := Pointer(Format);
  PEnd := P + Length(Format);
  while P < PEnd do begin
    if (Ord(P^) < Ord('0')) or ((Ord(P^) > Ord('9')) and (Ord(P^) <> Ord('.')) and
       (((Ord(P^) or $20 < Ord('a')) or (Ord(P^) or $20 > Ord('z'))))) then begin
      Delimiter := P^;
      Result := True;
      Break;
    end;
    Inc(P);
  end;
end;

Type
  TFormatLiterals = (flYear, flMonth, flDay, flHour, flMinute, flSecond, flFraction, flTimeZone);
function IsSimpleTimeFormat(const Format: String): Boolean;
var P, PEnd: PChar;
  Counters: array[flhour..flFraction] of Integer;
  OtherCount: Integer;
  FormatLiterals: TFormatLiterals;
begin
  FillChar(Counters, SizeOf(Counters), #0);
  OtherCount := 0;
  Result := False;
  P := Pointer(Format);
  PEnd := P + Length(Format);
  while P < PEnd do begin
    case Ord(P^) or $20 of
      Ord('h'): Inc(Counters[flHour]);
      Ord('n'),Ord('m'): Inc(Counters[flMinute]);
      Ord('s'): Inc(Counters[flSecond]);
      Ord('z'),Ord('f'): Inc(Counters[flFraction]);
      Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' '),Ord('t'),Ord('.'):;
      else Inc(OtherCount);
    end;
    Inc(P);
  end;
  if (OtherCount = 0) and (Length(Format) <= ZSysUtils.cMaxTimeLen) then begin
    for FormatLiterals := flHour to flSecond do
      if Counters[FormatLiterals] > 2 then
        Exit;
    Result := True;
  end;
end;

function IsSimpleDateFormat(const Format: String): Boolean;
var P, PEnd: PChar;
  Counters: array[flYear..flTimeZone] of Integer;
  OtherCount: Integer;
  FormatLiterals: TFormatLiterals;
begin
  FillChar(Counters, SizeOf(Counters), #0);
  OtherCount := 0;
  Result := False;
  P := Pointer(Format);
  PEnd := P + Length(Format);
  while P < PEnd do begin
    case Ord(P^) or $20 of
      Ord('y'): Inc(Counters[flYear]);
      Ord('m'): Inc(Counters[flMonth]);
      Ord('d'): Inc(Counters[flDay]);
      Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' '),Ord('t'):;
      else Inc(OtherCount);
    end;
    Inc(P);
  end;
  if (OtherCount = 0) and (Length(Format) <= ZSysUtils.cMaxTimeLen) then begin
    if Counters[flYear] > 4 then
      Exit;
    for FormatLiterals := flMonth to flDay do
      if Counters[FormatLiterals] > 2 then
        Exit;
    Result := True;
  end;
end;

function IsSimpleDateTimeFormat(const Format: String): Boolean;
var P, PEnd: PChar;
  Counters: array[flYear..flFraction] of Integer;
  OtherCount: Integer;
  FormatLiterals: TFormatLiterals;
begin
  FillChar(Counters, SizeOf(Counters), #0);
  OtherCount := 0;
  Result := False;
  P := Pointer(Format);
  PEnd := P + Length(Format);
  while P < PEnd do begin
    case Ord(P^) or $20 of
      Ord('y'): Inc(Counters[flYear]);
      Ord('m'): Inc(Counters[flMonth]);
      Ord('d'): Inc(Counters[flDay]);
      Ord('h'): Inc(Counters[flHour]);
      Ord('n'): Inc(Counters[flMinute]);
      Ord('s'): Inc(Counters[flSecond]);
      Ord('z'),Ord('f'): Inc(Counters[flFraction]);
      Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' '),Ord('t'),Ord('.'):;
      else Inc(OtherCount);
    end;
    Inc(P);
  end;
  if (OtherCount = 0) and (Length(Format) <= ZSysUtils.cMaxTimeStampLen) then begin
    if Counters[flYear] > 4 then
      Exit;
    for FormatLiterals := flMonth to flSecond do
      if Counters[FormatLiterals] > 2 then
        Exit;
    Result := True;
  end;
end;

initialization
  CommonTokenizer := TZGenericSQLTokenizer.Create as IZTokenizer;
finalization
  CommonTokenizer := nil;
end.
