{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Dataset utility functions and classes            }
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

unit ZDatasetUtils;

interface

{$I ZComponent.inc}

uses
  {$IFDEF WITH_DBCONSTS}DBConsts, {$ENDIF}
  Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}Db{$ENDIF},
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ELSE}ZClasses,{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFNDEF DISABLE_ZPARAM}ZDatasetParam,{$ENDIF}
  ZDbcIntfs, ZDbcCache, ZCompatibility, ZExpression, ZVariant, ZTokenizer,
  ZDbcResultSetMetadata, ZSelectSchema;

type
  TZFieldDataSourceType = (dltAccessor, dltResultSet);
  TZFieldsLookUp = record
    Field: Pointer;
    DataSource: TZFieldDataSourceType;
    Index: Integer;
  end;
  TZFieldsLookUpDynArray = array of TZFieldsLookUp;

  {** Defines the target Field-Type }
  TZTransliterationType = (ttField, ttParam,ttSQL);
  TZControlsCodePage = ( //EH: my name is obsolate it should be TZCharacterFieldType, left for backward compatibility
  {$IFDEF UNICODE}
    cCP_UTF16, cGET_ACP, cDynamic
  {$ELSE}
    {$IFDEF FPC}
      {$IFDEF LCL}
        cCP_UTF8, cCP_UTF16, cGET_ACP, cDynamic
      {$ELSE LCL}
        cGET_ACP, cCP_UTF16, cCP_UTF8, cDynamic
      {$ENDIF LCL}
    {$ELSE FPC}
      cGET_ACP, cCP_UTF16, cCP_UTF8, cDynamic
    {$ENDIF FPC}
  {$ENDIF UNICODE});
{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType; CPType: TZControlsCodePage;
  Precision: Integer): TFieldType;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @param StringFieldCodePage the codepage used for the String fields
    which are not data-fields
  @param DataFieldsOnly indicate if the ResultList contains fkDataFields only
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields; StringFieldCodePage: Word;
  NoDataFieldsOnly: Boolean): TObjectList;

{**
  Converts a field definitions into column information objects.
  @param Field a field object.
  @param NativeColumnCodePage the codepage used for the String/memo fields
  @return a column information object.
}
function ConvertFieldToColumnInfo(Field: TField; StringFieldCodePage: Word): TZColumnInfo;

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
  Fill a set of specified field values.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object used for the lookup fields.
  @param ResultSet the ResultSet containing the non calced fields date.
  @param ResultValues a container for result values.
}
procedure FillDataFieldsFromSourceLookup(const FieldIndices: TZFieldsLookUpDynArray;
  RowAccessor: TZRowAccessor; const ResultSet: IZResultSet;
  var ResultValues: TZVariantDynArray);

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
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TZFieldsLookUpDynArray;
  Field: TField): Integer;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TZFieldsLookUpDynArray;
  const FieldRefs: TObjectDynArray): TZFieldsLookUpDynArray;

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

function GetTransliterateCodePage(ControlsCodePage: TZControlsCodePage): Word; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetParam(Idex..);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
{$IFNDEF DISABLE_ZPARAM}
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TZParam);
{$ELSE}
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TParam);
{$ENDIF}

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
  FmtBCD, Variants,
  ZFastCode, ZMessages, ZGenericSqlToken, ZAbstractRODataset,
  ZSysUtils, ZDbcResultSet, ZDbcUtils, ZEncoding;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType; CPType: TZControlsCodePage;
  Precision: Integer): TFieldType;
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
    stString: if Precision <= 0
      then if CPType = cCP_UTF16
        then Result := {$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}
        else Result := ftMemo
      else if CPType = cCP_UTF16
        then Result := ftWideString
        else Result := ftString;
    stBytes: if (Precision <= 0) or (Precision > High(Word))
      then Result := ftBlob
      else Result := ftVarBytes;
    stGUID: Result := {$IFNDEF WITH_FTGUID}ftBytes{$ELSE}ftGUID{$ENDIF};
    stDate:
      Result := ftDate;
    stTime:
      Result := ftTime;
    stTimestamp:
      Result := ftDateTime;
    stAsciiStream: if CPType = cCP_UTF16
        then Result := {$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}
        else Result := ftMemo;
    stBinaryStream:
      Result := ftBlob;
    stUnicodeString: if (Precision <= 0) or (Precision > dsMaxStringSize)
      then if (CPType = cCP_UTF16) or (CPType = cDynamic)
        then Result := {$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}
        else Result := ftMemo
      else if CPType = cCP_UTF16
        then Result := ftWideString
        else Result := ftString;
    stUnicodeStream: if (CPType = cCP_UTF16)or (CPType = cDynamic)
        then Result := {$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftWideString{$ENDIF}
        else Result := ftMemo;
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
      Result := stDouble;
    {$ENDIF}
    ftLargeInt:
      Result := stLong;
    ftCurrency:
      Result := stBigDecimal;
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
  @param ControlsCodePage the codepage used for the String fields
    which are not data-fields
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields; StringFieldCodePage: Word;
  NoDataFieldsOnly: Boolean): TObjectList;
var
  I: Integer;
  Current: TField;
  ColumnInfo: TZColumnInfo;
begin
  Result := TObjectList.Create(True);
  for I := 0 to Fields.Count - 1 do begin
    Current := Fields[I];
    if (Current.FieldKind = fkData) and NoDataFieldsOnly then continue;
    ColumnInfo := ConvertFieldToColumnInfo(Current, StringFieldCodePage);
    Result.Add(ColumnInfo);
  end;
end;

{**
  Converts a field definitions into column information objects.
  @param Field a field object.
  @param NativeColumnCodePage the codepage used for the String/memo fields
  @return a column information object.
}
function ConvertFieldToColumnInfo(Field: TField; StringFieldCodePage: Word): TZColumnInfo;
begin
  Result := TZColumnInfo.Create;
  Result.ColumnType := ConvertDatasetToDbcType(Field.DataType);
  Result.ColumnName := Field.FieldName;
  Result.Precision := Field.Size;
  if Field.DataType in [ftBCD, ftFmtBCD] then
    Result.Scale := Field.DataSize
  else if Field.DataType in [ftMemo, ftString, ftFixedChar] then
    Result.ColumnCodePage := StringFieldCodePage
  else if Field.DataType in [{$IFDEF WITH_FTWIDEMEMO}ftWideMemo, {$ENDIF}
    ftWideString{$IFDEF WITH_FTFIXEDWIDECHAR}, ftFixedWideChar{$ENDIF}] then
    Result.ColumnCodePage := zCP_UTF16;
  Result.ColumnLabel := Field.DisplayName;
  Result.DefaultExpression := Field.DefaultExpression;
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
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
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
  result := nil;
  if Expression.Expression <> '' then begin
    SetLength(Result, Expression.DefaultVariables.Count);
    for I := 0 to Expression.DefaultVariables.Count - 1 do begin
      Current := DataSet.FindField(Expression.DefaultVariables.Names[I]);
      if Current <> nil then
        Result[I] := Current
      else
        Result[I] := nil;
    end;
  end;
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
  Metadata: IZResultSetMetaData;
begin
  Metadata := ResultSet.GetMetadata;
  for I := 0 to High(FieldRefs) do
  begin
    ColumnIndex := TField(FieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    if ColumnIndex >= 0 then begin
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
        {$IFDEF WITH_FTLONGWORD}ftLongword:
          ResultValues[I] := EncodeUInteger(ResultSet.GetULong(ColumnIndex));
        {$ENDIF}
        ftLargeInt: if Metadata.GetColumnType(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) = stULong
          then ResultValues[I] := EncodeUInteger(ResultSet.GetULong(ColumnIndex))
          else ResultValues[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
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
  Fill a set of specified field values.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object used for the lookup fields.
  @param ResultSet the ResultSet containing the non calced fields date.
  @param ResultValues a container for result values.
}
procedure FillDataFieldsFromSourceLookup(
  const FieldIndices: TZFieldsLookUpDynArray; RowAccessor: TZRowAccessor;
  const ResultSet: IZResultSet; var ResultValues: TZVariantDynArray);
var
  I: Integer;
  ColumnIndex: Integer;
  WasNull: Boolean;
begin
  WasNull := False;
  for I := 0 to High(FieldIndices) do
  begin
    ColumnIndex := FieldIndices[I].Index;
    case TField(FieldIndices[I].Field).DataType of
      ftBoolean: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeBoolean(RowAccessor.GetBoolean(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeBoolean(ResultSet.GetBoolean(ColumnIndex));
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftWord, ftSmallInt, ftInteger, ftAutoInc: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeInteger(RowAccessor.GetInt(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeInteger(ResultSet.GetInt(ColumnIndex));
        {$IFDEF WITH_FTSINGLE}ftSingle,{$ENDIF}
        {$IFDEF WITH_FTEXTENDED}ftExtended,{$ENDIF}
      ftFloat, ftCurrency: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeDouble(RowAccessor.GetDouble(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeDouble(ResultSet.GetDouble(ColumnIndex));
      ftBCD: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeCurrency(RowAccessor.GetCurrency(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeCurrency(ResultSet.GetCurrency(ColumnIndex));
      ftFmtBCD: begin
                  InitializeVariant(ResultValues[I], vtBigDecimal);
                  if FieldIndices[I].DataSource = dltAccessor
                  then RowAccessor.GetBigDecimal(ColumnIndex, ResultValues[I].VBigDecimal, WasNull)
                  else ResultSet.GetBigDecimal(ColumnIndex, ResultValues[I].VBigDecimal)
                end;
      {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeInteger(RowAccessor.GetLong(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
      ftDate:   begin
                  InitializeVariant(ResultValues[I], vtDate);
                  if FieldIndices[I].DataSource = dltAccessor
                  then RowAccessor.GetDate(ColumnIndex, WasNull, ResultValues[I].VDate)
                  else ResultSet.GetDate(ColumnIndex, ResultValues[I].VDate);
                end;
      ftTime:   begin
                  InitializeVariant(ResultValues[I], vtTime);
                  if FieldIndices[I].DataSource = dltAccessor
                  then RowAccessor.GetTime(ColumnIndex, WasNull, ResultValues[I].VTime)
                  else ResultSet.GetTime(ColumnIndex, ResultValues[I].VTime);
                end;
      ftDateTime:begin
                  InitializeVariant(ResultValues[I], vtTimeStamp);
                  if FieldIndices[I].DataSource = dltAccessor
                  then RowAccessor.GetTimeStamp(ColumnIndex, WasNull, ResultValues[I].VTimeStamp)
                  else ResultSet.GetTimeStamp(ColumnIndex, ResultValues[I].VTimeStamp);
                end;
      ftWidestring{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeUnicodeString(RowAccessor.GetUnicodeString(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeUnicodeString(ResultSet.GetUnicodeString(ColumnIndex));
      ftBytes, ftVarBytes: if FieldIndices[I].DataSource = dltAccessor
          then ResultValues[I] := EncodeBytes(RowAccessor.GetBytes(ColumnIndex, WasNull))
          else ResultValues[I] := EncodeBytes(ResultSet.GetBytes(ColumnIndex));
      else if FieldIndices[I].DataSource = dltAccessor
        then ResultValues[I] := EncodeString(RowAccessor.GetString(ColumnIndex, WasNull))
        else ResultValues[I] := EncodeString(ResultSet.GetString(ColumnIndex));
    end;
    if FieldIndices[I].DataSource = dltResultSet then
      WasNull := ResultSet.WasNull;
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
      {$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}
      ftWidestring, ftWideMemo:
        Variables.Values[I] := EncodeUnicodeString(TField(Fields[I]).AsWideString);
      {$ELSE}
      ftWidestring: Variables.Values[I] := EncodeUnicodeString(TWideStringField(Fields[I]).Value);
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
  WValue1, WValue2: UnicodeString;
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
  WValue1, WValue2: UnicodeString;
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
          Result := SysUtils.AnsiStrLComp(PWideChar(P2), PWideChar(P1), L1) = 0;
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
        if L2 < L1 then begin
          Result := False;
          Exit;
        end;
        P1 := Pointer(AValue1);
        P2 := Pointer(AValue2);
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
                        Result := ZBCDCompare(KeyValues[I].VBigDecimal, BCD) = 0;
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
      AppendSepString(Result, IdConverter.Quote(Fields[I].FieldName, iqColumn), ',');
  end;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Locale variable "$1" does not seem to be initialized} //BCD is always initialized
{$ENDIF}
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
  BCD1: TBCD;
  BCD2: TBCD;
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
          ResultSet.GetBigDecimal(ColumnIndex, BCD2);
          BCD1 := Field2.AsBCD;
          Result := ZBCDCompare(BCD1, BCD2) = 0;
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
{$IFDEF FPC} {$POP} {$ENDIF}

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
  {$IFDEF WITH_VAR_INIT_WARNING}FieldRefs := nil;{$ENDIF}
  SetLength(FieldRefs, FieldCount);
  {$IFDEF WITH_VAR_INIT_WARNING}CompareKinds := nil;{$ENDIF}
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
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TZFieldsLookUpDynArray;
  Field: TField): Integer;
var
  I: Integer;
begin
  Result := InvalidDbcIndex;
  for I := 0 to High(FieldsLookupTable) do
    if FieldsLookupTable[I].Field = Field then
    begin
      Result := FieldsLookupTable[I].Index;
      Break;
    end;
end;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TZFieldsLookUpDynArray;
  const FieldRefs: TObjectDynArray): TZFieldsLookUpDynArray;
var I, J: Integer;
begin
  if FieldRefs = nil then begin
    Result := nil;
    Exit;
  end;
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
  SetLength(Result, Length(FieldRefs));
  for I := 0 to High(Result) do
    for J := 0 to high(FieldsLookupTable) do
      if FieldsLookupTable[j].Field = TField(FieldRefs[I]) then begin
        Result[i] := FieldsLookupTable[j];
        Break;
      end;
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
        2:  if SupportsCatalogs then begin
              Catalog := SL.Strings[0];
              if SupportsSchemas
              then Schema := SL.Strings[1]
              else ObjectName := SL.Strings[1];
            end else if SupportsSchemas then begin
              Schema := SL.Strings[0];
              ObjectName := SL.Strings[1];
            end else
              ObjectName := SL.Strings[0]+'.'+SL.Strings[1];
        3:  if SupportsCatalogs then begin
              Catalog := SL.Strings[0];
              if SupportsSchemas then begin
                Schema := SL.Strings[1];
                ObjectName := SL.Strings[2]
              end else
                ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
            end else if SupportsSchemas then begin
              Schema := SL.Strings[0];
              ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
            end else
              ObjectName := SL.Strings[0]+'.'+SL.Strings[1]+'.'+SL.Strings[2];
        else if SupportsCatalogs then begin
              Catalog := SL.Strings[0];
              if SupportsSchemas then begin
                Schema := SL.Strings[1];
                for i := 2 to SL.Count-1 do
                  if i = 2
                  then ObjectName := SL.Strings[i]
                  else ObjectName := ObjectName+'.'+SL.Strings[i];
              end else begin
                ObjectName := '';
                for i := 2 to SL.Count-1 do
                  if I = 2
                  then ObjectName := SL.Strings[i]
                  else ObjectName := ObjectName+'.'+SL.Strings[i];
              end;
            end else if SupportsSchemas then begin
              Schema := SL.Strings[0];
              for i := 1 to SL.Count-1 do
                if i = 1
                then ObjectName := SL.Strings[i]
                else ObjectName := ObjectName+'.'+SL.Strings[i];
              end else for i := 0 to SL.Count-1 do
                if I = 0
                then ObjectName := SL.Strings[i]
                else ObjectName := ObjectName+'.'+SL.Strings[i];
        end;
    finally
      SL.Free;
    end;
  end;
end;

function GetTransliterateCodePage(ControlsCodePage: TZControlsCodePage): Word;
begin
  case ControlsCodePage of
    {$IFNDEF UNICODE}
    cCP_UTF8: Result := zCP_UTF8;
    {$ENDIF}
    cGET_ACP:  Result := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF};
    {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
    else Result := DefaultSystemCodePage
    {$ELSE}
      {$IFDEF LCL}
      else Result := zCP_UTF8
      {$ELSE}
      else Result := ZOSCodePage;
      {$ENDIF}
    {$ENDIF}
  end;
end;

{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetXxxx(ColumnIndex, xxx);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
{$IFNDEF DISABLE_ZPARAM}
type TZHackParam = class(TZParam);
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TZParam);
  function CreateUnknownTypeError: EZDatabaseError;
  begin
    Result := EZDatabaseError.Create(SUnKnownParamDataType + ' ('+
      ZFastCode.IntToStr(Ord(Param.DataType))+')');
      //GetEnumName(TypeInfo(TFieldType), Ord(Param.DataType))+')'); //using Typinfo results in collission with tFloatType ):
  end;
begin
  if TZHackParam(Param).FArraySize = 0 then
    if Param.IsNull then
      Statement.SetNull(Index, TZHackParam(Param).SQLType)
    else case TZHackParam(Param).FSQLDataType of
      stBoolean:      Statement.SetBoolean(Index, TZHackParam(Param).FData.pvBool);
      stByte:         Statement.SetByte(Index, TZHackParam(Param).FData.pvByte);
      stShort:        Statement.SetShort(Index, TZHackParam(Param).FData.pvShortInt);
      stWord:         Statement.SetWord(Index, TZHackParam(Param).FData.pvWord);
      stSmall:        Statement.SetSmall(Index, TZHackParam(Param).FData.pvSmallInt);
      stInteger:      Statement.SetInt(Index, TZHackParam(Param).FData.pvInteger);
      stLongWord:     Statement.SetUInt(Index, TZHackParam(Param).FData.pvCardinal);
      stULong:        Statement.SetULong(Index, TZHackParam(Param).FData.pvUInt64);
      stLong:         Statement.SetLong(Index, TZHackParam(Param).FData.pvInt64);
      stFloat:        Statement.SetFloat(Index, TZHackParam(Param).FData.pvSingle);
      stDouble:       Statement.SetDouble(Index, TZHackParam(Param).FData.pvDouble);
      stCurrency:     Statement.SetCurrency(Index, TZHackParam(Param).FData.pvCurrency);
      stBigDecimal:   Statement.SetBigDecimal(Index, TZHackParam(Param).FData.pvBCD);
      stDate:         Statement.SetDate(Index, TZHackParam(Param).FData.pvDate);
      stTime:         Statement.SetTime(Index, TZHackParam(Param).FData.pvTime);
      stTimestamp:    Statement.SetTimestamp(Index, TZHackParam(Param).FData.pvTimeStamp);
      stGUID:         Statement.SetGuid(Index, TZHackParam(Param).FData.pvGUID);
      stString:       if TZHackParam(Param).VariantType = vtUTF8String then
                        Statement.SetUTF8String(Index, UTF8String(TZHackParam(Param).FData.pvPointer))
                      {$IFNDEF NO_ANSISTRING}
                      else if TZHackParam(Param).VariantType = vtAnsiString then
                        Statement.SetAnsiString(Index, AnsiString(TZHackParam(Param).FData.pvPointer))
                      {$ENDIF}
                      else Statement.SetRawByteString(Index, RawByteString(TZHackParam(Param).FData.pvPointer));
      stUnicodeString:Statement.SetUnicodeString(Index, UnicodeString(TZHackParam(Param).FData.pvPointer));
      stBytes:        Statement.SetBytes(Index, TBytes(TZHackParam(Param).FData.pvPointer));
      stAsciiStream:  Statement.SetBlob(Index, stAsciiStream, IZClob(TZHackParam(Param).FData.pvPointer));
      stUnicodeStream:Statement.SetBlob(Index, stUnicodeStream, IZClob(TZHackParam(Param).FData.pvPointer));
      stBinaryStream: Statement.SetBlob(Index, stBinaryStream, IZBlob(TZHackParam(Param).FData.pvPointer));
      else raise CreateUnknownTypeError;
    end
  else begin
    Statement.SetDataArray(Index, TZHackParam(Param).FData.pvDynArray.VArray,
      TZSQLType(TZHackParam(Param).FData.pvDynArray.VArrayType),
      TZHackParam(Param).FData.pvDynArray.VArrayVariantType);
    Statement.SetNullArray(Index, stBoolean,
      TZHackParam(Param).FData.pvDynArray.VIsNullArray, vtNull);
  end;
end;
{$ELSE !DISABLE_ZPARAM}
type THackParam = class(TParam);
procedure SetStatementParam(Index: Integer;
  const Statement: IZPreparedStatement; Param: TParam);
  function CreateUnknownTypeError: EZDatabaseError;
  begin
    Result := EZDatabaseError.Create(SUnKnownParamDataType + ' ('+
      ZFastCode.IntToStr(Ord(Param.DataType))+')');
      //GetEnumName(TypeInfo(TFieldType), Ord(Param.DataType))+')'); //using Typinfo results in collission with tFloatType ):
  end;
  {$IFDEF WITH_FTGUID}
  function CreateGUIDSizeError(L: Integer): EZDatabaseError;
  begin
    Result := EZDatabaseError.Create(Format(SFieldSizeMismatch,
      [Param.NativeStr+'(ftGUID)', 16, L]));
  end;
  {$ENDIF WITH_FTGUID}
var
  TempBytes: TBytes;
  BlobData: TBlobData;
  Lob: IZBLob;
  ConSettings: PZConSettings;
  CP: Word;
  P: Pointer;
  L: NativeUInt;
  UniTemp: UnicodeString;
  {$IFNDEF UNICODE}
  R: RawByteString;
  {$ENDIF}
begin
  if Param.IsNull then
    Statement.SetNull(Index, ConvertDatasetToDbcType(Param.DataType))
  else case Param.DataType of
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
    ftCurrency, ftFloat:
      Statement.SetDouble(Index, Param.AsFloat);
    ftFmtBCD:
      Statement.SetBigDecimal(Index, Param.AsFMTBCD);
    {$IFDEF WITH_FTEXTENDED}
    ftExtended:
      Statement.SetDouble(Index, Param.AsFloat);
    {$ENDIF}
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord:
      Statement.SetUInt(Index, Param.AsLongWord);
    {$ENDIF}
    ftLargeInt: case TvarData(Param.Value).VType of
        {$IFDEF WITH_VARIANT_UINT64}
          {$IFDEF FPC}
          varqword: Statement.SetULong(Index, TVarData(Param.Value).vword64);
          {$ELSE}
          varUInt64: Statement.SetULong(Index, TVarData(Param.Value).VUInt64);
          {$ENDIF}
        {$ENDIF}
        varInt64:   Statement.SetLong(Index, TVarData(Param.Value).VInt64);
        {$IFNDEF WITH_FTLONGWORD}
        varLongWord: Statement.SetUInt(Index, TVarData(Param.Value).VLongWord);
        {$ENDIF}
        else Statement.SetLong(Index, {$IFDEF WITH_PARAM_ASLARGEINT}Param.AsLargeInt{$ELSE}StrToInt64(Param.AsString){$ENDIF});
      end;
    ftBCD:  Statement.SetCurrency(Index, Param.{$IFDEF WITH_PARAM_ASBCD}AsBCD{$ELSE}AsCurrency{$ENDIF});
    ftString, ftFixedChar{$IFDEF WITH_FTWIDESTRING}, ftWideString{$ENDIF}:
      {$IFNDEF UNICODE}
      if (TVarData(Param.Value).VType = varOleStr) {$IFDEF WITH_varUString} or (TVarData(Param.Value).VType = varUString){$ENDIF}
      then Statement.SetUnicodeString(Index, Param.Value)
      else begin
        ConSettings := TZAbstractRODataset(THackParam(Param).DataSet).Connection.DbcConnection.GetConSettings;
        if ConSettings.ClientCodePage.Encoding = ceUTF16 then begin
          CP := TZAbstractRODataset(THackParam(Param).DataSet).Connection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttParam);
          if CP = zCP_UTF8
          then Statement.SetUTF8String(Index, Param.AsString)
          else Statement.SetAnsiString(Index, Param.AsString);
        end else
          Statement.SetRawByteString(Index, Param.AsString);
      end;
      {$ELSE}
      Statement.SetUnicodeString(Index, Param.AsString);
      {$ENDIF}
    ftBytes, ftVarBytes:
        {$IFDEF TPARAM_HAS_ASBYTES}
        Statement.SetBytes(Index, Param.AsBytes);
        {$ELSE}
        Statement.SetBytes(Index, VarToBytes(Param.Value));
        {$ENDIF}
    {$IFDEF WITH_FTGUID}
    // As of now (on Delphi 10.2) TParam has no support of ftGuid data type.
    // GetData and GetDataSize will raise exception on unsupported data types.
    // But user can assign data type manually and as long as he doesn't call
    // these methods things will be fine.
    // Here we presume the data is stored as TBytes or as String.
    ftGuid: if VarIsStr(Param.Value) then
       Statement.SetGuid(Index, StringToGUID(PAram.AsString))
      else begin
        {$IFDEF TPARAM_HAS_ASBYTES}
        TempBytes := Param.AsBytes;
        {$ELSE}
        TempBytes := VarToBytes(Param.Value);
        {$ENDIF}
        if Length(TempBytes) <> SizeOf(TGUID) then
          raise CreateGUIDSizeError(Length(TempBytes));
        Statement.SetGuid(Index, PGUID(TempBytes)^);
      end;
    {$ENDIF}
    ftDate:
      Statement.SetDate(Index, Param.AsDate);
    ftTime:
      Statement.SetTime(Index, Param.AsTime);
    ftDateTime:
      Statement.SetTimestamp(Index, Param.AsDateTime);
    ftMemo, ftFmtMemo{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}: begin
        ConSettings := TZAbstractRODataset(THackParam(Param).DataSet).Connection.DbcConnection.GetConSettings;
        case TvarData(Param.Value).VType of //it's worth it checking the type i.e. Encodings
          {$IFDEF WITH_varUString}varUString,{$ENDIF}
          {$IFDEF UNICODE}varString,{$ENDIF} //otherwise we get a conversion warning
          varOleStr: begin
              UniTemp := Param.{$IFDEF UNICODE}AsMemo{$ELSE}Value{$ENDIF};
              P :=  Pointer(UniTemp);
              if P = nil then
                P := PEmptyUnicodeString;
              Lob := TZLocalMemCLob.CreateWithData(PWideChar(P), Length(UniTemp), ConSettings, nil);
              Statement.SetBlob(Index, stUnicodeStream, Lob);
            end;
          {$IFNDEF UNICODE}
          varString: begin
              R := RawByteString(TVarData(Param.Value).VString);
              P :=  Pointer(R);
              if P <> nil
              then CP := TZAbstractRODataset(THackParam(Param).DataSet).Connection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttParam)
              else begin
                CP := ConSettings.ClientCodePage.CP;
                P := PEmptyAnsiString;
              end;
              Lob := TZAbstractClob.CreateWithData(PAnsiChar(P), Length(R), CP, ConSettings);
              Statement.SetBlob(Index, stAsciiStream, Lob);
            end;
          {$ENDIF}
          else begin //LoadFromStream fills a varArray of Byte
              if not (VarIsArray(Param.Value) and (VarArrayDimCount(Param.Value) = 1) and
                 ((VarType(Param.Value) and VarTypeMask) = varByte)) then
                raise Exception.Create(SInvalidVarByteArray);
              BlobData := Param.AsBlob;
              P := Pointer(BlobData);
              if p = nil then
                P := PEmptyUnicodeString;
              L := Length(BlobData);
              {$IFDEF WITH_WIDEMEMO}
              if Param.DataType = ftWideMemo then begin
                 Lob := TZLocalMemCLob.CreateWithData(PWideChar(P), L shr 1, ConSettings, nil);
                 Statement.SetBlob(Index, stUnicodeStream, Lob);
              end else {$ENDIF}begin
                if ConSettings^.ClientCodePage.Encoding = ceUTF16
                //then CP := GetTransliterateCodePage(TZAbstractRODataset(THackParam(Param).DataSet).Connection.ControlsCodePage)
                then CP := TZAbstractRODataset(THackParam(Param).DataSet).Connection.RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttParam)
                else CP := ConSettings.ClientCodePage.CP;
                Lob := TZLocalMemCLob.CreateWithData(PAnsiChar(P), L, CP, ConSettings, nil);
                Statement.SetBlob(Index, stAsciiStream, Lob);
              end;
            end;
        end;
      end;
    ftBlob, ftGraphic:
      begin
        BlobData := Param.AsBlob;
        Lob := TZLocalMemBLob.CreateWithData(Pointer(BlobData), Length(BlobData), nil);
        Statement.SetBlob(Index, stBinaryStream, Lob);
      end;
    else
      raise CreateUnknownTypeError;
  end;
end;
{$ENDIF DISABLE_ZPARAM}

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

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Locale variable "$1" does not seem to be initialized} //Counters is always initialized
{$ENDIF}
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
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Locale variable "$1" does not seem to be initialized} //Counters is always initialized
{$ENDIF}
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
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Locale variable "$1" does not seem to be initialized} //Counters is always initialized
{$ENDIF}
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
{$IFDEF FPC} {$POP} {$ENDIF}

initialization
  CommonTokenizer := TZGenericSQLTokenizer.Create as IZTokenizer;
finalization
  CommonTokenizer := nil;
end.
