{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Connection Component              }
{                                                         }
{            Originally written by EgonHugeist            }
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
{                                 Zeos Development Group. }
{********************************************************@}

{
constributors:
  aehimself
}

unit ZDatasetParam;

interface

{$I ZComponent.inc}

{$IFNDEF DISABLE_ZPARAM}
uses
  Classes, DB, FmtBCD, {$IFDEF WITH_SqlTimSt_UNIT}SqlTimSt,{$ENDIF} Types,
  {$IFDEF WITH_INLINE_ANSICOMPARETEXT}Windows,{$ENDIF}
  {$IFDEF WITH_GENERIC_TPARAM_LIST}Generics.Collections, {$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for some Delphi inlined methods
  SysUtils, Variants,
  ZCompatibility, ZVariant, ZEncoding, ZDbcIntfs;

type
  {$IFDEF FPC}
  PBoolean = ^Boolean;
  {$ENDIF}
  TZParamValue = record //don't use any managed types here!!!!
    case Integer of
      0:  (pvBool: Boolean);
      1:  (pvByte: Byte);
      2:  (pvShortInt: ShortInt);
      3:  (pvWord: Word);
      4:  (pvSmallInt: SmallInt);
      5:  (pvCardinal: Cardinal);
      6:  (pvInteger: Integer);
      7:  (pvInt64: Int64);
      8:  (pvUInt64: UInt64);
      9:  (pvSingle: Single);
      10: (pvDouble: Double);
      11: (pvCurrency: Currency);
      12: (pvBCD: TBCD);
      13: (pvDateTime: TDateTime);
      14: (pvDate: TZDate);
      15: (pvTime: TZTime);
      16: (pvTimeStamp: TZTimeStamp);
      17: (pvGUID: TGUID);
      18: (pvPointer: Pointer);
      19: (pvDynArray: record
            VArray: Pointer; { Pointer to a Dynamic Array of X}
            VIsNullArray: Pointer; { Pointer to a dynamic boolean array}
          end);
      {$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
      20: (pvExtended: Extended);
      {$IFEND}
    end;

  TZArrayType = (atDML, atIN{, atArrayField yet not supported});
  TZParams = class; //forward
  TZParam = class(TCollectionItem)
  private //MemoryControl
    FDynamicParamType: Boolean;
  protected
    //for batch dml or in () resultset values
    FArraySize: Cardinal;
    FZVariantType: TZVariantType;
    FSQLDataType: TZSQLType; //uncoupled datatype contained in FData
    FData: TZParamValue;
    FConSettings: PZConSettings;
    procedure SetIZBlob(const Value: IZBlob);
    procedure SetIZCLob(const Value: IZCLob);
    property VariantType: TZVariantType read FZVariantType;
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}FParamRef: TZParam;
    FNativeStr: string;
    FPrecision: Integer;
    FNumericScale: Integer;
    FNull: Boolean;
    FName: string;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TParamType;
    FSize: Integer;
    FDecimalSeperator: Char;
    //FParamObjectClass: TParamObjectClass;
    FSQLType: TZSQLType; //the user set SQLType
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}FDataSet: TDataSet;
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}FConnection: TPersistent;
(*    function ParamRef: TParam;
    function GetDataSet: TDataSet;*)
    function IsParamStored: Boolean;
    (*function GetDataType: TFieldType;
    function GetParamType: TParamType;
    procedure SetParamType(Value: TParamType);
    procedure GetStreamData(Buffer: TValueBuffer); *)
  protected
    procedure AssignParam(Param: TParam); overload;
    procedure AssignParam(Param: TZParam); overload;
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(Params: TZParams; ParamType: TParamType); reintroduce; overload;
    constructor Create(Params: TZParams; ParamType: TParamType; SQLType: TZSQLType;
      Precision, Scale: Integer); reintroduce; overload;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignField(Field: TField);
    //procedure AssignFieldValue(Field: TField; const Value: Variant);
    procedure AssignTo(Dest: TPersistent); override;

    procedure GetData(Buffer: Pointer);
    procedure SetData(Buffer: Pointer; ByteLen: Cardinal = $FFFFFFFF);
    procedure SetBlobData(Buffer: Pointer; Size: Integer);

    //function GetDataSize: Integer;
    procedure SetDataSet(Value: TDataSet);
    (*function SetObjectValue(const AInstance: TObject; const ADataType: TFieldType;
      AInstanceOwner: Boolean): IParamObject;
    procedure SetParams(Params: TParams; AInstanceOwner: Boolean);
    procedure SetStream(Stream: TStream; AInstanceOwner: Boolean; KnownSize: Integer = 0);
  public *)
  private
    function TrySetConnection: Boolean;
    function SetConsettings: Boolean;
  private
    class function CreateConversionError(Current, Expected: TZSQLType): EVariantTypeCastError;
    class function CreateIndexError(Value: Integer): EListError;
    procedure SetSQLType(Value: TZSQLType);
    procedure SetSQLDataType(Value: TZSQLType; ZVarType: TZVariantType);
    procedure SetDataType(Value: TFieldType);
    procedure InternalSetAsUnicodeString(DataAddr: PPointer; IsNullAddr: PBoolean; const Value: UnicodeString);
    procedure InternalSetAsRawByteString(DataAddr: PPointer; IsNullAddr: PBoolean; const Value: RawByteString; CodePage: Word);
    function GetDefaultRawCP: Word;
    function IsEqual(Value: TZParam): Boolean;
    procedure CheckDataIndex(Value: Integer); overload;
    procedure CheckDataIndex(Value: Integer; SQLType: TZSQLType; VariantType: TZVariantType;
      out DataAddress: PPointer; out IsNullAddress: PBoolean); overload;
  private {Getter}
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Get the value of a TZParam object as an AnsiString.</summary>
    /// <returns>the value as an AnsiString.</returns>
    function GetAsAnsiString: AnsiString;
    /// <summary>Get the value of a TZParam object as an AnsiString.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as an AnsiString.</returns>
    function GetAsAnsiStrings(Index: Cardinal): AnsiString;
    {$ENDIF NO_ANSISTRING}
    /// <summary>Get the value of a TZParam object as a Boolean value.</summary>
    /// <returns>the value as a Boolean.</summary>
    function GetAsBoolean: Boolean;
    /// <summary>Get the value of a TZParam object as a Boolean value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Boolean</returns>
    function GetAsBooleans(Index: Cardinal): Boolean;
    /// <summary>Get the value of a TZParam object as a Byte value.</summary>
    /// <returns>the value as a Byte.</summary>
    function GetAsByte: Byte;
    /// <summary>Get the value of a TZParam object as a Byte value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Byte</returns>
    function GetAsByteArray(Index: Cardinal): Byte;
    /// <summary>Get the value of a TZParam object as a TBytes array value.</summary>
    /// <returns>the value as a TBytes array.</summary>
    function GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
    /// <summary>Get the value of a TZParam object as a TBytes array value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TBytes array.</returns>
    function GetAsBytesArray(Index: Cardinal): {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
    /// <summary>Get the value of a TZParam object as a Cardinal value.</summary>
    /// <returns>the value as a Cardinal.</summary>
    function GetAsCardinal: Cardinal;
    /// <summary>Get the value of a TZParam object as a Cardinal value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Cardinal.</returns>
    function GetAsCardinals(Index: Cardinal): Cardinal;
    /// <summary>Get the value of a TZParam object as a Currency value.</summary>
    /// <returns>the value as a Currency.</summary>
    function GetAsCurrency: Currency;
    /// <summary>Get the value of a TZParam object as a Currency value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Currency.</returns>
    function GetAsCurrencys(Index: Cardinal): Currency;
    /// <summary>Get the value of a TZParam object as a TDate value.</summary>
    /// <returns>the value as a TDate.</summary>
    function GetAsDate: TDate;
    /// <summary>Get the value of a TZParam object as a TDate value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TDate.</returns>
    function GetAsDates(Index: Cardinal): TDate;
    /// <summary>Get the value of a TZParam object as a TDateTime value.</summary>
    /// <returns>the value as a TDateTime.</summary>
    function GetAsDateTime: TDateTime;
    /// <summary>Get the value of a TZParam object as a TDateTime value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TDateTime.</returns>
    function GetAsDateTimes(Index: Cardinal): TDateTime;
    /// <summary>Get the value of a TZParam object as a Double value.</summary>
    /// <returns>the value as a Double.</summary>
    function GetAsDouble: Double;
    /// <summary>Get the value of a TZParam object as a Double value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Double.</returns>
    function GetAsDoubles(Index: Cardinal): Double;
    /// <summary>Get the value of a TZParam object as a TBCD value.</summary>
    /// <returns>the value as a TBCD.</summary>
    function GetAsFmtBCD: TBCD;
    /// <summary>Get the value of a TZParam object as a TBCD value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TBCD.</returns>
    function GetAsFmtBCDs(Index: Cardinal): TBCD;
    /// <summary>Get the value of a TZParam object as a TGUID value.</summary>
    /// <returns>the value as a TGUID.</summary>
    function GetAsGUID: TGUID;
    /// <summary>Get the value of a TZParam object as a TGUID value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TGUID.</returns>
    function GetAsGUIDs(Index: Cardinal): TGUID;
    /// <summary>Get the value of a TZParam object as a Integer value.</summary>
    /// <returns>the value as a Integer.</summary>
    function GetAsInteger: Integer;
    /// <summary>Get the value of a TZParam object as a Integer value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Integer.</returns>
    function GetAsIntegers(Index: Cardinal): Integer;
    /// <summary>Get the value of a TZParam object as a Int64 value.</summary>
    /// <returns>the value as a Int64.</summary>
    function GetAsInt64: Int64;
    /// <summary>Get the value of a TZParam object as a Int64 value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Int64.</returns>
    function GetAsInt64s(Index: Cardinal): Int64;
    /// <summary>Test if the TZParam object value is null.</summary>
    /// <returns><c>True</c> if the value is null; <c>False</c> otherwise.</summary>
    function GetIsNull: Boolean;
    /// <summary>Test if the TZParam object array value is null.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns><c>True</c> if the value is null; <c>False</c> otherwise.</summary>
    function GetIsNulls(Index: Cardinal): Boolean;
    /// <summary>Test if the TZParam object array value is null.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"DataAddress" the address of the data in the array.</param>
    /// <returns><c>True</c> if the value is null; <c>False</c> otherwise.</summary>
    function GetIsNullsAddr(Index: Integer; out DataAddress: PPointer): Boolean;
    /// <summary>Get the value of a TZParam object as a ShortInt value.</summary>
    /// <returns>the value as a ShortInt.</summary>
    function GetAsShortInt: ShortInt;
    /// <summary>Get the value of a TZParam object as a ShortInt value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a ShortInt.</returns>
    function GetAsShortInts(Index: Cardinal): ShortInt;
    /// <summary>Get the value of a TZParam object as a Single value.</summary>
    /// <returns>the value as a Single.</summary>
    function GetAsSingle: Single;
    /// <summary>Get the value of a TZParam object as a Single value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Single.</returns>
    function GetAsSingles(Index: Cardinal): Single;
    /// <summary>Get the value of a TZParam object as a SmallInt value.</summary>
    /// <returns>the value as a SmallInt.</summary>
    function GetAsSmallInt: SmallInt;
    /// <summary>Get the value of a TZParam object as a SmallInt value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a SmallInt.</returns>
    function GetAsSmallInts(Index: Cardinal): SmallInt;
    /// <summary>Get the value of a TZParam object as a String value.</summary>
    /// <returns>the value as a String.</summary>
    {$IFNDEF UNICODE}
    function GetAsString: String;
    /// <summary>Get the value of a TZParam object as a String value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a String.</returns>
    function GetAsStrings(Index: Cardinal): String;
    {$ENDIF}
    /// <summary>Get the value of a TZParam object as a UInt64 value.</summary>
    /// <returns>the value as a UInt64.</summary>
    function GetAsUInt64: UInt64;
    /// <summary>Get the value of a TZParam object as a UInt64 value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a UInt64.</returns>
    function GetAsUInt64s(Index: Cardinal): UInt64;
    /// <summary>Get the value of a TZParam object as a UTF8String value.</summary>
    /// <returns>the value as a UTF8String.</summary>
    function GetAsUTF8String: UTF8String;
    /// <summary>Get the value of a TZParam object as a UTF8String value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a UTF8String.</returns>
    function GetAsUTF8Strings(Index: Cardinal): UTF8String;
    /// <summary>Get the value of a TZParam object as a UnicodeString value.</summary>
    /// <returns>the value as a UnicodeString.</summary>
    function GetAsUnicodeString: UnicodeString;
    /// <summary>Get the value of a TZParam object as a UnicodeString value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a UnicodeString.</returns>
    function GetAsUnicodeStrings(Index: Cardinal): UnicodeString;
    /// <summary>Get the value of a TZParam object as a Word value.</summary>
    /// <returns>the value as a Word.</summary>
    function GetAsWord: Word;
    /// <summary>Get the value of a TZParam object as a Word value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a Word.</returns>
    function GetAsWords(Index: Cardinal): Word;
    /// <summary>Get the value of a TZParam object as a Variant value.</summary>
    /// <returns>the value as a Variant.</summary>
    function GetAsVariant: Variant;
    /// <summary>Get the value of a TZParam object as a TZTime value.</summary>
    /// <returns>the value as a TZTime.</returns>
    function GetAsTime: TTime;
    /// <summary>Get the value of a TZParam object as a TTime array value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TTime.</returns>
    function GetAsTimes(Index: Cardinal): TTime;
    /// <summary>Get the value of a TZParam object as a TZDate value.</summary>
    /// <returns>the value as a TZDate.</returns>
    function GetAsZDate: TZDate;
    /// <summary>Get the value of a TZParam object as a TZDate array value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TZDate.</returns>
    function GetAsZDates(Index: Cardinal): TZDate;
    /// <summary>Get the value of a TZParam object as a TZTime value.</summary>
    /// <returns>the value as a TZTime.</returns>
    function GetAsZTime: TZTime;
    /// <summary>Get the value of a TZParam object as a TZTime array value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TZTime.</returns>
    function GetAsZTimes(Index: Cardinal): TZTime;
    /// <summary>Get the value of a TZParam object as a TZTimestamp value.</summary>
    /// <returns>the value as a TZTimestamp.</returns>
    function GetAsZTimestamp: TZTimestamp;
    /// <summary>Get the value of a TZParam object as a TZTimestamp array value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <returns>the value as a TZTimestamp.</returns>
    function GetAsZTimestamps(Index: Cardinal): TZTimestamp;
  private { setter }
    procedure SetArraySize(Value: Cardinal);
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Sets a AnsiString value in the TZParam value.
    ///  In case of locked parameter types A
    ///  conversion to any Ordinal, floating, Numeric or implicit codepage
    ///  conversion from CP_ACP to X are allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param></summary>
    /// <param>"Value" the AnsiString value.</summary>
    procedure SetAsAnsiString(const Value: AnsiString);
    /// <summary>Sets a AnsiString value in the TZParam array value. A
    ///  conversion to any Ordinal, floating, Numeric or implicit codepage
    ///  conversion from CP_ACP to X are allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the AnsiString value.</summary>
    procedure SetAsAnsiStrings(Index: Cardinal; const Value: AnsiString);
    {$ENDIF NO_ANSISTRING}
    /// <summary>Sets a TBlobData value in the TZParam value.
    /// <param>"Value" the TBlobData value.</summary>
    procedure SetAsBlob(const Value: TBlobData);
    /// <summary>Sets a TBlobData value in the TZParam array value.
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the TBlobData value.</summary>
    procedure SetAsBlobs(Index: Cardinal; const Value: TBlobData);
    /// <summary>Sets a Boolean value in the TZParam value.
    ///  In case of locked parameter types a conversion to any Number or
    ///  String representation("False"/"True") is allowed.</summary>
    /// <param>"Value" the Boolean value.</summary>
    procedure SetAsBoolean(Value: Boolean);
    /// <summary>Sets a Boolean value in the TZParam array value. A
    ///  conversion to any Ordinal, floating, Numeric or implicit codepage
    ///  conversion from CP_ACP to X are allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Boolean value.</summary>
    procedure SetAsBooleans(Index: Cardinal; Value: Boolean);
    /// <summary>Sets a Byte value in the TZParam value.
    ///  In case of locked parameter types a conversion to FMTBCD, string and
    ///  UnicodeString is allowed.</summary>
    /// <param>"Value" the Byte value.</summary>
    procedure SetAsByte(Value: Byte);
    /// <summary>Sets a Byte value in the TZParam array value. A
    ///  conversion to any Ordinal, floating, Numeric or implicit codepage
    ///  conversion from CP_ACP to X are allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Byte value.</summary>
    procedure SetAsByteArray(Index: Cardinal; Value: Byte);
    /// <summary>Sets a TBytes value in the TZParam value.
    ///  In case of locked parameter types a conversion to FMTBCD, string and
    ///  UnicodeString is allowed.</summary>
    /// <param>"Value" the TBytes value.</summary>
    procedure SetAsBytes(const Value: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF});
    /// <summary>Sets a TBytes value in the TZParam array value. A
    ///  conversion to LOBS is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Byte value.</summary>
    procedure SetAsBytesArray(Index: Cardinal; const Value: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF});
    /// <summary>Sets a Currency value in the TZParam value.
    ///  In case of locked parameter types a conversion to FMTBCD, string and
    ///  UnicodeString is allowed.</summary>
    /// <param>"Value" the Currency value.</summary>
    procedure SetAsCurrency(const Value: Currency);
    /// <summary>Sets a Currency value in the TZParam array value. A conversion
    ///  to FMTBCD is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the currency value.</summary>
    procedure SetAsCurrencys(Index: Cardinal; const Value: Currency);
    /// <summary>Sets a Cardinal value in the TZParam value array.
    ///  In case of locked parameter types to any String or Number is allowed.</summary>
    /// <param>"Value" the Cardinal value.</summary>
    procedure SetAsCardinal(Value: Cardinal);
    /// <summary>Sets a Cardinal value in the TZParam array value. A conversion
    ///  to any String or Number is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Cardinal value.</summary>
    procedure SetAsCardinals(Index: Cardinal; Value: Cardinal);
    /// <summary>Sets a TDate value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the date value.</summary>
    procedure SetAsDate(const Value: TDate);
    /// <summary>Sets a TDate value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the date value.</summary>
    procedure SetAsDates(Index: Cardinal; const Value: TDate);
    /// <summary>Sets a TDateTime value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the DateTime value.</summary>
    procedure SetAsDateTime(const Value: TDateTime);
    /// <summary>Sets a TDateTime value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the DateTime value.</summary>
    procedure SetAsDateTimes(Index: Cardinal; const Value: TDateTime);
    /// <summary>Sets a Double value in the TZParam value array.
    ///  In case of locked parameter types to any String or Number is allowed.</summary>
    /// <param>"Value" the Double value.</summary>
    procedure SetAsDouble(const Value: Double);
    /// <summary>Sets a Double value in the TZParam array value. A conversion
    ///  to any String or Number is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Double value.</summary>
    procedure SetAsDoubles(Index: Cardinal; const Value: Double);
    /// <summary>Sets a TBCD value in the TZParam value.
    ///  In case of locked parameter types a conversion to FMTBCD, string and
    ///  UnicodeString is allowed.</summary>
    /// <param>"Value" the TBCD value.</summary>
    procedure SetAsFmtBCD(Value: TBCD);
    /// <summary>Sets a TBCD value in the TZParam array value. A conversion
    ///  to FMTBCD is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the TBCD value.</summary>
    procedure SetAsFmtBCDs(Index: Cardinal; const Value: TBCD);
    /// <summary>Sets NULL indicator in the TZParam value.</summary>
    /// <param>"Value" the NULL indicator.</summary>
    procedure SetIsNull(Value: Boolean);
    /// <summary>Sets NULL indicator in the TZParam value.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the NULL indicator.</summary>
    procedure SetIsNulls(Index: Cardinal; Value: Boolean);
    /// <summary>Sets a Integer value in the TZParam value array.
    ///  In case of locked parameter types to any String or Number is allowed.</summary>
    /// <param>"Value" the Integer value.</summary>
    procedure SetAsInteger(Value: Integer);
    /// <summary>Sets a Integer value in the TZParam array value. A conversion
    ///  to any String or Number is allowed.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Integer value.</summary>
    procedure SetAsIntegers(Index: Cardinal; Value: Integer);
    /// <summary>Sets a Int64 value in the TZParam value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsInt64(const Value: Int64);
    /// <summary>Sets a Int64 value in the TZParam array value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsInt64s(Index: Cardinal; const Value: Int64);
    /// <summary>Sets a UInt64 value in the TZParam value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUInt64(const Value: UInt64);
    /// <summary>Sets a UInt64 value in the TZParam array value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUInt64s(Index: Cardinal; const Value: UInt64);
    /// <summary>Sets a Memo value in the TZParam value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsMemo(const Value: String); overload;
    /// <summary>Sets a Memo value in the TZParam array value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsMemos(Index: Cardinal; const Value: String);
    /// <summary>Sets a TTime value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the Time value.</summary>
    procedure SetAsTime(const Value: TTime);
    /// <summary>Sets a TTime value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Time value.</summary>
    procedure SetAsTimes(Index: Cardinal; const Value: TTime);
    /// <summary>Sets a UTF8String value in the TZParam value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUTF8String(const Value: UTF8String);
    /// <summary>Sets a UTF8String value in the TZParam array value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUTF8Strings(Index: Cardinal; const Value: UTF8String);
    /// <summary>Sets a UnicodeString value in the TZParam value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUnicodeString(const Value: UnicodeString);
    /// <summary>Sets a UnicodeString value in the TZParam array value.
    ///  A codepage conversion to client encoding is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsUnicodeStrings(Index: Cardinal; const Value: UnicodeString);
    /// <summary>Sets a ShortInt value in the TZParam value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsShortInt(Value: ShortInt);
    /// <summary>Sets a ShortInt value in the TZParam array value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsShortInts(Index: Cardinal; Value: ShortInt);
    /// <summary>Sets a Single value in the TZParam value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsSingle(Value: Single);
    /// <summary>Sets a Single value in the TZParam array value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsSingles(Index: Cardinal; Value: Single);
    /// <summary>Sets a SmallInt value in the TZParam value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsSmallInt(Value: SmallInt);
    /// <summary>Sets a SmallInt value in the TZParam array value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsSmallInts(Index: Cardinal; Value: SmallInt);
    {$IFNDEF UNICODE}
    /// <summary>Sets a String value in the TZParam value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsString(const Value: String);
    /// <summary>Sets a String value in the TZParam array value.
    ///  A conversion to any other ordinal, float, time,date,timestamp or text
    ///   representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsStrings(Index: Cardinal; const Value: String);
    {$ENDIF UNICODE}
    /// <summary>Sets a Word value in the TZParam value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Value" the string value.</summary>
    procedure SetAsWord(const Value: Word);
    /// <summary>Sets a Word value in the TZParam array value.
    ///  A conversion to any other ordinal, float or text representation is
    ///   possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the string value.</summary>
    procedure SetAsWords(Index: Cardinal; const Value: Word);
    /// <summary>Sets a GUID value in the TZParam value.
    ///  A conversion to sting or bytes is possible.</summary>
    /// <param>"Value" the GUID value.</summary>
    procedure SetAsGUID(const Value: TGUID);
    /// <summary>Sets a GUID value in the TZParam array value.
    ///  A conversion to sting or bytes is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the GUID value.</summary>
    procedure SetAsGUIDs(Index: Cardinal; const Value: TGUID);
    /// <summary>Sets a long UTF16-String value in the TZParam value.
    ///  A conversion to any other type except bytes/Blob is possible.</summary>
    /// <param>"Value" the GUID value.</summary>
    procedure SetAsWideMemo(const Value: UnicodeString);
    /// <summary>Sets a long UTF16-String value in the TZParam value.
    ///  A conversion to any other type except bytes/Blob is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the GUID value.</summary>
    procedure SetAsWideMemos(Index: Cardinal; const Value: UnicodeString);
    /// <summary>Sets a Variant value in the TZParam value.</summary>
    /// <param>"Value" the Variant value.</summary>
    procedure SetAsVariant(const Value: Variant);
    /// <summary>Sets a TZDate value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the date value.</summary>
    procedure SetAsZDate(const Value: TZDate);
    /// <summary>Sets a TZDate value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the date value.</summary>
    procedure SetAsZDates(Index: Cardinal; const Value: TZDate);
    /// <summary>Sets a TZTime value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the Time value.</summary>
    procedure SetAsZTime(const Value: TZTime);
    /// <summary>Sets a TZTime value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Time value.</summary>
    procedure SetAsZTimes(Index: Cardinal; const Value: TZTime);
    /// <summary>Sets a TZTimestamp value in the TZParam value.
    ///  A conversion to any float or text representation is possible.</summary>
    /// <param>"Value" the Timestamp value.</param>
    procedure SetAsZTimestamp(const Value: TZTimestamp);
    /// <summary>Sets a TZTimestamp value in the TZParam array value.
    ///  A conversion to float or text representation is possible.</summary>
    /// <param>"Index" the zero based position in the array.</param>
    /// <param>"Value" the Timestamp value.</param>
    procedure SetAsZTimestamps(Index: Cardinal; const Value: TZTimestamp);
  published
    //ArrayType	published	Represents the type of an array-valued TZParam object.
  public
    procedure SetAsRawByteString(const Value: RawByteString; CodePage: Word);
    procedure SetAsRawByteStrings(Index: Cardinal; const Value: RawByteString; CodePage: Word);
    procedure SetAsRawMemo(const Value: RawByteString; CodePage: Word);
    procedure SetAsRawMemos(Index: Cardinal; const Value: RawByteString; CodePage: Word);
  public
    function GetAsRawByteString(CodePage: Word): RawByteString;
    function GetAsRawByteStrings(Index: Cardinal; CodePage: Word): RawByteString;
    procedure Clear;
  public //depracete the default Parameter load
    procedure LoadFromFile(const FileName: String; BlobType: TBlobType); overload; deprecated {$IFDEF WITH_DEPRECATED_MESSAGE}'Use overload instead'{$ENDIF};
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType); overload; deprecated {$IFDEF WITH_DEPRECATED_MESSAGE}'Use overload instead'{$ENDIF};
  public
    {$IFDEF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads text data from a file.</summary>
    /// <param>"FileName" the filename we load from.</param>
    /// <param>"Encoding" the text Encoding.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadFromFile(const FileName: String; Encoding: TEncoding; Index: Integer = -1); overload;
    {$ENDIF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads data from a file.</summary>
    /// <param>"FileName" the filename we load from.</param>
    /// <param>"CodePage" if zero we assume it's a binary file; The CodePage of
    ///  the text otherwise.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadFromFile(const FileName: String; CodePage: Word = zCP_Binary; Index: Integer = -1); overload;
    {$IFDEF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads text data from a stream.</summary>
    /// <param>"Stream" the Stream we load from.</param>
    /// <param>"Encoding" the text Encoding of the stream.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding; Index: Integer = -1); overload;
    {$ENDIF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads data from a Stream.</summary>
    /// <param>"Stream" the Stream we load from.</param>
    /// <param>"CodePage" if zero we assume it's a binary stream data; The CodePage text otherwise.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadFromStream(Stream: TStream; CodePage: Word = zCP_Binary; Index: Integer = -1); overload;
  public //Load from
    /// <summary>Loads text data from a file.</summary>
    /// <param>"FileName" the filename we load from.</param>
    /// <param>"CodePage" a valid CodePage of the text file.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadTextFromFile(const FileName: String; CodePage: Word; Index: Integer = -1); overload;
    {$IFDEF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads text data from a file.</summary>
    /// <param>"FileName" the filename we load from.</param>
    /// <param>"Encoding" the text Encoding.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadTextFromFile(const FileName: String; Encoding: TEncoding; Index: Integer = -1); overload;
    {$ENDIF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads binary data from a stream.</summary>
    /// <param>"Stream" the Stream we load from.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadBinaryFromStream(Stream: TStream; Index: Integer = -1);
    /// <summary>Loads binary data from a File.</summary>
    /// <param>"FileName" the FileName we load from.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadBinaryFromFile(const FileName: String; Index: Integer = -1);
    /// <summary>Loads text data from a stream.</summary>
    /// <param>"FileName" the filename we load from.</param>
    /// <param>"CodePage" a valid CodePage of the text file.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadTextFromStream(Stream: TStream; CodePage: Word; Index: Integer = -1); overload;
    {$IFDEF TENCODING_HAS_CODEPAGE}
    /// <summary>Loads text data from a stream.</summary>
    /// <param>"Stream" the Stream we load from.</param>
    /// <param>"Encoding" the text Encoding of the stream.</param>
    /// <param>"Index" if negative it's the native value index; An array index
    ///  otherwise.</summary>
    procedure LoadTextFromStream(Stream: TStream; Encoding: TEncoding; Index: Integer = -1); overload;
    {$ENDIF TENCODING_HAS_CODEPAGE}
  public //the users given published properties in alphabetical order
    // <summary>Represents the size of a TZParam object array.<summary>
    //commented yet: setting array size should happen by TZParams only
    //property ArraySize: Cardinal read fArraySize write SetArraySize;
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Represents the value of a TZParam object as an AnsiString.</summary>
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    /// <summary>Represents an array of AnsiString values.</summary>
    property AsAnsiStrings[Index: Cardinal]: AnsiString read GetAsAnsiStrings write SetAsAnsiStrings;
    {$ENDIF NO_ANSISTRING}
    /// <summary>Represents the value of a TZParam as a binary-coded decimal
    ///  (BCD/System.Currency)</summary>
    property AsBCD: Currency read GetAsCurrency write SetAsCurrency;
    /// <summary>Represents an array of binary-coded decimal (BCD/System.Currency) values.</summary>
    property AsBCDs[Index: Cardinal]: Currency read GetAsCurrencys write SetAsCurrencys;
    /// <summary>Specifies the value of a TZParam when it represents a
    ///  binary large object (BLOB).</summary>
    property AsBlob: TBlobData read {$IFDEF TBLOBDATA_IS_TBYTES}GetAsBytes{$ELSE}GetAsAnsiString{$ENDIF}write SetAsBlob;
    /// <summary>Represents an array of binary large object (BLOB) values.</summary>
    property AsBlobs[Index: Cardinal]: TBlobData read {$IFDEF TBLOBDATA_IS_TBYTES}GetAsBytesArray{$ELSE}GetAsAnsiStrings{$ENDIF} write SetAsBlobs;
    /// <summary>Specifies the value of a TZParam when it represents a Boolean.</summary>
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    /// <summary>Represents an array of Boolean values.</summary>
    property AsBooleans[Index: Cardinal]: Boolean read GetAsBooleans write SetAsBooleans;
    /// <summary>Specifies the value of a TZParam when it represents a Byte.</summary>
    property AsByte: Byte read GetAsByte write SetAsByte;
    /// <summary>Represents an array of Byte values.</summary>
    property AsByteArray[Index: Cardinal]: Byte read GetAsByteArray write SetAsByteArray;
    /// <summary>Specifies the value of a TZParam when it represents a Byte array.</summary>
    property AsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF} read GetAsBytes write SetAsBytes;
    /// <summary>Represents an array of Byte values.</summary>
    property AsBytesArray[Index: Cardinal]: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF} read GetAsBytesArray write SetAsBytesArray;
    /// <summary>Specifies the value of a TZParam when it represents a Cardinal</summary>
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    /// <summary>Represents an array of Cardinal values.</summary>
    property AsCardinals[Index: Cardinal]: Cardinal read GetAsCardinals write SetAsCardinals;
    /// <summary>Specifies the value of a TZParam when it represents a Currency</summary>
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    /// <summary>Represents an array of Currency values.</summary>
    property AsCurrencys[Index: Cardinal]: Currency read GetAsCurrencys write SetAsCurrencys;
    /// <summary>Specifies the value of a TZParam when it represents a Double</summary>
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    /// <summary>Represents an array of Double values.</summary>
    property AsDoubles[Index: Cardinal]: Double read GetAsDoubles write SetAsDoubles;

    {AsDataSet	public	Specifies the value of a TZParam when it represents a TDataSet.
    AsDataSets	public	Represents an array of TDataSet values.}

    /// <summary>Specifies the value of a TZParam when it represents a TDate</summary>
    property AsDate: TDate read GetAsDate write SetAsDate;
    /// <summary>Represents an array of TDate values.</summary>
    property AsDates[Index: Cardinal]: TDate read GetAsDates write SetAsDates;
    /// <summary>Specifies the value of a TZParam when it represents a TDateTime</summary>
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    /// <summary>Represents an array of TDateTime values.</summary>
    property AsDateTimes[Index: Cardinal]: TDateTime read GetAsDateTimes write SetAsDateTimes;
    {AsExtended	public	Specifies the value of a TZParam when it represents a Extended.
    AsExtendeds	public	Represents an array of Extended values.
    AsFixedChar	public	Specifies the value of a TZParam when it represents a String.
    AsFixedChars	public	Represents an array of String values.}
    /// <summary>Specifies the value of a TZParam when it represents a Float</summary>
    property AsFloat: Double read GetAsDouble write SetAsDouble;
    /// <summary>Represents an array of Float values.</summary>
    property AsFloats[Index: Cardinal]: Double read GetAsDoubles write SetAsDoubles;
    /// <summary>Specifies the value of a TZParam when it represents a TBcd</summary>
    property AsFmtBCD: TBCD read GetAsFmtBCD write SetAsFmtBCD;
    /// <summary>Represents an array of TBcd values.</summary>
    property AsFmtBCDs[Index: Cardinal]: TBCD read GetAsFmtBCDs write SetAsFmtBCDs;
    /// <summary>Specifies the value of a TZParam when it represents a TGUID</summary>
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;
    /// <summary>Represents an array of TGUID values.</summary>
    property AsGUIDs[Index: Cardinal]: TGUID read GetAsGUIDs write SetAsGUIDs;
    /// <summary>Specifies the value of a TZParam when it represents a 32Bit Integer.</summary>
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    /// <summary>Represents an array of 32Bit Integer values.</summary>
    property AsIntegers[Index: Cardinal]: Integer read GetAsIntegers write SetAsIntegers;
    /// <summary>Specifies the value of a TZParam when it represents a 64Bit Integer.</summary>
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    /// <summary>Represents an array of 64Bit Integer values.</summary>
    property AsInt64s[Index: Cardinal]: Int64 read GetAsInt64s write SetAsInt64s;
    /// <summary>Specifies the value of a TZParam when it represents a LargeInt.</summary>
    property AsLargeInt: LargeInt read GetAsInt64 write SetAsInt64;
    /// <summary>Represents an array of LargeInt values.</summary>
    property AsLargeInts[Index: Cardinal]: Int64 read GetAsInt64s write SetAsInt64s;
    /// <summary>Specifies the value of a TZParam when it represents a 32Bit LongWord.</summary>
    property AsLongword: Cardinal read GetAsCardinal write SetAsCardinal;
    /// <summary>Represents an array of 32Bit LongWord values.</summary>
    property AsLongwords[Index: Cardinal]: Cardinal read GetAsCardinals write SetAsCardinals;
    /// <summary>Specifies the value of a TZParam when it represents a Memo.</summary>
    property AsMemo: String read {$IFDEF UNICODE}GetAsUnicodeString{$ELSE}GetAsString{$ENDIF} write SetAsMemo;
    /// <summary>Represents an array of Memo values.</summary>
    property AsMemos[Index: Cardinal]: String read {$IFDEF UNICODE}GetAsUnicodeStrings{$ELSE}GetAsStrings{$ENDIF} write SetAsMemos;
    /// <summary>Specifies the value of a TZParam when it represents a 8Bit Integer.</summary>
    property AsShortInt: ShortInt read GetAsShortInt write SetAsShortInt;
    /// <summary>Represents an array of 8Bit signed integer values.</summary>
    property AsShortInts[Index: Cardinal]: ShortInt read GetAsShortInts write SetAsShortInts;
    /// <summary>Specifies the value of a TZParam when it represents a Single.</summary>
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    /// <summary>Represents an array of Single values.</summary>
    property AsSingles[Index: Cardinal]: Single read GetAsSingles write SetAsSingles;
    /// <summary>Specifies the value of a TZParam when it represents a 16Bit singned Integer.</summary>
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    /// <summary>Represents an array of 16Bit signed integer values.</summary>
    property AsSmallInts[Index: Cardinal]: SmallInt read GetAsSmallInts write SetAsSmallInts;
    {
    AsSQLTimeStamp	public	Specifies the value of a TZParam when it represents a TSQLTimeStamp.
    AsSQLTimeStamps	public	Represents an array of TSQLTimeStamp values.
    AsStream	public	Specifies the value of a TZParam when it represents a TStream object.
    AsStreams	public	Represents an array of TStream values.}
    /// <summary>Specifies the value of a TZParam when it represents a String.</summary>
    property AsString: String read {$IFDEF UNICODE}GetAsUnicodeString write SetAsUnicodeString{$ELSE}GetAsString write SetAsString{$ENDIF};
    /// <summary>Represents an array of String values.</summary>
    property AsStrings[Index: Cardinal]: String read {$IFDEF UNICODE}GetAsUnicodeStrings write SetAsUnicodeStrings{$ELSE}GetAsStrings write SetAsStrings{$ENDIF};
    /// <summary>Specifies the value of a TZParam when it represents a TTime.</summary>
    property AsTime: TTime read GetAsTime write SetAsTime;
    /// <summary>Represents an array of TTime values.</summary>
    property AsTimes[Index: Cardinal]: TTime read GetAsTimes write SetAsTimes;
    /// <summary>Specifies the value of a TZParam when it represents a UInt64.</summary>
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
    /// <summary>Represents an array of UInt64 values.</summary>
    property AsUInt64s[Index: Cardinal]: UInt64 read GetAsUInt64s write SetAsUInt64s;
    /// <summary>Specifies the value of a TZParam when it represents a UnicodeString.</summary>
    property AsUnicodeMemo: UnicodeString read GetAsUnicodeString write SetAsWideMemo;
    /// <summary>Represents an array of UnicodeString values.</summary>
    property AsUnicodeMemos[Index: Cardinal]: UnicodeString read GetAsUnicodeStrings write SetAsWideMemos;
    /// <summary>Specifies the value of a TZParam when it represents a UnicodeString.</summary>
    property AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
    /// <summary>Represents an array of UnicodeString values.</summary>
    property AsUnicodeStrings[Index: Cardinal]: UnicodeString read GetAsUnicodeStrings write SetAsUnicodeStrings;
    /// <summary>Specifies the value of a TZParam when it represents a UTF8String.</summary>
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    /// <summary>Represents an array of UTF8String values.</summary>
    property AsUTF8Strings[Index: Cardinal]: UTF8String read GetAsUTF8Strings write SetAsUTF8Strings;
    {AsVarByteStr	public	Specifies the value of a TZParam when it represents a TFDByteString.
    AsVarByteStrs	public	Represents an array of TFDByteString values.}
    /// <summary>Specifies the value of a TZParam when it represents a UnicodeString.</summary>
    property AsWideMemo: UnicodeString read GetAsUnicodeString write SetAsWideMemo;
    /// <summary>Represents an array of UnicodeString values.</summary>
    property AsWideMemos[Index: Cardinal]: UnicodeString read GetAsUnicodeStrings write SetAsWideMemos;
    /// <summary>Specifies the value of a TZParam when it represents a UnicodeString.</summary>
    property AsWideString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
    /// <summary>Represents an array of UnicodeString values.</summary>
    property AsWideStrings[Index: Cardinal]: UnicodeString read GetAsUnicodeStrings write SetAsUnicodeStrings;
    /// <summary>Specifies the value of a TZParam when it represents a 16Bit unsingned Integer.</summary>
    property AsWord: Word read GetAsWord write SetAsWord;
    /// <summary>Represents an array of 16Bit unsigned integer values.</summary>
    property AsWords[Index: Cardinal]: Word read GetAsWords write SetAsWords;
//  AsXML	public	Specifies the value of a TZParam when it represents a XML.
//  AsXMLs	public	Represents an array of UnicodeString values.
    //Values	public	Represents the value of a TZParam object as a Variant. }
    /// <summary>Specifies the value of a TZParam when it represents a TZDate.</summary>
    property AsZDate: TZDate read GetAsZDate write SetAsZDate;
    /// <summary>Represents an array of TZDate values.</summary>
    property AsZDates[Index: Cardinal]: TZDate read GetAsZDates write SetAsZDates;
    /// <summary>Specifies the value of a TZParam when it represents a TZTime.</summary>
    property AsZTime: TZTime read GetAsZTime write SetAsZTime;
    /// <summary>Represents an array of TZTime values.</summary>
    property AsZTimes[Index: Cardinal]: TZTime read GetAsZTimes write SetAsZTimes;
    /// <summary>Specifies the value of a TZParam when it represents a TZTimestamp.</summary>
    property AsZTimestamp: TZTimestamp read GetAsZTimestamp write SetAsZTimestamp;
    /// <summary>Represents an array of TZTimestamp values.</summary>
    property AsZTimestamps[Index: Cardinal]: TZTimestamp read GetAsZTimestamps write SetAsZTimestamps;
    /// <summary>Indicates whether a value (NULL or otherwise) has been assigned
    ///  to the parameter.</summary>
    property Bound: Boolean read FBound;
    {Collection	public	Specifies the TCollection instance to which the TCollectionItem belongs.
    DisplayName	public	The name displayed in the Collection editor.
    ID	public	A unique, permanent index for the item.
    Index	public	Returns the item's position in the Items array of TCollection.
    IsCaseSensitive	published	Identifies if a TZParam name is case sensitive.
    IsDataSet	public	Specifies if the DataType is ftDataSet and the TDataSet object is assigned to the parameter value.
    IsDatasets	public	Specifies if the DataType is ftDataSet and the TDataSet object is assigned to the parameter value.}
    /// <summary>Indicates whether a TZParam value is NULL.</summary>
    property IsNull: Boolean read GetIsNull write SetIsNull;
    /// <summary>Indicates whether a TZParam array value is NULL.</summary>
    property IsNulls[Index: Cardinal]: Boolean read GetIsNulls write SetIsNulls;
    {IsObject	public	Specifies whether the DataType is ftDataSet or ftStream and the TObject value is assigned.
    IsObjects	public	Specifies whether the DataType is ftDataSet or ftStream and the TObject value is assigned.
    IsStream	public	Specifies whether the DataType is ftStream and the TStream value is assigned.
    IsStreams	public	Specifies whether the DataType is ftStream and the TStream value is assigned.
    IsUnicode	public	Specifies whether the DataType of a TZParam object is an Unicode-based data type.
    Text	public	Represents the value of a TZParam object as a string.
    Texts	public	Represents the value of a TZParam object as a string.}
    property NativeStr: String read FNativeStr write FNativeStr;
  published
    /// <summary>Indicates the type of field whose value the TZParam represents.</summary>
    property DataType: TFieldType read FDataType write SetDataType default ftUnknown;
    /// <summary>Indicates the name of a TZParam object.</summary>
    property Name: String read FName write FName;
    /// <summary>Specifies the number of decimal places for a TZParam object.</summary>
    property NumericScale: Integer read FNumericScale write FNumericScale  default 0;
    /// <summary>Indicates the type of the parameter the TZParam represents.</summary>
    property ParamType: TParamType read FParamType write FParamType default ptUnknown;
//    Position published Indicates the position of the parameter.
    /// <summary>Specifies the number of digits allowed for a numeric parameter.</summary>
    property Precision: Integer read FPrecision write FPrecision default 0;
    /// <summary>Specifies the number of characters in a string-typed parameter.</summary>
    property Size: Integer read FSize write FSize default 0;
//    SQLName public Name of the parameter in the format required to use the parameter name in a SQL query.
    /// <summary>Specifies the exact SQLType for a TZParam object.</summary>
    property SQLType: TZSQLType read FSQLType write SetSQLType default stUnknown;
    //StreamMode	published	Stream mode of the parameter.
    /// <summary>Specifies the value of a TZParam when it represents a TZDate.</summary>
    property Value: Variant read GetAsVariant write SetAsVariant stored IsParamStored;
  end;

  TZParams = class(TCollection)
  private
    FArraySize: Cardinal;
    FConnection: TPersistent;
    procedure SetArraySize(Value: Cardinal);
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}FOwner: TPersistent;
    function GetParamValue(const ParamName: string): Variant;
    procedure SetParamValue(const ParamName: string;
      const Value: Variant);
    function GetItem(Index: Integer): TZParam;
    procedure SetItem(Index: Integer; Value: TZParam);
  protected
    function GetDataSet: TDataSet;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    function AddParameter: TZParam;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignValues(Value: TZParams);
    { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
    constructor Create; overload;
    constructor Create(Owner: TPersistent); overload;
    procedure AddParam(Value: TZParam);
    procedure RemoveParam(Value: TZParam);
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TParamType): TZParam; overload;
    function CreateParam(SQLType: TZSQLType; const ParamName: string;
      ParamType: TParamType; Precision, Scale: Integer): TZParam; overload;
    {$IFDEF WITH_GENERIC_TPARAM_LIST}
    procedure GetParamList(List: TList<TZParam>; const ParamNames: string); overload;
    {$ELSE}
    procedure GetParamList(List: TList; const ParamNames: string); overload;
    {$ENDIF}
    function IsEqual(Value: TZParams): Boolean;
    function ParamByName(const Value: string): TZParam;
    function FindParam(const Value: string): TZParam;
    procedure FlushParameterConSettings;
  public
    property BatchDMLCount: Cardinal read FArraySize write SetArraySize;
    property Items[Index: Integer]: TZParam read GetItem write SetItem; default;
    property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
  end;

{$ENDIF DISABLE_ZPARAM}
implementation
{$IFNDEF DISABLE_ZPARAM}

uses TypInfo, {$IFDEF WITH_DBCONSTS} DBConsts {$ELSE} DBConst{$ENDIF}, Math,
  ZSysUtils, ZFastCode, ZMessages,
  ZDbcUtils, ZDbcResultSet,
  ZAbstractRODataset, ZAbstractConnection, ZSqlUpdate, ZDatasetUtils,
  ZSqlProcessor;

var D1M1Y1: TDateTime;

const ZDataOffsetSizes: array[TZSQLType] of Cardinal = (0,
  SizeOf(Boolean), SizeOf(Byte), SizeOf(ShortInt), SizeOf(Word), SizeOf(SmallInt),
  SizeOf(Cardinal), SizeOf(Integer), SizeOf(UInt64), SizeOf(Int64),
  SizeOf(Single), SizeOf(Double), {$IFDEF ZEOS90UP}SizeOf(Decimal128),{$ENDIF}
  SizeOf(Currency), SizeOf(TBCD),
  SizeOf(TZDate), SizeOf(TZTime), SizeOf(TZTimestamp),
  SizeOf(TGUID), SizeOf(Pointer), SizeOf(Pointer), SizeOf(Pointer),
  SizeOf(Pointer), SizeOf(Pointer), SizeOf(Pointer),
  {$IFDEF ZEOS90UP}
  SizeOf(Pointer), SizeOf(Pointer), SizeOf(Variant),
  {$ENDIF ZEOS90UP}
    //finally the object types
  SizeOf(TZArray),{$IFDEF ZEOS90UP}SizeOf(Pointer), SizeOf(Pointer){$ELSE}SizeOf(Pointer){$ENDIF}
  );

const ZDataIncompatibilityMatrix: array[TZSQLType] of set of TZSQLType = (
  {stUnknown}   [Low(TZSQLType)..High(TZSQLType)],
  {stBoolean}   [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stByte}      [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stshort}     [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stWord}      [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stSmall}     [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stLongWord}  [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stInteger}   [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stULong}     [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stLong}      [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stFloat}     [stGUID, stBytes, stBinaryStream, stArray],
  {stDouble}    [stGUID, stBytes, stBinaryStream, stArray],
  {stCurrency}  [stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stBigDecimal}[stDate, stTime, stTimestamp, stGUID, stBytes, stBinaryStream, stArray],
  {stDate}      [stBoolean..stLong, stCurrency, stBigDecimal, stBytes, stBinaryStream, stArray],
  {stTime}      [stBoolean..stLong, stCurrency, stBigDecimal, stBytes, stBinaryStream, stArray],
  {stTimestamp} [stBoolean..stLong, stCurrency, stBigDecimal, stBytes, stBinaryStream, stArray],
  {stGUID}      [stBoolean..stTimestamp],
  {stString}    [],
  {stUnicodeString} [],
  {stBytes}     [stBoolean..stTimestamp, stUnicodeString, stUnicodeStream],
  {stAsciiStream} [stBoolean..stTimestamp],
  {stUnicodeStream} [stBoolean..stTimestamp],
  {stBinaryStream}  [stBoolean..stTimestamp, stUnicodeString, stUnicodeStream],
  {$IFDEF ZEOS90UP}
  SizeOf(Pointer), SizeOf(Pointer), SizeOf(Variant),
  {$ENDIF ZEOS90UP}
  {stArray}     [Low(TZSQLType)..High(TZSQLType)],
  {$IFDEF ZEOS90UP}SizeOf(Pointer), SizeOf(Pointer){$ELSE}[Low(TZSQLType)..High(TZSQLType)]{$ENDIF}
  );

function GetDefaultCharacterFieldType(Self: TZParam): TZControlsCodePage;
begin
  with Self do
    if ((fConnection <> nil) or TrySetConnection)
    then Result := TZAbstractConnection(fConnection).ControlsCodePage
    else Result :=
    {$IFDEF UNICODE}
      cCP_UTF16
    {$ELSE}
      {$IFDEF FPC}
        {$IFDEF LCL}
          cCP_UTF8
        {$ELSE LCL}
          cGET_ACP
        {$ENDIF LCL}
      {$ELSE FPC}
        cGET_ACP
      {$ENDIF FPC}
    {$ENDIF UNICODE};
end;

{ TZParam }

procedure TZParam.Assign(Source: TPersistent);
var
  StreamPersist: IStreamPersist;
  procedure LoadFromStreamPersist(const StreamPersist: IStreamPersist);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      StreamPersist.SaveToStream(MS);
      LoadBinaryFromStream(MS);
    finally
      MS.Free;
    end;
  end;
  procedure LoadFromStrings(Source: TSTrings);
  begin
    AsMemo := Source.Text;
  end;
begin
  if Source = nil then Exit;
  if Source is TZParam then
    AssignParam(TZParam(Source))
  else if Source is TParam then
    AssignParam(TParam(Source))
  else if Source is TField then
    AssignField(TField(Source))
  else if Source is TStrings then
    LoadFromStrings(TStrings(Source))
  else if Supports(Source, IStreamPersist, StreamPersist) then
    LoadFromStreamPersist(StreamPersist)
  else
    inherited Assign(Source);
end;

procedure TZParam.AssignField(Field: TField);
  procedure MoveUnicodeStrings;
  begin
    {$IFDEF WITH_WIDEMEMO}
    if Field.DataType = ftWideMemo then
      {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      SetAsWideMemo(Field.AsUnicodeString)
      {$ELSE}
      SetAsWideMemo(Field.AsWideString)
      {$ENDIF}
    else
    {$ENDIF}
      {$IFDEF WITH_VIRTUAL_TFIELD_ASUNICODESTRING}
      SetAsUnicodeString(Field.AsUnicodeString);
      {$ELSE}
        {$IFNDEF WITH_WIDEMEMO}
      SetAsUnicodeString(TWideStringField(Field).Value);
        {$ELSE}
      SetAsUnicodeString(Field.AsWideString);
        {$ENDIF}
      {$ENDIF}
  end;
  procedure MoveTBytes;
  begin
    if Field.DataType in [ftBytes, ftVarBytes] then
      {$IFDEF TFIELD_HAS_ASBYTES}
      SetAsBytes(Field.AsBytes)
      {$ELSE}
      SetAsBytes(VarToBytes(Field.Value))
      {$ENDIF}
    else
      {$IFDEF TBLOBDATA_IS_TBYTES}
      SetAsBlob(TBlobField(Field).AsBytes);
      {$ELSE}
      SetAsBlob(TBlobField(Field).Value);
      {$ENDIF}
  end;
  procedure MoveRawStrings;
  var CP: Word;
  begin
    if Field is TZRawCLobField then begin
      CP := TZRawCLobField(Field).CodePage;
      if CP = zCP_UTF16
      then SetAsWideMemo(TZRawCLobField(Field).AsUnicodeString)
      else SetAsRawMemo(TZRawCLobField(Field).AsRawByteString, CP);
    end else if Field is TZRawStringField then begin
      CP := TZRawStringField(Field).CodePage;
      if CP = zCP_UTF16
      then SetAsUnicodeString(TZRawStringField(Field).AsUnicodeString)
      else SetAsRawByteString(TZRawStringField(Field).AsRawByteString, TZRawStringField(Field).CodePage);
    end else if Field is TMemoField then
      SetAsMemo(Field.AsString)
    else {$IFDEF UNICODE}SetAsUnicodeString{$ELSE}SetAsString{$ENDIF}(Field.AsString);
  end;
begin
  if Field <> nil then
    case Field.DataType of
      //ftUnknown: ;
      ftString:     MoveRawStrings;
      ftSmallint:   {$IFNDEF WITH_FTSHORTINT}
                    if Field is TZShortIntField
                    then SetAsShortInt(TZShortIntField(Field).Value)
                    else {$ENDIF}SetAsSmallInt(SmallInt(Field.AsInteger));
      ftInteger:    SetAsInteger(Field.AsInteger);
      ftWord:       {$IFNDEF WITH_FTBYTE}
                    if Field is TZByteField
                    then SetAsByte(Field.AsInteger)
                    else {$ENDIF}SetAsWord(Word(Field.AsInteger));
      ftBoolean:    SetAsBoolean(Field.AsBoolean);
      ftFloat:      {$IFNDEF WITH_FTSINGLE}
                    if Field is TZSingleField
                    then SetAsSingle(TZSingleField(Field).Value)
                    else {$ENDIF}SetAsDouble(Field.AsFloat);
      ftCurrency:   SetAsDouble(Field.AsFloat);
      ftBCD:        SetAsCurrency(Field.AsCurrency);
      ftDate:       if Field is TZDateField
                    then SetAsZDate(TZDateField(Field).Value)
                    else SetAsDate(Field.AsDateTime);
      ftTime:       if Field is TZTimeField
                    then SetAsZTime(TZTimeField(Field).Value)
                    else SetAsTime(Field.AsDateTime);
      ftDateTime:   if Field is TZDateTimeField
                    then SetAsZTimestamp(TZDateTimeField(Field).Value)
                    else SetAsDateTime(Field.AsDateTime);
      ftBytes,
      ftVarBytes,
      //ftAutoInc: ;
      ftBlob:       MoveTBytes;
      ftMemo:       MoveRawStrings;
      ftGraphic:    MoveTBytes;
      ftFmtMemo:    MoveRawStrings;
      {ftParadoxOle: ;
      ftDBaseOle: ;
      ftTypedBinary: ;
      ftCursor: ;
      ftFixedChar: ; }
      ftWideString: MoveUnicodeStrings;
      ftLargeint:   {$IFDEF WITH_FTLONGWORD}
                    if Field is TLongWordField
                    then SetAsCardinal(TLongWordField(Field).Value)
                    else {$ENDIF}if Field is TZUInt64Field
                      then SetAsUInt64(TZUInt64Field(Field).Value)
                      else SetAsInt64(TLargeIntField(Field).Value);
      {ftADT: ;
      ftArray: ;
      ftReference: ;
      ftDataSet: ;}
      ftOraBlob: MoveTBytes;
      ftOraClob: MoveRawStrings;
      {ftVariant: ;
      ftInterface: ;
      ftIDispatch: ;}
      ftGuid:       SetAsGUID(TGUIDField(Field).AsGuid);
      //ftTimeStamp: ;
      ftFMTBcd:     SetAsFmtBCD(TFmtBCDField(Field).Value);
      {$IFDEF WITH_WIDEMEMO}
      ftFixedWideChar,
      ftWideMemo:   MoveUnicodeStrings;
      {$ENDIF}
      {ftOraTimeStamp: ;
      ftOraInterval: ;}
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:     SetAsCardinal(TLongWordField(Field).Value);
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortint:     SetAsShortInt(Field.AsInteger);
      {$ENDIF}
      {$IFDEF WITH_FTBYTE}
      ftByte:         SetAsByte(Field.AsInteger);
      {$ENDIF}
      {$IFDEF WITH_FTEXTENDED}
      Db.ftExtended:     SetAsDouble(Field.AsFloat);
      {$ENDIF}
      {ftConnection: ;
      ftParams: ;
      ftStream: ;
      ftTimeStampOffset: ;
      ftObject: ;}
      {$IFDEF WITH_FTSINGLE}
      Db.ftSingle:       SetAsSingle(TSingleField(Field).Value);
      {$ENDIF}
      else SetAsVariant(Field.Value);
    end;
end;

procedure TZParam.AssignParam(Param: TParam);
begin
  if Param <> nil then begin
    SetArraySize(0);
    SetAsVariant(Param.Value);
    SetDataType(Param.DataType);
    FBound := Param.Bound;
    FName := Param.Name;
    if FParamType = ptUnknown then
      FParamType := Param.ParamType;
    FSize := Param.Size;
    FPrecision := Param.Precision;
    FNumericScale := SmallInt(Param.NumericScale);
  end;
end;

procedure TZParam.AssignParam(Param: TZParam);
begin
  FData := Param.FData;
  FArraySize := Param.FArraySize;
  FSQLType := Param.FSQLType;
  FSQLDataType := Param.FSQLDataType;
  FZVariantType := Param.FZVariantType;
  FPrecision := Param.FPrecision;
  FNumericScale := Param.FNumericScale;
  FNull := Param.FNull;
  FDataType := Param.FDataType;
  if FParamType = ptUnknown then
    FParamType := Param.FParamType;
  FDecimalSeperator := Param.FDecimalSeperator;
  FSize := Param.FSize;
  FName := Param.FName;
  if (Param.FArraySize = 0) and not FNull and (Ord(FSQLDataType) >= Ord(stString)) then begin
    { inc the refcounts }
    FData.pvPointer := nil; //avoid gpf
    case SQLType of //handle refcounted variables
      stString: RawByteString(FData.pvPointer) := RawByteString(Param.FData.pvPointer);
      stUnicodeString: UnicodeString(FData.pvPointer) := UnicodeString(Param.FData.pvPointer);
      stBytes: TBytes(FData.pvPointer) := TBytes(Param.FData.pvPointer);
      stAsciiStream, stUnicodeStream: IZCLob(FData.pvPointer) := IZCLob(Param.FData.pvPointer);
      stBinaryStream: IZBLob(FData.pvPointer) := IZBLob(Param.FData.pvPointer);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end else if (Param.FArraySize > 0) then begin//increment the dyn array refcounts
    FData.pvDynArray.VArray := nil; //avoid gpf
    TByteDynArray(FData.pvDynArray.VArray) := TByteDynArray(FData.pvDynArray.VArray); //all dyn array have a refcount, elementsize etc. So know the type isn't necessary
    FData.pvPointer := nil; //avoid gpf
    TBooleanDynArray(FData.pvDynArray.VIsNullArray) := TBooleanDynArray(FData.pvDynArray.VIsNullArray); //all dyn array have a refcount, elementsize etc. So know the type isn't necessary
  end;
end;

procedure TZParam.AssignTo(Dest: TPersistent);
  procedure AssignRtlParam;
  begin
    With (Dest as TParam) do begin
      Value := GetAsVariant;
      DataType := FDataType;
      ParamType := FParamType;
      Precision := FPrecision;
      Size := FSize;
      NumericScale := FNumericScale;
      Name := FName;
      Bound := FBound;
      NativeStr := FNativeStr;
    end;
  end;
begin
  if Dest is TZParam then
    Dest.Assign(Self)
  else if Dest is TParam then
    AssignRtlParam
  else inherited;
end;

procedure TZParam.CheckDataIndex(Value: Integer);
begin
  if ((FArraySize = 0) and (Value >= 0)) or
     ((FArraySize > 0) and ((Value < 0) or (Cardinal(Value) > FArraySize))) then
    raise CreateIndexError(Value);
end;

procedure TZParam.CheckDataIndex(Value: Integer; SQLType: TZSQLType;
  VariantType: TZVariantType; out DataAddress: PPointer; out IsNullAddress: PBoolean);
begin
  if ((FArraySize = 0) and (Value >= 0)) or
     ((FArraySize > 0) and ((Value < 0) or (Cardinal(Value) > FArraySize))) then
    raise CreateIndexError(Value);
  if (SQLType <> FSQLDataType) then
    if (FSQLDataType = stUnknown) or ((FArraySize = 0) and FDynamicParamType) then
      SetSQLDataType(SQLType, VariantType)
    else if ((FArraySize > 0) and (FZVariantType <> VariantType)) or
            (SQLType in ZDataIncompatibilityMatrix[FSQLDataType]) then
      raise CreateConversionError(FSQLDataType, SQLType);
  if FArraySize = 0 then begin
    DataAddress := @FData.pvBool;
    IsNullAddress := @FNull;
  end else begin
    DataAddress := PPointer(PAnsiChar(FData.pvDynArray.VArray)+(Cardinal(Value)*ZDataOffsetSizes[FSQLDataType]));
    IsNullAddress := @TBooleanDynArray(FData.pvDynArray.VIsNullArray)[Cardinal(Value)];
  end;
end;

procedure TZParam.Clear;
var I: Integer;
begin
  if FArraySize > 0 then
    for i := 0 to FArraySize do
      SetIsNulls(I, True)
  else SetIsNull(True);
end;

constructor TZParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDynamicParamType := True;
  TrySetConnection;
  FNull := True;
end;

constructor TZParam.Create(Params: TZParams; ParamType: TParamType);
begin
  Create(TCollection(Params));
  FParamType := ParamType;
end;

constructor TZParam.Create(Params: TZParams; ParamType: TParamType;
  SQLType: TZSQLType; Precision, Scale: Integer);
begin
  Create(TCollection(Params));
  FSize := Precision;
  FNumericScale := Scale;
  SetSQLType(SQLType);
  FParamType := ParamType;
  if SQLType <> stUnknown then
    FDynamicParamType := False;
end;

class function TZParam.CreateConversionError(Current,
  Expected: TZSQLType): EVariantTypeCastError;
begin
  Result := EVariantTypeCastError.Create(SUnsupportedParameterType +', SQLType: '+
    TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Expected))+'/'+
    TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(Current)));
end;

class function TZParam.CreateIndexError(Value: Integer): EListError;
begin
  Result := EListError.CreateFmt(SListIndexError, [Value]);
end;

destructor TZParam.Destroy;
begin
  if FArraySize > 0 then
    SetArraySize(0);
  SetIsNull(True);
  inherited;
end;

{$IFNDEF NO_ANSISTRING}
function TZParam.GetAsAnsiString: AnsiString;
begin
  Result := GetAsAnsiStrings(Cardinal(-1));
end;

function TZParam.GetAsAnsiStrings(Index: Cardinal): AnsiString;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := ''
  else if (FSQLDataType = stString) and (FZVariantType = vtAnsiString)
    then Result := AnsiString(DataAddr^)
    else Result := GetAsRawByteStrings(Index, zOSCodePage);
end;
{$ENDIF NO_ANSISTRING}

function TZParam.GetAsBoolean: Boolean;
begin
  Result := GetAsBooleans(Cardinal(-1));
end;

function TZParam.GetAsBooleans(Index: Cardinal): Boolean;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := False
  else case FSQLDataType of
    stBoolean: Result := PBoolean(DataAddr)^;
    stByte, stShort: Result := PByte(DataAddr)^ <> 0;
    stWord, stSmall: Result := PWord(DataAddr)^ <> 0;
    stLongword, stInteger, stFloat: Result := PCardinal(DataAddr)^ <> 0;
    stLong, stULong, stDouble, stCurrency: Result := PInt64(DataAddr)^ <> 0;
    //stBigDecimal: ;
    stString: Result := StrToBoolEx(PAnsiChar(DataAddr^));
    stUnicodeString: Result := StrToBoolEx(PWideChar(DataAddr^));
    else raise CreateConversionError(FSQLDataType, stBoolean)
  end;
end;

function TZParam.GetAsByte: Byte;
begin
  Result := GetAsByteArray(Cardinal(-1));
end;

function TZParam.GetAsByteArray(Index: Cardinal): Byte;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else if FSQLDataType = stByte
    then Result := PByte(DataAddr)^
    else Result := GetAsCardinals(Index);
end;

function TZParam.GetAsBytes: {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
begin
  Result := GetAsBytesArray(Cardinal(-1));
end;

function TZParam.GetAsBytesArray(Index: Cardinal): {$IFDEF WITH_GENERICS_TFIELD_ASBYTES}TArray<Byte>{$ELSE}TBytes{$ENDIF};
var DataAddr: PPointer;
  procedure FromBlob;
  begin
    if DataAddr = @FData.pvBool then
      if FSQLDataType = stBinaryStream
      then Result := IZBlob(DataAddr^).GetBytes
      else Result := IZClob(DataAddr^).GetBytes
    else if FSQLDataType = stBinaryStream
      then Result := (IInterface(DataAddr^) as IZBlob).GetBytes
      else Result := (IInterface(DataAddr^) as IZClob).GetBytes
  end;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := nil
  else case FSQLDataType of
    stBytes: Result := TBytes(DataAddr^);
    stString: Result := BufferToBytes(DataAddr^, Length(RawByteString(DataAddr^)));
    stUnicodeString: Result := BufferToBytes(DataAddr^, Length(UnicodeString(DataAddr^)) shl 1);
    stAsciiStream, stUnicodeStream, stBinaryStream: FromBlob;
    else Result := BufferToBytes(DataAddr^, ZDataOffsetSizes[FSQLDataType]);
  end;
end;

function TZParam.GetAsCardinal: Cardinal;
begin
  Result := GetAsCardinals(Cardinal(-1));
end;

function TZParam.GetAsCardinals(Index: Cardinal): Cardinal;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean: Result := Ord(PBoolean(DataAddr)^);
    stByte: Result := PByte(DataAddr)^;
    stShort: Result := PShortInt(DataAddr)^;
    stWord: Result := PWord(DataAddr)^;
    stSmall: Result := PSmallInt(DataAddr)^;
    stLongWord: Result := PCardinal(DataAddr)^;
    stInteger: Result := PInteger(DataAddr)^;
    stULong: Result := {$IFDEF WITH_UINT64_C1118_ERROR}UInt64ToCardinal{$ENDIF}(PUInt64(DataAddr)^);
    stLong: Result := PInt64(DataAddr)^;
    stString: Result := ZFastCode.RawToUInt32(RawByteString(FData.pvPointer));
    stUnicodeString: Result := ZFastCode.UnicodeToUInt32(UnicodeString(FData.pvPointer));
    else Result := GetAsInt64s(Index);
  end;
end;

function TZParam.GetAsCurrency: Currency;
begin
  Result := GetAsCurrencys(Cardinal(-1));
end;

function TZParam.GetAsCurrencys(Index: Cardinal): Currency;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean..stLong: Result := GetAsInt64s(Index);
    stFloat: Result := PSingle(DataAddr)^;
    stDouble: Result := PDouble(DataAddr)^;
    stCurrency: Result := PCurrency(DataAddr)^;
    stBigDecimal: BCDToCurr(PBCD(DataAddr)^, Result);
    stString: SQLStrToFloatDef(PAnsiChar(DataAddr^), 0, FDecimalSeperator, Result, Length(RawByteString(DataAddr^)));
    stUnicodeString: SQLStrToFloatDef(PWideChar(DataAddr^), 0, FDecimalSeperator, Result, Length(UnicodeString(DataAddr^)));
    else raise CreateConversionError(FSQLDataType, stCurrency);
  end;
end;

function TZParam.GetAsDate: TDate;
begin
  Result := GetAsDates(Cardinal(-1));
end;

function TZParam.GetAsDates(Index: Cardinal): TDate;
var D: TZDate;
    DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else begin
    d := GetAsZDates(Index);
    if not TryDateToDateTime(D, TDateTime(Result)) then
      raise CreateConversionError(FSQLDataType, stDate);
  end;
end;

function TZParam.GetAsDateTime: TDateTime;
begin
  Result := GetAsDateTimes(Cardinal(-1));
end;

function TZParam.GetAsDateTimes(Index: Cardinal): TDateTime;
var TS: TZTimeStamp;
    DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else begin
    TS := GetAsZTimeStamps(Index);
    if not TryTimeStampToDateTime(TS, Result) then
      raise CreateConversionError(FSQLDataType, stTimestamp);
  end;
end;

function TZParam.GetAsDouble: Double;
begin
  Result := GetAsDoubles(Cardinal(-1));
end;

function TZParam.GetAsDoubles(Index: Cardinal): Double;
var DataAddr: PPointer;
label jmpFail;
  function FromCLob: Double;
  var Raw: RawByteString;
      Uni: UnicodeString;
      P: Pointer;
      PA: PAnsiChar absolute P;
      PW: PWideChar absolute P;
  begin
    if FSQLDataType = stAsciiStream then begin
      if DataAddr = @FData.pvBool
      then Raw := IZClob(DataAddr^).GetString
      else Raw := (IInterface(DataAddr^) as IZClob).GetString;
      P := Pointer(Raw);
      SQLStrToFloatDef(PA, 0, Result, Length(Raw));
    end else begin
      if DataAddr = @FData.pvBool
      then Uni := IZClob(DataAddr^).GetUnicodeString
      else Uni := (IInterface(DataAddr^) as IZClob).GetUnicodeString;
      P := Pointer(Uni);
      SQLStrToFloatDef(PW, 0, Result, Length(Uni));
    end;
  end;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean..stInteger: Result := GetAsInt64s(Index);
    stULong: Result := {$IFDEF WITH_UINT64_C1118_ERROR}UInt64ToInt64{$ENDIF}(PUInt64(DataAddr)^);
    stLong: Result := PInt64(DataAddr)^;
    stFloat: Result := PSingle(DataAddr)^;
    stDouble: Result := PDouble(DataAddr)^;
    stTime: if not TryTimeToDateTime(PZTime(DataAddr)^, TDateTime(Result)) then goto jmpFail;
    stDate: if not TryDateToDateTime(PZDate(DataAddr)^, TDateTime(Result)) then goto jmpFail;
    stTimeStamp: if not TryTimeStampToDateTime(PZTimeStamp(DataAddr)^, TDateTime(Result)) then goto jmpFail;
    stCurrency: Result := PCurrency(DataAddr)^;
    stBigDecimal: Result := BCDToDouble(PBCD(DataAddr)^);
    stString: SQLStrToFloatDef(PAnsiChar(DataAddr^), 0, Result, Length(RawByteString(DataAddr^)));
    stUnicodeString: SQLStrToFloatDef(PWideChar(DataAddr^), 0, Result, Length(UnicodeString(DataAddr^)));
    stAsciiStream, stUnicodeStream: Result := FromCLob;
    else
jmpFail: raise CreateConversionError(FSQLDataType, stDouble)
  end;
end;

function TZParam.GetAsFmtBCD: TBCD;
begin
  Result := GetAsFmtBCDs(Cardinal(-1));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function result variable does not seem to be set} {$ENDIF}
function TZParam.GetAsFmtBCDs(Index: Cardinal): TBCD;
var DataAddr: PPointer;
label jmpFail;
  function FromCLob: TBCD;
  var Raw: RawByteString;
      Uni: UnicodeString;
      P: Pointer;
      PA: PAnsiChar absolute P;
      PW: PWideChar absolute P;
  begin
    if FSQLDataType = stAsciiStream then begin
      if DataAddr = @FData.pvBool
      then Raw := IZClob(DataAddr^).GetString
      else Raw := (IInterface(DataAddr^) as IZClob).GetString;
      P := Pointer(Raw);
      Result := RawToBCD(PA, Length(Raw));
    end else begin
      if DataAddr = @FData.pvBool
      then Uni := IZClob(DataAddr^).GetUnicodeString
      else Uni := (IInterface(DataAddr^) as IZClob).GetUnicodeString;
      P := Pointer(Uni);
      Result := UniToBCD(PW, Length(Uni));
    end;
  end;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Fillchar(Result, SizeOf(TBCD), #0)
  else case FSQLDataType of
    stBoolean..stInteger, stLong: ScaledOrdinal2Bcd(GetAsInt64s(Index), 0, Result);
    stULong: ScaledOrdinal2Bcd(PUInt64(DataAddr)^, 0, Result, False);
    stFloat, stDouble, stTime, stDate, stTimeStamp: Double2BCD(GetAsDoubles(Index), Result);
    stString: Result := RawToBCD(RawByteString(DataAddr^));
    stUnicodeString: Result := UniToBCD(UnicodeString(DataAddr^));
    stAsciiStream, stUnicodeStream: Result := FromCLob;
    else
jmpFail: raise CreateConversionError(FSQLDataType, stBigDecimal)
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZParam.GetAsGUID: TGUID;
begin
  Result := GetAsGUIDs(Cardinal(-1));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function result variable does not seem to be set} {$ENDIF}
function TZParam.GetAsGUIDs(Index: Cardinal): TGUID;
var L: LengthInt;
    DataAddr: PPointer;
label jmpFail;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then FillChar(Result, SizeOf(TGUID), #0)
  else case FSQLDataType of
    stGUID: Result := PGUID(DataAddr)^;
    stBytes: begin
        L := Length(TBytes(DataAddr^));
        if (L = SizeOf(TGUID))
        then Result := PGUID(DataAddr^)^
        else goto jmpFail;
      end;
    stString: begin
        L := Length(RawByteString(DataAddr^));
        if (L = 36) or (L = 38) then
          ValidGUIDToBinary(PAnsiChar(DataAddr^), @Result.D1)
        else goto jmpFail;
      end;
    stUnicodeString: begin
        L := Length(UnicodeString(DataAddr^));
        if (L = 36) or (L = 38) then
          ValidGUIDToBinary(PWideChar(DataAddr^), @Result.D1)
        else goto jmpFail;
      end;
    else
jmpFail: raise CreateConversionError(FSQLDataType, stGUID)
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZParam.GetAsInt64: Int64;
begin
  Result := GetAsInt64s(Cardinal(-1));
end;

function TZParam.GetAsInt64s(Index: Cardinal): Int64;
var DataAddr: PPointer;
  function FromBCD: Int64;
  var BCD: TBCD;
      Prec: Word;
  begin
    BCD := PBCD(DataAddr)^;
    ZRoundBCD(BCD, 0, Prec);
    Result := BCD2Int64(BCD);
  end;
  function FromCurrency: Int64;
  var C: Currency;
      I64: Int64 absolute C;
  begin
    C := ZSysUtils.RoundCurrTo(PCurrency(DataAddr)^, 0);
    Result := i64 div 10000;
  end;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean: Result := Ord(PBoolean(DataAddr)^);
    stByte: Result := PByte(DataAddr)^;
    stShort: Result := PShortInt(DataAddr)^;
    stWord: Result := PWord(DataAddr)^;
    stSmall: Result := PSmallInt(DataAddr)^;
    stLongWord: Result := PCardinal(DataAddr)^;
    stInteger: Result := PInteger(DataAddr)^;
    stULong: Result := {$IFDEF WITH_UINT64_C1118_ERROR}UInt64ToInt64{$ENDIF}(PUInt64(DataAddr)^);
    stLong: Result := PInt64(DataAddr)^;
    stFloat: Result := Trunc(PSingle(DataAddr)^);
    stDouble: Result := Trunc(PDouble(DataAddr)^);
    stCurrency: Result := FromCurrency;
    stBigDecimal: Result := FromBCD;
    stString: Result := ZFastCode.RawToInt64(RawByteString(DataAddr^));
    stUnicodeString: Result := ZFastCode.UnicodeToInt64(UnicodeString(DataAddr^));
    else raise CreateConversionError(FSQLDataType, stLong)
  end;
end;

function TZParam.GetAsInteger: Integer;
begin
  Result := GetAsIntegers(Cardinal(-1));
end;

function TZParam.GetAsIntegers(Index: Cardinal): Integer;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean: Result := Ord(FData.pvBool);
    stByte: Result := FData.pvByte;
    stShort: Result := FData.pvShortInt;
    stWord: Result := FData.pvWord;
    stSmall: Result := FData.pvSmallInt;
    stLongWord: Result := FData.pvCardinal;
    stInteger: Result := FData.pvInteger;
    stString: Result := ZFastCode.RawToInt(RawByteString(FData.pvPointer));
    stUnicodeString: Result := ZFastCode.UnicodeToInt(UnicodeString(FData.pvPointer));
    else Result := GetAsInt64s(Index);
  end;
end;

function TZParam.GetAsRawByteString(CodePage: Word): RawByteString;
begin
  Result := GetAsRawByteStrings(Cardinal(-1), CodePage);
end;

function TZParam.GetAsRawByteStrings(Index: Cardinal;
  CodePage: Word): RawByteString;
var Len: LengthInt;
    DataAddr: PPointer;
    CP: Word absolute Len;
    TinyBuffer: array[0..MaxFMTBcdFractionSize+2] of AnsiChar;
    PEnd: PAnsiChar;
    procedure FromCLob;
    begin
      if DataAddr = @FData.pvBool
      then Result := IZClob(DataAddr^).GetRawByteString(CodePage)
      else Result := (IInterface(DataAddr^) as IZClob).GetRawByteString(CodePage);
    end;
    procedure FromBLob;
    begin
      if DataAddr = @FData.pvBool
      then Result := IZBlob(DataAddr^).GetString
      else Result := (IInterface(DataAddr^) as IZBlob).GetString;
    end;
label jmpLenFromPEnd, jmpSetFromBuf;
begin
  Result := '';
  if not GetIsNullsAddr(Integer(Index), DataAddr) then case FSQLDataType of
    stBoolean: Result := BoolStrsRaw[PBoolean(DataAddr)^];
    stByte:     begin
                  IntToRaw(Cardinal(PByte(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stShort:    begin
                  IntToRaw(Integer(PShortInt(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stWord:     begin
                  IntToRaw(Cardinal(PWord(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stSmall:    begin
                  IntToRaw(Integer(PSmallInt(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stLongWord: begin
                  IntToRaw(PCardinal(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stInteger:  begin
                  IntToRaw(PInteger(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stULong:    begin
                  IntToRaw(PUInt64(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stLong:     begin
                  IntToRaw(PInt64(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stFloat:    begin
                  Len := FloatToRaw(PSingle(DataAddr)^, @TinyBuffer[0]);
                  goto jmpSetFromBuf;
                end;
    stDouble:   begin
                  Len := FloatToRaw(PDouble(DataAddr)^, @TinyBuffer[0]);
                  Goto jmpSetFromBuf;
                end;
    stCurrency: begin
                  CurrToRaw(PCurrency(DataAddr)^, FDecimalSeperator, @TinyBuffer[0], @PEnd);
jmpLenFromPEnd:   Len := PEnd - PAnsiChar(@TinyBuffer[0]);
                  goto jmpSetFromBuf;
                end;
    stBigDecimal: begin
                  Len := BcdToRaw(PBCD(DataAddr)^, @TinyBuffer[0], FDecimalSeperator);
                  goto jmpSetFromBuf;
                end;
    stString:   begin
                  if DataAddr^ = nil then Exit;
                  {$IFNDEF NO_ANSISTRING}
                  if FZVariantType = vtAnsiString then
                    CP := ZOSCodePage
                  else {$ENDIF}if FZVariantType = vtUTF8String then
                    CP := zCP_UTF8
                  else CP := GetDefaultRawCP;
                  if CP <> CodePage then begin
                    Len := Length(RawByteString(DataAddr^));
                    ZEncoding.PRawToRawConvert(DataAddr^, Len, CP, CodePage, Result)
                  end else Result := RawByteString(DataAddr^)
                end;
    stUnicodeString: Result := ZUnicodeToRaw(UnicodeString(DataAddr^), CodePage);
    stBytes:    if DataAddr^ = nil
                then Exit
                {$IFDEF WITH_RAWBYTESTRING}
                else ZSetString(PAnsiChar(DataAddr^), Length(TBytes(DataAddr^)), Result, 0);
                {$ELSE}
                else System.SetString(Result, PAnsiChar(DataAddr^), Length(TBytes(DataAddr^)));
                {$ENDIF}
    stGUID:     begin
                  GUIDToBuffer(@PGUID(DataAddr)^.D1, PAnsiChar(@TinyBuffer[0]), [guidWithBrackets]);
                  Len := 38;
                  goto jmpSetFromBuf;
                end;
    stDate:     begin
                  with PZDate(DataAddr)^ do
                  Len := DateToRaw(Year, Month, Day, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, False, IsNegative);
                  goto jmpSetFromBuf;
                end;
    stTime:     begin
                  with PZTime(DataAddr)^ do
                  Len := TimeToRaw(Hour, Minute, Second, Fractions, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, IsNegative);
                  goto jmpSetFromBuf;
                end;
    stTimestamp:begin
                  with PZTimeStamp(DataAddr)^ do
                  Len := DateTimeToRaw(Year, Month, Day, Hour, Minute,
                    Second, Fractions, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, False, IsNegative);
jmpSetFromBuf:    {$IFDEF WITH_RAWBYTESTRING}
                  ZSetString(PAnsiChar(@TinyBuffer[0]), Len, Result, CodePage);
                  {$ELSE}
                  System.SetString(Result, PAnsiChar(@TinyBuffer[0]), Len);
                  {$ENDIF}
                end;
    stAsciiStream, stUnicodeStream: FromCLob;
    stBinaryStream: FromBLob;
    else raise CreateConversionError(FSQLDataType, stString);
  end;
end;

function TZParam.GetAsShortInt: ShortInt;
begin
  Result := GetAsShortInts(Cardinal(-1));
end;

function TZParam.GetAsShortInts(Index: Cardinal): ShortInt;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else if FSQLDataType = stShort
    then Result := PShortInt(DataAddr)^
    else Result := GetAsIntegers(Index);
end;

function TZParam.GetAsSingle: Single;
begin
  Result := GetAsSingles(Cardinal(-1));
end;

function TZParam.GetAsSingles(Index: Cardinal): Single;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else if FSQLDataType = stFloat
    then Result := PSingle(DataAddr)^
    else Result := GetAsDoubles(Index);
end;

function TZParam.GetAsSmallInt: SmallInt;
begin
  Result := GetAsSmallInts(Cardinal(-1));
end;

function TZParam.GetAsSmallInts(Index: Cardinal): SmallInt;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else if FSQLDataType = stSmall
    then Result := PSmallInt(DataAddr)^
    else Result := GetAsIntegers(Index);
end;

{$IFNDEF UNICODE}
function TZParam.GetAsString: String;
begin
  Result := GetAsRawByteStrings(Cardinal(-1), GetDefaultRawCP);
end;

function TZParam.GetAsStrings(Index: Cardinal): String;
begin
  Result := GetAsRawByteStrings(Index, GetDefaultRawCP);
end;
{$ENDIF}

function TZParam.GetAsTime: TTime;
begin
  Result := GetAsTimes(Cardinal(-1));
end;

function TZParam.GetAsTimes(Index: Cardinal): TTime;
var T: TZTime;
    DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else begin
    T := GetAsZTimes(Index);
    if not TryTimeToDateTime(T, TDateTime(Result)) then
      raise CreateConversionError(FSQLDataType, stTime);
  end;
end;

function TZParam.GetIsNull: Boolean;
begin
  if FArraySize = 0
  then Result := FNull
  else raise CreateConversionError(stArray, FSQLDataType);
end;

function TZParam.GetIsNullsAddr(Index: Integer; out DataAddress: PPointer): Boolean;
begin
  if ((FArraySize = 0) and (Index >= 0)) or
     ((FArraySize > 0) and ((Index < 0) or (Cardinal(Index) > FArraySize))) then
    raise CreateIndexError(Index);
  if FArraySize = 0 then begin
    DataAddress := @FData.pvBool;
    Result := FNull;
  end else begin
    DataAddress := Pointer(PAnsiChar(FData.pvDynArray.VArray)+(Cardinal(Index)*ZDataOffsetSizes[FSQLDataType]));
    Result := TBooleanDynArray(FData.pvDynArray.VIsNullArray)[Cardinal(Index)];
  end;
end;

function TZParam.GetIsNulls(Index: Cardinal): Boolean;
begin
  if (FArraySize > 0) and (Index <= FArraySize)
  then Result := TBooleanDynArray(FData.pvDynArray.VArray)[Index]
  else if (Index > FArraySize)
    then raise CreateIndexError(Index)
    else raise CreateConversionError(FSQLDataType, stArray);
end;

function TZParam.GetAsUInt64: UInt64;
begin
  Result := GetAsUInt64s(Cardinal(-1));
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZParam.GetAsUInt64s(Index: Cardinal): UInt64;
var DataAddr: PPointer;
  function FromBCD: UInt64;
  var BCD: TBCD;
      Prec: Word;
  begin
    BCD := PBCD(DataAddr)^;
    ZRoundBCD(BCD, 0, Prec);
    Result := BCD2UInt64(BCD);
  end;
  function FromCurrency: Int64;
  var C: Currency;
      I64: Int64 absolute C;
  begin
    C := ZSysUtils.RoundCurrTo(PCurrency(DataAddr)^, 0);
    Result := i64 div 10000;
  end;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else case FSQLDataType of
    stBoolean: Result := Ord(PBoolean(DataAddr)^);
    stByte: Result := PByte(DataAddr)^;
    stShort: Result := PShortInt(DataAddr)^;
    stWord: Result := PWord(DataAddr)^;
    stSmall: Result := PSmallInt(DataAddr)^;
    stLongWord: Result := PCardinal(DataAddr)^;
    stInteger: Result := PInteger(DataAddr)^;
    stULong: Result := PUInt64(DataAddr)^;
    stLong: Result :=  {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(PInt64(DataAddr)^);
    stFloat: Result :=  {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(Trunc(PSingle(DataAddr)^));
    stDouble: Result :=  {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(Trunc(PDouble(DataAddr)^));
    stCurrency: Result :=  {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(FromCurrency);
    stBigDecimal: Result :=  FromBCD;
    stString: Result := ZFastCode.RawToUInt64(RawByteString(DataAddr^));
    stUnicodeString: Result := ZFastCode.UnicodeToUInt64(UnicodeString(DataAddr^));
    else raise CreateConversionError(FSQLDataType, stULong)
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZParam.GetAsUnicodeString: UnicodeString;
begin
  Result := GetAsUnicodeStrings(Cardinal(-1));
end;

function TZParam.GetAsUnicodeStrings(Index: Cardinal): UnicodeString;
var Len: LengthInt;
    CP: Word absolute Len;
    TinyBuffer: array[0..MaxFMTBcdFractionSize+2] of WideChar;
    PEnd: PWideChar;
    DataAddr: PPointer;
    procedure FromCLob;
    begin
      if DataAddr = @FData.pvBool
      then Result := IZClob(DataAddr^).GetUnicodeString
      else Result := (IInterface(DataAddr^) as IZCLob).GetUnicodeString
    end;

label jmpLenFromPEnd, jmpSetFromBuf;
begin
  Result := '';
  if not GetIsNullsAddr(Integer(Index), DataAddr) then case FSQLDataType of
    stBoolean: Result := BoolStrsW[PBoolean(DataAddr)^];
    stByte:     begin
                  IntToUnicode(Cardinal(PByte(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stShort:    begin
                  IntToUnicode(Integer(PShortInt(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stWord:     begin
                  IntToUnicode(Cardinal(PWord(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stSmall:    begin
                  IntToUnicode(Integer(PSmallInt(DataAddr)^), @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stLongWord: begin
                  IntToUnicode(PCardinal(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stInteger:  begin
                  IntToUnicode(PInteger(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stULong:    begin
                  IntToUnicode(PUInt64(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stLong:     begin
                  IntToUnicode(PInt64(DataAddr)^, @TinyBuffer[0], @PEnd);
                  goto jmpLenFromPEnd;
                end;
    stFloat:    begin
                  Len := FloatToUnicode(PSingle(DataAddr)^, @TinyBuffer[0]);
                  goto jmpSetFromBuf;
                end;
    stDouble:   begin
                  Len := FloatToUnicode(PDouble(DataAddr)^, @TinyBuffer[0]);
                  Goto jmpSetFromBuf;
                end;
    stCurrency: begin
                  CurrToUnicode(PCurrency(DataAddr)^, FDecimalSeperator, @TinyBuffer[0], @PEnd);
jmpLenFromPEnd:   Len := PEnd - PWideChar(@TinyBuffer[0]);
                  goto jmpSetFromBuf;
                end;
    stBigDecimal: begin
                  Len := BcdToUni(PBCD(DataAddr)^, @TinyBuffer[0], FDecimalSeperator);
                  goto jmpSetFromBuf;
                end;
    stString:   begin
                  {$IFNDEF NO_ANSISTRING}
                  if FZVariantType = vtAnsiString then
                    CP := ZOSCodePage
                  else {$ENDIF}if FZVariantType = vtUTF8String then
                    CP := zCP_UTF8
                  else CP := GetDefaultRawCP;
                  Result := ZRawToUnicode(RawByteString(DataAddr^), CP);
                end;
    stUnicodeString: Result := UnicodeString(DataAddr^);
    //stBytes:
    stGUID:     begin
                  GUIDToBuffer(@PGUID(DataAddr)^.D1, PWideChar(@TinyBuffer[0]), [guidWithBrackets]);
                  Len := 38;
                  goto jmpSetFromBuf;
                end;
    stDate:     with PZDate(DataAddr)^ do begin
                  Len := DateToUni(Year, Month, Day, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, False, IsNegative);
                  goto jmpSetFromBuf;
                end;
    stTime:     with PZTime(DataAddr)^ do begin
                  Len := TimeToUni(Hour, Minute, Second, Fractions, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, IsNegative);
                  goto jmpSetFromBuf;
                end;
    stTimestamp:with PZTimestamp(DataAddr)^ do begin
                  Len := DateTimeToUni(Year, Month, Day, Hour, Minute, Second,
                    Fractions, @TinyBuffer[0],
                    {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, False, IsNegative);
jmpSetFromBuf:    System.SetString(Result, PWideChar(@TinyBuffer[0]), Len);
                end;
    stAsciiStream, stUnicodeStream: FromCLob;
    else raise CreateConversionError(FSQLDataType, stUnicodeString);
  end;
end;

function TZParam.GetAsUTF8String: UTF8String;
begin
  Result := GetAsUTF8Strings(Cardinal(-1));
end;

function TZParam.GetAsUTF8Strings(Index: Cardinal): UTF8String;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := ''
  else if (FSQLDataType = stString) and (FZVariantType = vtUTF8String)
    then Result := UTF8String(DataAddr^)
    else Result := GetAsRawByteStrings(Index, zCP_UTF8)
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5093 off : Function result variable does not seem to be initialized}
  {$WARN 5094 off : Function result variable does not seem to be initialized}
  {$IFDEF WITH_NOT_INLINED_WARNING}{$WARN 6058 off : Call to subroutine "operator:=(const source:UnicodeString):Variant" marked as inline is not inlined}{$ENDIF}
{$ENDIF}
function TZParam.GetAsVariant: Variant;
  {$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const source:UTF8String):Variant" marked as inline is not inlined}{$ENDIF}
  procedure SetAsRawString(var Result: Variant);
  begin
    {$IF defined(LCL) or defined(NO_ANSISTRING)}
    Result := GetAsUTF8String;
    {$ELSE}
    if ({$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF} = zCP_UTF8) then
      Result := GetAsUTF8String
    else if (FZVariantType = vtUTF8String) then
      Result := GetAsUnicodeString
    else if FZVariantType <> vtAnsiString then
      Result := GetAsUnicodeString
    else
      Result := GetAsAnsiString;
    {$IFEND}
  end;
  {$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}
  {$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const source:UnicodeString):Variant" marked as inline is not inlined}{$ENDIF}
  procedure SetAsUniStringFromClob(var Result: Variant);
  begin
    Result := GetAsUnicodeString;
  end;
  {$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}
  procedure SetAsBytes(var Result: Variant);
  begin
    Result := BytesToVar(GetAsBytes)
  end;
begin
  Result := null; //VarInit(Result) changed (let the compiler do the quirk)-> see https://zeoslib.sourceforge.io/viewtopic.php?f=50&p=162991
  If not GetIsNull then
    case FSQLDataType of
      stBoolean:     begin
                        TVarData(Result).VType := varBoolean;
                        TVarData(Result).VBoolean := FData.pvBool;
                      end;
      stByte:         begin
                        TVarData(Result).VType := varByte;
                        TVarData(Result).VByte := FData.pvByte;
                      end;
      stShort:        begin
                        TVarData(Result).VType := varShortInt;
                        TVarData(Result).VShortInt := FData.pvShortInt;
                      end;
      stWord:         begin
                        TVarData(Result).VType := varWord;
                        TVarData(Result).VWord := FData.pvWord;
                      end;
      stSmall:        begin
                        TVarData(Result).VType := varSmallint;
                        TVarData(Result).VSmallInt := FData.pvSmallInt;
                      end;
      stLongWord:     begin
                        {$IF Declared(varUInt32)}
                        TVarData(Result).VType := varUInt32;
                        TVarData(Result).VUInt32 := FData.pvCardinal;
                        {$ELSE}
                        TVarData(Result).VType := varLongWord;
                        TVarData(Result).VLongWord := FData.pvCardinal;
                        {$IFEND}
                      end;
      stInteger:      begin
                        TVarData(Result).VType := varInteger;
                        TVarData(Result).VInteger := FData.pvInteger;
                      end;
      stULong:        begin

                        {$IF Declared(varword64)}
                        TVarData(Result).VType := varQWord;
                        TVarData(Result).vQWord := FData.pvUInt64;
                        {$ELSE}
                          {$IF Declared(varUInt64)}
                        TVarData(Result).VType := varUInt64;
                        TVarData(Result).VUInt64 := FData.pvUInt64;
                          {$ELSE}
                        TVarData(Result).VType := varInt64;
                        TVarData(Result).VInt64 := FData.pvUInt64;
                          {$IFEND}
                        {$IFEND}
                      end;
      stLong:         begin
                        TVarData(Result).VType := varInt64;
                        TVarData(Result).VInt64 := FData.pvInt64;
                      end;
      stFloat:        begin
                        TVarData(Result).VType := varSingle;
                        TVarData(Result).VSingle := FData.pvSingle;
                      end;
      stDouble:       begin
                        TVarData(Result).VType := varDouble;
                        TVarData(Result).VDouble := FData.pvDouble;
                      end;
      stCurrency:    begin
                        TVarData(Result).VType := varCurrency;
                        TVarData(Result).VCurrency := FData.pvCurrency;
                      end;
      stBigDecimal:   VarFMTBcdCreate(Result, FData.pvBCD);
      stTime:         begin
                        TVarData(Result).VType := varDate;
                        if not ZSysUtils.TryTimeToDateTime(FData.pvTime, TVarData(Result).VDate) then
                          raise EVariantError.Create('time-Variant overflow');
                      end;
      stDate:         begin
                        TVarData(Result).VType := varDate;
                        if not ZSysUtils.TryDateToDateTime(FData.pvDate, TVarData(Result).VDate) then
                          raise EVariantError.Create('date-Variant overflow');
                      end;
      stTimeStamp:    begin
                        TVarData(Result).VType := varDate;
                        if not ZSysUtils.TryTimeStampToDateTime(FData.pvTimeStamp, TVarData(Result).VDate) then
                          raise EVariantError.Create('datetime-Variant overflow');
                      end;
      stGUID:         begin
                        TVarData(Result).VType := varStrArg;
                        UnicodeString(TVarData(Result).VAny) := GetAsUnicodeString;
                      end;
      stString,
      stAsciiStream:  SetAsRawString(Result);
      stUnicodeString:Result := UnicodeString(FData.pvPointer);
      stUnicodeStream:SetAsUniStringFromClob(Result);
      stBytes,
      stBinaryStream: SetAsBytes(Result);
      else raise EVariantError.Create('Unkown Variant type');
    end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZParam.GetAsWord: Word;
begin
  Result := GetAsWords(Cardinal(-1));
end;

function TZParam.GetAsWords(Index: Cardinal): Word;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Integer(Index), DataAddr)
  then Result := 0
  else if FSQLDataType = stWord
    then Result := PWord(DataAddr)^
    else Result := GetAsCardinals(Index);
end;

function TZParam.GetAsZDate: TZDate;
begin
  Result := GetAsZDates(Cardinal(-1));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function result variable does not seem to be initialized} {$ENDIF}
function TZParam.GetAsZDates(Index: Cardinal): TZDate;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Index, DataAddr) or (FSQLDataType = stTime)
  then FillChar(Result, SizeOf(TZDate), #0)
  else if FSQLDataType = stDate then
    Result := PZDate(DataAddr)^
  else if FSQLDataType = stTimeStamp then
    DateFromTimeStamp(PZTimeStamp(DataAddr)^, Result)
  else if FSQLDataType = stString then begin
    if not TryRawToDate(DataAddr^, Length(RawByteString(DataAddr^)),
       {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, Result) then
      raise CreateConversionError(stString, stDate);
  end else if FSQLDataType = stUnicodeString then begin
    if not TryUniToDate(DataAddr^, Length(UnicodeString(DataAddr^)),
       {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, Result) then
      raise CreateConversionError(stUnicodeString, stDate);
  end else DecodeDateTimeToDate(GetAsDoubles(Index), Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZParam.GetAsZTime: TZTime;
begin
  Result := GetAsZTimes(Cardinal(-1));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function result variable does not seem to be initialized} {$ENDIF}
function TZParam.GetAsZTimes(Index: Cardinal): TZTime;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Index, DataAddr) or (FSQLDataType = stDate)
  then FillChar(Result, SizeOf(TZTime), #0)
  else if FSQLDataType = stTime then
    Result := PZTime(DataAddr)^
  else if FSQLDataType = stTimeStamp then
    TimeFromTimestamp(PZTimestamp(DataAddr)^, Result)
  else if FSQLDataType = stString then begin
    if not TryRawToTime(DataAddr^, Length(RawByteString(DataAddr^)),
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, Result) then
        raise CreateConversionError(stString, stTime);
  end else if FSQLDataType = stUnicodeString then begin
    if not TryUniToTime(DataAddr^, Length(UnicodeString(DataAddr^)),
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, Result) then
        raise CreateConversionError(stUnicodeString, stTime);
  end else DecodeDateTimeToTime(GetAsDoubles(Index), Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZParam.GetAsZTimestamp: TZTimestamp;
begin
  Result := GetAsZTimestamps(Cardinal(-1));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5060 off : Function result variable does not seem to be initialized} {$ENDIF}
function TZParam.GetAsZTimestamps(Index: Cardinal): TZTimestamp;
var DataAddr: PPointer;
begin
  if GetIsNullsAddr(Index, DataAddr)
  then FillChar(Result, SizeOf(TZTimestamp), #0)
  else if FSQLDataType = stTimeStamp then
    Result := PZTimestamp(DataAddr)^
  else if FSQLDataType = stDate then
    TimeStampFromDate(PZDate(DataAddr)^, Result)
  else if FSQLDataType = stTime then
    TimeStampFromTime(PZTime(DataAddr)^, Result)
  else if FSQLDataType = stString then begin
    if not TryRawToTimestamp(DataAddr^, Length(RawByteString(DataAddr^)),
       {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, Result) then
      raise CreateConversionError(stString, stTimestamp);
  end else if FSQLDataType = stUnicodeString then begin
    if not TryUniToTimestamp(DataAddr^, Length(UnicodeString(DataAddr^)),
       {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, Result) then
      raise CreateConversionError(stUnicodeString, stTimestamp);
  end else DecodeDateTimeToTimestamp(GetAsDoubles(Index), Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZParam.GetData(Buffer: Pointer);
  procedure MoveRawByteString;
  var {$IF defined(LCL) or defined(NO_ANSISTRING)}
      Tmp: UTF8String;
      {$ELSE}
      Tmp: AnsiString;
      {$IFEND}
      P: Pointer absolute Tmp;
      L: LengthInt;
  begin
    {$IF defined(LCL) or defined(NO_ANSISTRING)}
    Tmp := GetAsUTF8String;
    {$ELSE}
    Tmp := GetAsAnsiString;
    {$IFEND}
    L := Length(Tmp);
    Move(P^, Buffer^, L);
    PByte(PAnsiChar(Buffer)+L)^ := 0;
  end;
  procedure MoveUniocdeString;
  var Tmp: UnicodeString;
      P: Pointer absolute Tmp;
      L: LengthInt;
  begin
    Tmp := GetAsUnicodeString;
    L := Length(Tmp) shl 1;
    Move(P^, Buffer^, L);
    PWord(PAnsiChar(Buffer)+L)^ := 0;
  end;
  procedure MoveBytes;
  var Tmp: TBytes;
      P: Pointer absolute Tmp;
      L: LengthInt;
  begin
    Tmp := GetAsBytes;
    L := Length(Tmp);
    Move(P^, Buffer^, L);
  end;
begin
  case FDataType of
    //ftUnknown: ;
    ftString, ftFixedChar, ftMemo, ftAdt: MoveRawByteString;
    ftSmallint: PSmallInt(Buffer)^  := GetAsSmallInt;
    ftAutoInc,
    ftInteger:  PInteger(Buffer)^   := GetAsInteger;
    ftWord:     PWord(Buffer)^      := GetAsWord;
    ftBoolean:  PWordBool(Buffer)^  := GetAsBoolean;
    ftFloat,
    ftCurrency: PDouble(Buffer)^    := GetAsDouble;
    ftBCD:      PCurrency(Buffer)^ := GetAsCurrency;
    ftDate:     PInteger(Buffer)^ := Trunc(GetAsDate - D1M1Y1 + 1);
    ftTime:     PInteger(Buffer)^ := Trunc(GetAsTime * MSecsOfDay + 0.1);
    ftDateTime: PDateTime(Buffer)^ := TimeStampToMSecs(DateTimeToTimeStamp(GetAsDateTime));
    ftBytes, ftVarBytes{, ftStream}, ftBlob, ftGraphic..ftTypedBinary, ftOraBlob,
    ftOraClob:  MoveBytes;
    ftCursor, ftArray, ftReference, ftDataSet{, ftObject,
    ftParams}:   {Nothing};
    ftWideString{$IFDEF WITH_WIDEMEMO}, ftFixedWideChar, ftWideMemo{$ENDIF}: MoveUniocdeString;
    ftLargeint: PInt64(Buffer)^ := GetAsInt64;
    ftVariant: PVariant(Buffer)^ := GetAsVariant;
    ftGuid:     PGUID(Buffer)^ := GetAsGUID;
    ftTimeStamp: ;
    ftFMTBcd:  PBCD(Buffer)^ := GetAsFmtBCD;
    {ftOraTimeStamp: ;
    ftOraInterval: ;}
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord: PCardinal(Buffer)^ := GetAsCardinal;
    {$ENDIF WITH_FTLONGWORD}
    {$IFDEF WITH_FTSHORTINT}
    ftShortint: PShortInt(Buffer)^ := GetAsShortInt;
    {$ENDIF WITH_FTSHORTINT}
    {$IFDEF WITH_FTBYTE}
    ftByte:     PByte(Buffer)^ := GetAsByte;
    {$ENDIF WITH_FTBYTE}
    {$IFDEF WITH_FTEXTENDED}
    DB.ftExtended: PExtended(Buffer)^ := GetAsDouble;
    {$ENDIF WITH_FTEXTENDED}
    //ftTimeStampOffset: ;
    {$IFDEF WITH_FTSINGLE}
    DB.ftSingle:   PSingle(Buffer)^ := GetAsSingle;
    {$ENDIF}
    else raise CreateConversionError(FSQLType, FSQLDataType);
  end;
end;

function TZParam.GetDefaultRawCP: Word;
begin
  if ((fConnection <> nil) or TrySetConnection) and TZAbstractConnection(fConnection).Connected
  then Result := TZAbstractConnection(fConnection).RawCharacterTransliterateOptions.GetRawTransliterateCodePage(ttParam)
  else Result := {$IFDEF LCL}ZCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}
end;

procedure TZParam.InternalSetAsRawByteString(DataAddr: PPointer;
  IsNullAddr: PBoolean; const Value: RawByteString; CodePage: Word);
var P: PAnsiChar;
    L: LengthInt;
    DestCP: Word;
  procedure ConvertRawToCLobVariable(const Value: RawByteString; CodePage: Word);
  var Lob: IZCLob;
      P: PAnsiChar;
  begin
    TrySetConnection;
    lob := TZLocalMemCLob.Create(CodePage, FConSettings);
    P := Pointer(Value);
    if P = nil then
      P := PEmptyAnsiString;
    lob.SetPAnsiChar(P, CodePage, Length(Value));
    if IsNullAddr = @FNull
    then IZClob(DataAddr^) := lob
    else IInterface(DataAddr^) := lob
  end;
  procedure ConvertRawToBLobVariable(const Value: RawByteString);
  var Lob: IZBLob;
      P: PAnsiChar;
  begin
    TrySetConnection;
    lob := TZLocalMemBLob.Create();
    P := Pointer(Value);
    if P = nil then
      P := PEmptyAnsiString;
    lob.SetBuffer(P, Length(Value));
    if IsNullAddr = @FNull
    then IZBlob(DataAddr^) := lob
    else IInterface(DataAddr^) := lob
  end;
label jmpFail;
begin
  P := Pointer(Value);
  if P = nil then begin
    P := PEmptyAnsiString;
    L := 0;
  end else
    L := Length(Value);
  if (FSQLDataType = stString) then
      if ((FConSettings <> nil) or SetConsettings) and (FConSettings.ClientCodePage.Encoding = ceUTF16) then begin
      SetIsNull(True);
      FSQLDataType := stUnicodeString;
      FZVariantType := vtUnicodeString;
    end else if CodePage = zCP_UTF8
      then FZVariantType := vtUTF8String
      else {$IFNDEF NO_ANSISTRING}if CodePage = ZOSCodePage
        then FZVariantType := vtAnsiString
        else {$ENDIF}FZVariantType := vtRawByteString;
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := StrToBoolEx(P, P+L);
    stByte: PByte(DataAddr)^ := ZFastCode.RawToUInt32(P, P+L);
    stWord: PWord(DataAddr)^ := ZFastCode.RawToUInt32(P, P+L);
    stLongWord: PCardinal(DataAddr)^ := ZFastCode.RawToUInt32(P, P+L);
    stShort: PShortInt(DataAddr)^ := ZFastCode.RawToInt(P);
    stSmall: PSmallInt(DataAddr)^ := ZFastCode.RawToInt(P);
    stInteger: PInteger(DataAddr)^ := ZFastCode.RawToInt(P);
    stLong: PInt64(DataAddr)^ := RawToInt64(Value);
    stULong: PUInt64(DataAddr)^ := RawToUInt64(Value);
    stFloat: RawToFloat(P, AnsiChar(FDecimalSeperator), PSingle(DataAddr)^);
    stDouble: RawToFloat(P, AnsiChar(FDecimalSeperator), PDouble(DataAddr)^);
    stCurrency: RawToFloat(P, AnsiChar(FDecimalSeperator), PCurrency(DataAddr)^);
    stBigDecimal: PBCD(DataAddr)^ := RawToBCD(P, L);
    stDate: if not ZSysUtils.TryRawToDate(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, PZDate(DataAddr)^) then
              goto jmpFail;
    stTime: if not ZSysUtils.TryRawToTime(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, PZTime(DataAddr)^) then
              goto jmpFail;
    stTimeStamp: if not ZSysUtils.TryRawToTimestamp(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, PZTimestamp(DataAddr)^) then
              goto jmpFail;
    stGUID: if (L = 36) or (L = 38) then
              ZSysUtils.ValidGUIDToBinary(P, @PGUID(DataAddr)^.D1)
            else goto jmpFail;
    stBytes: TBytes(DataAddr^) := BufferToBytes(P, L);
    stString: if ((FZVariantType = vtUTF8String) and (CodePage = zCP_UTF8)) or
                 {$IFNDEF NO_ANSISTRING}
                 ((FZVariantType = vtAnsiString) and (CodePage = ZOSCodePage)) or
                 {$ENDIF}
                 (FZVariantType = vtRawByteString)
              then RawByteString(DataAddr^) := Value
              else begin
                if FZVariantType = vtUTF8String
                then DestCP := zCP_UTF8
                else {$IFNDEF NO_ANSISTRING}if FZVariantType = vtAnsiString
                  then DestCP := ZOSCodePage
                  else {$ENDIF}DestCP := GetDefaultRawCP;
                PRawToRawConvert(P, L, CodePage, DestCP, RawByteString(DataAddr^));
              end;
    stUnicodeString: UnicodeString(DataAddr^) := ZRawToUnicode(Value, CodePage);
    stAsciiStream, stUnicodeStream: ConvertRawToCLobVariable(Value, CodePage);
    stBinaryStream: ConvertRawToBLobVariable(Value);
    else
jmpFail: raise Self.CreateConversionError(FSQLDataType, stString);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.InternalSetAsUnicodeString(DataAddr: PPointer;
  IsNullAddr: PBoolean; const Value: UnicodeString);
var P: PWidechar;
    L: NativeUint;
    procedure SetAsRaw;
    var CP: Word;
    begin
      CP := GetDefaultRawCP;
      InternalSetAsRawByteString(DataAddr, IsNullAddr, ZUnicodeToRaw(Value, CP), CP);
    end;
    procedure SetAsLob;
    begin
      if IsNullAddr = @FNull then
        if FSQLDataType = stBinaryStream
        then IZBlob(DataAddr^) := TZLocalMemBLob.CreateWithData(P, L shl 1)
        else IZCLob(DataAddr^) := TZLocalMemCLob.CreateWithData(P, L, FConSettings)
      else if FSQLDataType = stBinaryStream
        then IInterface(DataAddr^) := TZLocalMemBLob.CreateWithData(P, L shl 1)
        else IInterface(DataAddr^) := TZLocalMemCLob.CreateWithData(P, L, FConSettings);
    end;
label jmpErr;
begin
  L := Length(Value);
  if L = 0
  then P := PEmptyUnicodeString
  else P := Pointer(Value);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := StrToBoolEx(P, P+L);
    stByte: PByte(DataAddr)^ := UnicodeToUInt32(P,P+L);
    stWord: PWord(DataAddr)^ := UnicodeToUInt32(P,P+L);
    stLongWord: PCardinal(DataAddr)^ := UnicodeToUInt32(P,P+L);
    stShort: PShortInt(DataAddr)^ := UnicodeToInt(Value);
    stSmall: PSmallInt(DataAddr)^ := UnicodeToInt(Value);
    stInteger: PInteger(DataAddr)^ := UnicodeToInt(Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUint64(DataAddr)^ := UnicodeToUInt64(P, P+L);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(DataAddr)^ := UnicodeToInt64(P, P+L);
    stFloat: UnicodeToFloat(P, WideChar(FDecimalSeperator), PSingle(DataAddr)^);
    stDouble: UnicodeToFloat(P, WideChar(FDecimalSeperator), PDouble(DataAddr)^);
    stCurrency: UnicodeToFloat(P, WideChar(FDecimalSeperator), PCurrency(DataAddr)^);
    stBigDecimal: if not TryUniToBCD(P, L, PBCD(DataAddr)^, '.') then goto jmpErr;
    stDate: if not TryUniToDate(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, PZDate(DataAddr)^) then goto jmpErr;
    stTime: if not TryUniToTime(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, PZTime(DataAddr)^) then goto jmpErr;
    stTimestamp: if not TryUniToTimeStamp(P, L, {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongDateFormat, PZTimeStamp(DataAddr)^) then goto jmpErr;
    stGUID: ZSysUtils.ValidGUIDToBinary(P, @TGUIDDynArray(FData.pvDynArray.VArray)[Index].D1);
    stString: SetAsRaw;
    stUnicodeString: UnicodeString(DataAddr^) := Value;
    stBytes: TBytes(DataAddr^) := BufferToBytes(P, L shl 1);
    stAsciiStream, stUnicodeStream, stBinaryStream: SetAsLob;
    else
jmpErr: raise CreateConversionError(FSQLDataType, stUnicodeString);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

function TZParam.IsEqual(Value: TZParam): Boolean;
  function CompareValues: Boolean;
  var sDiv: Single;
      dDiff: Double;
  begin
    if FNull then
      Result := True
    else case FSQLDataType of
      stBoolean: Result := Ord(FData.pvBool) = Ord(Value.FData.pvBool);
      stByte, stShort: Result := FData.pvByte = Value.FData.pvByte;
      stWord, stSmall: Result := FData.pvWord = Value.FData.pvWord;
      stLongWord, stInteger: Result := FData.pvCardinal = Value.FData.pvCardinal;
      stLong, stULong, stCurrency: Result := FData.pvInt64 = Value.FData.pvInt64;
      stBigDecimal: Result := ZBCDCompare(FData.pvBCD, Value.FData.pvBCD) = 0;
      stFloat: if FData.pvSingle > Value.FData.pvSingle then begin
                sDiv := FData.pvSingle - Value.FData.pvSingle;
                Result := sDiv > FLOAT_COMPARE_PRECISION_SINGLE;
              end else begin
                sDiv := Value.FData.pvSingle - FData.pvSingle;
                Result := not (sDiv > FLOAT_COMPARE_PRECISION_SINGLE);
              end;
      stDouble: if FData.pvDouble > Value.FData.pvDouble then begin
                dDiff := FData.pvDouble - Value.FData.pvDouble;
                Result := dDiff > FLOAT_COMPARE_PRECISION;
              end else begin
                dDiff := Value.FData.pvDouble - FData.pvDouble;
                Result := not (dDiff > FLOAT_COMPARE_PRECISION);
              end;
      stDate: Result := ZCompareDate(FData.pvDate, Value.FData.pvDate) = 0;
      stTime: Result := ZCompareTime(FData.pvTime, Value.FData.pvTime) = 0;
      stTimeStamp: Result := ZCompareTimestamp(FData.pvTimeStamp, Value.FData.pvTimeStamp) = 0;
      stGUID: Result := CompareMem(@FData.pvGUID.D1, @Value.FData.pvGUID.D1, SizeOf(TGUID));
      stString: Result := (Length(RawByteString(FData.pvPointer)) = Length(RawByteString(Value.FData.pvPointer))) and
        ((FData.pvPointer = nil) or CompareMem(FData.pvPointer, Value.FData.pvPointer, Length(RawByteString(FData.pvPointer))));
      stUnicodeString: Result := (Length(UnicodeString(FData.pvPointer)) = Length(UnicodeString(Value.FData.pvPointer))) and
        ((FData.pvPointer = nil) or  CompareMem(FData.pvPointer, Value.FData.pvPointer, Length(UnicodeString(FData.pvPointer)) shl 1));
      stBytes: Result := (Length(TBytes(FData.pvPointer)) = Length(TBytes(Value.FData.pvPointer))) and
        ((FData.pvPointer = nil) or CompareMem(FData.pvPointer, Value.FData.pvPointer, Length(TBytes(FData.pvPointer))));
      else Result := True;
    end;
  end;
begin
  Result :=
    (FArraySize = Value.FArraySize) and (FSQLType = Value.FSQLType) and
    (FDataType = Value.FDataType) and (FSQLDataType = Value.FSQLDataType) and
    (FPrecision = Value.FPrecision) and (FNumericScale = Value.FNumericScale) and
    (FSize = Value.FSize) and (FNull = Value.FNull) and (FBound = Value.FBound) and
    (FName = Value.FName) and (FParamType = Value.FParamType) and (FNull = Value.FNull) and
    CompareValues;
end;

function TZParam.IsParamStored: Boolean;
begin
  Result := FBound;
end;

procedure TZParam.LoadBinaryFromFile(const FileName: String; Index: Integer);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadBinaryFromStream(Stream, Index);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TZParam.LoadBinaryFromStream(Stream: TStream; Index: Integer);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stBinaryStream, vtInterface, DataAddr, IsNullAddr);
  if Stream = nil then begin
    IsNullAddr^ := True;
    Exit;
  end;
  IsNullAddr^ := False;
  if FSQLDataType = stBytes then begin
    SetLength(TBytes(DataAddr^), Stream.Size);
    Stream.Read(DataAddr^^, Stream.Size);
  end else if FSQLDataType = stBinaryStream then begin
    if DataAddr^ = nil then
      if Index < 0
      then IZBlob(DataAddr^) := TZLocalMemBLob.Create
      else IInterface(DataAddr^) := TZLocalMemBLob.Create;
    if Index < 0
    then IZBlob(DataAddr^).SetStream(Stream)
    else (IInterface(DataAddr^) as IZBlob).SetStream(Stream);
  end else raise CreateConversionError(FSQLDataType, stBinaryStream);
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TZParam.LoadFromFile(const FileName: String; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    FreeAndNil(Stream);
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}

procedure TZParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
begin
  if BlobType in [ftWideString{$IFDEF WITH_WIDEMEMO}, ftFixedWideChar, ftWideMemo{$ENDIF}] then
    LoadTextFromStream(Stream, zCP_UTF16)
  else if BlobType in [ftBlob, ftGraphic, ftTypedBinary, ftOraBlob] then
    LoadBinaryFromStream(Stream)
  else if BlobType in [ftMemo, ftParadoxOle, ftDBaseOle, ftOraClob] then
    LoadTextFromStream(Stream, GetDefaultRawCP)
  else raise EZDatabaseError.Create(SUnKnownParamDataType);
end;

{$IFDEF TENCODING_HAS_CODEPAGE}
procedure TZParam.LoadTextFromFile(const FileName: String; Encoding: TEncoding;
  Index: Integer);
begin
  LoadTextFromFile(FileName, Encoding.CodePage, Index);
end;
{$ENDIF TENCODING_HAS_CODEPAGE}

procedure TZParam.LoadTextFromFile(const FileName: String; CodePage: Word;
  Index: Integer);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadTextFromStream(Stream, CodePage, Index);
  finally
    FreeAndNil(Stream);
  end;
end;

{$IFDEF TENCODING_HAS_CODEPAGE}
procedure TZParam.LoadTextFromStream(Stream: TStream; Encoding: TEncoding;
  Index: Integer);
begin
  LoadTextFromStream(Stream, Encoding.CodePage, Index);
end;
{$ENDIF TENCODING_HAS_CODEPAGE}

procedure TZParam.LoadTextFromStream(Stream: TStream; CodePage: Word;
  Index: Integer);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
var SQLType: TZSQLType;
    procedure BindAsCLob;
    var Clob: IZCLob;
    begin
      Clob := ZDbcResultSet.TZLocalMemCLob.Create(CodePage, FConSettings, nil);
      Clob.SetStream(Stream, CodePage);
      if (FArraySize = 0)
      then IZClob(DataAddr^) := Clob
      else IInterface(DataAddr^) := Clob;
    end;
    procedure BindAsBLob;
    var Blob: IZBlob;
    begin
      Blob := ZDbcResultSet.TZLocalMemBLob.Create(nil);
      Blob.SetStream(Stream);
      if (FArraySize = 0)
      then IZBlob(DataAddr^) := Blob
      else IInterface(DataAddr^) := Blob;
    end;
    {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "Buf" does not seem to be initialized}{$ENDIF}
    procedure BindAsString;
    const MaxBufSize = $F000;
    var Buf: Array[0..MaxBufSize] of Byte;
        P: PAnsiChar;
        B, L: Integer;
        R: RawByteString;
    begin
      R := '';
      Stream.Position := 0;
      L := 0;
      {$IFDEF WITH_RAWBYTESTRING}
      P := nil;
      {$ENDIF WITH_RAWBYTESTRING}
      while True do begin
        B := Stream.Read(Buf[0], MaxBufSize);
        if B = 0 then Break;
        SetLength(R, L+B);
        P := Pointer(R);
        Move(Buf[0], (P+L)^, B);
        Inc(L, B);
      end;
      {$IFDEF WITH_RAWBYTESTRING}
      if P <> nil then
        {$IFDEF FPC}
        PAnsiRec(P-AnsiFirstOff)^.CodePage := CodePage;
        {$ELSE}
        PWord(P - CodePageOffSet)^ := CodePage;
        {$ENDIF}
      {$ENDIF WITH_RAWBYTESTRING}
      InternalSetAsRawByteString(DataAddr, IsNullAddr, R, CodePage);
    end;
    {$IFDEF FPC}{$POP}{$ENDIF}
    {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "Buf" does not seem to be initialized}{$ENDIF}
    procedure BindAsTBytes;
    const MaxBufSize = $F000;
    var Buf: Array[0..MaxBufSize] of Byte;
        P: PAnsiChar;
        B, L: Integer;
    begin
      Stream.Position := 0;
      L := 0;
      while True do begin
        B := Stream.Read(Buf[0], MaxBufSize);
        if B = 0 then Break;
        SetLength(TBytes(DataAddr^), L+B);
        P := DataAddr^;
        Move(Buf[0], (P+L)^, B);
        Inc(L, B);
      end;
    end;
    {$IFDEF FPC}{$POP}{$ENDIF}
    {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "Buf" does not seem to be initialized}{$ENDIF}
    procedure BindAsUnicodeString;
    const MaxBufSize = $F000;
    var Buf: Array[0..MaxBufSize] of Byte;
        P: PAnsiChar;
        B, L: Integer;
        U: UnicodeString;
    begin
      U := '';
      Stream.Position := 0;
      L := 0;
      while True do begin
        B := Stream.Read(Buf[0], MaxBufSize);
        if B = 0 then Break;
        SetLength(UnicodeString(DataAddr^), (L+B) shr 1);
        P := DataAddr^;
        Move(Buf[0], (P+L)^, B);
        Inc(L, B);
      end;
      InternalSetAsUnicodeString(DataAddr, IsNullAddr, U);
    end;
    {$IFDEF FPC}{$POP}{$ENDIF}
begin
  if CodePage = zCP_UTF16 then
    SQLType := stUnicodeStream
  else if CodePage = zCP_Binary then
    SQLType := stBinaryStream
  else SQLType := stAsciiStream;
  CheckDataIndex(Integer(Index), SQLType, vtInterface, DataAddr, IsNullAddr);
  if Stream = nil then begin
    IsNullAddr^ := True;
    Exit;
  end;
  case FSQLDataType of
    stBytes: BindAsTBytes;
    stAsciiStream, stUnicodeStream: BindAsCLob;
    stBinaryStream: BindAsBLob;
    stString, stUnicodeString: if CodePage = zCP_UTF16
                              then BindAsUnicodeString
                              else BindAsString;
    else raise CreateConversionError(FSQLDataType, stBinaryStream);
  end;
  FBound := True;
  IsNullAddr^ := False;
end;

procedure TZParam.SetArraySize(Value: Cardinal);
  procedure ResizeDynArray(Value: Cardinal);
  begin
    SetLength(TBooleanDynArray(FData.pvDynArray.VIsNullArray), Value);
    if Value > FArraySize then
      FillChar((PAnsiChar(FData.pvDynArray.VIsNullArray)+FArraySize)^, (Value-FArraySize), #1);
    case FSQLDataType of
      stBoolean       : SetLength(TBooleanDynArray(FData.pvDynArray.VArray), Value);
      stByte,
      stShort         : SetLength(TByteDynArray(FData.pvDynArray.VArray), Value);
      stWord,
      stSmall         : SetLength(TWordDynArray(FData.pvDynArray.VArray), Value);
      stLongWord,
      stInteger,
      stFloat         : SetLength(TCardinalDynArray(FData.pvDynArray.VArray), Value);
      stULong,
      stLong,
      stDouble,
      stCurrency      : SetLength(TInt64DynArray(FData.pvDynArray.VArray), Value);
      stBigDecimal    : SetLength(TBCDDynArray(FData.pvDynArray.VArray), Value);
      stDate          : SetLength(TZDateDynArray(FData.pvDynArray.VArray), Value);
      stTime          : SetLength(TZTimeDynArray(FData.pvDynArray.VArray), Value);
      stTimestamp     : SetLength(TZTimeStampDynArray(FData.pvDynArray.VArray), Value);
      stGUID          : SetLength(TGUIDDynArray(FData.pvDynArray.VArray), Value);
      stString        : SetLength(TRawByteStringDynArray(FData.pvDynArray.VArray), Value);
      stUnicodeString : SetLength(TUnicodeStringDynArray(FData.pvDynArray.VArray), Value);
      stBytes         : SetLength(TBytesDynArray(FData.pvDynArray.VArray), Value);
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream  : SetLength(TInterfaceDynArray(FData.pvDynArray.VArray), Value);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end;
begin
  if (Value <> FArraySize) or ((Value > 0) and (FData.pvDynArray.VArray = nil)) then begin
    if FArraySize = 0 then
      SetIsNull(True);
    ResizeDynArray(Value);
    FArraySize := Value;
  end;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZParam.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsRawByteString(Value, ZOSCodePage);
end;

procedure TZParam.SetAsAnsiStrings(Index: Cardinal; const Value: AnsiString);
begin
  SetAsRawByteStrings(Index, Value, zOSCodePage);
end;
{$ENDIF NO_ANSISTRING}

procedure TZParam.SetAsBlob(const Value: TBlobData);
begin
  SetAsBlobs(Cardinal(-1), Value);
end;

procedure TZParam.SetAsBlobs(Index: Cardinal; const Value: TBlobData);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stBinaryStream, vtInterface, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBytes:  TBytes(DataAddr^) := {$IFDEF TBLOBDATA_IS_TBYTES}Value{$ELSE}BufferToBytes(Pointer(Value), Length(Value)){$ENDIF};
    stBinaryStream: begin
        if DataAddr^ = nil then
          if Integer(Index) < 0
          then IZBlob(DataAddr^) := TZLocalMemBLob.Create(nil)
          else IInterface(DataAddr^) := TZLocalMemBLob.Create(nil);
        if Integer(Index) < 0
        then IZBlob(DataAddr^).SetBuffer(Pointer(Value), Length(Value))
        else (IInterface(DataAddr^) as IZBlob).SetBuffer(Pointer(Value), Length(Value));
      end;
    else raise CreateConversionError(FSQLDataType, stBinaryStream);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsBoolean(Value: Boolean);
begin
  SetAsBooleans(Cardinal(-1), Value);
end;

procedure TZParam.SetAsBooleans(Index: Cardinal; Value: Boolean);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure BindAsString; //keep the _U/AStrClear method  out of main proc
  begin
    case FSQLDataType of
      stString, stAsciiStream: InternalSetAsRawByteString(DataAddr, IsNullAddr, BoolStrsRaw[Value], GetDefaultRawCP)
      else InternalSetAsUnicodeString(DataAddr, IsNullAddr, BoolStrsW[Value]);
    end;
  end;
begin
  CheckDataIndex(Integer(Index), stBoolean, vtBoolean, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := Value;
    stByte..stBigDecimal: {$IFDEF CPU64}SetAsInt64s{$ELSE}SetAsCardinals{$ENDIF}(Index, Ord(Value));
    else BindAsString
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsByte(Value: Byte);
begin
  SetAsByteArray(Cardinal(-1), Value);
end;

procedure TZParam.SetAsByteArray(Index: Cardinal; Value: Byte);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stByte, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := Value <> 0;
    stByte: PByte(DataAddr)^ := Value;
    else SetAsCardinals(Index, Value);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsBytes(const Value: TBytes);
begin
  SetAsBytesArray(Cardinal(-1), Value);
end;

procedure TZParam.SetAsBytesArray(Index: Cardinal; const Value: TBytes);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure BytesToBlob;
  var Blob: IZBlob;
  begin
    if DataAddr = nil then begin
      Blob := TZLocalMemBLob.Create;
      IInterface(DataAddr) := Blob;
    end else Blob := IInterface(DataAddr) as IZBlob;
    Blob.SetBytes(Value);
  end;
begin
  CheckDataIndex(Integer(Index), stBytes, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBytes: TBytes(DataAddr^) := Value;
    stGUID: if Length(Value) = SizeOf(TGUID)
            then PGUID(DataAddr)^ := PGUID(Value)^
            else raise CreateConversionError(stGUID, stBytes);
    else BytesToBlob;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsCardinal(Value: Cardinal);
begin
  SetAsCardinals(Cardinal(-1), Value);
end;

procedure TZParam.SetAsCardinals(Index: Cardinal; Value: Cardinal);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure CardinalToString; //keep the U/AStrClear aout of main proc
  begin
    if (SQLType = stString) or (SQLType = stAsciiStream)
    then SetAsRawByteStrings(Index, IntToRaw(Value), GetDefaultRawCP)
    else SetAsUnicodeStrings(Index, IntToUnicode(Value));
  end;
begin
  CheckDataIndex(Integer(Index), stLongWord, vtNull, DataAddr, IsNullAddr);
  case SQLType of
    stBoolean:  PBoolean(DataAddr)^ := Value <> 0;
    stByte:     PByte(DataAddr)^ := Value;
    stShort:    PShortInt(DataAddr)^ := Value;
    stWord:     PWord(DataAddr)^ := Value;
    stSmall:    PSmallInt(DataAddr)^ := Value;
    stLongWord: PCardinal(DataAddr)^ := Value;
    stInteger:  PInteger(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stLong:     PInt64(DataAddr)^ := Value;
    stULong:    PUInt64(DataAddr)^ := {$IFDEF WITH_UINT64_C1118_ERROR}CardinalToUint64{$ENDIF}(Value);
    stFloat:    PSingle(DataAddr)^ := Value;
    stDouble:   PDouble(DataAddr)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(DataAddr)^, False);
    else CardinalToString;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsCurrency(const Value: Currency);
begin
  SetAsCurrencys(Cardinal(-1), Value);
end;

procedure TZParam.SetAsCurrencys(Index: Cardinal; const Value: Currency);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure SetAsOrdinal(Value: Currency);
  begin
    ZSysUtils.RoundCurrTo(Value, 0);
    SetAsInt64(PInt64(@Value)^ div 10000);
  end;
  procedure SetAsString;
  begin
    if (SQLType = stString) or (SQLType = stAsciiStream)
    then SetAsRawByteStrings(Index, CurrToRaw(Value, FDecimalSeperator), GetDefaultRawCP)
    else InternalSetAsUnicodeString(DataAddr, IsNullAddr, CurrToUnicode(Value, FDecimalSeperator));
  end;
begin
  CheckDataIndex(Integer(Index), stCurrency, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := Value <> 0;
    stByte..stLong: SetAsOrdinal(Value);
    stFloat: PSingle(DataAddr)^ := Value;
    stDouble: PDouble(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stBigDecimal: Currency2Bcd(Value, PBCD(DataAddr)^);
    else SetAsString;
  end;
  FBound := True;
  IsNullAddr^ := False;
end;

procedure TZParam.SetAsDate(const Value: TDate);
begin
  SetAsDates(Cardinal(-1), Value);
end;

procedure TZParam.SetAsDates(Index: Cardinal; const Value: TDate);
  {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized}{$ENDIF}
  procedure ValueToZDate;
  var D: TZDate;
  begin
    DecodeDateTimeToDate(Value, D);
    SetAsZDates(Index, D);
  end;
  {$IFDEF FPC}{$POP}{$ENDIF}
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stDate, vtDate, DataAddr, IsNullAddr);
  case FSQLDataType of
    stFloat: PSingle(DataAddr)^ := Value;
    stDouble: PDouble(DataAddr)^ := Value;
    stTime: FillChar(DataAddr^, SizeOf(TZTime), #0);
    stDate: DecodeDateTimeToDate(Value, PZDate(DataAddr)^);
    stTimeStamp: DecodeDateTimeToTimestamp(Value, PZTimeStamp(DataAddr)^);
    else ValueToZDate;
  end;
  FBound := True;
  IsNullAddr^ := False;
end;

procedure TZParam.SetAsDateTime(const Value: TDateTime);
begin
  SetAsDateTimes(Cardinal(-1), Value);
end;

procedure TZParam.SetAsDateTimes(Index: Cardinal; const Value: TDateTime);
  {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized}{$ENDIF}
  procedure ValueToZTime;
  var TZ: TZTimeStamp;
  begin
    DecodeDateTimeToTimeStamp(Value, TZ);
    SetAsZTimeStamps(Index, TZ);
  end;
  {$IFDEF FPC}{$POP}{$ENDIF}
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stTimestamp, vtTimestamp, DataAddr, IsNullAddr);
  case FSQLDataType of
    stFloat: PSingle(DataAddr)^ := Value;
    stDouble: PDouble(DataAddr)^ := Value;
    stTime: DecodeDateTimeToTime(Value, PZTime(DataAddr)^);
    stDate: DecodeDateTimeToDate(Value, PZDate(DataAddr)^);
    stTimeStamp: DecodeDateTimeToTimestamp(Value, PZTimeStamp(DataAddr)^);
    else ValueToZTime;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsDouble(const Value: Double);
begin
  SetAsDoubles(Cardinal(-1), Value);
end;

procedure TZParam.SetAsDoubles(Index: Cardinal; const Value: Double);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure ValueToRawString; //keep the U/AStrClear aout of main proc
  begin
    SetAsRawByteStrings(Index, FloatToRaw(Value), GetDefaultRawCP)
  end;
  procedure ValueToUTF16String; //keep the U/AStrClear aout of main proc
  begin
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, FloatToUnicode(Value));
  end;
begin
  CheckDataIndex(Integer(Index), stDouble, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := Value <> 0;
    stByte..stLong: SetAsInt64s(Index, Trunc(Value));
    stFloat: PSingle(DataAddr)^ := Value;
    stDouble: PDouble(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stBigDecimal: Double2BCD(Value, PBCD(DataAddr)^);
    stTime: DecodeDateTimeToTime(Value, PZTime(DataAddr)^);
    stDate: DecodeDateTimeToDate(Value, PZDate(DataAddr)^);
    stTimeStamp: DecodeDateTimeToTimestamp(Value, PZTimestamp(DataAddr)^);
    stString, stAsciiStream: ValueToRawString;
    else ValueToUTF16String;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsFmtBCD(Value: TBCD);
begin
  SetAsFmtBCDs(Cardinal(-1), Value);
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "function __GetNull:<recordtype>;" marked as inline is not inlined}{$ENDIF}
procedure TZParam.SetAsFmtBCDs(Index: Cardinal; const Value: TBCD);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure ValueToRawString; //keep the U/AStrClear aout of main proc
  var Digits: array[0..MaxFMTBcdFractionSize-1+1{sign}+1{dot}] of AnsiChar;
    L: LengthInt;
    R: RawByteString;
    CP: Word;
  begin
    L := BCDToRaw(Value, @Digits[0], FDecimalSeperator);
    R := '';
    CP := GetDefaultRawCP;
    ZSetString(PAnsiChar(@Digits[0]), l, R {$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
    SetAsRawByteStrings(Index, R, CP)
  end;
  procedure ValueToUTF16String; //keep the U/AStrClear aout of main proc
  var Digits: array[0..MaxFMTBcdFractionSize-1+1{sign}+1{dot}] of WideChar;
    L: LengthInt;
    U: UnicodeString;
  begin
    L := BCDToUni(Value, @Digits[0], FDecimalSeperator);
    U := '';
    System.SetString(U, PWideChar(@Digits[0]), L);
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, U);
  end;
begin
  CheckDataIndex(Integer(Index), stBigDecimal, vtBigDecimal, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := BcdCompare(Value, NullBCD) <> 0;
    stByte, stShort, stWord, stSmall, stInteger: SetAsIntegers(Index, BCDToInteger(Value));
    stLong: PInt64(DataAddr)^ := Bcd2Int64(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stLongWord,
    stULong: SetAsUInt64s(Index, Bcd2UInt64(Value));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stFloat: PSingle(DataAddr)^ := BcdToDouble(Value);
    stDouble: PDouble(DataAddr)^ := BcdToDouble(Value);
    stCurrency: BCDToCurr(Value, PCurrency(DataAddr)^);
    stBigDecimal: PBCD(DataAddr)^ := Value;
    stString, stAsciiStream: ValueToRawString;
    else ValueToUTF16String;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

procedure TZParam.SetAsGUID(const Value: TGUID);
begin
  SetAsGUIDs(Cardinal(-1), Value);
end;

procedure TZParam.SetAsGUIDs(Index: Cardinal; const Value: TGUID);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure ValueToRawString;
  var S: RawByteString;
  begin
    S := ZSysUtils.GUIDToRaw(Value, [guidWithBrackets]);
    SetAsRawByteStrings(Index, S, GetDefaultRawCP);
  end;
  procedure ValueToUTF16String;
  var S: UnicodeString;
  begin
    S := GUIDToUnicode(Value, [guidWithBrackets]);
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, S);
  end;
  procedure ValueToBytes;
  begin
    TBytes(PPointer(DataAddr)^) := BufferToBytes(@Value.D1, SizeOf(TGUID));
  end;
  procedure ValueToBlob;
  begin
    if FArraySize = 0 then begin
      if PPointer(DataAddr)^ = nil then
        IZBlob(DataAddr^) := TZLocalMemBLob.Create(nil);
      IZBlob(DataAddr^).SetBuffer(@Value.D1, SizeOf(TGUID));
    end else begin
      if DataAddr^ = nil then
        IInterface(DataAddr^) := TZLocalMemBLob.Create(nil);
      (IInterface(DataAddr^) as IZBlob).SetBuffer(@Value.D1, SizeOf(TGUID));
    end;
  end;
begin
  CheckDataIndex(Integer(Index), stGUID, vtGUID, DataAddr, IsNullAddr);
  case FSQLDataType of
    stGUID: PGUID(DataAddr)^ := Value;
    stString, stAsciiStream: ValueToRawString;
    stBytes: ValueToBytes;
    stBinaryStream: ValueToBlob;
    else ValueToUTF16String;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsInt64(const Value: Int64);
begin
  SetAsInt64s(Cardinal(-1), Value);
end;

procedure TZParam.SetAsInt64s(Index: Cardinal; const Value: Int64);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure IntegerToString; //keep the U/AStrClear out of main proc
  begin
    if FSQLDataType in [stString, stAsciiStream]
    then SetAsRawByteStrings(Index, IntToRaw(Value), GetDefaultRawCP)
    else InternalSetAsUnicodeString(DataAddr, IsNullAddr, IntToUnicode(Value));
  end;
begin
  CheckDataIndex(Integer(Index), stLong, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean:  PBoolean(DataAddr)^ := Value <> 0;
    stByte:     PByte(DataAddr)^ := Value;
    stShort:    PShortInt(DataAddr)^ := Value;
    stWord:     PWord(DataAddr)^ := Value;
    stSmall:    PSmallInt(DataAddr)^ := Value;
    stLongWord: PCardinal(DataAddr)^ := Value;
    stInteger:  PInteger(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stLong:     PInt64(DataAddr)^ := Value;
    stULong:    PUInt64(DataAddr)^ := {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(Value);
    stFloat:    PSingle(DataAddr)^ := Value;
    stDouble:   PDouble(DataAddr)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(DataAddr)^);
    else IntegerToString;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsInteger(Value: Integer);
begin
  SetAsIntegers(Cardinal(-1), Value);
end;

procedure TZParam.SetAsIntegers(Index: Cardinal; Value: Integer);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure IntegerToString; //keep the U/AStrClear out of main proc
  begin
    if FSQLDataType in [stString, stAsciiStream]
    then SetAsRawByteStrings(Index, IntToRaw(Value), GetDefaultRawCP)
    else InternalSetAsUnicodeString(DataAddr, IsNullAddr, IntToUnicode(Value));
  end;
begin
  CheckDataIndex(Integer(Index), stInteger, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean:  PBoolean(DataAddr)^ := Value <> 0;
    stByte:     PByte(DataAddr)^ := Value;
    stShort:    PShortInt(DataAddr)^ := Value;
    stWord:     PWord(DataAddr)^ := Value;
    stSmall:    PSmallInt(DataAddr)^ := Value;
    stLongWord: PCardinal(DataAddr)^ := Value;
    stInteger:  PInteger(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stLong:     PInt64(DataAddr)^ := Value;
    stULong:    PUInt64(DataAddr)^ := {$IFDEF WITH_UINT64_C1118_ERROR}Int64ToUInt64{$ENDIF}(Value);
    stFloat:    PSingle(DataAddr)^ := Value;
    stDouble:   PDouble(DataAddr)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(DataAddr)^);
    else IntegerToString;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsMemo(const Value: String);
begin
  SetAsMemos(Cardinal(-1), Value);
end;

procedure TZParam.SetAsMemos(Index: Cardinal; const Value: String);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), {$IFDEF UNICODE}stUnicodeStream{$ELSE}stAsciiStream{$ENDIF}, vtInterface, DataAddr, IsNullAddr);
  {$IFDEF UNICODE}
  InternalSetAsUnicodeString(DataAddr, IsNullAddr, Value);
  {$ELSE}
  InternalSetAsRawByteString(DataAddr, IsNullAddr, Value, GetDefaultRawCP);
  {$ENDIF}
end;

procedure TZParam.SetAsRawByteString(const Value: RawByteString;
  CodePage: Word);
begin
  SetAsRawByteStrings(Cardinal(-1), Value, CodePage);
end;

procedure TZParam.SetAsRawByteStrings(Index: Cardinal;
  const Value: RawByteString; CodePage: Word);
var VariantType: TZVariantType;
    SQLType: TZSQLType;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  if Codepage = zCP_Binary then begin
    SQLType := stBytes;
    VariantType := vtBytes;
  end else begin
    SQLType := stString;
    if CodePage = zCP_UTF8
    then VariantType := vtUTF8String
    else {$IFNDEF NO_ANSISTRING}if CodePage = zOSCodePage
      then VariantType := vtAnsiString
      else {$ENDIF}VariantType := vtRawByteString;
  end;
  CheckDataIndex(Integer(Index), SQLType, VariantType, DataAddr, IsNullAddr);
  InternalSetAsRawByteString(DataAddr, IsNullAddr, Value, CodePage);
end;

procedure TZParam.SetAsRawMemo(const Value: RawByteString; CodePage: Word);
begin
  SetAsRawMemos(Cardinal(-1), Value, CodePage);
end;

procedure TZParam.SetAsRawMemos(Index: Cardinal; const Value: RawByteString; CodePage: Word);
var SQLType: TZSQLType;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  if Codepage = zCP_Binary
  then SQLType := stBinaryStream
  else SQLType := stAsciiStream;
  CheckDataIndex(Integer(Index), SQLType, vtInterface, DataAddr, IsNullAddr);
  InternalSetAsRawByteString(DataAddr, IsNullAddr, Value, CodePage);
end;

procedure TZParam.SetAsShortInt(Value: ShortInt);
begin
  SetAsShortInts(Cardinal(-1), Value);
end;

procedure TZParam.SetAsShortInts(Index: Cardinal; Value: ShortInt);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stShort, vtNull, DataAddr, IsNullAddr);
  if FSQLDataType = stShort then begin
    PShortInt(DataAddr)^ := Value;
    IsNullAddr^ := False;
    FBound := True;
  end else SetAsIntegers(Index, Value);
end;

procedure TZParam.SetAsSingle(Value: Single);
begin
  SetAsSingles(Cardinal(-1), Value);
end;

procedure TZParam.SetAsSingles(Index: Cardinal; Value: Single);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stFloat, vtNull, DataAddr, IsNullAddr);
  if FSQLDataType = stFloat then begin
    PSingle(DataAddr)^ := Value;
    IsNullAddr^ := False;
    FBound := True;
  end else SetAsDoubles(Index, Value);
end;

procedure TZParam.SetAsSmallInt(Value: SmallInt);
begin
  SetAsSmallInts(Cardinal(-1), Value);
end;

procedure TZParam.SetAsSmallInts(Index: Cardinal; Value: SmallInt);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stSmall, vtNull, DataAddr, IsNullAddr);
  if FSQLDataType = stSmall then begin
    PSmallInt(DataAddr)^ := Value;
    IsNullAddr^ := False;
    FBound := True;
  end else SetAsIntegers(Index, Value);
end;

{$IFNDEF UNICODE}
procedure TZParam.SetAsString(const Value: String);
begin
  SetAsStrings(Cardinal(-1), Value);
end;
{$ENDIF UNICODE}

{$IFNDEF UNICODE}
procedure TZParam.SetAsStrings(Index: Cardinal; const Value: String);
var VariantType: TZVariantType;
    SQLType: TZSQLType;
    CP: Word;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  If ((FConSettings <> nil) or SetConsettings) and (FConSettings.ClientCodePage.Encoding = ceUTF16) then begin
    SQLType := stUnicodeString;
    VariantType := vtUnicodeString;
  end else begin
    SQLType := stString;
    CP := GetDefaultRawCP;
    if CP = zCP_UTF8
    then VariantType := vtUTF8String
    else if CP = zOSCodePage
      then VariantType := vtAnsiString
      else VariantType := vtRawByteString;
  end;
  CheckDataIndex(Integer(Index), SQLType, VariantType, DataAddr, IsNullAddr);
  InternalSetAsRawByteString(DataAddr, IsNullAddr, Value, GetDefaultRawCP);
end;
{$ENDIF UNICODE}

procedure TZParam.SetAsTime(const Value: TTime);
begin
  SetAsTimes(Cardinal(-1), Value);
end;

procedure TZParam.SetAsTimes(Index: Cardinal; const Value: TTime);
  {$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized}{$ENDIF}
  procedure ValueToZTime;
  var T: TZTime;
  begin
    DecodeDateTimeToTime(Value, T);
    SetAsZTimes(Index, T);
  end;
  {$IFDEF FPC}{$POP}{$ENDIF}
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stTime, vtTime, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean: PBoolean(DataAddr)^ := Value <> 0;
    stByte..stLong: SetAsInt64s(Index, Trunc(Value));
    stFloat: PSingle(DataAddr)^ := Value;
    stDouble: PDouble(DataAddr)^ := Value;
    stTime: DecodeDateTimeToTime(Value, PZTime(DataAddr)^);
    stDate: DecodeDateTimeToDate(Value, PZDate(DataAddr)^);
    stTimeStamp: DecodeDateTimeToTimestamp(Value, PZTimestamp(DataAddr)^);
    else begin
      ValueToZTime;
      Exit;
    end;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsUInt64(const Value: UInt64);
begin
  SetAsUInt64s(Cardinal(-1), Value);
end;

procedure TZParam.SetAsUInt64s(Index: Cardinal; const Value: UInt64);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure IntegerToString; //keep the U/AStrClear out of main proc
  begin
    if FSQLDataType in [stString, stAsciiStream]
    then InternalSetAsRawByteString(DataAddr, IsNullAddr, IntToRaw(Value), GetDefaultRawCP)
    else InternalSetAsUnicodeString(DataAddr, IsNullAddr, IntToUnicode(Value));
  end;
begin
  CheckDataIndex(Integer(Index), stULong, vtNull, DataAddr, IsNullAddr);
  case FSQLDataType of
    stBoolean:  PBoolean(DataAddr)^ := Value <> 0;
    stByte:     PByte(DataAddr)^ := Value;
    stShort:    PShortInt(DataAddr)^ := Value;
    stWord:     PWord(DataAddr)^ := Value;
    stSmall:    PSmallInt(DataAddr)^ := Value;
    stLongWord: PCardinal(DataAddr)^ := Value;
    stInteger:  PInteger(DataAddr)^ := Value;
    stCurrency: PCurrency(DataAddr)^ := Value;
    stLong:     PInt64(DataAddr)^ := Value;
    stULong:    PUint64(DataAddr)^ := Value;
    stFloat:    PSingle(DataAddr)^ := Value;
    stDouble:   PDouble(DataAddr)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(DataAddr)^, False);
    else        begin
                  IntegerToString;
                  Exit;
                end;
  end;
  IsNullAddr^ := False;
  FBound := True;
end;

procedure TZParam.SetAsUnicodeString(const Value: UnicodeString);
begin
  SetAsUnicodeStrings(Cardinal(-1), Value);
end;

procedure TZParam.SetAsUnicodeStrings(Index: Cardinal;
  const Value: UnicodeString);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stUnicodeString, vtUnicodeString, DataAddr, IsNullAddr);
  InternalSetAsUnicodeString(DataAddr, IsNullAddr, Value);
end;

procedure TZParam.SetAsUTF8String(const Value: UTF8String);
begin
  SetAsRawByteStrings(Cardinal(-1), Value, zCP_UTF8);
end;

procedure TZParam.SetAsUTF8Strings(Index: Cardinal; const Value: UTF8String);
begin
  SetAsRawByteStrings(Index, Value, zCP_UTF8);
end;

procedure TZParam.SetAsVariant(const Value: Variant);
var vt: TVarType;
  procedure AsBCD(const Value: Variant);
  begin
     SetAsFmtBCD(VarToBCD(Value))
  end;
  {$IFDEF WITH_TSQLTimeStamp}
  procedure AsTimeStamp(const Value: Variant);
  var cTS: TSQLTimeStamp;
      zTS: TZTimeStamp;
  begin
    cTS := VarToSQLTimeStamp(Value);
    zTS.Year := cTS.Year;
    zTS.Month := cTS.Month;
    zTS.Day := cTS.Day;
    zTS.Hour := cTS.Hour;
    zTS.Minute := cTS.Minute;
    zTS.Second := cTS.Second;
    zTS.Fractions := cTS.Fractions;
    PCardinal(@zTS.TimeZoneHour)^ := 0;
    zTS.IsNegative := False;
    SetAsZTimestamp(zTS);
  end;
  {$ENDIF}
  {$IFDEF WITH_TSQLTimeStampOffset}
  procedure AsTimeStampOffset(const Value: Variant);
  var cTS: TSQLTimeStampOffset;
      zTS: TZTimeStamp;
  begin
    cTS := VarToSQLTimeStampOffset(Value);
    zTS.Year := cTS.Year;
    zTS.Month := cTS.Month;
    zTS.Day := cTS.Day;
    zTS.Hour := cTS.Hour;
    zTS.Minute := cTS.Minute;
    zTS.Second := cTS.Second;
    zTS.Fractions := cTS.Fractions;
    zTS.TimeZoneHour := cTS.TimeZoneHour;
    zTS.TimeZoneMinute := cTS.TimeZoneMinute;
    zTS.IsNegative := False;
    SetAsZTimestamp(zTS);
  end;
  {$ENDIF}
var SQLType: TZSQLType;
begin
  CheckDataIndex(-1);
  SQLType := FSQLType;
  vt := TVarData(Value).VType;
  //WriteLn('case vt of');
  case vt of
    varEmpty, varNull: SetIsNull(True);
    varSmallint:  SetAsSmallInt(TVarData(Value).VSmallInt);
    varInteger:   SetAsInteger(TVarData(Value).VInteger);
    varSingle:    SetAsSingle(TVarData(Value).VSingle);
    varDouble:    SetAsDouble(TVarData(Value).VDouble);
    varCurrency:  SetAsCurrency(TVarData(Value).VDouble);
    varDate:      SetAsDateTime(TVarData(Value).VDate);
    varOleStr:    SetAsUnicodeString(TVarData(Value).VOleStr);
  //varDispatch = $0009; { vt_dispatch     9 }
  //varError    = $000A; { vt_error       10 }
    varBoolean:   SetAsBoolean(TVarData(Value).VBoolean);
    varVariant:   SetAsVariant(PVariant(TVarData(Value).VAny)^);
  //varUnknown  = $000D; { vt_unknown     13 }
  //varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
  //varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
    varShortInt:  SetAsShortInt(TVarData(Value).VShortInt);
    varByte:      SetAsByte(TVarData(Value).VByte);
    varWord:      SetAsWord(TVarData(Value).VWord);
    {$IF Declared(varUInt32)}
    varUInt32:  SetAsCardinal(TVarData(Value).VUInt32);
    {$ELSE}
    varLongWord:  SetAsCardinal(TVarData(Value).VLongWord);
    {$IFEND}
    varInt64:     SetAsInt64(TVarData(Value).VInt64);
    {$IF Declared(varUInt64) and not DEFINED(FPC)}
    varUInt64:    SetAsUInt64(TVarData(Value).VUInt64);
    {$IFEND}
    {$IF Declared(varQWord) and DEFINED(FPC)}
    varQWord:    SetAsUInt64(TVarData(Value).vqword);
    {$IFEND}
  //varRecord   = $0024; { VT_RECORD      36 }
  {  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

    //varStrArg   = $0048; { vt_clsid        72 }
    //varObject   = $0049; {                 73 }
    //varUStrArg  = $004A; {                 74 }
    {$IFDEF LCL}
    varString:  SetAsUTF8String(UTF8String(TVarData(Value).VString));
    {$ELSE}
      {$IFNDEF NO_ANSISTRING}
    varString:  SetAsAnsiString(AnsiString(TVarData(Value).VString));
      {$ENDIF NO_ANSISTRING}
    {$ENDIF}
    //varAny      = $0101; { Corba any      257 } {not OLE compatible }
    {$IF declared(varUString)}
    varUString: SetAsUnicodeString(UnicodeString(TVarData(Value).VAny));
    {$IFEND}
    (varArray or varByte): SetAsBytes(VarToBytes(Value));
    // custom types range from $110 (272) to $7FF (2047)
    else  if vt = VarFMTBcd then
            AsBCD(Value)
          else {$IFDEF WITH_TSQLTimeStamp}if vt = VarSQLTimeStamp then
            AsTimeStamp(Value)
          else {$ENDIF}{$IFDEF WITH_TSQLTimeStampOffset}
          if vt = VarSQLTimeStampOffset then
            AsTimeStampOffset(Value)
          else {$ENDIF}raise EVariantError.Create(SUnsupportedVariantType);
  end;
 // WriteLn('SQLType <> FSQLType');
  if SQLType <> FSQLType then //mimic the TParam behavior
    SetSQLType(SQLType);
end;

procedure TZParam.SetAsWideMemo(const Value: UnicodeString);
begin
  SetAsWideMemos(Cardinal(-1), Value);
end;

procedure TZParam.SetAsWideMemos(Index: Cardinal; const Value: UnicodeString);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stUnicodeStream, vtInterface, DataAddr, IsNullAddr);
  InternalSetAsUnicodeString(DataAddr, IsNullAddr, Value);
end;

procedure TZParam.SetAsWord(const Value: Word);
begin
  SetAsWords(Cardinal(-1), Value);
end;

procedure TZParam.SetAsWords(Index: Cardinal; const Value: Word);
var DataAddr: PPointer;
    IsNullAddr: PBoolean;
begin
  CheckDataIndex(Integer(Index), stWord, vtnull, DataAddr, IsNullAddr);
  if FSQLDataType = stWord then begin
    PWord(DataAddr)^ := Value;
    IsNullAddr^ := False;
    FBound := True;
  end else SetAsCardinals(Index, Value);
end;

procedure TZParam.SetAsZDate(const Value: TZDate);
begin
  SetAsZDates(Cardinal(-1), Value);
end;

{$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized}{$ENDIF}
procedure TZParam.SetAsZDates(Index: Cardinal; const Value: TZDate);
var DT: TDateTime;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure SetAsRaw;
  var Buf: array[0..cMaxDateLen] of AnsiChar;
      L: Integer;
      R: RawByteString;
      CP: Word;
  begin
    L := DateToRaw(Value.Year, Value.Month, Value.Day, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, False, Value.IsNegative);
    R := '';
    CP := GetDefaultRawCP;
    ZSetString(PAnsiChar(@Buf[0]), L, R{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
    InternalSetAsRawByteString(DataAddr, IsNullAddr, R, CP);
  end;
  procedure SetAsUni;
  var Buf: array[0..cMaxDateLen] of WideChar;
      L: Integer;
      U: UnicodeString;
  begin
    L := DateToUni(Value.Year, Value.Month, Value.Day, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.ShortDateFormat, False, Value.IsNegative);
    U := '';
    System.SetString(U, PWideChar(@Buf[0]), L);
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, U);
  end;
begin
  CheckDataIndex(Integer(Index), stDate, vtDate, DataAddr, IsNullAddr);
  case FSQLDataType of
    stTime: FillChar(DataAddr^, SizeOf(TZTime), #0);
    stDate: PZDate(DataAddr)^ := Value;
    stTimeStamp: TimeStampFromDate(Value, PZTimeStamp(DataAddr)^);
    stString, stAsciiStream: SetAsRaw;
    stUnicodeString, stUnicodeStream: SetAsUni;
    else  if ZSysUtils.TryDateToDateTime(Value, DT) then begin
            SetAsDoubles(Index, DT);
            Exit;
          end else raise CreateConversionError(FSQLDataType, stDate);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;
{$IFDEF FPC}{$POP}{$ENDIF}

procedure TZParam.SetAsZTime(const Value: TZTime);
begin
  SetAsZTimes(Cardinal(-1), Value);
end;

{$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized}{$ENDIF}
procedure TZParam.SetAsZTimes(Index: Cardinal; const Value: TZTime);
var DT: TDateTime;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure SetAsRaw;
  var Buf: array[0..cMaxTimeLen] of AnsiChar;
      L: Integer;
      R: RawByteString;
      CP: Word;
  begin
    L := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, Value.IsNegative);
    R := '';
    CP := GetDefaultRawCP;
    ZSetString(PAnsiChar(@Buf[0]), L, R{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
    InternalSetAsRawByteString(DataAddr, IsNullAddr, R, CP);
  end;
  procedure SetAsUni;
  var Buf: array[0..cMaxTimeLen] of WideChar;
      L: Integer;
      U: UnicodeString;
  begin
    L := TimeToUni(Value.Hour, Value.Minute, Value.Second, Value.Fractions, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, Value.IsNegative);
    U := '';
    System.SetString(U, PWideChar(@Buf[0]), L);
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, U);
  end;
begin
  CheckDataIndex(Integer(Index), stTime, vtTime, DataAddr, IsNullAddr);
  case FSQLDataType of
    stTime: PZTime(DataAddr)^ := Value;
    stDate: FillChar(DataAddr, SizeOf(TZDate), #0);
    stTimeStamp: TimeStampFromTime(Value, PZTimeStamp(DataAddr)^);
    stString, stAsciiStream: SetAsRaw;
    stUnicodeString, stUnicodeStream: SetAsUni;
    else if ZSysUtils.TryTimeToDateTime(Value, DT) then begin
            SetAsDoubles(Index, DT);
            Exit;
          end else raise CreateConversionError(FSQLDataType, stTime);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;
{$IFDEF FPC}{$POP}{$ENDIF}

procedure TZParam.SetAsZTimestamp(const Value: TZTimestamp);
begin
  SetAsZTimestamps(Cardinal(-1), Value);
end;

{$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized}{$ENDIF}
procedure TZParam.SetAsZTimestamps(Index: Cardinal; const Value: TZTimestamp);
var DT: TDateTime;
    DataAddr: PPointer;
    IsNullAddr: PBoolean;
  procedure SetAsRaw;
  var Buf: array[0..cMaxTimeStampLen] of AnsiChar;
      L: Integer;
      R: RawByteString;
      CP: Word;
  begin
    L := DateTimeToRaw(Value.Year, Value.Month, Value.Day, Value.Hour, Value.Minute,
      Value.Second, Value.Fractions, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, Value.IsNegative);
    R := '';
    CP := GetDefaultRawCP;
    ZSetString(PAnsiChar(@Buf[0]), L, R{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
    InternalSetAsRawByteString(DataAddr, IsNullAddr, R, CP);
  end;
  procedure SetAsUni;
  var Buf: array[0..cMaxTimeStampLen] of WideChar;
      L: Integer;
      U: UnicodeString;
  begin
    L := DateTimeToUni(Value.Year, Value.Month, Value.Day, Value.Hour, Value.Minute,
      Value.Second, Value.Fractions, @Buf[0],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.LongTimeFormat, False, Value.IsNegative);
    U := '';
    System.SetString(U, PWideChar(@Buf[0]), L);
    InternalSetAsUnicodeString(DataAddr, IsNullAddr, U);
  end;
begin
  CheckDataIndex(Integer(Index), stTimestamp, vtTimestamp, DataAddr, IsNullAddr);
  case FSQLDataType of
    stTime: TimeFromTimeStamp(Value, PZTime(DataAddr)^);
    stDate: DateFromTimeStamp(Value, PZDate(DataAddr)^);
    stTimeStamp: PZTimeStamp(DataAddr)^ := Value;
    stString, stAsciiStream: SetAsRaw;
    stUnicodeString, stUnicodeStream: SetAsUni;
    else  if ZSysUtils.TryTimestampToDateTime(Value, DT) then begin
            SetAsDoubles(Index, DT);
            Exit;
          end else raise CreateConversionError(FSQLDataType, stTimeStamp);
  end;
  IsNullAddr^ := False;
  FBound := True;
end;
{$IFDEF FPC}{$POP}{$ENDIF}

procedure TZParam.SetBlobData(Buffer: Pointer; Size: Integer);
begin
  SetData(Buffer, Size);
end;

function TZParam.SetConsettings: Boolean;
begin
  if TrySetConnection
  then Result := FConSettings <> nil
  else Result := False;
end;

procedure TZParam.SetData(Buffer: Pointer; ByteLen: Cardinal);
  procedure SetFromWideString;
  var Tmp: UnicodeString;
  begin
    if ByteLen = $FFFFFFFF
    then ByteLen := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(PWideChar(Buffer))
    else ByteLen := ByteLen shr 1;
    System.SetString(Tmp, PWideChar(Buffer), ByteLen);
    InternalSetAsUnicodeString(@FData.pvPointer, @FNull, tmp);
  end;
  procedure SetFromRawString;
  var Tmp: RawByteString;
  begin
    if ByteLen = $FFFFFFFF then
      ByteLen := StrLen(PAnsiChar(Buffer));
    tmp := '';
    ZSetString(PAnsiChar(Buffer), ByteLen, tmp);
    InternalSetAsRawByteString(@FData.pvPointer, @FNull, tmp, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}ZOSCodePage{$ENDIF});
  end;
  procedure SetFromBytes;
  var Tmp: TBytes;
  begin
    if ByteLen = $FFFFFFFF then
      ByteLen := StrLen(PAnsiChar(Buffer));
    Tmp := nil;
    SetLength(Tmp, ByteLen);
    Move(Buffer^, Pointer(Tmp)^, ByteLen);
    SetAsBytes(Tmp);
  end;
  procedure SetFromBlobData;
  var Tmp: TBlobData;
  begin
    if ByteLen = $FFFFFFFF then
      ByteLen := StrLen(PAnsiChar(Buffer));
    Tmp := {$IFDEF TBLOBDATA_IS_TBYTES}nil{$ELSE}''{$ENDIF};
    SetLength(Tmp, ByteLen);
    Move(Buffer^, Pointer(Tmp)^, ByteLen);
    SetAsBlob(Tmp);
  end;
begin
  case FDataType of
    ftUnknown: ;
    ftString, ftFixedChar, ftADT, ftMemo: SetFromRawString;
    ftSmallint:   {$IFNDEF WITH_FTSHORTINT}
                  if FSQLType = stShort
                  then SetAsShortInt(PShortInt(Buffer)^)
                  else {$ENDIF}SetAsSmallInt(PSmallInt(Buffer)^);
    ftAutoInc,
    ftInteger:    SetAsInteger(PInteger(Buffer)^);
    ftWord:       SetAsWord(PWord(Buffer)^);
    ftBoolean:    SetAsBoolean(PWordBool(Buffer)^);
    ftFloat,
    ftCurrency:   {$IFNDEF WITH_FTSINGLE}
                  if FSQLType = stFloat
                  then SetAsSingle(PSingle(Buffer)^)
                  else {$ENDIF}SetAsDouble(PDouble(Buffer)^);
    ftBCD:        SetAsCurrency(PCurrency(Buffer)^);
    ftDate:       if FSQLDataType = stDate
                  then SetAsZDate(PZDate(Buffer)^)
                  else SetAsDate(PInteger(Buffer)^ - 1 + D1M1Y1);
    ftTime:       if FSQLType = stTime
                  then SetAsZTime(PZTime(Buffer)^)
                  else SetAsTime(PInteger(Buffer)^ / MSecsOfDay);
    ftDateTime:   if FSQLDataType = stTimestamp
                  then SetAsZTimeStamp(PZTimeStamp(Buffer)^)
                  {$IFDEF FPC}
                  else SetAsDateTime(TimeStampToDateTime(MSecsToTimeStamp(Trunc(PDouble(Buffer)^))));
                  {$ELSE}
                  else SetAsDateTime(TimeStampToDateTime(MSecsToTimeStamp(PDateTime(Buffer)^)));
                  {$ENDIF}
    ftBytes, ftVarBytes: SetFromBytes;
    ftBlob, ftGraphic, ftTypedBinary, ftParadoxOle, ftOraBlob: SetFromBlobData;
    //ftCursor: ;
    ftWideString, ftFmtMemo {$IFDEF WITH_WIDEMEMO}, ftWideMemo, ftFixedWideChar {$ENDIF},
    ftDBaseOle:   SetFromWideString;
    ftLargeint:   SetAsInt64(PInt64(Buffer)^);
    ftArray: ;
    //ftReference: ;
    //ftDataSet: ;
    ftOraClob: ;
    ftVariant:    SetAsVariant(PVariant(Buffer)^);
    ftInterface: ;
    ftIDispatch: ;
    ftGuid:       SetAsGUID(PGUID(Buffer)^);
    ftTimeStamp: ;
    ftFMTBcd:     SetAsFmtBCD(PBCD(Buffer)^);
    //ftOraTimeStamp: ;
    //ftOraInterval: ;
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord:   SetAsCardinal(PCardinal(Buffer)^);
    {$ENDIF WITH_FTLONGWORD}
    {$IFDEF WITH_FTSHORTINT}
    ftShortint:   SetAsShortInt(PShortInt(Buffer)^);
    {$ENDIF}
    {$IFDEF WITH_FTBYTE}
    ftByte:       SetAsByte(PByte(Buffer)^);
    {$ENDIF WITH_FTBYTE}
    {$IFDEF WITH_FTEXTENDED}
    DB.ftExtended:   SetAsDouble(PExtended(Buffer)^);
    {$ENDIF WITH_FTEXTENDED}
    //ftConnection: ;
    //ftParams: ;
    //ftStream: ;
    //ftTimeStampOffset: ;
    //ftObject: ;
    {$IFDEF WITH_FTSINGLE}
    DB.ftSingle:     SetAsSingle(PSingle(Buffer)^);
    {$ENDIF}
    else raise CreateConversionError(FSQLType, FSQLType);
  end;
end;

procedure TZParam.SetDataSet(Value: TDataSet);
begin
  if Value <> FDataSet then begin
    if Value <> nil then
      if not Value.InheritsFrom(TZAbstractRODataset) then
        raise EZDatabaseError.Create('Unsupported dataset. Expected and anchestor of TZAbstractRODataset');
    FDataSet := Value;
  end;
end;

procedure TZParam.SetDataType(Value: TFieldType);
var NewSQLType: TZSQLType;
begin
  if FDataType <> Value then begin
    NewSQLType := ConvertDatasetToDbcType(Value);
    if FDynamicParamType then begin
      FDataType := Value;
      FSQLType := NewSQLType;
    end;
    FDynamicParamType := Value <> ftUnknown;

  end;
end;

procedure TZParam.SetIsNull(Value: Boolean);
  procedure FlushManagedTypes;
  begin
    case FSQLDataType of
      stString: RawByteString(FData.pvPointer) := '';
      stBytes:  TBytes(FData.pvPointer) := nil;
      stUnicodeString: UnicodeString(FData.pvPointer) := '';
      stAsciiStream, stUnicodeStream: IZCLob(FData.pvPointer) := nil;
      stBinaryStream: IZBlob(FData.pvPointer) := nil;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end;
begin
  if GetIsNull <> Value then
    if Value then begin
      if Byte(FSQLDataType) >= Byte(stString) then
        FlushManagedTypes;
      FillChar(FData, SizeOf(TZParamValue), #0);
      FNull := True;
    end else if FSQLDataType = stUnknown then
      raise CreateConversionError(stUnknown, stUnknown);
  FBound := True;
end;

procedure TZParam.SetIsNulls(Index: Cardinal; Value: Boolean);
  procedure FlushManagedArrayTypes(Index: Integer);
  begin
    case FSQLDataType of
      stString: TRawByteStringDynArray(FData.pvDynArray.VArray)[Index] := '';
      stBytes:  TBytesDynArray(FData.pvDynArray.VArray)[Index] := nil;
      stUnicodeString: TUnicodeStringDynArray(FData.pvDynArray.VArray)[Index] := '';
      stAsciiStream..stBinaryStream: TInterfaceDynArray(FData.pvDynArray.VArray)[Index] := nil;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end;
begin
  if GetIsNulls(Index) <> Value then begin
    TBooleanDynArray(FData.pvDynArray.VIsNullArray)[Index] := Value;
    if Value and (Byte(FSQLDataType) >= Byte(stString)) then
      FlushManagedArrayTypes(Index);
  end;
  FBound := True;
end;

procedure TZParam.SetIZBlob(const Value: IZBlob);
begin
  CheckDataIndex(-1);
  SetSQLDataType(stBinaryStream, vtInterface);
  IZBlob(FData.pvPointer) := Value;
  SetIsNull((Value = nil) or (Value.IsEmpty));
end;

procedure TZParam.SetIZCLob(const Value: IZCLob);
var ASQLType: TZSQLType;
begin
  CheckDataIndex(-1);
  TrySetConnection;
  if (FConSettings <> nil) and (FConSettings.ClientCodePage.Encoding = ceUTF16)
  then ASQLType := stUnicodeStream
  else ASQLType := stAsciiStream;
  SetSQLDataType(ASQLType, vtInterface);
  IZClob(FData.pvPointer) := Value;
  SetIsNull((Value = nil) or (Value.IsEmpty));
end;

procedure TZParam.SetSQLDataType(Value: TZSQLType; ZVarType: TZVariantType);
var tmpSize: Integer;
begin
  if (FSQLDataType <> Value) or (FZVariantType <> ZVarType) then begin
    if (FArraySize = 0) or (FSQLDataType = stUnknown) then begin
      if (FSQLDataType <> stUnknown) and ((Ord(FSQLDataType) >= Ord(stString)) or ((Ord(Value) >= Ord(stString)) and (FArraySize = 0))) then
        SetIsNull(True);
      FZVariantType := ZVarType;
      if FSQLDataType = stUnknown then begin
        FSQLDataType := Value;
        if (FArraySize > 0) then
          SetArraySize(FArraySize);
      end else
        FSQLDataType := Value;
      FSQLType := Value;
      if Value in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
        FSize := 0;
        FPrecision := 0;
      end;
      if not (Value in [stCurrency, stBigDecimal, stTime, stTimestamp]) then
        FNumericScale := 0;
      tmpSize := FSize;
      if (tmpSize = 0) and (Value in [stString, stUnicodeString, stBytes]) then
        tmpSize := 1;
      FDataType := ConvertDbcToDatasetType(Value, GetDefaultCharacterFieldType(Self), tmpSize);
    end else
      raise EZDatabaseError.Create('Fehlermeldung');
  end;
end;

procedure TZParam.SetSQLType(Value: TZSQLType);
var tmpSize: Integer;
begin
  if FSQLType <> Value then begin
    if (FArraySize = 0) or (FSQLType = stUnknown) then begin
      FSQLType := Value;
      case FSQLDataType of
        stDate: FZVariantType := vtDate;
        stTime: FZVariantType := vtTime;
        stTimeStamp: FZVariantType := vtTimeStamp;
        stUnicodeString: FZVariantType := vtUnicodeString;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
      if FSQLDataType = stUnknown then begin
        FSQLDataType := Value;
        if (FArraySize > 0) then
          SetArraySize(FArraySize);
      end;
      if Value in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
        FSize := 0;
        FPrecision := 0;
      end;
      if not (Value in [stCurrency, stBigDecimal, stTime, stTimestamp]) then
        FNumericScale := 0;
      tmpSize := FSize;
      if (tmpSize = 0) and (Value in [stString, stUnicodeString, stBytes]) then
        tmpSize := 1;
      FDataType := ConvertDbcToDatasetType(Value, GetDefaultCharacterFieldType(Self), tmpSize);
    end else
      raise EZDatabaseError.Create('ArrayType is locked');
  end;
end;

type
  TZProtectedAbstractRODataset = Class(TZAbstractRODataset);
function TZParam.TrySetConnection: Boolean;
var Owner: TPersistent;
label jmpOwner;
begin
  if FConnection = nil then begin
    Owner := GetOwner;
  jmpOwner:
    if (FDataSet <> nil) and (TZProtectedAbstractRODataset(FDataSet).Connection <> nil) then
      FConnection := TZProtectedAbstractRODataset(FDataSet).Connection
    else if Owner <> nil then
      if Owner.InheritsFrom(TZParams) then begin
        Owner := (Owner as TZParams).GetOwner;
        goto jmpOwner
      end else if Owner.InheritsFrom(TZAbstractRODataset) then
        TZAbstractConnection(fConnection) := TZProtectedAbstractRODataset(Owner as TZAbstractRODataset).Connection
      else if Owner.InheritsFrom(TZUpdateSQL) then begin
        with Owner as TZUpdateSQL do
          if (Dataset <> nil) then
            TZAbstractConnection(fConnection) := TZProtectedAbstractRODataset(Dataset as TZAbstractRODataset).Connection;
      end else if Owner.InheritsFrom(TZSQLProcessor) then
        TZAbstractConnection(fConnection) := (Owner as TZSQLProcessor).Connection;
  end;
  Result := FConnection <> nil;
  if Result and TZAbstractConnection(fConnection).Connected then
    FConSettings := TZAbstractConnection(fConnection).DbcConnection.GetConSettings;
end;

{$IFDEF TENCODING_HAS_CODEPAGE}
procedure TZParam.LoadFromFile(const FileName: String; Encoding: TEncoding;
  Index: Integer);
begin
  LoadTextFromFile(FileName, Encoding.CodePage, Index);
end;
{$ENDIF TENCODING_HAS_CODEPAGE}

procedure TZParam.LoadFromFile(const FileName: String; CodePage: Word;
  Index: Integer);
begin
  if CodePage = zCP_Binary
  then LoadBinaryFromFile(FileName, Index)
  else LoadTextFromFile(FileName, CodePage, Index);
end;

{$IFDEF TENCODING_HAS_CODEPAGE}
procedure TZParam.LoadFromStream(Stream: TStream; Encoding: TEncoding;
  Index: Integer);
begin
  LoadTextFromStream(Stream, Encoding.CodePage, Index);
end;
{$ENDIF TENCODING_HAS_CODEPAGE}

procedure TZParam.LoadFromStream(Stream: TStream; CodePage: Word;
  Index: Integer);
begin
  if CodePage = zCP_Binary
  then LoadBinaryFromStream(Stream, Index)
  else LoadTextFromStream(Stream, CodePage, Index);
end;

{ TZParams }

procedure TZParams.AddParam(Value: TZParam);
begin
  Value.Collection := Self;
end;

function TZParams.AddParameter: TZParam;
begin
  Result := inherited Add as TZParam;
  Result.FDataSet := GetDataSet;
end;

procedure TZParams.AssignTo(Dest: TPersistent);
var I: Integer;
  RtlParam: TParam;
  SQLParam: TZParam;
begin
  if (Dest is TParams) then with Dest as TParams do begin
    for i := 0 to Count -1 do begin
      SQLParam := Self.GetItem(i);
      RtlParam := Items[i];
      SQLParam.AssignTo(RtlParam);
    end;
  end else inherited;
end;

procedure TZParams.AssignValues(Value: TZParams);
var
  I: Integer;
  P: TZParam;
begin
  for I := 0 to Value.Count - 1 do begin
    P := FindParam(Value[I].Name);
    if P <> nil then
      P.Assign(Value[I]);
  end;
end;

constructor TZParams.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TZParam);
  if (FOwner <> nil) then
    if FOwner.InheritsFrom(TZAbstractRODataset) then
      TZAbstractConnection(fConnection) := TZProtectedAbstractRODataset(FOwner as TZAbstractRODataset).Connection
    else if FOwner.InheritsFrom(TZUpdateSQL) then with (FOwner as TZUpdateSQL) do begin
      if DataSet <> nil then
        TZAbstractConnection(fConnection) := TZProtectedAbstractRODataset(DataSet as TZAbstractRODataset).Connection
    end else if FOwner.InheritsFrom(TZSQLProcessor) then
      TZAbstractConnection(fConnection) := (FOwner as TZSQLProcessor).Connection;
end;

constructor TZParams.Create;
begin
  inherited Create(TZParam);
end;

function TZParams.CreateParam(SQLType: TZSQLType; const ParamName: string;
  ParamType: TParamType; Precision, Scale: Integer): TZParam;
begin
  Result := TZParam.Create(Self, ParamType, SQLType, Precision, Scale);
  Result.FArraySize := FArraySize;
  Result.FParamType := ParamType;
  Result.FName := ParamName;
  Result.FDecimalSeperator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.DecimalSeparator;
  Result.SetDataSet(GetDataSet);
end;

function TZParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TZParam;
begin
  Result := TZParam.Create(Self, ParamType);
  Result.FArraySize := FArraySize;
  Result.SetSQLType(ZDatasetUtils.ConvertDatasetToDbcType(FldType));
  Result.FDataType := FldType;
  Result.FParamType := ParamType;
  Result.FDecimalSeperator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings{$ELSE}SysUtils{$ENDIF}.DecimalSeparator;
  Result.FName := ParamName;
  Result.SetDataSet(GetDataSet);
end;

function TZParams.FindParam(const Value: string): TZParam;
var i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if AnsiCompareText(Result.FName, Value) = 0 then
      Exit;
  end;
  Result := nil;
end;

procedure TZParams.FlushParameterConSettings;
var I: Integer;
begin
  for i := 0 to Count-1 do
    Items[i].FConSettings := nil;
end;

function TZParams.GetDataSet: TDataSet;
var Owner: TPersistent;
label jmpOwner;
begin
  Result := nil;
  Owner := GetOwner;
jmpOwner:
  if Owner <> nil then
    if Owner.InheritsFrom(TParam) then begin
      Owner := (Owner as TZParams).GetOwner;
      goto jmpOwner
    end else if Owner.InheritsFrom(TZParam) then begin
      Owner := (Owner as TZParams).GetOwner;
      goto jmpOwner
    end else if Owner.InheritsFrom(TZAbstractRODataset) then
      Result := Owner as TDataSet
    else if Owner.InheritsFrom(TZUpdateSQL) then
      Result := (Owner as TZUpdateSQL).DataSet;
end;

function TZParams.GetItem(Index: Integer): TZParam;
begin
  Result := TZParam(inherited GetItem(Index));
end;

function TZParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{$IFDEF WITH_GENERIC_TPARAM_LIST}
procedure TZParams.GetParamList(List: TList<TZParam>; const ParamNames: string);
{$ELSE}
procedure TZParams.GetParamList(List: TList; const ParamNames: string);
{$ENDIF}
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(ParamNames) do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

function TZParams.GetParamValue(const ParamName: string): Variant;
var Param: TZParam;
begin
  Param := ParamByName(ParamName);
  Result := Param.GetAsVariant;
end;

function TZParams.IsEqual(Value: TZParams): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := TZParam(Items[I]).IsEqual(Value.Items[I]);
      if not Result then Break;
    end
end;

function TZParams.ParamByName(const Value: string): TZParam;
  function CreateParameterNotFound: EZDatabaseError;
  var DataSet: TDataSet;
    Message: String;
  begin
    DataSet := GetDataSet;
    Message := Format(SParameterNotFound, [Value]);
    if Assigned(DataSet) and (DataSet.Name <> '') then
      Result := EZDatabaseError.Create(Format('%s: %s', [DataSet.Name, Message])) else
      Result := EZDatabaseError.Create(Message);
  end;
begin
  Result := FindParam(Value);
  if Result = nil then
    raise CreateParameterNotFound;
end;

procedure TZParams.RemoveParam(Value: TZParam);
begin
  Value.Collection := nil;
end;

type TZHackAbstractRODataset = class(TZAbstractRODataset);

procedure TZParams.SetArraySize(Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FArraySize then begin
    for i := 0 to Count -1 do
      TZParam(Items[i]).SetArraySize(Value);
    if (Value = 0) and (FOwner <> nil) and (FOwner.InheritsFrom(TZAbstractRODataset)) then
      with TZHackAbstractRODataset(FOwner as TZAbstractRODataset) do
        if (Statement <> nil) then Statement.ClearParameters;
    FArraySize := Value;
  end;
end;

procedure TZParams.SetItem(Index: Integer; Value: TZParam);
begin
  inherited Items[Index] := Value;
end;

procedure TZParams.SetParamValue(const ParamName: string; const Value: Variant);
var
  I: Integer;
  Params: {$IFDEF WITH_GENERIC_TPARAM_LIST}TList<TZParam>{$ELSE}TList{$ENDIF};
begin
  if ZFastCode.Pos(';', ParamName) <> 0 then begin
    Params := {$IFDEF WITH_GENERIC_TPARAM_LIST}TList<TZParam>{$ELSE}TList{$ENDIF}.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TZParam(Params[I]).Value := Value[I];
    finally
      Params.Free;
    end;
  end else
    ParamByName(ParamName).Value := Value;
end;

procedure TZParams.Update(Item: TCollectionItem);
var i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FParamRef := nil;
  inherited Update(Item);
end;

initialization
  D1M1Y1 := EncodeDate(1,1,1);
{$ENDIF DISABLE_ZPARAM}
end.
