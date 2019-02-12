{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZDbcOracleUtils;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs{$ELSE}ZClasses{$ENDIF}, ZDbcUtils, ZSelectSchema,
  ZSysUtils, ZDbcIntfs, ZVariant, ZPlainOracleDriver, ZDbcLogging,
  ZCompatibility, ZPlainOracleConstants, FmtBCD;

const
  MAX_SQLVAR_LIMIT = 1024;
  Max_OCI_String_Size = 4000; //prevent 'OCI_ERROR: ORA-01459: invalid length for variable character string' if buffer is to small
  Max_OCI_Raw_Size = 2000;

  NO_DTYPE = 0;
  SQLType2OCIDescriptor: array[stBoolean..stBinaryStream] of sb2 = (
    NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE,  //ordinals
    NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, //floats
    NO_DTYPE, OCI_DTYPE_TIMESTAMP, OCI_DTYPE_TIMESTAMP, //time values
    NO_DTYPE, //GUID
    NO_DTYPE, NO_DTYPE, NO_DTYPE,  //varying size types in equal order minimum sizes for 8Byte alignment
    OCI_DTYPE_LOB, OCI_DTYPE_LOB, OCI_DTYPE_LOB); //lob's

type
  { a struct for the ora long (char/byte) types }
  PPOCILong = ^POCILong;
  POCILong = ^TOCILong;
  TOCILong = record
    Len: sb4;
    data: array[0..7] of Byte; //just something for debugging
  end;
  { a struct for the ora var(char/byte) types }
  POCIVary = ^TOCIVary;
  TOCIVary = record
    Len: sb2;
    data: array[0..7] of Byte; //just something for debugging
  end;
  {** Declares SQL Object }
  POCIObject = ^TOCIObject;
  TObjFields = array of POCIObject;
  TOCIObject = Record                 // embedded object or table will work recursively
    type_name:      String;           //object's name (TDO)
    type_schema:    String;           //object's schema name (TDO)
    parmdp:         POCIParam;        //Describe attributes of the object OCI_DTYPE_PARAM
    parmap:         POCIParam;        //Describe attributes of the object OCI_ATTR_COLLECTION_ELEMENT OCI_ATTR_PARAM
    tdo:            POCIType;         //object's TDO handle
    typecode:       OCITypeCode;      //object's OCI_ATTR_TYPECODE
    col_typecode:   OCITypeCode;      //if collection this is its OCI_ATTR_COLLECTION_TYPECODE
    elem_typecode:  OCITypeCode;      //if collection this is its element's OCI_ATTR_TYPECODE
    obj_ref:        POCIRef;          //if an embeded object this is ref handle to its TDO
    obj_ind:        POCIInd;          //Null indictator for object
    obj_value:      POCIComplexObject;//the actual value from the DB
    obj_type:       POCIType;         //if an embeded object this is the  OCIType returned by a OCIObjectPin
    is_final_type:  ub1;              //object's OCI_ATTR_IS_FINAL_TYPE
    fields:         TObjFields;       //one object for each field/property
    field_count:    ub2;              //The number of fields Not really needed but nice to have
    next_subtype:   POCIObject;       //There is strored information about subtypes for inherited objects
    stmt_handle:    POCIStmt;         //the Statement-Handle
    Level:          Integer;          //the instance level
    Pinned:         Boolean;          //did we pin the obj on decribe?
  end;

  PUB2Array = ^TUB2Array;
  TUB2Array = array[0..0] of ub2;
  PSB2Array = ^TSB2Array;
  TSB2Array = array[0..0] of sb2;

  PZOCIParamBind = ^TZOCIParamBind;
  TZOCIParamBind = record
    {OCI bind Handles}
    bindpp:     POCIBind; //An address of a bind handle which is implicitly allocated by this call. The bind handle maintains all the bind information for this particular input value. The handle is freed implicitly when the statement handle is deallocated. On input, the value of the pointer must be null or a valid bind handle. binding values
    valuep:     PAnsiChar; //An address of a data value or an array of data values of the type specified in the dty parameter. An array of data values can be specified for mapping into a PL/SQL table or for providing data for SQL multiple-row operations. When an array of bind values is provided, this is called an array bind in OCI terms.
                         //For SQLT_NTY or SQLT_REF binds, the valuep parameter is ignored. The pointers to OUT buffers are set in the pgvpp parameter initialized by OCIBindObject().
                         //If the OCI_ATTR_CHARSET_ID attribute is set to OCI_UTF16ID (replaces the deprecated OCI_UCS2ID, which is retained for backward compatibility), all data passed to and received with the corresponding bind call is assumed to be in UTF-16 encoding.
    value_sz:   sb4; //The size of a data value. In the case of an array bind, this is the maximum size of any element possible with the actual sizes being specified in the alenp parameter.
                     //descriptors, locators, or REFs, whose size is unknown to client applications use the size of the structure you are passing in; for example, sizeof (OCILobLocator *).
    dty:        ub2; //The data type of the value(s) being bound. Named data types (SQLT_NTY) and REFs (SQLT_REF) are valid only if the application has been initialized in object mode. For named data types, or REFs, additional calls must be made with the bind handle to set up the datatype-specific attributes.
    indp:       PSB2Array; //Pointer to an indicator variable or array. For all data types, this is a pointer to sb2 or an array of sb2s. The only exception is SQLT_NTY, when this pointer is ignored and the actual pointer to the indicator structure or an array of indicator structures is initialized by OCIBindObject(). Ignored for dynamic binds.
    {zeos}
    DescriptorType: sb4; //holds our descriptor type we use
    curelen:      ub4; //the actual number of elements

    Precision: sb2; //field.precision used 4 out params
    Scale:     sb1; //field.scale used 4 out params
    ParamName: String;
  end;
  PZOCIParamBinds = ^TZOCIParamBinds;
  TZOCIParamBinds = array[0..MAX_SQLVAR_LIMIT] of TZOCIParamBind; //just a nice dubugging range

  PZSQLVar = ^TZSQLVar;
  TZSQLVar = record
    {OCI Handles}
    valuep:     PAnsiChar; //An address of a data value or an array of data values of the type specified in the dty parameter. An array of data values can be specified for mapping into a PL/SQL table or for providing data for SQL multiple-row operations. When an array of bind values is provided, this is called an array bind in OCI terms.
                         //For SQLT_NTY or SQLT_REF binds, the valuep parameter is ignored. The pointers to OUT buffers are set in the pgvpp parameter initialized by OCIBindObject().
                         //If the OCI_ATTR_CHARSET_ID attribute is set to OCI_UTF16ID (replaces the deprecated OCI_UCS2ID, which is retained for backward compatibility), all data passed to and received with the corresponding bind call is assumed to be in UTF-16 encoding.
    value_sz:   ub4{(ub2 on describe columns)};
    dty:        ub2;
    indp:       PSB2Array; //Pointer to an indicator variable or array. For all data types, this is a pointer to sb2 or an array of sb2s. The only exception is SQLT_NTY, when this pointer is ignored and the actual pointer to the indicator structure or an array of indicator structures is initialized by OCIBindObject(). Ignored for dynamic binds.
    alenp:      PUB2Array; //Pointer to array of actual lengths of array elements. Each element in alenp is the length (in bytes, unless the data in valuep is in Unicode, when it is in codepoints) of the data in the corresponding element in the bind value array before and after the execute. This parameter is ignored for dynamic binds.

    {binding values}
    _Obj:      POCIObject;
    {Zeos proceesing values}
    DescriptorType: sb4; //holds our descriptor type we use
    Precision: sb2; //field.precision used 4 out params
    Scale:     sb1; //field.scale used 4 out params

    ColType:   TZSQLType; //Zeos SQLType
  end;

  TZSQLVars = record
    AllocNum:  ub4;
    Variables: array[0..MAX_SQLVAR_LIMIT] of TZSQLVar; //just a nice dubugging range
  end;
  PZSQLVars = ^TZSQLVars;

type
  {$A-}
  TOraDate = record
    Cent, Year, Month, Day, Hour, Min, Sec: Byte;
  end;
  POraDate = ^TOraDate;
  {$A+}
{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: TZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const {%H-}ConSettings: PZConSettings);

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;

function NormalizeOracleTypeToSQLType(var DataType: ub2; var DataSize: ub4;
  out DescriptorType: sb4; Precision, Scale: sb2; ConSettings: PZConSettings;
  IO: OCITypeParamMode): TZSQLType;

  {**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(const PlainDriver: TZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);

function DescribeObject(const PlainDriver: TZOraclePlainDriver; const Connection: IZConnection;
  ParamHandle: POCIParam; {%H-}stmt_handle: POCIHandle; Level: ub2): POCIObject;

procedure OraWriteLob(const PlainDriver: TZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);


procedure BCD2Nvu(const bcd: TBCD; num: POCINumber);
procedure Nvu2BCD(num: POCINumber; var bcd: TBCD);


type { implements an enumerator of a dedected pascal type from an oracle number }
  TnvuKind = (nvu0, nvuNegInf, nvuPosInf,
                        vnuNegInt, vnuPosInt,
                        vnuNegCurr, vnuPosCurr,
                        nvuBigDecimal);
  TZvnuInfo = record
    Scale                     : ShortInt;
    Exponent                  : ShortInt;
    { dump some values }
    Len                       : Byte;
    Precision                 : Byte;
    FirstBase100Digit         : Byte;
    FirstBase100DigitDiv10Was0: Boolean;
    LastBase100DigitMod10Was0 : Boolean;
  end;

{** EH:
  detects the pascal type from an oracle ocie number oracle number
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @return an an enumerator of a dedected pascal type
}
function nvuKind(num: POCINumber; var vnuInfo: TZvnuInfo): TnvuKind; //{$IFDEF WITH_INLINE}inline{$ENDIF};

{** EH:
  convert a positive oracle oci number into currency value
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @return a converted value
}
function PosNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{** EH:
  convert a negative oracle oci number into currency value
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @return a converted value
}
function NegNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{** EH:
  convert a positve oracle oci number into a unsigned longlong
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @return a converted value
}
function PosNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): UInt64; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{** EH:
  convert a negative oracle oci number into a signed longlong
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @return a converted value
}
function NegNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): Int64; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}


{** EH:
  writes a negative unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
function PosOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{** EH:
  writes a positive unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
function NegOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;  {$IFDEF WITH_INLINE}inline;{$ENDIF}

{** EH:
  converts a currency value to a oracle oci number
  to be clear: this might not be the fastest way ( the mul/divs are slow)
  but is accurate in contrary to using the doubles
  @param value the currency to be converted
  @param num the pointer to the oci-number
}
procedure Curr2Vnu(const Value: Currency; num: POCINumber);

function OCIType2Name(DataType: ub2): String;

const
  VNU_NUM_INTState: array[Boolean] of TnvuKind = (vnuNegInt, vnuPosInt);
  VNU_NUM_CurState: array[Boolean] of TnvuKind = (vnuNegCurr, vnuPosCurr);
  sPosScaleFaktor: array[0..18] of Int64 = (
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000,
      10000000000,
      100000000000,
      1000000000000,
      10000000000000,
      100000000000000,
      1000000000000000,
      10000000000000000,
      100000000000000000,
      1000000000000000000);
  sCurrScaleFaktor: array[0..4] of Integer = (
      1,
      10,
      100,
      1000,
      10000);
  NVU_CurrencyExponents: array[0..10] of Integer =
    (-2,-1, 0, 1, 2, 3, 4, 5, 6, 7, 8);
  {$IFNDEF WITH_UINT64_C1118_ERROR}
  uPosScaleFaktor: array[0..19] of UInt64 = (
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000,
      10000000000,
      100000000000,
      1000000000000,
      10000000000000,
      100000000000000,
      1000000000000000,
      10000000000000000,
      100000000000000000,
      1000000000000000000,
      10000000000000000000);
  UInt64Divisor: array[0..10] of UInt64 = (
      1,
      100,
      10000,
      1000000,
      100000000,
      10000000000,
      1000000000000,
      100000000000000,
      10000000000000000,
      1000000000000000000,
      10000000000000000000);
  {$ELSE}
var
  uPosScaleFaktor: array[0..19] of UInt64;
  UInt64Divisor: array[0..10] of UInt64;
  {$ENDIF}

type
  { oracle loves it's recursion ... so we need a recursive obj model }
  TZOraProcDescriptor_A = class(TObject)
  private
    FParent: TZOraProcDescriptor_A;

    procedure InternalDescribeObject(Obj: POCIHandle;
      {$IFDEF AUTOREFCOUNT} const {$ENDIF}PlainDriver: TZOraclePlainDriver;
      ErrorHandle: POCIError; ConSettings: PZConSettings);

    function InternalDescribe(const Name: String; _Type: UB4;
      {$IFDEF AUTOREFCOUNT} const {$ENDIF}PlainDriver: TZOraclePlainDriver;
      ErrorHandle: POCIError; OCISvcCtx: POCISvcCtx; Owner: POCIHandle;
      ConSettings: PZConSettings): Sword;
  public
    procedure Describe(_Type: UB4; const Connection: IZConnection;
      const Name: {$IFDEF UNICODE}String{$ELSE}RawByteString{$ENDIF});

    {$IFDEF UNICODE}
    procedure ConcatParentName(NotArgName: Boolean; var Buf: TUCS2Buff; var Result: String; const IC: IZIdentifierConvertor);
    {$ELSE}
    procedure ConcatParentName(NotArgName: Boolean; var Buf: TRawBuff; var Result: RawByteString; const IC: IZIdentifierConvertor);
    {$ENDIF}
    constructor Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}Parent: TZOraProcDescriptor_A);
    destructor Destroy; override;
  public
    Args: TObjectList;
    SchemaName, AttributeName, TypeName: String;
    ObjType, Precision, Radix: UB1;
    Scale: SB1;
    DataSize: UB4;
    DataType: UB2;
    OverloadID: ub2;
    DescriptorType: SB4;
    IODirection: OCITypeParamMode;
    OrdPos: ub2;
    SQLType: TZSQLType;
    property Parent: TZOraProcDescriptor_A read FParent;
  end;

implementation

uses Math, ZMessages, ZDbcOracle, ZDbcOracleResultSet,
  ZEncoding, ZFastCode {$IFNDEF NO_UNIT_CONTNRS},ZClasses{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IFDEF UNICODE},StrUtils{$ENDIF};
(* Oracle Docs: https://docs.oracle.com/cd/B28359_01/appdev.111/b28395/oci03typ.htm#i423688
Oracle stores values of the NUMBER datatype in a variable-length format.
The first byte is the exponent and is followed by 1 to 20 mantissa bytes.
The high-order bit of the exponent byte is the sign bit;
it is set for positive numbers and it is cleared for negative numbers.
The lower 7 bits represent the exponent, which is a base-100 digit with an offset of 65.

To calculate the decimal exponent, add 65 to the base-100 exponent and
add another 128 if the number is positive. If the number is negative,
you do the same, but subsequently the bits are inverted.
For example, -5 has a base-100 exponent = 62 (0x3e).
The decimal exponent is thus (~0x3e) -128 - 65 = 0xc1 -128($7f) -65 = 193 -128($7f) -65 = 0.

Each mantissa byte is a base-100 digit, in the range 1..100. For positive numbers,
the digit has 1 added to it. So, the mantissa digit for the value 5 is 6.
For negative numbers, instead of adding 1, the digit is subtracted from 101.
So, the mantissa digit for the number -5 is 96 (101 - 5).
Negative numbers have a byte containing 102 appended to the data bytes.
However, negative numbers that have 20 mantissa bytes do not have the trailing 102 byte.
Because the mantissa digits are stored in base 100, each byte can represent 2 decimal digits.
The mantissa is normalized; leading zeroes are not stored.

Up to 20 data bytes can represent the mantissa. However,
only 19 are guaranteed to be accurate.
The 19 data bytes, each representing a base-100 digit,
yield a maximum precision of 38 digits for an Oracle NUMBER.

If you specify the datatype code 2 in the dty parameter of an OCIDefineByPos() call,
your program receives numeric data in this Oracle internal format.
The output variable should be a 21-byte array to accommodate the largest possible number.
Note that only the bytes that represent the number are returned.
There is no blank padding or NULL termination.
If you need to know the number of bytes returned,
use the VARNUM external datatype instead of NUMBER
*)

{** EH:
  convert a positive oracle oci number into a unsigned longlong
  @param num the pointer to the oci-number
  @param Neg100FactorCnt a scale for truncation if positive or base 100 multiplication if negative
  @return a converted value
}
function PosNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): UInt64;
var i: Byte;
begin
  {$R-} {$Q-}
  { initialize with first positive base-100-digit }
  Result := vnuInfo.FirstBase100Digit;
  { skip len, exponent and first base-100-digit -> start with 3}
  for i := 3 to vnuInfo.Len do
    Result := Result * 100 + Byte(num[i] - 1);
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then
    Result := Result * uPosScaleFaktor[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{** EH:
  convert a negative oracle oci number into a signed longlong
  @param num the pointer to the oci-number
  @param Neg100FactorCnt a scale for truncation if positive or base 100 multiplication if negative
  @param Len a true len of num gigits to work with
  @return a converted value
}
function NegNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): Int64;
var i: Byte;
begin
  {$R-} {$Q-}
  { initialize with first negative base-100-digit }
  Result := -ShortInt(vnuInfo.FirstBase100Digit); //init
  { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
  for i := 3 to vnuInfo.Len do
    Result := Result * 100 - (101 - num[i]);
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then
    Result := Result * sPosScaleFaktor[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{** EH:
  convert a positive oracle oci number into currency value
  @param num the pointer to the oci-number
  @param scale a given scale to align scale to 4 decimal digits
  @return a converted value
}
function PosNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency;
var I64: Int64 absolute Result;
  i: ShortInt;
begin
  {$R-} {$Q-}
  { initialize with first positive base-100-digit }
  I64 := vnuInfo.FirstBase100Digit;
  { skip len, exponent and first base-100-digit -> start with 3}
  for i := 3 to vnuInfo.Len do
    i64 := i64 * 100 + Byte(num[i] - 1);
  I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{**
  convert a negative oracle oci number into currency value
  @param num the pointer to the oci-number
  @param scale a given scale to align scale to 4 decimal digits
  @return a converted value
}
function NegNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency;
var I64: Int64 absolute Result;
  i: ShortInt;
begin
  {$R-} {$Q-}
  i64 := -ShortInt(vnuInfo.FirstBase100Digit); //init
  { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
  for i := 3 to vnuInfo.Len do
    i64 := i64 * 100 - (101 - num[i]);
  I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{** EH:
  converts a currency value to a oracle oci number
  to be clear: this might not be the fastest way ( the mul/divs are slow)
  but is accurate in contrary to using the doubles
  @param value the currency to be converted
  @param num the pointer to the oci-number
}
procedure Curr2Vnu(const Value: Currency; num: POCINumber);
(* this version writes from left to right
var I64: UInt64;
  Positive: Boolean;
  i, n, p, trailing_zeros: Byte;
  Exponent: ShortInt;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    num[0] := 1;
    num[1] := $80;
  end else begin
    Positive := Value > 0;
    if Positive
    then I64 :=  PInt64(@Value)^
    else I64 := -PInt64(@Value)^;
    i := 2;
    P := 0;
    trailing_zeros := 0;
    { reduce the int64 muls/mods by checking the high bytes for leading dbl zeros
      the docs: "The mantissa is normalized; leading zeroes are not stored."
      EH: also right packing the trailing zeros by using the exponents makes
      reading the values back loads faster}
    for n := Low(UInt64Divisor) to High(UInt64Divisor)-(5*Ord(Int64Rec(I64).Hi=0)) do
      if I64 >= UInt64Divisor[n]
      then P := N
      else Break;
    Inc(P);
    Exponent := NVU_CurrencyExponents[p];
    for P := P downto 1 do begin
      n := (I64 mod UInt64Divisor[p] div UInt64Divisor[p-1]);
      if (n = 0) then begin
        if ((i=2))
        then continue
        else Inc(trailing_zeros);
      end else
         trailing_zeros := 0; //reset again
      if Positive
      then num[i] := n + 1
      else num[i] := 101 - n;
      Inc(i);
    end;
    Dec(i, trailing_zeros+Ord(Positive));
    if Positive then
      num[1] := (64+Exponent) or $80
    else begin
      num[1] := not(64+Exponent) and $7f;
      num[i] := 102; //"Negative numbers have a byte containing 102 appended to the data bytes."
    end;
    num[0] := i;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
and this version writes from right to left*)
var I64, IDiv100, IMul100: UInt64;
  x{$IFNDEF CPUX64}, c32, cDiv100, cMul100{$ENDIF}: Cardinal;
  Negative: Boolean;
  i, Digits, l: Byte;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    num[0] := 1;
    num[1] := $80;
    Exit;
  end;
  Digits := GetOrdinalDigits(PInt64(@Value)^, i64, Negative);
  Digits := (Digits+Ord(Odd(Digits))) div 2;
  I := Digits+1;
  L := I;
  while I > {$IFNDEF CPUX64}6{$ELSE}2{$ENDIF} do begin
    IDiv100 := I64 div 100; {dividend div 100}
    IMul100 := IDiv100*100; {remainder}
    X := I64-IMul100; {dividend mod 100}
    I64 := IDiv100; {next dividend }
    if (X = 0) and (I=L) then
      Dec(L)
    else if Negative
      then num[I] := 101 - X
      else num[I] := X + 1;
    Dec(I);
  end;
  {$IFNDEF CPUX64}
  C32 := Int64Rec(I64).Lo;
  while I > 2 do begin
    cDiv100 := C32 div 100; {dividend div 100}
    cMul100 := cDiv100*100; {remainder}
    x := c32-cMul100; {dividend mod 100}
    C32 := cDiv100; {next dividend }
    if (x = 0) and (I=L) then
      Dec(L)
    else if Negative
      then num[I] := 101 - X
      else num[I] := x + 1;
    Dec(I);
  end;
  {$ENDIF}
  if Negative then begin
    num[1] := not(64+NVU_CurrencyExponents[Digits]) and $7f;
    num[I] := 101 - Byte({$IFNDEF CPUX64}C32{$ELSE}I64{$ENDIF});
    num[L+1] := 102; //"Negative numbers have a byte containing 102 appended to the data bytes."
    num[0] := L+1;
  end else begin
    num[1] := (64+NVU_CurrencyExponents[Digits]) or $80;
    num[I] := Byte({$IFNDEF CPUX64}C32{$ELSE}I64{$ENDIF}) + 1;
    num[0] := L;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{**  EH:
  writes a negative unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
function PosOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
var i: Byte;
  PStart: PAnsiChar;
begin
  {$R-} {$Q-}
  PStart := Buf;
  if vnuInfo.FirstBase100DigitDiv10Was0 then begin
    PByte(Buf)^ := Ord('0')+vnuInfo.FirstBase100Digit;
    Inc(Buf);
  end else begin
    PWord(Buf)^ := Word(TwoDigitLookupW[vnuInfo.FirstBase100Digit]);
    Inc(Buf,2);
  end;
  for I := 3 to vnuInfo.Len do begin
    PWord(Buf)^ := Word(TwoDigitLookupW[Byte(num[i] - 1)]);
    Inc(Buf,2);
  end;
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then begin
    i := vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0);
    while i >= 2 do begin
      PWord(Buf)^ := 12336;
      Inc(Buf,2);
      Dec(i, 2);
    end;
    if i > 0 then
      PByte(Buf)^ := Ord('0');
  end;
  Result := Buf-PStart;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{** EH:
  writes a positive unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
function NegOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
var i: Byte;
  PStart: PAnsiChar;
begin
  {$R-} {$Q-}
  PStart := Buf;
  PByte(Buf)^ := Ord('-');
  if vnuInfo.FirstBase100DigitDiv10Was0 then begin
    PByte(Buf+1)^ := Ord('0')+vnuInfo.FirstBase100Digit;
    Inc(Buf, 2);
  end else begin
    PWord(Buf+1)^ := Word(TwoDigitLookupW[vnuInfo.FirstBase100Digit]);
    Inc(Buf,3);
  end;
  for I := 3 to vnuInfo.Len do begin
    PWord(Buf)^ := Word(TwoDigitLookupW[Byte(101 - num[i])]);
    Inc(Buf,2);
  end;
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then begin
    i := vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0);
    while i >= 2 do begin
      PWord(Buf)^ := 12336;
      Inc(Buf,2);
      Dec(i, 2);
    end;
    if i > 0 then
      PByte(Buf)^ := Ord('0');
  end;
  Result := Buf-PStart;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

function nvuKind(num: POCINumber; var vnuInfo: TZvnuInfo): TnvuKind;
var
  Positive: Boolean;
begin
  {$R-} {$Q-}
  Result := nvuBigDecimal;
  vnuInfo.len := num[0];
  vnuInfo.FirstBase100Digit := num[2]; //dump first digit
  vnuInfo.Precision := num[1];//dump the value -> access packet stucts is dead slow
  if (vnuInfo.len=1) and ((vnuInfo.Precision=$80) or (vnuInfo.Precision=$c1)) then
    Result := nvu0
  else if (vnuInfo.len=1) and (vnuInfo.Precision= 0) then
    Result := nvuNegInf
  else if (vnuInfo.len=2) and (vnuInfo.Precision = 255) and (vnuInfo.FirstBase100Digit = 101) then
    Result := nvuPosInf
  else begin
    Positive := (vnuInfo.Precision and $80)=$80;
    if Positive then begin
      vnuInfo.Exponent := (vnuInfo.Precision and $7f)-65;
      vnuInfo.FirstBase100Digit := vnuInfo.FirstBase100Digit - 1;
      vnuInfo.LastBase100DigitMod10Was0 := (num[vnuInfo.len] - 1) mod 10 = 0;
    end else begin
      vnuInfo.Exponent := (not(vnuInfo.Precision) and $7f)-65;
      if Num[vnuInfo.Len] = 102 then//last byte does not count if 102
        Dec(vnuInfo.len);
      vnuInfo.FirstBase100Digit := (101 - vnuInfo.FirstBase100Digit);
      vnuInfo.LastBase100DigitMod10Was0 := (101 - num[vnuInfo.len]) mod 10 = 0;
    end;
    { align scale and precision! this took me ages and dozens of tests }
    if vnuInfo.Exponent < 0 then begin
      vnuInfo.Precision := (Abs(vnuInfo.Exponent) - 1) shl 1 + (vnuInfo.Len - 1) shl 1;
      vnuInfo.Scale := vnuInfo.Precision;
    end else if vnuInfo.Exponent >= (vnuInfo.Len - 1) then begin //int range ?
      vnuInfo.Precision := (vnuInfo.Exponent + 1) shl 1;
      vnuInfo.Scale := 0;
    end else begin
      vnuInfo.Precision := (vnuInfo.Len - 1) shl 1;
      vnuInfo.Scale := vnuInfo.Precision - (vnuInfo.Exponent + 1) shl 1;
    end;
    { final scale and prec calculation -> check first and last digit }
    vnuInfo.FirstBase100DigitDiv10Was0 := (vnuInfo.FirstBase100Digit div 10 = 0);
    Dec(vnuInfo.Precision, Ord(vnuInfo.FirstBase100DigitDiv10Was0));
    if vnuInfo.LastBase100DigitMod10Was0 then begin
      if (vnuInfo.Scale > 0) then
        Dec(vnuInfo.Scale);
      Dec(vnuInfo.Precision);
    end;

    { EH: we just test if we're in scale and precision range ..
      We don't know this for sure and sadly oracle gives us no way to find this out!
      Nice would by the ATTRIBUT min_val/max_val which is available 4 describing sequences only ):

      Note: Oracle always returns the significant decimal digits! }
    if (vnuInfo.Scale = 0) and (vnuInfo.Precision <= 19+Ord(Positive)) then
      Result := VNU_NUM_INTState[Positive]
    else if (vnuInfo.Scale>0) and (vnuInfo.Scale <= 4) and
            (vnuInfo.Precision <= sAlignCurrencyScale2Precision[vnuInfo.Scale]) then
      Result := VNU_NUM_CurState[Positive];
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

// Conversions
// original Autor might be Joost van der Sluis
// Code is from oracleconnection.pp of FPC
//Procedure FmtBCD2Nvu(bcd:tBCD;b:pByte);
procedure BCD2Nvu(const bcd: TBCD; num: POCINumber);
var
  i,j,cnt   : integer;
  nibbles   : array [0..maxfmtbcdfractionsize-1] of byte;
  exp       : shortint;
  bb        : byte;
begin
  //fillchar(num[0],22,#0);
  FillChar(num[0],SizeOf(TOCINUmber),#0);
  if BCDPrecision(bcd)=0 then begin// zero, special case
    num[0] := 1;
    num[1] := $80;
  end else begin
    if (BCDPrecision(bcd)-BCDScale(bcd)) mod 2 <>0 then begin// odd number before decimal point
      nibbles[0] := 0;
      j := 1;
    end else
      j := 0;
    for i := 0 to bcd.Precision -1 do
      if i mod 2 =0
      then nibbles[i+j] := bcd.Fraction[i div 2] shr 4
      else nibbles[i+j] := bcd.Fraction[i div 2] and $0f;
    nibbles[bcd.Precision+j] := 0; // make sure last nibble is also 0 in case we have odd scale
    exp := (BCDPrecision(bcd)-BCDScale(bcd)+1) div 2;
    cnt := exp+(BCDScale(bcd)+1) div 2;
    // to avoid "ora 01438: value larger than specified precision allowed for this column"
    // remove trailing zeros (scale < 0)...
    while (nibbles[cnt*2-2]*10+nibbles[cnt*2-1])=0 do
      cnt := cnt-1;
    // ... and remove leading zeros (scale > precision)
    j:=0;
    while (nibbles[j*2]*10+nibbles[j*2+1])=0 do begin
      j:=j+1;
      exp:=exp-1;
    end;
    if IsBCDNegative(bcd) then begin
      num[0]:=cnt-j+1;
      num[1]:=not(exp+64) and $7f ;
      for i:=j to cnt-1 do begin
        bb:=nibbles[i*2]*10+nibbles[i*2+1];
        num[2+i-j]:=101-bb;
        end;
      if 2+cnt-j<22 then begin // add a 102 at the end of the number if place left.
        num[0]:=num[0]+1;
        num[2+cnt-j]:=102;
      end;
    end else begin
      num[0]:=cnt-j+1;
      num[1]:=(exp+64) or $80 ;
      for i:=j to cnt-1 do begin
        bb:=nibbles[i*2]*10+nibbles[i*2+1];
        num[2+i-j]:=1+bb;
        end;
      end;
    end;
end;

// Conversions
// original Autor might be Joost van der Sluis
// Code is from oracleconnection.pp of FPC
procedure Nvu2BCD(num: POCINumber; var bcd: TBCD);
var
  i,j       : integer;
  bb,size   : byte;
  exp       : shortint;
  nibbles   : array [0..MaxFMTBcdFractionSize-1] of byte;
  scale     : integer;
begin
  size := num[0];
  if (size=1) and (num[1]=$80) then begin// special representation for 0
    //bcd:=IntegerToBCD(0)
    FillChar(bcd, SizeOf(bcd), #0);
    bcd.Precision := 10;
    bcd.SignSpecialPlaces := 2;
    Exit;
  end;
  bcd.SignSpecialPlaces := 0; //sign positive, non blank, scale 0
  //bcd.Precision:=1;         //BCDNegate works only if Precision <>0
  if (num[1] and $80)=$80 then begin// then the number is positive
    exp := (num[1] and $7f)-65;
    for i := 0 to size-2 do begin
      bb := num[i+2]-1;
      nibbles[i*2]:=bb div 10;
      nibbles[i*2+1]:=(bb mod 10);
    end;
  end else begin
    bcd.SignSpecialPlaces := bcd.SignSpecialPlaces xor $80; //BCDNegate(bcd);
    exp := (not(num[1]) and $7f)-65;
    if num[size]=102 then  // last byte doesn't count if = 102
      dec(Size);//size:=size-1;
    for i := 0 to size-2 do begin
      bb := 101-num[i+2];
      nibbles[i*2] := bb div 10;
      nibbles[i*2+1] := (bb mod 10);
    end;
  end;
  nibbles[(size-1)*2] := 0;
  bcd.Precision:=(size-1)*2;
  scale := bcd.Precision-(exp*2+2);
  if scale>=0 then begin
    if (scale > bcd.Precision) then begin // need to add leading 0s
      for i:=0 to (scale-bcd.Precision+1) div 2 do
        bcd.Fraction[i]:=0;
      i:=scale-bcd.Precision;
      bcd.Precision:=scale;
    end else
      i:=0;
    j:=i;
    if (i=0) and (nibbles[0]=0) then begin// get rid of leading zero received from oci
      bcd.Precision:=bcd.Precision-1;
      j:=-1;
    end;
    while i<=bcd.Precision do begin// copy nibbles
      if i mod 2 =0
      then bcd.Fraction[i div 2]:=nibbles[i-j] shl 4
      else bcd.Fraction[i div 2]:=bcd.Fraction[i div 2] or nibbles[i-j];
      Inc(i);//i:=i+1;
    end;
    bcd.SignSpecialPlaces:=bcd.SignSpecialPlaces or scale;
  end else begin // add trailing zeroes, increase precision to take them into account
    i:=0;
    while i<=bcd.Precision do begin// copy nibbles
      if i mod 2 =0
      then bcd.Fraction[i div 2]:=nibbles[i] shl 4
      else bcd.Fraction[i div 2]:=bcd.Fraction[i div 2] or nibbles[i];
      Inc(i);//i:=i+1;
    end;
    bcd.Precision:=bcd.Precision-scale;
    for i := size -1 to High(bcd.Fraction) do
      bcd.Fraction[i] := 0;
  end;
end;

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);
var
  Size: Integer;
begin
  if Variables <> nil then
    FreeMem(Variables);

  Size := SizeOf(ub4) + Max(1,Count) * SizeOf(TZSQLVar);
  GetMem(Variables, Size);
  FillChar(Variables^, Size, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  Variables^.AllocNum := Count;
end;

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: TZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const ConSettings: PZConSettings);
var
  I: Integer;
  J: NativeUInt;
  CurrentVar: PZSQLVar;
  Status: Sword;

  procedure DisposeObject(var Obj: POCIObject);
  var
    I: Integer;
  begin
    for i := 0 to High(Obj.fields) do
      DisposeObject(Obj.fields[i]);
    SetLength(Obj.fields, 0);
    if Assigned(Obj.next_subtype) then
    begin
      DisposeObject(Obj.next_subtype);
      Obj.next_subtype := nil;
    end;
    if Obj.Pinned then
      {Unpin tdo}
      //CheckOracleError(PlainDriver, ErrorHandle, //debug
        PlainDriver.OCIObjectUnpin(Handle,ErrorHandle, CurrentVar^._Obj.tdo)
        ;//debug, lcOther, 'OCIObjectUnpin', ConSettings);
    if (Obj.Level = 0) and assigned(Obj.tdo) then
      {Free Object}
      //debugCheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIObjectFree(Handle,ErrorHandle, CurrentVar^._Obj.tdo, 0)
      ;//debug, lcOther, 'OCIObjectFree', ConSettings);
    Dispose(Obj);
    Obj := nil;
  end;

begin
  if Variables <> nil then begin
    { Frees allocated memory for output variables }
    for I := 0 to Integer(Variables.AllocNum)-1 do begin
      {$R-}
      CurrentVar := @Variables.Variables[I];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      if Assigned(CurrentVar^._Obj) then
        DisposeObject(CurrentVar^._Obj);
      if (CurrentVar^.valuep <> nil) then
        if (CurrentVar^.DescriptorType > 0) then begin
          for J := 0 to Iteration-1 do
            if (PPOCIDescriptor(CurrentVar^.valuep+(J*SizeOf(Pointer))))^ <> nil then begin
              Status := PlainDriver.OCIDescriptorFree(PPOCIDescriptor(CurrentVar^.valuep+(J*SizeOf(Pointer)))^,
                CurrentVar^.DescriptorType);
              if Status <> OCI_SUCCESS then
                CheckOracleError(PlainDriver, ErrorHandle, status, lcOther, 'OCIDescriptorFree', ConSettings);
            end;
        end else if CurrentVar^.dty = SQLT_VST then
          for J := 0 to Iteration-1 do begin
            Status := PlainDriver.OCIStringResize(Handle, ErrorHandle, 0, PPOCIString(CurrentVar^.valuep+(J*SizeOf(POCIString))));
            if Status <> OCI_SUCCESS then
              CheckOracleError(PlainDriver, ErrorHandle, status, lcOther, 'OCIDescriptorFree', ConSettings);
          end;
      end;
    FreeMem(Variables);
    Variables := nil;
  end;
end;

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;
var TypeNameUp: string;
begin
  TypeNameUp := UpperCase(TypeName);

  if (TypeNameUp = 'CHAR') or (TypeNameUp = 'VARCHAR2') then
    Result := stString
  else if (TypeNameUp = 'NCHAR') or (TypeNameUp = 'NVARCHAR2') then
    Result := stString
  else if (TypeNameUp = 'FLOAT') or (TypeNameUp = 'BINARY_FLOAT') or (TypeNameUp = 'BINARY_DOUBLE') then
    Result := stDouble
  else if TypeNameUp = 'DATE' then  {precission - 1 sec, so Timestamp}
    Result := stTimestamp
  else if TypeNameUp = 'BLOB' then
    Result := stBinaryStream
  else if (TypeNameUp = 'RAW') then
    Result := stBytes
  else if (TypeNameUp = 'LONG RAW') then
    Result := stBinaryStream
  else if TypeNameUp = 'CLOB' then
    Result := stAsciiStream
  else if TypeNameUp = 'NCLOB' then
    Result := stAsciiStream
  else if TypeNameUp = 'LONG' then
    Result := stAsciiStream
  else if (TypeNameUp = 'ROWID') or (TypeNameUp = 'UROWID') then
    Result := stString
  else if StartsWith(TypeNameUp, 'TIMESTAMP') then
    Result := stTimestamp
  else if TypeNameUp = 'BFILE' then
    Result := stBinaryStream else
  if TypeNameUp = 'NUMBER' then begin
    if (Scale = 0) and (Precision > 0) and (Precision < 20) then begin
      if Precision <= 2 then
        Result := stByte
      else if Precision <= 4 then
        Result := stSmall
      else if Precision <= 9 then
        Result := stInteger
      else
        Result := stLong
    end else if (Scale >= 0) and (Scale <= 4) and (Precision > 0) and (Precision <= sAlignCurrencyScale2Precision[Scale]) then
      Result := stCurrency
    else
      Result := stBigDecimal;  { default for number types}
  end
  else if StartsWith(TypeNameUp, 'INTERVAL') then
    Result := stTimestamp
  else
    Result := stUnknown;
  if ( CtrlsCPType = cCP_UTF16 ) then
    case result of
      stString: Result := stUnicodeString;
      stAsciiStream: if not (TypeNameUp = 'LONG') then Result := stUnicodeStream; //fix: hhttp://zeoslib.sourceforge.net/viewtopic.php?t=3530
    end;
end;

function NormalizeOracleTypeToSQLType(var DataType: ub2; var DataSize: ub4;
  out DescriptorType: sb4; Precision, Scale: sb2; ConSettings: PZConSettings;
  IO: OCITypeParamMode): TZSQLType;
label VCS;
begin
  //some notes before digging in:
  // orl.h:
  // Strings:
  // "An ADT attribute declared as "x CHAR(n)" is mapped to "OCIString *x;""

  //The variable-length string is represented in C as a pointer to OCIString
  //structure. The OCIString structure is opaque to the user. Functions are
  //provided to allow the user to manipulate a variable-length string.
  //A variable-length string can be declared as:
  //OCIString *vstr;
  //For binding variables of type OCIString* in OCI calls (OCIBindByName(), * OCIBindByPos() and OCIDefineByPos()) use the external type code SQLT_VST.
  // same for OCIRaw

  // Any kind of Numbers
  //The OTS types: NUMBER, NUMERIC, INT, SHORTINT, REAL, DOUBLE PRECISION, * FLOAT and DECIMAL are represented by OCINumber.
  //The contents of OCINumber is opaque to clients.
  // For binding variables of type OCINumber in OCI calls (OCIBindByName(), * OCIBindByPos(), and OCIDefineByPos()) use the type code SQLT_VNU.

  // Any Kind of Time/data values:
  //OCIDate represents the C mapping of Oracle date.
  // This structure should be treated as an opaque structure as the format
  // of this structure may change. Use OCIDateGetDate/OCIDateSetDate
  // to access/initialize OCIDate.
  // For binding variables of type OCIDate in OCI calls (OCIBindByName(), * OCIBindByPos(), and OCIDefineByPos()) use the type code SQLT_ODT.

  Result := stUnknown; //init & satisfy the compiler
  DescriptorType := NO_DTYPE; //init
  case DataType of
    SQLT_NUM { NUMBER }, SQLT_PDN, SQLT_VNU { VARNUM recommended by Oracle!}:
        if (Scale = -127) and (Precision > 0) then begin
          //see: https://docs.oracle.com/cd/B13789_01/appdev.101/b10779/oci06des.htm
          //Table 6-14 OCI_ATTR_PRECISION/OCI_ATTR_SCALE
          Result := stDouble;
          DataType := SQLT_BDOUBLE;
          DataSize := SizeOf(Double);
        end else if (Scale = 0) and (Precision > 0) and (Precision < 19) then
          //No digits found, but possible signed or not/overrun of converiosn? No way to find this out -> just use a "save" type
          case Precision of
            1..2: begin // 0/-99..(-)99
                    Result := stShort;
                    DataType := SQLT_INT;
                    DataSize := SizeOf(ShortInt);
                  end;
            3..4: begin //(-)999..(-)9999
                    Result := stSmall; // -32768..32767
                    DataType := SQLT_INT;
                    DataSize := SizeOf(SmallInt);
                  end;
            5..9: begin //(-)99999..(-)999999999
                    Result := stInteger; // -2147483648..2.147.484.647
                    DataType := SQLT_INT;
                    DataSize := SizeOf(Integer);
                  end;
            else begin //(-)9999999999..(-)9999999999999999999
                    Result := stLong; //  -9223372036854775808..9.223.372.036.854.775.807
                    DataType := SQLT_INT;
                    DataSize := SizeOf(Int64);
                  end;
          end
        else begin
          DataType := SQLT_VNU; //see orl.h we can't use any other type using oci
          DataSize := SizeOf(TOCINumber);
          if (Scale >= 0) and (Scale <= 4) and (Precision > 0) and (Precision <= sAlignCurrencyScale2Precision[Scale])
          then Result := stCurrency
          else Result := stBigDecimal;
        end;
    SQLT_INT, _SQLT_PLI {signed short/int/long/longlong}: begin
        DataType := SQLT_INT;
        case DataSize of
          SizeOf(Int64):    Result := stLong;
          SizeOf(LongInt):  Result := stInteger;
          SizeOf(SmallInt): Result := stSmall;
          SizeOf(ShortInt): Result := stShort;
          else begin
            DataSize := SizeOf(Int64);
            Result := stLong;
          end;
        end;
      end;
    SQLT_FLT: if DataSize = SizeOf(Double) then begin
                  Result := stDouble;
                  DataType := SQLT_BDOUBLE;
                end else begin
                  Result := stFloat;
                  DataType := SQLT_BFLOAT;
                end;
    SQLT_AFC{ CHAR / char[n]}: begin
                if (DataSize = 0) then begin
                  if (IO <> OCI_TYPEPARAM_IN) then
                    DataSize := Max_OCI_String_Size;
                end;
                Result := stString;
              end;
    SQLT_RID, { char[n] }
    SQLT_AVC{CHAR / char[n+1]},
    SQLT_CHR, {VARCHAR2 / char[n+1]}
    SQLT_STR,{NULL-terminated STRING, char[n+1]}
    SQLT_VCS {VARCHAR / char[n+sizeof(short integer)]}: begin
VCS:            DataType := SQLT_VCS;
                if (DataSize = 0) then begin
                  if (IO <> OCI_TYPEPARAM_IN) then
                    DataSize := Max_OCI_String_Size+SizeOf(SmallInt);
                end else
                  DataSize := DataSize+SizeOf(SmallInt);
                Result := stString;
              end;
    SQLT_DAT: {char[7]} begin
              DataSize := SizeOf(TOraDate);
              Result := stTimestamp;
            end;
    SQLT_BIN, {RAW / unsigned char[n]}
    SQLT_VBI { unsigned char[n+sizeof(short integer)] }: begin
        result := stBytes;
        if (DataSize = 0) and (IO <> OCI_TYPEPARAM_IN) then
          DataSize := Max_OCI_Raw_Size;
        DataType := SQLT_VBI;
        DataSize := DataSize + SizeOf(SmallInt);
      end;
    SQLT_BFLOAT, SQLT_IBFLOAT {native/binary float / float }: begin
        DataType := SQLT_BFLOAT;
        Result := stFloat;
        DataSize := SizeOf(Single);
      end;
    SQLT_BDOUBLE, SQLT_IBDOUBLE {native/binary double / double}: begin
        DataType := SQLT_BDOUBLE;
        Result := stDouble;
        DataSize := SizeOf(Double);
      end;
    SQLT_UIN {unsigned short/int/long/longlong}: case DataSize of
          SizeOf(UInt64):   Result := stULong;
          SizeOf(Cardinal): Result := stLongWord;
          SizeOf(Word):     Result := stWord;
          SizeOf(Byte):     Result := stByte;
          else begin
            DataSize := SizeOf(UInt64);
            Result := stULong;
          end;
        end;
    SQLT_VST: begin{ OCI STRING type / *OCIString recommedend by Oracle see:
      https://docs.oracle.com/cd/B28359_01/appdev.111/b28395/oci12oty.htm#i421612
      this is a opaque Type...
      but there advice is using the OCIStringXXX functions for Length/data
      humm hard to judge! it migth be possible this is fast indeed!
      The more ... and for me the only advantage : Bidirectional or out params
      are buffered by OCI we can ignore all length buffers on oversized memallocs
      my crystall ball says this is a PP(Raw/Wide)Char-Struct including length like TOCILong
      -> just look to OCIRaw/SQLT_LVB of https://docs.oracle.com/cd/B13789_01/appdev.101/b10779/oci11oty.htm#421682}
        Result := stString;
        if (DataSize = 0) and (IO <> OCI_TYPEPARAM_IN) then
          DataSize := SizeOf(POCIString);
      end;
    SQLT_LNG: { LONG /char[n] } begin
        Result := stAsciiStream;
        if (DataSize = 0) or (IO <> OCI_TYPEPARAM_IN) then
           DataSize := 128 * 1024;
        DataSize := DataSize + SizeOf(Integer);
        DataType := SQLT_LVC; { EH: http://zeoslib.sourceforge.net/viewtopic.php?t=3530 }
        Exit; //is this correct?
      end;
    SQLT_LVC { LONG VARCHAR / char[n+sizeof(integer)] }: begin
        Result := stString;//stAsciiStream;
        if (DataSize = 0) or (IO <> OCI_TYPEPARAM_IN) then
          DataSize := Max_OCI_String_Size*ConSettings^.ClientCodePage^.CharWidth;
        DataSize := DataSize + SizeOf(Integer);
      end;
    SQLT_LBI, { LONG RAW / unsigned char[n] }
    SQLT_LVB { LONG VARRAW / unsigned char[n+sizeof(integer)]}:begin
        Result := stBinaryStream;
        if (DataSize = 0) then
          DataSize := 128 * 1024;
        DataSize := DataSize + SizeOf(Integer);
        DataType := SQLT_LVB;
      end;
    SQLT_RDD {ROWID descriptor / OCIRowid * }: begin
        {DescriptorType := OCI_DTYPE_ROWID;
        DataSize := SizeOf(POCIRowid);}
        DataSize := Max(20, DataSize);
        goto VCS;
      end;
    SQLT_NTY {NAMED DATATYPE / struct }: begin
        Result := stUnknown;
        DataSize := SizeOf(Pointer);
      end;
    SQLT_REF: { REF / OCIRef } ;
    SQLT_CLOB: { Character LOB descriptor / OCIClobLocator }begin
        Result := stAsciiStream;
        DescriptorType := OCI_DTYPE_LOB;
        if DataSize > 0 then
          DataSize := SizeOf(POCILobLocator);
      end;
    SQLT_BLOB: { Binary LOB descriptor / OCIBlobLocator } begin
        Result := stBinaryStream;
        DescriptorType := OCI_DTYPE_LOB;
        if DataSize > 0 then
          DataSize := SizeOf(POCILobLocator);
      end;
    SQLT_BFILEE, SQLT_CFILEE: { Binary file descriptor / OCILobLocator } begin
        Result := stBinaryStream;
        DescriptorType := OCI_DTYPE_FILE;
        if DataSize > 0 then
          DataSize := SizeOf(POCILobLocator);
      end;
    SQLT_ODT: { OCI DATE type / OCIDate * recommended as well -> no descriptor alloc? }
      begin
        Result := stTimeStamp;
        DataSize := SizeOf(POCIDate);
      end;
    SQLT_DATE:          { ANSI DATE descriptor / OCIDateTime * }
      begin
        Result := stTimeStamp; //note Oracle does NOT have a native Date type without hour,min,sec!
        DataSize := SizeOf(POCIDateTime);
        DescriptorType := OCI_DTYPE_DATE;
      end;
    SQLT_TIMESTAMP:     { TIMESTAMP descriptor / OCIDateTime * }
      begin
        Result := stTimeStamp;
        DataSize := SizeOf(POCIDateTime);
        DescriptorType := OCI_DTYPE_TIMESTAMP;
      end;
    SQLT_TIMESTAMP_TZ:  { TIMESTAMP WITH TIME ZONE descriptor / OCIDateTime * }
      begin
        Result := stTimeStamp;
        DataSize := SizeOf(POCIDateTime);
        DescriptorType := OCI_DTYPE_TIMESTAMP_TZ;
      end;
    SQLT_INTERVAL_YM:   {INTERVAL YEAR TO MONTH descriptor / OCIInterval *}
      begin
        DescriptorType := OCI_DTYPE_INTERVAL_YM;
        Result := stTimeStamp;
        DataSize := SizeOf(POCIInterval);
      end;
    SQLT_INTERVAL_DS:   {INTERVAL DAY TO SECOND descriptor / OCIInterval *}
      begin
        DescriptorType := OCI_DTYPE_INTERVAL_DS;
        Result := stTimeStamp;
        DataSize := SizeOf(POCIInterval);
      end;
    SQLT_TIMESTAMP_LTZ: {TIMESTAMP WITH LOCAL TIME ZONE descriptor / OCIDateTime *}
      begin
        DescriptorType := OCI_DTYPE_TIMESTAMP_LTZ;
        Result := stTimeStamp;
        DataSize := SizeOf(POCIDateTime);
      end;
    SQLT_TIME, SQLT_TIME_TZ:
      Result := stTime;
    _SQLT_BOL: begin
        { those pl/sql types can't be fetched by OCI -> make it possible}
        Result := stBoolean;
        DataType := SQLT_UIN;
        DataSize := SizeOf(Word);
      end
    //ELSE raise Exception.Create('Unknown datatype: '+ZFastCode.IntToStr(DataType));
  end;
  if (ConSettings^.CPType = cCP_UTF16) and (Result in [stString, stAsciiStream]) then
    if Result = stString
    then Result := stUnicodeString
    else Result := stUnicodeStream;
end;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(const PlainDriver: TZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);
var
  ErrorMessage: RawByteString;
  ErrorBuffer: TRawBuff;
  ErrorCode: SB4;
begin
  ErrorBuffer.Pos := 0;
  ErrorCode := Status;

  case Status of
    OCI_SUCCESS:
      Exit;
    OCI_SUCCESS_WITH_INFO:
      begin
        PlainDriver.OCIErrorGet(ErrorHandle, 1, nil, ErrorCode, @ErrorBuffer.Buf[0], SizeOf(ErrorBuffer.Buf)-1, OCI_HTYPE_ERROR);
        ErrorBuffer.Pos := StrLen(@ErrorBuffer.Buf[0])+1;
        ErrorMessage := 'OCI_SUCCESS_WITH_INFO: ';
      end;
    OCI_NEED_DATA:  ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:    ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        if PlainDriver.OCIErrorGet(ErrorHandle, 1, nil, ErrorCode, @ErrorBuffer.Buf[0], SizeOf(ErrorBuffer.Buf)-1, OCI_HTYPE_ERROR) = 100
        then ErrorMessage := 'OCI_ERROR: Unkown(OCI_NO_DATA)'
        else begin
          ErrorMessage := 'OCI_ERROR: ';
          ErrorBuffer.Pos := StrLen(@ErrorBuffer.Buf[0]);
        end;
      end;
    OCI_INVALID_HANDLE:
      ErrorMessage := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      ErrorMessage := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      ErrorMessage := 'OCI_CONTINUE';
    else ErrorMessage := '';
  end;
  FlushBuff(ErrorBuffer, ErrorMessage);

  if (Status <> OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
  begin
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
        ErrorCode, ErrorMessage);
    if not ( ( LogCategory = lcDisconnect ) and ( ErrorCode = 3314 ) ) then //patch for disconnected Server
      //on the other hand we can't close the connction  MantisBT: #0000227
      if LogMessage <> ''
      then raise EZSQLException.CreateWithCode(ErrorCode,
        Format(cSSQLError3, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP), ErrorCode, LogMessage]))
      else raise EZSQLException.CreateWithCode(ErrorCode,
        Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
  if (Status = OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogMessage(LogCategory, ConSettings^.Protocol, ErrorMessage);
end;

{**
  recurses down the field's TDOs and saves the little bits it need for later
  use on a fetch SQLVar._obj
}
function DescribeObject(const PlainDriver: TZOraclePlainDriver; const Connection: IZConnection;
  ParamHandle: POCIParam; stmt_handle: POCIHandle; Level: ub2): POCIObject;
var
  type_ref: POCIRef;
  ConSettings: PZConSettings;

  function AllocateObject: POCIObject;
  begin
    Result := New(POCIObject);
    FillChar(Result^, SizeOf(TOCIObject), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  end;

  procedure DescribeObjectByTDO(const PlainDriver: TZOraclePlainDriver;
    const Connection: IZConnection; var obj: POCIObject);
  var
    FConnection: IZOracleConnection;
    list_attibutes: POCIParam;
    name: PAnsiChar;
    temp: RawByteString;
    len: ub4;
    I: ub2;
    Fld: POCIObject;
  begin
    FConnection := Connection as IZOracleConnection;

    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.OCIDescribeAny(FConnection.GetServiceContextHandle,
        FConnection.GetErrorHandle, obj.tdo, 0, OCI_OTYPE_PTR, OCI_DEFAULT,
        OCI_PTYPE_TYPE, FConnection.GetDescribeHandle),
      lcOther, 'OCIDescribeAny(OCI_PTYPE_TYPE) of OCI_OTYPE_PTR', ConSettings);

    //we have the Actual TDO  so lets see what it is made up of by a describe
    Len := 0;  //and we store it in the object's paramdp for now
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.OCIAttrGet(FConnection.GetDescribeHandle, OCI_HTYPE_DESCRIBE,
        @obj.parmdp, @Len, OCI_ATTR_PARAM, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_HTYPE_DESCRIBE) of OCI_ATTR_PARAM', ConSettings);

    //Get the SchemaName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_SCHEMA_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_SCHEMA_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp{%H-});
    Obj.type_schema := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp);
    Obj.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeCode of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @Obj.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

    if (obj.typecode = OCI_TYPECODE_OBJECT ) or ( obj.typecode = OCI_TYPECODE_OPAQUE) then
    begin
      //we will need a reff to the TDO for the pin operation
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.obj_ref, nil, OCI_ATTR_REF_TDO, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_REF_TDO) of OCI_DTYPE_PARAM', ConSettings);

      //now we'll pin the object
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.OCIObjectPin(FConnection.GetConnectionHandle, FConnection.GetErrorHandle,
          Obj.obj_ref, nil, OCI_PIN_LATEST, OCI_DURATION_SESSION, pub2(OCI_LOCK_NONE),
          @obj.obj_type),
        lcOther, 'OCIObjectPin(OCI_PIN_LATEST, OCI_DURATION_SESSION, OCI_LOCK_NONE)', ConSettings);
      Obj.Pinned := True;

      //is the object the final type or an type-descriptor?
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.is_final_type, nil, OCI_ATTR_IS_FINAL_TYPE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_IS_FINAL_TYPE) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //Get the FieldCount
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.field_count, nil, OCI_ATTR_NUM_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_NUM_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //now get the differnt fields of this object add one field object for property
      SetLength(Obj.fields, Obj.field_count);

      //a field is just another instance of an obj not a new struct
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @list_attibutes, nil, OCI_ATTR_LIST_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_LIST_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      if obj.field_count > 0 then
        for I := 0 to obj.field_count-1 do
        begin
          Fld := AllocateObject;  //allocate a new object
          Obj.fields[i] := Fld;  //assign the object to the field-list

          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.OCIParamGet(list_attibutes, OCI_DTYPE_PARAM,
              FConnection.GetErrorHandle, Fld.parmdp, I+1),
            lcOther, 'OCIParamGet(OCI_DTYPE_PARAM) of OCI_DTYPE_PARAM(Element)', ConSettings);

          // get the name of the attribute
          len := 0;
          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.OCIAttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
              @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
            lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM(Element)', ConSettings);

          ZSetString(name, len, temp);
          Fld.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
            ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

          // get the typeCode of the attribute
          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.OCIAttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
              @Fld.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
            lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM(Element)', ConSettings);

          if (fld.typecode = OCI_TYPECODE_OBJECT) or
             (fld.typecode = OCI_TYPECODE_VARRAY) or
             (fld.typecode = OCI_TYPECODE_TABLE) or
             (fld.typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
            //this is some sort of object or collection so lets drill down some more
            fld.next_subtype := DescribeObject(PlainDriver, Connection, fld.parmdp,
              obj.stmt_handle, obj.Level+1);
        end;
      end
      else
      begin
        //this is an embedded table or varray of some form so find out what is in it*/

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.col_typecode, nil, OCI_ATTR_COLLECTION_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

        //first get what sort of collection it is by coll typecode
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.parmap, nil, OCI_ATTR_COLLECTION_ELEMENT, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_ELEMENT) of OCI_DTYPE_PARAM', ConSettings);

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.OCIAttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.elem_typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE of Element) of OCI_DTYPE_PARAM', ConSettings);

        if (obj.elem_typecode = OCI_TYPECODE_OBJECT) or
           (obj.elem_typecode = OCI_TYPECODE_VARRAY) or
           (obj.elem_typecode = OCI_TYPECODE_TABLE) or
           (obj.elem_typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
          //this is some sort of object or collection so lets drill down some more
          obj.next_subtype := DescribeObject(PlainDriver, Connection, obj.parmap,
            obj.stmt_handle, obj.Level+1);
      end;
  end;
begin
  ConSettings := Connection.GetConSettings;

  Result := AllocateObject;

  //Describe the field (OCIParm) we know it is a object or a collection

  //Get the Actual TDO
  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.OCIAttrGet(ParamHandle, OCI_DTYPE_PARAM, @type_ref, nil,
      OCI_ATTR_REF_TDO, (Connection as IZOracleConnection).GetErrorHandle),
    lcOther, 'OCIAttrGet OCI_ATTR_REF_TDO of OCI_DTYPE_PARAM', ConSettings);

  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.OCITypeByRef((Connection as IZOracleConnection).GetConnectionHandle,
      (Connection as IZOracleConnection).GetErrorHandle, type_ref,
      OCI_DURATION_TRANS, OCI_TYPEGET_ALL, @Result.tdo),
    lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
  Result^.Level := Level;
  DescribeObjectByTDO(PlainDriver, Connection, Result);
end;

procedure OraWriteLob(const PlainDriver: TZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);
var
  Status: sword;
  ContentSize, OffSet: ub4;

  function DoWrite(AOffSet: ub4; AChunkSize: ub4; APiece: ub1): sword;
  var
    AContentSize: ub4;
  begin
    if BinaryLob then
    begin
      AContentSize := ContentSize;
      Result := PlainDriver.OCILobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, 0, SQLCS_IMPLICIT);
    end
    else
    begin
      if ContentSize > 0 then
        AContentSize := ConSettings^.ClientCodePage^.CharWidth
      else
      begin
        AContentSize := ContentSize;
        AChunkSize := ConSettings^.ClientCodePage^.CharWidth;
      end;

      Result := PlainDriver.OCILobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, ConSettings^.ClientCodePage^.ID, SQLCS_IMPLICIT);
    end;
    ContentSize := AContentSize;
    inc(OffSet, AChunkSize);
  end;
begin

  { Opens a large object or file for read. }
  Status := PlainDriver.OCILobOpen(ContextHandle, ErrorHandle, LobLocator, OCI_LOB_READWRITE);
  if Status <> OCI_SUCCESS then
    CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Open Large Object', ConSettings);

  { Checks for empty blob.}
  { This test doesn't use IsEmpty because that function does allow for zero length blobs}
  if (BlobSize > 0) then
  begin
    if not BinaryLob then
      BlobSize := BlobSize-1;
    if BlobSize > ChunkSize then
    begin
      OffSet := 0;
      ContentSize := 0;

      Status := DoWrite(1, ChunkSize, OCI_FIRST_PIECE);
      if Status <> OCI_NEED_DATA then
        CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);

      if (BlobSize - OffSet) > ChunkSize then
        while (BlobSize - OffSet) > ChunkSize do begin //take care there is room left for LastPiece
          Status := DoWrite(offset, ChunkSize, OCI_NEXT_PIECE);
          if Status <> OCI_NEED_DATA then
            CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);
        end;
      Status := DoWrite(offset, BlobSize - OffSet, OCI_LAST_PIECE);
    end else begin
      ContentSize := BlobSize;
      Status := PlainDriver.OCILobWrite(ContextHandle, ErrorHandle, LobLocator,
        ContentSize, 1, BlobData, BlobSize, OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
    end;
  end else
    Status := PlainDriver.OCILobTrim(ContextHandle, ErrorHandle, LobLocator, 0);

  CheckOracleError(PlainDriver, ErrorHandle,
    Status, lcOther, 'Write Large Object', ConSettings);

  { Closes large object or file. }
  Status := PlainDriver.OCILobClose(ContextHandle, ErrorHandle, LobLocator);
  if Status <> OCI_SUCCESS then
    CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Close Large Object', ConSettings);
end;

function OCIType2Name(DataType: ub2): String;
begin
  Result := '';
end;

{ TZOraProcDescriptor_A }

Const ArgListType: array[Boolean] of ub4 = (OCI_ATTR_LIST_ARGUMENTS, OCI_ATTR_LIST_SUBPROGRAMS);

{$IFDEF UNICODE}
procedure TZOraProcDescriptor_A.ConcatParentName(NotArgName: Boolean;
  var Buf: TUCS2Buff; var Result: String; const IC: IZIdentifierConvertor);
{$ELSE}
procedure TZOraProcDescriptor_A.ConcatParentName(NotArgName: Boolean;
  var Buf: TRawBuff; var Result: RawByteString; const IC: IZIdentifierConvertor);
{$ENDIF}
begin
  if (FParent <> nil) then begin
    FParent.ConcatParentName(NotArgName, Buf, Result, IC);
    if NotArgName then begin
      ZDbcUtils.ToBuff(IC.Quote(FParent.AttributeName), Buf, Result);
      ZDbcUtils.ToBuff('.', Buf, Result);
    end else if ((ObjType = OCI_PTYPE_ARG) and (FParent.Parent <> nil) and (FParent.Parent.ObjType = OCI_PTYPE_PKG) and (FParent.Parent.Args.Count > 1)) {or
       ((FParent.ObjType = OCI_PTYPE_PKG) and (FParent.Args.Count > 1)) }then begin
      ZDbcUtils.ToBuff(FParent.AttributeName, Buf, Result);
      ZDbcUtils.ToBuff('_', Buf, Result);
    end;
  end;
end;

constructor TZOraProcDescriptor_A.Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}
  Parent: TZOraProcDescriptor_A);
begin
  fParent := Parent;
end;

function TZOraProcDescriptor_A.InternalDescribe(const Name: String; _Type: UB4;
  {$IFDEF AUTOREFCOUNT} const {$ENDIF}PlainDriver: TZOraclePlainDriver;
  ErrorHandle: POCIError; OCISvcCtx: POCISvcCtx; Owner: POCIHandle;
  ConSettings: PZConSettings): SWord;
var P: PAnsiChar;
  i: sb4;
  parmh: POCIHandle;
  Descriptor: POCIDescribe;
  tmp: RawByteString;
begin
  //https://www.bnl.gov/phobos/Detectors/Computing/Orant/doc/appdev.804/a58234/describe.htm#440341
  //section describing the stored procedure
  Descriptor := nil;
  { get a descriptor handle for the param/obj }
  CheckOracleError(PlainDriver, ErrorHandle,
    PlainDriver.OCIHandleAlloc(Owner, Descriptor, OCI_HTYPE_DESCRIBE, 0, nil),
      lcOther,'OCIHandleAlloc', ConSettings);
  {$IFDEF UNICODE}
  tmp := PUnicodeToRaw(Pointer(Name), Length(Name), ConSettings^.ClientCodePage.CP);
  Result := PlainDriver.OCIDescribeAny(OCISvcCtx, ErrorHandle, Pointer(tmp),
        Length(tmp), OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, Descriptor);
  {$ELSE}
  Result := PlainDriver.OCIDescribeAny(OCISvcCtx, ErrorHandle, Pointer(Name),
        Length(Name), OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, Descriptor);
  {$ENDIF}
  if Result <> OCI_SUCCESS then begin
    tmp := '"PUBLIC".'+{$IFDEF UNICODE}tmp{$ELSE}Name{$ENDIF};
    Result := PlainDriver.OCIDescribeAny(OCISvcCtx, ErrorHandle, Pointer(tmp),
        Length(tmp), OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, Descriptor);
  end;

  try
    if Result <> OCI_SUCCESS then
      Exit;
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(Descriptor, OCI_HTYPE_DESCRIBE, @parmh, nil, OCI_ATTR_PARAM, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    { get the schema name }
    P := nil;
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(parmh, OCI_HTYPE_DESCRIBE, @P, @I, OCI_ATTR_OBJ_SCHEMA, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    if P = nil then begin
      Result := OCI_ERROR;
      Exit;
    end;
    {$IFDEF UNICODE}
    SchemaName := PRawToUnicode(P, i, ConSettings.ClientCodePage.CP);
    {$ELSE}
    System.SetString(SchemaName, P, I);
    {$ENDIF}
    { get the objectname }
    P := nil;
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(parmh, OCI_HTYPE_DESCRIBE, @P, @I, OCI_ATTR_OBJ_NAME, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    {$IFDEF UNICODE}
    AttributeName := PRawToUnicode(P, i, ConSettings.ClientCodePage.CP);
    {$ELSE}
    System.SetString(AttributeName, P, I);
    {$ENDIF}
    { get the first object type }
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(parmh, OCI_HTYPE_DESCRIBE, @ObjType, nil, OCI_ATTR_PTYPE, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    InternalDescribeObject(parmh, PlainDriver, ErrorHandle, ConSettings);
  finally
    if Descriptor <> nil then begin
      PlainDriver.OCIDescriptorFree(Descriptor, OCI_HTYPE_DESCRIBE);
      Descriptor := nil;
    end;
  end;
end;

procedure TZOraProcDescriptor_A.InternalDescribeObject(Obj: POCIHandle;
  {$IFDEF AUTOREFCOUNT} const {$ENDIF}PlainDriver: TZOraclePlainDriver;
  ErrorHandle: POCIError; ConSettings: PZConSettings);
var
  arglst, arg: POCIHandle;
  i, N: sb4;
  ParamCount: ub2;
  p: PAnsichar;
  Param: TZOraProcDescriptor_A;
begin
  arglst := nil;
  if ObjType <> OCI_PTYPE_PKG then
    { get the overload position }
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(obj, OCI_HTYPE_DESCRIBE, @OverloadID, nil, OCI_ATTR_OVERLOAD_ID, ErrorHandle),
            lcOther,'OCIAttrGet', ConSettings);
  { get a argument-list handle }
  CheckOracleError(PlainDriver, ErrorHandle,
    PlainDriver.OCIAttrGet(Obj, OCI_DTYPE_PARAM, @arglst, nil,
      ArgListType[ObjType = OCI_PTYPE_PKG], ErrorHandle),
        lcExecute, 'OCIAttrGet', ConSettings);
  { get argument count using of the list handle }
  CheckOracleError(PlainDriver, ErrorHandle,
    PlainDriver.OCIAttrGet(arglst, OCI_DTYPE_PARAM, @ParamCount, nil,
      OCI_ATTR_NUM_PARAMS, ErrorHandle),
      lcOther, 'OCIAttrGet', ConSettings);
  Args := TObjectList.Create;//lse);
  Args.Capacity := ParamCount;
  for N := 0+Ord(ObjType = OCI_PTYPE_PROC) to ParamCount-1+Ord(ObjType = OCI_PTYPE_PROC) do begin
    { get a argument handle }
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIParamGet(arglst, OCI_DTYPE_PARAM, ErrorHandle, arg, N),
      lcOther, 'OCIParamGet', ConSettings);
    Param := TZOraProcDescriptor_A.Create(Self);
    Args.Add(Param);
    Param.SchemaName := SchemaName;
    { get the object type }
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(arg, OCI_HTYPE_DESCRIBE, @Param.ObjType, nil, OCI_ATTR_PTYPE, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    { get the attribute Name }
    P := nil;
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.OCIAttrGet(arg, OCI_HTYPE_DESCRIBE, @P, @I, OCI_ATTR_NAME, ErrorHandle),
        lcOther,'OCIAttrGet', ConSettings);
    {$IFDEF UNICODE}
    Param.AttributeName := PRawToUnicode(P, i, ConSettings.ClientCodePage.CP);
    {$ELSE}
    System.SetString(Param.AttributeName, P, I);
    {$ENDIF}
    if Param.ObjType = OCI_PTYPE_ARG then begin
      { get the ordinal position }
      CheckOracleError(PlainDriver, ErrorHandle,
        PlainDriver.OCIAttrGet(arg, OCI_HTYPE_DESCRIBE, @Param.OrdPos, nil, OCI_ATTR_POSITION, ErrorHandle),
          lcOther,'OCIAttrGet', ConSettings);
      if (Param.OrdPos = 0) and (Param.AttributeName = '') then
        Param.AttributeName := 'ReturnValue';
      P := nil;
      CheckOracleError(PlainDriver, ErrorHandle,
        PlainDriver.OCIAttrGet(arg, OCI_HTYPE_DESCRIBE, @P, @I, OCI_ATTR_TYPE_NAME, ErrorHandle),
          lcOther,'OCIAttrGet', ConSettings);
      ZSetString(P, I, Param.TypeName);
      { get datasize }
      CheckOracleError(PlainDriver, ErrorHandle,
        PlainDriver.OCIAttrGet(arg, OCI_DTYPE_PARAM,
          @Param.DataSize, nil, OCI_ATTR_DATA_SIZE, ErrorHandle),
        lcOther, 'OCIAttrGet', ConSettings);
      { get IO direction }
      CheckOracleError(PlainDriver, ErrorHandle,
        PlainDriver.OCIAttrGet(arg, OCI_DTYPE_PARAM,
          @Param.IODirection, nil, OCI_ATTR_IOMODE, ErrorHandle),
        lcOther, 'OCIAttrGet', ConSettings);
      { get oci data type }
      CheckOracleError(PlainDriver, ErrorHandle,
        PlainDriver.OCIAttrGet(arg, OCI_DTYPE_PARAM,
          @Param.DataType, nil, OCI_ATTR_DATA_TYPE, ErrorHandle),
        lcOther, 'OCIAttrGet', ConSettings);
      if Param.DataType in [SQLT_NUM, SQLT_VNU] then begin {11g returns Precision = 38 in all cases}
        CheckOracleError(PlainDriver, ErrorHandle,
          PlainDriver.OCIAttrGet(Arg, OCI_DTYPE_PARAM,
            @Param.Precision, nil, OCI_ATTR_PRECISION, ErrorHandle),
            lcOther, 'OCIAttrGet', ConSettings);
        CheckOracleError(PlainDriver, ErrorHandle,
          PlainDriver.OCIAttrGet(arg, OCI_DTYPE_PARAM,
            @Param.Scale, nil, OCI_ATTR_SCALE, ErrorHandle),
            lcOther, 'OCIAttrGet', ConSettings);
        CheckOracleError(PlainDriver, ErrorHandle,
          PlainDriver.OCIAttrGet(arg, OCI_DTYPE_PARAM,
            @Param.Radix, nil, OCI_ATTR_RADIX , ErrorHandle),
            lcOther, 'OCIAttrGet', ConSettings);
      end;
      Param.SQLType := NormalizeOracleTypeToSQLType(Param.DataType, Param.DataSize,
        Param.DescriptorType, Param.Precision, Param.Scale, ConSettings, Param.IODirection);
    end else
      Param.InternalDescribeObject(arg, PLainDriver, ErrorHandle, ConSettings);
  end;
end;

procedure TZOraProcDescriptor_A.Describe(_Type: UB4; const Connection: IZConnection;
  const Name: {$IFDEF UNICODE}String{$ELSE}RawByteString{$ENDIF});
var
  Plain: TZOraclePlainDriver;
  OracleConnection: IZOracleConnection;
  ProcSQL, tmp: String;
  Status, ps, ps2: sword;
  IC: IZIdentifierConvertor;
  ConSettings: PZConSettings;
begin
  OracleConnection := Connection as IZOracleConnection;
  ConSettings := Connection.GetConSettings;
  {$IFNDEF UNICODE}
  if ConSettings^.AutoEncode
  then ProcSQL := ConSettings^.ConvFuncs.ZStringToRaw(Name, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP)
  else {$ENDIF}ProcSQL := Name;

  IC := Connection.GetMetadata.GetIdentifierConvertor;
  Plain := TZOraclePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  { describe the object: }
  Status := InternalDescribe(ProcSQL, OCI_PTYPE_UNK, Plain, OracleConnection.GetErrorHandle,
    OracleConnection.GetServiceContextHandle, OracleConnection.GetConnectionHandle, ConSettings);
  if (Status <> OCI_SUCCESS) then begin
    ps := ZFastCode.Pos('.', ProcSQL);
    if ps <> 0 then begin //check wether Package or Schema!
      tmp := Copy(ProcSQL, 1, ps-1);
      Status := InternalDescribe(tmp, OCI_PTYPE_UNK, Plain, OracleConnection.GetErrorHandle,
        OracleConnection.GetServiceContextHandle, OracleConnection.GetConnectionHandle, ConSettings);
      if Status <> OCI_SUCCESS then begin
        ps2 := {$IFDEF UNICODE}StrUtils{$ELSE}ZFastCode{$ENDIF}.PosEx('.', ProcSQL, ps+1);
        if ps2 <> 0 then //check wether Package or Schema!
          tmp := Copy(ProcSQL, ps+1, ps2-ps-1)
        else begin
          ps2 := ps;
          tmp := Copy(ProcSQL, ps+1, maxint)
        end;
        Status := InternalDescribe(tmp, OCI_PTYPE_UNK, Plain, OracleConnection.GetErrorHandle,
          OracleConnection.GetServiceContextHandle, OracleConnection.GetConnectionHandle, ConSettings);
        if Status = OCI_SUCCESS then
          tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else begin { final approach to locate the procedure !}
          tmp := '"PUBLIC".'+tmp;
          Status := InternalDescribe(tmp, OCI_PTYPE_UNK, Plain, OracleConnection.GetErrorHandle,
          OracleConnection.GetServiceContextHandle, OracleConnection.GetConnectionHandle, ConSettings);
          CheckOracleError(Plain, OracleConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', ConSettings);
        end;
      end else begin
        ps2 := {$IFDEF UNICODE}StrUtils{$ELSE}ZFastCode{$ENDIF}.PosEx('.', ProcSQL, ps+1);
        if ps2 <> 0 //check wether Package or Schema!
        then tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else tmp := copy(ProcSQL, Ps+1, MaxInt)
      end;
      if (ObjType = OCI_PTYPE_PKG) then begin
        //next stage obj. needs to be compared to package procs
        //strip all other procs we don't need!
        //if IC.IsQuoted(Tmp) then
          Tmp := IC.ExtractQuote(Tmp);
        for ps := Args.Count -1 downto 0 do begin
          if TZOraProcDescriptor_A(Args[ps]).AttributeName <> tmp then
            Args.Delete(ps);
        end;
      end;
    end;
  end else
    if (Status <> OCI_SUCCESS) then
      CheckOracleError(Plain, OracleConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', ConSettings);
end;

destructor TZOraProcDescriptor_A.Destroy;
begin
  inherited Destroy;
  if Args <> nil then
    FreeAndNil(Args);
end;

{$IFDEF WITH_UINT64_C1118_ERROR}
procedure UIntFiller;
var U: UInt64;
  I: Byte;
begin
  {$R-} {$Q-}
  U := 1;
  UInt64Divisor[0] := U;
  uPosScaleFaktor[0] := U;
  for i := 1 to 18 do begin
    U := U * 10;
    uPosScaleFaktor[i] := U;
    if not Odd(i) then
      UInt64Divisor[I div 2] := U;
  end;
  U := U * 10;
  uPosScaleFaktor[19] := U;
  UInt64Divisor[10] := U;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;
initialization
UIntFiller;
{$ENDIF}

end.
