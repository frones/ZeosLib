{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZDbcOracleUtils;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  {$IFNDEF FPC}Types, {$ENDIF}Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined(WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}ZClasses, ZSysUtils, ZVariant,
  ZCompatibility, ZPlainOracleDriver,
  ZDbcIntfs, ZDbcUtils, ZSelectSchema, ZDbcLogging, ZDbcOracle, ZDbcStatement;

const
  MAX_SQLVAR_LIMIT = 1024;
  Max_OCI_String_Size = 4000; //prevent 'OCI_ERROR: ORA-01459: invalid length for variable character string' if buffer is to small
  Max_OCI_Raw_Size = 2000;

  NO_DTYPE = 0;
  SQLType2OCIDescriptor: array[stUnknown..stBinaryStream] of sb2 = (NO_DTYPE,
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
  TOCILong = packed record
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

  /// <author>EgonHugeist</author>
  /// <summary>Defines a reference of the TZOCIBindValue record</summary>
  PZOCIBindValue = ^TZOCIBindValue;
  /// <author>EgonHugeist</author>
  /// <summary>Defines a BindValue record which widened the TZBindValue by a
  ///  question mark position indicator</summary>
  TZOCIBindValue = record
    /// <summary>the generic TZQMarkPosBindValue record</summary>
    BindValue:  TZBindValue;
    /// <summary>An address of a bind handle which is implicitly allocated by
    ///  this call. The bind handle maintains all the bind information for this
    ///  particular input value. The handle is freed implicitly when the
    ///  statement handle is deallocated. On input, the value of the pointer
    ///must be null or a valid bind handle.</summary>
    bindpp:     POCIBind;
    /// <summary>An address of a data value or an array of data values of the
    ///  type specified in the dty parameter. An array of data values can be
    ///  specified for mapping into a PL/SQL table or for providing data for
    ///  SQL multiple-row operations. When an array of bind values is provided,
    ///  this is called an array bind in OCI terms.</summary>
    /// <remarks>For SQLT_NTY or SQLT_REF binds, the valuep parameter is
    ///  ignored. The pointers to OUT buffers are set in the pgvpp parameter
    ///  initialized by OCIBindObject(). If the OCI_ATTR_CHARSET_ID attribute is
    ///  set to OCI_UTF16ID (replaces the deprecated OCI_UCS2ID, which is
    ///  retained for backward compatibility), all data passed to and received
    ///  with the corresponding bind call is assumed to be in UTF-16 encoding.</remarks>
    valuep:     PAnsiChar;
    /// <summary>The size of a data value. In the case of an array bind, this is
    ///  the maximum size of any element possible with the actual sizes being
    ///  specified in the alenp parameter. sescriptors, locators, or REFs, whose
    ///  size is unknown to client applications use the size of the structure
    ///  you are passing in; for example, sizeof (OCILobLocator *).</summary>
    value_sz:   sb4;
    /// <summary>The data type of the value(s) being bound. Named data types
    ///  (SQLT_NTY) and REFs (SQLT_REF) are valid only if the application has
    ///  been initialized in object mode. For named data types, or REFs,
    ///  additional calls must be made with the bind handle to set up the
    ///  datatype-specific attributes.</summary>
    dty:        ub2;
    /// <summary>Pointer to an indicator variable or array. For all data types,
    ///  this is a pointer to sb2 or an array of sb2s. The only exception is
    ///  SQLT_NTY, when this pointer is ignored and the actual pointer to the
    ///  indicator structure or an array of indicator structures is initialized
    ///  by OCIBindObject(). Ignored for dynamic binds.</summary>
    indp:       PSB2Array;
    /// <summary>The descriptor type used for binding the values</summary>
    DescriptorType: sb4; //holds our descriptor type we use
    /// <summary>the actual number of elements</summary>
    curelen:      ub4;
    /// <summary>field.precision used 4 out params; oracle still can't discribe
    ///  them</summary>
    Precision: sb2; //field.precision used 4 out params
    /// <summary>field.scale used 4 out params; oracle still can't discribe
    ///  them</summary>
    Scale:     sb1;
    /// <summary>Defines the parametername</summary>
    ParamName: String;
  end;

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
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer): TZSQLType;

function NormalizeOracleTypeToSQLType(var DataType: ub2; var DataSize: ub4;
  out DescriptorType: sb4; var Precision: Integer; ScaleOrCharSetForm: sb2;
  ConSettings: PZConSettings): TZSQLType;
(*
function DescribeObject(const PlainDriver: TZOraclePlainDriver; const Connection: IZConnection;
  ParamHandle: POCIParam; {%H-}stmt_handle: POCIHandle; Level: ub2): POCIObject;*)

{** Autor: EgonHugeist (EH)
  Prolog:
  the TBCD is a nightmare because of missing strict rules to fill the record.
  I would vote for strict left padding like all database are doing that! That
  would push the performance and working with the bcd's would be much easier.

  converts a <code>java.math.BigDecimal</code> value into oracle number format;
  @param bcd the <code>java.math.BigDecimal</code> value which to be converted
  @param num the pointer to a valid oracle number value
  @return the length of used bytes
}
function BCD2Nvu({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} bcd: TBCD; num: POCINumber): SB2;

{** Autor: EgonHugeist (EH)
  converts a oracle number format into a <code>java.math.BigDecimal</code>;
  @param num the pointer to a valid oracle number value which to be converted
  @param bcd the <code>java.math.BigDecimal</code> value
}
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
  NVUBase100Adjust: array[Boolean] of Byte = (1,101);
  VNU_NUM_INTState: array[Boolean] of TnvuKind = (vnuNegInt, vnuPosInt);
  VNU_NUM_CurState: array[Boolean] of TnvuKind = (vnuNegCurr, vnuPosCurr);
  sCurrScaleFaktor: array[0..4] of Integer = (
      1,
      10,
      100,
      1000,
      10000);
  NVU_CurrencyExponents: array[0..10] of Integer =
    (-2,-1, 0, 1, 2, 3, 4, 5, 6, 7, 8);
{$IF defined(NEED_TYPED_UINT64_CONSTANTS) or defined(WITH_UINT64_C1118_ERROR)}
  {$IF DEFINED(FPC) and DEFINED(ENDIAN_BIG)}
  cInt64Divisor: array[0..10] of Int64Rec = (
    (hi: $00000000; lo: $00000001), {                   1}
    (hi: $00000000; lo: $00000064), {                 100}
    (hi: $00000000; lo: $00002710), {               10000}
    (hi: $00000000; lo: $000F4240), {             1000000}
    (hi: $00000000; lo: $05F5E100), {           100000000}
    (hi: $00000002; lo: $540BE400), {         10000000000}
    (hi: $000000E8; lo: $D4A51000), {       1000000000000}
    (hi: $00005AF3; lo: $107A4000), {     100000000000000}
    (hi: $002386F2; lo: $6FC10000), {   10000000000000000}
    (hi: $0DE0B6B3; lo: $A7640000), { 1000000000000000000}
    (hi: $8AC72304; lo: $89E80000));{10000000000000000000}
  {$ELSE}
  cInt64Divisor: array[0..10] of Int64Rec = (
    (lo: $00000001; hi: $00000000), {                   1}
    (lo: $00000064; hi: $00000000), {                 100}
    (lo: $00002710; hi: $00000000), {               10000}
    (lo: $000F4240; hi: $00000000), {             1000000}
    (lo: $05F5E100; hi: $00000000), {           100000000}
    (lo: $540BE400; hi: $00000002), {         10000000000}
    (lo: $D4A51000; hi: $000000E8), {       1000000000000}
    (lo: $107A4000; hi: $00005AF3), {     100000000000000}
    (lo: $6FC10000; hi: $002386F2), {   10000000000000000}
    (lo: $A7640000; hi: $0DE0B6B3), { 1000000000000000000}
    (lo: $89E80000; hi: $8AC72304));{10000000000000000000}
  {$IFEND}
var
  UInt64Divisor:   array[0..10] of UInt64 absolute cInt64Divisor;
{$ELSE}
  UInt64Divisor: array[0..10] of UInt64 = (
    $0000000000000001 {                   1},
    $0000000000000064 {                 100},
    $0000000000002710 {               10000},
    $00000000000F4240 {             1000000},
    $0000000005F5E100 {           100000000},
    $00000002540BE400 {         10000000000},
    $000000E8D4A51000 {       1000000000000},
    $00005AF3107A4000 {     100000000000000},
    $002386F26FC10000 {   10000000000000000},
    $0DE0B6B3A7640000 { 1000000000000000000},
    $8AC7230489E80000 {10000000000000000000});
{$IFEND}
type
  {$M+}
  TZOracleAttribute = class(TObject, IImmediatelyReleasable)
  private
    FConSettings: PZConSettings;
    FOwner: IImmediatelyReleasable;
    Ftrgthndlp: POCIHandle;
    FOCIError: POCIError;
    Ftrghndltyp: ub4;
    FPlainDriver: TZOraclePlainDriver;
    FConnection: IZOracleConnection;
  public //IImmediatelyReleasable
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
    function GetConSettings: PZConSettings;
    procedure HandleAttributError(Status: SWord; WasGet: Boolean);
  protected //implement fakes IInterface
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
  public
    constructor Create(const Connection: IZOracleConnection;
      trgthndlp: POCIHandle; trghndltyp: ub4; errhp: POCIError);
  public
    procedure SetHandleAndType(trgthndlp: POCIHandle; trghndltyp: ub4);
    function GetPChar(var Len: sb4; attrtype: ub4): Pointer;
    function GetPointer(attrtype: ub4): Pointer;
    function GetUb4(attrtype: ub4): ub4;
    procedure SetUb4(attrtype, Value: ub4);
    function GetSb4(attrtype: ub4): Sb4;
    procedure SetSb4(attrtype: ub4; Value: Sb4);
    function GetUb2(attrtype: ub4): ub2;
    procedure SetUb2(attrtype: ub4; Value: Ub2);
    function GetSb2(attrtype: ub4): Sb2;
    procedure SetSb2(attrtype: ub4; Value: Sb2);
    function GetUb1(attrtype: ub4): ub1;
    procedure SetUb1(attrtype: ub4; Value: Ub1);
    function GetSb1(attrtype: ub4): Sb1;
    procedure SetSb1(attrtype: ub4; Value: Sb1);
  end;
  {$M-}

  TZOraProcDescriptor = class(TZOracleAttribute)
  public
    Args: TObjectList;
    ObjType, Radix, csform: UB1;
    Precision: Integer;
    Scale: SB1;
    DataSize: UB4;
    DataType, CodePage: UB2;
    OverloadID, csid: ub2;
    DescriptorType: SB4;
    IODirection: OCITypeParamMode;
    OrdPos: ub2;
    SQLType: TZSQLType;
  end;

  { oracle loves it's recursion ... so we need a recursive obj model }
  TZOraProcDescriptor_A = class(TZOraProcDescriptor)
  private
    FParent: TZOraProcDescriptor_A;
    FConnection: IZOracleConnection;
    {$IFDEF UNICODE}
    FRawCP: Word;
    {$ENDIF}

    procedure InternalDescribeObject(Obj: POCIHandle);
    function InternalDescribe(const Name: RawByteString; _Type: UB4;
      Owner: POCIHandle): Sword;
  public
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
    procedure Describe(_Type: UB4; const Name: RawByteString);
    procedure ConcatParentName(NotArgName: Boolean; {$IFDEF AUTOREFCOUNT}const{$ENDIF}
      SQLWriter: TZRawSQLStringWriter; var Result: RawByteString; const IC: IZIdentifierConvertor);
    constructor Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}Parent: TZOraProcDescriptor_A;
      Const Connection: IZOracleConnection{$IFDEF UNICODE}; RawCP: Word{$ENDIF UNICODE});
    destructor Destroy; override;
  public
    SchemaName, AttributeName, TypeName: RawByteString;
    property Parent: TZOraProcDescriptor_A read FParent;
  end;

  { oracle loves it's recursion ... so we need a recursive obj model }
  TZOraProcDescriptor_W = class(TZOraProcDescriptor)
  private
    FParent: TZOraProcDescriptor_W;
    {$IFNDEF UNICODE}
    FRawCP: Word;
    {$ENDIF UNICODE}
    procedure InternalDescribeObject(Obj: POCIHandle);
    function InternalDescribe(const Name: UnicodeString; _Type: UB4;
      Owner: POCIHandle): Sword;
  public
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

    procedure Describe(_Type: UB4; const Name: UnicodeString);
    procedure ConcatParentName(NotArgName: Boolean; {$IFDEF AUTOREFCOUNT}const{$ENDIF}
      SQLWriter: TZUnicodeSQLStringWriter; var Result: UnicodeString; const IC: IZIdentifierConvertor);

    constructor Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}Parent: TZOraProcDescriptor_W;
      Const Connection: IZOracleConnection{$IFNDEF UNICODE}; RawCP: Word{$ENDIF});
    destructor Destroy; override;
  public
    SchemaName, AttributeName, TypeName: UnicodeString;
    property Parent: TZOraProcDescriptor_W read FParent;
  end;
{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses Math, ZMessages, ZDbcOracleResultSet,
  ZEncoding, ZFastCode {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
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
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors} {$ENDIF}
function PosNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): UInt64;
var i: Byte;
begin
  { initialize with first positive base-100-digit }
  Result := vnuInfo.FirstBase100Digit;
  { skip len, exponent and first base-100-digit -> start with 3}
  for i := 3 to vnuInfo.Len do
    Result := Result * 100 + Byte(num[i] - 1);
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then
    Result := Result * UInt64Tower[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

{** EH:
  convert a negative oracle oci number into a signed longlong
  @param num the pointer to the oci-number
  @param Neg100FactorCnt a scale for truncation if positive or base 100 multiplication if negative
  @param Len a true len of num gigits to work with
  @return a converted value
}
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH}
  {$WARN 4080 off : Converting the operands to "Int64" before doing the substract could prevent overflow errors}
  {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors}
{$ENDIF}
function NegNvu2Int(num: POCINumber; const vnuInfo: TZvnuInfo): Int64;
var i: Byte;
begin
  { initialize with first negative base-100-digit }
  Result := -ShortInt(vnuInfo.FirstBase100Digit); //init
  { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
  for i := 3 to vnuInfo.Len do
    Result := Result * 100 - (101 - num[i]);
  I := (vnuInfo.Len-1)*2;
  if I <= vnuInfo.Precision then
    Result := Result * Int64Tower[vnuInfo.Precision+Ord(vnuInfo.FirstBase100DigitDiv10Was0)-i+Ord(vnuInfo.LastBase100DigitMod10Was0)];
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

{** EH:
  convert a positive oracle oci number into currency value
  @param num the pointer to the oci-number
  @param scale a given scale to align scale to 4 decimal digits
  @return a converted value
}
{$R-} {$Q-}
function PosNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency;
var I64: Int64 absolute Result;
  i: ShortInt;
begin
  { initialize with first positive base-100-digit }
  I64 := vnuInfo.FirstBase100Digit;
  { skip len, exponent and first base-100-digit -> start with 3}
  for i := 3 to vnuInfo.Len do
    i64 := i64 * 100 + Byte(num[i] - 1);
  I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{**
  convert a negative oracle oci number into currency value
  @param num the pointer to the oci-number
  @param scale a given scale to align scale to 4 decimal digits
  @return a converted value
}
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4080 off : Converting the operands to "Int64" before doing the substract could prevent overflow errors} {$ENDIF}
function NegNvu2Curr(num: POCINumber; const vnuInfo: TZvnuInfo): Currency;
var I64: Int64 absolute Result;
  i: ShortInt;
begin
  i64 := -ShortInt(vnuInfo.FirstBase100Digit); //init
  { skip len, exponent and first base-100-digit / last byte doesn't count if = 102}
  for i := 3 to vnuInfo.Len do
    i64 := i64 * 100 - (101 - num[i]);
  I64 := I64 * sCurrScaleFaktor[4-(vnuInfo.Scale+Ord(vnuInfo.LastBase100DigitMod10Was0))];
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

{** EH:
  converts a currency value to a oracle oci number
  to be clear: this might not be the fastest way ( the mul/divs are slow)
  but is accurate in contrary to using the doubles
  @param value the currency to be converted
  @param num the pointer to the oci-number
}
{$R-} {$Q-}
procedure Curr2Vnu(const Value: Currency; num: POCINumber);
var I64, IDiv100, IMul100: UInt64;
  x{$IFNDEF CPUX64}, c32, cDiv100, cMul100{$ENDIF}: Cardinal;
  Negative: Boolean;
  i, Digits, l: Byte;
begin
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
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{**  EH:
  writes a negative unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors} {$ENDIF}
function PosOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
var i: Byte;
  PStart: PAnsiChar;
begin
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
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

{** EH:
  writes a positive unscaled oracle oci number into a buffer
  @param num the pointer to the oci-number
  @param vnuInfo the collected infos about the number comming from nvuKind()
  @param Buf the buffer we're writing into
  @return length in bytes
}
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors} {$ENDIF}
function NegOrdNVU2Raw(num: POCINumber; const vnuInfo: TZvnuInfo; Buf: PAnsiChar): Cardinal;
var i: Byte;
  PStart: PAnsiChar;
begin
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
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

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

{** Autor: EgonHugeist (EH)
  Prolog:
  the TBCD is a nightmare because of missing strict rules to fill the record.
  I would vote for strict left padding like all database are doing that! That
  would push the performance and working with the bcd's would be much easier.

  converts a <code>BigDecimal</code> value into oracle number format;
  @param bcd the <code>BigDecimal</code> value which to be converted
  @param num the pointer to a valid oracle number value
  @return the length of used bytes
}
function BCD2Nvu({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} bcd: TBCD; num: POCINumber): sb2;
var
  pNibble, pLastNibble, pNum, pLastNum: PAnsiChar;
  NumDigit: Integer;
  Negative, GetFirstBCDHalfByte, NotMultiplyBy10: Boolean;
label NextDigitOrNum, Done;
begin
  pNibble := @bcd.Fraction[0];
  pLastNum := pNibble; //remainder
  NumDigit := bcd.Precision;
  if NumDigit > 0 then begin
    //pad all zero nibbles away because the slow mainloop uses halfbytes
    pLastNibble := pNibble + ((NumDigit-1) shr 1); //top most significant digit
    // to avoid "ora 01438: value larger than specified precision allowed for this column"
    while (pLastNibble >= pNibble) and (PByte(pLastNibble)^ = 0) do Dec(pLastNibble); // skip trailing zeroes
    while (pNibble <= pLastNibble) and (PByte(pNibble)^ = 0) do Inc(pNibble); // ... skip leading zeroes
  end else
    pLastNibble := pNibble-1;
  pNum := @num^[2]; //set offset after len and exponent bytes
  //init the 22 byte result: we ignore the first two (len+exp) bytes -> set in all cases
  {$IFDEF CPU64}
  PInt64(pNum)^ := 0; PInt64(pNum+8)^ := 0; PInt64(pNum+14)^ := 0;
  {$ELSE}
  PCardinal(pNum)^    := 0; PCardinal(pNum+4)^  := 0; PCardinal(pNum+8)^ := 0;
  PCardinal(pNum+12)^ := 0; PCardinal(pNum+16)^ := 0;
  {$ENDIF}
  if (pLastNibble < pNibble) then begin //zero!
    num[0] := 1;
    num[1] := $80;
    Result := 2;
    Exit;
  end;
  Negative := (bcd.SignSpecialPlaces and (1 shl 7)) <> 0; //no call to BCDNegative
  GetFirstBCDHalfByte := (PByte(pNibble)^ shr 4) <> 0; //skip first half byte?
  NumDigit := NumDigit - (bcd.SignSpecialPlaces and 63);
  //find out if first halfbyte need to be multiplied by 10(padd left)
  if (NumDigit and 1 = 1) then //in case of odd precisons we usually add the values
    if not GetFirstBCDHalfByte and (
        ((PByte(pLastNibble)^ and $0F) = 0) {in case of last byte is zero: }or
        ((bcd.SignSpecialPlaces and 63) and 1 = 0)) {in case of odd scale: }
    then NotMultiplyBy10 := False //we padd the values a half byte to left
    else begin
      NotMultiplyBy10 := True;
      Inc(NumDigit); //corret results for the next division
    end
  else NotMultiplyBy10 := not GetFirstBCDHalfByte; //or if first half byte is zero
  num^[1] := (NumDigit shr 1)-(pNibble+1-pLastNum) + 65 + 128; //set the exponent
  pLastNum := pNum+(OCI_NUMBER_SIZE-2); //mark end of byte array
NextDigitOrNum: //main loop without any condition
  if GetFirstBCDHalfByte
  then NumDigit := (PByte(pNibble)^ shr 4)
  else begin
    NumDigit := (PByte(pNibble)^ and $0f);
    Inc(pNibble); //next nibble
  end;
  if NotMultiplyBy10 then begin
    if Negative
    then NumDigit := 101 - PByte(pNum)^ + NumDigit
    else NumDigit := PByte(pNum)^ + NumDigit + 1;
    PByte(pNum)^ := Byte(NumDigit);
    if (pNum < pLastNum) then
      if (pNibble > pLastNibble) then begin
        if (NumDigit = NVUBase100Adjust[Negative])
        then PByte(pNum)^ := 0
        else Inc(pNum, Ord(NumDigit <> 0));  //remainder for len calculation
        goto Done;
      end else
        Inc(pNum) //next base 100 vnu digit
    else goto Done;
  end else
    PByte(pNum)^ := NumDigit * 10;
  { now invert the getter/setter logic }
  GetFirstBCDHalfByte := not GetFirstBCDHalfByte;
  NotMultiplyBy10 := not NotMultiplyBy10;
  goto NextDigitOrNum;
Done: //job done -> finalize
  if Negative then begin
    num^[1] := not num^[1]; //invert the bits
    if pNum < PLastNum then begin
      PByte(pNum)^ := 102; //as documented for whatever it is..
      inc(pNum); //for len calculation
    end;
  end;
  Result := pNum-PAnsiChar(Num);
  num^[0] := Result - 1;
end;
(* second incomplete version
function BCD2Nvu({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} bcd: TBCD; num: POCINumber): sb2;
var
  pNibble, pLastNibble, pNum, pLastNum: PAnsiChar;
  Precision, Scale: Word;
  NumDigit, ExpOffset, X, Y: Integer;
  Negative, GetFirstBCDHalfByte, ZeroScale, NotMultiplyBy10: Boolean;
begin
  //set offsets and test for zero:
  VirtualPackBCD(bcd, pNibble, pLastNibble, Precision, Scale, GetFirstBCDHalfByte);
  pNum := @num^[2]; //set offset after len and exponent bytes
  //init the 22 byte result: we ignore the first two (len+exp) bytes -> set in all cases
  {$IFDEF CPU64}
  PInt64(pNum)^ := 0; PInt64(pNum+8)^ := 0; PInt64(pNum+14)^ := 0;
  {$ELSE}
  PCardinal(pNum)^    := 0; PCardinal(pNum+4)^  := 0; PCardinal(pNum+8)^ := 0;
  PCardinal(pNum+12)^ := 0; PCardinal(pNum+16)^ := 0;
  {$ENDIF}
  if (Precision = 0) then begin //zero!
    num[0] := 1;
    num[1] := $80;
    Result := 2;
    Exit;
  end;
  ExpOffset := 1;
  ZeroScale := Precision = Scale;
  Negative := (bcd.SignSpecialPlaces and (1 shl 7)) <> 0; //no call to BCDNegative
  pLastNum := pNum+(OCI_NUMBER_SIZE-2); //mark end of byte array
  NotMultiplyBy10 := Precision and 1 = 1;
  Y := Precision - Ord(NotMultiplyBy10);
  for X := 0 to Y do begin
    if GetFirstBCDHalfByte
    then NumDigit := (PByte(pNibble)^ shr 4)
    else begin
      NumDigit := (PByte(pNibble)^ and $0f);
      Inc(pNibble); //next nibble
    end;
    if ZeroScale and (NumDigit = 0)//skip !all! trailing zeroes
    then Inc(ExpOffset, Ord(NotMultiplyBy10)) //remainder pack the oci number else reading them back is dead slow
    else begin
      ZeroScale := False;
      if NotMultiplyBy10 then begin
        if Negative
        then NumDigit := 101 - PByte(pNum)^ + NumDigit
        else NumDigit := PByte(pNum)^ + NumDigit + 1;
        PByte(pNum)^ := Byte(NumDigit);
        if X = y then
          if (NumDigit = NVUBase100Adjust[Negative])
          then PByte(pNum)^ := 0
          else Inc(pNum, Ord(NumDigit <> 0));  //remainder for len calculation
      end else
        PByte(pNum)^ := NumDigit * 10;
    end;
    { now invert the getter/setter logic }
    GetFirstBCDHalfByte := not GetFirstBCDHalfByte;
    NotMultiplyBy10 := not NotMultiplyBy10;
  end;
  NumDigit := Precision - Scale;
  NumDigit := NumDigit + (NumDigit and 1);
  num^[1] := (NumDigit shr 1)-(ExpOffset) + 65 + 128; //set the exponent
  if Negative then begin
    num^[1] := not num^[1]; //invert the bits
    if pNum < PLastNum then begin
      PByte(pNum)^ := 102; //as documented for whatever it is..
      inc(pNum); //for len calculation
    end;
  end;
  Result := pNum-PAnsiChar(Num);
  num^[0] := Result - 1;
end;*)


{** Autor: EgonHugeist (EH) *}
procedure Nvu2BCD(num: POCINumber; var bcd: TBCD);
var bb,size   : byte;
  Exp       : SmallInt;
  Scale, Precision: integer;
  Positive, HalfNibbles :Boolean;
  pNibble, pFirstNuDigit, pNuDigits, pNuEndDigit: PAnsiChar;
label zero;
begin
  FillChar(bcd, SizeOf(bcd), #0);
  Size := num[0];
  bb := num[1];
  if (Size=1) and ((bb=$80) or (bb=$c1)) then
    goto Zero; //fpc zero bcd returns '000.00' if default precision is 10 and scale is 2
  if ((Size=1) and (bb = 0) {neg infinity}) or
     ((Size=2) and (bb = 255) and (num[2] = 101) {pos infinity}) then
    Exit;
  Positive := (bb and $80)=$80;
  if Positive
  then exp := (bb and $7f)-65
  else begin
    exp := (not(bb) and $7f)-65;
    if Num[Size] = 102 then//last byte does not count if 102
      Dec(Size);
  end;
  if exp < 0 then begin
    exp := Abs(exp);
    pNibble := @bcd.Fraction[exp - 1] ;
    Precision := (exp - 1) shl 1 + (Size - 1) shl 1;
    Scale := Precision;
  end else begin
    pNibble := @bcd.Fraction[0];
    if exp >= (Size - 1) then begin //int range ?
      Precision := (exp + 1) shl 1;
      Scale := 0;
    end else begin
      Precision := (Size - 1) shl 1;
      Scale := Precision - (exp + 1) shl 1;
    end;
  end;
  HalfNibbles := False;
  pFirstNuDigit := @num[2];
  pNuEndDigit := @num[Size];
  { padd leading double zeroes away }
  while (Precision > Scale +1) and ( pFirstNuDigit <= pNuEndDigit) and
        (PByte(pFirstNuDigit)^ =NVUBase100Adjust[Positive]) do begin
    Dec(Precision, 2);
    Inc(pFirstNuDigit);
  end;
  pNuDigits := pFirstNuDigit;
  { padd traling double zeroes away }
  while (Scale > 2 ) and ( pNuDigits <= pNuEndDigit) and
        (PByte(pNuEndDigit)^ =NVUBase100Adjust[Positive]) do begin
    Dec(Scale, 2);
    Dec(pNuEndDigit);
  end;
  if pNuDigits > pNuEndDigit then goto zero;
  { fill the bcd }
  while (pNuDigits <= pNuEndDigit) do begin
    if Positive
    then bb := PByte(pNuDigits)^ - 1
    else bb := 101 - PByte(pNuDigits)^;
    bb := ZSysUtils.ZBase100Byte2BcdNibbleLookup[BB];
    if (Precision > Scale) and (pFirstNuDigit = pNuDigits) and (bb shr 4 = 0) then begin //first digit decides if we pack left
      Dec(Precision);
      PByte(PNibble)^ := (bb and $0f) shl 4;
      HalfNibbles := True;
      Inc(pNuDigits);
      Continue;
    end;
    if (pNuDigits = pNuEndDigit) and { padd possible trailing !last! zero away }
          (Scale > 0) and ((bb and $0F) = 0) then begin
      Dec(Precision);
      Dec(Scale);
    end;
    if HalfNibbles then begin
      PByte(PNibble)^ := PByte(PNibble)^ or (bb shr 4);
      PByte(PNibble+1)^ := (bb and $0f) shl 4;
    end else
      PByte(PNibble)^ := bb;
    Inc(pNuDigits);
    Inc(pNibble);
  end;
  PCardinal(pNibble+Byte(HalfNibbles))^ := 0; //some compilers read over lastnibble (FPC+XE10.3x64 f.e.)
  if Positive
  then Bcd.SignSpecialPlaces := Byte(Scale)
  else Bcd.SignSpecialPlaces := Byte(Scale) or $80;
  if Precision = 0 then
zero: Bcd.Precision := 1
  else Bcd.Precision := Byte(Precision);
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
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer): TZSQLType;
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
    Result := stUnicodeStream
  else if TypeNameUp = 'LONG' then
    Result := stAsciiStream
  else if (TypeNameUp = 'ROWID') or (TypeNameUp = 'UROWID') then
    Result := stString
  else if StartsWith(TypeNameUp, 'TIMESTAMP') then
    Result := stTimestamp
  else if TypeNameUp = 'BFILE' then
    Result := stBinaryStream else
  if TypeNameUp = 'NUMBER' then begin //numer is signed always
    if (Scale = 0) and (Precision > 0) and (Precision <= 18) then begin
      if Precision <= 2 then
        Result := stShort
      else if Precision <= 4 then
        Result := stSmall
      else if Precision <= 8 then
        Result := stInteger
      else
        Result := stLong
    end else if (Scale >= 0) and (Scale <= 4) and (Precision > 0) and (Precision < sAlignCurrencyScale2Precision[Scale]) then
      Result := stCurrency
    else
      Result := stBigDecimal;  { default for number types}
  end
  else if StartsWith(TypeNameUp, 'INTERVAL') then
    Result := stTimestamp
  else
    Result := stUnknown;
end;

function NormalizeOracleTypeToSQLType(var DataType: ub2; var DataSize: ub4;
  out DescriptorType: sb4; var Precision: Integer; ScaleOrCharSetForm: sb2;
  ConSettings: PZConSettings): TZSQLType;
label VCS;
  procedure CharacterSizeToByteSize(var DataSize: ub4; Precision: Integer);
  begin
    //EH: Note NCHAR, NVARCHAR2, CLOB, and NCLOB columns are always character-based.
    if (ScaleOrCharSetForm = SQLCS_NCHAR) or (ConSettings.ClientCodePage.Encoding = ceUTF16)
    then DataSize := Precision shl 2
    else if (ConSettings.ClientCodePage.CharWidth > 1)
      then DataSize := Precision * Byte(ConSettings.ClientCodePage.CharWidth)
      else DataSize := Precision;
  end;
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
        if (ScaleOrCharSetForm = -127) and (Precision > 0) then begin
          //see: https://docs.oracle.com/cd/B13789_01/appdev.101/b10779/oci06des.htm
          //Table 6-14 OCI_ATTR_PRECISION/OCI_ATTR_SCALE
          Result := stDouble;
          DataType := SQLT_BDOUBLE;
          DataSize := SizeOf(Double);
        end else if (ScaleOrCharSetForm = 0) and (Precision > 0) and (Precision < 19) then
          //No digits found, but possible signed or not/overrun of converions? No way to find this out -> just use a "save" type
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
            5..9: begin //(-)99999..(-)999.999.999
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
          if (ScaleOrCharSetForm >= 0) and (ScaleOrCharSetForm <= 4) and (Precision > 0) and (Precision <= sAlignCurrencyScale2Precision[ScaleOrCharSetForm])
          then Result := stCurrency
          else Result := stBigDecimal;
        end;
    SQLT_INT, _SQLT_PLI {signed short/int/long/longlong}: begin
        DataType := SQLT_INT;
        case DataSize of
          SizeOf(Int64):    Result := stLong;
          SizeOf(Integer):  Result := stInteger;
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
                if (Precision = 0) then
                  Precision := 2000; //2000 bytes max
                CharacterSizeToByteSize(DataSize, Precision);
                if (ScaleOrCharSetForm = SQLCS_NCHAR)
                then Result := stUnicodeString
                else Result := stString;
              end;
    SQLT_RID, { char[n] }
    SQLT_AVC:{CHAR / char[n+1]} begin
          if Precision = 0 then
            Precision := 2000; //2000 bytes max
          goto VCS;
        end;
    SQLT_CHR, {VARCHAR2 / char[n+1]}
    SQLT_STR,{NULL-terminated STRING, char[n+1]}
    SQLT_VCS {VARCHAR / char[n+sizeof(short integer)]}: begin
                if Precision = 0 then
                  Precision := 4000; //4000 bytes max
VCS:            CharacterSizeToByteSize(DataSize, Precision);
                DataType := SQLT_VCS;
                DataSize := DataSize+SizeOf(sb2);
                if ScaleOrCharSetForm = SQLCS_NCHAR
                then Result := stUnicodeString
                else Result := stString;
              end;
    SQLT_DAT: {char[7]} begin
              DataSize := SizeOf(TOraDate);
              Result := stTimestamp;
            end;
    SQLT_BIN, {RAW / unsigned char[n]}
    SQLT_VBI { unsigned char[n+sizeof(short integer)] }: begin
        result := stBytes;
        if (DataSize = 0) then
          DataSize := 2000;
        if Precision = 0 then
          Precision := DataSize;
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
    SQLT_VST: begin{EH: OCI STRING type / *OCIString recommedend by Oracle see:
      https://docs.oracle.com/cd/B28359_01/appdev.111/b28395/oci12oty.htm#i421612
      this is a opaque Type...
      but there advice is using the OCIStringXXX functions for Length/data
      humm hard to judge! it migth be possible this is fast indeed!
      The more ... and for me the only advantage : Bidirectional or out params
      are buffered by OCI we can ignore all length buffers on oversized memallocs
      my crystall ball says this is a PP(Raw/Wide)Char-Struct including length like TOCILong
      -> just look to OCIRaw/SQLT_LVB of https://docs.oracle.com/cd/B13789_01/appdev.101/b10779/oci11oty.htm#421682
      might be a good replacement for SQLT_LNG/SQLT_LVC ?}
        DataSize := SizeOf(POCIString);
        if ScaleOrCharSetForm = SQLCS_NCHAR
        then Result := stUnicodeString //stUnicodeStream
        else Result := stString; //stAsciiStream
      end;
    SQLT_LNG: { LONG /char[n] } begin
        if (Precision = 0) then begin
           Precision := 128 * 1024;
           DataSize := Precision;
           Result := stAsciiStream;
        end else begin
          DataSize := Precision;
          Result := stString;
        end;
        DataSize := DataSize + SizeOf(sb4);
        DataType := SQLT_LVC; { EH: http://zeoslib.sourceforge.net/viewtopic.php?t=3530 }
        Exit; //is this correct?
      end;
    SQLT_LVC { LONG VARCHAR / char[n+sizeof(integer)] }: begin //up to 2GB
        if Precision = 0 then
          Precision := 128 * 1024;
        CharacterSizeToByteSize(DataSize, Precision);
        if ScaleOrCharSetForm = SQLCS_NCHAR
        then Result := stUnicodeString //stUnicodeStream
        else Result := stString; //stAsciiStream
        DataSize := DataSize + SizeOf(sb4);
      end;
    SQLT_LBI, { LONG RAW / unsigned char[n] }
    SQLT_LVB { LONG VARRAW / unsigned char[n+sizeof(integer)]}:begin
        Result := stBinaryStream;
        if (DataSize = 0) then
          DataSize := 128 * 1024; //total allowed: one col per table and limits to 2GB-1
        Precision := DataSize;
        DataSize := DataSize + SizeOf(Integer);
        DataType := SQLT_LVB;
      end;
    SQLT_RDD {ROWID descriptor / OCIRowid * }: begin
        {DescriptorType := OCI_DTYPE_ROWID;
        DataSize := SizeOf(POCIRowid);}
        Precision := DataSize;
        DataSize := Max(20, DataSize);
        goto VCS;
      end;
    SQLT_NTY {NAMED DATATYPE / struct }: begin
        Result := stUnknown;
        DataSize := SizeOf(Pointer);
      end;
    SQLT_REF: { REF / OCIRef } ;
    SQLT_CLOB: { Character LOB descriptor / OCIClobLocator }begin
        if ScaleOrCharSetForm = SQLCS_NCHAR
        then Result := stUnicodeStream
        else Result := stAsciiStream;
        DescriptorType := OCI_DTYPE_LOB;
        DataSize := SizeOf(POCILobLocator);
      end;
    SQLT_BLOB: { Binary LOB descriptor / OCIBlobLocator } begin
        Result := stBinaryStream;
        DescriptorType := OCI_DTYPE_LOB;
        DataSize := SizeOf(POCILobLocator);
      end;
    SQLT_BFILEE, SQLT_CFILEE: { Binary file descriptor / OCILobLocator } begin
        Result := stBinaryStream;
        DescriptorType := OCI_DTYPE_FILE;
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
end;
(*
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
end; *)

function OCIType2Name(DataType: ub2): String;
begin
  case DataType of
    SQLT_CHR: Result := 'CHAR';
    SQLT_NUM: Result := 'NUMBER';
    SQLT_INT: Result := 'INT';
    (*
    SQLT_FLT = 4  ;       //(ORANET TYPE) Floating point number
    SQLT_STR = 5  ;       //zero terminated string
    //see https://stackoverflow.com/questions/25808798/ocidefinebypos-extract-number-value-via-cursor-in-c
    SQLT_VNU = 6  ;       //NUM with preceding length byte suggested tu use in OCICalls see orl.h
    SQLT_PDN = 7  ;       //(ORANET TYPE) Packed Decimal Numeric
    SQLT_LNG = 8  ;       //long
    SQLT_VCS = 9  ;       //Variable character string
    SQLT_NON = 10 ;       //Null/empty PCC Descriptor entry
    SQLT_RID = 11 ;       //rowid
    SQLT_DAT = 12 ;       //date in oracle format
    SQLT_VBI = 15 ;       //binary in VCS format
    SQLT_BFLOAT = 21 ;    //Native Binary float
    SQLT_BDOUBLE = 22 ;   //NAtive binary double
    SQLT_BIN = 23 ;       //binary data(DTYBIN)
    SQLT_LBI = 24 ;       //long binary
    _SQLT_PLI = 29;
    SQLT_UIN = 68 ;       //unsigned integer
    SQLT_SLS = 91 ;       //Display sign leading separate
    SQLT_LVC = 94 ;       //Longer longs (char)
    SQLT_LVB = 95 ;       //Longer long binary
    SQLT_AFC = 96 ;       //char[n] Ansi fixed char
    SQLT_AVC = 97 ;       //char[n+1] Ansi Var char
    SQLT_IBFLOAT = 100;   //binary float canonical
    SQLT_IBDOUBLE = 101;  //binary double canonical
    SQLT_CUR = 102;       //cursor  type
    SQLT_RDD = 104;       //rowid descriptor
    SQLT_LAB = 105;       //label type
    SQLT_OSL = 106;       //oslabel type
    SQLT_NTY = 108;       //named object type
    SQLT_REF = 110;       //ref typ
    SQLT_CLOB = 112;      //character lob
    SQLT_BLOB = 113;      //binary lob
    SQLT_BFILEE = 114;    //binary file lob
    SQLT_CFILEE = 115;    //character file lob
    SQLT_RSET = 116;      //result set type
    SQLT_NCO = 122;       //named collection type (varray or nested table)
    SQLT_VST = 155;       //OCI STRING type / *OCIString
    SQLT_ODT = 156;       //OCIDate type

    { datetimes and intervals }
    SQLT_DATE = 184;            //ANSI Date
    SQLT_TIME = 185;            //TIME
    SQLT_TIME_TZ = 186;         //TIME WITH TIME ZONE
    SQLT_TIMESTAMP = 187;       //TIMESTAMP
    SQLT_TIMESTAMP_TZ = 188;    //TIMESTAMP WITH TIME ZONE
    SQLT_INTERVAL_YM = 189;     //INTERVAL YEAR TO MONTH
    SQLT_INTERVAL_DS = 190;     //INTERVAL DAY TO SECOND
    SQLT_TIMESTAMP_LTZ = 232;   //TIMESTAMP WITH LOCAL TZ

    _SQLT_REC = 250;
    _SQLT_TAB = 251;
    _SQLT_BOL = 252;

    { > typecode defines from oro.h }
    OCI_TYPECODE_REF              = SQLT_REF; //SQL/OTS OBJECT REFERENCE
    OCI_TYPECODE_VARRAY           = 247;      //SQL VARRAY  OTS PAGED VARRAY
    OCI_TYPECODE_TABLE            = 248;      //SQL TABLE  OTS MULTISET
    OCI_TYPECODE_OBJECT           = SQLT_NTY; //SQL/OTS NAMED OBJECT TYPE
    OCI_TYPECODE_OPAQUE           = 58;       //SQL/OTS Opaque Types
    OCI_TYPECODE_NAMEDCOLLECTION  = SQLT_NCO;

    *)
    else Result := 'unknown'
  end;
end;

{ TZOraProcDescriptor_A }

Const ArgListType: array[Boolean] of ub4 = (OCI_ATTR_LIST_ARGUMENTS, OCI_ATTR_LIST_SUBPROGRAMS);

procedure TZOraProcDescriptor_A.ConcatParentName(NotArgName: Boolean;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF} SQLWriter: TZRawSQLStringWriter;
  var Result: RawByteString; const IC: IZIdentifierConvertor);
{$IFDEF UNICODE}
var S: UnicodeString;
    R: RawByteString;
{$ENDIF}
begin
  if (FParent <> nil) then begin
    FParent.ConcatParentName(NotArgName, SQLWriter, Result, IC);
    if NotArgName then begin
      {$IFDEF UNICODE}
      S := ZRawToUnicode(FParent.AttributeName, FRawCP);
      S := IC.Quote(S);
      R := ZUnicodeToRaw(S, FRawCP);
      SQLWriter.AddText(R, Result);
      {$ELSE}
      SQLWriter.AddText(IC.Quote(FParent.AttributeName), Result);
      {$ENDIF}
      SQLWriter.AddChar(AnsiChar('.'), Result);
    end else if ((ObjType = OCI_PTYPE_ARG) and (FParent.Parent <> nil) and (FParent.Parent.ObjType = OCI_PTYPE_PKG) and (FParent.Parent.Args.Count > 1)) {or
       ((FParent.ObjType = OCI_PTYPE_PKG) and (FParent.Args.Count > 1)) }then begin
      SQLWriter.AddText(FParent.AttributeName, Result);
      SQLWriter.AddChar(AnsiChar('_'), Result);
    end;
  end;
end;

constructor TZOraProcDescriptor_A.Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}
  Parent: TZOraProcDescriptor_A; Const Connection: IZOracleConnection{$IFDEF UNICODE};
  RawCP: Word{$ENDIF});
begin
  inherited Create(Connection, nil, OCI_PTYPE_UNK, Connection.GetErrorHandle);
  fParent := Parent;
  {$IFDEF UNICODE}
  FRawCP := RawCP;
  {$ENDIF UNICODE}
  FConnection := Connection;
end;

const OCIAttrGetA: String = 'OCIAttrGet';

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "i" does not seem to be initialized} {$ENDIF}
function TZOraProcDescriptor_A.InternalDescribe(const Name: RawByteString;
  _Type: UB4; Owner: POCIHandle): Sword;
var P: PAnsiChar;
  i: sb4;
  parmh: POCIHandle;
  Descriptor: POCIDescribe;
  tmp: RawByteString;
  {$IFDEF WITH_RAWBYTESTRING}
  ConSettings: PZConSettings;
  {$ENDIF WITH_RAWBYTESTRING}
  OCISvcCtx: POCISvcCtx;
begin
  //https://www.bnl.gov/phobos/Detectors/Computing/Orant/doc/appdev.804/a58234/describe.htm#440341
  //section describing the stored procedure
  Descriptor := nil;
  {$IFDEF WITH_RAWBYTESTRING}
  ConSettings := FConnection.GetConSettings;
  {$ENDIF WITH_RAWBYTESTRING}
  OCISvcCtx   := FConnection.GetServiceContextHandle;
  { get a descriptor handle for the param/obj }
  Result := FPlainDriver.OCIHandleAlloc(Owner, Descriptor, OCI_HTYPE_DESCRIBE, 0, nil);
  if Result <> OCI_SUCCESS then
    FConnection.HandleErrorOrWarning(FOCIError, Result, lcOther, 'OCIHandleAlloc', Self);
  Result := FPlainDriver.OCIDescribeAny(OCISvcCtx, FOCIError, Pointer(Name),
        Length(Name), OCI_OTYPE_NAME, 0, _Type, Descriptor);
  if Result <> OCI_SUCCESS then begin
    tmp := '"PUBLIC".'+Name;
    Result := FPlainDriver.OCIDescribeAny(OCISvcCtx, FOCIError, Pointer(tmp),
        Length(tmp), OCI_OTYPE_NAME, 0, _Type, Descriptor);
  end;
  try
    if Result <> OCI_SUCCESS then
      Exit;
    SetHandleAndType(Descriptor, OCI_HTYPE_DESCRIBE);
    parmh := GetPointer(OCI_ATTR_PARAM);
    SetHandleAndType(parmh, OCI_HTYPE_DESCRIBE);
    { get the schema name }
    P := GetPChar(i, OCI_ATTR_OBJ_SCHEMA);
    if P = nil then begin
      Result := OCI_ERROR;
      Exit;
    end;
    ZSetString(P, I, SchemaName{$IFDEF WITH_RAWBYTESTRING}, ConSettings.ClientCodePage.CP{$ENDIF});
    { get the objectname }
    P := GetPChar(i, OCI_ATTR_OBJ_NAME);
    ZSetString(P, I, AttributeName{$IFDEF WITH_RAWBYTESTRING}, ConSettings.ClientCodePage.CP{$ENDIF});
    { get the first object type }
    ObjType := GetUb1(OCI_ATTR_PTYPE);
    InternalDescribeObject(parmh);
  finally
    if Descriptor <> nil then begin
      FPlainDriver.OCIDescriptorFree(Descriptor, OCI_HTYPE_DESCRIBE);
      Descriptor := nil;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "arg/i" does not seem to be initialized} {$ENDIF}
procedure TZOraProcDescriptor_A.InternalDescribeObject(Obj: POCIHandle);
var
  arglst, arg: POCIHandle;
  i, N: sb4;
  ParamCount: ub2;
  p: PAnsichar;
  Param: TZOraProcDescriptor_A;
  CP: Word;
  Status: Sword;
  ConSettings: PZConSettings;
begin
  ConSettings := FConnection.GetConSettings;
  CP := ConSettings.ClientCodePage.CP;
  if ObjType <> OCI_PTYPE_PKG then begin
    { get the overload position }
    SetHandleAndType(obj, OCI_HTYPE_DESCRIBE);
    OverloadID := GetUb2(OCI_ATTR_OVERLOAD_ID);
  end;
  { get a argument-list handle }
  SetHandleAndType(obj, OCI_DTYPE_PARAM);
  arglst := GetPointer(ArgListType[ObjType = OCI_PTYPE_PKG]);
  { get argument count using of the list handle }
  SetHandleAndType(arglst, OCI_DTYPE_PARAM);
  ParamCount := GetUb2(OCI_ATTR_NUM_PARAMS);
  Args := TObjectList.Create;
  Args.Capacity := ParamCount;
  for N := Ord(ObjType = OCI_PTYPE_PROC) to ParamCount-1+Ord(ObjType = OCI_PTYPE_PROC) do begin
    { get a argument handle }
    Status := FPlainDriver.OCIParamGet(arglst, OCI_DTYPE_PARAM, FOCIError, arg, N);
    if Status <> OCI_SUCCESS then
      FConnection.HandleErrorOrWarning(FOCIError, Status, lcOther, OCIAttrGetA, Self);
    Param := TZOraProcDescriptor_A.Create(Self, FConnection{$IFDEF UNICODE}, CP{$ENDIF});
    Args.Add(Param);
    Param.SchemaName := SchemaName;
    { get the object type }
    Param.SetHandleAndType(arg, OCI_HTYPE_DESCRIBE);
    Param.ObjType := Param.GetUb1(OCI_ATTR_PTYPE);
    { get the attribute Name }
    P := Param.GetPChar(I, OCI_ATTR_NAME);
    ZSetString(P, I, Param.AttributeName{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
    if Param.ObjType = OCI_PTYPE_ARG then begin
      { get the ordinal position }
      Param.OrdPos := Param.GetUb2(OCI_ATTR_POSITION);
      if (Param.OrdPos = 0) and (Param.AttributeName = '') then
        Param.AttributeName := 'ReturnValue';
      P := Param.GetPChar(I, OCI_ATTR_TYPE_NAME);
      ZSetString(P, I, Param.TypeName{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
      Param.DataSize := Param.GetUb4(OCI_ATTR_DATA_SIZE);
      Param.IODirection := Param.GetSb4(OCI_ATTR_IOMODE);
      Param.DataType := Param.GetUb2(OCI_ATTR_DATA_TYPE);
      if Param.DataType in [SQLT_NUM, SQLT_VNU] then begin {11g returns Precision = 38 in all cases}
        Param.Precision := Param.GetUb1(OCI_ATTR_PRECISION);
        Param.Scale := Param.GetSb1(OCI_ATTR_SCALE);
        Param.Radix := Param.GetUb1(OCI_ATTR_RADIX);
      end else if Param.DataType in [SQLT_DATE..SQLT_TIMESTAMP_LTZ] then begin
        Param.Precision := Param.GetUb1(OCI_ATTR_LFPRECISION);
        Param.Scale := Param.GetSb1(OCI_ATTR_FSPRECISION);
      end;
      if Param.DataType in [SQLT_CHR, SQLT_LNG, SQLT_VCS, SQLT_LVC, SQLT_AFC, SQLT_AVC, SQLT_CLOB, SQLT_VST] then begin
        if Param.DataType <> SQLT_CLOB then
          Param.Precision := Param.GetUb2(OCI_ATTR_CHAR_SIZE);
        Param.csform := Param.GetUb1(OCI_ATTR_CHARSET_FORM);
        Status := Param.csform;
      end else Status := Param.Scale;
      Param.SQLType := NormalizeOracleTypeToSQLType(Param.DataType, Param.DataSize,
        Param.DescriptorType, Param.Precision, Status, ConSettings);
      if (Param.SQLType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then
        if (Param.csform = SQLCS_NCHAR) or ((Consettings.ClientCodePage.Encoding = ceUTF16) and (Param.DataType <> SQLT_LNG)) then begin
          Param.CodePage := zCP_UTF16;
          Param.csid := OCI_UTF16ID;
        end else begin
          Param.CodePage := CP;
          Param.csid := Param.GetUb2(OCI_ATTR_CHARSET_ID);
        end;
    end else
      Param.InternalDescribeObject(arg);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOraProcDescriptor_A.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var imm: IImmediatelyReleasable;
  I: Integer;
begin
  if Assigned(FParent) then begin
    FParent.QueryInterface(IImmediatelyReleasable, imm);
    if (imm <> Sender) then
      imm.ReleaseImmediat(Sender, AError);
  end;
  if (Args <> nil) then
    for i := 0 to Args.Count do
      TZOracleAttribute(Args[i]).ReleaseImmediat(Sender, AError);
  inherited ReleaseImmediat(Sender, AError);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "arg/i" does not seem to be initialized} {$ENDIF}
procedure TZOraProcDescriptor_A.Describe(_Type: UB4; const Name: RawByteString);
var
  ProcSQL, tmp: RawByteString;
  Status, ps, ps2: sword;
  IC: IZIdentifierConverter;
  {$IFDEF UNICODE}
  S: String;
  {$ENDIF}
  OCIEnv: POCIEnv;
begin
  OCIEnv := FConnection.GetConnectionHandle;
  ProcSQL := Name;

  IC := FConnection.GetMetadata.GetIdentifierConverter;
  { describe the object: }
  Status := InternalDescribe(ProcSQL, _Type, OCIEnv);
  if (Status <> OCI_SUCCESS) then begin
    ps := ZFastCode.PosEx(RawByteString('.'), ProcSQL);
    if ps <> 0 then begin //check wether Package or Schema!
      tmp := Copy(ProcSQL, 1, ps-1);
      Status := InternalDescribe(tmp, _Type, OCIEnv);
      if Status <> OCI_SUCCESS then begin
        ps2 := ZFastCode.PosEx(RawByteString('.'), ProcSQL, ps+1);
        if ps2 <> 0 then //check wether Package or Schema!
          tmp := Copy(ProcSQL, ps+1, ps2-ps-1)
        else begin
          ps2 := ps;
          tmp := Copy(ProcSQL, ps+1, maxint)
        end;
        Status := InternalDescribe(tmp, OCI_PTYPE_UNK, OCIEnv);
        if Status = OCI_SUCCESS then
          tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else begin { final approach to locate the procedure !}
          tmp := '"PUBLIC".'+tmp;
          Status := InternalDescribe(tmp, OCI_PTYPE_UNK, OCIEnv);
          if Status <> OCI_SUCCESS then
            FConnection.HandleErrorOrWarning(FConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', Self);
        end;
      end else begin
        ps2 := ZFastCode.PosEx(RawByteString('.'), ProcSQL, ps+1);
        if ps2 <> 0 //check wether Package or Schema!
        then tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else tmp := copy(ProcSQL, Ps+1, MaxInt)
      end;
      if (ObjType = OCI_PTYPE_PKG) then begin
        //next stage obj. needs to be compared to package procs
        //strip all other procs we don't need!
        {$IFDEF UNICODE}
        S := ZRawToUnicode(Tmp, FRawCP);
        S := IC.ExtractQuote(S);
        Tmp := ZUnicodeToRaw(S, FRawCP);
        {$ELSE}
        Tmp := IC.ExtractQuote(Tmp);
        {$ENDIF}
        for ps := Args.Count -1 downto 0 do begin
          if TZOraProcDescriptor_A(Args[ps]).AttributeName <> tmp then
            Args.Delete(ps);
        end;
      end;
    end;
  end else if Status <> OCI_SUCCESS then
    FConnection.HandleErrorOrWarning(FConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', Self);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZOraProcDescriptor_A.Destroy;
begin
  inherited Destroy;
  if Args <> nil then
    FreeAndNil(Args);
end;

{ TZOraProcDescriptor_W }

procedure TZOraProcDescriptor_W.ConcatParentName(NotArgName: Boolean;
  {$IFDEF AUTOREFCOUNT} const {$ENDIF}SQLWriter: TZUnicodeSQLStringWriter;
  var Result: UnicodeString; const IC: IZIdentifierConverter);
{$IFNDEF UNICODE}
var S: UnicodeString;
    R: RawByteString;
{$ENDIF}
begin
  if (FParent <> nil) then begin
    FParent.ConcatParentName(NotArgName, SQLWriter, Result, IC);
    if NotArgName then begin
      {$IFDEF UNICODE}
      SQLWriter.AddText(IC.Quote(FParent.AttributeName), Result);
      {$ELSE}
      R := ZUnicodeToRaw(FParent.AttributeName, FRawCP);
      R := IC.Quote(R);
      S := ZRawToUnicode(R, FRawCP);
      SQLWriter.AddText(S, Result);
      {$ENDIF}
      SQLWriter.AddChar('.', Result);
    end else if ((ObjType = OCI_PTYPE_ARG) and (FParent.Parent <> nil) and (FParent.Parent.ObjType = OCI_PTYPE_PKG) and (FParent.Parent.Args.Count > 1)) {or
       ((FParent.ObjType = OCI_PTYPE_PKG) and (FParent.Args.Count > 1)) }then begin
      SQLWriter.AddText(FParent.AttributeName, Result);
      SQLWriter.AddChar('_', Result);
    end;
  end;
end;

constructor TZOraProcDescriptor_W.Create({$IFDEF AUTOREFCOUNT} const {$ENDIF}
  Parent: TZOraProcDescriptor_W; Const Connection: IZOracleConnection{$IFNDEF UNICODE};
  RawCP: Word{$ENDIF});
begin
  inherited Create(Connection, nil, OCI_PTYPE_UNK, Connection.GetErrorHandle);
  fParent := Parent;
  {$IFNDEF UNICODE}
  FRawCP := RawCP;
  {$ENDIF UNICODE}
end;

procedure TZOraProcDescriptor_W.Describe(_Type: UB4;
  const Name: UnicodeString);
var
  ProcSQL, tmp: UnicodeString;
  Status, ps, ps2: sword;
  IC: IZIdentifierConverter;
  {$IFNDEF UNICODE}
  S: String;
  {$ENDIF}
  OCIEnv: POCIEnv;
begin
  OCIEnv := FConnection.GetConnectionHandle;
  ProcSQL := Name;

  IC := FConnection.GetMetadata.GetIdentifierConverter;
  { describe the object: }
  Status := InternalDescribe(ProcSQL, _Type, OCIEnv);
  if (Status <> OCI_SUCCESS) then begin
    ps := ZFastCode.Pos(UnicodeString('.'), ProcSQL);
    if ps <> 0 then begin //check wether Package or Schema!
      tmp := Copy(ProcSQL, 1, ps-1);
      Status := InternalDescribe(tmp, _Type, OCIEnv);
      if Status <> OCI_SUCCESS then begin
        ps2 := ZFastCode.PosEx(UnicodeString('.'), ProcSQL, ps+1);
        if ps2 <> 0 then //check wether Package or Schema!
          tmp := Copy(ProcSQL, ps+1, ps2-ps-1)
        else begin
          ps2 := ps;
          tmp := Copy(ProcSQL, ps+1, maxint)
        end;
        Status := InternalDescribe(tmp, _Type, OCIEnv);
        if Status = OCI_SUCCESS then
          tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else begin { final approach to locate the procedure !}
          tmp := '"PUBLIC".'+tmp;
          Status := InternalDescribe(tmp, _Type, OCIEnv);
          if Status <> OCI_SUCCESS then
            FConnection.HandleErrorOrWarning(FConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', Self);
        end;
      end else begin
        ps2 := ZFastCode.PosEx(UnicodeString('.'), ProcSQL, ps+1);
        if ps2 <> 0 //check wether Package or Schema!
        then tmp := copy(ProcSQL, Ps2+1, MaxInt)
        else tmp := copy(ProcSQL, Ps+1, MaxInt)
      end;
      if (ObjType = OCI_PTYPE_PKG) then begin
        //next stage obj. needs to be compared to package procs
        //strip all other procs we don't need!
        {$IFNDEF UNICODE}
        S := ZUnicodeToRaw(Tmp, FRawCP);
        S := IC.ExtractQuote(S);
        Tmp := ZRawToUnicode(S, FRawCP);
        {$ELSE}
        Tmp := IC.ExtractQuote(Tmp);
        {$ENDIF}
        for ps := Args.Count -1 downto 0 do begin
          if TZOraProcDescriptor_W(Args[ps]).AttributeName <> tmp then
            Args.Delete(ps);
        end;
      end;
    end;
  end else if Status <> OCI_SUCCESS then
    FConnection.HandleErrorOrWarning(FConnection.GetErrorHandle, Status, lcExecute, 'OCIDescribeAny', Self);
end;

destructor TZOraProcDescriptor_W.Destroy;
begin
  inherited Destroy;
  if Args <> nil then
    FreeAndNil(Args);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "i" does not seem to be initialized} {$ENDIF}
function TZOraProcDescriptor_W.InternalDescribe(const Name: UnicodeString;
  _Type: UB4; Owner: POCIHandle): Sword;
var objptr: PWideChar;
  objnm_len: ub4;
  parmh: POCIHandle;
  Descriptor: POCIDescribe;
  tmp: UnicodeString;
  OCISvcCtx: POCISvcCtx;
  procedure GetStringProp(attrtype: ub4; var Result: UnicodeString);
  var i: sb4;
      P: PWideChar;
  begin
    P := GetPChar(I, attrtype);
    I := I shr 1;
    System.SetString(Result, P, I);
  end;
label jmpDescibe;
begin
  //https://www.bnl.gov/phobos/Detectors/Computing/Orant/doc/appdev.804/a58234/describe.htm#440341
  //section describing the stored procedure
  Descriptor := nil;
  OCISvcCtx   := FConnection.GetServiceContextHandle;
  { get a descriptor handle for the param/obj }
  Result := FPlainDriver.OCIHandleAlloc(Owner, Descriptor, OCI_HTYPE_DESCRIBE, 0, nil);
  if Result <> OCI_SUCCESS then
    FConnection.HandleErrorOrWarning(FOCIError, Result, lcOther, 'OCIHandleAlloc', Self);
  tmp := Name;
jmpDescibe:
  objptr := Pointer(tmp);
  objnm_len := Length(tmp) shl 1;
  Result := FPlainDriver.OCIDescribeAny(OCISvcCtx, FOCIError, objptr,
        objnm_len, OCI_OTYPE_NAME, 0, _Type, Descriptor);
  if (Result <> OCI_SUCCESS) and (tmp = Name) then begin
    tmp := '"PUBLIC".'+Name;
    goto jmpDescibe;
  end;
  try
    if Result <> OCI_SUCCESS then
      Exit;
    SetHandleAndType(Descriptor, OCI_HTYPE_DESCRIBE);
    parmh := GetPointer(OCI_ATTR_PARAM);
    SetHandleAndType(parmh, OCI_HTYPE_DESCRIBE);
    GetStringProp(OCI_ATTR_OBJ_SCHEMA, SchemaName);
    if SchemaName = '' then begin
      Result := OCI_ERROR;
      Exit;
    end;
    GetStringProp(OCI_ATTR_OBJ_NAME, AttributeName);
    { get the first object type }
    ObjType := GetUb1(OCI_ATTR_PTYPE);
    InternalDescribeObject(parmh);
  finally
    if Descriptor <> nil then begin
      FPlainDriver.OCIDescriptorFree(Descriptor, OCI_HTYPE_DESCRIBE);
      Descriptor := nil;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "arg/i" does not seem to be initialized} {$ENDIF}
procedure TZOraProcDescriptor_W.InternalDescribeObject(Obj: POCIHandle);
var
  arglst, arg: POCIHandle;
  i, N: sb4;
  CP, ParamCount: ub2;
  p: PWideChar;
  Status: SWord;
  Param: TZOraProcDescriptor_W;
  ConSettings: PZConSettings;
begin
  ConSettings := FConnection.GetConSettings;
  CP := ConSettings.ClientCodePage.CP;
  if ObjType <> OCI_PTYPE_PKG then begin
    { get the overload position }
    SetHandleAndType(obj, OCI_HTYPE_DESCRIBE);
    OverloadID := GetUb2(OCI_ATTR_OVERLOAD_ID);
  end;
  { get a argument-list handle }
  SetHandleAndType(obj, OCI_DTYPE_PARAM);
  arglst := GetPointer(ArgListType[ObjType = OCI_PTYPE_PKG]);
  { get argument count using of the list handle }
  SetHandleAndType(arglst, OCI_DTYPE_PARAM);
  ParamCount := GetUb2(OCI_ATTR_NUM_PARAMS);
  Args := TObjectList.Create;
  Args.Capacity := ParamCount;
  for N := Ord(ObjType = OCI_PTYPE_PROC) to ParamCount-1+Ord(ObjType = OCI_PTYPE_PROC) do begin
    { get a argument handle }
    Status := FPlainDriver.OCIParamGet(arglst, OCI_DTYPE_PARAM, FOCIError, arg, N);
    if Status <> OCI_SUCCESS then
      FConnection.HandleErrorOrWarning(FOCIError, Status, lcOther, 'OCIParamGet', Self);
    Param := TZOraProcDescriptor_W.Create(Self, FConnection{$IFNDEF UNICODE}, CP{$ENDIF});
    Args.Add(Param);
    Param.SchemaName := SchemaName;
    { get the object type }
    Param.SetHandleAndType(arg, OCI_HTYPE_DESCRIBE);
    Param.ObjType := Param.GetUb1(OCI_ATTR_PTYPE);
    { get the attribute Name }
    P := Param.GetPChar(I, OCI_ATTR_NAME);
    I := I shr 1;
    System.SetString(Param.AttributeName, P, I);
    if Param.ObjType = OCI_PTYPE_ARG then begin
      { get the ordinal position }
      Param.OrdPos := Param.GetUb2(OCI_ATTR_POSITION);
      if (Param.OrdPos = 0) and (Param.AttributeName = '') then
        Param.AttributeName := 'ReturnValue';
      P := Param.GetPChar(I, OCI_ATTR_TYPE_NAME);
      I := I shr 1;
      System.SetString(Param.TypeName, P, I);
      Param.DataSize := Param.GetUb4(OCI_ATTR_DATA_SIZE);
      Param.IODirection := Param.GetSb4(OCI_ATTR_IOMODE);
      Param.DataType := Param.GetUb2(OCI_ATTR_DATA_TYPE);
      if Param.DataType in [SQLT_NUM, SQLT_VNU] then begin {11g returns Precision = 38 in all cases}
        Param.Precision := Param.GetUb1(OCI_ATTR_PRECISION);
        Param.Scale := Param.GetSb1(OCI_ATTR_SCALE);
        Param.Radix := Param.GetUb1(OCI_ATTR_RADIX);
      end else if Param.DataType in [SQLT_DATE..SQLT_TIMESTAMP_LTZ] then begin
        Param.Precision := Param.GetUb1(OCI_ATTR_LFPRECISION);
        Param.Scale := Param.GetUb1(OCI_ATTR_LFPRECISION);
      end else if Param.DataType in [SQLT_CHR, SQLT_LNG, SQLT_VCS, SQLT_LVC, SQLT_AFC, SQLT_AVC, SQLT_CLOB, SQLT_VST] then begin
        if Param.DataType <> SQLT_CLOB then
          Param.Precision := Param.GetUb2(OCI_ATTR_CHAR_SIZE);
        Param.csform := Param.GetUb1(OCI_ATTR_CHARSET_FORM);
      end;
      Param.SQLType := NormalizeOracleTypeToSQLType(Param.DataType, Param.DataSize,
        Param.DescriptorType, Param.Precision, Param.Scale, ConSettings);
      if (Param.SQLType in [stString, stAsciiStream]) then begin
        {EH: Oracle does not calculate true data size if the attachment charset is a multibyte one
          and is different to the native db charset
          so we'll increase the buffers to avoid truncation errors
          and we use 8 byte aligned buffers. Here we go:}
        Param.csform := Param.GetUb1(OCI_ATTR_CHARSET_FORM);
        if Param.SQLType = stString then begin
          //Param.Precision := Param.GetUb1(OCI_ATTR_DISP_SIZE);
          if Param.csform = SQLCS_NCHAR then begin
            Param.DataSize := Param.Precision;
            Param.Precision := Param.Precision shr 1;
            Param.SQLType := stUnicodeString;
            Param.CodePage := zCP_UTF16;
            Param.csid := OCI_UTF16ID;
          end else if Consettings.ClientCodePage.Encoding <> ceUTF16 then begin
            Param.DataSize := Param.Precision * ConSettings.ClientCodePage.CharWidth;
            Param.CodePage := CP;
          end else begin
            Param.DataSize := Param.Precision shl 1;
            Param.CodePage := zCP_UTF16;
            Param.SQLType := stUnicodeString;
          end;
        end else if (Param.csform = SQLCS_NCHAR) or (Consettings.ClientCodePage.Encoding = ceUTF16) then begin
          Param.SQLType := stUnicodeStream;
          Param.CodePage := zCP_UTF16
        end else Param.CodePage := CP;
        Param.csid := Param.GetUb2(OCI_ATTR_CHARSET_ID);
      end;
    end else
      Param.InternalDescribeObject(arg);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOraProcDescriptor_W.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var imm: IImmediatelyReleasable;
  I: Integer;
begin
  if Assigned(FParent) then begin
    FParent.QueryInterface(IImmediatelyReleasable, imm);
    if (imm <> Sender) then
      imm.ReleaseImmediat(Self, AError);
  end;
  if (Args <> nil) then
    for i := 0 to Args.Count do
      TZOracleAttribute(Args[i]).ReleaseImmediat(Self, AError);
  inherited ReleaseImmediat(Sender, AError);
end;

{ TZOracleAttribute }

constructor TZOracleAttribute.Create(const Connection: IZOracleConnection;
  trgthndlp: POCIHandle; trghndltyp: ub4; errhp: POCIError);
begin
  FOwner := Connection;
  FConSettings := Connection.GetConSettings;
  Ftrgthndlp := trgthndlp;
  Ftrghndltyp := trghndltyp;
  FOCIError := errhp;
  FConnection := Connection;
  FplainDriver := Connection.GetPlainDriver;
end;

function TZOracleAttribute.GetConSettings: PZConSettings;
begin
  Result := FConSettings;
end;

function TZOracleAttribute.GetPChar(var Len: sb4; attrtype: ub4): Pointer;
var Status: sword;
begin
  Result := nil;
  Len := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, @Len, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetPointer(attrtype: ub4): Pointer;
var Status: sword;
begin
  Result := nil;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetSb1(attrtype: ub4): Sb1;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetSb2(attrtype: ub4): Sb2;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetSb4(attrtype: ub4): Sb4;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetUb1(attrtype: ub4): ub1;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetUb2(attrtype: ub4): ub2;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

function TZOracleAttribute.GetUb4(attrtype: ub4): ub4;
var Status: sword;
begin
  Result := 0;
  Status := FPlainDriver.OCIAttrGet(Ftrgthndlp, Ftrghndltyp, @Result, nil, attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, True);
end;

const OCIAttr: Array[Boolean] of String = ('OCIAttrSet', 'OCIAttrGet');
procedure TZOracleAttribute.HandleAttributError(Status: SWord; WasGet: Boolean);
begin
  FConnection.HandleErrorOrWarning(FOCIError, Status, lcOther, OCIAttr[WasGet], Self);
end;

function TZOracleAttribute.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TZOracleAttribute.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  Ftrgthndlp := nil;
  if (FOwner <> Sender) and (FOwner <> nil) then
     FOwner.ReleaseImmediat(Sender, AError);
end;

procedure TZOracleAttribute.SetHandleAndType(trgthndlp: POCIHandle;
  trghndltyp: ub4);
begin
  Ftrgthndlp := trgthndlp;
  Ftrghndltyp := trghndltyp;
end;

procedure TZOracleAttribute.SetUb4(attrtype, Value: Ub4);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(Ub4), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

procedure TZOracleAttribute.SetSb4(attrtype: ub4; Value: Sb4);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(Sb4), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

procedure TZOracleAttribute.SetUb2(attrtype: ub4; Value: ub2);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(Ub2), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

procedure TZOracleAttribute.SetSb2(attrtype: ub4; Value: Sb2);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(Sb2), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

procedure TZOracleAttribute.SetUb1(attrtype: ub4; Value: ub1);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(Ub1), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

procedure TZOracleAttribute.SetSb1(attrtype: ub4; Value: Sb1);
var Status: sword;
begin
  Status := FPlainDriver.OCIAttrSet(Ftrgthndlp, Ftrghndltyp, @Value, SizeOf(sb1), attrtype, FOCIError);
  if Status <> OCI_SUCCESS then
    HandleAttributError(Status, False);
end;

function TZOracleAttribute._AddRef: Integer;
begin
  Result := -1;
end;

function TZOracleAttribute._Release: Integer;
begin
  Result := -1;
end;

initialization
{$ENDIF ZEOS_DISABLE_ORACLE}
end.
