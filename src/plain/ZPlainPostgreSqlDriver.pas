{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for PostgreSQL           }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainPostgreSqlDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses {$IFDEF OLDFPC}ZClasses, {$ENDIF}ZCompatibility, ZPlainDriver;

const
  WINDOWS_DLL_LOCATION   = 'libpq.dll';
  WINDOWS_DLL7_LOCATION   = 'libpq74.dll';
  WINDOWS_DLL8_LOCATION   = 'libpq81.dll';
  LINUX_DLL_LOCATION   = 'libpq'+SharedSuffix;
  LINUX_DLL8_LOCATION  = 'libpq'+SharedSuffix+'.4';
  LINUX_DLL82_LOCATION = 'libpq'+SharedSuffix+'.5';
  LINUX_DLL9_LOCATION = LINUX_DLL82_LOCATION;

{ Type Lengths }
  NAMEDATALEN  = 32;
{ used for type modifier calculation }
  VARHDRSZ = 4;

{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;
  InvalidOid   = 0;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  BLOB_SEEK_SET     = 0;
  BLOB_SEEK_CUR     = 1;
  BLOB_SEEK_END     = 2;


  BASE1000Digits    = 4;
{ PostgreSQL basic type OIDs. These OIDs are hard coded in PostgreSQL and according to the following mail never change:
  https://www.postgresql.org/message-id/AANLkTimiNjQa7ws1tyR_W6RQPec6RlxQtWfACNMnZ_1P@mail.gmail.com
  
  From: 	Merlin Moncure <mmoncure(at)gmail(dot)com>
  To: 	zhong ming wu <mr(dot)z(dot)m(dot)wu(at)gmail(dot)com>
  Cc: 	pgsql-general <pgsql-general(at)postgresql(dot)org>
  Subject: 	Re: oid data types mapping in libpq functions
  Date: 	2010-06-17 14:32:01
  Message-ID: 	AANLkTimiNjQa7ws1tyR_W6RQPec6RlxQtWfACNMnZ_1P@mail.gmail.com (view raw or download thread mbox)

  On Wed, Jun 16, 2010 at 10:42 PM, zhong ming wu <mr(dot)z(dot)m(dot)wu(at)gmail(dot)com> wrote:
  > Dear List
  >
  > Where can I find this mapping of oid to pg data types mentioned in
  > libpq documentation?
  > Why is such information not mentioned in the documentation?  A general
  > knowledge?

  curious: what do you need the oids for?

  built in type oids are defined in pg_type.h:
  cat src/include/catalog/pg_type.h | grep OID | grep define

  built in type oids don't change. you can pretty much copy/pasto the
  output of above into an app...just watch out for some types that may
  not be in older versions.

  user defined type oids (tables, views, composite types, enums, and
  domains) have an oid generated when it is created.  since that oid can
  change via ddl so you should look it up by name at appropriate times.

  if you want to be completely abstracted from the type oids, look here:
  http://libpqtypes.esilo.com/

  merlin

  Ok - this information really should be somewhere else, in the developer documentation but I don't want to lose it for now. We can remove it later on.
  
  So what follows is a list of PG hard coded OIDs. All other OIDs must be converted
  by a separate function to determine their type name.
}

{--------------------------------------------------------------------------------------}

{ types as found in pg_type.h in PostgreSQL 9.6 }

  { OIDS 1 - 99 }
  BOOLOID			  = 16;
  BYTEAOID			= 17;
  CHAROID			  = 18;
  NAMEOID			  = 19;
  INT8OID			  = 20;
  INT2OID			  = 21;
  INT2VECTOROID	= 22;
  INT4OID			  = 23;
  REGPROCOID		= 24;
  TEXTOID			  = 25;
  OIDOID			  = 26;
  TIDOID			  = 27;
  XIDOID 			  = 28;
  CIDOID 			  = 29;
  OIDVECTOROID	= 30;

  { OIDS 100 - 199 }
  JSONOID 			= 114;
  XMLOID 			  = 142;
  PGNODETREEOID	= 194;
  PGDDLCOMMANDOID = 32;

  { OIDS 200 - 299 }

  { OIDS 300 - 399 }

  { OIDS 400 - 499 }

  { OIDS 500 - 599 }

  { OIDS 600 - 699 }
  POINTOID		= 600;
  LSEGOID			= 601;
  PATHOID			= 602;
  BOXOID			= 603;
  POLYGONOID	= 604;
  LINEOID			= 628;

  { OIDS 700 - 799 }

  FLOAT4OID 	= 700;
  FLOAT8OID 	= 701;
  ABSTIMEOID	= 702;
  RELTIMEOID	= 703;
  TINTERVALOID = 704;
  UNKNOWNOID	= 705;

  CIRCLEOID		= 718;
  CASHOID 		= 790;

  { OIDS 800 - 899 }
  MACADDROID 	= 829;
  INETOID 		= 869;
  CIDROID 		= 650;

  { OIDS 900 - 999 }

  { OIDS 1000 - 1099 }
  INT2ARRAYOID	= 1005;
  INT4ARRAYOID	= 1007;
  TEXTARRAYOID	= 1009;
  INT8ARRAYOID  = 1016;
  OIDARRAYOID	  = 1028;
  FLOAT4ARRAYOID 	= 1021;
  ACLITEMOID		= 1033;
  CSTRINGARRAYOID	= 1263;
  BPCHAROID		  = 1042;
  VARCHAROID		= 1043;
  DATEOID			  = 1082;
  TIMEOID			  = 1083;

  { OIDS 1100 - 1199 }
  TIMESTAMPOID	= 1114;
  TIMESTAMPTZOID	= 1184;
  INTERVALOID		= 1186;

  { OIDS 1200 - 1299 }
  TIMETZOID		  = 1266;

  { OIDS 1500 - 1599 }
  BITOID	 		  = 1560;
  VARBITOID		  = 1562;

  { OIDS 1600 - 1699 }

  { OIDS 1700 - 1799 }
  NUMERICOID		= 1700;
  REFCURSOROID	= 1790;

  { OIDS 2200 - 2299 }
  REGPROCEDUREOID = 2202;
  REGOPEROID		  = 2203;
  REGOPERATOROID	= 2204;
  REGCLASSOID		  = 2205;
  REGTYPEOID		  = 2206;
  REGROLEOID		  = 4096;
  REGNAMESPACEOID	= 4089;
  REGTYPEARRAYOID = 2211;

  { uuid }
  UUIDOID 		    = 2950;

  { pg_lsn }
  LSNOID			    = 3220;

  { text search }
  TSVECTOROID		  = 3614;
  GTSVECTOROID	  = 3642;
  TSQUERYOID		  = 3615;
  REGCONFIGOID	  = 3734;
  REGDICTIONARYOID = 3769;

  { jsonb }
  JSONBOID 		    = 3802;

  { range types }
  INT4RANGEOID	  = 3904;

{
 * pseudo-types
 *
 * types with typtype='p' represent various special cases in the type system.
 *
 * These cannot be used to define table columns, but are valid as function
 * argument and result types (if supported by the function's implementation
 * language).
 *
 * Note: cstring is a borderline case; it is still considered a pseudo-type,
 * but there is now support for it in records and arrays.  Perhaps we should
 * just treat it as a regular base type?
}
  RECORDOID		    = 2249;
  RECORDARRAYOID	= 2287;
  CSTRINGOID		  = 2275;
  ANYOID			    = 2276;
  ANYARRAYOID		  = 2277;
  VOIDOID			    = 2278;
  TRIGGEROID		  = 2279;
  EVTTRIGGEROID	  = 3838;
  LANGUAGE_HANDLEROID	= 2280;
  INTERNALOID		  = 2281;
  OPAQUEOID		    = 2282;
  ANYELEMENTOID	  = 2283;
  ANYNONARRAYOID	= 2776;
  ANYENUMOID		  = 3500;
  FDW_HANDLEROID	= 3115;
  INDEX_AM_HANDLEROID = 325;
  TSM_HANDLEROID	= 3310;
  ANYRANGEOID		  = 3831;

  { macros }
  TYPTYPE_BASE		  = 'b'; { base type (ordinary scalar type) }
  TYPTYPE_COMPOSITE	= 'c'; { composite (e.g., table's rowtype) }
  TYPTYPE_DOMAIN		= 'd'; { domain over another type }
  TYPTYPE_ENUM		  = 'e'; { enumerated type }
  TYPTYPE_PSEUDO		= 'p'; { pseudo-type }
  TYPTYPE_RANGE		  = 'r'; { range type }

  TYPCATEGORY_INVALID	  = #0;	{ not an allowed category }
  TYPCATEGORY_ARRAY		  = 'A';
  TYPCATEGORY_BOOLEAN	  = 'B';
  TYPCATEGORY_COMPOSITE	= 'C';
  TYPCATEGORY_DATETIME	= 'D';
  TYPCATEGORY_ENUM		  = 'E';
  TYPCATEGORY_GEOMETRIC	= 'G';
  TYPCATEGORY_NETWORK	  = 'I';		{ think INET }
  TYPCATEGORY_NUMERIC	  = 'N';
  TYPCATEGORY_PSEUDOTYPE = 'P';
  TYPCATEGORY_RANGE		  = 'R';
  TYPCATEGORY_STRING		= 'S';
  TYPCATEGORY_TIMESPAN	= 'T';
  TYPCATEGORY_USER		  = 'U';
  TYPCATEGORY_BITSTRING	= 'V';		{ er ... "varbit"? }
  TYPCATEGORY_UNKNOWN	  = 'X';

//some error codes
  indeterminate_datatype: PAnsiChar = '42P18';
  current_transaction_is_aborted: PAnsiChar = '25P02';
  no_binary_output_function_available_for_type_void: PAnsiChar = '42883';
//https://www.postgresql.org/docs/9.1/static/datatype-datetime.html

{------------------------------------------------------------------------------------------}
  //timestamp.h
  POSTGRES_EPOCH_JDATE = 2451545; //* == date2j(2000, 1, 1) */
  POSTGRES_BASE_DATE = 36526; //2000-01-01
  JULIAN_MINYEAR = (-4713);
  JULIAN_MINMONTH = (11);
  JULIAN_MINDAY = (24);
  JULIAN_MAXYEAR = (5874898);

  SECS_PER_YEAR=(36525 * 864); //* avoid floating-point computation */
  SECS_PER_DAY = 86400;
  SECS_PER_HOUR	= 3600;
  SECS_PER_MINUTE = 60;
  MINS_PER_HOUR	= 60;

  USECS_PER_DAY: Int64 = 86400000000;
  USECS_PER_HOUR:	INT64 = 3600000000;
  USECS_PER_MINUTE: Int64 = 60000000;
  USECS_PER_SEC: Int64 = 1000000;

{------------------------------------------------------------------------------------------}

type

{ Application-visible enum types }
  TZPostgreSQLConnectStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );
  TZPostgreSQLFieldCode = (pgdiagSEVERITY,
    pgdiagSQLSTATE, pgdiagMESSAGE_PRIMARY, pgdiagMESSAGE_DETAIL,
    pgdiagMESSAGE_HINT, pgdiagSTATEMENT_POSITION, pgdiagINTERNAL_POSITION,
    pgdiagINTERNAL_QUERY, pgdiagCONTEXT
    ,pgdiagSOURCE_FILE, pgdiagSOURCE_LINE, pgdiagSOURCE_FUNCTION);
const
  PG_DIAG_SEVERITY=ord('S');
  PG_DIAG_SQLSTATE=ord('C');
  PG_DIAG_MESSAGE_PRIMARY=ord('M');
  PG_DIAG_MESSAGE_DETAIL=ord('D');
  PG_DIAG_MESSAGE_HINT=ord('H');
  PG_DIAG_STATEMENT_POSITION=ord('P');
  PG_DIAG_INTERNAL_POSITION=ord('p');
  PG_DIAG_INTERNAL_QUERY=ord('q');
  PG_DIAG_CONTEXT=ord('W');
  PG_DIAG_SOURCE_FILE=ord('F');
  PG_DIAG_SOURCE_LINE=ord('L');
  PG_DIAG_SOURCE_FUNCTION=ord('R');

  TPG_DIAG_ErrorFieldCodes: array[TZPostgreSQLFieldCode] of Integer = (PG_DIAG_SEVERITY,
    PG_DIAG_SQLSTATE, PG_DIAG_MESSAGE_PRIMARY, PG_DIAG_MESSAGE_DETAIL,
    PG_DIAG_MESSAGE_HINT, PG_DIAG_STATEMENT_POSITION, PG_DIAG_INTERNAL_POSITION,
    PG_DIAG_INTERNAL_QUERY, PG_DIAG_CONTEXT, PG_DIAG_SOURCE_FILE,
    PG_DIAG_SOURCE_LINE, PG_DIAG_SOURCE_FUNCTION);

type {$Z+}
  TZPostgreSQLExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,     { a query command that doesn't return
                            anything was executed properly by the backend }
    PGRES_TUPLES_OK,      { a query command that returns tuples
                            was executed properly by the backend,
                            PGresult contains the result tuples }
    PGRES_COPY_OUT,       { Copy Out data transfer in progress }
    PGRES_COPY_IN,        { Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,	  { an unexpected response was recv'd from the backend }
    PGRES_NONFATAL_ERROR, { notice or warning message }
    PGRES_FATAL_ERROR,    { query failed }
    PGRES_COPY_BOTH,      { Copy In/Out data transfer in progress }
    PGRES_SINGLE_TUPLE    { since 9.2 single tuple from larger resultset }
  );
  {$Z-}

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  TZPostgreSQLNotify = {packed }record //the reocord is NOT packet
    relname: PAnsiChar;   { name of relation containing data }
    be_pid:  Integer; { process id of backend }
    payload: PAnsiChar; {additional data in notify}
  end;

  PZPostgreSQLNotify = ^TZPostgreSQLNotify;

{ Structure for the conninfo parameter definitions returned by PQconndefaults }

  TZPostgreSQLConnectInfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    disPAnsiChar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PZPostgreSQLConnectInfoOption = ^TZPostgreSQLConnectInfoOption;

{ PQArgBlock -- structure for PQfn() arguments }

  TZPostgreSQLArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PZPostgreSQLArgBlock = ^TZPostgreSQLArgBlock;

  PZPostgreSQLCancel = Pointer;
  POid = ^Oid;
  Oid = Cardinal;
  {$IF not declared(Int32)}
  Int32 = Integer;
  {$IFEND}
  {$IF not declared(PInt32)}
  PInt32 = ^Int32;
  {$IFEND}

  TZPgCharactersetType = (
    csSQL_ASCII,     { SQL/ASCII }
    csEUC_JP,        { EUC for Japanese }
    csEUC_CN,        { EUC for Chinese }
    csEUC_KR,        { EUC for Korean }
    csEUC_TW,        { EUC for Taiwan }
    csEUC_JIS_2004,  { Extended UNIX Code-JP, JIS X 0213 Japanese }
    csUTF8,          { Unicode UTF-8 }
    csMULE_INTERNAL, { Mule internal code }
    csLATIN1,        { ISO-8859 Latin 1 }
    csLATIN2,        { ISO-8859 Latin 2 }
    csLATIN3,        { ISO-8859 Latin 3 }
    csLATIN4,        { ISO-8859 Latin 4 }
    csLATIN5,        { ISO-8859 Latin 5 }
    csLATIN6,        { ISO-8859 Latin 6 }
    csLATIN7,        { ISO-8859 Latin 7 }
    csLATIN8,        { ISO-8859 Latin 8 }
    csLATIN9,        { ISO-8859 Latin 9 }
    csLATIN10,       { ISO-8859 Latin 10 }
    csWIN1256,       { Arabic Windows }
    csWIN1258,       { Vietnamese Windows }
    csWIN866,        { Alternativny Variant (MS-DOS CP866) }
    csWIN874,        { Thai Windows }
    csKOI8,          { KOI8-R(U) Cyrillic }
    csKOI8R,         { KOI8-R Cyrillic (Russian) }
    csWIN1251,       { windows-1251 }
    csWIN1252,       { Windows CP1252 Western European }
    csISO_8859_5,    { ISO-8859-5 }
    csISO_8859_6,    { ISO-8859-6 }
    csISO_8859_7,    { ISO-8859-7 }
    csISO_8859_8,    { ISO-8859-8 }
    csWIN1250,       { Windows CP1250 Central European }
    csWIN1253,       { Windows CP1253 Greek }
    csWIN1254,       { Windows CP1254 Turkish }
    csWIN1255,       { Windows CP1255 Hebrew }
    csWIN1257,       { Windows CP1257 Baltic }
    csKOI8U,         { KOI8-R Cyrillic (Ukrainian) }
    csSJIS,          { Shift JIS Japanese }
    csBIG5,          { Big Five Traditional Chinese }
    csGBK,           { Extended National Standard Simplified Chinese }
    csUHC,           { Unified Hangul Code Korean }
    csGB18030,       { National Standard Chinese }
    csJOHAB,         { JOHAB Korean (Hangul) }
    csSHIFT_JIS_2004,{ Shift JIS, JIS X 0213 Japanese }
    csUNICODE_PODBC, { UNICODE ( < Ver8.1). Can't call it UNICODE as that's already used }
    csTCVN,          { TCVN ( < Ver8.1) }
    csALT,           { ALT ( < Var8.1) }
    csWIN,           { WIN ( < Ver8.1) }
    csOTHER
  );

//pgtypes_numeric.h
const
  NUMERIC_POS   = $0000;
  NUMERIC_NEG   = $4000;
  NUMERIC_NAN   = $C000;
  NUMERIC_NULL  = $F000;
  NUMERIC_MAX_PRECISION = 1000;
  NUMERIC_MAX_DISPLAY_SCALE = NUMERIC_MAX_PRECISION;
  NUMERIC_MIN_DISPLAY_SCALE = 0;
  NUMERIC_MIN_SIG_DIGITS = 16;

  NBASE = 10000;
  DECSIZE = 30;

type
  //https://www.postgresql.org/message-id/16572.1091489720%40sss.pgh.pa.us
  PPGNumeric_External = ^TPGNumeric_External;
  TPGNumeric_External = packed record
    NBASEDigits:  Word; //count of NBASE digits
    weight:       SmallInt; //* weight of first digit */
    sign:         Word; //* NUMERIC_POS, NUMERIC_NEG, or NUMERIC_NAN */
    dscale:       Word; //* display scale */
    digits:       array[0..NUMERIC_MAX_PRECISION-1] of Word{SmallInt}; //no fix size -> aligned against scale or digits
  end;

  PInetRec = ^TInetRec;
  TInetRec = packed record
    family : byte;
    bits   : byte;
    is_cidr: byte;
    nb     : byte;
    ipaddr : array[0..15] of byte;
  end;

  PPGInterval = ^TPGInterval;
  TPGInterval = packed record
     time  : int64;
     day   : longint;
     month : longint;
   end;
{ ****************** Plain API Types definition ***************** }

type
{ String descriptions of the ExecStatusTypes }
  pgresStatus = array[$00..$ff] of PAnsiChar;

{ TPGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PPGconn = ^TPGconn;
  TPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PPGresult = ^TPGresult;
  TPGresult = Pointer;
  PGCancel = Pointer;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  TPQnoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

  TPQnoticeReceiver = procedure(arg: Pointer; res: TPGResult); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PAnsiCharArray = array[00..$ff] of PAnsiChar;

  PQprintOpt = packed record
    header:    Byte;	   { print output field headings and row count }
    align:     Byte;	   { fill align the fields }
    standard:  Byte;	   { old brain dead format }
    html3:     Byte;	   { output html tables }
    expanded:  Byte;	   { expand tables }
    pager:     Byte;	   { use pager for output if needed }
    fieldSep:  PAnsiChar;	   { field separator }
    tableOpt:  PAnsiChar;      { insert to HTML <table ...> }
    caption:   PAnsiChar;	   { HTML <caption> }
    fieldName: PAnsiCharArray;  { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
    keyword:  PAnsiChar; { The keyword of the option }
    envvar:   PAnsiChar; { Fallback environment variable name }
    compiled: PAnsiChar; { Fallback compiled in default value  }
    val:      PAnsiChar; { Options value}
    lab:      PAnsiChar; { Label for field in connect dialog }
    disPAnsiChar: PAnsiChar; { Character to display for this field
                               in a connect dialog. Values are:
                               ""  Display entered value as is
                               "*" Password field - hide value
                               "D" Debug options - don't create a field by default }
    dispsize: Integer; { Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger); { can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;

{Prepared statement types}
  TPQparamTypes = array of Oid;
  TPQparamValues = array of Pointer;
  TPQparamLengths = array of Integer;
  TPQparamFormats = array of Integer;

//array.h
  PArrayType = ^TArrayType;
  TArrayType = packed record
    //https://reviewable.io/reviews/cockroachdb/cockroach/13636#-KdAI6xOLxF4N43uWbYp
    //https://stackoverflow.com/questions/4016412/postgresqls-libpq-encoding-for-binary-transport-of-array-data
    //comment of user: "When using libpq, omit the vl_len_ portion"
    //vl_len_:    int32;   //* varlena header (do not touch directly!) */
    ndim:       integer; //* # of dimensions */
    flags:      int32;//* offset to data, or 0 if no bitmap */
    elemtype:   Oid;    //* element type OID */
  end;
//    dimensions: TIntegerArray; //length of each array axis (C array of int)
//    lower_bnds: TIntegerArray; //lower boundary of each dimension (C array of int)  this is ignored on param send
//    null_bitmap: TIntegerArray //bitmap showing locations of nulls (OPTIONAL)
//    actual_data: whatever is the stored data
type
  {** Represents a generic interface to PostgreSQL native API. }
  IZPostgreSQLPlainDriver = interface (IZPlainDriver)
    ['{03CD6345-2D7A-4FE2-B03D-3C5656789FEB}']
  end;

  {** Implements a base driver for PostgreSQL}
  TZPostgreSQLPlainDriver = class(TZAbstractPlainDriver, IZPostgreSQLPlainDriver)
  protected
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  public
  { ************** Plain API Function types definition ************* }
  { ===	in fe-connect.c === }
    PQconnectdb     : function(ConnInfo: PAnsiChar): TPGconn; cdecl; // FirmOS 8.1 OK
    PQsetdbLogin    : function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): TPGconn; cdecl; // FirmOS 8.1 OK
    PQconndefaults  : function: PPQconninfoOption; cdecl;
    PQfinish        : procedure(conn: TPGconn); cdecl;
    PQreset         : procedure(conn: TPGconn); cdecl;
  //15022006 FirmOS: omitting PQresetStart
  //15022006 FirmOS: omitting PQresetPoll
    PQrequestCancel : function(conn: TPGconn): Integer; cdecl;
    PQdb            : function(conn: TPGconn): PAnsiChar; cdecl;
    PQuser          : function(conn: TPGconn): PAnsiChar; cdecl;
    PQpass          : function(conn: TPGconn): PAnsiChar; cdecl;
    PQhost          : function(conn: TPGconn): PAnsiChar; cdecl;
    PQport          : function(conn: TPGconn): PAnsiChar; cdecl;
    PQtty           : function(conn: TPGconn): PAnsiChar; cdecl;
    PQoptions       : function(conn: TPGconn): PAnsiChar; cdecl;
    PQstatus        : function(conn: TPGconn): TZPostgreSQLConnectStatusType; cdecl;
  //TBD  PGTransactionStatusType PQtransactionStatus(const TPGconn *conn);
  //15022006 FirmOS: omitting const char *PQparameterStatus(const TPGconn *conn, const char *paramName);
  //15022006 FirmOS: omitting  PQprotocolVersion
    PQserverVersion : function(conn: TPGconn): Integer; cdecl;
    PQerrorMessage  : function(conn: TPGconn): PAnsiChar; cdecl;
    PQsocket        : function(conn: TPGconn): Integer; cdecl;
    PQbackendPID    : function(conn: TPGconn): Integer; cdecl;
  //15022006 FirmOS: omitting  SSL *PQgetssl(const TPGconn *conn);
    PQtrace         : procedure(conn: TPGconn; DebugPort: Pointer); cdecl;
    PQuntrace       : procedure(conn: TPGconn); cdecl;
    PQsetNoticeProcessor : function(conn: TPGconn; Proc: TPQnoticeProcessor; Arg: Pointer): TPQnoticeProcessor; cdecl;
    PQsetNoticeReceivcer : function(conn: TPGconn; Proc: TPQnoticeReceiver; Arg: Pointer): TPQnoticeReceiver; cdecl;

    PQclientEncoding : function(conn: TPGconn): Integer; cdecl; //EgonHugeist
  { === in fe-exec.c === }
  //* Simple synchronous query */
    PQexec          : function(conn: TPGconn; Query: PAnsiChar): TPGresult; cdecl;
    PQexecParams    : function(conn: TPGconn; command: PAnsichar;
          nParams: Integer; paramTypes: POid; paramValues: PPointer;
          paramLengths: PInteger; paramFormats: PInteger;
          resultFormat: Integer): TPGresult; cdecl;
    PQprepare        : function(conn: TPGconn; stmtName: PAnsichar;
          query: PAnsiChar; nParams: Integer; paramTypes: PInteger): TPGresult; cdecl;
    PQexecPrepared   : function(conn: TPGconn; stmtName: PAnsichar;
          nParams: Integer; paramValues: PPointer; paramLengths: PInteger;
          paramFormats: PInteger; resultFormat: Integer): TPGresult; cdecl;
  //* Interface for multiple-result or asynchronous queries */
    PQsendQuery      : function(conn: TPGconn; query: PAnsiChar): Integer; cdecl;
    PQsendQueryParams: function(conn: TPGconn; command: PAnsichar;
          nParams: Integer; paramTypes: PInteger; paramValues: PPointer;
          paramLengths: PInteger; paramFormats: PInteger;
          resultFormat: Integer): Integer; cdecl;
    PQsendPrepare    : function(conn: TPGconn; stmtName: PAnsichar;
          query: PAnsiChar; nParams: Integer; paramTypes: POid): Integer; cdecl;
    PQsendQueryPrepared : function(conn: TPGconn; stmtName: PAnsichar;
           nParams: Integer; paramValues: PPointer; paramLengths: PInteger;
           paramFormats: PInteger; resultFormat: Integer): Integer; cdecl;
    PQgetResult     : function(conn: TPGconn): TPGresult;  cdecl;
    PQsetSingleRowMode : function(conn: TPGconn): Integer; cdecl;
  //* Describe prepared statements and portals */
    PQdescribePrepared : function(conn: TPGconn; stmt: PAnsiChar): TPGresult; cdecl;
    PQnparams: function(res: TPGresult): Integer; cdecl;
    PQparamtype: function(res: TPGresult; param_number: Integer): OID; cdecl;
    PQdescribePortal : function(conn: TPGconn; portal: PAnsiChar): TPGresult; cdecl;
    PQsendDescribePrepared : function(conn: TPGconn; stmt: PAnsiChar): Integer; cdecl;
    PQsendDescribePortal : function(conn: TPGconn; portal: PAnsiChar): Integer; cdecl;

    PQsetnonblocking : function(conn: TPGconn; arg: Integer): Integer; cdecl;

    PQnotifies      : function(conn: TPGconn): PZPostgreSQLNotify; cdecl;
    PQisBusy        : function(conn: TPGconn): Integer; cdecl;
    PQconsumeInput  : function(conn: TPGconn): Integer; cdecl;
    PQgetCancel     : function(conn: TPGconn): PGcancel; cdecl;
    PQfreeCancel    : procedure(Canc: PGcancel); cdecl;
    PQcancel        : function(Canc: PGcancel; Buffer: PAnsiChar; BufSize: Integer): Integer; cdecl;
    PQgetline       : function(conn: TPGconn; Str: PAnsiChar; length: Integer): Integer; cdecl;
    PQputline       : function(conn: TPGconn; Str: PAnsiChar): Integer; cdecl;
    PQgetlineAsync  : function(conn: TPGconn; Buffer: PAnsiChar; BufSize: Integer): Integer; cdecl;
    PQputnbytes     : function(conn: TPGconn; Buffer: PAnsiChar; NBytes: Integer): Integer; cdecl;
    PQendcopy       : function(conn: TPGconn): Integer; cdecl;
    PQfn            : function(conn: TPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): TPGresult; cdecl;
    PQresultStatus  : function(res: TPGresult): TZPostgreSQLExecStatusType; cdecl;
    PQresultErrorMessage : function(res: TPGresult): PAnsiChar; cdecl;
    PQresultErrorField:function(res: TPGresult; fieldcode:integer):PAnsiChar;cdecl; // postgresql 8
    PQntuples       : function(res: TPGresult): Integer; cdecl;
    PQnfields       : function(res: TPGresult): Integer; cdecl;
    PQbinaryTuples  : function(res: TPGresult): Integer; cdecl;
    PQfname         : function(res: TPGresult; field_num: Integer): PAnsiChar; cdecl;
    PQfnumber       : function(res: TPGresult; field_name: PAnsiChar): Integer; cdecl;
    PQftable        : function(res: TPGresult; field_num: Integer): Oid; cdecl;
    PQftablecol     : function(res: TPGresult; field_num: Integer): Integer; cdecl;
    PQftype         : function(res: TPGresult; field_num: Integer): Oid; cdecl;
    PQfsize         : function(res: TPGresult; field_num: Integer): Integer; cdecl;
    PQfmod          : function(res: TPGresult; field_num: Integer): Integer; cdecl;
    PQcmdStatus     : function(res: TPGresult): PAnsiChar; cdecl;
    PQoidValue      : function(res: TPGresult): Oid; cdecl;
    PQoidStatus     : function(res: TPGresult): PAnsiChar; cdecl;
    PQcmdTuples     : function(res: TPGresult): PAnsiChar; cdecl;
    PQgetvalue      : function(res: TPGresult; tup_num, field_num: Integer): PAnsiChar; cdecl;
    PQgetlength     : function(res: TPGresult; tup_num, field_num: Integer): Integer; cdecl;
    PQgetisnull     : function(res: TPGresult; tup_num, field_num: Integer): Integer; cdecl;
    PQclear         : procedure(res: TPGresult); cdecl;
    PQmakeEmptyPGresult  : function(conn: TPGconn; status: TZPostgreSQLExecStatusType): TPGresult; cdecl;
    PQescapeStringConn : function(conn: TPGconn; ToChar: PAnsiChar;
      const FromChar: PAnsiChar; length: NativeUInt; error: PInteger): NativeUInt;cdecl; //7.3
    PQescapeLiteral    : function(conn: TPGconn; str: PAnsiChar; len: NativeUInt): PAnsiChar;cdecl;
    PQescapeIdentifier : function(conn: TPGconn; str: PAnsiChar; len: NativeUInt): PAnsiChar;cdecl; //7.3
    PQescapeByteaConn  : function(conn: TPGconn; from: PAnsiChar; from_length: longword; to_lenght: PLongword): PAnsiChar;cdecl;
    PQunescapeBytea    : function(const from:PAnsiChar;to_lenght:PLongword):PAnsiChar;cdecl;
    PQFreemem          : procedure(ptr:Pointer);cdecl;
    PQisthreadsafe     : function: Integer; cdecl;
    //* These forms are deprecated! */
    PQescapeString     : function(ToChar: PAnsiChar; const FormChar: PAnsiChar; length: NativeUInt): NativeUInt;cdecl; //7.2
    PQescapeBytea      : function(const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl; //7.2

    { === in fe-lobj.c === }
    lo_open         : function(conn: TPGconn; lobjId: Oid; mode: Integer): Integer; cdecl;
    lo_close        : function(conn: TPGconn; fd: Integer): Integer; cdecl;
    lo_read         : function(conn: TPGconn; fd: Integer; buf: PAnsiChar; len: NativeUInt): Integer; cdecl;
    lo_write        : function(conn: TPGconn; fd: Integer; buf: PAnsiChar; len: NativeUInt): Integer; cdecl;
    lo_lseek        : function(conn: TPGconn; fd, offset, whence: Integer): Integer; cdecl;
    lo_lseek64      : function(conn: TPGconn; fd: integer; offset: Int64; whence: Integer): Int64; cdecl;
    lo_creat        : function(conn: TPGconn; mode: Integer): Oid; cdecl;
    lo_tell         : function(conn: TPGconn; fd: Integer): Integer; cdecl;
    lo_unlink       : function(conn: TPGconn; lobjId: Oid): Integer; cdecl;
    lo_import       : function(conn: TPGconn; filename: PAnsiChar): Oid; cdecl;
    lo_export       : function(conn: TPGconn; lobjId: Oid; filename: PAnsiChar): Integer; cdecl;
    lo_truncate     : function(conn: TPGconn; fd: Integer; len: NativeInt): Integer; cdecl;
    lo_truncate64   : function(conn: TPGconn; fd: Integer; len: Int64): Integer; cdecl;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}

implementation

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses ZPlainLoader, ZEncoding;

{ TZPostgreSQLPlainDriver }

function TZPostgreSQLPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL';
end;

function TZPostgreSQLPlainDriver.GetProtocol: string;
begin
  Result := 'postgresql';
end;

function TZPostgreSQLPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF8';
end;

procedure TZPostgreSQLPlainDriver.LoadCodePages;
begin
  { all versions }
  { MultiByte }
  AddCodePage('EUC_JP', Ord(csEUC_JP), ceAnsi, zCP_EUC_JP, '', 3); { EUC_JP 	Japanese EUC }
  AddCodePage('EUC_CN', Ord(csEUC_CN), ceAnsi, zCP_EUC_CN, '', 3); {EUC_CN 	Chinese EUC}
  AddCodePage('EUC_KR', Ord(csEUC_KR), ceAnsi, zCP_euc_kr, '', 3); {Extended UNIX Code-KR 	Korean}
  AddCodePage('JOHAB', Ord(csJOHAB), ceAnsi, ZCP_JOHAB, '', 3); {JOHAB 	Korean (Hangul)}
  AddCodePage('EUC_TW', Ord(csEUC_TW), ceAnsi, $ffff, '', 3); {Extended UNIX Code-TW 	Traditional Chinese, Taiwanese}
  AddCodePage('UNICODE', Ord(csUNICODE_PODBC), ceUTF8, zCP_UTF8, '', 4); {UNICODE 	Unicode (UTF-8)}
  AddCodePage('MULE_INTERNAL', Ord(csMULE_INTERNAL), ceAnsi, $ffff, '', 4); { Mule internal code 	Multilingual Emacs }
  {SingleByte}
  AddCodePage('SQL_ASCII', Ord(csSQL_ASCII), ceAnsi, zCP_us_ascii); {unspecified (see text) 	any}
  AddCodePage('LATIN1', Ord(csLATIN1), ceAnsi, zCP_WIN1252); { ISO 8859-1, ECMA 94 	Western European }
  AddCodePage('LATIN2', Ord(csLATIN2), ceAnsi, zCP_L2_ISO_8859_2);  { 	ISO 8859-2, ECMA 94 	Central European }
  AddCodePage('LATIN3', Ord(csLATIN3), ceAnsi, zCP_L3_ISO_8859_3);  { ISO 8859-3, ECMA 94 	South European }
  AddCodePage('LATIN4', Ord(csLATIN4), ceAnsi, zCP_L4_ISO_8859_4);  { ISO 8859-4, ECMA 94 	North European }
  AddCodePage('LATIN5', Ord(csLATIN5), ceAnsi, zCP_L5_ISO_8859_9);  { ISO 8859-9, ECMA 128 	Turkish }
  AddCodePage('LATIN6', Ord(csLATIN6), ceAnsi, zCP_L6_ISO_8859_10);  { ISO 8859-10, ECMA 144 	Nordic }
  AddCodePage('LATIN7', Ord(csLATIN7), ceAnsi, zCP_L7_ISO_8859_13);  { ISO 8859-13 	Baltic }
  AddCodePage('LATIN8', Ord(csLATIN8), ceAnsi, zCP_L8_ISO_8859_14);  { ISO 8859-14 	Celtic }
  AddCodePage('LATIN9', Ord(csLATIN9), ceAnsi, zCP_L9_ISO_8859_15);  { ISO 8859-15 	LATIN1 with Euro and accents }
  AddCodePage('LATIN10', Ord(csLATIN10), ceAnsi, zCP_L10_ISO_8859_16);  { ISO 8859-16, ASRO SR 14111 	Romanian }
  AddCodePage('ISO_8859_5', Ord(csISO_8859_5), ceAnsi, zCP_L5_ISO_8859_5); { ISO 8859-5, ECMA 113 	Latin/Cyrillic}
  AddCodePage('ISO_8859_6', Ord(csISO_8859_6), ceAnsi, zCP_L6_ISO_8859_6); { ISO 8859-6, ECMA 114 	Latin/Arabic }
  AddCodePage('ISO_8859_7', Ord(csISO_8859_7), ceAnsi, zCP_L7_ISO_8859_7); { ISO 8859-7, ECMA 118 	Latin/Greek }
  AddCodePage('ISO_8859_8', Ord(csISO_8859_8), ceAnsi, zCP_L8_ISO_8859_8);  { ISO 8859-8, ECMA 121 	Latin/Hebrew }
  AddCodePage('KOI8', Ord(csKOI8), ceAnsi, zCP_KOI8R);  { KOI8-R(U) 	Cyrillic }
  AddCodePage('WIN', Ord(csWIN), ceAnsi, zCP_WIN1251); { Windows CP1251 }
  AddCodePage('ALT', Ord(csALT), ceAnsi, zCP_DOS866); { Windows CP866 }
  AddCodePage('WIN1256', Ord(csWIN1256), ceAnsi, zCP_WIN1256);  { Windows CP1256 	Arabic }
  AddCodePage('TCVN', Ord(csTCVN), ceAnsi, zCP_WIN1258); { TCVN-5712/Windows CP1258 (Vietnamese) }
  AddCodePage('WIN874', Ord(csWIN874), ceAnsi, zCP_WIN874); { Windows CP874 (Thai) }
  { Version 8.1 }
  {MultiByte}
  ResetCodePage(Ord(csUNICODE_PODBC), 'UTF8', Ord(csUTF8), ceUTF8, zCP_UTF8, '', 4); { Unicode, 8-bit 	all }
  AddCodePage('BIG5', Ord(csBIG5), ceAnsi, zCP_Big5, '', 2); { Big Five 	Traditional Chinese }
  AddCodePage('GB18030', Ord(csGB18030), ceAnsi, zCP_GB18030, '', 2); { National Standard 	Chinese }
  AddCodePage('GBK', Ord(csGBK), ceAnsi, zCP_GB2312, '', 2); { Extended National Standard 	Simplified Chinese }
  AddCodePage('SJIS', Ord(csSJIS), ceAnsi, zCP_SHIFTJS, '', 2); { Shift JIS 	Japanese }
  AddCodePage('UHC', Ord(csUHC), ceAnsi, zCP_EUCKR, '', 2); { Unified Hangul Code Korean }
  {SingleByte}
  ResetCodePage(Ord(csALT), 'WIN866', Ord(csWIN866), ceAnsi, zCP_DOS866); { Windows CP866 	Cyrillic } //No longer in use
  AddCodePage('WIN874', Ord(csWIN874), ceAnsi, zCP_WIN874); { Windows CP874 	Thai }
  AddCodePage('WIN1250', Ord(csWIN1250), ceAnsi, zCP_WIN1250); { Windows CP1250 	Central European }
  ResetCodePage(Ord(csWIN), 'WIN1251', Ord(csWIN1251), ceAnsi, zCP_WIN1251); { Windows CP1251 	Cyrillic } //No longer in use
  AddCodePage('WIN1252', Ord(csWIN1252), ceAnsi, zCP_WIN1252); { Windows CP1252 	Western European }
  ResetCodePage(Ord(csTCVN), 'WIN1258', Ord(csWIN1258),ceAnsi, zCP_WIN1258); { Windows CP1258 	Vietnamese } //No longer in use

  { Version 8.3 }
  {MultiByte}
  AddCodePage('EUC_JIS_2004', Ord(csEUC_JIS_2004), ceAnsi, $ffff, '', 3); { Extended UNIX Code-JP, JIS X 0213 	Japanese }
  AddCodePage('SHIFT_JIS_2004', Ord(csSHIFT_JIS_2004), ceAnsi, zCP_SHIFTJS, '', 3); { Shift JIS, JIS X 0213 	Japanese }
  {SingleChar}
  AddCodePage('WIN1253', Ord(csWIN1253), ceAnsi, zCP_WIN1253); { Windows CP1253  Greek }
  AddCodePage('WIN1254', Ord(csWIN1254), ceAnsi, zCP_WIN1254); { Windows CP1254 	Turkish }
  AddCodePage('WIN1255', Ord(csWIN1255), ceAnsi, zCP_WIN1255); { Windows CP1255 	Hebrew }
  AddCodePage('WIN1257', Ord(csWIN1257), ceAnsi, zCP_WIN1257); { Windows CP1257 	Baltic }

  { Version 8.4 }
  {SingleChar}
  AddCodePage('KOI8U', Ord(csKOI8U), ceAnsi, zCP_KOI8U); { 	KOI8-U 	Cyrillic (Ukrainian) }
  { Version 9+}
  //ResetCodePage(Ord(csKOI8), 'KOI8R', Ord(csKOI8R)); { KOI8-R 	Cyrillic (Russian) } //No longer in use
end;

procedure TZPostgreSQLPlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  { ===	in fe-connect.c === }
    @PQconnectdb    := GetAddress('PQconnectdb');
    @PQsetdbLogin   := GetAddress('PQsetdbLogin');
    @PQconndefaults := GetAddress('PQconndefaults');
    @PQfinish       := GetAddress('PQfinish');
    @PQreset        := GetAddress('PQreset');
    @PQrequestCancel := GetAddress('PQrequestCancel');
    @PQdb           := GetAddress('PQdb');
    @PQuser         := GetAddress('PQuser');
    @PQpass         := GetAddress('PQpass');
    @PQhost         := GetAddress('PQhost');
    @PQport         := GetAddress('PQport');
    @PQtty          := GetAddress('PQtty');
    @PQoptions      := GetAddress('PQoptions');
    @PQstatus       := GetAddress('PQstatus');
    @PQserverVersion:= GetAddress('PQserverVersion');
    @PQerrorMessage := GetAddress('PQerrorMessage');
    @PQsocket       := GetAddress('PQsocket');
    @PQbackendPID   := GetAddress('PQbackendPID');
    @PQtrace        := GetAddress('PQtrace');
    @PQuntrace      := GetAddress('PQuntrace');
    @PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');
    @PQsetNoticeReceivcer := GetAddress('PQsetNoticeReceivcer');
    @PQclientEncoding := GetAddress('PQclientEncoding');
  { === in fe-exec.c === }
    @PQexec         := GetAddress('PQexec');
    @PQexecParams   := GetAddress('PQexecParams');
    @PQprepare      := GetAddress('PQprepare');
    @PQexecPrepared := GetAddress('PQexecPrepared');
    @PQsendQuery    := GetAddress('PQsendQuery');
    @PQsendQueryParams:= GetAddress('PQsendQueryParams');
    @PQsendPrepare  := GetAddress('PQsendPrepare');
    @PQsendQueryPrepared := GetAddress('PQsendQueryPrepared');
    @PQgetResult    := GetAddress('PQgetResult');
    @PQdescribePrepared := GetAddress('PQdescribePrepared');
    @PQnparams      := GetAddress('PQnparams');
    @PQparamtype    := GetAddress('PQparamtype');
    @PQsendDescribePrepared := GetAddress('PQsendDescribePrepared');
    @PQsetSingleRowMode := GetAddress('PQsetSingleRowMode'); //9+ http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html
    @PQsetnonblocking := GetAddress('PQsetnonblocking');

    @PQnotifies     := GetAddress('PQnotifies');
    @PQisBusy       := GetAddress('PQisBusy');
    @PQconsumeInput := GetAddress('PQconsumeInput');
    @PQgetline      := GetAddress('PQgetline');
    @PQputline      := GetAddress('PQputline');
    @PQgetlineAsync := GetAddress('PQgetlineAsync');
    @PQputnbytes    := GetAddress('PQputnbytes');
    @PQendcopy      := GetAddress('PQendcopy');
    @PQfn           := GetAddress('PQfn');
    @PQresultStatus := GetAddress('PQresultStatus');
    @PQresultErrorMessage := GetAddress('PQresultErrorMessage');
    @PQntuples      := GetAddress('PQntuples');
    @PQnfields      := GetAddress('PQnfields');
    @PQbinaryTuples := GetAddress('PQbinaryTuples');
    @PQfname        := GetAddress('PQfname');
    @PQfnumber      := GetAddress('PQfnumber');
    @PQftable       := GetAddress('PQftable');
    @PQftablecol    := GetAddress('PQftablecol');
    @PQftype        := GetAddress('PQftype');
    @PQfsize        := GetAddress('PQfsize');
    @PQfmod         := GetAddress('PQfmod');
    @PQcmdStatus    := GetAddress('PQcmdStatus');
    @PQoidValue     := GetAddress('PQoidValue');
    @PQoidStatus    := GetAddress('PQoidStatus');
    @PQcmdTuples    := GetAddress('PQcmdTuples');
    @PQgetvalue     := GetAddress('PQgetvalue');
    @PQgetlength    := GetAddress('PQgetlength');
    @PQgetisnull    := GetAddress('PQgetisnull');
    @PQclear        := GetAddress('PQclear');
    @PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');

  { === in fe-lobj.c === }
    @lo_open        := GetAddress('lo_open');
    @lo_close       := GetAddress('lo_close');
    @lo_read        := GetAddress('lo_read');
    @lo_write       := GetAddress('lo_write');
    @lo_lseek       := GetAddress('lo_lseek');
    @lo_lseek64     := GetAddress('lo_lseek64');
    @lo_creat       := GetAddress('lo_creat');
    @lo_tell        := GetAddress('lo_tell');
    @lo_unlink      := GetAddress('lo_unlink');
    @lo_import      := GetAddress('lo_import');
    @lo_export      := GetAddress('lo_export');
    @lo_truncate    := GetAddress('lo_truncate');
    @lo_truncate64  := GetAddress('lo_truncate64');

    @PQescapeStringConn  := GetAddress('PQescapeStringConn'); //since 7.3
    @PQescapeByteaConn   := GetAddress('PQescapeByteaConn'); // postgresql since 7.3
    @PQFreemem           := GetAddress('PQfreemem'); // since postgresql 7.4
    @PQescapeString      := GetAddress('PQescapeString'); // since postgresql 7.4
    @PQescapeBytea       := GetAddress('PQescapeBytea'); // since postgresql 7.4
    @PQunescapeBytea     := GetAddress('PQunescapeBytea'); // since postgresql 8.3
    @PQescapeLiteral     := GetAddress('PQescapeLiteral'); // since postgresql 9.0
    @PQescapeIdentifier  := GetAddress('PQescapeIdentifier'); // since postgresql 9.0

    @PQisthreadsafe      := GetAddress('PQisthreadsafe');

    @PQresultErrorField  := GetAddress('PQresultErrorField');
    @PQgetCancel         := GetAddress('PQgetCancel');
    @PQfreeCancel        := GetAddress('PQfreeCancel');
    @PQcancel            := GetAddress('PQcancel');
  end;
end;

function TZPostgreSQLPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZPostgreSQLPlainDriver.Create;
end;

constructor TZPostgreSQLPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL7_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL8_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    FLoader.AddLocation(LINUX_DLL82_LOCATION);
    FLoader.AddLocation(LINUX_DLL8_LOCATION);
  {$ENDIF}
  LoadCodePages;
end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}


end.


