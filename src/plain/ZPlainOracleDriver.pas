{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for Oracle             }
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

unit ZPlainOracleDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ORACLE}
{$J+}

uses
  ZPlainLoader, ZCompatibility, ZPlainDriver;

{***************** Plain API types definition ****************}

const
  WINDOWS_DLL_LOCATION = 'oci.dll';
//  WINDOWS_DLL_LOCATION = 'ora803.dll';
  LINUX_DLL_LOCATION = 'libclntsh'+SharedSuffix;
//  LINUX_DLL_LOCATION = 'libwtc8.so';

type
  { Generic Oracle Types }
  sword   = Integer;
  psword  = ^sword;
  eword   = Integer;
  uword   = Cardinal;
  sb4     = Integer;
  ub4     = Cardinal;
  sb2     = SmallInt;
  ub2     = Word;
  sb1     = ShortInt;
  ub1     = Byte;
  dvoid   = Pointer;
  text    = PAnsiChar;
  size_T  = NativeUInt;
  oraub8  = Uint64;
  Poraub8 = ^oraub8;

  pub1 = ^ub1;
  psb1 = ^sb1;
  pub2 = ^ub2;
  psb2 = ^sb2;
  pub4 = ^ub4;
  psb4 = ^sb4;

  { Handle Types }
  POCIHandle = Pointer;
  PPOCIHandle = ^Pointer;
  PPOCIEnv = ^POCIEnv;
  POCIEnv = POCIHandle;
  POCIServer = POCIHandle;
  POCIError = POCIHandle;
  POCISvcCtx = POCIHandle;
  POCIStmt = POCIHandle;
  POCIDefine = POCIHandle;
  POCISession = POCIHandle;
  POCIBind = POCIHandle;
  POCIDescribe = POCIHandle;
  POCITrans = POCIHandle;
  POCITable = POCIHandle;
  POCIIter = POCIHandle;
  POCIType = Pointer;
  PPOCIType = ^POCIType;
  POCIInd = Pointer;
  PPOCIInd = ^POCIInd;

  POCITypeCode  = POCIHandle;
  POCITypeElem  = POCIHandle;
  POCITypeIter  = POCIHandle;
  PPOCITypeIter = ^POCITypeIter;
  POCITypeMethod  = POCIHandle;

  OCITypeGetOpt = (
    OCI_TYPEGET_HEADER,// load only the header portion of the TDO when getting type
    OCI_TYPEGET_ALL  //load all attribute and method descriptors as well
    );

  { Descriptor Types }
  POCIDescriptor = Pointer;
  PPOCIDescriptor = ^POCIDescriptor;
  POCISnapshot = POCIDescriptor;          //OCI snapshot descriptor
  PPOCILobLocator = ^POCILobLocator;
  POCILobLocator = POCIDescriptor;        //OCI Lob Locator descriptor
  POCIParam = POCIDescriptor;             //OCI Parameter descriptor
  POCIRowid = POCIDescriptor;             //OCI ROWID descriptor
  POCIComplexObjectComp = POCIDescriptor;
  POCIAQEnqOptions = POCIDescriptor;
  POCIAQDeqOptions = POCIDescriptor;
  POCIAQMsgProperties = POCIDescriptor;
  POCIAQAgent = POCIDescriptor;
  POCIDateTime = POCIDescriptor;          //OCI DateTime descriptor

const
  OCI_NUMBER_SIZE = 22;
type
  TOCINumberPart = array[0..OCI_NUMBER_SIZE-1] of ub1;
  PPOCINumber = ^POCINumber;
  POCINumber = ^TOCINumber;
  TOCINumber = TOCINumberPart;

  PPOCIString = ^POCIString;
  POCIString = Pointer;

  POCIInterval = POCIDescriptor;          //OCI Interval descriptor
  POCIResult = POCIDescriptor;            //OCI Result Set Descriptor
  PPOCITypeElem = PPOCIDescriptor;
  PPOCITypeMethod = PPOCIDescriptor;
  OCIDuration = ub4;       //enum!
  POCIDuration = ^OCIDuration;
  OCITypeEncap = ub2;      //enum!
  OCITypeMethodFlag = ub2; //enum!
  OCITypeParamMode = sb4;  //enum!
  OCIObjectPropId = ub1;
  OCIRefreshOpt = ub2;     //enum!

  OCITypeCode = ub2; //enum!
  OCIPinOpt = ub2;
  OCILockOpt = pub2;

  POCIComplexObject = Pointer;
  PPOCIComplexObject = ^POCIComplexObject;
const
  OCI_DURATION_INVALID = $FFFF;      { Invalid duration }
  OCI_DURATION_BEGIN = 10;           { beginning sequence of duration }
  OCI_DURATION_NULL = (OCI_DURATION_BEGIN-1); { null duration }
  OCI_DURATION_DEFAULT = (OCI_DURATION_BEGIN-2); { default }
  OCI_DURATION_USER_CALLBACK = (OCI_DURATION_BEGIN-3);
  OCI_DURATION_NEXT = (OCI_DURATION_BEGIN-4); { next special duration }
  OCI_DURATION_SESSION = (OCI_DURATION_BEGIN); { the end of user session }
  OCI_DURATION_TRANS = (OCI_DURATION_BEGIN+1); { the end of user transaction }
  OCI_DURATION_STATEMENT = (OCI_DURATION_BEGIN+3);
{ This is to be used only during callouts.  It is similar to that
of OCI_DURATION_CALL, but lasts only for the duration of a callout.
Its heap is from PGA }
  OCI_DURATION_CALLOUT = (OCI_DURATION_BEGIN+4);
  OCI_DURATION_LAST = OCI_DURATION_CALLOUT; { last of predefined durations }

  OCI_TEMP_BLOB     = 1; { LOB type - BLOB }
  OCI_TEMP_CLOB     = 2; { LOB type - CLOB }
  OCI_TEMP_NCLOB    = 3; { LOB type - NCLOB }

const
  MAXTXNAMELEN    = 64;
  XIDDATASIZE     = 128; { size in bytes }
  MAXGTRIDSIZE    = 64;  { maximum size in bytes of gtrid }
  MAXBQUALSIZE    = 64;  { maximum size in bytes of bqual }
  NULLXID_ID      = -1;

  { Transaction branch identification: XID and NULLXID: }
type
  PXID = ^TXID;
  TXID = record
    formatID: sb4;     { format identifier }
    gtrid_length: sb4; { value from 1 through 64 }
    bqual_length: sb4; { value from 1 through 64 }
    data: array [0 .. XIDDATASIZE - 1] of ub1;
  end;

(*------------------------- OBJECT REFERENCE (REF) --------------------------*)

  PPOCIRef = ^POCIRef;
  POCIRef = Pointer;
(*
 * OCIRef - OCI object REFerence
 *
 * In the Oracle object runtime environment, an object is identified by an
 * object reference (ref) which contains the object identifier plus other
 * runtime information.  The contents of a ref is opaque to clients.  Use
 * OCIObjectNew() to construct a ref.
 *)

  PPoratext = ^Poratext;
  Poratext = Pointer;

(*****************************************************************************
 *                             ORACLE DATE TYPE                              *
 *****************************************************************************)

  POCITime = ^TOCITime;
  TOCITime = record
    OCITimeHH: ub1;                     // hours; range is 0 <= hours <=23
    OCITimeMI: ub1;                     // minutes; range is 0 <= minutes <= 59
    OCITimeSS: ub1;                     // seconds; range is 0 <= seconds <= 59
  end;

(*
 * OCITime - OCI Time portion of date
 *
 * This structure should be treated as an opaque structure as the format
 * of this structure may change. Use OCIDateGetTime/OCIDateSetTime
 * to manipulate time portion of OCIDate.
 *)

  POCIDate = ^TOCIDate;
  TOCIDate = record
    OCIDateYYYY: sb2;         // gregorian year; range is -4712 <= year <= 9999
    OCIDateMM: ub1;           // month; range is 1 <= month < 12
    OCIDateDD: ub1;           // day; range is 1 <= day <= 31
    OCIDateTime: TOCITime;    // time
  end;

const
  MAXUB4  = High(ub4);
  MAXSB4  = High(sb4);

{***************** Plain API constants definition ****************}

  { OCI Handle Types }
  OCI_HTYPE_FIRST               = 1;
  OCI_HTYPE_ENV                 = 1;
  OCI_HTYPE_ERROR               = 2;
  OCI_HTYPE_SVCCTX              = 3;
  OCI_HTYPE_STMT                = 4;
  OCI_HTYPE_BIND                = 5;
  OCI_HTYPE_DEFINE              = 6;
  OCI_HTYPE_DESCRIBE            = 7;
  OCI_HTYPE_SERVER              = 8;
  OCI_HTYPE_SESSION             = 9;
  OCI_HTYPE_TRANS               = 10;
  OCI_HTYPE_COMPLEXOBJECT       = 11;
  OCI_HTYPE_SECURITY            = 12;
  OCI_HTYPE_SUBSCRIPTION        = 13;
  OCI_HTYPE_DIRPATH_CTX         = 14;
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;
  OCI_HTYPE_DIRPATH_STREAM      = 16;
  OCI_HTYPE_PROC                = 17;
  OCI_HTYPE_LAST                = 17;

  { OCI Descriptor Types }
  OCI_DTYPE_FIRST               = 50;
  OCI_DTYPE_LOB                 = 50; //lob  locator
  OCI_DTYPE_SNAP                = 51;
  OCI_DTYPE_RSET                = 52;
  OCI_DTYPE_PARAM               = 53; //a parameter descriptor obtained from ocigparm
  OCI_DTYPE_ROWID               = 54;
  OCI_DTYPE_COMPLEXOBJECTCOMP   = 55;
  OCI_DTYPE_FILE                = 56; //File Lob locator
  OCI_DTYPE_AQENQ_OPTIONS       = 57; //enqueue options
  OCI_DTYPE_AQDEQ_OPTIONS       = 58; //dequeue options
  OCI_DTYPE_AQMSG_PROPERTIES    = 59; //message properties
  OCI_DTYPE_AQAGENT             = 60; //aq agent
  OCI_DTYPE_LOCATOR             = 61;
  OCI_DTYPE_INTERVAL_YM         = 62; //Interval year month
  OCI_DTYPE_INTERVAL_DS         = 63; //Interval day second
  OCI_DTYPE_AQNFY_DESCRIPTOR    = 64;
  OCI_DTYPE_LAST                = 64;
  OCI_DTYPE_DATE                = 65;  { Date }
  OCI_DTYPE_TIME                = 66;  { Time }
  OCI_DTYPE_TIME_TZ             = 67;  { Time with timezone }
  OCI_DTYPE_TIMESTAMP           = 68;  { Timestamp }
  OCI_DTYPE_TIMESTAMP_TZ        = 69;  { Timestamp with timezone }
  OCI_DTYPE_TIMESTAMP_LTZ       = 70;  { Timestamp with local tz }

  { OCI Attributes Types }
  OCI_ATTR_FNCODE               = 1;   // the OCI function code
  OCI_ATTR_OBJECT               = 2;   // is the environment initialized in object mode
  OCI_ATTR_NONBLOCKING_MODE     = 3;   // non blocking mode
  OCI_ATTR_SQLCODE              = 4;   // the SQL verb
  OCI_ATTR_ENV                  = 5;   // the environment handle
  OCI_ATTR_SERVER               = 6;   // the server handle
  OCI_ATTR_SESSION              = 7;   // the user session handle
  OCI_ATTR_TRANS                = 8;   // the transaction handle
  OCI_ATTR_ROW_COUNT            = 9;   // the rows processed so far
  OCI_ATTR_SQLFNCODE            = 10;  // the SQL verb of the statement
  OCI_ATTR_PREFETCH_ROWS        = 11;  // sets the number of rows to prefetch
  OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  // the prefetch rows of nested table
  OCI_ATTR_PREFETCH_MEMORY      = 13;  // memory limit for rows fetched
  OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;// memory limit for nested rows
  OCI_ATTR_CHAR_COUNT           = 15;  // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL                = 16;  // packed decimal scale
  OCI_ATTR_FSPRECISION          = OCI_ATTR_PDSCL; // fs prec for datetime data types
  OCI_ATTR_PDPRC                = 17;  // packed decimal format
  OCI_ATTR_LFPRECISION          = OCI_ATTR_PDPRC; // fs prec for datetime data types
  OCI_ATTR_PARAM_COUNT          = 18;  // number of column in the select list
  OCI_ATTR_ROWID                = 19;  // the rowid
  OCI_ATTR_CHARSET              = 20;  // the character set value
  OCI_ATTR_NCHAR                = 21;  // NCHAR type
  OCI_ATTR_USERNAME             = 22;  // username attribute
  OCI_ATTR_PASSWORD             = 23;  // password attribute
  OCI_ATTR_STMT_TYPE            = 24;  // statement type
  OCI_ATTR_INTERNAL_NAME        = 25;  // user friendly global name
  OCI_ATTR_EXTERNAL_NAME        = 26;  // the internal name for global txn
  OCI_ATTR_XID                  = 27;  // XOPEN defined global transaction id
  OCI_ATTR_TRANS_LOCK           = 28;  //
  OCI_ATTR_TRANS_NAME           = 29;  // string to identify a global transaction
  OCI_ATTR_HEAPALLOC            = 30;  // memory allocated on the heap
  OCI_ATTR_CHARSET_ID           = 31;  // Character Set ID
  OCI_ATTR_CHARSET_FORM         = 32;  // Character Set Form
  OCI_ATTR_MAXDATA_SIZE         = 33;  // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE       = 34;  // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE       = 35;  // object cache maximum size percentage
  OCI_ATTR_PINOPTION            = 36;  // object cache default pin option
  OCI_ATTR_ALLOC_DURATION       = 37;  // object cache default allocation duration
  OCI_ATTR_PIN_DURATION         = 38;  // object cache default pin duration
  OCI_ATTR_FDO                  = 39;  // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT = 41; // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED        = 42;  // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK                = 43;  // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE           = 44;  // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY             = 45;  // empty lob ?
  OCI_ATTR_SESSLANG             = 46;  // session language handle

  OCI_ATTR_VISIBILITY           = 47;  // visibility
  OCI_ATTR_RELATIVE_MSGID       = 48;  // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION   = 49;  // sequence deviation

  OCI_ATTR_CONSUMER_NAME        = 50;  // consumer name
  OCI_ATTR_DEQ_MODE             = 51;  // dequeue mode
  OCI_ATTR_NAVIGATION           = 52;  // navigation
  OCI_ATTR_WAIT                 = 53;  // wait
  OCI_ATTR_DEQ_MSGID            = 54;  // dequeue message id

  OCI_ATTR_PRIORITY             = 55;  // priority
  OCI_ATTR_DELAY                = 56;  // delay
  OCI_ATTR_EXPIRATION           = 57;  // expiration
  OCI_ATTR_CORRELATION          = 58;  // correlation id
  OCI_ATTR_ATTEMPTS             = 59;  // # of attempts
  OCI_ATTR_RECIPIENT_LIST       = 60;  // recipient list
  OCI_ATTR_EXCEPTION_QUEUE      = 61;  // exception queue name
  OCI_ATTR_ENQ_TIME             = 62;  // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE            = 63;  // message state (only OCIAttrGet)
                                       // NOTE: 64-66 used below
  OCI_ATTR_AGENT_NAME           = 64;  // agent name
  OCI_ATTR_AGENT_ADDRESS        = 65;  // agent address
  OCI_ATTR_AGENT_PROTOCOL       = 66;  // agent protocol

  OCI_ATTR_SENDER_ID            = 68;  // sender id
  OCI_ATTR_ORIGINAL_MSGID       = 69;  // original message id

  OCI_ATTR_QUEUE_NAME           = 70;  // queue name
  OCI_ATTR_NFY_MSGID            = 71;  // message id
  OCI_ATTR_MSG_PROP             = 72;  // message properties

  OCI_ATTR_NUM_DML_ERRORS       = 73;  // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET       = 74;  // row offset in the array

  OCI_ATTR_DATEFORMAT           = 75;  // default date format string
  OCI_ATTR_BUF_ADDR             = 76;  // buffer address
  OCI_ATTR_BUF_SIZE             = 77;  // buffer size
  OCI_ATTR_DIRPATH_MODE         = 78;  // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG        = 79;  // nologging option
  OCI_ATTR_DIRPATH_PARALLEL     = 80;  // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS             = 81;  // number of rows in column array
                                       // NOTE that OCI_ATTR_NUM_COLS is a column
                                       // array attribute too.

  OCI_ATTR_COL_COUNT            = 82;  // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET        = 83;  // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC     = 84;  // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP         = 85;  // server group name

  OCI_ATTR_MIGSESSION           = 86;  // migratable session attribute

  OCI_ATTR_NOCACHE              = 87;  // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE         = 88;  // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME     = 89;  // Instance name
  OCI_ATTR_MEMPOOL_APPNAME      = 90;  // Application name
  OCI_ATTR_MEMPOOL_HOMENAME     = 91;  // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL        = 92;  // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES                = 93;  // Modes

  OCI_ATTR_SUBSCR_NAME          = 94;  // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK      = 95;  // associated callback
  OCI_ATTR_SUBSCR_CTX           = 96;  // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD       = 97;  // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE     = 98;  // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS    = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100; // Initial client role list

  OCI_ATTR_UNK                  = 101; // unknown attribute
  OCI_ATTR_NUM_COLS             = 102; // number of columns
  OCI_ATTR_LIST_COLUMNS         = 103; // parameter of the column list
  OCI_ATTR_RDBA                 = 104; // DBA of the segment header
  OCI_ATTR_CLUSTERED            = 105; // whether the table is clustered
  OCI_ATTR_PARTITIONED          = 106; // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY           = 107; // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS       = 108; // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS     = 109; // parameter of the subprogram list
  OCI_ATTR_REF_TDO              = 110; // REF to the type descriptor
  OCI_ATTR_LINK                 = 111; // the database link name
  OCI_ATTR_MIN                  = 112; // minimum value
  OCI_ATTR_MAX                  = 113; // maximum value
  OCI_ATTR_INCR                 = 114; // increment value
  OCI_ATTR_CACHE                = 115; // number of sequence numbers cached
  OCI_ATTR_ORDER                = 116; // whether the sequence is ordered
  OCI_ATTR_HW_MARK              = 117; // high-water mark
  OCI_ATTR_TYPE_SCHEMA          = 118; // type's schema name
  OCI_ATTR_TIMESTAMP            = 119; // timestamp of the object
  OCI_ATTR_NUM_ATTRS            = 120; // ub2: number of attributes
  OCI_ATTR_NUM_PARAMS           = 121; // ub2: number of parameters
  OCI_ATTR_OBJID                = 122; // ub4: object id for a table or view
  OCI_ATTR_PTYPE                = 123; // ub1: Type of information described by the parameter
  OCI_ATTR_PARAM                = 124; // parameter descriptor
  OCI_ATTR_OVERLOAD_ID          = 125; // ub2: overload ID for funcs and procs
  OCI_ATTR_TABLESPACE           = 126; // table name space
  OCI_ATTR_TDO                  = 127; // TDO of a type
  OCI_ATTR_LTYPE                = 128; // list type
  OCI_ATTR_PARSE_ERROR_OFFSET   = 129; // Parse Error offset
  OCI_ATTR_IS_TEMPORARY         = 130; // whether table is temporary
  OCI_ATTR_IS_TYPED             = 131; // whether table is typed
  OCI_ATTR_DURATION             = 132; // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS    = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME             = 134; // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA           = 135; // schema name
  OCI_ATTR_OBJ_ID               = 136; // top level schema object id

  OCI_ATTR_TRANS_TIMEOUT        = 142; // transaction timeout
  OCI_ATTR_SERVER_STATUS        = 143; // state of the server handle
  OCI_ATTR_STATEMENT            = 144; // statement txt in stmt hdl

  OCI_ATTR_DEQCOND              = 146; // dequeue condition
  OCI_ATTR_RESERVED_2           = 147; // reserved


  OCI_ATTR_SUBSCR_RECPT         = 148; // recepient of subscription
  OCI_ATTR_SUBSCR_RECPTPROTO    = 149; // protocol for recepient

  { For values 150 - 151, see DirPathAPI attribute section in this file }

  OCI_ATTR_LDAP_HOST            = 153; //LDAP host to connect to
  OCI_ATTR_LDAP_PORT            = 154; //LDAP port to connect to
  OCI_ATTR_BIND_DN              = 155; //bind DN
  OCI_ATTR_LDAP_CRED            = 156; //credentials to connect to LDAP
  OCI_ATTR_WALL_LOC             = 157; // client wallet location
  OCI_ATTR_LDAP_AUTH            = 158; // LDAP authentication method
  OCI_ATTR_LDAP_CTX             = 159; // LDAP adminstration context DN
  OCI_ATTR_SERVER_DNS           = 160; // list of registration server DNs

  OCI_ATTR_DN_COUNT             = 161; // the number of server DNs
  OCI_ATTR_SERVER_DN            = 162; // server DN attribute

  OCI_ATTR_MAXCHAR_SIZE         = 163; // max char size of data

  OCI_ATTR_CURRENT_POSITION     = 164; // for scrollable result sets

// Added to get attributes for ref cursor to statement handle
  OCI_ATTR_RESERVED_3           = 165; // reserved
  OCI_ATTR_RESERVED_4           = 166; // reserved

// For value 167, see DirPathAPI attribute section in this file

  OCI_ATTR_DIGEST_ALGO          = 168; // digest algorithm
  OCI_ATTR_CERTIFICATE          = 169; // certificate
  OCI_ATTR_SIGNATURE_ALGO       = 170; // signature algorithm
  OCI_ATTR_CANONICAL_ALGO       = 171; // canonicalization algo.
  OCI_ATTR_PRIVATE_KEY          = 172; // private key
  OCI_ATTR_DIGEST_VALUE         = 173; // digest value
  OCI_ATTR_SIGNATURE_VAL        = 174; // signature value
  OCI_ATTR_SIGNATURE            = 175; // signature

// attributes for setting OCI stmt caching specifics in svchp
  OCI_ATTR_STMTCACHESIZE        = 176; // size of the stm cache

// --------------------------- Connection Pool Attributes ------------------
  OCI_ATTR_CONN_NOWAIT          = 178;
  OCI_ATTR_CONN_BUSY_COUNT      = 179;
  OCI_ATTR_CONN_OPEN_COUNT      = 180;
  OCI_ATTR_CONN_TIMEOUT         = 181;
  OCI_ATTR_STMT_STATE           = 182;
  OCI_ATTR_CONN_MIN             = 183;
  OCI_ATTR_CONN_MAX             = 184;
  OCI_ATTR_CONN_INCR            = 185;

// For value 187, see DirPathAPI attribute section in this file

  OCI_ATTR_NUM_OPEN_STMTS       = 188; // open stmts in session
  OCI_ATTR_DESCRIBE_NATIVE      = 189; // get native info via desc

  OCI_ATTR_BIND_COUNT           = 190; // number of bind postions
  OCI_ATTR_HANDLE_POSITION      = 191; // pos of bind/define handle
  OCI_ATTR_RESERVED_5           = 192; // reserverd
  OCI_ATTR_SERVER_BUSY          = 193; // call in progress on server

// For value 194, see DirPathAPI attribute section in this file

// notification presentation for recipient
  OCI_ATTR_SUBSCR_RECPTPRES     = 195;
  OCI_ATTR_TRANSFORMATION       = 196; // AQ message transformation

  OCI_ATTR_ROWS_FETCHED         = 197; // rows fetched in last call

// --------------------------- Snapshot attributes -------------------------
  OCI_ATTR_SCN_BASE             = 198; // snapshot base
  OCI_ATTR_SCN_WRAP             = 199; // snapshot wrap

// --------------------------- Miscellanous attributes ---------------------
  OCI_ATTR_RESERVED_6           = 200; // reserved
  OCI_ATTR_READONLY_TXN         = 201; // txn is readonly
  OCI_ATTR_RESERVED_7           = 202; // reserved
  OCI_ATTR_ERRONEOUS_COLUMN     = 203; // position of erroneous col
  OCI_ATTR_RESERVED_8           = 204; // reserved
  OCI_ATTR_ASM_VOL_SPRT         = 205; // ASM volume supported?

  // for inheritance - part 2
  OCI_ATTR_IS_FINAL_TYPE        = 279; //is final type ?
  OCI_ATTR_IS_INSTANTIABLE_TYPE = 280; //is instantiable type ?
  OCI_ATTR_IS_FINAL_METHOD      = 281; //is final method ?
  OCI_ATTR_IS_INSTANTIABLE_METHOD = 282; // is instantiable method ?
  OCI_ATTR_IS_OVERRIDING_METHOD = 283; // is overriding method ?

  OCI_ATTR_DESC_SYNBASE         = 284; //Describe the base object

  OCI_ATTR_CHAR_USED            = 285; //char length semantics
  OCI_ATTR_CHAR_SIZE            = 286; //char length

  OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE = 438; // default prefetch size

  { OCI Error Return Values }
  OCI_SUCCESS             = 0;
  OCI_SUCCESS_WITH_INFO   = 1;
  OCI_NO_DATA             = 100;
  OCI_ERROR               = -1;
  OCI_INVALID_HANDLE      = -2;
  OCI_NEED_DATA           = 99;
  OCI_STILL_EXECUTING     = -3123;
  OCI_CONTINUE            = -24200;

  { Generic Default Value for Modes, .... }
  OCI_DEFAULT     = $0;

  { OCI Init Mode }
  OCI_THREADED    = $1;
  OCI_OBJECT      = $2;
  OCI_EVENTS      = $4;
  OCI_SHARED      = $10;
  OCI_NO_UCB      = $40;
  OCI_NO_MUTEX    = $80;

  { OCI Credentials }
  OCI_CRED_RDBMS  = 1;
  OCI_CRED_EXT    = 2;
  OCI_CRED_PROXY  = 3;

  { OCI Authentication Mode }
  OCI_MIGRATE     = $0001;             // migratable auth context
  OCI_SYSDBA      = $0002;             // for SYSDBA authorization
  OCI_SYSOPER     = $0004;             // for SYSOPER authorization
  OCI_PRELIM_AUTH = $0008;             // for preliminary authorization

  { OCIPasswordChange }
  OCI_AUTH        = $08;               // Change the password but do not login

  { OCI Data Types }
  SQLT_CHR = 1  ;       //(ORANET TYPE) character string
  SQLT_NUM = 2  ;       //(ORANET TYPE) oracle numeric
  SQLT_INT = 3  ;       //(ORANET TYPE) integer
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

  { OCI Statement Types }
  OCI_STMT_SELECT  = 1;   // select statement
  OCI_STMT_UPDATE  = 2;   // update statement
  OCI_STMT_DELETE  = 3;   // delete statement
  OCI_STMT_INSERT  = 4;   // Insert Statement
  OCI_STMT_CREATE  = 5;   // create statement
  OCI_STMT_DROP    = 6;   // drop statement
  OCI_STMT_ALTER   = 7;   // alter statement
  OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
  OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

  { OCI Statement language }
  OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
  OCI_V7_SYNTAX   = 2;    // V7 language
  OCI_V8_SYNTAX   = 3;    // V8 language

  { OCI Statement Execute mode }
  OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
  OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
  OCI_STMT_SCROLLABLE_READONLY = $08;    // cursor scrollable
  OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
  OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
  OCI_NON_BLOCKING      = $40;    // non-blocking
  OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
  OCI_PARSE_ONLY        = $100;   // only parse the statement
  OCI_SHOW_DML_WARNINGS = $400;   // return OCI_SUCCESS_WITH_INFO for delete/update w/no where clause
  OCI_RESULT_CACHE      = $20000; // hint to use query caching
  OCI_NO_RESULT_CACHE   = $40000; // hint to bypass query caching

  OCI_DATA_AT_EXEC    = $02;      // data at execute time
  OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
  OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

  { OCI Transaction modes }
  OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
  OCI_TRANS_JOIN         = $00000002; // join an existing transaction
  OCI_TRANS_RESUME       = $00000004; // resume this transaction
  OCI_TRANS_STARTMASK    = $000000ff;

  OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
  OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
  OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
  OCI_TRANS_ISOLMASK     = $0000ff00;

  OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
  OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
  OCI_TRANS_TYPEMASK     = $000f0000;

  OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
  OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

  { OCI piece wise fetch }
  OCI_ONE_PIECE       = 0; // one piece
  OCI_FIRST_PIECE     = 1; // the first piece
  OCI_NEXT_PIECE      = 2; // the next of many pieces
  OCI_LAST_PIECE      = 3; // the last piece

  { OCI fetch modes }
  OCI_FETCH_NEXT      = $02;  // next row
  OCI_FETCH_FIRST     = $04;  // first row of the result set
  OCI_FETCH_LAST      = $08;  // the last row of the result set
  OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
  OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
  OCI_FETCH_RELATIVE  = $40;  // offset relative to current

  {****************** Describe Handle Parameter Attributes *****************}

  { Attributes common to Columns and Stored Procs }
  OCI_ATTR_DATA_SIZE      = 1;    // ub2 The maximum size of the type attribute.
                                  // This length is returned in bytes and not
                                  // characters for strings and raws.
                                  // It returns 22 for NUMBERs
  OCI_ATTR_DATA_TYPE      = 2;    // ub2: the SQL type of the column/argument
  OCI_ATTR_DISP_SIZE      = 3;    // the display size
  OCI_ATTR_NAME           = 4;    // the name of the column/argument
  OCI_ATTR_PRECISION      = 5;    // ub1 for explicit describe (OCIDescribeAny)
                                  // sb2 for implicit describe (stmt)
                                  // precision if number type
  OCI_ATTR_SCALE          = 6;    // sb1: scale if number type
  OCI_ATTR_IS_NULL        = 7;    // is it null ?
  OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types
  OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name
  OCI_ATTR_SUB_NAME       = 10;   // type name if package private type
  OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

  { complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

  { Only Columns }
  OCI_ATTR_DISP_NAME                 = 100;  // the display name

  { Only Stored Procs }
  OCI_ATTR_OVERLOAD                  = 210;  // is this position overloaded
  OCI_ATTR_LEVEL                     = 211;  // level for structured types
  OCI_ATTR_HAS_DEFAULT               = 212;  // has a default value
  OCI_ATTR_IOMODE                    = 213;  // OCITypeParamMode(sb4): in, out inout
  OCI_ATTR_RADIX                     = 214;  // returns a radix
  OCI_ATTR_NUM_ARGS                  = 215;  // total number of arguments

  { only named type attributes }
  OCI_ATTR_TYPECODE                  = 216;   // object or collection
  OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table
  OCI_ATTR_VERSION                   = 218;   // user assigned version
  OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type
  OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type
  OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type
  OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type
  OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr
  OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute
  OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute
  OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute
  OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types
  OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes
  OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods
  OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods
  OCI_ATTR_MAP_METHOD                = 232;   // map method of type
  OCI_ATTR_ORDER_METHOD              = 233;   // order method of type

  { only collection element }
  OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements

  { only type methods }
  OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level
  OCI_ATTR_IS_SELFISH                = 236;   // method selfish
  OCI_ATTR_IS_VIRTUAL                = 237;   // virtual
  OCI_ATTR_IS_INLINE                 = 238;   // inline
  OCI_ATTR_IS_CONSTANT               = 239;   // constant
  OCI_ATTR_HAS_RESULT                = 240;   // has result
  OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
  OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor
  OCI_ATTR_IS_OPERATOR               = 243;   // operator
  OCI_ATTR_IS_MAP                    = 244;   // a map method
  OCI_ATTR_IS_ORDER                  = 245;   // order method
  OCI_ATTR_IS_RNDS                   = 246;   // read no data state method
  OCI_ATTR_IS_RNPS                   = 247;   // read no process state
  OCI_ATTR_IS_WNDS                   = 248;   // write no data state method
  OCI_ATTR_IS_WNPS                   = 249;   // write no process state

  OCI_ATTR_DESC_PUBLIC               = 250;   // public object

  { Object Cache Enhancements : attributes for User Constructed Instances }
  OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
  OCI_ATTR_UCI_CONSTRUCT             = 252;
  OCI_ATTR_UCI_DESTRUCT              = 253;
  OCI_ATTR_UCI_COPY                  = 254;
  OCI_ATTR_UCI_PICKLE                = 255;
  OCI_ATTR_UCI_UNPICKLE              = 256;
  OCI_ATTR_UCI_REFRESH               = 257;

  { for type inheritance }
  OCI_ATTR_IS_SUBTYPE                = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
  OCI_ATTR_SUPERTYPE_NAME            = 260;

  { for schemas }
  OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

  { Enable OCI Server-Side Statement Caching }
  OCI_STMT_CACHE       = $40;
  OCI_STMTCACHE_DELETE = $10;

  { for database }
  OCI_ATTR_NCHARSET_ID               = 262;   // char set id
  OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas
  OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length
  OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior
  OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength
  OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location
  OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support
  OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
  OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
  OCI_ATTR_LOCKING_MODE              = 272;   // locking mode

  OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
  OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
  OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

  {client side character and national character set ids }
  OCI_NLS_CHARSET_ID       = OCI_ATTR_CHARSET_ID;  // charset id in env
  OCI_NLS_NCHARSET_ID      = OCI_ATTR_NCHARSET_ID; // ncharset id in env

  { Piece Information }
  OCI_PARAM_IN                       = $01;  // in parameter
  OCI_PARAM_OUT                      = $02;  // out parameter

  { LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE     = 1;
  OCI_LOB_BUFFER_NOFREE   = 2;

  { FILE open modes }
  OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
  { LOB open modes }
  OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
  OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs

  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information }
  SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
  SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
  SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
  SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
  SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

  {************************ OCIDesribeAny *************************}

  { Describe mode }
  OCI_OTYPE_NAME = 1;
  OCI_OTYPE_REF = 2;
  OCI_OTYPE_PTR = 3;

  { Object type }
  OCI_PTYPE_UNK           = 0;    // unknown
  OCI_PTYPE_TABLE         = 1;    // table
  OCI_PTYPE_VIEW          = 2;    // view
  OCI_PTYPE_PROC          = 3;    // procedure
  OCI_PTYPE_FUNC          = 4;    // function
  OCI_PTYPE_PKG           = 5;    // package
  OCI_PTYPE_TYPE          = 6;    // user-defined type
  OCI_PTYPE_SYN           = 7;    // synonym
  OCI_PTYPE_SEQ           = 8;    // sequence
  OCI_PTYPE_COL           = 9;    // column
  OCI_PTYPE_ARG           = 10;   // argument
  OCI_PTYPE_LIST          = 11;   // list
  OCI_PTYPE_TYPE_ATTR     = 12;   // user-defined type's attribute
  OCI_PTYPE_TYPE_COLL     = 13;   // collection type's element
  OCI_PTYPE_TYPE_METHOD   = 14;   // user-defined type's method
  OCI_PTYPE_TYPE_ARG      = 15;   // user-defined type method's argument
  OCI_PTYPE_TYPE_RESULT   = 16;   // user-defined type method's result

  { Proc/Func param type }
  OCI_TYPEPARAM_IN    = 0;
  OCI_TYPEPARAM_OUT   = 1;
  OCI_TYPEPARAM_INOUT = 2;

  { NLS environmet }
  OCI_NLS_CHARSET_MAXBYTESZ = 91;

  { enum OCIPinOpt }
  OCI_PIN_DEFAULT = 1;            //* default pin option */
  OCI_PIN_ANY     = 3;            //* pin any copy of the object */
  OCI_PIN_RECENT  = 4;            //* pin recent copy of the object */
  OCI_PIN_LATEST  = 5;            //* pin latest copy of the object */

  { enum OCILockOpt }
  OCI_LOCK_NONE     = 1;          //* null (same as no lock) */
  OCI_LOCK_X        = 2;          //* exclusive lock */
  OCI_LOCK_X_NOWAIT = 3;          //* exclusive lock, do not wait  */

  { OBJECT FREE OPTION }
  OCI_OBJECTFREE_FORCE =1;
  OCI_OBJECTFREE_NONULL=2;

  OCI_PREP2_CACHE_SEARCHONLY: ub4 = $0010;

  { number }
  OCI_NUMBER_UNSIGNED = 0;                        // Unsigned type -- ubX
  OCI_NUMBER_SIGNED = 2;                          // Signed type -- sbX

  // some connection issued errors:
  ORA_03113_end_of_file_on_communication_channel = 3113;
  ORA_03114_not_connected_to_ORACLE = 3114;

type
  {** Represents a generic interface to Oracle native API. }
  IZOraclePlainDriver = interface (IZPlainDriver)
    ['{22404660-C95F-4346-A3DB-7C6DFE15F115}']
  end;

  {** Implements a driver for Oracle 9i }
  TZOraclePlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZOraclePlainDriver)
  protected
    procedure LoadApi; override;
  public
    function Clone: IZPlainDriver; override;
  public
    OCIEnvNlsCreate: function(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword; cdecl;
    OCIServerAttach: function(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword; cdecl;
    OCIServerDetach: function(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword; cdecl;
    OCIServerRelease: function(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1; version:pointer): sword; cdecl;
    OCIServerVersion: function(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1): sword; cdecl;

    OCISessionBegin: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4):sword; cdecl;
    OCISessionEnd: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword; cdecl;
    OCITransStart: function(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword; cdecl;
    OCITransRollback: function(svchp:POCISvcCtx; errhp:POCIError;
      flags: ub4): sword; cdecl;
    OCITransCommit: function(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4) :sword; cdecl;

    OCIPing: function(svchp: POCISvcCtx; errhp: POCIError; mode: ub4): sword; cdecl;
    OCIBreak: function(svchp: POCISvcCtx; errhp:POCIError): sword; cdecl;
    OCIPasswordChange: function(svchp: POCISvcCtx; errhp: POCIError;
      user_name: text; usernm_len: ub4; opasswd: text; opasswd_len: ub4;
      npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;

    OCIClientVersion: procedure(major_version, minor_version, update_num,
      patch_num, port_update_num: psword); cdecl;
    OCIHandleAlloc: function(parenth: POCIHandle; var hndlpp: POCIHandle;
      atype: ub4; xtramem_sz: size_T; usrmempp: PPointer): sword; cdecl;
    OCIHandleFree: function(hndlp: Pointer; atype: ub4): sword; cdecl;

    OCIErrorGet: function(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword; cdecl;

    OCIAttrSet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError):sword; cdecl;
    OCIAttrGet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError):sword; cdecl;
    OCINlsNumericInfoGet: function(envhp: POCIEnv; errhp: POCIError; val: psb4;
      item: ub2): sword; cdecl;
    OCIStmtPrepare: function(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4):sword; cdecl;

    OCIStmtPrepare2: function(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword; cdecl;

    OCIStmtRelease: function(stmtp: POCIStmt; errhp: POCIError; key: text;
      key_len: ub4; mode: ub4):sword; cdecl;

    OCIStmtExecute: function(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword; cdecl;

    OCIParamGet: function(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword; cdecl;

    OCIStmtFetch: function(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword; cdecl;

    OCIStmtFetch2: function(stmtp: POCIStmt; errhp: POCIError; const nrows: ub4;
      const orientation: ub2; const fetchOffset: sb4; const mode: ub4): sword; cdecl;

    OCIDefineByPos: function(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword; cdecl;
    OCIBindByPos: function(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword; cdecl;
    OCIBindObject: function(bindp: POCIBind; errhp: POCIError; const _type:
      POCIType; pgvpp: PPointer; pvszsp: pub4; indpp: PPointer;
      indszp: pub4): sword; cdecl;
    OCIDefineObject: function(defnpp: POCIDefine; errhp: POCIError;
      _type: POCIHandle; pgvpp: pointer; pvszsp: Pub4; indpp: pointer;
      indszp: Pub4):sword; cdecl;
    OCIResultSetToStmt: function(rsetdp: POCIHandle; errhp: POCIError): sword; cdecl;

    OCIDescriptorAlloc: function(parenth: POCIEnv; var descpp: POCIDescriptor;
      htype: ub4; xtramem_sz: size_t; usrmempp: PPointer): sword; cdecl;
    OCIArrayDescriptorAlloc: function(parenth: POCIEnv; var descpp: POCIDescriptor;
      htype: ub4; array_size: ub4; xtramem_sz: size_t; usrmempp: PPointer): sword; cdecl;
    OCIDescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;
    OCIArrayDescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;
    { lob methods }
    OCILobCopy: function(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
      dst_offset: ub4; src_offset: ub4): sword; cdecl;
    OCILobCopy2: function(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: oraub8;
      dst_offset: oraub8; src_offset: oraub8): sword; cdecl;
    OCILobGetChunkSize: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; out lenp: ub4): sword; cdecl;
    OCILobGetLength: function (svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; out lenp: ub4): sword; cdecl;
    OCILobGetLength2: function (svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; out lenp: oraub8): sword; cdecl;
    OCILobLocatorAssign: function(svchp: POCISvcCtx; errhp: POCIError;
      const src_locp: POCILobLocator; dst_locpp: PPOCILobLocator): sword; cdecl;
    OCILobOpen: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword; cdecl;
    OCILobRead: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobRead2: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; byte_amtp: Poraub8; char_amtp: Poraub8; offset: oraub8;
      bufp: Pointer; bufl: oraub8; piece: ub1; ctxp: Pointer; cbfp: Pointer;
      csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobTrim: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: ub4): sword; cdecl;
    OCILobTrim2: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: oraub8): sword; cdecl;
    OCILobWrite: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobWrite2: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var byte_amtp: oraub8; var char_amtp: oraub8;
      offset: oraub8; bufp: Pointer; bufl: oraub8; piece: ub1; ctxp: Pointer;
      cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobCreateTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
      cache: LongBool; duration: OCIDuration): sword; cdecl;
    OCILobIsEqual: function(envhp: POCIEnv; errhp: POCIError;
      const x: POCILobLocator; const y: POCILobLocator;
      is_equal: PLongBool): sword; cdecl;
    OCILobIsOpen: function(svchp: POCISvcCtx; errhp: POCIError;
               locp: POCILobLocator; var flag: LongBool): sword; cdecl;
    OCILobIsTemporary: function(envhp: POCIEnv; errhp: POCIError;
      locp: POCILobLocator; var is_temporary: LongBool): sword; cdecl;
    OCILobFreeTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;
    OCILobCharSetForm: function ( envhp: POCIEnv; errhp: POCIError;
          const locp: POCILobLocator; csfrm: pub1): sword; cdecl;
    OCILobCharSetId: function( envhp: POCIEnv; errhp: POCIError;
          const locp: POCILobLocator; csid: pub2): sword; cdecl;
    OCILobClose: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;

    OCIIntervalGetYearMonth: function (hndl: POCIHandle; err: POCIError; yr,mnth: psb4;
                          const int_result: POCIInterval): sword; cdecl;
    OCIIntervalGetDaySecond: function(hndl: POCIHandle; err:  POCIError; dy: psb4;
      hr, mm, ss, fsec: psb4; const int_result: POCIInterval): sword; cdecl;
    OCIDateTimeConstruct: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword; cdecl;
    OCIDateTimeGetDate: function(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; out year: sb2; out month: ub1;
      out day: ub1): sword; cdecl;
    OCIDateTimeGetTime: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; out hour: ub1; out minute: ub1; out sec: ub1;
      out fsec: ub4): sword; cdecl;
    { object api}
    OCITypeByRef: function(env: POCIEnv; err: POCIError; type_ref: POCIRef;
      pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword; cdecl;
    OCIObjectNew: function(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      const typecode: OCITypeCode; const tdo: POCIType; const table: Pointer;
      const duration: OCIDuration; const value: boolean;
      instance: PPointer): sword; cdecl;
    OCIObjectPin: function(env: POCIEnv; err: POCIError;
      const object_ref: POCIRef; const corhdl: POCIComplexObject;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock_option: OCILockOpt; _object: PPointer): sword; cdecl;
    OCIObjectUnpin: function(env: POCIEnv; err: POCIError;
      const _object: Pointer): sword; cdecl;
    OCIObjectFree: function(env: POCIEnv; err: POCIError;
      const instance: pointer; const flags: ub2): sword; cdecl;
    OCIObjectGetTypeRef: function(env: POCIEnv; err: POCIError;
      const instance:pointer; type_ref: POCIRef): sword; cdecl;
    OCIDescribeAny: function(svchp: POCISvcCtx; errhp: POCIError;
      objptr: Pointer; objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
      objtyp: ub1; dschp: POCIDescribe): sword; cdecl;
    { number conversion to/from XXX }
    OCINumberToInt: function(err: POCIError; const number: POCINumber;
      rsl_length: uword; rsl_flag: uword; rsl: Pointer): sword; cdecl;
    OCINumberFromInt: function(err: POCIError; const inum: Pointer;
      inum_length: uword; inum_s_flag: uword; number: POCINumber): sword; cdecl;
    OCINumberToReal: function(err: POCIError; number: POCINumber;
      rsl_length: uword; rsl: Pointer): sword; cdecl;
    OCINumberFromReal: function(err: POCIError; const rnum: Pointer;
      rnum_length: uword; number: POCINumber): sword; cdecl;
    OCIStringAllocSize: function(env: POCIEnv; err: POCIError;
      const vs: POCIString; allocsize: Pub4): sword; cdecl;
    OCIStringResize: function(env: POCIEnv; err: POCIError;
      new_size: ub4; str: PPOCIString): sword; cdecl;
    OCIStringPtr: function(env: POCIEnv; const vs: POCIString): POraText; cdecl;
    OCIStringSize: function(env: POCIEnv; const vs: POCIString): UB4; cdecl;
  //Poratext = PAnsiChar;
  //PPoratext = PPAnsiChar;
    OCINumberToText: function(err: POCIError; const number: POCINumber;
                          const fmt: Poratext; fmt_length: ub4;
                          const nls_params: Poratext; nls_p_length: ub4;
                          buf_size: pub4; buf: poratext): sword; cdecl;
  public
    {interface required api}
    constructor Create;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

  end;

  const OCI_UTF16ID = 1000;

{$ENDIF ZEOS_DISABLE_ORACLE}

implementation

{$IFNDEF ZEOS_DISABLE_ORACLE}

uses ZEncoding;

{ TZOraclePlainDriver }

function TZOraclePlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'AL32UTF8';
end;

procedure TZOraclePlainDriver.LoadCodePages;
begin
  //All supporteds from XE
  AddCodePage('US7ASCII', 1, ceAnsi, zCP_us_ascii);
  AddCodePage('US8PC437', 4, ceAnsi, zCP_DOS437);
  AddCodePage('WE8PC850', 10, ceAnsi, zCP_DOS850);
  AddCodePage('WE8PC858', 28, ceAnsi, zCP_DOS858);
  AddCodePage('WE8ISO8859P1', 31, ceAnsi, zCP_L1_ISO_8859_1);
  AddCodePage('EE8ISO8859P2', 32, ceAnsi, zCP_L2_ISO_8859_2);
  AddCodePage('SE8ISO8859P3', 33, ceAnsi, zCP_L3_ISO_8859_3);
  AddCodePage('NEE8ISO8859P4', 34, ceAnsi, zCP_L4_ISO_8859_4);
  AddCodePage('CL8ISO8859P5', 35, ceAnsi, zCP_L5_ISO_8859_5);
  AddCodePage('AR8ISO8859P6', 36, ceAnsi, zCP_L6_ISO_8859_6);
  AddCodePage('EL8ISO8859P7', 37, ceAnsi, zCP_L7_ISO_8859_7);
  AddCodePage('IW8ISO8859P8', 38, ceAnsi, zCP_L8_ISO_8859_8);
  AddCodePage('WE8ISO8859P9', 39, ceAnsi, zCP_L5_ISO_8859_9);
  AddCodePage('NE8ISO8859P10', 40, ceAnsi, zCP_L6_ISO_8859_10);
  AddCodePage('TH8TISASCII', 41, ceAnsi);
  AddCodePage('VN8MSWIN1258', 45, ceAnsi, zCP_WIN1258);
  AddCodePage('WE8ISO8859P15', 46, ceAnsi, zCP_L9_ISO_8859_15);
  AddCodePage('BLT8ISO8859P13', 47, ceAnsi, zCP_L7_ISO_8859_13);
  AddCodePage('CEL8ISO8859P14', 48, ceAnsi, zCP_L8_ISO_8859_14);
  AddCodePage('CL8KOI8U', 51, ceAnsi, zCP_KOI8U);
  AddCodePage('AZ8ISO8859P9E', 52, ceAnsi);
  AddCodePage('EE8PC852', 150, ceAnsi, zCP_DOS852);
  AddCodePage('RU8PC866', 152, ceAnsi, zCP_DOS866);
  AddCodePage('TR8PC857', 156, ceAnsi, zCP_DOS857);
  AddCodePage('EE8MSWIN1250', 170, ceAnsi, zCP_WIN1250);
  AddCodePage('CL8MSWIN1251', 171, ceAnsi, zCP_WIN1251);
  AddCodePage('ET8MSWIN923', 172, ceAnsi, zCP_MSWIN923);
  AddCodePage('EL8MSWIN1253', 174, ceAnsi, zCP_WIN1253);
  AddCodePage('IW8MSWIN1255', 175, ceAnsi, zCP_WIN1255);
  AddCodePage('LT8MSWIN921', 176, ceAnsi, zCP_MSWIN921);
  AddCodePage('TR8MSWIN1254', 177, ceAnsi, zCP_WIN1254);
  AddCodePage('WE8MSWIN1252', 178, ceAnsi, zCP_WIN1252);
  AddCodePage('BLT8MSWIN1257', 179, ceAnsi, zCP_WIN1257);
  AddCodePage('BLT8CP921', 191, ceAnsi, zCP_MSWIN921);
  AddCodePage('CL8KOI8R', 196, ceAnsi, zCP_KOI8R);
  AddCodePage('BLT8PC775', 197, ceAnsi, zCP_DOS775);
  AddCodePage('EL8PC737', 382, ceAnsi, zCP_DOS737);
  AddCodePage('AR8ASMO8X', 500, ceAnsi, zCP_DOS708);
  AddCodePage('AR8ADOS720', 558, ceAnsi, zCP_DOS720);
  AddCodePage('AR8MSWIN1256', 560, ceAnsi, zCP_WIN1256);
  AddCodePage('JA16EUC', 830, ceAnsi, zCP_euc_JP_win);
  AddCodePage('JA16SJIS', 832, ceAnsi, zCP_csISO2022JP);
  AddCodePage('JA16EUCTILDE', 837, ceAnsi);
  AddCodePage('JA16SJISTILDE', 838, ceAnsi);
  AddCodePage('KO16KSC5601', 840, ceAnsi, 601);
  AddCodePage('KO16MSWIN949', 846, ceAnsi, zCP_EUCKR);
  AddCodePage('ZHS16CGB231280', 850, ceAnsi, zCP_GB2312);
  AddCodePage('ZHS16GBK', 852, ceAnsi, zCP_GB2312);
  AddCodePage('ZHS32GB18030', 854, ceAnsi, zCP_GB18030);
  AddCodePage('ZHT32EUC', 860, ceAnsi, zCP_EUCKR);
  AddCodePage('ZHT16BIG5', 865, ceAnsi, zCP_Big5);
  AddCodePage('ZHT16MSWIN950', 867, ceAnsi, zCP_Big5);
  AddCodePage('ZHT16HKSCS', 868, ceAnsi);
  //2018-09-28 UTF8 removed by marsupilami79. UTF8 is CESU-8 in reality which cannot
  //be converted by Zeos correctly. For more information see:
  //https://en.wikipedia.org/wiki/CESU-8
  //https://community.oracle.com/thread/351482
  //UTF8 is aliased to AL32UTF8 to mitigate those rare problems.
  AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8, 'AL32UTF8', 4);
  AddCodePage('AL32UTF8', 873, ceUTF8, zCP_UTF8, '', 4);
  AddCodePage('UTF16', OCI_UTF16ID, ceUTF16, zCP_UTF16, '', 2);
//  AddCodePage('AL16UTF16', 2000, ceUTF16, zCP_UTF16BE, '', 4);
//  AddCodePage('AL16UTF16LE', 2002, ceUTF16, zCP_UTF16, '', 4);
end;

procedure TZOraclePlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    @OCIEnvNlsCreate              := GetAddress('OCIEnvNlsCreate');

    @OCIServerAttach              := GetAddress('OCIServerAttach');
    @OCIServerDetach              := GetAddress('OCIServerDetach');
    @OCIServerRelease             := GetAddress('OCIServerRelease');
    @OCIServerVersion             := GetAddress('OCIServerVersion');

    @OCISessionBegin              := GetAddress('OCISessionBegin');
    @OCISessionEnd                := GetAddress('OCISessionEnd');

    @OCITransStart                := GetAddress('OCITransStart');
    @OCITransCommit               := GetAddress('OCITransCommit');
    @OCITransRollback             := GetAddress('OCITransRollback');

    @OCIPing                      := GetAddress('OCIPing');
    @OCIBreak                     := GetAddress('OCIBreak');
    @OCIPasswordChange            := GetAddress('OCIPasswordChange');

    @OCIClientVersion             := GetAddress('OCIClientVersion');

    @OCIHandleAlloc               := GetAddress('OCIHandleAlloc');
    @OCIHandleFree                := GetAddress('OCIHandleFree');
    @OCIErrorGet                  := GetAddress('OCIErrorGet');

    @OCIAttrSet                   := GetAddress('OCIAttrSet');
    @OCIAttrGet                   := GetAddress('OCIAttrGet');
    @OCINlsNumericInfoGet         := GetAddress('OCINlsNumericInfoGet');
    {statement api}
    @OCIStmtPrepare               := GetAddress('OCIStmtPrepare');
    @OCIStmtPrepare2              := GetAddress('OCIStmtPrepare2');
    @OCIStmtRelease               := GetAddress('OCIStmtRelease');
    @OCIStmtExecute               := GetAddress('OCIStmtExecute');
    @OCIParamGet                  := GetAddress('OCIParamGet');
    @OCIStmtFetch                 := GetAddress('OCIStmtFetch');
    @OCIStmtFetch2                := GetAddress('OCIStmtFetch2');

    @OCIDefineByPos               := GetAddress('OCIDefineByPos');
    @OCIBindByPos                 := GetAddress('OCIBindByPos');
    @OCIBindObject                := GetAddress('OCIBindObject');

    @OCIDefineObject              := GetAddress('OCIDefineObject');
    @OCIResultSetToStmt           := GetAddress('OCIResultSetToStmt');
    {descriptors}
    @OCIDescriptorAlloc           := GetAddress('OCIDescriptorAlloc');
    @OCIArrayDescriptorAlloc      := GetAddress('OCIArrayDescriptorAlloc');
    @OCIDescriptorFree            := GetAddress('OCIDescriptorFree');
    @OCIArrayDescriptorFree       := GetAddress('OCIArrayDescriptorFree');
    {Lob}
    @OCILobCopy                   := GetAddress('OCILobCopy');
    @OCILobCopy2                  := GetAddress('OCILobCopy2');
    @OCILobGetChunkSize           := GetAddress('OCILobGetChunkSize');
    @OCILobGetLength              := GetAddress('OCILobGetLength');
    @OCILobGetLength2             := GetAddress('OCILobGetLength2');
    @OCILobLocatorAssign          := GetAddress('OCILobLocatorAssign');
    @OCILobOpen                   := GetAddress('OCILobOpen');
    @OCILobRead                   := GetAddress('OCILobRead');
    @OCILobRead2                  := GetAddress('OCILobRead2');
    @OCILobTrim                   := GetAddress('OCILobTrim');
    @OCILobTrim2                  := GetAddress('OCILobTrim2');
    @OCILobWrite                  := GetAddress('OCILobWrite');
    @OCILobWrite2                 := GetAddress('OCILobWrite2');
    @OCILobCreateTemporary        := GetAddress('OCILobCreateTemporary');
    @OCILobIsEqual                := GetAddress('OCILobIsEqual');
    @OCILobIsOpen                 := GetAddress('OCILobIsOpen');
    @OCILobIsTemporary            := GetAddress('OCILobIsTemporary');
    @OCILobFreeTemporary          := GetAddress('OCILobFreeTemporary');
    @OCILobCharSetForm            := GetAddress('OCILobCharSetForm');
    @OCILobCharSetId              := GetAddress('OCILobCharSetId');
    @OCILobClose                  := GetAddress('OCILobClose');
    {DateTime api}
    @OCIIntervalGetYearMonth      := GetAddress('OCIIntervalGetYearMonth');
    @OCIIntervalGetDaySecond      := GetAddress('OCIIntervalGetDaySecond');
    @OCIDateTimeConstruct         := GetAddress('OCIDateTimeConstruct');
    @OCIDateTimeGetDate           := GetAddress('OCIDateTimeGetDate');
    @OCIDateTimeGetTime           := GetAddress('OCIDateTimeGetTime');
    {object api}
    @OCITypeByRef                 := GetAddress('OCITypeByRef');
    @OCIObjectNew                 := GetAddress('OCIObjectNew');
    @OCIObjectPin                 := GetAddress('OCIObjectPin');
    @OCIObjectUnpin               := GetAddress('OCIObjectUnpin');
    @OCIObjectFree                := GetAddress('OCIObjectFree');
    @OCIObjectGetTypeRef          := GetAddress('OCIObjectGetTypeRef');

    @OCIDescribeAny               := GetAddress('OCIDescribeAny');

    @OCINumberToInt               := GetAddress('OCINumberToInt');
    @OCINumberFromInt             := GetAddress('OCINumberFromInt');
    @OCINumberToReal              := GetAddress('OCINumberToReal');
    @OCINumberFromReal            := GetAddress('OCINumberFromReal');
    @OCINumberToText              := GetAddress('OCINumberToText');

    @OCIStringAllocSize           := GetAddress('OCIStringAllocSize');
    @OCIStringResize              := GetAddress('OCIStringResize');
    @OCIStringPtr                 := GetAddress('OCIStringPtr');
    @OCIStringSize                := GetAddress('OCIStringSize');
  end;
end;

function TZOraclePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZOraclePlainDriver.Create;
end;

constructor TZOraclePlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
  LoadCodePages;
end;

function TZOraclePlainDriver.GetProtocol: string;
begin
  Result := 'oracle';
end;

function TZOraclePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Oracle';
end;

{$ENDIF ZEOS_DISABLE_ORACLE}

end.