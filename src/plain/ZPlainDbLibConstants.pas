unit ZPlainDbLibConstants;

interface

{$I ZPlain.inc}

uses ZCompatibility;

{***************** Plain API Constants definition ****************}
const
{ General  #define }
  TIMEOUT_IGNORE        = Cardinal(-1);
  TIMEOUT_INFINITE      = 0;
  TIMEOUT_MAXIMUM       = 1200; { 20 minutes maximum timeout value }

{ Used for ServerType in dbgetprocinfo }
  SERVTYPE_UNKNOWN      = 0;
  SERVTYPE_MICROSOFT    = 1;

{ Used by dbcolinfo }
{enum CI_TYPES }
  CI_REGULAR            = 1;
  CI_ALTERNATE          = 2;
  CI_CURSOR             = 3;

{ Bulk Copy Definitions (bcp) }
  DB_IN	                = 1;  { Transfer from client to server }
  DB_OUT	              = 2;  { Transfer from server to client }

  BCPMAXERRS            = 1;  { bcp_control parameter }
  BCPFIRST              = 2;  { bcp_control parameter }
  BCPLAST               = 3;  { bcp_control parameter }
  BCPBATCH              = 4;  { bcp_control parameter }
  BCPKEEPNULLS          = 5;  { bcp_control parameter }
  BCPABORT              = 6;  { bcp_control parameter }
  BCPKEEPIDENTITY	      = 8;  { bcp_control parameter }

  BCPLABELED            = 5;  { bcp_control parameter }
  BCPHINTS              = 6;  { bcp_control parameter }

  DBCMDNONE             = 0;  { bcp_control parameter }
  DBCMDPEND             = 1;  { bcp_control parameter }
  DBCMDSENT             = 2;  { bcp_control parameter }

  TINYBIND              = 1;
  SMALLBIND             = 2;
  INTBIND               = 3;
  CHARBIND              = 4;
  BINARYBIND            = 5;
  BITBIND               = 6;
  DATETIMEBIND          = 7;
  MONEYBIND             = 8;
  FLT8BIND              = 9;
  STRINGBIND            = 10;
  NTBSTRINGBIND         = 11;
  VARYCHARBIND          = 12;
  VARYBINBIND           = 13;
  FLT4BIND              = 14;
  SMALLMONEYBIND        = 15;
  SMALLDATETIBIND       = 16;
  DECIMALBIND           = 17;
  NUMERICBIND           = 18;
  SRCDECIMALBIND        = 19;
  SRCNUMERICBIND        = 20;
  MAXBIND               = SRCNUMERICBIND;

  DBSAVE                = 1;
  DBNOSAVE              = 0;

  DBNOERR               = -1;
  DBFAIL                = 0;
  DBSUCCEED             = 1;
  DBFINDONE             = $04;  { Definately done }
  DBMORE                = $10;  { Maybe more commands waiting }
  DBMORE_ROWS           = $20;  { This command returned rows }

  MAXNAME               = 31;
  DBTXTSLEN             = 8;     { Timestamp length }
  DBTXPLEN              = 16;    { Text pointer length }

{ Error code returns }
  INT_EXIT              = 0;
  INT_CONTINUE          = 1;
  INT_CANCEL            = 2;

  //from sybdb.h:
{ DBVERSION_xxx are used with dbsetlversion() }
  DBVERSION_100= 2; // Sybase TDS 5.0
  DBVERSION_42 = 3; // This can be used for old Microsoft and Sybase servers
  DBVERSION_70 = 4;
  DBVERSION_71 = 5;
  DBVERSION_72 = 6;
  DBVERSION_73 = 7;

{ TDS_DBVERSION_xxx are used with dbsetversion() }
  TDSDBVERSION_UNKNOWN  = 0;
  TDSDBVERSION_46       = 1;
  TDSDBVERSION_100      = 2; // Sybase TDS 5.0
  TDSDBVERSION_42       = 3; // This can be used for old Microsoft and Sybase servers
  TDSDBVERSION_70       = 4;
  TDSDBVERSION_71       = 5;
  TDSDBVERSION_80       = TDSDBVERSION_71;
  TDSDBVERSION_72       = 6;
  TDSDBVERSION_73       = 7;

{ these two are defined by Microsoft for dbsetlversion() }
  DBVER42 	            = DBVERSION_42;
  DBVER60 	            = DBVERSION_70;	{ our best approximation }

(**
 * DBTDS_xxx are returned by DBTDS()
 * The integer values of the constants are poorly chosen.
 *)
  DBTDS_UNKNOWN           = 0;
  DBTDS_2_0               = 1;	{ pre 4.0 SQL Server }
  DBTDS_3_4               = 2;	{ Microsoft SQL Server (3.0) }
  DBTDS_4_0               = 3;	{ 4.0 SQL Server }
  DBTDS_4_2               = 4;	{ 4.2 SQL Server }
  DBTDS_4_6               = 5;	{ 2.0 OpenServer and 4.6 SQL Server. }
  DBTDS_4_9_5             = 6;	{ 4.9.5 (NCR) SQL Server }
  DBTDS_5_0               = 7;	{ 5.0 SQL Server }
  DBTDS_7_0               = 8;	{ Microsoft SQL Server 7.0 }
  DBTDS_8_0               = 9;	{ Microsoft SQL Server 2000 }
  DBTDS_9_0               = 10;	{ Microsoft SQL Server 2005 }
  DBTDS_7_1               = 9;	{ Microsoft SQL Server 2000 }
  DBTDS_7_2               = 10;	{ Microsoft SQL Server 2005 }
  DBTDS_7_3               = 11;	{ Microsoft SQL Server 2008 }


{ Data Type Tokens }
  SQLVOID               = $1f;
  SQLTEXT               = $23;
  SQLVARBINARY          = $25;
  SQLINTN               = $26; { all nullable integers }
  SQLVARCHAR            = $27;
  SQLBINARY             = $2d;
  SQLIMAGE              = $22;
  SQLCHAR               = $2f;
  SQLINT1               = $30;
  SQLBIT                = $32;
  SQLINT2               = $34;
  SQLINT4               = $38;
  SQLMONEY              = $3c;
  SQLDATETIME           = $3d;
  SQLFLT8               = $3e;
  SQLFLTN               = $6d;
  SQLMONEYN             = $6e;
  SQLDATETIMN           = $6f;
  SQLFLT4               = $3b;
  SQLMONEY4             = $7a;
  SQLDATETIM4           = $3a;
  SQLDECIMAL            = $6a;
  SQLNUMERIC            = $6c;

  //from tds.h:
  SYBUNIQUE=$24;
  //XSYBVARCHAR=$A7;
  //XSYBNVARCHAR=$E7;
  //XSYBNCHAR = $EF;
  //XSYBBINARY= $AD;


{ Data stream tokens }
  SQLCOLFMT             = $a1;
  OLD_SQLCOLFMT         = $2a;
  SQLPROCID             = $7c;
  SQLCOLNAME            = $a0;
  SQLTABNAME            = $a4;
  SQLCOLINFO            = $a5;
  SQLALTNAME            = $a7;
  SQLALTFMT             = $a8;
  SQLERROR              = $aa;
  SQLINFO               = $ab;
  SQLRETURNVALUE        = $ac;
  SQLRETURNSTATUS       = $79;
  SQLRETURN             = $db;
  SQLCONTROL            = $ae;
  SQLALTCONTROL         = $af;
  SQLROW                = $d1;
  SQLALTROW             = $d3;
  SQLDONE               = $fd;
  SQLDONEPROC           = $fe;
  SQLDONEINPROC         = $ff;
  SQLOFFSET             = $78;
  SQLORDER              = $a9;
  SQLLOGINACK           = $ad; { NOTICE: change to real value }

{ Ag op tokens }
  SQLAOPCNT		= $4b;
  SQLAOPSUM             = $4d;
  SQLAOPAVG             = $4f;
  SQLAOPMIN             = $51;
  SQLAOPMAX             = $52;
  SQLAOPANY             = $53;
  SQLAOPNOOP            = $56;

{ Error numbers (dberrs) DB-Library error codes }
  SQLEMEM               = 10000;
  SQLENULL              = 10001;
  SQLENLOG              = 10002;
  SQLEPWD               = 10003;
  SQLECONN              = 10004;
  SQLEDDNE              = 10005;
  SQLENULLO             = 10006;
  SQLESMSG              = 10007;
  SQLEBTOK              = 10008;
  SQLENSPE              = 10009;
  SQLEREAD              = 10010;
  SQLECNOR              = 10011;
  SQLETSIT              = 10012;
  SQLEPARM              = 10013;
  SQLEAUTN              = 10014;
  SQLECOFL              = 10015;
  SQLERDCN              = 10016;
  SQLEICN               = 10017;
  SQLECLOS              = 10018;
  SQLENTXT              = 10019;
  SQLEDNTI              = 10020;
  SQLETMTD              = 10021;
  SQLEASEC              = 10022;
  SQLENTLL              = 10023;
  SQLETIME              = 10024;
  SQLEWRIT              = 10025;
  SQLEMODE              = 10026;
  SQLEOOB               = 10027;
  SQLEITIM              = 10028;
  SQLEDBPS              = 10029;
  SQLEIOPT              = 10030;
  SQLEASNL              = 10031;
  SQLEASUL              = 10032;
  SQLENPRM              = 10033;
  SQLEDBOP              = 10034;
  SQLENSIP              = 10035;
  SQLECNULL             = 10036;
  SQLESEOF              = 10037;
  SQLERPND              = 10038;
  SQLECSYN              = 10039;
  SQLENONET             = 10040;
  SQLEBTYP              = 10041;
  SQLEABNC              = 10042;
  SQLEABMT              = 10043;
  SQLEABNP              = 10044;
  SQLEBNCR              = 10045;
  SQLEAAMT              = 10046;
  SQLENXID              = 10047;
  SQLEIFNB              = 10048;
  SQLEKBCO              = 10049;
  SQLEBBCI              = 10050;
  SQLEKBCI              = 10051;
  SQLEBCWE              = 10052;
  SQLEBCNN              = 10053;
  SQLEBCOR              = 10054;
  SQLEBCPI              = 10055;
  SQLEBCPN              = 10056;
  SQLEBCPB              = 10057;
  SQLEVDPT              = 10058;
  SQLEBIVI              = 10059;
  SQLEBCBC              = 10060;
  SQLEBCFO              = 10061;
  SQLEBCVH              = 10062;
  SQLEBCUO              = 10063;
  SQLEBUOE              = 10064;
  SQLEBWEF              = 10065;
  SQLEBTMT              = 10066;
  SQLEBEOF              = 10067;
  SQLEBCSI              = 10068;
  SQLEPNUL              = 10069;
  SQLEBSKERR            = 10070;
  SQLEBDIO              = 10071;
  SQLEBCNT              = 10072;
  SQLEMDBP              = 10073;
  SQLINIT               = 10074;
  SQLCRSINV             = 10075;
  SQLCRSCMD             = 10076;
  SQLCRSNOIND           = 10077;
  SQLCRSDIS             = 10078;
  SQLCRSAGR             = 10079;
  SQLCRSORD             = 10080;
  SQLCRSMEM             = 10081;
  SQLCRSBSKEY           = 10082;
  SQLCRSNORES           = 10083;
  SQLCRSVIEW            = 10084;
  SQLCRSBUFR            = 10085;
  SQLCRSFROWN           = 10086;
  SQLCRSBROL            = 10087;
  SQLCRSFRAND           = 10088;
  SQLCRSFLAST           = 10089;
  SQLCRSRO              = 10090;
  SQLCRSTAB             = 10091;
  SQLCRSUPDTAB          = 10092;
  SQLCRSUPDNB           = 10093;
  SQLCRSVIIND           = 10094;
  SQLCRSNOUPD           = 10095;
  SQLCRSOS2             = 10096;
  SQLEBCSA              = 10097;
  SQLEBCRO              = 10098;
  SQLEBCNE              = 10099;
  SQLEBCSK              = 10100;
  SQLEUVBF              = 10101;
  SQLEBIHC              = 10102;
  SQLEBWFF              = 10103;
  SQLNUMVAL             = 10104;
  SQLEOLDVR             = 10105;
  SQLEBCPS	        = 10106;
  SQLEDTC 	        = 10107;
  SQLENOTIMPL	        = 10108;
  SQLENONFLOAT	        = 10109;
  SQLECONNFB            = 10110;

{ The severity levels are defined here }
  EXINFO                = 1;  { Informational, non-error }
  EXUSER                = 2;  { User error }
  EXNONFATAL            = 3;  { Non-fatal error }
  EXCONVERSION          = 4;  { Error in DB-LIBRARY data conversion }
  EXSERVER              = 5;  { The Server has returned an error flag }
  EXTIME                = 6;  { We have exceeded our timeout period while }
                           { waiting for a response from the Server - the }
                           { DBPROCESS is still alive }
  EXPROGRAM             = 7;  { Coding error in user program }
  EXRESOURCE            = 8;  { Running out of resources - the DBPROCESS may be dead }
  EXCOMM                = 9;  { Failure in communication with Server - the DBPROCESS is dead }
  EXFATAL               = 10; { Fatal error - the DBPROCESS is dead }
  EXCONSISTENCY         = 11; { Internal software error  - notify MS Technical Supprt }

{ Offset identifiers }
  OFF_SELECT            = $16d;
  OFF_FROM              = $14f;
  OFF_ORDER             = $165;
  OFF_COMPUTE           = $139;
  OFF_TABLE             = $173;
  OFF_PROCEDURE         = $16a;
  OFF_STATEMENT         = $1cb;
  OFF_PARAM             = $1c4;
  OFF_EXEC              = $12c;

{ Decimal constants }
  MAXNUMERICLEN = 32;
  MAXNUMERICDIG = 38;

  DEFAULTPRECISION = 18;
  DEFAULTSCALE     = 0;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  MAXTABLENAME  = 512+1;
  MAXCOLNAMELEN = 512+1;

{ DB-Library datatype definitions }
  DBMAXCHAR=256; // Max length of DBVARBINARY and DBVARCHAR, etc.

{ Print lengths for certain fixed length data types }
  PRINT4                = 11;
  PRINT2                = 6;
  PRINT1                = 3;
  PRFLT8                = 20;
  PRMONEY               = 26;
  PRBIT                 = 3;
  PRDATETIME            = 27;
  PRDECIMAL             = (MAXNUMERICDIG + 2);
  PRNUMERIC             = (MAXNUMERICDIG + 2);

  SUCCEED               = 1;
  FAIL                  = 0;
  SUCCEED_ABORT         = 2;

  DBUNKNOWN             = 2; { FALSE = 0, TRUE = 1 }

  MORE_ROWS             = -1;
  NO_MORE_ROWS          = -2;
  REG_ROW               = MORE_ROWS;
  BUF_FULL              = -3; { only if buffering is turned on }

{ Status code for dbresults(). Possible return values are }
{ SUCCEED, FAIL, and NO_MORE_RESULTS. }
  NO_MORE_RESULTS       = 2;
  NO_MORE_RPC_RESULTS   = 3;

{ Standard exit and error values }
  STDEXIT               = 0;
  ERREXIT               = -1;

{ dbrpcinit flags }
  DBRPCRECOMPILE        = $0001;
  DBRPCRESET            = $0004;
  DBRPCCURSOR           = $0008;

{ dbrpcparam flags }
  DBRPCRETURN           = $1;
  DBRPCDEFAULT          = $2;

{ Cursor related constants }

{ Following flags are used in the concuropt parameter in the dbcursoropen function }
  CUR_READONLY          = 1; { Read only cursor, no data modifications }
  CUR_LOCKCC            = 2; { Intent to update, all fetched data locked when }
                       { dbcursorfetch is called inside a transaction block }
  CUR_OPTCC             = 3; { Optimistic concurrency control, data modifications }
                       { succeed only if the row hasn't been updated since }
                       { the last fetch. }
  CUR_OPTCCVAL          = 4; { Optimistic concurrency control based on selected column values }

{ Following flags are used in the scrollopt parameter in dbcursoropen }
  CUR_FORWARD           = 0;   { Forward only scrolling }
  CUR_KEYSET            = -1;  { Keyset driven scrolling }
  CUR_DYNAMIC           = 1;   { Fully dynamic }
  CUR_INSENSITIVE       = -2;  { Server-side cursors only }

{ Following flags define the fetchtype in the dbcursorfetch function }
  FETCH_FIRST           = 1;  { Fetch first n rows }
  FETCH_NEXT            = 2;  { Fetch next n rows }
  FETCH_PREV            = 3;  { Fetch previous n rows }
  FETCH_RANDOM          = 4;  { Fetch n rows beginning with given row # }
  FETCH_RELATIVE        = 5;  { Fetch relative to previous fetch row # }
  FETCH_LAST            = 6;  { Fetch the last n rows }

{ Following flags define the per row status as filled by dbcursorfetch and/or dbcursorfetchex }
  FTC_EMPTY             = $00;  { No row available }
  FTC_SUCCEED           = $01;  { Fetch succeeded, (failed if not set) }
  FTC_MISSING           = $02;  { The row is missing }
  FTC_ENDOFKEYSET       = $04;  { End of the keyset reached }
  FTC_ENDOFRESULTS      = $08;  { End of results set reached }

{ Following flags define the operator types for the dbcursor function }
  CRS_UPDATE            = 1;  { Update operation }
  CRS_DELETE            = 2;  { Delete operation }
  CRS_INSERT            = 3;  { Insert operation }
  CRS_REFRESH           = 4;  { Refetch given row }
  CRS_LOCKCC            = 5;  { Lock given row }

{ Following value can be passed to the dbcursorbind function for NOBIND type }
  NOBIND                = -2; { Return length and pointer to data }

{ Following are values used by DBCURSORINFO's Type parameter }
  CU_CLIENT             = $00000001;
  CU_SERVER             = $00000002;
  CU_KEYSET             = $00000004;
  CU_MIXED              = $00000008;
  CU_DYNAMIC            = $00000010;
  CU_FORWARD            = $00000020;
  CU_INSENSITIVE        = $00000040;
  CU_READONLY           = $00000080;
  CU_LOCKCC             = $00000100;
  CU_OPTCC              = $00000200;
  CU_OPTCCVAL           = $00000400;

{ Following are values used by DBCURSORINFO's Status parameter }
  CU_FILLING            = $00000001;
  CU_FILLED             = $00000002;

{ Following are values used by dbupdatetext's type parameter }
  UT_TEXTPTR            = $0001;
  UT_TEXT               = $0002;
  UT_MORETEXT           = $0004;
  UT_DELETEONLY         = $0008;
  UT_LOG                = $0010;

{ The following values are passed to dbserverenum for searching criteria. }
  NET_SEARCH            = $0001;
  LOC_SEARCH            = $0002;

{ These constants are the possible return values from dbserverenum. }
  ENUM_SUCCESS          = $0000;
  MORE_DATA             = $0001;
  NET_NOT_AVAIL         = $0002;
  OUT_OF_MEMORY         = $0004;
  NOT_SUPPORTED         = $0008;
  ENUM_INVALID_PARAM    = $0010;

{ Netlib Error problem codes.  ConnectionError() should return one of }
{ these as the dblib-mapped problem code, so the corresponding string }
{ is sent to the dblib app's error handler as dberrstr.  Return NE_E_NOMAP }
{ for a generic DB-Library error string (as in prior versions of dblib). }

  NE_E_NOMAP            = 0;   { No string; uses dblib default. }
  NE_E_NOMEMORY         = 1;   { Insufficient memory. }
  NE_E_NOACCESS         = 2;   { Access denied. }
  NE_E_CONNBUSY         = 3;   { Connection is busy. }
  NE_E_CONNBROKEN       = 4;   { Connection broken. }
  NE_E_TOOMANYCONN      = 5;   { Connection limit exceeded. }
  NE_E_SERVERNOTFOUND   = 6;   { Specified SQL server not found. }
  NE_E_NETNOTSTARTED    = 7;   { The network has not been started. }
  NE_E_NORESOURCE       = 8;   { Insufficient network resources. }
  NE_E_NETBUSY          = 9;   { Network is busy. }
  NE_E_NONETACCESS      = 10;  { Network access denied. }
  NE_E_GENERAL          = 11;  { General network error.  Check your documentation. }
  NE_E_CONNMODE         = 12;  { Incorrect connection mode. }
  NE_E_NAMENOTFOUND     = 13;  { Name not found in directory service. }
  NE_E_INVALIDCONN      = 14;  { Invalid connection. }
  NE_E_NETDATAERR       = 15;  { Error reading or writing network data. }
  NE_E_TOOMANYFILES     = 16;  { Too many open file handles. }
  NE_E_CANTCONNECT	    = 17;  { SQL Server does not exist or access denied. }

  NE_MAX_NETERROR       = 17;


const
  MAXSERVERNAME = 30;
  MAXNETLIBNAME = 255;
  MAXNETLIBCONNSTR = 255;

const
  INVALID_UROWNUM       = Cardinal(-1);


{ copied from tds.h }
//enum
	SYBCHAR               = $2F;
	SYBVARCHAR            = $27;
	SYBINTN               = $26;
	SYBINT1               = $30;
	SYBINT2               = $34;
	SYBINT4               = $38;
	SYBINT8               = $7F;
	SYBFLT8               = $3E;
	SYBDATETIME           = $3D;
	SYBBIT                = $32;
	SYBBITN               = $68;
	SYBTEXT               = $23;
	SYBNTEXT              = $63;
	SYBIMAGE              = $22;
	SYBMONEY4             = $7A;
	SYBMONEY              = $3C;
	SYBDATETIME4          = $3A;
	SYBREAL               = $3B;
	SYBBINARY             = $2D;
	SYBVOID               = $1F;
	SYBVARBINARY          = $25;
	SYBNUMERIC            = $6C;
	SYBDECIMAL            = $6A;
	SYBFLTN               = $6D;
	SYBMONEYN             = $6E;
	SYBDATETIMN           = $6F;
	SYBNVARCHAR           = $67;

  SYBAOPCNT             = $4b;
  SYBAOPCNTU            = $4c;
  SYBAOPSUM             = $4d;
  SYBAOPSUMU            = $4e;
  SYBAOPAVG             = $4f;
  SYBAOPAVGU            = $50;
  SYBAOPMIN             = $51;
  SYBAOPMAX             = $52;

{ mssql2k compute operator }
  SYBAOPCNT_BIG		      = $09;
  SYBAOPSTDEV		        = $30;
  SYBAOPSTDEVP		      = $31;
  SYBAOPVAR		          = $32;
  SYBAOPVARP		        = $33;
  SYBAOPCHECKSUM_AGG	  = $72;
  {****************** Plain API Types definition *****************}
type
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;
  DBXLATE               = Pointer;
  DBSORTORDER           = Pointer;
  DBLOGINFO             = Pointer;
  DBVOIDPTR             = PPointer;
type
{ DB-Library datatypes }
  DBBOOL                = Byte;
  DBCHAR                = AnsiChar;
  DBBIT                 = Byte;
  DBTINYINT             = Byte;
  DBSMALLINT            = SmallInt; { int16_type }
  DBINT                 = LongInt;  { int32_type }
  DBBIGINT              = Int64;    { int64_type }
  DBBINARY              = Byte;
  DBFLT4                = Single;   { real32_type }
  DBFLT8                = Double;   { real64_type }

  DBSHORT               = SmallInt;
  DBUSMALLINT           = Word;
  DBMONEY4              = LongInt;
  PDBMONEY4             = ^DBMONEY4;

  RETCODE               = Integer;
  PRETCODE              = ^RETCODE;
  STATUS                = Integer;

(*typedef int (*INTFUNCPTR) (void *, ...);
typedef int (*DBWAITFUNC) (void);
typedef DBWAITFUNC(*DB_DBBUSY_FUNC) (void *dbproc);
typedef void (*DB_DBIDLE_FUNC) (DBWAITFUNC dfunc, void *dbproc);
typedef int (*DB_DBCHKINTR_FUNC) (void *dbproc);
typedef int (*DB_DBHNDLINTR_FUNC) (void *dbproc); *)
  DBREAL                = DBFLT4;
  DBUBOOL               = Cardinal;

  DBDATETIM4 = packed record
    numdays:    Word;        { No of days since Jan-1-1900 }
    nummins:    Word;        { No. of minutes since midnight }
  end;
  PDBDATETIM4 = ^DBDATETIM4;

  {$IFDEF FPC}
    {$PACKRECORDS C}
  {$ENDIF}
type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN{$IFNDEF FreeTDS}-1{$ENDIF}] of Byte;
  end;
  DBDECIMAL = DBNUMERIC;

  DBVARYCHAR = packed record
    Len: DBSMALLINT;
    Str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  DBVARYBIN = packed record
    Len: DBSMALLINT;
    Bytes: array[0..DBMAXCHAR-1] of Byte;
  end;

  DBMONEY = packed record
    mnyhigh:    DBINT;
    mnylow:     ULONG;
  end;
  PDBMONEY = ^DBMONEY;



type
{ TODO -ofjanos -cAPI :
Strange but I had to insert X1 and X2 into the structure to make it work.
I have not find any reason for this yet. }
  {$IFDEF FPC}
    {$PACKRECORDS 2}
  {$ENDIF}
  DBCOL = record
   	SizeOfStruct:   DBINT;
   	Name:           array[0..MAXCOLNAMELEN] of char;
   	ActualName:     array[0..MAXCOLNAMELEN] of char;
   	TableName:      array[0..MAXTABLENAME] of char;
    {$IFNDEF FPC}
    X1:             Byte;  //Record-Size diffs with C-Records
    {$ENDIF}
   	Typ:            DBSHORT;
   	UserType:       DBINT;
   	MaxLength:      DBINT;
   	Precision:      BYTE;
   	Scale:          BYTE;
   	VarLength:      LongBool; { TRUE, FALSE }
   	Null:           BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	CaseSensitive:  BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Updatable:      BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Identity:       LongBool; { TRUE, FALSE or DBUNKNOWN }
    {$IFNDEF FPC}
    X2:             Byte; //Record-Size diffs with C-Records
    {$ENDIF}
  end;
  {$IFDEF FPC}
    {$PACKRECORDS DEFAULT}
  {$ENDIF}
  PDBCOL = ^DBCOL;


type
  DBTYPEINFO = packed record
    Precision:  DBINT;
	  Scale:      DBINT;
  end;
  PDBTYPEINFO = ^DBTYPEINFO;

  DBPROC_INFO = packed record
    SizeOfStruct:       DBINT;
    ServerType:         Byte;
    ServerMajor:        Word;
    ServerMinor:        Word;
    ServerRevision:     Word;
    ServerName:         array[0..MAXSERVERNAME] of AnsiChar;
    NetLibName:         array[0..MAXNETLIBNAME] of AnsiChar;
    NetLibConnStr:      array[0..MAXNETLIBCONNSTR] of AnsiChar;
  end;
  PDBPROCINFO = ^DBPROC_INFO;

  DBCURSOR_INFO = packed record
    SizeOfStruct:       DBINT;    { Use sizeof(DBCURSORINFO) }
    TotCols:            Cardinal; { Total Columns in cursor }
    TotRows:            Cardinal; { Total Rows in cursor }
    CurRow:             Cardinal; { Current actual row in server }
    TotRowsFetched:     Cardinal; { Total rows actually fetched }
    CurType:            Cardinal; { See CU_... }
    Status:             Cardinal; { See CU_... }
  end;
  PDBCURSORINFO = ^DBCURSOR_INFO;

type
{ Pointer Datatypes }
  PDBINT        = ^DBINT;
  PDBBINARY     = ^DBBINARY;

type
  PDBLibError = ^TDBLibError;
  TDBLibError = record
    dbProc: PDBPROCESS;
    Severity: DBINT;
    DbErr: DBINT;
    OsErr: DBINT;
    DbErrStr: AnsiString;
    OsErrStr: AnsiString;
  end;

  PDBLibMessage = ^TDBLibMessage;
  TDBLibMessage = record
    dbProc: PDBPROCESS;
    MsgNo: DBINT;
    MsgState: DBINT;
    Severity: DBINT;
    MsgText: AnsiString;
    SrvName: AnsiString;
    ProcName: AnsiString;
    Line: DBUSMALLINT;
  end;

  col_t = record end;

  DBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
  DBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; cdecl;

  Tdberrhandle = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
  Tdbmsghandle = function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; cdecl;

  Tdbprocerrhandle = function(DbHandle: PDBHANDLE; Handler: DBERRHANDLE_PROC):
    DBERRHANDLE_PROC; cdecl;
  Tdbprocmsghandle = function(DbHandle: PDBHANDLE; Handler: DBMSGHANDLE_PROC):
    DBMSGHANDLE_PROC; cdecl;

implementation

end.
