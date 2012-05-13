unit ZPlainFreeTDSConstants;

interface

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
  DB_OUT	        = 2;  { Transfer from server to client }

  BCPMAXERRS            = 1;  { bcp_control parameter }
  BCPFIRST              = 2;  { bcp_control parameter }
  BCPLAST               = 3;  { bcp_control parameter }
  BCPBATCH              = 4;  { bcp_control parameter }
  BCPKEEPNULLS          = 5;  { bcp_control parameter }
  BCPABORT              = 6;  { bcp_control parameter }

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

{ DBTDS_xxx are returned by DBTDS() }
  DBTDS_UNKNOWN= 0;
  DBTDS_42     = 4;  // SQL Server 4.2
  DBTDS_50     = 7;	 // Sybase SQL Server 5.0; use this for connecting to Sybase (ASA or ASE)
  DBTDS_70     = 8;	 // Microsoft SQL Server 7.0
  DBTDS_71     = 9;  // Microsoft SQL Server 2000
  DBTDS_72     = 10; // Microsoft SQL Server 2005
  DBTDS_73     = 11; // Microsoft SQL Server 2008

  //from FreeTDS.sybdb.h
  DBSETHOST     = 1;
  DBSETUSER     = 2;
  DBSETPWD      = 3;
  DBSETHID      = 4; //not implemented
  DBSETAPP      = 5;
  DBSETBCP      = 6;
  DBSETNATLANG  = 7;
  DBSETNOSHORT  = 8; //not implemented
  DBSETHIER     = 9; //not implemented
  DBSETCHARSET  = 10;
  DBSETPACKET   = 11;
  DBSETENCRYPT  = 12;
  DBSETLABELED  = 13;
  DBSETDBNAME   = 14;

  //These two are defined by Microsoft for dbsetlversion():
  DBVER42=  DBVERSION_42;
  DBVER60=  DBVERSION_71;
  DBSET_LOGINTIME=10;
  DBSETFALLBACK=12;

{ dboptions }
  DBBUFFER              = 0;
  DBOFFSET              = 1;
  DBROWCOUNT            = 2;
  DBSTAT                = 3;
  DBTEXTLIMIT           = 7;
  DBTEXTSIZE            = 17;
  DBARITHABORT          = 6;
  DBARITHIGNORE         = 7;
  DBNOAUTOFREE          = 15;
  DBNOCOUNT             = 9;
  DBNOEXEC              = 10;
  DBPARSEONLY           = 11;
  DBSHOWPLAN            = 12;
  DBSTORPROCID		      = 13;
  DBANSITOOEM		        = 14;
  DBOEMTOANSI	          = 15;
  DBCLIENTCURSORS       = 16;
  DBSET_TIME            = 17;
  DBQUOTEDIDENT         = 35;

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
  SYBNTEXT=$63;
  SYBINT8=$7F;
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
  NE_E_CANTCONNECT	= 17;  { SQL Server does not exist or access denied. }

  NE_MAX_NETERROR       = 17;


const
  MAXSERVERNAME = 30;
  MAXNETLIBNAME = 255;
  MAXNETLIBCONNSTR = 255;

const
  INVALID_UROWNUM       = Cardinal(-1);


  {****************** Plain API Types definition *****************}
type
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;

type
  RETCODE               = Integer;
  PRETCODE              = ^RETCODE;
  STATUS                = Integer;

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
  DBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	ULONG;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;
  PDBDATETIME = ^DBDATETIME;

{ DBDATEREC structure used by dbdatecrack }
  DBDATEREC = packed record
    case boolean of
    false:(
      oldyear:        DBINT; { 1753 - 9999 }
      oldmonth:       DBINT; { 1 - 4 }
      oldday:         DBINT; { 1 - 12 }
      olddayofyear:   DBINT; { 1 - 366 (in sybdb.h dayofyear and day are changed around!) }
      oldweekday:     DBINT; { 1 - 7  (Mon - Sun) }
      oldhour:        DBINT; { 0 - 23 }
      oldminute:      DBINT; { 0 - 59 }
      oldsecond:      DBINT; { 0 - 59 }
      oldmillisecond: DBINT; { 0 - 999 }
      oldtzone:       DBINT; { 0 - 127 (Sybase only!) }
    );
    true:(
      year:           DBINT; { 1753 - 9999 }
      quarter:        DBINT; { 1 - 4 }
      month:          DBINT; { 1 - 12 }
      day:            DBINT; { 1 - 31 }
      dayofyear:      DBINT; { 1 - 366 (in sybdb.h dayofyear and day are changed around!) }
      week:           DBINT; { 1 - 54 (for leap years) }
      weekday:        DBINT; { 1 - 7  (Mon - Sun) }
      hour:           DBINT; { 0 - 23 }
      minute:         DBINT; { 0 - 59 }
      second:         DBINT; { 0 - 59 }
      millisecond:    DBINT; { 0 - 999 }
      tzone:          DBINT; { 0 - 127 (Sybase only!) }
    );
  end;
  PDBDATEREC = ^DBDATEREC;

type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;
  DBDECIMAL = DBNUMERIC;

  DBVARYCHAR = packed record
    Len: {$IFDEF FREETDS}DBINT{$ELSE}DBSMALLINT{$ENDIF};
    Str: array[0..DBMAXCHAR-1] of DBCHAR; //CHAR = Wide D12UP
  end;

  DBVARYBIN = packed record
    Len: {$IFDEF FREETDS}DBINT{$ELSE}DBSMALLINT{$ENDIF};
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
  DBMSGHANDLE_FUNC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; cdecl;

  Tdbadata        = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
  Tdbadlen        = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltbind      = function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbaltbind_ps   = function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE;
  Tdbaltcolid     = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltlen       = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltop        = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbalttype      = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltutype     = function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbanullbind    = function(dbproc: PDBPROCESS; ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; cdecl;
  Tdbbind         = function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; cdecl;
  Tdbbind_ps      = function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
  Tdbbufsize      = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbbylist       = function(dbproc: PDBPROCESS; ComputeId: Integer; Size: PInteger): PByte; cdecl;
  Tdbcancel       = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbcanquery     = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbchange       = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbclose        = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbclrbuf       = procedure(dbproc: PDBPROCESS; N: DBINT); cdecl;
  Tdbclropt       = function(dbproc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; cdecl;
  Tdbcmd          = function(dbproc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
  Tdbcmdrow       = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbtablecolinfo = function(dbproc: PDBPROCESS; Column: DBINT; pdbcol: DBCOL): RETCODE;
  Tdbcolinfo      = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer; DbColumn: PDBCOL): RETCODE; cdecl;
  Tdbcollen       = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcolname      = function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcolsource    = function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcoltype      = function(dbproc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbcolutype     = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbconvert      = function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
  Tdbconvert_ps   = function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; cdecl;
  Tdbiscount      = function(dbproc: PDBPROCESS): DBBOOL;
  Tdbcount        = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbcurcmd       = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbcurrow       = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbdata         = function(dbproc: PDBPROCESS; Column: Integer): PByte; cdecl;
  Tdbdatecmp      = function(dbproc: PDBPROCESS; d1, d2: PDBDATETIME): Integer;
  Tdbdatecrack    = function(dbproc: PDBPROCESS; DateInfo: PDBDATEREC; DateType: PDBDATETIME): RETCODE; cdecl;
  Tdbdatlen       = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbdead         = function(dbproc: PDBPROCESS): LongBool; cdecl;
  Tdberrhandle    = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
  Tdbexit         = procedure(); cdecl;
  Tdbfcmd         = function(dbproc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
  Tdbfirstrow     = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbfreebuf      = procedure(dbproc: PDBPROCESS); cdecl;
  Tdbgetchar      = function(dbproc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
  Tdbgetcharset   = function(dbproc: PDBPROCESS): PAnsiChar;
  Tdbgetlusername = function(login: PLOGINREC; name_buffer: PByte; buffer_len: Integer): Integer; cdecl;
  Tdbgetmaxprocs  = function(): Integer; cdecl;
  Tdbgetnatlanf   = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbgetpacket    = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbgetrow       = function(dbproc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbgettime      = function(): Integer; cdecl;
  Tdbgetuserdata  = function(dbproc: PDBPROCESS): PByte; cdecl;
  Tdbhasretstat   = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  Tdbinit         = function():RETCODE; cdecl;
  Tdbiordesc      = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbiowdesc      = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbisavail      = function(Proc: PDBPROCESS): DBBOOL; cdecl;
  Tdbisopt        = function(Proc: PDBPROCESS; Option: Integer; const Param: PAnsiChar): DBBOOL; cdecl;
  Tdblastrow      = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdblogin        = function(): PLOGINREC; cdecl;
  Tdbloginfree    = procedure(login: PLOGINREC); cdecl;
  Tdbmny4cmp      = function(dbproc: PDBPROCESS; m1, m: PDBMONEY4): Integer; cdecl;
  Tdbmnycmp       = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY): Integer; cdecl;
  Tdbmny4add      = function(dbproc: PDBPROCESS; m1, m2, sum: PDBMONEY4): RETCODE; cdecl;
  Tdbmnydec       = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  Tdbmnyinc       = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  Tdbmnymaxpos    = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  Tdbmnymaxneg    = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  Tdbmny4minus    = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  Tdbmnyminus     = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  Tdbmny4sub      = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY4): RETCODE; cdecl;
  Tdbmnysub       = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY): RETCODE; cdecl;
  Tdbmny4copy     = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY4): RETCODE; cdecl;
  Tdbmnycopy      = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  Tdbmny4zero     = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  Tdbmnyzero      = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  Tdbmonthname    = function(dbproc: PDBPROCESS; language: PAnsiChar; monthnum: Integer; shortform: DBBOOL): PAnsiChar; cdecl;
  Tdbmorecmds     = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbmoretext     = function(dbproc: PDBPROCESS; Size: DBINT; const Text: PByte): RETCODE; cdecl;
  Tdbmsghandle    = function(Handler: DBMSGHANDLE_FUNC): DBMSGHANDLE_FUNC; cdecl;
  Tdbname         = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbnextrow      = function(dbproc: PDBPROCESS): STATUS; cdecl;
  Tdbnullbind     = function(dbproc: PDBPROCESS; Column: Integer; Indicator: PDBINT): RETCODE; cdecl;
  Tdbnumalts      = function(dbproc: PDBPROCESS; ComputeId: Integer): Integer; cdecl;
  Tdbnumcols      = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbnumcompute   = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbnumorders    = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbnumrets      = function(dbproc: PDBPROCESS): Integer; cdecl;
  Ttdsdbopen      = function(Login: PLOGINREC; const Server: PAnsiChar; msdblib: Integer): PDBPROCESS;
  Tdbopen         = function(Login: PLOGINREC; const Server: PAnsiChar): PDBPROCESS;

{ pivot functions
void dbpivot_count (struct col_t *output, const struct col_t *input);
void dbpivot_sum (struct col_t *output, const struct col_t *input);
void dbpivot_min (struct col_t *output, const struct col_t *input);
void dbpivot_max (struct col_t *output, const struct col_t *input);

struct pivot_t;
typedef void (*DBPIVOT_FUNC)(struct col_t *output, const struct col_t *input);
struct pivot_t * dbrows_pivoted(DBPROCESS *dbproc);
STATUS dbnextrow_pivoted(DBPROCESS *dbproc, struct pivot_t *pp);
RETCODE dbpivot(DBPROCESS *dbproc, int nkeys, int *keys, int ncols, int *cols, DBPIVOT_FUNC func, int val);

DBPIVOT_FUNC dbpivot_lookup_name( const char name[] );
}
  Tdbprhead       = procedure(dbproc: PDBPROCESS); cdecl;
  Tdbprrow        = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbprtype       = function(Token: Integer): PAnsiChar; cdecl;
  TDRBUF          = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  Tdbreadtext     = function(dbproc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; cdecl;
  Tdbrecftos      = procedure(const FileName: PAnsiChar);
  Tdbresults      = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbresults_r    = function(dbproc: PDBPROCESS; Recursive: Integer): RETCODE; cdecl;
  Tdbretdata      = function(dbproc: PDBPROCESS; RetNum: Integer): PByte; cdecl;
  Tdbretlen       = function(dbproc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
  Tdbretname      = function(dbproc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
  Tdbretstatus    = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbrettype      = function(dbproc: PDBPROCESS; RetNum: Integer): Integer; cdecl;
  Tdbrows         = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbrowtype      = function(dbproc: PDBPROCESS): STATUS; cdecl;
  Tdbrpcinit      = function(dbproc: PDBPROCESS; ProcName: PAnsiChar; Options: DBSMALLINT): RETCODE; cdecl;
  Tdbrpcparam     = function(dbproc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte; Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; cdecl;
  Tdbrpcsend      = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbsafestr      = function(dbproc: PDBPROCESS; const Src: PAnsiChar; SrcLen: DBINT; Dest: PAnsiChar; DestLen: DBINT; QuoteType: integer): RETCODE; cdecl;
  //Tdbsechandle    = function(_Type: DBINT ; Handler: INTFUNCPTR): PRETCODE; cdecl;
  Tdbservcharset  = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbsetavail     = procedure(dbproc: PDBPROCESS); cdecl;
  //Tdbsetbusy      = procedure(dbproc: PDBPROCESS; BusyFunc: DB_DBBUSY_FUNC);  cdecl;
  Tdbsetdefcharset= function(Charset: PAnsiChar): RETCODE; cdecl;
  Tdbsetifile     = procedure(FileName: PAnsiChar); cdecl;
  //Tdbsetinterrupt = procedure(dbproc: PDBPROCESS; chkintr: DB_DBCHKINTR_FUNC; hndlintr: DB_DBHNDLINTR_FUNC);
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetmaxprocs  = function(MaxProcs: SmallInt): RETCODE; cdecl;
  Tdbsetlname     = function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; cdecl;
  Tdbsetnull      = function(dbproc: PDBPROCESS; BindType, BindLen: Integer; BindVal: PByte): RETCODE; cdecl;
  Tdbsetopt       = function(dbproc: PDBPROCESS; Option: DBINT; Param: PAnsiChar; int_param: DBINT): RETCODE; cdecl;
  Tdbsetrow       = function(dbproc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbsettime      = function(seconds: DBINT):RETCODE; cdecl;
  Tdbsetuserdata  = procedure(dbproc: PDBPROCESS; Ptr: Pointer); cdecl;
  Tdbsetversion   = function(Version: DBINT): RETCODE; cdecl;
  Tdbspid         = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbspr1row      = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  Tdbspr1rowlen   = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbsprhead      = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  Tdbsprline      = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT; line_char: DBCHAR): RETCODE; cdecl;
  Tdbsqlexec      = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlok        = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlsend      = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbstrcpy       = function(dbproc: PDBPROCESS; Start, NumBytes: Integer; Dest: PAnsiChar): RETCODE; cdecl;
  Tdbstrlen       = function(dbproc: PDBPROCESS): Integer; cdecl;
  Tdbvarylen      = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;

  Tdbtds          = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbtextsize     = function(dbproc: PDBPROCESS): DBINT; cdecl;
  Tdbtxptr        = function(dbproc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtimestamp  = function(dbproc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtsnewval   = function(dbproc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtxtsput      = function(dbproc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer): RETCODE; cdecl;
  Tdbuse          = function(dbproc: PDBPROCESS; DbName: PAnsiChar): RETCODE; cdecl;
  Tdbwillconvert  = function(SrcType, DestType: Integer): LongBool; cdecl;
  Tdbwritetext    = function(Proc: PDBPROCESS; ObjName: PAnsiChar; TextPtr: PDBBINARY; TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: DBBOOL; Size: DBINT; Text: PByte): RETCODE; cdecl;

  (* LOGINREC manipulation *)
  Tdbsetlbool     = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  Tdbsetllong     = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  Tdbsetlversion  = function(Login: PLOGINREC; Version: Byte): RETCODE; cdecl;


TFreeTDSAPI = Record
  dbadata:        Tdbadata;
  dbadlen:        Tdbadlen;
  dbaltbind:      Tdbaltbind;
  dbaltbind_ps:   Tdbaltbind_ps;
  dbaltcolid:     Tdbaltcolid;
  dbaltlen:       Tdbaltlen;
  dbaltop:        Tdbaltop;
  dbalttype:      Tdbalttype;
  dbaltutype:     Tdbaltutype;
  dbanullbind:    Tdbanullbind;
  dbbind:         Tdbbind;
  dbbind_ps:      Tdbbind_ps;
  dbbufsize:      Tdbbufsize;
  dbbylist:       Tdbbylist;
  dbcancel:       Tdbcancel;
  dbcanquery:     Tdbcanquery;
  dbchange:       Tdbchange;
  dbclose:        Tdbclose;
  dbclrbuf:       Tdbclrbuf;
  dbclropt:       Tdbclropt;
  dbcmd:          Tdbcmd;
  dbcmdrow:       Tdbcmdrow;
  dbtablecolinfo: Tdbtablecolinfo;
  dbcolinfo:      Tdbcolinfo;
  dbcollen:       Tdbcollen;
  dbcolname:      Tdbcolname;
  dbcolsource:    Tdbcolsource;
  dbcoltype:      Tdbcoltype;
  dbcolutype:     Tdbcolutype;
  dbconvert:      Tdbconvert;
  dbconvert_ps:   Tdbconvert_ps;
  dbiscount:      Tdbiscount;
  dbcount:        Tdbcount;
  dbcurcmd:       Tdbcurcmd;
  dbcurrow:       Tdbcurrow;
  dbdata:         Tdbdata;
  dbdatecmp:      Tdbdatecmp;
  dbdatecrack:    Tdbdatecrack;
  dbdatlen:       Tdbdatlen;
  dbdead:         Tdbdead;
  dberrhandle:    Tdberrhandle;
  dbexit:         Tdbexit;
  dbfcmd:         Tdbfcmd;
  dbfirstrow:     Tdbfirstrow;
  dbfreebuf:      Tdbfreebuf;
  dbgetchar:      Tdbgetchar;
  dbgetcharset:   Tdbgetcharset;
  dbgetlusername: Tdbgetlusername;
  dbgetmaxprocs:  Tdbgetmaxprocs;
  dbgetnatlanf:   Tdbgetnatlanf;
  dbgetpacket:    Tdbgetpacket;
  dbgetrow:       Tdbgetrow;
  dbgettime:      Tdbgettime;
  dbgetuserdata:  Tdbgetuserdata;
  dbhasretstat:   Tdbhasretstat;
  dbinit:         Tdbinit;
  dbiordesc:      Tdbiordesc;
  dbiowdesc:      Tdbiowdesc;
  dbisavail:      Tdbisavail;
  dbisopt:        Tdbisopt;
  dblastrow:      Tdblastrow;
  dblogin:        Tdblogin;
  dbloginfree:    Tdbloginfree;
  dbmny4cmp:      Tdbmny4cmp;
  dbmnycmp:       Tdbmnycmp;
  dbmny4add:      Tdbmny4add;
  dbmnydec:       Tdbmnydec;
  dbmnyinc:       Tdbmnyinc;
  dbmnymaxpos:    Tdbmnymaxpos;
  dbmnymaxneg:    Tdbmnymaxneg;
  dbmny4minus:    Tdbmny4minus;
  dbmnyminus:     Tdbmnyminus;
  dbmny4sub:      Tdbmny4sub;
  dbmnysub:       Tdbmnysub;
  dbmny4copy:     Tdbmny4copy;
  dbmnycopy:      Tdbmnycopy;
  dbmny4zero:     Tdbmny4zero;
  dbmnyzero:      Tdbmnyzero;
  dbmonthname:    Tdbmonthname;
  dbmorecmds:     Tdbmorecmds;
  dbmoretext:     Tdbmoretext;
  dbmsghandle:    Tdbmsghandle;
  dbname:         Tdbname;
  dbnextrow:      Tdbnextrow;
  dbnullbind:     Tdbnullbind;
  dbnumalts:      Tdbnumalts;
  dbnumcols:      Tdbnumcols;
  dbnumcompute:   Tdbnumcompute;
  dbnumorders:    Tdbnumorders;
  dbnumrets:      Tdbnumrets;
  tdsdbopen:      Ttdsdbopen;
  dbopen:         Tdbopen;

{ pivot functions */
void dbpivot_count (struct col_t *output, const struct col_t *input);
void dbpivot_sum (struct col_t *output, const struct col_t *input);
void dbpivot_min (struct col_t *output, const struct col_t *input);
void dbpivot_max (struct col_t *output, const struct col_t *input);

struct pivot_t;
typedef void (*DBPIVOT_FUNC)(struct col_t *output, const struct col_t *input);
struct pivot_t * dbrows_pivoted(DBPROCESS *dbproc);
STATUS dbnextrow_pivoted(DBPROCESS *dbproc, struct pivot_t *pp);
RETCODE dbpivot(DBPROCESS *dbproc, int nkeys, int *keys, int ncols, int *cols, DBPIVOT_FUNC func, int val);

DBPIVOT_FUNC dbpivot_lookup_name( const char name[] );
}
  dbprhead:       Tdbprhead;
  dbprrow:        Tdbprrow;
  dbprtype:       Tdbprtype;
  DRBUF:          TDRBUF;
  dbreadtext:     Tdbreadtext;
  dbrecftos:      Tdbrecftos;
  dbresults:      Tdbresults;
  dbresults_r:    Tdbresults_r;
  dbretdata:      Tdbretdata;
  dbretlen:       Tdbretlen;
  dbretname:      Tdbretname;
  dbretstatus:    Tdbretstatus;
  dbrettype:      Tdbrettype;
  dbrows:         Tdbrows;
  dbrowtype:      Tdbrowtype;
  dbrpcinit:      Tdbrpcinit;
  dbrpcparam:     Tdbrpcparam;
  dbrpcsend:      Tdbrpcsend;
  dbsafestr:      Tdbsafestr;
  //dbsechandle:    Tdbsechandle;
  dbservcharset:  Tdbservcharset;
  dbsetavail:     Tdbsetavail;
  //dbsetbusy:      Tdbsetbusy;
  dbsetdefcharset:Tdbsetdefcharset;
  dbsetifile:     Tdbsetifile;
  //dbsetinterrupt: Tdbsetinterrupt;
  dbsetlogintime: Tdbsetlogintime;
  dbsetmaxprocs:  Tdbsetmaxprocs;
  dbsetlname:     Tdbsetlname;
  dbsetnull:      Tdbsetnull;
  dbsetopt:       Tdbsetopt;
  dbsetrow:       Tdbsetrow;
  dbsettime:      Tdbsettime;
  dbsetuserdata:  Tdbsetuserdata;
  dbsetversion:   Tdbsetversion;
  dbspid:         Tdbspid;
  dbspr1row:      Tdbspr1row;
  dbspr1rowlen:   Tdbspr1rowlen;
  dbsprhead:      Tdbsprhead;
  dbsprline:      Tdbsprline;
  dbsqlexec:      Tdbsqlexec;
  dbsqlok:        Tdbsqlok;
  dbsqlsend:      Tdbsqlsend;
  dbstrcpy:       Tdbstrcpy;
  dbstrlen:       Tdbstrlen;
  dbvarylen:      Tdbvarylen;

  dbtds:          Tdbtds;
  dbtextsize:     Tdbtextsize;
  dbtxptr:        Tdbtxptr;
  dbtxtimestamp:  Tdbtxtimestamp;
  dbtxtsnewval:   Tdbtxtsnewval;
  dbtxtsput:      Tdbtxtsput;
  dbuse:          Tdbuse;
  dbwillconvert:  Tdbwillconvert;
  dbwritetext:    Tdbwritetext;

  (* LOGINREC manipulation *)
  dbsetlbool:     Tdbsetlbool;
  dbsetllong:     Tdbsetllong;
  dbsetlversion:  Tdbsetlversion;
End;

implementation

end.
