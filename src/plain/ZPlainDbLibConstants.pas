{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain driver interface to DBLibrary         }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{         FreeTDS supportd by Bogdan Dragulin             }
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

unit ZPlainDbLibConstants;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses // M.A.
   ZCompatibility; // M.A.
   
{***************** Plain API Constants definition ****************}
const
{ General  #define }
  TIMEOUT_IGNORE        = Cardinal(-1);
  TIMEOUT_INFINITE      = 0;
  TIMEOUT_MAXIMUM       = 1200*1000; { 20 minutes maximum timeout value in ms}

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

  //from FreeTDS sybdb.h:
{ DBVERSION_xxx are used with dbsetlversion() }
  DBVERSION_100= 2; // Sybase TDS 5.0
  DBVERSION_42 = 3; // This can be used for old Microsoft and Sybase servers
  DBVERSION_70 = 4;
  DBVERSION_71 = 5;
  DBVERSION_72 = 6;
  DBVERSION_73 = 7;

{Zeos dbsetversion placeholders}
  ZVersion_UNKNOWN           = 0;
  ZVersion_2_0               = 1;	{ pre 4.0 SQL Server }
  ZVersion_3_4               = 2;	{ Microsoft SQL Server (3.0) }
  ZVersion_4_0               = 3;	{ 4.0 SQL Server }
  ZVersion_4_2               = 4;	{ 4.2 SQL Server }
  ZVersion_4_6               = 5;	{ 2.0 OpenServer and 4.6 SQL Server. }
  ZVersion_4_9_5             = 6;	{ 4.9.5 (NCR) SQL Server }
  ZVersion_5_0               = 7;	{ 5.0 SQL Server }
  ZVersion_7_0               = 8;	{ Microsoft SQL Server 7.0 }
  ZVersion_8_0               = 9;	{ Microsoft SQL Server 2000 }
  ZVersion_9_0               = 10;	{ Microsoft SQL Server 2005 }
  ZVersion_7_1               = 9;	{ Microsoft SQL Server 2000 }
  ZVersion_7_2               = 10;	{ Microsoft SQL Server 2005 }
  ZVersion_7_3               = 11;	{ Microsoft SQL Server 2008 }

  ZVersionMax                = 13; { known count of available versions }
  ZVersionEmpty              = -1; { placeholder for unsuported version }

{ DB-Library datatypes }
const
{Zeos DBOption placeholders}
{ a large list of options, DBTEXTSIZE is needed by sybtcl }
  Z_PARSEONLY             = 0;
  Z_ESTIMATE              = 1;
  Z_SHOWPLAN              = 2;
  Z_NOEXEC                = 3;
  Z_ARITHIGNORE           = 4;
  Z_NOCOUNT               = 5;
  Z_ARITHABORT            = 6;
  Z_TEXTLIMIT             = 7;
  Z_BROWSE                = 8;
  Z_OFFSET                = 9;
  Z_STAT                  = 10;
  Z_ERRLVL                = 11;
  Z_CONFIRM               = 12;
  Z_STORPROCID            = 13;
  Z_BUFFER                = 14;
  Z_NOAUTOFREE            = 15;
  Z_ROWCOUNT              = 16;
  Z_TEXTSIZE              = 17;
  Z_NATLANG               = 18;
  Z_DATEFORMAT            = 19;
  Z_PRPAD                 = 20;
  Z_PRCOLSEP              = 21;
  Z_PRLINELEN             = 22;
  Z_PRLINESEP             = 23;
  Z_LFCONVERT             = 24;
  Z_DATEFIRST             = 25;
  Z_CHAINXACTS            = 26;
  Z_FIPSFLAG              = 27;
  Z_ISOLATION             = 28;
  Z_AUTH                  = 29;
  Z_IDENTITY              = 30;
  Z_NOIDCOL               = 31;
  Z_DATESHORT             = 32;
  Z_CLIENTCURSORS         = 33;
  Z_SETTIME               = 34;
  Z_QUOTEDIDENT           = 35;
  Z_NUMOPTIONS            = 36;
  Z_PADOFF                = 37;
  Z_PADON                 = 38;
  Z_OFF                   = 39;
  Z_ON                    = 40;
  Z_NOSUCHOPTION          = 41;
  Z_MAXOPTTEXT            = 42;
  Z_ANSITOOEM             = 43;
  Z_OEMTOANSI             = 44;

type TdbOption = ({Zeos DBOption placeholders}
{ a large list of options, DBTEXTSIZE is needed by sybtcl }
  dboptPARSEONLY             = 0,
  dboptESTIMATE              = 1,
  dboptSHOWPLAN              = 2,
  dboptNOEXEC                = 3,
  dboptARITHIGNORE           = 4,
  dboptNOCOUNT               = 5,
  dboptARITHABORT            = 6,
  dboptTEXTLIMIT             = 7,
  dboptBROWSE                = 8,
  dboptOFFSET                = 9,
  dboptSTAT                  = 10,
  dboptERRLVL                = 11,
  dboptCONFIRM               = 12,
  dboptSTORPROCID            = 13,
  dboptBUFFER                = 14,
  dboptNOAUTOFREE            = 15,
  dboptROWCOUNT              = 16,
  dboptTEXTSIZE              = 17,
  dboptNATLANG               = 18,
  dboptDATEFORMAT            = 19,
  dboptPRPAD                 = 20,
  dboptPRCOLSEP              = 21,
  dboptPRLINELEN             = 22,
  dboptPRLINESEP             = 23,
  dboptLFCONVERT             = 24,
  dboptDATEFIRST             = 25,
  dboptCHAINXACTS            = 26,
  dboptFIPSFLAG              = 27,
  dboptISOLATION             = 28,
  dboptAUTH                  = 29,
  dboptIDENTITY              = 30,
  dboptNOIDCOL               = 31,
  dboptDATESHORT             = 32,
  dboptCLIENTCURSORS         = 33,
  dboptSETTIME               = 34,
  dboptQUOTEDIDENT           = 35,
  dboptANSITOOEM             = 36,
  dboptOEMTOANSI             = 37
);
const
{ loginrec manipulation Placeholders}
  Z_SETHOST               = 0;
  Z_SETUSER               = 1;
  Z_SETPWD                = 2;
  Z_SETHID                = 3;
  Z_SETAPP                = 4;
  Z_SETBCP                = 5;
  Z_SETSECURE             = 6;
  Z_SETLANG               = 7;
  Z_SETNOSHORT            = 8;
  Z_SETHIER               = 9;
  Z_SETCHARSET            = 10;
  Z_SETPACKET             = 11;
  Z_SETENCRYPT            = 12;
  Z_SETLABELED            = 13;
  Z_SETDBNAME             = 14;
  Z_SETLOGINTIME          = 15;
  Z_SETFALLBACK           = 16;

{ DBLib options }
const
  DBLIBDBBUFFER               = 0;
  DBLIBDBOFFSET               = 1;
  DBLIBDBROWCOUNT             = 2;
  DBLIBDBSTAT                 = 3;
  DBLIBDBTEXTLIMIT            = 4;
  DBLIBDBTEXTSIZE             = 5;
  DBLIBDBARITHABORT           = 6;
  DBLIBDBARITHIGNORE          = 7;
  DBLIBDBNOAUTOFREE           = 8;
  DBLIBDBNOCOUNT              = 9;
  DBLIBDBNOEXEC               = 10;
  DBLIBDBPARSEONLY            = 11;
  DBLIBDBSHOWPLAN             = 12;
  DBLIBDBSTORPROCID           = 13;
  DBLIBDBANSITOOEM		        = 14;
  DBLIBDBOEMTOANSI	          = 15;
  DBLIBDBCLIENTCURSORS        = 16;
  DBLIBDBSET_TIME             = 17;
  DBLIBDBQUOTEDIDENT          = 18;

{ FreeTDS options, a large list of options, DBTEXTSIZE is needed by sybtcl }
  TDSPARSEONLY             = 0;
  TDSESTIMATE              = 1;
  TDSSHOWPLAN              = 2;
  TDSNOEXEC                = 3;
  TDSARITHIGNORE           = 4;
  TDSNOCOUNT               = 5;
  TDSARITHABORT            = 6;
  TDSTEXTLIMIT             = 7;
  TDSBROWSE                = 8;
  TDSOFFSET                = 9;
  TDSSTAT                  = 10;
  TDSERRLVL                = 11;
  TDSCONFIRM               = 12;
  TDSSTORPROCID            = 13;
  TDSBUFFER                = 14;
  TDSNOAUTOFREE            = 15;
  TDSROWCOUNT              = 16;
  TDSTEXTSIZE              = 17;
  TDSNATLANG               = 18;
  TDSDATEFORMAT            = 19;
  TDSPRPAD                 = 20;
  TDSPRCOLSEP              = 21;
  TDSPRLINELEN             = 22;
  TDSPRLINESEP             = 23;
  TDSLFCONVERT             = 24;
  TDSDATEFIRST             = 25;
  TDSCHAINXACTS            = 26;
  TDSFIPSFLAG              = 27;
  TDSISOLATION             = 28;
  TDSAUTH                  = 29;
  TDSIDENTITY              = 30;
  TDSNOIDCOL               = 31;
  TDSDATESHORT             = 32;
  TDSCLIENTCURSORS         = 33;
  TDSSETTIME               = 34;
  TDSQUOTEDIDENT           = 35;
  TDSNUMOPTIONS            = 36;
  TDSPADOFF                 = 0;
  TDSPADON                  = 1;
  TDSOFF                    = 0;
  TDSON                     = 1;

  NOSUCHOPTION              = 2;

  MAXOPTTEXT                = 32;

{ common Login manipulations }
  DBSETHOST                 = 1;
  DBSETUSER                 = 2;
  DBSETPWD                  = 3;
{ Sybase Login manipulations }
const
  SYBDBSETHOST              = DBSETHOST;
  SYBDBSETUSER              = DBSETUSER;
  SYBDBSETPWD               = DBSETPWD;
  SYBDBSETHID               = 4;
  SYBDBSETAPP               = 5;
  SYBDBSETBCP               = 6;
  SYBDBSETLANG              = 7;
  SYBDBSETNOSHORT           = 8;
  SYBDBSETHIER              = 9;
  SYBDBSETCHARSET           = 10;
  SYBDBSETPACKET            = 11;
  SYBDBSETENCRYPT           = 12;
  SYBDBSETLABELED           = 13;

{ MsSQL Login manipulations }
const
  MSDBSETHOST               = DBSETHOST;
  MSDBSETUSER               = DBSETUSER;
  MSDBSETPWD                = DBSETPWD;
  MSDBSETAPP                = 4;
  MSDBSETID                 = 5;
  MSDBSETLANG               = 6;
  MSDBSETSECURE             = 7;
  MSDBSET_LOGIN_TIME        = 10;
  MSDBSETFALLBACK           = 12;

{TDS Loginrec manipulations}
  TDSDBSETHOST               = DBSETHOST;
  TDSDBSETUSER               = DBSETUSER;
  TDSDBSETPWD                = DBSETPWD;
  TDSDBSETHID                = 4;
  TDSDBSETAPP                = 5;
  TDSDBSETBCP                = 6;
  TDSDBSETSECURE             = 6;
  TDSDBSETLANG               = 7;
  TDSDBSETNOSHORT            = 8;
  TDSDBSETHIER               = 9;
  TDSDBSETCHARSET            = 10;
  TDSDBSETPACKET             = 11;
  TDSDBSETENCRYPT            = 12;
  TDSDBSETLABELED            = 13;
  TDSDBSETDBNAME             = 14;

{ TDS_DBVERSION_xxx are used with dbsetversion() }
  TDSDBVERSION_UNKNOWN  = 0;
  TDSDBVERSION_46       = 1; // Sybase <= 10
  TDSDBVERSION_100      = 2; // Sybase TDS 5.0  Ver >= 10
  TDSDBVERSION_42       = 3; // This can be used for old Microsoft(<=6.5)
  TDSDBVERSION_70       = 4; // MSSQL v7..2000
  TDSDBVERSION_71       = 5;
  TDSDBVERSION_80       = TDSDBVERSION_71;
  TDSDBVERSION_72       = 6; //MSSQL >= 2005... ?
  TDSDBVERSION_73       = 7;

{ these two are defined by Microsoft for dbsetlname() }
  DBVER42 	            = 8;
  DBVER60 	            = 9;

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
  SQLEBCPS	            = 10106;
  SQLEDTC 	            = 10107;
  SQLENOTIMPL	          = 10108;
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
  EXCONSISTENCY         = 11; { Internal software error  - notify MS Technical Support }

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
  MAXNUMERICLEN = 16;
  MAXNUMERICDIG = 38;

  DEFAULTPRECISION = 18;
  DEFAULTSCALE     = 0;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  TDSMAXTABLENAME  = 512;
  TDSMAXCOLNAMELEN = 512;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  MAXTABLENAME = 30;
  MAXCOLNAMELEN= 30;

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


{ common TDS protocol Data Type mapping of ntwdblib, dblib, freeTDS }
type
  { tabular data stream protocol types }
  //Enum                  ordinal-value
  TTDSType = (
    tdsVoid               = 31,
    tdsImage              = 34,
    tdsText               = 35,
    tdsUnique             = 36, //Unique identifier type
    tdsVarBinary          = 37,
    tdsIntN               = 38,
    tdsVarchar            = 39,
    tdsBinary             = 45,
    tdsChar               = 47,
    tdsInt1               = 48,
    tdsBit                = 50,
    tdsInt2               = 52,
    tdsInt4               = 56,
    tdsDateTime4          = 58,
    tdsFlt4               = 59,
    tdsMoney              = 60,
    tdsDateTime           = 61,
    tdsFlt8               = 62,
    tdsVariant            = 98, {from tds.h -> sybase only}
    tdsNText              = 99, {from tds.h -> sybase only}
    tdsNVarChar           = 103, {from tds.h -> sybase only}
    tdsBitN               = 104, {from tds.h -> sybase only}
    tdsDecimal            = 106,
    tdsNumeric            = 108,
    tdsFltN               = 109,
    tdsMoneyN             = 110,
    tdsDateTimeN          = 111,
    tdsMoney4             = 122,
    {from tds.h -> sade, sybase only}
    tdsInt8                  = 127,
    tdsBigVarBinary          = 165,
    tdsBigVarChar            = 167,
    tdsBigBinary             = 173,
    tdsBigChar               = 175,
    tdsSybaseLongBinary      = 225,
    tdsBigNVarChar           = 231,
    tdsBigNChar              = 239,
    tdsUDT                   = 240,
    tdsMSXML                 = 241
    );

const
{ different type mappings / copied from tds.h -> sybase only!}
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

type
  tdsVARYCHAR=packed record
    len: DBINT;
    str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  dblibVARYCHAR=packed record
    len: DBSMALLINT;
    str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

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
  TDBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;
  TDBDECIMAL = TDBNUMERIC;

  PTDSDBNUMERIC = ^TTDSDBNUMERIC;
  TTDSDBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Val:        array[0..32] of Byte;
  end;
  TTDSDBDECIMAL = TTDSDBNUMERIC;

  DBVARYCHAR = packed record
    Len: DBSMALLINT;
    Str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  DBVARYBIN = packed record
    Len: DBSMALLINT;
    Bytes: array[0..DBMAXCHAR-1] of Byte;
  end;

  TDBMONEY = packed record
    mnyhigh:    DBINT;
    mnylow:     LongWord;
  end;
  PDBMONEY = ^TDBMONEY;

  PDBDATETIME = ^TDBDATETIME;
  TDBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	LongWord;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;

  PTDSDBDATETIME = ^TTDSDBDATETIME;
  TTDSDBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	DBINT;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;

(*
 * Sybase & Microsoft use different names for the dbdaterec members.
 * Keep these two structures physically identical in memory.
 * dbdatecrack() casts one to the other for ease of implementation.
 *
 * Giving credit where credit is due, we can acknowledge that
 * Microsoft chose the better names here, hands down.  ("datedmonth"?!)
 *)
  { FreeTDS sybdb.h }
  PTDS_DBDATEREC = ^Ttds_dbdaterec;
  Ttds_dbdaterec = packed record
    { fields }            {microsoft}                 {sybase}
    year:         DBINT;  { 1753 - 9999 }             { 1900 and counting }
    quarter:      DBINT;  { 1 - 4 }                   { 0 - 3 (Microsoft only) }
    month:        DBINT;  { 1 - 12 }                  { 0 - 11 }
    dayofmonth:   DBINT;  { 1 - 31 }                  { 1 - 31 }
    dayofyear:    DBINT;  { 1 - 366 }                 { 1 - 366 (in Sybase.sybdb.h dayofyear and day are changed around!) }
    week:         DBINT;  { 1 - 54 (for leap years) } { 1 - 54 (Microsoft only) }
    weekday:      DBINT;  { 1 - 7  (Mon - Sun) }      { 0 - 6  (Mon - Sun) }
    hour:         DBINT;  { 0 - 23 }                  { 0 - 23 }
    minute:       DBINT;  { 0 - 59 }                  { 0 - 59 }
    second:       DBINT;  { 0 - 59 }                  { 0 - 59 }
    millisecond:  DBINT;  { 0 - 999 }                 { 0 - 997 }
    tzone:        DBINT;  { 0 - 127 (Sybase only!) }  { 0 - 127 }
  end;

{ DBDATEREC structure used by dbdatecrack }
  DBDATEREC = packed record
    year:       DBINT;      { 1753 - 9999 }
    quarter:    DBINT;      { 1 - 4 }
    month:      DBINT;      { 1 - 12 }
    dayofyear:  DBINT;      { 1 - 366 }
    day:        DBINT;      { 1 - 31 }
    week:       DBINT;      { 1 - 54 (for leap years) }
    weekday:    DBINT;      { 1 - 7  (Mon - Sun) }
    hour:       DBINT;      { 0 - 23 }
    minute:     DBINT;      { 0 - 59 }
    second:     DBINT;      { 0 - 59 }
    millisecond: DBINT;     { 0 - 999 }
  end;
  PDBDATEREC = ^DBDATEREC;

type
  //EH: We need a size of 122 Bytes!
  {$IFDEF FPC}
    {$PACKRECORDS 2}
  {$ELSE}
    {$A2}
  {$ENDIF}
  PZDBCOL = ^ZDBCOL;
  ZDBCOL = record
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
  end;
  PDBCOL = ^DBCOL;
  DBCOL = record
   	SizeOfStruct:   DBINT;
   	Name:           array[0..MAXCOLNAMELEN] of DBCHAR;
   	ActualName:     array[0..MAXCOLNAMELEN] of DBCHAR;
   	TableName:      array[0..MAXTABLENAME] of DBCHAR;
    ColInfo:        ZDBCOL;
   	(*Typ:            DBSHORT;
   	UserType:       DBINT;
   	MaxLength:      DBINT;
   	Precision:      BYTE;
   	Scale:          BYTE;
   	VarLength:      LongBool; { TRUE, FALSE }
   	Null:           BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	CaseSensitive:  BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Updatable:      BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Identity:       LongBool; { TRUE, FALSE or DBUNKNOWN }*)
  end;

  PTDSDBCOL = ^TTDSDBCOL;
  TTDSDBCOL = record
    SizeOfStruct:   DBINT;
    Name:           array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    ActualName:     array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    TableName:      array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    ColInfo:        ZDBCOL;
    (*Typ:            SmallInt;
    UserType:       DBINT;
    MaxLength:      DBINT;
    Precision:      Byte;
    Scale:          Byte;
    VarLength:      LongBool;{ TRUE, FALSE }
    Null:           Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive:  Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:      Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:       LongBool;{ TRUE, FALSE }*)
  end;

  {$IFDEF FPC}
    {$PACKRECORDS DEFAULT}
  {$ELSE}
    {$A+}
  {$ENDIF}
type
  PDBTYPEINFO = ^TDBTYPEINFO;
  TDBTYPEINFO = packed record
    Precision:  DBINT;
    Scale:      DBINT;
  end;

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
    DbErrStr: RawByteString;
    OsErrStr: RawByteString;
  end;

  PDBLibMessage = ^TDBLibMessage;
  TDBLibMessage = record
    dbProc: PDBPROCESS;
    MsgNo: DBINT;
    MsgState: DBINT;
    Severity: DBINT;
    MsgText: RawByteString;
    SrvName: RawByteString;
    ProcName: RawByteString;
    Line: DBUSMALLINT;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB}

implementation

end.
