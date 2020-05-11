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

unit ZPlainDbLibDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses Classes, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZCompatibility, ZPlainDriver
  {$IFDEF TLIST_IS_DEPRECATED},ZClasses{$ENDIF};

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

  {** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface (IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']
  end;

  TDBERRHANDLE_PROC_cdecl = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
  TDBMSGHANDLE_PROC_cdecl = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; cdecl;
  {$IFDEF MSWINDOWS}
  TDBERRHANDLE_PROC_stdcall = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; stdcall;

  TDBMSGHANDLE_PROC_stdcall = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; stdcall;
  {$ENDIF}

  TZDBLibErrorList = Class({$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF})
  public
    procedure Clear; override;
  End;

  TZDBLibMessageList = Class({$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF})
  public
    procedure Clear; override;
  End;

  {$IFDEF TEST_CALLBACK}
  TDBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer of Object;
  TDBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer of Object;


  IZDBLibErrorHandler = interface(IZInterface)
    ['{E853EAE4-3E13-45FD-9483-726D25A7930E}']
  end;
  IZDBLibMessageHandler = interface(IZInterface)
    ['{2B0CB89E-7C17-4C38-B5C4-677C483672AF}']
  end;

  TDbLibErrorHandler = class; //forward
  TDbLibMessageHandler = class; //forward
  {$ENDIF TEST_CALLBACK}
  TDBLibraryVendorType = (lvtFreeTDS, lvtMS, lvtSybase);
  TZDBLIBPLainDriver = class(TZAbstractPlainDriver, IZPlainDriver)
  private
    {$IFDEF TEST_CALLBACK}
    FSQLErrorHandlerList: TZDBLibErrorList;
    FSQLMessageHandlerList: TZDBLibMessageList;
    FCS: TCriticalSection;
    {$ENDIF TEST_CALLBACK}
    {$IFDEF MSWINDOWS}FClientVersion: String;{$ENDIF}
  private
    { core }
    {$IFDEF MSWINDOWS}
    Fdbadata: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
    Fdbadata_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): PByte; stdcall;
    Fdbadlen: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    Fdbadlen_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbaltbind: function(dbproc: PDBPROCESS; ComputeId, Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
    Fdbaltbind_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; stdcall;
    Fdbaltbind_ps: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
    Fdbaltbind_ps_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; stdcall;
    Fdbaltcolid: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    Fdbaltcolid_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; stdcall;
    Fdbaltlen: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    Fdbaltlen_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbaltop: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    Fdbaltop_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; stdcall;
    Fdbalttype: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    Fdbalttype_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; stdcall;
    Fdbaltutype: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    Fdbaltutype_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbanullbind: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; cdecl;
    Fdbanullbind_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; stdcall;
    Fdbbind: function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; cdecl;
    Fdbbind_stdcall: function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; stdcall;
    Fdbbind_ps: function (dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
    Fdbbind_ps_stdcall: function (dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; stdcall;
    Fdbbufsize: function(dbproc: PDBPROCESS): Integer; cdecl;
    Fdbbufsize_stdcall: function(dbproc: PDBPROCESS): Integer; stdcall;
    Fdbbylist: function(dbproc: PDBPROCESS; ComputeId: Integer; Size: PInteger): PByte; cdecl;
    Fdbbylist_stdcall: function(dbproc: PDBPROCESS; ComputeId: Integer; Size: PInteger): PByte; stdcall;
    Fdbcancel: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    Fdbcancel_stdcall: function(dbproc: PDBPROCESS): RETCODE; stdcall;
    Fdbcanquery: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    Fdbcanquery_stdcall: function(dbproc: PDBPROCESS): RETCODE; stdcall;
    Fdbchange: function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
    Fdbchange_stdcall: function(dbroc: PDBPROCESS): PAnsiChar; stdcall;
    //dbcharsetconv
    {EH: Attention -> call convention and ms version returns a RETCODE}
    Fdbclose_SYB: procedure(dbproc: PDBPROCESS); cdecl;
    Fdbclose_MS: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    Fdbclose_stdcall: procedure(dbproc: PDBPROCESS); stdcall;
    Fdbclrbuf: procedure(dbproc: PDBPROCESS; N: DBINT); cdecl;
    Fdbclrbuf_stdcall: procedure(dbproc: PDBPROCESS; N: DBINT); stdcall;
    Fdbclropt: function(dbproc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; cdecl;
    Fdbclropt_stdcall: function(dbproc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; stdcall;
    Fdbcmd: function(dbproc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
    Fdbcmd_stdcall: function(dbproc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; stdcall;
    Fdbcmdrow: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    Fdbcmdrow_stdcall: function(dbproc: PDBPROCESS): RETCODE; stdcall;
    //result type(MS vs Syb) size is different
    Fdbcolbrowse_MS: function(dbproc: PDBPROCESS; Column: Integer): LongBool; cdecl;
    Fdbcolbrowse_SYB: function(dbproc: PDBPROCESS; Column: Integer): DBBOOL; cdecl; //no FreeTDS?
    Fdbcolbrowse_stdcall: function(dbproc: PDBPROCESS; Column: Integer): DBBOOL; stdcall;
    Fdbcollen: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    Fdbcollen_stdcall: function(dbproc: PDBPROCESS; Column: Integer): DBINT; stdcall;
    Fdbcolname: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
    Fdbcolname_stdcall: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; stdcall;
    Fdbcolsource: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl; //no FreeTDS?
    Fdbcolsource_stdcall: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; stdcall;
    Fdbcoltype: function(dbproc: PDBPROCESS; Column: Integer): Integer; cdecl;
    Fdbcoltype_stdcall: function(dbproc: PDBPROCESS; Column: Integer): Integer; stdcall;
    Fdbcoltypeinfo: function(dbproc: PDBPROCESS; Column: Integer): PDBTYPEINFO; cdecl; //no MS
    Fdbcoltypeinfo_stdcall:   function(dbproc: PDBPROCESS; Column: Integer): PDBTYPEINFO; stdcall;
    Fdbcolutype: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    Fdbcolutype_stdcall: function(dbproc: PDBPROCESS; Column: Integer): DBINT; stdcall;
    Fdbconvert: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
        SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
    Fdbconvert_stdcall: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
        SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; stdcall;
    Fdbconvert_ps: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; cdecl; //NO MS
    Fdbconvert_ps_stdcall: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; stdcall;
    Fdbcount: function(dbproc: PDBPROCESS): DBINT; cdecl;
    Fdbcount_stdcall: function(dbproc: PDBPROCESS): DBINT; stdcall;
    Fdbcurcmd_MS: function(dbproc: PDBPROCESS): Integer; cdecl;
    Fdbcurcmd_SYB: function(dbproc: PDBPROCESS): DBINT; cdecl;
    Fdbcurcmd_stdcall: function(dbproc: PDBPROCESS): DBINT; stdcall;
    Fdbcurrow: function(dbproc: PDBPROCESS): DBINT; cdecl;
    Fdbcurrow_stdcall: function(dbproc: PDBPROCESS): DBINT; stdcall;
    Fdbdata: function(dbproc: PDBPROCESS; Column: Integer): PByte; cdecl;
    Fdbdata_stdcall: function(dbproc: PDBPROCESS; Column: Integer): PByte; stdcall;
    //Fdbdatecrack: function(Proc: PDBPROCESS; DateInfo: PDBDATEREC; DateType: PDBDATETIME): RETCODE; cdecl;
    //Fdbdatecrack_stdcall: function(Proc: PDBPROCESS; DateInfo: PDBDATEREC; DateType: PDBDATETIME): RETCODE; cdecl;
    //Fdbdatecrack_TDS: function(dbproc: PDBPROCESS; DateInfo: PTDS_DBDATEREC; DateType: PTDSDBDATETIME): RETCODE; cdecl;
    Fdbdatlen: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    Fdbdatlen_stdcall: function(dbproc: PDBPROCESS; Column: Integer): DBINT; stdcall;

    Fdbdead_MS:  function(dbroc: PDBPROCESS): LongBool; cdecl;
    Fdbdead: function(dbroc: PDBPROCESS): DBBOOL; cdecl;
    Fdbdead_stdcall: function(dbproc: PDBPROCESS): DBBOOL; stdcall;
    Fdbexit: procedure; cdecl;
    Fdbexit_stdcall: procedure; stdcall;
    FdbHasRetStat_MS: function(dbProc: PDBPROCESS): LongBool; cdecl;
    FdbHasRetStat_SYB: function(dbProc: PDBPROCESS): DBBOOL; cdecl;
    FdbHasRetStat_stdcall: function(dbProc: PDBPROCESS): DBBOOL; stdcall;
    {Fdbfcmd: function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
    Fdbfcmd_stdcall: function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; stdcall;}
    Fdbfirstrow: function(Proc: PDBPROCESS): DBINT; cdecl;
    Fdbfirstrow_stdcall: function(Proc: PDBPROCESS): DBINT; stdcall;
    Fdbfreebuf: procedure(Proc: PDBPROCESS); cdecl;
    Fdbfreebuf_stdcall: procedure(Proc: PDBPROCESS); stdcall;
    Fdbfreequal: procedure(Ptr: PAnsiChar); cdecl;
    Fdbfreequal_stdcall: procedure(Ptr: PAnsiChar); stdcall;
    Fdbgetchar: function(Proc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
    Fdbgetchar_stdcall: function(Proc: PDBPROCESS; N: Integer): PAnsiChar; stdcall;
    Fdbgetcharset: function(dbproc: PDBPROCESS): PAnsiChar; cdecl; //NO MS
    Fdbgetcharset_stdcall: function(dbproc: PDBPROCESS): PAnsiChar; stdcall;
    FdbGetRow: function(dbProc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
    FdbGetRow_stdcall: function(dbProc: PDBPROCESS; Row: DBINT): STATUS; stdcall;
    FdbInit_MS: function: PAnsiChar; cdecl; //returns the librayversion
    FdbInit: function: RETCODE; cdecl;
    FdbInit_stdcall: function: RETCODE; stdcall; //SYBASE returns SUCCEED or FAIL
    FdbLogin_stdcall: function: PLOGINREC; stdcall;
    Fdbloginfree: procedure(loginptr: PLOGINREC); cdecl;
    Fdbloginfree_stdcall: procedure(loginptr: PLOGINREC); stdcall;
    FdbMoreCmds: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbMoreCmds_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbName: function(dbProc: PDBPROCESS): PAnsiChar; cdecl;
    FdbName_stdcall: function(dbProc: PDBPROCESS): PAnsiChar; stdcall;
    FdbNextRow: function(dbProc: PDBPROCESS): STATUS; cdecl;
    FdbNextRow_stdcall: function(dbProc: PDBPROCESS): STATUS; stdcall;
    FdbNumCols: function(dbProc: PDBPROCESS): DBINT; cdecl;
    FdbNumCols_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    FdbOpen: function(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; cdecl;
    FdbOpen_stdcall: function(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; stdcall;
    FtdsDbOpen: function(Login: PLOGINREC; server: PAnsiChar; msdblib: Integer): PDBPROCESS; cdecl;

    FdbResults: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbResults_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbRetData: function(dbProc: PDBPROCESS; RetNum: Integer): Pointer; cdecl;
    FdbRetData_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): Pointer; stdcall;
    FdbRetStatus: function(dbProc: PDBPROCESS): DBINT; cdecl;
    FdbRetStatus_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    FdbRetType: function(dbProc: PDBPROCESS; RetNum: DBINT): DBINT; cdecl;
    FdbRetType_stdcall: function(dbProc: PDBPROCESS; RetNum: DBINT): DBINT; stdcall;
    {rpc i.e remote procedure calls }
    FdbRpcInit: function(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: DBSMALLINT): RETCODE; cdecl;
    FdbRpcInit_stdcall: function(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: DBSMALLINT): RETCODE; stdcall;
    FdbRpcParam: function(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; cdecl;
    FdbRpcParam_stdcall: function(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; stdcall;
    FdbRpcSend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbRpcSend_stdcall: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbRpcExec: function(dbProc: PDBPROCESS): RETCODE; cdecl; //MS only
    FdbRetLen: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
    FdbRetLen_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; stdcall;
    FdbRetName: function(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
    FdbRetName_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; stdcall;
    FdbSqlExec: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbSqlExec_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbSqlOk: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbSqlOk_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    Fdbsqlsend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    Fdbsqlsend_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbSetLName: function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; cdecl;
    FdbSetLName_stdcall: function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; stdcall;
    FdbSetLoginTime: function(Seconds: Integer): RETCODE; cdecl;
    FdbSetLoginTime_stdcall: function(Seconds: Integer): RETCODE; stdcall;
    FdbSetOpt_MS: function(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar): RETCODE; cdecl;
    FdbSetOpt_SYB: function(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; cdecl;
    FdbSetOpt_stdcall: function(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; stdcall;
    Fdbsetlpacket: function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl; //MS only others use dbsetlname
    FdbSetMaxprocs_S: function(MaxProcs: SmallInt): RETCODE; cdecl;
    FdbSetMaxprocs_I: function(MaxProcs: DBINT): RETCODE; cdecl;
    FdbSetMaxprocs_stdcall: function(MaxProcs: DBINT): RETCODE; stdcall; //sybase has widened the type!
    FdbUse: function(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; cdecl;
    FdbUse_stdcall: function(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; stdcall;
    Fdbvarylen_MS: function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
    Fdbvarylen_SYB: function(Proc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
    Fdbvarylen_stdcall: function(Proc: PDBPROCESS; Column: Integer): DBBOOL; stdcall;
    //Fdb12hour: function(dbproc: PDBPROCESS; Language: PAnsiChar): DBBOOL; cdecl; //no MS
    //Fdb12hour_stdcall:          function(dbproc: PDBPROCESS; Language: PAnsiChar): DBBOOL; stdcall; //no MS
    dberrhandle_stdcall: function(Handler: TDBERRHANDLE_PROC_stdcall): TDBERRHANDLE_PROC_stdcall; stdcall;
    dbmsghandle_stdcall: function(Handler: TDBMSGHANDLE_PROC_stdcall): TDBMSGHANDLE_PROC_stdcall; stdcall;
    FdbSetVersion_stdcall: function(Version: DBINT): RETCODE; stdcall;
    fdbWinexit: procedure; cdecl; // MS only
    {$ENDIF}
    //Fdbtds: function(dbproc: PDBPROCESS): DBINT; cdecl;
    FdbLogin: function: PLOGINREC; cdecl;
  private
    FDBLibraryVendorType: TDBLibraryVendorType;
    FdbSetVersion: function(Version: DBINT): RETCODE; cdecl;
  protected
    procedure LoadApi; override;
    procedure LoadCodePages; override;
  public
    procedure AfterConstruction; override;
    {$IFDEF TEST_CALLBACK}
    procedure BeforeDestruction; override;
    {$ENDIF TEST_CALLBACK}
    destructor Destroy; override;
  public //core
    dberrhandle: function(Handler: TDBERRHANDLE_PROC_cdecl): TDBERRHANDLE_PROC_cdecl; cdecl;
    dbmsghandle: function(Handler: TDBMSGHANDLE_PROC_cdecl): TDBMSGHANDLE_PROC_cdecl; cdecl;

    dbsetlversion: function(Login: PLOGINREC; Version: Byte): RETCODE; cdecl; //just FreeTDS
    tdsdump_on: procedure; cdecl;
    tdsdump_off: procedure; cdecl;
    tdsdump_open: function (FileName : PAnsiChar): Integer; cdecl;
    tdsdump_close: procedure; cdecl;
    //_tds_socket_init: procedure ; cdecl;
    //_tds_socket_done: procedure ; cdecl;
    {$IFDEF MSWINDOWS}
    function dbadata(dbproc: PDBPROCESS; ComputeId, Column: Integer): PByte; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function dbadlen(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltbind(dbproc: PDBPROCESS; ComputeId, Column, VarType: Integer;
                       VarLen: DBINT; VarAddr: PByte): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltbind_ps(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer;
      VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltcolid(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltlen(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltop(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbalttype(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbaltutype(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbanullbind(dbproc: PDBPROCESS; ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbbind(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbbind_ps(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}//no MS
    function dbbufsize(dbproc: PDBPROCESS): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}//no MS
    function dbbylist(dbproc: PDBPROCESS; ComputeId: Integer; Size: PInteger): PByte; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcancel(dbproc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcanquery(dbproc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbchange(dbproc: PDBPROCESS): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    procedure dbclose(dbproc: PDBPROCESS); {$IFDEF WITH_INLINE}inline; {$ENDIF}
    procedure dbclrbuf(dbproc: PDBPROCESS; N: DBINT); {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbclropt(dbproc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcmd(dbproc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcmdrow(dbproc: PDBPROCESS): RETCODE;{$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcolbrowse(dbproc: PDBPROCESS; Column: Integer): DBBOOL; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    public dbcolinfo: function(pdbhandle: PDBHANDLE; _Type: Integer; Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE; cdecl;//no SYB but FreeTDS
    function dbcollen(dbproc: PDBPROCESS; Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcolname(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcolsource(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcoltype(dbproc: PDBPROCESS; Column: Integer): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcolutype(dbproc: PDBPROCESS; Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcoltypeinfo(dbproc: PDBPROCESS; Column: Integer): PDBTYPEINFO; {$IFDEF WITH_INLINE}inline; {$ENDIF}//no MS
    function dbconvert(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
        SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbconvert_ps(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; {$IFDEF WITH_INLINE}inline; {$ENDIF}//no MS
    function dbcount(dbproc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcurcmd(dbproc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbcurrow(dbproc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbdata(dbproc: PDBPROCESS; Column: Integer): PByte; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    public dbdataready: function(Proc: PDBPROCESS): LongBool; cdecl; //MS only
    function dbdatlen(dbproc: PDBPROCESS; Column: Integer): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbdead(dbproc: PDBPROCESS): DBBOOL; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    //public dbenlisttrans: function(Proc: PDBPROCESS; PurposeUnkonwn: Longbool): RETCODE; cdecl; //MS only purpose is unknown
    procedure dbexit; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSqlOk(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbsqlsend(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    //function dbfcmd(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE;
    function dbfirstrow(Proc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    procedure dbfreebuf(Proc: PDBPROCESS); {$IFDEF WITH_INLINE}inline; {$ENDIF}
    procedure dbfreequal(Ptr: PAnsiChar); {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbgetchar(Proc: PDBPROCESS; N: Integer): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbgetcharset(dbproc: PDBPROCESS): PAnsiChar;{$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbHasRetStat(dbProc: PDBPROCESS): DBBOOL; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    {$ENDIF}
    function dbOpen(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    {$IFDEF MSWINDOWS}
    procedure dbloginfree(loginptr: PLOGINREC); {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbname(dbProc: PDBPROCESS): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbNextRow(dbProc: PDBPROCESS): STATUS; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbNumCols(dbProc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbResults(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRetStatus(dbProc: PDBPROCESS): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT; {$IFDEF WITH_INLINE}inline; {$ENDIF}

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: DBSMALLINT): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): DBINT; cdecl; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; {$IFDEF WITH_INLINE}inline; {$ENDIF}

    function dbSetLoginTime(Seconds: Integer): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLName(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbVaryLen(dbProc: PDBPROCESS; Column: Integer): DBBOOL; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    {$ENDIF}
  public
    {$IFNDEF MSWINDOWS}
    dbadata: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
    dbadlen: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    dbaltbind: function(dbproc: PDBPROCESS; ComputeId, Column, VarType: Integer;
                       VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
    dbaltbind_ps: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer;
      VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
    dbaltcolid: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    dbaltlen: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    dbaltop: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    dbalttype: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
    dbaltutype: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    dbanullbind: function(dbproc: PDBPROCESS; ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; cdecl;
    dbbind: function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; cdecl;
    dbbind_ps: function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;//no MS
    dbbufsize: function(dbproc: PDBPROCESS): Integer; cdecl;//no MS
    dbbylist: function(dbproc: PDBPROCESS; ComputeId: Integer; Size: PInteger): PByte;  cdecl;
    dbcancel: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    dbcanquery: function(dbproc: PDBPROCESS): RETCODE; cdecl;
    dbchange: function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
    dbclose: procedure(dbproc: PDBPROCESS); cdecl;
    dbclrbuf: procedure(dbproc: PDBPROCESS; N: DBINT); cdecl;
    dbclropt: function(dbproc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; cdecl;
    dbcmd: function(dbproc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
    dbcmdrow: function(dbproc: PDBPROCESS): RETCODE;cdecl;
    dbcolbrowse: function(dbproc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
    dbcolinfo: function(pdbhandle: PDBHANDLE; _Type: Integer; Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE; cdecl;//no SYB but FreeTDS
    dbcollen: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    dbcolname: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
    dbcolsource: function(dbproc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
    dbcoltype: function(dbproc: PDBPROCESS; Column: Integer): Integer; cdecl;
    dbcolutype: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    dbcoltypeinfo: function(dbproc: PDBPROCESS; Column: Integer): PDBTYPEINFO; cdecl;//no MS
    dbconvert: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
        SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
    dbconvert_ps: function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; cdecl;//no MS
    dbcount: function(dbproc: PDBPROCESS): DBINT; cdecl;
    dbcurcmd: function(dbproc: PDBPROCESS): DBINT; cdecl;
    dbcurrow: function(dbproc: PDBPROCESS): DBINT; cdecl;
    dbdata: function(dbproc: PDBPROCESS; Column: Integer): PByte; cdecl;
    dbdatlen: function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    dbdead: function(dbproc: PDBPROCESS): DBBOOL; cdecl;
    dbexit: procedure; cdecl;
    dbHasRetStat: function(dbProc: PDBPROCESS): DBBOOL; cdecl;

    dbSqlExec: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    dbSqlOk: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    dbsqlsend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    //dbfcmd: function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
    dbfirstrow: function(Proc: PDBPROCESS): DBINT; cdecl;
    dbfreebuf: procedure(Proc: PDBPROCESS); cdecl;
    dbfreequal: procedure(Ptr: PAnsiChar); cdecl;
    dbgetchar: function(Proc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
    dbgetcharset: function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
    dbGetRow: function(dbProc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
    FdbInit: function: RETCODE; cdecl;

//    Jan: renamed this to FdbOpen. I don't know how wo decide between calling
//    tdsdbopen and dbopen otherwise.?
    FdbOpen: function(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; cdecl;
    FtdsDbOpen: function(Login: PLOGINREC; server: PAnsiChar; msdblib: Integer): PDBPROCESS; cdecl;

    dbloginfree: procedure(loginptr: PLOGINREC); cdecl;
    dbMoreCmds: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    dbname: function(dbProc: PDBPROCESS): PAnsiChar; cdecl;
    dbNextRow: function(dbProc: PDBPROCESS): STATUS; cdecl;
    dbNumCols: function(dbProc: PDBPROCESS): DBINT; cdecl;
    dbResults: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    dbRetStatus: function(dbProc: PDBPROCESS): DBINT; cdecl;
    dbRpcInit: function(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: DBSMALLINT): RETCODE; cdecl;
    dbRpcParam: function(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; cdecl;
    dbRpcSend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    dbRetData: function(dbProc: PDBPROCESS; RetNum: Integer): Pointer; cdecl;
    dbRetLen: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
    dbRetName: function(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
    dbRetType: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
    dbSetLoginTime: function(Seconds: Integer): RETCODE; cdecl;
    dbSetLName: function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; cdecl;
    dbSetMaxprocs: function(MaxProcs: DBINT): RETCODE; cdecl;
    dbSetOpt: function(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; cdecl;
    dbUse: function(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; cdecl;
    dbvarylen: function(Proc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
    {$ENDIF}
  public { macros }
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLSecure(Login: PLOGINREC): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLPacket(Login: PLOGINREC; packet_size: Word): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
  public { incompatibility workarounds }
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE; {$IFDEF WITH_INLINE}inline; {$ENDIF}
    function dbIntit: RETCODE;
    function dbsetversion(Version: DBINT): RETCODE;
    function dbLogin: PLOGINREC; virtual;
  public //mapings from zeos to provider enums
    function GetDBOption(AOption: TdbOption): DBINT;
  public
    {$IFDEF TEST_CALLBACK}
    function GetErrorHandler(dbProc: PDBPROCESS; ADBERRHANDLE_PROC: TDBERRHANDLE_PROC): IZDBLibErrorHandler;
    function GetMessageHandler(dbProc: PDBPROCESS; ADBMSGHANDLE_PROC: TDBMSGHANDLE_PROC): IZDBLibMessageHandler;
    procedure DeRegisterErrorHandler(Const Handler: TDbLibErrorHandler);
    procedure DeRegisterMessageHandler(Const Handler: TDbLibMessageHandler);
    {$ELSE}
    procedure AssignErrorMessages(dbProc: PDBPROCESS; DestErrors: TZDBLibErrorList; DestMessages: TZDBLibMessageList);
    {$ENDIF TEST_CALLBACK}
  public
    property DBLibraryVendorType: TDBLibraryVendorType read fDBLibraryVendorType;
  end;

  TMSSQLDBLibPLainDriver = class(TZDBLIBPLainDriver, IZPlainDriver)
  protected
    procedure LoadCodePages; override;
    function Clone: IZPlainDriver; override;
  public
    procedure AfterConstruction; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  TSybaseDBLibPLainDriver = class(TZDBLIBPLainDriver, IZPlainDriver)
  protected
    procedure LoadCodePages; override;
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    procedure AfterConstruction; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {$IFDEF TEST_CALLBACK}
//see: https://www.experts-exchange.com/questions/10414716/How-to-make-a-Callback-Procedure-Function-to-Procedure-Function-of-Object.html
//http://delphi.cjcsoft.net/viewthread.php?tid=45566
//https://entwickler-forum.de/forum/archiv/andere-sprachen-aa/delphi-aa/dll/25575-objekte-aus-callback-funktionen-ansprechen
  TZMethodToDllCallbackProcedure = class(TZMethodToDllCallbackDispatcher)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FManager: TZDBLIBPLainDriver;
  public
    constructor Create(const Instance: TObject; methodAddr: pointer;
      const CallBackManager: TZDBLIBPLainDriver);
  end;

  TDbLibErrorHandler = class(TZMethodToDllCallbackProcedure, IZDBLibErrorHandler)
  private
    FHandle: PDBPROCESS;
    {$IFDEF MSWINDOWS}
    FOlddberrhandle_stdcall: TDBERRHANDLE_PROC_stdcall;
    {$ENDIF MSWINDOWS}
    FOlddberrhandle_cdecl: TDBERRHANDLE_PROC_cdecl;
    FMydberrhandle: TDBERRHANDLE_PROC; //fastcall convention
    {$IFDEF MSWINDOWS}
    function dberrhandle_stdcall(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
      DbErrStr, OsErrStr: PAnsiChar): Integer; stdcall;
    {$ENDIF MSWINDOWS}
    function dberrhandle_cdecl(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
      DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
  public
    Constructor Create(Handle: PDBPROCESS; DBERRHANDLE_PROC: TDBERRHANDLE_PROC;
      const Manager: TZDBLIBPLainDriver);
    procedure BeforeDestruction; override;
  end;

  TDbLibMessageHandler = class(TZMethodToDllCallbackProcedure, IZDBLibMessageHandler)
  private
    FHandle: PDBPROCESS;
    {$IFDEF MSWINDOWS}
    FOlddbmsghandle_stdcall: TDBMSGHANDLE_PROC_stdcall;
    {$ENDIF MSWINDOWS}
    FOlddbmsghandle_cdecl: TDBMSGHANDLE_PROC_cdecl;
    FMydbmsghandle: TDBMSGHANDLE_PROC; //fastcall convention
    {$IFDEF MSWINDOWS}
    function dbmsghandle_stdcall(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
      MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; stdcall;
    {$ENDIF MSWINDOWS}
    function dbmsghandle_cdecl(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
      MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; cdecl;
  public
    Constructor Create(Handle: PDBPROCESS; DBMSGHANDLE_PROC: TDBMSGHANDLE_PROC;
      const Manager: TZDBLIBPLainDriver);
    procedure BeforeDestruction; override;
  end;
  {$ENDIF TEST_CALLBACK}
{$ENDIF ZEOS_DISABLE_DBLIB}

implementation

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses SysUtils, ZPlainLoader, ZEncoding, ZFastCode;

var
  OldFreeTDSErrorHandle: TDBERRHANDLE_PROC_cdecl = nil;
  OldFreeTDSMessageHandle: TDBMSGHANDLE_PROC_cdecl = nil;
  OldSybaseErrorHandle: {$IFDEF MSWINDOWS}TDBERRHANDLE_PROC_stdcall{$ELSE}TDBERRHANDLE_PROC_cdecl{$ENDIF} = nil;
  OldSybaseMessageHandle: {$IFDEF MSWINDOWS}TDBMSGHANDLE_PROC_stdcall{$ELSE}TDBMSGHANDLE_PROC_cdecl{$ENDIF} = nil;
  OldMsSQLErrorHandle: TDBERRHANDLE_PROC_cdecl = nil;
  OldMsSQLMessageHandle: TDBMSGHANDLE_PROC_cdecl = nil;
  ErrorCS: TCriticalSection;
  SQLErrors: TZDBLibErrorList;
  SQLMessages: TZDBLibMessageList;

procedure AddSybaseCodePages(PlainDriver: TZAbstractPlainDriver);
begin
// codepages as found in "SAP Adaptive Server Enterprise 16.0 > Configuration Guide for UNIX Adaptive Server Enterprise 16.0 > Localization Support"
  PlainDriver.AddCodePage('ascii_8', 1, ceAnsi, zCP_us_ascii);
  PlainDriver.AddCodePage('big5', 2, ceAnsi, zCP_Big5);
  PlainDriver.AddCodePage('cp437', 3, ceAnsi, zCP_DOS437);
  PlainDriver.AddCodePage('cp850', 4, ceAnsi, zCP_DOS850);
  PlainDriver.AddCodePage('cp852', 5, ceAnsi, zCP_DOS852);
  PlainDriver.AddCodePage('cp855', 6, ceAnsi, zCP_DOS855);
  PlainDriver.AddCodePage('cp857', 7, ceAnsi, zCP_DOS857);
  PlainDriver.AddCodePage('cp858', 8, ceAnsi, zCP_DOS858);
  PlainDriver.AddCodePage('cp860', 9, ceAnsi, zCP_DOS860);
  PlainDriver.AddCodePage('cp864', 10, ceAnsi, zCP_DOS864);
  PlainDriver.AddCodePage('cp866', 11, ceAnsi, zCP_DOS866);
  PlainDriver.AddCodePage('cp869', 12, ceAnsi, zCP_DOS869);
  PlainDriver.AddCodePage('cp874', 13, ceAnsi, zCP_WIN874);
  PlainDriver.AddCodePage('cp932', 14, ceAnsi, zCP_SHIFTJS);
  PlainDriver.AddCodePage('cp936', 15, ceAnsi, zCP_GB2312);
  PlainDriver.AddCodePage('cp950', 16, ceAnsi, zCP_Big5);
  PlainDriver.AddCodePage('cp1250', 17, ceAnsi, zCP_WIN1250);
  PlainDriver.AddCodePage('cp1251', 18, ceAnsi, zCP_WIN1251);
  PlainDriver.AddCodePage('cp1252', 19, ceAnsi, zCP_WIN1252);
  PlainDriver.AddCodePage('cp1253', 20, ceAnsi, zCP_WIN1253);
  PlainDriver.AddCodePage('cp1254', 21, ceAnsi, zCP_WIN1254);
  PlainDriver.AddCodePage('cp1255', 22, ceAnsi, zCP_WIN1255);
  PlainDriver.AddCodePage('cp1256', 23, ceAnsi, zCP_WIN1256);
  PlainDriver.AddCodePage('cp1257', 24, ceAnsi, zCP_WIN1257);
  PlainDriver.AddCodePage('cp1258', 25, ceAnsi, zCP_WIN1258);
  PlainDriver.AddCodePage('gb18030', 26, ceAnsi, zCP_GB18030);
  PlainDriver.AddCodePage('iso_1', 27, ceAnsi, zCP_L1_ISO_8859_1);
  PlainDriver.AddCodePage('iso88592', 28, ceAnsi, zCP_L2_ISO_8859_2);
  PlainDriver.AddCodePage('iso88595', 29, ceAnsi, zCP_L5_ISO_8859_5);
  PlainDriver.AddCodePage('iso88596', 30, ceAnsi, zCP_L6_ISO_8859_6);
  PlainDriver.AddCodePage('iso88597', 31, ceAnsi, zCP_L7_ISO_8859_7);
  PlainDriver.AddCodePage('iso88598', 32, ceAnsi, zCP_L8_ISO_8859_8);
  PlainDriver.AddCodePage('iso88599', 33, ceAnsi, zCP_L5_ISO_8859_9);
  PlainDriver.AddCodePage('iso15', 34, ceAnsi, zCP_L9_ISO_8859_15);
  PlainDriver.AddCodePage('sjis', 35, ceAnsi, zCP_SHIFTJS);
  PlainDriver.AddCodePage('utf8', 36, ceUTF8, zCP_UTF8);
end;

procedure AddmMSCodePages(PlainDriver: TZAbstractPlainDriver);
begin
  { SingleByte }
  PlainDriver.AddCodePage('WINDOWS-1250', 1, ceAnsi, zCP_WIN1250, '', 1, False); {Microsoft Windows Codepage 1250 (East European)}
  PlainDriver.AddCodePage('WINDOWS-1251', 2, ceAnsi, zCP_WIN1251, '', 1, False); {Microsoft Windows Codepage 1251 (Cyrl)}
  PlainDriver.AddCodePage('WINDOWS-1252', 3, ceAnsi, zCP_WIN1252, '', 1, False); {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
  PlainDriver.AddCodePage('WINDOWS-1253', 4, ceAnsi, zCP_WIN1253, '', 1, False); {Microsoft Windows Codepage 1253 (Greek)}
  PlainDriver.AddCodePage('WINDOWS-1254', 5, ceAnsi, zCP_WIN1254, '', 1, False); {Microsoft Windows Codepage 1254 (Turk)}
  PlainDriver.AddCodePage('WINDOWS-1255', 6, ceAnsi, zCP_WIN1255, '', 1, False); {Microsoft Windows Codepage 1255 (Hebrew)}
  PlainDriver.AddCodePage('WINDOWS-1256', 7, ceAnsi, zCP_WIN1256, '', 1, False); {Microsoft Windows Codepage 1256 (Arab)}
  PlainDriver.AddCodePage('WINDOWS-1257', 8, ceAnsi, zCP_WIN1257, '', 1, False); {Microsoft Windows Codepage 1257 (BaltRim)}
  PlainDriver.AddCodePage('WINDOWS-1258', 9, ceAnsi, zCP_WIN1258, '', 1, False); {Microsoft Windows Codepage 1258 (Viet), TCVN-5712}
end;

{ Handle sql server error messages }
function SybaseErrorHandle(dbproc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer;
{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := dbproc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr), SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr), SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function SybaseMessageHandle(dbproc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := dbproc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server error messages }
function DbLibErrorHandle(dbproc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := dbproc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr),SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr),SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function DbLibMessageHandle(dbproc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := dbproc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server error messages }
function FreeTDSErrorHandle(dbproc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := dbproc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr),SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr),SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function FreeTDSMessageHandle(dbproc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := dbproc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

{ TZDBLIBPLainDriver }

(** Return a pointer to the data for a compute column *)
procedure TZDBLIBPLainDriver.AfterConstruction;
begin
  inherited;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF TEST_CALLBACK}
  FSQLErrorHandlerList := TZDBLibErrorList.Create;
  FSQLMessageHandlerList := TZDBLibMessageList.Create;
  FCS := TCriticalSection.Create;
  {$ENDIF}
  FLoader.AddLocation('sybdb'+SharedSuffix);
  LoadCodePages;
end;

{$IFDEF TEST_CALLBACK}
procedure TZDBLIBPLainDriver.BeforeDestruction;
var I: Integer;
begin
  inherited;
  for i := FSQLErrorHandlerList.Count-1 downto 0 do
    Dispose(PDBLibError(FSQLErrorHandlerList[i]));
  FreeAndNil(FSQLErrorHandlerList);
  for i := FSQLMessageHandlerList.Count-1 downto 0 do
    Dispose(PDBLibMessage(FSQLMessageHandlerList[i]));
  FreeAndNil(FSQLMessageHandlerList);
  FCS.Free;
end;

{$ELSE TEST_CALLBACK}
procedure TZDBLIBPLainDriver.AssignErrorMessages(dbProc: PDBPROCESS;
  DestErrors: TZDBLibErrorList; DestMessages: TZDBLibMessageList);
var I: Integer;
begin
  ErrorCS.Enter;
  try
    for i := SQLErrors.Count -1 downto 0 do
      if (dbProc = nil) or (PDBLibError(SQLErrors[i]).dbProc = dbProc) then begin
        DestErrors.Add(SQLErrors[i]);
        SQLErrors.Delete(i);
      end;
    for i := SQLMessages.Count -1 downto 0 do
      if (dbProc = nil) or (PDBLibMessage(SQLMessages[i]).dbProc = dbProc) then begin
        DestMessages.Add(SQLMessages[i]);
        SQLMessages.Delete(i);
      end;
  finally
    ErrorCS.Leave;
  end;
end;

{$ENDIF TEST_CALLBACK}

{$IFDEF MSWINDOWS}
function TZDBLIBPLainDriver.dbadata(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): PByte;
begin
  if Assigned(Fdbadata)
  then Result := Fdbadata(dbproc, ComputeID, Column)
  else Result := Fdbadata_stdcall(dbproc, ComputeID, Column)
end;

(** Return the actual length of the data for a compute column *)
function TZDBLIBPLainDriver.dbadlen(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): DBINT;
begin
  if Assigned(Fdbadlen)
  then Result := Fdbadlen(dbproc, ComputeID, Column)
  else Result := Fdbadlen_stdcall(dbproc, ComputeID, Column);
end;

(** Bind a compute column to a program variable *)
function TZDBLIBPLainDriver.dbaltbind(dbproc: PDBPROCESS; ComputeId,
  Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE;
begin
  if Assigned(Fdbaltbind)
  then Result := Fdbaltbind(dbproc, ComputeID, Column, VarType, VarLen, VarAddr)
  else Result := Fdbaltbind_stdcall(dbproc, ComputeID, Column, VarType, VarLen, VarAddr)
end;

(** Bind a compute column to a program variable,
 with precision and scale support for numeric and decimal datatypes *)
function TZDBLIBPLainDriver.dbaltbind_ps(dbproc: PDBPROCESS; ComputeId,
  Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte;
  typinfo: PDBTYPEINFO): RETCODE;
begin
  if Assigned(Fdbaltbind_ps)
  then Result := Fdbaltbind_ps(dbproc, ComputeID, Column, VarType, VarLen, VarAddr, typinfo)
  else Result := Fdbaltbind_ps_stdcall(dbproc, ComputeID, Column, VarType, VarLen, VarAddr, typinfo)
end;

(** Return the column ID for a compute column *)
function TZDBLIBPLainDriver.dbaltcolid(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): Integer;
begin
  if Assigned(Fdbaltcolid)
  then Result := Fdbaltcolid(dbproc, ComputeID, Column)
  else Result := Fdbaltcolid_stdcall(dbproc, ComputeID, Column)
end;

(** Return the maximum length of the data for a particular compute column *)
function TZDBLIBPLainDriver.dbaltlen(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): DBINT;
begin
  if Assigned(Fdbaltlen)
  then Result := Fdbaltlen(dbproc, ComputeID, Column)
  else Result := Fdbaltlen_stdcall(dbproc, ComputeID, Column)
end;

(** Return the type of aggregate operator for a particular compute column *)
function TZDBLIBPLainDriver.dbaltop(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): Integer;
begin
  if Assigned(Fdbaltop)
  then Result := Fdbaltop(dbproc, ComputeID, Column)
  else Result := Fdbaltop_stdcall(dbproc, ComputeID, Column)
end;

(** Return the datatype for a compute column *)
function TZDBLIBPLainDriver.dbalttype(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): Integer;
begin
  if Assigned(Fdbalttype)
  then Result := Fdbalttype(dbproc, ComputeID, Column)
  else Result := Fdbalttype_stdcall(dbproc, ComputeID, Column)
end;

(** Return the user-defined datatype for a compute column *)
function TZDBLIBPLainDriver.dbaltutype(dbproc: PDBPROCESS; ComputeId,
  Column: Integer): DBINT;
begin
  if Assigned(Fdbaltutype)
  then Result := Fdbaltutype(dbproc, ComputeID, Column)
  else Result := Fdbaltutype_stdcall(dbproc, ComputeID, Column)
end;

(** Associate an indicator variable with a compute-row column *)
function TZDBLIBPLainDriver.dbanullbind(dbproc: PDBPROCESS; ComputeId,
  Column: Integer; Indicator: PDBINT): RETCODE;
begin
  if Assigned(Fdbanullbind)
  then Result := Fdbanullbind(dbproc, ComputeID, Column, Indicator)
  else Result := Fdbanullbind_stdcall(dbproc, ComputeID, Column, Indicator)
end;

(** Bind a regular result column to a program variable *)
function TZDBLIBPLainDriver.dbbind(dbproc: PDBPROCESS; Column, VarType,
  VarLen: Integer; VarAddr: PByte): RETCODE;
begin
  if Assigned(Fdbbind)
  then Result := Fdbbind(dbproc, Column, VarType, VarLen, VarAddr)
  else Result := Fdbbind_stdcall(dbproc, Column, VarType, VarLen, VarAddr)
end;

(** Bind a regular result column to a program variable,
  with precision and scale support for numeric and decimal datatypes *)
function TZDBLIBPLainDriver.dbbind_ps(dbproc: PDBPROCESS; Column,
  VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE;
begin
  if Assigned(Fdbbind_ps)
  then Result := Fdbbind_ps(dbproc, Column, VarType, VarLen, VarAddr, typinfo)
  else Result := Fdbbind_ps_stdcall(dbproc, Column, VarType, VarLen, VarAddr, typinfo)
end;

(** Return the size of a DBPROCESS row buffer *)
function TZDBLIBPLainDriver.dbbufsize(dbproc: PDBPROCESS): Integer;
begin
  if Assigned(Fdbbufsize)
  then Result := Fdbbufsize(dbproc)
  else Result := Fdbbufsize_stdcall(dbproc)
end;

(** Return the bylist for a compute row *)
function TZDBLIBPLainDriver.dbbylist(dbproc: PDBPROCESS;
  ComputeId: Integer; Size: PInteger): PByte;
begin
  if Assigned(Fdbbylist)
  then Result := Fdbbylist(dbproc, ComputeId, Size)
  else Result := Fdbbylist_stdcall(dbproc, ComputeId, Size)
end;

(** Cancel the current command batch *)
function TZDBLIBPLainDriver.dbcancel(dbproc: PDBPROCESS): RETCODE;
begin
  if Assigned(Fdbcancel)
  then Result := Fdbcancel(dbproc)
  else Result := Fdbcancel_stdcall(dbproc)
end;

(** Cancel any rows pending from the most recently executed query *)
function TZDBLIBPLainDriver.dbcanquery(dbproc: PDBPROCESS): RETCODE;
begin
  if Assigned(Fdbcanquery)
  then Result := Fdbcanquery(dbproc)
  else Result := Fdbcanquery_stdcall(dbproc)
end;

(** Determine whether a command batch has changed the current database *)
function TZDBLIBPLainDriver.dbchange(dbproc: PDBPROCESS): PAnsiChar;
begin
  if Assigned(Fdbchange)
  then Result := Fdbchange(dbproc)
  else Result := Fdbchange_stdcall(dbproc)
end;

(** Close and deallocate a single DBPROCESS structure *)
procedure TZDBLIBPLainDriver.dbclose(dbproc: PDBPROCESS);
begin
  if Assigned(Fdbclose_ms) then
    Fdbclose_ms(dbproc)
  else if Assigned(Fdbclose_stdcall) then
    Fdbclose_stdcall(dbproc)
  else Fdbclose_SYB(dbproc);
end;

(** Drop rows from the row buffer *)
procedure TZDBLIBPLainDriver.dbclrbuf(dbproc: PDBPROCESS; N: DBINT);
begin
  if Assigned(Fdbclrbuf)
  then Fdbclrbuf(dbproc, N)
  else Fdbclrbuf_stdcall(dbproc, n)
end;

(** Clear an option set by dbsetopt *)
function TZDBLIBPLainDriver.dbclropt(dbproc: PDBPROCESS; Option: Integer;
  Param: PAnsiChar): RETCODE;
begin
  if Assigned(Fdbclropt)
  then Result := Fdbclropt(dbproc, Option, Param)
  else Result := Fdbclropt_stdcall(dbproc, Option, Param)
end;

(** Add text to the DBPROCESS command buffer *)
function TZDBLIBPLainDriver.dbcmd(dbproc: PDBPROCESS;
  Cmd: PAnsiChar): RETCODE;
begin
  if Assigned(Fdbcmd)
  then Result := Fdbcmd(dbproc, Cmd)
  else Result := Fdbcmd_stdcall(dbproc, Cmd)
end;

(** Determine whether the current command can return rows *)
function TZDBLIBPLainDriver.dbcmdrow(dbproc: PDBPROCESS): RETCODE;
begin
  if Assigned(Fdbcmdrow)
  then Result := Fdbcmdrow(dbproc)
  else Result := Fdbcmdrow_stdcall(dbproc)
end;

(** Determine whether the source of a regular result column is updatable
  through the DB-Library browse-mode facilities *)
function TZDBLIBPLainDriver.dbcolbrowse(dbproc: PDBPROCESS;
  Column: Integer): DBBOOL;
begin
  if Assigned(Fdbcolbrowse_SYB) then
    Result := Fdbcolbrowse_SYB(dbproc, Column)
  else if Assigned(Fdbcolbrowse_MS) then
    Result := Ord(Fdbcolbrowse_MS(dbproc, Column))
  else
    Result := Fdbcolbrowse_stdcall(dbproc, Column)
end;

(** Return the maximum length of the data in a regular result column *)
function TZDBLIBPLainDriver.dbcollen(dbproc: PDBPROCESS;
  Column: Integer): DBINT;
begin
  if Assigned(Fdbcollen)
  then Result := Fdbcollen(dbproc, Column)
  else Result := Fdbcollen_stdcall(dbproc, Column)
end;

(** Return the name of a regular result column *)
function TZDBLIBPLainDriver.dbcolname(dbproc: PDBPROCESS;
  Column: Integer): PAnsiChar;
begin
  if Assigned(Fdbcolname)
  then Result := Fdbcolname(dbproc, Column)
  else Result := Fdbcolname_stdcall(dbproc, Column)
end;

(** Return a pointer to the name of the database column from which the
    specified regular result column was derived *)
function TZDBLIBPLainDriver.dbcolsource(dbproc: PDBPROCESS;
  Column: Integer): PAnsiChar;
begin
  if Assigned(Fdbcolsource)
  then Result := Fdbcolsource(dbproc, Column)
  else Result := Fdbcolsource_stdcall(dbproc, Column)
end;

(** Return the datatype for a regular result column *)
function TZDBLIBPLainDriver.dbcoltype(dbproc: PDBPROCESS;
  Column: Integer): Integer;
begin
  if Assigned(Fdbcoltype)
  then Result := Fdbcoltype(dbproc, Column)
  else Result := Fdbcoltype_stdcall(dbproc, Column)
end;

(** Return precision and scale information for a regular
    result column of type numeric or decimal. *)
function TZDBLIBPLainDriver.dbcoltypeinfo(dbproc: PDBPROCESS;
  Column: Integer): PDBTYPEINFO;
begin
  if Assigned(Fdbcoltypeinfo)
  then Result := Fdbcoltypeinfo(dbproc, Column)
  else Result := Fdbcoltypeinfo_stdcall(dbproc, Column)
end;

(** Return the user-defined datatype for a regular result column *)
function TZDBLIBPLainDriver.dbcolutype(dbproc: PDBPROCESS;
  Column: Integer): DBINT;
begin
  if Assigned(Fdbcolutype)
  then Result := Fdbcolutype(dbproc, Column)
  else Result := Fdbcolutype_stdcall(dbproc, Column)
end;

(** Convert data from one datatype to another *)
function TZDBLIBPLainDriver.dbconvert(dbproc: PDBPROCESS; SrcType: Integer;
  Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte;
  DestLen: DBINT): Integer;
begin
  if Assigned(Fdbconvert)
  then Result := Fdbconvert(dbproc, SrcType, Src, SrcLen, DestType, Dest, DestLen)
  else Result := Fdbconvert_stdcall(dbproc, SrcType, Src, SrcLen, DestType, Dest, DestLen)
end;

(** Convert data from one datatype to another, with precision and scale
    support for numeric and decimal datatypes *)
function TZDBLIBPLainDriver.dbconvert_ps(dbproc: PDBPROCESS;
  SrcType: Integer; Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte;
  DestLen: DBINT; typinfo: PDBTYPEINFO): Integer;
begin
  if Assigned(Fdbconvert_ps)
  then Result := Fdbconvert_ps(dbproc, SrcType, Src, SrcLen, DestType, Dest, DestLen, typinfo)
  else Result := Fdbconvert_ps_stdcall(dbproc, SrcType, Src, SrcLen, DestType, Dest, DestLen, typinfo)
end;

(** Returns the number of rows affected by a Transact-SQL command *)
function TZDBLIBPLainDriver.dbcount(dbproc: PDBPROCESS): DBINT;
begin
  if Assigned(Fdbcount)
  then Result := Fdbcount(dbproc)
  else Result := Fdbcount_stdcall(dbproc);
end;

(** Return the number of the current command *)
function TZDBLIBPLainDriver.dbcurcmd(dbproc: PDBPROCESS): DBINT;
begin
  if Assigned(Fdbcurcmd_SYB) then
    Result := Fdbcurcmd_SYB(dbproc)
  else if Assigned(Fdbcurcmd_MS) then
    Result := Fdbcurcmd_MS(dbproc)
  else Result := Fdbcurcmd_stdcall(dbproc)
end;

(** Return the number of the row currently being read *)
function TZDBLIBPLainDriver.dbcurrow(dbproc: PDBPROCESS): DBINT;
begin
  if Assigned(Fdbcurrow)
  then Result := Fdbcurrow(dbproc)
  else Result := Fdbcurrow_stdcall(dbproc)
end;

(** Return a pointer to the data in a regular result column *)
function TZDBLIBPLainDriver.dbdata(dbproc: PDBPROCESS;
  Column: Integer): PByte;
begin
  if Assigned(Fdbdata)
  then Result := Fdbdata(dbproc, Column)
  else Result := Fdbdata_stdcall(dbproc, Column)
end;

(** Return the length of the data in a regular result column *)
function TZDBLIBPLainDriver.dbdatlen(dbproc: PDBPROCESS;
  Column: Integer): DBINT;
begin
  if Assigned(Fdbdatlen)
  then Result := Fdbdatlen(dbproc, Column)
  else Result := Fdbdatlen_stdcall(dbproc, Column)
end;

(** Determine whether a particular DBPROCESS is dead *)
function TZDBLIBPLainDriver.dbdead(dbproc: PDBPROCESS): DBBOOL;
begin
  if Assigned(Fdbdead)
  then Result := Fdbdead(dbproc)
  else if Assigned(Fdbdead_MS)
    then Result := Ord(Fdbdead_MS(dbproc))
    else Result := Fdbdead_stdcall(dbproc);
end;

(** Close and deallocate all DBPROCESS structures,
    and clean up any structures initialized by dbinit. *)
procedure TZDBLIBPLainDriver.dbexit;
begin
  if Assigned(Fdbexit)
  then Fdbexit
  else Fdbexit_stdcall;
end;

(** Return the number of the first row in the row buffer *)
function TZDBLIBPLainDriver.dbfirstrow(Proc: PDBPROCESS): DBINT;
begin
  if Assigned(Fdbfirstrow)
  then Result := Fdbfirstrow(Proc)
  else Result := Fdbfirstrow_stdcall(Proc);
end;

(** Clear the command buffer *)
procedure TZDBLIBPLainDriver.dbfreebuf(Proc: PDBPROCESS);
begin
  if Assigned(Fdbfreebuf)
  then Fdbfreebuf(Proc)
  else Fdbfreebuf_stdcall(Proc);
end;

(** Free the memory allocated by dbqual *)
procedure TZDBLIBPLainDriver.dbfreequal(Ptr: PAnsiChar);
begin
  if Assigned(Fdbfreequal)
  then Fdbfreequal(Ptr)
  else Fdbfreequal_stdcall(Ptr);
end;

(** Return a pointer to a character in the command buffer. *)
function TZDBLIBPLainDriver.dbgetchar(Proc: PDBPROCESS;
  N: Integer): PAnsiChar;
begin
  if Assigned(Fdbgetchar)
  then Result := Fdbgetchar(Proc, N)
  else Result := Fdbgetchar_stdcall(Proc, N);
end;

(** Get the name of the client character set from the DBPROCESS structure *)
function TZDBLIBPLainDriver.dbgetcharset(dbproc: PDBPROCESS): PAnsiChar;
begin
  if Assigned(Fdbgetcharset)
  then Result := Fdbgetcharset(dbproc)
  else Result := Fdbgetcharset_stdcall(dbproc);
end;

{** Read the specified row in the row buffer. *}
function TZDBLIBPLainDriver.dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
begin
  if Assigned(FdbGetRow)
  then Result := FdbGetRow(dbproc, Row)
  else Result := FdbGetRow_stdcall(dbproc, Row);
end;

{** Determine whether the current command or remote procedure call
  generated a return status number. *}
function TZDBLIBPLainDriver.dbHasRetStat(dbProc: PDBPROCESS): DBBOOL;
begin
  if Assigned(FdbHasRetStat_MS)
  then Result := Ord(FdbHasRetStat_MS(dbproc))
  else if Assigned(FdbHasRetStat_SYB)
    then Result := FdbHasRetStat_SYB(dbproc)
    else Result := FdbHasRetStat_stdcall(dbproc);
end;
{$ENDIF MSWINDOWS}

{** Initialize DB-Library. }
function TZDBLIBPLainDriver.dbIntit: RETCODE;
{$IFDEF MSWINDOWS}var P: PAnsiChar;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if Assigned(FdbInit_MS) then begin
    P := FdbInit_MS;
    if P <> nil then begin
      Result := DBSUCCEED;
      {$IFDEF UNICODE}
      FClientVersion := ZEncoding.PRawToUnicode(P, StrLen(P), zCP_us_ascii);
      {$ELSE}
      SetString(FClientVersion, P, StrLen(P));
      {$ENDIF}
    end else Result := DBFail
  end else if Assigned(FdbInit)
    then Result := FdbInit
    else Result := FdbInit_stdcall;
  {$ELSE MSWINDOWS}
  Result := FdbInit;
  {$ENDIF MSWINDOWS}
end;

{** Allocates a login record for use in dbopen *}
function TZDBLIBPLainDriver.dbLogin: PLOGINREC;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(FdbLogin)
  then Result := FdbLogin
  else Result := FdbLogin_stdcall;
  {$ELSE}
  Result := FdbLogin;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
{** Free a login record *}
procedure TZDBLIBPLainDriver.dbloginfree(loginptr: PLOGINREC);
begin
  if Assigned(Fdbloginfree)
  then Fdbloginfree(loginptr)
  else FdbLoginfree_stdcall(loginptr);
end;

{** Indicate whether there are more commands to be processed. *}
function TZDBLIBPLainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(FdbMoreCmds)
  then Result := FdbMoreCmds(dbProc)
  else Result := FdbMoreCmds_stdcall(dbProc);
end;

{** Return the name of the current database. *}
function TZDBLIBPLainDriver.dbname(dbProc: PDBPROCESS): PAnsiChar;
begin
  if Assigned(Fdbname)
  then Result := Fdbname(dbProc)
  else Result := Fdbname_stdcall(dbProc);
end;

{** Read the next result row into the row buffer and into any program variables
  that are bound to column data *}
function TZDBLIBPLainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  if Assigned(FdbNextRow)
  then Result := FdbNextRow(dbProc)
  else Result := FdbNextRow_stdcall(dbProc);
end;

{** Determine the number of regular columns for the current set of results *}
function TZDBLIBPLainDriver.dbNumCols(dbProc: PDBPROCESS): DBINT;
begin
  if Assigned(FdbNumCols)
  then Result := FdbNumCols(dbProc)
  else Result := FdbNumCols_stdcall(dbProc);
end;
{$ENDIF}

{** Create and initialize a DBPROCESS structure. *}
function TZDBLIBPLainDriver.dbOpen(Login: PLOGINREC;
  server: PAnsiChar): PDBPROCESS;
begin
  if Assigned(FTdsDbOpen) then
    Result := FTdsDbOpen(Login, server, Ord(ZFastCode.Pos('Sybase', GetDescription) > 0))
  else
    {$IFNDEF MSWINDOWS}
    Result := FdbOpen(Login, server);
    {$ELSE}
    if Assigned(FdbOpen)
    then Result := FdbOpen(Login, server)
    else Result := FdbOpen_stdcall(Login, server)
    {$ENDIF}
end;

{$IFDEF MSWINDOWS}
{** Set up the results of the next query. *}
function TZDBLIBPLainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(FdbResults)
  then Result := FdbResults(dbProc)
  else Result := FdbResults_stdcall(dbProc);
end;

{** Return a pointer to a return parameter value generated by a stored procedure. *}
function TZDBLIBPLainDriver.dbRetData(dbProc: PDBPROCESS;
  RetNum: Integer): Pointer;
begin
  if Assigned(FdbRetData)
  then Result := FdbRetData(dbProc, RetNum)
  else Result := FdbRetData_stdcall(dbProc, RetNum);
end;

{** Determine the length of a return parameter value
  generated by a stored procedure. *}
function TZDBLIBPLainDriver.dbRetLen(dbProc: PDBPROCESS;
  RetNum: Integer): DBINT;
begin
  if Assigned(FdbRetLen)
  then Result := FdbRetLen(dbProc, RetNum)
  else Result := FdbRetLen_stdcall(dbProc, RetNum);
end;

{** Determine the name of the stored procedure parameter
  associated with a particular return parameter value. *}
function TZDBLIBPLainDriver.dbRetName(dbProc: PDBPROCESS;
  RetNum: Integer): PAnsiChar;
begin
  if Assigned(FdbRetName)
  then Result := FdbRetName(dbProc, RetNum)
  else Result := FdbRetName_stdcall(dbProc, RetNum);
end;

{** Determine the stored procedure status number returned by the
  current command or remote procedure call.  *}
function TZDBLIBPLainDriver.dbRetStatus(dbProc: PDBPROCESS): DBINT;
begin
  if Assigned(FdbRetStatus)
  then Result := FdbRetStatus(dbProc)
  else Result := FdbRetStatus_stdcall(dbProc);
end;

{** Determine the datatype of a return parameter value
  generated by a stored procedure. *}
function TZDBLIBPLainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  if Assigned(FdbRetType)
  then Result := FdbRetType(dbProc, RetNum)
  else Result := FdbRetType_stdcall(dbProc, RetNum);
end;

{$ENDIF MSWINDOWS}

{** emmidate exceute the remote procedure call. *}
function TZDBLIBPLainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(FdbRpcExec)
  then Result := FdbRpcExec(dbProc)
  else if Assigned(FdbRpcSend) then begin
    Result := FdbRpcSend(dbProc);
    if Result = SUCCEED then
      Result := dbSqlOk(dbProc);
  end else begin
    Result := FdbRpcSend_stdCall(dbProc);
    if Result = SUCCEED then
      Result := FdbSqlOk_stdcall(dbProc);
  end;
  {$ELSE}
  Result := dbRpcExec(dbProc);
  if Result = SUCCEED then
    Result := dbSqlOk(dbProc);
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
{** Initialize a remote procedure call *}
function TZDBLIBPLainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar;
  Options: DBSMALLINT): RETCODE;
begin
  if Assigned(FdbRpcInit)
  then Result := FdbRpcInit(dbProc, RpcName, Options)
  else Result := FdbRpcInit_stdcall(dbProc, RpcName, Options);
end;

{** Add a parameter to a remote procedure call. *}
function TZDBLIBPLainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar;
  Status: Byte; Type_, MaxLen, DataLen: DBINT; Value: Pointer): RETCODE;
begin
  if Assigned(FdbRpcParam)
  then Result := FdbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value)
  else Result := FdbRpcParam_stdcall(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

{** Signal the end of a remote procedure call. *}
function TZDBLIBPLainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(FdbRpcSend)
  then Result := FdbRpcSend(dbProc)
  else Result := FdbRpcSend_stdcall(dbProc)
end;

{$ENDIF MSWINDOWS}

{** Set the application name in the LOGINREC structure *}
function TZDBLIBPLainDriver.dbSetLApp(Login: PLOGINREC;
  AppName: PAnsiChar): RETCODE;
var Item: DBINT;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS: Item := TDSDBSETAPP;
    lvtSybase:  Item := SYBDBSETAPP;
    else        Item := MSDBSETAPP;
  end;
  Result := dbsetLName(login, AppName, Item)
end;

{** Set the character set in the LOGINREC structure. *}
function TZDBLIBPLainDriver.dbSetLCharSet(Login: PLOGINREC;
  CharsetName: PAnsiChar): RETCODE;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS: Result := dbsetLName(login, CharsetName, TDSDBSETCHARSET);
    lvtSybase:  Result := dbsetLName(login, CharsetName, SYBDBSETCHARSET);
    else        Result := DBFAIL;
  end;
end;

{** Set the host name in the LOGINREC structure *}
function TZDBLIBPLainDriver.dbSetLHost(Login: PLOGINREC;
  HostName: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(login, HostName, DBSETHOST)
end;

{$IFDEF MSWINDOWS}
{** Set a value in the LOGINREC structure. *}
function TZDBLIBPLainDriver.dbsetLName(Login: PLOGINREC; Value: PAnsiChar;
  Item: Integer): RETCODE;
begin
  if Assigned(FdbsetLName)
  then Result := FdbsetLName(login, Value, Item)
  else Result := FdbsetLName_stdcall(login, Value, Item);
end;
{$ENDIF MSWINDOWS}

(** Set the national language name in the LOGINREC structure. *)
function TZDBLIBPLainDriver.dbSetLNatLang(Login: PLOGINREC;
  NatLangName: PAnsiChar): RETCODE;
var Item: DBINT;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS: Item := TDSDBSETLANG;
    lvtSybase:  Item := SYBDBSETLANG;
    else        Item := MSDBSETLANG;
  end;
  Result := dbsetLName(login, NatLangName, Item)
end;

{$IFDEF MSWINDOWS}
{** Set the number of seconds that DB-Library waits for a server response to
  a request for a DBPROCESS connection. *}
function TZDBLIBPLainDriver.dbSetLoginTime(Seconds: Integer): RETCODE;
begin
  if Assigned(FdbSetLoginTime)
  then Result := FdbSetLoginTime(Seconds)
  else Result := FdbSetLoginTime_stdcall(Seconds);
end;
{$ENDIF MSWINDOWS}

{** Set the TDS packet size in an application’s LOGINREC structure. *}
function TZDBLIBPLainDriver.dbSetLPacket(Login: PLOGINREC;
  packet_size: Word): RETCODE;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS: Result := dbsetLName(login, @packet_size, TDSDBSETPACKET);
    lvtSybase:  Result := dbsetLName(login, @packet_size, SYBDBSETPACKET);
    else        Result := {$IFDEF MSWINDOWS}Fdbsetlpacket{$ELSE}dbsetlpacket{$ENDIF}(Login, packet_size)
  end;
end;

{** Set the user server password in the LOGINREC structure. *}
function TZDBLIBPLainDriver.dbSetLPwd(Login: PLOGINREC;
  Password: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(login, Password, DBSETPWD);
end;

{** Set the TDS packet size in an application’s LOGINREC structure. *}
function TZDBLIBPLainDriver.dbSetLSecure(Login: PLOGINREC): RETCODE;
begin
  if FDBLibraryVendorType = lvtMS
  then Result := dbsetLName(login, nil, MSDBSETSECURE)
  else Result := DBFail;
end;

{** Set the user name in the LOGINREC structure. *}
function TZDBLIBPLainDriver.dbSetLUser(Login: PLOGINREC;
  UserName: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(login, UserName, DBSETUSER);
end;

{$IFDEF MSWINDOWS}
{** Set the maximum number of simultaneously open DBPROCESS structures. *}
function TZDBLIBPLainDriver.dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
begin
  if Assigned(FdbSetMaxprocs_s)
  then Result := FdbSetMaxprocs_s(MaxProcs)
  else if Assigned(FdbSetMaxprocs_i)
    then Result := FdbSetMaxprocs_i(MaxProcs)
    else Result := FdbSetMaxprocs_stdcall(MaxProcs);
end;

{** Set a server or DB-Library option. *}
function TZDBLIBPLainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer;
  Char_Param: PAnsiChar; Int_Param: Integer): RETCODE;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS: Result := FdbSetOpt_SYB(dbProc, Option, Char_Param, Int_Param);
    lvtSybase:  Result := FdbSetOpt_stdcall(dbProc, Option, Char_Param, Int_Param);
    else        Result := FdbSetOpt_MS(dbProc, Option, Char_Param);
  end;
end;
{$ENDIF MSWINDOWS}

function TZDBLIBPLainDriver.dbsetversion(Version: DBINT): RETCODE;
begin
  if Assigned(Fdbsetversion)
  then Result := Fdbsetversion(Version)
  else {$IFDEF MSWINDOWS}if Assigned(Fdbsetversion_stdcall)
    then Result := Fdbsetversion_stdcall(Version)
    else {$ENDIF}Result := DBFAIL;
end;

{$IFDEF MSWINDOWS}
{** Send a command batch to the server. *}
function TZDBLIBPLainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(FdbSqlExec)
  then Result := FdbSqlExec(dbProc)
  else Result := FdbSqlExec_stdcall(dbProc);
end;

{** Wait for results from the server and verify the correctness of the
  instructions the server is responding to. *}
function TZDBLIBPLainDriver.dbSqlOk(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(FdbSqlOk)
  then Result := FdbSqlOk(dbProc)
  else Result := FdbSqlOk_stdcall(dbProc);
end;

{** Send a command batch to the server and do not wait for a response. *}
function TZDBLIBPLainDriver.dbsqlsend(dbProc: PDBPROCESS): RETCODE;
begin
  if Assigned(Fdbsqlsend)
  then Result := Fdbsqlsend(dbProc)
  else Result := Fdbsqlsend_stdcall(dbProc);
end;

{** Use a particular database. }
function TZDBLIBPLainDriver.dbUse(dbProc: PDBPROCESS;
  dbName: PAnsiChar): RETCODE;
begin
  if Assigned(FdbUse)
  then Result := FdbUse(dbProc, dbName)
  else Result := FdbUse_stdcall(dbProc, dbName);
end;

{** Determine whether the specified regular result column’s data can vary in length. *}
function TZDBLIBPLainDriver.dbVaryLen(dbProc: PDBPROCESS;
  Column: Integer): DBBOOL;
begin
  if Assigned(FdbVaryLen_MS)
  then Result := Ord(FdbVaryLen_MS(dbProc, Column))
    else if Assigned(Fdbvarylen_SYB)
    then Result := Fdbvarylen_SYB(dbProc, Column)
    else Result := Fdbvarylen_stdcall(dbProc, Column);
end;
{$ENDIF MSWINDOWS}

destructor TZDBLIBPLainDriver.Destroy;
begin
  if Loader.Loaded then
    case FDBLibraryVendorType of
      {$IFDEF MSWINDOWS}
      lvtMS: begin
          dberrhandle(OldMsSQLErrorHandle);
          dbmsghandle(OldMsSQLMessageHandle);
          //fdbWinexit;
          fdbExit;
        end;
      {$ENDIF}
      lvtSybase: begin
          {$IFDEF MSWINDOWS}
          dberrhandle_stdcall(OldSybaseErrorHandle);
          dbmsghandle_stdcall(OldSybaseMessageHandle);
          fdbExit_stdcall;
          {$ELSE}
          dberrhandle(OldSybaseErrorHandle);
          dbmsghandle(OldSybaseMessageHandle);
          {$IFDEF MSWINDOWS}fdbExit{$ELSE}dbExit{$ENDIF};
          {$ENDIF}
        end;
      else begin //FreeTDS
          dberrhandle(OldFreeTDSErrorHandle);
          dbmsghandle(OldFreeTDSMessageHandle);
          {$IFDEF MSWINDOWS}fdbExit{$ELSE}dbExit{$ENDIF};
        end;
    end;
  inherited Destroy;
end;

{$IFDEF TEST_CALLBACK}
procedure TZDBLIBPLainDriver.DeRegisterErrorHandler(
  const Handler: TDbLibErrorHandler);
var MyIdx, OldIdx: Integer;
begin
  FCS.Enter;
  try
    MyIdx := FSQLErrorHandlerList.IndexOf(Handler);
    Assert(MyIdx <> -1, 'worng DeRegisterErrorHandler behavior');
    if (FSQLErrorHandlerList.Count = 1) then //assign given handle back
      {$IFDEF MSWINDOWS}
      if Assigned(dberrhandle_stdcall)
      then dberrhandle_stdcall(TDbLibErrorHandler(FSQLErrorHandlerList[MyIdx]).FOlddberrhandle_stdcall)
      else {$ENDIF} dberrhandle(TDbLibErrorHandler(FSQLErrorHandlerList[MyIdx]).FOlddberrhandle_cdecl)
    else begin
      if MyIdx = 0
      then OldIdx := MyIdx+1
      else OldIdx := MyIdx-1;
      {$IFDEF MSWINDOWS}
      if Assigned(dberrhandle_stdcall)
      then TDbLibErrorHandler(FSQLErrorHandlerList[OldIdx]).FOlddberrhandle_stdcall := dberrhandle_stdcall(TDbLibErrorHandler(FSQLErrorHandlerList[MyIdx]).FOlddberrhandle_stdcall)
      else {$ENDIF} TDbLibErrorHandler(FSQLErrorHandlerList[OldIdx]).FOlddberrhandle_cdecl := dberrhandle(TDbLibErrorHandler(FSQLErrorHandlerList[MyIdx]).FOlddberrhandle_cdecl);
    end;
    FSQLErrorHandlerList.Delete(MyIdx);
  finally
    FCS.Leave;
  end;
end;

procedure TZDBLIBPLainDriver.DeRegisterMessageHandler(
  const Handler: TDbLibMessageHandler);
var MyIdx, OldIdx: Integer;
begin
  FCS.Enter;
  try
    MyIdx := FSQLMessageHandlerList.IndexOf(Handler);
    Assert(MyIdx <> -1, 'worng DeRegisterMessageHandler behavior');
    if (FSQLMessageHandlerList.Count = 1) then
      {$IFDEF MSWINDOWS}
      if Assigned(dbmsghandle_stdcall)
      then dbmsghandle_stdcall(TDbLibMessageHandler(FSQLMessageHandlerList[MyIdx]).FOlddbmsghandle_stdcall)
      else {$ENDIF} dbmsghandle(TDbLibMessageHandler(FSQLMessageHandlerList[MyIdx]).FOlddbmsghandle_cdecl)
    else begin
      if MyIdx = 0
      then OldIdx := MyIdx+1
      else OldIdx := MyIdx-1;
      {$IFDEF MSWINDOWS}TDbLibMessageHandler(FSQLMessageHandlerList[OldIdx]).FOlddbMSGhandle_stdcall := TDbLibMessageHandler(FSQLMessageHandlerList[MyIdx]).FOlddbMSGhandle_stdcall;{$ENDIF MSWINDOWS}
      TDbLibMessageHandler(FSQLMessageHandlerList[OldIdx]).FOlddbMSGhandle_cdecl := TDbLibMessageHandler(FSQLMessageHandlerList[MyIdx]).FOlddbMSGhandle_cdecl;
    end;
    FSQLMessageHandlerList.Delete(MyIdx);
  finally
    FCS.Leave;
  end;
end;

{$ENDIF TEST_CALLBACK}
function TZDBLIBPLainDriver.GetDBOption(AOption: TdbOption): DBINT;
begin
  case FDBLibraryVendorType of
    lvtMS:  case AOption of
              dboptPARSEONLY:     Result := DBLIBDBPARSEONLY;
              dboptSHOWPLAN:      Result := DBLIBDBSHOWPLAN;
              dboptNOEXEC:        Result := DBLIBDBNOEXEC;
              dboptARITHIGNORE:   Result := DBLIBDBARITHIGNORE;
              dboptNOCOUNT:       Result := DBLIBDBNOCOUNT;
              dboptARITHABORT:    Result := DBLIBDBARITHABORT;
              dboptTEXTLIMIT:     Result := DBLIBDBTEXTLIMIT;
              dboptOFFSET:        Result := DBLIBDBOFFSET;
              dboptSTAT:          Result := DBLIBDBSTAT;
              dboptSTORPROCID:    Result := DBLIBDBSTORPROCID;
              dboptBUFFER:        Result := DBLIBDBBUFFER;
              dboptNOAUTOFREE:    Result := DBLIBDBNOAUTOFREE;
              dboptROWCOUNT:      Result := DBLIBDBROWCOUNT;
              dboptTEXTSIZE:      Result := DBLIBDBTEXTSIZE;
              dboptCLIENTCURSORS: Result := DBLIBDBCLIENTCURSORS;
              dboptSETTIME:       Result := DBLIBDBSET_TIME;
              dboptQUOTEDIDENT:   Result := DBLIBDBQUOTEDIDENT;
              dboptANSITOOEM:     Result := DBLIBDBANSITOOEM;
              dboptOEMTOANSI:     Result := DBLIBDBOEMTOANSI;
              else                Result := DBNOERR;
           end;
    else  case AOption of
              dboptPARSEONLY:     Result := TDSPARSEONLY;
              dboptESTIMATE:      Result := TDSESTIMATE;
              dboptSHOWPLAN:      Result := TDSSHOWPLAN;
              dboptNOEXEC:        Result := TDSNOEXEC;
              dboptARITHIGNORE:   Result := TDSARITHIGNORE;
              dboptNOCOUNT:       Result := TDSNOCOUNT;
              dboptARITHABORT:    Result := TDSARITHABORT;
              dboptTEXTLIMIT:     Result := TDSTEXTLIMIT;
              dboptBROWSE:        Result := TDSBROWSE;
              dboptOFFSET:        Result := TDSOFFSET;
              dboptSTAT:          Result := TDSSTAT;
              dboptERRLVL:        Result := TDSERRLVL;
              dboptCONFIRM:       Result := TDSCONFIRM;
              dboptSTORPROCID:    Result := TDSSTORPROCID;
              dboptBUFFER:        Result := TDSBUFFER;
              dboptNOAUTOFREE:    Result := TDSNOAUTOFREE;
              dboptROWCOUNT:      Result := TDSROWCOUNT;
              dboptTEXTSIZE:      Result := TDSTEXTSIZE;
              dboptNATLANG:       Result := TDSNATLANG;
              dboptDATEFORMAT:    Result := TDSDATEFORMAT;
              dboptPRPAD:         Result := TDSPRPAD;
              dboptPRCOLSEP:      Result := TDSPRCOLSEP;
              dboptPRLINELEN:     Result := TDSPRLINELEN;
              dboptPRLINESEP:     Result := TDSPRLINESEP;
              dboptLFCONVERT:     Result := TDSLFCONVERT;
              dboptDATEFIRST:     Result := TDSDATEFIRST;
              dboptCHAINXACTS:    Result := TDSCHAINXACTS;
              dboptFIPSFLAG:      Result := TDSFIPSFLAG;
              dboptISOLATION:     Result := TDSISOLATION;
              dboptAUTH:          Result := TDSAUTH;
              dboptIDENTITY:      Result := TDSIDENTITY;
              dboptNOIDCOL:       Result := TDSNOIDCOL;
              dboptDATESHORT:     Result := TDSDATESHORT;
              dboptCLIENTCURSORS: Result := TDSCLIENTCURSORS;
              dboptSETTIME:       Result := TDSSETTIME;
              dboptQUOTEDIDENT:   Result := TDSQUOTEDIDENT;
              else                Result := DBNOERR;
            end;
  end;
end;

procedure TZDBLIBPLainDriver.LoadApi;
begin
  with FLoader do begin
    //test for not exported methods to identify the libs:
    if (GetAddress('dbcolbrowse') <> nil) and //not&never exported by FreeTDS see: http://www.freetds.org/userguide/dblib.api.summary.htm
       (GetAddress('dbcoltypeinfo') <> nil) //not exported by ntwdblib.dll
       //so we link against a sybaselib with stdcall on windows and cdecl for all other Os's
    then FDBLibraryVendorType := lvtSybase
    else if (GetAddress('dbcoltypeinfo') <> nil) //ntwdblib.dll does not export that function
      then FDBLibraryVendorType := lvtFreeTDS
      else FDBLibraryVendorType := lvtMS;
     // if type sizes or names are different:
    case FDBLibraryVendorType of
      lvtFreeTDS: begin
          @{$IFDEF MSWINDOWS}Fdbdead{$ELSE}dbdead{$ENDIF} := GetAddress('dbdead', True);
          @{$IFDEF MSWINDOWS}Fdbcmdrow{$ELSE}dbcmdrow{$ENDIF} := GetAddress('dbcmdrow');
          @{$IFDEF MSWINDOWS}Fdbcount{$ELSE}dbcount{$ENDIF} := GetAddress('dbcount');
          @{$IFDEF MSWINDOWS}Fdbcurrow{$ELSE}dbcurrow{$ENDIF} := GetAddress('dbcurrow');
          @{$IFDEF MSWINDOWS}Fdbfirstrow{$ELSE}dbfirstrow{$ENDIF} := GetAddress('dbfirstrow');
          @{$IFDEF MSWINDOWS}Fdbclose_SYB{$ELSE}dbclose{$ENDIF} := GetAddress('dbclose'); //is a procedure
          @{$IFDEF MSWINDOWS}Fdbsetmaxprocs_I{$ELSE}dbSetMaxprocs{$ENDIF} := GetAddress('dbsetmaxprocs'); //uses DBINT
          @{$IFDEF MSWINDOWS}Fdbloginfree{$ELSE}dbloginfree{$ENDIF} := GetAddress('dbloginfree', True); //name diff to ms
          //@{$IFDEF MSWINDOWS}Fdbcolbrowse_SYB{$ELSE}dbcolbrowse{$ENDIF} := GetAddress('dbcolbrowse'); //no FreeTDS
          @{$IFDEF MSWINDOWS}FdbMoreCmds{$ELSE}dbMoreCmds{$ENDIF} := GetAddress('dbmorecmds'); //name diff to ms
          @{$IFDEF MSWINDOWS}FdbSetOpt_SYB{$ELSE}dbsetopt{$ENDIF} := GetAddress('dbsetopt'); //int_param is available but not computed always
          @{$IFDEF MSWINDOWS}FdbHasRetStat_SYB{$ELSE}dbHasRetStat{$ENDIF} := GetAddress('dbhasretstat'); //DBBOOL vs. LongBool
          @{$IFDEF MSWINDOWS}Fdbvarylen_SYB{$ELSE}dbvarylen{$ENDIF} := GetAddress('dbvarylen'); //DBBOOL vs. LongBool
          @Fdbinit := GetAddress('dbinit'); //Result is a RetCode
          @FdbSetVersion := GetAddress('dbsetversion');  //no MS but ms supports dbSetLVersion
          @dbsetlversion := GetAddress('dbsetlversion'); //no sybase see: https://lists.ibiblio.org/pipermail/freetds/2011q4/027489.html
          @tdsdump_on := GetAddress('tdsdump_on');
          @tdsdump_off := GetAddress('tdsdump_off');
          @tdsdump_open := GetAddress('tdsdump_open');
          @tdsdump_close := GetAddress('tdsdump_close');
        end;
      lvtSybase: begin //handle lower vs uppercase and the call conventions
          @{$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}dbdead{$ENDIF} := GetAddress('DBDEAD'); //as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}dbdead{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}dbdead{$ENDIF} := GetAddress('dbdead'); //lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}dbcmdrow{$ENDIF} := GetAddress('DBCMDROW'); //as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}dbcmdrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}dbcmdrow{$ENDIF} := GetAddress('dbcmdrow'); //lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}dbcount{$ENDIF} := GetAddress('DBCOUNT'); //as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}dbcount{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}dbcount{$ENDIF} := GetAddress('dbcount'); //lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}dbcurrow{$ENDIF} := GetAddress('DBCURROW'); //as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}dbcurrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}dbcurrow{$ENDIF} := GetAddress('dbcurrow'); //lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}dbfirstrow{$ENDIF} := GetAddress('DBFIRSTROW'); //as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}dbfirstrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}dbfirstrow{$ENDIF} := GetAddress('dbfirstrow'); //lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbclose_stdcall{$ELSE}dbclose{$ENDIF} := GetAddress('dbclose');
          @{$IFDEF MSWINDOWS}Fdbsetmaxprocs_stdcall{$ELSE}dbSetMaxprocs{$ENDIF} := GetAddress('dbsetmaxprocs');
          @{$IFDEF MSWINDOWS}Fdbloginfree_stdcall{$ELSE}dbloginfree{$ENDIF} := GetAddress('dbloginfree', True); //name diff
          @{$IFDEF MSWINDOWS}Fdbcolbrowse_stdcall{$ELSE}dbcolbrowse{$ENDIF} := GetAddress('dbcolbrowse'); //no FreeTDS
          @{$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}dbMoreCmds{$ENDIF} := GetAddress('DBMORECMDS'); //uppercase
          if not Assigned({$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}dbMoreCmds{$ENDIF}) then
            @{$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}dbMoreCmds{$ENDIF} := GetAddress('dbmorecmds'); //lowercase since 15+

          @{$IFDEF MSWINDOWS}FdbSetOpt_stdcall{$ELSE}dbsetopt{$ENDIF} := GetAddress('dbsetopt'); //int_param is available
          @{$IFDEF MSWINDOWS}FdbHasRetStat_stdcall{$ELSE}dbhasretstat{$ENDIF} := GetAddress('dbhasretstat'); //DBBOOL vs. LongBool
          @{$IFDEF MSWINDOWS}Fdbvarylen_stdcall{$ELSE}dbvarylen{$ENDIF} := GetAddress('dbvarylen'); //DBBOOL vs. LongBool
          @{$IFDEF MSWINDOWS}Fdbinit_stdcall{$ELSE}Fdbinit{$ENDIF} := GetAddress('dbinit'); //Result is a RetCode
          @{$IFDEF MSWINDOWS}FdbSetVersion_stdcall{$ELSE}FdbSetVersion{$ENDIF} := GetAddress('dbsetversion');  //no MS
        end;
      {$IFDEF MSWINDOWS}
      lvtMS: begin
          @FdbWinexit := GetAddress('dbwinexit');
          @Fdbdead_MS := GetAddress('dbdead', True);
          @Fdbcmdrow := GetAddress('dbcmdrow');
          @Fdbcount := GetAddress('dbcount');
          @Fdbcurrow := GetAddress('dbcurrow');
          @Fdbfirstrow := GetAddress('dbfirstrow');
          @Fdbclose_MS := GetAddress('dbclose');  //is a function
          @Fdbsetlpacket := GetAddress('dbsetlpacket'); //does not use the dbsetlname method
          @Fdbsetmaxprocs_s := GetAddress('dbsetmaxprocs'); //uses a two byte int
          @Fdbloginfree := GetAddress('dbfreelogin', True);//name diff
          @Fdbcolbrowse_MS := GetAddress('dbcolbrowse'); //no FreeTDS
          @FdbMoreCmds := GetAddress('dbmorecmds'); //name diff to ms
          @FdbSetOpt_MS := GetAddress('dbsetopt'); //int_param is not available
          @FdbHasRetStat_MS := GetAddress('dbhasretstat'); //DBBOOL vs. LongBool
          @Fdbvarylen_MS := GetAddress('dbvarylen'); //DBBOOL vs. LongBool
          @Fdbinit_MS := GetAddress('dbinit'); //Result is a PAnsiChar not a RetCode
          @dbsetlversion := GetAddress('dbsetlversion') //no sybase see: https://lists.ibiblio.org/pipermail/freetds/2011q4/027489.html
        end;
      {$ENDIF}
    end;
    { type sizes are equal -> call convention! }
    {$IFDEF MSWINDOWS}
    if FDBLibraryVendorType = lvtSybase then begin
      @Fdbadata_stdcall := GetAddress('dbadata');
      @Fdbadlen_stdcall := GetAddress('dbadlen');
      @Fdbaltbind_stdcall := GetAddress('dbaltbind');
      @Fdbaltbind_ps_stdcall := GetAddress('dbaltbind_ps');
      @Fdbaltcolid_stdcall := GetAddress('dbaltcolid');
      @Fdbaltlen_stdcall := GetAddress('dbaltlen');
      @Fdbaltop_stdcall := GetAddress('dbaltop');
      @Fdbalttype_stdcall := GetAddress('dbalttype');
      @Fdbanullbind_stdcall := GetAddress('dbanullbind');
      @Fdbbind_stdcall := GetAddress('dbbind');
      @Fdbbind_ps_stdcall := GetAddress('dbbind_ps');
      @Fdbbufsize_stdcall := GetAddress('dbbufsize');
      @Fdbbylist_stdcall := GetAddress('dbbylist');
      @Fdbcancel_stdcall := GetAddress('dbcancel');
      @Fdbcanquery_stdcall := GetAddress('dbcanquery');
      @Fdbclrbuf_stdcall := GetAddress('dbclrbuf');
      @Fdbcmd_stdcall := GetAddress('dbcmd');
      @Fdbclropt_stdcall := GetAddress('dbclropt');
      @Fdbcollen_stdcall := GetAddress('dbcollen');
      @Fdbcolname_stdcall := GetAddress('dbcolname');
      @Fdbcolsource_stdcall := GetAddress('dbcolsource');
      @Fdbcoltype_stdcall := GetAddress('dbcoltype');
      @Fdbcolutype_stdcall := GetAddress('dbcolutype');
      @Fdbcoltypeinfo_stdcall := GetAddress('dbcoltypeinfo'); //no MS ?
      @Fdbcolutype_stdcall := GetAddress('dbcolutype');
      @Fdbconvert_stdcall := GetAddress('dbconvert');
      @Fdbconvert_ps_stdcall := GetAddress('bconvert_ps'); //no MS
      @Fdbdata_stdcall := GetAddress('dbdata');
      @Fdbdatlen_stdcall := GetAddress('dbdatlen');
      @Fdbexit_stdcall := GetAddress('dbexit');
      @Fdbfreebuf_stdcall := GetAddress('dbfreebuf');
      @Fdbfreequal_stdcall := GetAddress('dbfreequal');
      @Fdbgetchar_stdcall := GetAddress('dbgetchar');
      @Fdbgetcharset_stdcall := GetAddress('dbgetcharset');
      @FdbGetRow_stdcall := GetAddress('dbgetrow');
      @FdbLogin_stdcall := GetAddress('dblogin');
      @Fdbname_stdcall := GetAddress('dbname');
      @FdbNextRow_stdcall := GetAddress('dbnextrow');
      @FdbNumCols_stdcall := GetAddress('dbnumcols');
      @FdbOpen_stdcall := GetAddress('dbopen');
      @FdbSetLoginTime_stdcall := GetAddress('dbsetlogintime');
      @FdbsetLName_stdcall := GetAddress('dbsetlname');
      @FdbSqlExec_stdcall := GetAddress('dbsqlexec');
      @FdbSqlOk_stdcall := GetAddress('dbsqlok');
      @FdbSqlSend_stdcall := GetAddress('dbsqlsend');
      @FdbResults_stdcall := GetAddress('dbresults');
      @FdbRetData_stdcall := GetAddress('dbretdata');
      @FdbRetLen_stdcall := GetAddress('dbretlen');
      @FdbRetName_stdcall := GetAddress('dbretname');
      @Fdbretstatus_stdcall := GetAddress('dbretstatus');
      @FdbRetType_stdcall := GetAddress('dbrettype');
      @Fdbrpcinit_stdcall := GetAddress('dbrpcinit');
      @FdbRpcParam_stdcall := GetAddress('dbrpcparam');
      @FdbRpcSend_stdcall := GetAddress('dbrpcsend');
      @Fdbuse_stdcall := GetAddress('dbuse');

      @dberrhandle_stdcall := GetAddress('dberrhandle');
      @dbmsghandle_stdcall := GetAddress('dbmsghandle');
      {$IFNDEF TEST_CALLBACK}
      OldSybaseErrorHandle := dberrhandle_stdcall(SybaseErrorHandle);
      OldSybaseMessageHandle := dbmsghandle_stdcall(SybaseMessageHandle);
      {$ENDIF}
    end else {$ENDIF}begin
      @{$IFDEF MSWINDOWS}Fdbadata{$ELSE}dbadata{$ENDIF} := GetAddress('dbadata');
      @{$IFDEF MSWINDOWS}Fdbadlen{$ELSE}dbadlen{$ENDIF} := GetAddress('dbadlen');
      @{$IFDEF MSWINDOWS}Fdbaltbind{$ELSE}dbaltbind{$ENDIF} := GetAddress('dbaltbind');
      @{$IFDEF MSWINDOWS}Fdbaltbind_ps{$ELSE}dbaltbind_ps{$ENDIF} := GetAddress('dbaltbind_ps');
      @{$IFDEF MSWINDOWS}Fdbaltcolid{$ELSE}dbaltcolid{$ENDIF} := GetAddress('dbaltcolid');
      @{$IFDEF MSWINDOWS}Fdbaltlen{$ELSE}dbaltlen{$ENDIF} := GetAddress('dbaltlen');
      @{$IFDEF MSWINDOWS}Fdbaltop{$ELSE}dbaltop{$ENDIF} := GetAddress('dbaltop');
      @{$IFDEF MSWINDOWS}Fdbalttype{$ELSE}dbalttype{$ENDIF} := GetAddress('dbalttype');
      @{$IFDEF MSWINDOWS}Fdbanullbind{$ELSE}dbanullbind{$ENDIF} := GetAddress('dbanullbind');
      @{$IFDEF MSWINDOWS}Fdbbind{$ELSE}dbbind{$ENDIF} := GetAddress('dbbind');
      @{$IFDEF MSWINDOWS}Fdbbind_ps{$ELSE}dbbind_ps{$ENDIF} := GetAddress('dbbind_ps');
      @{$IFDEF MSWINDOWS}Fdbbufsize{$ELSE}dbbufsize{$ENDIF} := GetAddress('dbbufsize');
      @{$IFDEF MSWINDOWS}Fdbbylist{$ELSE}dbbylist{$ENDIF} := GetAddress('dbbylist');
      @{$IFDEF MSWINDOWS}Fdbcancel{$ELSE}dbcancel{$ENDIF} := GetAddress('dbcancel');
      @{$IFDEF MSWINDOWS}Fdbcanquery{$ELSE}dbcanquery{$ENDIF} := GetAddress('dbcanquery');
      @{$IFDEF MSWINDOWS}Fdbclrbuf{$ELSE}dbclrbuf{$ENDIF} := GetAddress('dbclrbuf');
      @{$IFDEF MSWINDOWS}Fdbclropt{$ELSE}dbclropt{$ENDIF} := GetAddress('dbclropt');
      @{$IFDEF MSWINDOWS}Fdbcmd{$ELSE}dbcmd{$ENDIF} := GetAddress('dbcmd');
      @{$IFDEF MSWINDOWS}Fdbcollen{$ELSE}dbcollen{$ENDIF} := GetAddress('dbcollen');
      @dbcolinfo := GetAddress('dbcolinfo'); //no sybase but freeTDS and MS
      @{$IFDEF MSWINDOWS}Fdbcolname{$ELSE}dbcolname{$ENDIF} := GetAddress('dbcolname');
      @{$IFDEF MSWINDOWS}Fdbcolsource{$ELSE}dbcolsource{$ENDIF} := GetAddress('dbcolsource'); //no FreeTDS?
      @{$IFDEF MSWINDOWS}Fdbcoltype{$ELSE}dbcoltype{$ENDIF} := GetAddress('dbcoltype');
      @{$IFDEF MSWINDOWS}Fdbcoltypeinfo{$ELSE}dbcoltypeinfo{$ENDIF} := GetAddress('dbcoltypeinfo'); //no MS
      @{$IFDEF MSWINDOWS}Fdbcolutype{$ELSE}dbcolutype{$ENDIF} := GetAddress('dbcolutype');
      @{$IFDEF MSWINDOWS}Fdbconvert{$ELSE}dbconvert{$ENDIF} := GetAddress('dbconvert');
      @{$IFDEF MSWINDOWS}Fdbconvert_ps{$ELSE}dbconvert_ps{$ENDIF} := GetAddress('bconvert_ps'); //no MS
      @{$IFDEF MSWINDOWS}Fdbdata{$ELSE}dbdata{$ENDIF} := GetAddress('dbdata');
      {$IFDEF MSWINDOWS}@dbdataready := GetAddress('dbdata'); {$ENDIF}
      @{$IFDEF MSWINDOWS}Fdbdatlen{$ELSE}dbdatlen{$ENDIF} := GetAddress('dbdatlen');
      @{$IFDEF MSWINDOWS}Fdbexit{$ELSE}dbexit{$ENDIF} := GetAddress('dbexit');
      @{$IFDEF MSWINDOWS}Fdbfreebuf{$ELSE}dbfreebuf{$ENDIF} := GetAddress('dbfreebuf');
      @{$IFDEF MSWINDOWS}Fdbfreequal{$ELSE}dbfreequal{$ENDIF} := GetAddress('dbfreequal');
      @{$IFDEF MSWINDOWS}Fdbgetchar{$ELSE}dbgetchar{$ENDIF} := GetAddress('dbgetchar');
      @{$IFDEF MSWINDOWS}Fdbgetcharset{$ELSE}dbgetcharset{$ENDIF} := GetAddress('dbgetcharset');
      @{$IFDEF MSWINDOWS}FdbGetRow{$ELSE}dbGetRow{$ENDIF} := GetAddress('dbgetrow');
      @FdbLogin := GetAddress('dblogin');
      @{$IFDEF MSWINDOWS}Fdbname{$ELSE}dbname{$ENDIF} := GetAddress('dbname');
      @{$IFDEF MSWINDOWS}FdbNextRow{$ELSE}dbNextRow{$ENDIF} := GetAddress('dbnextrow');
      @{$IFDEF MSWINDOWS}FdbNumCols{$ELSE}dbnumcols{$ENDIF} := GetAddress('dbnumcols');
      @{$IFDEF MSWINDOWS}FdbResults{$ELSE}dbResults{$ENDIF} := GetAddress('dbresults');
      @Fdbopen := GetAddress('dbopen');
      @FtdsDbOpen := GetAddress('tdsdbopen');
      @{$IFDEF MSWINDOWS}Fdbretdata{$ELSE}dbretdata{$ENDIF} := GetAddress('dbretdata');
      @{$IFDEF MSWINDOWS}Fdbretlen{$ELSE}dbretlen{$ENDIF} := GetAddress('dbretlen');
      @{$IFDEF MSWINDOWS}Fdbretname{$ELSE}dbretname{$ENDIF} := GetAddress('dbretname');
      @{$IFDEF MSWINDOWS}Fdbretstatus{$ELSE}dbretstatus{$ENDIF} := GetAddress('dbretstatus');
      @{$IFDEF MSWINDOWS}Fdbrettype{$ELSE}dbrettype{$ENDIF} := GetAddress('dbrettype');
      @{$IFDEF MSWINDOWS}Fdbrpcinit{$ELSE}dbrpcinit{$ENDIF} := GetAddress('dbrpcinit');
      @{$IFDEF MSWINDOWS}Fdbrpcparam{$ELSE}dbrpcparam{$ENDIF} := GetAddress('dbrpcparam');
      @{$IFDEF MSWINDOWS}FdbRpcSend{$ELSE}dbRpcSend{$ENDIF} := GetAddress('dbrpcsend');

      @{$IFDEF MSWINDOWS}FdbSetLoginTime{$ELSE}dbSetLoginTime{$ENDIF} := GetAddress('dbsetlogintime');
      @{$IFDEF MSWINDOWS}FdbsetLName{$ELSE}dbsetLName{$ENDIF} := GetAddress('dbsetlname');
      @{$IFDEF MSWINDOWS}FdbSqlExec{$ELSE}dbSqlExec{$ENDIF} := GetAddress('dbsqlexec');
      @{$IFDEF MSWINDOWS}FdbSqlOk{$ELSE}dbSqlOk{$ENDIF} := GetAddress('dbsqlok');
      @{$IFDEF MSWINDOWS}FdbSqlSend{$ELSE}dbSqlSend{$ENDIF} := GetAddress('dbsqlsend');
      @{$IFDEF MSWINDOWS}Fdbuse{$ELSE}dbuse{$ENDIF} := GetAddress('dbuse');

      @dbmsghandle := GetAddress('dbmsghandle');
      @dberrhandle := GetAddress('dberrhandle');
      if FDBLibraryVendorType = lvtFreeTDS then begin
        OldFreeTDSErrorHandle := dberrhandle(FreeTDSErrorHandle);
        OldFreeTDSMessageHandle := dbmsghandle(FreeTDSMessageHandle);
      end else begin
        OldMsSQLErrorHandle := dberrhandle(DbLibErrorHandle);
        OldMsSQLMessageHandle := dbmsghandle(DbLibMessageHandle);
      end;
      Assert(dbintit = SUCCEED, 'dbinit failed');
    end;
  end;
end;

procedure TZDBLIBPLainDriver.LoadCodePages;
begin
  //inherited;
  { add FreeTDS supported charactersets }
  AddCodePage('UTF-8', 1, ceUTF8, zCP_UTF8,  '', 4, True);
  AddCodePage('ISO-8859-1', 2, ceAnsi, zCP_L1_ISO_8859_1, '', 1, True);
  AddCodePage('ASCII', 3, ceAnsi, zCP_us_ascii, '', 1, False);
end;

{$IFDEF TEST_CALLBACK}
function TZDBLIBPLainDriver.GetErrorHandler(dbProc: PDBPROCESS;
  ADBERRHANDLE_PROC: TDBERRHANDLE_PROC): IZDBLibErrorHandler;
var DbLibErrorHandler: TDbLibErrorHandler;
begin
  FCS.Enter;
  Result := nil;
  try
    DbLibErrorHandler := TDbLibErrorHandler.Create(dbProc, ADBERRHANDLE_PROC, Self);
    Result := DbLibErrorHandler;
    FSQLErrorHandlerList.Add(DbLibErrorHandler);
  finally
    FCS.Leave;
  end;
end;

function TZDBLIBPLainDriver.GetMessageHandler(dbProc: PDBPROCESS;
  ADBMSGHANDLE_PROC: TDBMSGHANDLE_PROC): IZDBLibMessageHandler;
var DbLibMessageHandler: TDbLibMessageHandler;
begin
  FCS.Enter;
  Result := nil;
  try
    DbLibMessageHandler := TDbLibMessageHandler.Create(dbProc, ADBMSGHANDLE_PROC, Self);
    Result := DbLibMessageHandler;
    FSQLMessageHandlerList.Add(DbLibMessageHandler);
  finally
    FCS.Leave;
  end;
end;

constructor TZMethodToDllCallbackProcedure.Create(const Instance: TObject;
  methodAddr: pointer; const CallBackManager: TZDBLIBPLainDriver);
begin
  inherited Create(Instance, methodAddr);
  FManager := CallBackManager;
end;

{ TDbLibErrorHandler }

procedure TDbLibErrorHandler.BeforeDestruction;
begin
  inherited;
  FManager.DeRegisterErrorHandler(Self);
end;

constructor TDbLibErrorHandler.Create(Handle: PDBPROCESS;
  DBERRHANDLE_PROC: TDBERRHANDLE_PROC; const Manager: TZDBLIBPLainDriver);
var P: Pointer;
begin
  P := GetProcedureAddress;
  {$IFDEF MSWINDOWS}
  if Manager.FDBLibraryVendorType = lvtSybase then begin
    inherited Create(Self, @TDbLibErrorHandler.dberrhandle_stdcall, Manager);
    FOlddberrhandle_stdcall := Manager.dberrhandle_stdcall(P);
  end else {$ENDIF}begin
    inherited Create(Self, @TDbLibErrorHandler.dberrhandle_cdecl, Manager);
    FOlddberrhandle_cdecl := Manager.dberrhandle(P);
  end;
  FHandle := Handle;
  FMydberrhandle := DBERRHANDLE_PROC;
end;

function TDbLibErrorHandler.dberrhandle_cdecl(Proc: PDBPROCESS;
  Severity, DbErr, OsErr: Integer; DbErrStr, OsErrStr: PAnsiChar): Integer;
begin
  if FHandle = Proc
  then Result := FMydberrhandle(Proc, Severity, DbErr, OsErr, DbErrStr, OsErrStr)
  else if Assigned(FOlddberrhandle_cdecl)
    then Result := FOlddberrhandle_cdecl(Proc, Severity, DbErr, OsErr, DbErrStr, OsErrStr)
    else Result := INT_CANCEL;
end;

{$IFDEF MSWINDOWS}
function TDbLibErrorHandler.dberrhandle_stdcall(Proc: PDBPROCESS;
  Severity, DbErr, OsErr: Integer; DbErrStr, OsErrStr: PAnsiChar): Integer;
begin
  if FHandle = Proc
  then Result := FMydberrhandle(Proc, Severity, DbErr, OsErr, DbErrStr, OsErrStr)
  else if Assigned(FOlddberrhandle_stdcall)
    then Result := FOlddberrhandle_stdcall(Proc, Severity, DbErr, OsErr, DbErrStr, OsErrStr)
    else Result := INT_CANCEL;
end;
{$ENDIF MSWINDOWS}

{ TDbLibMessageHandler }

procedure TDbLibMessageHandler.BeforeDestruction;
begin
  inherited;
  FManager.DeRegisterMessageHandler(Self);
end;

constructor TDbLibMessageHandler.Create(
  Handle: PDBPROCESS; DBMSGHANDLE_PROC: TDBMSGHANDLE_PROC;
  const Manager: TZDBLIBPLainDriver);
var P: Pointer;
begin
  P := GetProcedureAddress;
  {$IFDEF MSWINDOWS}
  if Manager.FDBLibraryVendorType = lvtSybase then begin
    inherited Create(Self, @TDbLibMessageHandler.dbmsghandle_stdcall, Manager);
    FOlddbmsghandle_stdcall := Manager.dbmsghandle_stdcall(P);
  end else {$ENDIF}begin
    inherited Create(Self, PAnsiChar(@TDbLibMessageHandler.dbmsghandle_cdecl), Manager);
    FOlddbmsghandle_cdecl := Manager.dbmsghandle(P);
  end;
  FMydbmsghandle := DBMSGHANDLE_PROC;
  FHandle := Handle;
end;

function TDbLibMessageHandler.dbmsghandle_cdecl(Proc: PDBPROCESS; MsgNo: DBINT;
  MsgState, Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar;
  Line: DBUSMALLINT): Integer;
begin
  if FHandle = Proc
  then Result := FMydbmsghandle(Proc, MsgNo, MsgState, Severity, MsgText, SrvName, ProcName, Line)
  else if Assigned(FOlddbmsghandle_cdecl)
    then Result := FOlddbmsghandle_cdecl(Proc, MsgNo, MsgState, Severity, MsgText, SrvName, ProcName, Line)
    else Result := INT_EXIT;
end;

{$IFDEF MSWINDOWS}
function TDbLibMessageHandler.dbmsghandle_stdcall(Proc: PDBPROCESS;
  MsgNo: DBINT; MsgState, Severity: Integer; MsgText, SrvName,
  ProcName: PAnsiChar; Line: DBUSMALLINT): Integer;
begin
  if FHandle = Proc
  then Result := FMydbmsghandle(Proc, MsgNo, MsgState, Severity, MsgText, SrvName, ProcName, Line)
  else if Assigned(FOlddbmsghandle_stdcall)
    then Result := FOlddbmsghandle_stdcall(Proc, MsgNo, MsgState, Severity, MsgText, SrvName, ProcName, Line)
    else Result := INT_EXIT;
end;
{$ENDIF}
{$ENDIF TEST_CALLBACK}

{ TMSSQLDBLibPLainDriver }

procedure TMSSQLDBLibPLainDriver.AfterConstruction;
begin
  inherited;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation('ntwdblib.dll');
  {$ENDIF}
end;

function TMSSQLDBLibPLainDriver.Clone: IZPlainDriver;
begin
  Result := TMSSQLDBLibPLainDriver.Create;
end;

function TMSSQLDBLibPLainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for SQL Server';
end;

function TMSSQLDBLibPLainDriver.GetProtocol: string;
begin
  Result := 'mssql'
end;

procedure TMSSQLDBLibPLainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  AddmMSCodePages(Self);
end;

{ TSybaseDBLibPLainDriver }

procedure TSybaseDBLibPLainDriver.AfterConstruction;
begin
  inherited AfterConstruction;
  FLoader.AddLocation('libsybdb'+SharedSuffix);
end;

function TSybaseDBLibPLainDriver.Clone: IZPlainDriver;
begin
  Result := TSybaseDBLibPLainDriver.Create;
end;

function TSybaseDBLibPLainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for Sybase ASE';
end;

function TSybaseDBLibPLainDriver.GetProtocol: string;
begin
  Result := 'sybase'
end;

procedure TSybaseDBLibPLainDriver.LoadApi;
begin
  inherited LoadApi;
  {$IFDEF MSWINDOWS}
  if assigned(FdbSetVersion) then begin
    if FdbSetVersion(TDSDBVERSION_100) = DBFAIL then
      Assert(FdbSetVersion(TDSDBVERSION_46) = DBSUCCEED, 'failed to set the TDS version')
  end else begin
    if FdbSetVersion_stdcall(TDSDBVERSION_100) = DBFAIL then
      Assert(FdbSetVersion_stdcall(TDSDBVERSION_46) = DBSUCCEED, 'failed to set the TDS version');
  end;
  {$ELSE}
    if dbSetVersion(TDSDBVERSION_100) = DBFAIL then
      Assert(dbSetVersion(TDSDBVERSION_46) = DBSUCCEED, 'failed to set the TDS version')
  {$ENDIF}
end;

procedure TSybaseDBLibPLainDriver.LoadCodePages;
begin
  inherited;
  AddSybaseCodePages(Self);
end;

{ TZDBLibErrorList }

procedure TZDBLibErrorList.Clear;
var I: Integer;
begin
  for i := Count -1 downto 0 do
    Dispose(PDBLibError(Items[i]));
  inherited;
end;

{ TZDBLibMessageList }

procedure TZDBLibMessageList.Clear;
var I: Integer;
begin
  for i := Count -1 downto 0 do
    Dispose(PDBLibMessage(Items[i]));
  inherited;
end;

initialization
  SQLErrors := TZDBLibErrorList.Create;
  SQLMessages := TZDBLibMessageList.Create;
  ErrorCS := TCriticalSection.Create;
finalization
  FreeAndnil(ErrorCS);
//Free any record in the list if any
  FreeAndNil(SQLErrors);
//Free any record in the list if any
  FreeAndNil(SQLMessages);

{$ENDIF ZEOS_DISABLE_DBLIB}

end.
