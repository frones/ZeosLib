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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainDbLibDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZCompatibility, ZPlainDriver, ZPlainDbLibConstants;

const
  NTWDBLIB_DLL_LOCATION ='ntwdblib.dll';
  LIBSYBDB_WINDOWS_DLL_LOCATION = 'libsybdb.dll';
  LIBSYBDB_LINUX_DLL_LOCATION = 'libsybdb.so';
  FREETDS_WINDOWS_DLL_LOCATION = 'msdblibr.dll';
  FREETDS_LINUX_DLL_LOCATION = 'dblib.so';
  FREETDS_OSX_DLL_LOCATION = 'dblib.dylib';

{***************** Plain API Constants definition ****************}
  DBNOERR               = -1;
  DBFAIL                = 0;
  DBSUCCEED             = 1;
  DBFINDONE             = $04;  { Definately done }
  DBMORE                = $10;  { Maybe more commands waiting }
  DBMORE_ROWS           = $20;  { This command returned rows }
{ DB-Library datatypes }
type
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
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;
  DBXLATE               = Pointer;
  DBSORTORDER           = Pointer;
  DBLOGINFO             = Pointer;
  DBVOIDPTR             = PPointer;
const
{ Decimal constants }
  MAXNUMERICLEN = 16;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  MAXTABLENAME = 30;
  MAXCOLNAMELEN= 30;

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
  TDSNUMOPTIONS             = 36;
  TDSPADOFF                 = 0;
  TDSPADON                  = 1;
  TDSOFF                    = 0;
  TDSON                     = 1;

  NOSUCHOPTION              = 2;

  MAXOPTTEXT                = 32;
{ Sybase Login manipulations }
const
  SYBDBSETHOST              = 1;
  SYBDBSETUSER              = 2;
  SYBDBSETPWD               = 3;
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
  MSDBSETHOST             = 1;
  MSDBSETUSER             = 2;
  MSDBSETPWD              = 3;
  MSDBSETAPP              = 4;
  MSDBSETID               = 5;
  MSDBSETLANG             = 6;

  MSDBSETSECURE           = 7;
  MSDBSET_LOGIN_TIME      = 10;
  MSDBSETFALLBACK         = 12;

{TDS Loginrec manipulations}
  TDSDBSETHOST               = 1;
  TDSDBSETUSER               = 2;
  TDSDBSETPWD                = 3;
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

type
  TDBVariables = record
    dboptions: array[0..44]  of ShortInt;
    dbSetLoginRec: array[0..16] of ShortInt;
  End;

{ DB-Library datatypes }
  {$IFDEF FPC}
    {$PACKRECORDS C}
  {$ENDIF}

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

  PTDSDBDATETIME = ^TTDSDBDATETIME;
  TTDSDBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	DBINT;       // 300ths of a second since midnight, 25920000 unit is 1 day
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

  PDBDATETIME = ^DBDATETIME;
  DBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	ULONG;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;

type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;
  DBDECIMAL = DBNUMERIC;

  DBVARYCHAR = packed record
    Len: DBSMALLINT;
    Str: array[0..DBMAXCHAR-1] of DBCHAR; //CHAR = Wide D12UP
  end;

  DBVARYBIN = packed record
    Len: DBSMALLINT;
    Bytes: array[0..DBMAXCHAR-1] of Byte;
  end;

  {DBMONEY4 = packed record
	  mny4: DBINT;
  end;

type
{ TODO -ofjanos -cAPI :
Strange but I had to insert X1 and X2 into the structure to make it work.
I have not find any reason for this yet. }
  DBCOL = packed record
    SizeOfStruct: DBINT;
    Name:       array[0..MAXCOLNAMELEN] of AnsiChar;
    ActualName: array[0..MAXCOLNAMELEN] of AnsiChar;
    TableName:  array[0..MAXTABLENAME] of AnsiChar;
    {$IFNDEF FPC}
    X1:         Byte;
    {$ENDIF}
    Typ:        SmallInt;
    UserType:   DBINT;
    MaxLength:  DBINT;
    Precision:  Byte;
    Scale:      Byte;
    VarLength:  LongBool;{ TRUE, FALSE }
    Null:       Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive: Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:  Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:   LongBool;{ TRUE, FALSE }
    {$IFNDEF FPC}
    X2:         Byte;
    {$ENDIF}
  end;
  PDBCOL = ^DBCOL;

  PTDSDBCOL = ^TTDSDBCOL;
  TTDSDBCOL = packed record
    SizeOfStruct: DBINT;
    Name:       array[0..MAXCOLNAMELEN+2] of AnsiChar;
    ActualName: array[0..MAXCOLNAMELEN+2] of AnsiChar;
    TableName:  array[0..MAXTABLENAME+2] of AnsiChar;
    Typ:        SmallInt;
    UserType:   DBINT;
    MaxLength:  DBINT;
    Precision:  Byte;
    Scale:      Byte;
    VarLength:  LongBool;{ TRUE, FALSE }
    Null:       Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive: Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:  Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:   LongBool;{ TRUE, FALSE }
  end;
{common FreeTDS(dblib.dll) and ntwdblib.dll definitions
  requirements: the sam call convention }
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

  Tdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
  Tdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column, VarType: Integer;
    VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; cdecl;
  Tdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; cdecl;
  Tdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; cdecl;
  Tdbcancel = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcanquery = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbchange = function(Proc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); cdecl;
  Tdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; cdecl;
  Tdbcmd = function(Proc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
  Tdbcmdrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcolname = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
    SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
  Tdbiscount = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbcurcmd = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; cdecl;
  Tdbcursor = function(hCursor: PDBCURSOR; OpType, Row: DBINT; Table, Values: PAnsiChar): RETCODE; cdecl;
  Tdbexit = procedure; cdecl;
  Tdbfcmd = function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
  Tdbfirstrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbfreebuf = procedure(Proc: PDBPROCESS); cdecl;
  Tdbfreequal = procedure(Ptr: PAnsiChar); cdecl;
  Tdbgetchar = function(Proc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
  Tdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT; StartFrom: Integer): Integer; cdecl;
  Tdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbgettime = function: Integer; cdecl;
  Tdblastrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdblogin = function: PLOGINREC; cdecl;
  Tdbmorecmds = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; cdecl;
  Tdbname = function(Proc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbnextrow = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; cdecl;
  Tdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; cdecl;
  Tdbnumcols = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumcompute = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumorders = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumrets = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbopen = function(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; cdecl;
  Tdbprhead = procedure(Proc: PDBPROCESS); cdecl;
  Tdbprrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbprtype = function(Token: Integer): PAnsiChar; cdecl;
  Tdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PAnsiChar): PAnsiChar; cdecl;
  Tdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; cdecl;
  Tdbreadtext = function(dbproc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; cdecl;
  Tdbresults = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbretdata = function(dbproc: PDBPROCESS; RetNum: Integer): PByte; cdecl;
  Tdbretlen = function(dbproc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
  Tdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
  Tdbretstatus = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; cdecl;
  Tdbrows = function(Proc: PDBPROCESS): RETCODE; cdecl; //!!!
  Tdbrowtype = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbrpcinit = function(Proc: PDBPROCESS; ProcName: PAnsiChar; Options: DBSMALLINT):
    RETCODE; cdecl; //!!!
  Tdbrpcparam = function(Proc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; cdecl;
  Tdbrpcsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbrpwclr = procedure(Login: PLOGINREC); cdecl;
  Tdbsetavail = procedure(Proc: PDBPROCESS); cdecl;
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; cdecl;
  Tdbsettime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); cdecl;
  Tdbsqlexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlok = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PAnsiChar):
    RETCODE; cdecl;
  Tdbstrlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabname = function(Proc: PDBPROCESS; Table: Integer): PAnsiChar; cdecl;
  Tdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PAnsiChar; cdecl;
  Tdbtsnewlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsLen,
    TabNum: Integer; TableName: PAnsiChar): RETCODE; cdecl;
  Tdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; cdecl;
  Tdbuse = function(Proc: PDBPROCESS; DbName: PAnsiChar): RETCODE; cdecl;
  Tdbwritetext = function(Proc: PDBPROCESS; ObjName: PAnsiChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; cdecl;
  (* LOGINREC manipulation *)
  Tdbsetlname = function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; cdecl;
{ BCP functions }
  Tbcp_batch = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; cdecl;
  Tbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; cdecl;
  Tbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; cdecl;
  Tbcp_done = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; cdecl;
  Tbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
    Direction: Integer): RETCODE; cdecl;
  Tbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; cdecl;
  Tbcp_readfmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; cdecl;
  Tbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tbcp_setl = function(Login: PLOGINREC; Enable: LongBool): RETCODE; cdecl;
  Tbcp_writefmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; cdecl;
  { Two-phase commit functions }
  Tabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tbuild_xact_string = procedure(XActName, Service: PAnsiChar; CommId: DBINT;
    Result: PAnsiChar); cdecl;
  Tclose_commit = procedure(Proc: PDBPROCESS); cdecl;
  Tcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Topen_commit = function(Login: PLOGINREC; ServerName: PAnsiChar): PDBPROCESS; cdecl;
  Tremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer):
    RETCODE; cdecl;
  Tscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PAnsiChar;
    SiteCount: Integer): DBINT; cdecl;
  Tstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; cdecl;



{FreeTDS spezial API definitions}
  TFreeTDSdb12hour = function(Proc: PDBPROCESS; Language: PAnsiChar): DBBOOL; cdecl;
  TFreeTDSdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
  TFreeTDSdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen, POutLen: DBINT;
    VarAddr: PByte; DBTYPEINFO: PDBTYPEINFO): RETCODE; cdecl;
  TFreeTDSdbcursorclose = procedure(DbHandle: PDBHANDLE); cdecl;
  TFreeTDSdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: DBINT; ColName: PAnsiChar;
    ColType, ColLen, UserType: PDBINT): RETCODE; cdecl;
  TFreeTDSdbcursorfetch   = function(hCursor: PDBCURSOR; FetchType, RowNum: DBINT): RETCODE; cdecl;
  TFreeTDSdbcursorinfo    = function(hCursor: PDBCURSOR; nCols, nRows: PDBINT): RETCODE; cdecl;
  TFreeTDSdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: DBSHORT; nRows: DBUSMALLINT; PStatus: PDBINT): PDBCURSOR; cdecl;

  TFreeTDSdbaltbind_ps    = function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE;
  TFreeTDSdbbind_ps       = function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
  TFreeTDSdbbufsize       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbclose         = procedure(dbproc: PDBPROCESS); cdecl;
  TFreeTDSdbtablecolinfo  = function(dbproc: PDBPROCESS; Column: DBINT; DbColumn: PTDSDBCOL): RETCODE;
  TFreeTDSdbcolinfo       = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer; DbColumn: PTDSDBCOL): RETCODE; cdecl;
  TFreeTDSdbconvert_ps    = function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; cdecl;
  TFreeTDSdbcount         = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbdatecmp       = function(dbproc: PDBPROCESS; d1, d2: PTDS_DBDATEREC): Integer;
  TFreeTDSdbdatecrack     = function(dbproc: PDBPROCESS; DateInfo: PTDS_DBDATEREC; DateType: PTDSDBDATETIME): RETCODE; cdecl;
  TFreeTDSdbdatlen        = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  TFreeTDSdbdead          = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbgetcharset    = function(dbproc: PDBPROCESS): PAnsiChar;
  TFreeTDSdbgetlusername  = function(login: PLOGINREC; name_buffer: PByte; buffer_len: Integer): Integer; cdecl;
  TFreeTDSdbgetmaxprocs   = function: Integer; cdecl;
  TFreeTDSdbgetnatlanf    = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  TFreeTDSdbgetpacket     = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbgetuserdata   = function(dbproc: PDBPROCESS): PByte; cdecl;
  TFreeTDSdbhasretstat    = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbinit          = function:RETCODE; cdecl;
  TFreeTDSdbiordesc       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbiowdesc       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbisavail       = function(Proc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbisopt         = function(Proc: PDBPROCESS; Option: Integer; const Param: PAnsiChar): DBBOOL; cdecl;
  TFreeTDSdbloginfree     = procedure(Login: PLOGINREC); cdecl;
  TFreeTDSdbmny4cmp       = function(dbproc: PDBPROCESS; m1, m: PDBMONEY4): Integer; cdecl;
  TFreeTDSdbmnycmp        = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY): Integer; cdecl;
  TFreeTDSdbmny4add       = function(dbproc: PDBPROCESS; m1, m2, sum: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnydec        = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnyinc        = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnymaxpos     = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnymaxneg     = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4minus     = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnyminus      = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4sub       = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnysub        = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4copy      = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnycopy       = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4zero      = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnyzero       = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmonthname     = function(dbproc: PDBPROCESS; language: PAnsiChar; monthnum: Integer; shortform: DBBOOL): PAnsiChar; cdecl;
  TFreeTDSdbopen          = function(Login: PLOGINREC; const Server: PAnsiChar; msdblib: Integer): PDBPROCESS; cdecl;
  TFreeTDSdbrecftos       = procedure(const FileName: PAnsiChar);
  TDRBUF                  = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbresults_r     = function(dbproc: PDBPROCESS; Recursive: Integer): RETCODE; cdecl;
  TFreeTDSdbsafestr       = function(dbproc: PDBPROCESS; const Src: PAnsiChar; SrcLen: DBINT; Dest: PAnsiChar; DestLen: DBINT; QuoteType: integer): RETCODE; cdecl;
  TFreeTDSdbservcharset   = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  TFreeTDSdbsetdefcharset = function(Charset: PAnsiChar): RETCODE; cdecl;
  TFreeTDSdbsetifile      = procedure(FileName: PAnsiChar); cdecl;
  TFreeTDSdbsetmaxprocs   = function(MaxProcs: Integer): RETCODE; cdecl;
  TFreeTDSdbsetopt        = function(dbproc: PDBPROCESS; Option: DBINT; Param: PAnsiChar; int_param: DBINT): RETCODE; cdecl;
  TFreeTDSdbsetrow        = function(dbproc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  TFreeTDSdbsetversion    = function(Version: DBINT): RETCODE; cdecl;
  TFreeTDSdbspid          = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbspr1row       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  TFreeTDSdbspr1rowlen    = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbsprhead       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  TFreeTDSdbsprline       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT; line_char: DBCHAR): RETCODE; cdecl;
  TFreeTDSdbvarylen       = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  TFreeTDSdbtds           = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbtextsize      = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbwillconvert   = function(SrcType, DestType: Integer): DBBOOL; cdecl;
  TFreeTDSdbtabbrowse     = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; cdecl;
  (* LOGINREC manipulation *)
  TFreeTDSdbsetlbool      = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  TFreeTDSdbsetllong      = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  TFreeTDSdbsetlversion   = function(Login: PLOGINREC; Version: Byte): RETCODE; cdecl;
  Ttdsdump_on = procedure ; cdecl;
  Ttdsdump_off = procedure ; cdecl;
  Ttdsdump_open = function (FileName : PAnsiChar): Integer; cdecl;
  Ttdsdump_close = procedure ; cdecl;
  T_tds_socket_init = procedure ; cdecl;
  T_tds_socket_done = procedure ; cdecl;


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
  //TFreeTDSdbsechandle    = function(_Type: DBINT ; Handler: INTFUNCPTR): PRETCODE; cdecl;
  //TFreeTDSdbsetbusy      = procedure(dbproc: PDBPROCESS; BusyFunc: DB_DBBUSY_FUNC);  cdecl;
  //TFreeTDSdbsetinterrupt = procedure(dbproc: PDBPROCESS; chkintr: DB_DBCHKINTR_FUNC; hndlintr: DB_DBHNDLINTR_FUNC);



{MsSQL-spezial API definitions}

{ Standard DB-Library functions }
  TMsSQLdbclose = function(Proc: PDBPROCESS): RETCODE; cdecl;
  TMsSQLdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  TMsSQLdbcolinfo = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer;
    DbColumn: PDBCOL): RETCODE; cdecl;
  TMsSQLdbcount = function(Proc: PDBPROCESS): Integer; cdecl;

  TMsSQLdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; cdecl;
  TMsSQLdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; cdecl;
  TMsSQLdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PAnsiChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; cdecl;
  TMsSQLdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer): RETCODE; cdecl;
  TMsSQLdbcursorfetchex = function(hCursor: PDBCURSOR; FetchType: Integer; RowNum,
    nFetchRows, Reserved: DBINT): RETCODE; cdecl;
  TMsSQLdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; cdecl;
  TMsSQLdbcursorinfoex = function(hCursor: PDBCURSOR; DbCursorInfo: PDBCURSORINFO):
    RETCODE; cdecl;
  TMsSQLdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; cdecl;
  TMsSQLdbdataready = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; cdecl;
  TMsSQLdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  TMsSQLdbdead = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbWinexit = procedure; cdecl;
  TMsSQLdbenlisttrans = function(Proc: PDBPROCESS; Transaction: Pointer): RETCODE; cdecl;
  TMsSQLdbenlistxatrans = function(Proc: PDBPROCESS; EnlistTran: LongBool): RETCODE; cdecl;
  TMsSQLdbgetmaxprocs = function: SmallInt; cdecl;
  TMsSQLdbgetpacket = function(Proc: PDBPROCESS): Cardinal; cdecl;
  TMsSQLdbgetuserdata = function(Proc: PDBPROCESS): Pointer; cdecl;
  TMsSQLdbhasretstat = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbinit = function: PAnsiChar; cdecl;
  TMsSQLdbisavail = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): LongBool; cdecl;
  TMsSQLdbfreelogin = procedure(Login: PLOGINREC); cdecl;
  TMsSQLdbprocinfo = function(Proc: PDBPROCESS; DbProcInfo: PDBPROCINFO): RETCODE; cdecl;
  TMsSQLdbrpcexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  TMsSQLdbserverenum = function(SearchMode: Word; ServNameBuf: PAnsiChar;
    ServNameBufSize: Word; NumEntries: PWord): Integer; cdecl;
  TMsSQLdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; cdecl;
  TMsSQLdbsetlpacket = function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl; //TDS: dbsetllong
  TMsSQLdbsetopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar):
    RETCODE; cdecl;
  TMsSQLdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; cdecl;

  TMsSQLdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  TMsSQLdbwillconvert = function(SrcType, DestType: Integer): LongBool; cdecl;
  TMsSQLdbupdatetext = function(Proc: PDBPROCESS; DestObject: PAnsiChar; DestTextPtr,
    DestTimestamp: PDBBINARY; UpdateType: Integer; InsertOffset,
    DeleteLength: DBINT; SrcObject: PAnsiChar; SrcSize: DBINT; SrcText: PDBBINARY):
    RETCODE; cdecl;

{************* Plain API Function variables definition ************}

{Sybase API definitions}
type
  SYBDBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  SYBDBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdb12hour = function(Proc: PDBPROCESS; Language: PAnsiChar): DBBOOL; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdberrhandle = function(Handler: SYBDBERRHANDLE_PROC): SYBDBERRHANDLE_PROC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmsghandle = function(Handler: SYBDBMSGHANDLE_PROC): SYBDBMSGHANDLE_PROC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Two-phase commit functions }
  TSybabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbuild_xact_string = procedure(XActName, Service: PAnsiChar; CommId: DBINT;
      Result: PAnsiChar); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybclose_commit = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybopen_commit = function(Login: PLOGINREC; ServerName: PAnsiChar): PDBPROCESS; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PAnsiChar;
    SiteCount: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ BCP functions }
  TSybbcp_batch = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_done = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
    Direction: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_readfmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_writefmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ Standard DB-Library functions }
  TSybdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcancel = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcanquery = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbchange = function(Proc: PDBPROCESS): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclose = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcmd = function(Proc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcmdrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolname = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  TSybdbcoltypeinfo = function(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcount = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcurcmd = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcurrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdbcursor = function(hCursor: PDBCURSOR; OpType, Row: Integer; Table,
    Values: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PAnsiChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdead = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbexit = procedure; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfcmd = function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfirstrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfreebuf = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbloginfree = procedure(Login: PLOGINREC); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfreequal = procedure(Ptr: PAnsiChar); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetchar = function(Proc: PDBPROCESS; N: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetmaxprocs = function: SmallInt; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT;
    StartFrom: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetpacket = function(Proc: PDBPROCESS): Cardinal; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetuserdata = function(Proc: PDBPROCESS): Pointer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbhasretstat = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbinit = function: RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbisavail = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdblastrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdblogin = function: PLOGINREC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmorecmds = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbname = function(Proc: PDBPROCESS): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnextrow = function(Proc: PDBPROCESS): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumcols = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumcompute = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumorders = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumrets = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbopen = function(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprhead = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprtype = function(Token: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PAnsiChar): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbreadtext = function(Proc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbresults = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretdata = function(Proc: PDBPROCESS; RetNum: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretlen = function(Proc: PDBPROCESS; RetNum: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretstatus = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrows = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  TSybdbrowtype = function(Proc: PDBPROCESS): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrpcinit = function(Proc: PDBPROCESS; ProcName: PAnsiChar; Options: DBSMALLINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  TSybdbrpcparam = function(Proc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrpcsend = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdbrpwclr = procedure(Login: PLOGINREC); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybdbsetavail = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybdbsetlname = function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};
  TSybdbsetlogintime = function(Seconds: Integer): RETCODE; {$IFNDEF UNIX}stdcall{$ELSE}cdecl{$ENDIF};

  TSybdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetopt = function(Proc: PDBPROCESS; Option: Integer; CharParam: PAnsiChar; IntParam: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsettime = function(Seconds: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlexec = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlok = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlsend = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PAnsiChar):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbstrlen = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabcount = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabname = function(Proc: PDBPROCESS; Table: Integer): PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PAnsiChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsnewlen = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsName,
    TabNum: Integer; TableName: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbuse = function(Proc: PDBPROCESS; DbName: PAnsiChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbwillconvert = function(SrcType, DestType: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbwritetext = function(Proc: PDBPROCESS; ObjName: PAnsiChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  TDBLibAPI = Record
    dberrhandle           : Tdberrhandle;
    dbmsghandle           : Tdbmsghandle;

    dbprocerrhandle       : Tdbprocerrhandle;
    dbprocmsghandle       : Tdbprocmsghandle;

    { Two-phase commit functions }
    abort_xact            : Tabort_xact;
    build_xact_string     : Tbuild_xact_string;
    close_commit          : Tclose_commit;
    commit_xact           : Tcommit_xact;
    open_commit           : Topen_commit;
    remove_xact           : Tremove_xact;
    scan_xact             : Tscan_xact;
    start_xact            : Tstart_xact;
    stat_xact             : Tstat_xact;

  { BCP functions }
    bcp_batch             : Tbcp_batch;
    bcp_bind              : Tbcp_bind;
    bcp_colfmt            : Tbcp_colfmt;
    bcp_collen            : Tbcp_collen;
    bcp_colptr            : Tbcp_colptr;
    bcp_columns           : Tbcp_columns;
    bcp_control           : Tbcp_control;
    bcp_done              : Tbcp_done;
    bcp_exec              : Tbcp_exec;
    bcp_init              : Tbcp_init;
    bcp_moretext          : Tbcp_moretext;
    bcp_readfmt           : Tbcp_readfmt;
    bcp_sendrow           : Tbcp_sendrow;
    bcp_setl              : Tbcp_setl;
    bcp_writefmt          : Tbcp_writefmt;
    dbadata               : Tdbadata;
    dbadlen               : Tdbadlen;
    dbaltbind             : Tdbaltbind;
    dbaltcolid            : Tdbaltcolid;
    dbaltlen              : Tdbaltlen;
    dbaltop               : Tdbaltop;
    dbalttype             : Tdbalttype;
    dbaltutype            : Tdbaltutype;
    dbanullbind           : Tdbanullbind;
    dbbind                : Tdbbind;
    dbbylist              : Tdbbylist;
    dbcancel              : Tdbcancel;
    dbcanquery            : Tdbcanquery;
    dbchange              : Tdbchange;
    dbclrbuf              : Tdbclrbuf;
    dbclropt              : Tdbclropt;
    dbcmd                 : Tdbcmd;
    dbcmdrow              : Tdbcmdrow;
    dbcollen              : Tdbcollen;
    dbcolname             : Tdbcolname;
    dbcolsource           : Tdbcolsource;
    dbcoltype             : Tdbcoltype;
    dbcolutype            : Tdbcolutype;
    dbconvert             : Tdbconvert;
    dbcurcmd              : Tdbcurcmd;
    dbcurrow              : Tdbcurrow;
    dbcursor              : Tdbcursor;
    dbdata                : Tdbdata;
    dbexit                : Tdbexit;
    dbfcmd                : Tdbfcmd;
    dbfirstrow            : Tdbfirstrow;
    dbfreebuf             : Tdbfreebuf;
    dbfreequal            : Tdbfreequal;
    dbgetchar             : Tdbgetchar;
    dbgetoff              : Tdbgetoff;
    dbgetrow              : Tdbgetrow;
    dbgettime             : Tdbgettime;
    dbiscount             : Tdbiscount;
    dblastrow             : Tdblastrow;
    dblogin               : Tdblogin;
    dbmorecmds            : Tdbmorecmds;
    dbmoretext            : Tdbmoretext;
    dbname                : Tdbname;
    dbnextrow             : Tdbnextrow;
    dbnullbind            : Tdbnullbind;
    dbnumalts             : Tdbnumalts;
    dbnumcols             : Tdbnumcols;
    dbnumcompute          : Tdbnumcompute;
    dbnumorders           : Tdbnumorders;
    dbnumrets             : Tdbnumrets;
    dbopen                : Tdbopen;
    dbordercol            : Tdbordercol;
    dbprhead              : Tdbprhead;
    dbprrow               : Tdbprrow;
    dbprtype              : Tdbprtype;
    dbqual                : Tdbqual;
    dbreadtext            : Tdbreadtext;
    dbresults             : Tdbresults;
    dbretdata             : Tdbretdata;
    dbretlen              : Tdbretlen;
    dbretname             : Tdbretname;
    dbretstatus           : Tdbretstatus;
    dbrettype             : Tdbrettype;
    dbrows                : Tdbrows;
    dbrowtype             : Tdbrowtype;
    dbrpcinit             : Tdbrpcinit;
    dbrpcparam            : Tdbrpcparam;
    dbrpcsend             : Tdbrpcsend;
    dbrpwclr              : Tdbrpwclr;
    dbsetavail            : Tdbsetavail;
    dbsetlname            : Tdbsetlname;
    dbsetlogintime        : Tdbsetlogintime;
    dbsetnull             : Tdbsetnull;
    dbsettime             : Tdbsettime;
    dbsetuserdata         : Tdbsetuserdata;
    dbsqlexec             : Tdbsqlexec;
    dbsqlok               : Tdbsqlok;
    dbsqlsend             : Tdbsqlsend;
    dbstrcpy              : Tdbstrcpy;
    dbstrlen              : Tdbstrlen;
    dbtabcount            : Tdbtabcount;
    dbtabname             : Tdbtabname;
    dbtabsource           : Tdbtabsource;
    dbtsnewlen            : Tdbtsnewlen;
    dbtsnewval            : Tdbtsnewval;
    dbtsput               : Tdbtsput;
    dbtxptr               : Tdbtxptr;
    dbtxtimestamp         : Tdbtxtimestamp;
    dbtxtsnewval          : Tdbtxtsnewval;
    dbtxtsput             : Tdbtxtsput;
    dbuse                 : Tdbuse;
    dbwritetext           : Tdbwritetext;
  End;

  TFreeTDSAPI = Record
  {available but not implemented}
    db12hour:       TFreeTDSdb12hour; {no MS}
    dbcolbrowse:    TFreeTDSdbcolbrowse;
    dbcursorbind:   TFreeTDSdbcursorbind;
    dbcursorclose:  TFreeTDSdbcursorclose;
    dbcursorcolinfo:TFreeTDSdbcursorcolinfo;
    dbcursorfetch:  TFreeTDSdbcursorfetch;
    dbcursorinfo:   TFreeTDSdbcursorinfo;


    dbaltbind_ps:   TFreeTDSdbaltbind_ps;
    dbbind_ps:      TFreeTDSdbbind_ps;
    dbbufsize:      TFreeTDSdbbufsize;
    dbclose:        TFreeTDSdbclose;
    dbtablecolinfo: TFreeTDSdbtablecolinfo;
    dbcolinfo:      TFreeTDSdbcolinfo;
    dbconvert_ps:   TFreeTDSdbconvert_ps;
    dbcount:        TFreeTDSdbcount;
    dbdatecmp:      TFreeTDSdbdatecmp;
    dbdatecrack:    TFreeTDSdbdatecrack;
    dbdatlen:       TFreeTDSdbdatlen;
    dbdead:         TFreeTDSdbdead;

    dbgetcharset:   TFreeTDSdbgetcharset;
    dbgetlusername: TFreeTDSdbgetlusername;
    dbgetmaxprocs:  TMsSQLdbgetmaxprocs;
    dbgetnatlanf:   TFreeTDSdbgetnatlanf;
    dbgetpacket:    TFreeTDSdbgetpacket;
    dbgetuserdata:  TFreeTDSdbgetuserdata;
    dbhasretstat:   TFreeTDSdbhasretstat;
    dbinit:         TFreeTDSdbinit;
    dbiordesc:      TFreeTDSdbiordesc;
    dbiowdesc:      TFreeTDSdbiowdesc;
    dbisavail:      TFreeTDSdbisavail;
    dbisopt:        TFreeTDSdbisopt;
    dbloginfree:    TFreeTDSdbloginfree;
    dbmny4cmp:      TFreeTDSdbmny4cmp;
    dbmnycmp:       TFreeTDSdbmnycmp;
    dbmny4add:      TFreeTDSdbmny4add;
    dbmnydec:       TFreeTDSdbmnydec;
    dbmnyinc:       TFreeTDSdbmnyinc;
    dbmnymaxpos:    TFreeTDSdbmnymaxpos;
    dbmnymaxneg:    TFreeTDSdbmnymaxneg;
    dbmny4minus:    TFreeTDSdbmny4minus;
    dbmnyminus:     TFreeTDSdbmnyminus;
    dbmny4sub:      TFreeTDSdbmny4sub;
    dbmnysub:       TFreeTDSdbmnysub;
    dbmny4copy:     TFreeTDSdbmny4copy;
    dbmnycopy:      TFreeTDSdbmnycopy;
    dbmny4zero:     TFreeTDSdbmny4zero;
    dbmnyzero:      TFreeTDSdbmnyzero;
    dbmonthname:    TFreeTDSdbmonthname;
    tdsdbopen:      TFreeTDSdbopen;

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
    DRBUF:          TDRBUF;
    dbrecftos:      TFreeTDSdbrecftos;
    dbresults_r:    TFreeTDSdbresults_r;
    dbsafestr:      TFreeTDSdbsafestr;
    //dbsechandle:    TFreeTDSdbsechandle;
    dbservcharset:  TFreeTDSdbservcharset;
    //dbsetbusy:      TFreeTDSdbsetbusy;
    dbsetdefcharset:TFreeTDSdbsetdefcharset;
    dbsetifile:     TFreeTDSdbsetifile;
    //dbsetinterrupt: TFreeTDSdbsetinterrupt;
    dbsetmaxprocs:  TMsSQLdbsetmaxprocs;
    dbsetopt:       TFreeTDSdbsetopt;
    dbsetrow:       TFreeTDSdbsetrow;
    dbsetversion:   TFreeTDSdbsetversion;
    dbspid:         TFreeTDSdbspid;
    dbspr1row:      TFreeTDSdbspr1row;
    dbspr1rowlen:   TFreeTDSdbspr1rowlen;
    dbsprhead:      TFreeTDSdbsprhead;
    dbsprline:      TFreeTDSdbsprline;
    dbvarylen:      TFreeTDSdbvarylen;

    dbtds:          TFreeTDSdbtds;
    dbtextsize:     TFreeTDSdbtextsize;
    dbwillconvert:  TFreeTDSdbwillconvert;
    dbsetlbool:     TFreeTDSdbsetlbool;
    dbsetllong:     TFreeTDSdbsetllong;
    dbsetlversion:  TFreeTDSdbsetlversion;

    tdsdump_on            : Ttdsdump_on;
    tdsdump_off           : Ttdsdump_off;
    tdsdump_open          : Ttdsdump_open;
    tdsdump_close         : Ttdsdump_close;
    _tds_socket_init      : T_tds_socket_init;
    _tds_socket_done      : T_tds_socket_done;
  End;

  TMsSQLAPI = record

  { Standard DB-Library functions }
    dbclose               : TMsSQLdbclose;
    dbcolbrowse           : TMsSQLdbcolbrowse;
    dbcolinfo             : TMsSQLdbcolinfo;
    dbcount               : TMsSQLdbcount;

    dbcursorbind          : TMsSQLdbcursorbind;
    dbcursorclose         : TMsSQLdbcursorclose;
    dbcursorcolinfo       : TMsSQLdbcursorcolinfo;
    dbcursorfetch         : TMsSQLdbcursorfetch;
    dbcursorfetchex       : TMsSQLdbcursorfetchex;
    dbcursorinfo          : TMsSQLdbcursorinfo;
    dbcursorinfoex        : TMsSQLdbcursorinfoex;
    dbcursoropen          : TMsSQLdbcursoropen;
    dbdataready           : TMsSQLdbdataready;
    dbdatecrack           : TMsSQLdbdatecrack;
    dbdatlen              : TMsSQLdbdatlen;
    dbdead                : TMsSQLdbdead;
    dbWinexit             : TMsSQLdbWinexit;
    dbenlisttrans         : TMsSQLdbenlisttrans;
    dbenlistxatrans       : TMsSQLdbenlistxatrans;
    dbfreelogin           : TMsSQLdbfreelogin;
    dbgetmaxprocs         : TMsSQLdbgetmaxprocs;
    dbgetpacket           : TMsSQLdbgetpacket;
    dbgetuserdata         : TMsSQLdbgetuserdata;
    dbhasretstat          : TMsSQLdbhasretstat;
    dbinit                : TMsSQLdbinit;
    dbisavail             : TMsSQLdbisavail;
    dbisopt               : TMsSQLdbisopt;
    dbprocinfo            : TMsSQLdbprocinfo;
    dbrpcexec             : TMsSQLdbrpcexec;
    dbserverenum          : TMsSQLdbserverenum;
    dbsetmaxprocs         : TMsSQLdbsetmaxprocs;
    dbsetlpacket          : TMsSQLdbsetlpacket;
    dbsetopt              : TMsSQLdbsetopt;
    dbtabbrowse           : TMsSQLdbtabbrowse;
    dbvarylen             : TMsSQLdbvarylen;
    dbwillconvert         : TMsSQLdbwillconvert;
    dbupdatetext          : TMsSQLdbupdatetext;
  end;

  TSybaseAPI = record
    db12hour              : TSybdb12hour;

    dberrhandle           : TSybdberrhandle;
    dbmsghandle           : TSybdbmsghandle;

    { Two-phase commit functions }
    abort_xact            : TSybabort_xact;
    build_xact_string     : TSybbuild_xact_string;
    close_commit          : TSybclose_commit;
    commit_xact           : TSybcommit_xact;
    open_commit           : TSybopen_commit;
    remove_xact           : TSybremove_xact;
    scan_xact             : TSybscan_xact;
    start_xact            : TSybstart_xact;
    stat_xact             : TSybstat_xact;

  { BCP functions }
    bcp_batch             : TSybbcp_batch;
    bcp_bind              : TSybbcp_bind;
    bcp_colfmt            : TSybbcp_colfmt;
    bcp_collen            : TSybbcp_collen;
    bcp_colptr            : TSybbcp_colptr;
    bcp_columns           : TSybbcp_columns;
    bcp_control           : TSybbcp_control;
    bcp_done              : TSybbcp_done;
    bcp_exec              : TSybbcp_exec;
    bcp_init              : TSybbcp_init;
    bcp_moretext          : TSybbcp_moretext;
    bcp_readfmt           : TSybbcp_readfmt;
    bcp_sendrow           : TSybbcp_sendrow;
    bcp_writefmt          : TSybbcp_writefmt;

  { Standard DB-Library functions }
    dbadata               : TSybdbadata;
    dbadlen               : TSybdbadlen;
    dbaltbind             : TSybdbaltbind;
    dbaltcolid            : TSybdbaltcolid;
    dbaltlen              : TSybdbaltlen;
    dbaltop               : TSybdbaltop;
    dbalttype             : TSybdbalttype;
    dbaltutype            : TSybdbaltutype;
    dbanullbind           : TSybdbanullbind;
    dbbind                : TSybdbbind;
    dbbylist              : TSybdbbylist;
    dbcancel              : TSybdbcancel;
    dbcanquery            : TSybdbcanquery;
    dbchange              : TSybdbchange;
    dbclose               : TSybdbclose;
    dbclrbuf              : TSybdbclrbuf;
    dbclropt              : TSybdbclropt;
    dbcmd                 : TSybdbcmd;
    dbcmdrow              : TSybdbcmdrow;
    dbcolbrowse           : TSybdbcolbrowse;
    dbcollen              : TSybdbcollen;
    dbcolname             : TSybdbcolname;
    dbcolsource           : TSybdbcolsource;
  //  dbcoltypeinfo         : TSybdbcoltypeinfo;
    dbcoltype             : TSybdbcoltype;
    dbcolutype            : TSybdbcolutype;
    dbconvert             : TSybdbconvert;
    dbcount               : TSybdbcount;
    dbcurcmd              : TSybdbcurcmd;
    dbcurrow              : TSybdbcurrow;

    dbcursor              : TSybdbcursor;
    dbcursorbind          : TSybdbcursorbind;
    dbcursorclose         : TSybdbcursorclose;
    dbcursorcolinfo       : TSybdbcursorcolinfo;
    dbcursorfetch         : TSybdbcursorfetch;
    dbcursorinfo          : TSybdbcursorinfo;
    dbcursoropen          : TSybdbcursoropen;
    dbdata                : TSybdbdata;
    dbdatecrack           : TSybdbdatecrack;
    dbdatlen              : TSybdbdatlen;
    dbdead                : TSybdbdead;
    dbexit                : TSybdbexit;
    dbfcmd                : TSybdbfcmd;
    dbfirstrow            : TSybdbfirstrow;
    dbfreebuf             : TSybdbfreebuf;
    dbloginfree           : TSybdbloginfree;
    dbfreequal            : TSybdbfreequal;
    dbgetchar             : TSybdbgetchar;
    dbgetmaxprocs         : TSybdbgetmaxprocs;
    dbgetoff              : TSybdbgetoff;
    dbgetpacket           : TSybdbgetpacket;
    dbgetrow              : TSybdbgetrow;
    dbgetuserdata         : TSybdbgetuserdata;
    dbhasretstat          : TSybdbhasretstat;
    dbinit                : TSybdbinit;
    dbisavail             : TSybdbisavail;
    dbisopt               : TSybdbisopt;
    dblastrow             : TSybdblastrow;
    dblogin               : TSybdblogin;
    dbmorecmds            : TSybdbmorecmds;
    dbmoretext            : TSybdbmoretext;
    dbname                : TSybdbname;
    dbnextrow             : TSybdbnextrow;
    dbnullbind            : TSybdbnullbind;
    dbnumalts             : TSybdbnumalts;
    dbnumcols             : TSybdbnumcols;
    dbnumcompute          : TSybdbnumcompute;
    dbnumorders           : TSybdbnumorders;
    dbnumrets             : TSybdbnumrets;
    dbopen                : TSybdbopen;
    dbordercol            : TSybdbordercol;
    dbprhead              : TSybdbprhead;
    dbprrow               : TSybdbprrow;
    dbprtype              : TSybdbprtype;
    dbqual                : TSybdbqual;
    dbreadtext            : TSybdbreadtext;
    dbresults             : TSybdbresults;
    dbretdata             : TSybdbretdata;
    dbretlen              : TSybdbretlen;
    dbretname             : TSybdbretname;
    dbretstatus           : TSybdbretstatus;
    dbrettype             : TSybdbrettype;
    dbrows                : TSybdbrows;
    dbrowtype             : TSybdbrowtype;
    dbrpcinit             : TSybdbrpcinit;
    dbrpcparam            : TSybdbrpcparam;
    dbrpcsend             : TSybdbrpcsend;

    dbrpwclr              : TSybdbrpwclr;
    dbsetavail            : TSybdbsetavail;
    dbsetmaxprocs         : TSybdbsetmaxprocs;
    dbsetlname            : TSybdbsetlname;
    dbsetlogintime        : TSybdbsetlogintime;

    dbsetnull             : TSybdbsetnull;
    dbsetopt              : TSybdbsetopt;
    dbsettime             : TSybdbsettime;
    dbsetuserdata         : TSybdbsetuserdata;
    dbsqlexec             : TSybdbsqlexec;
    dbsqlok               : TSybdbsqlok;
    dbsqlsend             : TSybdbsqlsend;
    dbstrcpy              : TSybdbstrcpy;
    dbstrlen              : TSybdbstrlen;
    dbtabbrowse           : TSybdbtabbrowse;
    dbtabcount            : TSybdbtabcount;
    dbtabname             : TSybdbtabname;
    dbtabsource           : TSybdbtabsource;
    dbtsnewlen            : TSybdbtsnewlen;
    dbtsnewval            : TSybdbtsnewval;
    dbtsput               : TSybdbtsput;
    dbtxptr               : TSybdbtxptr;
    dbtxtimestamp         : TSybdbtxtimestamp;
    dbtxtsnewval          : TSybdbtxtsnewval;
    dbtxtsput             : TSybdbtxtsput;
    dbuse                 : TSybdbuse;
    dbvarylen             : TSybdbvarylen;
    dbwillconvert         : TSybdbwillconvert;
    dbwritetext           : TSybdbwritetext;
  end;

type
  {** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface (IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']

    procedure CheckError(dbProc: PDBPROCESS);

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS; Async: Boolean=False): RETCODE;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT;
      Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): DBINT;
    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
      SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): DBINT;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool;
    function GetVariables: TDBVariables;
  end;

  TZDBLibAbstractPlainDriver = class(TZAbstractPlainDriver, IZPlainDriver)
  protected
    DBVariables: TDBVariables;
  public
    constructor Create; virtual;
    procedure CheckError(dbProc: PDBPROCESS);
    function GetVariables: TDBVariables;
  end;

  TZDbLibBasePlainDriver = class (TZDBLibAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  protected
    DBLibAPI: TDBLibAPI;
  public
    procedure LoadApi; override;

    function dbDead(dbProc: PDBPROCESS): Boolean; virtual; abstract;
    function dbLogin: PLOGINREC; virtual;
    procedure dbLoginFree(Login: PLOGINREC); virtual; abstract;
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE; virtual; abstract;
    function dbSetLSecure(Login: PLOGINREC): RETCODE; virtual; abstract;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE; virtual; abstract;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; virtual;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS; Async: Boolean=False): RETCODE;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE; virtual; abstract;
    function dbClose(dbProc: PDBPROCESS): RETCODE; virtual; abstract;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): DBINT;
    function dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool; virtual; abstract;

    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT; virtual; abstract;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
             SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT; virtual; abstract;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; virtual;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): DBINT;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean; virtual; abstract;
    function dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool; virtual; abstract;
    function dbrbuf(Proc: PDBPROCESS): DBINT;
  end;

  {** Implements a dblib driver for Sybase ASE 12.5 }
  TZDBLibSybaseASE125PlainDriver = class (TZDBLibAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  private
    SybaseAPI: TSybaseAPI;
  protected
    procedure LoadApi; override;
    function Clone: IZPlainDriver; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS; Async: Boolean=False): RETCODE; virtual;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): DBINT;
    function dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool;

    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
             SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): DBINT;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbrbuf(Proc: PDBPROCESS): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool;
  end;

  {** Implements a dblib driver for MSSql7 }
  TZDBLibMSSQL7PlainDriver = class (TZDbLibBasePlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  private
    MsSQLAPI: TMsSQLAPI;
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadApi; override;
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    function dbDead(dbProc: PDBPROCESS): Boolean; override;
    procedure dbLoginFree(Login: PLOGINREC); override;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE; override;
    function dbSetLSecure(Login: PLOGINREC): RETCODE; override;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE; override;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE; override;
    function dbClose(dbProc: PDBPROCESS): RETCODE; override;
    function dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool; override;

    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT; override;
    function dbCount(dbProc: PDBPROCESS): DBINT; override;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean; override;
    function dbdataready(Proc: PDBPROCESS): LongBool; override;
  end;

  {** Implements a generic dblib driver}
  IZFreeTDSPlainDriver = interface (IZDBLibPlainDriver)
    ['{12FA5A22-59E5-4CBF-B745-96A7CDF9FBE0}']

    function dbSetTime(queryTime : Integer): RETCODE;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;
    procedure tdsSocketInit;
    procedure tdsSocketDone;

  end;

  {** Implements a dblib driver for Sybase/MSSQL }
  TZFreeTDSBasePlainDriver = class (TZDbLibBasePlainDriver, IZFreeTDSPlainDriver)
  private
    FreeTDSAPI: TFreeTDSAPI;
  protected
    function Clone: IZPlainDriver; override; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadApi; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    {API functions}
    function dbsetlversion(Login: PLOGINREC): RETCODE; virtual;
    function dbsetversion: RETCODE; virtual;


    function dbDead(dbProc: PDBPROCESS): Boolean; override;
    function dbLogin: PLOGINREC; override;
    procedure dbLoginFree(Login: PLOGINREC); override;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE; override;
    function dbSetLSecure(Login: PLOGINREC): RETCODE; override;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE; override;
    function dbSetTime(queryTime : Integer): RETCODE;

    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer;
      Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE; override;
    function dbClose(dbProc: PDBPROCESS): RETCODE; override;
    function dbColInfo(dbProc: PDBPROCESS; Column: Integer; var ADBInfo: DBCOL): RETCODE;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer; override;
    function dbCount(dbProc: PDBPROCESS): Integer; override;
    function dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool; override;

    function dbHasRetStat(dbProc: PDBPROCESS): Boolean; override;

    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;
    procedure tdsSocketInit;
    procedure tdsSocketDone;
    function dbdataready(Proc: PDBPROCESS): LongBool; override;
    procedure dbfreelogin(Login: PLOGINREC);
  end;


  TZFreeTDS42MsSQLPlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS42SybasePlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS50PlainDriver = class(TZFreeTDS42SybasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS70PlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS71PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS72PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;


var
  OldFreeTDSErrorHandle: DBERRHANDLE_PROC = nil;
  OldFreeTDSMessageHandle: DBMSGHANDLE_PROC = nil;
  OldSybaseErrorHandle: SYBDBERRHANDLE_PROC = nil;
  OldSybaseMessageHandle: SYBDBMSGHANDLE_PROC = nil;
  OldMsSQLMessageHandle: DBMSGHANDLE_PROC = nil;
  OldMsSQLErrorHandle: DBERRHANDLE_PROC = nil;
  SQLErrors: TList;
  SQLMessages: TList;

implementation

uses SysUtils, ZPlainLoader, Windows;

{ Handle sql server error messages }
function SybaseErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer;
{$IFNDEF UNIX} stdcall{$ELSE} cdecl{$ENDIF};
var
  SqlError: PDBLibError;
begin
  New(SqlError);
  SqlError.dbProc := Proc;
  SqlError.Severity := Severity;
  SqlError.DbErr := DbErr;
  SqlError.OsErr := OsErr;
  SqlError.DbErrStr := DbErrStr;
  SqlError.OsErrStr := OsErrStr;
  SQLErrors.Add(SqlError);

  Result := INT_CANCEL;
end;

{ Handle sql server messages }
function SybaseMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
var
  SQLMessage: PDBLibMessage;
begin
  New(SQLMessage);
  SQLMessage.dbProc := Proc;
  SQLMessage.MsgNo := MsgNo;
  SQLMessage.MsgState := MsgState;
  SQLMessage.Severity := Severity;
  SQLMessage.MsgText := MsgText;
  SQLMessage.SrvName := SrvName;
  SQLMessage.ProcName := ProcName;
  SQLMessage.Line := Line;
  SQLMessages.Add(SQLMessage);

  Result := 0;
end;

{ Handle sql server error messages }
function DbLibErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  New(SqlError);
  SqlError.dbProc := Proc;
  SqlError.Severity := Severity;
  SqlError.DbErr := DbErr;
  SqlError.OsErr := OsErr;
  SqlError.DbErrStr := DbErrStr;
  SqlError.OsErrStr := OsErrStr;
  SQLErrors.Add(SqlError);

  Result := INT_CANCEL;
end;

{ Handle sql server messages }
function DbLibMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  New(SQLMessage);
  SQLMessage.dbProc := Proc;
  SQLMessage.MsgNo := MsgNo;
  SQLMessage.MsgState := MsgState;
  SQLMessage.Severity := Severity;
  SQLMessage.MsgText := MsgText;
  SQLMessage.SrvName := SrvName;
  SQLMessage.ProcName := ProcName;
  SQLMessage.Line := Line;
  SQLMessages.Add(SQLMessage);

  Result := 0;
end;

constructor TZDBLibAbstractPlainDriver.Create;
var I: Integer;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  for i := 0 to high(DBVariables.DBoptions) do DBVariables.DBoptions[i] := -1;
  for i := 0 to high(DBVariables.DBSetLoginRec) do DBVariables.DBSetLoginRec[i] := -1;
end;

procedure TZDBLibAbstractPlainDriver.CheckError(dbProc: Pointer);
var
  I: Integer;
  S: String;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;

    procedure AddToErrorMsg(const AError: String);
    begin
      if S > '' then
        S := S + #13#10;
      S := S + AError;
    end;

begin
  if ((SQLErrors = nil) or (SQLErrors.Count = 0)) and
     ((SQLMessages = nil) or (SQLMessages.Count = 0)) then
    Exit;
  S := '';
  I := 0;
  while I < SQLErrors.Count do begin
    lErrorEntry := PDBLibError(SQLErrors[I]);
    if (dbProc = nil) or (lErrorEntry^.dbProc = dbProc) or (lErrorEntry^.dbProc = nil) then begin
        if lErrorEntry^.Severity > EXINFO then
          AddToErrorMsg(Format('DBError : [%4.4d] : %s', [lErrorEntry^.DbErr, String(lErrorEntry^.DbErrStr)]) );
        if lErrorEntry^.OsErr > EXINFO then
          AddToErrorMsg(Format('OSError : [%4.4d] : %s', [lErrorEntry^.OsErr, String(lErrorEntry^.OsErrStr)]) );
        Dispose(lErrorEntry);
        SQLErrors.Delete(I);
    end
    else
      Inc(I);
  end;
  I := 0;
  while I < SQLMessages.Count do begin
    lMesageEntry := PDBLibMessage(SQLMessages[I]);
    if (dbProc = nil) or (lMesageEntry^.dbProc = dbProc) or (lMesageEntry^.dbProc = nil) then begin
      if lMesageEntry^.Severity > EXINFO then
        AddToErrorMsg(String(lMesageEntry^.MsgText));
      Dispose(lMesageEntry);
      SQLMessages.Delete(I);
    end
    else
      Inc(I);
  end;
  if S <> '' then
    raise Exception.Create(String(S));
end;

function TZDBLibAbstractPlainDriver.GetVariables: TDBVariables;
begin
  Result := DBVariables;
end;

{ TZDBLibBasePlainDriver }

procedure TZDBLibBasePlainDriver.LoadApi;
begin
  inherited LoadAPI;
  with Loader do
  begin
    @DBLibAPI.dberrhandle           := GetAddress('dberrhandle');
    @DBLibAPI.dbmsghandle           := GetAddress('dbmsghandle');
    @DBLibAPI.dbprocerrhandle       := GetAddress('dbprocerrhandle');
    @DBLibAPI.dbprocmsghandle       := GetAddress('dbprocmsghandle');
    @DBLibAPI.abort_xact            := GetAddress('abort_xact');
    @DBLibAPI.build_xact_string     := GetAddress('build_xact_string');
    @DBLibAPI.close_commit          := GetAddress('close_commit');
    @DBLibAPI.commit_xact           := GetAddress('commit_xact');
    @DBLibAPI.open_commit           := GetAddress('open_commit');
    @DBLibAPI.remove_xact           := GetAddress('remove_xact');
    @DBLibAPI.scan_xact             := GetAddress('scan_xact');
    @DBLibAPI.start_xact            := GetAddress('start_xact');
    @DBLibAPI.stat_xact             := GetAddress('stat_xact');
    @DBLibAPI.bcp_batch             := GetAddress('bcp_batch');
    @DBLibAPI.bcp_bind              := GetAddress('bcp_bind');
    @DBLibAPI.bcp_colfmt            := GetAddress('bcp_colfmt');
    @DBLibAPI.bcp_collen            := GetAddress('bcp_collen');
    @DBLibAPI.bcp_colptr            := GetAddress('bcp_colptr');
    @DBLibAPI.bcp_columns           := GetAddress('bcp_columns');
    @DBLibAPI.bcp_control           := GetAddress('bcp_control');
    @DBLibAPI.bcp_done              := GetAddress('bcp_done');
    @DBLibAPI.bcp_exec              := GetAddress('bcp_exec');
    @DBLibAPI.bcp_init              := GetAddress('bcp_init');
    @DBLibAPI.bcp_moretext          := GetAddress('bcp_moretext');
    @DBLibAPI.bcp_readfmt           := GetAddress('bcp_readfmt');
    @DBLibAPI.bcp_sendrow           := GetAddress('bcp_sendrow');
    @DBLibAPI.bcp_setl              := GetAddress('bcp_setl');
    @DBLibAPI.bcp_writefmt          := GetAddress('bcp_writefmt');
    @DBLibAPI.dbadata               := GetAddress('dbadata');
    @DBLibAPI.dbadlen               := GetAddress('dbadlen');
    @DBLibAPI.dbaltbind             := GetAddress('dbaltbind');
    @DBLibAPI.dbaltcolid            := GetAddress('dbaltcolid');
    @DBLibAPI.dbaltlen              := GetAddress('dbaltlen');
    @DBLibAPI.dbaltop               := GetAddress('dbaltop');
    @DBLibAPI.dbalttype             := GetAddress('dbalttype');
    @DBLibAPI.dbaltutype            := GetAddress('dbaltutype');
    @DBLibAPI.dbanullbind           := GetAddress('dbanullbind');
    @DBLibAPI.dbbind                := GetAddress('dbbind');
    @DBLibAPI.dbbylist              := GetAddress('dbbylist');
    @DBLibAPI.dbcancel              := GetAddress('dbcancel');
    @DBLibAPI.dbcanquery            := GetAddress('dbcanquery');
    @DBLibAPI.dbchange              := GetAddress('dbchange');
    @DBLibAPI.dbclrbuf              := GetAddress('dbclrbuf');
    @DBLibAPI.dbclropt              := GetAddress('dbclropt');
    @DBLibAPI.dbcmd                 := GetAddress('dbcmd');
    @DBLibAPI.dbcmdrow              := GetAddress('dbcmdrow');
    @DBLibAPI.dbcollen              := GetAddress('dbcollen');
    @DBLibAPI.dbcolname             := GetAddress('dbcolname');
    @DBLibAPI.dbcolsource           := GetAddress('dbcolsource');
    @DBLibAPI.dbcoltype             := GetAddress('dbcoltype');
    @DBLibAPI.dbcolutype            := GetAddress('dbcolutype');
    @DBLibAPI.dbconvert             := GetAddress('dbconvert');
    @DBLibAPI.dbcurcmd              := GetAddress('dbcurcmd');
    @DBLibAPI.dbcurrow              := GetAddress('dbcurrow');
    @DBLibAPI.dbcursor              := GetAddress('dbcursor');
    @DBLibAPI.dbdata                := GetAddress('dbdata');
    @DBLibAPI.dbexit                := GetAddress('dbexit');
    @DBLibAPI.dbfcmd                := GetAddress('dbfcmd');
    @DBLibAPI.dbfirstrow            := GetAddress('dbfirstrow');
    @DBLibAPI.dbfreebuf             := GetAddress('dbfreebuf');
    @DBLibAPI.dbfreequal            := GetAddress('dbfreequal');
    @DBLibAPI.dbgetchar             := GetAddress('dbgetchar');
    @DBLibAPI.dbgetoff              := GetAddress('dbgetoff');
    @DBLibAPI.dbgetrow              := GetAddress('dbgetrow');
    @DBLibAPI.dbgettime             := GetAddress('dbgettime');
    @DBLibAPI.dbiscount             := GetAddress('dbiscount');
    @DBLibAPI.dblastrow             := GetAddress('dblastrow');
    @DBLibAPI.dblogin               := GetAddress('dblogin');
    @DBLibAPI.dbmorecmds            := GetAddress('dbmorecmds');
    @DBLibAPI.dbmoretext            := GetAddress('dbmoretext');
    @DBLibAPI.dbname                := GetAddress('dbname');
    @DBLibAPI.dbnextrow             := GetAddress('dbnextrow');
    @DBLibAPI.dbnullbind            := GetAddress('dbnullbind');
    @DBLibAPI.dbnumalts             := GetAddress('dbnumalts');
    @DBLibAPI.dbnumcols             := GetAddress('dbnumcols');
    @DBLibAPI.dbnumcompute          := GetAddress('dbnumcompute');
    @DBLibAPI.dbnumorders           := GetAddress('dbnumorders');
    @DBLibAPI.dbnumrets             := GetAddress('dbnumrets');
    @DBLibAPI.dbopen                := GetAddress('dbopen');
    @DBLibAPI.dbordercol            := GetAddress('dbordercol');
    @DBLibAPI.dbprhead              := GetAddress('dbprhead');
    @DBLibAPI.dbprrow               := GetAddress('dbprrow');
    @DBLibAPI.dbprtype              := GetAddress('dbprtype');
    @DBLibAPI.dbqual                := GetAddress('dbqual');
    @DBLibAPI.dbreadtext            := GetAddress('dbreadtext');
    @DBLibAPI.dbresults             := GetAddress('dbresults');
    @DBLibAPI.dbretdata             := GetAddress('dbretdata');
    @DBLibAPI.dbretlen              := GetAddress('dbretlen');
    @DBLibAPI.dbretname             := GetAddress('dbretname');
    @DBLibAPI.dbretstatus           := GetAddress('dbretstatus');
    @DBLibAPI.dbrettype             := GetAddress('dbrettype');
    @DBLibAPI.dbrows                := GetAddress('dbrows');
    @DBLibAPI.dbrowtype             := GetAddress('dbrowtype');
    @DBLibAPI.dbrpcinit             := GetAddress('dbrpcinit');
    @DBLibAPI.dbrpcparam            := GetAddress('dbrpcparam');
    @DBLibAPI.dbrpcsend             := GetAddress('dbrpcsend');
    @DBLibAPI.dbrpwclr              := GetAddress('dbrpwclr');
    @DBLibAPI.dbsetavail            := GetAddress('dbsetavail');
    @DBLibAPI.dbsetlname            := GetAddress('dbsetlname');
    @DBLibAPI.dbsetlogintime        := GetAddress('dbsetlogintime');
    @DBLibAPI.dbsetnull             := GetAddress('dbsetnull');
    @DBLibAPI.dbsettime             := GetAddress('dbsettime');
    @DBLibAPI.dbsetuserdata         := GetAddress('dbsetuserdata');
    @DBLibAPI.dbsqlexec             := GetAddress('dbsqlexec');
    @DBLibAPI.dbsqlok               := GetAddress('dbsqlok');
    @DBLibAPI.dbsqlsend             := GetAddress('dbsqlsend');
    @DBLibAPI.dbstrcpy              := GetAddress('dbstrcpy');
    @DBLibAPI.dbstrlen              := GetAddress('dbstrlen');
    @DBLibAPI.dbtabcount            := GetAddress('dbtabcount');
    @DBLibAPI.dbtabname             := GetAddress('dbtabname');
    @DBLibAPI.dbtabsource           := GetAddress('dbtabsource');
    @DBLibAPI.dbtsnewlen            := GetAddress('dbtsnewlen');
    @DBLibAPI.dbtsnewval            := GetAddress('dbtsnewval');
    @DBLibAPI.dbtsput               := GetAddress('dbtsput');
    @DBLibAPI.dbtxptr               := GetAddress('dbtxptr');
    @DBLibAPI.dbtxtimestamp         := GetAddress('dbtxtimestamp');
    @DBLibAPI.dbtxtsnewval          := GetAddress('dbtxtsnewval');
    @DBLibAPI.dbtxtsput             := GetAddress('dbtxtsput');
    @DBLibAPI.dbuse                 := GetAddress('dbuse');
    @DBLibAPI.dbwritetext           := GetAddress('dbwritetext');
  end;
end;

function TZDBLibBasePlainDriver.dbLogin: PLOGINREC;
begin
  Result := DBLibAPI.dblogin;
end;

function TZDBLibBasePlainDriver.dbSetLoginTime(Seconds: DBINT): RETCODE;
begin
  Result := DBLibAPI.dbsetlogintime(Seconds);
end;

function TZDBLibBasePlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, Value, Item);
end;

function TZDBLibBasePlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, HostName, Self.DBVariables.dbSetLoginRec[Z_SETHOST]);
end;

function TZDBLibBasePlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, UserName, Self.DBVariables.dbSetLoginRec[Z_SETUSER]);
end;

function TZDBLibBasePlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, Password, Self.DBVariables.dbSetLoginRec[Z_SETPWD]);
end;

function TZDBLibBasePlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, AppName, Self.DBVariables.dbSetLoginRec[Z_SETAPP]);
end;

function TZDBLibBasePlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, NatLangName, Self.DBVariables.dbSetLoginRec[Z_SETLANG]);
end;

function TZDBLibBasePlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  DBLibAPI.dbsetlogintime(10);
  Result := DBLibAPI.dbOpen(Login, Host);
end;

function TZDBLibBasePlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbcancel(dbProc);
end;

function TZDBLibBasePlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbcmd(dbProc, Cmd);
end;

function TZDBLibBasePlainDriver.dbSqlExec(dbProc: PDBPROCESS; Async: Boolean=False): RETCODE;
begin
  if Async then
    Result := dbSqlExecAsync(dbProc)
  else
    Result := dbSqlExecSync(dbProc);
end;

function TZDBLibBasePlainDriver.dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbSqlExec(dbProc);
end;

function TZDBLibBasePlainDriver.dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
var
  lStartTick : Int64;
begin
  Result := DBLibAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartTick := GetTickCount;
    repeat
      //DBApplication.ProcessMessages;
    until (GetTickCount > lStartTick + TIMEOUT_MAXIMUM * 1000) or (dbdataready(dbProc) = TRUE);
    Result := DBLibAPI.dbsqlok(dbProc);
  end;
end;

function TZDBLibBasePlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbResults(dbProc);
end;

function TZDBLibBasePlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbCanQuery(dbProc);
end;

function TZDBLibBasePlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbMoreCmds(dbProc);
end;

function TZDBLibBasePlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbUse(dbProc, dbName);
end;

function TZDBLibBasePlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := DBLibAPI.dbName(dbProc);
end;

function TZDBLibBasePlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbCmdRow(dbProc);
end;

function TZDBLibBasePlainDriver.dbNumCols(dbProc: PDBPROCESS): Integer;
begin
  Result := DBLibAPI.dbNumCols(dbProc);
end;

function TZDBLibBasePlainDriver.dbColName(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
begin
  Result := DBLibAPI.dbColName(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := DBLibAPI.dbColType(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
begin
  Result := DBLibAPI.dbColLen(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
begin
  Result := DBLibAPI.dbData(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
begin
  Result := DBLibAPI.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibBasePlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := DBLibAPI.dbNextRow(dbProc);
end;

function TZDBLibBasePlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
begin
  Result := DBLibAPI.dbGetRow(dbProc, Row);
end;

function TZDBLibBasePlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := DBLibAPI.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibBasePlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
begin
  Result := DBLibAPI.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibBasePlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbRpcSend(dbProc);
end;

function TZDBLibBasePlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := DBLibAPI.dbSqlOk(dbProc);
end;

function TZDBLibBasePlainDriver.dbRetStatus(dbProc: PDBPROCESS): Integer;
begin
  Result := DBLibAPI.dbRetStatus(dbProc);
end;

function TZDBLibBasePlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar;
begin
  Result := DBLibAPI.dbRetName(dbProc, RetNum);
end;

function TZDBLibBasePlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
begin
  Result := DBLibAPI.dbRetData(dbProc, RetNum);
end;

function TZDBLibBasePlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := DBLibAPI.dbRetLen(dbProc, RetNum);
end;

function TZDBLibBasePlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := DBLibAPI.dbRetType(dbProc, RetNum);
end;

function TZDBLibBasePlainDriver.dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := DBINT(dbdataready(Proc));
end;


{ TZDBLibSybaseASE125PlainDriver }

procedure TZDBLibSybaseASE125PlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    @SybaseAPI.db12hour              := GetAddress('db12hour');
    @SybaseAPI.dberrhandle           := GetAddress('dberrhandle');
    @SybaseAPI.dbmsghandle           := GetAddress('dbmsghandle');
    @SybaseAPI.abort_xact            := GetAddress('abort_xact');
    @SybaseAPI.build_xact_string     := GetAddress('build_xact_string');
    @SybaseAPI.close_commit          := GetAddress('close_commit');
    @SybaseAPI.commit_xact           := GetAddress('commit_xact');
    @SybaseAPI.open_commit           := GetAddress('open_commit');
    @SybaseAPI.remove_xact           := GetAddress('remove_xact');
    @SybaseAPI.scan_xact             := GetAddress('scan_xact');
    @SybaseAPI.start_xact            := GetAddress('start_xact');
    @SybaseAPI.stat_xact             := GetAddress('stat_xact');
    @SybaseAPI.bcp_batch             := GetAddress('bcp_batch');
    @SybaseAPI.bcp_bind              := GetAddress('bcp_bind');
    @SybaseAPI.bcp_colfmt            := GetAddress('bcp_colfmt');
    @SybaseAPI.bcp_collen            := GetAddress('bcp_collen');
    @SybaseAPI.bcp_colptr            := GetAddress('bcp_colptr');
    @SybaseAPI.bcp_columns           := GetAddress('bcp_columns');
    @SybaseAPI.bcp_control           := GetAddress('bcp_control');
    @SybaseAPI.bcp_done              := GetAddress('bcp_done');
    @SybaseAPI.bcp_exec              := GetAddress('bcp_exec');
    @SybaseAPI.bcp_init              := GetAddress('bcp_init');
    @SybaseAPI.bcp_moretext          := GetAddress('bcp_moretext');
    @SybaseAPI.bcp_readfmt           := GetAddress('bcp_readfmt');
    @SybaseAPI.bcp_sendrow           := GetAddress('bcp_sendrow');
    @SybaseAPI.bcp_writefmt          := GetAddress('bcp_writefmt');
    @SybaseAPI.dbadata               := GetAddress('dbadata');
    @SybaseAPI.dbadlen               := GetAddress('dbadlen');
    @SybaseAPI.dbaltbind             := GetAddress('dbaltbind');
    @SybaseAPI.dbaltcolid            := GetAddress('dbaltcolid');
    @SybaseAPI.dbaltlen              := GetAddress('dbaltlen');
    @SybaseAPI.dbaltop               := GetAddress('dbaltop');
    @SybaseAPI.dbalttype             := GetAddress('dbalttype');
    @SybaseAPI.dbaltutype            := GetAddress('dbaltutype');
    @SybaseAPI.dbanullbind           := GetAddress('dbanullbind');
    @SybaseAPI.dbbind                := GetAddress('dbbind');
    @SybaseAPI.dbbylist              := GetAddress('dbbylist');
    @SybaseAPI.dbcancel              := GetAddress('dbcancel');
    @SybaseAPI.dbcanquery            := GetAddress('dbcanquery');
    @SybaseAPI.dbchange              := GetAddress('dbchange');
    @SybaseAPI.dbclose               := GetAddress('dbclose');
    @SybaseAPI.dbclrbuf              := GetAddress('dbclrbuf');
    @SybaseAPI.dbclropt              := GetAddress('dbclropt');
    @SybaseAPI.dbcmd                 := GetAddress('dbcmd');
    @SybaseAPI.dbcmdrow              := GetAddress('dbcmdrow');
    @SybaseAPI.dbcolbrowse           := GetAddress('dbcolbrowse');
    @SybaseAPI.dbcollen              := GetAddress('dbcollen');
    @SybaseAPI.dbcolname             := GetAddress('dbcolname');
    @SybaseAPI.dbcolsource           := GetAddress('dbcolsource');
  //  @SybaseAPI.dbcoltypeinfo         := GetAddress('dbcoltypeinfo');
    @SybaseAPI.dbcoltype             := GetAddress('dbcoltype');
    @SybaseAPI.dbcolutype            := GetAddress('dbcolutype');
    @SybaseAPI.dbconvert             := GetAddress('dbconvert');
    @SybaseAPI.dbcount               := GetAddress('dbcount');
    @SybaseAPI.dbcurcmd              := GetAddress('dbcurcmd');
    @SybaseAPI.dbcurrow              := GetAddress('dbcurrow');
    @SybaseAPI.dbcursor              := GetAddress('dbcursor');
    @SybaseAPI.dbcursorbind          := GetAddress('dbcursorbind');
    @SybaseAPI.dbcursorclose         := GetAddress('dbcursorclose');
    @SybaseAPI.dbcursorcolinfo       := GetAddress('dbcursorcolinfo');
    @SybaseAPI.dbcursorfetch         := GetAddress('dbcursorfetch');
    @SybaseAPI.dbcursorinfo          := GetAddress('dbcursorinfo');
    @SybaseAPI.dbcursoropen          := GetAddress('dbcursoropen');
    @SybaseAPI.dbdata                := GetAddress('dbdata');
    @SybaseAPI.dbdatecrack           := GetAddress('dbdatecrack');
    @SybaseAPI.dbdatlen              := GetAddress('dbdatlen');
    @SybaseAPI.dbdead                := GetAddress('dbdead');
    @SybaseAPI.dbexit                := GetAddress('dbexit');
    @SybaseAPI.dbfcmd                := GetAddress('dbfcmd');
    @SybaseAPI.dbfirstrow            := GetAddress('dbfirstrow');
    @SybaseAPI.dbfreebuf             := GetAddress('dbfreebuf');
    @SybaseAPI.dbloginfree           := GetAddress('dbloginfree');
    @SybaseAPI.dbfreequal            := GetAddress('dbfreequal');
    @SybaseAPI.dbgetchar             := GetAddress('dbgetchar');
    @SybaseAPI.dbgetmaxprocs         := GetAddress('dbgetmaxprocs');
    @SybaseAPI.dbgetoff              := GetAddress('dbgetoff');
    @SybaseAPI.dbgetpacket           := GetAddress('dbgetpacket');
    @SybaseAPI.dbgetrow              := GetAddress('dbgetrow');
    @SybaseAPI.dbgetuserdata         := GetAddress('dbgetuserdata');
    @SybaseAPI.dbhasretstat          := GetAddress('dbhasretstat');
    @SybaseAPI.dbinit                := GetAddress('dbinit');
    @SybaseAPI.dbisavail             := GetAddress('dbisavail');
    @SybaseAPI.dbisopt               := GetAddress('dbisopt');
    @SybaseAPI.dblastrow             := GetAddress('dblastrow');
    @SybaseAPI.dblogin               := GetAddress('dblogin');
    @SybaseAPI.dbmorecmds            := GetAddress('dbmorecmds');
    @SybaseAPI.dbmoretext            := GetAddress('dbmoretext');
    @SybaseAPI.dbname                := GetAddress('dbname');
    @SybaseAPI.dbnextrow             := GetAddress('dbnextrow');
    @SybaseAPI.dbnullbind            := GetAddress('dbnullbind');
    @SybaseAPI.dbnumalts             := GetAddress('dbnumalts');
    @SybaseAPI.dbnumcols             := GetAddress('dbnumcols');
    @SybaseAPI.dbnumcompute          := GetAddress('dbnumcompute');
    @SybaseAPI.dbnumorders           := GetAddress('dbnumorders');
    @SybaseAPI.dbnumrets             := GetAddress('dbnumrets');
    @SybaseAPI.dbopen                := GetAddress('dbopen');
    @SybaseAPI.dbordercol            := GetAddress('dbordercol');
    @SybaseAPI.dbprhead              := GetAddress('dbprhead');
    @SybaseAPI.dbprrow               := GetAddress('dbprrow');
    @SybaseAPI.dbprtype              := GetAddress('dbprtype');
    @SybaseAPI.dbqual                := GetAddress('dbqual');
    @SybaseAPI.dbreadtext            := GetAddress('dbreadtext');
    @SybaseAPI.dbresults             := GetAddress('dbresults');
    @SybaseAPI.dbretdata             := GetAddress('dbretdata');
    @SybaseAPI.dbretlen              := GetAddress('dbretlen');
    @SybaseAPI.dbretname             := GetAddress('dbretname');
    @SybaseAPI.dbretstatus           := GetAddress('dbretstatus');
    @SybaseAPI.dbrettype             := GetAddress('dbrettype');
    @SybaseAPI.dbrows                := GetAddress('dbrows');
    @SybaseAPI.dbrowtype             := GetAddress('dbrowtype');
    @SybaseAPI.dbrpcinit             := GetAddress('dbrpcinit');
    @SybaseAPI.dbrpcparam            := GetAddress('dbrpcparam');
    @SybaseAPI.dbrpcsend             := GetAddress('dbrpcsend');
    @SybaseAPI.dbrpwclr              := GetAddress('dbrpwclr');
    @SybaseAPI.dbsetavail            := GetAddress('dbsetavail');
    @SybaseAPI.dbsetmaxprocs         := GetAddress('dbsetmaxprocs');
    @SybaseAPI.dbsetlname            := GetAddress('dbsetlname');
    @SybaseAPI.dbsetlogintime        := GetAddress('dbsetlogintime');
    @SybaseAPI.dbsetnull             := GetAddress('dbsetnull');
    @SybaseAPI.dbsetopt              := GetAddress('dbsetopt');
    @SybaseAPI.dbsettime             := GetAddress('dbsettime');
    @SybaseAPI.dbsetuserdata         := GetAddress('dbsetuserdata');
    @SybaseAPI.dbsqlexec             := GetAddress('dbsqlexec');
    @SybaseAPI.dbsqlok               := GetAddress('dbsqlok');
    @SybaseAPI.dbsqlsend             := GetAddress('dbsqlsend');
    @SybaseAPI.dbstrcpy              := GetAddress('dbstrcpy');
    @SybaseAPI.dbstrlen              := GetAddress('dbstrlen');
    @SybaseAPI.dbtabbrowse           := GetAddress('dbtabbrowse');
    @SybaseAPI.dbtabcount            := GetAddress('dbtabcount');
    @SybaseAPI.dbtabname             := GetAddress('dbtabname');
    @SybaseAPI.dbtabsource           := GetAddress('dbtabsource');
    @SybaseAPI.dbtsnewlen            := GetAddress('dbtsnewlen');
    @SybaseAPI.dbtsnewval            := GetAddress('dbtsnewval');
    @SybaseAPI.dbtsput               := GetAddress('dbtsput');
    @SybaseAPI.dbtxptr               := GetAddress('dbtxptr');
    @SybaseAPI.dbtxtimestamp         := GetAddress('dbtxtimestamp');
    @SybaseAPI.dbtxtsnewval          := GetAddress('dbtxtsnewval');
    @SybaseAPI.dbtxtsput             := GetAddress('dbtxtsput');
    @SybaseAPI.dbuse                 := GetAddress('dbuse');
    @SybaseAPI.dbvarylen             := GetAddress('dbvarylen');
    @SybaseAPI.dbwillconvert         := GetAddress('dbwillconvert');
    @SybaseAPI.dbwritetext           := GetAddress('dbwritetext');

    SybaseAPI.dbinit;

    OldSybaseErrorHandle := SybaseAPI.dberrhandle(SybaseErrorHandle);
    OldSybaseMessageHandle := SybaseAPI.dbmsghandle(SybaseMessageHandle);
  end;
end;

function TZDBLibSybaseASE125PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibSybaseASE125PlainDriver.Create;
end;

constructor TZDBLibSybaseASE125PlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  Loader.AddLocation(LIBSYBDB_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    Loader.AddLocation(LIBSYBDB_LINUX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}

  DBVariables.DBoptions[Z_PARSEONLY]      := DBLIBDBPARSEONLY;
  DBVariables.DBoptions[Z_SHOWPLAN]       := DBLIBDBSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC]         := DBLIBDBNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE]    := DBLIBDBARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT]        := DBLIBDBNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT]     := DBLIBDBARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT]      := DBLIBDBTEXTLIMIT;
  DBVariables.DBoptions[Z_OFFSET]         := DBLIBDBOFFSET;
  DBVariables.DBoptions[Z_STAT]           := DBLIBDBSTAT;
  DBVariables.DBoptions[Z_STORPROCID]     := DBLIBDBSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER]         := DBLIBDBBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE]     := DBLIBDBNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT]       := DBLIBDBROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE]       := DBLIBDBTEXTSIZE;
  DBVariables.DBoptions[Z_CLIENTCURSORS]  := DBLIBDBCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME] 	      := DBLIBDBSET_TIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT]    := DBLIBDBQUOTEDIDENT;
  DBVariables.DBoptions[Z_ANSITOOEM]      := DBLIBDBANSITOOEM;
  DBVariables.DBoptions[Z_OEMTOANSI]      := DBLIBDBOEMTOANSI;
  {MSSQL Loginrec manipulations}
  DBVariables.DBSetLoginRec[Z_SETHOST]    := SYBDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER]    := SYBDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD]     := SYBDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID]     := SYBDBSETHID;
  DBVariables.DBSetLoginRec[Z_SETAPP]     := SYBDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETBCP]     := SYBDBSETBCP;
  DBVariables.DBSetLoginRec[Z_SETLANG]    := SYBDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETNOSHORT] := SYBDBSETNOSHORT;
  DBVariables.DBSetLoginRec[Z_SETHIER]    := SYBDBSETHIER;
  DBVariables.DBSetLoginRec[Z_SETCHARSET] := SYBDBSETCHARSET;
  DBVariables.DBSetLoginRec[Z_SETPACKET]  := SYBDBSETPACKET;
  DBVariables.DBSetLoginRec[Z_SETENCRYPT] := SYBDBSETENCRYPT;
  DBVariables.dbSetLoginRec[Z_SETLABELED] := SYBDBSETLABELED;
end;

destructor TZDBLibSybaseASE125PlainDriver.Destroy;
begin
  if Loader.Loaded then
  begin
    SybaseAPI.dberrhandle(OldSybaseErrorHandle);
    SybaseAPI.dbmsghandle(OldSybaseMessageHandle);
    SybaseAPI.dbexit;
  end;
  inherited Destroy;
end;

procedure TZDBLibSybaseASE125PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZDBLibSybaseASE125PlainDriver.GetProtocol: string;
begin
  Result := 'sybase';
end;

function TZDBLibSybaseASE125PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for Sybase ASE 12.5';
end;

function TZDBLibSybaseASE125PlainDriver.dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool;
begin
  Result := SybaseAPI.dbcolbrowse(Proc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := SybaseAPI.dbDead(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbLogin: PLOGINREC;
begin
  Result := SybaseAPI.dbLogin;
end;

procedure TZDBLibSybaseASE125PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  SybaseAPI.dbLoginFree(Login);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLoginTime(Seconds: DBINT): RETCODE;
begin
  Result := SybaseAPI.dbsetlogintime(Seconds);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, Value, Item);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, HostName, Self.DBVariables.dbSetLoginRec[Z_SETHOST]);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, UserName, Self.DBVariables.dbSetLoginRec[Z_SETUSER]);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, Password, Self.DBVariables.dbSetLoginRec[Z_SETPWD]);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, AppName, Self.DBVariables.dbSetLoginRec[Z_SETAPP]);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, NatLangName, Self.DBVariables.dbSetLoginRec[Z_SETLANG]);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbsetlname(Login, CharsetName, Self.DBVariables.dbSetLoginRec[Z_SETCHARSET]);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLSecure(Login: PLOGINREC): RETCODE;
begin
  Result := 0
end;

function TZDBLibSybaseASE125PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := SybaseAPI.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibSybaseASE125PlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  Result := SybaseAPI.dbOpen(Login, Host);
end;

function TZDBLibSybaseASE125PlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbcancel(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbcmd(dbProc, Cmd);
end;

function TZDBLibSybaseASE125PlainDriver.dbSqlExec(dbProc: PDBPROCESS; Async: Boolean=False): RETCODE;
begin
  if Async then
    Result := dbSqlExecSync(dbProc)
  else
    Result := dbSqlExecAsync(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbSqlExec(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
var
  lStartTick : Int64;
begin
  Result := SybaseAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartTick := GetTickCount;
    repeat
      continue;
    until (GetTickCount > lStartTick + TIMEOUT_MAXIMUM * 1000) or (dbdataready(dbProc) = TRUE);
    Result := SybaseAPI.dbsqlok(dbProc);
  end;
end;

function TZDBLibSybaseASE125PlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbResults(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbCanQuery(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbMoreCmds(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.dbUse(dbProc, dbName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
begin
  Result := SybaseAPI.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZDBLibSybaseASE125PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbClose(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := SybaseAPI.dbName(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbCmdRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbNumCols(dbProc: PDBPROCESS): DBINT;
begin
  Result := SybaseAPI.dbNumCols(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
begin
  Result := SybaseAPI.dbColName(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := SybaseAPI.dbColType(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
begin
  Result := SybaseAPI.dbColLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
begin
  Result := SybaseAPI.dbData(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := SybaseAPI.dbDatLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
  SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
begin
  Result := SybaseAPI.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibSybaseASE125PlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := SybaseAPI.dbNextRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
begin
  Result := SybaseAPI.dbGetRow(dbProc, Row);
end;

function TZDBLibSybaseASE125PlainDriver.dbCount(dbProc: PDBPROCESS): DBINT;
begin
  Result := SybaseAPI.dbCount(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := SybaseAPI.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
begin
  Result := SybaseAPI.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbRpcSend(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := SybaseAPI.dbSqlOk(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetStatus(dbProc: PDBPROCESS): DBINT;
begin
  Result := SybaseAPI.dbRetStatus(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := SybaseAPI.dbHasRetStat(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
begin
  Result := SybaseAPI.dbRetName(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
begin
  Result := SybaseAPI.dbRetData(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := SybaseAPI.dbRetLen(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := SybaseAPI.dbRetType(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := 0;
end;

function TZDBLibSybaseASE125PlainDriver.dbdataready(Proc: PDBPROCESS): LongBool;
begin
  Result := Proc <> nil;
end;

{TZDBLibMSSQL7PlainDriver}

procedure TZDBLibMSSQL7PlainDriver.LoadApi;
begin
  inherited LoadAPI;
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    //@MsSQLAPI.dbtablecolinfo  := GetAddress('dbtablecolinfo');
    @MsSQLAPI.dbdataready           := GetAddress('dbdataready');
    @MsSQLAPI.dbdatecrack           := GetAddress('dbdatecrack');
    @MsSQLAPI.dbdatlen              := GetAddress('dbdatlen');
    @MsSQLAPI.dbdead                := GetAddress('dbdead');
    @MsSQLAPI.dbclose               := GetAddress('dbclose');
    @MsSQLAPI.dbcolbrowse           := GetAddress('dbcolbrowse');
    @MsSQLAPI.dbcolinfo             := GetAddress('dbcolinfo');
    @MsSQLAPI.dbcount               := GetAddress('dbcount');
    @MsSQLAPI.dbcursorbind          := GetAddress('dbcursorbind');
    @MsSQLAPI.dbcursorclose         := GetAddress('dbcursorclose');
    @MsSQLAPI.dbcursorcolinfo       := GetAddress('dbcursorcolinfo');
    @MsSQLAPI.dbcursorfetch         := GetAddress('dbcursorfetch');
    @MsSQLAPI.dbcursorfetchex       := GetAddress('dbcursorfetchex');
    @MsSQLAPI.dbcursorinfo          := GetAddress('dbcursorinfo');
    @MsSQLAPI.dbcursorinfoex        := GetAddress('dbcursorinfoex');
    @MsSQLAPI.dbcursoropen          := GetAddress('dbcursoropen');
    @MsSQLAPI.dbWinexit             := GetAddress('dbwinexit');
    @MsSQLAPI.dbenlisttrans         := GetAddress('dbenlisttrans');
    @MsSQLAPI.dbenlistxatrans       := GetAddress('dbenlistxatrans');
    @MsSQLAPI.dbfreelogin           := GetAddress('dbfreelogin');
    @MsSQLAPI.dbgetmaxprocs         := GetAddress('dbgetmaxprocs');
    @MsSQLAPI.dbgetpacket           := GetAddress('dbgetpacket');
    @MsSQLAPI.dbgetuserdata         := GetAddress('dbgetuserdata');
    @MsSQLAPI.dbhasretstat          := GetAddress('dbhasretstat');
    @MsSQLAPI.dbinit                := GetAddress('dbinit');
    @MsSQLAPI.dbisavail             := GetAddress('dbisavail');
    @MsSQLAPI.dbisopt               := GetAddress('dbisopt');
    @MsSQLAPI.dbprocinfo            := GetAddress('dbprocinfo');
    @MsSQLAPI.dbrpcexec             := GetAddress('dbrpcexec');
    @MsSQLAPI.dbserverenum          := GetAddress('dbserverenum');
    @MsSQLAPI.dbsetmaxprocs         := GetAddress('dbsetmaxprocs');
    @MsSQLAPI.dbsetlpacket          := GetAddress('dbsetlpacket');
    @MsSQLAPI.dbsetopt              := GetAddress('dbsetopt');
    @MsSQLAPI.dbtabbrowse           := GetAddress('dbtabbrowse');
    @MsSQLAPI.dbvarylen             := GetAddress('dbvarylen');
    @MsSQLAPI.dbwillconvert         := GetAddress('dbwillconvert');
    @MsSQLAPI.dbupdatetext          := GetAddress('dbupdatetext');
    MsSQLAPI.dbinit;
  end;
  OldMsSQLErrorHandle := DBLibAPI.dberrhandle(DbLibErrorHandle);
  OldMsSQLMessageHandle := DBLibAPI.dbmsghandle(DbLibMessageHandle);
end;

function TZDBLibMSSQL7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibMSSQL7PlainDriver.Create;
end;

constructor TZDBLibMSSQL7PlainDriver.Create;
begin
  inherited Create;
  Loader.AddLocation(NTWDBLIB_DLL_LOCATION);

  DBVariables.DBoptions[Z_PARSEONLY]      := DBLIBDBPARSEONLY;
  DBVariables.DBoptions[Z_SHOWPLAN]       := DBLIBDBSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC]         := DBLIBDBNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE]    := DBLIBDBARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT]        := DBLIBDBNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT]     := DBLIBDBARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT]      := DBLIBDBTEXTLIMIT;
  DBVariables.DBoptions[Z_OFFSET]         := DBLIBDBOFFSET;
  DBVariables.DBoptions[Z_STAT]           := DBLIBDBSTAT;
  DBVariables.DBoptions[Z_STORPROCID]     := DBLIBDBSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER]         := DBLIBDBBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE]     := DBLIBDBNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT]       := DBLIBDBROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE]       := DBLIBDBTEXTSIZE;
  DBVariables.DBoptions[Z_CLIENTCURSORS]  := DBLIBDBCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME] 	      := DBLIBDBSET_TIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT] 	  := DBLIBDBQUOTEDIDENT;
  DBVariables.DBoptions[Z_ANSITOOEM]      := DBLIBDBANSITOOEM;
  DBVariables.DBoptions[Z_OEMTOANSI]      := DBLIBDBOEMTOANSI;
  {MsSQL Loginrec manipulations}
  DBVariables.DBSetLoginRec[Z_SETHOST]    := MSDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER]    := MSDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD]     := MSDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID]     := MSDBSETID;
  DBVariables.DBSetLoginRec[Z_SETAPP]     := MSDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETSECURE]  := MSDBSETSECURE;
  DBVariables.DBSetLoginRec[Z_SETLANG]    := MSDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETLOGINTIME]:= MSDBSET_LOGIN_TIME;
  DBVariables.DBSetLoginRec[Z_SETFALLBACK]:= MSDBSETFALLBACK;
end;

destructor TZDBLibMSSQL7PlainDriver.Destroy;
begin
  if Loader.Loaded then
  begin
    DbLibAPI.dberrhandle(DbLibErrorHandle);
    DbLibAPI.dbmsghandle(DbLibMessageHandle);
    MsSQLAPI.dbWinexit;
    DbLibAPI.dbExit;
  end;
  inherited Destroy;
end;

procedure TZDBLibMSSQL7PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZDBLibMSSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'mssql';
end;

function TZDBLibMSSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for MS SQL 7+';
end;

function TZDBLibMSSQL7PlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, nil, Self.DBVariables.dbSetLoginRec[Z_SETSECURE]);
end;

function TZDBLibMSSQL7PlainDriver.dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool;
begin
  Result := MsSQLAPI.dbcolbrowse(Proc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := MsSQLAPI.dbDead(dbProc);
end;

procedure TZDBLibMSSQL7PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  MsSQLAPI.dbfreelogin(Login);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := DBFAIL;
end;

function TZDBLibMSSQL7PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := MsSQLAPI.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibMSSQL7PlainDriver.dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
var
  lStartTick : Int64;
begin
  Result := DBLibAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartTick := GetTickCount;
    repeat
      continue;
    until (GetTickCount > lStartTick + TIMEOUT_MAXIMUM * 1000) or (MsSQLAPI.dbdataready(dbProc) = TRUE);
    Result := DBLibAPI.dbsqlok(dbProc);
  end;
end;

function TZDBLibMSSQL7PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
begin
  Result := MsSQLAPI.dbSetOpt(dbProc, Option, Char_Param);
end;

function TZDBLibMSSQL7PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := MsSQLAPI.dbClose(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := MsSQLAPI.dbDatLen(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbCount(dbProc: PDBPROCESS): DBINT;
begin
  Result := MsSQLAPI.dbCount(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := MsSQLAPI.dbHasRetStat(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbdataready(Proc: PDBPROCESS): LongBool;
begin
  Result := MsSQLAPI.dbdataready(Proc);
end;

{ TFreeTGDBasePlainDriver }

{ TZFreeTDSBasePlainDriver }

procedure TZFreeTDSBasePlainDriver.LoadApi;
begin
  inherited LoadAPI;
  with Loader do
  begin
    @FreeTDSAPI.db12hour        := GetAddress('db12hour');
    @FreeTDSAPI.dbaltbind_ps    := GetAddress('dbaltbind_ps');
    @FreeTDSAPI.dbbufsize       := GetAddress('dbbufsize');
    @FreeTDSAPI.dbclose         := GetAddress('dbclose');
    @FreeTDSAPI.dbtablecolinfo  := GetAddress('dbtablecolinfo');
    @FreeTDSAPI.dbcolbrowse     := GetAddress('dbcolbrowse');
    @FreeTDSAPI.dbcolinfo       := GetAddress('dbcolinfo');
    @FreeTDSAPI.dbconvert_ps    := GetAddress('dbconvert_ps');
    @FreeTDSAPI.dbcount         := GetAddress('dbcount');
    @FreeTDSAPI.dbdatecmp       := GetAddress('dbdatecmp');
    @FreeTDSAPI.dbdatecrack     := GetAddress('dbdatecrack');
    @FreeTDSAPI.dbdatlen        := GetAddress('dbdatlen');
    @FreeTDSAPI.dbdead          := GetAddress('dbdead');
    @FreeTDSAPI.dbgetcharset    := GetAddress('dbgetcharset');
    @FreeTDSAPI.dbgetlusername  := GetAddress('dbgetlusername');
    @FreeTDSAPI.dbgetmaxprocs   := GetAddress('dbgetmaxprocs');
    @FreeTDSAPI.dbgetnatlanf    := GetAddress('dbgetnatlanf');
    @FreeTDSAPI.dbgetpacket     := GetAddress('dbgetpacket');
    @FreeTDSAPI.dbgetuserdata   := GetAddress('dbgetuserdata');
    @FreeTDSAPI.dbhasretstat    := GetAddress('dbhasretstat');
    @FreeTDSAPI.dbinit          := GetAddress('dbinit');
    @FreeTDSAPI.dbiordesc       := GetAddress('dbiordesc');
    @FreeTDSAPI.dbiowdesc       := GetAddress('dbiowdesc');
    @FreeTDSAPI.dbisavail       := GetAddress('dbisavail');
    @FreeTDSAPI.dbisopt         := GetAddress('dbisopt');
    @FreeTDSAPI.dbloginfree     := GetAddress('dbloginfree');
    @FreeTDSAPI.dbmny4cmp       := GetAddress('dbmny4cmp');
    @FreeTDSAPI.dbmnycmp        := GetAddress('dbmnycmp');
    @FreeTDSAPI.dbmny4add       := GetAddress('dbmny4add');
    @FreeTDSAPI.dbmnydec        := GetAddress('dbmnydec');
    @FreeTDSAPI.dbmnyinc        := GetAddress('dbmnyinc');
    @FreeTDSAPI.dbmnymaxpos     := GetAddress('dbmnymaxpos');
    @FreeTDSAPI.dbmnymaxneg     := GetAddress('dbmnymaxneg');
    @FreeTDSAPI.dbmny4minus     := GetAddress('dbmny4minus');
    @FreeTDSAPI.dbmnyminus      := GetAddress('dbmnyminus');
    @FreeTDSAPI.dbmny4sub       := GetAddress('dbmny4sub');
    @FreeTDSAPI.dbmnysub        := GetAddress('dbmnysub');
    @FreeTDSAPI.dbmny4copy      := GetAddress('dbmny4copy');
    @FreeTDSAPI.dbmnycopy       := GetAddress('dbmnycopy');
    @FreeTDSAPI.dbmny4zero      := GetAddress('dbmny4zero');
    @FreeTDSAPI.dbmnyzero       := GetAddress('dbmnyzero');
    @FreeTDSAPI.dbmonthname     := GetAddress('dbmonthname');
    @FreeTDSAPI.tdsdbopen       := GetAddress('tdsdbopen');


    @FreeTDSAPI.DRBUF           := GetAddress('DRBUF');
    @FreeTDSAPI.dbrecftos       := GetAddress('dbrecftos');
    @FreeTDSAPI.dbresults_r     := GetAddress('dbresults_r');
    //@FreeTDSAPI.dbsechandle     := GetAddress('dbsechandle');
    @FreeTDSAPI.dbservcharset   := GetAddress('dbservcharset');
    @FreeTDSAPI.dbsafestr       := GetAddress('dbsafestr');
    //@FreeTDSAPI.dbsetbusy       := GetAddress('dbsetbusy');
    @FreeTDSAPI.dbsetdefcharset := GetAddress('dbsetdefcharset');
    @FreeTDSAPI.dbsetifile      := GetAddress('dbsetifile');
    //@FreeTDSAPI.dbsetinterrupt  := GetAddress('dbsetinterrupt');
    @FreeTDSAPI.dbsetmaxprocs   := GetAddress('dbsetmaxprocs');
    @FreeTDSAPI.dbsetopt        := GetAddress('dbsetopt');
    @FreeTDSAPI.dbsetrow        := GetAddress('dbsetrow');
    @FreeTDSAPI.dbsetversion    := GetAddress('dbsetversion');
    @FreeTDSAPI.dbspid          := GetAddress('dbspid');
    @FreeTDSAPI.dbspr1row       := GetAddress('dbspr1row');
    @FreeTDSAPI.dbspr1rowlen    := GetAddress('dbspr1rowlen');
    @FreeTDSAPI.dbsprhead       := GetAddress('dbsprhead');
    @FreeTDSAPI.dbsprline       := GetAddress('dbsprline');
    @FreeTDSAPI.dbvarylen       := GetAddress('dbvarylen');

    @FreeTDSAPI.dbtds           := GetAddress('dbtds');
    @FreeTDSAPI.dbtextsize      := GetAddress('dbtextsize');
    @FreeTDSAPI.dbwillconvert   := GetAddress('dbwillconvert');

    (* LOGINREC manipulation *)
    @FreeTDSAPI.dbsetlbool      := GetAddress('dbsetlbool');
    @FreeTDSAPI.dbsetllong      := GetAddress('dbsetllong');
    @FreeTDSAPI.dbsetlversion   := GetAddress('dbsetlversion');

  end;
  FreeTDSAPI.dbinit;

  OldFreeTDSErrorHandle := DBLibAPI.dberrhandle(DbLibErrorHandle);
  OldFreeTDSMessageHandle := DBLibAPI.dbmsghandle(DbLibMessageHandle);
end;

constructor TZFreeTDSBasePlainDriver.Create;
begin
  inherited create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(FREETDS_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    FLoader.AddLocation(FREETDS_LINUX_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(FREETDS_OSX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}

  DBVariables.DBoptions[Z_PARSEONLY]      := TDSPARSEONLY;
  DBVariables.DBoptions[Z_ESTIMATE]       := TDSESTIMATE;
  DBVariables.DBoptions[Z_SHOWPLAN]       := TDSSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC]         := TDSNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE]    := TDSARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT]        := TDSNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT]     := TDSARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT]      := TDSTEXTLIMIT;
  DBVariables.DBoptions[Z_BROWSE]         := TDSBROWSE;
  DBVariables.DBoptions[Z_OFFSET]         := TDSOFFSET;
  DBVariables.DBoptions[Z_STAT]           := TDSSTAT;
  DBVariables.DBoptions[Z_ERRLVL]         := TDSERRLVL;
  DBVariables.DBoptions[Z_CONFIRM]        := TDSCONFIRM;
  DBVariables.DBoptions[Z_STORPROCID]     := TDSSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER]         := TDSBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE]     := TDSNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT]       := TDSROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE]       := TDSTEXTSIZE;
  DBVariables.DBoptions[Z_NATLANG]        := TDSNATLANG;
  DBVariables.DBoptions[Z_DATEFORMAT]     := TDSDATEFORMAT;
  DBVariables.DBoptions[Z_PRPAD]          := TDSPRPAD;
  DBVariables.DBoptions[Z_PRCOLSEP]       := TDSPRCOLSEP;
  DBVariables.DBoptions[Z_PRLINELEN]      := TDSPRLINELEN;
  DBVariables.DBoptions[Z_PRLINESEP]      := TDSPRLINESEP;
  DBVariables.DBoptions[Z_LFCONVERT]      := TDSLFCONVERT;
  DBVariables.DBoptions[Z_DATEFIRST]      := TDSDATEFIRST;
  DBVariables.DBoptions[Z_CHAINXACTS]     := TDSCHAINXACTS;
  DBVariables.DBoptions[Z_FIPSFLAG]	      := TDSFIPSFLAG;
  DBVariables.DBoptions[Z_ISOLATION]      := TDSISOLATION;
  DBVariables.DBoptions[Z_AUTH]           := TDSAUTH;
  DBVariables.DBoptions[Z_IDENTITY]       := TDSIDENTITY;
  DBVariables.DBoptions[Z_NOIDCOL]        := TDSNOIDCOL;
  DBVariables.DBoptions[Z_DATESHORT]      := TDSDATESHORT;
  DBVariables.DBoptions[Z_CLIENTCURSORS]  := TDSCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME]        := TDSSETTIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT]    := TDSQUOTEDIDENT;
  DBVariables.DBoptions[Z_NUMOPTIONS]     := TDSNUMOPTIONS;
  DBVariables.DBoptions[Z_PADOFF]         := TDSPADOFF;
  DBVariables.DBoptions[Z_PADON]          := TDSPADON;
  DBVariables.DBoptions[Z_OFF]            := TDSOFF;
  DBVariables.DBoptions[Z_ON]             := TDSON;
  DBVariables.DBoptions[Z_NOSUCHOPTION]   := NOSUCHOPTION;
  DBVariables.DBoptions[Z_MAXOPTTEXT]     := MAXOPTTEXT;
  {TDS Loginrec manipulations}
  DBVariables.DBSetLoginRec[Z_SETHOST]    := TDSDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER]    := TDSDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD]     := TDSDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID]     := TDSDBSETHID;
  DBVariables.DBSetLoginRec[Z_SETAPP]     := TDSDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETBCP]     := TDSDBSETBCP;
  DBVariables.DBSetLoginRec[Z_SETSECURE]  := TDSDBSETSECURE;
  DBVariables.DBSetLoginRec[Z_SETLANG]    := TDSDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETNOSHORT] := TDSDBSETNOSHORT;
  DBVariables.DBSetLoginRec[Z_SETHIER]    := TDSDBSETHIER;
  DBVariables.DBSetLoginRec[Z_SETCHARSET] := TDSDBSETCHARSET;
  DBVariables.DBSetLoginRec[Z_SETPACKET]  := TDSDBSETPACKET;
  DBVariables.DBSetLoginRec[Z_SETENCRYPT] := TDSDBSETENCRYPT;
  DBVariables.DBSetLoginRec[Z_SETLABELED] := TDSDBSETLABELED;
  DBVariables.DBSetLoginRec[Z_SETDBNAME]  := TDSDBSETDBNAME;
end;

destructor TZFreeTDSBasePlainDriver.Destroy;
begin
  if Loader.Loaded then
  begin
    DBLibAPI.dberrhandle(OldFreeTDSErrorHandle);
    DBLibAPI.dbmsghandle(OldFreeTDSMessageHandle);
    DBLibAPI.dbexit;
  end;
  inherited Destroy;
end;

function TZFreeTDSBasePlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS';
end;

function TZFreeTDSBasePlainDriver.GetDescription: string;
begin
  Result := 'Native FreeTDS driver for Sybase and MSSQL Servers';
end;

function TZFreeTDSBasePlainDriver.dbLogin: PLOGINREC;
begin
  Result := inherited dbLogin;
  if not Assigned(Result)  then
    if not  (dbsetlversion(Result) = DBSUCCEED ) then
    begin
      dbloginfree(Result);
      Result := nil;
    end;
end;

function TZFreeTDSBasePlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, CharsetName, DBVariables.dbSetLoginRec[Z_SETCHARSET]);
end;

function TZFreeTDSBasePlainDriver.dbSetLSecure(Login: PLOGINREC): RETCODE;
begin
  Result := DBLibAPI.dbsetlname(Login, nil, DBVariables.dbSetLoginRec[Z_SETSECURE]);
//  Result := FreeTDSAPI.dbsetlbool(Login, 1, Self.DBVariables.dbSetLoginRec[Z_SETSECURE]);
end;

function TZFreeTDSBasePlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, TDSDBVERSION_UNKNOWN);
end;

function TZFreeTDSBasePlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_UNKNOWN);
end;

procedure TZFreeTDSBasePlainDriver.tdsDumpOff;
begin
  FreeTDSAPI.tdsdump_off();
end;

procedure TZFreeTDSBasePlainDriver.tdsDumpOn;
begin
  FreeTDSAPI.tdsdump_on();
end;

procedure TZFreeTDSBasePlainDriver.tdsDump_Close;
begin
  FreeTDSAPI.tdsdump_close();
end;

procedure TZFreeTDSBasePlainDriver.tdsDump_Open(const FileName: String);
begin
  FreeTDSAPI.tdsdump_open(PAnsiChar( AnsiString(FileName) ));
end;

procedure TZFreeTDSBasePlainDriver.tdsSocketDone;
begin

end;

procedure TZFreeTDSBasePlainDriver.tdsSocketInit;
begin

end;

function TZFreeTDSBasePlainDriver.dbdataready(Proc: PDBPROCESS): LongBool;
begin
  Result := Proc <> nil;
end;

procedure TZFreeTDSBasePlainDriver.dbfreelogin(Login: PLOGINREC);
begin
  FreeTDSAPI.dbloginfree(Login);
end;

function TZFreeTDSBasePlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbDead(dbProc) = 1;
end;

procedure TZFreeTDSBasePlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  FreeTDSAPI.dbloginfree(Login);
end;

function TZFreeTDSBasePlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := FreeTDSAPI.dbsetmaxprocs(MaxProcs);
end;

function TZFreeTDSBasePlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE;
begin
  Result := FreeTDSAPI.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZFreeTDSBasePlainDriver.dbSetTime(queryTime: Integer): RETCODE;
begin
  Result := FreeTDSAPI.dbsetmaxprocs(queryTime);
end;

function TZFreeTDSBasePlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  FreeTDSAPI.dbClose(dbProc);
  Result := DBNOERR;
end;

function TZFreeTDSBasePlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := FreeTDSAPI.dbDatLen(dbProc, Column);
end;

function TZFreeTDSBasePlainDriver.dbCount(dbProc: PDBPROCESS): Integer;
begin
  Result := FreeTDSAPI.dbCount(dbProc);
end;

function TZFreeTDSBasePlainDriver.dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool;
begin
  Result := FreeTDSAPI.dbcolbrowse(Proc, Column) <> 0;
end;

function TZFreeTDSBasePlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbHasRetStat(dbProc) <> 0;
end;

function TZFreeTDSBasePlainDriver.dbColInfo(dbProc: PDBPROCESS;
  Column: Integer; var ADBInfo: DBCOL): RETCODE;
begin
  FillChar(ADBInfo, SizeOf(DBCol), #0);
  ADBInfo.SizeOfStruct := SizeOf(DBCol);
  Result := FreeTDSAPI.dbcolinfo(dbProc, CI_REGULAR, Column, 0, @ADBInfo);
end;

{ TZFreeTDS42MsSQLPlainDriver }
function TZFreeTDS42MsSQLPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS42MsSQLPlainDriver.Create;
end;

procedure TZFreeTDS42MsSQLPlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS42MsSQLPlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase<10)';
end;

function TZFreeTDS42MsSQLPlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 4.2 protocol for MsSQL <=6.5 Servers';
end;

function TZFreeTDS42MsSQLPlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, DBVER42);
end;

function TZFreeTDS42MsSQLPlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_42);
end;

{ TZFreeTDS42SybasePlainDriver }
function TZFreeTDS42SybasePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS42SybasePlainDriver.Create;
end;

procedure TZFreeTDS42SybasePlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS42SybasePlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL<=6.5)';
end;

function TZFreeTDS42SybasePlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 4.2 protocol for Sybase <10 Servers';
end;

function TZFreeTDS42SybasePlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := DBSUCCEED;
end;

function TZFreeTDS42SybasePlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_42);
end;

{ TZFreeTDS50PlainDriver }
function TZFreeTDS50PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS50PlainDriver.Create;
end;

procedure TZFreeTDS50PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS50PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase-10+';
end;

function TZFreeTDS50PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 5.0 Protocol for Sybase >= 10 Servers ';
end;

function TZFreeTDS50PlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_46);
end;

{ TZFreeTDS70PlainDriver }
function TZFreeTDS70PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS70PlainDriver.Create;
end;

procedure TZFreeTDS70PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS70PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL-7.0';
end;

function TZFreeTDS70PlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, DBVER60);
end;

function TZFreeTDS70PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.0 Protocol for MsSQL 7.0 Servers';
end;

function TZFreeTDS70PlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_70);
end;

{ TZFreeTDS71PlainDriver }
function TZFreeTDS71PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS71PlainDriver.Create;
end;

procedure TZFreeTDS71PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS71PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL-2000';
end;

function TZFreeTDS71PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.1 Protocol for MsSQL 2000 Servers';
end;

function TZFreeTDS71PlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_70);
end;

{ TZFreeTDS72PlainDriver }
function TZFreeTDS72PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS72PlainDriver.Create;
end;

procedure TZFreeTDS72PlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

function TZFreeTDS72PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL>=2005';
end;

function TZFreeTDS72PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.2 Protocol for MsSQL 2005, 2008, 2012 Servers';
end;

function TZFreeTDS72PlainDriver.dbsetversion: RETCODE;
begin
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_72);
end;

initialization
  SQLErrors := TList.Create;
  SQLMessages := TList.Create;
finalization
//Free any record in the list if any
  while SQLErrors.Count > 0 do
  begin
    Dispose(SQLErrors.Items[0]);
    SQLErrors.Delete(0);
  end;
  if SQLErrors <> nil then
    FreeAndNil(SQLErrors);

//Free any record in the list if any
  while SQLMessages.Count > 0 do
  begin
    Dispose(SQLMessages.Items[0]);
    SQLMessages.Delete(0);
  end;
  if SQLMessages <> nil then
    FreeAndNil(SQLMessages);
end.
