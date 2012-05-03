{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain driver interface to DBLibrary         }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

{***************** Plain API Constants definition ****************}
const
  //from sqlfront.h:
  DBSETHOST=1;
  DBSETUSER=2;
  DBSETPWD=3;
  DBSETAPP=4;
  DBSETID=5;
  DBSETLANG=6;
  DBSETSECURE=7;
  //These two are defined by Microsoft for dbsetlversion():
  DBVER42=8;
  DBVER60=9;
  DBSET_LOGINTIME=10;
  DBSETFALLBACK=12;

{ dboptions }
  DBBUFFER              = 0;
  DBOFFSET              = 1;
  DBROWCOUNT            = 2;
  DBSTAT                = 3;
  DBTEXTLIMIT           = 4;
  DBTEXTSIZE            = 5;
  DBARITHABORT          = 6;
  DBARITHIGNORE         = 7;
  DBNOAUTOFREE          = 8;
  DBNOCOUNT             = 9;
  DBNOEXEC              = 10;
  DBPARSEONLY           = 11;
  DBSHOWPLAN            = 12;
  DBSTORPROCID		      = 13;
  DBANSITOOEM		        = 14;
  DBOEMTOANSI	          = 15;
  DBCLIENTCURSORS       = 16;
  DBSET_TIME            = 17;
  DBQUOTEDIDENT         = 18;

{ Decimal constants }
  MAXNUMERICLEN = 16;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  MAXTABLENAME = 30;
  MAXCOLNAMELEN= 30;

{****************** Plain API Types definition *****************}
type
{ DB-Library datatypes }
  {$IFDEF FPC}
    {$PACKRECORDS C}
  {$ENDIF}

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


type
{ TODO -ofjanos -cAPI :
Strange but I had to insert X1 and X2 into the structure to make it work.
I have not find any reason for this yet. }
  DBCOL = packed record
    SizeOfStruct: DBINT;
    Name:       array[0..MAXCOLNAMELEN] of AnsiChar;
    ActualName: array[0..MAXCOLNAMELEN] of AnsiChar;
    TableName:  array[0..MAXTABLENAME] of AnsiChar;
    X1:         Byte;
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
    X2:         Byte;
  end;
  PDBCOL = ^DBCOL;

type
  {** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface (IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']

    procedure CheckError;

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
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
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
  end;

  {** Implements a dblib driver for Sybase ASE 12.5 }
  TZDBLibSybaseASE125PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZDBLibPlainDriver)
  protected
    function Clone: IZPlainDriver; reintroduce;
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
     procedure Initialize(const Location: String);

    procedure CheckError;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE;
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
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): Integer;
    function dbColName(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
             SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
    function dbCount(dbProc: PDBPROCESS): Integer;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): Integer;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
    function dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
  end;

  {** Implements a dblib driver for MSSql7 }
  TZDBLibMSSQL7PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZDBLibPlainDriver)
  protected
    function Clone: IZPlainDriver; reintroduce;
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize(const Location: String);

    procedure CheckError;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: Integer): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE;
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
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): Integer;
    function dbColName(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
    function dbCount(dbProc: PDBPROCESS): Integer;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): Integer;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
    function dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
  end;

  {** Implements a generic dblib driver}
  TZDbLibBasePlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  private
    FreeTDSAPI: TFreeTDSAPI;
  protected
    procedure LoadApi; override;
  public
    constructor Create;
    destructor Destroy;

    procedure CheckError;

    //function dbinit: RETCODE;
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
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
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
  end;

  {** Implements a dblib driver for Sybase/MSSQL }
  TZFreeTDSPlainDriver = class (TZDbLibBasePlainDriver)
  private
    FreeTDSAPI: TFreeTDSAPI;
  protected
    procedure LoadApi; override;
  public
    constructor Create;
    destructor Destroy;

    function GetProtocol: string; override;
    function GetDescription: string; override;

  end;

implementation

uses SysUtils, ZPlainDbLibSybaseAse125, ZPlainDbLibMsSql7, ZPlainLoader;

{ TZDBLibSybaseASE125PlainDriver }

function TZDBLibSybaseASE125PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibSybaseASE125PlainDriver.Create;
end;

constructor TZDBLibSybaseASE125PlainDriver.Create;
begin
end;

function TZDBLibSybaseASE125PlainDriver.GetProtocol: string;
begin
  Result := 'sybase';
end;

function TZDBLibSybaseASE125PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for Sybase ASE 12.5';
end;

procedure TZDBLibSybaseASE125PlainDriver.Initialize(const Location: String);
begin
  ZPlainDBLibSybaseASE125.LibraryLoader.AddLocation(Location);
  ZPlainDBLibSybaseASE125.LibraryLoader.LoadIfNeeded;
end;


procedure TZDBLibSybaseASE125PlainDriver.CheckError;
var
  I: Integer;
  S: AnsiString;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
begin
  { TODO -ofjanos -cGeneral : Error handling should be based on connection object.
  At the moment it is global. }
  if (SybaseErrors.Count = 0) and (SybaseMessages.Count = 0) then
    Exit;
  S := '';
  for I := 0 to SybaseErrors.Count - 1 do
    S := S + PDBLibError(SybaseErrors.Items[I]).DbErrStr + ' ' + PDBLibError(SybaseErrors.Items[I]).OsErrStr + ' '#13;
  for I := 0 to SybaseMessages.Count - 1 do
    if PDBLibMessage(SybaseMessages.Items[I]).Severity > EXINFO then
      S := S + PDBLibMessage(SybaseMessages.Items[I]).MsgText + ' '#13;
  while SybaseErrors.Count > 0 do
  begin
    lErrorEntry := SybaseErrors.Items[0];
    Dispose(lErrorEntry);
    SybaseErrors.Delete(0);
  end;
  SybaseErrors.Clear;
  while SybaseMessages.Count > 0 do
  begin
    lMesageEntry := SybaseMessages.Items[0];
    Dispose(lMesageEntry);
    SybaseMessages.Delete(0);
  end;
  SybaseMessages.Clear;
  if S <> '' then
    raise Exception.Create(S);
end;


function TZDBLibSybaseASE125PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibSybaseASE125.dbDead(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbLogin: PLOGINREC;
begin
  Result := ZPlainDBLibSybaseASE125.dbLogin;
end;

procedure TZDBLibSybaseASE125PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  ZPlainDBLibSybaseASE125.dbLoginFree(Login);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLoginTime(Seconds: Integer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlogintime(Seconds);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlname(Login, Value, Item);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLHOST(Login, HostName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetluser(Login, UserName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlpwd(Login, Password);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLAPP(Login, AppName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLNATLANG(Login, NatLangName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLCHARSET(Login, CharsetName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := 0;
end;

function TZDBLibSybaseASE125PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibSybaseASE125PlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  Result := ZPlainDBLibSybaseASE125.dbOpen(Login, Host);
end;

function TZDBLibSybaseASE125PlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbcancel(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbcmd(dbProc, Cmd);
end;

function TZDBLibSybaseASE125PlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbSqlExec(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbResults(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbCanQuery(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbMoreCmds(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbUse(dbProc, dbName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZDBLibSybaseASE125PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbClose(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbName(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbCmdRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbNumCols(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbNumCols(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbColName(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbColName(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbColType(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
begin
  Result := ZPlainDBLibSybaseASE125.dbColLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
begin
  Result := ZPlainDBLibSybaseASE125.dbData(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbDatLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibSybaseASE125PlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := ZPlainDBLibSybaseASE125.dbNextRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
begin
  Result := ZPlainDBLibSybaseASE125.dbGetRow(dbProc, Row);
end;

function TZDBLibSybaseASE125PlainDriver.dbCount(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbCount(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcSend(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := ZPlainDBLibSybaseASE125.dbSqlOk(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetStatus(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetStatus(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibSybaseASE125.dbHasRetStat(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetName(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetData(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetLen(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetType(dbProc, RetNum);
end;


{TZDBLibMSSQL7PlainDriver}

function TZDBLibMSSQL7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibMSSQL7PlainDriver.Create;
end;

constructor TZDBLibMSSQL7PlainDriver.Create;
begin
end;

function TZDBLibMSSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'mssql';
end;

function TZDBLibMSSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for MS SQL 7+';
end;

procedure TZDBLibMSSQL7PlainDriver.Initialize(const Location: String);
begin
  ZPlainDBLibMSSql7.LibraryLoader.AddLocation(Location);
  ZPlainDBLibMSSql7.LibraryLoader.LoadIfNeeded;
end;


procedure TZDBLibMSSQL7PlainDriver.CheckError;
var
  I: Integer;
  S: AnsiString;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
begin
  { TODO -ofjanos -cGeneral : Error handling should be based on connection object.
  At the moment it is global. }
  if (MSSqlErrors.Count = 0) and (MSSqlMessages.Count = 0) then
    Exit;
  S := '';
  for I := 0 to MSSqlErrors.Count - 1 do
    S := S + PDBLibError(MSSqlErrors.Items[I]).DbErrStr + ' ' + PDBLibError(MSSqlErrors.Items[I]).OsErrStr + ' '#13;
  for I := 0 to MSSqlMessages.Count - 1 do
    if PDBLibMessage(MSSqlMessages.Items[I]).Severity > EXINFO then
      S := S + PDBLibMessage(MSSqlMessages.Items[I]).MsgText + ' '#13;
  while MSSqlErrors.Count > 0 do
  begin
    lErrorEntry := MSSqlErrors.Items[0];
    Dispose(lErrorEntry);
    MSSqlErrors.Delete(0);
  end;
  MSSqlErrors.Clear;
  while MSSqlMessages.Count > 0 do
  begin
    lMesageEntry := MSSqlMessages.Items[0];
    Dispose(lMesageEntry);
    MSSqlMessages.Delete(0);
  end;
  MSSqlMessages.Clear;
  if S <> '' then
    raise Exception.Create(S);
end;


function TZDBLibMSSQL7PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibMSSql7.dbDead(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbLogin: PLOGINREC;
begin
  Result := ZPlainDBLibMSSql7.dbLogin;
end;

procedure TZDBLibMSSQL7PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  ZPlainDBLibMSSql7.dbFreeLogin(Login);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLoginTime(Seconds: Integer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSetLoginTime(Seconds);
end;

function TZDBLibMSSQL7PlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlname(Login, Value, Item);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLHOST(Login, HostName);
end;

function TZDBLibMSSQL7PlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetluser(Login, UserName);
end;

function TZDBLibMSSQL7PlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlpwd(Login, Password);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLAPP(Login, AppName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName:
   PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLNATLANG(Login, NatLangName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := 0;
end;

function TZDBLibMSSQL7PlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlsecure(Login);
end;

function TZDBLibMSSQL7PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibMSSQL7PlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  Result := ZPlainDBLibMSSql7.dbOpen(Login, Host);
end;


function TZDBLibMSSQL7PlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbcancel(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbcmd(dbProc, Cmd);
end;

function TZDBLibMSSQL7PlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSqlExec(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbResults(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbCanQuery(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbMoreCmds(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbUse(dbProc, dbName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PAnsiChar = nil; Int_Param: Integer = -1): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSetOpt(dbProc, Option, Char_Param);
end;

function TZDBLibMSSQL7PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbClose(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := ZPlainDBLibMSSql7.dbName(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbCmdRow(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbNumCols(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbNumCols(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbColName(dbProc: PDBPROCESS; Column:
   Integer): PAnsiChar;
begin
  Result := ZPlainDBLibMSSql7.dbColName(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbColType(dbProc: PDBPROCESS; Column:
   Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbColType(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer):
   DBInt;
begin
  Result := ZPlainDBLibMSSql7.dbColLen(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbData(dbProc: PDBPROCESS; Column: Integer):
   PByte;
begin
  Result := ZPlainDBLibMSSql7.dbData(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbDatLen(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibMSSQL7PlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := ZPlainDBLibMSSql7.dbNextRow(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: Integer):
   STATUS;
begin
  Result := ZPlainDBLibMSSql7.dbGetRow(dbProc, Row);
end;

function TZDBLibMSSQL7PlainDriver.dbCount(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbCount(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcSend(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcExec(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRetStatus(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetStatus(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibMSSql7.dbHasRetStat(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar;
begin
  Result := ZPlainDBLibMSSql7.dbRetName(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
begin
  Result := ZPlainDBLibMSSql7.dbRetData(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetLen(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetType(dbProc, RetNum);
end;

{ TZDbLibBasePlainDriver }

constructor TZDbLibBasePlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
end;

destructor TZDbLibBasePlainDriver.Destroy;
begin

  inherited Destroy;
end;

procedure TZDbLibBasePlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    @FreeTDSAPI.dbadata         := GetAddress('dbadata');
    @FreeTDSAPI.dbadlen         := GetAddress('dbadlen');
    @FreeTDSAPI.dbaltbind       := GetAddress('dbaltbind');
    @FreeTDSAPI.dbaltbind_ps    := GetAddress('dbaltbind_ps');
    @FreeTDSAPI.dbaltcolid      := GetAddress('dbaltcolid');
    @FreeTDSAPI.dbaltlen        := GetAddress('dbaltlen');
    @FreeTDSAPI.dbaltop         := GetAddress('dbaltop');
    @FreeTDSAPI.dbalttype       := GetAddress('dbalttype');
    @FreeTDSAPI.dbaltutype      := GetAddress('dbaltutype');
    @FreeTDSAPI.dbanullbind     := GetAddress('dbanullbind');
    @FreeTDSAPI.dbbind          := GetAddress('dbbind');
    @FreeTDSAPI.dbbind_ps       := GetAddress('dbbind_ps');
    @FreeTDSAPI.dbbufsize       := GetAddress('dbbufsize');
    @FreeTDSAPI.dbbylist        := GetAddress('dbbylist');
    @FreeTDSAPI.dbcancel        := GetAddress('dbcancel');
    @FreeTDSAPI.dbcanquery      := GetAddress('dbcanquery');
    @FreeTDSAPI.dbchange        := GetAddress('dbchange');
    @FreeTDSAPI.dbclose         := GetAddress('dbclose');
    @FreeTDSAPI.dbclrbuf        := GetAddress('dbclrbuf');
    @FreeTDSAPI.dbclropt        := GetAddress('dbclropt');
    @FreeTDSAPI.dbcmd           := GetAddress('dbcmd');
    @FreeTDSAPI.dbcmdrow        := GetAddress('dbcmdrow');
    @FreeTDSAPI.dbtablecolinfo  := GetAddress('dbtablecolinfo');
    @FreeTDSAPI.dbcolinfo       := GetAddress('dbcolinfo');
    @FreeTDSAPI.dbcollen        := GetAddress('dbcollen');
    @FreeTDSAPI.dbcolname       := GetAddress('dbcolname');
    @FreeTDSAPI.dbcolsource     := GetAddress('dbcolsource');
    @FreeTDSAPI.dbcoltype       := GetAddress('dbcoltype');
    @FreeTDSAPI.dbcolutype      := GetAddress('dbcolutype');
    @FreeTDSAPI.dbconvert       := GetAddress('dbcolutype');
    @FreeTDSAPI.dbconvert_ps    := GetAddress('dbconvert_ps');
    @FreeTDSAPI.dbiscount       := GetAddress('dbiscount');
    @FreeTDSAPI.dbcount         := GetAddress('dbcount');
    @FreeTDSAPI.dbcurcmd        := GetAddress('dbcurcmd');
    @FreeTDSAPI.dbcurrow        := GetAddress('dbcurrow');
    @FreeTDSAPI.dbdata          := GetAddress('dbdata');
    @FreeTDSAPI.dbdatecmp       := GetAddress('dbdatecmp');
    @FreeTDSAPI.dbdatecrack     := GetAddress('dbdatecrack');
    @FreeTDSAPI.dbdatlen        := GetAddress('dbdatlen');
    @FreeTDSAPI.dbdead          := GetAddress('dbdead');
    @FreeTDSAPI.dberrhandle     := GetAddress('dberrhandle');
    @FreeTDSAPI.dbexit          := GetAddress('dbexit');
    @FreeTDSAPI.dbfcmd          := GetAddress('dbfcmd');
    @FreeTDSAPI.dbfirstrow      := GetAddress('dbfirstrow');
    @FreeTDSAPI.dbfreebuf       := GetAddress('dbfreebuf');
    @FreeTDSAPI.dbgetchar       := GetAddress('dbgetchar');
    @FreeTDSAPI.dbgetcharset    := GetAddress('dbgetcharset');
    @FreeTDSAPI.dbgetlusername  := GetAddress('dbgetlusername');
    @FreeTDSAPI.dbgetmaxprocs   := GetAddress('dbgetmaxprocs');
    @FreeTDSAPI.dbgetnatlanf    := GetAddress('dbgetnatlanf');
    @FreeTDSAPI.dbgetpacket     := GetAddress('dbgetpacket');
    @FreeTDSAPI.dbgetrow        := GetAddress('dbgetrow');
    @FreeTDSAPI.dbgettime       := GetAddress('dbgettime');
    @FreeTDSAPI.dbgetuserdata   := GetAddress('dbgetuserdata');
    @FreeTDSAPI.dbhasretstat    := GetAddress('dbhasretstat');
    @FreeTDSAPI.dbinit          := GetAddress('dbinit');
    @FreeTDSAPI.dbiordesc       := GetAddress('dbiordesc');
    @FreeTDSAPI.dbiowdesc       := GetAddress('dbiowdesc');
    @FreeTDSAPI.dbisavail       := GetAddress('dbisavail');
    @FreeTDSAPI.dbisopt         := GetAddress('dbisopt');
    @FreeTDSAPI.dblastrow       := GetAddress('dblastrow');
    @FreeTDSAPI.dblogin         := GetAddress('dblogin');
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
    @FreeTDSAPI.dbmorecmds      := GetAddress('dbmorecmds');
    @FreeTDSAPI.dbmoretext      := GetAddress('dbmoretext');
    @FreeTDSAPI.dbmsghandle     := GetAddress('dbmsghandle');
    @FreeTDSAPI.dbname          := GetAddress('dbname');
    @FreeTDSAPI.dbnextrow       := GetAddress('dbnextrow');
    @FreeTDSAPI.dbnullbind      := GetAddress('dbnullbind');
    @FreeTDSAPI.dbnumalts       := GetAddress('dbnumalts');
    @FreeTDSAPI.dbnumcols       := GetAddress('dbnumcols');
    @FreeTDSAPI.dbnumcompute    := GetAddress('dbnumcompute');
    @FreeTDSAPI.dbnumorders     := GetAddress('dbnumorders');
    @FreeTDSAPI.dbnumrets       := GetAddress('dbnumrets');
    @FreeTDSAPI.tdsdbopen       := GetAddress('tdsdbopen');
    @FreeTDSAPI.dbopen          := GetAddress('dbopen');

    @FreeTDSAPI.dbprhead        := GetAddress('dbprhead');
    @FreeTDSAPI.dbprrow         := GetAddress('dbprrow');
    @FreeTDSAPI.dbprtype        := GetAddress('dbprtype');
    @FreeTDSAPI.DRBUF           := GetAddress('DRBUF');
    @FreeTDSAPI.dbreadtext      := GetAddress('dbreadtext');
    @FreeTDSAPI.dbrecftos       := GetAddress('dbrecftos');
    @FreeTDSAPI.dbresults       := GetAddress('dbresults');
    @FreeTDSAPI.dbresults_r     := GetAddress('dbresults_r');
    @FreeTDSAPI.dbretdata       := GetAddress('dbretdata');
    @FreeTDSAPI.dbretlen        := GetAddress('dbretlen');
    @FreeTDSAPI.dbretname       := GetAddress('dbretname');
    @FreeTDSAPI.dbretstatus     := GetAddress('dbretstatus');
    @FreeTDSAPI.dbrettype       := GetAddress('dbrettype');
    @FreeTDSAPI.dbrows          := GetAddress('dbrows');
    @FreeTDSAPI.dbrowtype       := GetAddress('dbrowtype');
    @FreeTDSAPI.dbrpcinit       := GetAddress('dbrpcinit');
    @FreeTDSAPI.dbrpcparam      := GetAddress('dbrpcparam');
    @FreeTDSAPI.dbrpcsend       := GetAddress('dbrpcsend');
    @FreeTDSAPI.dbsafestr       := GetAddress('dbsafestr');
    //@FreeTDSAPI.dbsechandle     := GetAddress('dbsechandle');
    @FreeTDSAPI.dbservcharset   := GetAddress('dbservcharset');
    @FreeTDSAPI.dbsetavail      := GetAddress('dbsetavail');
    //@FreeTDSAPI.dbsetbusy       := GetAddress('dbsetbusy');
    @FreeTDSAPI.dbsetdefcharset := GetAddress('dbsetdefcharset');
    @FreeTDSAPI.dbsetifile      := GetAddress('dbsetifile');
    //@FreeTDSAPI.dbsetinterrupt  := GetAddress('dbsetinterrupt');
    @FreeTDSAPI.dbsetlogintime  := GetAddress('dbsetlogintime');
    @FreeTDSAPI.dbsetmaxprocs   := GetAddress('dbsetmaxprocs');
    @FreeTDSAPI.dbsetlname      := GetAddress('dbsetlname');
    @FreeTDSAPI.dbsetnull       := GetAddress('dbsetnull');
    @FreeTDSAPI.dbsetopt        := GetAddress('dbsetopt');
    @FreeTDSAPI.dbsetrow        := GetAddress('dbsetrow');
    @FreeTDSAPI.dbsettime       := GetAddress('dbsettime');
    @FreeTDSAPI.dbsetuserdata   := GetAddress('dbsetuserdata');
    @FreeTDSAPI.dbsetversion    := GetAddress('dbsetversion');
    @FreeTDSAPI.dbspid          := GetAddress('dbspid');
    @FreeTDSAPI.dbspr1row       := GetAddress('dbspr1row');
    @FreeTDSAPI.dbspr1rowlen    := GetAddress('dbspr1rowlen');
    @FreeTDSAPI.dbsprhead       := GetAddress('dbsprhead');
    @FreeTDSAPI.dbsprline       := GetAddress('dbsprline');
    @FreeTDSAPI.dbsqlexec       := GetAddress('dbsqlexec');
    @FreeTDSAPI.dbsqlok         := GetAddress('dbsqlok');
    @FreeTDSAPI.dbsqlsend       := GetAddress('dbsqlsend');
    @FreeTDSAPI.dbstrcpy        := GetAddress('dbstrcpy');
    @FreeTDSAPI.dbstrlen        := GetAddress('dbstrlen');
    @FreeTDSAPI.dbvarylen       := GetAddress('dbvarylen');

    @FreeTDSAPI.dbtds           := GetAddress('dbtds');
    @FreeTDSAPI.dbtextsize      := GetAddress('dbtextsize');
    @FreeTDSAPI.dbtxptr         := GetAddress('dbtxptr');
    @FreeTDSAPI.dbtxtimestamp   := GetAddress('dbtxtimestamp');
    @FreeTDSAPI.dbtxtsnewval    := GetAddress('dbtxtsnewval');
    @FreeTDSAPI.dbtxtsput       := GetAddress('dbtxtsput');
    @FreeTDSAPI.dbuse           := GetAddress('dbuse');
    @FreeTDSAPI.dbwillconvert   := GetAddress('dbwillconvert');
    @FreeTDSAPI.dbwritetext     := GetAddress('dbwritetext');

    (* LOGINREC manipulation *)
    @FreeTDSAPI.dbsetlbool      := GetAddress('dbsetlbool');
    @FreeTDSAPI.dbsetllong      := GetAddress('dbsetllong');
    @FreeTDSAPI.dbsetlversion   := GetAddress('dbsetlversion');
  end;
end;

procedure TZDbLibBasePlainDriver.CheckError;
var
  I: DBINT;
  S: AnsiString;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
begin
  { TODO -ofjanos -cGeneral : Error handling should be based on connection object.
  At the moment it is global. }
  {if (SybaseErrors.Count = 0) and (SybaseMessages.Count = 0) then
    Exit;
  S := '';
  for I := 0 to SybaseErrors.Count - 1 do
    S := S + PDBLibError(SybaseErrors.Items[I]).DbErrStr + ' ' + PDBLibError(SybaseErrors.Items[I]).OsErrStr + ' '#13;
  for I := 0 to SybaseMessages.Count - 1 do
    if PDBLibMessage(SybaseMessages.Items[I]).Severity > EXINFO then
      S := S + PDBLibMessage(SybaseMessages.Items[I]).MsgText + ' '#13;
  while SybaseErrors.Count > 0 do
  begin
    lErrorEntry := SybaseErrors.Items[0];
    Dispose(lErrorEntry);
    SybaseErrors.Delete(0);
  end;
  SybaseErrors.Clear;
  while SybaseMessages.Count > 0 do
  begin
    lMesageEntry := SybaseMessages.Items[0];
    Dispose(lMesageEntry);
    SybaseMessages.Delete(0);
  end;
  SybaseMessages.Clear;
  if S <> '' then
    raise Exception.Create(S);}
end;


function TZDbLibBasePlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbDead(dbProc);
end;

function TZDbLibBasePlainDriver.dbLogin: PLOGINREC;
begin
  Result := FreeTDSAPI.dbLogin;
end;

procedure TZDbLibBasePlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  FreeTDSAPI.dbLoginFree(Login);
end;

function TZDbLibBasePlainDriver.dbSetLoginTime(Seconds: DBINT): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlogintime(Seconds);
end;

function TZDbLibBasePlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, Value, Item);
end;

function TZDbLibBasePlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, HostName, DBSETHOST);
end;

function TZDbLibBasePlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, UserName, DBSETUSER);
end;

function TZDbLibBasePlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, Password, DBSETPWD);
end;

function TZDbLibBasePlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, AppName, DBSETAPP);
end;

function TZDbLibBasePlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, NatLangName, DBSETNATLANG);
end;

function TZDbLibBasePlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, CharsetName, DBSETCHARSET);
end;

function TZDbLibBasePlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := 0; //dbsetlname(Login, nil, DBSETSECURE);
end;

function TZDbLibBasePlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := FreeTDSAPI.dbsetmaxprocs(MaxProcs);
end;

function TZDbLibBasePlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  Result := FreeTDSAPI.dbOpen(Login, Host);
end;

function TZDbLibBasePlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbcancel(dbProc);
end;

function TZDbLibBasePlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbcmd(dbProc, Cmd);
end;

function TZDbLibBasePlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbSqlExec(dbProc);
end;

function TZDbLibBasePlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbResults(dbProc);
end;

function TZDbLibBasePlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbCanQuery(dbProc);
end;

function TZDbLibBasePlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbMoreCmds(dbProc);
end;

function TZDbLibBasePlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbUse(dbProc, dbName);
end;

function TZDbLibBasePlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
begin
  Result := FreeTDSAPI.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZDbLibBasePlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbClose(dbProc);
end;

function TZDbLibBasePlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := FreeTDSAPI.dbName(dbProc);
end;

function TZDbLibBasePlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbCmdRow(dbProc);
end;

function TZDbLibBasePlainDriver.dbNumCols(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbNumCols(dbProc);
end;

function TZDbLibBasePlainDriver.dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
begin
  Result := FreeTDSAPI.dbColName(dbProc, Column);
end;

function TZDbLibBasePlainDriver.dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbColType(dbProc, Column);
end;

function TZDbLibBasePlainDriver.dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
begin
  Result := FreeTDSAPI.dbColLen(dbProc, Column);
end;

function TZDbLibBasePlainDriver.dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
begin
  Result := FreeTDSAPI.dbData(dbProc, Column);
end;

function TZDbLibBasePlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbDatLen(dbProc, Column);
end;

function TZDbLibBasePlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
  SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDbLibBasePlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := FreeTDSAPI.dbNextRow(dbProc);
end;

function TZDbLibBasePlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
begin
  Result := FreeTDSAPI.dbGetRow(dbProc, Row);
end;

function TZDbLibBasePlainDriver.dbCount(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbCount(dbProc);
end;

function TZDbLibBasePlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDbLibBasePlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDbLibBasePlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcSend(dbProc);
end;

function TZDbLibBasePlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := FreeTDSAPI.dbSqlOk(dbProc);
end;

function TZDbLibBasePlainDriver.dbRetStatus(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbRetStatus(dbProc);
end;

function TZDbLibBasePlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbHasRetStat(dbProc) <> 0;
end;

function TZDbLibBasePlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
begin
  Result := FreeTDSAPI.dbRetName(dbProc, RetNum);
end;

function TZDbLibBasePlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
begin
  Result := FreeTDSAPI.dbRetData(dbProc, RetNum);
end;

function TZDbLibBasePlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbRetLen(dbProc, RetNum);
end;

function TZDbLibBasePlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbRetType(dbProc, RetNum);
end;

{ TZFreeTDSPlainDriver }

constructor TZFreeTDSPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
end;

destructor TZFreeTDSPlainDriver.Destroy;
begin

  inherited Destroy;
end;

procedure TZFreeTDSPlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    @FreeTDSAPI.dbadata         := GetAddress('dbadata');
    @FreeTDSAPI.dbadlen         := GetAddress('dbadlen');
    @FreeTDSAPI.dbaltbind       := GetAddress('dbaltbind');
    @FreeTDSAPI.dbaltbind_ps    := GetAddress('dbaltbind_ps');
    @FreeTDSAPI.dbaltcolid      := GetAddress('dbaltcolid');
    @FreeTDSAPI.dbaltlen        := GetAddress('dbaltlen');
    @FreeTDSAPI.dbaltop         := GetAddress('dbaltop');
    @FreeTDSAPI.dbalttype       := GetAddress('dbalttype');
    @FreeTDSAPI.dbaltutype      := GetAddress('dbaltutype');
    @FreeTDSAPI.dbanullbind     := GetAddress('dbanullbind');
    @FreeTDSAPI.dbbind          := GetAddress('dbbind');
    @FreeTDSAPI.dbbind_ps       := GetAddress('dbbind_ps');
    @FreeTDSAPI.dbbufsize       := GetAddress('dbbufsize');
    @FreeTDSAPI.dbbylist        := GetAddress('dbbylist');
    @FreeTDSAPI.dbcancel        := GetAddress('dbcancel');
    @FreeTDSAPI.dbcanquery      := GetAddress('dbcanquery');
    @FreeTDSAPI.dbchange        := GetAddress('dbchange');
    @FreeTDSAPI.dbclose         := GetAddress('dbclose');
    @FreeTDSAPI.dbclrbuf        := GetAddress('dbclrbuf');
    @FreeTDSAPI.dbclropt        := GetAddress('dbclropt');
    @FreeTDSAPI.dbcmd           := GetAddress('dbcmd');
    @FreeTDSAPI.dbcmdrow        := GetAddress('dbcmdrow');
    @FreeTDSAPI.dbtablecolinfo  := GetAddress('dbtablecolinfo');
    @FreeTDSAPI.dbcolinfo       := GetAddress('dbcolinfo');
    @FreeTDSAPI.dbcollen        := GetAddress('dbcollen');
    @FreeTDSAPI.dbcolname       := GetAddress('dbcolname');
    @FreeTDSAPI.dbcolsource     := GetAddress('dbcolsource');
    @FreeTDSAPI.dbcoltype       := GetAddress('dbcoltype');
    @FreeTDSAPI.dbcolutype      := GetAddress('dbcolutype');
    @FreeTDSAPI.dbconvert       := GetAddress('dbcolutype');
    @FreeTDSAPI.dbconvert_ps    := GetAddress('dbconvert_ps');
    @FreeTDSAPI.dbiscount       := GetAddress('dbiscount');
    @FreeTDSAPI.dbcount         := GetAddress('dbcount');
    @FreeTDSAPI.dbcurcmd        := GetAddress('dbcurcmd');
    @FreeTDSAPI.dbcurrow        := GetAddress('dbcurrow');
    @FreeTDSAPI.dbdata          := GetAddress('dbdata');
    @FreeTDSAPI.dbdatecmp       := GetAddress('dbdatecmp');
    @FreeTDSAPI.dbdatecrack     := GetAddress('dbdatecrack');
    @FreeTDSAPI.dbdatlen        := GetAddress('dbdatlen');
    @FreeTDSAPI.dbdead          := GetAddress('dbdead');
    @FreeTDSAPI.dberrhandle     := GetAddress('dberrhandle');
    @FreeTDSAPI.dbexit          := GetAddress('dbexit');
    @FreeTDSAPI.dbfcmd          := GetAddress('dbfcmd');
    @FreeTDSAPI.dbfirstrow      := GetAddress('dbfirstrow');
    @FreeTDSAPI.dbfreebuf       := GetAddress('dbfreebuf');
    @FreeTDSAPI.dbgetchar       := GetAddress('dbgetchar');
    @FreeTDSAPI.dbgetcharset    := GetAddress('dbgetcharset');
    @FreeTDSAPI.dbgetlusername  := GetAddress('dbgetlusername');
    @FreeTDSAPI.dbgetmaxprocs   := GetAddress('dbgetmaxprocs');
    @FreeTDSAPI.dbgetnatlanf    := GetAddress('dbgetnatlanf');
    @FreeTDSAPI.dbgetpacket     := GetAddress('dbgetpacket');
    @FreeTDSAPI.dbgetrow        := GetAddress('dbgetrow');
    @FreeTDSAPI.dbgettime       := GetAddress('dbgettime');
    @FreeTDSAPI.dbgetuserdata   := GetAddress('dbgetuserdata');
    @FreeTDSAPI.dbhasretstat    := GetAddress('dbhasretstat');
    @FreeTDSAPI.dbinit          := GetAddress('dbinit');
    @FreeTDSAPI.dbiordesc       := GetAddress('dbiordesc');
    @FreeTDSAPI.dbiowdesc       := GetAddress('dbiowdesc');
    @FreeTDSAPI.dbisavail       := GetAddress('dbisavail');
    @FreeTDSAPI.dbisopt         := GetAddress('dbisopt');
    @FreeTDSAPI.dblastrow       := GetAddress('dblastrow');
    @FreeTDSAPI.dblogin         := GetAddress('dblogin');
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
    @FreeTDSAPI.dbmorecmds      := GetAddress('dbmorecmds');
    @FreeTDSAPI.dbmoretext      := GetAddress('dbmoretext');
    @FreeTDSAPI.dbmsghandle     := GetAddress('dbmsghandle');
    @FreeTDSAPI.dbname          := GetAddress('dbname');
    @FreeTDSAPI.dbnextrow       := GetAddress('dbnextrow');
    @FreeTDSAPI.dbnullbind      := GetAddress('dbnullbind');
    @FreeTDSAPI.dbnumalts       := GetAddress('dbnumalts');
    @FreeTDSAPI.dbnumcols       := GetAddress('dbnumcols');
    @FreeTDSAPI.dbnumcompute    := GetAddress('dbnumcompute');
    @FreeTDSAPI.dbnumorders     := GetAddress('dbnumorders');
    @FreeTDSAPI.dbnumrets       := GetAddress('dbnumrets');
    @FreeTDSAPI.tdsdbopen       := GetAddress('tdsdbopen');
    @FreeTDSAPI.dbopen          := GetAddress('dbopen');

    @FreeTDSAPI.dbprhead        := GetAddress('dbprhead');
    @FreeTDSAPI.dbprrow         := GetAddress('dbprrow');
    @FreeTDSAPI.dbprtype        := GetAddress('dbprtype');
    @FreeTDSAPI.DRBUF           := GetAddress('DRBUF');
    @FreeTDSAPI.dbreadtext      := GetAddress('dbreadtext');
    @FreeTDSAPI.dbrecftos       := GetAddress('dbrecftos');
    @FreeTDSAPI.dbresults       := GetAddress('dbresults');
    @FreeTDSAPI.dbresults_r     := GetAddress('dbresults_r');
    @FreeTDSAPI.dbretdata       := GetAddress('dbretdata');
    @FreeTDSAPI.dbretlen        := GetAddress('dbretlen');
    @FreeTDSAPI.dbretname       := GetAddress('dbretname');
    @FreeTDSAPI.dbretstatus     := GetAddress('dbretstatus');
    @FreeTDSAPI.dbrettype       := GetAddress('dbrettype');
    @FreeTDSAPI.dbrows          := GetAddress('dbrows');
    @FreeTDSAPI.dbrowtype       := GetAddress('dbrowtype');
    @FreeTDSAPI.dbrpcinit       := GetAddress('dbrpcinit');
    @FreeTDSAPI.dbrpcparam      := GetAddress('dbrpcparam');
    @FreeTDSAPI.dbrpcsend       := GetAddress('dbrpcsend');
    @FreeTDSAPI.dbsafestr       := GetAddress('dbsafestr');
    //@FreeTDSAPI.dbsechandle     := GetAddress('dbsechandle');
    @FreeTDSAPI.dbservcharset   := GetAddress('dbservcharset');
    @FreeTDSAPI.dbsetavail      := GetAddress('dbsetavail');
    //@FreeTDSAPI.dbsetbusy       := GetAddress('dbsetbusy');
    @FreeTDSAPI.dbsetdefcharset := GetAddress('dbsetdefcharset');
    @FreeTDSAPI.dbsetifile      := GetAddress('dbsetifile');
    //@FreeTDSAPI.dbsetinterrupt  := GetAddress('dbsetinterrupt');
    @FreeTDSAPI.dbsetlogintime  := GetAddress('dbsetlogintime');
    @FreeTDSAPI.dbsetmaxprocs   := GetAddress('dbsetmaxprocs');
    @FreeTDSAPI.dbsetlname      := GetAddress('dbsetlname');
    @FreeTDSAPI.dbsetnull       := GetAddress('dbsetnull');
    @FreeTDSAPI.dbsetopt        := GetAddress('dbsetopt');
    @FreeTDSAPI.dbsetrow        := GetAddress('dbsetrow');
    @FreeTDSAPI.dbsettime       := GetAddress('dbsettime');
    @FreeTDSAPI.dbsetuserdata   := GetAddress('dbsetuserdata');
    @FreeTDSAPI.dbsetversion    := GetAddress('dbsetversion');
    @FreeTDSAPI.dbspid          := GetAddress('dbspid');
    @FreeTDSAPI.dbspr1row       := GetAddress('dbspr1row');
    @FreeTDSAPI.dbspr1rowlen    := GetAddress('dbspr1rowlen');
    @FreeTDSAPI.dbsprhead       := GetAddress('dbsprhead');
    @FreeTDSAPI.dbsprline       := GetAddress('dbsprline');
    @FreeTDSAPI.dbsqlexec       := GetAddress('dbsqlexec');
    @FreeTDSAPI.dbsqlok         := GetAddress('dbsqlok');
    @FreeTDSAPI.dbsqlsend       := GetAddress('dbsqlsend');
    @FreeTDSAPI.dbstrcpy        := GetAddress('dbstrcpy');
    @FreeTDSAPI.dbstrlen        := GetAddress('dbstrlen');
    @FreeTDSAPI.dbvarylen       := GetAddress('dbvarylen');

    @FreeTDSAPI.dbtds           := GetAddress('dbtds');
    @FreeTDSAPI.dbtextsize      := GetAddress('dbtextsize');
    @FreeTDSAPI.dbtxptr         := GetAddress('dbtxptr');
    @FreeTDSAPI.dbtxtimestamp   := GetAddress('dbtxtimestamp');
    @FreeTDSAPI.dbtxtsnewval    := GetAddress('dbtxtsnewval');
    @FreeTDSAPI.dbtxtsput       := GetAddress('dbtxtsput');
    @FreeTDSAPI.dbuse           := GetAddress('dbuse');
    @FreeTDSAPI.dbwillconvert   := GetAddress('dbwillconvert');
    @FreeTDSAPI.dbwritetext     := GetAddress('dbwritetext');

    (* LOGINREC manipulation *)
    @FreeTDSAPI.dbsetlbool      := GetAddress('dbsetlbool');
    @FreeTDSAPI.dbsetllong      := GetAddress('dbsetllong');
    @FreeTDSAPI.dbsetlversion   := GetAddress('dbsetlversion');
  end;
end;

function TZFreeTDSPlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase+SQLServer';
end;

function TZFreeTDSPlainDriver.GetDescription: string;
begin
  Result := 'Native FreeTDS driver for Sybase and MMSQL Servers';
end;

end.
