{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain driver interface to DBLibrary         }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{          FreeTDS support by Michael Hiergeist           }
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

unit ZPlainFreeTDSDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZCompatibility, ZPlainDriver, ZPlainLoader,
  ZPlainFreeTDSConstants;

const
  WINDOWS_DLL_LOCATION = 'dblib.dll';
  LINUX_DLL_LOCATION = 'dblib.so';
  MAC_DLL_LOCATION = 'dblib.dynlib';

type
  {** Represents a generic interface to DBLIB native API. }
  IZFreeTDSPlainDriver = interface (IZPlainDriver)
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

  {** Implements a dblib driver for Sybase/MSSQL }
  TZFreeTDSPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZFreeTDSPlainDriver)
  private
    FreeTDSAPI: TFreeTDSAPI;
  protected
    procedure LoadApi; override;
  public
    constructor Create;
    destructor Destroy;

    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize(const Location: String); override;

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

implementation

uses SysUtils;

{ TZFreeTDSPlainDriver }

procedure TZFreeTDSPlainDriver.LoadCodePages;
begin
  AddCodePage('Not implemented!', -1);
   { TODO -oEgonHugeist : Must be completed!!!! }
end;

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
  LoadCodePages;
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

procedure TZFreeTDSPlainDriver.Initialize(const Location: String);
begin
  inherited Initialize(Location);
end;


procedure TZFreeTDSPlainDriver.CheckError;
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


function TZFreeTDSPlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbDead(dbProc);
end;

function TZFreeTDSPlainDriver.dbLogin: PLOGINREC;
begin
  Result := FreeTDSAPI.dbLogin;
end;

procedure TZFreeTDSPlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  FreeTDSAPI.dbLoginFree(Login);
end;

function TZFreeTDSPlainDriver.dbSetLoginTime(Seconds: DBINT): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlogintime(Seconds);
end;

function TZFreeTDSPlainDriver.dbsetlname(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, Value, Item);
end;

function TZFreeTDSPlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, HostName, DBSETHOST);
end;

function TZFreeTDSPlainDriver.dbsetluser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, UserName, DBSETUSER);
end;

function TZFreeTDSPlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, Password, DBSETPWD);
end;

function TZFreeTDSPlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, AppName, DBSETAPP);
end;

function TZFreeTDSPlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, NatLangName, DBSETNATLANG);
end;

function TZFreeTDSPlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlname(Login, CharsetName, DBSETCHARSET);
end;

function TZFreeTDSPlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := 0; //dbsetlname(Login, nil, DBSETSECURE);
end;

function TZFreeTDSPlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := FreeTDSAPI.dbsetmaxprocs(MaxProcs);
end;

function TZFreeTDSPlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  Result := FreeTDSAPI.dbOpen(Login, Host);
end;

function TZFreeTDSPlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbcancel(dbProc);
end;

function TZFreeTDSPlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbcmd(dbProc, Cmd);
end;

function TZFreeTDSPlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbSqlExec(dbProc);
end;

function TZFreeTDSPlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbResults(dbProc);
end;

function TZFreeTDSPlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbCanQuery(dbProc);
end;

function TZFreeTDSPlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbMoreCmds(dbProc);
end;

function TZFreeTDSPlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
begin
  Result := FreeTDSAPI.dbUse(dbProc, dbName);
end;

function TZFreeTDSPlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
begin
  Result := FreeTDSAPI.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZFreeTDSPlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbClose(dbProc);
end;

function TZFreeTDSPlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
  Result := FreeTDSAPI.dbName(dbProc);
end;

function TZFreeTDSPlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbCmdRow(dbProc);
end;

function TZFreeTDSPlainDriver.dbNumCols(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbNumCols(dbProc);
end;

function TZFreeTDSPlainDriver.dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
begin
  Result := FreeTDSAPI.dbColName(dbProc, Column);
end;

function TZFreeTDSPlainDriver.dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbColType(dbProc, Column);
end;

function TZFreeTDSPlainDriver.dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
begin
  Result := FreeTDSAPI.dbColLen(dbProc, Column);
end;

function TZFreeTDSPlainDriver.dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
begin
  Result := FreeTDSAPI.dbData(dbProc, Column);
end;

function TZFreeTDSPlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbDatLen(dbProc, Column);
end;

function TZFreeTDSPlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
  SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZFreeTDSPlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := FreeTDSAPI.dbNextRow(dbProc);
end;

function TZFreeTDSPlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
begin
  Result := FreeTDSAPI.dbGetRow(dbProc, Row);
end;

function TZFreeTDSPlainDriver.dbCount(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbCount(dbProc);
end;

function TZFreeTDSPlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcInit(dbProc, RpcName, Options);
end;

function TZFreeTDSPlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
  Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZFreeTDSPlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcSend(dbProc);
end;

function TZFreeTDSPlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := FreeTDSAPI.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := FreeTDSAPI.dbSqlOk(dbProc);
end;

function TZFreeTDSPlainDriver.dbRetStatus(dbProc: PDBPROCESS): DBINT;
begin
  Result := FreeTDSAPI.dbRetStatus(dbProc);
end;

function TZFreeTDSPlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := FreeTDSAPI.dbHasRetStat(dbProc) <> 0;
end;

function TZFreeTDSPlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
begin
  Result := FreeTDSAPI.dbRetName(dbProc, RetNum);
end;

function TZFreeTDSPlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
begin
  Result := FreeTDSAPI.dbRetData(dbProc, RetNum);
end;

function TZFreeTDSPlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbRetLen(dbProc, RetNum);
end;

function TZFreeTDSPlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
begin
  Result := FreeTDSAPI.dbRetType(dbProc, RetNum);
end;



end.
