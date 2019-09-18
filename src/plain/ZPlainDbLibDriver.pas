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

uses Classes, ZCompatibility, ZPlainDriver, ZPlainDbLibConstants,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF}
  {$IFDEF TLIST_IS_DEPRECATED},ZSysUtils{$ENDIF};

const
  NTWDBLIB_DLL_LOCATION ='ntwdblib.dll';
  LIBSYBDB_WINDOWS_DLL_LOCATION = 'libsybdb.dll';
  LIBSYBDB_LINUX_DLL_LOCATION = 'libsybdb.so';
  FREETDS_MSSQL_WINDOWS_DLL_LOCATION = 'sybdb.dll';
  FREETDS_LINUX_DLL_LOCATION = 'dblib.so';
  FREETDS_OSX_DLL_LOCATION = 'dblib.dylib';
  FREETDS_SYBASE_WINDOWS_DLL_LOCATION = 'sybdb.dll';
type
  {** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface (IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']

    procedure CheckError(dbProc: PDBPROCESS); deprecated;
    function GetErrorString(dbProc: PDBPROCESS): String;

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
    function dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
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
    function dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcoltypeinfo(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbcolinfo(pdbhandle :PDBHANDLE; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
      SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT;
    function dbbind(Proc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;

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
    { BCP functions }
    function bcp_batch(const Proc: PDBPROCESS): DBINT;
    function bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
    function bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
      TableColumn: Integer): RETCODE;
    function bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
    function bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
    function bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
    function bcp_control(Proc: PDBPROCESS; Field: Integer; Value: DBINT): RETCODE;
    function bcp_done(Proc: PDBPROCESS): DBINT;
    function bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
    function bcp_init(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE;
    function bcp_moretext(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE;
    function bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
    function bcp_sendrow(Proc: PDBPROCESS): RETCODE;
    function bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
    function bcp_writefmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
  end;

  TZDBLibAbstractPlainDriver = class(TZAbstractPlainDriver, IZPlainDriver)
  protected
    DBVariables: TDBVariables;
  public
    function dbDead(dbProc: PDBPROCESS): Boolean; virtual; abstract;
    procedure dbLoginFree(Login: PLOGINREC); virtual; abstract;
    constructor Create; virtual;
    procedure CheckError(dbProc: PDBPROCESS);
    function GetErrorString(dbProc: PDBPROCESS): String;
    function GetVariables: TDBVariables;
  end;

  TZDbLibBasePlainDriver = class(TZDBLibAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  protected
    DBLibAPI: TDBLibAPI;
  public
    procedure LoadApi; override;
    function dbLogin: PLOGINREC; virtual;
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
    function dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
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
    function dbbind(Proc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;

    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcoltypeinfo(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbColInfo(pdbhandle :PDBHANDLE; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
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
    { BCP functions }
    function bcp_batch(const Proc: PDBPROCESS): DBINT;
    function bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
    function bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
      TableColumn: Integer): RETCODE;
    function bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
    function bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
    function bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
    function bcp_control(Proc: PDBPROCESS; Field: Integer; Value: DBINT): RETCODE;
    function bcp_done(Proc: PDBPROCESS): DBINT;
    function bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
    function bcp_init(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE;
    function bcp_moretext(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE;
    function bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
    function bcp_sendrow(Proc: PDBPROCESS): RETCODE;
    function bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
    function bcp_writefmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
  end;

  {** Implements a dblib driver for Sybase ASE 12.5 }
  TZDBLibSybaseASE125PlainDriver = class (TZDBLibAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  private
    SybaseAPI: TSybaseAPI;
  protected
    procedure LoadApi; override;
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    function dbDead(dbProc: PDBPROCESS): Boolean; override;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC); override;
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
    function dbSetLSecure({%H-}Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
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
    function dbbind(Proc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;

    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcoltypeinfo(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbColInfo(pdbhandle :PDBHANDLE; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
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
    function dbrbuf({%H-}Proc: PDBPROCESS): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool;
    { BCP functions }
    function bcp_batch(const Proc: PDBPROCESS): DBINT;
    function bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
    function bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
      TableColumn: Integer): RETCODE;
    function bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
    function bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
    function bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
    function bcp_control(Proc: PDBPROCESS; Field: Integer; Value: DBINT): RETCODE;
    function bcp_done(Proc: PDBPROCESS): DBINT;
    function bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
    function bcp_init(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE;
    function bcp_moretext(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE;
    function bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
    function bcp_sendrow(Proc: PDBPROCESS): RETCODE;
    function bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
    function bcp_writefmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
  end;

  {** Implements a dblib driver for MSSql7 }
  TZDBLibMSSQL7PlainDriver = class (TZDbLibBasePlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  private
    MsSQLAPI: TMsSQLAPI;
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    procedure LoadApi; override;
    constructor Create; override;
    destructor Destroy; override;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    function dbDead(dbProc: PDBPROCESS): Boolean; override;
    procedure dbLoginFree(Login: PLOGINREC); override;
    function dbSetLCharSet({%H-}Login: PLOGINREC; {%H-}CharsetName: PAnsiChar): RETCODE; override;
    function dbSetLSecure(Login: PLOGINREC): RETCODE; override;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE; override;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT; Char_Param: PAnsiChar = nil; {%H-}Int_Param: DBINT = -1): RETCODE; override;
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
    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;

  end;

  {** Implements a dblib driver for Sybase/MSSQL }
  TZFreeTDSBasePlainDriver = class (TZDbLibBasePlainDriver,
    IZDBLibPlainDriver, IZFreeTDSPlainDriver)
  private
    FreeTDSAPI: TFreeTDSAPI;
  protected
    function Clone: IZPlainDriver; override; abstract;
    procedure LoadCodePages; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadApi; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    {API functions}
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; override;
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
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer; override;
    function dbCount(dbProc: PDBPROCESS): Integer; override;
    function dbcolbrowse(Proc: PDBPROCESS; Column: Integer): LongBool; override;

    function dbHasRetStat(dbProc: PDBPROCESS): Boolean; override;

    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;
    function dbdataready(Proc: PDBPROCESS): LongBool; override;
    procedure dbfreelogin(Login: PLOGINREC);
  end;


  TZFreeTDS42MsSQLPlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS42SybasePlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion({%H-}Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS50PlainDriver = class(TZFreeTDS42SybasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion({%H-}Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS70PlainDriver = class(TZFreeTDS42MsSQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS71PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS72PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
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
  ErrorCS: TCriticalSection;
  SQLErrors: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
  SQLMessages: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};

{$ENDIF ZEOS_DISABLE_DBLIB}

implementation

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses SysUtils, ZPlainLoader, ZEncoding, ZClasses, ZFastCode;

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
function SybaseErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer;
{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := Proc;
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
function SybaseMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := Proc;
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
function DbLibErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := Proc;
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
function DbLibMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := Proc;
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

constructor TZDBLibAbstractPlainDriver.Create;
var I: Integer;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  for i := 0 to high(DBVariables.DBoptions) do DBVariables.DBoptions[i] := -1;
  for i := 0 to high(DBVariables.DBSetLoginRec) do DBVariables.DBSetLoginRec[i] := -1;
end;

procedure TZDBLibAbstractPlainDriver.CheckError(dbProc: Pointer);
var S: String;
begin
  S := GetErrorString(dbProc);
  if S <> '' then
    raise EZSQLException.Create(S);
end;

function TZDBLibAbstractPlainDriver.GetErrorString(
  dbProc: PDBPROCESS): String;
var
  I: Integer;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;

    procedure AddToErrorMsg(const AError: String);
    begin
      if Result <> EmptyRaw then
        Result := Result + LineEnding;
      Result := Result + AError;
    end;

begin
  ErrorCS.Enter;
  Result := '';
  try
  if ((SQLErrors = nil) or (SQLErrors.Count = 0)) and
     ((SQLMessages = nil) or (SQLMessages.Count = 0)) then
    Exit;
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
      if lMesageEntry^.Severity > EXINFO then begin
        if lMesageEntry^.MsgNo <> 5701
        then AddToErrorMsg(String(lMesageEntry^.MsgText));
      end;
      Dispose(lMesageEntry);
      SQLMessages.Delete(I);
    end
    else
      Inc(I);
  end;
  finally
    ErrorCS.Leave;
  end;
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
    @DBLibAPI.dbcolinfo             := GetAddress('dbcolinfo');
    @DBLibAPI.dbcolname             := GetAddress('dbcolname');
    @DBLibAPI.dbcolsource           := GetAddress('dbcolsource');
    @DBLibAPI.dbcoltype             := GetAddress('dbcoltype');
    @DBLibAPI.dbcoltypeinfo         := GetAddress('dbcoltypeinfo');
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

function TZDBLibBasePlainDriver.dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
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
  lStartMs: Integer;
begin
  Result := DBLibAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartMs := 0;
    repeat
      Sleep(1);
      Inc(lStartMs);
    until (lStartMs = TIMEOUT_MAXIMUM) or (dbdataready(dbProc) = TRUE);
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

function TZDBLibBasePlainDriver.dbbind(Proc: PDBPROCESS;
  Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;
begin
  Result := DBLibAPI.dbbind(Proc, Column, VarType, VarLen, VarAddr);
end;

function TZDBLibBasePlainDriver.dbColName(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
begin
  Result := DBLibAPI.dbColName(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbColSource(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
begin
  if Assigned(DBLibAPI.dbColSource)
  then Result := DBLibAPI.dbColSource(dbProc, Column)
  else Result := nil;
end;

function TZDBLibBasePlainDriver.dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := DBLibAPI.dbColType(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbcoltypeinfo(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
begin
  if Assigned(DBLibAPI.dbcoltypeinfo) then
    Result := DBLibAPI.dbcoltypeinfo(dbProc, Column)
  else
    Result := nil
end;

function TZDBLibBasePlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
begin
  Result := DBLibAPI.dbColLen(dbProc, Column);
end;

function TZDBLibBasePlainDriver.dbColInfo(pdbhandle: PDBHANDLE; _Type: Integer;
  Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
begin
  Result := DBLIBAPI.dbColInfo(pdbhandle, _Type, Column, ComputeId, lpdbcol);
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
  if Assigned(DBLibAPI.dbRetData) then
    Result := DBLibAPI.dbRetData(dbProc, RetNum)
  else
    Result := nil;
end;

function TZDBLibBasePlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := DBLibAPI.dbRetLen(dbProc, RetNum);
end;

function TZDBLibBasePlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  if Assigned(DBLibAPI.dbRetType) then
    Result := DBLibAPI.dbRetType(dbProc, RetNum)
  else
    Result := Ord(tdsVoid);
end;

function TZDBLibBasePlainDriver.dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := DBINT(dbdataready(Proc));
end;

{ BCP functions }
function TZDBLibBasePlainDriver.bcp_batch(const Proc: PDBPROCESS): DBINT;
begin
  Result := DBLibAPI.bcp_batch(Proc);
end;

function TZDBLibBasePlainDriver.bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
  VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_bind(Proc, VarAddr, PrefixLen, VarLen, Terminator,
    TermLen, Typ, TableColumn);
end;

function TZDBLibBasePlainDriver.bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
  FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
  TableColumn: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_colfmt(Proc, FileColumn, FileType, FilePrefixLen,
    FileColLen, FileTerm, FileTermLen, TableColumn);
end;

function TZDBLibBasePlainDriver.bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_collen(Proc, VarLen, TableColumn);
end;

function TZDBLibBasePlainDriver.bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_colptr(Proc, ColPtr, TableColumn);
end;

function TZDBLibBasePlainDriver.bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_columns(Proc, FileColCount);
end;

function TZDBLibBasePlainDriver.bcp_control(Proc: PDBPROCESS; Field: Integer;
  Value: DBINT): RETCODE;
begin
  Result := DBLibAPI.bcp_control(Proc, Field, Value);
end;

function TZDBLibBasePlainDriver.bcp_done(Proc: PDBPROCESS): DBINT;
begin
  Result := DBLibAPI.bcp_done(Proc);
end;

function TZDBLibBasePlainDriver.bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
begin
  Result := DBLibAPI.bcp_exec(Proc, RowsCopied);
end;

function TZDBLibBasePlainDriver.bcp_init(Proc: PDBPROCESS; TableName, hFile,
  ErrFile: PAnsiChar; Direction: Integer): RETCODE;
begin
  Result := DBLibAPI.bcp_init(Proc, TableName, hFile, ErrFile, Direction);
end;

function TZDBLibBasePlainDriver.bcp_moretext(Proc: PDBPROCESS; Size: DBINT;
  Text: PByte): RETCODE;
begin
  Result := DBLibAPI.bcp_moretext(Proc, Size, Text);
end;

function TZDBLibBasePlainDriver.bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.bcp_readfmt(Proc, FileName);
end;

function TZDBLibBasePlainDriver.bcp_sendrow(Proc: PDBPROCESS): RETCODE;
begin
  Result := DBLibAPI.bcp_sendrow(Proc);
end;

function TZDBLibBasePlainDriver.bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
begin
  Result := DBLibAPI.bcp_setl(Login, Enable);
end;

function TZDBLibBasePlainDriver.bcp_writefmt(Proc: PDBPROCESS;
  FileName: PAnsiChar): RETCODE;
begin
  Result := DBLibAPI.bcp_writefmt(Proc, FileName);
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
    {bcp function}
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
    @SybaseAPI.bcp_setl              := GetAddress('bcp_setl');
    @SybaseAPI.bcp_writefmt          := GetAddress('bcp_writefmt');
    { core function }
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
    @SybaseAPI.dbfcmd                := GetAddress('dbfcmd');
    @SybaseAPI.dbcmdrow              := GetAddress('DBCMDROW'); //Syb use uppercase name!!
    if not Assigned(SybaseAPI.dbcmdrow) then //test if renamed
      @SybaseAPI.dbcmdrow            := GetAddress('dbcmdrow');
    @SybaseAPI.dbcolbrowse           := GetAddress('dbcolbrowse');
    @SybaseAPI.dbcollen              := GetAddress('dbcollen');
    @SybaseAPI.dbcolinfo             := GetAddress('dbcolinfo');
    @SybaseAPI.dbcolname             := GetAddress('dbcolname');
    @SybaseAPI.dbcolsource           := GetAddress('dbcolsource');
    @SybaseAPI.dbcoltypeinfo         := GetAddress('dbcoltypeinfo');
    @SybaseAPI.dbcoltype             := GetAddress('dbcoltype');
    @SybaseAPI.dbcolutype            := GetAddress('dbcolutype');
    @SybaseAPI.dbconvert             := GetAddress('dbconvert');
    @SybaseAPI.dbcount               := GetAddress('DBCOUNT'); //Syb use uppercase name!!
    if not Assigned(SybaseAPI.dbcount) then //test if renamed
      @SybaseAPI.dbcount            := GetAddress('dbcount');
    @SybaseAPI.dbcurcmd              := GetAddress('DBCURCMD'); //Syb use uppercase name!!
    if not Assigned(SybaseAPI.dbcurcmd) then //test if renamed
      @SybaseAPI.dbcurcmd            := GetAddress('dbcurcmd');
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
    @SybaseAPI.dbdead                := GetAddress('DBDEAD');//Syb use uppercase name!!
    if not Assigned(SybaseAPI.dbdead) then //test if renamed
      @SybaseAPI.dbdead              := GetAddress('dbdead');
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
  LoadCodePages;
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
  AddSybaseCodePages(Self);
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

function TZDBLibSybaseASE125PlainDriver.dbbind(Proc: PDBPROCESS;
  Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;
begin
  Result := SybaseAPI.dbbind(Proc, Column, VarType, VarLen, VarAddr);
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

function TZDBLibSybaseASE125PlainDriver.dbCmd(const dbProc: PDBPROCESS;
  const Cmd: PAnsiChar): RETCODE;
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
  lStartMs : Cardinal;
begin
  Result := SybaseAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartMs := 0;
    repeat
      Sleep(1);
      Inc(lStartMs);
    until (lStartMs = TIMEOUT_MAXIMUM) or (dbdataready(dbProc) = TRUE);
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

function TZDBLibSybaseASE125PlainDriver.dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
begin
  if Assigned(SybaseAPI.dbColSource)
  then Result := SybaseAPI.dbColSource(dbProc, Column)
  else Result := nil;
end;

function TZDBLibSybaseASE125PlainDriver.dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
begin
  Result := SybaseAPI.dbColType(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbcoltypeinfo(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
begin
  if Assigned(SybaseAPI.dbcoltypeinfo) then
    Result := SybaseAPI.dbcoltypeinfo(dbProc, Column)
  else
    Result := nil;
end;

function TZDBLibSybaseASE125PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
begin
  Result := SybaseAPI.dbColLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColInfo(pdbhandle :PDBHANDLE; _Type:
  Integer; Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
begin
  Result := SybaseAPI.dbColInfo(pdbhandle, _Type, Column, ComputeId, lpdbcol);
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

{ BCP functions }
function TZDBLibSybaseASE125PlainDriver.bcp_batch(const Proc: PDBPROCESS): DBINT;
begin
  Result := SybaseAPI.bcp_batch(Proc);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
  VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_bind(Proc, VarAddr, PrefixLen, VarLen, Terminator,
    TermLen, Typ, TableColumn);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
  FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
  TableColumn: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_colfmt(Proc, FileColumn, FileType, FilePrefixLen,
    FileColLen, FileTerm, FileTermLen, TableColumn);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_collen(Proc, VarLen, TableColumn);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_colptr(Proc, ColPtr, TableColumn);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_columns(Proc, FileColCount);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_control(Proc: PDBPROCESS; Field: Integer;
  Value: DBINT): RETCODE;
begin
  Result := SybaseAPI.bcp_control(Proc, Field, Value);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_done(Proc: PDBPROCESS): DBINT;
begin
  Result := SybaseAPI.bcp_done(Proc);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
begin
  Result := SybaseAPI.bcp_exec(Proc, RowsCopied);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_init(Proc: PDBPROCESS; TableName, hFile,
  ErrFile: PAnsiChar; Direction: Integer): RETCODE;
begin
  Result := SybaseAPI.bcp_init(Proc, TableName, hFile, ErrFile, Direction);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_moretext(Proc: PDBPROCESS; Size: DBINT;
  Text: PByte): RETCODE;
begin
  Result := SybaseAPI.bcp_moretext(Proc, Size, Text);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.bcp_readfmt(Proc, FileName);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_sendrow(Proc: PDBPROCESS): RETCODE;
begin
  Result := SybaseAPI.bcp_sendrow(Proc);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
begin
  Result := SybaseAPI.bcp_setl(Login, Enable);
end;

function TZDBLibSybaseASE125PlainDriver.bcp_writefmt(Proc: PDBPROCESS;
  FileName: PAnsiChar): RETCODE;
begin
  Result := SybaseAPI.bcp_writefmt(Proc, FileName);
end;

{TZDBLibMSSQL7PlainDriver}

procedure TZDBLibMSSQL7PlainDriver.LoadApi;
begin
  inherited LoadAPI;
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    if GetAddress('dbcoltypeinfo') <> nil then
      raise Exception.Create('Wrong library bound. Propably a FreeTDS-Lib.'+LineEnding+
         'Use the FreeTDS protocols instead!');
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
  LoadCodePages;
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
  AddmMSCodePages(Self);
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
  lStartMs : Cardinal;
begin
  Result := DBLibAPI.dbsqlsend(dbProc);
  if Result = SUCCEED then begin
    lStartMs := 0;
    repeat
      Sleep(1);
      Inc(lStartMs);
    until (lStartMs = TIMEOUT_MAXIMUM) or (MsSQLAPI.dbdataready(dbProc) = TRUE);
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
    @FreeTDSAPI.tdsdump_open    := GetAddress('tdsdump_open');
    @FreeTDSAPI.tdsdump_on      := GetAddress('tdsdump_on');
    @FreeTDSAPI.tdsdump_off     := GetAddress('tdsdump_off');
    @FreeTDSAPI.tdsdump_close   := GetAddress('tdsdump_close');

  end;
  FreeTDSAPI.dbinit;

  OldFreeTDSErrorHandle := DBLibAPI.dberrhandle(DbLibErrorHandle);
  OldFreeTDSMessageHandle := DBLibAPI.dbmsghandle(DbLibMessageHandle);
end;

constructor TZFreeTDSBasePlainDriver.Create;
begin
  inherited create;

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

function TZFreeTDSBasePlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  DBLibAPI.dbsetlogintime(10);
  Result := FreeTDSAPI.tdsdbopen(Login, Host, 0);
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
  if Assigned(Result)  then
    if not (dbsetlversion(Result) = DBSUCCEED ) then
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
  {$IFDEF UNICODE}
  FreeTDSAPI.tdsdump_open(PAnsiChar(ZUnicodeToRaw(FileName,ZOSCodePage)));
  {$ELSE}
  FreeTDSAPI.tdsdump_open(Pointer(FileName));
  {$ENDIF}
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
  if Assigned(FreeTDSAPI.dbHasRetStat) then
    Result := FreeTDSAPI.dbHasRetStat(dbProc) <> 0
  else
    Result := False;
end;

procedure TZFreeTDSBasePlainDriver.LoadCodePages;
begin
  AddCodePage('UTF-8', 1, ceUTF8, zCP_UTF8,  '', 4, True);
  AddCodePage('ISO-8859-1', 2, ceAnsi, zCP_L1_ISO_8859_1, '', 1, False);
  AddCodePage('ASCII', 3, ceAnsi, zCP_us_ascii, '', 1, False);
end;

{ TZFreeTDS42MsSQLPlainDriver }
function TZFreeTDS42MsSQLPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS42MsSQLPlainDriver.Create;
end;

procedure TZFreeTDS42MsSQLPlainDriver.LoadCodePages;
begin
  AddmMSCodePages(Self);
  inherited;
end;

constructor TZFreeTDS42MsSQLPlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(FREETDS_MSSQL_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    FLoader.AddLocation(FREETDS_LINUX_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(FREETDS_OSX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}
  LoadCodePages;
end;

function TZFreeTDS42MsSQLPlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL<=6.5';
end;

function TZFreeTDS42MsSQLPlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 4.2 protocol for MsSQL <=6.5 Servers';
end;

function TZFreeTDS42MsSQLPlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, DBVERSION_42);
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

constructor TZFreeTDS42SybasePlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(FREETDS_SYBASE_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    FLoader.AddLocation(FREETDS_LINUX_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(FREETDS_OSX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFreeTDS42SybasePlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase<10';
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

constructor TZFreeTDS50PlainDriver.Create;
begin
  inherited Create;
  LoadCodePages;
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
  Result := FreeTDSAPI.dbsetversion(TDSDBVERSION_100);
end;

function TZFreeTDS50PlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, DBVERSION_100);
end;

{ TZFreeTDS70PlainDriver }

function TZFreeTDS70PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS70PlainDriver.Create;
end;

function TZFreeTDS70PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL-7.0';
end;

function TZFreeTDS70PlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := FreeTDSAPI.dbsetlversion(Login, DBVERSION_70);
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
  SQLErrors := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  SQLMessages := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  ErrorCS := TCriticalSection.Create;
finalization
  FreeAndnil(ErrorCS);
//Free any record in the list if any
  while SQLErrors.Count > 0 do
  begin
    Dispose(PDBLibError(SQLErrors.Items[0]));
    SQLErrors.Delete(0);
  end;
  if SQLErrors <> nil then
    FreeAndNil(SQLErrors);

//Free any record in the list if any
  while SQLMessages.Count > 0 do
  begin
    Dispose(PDBLibMessage(SQLMessages.Items[0]));
    SQLMessages.Delete(0);
  end;
  if SQLMessages <> nil then
    FreeAndNil(SQLMessages);
{$ENDIF ZEOS_DISABLE_DBLIB}

end.
