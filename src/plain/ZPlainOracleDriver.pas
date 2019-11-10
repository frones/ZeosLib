{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for Oracle             }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainOracleDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ORACLE}
{$J+}

uses
  ZPlainLoader, ZCompatibility, ZPlainOracleConstants, ZPlainDriver;

{***************** Plain API types definition ****************}

const
  WINDOWS_DLL_LOCATION = 'oci.dll';
//  WINDOWS_DLL_LOCATION = 'ora803.dll';
  LINUX_DLL_LOCATION = 'libclntsh'+SharedSuffix;
//  LINUX_DLL_LOCATION = 'libwtc8.so';

type

  {** Represents a generic interface to Oracle native API. }
  IZOraclePlainDriver = interface (IZPlainDriver)
    ['{22404660-C95F-4346-A3DB-7C6DFE15F115}']
  end;

  {** Implements a driver for Oracle 9i }
  TZOraclePlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZOraclePlainDriver)
    {api definitions} //EH: if we would drop the interface overheap we simply
      //could move the defs up to public section, omit starting 'OCI'
      //and have native access without addition calls just like inline code
      //10mio calls loose = 1,5 sec with a fast i7 CPU
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
      htype: ub4; xtramem_sz: integer; usrmempp: Pointer): sword; cdecl;
    OCIDescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;

    OCILobOpen: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword; cdecl;
    OCILobRead: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobTrim: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: ub4): sword; cdecl;
    OCILobWrite: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobCreateTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
      cache: LongBool; duration: OCIDuration): sword; cdecl;
    OCILobIsTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
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
  //AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8);
  AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8, 'AL32UTF8', 4);
  AddCodePage('AL32UTF8', 873, ceUTF8, zCP_UTF8, '', 4);
  AddCodePage('UTF16', 1000, ceUTF16, zCP_UTF16, '', 2);
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
    @OCIDescriptorFree            := GetAddress('OCIDescriptorFree');
    {Lob}
    @OCILobOpen                   := GetAddress('OCILobOpen');
    @OCILobRead                   := GetAddress('OCILobRead');
    @OCILobTrim                   := GetAddress('OCILobTrim');
    @OCILobWrite                  := GetAddress('OCILobWrite');

    @OCILobCreateTemporary        := GetAddress('OCILobCreateTemporary');
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