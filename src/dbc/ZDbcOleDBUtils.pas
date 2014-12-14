{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcOleDBUtils;

interface

{$I ZDbc.inc}

{$IF defined(ENABLE_ADO) or defined(ENABLE_OLEDB)}
uses
  Types, SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZCompatibility, ZDbcIntfs, ZOleDB, ZVariant, ZDbcStatement;

type
  TInterfacesDynArray = array of TInterfaceDynArray;

  /// binding status of a given column
  // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms720969
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TOleDBBindStatus = (DBBINDSTATUS_OK, DBBINDSTATUS_BADORDINAL,
    DBBINDSTATUS_UNSUPPORTEDCONVERSION, DBBINDSTATUS_BADBINDINFO,
    DBBINDSTATUS_BADSTORAGEFLAGS, DBBINDSTATUS_NOINTERFACE,
    DBBINDSTATUS_MULTIPLESTORAGE);

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  CtrlsCPType: TZControlsCodePage; IsLong: Boolean): TZSQLType; overload;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  CtrlsCPType: TZControlsCodePage; SrcRS: IZResultSet): TZSQLType; overload;

procedure OleDBCheck(aResult: HRESULT; const aStatus: TDBBINDSTATUSDynArray = nil);

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO): DBROWOFFSET;

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; ClientVarManager: IZClientVariantManager);

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray);

{$IFEND}
implementation
{$IF defined(ENABLE_ADO) or defined(ENABLE_OLEDB)}
uses
  {$ifdef WITH_SYSTEM_PREFIX}System.Win.ComObj,{$else}ComObj,{$endif}
  ActiveX, Windows, Math, TypInfo,
  ZEncoding, ZDbcLogging, ZDbcUtils, ZDbcResultSet, ZFastCode, ZSysUtils, ZMessages;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  CtrlsCPType: TZControlsCodePage; IsLong: Boolean): TZSQLType;
begin
  case OleDBType of
    DBTYPE_EMPTY:     Result := stUnknown;
    DBTYPE_NULL:      Result := stUnknown;
    DBTYPE_I2:        Result := stSmall;
    DBTYPE_I4:        Result := stInteger;
    DBTYPE_R4:        Result := stFloat;
    DBTYPE_R8:        Result := stDouble;
    DBTYPE_CY:        Result := stCurrency;
    DBTYPE_DATE:      Result := stTimeStamp;
    DBTYPE_BSTR:
      if IsLong then
        Result := stAsciiStream
      else
        Result := stString;
    DBTYPE_ERROR:     Result := stInteger;
    DBTYPE_BOOL:      Result := stBoolean;
    DBTYPE_VARIANT:   Result := stString;
    DBTYPE_IUNKNOWN:  Result := stUnknown; //note this could be used to bind IStream for reading/writing data
    DBTYPE_DECIMAL:   Result := stBigDecimal;
    DBTYPE_UI1:       Result := stByte;
    DBTYPE_I1:        Result := stShort;
    DBTYPE_UI2:       Result := stWord;
    DBTYPE_UI4:       Result := stLongWord;
    DBTYPE_I8:        Result := stLong;
    DBTYPE_UI8:       Result := stULong;
    DBTYPE_GUID:      Result := stGUID;
    DBTYPE_BYTES:     if IsLong then Result := stBinaryStream
                      else Result := stBytes;
    DBTYPE_STR:       if IsLong then Result := stAsciiStream
                      else Result := stString;
    DBTYPE_WSTR:      if IsLong then Result := stAsciiStream
                      else Result := stString;
    DBTYPE_NUMERIC:     Result := stDouble;
    DBTYPE_UDT:         Result := stUnknown;
    DBTYPE_DBDATE:      Result := stDate;
    DBTYPE_DBTIME:      Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_VARNUMERIC:  Result := stDouble;
    else //makes compiler happy
      {
      DBTYPE_IDISPATCH:
      DBTYPE_HCHAPTER:    }Result := stUnknown;
  end;
  if (Result = stString) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeString;
  if (Result = stAsciiStream) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeStream;
end;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  CtrlsCPType: TZControlsCodePage; SrcRS: IZResultSet): TZSQLType; overload;
const LongNames: array [0..8] of ZWideString = ('TEXT', 'NTEXT', 'MEDIUMTEXT',
  'LONGTEXT', 'CLOB', 'BLOB', 'MEDIUMBLOB', 'LONGBLOB', 'IMAGE');
function IsLong: Boolean;
var I: Integer;
  Uni: ZWideString;
begin
  Uni := UpperCase(SrcRS.GetUnicodeStringByName('TYPE_NAME'));
  Result := False;
  for i := 0 to high(LongNames) do
    if CompareMem(Pointer(LongNames[i]), Pointer(Uni), Length(Uni) shl 1) then
    begin
      Result := True;
      Break;
    end;
end;
begin
  case OleDBType of
    DBTYPE_EMPTY:     Result := stUnknown;
    DBTYPE_NULL:      Result := stUnknown;
    DBTYPE_I2:        Result := stSmall;
    DBTYPE_I4:        Result := stInteger;
    DBTYPE_R4:        Result := stFloat;
    DBTYPE_R8:        Result := stDouble;
    DBTYPE_CY:        Result := stCurrency;
    DBTYPE_DATE:      Result := stTimeStamp;
    DBTYPE_BSTR:
      if IsLong then
        Result := stAsciiStream
      else
        Result := stString;
    DBTYPE_ERROR:     Result := stInteger;
    DBTYPE_BOOL:      Result := stBoolean;
    DBTYPE_VARIANT:   Result := stString;
    DBTYPE_IUNKNOWN:  Result := stUnknown; //note this could be used to bind IStream for reading/writing data
    DBTYPE_DECIMAL:   Result := stBigDecimal;
    DBTYPE_UI1:       Result := stByte;
    DBTYPE_I1:        Result := stShort;
    DBTYPE_UI2:       Result := stWord;
    DBTYPE_UI4:       Result := stLongWord;
    DBTYPE_I8:        Result := stLong;
    DBTYPE_UI8:       Result := stULong;
    DBTYPE_GUID:      Result := stGUID;
    DBTYPE_BYTES:     if IsLong then Result := stBinaryStream
                      else Result := stBytes;
    DBTYPE_STR:       if IsLong then Result := stAsciiStream
                      else Result := stString;
    DBTYPE_WSTR:      if IsLong then Result := stAsciiStream
                      else Result := stString;
    DBTYPE_NUMERIC:     Result := stDouble;
    DBTYPE_UDT:         Result := stUnknown;
    DBTYPE_DBDATE:      Result := stDate;
    DBTYPE_DBTIME:      Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_VARNUMERIC:  Result := stDouble;
    else //makes compiler happy
      {
      DBTYPE_IDISPATCH:
      DBTYPE_HCHAPTER:    }Result := stUnknown;
  end;
  if (Result = stString) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeString;
  if (Result = stAsciiStream) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeStream;
end;

procedure OleDBCheck(aResult: HRESULT; const aStatus: TDBBINDSTATUSDynArray = nil);
{ thanks to AB from Synopse project! }
var
  OleDBErrorMessage: String;
  ErrorInfo, ErrorInfoDetails: IErrorInfo;
  ErrorRecords: IErrorRecords;
  i: Integer;
  ErrorCount: ULONG;
  Desc: WideString;
  s: string;
begin
  OleDBErrorMessage := '';
  if not Succeeded(aResult) then
  begin // get OleDB specific error information
    GetErrorInfo(0,ErrorInfo);
    if Assigned(ErrorInfo) then
    begin
      ErrorRecords := ErrorInfo as IErrorRecords;
      ErrorRecords.GetRecordCount(ErrorCount);
      for i := 0 to ErrorCount-1 do
      begin
        // retrieve generic error info
        OleCheck(ErrorRecords.GetErrorInfo(i,GetSystemDefaultLCID,ErrorInfoDetails));
        OleCheck(ErrorInfoDetails.GetDescription(Desc));
        if OleDBErrorMessage<>'' then
          OleDBErrorMessage := OleDBErrorMessage+'  ';
        OleCheck(SetErrorInfo(0, ErrorInfoDetails));
        OleDBErrorMessage := OleDBErrorMessage+String(Desc);
        Desc := '';
        ErrorInfoDetails := nil;
        OleDBErrorMessage := 'ERROR';
      end;
    end;
    ErrorRecords := nil;
    //OleCheck(SetErrorInfo(0,  ErrorInfo));
    ErrorInfo := nil;
    // get generic HRESULT error
    if not Succeeded(aResult) or (OleDBErrorMessage<>'') then begin
      s := SysErrorMessage(aResult);
      if s='' then
        s := 'OLEDB Error '+IntToHex(aResult,8);
      OleDBErrorMessage := s+' - '+OleDBErrorMessage;
    end;
    if OleDBErrorMessage='' then
      exit;
    // retrieve binding information from Status[]
    s := '';
    for i := 0 to high(aStatus) do
      if aStatus[i]<>ZOleDB.DBBINDSTATUS_OK then
        if aStatus[i]<=cardinal(high(TOleDBBindStatus)) then
          s := Format('%s Status[%d]="%s"',[s,i,GetEnumName(TypeInfo(TOleDBBindStatus),aStatus[i])])
        else
          s := Format('%s Status[%d]=%d',[s,i,aStatus[i]]);
    if s<>'' then
      OleDBErrorMessage := OleDBErrorMessage+s;
    // raise exception
    DriverManager.LogMessage(lcExecute, 'OleDB', RawByteString(OleDBErrorMessage));
    raise EZSQLException.Create(OleDBErrorMessage);
  end;
end;

function MapOleTypesToZeos(DBType: DBTYPEENUM): DBTYPE;
begin
  //ole type mappings:
  //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
  { we only map types to Zeos simple types here}
  Result := DBType;
  case DBType of
    { all commented enums are types i've no idea about. All droped are supported as is }
    DBTYPE_BSTR: Result := DBTYPE_WSTR;
    //DBTYPE_IDISPATCH	= 9;
    //DBTYPE_ERROR: 	= 10;
    //DBTYPE_VARIANT	= 12;
    //DBTYPE_IUNKNOWN	= 13;
    DBTYPE_DECIMAL: Result := DBTYPE_R8;
    DBTYPE_STR: Result := DBTYPE_WSTR;  //if we would know the server-codepage ... we could decrease mem
    DBTYPE_NUMERIC: Result := DBTYPE_R8;
    //DBTYPE_UDT	= 132;
    DBTYPE_DBDATE: Result := DBTYPE_DATE;
    DBTYPE_DBTIME: Result := DBTYPE_DATE;
    DBTYPE_DBTIMESTAMP: Result := DBTYPE_DATE;
    //DBTYPE_HCHAPTER	= 136;
    DBTYPE_FILETIME: Result := DBTYPE_DATE;
    //DBTYPE_PROPVARIANT	= 138;
    DBTYPE_VARNUMERIC: Result := DBTYPE_R8;
  end;
end;

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;
var
  I: Integer;
  LobBufCount: Integer;
  Procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := ParamInfoArray^[Index].iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(ParamInfoArray^[Index].wType);
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //lob's
    begin
      { cbMaxLen returns max allowed bytes for Lob's which depends to server settings.
       So rowsize could have a overflow. In all cases we need to use references
       OR introduce DBTYPE_IUNKNOWN by using a IPersistStream/ISequentialStream/IStream see:
       http://msdn.microsoft.com/en-us/library/windows/desktop/ms709690%28v=vs.85%29.aspx }
      DBBindingArray[Index].cbMaxLen := SizeOf(Pointer);
      { now let's decide if we can use direct references or need space in buffer
        and a reference or if we need a external object for lob's}
      if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISOUTPUT <> 0) then
        raise Exception.Create('OUT/INOUT Parameter for LOB''s are currently not supported!');
      if not (InParamTypes[Index] in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
        Inc(LobBufCount);
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
    end
    else
    begin
      { all other types propably fit into one RowSize-Buffer }
      if DBBindingArray[Index].wType in [DBTYPE_GUID, DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR] then
        {for all these types we reserve a pointer and the buffer-memory, if we need it or not!
         this catches possible conversion later on. So we can either directly address or
         point to the buffer after the pointer where a converted value was moved in (:
         This may waste mem but makes everything flexible like a charm!}
      begin
         //all these types including GUID need a reference pointer except we do not play with multiple row binding
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        if DBBindingArray[Index].wType = DBTYPE_STR then
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+ParamInfoArray^[Index].ulParamSize +1
        else if DBBindingArray[Index].wType = DBTYPE_WSTR then
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+((ParamInfoArray^[Index].ulParamSize +1) shl 1)
        else
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer)+ParamInfoArray^[Index].ulParamSize;
        DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      end
      else
      begin { fixed types do not need a length indicator }
        DBBindingArray[Index].cbMaxLen := ParamInfoArray^[Index].ulParamSize;
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength;
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_STATUS;
      end;
    end;
    DBBindingArray[Index].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    { let's check param directions and set IO modes}
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISINPUT <> 0) then //input found
      if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISOUTPUT <> 0) then //output found too
        DBBindingArray[Index].eParamIO := DBPARAMIO_INPUT or DBPARAMIO_OUTPUT
      else
        DBBindingArray[Index].eParamIO := DBPARAMIO_INPUT
    else
      DBBindingArray[Index].eParamIO := DBPARAMIO_OUTPUT;
    DBBindingArray[Index].dwFlags :=  ParamInfoArray^[Index].dwFlags; //set found flags to indicate long types too
    DBBindingArray[Index].bPrecision := ParamInfoArray^[Index].bPrecision; //looks nice ... but do we need it?
    DBBindingArray[Index].bScale := ParamInfoArray^[Index].bScale; //looks nice ... but do we need it?
  end;
begin
  LobBufCount := 0;
  SetLength(DBBindingArray, DBUPARAMS);

  DBBindingArray[0].obStatus := 0;
  SetDBBindingProps(0);
  for i := 1 to DBUPARAMS -1 do
  begin
    DBBindingArray[i].obStatus := DBBindingArray[i-1].obValue  + DBBindingArray[i-1].cbMaxLen;
    SetDBBindingProps(I);
  end;
  Result := DBBindingArray[DBUPARAMS -1].obValue + DBBindingArray[DBUPARAMS -1].cbMaxLen;
  SetLength(TempLobs, LobBufCount);
end;

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO): DBROWOFFSET;
var
  I: Integer;
  Procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := DBCOLUMNINFO^.iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(DBCOLUMNINFO^.wType);
    if (DBCOLUMNINFO^.dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //lob's/referenced
    begin
      { cbMaxLen returns max allowed bytes for Lob's which depends to server settings.
       So rowsize could have a overflow. In all cases we need to use references
       OR introduce DBTYPE_IUNKNOWN by using a IPersistStream/ISequentialStream/IStream see:
       http://msdn.microsoft.com/en-us/library/windows/desktop/ms709690%28v=vs.85%29.aspx }
      DBBindingArray[Index].cbMaxLen := SizeOf(Pointer);
      { now let's decide if we can use direct references or need space in buffer
        and a reference or if we need a external object for lob's}
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
      DBBindingArray[Index].dwMemOwner := DBMEMOWNER_PROVIDEROWNED;
    end
    else
    begin
      { all other types propably fit into one RowSize-Buffer }
      if DBBindingArray[Index].wType in [DBTYPE_GUID, DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR, DBTYPE_BSTR] then
        {for all these types we reserve a pointer and the buffer-memory, if we need it or not!
         this catches possible conversion later on. So we can either directly address or
         point to the buffer after the pointer where a converted value was moved in (:
         This may waste mem but makes everything flexible like a charm!}
      begin
         //all these types including GUID need a reference pointer except we do not play with multiple row binding
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        {if DBBindingArray[Index].wType = DBCOLUMNINFO^.wType then
        begin //only types with NO conversion can be referenced!
          DBBindingArray[Index].cbMaxLen := SizeOf(Pointer);
          DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF;
        end does sadly not work! got a exception on first IRowSet.GetData call: conversion not supported!
        else}
          if DBBindingArray[Index].wType = DBTYPE_STR then
            DBBindingArray[Index].cbMaxLen := {SizeOf(Pointer)+}DBCOLUMNINFO^.ulColumnSize +1
          else if DBBindingArray[Index].wType in [DBTYPE_WSTR, DBTYPE_BSTR] then
            DBBindingArray[Index].cbMaxLen := {SizeOf(Pointer)+}((DBCOLUMNINFO^.ulColumnSize +1) shl 1)
          else
            DBBindingArray[Index].cbMaxLen := {SizeOf(Pointer)+}DBCOLUMNINFO^.ulColumnSize;
        //DBBindingArray[Index].wType := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      end
      else
      begin { fixed types do not need a length indicator }
        DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize;
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength;
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_STATUS;
      end;
    end;
    DBBindingArray[Index].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    DBBindingArray[Index].eParamIO := DBPARAMIO_NOTPARAM;
    DBBindingArray[Index].dwFlags :=  DBCOLUMNINFO^.dwFlags; //set found flags to indicate long types too
    DBBindingArray[Index].bPrecision := DBCOLUMNINFO^.bPrecision; //looks nice ... but do we need it?
    DBBindingArray[Index].bScale := DBCOLUMNINFO^.bScale; //looks nice ... but do we need it?
  end;
begin
  SetLength(DBBindingArray, DBUPARAMS);

  DBBindingArray[0].obStatus := 0;
  SetDBBindingProps(0);
  Inc(NativeUInt(DBCOLUMNINFO), SizeOf(TDBCOLUMNINFO));
  for i := 1 to DBUPARAMS -1 do
  begin
    DBBindingArray[i].obStatus := DBBindingArray[i-1].obValue  + DBBindingArray[i-1].cbMaxLen;
    SetDBBindingProps(I);
    Inc(NativeUInt(DBCOLUMNINFO), SizeOf(TDBCOLUMNINFO));
  end;
  Result := DBBindingArray[DBUPARAMS -1].obValue + DBBindingArray[DBUPARAMS -1].cbMaxLen;
end;

{$HINTS OFF}
procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; ClientVarManager: IZClientVariantManager);
var
  I: Integer;
  TempBlob: IZBlob;
  TmpStream: TStream;
  GUID: TGUID;

  procedure ProcessUnicode(ByRef: Boolean; Src: Pointer; CodePoints: Integer);
  begin
    if ByRef then
      if (Src = nil) or (CodePoints = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := PEmptyUnicodeString;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := (CodePoints) shl 1;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer);
      if (Src = nil) or (CodePoints = 0) then
      begin
        PWideChar(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer))^, (CodePoints+1) shl 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := (CodePoints) shl 1;
      end;
    end;
  end;
  procedure ProcessAnsi(ByRef: Boolean; Src: Pointer; Len: Integer);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := PEmptyAnsiString;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue +SizeOf(Pointer);
      if (Src = nil) or (Len = 0) then
      begin
        PAnsiChar(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue + SizeOf(Pointer))^, Len + 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end;
    end;
  end;
  procedure ProcessBinary(ByRef: Boolean; Src: Pointer; Len: Cardinal);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end
    else
    begin
      PNativeUInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+SizeOf(Pointer);
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+ SizeOf(Pointer))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue+ SizeOf(Pointer))^, Len);
        PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength)^ := Len;
      end;
    end;
  end;
begin
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to High(InParamValues) do
  begin
    if (InParamValues[I].VType = vtNull)  then
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL
    else
    begin
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_OK;
      case DBBindingArray[i].wType of
        DBTYPE_NULL:      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
        DBTYPE_I2:        PSmallInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_I4:        PInteger(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_R4:        PSingle(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_R8:        PDouble(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_CY:        PCurrency(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_DATE:      PDateTime(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsDateTime(InParamValues[i]);
        //DBTYPE_IDISPATCH	= 9;
        //DBTYPE_ERROR	= 10;
        DBTYPE_BOOL:      PWordBool(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsBoolean(InParamValues[i]);
        //DBTYPE_VARIANT	= 12;
        //DBTYPE_IUNKNOWN	= 13;
        DBTYPE_UI1:       PByte(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I1:        PShortInt(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI2:       PWord(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_UI4:       PLongWord(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I8:        PInt64(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI8:       PUInt64(NativeUInt(DBParams.pData)+DBBindingArray[i].obValue)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_GUID or DBTYPE_BYREF:
          if InParamValues[i].vType = vtBytes then
            ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16)
          else
            if InParamValues[i].vType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString] then
            begin
              GUID := StringToGUID(ClientVarManager.GetAsString(InParamValues[i]));
              ProcessBinary(False, @GUID.D1, 16)
            end
            else
              raise EZSQLException.Create(IntToStr(Ord(InParamTypes[i]))+' '+SUnsupportedParameterType);
        DBTYPE_BYTES or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(True, TempBlob.GetBuffer, TempBlob.Length);
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(True, Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(True,
                  Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes));
              end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                TempBlob.GetBuffer, Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(InParamValues[i].vBytes))))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  Min(DBBindingArray[I].cbMaxLen, NativeUInt(Length(InParamValues[i].vBytes))));
              end;
        DBTYPE_STR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //Ansi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, ConSettings);
                ProcessAnsi(True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                    Min(DBBindingArray[I].cbMaxLen-1, TempBlob.Length));
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(
                  TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].VRawByteString),
                  Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VRawByteString),
                Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
            end;
        DBTYPE_WSTR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length  shr 1);
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(True, Pointer(InParamValues[i].VUnicodeString), Length(InParamValues[i].VUnicodeString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VUnicodeString),
                Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(InParamValues[i].VUnicodeString)));
            end;
        else
          raise EZSQLException.Create(ZFastCode.IntToStr(DBBindingArray[i].wType)+' '+SUnsupportedParameterType);
        //DBTYPE_UDT: ;
        //DBTYPE_HCHAPTER:;
        //DBTYPE_PROPVARIANT:;
        //DBTYPE_VARNUMERIC:;


      end;
    end;
  end;
end;

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray);
var
  I, TempLobOffSet: Integer;
  J, BuffOffSet: DB_UPARAMS;
  TempBlob: IZBlob;
  UniTemp: ZWideString;
  AnsiTemp: AnsiString;
  IsNull: Boolean;
  SQLType: TZSQLType;
  TmpStream: TStream;
  GUID: TGUID;

  { array DML bindings }
  ZData: Pointer; //array entry
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;

  function IsNullFromIndicator: Boolean;
  begin
    case TZSQLType(InParamValues[I].VArray.VIsNullArrayType) of
      stBoolean: Result := ZBooleanArray[ArrayOffSet];
      stByte: Result := ZByteArray[ArrayOffSet] <> 0;
      stShort: Result := ZShortIntArray[ArrayOffSet] <> 0;
      stWord: Result := ZWordArray[ArrayOffSet] <> 0;
      stSmall: Result := ZSmallIntArray[ArrayOffSet] <> 0;
      stLongWord: Result := ZLongWordArray[ArrayOffSet] <> 0;
      stInteger: Result := ZIntegerArray[ArrayOffSet] <> 0;
      stLong: Result := ZInt64Array[ArrayOffSet] <> 0;
      stULong: Result := ZUInt64Array[ArrayOffSet] <> 0;
      stFloat: Result := ZSingleArray[ArrayOffSet] <> 0;
      stDouble: Result := ZDoubleArray[ArrayOffSet] <> 0;
      stCurrency: Result := ZCurrencyArray[ArrayOffSet] <> 0;
      stBigDecimal: Result := ZExtendedArray[ArrayOffSet] <> 0;
      stGUID:
        Result := True;
      stString, stUnicodeString:
        begin
          case InParamValues[i].VArray.VIsNullArrayVariantType of
            vtString: Result := StrToBoolEx(ZStringArray[ArrayOffSet]);
            vtAnsiString: Result := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
            vtUTF8String: Result := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
            vtRawByteString: Result := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
            vtUnicodeString: Result := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
            vtCharRec:
              if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                Result := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
              else
                Result := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
            vtNull: Result := True;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        end;
      stBytes:
        Result := ZBytesArray[ArrayOffSet] = nil;
      stDate, stTime, stTimestamp:
        Result := ZDateTimeArray[ArrayOffSet] <> 0;
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        Result := ZInterfaceArray[ArrayOffSet] = nil;
      else
        raise EZSQLException.Create(SUnsupportedParameterType);
    end;
  end;

  procedure ProcessUnicode(ByRef: Boolean; Src: Pointer; CodePoints: Integer);
  begin
    if ByRef then
      if (Src = nil) or (CodePoints = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := PEmptyUnicodeString;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := (CodePoints) shl 1;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (CodePoints = 0) then
      begin
        PWideChar(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet + SizeOf(Pointer)))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer)))^, (CodePoints+1) shl 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := (CodePoints) shl 1;
      end;
    end;
  end;
  procedure ProcessAnsi(ByRef: Boolean; Src: Pointer; Len: Integer);
  begin
    if ByRef then
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := PEmptyAnsiString;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (Len = 0) then
      begin
        PAnsiChar(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet + SizeOf(Pointer)))^ := #0;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer)))^, Len + 1);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end;
    end;
  end;
  procedure ProcessBinary(ByRef: Boolean; Src: Pointer; Len: Cardinal);
  begin
    if ByRef then // bind by ref is
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Src;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end
    else
    begin
      {set Reference Pointer first! see: PrepareOleDBBindings comment}
      PNativeUInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet+SizeOf(Pointer));
      if (Src = nil) or (Len = 0) then
      begin
        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := nil;
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := 0;
      end
      else
      begin
        System.Move(Src^, Pointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^, Len);
        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := Len;
      end;
    end;
  end;
begin
  BuffOffSet := 0;
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for J := 0 to DBParams.cParamSets-1 do
  begin
    TempLobOffSet := 0;
    for i := 0 to High(InParamValues) do
    begin
      ZData := InParamValues[I].VArray.VIsNullArray;
      if (ZData = nil) then
        IsNull := True
      else
        IsNull := IsNullFromIndicator;
      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL
      else
      begin
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
        SQLType := TZSQLType(InParamValues[I].VArray.VArrayType);
        case DBBindingArray[i].wType of
          DBTYPE_NULL:  PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL; //Shouldn't happen
          DBTYPE_I2:
            case SQLType of
              stBoolean:    PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSmallInt(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I4:
            case SQLType of
              stBoolean:    PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInteger(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R4:
            case SQLType of
              stBoolean:    PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSingle(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R8:
            case SQLType of
              stBoolean:    PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDouble(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_CY:
            case SQLType of
              stBoolean:    PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ELSE}
                  vtString:         PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZStringArray[ArrayOffSet], '.', 0);
                  {$ENDIF}
                  vtAnsiString:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZAnsiStringArray[ArrayOffSet], '.', 0);
                  vtUTF8String:     PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZUTF8StringArray[ArrayOffSet], '.', 0);
                  vtRawByteString:  PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZRawByteStringArray[ArrayOffSet], '.', 0);
                  vtUnicodeString:  PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZUnicodeStringArray[ArrayOffSet], WideChar('.'), 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToFloatDef(ZCharRecArray[ArrayOffSet].P, WideChar('.'), 0)
                    else
                      PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToFloatDef(ZCharRecArray[ArrayOffSet].P, '.', 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PCurrency(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_DATE:
            case SQLType of
              stBoolean:    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                  vtAnsiString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                  vtUTF8String:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                  vtRawByteString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                  vtUnicodeString:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ :=
                      ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDateTime(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_BOOL:
            case SQLType of
              stBoolean:    PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZBooleanArray[ArrayOffSet];
              stByte:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet] <> 0;
              stShort:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet] <> 0;
              stWord:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet] <> 0;
              stSmall:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet] <> 0;
              stLongWord:   PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet] <> 0;
              stInteger:    PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet] <> 0;
              stLong:       PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet] <> 0;
              stULong:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet] <> 0;
              stFloat:      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSingleArray[ArrayOffSet] <> 0;
              stDouble:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDoubleArray[ArrayOffSet] <> 0;
              stCurrency:   PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCurrencyArray[ArrayOffSet] <> 0;
              stBigDecimal: PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZExtendedArray[ArrayOffSet] <> 0;
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ELSE}
                  vtString:         PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ENDIF}
                  vtAnsiString:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
                  vtUTF8String:     PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
                  vtRawByteString:  PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
                  vtUnicodeString:  PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
                    else
                      PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWordBool(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZDateTimeArray[ArrayOffSet] <> 0;
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI1:
            case SQLType of
              stBoolean:    PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PByte(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI2:
            case SQLType of
              stBoolean:    PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI4:
            case SQLType of
              stBoolean:    PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PLongWord(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I8:
            case SQLType of
              stBoolean:    PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI8:
            case SQLType of
              stBoolean:    PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZByteArray[ArrayOffSet];
              stShort:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZShortIntArray[ArrayOffSet];
              stWord:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZWordArray[ArrayOffSet];
              stSmall:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZIntegerArray[ArrayOffSet];
              stLong:       PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZInt64Array[ArrayOffSet];
              stULong:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PUInt64(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_GUID or DBTYPE_BYREF: //GUID
            case SQLType of
              stGUID:
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), @ZGUIDArray[ArrayOffSet].D1, 16);
              stBytes:
                ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16);
              stString, stUnicodeString:
                begin
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: GUID := StringToGUID(ZStringArray[ArrayOffSet]);
                    vtAnsiString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet])));
                    vtUTF8String:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet])));
                    vtRawByteString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet])));
                    vtUnicodeString:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet])));
                    vtCharRec:
                      GUID := StringToGUID(ClientVarManager.GetAsString(EncodeCharRec(ZCharRecArray[ArrayOffSet])));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                  ProcessBinary(False, @GUID.D1, 16)
                end;
              else
                raise EZSQLException.Create('Unsupported GUID Variant');
            end;
          DBTYPE_BYTES or DBTYPE_BYREF:
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary(True, TempBlob.GetBuffer, TempBlob.Length);
                  end;
                stBytes:
                  ProcessBinary(True, Pointer(ZBytesArray[ArrayOffSet]), Length(ZBytesArray[ArrayOffSet]));
                stGUID:
                  ProcessBinary(True, @ZGUIDArray[ArrayOffSet].D1, 16);
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end
            else
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetBuffer, Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                  end;
                stBytes:
                  ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    Pointer(ZBytesArray[ArrayOffSet]),
                    Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(ZBytesArray[ArrayOffSet]))));
                stGUID:
                  ProcessBinary((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    @ZGUIDArray[ArrayOffSet].D1, Min(DBBindingArray[I].cbMaxLen, 16));
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end;
          DBTYPE_STR or DBTYPE_BYREF: //just prepared case!
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            begin
              case SQLType of
                stBoolean:      AnsiTemp := BoolToRawEx(ZBooleanArray[ArrayOffSet]);
                stByte:         AnsiTemp := IntToRaw(ZByteArray[ArrayOffSet]);
                stShort:        AnsiTemp := IntToRaw(ZShortIntArray[ArrayOffSet]);
                stWord:         AnsiTemp := IntToRaw(ZWordArray[ArrayOffSet]);
                stSmall:        AnsiTemp := IntToRaw(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     AnsiTemp := IntToRaw(ZLongWordArray[ArrayOffSet]);
                stInteger:      AnsiTemp := IntToRaw(ZIntegerArray[ArrayOffSet]);
                stULong:        AnsiTemp := IntToRaw(ZUInt64Array[ArrayOffSet]);
                stLong:         AnsiTemp := IntToRaw(ZInt64Array[ArrayOffSet]);
                stFloat:        AnsiTemp := FloatToRaw(ZSingleArray[ArrayOffSet]);
                stDouble:       AnsiTemp := FloatToRaw(ZDoubleArray[ArrayOffSet]);
                stCurrency:     AnsiTemp := FloatToRaw(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   AnsiTemp := FloatToRaw(ZExtendedArray[ArrayOffSet]);
                stTime:         AnsiTemp := DateTimeToRawSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         AnsiTemp := DateTimeToRawSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    AnsiTemp := DateTimeToRawSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: AnsiTemp := ConSettings^.ConvFuncs.ZStringToAnsi(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                    vtAnsiString: AnsiTemp := ZAnsiStringArray[ArrayOffSet];
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString: AnsiTemp := ZRawByteStringArray[ArrayOffSet];
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, ZDefaultSystemCodePage) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].P;
                        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
                        continue;
                      end
                      else
                        if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                          AnsiTemp := PUnicodeToRaw(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, GetACP)
                        else
                        begin
                          UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                          AnsiTemp := AnsiString(UniTemp);
                        end
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                    begin
                      TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //
                      ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PAnsiChar(AnsiTemp), Length(AnsiTemp), GetAcp, ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessAnsi(True, TempBlob.GetBuffer, TempBlob.Length);
              Inc(TempLobOffSet);
            end
            else
            begin
              case SQLType of
                stBoolean:      AnsiTemp := BoolToRawEx(ZBooleanArray[ArrayOffSet]);
                stByte:         AnsiTemp := IntToRaw(ZByteArray[ArrayOffSet]);
                stShort:        AnsiTemp := IntToRaw(ZShortIntArray[ArrayOffSet]);
                stWord:         AnsiTemp := IntToRaw(ZWordArray[ArrayOffSet]);
                stSmall:        AnsiTemp := IntToRaw(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     AnsiTemp := IntToRaw(ZLongWordArray[ArrayOffSet]);
                stInteger:      AnsiTemp := IntToRaw(ZIntegerArray[ArrayOffSet]);
                stULong:        AnsiTemp := IntToRaw(ZUInt64Array[ArrayOffSet]);
                stLong:         AnsiTemp := IntToRaw(ZInt64Array[ArrayOffSet]);
                stFloat:        AnsiTemp := FloatToRaw(ZSingleArray[ArrayOffSet]);
                stDouble:       AnsiTemp := FloatToRaw(ZDoubleArray[ArrayOffSet]);
                stCurrency:     AnsiTemp := FloatToRaw(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   AnsiTemp := FloatToRaw(ZExtendedArray[ArrayOffSet]);
                stTime:         AnsiTemp := DateTimeToRawSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         AnsiTemp := DateTimeToRawSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    AnsiTemp := DateTimeToRawSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString: AnsiTemp := ConSettings^.ConvFuncs.ZStringToAnsi(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                    vtAnsiString:
                      begin
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZAnsiStringArray[ArrayOffSet]),
                          Min(DBBindingArray[I].cbMaxLen-1, Length(ZAnsiStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString:
                      begin
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZRawByteStringArray[ArrayOffSet]),
                          Min(DBBindingArray[I].cbMaxLen-1, Length(ZRawByteStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, GetACP) then
                      begin
                        ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          Min(DBBindingArray[I].cbMaxLen-1, ZCharRecArray[ArrayOffSet].Len));
                        continue;
                      end
                      else
                        if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                          AnsiTemp := PUnicodeToRaw(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, GetACP)
                        else
                        begin
                          UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                          AnsiTemp := AnsiString(UniTemp);
                        end
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                    begin
                      TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //make internal conversion first
                      ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessAnsi(False, Pointer(AnsiTemp), //converted values can't be referenced
                Min(DBBindingArray[I].cbMaxLen-1, Length(AnsiTemp)));
            end;
          DBTYPE_WSTR or DBTYPE_BYREF:
            if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //insi lob's!!!
            begin
              case SQLType of
                stBoolean:      UniTemp := BoolToUnicodeEx(ZBooleanArray[ArrayOffSet]);
                stByte:         UniTemp := IntToUnicode(ZByteArray[ArrayOffSet]);
                stShort:        UniTemp := IntToUnicode(ZShortIntArray[ArrayOffSet]);
                stWord:         UniTemp := IntToUnicode(ZWordArray[ArrayOffSet]);
                stSmall:        UniTemp := IntToUnicode(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     UniTemp := IntToUnicode(ZLongWordArray[ArrayOffSet]);
                stInteger:      UniTemp := IntToUnicode(ZIntegerArray[ArrayOffSet]);
                stULong:        UniTemp := IntToUnicode(ZUInt64Array[ArrayOffSet]);
                stLong:         UniTemp := IntToUnicode(ZInt64Array[ArrayOffSet]);
                stFloat:        UniTemp := FloatToUnicode(ZSingleArray[ArrayOffSet]);
                stDouble:       UniTemp := FloatToUnicode(ZDoubleArray[ArrayOffSet]);
                stCurrency:     UniTemp := FloatToUnicode(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   UniTemp := FloatToUnicode(ZExtendedArray[ArrayOffSet]);
                stTime:         UniTemp := DateTimeToUnicodeSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         UniTemp := DateTimeToUnicodeSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    UniTemp := DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      {$IFDEF UNICODE}
                      UniTemp := ZStringArray[ArrayOffSet];
                      {$ELSE}
                      UniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                      {$ENDIF}
                    vtAnsiString: UniTemp := PRawToUnicode(Pointer(ZAnsiStringArray[ArrayOffSet]), Length(ZAnsiStringArray[ArrayOffSet]), GetACP);
                    vtUTF8String: UniTemp := PRawToUnicode(Pointer(ZUTF8StringArray[ArrayOffSet]), Length(ZUTF8StringArray[ArrayOffSet]), zCP_UTF8);
                    vtRawByteString: UniTemp := ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[ArrayOffSet], ConSettings^.ClientCodePage^.CP);
                    vtUnicodeString: UniTemp := ZUnicodeStringArray[ArrayOffSet];
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].P;
                        PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet))^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
                        continue;
                      end
                      else
                        UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP)
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                      TempBlob.GetPWideChar //make conversion first
                    else
                    begin
                      TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                      TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                      InParamValues[i].vInterface := TempBlob; //keep mem alive!
                      TmpStream.Free;
                    end;
                    ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PWideChar(UniTemp), Length(UniTemp), ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessUnicode(True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              Inc(TempLobOffSet);
            end
            else
            begin
              case SQLType of
                stBoolean:      UniTemp := BoolToUnicodeEx(ZBooleanArray[ArrayOffSet]);
                stByte:         UniTemp := IntToUnicode(ZByteArray[ArrayOffSet]);
                stShort:        UniTemp := IntToUnicode(ZShortIntArray[ArrayOffSet]);
                stWord:         UniTemp := IntToUnicode(ZWordArray[ArrayOffSet]);
                stSmall:        UniTemp := IntToUnicode(ZSmallIntArray[ArrayOffSet]);
                stLongWord:     UniTemp := IntToUnicode(ZLongWordArray[ArrayOffSet]);
                stInteger:      UniTemp := IntToUnicode(ZIntegerArray[ArrayOffSet]);
                stULong:        UniTemp := IntToUnicode(ZUInt64Array[ArrayOffSet]);
                stLong:         UniTemp := IntToUnicode(ZInt64Array[ArrayOffSet]);
                stFloat:        UniTemp := FloatToUnicode(ZSingleArray[ArrayOffSet]);
                stDouble:       UniTemp := FloatToUnicode(ZDoubleArray[ArrayOffSet]);
                stCurrency:     UniTemp := FloatToUnicode(ZCurrencyArray[ArrayOffSet]);
                stBigDecimal:   UniTemp := FloatToUnicode(ZExtendedArray[ArrayOffSet]);
                stTime:         UniTemp := DateTimeToUnicodeSQLTime(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stDate:         UniTemp := DateTimeToUnicodeSQLDate(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stTimeStamp:    UniTemp := DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[ArrayOffSet], ConSettings.WriteFormatSettings, False);
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      {$IFDEF UNICODE}
                      begin
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZStringArray[ArrayOffSet]),
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZStringArray[ArrayOffSet])));
                        continue;
                      end;
                      {$ELSE}
                      UniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[ArrayOffSet], ConSettings^.CTRL_CP);
                      {$ENDIF}
                    vtAnsiString: UniTemp := PRawToUnicode(Pointer(ZAnsiStringArray[ArrayOffSet]), Length(ZAnsiStringArray[ArrayOffSet]), GetACP);
                    vtUTF8String: UniTemp := PRawToUnicode(Pointer(ZUTF8StringArray[ArrayOffSet]), Length(ZUTF8StringArray[ArrayOffSet]), zCP_UTF8);
                    vtRawByteString: UniTemp := ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[ArrayOffSet], ConSettings^.ClientCodePage^.CP);
                    vtUnicodeString:
                      begin
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZUnicodeStringArray[ArrayOffSet]),
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZUnicodeStringArray[ArrayOffSet])));
                        continue;
                      end;
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin
                        ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          Min((DBBindingArray[I].cbMaxLen shr 1) -1, ZCharRecArray[ArrayOffSet].Len));
                        continue;
                      end
                      else
                        UniTemp := PRawToUnicode(ZCharRecArray[ArrayOffSet].P, ZCharRecArray[ArrayOffSet].Len, ZCharRecArray[ArrayOffSet].CP);
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stAsciiStream, stUnicodeStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    if TempBlob.IsClob then
                      TempBlob.GetPWideChar //make internal conversion first
                    else
                    begin
                      TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                      TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                      InParamValues[i].vInterface := TempBlob; //keep mem alive!
                      TmpStream.Free;
                    end;
                    ProcessUnicode((DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetPWideChar,
                      Min((DBBindingArray[I].cbMaxLen shr 1) -1, TempBlob.Length shr 1));
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessUnicode(False, Pointer(UniTemp),
                Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(UniTemp)));
            end;
          else
            raise EZSQLException.Create(ZFastCode.IntToStr(DBBindingArray[i].wType)+' '+SUnsupportedParameterType);
          //DBTYPE_UDT: ;
          //DBTYPE_HCHAPTER:;
          //DBTYPE_PROPVARIANT:;
          //DBTYPE_VARNUMERIC:;
        end;
      end;
    end;
    Inc(ArrayOffSet);
    Inc(BuffOffSet, RowSize);
  end;
end;
{$HINTS ON}

{$IFEND}
end.
