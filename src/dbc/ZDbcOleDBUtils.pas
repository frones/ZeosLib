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

uses
  Types, SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZCompatibility, ZDbcIntfs, ZOleDB, ZVariant, ZDbcStatement, Variants;

type
  TInterfacesDynArray = array of TInterfaceDynArray;

  /// binding status of a given column
  // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms720969
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TOleDBBindStatus = (DBBINDSTATUS_OK, DBBINDSTATUS_BADORDINAL,
    DBBINDSTATUS_UNSUPPORTEDCONVERSION, DBBINDSTATUS_BADBINDINFO,
    DBBINDSTATUS_BADSTORAGEFLAGS, DBBINDSTATUS_NOINTERFACE,
    DBBINDSTATUS_MULTIPLESTORAGE);

const
  VARIANT_TRUE = -1;
  VARIANT_FALSE = 0;

function ConvertSQLTypeToOleDBType(SQLType: TZSQLType): DBTYPEENUM;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM; IsLong: Boolean;
  CtrlsCPType: TZControlsCodePage): TZSQLType; overload;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  CtrlsCPType: TZControlsCodePage; const SrcRS: IZResultSet): TZSQLType; overload;

procedure OleDBCheck(aResult: HRESULT; const aStatus: TDBBINDSTATUSDynArray = nil);

{**
  Brings up the OleDB connection string builder dialog.
}
function PromptDataSource(Handle: THandle; const InitialString: WideString): WideString;

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; const InParamTypes: TZSQLTypeArray;
  ParamInfoArray: PDBParamInfoArray; var TempLobs: TInterfacesDynArray): DBROWOFFSET;

procedure InitOleParamDBBindings(out DBBindingArray: TDBParamInfoDynArray;
  const InParamTypes: TZSQLTypeArray; const InParamValues: TZVariantDynArray;
  const ClientVarManager: IZClientVariantManager);

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS; InMemoryData: Boolean;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO;
  var LobColIndexArray: TIntegerDynArray): DBROWOFFSET;

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; const ClientVarManager: IZClientVariantManager;
  SupportsMilliSeconds: Boolean = True);

procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray;
  const SupportsMilliseconds: Boolean = True);

procedure SetOleCommandProperties(const Command: ICommandText; TimeOut: SmallInt;
  Provider: TZServerProvider; SupportsMARSConnection, Prepare: Boolean);

function ProviderNamePrefix2ServerProvider(const ProviderNamePrefix: String): TZServerProvider;

implementation

uses
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ActiveX, Windows, Math, TypInfo,
  ZEncoding, ZDbcLogging, ZDbcUtils, ZDbcResultSet, ZFastCode, ZSysUtils, ZMessages,
  ZClasses;

function ConvertSQLTypeToOleDBType(SQLType: TZSQLType): DBTYPEENUM;
begin
  case SQLType of
    stBoolean:        Result := DBTYPE_BOOL;
    stByte:           Result := DBTYPE_UI1;
    stShort:          Result := DBTYPE_I1;
    stWord:           Result := DBTYPE_UI2;
    stSmall:          Result := DBTYPE_I2;
    stLongWord:       Result := DBTYPE_UI4;
    stInteger:        Result := DBTYPE_I4;
    stULong:          Result := DBTYPE_UI8;
    stLong:           Result := DBTYPE_I8;
    stFloat:          Result := DBTYPE_R4;
    stDouble:         Result := DBTYPE_R8;
    stCurrency:       Result := DBTYPE_CY;
    stBigDecimal:     Result := DBTYPE_R8;
    stDate:           Result := DBTYPE_DBDATE;
    stTime:           Result := DBTYPE_DBTIME2;
    stTimestamp:      Result := DBTYPE_DATE;
    stGUID:           Result := DBTYPE_GUID;
    stString:         Result := DBTYPE_WSTR;
    stUnicodeString:  Result := DBTYPE_WSTR;
    stBytes:          Result := DBTYPE_BYTES;
    stAsciiStream:    Result := DBTYPE_WSTR;
    stUnicodeStream:  Result := DBTYPE_WSTR;
    stBinaryStream:   Result := DBTYPE_BYTES;
    stArray:          Result := DBTYPE_VARIANT;
    stDataSet:        Result := DBTYPE_TABLE;
    else Result := DBTYPE_VARIANT;
  end;
end;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM; IsLong: Boolean;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case OleDBType of
    DBTYPE_EMPTY:       Result := stUnknown;
    DBTYPE_NULL:        Result := stUnknown;
    DBTYPE_I2:          Result := stSmall;
    DBTYPE_I4:          Result := stInteger;
    DBTYPE_R4:          Result := stFloat;
    DBTYPE_R8:          Result := stDouble;
    DBTYPE_CY:          Result := stCurrency;
    DBTYPE_DATE:        Result := stTimeStamp;
    DBTYPE_BSTR:        if IsLong then Result := stAsciiStream
                        else Result := stString;
    DBTYPE_ERROR:       Result := stInteger;
    DBTYPE_BOOL:        Result := stBoolean;
    DBTYPE_VARIANT:     Result := stString;
    DBTYPE_IUNKNOWN:    Result := stUnknown; //note this could be used to bind IStream for reading/writing data
    DBTYPE_DECIMAL:     Result := stBigDecimal;
    DBTYPE_UI1:         Result := stByte;
    DBTYPE_I1:          Result := stShort;
    DBTYPE_UI2:         Result := stWord;
    DBTYPE_UI4:         Result := stLongWord;
    DBTYPE_I8:          Result := stLong;
    DBTYPE_UI8:         Result := stULong;
    DBTYPE_GUID:        Result := stGUID;
    DBTYPE_BYTES:       if IsLong then Result := stBinaryStream
                        else Result := stBytes;
    DBTYPE_STR:         if IsLong then Result := stAsciiStream
                        else Result := stString;
    DBTYPE_WSTR:        if IsLong then Result := stAsciiStream
                        else Result := stString;
    DBTYPE_NUMERIC:     Result := stDouble;
    DBTYPE_UDT:         Result := stUnknown;
    DBTYPE_DBDATE:      Result := stDate;
    DBTYPE_DBTIME,
    DBTYPE_DBTIME2:     Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_VARNUMERIC:  Result := stDouble;
    DBTYPE_XML:         Result := stAsciiStream;
    DBTYPE_TABLE:       Result := stDataSet;
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
  CtrlsCPType: TZControlsCodePage; const SrcRS: IZResultSet): TZSQLType; overload;
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
    DBTYPE_DBTIME,
    DBTYPE_DBTIME2: Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_VARNUMERIC:  Result := stDouble;
    DBTYPE_XML:         Result := stAsciiStream;
    DBTYPE_TABLE:       Result := stDataSet;
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
var
  OleDBErrorMessage, FirstSQLState: String;
  ErrorInfo, ErrorInfoDetails: IErrorInfo;
  SQLErrorInfo: ISQLErrorInfo;
  MSSQLErrorInfo: ISQLServerErrorInfo;
  ErrorRecords: IErrorRecords;
  SSErrorPtr: PMSErrorInfo;
  i, ErrorCode, FirstErrorCode: Integer;
  ErrorCount: ULONG;
  Desc, SQLState: WideString;
  StringsBufferPtr: PWideChar;
  s: string;
begin
  if not Succeeded(aResult) then
  begin // get OleDB specific error information
    OleDBErrorMessage := '';
    FirstSQLState := '';
    FirstErrorCode := 0;
    GetErrorInfo(0,ErrorInfo);
    if Assigned(ErrorInfo) then
    begin
      ErrorRecords := ErrorInfo as IErrorRecords;
      ErrorRecords.GetRecordCount(ErrorCount);
      OleDBErrorMessage := '';
      for i := 0 to ErrorCount-1 do
      begin
        SQLErrorInfo := nil;
        if Succeeded(ErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, IUnknown(MSSQLErrorInfo)) ) and
          Assigned(MSSQLErrorInfo) then
        begin
          SSErrorPtr := nil;
          StringsBufferPtr:= nil;
          try //try use a SQL Server error interface
            if Succeeded(MSSQLErrorInfo.GetErrorInfo(SSErrorPtr, StringsBufferPtr)) and
              Assigned(SSErrorPtr) then
            begin
              if OleDBErrorMessage <> '' then OleDBErrorMessage := OleDBErrorMessage + LineEnding;
              if I = 0 then begin
                FirstErrorCode := SSErrorPtr^.lNative;
                FirstSQLState := String(SSErrorPtr^.pwszMessage);
              end;
              if OleDBErrorMessage <> '' then OleDBErrorMessage := OleDBErrorMessage+LineEnding;
              OleDBErrorMessage := OleDBErrorMessage + 'SQLState: '+ String(SSErrorPtr^.pwszMessage) +
                ' ErrorCode: '+ ZFastCode.IntToStr(SSErrorPtr^.lNative) +
                ' Line: '+ZFastCode.IntToStr(SSErrorPtr^.wLineNumber);
            end;
          finally
            if Assigned(SSErrorPtr) then CoTaskMemFree(SSErrorPtr);
            if Assigned(StringsBufferPtr) then CoTaskMemFree(StringsBufferPtr);
            MSSQLErrorInfo := nil;
          end
        end
        else //try use a common error interface
          if Succeeded(ErrorRecords.GetCustomErrorObject(i, IID_ISQLErrorInfo, IUnknown(SQLErrorInfo)) ) and
             Assigned(SQLErrorInfo) then
            try
              SQLErrorInfo.GetSQLInfo( SqlState, ErrorCode );
              if I = 0 then begin
                FirstErrorCode := ErrorCode;
                FirstSQLState := String(SqlState);
              end;
              if OleDBErrorMessage <> '' then OleDBErrorMessage := OleDBErrorMessage + LineEnding;
              OleDBErrorMessage := OleDBErrorMessage+'SQLState: '+ String(SqlState) + ' ErrorCode: '+ZFastCode.IntToStr(ErrorCode);
            finally
              SQLErrorInfo := nil;
            end;        // retrieve generic error info
        OleCheck(ErrorRecords.GetErrorInfo(i,GetSystemDefaultLCID,ErrorInfoDetails));
        OleCheck(ErrorInfoDetails.GetDescription(Desc));
        if OleDBErrorMessage<>'' then
          OleDBErrorMessage := OleDBErrorMessage+LineEnding
        else begin
          FirstErrorCode := aResult;
          FirstSQLState:= IntToHex(aResult,8);
        end;
        OleCheck(SetErrorInfo(0, ErrorInfoDetails));
        OleDBErrorMessage := OleDBErrorMessage+String(Desc);
        Desc := '';
        ErrorInfoDetails := nil;
      end;
    end;
    ErrorRecords := nil;
    ErrorInfo := nil;
    // get generic HRESULT error
    if aResult < 0 then //avoid range check error for some negative unknown errors
      s := '' else
      s := SysErrorMessage(aResult);
    if s='' then
      s := 'OLEDB Error '+IntToHex(aResult,8);
    if OleDBErrorMessage = '' then begin
      FirstErrorCode := aResult;
      FirstSQLState := IntToHex(aResult,8);
      OleDBErrorMessage := s;
    end else
      OleDBErrorMessage := s+':'+LineEnding+OleDBErrorMessage;
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
    raise EZSQLException.CreateWithCodeAndStatus(FirstErrorCode, FirstSQLState, OleDBErrorMessage);
  end;
end;

{**
  Brings up the OleDB connection string builder dialog.
}
function PromptDataSource(Handle: THandle; const InitialString: WideString): WideString;
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
begin
  Result := InitialString;
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if InitialString <> '' then
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(InitialString), IUnknown, DataSource{%H-});
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;
  if Succeeded(DBPrompt.PromptDataSource(nil, Handle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource)) then
  begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    Result := InitStr;
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
    //DBTYPE_HCHAPTER	= 136;
    DBTYPE_FILETIME: Result := DBTYPE_DATE;
    //DBTYPE_PROPVARIANT	= 138;
    DBTYPE_VARNUMERIC: Result := DBTYPE_R8;
    //DBTYPE_DBTIME2: Result := DBTYPE_DBTIME2;
    DBTYPE_XML:     Result := DBTYPE_WSTR;
    DBTYPE_DBTIMESTAMPOFFSET: Result := DBTYPE_DBTIMESTAMP;
   // DBTYPE_TABLE;
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
        DBBindingArray[Index].cbMaxLen := ParamInfoArray[Index].ulParamSize;
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
    DBBindingArray[Index].bPrecision := ParamInfoArray^[Index].bPrecision;
    DBBindingArray[Index].bScale := ParamInfoArray^[Index].bScale;
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

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS; InMemoryData: Boolean;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO;
  var LobColIndexArray: TIntegerDynArray): DBROWOFFSET;
var
  I: Integer;
  procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := DBCOLUMNINFO^.iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(DBCOLUMNINFO^.wType);
    if (DBCOLUMNINFO^.dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //lob's/referenced
    begin
      if InMemoryData then //BLOBs as In-Memory Data version
      begin
        //(need to release mem on freeing the rows!):
        DBBindingArray[Index].cbMaxLen  := SizeOf(Pointer);
        DBBindingArray[Index].obValue   := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].wType     := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
        DBBindingArray[Index].dwPart    := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        //DBBindingArray[Index].dwFlags   := DBCOLUMNFLAGS_ISLONG; //indicate long values! <- trouble with SQLNCLI11 provider!
      end
      else
      begin //using ISeqentialStream -> Retrieve data directly from Provider
        DBBindingArray[Index].cbMaxLen  := 0;
        DBBindingArray[Index].dwPart    := DBPART_STATUS; //we only need a NULL indicator!
        DBBindingArray[Index].wType     := DBCOLUMNINFO^.wType; //Save the wType to know Binary/Ansi/Unicode-Lob's later on
        DBBindingArray[Index].obValue   := DBBindingArray[Index].obLength;
        //DBBindingArray[Index].dwFlags   := DBCOLUMNFLAGS_ISLONG; //indicate long values! <- trouble with SQLNCLI11 provider!
        //dirty improvements!
        DBBindingArray[Index].obLength  := Length(LobColIndexArray); //Save the HACCESSOR lookup index -> avoid loops!
      end;
      SetLength(LobColIndexArray, Length(LobColIndexArray)+1);
      LobColIndexArray[High(LobColIndexArray)] := Index;
    end
    else
    begin
      { all other types propably fit into one RowSize-Buffer }
      if DBBindingArray[Index].wType in [DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR, DBTYPE_BSTR] then
      begin
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        if DBBindingArray[Index].wType = DBTYPE_STR then
          DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize +1
        else if DBBindingArray[Index].wType in [DBTYPE_WSTR, DBTYPE_BSTR] then
          DBBindingArray[Index].cbMaxLen := (DBCOLUMNINFO^.ulColumnSize+1) shl 1
        else
          DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize;
        //8Byte Alignment and optimzed Accessor(fetch) does NOT  work if:
        //fixed width fields came to shove!!!!
        //DBBindingArray[Index].cbMaxLen := ((DBBindingArray[Index].cbMaxLen-1) shr 3+1) shl 3;
        {if (DBCOLUMNINFO^.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0) then //vary
          DBBindingArray[Index].cbMaxLen := ((DBBindingArray[Index].cbMaxLen-1) shr 3+1) shl 3
        else}
        if (DBCOLUMNINFO^.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0) then //fixed length ' ' padded?
          DBBindingArray[Index].dwFlags := DBCOLUMNFLAGS_ISFIXEDLENGTH;//keep this flag alive! We need it for conversions of the RS's
      end
      else
      begin { fixed types do not need a length indicator }
        if DBBindingArray[Index].wType = DBTYPE_DBTIME2 then
          DBBindingArray[Index].dwFlags := DBCOLUMNINFO^.dwFlags; //keep it!
        DBBindingArray[Index].bPrecision := DBCOLUMNINFO^.bPrecision;
        DBBindingArray[Index].bScale := DBCOLUMNINFO^.bScale;
        DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize;
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength;
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_STATUS;
      end;
    end;
    DBBindingArray[Index].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    DBBindingArray[Index].eParamIO := DBPARAMIO_NOTPARAM;
    //makes trouble !!DBBindingArray[Index].dwFlags :=  DBCOLUMNINFO^.dwFlags; //set found flags to indicate long types too
  end;
begin
  SetLength(LobColIndexArray, 0);
  SetLength(DBBindingArray, DBUPARAMS);

  DBBindingArray[0].obStatus := 0;
  SetDBBindingProps(0);
  Inc({%H-}NativeUInt(DBCOLUMNINFO), SizeOf(TDBCOLUMNINFO));
  for i := 1 to DBUPARAMS -1 do
  begin
    DBBindingArray[i].obStatus := DBBindingArray[i-1].obValue  + DBBindingArray[i-1].cbMaxLen;
    SetDBBindingProps(I);
    Inc({%H-}NativeUInt(DBCOLUMNINFO), SizeOf(TDBCOLUMNINFO));
  end;
  Result := DBBindingArray[DBUPARAMS -1].obValue + DBBindingArray[DBUPARAMS -1].cbMaxLen;
end;

procedure InitOleParamDBBindings(out DBBindingArray: TDBParamInfoDynArray;
  const InParamTypes: TZSQLTypeArray; const InParamValues: TZVariantDynArray;
  const ClientVarManager: IZClientVariantManager);
var I: Integer;
begin
  SetLength(DBBindingArray, Length(InParamTypes));
  for I := 0 to High(InParamTypes) do begin
    DBBindingArray[i].iOrdinal := i+1;
    DBBindingArray[i].wType := ConvertSQLTypeToOleDBType(InParamTypes[i]);
    DBBindingArray[i].ulParamSize := ZSQLTypeToBuffSize(InParamTypes[i]);
    DBBindingArray[i].dwFlags := STGM_READ or DBPARAMFLAGS_ISINPUT;
    DBBindingArray[i].pwszName := nil;
    DBBindingArray[i].pTypeInfo := nil;
    DBBindingArray[i].bPrecision := DBBindingArray[i].ulParamSize;
    if (DBBindingArray[i].ulParamSize = 0) then begin
      if InParamTypes[i] in [stString, stUnicodeString] then
        DBBindingArray[i].ulParamSize := Min(255, Length(ClientVarManager.GetAsUnicodeString(InParamValues[i])) shl 3 shr 1)
      else if InParamTypes[i] = stBytes then
        DBBindingArray[i].ulParamSize := Min(255, Length(ClientVarManager.GetAsBytes(InParamValues[i])) shl 3 shr 1)
      else if InParamTypes[i] in [stAsciiStream, stUnicodeStream, stBinaryStream] then
        DBBindingArray[i].dwFlags := DBBindingArray[i].dwFlags or DBPARAMFLAGS_ISLONG;
    end;
  end;
end;

{$HINTS OFF}

procedure ProcessUnicode(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; CodePoints: Integer);
begin
  PLen^ := CodePoints*SizeOf(WideChar);
  if ByRef then
    if (Src = nil) or (CodePoints = 0) then
      PPointer(Data)^ := PEmptyUnicodeString
    else
      PPointer(Data)^ := Src
  else
  begin
    {set Reference Pointer first! see: PrepareOleDBBindings comment}
    PNativeUInt(Data)^ := Data+SizeOf(Pointer);
    if (Src = nil) or (CodePoints = 0) then
      PWideChar(Data + SizeOf(Pointer))^ := #0
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^+SizeOf(WideChar));
  end;
end;

procedure ProcessAnsi(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; Len: Integer);
begin
  PLen^ := Len*SizeOf(AnsiChar);
  if ByRef then
    if (Src = nil) or (Len = 0) then
      PPointer(Data)^ := PEmptyAnsiString
    else
      PPointer(Data)^ := Src
  else
  begin
    {set Reference Pointer first! see: PrepareOleDBBindings comment}
    PNativeUInt(Data)^ := Data +SizeOf(Pointer);
    if (Src = nil) or (Len = 0) then
      PAnsiChar(Data + SizeOf(Pointer))^ := #0
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^+SizeOf(AnsiChar));
  end;
end;

procedure ProcessBinary(Data: NativeUInt; PLen: PDBLENGTH; ByRef: Boolean; Src: Pointer; Len: Cardinal);
begin
  PLen^ := Len;
  if ByRef then
    if (Src = nil) or (Len = 0) then
      PPointer(Data)^ := nil
    else
      PPointer(Data)^ := Src
  else
  begin
    PNativeUInt(Data)^ := Data+SizeOf(Pointer);
    if (Src = nil) or (Len = 0) then
      PPointer(Data + SizeOf(Pointer))^ := nil
    else
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Data + SizeOf(Pointer))^, PLen^);
  end;
end;

procedure OleBindParams(const DBParams: TDBParams; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const InParamValues: TZVariantDynArray;
  const InParamTypes: TZSQLTypeArray; const ClientVarManager: IZClientVariantManager;
  SupportsMilliSeconds: Boolean = True);
var
  Year, MilliSecond: Word;
  I: Integer;
  TempBlob: IZBlob;
  TmpStream: TStream;
  GUID: TGUID;
  Data: NativeUInt;
  PLen: PDBLENGTH;
begin
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to High(InParamValues) do
  begin
    if (InParamValues[I].VType = vtNull)  then
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL
    else
    begin
      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_OK;
      Data := NativeUInt(DBParams.pData)+DBBindingArray[i].obValue;
      //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
      PLen := PDBLENGTH(NativeUInt(DBParams.pData)+DBBindingArray[i].obLength);
      case DBBindingArray[i].wType of
        DBTYPE_NULL:      PDBSTATUS(NativeUInt(DBParams.pData)+DBBindingArray[i].obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
        DBTYPE_I2:        PSmallInt(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_I4:        PInteger(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_R4:        PSingle(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_R8:        PDouble(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_CY:        PCurrency(Data)^ := ClientVarManager.GetAsFloat(InParamValues[i]);
        DBTYPE_DATE:      PDateTime(Data)^ := ClientVarManager.GetAsDateTime(InParamValues[i]);
        //DBTYPE_IDISPATCH	= 9;
        //DBTYPE_ERROR	= 10;
        DBTYPE_BOOL:      PWordBool(Data)^ := ClientVarManager.GetAsBoolean(InParamValues[i]);
        //DBTYPE_VARIANT	= 12;
        //DBTYPE_IUNKNOWN	= 13;
        DBTYPE_UI1:       PByte(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I1:        PShortInt(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI2:       PWord(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_UI4:       PLongWord(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_I8:        PInt64(Data)^ := ClientVarManager.GetAsInteger(InParamValues[i]);
        DBTYPE_UI8:       PUInt64(Data)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        DBTYPE_GUID or DBTYPE_BYREF:
          if InParamValues[i].vType = vtBytes then
            ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16)
          else
            if InParamValues[i].vType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString] then
            begin
              GUID := StringToGUID(ClientVarManager.GetAsString(InParamValues[i]));
              ProcessBinary(Data, PLen, False, @GUID.D1, 16)
            end
            else
              raise EZSQLException.Create(IntToStr(Ord(InParamTypes[i]))+' '+SUnsupportedParameterType);
        DBTYPE_BYTES or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //binary lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(Data, PLen, True, Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(Data, PLen, True,
                  Pointer(InParamValues[i].vBytes), Length(InParamValues[i].vBytes));
              end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                TempBlob.GetBuffer, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
            end
            else
              if InParamValues[i].vType = vtBytes then
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(InParamValues[i].vBytes))))
              else
              begin
                InParamValues[i] := ClientVarManager.Convert(InParamValues[i], vtBytes);
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].vBytes),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, NativeUInt(Length(InParamValues[i].vBytes))));
              end;
        DBTYPE_STR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //Ansi lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, ConSettings);
                ProcessAnsi(Data, PLen, True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(Data, PLen, True, Pointer(InParamValues[i].VRawByteString), Length(InParamValues[i].VRawByteString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                    {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, TempBlob.Length));
              end
              else
              begin
                InParamValues[i].VRawByteString := GetValidatedAnsiStringFromBuffer(
                  TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  Pointer(InParamValues[i].VRawByteString),
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
              end;
            end
            else
            begin
              InParamValues[i].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[i]);
              ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VRawByteString),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(InParamValues[i].VRawByteString)));
            end;
        DBTYPE_WSTR or DBTYPE_BYREF:
          if (DBBindingArray[i].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then //Unicode lob's!!!
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(Data, PLen, True, Pointer(InParamValues[i].VUnicodeString), Length(InParamValues[i].VUnicodeString));
            end
          else
            if (InParamValues[i].vType = vtInterface) then
            begin
              TempBlob := InParamValues[i].vInterface as IZBLob;
              if TempBlob.IsClob then
              begin
                TempBlob.GetPWideChar;
                ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end
              else
              begin
                TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                TempBlob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                InParamValues[i].vInterface := TempBlob;
                TmpStream.Free;
                ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                  TempBlob.GetPWideChar,
                  {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, TempBlob.Length shr 1));
              end;
            end
            else
            begin
              InParamValues[i].VUnicodeString := ClientVarManager.GetAsUnicodeString(InParamValues[i]);
              ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                Pointer(InParamValues[i].VUnicodeString),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(InParamValues[i].VUnicodeString)));
            end;
        DBTYPE_DBDATE:
          begin
            DecodeDate(ClientVarManager.GetAsDateTime(InParamValues[i]), Year,
              PDBDate(Data)^.month, PDBDate(Data)^.day);
            PDBDate(Data)^.year := Year;
          end;
        DBTYPE_DBTIME:
          DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
            PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second,
            MilliSecond);
        DBTYPE_DBTIME2:
          begin
            DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
              PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second,
              MilliSecond);
              PDBTIME2(Data)^.fraction := Millisecond * 1000000;
          end;
        DBTYPE_DBTIMESTAMP:
          begin
            DecodeDate(ClientVarManager.GetAsDateTime(InParamValues[i]), Year,
              PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
            PDBTimeStamp(Data)^.year := Year;
            DecodeTime(ClientVarManager.GetAsDateTime(InParamValues[i]),
              PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute,
              PDBTimeStamp(Data)^.second, MilliSecond);
            if SupportsMilliSeconds then
              PDBTimeStamp(Data)^.fraction := MilliSecond*1000000
            else
              PDBTimeStamp(Data)^.fraction := 0;
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

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure OleBindArrayParams(const DBParams: TDBParams; ArrayOffSet: DB_UPARAMS;
  RowSize: NativeUInt; ConSettings: PZConSettings;
  const DBBindingArray: TDBBindingDynArray; const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const TempLobs: TInterfacesDynArray;
  const SupportsMilliseconds: Boolean = True);
var
  I, TempLobOffSet: Integer;
  Year, MilliSecond: Word;
  J, BuffOffSet: DB_UPARAMS;
  TempBlob: IZBlob;
  UniTemp: ZWideString;
  AnsiTemp: AnsiString;
  DateTimeTemp: TDateTime;
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

  Data: NativeUInt;
  PLen: PDBLENGTH;

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
        IsNull := False
      else
        IsNull := IsNullFromIndicator;
      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL
      else
      begin
        PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
        SQLType := TZSQLType(InParamValues[I].VArray.VArrayType);
        Data := NativeUInt(DBParams.pData)+(DBBindingArray[i].obValue + BuffOffSet);
        //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
        PLen := PDBLENGTH(NativeUInt(DBParams.pData)+(DBBindingArray[i].obLength + BuffOffSet));
        case DBBindingArray[i].wType of
          DBTYPE_NULL:  PDBSTATUS(NativeUInt(DBParams.pData)+(DBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL; //Shouldn't happen
          DBTYPE_I2:
            case SQLType of
              stBoolean:    PSmallInt(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSmallInt(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PSmallInt(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSmallInt(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PSmallInt(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSmallInt(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSmallInt(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSmallInt(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PSmallInt(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSmallInt(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PSmallInt(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PSmallInt(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PSmallInt(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PSmallInt(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PSmallInt(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PSmallInt(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PSmallInt(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PSmallInt(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PSmallInt(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PSmallInt(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSmallInt(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I4:
            case SQLType of
              stBoolean:    PInteger(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInteger(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PInteger(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInteger(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PInteger(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInteger(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInteger(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInteger(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PInteger(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInteger(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInteger(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInteger(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInteger(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInteger(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInteger(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInteger(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInteger(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInteger(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInteger(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInteger(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInteger(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R4:
            case SQLType of
              stBoolean:    PSingle(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PSingle(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PSingle(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PSingle(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PSingle(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PSingle(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PSingle(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PSingle(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PSingle(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PSingle(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PSingle(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PSingle(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PSingle(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PSingle(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PSingle(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PSingle(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PSingle(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PSingle(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_R8:
            case SQLType of
              stBoolean:    PDouble(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDouble(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PDouble(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDouble(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PDouble(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDouble(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDouble(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDouble(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PDouble(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDouble(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDouble(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDouble(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDouble(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PDouble(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PDouble(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PDouble(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PDouble(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDouble(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_CY:
            case SQLType of
              stBoolean:    PCurrency(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PCurrency(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PCurrency(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PCurrency(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PCurrency(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PCurrency(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PCurrency(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PCurrency(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PCurrency(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PCurrency(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PCurrency(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PCurrency(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PCurrency(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString,{$ENDIF}
                  vtAnsiString,vtUTF8String,
                  vtRawByteString:  SQLStrToFloatDef(PAnsiChar(Pointer(ZRawByteStringArray[ArrayOffSet])), 0, PCurrency(Data)^, Length(ZRawByteStringArray[ArrayOffSet]));
                  {$IFDEF UNICODE}vtString,{$ENDIF}
                  vtUnicodeString:  SQLStrToFloatDef(PWideChar(Pointer(ZUnicodeStringArray[ArrayOffSet])), 0, PCurrency(Data)^, Length(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      SQLStrToFloatDef(PWideChar(ZCharRecArray[ArrayOffSet].P), 0, PCurrency(Data)^, ZCharRecArray[ArrayOffSet].Len)
                    else
                      SQLStrToFloatDef(PAnsiChar(ZCharRecArray[ArrayOffSet].P), 0, PCurrency(Data)^, ZCharRecArray[ArrayOffSet].Len)
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PCurrency(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_DATE:
            case SQLType of
              stBoolean:    PDateTime(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PDateTime(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PDateTime(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PDateTime(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PDateTime(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PDateTime(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PDateTime(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PDateTime(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PDateTime(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PDateTime(Data)^ := ZSingleArray[ArrayOffSet];
              stDouble:     PDateTime(Data)^ := ZDoubleArray[ArrayOffSet];
              stCurrency:   PDateTime(Data)^ := ZCurrencyArray[ArrayOffSet];
              stBigDecimal: PDateTime(Data)^ := ZExtendedArray[ArrayOffSet];
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                  vtAnsiString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                  vtUTF8String:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                  vtRawByteString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                  vtUnicodeString:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                  vtCharRec:
                    PDateTime(Data)^ := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PDateTime(Data)^ := ZDateTimeArray[ArrayOffSet];
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_BOOL:
            case SQLType of
              stBoolean:    PWordBool(Data)^ := ZBooleanArray[ArrayOffSet];
              stByte:       PWordBool(Data)^ := ZByteArray[ArrayOffSet] <> 0;
              stShort:      PWordBool(Data)^ := ZShortIntArray[ArrayOffSet] <> 0;
              stWord:       PWordBool(Data)^ := ZWordArray[ArrayOffSet] <> 0;
              stSmall:      PWordBool(Data)^ := ZSmallIntArray[ArrayOffSet] <> 0;
              stLongWord:   PWordBool(Data)^ := ZLongWordArray[ArrayOffSet] <> 0;
              stInteger:    PWordBool(Data)^ := ZIntegerArray[ArrayOffSet] <> 0;
              stLong:       PWordBool(Data)^ := ZInt64Array[ArrayOffSet] <> 0;
              stULong:      PWordBool(Data)^ := ZUInt64Array[ArrayOffSet] <> 0;
              stFloat:      PWordBool(Data)^ := ZSingleArray[ArrayOffSet] <> 0;
              stDouble:     PWordBool(Data)^ := ZDoubleArray[ArrayOffSet] <> 0;
              stCurrency:   PWordBool(Data)^ := ZCurrencyArray[ArrayOffSet] <> 0;
              stBigDecimal: PWordBool(Data)^ := ZExtendedArray[ArrayOffSet] <> 0;
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  {$IFDEF UNICODE}
                  vtString:         PWordBool(Data)^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ELSE}
                  vtString:         PWordBool(Data)^ := StrToBoolEx(ZStringArray[ArrayOffSet]);
                  {$ENDIF}
                  vtAnsiString:     PWordBool(Data)^ := StrToBoolEx(ZAnsiStringArray[ArrayOffSet]);
                  vtUTF8String:     PWordBool(Data)^ := StrToBoolEx(ZUTF8StringArray[ArrayOffSet]);
                  vtRawByteString:  PWordBool(Data)^ := StrToBoolEx(ZRawByteStringArray[ArrayOffSet]);
                  vtUnicodeString:  PWordBool(Data)^ := StrToBoolEx(ZUnicodeStringArray[ArrayOffSet]);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWordBool(Data)^ := StrToBoolEx(PWideChar(ZCharRecArray[ArrayOffSet].P))
                    else
                      PWordBool(Data)^ := StrToBoolEx(PAnsiChar(ZCharRecArray[ArrayOffSet].P));
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWordBool(Data)^ := ZDateTimeArray[ArrayOffSet] <> 0;
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI1:
            case SQLType of
              stBoolean:    PByte(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PByte(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PByte(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PByte(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PByte(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PByte(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PByte(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PByte(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PByte(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PByte(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PByte(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PByte(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PByte(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PByte(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PByte(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PByte(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PByte(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PByte(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PByte(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PByte(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PByte(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI2:
            case SQLType of
              stBoolean:    PWord(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PWord(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PWord(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PWord(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PWord(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PWord(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PWord(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PWord(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PWord(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PWord(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PWord(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PWord(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PWord(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PWord(Data)^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PWord(Data)^ := RawToIntDef(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PWord(Data)^ := RawToIntDef(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PWord(Data)^ := RawToIntDef(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PWord(Data)^ := UnicodeToIntDef(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PWord(Data)^ := UnicodeToIntDef(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PWord(Data)^ := RawToIntDef(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PWord(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI4:
            case SQLType of
              stBoolean:    PLongWord(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PLongWord(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PLongWord(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PLongWord(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PLongWord(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PLongWord(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PLongWord(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PLongWord(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PLongWord(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PLongWord(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PLongWord(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PLongWord(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PLongWord(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PLongWord(Data)^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PLongWord(Data)^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PLongWord(Data)^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PLongWord(Data)^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PLongWord(Data)^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PLongWord(Data)^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PLongWord(Data)^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PLongWord(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_I8:
            case SQLType of
              stBoolean:    PInt64(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PInt64(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PInt64(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PInt64(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PInt64(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PInt64(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PInt64(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PInt64(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PInt64(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PInt64(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PInt64(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PInt64(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PInt64(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PInt64(Data)^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PInt64(Data)^ := RawToInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PInt64(Data)^ := RawToInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PInt64(Data)^ := RawToInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PInt64(Data)^ := UnicodeToInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PInt64(Data)^ := UnicodeToInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PInt64(Data)^ := RawToInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PInt64(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_UI8:
            case SQLType of
              stBoolean:    PUInt64(Data)^ := Ord(ZBooleanArray[ArrayOffSet]);
              stByte:       PUInt64(Data)^ := ZByteArray[ArrayOffSet];
              stShort:      PUInt64(Data)^ := ZShortIntArray[ArrayOffSet];
              stWord:       PUInt64(Data)^ := ZWordArray[ArrayOffSet];
              stSmall:      PUInt64(Data)^ := ZSmallIntArray[ArrayOffSet];
              stLongWord:   PUInt64(Data)^ := ZLongWordArray[ArrayOffSet];
              stInteger:    PUInt64(Data)^ := ZIntegerArray[ArrayOffSet];
              stLong:       PUInt64(Data)^ := ZInt64Array[ArrayOffSet];
              stULong:      PUInt64(Data)^ := ZUInt64Array[ArrayOffSet];
              stFloat:      PUInt64(Data)^ := Trunc(ZSingleArray[ArrayOffSet]);
              stDouble:     PUInt64(Data)^ := Trunc(ZDoubleArray[ArrayOffSet]);
              stCurrency:   PUInt64(Data)^ := Trunc(ZCurrencyArray[ArrayOffSet]);
              stBigDecimal: PUInt64(Data)^ := Trunc(ZExtendedArray[ArrayOffSet]);
              stString, stUnicodeString:
                case InParamValues[i].VArray.VArrayVariantType of
                  vtString:         PUInt64(Data)^ := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZStringArray[ArrayOffSet], 0);
                  vtAnsiString:     PUInt64(Data)^ := RawToUInt64Def(ZAnsiStringArray[ArrayOffSet], 0);
                  vtUTF8String:     PUInt64(Data)^ := RawToUInt64Def(ZUTF8StringArray[ArrayOffSet], 0);
                  vtRawByteString:  PUInt64(Data)^ := RawToUInt64Def(ZRawByteStringArray[ArrayOffSet], 0);
                  vtUnicodeString:  PUInt64(Data)^ := UnicodeToUInt64Def(ZUnicodeStringArray[ArrayOffSet], 0);
                  vtCharRec:
                    if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      PUInt64(Data)^ := UnicodeToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0)
                    else
                      PUInt64(Data)^ := RawToUInt64Def(ZCharRecArray[ArrayOffSet].P, 0);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              stTime, stDate, stTimeStamp:
                PUInt64(Data)^ := Trunc(ZDateTimeArray[ArrayOffSet]);
              else
                raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
            end;
          DBTYPE_GUID or DBTYPE_BYREF: //GUID
            case SQLType of
              stGUID:
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), @ZGUIDArray[ArrayOffSet].D1, 16);
              stBytes:
                ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0), Pointer(InParamValues[i].vBytes), 16);
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
                  ProcessBinary(Data, PLen, False, @GUID.D1, 16)
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
                    ProcessBinary(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
                  end;
                stBytes:
                  ProcessBinary(Data, PLen, True, Pointer(ZBytesArray[ArrayOffSet]), Length(ZBytesArray[ArrayOffSet]));
                stGUID:
                  ProcessBinary(Data, PLen, True, @ZGUIDArray[ArrayOffSet].D1, 16);
                else
                  raise Exception.Create('Unsupported Byte-Array Variant');
              end
            else
              case SQLType of
                stBinaryStream:
                  begin
                    TempBlob := ZInterfaceArray[ArrayOffSet] as IZBLob;
                    ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetBuffer, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                  end;
                stBytes:
                  ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    Pointer(ZBytesArray[ArrayOffSet]),
                    {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen,NativeUInt(Length(ZBytesArray[ArrayOffSet]))));
                stGUID:
                  ProcessBinary(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                    @ZGUIDArray[ArrayOffSet].D1, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, 16));
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
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, ZOSCodePage) then
                      begin //here we always reference as long we do not support Out-IO. So this is valid!
                        PPointer(Data)^ := ZCharRecArray[ArrayOffSet].P;
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
                      ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(Data, PLen, True, TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), TempBlob.Length);
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PAnsiChar(AnsiTemp), Length(AnsiTemp), GetAcp, ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessAnsi(Data, PLen, True, TempBlob.GetBuffer, TempBlob.Length);
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
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZAnsiStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(ZAnsiStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUTF8String: AnsiTemp := ZConvertUTF8ToAnsi(ZUTF8StringArray[ArrayOffSet]);
                    vtRawByteString:
                      begin
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZRawByteStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(ZRawByteStringArray[ArrayOffSet])));
                        Continue;
                      end;
                    vtUnicodeString: AnsiTemp := AnsiString(ZUnicodeStringArray[ArrayOffSet]);
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, GetACP) then
                      begin
                        ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, ZCharRecArray[ArrayOffSet].Len));
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
                      ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end
                    else
                    begin
                      TempBlob.SetAnsiString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, ConSettings));
                      ProcessAnsi(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
                        {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen, TempBlob.Length));
                    end;
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessAnsi(Data, PLen, False, Pointer(AnsiTemp), //converted values can't be referenced
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(DBBindingArray[I].cbMaxLen-1, Length(AnsiTemp)));
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
                        PPointer(Data)^ := ZCharRecArray[ArrayOffSet].P;
                        PLen^ := ZCharRecArray[ArrayOffSet].Len; //inlcuding #0
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
                    ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              {we need a temporary storage -> we only reference lob pointers }
              TempBlob := TZAbstractCLob.CreateWithData(PWideChar(UniTemp), Length(UniTemp), ConSettings);
              TempLobs[TempLobOffSet][ArrayOffSet] := TempBlob;
              ProcessUnicode(Data, PLen, True, TempBlob.GetPWideChar, TempBlob.Length shr 1);
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
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZStringArray[ArrayOffSet])));
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
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          Pointer(ZUnicodeStringArray[ArrayOffSet]),
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, Length(ZUnicodeStringArray[ArrayOffSet])));
                        continue;
                      end;
                    vtCharRec:
                      if ZCompatibleCodePages(ZCharRecArray[ArrayOffSet].CP, zCP_UTF16) then
                      begin
                        ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                          ZCharRecArray[ArrayOffSet].P,
                          {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, ZCharRecArray[ArrayOffSet].Len));
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
                    ProcessUnicode(Data, PLen, (DBBindingArray[i].eParamIO and DBPARAMIO_OUTPUT = 0),
                      TempBlob.GetPWideChar,
                      {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1) -1, TempBlob.Length shr 1));
                    Continue;
                  end;
                else
                  raise Exception.Create('Unsupported AnsiString-Array Variant');
              end;
              ProcessUnicode(Data, PLen, False, Pointer(UniTemp),
                {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min((DBBindingArray[I].cbMaxLen shr 1)-1, Length(UniTemp)));
            end;
          DBTYPE_DBDATE:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeDate(DateTimeTemp, Year, PDBDate(Data)^.month, PDBDate(Data)^.day);
              PDBDate(Data)^.year := Year;
            end;
          DBTYPE_DBTIME:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeTime(DateTimeTemp, PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, MilliSecond);
            end;
          DBTYPE_DBTIME2:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeTime(DateTimeTemp,
                PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second,
                MilliSecond);
                PDBTIME2(Data)^.fraction := Millisecond * 1000000;
            end;
          DBTYPE_DBTIMESTAMP:
            begin
              case SQLType of
                stBoolean:    DateTimeTemp := Ord(ZBooleanArray[ArrayOffSet]);
                stByte:       DateTimeTemp := ZByteArray[ArrayOffSet];
                stShort:      DateTimeTemp := ZShortIntArray[ArrayOffSet];
                stWord:       DateTimeTemp := ZWordArray[ArrayOffSet];
                stSmall:      DateTimeTemp := ZSmallIntArray[ArrayOffSet];
                stLongWord:   DateTimeTemp := ZLongWordArray[ArrayOffSet];
                stInteger:    DateTimeTemp := ZIntegerArray[ArrayOffSet];
                stLong:       DateTimeTemp := ZInt64Array[ArrayOffSet];
                stULong:      DateTimeTemp := ZUInt64Array[ArrayOffSet];
                stFloat:      DateTimeTemp := ZSingleArray[ArrayOffSet];
                stDouble:     DateTimeTemp := ZDoubleArray[ArrayOffSet];
                stCurrency:   DateTimeTemp := ZCurrencyArray[ArrayOffSet];
                stBigDecimal: DateTimeTemp := ZExtendedArray[ArrayOffSet];
                stString, stUnicodeString:
                  case InParamValues[i].VArray.VArrayVariantType of
                    vtString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeString(ZStringArray[ArrayOffSet]));
                    vtAnsiString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeAnsiString(ZAnsiStringArray[ArrayOffSet]));
                    vtUTF8String:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUTF8String(ZUTF8StringArray[ArrayOffSet]));
                    vtRawByteString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[ArrayOffSet]));
                    vtUnicodeString:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[ArrayOffSet]));
                    vtCharRec:
                      DateTimeTemp := ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[ArrayOffSet]));
                    else
                      raise Exception.Create('Unsupported String Variant');
                  end;
                stTime, stDate, stTimeStamp:
                  DateTimeTemp := ZDateTimeArray[ArrayOffSet];
                else
                  raise EZSQLException.Create(IntToStr(Ord(SQLType))+' '+SUnsupportedParameterType);
              end;
              DecodeDate(DateTimeTemp, Year, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
              PDBTimeStamp(Data)^.year := Year;
              DecodeTime(DateTimeTemp, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MilliSecond);
              if SupportsMilliseconds then
                PDBTimeStamp(Data)^.fraction := MilliSecond * 1000*1000
              else
                PDBTimeStamp(Data)^.fraction := 0;
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
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
{$HINTS ON}

procedure SetOleCommandProperties(const Command: ICommandText; TimeOut: SmallInt;
  Provider: TZServerProvider; SupportsMARSConnection: Boolean; Prepare: Boolean);
var
  FCmdProps: ICommandProperties;
  rgCommonProperties: array[0..20] of TDBProp;
  rgProviderProperties: TDBProp;
  rgPropertySets: array[0..1] of TDBPROPSET;

  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
begin
  FCmdProps := nil; //init
  if Succeeded(Command.QueryInterface(IID_ICommandProperties, FCmdProps)) then
  begin
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
    rgPropertySets[0].cProperties     := 0; //init
    rgPropertySets[0].guidPropertySet := DBPROPSET_ROWSET;
    rgPropertySets[0].rgProperties    := @rgCommonProperties[0];
    rgPropertySets[1].cProperties     := 0;
    case Provider of
      spMSSQL: rgPropertySets[1].guidPropertySet := DBPROPSET_SQLSERVERROWSET
      else rgPropertySets[1].guidPropertySet := DBPROPSET_ROWSET;
    end;
    rgPropertySets[1].rgProperties    := @rgProviderProperties;

    SetProp(rgPropertySets[0], DBPROP_COMMANDTIMEOUT,    Max(0, TimeOut)); //Set command time_out static!
    SetProp(rgPropertySets[0], DBPROP_SERVERCURSOR,      VARIANT_TRUE); //force a server side cursor
    if (Provider = spMSSQL) then
    begin
      //turn off deferred prepare -> raise exception on Prepare if command can't be executed!
      //http://msdn.microsoft.com/de-de/library/ms130779.aspx
      if Prepare then
        SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_FALSE)
      else
        SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_TRUE);
    end else begin
      //to avoid http://support.microsoft.com/kb/272358/de we need a
      //FAST_FORWARD(RO) server cursor
      {common sets which are NOT default: according the cursor models of
      http://msdn.microsoft.com/de-de/library/ms130840.aspx }
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,        VARIANT_FALSE);
      if SupportsMARSConnection then begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_FALSE);
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_FALSE);
      end else begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
      end;
      SetProp(rgPropertySets[0], DBPROP_OTHERINSERT,       VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_OTHERUPDATEDELETE, VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,         VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANFETCHBACKWARDS,  VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANSCROLLBACKWARDS, VARIANT_FALSE);
    end;
    try
      OleDBCheck(FCmdProps.SetProperties(2,@rgPropertySets[0]));
    finally
      FCmdProps := nil;
    end;
  end;
end;

function ProviderNamePrefix2ServerProvider(const ProviderNamePrefix: String): TZServerProvider;
type
  TDriverNameAndServerProvider = record
    ProviderNamePrefix: String;
    Provider: TZServerProvider;
  end;
const
  KnownDriverName2TypeMap: array[0..12] of TDriverNameAndServerProvider = (
    (ProviderNamePrefix: 'ORAOLEDB';      Provider: spOracle),
    (ProviderNamePrefix: 'MSDAORA';       Provider: spOracle),
    (ProviderNamePrefix: 'SQLNCLI';       Provider: spMSSQL),
    (ProviderNamePrefix: 'SQLOLEDB';      Provider: spMSSQL),
    (ProviderNamePrefix: 'SSISOLEDB';     Provider: spMSSQL),
    (ProviderNamePrefix: 'MSDASQL';       Provider: spMSSQL), //??
    (ProviderNamePrefix: 'MYSQLPROV';     Provider: spMySQL),
    (ProviderNamePrefix: 'IBMDA400';      Provider: spAS400),
    (ProviderNamePrefix: 'IFXOLEDBC';     Provider: spInformix),
    (ProviderNamePrefix: 'MICROSOFT.JET.OLEDB'; Provider: spMSJet),
    (ProviderNamePrefix: 'IB';            Provider: spIB_FB),
    (ProviderNamePrefix: 'POSTGRESSQL';   Provider: spPostgreSQL),
    (ProviderNamePrefix: 'CUBRID';        Provider: spCUBRID)
    );
var
  I: Integer;
  ProviderNamePrefixUp: string;
begin
  Result := spMSSQL;
  ProviderNamePrefixUp := UpperCase(ProviderNamePrefix);
  for i := low(KnownDriverName2TypeMap) to high(KnownDriverName2TypeMap) do
    if StartsWith(ProviderNamePrefixUp, KnownDriverName2TypeMap[i].ProviderNamePrefix) then begin
      Result := KnownDriverName2TypeMap[i].Provider;
      Break;
    end;
end;

end.
