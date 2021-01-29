{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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

{$IF defined(ZEOS_DISABLE_OLEDB) and defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_OLEDB_UTILS}//if set we have an empty unit
{$IFEND}

{$IFNDEF ZEOS_DISABLE_OLEDB_UTILS} //if set we have an empty unit
uses
  Types, SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} FmtBCD,
  ZCompatibility, ZDbcIntfs, ZPlainOleDBDriver, ZVariant, ZDbcStatement, Variants;

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
  ZVARIANT_TRUE = SmallInt(-1);
  ZVARIANT_FALSE = SmallInt(0);

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM; IsLong: Boolean;
  Scale, Precision: Integer): TZSQLType; overload;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  const SrcRS: IZResultSet): TZSQLType; overload;

{**
  Brings up the OleDB connection string builder dialog.
}
function PromptDataSource(Handle: THandle; const InitialString: WideString): WideString;

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; ParamInfoArray: PDBParamInfoArray): DBROWOFFSET;

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO{;
  var LobColIndexArray: TIntegerDynArray}): DBROWOFFSET;

function MapOleTypesToZeos(DBType: DBTYPEENUM; Precision, Scale: Integer): DBTYPE;

/// <summary>Try to determine the server-proverder by given Provider-Name</summary>
/// <returns>the best knonw Server-proveider</returns>
function ProviderNamePrefix2ServerProvider(const ProviderNamePrefix: String): TZServerProvider;

const SQLType2OleDBTypeEnum: array[TZSQLType] of DBTYPEENUM = (DBTYPE_NULL,
  DBTYPE_BOOL,
  DBTYPE_UI1, DBTYPE_I1, DBTYPE_UI2, DBTYPE_I2, DBTYPE_UI4, DBTYPE_I4,
  DBTYPE_UI8, DBTYPE_I8, DBTYPE_R4, DBTYPE_R8, DBTYPE_CY, DBTYPE_NUMERIC,
  DBTYPE_DBDATE, DBTYPE_DBTIME2, DBTYPE_DATE,
  DBTYPE_GUID, DBTYPE_WSTR, DBTYPE_WSTR, DBTYPE_BYTES,
  DBTYPE_WSTR, DBTYPE_WSTR, DBTYPE_BYTES, DBTYPE_NULL, DBTYPE_TABLE);

const ParamType2OleIO: array[TZProcedureColumnType] of DBTYPEENUM = (
  DBPARAMIO_INPUT{pctUnknown}, DBPARAMIO_INPUT{pctIn}, DBPARAMIO_INPUTOUTPUT{pctInOut},
    DBPARAMIO_OUTPUT{pctOut}, DBPARAMIO_OUTPUT{pctReturn}, DBPARAMIO_OUTPUT{pctResultSet});

{$ENDIF ZEOS_DISABLE_OLEDB_UTILS} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB_UTILS} //if set we have an empty unit

uses
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ActiveX, Windows, Math, TypInfo,
  ZEncoding, ZDbcLogging, ZDbcUtils, ZDbcResultSet, ZFastCode, ZSysUtils, ZMessages,
  ZClasses;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM; IsLong: Boolean;
  Scale, Precision: Integer): TZSQLType;
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
    DBTYPE_BSTR:        if IsLong
                        then Result := stUnicodeStream
                        else Result := stUnicodeString;
    DBTYPE_ERROR:       Result := stInteger;
    DBTYPE_BOOL:        Result := stBoolean;
    DBTYPE_VARIANT:     Result := stString;
    DBTYPE_IUNKNOWN:    Result := stUnknown; //note this could be used to bind IStream for reading/writing data
    DBTYPE_VARNUMERIC,
    DBTYPE_NUMERIC,
    DBTYPE_DECIMAL:     if (Scale <= 4) and (Precision < sAlignCurrencyScale2Precision[Scale])
                        then Result := stCurrency
                        else Result := stBigDecimal;
    DBTYPE_UI1:         Result := stByte;
    DBTYPE_I1:          Result := stShort;
    DBTYPE_UI2:         Result := stWord;
    DBTYPE_UI4:         Result := stLongWord;
    DBTYPE_I8:          Result := stLong;
    DBTYPE_UI8:         Result := stULong;
    DBTYPE_GUID:        Result := stGUID;
    DBTYPE_BYTES:       if IsLong
                        then Result := stBinaryStream
                        else Result := stBytes;
    DBTYPE_STR:         if IsLong
                        then Result := stAsciiStream
                        else Result := stString;
    DBTYPE_WSTR:        if IsLong
                        then Result := stUnicodeStream
                        else Result := stUnicodeString;
    DBTYPE_UDT:         Result := stUnknown;
    DBTYPE_DBDATE:      Result := stDate;
    DBTYPE_DBTIME,
    DBTYPE_DBTIME2:     Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_XML:         Result := stUnicodeStream;
    DBTYPE_TABLE:       Result := stResultSet;
    else //makes compiler happy
      {
      DBTYPE_IDISPATCH:
      DBTYPE_HCHAPTER:    }Result := stUnknown;
  end;
end;

function ConvertOleDBTypeToSQLType(OleDBType: DBTYPEENUM;
  const SrcRS: IZResultSet): TZSQLType; overload;
const LongNames: array [0..8] of UnicodeString = ('TEXT', 'NTEXT', 'MEDIUMTEXT',
  'LONGTEXT', 'CLOB', 'BLOB', 'MEDIUMBLOB', 'LONGBLOB', 'IMAGE');
function IsLong: Boolean;
var I: Integer;
  Uni: UnicodeString;
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
var Scale, I: Integer;
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
    DBTYPE_BSTR:      if IsLong
                      then Result := stUnicodeStream
                      else Result := stUnicodeString;
    DBTYPE_ERROR:     Result := stInteger;
    DBTYPE_BOOL:      Result := stBoolean;
    DBTYPE_VARIANT:   Result := stString;
    DBTYPE_IUNKNOWN:  Result := stUnknown; //note this could be used to bind IStream for reading/writing data
    DBTYPE_NUMERIC,
    DBTYPE_VARNUMERIC,
    DBTYPE_DECIMAL:   begin
                        i := SrcRS.FindColumn('NUMERIC_SCALE');
                        if i <> InvalidDbcIndex
                        then Scale := SrcRS.GetInt(i)
                        else Scale := 10;
                        I := SrcRS.FindColumn('NUMERIC_PRECISION');
                        if (i <> InvalidDbcIndex) and (Scale <= 4) and (SrcRS.GetInt(I) < sAlignCurrencyScale2Precision[Scale])
                        then Result := stCurrency
                        else Result := stBigDecimal;
                      end;
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
    DBTYPE_WSTR:      if IsLong then Result := stUnicodeStream
                      else Result := stUnicodeString;
    DBTYPE_UDT:         Result := stUnknown;
    DBTYPE_DBDATE:      Result := stDate;
    DBTYPE_DBTIME,
    DBTYPE_DBTIME2: Result := stTime;
    DBTYPE_DBTIMESTAMP:	Result := stTimeStamp;
    DBTYPE_FILETIME:    Result := stTimeStamp;
    DBTYPE_PROPVARIANT: Result := stString;
    DBTYPE_XML:         Result := stAsciiStream;
    DBTYPE_TABLE:       Result := stResultSet;
    else //makes compiler happy
      {
      DBTYPE_IDISPATCH:
      DBTYPE_HCHAPTER:    }Result := stUnknown;
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

function MapOleTypesToZeos(DBType: DBTYPEENUM; Precision, Scale: Integer): DBTYPE;
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
    DBTYPE_STR: Result := DBTYPE_WSTR;  //if we would know the server-codepage ... we could decrease mem
    DBTYPE_DECIMAL,
    DBTYPE_NUMERIC,
    DBTYPE_VARNUMERIC: if (Scale <= 4) and (Precision < sAlignCurrencyScale2Precision[Scale])
            then Result := DBTYPE_CY
            else Result := DBTYPE_NUMERIC;
    //DBTYPE_UDT	= 132;
    //DBTYPE_HCHAPTER	= 136;
    DBTYPE_FILETIME: Result := DBTYPE_DATE;
    //DBTYPE_PROPVARIANT	= 138;
    //DBTYPE_DBTIME2: Result := DBTYPE_DBTIME2;
    DBTYPE_XML:     Result := DBTYPE_WSTR;
    DBTYPE_DBTIMESTAMPOFFSET: Result := DBTYPE_DBTIMESTAMP;
   // DBTYPE_TABLE;
  end;
end;

function PrepareOleParamDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; ParamInfoArray: PDBParamInfoArray): DBROWOFFSET;
var
  I: Integer;
  Procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := ParamInfoArray^[Index].iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(ParamInfoArray^[Index].wType, ParamInfoArray^[Index].bPrecision, ParamInfoArray^[Index].bScale);
    if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISLONG <> 0) then begin//lob's
      { cbMaxLen returns max allowed bytes for Lob's which depends to server settings.
       So rowsize could have a overflow. In all cases we need to use references
       OR introduce DBTYPE_IUNKNOWN by using a IPersistStream/ISequentialStream/IStream see:
       http://msdn.microsoft.com/en-us/library/windows/desktop/ms709690%28v=vs.85%29.aspx }
      DBBindingArray[Index].cbMaxLen := SizeOf(Pointer);
      { now let's decide if we can use direct references or need space in buffer
        and a reference or if we need a external object for lob's}
      if (ParamInfoArray^[Index].dwFlags and DBPARAMFLAGS_ISOUTPUT <> 0) then
        raise Exception.Create('RESULT/OUT/INOUT Parameter for LOB''s are currently not supported!');
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      DBBindingArray[Index].wType   := DBBindingArray[Index].wType or DBTYPE_BYREF; //indicate we address a buffer
      DBBindingArray[Index].dwPart  := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
    end else begin
      { all other types propably fit into one RowSize-Buffer }
      if DBBindingArray[Index].wType in [DBTYPE_VARNUMERIC, DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR] then begin
        {for all these types we reserve a pointer and the buffer-memory, if we need it or not!
         this catches possible conversion later on. So we can either directly address or
         point to the buffer after the pointer where a converted value was moved in (:
         This may waste mem but makes everything flexible like a charm!}
         //all these types including GUID need a reference pointer except we do not play with multiple row binding
        DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
        DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS; //we need a length indicator for vary data only
        if DBBindingArray[Index].wType = DBTYPE_STR then
          DBBindingArray[Index].cbMaxLen := ParamInfoArray^[Index].ulParamSize +1
        else if DBBindingArray[Index].wType = DBTYPE_WSTR then
          DBBindingArray[Index].cbMaxLen := ((ParamInfoArray^[Index].ulParamSize +1) shl 1)
        else
          DBBindingArray[Index].cbMaxLen := ParamInfoArray^[Index].ulParamSize;
      end else begin { fixed size types do not need a length indicator }
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
  SetLength(DBBindingArray, DBUPARAMS);

  DBBindingArray[0].obStatus := 0;
  SetDBBindingProps(0);
  for i := 1 to DBUPARAMS -1 do begin
    DBBindingArray[i].obStatus := DBBindingArray[i-1].obValue  + DBBindingArray[i-1].cbMaxLen;
    SetDBBindingProps(I);
  end;
  Result := DBBindingArray[DBUPARAMS -1].obValue + DBBindingArray[DBUPARAMS -1].cbMaxLen;
end;

function PrepareOleColumnDBBindings(DBUPARAMS: DB_UPARAMS;
  var DBBindingArray: TDBBindingDynArray; DBCOLUMNINFO: PDBCOLUMNINFO{;
  var LobColIndexArray: TIntegerDynArray}): DBROWOFFSET;
var
  I: Integer;
  procedure SetDBBindingProps(Index: Integer);
  begin
    //type indicators
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms711251%28v=vs.85%29.aspx
    DBBindingArray[Index].iOrdinal := DBCOLUMNINFO^.iOrdinal;
    DBBindingArray[Index].obLength := DBBindingArray[Index].obStatus + SizeOf(DBSTATUS);
    DBBindingArray[Index].wType := MapOleTypesToZeos(DBCOLUMNINFO^.wType, DBCOLUMNINFO^.bPrecision, DBCOLUMNINFO^.bScale);
    if (DBCOLUMNINFO^.dwFlags and DBPARAMFLAGS_ISLONG <> 0) then begin //lob's
      //using ISeqentialStream -> Retrieve data directly from Provider
      DBBindingArray[Index].cbMaxLen  := 0;
      DBBindingArray[Index].dwPart    := DBPART_LENGTH or DBPART_STATUS;
      DBBindingArray[Index].wType     := DBCOLUMNINFO^.wType; //Save the wType to know Binary/Ansi/Unicode-Lob's later on
      DBBindingArray[Index].obValue   := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      //DBBindingArray[Index].dwFlags   := DBCOLUMNFLAGS_ISLONG; //indicate long values! <- trouble with SQLNCLI11 provider!
    end else if DBBindingArray[Index].wType in [DBTYPE_VARNUMERIC, DBTYPE_BYTES, DBTYPE_STR, DBTYPE_WSTR, DBTYPE_BSTR] then begin
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength + SizeOf(DBLENGTH);
      DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
      if DBBindingArray[Index].wType = DBTYPE_STR then
        DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize +1
      else if DBBindingArray[Index].wType in [DBTYPE_WSTR, DBTYPE_BSTR] then begin
        DBBindingArray[Index].wType := DBTYPE_WSTR;
        DBBindingArray[Index].cbMaxLen := (DBCOLUMNINFO^.ulColumnSize+1) shl 1
      end else begin
        DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize;
        if DBBindingArray[Index].wType = DBTYPE_VARNUMERIC then begin
          DBBindingArray[Index].bPrecision := DBCOLUMNINFO^.bPrecision;
          DBBindingArray[Index].bScale := DBCOLUMNINFO^.bScale;
        end;
      end;
      //8Byte Alignment and optimized Accessor(fetch) does NOT  work if:
      //fixed width fields came to shove!!!!
      //DBBindingArray[Index].cbMaxLen := ((DBBindingArray[Index].cbMaxLen-1) shr 3+1) shl 3;
      {if (DBCOLUMNINFO^.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0) then //vary
        DBBindingArray[Index].cbMaxLen := ((DBBindingArray[Index].cbMaxLen-1) shr 3+1) shl 3
      else}
      if (DBCOLUMNINFO^.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0) then //fixed length ' ' padded?
        DBBindingArray[Index].dwFlags := DBCOLUMNFLAGS_ISFIXEDLENGTH;//keep this flag alive! We need it for conversions of the RS's
    end else begin { fixed types do not need a length indicator }
      if DBBindingArray[Index].wType = DBTYPE_DBTIME2 then
        DBBindingArray[Index].dwFlags := DBCOLUMNINFO^.dwFlags; //keep it!
      if (DBCOLUMNINFO^.wType in [DBTYPE_DECIMAL, DBTYPE_NUMERIC]) and
        (DBBindingArray[Index].wType = DBTYPE_CY)
      then DBBindingArray[Index].cbMaxLen := SizeOf(Currency)
      else DBBindingArray[Index].cbMaxLen := DBCOLUMNINFO^.ulColumnSize;
      DBBindingArray[Index].bPrecision := DBCOLUMNINFO^.bPrecision;
      DBBindingArray[Index].bScale := DBCOLUMNINFO^.bScale;
      DBBindingArray[Index].obValue := DBBindingArray[Index].obLength;
      DBBindingArray[Index].dwPart := DBPART_VALUE or DBPART_STATUS;
    end;
    DBBindingArray[Index].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    DBBindingArray[Index].eParamIO := DBPARAMIO_NOTPARAM;
    //makes trouble !!DBBindingArray[Index].dwFlags :=  DBCOLUMNINFO^.dwFlags; //set found flags to indicate long types too
  end;
begin
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
{$HINTS OFF}

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
    //(ProviderNamePrefix: 'MSDASQL';       Provider: spMSSQL), ODBC proivider for everything
    (ProviderNamePrefix: 'MYSQLPROV';     Provider: spMySQL),
    (ProviderNamePrefix: 'IBMDA400';      Provider: spAS400),
    (ProviderNamePrefix: 'IFXOLEDBC';     Provider: spInformix),
    (ProviderNamePrefix: 'MICROSOFT.JET.OLEDB'; Provider: spMSJet),
    (ProviderNamePrefix: 'MICROSOFT.ACE'; Provider: spMSJet),
    (ProviderNamePrefix: 'IB';            Provider: spIB_FB),
    (ProviderNamePrefix: 'POSTGRESSQL';   Provider: spPostgreSQL),
    (ProviderNamePrefix: 'CUBRID';        Provider: spCUBRID)
    );
var
  I: Integer;
  ProviderNamePrefixUp: string;
begin
  Result := spUnknown;
  ProviderNamePrefixUp := UpperCase(ProviderNamePrefix);
  for i := low(KnownDriverName2TypeMap) to high(KnownDriverName2TypeMap) do
    if StartsWith(ProviderNamePrefixUp, KnownDriverName2TypeMap[i].ProviderNamePrefix) then begin
      Result := KnownDriverName2TypeMap[i].Provider;
      Break;
    end;
end;

{$ENDIF ZEOS_DISABLE_OLEDB_UTILS} //if set we have an empty unit
end.
