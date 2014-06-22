{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

unit ZDbcOracleStatement;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IFDEF MSWINDOWS}{%H-}Windows,{$ENDIF}
  ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainOracleDriver,
  ZCompatibility, ZVariant, ZDbcOracleUtils, ZPlainOracleConstants;

type

  {** Implements Prepared SQL Statement. }

  { TZOraclePreparedStatement }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FParams: PZSQLVars;
    FRowPrefetchSize: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FServerStmtCache: Boolean;
    FConnectionHandle: POCIEnv;
    FContextHandle: POCISvcCtx;
    FBuffer: TByteDynArray; { holds all data for bindings }
    {some temporary array for array bindings}
    FNullIndicators: array of array of sb2;
    FLengthIndicators: array of array of ub2;
    FDescriptors: TDesciptorRecArray;
    FIntegerValues: array of TIntegerDynArray;
    FInt64Values: array of TInt64DynArray;
    FDoubleValues: Array of TDoubleDynArray;
    function ConvertToOracleSQLQuery: RawByteString;
    function CreateResultSet: IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; Info: TStrings); overload;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZOracleStatement = class(TZAbstractPreparedStatement);


  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FOutParamCount: Integer;
    FErrorHandle: POCIError;
    FParams: PZSQLVars;
    FPlainDriver:IZOraclePlainDriver;
    FHandle: POCIStmt;
    FOracleParams: TZOracleParams;
    FOracleParamsCount: Integer;
    FParamNames: TStringDynArray;
    PackageIncludedList: TStrings;
    FConnectionHandle: POCIEnv;
    FBuffer: TByteDynArray;
    FRowPrefetchSize: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FIteration: Integer;
    procedure ArrangeInParams;
    procedure FetchOutParamsFromOracleVars;
    function GetProcedureSql(SelectProc: boolean): RawByteString;
  protected
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      ParamTypeName: String; const ParamName: String; Const {%H-}ColumnSize, {%H-}Precision: Integer);
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); override;
    procedure RegisterParamType(ParameterIndex: integer; ParamType: Integer); override;
    procedure Prepare; override;
    procedure Unprepare; override;

    Function ExecuteUpdatePrepared: Integer; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    constructor Create(Connection: IZConnection; const pProcName: string; Info: TStrings);
    destructor Destroy; override;
    procedure ClearParameters; override;
  end;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZDbcOracle, ZDbcOracleResultSet,
  ZEncoding, ZDbcUtils;

{ TZOraclePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOraclePreparedStatement.Create(
  PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  ASQL := ConvertToOracleSQLQuery;
  FRowPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'row_prefetch_size', ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072);
  FConnectionHandle := (Connection as IZOracleConnection).GetConnectionHandle;
  FContextHandle := (Connection as IZOracleConnection).GetContextHandle;
end;

constructor TZOraclePreparedStatement.Create(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  Create(PlainDriver, Connection, '', Info);
end;

{**
  Converts an SQL query into Oracle format.
  @param SQL a query with parameters defined with '?'
  @returns a query with parameters in Oracle format ':pN'.
}
function TZOraclePreparedStatement.ConvertToOracleSQLQuery: RawByteString;
var
  I, N: Integer;
  SelectFound: Boolean;
begin
  FServerStmtCache := False;
  SelectFound := False;
  N := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
  begin
    SelectFound := (I = 0) and (AnsiUpperCase(CachedQueryRaw[i]) = 'SELECT');
    if IsParamIndex[i] then
    begin
      FServerStmtCache := True;
      Inc(N);
      Result := Result + ':P' + IntToRaw(N);
    end else
    begin
      if SelectFound and not FServerStmtCache then
        SelectFound := AnsiUpperCase(CachedQueryRaw[i]) <> 'WHERE';
      Result := Result + CachedQueryRaw[i];
    end;
  end;
  FServerStmtCache := SelectFound or FServerStmtCache;
  {$IFNDEF UNICODE}
  if ConSettings^.AutoEncode then
     Result := GetConnection.GetDriver.GetTokenizer.GetEscapeString(Result);
  {$ENDIF}
end;

function TZOraclePreparedStatement.CreateResultSet: IZResultSet;
begin
  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  Result := CreateOracleResultSet(FPlainDriver, Self, SQL, FHandle, FErrorHandle, FZBufferSize);
  FOpenResultSet := Pointer(Result);
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.PrepareInParameters;
begin
  AllocateOracleSQLVars(FParams, InParamCount);
end;

{**
  Binds the input parameters
}
procedure TZOraclePreparedStatement.BindInParameters;
var
  I, J, Iteration: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  CharRec: TZCharRec;
  OCIData: Pointer;
  DataEntry: PAnsiChar; //nice to increment positions
  {using mem entry of OCIData is faster then casting}
  BooleanArray: TBooleanDynArray absolute OCIData;
  ByteArray: TByteDynArray absolute OCIData;
  ShortIntArray: TShortIntDynArray absolute OCIData;
  WordArray: TWordDynArray absolute OCIData;
  SmallIntArray: TSmallIntDynArray absolute OCIData;
  LongWordArray: TLongWordDynArray absolute OCIData;
  IntegerArray: TIntegerDynArray absolute OCIData;
  Int64Array: TInt64DynArray absolute OCIData;
  UInt64Array: TUInt64DynArray absolute OCIData;
  SingleArray: TSingleDynArray absolute OCIData;
  DoubleArray: TDoubleDynArray absolute OCIData;
  CurrencyArray: TCurrencyDynArray absolute OCIData;
  ExtendedArray: TExtendedDynArray absolute OCIData;
  RawByteStringArray: TRawByteStringDynArray absolute OCIData;
  UnicodeStringArray: TUnicodeStringDynArray absolute OCIData;
  CharRecArray: TZCharRecDynArray absolute OCIData;
  PointerArray: TPointerDynArray absolute OCIData;
  InterfaceArray: TInterfaceDynArray absolute OCIData;

  TempRawByteStringArray: TRawByteStringDynArray;

  PNullIndicator: Pointer;
  NullIndicator: array of sb2 absolute PNullIndicator;
  PLengthIndicator: Pointer;
  LengthIndicator: array of ub2 absolute PLengthIndicator;

  ZData: Pointer;
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
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

  ZVariant: TZVariant;
  BufferSize: Int64;
  TmpStrLen: Cardinal;
  WideRec: TZWideRec;
  AnsiRec: TZAnsiRec;
  WS: ZWideString; //temp val to avoid overrun if WideString(com based)
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  WriteTempBlob: IZOracleBlob;
  TempBlob: IZBlob;
  OracleConnection: IZOracleConnection;
  LobBuffer: Pointer;
  CharBuffer: PAnsiChar;
  AnsiTemp: RawByteString;
  Label SkipStringAssembling, StringAssembling;

begin
  if (InParamCount > 0) and (InParamValues[0].VType = vtArray) then
  begin
    { array DML binding}
    BufferSize := 0;
    OracleConnection := Connection as IZOracleConnection;
    SetLength(FNullIndicators,InParamCount);
    SetLength(FLengthIndicators, InParamCount);
    SetLength(FDescriptors, InParamCount);
    SetLength(FIntegerValues, InParamCount);
    SetLength(FInt64Values, InParamCount);
    SetLength(FDoubleValues, InParamCount);
    SetLength(TempRawByteStringArray, ArrayCount);
    for i := 0 to InParamCount -1 do
      SetLength(FNullIndicators[i], ArrayCount);
    for I := 0 to InParamCount - 1 do
    begin
      CurrentVar := @FParams.Variables[I];
      CurrentVar.Handle := nil;

      ZVariant := InParamValues[i];
      {build null indicators first}
      PNullIndicator := FNullIndicators[i];
      PLengthIndicator := nil;
      OCIData := ZVariant.VArray.VIsNullArray; //temp the pointer to get the absolute arrays of X
      if ZVariant.VArray.VArray = nil then //no data assigned so let's set all to null
        for J := 0 to ArrayCount -1 do
          NullIndicator[j] := -1
      else
        if ZVariant.VArray.VIsNullArray = nil then //no null values.. So let's build the indicators as not null
          for J := 0 to ArrayCount -1 do
            NullIndicator[j] := 0
        else
          for J := 0 to ArrayCount -1 do
            case TZSQLType(ZVariant.VArray.VIsNullArrayType) of
              stBoolean:
                NullIndicator[j] := -Ord(BooleanArray[J]);
              stByte:
                if ByteArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stShort:
                if ShortIntArray[j] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stWord:
                if WordArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stSmall:
                if SmallIntArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stLongWord:
                if LongWordArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stInteger:
                if IntegerArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stULong:
                if UInt64Array[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stLong:
                if Int64Array[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stFloat:
                if SingleArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stDouble:
                if DoubleArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stCurrency:
                if CurrencyArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stBigDecimal:
                if ExtendedArray[J] = 0 then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
              stUnicodeString:
                if ZVariant.VArray.VIsNullArrayVariantType = vtUnicodeString then
                  if StrToBoolEx(UnicodeStringArray[J], True) = False then
                    NullIndicator[j] := 0 //not null
                  else
                    NullIndicator[j] := -1 //null
                else
                  if StrToBoolEx(PWideChar(CharRecArray[J].P), True) = False then
                    NullIndicator[j] := 0 //not null
                  else
                    NullIndicator[j] := -1; //null
              stString:
                if ZVariant.VArray.VIsNullArrayVariantType = vtCharRec then
                  if StrToBoolEx(PAnsiChar(CharRecArray[J].P), True) = False then
                    NullIndicator[j] := 0 //not null
                  else
                    NullIndicator[j] := -1 //null
                else
                  if StrToBoolEx(RawByteStringArray[J], True) = False then
                    NullIndicator[j] := 0 //not null
                  else
                    NullIndicator[j] := -1; //null
              stAsciiStream, stUnicodeStream, stBinaryStream:
                if (InterfaceArray[J] <> nil) or not (InterfaceArray[J] as IZBLob).IsEmpty then
                  NullIndicator[j] := 0 //not null
                else
                  NullIndicator[j] := -1; //null
          end;
      ZData := ZVariant.VArray.VArray;
      if ZData <> nil then
        case InParamTypes[I] of
          stBoolean: //Oracle doesn't support booleans so lets use integers and OCI converts it..
            begin
              if (FIntegerValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FIntegerValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FIntegerValues[I], ArrayCount);
              OCIData := FIntegerValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                IntegerArray[J] := Ord(ZBooleanArray[j]); //convert Boolean to integer
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stByte:
            begin
              if (FIntegerValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FIntegerValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FIntegerValues[I], ArrayCount);
              OCIData := FIntegerValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                IntegerArray[J] := ZByteArray[j]; //convert byte to integer
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stShort:
            begin
              if (FIntegerValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FIntegerValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FIntegerValues[I], ArrayCount);
              OCIData := FIntegerValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                IntegerArray[J] := ZShortIntArray[j]; //convert shortint to integer
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stWord:
            begin
              if (FIntegerValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FIntegerValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FIntegerValues[I], ArrayCount);
              OCIData := FIntegerValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                IntegerArray[J] := ZWordArray[j]; //convert shortint to integer
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stSmall:
            begin
              if (FIntegerValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FIntegerValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FIntegerValues[I], ArrayCount);
              OCIData := FIntegerValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                IntegerArray[J] := ZSmallIntArray[j]; //convert shortint to integer
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stLongWord:
            //since 11.2 we can use Int64 types too
            if Connection.GetClientVersion >= 11002000 then
            begin
              if (FInt64Values[I] = nil) or ({%H-}PLongInt(NativeUInt(FInt64Values[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FInt64Values[I], ArrayCount);
              OCIData := FInt64Values[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Int64)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                Int64Array[J] := ZLongWordArray[j]; //convert LongWord to Int64
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_INT;
            end
            else
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZLongWordArray[j]; //convert LongWord to double
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stInteger: { no conversion required }
            begin
              OCIData := ZData;
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Integer)); //Count mem to avoid overrun of OCI limit of 4GB-1
              CurrentVar.Length := 4;
              CurrentVar.TypeCode := SQLT_INT;
            end;
          stULong: //conversion required below 11.2
            //since 11.2 we can use Int64 types too
            if Connection.GetClientVersion >= 11002000 then
            begin
              if (FInt64Values[I] = nil) or ({%H-}PLongInt(NativeUInt(FInt64Values[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FInt64Values[I], ArrayCount);
              OCIData := FInt64Values[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Int64)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                Int64Array[J] := ZUInt64Array[j]; //convert UInt64 to Double. Range???? Better to use strings?
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_INT;
            end
            else
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZUInt64Array[j]; //convert UInt64 to double. Range?????? Better to use strings?
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stLong: //conversion required below 11.2
            //since 11.2 we can use Int64 types too
            if Connection.GetClientVersion >= 11002000 then
            begin
              OCIData := ZData;
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Int64)); //Count mem to avoid overrun of OCI limit of 4GB-1
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_INT;
            end
            else
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZInt64Array[j]; //convert UInt64 to double. Range???? Better to use strings?
              CurrentVar.Length := 8;
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stFloat: //conversion required
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZSingleArray[j]; //convert UInt64 to double. Range?????? Better to use strings?
              CurrentVar.Length := SizeOf(Double);
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stDouble: //no conversion required
            begin
              OCIData := ZData;
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              CurrentVar.Length := SizeOf(Double);
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stCurrency: //conversion required
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZCurrencyArray[j]; //convert Currency to double.
              CurrentVar.Length := SizeOf(Double);
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stBigDecimal: //conversion required
            begin
              if (FDoubleValues[I] = nil) or ({%H-}PLongInt(NativeUInt(FDoubleValues[I]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDoubleValues[I], ArrayCount);
              OCIData := FDoubleValues[I];
              {%H-}Inc(BufferSize, ArrayCount * SizeOf(Double)); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                DoubleArray[J] := ZExtendedArray[j]; //convert Extended to double. Range?????? Better to use strings?
              CurrentVar.Length := SizeOf(Double);
              CurrentVar.TypeCode := SQLT_FLT;
            end;
          stString:
            begin
              if (FLengthIndicators[i] = nil ) or ({%H-}PLongInt(NativeUInt(FLengthIndicators[i]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FLengthIndicators[i], ArrayCount);
              PLengthIndicator := FLengthIndicators[i];
              case TZVariantType(ZVariant.VArray.VArrayVariantType) of
                vtString:
                  for j := 0 to ArrayCount -1 do
                    if NullIndicator[j] = 0 then //not NULL
                      if Pointer(ZStringArray[j]) = nil then //Length = 0
                      begin
                        //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                        NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                      end
                      else
                      begin
                        TempRawByteStringArray[j] := ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[j], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP); //conversion possible or just an inc of RefCount? ):
                        TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                        LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                        CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                      end
                    else
                      LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                vtAnsiString:
                  for j := 0 to ArrayCount -1 do
                    if NullIndicator[j] = 0 then //not NULL
                      if Pointer(ZAnsiStringArray[j]) = nil then //Length = 0
                      begin
                        //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                        NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                      end
                      else
                      begin
                        TempRawByteStringArray[j] := ConSettings^.ConvFuncs.ZAnsiToRaw(ZAnsiStringArray[j], ConSettings^.ClientCodePage^.CP);
                        TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                        LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                        CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                      end
                    else
                      LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                vtUTF8String:
                  if ZCompatibleCodePages(ZCharRecArray[0].CP, ConSettings^.ClientCodePage^.CP) then
                    for j := 0 to ArrayCount -1 do
                      if NullIndicator[j] = 0 then //not NULL
                        if Pointer(ZUTF8StringArray[j]) = nil then //Length = 0
                        begin
                          //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                          NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                          LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                        end
                        else
                        begin
                          TempRawByteStringArray[j] := ZUTF8StringArray[j];//virtually no move, just inc refcount of string
                          TmpStrLen := {%H-}PLongInt(NativeInt(ZUTF8StringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                          LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                          CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                        end
                      else
                        LengthIndicator[J] := 0 //help OCI by giving the length so they can move instead of using StrPLCopy
                  else
                    for j := 0 to ArrayCount -1 do
                      if NullIndicator[j] = 0 then //not NULL
                        if Pointer(ZUTF8StringArray[j]) = nil then //Length = 0
                        begin
                          //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                          NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                          LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                        end
                        else
                        begin
                          TempRawByteStringArray[j] := ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[j], ConSettings^.ClientCodePage^.CP);
                          TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                          LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                          CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                        end
                      else
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                vtRawByteString:
                  for j := 0 to ArrayCount -1 do
                    if NullIndicator[j] = 0 then //not NULL
                    begin
                      if Pointer(ZRawByteStringArray[j]) = nil then //Length = 0
                      begin
                        //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                        NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                      end
                      else
                      begin
                        TmpStrLen := {%H-}PLongInt(NativeInt(ZRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                        TempRawByteStringArray[j] := ZRawByteStringArray[j]; //virtually no move, just inc refcount of string
                        LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                        CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                      end
                    end
                    else
                      LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                vtCharRec:
                  {in array bindings we assume all codepages are equal!}
                  if ZCompatibleCodePages(ZCharRecArray[0].CP, ConSettings^.ClientCodePage^.CP) then
                  begin
                    //let's avoid localized strings -> use faster way Zeos way (direct addessing of RowBuffer f.e.)
                    for j := 0 to ArrayCount -1 do
                      if NullIndicator[j] = 0 then //not NULL
                        if ZCharRecArray[j].Len = 0 then
                        begin
                          //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                          NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                          LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                        end
                        else
                        begin
                          LengthIndicator[J] := ZCharRecArray[j].Len; //help OCI by giving the length so they can move instead of using StrPLCopy
                          CurrentVar.Length := Max(CurrentVar.Length, LengthIndicator[J]);//OCI expects equal mem blocks array[xx][y] including trailing #0
                        end
                      else
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                    {now build the huge string including the #0's because OCI expects it}
                    Inc(CurrentVar.Length); //Left space for trailing #0
                    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
                    System.SetLength(InParamValues[I].VRawByteString, CurrentVar.Length*ArrayCount); //alloc mem as static blocks
                    {$ELSE}
                    System.SetString(InParamValues[I].VRawByteString, nil, Integer(CurrentVar.Length)*ArrayCount); //alloc mem as static blocks
                    {$ENDIF}
                    CharBuffer := Pointer(InParamValues[I].VRawByteString);
                    for J := 0 to ArrayCount -1 do
                    begin
                      if NullIndicator[j] = 0 then
                      begin
                        Inc(LengthIndicator[J]); //indicate length including trailing #0
                        System.Move(ZCharRecArray[j].P^, CharBuffer^, LengthIndicator[J]);
                      end
                      else
                        CharBuffer^ := #0;  //add a trailing $0 byte for empty or null strings
                      Inc(CharBuffer, CurrentVar.Length);
                    end;
                    goto SkipStringAssembling;
                  end
                  else
                    if ZCompatibleCodePages(ZCharRecArray[0].CP, zCP_UTF16) then
                      for j := 0 to ArrayCount -1 do
                        if NullIndicator[j] = 0 then //not NULL
                          if ZCharRecArray[j].Len = 0 then
                          begin
                            //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                            NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                            LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                          end
                          else
                          begin
                            WideRec.Len := ZCharRecArray[j].Len;
                            WideRec.P := ZCharRecArray[j].P;
                            TempRawByteStringArray[j] := ZWideRecToRaw(WideRec,ConSettings^.ClientCodePage^.CP); //convert to client encoding
                            TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                            LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                            CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                          end
                        else
                          LengthIndicator[J] := 0 //help OCI by giving the length so they can move instead of using StrPLCopy
                    else
                      for j := 0 to ArrayCount -1 do
                        if NullIndicator[j] = 0 then //not NULL
                          if ZCharRecArray[j].Len = 0 then
                          begin
                            //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                            NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                            LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                          end
                          else
                          begin
                            AnsiRec.Len := ZCharRecArray[j].Len;
                            AnsiRec.P := ZCharRecArray[j].P;
                            WS := ZAnsiRecToUnicode(AnsiRec, ZCharRecArray[j].CP); //localize ?WideString? to avoid overrun
                            TempRawByteStringArray[j] := ZUnicodeToRaw(WS, ConSettings^.ClientCodePage^.CP); //convert to client encoding
                            TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                            LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                            CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen); //OCI expects equal mem blocks array[xx][y] including trailing #0
                          end
                        else
                          LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
  StringAssembling:
              Inc(CurrentVar.Length); //left space for trailing #0
              {now build the huge string including the #0's because OCI expects it}
              {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
              System.SetLength(InParamValues[I].VRawByteString, CurrentVar.Length*ArrayCount); //alloc mem
              {$ELSE}
              System.SetString(InParamValues[I].VRawByteString, nil, Integer(CurrentVar.Length)*ArrayCount); //alloc mem as static blocks
              {$ENDIF}
              CharBuffer := Pointer(InParamValues[I].VRawByteString);
              for J := 0 to ArrayCount -1 do
              begin
                if NullIndicator[j] = 0 then
                begin
                  Inc(LengthIndicator[J]); //indicate length including trailing #0
                  System.Move(Pointer(TempRawByteStringArray[j])^, CharBuffer^, LengthIndicator[J]); //including trailing #0
                end
                else
                  CharBuffer^ := #0;  //add a trailing $0 byte for empty or null strings
                Inc(CharBuffer, CurrentVar.Length);
              end;
  SkipStringAssembling:
              {%H-}Inc(BufferSize, Integer(CurrentVar.Length)*ArrayCount); //Count mem to avoid overrun of OCI limit of 4GB-1
              OCIData := Pointer(InParamValues[I].VRawByteString);
              CurrentVar.TypeCode := SQLT_STR;
            end;
          stUnicodeString:
            begin
              if (FLengthIndicators[i] = nil) or ({%H-}PLongInt(NativeUInt(FLengthIndicators[i]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FLengthIndicators[i], ArrayCount);
              PLengthIndicator := FLengthIndicators[i];
              case TZVariantType(ZVariant.VArray.VArrayVariantType) of
                vtUnicodeString:
                  begin
                    for j := 0 to ArrayCount -1 do
                      if (NullIndicator[j] = 0) then //not NULL
                        if Pointer(ZUnicodeStringArray[j]) = nil then
                        begin
                          //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                          NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                          LengthIndicator[J] := 0;
                        end
                        else
                        begin
                          TempRawByteStringArray[j] := ZUnicodeToRaw(ZUnicodeStringArray[j], ConSettings^.ClientCodePage^.CP); //convert UTF16 to client-encoding
                          TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                          LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                          CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen);
                        end
                      else
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                    goto StringAssembling;
                  end;
                vtCharRec:
                  begin
                    for j := 0 to ArrayCount -1 do
                      if NullIndicator[j] = 0 then //not NULL
                        if ZCharRecArray[j].Len = 0 then
                        begin
                          //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                          NullIndicator[j] := -1; //so let's skip processing and set value to NULL
                          LengthIndicator[J] := 0;
                        end
                        else
                        begin
                          WideRec.Len := ZCharRecArray[j].Len;
                          WideRec.P := ZCharRecArray[j].P;
                          TempRawByteStringArray[j] := ZWideRecToRaw(WideRec,ConSettings^.ClientCodePage^.CP); //convert to client encoding
                          TmpStrLen := {%H-}PLongInt(NativeInt(TempRawByteStringArray[j]) - 4)^; //for D7..< 2005 Length() isn't inlined!
                          LengthIndicator[J] := TmpStrLen; //help OCI by giving the length so they can move instead of using StrPLCopy
                          CurrentVar.Length := Max(CurrentVar.Length, TmpStrLen);
                        end
                      else
                        LengthIndicator[J] := 0; //help OCI by giving the length so they can move instead of using StrPLCopy
                    goto StringAssembling;
                  end;
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
            end;
          stBytes:
            begin
              if (FLengthIndicators[i] = nil) or ({%H-}PLongInt(NativeUInt(FLengthIndicators[i]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FLengthIndicators[i], ArrayCount);
              PLengthIndicator := FLengthIndicators[i]; //make absolute var running
              CurrentVar.Length := 0;
              {first calc new size of Blocks Array[x][Length,Data] }
              for J := 0 to ArrayCount -1 do
                if NullIndicator[j] = 0 then //not NULL
                begin
                  LengthIndicator[j] := Length(ZBytesArray[j]);
                  CurrentVar.Length := Max(CurrentVar.Length, SizeOf(Integer)+LengthIndicator[j]);
                end
                else
                  LengthIndicator[j] := 0;
              System.SetLength(InParamValues[I].VBytes, Integer(CurrentVar.Length)*ArrayCount); //alloc mem
              CharBuffer := Pointer(InParamValues[I].VBytes);
              {now let's move mem... }
              for J := 0 to ArrayCount -1 do
              begin
                PInteger(CharBuffer)^ := LengthIndicator[j]; //give OCI length of byte array
                if NullIndicator[j] = 0 then //not null
                  System.Move(Pointer(ZBytesArray[j])^, (CharBuffer+SizeOf(Integer))^, LengthIndicator[J]);
                Inc(CharBuffer, CurrentVar.Length); //set new entry Pointer
              end;
              {%H-}Inc(BufferSize, Integer(CurrentVar.Length)*ArrayCount); //Count mem to avoid overrun of OCI limit of 4GB-1
              PLengthIndicator := nil; //do not set an own indicator!
              OCIData := Pointer(InParamValues[I].VBytes);
              CurrentVar.TypeCode := SQLT_LVB;
            end;
          stGUID: //AFIAK OCI doesn't support GUID fields so let's convert them to stings
            begin
              if (FLengthIndicators[i] = nil) or ({%H-}PLongInt(NativeUInt(FLengthIndicators[i]) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FLengthIndicators[i], ArrayCount);
              PLengthIndicator := FLengthIndicators[i];
              {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
              System.SetLength(InParamValues[I].VRawByteString, 39*ArrayCount); //alloc mem
              {$ELSE}
              System.SetString(InParamValues[I].VRawByteString, nil, 39*ArrayCount); //alloc mem as static blocks
              {$ENDIF}
              OCIData := Pointer(InParamValues[I].VRawByteString);
              CharBuffer := OCIData;
              CurrentVar.Length := 39;
              for J := 0 to ArrayCount -1 do
              begin
                if NullIndicator[j] = 0 then //not NULL
                begin
                  System.Move(Pointer({$IFDEF UNICODE}NotEmptyStringToASCII7{$ENDIF}(GuidToString(ZGUIDArray[j])))^, CharBuffer^, 39);
                  LengthIndicator[j] := 39;
                end
                else
                  LengthIndicator[j] := 0;
                Inc(CharBuffer, 39);
              end;
              goto SkipStringAssembling;
            end;
          stDate, stTime, stTimeStamp:
            begin
              CurrentVar.Length :=  SizeOf(POCIDateTime);
              FDescriptors[I].htype := OCI_DTYPE_TIMESTAMP; //need a fast way to dealloc the descriptors again
              SetLength(FDescriptors[I].Descriptors, ArrayCount);
              OCIData := FDescriptors[I].Descriptors;

              {%H-}Inc(BufferSize, Integer(CurrentVar.Length)*ArrayCount); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                if NullIndicator[j] = 0 then //not NULL
                begin
                  if PointerArray[j] = nil then //alloc new FDescriptors only! They are consitent until we free them
                    FPlainDriver.DescriptorAlloc(FConnectionHandle,
                      PointerArray[j], OCI_DTYPE_TIMESTAMP, 0, nil);
                  DecodeDate(ZDateTimeArray[j], Year, Month, Day);
                  DecodeTime(ZDateTimeArray[j], Hour, Min, Sec, MSec);
                  CheckOracleError(FPlainDriver, FErrorHandle,
                    FPlainDriver.DateTimeConstruct(FConnectionHandle,
                      FErrorHandle, PointerArray[j], //direct addressing descriptore to array. So we don't need to free the mem again
                      Year, Month, Day, Hour, Min, Sec, MSec * 1000000, nil, 0),
                    lcOther, 'OCIDateTimeConstruct', ConSettings);
                end
                else
                  if PointerArray[j] <> nil then
                  begin
                    FPlainDriver.DescriptorFree(PointerArray[j], OCI_DTYPE_TIMESTAMP);
                    PointerArray[j] := nil;
                  end;
              CurrentVar.TypeCode := SQLT_TIMESTAMP;
              CurrentVar.Length :=  SizeOf(POCIDateTime);
            end;
          stArray, stDataSet: ; //no idea yet
          stAsciiStream, stUnicodeStream, stBinaryStream:
            begin
              CurrentVar.Length :=  SizeOf(POCIDescriptor);
              FDescriptors[I].htype := OCI_DTYPE_LOB; //need a fast way to dealloc the descriptors again
              if (FDescriptors[I].Descriptors = nil) or ({%H-}PLongInt(NativeUInt(FDescriptors[I].Descriptors) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDescriptors[I].Descriptors, ArrayCount);
              if (FDescriptors[I].Lobs = nil) or ({%H-}PLongInt(NativeUInt(FDescriptors[I].Lobs) - 4)^{$IFDEF FPC}+1{$ENDIF} <> ArrayCount) then
                SetLength(FDescriptors[I].Lobs, ArrayCount);
              OCIData := FDescriptors[I].Descriptors;

              {%H-}Inc(BufferSize, Integer(CurrentVar.Length)*ArrayCount); //Count mem to avoid overrun of OCI limit of 4GB-1
              for J := 0 to ArrayCount -1 do
                if (NullIndicator[j] = 0) and (ZInterfaceArray[J] <> nil) and
                   not (ZInterfaceArray[J] as IZBlob).IsEmpty then
                begin //not null
                  if PointerArray[j] = nil then //alloc new FDescriptors only! They are consitent until we free them
                    FPlainDriver.DescriptorAlloc(FConnectionHandle,
                      PointerArray[j], OCI_DTYPE_LOB, 0, nil);
                  if TZSQLType(ZVariant.VArray.VArrayType) = stBinaryStream then
                  begin
                    CurrentVar.TypeCode := SQLT_BLOB;
                    TempBlob := ZInterfaceArray[j] as IZBlob;
                    WriteTempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
                      OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                      PointerArray[j], ChunkSize, ConSettings);
                    WriteTempBlob.CreateBlob;
                    WriteTempBlob.WriteLobFromBuffer(TempBlob.GetBuffer, TempBlob.Length);
                  end
                  else
                  begin
                    CurrentVar.TypeCode := SQLT_CLOB;
                    TempBlob := ZInterfaceArray[J] as IZBlob;
                    if TempBlob.IsClob then
                    begin
                      WriteTempBlob := TZOracleClob.Create(FPlainDriver,
                        nil, 0, OracleConnection.GetConnectionHandle,
                        OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                        PointerArray[j], ChunkSize, ConSettings,
                        ConSettings^.ClientCodePage^.CP);
                      WriteTempBlob.CreateBlob;
                      LobBuffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                      WriteTempBlob.WriteLobFromBuffer(LobBuffer, TempBlob.Length);
                    end
                    else
                    begin
                      AnsiTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                          TempBlob.Length, Connection.GetConSettings);
                      LobBuffer := Pointer(AnsiTemp);
                      WriteTempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
                        OracleConnection.GetConnectionHandle, OracleConnection.GetContextHandle,
                        OracleConnection.GetErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
                        ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
                      WriteTempBlob.CreateBlob;
                      WriteTempBlob.WriteLobFromBuffer(LobBuffer, {%H-}PLongInt(NativeInt(AnsiTemp) - 4)^);
                    end;
                  end;
                  FDescriptors[I].Lobs[j] := WriteTempBlob; //ref interface to keep OCILob alive otherwise OCIFreeLobTemporary will be called
                end
                else
                  if PointerArray[j] <> nil then
                  begin
                    FPlainDriver.DescriptorFree(PointerArray[j], OCI_DTYPE_LOB);
                    PointerArray[j] := nil;
                  end;
            end;
        end;
      if BufferSize >= High(Cardinal) then
        raise Exception.Create('Memory out of bounds! OCI-Limit = 4GB -1Byte');
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
          FErrorHandle, I + 1, OCIData, CurrentVar.Length,
          CurrentVar.TypeCode, PNullIndicator, PLengthIndicator, nil, 0, nil,
          OCI_DEFAULT),
        lcExecute, ASQL, ConSettings);
    end
  end
  else
  begin
    BufferSize := 0;
    Iteration := 1;
    { single row execution}
    for I := 0 to InParamCount - 1 do
    begin
      CurrentVar := @FParams.Variables[I];
      CurrentVar.Handle := nil;

      { Artificially define Oracle internal type. }
      if InParamTypes[I] in [stBytes, stBinaryStream] then
        DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_BLOB)
      else if InParamTypes[I] in [stAsciiStream, stUnicodeStream] then
        DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_CLOB)
      else if InParamTypes[I] in [stString, stUnicodeString] then
      begin
        CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
        //(pl/sql statement) may have possible out params!!
        //so take care to use a new bidirectional memory addressation
        DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_STR,
          (CharRec.Len > Max_OCI_String_Size) or (FStatementType in [OCI_STMT_BEGIN, OCI_STMT_DECLARE]));
      end
      else
        DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_STR);
      Inc(BufferSize, CalcBufferSizeOfSQLVar(CurrentVar));
    end; //Bufsize is determined
    if Length(FBuffer) < BufferSize then SetLength(FBuffer, BufferSize); //realloc the buffer if required
    CharBuffer := Pointer(FBuffer); //set first data-entry in rowBuffer

    for i := 0 to FParams^.AllocNum -1 do
    begin
      CurrentVar := @FParams.Variables[I];
      SetVariableDataEntrys(CharBuffer, CurrentVar, Iteration);
      AllocDesriptors(FPlainDriver, FConnectionHandle, CurrentVar, Iteration);
      if CurrentVar^.AllocMem then //point to allocated mem ?
        Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
          I + 1, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
          CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT)
      else //nope, just to local string data
      begin //point to data of string
        CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
        Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
          I + 1, CharRec.P, Math.Max(2, CharRec.Len), CurrentVar^.TypeCode, CurrentVar^.oIndicatorArray,
          CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT);
        //set oDataSize, so oracle can move data instead of use StrPCopy
        Pub2(CurrentVar^.oDataSizeArray)^ := CharRec.Len+1; //include trailing #0
      end;
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
    end;

    { Loads binded variables with values. }
    LoadOracleVars(FPlainDriver, Connection, FErrorHandle,
      FParams, InParamValues,ChunkSize);
  end;
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.UnPrepareInParameters;
var
  I, J: Integer;
begin
  if ArrayCount = 0 then
    FreeOracleSQLVars(FPlainDriver, FParams, 1, FConnectionHandle, FErrorHandle, ConSettings)
  else
  begin
    for i := 0 to high(FDescriptors) do
      if FDescriptors[I].htype > 0 then
        for J := 0 to ArrayCount -1 do
          if FDescriptors[I].Descriptors[J] <> nil then
            FPlainDriver.DescriptorFree(FDescriptors[I].Descriptors[J], FDescriptors[I].htype);
    SetLength(FDescriptors, 0);
    SetLength(FNullIndicators, 0);
    SetLength(FLengthIndicators, 0);
    SetLength(FIntegerValues, 0);
    SetLength(FInt64Values, 0);
    SetLength(FDoubleValues, 0);
  end;
end;

{**
  Prepares an SQL statement
}
procedure TZOraclePreparedStatement.Prepare;
begin
  if not Prepared then
  begin
    if (FHandle = nil) or (FErrorHandle = nil) then
    { Allocates statement handles. }
    AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle, False{FServerStmtCache});
    { prepare stmt }
    PrepareOracleStatement(FPlainDriver, FContextHandle, ASQL, FHandle, FErrorHandle,
        FRowPrefetchSize, False{FServerStmtCache}, ConSettings);
    { get Statemant type }
    FPlainDriver.AttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;

procedure TZOraclePreparedStatement.UnPrepare;
const RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
begin
  try
    {if Self.FServerStmtCache then
      CheckOracleError(FPlainDriver, FErrorHandle,
        FplainDriver.StmtRelease(FHandle, FErrorHandle, nil, 0, RELEASE_MODE[False]),
      lcExecute, ASQL, ConSettings)
    else}
      FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  finally
    inherited Unprepare;
  end;
end;



{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;

  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;

  if FStatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    LastResultSet := CreateResultSet;
    Result := LastResultSet <> nil;
  end
  else
  begin
    { Executes the statement and gets a result. }
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit,
      Max(1, ArrayCount));
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
  end;
  inherited ExecutePrepared;

  { Unloads binded variables with values. }
  if ArrayCount = 0 then
    UnloadOracleVars(FParams, 0)
  else
    UnloadOracleVars(FDescriptors, ArrayCount)

  { Autocommit statement. done by ExecuteOracleStatement}
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;
  BindInParameters;

  { Executes the statement and gets a resultset. }
  Result := CreateResultSet;
  inherited ExecuteQueryPrepared;

  { Unloads binded variables with values. }
  if ArrayCount = 0 then
    UnloadOracleVars(FParams, ArrayCount)
  else
    UnloadOracleVars(FDescriptors, ArrayCount)
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  ResultSet: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;
  try
    if FStatementType = OCI_STMT_SELECT then
    begin
      Result := -1;

      { Executes the statement and gets a resultset. }
      ResultSet := CreateResultSet;
      try
        while ResultSet.Next do;
        LastUpdateCount := ResultSet.GetRow;
      finally
        ResultSet.Close;
      end;
    end
    else
    begin
      { Executes the statement and gets a result. }
      ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
        ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit,Max(1, ArrayCount));
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    end;
    Result := LastUpdateCount;
    inherited ExecuteUpdatePrepared;
  finally
    { Unloads binded variables with values. }
    if ArrayCount = 0 then
      UnloadOracleVars(FParams, ArrayCount)
    else
      UnloadOracleVars(FDescriptors, ArrayCount)
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
end;

procedure TZOracleCallableStatement.Prepare;
begin
  if not Prepared then
  begin
    ASQL := GetProcedureSql(False);
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    PrepareOracleStatement(FPlainDriver, nil, ASQL, FHandle, FErrorHandle,
          FRowPrefetchSize, False, ConSettings);
    FPlainDriver.AttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;


procedure TZOracleCallableStatement.UnPrepare;
const RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
begin
  try
    {if Self.FServerStmtCache then
      CheckOracleError(FPlainDriver, FErrorHandle,
        FplainDriver.StmtRelease(FHandle, FErrorHandle, nil, 0, RELEASE_MODE[False]),
      lcExecute, ASQL, ConSettings)
    else}
      FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  finally
    inherited Unprepare;
  end;
end;

procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  with FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := SQLType;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamType(ParameterIndex: integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF};
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pType := ParamType;
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pParamIndex := ParameterIndex;
  if ParamType in [2,3,4] then //ptInOut, ptOut, ptResult
  begin
    FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pOutIndex := FOutParamCount;
    Inc(FOutParamCount);
  end;
end;

procedure TZOracleCallableStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
var 
  AConnection: IZConnection;

  function GetOracleParamIndexOfParameterIndex: Integer;
  var I: Integer;
  begin
    Result := 0;
    for i := 0 to high(FOracleParams) do
      if ParameterIndex = FOracleParams[i].pParamIndex then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  inherited SetInParam(ParameterIndex, SQLType, Value);
  with FOracleParams[GetOracleParamIndexOfParameterIndex] do
  begin
    AConnection := GetConnection;
    if Assigned(AConnection) and ( not AConnection.UseMetadata ) then
      pName := 'p'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := ord(SQLType);
    pValue := Value;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
var
  iPos: Integer;
  ProcName: String;
begin
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
  iPos := Pos('.', ParamName);
  if iPos > 0 then
  begin
    ProcName := Copy(ParamName, 1, iPos-1); //extract function or Procedure names
    FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.IndexOf(ProcName); //check index
    if FOracleParams[ParameterIndex].pProcIndex = -1 then //if not exists
      FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.Add(ProcName); //Add to List
  end
  else //No package
    FOracleParams[ParameterIndex].pProcIndex := 0;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.PrepareInParameters;
var I: Integer;
begin
  AllocateOracleSQLVars(FParams, FOracleParamsCount);
  SetLength(FParamNames, FOracleParamsCount);
  for i := 0 to FOracleParamsCount -1 do
    FParamNames[I] := Self.FOracleParams[I].pName;
end;

{**
  Binds the input parameters
}
procedure TZOracleCallableStatement.BindInParameters;
var
  I, Iteration: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  CharRec: TZCharRec;
  BufferSize: Int64;
  CharBuffer: PAnsiChar;
  SQLType: TZSQLType;
begin
  ArrangeInParams; //need to sort ReturnValues for functions
  BufferSize := 0;
  Iteration := 1;
  { single row execution}
  for I := 0 to FOracleParamsCount - 1 do
  begin
    CurrentVar := @FParams.Variables[I];
    CurrentVar.Handle := nil;

    SQLType := TZSQLType(FOracleParams[I].pSQLType);
    { Artificially define Oracle internal type. }
    if SQLType in [stBytes, stBinaryStream] then
      DefineOracleVarTypes(CurrentVar, SQLType, 4096, SQLT_BLOB)
    else if SQLType in [stAsciiStream, stUnicodeStream] then
      DefineOracleVarTypes(CurrentVar, SQLType, 4096, SQLT_CLOB)
    else if SQLType in [stString, stUnicodeString] then
      //(pl/sql statement) may have possible out params!!
      //so take care to use a new bidirectional memory addressation
      DefineOracleVarTypes(CurrentVar, SQLType, 4096, SQLT_STR,
        (FStatementType in [OCI_STMT_BEGIN, OCI_STMT_DECLARE]))
    else
      DefineOracleVarTypes(CurrentVar, SQLType, 4096, SQLT_STR);
    Inc(BufferSize, Iteration * CalcBufferSizeOfSQLVar(CurrentVar));
  end; //Bufsize is determined
  if Length(FBuffer) < BufferSize then SetLength(FBuffer, BufferSize); //realloc the buffer if required
  CharBuffer := Pointer(FBuffer); //set first data-entry in rowBuffer

  for i := 0 to FParams^.AllocNum -1 do
  begin
    CurrentVar := @FParams.Variables[I];
    SetVariableDataEntrys(CharBuffer, CurrentVar, Iteration);
    AllocDesriptors(FPlainDriver, FConnectionHandle, CurrentVar, Iteration);
    if CurrentVar^.AllocMem then //point to allocated mem ?
      Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
        I + 1, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
        CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT)
    else //nope, just to local string data
    begin //point to data of string
      CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
      Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
        I + 1, CharRec.P, CharRec.Len, CurrentVar^.TypeCode, CurrentVar^.oIndicatorArray,
        CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT);
      //set oDataSize, so oracle can move data instead of use StrPCopy
      CurrentVar^.oDataSizeArray^[0] := CharRec.Len+1; //include trailing #0
    end;
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
  end;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, FErrorHandle,
    FParams, InParamValues,ChunkSize);

  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.UnPrepareInParameters;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, 1, FConnectionHandle, FErrorHandle, ConSettings)
end;

procedure TZOracleCallableStatement.ArrangeInParams;
var
  I, J, NewProcIndex, StartProcIndex: Integer;
  TempVars: TZVariantDynArray;
  TempOraVar: TZOracleParam;
begin
  NewProcIndex := -1;
  StartProcIndex := 0;
  if IsFunction then
  begin
    for i := 0 to high(FOracleParams) do
    begin
      if not ( FOracleParams[i].pProcIndex = NewProcIndex ) then
      begin
        NewProcIndex := FOracleParams[i].pProcIndex;
        StartProcIndex := I;
      end;
      if ( FOracleParams[i].pType = 4 ) then
      begin
        ClientVarManager.SetNull(FOracleParams[i].pValue);
        if not (i = StartProcIndex) then
        begin
          TempOraVar := FOracleParams[I];
          for J := I downto StartProcIndex+1 do
            FOracleParams[j] := FOracleParams[j-1];
          FOracleParams[StartProcIndex] := TempOraVar;
        end;
      end;
    end;
    SetLength(TempVars, Length(FOracleParams));
    for i := 0 to high(FOracleParams) do
      TempVars[i] := FOracleParams[i].pValue;
    InParamValues := TempVars;
  end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  LobLocator: POCILobLocator;
  I: integer;
  L: Cardinal;
  TempBlob: IZBlob;

  procedure SetOutParam(CurrentVar: PZSQLVar; Index: Integer);
  var
    OracleConnection :IZOracleConnection;
    Year:SmallInt;
    Month, Day:Byte; Hour, Min, Sec:ub1; MSec: ub4;
    {$IFDEF UNICODE}
    AnsiRec: TZAnsiRec;
    {$ELSE}
    RawTemp: RawByteString;
    {$ENDIF}
  begin
    if CurrentVar^.oIndicatorArray[0] < 0 then
      outParamValues[Index] := NullVariant
    else
      case CurrentVar^.TypeCode of
        SQLT_INT: outParamValues[Index] := EncodeInteger(PLongInt(CurrentVar^.Data)^ );
        SQLT_FLT: outParamValues[Index] := EncodeFloat(PDouble(CurrentVar^.Data)^ );
        SQLT_STR:
          begin
            L := CurrentVar^.oDataSizeArray[0];
            {$IFDEF UNICODE}
            AnsiRec.Len := L;
            AnsiRec.P := CurrentVar^.Data;// .Data;
            outParamValues[Index] := EncodeString(ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP));
            {$ELSE}
            ZSetString(CurrentVar^.Data, L, RawTemp{%H-});
            outParamValues[Index] := EncodeString(ConSettings.ConvFuncs.ZRawToString(RawTemp, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP));
            {$ENDIF}
          end;
        SQLT_TIMESTAMP:
          begin
            OracleConnection := Connection as IZOracleConnection;
            FPlainDriver.DateTimeGetDate(
              OracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Year{%H-}, Month{%H-}, Day{%H-});
            FPlainDriver.DateTimeGetTime(
              OracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Hour{%H-}, Min{%H-}, Sec{%H-},MSec{%H-});
            outParamValues[Index] := EncodeDateTime(EncodeDate(year,month,day )+EncodeTime(Hour,min,sec,  msec div 1000000));
          end;
        SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE:
          begin
            LobLocator := PPOCIDescriptor(CurrentVar^.Data)^;

            OracleConnection := Connection as IZOracleConnection;
            if CurrentVar^.TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
              TempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                  LobLocator, GetChunkSize, ConSettings)
            else
              TempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
                OracleConnection.GetConnectionHandle,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                LobLocator, GetChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
            outParamValues[Index] := EncodeInterface(TempBlob);
            TempBlob := nil;
          end;
        SQLT_NTY: //currently not supported
          outParamValues[Index] := NullVariant;
      end;
  end;
begin
  for I := 0 to FOracleParamsCount -1 do
    if FOracleParams[i].pType in [2,3,4] then
      SetOutParam(@FParams^.Variables[I], FOracleParams[i].pParamIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
end;

function TZOracleCallableStatement.GetProcedureSql(SelectProc: boolean): RawByteString;
var
  sFunc: string;
  I, IncludeCount, LastIndex: Integer;
  PackageBody: TStrings;
  TempResult: String;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if ( FDBParamTypes[I] = 4 ) then //ptResult
      begin
        sFunc := ' :'+FOracleParams[0].pName+' := ';
        continue;
      end;
      if Result <> '' then
        Result := Result + ',';
      if IsFunction then
        Result := Result + ':'+FOracleParams[I+1].pName
      else
        Result := Result + ':'+FOracleParams[I].pName;
    end;
    Result := '('+Result+')'
  end;

var
  InParams: string;
begin
  sFunc := '';
  if PackageIncludedList.Count > 0 then
  begin
    PackageBody := TStringList.Create;
    PackageBody.Add('BEGIN');
    LastIndex := 0;
    for IncludeCount := 0 to PackageIncludedList.Count -1 do
    begin
      InParams := '';
      sFunc := '';
      for i := LastIndex to high(FOracleParams) do
        if IncludeCount = FOracleParams[i].pProcIndex then
          if ( FOracleParams[I].pType = 4 ) then //ptResult
            sFunc := ' :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])+' := '
          else
            if InParams <> '' then
              InParams := InParams +', :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
            else
              InParams := InParams +':'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
        else
        begin
          LastIndex := I;
          break;
        end;
      PackageBody.Add('BEGIN '+sFunc+SQL+
        '.'+GetConnection.GetMetadata.GetIdentifierConvertor.Quote(PackageIncludedList[IncludeCount])+'('+InParams+'); END;');
    end;
    PackageBody.Add('END;');
    TempResult := TrimRight(PackageBody.Text);
    FreeAndNil(PackageBody);
  end
  else
  begin
    InParams := GenerateParamsStr( FOracleParamsCount );
    TempResult := 'BEGIN ' + sFunc +SQL + InParams+'; END;';
  end;
  Result := NotEmptyStringToASCII7(TempResult);
end;

procedure TZOracleCallableStatement.ClearParameters;
begin
  inherited;
  FOracleParamsCount := 0;
  SetLength(FOracleParams, 0);
end;

constructor TZOracleCallableStatement.Create(Connection: IZConnection;
  const pProcName: string; Info: TStrings);
begin
  inherited Create(Connection, pProcName, Info);

  FOracleParamsCount := 0;
  FPlainDriver := Connection.GetIZPlainDriver as IZOraclePlainDriver;
  ResultSetType := rtForwardOnly;
  PackageIncludedList := TStringList.Create;
  FOutParamCount := 0;
  FRowPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'row_prefetch_size', ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072);
  FConnectionHandle := (Connection as IZOracleConnection).GetConnectionHandle;
  FIteration := 1;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings);
  PackageIncludedList.Free;
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit, FIteration);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams, 0)
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit, 1);
    FetchOutParamsFromOracleVars;
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self, Self.SQL,
      FHandle, FErrorHandle, FParams, FOracleParams);
    Result := LastResultSet;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams, 0);
  end;
end;

end.
