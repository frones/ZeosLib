{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                WebService Proxy Server                  }
{                                                         }
{         Originally written by Jan Baumgarten            }
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

unit DbcProxyUtils;

{$ifdef fpc}
{$H+}
{$ifend}

{$I ../../Zeos.inc}

interface

uses
  Classes, SysUtils, ZDbcIntfs;

function encodeConnectionProperties(const Connection: IZConnection): String;
function encodeDatabaseInfo(const Connection: IZConnection): String;

procedure decodeParameters(const ParamXML: String; Statement: IZPreparedStatement);
procedure applyConnectionProperties(const Connection: IZConnection; const Properties: String);

//function ZXMLEncode(Input: String): String;

implementation

uses
  typinfo, dom, XMLRead, Base64, ZExceptions{$IFDEF ZEOS73UP}, FMTBCD{$ENDIF}, ZDbcXmlUtils;

{$IFNDEF FPC}
const
  LineEnding = sLineBreak;
{$IFEND}

{------------------------- DecodeParameters -----------------------------------}

procedure DecodeParameters(const ParamXML: String; Statement: IZPreparedStatement);
var
  Doc: TXMLDocument;
  ParamsNode: TDomNode;
  ParamNode: TDOMNode;
  IsNull: Boolean;
  ParamTypeStr: String;
  ParamType: TZSQLType;
  ParamValue: String;
  x: Integer;
  Stream: TStringStream;
  ParamIdx: Integer;

  function GetNodeValue(Node: TDomNode): String;
  begin
    if Assigned(Node) then
      Result := UTF8Encode(Node.NodeValue)
    else
      Result := '';
  end;

  function BinaryToBytes(const Base64Str: String): TBytes;
  var
    StringStream: TStringStream;
    DecodingStream: TBase64DecodingStream;
  begin
    Result := [];
    StringStream := TStringStream.Create(Base64Str);
    try
      DecodingStream := TBase64DecodingStream.Create(StringStream);
      try
        SetLength(Result, DecodingStream.Size);
        DecodingStream.Read(Result[0], DecodingStream.Size);
      finally
        FreeAndNil(DecodingStream);
      end;
    finally
      FreeAndNil(StringStream);
    end;
  end;

begin
  ParamType := stBoolean;
  Stream := TStringStream.Create(ParamXML);
  try
    XMLRead.ReadXMLFile(Doc, Stream);
    try
      ParamsNode := Doc.GetChildNodes.Item[0];

      for x := 1 to ParamsNode.GetChildNodes.Count do begin
        ParamIdx := x - 1 + FirstDbcIndex;
        ParamNode := ParamsNode.GetChildNodes.Item[x - 1];
        IsNull := StrToBoolDef(GetNodeValue(ParamNode.Attributes.GetNamedItem('isnull')), false);
        ParamTypeStr := GetNodeValue(ParamNode.Attributes.GetNamedItem('type'));
        ParamType := TZSQLType(GetEnumValue(TypeInfo(ParamType), ParamTypeStr));
        ParamValue := GetNodeValue(ParamNode.Attributes.GetNamedItem('value'));
        if IsNull then
          Statement.SetNull(ParamIdx, ParamType)
        else begin
          case ParamType of
            stBoolean:
              Statement.SetBoolean(ParamIdx, StrToBool(ParamValue));
            stByte:
              Statement.SetByte(ParamIdx, StrToInt(ParamValue));
            stShort:
              Statement.SetShort(ParamIdx, StrToInt(ParamValue));
            stWord:
              Statement.SetWord(ParamIdx, StrToInt(ParamValue));
            stSmall:
              Statement.SetSmall(ParamIdx, StrToInt(ParamValue));
            stLongWord:
              Statement.SetUInt(ParamIdx, StrToDWord(ParamValue));
            stInteger:
              Statement.SetInt(ParamIdx, StrToInt(ParamValue));
            stULong:
              Statement.SetULong(ParamIdx, StrToQWord(ParamValue));
            stLong:
              Statement.SetLong(ParamIdx, StrToInt64(ParamValue));
            stFloat:
              Statement.SetFloat(ParamIdx, StrToFloat(ParamValue, ZXmlProxyFormatSettings));
            stDouble:
              Statement.SetDouble(ParamIdx, StrToFloat(ParamValue, ZXmlProxyFormatSettings));
            stCurrency:
              Statement.SetCurrency(ParamIdx, StrToCurr(ParamValue, ZXmlProxyFormatSettings));
            stBigDecimal:
              Statement.SetBigDecimal(ParamIdx, StrToFloat(ParamValue, ZXmlProxyFormatSettings));
            stString, stUnicodeString:
              Statement.SetString(ParamIdx, ParamValue);
            stDate:
              Statement.SetDate(ParamIdx, StrToDate(ParamValue, ZXmlProxyFormatSettings));
            stTime:
              Statement.SetTime(ParamIdx, StrToTime(ParamValue, ZXmlProxyFormatSettings));
            stTimestamp:
              Statement.SetTimestamp(ParamIdx, StrToDateTime(ParamValue, ZXmlProxyFormatSettings));
            stAsciiStream, stUnicodeStream:
              Statement.SetString(ParamIdx, ParamValue);
            stBinaryStream, stBytes:
              Statement.SetBytes(ParamIdx, BinaryToBytes(ParamValue));
            else
              raise EZSQLException.Create('Conversion of parameter of type ' + ParamTypeStr + ' is not supported (yet).');
          end;
        end;
      end;
    finally
      FreeAndNil(Doc);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

function encodeConnectionProperties(const Connection: IZConnection): String;
var
  List: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
  ServerProvider: TZServerProvider;
begin
  List := TStringList.Create;
  try
    List.Values['readonly'] := BoolToStr(Connection.IsReadOnly, true);
    List.Values['catalog'] := Connection.GetCatalog;
    TransactionIsolation := Connection.GetTransactionIsolation;
    TempStr := GetEnumName(TypeInfo(TransactionIsolation), Ord(TransactionIsolation));
    List.Values['transactionisolation'] := TempStr;
    List.Values['usemetadata'] := BoolToStr(Connection.UseMetadata, true);
    {$IFNDEF ZEOS73UP}
    List.Values['autoencodestrings'] := BoolToStr(Connection.GetAutoEncodeStrings, true);
    {$IFEND}
    ServerProvider := Connection.GetServerProvider;
    TempStr := GetEnumName(TypeInfo(ServerProvider), Ord(ServerProvider));
    List.Values['serverprovider'] := TempStr;
    Result := List.Text;
  finally
    FreeAndNil(List);
  end;
end;

procedure applyConnectionProperties(const Connection: IZConnection; const Properties: String);
var
  List: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
begin
  TransactionIsolation := tiNone;
  List := TStringList.Create;
  try
    List.Text := Properties;
    //- Set-/IsReadOnly
    TempStr := List.Values['readonly'];
    if TempStr <> '' then
      Connection.SetReadOnly(StrToBool(TempStr));
    //- Set-/GetCatalog
    TempStr := List.Values['catalog'];
    if TempStr <> '' then
      Connection.SetCatalog(TempStr);
    //- Set-/GetTransactionIsolation
    TempStr := List.Values['transactionisolation'];
    if TempStr <> '' then begin
      TransactionIsolation := TZTransactIsolationLevel(GetEnumValue(TypeInfo(TransactionIsolation), TempStr));
      Connection.SetTransactionIsolation(TransactionIsolation);
    end;
    //- (Set)UseMetaData
    TempStr := List.Values['usemetadata'];
    if TempStr <> '' then
      Connection.SetUseMetadata(StrToBool(TempStr));
    //- Get-/SetAutoEncodeStrings
    {$IFNDEF ZEOS73UP}
    TempStr := List.Values['autoencodestrings'];
    if TempStr <> '' then
      Connection.SetAutoEncodeStrings(StrToBool(TempStr));
    {$ENDIF}
    //- Get-/SetAutoCommit (as part of initial property transfer)
    TempStr := List.Values['autocommit'];
    if TempStr <> '' then
      Connection.SetAutoCommit(StrToBool(TempStr));
  finally
    FreeAndNil(List);
  end;
end;

function encodeDatabaseInfo(const Connection: IZConnection): String;
var
  DbInfo: IZDatabaseInfo;
  PropList: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
begin
  Result := '';
  DbInfo := Connection.GetMetadata.GetDatabaseInfo;
  PropList := TStringList.Create;
  try
    PropList.Values['AllProceduresAreCallable'] := BoolToStr(DbInfo.AllProceduresAreCallable, true);
    PropList.Values['AllTablesAreSelectable'] := BoolToStr(DbInfo.AllTablesAreSelectable, true);
    PropList.Values['DataDefinitionCausesTransactionCommit'] := BoolToStr(DbInfo.DataDefinitionCausesTransactionCommit, true);
    PropList.Values['DoesMaxRowSizeIncludeBlobs'] := BoolToStr(DbInfo.DoesMaxRowSizeIncludeBlobs, true);
    PropList.Values['DataDefinitionIgnoredInTransactions'] := BoolToStr(DbInfo.DataDefinitionIgnoredInTransactions, true);
    PropList.Values['CatalogSeparator'] := DbInfo.GetCatalogSeparator;
    PropList.Values['CatalogTerm'] := DbInfo.GetCatalogTerm;
    PropList.Values['DatabaseProductName'] := DbInfo.GetDatabaseProductName;
    PropList.Values['DatabaseProductVersion'] := DbInfo.GetDatabaseProductVersion;
    TransactionIsolation := DbInfo.GetDefaultTransactionIsolation;
    TempStr := GetEnumName(TypeInfo(TransactionIsolation), Ord(TransactionIsolation));
    PropList.Values['DefaultTransactionIsolation'] := TempStr;
    PropList.Values['DriverMajorVersion'] := IntToStr(DbInfo.GetDriverMajorVersion);
    PropList.Values['DriverMinorVersion'] := IntToStr(DbInfo.GetDriverMinorVersion);
    PropList.Values['DriverName'] := DbInfo.GetDriverName;
    PropList.Values['DriverVersion'] := DbInfo.GetDriverVersion;
    PropList.Values['ExtraNameCharacters'] := DbInfo.GetExtraNameCharacters;
    // todo: encode StringList
    //PropList.Values['GetIdentifierQuoteKeywordsSorted'] := DbInfo.GetIdentifierQuoteKeywordsSorted;
    PropList.Values['IdentifierQuoteString'] := DbInfo.GetIdentifierQuoteString;
    PropList.Values['MaxBinaryLiteralLength'] := IntToStr(DbInfo.GetMaxBinaryLiteralLength);
    PropList.Values['MaxCatalogNameLength'] := IntToStr(DbInfo.GetMaxCatalogNameLength);
    PropList.Values['MaxCharLiteralLength'] := IntToStr(DbInfo.GetMaxCharLiteralLength);
    PropList.Values['MaxColumnNameLength'] := IntToStr(DbInfo.GetMaxColumnNameLength);
    PropList.Values['MaxColumnsInGroupBy'] := IntToStr(DbInfo.GetMaxColumnsInGroupBy);
    PropList.Values['MaxColumnsInIndex'] := IntToStr(DbInfo.GetMaxColumnsInIndex);
    PropList.Values['MaxColumnsInOrderBy'] := IntToStr(DbInfo.GetMaxColumnsInOrderBy);
    PropList.Values['MaxColumnsInSelect'] := IntToStr(DbInfo.GetMaxColumnsInSelect);
    PropList.Values['MaxColumnsInTable'] := IntToStr(DbInfo.GetMaxColumnsInTable);
    PropList.Values['MaxConnections'] := IntToStr(DbInfo.GetMaxConnections);
    PropList.Values['MaxCursorNameLength'] := IntToStr(DbInfo.GetMaxCursorNameLength);
    PropList.Values['MaxIndexLength'] := IntToStr(DbInfo.GetMaxIndexLength);
    PropList.Values['MaxProcedureNameLength'] := IntToStr(DbInfo.GetMaxProcedureNameLength);
    PropList.Values['MaxRowSize'] := IntToStr(DbInfo.GetMaxRowSize);
    PropList.Values['MaxSchemaNameLength'] := IntToStr(DbInfo.GetMaxSchemaNameLength);
    PropList.Values['MaxStatementLength'] := IntToStr(DbInfo.GetMaxStatementLength);
    PropList.Values['MaxStatements'] := IntToStr(DbInfo.GetMaxStatements);
    PropList.Values['MaxTableNameLength'] := IntToStr(DbInfo.GetMaxTableNameLength);
    PropList.Values['MaxTablesInSelect'] := IntToStr(DbInfo.GetMaxTablesInSelect);
    PropList.Values['MaxUserNameLength'] := IntToStr(DbInfo.GetMaxUserNameLength);
    PropList.Values['NumericFunctions'] := DbInfo.GetNumericFunctions;
    PropList.Values['ProcedureTerm'] := DbInfo.GetProcedureTerm;
    PropList.Values['SchemaTerm'] := DbInfo.GetSchemaTerm;
    PropList.Values['SearchStringEscape'] := DbInfo.GetSearchStringEscape;
    PropList.Values['ServerVersion'] := DbInfo.GetServerVersion;
    PropList.Values['SQLKeywords'] := DbInfo.GetSQLKeywords;
    PropList.Values['StringFunctions'] := DbInfo.GetStringFunctions;
    PropList.Values['SystemFunctions'] := DbInfo.GetSystemFunctions;
    PropList.Values['TimeDateFunctions'] := DbInfo.GetTimeDateFunctions;
    PropList.Values['IsCatalogAtStart'] := BoolToStr(DbInfo.IsCatalogAtStart, True);
    PropList.Values['IsReadOnly'] := BoolToStr(DbInfo.IsReadOnly, True);
    PropList.Values['NullPlusNonNullIsNull'] := BoolToStr(DbInfo.NullPlusNonNullIsNull, True);
    PropList.Values['NullsAreSortedAtEnd'] := BoolToStr(DbInfo.NullsAreSortedAtEnd, True);
    PropList.Values['NullsAreSortedAtStart'] := BoolToStr(DbInfo.NullsAreSortedAtStart, True);
    PropList.Values['NullsAreSortedHigh'] := BoolToStr(DbInfo.NullsAreSortedHigh, True);
    PropList.Values['NullsAreSortedLow'] := BoolToStr(DbInfo.NullsAreSortedLow, True);
    PropList.Values['StoresLowerCaseIdentifiers'] := BoolToStr(DbInfo.StoresLowerCaseIdentifiers, True);
    PropList.Values['StoresLowerCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresLowerCaseQuotedIdentifiers, True);
    PropList.Values['StoresMixedCaseIdentifiers'] := BoolToStr(DbInfo.StoresMixedCaseIdentifiers, True);
    PropList.Values['StoresMixedCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresMixedCaseQuotedIdentifiers, True);
    PropList.Values['StoresUpperCaseIdentifiers'] := BoolToStr(DbInfo.StoresUpperCaseIdentifiers, True);
    PropList.Values['StoresUpperCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresUpperCaseQuotedIdentifiers, True);
    PropList.Values['SupportsAlterTableWithAddColumn'] := BoolToStr(DbInfo.SupportsAlterTableWithAddColumn, True);
    PropList.Values['SupportsAlterTableWithDropColumn'] := BoolToStr(DbInfo.SupportsAlterTableWithDropColumn, True);
    PropList.Values['SupportsANSI92EntryLevelSQL'] := BoolToStr(DbInfo.SupportsANSI92EntryLevelSQL, True);
    PropList.Values['SupportsANSI92FullSQL'] := BoolToStr(DbInfo.SupportsANSI92FullSQL, True);
    PropList.Values['SupportsANSI92IntermediateSQL'] := BoolToStr(DbInfo.SupportsANSI92IntermediateSQL, True);
    PropList.Values['SupportsArrayBindings'] := BoolToStr(DbInfo.SupportsArrayBindings, True);
    PropList.Values['SupportsBatchUpdates'] := BoolToStr(DbInfo.SupportsBatchUpdates, True);
    PropList.Values['SupportsCatalogsInDataManipulation'] := BoolToStr(DbInfo.SupportsCatalogsInDataManipulation, True);
    PropList.Values['SupportsCatalogsInIndexDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInIndexDefinitions, True);
    PropList.Values['SupportsCatalogsInPrivilegeDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInPrivilegeDefinitions, True);
    PropList.Values['SupportsCatalogsInProcedureCalls'] := BoolToStr(DbInfo.SupportsCatalogsInProcedureCalls, True);
    PropList.Values['SupportsCatalogsInTableDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInTableDefinitions, True);
    PropList.Values['SupportsColumnAliasing'] := BoolToStr(DbInfo.SupportsColumnAliasing, True);
    PropList.Values['SupportsConvert'] := BoolToStr(DbInfo.SupportsConvert, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsConvertForTypes'] := BoolToStr(DbInfo.SupportsConvertForTypes(), True);
    PropList.Values['SupportsCoreSQLGrammar'] := BoolToStr(DbInfo.SupportsCoreSQLGrammar, True);
    PropList.Values['SupportsCorrelatedSubqueries'] := BoolToStr(DbInfo.SupportsCorrelatedSubqueries, True);
    PropList.Values['SupportsDataDefinitionAndDataManipulationTransactions'] := BoolToStr(DbInfo.SupportsDataDefinitionAndDataManipulationTransactions, True);
    PropList.Values['SupportsDataManipulationTransactionsOnly'] := BoolToStr(DbInfo.SupportsDataManipulationTransactionsOnly, True);
    PropList.Values['SupportsDifferentTableCorrelationNames'] := BoolToStr(DbInfo.SupportsDifferentTableCorrelationNames, True);
    PropList.Values['SupportsExpressionsInOrderBy'] := BoolToStr(DbInfo.SupportsExpressionsInOrderBy, True);
    PropList.Values['SupportsExtendedSQLGrammar'] := BoolToStr(DbInfo.SupportsExtendedSQLGrammar, True);
    PropList.Values['SupportsFullOuterJoins'] := BoolToStr(DbInfo.SupportsFullOuterJoins, True);
    PropList.Values['SupportsGroupBy'] := BoolToStr(DbInfo.SupportsGroupBy, True);
    PropList.Values['SupportsGroupByBeyondSelect'] := BoolToStr(DbInfo.SupportsGroupByBeyondSelect, True);
    PropList.Values['SupportsGroupByUnrelated'] := BoolToStr(DbInfo.SupportsGroupByUnrelated, True);
    PropList.Values['SupportsIntegrityEnhancementFacility'] := BoolToStr(DbInfo.SupportsIntegrityEnhancementFacility, True);
    PropList.Values['SupportsLikeEscapeClause'] := BoolToStr(DbInfo.SupportsLikeEscapeClause, True);
    PropList.Values['SupportsLimitedOuterJoins'] := BoolToStr(DbInfo.SupportsLimitedOuterJoins, True);
    PropList.Values['SupportsMilliseconds'] := BoolToStr(DbInfo.SupportsMilliseconds, True);
    PropList.Values['SupportsMinimumSQLGrammar'] := BoolToStr(DbInfo.SupportsMinimumSQLGrammar, True);
    PropList.Values['SupportsMixedCaseIdentifiers'] := BoolToStr(DbInfo.SupportsMixedCaseIdentifiers, True);
    PropList.Values['SupportsMixedCaseQuotedIdentifiers'] := BoolToStr(DbInfo.SupportsMixedCaseQuotedIdentifiers, True);
    PropList.Values['SupportsMultipleResultSets'] := BoolToStr(DbInfo.SupportsMultipleResultSets, True);
    PropList.Values['SupportsMultipleTransactions'] := BoolToStr(DbInfo.SupportsMultipleTransactions, True);
    PropList.Values['SupportsNonEscapedSearchStrings'] := BoolToStr(DbInfo.SupportsNonEscapedSearchStrings, True);
    PropList.Values['SupportsNonNullableColumns'] := BoolToStr(DbInfo.SupportsNonNullableColumns, True);
    PropList.Values['SupportsOpenCursorsAcrossCommit'] := BoolToStr(DbInfo.SupportsOpenCursorsAcrossCommit, True);
    PropList.Values['SupportsOpenCursorsAcrossRollback'] := BoolToStr(DbInfo.SupportsOpenCursorsAcrossRollback, True);
    PropList.Values['SupportsOpenStatementsAcrossCommit'] := BoolToStr(DbInfo.SupportsOpenStatementsAcrossCommit, True);
    PropList.Values['SupportsOpenStatementsAcrossRollback'] := BoolToStr(DbInfo.SupportsOpenStatementsAcrossRollback, True);
    PropList.Values['SupportsOrderByUnrelated'] := BoolToStr(DbInfo.SupportsOrderByUnrelated, True);
    PropList.Values['SupportsOuterJoins'] := BoolToStr(DbInfo.SupportsOuterJoins, True);
    PropList.Values['SupportsOverloadPrefixInStoredProcedureName'] := BoolToStr(DbInfo.SupportsOverloadPrefixInStoredProcedureName, True);
    PropList.Values['SupportsParameterBinding'] := BoolToStr(DbInfo.SupportsParameterBinding, True);
    PropList.Values['SupportsPositionedDelete'] := BoolToStr(DbInfo.SupportsPositionedDelete, True);
    PropList.Values['SupportsPositionedUpdate'] := BoolToStr(DbInfo.SupportsPositionedUpdate, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsResultSetConcurrency'] := BoolToStr(DbInfo.SupportsResultSetConcurrency(), True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsResultSetType'] := BoolToStr(DbInfo.SupportsResultSetType(), True);
    PropList.Values['SupportsSchemasInDataManipulation'] := BoolToStr(DbInfo.SupportsSchemasInDataManipulation, True);
    PropList.Values['SupportsSchemasInIndexDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInIndexDefinitions, True);
    PropList.Values['SupportsSchemasInPrivilegeDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInPrivilegeDefinitions, True);
    PropList.Values['SupportsSchemasInProcedureCalls'] := BoolToStr(DbInfo.SupportsSchemasInProcedureCalls, True);
    PropList.Values['SupportsSchemasInTableDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInTableDefinitions, True);
    PropList.Values['SupportsSelectForUpdate'] := BoolToStr(DbInfo.SupportsSelectForUpdate, True);
    PropList.Values['SupportsStoredProcedures'] := BoolToStr(DbInfo.SupportsStoredProcedures, True);
    PropList.Values['SupportsSubqueriesInComparisons'] := BoolToStr(DbInfo.SupportsSubqueriesInComparisons, True);
    PropList.Values['SupportsSubqueriesInExists'] := BoolToStr(DbInfo.SupportsSubqueriesInExists, True);
    PropList.Values['SupportsSubqueriesInIns'] := BoolToStr(DbInfo.SupportsSubqueriesInIns, True);
    PropList.Values['SupportsSubqueriesInQuantifieds'] := BoolToStr(DbInfo.SupportsSubqueriesInQuantifieds, True);
    PropList.Values['SupportsTableCorrelationNames'] := BoolToStr(DbInfo.SupportsTableCorrelationNames, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsTransactionIsolationLevel'] := BoolToStr(DbInfo.SupportsTransactionIsolationLevel(), True);
    PropList.Values['SupportsTransactions'] := BoolToStr(DbInfo.SupportsTransactions, True);
    PropList.Values['SupportsUnion'] := BoolToStr(DbInfo.SupportsUnion, True);
    PropList.Values['SupportsUnionAll'] := BoolToStr(DbInfo.SupportsUnionAll, True);
    PropList.Values['SupportsUpdateAutoIncrementFields'] := BoolToStr(DbInfo.SupportsUpdateAutoIncrementFields, True);
    PropList.Values['UsesLocalFilePerTable'] := BoolToStr(DbInfo.UsesLocalFilePerTable, True);
    PropList.Values['UsesLocalFiles'] := BoolToStr(DbInfo.UsesLocalFiles, True);
    Result := PropList.Text;
  finally
    FreeAndNil(PropList);
  end;
end;

end.
