{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
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

unit ZDbcSQLAnywhereUtils;

{$I ZDbc.inc}

interface

{$IFNDEF ZEOS_DISABLE_SQLANY}
uses ZPlainSQLAnywhere, ZDbcIntfs;

{**
  Converts a Sybase SQLAnywhere native types into ZDBC SQL types.
  @param native_type the sybase native type.
  @return a SQL undepended type.
}

function ConvertSQLAnyTypeToSQLType(native_type: Ta_sqlany_native_type): TZSQLType;
{$ENDIF ZEOS_DISABLE_SQLANY}
implementation
{$IFNDEF ZEOS_DISABLE_SQLANY}

{**
  Converts a Sybase SQLAnywhere native types into ZDBC SQL types.
  @param native_type the sybase native type.
  @return a SQL undepended type.
}
function ConvertSQLAnyTypeToSQLType(native_type: Ta_sqlany_native_type): TZSQLType;
begin
  case native_type of
    //DT_NOTYPE:          Result := stUnknown;
    DT_SMALLINT:        Result := stSmall;
    DT_INT:             Result := stInteger;
    DT_DECIMAL:         Result := stBigDecimal;
    DT_FLOAT:           Result := stFloat;
    DT_DOUBLE:          Result := stDouble;
    DT_DATE:            Result := stDate;
    DT_VARIABLE, DT_STRING, DT_FIXCHAR, DT_VARCHAR: Result := stString;
    DT_NSTRING, DT_NFIXCHAR, DT_NVARCHAR: Result := stUnicodeString; //just tag it
    DT_LONGNVARCHAR:    Result := stUnicodeStream; //just tag it
    DT_LONGVARCHAR:     Result := stAsciiStream;
    DT_TIME:            Result := stTime;
    DT_TIMESTAMP:       Result := stTimestamp;
    DT_TIMESTAMP_STRUCT:Result := stTimestamp;
    DT_BINARY:          Result := stBytes;
    DT_LONGBINARY:      Result := stBinaryStream;
    DT_TINYINT:         Result := stByte;
    DT_BIGINT:          Result := stLong;
    DT_UNSINT:          Result := stInteger;
    DT_UNSSMALLINT:     Result := stSmall;
    DT_UNSBIGINT:       Result := stLong;
    DT_BIT:             Result := stBoolean;
    else                Result := stUnknown;
  end;
end;
initialization
{$ENDIF ZEOS_DISABLE_SQLANY}
end.
