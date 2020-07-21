{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Database Logging Classes and Interfaces         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcLogging;

interface

{$I ZDbc.inc}

uses SysUtils, ZClasses, ZCompatibility;

type

  {** Defines a time or the message. }
  TZLoggingCategory = (lcConnect, lcDisconnect, lcTransaction, lcExecute, lcOther,
    lcPrepStmt, lcBindPrepStmt, lcExecPrepStmt, lcUnprepStmt, lcFetch, lcFetchDone);

  {** Defines a object for logging event. }
  TZLoggingEvent = class;

  {** Defines an interface to format logging events. }
  IZLoggingFormatter = interface (IZInterface)
//    ['{53559F5F-AC22-4DDC-B2EA-45D21ADDD2D5}']
    function Format(LoggingEvent: TZLoggingEvent) : RawByteString;
  end;

  { TZLoggingFormatter }
  {** Defines a object for logging event. }
  TZLoggingFormatter = class (TInterfacedObject, IZLoggingFormatter)
  public
    function Format(LoggingEvent: TZLoggingEvent) : RawByteString; virtual;
  end;

  {** Defines a object for logging event. }
  TZLoggingEvent = class (TObject)
  private
    FCategory: TZLoggingCategory;
    FProtocol: RawByteString;
    FMessage: RawByteString;
    FErrorCodeOrAffectedRows: Integer;
    FError: RawByteString;
    FTimestamp, FTimeStampStart: TDateTime;
  public
    constructor Create(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString; ErrorCodeOrAffectedRows: Integer; const Error: RawByteString;
      TimeStampStart: TDateTime = 0);

    function AsString(const LoggingFormatter: IZLoggingFormatter = nil): RawByteString;

    property Category: TZLoggingCategory read FCategory;
    property Protocol: RawByteString read FProtocol;
    property Message: RawByteString read FMessage;
    property ErrorCodeOrAffectedRows: Integer read FErrorCodeOrAffectedRows write FErrorCodeOrAffectedRows;
    property Error: RawByteString read FError;
    property Timestamp: TDateTime read FTimestamp;
    property TimeStampStart: TDateTime read FTimeStampStart;
  end;

  {** Defines an interface to accept logging events. }
  IZLoggingListener = interface (IZInterface)
    ['{53559F5F-AC22-4DDC-B2EA-45D21ADDD2D4}']
    procedure LogEvent(Event: TZLoggingEvent);
  end;

  IZLoggingObject = interface (IZInterface)
    ['{67681CC9-53D4-4350-B6C0-423ECFD88B48}']
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent;
  end;


implementation

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZSysUtils, ZDbcUtils;

var DefaultLoggingFormatter: TZLoggingFormatter;

{ TZLoggingFormatter }

function TZLoggingFormatter.Format(LoggingEvent: TZLoggingEvent): RawByteString;
var SQLWriter: TZRawSQLStringWriter;
begin
  Result := EmptyRaw;
  SQLWriter := TZRawSQLStringWriter.Create(100+Length(LoggingEvent.Message)+Length(LoggingEvent.Error));
  try
    SQLWriter.AddDateTime(LoggingEvent.Timestamp, 'yyyy-mm-dd hh:mm:ss.fff', Result);
    SQLWriter.AddText(' cat: ', Result);
    case LoggingEvent.Category of
      lcConnect: SQLWriter.AddText('Connect', Result);
      lcDisconnect: SQLWriter.AddText('Disconnect', Result);
      lcTransaction: SQLWriter.AddText('Transaction', Result);
      lcExecute: SQLWriter.AddText('Execute', Result);
      lcPrepStmt: SQLWriter.AddText('Prepare', Result);
      lcBindPrepStmt: SQLWriter.AddText('Bind prepared', Result);
      lcExecPrepStmt: SQLWriter.AddText('Execute prepared', Result);
      lcUnprepStmt: SQLWriter.AddText('Unprepare prepared', Result);
      lcFetch: SQLWriter.AddText('Fetch row(s)', Result);
      lcFetchDone: SQLWriter.AddText('Fetch complete', Result);
    else
      SQLWriter.AddText('Other', Result);
    end;
    if LoggingEvent.Protocol <> EmptyRaw then begin
      SQLWriter.AddText(', proto: ', Result);
      SQLWriter.AddText(LoggingEvent.Protocol, Result);
    end;
    if (LoggingEvent.ErrorCodeOrAffectedRows <> -1) and (
        (LoggingEvent.Category = lcExecPrepStmt) or
        (LoggingEvent.Category = lcExecute) or
        (LoggingEvent.Category = lcFetchDone) ) then begin
      if LoggingEvent.Category = lcFetchDone
      then SQLWriter.AddText(', fetched row(s): ', Result)
      else SQLWriter.AddText(', affected row(s): ', Result);
      SQLWriter.AddOrd(LoggingEvent.ErrorCodeOrAffectedRows, Result);
    end;
    if (LoggingEvent.TimeStampStart <> 0) and (
        (LoggingEvent.Category = lcExecPrepStmt) or
        (LoggingEvent.Category = lcExecute)) then begin
      SQLWriter.AddText(', elapsed time: ', Result);
      SQLWriter.AddDateTime(LoggingEvent.Timestamp-LoggingEvent.TimeStampStart, DefTimeFormatMsecs, Result);
    end;
    SQLWriter.AddText(', msg: ', Result);
    SQLWriter.AddText(LoggingEvent.Message, Result);
    if (LoggingEvent.Error <> EmptyRaw) then begin
      SQLWriter.AddText(', errcode: ', Result);
      SQLWriter.AddOrd(LoggingEvent.ErrorCodeOrAffectedRows, Result);
      SQLWriter.AddText(', error: ', Result);
      SQLWriter.AddText(LoggingEvent.Error, Result);
    end;
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
  end;
end;

{ TZLoggingEvent }

{**
  Constructs this logging event.
  @param Protocol a DBC protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
constructor TZLoggingEvent.Create(Category: TZLoggingCategory;
  const Protocol: RawByteString; const Msg: RawByteString;
  ErrorCodeOrAffectedRows: Integer; const Error: RawByteString;
  TimeStampStart: TDateTime = 0);
begin
  FCategory := Category;
  FProtocol := Protocol;
  FMessage := Msg;
  FErrorCodeOrAffectedRows := ErrorCodeOrAffectedRows;
  FError := Error;
  FTimestamp := now;
  FTimeStampStart := TimeStampStart;
end;

{**
  Gets a string representation for this event.
  @returns a string representation.
}
function TZLoggingEvent.AsString(const LoggingFormatter: IZLoggingFormatter = nil): RawByteString;
begin
  If Assigned(LoggingFormatter) then
    Result := LoggingFormatter.Format(Self)
  else
    Result := DefaultLoggingFormatter.Format(Self);
end;

initialization
  DefaultLoggingFormatter := TZLoggingFormatter.Create;

finalization
  DefaultLoggingFormatter.Free;
end.

