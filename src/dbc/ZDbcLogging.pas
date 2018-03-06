{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Database Logging Classes and Interfaces         }
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

unit ZDbcLogging;

interface

{$I ZDbc.inc}

uses SysUtils, ZClasses, ZCompatibility;

type

  {** Defines a time or the message. }
  TZLoggingCategory = (lcConnect, lcDisconnect, lcTransaction, lcExecute, lcOther,
    lcPrepStmt, lcBindPrepStmt, lcExecPrepStmt, lcUnprepStmt);

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
  private
  public
    function Format(LoggingEvent: TZLoggingEvent) : RawByteString; virtual;
  end;

  {** Defines a object for logging event. }
  TZLoggingEvent = class (TObject)
  private
    FCategory: TZLoggingCategory;
    FProtocol: RawByteString;
    FMessage: RawByteString;
    FErrorCode: Integer;
    FError: RawByteString;
    FTimestamp: TDateTime;
  public
    constructor Create(Category: TZLoggingCategory; const Protocol: RawByteString;
      const Msg: RawByteString; ErrorCode: Integer; const Error: RawByteString);

    function AsString(const LoggingFormatter: IZLoggingFormatter = nil): RawByteString;

    property Category: TZLoggingCategory read FCategory;
    property Protocol: RawByteString read FProtocol;
    property Message: RawByteString read FMessage;
    property ErrorCode: Integer read FErrorCode;
    property Error: RawByteString read FError;
    property Timestamp: TDateTime read FTimestamp;
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
  ZFastCode, ZSysUtils;

var DefaultLoggingFormatter: TZLoggingFormatter;

{ TZLoggingFormatter }

function TZLoggingFormatter.Format(LoggingEvent: TZLoggingEvent): RawByteString;
begin
  Result := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FormatDateTime('yyyy-mm-dd hh:mm:ss', LoggingEvent.Timestamp)) + ' cat: ';
  case LoggingEvent.Category of
    lcConnect: Result := Result + 'Connect';
    lcDisconnect: Result := Result + 'Disconnect';
    lcTransaction: Result := Result + 'Transaction';
    lcExecute: Result := Result + 'Execute';
    lcPrepStmt: Result := Result + 'Prepare';
    lcBindPrepStmt: Result := Result + 'Bind prepared';
    lcExecPrepStmt: Result := Result + 'Execute prepared';
    lcUnprepStmt: Result := Result + 'Unprepare prepared';
  else
    Result := Result + 'Other';
  end;
  if LoggingEvent.Protocol <> '' then
    Result := Result + ', proto: ' + LoggingEvent.Protocol;
  Result := Result + ', msg: ' + LoggingEvent.Message;
  if (LoggingEvent.ErrorCode <> 0) or (LoggingEvent.Error <> '') then
  begin
    Result := Result + ', errcode: ' + IntToRaw(LoggingEvent.ErrorCode)
      + ', error: ' + LoggingEvent.Error;
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
  const Protocol: RawByteString; const Msg: RawByteString; ErrorCode: Integer; const Error: RawByteString);
begin
  FCategory := Category;
  FProtocol := Protocol;
  FMessage := Msg;
  FErrorCode := ErrorCode;
  FError := Error;
  FTimestamp := Now;
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

