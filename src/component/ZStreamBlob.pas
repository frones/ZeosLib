{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Blob streams classes                   }
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

unit ZStreamBlob;

interface

{$I ZComponent.inc}

uses Classes, SysUtils, ZDbcIntfs, DB, ZCompatibility;

type
  {** Implements a class for blobs stream. }
  TZBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FBlob: IZBlob;
    FMode: TBlobStreamMode;
    FConSettings: PZConSettings;
  protected
    property Blob: IZBlob read FBlob write FBlob;
    property Mode: TBlobStreamMode read FMode write FMode;
  public
    constructor Create(Field: TBlobField; Blob: IZBlob; Mode: TBlobStreamMode;
      ConSettings: PZConSettings);
    destructor Destroy; override;
  end;

implementation

uses ZEncoding;

{ TZBlobStream }

{**
  Constructs this object and assignes the main properties.
  @param Blob
}
constructor TZBlobStream.Create(Field: TBlobField; Blob: IZBlob;
  Mode: TBlobStreamMode; ConSettings: PZConSettings);
var
  TempStream: TStream;
begin
  inherited Create;

  FBlob := Blob;
  FMode := Mode;
  FField := Field;
  FConSettings := ConSettings;
  if (Mode in [bmRead, bmReadWrite]) and not Blob.IsEmpty then
  begin
    TempStream := Blob.GetStream;
    try
      TempStream.Position := 0;
      CopyFrom(TempStream, TempStream.Size);
      Position := 0;
    finally
      TempStream.Free;
    end;
  end
end;

type THackedDataset = class(TDataset);

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
var
  CLob: IZClob;
  {$IFDEF WITH_WIDEMEMO}
  TempStream: TStream;
  {$ENDIF}
  {$IFNDEF UNICODE}
  TempAnsi: RawByteString;
  {$ENDIF}
begin
  if Mode in [bmWrite, bmReadWrite] then
  begin
    if Assigned(Self.Memory) then
    begin
    {$IFDEF WITH_WIDEMEMO}
      if FField.DataType = ftWideMemo then
      begin
        if Supports(Blob, IZCLob, Clob) then
          Clob.SetPWideChar(Memory, Cardinal(Size) div 2)
        else
        begin
          TempStream := GetValidatedUnicodeStream(Memory, Cardinal(Size),
            FConSettings, False);
          Blob.SetStream(TempStream);
          TempStream.Free;
        end;
      end
      else
    {$ENDIF}
      if (FField.DataType = ftMemo) and Supports(Blob, IZCLob, Clob) then
        {$IFDEF UNICODE}
        Clob.SetPAnsiChar(Memory, ZDefaultSystemCodePage, Size)
        {$ELSE}
        if FConSettings^.AutoEncode then
        begin
          TempAnsi := GetValidatedAnsiStringFromBuffer(Memory, Size, FConSettings, FConSettings^.CTRL_CP);
          Clob.SetRawByteString(TempAnsi, FConSettings^.CTRL_CP);
        end
        else
          Clob.SetPAnsiChar(Memory, FConSettings^.ClientCodePage^.CP, Size)
        {$ENDIF}
      else
        Blob.SetStream(Self)
    end
    else
      Blob.SetStream(nil);
    try
      if Assigned(FField.Dataset) then
        THackedDataset(FField.DataSet).DataEvent(deFieldChange, ULong(FField));
    except
        ApplicationHandleException(Self);
    end;
  end;
  inherited Destroy;
end;

end.

