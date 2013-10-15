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

uses Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  {$IFDEF WITH_WIDESTRUTILS}WideStrUtils, {$ENDIF}
  ZDbcIntfs, ZCompatibility;

type
  {** Implements a class for blobs stream. }
  TZBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FBlob: IZBlob;
    FMode: TBlobStreamMode;
    FConSettings: PZConSettings;
    {$IFDEF WITH_WIDEMEMO}
    function TestEncoding: TZCharEncoding;
    {$ENDIF}
  protected
    property Blob: IZBlob read FBlob write FBlob;
    property Mode: TBlobStreamMode read FMode write FMode;
  public
    constructor Create(Field: TBlobField; Blob: IZBlob; Mode: TBlobStreamMode;
      ConSettings: PZConSettings);
    destructor Destroy; override;
  end;

implementation

uses ZFastCode, ZSysUtils, ZEncoding;

{ TZBlobStream }

{**
  Constructs this object and assignes the main properties.
  @param Blob
}
constructor TZBlobStream.Create(Field: TBlobField; Blob: IZBlob;
  Mode: TBlobStreamMode; ConSettings: PZConSettings);
var
  Buffer: Pointer;
  ASize: Integer;
begin
  inherited Create;

  FBlob := Blob;
  FMode := Mode;
  FField := Field;
  FConSettings := ConSettings;
  if (Mode in [bmRead, bmReadWrite]) and not Blob.IsEmpty then
  begin
    if Blob.IsClob then
      case Field.DataType of
        ftMemo, ftFmtMemo:
          if FConSettings^.AutoEncode then
            Buffer := Blob.GetPAnsiChar(FConSettings^.CTRL_CP)
          else
            Buffer := Blob.GetPAnsiChar(FConSettings^.ClientCodePage^.CP);
        {$IFDEF WITH_WIDEMEMO}
        ftWideMemo:
          Buffer := Blob.GetPWideChar;
        {$ENDIF}
        else
          Buffer := Blob.GetBuffer;
      end
    else
      Buffer := Blob.GetBuffer;
    ASize := Blob.Length;
    if Mode = bmRead then  //set Streambuffer from Blob
      SetPointer(Buffer, ASize)
    else
    begin //TestEncoding fails if external buffer
      Self.SetSize(ASize);
      System.Move(Buffer^, Memory^, ASize);
    end;
  end;
end;

type THackedDataset = class(TDataset);

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
{$IFDEF WITH_WIDEMEMO}
Label DataReady;
var
  US: ZWideString;
  {$ENDIF}
begin
  if Mode in [bmWrite, bmReadWrite] then
  begin
    if Assigned(Self.Memory) then
    begin
    {$IFDEF WITH_WIDEMEMO}
      if FField.DataType = ftWideMemo then
      begin
        {EH: not happy about this part. TBlobStream.LoadFromFile loads single encoded strings
        but if the Data is set by a Memo than we've got two-byte encoded strings.
        So there is NO way around to test this encoding. Acutally i've no idea about a more exact way
        than going this route...}
        case TestEncoding of  //testencoding adds two leadin null bytes
          ceDefault: //us ascii found, use faster conversion
            US := NotEmptyASCII7ToUnicodeString(Memory, Size-2);
          ceAnsi, ceUTF16: //We've to start from the premisse we've got a Unicode string in there
            begin
              if Blob.IsClob then
                Blob.SetPWideChar(Memory, (Size div 2)-1)
              else
                Blob.SetBuffer(PWideChar(US), Size-2);
              goto DataReady; //this avoids extra moving to UnicodeString and stream
            end;
          ceUTF8: US := UTF8ToString(PAnsiChar(Memory));
        end;
        if Blob.isClob then
          Blob.SetUnicodeString(US)
        else
          Blob.SetBuffer(PWideChar(US), Length(US)*2);
        DataReady:
        SetSize(Size-2);
      end
      else
    {$ENDIF}
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
  end
  else
    SetPointer(nil, 0); //don't forget! Keep Lob mem alive!

  inherited Destroy;
end;

{$IFDEF WITH_WIDEMEMO}
function TZBlobStream.TestEncoding: TZCharEncoding;
begin
  Result := ceDefault;
  Self.SetSize(Size+2);
  (PAnsiChar(Memory)+(Size-1))^ := #0;
  (PAnsiChar(Memory)+(Size-2))^ := #0;
  {EgonHugeist:
    Step one: Findout, wat's comming in! To avoid User-Bugs as good as possible
      it is possible that a PAnsiChar OR a PWideChar was written into
      the Stream!!!  And these chars could be trunced with changing the
      Stream.Size.
      I know this can lead to pain with two byte ansi chars, but what else can i do?
    step two: detect the encoding }

  if ( ZFastCode.StrLen(PAnsiChar(Memory)) < Size-2 ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16 //exact
  else
    if FConSettings.AutoEncode then
      case DetectUTF8Encoding(PAnsichar(Memory)) of
        etUSASCII: Result := ceDefault; //Exact!
        etAnsi:
          { Sure this isn't right in all cases!
            Two/four byte WideChars causing the same result!
            Leads to pain! Is there a way to get a better test?
            I've to start from the premise the function which calls this func
            should decide if ansi or unicode}
          Result := ceAnsi;
        etUTF8: Result := ceUTF8; //Exact!
      end
    else
      Result := ceDefault;
end;
{$ENDIF}

end.

