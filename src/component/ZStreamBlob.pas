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
    function TestEncoding: TZCharEncoding;
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

  if (Mode in [bmRead, bmReadWrite] ) and not Blob.IsEmpty then
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
    {$IFNDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    if Mode = bmReadWrite then
    begin
      WriteBuffer(Buffer^, ASize); //something courrupts the FPC-Memory-Manager here??? D7??
      Position := 0;
    end
    else
    {$ENDIF}
      SetPointer(Buffer, ASize);
  end;
end;

type THackedDataset = class(TDataset);

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
var
  ATmp: AnsiString;
  UTmp: ZWideString;
  UnCachedLob: IZUnCachedLob;
begin
  if Mode in [bmWrite, bmReadWrite] then
  begin
    Self.Position := 0;
    {EH: speed upgrade:
     instead of moving mem from A to B i set the mem-pointer to the lobs instead.
     But we have to validate the mem if required.. }

    if Memory <> nil then
    begin
      case FField.DataType of
        {$IFDEF WITH_WIDEMEMO}ftWideMemo, {$ENDIF} ftMemo:
          if Blob.IsClob then
            {EH: not happy about this part. TBlobStream.LoadFromFile loads single encoded strings
            but if the Data is set by a Memo than we've got two-byte encoded strings.
            So there is NO way around to test this encoding. Acutally i've no idea about a more exact way
            than going this route...}
            {$IFDEF WITH_WIDEMEMO}
            if FField.DataType = ftWideMemo then
              case TestEncoding of  //testencoding adds two leadin null bytes
                ceDefault: //us ascii found, use faster conversion
                  {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                  Blob.SetBlobData(Memory, Size -1, ZEncoding.zCP_us_ascii); //use only one #0 terminator
                  {$ELSE} //need to move data
                  Blob.SetPAnsiChar(Memory, ZEncoding.zCP_us_ascii, Size -2);
                  {$ENDIF}
                ceAnsi, ceUTF16: //We've to start from the premisse we've got a Unicode string in there
                  {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                  Blob.SetBlobData(Memory, Size, ZEncoding.zCP_UTF16); //use the #0#0 terminator
                  {$ELSE} //need to move data
                  Blob.SetPWideChar(Memory, (Size -2) div 2);
                  {$ENDIF}
                ceUTF8:
                  {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                  Blob.SetBlobData(Memory, Size -1, ZEncoding.zCP_UTF8); //use only one #0 terminator
                  {$ELSE} //need to move data
                  Blob.SetPAnsiChar(Memory, ZEncoding.zCP_UTF8, Size -2);
                  {$ENDIF}
              end
            else
            {$ENDIF}
              if FConSettings^.AutoEncode then
                case TestEncoding of  //testencoding adds two leadin null bytes
                  ceDefault: //us ascii found, use faster conversion
                    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                    Blob.SetBlobData(Memory, Size -1, ZEncoding.zCP_us_ascii); //use only one #0 terminator
                    {$ELSE} //need to move data
                    Blob.SetPAnsiChar(Memory, ZEncoding.zCP_us_ascii, Size -2);
                    {$ENDIF}
                  ceUTF16: //We've to start from the premisse we've got a Unicode string in there
                    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                    Blob.SetBlobData(Memory, Size, ZEncoding.zCP_UTF16); //use the #0#0 terminator
                    {$ELSE} //need to move data
                    Blob.SetPWideChar(Memory, (Size -2) div 2);
                    {$ENDIF}
                  ceAnsi:
                    if (ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, zCP_UTF8)) then
                      if (ZCompatibleCodePages(FConSettings^.CTRL_CP, zCP_UTF8)) then
                        if (ZCompatibleCodePages(ZOSCodePage, zCP_UTF8)) then
                        {no idea what to do with ansiencoding, if everything if set to UTF8!}
                        begin
                          SetLength(ATmp, Size-2);
                          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Memory^, Pointer(ATmp)^, Size -2);
                          UTmp := ZWideString(ATmp); //random success
                          Blob.SetPWideChar(Pointer(UTmp), Length(UTmp));
                        end
                        else
                          {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                          Blob.SetBlobData(Memory, Size -1, ZOSCodePage) //use only one #0 terminator
                          {$ELSE} //need to move data
                          Blob.SetPAnsiChar(Memory, ZOSCodePage, Size -2)
                          {$ENDIF}
                      else
                        {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                        Blob.SetBlobData(Memory, Size -1, FConSettings^.CTRL_CP) //use only one #0 terminator
                        {$ELSE} //need to move data
                        Blob.SetPAnsiChar(Memory, FConSettings^.CTRL_CP, Size -2)
                        {$ENDIF}
                    else
                      {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                      Blob.SetBlobData(Memory, Size -1, FConSettings^.ClientCodePage^.CP); //use only one #0 terminator
                      {$ELSE} //need to move data
                      Blob.SetPAnsiChar(Memory, FConSettings^.ClientCodePage^.CP, Size -2);
                      {$ENDIF}
                  ceUTF8:
                    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                    Blob.SetBlobData(Memory, Size -1, ZEncoding.zCP_UTF8); //use only one #0 terminator
                    {$ELSE} //need to move data
                    Blob.SetPAnsiChar(Memory, ZEncoding.zCP_UTF8, Size -2);
                    {$ENDIF}
                end
              else
                {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
                begin
                  Self.SetSize(Size+1);
                  (PAnsiChar(Memory)+Size-1)^ := #0; //add leading terminator
                  Blob.SetBlobData(Memory, Size, FConSettings^.ClientCodePage^.CP); //use only one #0 terminator
                end
                {$ELSE} //need to move data
                Blob.SetPAnsiChar(Memory, FConSettings^.ClientCodePage^.CP, Size)
                {$ENDIF}
          else
            {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
            Blob.SetBlobData(Memory, Size);
            {$ELSE} //need to move data
            Blob.SetBuffer(Memory, Size);
            {$ENDIF}
        else
          {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
          Blob.SetBlobData(Memory, Size);
          {$ELSE} //need to move data
          Blob.SetBuffer(Memory, Size);
          {$ENDIF}
      end;
      {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
      SetPointer(nil, 0); //don't forget! Keep Lob mem alive!
      {$ENDIF}
    end
    else
      Blob.Clear;
    //try
      if Assigned(FField.Dataset) then
        THackedDataset(FField.DataSet).DataEvent(deFieldChange, NativeInt(FField));
    //except ApplicationHandleException(Self); end; //commented see https://sourceforge.net/p/zeoslib/tickets/226/
  end
  else
  begin
    SetPointer(nil, 0); //don't forget! Keep Lob mem alive!
    if Supports(Blob, IZUnCachedLob, UnCachedLob) then
      UnCachedLob.FlushBuffer;
  end;

  inherited Destroy;
end;

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

  if (Size mod 2 = 0) and ( ZFastCode.StrLen(PAnsiChar(Memory)) < Size-2 ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16 //exact
  else
    if FConSettings.AutoEncode then
      case ZDetectUTF8Encoding(Memory, Size -2) of
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

end.

