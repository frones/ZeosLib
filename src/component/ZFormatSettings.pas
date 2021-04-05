{*********************************************************}
{                                                         }
{              Zeos Format Setting Objects                }
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
{                                 Zeos Development Group. }
{********************************************************@}

{ constributor(s):
  Mark Ford
}

unit ZFormatSettings;

interface

{$I ZComponent.inc}
uses
  SysUtils, Classes{$IFDEF MSEgui}, mclasses{$ENDIF},
  ZCompatibility;
type
  {$IF not declared(PFormatSettings)}
  PFormatSettings = ^TFormatSettings;
  {$IFEND}
  /// <summary>Defines an event for changed format settings.</summary>
  TZOnFormatChanged = Procedure of Object;

  /// <summary>Defines format options for the second frations.</summary>
  TZSecondFractionOption = (
    /// <summary>use right zero trimmed fractions</summary>
    foRightZerosTrimmed,
    /// <summary>zero padding the fraction part as scale defines</summary>
    foScaleNormalized,
    /// <summary>strictly align fractions as specified by format. If
    ///  no fraction part is in the format the fractions are ignored</summary>
    foSetByFormat
  );

  /// <summary>implements the TZAbstractTimeFormatSettings.</summary>
  TZAbstractDateTimeFormatSettings = class(TPersistent)
  private
    FFormatSettings: TFormatSettings;
    FFormat: PString;
    FOnFormatChanged: TZOnFormatChanged;
    FInvalidValueText: String;
    {$IFDEF AUTOREFCOUNT}[WEAK]{$ENDIF}FOwner: TComponent;
    {$IFDEF AUTOREFCOUNT}[WEAK]{$ENDIF}FParent: TZAbstractDateTimeFormatSettings;
    /// <summary>Gets the format. If the format is empty and a parent is set,
    ///  the format of the parent class will be returned, except the owner
    ///  component is in designing state. If parent is empty too the result will
    ///  be assigned by calling InternalGetFromFormatSettings</summary>
    /// <returns>the format string.</returns>
    function GetFormat: String;
    /// <summary>Gets the invalid value representation. If the format is empty
    ///  and the parent object is assigned, the parents invalid value text
    ///  will be used instead, except the owner component is in designing state.</summary>
    /// <returns>the invalid value text string.</returns>
    function GetInvalidValueText: String;
    /// <summary>Sets the FormatSettings</summary>
    /// <param>"Value" the TFormatSettings record we assign the values from.</param>
    procedure SetFormatSettings(const Value: TFormatSettings);
  protected
    /// <summary>Sets the new format. OnFormatChanged will be called if assigned</summary>
    /// <param>"Value" the new format to be used.</param>
    procedure SetFormat(const Value: String); virtual; abstract;
    /// <summary>Gets the format from the global formatsettings</summary>
    /// <remarks>abstract forward implementation</remarks>
    /// <returns>the format string.</returns>
    function InternalGetFromFormatSettings: String; virtual; abstract;
    /// <summary>Gets the invalid value representation</summary>
    /// <remarks>abstract forward implementation</remarks>
    /// <returns>the invalid value string.</returns>
    function InternalGetInvalidValueText: String; virtual; abstract;
    /// <summary>Represents a text for an invalid date/time/timestamp value.
    ///  If the format is empty and the parent object is assigned, the parents
    ///  invalid value text will be used instead, except the owner component is
    ///  in designing state.</summary>
    property InvalidValueText: String read GetInvalidValueText write FInvalidValueText;
    /// <summary>Is the Format assigned?</summary>
    /// <returns><c>True</c> if so; <c>False</c> otherwise.</returns>
    function IsFormatAssigned: Boolean;
  public
    /// <summary>Gets a reference of the object TFormatSettings.</summary>
    /// <returns>the TFormatSettings reference.</returns>
    function GetFormatSettings: PFormatSettings;
    /// <summary>Assigns the values from a Source object.</summary>
    /// <param>"Source" the Source object.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"AOwner" the Owner of this object.</param>
    constructor Create(AOwner: TComponent);
    /// <summary>Set a TZOnFormatChanged event handler.</summary>
    /// <param>"Value" the event to be set.</param>
    procedure SetOnFormatChanged(Value: TZOnFormatChanged);
    /// <author>EgonHugeist</author>
    /// <summary>Replace chars in the string. Respecting the " escapes.</summary>
    /// <param>"Source" a char to search.</param>
    /// <param>"Target" Target a char to replace.</param>
    /// <param>"Str" a source string.</param>
    /// <returns>a string with replaced chars.</returns>
    class procedure ReplaceFormatChar(Var Str: string; const Source, Target: Char);
  public
    /// <summary>Represents the TFormatSettings of this object.</summary>
    property FormatSettings: TFormatSettings read FFormatSettings write SetFormatSettings;
  published
    /// <summary>Represents the Format of this object or parents format or
    ///  the format of the global FormatSettings</summary>
    property Format: String read GetFormat write SetFormat;
  end;

   /// <summary>implements the TZAbstractSecondFractionFormatSettings.</summary>
  TZAbstractSecondFractionFormatSettings = class(TZAbstractDateTimeFormatSettings)
  private
    FSecondFractionOption: TZSecondFractionOption;
    /// <summary>Gets the fraction option. If the format is empty and a parent
    ///  is set, the fraction option of the parent class will be returned.</summary>
    /// <returns>the fraction option.</returns>
    function GetSecondFractionOption: TZSecondFractionOption;
    /// <summary>Gets the fraction seperator. If the format is empty and a parent
    ///  is set, the fraction seperator of the parent class will be returned.</summary>
    /// <returns>the fraction seperator.</returns>
    function GetSecondFractionSeperator: Char;
    /// <summary>Sets the fraction option. OnFormatChanged will be called if
    ///  assigned</summary>
    /// <param>"Value" the new fraction option to be used.</param>
    procedure SetSecondFractionOption(Value: TZSecondFractionOption);
    /// <summary>Sets the fraction seperator. OnFormatChanged will be called if
    ///  assigned</summary>
    /// <param>"Value" the new fraction seperator to be used.</param>
    procedure SetSecondFractionSeperator(Value: Char);
  protected
    /// <summary>Escapes the fractions of the given format.</summary>
    /// <param>"NanoFractions" the nano second fractions.</param>
    /// <param>"Scale" the scale we align according the fraction option.</param>
    /// <param>"IsNegate" if <c>True</c> then a '-' will be added to either the
    ///  year or the hours if given.</param>
    /// <param>"Format" a reference to the format we escape in.</param>
    procedure EscapeFractionFormat(NanoFractions, Scale: Cardinal; IsNegative: Boolean; var Format: String);
    /// <summary>Represents the SecondFractionOption of this object or parents
    ///  SecondFractionOption if the Format is empty</summary>
    property SecondFractionOption: TZSecondFractionOption read GetSecondFractionOption write SetSecondFractionOption;
  public
    /// <summary>Assigns the values from a Source object.</summary>
    /// <param>"Source" the Source object.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Represents the fraction seperator of this object or parents
    ///  seperator or the decimal seperator of the global FormatSettings</summary>
    property SecondFractionSeperator: Char read GetSecondFractionSeperator write SetSecondFractionSeperator stored IsFormatAssigned;
  end;

 /// <summary>implements the TZAbstractTimeFormatSettings.</summary>
  TZAbstractTimeFormatSettings = class(TZAbstractSecondFractionFormatSettings)
  protected
    /// <summary>Gets the format from the global formatsettings</summary>
    /// <returns>the format string.</returns>
    function InternalGetFromFormatSettings: String; override;
    /// <summary>Gets the invalid value representation</summary>
    /// <returns>the invalid value string.</returns>
    function InternalGetInvalidValueText: String; override;
    /// <summary>Sets the new format. OnFormatChanged will be called if assigned</summary>
    /// <param>"Value" the new format to be used.</param>
    procedure SetFormat(const Value: String); override;
  public
    /// <summary>Attempt to convert the time values into a string</summary>
    /// <param>"Dest" a reference of the string we convert into.</param>
    /// <param>"Hours" the hours value.</param>
    /// <param>"Minutes" the minutes value.</param>
    /// <param>"Seconds" the seconds value.</param>
    /// <param>"NanoFractions" the nano fractions value.</param>
    /// <param>"Scale" the Scale to be used.</param>
    /// <param>"IsNegative" the hours value.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryTimeToString(var Dest: String; Hours, Minutes, Seconds: Word;
      NanoFractions, Scale: Cardinal; IsNegative: Boolean): Boolean;
    /// <summary>Attempt to convert a string into a TZTime value</summary>
    /// <param>"Dest" a reference of the TZTime record.</param>
    /// <param>"Source" the string we try to convert from.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryStringToTime(var Dest: TZTime; const Source: String;
      Scale: Cardinal): Boolean;
  end;

  /// <summary>implements the TZEditTimeFormatSettings.</summary>
  TZEditTimeFormatSettings = class(TZAbstractTimeFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(const Value: TZEditTimeFormatSettings);
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"AOwner" the Owner of this object.</param>
    constructor Create(AOwner: TComponent);
  published
    /// <summary>Represents the second fraction option of this object or parents
    ///  second fraction option if the format is not assigned.</summary>
    property SecondFractionOption default foSetByFormat;
  end;

  /// <summary>implements the TZDisplayTimeFormatSettings.</summary>
  TZDisplayTimeFormatSettings = Class(TZAbstractTimeFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(const Value: TZDisplayTimeFormatSettings);
  published
    /// <summary>Represents a text for an invalid date/time/timestamp value.
    ///  If the format is empty and the parent object is assigned, the parents
    ///  invalid value text will be used instead, except the owner component is
    ///  in designing state.</summary>
    property InvalidValueText stored IsFormatAssigned;
    /// <summary>Represents the second fraction option of this object or parents
    ///  second fraction option if the format is not assigned.</summary>
    property SecondFractionOption stored IsFormatAssigned default foRightZerosTrimmed;
  End;

  /// <summary>implements the TZAbstractDateFormatSettings.</summary>
  TZAbstractDateFormatSettings = class(TZAbstractDateTimeFormatSettings)
  protected
    /// <summary>Gets the format from the global formatsettings</summary>
    /// <returns>the format string.</returns>
    function InternalGetFromFormatSettings: String; override;
    /// <summary>Gets the invalid value representation</summary>
    /// <returns>the invalid value string.</returns>
    function InternalGetInvalidValueText: String; override;
    /// <summary>Sets the new format. OnFormatChanged will be called if assigned</summary>
    /// <param>"Value" the new format to be used.</param>
    procedure SetFormat(const Value: String); override;
  public
    /// <summary>Assigns the values from a Source object.</summary>
    /// <param>"Source" the Source object.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Attempt to convert the date values into a string</summary>
    /// <param>"Year" the year value.</param>
    /// <param>"Month" the month value.</param>
    /// <param>"Day" the day value.</param>
    /// <param>"IsNegative" the hours value.</param>
    /// <param>"Dest" a reference of the string we convert into.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryDateToStr(Year, Month, Day: Word; IsNegative: Boolean; var Dest: String): Boolean;
    /// <summary>Attempt to convert a string into a TZDate value</summary>
    /// <param>"Dest" a reference of the TZDate record.</param>
    /// <param>"Source" the string we try to convert from.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryStrToDate(var Dest: TZDate; const Source: String): Boolean;
  end;

  /// <summary>implements the TZEditDateFormatSettings.</summary>
  TZEditDateFormatSettings = Class(TZAbstractDateFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(const Value: TZEditDateFormatSettings);
  End;

  /// <summary>implements the TZDisplayDateFormatSettings.</summary>
  TZDisplayDateFormatSettings = Class(TZAbstractDateFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(const Value: TZDisplayDateFormatSettings);
  published
    /// <summary>Represents a text for an invalid date/time/timestamp value.
    ///  If the format is empty and the parent object is assigned, the parents
    ///  invalid value text will be used instead, except the owner component is
    ///  in designing state.</summary>
    property InvalidValueText stored IsFormatAssigned;
  End;

  /// <summary>implements the TZAbstractTimestampFormatSettings.</summary>
  TZAbstractTimestampFormatSettings = class(TZAbstractSecondFractionFormatSettings)
  private
    FDateFormat, FTimeFormat: PString;
    FDatePartOnlyIfZeroTime, FTimePartOnlyIfPascalIntegralDate: Boolean;
    /// <summary>Gets the date format used if time value is the zero. If the
    ///  value is not set the value of the parent is used or the shortdate format
    ///  of the formatsettings if the parent is not assigned.</summary>
    /// <returns>the date format string.</returns>
    function GetDateFormat: String;
    /// <summary>Sets the date format used if time value is zero.</summary>
    /// <param>"Value" the new date format string.</param>
    procedure SetDateFormat(const Value: String);
    /// <summary>Gets the time format used if date value is the pascal
    ///  integral date; alias zero. If the value is not set the value of the
    ///  parent is used or the longtime format of the formatsettings if the
    ///  parent is not assigned.</summary>
    /// <returns>the time format string.</returns>
    function GetTimeFormat: String;
    /// <summary>Sets the time format used if date value is the pascal
    ///  integral date; alias zero.</summary>
    /// <param>"Value" the new time format string.</param>
    procedure SetTimeFormat(const Value: String);
    /// <summary>Get if the date format used if time value is the zero.</summary>
    /// <returns><c>True</c> if date format used; <c>False</c> otherwise.</returns>
    function GetDatePartOnlyIfZeroTime: Boolean;
    /// <summary>Sets if the date format used in case of time value is zero.</summary>
    /// <param>"Value" the new option to be used.</param>
    procedure SetDatePartOnlyIfZeroTime(const Value: Boolean);
    /// <summary>Get if the time format used if date value is the pascal
    ///  integral date; alias zero.</summary>
    /// <returns><c>True</c> if time format used; <c>False</c> otherwise.</returns>
    function GetTimePartOnlyIfPascalIntegralDate: Boolean;
    /// <summary>Sets if the time format used in case of date value is the
    ///  pascal integral date; alias zero.</summary>
    /// <param>"Value" the new option to be used.</param>
    procedure SetTimePartOnlyIfPascalIntegralDate(const Value: Boolean);
  protected
    /// <summary>Gets the format from the global formatsettings</summary>
    /// <returns>the format string.</returns>
    function InternalGetFromFormatSettings: String; override;
    /// <summary>Gets the invalid value representation</summary>
    /// <returns>the invalid value string.</returns>
    function InternalGetInvalidValueText: String; override;
    /// <summary>Sets the new format. OnFormatChanged will be called if assigned</summary>
    /// <param>"Value" the new format to be used.</param>
    procedure SetFormat(const Value: String); override;
  protected
    /// <summary>Represents the date format used if time value is the zero. If
    ///  the value is not set the value of the parent is used or the shortdate
    ///  format of the formatsettings if the parent is not assigned.</summary>
    /// <returns>the date format string.</returns>
    property DateFormat: String read GetDateFormat write SetDateFormat;
    /// <summary>Represents the time format used if date value is the pascal
    ///  integral date; alias zero. If the value is not set the value of the
    ///  parent is used or the longtime format of the formatsettings if the
    ///  parent is not assigned.</summary>
    property TimeFormat: String read GetTimeFormat write SetTimeFormat;
    /// <summary>Represents if the date format used if time value is the zero.</summary>
    property DatePartOnlyIfZeroTime: Boolean Read GetDatePartOnlyIfZeroTime write SetDatePartOnlyIfZeroTime;
    /// <summary>Represents if the time format used if date value is the pascal
    ///  integral date; alias zero.</summary>
    property TimePartOnlyIfPascalIntegralDate: Boolean read GetTimePartOnlyIfPascalIntegralDate write SetTimePartOnlyIfPascalIntegralDate;
  public
    /// <summary>Attempt to convert the date and time values into a string</summary>
    /// <param>"Year" the year value.</param>
    /// <param>"Month" the month value.</param>
    /// <param>"Day" the day value.</param>
    /// <param>"Hours" the hours value.</param>
    /// <param>"Minutes" the minutes value.</param>
    /// <param>"Seconds" the seconds value.</param>
    /// <param>"NanoFractions" the nano fractions value.</param>
    /// <param>"Scale" the Scale to be used.</param>
    /// <param>"IsNegative" the hours value.</param>
    /// <param>"Dest" a reference of the string we convert into.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryTimestampToStr(Year, Month, Day, Hour, Minute, Second: Word;
      NanoFractions, Scale: Cardinal; IsNegative: Boolean; var Dest: String): Boolean;
    /// <summary>Attempt to convert a string into a TZTimeStamp value</summary>
    /// <param>"Dest" a reference of the TZTimeStamp record.</param>
    /// <param>"Source" the string we try to convert from.</param>
    /// <param>"Scale" the field fraction scale the string may represent.</param>
    /// <returns><c>True</c> if the conversion was successful; <c>False</c> otherwise.</returns>
    function TryStrToTimestamp(var Dest: TZTimeStamp; const Source: String;
      Scale: Cardinal): Boolean;
    /// <summary>Assigns the values from a Source object.</summary>
    /// <param>"Source" the Source object.</param>
    procedure Assign(Source: TPersistent); override;
  end;

  /// <summary>implements the TZEditTimestampFormatSettings.</summary>
  TZEditTimestampFormatSettings = Class(TZAbstractTimestampFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(Value: TZEditTimestampFormatSettings);
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"AOwner" the Owner of this object.</param>
    constructor Create(AOwner: TComponent);
  published
    /// <summary>Represents the SecondFractionOption of this object or parents
    ///  SecondFractionOption if the Format is empty</summary>
    property SecondFractionOption stored IsFormatAssigned default foSetByFormat;
  End;

  /// <summary>implements the TZDisplayTimestampFormatSettings.</summary>
  TZDisplayTimestampFormatSettings = Class(TZAbstractTimestampFormatSettings)
  public
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(Value: TZDisplayTimestampFormatSettings);
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"AOwner" the Owner of this object.</param>
    constructor Create(AOwner: TComponent);
  published
    /// <summary>Represents a text for an invalid date/time/timestamp value.
    ///  If the format is empty and the parent object is assigned, the parents
    ///  invalid value text will be used instead, except the owner component is
    ///  in designing state.</summary>
    property InvalidValueText stored IsFormatAssigned;
    /// <summary>Represents the SecondFractionOption of this object or parents
    ///  SecondFractionOption if the Format is empty</summary>
    property SecondFractionOption default foRightZerosTrimmed;
    /// <summary>Represents the date format used if time value is the zero. If
    ///  the value is not set the value of the parent is used or the shortdate
    ///  format of the formatsettings if the parent is not assigned.</summary>
    /// <returns>the date format string.</returns>
    property DateFormat;
    /// <summary>Represents the time format used if date value is the pascal
    ///  integral date; alias zero. If the value is not set the value of the
    ///  parent is used or the longtime format of the formatsettings if the
    ///  parent is not assigned.</summary>
    property TimeFormat;
    /// <summary>Represents if the date format used if time value is the zero.</summary>
    property DatePartOnlyIfZeroTime default True;
    /// <summary>Represents if the time format used if date value is the pascal
    ///  integral date; alias zero.</summary>
    property TimePartOnlyIfPascalIntegralDate default True;
  End;

  /// <summary>implements the TZFormatSettings.</summary>
  TZFormatSettings = class(TPersistent)
  private
    FDisplayDateFormatSettings: TZDisplayDateFormatSettings;
    FEditDateFormatSettings: TZEditDateFormatSettings;
    FDisplayTimeFormatSettings: TZDisplayTimeFormatSettings;
    FEditTimeFormatSettings: TZEditTimeFormatSettings;
    FDisplayTimestampFormatSettings: TZDisplayTimestampFormatSettings;
    FEditTimestampFormatSettings: TZEditTimestampFormatSettings;
    /// <summary>Sets new values for the internal display date formatsettings.</summary>
    /// <param>"Value" the new TZDisplayDateFormatSettings object we assign from.</param>
    procedure SetDisplayDateFormatSettings(const Value: TZDisplayDateFormatSettings);
    /// <summary>Sets new values for the internal display time formatsettings.</summary>
    /// <param>"Value" the new TZDisplayTimeFormatSettings object we assign from.</param>
    procedure SetDisplayTimeFormatSettings(const Value: TZDisplayTimeFormatSettings);
    /// <summary>Sets new values for the internal display timestamp formatsettings.</summary>
    /// <param>"Value" the new TZDisplayTimestampFormatSettings object we assign from.</param>
    procedure SetDisplayTimestampFormatSettings(const Value: TZDisplayTimestampFormatSettings);
    /// <summary>Sets new values for the internal edit date formatsettings.</summary>
    /// <param>"Value" the new TZEditDateFormatSettings object we assign from.</param>
    procedure SetEditDateFormatSettings(const Value: TZEditDateFormatSettings);
    /// <summary>Sets new values for the internal edit time formatsettings.</summary>
    /// <param>"Value" the new TZEditTimeFormatSettings object we assign from.</param>
    procedure SetEditTimeFormatSettings(const Value: TZEditTimeFormatSettings);
    /// <summary>Sets new values for the internal edit timestamp formatsettings.</summary>
    /// <param>"Value" the new TZEditTimestampFormatSettings object we assign from.</param>
    procedure SetEditTimestampFormatSettings(const Value: TZEditTimestampFormatSettings);
  public
    /// <summary>Assigns the values from a Source object.</summary>
    /// <param>"Source" the Source object.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Sets a parent to this object..</summary>
    /// <param>"Value" the new parent of this object.</param>
    procedure SetParent(const Value: TZFormatSettings);
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"AOwner" the Owner of this object.</param>
    constructor Create(const AOwner: TComponent);
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
  published
    property DisplayDateFormatSettings: TZDisplayDateFormatSettings read FDisplayDateFormatSettings write SetDisplayDateFormatSettings;
    property DisplayTimeFormatSettings: TZDisplayTimeFormatSettings read FDisplayTimeFormatSettings write SetDisplayTimeFormatSettings;
    property DisplayTimestampFormatSettings: TZDisplayTimestampFormatSettings read FDisplayTimestampFormatSettings write SetDisplayTimestampFormatSettings;
    property EditDateFormatSettings: TZEditDateFormatSettings read FEditDateFormatSettings write SetEditDateFormatSettings;
    property EditTimeFormatSettings: TZEditTimeFormatSettings read FEditTimeFormatSettings write SetEditTimeFormatSettings;
    property EditTimestampFormatSettings: TZEditTimestampFormatSettings read FEditTimestampFormatSettings write SetEditTimestampFormatSettings;
  end;

  function FindFirstFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;

implementation

uses ZFastCode, ZSysUtils;

function FindFirstFormatDelimiter(const Format: String; out Delimiter: Char): Boolean;
var P, PEnd: PChar;
  EscapeCount: Cardinal;
begin
  Delimiter := #0;
  Result := False;
  P := Pointer(Format);
  EscapeCount := 0;
  PEnd := P + Length(Format);
  while P < PEnd do begin
    if (P^ = '"') then
      if Odd(EscapeCount)
      then Dec(EscapeCount)
      else Inc(EscapeCount)
    else if (EscapeCount = 0) and (((Ord(P^) < Ord('0')) or ((Ord(P^) > Ord('9')))) and
       (((Ord(P^) or $20 < Ord('a')) or (Ord(P^) or $20 > Ord('z'))))) then begin
      Delimiter := P^;
      Result := True;
      Break;
    end;
    Inc(P);
  end;
end;
{ TZAbstractDateTimeFormatSettings }

procedure TZAbstractDateTimeFormatSettings.Assign(Source: TPersistent);
var ASource: TZAbstractDateTimeFormatSettings;
begin
  ASource := Source as TZAbstractDateTimeFormatSettings;
  if ASource.FFormat = nil
  then FFormat := nil
  else SetFormat(ASource.FFormat^);
  FInvalidValueText := ASource.FInvalidValueText;
end;

constructor TZAbstractDateTimeFormatSettings.Create(AOwner: TComponent);
{$IFNDEF WITH_FORMATSETTINGS}
var I: Integer;
{$ENDIF}
begin
  inherited Create;
  FOwner := AOwner;
  {$IFDEF WITH_FORMATSETTINGS}
  SetFormatSettings(SysUtils.FormatSettings);
  {$ELSE}
  FFormatSettings.CurrencyFormat := SysUtils.CurrencyFormat;
  FFormatSettings.NegCurrFormat := SysUtils.NegCurrFormat;
  FFormatSettings.ThousandSeparator := SysUtils.ThousandSeparator;
  FFormatSettings.DecimalSeparator := SysUtils.DecimalSeparator;
  FFormatSettings.CurrencyDecimals := SysUtils.CurrencyDecimals;
  FFormatSettings.DateSeparator := SysUtils.DateSeparator;
  FFormatSettings.TimeSeparator := SysUtils.TimeSeparator;
  FFormatSettings.ListSeparator := SysUtils.ListSeparator;
  FFormatSettings.CurrencyString := SysUtils.CurrencyString;
  FFormatSettings.ShortDateFormat := SysUtils.ShortDateFormat;
  FFormatSettings.LongDateFormat := SysUtils.LongDateFormat;
  FFormatSettings.TimeAMString := SysUtils.TimeAMString;
  FFormatSettings.TimePMString := SysUtils.TimePMString;
  FFormatSettings.ShortTimeFormat := SysUtils.ShortTimeFormat;
  FFormatSettings.LongTimeFormat := SysUtils.LongTimeFormat;
  for I := 1 to 12 do begin
    FFormatSettings.ShortMonthNames[I] := SysUtils.ShortMonthNames[I];
    FFormatSettings.LongMonthNames[I] := SysUtils.LongMonthNames[I];
  end;
  for I := 1 to 7 do begin
    FFormatSettings.ShortDayNames[I] := SysUtils.ShortDayNames[I];
    FFormatSettings.LongDayNames[I] := SysUtils.LongDayNames[I];
  end;
  FFormatSettings.TwoDigitYearCenturyWindow := SysUtils.TwoDigitYearCenturyWindow;
  {$ENDIF}
end;

function TZAbstractDateTimeFormatSettings.GetFormat: String;
begin
  if (FFormat <> nil) then
    Result := FFormat^
  else if (FOwner <> nil ) and (csDesigning in FOwner.ComponentState) then
    Result := ''
  else if FParent <> nil
    then Result := FParent.GetFormat
    else Result := InternalGetFromFormatSettings;
end;

function TZAbstractDateTimeFormatSettings.GetFormatSettings: PFormatSettings;
begin
  if (FFormat <> nil) or (FParent = nil)
  then Result := @FFormatSettings
  else Result := FParent.GetFormatSettings;
end;

function TZAbstractDateTimeFormatSettings.GetInvalidValueText: String;
begin
  if (FFormat <> nil) or ((FOwner <> nil ) and (csDesigning in FOwner.ComponentState)) then
    Result := FInvalidValueText
  else if FParent <> nil then
    Result := FParent.GetInvalidValueText
  else Result := InternalGetInvalidValueText;
end;

function TZAbstractDateTimeFormatSettings.IsFormatAssigned: Boolean;
begin
  Result := FFormat <> nil;
end;

class procedure TZAbstractDateTimeFormatSettings.ReplaceFormatChar(
  Var Str: string; const Source, Target: Char);
var
  P, PEnd: PChar;
  QuoteCount, DoubleQuoteCount: Cardinal;
begin
  UniqueString(Str);
  P := Pointer(Str);
  PEnd := P + Length(Str);
  QuoteCount := 0;
  DoubleQuoteCount := 0;
  while P < PEnd do begin
    if (P^ = Source) and (QuoteCount = 0) and (DoubleQuoteCount = 0) then
      P^ := Target
    else if (P^ = '"') then
      if Odd(DoubleQuoteCount)
      then Dec(DoubleQuoteCount)
      else Inc(DoubleQuoteCount)
    else if (P^ = #39) then
      if Odd(QuoteCount)
      then Dec(QuoteCount)
      else Inc(QuoteCount);
    Inc(P);
  end;
end;

procedure TZAbstractDateTimeFormatSettings.SetFormatSettings(
  const Value: TFormatSettings);
var I: Integer;
begin
  FFormatSettings.CurrencyFormat := Value.CurrencyFormat;
  FFormatSettings.NegCurrFormat := Value.NegCurrFormat;
  FFormatSettings.ThousandSeparator := Value.ThousandSeparator;
  FFormatSettings.DecimalSeparator := Value.DecimalSeparator;
  FFormatSettings.CurrencyDecimals := Value.CurrencyDecimals;
  FFormatSettings.DateSeparator := Value.DateSeparator;
  FFormatSettings.TimeSeparator := Value.TimeSeparator;
  FFormatSettings.ListSeparator := Value.ListSeparator;
  FFormatSettings.CurrencyString := Value.CurrencyString;
  FFormatSettings.ShortDateFormat := Value.ShortDateFormat;
  FFormatSettings.LongDateFormat := Value.LongDateFormat;
  FFormatSettings.TimeAMString := Value.TimeAMString;
  FFormatSettings.TimePMString := Value.TimePMString;
  FFormatSettings.ShortTimeFormat := Value.ShortTimeFormat;
  FFormatSettings.LongTimeFormat := Value.LongTimeFormat;
  for I := 1 to 12 do begin
    FFormatSettings.ShortMonthNames[I] := Value.ShortMonthNames[I];
    FFormatSettings.LongMonthNames[I] := Value.LongMonthNames[I];
  end;
  for I := 1 to 7 do begin
    FFormatSettings.ShortDayNames[I] := Value.ShortDayNames[I];
    FFormatSettings.LongDayNames[I] := Value.LongDayNames[I];
  end;
  FFormatSettings.TwoDigitYearCenturyWindow := Value.TwoDigitYearCenturyWindow;
end;

procedure TZAbstractDateTimeFormatSettings.SetOnFormatChanged(
  Value: TZOnFormatChanged);
begin
  FOnFormatChanged := Value;
end;

{ TZAbstractSecondFractionFormatSettings }

procedure TZAbstractSecondFractionFormatSettings.Assign(Source: TPersistent);
var AValue: TZAbstractSecondFractionFormatSettings;
begin
  AValue := Source as TZAbstractSecondFractionFormatSettings;
  FSecondFractionOption := AValue.FSecondFractionOption;
  inherited Assign(Source);
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors}
{$ENDIF}
procedure TZAbstractSecondFractionFormatSettings.EscapeFractionFormat(
  NanoFractions, Scale: Cardinal; IsNegative: Boolean; var Format: String);
var P, PEnd, PFractionSep, PSecond, PFractionStart, PFractionEnd, NewP, PYear, PHour: PChar;
  c: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EscapeCount, L, FractionDigitsInFormat, DigitsLeft: Cardinal;
  FormatSettings: PFormatSettings;
begin
  UniqueString(Format);
  P := Pointer(Format);
  L := Length(Format);
  PEnd := P+L;
  FractionDigitsInFormat := 0;
  EscapeCount := 0;
  PFractionSep := nil;
  PSecond := nil;
  PFractionStart := nil;
  PYear := nil;
  PHour := nil;
  PFractionEnd := nil;
  FormatSettings := GetFormatSettings;
  { first count the fraction chars given by format, assign remainders for the
    fraction seperator and (if not given one of both) the last second pos }
  while P < PEnd do begin //overflow save (min last address if the zero char)
    C := {$IFDEF UNICODE}PWord(P)^ or $0020{$ELSE}PByte(P)^ or $20{$ENDIF};
    if (P^ = Char('"')) or (P^ = #39) then
      if Odd(EscapeCount)
      then Dec(EscapeCount)
      else Inc(EscapeCount);
    if (EscapeCount = 0) then
      if ((C = Ord('z')) or (C = Ord('f'))) then begin
        if PFractionStart = nil then
          PFractionStart := P;
        PFractionEnd := P;
        Inc(FractionDigitsInFormat)
      end else if P^ = FormatSettings.DecimalSeparator then
        PFractionSep := P
      else if (C = Ord('s')) then
        PSecond := P
      else if (C = Ord('y')) and IsNegative then begin
        if PYear = nil then
          PYear := P;
        PHour := nil;
      end else if (C = Ord('h')) and IsNegative and (PYear = nil) then
        PHour := P;
    Inc(P);
  end;
  if (PSecond = nil) and (PFractionStart = nil) and (PHour = nil) and (PYear = nil) then
    Exit;
  if (PFractionSep = nil) and (PSecond <> nil) and (PFractionStart <> nil) and (PFractionStart = PSecond+2) then begin
    //try handle wrong formats where decimal-sep <> sep of given formatsettings
    //i.e. 'hh:mm:ss.zzz' but decimalsep in formatsettings is a comma f.e.
    PFractionSep := PSecond +1;
    PFractionSep^ := FormatSettings.DecimalSeparator;
  end;

  { determine amount of fraction digits -> fix scale ? }
  if SecondFractionOption = foRightZerosTrimmed then begin
    DigitsLeft := 9;
    while (NanoFractions > 0) do begin
      Scale := NanoFractions mod 10;
      if Scale <> 0
      then Break
      else NanoFractions := NanoFractions div 10;
      Dec(DigitsLeft);
    end;
    if NanoFractions = 0
    then Scale := 0
    else Scale := DigitsLeft;
  end else begin
    if SecondFractionOption = foSetByFormat then
      Scale := FractionDigitsInFormat;
    if (Scale > 0) and (GetOrdinalDigits(NanoFractions) <> Scale) then begin
      NanoFractions := RoundNanoFractionTo(NanoFractions, Scale);
      NanoFractions := NanoFractions div FractionLength2NanoSecondMulTable[Scale];
    end;
  end;
  P := Pointer(Format);
  if PFractionStart <> nil then begin
    Dec(PEnd);
    if Scale = 0 then begin
      if PFractionSep = PFractionStart-1 then
        PFractionStart := PFractionSep;
      Move((PFractionEnd+1)^, PFractionStart^, (NativeUInt(PEnd)-NativeUInt(PFractionEnd))); //backward move
      SetLength(Format, L-NativeUInt(((PFractionEnd+1)-PFractionStart)){+Ord(IsNegative)});
      Exit;
    end else if ((Scale+2) < FractionDigitsInFormat) then begin  //backward move?
      Move((PFractionEnd+1)^, (PFractionStart+1+Scale)^, (NativeUInt(PEnd)-NativeUInt(PFractionEnd))); //backward move
      L := L-(FractionDigitsInFormat-(Scale+2));
      SetLength(Format, L);
      NewP := Pointer(Format);
      PFractionStart := NewP +(PFractionStart-P);
    end else if ((Scale+2) > FractionDigitsInFormat) then begin//forward move
      SetLength(Format, L+((Scale+2)-FractionDigitsInFormat));
      NewP := Pointer(Format);
      PFractionStart := NewP +(PFractionStart-P);
      NewP := NewP + (PFractionEnd+1-P);
      Move(NewP^, (NewP+((Scale+2)-FractionDigitsInFormat))^, (NativeUInt(PEnd)-NativeUInt(PFractionEnd)));
    end;
    (PFractionStart)^ := '"';
    if Scale > 0 then
      {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(NanoFractions, PFractionStart+1, Byte(Scale));
    (PFractionStart+1+Scale)^ := '"';
  end else if (PSecond <> nil) and (Scale > 0) then begin
    PFractionEnd := PSecond+1;
    SetLength(Format, L+3+Scale);
    PFractionSep := Pointer(Format);
    Inc(PFractionSep, (PFractionEnd - P));
    EscapeCount := (NativeUInt(PEnd) - NativeUInt(PFractionEnd));
    if EscapeCount > 0 then begin
      P := PFractionSep + (3 + Scale);
      Move(PFractionSep^, P^, EscapeCount); //forward move
    end;
    (PFractionSep)^ := '"';
    (PFractionSep+1)^ := FormatSettings.DecimalSeparator;
    {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(NanoFractions, PFractionSep+2, Byte(Scale));
    (PFractionSep+2+Scale)^ := '"';
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractSecondFractionFormatSettings.GetSecondFractionOption: TZSecondFractionOption;
begin
  if (FFormat <> nil) or (FParent = nil)
  then Result := FSecondFractionOption
  else Result := TZAbstractSecondFractionFormatSettings(FParent).GetSecondFractionOption
end;

function TZAbstractSecondFractionFormatSettings.GetSecondFractionSeperator: Char;
begin
  if (FFormat <> nil) or (FParent = nil)
  then Result := GetFormatSettings.DecimalSeparator
  else Result := TZAbstractSecondFractionFormatSettings(FParent).GetSecondFractionSeperator;
end;

procedure TZAbstractSecondFractionFormatSettings.SetSecondFractionOption(
  Value: TZSecondFractionOption);
begin
  if FSecondFractionOption <> Value then begin
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
    FSecondFractionOption := Value;
  end;
end;

procedure TZAbstractSecondFractionFormatSettings.SetSecondFractionSeperator(
  Value: Char);
begin
  if (FFormat <> nil) and (FFormatSettings.DecimalSeparator <> Value) then begin
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
    FFormatSettings.DecimalSeparator := Value;
  end;
end;

{ TZAbstractTimeFormatSettings }

function TZAbstractTimeFormatSettings.InternalGetFromFormatSettings: String;
var Sep, Delim: Char;
begin
  Result := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
  if FindFirstFormatDelimiter(Result, Delim) and (Delim <> Sep) then
    ReplaceFormatChar(Result, Delim, Sep);
end;

function TZAbstractTimeFormatSettings.InternalGetInvalidValueText: String;
begin
  Result := 'NAT'
end;

procedure TZAbstractTimeFormatSettings.SetFormat(const Value: String);
begin
  if ((FFormat = nil) and (Value <> '')) or ((FFormat <> nil) and (Value = '')) or ((FFormat <> nil) and (FFormat^ <> Value)) then begin
    if Value <> '' then begin
      FFormat := @FFormatSettings.LongTimeFormat;
      FFormat^ := Value;
    end else FFormat := nil;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractTimeFormatSettings.TryStringToTime(var Dest: TZTime;
  const Source: String; Scale: Cardinal): Boolean;
var P, PStart, FEnd, PEnd: PChar;
    Fractions, FractionDigits: Cardinal;
    ExtractedCopy: String;
    DT: TDateTime;
    FractionSep: Char;
begin
  Fractions := 0;
  FractionDigits := 0;
  P := Pointer(Source);
  PStart := P;
  PEnd := PStart + Length(Source);
  FractionSep := GetSecondFractionSeperator;
  while (PStart < PEnd) do begin
    if (PStart^ = '"') then
      if Odd(FractionDigits)
      then Dec(FractionDigits)
      else Inc(FractionDigits);
    if (FractionDigits = 0) and (PStart^ = FractionSep) then
      Break;
    Inc(PStart);
  end;
  if PStart <> PEnd then begin
    Inc(PStart);
    FEnd := PStart;
    while (Ord(FEnd^) >= Ord('0')) and (Ord(FEnd^) <= Ord('9')) do
      Inc(FEnd);
    FractionDigits := FEnd - PStart;
  end else begin
    FEnd := PEnd;//satisfy compiler
    FractionDigits := 0;
  end;
  if (FractionDigits > 0) and (FractionDigits <= Cardinal(Scale)) then begin
    Fractions := {$IFDEF UNICODE}UnicodeToUInt32{$ELSE}RawToUInt32{$ENDIF}(PStart, FEnd);
    Fractions := Fractions * ZSysUtils.FractionLength2NanoSecondMulTable[FractionDigits];
    Dec(PStart);
    ExtractedCopy := '';
    FractionDigits := (NativeUInt(PStart)-NativeUInt(P));
    SetLength(ExtractedCopy, (PEnd-P)-(FEnd-PStart));
    Move(P^, Pointer(ExtractedCopy)^, FractionDigits);
    P := Pointer(NativeUInt(ExtractedCopy)+FractionDigits);
    Move(FEnd^, P^, (NativeUInt(PEnd)-NativeUInt(FEnd)));
  end else
    ExtractedCopy := Source;
  DT := StrToTime(ExtractedCopy);
  DecodeDateTimeToTime(DT, Dest);
  Dest.Fractions := Fractions;
  Result := True;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractTimeFormatSettings.TryTimeToString(var Dest: String; Hours,
  Minutes, Seconds: Word; NanoFractions, Scale: Cardinal;
  IsNegative: Boolean): Boolean;
var Frmt: String;
    DT: TDateTime;
begin
  Frmt := GetFormat;
  EscapeFractionFormat(NanoFractions, Scale, IsNegative, Frmt);
  Result := TryEncodeTime(Hours, Minutes, Seconds, Word(NanoFractions > 0), DT);
  if Result
  then Dest := FormatDateTime(Frmt, DT)
  else Dest := GetInvalidValueText;
end;

{ TZEditTimeFormatSettings }

constructor TZEditTimeFormatSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecondFractionOption := foSetByFormat;
end;

procedure TZEditTimeFormatSettings.SetParent(
  const Value: TZEditTimeFormatSettings);
begin
  FParent := Value;
end;

{ TZDisplayTimeFormatSettings }

procedure TZDisplayTimeFormatSettings.SetParent(
  const Value: TZDisplayTimeFormatSettings);
begin
  FParent := Value;
end;

{ TZAbstractDateFormatSettings }

procedure TZAbstractDateFormatSettings.Assign(Source: TPersistent);
var AValue: TZAbstractDateFormatSettings;
begin
  Avalue := Source as TZAbstractDateFormatSettings;
  FFormat := AValue.FFormat;
end;

function TZAbstractDateFormatSettings.InternalGetFromFormatSettings: String;
var Delim, Sep: Char;
begin
  Result := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
  if FindFirstFormatDelimiter(Result, Delim) and (Delim <> Sep) then
    ReplaceFormatChar(Result, Delim, Sep);
end;

function TZAbstractDateFormatSettings.InternalGetInvalidValueText: String;
begin
  Result := '0000-00-00'
end;

procedure TZAbstractDateFormatSettings.SetFormat(const Value: String);
begin
  if ((FFormat = nil) and (Value <> '')) or ((FFormat <> nil) and (Value = '')) or ((FFormat <> nil) and (FFormat^ <> Value)) then begin
    if Value <> '' then begin
      FFormat := @FFormatSettings.ShortDateFormat;
      FFormat^ := Value;
    end else FFormat := nil;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "IsNegative" not used} {$ENDIF}
function TZAbstractDateFormatSettings.TryDateToStr(Year, Month, Day: Word;
  IsNegative: Boolean; var Dest: String): Boolean;
var Frmt: String;
    DT: TDateTime;
begin
  Frmt := GetFormat;
  Result := TryEncodeDate(Year, Month, Day, DT);
  if Result
  then Dest := FormatDateTime(Frmt, DT, GetFormatSettings^)
  else Dest := GetInvalidValueText;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractDateFormatSettings.TryStrToDate(var Dest: TZDate;
  const Source: String): Boolean;
var DT: TDateTime;
begin
  Result := SysUtils.TryStrToDate(Source, DT, GetFormatSettings^);
  if Result then
    DecodeDateTimeToDate(DT, Dest);
end;

{ TZEditDateFormatSettings }

procedure TZEditDateFormatSettings.SetParent(
  const Value: TZEditDateFormatSettings);
begin
  FParent := Value;
end;

{ TZDisplayDateFormatSettings }

procedure TZDisplayDateFormatSettings.SetParent(
  const Value: TZDisplayDateFormatSettings);
begin
  FParent := Value;
end;

{ TZAbstractTimestampFormatSettings }

procedure TZAbstractTimestampFormatSettings.Assign(Source: TPersistent);
var AValue: TZAbstractTimestampFormatSettings;
begin
  AValue := Source as TZAbstractTimestampFormatSettings;
  FDatePartOnlyIfZeroTime := AValue.FDatePartOnlyIfZeroTime;
  FTimePartOnlyIfPascalIntegralDate := AValue.FTimePartOnlyIfPascalIntegralDate;
  inherited Assign(Source);
end;

function TZAbstractTimestampFormatSettings.GetDateFormat: String;
var Sep, Delim: Char;
begin
  if (FDateFormat <> nil)
  then Result := FDateFormat^
  else if (FOwner <> nil ) and (csDesigning in FOwner.ComponentState) then
    Result := ''
  else if FParent <> nil
    then Result := TZAbstractTimestampFormatSettings(FParent).GetDateFormat
    else begin
      Result := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
      Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
      if FindFirstFormatDelimiter(Result, Delim) and (Delim <> Sep) then
        ReplaceFormatChar(Result, Delim, Sep);
    end;
end;

function TZAbstractTimestampFormatSettings.GetDatePartOnlyIfZeroTime: Boolean;
begin
  if (FFormat <> nil) or (FParent = nil)
  then Result := FDatePartOnlyIfZeroTime
  else Result := TZAbstractTimestampFormatSettings(FParent).FDatePartOnlyIfZeroTime;
end;

function TZAbstractTimestampFormatSettings.GetTimeFormat: String;
var Sep, Delim: Char;
begin
  if (FTimeFormat <> nil)
  then Result := FTimeFormat^
  else if (FOwner <> nil ) and (csDesigning in FOwner.ComponentState) then
    Result := ''
  else if FParent <> nil
    then Result := TZAbstractTimestampFormatSettings(FParent).GetTimeFormat
    else begin
      Result := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
      Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
      if FindFirstFormatDelimiter(Result, Delim) and (Delim <> Sep) then
        ReplaceFormatChar(Result, Delim, Sep);
    end;
end;

function TZAbstractTimestampFormatSettings.GetTimePartOnlyIfPascalIntegralDate: Boolean;
begin
  if (FFormat <> nil) or (FParent = nil)
  then Result := FTimePartOnlyIfPascalIntegralDate
  else Result := TZAbstractTimestampFormatSettings(FParent).FTimePartOnlyIfPascalIntegralDate;
end;

function TZAbstractTimestampFormatSettings.InternalGetFromFormatSettings: String;
var tmp: String;
  Sep, Delim: Char;
begin
  tmp := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
  if FindFirstFormatDelimiter(tmp, Delim) and (Delim <> Sep) then
    ReplaceFormatChar(tmp, Delim, Sep);
  Result := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  Sep := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
  if FindFirstFormatDelimiter(Result, Delim) and (Delim <> Sep) then
    ReplaceFormatChar(Result, Delim, Sep);
  Result := Result+' '+tmp;
end;

function TZAbstractTimestampFormatSettings.InternalGetInvalidValueText: String;
begin
  Result := '0000-00-00 00:00:00'
end;

procedure TZAbstractTimestampFormatSettings.SetDateFormat(const Value: String);
begin
  if ((FDateFormat = nil) and (Value <> '')) or ((FDateFormat <> nil) and (Value = '')) or ((FDateFormat <> nil) and (FDateFormat^ <> Value)) then begin
    if Value <> '' then begin
      FDateFormat := @FFormatSettings.ShortDateFormat;
      FDateFormat^ := Value;
    end else FDateFormat := nil;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

procedure TZAbstractTimestampFormatSettings.SetDatePartOnlyIfZeroTime(
  const Value: Boolean);
begin
  if (Value <> FDatePartOnlyIfZeroTime) then begin
    FDatePartOnlyIfZeroTime := Value;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

procedure TZAbstractTimestampFormatSettings.SetFormat(const Value: String);
begin
  if ((FFormat = nil) and (Value <> '')) or ((FFormat <> nil) and (Value = '')) or ((FFormat <> nil) and (FFormat^ <> Value)) then begin
    if Value <> '' then begin
      FFormat := @FFormatSettings.LongDateFormat;
      FFormat^ := Value;
    end else FFormat := nil;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

procedure TZAbstractTimestampFormatSettings.SetTimeFormat(const Value: String);
begin
  if ((FTimeFormat = nil) and (Value <> '')) or ((FTimeFormat <> nil) and (Value = '')) or ((FTimeFormat <> nil) and (FTimeFormat^ <> Value)) then begin
    if Value <> '' then begin
      FTimeFormat := @FFormatSettings.ShortTimeFormat;
      FTimeFormat^ := Value;
    end else FTimeFormat := nil;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

procedure TZAbstractTimestampFormatSettings.SetTimePartOnlyIfPascalIntegralDate(
  const Value: Boolean);
begin
  if (Value <> FTimePartOnlyIfPascalIntegralDate) then begin
    FTimePartOnlyIfPascalIntegralDate := Value;
    if Assigned(FOnFormatChanged) then
      FOnFormatChanged;
  end;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractTimestampFormatSettings.TryStrToTimestamp(
  var Dest: TZTimeStamp; const Source: String; Scale: Cardinal): Boolean;
var P, PStart, FEnd, PEnd, DateTimeDelimiter: PChar;
    Fractions, FractionDigits: Cardinal;
    ExtractedCopy: String;
    DT: TDateTime;
    FractionSep: Char;
begin
  P := Pointer(Source);
  Fractions := 0;
  FractionDigits := 0;
  PStart := P;
  PEnd := PStart + Length(Source);
  DateTimeDelimiter := nil;
  FractionSep := GetSecondFractionSeperator;
  while (PStart < PEnd) do begin
    if (PStart^ = '"') then
      if Odd(FractionDigits)
      then Dec(FractionDigits)
      else Inc(FractionDigits);
    if (FractionDigits = 0) then begin
      if (PStart^ = FractionSep) and (DateTimeDelimiter <> nil) then
        Break;
      if (PStart^ = '.') and (DateTimeDelimiter = nil) then
        Inc(PStart)
      else if (PStart^ = ' ') or (Ord(PStart^) or $20 = Ord('t')) then
        DateTimeDelimiter := PStart;
    end;
    Inc(PStart);
  end;
  if PStart <> PEnd then begin
    Inc(PStart);
    FEnd := PStart;
    while (Ord(FEnd^) >= Ord('0')) and (Ord(FEnd^) <= Ord('9')) do
      Inc(FEnd);
    FractionDigits := FEnd - PStart;
  end else FEnd := PEnd;//satisfy compiler
  if (FractionDigits > 0) and (FractionDigits <= Cardinal(Scale)) then begin
    Fractions := {$IFDEF UNICODE}UnicodeToUInt32{$ELSE}RawToUInt32{$ENDIF}(PStart, FEnd);
    Fractions := Fractions * ZSysUtils.FractionLength2NanoSecondMulTable[FractionDigits];
    Dec(PStart);
    ExtractedCopy := '';
    FractionDigits := (NativeUInt(PStart)-NativeUInt(P));
    SetLength(ExtractedCopy, (PEnd-P)-(FEnd-PStart));
    Move(P^, Pointer(ExtractedCopy)^, FractionDigits);
    P := Pointer(NativeUInt(ExtractedCopy)+FractionDigits);
    Move(FEnd^, P^, (NativeUInt(PEnd)-NativeUInt(FEnd)));
  end else
    ExtractedCopy := Source;
  Result := TryStrToDateTime(ExtractedCopy, DT, GetFormatSettings^);
  if Result then begin
    DecodeDateTimeToTimeStamp(DT, Dest);
    Dest.Fractions := Fractions;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractTimestampFormatSettings.TryTimestampToStr(Year, Month,
  Day, Hour, Minute, Second: Word; NanoFractions, Scale: Cardinal; IsNegative: Boolean;
  var Dest: String): Boolean;
var Frmt: String;
    D, T: TDateTime;
    DateIsPascalIntegralDate: Boolean;
    TimeIsZero: Boolean;
label jmpEsc;
begin
  DateIsPascalIntegralDate := (Year = 1899) and (Month = 12) and (Day = 30) and not IsNegative;
  TimeIsZero := (Hour = 0) and (Minute = 0) and (Second = 0) and (NanoFractions = 0);
  if (FFormat = nil) then
    if TimeIsZero and GetDatePartOnlyIfZeroTime then
      Frmt := GetDateFormat
    else if DateIsPascalIntegralDate and GetTimePartOnlyIfPascalIntegralDate then begin
      Frmt := GetTimeFormat;
      goto jmpEsc;
    end else begin
      Frmt := GetFormat;
      goto jmpEsc;
    end
  else begin
    Frmt := FFormat^;
jmpEsc:
    EscapeFractionFormat(NanoFractions, Scale, IsNegative, Frmt);
  end;
  Result := TryEncodeDate(Year, Month, Day, D) and TryEncodeTime(Hour, Minute, Second, Ord(NanoFractions > 0), T);
  if Result then begin
    if D < 0
    then D := D - T
    else D := D + T;
    Dest := FormatDateTime(Frmt, D, GetFormatSettings^);
  end else Dest := GetInvalidValueText;
end;

{ TZDisplayTimestampFormatSettings }

constructor TZDisplayTimestampFormatSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatePartOnlyIfZeroTime := True;
  FTimePartOnlyIfPascalIntegralDate := True;
end;

procedure TZDisplayTimestampFormatSettings.SetParent(
  Value: TZDisplayTimestampFormatSettings);
begin
  FParent := Value;
end;

{ TZEditTimestampFormatSettings }

constructor TZEditTimestampFormatSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecondFractionOption := foSetByFormat;
  FDatePartOnlyIfZeroTime := False;
  FTimePartOnlyIfPascalIntegralDate := False;
end;

procedure TZEditTimestampFormatSettings.SetParent(Value: TZEditTimestampFormatSettings);
begin
  FParent := Value;
end;

{ TZFormatSettings }

procedure TZFormatSettings.Assign(Source: TPersistent);
var AValue: TZFormatSettings;
begin
  AValue := Source as TZFormatSettings;
  FDisplayDateFormatSettings.Assign(AValue.FDisplayDateFormatSettings);
  FEditDateFormatSettings.Assign(AValue.FEditDateFormatSettings);
  FDisplayTimeFormatSettings.Assign(AValue.FDisplayTimeFormatSettings);
  FEditTimeFormatSettings.Assign(AValue.FEditTimeFormatSettings);
  FDisplayTimestampFormatSettings.Assign(AValue.FDisplayTimestampFormatSettings);
  FEditTimestampFormatSettings.Assign(AValue.FEditTimestampFormatSettings);
end;

constructor TZFormatSettings.Create(const AOwner: TComponent);
begin
  inherited Create;
  FDisplayDateFormatSettings      := TZDisplayDateFormatSettings.Create(AOwner);
  FEditDateFormatSettings         := TZEditDateFormatSettings.Create(AOwner);
  FDisplayTimeFormatSettings      := TZDisplayTimeFormatSettings.Create(AOwner);
  FEditTimeFormatSettings         := TZEditTimeFormatSettings.Create(AOwner);
  FDisplayTimestampFormatSettings := TZDisplayTimestampFormatSettings.Create(AOwner);
  FEditTimestampFormatSettings    := TZEditTimestampFormatSettings.Create(AOwner);
end;

destructor TZFormatSettings.Destroy;
begin
  FreeAndNil(FDisplayDateFormatSettings);
  FreeAndNil(FEditDateFormatSettings);
  FreeAndNil(FDisplayTimeFormatSettings);
  FreeAndNil(FEditTimeFormatSettings);
  FreeAndNil(FDisplayTimestampFormatSettings);
  FreeAndNil(FEditTimestampFormatSettings);
  inherited;
end;

procedure TZFormatSettings.SetDisplayDateFormatSettings(
  const Value: TZDisplayDateFormatSettings);
begin
  FDisplayDateFormatSettings.Assign(Value);
end;

procedure TZFormatSettings.SetDisplayTimeFormatSettings(
  const Value: TZDisplayTimeFormatSettings);
begin
  FDisplayTimeFormatSettings.Assign(Value);
end;

procedure TZFormatSettings.SetDisplayTimestampFormatSettings(
  const Value: TZDisplayTimestampFormatSettings);
begin
  FDisplayTimestampFormatSettings.Assign(Value);
end;

procedure TZFormatSettings.SetEditDateFormatSettings(
  const Value: TZEditDateFormatSettings);
begin
  FEditDateFormatSettings.Assign(value);
end;

procedure TZFormatSettings.SetEditTimeFormatSettings(
  const Value: TZEditTimeFormatSettings);
begin
  FEditTimeFormatSettings.Assign(Value);
end;

procedure TZFormatSettings.SetEditTimestampFormatSettings(
  const Value: TZEditTimestampFormatSettings);
begin
  FEditTimestampFormatSettings.Assign(Value);
end;

procedure TZFormatSettings.SetParent(const Value: TZFormatSettings);
begin
  if Value = nil then begin
    FDisplayDateFormatSettings.SetParent(nil);
    FDisplayTimeFormatSettings.SetParent(nil);
    FDisplayTimestampFormatSettings.SetParent(nil);
    FEditDateFormatSettings.SetParent(nil);
    FEditTimeFormatSettings.SetParent(nil);
    FEditTimestampFormatSettings.SetParent(nil);
  end else begin
    FDisplayDateFormatSettings.SetParent(Value.FDisplayDateFormatSettings);
    FDisplayTimeFormatSettings.SetParent(Value.FDisplayTimeFormatSettings);
    FDisplayTimestampFormatSettings.SetParent(Value.FDisplayTimestampFormatSettings);
    FEditDateFormatSettings.SetParent(Value.FEditDateFormatSettings);
    FEditTimeFormatSettings.SetParent(Value.FEditTimeFormatSettings);
    FEditTimestampFormatSettings.SetParent(Value.FEditTimestampFormatSettings);
  end;
end;

end.
