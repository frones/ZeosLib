unit ZExceptions;

{$I ZCore.inc}

interface

{$IFNDEF DO_NOT_DERIVE_FROM_EDATABASEERROR}
uses
 DB;
{$ENDIF}

// Exceptions
type
  /// <author>Fr0st</author>
  /// <summary>Defines an Abstract exception data object.</summary>
  TZExceptionSpecificData = class
  public
    function Clone: TZExceptionSpecificData; virtual; abstract;
  end;

  /// <summary>Implements an abstract SQL exception.</summary>
  EZSQLThrowable = class({$IFDEF DO_NOT_DERIVE_FROM_EDATABASEERROR}Exception{$ELSE}EDatabaseError{$ENDIF})
  private
    FErrorCode: Integer;
    FStatusCode: String;
  protected
    FSpecificData: TZExceptionSpecificData;
  public
    /// <summary>Creates an exception with message string.</summary>
    /// <param>"Msg" a error description.</param>
    constructor Create(const Msg: string);
    /// <summary>Creates an exception with message string and an ErrorCode.</summary>
    /// <param>"Msg" a error description.</param>
    /// <param>"ErrorCode" a native server error code.</param>
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    /// <summary>Creates an exception with message string and a StatusCode.</summary>
    /// <param>"StatusCode" a server status code.</param>
    /// <param>"Msg" a error description.</param>
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    /// <summary>Creates an exception with and ErrorCode, StatusCode and a
    ///  message string.</summary>
    /// <param>"ErrorCode" a native server error code.</param>
    /// <param>"StatusCode" a server status code.</param>
    /// <param>"Msg" a error description.</param>
    constructor CreateWithCodeAndStatus(ErrorCode: Integer; const StatusCode: String; const Msg: string);
    /// <summary>Creates an exception cloned from an EZSQLThrowable.</summary>
    /// <param>"E" the source we clone from.</param>
    constructor CreateClone(const E:EZSQLThrowable);
    /// <summary>Destroys this object and releases all resources.</summary>
    destructor Destroy; override;
  public
    /// <summary>Specifies the ErrorCode value of the EZSQLThrowable.</summary>
    property ErrorCode: Integer read FErrorCode;
    /// <author>FirmOS</summary>
    /// <summary>Specifies the StatusCode value of the EZSQLThrowable.</summary>
    property StatusCode: string read FStatuscode;
    /// <author>Fr0sT</summary>
    /// <summary>Specifies the Provider SpecificData of the EZSQLThrowable.</summary>
    property SpecificData: TZExceptionSpecificData read FSpecificData; // Engine-specific data
  end;

  /// <author>EgonHugeist</summary>
  /// <summary>Specifies a class of EZSQLThrowable.</summary>
  EZSQLThrowableClass = class of EZSQLThrowable;

  /// <summary>Generic SQL exception.</summary>
  EZSQLException = class(EZSQLThrowable);

  /// <summary>Generic SQL warning.</summary>
  EZSQLWarning = class(EZSQLThrowable);

  /// <summary>Requested operation is not (yet) supported by Zeos.</summary>
  EZUnsupportedException = class(EZSQLException);

  /// <summary>Generic connection lost exception.</summary>
  EZSQLConnectionLost = class(EZSQLException);

implementation

uses
 SysUtils;

{ EZSQLThrowable }

constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode := E.ErrorCode;
  FStatusCode := E.Statuscode;
  if E.SpecificData <> nil then
    FSpecificData := E.SpecificData.Clone;
end;

constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor EZSQLThrowable.CreateWithCodeAndStatus(ErrorCode: Integer;
  const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
  FStatusCode := StatusCode;
end;

constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

destructor EZSQLThrowable.Destroy;
begin
  FreeAndNil(FSpecificData);
  inherited;
end;

end.
