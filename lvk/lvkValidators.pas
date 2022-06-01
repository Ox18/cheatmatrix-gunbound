{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains various validator components. These components can
    be used together with the TlvkValidationLabel component and the
    TlvkValidationEnabler component for easy control of a GUI.
}
unit lvkValidators;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkValidators.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, lvkRegExp, lvkComponents;

type
  { Description:
      This class creates the basic framework for a validator component. A
      validator component is a component that is supposed to check the state
      of something and return a value of Valid or Not valid. Along with this,
      an error message to use if the state is invalid is also supplied.

      The TlvkValidationLabel control can refer to a number of validator
      components and show the error message, if any of them fails validation.
  }
  TlvkCustomValidator = class(TlvkComponent)
  private
    FEnabled        : Boolean;
    FInvalidMessage : string;

  protected
    procedure SetEnabled(const Value: Boolean); virtual;

    { Description:
        Set Enabled to True to enable the validator. Setting it to False will
        disable it, and not check it during validation.
    }
    property Enabled: Boolean read FEnabled write SetEnabled;

    { Description:
        If the state of the validator is invalid, this property contains an
        error message that TlvkValidationLabel will show to tell the user
        what is wrong. Use descriptive, but short messages.
    }
    property InvalidMessage: string read FInvalidMessage
      write FInvalidMessage;

    { Description:
        Descendant classes must override this method to do their work. See
        the other classes in this unit for samples on how to do that.
    }
    function DoValidate(out Complete: Boolean): Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;

    { Description:
        To check the valid/invalid state, call this function.
    }
    function Validate: Boolean;

    { Description:
        Call this function to set focus to the control that has the error. The
        function returns True if it could set focus, False if not.
    }
    function SetFocus: Boolean; virtual;
  end;

  { Description:
      This event handler is used with the TlvkEventValidator component. It is
      called whenever the component needs to check its valid/invalid state.
      Set Valid to True or False to reflect the Invalid/Invalid state.
  }
  TValidatorEvent = procedure(Sender: TObject; var Valid: Boolean) of object;

  { Description:
      This component uses an event instead of being linked to a control
      to validate itself.
  }
  TlvkEventValidator = class(TlvkCustomValidator)
  private
    FOnValidate   : TValidatorEvent;
    FFocusControl : TWinControl;

  protected
    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    { Description:
        Set this property during the validation event handler to allow the
        validator to set the focus to the control that has the error. Since the
        event validator might be linked to more than one control, you must
        explicitly set this yourself.

        Note: This property will be cleared before the validation event handler
          is called.
    }
    property FocusControl: TWinControl read FFocusControl write FFocusControl;

    // <ALIAS TlvkCustomValidator.SetFocus>
    function SetFocus: Boolean; override;

  published
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TValidatorEvent>
    property OnValidate: TValidatorEvent read FOnValidate write FOnValidate;
  end;

  // <COMBINE TlvkCustomControlValidator.TrimOptions>
  TlvkControlTrimOption = (voTrimStart, voTrimEnd);
  // <COMBINE TlvkCustomControlValidator.TrimOptions>
  TlvkControlTrimOptions = set of TlvkControlTrimOption;

  { Description:
      This component forms the basis for other validator components that checks
      the status of a control to see wether they're valid or not. Usually it's
      the text of the control they check.
  }
  TlvkCustomControlValidator = class(TlvkCustomValidator)
  private
    FControl  : TWinControl;
    FOptions  : TlvkControlTrimOptions;
    FRequired : Boolean;

  protected
    { Description:
        This property is used to specify the control to check the state of.
    }
    property Control: TWinControl read FControl write FControl;

    { Description:
        This option specifies how to prepare the text in the control for
        validation. If the voTrimStart is in this option, any spaces at the
        start of the text will be removed. Likewise, the voTrimEnd will
        remove spaces at the end of the text.
      Parameters:
        -
      See also:
        -
    }
    property TrimOptions: TlvkControlTrimOptions read FOptions write FOptions;

    { Description:
        This property is mostly an internal property. Descendants will set it
        to True to have the validator make sure we got a value before we
        check the value. If it's set to False (default for most validators),
        then a blank control text is not checked further but is considered
        a valid value.
    }
    property Required: Boolean read FRequired write FRequired default True;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    function GetControlText: string; virtual;
    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

    // <ALIAS TlvkCustomValidator.SetFocus>
    function SetFocus: Boolean; override;
  end;

  TlvkCustomRequiredValidator = class(TlvkCustomControlValidator)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component is a simple validator that just makes sure that the
      linked control contains text.
  }
  TlvkRequiredValidator = class(TlvkCustomRequiredValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  // <COMBINE TlvkCustomRegularExpressionValidator.Kind>
  TlvkRegularExpressionValidatorKind = (rkCustom, rkInteger, rkFloatingPoint,
    rkEmailAddress, rkHttpAddress, rkFtpAddress);
    
  { Description:
      This component is the basis for all other validator components
      that uses regular expressions internally to do their validation.
  }
  TlvkCustomRegularExpressionValidator = class(TlvkCustomControlValidator)
  private
    FRegularExpression  : string;
    FOptions            : TlvkRegExpOptions;
    FRegExp             : TRegularExpression;
    FKind               : TlvkRegularExpressionValidatorKind;

    procedure RecompileRegularExpression;
    procedure SetRegularExpression(const Value: string);
    procedure SetOptions(const Value: TlvkRegExpOptions);
    function IsCustom: Boolean;

  protected
    procedure SetKind(const Value: TlvkRegularExpressionValidatorKind); virtual;

    { Description:
        This property contains the regular expression to use when validating
        the control text. A match is considered valid.
    }
    property RegularExpression: string read FRegularExpression
      write SetRegularExpression stored IsCustom;

    { Description:
        This property describes options to use when doing the regular
        expression matching.
    }
    property Options: TlvkRegExpOptions read FOptions write SetOptions;

    { Description:
        Change this property to set the RegularExpression property to a preset
        expression created for a specific kind of input.
    }
    property Kind: TlvkRegularExpressionValidatorKind read FKind
      write SetKind default rkCustom;

    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component implements a simple regular-expression-based
      validator component. You can feed it a regular expression, and
      if the text of the control matches the expression, the 
      validator is valid.
  }
  TlvkRegularExpressionValidator = class(TlvkCustomRegularExpressionValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;

    // <ALIAS TlvkCustomRegularExpressionValidator.Kind>
    property Kind;
    // <ALIAS TlvkCustomRegularExpressionValidator.RegularExpression>
    property RegularExpression;
    // <ALIAS TlvkCustomRegularExpressionValidator.Options>
    property Options;
  end;

  TlvkCustomIntegerValidator = class(TlvkCustomRegularExpressionValidator)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component validates integer values in the control text.
  }
  TlvkIntegerValidator = class(TlvkCustomIntegerValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  { Description:
      This class is the basis for the TlvkRangedIntegerValidator
      component.
  }
  TlvkCustomRangedIntegerValidator = class(TlvkCustomIntegerValidator)
  private
    FMin  : Int64;
    FMax  : Int64;

  protected
    { Description:
        This property specifies the minimum value of the control text
        that is considered valid.
      See also:
        Max
    }
    property Min: Int64 read FMin write FMin;

    { Description:
        This property specifies the maximum value of the control text
        that is considered valid.
      See also:
        Min
    }
    property Max: Int64 read FMax write FMax;

    function DoValidate(out Complete: Boolean): Boolean; override;
  end;

  { Description:
      This component works like a TlvkIntegerValidator, but it also
      checks to make sure the value of the control has an integer 
      value inside the given range. The values specified are also legal.
  }
  TlvkRangedIntegerValidator = class(TlvkCustomRangedIntegerValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;

    // <ALIAS TlvkCustomRangedIntegerValidator.Min>
    property Min;
    // <ALIAS TlvkCustomRangedIntegerValidator.Max>
    property Max;
  end;

  TlvkCustomFloatValidator = class(TlvkCustomRegularExpressionValidator)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component validates floating point values.
  }
  TlvkFloatValidator = class(TlvkCustomFloatValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  TValidatorItem = class(TCollectionItem)
  private
    FValidator  : TlvkCustomValidator;

  protected
    function GetDisplayName: string; override;

  published
    { Description:
        This property is used to specify which validator this collection item
        refers to.
    }
    property Validator: TlvkCustomValidator read FValidator write FValidator;
  end;

  TValidatorCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TValidatorItem;
    procedure SetItem(Index: Integer; const Value: TValidatorItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TValidatorItem;
    function FindItemID(ID: Integer): TValidatorItem;
    function Insert(Index: Integer): TValidatorItem;
    property Items[Index: Integer]: TValidatorItem read GetItem write SetItem;
      default;
  end;

  { Description:
      This is the base framework for the Any validator.
  }
  TlvkCustomAnyValidator = class(TlvkCustomValidator)
  private
    FValidators : TValidatorCollection;

    procedure SetValidators(const Value: TValidatorCollection);

  protected
    { Description:
        This property holds a list of validators to check. If any of these
        validators are valid, the Any validator is also valid. If none is
        valid, the Any validator is invalid.
    }
    property Validators: TValidatorCollection read FValidators
      write SetValidators;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component is used to validate "any" validator amongst a set.
      Basically, if one of the listed validators are ok, then the Any
      validator is also ok.
  }
  TlvkAnyValidator = class(TlvkCustomAnyValidator)
  published
    // <ALIAS TlvkCustomAnyValidator.Validators>
    property Validators;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
  end;

  { Description:
      This component validates email addresses.
    See also:
      TlvkHTTPAddressValidator, TlvkFTPAddressValidator
  }
  TlvkEmailAddressValidator = class(TlvkCustomRegularExpressionValidator)
  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  { Description:
      This component validates http addresses.
    See also:
      TlvkFTPAddressValidator, TlvkEmailAddressValidator
  }
  TlvkHTTPAddressValidator = class(TlvkCustomRegularExpressionValidator)
  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  { Description:
      This component validates ftp addresses.
    See also:
      TlvkEmailAddressValidator, TlvkHTTPAddressValidator
  }
  TlvkFTPAddressValidator = class(TlvkCustomRegularExpressionValidator)
  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
  end;

  { Description:
      This component forms the basis for authentication validator components.
  }
  TlvkCustomAuthenticationValidator = class(TlvkCustomRegularExpressionValidator)
  private
    FMinLength          : Byte;
    FMaxLength          : Byte;
    FValidCharacters    : string;
    FTooShortMessage    : string;
    FTooLongMessage     : string;
    FInvalidAuthMessage : string;

    procedure SetMaxLength(const Value: Byte);
    procedure SetMinLength(const Value: Byte);

  protected
    procedure Changed; virtual;

    { Description:
        This property dictates how long the authentication token
        (username or password) must be before it's considered a valid token.
      See also:
        TlvkCustomAuthenticationValidator.MaxLength
    }
    property MinLength: Byte read FMinLength write SetMinLength default 0;

    { Description:
        This property dictates how long the authentication token
        (username or password) can be and still be considered a valid token.
      See also:
        TlvkCustomAuthenticationValidator.MinLength
    }
    property MaxLength: Byte read FMaxLength write SetMaxLength default 0;

    { Description:
        This property contains a regular expression set expression for
        specifying what characters are considered valid in the token.
    }
    property ValidCharacters: string read FValidCharacters
      write FValidCharacters;

    { Description:
        This error message is used if the token is too short.
    }
    property TooShortMessage: string read FTooShortMessage
      write FTooShortMessage;

    { Description:
        This error message is used if the token is too long.
    }
    property TooLongMessage: string read FTooLongMessage write FTooLongMessage;

    { Description:
        This error message is used if the token contains invalid characters.
    }
    property InvalidMessage: string read FInvalidAuthMessage
      write FInvalidAuthMessage;
    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component is used to validate usernames for various services.
      It simply checks that the entered username conforms to the rules
      for a "good" username.
  }
  TlvkUsernameValidator = class(TlvkCustomAuthenticationValidator)
  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomAuthenticationValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
    // <ALIAS TlvkCustomAuthenticationValidator.TooShortMessage>
    property TooShortMessage;
    // <ALIAS TlvkCustomAuthenticationValidator.TooLongMessage>
    property TooLongMessage;

    // <ALIAS TlvkCustomAuthenticationValidator.MinLength>
    property MinLength;
    // <ALIAS TlvkCustomAuthenticationValidator.MaxLength>
    property MaxLength;
    // <ALIAS TlvkCustomAuthenticationValidator.ValidCharacters>
    property ValidCharacters;
  end;

  { Description:
      This component is used to validate passwords for various services.
      It simply checks that the entered password conforms to the rules
      for a "good" password.
  }
  TlvkPasswordValidator = class(TlvkCustomAuthenticationValidator)
  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomAuthenticationValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;
    // <ALIAS TlvkCustomAuthenticationValidator.TooShortMessage>
    property TooShortMessage;
    // <ALIAS TlvkCustomAuthenticationValidator.TooLongMessage>
    property TooLongMessage;

    // <ALIAS TlvkCustomAuthenticationValidator.MinLength>
    property MinLength;
    // <ALIAS TlvkCustomAuthenticationValidator.MaxLength>
    property MaxLength;
    // <ALIAS TlvkCustomAuthenticationValidator.ValidCharacters>
    property ValidCharacters;
  end;

  { Description:
      This component forms the basis for the length validator component.
  }
  TlvkCustomLengthValidator = class(TlvkCustomControlValidator)
  private
    FMinLength  : LongWord;
    FMaxLength  : LongWord;

  protected
    { Description:
        This property dictates how long the control text must be before it
        is considered valid.
      See also:
        TlvkCustomLengthValidator.MaxLength
    }
    property MinLength: LongWord read FMinLength write FMinLength default 0;

    { Description:
        This property dictates how long the control text can be and still be
        considered valid.
      See also:
        TlvkCustomLengthValidator.MinLength
    }
    property MaxLength: LongWord read FMaxLength write FMaxLength default 100;

    function DoValidate(out Complete: Boolean): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component validates the control text and it is valid if the
      length of the control text is within the given range.
  }
  TlvkLengthValidator = class(TlvkCustomLengthValidator)
  published
    // <ALIAS TlvkCustomControlValidator.Control>
    property Control;
    // <ALIAS TlvkCustomValidator.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomValidator.InvalidMessage>
    property InvalidMessage;
    // <ALIAS TlvkCustomControlValidator.TrimOptions>
    property TrimOptions;

    // <ALIAS TlvkCustomLengthValidator.MinLength>
    property MinLength;
    // <ALIAS TlvkCustomLengthValidator.MaxLength>
    property MaxLength;
  end;

const
  RegularExpressionValidatorKindNames : array[TlvkRegularExpressionValidatorKind] of string = (
    'Custom',
    'Integer',
    'Floating point',
    'Email address',
    'Http address',
    'Ftp address'
  );

function RegularExpressionValidatorKindExpressions(
  const Kind: TlvkRegularExpressionValidatorKind): string;

implementation

const
  CUSTOM_REGEXP               = '^.*$';
  INTEGER_REGEXP              = '^[+\-]?[0-9]+$';
  FLOATING_POINT_REGEXP       = '^([+\-]?[0-9]+(%DECIMALSEPARATOR%[0-9]*)?(E[+\-]?[0-9]+)?|[+\-]?[0-9]*%DECIMALSEPARATOR%[0-9]+(E[+\-]?[0-9]+)?)$';

  INET_USERNAME_REGEXP        = '[a-z_0-9\-%]+';
  INET_PASSWORD_REGEXP        = INET_USERNAME_REGEXP;
  INET_AUTHENTICATION_REGEXP  = '(' + INET_USERNAME_REGEXP +
                                '(:' + INET_PASSWORD_REGEXP + ')?@)?';
  INET_DOMAIN_REGEXP          = '[a-z_0-9\-\.]+';
  INET_PORT_REGEXP            = '(:[0-9]+)?';
  INET_DOCUMENT_REGEXP        = '(/[^?]*)?';
  INET_PARAMS_REGEXP          = '(\?.*)?';
  FTP_PROTOCOL_REGEXP         = 'ftp';
  HTTP_PROTOCOL_REGEXP        = 'https?';
  EMAIL_NAME_REGEXP           = '[a-z_0-9\-\.]+';
  EMAIL_ADDRESS_REGEXP        = '^' + EMAIL_NAME_REGEXP + '@' +
                                INET_DOMAIN_REGEXP + '$';
  HTTP_ADDRESS_REGEXP         = '^' + HTTP_PROTOCOL_REGEXP + '://' +
                                INET_AUTHENTICATION_REGEXP +
                                INET_DOMAIN_REGEXP + INET_PORT_REGEXP +
                                INET_DOCUMENT_REGEXP + INET_PARAMS_REGEXP + '$';
  FTP_ADDRESS_REGEXP          = '^' + FTP_PROTOCOL_REGEXP + '://' +
                                INET_AUTHENTICATION_REGEXP +
                                INET_DOMAIN_REGEXP + INET_PORT_REGEXP +
                                INET_DOCUMENT_REGEXP + '$';

{  DATE_PART_REGEXP            = '.*';
  TIME_PART_REGEXP            = '.*';
  DATE_REGEXP                 = '^' + DATE_PART_REGEXP + '$';
  TIME_REGEXP                 = '^' + TIME_PART_REGEXP + '$';
  DATETIME_REGEXP             = '^' + DATE_PART_REGEXP + '(\s+' +
                                TIME_PART_REGEXP + ')?'; }

resourcestring
  AUTHENTICATION_TOO_LONG                 = 'Authentication token is too long';
  AUTHENTICATION_TOO_SHORT                = 'Authentication token is too short';
  AUTHENTICATION_INVALID                  = 'Authentication token username';
  USERNAME_TOO_LONG                       = 'Username is too long';
  USERNAME_TOO_SHORT                      = 'Username is too short';
  USERNAME_INVALID                        = 'Invalid username';
  PASSWORD_TOO_LONG                       = 'Password is too long';
  PASSWORD_TOO_SHORT                      = 'Password is too short';
  PASSWORD_INVALID                        = 'Invalid password';
  DEFAULT_AUTHENTICATION_VALID_CHARACTERS = 'a-z_0-9';
  REGEXP_INVALID_MESSAGE                  = 'Fill out a valid value';
  REQUIRED_INVALID_MESSAGE                = 'This field is required';
  INTEGER_INVALID_MESSAGE                 = 'Please fill in a valid integer value';
  FLOAT_INVALID_MESSAGE                   = 'Please fill in a valid floating point value';
  EMAIL_INVALID_MESSAGE                   = 'Please fill in a valid email address';
  HTTP_INVALID_MESSAGE                    = 'Please fill in a valid http address';
  FTP_INVALID_MESSAGE                     = 'Please fill in a valid ftp address';
  INVALID_LENGTH_MESSAGE                  = 'Field has an invalid length';                      

function RegularExpressionValidatorKindExpressions(
  const Kind: TlvkRegularExpressionValidatorKind): string;
const
  RegularExpressions  : array[TlvkRegularExpressionValidatorKind] of string = (
    CUSTOM_REGEXP,          // rkCustom
    INTEGER_REGEXP,         // rkInteger
    FLOATING_POINT_REGEXP,  // rkFloatingPoint
    EMAIL_ADDRESS_REGEXP,   // rkEmailAddress
    HTTP_ADDRESS_REGEXP,    // rkHttpAddress
    FTP_ADDRESS_REGEXP      // rkFtpAddress
  );
begin
  Result := RegExpStringReplace(RegularExpressions[Kind], '%DECIMALSEPARATOR%',
    '\' + DecimalSeparator, True);
end;

{ TlvkCustomValidator }

constructor TlvkCustomValidator.Create(AOwner: TComponent);
begin
  inherited;

  Enabled := True;
end;

function TlvkCustomValidator.DoValidate(out Complete: Boolean): Boolean;
begin
  Result := True;
  Complete := not Enabled;
end;

procedure TlvkCustomValidator.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TlvkCustomValidator.SetFocus: Boolean;
begin
  Result := False;
end;

function TlvkCustomValidator.Validate: Boolean;
var
  Complete  : Boolean;
begin
  Result := DoValidate(Complete);
end;

{ TlvkEventValidator }

function TlvkEventValidator.DoValidate(out Complete: Boolean): Boolean;
begin
  FFocusControl := nil;
  Result := inherited DoValidate(Complete);

  if Result and (not Complete) and Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

function TlvkEventValidator.SetFocus: Boolean;
begin
  Result := False;

  if Assigned(FFocusControl) then
  begin
    if FFocusControl.CanFocus then
    begin
      FFocusControl.SetFocus;
      Result := True;
    end;
  end;
end;

{ TlvkCustomControlValidator }

constructor TlvkCustomControlValidator.Create(AOwner: TComponent);
begin
  inherited;

  FOptions := [Low(TlvkControlTrimOption)..High(TlvkControlTrimOption)];
  FRequired := False;
end;

function TlvkCustomControlValidator.DoValidate(
  out Complete: Boolean): Boolean;
begin
  Result := inherited DoValidate(Complete);

  if Result and (not Complete) then
  begin
    if Assigned(FControl) then
    begin
      if (not Control.Enabled) or (not Control.Visible) then
        Complete := True
      else if GetControlText = '' then
      begin
        Complete := True;
        Result := not FRequired;
      end;
    end else
      Complete := True;
  end;
end;

function TlvkCustomControlValidator.GetControlText: string;
var
  Buffer  : PChar;
begin
  Result := '';

  if Assigned(FControl) then
  begin
    GetMem(Buffer, FControl.GetTextLen+1);
    if Assigned(Buffer) then
    try
      FControl.GetTextBuf(Buffer, FControl.GetTextLen+1);
      Result := Buffer;
    finally
      FreeMem(Buffer);
    end;
  end;

  if (voTrimStart in FOptions) and (voTrimEnd in FOptions) then
    Result := Trim(Result)
  else if voTrimStart in FOptions then
    Result := TrimLeft(Result)
  else if voTrimEnd in FOptions then
    Result := TrimRight(Result);
end;

procedure TlvkCustomControlValidator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FControl then
      FControl := nil;
  end;
end;

function TlvkCustomControlValidator.SetFocus: Boolean;
begin
  Result := False;

  if Assigned(FControl) then
  begin
    if FControl.CanFocus then
    begin
      FControl.SetFocus;
      Result := True;
    end;
  end;
end;

{ TlvkCustomRequiredValidator }

constructor TlvkCustomRequiredValidator.Create(AOwner: TComponent);
begin
  inherited;

  Required := True;
  InvalidMessage := REQUIRED_INVALID_MESSAGE;
end;

{ TlvkCustomRegularExpressionValidator }

constructor TlvkCustomRegularExpressionValidator.Create(
  AOwner: TComponent);
begin
  inherited;

  Kind := rkCustom;
  RegularExpression := '^.*$';
  FOptions := [roGreedy, roBol, roEol];
  InvalidMessage := REGEXP_INVALID_MESSAGE;
end;

function TlvkCustomRegularExpressionValidator.DoValidate(
  out Complete: Boolean): Boolean;
begin
  Result := inherited DoValidate(Complete);

  if Result and (not Complete) then
  begin
    if FRegularExpression <> '' then
      Result := RegExpMatch(FRegExp, GetControlText);
  end;
end;

function TlvkCustomRegularExpressionValidator.IsCustom: Boolean;
begin
  Result := FKind = rkCustom;
end;

procedure TlvkCustomRegularExpressionValidator.RecompileRegularExpression;
begin
  Finalize(FRegExp);

  if FRegularExpression <> '' then
    PrepareRegExp(FRegExp, FRegularExpression, FOptions);
end;

procedure TlvkCustomRegularExpressionValidator.SetKind(
  const Value: TlvkRegularExpressionValidatorKind);
begin
  if Value <> FKind then
  begin
    if Value = rkCustom then
    begin
      FKind := rkCustom;
      if RegularExpression = '' then
        RegularExpression := RegularExpressionValidatorKindExpressions(Value);
    end else begin
      RegularExpression := RegularExpressionValidatorKindExpressions(Value);
      FKind := Value;
    end;
  end;
end;

procedure TlvkCustomRegularExpressionValidator.SetOptions(
  const Value: TlvkRegExpOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    RecompileRegularExpression;
  end;
end;

procedure TlvkCustomRegularExpressionValidator.SetRegularExpression(
  const Value: string);
begin
  if FRegularExpression <> Value then
  begin
    FKind := rkCustom;
    FRegularExpression := Value;
    RecompileRegularExpression;
  end;
end;

{ TlvkCustomIntegerValidator }

constructor TlvkCustomIntegerValidator.Create(AOwner: TComponent);
begin
  inherited;

  Kind := rkInteger;
  InvalidMessage := INTEGER_INVALID_MESSAGE;
end;

{ TlvkCustomRangedIntegerValidator }

function TlvkCustomRangedIntegerValidator.DoValidate(
  out Complete: Boolean): Boolean;
var
  Value : Int64;
begin
  Result := inherited DoValidate(Complete);

  if Result and (not Complete) then
  begin
    try
      Value := StrToInt(GetControlText);
      Result := (Value >= FMin) and (Value <= FMax);
    except
      Result := False;
    end;
  end;
end;

{ TlvkCustomFloatValidator }

constructor TlvkCustomFloatValidator.Create(AOwner: TComponent);
begin
  inherited;

  Kind := rkFloatingPoint;
  InvalidMessage := FLOAT_INVALID_MESSAGE;
end;

{ TValidatorItem }

function TValidatorItem.GetDisplayName: string;
begin
  if Assigned(FValidator) then
    Result := FValidator.Name
  else
    Result := inherited GetDisplayName;
end;

{ TValidatorCollection }

function TValidatorCollection.Add: TValidatorItem;
begin
  Result := inherited Add as TValidatorItem;
end;

constructor TValidatorCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TValidatorItem);
end;

function TValidatorCollection.FindItemID(ID: Integer): TValidatorItem;
begin
  Result := inherited FindItemID(ID) as TValidatorItem;
end;

function TValidatorCollection.GetItem(Index: Integer): TValidatorItem;
begin
  Result := inherited Items[Index] as TValidatorItem;
end;

function TValidatorCollection.Insert(Index: Integer): TValidatorItem;
begin
  Result := inherited Insert(Index) as TValidatorItem;
end;

procedure TValidatorCollection.SetItem(Index: Integer;
  const Value: TValidatorItem);
begin
  inherited Items[Index] := Value;
end;

{ TlvkCustomAnyValidator }

constructor TlvkCustomAnyValidator.Create(AOwner: TComponent);
begin
  inherited;

  FValidators := TValidatorCollection.Create(Self);
end;

destructor TlvkCustomAnyValidator.Destroy;
begin
  FreeAndNil(FValidators);

  inherited;
end;

function TlvkCustomAnyValidator.DoValidate(out Complete: Boolean): Boolean;
var
  Index     : Integer;
  Validator : TlvkCustomValidator;
begin
  Result := inherited DoValidate(Complete);

  if Result and (not Complete) then
  begin
    Result := False;
    Index := 0;
    if Assigned(FValidators) then
    begin
      while Index < FValidators.Count do
      begin
        Validator := FValidators[Index].Validator;
        if Assigned(Validator) then
        begin
          if Validator.Validate then
          begin
            Result := True;
            Break;
          end;
        end;

        Inc(Index);
      end;
    end;
  end;
end;

procedure TlvkCustomAnyValidator.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if Assigned(FValidators) then
  begin
    Index := 0;
    while Index < FValidators.Count do
    begin
      if FValidators[Index].Validator = AComponent then
        FValidators.Delete(Index)
      else
        Inc(Index);
    end;
  end;
end;

procedure TlvkCustomAnyValidator.SetValidators(
  const Value: TValidatorCollection);
begin
  FValidators.Assign(Value);
end;

{ TlvkEmailAddressValidator }

constructor TlvkEmailAddressValidator.Create(AOwner: TComponent);
begin
  inherited;

  Kind := rkEmailAddress;
  InvalidMessage := EMAIL_INVALID_MESSAGE;
end;

{ TlvkHTTPAddressValidator }

constructor TlvkHTTPAddressValidator.Create(AOwner: TComponent);
begin
  inherited;

  Kind := rkHttpAddress;
  InvalidMessage := HTTP_INVALID_MESSAGE;
end;

{ TlvkFTPAddressValidator }

constructor TlvkFTPAddressValidator.Create(AOwner: TComponent);
begin
  inherited;

  Kind := rkFtpAddress;
  InvalidMessage := FTP_INVALID_MESSAGE;
end;

{ TlvkCustomAuthenticationValidator }

procedure TlvkCustomAuthenticationValidator.Changed;
begin
  RegularExpression := '^[' + FValidCharacters + ']*$';
end;

constructor TlvkCustomAuthenticationValidator.Create(AOwner: TComponent);
begin
  inherited;

  FMinLength := 0;
  FMaxLength := 0;
  FValidCharacters := DEFAULT_AUTHENTICATION_VALID_CHARACTERS;
  Changed;

  FTooLongMessage := AUTHENTICATION_TOO_LONG;
  FTooShortMessage := AUTHENTICATION_TOO_SHORT;
  FInvalidAuthMessage := AUTHENTICATION_INVALID;
end;

function TlvkCustomAuthenticationValidator.DoValidate(
  out Complete: Boolean): Boolean;
begin
  inherited InvalidMessage := FInvalidAuthMessage;

  Result := inherited DoValidate(Complete);

  if Result and (not Complete) then
  begin
    if FMinLength > 0 then
    begin
      if Length(GetControlText) < FMinLength then
      begin
        Result := False;
        inherited InvalidMessage := FTooShortMessage;
        Complete := True;
      end;
    end;

    if FMaxLength > 0 then
    begin
      if Length(GetControlText) > FMaxLength then
      begin
        Result := False;
        inherited InvalidMessage := FTooLongMessage;
        Complete := True;
      end;
    end;
  end;
end;

procedure TlvkCustomAuthenticationValidator.SetMaxLength(
  const Value: Byte);
begin
  if Value <> FMaxLength then
  begin
    FMaxLength := Value;
    Changed;
  end;
end;

procedure TlvkCustomAuthenticationValidator.SetMinLength(
  const Value: Byte);
begin
  if Value <> FMinLength then
  begin
    FMinLength := Value;
    Changed;
  end;
end;

{ TlvkUsernameValidator }

constructor TlvkUsernameValidator.Create(AOwner: TComponent);
begin
  inherited;

  TooLongMessage := USERNAME_TOO_LONG;
  TooShortMessage := USERNAME_TOO_SHORT;
  InvalidMessage := USERNAME_INVALID;
end;

{ TlvkPasswordValidator }

constructor TlvkPasswordValidator.Create(AOwner: TComponent);
begin
  inherited;

  TooLongMessage := PASSWORD_TOO_LONG;;
  TooShortMessage := PASSWORD_TOO_SHORT;
  InvalidMessage := PASSWORD_INVALID;
end;

{ TlvkCustomLengthValidator }

constructor TlvkCustomLengthValidator.Create(AOwner: TComponent);
begin
  inherited;

  FMinLength := 0;
  FMaxLength := 100;
  InvalidMessage := INVALID_LENGTH_MESSAGE;
end;

function TlvkCustomLengthValidator.DoValidate(
  out Complete: Boolean): Boolean;
var
  Len : Cardinal;
begin
  Complete := False;

  Len := Length(GetControlText);
  Result := (Len >= FMinLength) and (Len <= FMaxLength);
end;

end.
