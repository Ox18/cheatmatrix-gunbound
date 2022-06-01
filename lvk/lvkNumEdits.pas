{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains two controls for inputing number values on a form, one
    for integer values, and one for floating point values.
}
unit lvkNumEdits;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkNumEdits.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Messages, lvkEdits, lvkTypes;

type
  TTextOperation = (toAdd, toDelete);

  { Description:
      This is the base class for the two number edit controls. It implements
      the common code between the two.
  }
  TlvkCustomNumEdit = class(TlvkCustomEdit)
  protected
    function ValidAfter(const Operation: TTextOperation;
      const AdditionalText: string=''): Boolean; virtual;
    function IsValid(const Text: string): Boolean; virtual; abstract;

    function GetAsInteger: Int32; virtual; abstract;
    procedure SetAsInteger(const Value: Int32); virtual; abstract;
    function GetAsFloat: Extended; virtual; abstract;
    procedure SetAsFloat(const Value: Extended); virtual; abstract;

  public
    { Description:
        The AsInteger property can be used to read and write the integer value
        of the control in question. Notice that if you read the integer value
        of the floating point control, the value will be truncated.
      See also:
        AsFloat
    }
    property AsInteger: Int32 read GetAsInteger write SetAsInteger;

    { Description:
        The AsFloat property can be used to read and write the floating point
        value of the control in question as an Extended value. Notice that
        if you write the floating point value of the integer control, the
        value will be truncated.
      See also:
        AsInteger
    }
    property AsFloat: Extended read GetAsFloat write SetAsFloat;

    property Alignment default taRightJustify;
  end;

  { Description:
      This is the custom version of the TlvkIntegerEdit control, and implements
      all the code necessary to function as a integer numeric edit control.
    See also:
      TlvkIntegerEdit, TlvkCustomFloatEdit, TlvkFloatEdit
  }
  TlvkCustomIntegerEdit = class(TlvkCustomNumEdit)
  private
    FMinValue   : Int32;
    FMaxValue   : Int32;
    FBlankValue : Int32;
    FUseBlank   : Boolean;

    procedure SetMaxValue(const Value: Int32);
    procedure SetMinValue(const Value: Int32);
    procedure SetUseBlank(const Value: Boolean);
    procedure SetBlankValue(const Value: Int32);

    procedure AdjustMinMax;
    procedure AdjustBlank;

  protected
    function IsValid(const Text: string): Boolean; override;

    function GetAsInteger: Int32; override;
    procedure SetAsInteger(const Value: Int32); override;
    function GetAsFloat: Extended; override;
    procedure SetAsFloat(const Value: Extended); override;

    { Description:
        This property gives read/write access to the value stored in the
        edit control. It will function just like the AsInteger property
        inherited from TlvkCustomNumEdit
    }
    property Value: Int32 read GetAsInteger write SetAsInteger;

    function GetText: string;
    { Description:
        This property gives read-access to the actual text shown in the edit
        control.
    }
    property Text: string read GetText;

    procedure KeyPress(var Key: Char); override;

    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

    { Description:
        The MinValue property controls the minimum value you can store in the
        control. You will not be allowed to type in a value that has a lower
        value than the minimum value. If you set the value of the edit
        control from code to a lower value, it will be clamped, ie. adjusted
        to the minimum value.
      See also:
        MaxValue
    }
    property MinValue: Int32 read FMinValue write SetMinValue default MIN_INT_32;

    { Description:
        The MaxValue property controls the maximum value you can store in the
        control. You will not be allowed to type in a value that has a higher
        value than the maximum value. If you set the value of the edit
        control from code to a higher value, it will be clamped, ie. adjusted
        to the maximum value.
      See also:
        MaxValue
    }
    property MaxValue: Int32 read FMaxValue write SetMaxValue default MAX_INT_32;

    { Description:
        This property controls what value is considered to the "blank value".
        If the user leaves the control with this value in it, and the
        UseBlank property is True, the edit control will be blanked out.

        If the edit control is blank, reading the Value property will yield
        the value of this property.
      See also:
        UseBlank
    }
    property BlankValue: Int32 read FBlankValue write SetBlankValue default 0;

    { Description:
        When the user leaves the control, if the contents of it is blank,
        and this property is True, the contents are left as blank.

        If this property is False, the contents is replaced by the value of
        the BlankValue property instead.

        In addition, if the value of the control is the same as the BlankValue,
        and the UseBlank property is True, when the user leaves the control,
        it will be blanked.
      See also:
        BlankValue
    }
    property UseBlank: Boolean read FUseBlank write SetUseBlank default False;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This control is the integer edit control. It will allow you to input
      integer values only. The edit control will enforce the format of the
      integer value so you won't be able to add non-digit characters,
      sign characters at improper places, etc. This includes trying to paste
      illegal text into it.
  }
  TlvkIntegerEdit = class(TlvkCustomIntegerEdit)
  public
    // <ALIAS TlvkCustomIntegerEdit.Text>
    property Text;

  published
    // From TlvkCustomIntegerEdit
    // <ALIAS TlvkCustomIntegerEdit.Value>
    property Value;
    // <ALIAS TlvkCustomIntegerEdit.MinValue>
    property MinValue;
    // <ALIAS TlvkCustomIntegerEdit.MaxValue>
    property MaxValue;
    // <ALIAS TlvkCustomIntegerEdit.BlankValue>
    property BlankValue;
    // <ALIAS TlvkCustomIntegerEdit.UseBlank>
    property UseBlank;

    // From TEdit
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    // <ALIAS TlvkCustomEdit.Color>
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    // From TlvkCustomEdit
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
  end;

  TRoundMode = (rmTruncate, rmRoundNearest);

  { Description:
      This is the custom version of the TlvkFloatEdit control, and implements
      all the code necessary to function as a floating point numeric edit
      control.
    See also:
      TlvkFloatEdit, TlvkCustomIntegerEdit, TlvkIntegerEdit
  }
  TlvkCustomFloatEdit = class(TlvkCustomNumEdit)
  private
    FMinValue         : Extended;
    FMaxValue         : Extended;
    FBlankValue       : Extended;
    FUseBlank         : Boolean;
    FDecimalSeparator : Char;
    FDecimalPlaces    : ShortInt;
    FRoundMode        : TRoundMode;

    procedure SetMaxValue(const Value: Extended);
    procedure SetMinValue(const Value: Extended);
    procedure SetUseBlank(const Value: Boolean);
    procedure SetBlankValue(const Value: Extended);

    procedure AdjustMinMax;
    procedure AdjustBlank;
    procedure SetDecimalSeparator(const Value: Char);
    procedure SetDecimalPlaces(const Value: ShortInt);

  protected
    function CurrentDecimalSeparator: Char;
    function IsValid(const Text: string): Boolean; override;

    function GetAsInteger: Int32; override;
    procedure SetAsInteger(const Value: Int32); override;
    function GetAsFloat: Extended; override;
    procedure SetAsFloat(const Value: Extended); override;

    { Description:
        This property gives read/write access to the value stored in the
        edit control. It will function just like the AsFloat property
        inherited from TlvkCustomNumEdit
    }
    property Value: Extended read GetAsFloat write SetAsFloat;

    function GetText: string;

    { Description:
        This property gives read-access to the actual text shown in the edit
        control.
    }
    property Text: string read GetText;
    procedure Reformat; virtual;

    procedure KeyPress(var Key: Char); override;

    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

    // <ALIAS TlvkCustomIntegerEdit.MinValue>
    property MinValue: Extended read FMinValue write SetMinValue;
    // <ALIAS TlvkCustomIntegerEdit.MaxValue>
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    // <ALIAS TlvkCustomIntegerEdit.BlankValue>
    property BlankValue: Extended read FBlankValue write SetBlankValue;
    // <ALIAS TlvkCustomIntegerEdit.UseBlank>
    property UseBlank: Boolean read FUseBlank write SetUseBlank default False;

    { Description:
        This property can be used to override the decimal separator character
        used. By default, the control will use the current system default.
        If you want a consistent user interface, set it to '.' or ',' to
        override and always use that character. Leave it as #0 to have it
        use the system default.

        Note: This control does not react to system default changes while on
          the screen.
    }
    property DecimalSeparator: Char read FDecimalSeparator
      write SetDecimalSeparator default #0;

    { Description:
        This property controls how many digits to show and store after the
        decimal separator. Set it to 0 to make it into a sort-of integer
        edit control. Set it to 2 to have 2 decimals.

        The way decimal places work is that if the value of the control is set
        from code, the value will be rounded or truncated to the number of
        decimals given in this property.

        The default value is -1, which means no specific setting.
      See also:
        RoundMode
    }
    property DecimalPlaces: ShortInt read FDecimalPlaces write SetDecimalPlaces
      default -1;

    { Description:
        The control supports to methods for adjusting numbers that have more
        decimal places than the control allows for: truncating and rounding.

        If you set it to truncate, the value 9.555 will be truncated to 9.55
        if 2 decimal places are used.

        The same value will be rounded to 9.56 with 2 decimal places.
      See also:
        DecimalPlaces
    }
    property RoundMode: TRoundMode read FRoundMode write FRoundMode
      default rmTruncate;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This control is the floating point edit control. It will allow you to
      input floating point values only. The edit control will enforce the format
      of the floating point value so you won't be able to add non-digit
      characters, sign characters at improper places, etc. This includes trying
      to paste illegal text into it.
  }
  TlvkFloatEdit = class(TlvkCustomFloatEdit)
  public
    // <ALIAS TlvkCustomFloatEdit.Text>
    property Text;

  published
    // From TlvkCustomIntegerEdit
    // <ALIAS TlvkCustomIntegerEdit.Value>
    property Value;
    // <ALIAS TlvkCustomIntegerEdit.MinValue>
    property MinValue;
    // <ALIAS TlvkCustomIntegerEdit.MaxValue>
    property MaxValue;
    // <ALIAS TlvkCustomIntegerEdit.BlankValue>
    property BlankValue;
    // <ALIAS TlvkCustomIntegerEdit.UseBlank>
    property UseBlank;
    // <ALIAS TlvkCustomFloatEdit.DecimalSeparator>
    property DecimalSeparator;
    // <ALIAS TlvkCustomFloatEdit.DecimalPlaces>
    property DecimalPlaces;
    // <ALIAS TlvkCustomFloatEdit.RoundMode>
    property RoundMode;

    // From TEdit
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    // <ALIAS TlvkCustomEdit.Color>
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    // From TlvkCustomEdit
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
  end;

implementation

uses
  Math, Windows, Clipbrd;

function FirstClipboardLine: string;
var
  Index : Integer;
begin
  Result := Clipboard.AsText;

  Index := Pos(#13, Result);
  if Index > 0 then
    Delete(Result, Index, Length(Result) - Index + 1);

  Index := Pos(#10, Result);
  if Index > 0 then
    Delete(Result, Index, Length(Result) - Index + 1);
end;

{ TlvkCustomNumEdit }

function TlvkCustomNumEdit.ValidAfter(const Operation: TTextOperation;
  const AdditionalText: string): Boolean;
var
  Text  : string;
begin
  Text := inherited Text;

  case Operation of
    toDelete:
      if SelLength > 0 then
        Delete(Text, SelStart+1, SelLength)
      else
        Delete(Text, SelStart+1, 1);

    toAdd:
      if (SelStart = 0) and (SelLength = Length(Text)) then
        Text := AdditionalText
      else begin
        if SelLength > 0 then
          Delete(Text, SelStart+1, SelLength);
        Insert(AdditionalText, Text, SelStart+1);
      end;
  end;

  Result := IsValid(Text);
end;

{ TlvkCustomIntegerEdit }

procedure TlvkCustomIntegerEdit.AdjustBlank;
begin
  if FUseBlank then
  begin
    if Value = FBlankValue then
      inherited Text := '';
  end else begin
    if inherited Text = '' then
      inherited Text := IntToStr(FBlankValue);
  end;
end;

procedure TlvkCustomIntegerEdit.AdjustMinMax;
begin
  if inherited Text <> '' then
  begin
    if Value < FMinValue then
      Value := FMinValue
    else if Value > FMaxValue then
      Value := FMaxValue;
  end;
end;

constructor TlvkCustomIntegerEdit.Create(AOwner: TComponent);
begin
  inherited;

  Alignment := taRightJustify;
  FMinValue := MIN_INT_32;
  FMaxValue := MAX_INT_32;
  FBlankValue := 0;

  SetAsInteger(0);
end;

function TlvkCustomIntegerEdit.GetAsFloat: Extended;
begin
  Result := GetAsInteger;
end;

function TlvkCustomIntegerEdit.GetAsInteger: Int32;
begin
  if inherited Text = '' then
    Result := FBlankValue
  else
    Result := StrToInt(inherited Text);
end;

function TlvkCustomIntegerEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TlvkCustomIntegerEdit.IsValid(const Text: string): Boolean;
var
  Index     : Integer;
  GotDigits : Boolean;
  Value     : Int32;
begin
  Result := True;
  GotDigits := False;

  if Text <> '' then
  begin
    Index := 1;

    if (Index <= Length(Text)) and (Text[Index] in ['-', '+']) then
      Inc(Index);

    while Index <= Length(Text) do
    begin
      if Text[Index] in ['0'..'9'] then
      begin
        GotDigits := True;
        Inc(Index);
      end else begin
        Result := False;
        Break;
      end;
    end;
  end;

  if Result and GotDigits then
  begin
    try
      Value := StrToInt(Text);
      if (Value < FMinValue) or (Value > FMaxValue) then
        Result := False; 
    except
      Result := False;
    end;
  end;
end;

procedure TlvkCustomIntegerEdit.KeyPress(var Key: Char);
begin
  case Key of
    #3, #23, #24: // Ctrl+C, Ctrl+V, Ctrl+X
      ;

    '0'..'9':
      if SelStart = 0 then
      begin
        if (inherited Text <> '') and (inherited Text[1] in ['-', '+']) then
          Key := #0;
      end;

    '-', '+':
      if SelStart > 0 then
        Key := #0
      else if (inherited Text <> '') and (inherited Text[1] in ['-', '+']) then
        Key := #0;

    #8:
      ;

  else
    Key := #0;
  end;

  if (Key in ['0'..'9', '-', '+']) and (not ValidAfter(toAdd, Key)) then
    Key := #0;

  inherited;
end;

procedure TlvkCustomIntegerEdit.SetAsFloat(const Value: Extended);
begin
  SetAsInteger(Trunc(Value));
end;

procedure TlvkCustomIntegerEdit.SetAsInteger(const Value: Int32);
begin
  if (Value = FBlankValue) and FUseBlank then
    inherited Text := ''
  else
    inherited Text := IntToStr(Value);
end;

procedure TlvkCustomIntegerEdit.SetBlankValue(const Value: Int32);
begin
  if FBlankValue <> Value then
  begin
    FBlankValue := Value;
    AdjustBlank;
  end;
end;

procedure TlvkCustomIntegerEdit.SetMaxValue(const Value: Int32);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    AdjustMinMax;
  end;
end;

procedure TlvkCustomIntegerEdit.SetMinValue(const Value: Int32);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    AdjustMinMax;
  end;
end;

procedure TlvkCustomIntegerEdit.SetUseBlank(const Value: Boolean);
begin
  if FUseBlank <> Value then
  begin
    FUseBlank := Value;
    AdjustBlank;
  end;
end;

procedure TlvkCustomIntegerEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_DELETE) and (Message.KeyData and $1000000 <> 0) then
  begin
    if ValidAfter(toDelete) then
      inherited
    else
      Message.Result := 0;
  end else
    inherited;
end;

procedure TlvkCustomIntegerEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  AdjustBlank;
end;

procedure TlvkCustomIntegerEdit.WMPaste(var Message: TMessage);
begin
  if Clipboard.HasFormat(CF_TEXT) and ValidAfter(toAdd, FirstClipboardLine) then
    inherited;
end;

{ TlvkCustomFloatEdit }

procedure TlvkCustomFloatEdit.AdjustBlank;
begin
  if FUseBlank then
  begin
    if Value = FBlankValue then
      inherited Text := '';
  end else begin
    if inherited Text = '' then
      inherited Text := FloatToStr(FBlankValue);
  end;
end;

procedure TlvkCustomFloatEdit.AdjustMinMax;
begin
  if inherited Text <> '' then
  begin
    if Value < FMinValue then
      Value := FMinValue
    else if Value > FMaxValue then
      Value := FMaxValue;
  end;
end;

constructor TlvkCustomFloatEdit.Create(AOwner: TComponent);
begin
  inherited;

  Alignment := taRightJustify;
  FMinValue := -1.1e+4932;
  FMaxValue := 1.1e+4932;
  FDecimalPlaces := -1;

  SetAsFloat(0.0);
end;

function TlvkCustomFloatEdit.CurrentDecimalSeparator: Char;
begin
  if FDecimalSeparator = #0 then
    Result := SysUtils.DecimalSeparator
  else
    result := FDecimalSeparator;
end;

function TlvkCustomFloatEdit.GetAsFloat: Extended;
var
  s     : string;
  Index : Integer;
begin
  if inherited Text = '' then
    Result := FBlankValue
  else begin
    s := inherited Text;
    if CurrentDecimalSeparator <> SysUtils.DecimalSeparator then
      for Index := 1 to Length(s) do
        if s[Index] = CurrentDecimalSeparator then
        begin
          s[Index] := SysUtils.DecimalSeparator;
          Break;
        end;
    if (s = CurrentDecimalSeparator) or (s = '+' + CurrentDecimalSeparator) or
      (s = '-' + CurrentDecimalSeparator) or (s = '+') or (s = '-') then
    begin
      Result := FBlankValue;
    end else
      Result := StrToFloat(s);
  end;
end;

function TlvkCustomFloatEdit.GetAsInteger: Int32;
begin
  Result := Trunc(GetAsFloat);
end;

function TlvkCustomFloatEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TlvkCustomFloatEdit.IsValid(const Text: string): Boolean;
var
  Index           : Integer;
  GotDigits       : Boolean;
  GotDecimalPoint : Boolean;
  Value           : Extended;
begin
  Result := True;
  GotDigits := False;
  GotDecimalPoint := False;

  if Text <> '' then
  begin
    Index := 1;

    if (Index <= Length(Text)) and (Text[Index] in ['-', '+']) then
      Inc(Index);

    while Index <= Length(Text) do
    begin
      if Text[Index] = CurrentDecimalSeparator then
      begin
        if GotDecimalPoint then
        begin
          Result := False;
          Break;
        end;

        GotDecimalPoint := True;
        Inc(Index);
      end else if Text[Index] in ['0'..'9'] then
      begin
        GotDigits := True;
        Inc(Index);
      end else begin
        Result := False;
        Break;
      end;
    end;
  end;

  if Result and GotDigits then
  begin
    try
      Value := StrToFloat(Text);
      if (Value < FMinValue) or (Value > FMaxValue) then
        Result := False;
    except
      Result := False;
    end;
  end;
end;

procedure TlvkCustomFloatEdit.KeyPress(var Key: Char);
begin
  if Key = CurrentDecimalSeparator then
  begin
    if Pos(CurrentDecimalSeparator, inherited Text) > 0 then
      Key := #0
    else if (SelStart = 0) and (inherited Text <> '') and (inherited Text[1] in ['-', '+']) then
      Key := #0;
  end else begin
    case Key of
      #3, #23, #24: // Ctrl+C, Ctrl+V, Ctrl+X
        ;

      '0'..'9':
        if SelStart = 0 then
        begin
          if (inherited Text <> '') and (inherited Text[1] in ['-', '+']) then
            Key := #0;
        end;

      '-', '+':
        if SelStart > 0 then
          Key := #0
        else if (inherited Text <> '') and (inherited Text[1] in ['-', '+']) then
          Key := #0;

      #8:
        ;

    else
      Key := #0;
    end;
  end;

  if (Key in ['0'..'9', '-', '+', '.']) and (not ValidAfter(toAdd, Key)) then
    Key := #0;

  inherited;
end;

procedure TlvkCustomFloatEdit.Reformat;
begin
  SetAsFloat(GetAsFloat);
end;

procedure TlvkCustomFloatEdit.SetAsFloat(const Value: Extended);
var
  RoundedValue  : Extended;
  Scale         : Extended;
begin
  RoundedValue := Value;

  if FDecimalPlaces > -1 then
  begin
    Scale := Power(10, FDecimalPlaces);
    case FRoundMode of
      rmTruncate:
        RoundedValue := Int(Value * Scale) / Scale;

      rmRoundNearest:
        RoundedValue := Round(Value * Scale) / Scale;
    end;
  end;

  if (Abs(RoundedValue - FBlankValue) < 1e-7) and FUseBlank then
    inherited Text := ''
  else if FDecimalPlaces > -1 then
    inherited Text := Format('%.*f', [FDecimalPlaces, RoundedValue])
  else
    inherited Text := FloatToStr(RoundedValue);
end;

procedure TlvkCustomFloatEdit.SetAsInteger(const Value: Int32);
begin
  SetAsFloat(Value);
end;

procedure TlvkCustomFloatEdit.SetBlankValue(const Value: Extended);
begin
  if FBlankValue <> Value then
  begin
    FBlankValue := Value;
    AdjustBlank;
  end;
end;

procedure TlvkCustomFloatEdit.SetDecimalPlaces(const Value: ShortInt);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    SetAsFloat(GetAsFloat);
  end;
end;

procedure TlvkCustomFloatEdit.SetDecimalSeparator(const Value: Char);
var
  CurrentValue  : Extended;
begin
  if FDecimalSeparator <> Value then
  begin
    CurrentValue := GetAsFloat;
    FDecimalSeparator := Value;
    SetAsFloat(CurrentValue);
  end;
end;

procedure TlvkCustomFloatEdit.SetMaxValue(const Value: Extended);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    AdjustMinMax;
  end;
end;

procedure TlvkCustomFloatEdit.SetMinValue(const Value: Extended);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    AdjustMinMax;
  end;
end;

procedure TlvkCustomFloatEdit.SetUseBlank(const Value: Boolean);
begin
  if FUseBlank <> Value then
  begin
    FUseBlank := Value;
    AdjustBlank;
  end;
end;

procedure TlvkCustomFloatEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_DELETE) and (Message.KeyData and $1000000 <> 0) then
  begin
    if ValidAfter(toDelete) then
      inherited
    else
      Message.Result := 0;
  end else
    inherited;
end;

procedure TlvkCustomFloatEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  AdjustBlank;
  Reformat;
end;

procedure TlvkCustomFloatEdit.WMPaste(var Message: TMessage);
begin
  if Clipboard.HasFormat(CF_TEXT) and ValidAfter(toAdd, FirstClipboardLine) then
    inherited;
end;

end.
