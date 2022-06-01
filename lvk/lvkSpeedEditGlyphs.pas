{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code and resources for glyphs for various buttons in
    the component collection.
}
unit lvkSpeedEditGlyphs;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSpeedEditGlyphs.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkEdits, Graphics;

type
  { Description:
      This type contains the various button kinds that GetGlyphFor handles.
  }
  TSpeedEditButtonKind = (
    // This is the "..." kind of button.
    sebkEllipsis,
    // This is a button with a single triangle, pointing downwards.
    sebkComboBox);

{ Description:
    This function returns a new bitmap class with the correct glyph image
    loaded.

    Note: The caller is responsible for destroying the bitmap class after use.
  Parameters:
    ButtonKind  - What kind of button to return a bitmap for.
}
function GetGlyphFor(const ButtonKind: TSpeedEditButtonKind): TBitmap;

implementation

uses
  SysUtils;
  
{ TGlyphs }

type
  TGlyphs = class
  private
    FGlyphs : array[TSpeedEditButtonKind] of TBitmap;

  public
    function GetGlyphFor(const ButtonKind: TSpeedEditButtonKind): TBitmap;
  end;

var
  FGlyphs : TGlyphs = nil;

function TGlyphs.GetGlyphFor(const ButtonKind: TSpeedEditButtonKind): TBitmap;
const
  GLYPH_NAMES : array[TSpeedEditButtonKind] of string = (
    'LVK_ELLIPSIS',           // sebkEllipsis
    'LVK_COMBOBOX_LOOKALIKE'  // sebkComboBox
  );

begin
  if FGlyphs[ButtonKind] = nil then
  begin
    FGlyphs[ButtonKind] := TBitmap.Create;
    FGlyphs[ButtonKind].LoadFromResourceName(HInstance, GLYPH_NAMES[ButtonKind]);
  end;

  Result := FGlyphs[ButtonKind];

end;

function GetGlyphFor(const ButtonKind: TSpeedEditButtonKind): TBitmap;
begin
  if not Assigned(FGlyphs) then
    FGlyphs := TGlyphs.Create;

  Result := FGlyphs.GetGlyphFor(ButtonKind);
end;

initialization
  FGlyphs := TGlyphs.Create;
finalization
  FreeAndNil(FGlyphs);
end.
