{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkPopupMainMenu component, which allows you to
    use pieces of the main menu of a form as a popup menu for components.
}
unit lvkPopupMainMenu;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkPopupMainMenu.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Menus;

type
  { Description:
      This component allows you to use pieces of the main menu of the form as
      a popup menu for other components. Basically you link the component
      to a TMainMenu class, and give it a valid RootCaption, and then just link
      components up to this popup menu component as usual.
  }
  TlvkPopupMainMenu = class(TPopupMenu)
  private
    FMainMenu : TMainMenu;
    FRootCaption : string;
    FChanged  : Boolean;

    procedure SetMainMenu(const Value: TMainMenu);
    procedure SetRootCaption(const Value: string);
    procedure SetChanged;

    procedure CopyMainMenu;
    procedure CopySubMenu;

    procedure CopyItemsFrom(const MenuItem: TMenuItem);
    procedure AddChildItem(const Parent, Item: TMenuItem);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    procedure Popup(X: Integer; Y: Integer); override;

    procedure Update;
    property Changed: Boolean read FChanged;

  published
    { Description:
        This property must contain a reference to a valid instance of a
        TMainMenu component.

        If you set this property to nil, the popup menu will be blank.
      See also:
        RootCaption
    }
    property MainMenu: TMainMenu read FMainMenu write SetMainMenu;

    { Description:
        This property contains the full root caption of the items to use
        in the popup menu. Use the bar character | to separate captions. You
        must specify the name of the menuitem that you want the sub-menu-items
        of in the popup menu.

        If you want the whole main menu, leave this property blank.

        If you want the contents of the file menu, set this property to the
        value File.

        If you want the contents of the sub-menu Recent Files on the File
        menu as the popup menu, set this property to File|Recent Files.

        If you specify an invalid root name, an exception will be raised.
    }
    property RootCaption: string read FRootCaption write SetRootCaption;
  end;

  ElvkPopupMainMenu = class(Exception);

{ Description:
    This function copies the property values from one menu item to another.
  Parameters:
    Source  - The menu item instance to copy.
    Dest    - The menu item that will get the new property values.
}
procedure CopyMenuItem(const Source, Dest: TMenuItem);

implementation

procedure CopyMenuItem(const Source, Dest: TMenuItem);
begin
  // Copy non-event properties
  {$IFDEF DELPHI6UP}
  Dest.AutoCheck := Source.AutoCheck;
  {$ENDIF}
  Dest.AutoHotkeys := Source.AutoHotkeys;
  Dest.AutoLineReduction := Source.AutoLineReduction;
  Dest.Bitmap := Source.Bitmap;
  Dest.Break := Source.Break;
  Dest.Caption := Source.Caption;
  Dest.Checked := Source.Checked;
  Dest.Default := Source.Default;
  Dest.Enabled := Source.Enabled;
  Dest.GroupIndex := Source.GroupIndex;
  Dest.HelpContext := Source.HelpContext;
  Dest.Hint := Source.Hint;
  Dest.ImageIndex := Source.ImageIndex;
  Dest.RadioItem := Source.RadioItem;
  Dest.ShortCut := Source.ShortCut;
  Dest.SubMenuImages := Source.SubMenuImages;
  Dest.Tag := Source.Tag;
  Dest.Visible := Source.Visible;

  // Copy event properties
  Dest.OnAdvancedDrawItem := Source.OnAdvancedDrawItem;
  Dest.OnClick := Source.OnClick;
  Dest.OnDrawItem := Source.OnDrawItem;
  Dest.OnMeasureItem := Source.OnMeasureItem;

  // Copy action last, to make sure it overrides all the necessary properties
  Dest.Action := Source.Action;
end;

{ TlvkPopupMainMenu }

procedure TlvkPopupMainMenu.AddChildItem(const Parent, Item: TMenuItem);
var
  NewItem : TMenuItem;
  Index   : Integer;
begin
  NewItem := TMenuItem.Create(Self);
  CopyMenuItem(Item, NewItem);

  if Assigned(Parent) then
    Parent.Add(NewItem)
  else
    Items.Add(NewItem);

  for Index := 0 to Item.Count-1 do
    AddChildItem(NewItem, Item.Items[Index]);
end;

procedure TlvkPopupMainMenu.CopyItemsFrom(const MenuItem: TMenuItem);
var
  Index : Integer;
begin
  for Index := 0 to MenuItem.Count-1 do
    AddChildItem(nil, MenuItem.Items[Index]);
end;

procedure TlvkPopupMainMenu.CopyMainMenu;
var
  Index : Integer;
begin
  for Index := 0 to FMainMenu.Items.Count-1 do
    AddChildItem(nil, FMainMenu.Items[Index]);
end;

procedure TlvkPopupMainMenu.CopySubMenu;
var
  Names     : TStrings;
  Index     : Integer;
  MenuItem  : TMenuItem;

  function LocateRoot(const Caption: string): TMenuItem;
  var
    Index : Integer;
  begin
    Result := nil;
    for Index := 0 to FMainMenu.Items.Count-1 do
      if CompareText(StringReplace(FMainMenu.Items[Index].Caption, '&', '', []), Caption) = 0 then
      begin
        Result := FMainMenu.Items[Index];
        Break;
      end;
  end;

  function LocateChild(const Parent: TMenuItem; const Caption: string): TMenuItem;
  var
    Index : Integer;
  begin
    Result := nil;
    for Index := 0 to Parent.Count-1 do
      if CompareText(StringReplace(Parent.Items[Index].Caption, '&', '', []), Caption) = 0 then
      begin
        Result := Parent.Items[Index];
        Break;
      end;
  end;

begin
  Names := TStringList.Create;
  try
    Names.Text := StringReplace(FRootCaption, '|', #13#10, [rfReplaceAll]);
    Index := 0;
    while Index < Names.Count do
    begin
      Names[Index] := Trim(Names[Index]);
      if Names[Index] = '' then
        Names.Delete(Index)
      else
        Inc(Index);
    end;

    if Names.Count = 0 then
      CopyMainMenu
    else begin
      MenuItem := LocateRoot(Names[0]);
      if not Assigned(MenuItem) then
        raise ElvkPopupMainMenu.Create('No root menu item with the caption ' + Names[0]);

      for Index := 1 to Names.Count-1 do
      begin
        MenuItem := LocateChild(MenuItem, Names[Index]);
        if not Assigned(MenuItem) then
          raise ElvkPopupMainMenu.Create('No submenu item with the caption ' + Names[Index]);
      end;
      CopyItemsFrom(MenuItem);
    end;
  finally
    Names.Free;
  end;
end;

procedure TlvkPopupMainMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMainMenu then
    begin
      FMainMenu := nil;
      SetChanged;
    end;
end;

procedure TlvkPopupMainMenu.Popup(X, Y: Integer);
begin
  if Changed then
    Update;

  inherited;
end;

procedure TlvkPopupMainMenu.SetChanged;
begin
  FChanged := True;
  Items.Clear;
end;

procedure TlvkPopupMainMenu.SetMainMenu(const Value: TMainMenu);
begin
  if Value <> FMainMenu then
  begin
    FMainMenu := Value;
    SetChanged;
  end;
end;

procedure TlvkPopupMainMenu.SetRootCaption(const Value: string);
begin
  if Value <> FRootCaption then
  begin
    FRootCaption := Value;
    SetChanged;
  end;
end;

procedure TlvkPopupMainMenu.Update;
begin
  Items.Clear;

  if Assigned(FMainMenu) then
  begin
    if FRootCaption = '' then
      CopyMainMenu
    else
      CopySubMenu;
  end;

  FChanged := False;
end;

end.
