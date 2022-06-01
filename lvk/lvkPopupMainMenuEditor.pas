{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the property editor for the popup main menu component.
}
unit lvkPopupMainMenuEditor;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/lvkPopupMainMenuEditor.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, lvkPopupMainMenu;

type
  TfmEditPopupMainMenu = class(TForm)
    Label1: TLabel;
    eRootCaption: TEdit;
    SelectMenu: TMainMenu;
    Label2: TLabel;
  private
    procedure SetMainMenu(const MainMenu: TMainMenu);
    procedure CopyItems(const Parent, MenuItem: TMenuItem);

    procedure MenuItemClick(Sender: TObject);

  public
    { Public declarations }
    property MainMenu: TMainMenu write SetMainMenu;
  end;

var
  fmEditPopupMainMenu: TfmEditPopupMainMenu;

implementation

{$R *.DFM}

{ TfmEditPopupMainMenu }

procedure TfmEditPopupMainMenu.CopyItems(const Parent, MenuItem: TMenuItem);
var
  NewItem : TMenuItem;
  Index   : Integer;
begin
  NewItem := TMenuItem.Create(SelectMenu);
  CopyMenuItem(MenuItem, NewItem);
  NewItem.OnClick := MenuItemClick;

  if Assigned(Parent) then
    Parent.Add(NewItem)
  else
    SelectMenu.Items.Add(NewItem);

  for Index := 0 to MenuItem.Count-1 do
    CopyItems(NewItem, MenuItem.Items[Index]);
end;

procedure TfmEditPopupMainMenu.MenuItemClick(Sender: TObject);
var
  NewRoot : string;

  function CaptionOf(const MenuItem: TMenuItem): string;
  begin
    Result := StringReplace(MenuItem.Caption, '&', '', []);

    if Assigned(MenuItem.Parent) then
      Result := CaptionOf(MenuItem.Parent) + '|' + Result;
  end;

begin
  NewRoot := CaptionOf((Sender as TMenuItem).Parent);
  if Copy(NewRoot, 1, 1) = '|' then
    Delete(NewRoot, 1, 1);
  eRootCaption.Text := NewRoot;
end;

procedure TfmEditPopupMainMenu.SetMainMenu(const MainMenu: TMainMenu);
var
  Index : Integer;
begin
  Assert(Assigned(MainMenu));

  for Index := 0 to MainMenu.Items.Count-1 do
    CopyItems(nil, MainMenu.Items[Index]);
end;

end.
