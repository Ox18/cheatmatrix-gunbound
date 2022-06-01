{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component that will save the state of a form, including
    its size, position and current maximize/minimize state.
}
unit lvkFormState;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFormState.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkState;

type
  { Description:
      These options are used by the TlvkFormState component to specify what
      of the form state to save/restore. The values are:

        * fsoPosition - The current left and top property values (handles
          maximized forms correctly)
        * fsoSize - The current width and height property values (handles
          maximized forms correctly)
        * fsoState - The current minimize/maximize and position property values.
        * fsoActiveControl - Which control is active when the form is closed.
  }
  TlvkFormStateOption = (fsoPosition, fsoSize, fsoState, fsoActiveControl);
  // <COMBINE TlvkFormStateOption>
  TlvkFormStateOptions = set of TlvkFormStateOption;

  { Description:
      This component implements a state saving component for a form. Just
      drop this component on a form, alter the options property if needed,
      link it to a storage and voila, instant saving of state. See the
      options property and its associated datatypes for more information about
      what this component saves.
    See also:
      TlvkFormState.Options, TlvkFormStateOptions
  }
  TlvkFormState = class(TlvkCustomState)
  private
    FOptions  : TlvkFormStateOptions;

  protected
    procedure SaveState; override;
    procedure RestoreState; override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomState.SectionName>
    property SectionName;
    // <ALIAS TlvkCustomState.Storage>
    property Storage;
    // <ALIAS TlvkCustomState.Active>
    property Active;
    // <ALIAS TlvkCustomState.OnBeforeSave>
    property OnBeforeSave;
    // <ALIAS TlvkCustomState.OnAfterSave>
    property OnAfterSave;
    // <ALIAS TlvkCustomState.OnBeforeRestore>
    property OnBeforeRestore;
    // <ALIAS TlvkCustomState.OnAfterRestore>
    property OnAfterRestore;

    { Description:
        This property specifies what kind of state information to save and
        restore for the form. See the TlvkFormStateOptions data type for
        more information about the available values.
      See also:
        TlvkFormStateOptions
    }
    property Options: TlvkFormStateOptions read FOptions write FOptions;
  end;

implementation

uses
  Forms, Controls, Windows;

{ TlvkFormState }

constructor TlvkFormState.Create(AOwner: TComponent);
begin
  inherited;

  Assert(Assigned(Form));

  SectionName := Form.Name + '.Form';
  FOptions := [Low(TlvkFormStateOption)..High(TlvkFormStateOption)];
end;

procedure TlvkFormState.RestoreState;
var
  WindowState : TWindowState;
  Position    : TPosition;
  x, y        : Integer;
  w, h        : Integer;
  s           : string;
  Placement   : TWindowPlacement;
begin
  ZeroMemory(@Placement, SizeOf(Placement));
  Placement.length := SizeOf(Placement);
  GetWindowPlacement(Form.Handle, @Placement);

  if fsoPosition in FOptions then
  begin
    x := ReadInteger('Left', Placement.rcNormalPosition.Left);
    y := ReadInteger('Top', Placement.rcNormalPosition.Top);
  end else begin
    x := Placement.rcNormalPosition.Left;
    y := Placement.rcNormalPosition.Top;
  end;

  if fsoSize in FOptions then
  begin
    w := ReadInteger('Width', Placement.rcNormalPosition.Right - Placement.rcNormalPosition.Left);
    h := ReadInteger('Height', Placement.rcNormalPosition.Bottom - Placement.rcNormalPosition.Top);
  end else begin
    w := Placement.rcNormalPosition.Right - Placement.rcNormalPosition.Left;
    h := Placement.rcNormalPosition.Bottom - Placement.rcNormalPosition.Top;
  end;

  Placement.rcNormalPosition.Left := x;
  Placement.rcNormalPosition.Top := y;
  Placement.rcNormalPosition.Right := x + w;
  Placement.rcNormalPosition.Bottom := y + h;
  Form.Left := x;
  Form.Top := y;

  if fsoState in FOptions then
  begin
    WindowState := TWindowState(ReadEnumerated('WindowState',
      TypeInfo(TWindowState), Ord(Form.WindowState)));
    Position := TPosition(ReadEnumerated('Position',
      TypeInfo(TPosition), Ord(TForm(Form).Position)));
  end else begin
    WindowState := Form.WindowState;
    Position := TForm(Form).Position;
  end;

  case WindowState of
    wsNormal:
      begin
        Form.WindowState := wsNormal;
        TForm(Form).Position := Position;
        Form.SetBounds(x, y, w, h);
      end;

    wsMaximized:
      begin
        TForm(Form).Position := Position;
        Form.SetBounds(x, y, w, h);
        Form.WindowState := WindowState;
      end;

    wsMinimized:
      begin
        TForm(Form).Position := Position;
        Form.SetBounds(x, y, w, h);
        Form.WindowState := WindowState;
      end;
  end;

  if fsoActiveControl in FOptions then
  begin
    s := ReadString('ActiveControl', '');

    if s = '' then
      Form.ActiveControl := nil
    else
      Form.ActiveControl := Form.FindChildControl(s) as TWinControl;
  end;
end;

procedure TlvkFormState.SaveState;
var
  Placement   : TWindowPlacement;
begin
  ZeroMemory(@Placement, SizeOf(Placement));
  Placement.length := SizeOf(Placement);
  GetWindowPlacement(Form.Handle, @Placement);

  if fsoPosition in FOptions then
  begin
    WriteInteger('Left', Placement.rcNormalPosition.Left);
    WriteInteger('Top', Placement.rcNormalPosition.Top);
  end else
    DeleteValues(['Left', 'Top']);

  if fsoSize in FOptions then
  begin
    WriteInteger('Width', Placement.rcNormalPosition.Right - Placement.rcNormalPosition.Left);
    WriteInteger('Height', Placement.rcNormalPosition.Bottom - Placement.rcNormalPosition.Top);
  end else
    DeleteValues(['Width', 'Height']);

  if fsoState in FOptions then
  begin
    WriteEnumerated('WindowState', TypeInfo(TWindowState), Ord(Form.WindowState));
    WriteEnumerated('Position', TypeInfo(TPosition), Ord(TForm(Form).Position));
  end else
    DeleteValues(['WindowState', 'Position']);

  if fsoActiveControl in FOptions then
  begin
    if Assigned(Form.ActiveControl) then
      WriteString('ActiveControl', Form.ActiveControl.Name)
    else
      WriteString('ActiveControl', '');
  end else
    DeleteValues(['ActiveControl']);
end;

end.
