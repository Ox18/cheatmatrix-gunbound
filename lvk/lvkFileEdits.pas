{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkDirectoryEdit and TlvkFileEdit controls.
}
unit lvkFileEdits;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFileEdits.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, lvkEdits, ShlObj, ActiveX, FileCtrl, Dialogs,
  lvkSpeedEdit;

type
  { Description:
      This property allows the user to select a directory by clicking on the
      "..." button to the right to the text.
  }
  TlvkDirectoryEdit = class(TlvkCustomSpeedEdit)
  private
    procedure ButtonClick(Sender: TObject);
    function GetDirectoryExists: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    property DirectoryExists: Boolean read GetDirectoryExists;

  published
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
    property Text;
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
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;

    // From TlvkCustomSpeedEdit
    // <ALIAS TlvkCustomSpeedEdit.Button>
    property Button;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      A simple descendant of TOpenDialog that implements the AssignTo
      method.

      Is used internally by TlvkFileEdit
    See also:
      TlvkFileEdit
  }
  TAssignableOpenDialog = class(TOpenDialog)
  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component allows the user to select a file by clicking on the
      "..." button next to the text. The name and path of the selected
      file will be shown in the text box.
  }
  TlvkFileEdit = class(TlvkCustomSpeedEdit)
  private
    FOpenDialog : TAssignableOpenDialog;

    procedure ButtonClick(Sender: TObject);
    function GetFileExists: Boolean;
    procedure SetOpenDialog(const Value: TAssignableOpenDialog);

  public
    constructor Create(AOwner: TComponent); override;
    property FileExists: Boolean read GetFileExists;

  published
    { Description:
        This property gives access to the open dialog used to select the file.
    }
    property OpenDialog: TAssignableOpenDialog read FOpenDialog write SetOpenDialog;

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
    property Text;
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
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;

    // From TlvkCustomSpeedEdit
    // <ALIAS TlvkCustomSpeedEdit.Button>
    property Button;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

resourcestring
  SDirectoryDialogCaption = 'Select directory';

implementation

uses
  Windows, Forms;

{ TlvkDirectoryEdit }

procedure TlvkDirectoryEdit.ButtonClick(Sender: TObject);
var
  Directory : string;
begin
  Directory := Text;
  if SelectDirectory(Directory, [], 0) then
  begin
    Text := Directory;
    SelectAll;
    SetFocus;
  end;
end;

constructor TlvkDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited;

  inherited OnButtonClick := ButtonClick;
end;

function TlvkDirectoryEdit.GetDirectoryExists: Boolean;
var
  s : string;
begin
  s := Text;
  if s <> '' then
  begin
    if Copy(s, Length(s), 1) = '\' then
      s := Copy(s, 1, Length(s)-1);
    Result := SysUtils.FileExists(s);
  end else
    Result := False;
end;

{ TlvkFileEdit }

procedure TlvkFileEdit.ButtonClick(Sender: TObject);
begin
  FOpenDialog.InitialDir := ExtractFilePath(Text);
  FOpenDialog.FileName := ExtractFileName(Text);

  if FOpenDialog.Execute then
  begin
    Text := FOpenDialog.FileName;
    SelectAll;
    SetFocus;
  end;
end;

constructor TlvkFileEdit.Create(AOwner: TComponent);
begin
  inherited;

  inherited OnButtonClick := ButtonClick;
  FOpenDialog := TAssignableOpenDialog.Create(Self);
  FOpenDialog.FreeNotification(Self);
end;

function TlvkFileEdit.GetFileExists: Boolean;
begin
  Result := SysUtils.FileExists(Text);
end;

procedure TlvkFileEdit.SetOpenDialog(const Value: TAssignableOpenDialog);
begin
  FOpenDialog.Assign(Value);
end;

{ TAssignableOpenDialog }

procedure TAssignableOpenDialog.AssignTo(Dest: TPersistent);
begin
  if Dest is TOpenDialog then
  begin
    (Dest as TOpenDialog).DefaultExt := DefaultExt;
    (Dest as TOpenDialog).FileName := FileName;
    (Dest as TOpenDialog).Filter := Filter;
    (Dest as TOpenDialog).FilterIndex := FilterIndex;
    (Dest as TOpenDialog).InitialDir := InitialDir;
    (Dest as TOpenDialog).Options := Options;
    {$IFDEF DELPHI6UP}
    (Dest as TOpenDialog).OptionsEx := OptionsEx;
    {$ENDIF}
    (Dest as TOpenDialog).Title := Title;
    (Dest as TOpenDialog).OnCanClose := OnCanClose;
    (Dest as TOpenDialog).OnFolderChange := OnFolderChange;
    (Dest as TOpenDialog).OnSelectionChange := OnSelectionChange;
    (Dest as TOpenDialog).OnTypeChange := OnTypeChange;
    (Dest as TOpenDialog).OnIncludeItem := OnIncludeItem;
  end else
    inherited;
end;

constructor TAssignableOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  Name := 'SubOpenDialog';
  {$IFDEF DELPHI6UP}
  SetSubComponent(True);
  {$ENDIF}
end;

end.
