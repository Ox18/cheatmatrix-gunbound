{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkLabel component.
}
unit lvkLabel;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 9 $
// $Archive: /Components/LVK/source/lvkLabel.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, AppEvnts, StdCtrls, lvkVersion;

type
  TlvkCustomLabel = class(TCustomLabel)
  private
    FAppEvents  : TApplicationEvents;
    FEnabled    : Boolean;
    FVisible    : Boolean;

    procedure AppIdle(Sender: TObject; var Done: Boolean);
    function GetShowAccelChar: Boolean;
    procedure SetShowAccelChar(const Value: Boolean);

  protected
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Visible: Boolean read FVisible write FVisible default True;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    property ShowAccelChar: Boolean read GetShowAccelChar
      write SetShowAccelChar default True;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This TCustomLabel descendant component functions exactly like a TLabel,
      with one exception. If you set the FocusControl property to refer to
      a control, and this control becomes disabled, this label will also
      disable itself.
  }
  TlvkLabel = class(TlvkCustomLabel)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    property Enabled;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI6UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TlvkCustomLabel }

procedure TlvkCustomLabel.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FocusControl) then
  begin
    inherited Enabled := FEnabled and FocusControl.Enabled;
    inherited Visible := FVisible and FocusControl.Visible;
  end else begin
    inherited Enabled := FEnabled;
    inherited Visible := FVisible;
  end;
end;

constructor TlvkCustomLabel.Create(AOwner: TComponent);
begin
  inherited;

  FAppEvents := TApplicationEvents.Create(Self);
  FAppEvents.OnIdle := AppIdle;
  FEnabled := True;
  FVisible := True;
end;

destructor TlvkCustomLabel.Destroy;
begin
  FAppEvents.Free;

  inherited;
end;

function TlvkCustomLabel.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkCustomLabel.GetShowAccelChar: Boolean;
begin
  Result := inherited ShowAccelChar;
end;

procedure TlvkCustomLabel.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomLabel.SetShowAccelChar(const Value: Boolean);
begin
  if Value <> (inherited ShowAccelChar) then
  begin
    inherited ShowAccelChar := Value;
    if AutoSize then
      AdjustBounds;
  end;
end;

end.
