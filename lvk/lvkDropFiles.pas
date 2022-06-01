{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkDropFiles component for dropping files on a form
    from explorer.
}
unit lvkDropFiles;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDropFiles.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, Controls, Forms, Messages,
  lvkSubclass;

type
  // <COMBINE TlvkCustomDropFiles.OnDrop>
  TDropFilesEvent = procedure(Sender: TObject;
    const Files: TStrings) of object;

  TFormSubClass = class(TlvkSubClass)
  protected
    procedure BeforeTargetWindowProc(var Message: TMessage;
      var PassToTarget: Boolean); override;
    procedure AfterTargetWindowProc(var Message: TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This is the bases for the TlvkDropFiles component.
  }
  TlvkCustomDropFiles = class(TlvkSubClass)
  private
    FOnDrop       : TDropFilesEvent;
    FEnabled      : Boolean;
    FDropZone     : TWinControl;
    FSortList     : Boolean;
    FFormSubClass : TFormSubClass;

    procedure DoDrop(const Files: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure SetDropZone(const Value: TWinControl);
    function UseHandle: THandle;

    procedure SetSubClassTarget;
    procedure RemoveSubClassTarget;

    procedure DoEnable;
    procedure DoDisable;

  protected
    procedure Loaded; override;

    { Description:
        This event handler is called when files are dropped on the dropzone
        from explorer.
    }
    property OnDrop: TDropFilesEvent read FOnDrop write FOnDrop;

    { Description:
        Set this property to True to let the dropzone accept files dropped
        on it.
    }
    property Enabled: Boolean read FEnabled write SetEnabled default False;

    { Description:
        This property defines which control on the form that will act as a
        drop zone. If no control is selected, the whole form will act as one.
    }
    property DropZone: TWinControl read FDropZone write SetDropZone;

    { Description:
        Set this property to True to have the list of files be sorted before
        handed to the event handler.
    }
    property SortList: Boolean read FSortList write FSortList default False;

    procedure HandleDropFiles(const Drop: Integer);

    procedure BeforeTargetWindowProc(var Message: TMessage;
      var PassToTarget: Boolean); override;
    procedure AfterTargetWindowProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Enable;
    procedure Disable;
  end;

  { Description:
      This component allows you to drop files from explorer on the form
      and let the component fire an event for the files.
    Parameters:
      -
    See also:
      -
  }
  TlvkDropFiles = class(TlvkCustomDropFiles)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomDropFiles.OnDrop>
    property OnDrop;
    // <ALIAS TlvkCustomDropFiles.Enabled>
    property Enabled;
    // <ALIAS TlvkCustomDropFiles.DropZone>
    property DropZone;
    // <ALIAS TlvkCustomDropFiles.SortList>
    property SortList;
  end;

  EDropFiles = class(Exception);

implementation

uses
  ShellApi;

{ TlvkCustomDropFiles }

procedure TlvkCustomDropFiles.AfterTargetWindowProc(var Message: TMessage);
begin
  if (Message.Msg = CM_RECREATEWND) and TlvkCustomDropFiles(Owner).FEnabled and (not (csDesigning in ComponentState))then
    DragAcceptFiles(TlvkCustomDropFiles(Owner).UseHandle, True);
end;

procedure TlvkCustomDropFiles.BeforeTargetWindowProc(var Message: TMessage;
  var PassToTarget: Boolean);
begin
  if Message.Msg = WM_DROPFILES then
    HandleDropFiles(Message.WParam);

  if (Message.Msg = CM_RECREATEWND) and TlvkCustomDropFiles(Owner).FEnabled and (not (csDesigning in ComponentState))then
    DragAcceptFiles(TlvkCustomDropFiles(Owner).UseHandle, False);
end;

constructor TlvkCustomDropFiles.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    FFormSubClass := TFormSubClass.Create(Self);

  if not (AOwner is TCustomForm) then
    raise EDropFiles.Create('This component must be dropped on a form');

  SubClassTarget := AOwner as TWinControl;
end;

destructor TlvkCustomDropFiles.Destroy;
begin
  if not (csDestroying in ComponentState) then
    Disable;

  if not (csDesigning in ComponentState) then
    FFormSubClass.Free;

  inherited;
end;

procedure TlvkCustomDropFiles.Disable;
begin
  Enabled := False;
end;

procedure TlvkCustomDropFiles.DoDisable;
begin
  if not (csDesigning in ComponentState) then
    DragAcceptFiles(UseHandle, False);
end;

procedure TlvkCustomDropFiles.DoDrop(const Files: TStrings);
begin
  if Assigned(FOnDrop) then
    FOnDrop(Self, Files);
end;

procedure TlvkCustomDropFiles.DoEnable;
begin
  if not (csDesigning in ComponentState) then
    DragAcceptFiles(UseHandle, True);
end;

procedure TlvkCustomDropFiles.Enable;
begin
  Enabled := True;
end;

procedure TlvkCustomDropFiles.HandleDropFiles(const Drop: Integer);
var
  Files     : TStrings;
  FileName  : array[0..MAX_PATH] of Char;
  Index     : Integer;
  Count     : Integer;
begin
  try
    Files := TStringList.Create;
    try
      Count := DragQueryFile(Drop, Cardinal(-1), nil, 0);
      for Index := 0 to Count-1 do
      begin
        DragQueryFile(Drop, Index, FileName, SizeOf(FileName));

        if FSortList then
          TStringList(Files).Sort;
        Files.Add(FileName);
      end;

      DoDrop(Files);
    finally
      Files.Free;
    end;
  finally
    DragFinish(Drop);
  end;
end;

procedure TlvkCustomDropFiles.Loaded;
begin
  inherited;

  SetSubClassTarget;
  if FEnabled then
    DoEnable;
end;

procedure TlvkCustomDropFiles.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FDropZone then
      DropZone := nil;
  end;
end;

procedure TlvkCustomDropFiles.RemoveSubClassTarget;
begin
  SubClassed := False;
  SubClassTarget := Owner as TWinControl;
end;

procedure TlvkCustomDropFiles.SetDropZone(const Value: TWinControl);
begin
  if Value <> FDropZone then
  begin
    if not (csDesigning in ComponentState) then
      RemoveSubClassTarget;

    FDropZone := Value;

    if not (csDesigning in ComponentState) then
      SetSubClassTarget;
  end;
end;

procedure TlvkCustomDropFiles.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    if (not (csDesigning in ComponentState)) and
      (not (csLoading in ComponentState)) then
    begin
      if Value then
        DoEnable
      else
        DoDisable;
    end;

    FEnabled := Value;
  end;
end;

procedure TlvkCustomDropFiles.SetSubClassTarget;
begin
  if Assigned(FDropZone) then
    SubClassTarget := FDropZone
  else
    SubClassTarget := Owner as TWinControl;
  SubClassed := True;
end;

function TlvkCustomDropFiles.UseHandle: THandle;
begin
  if Assigned(FDropZone) then
  begin
    FDropZone.HandleNeeded;
    Result := FDropZone.Handle;
  end else begin
    (Owner as TWinControl).HandleNeeded;
    Result := (Owner as TWinControl).Handle;
  end;
end;

{ TFormSubClass }

procedure TFormSubClass.AfterTargetWindowProc(var Message: TMessage);
begin
  if (Message.Msg = CM_RECREATEWND) and TlvkCustomDropFiles(Owner).FEnabled and (not (csDesigning in ComponentState))then
    DragAcceptFiles(TlvkCustomDropFiles(Owner).UseHandle, True);
end;

procedure TFormSubClass.BeforeTargetWindowProc(var Message: TMessage;
  var PassToTarget: Boolean);
begin
  if (Message.Msg = CM_RECREATEWND) and TlvkCustomDropFiles(Owner).FEnabled and (not (csDesigning in ComponentState)) then
    DragAcceptFiles(TlvkCustomDropFiles(Owner).UseHandle, False);
end;

constructor TFormSubClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SubClassTarget := AOwner.Owner as TWinControl;
  SubClassed := True;
end;

end.
