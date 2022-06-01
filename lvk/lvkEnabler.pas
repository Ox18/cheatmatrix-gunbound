{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains components for enabling/disabling controls in a GUI.
}
unit lvkEnabler;

// $Author: Lasse V. Karlsen $
// $Revision: 16 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkEnabler.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Controls, SysUtils, Classes, StdCtrls, lvkThread, lvkComponents,
  AppEvnts, lvkUtilityFunctions;

type
  TEnable = (eSetState, eInvertState, eLeaveState);
  TVisibility = (vSetState, vInvertState, vLeaveState);

const
  DEFAULT_ENABLE        = eSetState;
  DEFAULT_VISIBILITY    = vLeaveState;

  DEFAULT_ENABLED       = True;
  DEFAULT_INVERT_STATE  = False;

type
  { Description:
      This class is used as items for the Controls collection property of the
      components that derive from TlvkCustomEnabler.
  }
  TControlItem = class(TCollectionItem)
  private
    FControl          : TControl;
    FVisibility       : TVisibility;
    FEnable           : TEnable;
    FOnBeforeShow     : TNotifyEvent;
    FOnAfterShow      : TNotifyEvent;
    FOnBeforeHide     : TNotifyEvent;
    FOnAfterHide      : TNotifyEvent;
    FOnBeforeEnable   : TNotifyEvent;
    FOnAfterEnable    : TNotifyEvent;
    FOnBeforeDisable  : TNotifyEvent;
    FOnAfterDisable   : TNotifyEvent;

    procedure SetControl(const Value: TControl);
    procedure SetEnable(const Value: TEnable);
    procedure SetVisibility(const Value: TVisibility);
    procedure SetState(const State: Boolean);

  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Collection: TCollection); override;

  published
    { Description:
        Which control to enable/disable.
    }
    property Control: TControl read FControl write SetControl;

    { Description:
        This property controls wether the Enabled property of the control
        is changed or not, and how it is changed.
    }
    property Enable: TEnable read FEnable write SetEnable
      default DEFAULT_ENABLE;

    { Description:
        This proeprty controls wether the Visible property of the control
        is changed or not, and how it is changed.
    }
    property Visibility: TVisibility read FVisibility write SetVisibility
      default DEFAULT_VISIBILITY;

    { Description:
        This event handler will be called right before the control is shown.
      See also:
        OnAfterShow, OnBeforeHide, OnAfterHide
    }
    property OnBeforeShow: TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    { Description:
        This event handler will be called right after the control is shown.
      See also:
        OnBeforeShow, OnBeforeHide, OnAfterHide
    }
    property OnAfterShow: TNotifyEvent read FOnAfterShow write FOnAfterShow;

    { Description:
        This event handler will be called right before the control is hidden.
      See also:
        OnAfterHide, OnBeforeShow, OnAfterShow
    }
    property OnBeforeHide: TNotifyEvent read FOnBeforeHide write FOnBeforeHide;
    { Description:
        This event handler will be called right after the control is hidden.
      See also:
        OnBeforeHide, OnBeforeShow, OnAfterShow
    }
    property OnAfterHide: TNotifyEvent read FOnAfterHide write FOnAfterHide;

    { Description:
        This event handler will be called right before the control is enabled.
      See also:
        OnAfterEnable, OnBeforeHide, OnAfterHide
    }
    property OnBeforeEnable: TNotifyEvent read FOnBeforeEnable
      write FOnBeforeEnable;
    { Description:
        This event handler will be called right after the control is enabled.
      See also:
        OnBeforeEnable, OnBeforeHide, OnAfterHide
    }
    property OnAfterEnable: TNotifyEvent read FOnAfterEnable
      write FOnAfterEnable;

    { Description:
        This event handler will be called right before the control is disabled.
      See also:
        OnAfterDisable, OnBeforeShow, OnAfterShow
    }
    property OnBeforeDisable: TNotifyEvent read FOnBeforeDisable
      write FOnBeforeDisable;
    { Description:
        This event handler will be called right after the control is disabled.
      See also:
        OnBeforeDisable, OnBeforeShow, OnAfterShow
    }
    property OnAfterDisable: TNotifyEvent read FOnAfterDisable
      write FOnAfterDisable;
  end;

  TlvkManualEnabler = class;

  { Description:
      This class is used as items for the Enablers collection property of
      the components that derive from TlvkCustomEnabler.
  }
  TEnablerItem = class(TCollectionItem)
  private
    FEnabler      : TlvkManualEnabler;
    FInvertState  : Boolean;

    procedure SetEnabler(const Value: TlvkManualEnabler);
    procedure SetInvertState(const Value: Boolean);
    procedure SetState(const State: Boolean);

  protected
    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;

  published
    { Description:
        Which enabler to enable/disable.
    }
    property Enabler: TlvkManualEnabler read FEnabler write SetEnabler;

    { Description:
        Set this property to True to invert the state of the enable/disable
        action.
    }
    property InvertState: Boolean read FInvertState write SetInvertState
      default DEFAULT_INVERT_STATE;
  end;

  { Description:
      This is the base class for components that will enable or disable other
      components automatically in a gui, depending on certain criteria.

      The component contains a Controls collection that links the other
      components to this enabler.
    See also:
      TlvkCheckBoxEnabler, TlvkManualEnabler
  }
  TlvkCustomEnabler = class(TlvkComponent)
  private
    FControls : TOwnedCollection;
    FEnablers : TOwnedCollection;
    FSetting  : Boolean;
    FForce    : Boolean;
    FCurrent  : Boolean;

    procedure SetControls(const Value: TOwnedCollection);
    procedure SetEnablers(const Value: TOwnedCollection);

  protected
    procedure ForceRefresh; virtual;
    procedure EnableDisable; virtual;
    function GetCurrentStatus: Boolean; virtual; abstract;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    { Description:
        This property contains a collection of control items that will get
        enabler or disabled depending on the current state of the component.
    }
    property Controls: TOwnedCollection read FControls write SetControls;

    { Description:
        This property contains a collection of enabler components that will get
        enabler or disabled depending on the current state of the component.
    }
    property Enablers: TOwnedCollection read FEnablers write SetEnablers;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component implements the basic framework that all enabler components
      will inherit from that needs to check the state when the message loop
      goes idle.
  }
  TlvkIdleEnabler = class(TlvkCustomEnabler)
  private
    FApplicationEvents  : TApplicationEvents;

    procedure MessageLoopIdle(Sender: TObject; var Done: Boolean);

  protected
    { Description:
        This method should be overridden if the enabler needs to do more
        processing when the loop goes idle. Just remember to call inherited.
    }
    procedure Idle; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component derives from TlvkCustomEnabler and is responsible for
      enabling or disabling a collection of controls in a gui, based on wether
      a thread is running or not.

      Note: The enabler will become Disabled when the thread starts, and
        Enabled when it terminates.

      Note: This component overrides the event handlers for the thread
        component.
    See also:
      TlvkCustomEnabler, TlvkManualEnabler, TlvkCheckBoxEnabler,
      TlvkEventBasedEnabler
  }
  TlvkThreadEnabler = class(TlvkIdleEnabler)
  private
    FThread : TlvkThreadWrapper;

    procedure SetThread(const Value: TlvkThreadWrapper);

  protected
    function GetCurrentStatus: Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  published
    // <ALIAS TlvkCustomEnabler.Controls>
    property Controls;
    // <ALIAS TlvkCustomEnabler.Enablers>
    property Enablers;

    { Description:
        This property controls which thread component that dictates the current
        enabled state.
    }
    property Thread: TlvkThreadWrapper read FThread write SetThread;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;


  { Description:
      This component derives from TlvkCustomEnabler and is responsible for
      enabling or disabling a collection of controls in a gui, based on some
      criteria.
    See also:
      TlvkCustomEnabler, TlvkThreadEnabler, TlvkCheckBoxEnabler,
      TlvkEventBasedEnabler
  }
  TlvkManualEnabler = class(TlvkCustomEnabler)
  private
    FEnabled  : Boolean;

    procedure SetEnabled(const Value: Boolean);

  protected
    function GetCurrentStatus: Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomEnabler.Controls>
    property Controls;
    // <ALIAS TlvkCustomEnabler.Enablers>
    property Enablers;

    { Description:
        This property controls the current state of the controls linked to this
        enabler.
    }
    property Enabled: Boolean read FEnabled write SetEnabled
      default DEFAULT_ENABLED;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      This enabler component uses a checkbox as source for enabling or disabling
      the controls in the collection. If the checkbox is checked, the controls
      will be enabler, otherwise they'll be disabled.
    See also:
      TlvkCustomEnabler, TlvkThreadEnabler, TlvkManualEnabler,
      TlvkEventBasedEnabler
  }
  TlvkCheckBoxEnabler = class(TlvkIdleEnabler)
  private
    FCheckBox : TCustomCheckBox;

    procedure SetCheckBox(const Value: TCustomCheckBox);

  protected
    function GetCurrentStatus: Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  published
    // <ALIAS TlvkCustomEnabler.Controls>
    property Controls;

    { Description:
        This property controls which checkbox is controlling the current state
        of the controls linked to this enabler.
    }
    property CheckBox: TCustomCheckBox read FCheckBox write SetCheckBox;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  TEnablerUpdateEvent = procedure(Sender: TObject; var Enabled: Boolean) of object;

  { Description:
      This enabler component uses an event handler to decide wether to enable
      or disable the controls.
    See also:
      TlvkCustomEnabler, TlvkThreadEnabler, TlvkManualEnabler,
      TlvkCheckBoxEnabler
  }
  TlvkEventBasedEnabler = class(TlvkIdleEnabler)
  private
    FOnUpdate : TEnablerUpdateEvent;

  protected
    function GetCurrentStatus: Boolean; override;

  public
    procedure ForceUpdate; virtual;
    
  published
    // <ALIAS TlvkCustomEnabler.Controls>
    property Controls;

    { Description:
        This property controls which checkbox is controlling the current state
        of the controls linked to this enabler.
    }
    property OnUpdate: TEnablerUpdateEvent read FOnUpdate write FOnUpdate;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

type
  TCollectionCracker = class(TOwnedCollection);
  TCheckBoxCracker = class(TCustomCheckBox);

{ TlvkCustomEnabler }

constructor TlvkCustomEnabler.Create(AOwner: TComponent);
begin
  inherited;

  FControls := TOwnedCollection.Create(Self, TControlItem);
  FEnablers := TOwnedCollection.Create(Self, TEnablerItem);

  ForceRefresh;
end;

destructor TlvkCustomEnabler.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(FEnablers);

  inherited;
end;

procedure TlvkCustomEnabler.EnableDisable;
var
  Index   : Integer;
  Status  : Boolean;
begin
  if ComponentState * [csLoading, csDesigning] <> [] then
    Exit;

  if not FSetting then
  begin
    FSetting := True;
    try
      Status := GetCurrentStatus;
      if FForce or (Status <> FCurrent) then
      begin
        for Index := 0 to FControls.Count-1 do
          if Assigned(TControlItem(FControls.Items[Index]).Control) then
            TControlItem(FControls.Items[Index]).SetState(Status);

        for Index := 0 to FEnablers.Count-1 do
          if Assigned(TEnablerItem(FEnablers.Items[Index]).Enabler) then
            TEnablerItem(FEnablers.Items[Index]).SetState(Status);
      end;

      FCurrent := Status;
      FForce := False;
    finally
      FSetting := False;
    end;
  end;
end;

procedure TlvkCustomEnabler.ForceRefresh;
begin
  FForce := True;
end;

procedure TlvkCustomEnabler.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if Assigned(FControls) then
  begin
    if Operation = opRemove then
    begin
      Index := 0;
      while Index < FControls.Count do
        if TControlItem(FControls.Items[Index]).Control = AComponent then
          FControls.Delete(Index)
        else
          Inc(Index);
    end;
  end;
  if Assigned(FEnablers) then
  begin
    if Operation = opRemove then
    begin
      Index := 0;
      while Index < FEnablers.Count do
        if TEnablerItem(FEnablers.Items[Index]).Enabler = AComponent then
          FEnablers.Delete(Index)
        else
          Inc(Index);
    end;
  end;
end;

procedure TlvkCustomEnabler.SetControls(const Value: TOwnedCollection);
begin
  FControls.Assign(Value);
end;

procedure TlvkCustomEnabler.SetEnablers(const Value: TOwnedCollection);
begin
  FEnablers.Assign(Value);
end;

{ TControlItem }

procedure TControlItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TControlItem then
  begin
    MemberwiseCopy(Self, Dest);
  end else
    inherited;
end;

constructor TControlItem.Create(Collection: TCollection);
begin
  inherited;

  FEnable := DEFAULT_ENABLE;
  FVisibility := DEFAULT_VISIBILITY;
end;

function TControlItem.GetDisplayName: string;
begin
  if Assigned(FControl) then
    Result := FControl.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TControlItem.SetControl(const Value: TControl);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    (TCollectionCracker(Collection).GetOwner as TlvkCustomEnabler).EnableDisable;
  end;
end;

procedure TControlItem.SetEnable(const Value: TEnable);
begin
  if Value <> FEnable then
  begin
    FEnable := Value;
    (TCollectionCracker(Collection).GetOwner as TlvkCustomEnabler).EnableDisable;
  end;
end;

procedure TControlItem.SetState(const State: Boolean);

  procedure SetEnabled(const Control: TControl; const Enabled: Boolean);
  begin
    if FControl.Enabled <> Enabled then
    begin
      if Enabled then
      begin
        if Assigned(FOnBeforeEnable) then
          FOnBeforeEnable(TCollectionCracker(Collection).GetOwner);
        FControl.Enabled := Enabled;
        if Assigned(FOnAfterEnable) then
          FOnAfterEnable(TCollectionCracker(Collection).GetOwner);
      end else begin
        if Assigned(FOnBeforeDisable) then
          FOnBeforeDisable(TCollectionCracker(Collection).GetOwner);
        FControl.Enabled := Enabled;
        if Assigned(FOnAfterDisable) then
          FOnAfterDisable(TCollectionCracker(Collection).GetOwner);
      end;
    end;
  end;

  procedure SetVisibility(const Control: TControl; const Visible: Boolean);
  begin
    if FControl.Visible <> Visible then
    begin
      if Visible then
      begin
        if Assigned(FOnBeforeShow) then
          FOnBeforeShow(TCollectionCracker(Collection).GetOwner);
        FControl.Visible := Visible;
        if Assigned(FOnAfterShow) then
          FOnAfterShow(TCollectionCracker(Collection).GetOwner);
      end else begin
        if Assigned(FOnBeforeHide) then
          FOnBeforeHide(TCollectionCracker(Collection).GetOwner);
        FControl.Visible := Visible;
        if Assigned(FOnAfterHide) then
          FOnAfterHide(TCollectionCracker(Collection).GetOwner);
      end;
    end;
  end;

begin
  if Assigned(FControl) then
  begin
    case FEnable of
      eLeaveState:
        ;

      eSetState:
        SetEnabled(FControl, State);

      eInvertState:
        SetEnabled(FControl, not State);
    end;

    case FVisibility of
      vLeaveState:
        ;

      vSetState:
        SetVisibility(FControl, State);

      vInvertState:
        SetVisibility(FControl, not State);
    end;
  end;
end;

procedure TControlItem.SetVisibility(const Value: TVisibility);
begin
  if Value <> FVisibility then
  begin
    FVisibility := Value;
    (TCollectionCracker(Collection).GetOwner as TlvkCustomEnabler).EnableDisable;
  end;
end;

{ TlvkCheckBoxEnabler }

function TlvkCheckBoxEnabler.GetCurrentStatus: Boolean;
begin
  if Assigned(FCheckBox) then
    Result := TCheckBoxCracker(FCheckBox).GetChecked
  else
    Result := True;
end;

procedure TlvkCheckBoxEnabler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FCheckBox) then
    CheckBox := nil;
end;

procedure TlvkCheckBoxEnabler.SetCheckBox(const Value: TCustomCheckBox);
begin
  if Value <> FCheckBox then
    FCheckBox := Value;
end;

{ TlvkManualEnabler }

constructor TlvkManualEnabler.Create(AOwner: TComponent);
begin
  inherited;

  FEnabled := DEFAULT_ENABLED;
end;

function TlvkManualEnabler.GetCurrentStatus: Boolean;
begin
  Result := FEnabled;
end;

procedure TlvkManualEnabler.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    EnableDisable;
  end;
end;

{ TEnablerItem }

constructor TEnablerItem.Create(Collection: TCollection);
begin
  inherited;

  FInvertState := DEFAULT_INVERT_STATE;
end;

function TEnablerItem.GetDisplayName: string;
begin
  if Assigned(FEnabler) then
    Result := FEnabler.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TEnablerItem.SetEnabler(const Value: TlvkManualEnabler);
begin
  if Value <> FEnabler then
  begin
    FEnabler := Value;
    (TCollectionCracker(Collection).GetOwner as TlvkCustomEnabler).EnableDisable;
  end;
end;

procedure TEnablerItem.SetInvertState(const Value: Boolean);
begin
  if Value <> FInvertState then
  begin
    FInvertState := Value;
    (TCollectionCracker(Collection).GetOwner as TlvkCustomEnabler).EnableDisable;
  end;
end;

procedure TEnablerItem.SetState(const State: Boolean);
begin
  if Assigned(FEnabler) then
    FEnabler.Enabled := State xor FInvertState;
end;

{ TlvkIdleEnabler }

constructor TlvkIdleEnabler.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(Self);
    FApplicationEvents.OnIdle := MessageLoopIdle;
  end;
  ForceRefresh;
end;

destructor TlvkIdleEnabler.Destroy;
begin
  FApplicationEvents.Free;

  inherited;
end;

procedure TlvkIdleEnabler.Idle;
begin
  EnableDisable;
end;

procedure TlvkIdleEnabler.MessageLoopIdle(Sender: TObject;
  var Done: Boolean);
begin
  Idle;
end;

{ TlvkThreadEnabler }

function TlvkThreadEnabler.GetCurrentStatus: Boolean;
begin
  if Assigned(FThread) then
    Result := not FThread.Running
  else
    Result := True;
end;

procedure TlvkThreadEnabler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FThread) then
    FThread := nil;
end;

procedure TlvkThreadEnabler.SetThread(const Value: TlvkThreadWrapper);
begin
  if Value <> FThread then
    FThread := Value;
end;

{ TlvkEventBasedEnabler }

procedure TlvkEventBasedEnabler.ForceUpdate;
begin
  EnableDisable;
end;

function TlvkEventBasedEnabler.GetCurrentStatus: Boolean;
begin
  if Assigned(FOnUpdate) then
  begin
    Result := FCurrent;
    FOnUpdate(Self, Result);
  end else
    Result := True;
end;

end.
