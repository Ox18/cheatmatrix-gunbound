{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains various thread synchronization objects.
}
unit lvkSynchronizerObjects;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSynchronizerObjects.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFNDEF DELPHI6UP}
  Forms,
  {$ENDIF}
  Controls, SysUtils, Classes, StdCtrls, ComCtrls, lvkTypes;

type
  TlvkSynchronizerObject = class(TInterfacedObject)
  protected
    procedure Execute_MainThread; virtual; abstract;
    procedure Execute_Thread; virtual;
  end;

  IlvkProgressBarSynchronizerObject = interface
    ['{F8DA6768-E409-43A7-8DD9-507B25E7BC52}']

    function GetMin: Integer;
    procedure SetMin(const Value: Integer);
    property Min: Integer read GetMin write SetMin;

    function GetMax: Integer;
    procedure SetMax(const Value: Integer);
    property Max: Integer read GetMax write SetMax;

    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    property Position: Integer read GetPosition write SetPosition;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    procedure SetProperties(const Position: Integer; const Min: Integer=-1;
      const Max: Integer=-1; const Visible: TTriState=tsUnknown);
  end;

  TlvkProgressBarAction = (paSetMin, paSetMax, paSetPosition, paSetVisible);
  TlvkProgressBarActions = set of TlvkProgressBarAction;
  TlvkProgressBarSynchronizerObject = class(TlvkSynchronizerObject,
    IlvkProgressBarSynchronizerObject)
  private
    FProgressBar  : TProgressBar;
    FActions      : TlvkProgressBarActions;
    FNewMin       : Integer;
    FNewMax       : Integer;
    FNewPosition  : Integer;
    FNewVisible   : Boolean;

  protected
    procedure Execute_MainThread; override;
    function GetMin: Integer;
    procedure SetMin(const Value: Integer);
    function GetMax: Integer;
    procedure SetMax(const Value: Integer);
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

  public
    constructor Create(const ProgressBar: TProgressBar);

    property Min: Integer read GetMin write SetMin;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition;
    property Visible: Boolean read GetVisible write SetVisible;

    procedure SetProperties(const Position: Integer; const Min: Integer=-1;
      const Max: Integer=-1; const Visible: TTriState=tsUnknown);
  end;

  IlvkLabelSynchronizerObject = interface
    ['{F8355005-8250-42E9-99AD-1E5CE657D998}']

    function GetCaption: string;
    procedure SetCaption(const Value: string);
    property Caption: string read GetCaption write SetCaption;

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    procedure SetProperties(const Caption: string;
      const Enabled: TTriState=tsUnknown;
      const Visible: TTriState=tsUnknown);
  end;

  TlvkLabelAction = (eaSetCaption, eaSetEnabled, eaSetVisible, eaGetCaption);
  TlvkLabelActions = set of TlvkLabelAction;
  TlvkLabelSynchronizerObject = class(TlvkSynchronizerObject,
    IlvkLabelSynchronizerObject)
  private
    FLabel    : TLabel;
    FActions  : TlvkLabelActions;
    FCaption  : string;
    FEnabled  : Boolean;
    FVisible  : Boolean;

  protected
    procedure Execute_MainThread; override;

    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

  public
    constructor Create(const Label_: TLabel);

    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Visible: Boolean read GetVisible write SetVisible;

    procedure SetProperties(const Caption: string;
      const Enabled: TTriState=tsUnknown;
      const Visible: TTriState=tsUnknown);
  end;

implementation

uses
  Messages, Windows;

const
  CM_SYNCHRONIZE  = WM_USER + 1;

type
  TSynchroObject = class
  private
    FHandle : THandle;

  protected
    procedure WndProc(var Msg: TMessage);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const Synchronizer: TLvkSynchronizerObject);
  end;

var
  SynchroObject : TSynchroObject  = nil;

procedure CreateSynchroWindow;
begin
  SynchroObject := TSynchroObject.Create;
end;

procedure DestroySynchroWindow;
begin
  FreeAndNil(SynchroObject);
end;

{ TlvkSynchronizerObject }

procedure TlvkSynchronizerObject.Execute_Thread;
begin
  if Assigned(SynchroObject) then
    SynchroObject.Execute(Self);
end;

{ TlvkProgressBarSynchronizerObject }

constructor TlvkProgressBarSynchronizerObject.Create(
  const ProgressBar: TProgressBar);
begin
  inherited Create;

  Assert(Assigned(ProgressBar));
  FProgressBar := ProgressBar;
end;

procedure TlvkProgressBarSynchronizerObject.Execute_MainThread;
begin
  if paSetMin in FActions then
    FProgressBar.Min := FNewMin;
  if paSetMax in FActions then
    FProgressBar.Max := FNewMax;
  if paSetPosition in FActions then
    FProgressBar.Position := FNewPosition;
  if paSetVisible in FActions then
    FProgressBar.Visible := FNewVisible;
end;

function TlvkProgressBarSynchronizerObject.GetMax: Integer;
begin
  Result := FProgressBar.Max;
end;

function TlvkProgressBarSynchronizerObject.GetMin: Integer;
begin
  Result := FProgressBar.Min;
end;

function TlvkProgressBarSynchronizerObject.GetPosition: Integer;
begin
  Result := FProgressBar.Position;
end;

function TlvkProgressBarSynchronizerObject.GetVisible: Boolean;
begin
  Result := FProgressBar.Visible;
end;

procedure TlvkProgressBarSynchronizerObject.SetMax(const Value: Integer);
begin
  FActions := [paSetMax];
  FNewMax := Value;
  Execute_Thread;
end;

procedure TlvkProgressBarSynchronizerObject.SetMin(const Value: Integer);
begin
  FActions := [paSetMin];
  FNewMin := Value;
  Execute_Thread;
end;

procedure TlvkProgressBarSynchronizerObject.SetPosition(
  const Value: Integer);
begin
  FActions := [paSetPosition];
  FNewPosition := Value;
  Execute_Thread;
end;

procedure TlvkProgressBarSynchronizerObject.SetProperties(const Position,
  Min, Max: Integer; const Visible: TTriState);
begin
  FActions := [paSetPosition];
  FNewPosition := Position;

  if Min <> -1 then
  begin
    FNewMin := Min;
    Include(FActions, paSetMin);
  end;

  if Max <> -1 then
  begin
    FNewMax := Max;
    Include(FActions, paSetMax);
  end;

  if Visible <> tsUnknown then
  begin
    FNewVisible := TriStateToBoolean[Visible];
    Include(FActions, paSetvisible);
  end;

  Execute_Thread;
end;

procedure TlvkProgressBarSynchronizerObject.SetVisible(
  const Value: Boolean);
begin
  FActions := [paSetVisible];
  FNewVisible := Value;
  Execute_Thread;
end;

{ TSynchroObject }

constructor TSynchroObject.Create;
begin
  inherited Create;
  FHandle := AllocateHWnd(WndProc)
end;

destructor TSynchroObject.Destroy;
begin
  DeallocateHWnd(FHandle);

  inherited;
end;

procedure TSynchroObject.Execute(
  const Synchronizer: TLvkSynchronizerObject);
begin
  SendMessage(FHandle, CM_SYNCHRONIZE, Integer(Synchronizer), 0);
end;

procedure TSynchroObject.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = CM_SYNCHRONIZE then
    TlvkSynchronizerObject(Msg.WParam).Execute_MainThread
  else
    DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{ TlvkLabelSynchronizerObject }

constructor TlvkLabelSynchronizerObject.Create(const Label_: TLabel);
begin
  inherited Create;

  Assert(Assigned(Label_));

  FLabel := Label_;
end;

procedure TlvkLabelSynchronizerObject.Execute_MainThread;
begin
  if eaSetCaption in FActions then
    FLabel.Caption := FCaption;
  if eaSetEnabled in FActions then
    FLabel.Enabled := FEnabled;
  if eaSetVisible in FActions then
    FLabel.Visible := FVisible;
  if eaGetCaption in FActions then
    FCaption := FLabel.Caption;
end;

function TlvkLabelSynchronizerObject.GetCaption: string;
begin
  FActions := [eaGetCaption];
  Execute_Thread;
  Result := FCaption;
end;

function TlvkLabelSynchronizerObject.GetEnabled: Boolean;
begin
  Result := FLabel.Enabled;
end;

function TlvkLabelSynchronizerObject.GetVisible: Boolean;
begin
  Result := FLabel.Visible;
end;

procedure TlvkLabelSynchronizerObject.SetCaption(const Value: string);
begin
  FActions := [eaSetCaption];
  FCaption := Value;
  Execute_Thread;
end;

procedure TlvkLabelSynchronizerObject.SetEnabled(const Value: Boolean);
begin
  FActions := [eaSetEnabled];
  FEnabled := Value;
  Execute_Thread;
end;

procedure TlvkLabelSynchronizerObject.SetProperties(const Caption: string;
  const Enabled, Visible: TTriState);
begin
  FActions := [eaSetCaption];
  FCaption := Caption;

  if Enabled <> tsUnknown then
  begin
    FEnabled := TriStateToBoolean[Enabled];
    Include(FActions, eaSetEnabled);
  end;

  if Visible <> tsUnknown then
  begin
    FVisible := TriStateToBoolean[Visible];
    Include(FActions, eaSetVisible);
  end;

  Execute_Thread;
end;

procedure TlvkLabelSynchronizerObject.SetVisible(const Value: Boolean);
begin
  FActions := [eaSetVisible];
  FVisible := Value;
  Execute_Thread;
end;

initialization
  CreateSynchroWindow;
finalization
  DestroySynchroWindow;
end.
