{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a simple TButton descendand component that runs a
    OnExecute event handler in a thread when clicked. The button will
    either disable itself while the thread is running, or you can
}
unit lvkThreadButton;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkThreadButton.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Classes, Forms, StdCtrls, Buttons, Messages, lvkThread, lvkVersion;

type
{ Description:
    This is a TButton descendant control which fuses a thread with a button.
    Simply put, when you click the button, a background thread starts and
    executes the code in the OnExecute event handler. While the thread is
    running, the button will be disabled.

    Optionally, you can have the button switch to a Cancel button which will
    ask the thread to terminate itself. The button will not become disabled
    if you use the Cancel functionality.
  See also:
    TlvkThreadBitBtn, TlvkThreadSpeedButton, TlvkThreadWrapper
}
  TlvkThreadButton = class(TButton)
  private
    FThread         : TlvkThreadWrapper;
    FOnTerminate    : TNotifyEvent;
    FOnStart        : TNotifyEvent;
    FAllowCancel    : Boolean;
    FOldCaption     : string;
    FCancelCaption  : string;

    function GetPriority: TThreadPriority;
    procedure SetPriority(const Value: TThreadPriority);
    function GetThreadingModel: TlvkThreadingModel;
    procedure SetThreadingModel(const Value: TlvkThreadingModel);
    function GetOnExecute: TThreadExecuteEvent;
    procedure SetOnExecute(const Value: TThreadExecuteEvent);
    procedure SetOnStart(const Value: TNotifyEvent);
    procedure SetOnTerminate(const Value: TNotifyEvent);

    procedure HandleOnStart(Sender: TObject);
    procedure HandleOnTerminate(Sender: TObject);
    procedure SetAllowCancel(const Value: Boolean);
    procedure SetCancelCaption(const Value: string);
    function GetRunning: Boolean;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;

    // <ALIAS TlvkThreadWrapper.Running>
    property Running: Boolean read GetRunning;
    // <ALIAS TlvkCustomThreadWrapper.Terminate>
    procedure Terminate;
    // <ALIAS TlvkCustomThreadWrapper.WaitFor>
    procedure WaitFor;
    // <ALIAS TlvkCustomThreadWrapper.ReturnValue>
    function ReturnValue: LongWord;

  published
    // <ALIAS TlvkThreadWrapper.Priority>
    property Priority: TThreadPriority read GetPriority write SetPriority;

    // <ALIAS TlvkThreadWrapper.ThreadingModel>
    property ThreadingModel: TlvkThreadingModel read GetThreadingModel write SetThreadingModel;

    // <ALIAS TlvkThreadWrapper.OnExecute>
    property OnExecute: TThreadExecuteEvent read GetOnExecute write SetOnExecute;

    // <ALIAS TlvkThreadWrapper.OnStart>
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;

    // <ALIAS TlvkThreadWrapper.OnTerminate>
    property OnTerminate: TNotifyEvent read FOnTerminate write SetOnTerminate;

    { Description:
        If you want the button to switch to a Cancel button instead of becoming
        disabled, set the AllowCancel property to True. You can also set a
        different caption than simply 'Cancel' to be used by changing the
        CancelCaption property.

        Once the thread has finished running, the caption will revert to the
        caption it had before the thread started.
      See also:
        CancelCaption
    }
    property AllowCancel: Boolean read FAllowCancel write SetAllowCancel;

    { Description:
        This property determines what caption to use on the button when the
        thread is running, and the button has switch to becoming a Cancel-button
        instead of becoming disabled while the thread is running.
      See also:
        AllowCancel
    }
    property CancelCaption: string read FCancelCaption write SetCancelCaption;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion write SetPackageVersion stored False;
  end;

{ Description:
    This is a TBitBtn descendant control which fuses a thread with a button.
    Simply put, when you click the button, a background thread starts and
    executes the code in the OnExecute event handler. While the thread is
    running, the button will be disabled.

    Optionally, you can have the button switch to a Cancel button which will
    ask the thread to terminate itself. The button will not become disabled
    if you use the Cancel functionality.
  See also:
    TlvkThreadButton, TlvkThreadSpeedButton, TlvkThreadWrapper
}
  TlvkThreadBitBtn = class(TBitBtn)
  private
    FThread         : TlvkThreadWrapper;
    FOnTerminate    : TNotifyEvent;
    FOnStart        : TNotifyEvent;
    FAllowCancel    : Boolean;
    FOldCaption     : string;
    FCancelCaption  : string;

    function GetPriority: TThreadPriority;
    procedure SetPriority(const Value: TThreadPriority);
    function GetThreadingModel: TlvkThreadingModel;
    procedure SetThreadingModel(const Value: TlvkThreadingModel);
    function GetOnExecute: TThreadExecuteEvent;
    procedure SetOnExecute(const Value: TThreadExecuteEvent);
    procedure SetOnStart(const Value: TNotifyEvent);
    procedure SetOnTerminate(const Value: TNotifyEvent);

    procedure HandleOnStart(Sender: TObject);
    procedure HandleOnTerminate(Sender: TObject);
    procedure SetAllowCancel(const Value: Boolean);
    procedure SetCancelCaption(const Value: string);
    function GetRunning: Boolean;

    procedure SwitchToCancel;
    procedure SwitchFromCancel;
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;

    // <ALIAS TlvkThreadWrapper.Running>
    property Running: Boolean read GetRunning;
    // <ALIAS TlvkCustomThreadWrapper.Terminate>
    procedure Terminate;
    // <ALIAS TlvkCustomThreadWrapper.WaitFor>
    procedure WaitFor;
    // <ALIAS TlvkCustomThreadWrapper.ReturnValue>
    function ReturnValue: LongWord;

  published
    // <ALIAS TlvkThreadWrapper.Priority>
    property Priority: TThreadPriority read GetPriority write SetPriority;

    // <ALIAS TlvkThreadWrapper.ThreadingModel>
    property ThreadingModel: TlvkThreadingModel read GetThreadingModel write SetThreadingModel;

    // <ALIAS TlvkThreadWrapper.OnExecute>
    property OnExecute: TThreadExecuteEvent read GetOnExecute write SetOnExecute;

    // <ALIAS TlvkThreadWrapper.OnStart>
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;

    // <ALIAS TlvkThreadWrapper.OnTerminate>
    property OnTerminate: TNotifyEvent read FOnTerminate write SetOnTerminate;

    { Description:
        If you want the button to switch to a Cancel button instead of becoming
        disabled, set the AllowCancel property to True. You can also set a
        different caption than simply 'Cancel' to be used by changing the
        CancelCaption property.

        Once the thread has finished running, the caption will revert to the
        caption it had before the thread started.
      See also:
        CancelCaption
    }
    property AllowCancel: Boolean read FAllowCancel write SetAllowCancel;

    { Description:
        This property determines what caption to use on the button when the
        thread is running, and the button has switch to becoming a Cancel-button
        instead of becoming disabled while the thread is running.
      See also:
        AllowCancel
    }
    property CancelCaption: string read FCancelCaption write SetCancelCaption;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion write SetPackageVersion stored False;
  end;

{ Description:
    This is a TSpeedButton descendant control which fuses a thread with a
    button.
    Simply put, when you click the button, a background thread starts and
    executes the code in the OnExecute event handler. While the thread is
    running, the button will be disabled.

    Optionally, you can have the button switch to a Cancel button which will
    ask the thread to terminate itself. The button will not become disabled
    if you use the Cancel functionality.
  See also:
    TlvkThreadButton, TlvkThreadSpeedButton, TlvkThreadWrapper
}
  TlvkThreadSpeedButton = class(TSpeedButton)
  private
    FThread         : TlvkThreadWrapper;
    FOnTerminate    : TNotifyEvent;
    FOnStart        : TNotifyEvent;
    FAllowCancel    : Boolean;
    FOldCaption     : string;
    FCancelCaption  : string;

    function GetPriority: TThreadPriority;
    procedure SetPriority(const Value: TThreadPriority);
    function GetThreadingModel: TlvkThreadingModel;
    procedure SetThreadingModel(const Value: TlvkThreadingModel);
    function GetOnExecute: TThreadExecuteEvent;
    procedure SetOnExecute(const Value: TThreadExecuteEvent);
    procedure SetOnStart(const Value: TNotifyEvent);
    procedure SetOnTerminate(const Value: TNotifyEvent);

    procedure HandleOnStart(Sender: TObject);
    procedure HandleOnTerminate(Sender: TObject);
    procedure SetAllowCancel(const Value: Boolean);
    procedure SetCancelCaption(const Value: string);
    function GetRunning: Boolean;
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;

    // <ALIAS TlvkThreadWrapper.Running>
    property Running: Boolean read GetRunning;
    // <ALIAS TlvkCustomThreadWrapper.Terminate>
    procedure Terminate;
    // <ALIAS TlvkCustomThreadWrapper.WaitFor>
    procedure WaitFor;
    // <ALIAS TlvkCustomThreadWrapper.ReturnValue>
    function ReturnValue: LongWord;

  published
    // <ALIAS TlvkThreadWrapper.Priority>
    property Priority: TThreadPriority read GetPriority write SetPriority;

    // <ALIAS TlvkThreadWrapper.ThreadingModel>
    property ThreadingModel: TlvkThreadingModel read GetThreadingModel write SetThreadingModel;

    // <ALIAS TlvkThreadWrapper.OnExecute>
    property OnExecute: TThreadExecuteEvent read GetOnExecute write SetOnExecute;

    // <ALIAS TlvkThreadWrapper.OnStart>
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;

    // <ALIAS TlvkThreadWrapper.OnTerminate>
    property OnTerminate: TNotifyEvent read FOnTerminate write SetOnTerminate;

    { Description:
        If you want the button to switch to a Cancel button instead of becoming
        disabled, set the AllowCancel property to True. You can also set a
        different caption than simply 'Cancel' to be used by changing the
        CancelCaption property.

        Once the thread has finished running, the caption will revert to the
        caption it had before the thread started.
      See also:
        CancelCaption
    }
    property AllowCancel: Boolean read FAllowCancel write SetAllowCancel;

    { Description:
        This property determines what caption to use on the button when the
        thread is running, and the button has switch to becoming a Cancel-button
        instead of becoming disabled while the thread is running.
      See also:
        AllowCancel
    }
    property CancelCaption: string read FCancelCaption write SetCancelCaption;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion write SetPackageVersion stored False;
  end;

implementation

{ TlvkThreadButton }

procedure TlvkThreadButton.Click;
begin
  inherited;

  if FThread.Running then
    FThread.Terminate
  else
    FThread.Start;
end;

constructor TlvkThreadButton.Create(AOwner: TComponent);
begin
  inherited;

  FThread := TlvkThreadWrapper.Create(nil);
  FThread.OnStart := HandleOnStart;
  FThread.OnTerminate := HandleOnTerminate;

  FAllowCancel := False;
  FCancelCaption := 'Cancel';
end;

destructor TlvkThreadButton.Destroy;
begin
  FThread.Terminate;
  while FThread.Running do
  begin
    Application.ProcessMessages;
    {$IFDEF DELPHI6UP}
    CheckSynchronize;
    {$ENDIF}
  end;
  FThread.Free;

  inherited;
end;

function TlvkThreadButton.GetOnExecute: TThreadExecuteEvent;
begin
  Result := FThread.OnExecute;
end;

function TlvkThreadButton.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkThreadButton.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TlvkThreadButton.GetRunning: Boolean;
begin
  Result := FThread.Running;
end;

function TlvkThreadButton.GetThreadingModel: TlvkThreadingModel;
begin
  Result := FThread.ThreadingModel;
end;

procedure TlvkThreadButton.HandleOnStart(Sender: TObject);
begin
  if Assigned(FOnStart) then
    FOnStart(Sender);

  if FAllowCancel then
  begin
    FOldCaption := inherited Caption;
    inherited Caption := FCancelCaption;
  end else
    Enabled := False;
end;

procedure TlvkThreadButton.HandleOnTerminate(Sender: TObject);
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Sender);

  if FAllowCancel then
    inherited Caption := FOldCaption
  else
    Enabled := True;
end;

function TlvkThreadButton.ReturnValue: LongWord;
begin
  Result := FThread.ReturnValue;
end;

procedure TlvkThreadButton.SetAllowCancel(const Value: Boolean);
begin
  if Value <> FAllowCancel then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['AllowCancel']);

    FAllowCancel := Value;
  end;
end;

procedure TlvkThreadButton.SetCancelCaption(const Value: string);
begin
  if Value <> FCancelCaption then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['CancelCaption']);

    FCancelCaption := Value;
  end;
end;

procedure TlvkThreadButton.SetOnExecute(const Value: TThreadExecuteEvent);
begin
  FThread.OnExecute := Value;
end;

procedure TlvkThreadButton.SetOnStart(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnStart then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnStart']);

    FOnStart := Value;
  end;
end;

procedure TlvkThreadButton.SetOnTerminate(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnTerminate then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnTerminate']);

    FOnTerminate := Value;
  end;
end;

procedure TlvkThreadButton.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkThreadButton.SetPriority(const Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TlvkThreadButton.SetThreadingModel(
  const Value: TlvkThreadingModel);
begin
  FThread.ThreadingModel := Value;
end;

procedure TlvkThreadButton.Terminate;
begin
  FThread.Terminate;
end;

procedure TlvkThreadButton.WaitFor;
begin
  FThread.WaitFor;
end;

{ TlvkThreadBitBtn }

procedure TlvkThreadBitBtn.Click;
begin
  inherited;

  if FThread.Running then
    FThread.Terminate
  else
    FThread.Start;
end;

constructor TlvkThreadBitBtn.Create(AOwner: TComponent);
begin
  inherited;

  FThread := TlvkThreadWrapper.Create(nil);
  FThread.OnStart := HandleOnStart;
  FThread.OnTerminate := HandleOnTerminate;

  FAllowCancel := False;
  FCancelCaption := 'Cancel';
end;

destructor TlvkThreadBitBtn.Destroy;
begin
  FThread.Terminate;
  while FThread.Running do
  begin
    Application.ProcessMessages;
    {$IFDEF DELPHI6UP}
    CheckSynchronize;
    {$ENDIF}
  end;
  FThread.Free;

  inherited;
end;

function TlvkThreadBitBtn.GetOnExecute: TThreadExecuteEvent;
begin
  Result := FThread.OnExecute;
end;

function TlvkThreadBitBtn.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkThreadBitBtn.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TlvkThreadBitBtn.GetRunning: Boolean;
begin
  Result := FThread.Running;
end;

function TlvkThreadBitBtn.GetThreadingModel: TlvkThreadingModel;
begin
  Result := FThread.ThreadingModel;
end;

procedure TlvkThreadBitBtn.HandleOnStart(Sender: TObject);
begin
  if Assigned(FOnStart) then
    FOnStart(Sender);

  if FAllowCancel then
  begin
    SwitchToCancel;
  end else
    Enabled := False;
end;

procedure TlvkThreadBitBtn.HandleOnTerminate(Sender: TObject);
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Sender);

  SwitchFromCancel;
end;

function TlvkThreadBitBtn.ReturnValue: LongWord;
begin
  Result := FThread.ReturnValue;
end;

procedure TlvkThreadBitBtn.SetAllowCancel(const Value: Boolean);
begin
  if Value <> FAllowCancel then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['AllowCancel']);

    FAllowCancel := Value;
  end;
end;

procedure TlvkThreadBitBtn.SetCancelCaption(const Value: string);
begin
  if Value <> FCancelCaption then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['CancelCaption']);

    FCancelCaption := Value;
  end;
end;

procedure TlvkThreadBitBtn.SetOnExecute(const Value: TThreadExecuteEvent);
begin
  FThread.OnExecute := Value;
end;

procedure TlvkThreadBitBtn.SetOnStart(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnStart then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnStart']);

    FOnStart := Value;
  end;
end;

procedure TlvkThreadBitBtn.SetOnTerminate(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnTerminate then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnTerminate']);

    FOnTerminate := Value;
  end;
end;

procedure TlvkThreadBitBtn.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkThreadBitBtn.SetPriority(const Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TlvkThreadBitBtn.SetThreadingModel(
  const Value: TlvkThreadingModel);
begin
  FThread.ThreadingModel := Value;
end;

procedure TlvkThreadBitBtn.SwitchFromCancel;
begin
  if FAllowCancel then
    inherited Caption := FOldCaption
  else
    Enabled := True;
end;

procedure TlvkThreadBitBtn.SwitchToCancel;
begin
  FOldCaption := inherited Caption;
  inherited Caption := FCancelCaption;
end;

procedure TlvkThreadBitBtn.Terminate;
begin
  FThread.Terminate;
end;

procedure TlvkThreadBitBtn.WaitFor;
begin
  FThread.WaitFor;
end;

{ TlvkThreadSpeedButton }

procedure TlvkThreadSpeedButton.Click;
begin
  inherited;

  if FThread.Running then
    FThread.Terminate
  else
    FThread.Start;
end;

constructor TlvkThreadSpeedButton.Create(AOwner: TComponent);
begin
  inherited;

  FThread := TlvkThreadWrapper.Create(nil);
  FThread.OnStart := HandleOnStart;
  FThread.OnTerminate := HandleOnTerminate;

  FAllowCancel := False;
  FCancelCaption := 'Cancel';
end;

destructor TlvkThreadSpeedButton.Destroy;
begin
  FThread.Terminate;
  while FThread.Running do
  begin
    Application.ProcessMessages;
    {$IFDEF DELPHI6UP}
    CheckSynchronize;
    {$ENDIF}
  end;
  FThread.Free;

  inherited;
end;

function TlvkThreadSpeedButton.GetOnExecute: TThreadExecuteEvent;
begin
  Result := FThread.OnExecute;
end;

function TlvkThreadSpeedButton.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkThreadSpeedButton.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TlvkThreadSpeedButton.GetRunning: Boolean;
begin
  Result := FThread.Running;
end;

function TlvkThreadSpeedButton.GetThreadingModel: TlvkThreadingModel;
begin
  Result := FThread.ThreadingModel;
end;

procedure TlvkThreadSpeedButton.HandleOnStart(Sender: TObject);
begin
  if Assigned(FOnStart) then
    FOnStart(Sender);

  if FAllowCancel then
  begin
    FOldCaption := inherited Caption;
    inherited Caption := FCancelCaption;
  end else
    Enabled := False;
end;

procedure TlvkThreadSpeedButton.HandleOnTerminate(Sender: TObject);
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Sender);

  if FAllowCancel then
    inherited Caption := FOldCaption
  else
    Enabled := True;
end;

function TlvkThreadSpeedButton.ReturnValue: LongWord;
begin
  Result := FThread.ReturnValue;
end;

procedure TlvkThreadSpeedButton.SetAllowCancel(const Value: Boolean);
begin
  if Value <> FAllowCancel then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['AllowCancel']);

    FAllowCancel := Value;
  end;
end;

procedure TlvkThreadSpeedButton.SetCancelCaption(const Value: string);
begin
  if Value <> FCancelCaption then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['CancelCaption']);

    FCancelCaption := Value;
  end;
end;

procedure TlvkThreadSpeedButton.SetOnExecute(const Value: TThreadExecuteEvent);
begin
  FThread.OnExecute := Value;
end;

procedure TlvkThreadSpeedButton.SetOnStart(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnStart then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnStart']);

    FOnStart := Value;
  end;
end;

procedure TlvkThreadSpeedButton.SetOnTerminate(const Value: TNotifyEvent);
begin
  if @@Value <> @@FOnTerminate then
  begin
    if FThread.Running then
      raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnTerminate']);

    FOnTerminate := Value;
  end;
end;

procedure TlvkThreadSpeedButton.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkThreadSpeedButton.SetPriority(const Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TlvkThreadSpeedButton.SetThreadingModel(
  const Value: TlvkThreadingModel);
begin
  FThread.ThreadingModel := Value;
end;

procedure TlvkThreadSpeedButton.Terminate;
begin
  FThread.Terminate;
end;

procedure TlvkThreadSpeedButton.WaitFor;
begin
  FThread.WaitFor;
end;

end.
