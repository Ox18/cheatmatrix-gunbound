{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component that is used to dispatch information from
    a thread to a progressbar, a log console or a status bar, in a thread-safe
    manner.
}
unit lvkThreadInfoDispatcher;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 16:57 $
// $Archive: /Components/LVK/Source/lvkThreadInfoDispatcher.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, ComCtrls,
  lvkComponents, lvkThreadCommunicationsQueue, lvkConsole, lvkStatusBar,
  lvkStandardQueueItems, lvkProgressBar;

type
  TlvkCustomThreadInfoDispatcher = class(TlvkComponent)
  private
    FQueue        : TlvkThreadCommunicationsQueue;
    FLogConsole   : TlvkCustomConsole;
    FStatusBar    : TlvkLabelStatusBarPanel;
    FProgressBar  : TControl;

    procedure Push(const CommandID: Integer); overload;
    procedure Push(const CommandID: Integer; const Data: IUnknown); overload;
    procedure ItemAddedToQueue(Sender: TObject);
    procedure HandleItem(const Item: IArrayItem);

    procedure DoAddToLog(const Msg: string);
    procedure DoReplaceLastLog(const Msg: string);
    procedure DoClearLog;
    procedure DoSetStatus(const Msg: string);
    procedure DoClearStatus;
    procedure DoShowProgress;
    procedure DoSetVCLProgress(const CurrentProgress, TotalProgress: Integer);
    procedure DoSetLVKProgress(const CurrentProgress, TotalProgress: Integer);
    procedure DoSetLVKSegmentedProgress(const Segment, CurrentProgress, TotalProgress: Integer);
    procedure DoSetWinControlProgress(const CurrentProgress, TotalProgress: Integer);
    procedure DoHideProgress;
    procedure SetProgressBar(const Value: TControl);

  protected
    property LogConsole: TlvkCustomConsole read FLogConsole write FLogConsole;
    property StatusBar: TlvkLabelStatusBarPanel read FStatusBar write FStatusBar;
    property ProgressBar: TControl read FProgressBar write SetProgressBar;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Log(const Msg: string); overload;
    procedure Log(const Fmt: string; const Args: array of const); overload;
    procedure ReplaceLastLog(const Msg: string); overload;
    procedure ReplaceLastLog(const Fmt: string; const Args: array of const); overload;
    procedure ClearLog;

    procedure Status(const Msg: string); overload;
    procedure Status(const Fmt: string; const Args: array of const); overload;
    procedure ClearStatus;

    procedure ShowProgress;
    procedure Progress(const SegmentIndex, CurrentProgress, TotalProgress: Integer); overload;
    procedure Progress(const CurrentProgress, TotalProgress: Integer); overload;
    procedure Progress(const CurrentProgress: Integer); overload;
    procedure HideProgress;
  end;

  TlvkThreadInfoDispatcher = class(TlvkCustomThreadInfoDispatcher)
  published
    property LogConsole;
    property StatusBar;
    property ProgressBar;
  end;

  ElvkThreadInfoDispatcher = class(Exception);

resourcestring
  SERR_INVALID_PROGRESSBAR_CLASS  = 'Invalid progressbar-class';

implementation

const
  CMD_ADD_TO_LOG        = 0;
  CMD_REPLACE_LAST_LOG  = 1;
  CMD_CLEAR_LOG         = 2;

  CMD_SET_STATUS        = 3;
  CMD_CLEAR_STATUS      = 4;

  CMD_SHOW_PROGRESS     = 5;
  CMD_SET_PROGRESS      = 6;
  CMD_HIDE_PROGRESS     = 7;

type
  TCrackWinControl = class(TWinControl);

{ TlvkCustomThreadInfoDispatcher }

procedure TlvkCustomThreadInfoDispatcher.ClearLog;
begin
  Push(CMD_CLEAR_LOG);
end;

procedure TlvkCustomThreadInfoDispatcher.ClearStatus;
begin
  Push(CMD_CLEAR_STATUS);
end;

constructor TlvkCustomThreadInfoDispatcher.Create(AOwner: TComponent);
begin
  inherited;

  FQueue := TlvkThreadCommunicationsQueue.Create(Self);
  FQueue.ItemAdded := ItemAddedToQueue;
end;

destructor TlvkCustomThreadInfoDispatcher.Destroy;
begin
  FreeAndNil(FQueue);
  
  inherited;
end;

procedure TlvkCustomThreadInfoDispatcher.DoAddToLog(const Msg: string);
begin
  if Assigned(FLogConsole) then
    FLogConsole.Add(Msg);
end;

procedure TlvkCustomThreadInfoDispatcher.DoClearLog;
begin
  if Assigned(FLogConsole) then
    FLogConsole.Clear;
end;

procedure TlvkCustomThreadInfoDispatcher.DoClearStatus;
begin
  if Assigned(FStatusBar) then
    FStatusBar.Caption := '';
end;

procedure TlvkCustomThreadInfoDispatcher.DoHideProgress;
begin
  if Assigned(FProgressBar) then
    FProgressBar.Hide;
end;

procedure TlvkCustomThreadInfoDispatcher.DoSetLVKProgress(
  const CurrentProgress, TotalProgress: Integer);
begin
  if Assigned(FProgressBar) then
  begin
    (FProgressBar as TlvkProgressBar).BeginUpdate;
    try
      if TotalProgress <> -1 then
        (FProgressBar as TlvkProgressBar).MaxValue := TotalProgress;
      (FProgressBar as TlvkProgressBar).Value := CurrentProgress;
    finally
      (FProgressBar as TlvkProgressBar).EndUpdate;
    end;
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.DoSetLVKSegmentedProgress(
  const Segment, CurrentProgress, TotalProgress: Integer);
begin
  if Assigned(FProgressBar) then
  begin
    (FProgressBar as TlvkSegmentedProgressBar).Segments[Segment].BeginUpdate;
    try
      if TotalProgress <> -1 then
        (FProgressBar as TlvkSegmentedProgressBar).Segments[Segment].MaxValue := TotalProgress;
      (FProgressBar as TlvkSegmentedProgressBar).Segments[Segment].Value := CurrentProgress;
    finally
      (FProgressBar as TlvkSegmentedProgressBar).Segments[Segment].EndUpdate;
    end;
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.DoSetStatus(const Msg: string);
begin
  if Assigned(FStatusBar) then
    FStatusBar.Caption := Msg;
end;

procedure TlvkCustomThreadInfoDispatcher.DoSetVCLProgress(
  const CurrentProgress, TotalProgress: Integer);
begin
  if Assigned(FProgressBar) then
  begin
    if TotalProgress <> -1 then
      (FProgressBar as TProgressBar).Max := TotalProgress;
    (FProgressBar as TProgressBar).Position := CurrentProgress;
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.DoShowProgress;
begin
  if Assigned(FProgressBar) then
    FProgressBar.Show;
end;

procedure TlvkCustomThreadInfoDispatcher.HandleItem(
  const Item: IArrayItem);
var
  CommandID : Integer;
  Data      : IUnknown;
begin
  CommandID := (Item[0] as IIntegerItem).Value;
  if Item.Count > 1 then
    Data := Item[1]
  else
    Data := nil;

  case CommandID of
    CMD_ADD_TO_LOG:
      DoAddToLog((Data as IStringItem).Value);

    CMD_REPLACE_LAST_LOG:
      DoReplaceLastLog((Data as IStringItem).Value);

    CMD_CLEAR_LOG:
      DoClearLog;

    CMD_SET_STATUS:
      DoSetStatus((Data as IStringItem).Value);

    CMD_CLEAR_STATUS:
      DoClearStatus;

    CMD_SHOW_PROGRESS:
      DoShowProgress;

    CMD_SET_PROGRESS:
      if Assigned(FProgressBar) then
      begin
        if FProgressBar is TProgressBar then
        begin
          DoSetVCLProgress(((Data as IArrayItem)[0] as IIntegerItem).Value,
            ((Data as IArrayItem)[1] as IIntegerItem).Value);
        end else if FProgressBar is TlvkProgressBar then
        begin
          DoSetLVKProgress(((Data as IArrayItem)[0] as IIntegerItem).Value,
            ((Data as IArrayItem)[1] as IIntegerItem).Value);
        end else if FProgressBar is TlvkSegmentedProgressBar then
        begin
          DoSetLVKSegmentedProgress(
            ((Data as IArrayItem)[0] as IIntegerItem).Value,
            ((Data as IArrayItem)[1] as IIntegerItem).Value,
            ((Data as IArrayItem)[2] as IIntegerItem).Value);
        end else
          if FProgressBar is TWinControl then
            DoSetWinControlProgress(((Data as IArrayItem)[0] as IIntegerItem).Value,
              ((Data as IArrayItem)[1] as IIntegerItem).Value);
      end;

    CMD_HIDE_PROGRESS:
      DoHideProgress;
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.HideProgress;
begin
  Push(CMD_HIDE_PROGRESS);
end;

procedure TlvkCustomThreadInfoDispatcher.ItemAddedToQueue(Sender: TObject);
var
  Item  : IUnknown;
begin
  while FQueue.Pop(Item, False, 0) do
  begin
    HandleItem(Item as IArrayItem);
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.Log(const Fmt: string;
  const Args: array of const);
begin
  Push(CMD_ADD_TO_LOG, NewStringItem(Fmt, Args));
end;

procedure TlvkCustomThreadInfoDispatcher.Log(const Msg: string);
begin
  Push(CMD_ADD_TO_LOG, NewStringItem(Msg));
end;

procedure TlvkCustomThreadInfoDispatcher.Progress(
  const CurrentProgress: Integer);
begin
  Push(CMD_SET_PROGRESS, NewArrayItem([NewIntegerItem(CurrentProgress),
    NewIntegerItem(-1)]));
end;

procedure TlvkCustomThreadInfoDispatcher.Progress(const CurrentProgress,
  TotalProgress: Integer);
begin
  Push(CMD_SET_PROGRESS, NewArrayItem([NewIntegerItem(CurrentProgress),
    NewIntegerItem(TotalProgress)]));
end;

procedure TlvkCustomThreadInfoDispatcher.Push(const CommandID: Integer);
begin
  if Assigned(FQueue) then
    FQueue.Push(NewArrayItem([NewIntegerItem(CommandID)]));
end;

procedure TlvkCustomThreadInfoDispatcher.Progress(const SegmentIndex,
  CurrentProgress, TotalProgress: Integer);
begin
  Push(CMD_SET_PROGRESS, NewArrayItem([NewIntegerItem(CurrentProgress),
    NewIntegerItem(TotalProgress), NewIntegerItem(SegmentIndex)]));
end;

procedure TlvkCustomThreadInfoDispatcher.Push(const CommandID: Integer;
  const Data: IUnknown);
begin
  if Assigned(FQueue) then
    FQueue.Push(NewArrayItem([NewIntegerItem(CommandID), Data]));
end;

procedure TlvkCustomThreadInfoDispatcher.SetProgressBar(
  const Value: TControl);
begin
  if (not Assigned(Value)) or (Value is TProgressBar) or
    (Value is TlvkProgressBar) or (Value is TlvkSegmentedProgressBar) or
    (Value is TWinControl) then
  begin
    FProgressBar := Value;
  end else
    raise Exception.Create(SERR_INVALID_PROGRESSBAR_CLASS);
end;

procedure TlvkCustomThreadInfoDispatcher.ShowProgress;
begin
  Push(CMD_SHOW_PROGRESS);
end;

procedure TlvkCustomThreadInfoDispatcher.Status(const Fmt: string;
  const Args: array of const);
begin
  Push(CMD_SET_STATUS, NewStringItem(Fmt, Args));
end;

procedure TlvkCustomThreadInfoDispatcher.Status(const Msg: string);
begin
  Push(CMD_SET_STATUS, NewStringItem(Msg));
end;

procedure TlvkCustomThreadInfoDispatcher.DoSetWinControlProgress(
  const CurrentProgress, TotalProgress: Integer);
begin
  if Assigned(FProgressBar) then
  begin
    if TotalProgress <> -1 then
      TCrackWinControl(FProgressBar).Tag := TotalProgress;
    if TCrackWinControl(FProgressBar).Tag  <> 0 then
      TCrackWinControl(FProgressBar).Text := Format('%.1f %%', [
        (CurrentProgress * 100.0) / TCrackWinControl(FProgressBar).Tag])
    else
      TCrackWinControl(FProgressBar).Text := Format('%.1f %%', [0.0]);
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FLogConsole then
      FLogConsole := nil
    else if AComponent = FStatusBar then
      FStatusBar := nil
    else if AComponent = FProgressBar then
      FProgressBar := nil;
  end;
end;

procedure TlvkCustomThreadInfoDispatcher.ReplaceLastLog(const Msg: string);
begin
  Push(CMD_REPLACE_LAST_LOG, NewStringItem(Msg));
end;

procedure TlvkCustomThreadInfoDispatcher.ReplaceLastLog(const Fmt: string;
  const Args: array of const);
begin
  Push(CMD_REPLACE_LAST_LOG, NewStringItem(Fmt, Args));
end;

procedure TlvkCustomThreadInfoDispatcher.DoReplaceLastLog(
  const Msg: string);
begin
  if Assigned(FLogConsole) then
    if FLogConsole.MessageCount > 0 then
      FLogConsole.Replace(FLogConsole.Messages[0], Msg)
    else
      raise ElvkThreadInfoDispatcher.Create('No message to replace for ' + ClassName);
end;

end.
