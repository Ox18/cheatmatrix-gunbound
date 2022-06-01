{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{ TODO  -oLVK -cQA : Test this code, make sure it works properly }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code for a file change notification thread, a
    component that watches over a directory and handles changes to the files
    in it.
}
unit lvkFileChangeNotificationThread;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFileChangeNotificationThread.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, lvkThread, lvkThreadCommunicationsQueue,
  lvkComponents;

type
  TFileChangeFilter = (cfFileName, cfDirName, cfAttributes, cfSize,
    cfLastWrite, cfSecurity);

  TFileChangeFilters = set of TFileChangeFilter;

  TlvkFileChangeNotificationThread = class(TlvkComponent)
  private
    FSetEnabled : Boolean;
    FThread     : TlvkThread;
    FFilters    : TFileChangeFilters;
    FDirectory  : string;
    FFileMasks  : TStrings;
{$IFDEF DELPHI6UP}
    FQueue      : IThreadCommunicationsQueue;
{$ELSE}
    FQueue      : TlvkThreadCommunicationsQueue;
{$ENDIF}

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure DoEnable;
    procedure DoDisable;
    procedure SetDirectory(const Value: string);
    procedure SetFilters(const Value: TFileChangeFilters);
{$IFDEF DELPHI6UP}
    procedure SetQueue(const Value: IThreadCommunicationsQueue);
{$ELSE}
    procedure SetQueue(const Value: TlvkThreadCommunicationsQueue);
{$ENDIF}

  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
{$IFDEF DELPHI6UP}
    property Queue: IThreadCommunicationsQueue read FQueue write SetQueue;
{$ELSE}
    property Queue: TlvkThreadCommunicationsQueue read FQueue write SetQueue;
{$ENDIF}
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Directory: string read FDirectory write SetDirectory;
    property Filters: TFileChangeFilters read FFilters write SetFilters;
  end;

  IFileChangeNotification = interface
    ['{9144E7D1-AB61-11D5-B29A-0004761A6377}']

    function GetFileChangeNotificationThread: TlvkFileChangeNotificationThread;
    property FileChangeNotificationThread: TlvkFileChangeNotificationThread read GetFileChangeNotificationThread;
  end;

implementation

type
  TScanThread = class(TlvkThread)
  private
    FThread     : TlvkFileChangeNotificationThread;
    FFileMasks  : TStrings;

  protected
    procedure Execute; override;
    function FilterValue: LongWord;

  public
    constructor Create(const Thread: TlvkFileChangeNotificationThread);
    destructor Destroy; override;
  end;

  TFileChangeNotification = class(TInterfacedObject, IFileChangeNotification)
  private
    FThread: TlvkFileChangeNotificationThread;

  protected
    function GetFileChangeNotificationThread: TlvkFileChangeNotificationThread;

  public
    constructor Create(const Thread: TlvkFileChangeNotificationThread);
  end;

{ TlvkFileChangeNotificationThread }

constructor TlvkFileChangeNotificationThread.Create(AOwner: TComponent);
begin
  inherited;

  FThread := nil;
  FFilters := [cfLastWrite];
  FDirectory := 'C:\';
  FFileMasks := TStringList.Create;
end;

destructor TlvkFileChangeNotificationThread.Destroy;
begin
  Enabled := False;
  FFileMasks.Free;

  inherited;
end;

procedure TlvkFileChangeNotificationThread.DoDisable;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TlvkFileChangeNotificationThread.DoEnable;
begin
  Assert(Assigned(FQueue));
  FThread := TScanThread.Create(Self);
end;

function TlvkFileChangeNotificationThread.GetEnabled: Boolean;
begin
  Result := Assigned(FThread);
end;

procedure TlvkFileChangeNotificationThread.Loaded;
begin
  inherited;
  if FSetEnabled then
    Enabled := True;
end;

procedure TlvkFileChangeNotificationThread.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  case Operation of
    opInsert:
      ;

    opRemove:
{$IFDEF DELPHI6UP}
      if AComponent.IsImplementorOf(FQueue) then
{$ELSE}
      if AComponent = FQueue then
{$ENDIF}
      begin
        Enabled := False;
        FQueue := nil;
      end;
  end;
end;

procedure TlvkFileChangeNotificationThread.SetDirectory(
  const Value: string);
begin
  if Value <> FDirectory then
  begin
    if Enabled then
      raise Exception.Create('Cannot change directory while enabled');

    FDirectory := Value;
  end;
end;

procedure TlvkFileChangeNotificationThread.SetEnabled(
  const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    if csLoading in ComponentState then
      FSetEnabled := Value
    else if Value then
      DoEnable
    else
      DoDisable;
  end;
end;

procedure TlvkFileChangeNotificationThread.SetFilters(
  const Value: TFileChangeFilters);
begin
  if FFilters <> Value then
  begin
    if (not (csDesigning in ComponentState)) and Enabled then
      raise Exception.Create('Cannot change directory while enabled');

    FFilters := Value;
  end;
end;

{$IFDEF DELPHI6UP}
procedure TlvkFileChangeNotificationThread.SetQueue(
  const Value: IThreadCommunicationsQueue);
{$ELSE}
procedure TlvkFileChangeNotificationThread.SetQueue(
  const Value: TlvkThreadCommunicationsQueue);
{$ENDIF}
begin
  if FQueue <> Value then
  begin
    if (not (csDesigning in ComponentState)) and Enabled then
      raise Exception.Create('Cannot change queue while enabled');

    FQueue := Value;
  end;
end;

{ TScanThread }

constructor TScanThread.Create(
  const Thread: TlvkFileChangeNotificationThread);
begin
  inherited Create(True);

  FThread := Thread;
  FFileMasks := TStringList.Create;
  FFileMasks.Assign(FThread.FFileMasks);

  Resume;
end;

destructor TScanThread.Destroy;
begin
  FFileMasks.Free;
  inherited;
end;

procedure TScanThread.Execute;
type
  PFileNode = ^TFileNode;
  TFileNode = record
    Filename  : string;

    Found     : Boolean;
    Next      : PFileNode;
  end;
var
  NotificationHandle  : THandle;
  WaitHandles         : array[0..1] of THandle;
  rc                  : DWORD;

  procedure ScanForChanges(const InitialRun: Boolean);
  var
    rc  : Boolean;
  begin
    FThread.Queue.Push(TFileChangeNotification.Create(FThread) as IUnknown);

    rc := FindNextChangeNotification(NotificationHandle);
    Assert(rc);
  end;

begin
  NotificationHandle := FindFirstChangeNotification(PChar(FThread.Directory),
    True{FThread.WatchSubtree}, FilterValue);

  ScanForChanges(True);
  Assert(NotificationHandle <> INVALID_HANDLE_VALUE);
  try
    WaitHandles[0] := TerminationEvent;
    WaitHandles[1] := NotificationHandle;

    repeat
      rc := WaitForMultipleObjects(2, @WaitHandles, False, INFINITE);

      case rc of
        WAIT_OBJECT_0:
          Break;

        WAIT_OBJECT_0+1:
          ScanForChanges(False);
      else
        Assert(False);
      end;
    until False;
  finally
    FindCloseChangeNotification(NotificationHandle);
  end;
end;

function TScanThread.FilterValue: LongWord;
const
  FilterValues  : array[TFileChangeFilter] of LongWord = (
    FILE_NOTIFY_CHANGE_FILE_NAME,
    FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES,
    FILE_NOTIFY_CHANGE_SIZE,
    FILE_NOTIFY_CHANGE_LAST_WRITE,
    FILE_NOTIFY_CHANGE_SECURITY
  );
var
  Index : TFileChangeFilter;
begin
  Result := 0;
  for Index := Low(FilterValues) to High(FilterValues) do
    if Index in FThread.Filters then
      Result := Result or FilterValues[Index];
end;

{ TFileChangeNotification }

constructor TFileChangeNotification.Create(
  const Thread: TlvkFileChangeNotificationThread);
begin
  inherited Create;

  FThread := Thread;
end;

function TFileChangeNotification.GetFileChangeNotificationThread: TlvkFileChangeNotificationThread;
begin
  Result := FThread;
end;

end.
