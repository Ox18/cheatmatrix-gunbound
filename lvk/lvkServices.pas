{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvkServices;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkServices.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

type
  TServiceStatus = (stUnknown, stStopped, stStartPending, stStopPending,
    stRunning, stContinuePending, stPausePending, stPaused);
  TServiceStatusSet = set of TServiceStatus;

function ServiceExists(const MachineName, ServiceName: string): Boolean;
function GetServiceStatus(const MachineName, ServiceName: string): TServiceStatus;
function StartService(const MachineName, ServiceName: string): Boolean;
function StopService(const MachineName, ServiceName: string): Boolean;
function PauseService(const MachineName, ServiceName: string): Boolean;
function ContinueService(const MachineName, ServiceName: string): Boolean;
function GetServiceDisplayName(const MachineName, ServiceName: string): string;
function WaitForServiceStatus(const MachineName, ServiceName: string;
  const AcceptedStatus: TServiceStatusSet; const Timeout: LongWord): Boolean;

const
  LOCAL_MACHINE       = '';
  
  NO_SERVICE_MANAGER  = 'No service manager provided';
  NO_SERVICE_NAME     = 'No service name provided';

implementation

uses
  SysUtils, Windows, WinSvc;

function OpenServiceManager(const MachineName: string): SC_HANDLE;
begin
  if Trim(MachineName) = '' then
    Result := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS)
  else
    Result := OpenSCManager(PChar(Trim(MachineName)), nil, SC_MANAGER_ALL_ACCESS)
end;

function OpenService(const ServiceManager: SC_HANDLE; const ServiceName: string): SC_HANDLE;
begin
  Assert(ServiceManager <> 0, NO_SERVICE_MANAGER);
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  Result := WinSvc.OpenService(ServiceManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
end;

function ServiceExists(const MachineName, ServiceName: string): Boolean;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := False
    else try
      Result := True;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function WaitForServiceStatus(const MachineName, ServiceName: string;
  const AcceptedStatus: TServiceStatusSet; const Timeout: LongWord): Boolean;
var
  Start : TDateTime;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);
  Start := Now;

  Result := False;
  while Now < Start + Timeout/86400000.0 do
  begin
    if GetServiceStatus(MachineName, ServiceName) in AcceptedStatus then
    begin
      Result := True;
      Break;
    end else
      Sleep(250);
  end;
end;

function GetServiceStatus(const MachineName, ServiceName: string): TServiceStatus;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  Status          : WinSvc.TServiceStatus;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := stUnknown
    else try
      if QueryServiceStatus(hService, Status) then
        case Status.dwCurrentState of
          SERVICE_STOPPED           : Result := stStopped;
          SERVICE_START_PENDING     : Result := stStartPending;
          SERVICE_STOP_PENDING      : Result := stStopPending;
          SERVICE_RUNNING           : Result := stRunning;
          SERVICE_CONTINUE_PENDING  : Result := stContinuePending;
          SERVICE_PAUSE_PENDING     : Result := stPausePending;
          SERVICE_PAUSED            : Result := stPaused;
        else
          Result := stUnknown;
        end
      else
        Result := stUnknown;
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function GetServiceDisplayName(const MachineName, ServiceName: string): string;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  DisplayName     : array[0..MAX_PATH] of Char;
  Size            : DWord;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := ''
    else try
      if WinSvc.GetServiceDisplayName(hServiceManager, PChar(ServiceName), DisplayName, Size) then
        Result := DisplayName
      else
        Result := '';
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function StartService(const MachineName, ServiceName: string): Boolean;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  Args            : PChar;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := False
    else try
      Args := nil;
      Result := WinSvc.StartService(hService, 0, Args);
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function StopService(const MachineName, ServiceName: string): Boolean;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  Status          : WinSvc.TServiceStatus;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := False
    else try
      Result := WinSvc.ControlService(hService, SERVICE_CONTROL_STOP, Status);
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function PauseService(const MachineName, ServiceName: string): Boolean;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  Status          : WinSvc.TServiceStatus;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := False
    else try
      Result := WinSvc.ControlService(hService, SERVICE_CONTROL_PAUSE, Status);
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

function ContinueService(const MachineName, ServiceName: string): Boolean;
var
  hServiceManager : SC_HANDLE;
  hService        : SC_HANDLE;
  Status          : WinSvc.TServiceStatus;
begin
  Assert(Trim(ServiceName) <> '', NO_SERVICE_NAME);

  hServiceManager := OpenServiceManager(MachineName);
  try
    hService := OpenService(hServiceManager, ServiceName);
    if hService = 0 then
      Result := False
    else try
      Result := WinSvc.ControlService(hService, SERVICE_CONTROL_CONTINUE, Status);
    finally
      CloseServiceHandle(hService);
    end;
  finally
    CloseServiceHandle(hServiceManager);
  end;
end;

end.
