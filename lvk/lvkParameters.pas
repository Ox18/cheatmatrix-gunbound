{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component for easy handling of command line parameters,
    including passing the parameters to the first instance of the application
    (if needed).
}
unit lvkParameters;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkParameters.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, Messages, lvkComponents;

const
  DEFAULT_AUTO_TERMINATE_SECONDARY  = True;
  DEFAULT_SINGLE_INSTANCE           = False;
  DEFAULT_OPTION_CHARS              = '/-';
  DEFAULT_AUTO_RESTORE              = True;

type
  { Description:
      This event handler type is used for the OnOption event of the
      TlvkParameters component.
  }
  TOptionEvent = procedure(Sender: TObject; const ParameterIndex: Integer;
    const ParameterString, Option: string) of object;

  { Description:
      This event handler type is used for the OnParameter event of the
      TlvkParameters component.
  }
  TParameterEvent = procedure(Sender: TObject; const ParameterIndex: Integer;
    const ParameterString: string) of object;

  { Description:
      This component handles extracting the parameters from the command line
      and calling the events for each parameter/option.
  }
  TlvkParameters = class(TlvkComponent)
  private
    FOnOption               : TOptionEvent;
    FOnParameter            : TParameterEvent;
    FOnPrimaryInstance      : TNotifyEvent;
    FOnSecondaryInstance    : TNotifyEvent;
    FOnSecondaryExecuted    : TNotifyEvent;
    FSingleInstance         : Boolean;
    FAutoTerminateSecondary : Boolean;
    FOptionChars            : string;
    FHandled                : Boolean;

    FAutoRestore            : Boolean;
    FHandle                 : THandle;
    FOwnsMemoryMap          : Boolean;
    FFileMapping            : THandle;
    FMapView                : PChar;

  protected
    procedure DoOption(const ParameterIndex: Integer; const ParameterString,
      Option: string); virtual;
    procedure DoParameter(const ParameterIndex: Integer;
      const ParameterString: string); virtual;
    procedure DoPrimaryInstance; virtual;
    procedure DoSecondaryInstance; virtual;
    procedure DoSecondaryExecuted; virtual;

    procedure HandleSingleParameter(const Index: Integer;
      const Parameter: string); virtual;
    procedure HandleParameters; virtual;
    procedure SendParameters; virtual;

    function GetMapName: string; virtual;
    procedure OpenMemoryMap; virtual;
    procedure CloseMemoryMap; virtual;

    procedure PrepareWndProc; virtual;
    procedure WndProc(var Message: TMessage); virtual;
    procedure UnprepareWndProc; virtual;

    function IsNotDefaultOptionChars: Boolean; virtual;

    procedure Loaded; override;
    procedure Execute;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        This property controls what characters that start an option. By default
        the characters - and / are used.
    }
    property OptionChars: string read FOptionChars write FOptionChars
      stored IsNotDefaultOptionChars;

    { Description:
        Set this property to True in order to auto-terminate any secondary
        instances of your program. This parameter will only get used if you
        also set the SingleInstance property to True.
    }
    property AutoTerminateSecondary: Boolean read FAutoTerminateSecondary
      write FAutoTerminateSecondary default DEFAULT_AUTO_TERMINATE_SECONDARY;

    { Description:
        Set this property to True in order to have only one instance of your
        program. In addition to closing down the second instance, any parameters
        given to the second instance will be passed on to the first.
    }
    property SingleInstance: Boolean read FSingleInstance
      write FSingleInstance default DEFAULT_SINGLE_INSTANCE;

    { Description:
        This event handler gets called once for each option listed on the
        command line.
    }
    property OnOption: TOptionEvent read FOnOption write FOnOption;

    { Description:
        This event handler gets called once for each parameter listed on the
        command line.
    }
    property OnParameter: TParameterEvent read FOnParameter write FOnParameter;

    { Description:
        This event handler gets called when the component determines that this
        is the first, single, instance.
    }
    property OnPrimaryInstance: TNotifyEvent read FOnPrimaryInstance
      write FOnPrimaryInstance;

    { Description:
        This event handler gets called before the parameters are passed on to
        the first, single, instance.
    }
    property OnSecondaryInstance: TNotifyEvent read FOnSecondaryInstance
      write FOnSecondaryInstance;

    { Description:
        This event handler gets called in the primary instance, whenever a
        secondary instance starts, and just before the parameters from that
        secondary instance (if any) starts trickling in.
    }
    property OnSecondaryExecuted: TNotifyEvent read FOnSecondaryExecuted
      write FOnSecondaryExecuted;

    { Description:
        If this property is True, whenever a secondary instance is executed,
        and the component is set to Single Instance, then the primary
        instance is restored. This means that executing the program again
        will bring it to the front of active applications, if this property
        is True. Default is True.
    }
    property AutoRestore: Boolean read FAutoRestore write FAutoRestore
      default DEFAULT_AUTO_RESTORE;
  end;

implementation

uses
  Forms;

const
  CM_EXECUTE    = WM_USER + 1;
  CM_SECONDARY  = WM_USER + 2;

{ TlvkParameters }

procedure TlvkParameters.CloseMemoryMap;
begin
  if FMapView <> nil then
  begin
    UnmapViewOfFile(FMapView);
    FMapView := nil;
  end;

  if FFileMapping <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FFileMapping);
    FFileMapping := INVALID_HANDLE_VALUE;
  end;
end;

constructor TlvkParameters.Create(AOwner: TComponent);
begin
  inherited;

  FSingleInstance := DEFAULT_SINGLE_INSTANCE;
  FAutoTerminateSecondary := DEFAULT_AUTO_TERMINATE_SECONDARY;
  FOptionChars := DEFAULT_OPTION_CHARS;
  FAutoRestore := DEFAULT_AUTO_RESTORE;

  if not (csDesigning in ComponentState) then
  begin
    PrepareWndProc;
    OpenMemoryMap;

    if not (csLoading in ComponentState) then
    begin
      if not FOwnsMemoryMap then
        Application.ShowMainForm := False;
      PostMessage(FHandle, CM_EXECUTE, 0, 0);
    end;
  end;
end;

destructor TlvkParameters.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    CloseMemoryMap;
    UnprepareWndProc;
  end;

  inherited;
end;

procedure TlvkParameters.DoOption(const ParameterIndex: Integer;
  const ParameterString, Option: string);
begin
  if Assigned(FOnOption) then
    FOnOption(Self, ParameterIndex, ParameterString, Option)
  else
    DoParameter(ParameterIndex, ParameterString);
end;

procedure TlvkParameters.DoParameter(const ParameterIndex: Integer;
  const ParameterString: string);
begin
  if Assigned(FOnParameter) then
    FOnParameter(Self, ParameterIndex, ParameterString);
end;

procedure TlvkParameters.DoPrimaryInstance;
begin
  if Assigned(FOnPrimaryInstance) then
    FOnPrimaryInstance(Self);
end;

procedure TlvkParameters.DoSecondaryExecuted;
begin
  if Assigned(FOnSecondaryExecuted) then
    FOnSecondaryExecuted(Self);
end;

procedure TlvkParameters.DoSecondaryInstance;
begin
  if Assigned(FOnSecondaryInstance) then
    FOnSecondaryInstance(Self);
end;

procedure TlvkParameters.Execute;
begin
  if not FHandled then
  begin
    FHandled := True;
    if FSingleInstance then
    begin
      if FOwnsMemoryMap then
      begin
        DoPrimaryInstance;
        HandleParameters;
      end else begin
        DoSecondaryInstance;
        SendParameters;

        if FAutoTerminateSecondary then
        begin
          Application.ShowMainForm := False;
          Application.Terminate;
        end else
          Application.ShowMainForm := True;
      end;
    end else begin
      DoPrimaryInstance;
      HandleParameters;
      Application.ShowMainForm := True;
    end;
  end;
end;

function TlvkParameters.GetMapName: string;
var
  Index : Integer;
begin
  Result := ParamStr(0);

  for Index := 1 to Length(Result) do
    if Result[Index] in [' ', ':', '\'] then
      Result[Index] := '_';
end;

procedure TlvkParameters.HandleParameters;
var
  Temp  : string;
  Index : Integer;
begin
  for Index := 1 to ParamCount do
  begin
    Temp := ParamStr(Index);
    HandleSingleParameter(Index, Temp);
  end;
end;

procedure TlvkParameters.HandleSingleParameter(const Index: Integer;
  const Parameter: string);
begin
  if Pos(Parameter[1], FOptionChars) > 0 then
    DoOption(Index, Parameter, Copy(Parameter, 2, Length(Parameter)-1))
  else
    DoParameter(Index, Parameter);
end;

function TlvkParameters.IsNotDefaultOptionChars: Boolean;
begin
  Result := (FOptionChars <> DEFAULT_OPTION_CHARS);
end;

procedure TlvkParameters.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) and FOwnsMemoryMap then
    Execute;
end;

procedure TlvkParameters.OpenMemoryMap;
begin
  FFileMapping := INVALID_HANDLE_VALUE;
  FMapView := nil;

  FFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
    0, SizeOf(THandle), PChar(GetMapName));
  if FFileMapping = 0 then
    RaiseLastWin32Error;

  FOwnsMemoryMap := (GetLastError <> ERROR_ALREADY_EXISTS);

  FMapView := MapViewOfFile(FFileMapping, FILE_MAP_ALL_ACCESS, 0, 0,
    SizeOf(THandle));
  if not Assigned(FMapView) then
    RaiseLastWin32Error;

  if FOwnsMemoryMap then
    Move(FHandle, FMapView^, SizeOf(THandle));
end;

procedure TlvkParameters.PrepareWndProc;
begin
  FHandle := AllocateHWnd(WndProc);
end;

procedure TlvkParameters.SendParameters;
var
  DestHandle  : THandle;
  Index       : Integer;
  Temp        : string;
  cds         : TCopyDataStruct;
begin
  Move(FMapView^, DestHandle, SizeOf(DestHandle));

  SendMessage(DestHandle, CM_SECONDARY, 0, 0);
  for Index := 1 to ParamCount do
  begin
    Temp := ParamStr(Index);
    cds.dwData := Index;
    cds.cbData := Length(Temp);
    cds.lpData := PChar(Temp);

    SendMessage(DestHandle, WM_COPYDATA, FHandle, Integer(@cds));
  end;
end;

procedure TlvkParameters.UnprepareWndProc;
begin
  DeallocateHWnd(FHandle);
end;

procedure TlvkParameters.WndProc(var Message: TMessage);
var
  Temp  : string;
  cds   : PCopyDataStruct;
begin
  if Message.Msg = CM_SECONDARY then
  begin
    DoSecondaryExecuted;
    if FAutoRestore then
    begin
      ShowWindow(Application.Handle, SW_NORMAL);
      SetForegroundWindow(Application.Handle);
    end;
  end else if Message.Msg = WM_COPYDATA then
  begin
    cds := PCopyDataStruct(Message.LParam);
    SetLength(Temp, cds^.cbData);
    if Length(Temp) > 0 then
      Move(cds^.lpData^, Temp[1], cds^.cbData);

    HandleSingleParameter(cds^.cbData, Temp);
  end else if Message.Msg = CM_EXECUTE then
  begin
    Execute;
    if Application.ShowMainForm and Assigned(Application.MainForm) then
      Application.MainForm.Show;
  end else
    DefaultHandler(Message);
end;

end.
