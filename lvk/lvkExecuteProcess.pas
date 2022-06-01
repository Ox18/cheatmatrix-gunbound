{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code for executing a program and capturing output from
    it (the console output that is). You will also be able to wait for the
    process to finish before continuing your own program.
  See also:
    IExecuteProcess, NewExecuteProcess
}
unit lvkExecuteProcess;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkExecuteProcess.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows;

type
  { Description:
      This interface wraps an object that is responsible for creating a separate
      process from a executable program file, and monitoring it as it executes.

      You can use it to check if the program is still running, the exit code
      it returned when it exited, and read console output from the program.
    See also:
      NewExecuteProcess
  }
  IExecuteProcess = interface
    ['{139FB7CD-644E-49B1-A63F-7EF85F96AF4A}']

    // <COMBINE ProcessFileName>
    function GetProcessFileName: string;
    // <COMBINE ProcessFileName>
    procedure SetProcessFileName(const Value: string);
    { Description:
        This property controls the name of the executable file to execute.

        See the Parameters property for more information.
      See also:
        Parameters, WorkingDirectory
    }
    property ProcessFileName: string read GetProcessFileName
      write SetProcessFileName;

    // <COMBINE Parameters>
    function GetParameters: string;
    // <COMBINE Parameters>
    procedure SetParameters(const Value: string);
    { Description:
        This property controls what parameters to pass to the program. You can
        optionally put the name of the program here and leave the
        ProcessFileName property blank if you wish to avoid having to search
        for the program.

        Putting the name of the program as the start of the parameters list
        will search the standard path and file locations for it.
      See also:
        ProcessFileName, WorkingDirectory
    }
    property Parameters: string read GetParameters write SetParameters;

    // <COMBINE WorkingDirectory>
    function GetWorkingDirectory: string;
    // <COMBINE WorkingDirectory>
    procedure SetWorkingDirectory(const Value: string);
    { Description:
        This property controls which directory is to be the "current directory"
        for the program when it starts executing. If you wish to use the default
        working directory for the application, leave it blank.
      See also:
        ProcessFileName, Parameters
    }
    property WorkingDirectory: string read GetWorkingDirectory
      write SetWorkingDirectory;

    // <COMBINE EnvironmentString>
    function GetEnvironmentString: string;
    // <COMBINE EnvironmentString>
    procedure SetEnvironmentString(const Value: string);
    { Description:
        You can optionally provide an environmentstring containing environment
        variables for the program to use while executing. If you leave this
        blank, the default environment block will be used instead.

        The format of the environment string is a set of variables on the form

          NAME=VALUE

        separated by Carriage Return + LineFeed (#13#10).
      See also:
        Environment
    }
    property EnvironmentString: string read GetEnvironmentString
      write SetEnvironmentString;

    // <COMBINE Environment>
    function GetEnvironment: TStrings;
    { Description:
        This property is a different way of accessing the environment
        data to provide to the program to run, as a TStrings object,
        each item in the list is one variable.
      See also:
        EnvironmentString
    }
    property Environment: TStrings read GetEnvironment;

    // <COMBINE CaptureOutput>
    function GetCaptureOutput: Boolean;
    // <COMBINE CaptureOutput>
    procedure SetCaptureOutput(const Value: Boolean);
    { Description:
        This property controls wether the class should capture the output
        from the program as it runs. Leave it as False to just ignore any
        output the program might make on the console, set it to True to capture
        this output.

        You can use the ReadOutput method to access the captured output.
      See also:
        ReadOutput
    }
    property CaptureOutput: Boolean read GetCaptureOutput
      write SetCaptureOutput;

    // <COMBINE ShowWindow>
    function GetShowWindow: Word;
    // <COMBINE ShowWindow>
    procedure SetShowWindow(const Value: Word);
    { Description:
      This property controls how to show the program (or not). Valid values for
      the property are:
        SW_HIDE
        SW_NORMAL
        SW_NORMAL
        SW_SHOWMINIMIZED
        SW_SHOWMAXIMIZED
        SW_MAXIMIZE
        SW_SHOWNOACTIVATE
        SW_SHOW
        SW_MINIMIZE
        SW_SHOWMINNOACTIVE
        SW_SHOWNA
        SW_RESTORE
        SW_SHOWDEFAULT
        SW_MAX

      Look up the ShowWindow function in the windows api help for more
      information about these values.
    }
    property ShowWindow: Word read GetShowWindow write SetShowWindow;

    { Description:
        This method starts the program running. An exception will be raised
        if something goes wrong.

        Note: After the object has started the program, the object will maintain
          "ownership" of the program. If you destroy the object, the program
          will be terminated. Look at the Release method for a way to avoid
          this.
      See also:
        Kill, Release
    }
    procedure Start;

    { Description:
        This method will terminate the program, no questions asked.
      See also:
        Start, Release
    }
    procedure Kill;

    { Description:
        When you destroy the object, if the program is still running it will
        be killed. If you want to avoid this, you can call the Release method.

        The Release method will stop the object from monitoring the program
        and capturing the output from it, and the program will continue running
        after the object is destroyed.
      See also:
        Start, Kill
    }
    procedure Release;

    { Description:
        This method will wait for the program to complete. If the program
        terminated within the timeout given, the function returns True,
        otherwise False.
    }
    function WaitFor(const Timeout: Cardinal=INFINITE): Boolean;

    // <COMBINE IsExecuting>
    function GetIsExecuting: Boolean;
    { Description:
        This property returns True if the program is still executing.
    }
    property IsExecuting: Boolean read GetIsExecuting;

    // <COMBINE ExitCode>
    function GetExitCode: Cardinal;
    { Description:
        Once the program has finished executing, this property will return the
        exitcode it returned when it exited.
    }
    property ExitCode: Cardinal read GetExitCode;

    { Description:
        Use this method to read output from the monitored program.

        Note: This method is a blocking method. This means that if you call it,
          and the monitored program has not produced any output yet, your
          program will stop and wait for the monitored program to output data,
          or exit, whatever comes first. This means that for optimal usage of
          this class, execute it in a thread so that the main thread is still
          responsive to the user.
      Parameters:
        Buffer - The buffer to store the data into.
        BufferSize - The size of the buffer provided.
        AmountRead - The number of bytes actually read into the buffer.
      Returns:
        True if data was read, False if not (either monitored program is not
          having its output captured, or the program terminated with no
          output).
    }
    function ReadOutput(var Buffer; const BufferSize: LongWord;
      out AmountRead: LongWord): Boolean;
  end;

  // This exception class is used for exceptions raised from the IExecuteProcess
  // wrapped object
  EExecuteProcess = class(Exception);

{ Description:
    This function will return an IExecuteProcess interface for the program to
    monitor.
  Parameters:
    ProcessFileName - The path and filename of the executable file to load and
      execute.
    Parameters - The parameters to pass to it.
    WorkingDirectory - The directory to use as the current directory for the
      program.
    ShowWindow - How to show the program (or not). Valid values are
      SW_HIDE, SW_NORMAL, SW_NORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED,
      SW_MAXIMIZE, SW_SHOWNOACTIVATE, SW_SHOW, SW_MINIMIZE, SW_SHOWMINNOACTIVE,
      SW_SHOWNA, SW_RESTORE, SW_SHOWDEFAULT, SW_MAX. Look up the ShowWindow
      function in the windows api help for more information about these values.
  See also:
    IExecuteProcess.ProcessFileName, IExecuteProcess.Parameters,
    IExecuteProcess.WorkingDirectory
}
function NewExecuteProcess(const ProcessFileName: string;
  const Parameters: string='';
  const WorkingDirectory: string='';
  const ShowWindow: Word=SW_NORMAL): IExecuteProcess;

resourcestring
  ERR_UNABLE_TO_CREATE_PROCESS  = 'Unable to create process, error #%0:d';

implementation

type
  TParameterArray = array of string;

  TExecuteProcess = class(TInterfacedObject, IExecuteProcess)
  private
    FProcessFileName  : string;
    FParameters       : string;
    FEnvironment      : TStrings;
    FWorkingDirectory : string;
    FCaptureOutput    : Boolean;

    (* Not in use ??
    FErrorCode        : Integer;
    FErrorText        : string;
    *)
    FExitCode         : Cardinal;
    FOutputHandle     : THandle;
    FShowWindow       : Word;

    FProcessHandle    : THandle;

    function PrepareEnvironment: string;
    procedure CheckStillRunning;

  protected
    // IExecuteProcess interface
    function GetEnvironmentString: string;
    function GetProcessFileName: string;
    function GetWorkingDirectory: string;
    procedure SetEnvironmentString(const Value: string);
    procedure SetProcessFileName(const Value: string);
    procedure SetWorkingDirectory(const Value: string);
    function GetParameters: string;
    procedure SetParameters(const Value: string);
    property Parameters: string read GetParameters write SetParameters;
    procedure Start;
    procedure Kill;
    procedure Release;
    function WaitFor(const Timeout: Cardinal): Boolean;
    function GetIsExecuting: Boolean;
    function GetExitCode: Cardinal;
    (* Not in use ?
    function GetErrorCode: Integer;
    function GetErrorText: string;
    *)
    function GetCaptureOutput: Boolean;
    procedure SetCaptureOutput(const Value: Boolean);
    function ReadOutput(var Buffer; const BufferSize: LongWord;
      out AmountRead: LongWord): Boolean;
    function GetEnvironment: TStrings;
    function GetShowWindow: Word;
    procedure SetShowWindow(const Value: Word);

  public
    constructor Create(const ProcessFileName, Parameters,
      WorkingDirectory: string; const ShowWindow: Word); overload;
    destructor Destroy; override;
  end;

function NewExecuteProcess(const ProcessFileName, Parameters,
  WorkingDirectory: string; const ShowWindow: Word): IExecuteProcess;
begin
  Result := TExecuteProcess.Create(ProcessFileName, Parameters,
    WorkingDirectory, ShowWindow);
end;

function PCharOf(var s: string): PChar;
begin
  if s = '' then
    Result := nil
  else
    Result := PChar(s);
end;

procedure FinalizeHandle(var Handle: THandle);
begin
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(Handle);
    Handle := INVALID_HANDLE_VALUE;
  end;
end;

{ TExecuteProcess }

constructor TExecuteProcess.Create(const ProcessFileName, Parameters,
  WorkingDirectory: string; const ShowWindow: Word);
begin
  inherited Create;

  FProcessFileName := ProcessFileName;
  FWorkingDirectory := WorkingDirectory;
  FEnvironment := TStringList.Create;
  FProcessHandle := INVALID_HANDLE_VALUE;
  FOutputHandle := INVALID_HANDLE_VALUE;
  FParameters := Parameters;
  FShowWindow := ShowWindow;
end;

destructor TExecuteProcess.Destroy;
begin
  if GetIsExecuting then
    Kill;
  FEnvironment.Free;

  inherited;
end;

function TExecuteProcess.GetEnvironmentString: string;
begin
  Result := FEnvironment.Text;
end;

(* Not in use ??
function TExecuteProcess.GetErrorCode: Integer;
begin
  Result := FErrorCode;
end;

function TExecuteProcess.GetErrorText: string;
begin
  Result := FErrorText;
end;
*)

function TExecuteProcess.GetIsExecuting: Boolean;
begin
  CheckStillRunning;
  Result := FProcessHandle <> INVALID_HANDLE_VALUE;
end;

function TExecuteProcess.GetParameters: string;
begin
  Result := FParameters;
end;

function TExecuteProcess.GetProcessFileName: string;
begin
  Result := FProcessFileName;
end;

function TExecuteProcess.GetExitCode: Cardinal;
begin
  Result := FExitCode;
end;

function TExecuteProcess.GetWorkingDirectory: string;
begin
  Result := FWorkingDirectory;
end;

function TExecuteProcess.PrepareEnvironment: string;
begin
  Result := '';
end;

procedure TExecuteProcess.SetEnvironmentString(const Value: string);
begin
  if GetIsExecuting then
    raise EExecuteProcess.Create('Cannot change the environment variables of the program while it is executing');
  FEnvironment.Text := Value;
end;

procedure TExecuteProcess.SetParameters(const Value: string);
begin
  if GetIsExecuting then
    raise EExecuteProcess.Create('Cannot change the parameters for the program to execute while it is executing');
  FParameters := Value;
end;

procedure TExecuteProcess.SetProcessFileName(const Value: string);
begin
  if GetIsExecuting then
    raise EExecuteProcess.Create('Cannot change the name of the program file to execute while it is executing');
  FProcessFileName := Value;
end;

procedure TExecuteProcess.SetWorkingDirectory(const Value: string);
begin
  if GetIsExecuting then
    raise EExecuteProcess.Create('Cannot change the working directory of the program while it is executing');
  FWorkingDirectory := Value;
end;

procedure TExecuteProcess.Start;
var
  SecurityAttributes  : TSecurityAttributes;
  StartupInfo         : TStartupInfo;
  ProcessInformation  : TProcessInformation;
  hOutputReadTmp      : THandle;
  hOutputWrite        : THandle;
  hInputWriteTmp      : THandle;
  hInputRead          : THandle;
  hInputWrite         : THandle;
  hErrorWrite         : THandle;
  Environment         : string;
begin
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := FShowWindow;

  ZeroMemory(@ProcessInformation, SizeOf(ProcessInformation));
  ProcessInformation.hThread := INVALID_HANDLE_VALUE;
  ProcessInformation.hProcess := INVALID_HANDLE_VALUE;

  hInputWriteTmp := INVALID_HANDLE_VALUE;
  hInputWrite := INVALID_HANDLE_VALUE;
  hOutputReadTmp := INVALID_HANDLE_VALUE;
  hOutputWrite := INVALID_HANDLE_VALUE;
  hErrorWrite := INVALID_HANDLE_VALUE;
  hInputRead := INVALID_HANDLE_VALUE;

  Environment := PrepareEnvironment;

  try
    if FCaptureOutput then
    begin
      Win32Check(CreatePipe(hOutputReadTmp, hOutputWrite, @SecurityAttributes,
        0));
      Win32Check(DuplicateHandle(GetCurrentProcess, hOutputWrite,
        GetCurrentProcess, @hErrorWrite, 0, True, DUPLICATE_SAME_ACCESS));

      Win32Check(CreatePipe(hInputRead, hInputWriteTmp, @SecurityAttributes,
        0));

      Win32Check(DuplicateHandle(GetCurrentProcess, hOutputReadTmp,
        GetCurrentProcess, @FOutputHandle, 0, False, DUPLICATE_SAME_ACCESS));
      Win32Check(DuplicateHandle(GetCurrentProcess, hInputWriteTmp,
        GetCurrentProcess, @hInputWrite, 0, False, DUPLICATE_SAME_ACCESS));

      FinalizeHandle(hOutputReadTmp);
      FinalizeHandle(hInputWriteTmp);

      StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
      StartupInfo.hStdInput := hInputRead;
      StartupInfo.hStdOutput := hOutputWrite;
      StartupInfo.hStdError := hErrorWrite;
    end;

    if not CreateProcess(PCharOf(FProcessFileName), PChar(FParameters),
      @SecurityAttributes, @SecurityAttributes, True, 0, PCharOf(Environment),
      PCharOf(FWorkingDirectory), StartupInfo, ProcessInformation) then
    begin
      RaiseLastWin32Error;
    end;

    FProcessHandle := ProcessInformation.hProcess;
  finally
    FinalizeHandle(hOutputWrite);
    FinalizeHandle(hErrorWrite);
    FinalizeHandle(hInputRead);
    FinalizeHandle(ProcessInformation.hThread);
    FinalizeHandle(hOutputReadTmp);
    FinalizeHandle(hOutputWrite);
  end;
end;

function TExecuteProcess.GetCaptureOutput: Boolean;
begin
  Result := FCaptureOutput;
end;

procedure TExecuteProcess.SetCaptureOutput(const Value: Boolean);
begin
  FCaptureOutput := Value;
end;

procedure TExecuteProcess.CheckStillRunning;
var
  rc  : Integer;
begin
  if FProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    rc := WaitForSingleObject(FProcessHandle, 0);
    if rc = WAIT_OBJECT_0 then
    begin
      GetExitCodeProcess(FProcessHandle, FExitCode);
      (* Not in use ?
      FErrorCode := 0;
      FErrorText := '';
      *)
      FinalizeHandle(FProcessHandle);
      FinalizeHandle(FOutputHandle);
    end else if rc <> WAIT_TIMEOUT then
      RaiseLastWin32Error;
  end;
end;

procedure TExecuteProcess.Kill;
begin
  CheckStillRunning;
  if FProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    TerminateProcess(FProcessHandle, 1);
    GetExitCodeProcess(FProcessHandle, FExitCode);
    FinalizeHandle(FProcessHandle);
    FinalizeHandle(FOutputHandle);
  end;
end;

function TExecuteProcess.ReadOutput(var Buffer; const BufferSize: LongWord;
  out AmountRead: LongWord): Boolean;
begin
  if FOutputHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := ReadFile(FOutputHandle, Buffer, BufferSize, AmountRead, nil);
    if not Result then
      if GetLastError <> ERROR_BROKEN_PIPE then
        RaiseLastWin32Error;
  end else
    Result := False;
    
  CheckStillRunning;
end;

procedure TExecuteProcess.Release;
begin
  CheckStillRunning;
  FinalizeHandle(FProcessHandle);
  FinalizeHandle(FOutputHandle);
end;

function TExecuteProcess.GetEnvironment: TStrings;
begin
  Result := FEnvironment;
end;

function TExecuteProcess.GetShowWindow: Word;
begin
  Result := FShowWindow;
end;

procedure TExecuteProcess.SetShowWindow(const Value: Word);
begin
  if GetIsExecuting then
    raise EExecuteProcess.Create('Cannot change the window mode of the program while it is executing');

  FShowWindow := Value;
end;

function TExecuteProcess.WaitFor(const Timeout: Cardinal): Boolean;
begin
  CheckStillRunning;
  if FProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    WaitForSingleObject(FProcessHandle, Timeout);
    Result := not GetIsExecuting;
  end else
    Result := False;
end;

end.
