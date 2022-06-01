{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains RS232 serial communications code.
}
unit lvkSerial;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSerial.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils;
  
type
  TComPort = (cpCOM1, cpCOM2, cpCOM3, cpCOM4, cpCOM5, cpCOM6, cpCOM7, cpCOM8,
    cpCOM9, cpCOM10, cpCOM11, cpCOM12, cpCOM13, cpCOM14, cpCOM15, cpCOM16,
    cpCOM17, cpCOM18, cpCOM19, cpCOM20, cpCOM21, cpCOM22, cpCOM23, cpCOM24,
    cpCOM25, cpCOM26, cpCOM27, cpCOM28, cpCOM29, cpCOM30, cpCOM31, cpCOM32);
  TStopBits = (sb1, sb1_5, sb2);
  TDataBits = (db5, db6, db7, db8);
  TParityBits = (pbNone, pbOdd, pbEven, pbMark, pbSpace);
  TDTRFlowControl = (dtrDisable, dtrEnable, dtrHandshake);
  TRTSFlowControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);
  TTrueMeans = (tmCompleted, tmPartial);

  ISerial = interface
    ['{9DE30FAE-DE94-4B48-A96F-E5C00AE46B1F}']

    function GetComPort: TComPort;
    procedure SetComPort(const Value: TComPort);
    property ComPort: TComPort read GetComPort write SetComPort;

    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    function GetStopBits: TStopBits;
    procedure SetStopBits(const Value: TStopBits);
    property StopBits: TStopBits read GetStopBits write SetStopBits;

    function GetDataBits: TDataBits;
    procedure SetDataBits(const Value: TDataBits);
    property DataBits: TDataBits read GetDataBits write SetDataBits;

    function GetParityBits: TParityBits;
    procedure SetParityBits(const Value: TParityBits);
    property ParityBits: TParityBits read GetParityBits write SetParityBits;

    procedure BeginConfigureBuffers;
    procedure EndConfigureBuffers;

    function GetInputBufferSize: Cardinal;
    procedure SetInputBufferSize(const Value: Cardinal);
    property InputBufferSize: Cardinal read GetInputBufferSize
      write SetInputBufferSize;

    function GetOutputBufferSize: Cardinal;
    procedure SetOutputBufferSize(const Value: Cardinal);
    property OutputBufferSize: Cardinal read GetOutputBufferSize
      write SetOutputBufferSize;

    procedure SetBufferSizes(const InputBufferSize, OutputBufferSize: Cardinal);

    procedure BeginConfigureTimeout;
    procedure EndConfigureTimeout;
    procedure SetTimeouts(const ReadIntervalTimeout, ReadTotalTimeoutMultiplier,
      ReadTotalTimeoutConstant, WriteTotalTimeoutMultiplier,
      WriteTotalTimeoutConstant: Integer);
    function GetReadIntervalTimeout: Integer;
    procedure SetReadIntervalTimeout(const Value: Integer);
    property ReadIntervalTimeout: Integer read GetReadIntervalTimeout
      write SetReadIntervalTimeout;

    function GetReadTotalTimeoutMultiplier: Integer;
    procedure SetReadTotalTimeoutMultiplier(const Value: Integer);
    property ReadTotalTimeoutMultiplier: Integer read GetReadTotalTimeoutMultiplier
      write SetReadTotalTimeoutMultiplier;

    function GetReadTotalTimeoutConstant: Integer;
    procedure SetReadTotalTimeoutConstant(const Value: Integer);
    property ReadTotalTimeoutConstant: Integer read GetReadTotalTimeoutConstant
      write SetReadTotalTimeoutConstant;

    function GetWriteTotalTimeoutMultiplier: Integer;
    procedure SetWriteTotalTimeoutMultiplier(const Value: Integer);
    property WriteTotalTimeoutMultiplier: Integer read GetWriteTotalTimeoutMultiplier
      write SetWriteTotalTimeoutMultiplier;

    function GetWriteTotalTimeoutConstant: Integer;
    procedure SetWriteTotalTimeoutConstant(const Value: Integer);
    property WriteTotalTimeoutConstant: Integer read GetWriteTotalTimeoutConstant
      write SetWriteTotalTimeoutConstant;

    function ReadChar(out c: Char; const TerminationEvent: THandle=0): Boolean;
    function WriteChar(const c: Char; const TerminationEvent: THandle=0): Boolean;
    function ReadString(out s: string; const Length: Integer;
      const TrueMeans: TTrueMeans=tmCompleted;
      const TerminationEvent: THandle=0): Boolean;
    function WriteString(const s: string; out AmountWritten: Integer;
      const TrueMeans: TTrueMeans=tmCompleted; 
      const TerminationEvent: THandle=0): Boolean;
    function Read(var Buffer; const Count: Integer; out AmountRead: Integer;
      const TrueMeans: TTrueMeans=tmCompleted;
      const TerminationEvent: THandle=0): Boolean;
    function Write(const Buffer; const Count: Integer;
      out AmountWritten: Integer; const TrueMeans: TTrueMeans=tmCompleted;
      const TerminationEvent: THandle=0): Boolean;

    function GetCheckParity: Boolean;
    procedure SetCheckParity(const Value: Boolean);
    property CheckParity: Boolean read GetCheckParity write SetCheckParity;

    function GetReplaceParity: Boolean;
    procedure SetReplaceParity(const Value: Boolean);
    property ReplaceParity: Boolean read GetReplaceParity
      write SetReplaceParity;

    function GetReplaceParityChar: Char;
    procedure SetReplaceParityChar(const Value: Char);
    property ReplaceParityChar: Char read GetReplaceParityChar
      write SetReplaceParityChar;

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    procedure Enable;
    procedure Disable;

    function GetXonChar: Char;
    procedure SetXonChar(const Value: Char);
    property XonChar: Char read GetXonChar write SetXonChar;

    function GetXoffChar: Char;
    procedure SetXoffChar(const Value: Char);
    property XoffChar: Char read GetXoffChar write SetXoffChar;

    function GetInXonXoff: Boolean;
    procedure SetInXonXoff(const Value: Boolean);
    property InXonXoff: Boolean read GetInXonXoff write SetInXonXoff;

    function GetOutXonXoff: Boolean;
    procedure SetOutXonXoff(const Value: Boolean);
    property OutXonXoff: Boolean read GetOutXonXoff write SetOutXonXoff;

    function GetOutCTS: Boolean;
    procedure SetOutCTS(const Value: Boolean);
    property OutCTS: Boolean read GetOutCTS write SetOutCTS;

    function GetOutDSR: Boolean;
    procedure SetOutDSR(const Value: Boolean);
    property OutDSR: Boolean read GetOutDSR write SetOutDSR;

    function GetRTSFlowControl: TRTSFlowControl;
    procedure SetRTSFlowControl(const Value: TRTSFlowControl);
    property RTSFlowControl: TRTSFlowControl read GetRTSFlowControl
      write SetRTSFlowControl;

    function GetDTRFlowControl: TDTRFlowControl;
    procedure SetDTRFlowControl(const Value: TDTRFlowControl);
    property DTRFlowControl: TDTRFlowControl read GetDTRFlowControl
      write SetDTRFlowControl;

    function GetTxContinueOnXoff: Boolean;
    procedure SetTxContinueOnXoff(const Value: Boolean);
    property TxContinueOnXoff: Boolean read GetTxContinueOnXoff
      write SetTxContinueOnXoff;

    function GetDSRSensitivity: Boolean;
    procedure SetDSRSensitivity(const Value: Boolean);
    property DSRSensitivity: Boolean read GetDSRSensitivity
      write SetDSRSensitivity;

    function GetEventChar: Char;
    procedure SetEventChar(const Value: Char);
    property EventChar: Char read GetEventChar write SetEventChar;
  end;

  ESerial = class(Exception);

function NewSerial(const ComPort: TComPort;
  const BaudRate: Integer=9600;
  const ParityBits: TParityBits=pbNone;
  const DataBits: TDataBits=db8;
  const StopBits: TStopBits=sb1;
  const InputBufferSize: Cardinal=32768;
  const OutputBufferSize: Cardinal=32768;
  const Enable: Boolean=True): ISerial;

implementation

uses
  Windows;

const
  DCB_FLAG_Binary           = $00000001;
  DCB_FLAG_Parity           = $00000002;

  DCB_FLAG_OutxCtsFlow      = $00000004;
  DCB_FLAG_OutxDsrFlow      = $00000008;
  DCB_FLAG_DtrControl       = $00000030;
  DCB_FLAG_DsrSensivity     = $00000040;
  DCB_FLAG_TXContinueOnXoff = $00000080;
  DCB_FLAG_OutX             = $00000100;
  DCB_FLAG_InX              = $00000200;
  DCB_FLAG_ErrorChar        = $00000400;
  DCB_FLAG_Null             = $00000800;
  DCB_FLAG_RtsControl       = $00003000;
  DCB_FLAG_AbortOnError     = $00004000;

  DTRValues : array[TDTRFlowControl] of Integer = (
    DTR_CONTROL_DISABLE shl 4, DTR_CONTROL_ENABLE shl 4,
    DTR_CONTROL_HANDSHAKE shl 4);
  RTSValues : array[TRTSFlowControl] of Integer = (
    RTS_CONTROL_DISABLE shl 12, RTS_CONTROL_ENABLE shl 12,
    RTS_CONTROL_HANDSHAKE shl 12, RTS_CONTROL_TOGGLE shl 12);
  ParityBitValues : array[TParityBits] of Integer = (
    NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
  StopBitValues : array[TStopBits] of Integer = (
    ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
  DataBitValues : array[TDataBits] of Integer = (
    5, 6, 7, 8);

type
  TSerial = class(TInterfacedObject, ISerial)
  private
    FComPort                      : TComPort;
    FConfigureDCBLevel            : Integer;
    FBaudRate                     : Integer;
    FStopBits                     : TStopBits;
    FDataBits                     : TDataBits;
    FCheckParity                  : Boolean;
    FParityBits                   : TParityBits;
    FReplaceParity                : Boolean;
    FReplaceParityChar            : Char;

    FInputBufferSize              : Cardinal;
    FOutputBufferSize             : Cardinal;
    FConfigureBuffersLevel        : Integer;

    FReadIntervalTimeout          : Integer;
    FReadTotalTimeoutMultiplier   : Integer;
    FReadTotalTimeoutConstant     : Integer;
    FWriteTotalTimeoutMultiplier  : Integer;
    FWriteTotalTimeoutConstant    : Integer;
    FConfigureTimeoutLevel        : Integer;

    FXonChar                      : Char;
    FXoffChar                     : Char;
    FEventChar                    : Char;
    FInXonXoff                    : Boolean;
    FOutXonXoff                   : Boolean;
    FOutCTS                       : Boolean;
    FOutDSR                       : Boolean;
    FDSRSensitivity               : Boolean;
    FTxContinueOnXoff             : Boolean;
    FDTRFlowControl               : TDTRFlowControl;
    FRTSFlowControl               : TRTSFlowControl;

    FHandle                       : THandle;
    FOverlapped                   : TOverlapped;

    procedure DoEnable;
    procedure DoDisable;

    procedure ConfigureTimeouts;
    procedure ConfigureBuffers;
    procedure ConfigureDeviceControlBlock;

    procedure InitializeOverlapped;
    procedure FinalizeOverlapped;

    procedure StartAsyncRead(var Buffer; const Count: Integer);
    procedure StartAsyncWrite(const Buffer; const Count: Integer);
    function WaitForAsyncToComplete(const TerminationEvent: THandle): Integer;

  protected
    // ISerial interface
    function GetComPort: TComPort;
    procedure SetComPort(const Value: TComPort);
    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure Enable;
    procedure Disable;
    procedure BeginConfigureDeviceControlBlock;
    procedure EndConfigureDeviceControlBlock;
    function GetStopBits: TStopBits;
    procedure SetStopBits(const Value: TStopBits);
    function GetDataBits: TDataBits;
    procedure SetDataBits(const Value: TDataBits);
    function GetParityBits: TParityBits;
    procedure SetParityBits(const Value: TParityBits);
    function GetReadIntervalTimeout: Integer;
    procedure SetReadIntervalTimeout(const Value: Integer);
    function GetReadTotalTimeoutMultiplier: Integer;
    procedure SetReadTotalTimeoutMultiplier(const Value: Integer);
    function GetReadTotalTimeoutConstant: Integer;
    procedure SetReadTotalTimeoutConstant(const Value: Integer);
    function GetWriteTotalTimeoutMultiplier: Integer;
    procedure SetWriteTotalTimeoutMultiplier(const Value: Integer);
    function GetWriteTotalTimeoutConstant: Integer;
    procedure SetWriteTotalTimeoutConstant(const Value: Integer);
    procedure BeginConfigureTimeout;
    procedure EndConfigureTimeout;
    function GetCheckParity: Boolean;
    procedure SetCheckParity(const Value: Boolean);
    function GetReplaceParity: Boolean;
    procedure SetReplaceParity(const Value: Boolean);
    function GetReplaceParityChar: Char;
    procedure SetReplaceParityChar(const Value: Char);
    procedure BeginConfigureBuffers;
    procedure EndConfigureBuffers;
    function GetInputBufferSize: Cardinal;
    procedure SetInputBufferSize(const Value: Cardinal);
    function GetOutputBufferSize: Cardinal;
    procedure SetOutputBufferSize(const Value: Cardinal);
    procedure SetBufferSizes(const InputBufferSize, OutputBufferSize: Cardinal);
    procedure SetTimeouts(const ReadIntervalTimeout, ReadTotalTimeoutMultiplier,
      ReadTotalTimeoutConstant, WriteTotalTimeoutMultiplier,
      WriteTotalTimeoutConstant: Integer);
    function ReadChar(out c: Char; const TerminationEvent: THandle): Boolean;
    function WriteChar(const c: Char; const TerminationEvent: THandle): Boolean;
    function ReadString(out s: string; const Length: Integer;
      const TrueMeans: TTrueMeans; const TerminationEvent: THandle): Boolean;
    function WriteString(const s: string; out AmountWritten: Integer;
      const TrueMeans: TTrueMeans; const TerminationEvent: THandle): Boolean;
    function Read(var Buffer; const Count: Integer; out AmountRead: Integer;
      const TrueMeans: TTrueMeans; const TerminationEvent: THandle): Boolean;
    function Write(const Buffer; const Count: Integer;
      out AmountWritten: Integer; const TrueMeans: TTrueMeans;
      const TerminationEvent: THandle): Boolean;
    function GetXonChar: Char;
    procedure SetXonChar(const Value: Char);
    function GetXoffChar: Char;
    procedure SetXoffChar(const Value: Char);
    function GetInXonXoff: Boolean;
    procedure SetInXonXoff(const Value: Boolean);
    function GetOutXonXoff: Boolean;
    procedure SetOutXonXoff(const Value: Boolean);
    function GetOutCTS: Boolean;
    procedure SetOutCTS(const Value: Boolean);
    function GetOutDSR: Boolean;
    procedure SetOutDSR(const Value: Boolean);
    function GetRTSFlowControl: TRTSFlowControl;
    procedure SetRTSFlowControl(const Value: TRTSFlowControl);
    function GetDTRFlowControl: TDTRFlowControl;
    procedure SetDTRFlowControl(const Value: TDTRFlowControl);
    function GetTxContinueOnXoff: Boolean;
    procedure SetTxContinueOnXoff(const Value: Boolean);
    function GetDSRSensitivity: Boolean;
    procedure SetDSRSensitivity(const Value: Boolean);
    function GetEventChar: Char;
    procedure SetEventChar(const Value: Char);

  public
    constructor Create(const ComPort: TComPort; const BaudRate: Integer;
      const ParityBits: TParityBits; const DataBits: TDataBits;
      const StopBits: TStopBits;
      const InputBufferSize, OutputBufferSize: Cardinal);
    destructor Destroy; override;
  end;

function NewSerial(const ComPort: TComPort; const BaudRate: Integer;
  const ParityBits: TParityBits; const DataBits: TDataBits;
  const StopBits: TStopBits; const InputBufferSize, OutputBufferSize: Cardinal;
  const Enable: Boolean): ISerial;
begin
  Result := TSerial.Create(ComPort, BaudRate, ParityBits, DataBits, StopBits,
    InputBufferSize, OutputBufferSize);
  if Enable then
    Result.Enable;
end;

{ TSerial }

procedure TSerial.BeginConfigureBuffers;
begin
  Inc(FConfigureBuffersLevel);
end;

procedure TSerial.BeginConfigureDeviceControlBlock;
begin
  Inc(FConfigureDCBLevel);
end;

procedure TSerial.BeginConfigureTimeout;
begin
  Inc(FConfigureTimeoutLevel);
end;

procedure TSerial.ConfigureBuffers;
begin
  if not GetEnabled then
    Exit;

  if not SetupComm(FHandle, FInputBufferSize, FOutputBufferSize) then
    RaiseLastWin32Error;
end;

procedure TSerial.ConfigureDeviceControlBlock;
var
  DCB : TDCB;
begin
  if not GetEnabled then
    Exit;

  FillChar(DCB, SizeOf(DCB), #0);
  DCB.DCBlength := SizeOf(DCB);
  DCB.XonLim := FInputBufferSize div 4;
  DCB.XoffLim := DCB.XonLim;
  DCB.EvtChar := FEventChar;
  DCB.Flags := DCB_FLAG_Binary;

  DCB.XonChar := FXonChar;
  DCB.XoffChar := FXoffChar;
  if FOutCTS then
    DCB.Flags := DCB.Flags or DCB_FLAG_OutxCtsFlow;
  if FOutDSR then
    DCB.Flags := DCB.Flags or DCB_FLAG_OutxDsrFlow;
  DCB.Flags := DCB.Flags or DTRValues[FDTRFlowControl] or
    RTSValues[FRTSFlowControl];
  if FInXonXoff then
    DCB.Flags := DCB.Flags or DCB_FLAG_InX;
  if FOutXonXoff then
    DCB.Flags := DCB.Flags or DCB_FLAG_OutX;
  if FDSRSensitivity then
    DCB.Flags := DCB.Flags or DCB_FLAG_DsrSensivity;
  if FTxContinueOnXoff then
    DCB.Flags := DCB.Flags or DCB_FLAG_TXContinueOnXoff;

  DCB.Parity := ParityBitValues[FParityBits];
  DCB.StopBits := StopBitValues[FStopBits];
  DCB.BaudRate := FBaudRate;
  DCB.ByteSize := DataBitValues[FDataBits];
  if FCheckParity then
  begin
    DCB.Flags := DCB.Flags or DCB_FLAG_Parity;
    if FReplaceParity then
    begin
      DCB.Flags := DCB.Flags or DCB_FLAG_ErrorChar;
      DCB.ErrorChar := FReplaceParityChar;
    end;
  end;

  if not SetCommState(FHandle, DCB) then
    RaiseLastWin32Error;
end;

procedure TSerial.ConfigureTimeouts;
var
  Timeouts  : TCommTimeouts;

  function FixupTimeout(const Value: Integer): Cardinal;
  begin
    if Value = -1 then
      Result := MAXDWORD
    else
      Result := Value;  
  end;

begin
  if not GetEnabled then
    Exit;

  Timeouts.ReadIntervalTimeout := FixupTimeout(FReadIntervalTimeout);
  Timeouts.ReadTotalTimeoutMultiplier := FixupTimeout(FReadTotalTimeoutMultiplier);
  Timeouts.ReadTotalTimeoutConstant := FixupTimeout(FReadTotalTimeoutConstant);
  Timeouts.WriteTotalTimeoutMultiplier := FixupTimeout(FWriteTotalTimeoutMultiplier);
  Timeouts.WriteTotalTimeoutConstant := FixupTimeout(FWriteTotalTimeoutConstant);
  if not SetCommTimeouts(FHandle, Timeouts) then
    RaiseLastWin32Error;
end;

constructor TSerial.Create(const ComPort: TComPort;
  const BaudRate: Integer; const ParityBits: TParityBits;
  const DataBits: TDataBits; const StopBits: TStopBits;
  const InputBufferSize, OutputBufferSize: Cardinal);
begin
  inherited Create;

  // Set up for completely blocking send/receive
  FReadIntervalTimeout := 0;
  FReadTotalTimeoutMultiplier := -1;
  FReadTotalTimeoutConstant := -1;
  FWriteTotalTimeoutMultiplier := -1;
  FWriteTotalTimeoutConstant := -1;

  // Configure flow-control for blocking
  FInXonXoff := False;
  FOutXonXoff := False;
  FCheckParity := False;
  FReplaceParity := False;
  FReplaceParityChar := #0;
  FXonChar := #17;
  FXoffChar := #19;
  FEventChar := #0;
  FOutCTS := True;
  FOutDSR := False;
  FDSRSensitivity := False;
  FDTRFlowControl := dtrEnable;
  FRTSFlowControl := rtsDisable;
  FTxContinueOnXoff := True;

  FComPort := ComPort;
  FBaudRate := BaudRate;
  FParityBits := ParityBits;
  FDataBits := DataBits;
  FStopBits := StopBits;
  FInputBufferSize := InputBufferSize;
  FOutputBufferSize := OutputBufferSize;
  FHandle := INVALID_HANDLE_VALUE;
end;

destructor TSerial.Destroy;
begin
  Disable;
  inherited;
end;

procedure TSerial.Disable;
begin
  SetEnabled(False);
end;

procedure TSerial.DoDisable;
begin
  CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
end;

procedure TSerial.DoEnable;
var
  ComPortStr  : string;
begin
  // Create and open serial port
  ComPortStr := Format('\\.\COM%d', [Ord(FComPort)+1]);
  FHandle := CreateFile(PChar(ComPortStr), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING, 0{FILE_FLAG_OVERLAPPED}, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;

  // Configure COM-port
  try
    ConfigureBuffers;
    ConfigureDeviceControlBlock;
    ConfigureTimeouts;
  except
    CloseHandle(FHandle);
    raise;
  end;
end;

procedure TSerial.Enable;
begin
  SetEnabled(True);
end;

procedure TSerial.EndConfigureBuffers;
begin
  if FConfigureBuffersLevel > 0 then
  begin
    Dec(FConfigureBuffersLevel);
    if FConfigureBuffersLevel = 0 then
      ConfigureBuffers;
  end;
end;

procedure TSerial.EndConfigureDeviceControlBlock;
begin
  if FConfigureDCBLevel > 0 then
  begin
    Dec(FConfigureDCBLevel);
    if FConfigureDCBLevel = 0 then
      ConfigureDeviceControlBlock;
  end;
end;

procedure TSerial.EndConfigureTimeout;
begin
  if FConfigureTimeoutLevel > 0 then
  begin
    Dec(FConfigureTimeoutLevel);
    if FConfigureTimeoutLevel = 0 then
      ConfigureTimeouts;
  end else
    raise ESerial.Create('EndConfigureTimeout with BeginConfigureTimeout');
end;

procedure TSerial.FinalizeOverlapped;
begin
  CloseHandle(FOverlapped.hEvent);
  FillChar(FOverlapped, SizeOf(FOverlapped), #0);  
end;

function TSerial.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TSerial.GetCheckParity: Boolean;
begin
  Result := FCheckParity;
end;

function TSerial.GetComPort: TComPort;
begin
  Result := FComPort;
end;

function TSerial.GetDataBits: TDataBits;
begin
  Result := FDataBits;
end;

function TSerial.GetDSRSensitivity: Boolean;
begin
  Result := FDSRSensitivity;
end;

function TSerial.GetDTRFlowControl: TDTRFlowControl;
begin
  Result := FDTRFlowControl;
end;

function TSerial.GetEnabled: Boolean;
begin
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function TSerial.GetEventChar: Char;
begin
  Result := FEventChar;
end;

function TSerial.GetInputBufferSize: Cardinal;
begin
  Result := FInputBufferSize;
end;

function TSerial.GetInXonXoff: Boolean;
begin
  Result := FInXonXoff;
end;

function TSerial.GetOutCTS: Boolean;
begin
  Result := FOutCTS;
end;

function TSerial.GetOutDSR: Boolean;
begin
  Result := FOutDSR;
end;

function TSerial.GetOutputBufferSize: Cardinal;
begin
  Result := FOutputBufferSize;
end;

function TSerial.GetOutXonXoff: Boolean;
begin
  Result := FOutXonXoff;
end;

function TSerial.GetParityBits: TParityBits;
begin
  Result := FParityBits;
end;

function TSerial.GetReadIntervalTimeout: Integer;
begin
  Result := FReadIntervalTimeout;
end;

function TSerial.GetReadTotalTimeoutConstant: Integer;
begin
  Result := FReadTotalTimeoutConstant;
end;

function TSerial.GetReadTotalTimeoutMultiplier: Integer;
begin
  Result := FReadTotalTimeoutMultiplier;
end;

function TSerial.GetReplaceParity: Boolean;
begin
  Result := FReplaceParity;
end;

function TSerial.GetReplaceParityChar: Char;
begin
  Result := FReplaceParityChar;
end;

function TSerial.GetRTSFlowControl: TRTSFlowControl;
begin
  Result := FRTSFlowControl;
end;

function TSerial.GetStopBits: TStopBits;
begin
  Result := FStopBits;
end;

function TSerial.GetTxContinueOnXoff: Boolean;
begin
  Result := FTxContinueOnXoff;
end;

function TSerial.GetWriteTotalTimeoutConstant: Integer;
begin
  Result := FWriteTotalTimeoutConstant;
end;

function TSerial.GetWriteTotalTimeoutMultiplier: Integer;
begin
  Result := FWriteTotalTimeoutMultiplier;
end;

function TSerial.GetXoffChar: Char;
begin
  Result := FXoffChar;
end;

function TSerial.GetXonChar: Char;
begin
  Result := FXonChar;
end;

procedure TSerial.InitializeOverlapped;
begin
  FillChar(FOverlapped, SizeOf(FOverlapped), #0);
  FOverlapped.hEvent := CreateEvent(nil, True, True, nil);
end;

function TSerial.Read(var Buffer; const Count: Integer;
  out AmountRead: Integer; const TrueMeans: TTrueMeans;
  const TerminationEvent: THandle): Boolean;
begin
  Result := False;
  InitializeOverlapped;
  try
    StartAsyncRead(Buffer, Count);
    AmountRead := WaitForAsyncToComplete(TerminationEvent);

    case TrueMeans of
      tmCompleted:
        Result := AmountRead = Count;

      tmPartial:
        Result := AmountRead > 0;

    else
      raise ESerial.Create('Internal error');
    end;
  finally
    FinalizeOverlapped;
  end;
end;

function TSerial.ReadChar(out c: Char;
  const TerminationEvent: THandle): Boolean;
var
  AmountRead  : Integer;
begin
  Result := Read(c, 1, AmountRead, tmCompleted, TerminationEvent);
end;

function TSerial.ReadString(out s: string; const Length: Integer;
  const TrueMeans: TTrueMeans; const TerminationEvent: THandle): Boolean;
var
  AmountRead  : Integer;
begin
  SetLength(s, Length);
  Result := Read(s[1], Length, AmountRead, TrueMeans, TerminationEvent);
  SetLength(s, AmountRead);
end;

procedure TSerial.SetBaudRate(const Value: Integer);
begin
  if Value <> FBaudRate then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FBaudRate := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetBufferSizes(const InputBufferSize,
  OutputBufferSize: Cardinal);
begin
  BeginConfigureBuffers;
  try
    SetInputBufferSize(InputBufferSize);
    SetOutputBufferSize(OutputBufferSize);
  finally
    EndConfigureBuffers;
  end;
end;

procedure TSerial.SetCheckParity(const Value: Boolean);
begin
  if Value <> FCheckParity then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FCheckParity := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetComPort(const Value: TComPort);
begin
  if Value <> FComPort then
  begin
    Disable;
    FComPort := Value;
  end;
end;

procedure TSerial.SetDataBits(const Value: TDataBits);
begin
  if Value <> FDataBits then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FDataBits := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetDSRSensitivity(const Value: Boolean);
begin
  if Value <> FDSRSensitivity then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FDSRSensitivity := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetDTRFlowControl(const Value: TDTRFlowControl);
begin
  if Value <> FDTRFlowControl then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FDTRFlowControl := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetEnabled(const Value: Boolean);
begin
  if Value <> GetEnabled then
  begin
    if Value then
      DoEnable
    else
      DoDisable;
  end;
end;

procedure TSerial.SetEventChar(const Value: Char);
begin
  if Value <> FEventChar then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FEventChar := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetInputBufferSize(const Value: Cardinal);
begin
  if Value <> FInputBufferSize then
  begin
    BeginConfigureBuffers;
    try
      FInputBufferSize := Value;
    finally
      EndConfigureBuffers;
    end;
  end;
end;

procedure TSerial.SetInXonXoff(const Value: Boolean);
begin
  if Value <> FInXonXoff then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FInXonXoff := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetOutCTS(const Value: Boolean);
begin
  if Value <> FOutCTS then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FOutCTS := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetOutDSR(const Value: Boolean);
begin
  if Value <> FOutDSR then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FOutDSR := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetOutputBufferSize(const Value: Cardinal);
begin
  if Value <> FOutputBufferSize then
  begin
    BeginConfigureBuffers;
    try
      FOutputBufferSize := Value;
    finally
      EndConfigureBuffers;
    end;
  end;
end;

procedure TSerial.SetOutXonXoff(const Value: Boolean);
begin
  if Value <> FOutXonXoff then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FOutXonXoff := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetParityBits(const Value: TParityBits);
begin
  if Value <> FParityBits then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FParityBits := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetReadIntervalTimeout(const Value: Integer);
begin
  if FReadIntervalTimeout <> Value then
  begin
    BeginConfigureTimeout;
    try
      FReadIntervalTimeout := Value;
    finally
      EndConfigureTimeout;
    end;
  end;
end;

procedure TSerial.SetReadTotalTimeoutConstant(const Value: Integer);
begin
  if FReadTotalTimeoutConstant <> Value then
  begin
    BeginConfigureTimeout;
    try
      FReadTotalTimeoutConstant := Value;
    finally
      EndConfigureTimeout;
    end;
  end;
end;

procedure TSerial.SetReadTotalTimeoutMultiplier(const Value: Integer);
begin
  if FReadTotalTimeoutMultiplier <> Value then
  begin
    BeginConfigureTimeout;
    try
      FReadTotalTimeoutMultiplier := Value;
    finally
      EndConfigureTimeout;
    end;
  end;
end;

procedure TSerial.SetReplaceParity(const Value: Boolean);
begin
  if Value <> FReplaceParity then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FReplaceParity := Value;    
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetReplaceParityChar(const Value: Char);
begin
  if Value <> FReplaceParityChar then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FReplaceParityChar := Value;    
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetRTSFlowControl(const Value: TRTSFlowControl);
begin
  if Value <> FRTSFlowControl then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FRTSFlowControl := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetStopBits(const Value: TStopBits);
begin
  if Value <> FStopBits then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FStopBits := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetTimeouts(const ReadIntervalTimeout,
  ReadTotalTimeoutMultiplier, ReadTotalTimeoutConstant,
  WriteTotalTimeoutMultiplier, WriteTotalTimeoutConstant: Integer);
begin
  BeginConfigureTimeout;
  try
    SetReadIntervalTimeout(ReadIntervalTimeout);
    SetReadTotalTimeoutMultiplier(ReadTotalTimeoutMultiplier);
    SetReadTotalTimeoutConstant(ReadTotalTimeoutConstant);
    SetWriteTotalTimeoutMultiplier(WriteTotalTimeoutMultiplier);
    SetWriteTotalTimeoutConstant(WriteTotalTimeoutConstant);
  finally
    EndConfigureTimeout;
  end;
end;

procedure TSerial.SetTxContinueOnXoff(const Value: Boolean);
begin
  if Value <> FTxContinueOnXoff then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FTxContinueOnXoff := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetWriteTotalTimeoutConstant(const Value: Integer);
begin
  if FWriteTotalTimeoutConstant <> Value then
  begin
    BeginConfigureTimeout;
    try
      FWriteTotalTimeoutConstant := Value;
    finally
      EndConfigureTimeout;
    end;
  end;
end;

procedure TSerial.SetWriteTotalTimeoutMultiplier(const Value: Integer);
begin
  if FWriteTotalTimeoutMultiplier <> Value then
  begin
    BeginConfigureTimeout;
    try
      FWriteTotalTimeoutMultiplier := Value;
    finally
      EndConfigureTimeout;
    end;
  end;
end;

procedure TSerial.SetXoffChar(const Value: Char);
begin
  if Value <> FXoffChar then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FXoffChar := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.SetXonChar(const Value: Char);
begin
  if Value <> FXonChar then
  begin
    BeginConfigureDeviceControlBlock;
    try
      FXonChar := Value;
    finally
      EndConfigureDeviceControlBlock;
    end;
  end;
end;

procedure TSerial.StartAsyncRead(var Buffer; const Count: Integer);
var
  BytesRead : Cardinal;
begin
  // Kick off the overlapped read operation
  if not ReadFile(FHandle, Buffer, Count, BytesRead, @FOverlapped) then
    if GetLastError <> ERROR_IO_PENDING then
      RaiseLastWin32Error;
end;

procedure TSerial.StartAsyncWrite(const Buffer;
  const Count: Integer);
var
  BytesWritten  : Cardinal;
begin
  // Kick off the overlapped read operation
  if not WriteFile(FHandle, Buffer, Count, BytesWritten, @FOverlapped) then
    if GetLastError <> ERROR_IO_PENDING then
      RaiseLastWin32Error;
end;

function TSerial.WaitForAsyncToComplete(
  const TerminationEvent: THandle): Integer;
var
  Events    : array[0..1] of THandle;
  rc        : Integer;
  BytesRead : Cardinal;
begin
  Result := 0;

  // Wait for it to complete
  if TerminationEvent = 0 then
    rc := WaitForSingleObject(FOverlapped.hEvent, INFINITE)
  else begin
    Events[0] := FOverlapped.hEvent;
    Events[1] := TerminationEvent;
    rc := WaitForMultipleObjects(2, @Events, False, INFINITE);
  end;

  if rc = WAIT_OBJECT_0 then
  begin
    if not GetOverlappedResult(FHandle, FOverlapped, BytesRead, False) then
      RaiseLastWin32Error;
    Result := BytesRead;
  end;
end;

function TSerial.Write(const Buffer; const Count: Integer;
  out AmountWritten: Integer; const TrueMeans: TTrueMeans;
  const TerminationEvent: THandle): Boolean;
begin
  Result := False;
  InitializeOverlapped;
  try
    StartAsyncWrite(Buffer, Count);
    AmountWritten := WaitForAsyncToComplete(TerminationEvent);

    case TrueMeans of
      tmCompleted:
        Result := AmountWritten = Count;

      tmPartial:
        Result := AmountWritten > 0;

    else
      raise ESerial.Create('Internal error');
    end;
  finally
    FinalizeOverlapped;
  end;
end;

function TSerial.WriteChar(const c: Char;
  const TerminationEvent: THandle): Boolean;
var
  AmountWritten : Integer;
begin
  Result := Write(c, 1, AmountWritten, tmCompleted, TerminationEvent);
end;

function TSerial.WriteString(const s: string; out AmountWritten: Integer;
  const TrueMeans: TTrueMeans; const TerminationEvent: THandle): Boolean;
begin
  Result := Write(s[1], Length(s), AmountWritten, TrueMeans, TerminationEvent);
end;

end.
