unit Unit16;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, hwid_impl;

type
  TForm16 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetIdeDiskSerialNumber: string;
    function GetHDCode(dispositivo: PWideChar; indiceHD: byte = 0): AnsiString;
  end;

var
  Form16: TForm16;

implementation

{$DEFINE SystemFind}
{$R *.dfm}

function TForm16.GetHDCode(dispositivo: PWideChar; indiceHD: byte = 0): AnsiString;
var erro: cardinal;
const
  IDENTIFY_BUFFER_SIZE = 512;
type
  TIDERegs = packed record
    bFeaturesReg: BYTE; // Used for specifying SMART "commands".
    bSectorCountReg: BYTE; // IDE sector count register
    bSectorNumberReg: BYTE; // IDE sector number register
    bCylLowReg: BYTE; // IDE low order cylinder value
    bCylHighReg: BYTE; // IDE high order cylinder value
    bDriveHeadReg: BYTE; // IDE drive/head register
    bCommandReg: BYTE; // Actual IDE command.
    bReserved: BYTE; // reserved for future use.  Must be zero.
  end;

  TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize: DWORD;
    // Structure with drive register values.
    irDriveRegs: TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber: BYTE;
    bReserved: Array [0 .. 2] of BYTE;
    dwReserved: Array [0 .. 3] of DWORD;
    bBuffer: Array [0 .. 0] of BYTE; // Input buffer.
  end;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: Array [0 .. 2] of Word;

    sSerialNumber: Array [0 .. 19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: Array [0 .. 7] of CHAR;
    sModelNumber: Array [0 .. 39] of CHAR;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: Array [0 .. 127] of BYTE;
  end;

  PIdSector = ^TIdSector;

  TDriverStatus = packed record
    // Error code from driver, or 0 if no error.
    bDriverError: BYTE;
    // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
    bIDEStatus: BYTE;
    bReserved: Array [0 .. 1] of BYTE;
    dwReserved: Array [0 .. 1] of DWORD;
  end;

  TSendCmdOutParams = packed record
    // Size of bBuffer in bytes
    cBufferSize: DWORD;
    // Driver status structure.
    DriverStatus: TDriverStatus;
    // Buffer of arbitrary length in which to store the data read from the drive.
    bBuffer: Array [0 .. 0] of BYTE;
  end;

var
  hDevice: THandle;
  cbBytesReturned: DWORD;
  ptr: PChar;
  SCIP: TSendCmdInParams;
  aIdOutCmd: Array [0 .. (SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1)
    - 1] of BYTE;
  IdOutCmd: TSendCmdOutParams absolute aIdOutCmd;

  procedure ChangeByteOrder(var Data; Size: Integer);
  var
    ptr: PAnsiChar;
    i: Integer;
    c: AnsiChar;
  begin
    ptr := @Data;
    for i := 0 to (Size shr 1) - 1 do
    begin
      c := ptr^;
      ptr^ := (ptr + 1)^; (ptr + 1)
      ^ := c;
      Inc(ptr, 2);
    end;
  end;

begin

  // result := 'WD-WXE309KA8460';
  // exit;

  Result := ''; // return empty string on error
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
  // Windows NT, Windows 2000
  begin
    // warning! change name for other drives: ex.: second drive '\\.\PhysicalDrive1\'
    hDevice := CreateFile(dispositivo, GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  end
  else // Version Windows 95 OSR2, Windows 98
    hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
  if hDevice = INVALID_HANDLE_VALUE then
  begin
    result := (SysErrorMessage(GetLastError))+' [1]';
    Exit;
  end;
  try
    FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
    FillChar(aIdOutCmd, SizeOf(aIdOutCmd), #0);
    cbBytesReturned := 0;
    // Set up data structures for IDENTIFY command.
    with SCIP do
    begin
      cBufferSize := IDENTIFY_BUFFER_SIZE;
      bDriveNumber := indiceHD;
      with irDriveRegs do
      begin
        bSectorCountReg := 1;
        bSectorNumberReg := 1;
        // if Win32Platform=VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
        // else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
        bDriveHeadReg := $A0;
        bCommandReg := $EC;
      end;
    end;
    if not DeviceIoControl(hDevice, $0007C088, @SCIP, SizeOf(TSendCmdInParams)
        - 1, @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil) then
    begin
      erro := GetLastError;
      OutputDebugStringA(PAnsiChar(AnsiString('Erro '+IntToStr(erro))));
      result := (SysErrorMessage(erro))+' [2]';
      Exit;
    end;
  finally
    CloseHandle(hDevice);
  end;

  with PIdSector(@IdOutCmd.bBuffer)^ do
  begin
    ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
(PAnsiChar(@sSerialNumber) + SizeOf(sSerialNumber))
    ^ := #0;
    Result := trim(strpas(PAnsiChar(@sSerialNumber)));
  end;
end;

function TForm16.GetIdeDiskSerialNumber: string;
type
  TSrbIoControl = packed record
    HeaderLength: ULONG;
    Signature: array [0 .. 7] of CHAR;
    Timeout: ULONG;
    ControlCode: ULONG;
    ReturnCode: ULONG;
    Length: ULONG;
  end;

  SRB_IO_CONTROL = TSrbIoControl;
  PSrbIoControl = ^TSrbIoControl;

  TIDERegs = packed record
    bFeaturesReg: BYTE; // Used for specifying SMART "commands".
    bSectorCountReg: BYTE; // IDE sector count register
    bSectorNumberReg: BYTE; // IDE sector number register
    bCylLowReg: BYTE; // IDE low order cylinder value
    bCylHighReg: BYTE; // IDE high order cylinder value
    bDriveHeadReg: BYTE; // IDE drive/head register
    bCommandReg: BYTE; // Actual IDE command.
    bReserved: BYTE; // reserved. Must be zero.
  end;

  IDEREGS = TIDERegs;
  PIDERegs = ^TIDERegs;

  TSendCmdInParams = packed record
    cBufferSize: DWORD;
    irDriveRegs: TIDERegs;
    bDriveNumber: BYTE;
    bReserved: array [0 .. 2] of BYTE;
    dwReserved: array [0 .. 3] of DWORD;
    bBuffer: array [0 .. 0] of BYTE;
  end;

  SENDCMDINPARAMS = TSendCmdInParams;
  PSendCmdInParams = ^TSendCmdInParams;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array [0 .. 2] of Word;
    sSerialNumber: array [0 .. 19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array [0 .. 7] of CHAR;
    sModelNumber: array [0 .. 39] of CHAR;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: ULONG;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: ULONG;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array [0 .. 127] of BYTE;
  end;

  PIdSector = ^TIdSector;

const
  IDE_ID_FUNCTION = $EC;
  IDENTIFY_BUFFER_SIZE = 512;
  DFP_RECEIVE_DRIVE_DATA = $0007C088;
  IOCTL_SCSI_MINIPORT = $0004D008;
  IOCTL_SCSI_MINIPORT_IDENTIFY = $001B0501;
  DataSize = SizeOf(TSendCmdInParams) - 1 + IDENTIFY_BUFFER_SIZE;
  BufferSize = SizeOf(SRB_IO_CONTROL) + DataSize;
  W9xBufferSize = IDENTIFY_BUFFER_SIZE + 16;
var
  hDevice: THandle;
  cbBytesReturned: DWORD;
  pInData: PSendCmdInParams;
  pOutData: Pointer; // PSendCmdOutParams
  Buffer: array [0 .. BufferSize - 1] of BYTE;
  srbControl: TSrbIoControl absolute Buffer;
  pc: PChar;

  procedure ChangeByteOrder(var Data; Size: Integer);
  var
    ptr: PChar;
    i: Integer;
    c: CHAR;
  begin
    ptr := @Data;
    for i := 0 to (Size shr 1) - 1 do
    begin
      c := ptr^;
      ptr^ := (ptr + 1)^; (ptr + 1)
      ^ := c;
      Inc(ptr, 2);
    end;
  end;

begin
  Result := '';
  FillChar(Buffer, BufferSize, #0);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin // Windows NT, Windows 2000
    // Get SCSI port handle
    hDevice := CreateFile('\\.\Scsi0:', GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if hDevice = INVALID_HANDLE_VALUE then
    begin
      result := (SysErrorMessage(GetLastError))+' [3]';
      Exit;
    end;

    try
      srbControl.HeaderLength := SizeOf(SRB_IO_CONTROL);
{$IFNDEF SystemFind}
      System.Move('SCSIDISK', srbControl.Signature, 8);
{$ENDIF}
      srbControl.Timeout := 2;
      srbControl.Length := DataSize;
      srbControl.ControlCode := IOCTL_SCSI_MINIPORT_IDENTIFY;
      pInData := PSendCmdInParams(PChar(@Buffer) + SizeOf(SRB_IO_CONTROL));
      pOutData := pInData;
      with pInData^ do
      begin
        cBufferSize := IDENTIFY_BUFFER_SIZE;
        bDriveNumber := 0;
        with irDriveRegs do
        begin
          bFeaturesReg := 0;
          bSectorCountReg := 1;
          bSectorNumberReg := 1;
          bCylLowReg := 0;
          bCylHighReg := 0;
          bDriveHeadReg := $A0;
          bCommandReg := IDE_ID_FUNCTION;
        end;
      end;
      if not DeviceIoControl(hDevice, IOCTL_SCSI_MINIPORT, @Buffer, BufferSize,
        @Buffer, BufferSize, cbBytesReturned, nil) then
      begin
        result := (SysErrorMessage(GetLastError))+' [4]';
        Exit;
      end;
    finally
      CloseHandle(hDevice);
    end;
  end
  else
  begin // Windows 95 OSR2, Windows 98
    hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
    if hDevice = INVALID_HANDLE_VALUE then
      Exit;
    try
      pInData := PSendCmdInParams(@Buffer);
      pOutData := @pInData^.bBuffer;
      with pInData^ do
      begin
        cBufferSize := IDENTIFY_BUFFER_SIZE;
        bDriveNumber := 0;
        with irDriveRegs do
        begin
          bFeaturesReg := 0;
          bSectorCountReg := 1;
          bSectorNumberReg := 1;
          bCylLowReg := 0;
          bCylHighReg := 0;
          bDriveHeadReg := $A0;
          bCommandReg := IDE_ID_FUNCTION;
        end;
      end;
      if not DeviceIoControl(hDevice, DFP_RECEIVE_DRIVE_DATA, pInData, SizeOf
          (TSendCmdInParams) - 1, pOutData, W9xBufferSize, cbBytesReturned,
        nil) then
      begin
        result := (SysErrorMessage(GetLastError))+' [5]';
        Exit;
      end;
    finally
      CloseHandle(hDevice);
    end;
  end;
  with PIdSector(PChar(pOutData) + 16)^ do
  begin
    ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
    SetString(Result, sSerialNumber, SizeOf(sSerialNumber));
  end;
end;

procedure TForm16.FormCreate(Sender: TObject);
var
  i,j, n: Integer;
  s: String;
  wc: PWideChar;
  serialNumber: array [0..1024-1] of char;
  resultado: tresults_array_dv;
begin
  getHardDriveComputerID(resultado);
  Memo1.Lines.Add('Count: '+IntToStr(length(resultado)));
  if length(resultado) > 0 then
    for i := 0 to length(resultado) - 1 do
        Memo1.Lines.Add(trim(resultado[i].DriveSerialNumber));
  (*Memo1.Lines.Add(GetIdeDiskSerialNumber);
  Edit1.Text := IntToStr(SysUtils.Win32Platform) + ' - ' + IntToStr
    (SysUtils.Win32MajorVersion) + ' - ' + IntToStr(SysUtils.Win32MinorVersion)
    + ' - ' + IntToStr(SysUtils.Win32BuildNumber) + ' - ' +
    (SysUtils.Win32CSDVersion);
  for i := 0 to 10 do
  begin
    for j := 0 to 4 do
    begin
        s := '\\.\PhysicalDrive' + IntToStr(i);
        n := (Length(s) * 2) + 2;
        wc := GetMemory(n);
        ZeroMemory(wc, n);
        Memo1.Lines.Add(IntToStr(i) + ' - ' + GetHDCode(StringToWideChar(s, wc, n), j)
          );
        FreeMem(wc);
    end;
  end;   *)



end;

end.
