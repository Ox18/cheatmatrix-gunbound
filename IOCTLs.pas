unit IOCTLs;

interface

uses windows;

const
  KM_USER = $800;
  CM_KM_USER = KM_USER + 248;

  METHOD_BUFFERED=    0;
  METHOD_IN_DIRECT=   1;
  METHOD_OUT_DIRECT=  2;
  METHOD_NEITHER=     3;
  FILE_DEVICE_UNKNOWN=$00000022;
  IOCTL_UNKNOWN_BASE=FILE_DEVICE_UNKNOWN;

  FILE_ANY_ACCESS=0;
  FILE_SPECIAL_ACCESS=FILE_ANY_ACCESS;
  FILE_READ_ACCESS=$0001;
  FILE_WRITE_ACCESS=$0002;
  FILE_RW_ACCESS=FILE_READ_ACCESS or FILE_WRITE_ACCESS;

  FLAG_GetVersion = 1;

type
 TDrvCommRequestBufferStructParametersUnionProtectProcess = packed record
  Pid: ULONG;
  Enable:Integer;
 end;

 TDrvCommRequestBufferStructParameters=packed record
  case Byte of 
   0:(ProtectProcess:TDrvCommRequestBufferStructParametersUnionProtectProcess);
 end;

 PDrvCommRequestBuffer=^TDrvCommRequestBuffer;
 TDrvCommRequestBuffer=packed record
  Parameters:TDrvCommRequestBufferStructParameters;
 end;

 PDrvCommResponseBuffer=^TDrvCommResponseBuffer;
 TDrvCommResponseBuffer=packed record
  Status:ULONG;
 end;

 const
  IOCTL_GetPspCidTable          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 01) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_GetDeviceVersion        = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 02) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_KiServiceCount          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 03) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_AddService              = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 04) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_GetCMTableStart         = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 05) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_SDTPointerInfo          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 06) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_ChangeSDTEntry          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 07) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_ChangeCMTableStart      = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 08) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_ReadMemory              = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 09) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_WriteMemory             = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 10) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_OpenProcess             = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 11) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_GetProcName             = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 12) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_ForceUnloadDriver       = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 13) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_GetProcessListLength    = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 14) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_GetProcessList          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 15) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_HideCM                  = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 16) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_UnHideCM                = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 17) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_InitializeVars          = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 18) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_ProtectMemory           = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 19) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
  IOCTL_Disasm                  = (IOCTL_UNKNOWN_BASE shl 16) or ((CM_KM_USER + 20) shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

implementation

end.
