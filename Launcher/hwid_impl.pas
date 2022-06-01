unit hwid_impl;
//{$DEFINE DEBUG}
//{$DEFINE PRINTING_TO_CONSOLE_ALLOWED}

//  diskid32.cpp


//  for displaying the details of hard drives in a command window


//  06/11/00  Lynn McGuire  written with many contributions from others,
//                            IDE drives only under Windows NT/2K and 9X,
//                            maybe SCSI drives later
//  11/20/03  Lynn McGuire  added ReadPhysicalDriveInNTWithZeroRights
//  10/26/05  Lynn McGuire  fix the flipAndCodeBytes function
//  01/22/08  Lynn McGuire  incorporate changes from Gonzalo Diethelm,
//                             remove media serial number code since does
//                             not work on USB hard drives or thumb drives
//  01/29/08  Lynn McGuire  add ReadPhysicalDriveInNTUsingSmart
//  05/02/09  Ported from C++ to Delphi by Victor Derevyanko, dvpublic0@gmail.com
//            The translation was donated by efaktum (http://www.efaktum.dk).
//            If you find any error please send me bugreport by email. Thanks in advance.

// Original code can be taken here:
// http://www.winsim.com/diskid32/diskid32.html

//{$Id: hwid_impl.pas 415 2009-02-07 02:44:07Z Administrator $}

interface
uses Windows, SysUtils;

type
//record for storing results (instead of printing on console)
  tresults_dv = record
    ControllerType: Integer; //0 - primary, 1 - secondary, 2 - Tertiary, 3 - Quaternary
    DriveMS: Integer; //0 - master, 1 - slave
    DriveModelNumber: AnsiString;
    DriveSerialNumber: AnsiString;
    DriveControllerRevisionNumber: AnsiString;
    ControllerBufferSizeOnDrive: Int64;
    DriveType: AnsiString; //fixed or removable or unknown
    DriveSizeBytes: Int64;
  end;

  tresults_array_dv = array of tresults_dv;
//use one by one all ReadXXX methods to fill global variables HardDriveSerialNumber and HardDriveModelNumber
//with information about FIRST found HDD
//returns unique id for this computer
function getHardDriveComputerID(var Dest: tresults_array_dv): Longint;

//Calling SetPrintDebugInfo(true) will lead to printing additional debug info on console
//(only if PRINTING_TO_CONSOLE_ALLOWED is defined)
procedure SetPrintDebugInfo(bOn: Boolean);

function ReadIdeDriveAsScsiDriveInNT(var Dest: tresults_array_dv): Boolean;
function ReadDrivePortsInWin9X(var Dest: tresults_array_dv): Boolean; //!This code wasn't tested
function ReadPhysicalDriveInNTWithZeroRights(var Dest: tresults_array_dv): Boolean;
function ReadPhysicalDriveInNTUsingSmart(var Dest: tresults_array_dv): Boolean;
function ReadPhysicalDriveInNTWithAdminRights(var Dest: tresults_array_dv): Boolean;

{$IFDEF DEBUG}
//to ensure that sizes of declared structurs are correct (same as in original c++-sources)
procedure test;
{$ENDIF}

implementation
uses winioctl, crtdll_wrapper;
var
  HardDriveSerialNumber: array [0..1023] of AnsiChar;
  HardDriveModelNumber: array [0..1023] of AnsiChar;

//readonly values
  SMART_GET_VERSION: Integer;
  SMART_SEND_DRIVE_COMMAND: Integer;
  SMART_RCV_DRIVE_DATA: Integer;

  PRINT_DEBUG: Boolean;

const
  TITLE = 'DiskId32';

  IDENTIFY_BUFFER_SIZE = 512;

   //  IOCTL commands
  DFP_GET_VERSION = $00074080;
  DFP_SEND_DRIVE_COMMAND = $0007c084;
  DFP_RECEIVE_DRIVE_DATA = $0007c088;

  FILE_DEVICE_SCSI = $0000001b;
  IOCTL_SCSI_MINIPORT_IDENTIFY = ((FILE_DEVICE_SCSI shl 16) + $0501);
  IOCTL_SCSI_MINIPORT = $0004D008;  //  see NTDDSCSI.H for definition

type
{$Align 1}  //#pragma pack(1) //  Required to ensure correct PhysicalDrive IOCTL structure setup
  GETVERSIONINPARAMS = record
    bVersion: Byte;               // Binary driver version.
    bRevision: Byte;              // Binary driver revision.
    bReserved: Byte;              // Not used.
    bIDEDeviceMap: Byte;          // Bit map of IDE devices.
    fCapabilities: Cardinal;          // Bit mask of driver capabilities.
    dwReserved: array [0..3] of Cardinal;          // For future use.
  end;
{$Align On} //default value
  PGETVERSIONINPARAMS = ^GETVERSIONINPARAMS;
  LPGETVERSIONINPARAMS = ^GETVERSIONINPARAMS;

//  GETVERSIONOUTPARAMS contains the data returned from the
//  Get Driver Version function.
  GETVERSIONOUTPARAMS = record
   bVersion: Byte;// Binary driver version.
   bRevision: Byte;// Binary driver revision.
   bReserved: Byte;// Not used.
   bIDEDeviceMap: Byte;// Bit map of IDE devices.
   fCapabilities: Longword;// Bit mask of driver capabilities.
   dwReserved: array [0..3] of Longword;// For future use.
  end;
  PGETVERSIONOUTPARAMS = ^GETVERSIONOUTPARAMS;
  LPGETVERSIONOUTPARAMS = ^GETVERSIONOUTPARAMS;

const
   //  Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
  CAP_IDE_ID_FUNCTION = 1;  // ATA ID command supported
  CAP_IDE_ATAPI_ID = 2;  // ATAPI ID command supported
  CAP_IDE_EXECUTE_SMART_FUNCTION = 4;  // SMART commannds supported

type
   //  IDE registers
  IDEREGS = record
   bFeaturesReg: Byte;// Used for specifying SMART "commands".
   bSectorCountReg: Byte;// IDE sector count register
   bSectorNumberReg: Byte;// IDE sector number register
   bCylLowReg: Byte;// IDE low order cylinder value
   bCylHighReg: Byte;// IDE high order cylinder value
   bDriveHeadReg: Byte;// IDE drive/head register
   bCommandReg: Byte;// Actual IDE command.
   bReserved: Byte;// reserved for future use.  Must be zero.
  end;
  PIDEREGS = ^IDEREGS;
  LPIDEREGS = ^IDEREGS;


//  SENDCMDINPARAMS contains the input parameters for the
//  Send Command to Drive function.
{$ALIGN 1}
  SENDCMDINPARAMS = record
   cBufferSize: Longword;//  Buffer size in bytes
   irDriveRegs: IDEREGS;   //  Structure with drive register values.
   bDriveNumber: Byte;//  Physical drive number to send
                            //  command to (0,1,2,3).
   bReserved: array[0..2] of Byte;//  Reserved for future expansion.
   dwReserved: array [0..3] of Longword;//  For future use.
   bBuffer: array [0..0] of Byte;//  Input buffer.     //!TODO: this is array of single element
  end;
{$ALIGN on}
  PSENDCMDINPARAMS = ^SENDCMDINPARAMS;
  LPSENDCMDINPARAMS = ^SENDCMDINPARAMS;


   //  Valid values for the bCommandReg member of IDEREGS.
const
  IDE_ATAPI_IDENTIFY = $A1;  //  Returns ID sector for ATAPI.
  IDE_ATA_IDENTIFY = $EC;  //  Returns ID sector for ATA.

{$ALIGN 1}
type
   // Status returned from driver
  DRIVERSTATUS = record
   bDriverError: Byte;//  Error code from driver, or 0 if no error.
   bIDEStatus: Byte;//  Contents of IDE Error register.
                        //  Only valid when bDriverError is SMART_IDE_ERROR.
   bReserved: array [0..1] of Byte;//  Reserved for future expansion.
   dwReserved: array [0..1] of Longword;//  Reserved for future expansion.
  end;
{$ALIGN on}
  PDRIVERSTATUS = ^DRIVERSTATUS;
  LPDRIVERSTATUS = ^DRIVERSTATUS;

   // Structure returned by PhysicalDrive IOCTL for several commands
{$ALIGN 1}
  SENDCMDOUTPARAMS = record
   cBufferSize: Longword;//  Size of bBuffer in bytes
   DriverStatus: DRIVERSTATUS;//  Driver status structure.
   bBuffer: array [0..0] of Byte;//  Buffer of arbitrary length in which to store the data read from the                                                       // drive.
  end;
{$ALIGN on}
  PSENDCMDOUTPARAMS = ^SENDCMDOUTPARAMS;
  LPSENDCMDOUTPARAMS = ^SENDCMDOUTPARAMS;

// The following struct defines the interesting part of the IDENTIFY
// buffer:
{$ALIGN 1}
  IDSECTOR = record
   wGenConfig: Word;
   wNumCyls: Word;
   wReserved: Word;
   wNumHeads: Word;
   wBytesPerTrack: Word;
   wBytesPerSector: Word;
   wSectorsPerTrack: Word;
   wVendorUnique: array [0..3-1] of Word;
   sSerialNumber: array [0..20-1] of Ansichar;
   wBufferType: Word;
   wBufferSize: Word;
   wECCSize: Word;
   sFirmwareRev: array [0..8-1] of AnsiChar;
   sModelNumber: array [0..40-1] of AnsiChar;
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
   ulCurrentSectorCapacity: Cardinal;
   wMultSectorStuff: Word;
   ulTotalAddressableSectors: Cardinal;
   wSingleWordDMA: Word;
   wMultiWordDMA: Word;
   bReserved: array [0..128-1] of Byte;
  end;
{$ALIGN on}
  PIDSECTOR = ^IDSECTOR;

  SRB_IO_CONTROL = record
   HeaderLength: Cardinal;
   Signature: array [0..8-1] of Byte;
   Timeout: Cardinal;
   ControlCode: Cardinal;
   ReturnCode: Cardinal;
   Length: Cardinal;
  end;
  PSRB_IO_CONTROL = ^SRB_IO_CONTROL;

 // Define global buffers.
var IdOutCmd: array [0..sizeof (SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1 - 1] of Byte;

type tdiskdata_dv = array [0..256-1] of DWORD;
function ConvertToString (diskdata: tdiskdata_dv;
  firstIndex: Integer;
	lastIndex: Integer;
  buf: PAnsiChar): PAnsiChar; forward;

function PrintIdeInfo (drive: Integer; diskdata: tdiskdata_dv): tresults_dv; forward;
function DoIDENTIFY (hPhysicalDriveIOCTL: THandle; pSCIP: PSENDCMDINPARAMS;
                 pSCOP: PSENDCMDOUTPARAMS; bIDCmd: Byte; bDriveNum: Byte;
                 lpcbBytesReturned: PCardinal): Integer; forward;//BOOL


   //  Max number of drives assuming primary/secondary, master/slave topology
const MAX_IDE_DRIVES = 16;


///begin dv auxilary declarations
type
  tarray_of_words256_dv = array [0..256-1] of WORD;
  parray_of_words256_dv = ^tarray_of_words256_dv;
///end dv auxilary declarations

function debugar(valor: String): AnsiString;
var p: PAnsiChar;
begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
    p := PAnsiChar(AnsiString(valor));
    OutputDebugStringA(p);
{$ENDIF}
end;

function StringToPAnsiChar(stringVar : string) : PAnsiChar;
Var
  AnsString : AnsiString;
  InternalError : Boolean;
begin
  InternalError := false;
  Result := '';
  try
    if stringVar <> '' Then
    begin
       AnsString := AnsiString(StringVar);
       Result := PAnsiChar(PAnsiString(AnsString));
    end;
  Except
    InternalError := true;
  end;
  if InternalError or (String(Result) <> stringVar) then
  begin
    Raise Exception.Create('Conversion from string to PAnsiChar failed!');
  end;
end;


function ReadPhysicalDriveInNTWithAdminRights(var Dest: tresults_array_dv): Boolean;
var
  drive: Integer;
  hPhysicalDriveIOCTL: THandle;
  driveName: array [0..256-1] of ansichar;

  VersionParams: GETVERSIONOUTPARAMS;
  cbBytesReturned: DWORD;

  bIDCmd: Byte;   // IDE or ATAPI IDENTIFY cmd
  scip: SENDCMDINPARAMS;

  diskdata: tdiskdata_dv;
  ijk: Integer;
  pIdSector: PWord;

  count_drives_dv: Integer;
begin
   SetLength(Dest, MAX_IDE_DRIVES-1);
   count_drives_dv := 0;

   for drive := 0 to MAX_IDE_DRIVES-1 do begin
      //-hPhysicalDriveIOCTL := 0;

         //  Try to get a handle to PhysicalDrive IOCTL, report failure
         //  and exit if can't.
      //- char driveName [256];

      //= sprintf (driveName, "\\\\.\\PhysicalDrive%d", drive);
      StrCopy(driveName, StringToPAnsiChar(Format('\\.\PhysicalDrive%d', [drive])));

         //  Windows NT, Windows 2000, must have admin rights
      hPhysicalDriveIOCTL := CreateFileA (driveName,
                               GENERIC_READ or GENERIC_WRITE,
                               FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                               OPEN_EXISTING, 0, 0);
      // if (hPhysicalDriveIOCTL == INVALID_HANDLE_VALUE)
      //    printf ("Unable to open physical drive %d, error code: 0x%lX'+#$0D#$0A+'",
      //            drive, GetLastError ());

      if (hPhysicalDriveIOCTL = INVALID_HANDLE_VALUE) then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
        if (PRINT_DEBUG) then begin
			    debugar((Format(#$0D+#$0A+'%d ReadPhysicalDriveInNTWithAdminRights ERROR'+#$0D#$0A+'CreateFile(%s) returned INVALID_HANDLE_VALUE'+#$0D#$0A,
            [0//=__LINE__
            , driveName])));
        end;
{$endif}
      end else begin
//-         GETVERSIONOUTPARAMS VersionParams;
         cbBytesReturned := 0;

            // Get the version, etc of PhysicalDrive IOCTL
         FillMemory(@VersionParams, sizeof(VersionParams), 0);      //=memset ((void*) &VersionParams, 0, sizeof(VersionParams));

         if ( not DeviceIoControl(hPhysicalDriveIOCTL, DFP_GET_VERSION,
                   nil,
                   0,
                   @VersionParams,
                   sizeof(VersionParams),
                   cbBytesReturned, nil)
         ) then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
            if (PRINT_DEBUG) then begin
	            debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTWithAdminRights ERROR' +
		               #$0D#$0A+'DeviceIoControl(%d, DFP_GET_VERSION) returned 0, error is %d'+#$0D#$0A,
		               [0//__LINE__
                    , Integer(hPhysicalDriveIOCTL)
                    , Integer(GetLastError())]));
		        end
{$ENDIF}
         end;

            // If there is a IDE device at number "i" issue commands
            // to the device
         if (VersionParams.bIDEDeviceMap <= 0) then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
            if (PRINT_DEBUG) then begin
	            debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTWithAdminRights ERROR' +
		                #$0D#$0A+'No device found at position %d (%d)'+#$0D#$0A,
		                [0//__LINE__
                      , Integer(drive)
                      , Integer(VersionParams.bIDEDeviceMap)]));
            end;
{$ENDIF}
         end else begin
//-            bIDCmd := 0;   // IDE or ATAPI IDENTIFY cmd
//-            SENDCMDINPARAMS  scip;
            //SENDCMDOUTPARAMS OutCmd;

			   // Now, get the ID sector for all IDE devices in the system.
               // If the device is ATAPI use the IDE_ATAPI_IDENTIFY command,
               // otherwise use the IDE_ATA_IDENTIFY command
            if (((VersionParams.bIDEDeviceMap shr drive) and $10) <> 0)
              then bIDCmd := IDE_ATAPI_IDENTIFY
              else bIDCmd := IDE_ATA_IDENTIFY;

            FillMemory(@scip, sizeof(scip), 0);
            FillMemory(@IdOutCmd[0], sizeof(IdOutCmd), 0);

            if ( 0 <> DoIDENTIFY (hPhysicalDriveIOCTL, @scip, PSENDCMDOUTPARAMS(@IdOutCmd[0])
              , BYTE(bIDCmd), BYTE(drive), @cbBytesReturned))
            then begin
//-               DWORD diskdata [256];
//=               USHORT *pIdSector = (USHORT *) ((PSENDCMDOUTPARAMS) IdOutCmd) -> bBuffer;
               pIdSector := PWord(@PSENDCMDOUTPARAMS(@IdOutCmd[0])^.bBuffer[0]); //!TOCHECK

               //delphi has no arithmetic for pointers; so, emulate it using arrays
               for ijk := 0 to 256-1 do begin
                  diskdata [ijk] := parray_of_words256_dv(pIdSector)[ijk];
               end;

               Dest[count_drives_dv] := PrintIdeInfo (drive, diskdata);
               inc(count_drives_dv);
            end;
	      end;

        CloseHandle(hPhysicalDriveIOCTL);
      end;
   end;

   SetLength(Dest, count_drives_dv);
   Result := count_drives_dv > 0;
end;

//
// IDENTIFY data (from ATAPI driver source)
//
{$ALIGN 1}//pragma pack(1)
type IDENTIFY_DATA = record
    GeneralConfiguration: Word;             // 00 00
    NumberOfCylinders: Word;                // 02  1
    Reserved1: Word;                        // 04  2
    NumberOfHeads: Word;                    // 06  3
    UnformattedBytesPerTrack: Word;         // 08  4
    UnformattedBytesPerSector: Word;        // 0A  5
    SectorsPerTrack: Word;                  // 0C  6
    VendorUnique1: array [0..3-1] of Word;                 // 0E  7-9
    SerialNumber: array [0..10-1] of Word;                 // 14  10-19
    BufferType: Word;                       // 28  20
    BufferSectorSize: Word;                 // 2A  21
    NumberOfEccBytes: Word;                 // 2C  22
    FirmwareRevision: array [0..4-1] of  Word;              // 2E  23-26
    ModelNumber: array [0..20-1] of  Word;                  // 36  27-46
    MaximumBlockTransfer: Byte;             // 5E  47
    VendorUnique2: Byte;                    // 5F
    DoubleWordIo: Word;                     // 60  48
    Capabilities: Word;                     // 62  49
    Reserved2: Word;                        // 64  50
    VendorUnique3: Byte;                    // 66  51
    PioCycleTimingMode: Byte;               // 67
    VendorUnique4: Byte;                    // 68  52
    DmaCycleTimingMode: Byte;               // 69

// Delhpi has no bit fields. Fortunately, we don't need this
// record memebers in our application. So, we can simplify declaration of the record.

//    USHORT TranslationFieldsValid:1;        // 6A  53
//    USHORT Reserved3:15;
    TranslationFieldsValid: Word;         // 6A  53 //Reserved3 is in the last 15 bits.

    NumberOfCurrentCylinders: Word;         // 6C  54
    NumberOfCurrentHeads: Word;             // 6E  55
    CurrentSectorsPerTrack: Word;           // 70  56
    CurrentSectorCapacity: Cardinal;            // 72  57-58
    CurrentMultiSectorSetting: Word;        //     59
    UserAddressableSectors: Cardinal;           //     60-61

//USHORT SingleWordDMASupport : 8;        //     62
//USHORT SingleWordDMAActive : 8;
//USHORT MultiWordDMASupport : 8;         //     63
//USHORT MultiWordDMAActive : 8;
//USHORT AdvancedPIOModes : 8;            //     64
//USHORT Reserved4 : 8;
    SingleWordDMASupport: Word;        //     62 //SingleWordDMAActive is in the second byte
    MultiWordDMASupport: Word;         //     63 //MultiWordDMAActive is in the second byte
    AdvancedPIOModes: Word;            //     64 //Reserved4 is in the second byte

    MinimumMWXferCycleTime: Word;           //     65
    RecommendedMWXferCycleTime: Word;       //     66
    MinimumPIOCycleTime: Word;              //     67
    MinimumPIOCycleTimeIORDY: Word;         //     68
    Reserved5: array [0..2-1] of  Word;                     //     69-70
    ReleaseTimeOverlapped: Word;            //     71
    ReleaseTimeServiceCommand: Word;        //     72
    MajorRevision: Word;                    //     73
    MinorRevision: Word;                    //     74
    Reserved6: array [0..50-1] of  Word;                    //     75-126
    SpecialFunctionsEnabled: Word;          //     127
    Reserved7: array [0..128-1] of  Word;                   //     128-255
end;
PIDENTIFY_DATA = ^IDENTIFY_DATA;

{$ALIGN on}//#pragma pack()



function ReadPhysicalDriveInNTUsingSmart(var Dest: tresults_array_dv): Boolean;
var
  drive: Integer;
  hPhysicalDriveIOCTL: THandle;
  driveName: array [0..256-1] of Ansichar;

  GetVersionParams: GETVERSIONINPARAMS;
  cbBytesReturned: Cardinal;

  CommandSize: ULONG;
  Command: PSENDCMDINPARAMS;

  BytesReturned: Cardinal;

  diskdata: tdiskdata_dv;
  ijk: Integer;
  pIdSector: PWord;

  count_drives_dv: Integer;
const
  ID_CMD = $EC; // Returns ID sector for ATA

begin
   SetLength(Dest, MAX_IDE_DRIVES-1);
   count_drives_dv := 0;
   for drive := 0 to MAX_IDE_DRIVES-1 do begin
      //-hPhysicalDriveIOCTL := 0;

         //  Try to get a handle to PhysicalDrive IOCTL, report failure
         //  and exit if can't.
      //-char driveName [256];

      //=sprintf (driveName, "\\\\.\\PhysicalDrive%d", drive);
      StrCopy(driveName, StringToPAnsiChar(Format('\\.\PhysicalDrive%d', [drive])));

      //  Windows NT, Windows 2000, Windows Server 2003, Vista
      hPhysicalDriveIOCTL := CreateFileA (driveName, GENERIC_READ or GENERIC_WRITE
        , FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      // if (hPhysicalDriveIOCTL == INVALID_HANDLE_VALUE)
      //    printf ("Unable to open physical drive %d, error code: 0x%lX'+#$0D#$0A+'",
      //            drive, GetLastError ());

      if (hPhysicalDriveIOCTL = INVALID_HANDLE_VALUE) then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
        if (PRINT_DEBUG) then begin
          debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTUsingSmart ERROR' +
					  #$0D#$0A+'CreateFile(%s) returned INVALID_HANDLE_VALUE'+#$0D#$0A+'Error Code %d'+#$0D#$0A
            ,  [0 //__LINE__
                , driveName, GetLastError ()]));
        end;
{$ENDIF}
      end else begin
         //-GETVERSIONINPARAMS GetVersionParams;
         cbBytesReturned := 0;

            // Get the version, etc of PhysicalDrive IOCTL
         FillMemory (@GetVersionParams, sizeof(GetVersionParams), 0);

         if (not DeviceIoControl (hPhysicalDriveIOCTL, SMART_GET_VERSION, nil, 0,
     			   @GetVersionParams, sizeof (GETVERSIONINPARAMS), cbBytesReturned, nil) )
         then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
          if (PRINT_DEBUG) then begin
	          debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTUsingSmart ERROR' +
		               #$0D#$0A+'DeviceIoControl(%d, SMART_GET_VERSION) returned 0, error is %d'+#$0D#$0A
                   , [0 //__LINE__
                      , Integer(hPhysicalDriveIOCTL), Integer(GetLastError)]));
		      end
{$ENDIF}
         end else begin
			 	// Print the SMART version
           	// PrintVersion (& GetVersionParams);
	           // Allocate the command buffer
    			CommandSize := sizeof(SENDCMDINPARAMS) + IDENTIFY_BUFFER_SIZE;
        	GetMem(PSENDCMDINPARAMS(Command), CommandSize);
          try
	           // Retrieve the IDENTIFY data
	           // Prepare the command
//-#define ID_CMD          0xEC            // Returns ID sector for ATA
            Command^.irDriveRegs.bCommandReg := ID_CMD;
            BytesReturned := 0;
            if (not DeviceIoControl (hPhysicalDriveIOCTL, SMART_RCV_DRIVE_DATA, Command, sizeof(SENDCMDINPARAMS),
                    Command, CommandSize, BytesReturned, nil) ) then begin
              // Print the error
              // PrintError ("SMART_RCV_DRIVE_DATA IOCTL", GetLastError());
            end else begin
              // Print the IDENTIFY data
              //-DWORD diskdata [256];

              //=USHORT *pIdSector = (USHORT *)(PIDENTIFY_DATA) ((PSENDCMDOUTPARAMS) Command) -> bBuffer;
              pIdSector := PWord(PIDENTIFY_DATA(@PSENDCMDOUTPARAMS(Command)^.bBuffer[0])); //!TOCHECK


              for ijk := 0 to 256-1 do begin
                diskdata [ijk] := parray_of_words256_dv(pIdSector)[ijk];
              end;

              Dest[count_drives_dv] := PrintIdeInfo (drive, diskdata);
              inc(count_drives_dv);
            end;
            // Done
            CloseHandle(hPhysicalDriveIOCTL);
          finally
		    	  FreeMem(Command, CommandSize);
          end;
  		 end
     end
   end;

   SetLength(Dest, count_drives_dv);
   Result := count_drives_dv > 0;
end;



//  Required to ensure correct PhysicalDrive IOCTL structure setup
{$ALIGN 4}

//
// IOCTL_STORAGE_QUERY_PROPERTY
//
// Input Buffer:
//      a STORAGE_PROPERTY_QUERY structure which describes what type of query
//      is being done, what property is being queried for, and any additional
//      parameters which a particular property query requires.
//
//  Output Buffer:
//      Contains a buffer to place the results of the query into.  Since all
//      property descriptors can be cast into a STORAGE_DESCRIPTOR_HEADER,
//      the IOCTL can be called once with a small buffer then again using
//      a buffer as large as the header reports is necessary.
//


//
// Types of queries
//

type
{$Z4} //size of each enumeration type should be equal 4
STORAGE_QUERY_TYPE = (
    PropertyStandardQuery = 0,          // Retrieves the descriptor
    PropertyExistsQuery,                // Used to test whether the descriptor is supported
    PropertyMaskQuery,                  // Used to retrieve a mask of writeable fields in the descriptor
    PropertyQueryMaxDefined     // use to validate the value
);
{$Z1}

//
// define some initial property id's
//
{$Z4} //size of each enumeration type should be equal 4
STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
{$Z1}

//
// Query structure - additional parameters for specific queries can follow
// the header
//

type
	STORAGE_PROPERTY_QUERY = record
    //
    // ID of the property being retrieved
    //
    PropertyId: STORAGE_PROPERTY_ID;
    //
    // Flags indicating the type of query being performed
    //
    QueryType: STORAGE_QUERY_TYPE;
    //
    // Space for additional parameters if necessary
    //
    AdditionalParameters: array [0..1-1] of UCHAR;
end;
{$ALIGN on}
PSTORAGE_PROPERTY_QUERY = ^STORAGE_PROPERTY_QUERY;

var IOCTL_STORAGE_QUERY_PROPERTY: Integer; //initialization in initialize-section


//
// Device property descriptor - this is really just a rehash of the inquiry
// data retrieved from a scsi device
//
// This may only be retrieved from a target device.  Sending this to the bus
// will result in an error
//

{$ALIGN 4}
type
  STORAGE_DEVICE_DESCRIPTOR = record
    // Sizeof(STORAGE_DEVICE_DESCRIPTOR)
    Version: Cardinal;
    // Total size of the descriptor, including the space for additional
    // data and id strings
    Size: Cardinal;
    // The SCSI-2 device type
    DeviceType: Byte;
    // The SCSI-2 device type modifier (if any) - this may be zero
    DeviceTypeModifier: Byte;
    // Flag indicating whether the device's media (if any) is removable.  This
    // field should be ignored for media-less devices
    RemovableMedia: Byte;
    // Flag indicating whether the device can support mulitple outstanding
    // commands.  The actual synchronization in this case is the responsibility
    // of the port driver.
    CommandQueueing: Byte;
    // Byte offset to the zero-terminated ascii string containing the device's
    // vendor id string.  For devices with no such ID this will be zero
    VendorIdOffset: Cardinal;
    // Byte offset to the zero-terminated ascii string containing the device's
    // product id string.  For devices with no such ID this will be zero
    ProductIdOffset: Cardinal;
    // Byte offset to the zero-terminated ascii string containing the device's
    // product revision string.  For devices with no such string this will be
    // zero
    ProductRevisionOffset: Cardinal;
    // Byte offset to the zero-terminated ascii string containing the device's
    // serial number.  For devices with no serial number this will be zero
    SerialNumberOffset: Cardinal;
    // Contains the bus type (as defined above) of the device.  It should be
    // used to interpret the raw device properties at the end of this structure
    // (if any)
    BusType: STORAGE_BUS_TYPE;
    // The number of bytes of bus-specific data which have been appended to
    // this descriptor
    RawPropertiesLength: Cardinal;
    // Place holder for the first byte of the bus specific property data
    RawDeviceProperties: array [0..1-1] of Byte;
end;
PSTORAGE_DEVICE_DESCRIPTOR = ^STORAGE_DEVICE_DESCRIPTOR;
{$ALIGN on}


	//  function to decode the serial numbers of IDE hard drives
	//  using the IOCTL_STORAGE_QUERY_PROPERTY command
function flipAndCodeBytes (str: PAnsiChar; pos: Integer; flip: Integer; buf: PAnsiChar): AnsiString;
var i, j, k: Integer;
    p: Integer;
    c: AnsiChar;
    t: AnsiChar;
begin
   j := 0;
   k := 0;

   buf [0] := AnsiChar(0);
   if (pos <= 0) then begin
      Result := buf;
      exit;
   end;

   if (j = 0) then begin
      p := 0;

      // First try to gather all characters representing hex digits only.
      j := 1;
      k := 0;
      buf[k] := AnsiChar(0);
      i := pos;
      while (j <> 0) and (str[i] <> AnsiChar(0)) do begin
        c := tolower(str[i]);

    	  if (isspace(c)) then c := AnsiChar(0);

    	  inc(p);
    	  buf[k] :=  AnsiChar(Ord(buf[k]) shl 4);

        if ((c >= '0') and (c <= '9'))
          then buf[k] := AnsiChar(Ord(buf[k]) or Byte(Ord(c) - Ord('0')))
      	  else if ((c >= 'a') and (c <= 'f'))
            then buf[k] := AnsiChar(Ord(buf[k]) or Byte(Ord(c) - Ord('a') + 10))
	          else begin
              j := 0;
	            break;
      	    end;

	      if (p = 2) then begin
    	    if ((buf[k] <> AnsiChar(0)) and (not isprint(buf[k]))) then begin
    	       j := 0;
	           break;
    	    end;
	        inc(k);
  	      p := 0;
	        buf[k] := AnsiChar(0);
  	    end;
        inc(i);
      end;
   end;

   if (j = 0) then begin
      // There are non-digit characters, gather them as is.
      j := 1;
      k := 0;
      i := pos;
      while ( (j <> 0) and (str[i] <> AnsiChar(0)) ) do begin
        c := str[i];

	      if ( not isprint(c)) then begin
	        j := 0;
	        break;
	      end;

	      buf[(k)] := c;
        inc(k);
        inc(i);
      end;
   end;

   if (j = 0) then begin
      // The characters are not there or are not printable.
      k := 0;
   end;

   buf[k] := AnsiChar(0);

   if (flip <> 0) then begin
      // Flip adjacent characters
      j := 0;
      while (j < k) do begin
        t := buf[j];
	      buf[j] := buf[j + 1];
	      buf[j + 1] := t;
        j := j + 2;
      end
   end;

   // Trim any beginning and end space
   i := -1;
   j := -1;
   k := 0;
   while (buf[k] <> AnsiChar(0)) do begin
      if (not isspace(buf[k])) then begin
        if (i < 0) then i := k;
	      j := k;
      end;
      inc(k);
   end;

   if ((i >= 0) and (j >= 0)) then begin
      k := i;
      while ( ( k <= j) and (buf[k] <> AnsiChar(0)) ) do begin
         buf[k - i] := buf[k];
         inc(k);
      end;
      buf[k - i] := AnsiChar(0);
   end;

   Result := buf;
end;

var
  IOCTL_DISK_GET_DRIVE_GEOMETRY_EX: Integer;

type
  DISK_GEOMETRY_EX = record
    Geometry: DISK_GEOMETRY;
    DiskSize: Int64; //LARGE_INTEGER
    Data: array [0..1-1] of Byte;
  end;
  PDISK_GEOMETRY_EX = ^DISK_GEOMETRY_EX;

function ReadPhysicalDriveInNTWithZeroRights(var Dest: tresults_array_dv): Boolean;
var
  drive: Integer;
  hPhysicalDriveIOCTL: THandle;
  driveName: array [0..256-1] of ansichar;

	query: STORAGE_PROPERTY_QUERY;
  cbBytesReturned: Cardinal;
  buffer: array [0..10000-1] of ansichar;

  serialNumber: array [0..10000-1] of ansichar;
	modelNumber: array [0..10000-1] of ansichar;
  vendorId: array [0..10000-1] of ansichar;
	productRevision: array [0..10000-1] of ansichar;

  descrip: PSTORAGE_DEVICE_DESCRIPTOR;

  geom: PDISK_GEOMETRY_EX;
	fixed: AnsiString; //=Integer;
  size: Int64;

  count_drives_dv: Integer;
  found_drive_id_dv: Integer;
begin
   SetLength(Dest, MAX_IDE_DRIVES-1);
   count_drives_dv := 0;
  for drive := 0 to MAX_IDE_DRIVES-1 do begin
    found_drive_id_dv := -1;
    //-hPhysicalDriveIOCTL := 0;

         //  Try to get a handle to PhysicalDrive IOCTL, report failure
         //  and exit if can't.
      //-char driveName [256];

      //=sprintf (driveName, "\\\\.\\PhysicalDrive%d", drive);
      StrCopy(driveName, StringToPAnsiChar(Format('\\.\PhysicalDrive%d', [drive])));

         //  Windows NT, Windows 2000, Windows XP - admin rights not required
    hPhysicalDriveIOCTL := CreateFileA (driveName, 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if (hPhysicalDriveIOCTL = INVALID_HANDLE_VALUE) then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
      if (PRINT_DEBUG) then begin
        debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTWithZeroRights ERROR' +
		             #$0D#$0A+'CreateFile(%s) returned INVALID_HANDLE_VALUE'+#$0D#$0A
                 , [0 //__LINE__
                  , driveName]));
      end;
{$ENDIF}
    end else begin
		  //-STORAGE_PROPERTY_QUERY query;
      cbBytesReturned := 0;
		  //-char buffer [10000];

      FillMemory(@query, sizeof (query), 0);
      query.PropertyId := StorageDeviceProperty;
		  query.QueryType := PropertyStandardQuery;

      FillMemory(@buffer, sizeof (buffer), 0);

      if ( DeviceIoControl (hPhysicalDriveIOCTL, IOCTL_STORAGE_QUERY_PROPERTY, @query, sizeof (query),
				   @buffer, sizeof (buffer), cbBytesReturned, nil) )
      then begin
			  descrip := PSTORAGE_DEVICE_DESCRIPTOR(@buffer);
			  //-char serialNumber [1000];
			  //-char modelNumber [1000];
        //-char vendorId [1000];
	      //-char productRevision [1000];

{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
             if (PRINT_DEBUG) then begin
                 debugar(Format(#$0D#$0A+'%d STORAGE_DEVICE_DESCRIPTOR contents for drive %d'+#$0D#$0A
		                 +'                Version: %s'+#$0D#$0A
		                 +'                   Size: %d'+#$0D#$0A
		                 +'             DeviceType: %02x'+#$0D#$0A
		                 +'     DeviceTypeModifier: %02x'+#$0D#$0A
		                 +'         RemovableMedia: %d'+#$0D#$0A
		                 +'        CommandQueueing: %d'+#$0D#$0A
		                 +'         VendorIdOffset: %x'+#$0D#$0A
		                 +'        ProductIdOffset: %x'+#$0D#$0A
		                 +'  ProductRevisionOffset: %x'+#$0D#$0A
		                 +'     SerialNumberOffset: %x'+#$0D#$0A
		                 +'                BusType: %d'+#$0D#$0A
		                 +'    RawPropertiesLength: %s'+#$0D#$0A,
		                 [0//__LINE__
                     , drive
                     , IntToStr(Cardinal(descrip^.Version))
                     , Cardinal(descrip^.Size),
		                 Integer(descrip^.DeviceType),
		                 Integer(descrip^.DeviceTypeModifier),
		                 Integer(descrip^.RemovableMedia),
	                   Integer(descrip^.CommandQueueing),
		                 Cardinal(descrip^.VendorIdOffset),
		                 Cardinal( descrip^.ProductIdOffset),
		                 Cardinal( descrip^.ProductRevisionOffset),
		                 Cardinal( descrip^.SerialNumberOffset),
		                 Integer(descrip^.BusType),
		                 IntToStr(Cardinal( descrip^.RawPropertiesLength))]));

//-	            dump_buffer ('Contents of RawDeviceProperties',
//-			                 (unsigned char*) descrip^.RawDeviceProperties,
//-			                 descrip^.RawPropertiesLength);

//-	            dump_buffer ('Contents of first 256 bytes in buffer',
//-			                 (unsigned char*) buffer, 256);
			 end;
{$ENDIF}
        flipAndCodeBytes (buffer, descrip^.VendorIdOffset, 0, vendorId);
        flipAndCodeBytes (buffer, descrip^.ProductIdOffset, 0, modelNumber );
	      flipAndCodeBytes (buffer, descrip^.ProductRevisionOffset, 0, productRevision );
        flipAndCodeBytes (buffer, descrip^.SerialNumberOffset, 1, serialNumber);

			  if ( (AnsiChar(0) = HardDriveSerialNumber [0]) and
						//  serial number must be alphanumeric
			            //  (but there can be leading spaces on IBM drives)
				   (isalnum (serialNumber [0]) or isalnum (serialNumber [19])))
        then begin
				  StrCopy(HardDriveSerialNumber, serialNumber);
				  StrCopy(HardDriveModelNumber, modelNumber);
          Dest[count_drives_dv].ControllerType := 0; //unknown
          Dest[count_drives_dv].DriveMS := 0; //unknown
          Dest[count_drives_dv].DriveModelNumber := HardDriveModelNumber;
          Dest[count_drives_dv].DriveSerialNumber := HardDriveSerialNumber;
          Dest[count_drives_dv].DriveControllerRevisionNumber := ''; //unknown
          Dest[count_drives_dv].ControllerBufferSizeOnDrive := 0; //unknown
          Dest[count_drives_dv].DriveSizeBytes := 0; //unknown
          Dest[count_drives_dv].DriveType := 'Unknown';
          found_drive_id_dv := count_drives_dv;
          inc(count_drives_dv);
			  end;
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
           debugar(Format(#$0D#$0A+'**** STORAGE_DEVICE_DESCRIPTOR for drive %d ****'+#$0D#$0A+
               'Vendor Id = [%s]'+#$0D#$0A+
               'Product Id = [%s]'+#$0D#$0A+
               'Product Revision = [%s]'+#$0D#$0A+
               'Serial Number = [%s]'+#$0D#$0A,
               [drive,
               vendorId,
               modelNumber,
               productRevision,
               serialNumber]));
{$ENDIF}
           // Get the disk drive geometry.
        FillMemory(@buffer, sizeof(buffer), 0);
        if (not DeviceIoControl (hPhysicalDriveIOCTL,
                IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
                nil,
                0,
                @buffer,
                sizeof(buffer),
                cbBytesReturned,
                nil))
        then begin
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
          if (PRINT_DEBUG) then begin
            debugar(Format(#$0D#$0A+'%d ReadPhysicalDriveInNTWithZeroRights ERROR' +
              '|nDeviceIoControl(%s, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX) returned 0'
              , [0//__LINE__
                ,driveName]));
         end;
{$ENDIF}
        end else begin
            geom := PDISK_GEOMETRY_EX(@buffer);
            if (geom^.Geometry.MediaType = FixedMedia)
              then fixed := 'fixed'
              else fixed := 'removable';
            size := geom^.DiskSize; //-.QuadPart;
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
              debugar(Format (#$0D#$0A+'**** DISK_GEOMETRY_EX for drive %d ****'+#$0D#$0A +
                    'Disk is%s fixed'+#$0D#$0A +
                    'DiskSize = %s'+#$0D#$0A,
                    [drive, fixed, IntToStr(size)]));
{$ENDIF}
            if found_drive_id_dv <> -1 then begin
              Dest[count_drives_dv].DriveSizeBytes := size;
              Dest[count_drives_dv].DriveType := fixed;
            end;
        end;
      end else begin
        //DWORD err = GetLastError ();
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
      debugar(Format (#$0D#$0A+'DeviceIOControl IOCTL_STORAGE_QUERY_PROPERTY error = %d'+#$0D#$0A
        , [Integer(GetLastError)]));
{$ENDIF}
		  end;
      CloseHandle (hPhysicalDriveIOCTL);
    end;
  end;

  SetLength(Dest, count_drives_dv);
  Result := count_drives_dv > 0;
end;


   // DoIDENTIFY
   // FUNCTION: Send an IDENTIFY command to the drive
   // bDriveNum = 0-3
   // bIDCmd = IDE_ATA_IDENTIFY or IDE_ATAPI_IDENTIFY
function DoIDENTIFY (hPhysicalDriveIOCTL: THandle; pSCIP: PSENDCMDINPARAMS;
                 pSCOP: PSENDCMDOUTPARAMS; bIDCmd: Byte; bDriveNum: Byte;
                 lpcbBytesReturned: PCardinal): Integer; //BOOL
begin
      // Set up data structures for IDENTIFY command.
   pSCIP^.cBufferSize := IDENTIFY_BUFFER_SIZE;
   pSCIP^.irDriveRegs.bFeaturesReg := 0;
   pSCIP^.irDriveRegs.bSectorCountReg := 1;
   //pSCIP ^. irDriveRegs.bSectorNumberReg = 1;
   pSCIP^.irDriveRegs.bCylLowReg := 0;
   pSCIP^.irDriveRegs.bCylHighReg := 0;

      // Compute the drive number.
   pSCIP^.irDriveRegs.bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);

      // The command can either be IDE identify or ATAPI identify.
   pSCIP^.irDriveRegs.bCommandReg := bIDCmd;
   pSCIP^.bDriveNumber := bDriveNum;
   pSCIP^.cBufferSize := IDENTIFY_BUFFER_SIZE;

   Result := Integer(DeviceIoControl (hPhysicalDriveIOCTL, DFP_RECEIVE_DRIVE_DATA
      , Pointer(pSCIP)
      , sizeof(SENDCMDINPARAMS) - 1
      , Pointer(pSCOP)
      , sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1
      , lpcbBytesReturned^
      , nil));
end;


//  ---------------------------------------------------
// (* Output Bbuffer for the VxD (rt_IdeDinfo record) *)
type
  rt_IdeDInfo = record
    IDEExists: array [0..4-1] of Byte;
    DiskExists: array [0..8-1] of Byte;
    DisksRawInfo: array[0..8*256-1] of Word;
  end;
  pt_IdeDInfo = ^rt_IdeDInfo;

   // (* IdeDinfo 'data fields' *)
  rt_DiskInfo = record
   DiskExists: Integer;//BOOL;
   ATAdevice: Integer;//BOOL;
   RemovableDevice: Integer;//BOOL;
   TotLogCyl: WORD;
   TotLogHeads: WORD;
   TotLogSPT: WORD;
   SerialNumber: array [0..20-1] of AnsiChar;
   FirmwareRevision: array [0..8-1] of AnsiChar;
   ModelNumber: array [0..40-1] of AnsiChar;
   CurLogCyl: WORD;
   CurLogHeads: WORD;
   CurLogSPT: WORD;
  end;

const m_cVxDFunctionIdesDInfo = 1;


//  ---------------------------------------------------


function ReadDrivePortsInWin9X(var Dest: tresults_array_dv): Boolean;
var
  i: Cardinal;
  VxDHandle: THandle;
  pOutBufVxD: pt_IdeDInfo;
  lpBytesReturned: DWORD;
  status: LongBool; //BOOL
  info: rt_IdeDInfo;

  diskinfo: tdiskdata_dv;
  j: Integer;

  count_drives_dv: Integer;
begin
//  assert(false, 'This code wasn'' tested!');

  //-VxDHandle := 0;
  //-pOutBufVxD := nil;
  lpBytesReturned := 0;

		//  set the thread priority high so that we get exclusive access to the disk
  status :=
		// SetThreadPriority (GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
    SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
		// SetPriorityClass (GetCurrentProcess (), HIGH_PRIORITY_CLASS);

{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
   if (not status) then begin
	   // Write(Format (#$0D#$0A+'ERROR: Could not SetThreadPriority, LastError: %d'+#$0D#$0A, GetLastError ());
	   debugar(Format (#$0D#$0A+'ERROR: Could not SetPriorityClass, LastError: %d'+#$0D#$0A, [GetLastError]));
   end;
{$ENDIF}

      // 1. Make an output buffer for the VxD
   //-rt_IdeDInfo info;
   pOutBufVxD := @info;

      // *****************
      // KLUDGE WARNING!!!
      // HAVE to zero out the buffer space for the IDE information!
      // If this is NOT done then garbage could be in the memory
      // locations indicating if a disk exists or not.
   ZeroMemory (@info, sizeof(info));

      // 1. Try to load the VxD
       //  must use the short file name path to open a VXD file
   //char StartupDirectory [2048];
   //char shortFileNamePath [2048];
   //char *p = nil;
   //char vxd [2048];
      //  get the directory that the exe was started from
   //GetModuleFileName (hInst, (LPSTR) StartupDirectory, sizeof (StartupDirectory));
      //  cut the exe name from string
   //p = &(StartupDirectory [strlen (StartupDirectory) - 1]);
   //while (p >= StartupDirectory && *p && '\\' != *p) p--;
   //*p = '\0';
   //GetShortPathName (StartupDirectory, shortFileNamePath, 2048);
   //sWrite(Format (vxd, '\\\\.\\%s\\IDE21201.VXD', shortFileNamePath);
   //VxDHandle = CreateFile (vxd, 0, 0, 0,
   //               0, FILE_FLAG_DELETE_ON_CLOSE, 0);
  VxDHandle := CreateFile ('\\.\IDE21201.VXD', 0, 0, nil, 0, FILE_FLAG_DELETE_ON_CLOSE, 0);

  if (VxDHandle <> INVALID_HANDLE_VALUE) then begin
         // 2. Run VxD function
      DeviceIoControl(VxDHandle, m_cVxDFunctionIdesDInfo, nil, 0, pOutBufVxD, sizeof(pt_IdeDInfo), lpBytesReturned, nil);

         // 3. Unload VxD
      CloseHandle(VxDHandle);
  end else begin
		  MessageBox(0, 'ERROR: Could not open IDE21201.VXD file', TITLE, MB_ICONSTOP);
  end;

      // 4. Translate and store data
   SetLength(Dest, 8-1);
   count_drives_dv := 0;

  for i := 0 to 8-1 do begin
    if ( ((pOutBufVxD^.DiskExists[i]) and (pOutBufVxD^.IDEExists[i div 2])) <> 0) then begin
      //-DWORD diskinfo [256];
			for j := 0 to 256-1 do begin
        diskinfo [j] := pOutBufVxD^.DisksRawInfo [i * 256 + j];
      end;

            // process the information for this buffer
      Dest[count_drives_dv] := PrintIdeInfo (i, diskinfo);
      inc(count_drives_dv);
    end;
  end;

		//  reset the thread priority back to normal
   // SetThreadPriority (GetCurrentThread(), THREAD_PRIORITY_NORMAL);
   SetPriorityClass (GetCurrentProcess(), NORMAL_PRIORITY_CLASS);

   SetLength(Dest, count_drives_dv);
   Result := count_drives_dv > 0;
end;


const SENDIDLENGTH = sizeof (SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE;


function ReadIdeDriveAsScsiDriveInNT(var Dest: tresults_array_dv): Boolean;
var
  controller: Integer;
  hScsiDriveIOCTL: THandle;
  driveName: array [0..256-1] of AnsiChar;

  drive: Integer;

  buffer: array [0..sizeof (SRB_IO_CONTROL) + SENDIDLENGTH - 1] of AnsiChar;
  p: PSRB_IO_CONTROL;
  pin: PSENDCMDINPARAMS;
  dummy: DWORD;

  pOut: PSENDCMDOUTPARAMS;
  pId: PIDSECTOR;
  diskdata: tdiskdata_dv;
  ijk: Integer;
  pIdSectorPtr: PWord;

  count_drives_dv: Integer;
begin
   SetLength(Dest, 16-1);
   count_drives_dv := 0;
   for controller := 0 to 16-1 do begin
      //-hScsiDriveIOCTL := 0;
      //-char   driveName [256];

         //  Try to get a handle to PhysicalDrive IOCTL, report failure
         //  and exit if can't.
      //=sWrite(Format (driveName, '\\\\.\\Scsi%d:', controller);
      StrCopy(driveName, StringToPAnsiChar(Format('\\.\Scsi%d:', [controller])));

         //  Windows NT, Windows 2000, any rights should do
      hScsiDriveIOCTL := CreateFileA (driveName,
                               GENERIC_READ or GENERIC_WRITE,
                               FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                               OPEN_EXISTING, 0, 0);
      // if (hScsiDriveIOCTL == INVALID_HANDLE_VALUE)
      //    Write(Format ('Unable to open SCSI controller %d, error code: 0x%lX'+#$0D#$0A,
      //            controller, GetLastError ());

      if (hScsiDriveIOCTL <> INVALID_HANDLE_VALUE) then begin
         //-drive := 0;

         for drive := 0 to 2-1 do begin
            //-char buffer [sizeof (SRB_IO_CONTROL) + SENDIDLENGTH];
            p := PSRB_IO_CONTROL(@buffer);
            pin := PSENDCMDINPARAMS(buffer + sizeof (SRB_IO_CONTROL));
            //-DWORD dummy;

            FillMemory(@buffer, sizeof(buffer), 0);
            p^.HeaderLength := sizeof (SRB_IO_CONTROL);
            p^.Timeout := 10000;
            p^.Length := SENDIDLENGTH;
            p^.ControlCode := IOCTL_SCSI_MINIPORT_IDENTIFY;
            StrLCopy(PAnsiChar(@p^.Signature), 'SCSIDISK', 8);

            pin^.irDriveRegs.bCommandReg := IDE_ATA_IDENTIFY;
            pin^.bDriveNumber := drive;

            if (DeviceIoControl (hScsiDriveIOCTL, IOCTL_SCSI_MINIPORT,
                                 @buffer,
                                 sizeof (SRB_IO_CONTROL) + sizeof (SENDCMDINPARAMS) - 1,
                                 @buffer,
                                 sizeof (SRB_IO_CONTROL) + SENDIDLENGTH,
                                 dummy, nil))
            then begin
               pOut := PSENDCMDOUTPARAMS(buffer + sizeof (SRB_IO_CONTROL)); //!TOCHECK
               pId := PIDSECTOR(@pOut^.bBuffer[0]);
               if (pId^.sModelNumber[0] <> AnsiChar(0) ) then begin
                  //-DWORD diskdata [256];
                  //-ijk := 0;
                  pIdSectorPtr := PWord(pId);

                  for ijk := 0 to 256-1 do begin
                     diskdata[ijk] := parray_of_words256_dv(pIdSectorPtr)[ijk];
                  end;

                  Dest[count_drives_dv] := PrintIdeInfo (controller * 2 + drive, diskdata);
                  inc(count_drives_dv);
               end;
            end;
         end;
         CloseHandle(hScsiDriveIOCTL);
      end;
   end;

   SetLength(Dest, count_drives_dv);
   Result := count_drives_dv > 0;
end;


function PrintIdeInfo (drive: Integer; diskdata: tdiskdata_dv): tresults_dv;
var
   serialNumber: array [0..1024-1] of Ansichar;
   modelNumber: array [0..1024-1] of Ansichar;
   revisionNumber: array [0..1024-1] of Ansichar;
   //-bufferSize: array [0..32-1] of char;

   sectors: Int64;
//-   bytes: Int64;
begin
//-   char serialNumber [1024];
//-   char modelNumber [1024];
//-   char revisionNumber [1024];
//-   char bufferSize [32];

      //  copy the hard drive serial number to the buffer
   ConvertToString (diskdata, 10, 19, @serialNumber);
   ConvertToString (diskdata, 27, 46, @modelNumber);
   ConvertToString (diskdata, 23, 26, @revisionNumber);
   //-sWrite(Format (bufferSize, '%u', diskdata [21] * 512);

   if ((AnsiChar(0) = HardDriveSerialNumber[0]) and
       //  serial number must be alphanumeric
       //  (but there can be leading spaces on IBM drives)
       (isalnum (serialNumber [0]) or isalnum (serialNumber [19])))
   then begin
      StrCopy(PAnsiCHAR(@HardDriveSerialNumber), PAnsiCHAR(@serialNumber));
      StrCopy(PAnsiCHAR(@HardDriveModelNumber), PAnsiCHAR(@modelNumber));
   end;
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}

   debugar(Format (#$0D#$0A+'Drive %d - ', [drive]));

   case (drive div 2) of
      0: debugar('Primary Controller - ');
      1: debugar('Secondary Controller - ');
      2: debugar('Tertiary Controller - ');
      3: debugar('Quaternary Controller - ');
   end;

   case (drive mod 2) of
      0: debugar(' - Master drive');
      1: debugar(' - Slave drive');
   end;

   debugar(Format ('Drive Model Number________________: [%s]'+#$0D#$0A, [modelNumber]));
   debugar(Format ('Drive Serial Number_______________: [%s]'+#$0D#$0A, [serialNumber]));
   debugar(Format ('Drive Controller Revision Number__: [%s]'+#$0D#$0A, [revisionNumber]));

//-   Write(Format ('Controller Buffer Size on Drive___: %s bytes'+#$0D#$0A, [bufferSize]));

   debugar('Drive Type________________________: ');
   if (0 <> (diskdata [0] and $0080)) then debugar('Removable')
   else if (0 <> (diskdata [0] and $0040)) then debugar('Fixed')
   else debugar('Unknown');

		//  calculate size based on 28 bit or 48 bit addressing
		//  48 bit addressing is reflected by bit 10 of word 83
	if (0 <> (diskdata [83] and $400)) then begin
		sectors := diskdata [103] * Int64(65536) * Int64(65536) * Int64(65536) +
					diskdata [102] * Int64(65536) * Int64(65536) +
					diskdata [101] * Int64(65536) +
					diskdata [100];
  end else begin
		sectors := diskdata [61] * 65536 + diskdata [60];
  end;
		//  there are 512 bytes in a sector
	debugar(Format ('Drive Size________________________: %s bytes'+#$0D#$0A, [IntToStr(sectors * 512)]));

{$ENDIF}  // PRINTING_TO_CONSOLE_ALLOWED

(* TODO: we don't need this code (?)
   char string1 [1000];
   sWrite(Format (string1, 'Drive%dModelNumber', drive);
   WriteConstantString (string1, modelNumber);

   sWrite(Format (string1, 'Drive%dSerialNumber', drive);
   WriteConstantString (string1, serialNumber);

   sWrite(Format (string1, 'Drive%dControllerRevisionNumber', drive);
   WriteConstantString (string1, revisionNumber);

   sWrite(Format (string1, 'Drive%dControllerBufferSize', drive);
   WriteConstantString (string1, bufferSize);

   sWrite(Format (string1, 'Drive%dType', drive);
   if (diskdata [0] & 0x0080)
      WriteConstantString (string1, 'Removable');
   else if (diskdata [0] & 0x0040)
      WriteConstantString (string1, 'Fixed');
   else
      WriteConstantString (string1, 'Unknown');
*)

   Result.ControllerType := drive div 2;
   Result.DriveMS := drive mod 2;
   Result.DriveModelNumber := modelNumber;
   Result.DriveSerialNumber := serialNumber;
   Result.DriveControllerRevisionNumber := revisionNumber;
   Result.ControllerBufferSizeOnDrive := diskdata [21] * 512;
   if ((diskdata [0] and $0080) <> 0)
      then Result.DriveType := 'Removable'
      else if ((diskdata [0] and $0040) <> 0)
          then Result.DriveType := 'Fixed'
          else Result.DriveType := 'Unknown';
//  calculate size based on 28 bit or 48 bit addressing
//  48 bit addressing is reflected by bit 10 of word 83
  if ((diskdata[83] and $400) <> 0) then begin
	  sectors := diskdata[103] * Int64(65536) * Int64(65536) * Int64(65536) +
					diskdata[102] * Int64(65536) * Int64(65536) +
					diskdata[101] * Int64(65536) +
					diskdata[100];
  end else begin
		sectors := diskdata [61] * 65536 + diskdata [60];
  end;

//  there are 512 bytes in a sector
  Result.DriveSizeBytes := sectors * 512;
end;



function ConvertToString (diskdata: tdiskdata_dv;
		       firstIndex: Integer;
		       lastIndex: Integer;
		       buf: PAnsiChar): PAnsiChar;
var
   index: Integer;
   position: Integer;
begin
   position := 0;

      //  each integer has two characters stored in it backwards
   for index := firstIndex to lastIndex do begin
         //  get high byte for 1st character
      buf[position] := AnsiChar(diskdata [index] div 256);
      inc(position);

         //  get low byte for 2nd character
      buf [position] := AnsiChar(diskdata [index] mod 256);
      inc(position);
   end;

      //  end the string
   buf[position] := AnsiChar(0);

      //  cut off the trailing blanks
   index := position - 1;
   while (index >0) do begin
      if not isspace(buf [index]) then break;
      buf [index] := AnsiChar(0);
      dec(index);
   end;

   Result := buf;
end;


function getHardDriveComputerID(var Dest: tresults_array_dv): Longint;
var
  id: Int64;
  version: OSVERSIONINFOA;
  attempt: Integer;
  ip: Integer; //dv: index in array instead of original pointer
  done: Boolean;
begin
  // char string [1024];
  id := 0;
  //-OSVERSIONINFO version;

  StrCopy(HardDriveSerialNumber, '');

  FillMemory(@version, sizeof (version), 0);
  version.dwOSVersionInfoSize := sizeof (OSVERSIONINFOA);
  GetVersionExA(version);
  if (version.dwPlatformId = VER_PLATFORM_WIN32_NT) then begin
		  //  this works under WinNT4 or Win2K if you have admin rights
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
		debugar(#$0D#$0A+'Trying to read the drive IDs using physical access with admin rights'+#$0D#$0A);
{$ENDIF}
	  //-done :=
      ReadPhysicalDriveInNTWithAdminRights(Dest);
      if( (length(dest) > 0) and (length(trim(dest[0].DriveSerialNumber)) > 0) ) then
      begin
          exit;
      end;

			//  this should work in WinNT or Win2K if previous did not work
			//  this is kind of a backdoor via the SCSI mini port driver into
			//     the IDE drives
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
		debugar(#$0D#$0A+'Trying to read the drive IDs using the SCSI back door'+#$0D#$0A);
{$ENDIF}
		// if ( ! done)
    //-done :=
      ReadIdeDriveAsScsiDriveInNT(Dest);
      if( (length(dest) > 0) and (length(trim(dest[0].DriveSerialNumber)) > 0) ) then
      begin
          exit;
      end;

		  //  this works under WinNT4 or Win2K or WinXP if you have any rights
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
		debugar(#$0D#$0A+'Trying to read the drive IDs using physical access with zero rights'+#$0D#$0A);
{$ENDIF}
		//if ( ! done)
		//-done :=
      ReadPhysicalDriveInNTWithZeroRights(Dest);
      if( (length(dest) > 0) and (length(trim(dest[0].DriveSerialNumber)) > 0) ) then
      begin
          exit;
      end;

		  //  this works under WinNT4 or Win2K or WinXP or Windows Server 2003 or Vista if you have any rights
{$ifdef PRINTING_TO_CONSOLE_ALLOWED}
		debugar(#$0D#$0A+'Trying to read the drive IDs using Smart'+#$0D#$0A);
{$ENDIF}
		//if ( ! done)
    //-done :=
      ReadPhysicalDriveInNTUsingSmart(Dest);
      if( (length(dest) > 0) and (length(trim(dest[0].DriveSerialNumber)) > 0) ) then
      begin
          exit;
      end;
  end else begin
         //  this works under Win9X and calls a VXD
    attempt := 0;

    done := false;
         //  try this up to 10 times to get a hard drive serial number
    while ( (attempt < 10) and (not done) and (AnsiChar(0) = HardDriveSerialNumber[0]) ) do begin
      done := ReadDrivePortsInWin9X (Dest);
    end;
  end;

  if (Ord(HardDriveSerialNumber[0]) > 0) then begin
    ip := 0;

    //?WriteConstantString ('HardDriveSerialNumber', HardDriveSerialNumber);

         //  ignore first 5 characters from western digital hard drives if
         //  the first four characters are WD-W
    if ( 0 = StrLComp(HardDriveSerialNumber, 'WD-W', 4)) then inc(ip, 5);

    assert(sizeof(HardDriveSerialNumber) = 1024);
    while ((HardDriveSerialNumber[ip] <> AnsiChar(0)) and (ip < 1024))  do begin
      if ('-' = HardDriveSerialNumber[ip]) then
      begin
        inc(ip);
        continue;
      end;
      id := id * 10;

      case HardDriveSerialNumber[ip] of
        '0': id := id + 0;
        '1': id := id + 1;
        '2': id := id + 2;
        '3': id := id + 3;
        '4': id := id + 4;
        '5': id := id + 5;
        '6': id := id + 6;
        '7': id := id + 7;
        '8': id := id + 8;
        '9': id := id + 9;
        'a', 'A': id := id + 10;
        'b', 'B': id := id + 11;
        'c', 'C': id := id + 12;
        'd', 'D': id := id + 13;
        'e', 'E': id := id + 14;
        'f', 'F': id := id + 15;
        'g', 'G': id := id + 16;
        'h', 'H': id := id + 17;
        'i', 'I': id := id + 18;
        'j', 'J': id := id + 19;
        'k', 'K': id := id + 20;
        'l', 'L': id := id + 21;
        'm', 'M': id := id + 22;
        'n', 'N': id := id + 23;
        'o', 'O': id := id + 24;
        'p', 'P': id := id + 25;
        'q', 'Q': id := id + 26;
        'r', 'R': id := id + 27;
        's', 'S': id := id + 28;
        't', 'T': id := id + 29;
        'u', 'U': id := id + 30;
        'v', 'V': id := id + 31;
        'w', 'W': id := id + 32;
        'x', 'X': id := id + 33;
        'y', 'Y': id := id + 34;
        'z', 'Z': id := id + 35;
      end;
      inc(ip);
    end; //for
  end; //if

   id := id mod 100000000;
   if (nil <> StrPos(HardDriveModelNumber, 'IBM-')) then begin
      id := id + 300000000;
   end else if ( (nil <> StrPos(HardDriveModelNumber, 'MAXTOR')) or
            (nil <> StrPos (HardDriveModelNumber, 'Maxtor')) ) then begin
      id := id + 400000000;
   end else if (nil <> StrPos(HardDriveModelNumber, 'WDC ')) then begin
      id := id + 500000000;
   end else begin
      id := id + 600000000;
   end;

{$ifdef PRINTING_TO_CONSOLE_ALLOWED}

   debugar(Format (#$0D#$0A+'Hard Drive Serial Number__________: %s'+#$0D#$0A, [HardDriveSerialNumber]));
   debugar(Format (#$0D#$0A+'Hard Drive Model Number___________: %s'+#$0D#$0A, [HardDriveModelNumber]));
   debugar(Format (#$0D#$0A+'Computer ID_______________________: %s'+#$0D#$0A, [IntToStr(id)]));

{$ENDIF}
   Result := id;
end;

{$IFDEF DEBUG}
procedure test;
const
   SIZE_GETVERSIONINPARAMS = sizeof(GETVERSIONINPARAMS);
   SIZE_GETVERSIONOUTPARAMS = sizeof(GETVERSIONOUTPARAMS);
   SIZE_IDEREGS = sizeof(IDEREGS);
   SIZE_SENDCMDINPARAMS = sizeof(SENDCMDINPARAMS);
   SIZE_DRIVERSTATUS = sizeof(DRIVERSTATUS);
   SIZE_SENDCMDOUTPARAMS = sizeof(SENDCMDOUTPARAMS);
   SIZE_IDSECTOR = sizeof(IDSECTOR);
   SIZE_SRB_IO_CONTROL = sizeof(SRB_IO_CONTROL);
   SIZE_IdOutCmd = sizeof(IdOutCmd);
   SIZE_IDENTIFY_DATA = sizeof(IDENTIFY_DATA);
   SIZE_STORAGE_PROPERTY_QUERY = sizeof(STORAGE_PROPERTY_QUERY);
   SIZE_STORAGE_DEVICE_DESCRIPTOR = sizeof(STORAGE_DEVICE_DESCRIPTOR);
   SIZE_DISK_GEOMETRY_EX = sizeof(DISK_GEOMETRY_EX);
   SIZE_rt_IdeDInfo = sizeof(rt_IdeDInfo);
   SIZE_rt_DiskInfo = sizeof(rt_DiskInfo);
begin
		assert(SIZE_GETVERSIONINPARAMS = 24);
		assert(SMART_GET_VERSION = 475264);
		assert(SIZE_rt_IdeDInfo	=4108);
		assert(SIZE_STORAGE_PROPERTY_QUERY	=12);
		assert(SIZE_IDEREGS	=8);
		assert(SIZE_STORAGE_DEVICE_DESCRIPTOR	=40);
		assert(SIZE_rt_DiskInfo	=92);
		assert(SMART_SEND_DRIVE_COMMAND	=508036);
		assert(SIZE_GETVERSIONOUTPARAMS	=24);
		assert(SIZE_DRIVERSTATUS	=12);
		assert(SIZE_SENDCMDOUTPARAMS	=17);
		assert(SIZE_IdOutCmd	=528);
		assert(SIZE_SRB_IO_CONTROL	=28);
		assert(IOCTL_STORAGE_QUERY_PROPERTY	=2954240);
		assert(IOCTL_DISK_GET_DRIVE_GEOMETRY_EX	=458912);
		assert(SIZE_DISK_GEOMETRY_EX	=40);
		assert(SIZE_SENDCMDINPARAMS	=33);
		assert(SIZE_IDENTIFY_DATA	=508);
		assert(SMART_RCV_DRIVE_DATA =508040);
		assert(SIZE_IDSECTOR	=256);
end;
{$ENDIF}

procedure SetPrintDebugInfo(bOn: Boolean);
begin
  PRINT_DEBUG := bOn;
end;

initialization
  SMART_GET_VERSION := CTL_CODE(IOCTL_DISK_BASE, $0020, METHOD_BUFFERED, FILE_READ_ACCESS);
  SMART_SEND_DRIVE_COMMAND := CTL_CODE(IOCTL_DISK_BASE, $0021, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
  SMART_RCV_DRIVE_DATA := CTL_CODE(IOCTL_DISK_BASE, $0022, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
  IOCTL_STORAGE_QUERY_PROPERTY := CTL_CODE(IOCTL_STORAGE_BASE, $0500, METHOD_BUFFERED, FILE_ANY_ACCESS);
  IOCTL_DISK_GET_DRIVE_GEOMETRY_EX := CTL_CODE(IOCTL_DISK_BASE, $0028, METHOD_BUFFERED, FILE_ANY_ACCESS);
  PRINT_DEBUG := false;
end.
