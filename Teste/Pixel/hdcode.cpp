// ---------------------------------------------------------------------------

#pragma hdrstop

#include "hdcode.h"

// ---------------------------------------------------------------------------

#pragma package(smart_init)

// Required to ensure correct PhysicalDrive IOCTL structure setup
#pragma pack(1)

#define  IDENTIFY_BUFFER_SIZE  512

// IOCTL commands
#define  DFP_GET_VERSION          0x00074080
#define  DFP_SEND_DRIVE_COMMAND   0x0007c084
#define  DFP_RECEIVE_DRIVE_DATA   0x0007c088

#define  FILE_DEVICE_SCSI          0x0000001b
#define  IOCTL_SCSI_MINIPORT_IDENTIFY  ((FILE_DEVICE_SCSI << 16) + 0x0501)
#define  IOCTL_SCSI_MINIPORT 0x0004D008 //  see NTDDSCSI.H for definition

// GETVERSIONOUTPARAMS contains the data returned from the
// Get Driver Version function.
typedef struct _GETVERSIONOUTPARAMS {
	BYTE bVersion; // Binary driver version.
	BYTE bRevision; // Binary driver revision.
	BYTE bReserved; // Not used.
	BYTE bIDEDeviceMap; // Bit map of IDE devices.
	DWORD fCapabilities; // Bit mask of driver capabilities.
	DWORD dwReserved[4]; // For future use.
}GETVERSIONOUTPARAMS, *PGETVERSIONOUTPARAMS, *LPGETVERSIONOUTPARAMS;

// Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
#define  CAP_IDE_ID_FUNCTION         1  // ATA ID command supported
#define  CAP_IDE_ATAPI_ID         2  // ATAPI ID command supported
#define  CAP_IDE_EXECUTE_SMART_FUNCTION  4  // SMART commannds supported

// Valid values for the bCommandReg member of IDEREGS.
#define  IDE_ATAPI_IDENTIFY  0xA1  // Returns ID sector for ATAPI.
#define  IDE_ATA_IDENTIFY    0xEC  // Returns ID sector for ATA.

// The following struct defines the interesting part of the IDENTIFY
// buffer:
typedef struct _IDSECTOR {
	USHORT wGenConfig;
	USHORT wNumCyls;
	USHORT wReserved;
	USHORT wNumHeads;
	USHORT wBytesPerTrack;
	USHORT wBytesPerSector;
	USHORT wSectorsPerTrack;
	USHORT wVendorUnique[3];
	CHAR sSerialNumber[20];
	USHORT wBufferType;
	USHORT wBufferSize;
	USHORT wECCSize;
	CHAR sFirmwareRev[8];
	CHAR sModelNumber[40];
	USHORT wMoreVendorUnique;
	USHORT wDoubleWordIO;
	USHORT wCapabilities;
	USHORT wReserved1;
	USHORT wPIOTiming;
	USHORT wDMATiming;
	USHORT wBS;
	USHORT wNumCurrentCyls;
	USHORT wNumCurrentHeads;
	USHORT wNumCurrentSectorsPerTrack;
	ULONG ulCurrentSectorCapacity;
	USHORT wMultSectorStuff;
	ULONG ulTotalAddressableSectors;
	USHORT wSingleWordDMA;
	USHORT wMultiWordDMA;
	BYTE bReserved[128];
}IDSECTOR, *PIDSECTOR;

typedef struct _SRB_IO_CONTROL {
	ULONG HeaderLength;
	UCHAR Signature[8];
	ULONG Timeout;
	ULONG ControlCode;
	ULONG ReturnCode;
	ULONG Length;
}SRB_IO_CONTROL, *PSRB_IO_CONTROL;

// Define global buffers.
BYTE IdOutCmd[sizeof(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1];
char strHDSerialNumber[1024];
DWORD diskData[256];

BOOL DoIDENTIFY(HANDLE hPhysicalDriveIOCTL, PSENDCMDINPARAMS pSCIP,
	PSENDCMDOUTPARAMS pSCOP, BYTE bIDCmd, BYTE bDriveNum,
	PDWORD lpcbBytesReturned) {
	// Set up data structures for IDENTIFY command.
	pSCIP->cBufferSize = IDENTIFY_BUFFER_SIZE;
	pSCIP->irDriveRegs.bFeaturesReg = 0;
	pSCIP->irDriveRegs.bSectorCountReg = 1;
	pSCIP->irDriveRegs.bSectorNumberReg = 1;
	pSCIP->irDriveRegs.bCylLowReg = 0;
	pSCIP->irDriveRegs.bCylHighReg = 0;

	// Compute the drive number.
	pSCIP->irDriveRegs.bDriveHeadReg = 0xA0 | ((bDriveNum & 1) << 4);

	// The command can either be IDE identify or ATAPI identify.
	pSCIP->irDriveRegs.bCommandReg = bIDCmd;
	pSCIP->bDriveNumber = bDriveNum;
	pSCIP->cBufferSize = IDENTIFY_BUFFER_SIZE;

	return(DeviceIoControl(hPhysicalDriveIOCTL, DFP_RECEIVE_DRIVE_DATA, (LPVOID)
			pSCIP, sizeof(SENDCMDINPARAMS) - 1, (LPVOID)pSCOP, sizeof
			(SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE - 1, lpcbBytesReturned, NULL));
}

BOOL ReadPhysicalDriveInNT(void) {
	BOOL bRet = FALSE;

	HANDLE hPhysicalDriveIOCTL = 0;
	int drive = 0;

	// Try to get a handle to PhysicalDrive IOCTL,
	// report failure and exit if can't.
	char driveName[256];

	sprintf(driveName, "\\\\.\\PhysicalDrive0");

	// Windows NT, Windows 2000, must have admin rights
	hPhysicalDriveIOCTL = CreateFile(driveName, GENERIC_READ | GENERIC_WRITE,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);

	// if (hPhysicalDriveIOCTL == INVALID_HANDLE_VALUE)
	// printf ("Unable to open physical drive %d, error code: 0x%lX\n",
	// drive, GetLastError ());

	if (hPhysicalDriveIOCTL != INVALID_HANDLE_VALUE) {
		GETVERSIONOUTPARAMS VersionParams;
		DWORD cbBytesReturned = 0;

		// Get the version, etc of PhysicalDrive IOCTL
		memset((void*) & VersionParams, 0, sizeof(VersionParams));

		if (!DeviceIoControl(hPhysicalDriveIOCTL, DFP_GET_VERSION, NULL, 0,
				&VersionParams, sizeof(VersionParams), &cbBytesReturned, NULL)) {
		}

		// If there is a IDE device at number "i" issue commands
		// to the device
		if (VersionParams.bIDEDeviceMap > 0) {
			BYTE bIDCmd = 0; // IDE or ATAPI IDENTIFY cmd
			SENDCMDINPARAMS scip;

			// Now, get the ID sector for all IDE devices in the
			// system. If the device is ATAPI use the
			// IDE_ATAPI_IDENTIFY command, otherwise use the
			// IDE_ATA_IDENTIFY command
			bIDCmd = (VersionParams.bIDEDeviceMap >> drive & 0x10)
				? IDE_ATAPI_IDENTIFY : IDE_ATA_IDENTIFY;

			memset(&scip, 0, sizeof(scip));
			memset(IdOutCmd, 0, sizeof(IdOutCmd));

			if (DoIDENTIFY(hPhysicalDriveIOCTL, &scip, (PSENDCMDOUTPARAMS)
					& IdOutCmd, (BYTE)bIDCmd, (BYTE)drive, &cbBytesReturned)) {
				memset(diskData, 0, 1024);
				int i = 0;

				USHORT *pIdSector = (USHORT*)((PSENDCMDOUTPARAMS)IdOutCmd)
					->bBuffer;

				for (i = 0; i < 256; i++)
					diskData[i] = pIdSector[i];

				bRet = TRUE;
			}
		}

		CloseHandle(hPhysicalDriveIOCTL);
	}

	return bRet;
}

// --------------------------------------------------------------------------------

// =============================================================
// DoIDENTIFY
// FUNCTION: Send an IDENTIFY command to the drive
// bDriveNum = 0-3
// bIDCmd = IDE_ATA_IDENTIFY or IDE_ATAPI_IDENTIFY
// ==============================================================
#define  SENDIDLENGTH  sizeof (SENDCMDOUTPARAMS) + IDENTIFY_BUFFER_SIZE

BOOL ReadIdeDriveAsScsiDriveInNT(void) {
	BOOL bRet = FALSE;
	int controller = 0;

	HANDLE hScsiDriveIOCTL = 0;
	char driveName[256];

	// Try to get a handle to PhysicalDrive IOCTL, report failure
	// and exit if can't.
	sprintf(driveName, "\\\\.\\Scsi%d:", controller);

	// Windows NT, Windows 2000, any rights should do
	hScsiDriveIOCTL = CreateFile(driveName, GENERIC_READ | GENERIC_WRITE,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);

	// if (hScsiDriveIOCTL == INVALID_HANDLE_VALUE)
	// printf ("Unable to open SCSI controller %d, error code: 0x%lX\n",
	// controller, GetLastError ());

	if (hScsiDriveIOCTL != INVALID_HANDLE_VALUE) {
		int drive = 0;

		char buffer[sizeof(SRB_IO_CONTROL) + SENDIDLENGTH];
		SRB_IO_CONTROL *p = (SRB_IO_CONTROL*)buffer;
		SENDCMDINPARAMS *pin = (SENDCMDINPARAMS*)
			(buffer+sizeof(SRB_IO_CONTROL));
		DWORD dummy;

		memset(buffer, 0, sizeof(buffer));
		p->HeaderLength = sizeof(SRB_IO_CONTROL);
		p->Timeout = 10000;
		p->Length = SENDIDLENGTH;
		p->ControlCode = IOCTL_SCSI_MINIPORT_IDENTIFY;
		strncpy((char*)p->Signature, "SCSIDISK", 8);

		pin->irDriveRegs.bCommandReg = IDE_ATA_IDENTIFY;
		pin->bDriveNumber = drive;

		if (DeviceIoControl(hScsiDriveIOCTL, IOCTL_SCSI_MINIPORT, buffer, sizeof
				(SRB_IO_CONTROL)+sizeof(SENDCMDINPARAMS) - 1, buffer, sizeof
				(SRB_IO_CONTROL) + SENDIDLENGTH, &dummy, NULL)) {
			SENDCMDOUTPARAMS *pOut = (SENDCMDOUTPARAMS*)
				(buffer+sizeof(SRB_IO_CONTROL));
			IDSECTOR *pId = (IDSECTOR*)(pOut->bBuffer);
			if (pId->sModelNumber[0]) {
				memset(diskData, 0, 1024);
				int i = 0;
				USHORT *pIdSector = (USHORT*)pId;

				for (i = 0; i < 256; i++)
					diskData[i] = pIdSector[i];

				bRet = TRUE;
			}
		}
		CloseHandle(hScsiDriveIOCTL);
	}

	return bRet;
}

char *ConvertToString(DWORD diskData[256], int firstIndex, int lastIndex) {
	static char string[1024];
	int index = 0;
	int position = 0;

	// each integer has two characters stored in it backwards
	for (index = firstIndex; index <= lastIndex; index++) {
		// get high byte for 1st character
		string[position] = (char)(diskData[index] / 256);
		position++;

		// get low byte for 2nd character
		string[position] = (char)(diskData[index] % 256);
		position++;
	}

	// end the string
	string[position] = '\0';

	// cut off the trailing blanks
	for (index = position - 1; index >= 0; index--) {
		if (string[index] != ' ')
			break;
		else
			string[index] = 0;
	}
	for (index = 0; index < position; index++)
		if (string[index] != ' ')
			break;
	if (index != 0) {
		strncpy(string, string + index, position - index);
		string[position - index] = 0;
	}

	return string;
}

BOOL GetHDSerialCode(char* lpSerialID) {
	if (lpSerialID == NULL)
		return FALSE;

	memset(strHDSerialNumber, 0, 1024);

	BOOL bRet;

	bRet = ReadPhysicalDriveInNT();

	if (!bRet)
		bRet = ReadIdeDriveAsScsiDriveInNT();

	if (bRet) {
		strcpy(strHDSerialNumber, ConvertToString(diskData, 10, 19));

		try {
			strcpy(lpSerialID, strHDSerialNumber);
		}
		catch(...) {
			return FALSE;
		}
	}

	return bRet;
}


