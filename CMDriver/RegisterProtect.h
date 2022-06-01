#include "ntddk.h"
#include <stdio.h>
#include "ntifs.h"
#include <windef.h>

#pragma pack(1)

#define IOCTL_CM_READMEMORY 	CTL_CODE(FILE_DEVICE_UNKNOWN, 0x0230, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CM_WRITEMEMORY	CTL_CODE(FILE_DEVICE_UNKNOWN, 0x0231, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
#define IOCTL_CM_OPENPROCESS	CTL_CODE(FILE_DEVICE_UNKNOWN, 0x0232, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)

struct PTEStruct
{
	unsigned P         :  1; // present (1 = present)
	unsigned RW        :  1; // read/write
	unsigned US        :  1; // user/supervisor
	unsigned PWT       :  1; // page-level write-through
	unsigned PCD       :  1; // page-level cache disabled
	unsigned A         :  1; // accessed
	unsigned Reserved  :  1; // dirty
	unsigned PS        :  1; // page size (0 = 4-KB page)
	unsigned G         :  1; // global page
	unsigned A1		   :  1; // available 1 aka copy-on-write
	unsigned A2		   :  1; // available 2/ is 1 when paged to disk
	unsigned A3		   :  1; // available 3
	unsigned PFN       : 20; // page-frame number
};

int PTESize;
ULONG cr4reg;
UINT_PTR PAGE_SIZE_LARGE;
UINT_PTR MAX_PDE_POS;
UINT_PTR MAX_PTE_POS;


NTSTATUS DispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);