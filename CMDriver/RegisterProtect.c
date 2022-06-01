#include "RegisterProtect.h"

// extern NTSTATUS ObQueryNameString(void *, void *, int size, int *);
// extern NTSYSAPI NTSTATUS NTAPI ZwSetValueKey(IN HANDLE KeyHandle, IN PUNICODE_STRING ValueName, IN ULONG TitleIndex OPTIONAL, IN ULONG Type, IN PVOID Data, IN ULONG DataSize);
// typedef NTSTATUS(*REALZWSETVALUEKEY)(IN HANDLE KeyHandle, IN PUNICODE_STRING ValueName, IN ULONG TitleIndex OPTIONAL, IN ULONG Type, IN PVOID Data, IN ULONG DataSize);
// REALZWSETVALUEKEY RealZwSetValueKey;
// NTSTATUS HookZwSetValueKey(IN HANDLE KeyHandle, IN PUNICODE_STRING ValueName, IN ULONG TitleIndex OPTIONAL, IN ULONG Type, IN PVOID Data, IN ULONG DataSize);

// SYSTEMSERVICE 的定义
typedef struct ServiceDescriptorEntry {
	unsigned int * ServiceTableBase; // 关键字段, 指向系统服务分发例程的基地址
	unsigned int * ServiceCounterTableBase;
	unsigned int NumberOfServices;
	unsigned char * ParamTableBase;
} ServiceDescriptorTableEntry_t, *PServiceDescriptorTableEntry_t;

// __declspec(dllimport) ServiceDescriptorTableEntry_t KeServiceDescriptorTable;
// #define SYSTEMSERVICE(_function) KeServiceDescriptorTable.ServiceTableBase[*(PULONG)((PUCHAR)_function+1)]

void Driver_Unload(PDRIVER_OBJECT);
// PVOID GetPointer(HANDLE);
// NTSTATUS HookZwSetValueKey(IN HANDLE, IN PUNICODE_STRING, IN ULONG TitleIndex, IN ULONG, IN PVOID, IN ULONG);

UINT64 getCR4(void) {
	return __readcr4();
}

void DriverUnload(PDRIVER_OBJECT DriverObject) {
	UNICODE_STRING usDosDeviceName;
	// (REALZWSETVALUEKEY)(SYSTEMSERVICE(ZwSetValueKey)) = RealZwSetValueKey;
	// DbgPrint("HideFile_Unload Called\r\n");
	DbgPrint("un");
	RtlInitUnicodeString(&usDosDeviceName, L"\\DosDevices\\CReg");
	IoDeleteSymbolicLink(&usDosDeviceName);
	IoDeleteDevice(DriverObject->DeviceObject);
}

NTSTATUS DispatchCreate(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	DbgPrint("cs");

	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return (STATUS_SUCCESS);
}

NTSTATUS DispatchClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	DbgPrint("cs");

	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return (STATUS_SUCCESS);
}

// 驱动的入口函数
NTSTATUS DriverEntry(IN PDRIVER_OBJECT pDriverObject, IN PUNICODE_STRING RegistryPath) {
	NTSTATUS NtStatus = STATUS_SUCCESS;
	PDEVICE_OBJECT pDeviceObject = NULL;
	UNICODE_STRING usDriverName, usDosDeviceName;
	DbgPrint("de\r\n");
	RtlInitUnicodeString(&usDriverName, L"\\Device\\CReg");
	RtlInitUnicodeString(&usDosDeviceName, L"\\DosDevices\\CReg");
	NtStatus = IoCreateDevice(pDriverObject, 0, &usDriverName, FILE_DEVICE_UNKNOWN, FILE_DEVICE_SECURE_OPEN, FALSE, &pDeviceObject);
	if (STATUS_SUCCESS == NtStatus) {
		pDriverObject->DriverUnload = DriverUnload;
		pDriverObject->MajorFunction[IRP_MJ_CREATE] = DispatchCreate;
		pDriverObject->MajorFunction[IRP_MJ_CLOSE] = DispatchClose;
		pDriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DispatchIoctl;
	}
	IoCreateSymbolicLink(&usDosDeviceName, &usDriverName);

#ifndef AMD64
	// determine if PAE is used
	cr4reg = (ULONG)getCR4();

	if ((cr4reg & 0x20) == 0x20) {
		PTESize = 8; // pae
		PAGE_SIZE_LARGE = 0x200000;
		MAX_PDE_POS = 0xC0604000;
		MAX_PTE_POS = 0xC07FFFF8;

	}
	else {
		PTESize = 4;
		PAGE_SIZE_LARGE = 0x400000;
		MAX_PDE_POS = 0xC0301000;
		MAX_PTE_POS = 0xC03FFFFC;
	}
#else
	PTESize = 8; // pae
	PAGE_SIZE_LARGE = 0x200000;
	MAX_PTE_POS = 0xFFFFF6FFFFFFFFF8ULL;
	MAX_PDE_POS = 0xFFFFF6FB7FFFFFF8ULL;
#endif

	DbgPrint("cd");
	// 保存 ZwSetValueKey 函数的入口地址
	// RealZwSetValueKey = (REALZWSETVALUEKEY)(SYSTEMSERVICE(ZwSetValueKey));
	// (REALZWSETVALUEKEY)(SYSTEMSERVICE(ZwSetValueKey)) = HookZwSetValueKey;
	return NtStatus;
}

BOOLEAN IsAddressSafe(UINT_PTR StartAddress) {

	// return TRUE;
#ifdef AMD64
	UINT_PTR kernelbase = 0x7fffffffffffffffULL;

	if (StartAddress < kernelbase)
		return TRUE;
	else {
		PHYSICAL_ADDRESS physical;
		physical.QuadPart = 0;
		physical = MmGetPhysicalAddress((PVOID)StartAddress);
		return (physical.QuadPart != 0);
	}

	return TRUE; // for now untill I ave figure out the win 4 paging scheme
#else

	ULONG kernelbase = 0x7ffe0000;

	if (StartAddress < kernelbase)
		return TRUE;

	{
		UINT_PTR PTE, PDE;
		struct PTEStruct *x;
		PTE = (UINT_PTR)StartAddress;
		PTE = PTE / 0x1000 * PTESize + 0xc0000000;
		// now check if the address in PTE is valid by checking the page table directory at 0xc0300000 (same location as CR3 btw)
		PDE = PTE / 0x1000 * PTESize + 0xc0000000; // same formula
		x = (PVOID)PDE;
		if ((x->P == 0) && (x->A2 == 0)) {
			// Not present or paged, and since paging in this area isn't such a smart thing to do just skip it
			// perhaps this is only for the 4 mb pages, but those should never be paged out, so it should be 1
			// bah, I've got no idea what this is used for
			return FALSE;
		}
		if (x->PS == 1) {
			// This is a 4 MB page (no pte list)
			// so, (startaddress/0x400000*0x400000) till ((startaddress/0x400000*0x400000)+(0x400000-1) ) ) is specified by this page
		}
		else // if it's not a 4 MB page then check the PTE
		{
			// still here so the page table directory agreed that it is a usable page table entry
			x = (PVOID)PTE;
			if ((x->P == 0) && (x->A2 == 0))
				return FALSE; // see for explenation the part of the PDE
		}
		return TRUE;
	}
#endif

}

BOOLEAN WriteProcessMemory(unsigned long PID, PEPROCESS PEProcess, PVOID Address, unsigned long Size, PVOID Buffer) {
	PEPROCESS selectedprocess = PEProcess;
	KAPC_STATE apc_state;
	NTSTATUS ntStatus = STATUS_SUCCESS;

//	DbgPrint("G0");
	if (selectedprocess == NULL) {
		if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID, &selectedprocess)))
			return FALSE;
	}

//	DbgPrint("Gravando na memoria %X - %d",Address, *((unsigned long*)Buffer));
	__try {
		UINT_PTR temp = (UINT_PTR)Address;
		RtlZeroMemory(&apc_state, sizeof(apc_state));
		KeAttachProcess((PEPROCESS)selectedprocess);

		__try {
			char* target;
			char* source;
			unsigned int i;

			if ((!IsAddressSafe((UINT_PTR)Address)) || (!IsAddressSafe((UINT_PTR)Address + Size - 1)))
				return FALSE;

			target = Address;
			source = Buffer;
			for (i = 0; i < Size; i++) {
//				DbgPrint("G1: %X", source[i]);
				target[i] = source[i];
			}

			ntStatus = STATUS_SUCCESS;
		}
		__finally {
			KeDetachProcess();
		}
	}
	__except (1) {
		ntStatus = STATUS_UNSUCCESSFUL;
	}

	if (PEProcess == NULL) // no valid peprocess was given so I made a reference, so lets also dereference
			ObDereferenceObject(selectedprocess);

	return NT_SUCCESS(ntStatus);
}

BOOLEAN ReadProcessMemory(unsigned long PID, PEPROCESS PEProcess, PVOID Address, unsigned long Size, PVOID Buffer) {
	PEPROCESS selectedprocess = PEProcess;
	NTSTATUS ntStatus = STATUS_SUCCESS;

	if (PEProcess == NULL) {
		if (!NT_SUCCESS(PsLookupProcessByProcessId((PVOID)(UINT_PTR)PID, &selectedprocess)))
			return FALSE; // couldn't get the PID
	}

	__try {
		UINT_PTR temp = (UINT_PTR)Address;
		KeAttachProcess((PEPROCESS)selectedprocess);
		__try {
			char* target;
			char* source;

			if ((!IsAddressSafe((UINT_PTR)Address)) || (!IsAddressSafe((UINT_PTR)Address + Size - 1)))
				return FALSE; // if the first or last byte of this region is not safe then exit;

			// still here, then I gues it's safe to read. (But I can't be 100% sure though, it's still the users problem if he accesses memory that doesn't exist)
			target = Buffer;
			source = Address;
			RtlCopyMemory(target, source, Size);
			ntStatus = STATUS_SUCCESS;
		}
		__finally {
			KeDetachProcess();
		}
	}
	__except (1) {
		// DbgPrint("Error while reading: ReadProcessMemory(%x,%p, %p, %d, %p\n", PID, PEProcess, Address, Size, Buffer);

		ntStatus = STATUS_UNSUCCESSFUL;
	}

	if (PEProcess == NULL) // no valid peprocess was given so I made a reference, so lets also dereference
			ObDereferenceObject(selectedprocess);

	return NT_SUCCESS(ntStatus);
}

NTSTATUS DispatchIoctl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
	NTSTATUS ntStatus = STATUS_UNSUCCESSFUL;
	PIO_STACK_LOCATION irpStack = IoGetCurrentIrpStackLocation(Irp);

	switch (irpStack->Parameters.DeviceIoControl.IoControlCode) {

	case IOCTL_CM_READMEMORY:
		__try {
			struct input {
				UINT64 processid;
				UINT64 startaddress;
				WORD bytestoread;
			}*pinp;

			pinp = Irp->AssociatedIrp.SystemBuffer;

			ntStatus = ReadProcessMemory((DWORD)pinp->processid, NULL, (PVOID)pinp->startaddress, pinp->bytestoread, pinp) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
		}
		__except (1) {
			ntStatus = STATUS_UNSUCCESSFUL;
		};

		break;
	case IOCTL_CM_WRITEMEMORY:
		__try {

			struct input {
				UINT64 processid;
				UINT64 startaddress;
				WORD bytestowrite;
			}*pinp, inp;
			long i;

			pinp = Irp->AssociatedIrp.SystemBuffer;
//			DbgPrint("W----: %d\n", sizeof(inp));
//			for(i = 0; i < 500; i++)
//				DbgPrint("%X",((BYTE*)pinp)[i]);
			ntStatus = WriteProcessMemory((DWORD)pinp->processid, NULL, (PVOID)pinp->startaddress, pinp->bytestowrite, (PVOID)((UINT_PTR)pinp +sizeof(inp))) ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
		}
		__except (1) {
			// something went wrong and I don't know what
			ntStatus = STATUS_UNSUCCESSFUL;
		};
	case IOCTL_CM_OPENPROCESS: {
			PEPROCESS selectedprocess;
			ULONG processid = *(PULONG)Irp->AssociatedIrp.SystemBuffer;
			HANDLE ProcessHandle;

			ntStatus = STATUS_SUCCESS;

			__try {
				ProcessHandle = 0;
//                DbgPrint("Opening process..");
				if (PsLookupProcessByProcessId((PVOID)(UINT_PTR)(processid), &selectedprocess) == STATUS_SUCCESS) {
//					DbgPrint("Calling ObOpenObjectByPointer\n");
					ntStatus = ObOpenObjectByPointer(selectedprocess, 0, NULL, PROCESS_ALL_ACCESS, *PsProcessType, KernelMode, // UserMode,
						&ProcessHandle);
//					DbgPrint("ntStatus=%x", ntStatus);
				}
			}
			__except (1) {
				ntStatus = STATUS_UNSUCCESSFUL;
			}

			*(PUINT64)Irp->AssociatedIrp.SystemBuffer = (UINT64)ProcessHandle;
			break;
		}
	default:
		break;
	}

	Irp->IoStatus.Status = ntStatus;

	// Set # of bytes to copy back to user-mode...
	if (ntStatus == STATUS_SUCCESS)
		Irp->IoStatus.Information = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
	else
		Irp->IoStatus.Information = 0;

	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return ntStatus;
}

//
// PVOID GetPointer(HANDLE handle) {
// PVOID pKey;
// if (!handle)
// return NULL;
// // ObReferenceObjectByHandle函数来获得这个Handle对应的FileObject, 得到的指针转换成文件对象的指针
// if (ObReferenceObjectByHandle(handle, 0, NULL, KernelMode, &pKey, NULL) != STATUS_SUCCESS) {
// pKey = NULL;
// }
// return pKey;
// }
//
//// HOOK设置注册表键值的函数
// NTSTATUS HookZwSetValueKey(IN HANDLE KeyHandle, IN PUNICODE_STRING ValueName, IN ULONG TitleIndex OPTIONAL, IN ULONG Type, IN PVOID Data, IN ULONG DataSize) {
// NTSTATUS rc;
// UNICODE_STRING *pUniName; // 定义得到修改注册表的UNI路径
// ULONG actualLen;
// ANSI_STRING keyname, akeyname, m_keyname; // 定义得到修改注册表的UNI路径
// PVOID pKey;
// RtlUnicodeStringToAnsiString(&akeyname, ValueName, TRUE);
// // 得到文件对象的指针
// if (pKey = GetPointer(KeyHandle)) {
// // 分配内存
// pUniName = ExAllocatePool(NonPagedPool, 512 * 2 + 2 * sizeof(ULONG));
// pUniName->MaximumLength = 512 * 2;
// // 保证空间内没有不干净的数据
// memset(pUniName, 0, pUniName->MaximumLength);
//
// if (NT_SUCCESS(ObQueryNameString(pKey, pUniName, 512 * 2, &actualLen))) {
// RtlUnicodeStringToAnsiString(&keyname, pUniName, TRUE);
// keyname.Buffer = _strupr(keyname.Buffer);
// akeyname.Buffer = _strupr(akeyname.Buffer);
// if (strcmp(keyname.Buffer, "\\REGISTRY\\MACHINE\\SOFTWARE\\MICROSOFT\\WINDOWS\\CURRENTVERSION\\RUN") == 0) {
// if (strcmp(akeyname.Buffer, "TESTEXE") == 0) {
// DbgPrint("试图修改键值:%s的数据,已被拦截", akeyname.Buffer);
// RtlFreeAnsiString(&akeyname);
// RtlFreeAnsiString(&keyname);
// // 释放内存
// if (pUniName) {
// ExFreePool(pUniName);
// }
// return 0;
// }
// }
// }
// }
// RtlFreeAnsiString(&akeyname);
// rc = RealZwSetValueKey(KeyHandle, ValueName, TitleIndex, Type, Data, DataSize);
// // 释放内存
// if (pUniName) {
// ExFreePool(pUniName);
// }
// return (rc);
// }
