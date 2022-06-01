unit PEUtils;
////////////////////////////////////////////////////////////////////////////////
//
//   Unit        :  PEUtils
//   Date        :  06.07.2006
//   Description :  PE utility functions
//
////////////////////////////////////////////////////////////////////////////////
interface

////////////////////////////////////////////////////////////////////////////////
//   Include units
////////////////////////////////////////////////////////////////////////////////
uses
  Windows, SysUtils, TlHelp32, classes, common, uallKernel, uallRing0, utils,
  graphics, syncobjs, CMApis, CM_KM_Apis;

type

  TUndoChangesThread=class(TThread)
  private
    procedure ShutdownProtection;
    procedure ShowModification;
    Function CheckForChanges: boolean;
  public
    procedure execute; override;

  end;

  //PRegionData = ^TRegionData;
  TRegionData = record
    dllnr: byte;
    address: dword;
    size: dword;
  end;

  TApis = record
      Nome: String[250];
      Address: Dword;
  end;

  TDll = record
       Nome: String[250];
       OldBase: DWord;
       NewBase: Dword;
       Regions: array of TRegionData;
  end;

  ByteArray = array of byte;

  TMemoryRegion = record
     Dll: TDll;
     Memory: Array of ByteArray;
     Apis: array of TApis;
  end;

  var
  //MemRegions: array of TDll;
  DllsMemory: array of TMemoryRegion;

////////////////////////////////////////////////////////////////////////////////
//   Utility functions
////////////////////////////////////////////////////////////////////////////////
function   GetSizeOfImage(pImageBase: Pointer): ULONG;
function   GetSectionHeader(pImageBase: Pointer): PImageSectionHeader;
function   FindLastVirtualSection(pImageBase: Pointer): PImageSectionHeader;
function   FindLastPhysicalSection(pImageBase: Pointer): PImageSectionHeader;
function   FindFirstSection(pImageBase: Pointer): PImageSectionHeader;
function   FindNextSection(pImageBase: Pointer; CurrentSection: PImageSectionHeader): PImageSectionHeader;
function   FindSection(pImageBase: Pointer; SectionBase: DWORD): PImageSectionHeader;
procedure  MakeAllSectionsWritable(pImageBase: Pointer);
function   AddSection(pImageBase: Pointer; CodeVirtualStart, CodeSize: DWORD): PImageSectionHeader;
function   EncryptSection(CodeSection: PImageSectionHeader; pImageBase: Pointer; Seed: DWORD): DWORD;
Function   procAPIExportAddr(hModule: cardinal; apiName: Pchar): cardinal;

function HasChanges: boolean;
function UndoMemoryChanges: boolean;
function LoadMemory:boolean;
Function SaveMemory: boolean;


var
  FirstRun: boolean;
  UndoThread: TUndoChangesThread;
  CriticalSection: TCriticalSection;

implementation

uses UnitMatriz, constantes;

Function GetAddress(Ender: Pointer; offset: cardinal; Base: cardinal): Pointer;
begin
     result := Pointer(cardinal((Pointer(cardinal(Ender)+offset))^) + base);
end;

Function procAPIExportAddr(hModule: cardinal; apiName: Pchar): cardinal;
var numEntries, ordinalBase: cardinal;
    ordinal: word;

    ExportTable, PEHeader, Ponteiro : Pointer;
                                   
    i,j: integer;
    ExportName, API : String;
    ExpNameStr: PChar;
    ExportNamePointerTable: Pointer;
    ExportOrdinalTable, ExportAddrTable: Pointer;
begin
  result := 0;

  try
    API := strpas(apiName);
  except
     on e:exception do
        exit;
  end;

	if(hModule = 0) or (apiName = nil) then
		 exit;

	Ponteiro := Pointer(hModule); //base do modulo
	PEHeader :=  GetAddress(Ponteiro, $3c, hModule); // offset 0x3c contains offset to PE header
  //(Pointer(cardinal(Ponteiro) + $3c))^ + hModule;
	ExportTable := GetAddress(PEHeader, $78, hModule);  // offset 78h into PE header contains offset of export table
	// ptr now points to export directory table
	// offset 24 into the export directory table == number of entries in the Export Name Pointer Table
	// table
	numEntries := cardinal((Pointer(cardinal(ExportTable) + 24))^);
	//printf("NumEntries = %d\n", numEntries);
  //frmMatriz.Memo1.Lines.Add( #13#10+'entradas: '+IntToStr(numEntries));
	ExportNamePointerTable := GetAddress(ExportTable, 32, hModule);  // offset 32 into export directory contains offset to Export Name Pointer Table

	ordinalBase := cardinal(GetAddress(Ponteiro, 16, 0));
  //frmMatriz.Memo1.Lines.Add( #13#10+'OrdinalBase: '+IntToStr(ordinalBase));
	//printf("OrdinalBase is %d\n", ordinalBase);
	ExportOrdinalTable := GetAddress(ExportTable, 36, hModule);	// offset 36 into export directory contains offset to Ordinal Table
	ExportAddrTable := GetAddress(ExportTable, 28, hModule); // offset 28 into export directory contains offset to Export Addr Table
  i := 0;
  j:= 0;
	while i < numEntries-1 do
	begin
      inc(j);
		 //exportName := Pointer(cardinal(ExportNamePointerTable[i]) + hModule);
     ExpNameStr := Pointer(cardinal((pointer(cardinal(ExportNamePointerTable) + i))^) + hModule);

     //ExpNameStr := PChar(exportName^);
     try
       if (ExpNameStr = nil) or (ExpNameStr = '') or (strlen(ExpNameStr) <= 0) then
       begin
          inc(i,4);
          continue;
       end;
       ExportName := strpas(ExpNameStr);
     except
       on e:exception do
       begin
          inc(i,4);
          continue;
       end;
     end;

     //if  LowerCase(Trim(ExportName))[1] = 'r' then
		 if  LowerCase(Trim(ExportName)) = LowerCase(Trim(API)) then
		 begin		
			 ordinal := cardinal((  pointer( cardinal(ExportOrdinalTable)+i  )  )^);
       
			//printf("%s (i = %d) Ordinal = %d at %X\n", exportName, i, ordinal, ExportAddrTable[ordinal]);
  		 result := cardinal((pointer(cardinal(ExportAddrTable) + ordinal))^)+hModule;
       //frmMatriz.Memo1.Lines.Add( #13#10+ExportName+#13#10+' id = '+IntToStr(i div 4)+#13#10+'em '+IntToHex(result, 4));
       exit;
		 end;
     inc(i,4);
	end;

	result := 0;
end;

function CalcOffset(Pointers: Array of Pointer): Pointer;
var  dwIndex:          Integer;
begin

  // Default result
  result:=nil;

  // Calculate the offset by adding the pointers
  for dwIndex:=0 to High(Pointers) do
     Inc(PChar(result), Integer(Pointers[dwIndex]));

end;

function GetSizeOfImage(pImageBase: Pointer): ULONG;
var  pDosHeader:       PImageDosHeader;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pNtSignature:     PLongWord;
     nSizeOfImage:     ULONG;
     dwIndex:          Integer;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Calculate the size
     nSizeOfImage:=pOptHeader^.SizeOfHeaders;
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Sum size of all image sections; this will result in the image size
     for dwIndex:=0 to Pred(pFileHeader^.NumberOfSections) do
     begin
        // Add the raw data size
        Inc(nSizeOfImage, pSecHeader^.SizeOfRawData);
        // Next section
        pSecHeader:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(IMAGE_SIZEOF_SECTION_HEADER)]));
     end;
     // Return size of the executable
     result:=nSizeOfImage;
  end
  else
     // Not a valid signature
     result:=0;

end;

function GetSectionHeader(pImageBase: Pointer): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pNtSignature:     PLongWord;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     result:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
  end
  else
     // Not a valid image
     result:=nil;

end;

function FindLastVirtualSection(pImageBase: Pointer): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pTempHeader:      PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwCurrentMaxVA:   DWORD;
     dwPos:            Integer;
     dwIndex:          Integer;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Save first section header
     dwPos:=0;
     dwCurrentMaxVA:=0;
     pTempHeader:=pSecHeader;
     // Walk the sections
     for dwIndex:=0 to Pred(pNtHeaders^.FileHeader.NumberOfSections) do
     begin
        // Check virtual address
        if (pTempHeader^.VirtualAddress > dwCurrentMaxVA) then
        begin
           // Update current max VA
           dwCurrentMaxVA:=pTempHeader^.VirtualAddress;
           // Save index
           dwPos:=dwIndex;
        end;
        // Get next section header
        pTempHeader:=PImageSectionHeader(CalcOffset([pTempHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
     end;
     // Return the section
     result:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(dwPos * SizeOf(IMAGE_SECTION_HEADER))]));
  end
  else
     // Not a valid image
     result:=nil;

end;

function FindLastPhysicalSection(pImageBase: Pointer): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pTempHeader:      PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwCurrentMaxPA:   DWORD;
     dwPos:            Integer;
     dwIndex:          Integer;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Save first section header
     dwPos:=0;
     dwCurrentMaxPA:=0;
     pTempHeader:=pSecHeader;
     // Walk the sections
     for dwIndex:=0 to Pred(pNtHeaders^.FileHeader.NumberOfSections) do
     begin
        // Check raw data pointer
        if (pTempHeader^.PointerToRawData > dwCurrentMaxPA) then
        begin
           // Update current max PA
           dwCurrentMaxPA:=pTempHeader^.PointerToRawData;
           // Save index
           dwPos:=dwIndex;
        end;
        // Get next section header
        pTempHeader:=PImageSectionHeader(CalcOffset([pTempHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
     end;
     // Return the section
     result:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(dwPos * SizeOf(IMAGE_SECTION_HEADER))]));
  end
  else
     // Not a valid image
     result:=nil;

end;

function FindFirstSection(pImageBase: Pointer): PImageSectionHeader;
begin

  // Same as GetSectionHeader
  result:=GetSectionHeader(pImageBase);

end;

function FindNextSection(pImageBase: Pointer; CurrentSection: PImageSectionHeader): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pPrevHeader:      PImageSectionHeader;
     pCurrHeader:      PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwIndex:          Integer;
begin

  // Set default result
  result:=nil;

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Check section count
     if (pNtHeaders^.FileHeader.NumberOfSections > 1) then
     begin
        // Save prev and current headers
        pPrevHeader:=pSecHeader;
        pCurrHeader:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
        // Walk sections starting with the second in the list
        for dwIndex:=1 to Pred(pNtHeaders^.FileHeader.NumberOfSections) do
        begin
           // Check to see if prev = passed section
           if (pPrevHeader = CurrentSection) then
           begin
              // Return next section
              result:=pCurrHeader;
              // Done processing
              break;
           end;
           // Chain next section
           pPrevHeader:=pCurrHeader;
           pCurrHeader:=PImageSectionHeader(CalcOffset([pCurrHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
        end;
     end;
  end;

end;

function FindSection(pImageBase: Pointer; SectionBase: DWORD): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwIndex:          Integer;
begin

  // Set default result
  result:=nil;

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Walk the sections
     for dwIndex:=0 to Pred(pNtHeaders^.FileHeader.NumberOfSections) do
     begin
        // Check raw data pointer
        if (pSecHeader^.VirtualAddress = SectionBase) then
        begin
           // Return section
           result:=pSecHeader;
           // Done processing
           break;
        end;
        // Get next section header
        pSecHeader:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
     end;
  end;

end;

procedure MakeAllSectionsWritable(pImageBase: Pointer);
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwIndex:          Integer;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Walk the sections
     for dwIndex:=0 to Pred(pNtHeaders^.FileHeader.NumberOfSections) do
     begin
        // Update the characteristics
        pSecHeader^.Characteristics:=pSecHeader^.Characteristics or IMAGE_SCN_MEM_WRITE;
        // Get next section header
        pSecHeader:=PImageSectionHeader(CalcOffset([pSecHeader, Pointer(SizeOf(IMAGE_SECTION_HEADER))]));
     end;
  end;

end;

function AddSection(pImageBase: Pointer; CodeVirtualStart, CodeSize: DWORD): PImageSectionHeader;
var  pDosHeader:       PImageDosHeader;
     pNtHeaders:       PImageNtHeaders;
     pFileHeader:      PImageFileHeader;
     pOptHeader:       PImageOptionalHeader;
     pSecHeader:       PImageSectionHeader;
     pNtSignature:     PLongWord;
     dwSectionOffset:  DWORD;
begin

  // Get DOS header
  pDosHeader:=pImageBase;

  // Get PE header location
  pNtSignature:=PLongWord(CalcOffset([pImageBase, Pointer(pDosHeader._lfanew)]));

  // Check PE signature
  if (pNtSignature^ = IMAGE_NT_SIGNATURE) then
  begin
     // Save the NT header
     pNtHeaders:=PImageNtHeaders(pNtSignature);
     // Push past the signature
     Inc(PChar(pNtSignature), SizeOf(LongWord));
     // Find an offset to the main PE header
     pFileHeader:=PImageFileHeader(pNtSignature);
     // Get optional PE header
     pOptHeader:=PImageOptionalHeader(CalcOffset([pFileHeader, Pointer(IMAGE_SIZEOF_FILE_HEADER)]));
     // Get first section header
     pSecHeader:=PImageSectionHeader(CalcOffset([pOptHeader, Pointer(pFileHeader^.SizeOfOptionalHeader)]));
     // Calculate the new section offset
     dwSectionOffset:=DWORD(DWORD(pSecHeader) + pNtHeaders^.FileHeader.NumberOfSections * SizeOf(IMAGE_SECTION_HEADER) - DWORD(pImageBase));
     // Check whether there's room for a new section
     if (pNtHeaders^.OptionalHeader.SizeOfHeaders < (dwSectionOffset + SizeOf(IMAGE_SECTION_HEADER))) then
        // Failure
        result:=nil
     else
     begin
        // Get pointer to new section
        result:=PImageSectionHeader(CalcOffset([pImageBase, Pointer(dwSectionOffset)]));
        // Copy section data from previous section
        CopyMemory(result, PImageSectionHeader(PChar(result)-SizeOf(IMAGE_SECTION_HEADER)), SizeOf(IMAGE_SECTION_HEADER));
        // Update the PE header
        Inc(pNtHeaders^.FileHeader.NumberOfSections);
     end;
  end
  else
     // Not a valid image
     result:=nil;

end;

function EncryptSection(CodeSection: PImageSectionHeader; pImageBase: Pointer; Seed: DWORD): DWORD;
var  pData:            PByte;
     dwOldProtect:     DWORD;
     dwRawCP:          DWORD;
     dwVCS:            DWORD;
     dwStartPos:       DWORD;
     dwEndPos:         DWORD;
     dwSeed:           DWORD;
     NOld: dword;
begin

  // Get raw data pointer and size
  dwRawCP:=CodeSection^.PointerToRawData;
  dwVCS:=CodeSection^.SizeOfRawData;

  // Get start position relative from image base
  dwStartPos:=DWORD(pImageBase) + dwRawCP;

  // Check code section name, push start position if rsrc
  if (StrIComp(PChar(@CodeSection^.Name), '.rsrc') = 0) then Inc(dwStartPos, $11);

  // Get end position for raw data
  dwEndPos:=dwStartPos + dwVCS;

  // Set starting seed value
  dwSeed:=Seed;

  // Unprotect code
  if VirtualProtect(pImageBase, dwEndPos - DWORD(pImageBase), PAGE_READWRITE, dwOldProtect) then
  begin
     // Resource protection
     try
        // Encrypt the section data
        while (dwStartPos < dwEndPos) do
        begin
           // Get pointer to data
           pData:=PByte(Pointer(dwStartPos));
           // Add low byte of seed to data byte
           Inc(pData^, LoByte(LoWord(dwSeed)));
           // Missing piece
           //
           //    dwSeed:=GenerateRandomNumber(dwSeed);
           //
           // Push starting position
           Inc(dwStartPos);
        end;
     finally
        // Reset protection
        VirtualProtect(pImageBase, dwEndPos - DWORD(pImageBase), dwOldProtect, NOld);
     end;
  end;

  // Return new seed
  result:=dwSeed;
 
end; 

type
   TArrayPos = record
      Pos: integer;
      Quantidade: integer;
   end;

Function GetApiCount(Dll: string): TArrayPos;
var i:integer; j : boolean;
begin
    result.Quantidade := 0;
    j := false;
    for i := 1 to length(ProtectedApis) do
    begin

         if LowerCase(ProtectedApis[i]) = LowerCase(Dll) then
         begin
           j := true;
           result.Pos := i+1;
           continue;
         end;

         if pos('.dll',LowerCase(ProtectedApis[i])) > 0 then
           j := false;

         if j then
         begin
             inc(result.Quantidade);
         end;
    end; 

end;

Function DllInArray(Dll: string; Array_: array of string): boolean;
var i: integer;
begin
    result := false;
    for i := 0 to Length(array_)-1 do
    begin
        if LowerCase(Trim(dll)) = LowerCase(Trim(Array_[i])) then
        begin
            result := boolean(i+1);
            //result := true;
            exit;
        end;
    end; 
end;

Function SaveMemory: boolean;

var tlhlp: thandle;
    Modulo:tagMODULEENTRY32;
    MemoryInfo: _MEMORY_BASIC_INFORMATION;

    i,j:integer;
    Savedata: tfilestream;

    Regions: array of TDll;
    Flag: boolean;
    //Base: dword;
    TempBase: DWORD;
    Lib, Temp, DllHandle: dword;
    Apis: TArrayPos;
    Nome: PShortString;
    NomeArray: ShortString;
begin

  if ( ProtectPE ) then
  begin
    setlength(Regions,0);

    tlhlp:=createtoolhelp32snapshot(TH32CS_SNAPMODULE, getcurrentprocessid);
    Modulo.dwSize := sizeof(tagModuleentry32);


    try

      if module32first(tlhlp, Modulo) then
      begin
        repeat
            flag := false;
            Nome := nil;
            Nome := new(PShortString);
            ShortString(Nome^) := ShortString(Modulo.szModule);
            //CopyMemory(Nome, @Modulo.szModule , length(Modulo.szModule));

            //CopyMemory(Nome, @Modulo.szModule[0] , length(Modulo.szModule));

            if DllInArray(Modulo.szModule, ProtectedApis) then
            begin

                for i := 0 to length(Regions)-1 do
                begin
                     if uppercase(Modulo.szModule) = uppercase(Regions[i].Nome) then
                     begin
                           flag := true;
                           break;
                     end;
                end;

                if not Flag then
                begin
                    SetLength(Regions, Length(Regions) + 1);
                    i := length(Regions) - 1;
                    Regions[i].Nome := Nome^;//(Modulo.szModule);
                end;

                Regions[i].OldBase := dword( Modulo.modBaseAddr );
                //TempBase := new(PDWORD);
                //CopyMemory(TempBase, @dword(Modulo.modBaseAddr), sizeof(Dword) );
                TempBase := Regions[i].OldBase;

                while ( VirtualQueryEx(GetCurrentProcess, pointer(TempBase), MemoryInfo, SizeOf(MemoryInfo)) > 0 ) and (TempBase < (dword(Modulo.modbaseaddr)+Modulo.modBaseSize) ) do
                begin
                  if (MemoryInfo.Protect and (page_execute or page_execute_read or page_execute_readwrite))>0 then
                  begin
                    setlength(Regions[i].Regions ,length( Regions[i].Regions )+1);
                    Regions[i].Regions[ high(Regions[i].Regions) ].dllnr := 1;
                    Regions[i].Regions[ high(Regions[i].Regions) ].address := TempBase;
                    Regions[i].Regions[ high(Regions[i].Regions) ].size := MemoryInfo.RegionSize;
                  end;
                  inc( TempBase, MemoryInfo.RegionSize);
                end;
                
            end;
            //TempBase := nil;
            Nome := nil;
        until not module32next(tlhlp,Modulo);

      end;

      savedata := tfilestream.Create(CMDir+MemoryFile,fmCreate);

      Temp := MemoryVersion;
      savedata.WriteBuffer(Temp ,4); //Salva a versão da memoria
      Temp := SizeOf(TRegionData);
      savedata.WriteBuffer(Temp ,4); //Grava o tamanho do record TRegionData. Soh por garantia;
      Temp := Length(Regions);
      savedata.WriteBuffer(Temp ,4); //Salva a quantidade de Dlls

      for j:= 0 to Length(Regions) - 1 do
      begin
         DllHandle := LoadLibrary( PChar( String(Regions[j].Nome)) );
         savedata.WriteBuffer(Regions[j].Nome , SizeOf(Regions[j].Nome));  //Grava o nome da Dll
         savedata.WriteBuffer(Regions[j].OldBase ,4);              //Grava o endereço base da Dll
         Temp := Length(Regions[j].Regions);                    
         savedata.WriteBuffer(Temp ,4);                         //Grava a quantidade de Regions da Dll
         savedata.Writebuffer(Regions[j].Regions[0], Temp*SizeOf(TRegionData)); //Grava as Regions

         for i:=0 to Temp - 1 do
            savedata.WriteBuffer(pbyte(Regions[j].Regions[i].address)^, Regions[j].Regions[i].size);

         Apis := GetApiCount(Regions[j].Nome);

         savedata.WriteBuffer(Apis.Quantidade ,4);
         //Grava as apis indicadas no array 'ProtectedApis'
         for i := 0 to Apis.Quantidade - 1 do
         begin
             Temp := dword(GetProcAddress(DllHandle, pchar(ProtectedApis[i+Apis.Pos])));
             if temp = 0 then
             begin
                Mensagem.Add(Mensagem.GetFreeName, Const_0022, 5000, 1, GetColorLevel(4), false, true);
                Deletefile(CMDir+MemoryFile);
                exit;
             end;
             savedata.WriteBuffer(Temp,4);
         end;

         freelibrary(DllHandle);
      end;

      ProtectPe := true;

    finally
      savedata.Free;
    end;
  end;


end;


function LoadMemory:boolean;
var savedata: tfilestream;
    i,j:integer;
    Versao, Temp: dword;
    RegDataSize: dword;
    DllCount: dword;
    DllHandle: dword;
    RegCount, ApiCount: dword;
    ApiPos: TArrayPos;
    Api: TApis;
    NomeLen: dword;
    Nome: ShortString;
begin
  if ProtectionLoaded then
  begin
      result := ProtectionLoaded;
      exit;
  end;

  try
    CriticalSection.Enter;

    try
      if FirstRun then 
      begin
        //Versão da memoria
        SaveData := TFileStream.Create(CMDir+MemoryFile, fmOpenRead);
        SetLength(DllsMemory, 0);

        try

          savedata.ReadBuffer(Versao ,4);

          if Versao <> MemoryVersion then
          begin
            ProtectPE := False;
            frmMatriz.frameOptions1.cnf_ProtectPE.Checked := false;
            Mensagem.Add(Mensagem.GetFreeName, Const_0015, 5000, 1, GetColorLevel(4), false, true);
          end;

          savedata.ReadBuffer(RegDataSize ,4); //Tamanho do record TRegionData

          if RegDataSize <> SizeOf(TRegionData) then
          begin
            ProtectPE := False;
            frmMatriz.frameOptions1.cnf_ProtectPE.Checked := false;
            Mensagem.Add(Mensagem.GetFreeName, Const_0015, 5000, 1, GetColorLevel(4), false, true);
          end;

          savedata.ReadBuffer(DllCount ,4); //Quantidade de Dlls
          setlength(DllsMemory, DllCount);

          for i := 0 to DllCount - 1 do
          begin
              //savedata.ReadBuffer(NomeLen , 4); //Length do nome
              savedata.ReadBuffer(DllsMemory[i].Dll.Nome , SizeOf(DllsMemory[i].Dll.Nome)); //Nome do modulo
              //DllsMemory[i].Dll.Nome := Nome;

              if DllsMemory[i].Dll.Nome = '' then
                 continue;
              savedata.ReadBuffer(DllsMemory[i].Dll.OldBase ,4); //Base original do modulo

              try
                DllsMemory[i].Dll.NewBase := LoadLibrary( PChar(String(DllsMemory[i].Dll.Nome)) ); //Base atual do modulo
                FreeLibrary(DllsMemory[i].Dll.NewBase);
              except
                on e:exception do
                begin
                  ProtectPE := False;
                  frmMatriz.frameOptions1.cnf_ProtectPE.Checked := false;
                  Mensagem.Add(Mensagem.GetFreeName, Const_0021, 5000, 1, GetColorLevel(4), false, true);
                end;
              end;
              
              savedata.ReadBuffer(RegCount ,4); //Quantidade de Regions
              setlength(DllsMemory[i].Dll.Regions, RegCount);
              setlength(DllsMemory[i].Memory , RegCount);
              savedata.ReadBuffer(DllsMemory[i].Dll.Regions[0] , RegCount*RegDataSize);
              for j:= 0 to RegCount - 1 do
              begin
                  setlength(DllsMemory[i].Memory[j], DllsMemory[i].Dll.Regions[j].size);
                  savedata.ReadBuffer(DllsMemory[i].Memory[j][0], DllsMemory[i].Dll.Regions[j].size);
              end;
              savedata.ReadBuffer(ApiCount ,4); //Quantidade de Apis dessa dll que serão checadas 
              setlength(DllsMemory[i].Apis, ApiCount);
              ApiPos := GetApiCount(DllsMemory[i].Dll.Nome);
              for j:= 0 to ApiCount-1 do
              begin
                  //setLength(DllsMemory[i].Apis, length(DllsMemory[i].Apis) + 1);
                  DllsMemory[i].Apis[j].Nome := PChar(ProtectedApis[ApiPos.Pos+j]);
                  savedata.ReadBuffer(DllsMemory[i].Apis[j].Address ,4);
              end;
              
          end;

        finally
          savedata.free;
        end;

        firstrun:=false;
      end;

    finally
      CriticalSection.leave;
    end;
    //if it managed to get to here I gues nothing went wrong
    ProtectionLoaded := true;
  except

  end;

  result := ProtectionLoaded;
end;

function TUndoChangesThread.CheckForChanges: boolean;
begin
  result:=false;

  //if not LoadMemory then
  //begin
    Mensagem.Add(Mensagem.GetFreeName, Const_0016, 5000, 1, clAqua, true, true);
    synchronize(shutdownprotection);
    exit;
  //end;
  result := true;

end;

procedure TUndoChangesThread.execute;
begin
  while not terminated do
  begin
    {if (ProtectPE) and (CheckForChanges) and (not terminated) then
    begin
      synchronize(ShowModification);
    end;   }
    if ProtectPE then
       HasChanges;

    if not terminated then sleep(5000); //check ebvery 5 seconds for changes
  end;
end;

procedure TUndoChangesThread.ShowModification;
begin                                                                                                                             //GetColorLevel(7)
     Mensagem.Add(Mensagem.GetFreeName, Const_0017, 5000, 1, clAqua, true, true);
end;

procedure TUndoChangesThread.ShutdownProtection;
begin
     ProtectPE := false;
end;

function UndoMemoryChanges: boolean;
var i,j: integer;
    Address: dword;
    Temp, Buf, ar: dword;
    pHandle: cardinal;
begin
  result := true;

  For i:= 0 to length(DllsMemory)-1 do
  begin
       For j := 0 to length(DllsMemory[i].Dll.Regions) - 1 do
       begin
            Address := DllsMemory[i].Dll.Regions[j].address;
            
            if UseRing0 then
            begin
              pHandle := KM_OpenProcess(PROCESS_ALL_ACCESS, false, getcurrentprocessid);

              while Address < DllsMemory[i].Dll.Regions[j].address + DllsMemory[i].Dll.Regions[j].size do
              begin
                try
                  if pbyte(Address)^ = $cc then
                     sleep(0);

                  //now make that address writable
                  Temp := ((Address div $1000) *4)+$c0000000;

                  if KM_ReadProcessMemory(pHandle, pointer(Temp), @Buf, 4, ar) then
                  begin
                    //make it writable without copy-on-write. Copy-on0write should already have taken place
                    //when it got modified in the first place. (and if it isn't this'll fix ALL the processes)
                    buf := (buf or $2);
                    KM_WriteProcessMemory(pHandle , pointer(Temp),@Buf, 4, ar);
                  end;

                  //this page should be writable now, so lets write to it
                  CopyMemory(pointer(Temp), @DllsMemory[i].Memory[j][Address - DllsMemory[i].Dll.Regions[j].address], 4096)
                except
                  //error? ehrm, thats a bug and should never occur
                  inc(Address, 4096);
                  continue;
                end;

                inc(Address, 4096);
              end;
              closehandle(pHandle);
            end else
            begin
               try
                 if pbyte(Address)^ = $cc then
                     sleep(0);

                  //now make that address writable
                  Temp := ((Address div $1000) *4)+$c0000000;
                  CopyMemory(@Buf, Ptr(Temp), 4);
                  buf := (buf or $2);
                  CopyMemory(Ptr(Temp), @Buf, 4);
                  CopyMemory(Ptr(Temp), @DllsMemory[i].Memory[j][Address - DllsMemory[i].Dll.Regions[j].address], 4096);
                except
                  result := false;
                end;
                  //KM_WriteProcessMemory(pHandle , pointer(Temp),@Buf, 4, ar);
                 //VirtualProtect(pointer(Address), DllsMemory[i].Dll.Regions[j].size, PAGE_EXECUTE_READWRITE, ar);
                 {try
                   CopyMemory(pointer(Address),@DllsMemory[i].Memory[j][0], DllsMemory[i].Dll.Regions[j].size);
                 except
                  result:=false;
                 end; }
            end;
       end;
  end;

  if result then
    Mensagem.Add(Mensagem.GetFreeName, Const_0017, 5000, 1, clAqua, true, true)
  else
    Mensagem.Add(Mensagem.GetFreeName, Const_0018, 5000, 1, GetColorLevel(6), true, true);
end;

function HasChanges: boolean;
var kernel32,ntdll: thandle;
    NewMem: array [0..19] of byte;
    ApiAddress: Dword;
    RegionAddress, RegionSize: dword;
    i, j, k: integer;
    Found: boolean;
begin
  result := true;

  LoadMemory;
  Try
    For i := 0 to Length(DllsMemory)-1 do
    begin
       if DllsMemory[i].Dll.OldBase <> DllsMemory[i].Dll.NewBase then
          exit;

       for j := 0 to length(DllsMemory[i].Apis)-1 do
       begin
          ApiAddress := DllsMemory[i].Apis[j].Address;

          copyMemory(@NewMem[0], Ptr(ApiAddress), 20);
          Found := False;

          for k:=0 to length(DllsMemory[i].Dll.Regions) - 1 do
          begin
              RegionAddress := DllsMemory[i].Dll.Regions[k].address;
              RegionSize := DllsMemory[i].Dll.Regions[k].size;

              if (ApiAddress >= RegionAddress) and (ApiAddress < RegionAddress + RegionSize) then
              begin
                Found:=true;
                //find the offset into the array
                if not CompareMem(@DllsMemory[i].Memory[k][ApiAddress - RegionAddress], @NewMem[0] ,20) then
                begin
                  UndoMemoryChanges;
                  exit;
                end;
              end;
          end;

          if not found then exit;
       end;
     
    end;

    result := false;
  except
    on e: exception do
  end;

end;


  

end.


