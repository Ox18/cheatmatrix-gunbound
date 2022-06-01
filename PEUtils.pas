unit PEUtils;

interface

uses windows, sysutils, classes, utils;

const
  SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeInformation = 3;

type
  TPDWord = ^DWORD;

  TSystem_Basic_Information = packed record
    dwUnknown1: DWORD;
    uKeMaximumIncrement: ULONG;
    uPageSize: ULONG;
    uMmNumberOfPhysicalPages: ULONG;
    uMmLowestPhysicalPage: ULONG;
    uMmHighestPhysicalPage: ULONG;
    uAllocationGranularity: ULONG;
    pLowestUserAddress: Pointer;
    pMmHighestUserAddress: Pointer;
    uKeActiveProcessors: ULONG;
    bKeNumberProcessors: byte;
    bUnknown2: byte;
    wUnknown3: word;
  end;

type
  TSystem_Performance_Information = packed record
    liIdleTime: LARGE_INTEGER; {LARGE_INTEGER}
    dwSpare: array[0..75] of DWORD;
  end;

type
  TSystem_Time_Information = packed record
    liKeBootTime: LARGE_INTEGER;
    liKeSystemTime: LARGE_INTEGER;
    liExpTimeZoneBias: LARGE_INTEGER;
    uCurrentTimeZoneId: ULONG;
    dwReserved: DWORD;
  end;

var
  NtQuerySystemInformation: function(infoClass: DWORD;
    buffer: Pointer;
    bufSize: DWORD;
    returnSize: TPDword): DWORD; stdcall = nil;


  liOldIdleTime: LARGE_INTEGER = ();
  liOldSystemTime: LARGE_INTEGER = ();

const
SystemModuleInformation	=	11;

Function GetExportFunctionName(dll: string; index: integer): string;
function RVAToFileOffset(Arquivo: string; RVA: Cardinal): Cardinal;

implementation

function Li2Double(x: LARGE_INTEGER): Double;
begin
  Result := x.HighPart * 4.294967296E9 + x.LowPart
end;

{Function GetKernelBase: cardinal;
var
  temp, Index, numBytes, numEntries: DWORD;
  buf: TPDword;
  driverInfo: ^TDriverInfo;
begin
  if @NtQuerySystemInformation = nil then
    NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'),
      'NtQuerySystemInformation');

  // Obtain required buffer size
  NtQuerySystemInformation(SystemModuleInformation, @temp, 0, @numBytes);
  // Allocate buffer
  buf := AllocMem(numBytes * 2);

  NtQuerySystemInformation(DRIVER_INFORMATION, buf, numBytes * 2, @numBytes);
  numEntries := buf^;
  driverInfo := Pointer(DWORD(buf) + 12);
  //Result     := '';
  for Index := 1 to numEntries do 
  begin
    //Result := Result + #$D#$A + 'Address: $' + IntToHex(DWORD(driverInfo^.Address), 8) +
      //'Name: "' + (driverInfo^.Name) + '"';
    if driverInfo^.Name
    Inc(driverInfo);
  end;
  Delete(Result, 1, 2);
  FreeMem(buf);
end; }


function RVAToFileOffset(Arquivo: string; RVA: Cardinal): Cardinal;
var
  OrgPos: Int64;
  EnteteMZ: TImageDosHeader;
  EntetePE: TImageNtHeaders;
  Section : TImageSectionHeader;
  i, Nbr  : Integer;
  FichierPE: TMemoryStream;
begin
  Result := RVA;
  FichierPE := TMemoryStream.Create;
  FichierPE.LoadFromFile(Arquivo);

  //Sauvegarde de la position actuelle du stream
  OrgPos := FichierPE.Position;
  try
    FichierPE.Seek(0, soFromBeginning);
    FichierPE.Read(EnteteMZ, SizeOf(EnteteMZ));
    FichierPE.Seek(EnteteMZ._lfanew, soFromBeginning);
    FichierPE.Read(EntetePE, SizeOf(EntetePE));

    //Nombre de sections dans le fichier
    Nbr := EntetePE.FileHeader.NumberOfSections;

    if Nbr <> 0 then
      for i := 0 to Nbr - 1 do
      begin
        FichierPE.Read(Section, SizeOf(Section));
        if (RVA >= Section.VirtualAddress) and (RVA < Section.VirtualAddress + Section.SizeOfRawData) then
        begin
          Result := RVA - Section.VirtualAddress + Section.PointerToRawData;
          Break;
        end;
      end;
  finally
    //Retour à la position initiale
    //FichierPE.Seek(OrgPos, soFromBeginning);
    FichierPE.Clear;
  end;
end;

Function GetExportFunctionName(dll: string; index: integer): string;
var
  Stream    : TMemoryStream;
  EnteteMZ  : TImageDosHeader;
  EntetePE  : TImageNtHeaders;
  ExportDir : TImageExportDirectory;
  i, NamePos: Cardinal;
  j,k         : Integer;
  Address   : Cardinal;
  Presente  : Boolean;
  s: string;
  Fichier: String;
  base: cardinal;
begin
  Fichier:= dll;
  //SetLength(fFunctions, 0);

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Fichier);
    Stream.Read(EnteteMZ, SizeOf(EnteteMZ));
    Stream.Seek(EnteteMZ._lfanew, soFromBeginning);
    Stream.Read(EntetePE, SizeOf(EntetePE));
    Stream.Seek(RVAToFileOffset(Fichier, EntetePE.OptionalHeader.DataDirectory[0].VirtualAddress), soFromBeginning);
    Stream.Read(ExportDir, SizeOf(ExportDir));

    //On retrouve toutes les fonctions
    if ExportDir.NumberOfFunctions <> 0 then
    begin
      Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)), soFromBeginning);

      if ExportDir.NumberOfNames <> 0 then
        for i := 0 to ExportDir.NumberOfNames - 1 do
        begin
          if i+1 <> index then
             continue;
          Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)) + i * SizeOf(NamePos), soFromBeginning);
          Stream.Read(NamePos, SizeOf(NamePos));
          Stream.Seek(RVAToFileOffset(Fichier, NamePos), soFromBeginning);
          s := GetRandomString(18);

          j := 0;
          result := '';
          while Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) <> 0 do
          begin
              result := result + char(Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^));
              inc(j);
          end;
          exit;
        end;
    end;

  finally
    Stream.Free;
  end;
end;

{Function GetExportFunctionName(dll: string; index: integer): string;
var
  Stream    : TMemoryStream;
  EnteteMZ  : TImageDosHeader;
  EntetePE  : TImageNtHeaders;
  ExportDir : TImageExportDirectory;
  i, NamePos: Cardinal;
  j,k         : Integer;
  Address   : Cardinal;
  Presente  : Boolean;
  s: string;
  Fichier: String;
  base: cardinal;
begin
  Fichier:= dll;
  //SetLength(fFunctions, 0);

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Fichier);
    Stream.Read(EnteteMZ, SizeOf(EnteteMZ));
    Stream.Seek(EnteteMZ._lfanew, soFromBeginning);
    Stream.Read(EntetePE, SizeOf(EntetePE));
    Stream.Seek(RVAToFileOffset(Fichier, EntetePE.OptionalHeader.DataDirectory[0].VirtualAddress), soFromBeginning);
    Stream.Read(ExportDir, SizeOf(ExportDir));

    //On retrouve toutes les fonctions
    if ExportDir.NumberOfFunctions <> 0 then
    begin
      Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)), soFromBeginning);

      if ExportDir.NumberOfNames <> 0 then
        for i := 0 to ExportDir.NumberOfNames - 1 do
        begin
          if i+1 <> index then
             continue;
          Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)) + i * SizeOf(NamePos), soFromBeginning);
          Stream.Read(NamePos, SizeOf(NamePos));
          Stream.Seek(RVAToFileOffset(Fichier, NamePos), soFromBeginning);
          s := GetRandomString(18);

          j := 0;
          result := '';
          while Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) <> 0 do
          begin
              result := result + char(Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^));
              inc(j);
          end;
          exit;
        end;
    end;

  finally
    Stream.Free;
  end;
end;}



end.
 