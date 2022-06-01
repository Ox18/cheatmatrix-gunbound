{
  GenCodeHook.pas

  Delphi unit containing functions to do a generic code hook by replacing the
  code location to be patched by a jump to the new location. Additionally, a
  new code fragment is created that allows to call the old function.

  Version 1.5 - Always find the most current version at
  http://flocke.vssd.de/prog/code/pascal/codehook/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
}

unit GenCodeHook;

interface

uses
  Windows, SysUtils;

{ Creates a generic code hook by patching "Proc" to execute "NewProc" instead.
  Before patching, "OldProc" receives a pointer to a function that can be used
  to call the old code (though not at the same location).
}
function CreateGenericCodeHook(Proc, NewProc: pointer; var OldProc: pointer): boolean;

{ Restore a patched location and free the memory
}
procedure RemoveGenericCodeHook(var OldProc: pointer);

{ Function that patches protected memory
}
function PatchMemory(Where: pointer; const What; Size: integer;
  PadSize: integer = 0; PadByte: byte = $90): boolean;

implementation

uses
  CodeLen, CodeMem;

{$DEFINE ABSOLUTE_ADDRESSES}
{.$DEFINE ABS_JUMP_INDIRECT}

type
  TNearJmp = packed record
    Insn: byte;         // $E9 = jmp disp32
    Disp: integer;
  end;

  TAbsJmp = packed record
{$IFDEF ABS_JUMP_INDIRECT}
    Insn1: byte;        // $FF
    Insn2: byte;        // $25 (%00-100-101) = jmp dword ptr [offs]
    Addr: pointer;
{$ELSE}
    Insn1: byte;        // $68 = push imm32
    Addr: pointer;
    Insn2: byte;        // $C3 = ret
{$ENDIF}
  end;

const
  SIGNATURE   = $68636746;  // 'Fgch'
{$IFDEF ABSOLUTE_ADDRESSES}
  PATCH_SIZE  = SizeOf(TAbsJmp);
{$ELSE}
  PATCH_SIZE  = SizeOf(TNearJmp);
{$ENDIF}
  BACKUP_SIZE = MAX_IA32_INSTRUCTION_LENGTH + PATCH_SIZE;

type
  TGenCodeHookHeader = record
    BupLen: integer;
    Backup: array [0 .. BACKUP_SIZE - 1] of byte;
    Location: pointer;
{$IFDEF ABS_JUMP_INDIRECT}
    JumpHere: pointer;
{$ENDIF}
    Signature: cardinal;
  end;

  PGenCodeHookStruct = ^TGenCodeHookStruct;
  TGenCodeHookStruct = record
    Header: TGenCodeHookHeader;
    Code: array [0 .. SizeOf(TNearJmp) - 1] of byte;  // dynamisch
  end;

{ Pointer increment
}
function pinc(Ptr: pointer; incr: integer = 1): pointer;
begin
  Result := pointer(integer(Ptr) + incr);
end;

{ Function that patches protected memory
}
function PatchMemory(Where: pointer; const What; Size: integer;
  PadSize: integer = 0; PadByte: byte = $90): boolean;
var
  OldProtect: cardinal;
begin
  if Size <= 0 then
    Result := true
  else
  begin
    Result := VirtualProtect(Where, Size, PAGE_EXECUTE_READWRITE, OldProtect);
    if Result then
      try
        if Size = 4 then
          PCardinal(Where)^ := PCardinal(@What)^
        else if Size = 2 then
          PWord(Where)^ := PWord(@What)^
        else if Size = 1 then
          PByte(Where)^ := PByte(@What)^
        else
        begin
          PByte(Where)^ := $CC;   // int3
          Move(pinc(@What)^, pinc(Where)^, Size - 1);
          PByte(Where)^ := PByte(@What)^;
        end;

        if Size < PadSize then
        begin
          FillChar(pinc(Where, Size)^, PadSize - Size, PadByte);
          Size := PadSize;
        end;

        FlushInstructionCache(GetCurrentProcess(), Where, Size);
      finally
        VirtualProtect(Where, Size, OldProtect, OldProtect);
      end;
  end;
end;

{ Calculate the displacement from Source to Target
  (Intel IA-32 use pc-relative displacements for jumps and calls)
}
function CalcDisplacement(Source, Target: pointer): integer;
begin
  Result := integer(Target) - (integer(Source) + SizeOf(integer));
end;

{ Creates a generic code hook by patching "Proc" to execute "NewProc" instead.
  Before patching, "OldProc" receives a pointer to a function that can be used
  to call the old code (though not at the same location).
}
function CreateGenericCodeHook(Proc, NewProc: pointer; var OldProc: pointer): boolean;
var
  mbi: TMemoryBasicInformation;
  len, bup: integer;
  cop: PGenCodeHookStruct;
  nj: TNearJmp;
{$IFDEF ABSOLUTE_ADDRESSES}
  aj: TAbsJmp;
{$ENDIF}
  map: TCodeMap;
begin
  Result := false;
  OldProc := nil;

  // PSDK quote: "Windows Me/98/95:  You cannot use VirtualProtect on
  //   any memory region located in the shared virtual address space
  //   (from 0x80000000 through 0xBFFFFFFF)."
  //
  // -> Hooking will fail!

  // Get size of memory region
  if VirtualQuery(Proc, mbi, SizeOf(mbi)) < SizeOf(mbi) then
    exit;

  // Analyze CPU code and find amount of memory to copy
  len := integer(mbi.RegionSize) - (integer(Proc) - integer(mbi.BaseAddress));
  map := AnalyzeCpuInstructionSequence(Proc, len);
  len := LengthOfCpuInstructionSequence(Proc, Map, PATCH_SIZE);
{$IFDEF ABSOLUTE_ADDRESSES}
  if len = 0 then
    len := LengthOfCpuInstructionSequence(Proc, Map, SizeOf(TNearJmp));
{$ENDIF}
  if len = 0 then
    exit;

  // Calculate the backup size (the remaining
  // bytes will be filled with NOPs later)
  bup := 0;
  while bup < PATCH_SIZE do
  begin
    if (Map[bup] and AISQ_SIZE_MASK) = 0 then
      inc(bup)
    else
      inc(bup, Map[bup] and AISQ_SIZE_MASK);
  end;

  // Get new memory
  cop := AllocCodeMem(SizeOf(TGenCodeHookStruct) + len);
  try
    // Initialize the header
    cop^.Header.Location := Proc;
    cop^.Header.Signature := SIGNATURE;
{$IFDEF ABS_JUMP_INDIRECT}
    cop^.Header.JumpHere := NewProc;
{$ENDIF}

    // Make backup copy
    cop^.Header.BupLen := bup;
    Move(Proc^, cop^.Header.Backup[0], bup);

    // Copy instructions (and do relocations)
    CopyCpuInstructionSequence(Proc, len, Map, @cop^.Code[0]);

    // place jump to old procedure at the end
    nj.Insn := $E9;    // call disp32
    nj.Disp := CalcDisplacement(@cop^.Code[len + 1], pinc(Proc, len));
    Move(nj, cop^.Code[len], SizeOf(TNearJmp));

    // Pointer to old procedure
    OldProc := @cop^.Code[0];

    // Finally, patch the target memory
{$IFDEF ABSOLUTE_ADDRESSES}
    if len = SizeOf(TNearJmp) then
    begin
    //nj.Insn := $E9;    // jmp disp32
      nj.Disp := CalcDisplacement(pinc(Proc), NewProc);
      Result := PatchMemory(Proc, nj, SizeOf(TNearJmp), bup);
    end
    else
    begin
{$IFDEF ABS_JUMP_INDIRECT}
      aj.Insn1 := $FF;   // jmp dword ptr [offs]
      aj.Insn2 := $25;   // ...
      aj.Addr := @cop^.Header.JumpHere;
{$ELSE}
      aj.Insn1 := $68;   // push imm32
      aj.Addr := NewProc;
      aj.Insn2 := $C3;   // ret
{$ENDIF}
      Result := PatchMemory(Proc, aj, SizeOf(TAbsJmp), bup);
    end;
{$ELSE}
  //nj.Insn := $E9;
    nj.Disp := CalcDisplacement(pinc(Proc), NewProc);
    Result := PatchMemory(Proc, nj, SizeOf(TNearJmp), bup);
{$ENDIF}

    if not Result then
    begin
      OldProc := nil;
      FreeCodeMem(cop);
    end;
  except
    OldProc := nil;
    FreeCodeMem(cop);
    raise;
  end;
end;

{ Restore a patched location and free the memory
}
procedure RemoveGenericCodeHook(var OldProc: pointer);
var
  cop: PGenCodeHookStruct;
begin
  if OldProc = nil then
    exit;

  cop := pinc(OldProc, -SizeOf(TGenCodeHookHeader));
  if (cop^.Header.Signature <> SIGNATURE) then
    exit;

  PatchMemory(cop^.Header.Location, cop^.Header.Backup[0], cop^.Header.BupLen);
  OldProc := cop^.Header.Location;
  cop^.Header.Signature := 0;

  FreeCodeMem(cop);
end;

end.
