{
  CodeMem.pas

  Delphi unit containing a functions for virtually allocated memory that allows
  execution of run-time created code. This way the allocate and free function
  can be replaced by user-defined (optimized) ones.

  Version 1.4 - Always find the most current version at
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

unit CodeMem;

interface

uses
  Windows, SysUtils;

type
  TCodeMemoryManager = record
    GetMem: function(Size: Integer): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer;
  end;

procedure GetCodeMemManager(var MemMgr: TCodeMemoryManager);
procedure SetCodeMemManager(const MemMgr: TCodeMemoryManager);
procedure ResetCodeMemManager;
function IsCodeMemManagerSet: Boolean;

function SysGetCodeMem(Size: integer): pointer;
function SysReallocCodeMem(P: pointer; Size: integer): pointer;
function SysFreeCodeMem(P: pointer): integer;

function AllocCodeMem(Size: integer): pointer;
function FreeCodeMem(P: pointer): integer;

implementation

{ The code memory manager }

var
  CodeMemManager: TCodeMemoryManager = (
    GetMem:     SysGetMem;
    FreeMem:    SysFreeMem;
    ReallocMem: SysReallocMem
  );

{ Code memory manager access }

procedure GetCodeMemManager(var MemMgr: TCodeMemoryManager);
begin
  MemMgr := CodeMemManager;
end;

procedure SetCodeMemManager(const MemMgr: TCodeMemoryManager);
begin
  CodeMemManager := MemMgr;
end;

procedure ResetCodeMemManager;
begin
  with CodeMemManager do
  begin
    GetMem     := SysGetMem;
    FreeMem    := SysFreeMem;
    ReallocMem := SysReallocMem
  end;
end;

function IsCodeMemManagerSet: Boolean;
begin
  with CodeMemManager do
    Result := (@GetMem <> @SysGetCodeMem) or
              (@FreeMem <> @SysFreeCodeMem) or
              (@ReallocMem <> @SysReallocCodeMem);
end;

{ Default functions }

function SysGetCodeMem(Size: integer): pointer;
begin
  Result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
end;

function SysReallocCodeMem(P: pointer; Size: integer): pointer;
begin
  SetLastError(ERROR_NOT_ENOUGH_MEMORY);
  Result := nil;
end;

function SysFreeCodeMem(P: pointer): integer;
begin
  Result := Ord(VirtualFree(P, 0, MEM_RELEASE) = FALSE);
end;

{ Public interface functions }

function AllocCodeMem(Size: integer): pointer;
begin
  if Size = 0 then
    Size := 1;

  Result := CodeMemManager.GetMem(Size);
  if Result = nil then
    RaiseLastOsError;

  FillChar(Result^, Size, 0);
end;

function FreeCodeMem(P: pointer): integer;
begin
  Result := CodeMemManager.FreeMem(P);
  if Result <> 0 then
    RaiseLastOsError;
end;

end.
