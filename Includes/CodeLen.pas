{
  CodeLen.pas

  Delphi unit containing a few functions for Intel IA-32 instruction code
  analysis. The main purpose of these functions is to be able to hook existing
  code by finding a valid re-entrance point at the entry code.

  Supports:
  - 16- and 32-bit instructions (initial setting specified by parameter);
  - complete intel pentium 4 instruction set incl. floating point;
  - MMX, SSE, SSE2, and SSE3;
  - AMD 3DNow.

  Most of the information is taken from:
  - "The IA-32(R) Intel Architecture Software Developer's Manual", June 2005;
    http://developer.intel.com/design/pentium/manuals/
  - "The AMD-K6(R)-III Processor Data Sheet", 1999;
    "AMD Extensions to the 3DNow! and MMX Instruction Sets Manual", March 2000;
    http://www.amd.com/us-en/Processors/TechnicalResources/0,,30_182_739_1102,00.html

  Version 1.5b - Always find the most current version at
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

unit CodeLen;

interface

uses
  SysUtils;

const
  MAX_IA32_INSTRUCTION_LENGTH = 15;

  // Prefix bits for TAnalyzedInstruction.Prefixes
  AIPFX_ESCAPE     = $0001;          // Double byte opcode
  AIPFX_LOCKREP    = $0002;          // lock/repe/repne
  AIPFX_ES         = $0004;          // Segment prefixes
  AIPFX_CS         = $0008;
  AIPFX_SS         = $0010;
  AIPFX_DS         = $0020;
  AIPFX_FS         = $0040;
  AIPFX_GS         = $0080;
  AIPFX_DATA_SIZE  = $0100;          // Operand size prefix
  AIPFX_ADDR_SIZE  = $0200;          // Address size prefix

  // Instruction flags for TAnalyzedInstruction.Flags
  AIFL_FINAL       = $01;            // Instruction is final (ret, jmp)
  AIFL_BRANCH      = $02;            // Instruction is a branch
  AIFL_CONDITIONAL = $04;            // Branch is conditional (no call)
  AIFL_DEST_KNOWN  = $08;            // Destination of branch is known
  AIFL_HAS_MODRM   = $10;            // Instruction has mod r/m address
  AIFL_CODEREF_I   = $20;            // Instruction maybe has a code reference (immediate)
  AIFL_CODEREF_A   = $40;            // Instruction maybe has a code reference (address)

  // Branch types for TAnalyzedInstruction.BranchType
  AIBT_NONE        = $00;
  AIBT_DISP8       = $01;            // Direct short displacement at [BranchAddr]
  AIBT_DISP16      = $02;            // Direct 16-bit displacement at [BranchAddr]
  AIBT_DISP32      = $03;            // Direct 32-bit displacement at [BranchAddr]
  AIBT_PTR32       = $04;            // Direct 16-bit far pointer at [BranchAddr]
  AIBT_PTR48       = $05;            // Direct 32-bit far pointer at [BranchAddr]
  AIBT_REG16       = $06;            // Indirect 16-bit register jump
  AIBT_REG32       = $07;            // Indirect 32-bit register jump
  AIBT_MEM16       = $08;            // Indirect 16-bit memory jump at [BranchAddr[BranchCount]]
  AIBT_MEM32       = $09;            // Indirect 32-bit memory jump at [BranchAddr[BranchCount]]

type
  { Comment for code flow analysis for the bits in .Flags:

    Code    Instruction   Fin Bra Kno Con  Branch Type
    ======  ============  === === === ===  ===========
    7#      j.. d8         -   1   1   1   DISP8
    9A      call seg:ofs   -   1   1   -   PTR32/PTR48
    C2      ret i16        1   -   -   -   NONE
    C3      ret            1   -   -   -   NONE
    CA      retf i16       1   -   -   -   NONE
    CB      retf           1   -   -   -   NONE
    CF      iretd          1   -   -   -   NONE
    E#      loop.. d8      -   1   1   1   DISP8
    E3      jecxz d8       -   1   1   1   DISP8
    E8      call d32       -   1   1   -   DISP16/DISP32
    E9      jmp d32        1   1   1   -   DISP16/DISP32
    EA      jmp seg:ofs    1   1   1   -   PTR32/PTR48
    EB      jmp d8         1   1   1   -   DISP8
    FF /2   call [r/m]     -   1   -   -   REG16/32 / MEM16/32
    FF /4   jmp [r/m]      1   1   -   -   REG16/32 / MEM16/32 -> Branch[Addr]
    0F 8#   j.. d32        -   1   1   1   DISP16/32

    There is a special case for the indirect jmp instruction FF /4.
    What we really need are case/switch jump table statements. These
    instructions normally have the form

        jmp [@codetab + 4 * eax]

    meaning:

        a) uses SIB                  .....100 ........
        b) 16/32-bit displacement    00...... .....101
        c) no base register          00...... .....101
        d) scale value is 4          ........ 10......
                                     ======== ========
                                     00...100 10...101  <- check values

    In this case we assume a case/switch jump table and set BranchCount to
    1 to indicate this fact. Branch itself is the address of the jump table.
  }
  TAnalyzedInstruction = record
    Address: pointer;               // Address we have read from
    Size: integer;                  // Found size of instruction
    Prefixes: word;                 // AIPFX_... bits
    Flags: byte;                    // AIFL_... bits
    BranchType: byte;               // AIBT_... constants
    Branch: cardinal;               // Branch target address (if known)
    BranchAddr: pointer;            // Location of branch
    BranchCount: integer;           // Count of branches there (case tables)
    AddrSize: byte;                 // Size of r/m address (0/1/2/4)
    ImmSize: byte;                  // Size of immediate operand (0/1/2/4)
    AddrAddr: pointer;              // Location of r/m address
    ImmAddr: pointer;               // Address of immediate operand
  end;

{ Analyzes the CPU instruction at "CodePtr" and returns the length of the
  instruction found. Returns 0 in case of errors.
}
function AnalyzeCpuInstruction(var Insn: TAnalyzedInstruction;
  CodePtr: pointer; Size: integer): integer;

{ Returns the length of the CPU instruction at "CodePtr", 0 if any error occurs.
}
function GetCpuInstructionLength(CodePtr: pointer; Size: integer): integer;

{ Returns the length of a sequence of CPU instructions at "CodePtr" that have at least
  "MinSize" bytes in length, 0 if any error occurs.
}
function GetCpuInstructionSequence(CodePtr: pointer; Size, MinSize: integer): integer;

{ Returns TRUE if the CPU instruction at "CodePtr" is a return instruction.
}
function IsReturnInstruction(CodePtr: pointer; Size: integer): boolean;

{ Returns TRUE if the CPU instruction at "CodePtr" is a unconditional jump instruction.
}
function IsJumpInstruction(CodePtr: pointer; Size: integer): boolean;

{ Returns the length of the CPU instruction at "CodePtr" IFF it is a no-op.
}
function IsNopInstruction(CodePtr: pointer; Size: integer): integer;

{ Information about a code location is stored in the bits of a byte.

  +---+---+---+---+---+---+---+---+
  | U | C |  Type |      Size     |
  +---+---+---+---+---+---+---+---+
    |   |     |           |
    |   |     |           +-------- Instruction length (if start of insn)
    |   |     +-------------------- Data type (always)
    |   +-------------------------- Conditional jump pointing here
    +------------------------------ Unconditional jump pointing here

  First take a look at the data type:
    0 - unknown (not yet examinated, size may be 0!)
    1 - code
    2 - data
    3 - pointer (jump table, size always 4)

  If the type is 0 (`unknown´), the byte has not been analyzed. This may be
  due to the fact that no code sequence or branch is leading here.

  For all other types, if "Size" is not zero, then this byte is the start
  of a code or data block of "Size" bytes in length. All following bytes
  that also belong to this block get the same type but a "Size" of 0 to
  indicate this fact.

  The upper 2 bits store information about branch targets:
    Bit 6 - at least one conditional jump points here
    Bit 7 - at least one unconditional jump points here
}
type
  TCodeMap = array of byte;

const
  AISQ_SIZE_MASK                = $0F;
  AISQ_TYPE_MASK                = $30;
  AISQ_TYPE_UNKNOWN             = $00;
  AISQ_TYPE_CODE                = $10;
  AISQ_TYPE_DATA                = $20;
  AISQ_TYPE_POINTER             = $30;
  AISQ_COND_TARGET              = $40;
  AISQ_UNCOND_TARGET            = $80;

{ Analyzes the CPU instruction sequence at "CodePtr" and returns a `map´ that
  describes each byte. The intention is to analyze a complete function and
  to return useful information about it's lenght and the branches that were
  taken. After analysis, the resulting map is a dynamic array where
  Length(Result) gives the code length of the function in bytes and each
  entry consists of a combination of the AISQ_... constants.
}
function AnalyzeCpuInstructionSequence(CodePtr: pointer; Size: integer): TCodeMap;

{ Returns the length of a sequence of CPU instructions at "CodePtr", that
  has been analyzed by the function above which returned "Map", that have at
  least "MinSize" bytes in length.
}
function LengthOfCpuInstructionSequence(CodePtr: pointer; Map: TCodeMap;
  MinSize: integer): integer;

{ Uses a map calculated by the previous function to copy cpu instructions
  from "CodePtr" to "Destination". Relocates everything it detects, though
  this is far from being complete.
}
procedure CopyCpuInstructionSequence(CodePtr: pointer; Size: integer;
  Map: TCodeMap; Destination: pointer);

implementation

{ Coding the information:

  10 - prefix opcode, bits 0-5 specify which prefix
       10......
          +-+-+
            +-- 0000 = 0F: table switch prefix
                0001 = F0: lock | F2: repne / repnz | F3: rep / repe / repz
                0010 = 26: es:
                0011 = 2E: cs: | branch-not-taken
                0100 = 36: ss:
                0101 = 3E: ds: | branch-taken
                0110 = 64: fs:
                0111 = 65: gs:
                1000 = 66: size prefix
                1001 = 67: address prefix

  00 - neither reg/mem nor additional code byte
  01 - reg/mem follows the opcode
  11 - one code byte plus reg/mem follow the opcode
       (double table prefix / always has a reg/mem address)
       ........
       ||||+-++
       ||||  +-- 0000 = INVALID OPCODE
       ||||      0001 = no more operand, done
       ||||      0010 = immediate byte
       ||||      0011 = immediate word (16/32-Bit)
       ||||      0100 = immediate 16-bit
       ||||      0101 = immediate 16+8-bit (enter)
       ||||      0110 = address offset (16/32-Bit)
       ||||      0111 = segment address offset (32/48-Bit)
       ||||      1000 = short displacement (8-Bit)
       ||||      1001 = long displacement (16/32-Bit)
       ||||      1010 = long displacement like above, for a call
       ||||      1111 = reg/mem is a jump or call
       ||||
       |||Special F6/F7, operand only if reg=0
       ||Instruction does not continue here
       |Read reg/mem address
       Read one additional opcode byte before next step
}

const
  csModeMask      = $C0;   // Check for first mode
  csPrefixByte    = $80;   // Only without reg/mem following
  csOneMoreByte   = $80;   // Only with reg/mem following (AMD 3DNow)
  csRegMem        = $40;

  csPfxMask       = $0F;   // Prefix mask
  csPfxEscape     = $00;
  csPfxLockRep    = $01;
  csPfxES         = $02;
  csPfxCS         = $03;
  csPfxSS         = $04;
  csPfxDS         = $05;
  csPfxFS         = $06;
  csPfxGS         = $07;
  csPfxDataSize   = $08;
  csPfxAddrSize   = $09;

  csFinalInsn     = $20;   // is final instruction (ret, jmp)
  csSpecialCheck  = $10;   // check for special reg/mem with F6h/F7h "test"

  csOpMask        = $0F;
  csInvalid       = $00;   // invalid opcode
  csNone          = $01;   // no operand
  csImmByte       = $02;   // immediate 8-bit value
  csImmWord       = $03;   // immediate 16/32-bit value [OpSz]
  csImm16         = $04;   // ret imm16
  csImm24         = $05;   // enter imm8, imm16
  csAddress       = $06;   // 16/32-bit near address [AdSz]
  csPointer       = $07;   // 16:16/32-bit far address [AdSz]
  csDispByte      = $08;   // short displacement (8-bit)
  csDispWord      = $09;   // near displacement (16/32-bit) [OpSz]
  csDispCall      = $0A;   // near displacement for a call [OpSz]
  csIsJumpCall    = $0F;   // jmp/call instruction

type
  PByte = ^Byte; // [Fix] Delphi 5 does not have a public definition in System.pas

  TCodeSpec = byte;

  PCodeTable = ^TCodeTable;
  TCodeTable = array [byte] of TCodeSpec;

const
  //########## The base opcode tables ##########
  CInitTable: TCodeTable = (
    // $0x
    csRegMem,                   // 00: add byte ptr [r/m], reg
    csRegMem,                   // 01: add [d]word ptr [r/m], reg
    csRegMem,                   // 02: add reg, byte ptr [r/m]
    csRegMem,                   // 03: add reg, [d]word ptr [r/m]
    csImmByte,                  // 04: add al, imm8
    csImmWord,                  // 05: add [e]ax, imm32
    csNone,                     // 06: push es
    csNone,                     // 07: pop es
    csRegMem,                   // 08: or byte ptr [r/m], reg
    csRegMem,                   // 09: or [d]word ptr [r/m], reg
    csRegMem,                   // 0A: or reg, byte ptr [r/m]
    csRegMem,                   // 0B: or reg, [d]word ptr [r/m]
    csImmByte,                  // 0C: or al, imm8
    csImmWord,                  // 0D: or [e]ax, imm32
    csNone,                     // 0E: push cs
    csPrefixByte or csPfxEscape,// 0F: escape opcode to 2nd byte

    // $1x
    csRegMem,                   // 10: adc byte ptr [r/m], reg
    csRegMem,                   // 11: adc [d]word ptr [r/m], reg
    csRegMem,                   // 12: adc reg, byte ptr [r/m]
    csRegMem,                   // 13: adc reg, [d]word ptr [r/m]
    csImmByte,                  // 14: adc al, imm8
    csImmWord,                  // 15: adc [e]ax, imm32
    csNone,                     // 16: push ss
    csNone,                     // 17: pop ss
    csRegMem,                   // 18: sbb byte ptr [r/m], reg
    csRegMem,                   // 19: sbb [d]word ptr [r/m], reg
    csRegMem,                   // 1A: sbb reg, byte ptr [r/m]
    csRegMem,                   // 1B: sbb reg, [d]word ptr [r/m]
    csImmByte,                  // 1C: sbb al, imm8
    csImmWord,                  // 1D: sbb [e]ax, imm32
    csNone,                     // 1E: push ds
    csNone,                     // 1F: pop ds

    // $2x
    csRegMem,                   // 20: and byte ptr [r/m], reg
    csRegMem,                   // 21: and [d]word ptr [r/m], reg
    csRegMem,                   // 22: and reg, byte ptr [r/m]
    csRegMem,                   // 23: and reg, [d]word ptr [r/m]
    csImmByte,                  // 24: and al, imm8
    csImmWord,                  // 25: and [e]ax, imm32
    csPrefixByte or csPfxES,    // 26: es:
    csNone,                     // 27: daa
    csRegMem,                   // 28: sub byte ptr [r/m], reg
    csRegMem,                   // 29: sub [d]word ptr [r/m], reg
    csRegMem,                   // 2A: sub reg, byte ptr [r/m]
    csRegMem,                   // 2B: sub reg, [d]word ptr [r/m]
    csImmByte,                  // 2C: sub al, imm8
    csImmWord,                  // 2D: sub [e]ax, imm32
    csPrefixByte or csPfxCS,    // 2E: cs: | branch-not-taken
    csNone,                     // 2F: das

    // $3x
    csRegMem,                   // 30: xor byte ptr [r/m], reg
    csRegMem,                   // 31: xor [d]word ptr [r/m], reg
    csRegMem,                   // 32: xor reg, byte ptr [r/m]
    csRegMem,                   // 33: xor reg, [d]word ptr [r/m]
    csImmByte,                  // 34: xor al, imm8
    csImmWord,                  // 35: xor [e]ax, imm32
    csPrefixByte or csPfxSS,    // 36: ss:
    csNone,                     // 37: aaa
    csRegMem,                   // 38: cmp byte ptr [r/m], reg
    csRegMem,                   // 39: cmp [d]word ptr [r/m], reg
    csRegMem,                   // 3A: cmp reg, byte ptr [r/m]
    csRegMem,                   // 3B: cmp reg, [d]word ptr [r/m]
    csImmByte,                  // 3C: cmp al, imm8
    csImmWord,                  // 3D: cmp [e]ax, imm32
    csPrefixByte or csPfxDS,    // 3E: ds: | branch-taken
    csNone,                     // 3F: aas

    // $4x
    csNone,                     // 40: inc eax
    csNone,                     // 41: inc ecx
    csNone,                     // 42: inc edx
    csNone,                     // 43: inc ebx
    csNone,                     // 44: inc esp
    csNone,                     // 45: inc ebp
    csNone,                     // 46: inc esi
    csNone,                     // 47: inc edi
    csNone,                     // 48: dec eax
    csNone,                     // 49: dec ecx
    csNone,                     // 4A: dec edx
    csNone,                     // 4B: dec ebx
    csNone,                     // 4C: dec esp
    csNone,                     // 4D: dec ebp
    csNone,                     // 4E: dec esi
    csNone,                     // 4F: dec edi

    // $5x
    csNone,                     // 50: push eax
    csNone,                     // 51: push ecx
    csNone,                     // 52: push edx
    csNone,                     // 53: push ebx
    csNone,                     // 54: push esp
    csNone,                     // 55: push ebp
    csNone,                     // 56: push esi
    csNone,                     // 57: push edi
    csNone,                     // 58: pop eax
    csNone,                     // 59: pop ecx
    csNone,                     // 5A: pop edx
    csNone,                     // 5B: pop ebx
    csNone,                     // 5C: pop esp
    csNone,                     // 5D: pop ebp
    csNone,                     // 5E: pop esi
    csNone,                     // 5F: pop edi

    // $6x
    csNone,                     // 60: pushad
    csNone,                     // 61: popad
    csRegMem,                   // 62: bound reg32, dword ptr [r/m]
    csRegMem,                   // 63: arpl word ptr [r/m], reg16
    csPrefixByte or csPfxFS,    // 64: fs:
    csPrefixByte or csPfxGS,    // 65: gs:
    csPrefixByte or csPfxDataSize,  // 66: size prefix
    csPrefixByte or csPfxAddrSize,  // 67: address prefix
    csImmWord,                  // 68: push imm32
    csRegMem or csImmWord,      // 69: imul r32, r/m32, imm32
    csImmByte,                  // 6A: push imm8
    csRegMem or csImmByte,      // 6B: imul r32, r/m32, imm8
    csNone,                     // 6C: insb
    csNone,                     // 6D: insd
    csNone,                     // 6E: outsb
    csNone,                     // 6F: outsd

    // $7x
    csDispByte,                 // 70: jo disp8
    csDispByte,                 // 71: jno disp8
    csDispByte,                 // 72: jb disp8
    csDispByte,                 // 73: jae disp8
    csDispByte,                 // 74: je disp8
    csDispByte,                 // 75: jne disp8
    csDispByte,                 // 76: jbe disp8
    csDispByte,                 // 77: ja disp8
    csDispByte,                 // 78: js disp8
    csDispByte,                 // 79: jns disp8
    csDispByte,                 // 7A: jp disp8
    csDispByte,                 // 7B: jnp disp8
    csDispByte,                 // 7C: jl disp8
    csDispByte,                 // 7D: jge disp8
    csDispByte,                 // 7E: jle disp8
    csDispByte,                 // 7F: jg disp8

    // $8x
    csRegMem or csImmByte,      // 80: INSN byte ptr [r/m], imm8
    csRegMem or csImmWord,      // 81: INSN dword ptr [r/m], imm32
    csRegMem or csImmByte,      // 82: INSN byte ptr [r/m], imm8
    csRegMem or csImmByte,      // 83: INSN dword ptr [r/m], imm8
    csRegMem,                   // 84: test r/m8, r8
    csRegMem,                   // 85: test r/m32, r32
    csRegMem,                   // 86: xchg r/m8, reg8
    csRegMem,                   // 87: xchg r/m32, reg32
    csRegMem,                   // 88: mov r/m8, reg8
    csRegMem,                   // 89: mov r/m32, reg32
    csRegMem,                   // 8A: mov reg8, r/m8
    csRegMem,                   // 8B: mov reg32, r/m32
    csRegMem,                   // 8C: mov r/m, sreg
    csRegMem,                   // 8D: lea r32, r/m
    csRegMem,                   // 8E: mov sreg, r/m
    csRegMem,                   // 8F: [pop| | | | | | | ] r/m

    // $9x
    csNone,                     // 90: nop
    csNone,                     // 91: xchg eax, ecx
    csNone,                     // 92: xchg eax, edx
    csNone,                     // 93: xchg eax, ebx
    csNone,                     // 92: xchg eax, esp
    csNone,                     // 93: xchg eax, ebp
    csNone,                     // 92: xchg eax, esi
    csNone,                     // 93: xchg eax, edi
    csNone,                     // 98: cwde
    csNone,                     // 99: cdq
    csPointer,                  // 9A: call seg:disp32
    csNone,                     // 9B: fwait
    csNone,                     // 9C: pushfd
    csNone,                     // 9D: popfd
    csNone,                     // 9E: sahf
    csNone,                     // 9F: lahf

    // $Ax
    csAddress,                  // A0: mov al, [mem]
    csAddress,                  // A1: mov eax, [mem]
    csAddress,                  // A2: mov [mem], al
    csAddress,                  // A3: mov [mem], eax
    csNone,                     // A4: movsb
    csNone,                     // A5: movsd
    csNone,                     // A6: cmpsb
    csNone,                     // A7: cmpsd
    csImmByte,                  // A8: test al, imm8
    csImmWord,                  // A9: test eax, imm32
    csNone,                     // AA: stosb
    csNone,                     // AB: stosd
    csNone,                     // AC: lodsb
    csNone,                     // AD: lodsd
    csNone,                     // AE: scasb
    csNone,                     // AF: scasd

    // $Bx
    csImmByte,                  // B0: mov al, imm8
    csImmByte,                  // B1: mov cl, imm8
    csImmByte,                  // B2: mov dl, imm8
    csImmByte,                  // B3: mov bl, imm8
    csImmByte,                  // B4: mov ah, imm8
    csImmByte,                  // B5: mov ch, imm8
    csImmByte,                  // B6: mov dh, imm8
    csImmByte,                  // B7: mov bh, imm8
    csImmWord,                  // B8: mov eax, imm32
    csImmWord,                  // B9: mov ecx, imm32
    csImmWord,                  // BA: mov edx, imm32
    csImmWord,                  // BB: mov ebx, imm32
    csImmWord,                  // BC: mov esp, imm32
    csImmWord,                  // BD: mov ebp, imm32
    csImmWord,                  // BE: mov esi, imm32
    csImmWord,                  // BF: mov edi, imm32

    // $Cx
    csRegMem or csImmByte,      // C0: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m8, imm8
    csRegMem or csImmByte,      // C1: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m32, imm8
    csFinalInsn or csImm16,     // C2: ret imm16
    csFinalInsn or csNone,      // C3: ret
    csRegMem,                   // C4: les r32, r/m
    csRegMem,                   // C5: lds r32, r/m
    csRegMem or csImmByte,      // C6: mov r/m, imm8
    csRegMem or csImmWord,      // C7: mov r/m, imm32
    csImm24,                    // C8: enter imm16, imm8
    csNone,                     // C9: leave
    csFinalInsn or csImm16,     // CA: retf imm16
    csFinalInsn or csNone,      // CB: retf
    csNone,                     // CC: int3
    csImmByte,                  // CD: int imm8
    csNone,                     // CE: into
    csFinalInsn or csNone,      // CF: iretd

    // $Dx
    csRegMem,                   // D0: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m8, 1
    csRegMem,                   // D1: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m32, 1
    csRegMem,                   // D2: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m8, cl
    csRegMem,                   // D3: [rol|ror|rcl|rcr|shl|shr|sal|sar] r/m32, cl
    csImmByte,                  // D4: aam [ imm8 | 10 ]
    csImmByte,                  // D5: aad [ imm8 | 10 ]
    0,
    csNone,                     // D7: xlatb
    csRegMem,                   // D8: floating point instruction
    csRegMem,                   // D9: floating point instruction
    csRegMem,                   // DA: floating point instruction
    csRegMem,                   // DB: floating point instruction
    csRegMem,                   // DC: floating point instruction
    csRegMem,                   // DD: floating point instruction
    csRegMem,                   // DE: floating point instruction
    csRegMem,                   // DF: floating point instruction

    // $Ex
    csDispByte,                 // E0: loopnz disp8
    csDispByte,                 // E1: loopz disp8
    csDispByte,                 // E2: loop disp8
    csDispByte,                 // E3: jecxz disp8
    csImmByte,                  // E4: in al, imm8
    csImmByte,                  // E5: in eax, imm8
    csImmByte,                  // E6: out imm8, al
    csImmByte,                  // E7: out imm8, eax
    csDispCall,                 // E8: call disp32
    csFinalInsn or csDispWord,  // E9: jmp disp32
    csFinalInsn or csPointer,   // EA: jmp seg:disp32
    csFinalInsn or csDispByte,  // EB: jmp disp8
    csNone,                     // EC: in al, dx
    csNone,                     // ED: in eax, dx
    csNone,                     // EE: out dx, al
    csNone,                     // EF: out dx, eax

    // $Fx
    csPrefixByte or csPfxLockRep,   // F0: lock
    0,
    csPrefixByte or csPfxLockRep,   // F2: repne / repnz
    csPrefixByte or csPfxLockRep,   // F3: rep / repe / repz
    csNone,                     // F4: hlt
    csNone,                     // F5: cmc
    csRegMem or csImmByte or csSpecialCheck,
                                // F6: [test| |not|neg|mul|imul|div|idiv] r/m8
    csRegMem or csImmWord or csSpecialCheck,
                                // F7: [test| |not|neg|mul|imul|div|idiv] r/m32
    csNone,                     // F8: clc
    csNone,                     // F9: stc
    csNone,                     // FA: cli
    csNone,                     // FB: std
    csNone,                     // FC: cld
    csNone,                     // FD: std
    csRegMem,                   // FE: [inc|dec|    |    |   |   | | ] r/m8
    csRegMem or csSpecialCheck  // FF: [inc|dec|call|call|jmp|jmp|push| ] r/m32
  );

  //########## Opcodes for prefix 0Fh ##########
  CPrefix0FTable: TCodeTable = (
    // $0x
    csRegMem,                   // 0F 00: [sldt|str|lldt|ltr|verr|verw| | ] r/m
    csRegMem,                   // 0F 01: [sgdt|sidt|lgdt|lidt|smsw| |lmsw|swapgs] r/m
    csRegMem,                   // 0F 02: lar reg, r/m
    csRegMem,                   // 0F 03: lsl reg, r/m
    0,
    csPrefixByte or csPfxLockRep,   // 0F 05: syscall
    csNone,                     // 0F 06: clts
    csNone,                     // 0F 07: sysret
    csNone,                     // 0F 08: invd
    csNone,                     // 0F 09: wbinvd
    0,
    csNone,                     // 0F 0B: ud2
    0,
    csRegMem,                   // 0F 0D: nop r/m
    csNone,                     // 0F 0E: femms (AMD 3DNow)
    csOneMoreByte or csRegMem,  // 0F 0F: AMD 3DNow double escape (always r/m)

    // $1x
    csRegMem,                   // 0F 10: SIMD reg, r/m
    csRegMem,                   // 0F 11: SIMD r/m, reg
    csRegMem,                   // 0F 12: SIMD reg, r/m
    csRegMem,                   // 0F 13: SIMD r/m, reg
    csRegMem,                   // 0F 14: SIMD reg,r/m
    csRegMem,                   // 0F 15: SIMD reg,r/m
    csRegMem,                   // 0F 16: SIMD reg, r/m
    csRegMem,                   // 0F 17: SIMD r/m, reg
    csRegMem,                   // 0F 18: prefetch r/m
    0,
    0,
    0,
    0,
    0,
    0,
    csRegMem,                   // 0F 1F: nop r/m

    // $2x
    csRegMem,                   // 0F 20: mov r/m, cr#
    csRegMem,                   // 0F 21: mov r/m, dr#
    csRegMem,                   // 0F 22: mov cr#, r/m
    csRegMem,                   // 0F 23: mov dr#, r/m
    csRegMem,                   // 0F 24: mov r/m, tr#
    0,
    csRegMem,                   // 0F 26: mov tr#, r/m
    0,
    csRegMem,                   // 0F 28: SIMD reg, r/m
    csRegMem,                   // 0F 29: SIMD r/m, reg
    csRegMem,                   // 0F 2A: SIMD reg, r/m
    csRegMem,                   // 0F 2A: SIMD r/m, reg
    csRegMem,                   // 0F 2C: SIMD reg, r/m
    csRegMem,                   // 0F 2D: SIMD reg, r/m
    csRegMem,                   // 0F 2E: SIMD reg, r/m
    csRegMem,                   // 0F 2F: SIMD reg, r/m

    // $3x
    csNone,                     // 0F 30: wrmsr
    csNone,                     // 0F 31: rdtsc
    csNone,                     // 0F 32: rdmsr
    csNone,                     // 0F 33: rdpmc
    csNone,                     // 0F 34: sysenter
    csNone,                     // 0F 35: sysexit
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,

    // $4x
    csRegMem,                   // 0F 40: cmovo reg32, [r/m32]
    csRegMem,                   // 0F 41: cmovno reg32, [r/m32]
    csRegMem,                   // 0F 42: cmovb reg32, [r/m32]
    csRegMem,                   // 0F 43: cmovae reg32, [r/m32]
    csRegMem,                   // 0F 44: cmove reg32, [r/m32]
    csRegMem,                   // 0F 45: cmovne reg32, [r/m32]
    csRegMem,                   // 0F 46: cmovbe reg32, [r/m32]
    csRegMem,                   // 0F 47: cmova reg32, [r/m32]
    csRegMem,                   // 0F 48: cmovs reg32, [r/m32]
    csRegMem,                   // 0F 49: cmovns reg32, [r/m32]
    csRegMem,                   // 0F 4A: cmovp reg32, [r/m32]
    csRegMem,                   // 0F 4B: cmovnp reg32, [r/m32]
    csRegMem,                   // 0F 4C: cmovl reg32, [r/m32]
    csRegMem,                   // 0F 4D: cmovge reg32, [r/m32]
    csRegMem,                   // 0F 4E: cmovle reg32, [r/m32]
    csRegMem,                   // 0F 4F: cmovg reg32, [r/m32]

    // $5x
    csRegMem,                   // 0F 50: SIMD reg, r/m
    csRegMem,                   // 0F 51: SIMD reg, r/m
    csRegMem,                   // 0F 52: SIMD reg, r/m
    csRegMem,                   // 0F 53: SIMD reg, r/m
    csRegMem,                   // 0F 54: SIMD reg, r/m
    csRegMem,                   // 0F 55: SIMD reg, r/m
    csRegMem,                   // 0F 56: SIMD reg, r/m
    csRegMem,                   // 0F 57: SIMD reg, r/m
    csRegMem,                   // 0F 58: SIMD reg, r/m
    csRegMem,                   // 0F 59: SIMD reg, r/m
    csRegMem,                   // 0F 5A: SIMD reg, r/m
    csRegMem,                   // 0F 5B: SIMD reg, r/m
    csRegMem,                   // 0F 5C: SIMD reg, r/m
    csRegMem,                   // 0F 5D: SIMD reg, r/m
    csRegMem,                   // 0F 5E: SIMD reg, r/m
    csRegMem,                   // 0F 5F: SIMD reg, r/m

    // $6x
    csRegMem,                   // 0F 60: SIMD reg, r/m
    csRegMem,                   // 0F 61: SIMD reg, r/m
    csRegMem,                   // 0F 62: SIMD reg, r/m
    csRegMem,                   // 0F 63: SIMD reg, r/m
    csRegMem,                   // 0F 64: SIMD reg, r/m
    csRegMem,                   // 0F 65: SIMD reg, r/m
    csRegMem,                   // 0F 66: SIMD reg, r/m
    csRegMem,                   // 0F 67: SIMD reg, r/m
    csRegMem,                   // 0F 68: SIMD reg, r/m
    csRegMem,                   // 0F 69: SIMD reg, r/m
    csRegMem,                   // 0F 6A: SIMD reg, r/m
    csRegMem,                   // 0F 6B: SIMD reg, r/m
    csRegMem,                   // 0F 6C: SIMD reg, r/m
    csRegMem,                   // 0F 6D: SIMD reg, r/m
    csRegMem,                   // 0F 6E: SIMD reg, r/m
    csRegMem,                   // 0F 6F: SIMD reg, r/m

    // $7x
    csRegMem or csImmByte,      // 0F 70: SIMD reg, r/m, imm8
    csRegMem or csImmByte,      // 0F 71: SIMD reg, r/m, imm8
    csRegMem or csImmByte,      // 0F 72: SIMD reg, r/m, imm8
    csRegMem or csImmByte,      // 0F 73: SIMD reg, r/m, imm8
    csRegMem,                   // 0F 74: SIMD reg, r/m
    csRegMem,                   // 0F 75: SIMD reg, r/m
    csRegMem,                   // 0F 76: SIMD reg, r/m
    csNone,                     // 0F 77: emms
    0,
    0,
    0,
    0,
    csRegMem,                   // 0F 7C: SIMD reg, r/m
    csRegMem,                   // 0F 7D: SIMD reg, r/m
    csRegMem,                   // 0F 7E: movd r/m, reg
    csRegMem,                   // 0F 7F: movdqa r/m, reg

    // $8x
    csDispWord,                 // 0F 80: jo disp32
    csDispWord,                 // 0F 81: jno disp32
    csDispWord,                 // 0F 82: jb disp32
    csDispWord,                 // 0F 83: jae disp32
    csDispWord,                 // 0F 84: je disp32
    csDispWord,                 // 0F 85: jne disp32
    csDispWord,                 // 0F 86: jbe disp32
    csDispWord,                 // 0F 87: ja disp32
    csDispWord,                 // 0F 88: js disp32
    csDispWord,                 // 0F 89: jns disp32
    csDispWord,                 // 0F 8A: jp disp32
    csDispWord,                 // 0F 8B: jnp disp32
    csDispWord,                 // 0F 8C: jl disp32
    csDispWord,                 // 0F 8D: jge disp32
    csDispWord,                 // 0F 8E: jle disp32
    csDispWord,                 // 0F 8F: jg disp32

    // $9x
    csRegMem,                   // 0F 90: seto [r/m8]
    csRegMem,                   // 0F 91: setno [r/m8]
    csRegMem,                   // 0F 92: setb [r/m8]
    csRegMem,                   // 0F 93: setae [r/m8]
    csRegMem,                   // 0F 94: sete [r/m8]
    csRegMem,                   // 0F 95: setne [r/m8]
    csRegMem,                   // 0F 96: setbe [r/m8]
    csRegMem,                   // 0F 97: seta [r/m8]
    csRegMem,                   // 0F 98: sets [r/m8]
    csRegMem,                   // 0F 99: setns [r/m8]
    csRegMem,                   // 0F 9A: setp [r/m8]
    csRegMem,                   // 0F 9B: setnp [r/m8]
    csRegMem,                   // 0F 9C: setl [r/m8]
    csRegMem,                   // 0F 9D: setge [r/m8]
    csRegMem,                   // 0F 9E: setle [r/m8]
    csRegMem,                   // 0F 9F: setg [r/m8]

    // $Ax
    csNone,                     // 0F A0: push fs
    csNone,                     // 0F A1: pop fs
    csNone,                     // 0F A2: cpuid
    csRegMem,                   // 0F A3: bt dword ptr [r/m], reg32
    csRegMem or csImmByte,      // 0F A4: shld r/m32, r32, imm8
    csRegMem,                   // 0F A5: shld r/m32, r32, cl
    0,
    0,
    csNone,                     // 0F A8: push gs
    csNone,                     // 0F A9: pop gs
    csNone,                     // 0F AA: rsm
    csRegMem,                   // 0F AB: bts dword ptr [r/m], reg32
    csRegMem or csImmByte,      // 0F AC: shrd r/m32, r32, imm8
    csRegMem,                   // 0F AD: shrd r/m32, r32, cl
    csRegMem,                   // 0F AE: (multiple insn) [r/m]
    csRegMem,                   // 0F AF: imul r32, r/m32

    // $Bx
    csRegMem,                   // 0F B0: cmpxchg [r/m8], reg8
    csRegMem,                   // 0F B1: cmpxchg [r/m32], reg32
    csRegMem,                   // 0F B2: lss r32, rm32
    csRegMem,                   // 0F B3: btr dword ptr [r/m], reg32
    csRegMem,                   // 0F B4: lfs r32, rm32
    csRegMem,                   // 0F B5: lgs r32, rm32
    csRegMem,                   // 0F B6: movzx r32, r/m8
    csRegMem,                   // 0F B7: movzx r32, r/m16
    0,
    0,
    csRegMem or csImmByte,      // 0F BA: bt? dword ptr [r/m], imm8
    csRegMem,                   // 0F BB: btc dword ptr [r/m], reg32
    csRegMem,                   // 0F BC: bsf reg, dword ptr [r/m]
    csRegMem,                   // 0F BD: bsr reg, dword ptr [r/m]
    csRegMem,                   // 0F BE: movsx r32, r/m8
    csRegMem,                   // 0F BF: movsx r32, r/m16

    // $Cx
    csRegMem,                   // 0F C0: xadd r/m8, reg8
    csRegMem,                   // 0F C1: xadd r/m32, reg32
    csRegMem or csImmByte,      // 0F C2: SIMD reg, r/m, imm8
    csRegMem,                   // 0F C3: SIMD r/m,reg
    csRegMem or csImmByte,      // 0F C4: SIMD reg, r/m, imm8
    csRegMem or csImmByte,      // 0F C5: SIMD reg, r/m, imm8
    csRegMem or csImmByte,      // 0F C6: SIMD reg, r/m, imm8
    csRegMem,                   // 0F C7: [|cmpxchg8b||||||] [r/m]
    csNone,                     // 0F C8: bswap eax
    csNone,                     // 0F C9: bswap ecx
    csNone,                     // 0F CA: bswap edx
    csNone,                     // 0F CB: bswap ebx
    csNone,                     // 0F CC: bswap esp
    csNone,                     // 0F CD: bswap ebp
    csNone,                     // 0F CE: bswap esi
    csNone,                     // 0F CF: bswap edi

    // $Dx
    csRegMem,                   // 0F D0: SIMD reg, r/m
    csRegMem,                   // 0F D1: SIMD reg, r/m
    csRegMem,                   // 0F D2: SIMD reg, r/m
    csRegMem,                   // 0F D3: SIMD reg, r/m
    csRegMem,                   // 0F D4: SIMD reg, r/m
    csRegMem,                   // 0F D5: SIMD reg, r/m
    csRegMem,                   // 0F D6: SIMD r/m, reg
    csRegMem,                   // 0F D7: SIMD reg, r/m
    csRegMem,                   // 0F D8: SIMD reg, r/m
    csRegMem,                   // 0F D9: SIMD reg, r/m
    csRegMem,                   // 0F DA: SIMD reg, r/m
    csRegMem,                   // 0F DB: SIMD reg, r/m
    csRegMem,                   // 0F DC: SIMD reg, r/m
    csRegMem,                   // 0F DD: SIMD reg, r/m
    csRegMem,                   // 0F DE: SIMD reg, r/m
    csRegMem,                   // 0F DF: SIMD reg, r/m

    // $Ex
    csRegMem,                   // 0F E0: SIMD reg, r/m
    csRegMem,                   // 0F E1: SIMD reg, r/m
    csRegMem,                   // 0F E2: SIMD reg, r/m
    csRegMem,                   // 0F E3: SIMD reg, r/m
    csRegMem,                   // 0F E4: SIMD reg, r/m
    csRegMem,                   // 0F E5: SIMD reg, r/m
    csRegMem,                   // 0F E6: SIMD reg, r/m
    csRegMem,                   // 0F E7: SIMD r/m, reg
    csRegMem,                   // 0F E8: SIMD reg, r/m
    csRegMem,                   // 0F E9: SIMD reg, r/m
    csRegMem,                   // 0F EA: SIMD reg, r/m
    csRegMem,                   // 0F EB: SIMD reg, r/m
    csRegMem,                   // 0F EC: SIMD reg, r/m
    csRegMem,                   // 0F ED: SIMD reg, r/m
    csRegMem,                   // 0F EE: SIMD reg, r/m
    csRegMem,                   // 0F EF: SIMD reg, r/m

    // $Fx
    csRegMem,                   // 0F F0: SIMD reg, r/m
    csRegMem,                   // 0F F1: SIMD reg, r/m
    csRegMem,                   // 0F F2: SIMD reg, r/m
    csRegMem,                   // 0F F3: SIMD reg, r/m
    csRegMem,                   // 0F F4: SIMD reg, r/m
    csRegMem,                   // 0F F5: SIMD reg, r/m
    csRegMem,                   // 0F F6: SIMD reg, r/m
    csRegMem,                   // 0F F7: SIMD reg, r/m
    csRegMem,                   // 0F F8: SIMD reg, r/m
    csRegMem,                   // 0F F9: SIMD reg, r/m
    csRegMem,                   // 0F FA: SIMD reg, r/m
    csRegMem,                   // 0F FB: SIMD reg, r/m
    csRegMem,                   // 0F FC: SIMD reg, r/m
    csRegMem,                   // 0F FD: SIMD reg, r/m
    csRegMem,                   // 0F FE: SIMD reg, r/m
    0
  );

{ Pointer increment
}
function pinc(Ptr: pointer; incr: integer = 1): pointer;
begin
  Result := pointer(integer(Ptr) + incr);
end;

{ Returns the length (in bytes) of the cpu instruction code at "Code". There
  should be at least "Size" valid bytes to read at that location.
}
function AnalyzeCpuInstruction(var Insn: TAnalyzedInstruction;
  CodePtr: pointer; Size: integer): integer;
var
  code: word;
  tag, modrm, sib: byte;
  len, disp: integer;
  opsz, adsz: integer;

  function FetchByte(incr: integer = 1): byte;
  begin
    if Size >= incr then
      Result := PByte(integer(CodePtr))^
    else
      Result := $CC;

    inc(integer(CodePtr), incr);
    dec(Size, incr);
    inc(len, incr);
  end;

begin
  Result := 0;

  FillChar(Insn, SizeOf(Insn), 0);
  Insn.Address := CodePtr;

  len := 0;
  opsz := 4;
  adsz := 4;
  code := FetchByte;
  tag := CInitTable[code];

  while (tag and csModeMask) = csPrefixByte do
  begin
    Insn.Prefixes := Insn.Prefixes or (1 shl (tag and csPfxMask));

    case tag and csPfxMask of
      csPfxEscape: begin
        code := $0F00 or FetchByte;
        tag := CPrefix0FTable[code and $FF];
      end;

      csPfxDataSize: begin
        opsz := 6 - opsz;
        code := FetchByte;
        tag := CInitTable[code];
      end;

      csPfxAddrSize: begin
        adsz := 6 - adsz;
        code := FetchByte;
        tag := CInitTable[code];
      end;

      else begin
        code := FetchByte;
        tag := CInitTable[code];
      end;
    end;
  end;

  if tag = 0 then
    exit;

  if (tag and csOneMoreByte) <> 0 then
    FetchByte;

  Insn.Flags := (tag and csFinalInsn) shr 5;

  if (tag and csRegMem) <> 0 then
  begin
    Insn.Flags := Insn.Flags or AIFL_HAS_MODRM;

    modrm := FetchByte;
    sib := 0;
    disp := 0;

    if adsz = 2 then
    begin
      // 16-bit addressing mode R/M byte
      // +---+---+---+---+---+---+---+---+
      // |  Mod  | -   -   - |    R/M    |
      // +---+---+---+---+---+---+---+---+
      //
      // Mod:
      //      00 = no displacement
      //      01 = 8-bit sign extended displacement
      //      10 = 32-bit displacement
      //      11 = register
      // R/M (if Mod <> 11):
      //      000 = ds:[bx + si]
      //      001 = ds:[bx + di]
      //      010 = ss:[bp + si]
      //      011 = ss:[bp + di]
      //      100 = ds:[si]
      //      101 = ds:[di]
      //      110 = ss:[bp]     (if Mod = 01 or 10)
      //            ds:[disp32] (if Mod = 00)
      //      111 = ds:[bx]

      case (modrm shr 6) and 3 of
        0: if (modrm and 7) = 6 then
             disp := 2;
        1: disp := 1;
        2: disp := 2;
      end;
    end
    else
    begin
      // 32-bit addressing mode R/M byte
      // +---+---+---+---+---+---+---+---+
      // |  Mod  | -   -   - |    R/M    |
      // +---+---+---+---+---+---+---+---+
      //
      // Mod:
      //      00 = no displacement
      //      01 = 8-bit sign extended displacement
      //      10 = 32-bit displacement
      //      11 = register
      // R/M (if Mod <> 11):
      //      000 = [eax {+ disp}]
      //      001 = [ecx {+ disp}]
      //      010 = [edx {+ disp}]
      //      011 = [ebx {+ disp}]
      //      100 = SIB escape
      //      101 = [ebp {+ disp}] (special case!)
      //      110 = [edi {+ disp}]
      //      111 = [esi {+ disp}]
      //
      // S/I/B Byte (Scale/Index/Base)
      // +---+---+---+---+---+---+---+---+
      // | Scale |   Index   |   Base    |
      // +---+---+---+---+---+---+---+---+
      // Scale: shift factor for index
      // Index: register index (100 = ESP meaning "no index register")
      // Base: register index (special case for 101 = EBP)
      //
      // Effective address:
      //      [Base + Index shl Scale {+ disp}]
      //
      // Special cases:
      //      if EBP is the base register (resp. only register) and there
      //      was no displacement, then a 32-bit displacement is added and
      //      the EBP register is ignored, i.e. not taken into account.

      case (modrm shr 6) and 3 of
        0: // Special case: Mode=0 and Reg=EBP => no reg but 32-bit displacement
           if (modrm and 7) = 5 then
             disp := 4;
        1: disp := 1;
        2: disp := 4;
      end;

      if ((modrm shr 6) and 3) <> 3 then
        if (modrm and 7) = 4 then
        begin
          // SIB (Scale/Index/Base) byte is following
          // Special case: Mode=0 and Base=EBP => no base but 32-bit displacement
          sib := FetchByte;
          if (sib and 7) = 5 then
            if disp = 0 then
              disp := 4;
        end;
    end;

    if (tag and csSpecialCheck) <> 0 then
    begin
      case code of
        $F6, $F7:
          // only "test r/m, imm" has an immediate operand following
          if (modrm and $38) <> 0 then
            tag := csNone;

        $FF:
          case (modrm shr 3) and 7 of
            2, 3: begin
              // 2, 3: call
              Insn.Flags := Insn.Flags or AIFL_BRANCH;
              if (modrm and $C0) = $C0 then
                Insn.BranchType := AIBT_REG16 + (opsz shr 2)
              else
                Insn.BranchType := AIBT_MEM16 + (opsz shr 2);
            end;

            4, 5: begin
              // 4, 5: jmp
              Insn.Flags := Insn.Flags or AIFL_FINAL or AIFL_BRANCH;
              if (modrm and $C0) = $C0 then
                Insn.BranchType := AIBT_REG16 + (opsz shr 2)
              else
              begin
                Insn.BranchType := AIBT_MEM16 + (opsz shr 2);

                if disp = 4 then
                begin
                //Insn.Flags := Insn.Flags or AIFL_DEST_KNOWN;
                  Insn.Branch := PCardinal(CodePtr)^;
                  Insn.BranchAddr := CodePtr;

                  //00...100 10...101  <- check values
                  if (modrm and $C7) = $04 then
                    if (sib and $C7) = $85 then
                      Insn.BranchCount := 1;
                end;
              end;
            end;
          end;
      end;
    end;

    if Size < disp then
      exit;

    Insn.AddrSize := disp;
    Insn.AddrAddr := CodePtr;

    FetchByte(disp);
  end;

  case tag and csOpMask of
    csImmByte:      // immediate 8-bit value
      disp := 1;

    csImmWord:      // immediate 16/32-bit value [OpSz]
      disp := opsz;
    csImm16:        // ret imm16
      disp := 2;
    csImm24:        // enter imm8, imm16
      disp := 3;
    csAddress:      // 16/32-bit near address [AdSz]
      disp := adsz;

    csPointer: begin
      // 16:16/32-bit far address [AdSz]
      disp := 2 + adsz;
      if Size < disp then
        exit;

      Insn.Flags := Insn.Flags or AIFL_BRANCH;
      Insn.BranchType := AIBT_PTR32 + (adsz shr 2);
      Insn.BranchAddr := CodePtr;
    end;

    csDispByte: begin
      // short displacement (8-bit)
      disp := 1;
      if Size < disp then
        exit;

      Insn.Flags := Insn.Flags or AIFL_BRANCH or AIFL_DEST_KNOWN;
      if (Insn.Flags and AIFL_FINAL) = 0 then
        Insn.Flags := Insn.Flags or AIFL_CONDITIONAL;

      if opsz = 4 then
        Insn.Branch := cardinal(integer(CodePtr) + 1 + PShortInt(CodePtr)^)
      else
        Insn.Branch := cardinal(integer(CodePtr) + 1 + PShortInt(CodePtr)^) and $FFFF;

      Insn.BranchType := AIBT_DISP8;
      Insn.BranchAddr := CodePtr;
    end;

    csDispWord, csDispCall: begin
      // near displacement (16/32-bit) [OpSz]
      disp := opsz;
      if Size < disp then
        exit;

      Insn.Flags := Insn.Flags or AIFL_BRANCH or AIFL_DEST_KNOWN;
      if (tag and csOpMask) <> csDispCall then
        if (Insn.Flags and AIFL_FINAL) = 0 then
          Insn.Flags := Insn.Flags or AIFL_CONDITIONAL;

      if opsz = 4 then
        Insn.Branch := cardinal(integer(CodePtr) + 4 + PInteger(CodePtr)^)
      else
        Insn.Branch := cardinal(integer(CodePtr) + 2 + PSmallInt(CodePtr)^) and $FFFF;

      Insn.BranchType := AIBT_DISP16 + (opsz shr 2);
      Insn.BranchAddr := CodePtr;
    end;

    else            // no operand
      disp := 0;
  end;

  if disp > 0 then
    case tag and csOpMask of
      csImmByte,
      csImmWord,
      csImm16,
      csImm24,
      csAddress,
      csPointer: begin
        Insn.ImmSize := disp;
        Insn.ImmAddr := CodePtr;
      end;
    end;

  FetchByte(disp);
  if (tag = 0) or (Size < 0) or (len > MAX_IA32_INSTRUCTION_LENGTH) then
    exit;

  if (code = $68) and (Insn.ImmSize = 4) and (Size > 0) then
  begin
    // "push imm32", check for "ret" or "push fs:[...]"
    if (Size >= 1) and (PByte(CodePtr)^ = $C3) then
      Insn.Flags := Insn.Flags or AIFL_CODEREF_I
    else if (Size >= 3) and
            (PByte(CodePtr)^ = $64) and
            (PByte(pinc(CodePtr, 1))^ = $FF) and
            ((PByte(pinc(CodePtr, 2))^ and $38) = $30) and
            ((PByte(pinc(CodePtr, 2))^ and $C0) <> $C0) then
      Insn.Flags := Insn.Flags or AIFL_CODEREF_I
  end;

  Insn.Size := len;
  Result := len;
end;

{ Returns the length (in bytes) of the cpu instruction code at "CodePtr".
  There should be at least "Size" valid bytes to read at that location.
}
function GetCpuInstructionLength(CodePtr: pointer; Size: integer): integer;
var
  Insn: TAnalyzedInstruction;
begin
  Result := AnalyzeCpuInstruction(Insn, CodePtr, Size);
end;

{ Returns TRUE if the instruction pointed to is a return instruction.
  Note: iret is also taken as return instruction
}
function IsReturnInstruction(CodePtr: pointer; Size: integer): boolean;
const
  Mask = AIFL_FINAL or AIFL_BRANCH or AIFL_CONDITIONAL;
  Value = AIFL_FINAL;
var
  Insn: TAnalyzedInstruction;
begin
  Result := (AnalyzeCpuInstruction(Insn, CodePtr, Size) > 0) and
            ((Insn.Flags and Mask) = Value);
end;

{ Returns TRUE if the instruction pointed to is an unconditional jump instruction.
}
function IsJumpInstruction(CodePtr: pointer; Size: integer): boolean;
const
  Mask = AIFL_FINAL or AIFL_BRANCH or AIFL_CONDITIONAL;
  Value = AIFL_FINAL or AIFL_BRANCH;
var
  Insn: TAnalyzedInstruction;
begin
  Result := (AnalyzeCpuInstruction(Insn, CodePtr, Size) > 0) and
            ((Insn.Flags and Mask) = Value);
end;

{ Returns the length of the instruction IFF it is a no-op.
  Examples:  90               xchg  eax, eax
             86C0             xchg  al, al
             86C9             xchg  cl, cl
             87D2             xchg  edx, edx
             88C0             mov   al, al
             89C0             mov   eax, eax
             8AC0             mov   al, al
             8BC0             mov   eax, eax
             8D4000           lea   eax, [eax + 0]
             8D642400         lea   esp, [esp + 0]
             8DA42400000000   lea   esp, [esp + 0]
}
function IsNopInstruction(CodePtr: pointer; Size: integer): integer;
var
  rm, sib: byte;
begin
  Result := 0;

  if (Size >= 1) and
     (PByte(CodePtr)^ = $90) then
    Result := 1;

  if (Result = 0) and
     (Size >= 2) and
     ($86 <= PByte(CodePtr)^) and (PByte(CodePtr)^ <= $8B) then
  begin
    rm := PByte(pinc(CodePtr, 1))^;
    if ((rm and $C0) = $C0) and
       ((rm and $07) = ((rm shr 3) and $07)) then
      Result := 2;
  end;

  if (Result = 0) and
     (Size >= 3) and
     (PByte(CodePtr)^ = $8D) then
  begin
    rm := PByte(pinc(CodePtr, 1))^;

    if ((rm and $C0) = $40) and
       ((rm and $07) = ((rm shr 3) and $07)) and
       ((rm and $07) <> 4) and ((rm and $07) <> 5) and
       (PByte(pinc(CodePtr, 2))^ = 0) then
      Result := 3;

    if (Size >= 4) and ((rm and $C7) = $44) then
    begin
      sib := PByte(pinc(CodePtr, 2))^;
      if ((sib and $F8) = $20) and
         ((sib and $07) = ((rm shr 3) and $07)) and
         (PByte(pinc(CodePtr, 3))^ = 0) then
        Result := 4;
    end;

    if (Size >= 7) and ((rm and $C7) = $84) then
    begin
      sib := PByte(pinc(CodePtr, 2))^;
      if ((sib and $F8) = $20) and
         ((sib and $07) = ((rm shr 3) and $07)) and
         (PInteger(pinc(CodePtr, 3))^ = 0) then
        Result := 7;
    end;
  end;
end;

{ Returns the length (in bytes) of the sequence of cpu instruction codes at
  "Code" that are at least "MinSize" bytes in length. Returns ZERO if there
  is not enough space.
}
function GetCpuInstructionSequence(CodePtr: pointer; Size, MinSize: integer): integer;
// NEW VERSION
var
  Map: TCodeMap;
begin
  Map := AnalyzeCpuInstructionSequence(CodePtr, Size);
  Result := LengthOfCpuInstructionSequence(CodePtr, Map, MinSize);
end;

{ Very small integer stack implementation, just count, push and pop
}
type
  TStackList = array of integer;
  TStack = record
    Count: integer;
    List: TStackList;
  end;

procedure StackPush(var Stk: TStack; Value: integer);
begin
  if Stk.Count >= High(Stk.List) then
    SetLength(Stk.List, Length(Stk.List) + 64);

  Stk.List[Stk.Count] := Value;
  inc(Stk.Count);
end;

function StackPop(var Stk: TStack): integer;
begin
  if Stk.Count <= 0 then
    Result := -1
  else
  begin
    dec(Stk.Count);
    Result := Stk.List[Stk.Count];
  end;
end;

function StackPopMin(var Stk: TStack): integer;
var
  Pos, Best: integer;
begin
  if Stk.Count <= 0 then
    Result := -1
  else if Stk.Count <= 1 then
  begin
    dec(Stk.Count);
    Result := Stk.List[Stk.Count];
  end
  else
  begin
    Best := Stk.Count - 1;
    Pos := Best - 1;
    while Pos >= 0 do
    begin
      if Stk.List[Pos] < Stk.List[Best] then
        Best := Pos;
      dec(Pos);
    end;

    Result := Stk.List[Best];

    dec(Stk.Count);
    while Best < Stk.Count do
    begin
      Stk.List[Best] := Stk.List[Best + 1];
      inc(Best);
    end;
  end;
end;

{ This switch activates the complete analysis of the given memory region.
  This is normally not necessary if we consider the fact, that we want to
  analyze a single function, where each instruction should be reachable
  by a direct or indirect branch.

  Without this definition the analysis is about 10 times faster!
}
{.$DEFINE FULL_ANALYSIS}

{ Analyzes the CPU instruction sequence at "CodePtr" and returns a `map´ that
  describes each byte.

  The algorithm uses three stacks to keep track of addresses that have a
  special meaning:

  "CodeStack" contains branch target addresses. If we e.g. find the
    machine instruction

      je @empty

    we will continue with analyzing the next instruction but we will also
    push the address of @empty onto "CodeStack" to continue there later.

  "PtrStack" contains jump table addresses. If we e.g. find the
    machine instructions

      jmp [@switch + 4 * eax]

    we will push the address of @switch onto "PtrStack". When "CodeStack" runs
    empty, we will pick the addresses from "PtrStack" and look for a valid
    code address there. We will also put the next entry of the @switch table
    onto "PtrStack".

  "FarStack" contains branch target addresses like "CodeStack", but this
    stack is used for `far´ unconditional branches.

    After both "CodeStack" and "PtrStack" run empty, we will pop the smallest
    address stored on "FarStack". If this address is near enough to the end
    of the current end of analysis ("Max"), we will push it onto "CodeStack"
    and cycle through the loop once again. If not, the whole stack is dropped
    (there can be no nearer address on it) at the analysis is finished.

  Last but not least, if the symbol `FULL_ANALYSIS´ is defined (see above),
  if all three stacks run empty, we will look through the whole area up to
  "Max" for any unexplored block. The first one we find is then pushed onto
  the "CodeStack".
}
function AnalyzeCpuInstructionSequence(CodePtr: pointer; Size: integer): TCodeMap;
const
  FAR_THRESHOLD = 1024;
var
  CodeStack, PtrStack, FarStack: TStack;
  Insn: TAnalyzedInstruction;
  Index, Len, Step, Max, Alloc: integer;
{$IFDEF FULL_ANALYSIS}
  Min: integer;
{$ENDIF}

  procedure Grow(Limit: integer);
  var
    NewSize: integer;
  begin
    NewSize := (3 * Alloc) div 2;
    while Limit >= NewSize do
      NewSize := (3 * NewSize) div 2;

    SetLength(Result, NewSize);
    FillChar(Result[Alloc], NewSize - Alloc, 0);
    Alloc := NewSize;
  end;

begin
  // Initialize stack pointers (dyn. variables are auto-initialized)
  CodeStack.Count := 0;
  PtrStack.Count := 0;
  FarStack.Count := 0;

  // Pre-allocate result
  Alloc := 512;
  //Alloc := Size;
  SetLength(Result, Alloc);
  FillChar(Result[0], Alloc, 0);

  // Push the first known instruction location onto the stack
  StackPush(CodeStack, 0);
  Max := 0;
{$IFDEF FULL_ANALYSIS}
  Min := 0;
{$ENDIF}

  while true do
  begin
    { The "CodeStack" contains indexes of known (possible) cpu instructions.
      The stack grows when we find an undiscovered branch and is shrinks
      each time the loop cycles.
    }
    while CodeStack.Count > 0 do
    begin
      Index := StackPop(CodeStack);

      if Index + MAX_IA32_INSTRUCTION_LENGTH >= Alloc then
        Grow(Index + MAX_IA32_INSTRUCTION_LENGTH);

      // Proceed as long as the area has not yet been analyzed
      while (Result[Index] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 do
      begin
        // Calculate maximum length of instruction
        Len := 1;
        while (Len < MAX_IA32_INSTRUCTION_LENGTH) and
              (Result[Index + Len] = 0) do
          inc(Len);

        // Analyze instruction
        Len := AnalyzeCpuInstruction(Insn, pinc(CodePtr, Index), Len);

        // Unknown or not enough bytes?
        if Len = 0 then
        begin
          if Index + 1 > Max then
            Max := Index + 1;

          Result[Index] := AISQ_TYPE_DATA or 1;
          break;
        end;

        // Remember max. size of function
        if Index + Len > Max then
          Max := Index + Len;

        // Mark as analyzed and store result (type and length)
        Result[Index] := Result[Index] or AISQ_TYPE_CODE or Len;
        for Step := 1 to Len - 1 do
          Result[Index + Step] := AISQ_TYPE_CODE;

        // Check for instructions that branch
        if (Insn.Flags and AIFL_BRANCH) <> 0 then
        begin
          if (Insn.Flags and AIFL_DEST_KNOWN) <> 0 then
          begin
            // Direct jump (destination is known)
            if (Insn.Flags and (AIFL_CONDITIONAL or AIFL_FINAL)) <> 0 then
            begin
              // It's not a call instruction (conditional or uncond. jump)
              // "Insn.Branch" is the target address
              Step := integer(Insn.Branch) - integer(CodePtr);
              if (0 <= Step) and (Step < Size) then
              begin
                if Step >= Alloc then
                  Grow(Step);

                // Branch is inside our region
                if (Insn.Flags and AIFL_CONDITIONAL) <> 0 then
                  Result[Step] := Result[Step] or AISQ_COND_TARGET
                else
                  Result[Step] := Result[Step] or AISQ_UNCOND_TARGET;

                if (Result[Step] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 then
                  if ((Insn.Flags and AIFL_CONDITIONAL) <> 0) or (Step < Max) then
                    // Conditionals do always belong to the current function
                    StackPush(CodeStack, Step)
                  else
                    // Unconditional jumps beyond "Max" are delayed
                    StackPush(FarStack, Step);
              end;
            end;
          end
          else if Insn.BranchCount > 0 then
          begin
            // Indirect jump, "BranchCount" marks a case/switch jump table.
            // -> Branch is address of first list entry, just push onto "PtrStack"
            Step := integer(Insn.Branch) - integer(CodePtr);

            if (0 <= Step) and (Step + 4 <= Size) then
            begin
              if Step >= Alloc then
                Grow(Step);

              if (Result[Step] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 then
              begin
                Result[Step] := Result[Step] or AISQ_COND_TARGET;
                StackPush(PtrStack, Step);
              end;
            end;
          end;
{$IFDEF FULL_ANALYSIS}
        end
        else if (Insn.Flags and AIFL_CODEREF_I) <> 0 then
        begin
          // Immediate operand may have a code reference

          Step := PInteger(Insn.ImmAddr)^ - integer(CodePtr);
          if (0 <= Step) and (Step < Size) then
          begin
            if Step >= Alloc then
              Grow(Step);

            if (Result[Step] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 then
            begin
              Result[Step] := Result[Step] or AISQ_COND_TARGET;
              StackPush(FarStack, Step);
            end;
          end;
{$ENDIF}
        end;

        // If this is a final instruction then just stop
        if (Insn.Flags and AIFL_FINAL) <> 0 then
          break;

        // Otherwise step to next instruction
        inc(Index, Len);
      end;
    end;

    { Assertion A1: At this point, one condition holds
      1. "CodeStack" is empty
      Reason: the "while" loop above.
    }

    { The "PtrStack" contains indexes of case/switch jump tables.
      Try each one and check if we can expand it.
    }
    while (CodeStack.Count = 0) and (PtrStack.Count > 0) do
    begin
      Index := StackPop(PtrStack);

      if Index + 3 >= Alloc then
        Grow(Index + 3);

      if (Result[Index] and AISQ_SIZE_MASK) = 0 then
      begin
        Result[Index] := AISQ_COND_TARGET or AISQ_TYPE_POINTER or 4;
        Result[Index + 1] := AISQ_TYPE_POINTER;
        Result[Index + 2] := AISQ_TYPE_POINTER;
        Result[Index + 3] := AISQ_TYPE_POINTER;

        // Push the next data entry to check
        if Index + 8 <= Size then
          StackPush(PtrStack, Index + 4);

        // Also check the instruction it is pointing to
        Step := PInteger(pinc(CodePtr, Index))^ - integer(CodePtr);
        if (0 <= Step) and (Step < Size) then
        begin
          if Step >= Alloc then
            Grow(Step);

          if (Result[Step] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 then
          begin
            Result[Step] := Result[Step] or AISQ_COND_TARGET;
            StackPush(CodeStack, Step);
          end;
        end;
      end;
    end;

    { Assertion A2: At this point, two conditions are possible
      Either 1. "CodeStack" is not empty
          or 2. both "CodeStack" AND "PtrStack" are empty
      Reason: the "while" loop above.
    }

    { The "FarStack" holds the same kind of addresses as the "CodeStack" but
      is used for unconditional jump addresses that point beyond "Max" and
      thus may be a kind of tail recursion.
    }
    while (CodeStack.Count = 0) and (FarStack.Count > 0) do
    begin
      Index := StackPopMin(FarStack);
      if Index >= Max + FAR_THRESHOLD then
        // This once was "FarStack.Count := 0"
        break
      else if (0 <= Index) and (Index < Size) then
      begin
        if Index >= Alloc then
          Grow(Index);

        if (Result[Index] and (AISQ_TYPE_MASK or AISQ_SIZE_MASK)) = 0 then
          StackPush(CodeStack, Index);
      end;
    end;

    { Assertion A3: At this point, two conditions are possible
      Either 1. "CodeStack" is not empty
          or 2. "CodeStack" AND "PtrStack" AND "FarStack" are empty
      Reason: assertion A2 plus the "while" loop above.
    }

    if CodeStack.Count = 0 then
    begin
{$IFDEF FULL_ANALYSIS}
      { If no more stack entry is left, check up to "Max" if there are any
        unexplored regions and push the first to be found onto the code stack.
      }
      while (Min < Max) and ((Result[Min] and AISQ_SIZE_MASK) <> 0) do
        inc(Min, Result[Min] and AISQ_SIZE_MASK);

      if Min < Max then
        StackPush(CodeStack, Min)
      else
        break;
{$ELSE}
      Index := 0;
      while Index < Max do
      begin
        if (Result[Index] and AISQ_SIZE_MASK) = 0 then
        begin
          Len := 1;
          while (((Index + Len) and 7) <> 0) and
                ((Result[Index + Len] and AISQ_SIZE_MASK) = 0) do
            inc(Len);

          Result[Index] := Result[Index] or AISQ_TYPE_UNKNOWN or Len;
          for Step := 1 to Len - 1 do
            Result[Index + Step] := Result[Index + Step] or AISQ_TYPE_UNKNOWN;

          inc(Index, Len);
        end
        else
          inc(Index, Result[Index] and AISQ_SIZE_MASK);
      end;

      break;
{$ENDIF}
    end;
  end;

  { 1. If there are NOPs after this piece of code, collect and add them.
    2. If the space after this piece of code is filled with $CC, add them.
  }
  while Max < Size do
  begin
    if PByte(pinc(CodePtr, Max))^ = $CC then
      Len := 1
    else
    begin
      Len := IsNopInstruction(pinc(CodePtr, Max), Size - Max);
      if (Len = 0) or (Max + Len > Size) then
        break;
    end;

    if Max + Len >= Alloc then
      Grow(Max + Len);

    Result[Max] := Result[Max] or AISQ_TYPE_CODE or Len;
    for Step := 1 to Len - 1 do
      Result[Max + Step] := AISQ_TYPE_CODE;

    inc(Max, Len);
  end;

  { Done, the function's length is "Max" - the maximum instruction we
    have analyzed.
  }
  SetLength(Result, Max);
end;

{ Returns the length of a sequence of CPU instructions at "CodePtr", that
  has been analyzed by the function above (which returned "Map") and that
  has at least "MinSize" bytes in length.

  Given the analyzed range of code, we need to find the first N bytes of
  complete CPU instructions such that there is

  a) no jump from the outside into the inside  and
  b) no *short* jump from the inside to the outside.
}
function LengthOfCpuInstructionSequence(CodePtr: pointer; Map: TCodeMap;
  MinSize: integer): integer;
var
  Index, Size, Len, Offs: integer;
  Insn: TAnalyzedInstruction;
begin
  Result := 0;
  Index := 0;
  Size := Length(Map);

  if MinSize > Size then
    exit;

  while Index < Size do
  begin
    Len := Map[Index] and AISQ_SIZE_MASK;
    if Len < 1 then
      Len := 1;

    if Index < MinSize then
    begin
      if MinSize < Index + Len then
        MinSize := Index + Len;

      // Index is smaller than our current region -
      // just check for short displacements.
      if (Map[Index] and AISQ_TYPE_MASK) = AISQ_TYPE_CODE then
      begin
        if AnalyzeCpuInstruction(Insn, pinc(CodePtr, Index), Size - Index) <> Len then
          raise Exception.Create('LengthOfCpuInstructionSequence: invalid map');

        if (Insn.Flags and AIFL_DEST_KNOWN) <> 0 then
          if Insn.BranchType = AIBT_DISP8 then
          begin
            // Short branch *BEFORE* our piece of code is not possible (yet)
            if Insn.Branch < cardinal(CodePtr) then
              exit;

            // Short jump *BEYOND* MinSize - expand it.
            Offs := integer(Insn.Branch) - integer(CodePtr);
            if (MinSize < Offs) and (Offs < Size) then
{$IFDEF FULL_ANALYSIS}
              if ((Map[Offs] and AISQ_TYPE_MASK) = AISQ_TYPE_CODE) and
                 ((Map[Offs] and AISQ_SIZE_MASK) > 0) then
{$ENDIF}
                MinSize := Offs;
          end;
      end;
    end
    else
    begin
      // Index is beyond our current region -
      // check for references to the current region and expand MinSize if so
      case Map[Index] and AISQ_TYPE_MASK of
        AISQ_TYPE_UNKNOWN,
        AISQ_TYPE_DATA: begin
        end;

        AISQ_TYPE_POINTER: begin
          if Len = 4 then
          begin
            Offs := PInteger(pinc(CodePtr, Index))^ - integer(CodePtr);
            if (0 < Offs) and (Offs < MinSize) then
              MinSize := Index + Len;
          end;
        end;

        AISQ_TYPE_CODE: begin
          if AnalyzeCpuInstruction(Insn, pinc(CodePtr, Index), Size - Index) <> Len then
            raise Exception.Create('LengthOfCpuInstructionSequence: invalid map');

          if (Insn.Flags and AIFL_DEST_KNOWN) <> 0 then
          begin
            Offs := integer(Insn.Branch) - integer(CodePtr);
            if (0 < Offs) and (Offs < MinSize) then
{$IFDEF FULL_ANALYSIS}
              if ((Map[Offs] and AISQ_TYPE_MASK) = AISQ_TYPE_CODE) and
                 ((Map[Offs] and AISQ_SIZE_MASK) > 0) then
{$ENDIF}
                MinSize := Index + Len;
          end;

          if Insn.AddrSize = 4 then
          begin
            Offs := PInteger(Insn.AddrAddr)^ - integer(CodePtr);
            if (0 < Offs) and (Offs < MinSize) then
              MinSize := Index + Len;
          end;

          if Insn.ImmSize = 4 then
          begin
            Offs := PInteger(Insn.ImmAddr)^ - integer(CodePtr);
            if (0 < Offs) and (Offs < MinSize) then
              MinSize := Index + Len;
          end;
        end;
      end;
    end;

    inc(Index, Len);
  end;

  Result := MinSize;
end;

{ Uses a map calculated by the previous function to copy cpu instructions
  from "CodePtr" to "Destination". Relocates everything it detects, though
  this is far from being complete.
}
procedure CopyCpuInstructionSequence(CodePtr: pointer; Size: integer;
  Map: TCodeMap; Destination: pointer);
var
  Index, Delta, Len: integer;
  Insn: TAnalyzedInstruction;
  Ptr: Pinteger;
begin
  Move(CodePtr^, Destination^, Size);

  if Size > Length(Map) then
    Size := Length(Map);

  Index := 0;
  Delta := integer(Destination) - integer(CodePtr);
  while Index < Size do
  begin
    Len := Map[Index] and AISQ_SIZE_MASK;
    if Len < 1 then
      Len := 1;

    case Map[Index] and AISQ_TYPE_MASK of
      AISQ_TYPE_UNKNOWN,
      AISQ_TYPE_DATA: begin
      end;

      AISQ_TYPE_POINTER: begin
        if Len = 4 then
        begin
          Ptr := pinc(Destination, Index);
          if (cardinal(CodePtr) <= cardinal(Ptr^)) and
             (cardinal(Ptr^) <= cardinal(pinc(CodePtr, Size))) then
            inc(Ptr^, Delta);
        end;
      end;

      AISQ_TYPE_CODE: begin
        if AnalyzeCpuInstruction(Insn, pinc(Destination, Index), Size - Index) <> Len then
          raise Exception.Create('CopyCpuInstructionSequence: invalid map');

        if (Insn.Flags and AIFL_DEST_KNOWN) <> 0 then
          if (cardinal(Destination) > cardinal(Insn.Branch)) or
             (cardinal(Insn.Branch) > cardinal(pinc(Destination, Size))) then
          begin
            (* Ignored for now, should have been checked in the function above!

            if Insn.BranchType <> AIBT_DISP32 then
              { This may happen if there is a short jump *BEFORE* the piece
                of code we want to copy. Later this should be fixed by
                expanding the area to be copied (in front) and storing
                fixup jumps there.
              }
              raise Exception.Create('CopyCpuInstructionSequence: unrelocatable short jump');
            *)

            if Insn.BranchType = AIBT_DISP32 then
              dec(Pinteger(Insn.BranchAddr)^, Delta)
          end;

        if Insn.AddrSize = 4 then
        begin
          Ptr := Insn.AddrAddr;
          if (cardinal(CodePtr) <= cardinal(Ptr^)) and
             (cardinal(Ptr^) <= cardinal(pinc(CodePtr, Size))) then
            inc(Ptr^, Delta);
        end;

        if Insn.ImmSize = 4 then
        begin
          Ptr := Insn.ImmAddr;
          if (cardinal(CodePtr) <= cardinal(Ptr^)) and
             (cardinal(Ptr^) <= cardinal(pinc(CodePtr, Size))) then
            inc(Ptr^, Delta);
        end;
      end;
    end;

    inc(Index, Len);
  end;
end;

end.
