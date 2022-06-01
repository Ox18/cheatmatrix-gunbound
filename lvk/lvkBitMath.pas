{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains function related to bits and bytes. It acts as a support
    unit for the encryption, checksum and compression filters.
}
unit lvkBitMath;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBitMath.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    This function rearranges the ordering of the bytes in the LongWord to
    the exact opposite.

    For instance, value $12345678 will be swapped to $78563412.

  Parameters:
    X - The value to swap the byte ordering of.
  See also:
    SwapHiLo64
}
function SwapHiLo32(X: LongWord): LongWord;

{ Description:
    This function rearranges the ordering of the bytes in the Int64 to
    the exact opposite.

    For instance, value $0123456789ABCDEF will be swapped to
    $EFCDAB8967452301.

  Parameters:
    X - The value to swap the byte ordering of.
  See also:
    SwapHiLo32
}
function SwapHiLo64(X: Int64): Int64;

{ Description:
    This function rotates a LongWord to the left by a given amount of bits.
    Bits that are rotated out on the left side are immediately rotated in on
    the right side.
  Parameters:
    a - The value to rotate.
    s - The number of bits to rotate.
  See also:
    ROR
}
function ROL(a, s: LongWord): LongWord;

{ Description:
    This function rotates a LongWord to the right by a given amount of bits.
    Bits that are rotated out on the right side are immediately rotated in on
    the left side.
  Parameters:
    a - The value to rotate.
    s - The number of bits to rotate.
  See also:
    ROL
}
function ROR(a, s: LongWord): LongWord;

implementation

function ROL(a, s: LongWord): LongWord;
asm
	mov		ecx,s
	rol		eax,cl
end;

function ROR(a, s: LongWord): LongWord;
asm
	mov		ecx,s
	ror		eax,cl
end;

function SwapHiLo32(X: LongWord): LongWord;
asm
  bswap eax
end;

function SwapHiLo64(X: Int64): Int64;
var
	A	: array[0..1] of LongWord absolute X;
	B	: array[0..1] of LongWord absolute Result;
begin
	B[1] := SwapHiLo32(A[0]);
	B[0] := SwapHiLo32(A[1]);
end;

end.
 