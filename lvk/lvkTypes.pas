{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains various version-independant data types, all named so that
    it's easy to understand what kind of data type it is.
}
unit lvkTypes;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkTypes.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows;

type
  { Description:
      This is an extension to the Boolean data type called a Tristate. It
      can either be True, False, or Unknown.
  }
  TTriState = (
    // This is equivalent to False.
    tsFalse,
    // This is equivalent to True.
    tsTrue,
    // This is equivalent to Unknown or Null.
    tsUnknown);

  { Description:
      This is a 32-bit floating point data type.
  }
  PFloat32    = ^Float32;
  // <COMBINE PFloat32>
  Float32     = Single;

  { Description:
      This is a 48-bit floating point data type.
  }
  PFloat48    = ^Float48;
  // <COMBINE PFloat32>
  Float48     = Real48;

  { Description:
      This is a 64-bit floating point data type.
  }
  PFloat64    = ^Float64;
  // <COMBINE PFloat64>
  Float64     = Double;

  { Description:
      This is a 80-bit floating point data type.
  }
  PFloat80    = ^Float80;
  // <COMBINE PFloat80>
  Float80     = Extended;

  { Description:
      This is an 8-bit boolean data type.
  }
  PBoolean8   = ^Boolean8;
  // <COMBINE PBoolean8>
  Boolean8    = Boolean;

  { Description:
      This is a 16-bit boolean data type.
  }
  PBoolean16  = ^Boolean16;
  // <COMBINE PBoolean16>
  Boolean16   = WordBool;

  { Description:
      This is a 32-bit boolean data type.
  }
  PBoolean32  = ^Boolean32;
  // <COMBINE PBoolean32>
  Boolean32   = LongBool;

  { Description:
      This is an 8-bit signed integer data type.
  }
  PInt8       = ^Int8;
  // <COMBINE PInt8>
  Int8        = ShortInt;

  { Description:
      This is an 8-bit unsigned integer data type.
  }
  PUInt8      = ^UInt8;
  // <COMBINE PUInt8>
  UInt8       = Byte;

  { Description:
      This is a 16-bit signed integer data type.
  }
  PInt16      = ^Int16;
  // <COMBINE PInt16>
  Int16       = SmallInt;

  { Description:
      This is a 16-bit unsigned integer data type.
  }
  PUInt16     = ^UInt16;
  // <COMBINE PUInt16>
  UInt16      = Word;

  { Description:
      This is a 32-bit signed integer data type.
  }
  PInt32      = ^Int32;
  // <COMBINE PInt32>
  Int32       = LongInt;

  { Description:
      This is a 32-bit unsigned integer data type.
  }
  PUInt32     = ^UInt32;
  // <COMBINE PUInt32>
  UInt32      = LongWord;

  { Description:
      This is an 8-bit character data type.
  }
  PChar8      = ^Char8;
  // <COMBINE PChar8>
  Char8       = Char;

  { Description:
      This is a 16-bit character data type.
  }
  PChar16     = ^Char16;
  // <COMBINE PChar16>
  Char16      = WideChar;

const
  { Description:
      This constant array contains Boolean equivalent values for the
      TTriState data type.
    See also:
      TTriState
  }
  TriStateToBoolean : array[TTriState] of Boolean = (False, True, False);

{ Description:
    These functions swap the byte order around to the opposite for the
    different data types.
}
function SwapByteOrder(const Value: Int16): Int16; overload;
// <COMBINE SwapByteOrder@Int16
function SwapByteOrder(const Value: UInt16): UInt16; overload;
// <COMBINE SwapByteOrder@Int16
function SwapByteOrder(const Value: Int32): Int32; overload;
// <COMBINE SwapByteOrder@Int16
function SwapByteOrder(const Value: UInt32): UInt32; overload;

const
  MIN_UINT_8    = UInt8($00);               MAX_UINT_8  = UInt8($FF);
  MIN_INT_8     = Int8($80);                MAX_INT_8   = Int8($7F);
  MIN_UINT_16   = UInt16($0000);            MAX_UINT_16 = UInt16($FFFF);
  MIN_INT_16    = Int16($8000);             MAX_INT_16  = Int16($7FFF);
  MIN_UINT_32   = UInt32($00000000);        MAX_UINT_32 = UInt32($FFFFFFFF);
  MIN_INT_32    = Int32($80000000);         MAX_INT_32  = Int32($7FFFFFFF);
  MIN_INT_64    = Int64($8000000000000000); MAX_INT_64  = Int64($7FFFFFFFFFFFFFFF);

implementation

function SwapByteOrder(const Value: Int16): Int16;
asm
  xchg  al, ah
end;

function SwapByteOrder(const Value: UInt16): UInt16;
asm
  xchg  al, ah
end;

function SwapByteOrder(const Value: Int32): Int32;
asm
  bswap eax
end;

function SwapByteOrder(const Value: UInt32): UInt32;
asm
  bswap eax
end;

end.
