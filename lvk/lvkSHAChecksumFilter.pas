{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the SHA message digest
    algorithm.
}
unit lvkSHAChecksumFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSHAChecksumFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicChecksumFilters;

{ Description:
    This function returns a stream filter that implements the SHA
    message digest algorithm.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    IChecksumStreamFilter
}
function NewSHAChecksumFilter: IStreamFilter;

implementation

uses
  lvkBitMath, Math;

type
  TSignature = array[0..4] of LongWord;
  TBlock = array[0..63] of Byte;
  TSHAChecksumFilter = class(TlvkBaseChecksumFilter)
  private
    FSignature  : TSignature;
    FBlock      : TBlock;
    FInBlock    : Integer;
    FLength     : Int64;

    procedure FlushBlock;
    function SignatureToChecksumBytes(
      const Signature: TSignature): TChecksumBytes;
    
  protected
    // Internal
    procedure Process(const Data; const DataSize: Integer); override;
    function CalculateChecksum: TChecksumBytes; override;
    procedure ResetChecksum; override;
  end;
  
function NewSHAChecksumFilter: IStreamFilter;
begin
  Result := TSHAChecksumFilter.Create as IStreamFilter;
end;

{ TSHAChecksumFilter }

const
	S11	=  7;		S12	= 12;		S13	= 17;		S14	= 22;
	S21	=  5;		S22	=  9;		S23	= 14;		S24	= 20;
	S31	=  4;		S32	= 11;		S33	= 16;		S34	= 23;
	S41	=  6;		S42	= 10;		S43	= 15;		S44	= 21;

function TSHAChecksumFilter.CalculateChecksum: TChecksumBytes;
var
	DummyBlock		: array[0..63] of Byte;
	DummyByte			: Byte;
	OldBlock			: TBlock;
	OldInBlock		: Integer;
	OldSignature	: TSignature;
	OldLength			: Int64;
  Temp          : Int64;
begin
	// Save old data before doing this
	OldBlock := FBlock;
	OldInBlock := FInBlock;
	OldLength := FLength;
	OldSignature := FSignature;

	// We need to pad with 1 1-bit and the rest 0-bits
	DummyByte := 128;
	FillChar(DummyBlock, SizeOf(DummyBlock), 0);

	// Now pad the data
	Process(DummyByte, 1);
	repeat
		if FInBlock >= 56 then
			Process(DummyBlock, 64-FInBlock)
		else
			Process(DummyBlock, 56-FInBlock);
	until FInBlock = 56;

  Temp := SwapHiLo64(OldLength);
	Process(Temp , SizeOf(Temp));

	// Copy the signature
	Result := SignatureToChecksumBytes(FSignature);

	// Restore old data
	FBlock := OldBlock;
	FInBlock := OldInBlock;
	FLength := OldLength;
	FSignature := OldSignature;
end;

{$R-,Q-}
procedure TSHAChecksumFilter.FlushBlock;
var
	Round			: Integer;
	A,B,C,D,E	: LongWord;
	Temp			: LongWord;
	W					: array[0..79] of LongWord;
	i					: Integer;
const
	K	: array[0..3] of LongWord = (
		$5a827999, $6ed9eba1, $8f1bbcdc, $ca62c1d6
	);

	function F(B,C,D: LongWord): LongWord;
	begin
		Result := 0;
		case Round of
			0..19		: Result := (B and C) or ((not B) and D);
			20..39	: Result := B xor C xor D;
			40..59	: Result := (B and C) or (B and D) or (C and D);
			60..79	: Result := B xor C xor D;
		end; // case Round
	end; // function TSHA.FlushBlock.F

begin
	Move(FBlock, W, SizeOf(FBlock));
	for i := 0 to 15 do
		W[i] := SwapHiLo32(W[i]);

	for i := 16 to 79 do
	begin
		W[i] := ROL(W[i-16] xor W[i-14] xor W[i-8] xor W[i-3], 1);
	end;

	A := FSignature[0];
	B := FSignature[1];
	C := FSignature[2];
	D := FSignature[3];
	E := FSignature[4];

	for Round := 0 to 79 do
	begin
		Temp := ROL(A, 5) + E + K[Round div 20] + W[Round] + F(B,C,D);
		E := D;
		D := C;
		C := ROL(B, 30);
		B := A;
		A := Temp;
	end;

	Inc(FSignature[0], A);
	Inc(FSignature[1], B);
	Inc(FSignature[2], C);
	Inc(FSignature[3], D);
	Inc(FSignature[4], E);

	FillChar(FBlock, SizeOf(FBlock), 0);
	FInBlock := 0;
end;
{$R+,Q+}

procedure TSHAChecksumFilter.Process(const Data; const DataSize: Integer);
var
  Written : Integer;
  DataPtr : PChar;
  ToCopy  : Integer;
begin
  DataPtr := @Data;
  Written := 0;

  while Written < DataSize do
  begin
    ToCopy := Min(SizeOf(FBlock)-FInBlock, DataSize-Written);
    Move(DataPtr^, FBlock[FInBlock], ToCopy);
    Inc(FInBlock, ToCopy);
    Inc(Written, ToCopy);
    Inc(DataPtr, ToCopy);
    Inc(FLength, ToCopy*8);

    if FInBlock = SizeOf(FBlock) then
      FlushBlock;
  end;
end;

procedure TSHAChecksumFilter.ResetChecksum;
const
	InitSignature	: TSignature = (
		$67452301, $efcdab89, $98badcfe, $10325476, $c3d2e1f0
	);
begin
  inherited;

	FillChar(FBlock, SizeOf(FBlock), 0);
	FInBlock := 0;
	FLength := 0;

	FSignature := InitSignature;
end;

function TSHAChecksumFilter.SignatureToChecksumBytes(
  const Signature: TSignature): TChecksumBytes;
begin
  SetLength(Result, SizeOf(TSignature));
  Move(Signature, Result[0], SizeOf(TSignature));
end;

end.

