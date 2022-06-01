{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the MD5 (Message Digest 5)
    checksum filter.
}
unit lvkMD5ChecksumFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMD5ChecksumFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicChecksumFilters;

{ Description:
    This function creates and returns a new stream filter that implements the
    MD5 checksum algorithm.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    IChecksumStreamFilter
}
function NewMD5ChecksumFilter: IStreamFilter;

implementation

uses
  lvkBitMath, Math;

type
  TSignature = array[0..3] of LongWord;
  TBlock = array[0..63] of Byte;
  TMD5ChecksumFilter = class(TlvkBaseChecksumFilter)
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
  
function NewMD5ChecksumFilter: IStreamFilter;
begin
  Result := TMD5ChecksumFilter.Create as IStreamFilter;
end;

{ TMD5ChecksumFilter }

const
	S11	=  7;		S12	= 12;		S13	= 17;		S14	= 22;
	S21	=  5;		S22	=  9;		S23	= 14;		S24	= 20;
	S31	=  4;		S32	= 11;		S33	= 16;		S34	= 23;
	S41	=  6;		S42	= 10;		S43	= 15;		S44	= 21;

function TMD5ChecksumFilter.CalculateChecksum: TChecksumBytes;
var
	DummyBlock		: array[ 0..63 ] of Byte;
	DummyByte			: Byte;
	OldBlock			: TBlock;
	OldInBlock		: Integer;
	OldSignature	: TSignature;
	OldLength			: Int64;
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

	Process(OldLength, SizeOf(OldLength));

	// Copy the signature
	Result := SignatureToChecksumBytes(FSignature);

	// Restore old data
	FBlock := OldBlock;
	FInBlock := OldInBlock;
	FLength := OldLength;
	FSignature := OldSignature;
end;

{$R-,Q-}
procedure TMD5ChecksumFilter.FlushBlock;
var
	A,B,C,D	: LongWord;
	W				: array[0..15] of LongWord;

	procedure FF(var a: LongWord; b, c, d, x, s, ac: LongWord);

		function F(X, Y, Z: LongWord): LongWord;
		begin
			Result := (X and Y) or ((not X) and Z);
		end;

	begin
		a := a + F(b, c, d) + x + ac;
		a := ROL(a, s);
		a := a + b;
	end;

	procedure GG(var a: LongWord; b, c, d, x, s, ac: LongWord);

		function G(X, Y, Z: LongWord): LongWord;
		begin
			Result := (X and Z) or (Y and (not Z));
		end;

	begin
		Inc(a, G(b, c, d) + x + ac);
		a := ROL(a, s);
		Inc(a, b);
	end;

	procedure HH(var a: LongWord; b, c, d, x, s, ac: LongWord);

		function H(X, Y, Z: LongWord): LongWord;
		begin
			Result := X xor Y xor Z;
		end;

	begin
		Inc(a, H(b, c, d) + x + ac);
		a := ROL(a, s);
		Inc(a, b);
	end;

	procedure II(var a: LongWord; b, c, d, x, s, ac: LongWord);

		function I(X, Y, Z: LongWord): LongWord;
		begin
			Result := (Y xor (X or (not Z)));
		end;

	begin
		Inc(a, I(b, c, d) + x + ac);
		a := ROL(a, s);
		Inc(a, b);
	end; 

begin
	// Calculate W[]
	Move(FBlock, W, SizeOf(FBlock));

	// Put old signature values in A-D
	A := FSignature[0];
	B := FSignature[1];
	C := FSignature[2];
	D := FSignature[3];

	// Do the math
	// Round 1
	FF(A, B, C, D, W[ 0], S11, $d76aa478);
	FF(D, A, B, C, W[ 1], S12, $e8c7b756);
	FF(C, D, A, B, W[ 2], S13, $242070db);
	FF(B, C, D, A, W[ 3], S14, $c1bdceee);
	FF(A, B, C, D, W[ 4], S11, $f57c0faf);
	FF(D, A, B, C, W[ 5], S12, $4787c62a);
	FF(C, D, A, B, W[ 6], S13, $a8304613);
	FF(B, C, D, A, W[ 7], S14, $fd469501);
	FF(A, B, C, D, W[ 8], S11, $698098d8);
	FF(D, A, B, C, W[ 9], S12, $8b44f7af);
	FF(C, D, A, B, W[10], S13, $ffff5bb1);
	FF(B, C, D, A, W[11], S14, $895cd7be);
	FF(A, B, C, D, W[12], S11, $6b901122);
	FF(D, A, B, C, W[13], S12, $fd987193);
	FF(C, D, A, B, W[14], S13, $a679438e);
	FF(B, C, D, A, W[15], S14, $49b40821);

	// Round 2
	GG(A, B, C, D, W[ 1], S21, $f61e2562);
	GG(D, A, B, C, W[ 6], S22, $c040b340);
	GG(C, D, A, B, W[11], S23, $265e5a51);
	GG(B, C, D, A, W[ 0], S24, $e9b6c7aa);
	GG(A, B, C, D, W[ 5], S21, $d62f105d);
	GG(D, A, B, C, W[10], S22, $02441453);
	GG(C, D, A, B, W[15], S23, $d8a1e681);
	GG(B, C, D, A, W[ 4], S24, $e7d3fbc8);
	GG(A, B, C, D, W[ 9], S21, $21e1cde6);
	GG(D, A, B, C, W[14], S22, $c33707d6);
	GG(C, D, A, B, W[ 3], S23, $f4d50d87);
	GG(B, C, D, A, W[ 8], S24, $455a14ed);
	GG(A, B, C, D, W[13], S21, $a9e3e905);
	GG(D, A, B, C, W[ 2], S22, $fcefa3f8);
	GG(C, D, A, B, W[ 7], S23, $676f02d9);
	GG(B, C, D, A, W[12], S24, $8d2a4c8a);

	// Round 3
	HH(A, B, C, D, W[ 5], S31, $fffa3942);
	HH(D, A, B, C, W[ 8], S32, $8771f681);
	HH(C, D, A, B, W[11], S33, $6d9d6122);
	HH(B, C, D, A, W[14], S34, $fde5380c);
	HH(A, B, C, D, W[ 1], S31, $a4beea44);
	HH(D, A, B, C, W[ 4], S32, $4bdecfa9);
	HH(C, D, A, B, W[ 7], S33, $f6bb4b60);
	HH(B, C, D, A, W[10], S34, $bebfbc70);
	HH(A, B, C, D, W[13], S31, $289b7ec6);
	HH(D, A, B, C, W[ 0], S32, $eaa127fa);
	HH(C, D, A, B, W[ 3], S33, $d4ef3085);
	HH(B, C, D, A, W[ 6], S34, $04881d05);
	HH(A, B, C, D, W[ 9], S31, $d9d4d039);
	HH(D, A, B, C, W[12], S32, $e6db99e5);
	HH(C, D, A, B, W[15], S33, $1fa27cf8);
	HH(B, C, D, A, W[ 2], S34, $c4ac5665);

	// Round 4
	II(A, B, C, D, W[ 0], S41, $f4292244);
	II(D, A, B, C, W[ 7], S42, $432aff97);
	II(C, D, A, B, W[14], S43, $ab9423a7);
	II(B, C, D, A, W[ 5], S44, $fc93a039);
	II(A, B, C, D, W[12], S41, $655b59c3);
	II(D, A, B, C, W[ 3], S42, $8f0ccc92);
	II(C, D, A, B, W[10], S43, $ffeff47d);
	II(B, C, D, A, W[ 1], S44, $85845dd1);
	II(A, B, C, D, W[ 8], S41, $6fa87e4f);
	II(D, A, B, C, W[15], S42, $fe2ce6e0);
	II(C, D, A, B, W[ 6], S43, $a3014314);
	II(B, C, D, A, W[13], S44, $4e0811a1);
	II(A, B, C, D, W[ 4], S41, $f7537e82);
	II(D, A, B, C, W[11], S42, $bd3af235);
	II(C, D, A, B, W[ 2], S43, $2ad7d2bb);
	II(B, C, D, A, W[ 9], S44, $eb86d391);

	// Now add it to the signature
	Inc(FSignature[0], A);
	Inc(FSignature[1], B);
	Inc(FSignature[2], C);
	Inc(FSignature[3], D);

	// Reset data
	FillChar(FBlock, SizeOf(FBlock), 0);
	FInBlock := 0;
end;
{$R+,Q+}

procedure TMD5ChecksumFilter.Process(const Data; const DataSize: Integer);
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

procedure TMD5ChecksumFilter.ResetChecksum;
const
	InitSignature	: TSignature = ( 
		$67452301, $efcdab89, $98badcfe, $10325476
	 );
begin
  inherited;

	FillChar(FBlock, SizeOf(FBlock), 0);
	FInBlock := 0;
	FLength := 0;

	FSignature := InitSignature;
end;

function TMD5ChecksumFilter.SignatureToChecksumBytes(
  const Signature: TSignature): TChecksumBytes;
begin
  SetLength(Result, SizeOf(TSignature));
  Move(Signature, Result[0], SizeOf(TSignature));
end;

end.
 