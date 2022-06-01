{ TODO 2 -oLVK -cSource : Update documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the IDEA encryption
    algorithm.
}
unit lvkIDEAEncryptionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkIDEAEncryptionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicEncryptionFilters;

const
	Rounds		= 8;
	KeyLength	= Rounds*6 + 4;

type
  { Description:
      This type is used as the basis for keys passed to the IDEA
      encryption algorithm.
  }
	TIDEAKey = array[0..KeyLength*2-1] of Byte;

{ Description:
    This function creates a new stream filter that implements the
    IDEA encryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewWriteableIDEAEncryptionFilter@TIDEAKey
}
function NewWriteableIDEAEncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA encryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewWriteableIDEAEncryptionFilter@string
}
function NewWriteableIDEAEncryptionFilter(
  const Key: TIDEAKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA decryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewWriteableIDEADecryptionFilter@TIDEAKey
}
function NewWriteableIDEADecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA decryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewWriteableIDEADecryptionFilter@string
}
function NewWriteableIDEADecryptionFilter(
  const Key: TIDEAKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA encryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewReadableIDEAEncryptionFilter@TIDEAKey
}
function NewReadableIDEAEncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA encryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewReadableIDEAEncryptionFilter@string
}
function NewReadableIDEAEncryptionFilter(
  const Key: TIDEAKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA decryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewReadableIDEADecryptionFilter@TIDEAKey
}
function NewReadableIDEADecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    IDEA decryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewReadableIDEADecryptionFilter@string
}
function NewReadableIDEADecryptionFilter(
  const Key: TIDEAKey): IStreamFilter; overload;

implementation

uses
  lvkBitMath;

const
	Maxim	= 65537;

type
  TIDEABlock = array[1..4] of Word;
  TIDEAInternalKey = array[0..KeyLength-1] of Word;

  TIDEA = class
  private
    Z, K  : TIDEAInternalKey;

    procedure ExpandSubKeys(const Key: TIDEAInternalKey);
    procedure Cipher(var Block: TIDEABlock; const Key: TIDEAInternalKey);

  public
    constructor Create(const Key: TIDEAKey);
    procedure EncryptBlock(var Block);
    procedure DecryptBlock(var Block);
  end;

  TWriteableIDEAFilter = class(TlvkBaseWriteableBlockFilter)
  private
    FIDEA     : TIDEA;
    FEncrypt  : Boolean;

  protected
    procedure ProcessBlock(var Block); override;

  public
    constructor Create(const Key: TIDEAKey; const Encrypt: Boolean);
    destructor Destroy; override;
  end;

  TReadableIDEAFilter = class(TlvkBaseReadableBlockFilter)
  private
    FIDEA     : TIDEA;
    FEncrypt  : Boolean;

  protected
    procedure ProcessBlock(var Block); override;

  public
    constructor Create(const Key: TIDEAKey; const Encrypt: Boolean);
    destructor Destroy; override;
  end;

function NewWriteableIDEAEncryptionFilter(const Passphrase: string): IStreamFilter;
var
  Key : TIDEAKey;
begin
  HashPassphrase(Passphrase, Key);
  Result := NewWriteableIDEAEncryptionFilter(Key);
end;

function NewWriteableIDEAEncryptionFilter(const Key: TIDEAKey): IStreamFilter;
begin
  Result := TWriteableIDEAFilter.Create(Key, True);
end;

function NewWriteableIDEADecryptionFilter(const Passphrase: string): IStreamFilter;
var
  Key : TIDEAKey;
begin
  HashPassphrase(Passphrase, Key);
  Result := NewWriteableIDEADecryptionFilter(Key);
end;

function NewWriteableIDEADecryptionFilter(const Key: TIDEAKey): IStreamFilter;
begin
  Result := TWriteableIDEAFilter.Create(Key, False);
end;

function NewReadableIDEAEncryptionFilter(const Passphrase: string): IStreamFilter;
var
  Key : TIDEAKey;
begin
  HashPassphrase(Passphrase, Key);
  Result := NewReadableIDEAEncryptionFilter(Key);
end;

function NewReadableIDEAEncryptionFilter(const Key: TIDEAKey): IStreamFilter;
begin
  Result := TReadableIDEAFilter.Create(Key, True) as IStreamFilter;
end;

function NewReadableIDEADecryptionFilter(const Passphrase: string): IStreamFilter;
var
  Key : TIDEAKey;
begin
  HashPassphrase(Passphrase, Key);
  Result := NewReadableIDEADecryptionFilter(Key);
end;

function NewReadableIDEADecryptionFilter(const Key: TIDEAKey): IStreamFilter;
begin
  Result := TReadableIDEAFilter.Create(Key, False) as IStreamFilter;
end;

{ TWriteableIDEAFilter }

constructor TWriteableIDEAFilter.Create(const Key: TIDEAKey; const Encrypt: Boolean);
begin
  inherited Create(SizeOf(TIDEABlock));

  FIDEA := TIDEA.Create(Key);
  FEncrypt := Encrypt;
end;

destructor TWriteableIDEAFilter.Destroy;
begin
  FIDEA.Free;

  inherited;
end;

procedure TWriteableIDEAFilter.ProcessBlock(var Block);
begin
  if FEncrypt then
    FIDEA.EncryptBlock(Block)
  else
    FIDEA.DecryptBlock(Block)
end;

{ TIDEA }

{$R-,Q-}
procedure TIDEA.Cipher(var Block: TIDEABlock; const Key: TIDEAInternalKey);
var
	x1,x2,x3,x4	: Word;
	t1,t2				: Word;
	pz					: ^Word;
	r						: Integer;

	function Mul( a,b: Word ): Word;
	var
		p	: LongWord;
	begin
		if ( a > 0 ) then
		begin
			if ( b > 0 ) then
			begin
				p := LongWord( a )*b;
				b := p and $ffff;
				a := p shr 16;
				Result := ( ( b - a ) + Ord( b < a ) );
			end // if ( b > 0 )
			else
				Result := 1 - a;
		end // if ( a > 0 )
		else
			Result := 1 - b;
	end; // function TIDEA.Cipher.Mul

begin
//       x1 = *in++;  x2 = *in++;
	x1 := Block[ 1 ];
	x2 := Block[ 2 ];
//       x3 = *in++;  x4 = *in;
	x3 := Block[ 3 ];
	x4 := Block[ 4 ];

	pz := @Key;
	for r := 1 to Rounds do
	begin
//             MUL( x1,*Z++ );
		x1 := Mul( x1, pz^ );
		Inc( pz );

//             x2 += *Z++;
		x2 := x2 + pz^;
		Inc( pz );

//             x3 += *Z++;
		x3 := x3 + pz^;
		Inc( pz );

//             MUL( x4, *Z++ );
		x4 := Mul( x4, pz^ );
		Inc( pz );

//             t2 = x1^x3;
		t2 := x1 xor x3;

//             MUL( t2, *Z++ );
		t2 := Mul( t2, pz^ );
		Inc( pz );

//             t1 = t2 + ( x2^x4 );
		t1 := t2 + ( x2 xor x4 );

//             MUL( t1, *Z++ );
		t1 := Mul( t1, pz^ );
		Inc( pz );

//             t2 = t1+t2;
		t2 := ( t1 + t2 );

//             x1 ^= t1;
		x1 := x1 xor t1;

//             x4 ^= t2;
		x4 := x4 xor t2;

//             t2 ^= x2;
		t2 := t2 xor x2;

//             x2 = x3^t1;
		x2 := x3 xor t1;

//             x3 = t2;
		x3 := t2;
	end; // for r := 1 to Rounds

//       MUL( x1, *Z++ );
	x1 := Mul( x1, pz^ );
	Inc( pz );

//       *out++ = x1;
	Block[ 1 ] := x1;

//       *out++ = x3 + *Z++;
	Block[ 2 ] := x3 + pz^;
	Inc( pz );

//       *out++ = x2 + *Z++;
	Block[ 3 ] := x2 + pz^;
	Inc( pz );

//       MUL( x4, *Z );
	x4 := Mul( x4, pz^ );

//       *out = x4;
	Block[ 4 ] := x4;
end;
{$R+,Q+}

constructor TIDEA.Create(const Key: TIDEAKey);
var
  InternalKey : TIDEAInternalKey;
begin
  inherited Create;

  Move(Key, InternalKey, SizeOf(Key));
  ExpandSubKeys(InternalKey);
end;

procedure TIDEA.DecryptBlock(var Block);
begin
  Cipher(TIDEABlock(Block), K);
end;

procedure TIDEA.EncryptBlock(var Block);
begin
  Cipher(TIDEABlock(Block), Z);
end;

procedure TIDEA.ExpandSubKeys(const Key: TIDEAInternalKey);

  {$R-,Q-}
  procedure ExpandKeys;
  var
    i			: Integer;
  begin
    Move(Key, Z, SizeOf(Key));

    for i := 8 to KeyLength-1 do
    begin
      if (i+2) mod 8 = 0 then
        Z[i] := (Z[i-7] shl 9) xor (Z[i-14] shr 7)
      else if (i+1) mod 8 = 0 then
        Z[i] := (Z[i-15] shl 9) xor (Z[i-14] shr 7)
      else
        Z[i] := (Z[i-7] shl 9) xor (Z[i-6] shr 7);
    end;
  end;

  procedure InvertKeys;
  type
    PWord		= ^Word;
  var
    j					: Integer;
    pz, pp		: PWord;
    t1,t2,t3	: Word;

    function Inv( I: Integer ): Integer;
    var
      n1,n2,q,r,b1,b2,t	: Integer;
    begin
      if ( I = 0 ) then
        Result := 0
      else begin
        n1 := Maxim;
        n2 := I;
        b2 := 1;
        b1 := 0;
        repeat
          r := ( n1 mod n2 );
          q := ( n1-r ) div n2;
          if ( r = 0 ) then
          begin
            if ( b2 < 0 ) then
              b2 := Maxim+ b2;
          end // if ( r = 0 )
          else begin
            n1 := n2;
            n2 := r;
            t := b2;
            b2 := b1 - q * b2;
            b1 := t;
          end; // if ( r = 0 ) ... else
        until ( r = 0 ); // repeat
        Result := b2;
      end; // if ( I = 0 ) ... else
      Result := ( Result and $ffff );
    end; // function TIDEA.InvertKeys.Inv

  begin
    pz := @Z;
    pp := @K;	Inc( pp, KeyLength );

  //       t1 = inv( *Z++ );
    t1 := Inv( pz^ );
    Inc( pz );

  //       t2 = -*Z++;
    t2 := -pz^;
    Inc( pz );

  //       t3 = -*Z++;
    t3 := -pz^;
    Inc( pz );

  //       *--p = inv( *Z++ );
    Dec( pp );
    pp^ := Inv( pz^ );
    Inc( pz );

  //       *--p = t3;
    Dec( pp );
    pp^ := t3;

  //       *--p = t2;
    Dec( pp );
    pp^ := t2;

  //       *--p = t1;
    Dec( pp );
    pp^ := t1;

    for j := 1 to Rounds-1 do
    begin
  //             t1 = *Z++;
      t1 := pz^;
      Inc( pz );

  //             *--p = *Z++;
      Dec( pp );
      pp^ := pz^;
      Inc( pz );

  //             *--p = t1;
      Dec( pp );
      pp^ := t1;

  //             t1 = inv( *Z++ );
      t1 := Inv( pz^ );
      Inc( pz );

  //             t2 = -*Z++;
      t2 := -pz^;
      Inc( pz );

  //             t3 = -*Z++;
      t3 := -pz^;
      Inc( pz );

  //             *--p = inv( *Z++ );
      Dec( pp );
      pp^ := Inv( pz^ );
      Inc( pz );

  //             *--p = t2;
      Dec( pp );
      pp^ := t2;

  //             *--p = t3;
      Dec( pp );
      pp^ := t3;

  //             *--p = t1;
      Dec( pp );
      pp^ := t1;
    end; // for j := 1 to Rounds-1

  //       t1 = *Z++;
    t1 := pz^;
    Inc( pz );

  //       *--p = *Z++;
    Dec( pp );
    pp^ := pz^;
    Inc( pz );

  //       *--p = t1;
    Dec( pp );
    pp^ := t1;

  //       t1 = inv( *Z++ );
    t1 := Inv( pz^ );
    Inc( pz );

  //       t2 = -*Z++;
    t2 := -pz^;
    Inc( pz );

  //       t3 = -*Z++;
    t3 := -pz^;
    Inc( pz );

  //       *--p = inv( *Z++ );
    Dec( pp );
    pp^ := Inv( pz^ );

  //       *--p = t3;
    Dec( pp );
    pp^ := t3;

  //       *--p = t2;
    Dec( pp );
    pp^ := t2;

  //       *--p = t1;
    Dec( pp );
    pp^ := t1;
  end;
  {$R+,Q+}

begin
	ExpandKeys;
	InvertKeys;
end;

{ TReadableIDEAFilter }

constructor TReadableIDEAFilter.Create(const Key: TIDEAKey; const Encrypt: Boolean);
begin
  inherited Create(SizeOf(TIDEABlock));

  FIDEA := TIDEA.Create(Key);
  FEncrypt := Encrypt;
end;

destructor TReadableIDEAFilter.Destroy;
begin
  FIDEA.Free;

  inherited;
end;

procedure TReadableIDEAFilter.ProcessBlock(var Block);
begin
  if FEncrypt then
    FIDEA.EncryptBlock(Block)
  else
    FIDEA.DecryptBlock(Block);
end;

end.
