{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the RC5 encryption
    algorithm.
}
unit lvkRC5EncryptionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkRC5EncryptionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicEncryptionFilters;

const
	Rounds		= 12;
	KeyLength	= 2*(Rounds+1);

type
  { Description:
      This type is used as the basis for keys passed to the RC5
      encryption algorithm.
  }
  TRC5Key = array[0..63] of Byte;

{ Description:
    This function creates a new stream filter that implements the
    RC5 encryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewWriteableRC5EncryptionFilter@TRC5Key
}
function NewWriteableRC5EncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 encryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewWriteableRC5EncryptionFilter@string
}
function NewWriteableRC5EncryptionFilter(
  const Key: TRC5Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 decryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewWriteableRC5DecryptionFilter@TRC5Key
}
function NewWriteableRC5DecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 decryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewWriteableRC5DecryptionFilter@string
}
function NewWriteableRC5DecryptionFilter(
  const Key: TRC5Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 encryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewReadableRC5EncryptionFilter@TRC5Key
}
function NewReadableRC5EncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 encryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewReadableRC5EncryptionFilter@string
}
function NewReadableRC5EncryptionFilter(
  const Key: TRC5Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 decryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewReadableRC5DecryptionFilter@TRC5Key
}
function NewReadableRC5DecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC5 decryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewReadableRC5DecryptionFilter@string
}
function NewReadableRC5DecryptionFilter(
  const Key: TRC5Key): IStreamFilter; overload;

implementation

uses
  lvkBitMath;
  
const
	SRC5	= 'RC5';
	P32		= $b7e15163;
	Q32		= $9e3779b9;

type
	TRC5Block	= array[0..1] of LongWord;

  TRC5EncryptionAlgorithm = class(TInterfacedObject, IEncryptionAlgorithm)
  private
		S	: array[0..KeyLength-1] of LongWord;
    
		procedure CalculateSubKeys(const Key: TRC5Key);
    
  protected
    // IEncryptionAlgorithm interface
    function GetBlockSize: Integer;
    procedure EncryptBlock(var Block);
    procedure DecryptBlock(var Block);

  public
    constructor Create(const Passphrase: string); overload;
    constructor Create(const Key: TRC5Key); overload;
  end;

function NewWriteableRC5EncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewWriteableRC5EncryptionFilter(const Key: TRC5Key): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewWriteableRC5DecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewWriteableRC5DecryptionFilter(const Key: TRC5Key): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

function NewReadableRC5EncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewReadableRC5EncryptionFilter(const Key: TRC5Key): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewReadableRC5DecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewReadableRC5DecryptionFilter(const Key: TRC5Key): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC5EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

{ TRC5EncryptionAlgorithm }

constructor TRC5EncryptionAlgorithm.Create(const Passphrase: string);
var
  Key : TRC5Key;
begin
  HashPassphrase(Passphrase, Key);
  Create(Key);
end;

{$R-,Q-}
procedure TRC5EncryptionAlgorithm.CalculateSubKeys(const Key: TRC5Key);
var
	i,j,k	: Integer;
	L			: array[0..15] of LongWord;
	A,B		: LongWord;
begin
  Move(Key, L, SizeOf(Key));

	S[0] := P32;
	for i := 1 to KeyLength-1 do
		S[i] := S[i-1] + Q32;

	i := 0; j := 0; A := 0; B := 0;
	for k := 1 to 3*KeyLength do
	begin
		A := ROL((S[i] + A + B), 3);
		S[i] := A;
		B := ROL((L[j] + A + B), (A + B));
		L[j] := B;
		i := (i + 1) mod KeyLength;
		j := (j + 1) mod 16;
	end;
end;
{$R+,Q+}

constructor TRC5EncryptionAlgorithm.Create(const Key: TRC5Key);
begin
  inherited Create;

  CalculateSubKeys(Key);
end;

{$R-,Q-}
procedure TRC5EncryptionAlgorithm.DecryptBlock(var Block);
var
	RC5Block	: TRC5Block absolute Block;
	i					: Integer;
begin
	for i := Rounds downto 1 do
	begin
		RC5Block[1] := ROR((RC5Block[1]-S[2*i+1]), RC5Block[0]) xor RC5Block[0];
		RC5Block[0] := ROR((RC5Block[0]-S[2*i] ), RC5Block[1] ) xor RC5Block[1];
	end; // for i := 1 to Rounds

	Dec(RC5Block[1], S[1]);
	Dec(RC5Block[0], S[0]);
end;

procedure TRC5EncryptionAlgorithm.EncryptBlock(var Block);
var
	RC5Block	: TRC5Block absolute Block;
	i					: Integer;
begin
	Inc(RC5Block[0], S[0] );
	Inc(RC5Block[1], S[1] );

	for i := 1 to Rounds do
	begin
		RC5Block[0] := ROL((RC5Block[0] xor RC5Block[1] ),RC5Block[1] ) + S[2*i];
		RC5Block[1] := ROL((RC5Block[1] xor RC5Block[0] ),RC5Block[0] ) + S[2*i+1];
	end;
end;
{$R+,Q+}

function TRC5EncryptionAlgorithm.GetBlockSize: Integer;
begin
  Result := SizeOf(TRC5Block);
end;

end.
