{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the RC6 encryption
    algorithm.
}
unit lvkRC6EncryptionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkRC6EncryptionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicEncryptionFilters;

const
	Rounds		= 20;
	KeyLength	= 2*(Rounds+2);

type
  { Description:
      This type is used as the basis for keys passed to the RC6
      encryption algorithm.
  }
  TRC6Key = array[0..63] of Byte;

{ Description:
    This function creates a new stream filter that implements the
    RC6 encryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewWriteableRC6EncryptionFilter@TRC6Key
}
function NewWriteableRC6EncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 encryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewWriteableRC6EncryptionFilter@string
}
function NewWriteableRC6EncryptionFilter(
  const Key: TRC6Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 decryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewWriteableRC6DecryptionFilter@TRC6Key
}
function NewWriteableRC6DecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 decryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewWriteableRC6DecryptionFilter@string
}
function NewWriteableRC6DecryptionFilter(
  const Key: TRC6Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 encryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewReadableRC6EncryptionFilter@TRC6Key
}
function NewReadableRC6EncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 encryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewReadableRC6EncryptionFilter@string
}
function NewReadableRC6EncryptionFilter(
  const Key: TRC6Key): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 decryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewReadableRC6DecryptionFilter@TRC6Key
}
function NewReadableRC6DecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    RC6 decryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewReadableRC6DecryptionFilter@string
}
function NewReadableRC6DecryptionFilter(
  const Key: TRC6Key): IStreamFilter; overload;

implementation

uses
  lvkBitMath;

const
	SRC6	= 'RC6';
	P32		= $b7e15163;
	Q32		= $9e3779b9;
	lgw		= 5;					// log( 32,2 )

type
	TRC6Block	= array[0..3] of LongWord;

  TRC6EncryptionAlgorithm = class(TInterfacedObject, IEncryptionAlgorithm)
  private
		S	: array[0..KeyLength-1] of LongWord;
    
		procedure CalculateSubKeys(const Key: TRC6Key);
    
  protected
    // IEncryptionAlgorithm interface
    function GetBlockSize: Integer;
    procedure EncryptBlock(var Block);
    procedure DecryptBlock(var Block);

  public
    constructor Create(const Passphrase: string); overload;
    constructor Create(const Key: TRC6Key); overload;
  end;

function NewWriteableRC6EncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewWriteableRC6EncryptionFilter(const Key: TRC6Key): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewWriteableRC6DecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewWriteableRC6DecryptionFilter(const Key: TRC6Key): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

function NewReadableRC6EncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewReadableRC6EncryptionFilter(const Key: TRC6Key): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewReadableRC6DecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewReadableRC6DecryptionFilter(const Key: TRC6Key): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TRC6EncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

{ TRC6EncryptionAlgorithm }

constructor TRC6EncryptionAlgorithm.Create(const Passphrase: string);
var
  Key : TRC6Key;
begin
  HashPassphrase(Passphrase, Key);
  Create(Key);
end;

{$R-,Q-}
procedure TRC6EncryptionAlgorithm.CalculateSubKeys(const Key: TRC6Key);
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

constructor TRC6EncryptionAlgorithm.Create(const Key: TRC6Key);
begin
  inherited Create;

  CalculateSubKeys(Key);
end;

{$R-,Q-}
procedure TRC6EncryptionAlgorithm.DecryptBlock(var Block);
var
	RC6Block	: TRC6Block absolute Block;
	i					: Integer;
	t,u				: LongWord;
	Temp			: LongWord;
begin
	RC6Block[2] := RC6Block[2] - S[2*Rounds+3];
	RC6Block[0] := RC6Block[0] - S[2*Rounds+2];

	for i := Rounds downto 1 do
	begin
		Temp := RC6Block[3];
		RC6Block[3] := RC6Block[2];
		RC6Block[2] := RC6Block[1];
		RC6Block[1] := RC6Block[0];
		RC6Block[0] := Temp;

		u := ROL((RC6Block[3] * (2*RC6Block[3] + 1)), lgw);
		t := ROL((RC6Block[1] * (2*RC6Block[1] + 1)), lgw);
		RC6Block[2] := ROR((RC6Block[2]-S[2*i+1]), t) xor u;
		RC6Block[0] := ROR((RC6Block[0]-S[2*i]), u) xor t;
	end;

	Dec(RC6Block[3], S[1]);
	Dec(RC6Block[1], S[0]);
end;

procedure TRC6EncryptionAlgorithm.EncryptBlock(var Block);
var
	RC6Block	: TRC6Block absolute Block;
	i					: Integer;
	t,u				: LongWord;
	Temp			: LongWord;
begin
	Inc(RC6Block[1], S[0] );
	Inc(RC6Block[3], S[1] );

	for i := 1 to Rounds do
	begin
		t := ROL((RC6Block[1] * (2*RC6Block[1] + 1)),lgw);
		u := ROL((RC6Block[3] * (2*RC6Block[3] + 1)),lgw);
		RC6Block[0] := ROL((RC6Block[0] xor t), u) + S[2*i];
		RC6Block[2] := ROL((RC6Block[2] xor u), t) + S[2*i+1];

		Temp := RC6Block[0];
		RC6Block[0] := RC6Block[1];
		RC6Block[1] := RC6Block[2];
		RC6Block[2] := RC6Block[3];
		RC6Block[3] := Temp;
	end;

	RC6Block[0] := RC6Block[0] + S[2*Rounds+2];
	RC6Block[2] := RC6Block[2] + S[2*Rounds+3];
end;
{$R+,Q+}

function TRC6EncryptionAlgorithm.GetBlockSize: Integer;
begin
  Result := SizeOf(TRC6Block);
end;

end.
