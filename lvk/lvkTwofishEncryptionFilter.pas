{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the Twofish encryption
    algorithm.
}
unit lvkTwofishEncryptionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkTwofishEncryptionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicEncryptionFilters;

type
  { Description:
      This type is used as the basis for keys passed to the Twofish
      encryption algorithm.
  }
  TTwofishKey  = array[0..0] of Byte;

{ Description:
    This function creates a new stream filter that implements the
    Twofish encryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewWriteableTwofishEncryptionFilter@TTwofishKey
}
function NewWriteableTwofishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish encryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewWriteableTwofishEncryptionFilter@string
}
function NewWriteableTwofishEncryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish decryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewWriteableTwofishDecryptionFilter@TTwofishKey
}
function NewWriteableTwofishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish decryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewWriteableTwofishDecryptionFilter@string
}
function NewWriteableTwofishDecryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish encryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewReadableTwofishEncryptionFilter@TTwofishKey
}
function NewReadableTwofishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish encryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewReadableTwofishEncryptionFilter@string
}
function NewReadableTwofishEncryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish decryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewReadableTwofishDecryptionFilter@TTwofishKey
}
function NewReadableTwofishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Twofish decryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewReadableTwofishDecryptionFilter@string
}
function NewReadableTwofishDecryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;

implementation

type
  TTwofishBlock = array[0..0] of Byte;
  TTwofishEncryptionAlgorithm = class(TInterfacedObject, IEncryptionAlgorithm)
  private
    procedure CalculateSubKeys(const Key: TTwofishKey);

  protected
    // IEncryptionAlgorithm interface
    function GetBlockSize: Integer;
    procedure EncryptBlock(var Block);
    procedure DecryptBlock(var Block);

  public
    constructor Create(const Key: TTwofishKey); overload;
    constructor Create(const Passphrase: string); overload;
  end;

function NewWriteableTwofishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewWriteableTwofishEncryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewWriteableTwofishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewWriteableTwofishDecryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

function NewReadableTwofishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewReadableTwofishEncryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewReadableTwofishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewReadableTwofishDecryptionFilter(const Key: TTwofishKey): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TTwofishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

{ TTwofishEncryptionAlgorithm }

constructor TTwofishEncryptionAlgorithm.Create(const Key: TTwofishKey);
begin
  inherited Create;
  
  CalculateSubKeys(Key);
end;

procedure TTwofishEncryptionAlgorithm.CalculateSubKeys(
  const Key: TTwofishKey);
begin
  { TODO 2 -oLVK -cSource : Implement TTwofishEncryptionAlgorithm.CalculateSubKeys }
end;

constructor TTwofishEncryptionAlgorithm.Create(const Passphrase: string);
var
  Key : TTwofishKey;
begin
  HashPassphrase(Passphrase, Key);
  Create(Key);
end;

procedure TTwofishEncryptionAlgorithm.DecryptBlock(var Block);
begin
  { TODO 2 -oLVK -cSource : Implement TTwofishEncryptionAlgorithm.DecryptBlock }
end;

procedure TTwofishEncryptionAlgorithm.EncryptBlock(var Block);
begin
  { TODO 2 -oLVK -cSource : Implement TTwofishEncryptionAlgorithm.EncryptBlock }
end;

function TTwofishEncryptionAlgorithm.GetBlockSize: Integer;
begin
  Result := SizeOf(TTwofishBlock);
end;

end.
 