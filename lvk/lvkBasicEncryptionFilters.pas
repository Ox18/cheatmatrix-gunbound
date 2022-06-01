{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the base code related to encryption/decryption
    filters. New encryption/decryption filters can inherit from the classes
    defined in this unit to get some of the necessary functionality for
    free.
}
unit lvkBasicEncryptionFilters;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBasicEncryptionFilters.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFNDEF DELPHI6UP}
  Windows,
  {$ENDIF}
  Classes, SysUtils, lvkStreamFilters, lvkBasicStreamFilters;

type
{ Description:
    This class is the base class for all writeable block encryption/decryption
    filters. It deals with the blocking of data and will call ProcessBlock
    before data is written to the next filter.

    Descendant classes will need to override ProcessBlock in order to
    support the encryption/decryption algorithm. Note that this class is not
    specifically related to encryption and can thus be used as a base class
    for other classes that writes in blocks as well.
  See also:
    TlvkBaseReadableBlockFilter
}
  TlvkBaseWriteableBlockFilter = class(TlvkBaseStreamFilter, IWriteableStreamFilter,
    ISeekableStreamFilter)
  private
    FBlock      : PChar;
    FInBlock    : Integer;
    FBlockSize  : Integer;

  protected
    // <ALIAS IWriteableStreamFilter.Write@@Longint>
    function Write(const Buffer; const Count: Longint): Longint;

    // <ALIAS IWriteableStreamFilter.Flush>
    procedure Flush;

    // <ALIAS ISeekableStreamFilter.Seek@Int64@TSeekOrigin>
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

    { Description:
        This method is called just before the block is written to the next
        filter. Descendant classes will need to override this method to
        do whatever processing that needs to be done on the block,
        like encrypting or decrypting it.
      Parameters:
        Block - The block to process.
      See also:
        WriteBlock
    }
    procedure ProcessBlock(var Block); virtual;

    { Description:
        This method will first call ProcessBlock and then the block will
        be written to the next filter in the sequence.
      See also:
        ProcessBlock
    }
    procedure WriteBlock; virtual;

  public
    { Description:
        This constructor creates the class and initializes the blocking
        mechanism with the given block size.
      Parameters:
        BlockSize - The size of each block, must be positive and non-zero.
    }
    constructor Create(const BlockSize: Integer);

    { Description:
        This destructor cleans up and destroys the internal block
        buffer.
    }
    destructor Destroy; override;
  end;

{ Description:
    This class is the base class for all readable block encryption/decryption
    filters. It deals with the blocking of data and will call ProcessBlock
    after data has been read from the next filter.

    Descendant classes will need to override ProcessBlock in order to
    support the encryption/decryption algorithm. Note that this class is not
    specifically related to encryption and can thus be used as a base class
    for other classes that reads in blocks as well.
  See also:
    TlvkBaseWriteableBlockFilter
}
  TlvkBaseReadableBlockFilter = class(TlvkBaseStreamFilter, IReadableStreamFilter,
    ISeekableStreamFilter)
  private
    FBlock      : PChar;
    FInBlock    : Integer;
    FBlockSize  : Integer;

  protected
    // <ALIAS IReadableStreamFilter.Read@@Longint>
    function Read(var Buffer; const Count: Longint): Longint;

    // <ALIAS ISeekableStreamFilter.Seek@Int64@TSeekOrigin>
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

    { Description:
        This method is called just after the block has been read from the next
        filter. Descendant classes will need to override this method to
        do whatever processing that needs to be done on the block,
        like encrypting or decrypting it.
      Parameters:
        Block - The block to process.
      See also:
        ReadBlock
    }
    procedure ProcessBlock(var Block); virtual;

    { Description:
        This method will read a new block from the next filter in the
        sequence and then call ProcessBlock on the new block.
      See also:
        ProcessBlock
    }
    procedure ReadBlock; virtual;

  public
    { Description:
        This constructor creates the class and initializes the blocking
        mechanism with the given block size.
      Parameters:
        BlockSize - The size of each block, must be positive and non-zero.
    }
    constructor Create(const BlockSize: Integer);

    { Description:
        This destructor cleans up and destroys the internal block
        buffer.
    }
    destructor Destroy; override;
  end;

{ Description:
    This interface should be supported by all encryption/decryption filter
    classes that will be used with the TlvkWriteableEncryptionFilter and
    TlvkReadableEncryptionFilter classes.

  See also:
    TlvkWriteableEncryptionFilter,
    TlvkReadableEncryptionFilter
}
  IEncryptionAlgorithm = interface
    ['{C6DFEAE2-92E3-11D5-B272-0004761A6377}']

    { Description:
        This method must return the size of the block that the encryption
        algorithm handles, as a number of bytes. For instance, if the
        encryption algorithm deals with 64-bit blocks, this method should
        return the value 8 (8 bytes = 64 bits).
    }
    function GetBlockSize: Integer;

    { Description:
        This method will be called to encrypt a single block of data. The
        block is always full.
      Parameters:
        Block - The block to encrypt.
      See also:
        DecryptBlock
    }
    procedure EncryptBlock(var Block);

    { Description:
        This method will be called to decrypt a single block of data. The
        block is always full.
      Parameters:
        Block - The block to decrypt.
      See also:
        EncryptBlock
    }
    procedure DecryptBlock(var Block);
  end;

{ Description:
    This class is a simple class that can be used to encrypt data. In order to
    add support for new encryption algorithms, the programmer would have to
    create a new class that supports the IEncryptionAlgorithm interface and use
    it in conjunction with TlvkWriteableEncryptionFilter and
    TlvkReadableEncryptionFilter to allow for reading and writing of
    encrypted/decrypted data.

    This is a write-only filter.
  See also:
    TlvkReadableEncryptionFilter
}
  TlvkWriteableEncryptionFilter = class(TlvkBaseWriteableBlockFilter)
  private
    FAlgorithm  : IEncryptionAlgorithm;
    FEncrypt    : Boolean;

  protected
    // <ALIAS TlvkBaseWriteableBlockFilter.ProcessBlock@>
    procedure ProcessBlock(var Block); override;

  public
    { Description:
        This constructor initializes the class and stores the encryption
        algorithm interface for future reference.
      Parameters:
        Algorithm - Interface that will be responsible for encrypting and
          decrypting data that passes through this filter.
        Encrypt - Set this parameter to True if the filter should encrypt data,
          set it to False if it should decrypt data.
    }
    constructor Create(const Algorithm: IEncryptionAlgorithm; const Encrypt: Boolean);

    { Description:
        This destructor shuts down and deallocates the encryption algorithm
        interface.
    }
    destructor Destroy; override;
  end;

{ Description:
    This class is a simple class that can be used to encrypt data. In order to
    add support for new encryption algorithms, the programmer would have to
    create a new class that supports the IEncryptionAlgorithm interface and use
    it in conjunction with TlvkWriteableEncryptionFilter and
    TlvkReadableEncryptionFilter to allow for reading and writing of
    encrypted/decrypted data.

    This is a read-only filter.
  See also:
    TlvkReadableEncryptionFilter
}
  TlvkReadableEncryptionFilter = class(TlvkBaseReadableBlockFilter)
  private
    FAlgorithm  : IEncryptionAlgorithm;
    FEncrypt    : Boolean;

  protected
    // <ALIAS TlvkBaseWriteableBlockFilter.ProcessBlock@>
    procedure ProcessBlock(var Block); override;

  public
    { Description:
        This constructor initializes the class and stores the encryption
        algorithm interface for future reference.
      Parameters:
        Algorithm - Interface that will be responsible for encrypting and
          decrypting data that passes through this filter.
        Encrypt - Set this parameter to True if the filter should encrypt data,
          set it to False if it should decrypt data.
    }
    constructor Create(const Algorithm: IEncryptionAlgorithm; const Encrypt: Boolean);

    { Description:
        This destructor shuts down and deallocates the encryption algorithm
        interface.
    }
    destructor Destroy; override;
  end;

{ Description:
    This procedure takes a given passphrase string and generates a binary
    key of it. This is used throughout the existing encryption algorithms
    to convert a passphrase to the internal byte format used for keys.
  Parameters:
    Passphrase - The passphrase to convert to a byte array.
    Key - The byte array to fill with key byte data. This array must already
      have the correct length as specified by the internal data structures
      of the encryption algorithm it's going to be used with.
}
procedure HashPassphrase(const Passphrase: string; var Key: array of Byte);

implementation

uses
  lvkBitMath, Math;

procedure HashPassphrase(const Passphrase: string; var Key: array of Byte);
var
	TempKey	: string;
	i,j			: Integer;
	K1,K2		: LongWord;
begin
  FillChar(Key[0], Length(Key), #0);
  
	TempKey := Passphrase;
	i := 1;
	while Length(TempKey) mod Length(Key) > 0 do
	begin
		TempKey := TempKey + TempKey[i];
		Inc(i);
	end;

	i := 1;
	j := 0;
	while i < Length(TempKey) do
	begin
		Move(Key[j], K1, 4);
		Move(TempKey[i], K2, 4);
		K1 := ROL(K1, K2) xor K2;
		Move(K1, Key[j], 4);
		j := (j + 4) mod Length(Key);
		Inc(i, 4);
	end; 
end;

{ TlvkBaseWriteableBlockFilter }

constructor TlvkBaseWriteableBlockFilter.Create(const BlockSize: Integer);
begin
  Assert(BlockSize > 0);
  
  inherited Create;

  GetMem(FBlock, BlockSize);
  FBlockSize := BlockSize;
  FInBlock := 0;
end;

destructor TlvkBaseWriteableBlockFilter.Destroy;
begin
  FreeMem(FBlock);

  inherited;
end;

procedure TlvkBaseWriteableBlockFilter.Flush;
var
  b   : Byte;
  rc  : Integer;
begin
  while FInBlock > 0 do
  begin
    b := 0;
    rc := Write(b, 1);
    Assert(rc=1);
  end;
end;

procedure TlvkBaseWriteableBlockFilter.ProcessBlock(var Block);
begin
  // Do nothing
end;

function TlvkBaseWriteableBlockFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to beginning or current');
    ssf.Seek(0, soBeginning);
    FInBlock := 0;
    Result := 0;
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  begin
    if Offset = 0 then
      Result := ssf.Seek(0, soCurrent)
    else
      Result := SeekFromBeginning(ssf.Seek(0, soCurrent) + Offset);
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  begin
    Result := ssf.Seek(0, soCurrent);
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := ssf.Seek(0, soCurrent);
  end;

  Result := Result + FInBlock;
end;

function TlvkBaseWriteableBlockFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  ToCopy    : Integer;
begin
  Result := 0;
  BufferPtr := @Buffer;

  while Result < Count do
  begin
    ToCopy := Min(FBlockSize-FInBlock, Count-Result);
    Move(BufferPtr^, (FBlock+FInBlock)^, ToCopy);
    Inc(FInBlock, ToCopy);
    Inc(Result, ToCopy);
    Inc(BufferPtr, ToCopy);

    if FInBlock = FBlockSize then
      WriteBlock;
  end;
end;

procedure TlvkBaseWriteableBlockFilter.WriteBlock;
var
  wsf : IWriteableStreamFilter;
  rc  : Integer;
begin
  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) <> S_OK then
    raise EStreamWrite.Create('Next filter not writeable in call to WriteBlock');

  ProcessBlock(FBlock^);
  rc := wsf.Write(FBlock^, FBlockSize);
  Assert(rc = FBlockSize);

  FInBlock := 0;
end;

{ TlvkBaseReadableBlockFilter }

constructor TlvkBaseReadableBlockFilter.Create(const BlockSize: Integer);
begin
  inherited Create;

  GetMem(FBlock, BlockSize);
  FBlockSize := BlockSize;
  FInBlock := 0;
end;

destructor TlvkBaseReadableBlockFilter.Destroy;
begin
  FreeMem(FBlock);

  inherited;
end;

procedure TlvkBaseReadableBlockFilter.ProcessBlock(var Block);
begin
  // Do nothing
end;

function TlvkBaseReadableBlockFilter.Read(var Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  ToCopy    : Integer;
begin
  Result := 0;
  BufferPtr := @Buffer;

  while Result < Count do
  begin
    ToCopy := Min(FInBlock, Count-Result);
    Move((FBlock+FBlockSize-FInBlock)^, BufferPtr^, ToCopy);
    Inc(BufferPtr, ToCopy);
    Inc(Result, ToCopy);
    Dec(FInBlock, ToCopy);

    If FInBlock = 0 then
    begin
      ReadBlock;
      if FInBlock = 0 then
        Break;
    end;
  end;
end;

procedure TlvkBaseReadableBlockFilter.ReadBlock;
var
  rsf : IReadableStreamFilter;
  rc  : Integer;
begin
  if NextFilter.QueryInterface(IReadableStreamFilter, rsf) <> S_OK then
    raise EStreamWrite.Create('Next filter not readable in call to ReadBlock');

  rc := rsf.Read(FBlock^, FBlockSize);
  Assert((rc = FBlockSize) or (rc = 0));
  if rc > 0 then
    ProcessBlock(FBlock^);

  FInBlock := rc;
end;

function TlvkBaseReadableBlockFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromBeginning(const Offset: Int64): Int64;
  var
    Position    : Int64;
    BlockOffset : Int64;
  begin
    BlockOffset := (Offset div FBlockSize)*FBlockSize;
    Position := ssf.Seek(BlockOffset, soBeginning);
    Assert(Position = BlockOffset);

    FInBlock := 0;
    ReadBlock;

    Dec(FInBlock, Offset mod FBlockSize);
    Result := ssf.Seek(0, soCurrent);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  begin
    if Offset = 0 then
      Result := ssf.Seek(0, soCurrent)
    else
      Result := SeekFromBeginning(ssf.Seek(0, soCurrent) + Offset);
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  var
    Size  : Integer;
  begin
    Size := (ssf.Seek(0, soEnd) div FBlockSize)*FBlockSize;
    Result := SeekFromBeginning(Size + Offset);
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := ssf.Seek(0, soCurrent);
  end;

  Result := Result - FInBlock;
end;

{ TWriteableEncryptionFilter }

constructor TlvkWriteableEncryptionFilter.Create(
  const Algorithm: IEncryptionAlgorithm; const Encrypt: Boolean);
begin
  Assert(Assigned(Algorithm));
  inherited Create(Algorithm.GetBlockSize);

  FAlgorithm := Algorithm;
  FEncrypt := Encrypt;
end;

destructor TlvkWriteableEncryptionFilter.Destroy;
begin
  FAlgorithm := nil;

  inherited;
end;

procedure TlvkWriteableEncryptionFilter.ProcessBlock(var Block);
begin
  if FEncrypt then
    FAlgorithm.EncryptBlock(Block)
  else
    FAlgorithm.DecryptBlock(Block);
end;

{ TReadableEncryptionFilter }

constructor TlvkReadableEncryptionFilter.Create(
  const Algorithm: IEncryptionAlgorithm; const Encrypt: Boolean);
begin
  Assert(Assigned(Algorithm));

  inherited Create(Algorithm.GetBlockSize);

  FAlgorithm := Algorithm;
  FEncrypt := Encrypt;
end;

destructor TlvkReadableEncryptionFilter.Destroy;
begin
  FAlgorithm := nil;

  inherited;
end;

procedure TlvkReadableEncryptionFilter.ProcessBlock(var Block);
begin
  if FEncrypt then
    FAlgorithm.EncryptBlock(Block)
  else
    FAlgorithm.DecryptBlock(Block);
end;

end.
