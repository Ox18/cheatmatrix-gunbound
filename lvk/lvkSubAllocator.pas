{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a class specialized for memory allocation that allocates
    lots of small blocks after each other, and then frees them all in one
    big swoop at the end.
}
unit lvkSubAllocator;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSubAllocator.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils,
  lvkTypes;

type
  { Description:
      This interface encapsulates an object that allows you to allocate lots
      of small blocks of memory after each other and then free them all in one
      big swoop at the end.
  }
  ISubAllocator = interface
    ['{5504499F-DB5F-4642-89DD-E6F044D3B19D}']

    { Description:
        Allocates a portion of the big block and returns a pointer to the
        newly allocated portion.
      Parameters:
        Size  - How many bytes to allocate.
    }
    function Grab(const Size: UInt32): Pointer;

    { Description:
        This method will release all allocated portions from this block.
    }
    procedure ReleaseAll;

    { Description:
        This property returns the size allocated to the block.
    }
    function GetSize: UInt32;
    // <COMBINE GetSize>
    property Size: UInt32 read GetSize;
  end;

  ESubAllocator = class(Exception);

  { Description:
      This type is used when creating a new sub-allocator block. It
      describes how the smaller portions within it should be aligned, if
      at all.
    See also:
      NewSubAllocator, ISubAllocator.Grab@UInt32
  }
  TMemoryAlignment = (
    // Don't align
    al1,
    // Align all grabs on a two-byte boundary (16-bit alignment)
    al2,
    // Align all grabs on a four-byte boundary (32-bit alignment)
    al4,
    // Align all grabs on a eight-byte boundary (64-bit alignment)
    al8);

{ Description:
    This function creates a new sub-allocator object instance of the given
    size and returns it.
  Parameters:
    BlockSize - The size of each block. If the program grabs more memory than
      a block has left, another block will be allocated and used.
    Alignment - What kind of alignment to use for each grab.
  Returns:
    The new sub-allocator object instance.
  See also:
    ISubAllocator
}
function NewSubAllocator(const BlockSize: UInt32=32768;
  const Alignment: TMemoryAlignment=al1): ISubAllocator;

implementation

type
  PBlockNode = ^TBlockNode;
  TBlockNode = record
    Block     : PChar;
    NextPtr   : PChar;
    Left      : UInt32;
    NextNode  : PBlockNode;
  end;

  TSubAllocator = class(TInterfacedObject, ISubAllocator)
  private
    FFirst      : PBlockNode;
    FLast       : PBlockNode;

    FSize       : UInt32;
    FBlockSize  : UInt32;
    FAlignment  : TMemoryAlignment;

    function AllocateBlockNode: PBlockNode;

  protected
    // ISubAllocator interface
    function Grab(const Size: UInt32): Pointer;
    procedure ReleaseAll;
    function GetSize: UInt32;

  public
    constructor Create(const BlockSize: UInt32;
      const Alignment: TMemoryAlignment);
    destructor Destroy; override;
  end;

const
  Padding : array[TMemoryAlignment] of UInt32 = (
    0, 1, 3, 7
  );

function NewSubAllocator(const BlockSize: UInt32;
  const Alignment: TMemoryAlignment): ISubAllocator;
begin
  Result := TSubAllocator.Create(BlockSize, Alignment);
end;

{ TSubAllocator }

function TSubAllocator.AllocateBlockNode: PBlockNode;
begin
  New(Result);
  try
    GetMem(Result^.Block, FBlockSize + Padding[FAlignment]);
    try
      Result^.NextPtr := Result^.Block;
      Result^.Left := FBlockSize + Padding[FAlignment];
      Result^.NextNode := nil;
    except
      FreeMem(Result^.Block);
      raise;
    end;
  except
    Dispose(Result);
    raise;
  end;
end;

constructor TSubAllocator.Create(const BlockSize: UInt32;
  const Alignment: TMemoryAlignment);
begin
  inherited Create;

  FBlockSize := BlockSize;
  FAlignment := Alignment;
end;

destructor TSubAllocator.Destroy;
begin
  ReleaseAll;

  inherited;
end;

function TSubAllocator.GetSize: UInt32;
begin
  Result := FSize;
end;

function TSubAllocator.Grab(const Size: UInt32): Pointer;
var
  AdjustedSize  : UInt32;
begin
  if Size > FBlockSize then
    raise ESubAllocator.Create('Cannot grab more than one blocksize of memory');

  AdjustedSize := Size + Padding[FAlignment];

  if not Assigned(FFirst) then
  begin
    FFirst := AllocateBlockNode;
    FLast := FFirst;
  end;

  if FLast^.Left < AdjustedSize then
  begin
    FLast^.NextNode := AllocateBlockNode;
    FLast := FLast^.NextNode;
  end;

  Assert(FLast^.Left >= AdjustedSize);

  Result := FLast^.NextPtr;
  while (UInt32(Result) and Padding[FAlignment]) <> 0 do
    Inc(PChar(Result));

  Inc(FLast^.NextPtr, AdjustedSize);
  Dec(FLast^.Left, AdjustedSize);
  Inc(FSize, AdjustedSize);
end;

procedure TSubAllocator.ReleaseAll;
begin
  while Assigned(FFirst) do
  begin
    FLast := FFirst;
    FFirst := FFirst^.NextNode;

    FreeMem(FLast^.Block);
    Dispose(FLast);
  end;

  FLast := nil;
  FFirst := nil;
  FSize := 0;
end;

end.
