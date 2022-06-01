{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code that can compare two files (or more correctly,
    two blocks of data) and produce a list of the differences between the
    two. You can then ship those differences to someone who has a copy of one
    of the files, and apply the differences there to produce the other of
    the files, in effect patching or updating the file.
}
unit lvkBinaryCompare {$IFDEF DELPHI6UP} deprecated {$ENDIF};

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBinaryCompare.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}
{$R-,Q-,O-}

uses
  SysUtils, Classes, lvkComponents;

const
  DEFAULT_WINDOWSIZE  = 65536;
  DEFAULT_BLOCKSIZE   = 12;

type
  { Description:
      This event handler type is used for progress events from the patch
      generation and application functions and methods. It will be called at
      various times to provide feedback to the program about progress.

      The program can opt to stop the patch, either by a request from the user
      or other criteria being met (such as patch generation taking too long
      to complete).
    Parameters:
      Sender - The object that generated the event. Might be nil, if not passed
        to the Compare function.
      Current - The current position in the total work.
      Total - The total amount of work to complete. This value will not change
        during the current method call.
      ContinueOperation - The event handler can set this to False to stop the
        patch generation or application from continuing. The value has been
        preset to True from the method that called the event handler so its
        safe to leave it unchanged if the process should continue.
    See also:
      Compare, IPatchOperations.Apply
  }
	TProgressEvent	= procedure(const Sender: TObject; const Current, Total: Integer;
		var ContinueOperation: Boolean) of object;

  { Description:
      There are four types of patch operations available, and these are:

        * ptCopy - Copy a number of bytes from the old file to the new file.
        * ptInsert - Insert a number of bytes into the new file.
        * ptDelete - Skip a number of bytes in the old file, effectively
            deleting them.
        * ptReplace - Skip a number of bytes in the old file, and insert a
            number of bytes in the new file, effectively doing a replace.
    See also:
      TPatchOperation
  }
  TPatchOperationType = (ptCopy, ptInsert, ptDelete, ptReplace);

  { Description:
      This is simply a pointer to a TPatchOperation record. It's mostly used
      internally and no public method will return or work with this type.
    See also:
      TPatchOperation
  }
  PPatchOperation = ^TPatchOperation;

  { Description:
      This record holds information related to a single patch operation. The
      IPatchOperations interface holds a list of such records.

      For information about the Position members of this record, see the
      IPatchOperations.SaveToStream and IPatchOperations.Apply methods for
      more information.
    See also:
      PPatchOperation, TPatchOperationType
  }
  TPatchOperation = record
  case PatchOperationType: TPatchOperationType of
    ptCopy:
      (CopyAmount: Integer);

    ptInsert:
      (InsertAmount: Integer;
       InsertPosition: Integer);

    ptDelete:
      (DeleteAmount: Integer);

    ptReplace:
      (ReplaceAmount: Integer;
       ReplaceWithAmount: Integer;
       ReplacePosition: Integer);
  end;

  { Description:
      This interface is the key to the patching framework. When you do a compare
      of an old and a new file, you'll get an object wrapped in this interface
      in return, giving you access to the patch operations that the compare
      resulted in.
  }
  IPatchOperations = interface
    ['{4B8366AC-8C2A-4362-BF23-E82C6885A9CB}']

    // <COMBINE Count>
    function GetCount: Integer;
    { Description:
        This returns the number of patch operations stored in the list.
      See also:
        Operations
    }
    property Count: Integer read GetCount;

    // <COMBINE Operations>
    function GetOperations(const Index: Integer): TPatchOperation;
    { Description:
        This returns the individual patch operations, indexed through a 0-based
        parameterized property.
      See also:
        Count
    }
    property Operations[const Index: Integer]: TPatchOperation read GetOperations; default;

    { Description:
        This method saves the patch operations to a stream for future use.
        The NewData pointer must point to the same data passed to the
        Compare method.

        The reason for this odd requirement is because of the way the patch
        operations are stored internally. The two patch operation types
        ptInsert and ptReplace must have a copy of the data to insert into the
        new data when the patch is being applied, and this data must be saved
        to the stream along with the patch operations themselves. Instead of
        making a copy of the data and storing it in the list of patch
        operations, the position of where the data originated is stored instead.
        This means that when that data is required, the method needs to find
        it somewhere, and this is why you pass NewData.

        Note: If you load the patch operations from a stream, using
          LoadPatchOperations, and you want to save the patch operations
          again, you must pass the data found in the stream you loaded it
          from instead of the "new" data block, as the positions have been
          adjusted for that stream.

        Note: If all this about positions sound hard to understand, then
          check out the examples, or you can use the two components provided
          for easier use: TlvkPatchBuilder and TlvkPatchApplier.
      Parameters:
        Stream - The stream to save the patch operations to.
        NewData - Pointer to the data that the position members of the
          patch operations refer to.
      See also:
        LoadPatchOperations@TStream
    }
    procedure SaveToStream(const Stream: TStream; const NewData: Pointer);

    { Description:
        This method takes a stream containing the "old" data, and fills the
        NewStream with the "new" data, as dictated by the patch operations.

        The PatchData parameter must point to either the NewData block used
        when comparing, or the PatchData stream (which must be available
        through a pointer) used when loading the patch operations. For more
        information about this requirement, check out the SaveToStream method.

        The same note about the two components applies to this method.
      Parameters:
        OldStream - This stream contains the data before patching, designated
          the "old" data.
        NewStream - After Apply has finished, this stream will contain the
          patched version of the data.
        PatchData - Points to the data where the patch operations originated
          from (NewData if you used Compare, PatchData if you used
          LoadPatchOperations).
        Sender - If you pass an event handler in the next parameter, this
          Sender parameter will be passed to the event handler as the
          Sender parameter to it. You can pass nil here if your event handler
          does not need Sender.
        ProgressEvent - Optional parameter that you can use to get feedback
          during the patching. It will be called periodically to inform the
          program of its progress.
      See also:
        -
    }
    procedure Apply(const OldStream, NewStream: TStream;
      const PatchData: Pointer;
      const Sender: TObject=nil; const ProgressEvent: TProgressEvent=nil);

    { Description:
        Use this method to add a new patch operation to the list. The new
        operation will be added at the end of the current list.
    }
    procedure Add(const Operation: TPatchOperation);
  end;

  { Description:
      This is a wrapper for the Compare and IPatchOperations.SaveToStream
      functions to simplify the use of the code in this unit.

      Basically, you drop a TlvkPatchBuilder on a form or datamodule, or
      instantiate it from code, set the filename properties and call the
      Build method, and a patch file will be produced. See the help on the
      individual properties and methods for more information.

      The patch file can be applied using the other component,
      TlvkPatchApplier.
    See also:
      TlvkPatchApplier
  }
  TlvkPatchBuilder = class(TlvkComponent)
  private
    FOldFilename    : string;
    FNewFilename    : string;
    FPatchFilename  : string;
    FWindowSize     : Integer;
    FBlockSize      : Integer;
    FOnProgress     : TProgressEvent;
    procedure SetBlockSize(const Value: Integer);
    procedure SetWindowSize(const Value: Integer);

  public
    constructor Create(AOwner: TComponent); override;

    { Description:
        This method builds the patch file. The method compares the contents of
        the OldFilename and NewFilename files (which must both exist), produces
        a set of patch operations, and saves those operations to the
        PatchFilename file, which will be created and/or overwritten.

        This PatchFilename file, in addition to a copy of the OldFilename file,
        can be fed through the TlvkPatchApplier component to produce
        the NewFilename file.

        Note: Build requires all the filename properties to be set (that is,
          not contain a blank string), and the OldFilename and NewFilename
          properties must refer to files which exist and that the program
          has access to. The PatchFilename file will be created and/or
          overwritten, depending on wether it exists or not, and must
          point to a valid filename in a valid directory.
      See also:
        TlvkPatchApplier, TlvkPatchApplier.Apply, OldFilename, NewFilename,
        PatchFilename
    }
    procedure Build;

  published
    { Description:
        This property specifies the filename of the "old" file. It is used
        by Build to access the contents of the old file.

        This file must exist and be accessible by the program.
      See also:
        NewFilename, PatchFilename
    }
    property OldFilename: string read FOldFilename write FOldFilename;

    { Description:
        This property specifies the filename of the "new" file. It is used
        by Build to access the contents of the new file.

        This file must exist and be accessible by the program.
      See also:
        OldFilename, PatchFilename
    }
    property NewFilename: string read FNewFilename write FNewFilename;

    { Description:
        This property specifies the filename of the file where the patch
        operations will be saved when Build has finished comparing the
        OldFilename file and NewFilename file.

        It will be overwritten and the property must contain a valid, existing,
        path.
      See also:
        OldFilename, NewFilename
    }
    property PatchFilename: string read FPatchFilename write FPatchFilename;

    { Description:
        When comparing, and differences are found, the compare algorithm
        tries to find a match by looking ahead in both files for a set of
        bytes that appear in both files. WindowSize specifies how far ahead
        it will try to find this match.

        Setting a large value for this property might produce smaller patches,
        as more optimal matches can be found, but it will also take more
        time to produce the patch. The default value of 64Kb seems to be a good
        allround value.
      See also:
        BlockSize
    }
    property WindowSize: Integer read FWindowSize write SetWindowSize
      default DEFAULT_WINDOWSIZE;

    { Description:
        When comparing, and differences are found, the compare algorithm
        tries to find a match by looking ahead in both files for a set of
        bytes that appear in both files. BlockSize specifies how large this
        "set of bytes" must be before the algorithm considers it a match.

        Setting a small value for this might lead to false matches, and
        a high value might skip good matches. You can tune this to your data
        if you have the time and inclination to do so, but the default value
        of 12 seems to be a good allround value.
      See also:
        WindowSize
    }
    property BlockSize: Integer read FBlockSize write SetBlockSize
      default DEFAULT_BLOCKSIZE;

    { Description:
        This event handler, if set, will be called regularly during the
        build-process to inform the program of its progress. You can use
        it to provide feedback to the user in the form of a progressbar or
        similar, or even calculate approximately how much time it will take
        to complete the patch generation.
      See also:
        TProgressEvent
    }
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  { Description:
      This is a wrapper for the LoadPatchOperations and IPatchOperations.Apply
      functions to simplify the use of the code in this unit.

      Basically, you drop a TlvkPatchApplier on a form or datamodule, or
      instantiate it from code, set the filename properties and call the
      Apply method, and a new, updated, file will be produced. See the help
      on the individual properties and methods for more information.

      The patch file is generated by using the other copmonent,
      TlvkPatchBuilder.
    See also:
      TlvkPatchBuilder
  }
  TlvkPatchApplier = class(TlvkComponent)
  private
    FOldFilename    : string;
    FNewFilename    : string;
    FPatchFilename  : string;
    FOnProgress     : TProgressEvent;

  public
    { Description:
        This method takes the OldFilename file and the PatchFilename file
        and combines the two and produces the NewFilename file.

        All the filename properties must be set (that is, not be blank strings),
        and the OldFilename and PatchFilename properties must refer to existing
        files that the program has access to. The NewFilename file will be
        created and/or overwritten and must contain a valid, existing,
        pathname.
      See also:
        TlvkPatchBuilder, TlvkPatchBuilder.Build, OldFilename, NewFilename,
        PatchFilename
    }
    procedure Apply;

  published
    { Description:
        This property specifies the filename of the "old" file. It is used
        by Apply to access the appearance of the file before the patch was
        generated.

        This file must exist and be accessible by the program.
      See also:
        NewFilename, PatchFilename
    }
    property OldFilename: string read FOldFilename write FOldFilename;

    { Description:
        This property specifies the filename of the "new" file. It is used
        by Apply to create/overwrite a file with the contents of the combined
        OldFilename file and the PatchFilename file.

        This file will be created and/or overwritten. If it contains a pathname,
        the pathname must exist and must be accessible by the program.
      See also:
        OldFilename, PatchFilename
    }
    property NewFilename: string read FNewFilename write FNewFilename;

    { Description:
        This property specifies the filename of the "old" file. It is used
        by Apply to read the differences between the OldFilename file and
        the NewFilename file, and is combined with the contents of the
        OldFilename file to produce the NewFilename file from scratch.

        This file must exist and be accessible by the program.
      See also:
        NewFilename, PatchFilename
    }
    property PatchFilename: string read FPatchFilename write FPatchFilename;

    { Description:
        This event handler, if set, will be called regularly during the
        application-process to inform the program of its progress. You can use
        it to provide feedback to the user in the form of a progressbar or
        similar, or even calculate approximately how much time it will take
        to complete the application of the patch.
      See also:
        TProgressEvent
    }
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  { Description:
      This exception class is used for exceptions raised in the classes
      and functions in this unit.
  }
  EPatch = class(Exception);

{ Description:
    This function will create a blank patch operations list. You can use that
    if you want to explicitly build the patch operations yourself through
    the use of the Add method of the IPatchOperations interface.
  See also:
    IPatchOperations
}
function CreateBlankPatchOperations: IPatchOperations;

{ Description:
    This function will take the data pointed to by the two pointers and compare
    it. The result will be returned as a list of patch operations through a
    IPatchOperations interface. You can then use this interface to update
    a file, or save the operations to disk for future use.

    When comparing, the two blocks of data are designated "old" and "new".
    Think of it as revisions or versions of the same information, for instance
    an old and a newer version of a pascal file. The patch operations returned
    through the interface are the operations needed to "upgrade" the old data
    to have the contents of the new data.

    Note that this function does not only handle simple byte replacements,
    it also supports insertions, deletions, and block replacements (even
    when you replace a block with a differently sized block).

    Note: If you intend to save the patch operations to a stream, you must
      keep the NewData pointer intact until after you've done so. See
      IPatchOperations.SaveToStream for more information about this
      requirement.

  Parameters:
    OldData - The "old" data block.
    OldSize - The number of bytes in the "old" data block.
    NewData - The "new" data block.
    NewSize - The number of bytes in the "new" data block. Does not have to be
      the same as in OldSize.
    WindowSize - How many bytes forward to look for matching data when a
      difference is found. Larger values might generate smaller patch files,
      but will take more time to generate.
    BlockSize - When searching for a match after a difference is found, how
      many bytes must be equal in the two data blocks before the function
      considers it a good match. A too small value will generate false matches,
      and a too large value will skip good matches. 12 has proved to be a good
      all-round value.
      Sender - When calling the event handler, this parameter will be passed
        as the Sender parameter to it.
      ProgressEvent - If set, this event handler will be called while the
        patch is being generated.
    Returns:
      IPatchOperations interface which contains the operations necessary to
      upgrade the old data to the new data.
  See also:
    IPatchOperations
}
function Compare(
  const OldData: Pointer; const OldSize: Integer;
  const NewData: Pointer; const NewSize: Integer;
  const WindowSize: Integer=DEFAULT_WINDOWSIZE;
  const BlockSize: Integer=DEFAULT_BLOCKSIZE;
  const Sender: TObject=nil;
  const ProgressEvent: TProgressEvent=nil): IPatchOperations;

{ Description:
    This function will load patch operations from a stream and return a
    IPatchOperations interface containing those operations.

    Note: If you intend to apply the patch to an old file to upgrade it, you
      have to keep the stream data available as a pointer for the Apply
      method to work properly. See the IPatchOperations.Apply method for
      more information about this requirement.
  Parameters:
    Stream - The stream to load the patch operations from.
  See also:
    IPatchOperations
}
function LoadPatchOperations(const Stream: TStream): IPatchOperations;

resourcestring
  // This text is used for exceptions when trying to load patch operations from
  // a stream, and a valid signature is not found
  SERR_INVALID_SIGNATURE  = 'Invalid signature, not a patch file';
  // This text is used for exceptions when trying to load patch operations from
  // a stream, and a different version was found (signature is ok though)
  SERR_INVALID_VERSION    = 'Invalid version, cannot load this patch file';

implementation

uses
  lvkAdvancedFileStreams, Windows;

const
  PATCH_SIGNATURE = 'CPTC';
  PATCH_VERSION   = $0201;

  OP_INSERT       = 3 shl 6;
  OP_COPY         = 2 shl 6;
  OP_DELETE       = 1 shl 6;
  OP_REPLACE      = 0;
  OP_SINGLEBYTE   = 1 shl 5;

type
  IPatchOperationsInternal = interface
    ['{0477D538-1A6C-4D0B-AA71-A3B471F59E22}']

    procedure Add(const Operation: TPatchOperation);
    procedure LoadFromStream(const Stream: TStream);
    procedure FinalizeList;
  end;

  TPatchOperations = class(TInterfacedObject, IPatchOperations,
    IPatchOperationsInternal)
  private
    FOperationsCount  : Integer;
    FOperations       : array of TPatchOperation;

  protected
    // IPatchOperations interface
    function GetCount: Integer;
    function GetOperations(const Index: Integer): TPatchOperation;
    procedure SaveToStream(const Stream: TStream;
      const NewData: Pointer);
    procedure Apply(const OldStream, NewStream: TStream;
      const PatchData: Pointer;
      const Sender: TObject; const ProgressEvent: TProgressEvent=nil);
    procedure Add(const Operation: TPatchOperation);

    // IPatchOperationsInternal interface
    procedure LoadFromStream(const Stream: TStream);
    procedure FinalizeList;
  end;

function CreateBlankPatchOperations: IPatchOperations;
begin
  Result := TPatchOperations.Create;
end;

function FindSyncPoint(const OldBuffer, NewBuffer: PChar;
  const InOldBuffer, InNewBuffer, SynchronizeSize: Integer;
  var OldIndex, NewIndex: Integer): Boolean;
var
  Origin1		  : PChar;
  Origin2		  : PChar;
  BufferEnd1	: PChar;
  BufferEnd2	: PChar;
asm
  // Save registers
  push	esi
  push	edi
  push	ebx

  // Origin1 := eax; Origin2 := edx;
  mov		Origin1, eax
  mov		Origin2, edx

  // BufferEnd1 := Origin1 + InOldBuffer - SynchronizeSize
  add		eax, ecx
  sub		eax, SynchronizeSize
  mov		BufferEnd1, eax
  // BufferEnd2 := Origin2 + InNewBuffer - SynchronizeSize
  add		edx, InNewBuffer
  sub		edx, SynchronizeSize
  mov		BufferEnd2, edx

  // Now start up the comparison code
  mov		@Result, False
  mov		esi, Origin1
  mov		edi, Origin2
  mov		ebx, SynchronizeSize

  // Outer loop
@@outer:

  cmp		esi, BufferEnd1
  ja    @@outer_exit

  // edi := Origin2;
  mov		edi, Origin2
@@inner:

  cmp		edi, BufferEnd2
  ja		@@inner_exit

  mov		eax, [esi]
  cmp		eax, [edi]
  jne		@@compare_exit

  // Result := True; ecx := 0;
  mov		@Result, True
  mov		ecx, 4 // we already know the first four are equal
  jmp		@@compare // trick to minimize number of jumps in loop
@@compare_equal:
  inc		ecx
@@compare:

  cmp		ecx, ebx
  je		@@compare_exit


  mov		al, [esi+ecx]
  cmp		al, [edi+ecx]
  je		@@compare_equal

  // Result := False;
  mov		@Result, False
@@compare_exit:

  cmp		@Result, True
  je		@@outer_exit

  // Inc(edi); goto @@inner;
  inc		edi
  jmp		@@inner
@@inner_exit:

  // Inc(esi); goto @@outer;
  inc		esi
  jmp		@@outer
@@outer_exit:

  // OldIndex := esi - Origin1
  sub		esi, Origin1
  mov		eax, OldIndex
  mov		[eax], esi

  // NewIndex := edi - Origin2
  sub		edi, Origin2
  mov		eax, NewIndex
  mov		[eax], edi

@@exit:
  pop		ebx
  pop		edi
  pop		esi
end;

function Compare(
  const OldData: Pointer; const OldSize: Integer;
  const NewData: Pointer; const NewSize: Integer;
  const WindowSize: Integer;
  const BlockSize: Integer;
  const Sender: TObject;
  const ProgressEvent: TProgressEvent): IPatchOperations;
var
  LastOperation                   : TPatchOperation;
  OldPtr, OldPtrEnd, OldPtrStart  : PChar;
  NewPtr, NewPtrEnd, NewPtrStart  : PChar;
  PatchOperations                 : IPatchOperationsInternal;
  ContinueOperation               : Boolean;

  procedure DetermineLastCopy;
  var
    OldEnd, NewEnd  : PChar;
  begin
    OldEnd := OldPtrEnd;
    NewEnd := NewPtrEnd;

    while (OldEnd > OldPtr) and (NewEnd > NewPtr) and (OldEnd[-1] = NewEnd[-1]) do
    begin
      Dec(OldEnd);
      Dec(NewEnd);
    end;

    if OldEnd < OldPtrEnd then
    begin
      LastOperation.PatchOperationType := ptCopy;
      LastOperation.CopyAmount := OldPtrEnd - OldEnd;

      Dec(OldPtrEnd, LastOperation.CopyAmount);
      Dec(NewPtrEnd, LastOperation.CopyAmount);
    end else
      ZeroMemory(@LastOperation, SizeOf(LastOperation));
  end;

  procedure ProcessEqual;
  var
    CurrentOldPtr : PChar;
    Operation     : TPatchOperation;
  begin
    CurrentOldPtr := OldPtr;
    while (NewPtr < NewPtrEnd) and (OldPtr < OldPtrEnd) and (NewPtr^ = OldPtr^) do
    begin
      Inc(OldPtr);
      Inc(NewPtr);
    end;

    if OldPtr > CurrentOldPtr then
    begin
      Operation.PatchOperationType := ptCopy;
      Operation.CopyAmount := OldPtr - CurrentOldPtr;

      PatchOperations.Add(Operation);
    end;
  end;

  procedure ProcessLastDelete;
  var
    Operation : TPatchOperation;
  begin
    if OldPtr < OldPtrEnd then
    begin
      Operation.PatchOperationType := ptDelete;
      Operation.DeleteAmount := OldPtrEnd - OldPtr;
      PatchOperations.Add(Operation);
    end;
  end;

  procedure ProcessLastInsert;
  var
    Operation : TPatchOperation;
  begin
    if NewPtr < NewPtrEnd then
    begin
      Operation.PatchOperationType := ptInsert;
      Operation.InsertAmount := NewPtrEnd - NewPtr;
      Operation.InsertPosition := NewPtr - NewPtrStart;
      PatchOperations.Add(Operation);
    end;
  end;

  procedure Resynchronize;
  var
    OldIndex, NewIndex  : Integer;
    Operation           : TPatchOperation;
    Size                : Integer;
    rc                  : Boolean;
    OldSkip, NewSkip    : Integer;
  begin
    rc := False;
    Size := 64;
    while not rc do
    begin
      if (OldPtrEnd-OldPtr < Size) or (NewPtrEnd-NewPtr < Size) then
        Break;
      rc := FindSyncPoint(OldPtr, NewPtr, Size, Size, BlockSize, OldIndex, NewIndex);
      if rc then
        Break;
      Size := Size * 2;
      if Size > WindowSize then
        Break;
    end;

    if rc then
    begin
      if (OldIndex > 0) and (NewIndex > 0) then
      begin
        Operation.PatchOperationType := ptReplace;
        Operation.ReplaceAmount := OldIndex;
        Operation.ReplaceWithAmount := NewIndex;
        Operation.ReplacePosition := NewPtr-NewPtrStart;
        PatchOperations.Add(Operation);
      end else begin
        if OldIndex > 0 then
        begin
          Operation.PatchOperationType := ptDelete;
          Operation.DeleteAmount := OldIndex;
          PatchOperations.Add(Operation);
        end else begin
          Operation.PatchOperationType := ptInsert;
          Operation.InsertAmount := NewIndex;
          Operation.InsertPosition := NewPtr-NewPtrStart;
          PatchOperations.Add(Operation);
        end;
      end;

      Inc(OldPtr, OldIndex);
      Inc(NewPtr, NewIndex);
    end else begin
      Operation.PatchOperationType := ptReplace;
      if OldPtrEnd-OldPtr < WindowSize then
        OldSkip := OldPtrEnd-OldPtr
      else
        OldSkip := WindowSize;
      if NewPtrEnd-NewPtr < WindowSize then
        NewSkip := NewPtrEnd-NewPtr
      else
        NewSkip := WindowSize;

      Operation.ReplaceAmount := OldSkip;
      Operation.ReplaceWithAmount := NewSkip;
      Operation.ReplacePosition := NewPtr-NewPtrStart;
      PatchOperations.Add(Operation);

      Inc(OldPtr, OldSkip);
      Inc(NewPtr, NewSkip);
    end
  end;

begin
  Result := TPatchOperations.Create;
  PatchOperations := Result as IPatchOperationsInternal;

  Assert(Assigned(OldData));
  Assert(Assigned(NewData));

  OldPtrStart := OldData;
  OldPtr := OldPtrStart;
  OldPtrEnd := OldPtrStart + OldSize;

  NewPtrStart := NewData;
  NewPtrEnd := NewPtrStart + NewSize;
  NewPtr := NewPtrStart;

  DetermineLastCopy;

  while (NewPtr < NewPtrEnd) and (OldPtr < OldPtrEnd) do
  begin
    if Assigned(ProgressEvent) then
    begin
      ProgressEvent(Sender, (NewPtr-NewPtrStart) + (OldPtr-OldPtrStart),
        (NewPtrEnd-NewPtrStart) + (OldPtrEnd-OldPtrStart), ContinueOperation);
      if not ContinueOperation then
        Break;
    end;
    ProcessEqual;
    if (NewPtr < NewPtrEnd) and (OldPtr < OldPtrEnd) then
      Resynchronize;
  end;

  ProcessLastDelete;
  ProcessLastInsert;

  if LastOperation.CopyAmount > 0 then
    PatchOperations.Add(LastOperation);
  PatchOperations.FinalizeList;
end;

{ TPatchOperations }

procedure TPatchOperations.Add(const Operation: TPatchOperation);
begin
  if FOperationsCount >= Length(FOperations) then
  begin
    if Length(FOperations) = 0 then
      SetLength(FOperations, 16)
    else
      SetLength(FOperations, Length(FOperations)+1);
  end;
  FOperations[FOperationsCount] := Operation;
  Inc(FOperationsCount);
end;

procedure TPatchOperations.Apply(
  const OldStream, NewStream: TStream;
  const PatchData: Pointer;
  const Sender: TObject;
  const ProgressEvent: TProgressEvent);
var
  Index             : Integer;
  Operation         : PPatchOperation;
  Total             : Integer;
  ContinueOperation : Boolean;
begin
  Assert(Assigned(OldStream));
  Assert(Assigned(NewStream));
  Assert(Assigned(PatchData));

  OldStream.Position := 0;
  NewStream.Position := 0;

  Total := OldStream.Size;
  for Index := Low(FOperations) to High(FOperations) do
  begin
    if Assigned(ProgressEvent) then
    begin
      ProgressEvent(Sender, OldStream.Position, Total, ContinueOperation);
      if not ContinueOperation then
        Break;
    end;

    Operation := @FOperations[Index];
    case Operation^.PatchOperationType of
      ptCopy:
        NewStream.CopyFrom(OldStream, Operation^.CopyAmount);

      ptInsert:
        NewStream.WriteBuffer((PChar(PatchData) + Operation^.InsertPosition)^,
          Operation^.InsertAmount);

      ptDelete:
        OldStream.Position := OldStream.Position + Operation^.DeleteAmount;

      ptReplace:
        begin
          OldStream.Position := OldStream.Position + Operation^.ReplaceAmount;
          NewStream.WriteBuffer((PChar(PatchData) + Operation^.ReplacePosition)^,
            Operation^.ReplaceWithAmount);
        end;
    end;
  end;
end;

procedure TPatchOperations.FinalizeList;
begin
  SetLength(FOperations, FOperationsCount);
end;

function TPatchOperations.GetCount: Integer;
begin
  Result := Length(FOperations);
end;

function TPatchOperations.GetOperations(
  const Index: Integer): TPatchOperation;
begin
  Result := FOperations[Index];
end;

procedure TPatchOperations.LoadFromStream(const Stream: TStream);
var
  Signature       : array[0..Length(PATCH_SIGNATURE)] of Char;
  Version         : Word;
  TempInt         : Integer;
  Index           : Integer;
  TempByte        : Byte;

  function ReadAmount(const SizeNeeded: Byte): Integer;
  begin
    Result := 0;
    Stream.ReadBuffer(Result, SizeNeeded+1);
  end;

begin
  Stream.Position := 0;
  Assert(Assigned(Stream));
  Stream.ReadBuffer(Signature, SizeOf(Signature));
  if StrLComp(Signature, PATCH_SIGNATURE, Length(PATCH_SIGNATURE)) <> 0 then
    raise EPatch.Create(SERR_INVALID_SIGNATURE);
  Stream.ReadBuffer(Version, SizeOf(Version));
  if Version <> PATCH_VERSION then
    raise EPatch.Create(SERR_INVALID_VERSION);

  Stream.ReadBuffer(TempInt, SizeOf(TempInt));
  SetLength(FOperations, TempInt);

  for Index := Low(FOperations) to High(FOperations) do
  begin
    Stream.ReadBuffer(TempByte, 1);

    case TempByte and $C0 of
      OP_REPLACE:
        begin
          FOperations[Index].PatchOperationType := ptReplace;
          if (TempByte and OP_SINGLEBYTE) <> 0 then
          begin
            FOperations[Index].ReplaceAmount := TempByte and 31;
            FOperations[Index].ReplaceWithAmount := TempByte and 31;
          end else begin
            FOperations[Index].ReplaceAmount := ReadAmount(TempByte and 3);
            FOperations[Index].ReplaceWithAmount := ReadAmount((TempByte shr 2) and 3);
          end;
          FOperations[Index].ReplacePosition := Stream.Position;
          Stream.Position := Stream.Position + FOperations[Index].ReplaceWithAmount;
        end;

      OP_DELETE:
        begin
          FOperations[Index].PatchOperationType := ptDelete;
          if (TempByte and OP_SINGLEBYTE) <> 0 then
            FOperations[Index].DeleteAmount := TempByte and 31
          else
            FOperations[Index].DeleteAmount := ReadAmount(TempByte and 3);
        end;

      OP_COPY:
        begin
          FOperations[Index].PatchOperationType := ptCopy;
          if (TempByte and OP_SINGLEBYTE) <> 0 then
            FOperations[Index].CopyAmount := TempByte and 31
          else
            FOperations[Index].CopyAmount := ReadAmount(TempByte and 3);
        end;

      OP_INSERT:
        begin
          FOperations[Index].PatchOperationType := ptInsert;
          if (TempByte and OP_SINGLEBYTE) <> 0 then
            FOperations[Index].InsertAmount := TempByte and 31
          else
            FOperations[Index].InsertAmount := ReadAmount(TempByte and 3);
          FOperations[Index].InsertPosition := Stream.Position;
          Stream.Position := Stream.Position + FOperations[Index].InsertAmount;
        end;
    end;
  end;
end;

procedure TPatchOperations.SaveToStream(const Stream: TStream;
  const NewData: Pointer);
const
  Signature : array[0..Length(PATCH_SIGNATURE)] of Char = PATCH_SIGNATURE;
var
  TempInt   : Integer;
  TempByte  : Byte;
  TempWord  : Word;
  Index     : Integer;

  function SizeNeeded(const Amount: Integer): Byte;
  begin
    if Amount < (1 shl 8) then
      Result := 0
    else if Amount < (1 shl 16) then
      Result := 1
    else if Amount < (1 shl 24) then
      Result := 2
    else
      Result := 3;
  end;

begin
  Assert(Assigned(Stream));

  Stream.WriteBuffer(Signature, SizeOf(Signature));
  TempWord := PATCH_VERSION;
  Stream.WriteBuffer(TempWord, SizeOf(TempWord));

  TempInt := Length(FOperations);
  Stream.WriteBuffer(TempInt, SizeOf(TempInt));

  for Index := Low(FOperations) to High(FOperations) do
  begin
    case FOperations[Index].PatchOperationType of
      ptCopy:
        begin
          if FOperations[Index].CopyAmount < 32 then
          begin
            TempByte := OP_COPY or OP_SINGLEBYTE or
              FOperations[Index].CopyAmount;
            Stream.WriteBuffer(TempByte, 1);
          end else begin
            TempByte := OP_COPY or
              SizeNeeded(FOperations[Index].CopyAmount);
            Stream.WriteBuffer(TempByte, 1);
            Stream.WriteBuffer(FOperations[Index].CopyAmount,
              SizeNeeded(FOperations[Index].CopyAmount)+1);
          end;
        end;

      ptInsert:
        begin
          if FOperations[Index].InsertAmount < 32 then
          begin
            TempByte := OP_INSERT or OP_SINGLEBYTE or
              FOperations[Index].InsertAmount;
            Stream.WriteBuffer(TempByte, 1);
          end else begin
            TempByte := OP_INSERT or
              SizeNeeded(FOperations[Index].InsertAmount);
            Stream.WriteBuffer(TempByte, 1);
            Stream.WriteBuffer(FOperations[Index].InsertAmount,
              SizeNeeded(FOperations[Index].InsertAmount)+1);
          end;
          Stream.WriteBuffer((PChar(NewData) + FOperations[Index].InsertPosition)^, FOperations[Index].InsertAmount);
        end;

      ptDelete:
        begin
          if FOperations[Index].DeleteAmount < 32 then
          begin
            TempByte := OP_DELETE or OP_SINGLEBYTE or
              FOperations[Index].DeleteAmount;
            Stream.WriteBuffer(TempByte, 1);
          end else begin
            TempByte := OP_DELETE or
              SizeNeeded(FOperations[Index].DeleteAmount);
            Stream.WriteBuffer(TempByte, 1);
            Stream.WriteBuffer(FOperations[Index].DeleteAmount,
              SizeNeeded(FOperations[Index].DeleteAmount)+1);
          end;
        end;

      ptReplace:
        begin
          if (FOperations[Index].ReplaceAmount < 32) and
            (FOperations[Index].ReplaceWithAmount = FOperations[Index].ReplaceAmount) then
          begin
            TempByte := OP_REPLACE or OP_SINGLEBYTE or
              FOperations[Index].ReplaceAmount;
            Stream.WriteBuffer(TempByte, 1);
          end else begin
            TempByte := OP_REPLACE or
              SizeNeeded(FOperations[Index].ReplaceAmount) or
              SizeNeeded(FOperations[Index].ReplaceWithAmount) shl 2;
            Stream.WriteBuffer(TempByte, 1);
            Stream.WriteBuffer(FOperations[Index].ReplaceAmount,
              SizeNeeded(FOperations[Index].ReplaceAmount)+1);
            Stream.WriteBuffer(FOperations[Index].ReplaceWithAmount,
              SizeNeeded(FOperations[Index].ReplaceWithAmount)+1);
          end;
          Stream.WriteBuffer((PChar(NewData) + FOperations[Index].ReplacePosition)^, FOperations[Index].ReplaceWithAmount);
        end;
    end;
  end;
end;

function LoadPatchOperations(const Stream: TStream): IPatchOperations;
begin
  Result := TPatchOperations.Create;
  (Result as IPatchOperationsInternal).LoadFromStream(Stream);
end;

{ TlvkPatchBuilder }

constructor TlvkPatchBuilder.Create(AOwner: TComponent);
begin
  inherited;

  FWindowSize := DEFAULT_WINDOWSIZE;
  FBlockSize := DEFAULT_BLOCKSIZE;
end;

procedure TlvkPatchBuilder.Build;
var
  Operations  : IPatchOperations;
  OldStream   : TFileStream;
  NewStream   : TFileStream;
  OldData     : TMemoryMappedFileStream;
  NewData     : TMemoryMappedFileStream;
  PatchStream : TStream;
begin
  Assert((FOldFilename <> '') and FileExists(FOldFilename));
  Assert((FNewFilename <> '') and FileExists(FNewFilename));
  Assert(FPatchFilename <> '');

  OldData := nil;
  NewData := nil;
  OldStream := nil;
  NewStream := nil;
  try
    OldStream := TFileStream.Create(FOldFilename, fmOpenRead or fmShareDenyWrite);
    OldData := TMemoryMappedFileStream.Create(OldStream, fmOpenRead, 0, OldStream.Size);

    NewStream := TFileStream.Create(FNewFilename, fmOpenRead or fmShareDenyWrite);
    NewData := TMemoryMappedFileStream.Create(NewStream, fmOpenRead, 0, NewStream.Size);

    Operations := Compare(OldData.Memory, OldData.Size, NewData.Memory,
      NewData.Size, FWindowSize, FBlockSize, Self, FOnProgress);

    PatchStream := TFileStream.Create(FPatchFilename, fmCreate);
    try
      Operations.SaveToStream(PatchStream, NewData.Memory);
    finally
      PatchStream.Free;
    end;
  finally
    OldData.Free;
    NewData.Free;
    OldStream.Free;
    NewStream.Free;
  end;
end;

procedure TlvkPatchBuilder.SetBlockSize(const Value: Integer);
begin
  if Value < 4 then
    FBlockSize := 4
  else
    FBlockSize := Value;
end;

procedure TlvkPatchBuilder.SetWindowSize(const Value: Integer);
begin
  if Value < 1024 then
    FWindowSize := 1024
  else
    FWindowSize := Value;
end;

{ TlvkPatchApplier }

procedure TlvkPatchApplier.Apply;
var
  Operations  : IPatchOperations;
  PatchStream : TFileStream;
  PatchData   : TMemoryMappedFileStream;
  OldStream   : TStream;
  NewStream   : TStream;
begin
  Assert((FOldFilename <> '') and FileExists(FOldFilename));
  Assert(FNewFilename <> '');
  Assert((FPatchFilename <> '') and FileExists(FPatchFilename));

  PatchStream := TFileStream.Create(FPatchFilename, fmOpenRead or fmShareDenyWrite);
  try
    PatchData := TMemoryMappedFileStream.Create(PatchStream, fmOpenRead,
      0, PatchStream.Size);
    try
      Operations := LoadPatchOperations(PatchStream);

      OldStream := TFileStream.Create(FOldFilename, fmOpenRead);
      try
        NewStream := TFileStream.Create(FNewFilename, fmCreate);
        try
          Operations.Apply(OldStream, NewStream, PatchData.Memory, Self,
            FOnProgress);
        finally
          NewStream.Free;
        end;
      finally
        OldStream.Free;
      end;
    finally
      PatchData.Free;
    end;
  finally
    PatchStream.Free;
  end;
end;

end.
