{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the new compare and patch algorithm implementation.
}
unit lvkBinaryCompare2;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 4 $
// $Archive: /Components/LVK/source/lvkBinaryCompare2.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}
{$R-,Q-,O-}

uses
  lvkComponents, SysUtils, Classes, lvkTypes;

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
      There is only two types of patch operations available, insert and copy.
      Insert inserts new data into the file, copy copies data from the old
      file to the new file.
    See also:
      TPatchOperation
  }
  TPatchOperationType = (
    // Copy a number of bytes from the old file to the new file.
    ptCopy,
    // Insert a number of bytes into the new file.
    ptInsert);

  // <COMBINE TPatchOperation>
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
    OperationType : TPatchOperationType;
    Amount        : UInt32;
    Offset        : UInt32;
  end;

  { Description:
      This interface is the key to the patching framework. When you do a compare
      of an old and a new file, you'll get an object wrapped in this interface
      in return, giving you access to the patch operations that the compare
      resulted in.
  }
  IPatchOperations = interface
    ['{09A3F406-C191-413B-B87A-3F9233110C56}']

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
          for easier use: TlvkPatchBuilder2 and TlvkPatchApplier2.
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
    }
    procedure Apply(const OldStream, NewStream: TStream;
      const PatchData: Pointer;
      const Sender: TObject=nil; const ProgressEvent: TProgressEvent=nil);

    { Description:
        Use this method to add a new patch operation to the list. The new
        operation will be added at the end of the current list.
    }
    procedure Add(const Operation: TPatchOperation);

    { Description:
        Use this method to clear the list of patch operations.
    }
    procedure Clear;
  end;

  { Description:
      This exception class is used for exceptions raised in the classes
      and functions in this unit.
  }
  EPatch = class(Exception);

  { Description:
      This is a wrapper for the Compare and IPatchOperations.SaveToStream
      functions to simplify the use of the code in this unit.

      Basically, you drop a TlvkPatchBuilder2 on a form or datamodule, or
      instantiate it from code, set the filename properties and call the
      Build method, and a patch file will be produced. See the help on the
      individual properties and methods for more information.

      The patch file can be applied using the other component,
      TlvkPatchApplier2.
    See also:
      TlvkPatchApplier2
  }
  TlvkPatchBuilder2 = class(TlvkComponent)
  private
    FOldFilename    : string;
    FNewFilename    : string;
    FPatchFilename  : string;
    FOnProgress     : TProgressEvent;

  public
    { Description:
        This method builds the patch file. The method compares the contents of
        the OldFilename and NewFilename files (which must both exist), produces
        a set of patch operations, and saves those operations to the
        PatchFilename file, which will be created and/or overwritten.

        This PatchFilename file, in addition to a copy of the OldFilename file,
        can be fed through the TlvkPatchApplier2 component to produce
        the NewFilename file.

        Note: Build requires all the filename properties to be set (that is,
          not contain a blank string), and the OldFilename and NewFilename
          properties must refer to files which exist and that the program
          has access to. The PatchFilename file will be created and/or
          overwritten, depending on wether it exists or not, and must
          point to a valid filename in a valid directory.
      See also:
        TlvkPatchApplier2, TlvkPatchApplier2.Apply, OldFilename, NewFilename,
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

      Basically, you drop a TlvkPatchApplier2 on a form or datamodule, or
      instantiate it from code, set the filename properties and call the
      Apply method, and a new, updated, file will be produced. See the help
      on the individual properties and methods for more information.

      The patch file is generated by using the other copmonent,
      TlvkPatchBuilder2.
    See also:
      TlvkPatchBuilder2
  }
  TlvkPatchApplier2 = class(TlvkComponent)
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
        TlvkPatchBuilder2, TlvkPatchBuilder2.Build, OldFilename, NewFilename,
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
    OldDataSize - The number of bytes in the "old" data block.
    NewData - The "new" data block.
    NewDataSize - The number of bytes in the "new" data block. Does not have to
      be the same as in OldSize.
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
  const OldData: PChar; const OldDataSize: UInt32;
  const NewData: PChar; const NewDataSize: UInt32;
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
  lvkAdvancedFileStreams, Math;

const
  PATCH_SIGNATURE = 'CPTC';
  PATCH_VERSION   = $0300;

  OP_INSERT_8       = 0;
  OP_INSERT_16      = 1;
  OP_INSERT_32      = 2;
  OP_INSERT_CUSTOM  = 3;
  OP_COPY_8         = 128;
  OP_COPY_16        = 129;
  OP_COPY_32        = 130;
  OP_COPY_CUSTOM    = 131;

type
  IPatchOperationsInternal = interface
    ['{0477D538-1A6C-4D0B-AA71-A3B471F59E22}']

    procedure Add(const Operation: TPatchOperation);
    procedure LoadFromStream(const Stream: TStream);
  end;

  TPatchOperations = class(TInterfacedObject, IPatchOperations,
    IPatchOperationsInternal)
  private
    FOperations : TList;

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
    procedure Clear;

    // IPatchOperationsInternal interface
    procedure LoadFromStream(const Stream: TStream);

  public
    constructor Create;
    destructor Destroy; override;
  end;

function CreateBlankPatchOperations: IPatchOperations;
begin
  Result := TPatchOperations.Create as IPatchOperations;
end;

function LoadPatchOperations(const Stream: TStream): IPatchOperations;
begin
  Result := CreateBlankPatchOperations;
  (Result as IPatchOperationsInternal).LoadFromStream(Stream);
end;

function Compare(
  const OldData: PChar; const OldDataSize: UInt32;
  const NewData: PChar; const NewDataSize: UInt32;
  const Sender: TObject=nil;
  const ProgressEvent: TProgressEvent=nil): IPatchOperations;
type
  TSortData = packed record
    FirstFour : UInt32;
    Position  : PChar;
  end;
  TLocationList = array of PChar;
var
  Operations      : IPatchOperations;
  SortedOldFile   : array of TSortData;
  CurrentPtr      : PChar;
  EndPtr          : PChar;
  InsertPosition  : UInt32;
  InsertLength    : UInt32;
  LastProgress    : TDateTime;

  procedure SortOldFile;
  var
    Index : UInt32;
    Temp  : array[0..6] of Byte;

    procedure QuickSort(L, R: Int32);
    var
      I, J, P : Int32;
      Temp    : TSortData;
    begin
      repeat
        I := L;
        J := R;
        P := (L + R) shr 1;
        repeat
          while (SortedOldFile[I].FirstFour < SortedOldFile[P].FirstFour) do Inc(I);
          while (SortedOldFile[J].FirstFour > SortedOldFile[P].FirstFour) do Dec(J);
          if I <= J then
          begin
            Temp := SortedOldFile[I];
            SortedOldFile[I] := SortedOldFile[J];
            SortedOldFile[J] := Temp;

            if P = I then
              P := J
            else if P = J then
              P := I;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if L < J then QuickSort(L, J);
        L := I;
      until I >= R;
    end;

  begin
    SetLength(SortedOldFile, OldDataSize);

    // Assign positions
    for Index := 0 to OldDataSize - 1 do
      SortedOldFile[Index].Position := OldData + Index;

    // Copy all the complete 4-char values
    for Index := 0 to OldDataSize - 4 do
      SortedOldFile[Index].FirstFour := PUInt32(OldData + Index)^;

    // Copy the 4-char values straddling file boundary
    Move((OldData + OldDataSize - 4)^, Temp, 4);
    Move(OldData^, Temp[4], 3);
    for Index := 0 to 3 do
      SortedOldFile[OldDataSize - 4 + Index].FirstFour := PUInt32(@Temp[Index])^;

    // Sort the values
    QuickSort(0, OldDataSize-1);
  end;

  procedure InsertByte;
  begin
    if InsertLength = 0 then
      InsertPosition := CurrentPtr - NewData;
    Inc(InsertLength);
    Inc(CurrentPtr);
  end;

  procedure FlushInsert;
  var
    Operation : TPatchOperation;
  begin
    if InsertLength > 0 then
    begin
      Operation.OperationType := ptInsert;
      Operation.Offset := InsertPosition;
      Operation.Amount := InsertLength;
      Operations.Add(Operation);
      InsertLength := 0;
    end;
  end;

  function FindLocationCandidates(const FirstFour: UInt32): TLocationList;
  var
    i1, i2, ic  : UInt32;
    Value       : UInt32;
  begin
    i1 := 0;
    i2 := OldDataSize - 1;
    ic := 0;
    Value := 0;

    while (i1 <= i2) do
    begin
      ic := (i1 + i2) div 2;
      Value := SortedOldFile[ic].FirstFour;

      if FirstFour < Value then
        i2 := ic - 1
      else if FirstFour > Value then
        i1 := ic + 1
      else
        Break;
    end;

    if (i1 <= i2) and (FirstFour = Value) then
    begin
      while (ic > 0) and (SortedOldFile[ic-1].FirstFour = FirstFour) do
        Dec(ic);

      i1 := ic;
      while (ic < OldDataSize) and (SortedOldFile[ic].FirstFour = FirstFour) do
        Inc(ic);
      i2 := ic-1;
      SetLength(Result, i2-i1+1);

      for ic := 0 to Length(Result)-1 do
        Result[ic] := SortedOldFile[i1 + ic].Position;
    end else
      Result := nil;
  end;

  function GetMatchLength(const OldPtr, NewPtr: PChar): UInt32;
  var
    CurrentOldPtr : PChar;
    CurrentNewPtr : PChar;
  begin
    Result := 0;
    CurrentOldPtr := OldPtr;
    CurrentNewPtr := NewPtr;

    while (CurrentNewPtr < EndPtr) and (CurrentOldPtr^ = CurrentNewPtr^) do
    begin
      Inc(Result);

      Inc(CurrentOldPtr);
      if CurrentOldPtr = OldData + OldDataSize then
        CurrentOldPtr := OldData;

      Inc(CurrentNewPtr);
    end;
  end;

  procedure CopyBytes;
  var
    LocationCandidates  : TLocationList;
    MaxLength           : UInt32;
    MaxPosition         : PChar;
    NewLength           : UInt32;
    Index               : UInt32;
    Operation           : TPatchOperation;
  begin
    LocationCandidates := FindLocationCandidates(PUInt32(CurrentPtr)^);
    if Length(LocationCandidates) = 0 then
      InsertByte
    else begin
      MaxLength := GetMatchLength(LocationCandidates[0], CurrentPtr);
      MaxPosition := LocationCandidates[0];

      for Index := 1 to High(LocationCandidates) do
      begin
        NewLength := GetMatchLength(LocationCandidates[Index], CurrentPtr);
        if NewLength > MaxLength then
        begin
          MaxLength := NewLength;
          MaxPosition := LocationCandidates[Index];
        end;
      end;

      if MaxLength <= 6 then
        InsertByte
      else begin
        FlushInsert;
        Operation.OperationType := ptCopy;
        Operation.Offset := MaxPosition - OldData;
        Operation.Amount := MaxLength;
        Operations.Add(Operation);
        Inc(CurrentPtr, MaxLength);
      end;
    end;
  end;

  function DoProgress: Boolean;
  begin
    Result := True;
    if Assigned(ProgressEvent) and (Now - LastProgress > 1/86400) then
    begin
      ProgressEvent(Sender, CurrentPtr - NewData, NewDataSize, Result);
      LastProgress := Now;
    end;
  end;

begin
  Operations := CreateBlankPatchOperations;
  SortOldFile;

  CurrentPtr := NewData;
  EndPtr := NewData + NewDataSize;
  InsertLength := 0;
  LastProgress := 0;
  if DoProgress then
  begin
    while CurrentPtr < EndPtr do
    begin
      if EndPtr - CurrentPtr <= 9 then
        InsertByte
      else
        CopyBytes;

      if not DoProgress then
      begin
        Operations.Clear;
        Break;
      end;
    end;
  end;
  FlushInsert;

  LastProgress := 0;
  DoProgress;
  Result := Operations;
end;

{ TPatchOperations }

procedure TPatchOperations.Add(const Operation: TPatchOperation);
var
  DynamicOperation  : PPatchOperation;
begin
  New(DynamicOperation);
  DynamicOperation^ := Operation;
  FOperations.Add(DynamicOperation);
end;

procedure TPatchOperations.Apply(const OldStream, NewStream: TStream;
  const PatchData: Pointer; const Sender: TObject;
  const ProgressEvent: TProgressEvent);
var
  Index         : UInt32;
  Operation     : TPatchOperation;
  TotalLength   : UInt32;
  LastProgress  : TDateTime;
  ToCopy        : UInt32;

  function DoProgress: Boolean;
  begin
    Result := True;
    if Assigned(ProgressEvent) and (Now - LastProgress > 1/86400) then
    begin
      ProgressEvent(Sender, NewStream.Size, TotalLength, Result);
      LastProgress := Now;
    end;
  end;

begin
  Assert(Assigned(OldStream));
  Assert(Assigned(NewStream));
  Assert(Assigned(PatchData));

  TotalLength := 0;

  for Index := 0 to FOperations.Count-1 do
  begin
    Operation := PPatchOperation(FOperations[Index])^;
    Inc(TotalLength, Operation.Amount);
  end;
  LastProgress := 0.0;

  if DoProgress then
  begin
    for Index := 0 to FOperations.Count-1 do
    begin
      Operation := PPatchOperation(FOperations[Index])^;
      case Operation.OperationType of
        ptInsert:
          NewStream.WriteBuffer((PChar(PatchData) + Operation.Offset)^, Operation.Amount);

        ptCopy:
          begin
            while Operation.Amount > 0 do
            begin
              OldStream.Position := Operation.Offset;
              ToCopy := Min(UInt32(OldStream.Size) - Operation.Offset, Operation.Amount);
              NewStream.CopyFrom(OldStream, ToCopy);
              Dec(Operation.Amount, ToCopy);
              if Operation.Amount > 0 then
                Operation.Offset := 0;
            end;
          end;
      end;
      if not DoProgress then
        Break;
    end;
  end;
  LastProgress := 0;
  DoProgress;
end;

procedure TPatchOperations.Clear;
var
  Index : Integer;
begin
  for Index := 0 to FOperations.Count-1 do
    Dispose(PPatchOperation(FOperations[Index]));
  FOperations.Clear;
end;

constructor TPatchOperations.Create;
begin
  inherited Create;

  FOperations := TList.Create;
end;

destructor TPatchOperations.Destroy;
begin
  Clear;
  FOperations.Free;

  inherited;
end;

function TPatchOperations.GetCount: Integer;
begin
  Result := FOperations.Count;
end;

function TPatchOperations.GetOperations(
  const Index: Integer): TPatchOperation;
begin
  Result := PPatchOperation(FOperations[Index])^;
end;

procedure TPatchOperations.LoadFromStream(const Stream: TStream);
var
  Signature : array[0..Length(PATCH_SIGNATURE)] of Char;
  Version   : UInt16;
  TempInt   : UInt32;
  Index     : UInt32;

  procedure LoadOperation;
  var
    ID        : UInt8;
    Len8      : UInt8;
    Len16     : UInt16;
    Len32     : UInt32;
    Offset    : UInt32;
    Amount    : UInt32;
    Operation : TPatchOperation;
  begin
    Stream.ReadBuffer(ID, 1);

    Len8 := 0;
    Len16 := 0;
    Len32 := 0;

    case ID of
      OP_INSERT_8:
        Stream.ReadBuffer(Len8, 1);

      OP_INSERT_16:
        Stream.ReadBuffer(Len16, 2);

      OP_INSERT_32:
        Stream.ReadBuffer(Len32, 4);

      OP_INSERT_CUSTOM..OP_INSERT_CUSTOM+124:
        Len8 := ID - OP_INSERT_CUSTOM + 1;

      OP_COPY_8:
        begin
          Stream.ReadBuffer(Len8, 1);
          Stream.ReadBuffer(Offset, 4);
        end;

      OP_COPY_16:
        begin
          Stream.ReadBuffer(Len16, 2);
          Stream.ReadBuffer(Offset, 4);
        end;

      OP_COPY_32:
        begin
          Stream.ReadBuffer(Len32, 4);
          Stream.ReadBuffer(Offset, 4);
        end;

      OP_COPY_CUSTOM..OP_COPY_CUSTOM + 124:
        begin
          Len8 := ID - OP_COPY_CUSTOM + 1;
          Stream.ReadBuffer(Offset, 4);
        end;
    end;
    Amount := Len8 + Len16 + Len32;
    if ID in [OP_INSERT_8..OP_INSERT_8 + 127] then
    begin
      Offset := Stream.Position;
      Stream.Position := UInt32(Stream.Position) + Amount;

      Operation.OperationType := ptInsert;
    end else
      Operation.OperationType := ptCopy;

    Operation.Amount := Amount;
    Operation.Offset := Offset;
    Add(Operation);
  end;

begin
  Assert(Assigned(Stream));        
  Stream.Position := 0;

  Stream.ReadBuffer(Signature, SizeOf(Signature));
  if StrLComp(Signature, PATCH_SIGNATURE, Length(PATCH_SIGNATURE)) <> 0 then
    raise EPatch.Create(SERR_INVALID_SIGNATURE);

  Stream.ReadBuffer(Version, SizeOf(Version));
  if Version <> PATCH_VERSION then
    raise EPatch.Create(SERR_INVALID_VERSION);

  Stream.ReadBuffer(TempInt, SizeOf(TempInt));
  Clear;
  for Index := 0 to TempInt-1 do
    LoadOperation;
end;

procedure TPatchOperations.SaveToStream(const Stream: TStream;
  const NewData: Pointer);
const
  Signature : array[0..Length(PATCH_SIGNATURE)] of Char = PATCH_SIGNATURE;
var
  Index : Integer;

  procedure Header;
  var
    TempWord  : UInt16;
    TempInt   : UInt32;
  begin
    Stream.WriteBuffer(Signature, SizeOf(Signature));
    TempWord := PATCH_VERSION;
    Stream.WriteBuffer(TempWord, SizeOf(TempWord));

    TempInt := FOperations.Count;
    Stream.WriteBuffer(TempInt, SizeOf(TempInt));
  end;

  procedure Output8(const Value: UInt8);
  begin
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure Output16(const Value: UInt16);
  begin
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure Output32(const Value: UInt32);
  begin
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure SaveOperation(const Operation: TPatchOperation);
  begin
    case Operation.OperationType of
      ptInsert:
        begin
          case Operation.Amount of
            1..124:
              Output8(OP_INSERT_CUSTOM + Operation.Amount - 1);

            125..255:
              begin
                Output8(OP_INSERT_8);
                Output8(Operation.Amount);
              end;

            256..65535:
              begin
                Output8(OP_INSERT_16);
                Output16(Operation.Amount);
              end;

            65536..MAX_UINT_32:
              begin
                Output8(OP_INSERT_32);
                Output32(Operation.Amount);
              end;
          end;
          Stream.WriteBuffer((PChar(NewData) + Operation.Offset)^, Operation.Amount);
        end;

      ptCopy:
        begin
          case Operation.Amount of
            1..125:
              Output8(OP_COPY_CUSTOM + Operation.Amount - 1);

            126..255:
              begin
                Output8(OP_COPY_8);
                Output8(Operation.Amount);
              end;

            256..65535:
              begin
                Output8(OP_COPY_16);
                Output16(Operation.Amount);
              end;

            65536..MAX_UINT_32:
              begin
                Output8(OP_COPY_32);
                Output32(Operation.Amount);
              end;
          end;
          Output32(Operation.Offset);
        end;
    end;
  end;

begin
  Assert(Assigned(Stream));

  Header;

  for Index := 0 to FOperations.Count-1 do
    SaveOperation(PPatchOperation(FOperations[Index])^);
end;

{ TlvkPatchBuilder2 }

procedure TlvkPatchBuilder2.Build;
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
      NewData.Size, Self, FOnProgress);

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

{ TlvkPatchApplier2 }

procedure TlvkPatchApplier2.Apply;
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
