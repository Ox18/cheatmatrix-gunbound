{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code for allocating memory in such a way that it will
    be released automatically when it's no longer needed.
}
unit lvkSafeMem;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSafeMem.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes;

type
  { Description:
      This is the interface that wraps around the safe memory object. It
      provides means to get access to the allocated memory block, as well
      as manipulating it.
  }
  ISafeMem = interface
    ['{E8D94972-A700-4FD7-82D0-C191131979BC}']

    { Description:
        This property returns the current size of the memory block. A size of
        0 means that currently there is no memory allocated to the object.

        You can write a new value into this property to change the allocated
        size. Set it to 0 and the memory block will be released, set it
        to a positive value and it will be reallocated or allocated depending
        on its current status.

        Note: The Pointer property might return different values before
          and after a change in size, so if you need to change the size, don't
          cache the pointer value outside this object.
      Parameters:
        -
      See also:
        -
    }
    function GetSize: Integer;
    // <COMBINE GetSize>
    procedure SetSize(const NewSize: Integer);
    // <COMBINE GetSize>
    property Size: Integer read GetSize write SetSize;

    { Description:
        This method releases the memory object, simply by setting the size to
        0.
      See also:
        GetSize
    }
    procedure Release;

    { Description:
        This method grows the memory block by adjusting the size upwards by
        the amount specified.

        Note: The Pointer property might return different values before
          and after a change in size, so if you need to change the size, don't
          cache the pointer value outside this object.
      Parameters:
        By - The amount of memory to adjust the size of the memory block with.
      See also:
        Size, Shrink
    }
    procedure Grow(const By: Integer);

    { Description:
        This method shrinks the memory block by adjusting the size downwards by
        the amount specified.

        Note: The Pointer property might return different values before
          and after a change in size, so if you need to change the size, don't
          cache the pointer value outside this object.
      Parameters:
        By - The amount of memory to adjust the size of the memory block with.
      See also:
        Size, Grow
    }
    procedure Shrink(const By: Integer);

    { Description:
        This method will fill the entire memory object with values corresponding
        to the specified byte.
      Parameters:
        b - The byte value to fill the memory object with.
      See also:
        Zero
    }
    procedure Fill(const b: Byte);

    { Description:
        This method will fill the entire memory object with zeroes, to blank
        out the data stored in it.
      See also:
        Fill
    }
    procedure Zero;

    { Description:
        This property returns a pointer to the start of the memory block.

        Note: The Pointer property might return different values before
          and after a change in size, so if you need to change the size, don't
          cache the pointer value outside this object.
      See also:
        Size
    }
    function GetPointer: Pointer;
    // <COMBINE GetPointer>
    property Pointer: Pointer read GetPointer;

    { Description:
        This property returns a stream object that wraps the contents of the
        memory block. Note that the stream object is owned by the safe memory
        object, and only one stream object per safe memory object is created.
    }
    function GetStream: TStream;
    // <COMBINE GetStream>
    property Stream: TStream read GetStream;

    { Description:
        This property returns the contents of the memory object as a string,
        and you can also set the contents of the object to the value of a
        string by writing to it.
    }
    function GetAsString: string;
    // <COMBINE GetAsString>
    procedure SetAsString(const NewValue: string);
    // <COMBINE GetAsString>
    property AsString: string read GetAsString write SetAsString;

    { Description:
        This method returns a new safe memory object. The default setting is
        to copy the contents of the memory into this new object before returning
        it, but if you specify False as the parameter, only a new safe memory
        object of the same size will be returned. You can use this to produce
        copies of the memory object, or new buffers of the same size.
      Parameters:
        CopyContents - Defaults to True, which means that the object that is
          returned will have the exact same contents as the original.
    }
    function Clone(const CopyContents: Boolean=True): ISafeMem;

    { Description:
        This method loads data from the given file into the safe memory
        object, replacing the entire contents of it. This means that if the
        size of the file is less than the current size of the safe memory
        object, the safe memory object will be reduced in size to match the
        file.
      Parameters:
        FileName - Full path and filename of the file to load into the
          object.
      See also:
        SaveToFile
    }
    procedure LoadFromFile(const FileName: string);

    { Description:
        This method saves the contents of the safe memory object to a file
        with the given filename, overwriting the file entirely. The file will
        have size that matches the size of the object after this method
        has completed.
      Parameters:
        FileName - Full path and filename of the file to save the contents to.
      See also:
        LoadFromFile
    }
    procedure SaveToFile(const FileName: string);

    { Description:
        This function tries to grab enough data from the stream to fill the
        entire buffer. If you let the AdjustSize parameter be True, the
        buffer will adjust its size to reflect the amount of data it managed
        to grab.
      Parameters:
        Stream - The stream to grab data from.
        AdjustSize - Leave as True if you want the buffer object to adjust its
          size to match the grabbed amount.
      Returns:
        The number of bytes it managed to grab.
    }
    function Grab(const Stream: TStream;
      const AdjustSize: Boolean=True): Integer;
  end;

  // <COMBINE NewDisposable@Pointer@TDisposeProcedure>
  TDisposeProcedure = procedure(const Data: Pointer);
  // <COMBINE NewDisposable@Pointer@TDisposeMethod>
  TDisposeMethod = procedure(const Sender: TObject; const Data: Pointer) of object;

  { Description:
      This interface is used with disposable classes. The purpose is that
      you can create a disposable class that knows about a certain type
      of record type, and then put a reference to such a class instance
      inside the record. This way, when the record goes out of scope, the
      interface will be destroyed, and the class will clean up the data
      allocated to the record. Sort of an auto-dispose on record types.
    See also:
      IDisposable.Forget, IDisposable.Dispose
  }
  IDisposable = interface
    ['{C4A1C82E-7D1A-4A7D-9AA9-C1DBED9F4BF1}']

    { Description:
        You can call this method if you want the disposable to forget the
        job it's supposed to do when the disposable reference goes out of
        scope. This is useful if you want to take over the job of cleaning up
        and hand it over to a different piece of code.
      See also:
        Dispose
    }
    procedure Forget;

    { Description:
        You can call this method to dispose of the data that the object refers
        to before the disposable reference goes out of scope.
      See also:
        Forget
    }
    procedure Dispose;
  end;

{ Description:
    This function allocates a new memory block in a ISafeMem reference, and
    returns the new interface reference for it.
  Parameters:
    Amount      - The size of the memory block to allocate.
    UseTempFile - Set this to True to have the procedure create a temporary
      file and map this into memory for the memory block. This way you can
      create huge memory blocks that occupies disk space, and not so much
      physical memory.
}
function AllocateSafeMem(const Amount: Integer=0;
  const UseTempFile: Boolean=False): ISafeMem;

{ Description:
    This function creates a disposable object for a block of memory that you
    want to Free when the disposable goes out of scope. The procedure specified
    as the second parameter is the one that is responsible for destroying
    the data when it goes out of scope.
  Parameters:
    Data    - The data block to dispose of when the dispose object goes out of
      scope.
    Dispose - The procedure that will handle the dispose mechanism for this
      data block.
  See also:
    NewDisposable@Pointer@TDisposeMethod
}
function NewDisposable(const Data: Pointer;
  const Dispose: TDisposeProcedure): IDisposable; overload;

{ Description:
    This function creates a disposable object for a block of memory that you
    want to Free when the disposable goes out of scope. The method specified
    as the second parameter is the one that is responsible for destroying
    the data when it goes out of scope.
  Parameters:
    Data    - The data block to dispose of when the dispose object goes out of
      scope.
    Dispose - The method that will handle the dispose mechanism for this
      data block.
  See also:
    NewDisposable@Pointer@TDisposeProcedure
}
function NewDisposable(const Data: Pointer;
  const Dispose: TDisposeMethod): IDisposable; overload;

implementation

uses
  Windows, SysUtils;

type
  TBaseSafeMem = class(TInterfacedObject, ISafeMem)
  private
    FStream : TStream;

  protected
    function GetSize: Integer; virtual;
    procedure SetSize(const NewSize: Integer); virtual;
    function Clone(const CopyContents: Boolean): ISafeMem; virtual;
    function GetPointer: Pointer; virtual;
    procedure Release; virtual;
    procedure Grow(const By: Integer); virtual;
    procedure Shrink(const By: Integer); virtual;
    function GetStream: TStream; virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(const NewValue: string); virtual;
    procedure Fill(const b: Byte); virtual;
    procedure Zero; virtual;
    function Grab(const Stream: TStream;
      const AdjustSize: Boolean=True): Integer; virtual;

  public
    constructor Create(const Stream: TStream);
    destructor Destroy; override;
  end;

  TSafeMem = class(TBaseSafeMem, ISafeMem)
  protected
    function Clone(const CopyContents: Boolean): ISafeMem; override;
    function GetPointer: Pointer; override;

  public
    constructor Create(const Amount: Integer);
  end;

  TTempFileSafeMem = class(TBaseSafeMem, ISafeMem)
  private
    FFileName       : string;
    FDummy          : Byte;

    FMappingHandle  : THandle;
    FMemoryMap      : PAnsiChar;
    FMemoryMapSize  : Integer;

    procedure CloseMemoryMap;
    procedure OpenMemoryMap;

  protected
    function Clone(const CopyContents: Boolean): ISafeMem; override;
    function GetPointer: Pointer; override;

  public
    constructor Create(const Amount: Integer);
    destructor Destroy; override;
  end;

  TDisposable = class(TInterfacedObject, IDisposable)
  private
    FData             : Pointer;
    FDisposeProcedure : TDisposeProcedure;
    FDisposeMethod    : TDisposeMethod;

  protected
    // IDisposable interface
    procedure Forget;
    procedure Dispose;

  public
    constructor Create(const Data: Pointer;
      const DisposeProcedure: TDisposeProcedure;
      const DisposeMethod: TDisposeMethod);
    destructor Destroy; override;
  end;

function AllocateSafeMem(const Amount: Integer;
  const UseTempFile: Boolean): ISafeMem;
begin
  if UseTempFile then
    Result := TTempFileSafeMem.Create(Amount)
  else
    Result := TSafeMem.Create(Amount);
end;

function NewDisposable(const Data: Pointer;
  const Dispose: TDisposeProcedure): IDisposable; overload;
begin
  Result := TDisposable.Create(Data, Dispose, nil);
end;

function NewDisposable(const Data: Pointer;
  const Dispose: TDisposeMethod): IDisposable; overload;
begin
  Result := TDisposable.Create(Data, nil, Dispose);
end;

{ TBaseSafeMem }

function TBaseSafeMem.Clone(const CopyContents: Boolean): ISafeMem;
begin
  Result := nil;
end;

constructor TBaseSafeMem.Create(const Stream: TStream);
begin
  inherited Create;

  Assert(Assigned(Stream));

  FStream := Stream;
end;

destructor TBaseSafeMem.Destroy;
begin
  FreeAndNil(FStream);

  inherited;
end;

procedure TBaseSafeMem.Fill(const b: Byte);
begin
  if GetSize > 0 then
    FillChar(GetPointer^, GetSize, b);
end;

function TBaseSafeMem.GetAsString: string;
begin
  SetLength(Result, GetSize);
  if GetSize > 0 then
    Move(GetPointer^, Result[1], GetSize);
end;

function TBaseSafeMem.GetPointer: Pointer;
begin
  Result := nil;
end;

function TBaseSafeMem.GetSize: Integer;
begin
  Result := FStream.Size;
end;

function TBaseSafeMem.GetStream: TStream;
begin
  Result := FStream;
end;

function TBaseSafeMem.Grab(const Stream: TStream;
  const AdjustSize: Boolean): Integer;
var
  Grabbed : Integer;
begin
  Assert(Assigned(Stream));

  if GetSize > 0 then
  begin
    Grabbed := Stream.Read(GetPointer^, GetSize);

    if AdjustSize then
      SetSize(Grabbed);

    Result := Grabbed;
  end else
    Result := 0;
end;

procedure TBaseSafeMem.Grow(const By: Integer);
begin
  SetSize(GetSize + By);
end;

procedure TBaseSafeMem.LoadFromFile(const FileName: string);
var
  FileStream  : TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FStream.Size := 0;
    FStream.CopyFrom(FileStream, 0);
    FStream.Position := 0;
  finally
    FileStream.Free;
  end;
end;

procedure TBaseSafeMem.Release;
begin
  SetSize(0);
end;

procedure TBaseSafeMem.SaveToFile(const FileName: string);
var
  FileStream  : TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.CopyFrom(FStream, 0);
    FStream.Position := 0;
  finally
    FileStream.Free;
  end;
end;

procedure TBaseSafeMem.SetAsString(const NewValue: string);
begin
  SetSize(Length(NewValue));
  if NewValue <> '' then
    Move(NewValue[1], GetPointer^, Length(NewValue));
end;

procedure TBaseSafeMem.SetSize(const NewSize: Integer);
begin
  FStream.Size := NewSize;
end;

procedure TBaseSafeMem.Shrink(const By: Integer);
begin
  Grow(-By);
end;

procedure TBaseSafeMem.Zero;
begin
  Fill(0);
end;

{ TSafeMem }

function TSafeMem.Clone(const CopyContents: Boolean): ISafeMem;
begin
  Result := TSafeMem.Create(GetSize);
  if CopyContents and (GetSize > 0) then
    Move(GetPointer^, Result.Pointer^, GetSize);
end;

constructor TSafeMem.Create(const Amount: Integer);
var
  Stream  : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Assert(Amount >= 0, 'Amount is negative: ' + IntToStr(Amount));
    Assert(Amount < 100*1024*1024, 'Amount is too large: ' + IntToStr(Amount));
    Stream.Size := Amount;
    inherited Create(Stream);
  except
    Stream.Free;
    raise;
  end;
end;

function TSafeMem.GetPointer: Pointer;
begin
  Result := TMemoryStream(GetStream).Memory;
end;

{ TSwapFileSafeMem }

function TTempFileSafeMem.Clone(const CopyContents: Boolean): ISafeMem;
begin
  Result := TTempFileSafeMem.Create(GetSize);
  if CopyContents and (GetSize > 0) then
    Move(GetPointer^, Result.Pointer^, GetSize);
end;

procedure TTempFileSafeMem.CloseMemoryMap;
begin
  if Assigned(FMemoryMap) and (FMemoryMap <> @FDummy) then
    if not UnmapViewOfFile(FMemoryMap) then
      RaiseLastWin32Error;
  FMemoryMap := @FDummy;

  if FMappingHandle <> INVALID_HANDLE_VALUE then
    if not CloseHandle(FMappingHandle) then
      RaiseLastWin32Error;
  FMappingHandle := INVALID_HANDLE_VALUE;

  FMemoryMapSize := 0;
end;

constructor TTempFileSafeMem.Create(const Amount: Integer);
var
  TempPath  : array[0..MAX_PATH] of Char;
  TempName  : array[0..MAX_PATH] of Char;
  Stream    : TFileStream;
begin
  if GetTempPath(SizeOf(TempPath), TempPath) = 0 then
    RaiseLastWin32Error;

  if GetTempFileName(TempPath, nil, 0, TempName) = 0 then
    RaiseLastWin32Error;

  FMappingHandle := INVALID_HANDLE_VALUE;
  FMemoryMap := @FDummy;
  FMemoryMapSize := 0;

  FFileName := TempName;
  Stream := TFileStream.Create(FFileName, fmCreate);
  try
    inherited Create(Stream);
    SetSize(Amount);
  except
    Stream.Free;
    raise;
  end;
end;

destructor TTempFileSafeMem.Destroy;
begin
  CloseMemoryMap;
  inherited;

  if not DeleteFile(FFileName) then
    RaiseLastWin32Error;
end;

function TTempFileSafeMem.GetPointer: Pointer;
begin
  if FMemoryMapSize <> GetSize then
  begin
    CloseMemoryMap;
    OpenMemoryMap;
  end;

  Result := FMemoryMap;
end;

procedure TTempFileSafeMem.OpenMemoryMap;
var
  SizeRec : Int64Rec;
begin
  Int64(SizeRec) := GetStream.Size;

  FMappingHandle := CreateFileMapping(TFileStream(GetStream).Handle, nil,
    PAGE_READWRITE, SizeRec.Hi, SizeRec.Lo, nil);
  if FMappingHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;

  FMemoryMap := MapViewOfFile(FMappingHandle, FILE_MAP_WRITE, 0, 0, SizeRec.Lo);
  if not Assigned(FMemoryMap) then
    RaiseLastWin32Error;
end;

{ TDisposable }

constructor TDisposable.Create(const Data: Pointer;
  const DisposeProcedure: TDisposeProcedure;
  const DisposeMethod: TDisposeMethod);
begin
  inherited Create;

  FData := Data;
  FDisposeProcedure := DisposeProcedure;
  FDisposeMethod := DisposeMethod;
end;

destructor TDisposable.Destroy;
begin
  Dispose;

  inherited;
end;

procedure TDisposable.Dispose;
begin
  if Assigned(FDisposeMethod) then
    FDisposeMethod(Self, FData);
  if Assigned(FDisposeProcedure) then
    FDisposeProcedure(FData);

  Forget;
end;

procedure TDisposable.Forget;
begin
  FData := nil;
  FDisposeMethod := nil;
  FDisposeProcedure := nil;
end;

end.
