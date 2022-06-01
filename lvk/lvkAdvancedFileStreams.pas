{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains classes for NTFS File streams and memory mapped
    files.
}
unit lvkAdvancedFileStreams;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 17.04.03 16:18 $
// $Archive: /Components/LVK/Source/lvkAdvancedFileStreams.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{$IFNDEF WIN32}
These streams are only usable in Windows
{$ENDIF}

uses
  Windows, SysUtils, Classes, lvkVersion;

type
{ Description:
    This enumerated type is used by TNTFSFileStream to specify what kind of
    options to use when creating a new filestream.

    The values are:
      * fsoCompressed - The file is created with the Compressed attribute set.
          This option is only used when a new file is created, not when it is
          opened.
      * fsoSparse - The file is created with the Sparse attribute set. Only
          regions that contain non-zero data will commit space from the disk.
          This option is only used when a new file is created, not when it is
          opened.
      * fsoEncrypted - The file is created with the Encrypted attribute set.
          The file will only be readable by the user that originally created
          the file.
          This option is only used when a new file is created, not when it is
          opened.
          This option can not be used together with compressed files, and if
          you specify both, the file will be encrypted only.
      * fsoTemporary - The file is created with the Temporary flag. The system
          will try to keep the file in memory for faster access.
      * fsoWriteThrough - No write-behind buffering is performed when this
          option is used. Use this for files that contain data that it is
          critical that are saved to disk at once.
      * fsoNoBuffering - No buffering is done for the file.
      * fsoRandomAccess - Hint to the operating system that it should
          optimize buffers for random access.
      * fsoSequentialScan - Hint to the operating system that it should
          optimize buffers for sequential scan through the file.
      * fsoDeleteOnClose - The file will be deleted when it is closed.
  See also:
    TNTFSFileStreamOptions, TNTFSFileStream
}
  TNTFSFileStreamOption = (fsoCompressed, fsoSparse, fsoTemporary, fsoEncrypted,
    fsoWriteThrough, fsoNoBuffering, fsoRandomAccess, fsoSequentialScan,
    fsoDeleteOnClose);

{ Description:
    This is simply a set of TNTFSFileStreamOption values.
  See also:
    TNTFSFileStreamOption, TNTFSFileStream
}
  TNTFSFileStreamOptions = set of TNTFSFileStreamOption;

{ Description:
    This is a new filestream class that implements new options usable with the
    NTFS file system on Windows NT and 2000.
}
  TNTFSFileStream = class(THandleStream)
  private
    procedure SetCompression(const Handle: THandle; const Enabled: Boolean);
    procedure SetSparse(const Handle: THandle);

    function CreateFile(const Filename: string; const Mode: Word; const Options: TNTFSFileStreamOptions = []): THandle;

    function GetCompressed: Boolean;
    procedure SetCompressed(const Value: Boolean);
    function GetPackageVersion: TPackageVersion;

  public
    { Description:
        This constructor creates and initializes the TNTFSFileStream object.
      Parameters:
        Filename - Name and path of the file to create or open.
        Mode - Contains file open mode and sharing attributes. See remarks for
          more information.
        Options - Set containing options to use for opening or creating the
          file.
      Remarks:
        The Mode parameter can contain a single open mode value OR'ed together
        with a single share mode value.

        Open modes are:
          * fmOpenRead - Opens an existing file in read-only mode. The file must
              already exist.
          * fmOpenReadWrite - Opens an existing file in read/write mode. The
              file must already exist.
          * fmCreate - Creates a new file. If the file already exists, it will
              be deleted prior to creation.

        Share modes are:
          * fmShareDenyNone - Other processes/threads can open the file
              simultaneously in both read and/or write modes.
          * fmShareDenyRead - Other processes/threads can open the file
              in write mode only.
          * fmShareDenyWrite - Other processes/threads can open the file
              in read mode only.
          * fmShareExclusive - Other processes/threads have no access to the
              file.
      See also:
        Destroy
    }
    constructor Create(const Filename: string; const Mode: Word; const Options: TNTFSFileStreamOptions = []);

    { Description:
        This destructor will destroy the class and close the file.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        This method can be used to zero portions of the file. If you use
        sparse and/or compressed files, you should use this method to
        clear any unused portions of the file once they become unused.
      Parameters:
        FileOffset - The starting offset of the portion to clear.
        Length - The length of the portion to clear. If this parameter is 0,
          the portion will extend from the given starting position to the end
          of the file.
    }
    procedure ZeroData(const FileOffset: Int64=0; const Length: Int64=0);

    { Description:
        Use this property to check if the file is compressed or not, or to
        enable or disable compression on the file.
      See also:
        Recompress, Decompress, Compress
    }
    property Compressed: Boolean read GetCompressed write SetCompressed;

    { Description:
        This method will decompres and then compress the file, if the file
        is in a compressed state to begin with. Use this after doing a lot
        of random access writing to the file to optimize the compression.
      See also:
        Compressed, Decompress, Compress
    }
    procedure Recompress;

    { Description:
        Use this method to decompress the file. It has the same effect as
        setting the Compressed property to False.
      See also:
        Compressed, Recompress, Compress
    }
    procedure Decompress;

    { Description:
        Use this method to compress the file. It has the same effect as
        setting the Compressed property to True.
      See also:
        Compressed, Recompress, Decompress
    }
    procedure Compress;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;

{ Description:
    This class implements a memory mapped file based on top of a THandleStream.
    You can use this to obtain a pointer to the contents of a file as though
    it were completely read into memory.
}
  TMemoryMappedFileStream = class(TCustomMemoryStream)
  private
    FHandleStream : THandleStream;
    FFileOffset   : Int64;
    FLength       : Cardinal;
    FMode         : Word;

    FFileMapping  : THandle;
    FMemoryPtr    : Pointer;

    procedure CreateFileMapping;
    procedure MapViewOfFile;
    procedure UnmapViewOfFile;
    procedure CloseFileMapping;
    function GetPackageVersion: TPackageVersion;

  public
    { Description:
        This constructor creates the memory mappes file class
      Parameters:
        HandleStream - The stream to map a view against. Must be a THandleStream
          that represents a file. Use TFileStream or TNTFSFileStream or
          similar classes for this parameter.
        Mode - Pass the same value you used to open the filestream with to this
          parameter.
          Can be one of the following values:
            * fmOpenRead - The memory will be read-only.
            * fmOpenReadWrite - The memory will be read/write.
            * fmCreate - The memory will be read7write.
        FileOffset - The starting position in the file to map into memory.
          Position 0 in this memory mapped file stream will match the given
          position in the underlying file.
        Length - How many bytes to map into memory, starting at the FileOffset
          specified above.
      See also:
        Destroy
    }
    constructor Create(const HandleStream: THandleStream; const Mode: Word; const FileOffset: Int64=0; Length: Cardinal=0);

    { Description:
        Closes the memory mapped file and unmaps the memory.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        Writes data to the stream.
    }
    function Write(const Buffer; Count: Longint): Longint; override;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;

implementation

type
  TEncryptFile = function(lpFilename: PChar): BOOL; stdcall;

var
  EncryptFile : TEncryptFile;

const
  FILE_ATTRIBUTE_SPARSE_FILE  = $80;
  FSCTL_GET_COMPRESSION       = $9003C;
  FSCTL_SET_COMPRESSION       = $9C040;
  FSCTL_SET_ZERO_DATA         = $980C8;
  FSCTL_SET_SPARSE            = $900C4;

  COMPRESSION_FORMAT_NONE     = 0;
  COMPRESSION_FORMAT_DEFAULT  = 1;

{ TNTFSFileStream }

procedure TNTFSFileStream.Compress;
begin
  Compressed := True;
end;

constructor TNTFSFileStream.Create(const Filename: string;
  const Mode: Word; const Options: TNTFSFileStreamOptions);
begin
  inherited Create(CreateFile(Filename, Mode, Options));
end;

function TNTFSFileStream.CreateFile(const Filename: string;
  const Mode: Word; const Options: TNTFSFileStreamOptions): THandle;
var
  DesiredAccess       : Cardinal;
  ShareMode           : Cardinal;
  CreationDisposition : Cardinal;
  FlagsAndAttributes  : Cardinal;
  Option              : TNTFSFileStreamOption;
begin
  DesiredAccess := 0;
  ShareMode := 0;
  if Mode = fmCreate then
  begin
    if FileExists(Filename) then
      if not DeleteFile(Filename) then
        RaiseLastWin32Error;

    DesiredAccess := GENERIC_READ or GENERIC_WRITE;
    ShareMode := 0;
    CreationDisposition := CREATE_ALWAYS;

    FlagsAndAttributes := FILE_ATTRIBUTE_NORMAL;
    for Option := Low(TNTFSFileStreamOption) to High(TNTFSFileStreamOption) do
      if Option in Options then
        case Option of
          fsoTemporary:
            FlagsAndAttributes := FlagsAndAttributes or FILE_ATTRIBUTE_TEMPORARY;

          fsoWriteThrough:
            FlagsAndAttributes := FlagsAndAttributes or FILE_FLAG_WRITE_THROUGH;

          fsoNoBuffering:
            FlagsAndAttributes := FlagsAndAttributes or FILE_FLAG_NO_BUFFERING;

          fsoRandomAccess:
            FlagsAndAttributes := FlagsAndAttributes or FILE_FLAG_RANDOM_ACCESS;

          fsoSequentialScan:
            FlagsAndAttributes := FlagsAndAttributes or FILE_FLAG_SEQUENTIAL_SCAN;

          fsoDeleteOnClose:
            FlagsAndAttributes := FlagsAndAttributes or FILE_FLAG_DELETE_ON_CLOSE;
        end;
  end else begin
    FlagsAndAttributes := FILE_ATTRIBUTE_NORMAL;

    case Mode and 3 of
      fmOpenRead:
        DesiredAccess := GENERIC_READ;

      fmOpenWrite:
        DesiredAccess := GENERIC_WRITE;

      fmOpenReadWrite:
        DesiredAccess := GENERIC_READ or GENERIC_WRITE;

      fmOpenReadWrite or fmOpenWrite:
        DesiredAccess := GENERIC_READ or GENERIC_WRITE;
    end;

    case Mode and $F0 of
      fmShareCompat, fmShareExclusive:
        DesiredAccess := DesiredAccess or 0;

      fmShareDenyWrite:
        DesiredAccess := DesiredAccess or FILE_SHARE_READ;

      fmShareDenyRead:
        DesiredAccess := DesiredAccess or FILE_SHARE_WRITE;

      fmShareDenyNone:
        DesiredAccess := DesiredAccess or (FILE_SHARE_READ or FILE_SHARE_WRITE);
    end;
    CreationDisposition := OPEN_EXISTING;
  end;

  Result := Windows.CreateFile(PChar(Filename), DesiredAccess, ShareMode, nil, CreationDisposition, FlagsAndAttributes, 0);
  if Result = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;

  try
    if Mode = fmCreate then
    begin
      if fsoEncrypted in Options then
      begin
        CloseHandle(Result);
        if not Assigned(EncryptFile) then
          raise Exception.Create('Can not encrypt files on a non-NT platform');
        if not EncryptFile(PChar(Filename)) then
          RaiseLastWin32Error;

        Result := CreateFile(Filename, fmOpenReadWrite or fmShareExclusive, Options);
      end else
        SetCompression(Result, fsoCompressed in Options);

      if fsoSparse in Options then
        SetSparse(Result);
    end;
  except
    CloseHandle(Result);
    raise;
  end;
end;

procedure TNTFSFileStream.Decompress;
begin
  Compressed := False;
end;

destructor TNTFSFileStream.Destroy;
begin
  if Handle <> 0 then
    CloseHandle(Handle);

  inherited;
end;

function TNTFSFileStream.GetCompressed: Boolean;
var
  CompressionAlgorithm  : Word;
  Dummy                 : Cardinal;
begin
  if not DeviceIoControl(Handle, FSCTL_GET_COMPRESSION, nil, 0,
    @CompressionAlgorithm, SizeOf(CompressionAlgorithm), Dummy, nil) then
    RaiseLastWin32Error;

  Result := (CompressionAlgorithm <> COMPRESSION_FORMAT_NONE);
end;

function TNTFSFileStream.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TNTFSFileStream.Recompress;
begin
  if Compressed then
  begin
    Decompress;
    Compress;
  end;
end;

procedure TNTFSFileStream.SetCompressed(const Value: Boolean);
begin
  if Value <> GetCompressed then
    SetCompression(Handle, Value);
end;

procedure TNTFSFileStream.SetCompression(const Handle: THandle;
  const Enabled: Boolean);
var
  Dummy                 : Cardinal;
  CompressionAlgorithm  : Word;
const
  CompressionAlgorithms : array[Boolean] of Word = (
    COMPRESSION_FORMAT_NONE,
    COMPRESSION_FORMAT_DEFAULT
  );
begin
  CompressionAlgorithm := CompressionAlgorithms[Enabled];
  if not DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @CompressionAlgorithm,
    SizeOf(CompressionAlgorithm), nil, 0, Dummy, nil) then
    RaiseLastWin32Error;
end;

procedure TNTFSFileStream.SetSparse(const Handle: THandle);
var
  Dummy : Cardinal;
begin
  if not DeviceIoControl(Handle, FSCTL_SET_SPARSE, nil, 0, nil, 0, Dummy, nil) then
    RaiseLastWin32Error;
end;

procedure TNTFSFileStream.ZeroData(const FileOffset, Length: Int64);
var
  FileZeroDataInformation : packed record
    FileOffset        : Int64;
    BeyondFinalOffset : Int64;
  end;
  Dummy                   : Cardinal;
begin
  FileZeroDataInformation.FileOffset := FileOffset;
  if Length = 0 then
    FileZeroDataInformation.FileOffset := Size - FileOffset
  else
    FileZeroDataInformation.FileOffset := Length;
  if not DeviceIoControl(Handle, FSCTL_SET_ZERO_DATA, @FileZeroDataInformation,
    SizeOf(FileZeroDataInformation), nil, 0, Dummy, nil) then
    RaiseLastWin32Error;
end;

{ TMemoryMappedFileStream }

procedure TMemoryMappedFileStream.CloseFileMapping;
begin
  if FFileMapping <> 0 then
    if not CloseHandle(FFileMapping) then
      RaiseLastWin32Error;
end;

constructor TMemoryMappedFileStream.Create(const HandleStream: THandleStream;
  const Mode: Word; const FileOffset: Int64; Length: Cardinal);
begin
  Assert(Assigned(HandleStream));
  Assert(FileOffset >= 0);
  Assert(Length > 0);
  Assert(FileOffset + Length >= HandleStream.Size);
  
  inherited Create;

  FHandleStream := HandleStream;
  FFileOffset := FileOffset;
  FLength := Length;
  FMode := Mode;

  FFileMapping := 0;
  FMemoryPtr := nil;

  CreateFileMapping;
  MapViewOfFile;

  if FLength = 0 then
    FLength := HandleStream.Size;

  SetPointer(FMemoryPtr, FLength);
end;

procedure TMemoryMappedFileStream.CreateFileMapping;
const
  PageAccess  : array[0..3] of Cardinal = (
    PAGE_READONLY,
    PAGE_READWRITE,
    PAGE_READWRITE,
    PAGE_READWRITE
  );
begin
  FFileMapping := Windows.CreateFileMapping(FHandleStream.Handle, nil,
    PageAccess[FMode and 3], 0, 0, nil);
  if FFileMapping = 0 then
    RaiseLastWin32Error;
end;

destructor TMemoryMappedFileStream.Destroy;
begin
  UnmapViewOfFile;
  CloseFileMapping;

  inherited;
end;

function TMemoryMappedFileStream.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

// Work-around for C3517 internal compiler error
{$IFOPT O+}
{$DEFINE OPT_ON}
{$O-}
{$ENDIF}
procedure TMemoryMappedFileStream.MapViewOfFile;
const
  MapAccess : array[0..3] of Cardinal = (
    FILE_MAP_READ,
    FILE_MAP_WRITE,
    FILE_MAP_ALL_ACCESS,
    FILE_MAP_ALL_ACCESS
  );
begin
  FMemoryPtr := Windows.MapViewOfFile(FFileMapping, MapAccess[FMode and 3],
    Int64Rec(FFileOffset).Hi, Int64Rec(FFileOffset).Lo, FLength);
  if not Assigned(FMemoryPtr) then
    RaiseLastWin32Error;
end;
{$IFDEF OPT_ON}
{$O+}
{$UNDEF OPT_ON}
{$ENDIF}

procedure TMemoryMappedFileStream.UnmapViewOfFile;
begin
  if Assigned(FMemoryPtr) then
    if not Windows.UnmapViewOfFile(FMemoryPtr) then
      RaiseLastWin32Error;
end;

function TMemoryMappedFileStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos: Longint;
begin
  Result := 0;

  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;
    if Cardinal(Pos) > FLength then
    begin
      Count := FLength - Cardinal(Position);
      Pos := Position + Count;
    end;
    if Pos > 0 then
    begin
      System.Move(Buffer, Pointer(Longint(FMemoryPtr) + Position)^, Count);
      Position := Pos;
      Result := Count;
    end;
  end;
end;

var
  ADVAPI32Handle  : THandle;

initialization
  EncryptFile := nil;

  ADVAPI32Handle := LoadLibrary('advapi32.dll');
  if ADVAPI32Handle <> 0 then
    EncryptFile := GetProcAddress(ADVAPI32Handle, 'EncryptFileA');
finalization
  FreeLibrary(ADVAPI32Handle);
  EncryptFile := nil;
end.
