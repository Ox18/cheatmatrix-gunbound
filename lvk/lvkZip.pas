{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{* Thanks to the following people to spot bugs, mistakes and flaws:           *}
{*   J.Z. by email, spotted External/Internal attributes switched around      *}
{*                  as well as bringing up the question about directory       *}
{*                  entries                                                   *}
{*   A.E. by email, spotted a off-by-one bug                                  *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains classes and interfaces related to opening and creating
    .zip files.
}
unit lvkZip;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkZip.pas $

{ TODO 2 -oLVK -cSource : Determine if I need to add 0-sized Directory entries
  for sub-directories used in the zip file. Documentation not clear, will look
  for references on the web for this before deciding. }
interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFNDEF DELPHI6UP}
  FileCtrl,
  {$ENDIF}
  lvkTypes, lvkCRC, lvkSafeMem, SysUtils, Classes;

type
  TZipFileSize = UInt32;
  TZipFileInternalAttributes = UInt16;
  TZipFileExternalAttributes = UInt32;

  { Description:
      These options are used when opening and using the zip file. For now there
      is only one option available, zfoScanWholeFile. If you use this option,
      the zip file object will scan the whole file for the zip file. If you
      don't use it, it will assume the end of the file is the end of the zip
      file contents. Scanning the whole file will take more time, but then
      you can attach more data after the zip file.
    See also:
      OpenZipFile
  }
  TZipFileOption  = (zfoScanWholeFile);
  // <COMBINE TZipFileOption>
  TZipFileOptions = set of TZipFileOption;

  IZipFile = interface;

  { Description:
      This type contains the compression methods used in a zip file. Note that
      these classes only knows how to handle zfcmStored and zfcmDeflated.
    See also:
      IZipFileEntry.CompressionMethod
  }
  TZipFileCompressionMethod = (zfcmStored, zfcmShrunk, zfcmReduced1,
    zfcmReduced2, zfcmReduced3, zfcmReduced4, zfcmImploded, zfcmTokenized,
    zfcmDeflated, zfcmEnhancedDeflated, zfcmPKWARELibraryImploded,
    zfcmUnknown);

  { Description:
      This type contains the operating systems that can create zip files. You
      can use this to interpret line endings or the extra data attached to
      an entry in a zip file.
    See also:
      IZipFileEntry.MadeBy
  }
  TZipFileMadeBy = (zfmbMSDOS_OS2_FAT, zfmbAmiga, zfmbVAX_VMS, zfmbUnix,
    zfmbVM_CMS, zfmbAtariST, zfmbOS2HPFS, zfmbMacintosh, zfmbZSystem,
    zfmbCPM, zfmbWindowsNT_NTFS, zfmbUnknown);

  { Description:
      This type contains the available open modes that
      IZipFileEntry.OpenAsStream@TStream@TZipFileOpenMode@Boolean
      employs when you open an entry as a stream.
    See also:
      IZipFileEntry.OpenAsStream@TStream@TZipFileOpenMode@Boolean
  }
  TZipFileOpenMode = (zomRead, zomReadWrite, zomWrite);

  { Description:
      This interface wraps an object responsible for a single entry in the
      zip file. Normally all entries will be files, but there can also be
      directories. These can be considered files without content.
    See also:
      IZipFile
  }
  IZipFileEntry = interface
    ['{B8B3F228-96E8-4E1A-9994-B0BA50033FEB}']

    // <COMBINE Name>
    function GetName: string;
    // <COMBINE Name>
    procedure SetName(const NewName: string);
    { Description:
        This property controls the name of the entry in the zip file. You can
        have nameless entries.
    }
    property Name: string read GetName write SetName;

    // <COMBINE Comment>
    function GetComment: string;
    // <COMBINE Comment>
    procedure SetComment(const NewComment: string);
    { Description:
        This property contains the comment attached to the entry in the zip
        file. A comment can be up to 64kb in size. Comments are not compressed
        so they will add directly to the size of the file.
    }
    property Comment: string read GetComment write SetComment;

    // <COMBINE DateTime>
    function GetDateTime: TDateTime;
    // <COMBINE DateTime>
    procedure SetDateTime(const NewDateTime: TDateTime);
    { Description:
        This property contains the datetime stamp associated with the entry in
        the zip file.
    }
    property DateTime: TDateTime read GetDateTime write SetDateTime;

    { Description:
        This method reverts the entry back to its pristine state. For an entry
        that was loaded from an existing entry in an existing zip file, this
        means reloading the information. For a new entry, it means reverting to
        an empty entry.
    }
    procedure Revert;

    // <COMBINE ZipFile>
    function GetZipFile: IZipFile;
    { Description:
        Each entry belongs in a zipfile. This will return the zipfile this
        entry is associated with. If it returns nil, it means you have destroyed
        the zipfile object instance and still holding on to a entry instance.
    }
    property ZipFile: IZipFile read GetZipFile;

    // <COMBINE UncompressedSize>
    function GetUncompressedSize: TZipFileSize;
    { Description:
        This returns the size of the data in their uncompressed, original,
        format.
      See also:
        CompressedSize
    }
    property UncompressedSize: TZipFileSize read GetUncompressedSize;

    // <COMBINE CompressedSize>
    function GetCompressedSize: TZipFileSize;
    { Description:
        This returns the size of the data in their compressed format.
      See also:
        UncompressedSize
    }
    property CompressedSize: TZipFileSize read GetCompressedSize;

    // <COMBINE CRC32>
    function GetCRC32: TCRC32;
    { Description:
        This returns the CRC32 checksum of the data.
    }
    property CRC32: TCRC32 read GetCRC32;

    // <COMBINE MadeBy
    function GetMadeBy: TZipFileMadeBy;
    { Description:
        This returns a value corresponding to the type of operating system
        that made this file. This could be useful if you want to interpret
        line endings in ascii files, or the data in the optional extra field.
    }
    property MadeBy: TZipFileMadeBy read GetMadeBy;

    // <COMBINE ExtraField>
    function GetExtraField: ISafeMem;
    // <COMBINE ExtraField>
    procedure SetExtraField(const NewField: ISafeMem);
    { Description:
        This holds extra data stored with the entry in the zip file. If it
        returns nil, no extra data was stored. You can give the entry a new
        safemem object with new extra data to store, or you can set it to
        nil to remove the extra data associated with it.
    }
    property ExtraField: ISafeMem read GetExtraField write SetExtraField;

    // <COMBINE CompressionMethod>
    function GetCompressionMethod: TZipFileCompressionMethod;
    { Description:
        This returns the compression method used to compress this entry. Note
        that these classes only knows how to handle entries stored uncompressed
        or stored in the deflated compression format.
    }
    property CompressionMethod: TZipFileCompressionMethod
      read GetCompressionMethod;

    // <COMBINE InternalAttributes>
    function GetInternalAttributes: TZipFileInternalAttributes;
    // <COMBINE InternalAttributes>
    procedure SetInternalAttributes(const NewAttributes: TZipFileInternalAttributes);
    { Description:
        This holds the internal file attributes associated with the entry.
    }
    property InternalAttributes: TZipFileInternalAttributes
      read GetInternalAttributes write SetInternalAttributes;

    // <COMBINE ExternalAttributes>
    function GetExternalAttributes: TZipFileExternalAttributes;
    // <COMBINE ExternalAttributes>
    procedure SetExternalAttributes(const NewAttributes: TZipFileExternalAttributes);
    { Description:
        This holds the external, operating system, file attributes
        associated with the entry. They are stored in MS-DOS format.
    }
    property ExternalAttributes: TZipFileExternalAttributes
      read GetExternalAttributes write SetExternalAttributes;

    { Description:
        Decompresses the contents of the entry to the specified stream.
      Parameters:
        Stream - The stream object to extract the contents to.
      See also:
        ExtractToFile, ExtractToDirectory, SaveToFile, SaveToStream,
        OpenAsStream
    }
    procedure ExtractToStream(const Stream: TStream);

    { Description:
        Decompresses the contents of the entry to the specified file.
      Parameters:
        FileName - The name of the file to decompress the contents to.
        ForceOverwrite - If the file exists, and is read-only, you can use True
          to delete the file before extracting to it.
      See also:
        ExtractToStream, ExtractToDirectory, SaveToFile, SaveToStream,
        OpenAsStream
    }
    procedure ExtractToFile(const FileName: string;
      const ForceOverwrite: Boolean=False);

    { Description:
        Decompresses the contents of the entry to a file stored beneath the
        specified base directory.
      Parameters:
        BaseDirectory - The base directory to prepend to the name of the
          entry.
        ForceOverwrite - If the file exists, and is read-only, you can use True
          to delete the file before extracting to it.
      See also:
        ExtractToStream, ExtractToFile, SaveToFile, SaveToStream,
        OpenAsStream
    }
    procedure ExtractToDirectory(const BaseDirectory: string;
      const ForceOverwrite: Boolean=False);

    { Description:
        This method opens up the contents of the entry as a stream. This stream
        will be returned through the output parameter Stream. The caller of
        this method is responsible for freeing this object.

        The ways you can open the stream are:
          * zomRead: Read only, sequential reading. You can't seek nor write
              on this stream.
          * zomReadWrite: Read and write, random access.
          * zomWrite: Write-only, sequential writing. You can't seek nor read
              on this stream.
      Parameters:
        Stream - A new stream object will be returned through this parameter.
        OpenMode - What mode to use for opening the contents with.
        Compress - If this is True, the contents will be compressed, otherwise
          they will simply be stored uncompressed.
    }
    procedure OpenAsStream(out Stream: TStream;
      const OpenMode: TZipFileOpenMode=zomRead;
      const Compress: Boolean=True);

    { Description:
        This method deletes the entry from the given zip file.
    }
    procedure Delete;

    // <COMBINE IsEncrypted>
    function GetIsEncrypted: Boolean;
    { Description:
        This returns True if the entry is stored in encrypted form. Note that
        these classes currently can't handle encrypted entries.
    }
    property IsEncrypted: Boolean read GetIsEncrypted;

    { Description:
        This method will load new contents from the object or file specified.
      Parameters:
        FileName - The name of the file to load the new contents from.
        Stream - The stream object to load the new contents from.
        SafeMem - The SafeMem object to load the new contents from.
        Data - A pointer to the data to store as the new contents.
        Size - The size of the data stored where the pointer points.
      See also:
        LoadFromFile, LoadFromStream
    }
    procedure ReplaceContents(const FileName: string); overload;
    // <COMBINE ReplaceContents@string>
    procedure ReplaceContents(const Stream: TStream); overload;
    // <COMBINE ReplaceContents@string>
    procedure ReplaceContents(const SafeMem: ISafeMem); overload;
    // <COMBINE ReplaceContents@string>
    procedure ReplaceContents(const Data: Pointer; const Size: Integer); overload;

    { Description:
        This method will recompress the contents. Useful if you are optimizing
        an existing zip file which has used inefficient configuration options
        during compression.
    }
    procedure Recompress;

    { Description:
        If the zipe file is using delayed compression, you can ask an entry
        to compress itself by calling this method.
    }
    procedure CompressNow;

    { Description:
        This method will load new contents from the file specified.
      Parameters:
        FileName - The name of the file to load the new contents from.
      See also:
        ReplaceContents@string
    }
    procedure LoadFromFile(const FileName: string);

    { Description:
        This method will load new contents from the stream specified.
      Parameters:
        Stream - The stream object to load the new contents from.
      See also:
        ReplaceContents@string
    }
    procedure LoadFromStream(const Stream: TStream);

    { Description:
        Decompresses the contents of the entry to the specified file.
      Parameters:
        FileName - The name of the file to decompress the contents to.
      See also:
        ExtractToStream, ExtractToDirectory, ExtractToFile, SaveToStream,
        OpenAsStream
    }
    procedure SaveToFile(const FileName: string);

    { Description:
        Decompresses the contents of the entry to the specified stream.
      Parameters:
        Stream - The stream object to extract the contents to.
      See also:
        ExtractToStream, ExtractToFile, ExtractToDirectory, SaveToStream,
        OpenAsStream
    }
    procedure SaveToStream(const Stream: TStream);
  end;

  { Description:
      This interface wraps an object that corresponds directly to a zip file
      on disk.

      Note: any changes you perform on the zip file object will not be
        saved to disk before the program explicitly calls the Save method, so
        you can add new elements, change existing ones, change comments, names,
        etc. and if you simply destroy the object, the original zip file will
        be left alone.

      Note: This class does not handle spanned zip files, nor the new Deflate64
        algorithm, nor any of the old compression methods (Shrink etc.),
        nor password-protected zip files. It's basic zip file support.
    See also:
      IZipFile.Save@string
  }
  IZipFile = interface
    ['{4F0CFB6B-A949-4A5E-B1CC-B5A18EB7299F}']

    // <COMBINE FileName>
    function GetFileName: string;
    { Description:
        This returns the current filename of the zip file. It will be blank
        for an unsaved, new, zip file.
    }
    property FileName: string read GetFileName;

    { Description:
        This method will save the zip file to disk.
      Parameters:
        NewFileName - Leave blank to use the current filename (will overwrite
          the existing zip file), or specify a new name for the zip file.
    }
    procedure Save(const NewFileName: string='');

    // <COMBINE Count>
    function GetCount: Integer;
    { Description:
        This returns the number of entries available in the zip file.
      See also:
        Entries
    }
    property Count: Integer read GetCount;

    // <COMBINE Entries>
    function GetEntries(const Index: Integer): IZipFileEntry;
    { Description:
        This gives access to the entries stored in the zip file. It's a
        zero-based "array" of zip file entries.
      See also:
        Count, IZipFileEntry
    }
    property Entries[const Index: Integer]: IZipFileEntry read GetEntries;
      default;

    { Description:
        This method will fill the specified TStrings object with the names of
        all the entries in the zip file, one entry per line/item.
      Parameters:
        Dest - The TStrings object to store the names in.
    }
    procedure GetEntryNames(const Dest: TStrings);

    // <COMBINE Comment>
    function GetComment: string;
    // <COMBINE Comment>
    procedure SetComment(const NewComment: string);
    { Description:
        This property holds the current comment attached to the zip file.
        Comments can be up to 64kb in size. Note that comments are not
        compressed and contribute directly to the size of the zip file.
    }
    property Comment: string read GetComment write SetComment;

    { Description:
        This overloaded method will add another entry to the zip file. The
        version with only a Name parameter will add an empty entry, and the
        other methods will add an entry with contents taken from the
        specified file or object.
      Parameters:
        Name - The name of the new entry.
        SourceFileName - The name of the file to load the contents of the new
          entry from.
        SourceStream - The stream object to load the contents of the new entry
          from.
        SourceSafeMem - The SafeMem object to load the conetns of the new
          entry from.
    }
    function Add(const Name: string): IZipFileEntry; overload;
    // <COMBINE Add@string>
    function Add(const Name: string;
      const SourceFileName: string): IZipFileEntry; overload;
    // <COMBINE Add@string>
    function Add(const Name: string;
      const SourceStream: TStream): IZipFileEntry; overload;
    // <COMBINE Add@string>
    function Add(const Name: string;
      const SourceSafeMem: ISafeMem): IZipFileEntry; overload;

    // <COMBINE DelayCompression>
    function GetDelayCompression: Boolean;
    // <COMBINE DelayCompression>
    procedure SetDelayCompression(const NewDelayCompression: Boolean);
    { Description:
        This controls when compression will take place for new and changed
        entries in the zip file. If you enable delayed compression (set this
        property to True), compression will be done when the zip file is
        saved to disk. If you disable it, compression will be done as soon
        as you've added a new entry from an existing object or file, or when
        you close a stream you've opened from an entry.
    }
    property DelayCompression: Boolean read GetDelayCompression
      write SetDelayCompression;

    { Description:
        This method returns True if there exists an entry in the zip file with
        the specified name. Note that it uses a case insensitive comparison
        so the filename TEST.TXT is counted as the same as test.txt.
      Parameters:
        Name - The name of the entry to check for.
      See also:
        EntryByName
    }
    function Exists(const Name: string): Boolean;

    // <COMBINE EntryByName>
    function GetEntryByName(const Name: string): IZipFileEntry;
    { Description:
        This returns the entry with the specified name, or nil if no entry with
        that name exists.
      Parameters:
        Name - The name of the entry to return.
      See also:
        Exists
    }
    property EntryByName[const Name: string]: IZipFileEntry
      read GetEntryByName;

    { Description:
        This overloaded method removes the entry from the zip file.
      Parameters:
        Name - The name of the entry to delete.
        Entry - The specified entry to delete.
      See also:
        -
    }
    procedure Delete(const Name: string); overload;
    // <COMBINE Delete@string>
    procedure Delete(const Entry: IZipFileEntry); overload;

    { Description:
        This method extracts all entries in the zip file to the specified
        base directory. It is equivalent to calling ExtractTo on each
        entry with ForceOverwrite to True.
      Parameters:
        BaseDirectory - The base directory to decompress and store all
          entries into.
    }
    procedure ExtractTo(const BaseDirectory: string);
  end;

  // Base exception class for all lvkZip related exceptions
  EZipFile = class(Exception);
  // This exception is raised if the zip file cannot be opened
  EZipFileOpen = class(EZipFile);
  // This exception is raised if the zip file entry/stream cannot be opened
  EZipFileOpenStream = class(EZipFile);
  // This exception is raised if a locked entry is tried locked again
  // (you can only keep a stream for an entry one at a time)
  EZipFileLock = class(EZipFile);
  // This exception is raised if the entry is already locked for reading
  EZipFileReadLock = class(EZipFileLock);
  // This exception is raised if the stream cannot be seeked in
  EZipFileSeek = class(EZipFile);
  // This exception is raised when a I/O operation failed
  EZipFileIO = clasS(EZipFile);
  // This exception is raised when the stream cannot be read from
  EZipFileRead = class(EZipFileIO);
  // This exception is raised when the stream cannot be written to
  EZipFileWrite = class(EZipFileIO);
  // This exception is raised when the zip file cannot be saved
  EZipFileSave = class(EzipFile);
  // This exception is raised when an entry cannot be extracted
  EZipFileExtract = class(EZipFileIO);
  // This exception is raised when there is a problem during zip file closing
  EZipFileClose = class(EZipFile);

{ Description:
    This function returns a new and empty zip file. Remember to save the
    zip file and give it a new name in the process.
  See also:
    OpenZipFile@string@TZipFileOptions
}
function NewZipFile: IZipFile;

{ Description:
    This function returns a zip file object corresponding to an existing zip
    file on disk.
  Parameters:
    FileName - The name of the zip file to open.
    Options - Options to use when opening the zip file.
  See also:
    NewZipFile
}
function OpenZipFile(const FileName: string;
  const Options: TZipFileOptions=[]): IZipFile;

resourcestring
  ERR_UNABLE_TO_LOCATE_CENTRAL_DIRECTORY  = 'Unable to open zip file %0:s, no central directory found';
  ERR_UNABLE_TO_OPEN_SPANNED_ZIP_FILES    = 'Unable to open zip file %0:s, zip file is spanned';
  ERR_INVALID_CENTRAL_FILE_HEADER         = 'Unable to open zip file %0:s, invalid central directory file header entry found';
  ERR_INVALID_LOCAL_FILE_HEADER           = 'Unable to open zip file %0:s, invalid local file header entry found';
  ERR_FILE_HEADERS_DIFFER                 = 'Unable to open zip file %0:s, central and local file headers differ';
  ERR_INVALID_COMPRESSION_METHOD          = 'Unable to open file %1:s in zip file %0:s, don''t know how to handle the compression method';
  ERR_ENTRY_ALREADY_LOCKED                = 'Entry in zip file %0:s already locked';
  ERR_ENTRY_NOT_LOCKED                    = 'Entry in zip file %0:s not locked';
  ERR_UNABLE_TO_HANDLE_ENCRYPTED_FILES    = 'Unable to extract file %0:s from %1:s, don''t know how to handle encrypted files';
  ERR_FILE_IS_READ_ONLY                   = 'Entry %0:s in zip file %1:s opened for reading only';
  ERR_CANT_SEEK_IN_ON_THE_FLY_STREAM      = 'Cannot seek in a stream which is compressed on-the-fly';
  ERR_CANT_SET_SIZE_OF_ON_THE_FLY_STREAM  = 'Cannot set size of a stream which is compressed on-the-fly';
  ERR_CANT_READ_FROM_ON_THE_FLY_STREAM    = 'Cannot read from a stream which is compressed on-the-fly';
  ERR_UNABLE_TO_CLOSE_LOCKS_PENDING       = 'Unable to close zip file %0:s with pending locks';
  ERR_NO_FILENAME                         = 'No filename given to zip file';
  ERR_ZIP_FILE_GONE                       = 'Zip file object instance destroyed, cannot call method %0:s now';

implementation

uses
  Math, Windows, ActiveX,
  lvkAdvancedFileStreams, lvkZLib, lvkZLibTypes, lvkChecksumUtils, lvkZLibUtils,
  lvkZLibConsts;

type
  PZipFileSignature = ^TZipFileSignature;
  TZipFileSignature = UInt32;

  TZipFileOffset = TZipFileSize;
  TZipStringLength = UInt16;
  TZipFileNumber = UInt16;
  TZipFileDiskNumber = TZipFileNumber;
  TZipFileVersion = UInt16;
  TZipFileGeneralPurposeBitFlag = UInt16;
  TZipFileTime = UInt16;
  TZipFileDate = UInt16;

  PLocalFileHeader = ^TLocalFileHeader;
  TLocalFileHeader = packed record
    Signature               : TZipFileSignature;
    VersionNeededToExtract  : TZipFileVersion;
    GeneralPurposeBitFlag   : TZipFileGeneralPurposeBitFlag;
    CompressionMethod       : UInt16;
    LastModFileTime         : TZipFileTime;
    LastModFileDate         : TZipFileDate;
    CRC32                   : TCRC32;
    CompressedSize          : TZipFileSize;
    UncompressedSize        : TZipFileSize;
    FilenameLength          : TZipStringLength;
    ExtraFieldLength        : TZipStringLength;
  end;

  PDataDescriptor = ^TDataDescriptor;
  TDataDescriptor = packed record
    CRC32             : TCRC32;
    CompressedSize    : TZipFileSize;
    UncompressedSize  : TZipFileSize;
  end;

  PCentralDirectoryFileHeader = ^TCentralDirectoryFileHeader;
  TCentralDirectoryFileHeader = packed record
    Signature                 : TZipFileSignature;
    VersionMadeBy             : TZipFileVersion;
    VersionNeededToExtract    : TZipFileVersion;
    GeneralPurposeBitFlag     : TZipFileGeneralPurposeBitFlag;
    CompressionMethod         : UInt16;
    LastModFileTime           : TZipFileTime;
    LastModFileDate           : TZipFileDate;
    CRC32                     : TCRC32;
    CompressedSize            : TZipFileSize;
    UncompressedSize          : TZipFileSize;
    FilenameLength            : TZipStringLength;
    ExtraFieldLength          : TZipStringLength;
    FileCommentLength         : TZipStringLength;
    DiskNumberStart           : TZipFileDiskNumber;
    InternalFileAttributes    : TZipFileInternalAttributes;
    ExternalFileAttributes    : TZipFileExternalAttributes;
    RelativeOffsetLocalHeader : UInt32;
  end;

  PEndOfCentralDirectoryRecord = ^TEndOfCentralDirectoryRecord;
  TEndOfCentralDirectoryRecord = packed record
    Signature                         : TZipFileSignature;
    ThisDiskNumber                    : TZipFileDiskNumber;
    DiskNumberCentralDirectory        : TZipFileDiskNumber;
    EntriesInCentralDirectoryThisDisk : TZipFileNumber;
    EntriesInCentralDirectory         : TZipFileNumber;
    CentralDirectorySize              : TZipFileSize;
    CentralDirectoryOffset            : TZipFileOffset;
    ZipFileCommentLength              : TZipStringLength;
  end;

  IZipFileInternal = interface
    ['{FA68031A-54CA-4329-A896-CCCB59892284}']

    function GetMemory: PChar;
    function GetSize: UInt32;
  end;

  TZipFile = class(TInterfacedObject, IZipFile, IZipFileInternal)
  private
    FFileName               : string;
    FOptions                : TZipFileOptions;
    FFileStream             : TFileStream;
    FMemStream              : TMemoryMappedFileStream;
    FMemory                 : PChar;
    FDirectoryEntries       : IInterfaceList;
    FComment                : string;
    FDelayCompression       : Boolean;

    FEndOfCentralDirectory  : PEndOfCentralDirectoryRecord;

    procedure SaveOverExisting;
    procedure SaveAsNew(const NewFileName: string);

  protected
    // IZipFile interface
    function GetFileName: string;
    procedure Save(const NewFileName: string='');
    function GetCount: Integer;
    function GetEntries(const Index: Integer): IZipFileEntry;
    function GetComment: string;
    procedure SetComment(const NewComment: string);
    function Add(const Name: string): IZipFileEntry; overload;
    function Add(const Name: string;
      const SourceFileName: string): IZipFileEntry; overload;
    function Add(const Name: string;
      const SourceStream: TStream): IZipFileEntry; overload;
    function Add(const Name: string;
      const SourceSafeMem: ISafeMem): IZipFileEntry; overload;
    function GetDelayCompression: Boolean;
    procedure SetDelayCompression(const NewDelayCompression: Boolean);
    function Exists(const Name: string): Boolean;
    function GetEntryByName(const Name: string): IZipFileEntry;
    procedure Delete(const Name: string); overload;
    procedure Delete(const Entry: IZipFileEntry); overload;
    procedure GetEntryNames(const Dest: TStrings);
    procedure ExtractTo(const BaseDirectory: string);

    // IZipFileInternal interface
    function GetMemory: PChar;
    function GetSize: UInt32;

    // Internal
    procedure OpenZipFile;
    procedure PrepareNewZipFile;
    function LocateEndOfCentralDirectory: Int64;
    procedure ReadCentralDirectory;
    procedure CloseZipFile;

  public
    constructor Create(const FileName: string='';
      const Options: TZipFileOptions=[]);
    destructor Destroy; override;
  end;

  IZipFileEntryInternal = interface
    ['{E40A3A2C-FABA-47CE-AB82-95F1940F093A}']

    function GetDataPointer: Pointer;
    procedure PrepareForSave;
    procedure SaveLocalFileHeader(const Stream: TStream);
    procedure SaveFileContents(const Stream: TStream);
    procedure SaveCentralFileHeader(const Stream: TStream);
    function IsLocked: Boolean;
    procedure ZipFileGone;
  end;

  TZipFileEntry = class(TInterfacedObject, IZipFileEntry, IZipFileEntryInternal)
  private
    FZipFile                    : TZipFile;
    FExisting                   : Boolean;
    FCentralDirectoryFileHeader : TCentralDirectoryFileHeader;
    FLocalFileHeader            : TLocalFileHeader;
    FCentralFileHeaderOffset    : TZipFileOffset;

    FName                       : string;
    FExtraField                 : ISafeMem;
    FComment                    : string;

    FNewContents                : ISafeMem;
    FNewContentsSize            : Integer;
    FNewContentsCompress        : Boolean;
    FNewContentsMethod          : TZipFileCompressionMethod;

    FLocalSavePosition          : Int64;
    FLocked                     : Boolean;

    procedure SanityCheck;
    procedure ReadData;
    procedure InitializeNew;
    procedure NeedZipFile(const Method: string);

    procedure CloseStream;
    procedure CompressNewContents;

    procedure OpenAsStreamRead(out Stream: TStream);
    procedure OpenAsStreamReadWrite(out Stream: TStream);
    procedure OpenAsStreamWrite(out Stream: TStream);

  protected
    // IZipFileEntry interface
    function GetName: string;
    procedure SetName(const NewName: string);
    function GetComment: string;
    procedure SetComment(const NewComment: string);
    function GetDateTime: TDateTime;
    procedure SetDateTime(const NewDateTime: TDateTime);
    procedure Revert;
    function GetZipFile: IZipFile;
    function GetUncompressedSize: TZipFileSize;
    function GetCompressedSize: TZipFileSize;
    function GetCRC32: TCRC32;
    function GetCompressionMethod: TZipFileCompressionMethod;
    function GetMadeBy: TZipFileMadeBy;
    procedure OpenAsStream(out Stream: TStream;
      const OpenMode: TZipFileOpenMode;
      const Compress: Boolean=True);
    procedure ExtractToStream(const Stream: TStream);
    procedure ExtractToFile(const FileName: string;
      const ForceOverwrite: Boolean);
    procedure ExtractToDirectory(const BaseDirectory: string;
      const ForceOverwrite: Boolean=False);
    procedure ReplaceContents(const FileName: string); overload;
    procedure ReplaceContents(const Stream: TStream); overload;
    procedure ReplaceContents(const SafeMem: ISafeMem); overload;
    procedure ReplaceContents(const Data: Pointer; const Size: Integer); overload;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(const Stream: TStream);
    function GetInternalAttributes: TZipFileInternalAttributes;
    procedure SetInternalAttributes(const NewAttributes: TZipFileInternalAttributes);
    function GetExternalAttributes: TZipFileExternalAttributes;
    procedure SetExternalAttributes(const NewAttributes: TZipFileExternalAttributes);
    function GetIsEncrypted: Boolean;
    procedure Recompress;
    procedure CompressNow;
    function GetExtraField: ISafeMem;
    procedure SetExtraField(const NewField: ISafeMem);
    procedure Delete;

    // IZipFileEntryInternal interface
    function GetDataPointer: Pointer;
    procedure PrepareForSave;
    procedure SaveLocalFileHeader(const Stream: TStream);
    procedure SaveFileContents(const Stream: TStream);
    procedure SaveCentralFileHeader(const Stream: TStream);
    function IsLocked: Boolean;
    procedure ZipFileGone;

    // Internal
    procedure Lock;
    procedure Unlock;

  public
    constructor Create(const ZipFile: TZipFile;
      const CentralFileHeaderOffset: TZipFileOffset); overload;
    constructor Create(const ZipFile: TZipFile); overload;
  end;

  TReadZipFileContents = class(TStream)
  private
    FEntry          : TZipFileEntry;
    FLocked         : Boolean;
    FRealPosition   : Int64;
    FSeekedPosition : Int64;
    FSize           : Int64;

  protected
    procedure Restart; virtual;

    function ReadData(var Buffer; const Size: Integer): Integer; virtual;
    procedure ResynchronizePositions; virtual;

    procedure Initialize; virtual;
    procedure CleanUp; virtual;

    {$IFDEF DELPHI6UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ELSE}
    procedure SetSize(NewSize: Longint); override;
    {$ENDIF}

  public
    constructor Create(const Entry: TZipFileEntry);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$IFDEF DELPHI6UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ENDIF}
  end;

  TReadStored = class(TReadZipFileContents)
  private
    FNext : PChar;
    FLeft : Int64;

  protected
    procedure Restart; override;
    function ReadData(var Buffer; const Size: Integer): Integer; override;

  public
    constructor Create(const Entry: TZipFileEntry);
  end;

  TReadDeflated = class(TReadZipFileContents)
  private
    Fzstream      : z_stream;
    FBuffer       : PChar;
    FHead         : PChar;
    FTail         : PChar;
    FInitialized  : Boolean;

    procedure GetNextBuffer;
    procedure StartDecompress;
    procedure EndDecompress;

  protected
    procedure Restart; override;
    function ReadData(var Buffer; const Size: Integer): Integer; override;

    procedure Initialize; override;
    procedure CleanUp; override;
  end;

  TDelayedStream = class(TStream)
  private
    FEntry  : TZipFileEntry;
    FStream : TStream;
    FLocked : Boolean;

  protected
    {$IFDEF DELPHI6UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ELSE}
    procedure SetSize(NewSize: Longint); override;
    {$ENDIF}

  public
    constructor Create(const Entry: TZipFileEntry);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI6UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
  end;

const
  INTERNAL_BUFFER_SIZE                  = 16384;

  ZF_LOCAL_FILE_HEADER_SIGNATURE        = $04034B50;
  ZF_END_OF_CENTRAL_DIRECTORY_SIGNATURE = $06054B50;
  ZF_CENTRAL_FILE_HEADER_SIGNATURE      = $02014B50;

  ZF_MAX_END_OF_CENTRAL_DIRECTORY_SIZE  = SizeOf(TEndOfCentralDirectoryRecord) +
    65536;

function NewZipFile: IZipFile;
begin
  Result := TZipFile.Create;
end;

function OpenZipFile(const FileName: string;
  const Options: TZipFileOptions): IZipFile;
begin
  Result := TZipFile.Create(FileName, Options);
end;

{ TZipFile }

function TZipFile.Add(const Name,
  SourceFileName: string): IZipFileEntry;
begin
  Assert(FileExists(SourceFileName));
  
  Result := Add(Name);
  try
    Result.LoadFromFile(SourceFileName);
  except
    Delete(Result);
    raise;
  end;
end;

function TZipFile.Add(const Name: string): IZipFileEntry;
begin
  Assert(not Exists(Name));

  Result := TZipFileEntry.Create(Self);
  FDirectoryEntries.Add(Result);
  Result.Name := Name;
end;

function TZipFile.Add(const Name: string;
  const SourceSafeMem: ISafeMem): IZipFileEntry;
begin
  Assert(Assigned(SourceSafeMem));

  Result := Add(Name);
  try
    Result.ReplaceContents(SourceSafeMem);
  except
    Delete(Result);
    raise;
  end;
end;

function TZipFile.Add(const Name: string;
  const SourceStream: TStream): IZipFileEntry;
begin
  Assert(Assigned(SourceStream));
  
  Result := Add(Name);
  try
    Result.ReplaceContents(SourceStream);
  except
    Delete(Result);
    raise;
  end;
end;

procedure TZipFile.CloseZipFile;
var
  Index : Integer;
begin
  for Index := 0 to FDirectoryEntries.Count-1 do
    if (FDirectoryEntries[Index] as IZipFileEntryInternal).IsLocked then
      raise EZipFileClose.CreateFmt(ERR_UNABLE_TO_CLOSE_LOCKS_PENDING,
        [FFileName]);

  FDirectoryEntries.Clear;
  FMemory := nil;
  FreeAndNil(FMemStream);
  FreeAndNil(FFileStream);
end;

constructor TZipFile.Create(const FileName: string;
  const Options: TZipFileOptions);
begin
  inherited Create;

  FFileName := FileName;
  FOptions := Options;
  FDirectoryEntries := TInterfaceList.Create;

  if FileName <> '' then
    OpenZipFile
  else
    PrepareNewZipFile;
end;

procedure TZipFile.Delete(const Name: string);
var
  Entry : IZipFileEntry;
begin
  Entry := GetEntryByName(Name);
  if Assigned(Entry) then
    Delete(Entry);
end;

procedure TZipFile.Delete(const Entry: IZipFileEntry);
begin
  Assert(Assigned(Entry));

  (Entry as IZipFileEntryInternal).ZipFileGone;
  FDirectoryEntries.Remove(Entry);
end;

destructor TZipFile.Destroy;
begin
  CloseZipFile;
  FDirectoryEntries := nil;

  inherited;
end;

function TZipFile.Exists(const Name: string): Boolean;
begin
  Result := Assigned(GetEntryByName(Name));
end;

procedure TZipFile.ExtractTo(const BaseDirectory: string);
var
  Index : Integer;
begin
  for Index := 0 to FDirectoryEntries.Count-1 do
    (FDirectoryEntries[Index] as IZipFileEntry).ExtractToDirectory(BaseDirectory);
end;

function TZipFile.GetComment: string;
begin
  Result := FComment;
end;

function TZipFile.GetCount: Integer;
begin
  Result := FDirectoryEntries.Count;
end;

function TZipFile.GetDelayCompression: Boolean;
begin
  Result := FDelayCompression;
end;

function TZipFile.GetEntries(const Index: Integer): IZipFileEntry;
begin
  Result := FDirectoryEntries[Index] as IZipFileEntry;
end;

function TZipFile.GetEntryByName(const Name: string): IZipFileEntry;
var
  Index : Integer;
  Entry : IZipFileEntry;
begin
  Result := nil;
  
  for Index := 0 to FDirectoryEntries.Count-1 do
  begin
    Entry := FDirectoryEntries[Index] as IZipFileEntry;
    if CompareText(Name, Entry.Name) = 0 then
    begin
      Result := Entry;
      Break;
    end;
  end;
end;

procedure TZipFile.GetEntryNames(const Dest: TStrings);
var
  Index : Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for Index := 0 to FDirectoryEntries.Count-1 do
      Dest.Add((FDirectoryEntries[Index] as IZipFileEntry).Name);
  finally
    Dest.EndUpdate;
  end;
end;

function TZipFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TZipFile.GetMemory: PChar;
begin
  Result := FMemory;
end;

function TZipFile.GetSize: UInt32;
begin
  Result := FMemStream.Size;
end;

function TZipFile.LocateEndOfCentralDirectory: Int64;
var
  Found       : Boolean;
  Ptr         : PChar;
  LowerLimit  : Int64;
begin
  Assert(Assigned(FMemStream));
  Assert(FMemStream.Size > SizeOf(TZipFileSignature));
  
  Result := FMemStream.Size - SizeOf(TZipFileSignature);
  Ptr := FMemory + Result;
  Found := False;

  // We can scan the whole file, which might take some time, or we can scan
  // just enough to find the "End of central dir record" that is kept at the
  // end. If the zip file is embedded into another file, possibly with more
  // data after it, then you need to scan the whole file.
  if zfoScanWholeFile in FOptions then
    LowerLimit := 0
  else
    LowerLimit := Max(0, FMemStream.Size - ZF_MAX_END_OF_CENTRAL_DIRECTORY_SIZE);

  // Try to locate the signature, we won't verify the data any closer than that
  while (Result >= LowerLimit) and not Found do
  begin
    if PZipFileSignature(Ptr)^ = ZF_END_OF_CENTRAL_DIRECTORY_SIGNATURE then
      Found := True
    else begin
      Dec(Ptr);
      Dec(Result);
    end;
  end;

  if not Found then
    Result := -1;
end;

procedure TZipFile.OpenZipFile;
begin
  FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  FMemStream := TMemoryMappedFileStream.Create(FFileStream, fmOpenRead, 0,
    FFileStream.Size);

  // This variable is just for convenience
  FMemory := FMemStream.Memory;

  // Get all the data from the zip file so we don't have to keep re-reading this
  // every time we need some of it.
  ReadCentralDirectory;
end;

procedure TZipFile.PrepareNewZipFile;
begin
end;

procedure TZipFile.ReadCentralDirectory;
var
  EOCDPosition      : Int64;
  Index             : Integer;
  Entry             : IZipFileEntry;
  FileHeaderOffset  : TZipFileOffset;
begin
  EOCDPosition := LocateEndOfCentralDirectory;

  // This functions as the sanity check to make sure we got a zip file and not
  // just any other file.
  if EOCDPosition < 0 then
  begin
    raise EZipFileOpen.CreateFmt(ERR_UNABLE_TO_LOCATE_CENTRAL_DIRECTORY,
      [FFilename]);
  end;
  FEndOfCentralDirectory := PEndOfCentralDirectoryRecord(FMemory + EOCDPosition);
  SetLength(FComment, FEndOfCentralDirectory.ZipFileCommentLength);
  Move((FMemory + EOCDPosition + SizeOf(FEndOfCentralDirectory^))^,
    PChar(FComment)^, FEndOfCentralDirectory.ZipFileCommentLength);

  // Since the library can't handle spanned zip files yet, raise exception if
  // such a zip file is encountered.
  if (FEndOfCentralDirectory^.ThisDiskNumber > 0) or
    (FEndOfCentralDirectory^.DiskNumberCentralDirectory > 0) then
  begin
    raise EZipFileOpen.CreateFmt(ERR_UNABLE_TO_OPEN_SPANNED_ZIP_FILES,
      [FFileName]);
  end;

  FileHeaderOffset := FEndOfCentralDirectory^.CentralDirectoryOffset;
  for Index := 0 to FEndOfCentralDirectory^.EntriesInCentralDirectory-1 do
  begin
    Entry := TZipFileEntry.Create(Self, FileHeaderOffset);
    FDirectoryEntries.Add(Entry);

    Inc(FileHeaderOffset, SizeOf(TCentralDirectoryFileHeader));
    Inc(FileHeaderOffset, Length(Entry.Name) + Length(Entry.Comment));
    //Inc(FileHeaderOffset, Entry.ExtraField.Size);
  end;
end;

procedure TZipFile.Save(const NewFileName: string);
begin
  if NewFileName = '' then
    SaveOverExisting
  else
    SaveAsNew(NewFileName);
end;

procedure TZipFile.SaveAsNew(const NewFileName: string);
var
  Stream                  : TFileStream;
  Index                   : Integer;
  EndOfCentralDirectory   : TEndOfCentralDirectoryRecord;
  CentralDirectoryOffset  : Int64;
begin
  if NewFileName = '' then
    raise EZipFileSave.Create(ERR_NO_FILENAME);

  if CompareText(NewFileName, FFileName) = 0 then
  begin
    SaveOverExisting;
    Exit;
  end;

  Stream := TFileStream.Create(NewFileName, fmCreate);
  try
    // Prepare all entries for saving
    for Index := 0 to FDirectoryEntries.Count-1 do
      (FDirectoryEntries[Index] as IZipFileEntryInternal).PrepareForSave;

    // Save local file headers + file contents
    for Index := 0 to FDirectoryEntries.Count-1 do
    begin
      (FDirectoryEntries[Index] as IZipFileEntryInternal).SaveLocalFileHeader(
        Stream);
      (FDirectoryEntries[Index] as IZipFileEntryInternal).SaveFileContents(
        Stream);
    end;

    // Save central directory file headers
    CentralDirectoryOffset := Stream.Position;
    for Index := 0 to FDirectoryEntries.Count-1 do
      (FDirectoryEntries[Index] as IZipFileEntryInternal).SaveCentralFileHeader(
        Stream);

    // Save end of central directory structure
    ZeroMemory(@EndOfCentralDirectory, SizeOf(EndOfCentralDirectory));
    EndOfCentralDirectory.Signature := ZF_END_OF_CENTRAL_DIRECTORY_SIGNATURE;
    EndOfCentralDirectory.EntriesInCentralDirectoryThisDisk :=
      FDirectoryEntries.Count;
    EndOfCentralDirectory.EntriesInCentralDirectory := FDirectoryEntries.Count;
    EndOfCentralDirectory.CentralDirectorySize := Stream.Position -
      CentralDirectoryOffset;
    EndOfCentralDirectory.CentralDirectoryOffset := CentralDirectoryOffset;
    EndOfCentralDirectory.ZipFileCommentLength := Length(FComment);
    Stream.WriteBuffer(EndOfCentralDirectory, SizeOf(EndOfCentralDirectory));

    if FComment <> '' then
      Stream.WriteBuffer(PChar(FComment)^, Length(FComment));
  finally
    Stream.Free;
  end;
end;

procedure TZipFile.SaveOverExisting;
var
  TempFileName1 : string;
  TempFileName2 : string;

  function GetUniqueEnding(const s: string): string;
  var
    GUID  : TGUID;
  begin
    CoCreateGUID(GUID);
    Result := s + '.$$' + IntToStr(GUID.D2);
  end;

begin
  if FFileName = '' then
    raise EZipFileSave.Create(ERR_NO_FILENAME);
  repeat
    TempFileName1 := GetUniqueEnding(FFileName);
  until not FileExists(TempFileName1);
  repeat
    TempFileName2 := GetUniqueEnding(FFileName);
  until not FileExists(TempFileName2);

  Save(TempFileName1);
  try
    CloseZipFile;
    try
      if not RenameFile(FFileName, TempFileName2) then
        RaiseLastWin32Error;
      try
        if not RenameFile(TempFileName1, FFileName) then
          RaiseLastWin32Error;
        DeleteFile(PChar(TempFileName2));
      except
        RenameFile(TempFileName2, FFileName);
        raise;
      end;
    finally
      OpenZipFile;
    end;
  except
    DeleteFile(PChar(TempFileName1));
    raise;
  end;
end;

procedure TZipFile.SetComment(const NewComment: string);
begin
  FComment := NewComment;
end;

procedure TZipFile.SetDelayCompression(const NewDelayCompression: Boolean);
begin
  FDelayCompression := NewDelayCompression;
end;

{ TZipFileEntry }

constructor TZipFileEntry.Create(const ZipFile: TZipFile;
  const CentralFileHeaderOffset: TZipFileOffset);
begin
  inherited Create;

  FZipFile := ZipFile;
  FCentralFileHeaderOffset := CentralFileHeaderOffset;
  FCentralDirectoryFileHeader := PCentralDirectoryFileHeader(
    (ZipFile as IZipFileInternal).GetMemory + FCentralFileHeaderOffset)^;
  FExisting := True;

  if FCentralDirectoryFileHeader.Signature <> ZF_CENTRAL_FILE_HEADER_SIGNATURE then
  begin
    raise EZipFileOpen.CreateFmt(ERR_INVALID_CENTRAL_FILE_HEADER,
      [ZipFile.GetFileName]);
  end;

  FLocalFileHeader := PLocalFileHeader(
    (ZipFile as IZipFileInternal).GetMemory +
      FCentralDirectoryFileHeader.RelativeOffsetLocalHeader)^;

  if FLocalFileHeader.Signature <> ZF_LOCAL_FILE_HEADER_SIGNATURE then
  begin
    raise EZipFileOpen.CreateFmt(ERR_INVALID_LOCAL_FILE_HEADER,
      [ZipFile.GetFileName]);
  end;

  ReadData;
end;

procedure TZipFileEntry.CloseStream;
begin
  FExisting := False;

  FNewContentsSize := FNewContents.Size;
    if FNewContentsCompress and not FZipFile.GetDelayCompression then
      CompressNewContents;
end;

constructor TZipFileEntry.Create(const ZipFile: TZipFile);
begin
  inherited Create;

  FZipFile := ZipFile;
  FExisting := False;
  InitializeNew;
end;

procedure TZipFileEntry.ExtractToFile(const FileName: string;
  const ForceOverwrite: Boolean);
var
  Stream        : TFileStream;
  SystemTime    : TSystemTime;
  LastWriteTime : TFileTime;
begin
  NeedZipFile('ExtractToFile');

  Assert(FileName <> '');
  if GetIsEncrypted then
    raise EZipFileExtract.CreateFmt(ERR_UNABLE_TO_HANDLE_ENCRYPTED_FILES,
      [FName, FZipFile.GetFileName]);

  if ForceOverwrite and FileExists(FileName) then
  begin
    SetFileAttributes(PChar(FileName), 0);
    DeleteFile(PChar(FileName));
  end;

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExtractToStream(Stream);

    DateTimeToSystemTime(GetDateTime, SystemTime);
    if not SystemTimeToFileTime(SystemTime, LastWriteTime) then
      RaiseLastWin32Error;
    if not SetFileTime(Stream.Handle, nil, nil, @LastWriteTime) then
      RaiseLastWin32Error;
  finally
    Stream.Free;
  end;

  if GetMadeBy in [zfmbMSDOS_OS2_FAT, zfmbWindowsNT_NTFS] then
    if not SetFileAttributes(PChar(FileName), GetExternalAttributes) then
      RaiseLastWin32Error;
end;

procedure TZipFileEntry.ExtractToStream(const Stream: TStream);
var
  SourceStream  : TStream;
begin
  NeedZipFile('ExtractToStream');

  Assert(Assigned(Stream));
  if GetIsEncrypted then
    raise EZipFileExtract.CreateFmt(ERR_UNABLE_TO_HANDLE_ENCRYPTED_FILES,
      [FName, FZipFile.GetFileName]);

  OpenAsStream(SourceStream, zomRead);
  try
    Stream.CopyFrom(SourceStream, 0);
  finally
    SourceStream.Free;
  end;
end;

function TZipFileEntry.GetComment: string;
begin
  Result := FComment;
end;

function TZipFileEntry.GetCompressedSize: TZipFileSize;
begin
  if FExisting then
    Result := FCentralDirectoryFileHeader.CompressedSize
  else
    Result := FNewContents.Size;
end;

function TZipFileEntry.GetCompressionMethod: TZipFileCompressionMethod;
begin
  if FExisting then
  begin
    if FCentralDirectoryFileHeader.CompressionMethod in [0..10] then
      Result := TZipFileCompressionMethod(
        FCentralDirectoryFileHeader.CompressionMethod)
    else
      Result := zfcmUnknown;
  end else
    Result := FNewContentsMethod;
end;

function TZipFileEntry.GetCRC32: TCRC32;
begin
  Result := FCentralDirectoryFileHeader.CRC32
end;

function TZipFileEntry.GetDataPointer: Pointer;
begin
  NeedZipFile('GetDataPointer');

  if FExisting then
  begin
    Result := (FZipFile as IZipFileInternal).GetMemory +
      FCentralDirectoryFileHeader.RelativeOffsetLocalHeader +
      SizeOf(FLocalFileHeader) +
      FLocalFileHeader.FileNameLength +
      FLocalFileHeader.ExtraFieldLength;
  end else
    Result := FNewContents.Pointer;
end;

function TZipFileEntry.GetDateTime: TDateTime;
var
  FileTime    : TFileTime;
  SystemTime  : TSystemTime;
begin
  if not DosDateTimeToFileTime(FCentralDirectoryFileHeader.LastModFileDate,
    FCentralDirectoryFileHeader.LastModFileTime, FileTime) then
  begin
    RaiseLastWin32Error;
  end;

  if not FileTimeToSystemTime(FileTime, SystemTime) then
    RaiseLastWin32Error;

  Result := SystemTimeToDateTime(SystemTime);
end;

function TZipFileEntry.GetExternalAttributes: TZipFileExternalAttributes;
begin
  Result := FCentralDirectoryFileHeader.ExternalFileAttributes;
end;

function TZipFileEntry.GetInternalAttributes: TZipFileInternalAttributes;
begin
  Result := FCentralDirectoryFileHeader.InternalFileAttributes;
end;

function TZipFileEntry.GetIsEncrypted: Boolean;
begin
  Result := (FCentralDirectoryFileHeader.GeneralPurposeBitFlag and 1) <> 0;
end;

function TZipFileEntry.GetMadeBy: TZipFileMadeBy;
begin
  case FCentralDirectoryFileHeader.VersionMadeBy of
    0..10:
      Result := TZipFileMadeBy(FCentralDirectoryFileHeader.VersionMadeBy);

    2838:
      Result := zfmbWindowsNT_NTFS; // ???
  else
    Result := zfmbUnknown;
  end;
end;

function TZipFileEntry.GetName: string;
begin
  Result := FName;
end;

function TZipFileEntry.GetUncompressedSize: TZipFileSize;
begin
  if FExisting then
    Result := FCentralDirectoryFileHeader.UncompressedSize
  else
    Result := FNewContentsSize;
end;

function TZipFileEntry.GetZipFile: IZipFile;
begin
  Result := FZipFile;
end;

procedure TZipFileEntry.InitializeNew;
begin
  ZeroMemory(@FCentralDirectoryFileHeader, SizeOf(FCentralDirectoryFileHeader));
  SetDateTime(Now);
  FCentralDirectoryFileHeader.Signature := ZF_CENTRAL_FILE_HEADER_SIGNATURE;
  FCentralDirectoryFileHeader.VersionMadeBy := 10; // 2838 ?
  FCentralDirectoryFileHeader.VersionNeededToExtract := 20;
  FCentralDirectoryFileHeader.GeneralPurposeBitFlag := 0;

  ZeroMemory(@FLocalFileHeader, SizeOf(FLocalFileHeader));
  FLocalFileHeader.Signature := ZF_LOCAL_FILE_HEADER_SIGNATURE;
  FLocalFileHeader.VersionNeededToExtract := 20;

  FExisting := False;
  FNewContents := AllocateSafeMem(0, True);
  FNewContentsSize := 0;
end;

procedure TZipFileEntry.Lock;
begin
  if FLocked then
    raise EZipFileReadLock.Create(ERR_ENTRY_ALREADY_LOCKED);

  FLocked := True;
end;

procedure TZipFileEntry.OpenAsStream(out Stream: TStream;
  const OpenMode: TZipFileOpenMode;
  const Compress: Boolean);
begin
  case OpenMode of
    zomRead:
      OpenAsStreamRead(Stream);

    zomWrite:
      begin
        OpenAsStreamWrite(Stream);
        FNewContentsCompress := Compress;
      end;

    zomReadWrite:
      begin
        OpenAsStreamReadWrite(Stream);
        FNewContentsCompress := Compress;
      end;
  end;
end;

procedure TZipFileEntry.OpenAsStreamRead(out Stream: TStream);
begin
  NeedZipFile('OpenAsStreamRead');

  if GetIsEncrypted then
    raise EZipFileExtract.CreateFmt(ERR_UNABLE_TO_HANDLE_ENCRYPTED_FILES,
      [FName, FZipFile.GetFileName]);

  case GetCompressionMethod of
    zfcmStored:
      Stream := TReadStored.Create(Self);

    zfcmShrunk..zfcmReduced4, zfcmTokenized, zfcmEnhancedDeflated,
    zfcmPKWARELibraryImploded, zfcmUnknown:
      raise EZipFileOpenStream.CreateFmt(ERR_INVALID_COMPRESSION_METHOD,
        [FZipFile.GetFileName, FName]);

    zfcmDeflated:
      Stream := TReadDeflated.Create(Self);

  else
    Assert(False);
  end;
end;

procedure TZipFileEntry.OpenAsStreamReadWrite(out Stream: TStream);

  procedure Decompress;
  var
    TempStream    : TStream;
    TempContents  : ISafeMem;
  begin
    TempContents := nil;
    OpenAsStream(TempStream, zomRead);
    try
      TempContents := AllocateSafeMem(TempStream.Size, True);
      TempContents.Stream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    FNewContents := TempContents;
    FExisting := False;
    FNewContentsMethod := zfcmStored;
  end;

begin
  NeedZipFile('OpenAsStreamReadWrite');

  if FLocked then
    raise EZipFileReadLock.Create(ERR_ENTRY_ALREADY_LOCKED);
  if GetIsEncrypted then
    raise EZipFileExtract.CreateFmt(ERR_UNABLE_TO_HANDLE_ENCRYPTED_FILES,
      [FName, FZipFile.GetFileName]);

  if FExisting then
  begin
    Decompress;
    Stream := TDelayedStream.Create(Self);
  end else begin
    if FNewContentsMethod = zfcmDeflated then
      Decompress;

    Stream := TDelayedStream.Create(Self);
  end;
end;

procedure TZipFileEntry.PrepareForSave;
begin
  if FNewContentsCompress and (FNewContentsMethod = zfcmStored) then
    CompressNewContents;

  FCentralDirectoryFileHeader.FilenameLength := Length(FName);
  if Assigned(FExtraField) then
    FCentralDirectoryFileHeader.ExtraFieldLength := FExtraField.Size
  else
    FCentralDirectoryFileHeader.ExtraFieldLength := 0;
  FCentralDirectoryFileHeader.FileCommentLength := Length(FComment);

  FLocalFileHeader.FilenameLength := Length(FName);
  if Assigned(FExtraField) then
    FLocalFileHeader.ExtraFieldLength := FExtraField.Size
  else
    FLocalFileHeader.ExtraFieldLength := 0;

  FLocalFileHeader.LastModFileDate :=
    FCentralDirectoryFileHeader.LastModFileDate;
  FLocalFileHeader.LastModFileTime :=
    FCentralDirectoryFileHeader.LastModFileTime;

  if not FExisting then
  begin
    FCentralDirectoryFileHeader.CompressionMethod := Ord(FNewContentsMethod);
    FCentralDirectoryFileHeader.UncompressedSize := FNewContentsSize;
    FCentralDirectoryFileHeader.CompressedSize := FNewContents.Size;

    FLocalFileHeader.CompressionMethod := Ord(FNewContentsMethod);
    FLocalFileHeader.UncompressedSize := FNewContentsSize;
    FLocalFileHeader.CompressedSize := FNewContents.Size;
  end;
end;

procedure TZipFileEntry.ReadData;
var
  Ptr : PChar;
begin
  SanityCheck;

  Ptr := (FZipFile as IZipFileInternal).GetMemory +
    FCentralFileHeaderOffset +
    SizeOf(FCentralDirectoryFileHeader);

  // Extract name of entry
  SetLength(FName, FCentralDirectoryFileHeader.FilenameLength);
  Move(Ptr^, PChar(FName)^, FCentralDirectoryFileHeader.FilenameLength);
  Inc(Ptr, FCentralDirectoryFileHeader.FilenameLength);

  // Extract extra data
  if FCentralDirectoryFileHeader.ExtraFieldLength > 0 then
  begin
    // If the extra field is more than 1KB in size, it will be allocated and
    // stored as a temporary field instead of in memory, to reduce the overall
    // memory use of the zipfile class.
    FExtraField := AllocateSafeMem(
      FCentralDirectoryFileHeader.ExtraFieldLength,
      FCentralDirectoryFileHeader.ExtraFieldLength > 1024);
    Move(Ptr^, FExtraField.Pointer^,
      FCentralDirectoryFileHeader.ExtraFieldLength);
  end else
    FExtraField := nil;
  Inc(Ptr, FCentralDirectoryFileHeader.ExtraFieldLength);

  // Extract file comment
  SetLength(FComment, FCentralDirectoryFileHeader.FileCommentLength);
  Move(Ptr^, PChar(FComment)^, FCentralDirectoryFileHeader.FileCommentLength);
end;

procedure TZipFileEntry.Revert;
begin
  if FExisting then
    ReadData
  else
    InitializeNew;
end;

procedure TZipFileEntry.SanityCheck;
var
  OK  : Boolean;
begin
  NeedZipFile('SanityCheck');

  OK := FCentralDirectoryFileHeader.VersionNeededToExtract =
    FLocalFileHeader.VersionNeededToExtract;
  OK := OK and (FCentralDirectoryFileHeader.GeneralPurposeBitFlag =
    FLocalFileHeader.GeneralPurposeBitFlag);
  OK := OK and (FCentralDirectoryFileHeader.CompressionMethod =
    FLocalFileHeader.CompressionMethod);
  OK := OK and (FCentralDirectoryFileHeader.LastModFileTime =
    FLocalFileHeader.LastModFileTime);
  OK := OK and (FCentralDirectoryFileHeader.LastModFileDate =
    FLocalFileHeader.LastModFileDate);
  { TODO 2 -oLVK -cSource : Make sure we handle the crc32 follows data case }
  if (FLocalFileHeader.GeneralPurposeBitFlag and 8) = 0 then
  begin
    OK := OK and (FCentralDirectoryFileHeader.CRC32 =
      FLocalFileHeader.CRC32);
    OK := OK and (FCentralDirectoryFileHeader.CompressedSize =
      FLocalFileHeader.CompressedSize);
    OK := OK and (FCentralDirectoryFileHeader.UncompressedSize =
      FLocalFileHeader.UncompressedSize);
  end;
  OK := OK and (FCentralDirectoryFileHeader.FilenameLength =
    FLocalFileHeader.FilenameLength);
  OK := OK and (FCentralDirectoryFileHeader.ExtraFieldLength =
    FLocalFileHeader.ExtraFieldLength);

  if not OK then
    raise EZipFileOpen.CreateFmt(ERR_FILE_HEADERS_DIFFER,
      [FZipFile.GetFileName]);
end;

procedure TZipFileEntry.SaveCentralFileHeader(const Stream: TStream);
begin
  FCentralDirectoryFileHeader.RelativeOffsetLocalHeader := FLocalSavePosition;
  Stream.WriteBuffer(FCentralDirectoryFileHeader,
    SizeOf(FCentralDirectoryFileHeader));
  Stream.WriteBuffer(PChar(FName)^, Length(FName));
  if Assigned(FExtraField) then
    Stream.CopyFrom(FExtraField.Stream, 0);
  Stream.WriteBuffer(PChar(FComment)^, Length(FComment));
end;

procedure TZipFileEntry.SaveFileContents(const Stream: TStream);
begin
  if FExisting then
  begin
    Stream.WriteBuffer(GetDataPointer^,
      FCentralDirectoryFileHeader.CompressedSize)
  end else
    Stream.WriteBuffer(FNewContents.Pointer^, FNewContents.Size);
end;

procedure TZipFileEntry.SaveLocalFileHeader(const Stream: TStream);
begin
  FLocalSavePosition := Stream.Position;
  Stream.WriteBuffer(FLocalFileHeader, SizeOf(FLocalFileHeader));
  Stream.WriteBuffer(PChar(FName)^, Length(FName));
  if Assigned(FExtraField) then
    Stream.CopyFrom(FExtraField.Stream, 0);
end;

procedure TZipFileEntry.SetComment(const NewComment: string);
begin
  FComment := NewComment;
end;

procedure TZipFileEntry.SetDateTime(const NewDateTime: TDateTime);
var
  FileTime    : TFileTime;
  SystemTime  : TSystemTime;
begin
  DateTimeToSystemTime(NewDateTime, SystemTime);

  if not SystemTimeToFileTime(SystemTime, FileTime) then
    RaiseLastWin32Error;

  if not FileTimeToDosDateTime(FileTime,
    FCentralDirectoryFileHeader.LastModFileDate,
    FCentralDirectoryFileHeader.LastModFileTime) then
  begin
    RaiseLastWin32Error;
  end;
end;

procedure TZipFileEntry.SetExternalAttributes(
  const NewAttributes: TZipFileExternalAttributes);
begin
  FCentralDirectoryFileHeader.ExternalFileAttributes := NewAttributes;
end;

procedure TZipFileEntry.SetInternalAttributes(
  const NewAttributes: TZipFileInternalAttributes);
begin
  FCentralDirectoryFileHeader.InternalFileAttributes := NewAttributes;
end;

procedure TZipFileEntry.SetName(const NewName: string);
begin
  FName := NewName;
end;

procedure TZipFileEntry.Unlock;
begin
  if not FLocked then
    raise EZipFileReadLock.Create(ERR_ENTRY_NOT_LOCKED);

  FLocked := False;
end;

procedure TZipFileEntry.CompressNewContents;
var
  Compressed  : ISafeMem;
begin
  FCentralDirectoryFileHeader.CRC32 := CRC32Of(FNewContents);
  FLocalFileHeader.CRC32 := FCentralDirectoryFileHeader.CRC32;
  
  if FNewContents.Size > 10 then
  begin
    Compressed := zLibCompress(FNewContents, 9, 32768, False);
    if Compressed.Size < FNewContents.Size then
    begin
      FNewContents := Compressed;
      FNewContentsMethod := zfcmDeflated;
    end;
  end;
end;

procedure TZipFileEntry.OpenAsStreamWrite(out Stream: TStream);
begin
  if FLocked then
    raise EZipFileReadLock.Create(ERR_ENTRY_ALREADY_LOCKED);

  FCentralDirectoryFileHeader.GeneralPurposeBitFlag :=
    FCentralDirectoryFileHeader.GeneralPurposeBitFlag and (not 1);
  FLocalFileHeader.GeneralPurposeBitFlag :=
    FCentralDirectoryFileHeader.GeneralPurposeBitFlag;

  FNewContentsMethod := zfcmStored;
  FNewContents := AllocateSafeMem(0, True);
  Stream := TDelayedStream.Create(Self);
end;

procedure TZipFileEntry.LoadFromFile(const FileName: string);
begin
  ReplaceContents(FileName);
end;

procedure TZipFileEntry.LoadFromStream(const Stream: TStream);
begin
  ReplaceContents(Stream);
end;

procedure TZipFileEntry.ReplaceContents(const Stream: TStream);
var
  TempStream  : TStream;
begin
  OpenAsStream(TempStream, zomWrite);
  try
    TempStream.CopyFrom(Stream, 0);
  finally
    TempStream.Free;
  end;
end;

procedure TZipFileEntry.ReplaceContents(const FileName: string);
var
  TempStream  : TStream;
begin
  TempStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ReplaceContents(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TZipFileEntry.ReplaceContents(const Data: Pointer;
  const Size: Integer);
var
  TempStream  : TStream;
begin
  OpenAsStream(TempStream, zomWrite);
  try
    TempStream.WriteBuffer(Data^, Size);
  finally
    TempStream.Free;
  end;
end;

procedure TZipFileEntry.ReplaceContents(const SafeMem: ISafeMem);
begin
  Assert(Assigned(SafeMem));

  ReplaceContents(SafeMem.Pointer, SafeMem.Size);
end;

procedure TZipFileEntry.SaveToFile(const FileName: string);
begin
  ExtractToFile(FileName, True);
end;

procedure TZipFileEntry.SaveToStream(const Stream: TStream);
begin
  ExtractToStream(Stream);
end;

procedure TZipFileEntry.CompressNow;
begin
  if FNewContentsMethod = zfcmStored then
    Recompress;
end;

procedure TZipFileEntry.Recompress;
var
  Stream  : TStream;
begin
  OpenAsStream(Stream, zomReadWrite, True);
  Stream.Free;
end;

function TZipFileEntry.IsLocked: Boolean;
begin
  Result := FLocked;
end;

function TZipFileEntry.GetExtraField: ISafeMem;
begin
  Result := FExtraField;
end;

procedure TZipFileEntry.SetExtraField(const NewField: ISafeMem);
begin
  FExtraField := NewField;
end;

procedure TZipFileEntry.ZipFileGone;
begin
  FZipFile := nil;
end;

procedure TZipFileEntry.ExtractToDirectory(const BaseDirectory: string;
  const ForceOverwrite: Boolean);
var
  FileName  : string;
begin
  FileName := IncludeTrailingBackslash(BaseDirectory) + GetName;
  ForceDirectories(ExtractFilePath(FileName));
  ExtractToFile(FileName, ForceOverwrite);
end;

procedure TZipFileEntry.Delete;
begin
  NeedZipFile('Delete');

  if Assigned(FZipFile) then
    FZipFile.Delete(Self);
end;

procedure TZipFileEntry.NeedZipFile(const Method: string);
begin
  if not Assigned(FZipFile) then
    raise EZipFile.CreateFmt(ERR_ZIP_FILE_GONE, [Method]);
end;

{ TReadZipFileContents }

constructor TReadZipFileContents.Create(const Entry: TZipFileEntry);
begin
  inherited Create;

  FEntry := Entry;
  FEntry.Lock;
  FLocked := True;

  FSize := Entry.GetUncompressedSize;

  Initialize;
  Restart;
end;

destructor TReadZipFileContents.Destroy;
begin
  CleanUp;
  if FLocked then
    FEntry.Unlock;

  inherited;
end;

function TReadZipFileContents.Read(var Buffer; Count: Integer): Longint;
begin
  if FRealPosition <> FSeekedPosition then
    ResynchronizePositions;

  Result := ReadData(Buffer, Count);
  Inc(FSeekedPosition, Result);
  Inc(FRealPosition, Result);
end;

function TReadZipFileContents.ReadData(var Buffer; const Size: Integer): Integer;
begin
  // Dummy
  Result := 0;
end;

procedure TReadZipFileContents.Restart;
begin
  FSeekedPosition := 0;
  FRealPosition := 0;
end;

function TReadZipFileContents.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FSeekedPosition := Offset;

    soFromCurrent:
      FSeekedPosition := FSeekedPosition + Offset;

    soFromEnd:
      FSeekedPosition := FSize + Offset;
  end;

  if FSeekedPosition < 0 then
    FSeekedPosition := 0
  else if FSeekedPosition > FSize then
    FSeekedPosition := FSize;

  Result := FSeekedPosition;
end;

procedure TReadZipFileContents.ResynchronizePositions;
var
  Amount      : Int64;
  ToRead      : Int64;
  AmountRead  : Int64;
  Buffer      : PChar;
  ToPosition  : Int64;
begin
  if FRealPosition <> FSeekedPosition then
  begin
    if FRealPosition < FSeekedPosition then
    begin
      Amount := FSeekedPosition - FRealPosition;
      GetMem(Buffer, INTERNAL_BUFFER_SIZE);
      try
        while Amount > 0 do
        begin
          ToRead := Min(INTERNAL_BUFFER_SIZE, Amount);
          AmountRead := ReadData(Buffer^, ToRead);
          if AmountRead <> ToRead then
            raise EZipFileSeek.Create('Unable to read data, file pointer is invalid');
          Dec(Amount, AmountRead);
        end;
      finally
        FreeMem(Buffer);
      end;
    end else begin
      ToPosition := FSeekedPosition;
      Restart;
      FSeekedPosition := ToPosition;
      ResynchronizePositions;
    end;
  end;
end;

{$IFDEF DELPHI6UP}
function TReadZipFileContents.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FSeekedPosition := Offset;

    soCurrent:
      FSeekedPosition := FSeekedPosition + Offset;

    soEnd:
      FSeekedPosition := FSize + Offset;
  end;

  if FSeekedPosition < 0 then
    FSeekedPosition := 0
  else if FSeekedPosition > FSize then
    FSeekedPosition := FSize;

  Result := FSeekedPosition;
end;
{$ENDIF}

function TReadZipFileContents.Write(const Buffer; Count: Integer): Longint;
begin
  raise EZipFileWrite.Create('Unable to write to zip file entry, has been opened for reading only');
end;

procedure TReadZipFileContents.CleanUp;
begin
  // Do nothing by default
end;

procedure TReadZipFileContents.Initialize;
begin
  // Do nothing by default
end;

{$IFDEF DELPHI6UP}
procedure TReadZipFileContents.SetSize(const NewSize: Int64);
{$ELSE}
procedure TReadZipFileContents.SetSize(NewSize: Longint); 
{$ENDIF}
begin
  raise EZipFileRead.CreateFmt(ERR_FILE_IS_READ_ONLY,
    [FEntry.GetName, FEntry.FZipFile.GetFileName]);
end;

{ TReadStored }

constructor TReadStored.Create(const Entry: TZipFileEntry);
begin
  inherited Create(Entry);

  Assert(Entry.GetCompressionMethod = zfcmStored);
end;

function TReadStored.ReadData(var Buffer; const Size: Integer): Integer;
begin
  Assert(Size > 0);

  Result := Min(Size, FLeft);
  Move(FNext^, Buffer, Result);

  Inc(FNext, Result);
  Dec(FLeft, Result);
end;

procedure TReadStored.Restart;
begin
  inherited;

  FLeft := FEntry.GetUncompressedSize;
  FNext := (FEntry as IZipFileEntryInternal).GetDataPointer;
end;

{ TReadDeflated }

procedure TReadDeflated.CleanUp;
begin
  EndDecompress;
  FreeMem(FBuffer, INTERNAL_BUFFER_SIZE);
end;

procedure TReadDeflated.EndDecompress;
begin
  if FInitialized then
  begin
    zLibCheckDecompress(inflateEnd(Fzstream));
    FInitialized := False;
    ZeroMemory(@Fzstream, SizeOf(Fzstream));
  end;
end;

procedure TReadDeflated.GetNextBuffer;
var
  rc  : Integer;
begin
  Fzstream.next_out := FBuffer;
  Fzstream.avail_out := INTERNAL_BUFFER_SIZE;

  rc := inflate(Fzstream, 0);
  if rc <> Z_STREAM_END then
    zLibCheckDecompress(rc);

  FHead := FBuffer;
  FTail := FBuffer + INTERNAL_BUFFER_SIZE - Fzstream.avail_out;
end;

procedure TReadDeflated.Initialize;
begin
  GetMem(FBuffer, INTERNAL_BUFFER_SIZE);
  StartDecompress;
end;

function TReadDeflated.ReadData(var Buffer; const Size: Integer): Integer;
var
  Ptr     : PChar;
  ToCopy  : Integer;
begin
  Ptr := @Buffer;
  Result := 0;

  while Result < Size do
  begin
    if FHead = FTail then
      GetNextBuffer;

    if FTail > FHead then
    begin
      ToCopy := Min(FTail - FHead, Size - Result);
      Move(FHead^, Ptr^, ToCopy);

      Inc(FHead, ToCopy);
      Inc(Ptr, ToCopy);
      Inc(Result, ToCopy);
    end else
      Break;
  end;
end;

procedure TReadDeflated.Restart;
begin
  inherited;

  EndDecompress;
  StartDecompress;
end;

procedure TReadDeflated.StartDecompress;
begin
  ZeroMemory(@Fzstream, SizeOf(Fzstream));

  Fzstream.next_in := (FEntry as IZipFileEntryInternal).GetDataPointer;
  Fzstream.avail_in := FEntry.GetCompressedSize;

  Fzstream.next_out := FBuffer;
  Fzstream.avail_out := INTERNAL_BUFFER_SIZE;

  zLibCheckDecompress(inflateInit2(Fzstream, -15));

  FTail := FBuffer;
  FHead := FBuffer;

  FInitialized := True;
end;

{ TDelayedStream }

constructor TDelayedStream.Create(const Entry: TZipFileEntry);
begin
  inherited Create;

  FEntry := Entry;
  FStream := FEntry.FNewContents.Stream;
  FStream.Position := 0;

  FEntry.Lock;
  FLocked := True;
end;

destructor TDelayedStream.Destroy;
begin
  FStream := nil;
  FEntry.CloseStream;
  if FLocked then
    FEntry.Unlock;

  inherited;
end;

function TDelayedStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TDelayedStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

{$IFDEF DELPHI6UP}
function TDelayedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;
{$ENDIF}

{$IFDEF DELPHI6UP}
procedure TDelayedStream.SetSize(const NewSize: Int64);
{$ELSE}
procedure TDelayedStream.SetSize(NewSize: Longint);
{$ENDIF}
begin
  FStream.Size := NewSize;
end;

function TDelayedStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

end.

