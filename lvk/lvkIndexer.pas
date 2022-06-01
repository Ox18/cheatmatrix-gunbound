{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the interface definitions and the framework for
    creating instances of classes that support these interfaces.
}
{ TODO 2 -oLVK -cSource : Add locale-handling to this code, look at GetStringType }
unit lvkIndexer;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkIndexer.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, ActiveX, lvkSafeMem, lvkTypes;

const
  INVALID_DOCUMENT_ID = High(UInt32);

type
  { Description:
      This interface is used for files that should be indexed. An "Indexable"
      interface represents an amount of data that can be indexed.
    See also:
      IIndexableTStream, IIndexableIStream, IIndexableString, IIndexableMemory
  }
  IIndexableFile = interface
    ['{E1A7898B-872A-11D5-B249-0004761A6377}']

    { Description:
        This method opens up the file with the given filename and prepares
        internal data structures for indexing.
      Parameters:
        Filename - The full path and name of the file to open for indexing.
    }
    procedure Open(const Filename: string);
  end;

  { Description:
      This interface is used for TStream instances that should be indexed.
    See also:
      IIndexableFile, IIndexableIStream, IIndexableString, IIndexableMemory
  }
  IIndexableTStream = interface
    ['{E1A7898C-872A-11D5-B249-0004761A6377}']

    { Description:
        This method opens up the TStream instance and prepares internal data
        structures for indexing. The Indexable instance can optionally
        own the TStream instance, in which case the instance will be destroyed
        when the indexable object is destroyed.
      Parameters:
        Stream - The TStream instance to index.
        OwnsStream - Set this to True to let the indexable object "own" the
          instance.
    }
    procedure Open(const Stream: TStream; const OwnsStream: Boolean);
  end;

  { Description:
      This interface is used for IStream-able object instances that should be
      indexed.
    See also:
      IIndexableFile, IIndexableTStream, IIndexableString, IIndexableMemory
  }
  IIndexableIStream = interface
    ['{E1A7898F-872A-11D5-B249-0004761A6377}']

    { Description:
        This method opens up the object referred to by the IStream interface
        for indexing.
      Parameters:
        Stream - The IStream interface that represents the object to be
          indexed.
    }
    procedure Open(const Stream: IStream);
  end;

  { Description:
      This interface is used for indexing raw text. Just give this indexable
      a string and it'll prepare it for indexing.
    See also:
      IIndexableFile, IIndexableTStream, IIndexableIStream, IIndexableMemory
  }
  IIndexableString = interface
    ['{E1A7898D-872A-11D5-B249-0004761A6377}']

    { Description:
        This method takes the given string and prepares it for indexing.
      Parameters:
        Text - The text string to index.
    }
    procedure Open(const Text: string);
  end;

  { Description:
      This interface is used for indexing data in memory.
    See also:
      IIndexableFile, IIndexableTStream, IIndexableIStream, IIndexableString
  }
  IIndexableMemory = interface
    ['{E1A7898E-872A-11D5-B249-0004761A6377}']

    { Description:
        This method will prepare data structures for the memory block given
        and prepare it for indexing. Note that the data must not be
        freed or changed while the indexing is running.
      Parameters:
        Data - Pointer to the start of the memory block to index.
        DataSize - Number of bytes available in the memory block.
    }
    procedure Open(const Data: Pointer; const DataSize: Integer);
  end;

  { Description:
      This is the basic indexable interface. All indexable objects must
      support this interface. It is responsible for handing out more
      raw text to the indexing engine. Note that any formatting or
      special characters that isn't part of the "text" of the document must
      be stripped out. This means that a HTML indexable would have to strip
      out all tags and leave the text.

      Note: An object that supports this interface must have a "brother object"
        that supports the IIndexableFactory before it can be registered with
        the indexing engine.
      Note: An object that supports this interface should also support one or
        more of the IIndexableFile, IIndexableTStream, IIndexableIStream,
        IIndexableString or IIndexableMemory interfaces to allow for opening
        various types of data storages.
  }
  IIndexable = interface
    ['{E1A78984-872A-11D5-B249-0004761A6377}']

    { Description:
        This method will be called periodically to retrieve more text from the
        indexable document. The text should be raw and contain no
        character-based formatting codes, like HTML, PDF or RTF.
      Parameters:
        Buffer      - Store the text you want to return into this parameter.
          You can use the BufferSize parameter to find out how much text you
          got room for. You don't need to provide complete words at the
          end of the text as the indexer will not index on a per-buffer basis,
          but will read the text as one large stream.
        BufferSize  - The size of the buffer. You can fill up to this amount of
          text into the Buffer parameter.
        TextSize    - In this parameter you return how much text the method
          was able to store in the Buffer parameter.
    }
    function GetMoreText(var Buffer; const BufferSize: Integer; out TextSize: Integer): Boolean;
  end;

  { Description:
      This interface represents an object that can produce an indexable
      object. Indexables can typically be heavy objects, but the factory
      should not be, so when you register an indexable with the indexing
      engine, you'll register a factory and delay creation of indexables.
    See also:
      IIndexable, IIndexablesFactory
  }
  IIndexableFactory = interface
    ['{E1A78989-872A-11D5-B249-0004761A6377}']

    { Description:
        This method will create an instance of whatever kind of indexable
        object this factory comes with.
      Parameters:
        Indexable - Upon return of this method, this parameter will contain
          a reference to a newly created indexable object.
    }
    procedure CreateIndexable(out Indexable: IIndexable);
  end;

  { Description:
      This interface represents a collection of indexable types. You typically
      register all the file types you can index along with the indexable
      factories that represents those extensions. Later on, you can request
      an indexable object for a particular file extension and the object that
      supports this interface will provide one for you, if one has been
      registered.
    See also:
      IIndexableFactory, IIndexable
  }
  IIndexablesFactory = interface
    ['{E1A7898A-872A-11D5-B249-0004761A6377}']

    { Description:
        This method registers a file extension along with the indexable
        factory that can provide indexable objects for that extension.
      Parameters:
        Extension - The file extension to register an indexable factory for.
          Specify the file extension with the . in front of it. If you
          specify more than just the file extension (ie. a full filename), only
          the extension of the name will be used.
        IndexableFactory - The factory that will provide indexable objects
          for the given extension.
      See also:
        CreateIndexable@string@IIndexable
    }
    procedure RegisterIndexableFactory(const Extension: string; const IndexableFactory: IIndexableFactory);

    { Description:
        This method will create an instance of an indexable object that can
        handle the given file extension.
      Parameters:
        Extension - The file extension to provide an indexable object for.
          Specify the file extension with the . in front of it. If you
          specify more than just the file extension (ie. a full filename), only
          the extension of the name will be used.
        Indexable - Upon return from this method, this parameter will contain
          a reference to an indexable object that can handle the given file
          extension.
      See also:
        RegisterIndexableFactory
    }
    procedure CreateIndexable(const Extension: string; out Indexable: IIndexable);
  end;

  { Description:
      This interface is used to wrap an indexer object. The indexer object
      is responsible for indexing indexables and produce a binary file with the
      contents of the index. This file can then later be opened to look up
      words or expressions.
    See also:
      IIndex
  }
  IIndexer = interface
    ['{E1A78980-872A-11D5-B249-0004761A6377}']

    { Description:
        This method adds the given indexable into the index. Each document
        is given a reference ID (basically just an ever-increasing number)
        that is unique.
      Parameters:
        Indexable - The object that is to be indexed.
        RefID - Output parameter that will contain the reference ID for the
          given indexable once it has been indexed.
      See also:
        Save
    }
    procedure Index(const Indexable: IIndexable; out RefID: UInt32);

    { Description:
        These methods allows the application to associate data with a document
        that can be retrieved later, such as the filename, url for document
        and other information.
      Parameters:
        RefID - The document id to associate the data with.
        Name - The name of the data block to add, each block must have a unique
          name.
        Data - The data to associate with the document.
      See also:
        AssociateData@UInt32@string, AssociatedData@UInt32@@Integer
    }
    procedure AssociateData(const RefID: UInt32; const Data: ISafeMem); overload;

    { Description:
        These methods allows the application to associate data with a document
        that can be retrieved later, such as the filename, url for document
        and other information.
      Parameters:
        RefID - The document id to associate the data with.
        Name - The name of the data block to add, each block must have a unique
          name.
        Data - The data to associate with the document.
      See also:
        AssociateData@UInt32@ISafeMem, AssociatedData@UInt32@@Integer
    }
    procedure AssociateData(const RefID: UInt32; const Data: string); overload;

    { Description:
        These methods allows the application to associate data with a document
        that can be retrieved later, such as the filename, url for document
        and other information.
      Parameters:
        RefID - The document id to associate the data with.
        Name - The name of the data block to add, each block must have a unique
          name.
        Data - The data to associate with the document.
        Size - The size of the data.
      See also:
        AssociateData@UInt32@ISafeMem, AssociatedData@UInt32@string
    }
    procedure AssociateData(const RefID: UInt32; const Data; const Size: Integer); overload;

    { Description:
        This method saves a binary copy of the index to a file with the
        given name. You can later open this file with an object that supports
        the IIndex interface to lookup words and expressions in the index.
      Parameters:
        Filename - The full path and name of the file to save the index to.
      See also:
        Index@IIndexable@UInt32
    }
    procedure Save(const Filename: string);
  end;

  { Description:
      This interface wraps an object that is responsible for holding a list
      of documents. Basically you use the RefID value returned with the
      IIndexer.Index@IIndexable@UInt32 method as an index and you're told
      if the document is in this list or not. This object is typically
      returned from one of the methods that looks up words or expressions in
      the index. After it has done so, it returns an IDocumentList interface
      which you can use to get a list of what documents that were found to
      match.
    See also:
      IIndex.Lookup@string@IDocumentList,
      IIndex.Query@string@IDocumentList
  }
  IDocumentList = interface
    ['{2A1A75F1-A5D6-11D5-B292-0004761A6377}']

    // <COMBINE DocumentCount>
    function GetDocumentCount: Integer;
    { Description:
        This property returns the number of documents potentially available in
        the list. You will typically use this as the upper bound of a loop
        to run through all documents, and then use the DocumentPresent
        property to check each one.
      See also:
        DocumentPresent
    }
    property DocumentCount: Integer read GetDocumentCount;

    // <COMBINE DocumentPresent>
    function GetDocumentPresent(const Index: Integer): Boolean;
    { Description:
        This property returns wether a given document is in the list or not.
        The index value is 0-based, and ranges from 0 to DocumentCount-1.
      See also:
        DocumentCount
    }
    property DocumentPresent[const Index: Integer]: Boolean read GetDocumentPresent; default;
  end;

  { Description:
      This interface wraps an object that an open a index file and do
      lookups and queries against the index. The file is produced by an
      object that supports the IIndexer interface.
    See also:
      IIndexer, IIndexInformation
  }
  IIndex = interface
    ['{E1A78986-872A-11D5-B249-0004761A6377}']

    { Description:
        This method opens up the index file with the given name.
      Parameters:
        Filename - The full path and name of the index file to open.
      See also:
        Close
    }
    procedure Open(const Filename: string);

    { Description:
        This method closes the index file. You don't have to call this method
        unless you want to free up memory temporarily.
      See also:
        Open@string
    }
    procedure Close;

    { Description:
        This method looks up a single word in the index, and returns a
        IDocumentList interface wrapping a list of matching documents.
      Parameters:
        Word - The single word to look up.
        DocumentList - Output parameter that will contain an interface to an
          object that contains a list of matching documents.
      See also:
        Query
    }
    function Lookup(const Word: string; out DocumentList: IDocumentList): Boolean;

    { Description:
        This method queries the index for documents that match the given
        expression. The expression can contain single words combined with the
        operators AND, NOT and OR, including parenthesis. The method will
        return an IDocumentList interface wrapping a list of matching documents.
      Parameters:
        Expression - The expression to query for.
        DocumentList - Output parameter that will contain an interface to an
          object that contains a list of matching documents.
      See also:
        Lookup
    }
    function Query(const Expression: string; out DocumentList: IDocumentList): Boolean;

    { Description:
        This method returns associated data for a document, or nil if no
        associated data is stored for that document.
      Parameters:
        RefID - The ID for the document to retrieve data for.
    }
    function GetAssociatedData(const RefID: UInt32): ISafeMem;
  end;

  { Description:
      The object that supports the IIndex interface can also support this
      interface to allow for more information from the index.
    See also:
      IIndexer, IIndex
  }
  IIndexInformation = interface
    ['{6ACDCE8E-3BB8-4114-B7B0-63251D28FA47}']

    // <COMBINE WordCount>
    function GetWordCount: UInt32;
    { Description:
        This property simply returns how many unique words that is stored in
        the index.
      See also:
        DocumentCount, Words
    }
    property WordCount: UInt32 read GetWordCount;

    // <COMBINE DocumentCount>
    function GetDocumentCount: UInt32;
    { Description:
        This property simply returns how many documents that has been
        indexed into the index.
      See also:
        WordCount, Words
    }
    property DocumentCount: UInt32 read GetDocumentCount;

    // <COMBINE Words>
    function GetWords(const Index: UInt32): string;
    { Description:
        This property gives access to the individual words stored in the index.
        The Index parameter is 0-based and ranges from 0 to WordCount-1.
      Parameters:
        Index - The index of the word to retrieve, from 0 to WordCount-1.
      See also:
        WordCount, DocumentCount
    }
    property Words[const Index: UInt32]: string read GetWords; default;

    // <COMBINE DocumentPresent>
    function GetDocumentPresent(const Index: UInt32): Boolean;
    { Description:
        If you delete documents from an index, it will still occupy an index
        id. Use the DocumentPresent method to check if a given document
        id is still referring to a valid document (ie. not deleted).
      Parameters:
        Index - The index of the document to check.
    }
    property DocumentPresent[const Index: UInt32]: Boolean
      read GetDocumentPresent;
  end;

  { Description:
      Used by IIndexMaintenance.Pack@string@string@TDocumentIDArray
  }
  TDocumentIDArray = array of UInt32;

  { Description:
      This interface wraps an object that does maintenance on index
      files.
    See also:
      IIndexer, IIndex
  }
  IIndexMaintenance = interface
    ['{ED6A6B3E-E308-484C-AF9C-2DA7F3A09D17}']

    { Description:
        This method combines all the index files given into one, bigger,
        index. The document id's that each index contains will be concatenated.
        This means that if index A contains documents 0-9, index B contains
        0-9 and index C contains 0-9, producing a single index from these,
        in that order, will produce a single index with the following
        characteristics:

          * Number of documents in index: 30
          * Index id's 0-9, the ones that came from index A
          * Index id's 10-19, the ones that came from index B
          * Index id's 20-29, the ones that came from index C
      Parameters:
        InputFilenames - An open array of names of index files to be combined.
          The combining will happen in the order given, as specified above.
        OutputFilename - The name of the resulting index file to produce,
          which will contain the information from the input index files
          combined into one index.
      See also:
        Delete
    }
    procedure Combine(const InputFilenames: array of string; const OutputFilename: string);

    { Description:
        This method will read a single index file, delete a number of documents
        from it and write out a single index file minus the given documents.
        Words that no longer appear in any documents will be removed as well.
      Parameters:
        InputFilename - The full path and name of the index file to read from.
        OutputFilename - The full path and name of the index file to write to.
        DocumentIDs - An open array of document id's to delete.
      See also:
        Combine
    }
    procedure Delete(const InputFilename, OutputFilename: string; const DocumentIDs: array of Integer);

    { Description:
        This method will pack the index by removing all information about
        deleted documents. This means re-ordering the document id's for
        present documents so that they again start at 0 and span an unbroken
        interval up to the highest document id. The pack method will:

          1. Re-order present documents to start from 0, increasing by 1
          2. Remove words only present in deleted documents
          3. Return an array in which each array item refers contains the
             new document id after packing, where INVALID_DOCUMENT_ID means the
             document was deleted.
      Parameters:
        InputFilename - The name of the index file to pack.
        OutputFilename - The name of the file to store the packed index into.
        DocumentIDs - Resulting list of new document id's.
    }
    procedure Pack(const InputFilename, OutputFilename: string;
      out PackedIDs: TDocumentIDArray);
  end;

  // Exception to use as base class for other exceptions that will be raised
  // from within the lvkIndexer engine.
  ElvkIndexer = class(Exception);
  // Exception class that is used from the object that supports IIndexer
  EIndexer = class(ElvkIndexer);
  // Exception class that is used from the object that supports IIndex
  EIndex = class(ElvkIndexer);
  // Exception class raised when the signature is invalid
  EIndexSignature = class(ElvkIndexer);

var
  { Description:
      This variable holds a reference to an object that supports
      IIndexablesFactory, which can be used to retrieve indexable objects for
      the registered file extensions.
  }
  IndexablesFactory : IIndexablesFactory  = nil;

resourcestring
  ERR_NO_TEMP_DIRECTORY         = 'Unable to determine temporary directory';
  ERR_NO_TEMP_FILENAME          = 'Unable to generate temporary filename';
  ERR_NO_TEMP_FILE              = 'Unable to create temporary file';
  ERR_NO_MAPPING                = 'Unable to create mapping of temporary file';
  ERR_NO_VIEW                   = 'Unable to map view of temporary file';
  ERR_UNABLE_TO_OPEN_INDEX_FILE	= 'Unable to open index file';
  ERR_UNABLE_TO_MAP_INDEX_FILE	= 'Unable to create mapping of index file';
  ERR_UNABLE_TO_MAP_INTO_MEMORY	= 'Unable to map index into memory';
  ERR_NOT_A_VALID_INDEX					= 'File does not contain a valid index';

{ Description:
    Units trying to register indexable classes should call this procedure first
    to make sure the necessary data structures have been created.
}
procedure PrepareIndexer;

{ Description:
    This function returns a newly created indexer object to use for indexing
    documents.
  See also:
    NewIndex, NewIndexMaintenance
}
function NewIndexer: IIndexer;

{ Description:
    This function returns a newly created index object to use for quering
    an index file.
  See also:
    NewIndexer, NewIndexMaintenance
}
function NewIndex: IIndex;

{ Description:
    This function returns a newly created index maintenance object.
  See also:
    NewIndexer, NewIndex
}
function NewIndexMaintenance: IIndexMaintenance;

{ Description:
    Checks wether the file exists, and if it looks to be a valid index file.
    The file is opened and the signature checked. If the signature matches,
    True is returned, otherwise False is returned.
  Parameters:
    FileName - The full path and name of the file to check.
}
function IsIndexFile(const FileName: string): Boolean;

{ Description:
    This function creates and returns a new instance of an indexable
    document for the given filename. An exception will be raised if no
    matching indexable document type can be located.
  Parameters:
    FileName - The name of the file to open.
  See also:
    NewStringIndexable
}
function NewFileIndexable(const FileName: string): IIndexable;

{ Description:
    This function creates and returns a new instance of an indexable
    document for the given text based upon the extension given. An exception
    will be raised if no matching indexable document type can be located.
  Parameters:
    s - The string to open for indexing.
    Extension - The extension to use when locating the document type.
  See also:
    -
}
function NewStringIndexable(const s: string; Extension: string='.txt'): IIndexable;

implementation

uses
  Windows, lvkBasicIndexable, lvkRegExp;

type
  TIndexablesFactory = class(TInterfacedObject, IIndexablesFactory)
  private
    FExtensions : TStringList;
    FFactories  : TInterfaceList;

  protected
    // IIndexablesFactory interface
    procedure RegisterIndexableFactory(const Extension: string; const IndexableFactory: IIndexableFactory);
    procedure CreateIndexable(const Extension: string; out Indexable: IIndexable);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRefIDs = array[0..3] of UInt32;
  PDocBlock = ^TDocBlock;
  TDocBlock = packed record
    RefIDs  : TRefIDs;
    Next    : UInt32;
  end;

const
  DocBlockSize  = 8*SizeOf(TRefIDs);

type
  PListNode = ^TListNode;
  TListNode = packed record
    Ch                : Char;
    WordStop          : Boolean;
    DocBlock          : UInt32;
    Left, Right, Same : UInt32;
  end;

  TWordVisitMethod = procedure(const Word: PChar; const WordLength: Integer; const DocBlock: UInt32) of object;

  TWordList = class
  private
    FFileHandle         : THandle;
    FFileMappingHandle  : THandle;
    FSize               : UInt32;
    FUsedSize           : UInt32;
    FWordList           : PChar;

    FCount              : Integer;

    procedure MapFile;
    procedure UnmapFile;
    procedure GrowFile;

  public
    constructor Create(const InitialSize: UInt32);
    destructor Destroy; override;

    function Add(const Word: PChar; const WordLength: Integer): UInt32;
    procedure SetDocBit(const WordOffset: UInt32; const RefID: UInt32);
    procedure Traverse(const WordVisitMethod: TWordVisitMethod; const Data: Pointer);
    property Count: Integer read FCount;

    function GetDocBlock(const BlockOffset: UInt32): PDocBlock;
  end;

  TAssociatedDataFormat = (adfText, adfBinary);
  TIndexer = class(TInterfacedObject, IIndexer)
  private
    FGlobalWordList       : TWordList;
    FAssociatedData       : ISafeMem;
    FAssociatedDataIndex  : array of Integer;
    FNextDocumentID       : Integer;
    FCaseSensitive        : Boolean;

    FCurrentSaveStream    : TStream;
    FCurrentSavePosition  : UInt32;

    procedure InternalAssociateData(const RefID: UInt32;
      const Data; const Size: Integer);

  protected
    // IIndexer interface
    procedure Index(const Indexable: IIndexable; out RefID: UInt32);
    procedure Save(const Filename: string);
    procedure AssociateData(const RefID: Cardinal; const Data: ISafeMem); overload;
    procedure AssociateData(const RefID: Cardinal; const Data; const Size: Integer); overload;
    procedure AssociateData(const RefID: Cardinal; const Data: string); overload;

    // Internal
    procedure ExtractWords(const Indexable: IIndexable; const WordList: TWordList);
    procedure AddWordToGlobal(const Word: PChar; const WordLength: Integer; const DocBlock: UInt32);
    procedure SaveWord(const Word: PChar; const WordLength: Integer; const DocBlock: UInt32);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TReadWordFlag = (rwiWord, rwiDocumentList);
  TReadWordInclude = set of TReadWordFlag;

  IInternalIndex = interface
    ['{AA44BC6A-B2D1-494F-81C7-153874A72A80}']

    function IsActive(const Index: Integer): Boolean;
  end;

  TIndex = class(TInterfacedObject, IIndex, IIndexInformation, IInternalIndex)
  private
    FFileHandle         : THandle;
    FFileMappingHandle  : THandle;
    FIndexPtr           : PChar;

  protected
    // IIndex interface
    procedure Open(const Filename: string);
    procedure Close;
    function Lookup(const Word: string; out DocumentList: IDocumentList): Boolean;
    function Query(const Expression: string; out DocumentList: IDocumentList): Boolean;
    function GetAssociatedData(const RefID: UInt32): ISafeMem;

    // IIndexInformation interface
    function GetWordCount: UInt32;
    function GetDocumentCount: UInt32;
    function GetWords(const Index: UInt32): string;
    function GetDocumentPresent(const Index: UInt32): Boolean;

    // IInternalIndex interface
    function IsActive(const Index: Integer): Boolean;

    // Internal
    procedure ReadWordInformation(const WordIndex: UInt32;
      const Include: TReadWordInclude;
      out Word: string;
      out DocumentList: IDocumentList);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  IInternalDocumentList = interface
    ['{D117894F-5341-4667-949A-F0FA7353B6EC}']

    procedure Invert;
    procedure CombineAnd(const OtherIndex: IDocumentList);
    procedure CombineOr(const OtherIndex: IDocumentList);
    function GetDocumentBlock: PChar;
    function AnyPresent: Boolean;
  end;

  TDocumentList = class(TInterfacedObject, IDocumentList, IInternalDocumentList)
  private
    FIndex              : IInternalIndex;
    FDocumentCount      : Integer;
    FDocumentBlock      : PChar;
    FDocumentBlockSize  : Integer;
    FOutside            : Boolean;

  protected
    // IDocumentList interface
    function GetDocumentCount: Integer;
    function GetDocumentPresent(const Index: Integer): Boolean;

    // IInternalDocumentList interface
    procedure Invert;
    procedure CombineAnd(const OtherIndex: IDocumentList);
    procedure CombineOr(const OtherIndex: IDocumentList);
    function GetDocumentBlock: PChar;
    function AnyPresent: Boolean;

    // Internal
    procedure ExpandBlock;

  public
    constructor Create(const DocumentCount, BlockSize: Integer; const Block: PChar; const Index: IInternalIndex);
    constructor CreateAll(const DocumentCount: Integer;
      const Index: IInternalIndex);
    destructor Destroy; override;
  end;

  TIndexMaintenance = class(TInterfacedObject, IIndexMaintenance)
  protected
    // IIndexMaintenance interface
    procedure Combine(const InputFilenames: array of string; const OutputFilename: string);
    procedure Delete(const InputFilename, OutputFilename: string; const DocumentIDs: array of Integer);
    procedure Pack(const InputFilename, OutputFilename: string;
      out PackedIDs: TDocumentIDArray);
  end;

  TIndexHeader = packed record
    Signature             : array[0..3] of Char;
    DocCount              : UInt32;
    AssociatedDataOffset  : Int32;
    WordCount             : UInt32;
  end;

const
  DEFAULT_GLOBAL_WORDLIST_SIZE    = 512*1024;
  DEFAULT_DOCUMENT_WORDLIST_SIZE  = 64*1024;
  DEFAULT_GROWTH                  = 1.5;
  DEFAULT_BUFFER_SIZE             = 64*1024;
  MAX_WORD_LENGTH                 = 64;

  { TODO 2 -oLVK -cSource : Add extended characters here }
  INITIAL_WORD_CHARACTERS         = ['A'..'Z', 'a'..'z', '_'];
  SUBSEQUENT_WORD_CHARACTERS      = INITIAL_WORD_CHARACTERS +
                                    ['0'..'9'];

  SIGNATURE : array[0..3] of Char = 'IDX'#26;

var
  UppercaseTranslate  : array[Char] of Char;

procedure PrepareTranslationTables;
var
  Ch  : Char;
  Ch2 : array[0..1] of Char;
begin
  Ch2[1] := #0;

  for Ch := #0 to #255 do
  begin
    Ch2[0] := Ch;
    CharUpper(Ch2);

    UppercaseTranslate[Ch] := Ch2[0];
  end;
end;

procedure PrepareIndexer;
begin
  if not Assigned(IndexablesFactory) then
  begin
    PrepareTranslationTables;
    IndexablesFactory := TIndexablesFactory.Create as IIndexablesFactory;
  end;
end;

function NewIndexer: IIndexer;
begin
  Result := TIndexer.Create;
end;

function NewIndex: IIndex;
begin
  Result := TIndex.Create;
end;

function NewIndexMaintenance: IIndexMaintenance;
begin
  Result := TIndexMaintenance.Create;
end;

function IsIndexFile(const FileName: string): Boolean;
var
  Index : IIndex;
begin
  Index := NewIndex;
  try
    Index.Open(FileName);
    Result := True;
  except
    Result := False;
  end;
end;

function NewFileIndexable(const FileName: string): IIndexable;
begin
  IndexablesFactory.CreateIndexable(ExtractFileExt(FileName), Result);
  (Result as IIndexableFile).Open(FileName);
end;

function NewStringIndexable(const s: string; Extension: string='.txt'): IIndexable;
begin
  IndexablesFactory.CreateIndexable(Extension, Result);
  (Result as IIndexableString).Open(s);
end;

{ TIndexablesFactory }

constructor TIndexablesFactory.Create;
begin
  inherited Create;

  FExtensions := TStringList.Create;
  FExtensions.Sorted := True;
  FFactories := TInterfaceList.Create;
end;

procedure TIndexablesFactory.CreateIndexable(const Extension: string;
  out Indexable: IIndexable);
var
  Index : Integer;
  Ext   : string;
begin
  Assert(Extension <> '');

  Ext := UpperCase(ExtractFileExt(Extension));
  Index := FExtensions.IndexOf(Ext);

  if Index >= 0 then
    (FFactories[Index] as IIndexableFactory).CreateIndexable(Indexable)
  else
    raise EIndexer.CreateFmt('No indexable factory available for the extension %s', [Ext]);
end;

destructor TIndexablesFactory.Destroy;
begin
  FFactories.Free;
  FExtensions.Free;

  inherited;
end;

procedure TIndexablesFactory.RegisterIndexableFactory(
  const Extension: string; const IndexableFactory: IIndexableFactory);
var
  Index : Integer;
  Ext   : string;
begin
  Assert(Extension <> '');
  Assert(Assigned(IndexableFactory));

  Ext := UpperCase(ExtractFileExt(Extension));
  Index := FExtensions.IndexOf(Ext);

  if Index >= 0 then
  begin
    // This removes previous factory for this extension
    FFactories[Index] := IndexableFactory;
  end else begin
    // Register a new extension and factory
    Index := FExtensions.Add(Ext);
    FFactories.Insert(Index, IndexableFactory);
  end;
end;

{ TIndexer }

procedure TIndexer.AddWordToGlobal(const Word: PChar;
  const WordLength: Integer; const DocBlock: UInt32);
var
  WordOffset  : UInt32;
begin
  WordOffset := FGlobalWordList.Add(Word, WordLength);
  FGlobalWordList.SetDocBit(WordOffset, FNextDocumentID);
end;

procedure TIndexer.AssociateData(const RefID: Cardinal; const Data: ISafeMem);
begin
  Assert(Assigned(Data));

  InternalAssociateData(RefID, Data.Pointer^, Data.Size);
end;

procedure TIndexer.AssociateData(const RefID: Cardinal; const Data;
  const Size: Integer);
begin
  Assert(@Data <> nil);
  Assert(Size > 0);

  InternalAssociateData(RefID, Data, Size);
end;

procedure TIndexer.AssociateData(const RefID: Cardinal;
  const Data: string);
begin
  Assert(Data <> '');

  InternalAssociateData(RefID, PChar(Data)^, Length(Data));
end;

constructor TIndexer.Create;
begin
  inherited Create;

  FGlobalWordList := TWordList.Create(DEFAULT_GLOBAL_WORDLIST_SIZE);
  FAssociatedData := AllocateSafeMem(0, True);
  FNextDocumentID := 0;
end;

destructor TIndexer.Destroy;
begin
  FAssociatedData := nil;
  FGlobalWordList.Free;

  inherited;
end;

procedure TIndexer.ExtractWords(const Indexable: IIndexable;
  const WordList: TWordList);
var
  Buffer      : PChar;
  TextSize    : Integer;

  CurrentWord : array[0..MAX_WORD_LENGTH-1] of Char;
  WordIndex   : Integer;
  Index       : Integer;
  Ch          : Char;
  AddToWord   : Boolean;
begin
  GetMem(Buffer, DEFAULT_BUFFER_SIZE);
  try
    WordIndex := 0;

    while Indexable.GetMoreText(Buffer^, DEFAULT_BUFFER_SIZE, TextSize) do
    begin
      for Index := 0 to TextSize-1 do
      begin
        Ch := Buffer[Index];

        if WordIndex = 0 then
          AddToWord := (Ch in INITIAL_WORD_CHARACTERS)
        else
          AddToWord := (Ch in SUBSEQUENT_WORD_CHARACTERS);

        if AddToWord then
        begin
          if FCaseSensitive then
            CurrentWord[WordIndex] := Ch
          else
            CurrentWord[WordIndex] := UppercaseTranslate[Ch];

          Inc(WordIndex);
          if WordIndex = MAX_WORD_LENGTH then
          begin
            WordList.Add(CurrentWord, WordIndex);
            WordIndex := 0;
          end;
        end else begin
          if WordIndex > 2 then
            WordList.Add(CurrentWord, WordIndex);
          WordIndex := 0;
        end;
      end;
    end;

    if WordIndex > 2 then
      WordList.Add(CurrentWord, WordIndex);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TIndexer.Index(const Indexable: IIndexable; out RefID: UInt32);
var
  DocumentWordList  : TWordList;
begin
  Assert(Assigned(Indexable));
  RefID := FNextDocumentID;

  DocumentWordList := TWordList.Create(DEFAULT_DOCUMENT_WORDLIST_SIZE);
  try
    ExtractWords(Indexable, DocumentWordList);
    DocumentWordList.Traverse(AddWordToGlobal, nil);
  finally
    DocumentWordList.Free;
  end;

  Inc(FNextDocumentID);
end;

procedure TIndexer.InternalAssociateData(const RefID: UInt32;
  const Data; const Size: Integer);
var
  OldLength     : UInt32;
  NewLength     : UInt32;
  OldPosition   : Integer;
begin
  NewLength := Length(FAssociatedDataIndex);
  while RefID >= NewLength do
  begin
    if NewLength = 0 then
      NewLength := 256
    else
      NewLength := NewLength * 2;
  end;

  if NewLength > UInt32(Length(FAssociatedDataIndex)) then
  begin
    OldLength := Length(FAssociatedDataIndex);
    SetLength(FAssociatedDataIndex, NewLength);
    while OldLength < NewLength do
    begin
      FAssociatedDataIndex[OldLength] := -1;
      Inc(OldLength);
    end;
  end;

  if FAssociatedDataIndex[RefID] >= 0 then
    raise ElvkIndexer.Create('Unable to associate data with document, document already has associated data');

  FAssociatedDataIndex[RefID] := FAssociatedData.Stream.Position;
  FAssociatedData.Stream.WriteBuffer(Size, SizeOf(Size));
  FAssociatedData.Stream.WriteBuffer(Data, Size);

  if FAssociatedData.Stream.Size - FAssociatedData.Stream.Position < 1024 then
  begin
    OldPosition := FAssociatedData.Stream.Position;
    FAssociatedData.Grow(16384);
    FAssociatedData.Stream.Position := OldPosition;
  end;
end;

procedure TIndexer.Save(const Filename: string);
var
  TempUInt32          : UInt32;
  Index               : Integer;
  ActiveList          : PChar;
  AssociatedDataSize  : Integer;
begin
  try
    FCurrentSaveStream := TFileStream.Create(Filename, fmCreate);
    try
      FCurrentSaveStream.WriteBuffer(SIGNATURE, SizeOf(SIGNATURE));

      TempUInt32 := FNextDocumentID;
      FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

      TempUInt32 := 0;
      FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

      TempUInt32 := FGlobalWordList.Count;
      FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

      GetMem(ActiveList, FNextDocumentID);
      try
        FillChar(ActiveList^, FNextDocumentID, #1);
        FCurrentSaveStream.WriteBuffer(ActiveList^, FNextDocumentID);
      finally
        FreeMem(ActiveList);
      end;

      FCurrentSavePosition := FCurrentSaveStream.Position;
      for Index := 1 to TempUInt32 do
        FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

      FGlobalWordList.Traverse(SaveWord, Pointer(FCurrentSaveStream));

      FCurrentSaveStream.Position := SizeOf(SIGNATURE) + SizeOf(TempUInt32);
      TempUInt32 := FCurrentSaveStream.Size;
      FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

      FCurrentSaveStream.Position := FCurrentSaveStream.Size;
      AssociatedDataSize := FAssociatedData.Stream.Position;
      FAssociatedData.Stream.Position := 0;
      FCurrentSaveStream.WriteBuffer(FAssociatedDataIndex[0], SizeOf(UInt32) * FNextDocumentID);
      FCurrentSaveStream.CopyFrom(FAssociatedData.Stream, AssociatedDataSize);
    finally
      FCurrentSaveStream.Free;
    end;
  except
    DeleteFile(PChar(Filename));
    raise;
  end;
end;

procedure TIndexer.SaveWord(const Word: PChar; const WordLength: Integer;
  const DocBlock: UInt32);
var
  Position      : UInt32;
  TempStream    : TMemoryStream;
  BlockOffset   : UInt32;
  BlockPtr      : PDocBlock;

  TempUInt32  : UInt32;
  Index, Last   : UInt32;
  StartIndex    : UInt32;
  ChPtr         : PChar;
begin
  FCurrentSaveStream.Position := FCurrentSavePosition;
  TempUInt32 := FCurrentSaveStream.Size;
  FCurrentSaveStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));
  FCurrentSavePosition := FCurrentSaveStream.Position;

  TempStream := TMemoryStream.Create;
  try
    TempStream.WriteBuffer(WordLength, SizeOf(WordLength));
    TempStream.WriteBuffer(Word^, WordLength);
    Position := TempStream.Size;
    TempUInt32 := 0;
    TempStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

    BlockOffset := DocBlock;
    Assert(BlockOffset > 0);

    StartIndex := TempStream.Size;
    while BlockOffset > 0 do
    begin
      BlockPtr := FGlobalWordList.GetDocBlock(BlockOffset);
      TempStream.WriteBuffer(BlockPtr^.RefIDs, SizeOf(BlockPtr^.RefIDs));

      BlockOffset := BlockPtr^.Next;
      Inc(TempUInt32, Length(BlockPtr^.RefIDs));
    end;

    Index := StartIndex;
    ChPtr := PChar(TempStream.Memory) + Index;
    Last := Index;
    {$WARNINGS OFF}
    while Index < TempStream.Size do
    begin
      if ChPtr^ <> #0 then
        Last := Index;

      Inc(Index);
      Inc(ChPtr);
    end;
    {$WARNINGS ON}

    TempUInt32 := Last - StartIndex + 1;
    TempStream.Position := Position;
    TempStream.WriteBuffer(TempUInt32, SizeOf(TempUInt32));

    FCurrentSaveStream.Position := FCurrentSaveStream.Size;
    FCurrentSaveStream.WriteBuffer(TempStream.Memory^, Last + 1);
  finally
    TempStream.Free;
  end;
end;

{ TWordList }

function TWordList.Add(const Word: PChar; const WordLength: Integer): UInt32;
var
  LastNode  : UInt32;

  function NewNode: UInt32;
  begin
    Result := FUsedSize;
    Inc(FUsedSize, SizeOf(TListNode));

    if FUsedSize >= FSize then
      GrowFile;
  end;

  function AddRest(const Index: Integer): UInt32;
  var
    NodePtr   : PListNode;
    NewOffset : UInt32;
  begin
    if Index >= WordLength then
      Result := 0
    else begin
      Result := NewNode;
      LastNode := Result;

      NodePtr := PListNode(FWordList + Result);
      NodePtr^.Ch := Word[Index];
      NodePtr^.WordStop := (Index = WordLength-1);
      if NodePtr^.WordStop then
        Inc(FCount);
      NodePtr^.Left := 0;
      NodePtr^.Right := 0;
      NewOffset := AddRest(Index+1);
      NodePtr := PListNode(FWordList + Result);
      NodePtr^.Same := NewOffset;
    end;
  end;

  procedure PlaceWord(Index: Integer);
  var
    NodeOffset  : UInt32;
    NewOffset   : UInt32;
    NodePtr     : PListNode;
    WordPtr     : PChar;
    Ch          : Char;
  begin
    NodeOffset := 0;
    WordPtr := @Word[Index];
    repeat
      LastNode := NodeOffset;
      NodePtr := PListNode(FWordList + NodeOffset);

      Ch := WordPtr^;
      if Ch < NodePtr^.Ch then
      begin
        if NodePtr^.Left = 0 then
        begin
          NewOffset := AddRest(Index);
          NodePtr := PListNode(FWordList + NodeOffset);
          NodePtr^.Left := NewOffset;
          Exit;
        end else
          NodeOffset := NodePtr^.Left;
      end else if Ch > NodePtr^.Ch then
      begin
        if NodePtr^.Right = 0 then
        begin
          NewOffset := AddRest(Index);
          NodePtr := PListNode(FWordList + NodeOffset);
          NodePtr^.Right := NewOffset;
          Exit;
        end else
          NodeOffset := NodePtr^.Right;
      end else begin
        if NodePtr^.Same = 0 then
        begin
          NewOffset := AddRest(Index+1);
          NodePtr := PListNode(FWordList + NodeOffset);
          NodePtr^.Same := NewOffset;
          Exit;
        end;

        NodeOffset := NodePtr^.Same;
        Inc(Index);
        Inc(WordPtr);
      end;
    until Index >= WordLength;

    if not NodePtr^.WordStop then
    begin
      NodePtr^.WordStop := True;
      Inc(FCount);
    end;
  end;

begin
  Assert((Word <> '') and (WordLength > 0));

  if FCount = 0 then
    AddRest(0)
  else
    PlaceWord(0);

  Result := LastNode;
end;

constructor TWordList.Create(const InitialSize: UInt32);
var
  TempPath  : array[0..MAX_PATH] of Char;
  TempName  : array[0..MAX_PATH] of Char;
  rc        : Integer;
begin
  rc := GetTempPath(SizeOf(TempPath), TempPath);
  if rc = 0 then
    raise EIndexer.Create(ERR_NO_TEMP_DIRECTORY);

  rc := GetTempFileName(TempPath, nil, 0, TempName);
  if rc = 0 then
    raise EIndexer.Create(ERR_NO_TEMP_FILENAME);

  FFileHandle := CreateFile(TempName, GENERIC_READ or GENERIC_WRITE, 0, NIL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL or
    FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_DELETE_ON_CLOSE, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EIndexer.Create(ERR_NO_TEMP_FILE);

  FSize := InitialSize;
  FUsedSize := 0;
  MapFile;

  FCount := 0;
end;

destructor TWordList.Destroy;
begin
  UnmapFile;
  CloseHandle(FFileHandle);

  inherited;
end;

function TWordList.GetDocBlock(const BlockOffset: UInt32): PDocBlock;
begin
  Result := PDocBlock(FWordList + BlockOffset);
end;

procedure TWordList.GrowFile;
begin
  UnmapFile;
  try
    FSize := Trunc(FSize * DEFAULT_GROWTH);
  finally
    MapFile;
  end;
end;

procedure TWordList.MapFile;
begin
  // Adjust size to match even number of 4kb pages
  FSize := (FSize div 4096 + UInt32(Ord(FSize mod 4096 > 0)))*4096;

  // Create file mapping
  FFileMappingHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, 0, FSize, nil);
  if FFileMappingHandle = 0 then
    raise EIndexer.Create(ERR_NO_MAPPING);

  // Map view of file
  FWordList := MapViewOfFile(FFileMappingHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if FWordList = nil then
    raise EIndexer.Create(ERR_NO_VIEW);
end;

procedure TWordList.SetDocBit(const WordOffset, RefID: UInt32);
var
  NodePtr     : PListNode;
  BlockOffset : UInt32;
  NewOffset   : UInt32;
  BlockPtr    : PDocBlock;
  UseID       : UInt32;

  function NewDocBlock: UInt32;
  var
    BlockPtr  : PDocBlock;
  begin
    Result := FUsedSize;
    Inc(FUsedSize, SizeOf(TDocBlock));

    if FUsedSize >= FSize then
      GrowFile;

    BlockPtr := PDocBlock(FWordList + Result);
    BlockPtr^.Next := 0;
    BlockPtr^.RefIDs[0] := 0;
    BlockPtr^.RefIDs[1] := 0;
    BlockPtr^.RefIDs[2] := 0;
    BlockPtr^.RefIDs[3] := 0;
  end;

begin
  NodePtr := PListNode(FWordList + WordOffset);

  if NodePtr^.DocBlock = 0 then
  begin
    NewOffset := NewDocBlock;
    NodePtr := PListNode(FWordList + WordOffset);
    NodePtr^.DocBlock := NewOffset;
  end;

  BlockOffset := NodePtr^.DocBlock;

  UseID := RefID;
  while UseID >= DocBlockSize do
  begin
    BlockPtr := PDocBlock(FWordList + BlockOffset);

    if BlockPtr^.Next = 0 then
    begin
      NewOffset := NewDocBlock;
      BlockPtr := PDocBlock(FWordList + BlockOffset);
      BlockPtr^.Next := NewOffset;
    end;

    BlockOffset := BlockPtr^.Next;
    Dec(UseID, DocBlockSize);
  end;

  BlockPtr := PDocBlock(FWordList + BlockOffset);
  BlockPtr^.RefIDs[UseID div 32] := BlockPtr^.RefIDs[UseID div 32] or (1 shl (UseID mod 32));
end;

procedure TWordList.Traverse(const WordVisitMethod: TWordVisitMethod; const Data: Pointer);
var
  CurrentWord : array[0..MAX_WORD_LENGTH] of Char;

  procedure TraverseWord(const WordOffset: UInt32; const CurrentIndex: Integer);
  var
    NodePtr : PListNode;
  begin
    NodePtr := PListNode(FWordList + WordOffset);

    if NodePtr^.Left > 0 then
      TraverseWord(NodePtr^.Left, CurrentIndex);
    if NodePtr^.WordStop then
    begin
      CurrentWord[CurrentIndex] := NodePtr^.Ch;
      CurrentWord[CurrentIndex+1] := #0;
      WordVisitMethod(CurrentWord, CurrentIndex+1, NodePtr^.DocBlock);
    end;
    if NodePtr^.Same > 0 then
    begin
      CurrentWord[CurrentIndex] := NodePtr^.Ch;
      TraverseWord(NodePtr^.Same, CurrentIndex+1);
    end;
    if NodePtr^.Right > 0 then
      TraverseWord(NodePtr^.Right, CurrentIndex);
  end;

begin
  TraverseWord(0, 0);
end;

procedure TWordList.UnmapFile;
begin
  UnmapViewOfFile(FWordList);
  FWordList := nil;
  CloseHandle(FFileMappingHandle);
  FFileMappingHandle := INVALID_HANDLE_VALUE;
end;

{ TIndex }

procedure TIndex.Close;
var
  rc  : Boolean;
begin
  if FIndexPtr <> nil then
  begin
    rc := UnmapViewOfFile(FIndexPtr);
    Assert(rc);
    FIndexPtr := nil;
  end;
  if FFileMappingHandle <> 0 then
  begin
    rc := CloseHandle(FFileMappingHandle);
    Assert(rc);
    FFileMappingHandle := 0;
  end;
  if FFileHandle <> INVALID_HANDLE_VALUE then
  begin
    rc := CloseHandle(FFileHandle);
    Assert(rc);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;
end;

constructor TIndex.Create;
begin
  inherited Create;

  FFileHandle := INVALID_HANDLE_VALUE;
  FFileMappingHandle := 0;
  FIndexPtr := nil;
end;

destructor TIndex.Destroy;
begin
  Close;
  
  inherited;
end;

function TIndex.GetAssociatedData(const RefID: UInt32): ISafeMem;
var
  DocumentCount : UInt32;
  DataOffset    : Int32;
  DataOffset2   : Int32;
  DataSize      : Int32;
begin
  Move((FIndexPtr + SizeOf(SIGNATURE))^, DocumentCount, SizeOf(DocumentCount));
  Assert(RefID < DocumentCount);

  Move((FIndexPtr + SizeOf(SIGNATURE) + SizeOf(UInt32))^, DataOffset, SizeOf(DataOffset));
  Move((FIndexPtr + DataOffset + RefID * SizeOf(UInt32))^, DataOffset2, SizeOf(DataOffset2));

  if DataOffset2 < 0 then
    Result := nil
  else begin
    Move((FIndexPtr + DataOffset + SizeOf(UInt32) * DocumentCount + DataOffset2)^, DataSize, SizeOf(DataSize));
    Assert((DataSize >= 0) and (DataSize < 2000000000));
    Result := AllocateSafeMem(DataSize);
    Move((FIndexPtr + DataOffset + SizeOf(UInt32) * DocumentCount + DataOffset2 + SizeOf(DataSize))^, Result.Pointer^, DataSize);
  end;
end;

function TIndex.GetDocumentCount: UInt32;
begin
  Assert(Assigned(FIndexPtr));

  Move((FIndexPtr + SizeOf(SIGNATURE))^, Result, SizeOf(Result));
end;

function TIndex.GetDocumentPresent(const Index: UInt32): Boolean;
begin
  Result := IsActive(Index);
end;

function TIndex.GetWordCount: UInt32;
begin
  Assert(Assigned(FIndexPtr));

  Move((FIndexPtr + SizeOf(SIGNATURE) + 2 * SizeOf(UInt32))^, Result, SizeOf(Result));
end;

function TIndex.GetWords(const Index: UInt32): string;
var
  DummyList : IDocumentList;
begin
  Assert(Index < GetWordCount);
  Assert(Assigned(FIndexPtr));

  ReadWordInformation(Index, [rwiWord], Result, DummyList);
end;

function TIndex.IsActive(const Index: Integer): Boolean;
var
  DocumentCount : Integer;
begin
  Assert(Assigned(FIndexPtr));

  Move((FIndexPtr + SizeOf(SIGNATURE))^, DocumentCount, SizeOf(DocumentCount));
  if (Index >= 0) and (Index < DocumentCount) then
    Move((FIndexPtr + SizeOf(SIGNATURE) + 3*SizeOf(UInt32) + Index)^, Result, 1)
  else
    Result := False;
end;

function TIndex.Lookup(const Word: string;
  out DocumentList: IDocumentList): Boolean;
var
  i1, i2    : Integer;
  im        : Integer;
  TempWord  : string;
  TempList  : IDocumentList;
begin
  i1 := 0;
  i2 := GetWordCount-1;
  im := 0;

  while i1 <= i2 do
  begin
    im := (i1 + i2) div 2;

    ReadWordInformation(im, [rwiWord], TempWord, TempList);
    if TempWord < Word then
      i1 := im+1
    else if TempWord > Word then
      i2 := im-1
    else
      Break;
  end;

  if i1 <= i2 then
  begin
    ReadWordInformation(im, [rwiWord, rwiDocumentList], TempWord, DocumentList);
    Result := True;
  end else begin
    DocumentList := nil;
    Result := False;
  end;
end;

procedure TIndex.Open(const Filename: string);
begin
  Close;
  
  FFileHandle := CreateFile(PChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EIndex.Create(ERR_UNABLE_TO_OPEN_INDEX_FILE);

  FFileMappingHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, 0, 0, nil);
  if FFileMappingHandle = 0 then
    raise EIndex.Create(ERR_UNABLE_TO_MAP_INDEX_FILE);

  FIndexPtr := MapViewOfFile(FFileMappingHandle, FILE_MAP_READ, 0, 0, 0);
  if FIndexPtr = nil then
    raise EIndex.Create(ERR_UNABLE_TO_MAP_INTO_MEMORY);

  if not CompareMem(FIndexPtr, @SIGNATURE, SizeOf(Signature)) then
    raise EIndexSignature.Create(ERR_NOT_A_VALID_INDEX);
end;

function TIndex.Query(const Expression: string;
  out DocumentList: IDocumentList): Boolean;
type
  TPartType = (ptWord, ptOperator);
  TPart = record
    PartType  : TPartType;
    Level     : Integer;
    Text      : string;
  end;
var
  Parts : array of TPart;

  procedure Split(const Expression: string);
  var
    CurrentWord   : array[0..MAX_WORD_LENGTH] of Char;
    CurrentIndex  : Integer;
    Index         : Integer;
    Level         : Integer;

    procedure AddPart(const PartType: TPartType; const Text: string);
    var
      Index : Integer;
    begin
      Index := Length(Parts);
      SetLength(Parts, Index+1);

      Parts[Index].Level := Level;
      if PartType = ptWord then
      begin
        if CompareText(Text, 'OR') = 0 then
        begin
          Parts[Index].PartType := ptOperator;
          Parts[Index].Text := '+';
        end else if CompareText(Text, 'AND') = 0 then
        begin
          Parts[Index].PartType := ptOperator;
          Parts[Index].Text := '*';
        end else if CompareText(Text, 'NOT') = 0 then
        begin
          Parts[Index].PartType := ptOperator;
          Parts[Index].Text := '!';
        end else begin
          Parts[Index].PartType := ptWord;
          Parts[Index].Text := Text;
        end;
      end else begin
        Parts[Index].PartType := PartType;
        Parts[Index].Text := Text;
      end;
    end;

    procedure FinishCurrentWord;
    begin
      if CurrentIndex > 0 then
      begin
        CurrentWord[CurrentIndex] := #0;
        AddPart(ptWord, CurrentWord);
      end;
      CurrentIndex := 0;
    end;

  begin
    SetLength(Parts, 0);
    Level := 0;

    Index := 1;
    CurrentIndex := 0;

    while Index <= Length(Expression) do
    begin
      case Expression[Index] of
        #32, #9, #10, #13:
          FinishCurrentWord;

        '(':
          begin
            FinishCurrentWord;
            Inc(Level);
            AddPart(ptOperator, '(');
          end;

        ')':
          begin
            FinishCurrentWord;
            AddPart(ptOperator, ')');
            Dec(Level);
          end;

      else
        CurrentWord[CurrentIndex] := Expression[Index];
        Inc(CurrentIndex);
        if CurrentIndex >= High(CurrentWord) then
          raise EIndex.CreateFmt('Too long a word in query (max is %d characters)', [High(CurrentWord)-1]);
      end;

      Inc(Index);
    end;

    if CurrentIndex > 0 then
    begin
      CurrentWord[CurrentIndex] := #0;
      AddPart(ptWord, CurrentWord);
    end;
  end;

  function Evaluate(const Level, i1, i2: Integer): IDocumentList;
  var
    Index : Integer;
    Point : Integer;
    Pri   : Integer;

    function PriFrom(const Part: TPart): Integer;
    begin
      if Part.Text = '+' then
        Result := 1
      else if Part.Text = '*' then
        Result := 2
      else if Part.Text = '!' then
        Result := 3
      else
        Result := 4;
    end;

    function OrOperator(const Level, i1, i2, Point: Integer): IDocumentList;
    var
      List1, List2  : IDocumentList;
    begin
      List1 := Evaluate(Level, i1, Point-1);
      List2 := Evaluate(Level, Point+1, i2);

      if Assigned(List1) and Assigned(List2) then
      begin
        (List1 as IInternalDocumentList).CombineOr(List2);
        Result := List1;
      end else if Assigned(List1) then
        Result := List1
      else if Assigned(List2) then
        Result := List2
      else
        Result := TDocumentList.CreateAll(GetDocumentCount, Self as IInternalIndex) as IDocumentList;
    end;

    function AndOperator(const Level, i1, i2, Point: Integer): IDocumentList;
    var
      List1, List2  : IDocumentList;
    begin
      List1 := Evaluate(Level, i1, Point-1);
      List2 := Evaluate(Level, Point+1, i2);

      if Assigned(List1) and Assigned(List2) then
      begin
        (List1 as IInternalDocumentList).CombineAnd(List2);
        Result := List1;
      end else
        Result := nil;
    end;

    function NotOperator(const Level, i1, i2, Point: Integer): IDocumentList;
    begin
      Assert(Point=i1);

      Result := Evaluate(Level, i1+1, i2);
      if Assigned(Result) then
        (Result as IInternalDocumentList).Invert
      else begin
        Result := nil;
      end;
    end;

    function Parenthesis(const Level, i1, i2, Point: Integer): IDocumentList;
    var
      Index : Integer;
    begin
      Assert(Point=i1);
      for Index := i1+1 to i2-1 do
        if Parts[Index].Level <= Level then
          raise Exception.Create('Syntax error in query expression');

      Result := Evaluate(Level+1, i1+1, i2-1);
    end;

    procedure EvaluateWildCard(const Text: string; out Result: IDocumentList);
    var
      WordIndex   : Integer;
      Word        : string;
      Pattern     : string;
      re          : IRegExp;
      NewList     : IDocumentList;
      LowerIndex  : Integer;
      UpperIndex  : Integer;

      function LocateLetter(const Letter: Char; const Direction: Integer): Integer;
      var
        TempLetter  : Char;
        Index1      : Integer;
        Index2      : Integer;
        IndexMiddle : Integer;
        Word        : string;
        WordCount   : Integer;
      begin
        TempLetter := UpCase(Letter);
        Index1 := 0;
        Index2 := GetWordCount-1;

        repeat
          IndexMiddle := (Index1 + Index2) div 2;
          Word := GetWords(IndexMiddle);
          if Word[1] = TempLetter then
          begin
            Index1 := IndexMiddle;
            Break;
          end else if Word[1] < Letter then
            Index1 := IndexMiddle + 1
          else
            Index2 := IndexMiddle - 1;
        until Index1 > Index2;
        Result := Index1;

        if Direction = -1 then
        begin
          while (Result > 0) and (GetWords(Result-1)[1] = TempLetter) do
            Dec(Result);
        end else begin
          WordCount := GetWordCount;
          while (Result < WordCount) and (GetWords(Result+1)[1] = TempLetter) do
            Inc(Result);
        end;
      end;

    begin
      Pattern := '^';
      for WordIndex := 1 to Length(Text) do
        case Text[WordIndex] of
          '*':
            Pattern := Pattern + '.*';
          '?':
            Pattern := Pattern + '.';

        else
          Pattern := Pattern + Text[WordIndex];
        end;
      Pattern := Pattern + '$';
      re := NewRegExp(Pattern);

      if Pattern[2] = '.' then
      begin
        LowerIndex := 0;
        UpperIndex := GetWordCount-1;
      end else begin
        LowerIndex := LocateLetter(Pattern[2], -1);
        UpperIndex := LocateLetter(Pattern[2], +1);
      end;

      Result := nil;
      for WordIndex := LowerIndex to UpperIndex do
      begin
        Word := GetWords(WordIndex);

        if re.MatchAgainst(Word) then
        begin
          Lookup(Word, NewList);
          if Assigned(Result) then
          begin
            (Result as IInternalDocumentList).CombineOr(NewList);
          end else
            Result := NewList
        end;
      end;
    end;

  begin
    Point := -1;
    Pri := 0;

    for Index := i1 to i2 do
    begin
      if (Parts[Index].Level = Level) and (Parts[Index].PartType = ptOperator) then
      begin
        if (Parts[Index].Text <> '(') and (Parts[Index].Text <> ')') then
        begin
          if Point = -1 then
          begin
            Point := Index;
            Pri := PriFrom(Parts[Point]);
          end else begin
            if PriFrom(Parts[Index]) < Pri then
            begin
              Point := Index;
              Pri := PriFrom(Parts[Point]);
            end;
          end;
        end;
      end;
    end;

    if Point = -1 then
    begin
      if (Parts[i1].PartType = ptOperator) and (Parts[i1].Text = '(') and
        (Parts[i2].PartType = ptOperator) and (Parts[i2].Text = ')') then
      begin
        Point := i1;
        Result := Parenthesis(Level, i1, i2, Point)
      end else begin
        Assert(i1=i2);
        if Pos('*', Parts[i1].Text) + Pos('?', Parts[i1].Text) > 0 then
          EvaluateWildCard(Parts[i1].Text, Result)
        else
          Lookup(Parts[i1].Text, Result);
      end;
    end else begin
      if Parts[Point].Text = '+' then
        Result := OrOperator(Level, i1, i2, Point)
      else if Parts[Point].Text = '*' then
        Result := AndOperator(Level, i1, i2, Point)
      else if Parts[Point].Text = '!' then
        Result := NotOperator(Level, i1, i2, Point)
      else if Parts[Point].Text = '(' then
        Result := Parenthesis(Level, i1, i2, Point);
    end;
  end;

begin
  Split(Expression);
  DocumentList := Evaluate(0, Low(Parts), High(Parts));
  if Assigned(DocumentList) then
    if not (DocumentList as IInternalDocumentList).AnyPresent then
      DocumentList := nil;

  Result := Assigned(DocumentList);
end;

procedure TIndex.ReadWordInformation(const WordIndex: UInt32;
  const Include: TReadWordInclude; out Word: string;
  out DocumentList: IDocumentList);
var
  WordOffset          : UInt32;
  WordLength          : UInt32;
  DocumentListLength  : UInt32;
  TempBlock           : PChar;
  DocumentCount       : UInt32;
begin
  Word := '';
  DocumentList := nil;

  Move((FIndexPtr + SizeOf(SIGNATURE))^, DocumentCount, SizeOf(DocumentCount));
  Move((FIndexPtr + SizeOf(SIGNATURE) + DocumentCount + 3*SizeOf(UInt32) + WordIndex * SizeOf(UInt32))^,
    WordOffset, SizeOf(WordOffset));
  Move((FIndexPtr + WordOffset)^, WordLength, SizeOf(WordLength));
  Move((FIndexPtr + WordOffset + SizeOf(WordLength) + WordLength)^, DocumentListLength, SizeOf(DocumentListLength));

  if rwiWord in Include then
  begin
    SetLength(Word, WordLength);
    if WordLength > 0 then
      Move((FIndexPtr + WordOffset + SizeOf(WordLength))^, Word[1], WordLength);
  end;

  if rwiDocumentList in Include then
  begin
    GetMem(TempBlock, DocumentListLength);
    try
      Move((FIndexPtr + WordOffset + SizeOf(WordLength) + WordLength + SizeOf(DocumentListLength))^,
        TempBlock^, DocumentListLength);
      DocumentList := TDocumentList.Create(GetDocumentCount, DocumentListLength, TempBlock, Self as IInternalIndex) as IDocumentList;
    finally
      FreeMem(TempBlock);
    end;
  end;
end;

{ TDocumentList }

function TDocumentList.AnyPresent: Boolean;
var
  Index : Integer;
begin
  Result := False;
  for Index := 0 to FDocumentBlockSize-1 do
    if FDocumentBlock[Index] <> #0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TDocumentList.CombineAnd(const OtherIndex: IDocumentList);
var
  p1, p2  : PChar;
  Index   : Integer;
begin
  ExpandBlock;
  p1 := FDocumentBlock;
  p2 := (OtherIndex as IInternalDocumentList).GetDocumentBlock;
  for Index := 0 to FDocumentBlockSize-1 do
  begin
    p1^ := Char(Ord(p1^) and Ord(p2^));
    Inc(p1);
    Inc(p2);
  end;
end;

procedure TDocumentList.CombineOr(const OtherIndex: IDocumentList);
var
  p1, p2  : PChar;
  Index   : Integer;
begin
  ExpandBlock;
  p1 := FDocumentBlock;
  p2 := (OtherIndex as IInternalDocumentList).GetDocumentBlock;
  for Index := 0 to FDocumentBlockSize-1 do
  begin
    p1^ := Char(Ord(p1^) or Ord(p2^));
    Inc(p1);
    Inc(p2);
  end;
end;

constructor TDocumentList.Create(const DocumentCount, BlockSize: Integer;
  const Block: PChar; const Index: IInternalIndex);
begin
  Assert(BlockSize > 0);
  Assert(DocumentCount > 0);
  Assert(Assigned(Block));
  Assert(Assigned(Index));

  inherited Create;

  FOutside := False;
  FDocumentCount := DocumentCount;
  FDocumentBlockSize := BlockSize;
  FIndex := Index;
  GetMem(FDocumentBlock, BlockSize);
  Move(Block^, FDocumentBlock^, BlockSize);
end;

constructor TDocumentList.CreateAll(const DocumentCount: Integer;
  const Index: IInternalIndex);
begin
  Assert(DocumentCount > 0);

  inherited Create;

  FOutside := True;
  FDocumentCount := DocumentCount;
  FDocumentBlockSize := (DocumentCount div 8)+1;
  GetMem(FDocumentBlock, FDocumentBlockSize);
  FillChar(FDocumentBlock^, FDocumentBlockSize, #255);
  FIndex := Index;
end;

destructor TDocumentList.Destroy;
begin
  FreeMem(FDocumentBlock);
  
  inherited;
end;

procedure TDocumentList.ExpandBlock;
var
  NeededBlockSize : Integer;
  NewBlock        : PChar;
begin
  NeededBlockSize := (FDocumentCount div 8) + Ord(FDocumentCount mod 8 > 0);
  if FDocumentBlockSize < NeededBlockSize then
  begin
    GetMem(NewBlock, NeededBlockSize);
    try
      FillChar(NewBlock^, NeededBlockSize, #0);
      Move(FDocumentBlock^, NewBlock^, FDocumentBlockSize);
    except
      FreeMem(NewBlock);
      raise;
    end;
    FreeMem(FDocumentBlock);
    FDocumentBlockSize := NeededBlockSize;
    FDocumentBlock := NewBlock;
  end;
end;

function TDocumentList.GetDocumentBlock: PChar;
begin
  ExpandBlock;
  Result := FDocumentBlock;
end;

function TDocumentList.GetDocumentCount: Integer;
begin
  Result := FDocumentCount;
end;

function TDocumentList.GetDocumentPresent(const Index: Integer): Boolean;
var
  b : Byte;
begin
  if (Index div 8) < FDocumentBlockSize then
  begin
    if FIndex.IsActive(Index) then
    begin
      b := Ord(FDocumentBlock[Index div 8]);
      Result := (b and (1 shl (Index mod 8))) <> 0;
    end else
      Result := False;
  end else
    Result := FOutside;
end;

procedure TDocumentList.Invert;
var
  Index : Integer;
begin
  ExpandBlock;
  FOutside := not FOutside;
  for Index := 0 to FDocumentBlockSize-1 do
    FDocumentBlock[Index] := Char(Ord(FDocumentBlock[Index]) xor 255);
end;

{ TIndexMaintenance }

procedure TIndexMaintenance.Combine(const InputFilenames: array of string;
  const OutputFilename: string);
var
  TempFilename1 : string;
  TempFilename2 : string;
  Index         : Integer;

  function NewTempFilename: string;
  var
    TempPath  : array[0..MAX_PATH] of Char;
    TempName  : array[0..MAX_PATH] of Char;
    rc        : Integer;
  begin
    rc := GetTempPath(SizeOf(TempPath), TempPath);
    if rc = 0 then
      raise EIndexer.Create(ERR_NO_TEMP_DIRECTORY);

    rc := GetTempFileName(TempPath, nil, 0, TempName);
    if rc = 0 then
      raise EIndexer.Create(ERR_NO_TEMP_FILENAME);

    Result := TempName;
  end;

  procedure CombineTwo(const InputFilename1, InputFilename2, OutputFilename: string);
  var
    InputFile1                        : THandle;
    InputMapping1                     : THandle;
    InputView1                        : PChar;

    InputFile2                        : THandle;
    InputMapping2                     : THandle;
    InputView2                        : PChar;

    OutputStream                      : TStream;

    DocCount1, DocCount2, DocCount    : UInt32;
    WordCount1, WordCount2, WordCount : UInt32;
    AssociatedDataPosition            : UInt32;

    procedure Header;
    begin
      OutputStream.WriteBuffer(SIGNATURE, SizeOf(SIGNATURE));

      // Calculate and write out new document count
      Move((InputView1 + SizeOf(SIGNATURE))^, DocCount1, SizeOf(DocCount1));
      Move((InputView2 + SizeOf(SIGNATURE))^, DocCount2, SizeOf(DocCount2));
      DocCount := DocCount1 + DocCount2;
      OutputStream.WriteBuffer(DocCount, SizeOf(DocCount));

      // Write out temporary associated data position
      AssociatedDataPosition := 0;
      OutputStream.WriteBuffer(AssociatedDataPosition, SizeOf(AssociatedDataPosition));

      // Calculate and write out new word count
      Move((InputView1 + SizeOf(SIGNATURE) + 2*SizeOf(UInt32))^, WordCount1, SizeOf(WordCount1));
      Move((InputView2 + SizeOf(SIGNATURE) + 2*SizeOf(UInt32))^, WordCount2, SizeOf(WordCount2));
      WordCount := 0;
      OutputStream.WriteBuffer(WordCount, SizeOf(WordCount));

      // Write out new active-list
      OutputStream.WriteBuffer((InputView1 + SizeOf(SIGNATURE) + 3*SizeOf(UInt32))^, DocCount1);
      OutputStream.WriteBuffer((InputView2 + SizeOf(SIGNATURE) + 3*SizeOf(UInt32))^, DocCount2);
    end;

    procedure UpdateHeader;
    begin
      OutputStream.Position := SizeOf(SIGNATURE) + SizeOf(UInt32);
      OutputStream.WriteBuffer(AssociatedDataPosition, SizeOf(AssociatedDataPosition));
      OutputStream.WriteBuffer(WordCount, SizeOf(WordCount));
      OutputStream.Position := OutputStream.Size;
    end;

    procedure CopyAndMergeWords;
    var
      Word1         : string;
      Word2         : string;
      Index1        : Integer;
      Index2        : Integer;
      BlockPos1     : PChar;
      BlockPos2     : PChar;
      Pass          : Integer;
      WordIndex     : Integer;
      WordPosition  : Integer;

      function GetWord(const View: PChar; const Index: Integer;
        out BlockPosition: PChar): string;
      var
        DocumentCount : Integer;
        WordOffset    : UInt32;
        WordLength    : UInt32;
      begin
        Move((View + SizeOf(SIGNATURE))^, DocumentCount, SizeOf(DocumentCount));
        Move((View + SizeOf(SIGNATURE) + DocumentCount + 3*SizeOf(UInt32) + Index * SizeOf(UInt32))^,
          WordOffset, SizeOf(WordOffset));
        Move((View + WordOffset)^, WordLength, SizeOf(WordLength));
        SetLength(Result, WordLength);
        if WordLength > 0 then
          Move((View + WordOffset + SizeOf(WordLength))^, Result[1], WordLength);
        BlockPosition := (View + WordOffset + SizeOf(WordLength) + WordLength);
      end;

      procedure ShiftRight(const Data: PChar; const Bytes, Bits: Integer);
      var
        Delta     : Integer;
        Start     : PChar;
        Index     : Integer;
        b1, b2    : Byte;
        BitDelta  : Integer;
      const
        Masks : array[1..7] of Byte = (128, 192, 224, 240, 248, 252, 254);
      begin
        Delta := Bits div 8;
        BitDelta := Bits mod 8;

        if Delta > 0 then
          Move(Data^, (Data + Delta)^, Bytes);
        Start := Data + Delta;

        if BitDelta > 0 then
        begin
          for Index := Bytes-1 downto 0 do
          begin
            b1 := Ord(Start[Index]);
            b2 := Ord(Start[Index+1]);

            {$R-}
            b2 := b2 or ((b1 and Masks[BitDelta]) shr (8-BitDelta));
            b1 := (b1 and 255-Masks[BitDelta]) shl BitDelta;
            {$R+}

            Start[Index] := Char(b1);
            Start[Index+1] := Char(b2);
          end;
        end;
      end;

      procedure NewWord(const Word: string);
      var
        WordPos   : UInt32;
        WordSize  : UInt32;
      begin
        Assert(Word <> '');
        
        WordPos := OutputStream.Size;
        OutputStream.Position := WordPosition + WordIndex * SizeOf(UInt32);
        OutputStream.WriteBuffer(WordPos, SizeOf(WordPos));

        OutputStream.Position := WordPos;
        WordSize := Length(Word);
        OutputStream.WriteBuffer(WordSize, SizeOf(WordSize));
        OutputStream.WriteBuffer(Word[1], Length(Word));

        Inc(WordIndex);
      end;

      procedure OutputWord(const Word: string; const BlockPosition: PChar;
        const Shift: Integer);
      var
        BlockSize : UInt32;
        NewSize   : Integer;
        Temp      : PChar;
      begin
        if Pass = 1 then
          Inc(WordCount)
        else begin
          NewWord(Word);
          if Shift = 0 then
          begin
            Move(BlockPosition^, BlockSize, SizeOf(BlockSize));
            OutputStream.WriteBuffer(BlockPosition^, BlockSize + SizeOf(BlockSize));
          end else begin
            Move(BlockPosition^, BlockSize, SizeOf(BlockSize));
            NewSize := Integer(BlockSize) + (Shift div 8) + Ord(Shift mod 8 > 0);
            GetMem(Temp, NewSize);
            try
              FillChar(Temp^, NewSize, #0);
              Move((BlockPosition + SizeOf(UInt32))^, Temp^, BlockSize);
              ShiftRight(Temp, BlockSize, Shift);

              while NewSize > 0 do
                if Temp[NewSize-1] = #0 then
                  Dec(NewSize)
                else
                  Break;

              BlockSize := NewSize;
              OutputStream.WriteBuffer(BlockSize, SizeOf(BlockSize));
              OutputStream.WriteBuffer(Temp^, BlockSize);
            finally
              FreeMem(Temp);
            end;
          end;
        end;
      end;

      procedure CombineWord(const Word: string; const BlockPosition1: PChar;
        const BlockPosition2: PChar; const Shift2: Integer);
      var
        BlockSize1    : UInt32;
        BlockSize2    : UInt32;
        Temp1, Temp2  : PChar;
        NewSize       : Integer;
        Index         : Integer;
      begin
        if Pass = 1 then
          Inc(WordCount)
        else begin
          NewWord(Word);
          Move(BlockPosition1^, BlockSize1, SizeOf(BlockSize1));
          Move(BlockPosition2^, BlockSize2, SizeOf(BlockSize2));

          NewSize := UInt32(Shift2) div 8 + BlockSize2 + 1;
          GetMem(Temp1, NewSize);
          try
            FillChar(Temp1^, NewSize, #0);
            Move((BlockPosition1 + SizeOf(BlockSize1))^, Temp1^, BlockSize1);

            GetMem(Temp2, NewSize);
            try
              FillChar(Temp2^, NewSize, #0);
              Move((BlockPosition2 + SizeOf(BlockSize2))^, Temp2^, BlockSize2);
              ShiftRight(Temp2, BlockSize2, Shift2);

              for Index := 0 to NewSize-1 do
                Temp1[Index] := Char(Ord(Temp1[Index]) or Ord(Temp2[Index]));

              while NewSize > 0 do
                if Temp1[NewSize-1] = #0 then
                  Dec(NewSize)
                else
                  Break;

              BlockSize1 := NewSize;
              OutputStream.WriteBuffer(BlockSize1, SizeOf(BlockSize1));
              OutputStream.WriteBuffer(Temp1^, BlockSize1);
            finally
              FreeMem(Temp2);
            end;
          finally
            FreeMem(Temp1);
          end;
        end;
      end;

    begin
      for Pass := 1 to 2 do
      begin
        Index1 := 0;
        Index2 := 0;
        Word1 := GetWord(InputView1, Index1, BlockPos1);
        Word2 := GetWord(InputView2, Index2, BlockPos2);
        WordIndex := 0;

        while (Index1 < Integer(WordCount1)) and (Index2 < Integer(WordCount2)) do
        begin
          if Word1 < Word2 then
          begin
            OutputWord(Word1, BlockPos1, 0);
            Inc(Index1);
            if Index1 < Integer(WordCount1) then
              Word1 := GetWord(InputView1, Index1, BlockPos1);
          end else if Word2 < Word1 then
          begin
            OutputWord(Word2, BlockPos2, DocCount1);
            Inc(Index2);
            if Index2 < Integer(WordCount2) then
              Word2 := GetWord(InputView2, Index2, BlockPos2);
          end else begin
            CombineWord(Word1, BlockPos1, BlockPos2, DocCount1);

            Inc(Index1);
            if Index1 < Integer(WordCount1) then
              Word1 := GetWord(InputView1, Index1, BlockPos1);

            Inc(Index2);
            if Index2 < Integer(WordCount2) then
              Word2 := GetWord(InputView2, Index2, BlockPos2);
          end;
        end;

        while Index1 < Integer(WordCount1) do
        begin
          OutputWord(Word1, BlockPos1, 0);
          Inc(Index1);
          if Index1 < Integer(WordCount1) then
            Word1 := GetWord(InputView1, Index1, BlockPos1);
        end;

        while Index2 < Integer(WordCount2) do
        begin
          OutputWord(Word2, BlockPos2, DocCount1);
          Inc(Index2);
          if Index2 < Integer(WordCount2) then
            Word2 := GetWord(InputView2, Index2, BlockPos2);
        end;

        if Pass = 1 then
        begin
          WordPosition := OutputStream.Size;
          for WordIndex := 0 to WordCount-1 do
            OutputStream.WriteBuffer(WordPosition, SizeOf(WordPosition));
        end;
      end;
    end;

    procedure CopyAssociatedInformation;
    var
      Index : Integer;
      Dummy : Integer;

      procedure CopyDataFor(const View: PChar; const DocCount, ViewRefID: Integer;
        const OutputRefID: UInt32);
      var
        ViewDataPos   : UInt32;
        DocDataPos    : Int32;
        OutputOffset  : Int32;
        DataPtr       : PChar;
        DataSize      : UInt32;
      begin
        Move((View + SizeOf(SIGNATURE) + SizeOf(UInt32))^, ViewDataPos, SizeOf(ViewDataPos));
        Move((View + ViewDataPos + ViewRefID * SizeOf(UInt32))^, DocDataPos, SizeOf(DocDataPos));

        if DocDataPos >= 0 then
        begin
          DataPtr := View + ViewDataPos + SizeOf(UInt32) * DocCount + DocDataPos;
          OutputOffset := UInt32(OutputStream.Size) - AssociatedDataPosition - (DocCount1 + DocCount2) * SizeOf(UInt32);
          OutputStream.Position := AssociatedDataPosition + OutputRefID * SizeOf(UInt32);
          OutputStream.WriteBuffer(OutputOffset, SizeOf(OutputOffset));

          OutputStream.Position := OutputStream.Size;

          Move(DataPtr^, DataSize, SizeOf(DataSize));
          OutputStream.WriteBuffer(DataPtr^, DataSize + SizeOf(DataSize));
        end;
      end;

    begin
      AssociatedDataPosition := OutputStream.Position;
      Dummy := -1;
      for Index := 1 to DocCount1 + DocCount2 do
        OutputStream.WriteBuffer(Dummy, SizeOf(Dummy));

      for Index := 0 to DocCount1-1 do
        CopyDataFor(InputView1, DocCount1, Index, Index);
      for Index := 0 to DocCount2-1 do
        CopyDataFor(InputView2, DocCount2, Index, DocCount1 + UInt32(Index));
    end;

  begin
    if not IsIndexFile(InputFilename1) then
      raise ElvkIndexer.CreateFmt('File %s is not a valid index file', [InputFilename1]);
    if not IsIndexFile(InputFilename2) then
      raise ElvkIndexer.CreateFmt('File %s is not a valid index file', [InputFilename2]);

    InputFile1 := INVALID_HANDLE_VALUE;
    InputFile2 := INVALID_HANDLE_VALUE;
    InputMapping1 := 0;
    InputMapping2 := 0;
    InputView1 := nil;
    InputView2 := nil;
    OutputStream := nil;
    try
      InputFile1 := CreateFile(PChar(InputFilename1), GENERIC_READ, 0, NIL,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
      if InputFile1 = INVALID_HANDLE_VALUE then
        raise EIndex.Create(ERR_UNABLE_TO_OPEN_INDEX_FILE);

      InputMapping1 := CreateFileMapping(InputFile1, nil, PAGE_READONLY, 0, 0, nil);
      if InputMapping1 = 0 then
        raise EIndex.Create(ERR_UNABLE_TO_MAP_INDEX_FILE);

      InputView1 := MapViewOfFile(InputMapping1, FILE_MAP_READ, 0, 0, 0);
      if InputView1 = nil then
        raise EIndex.Create(ERR_UNABLE_TO_MAP_INTO_MEMORY);

      Assert(CompareMem(InputView1, @SIGNATURE, SizeOf(Signature)));

      InputFile2 := CreateFile(PChar(InputFilename2), GENERIC_READ, 0, NIL,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
      if InputFile2 = INVALID_HANDLE_VALUE then
        raise EIndex.Create(ERR_UNABLE_TO_OPEN_INDEX_FILE);

      InputMapping2 := CreateFileMapping(InputFile2, nil, PAGE_READONLY, 0, 0, nil);
      if InputMapping2 = 0 then
        raise EIndex.Create(ERR_UNABLE_TO_MAP_INDEX_FILE);

      InputView2 := MapViewOfFile(InputMapping2, FILE_MAP_READ, 0, 0, 0);
      if InputView2 = nil then
        raise EIndex.Create(ERR_UNABLE_TO_MAP_INTO_MEMORY);

      Assert(CompareMem(InputView2, @SIGNATURE, SizeOf(Signature)));

      OutputStream := TFileStream.Create(OutputFilename, fmCreate);

      Header;
      CopyAndMergeWords;
      CopyAssociatedInformation;
      UpdateHeader;
    finally
      OutputStream.Free;

      if Assigned(InputView2) then
        UnmapViewOfFile(InputView2);
      if InputMapping2 <> 0 then
        CloseHandle(InputMapping2);
      if InputFile2 <> INVALID_HANDLE_VALUE then
        CloseHandle(InputFile2);

      if Assigned(InputView1) then
        UnmapViewOfFile(InputView1);
      if InputMapping1 <> 0 then
        CloseHandle(InputMapping1);
      if InputFile1 <> INVALID_HANDLE_VALUE then
        CloseHandle(InputFile1);
    end;
  end;

begin
  Assert(Length(InputFilenames) >= 2);

  if Length(InputFilenames) = 2 then
    CombineTwo(InputFilenames[0], InputFilenames[1], OutputFilename)
  else begin
    TempFilename1 := NewTempFilename;
    CombineTwo(InputFilenames[0], InputFilenames[1], TempFilename1);
    Index := 2;
    while Index <= High(InputFilenames) do
    begin
      if Index < High(InputFilenames) then
        TempFilename2 := NewTempFilename
      else
        TempFilename2 := OutputFilename;
      CombineTwo(TempFilename1, InputFilenames[Index], TempFilename2);
      DeleteFile(PChar(TempFilename1));

      TempFilename1 := TempFilename2;
      Inc(Index);
    end;
  end;
end;

procedure TIndexMaintenance.Delete(const InputFilename,
  OutputFilename: string; const DocumentIDs: array of Integer);
var
  InputStream   : TFileStream;
  OutputStream  : TStream;
  Header        : TIndexHeader;

  procedure CopyHeader;
  var
    ActiveList  : packed array of Byte;
    Index, ID   : Integer;
  begin
    InputStream.ReadBuffer(Header, SizeOf(Header));
    OutputStream.WriteBuffer(Header, SizeOf(Header));

    SetLength(ActiveList, Header.DocCount);
    if Header.DocCount > 0 then
      InputStream.ReadBuffer(ActiveList[0], Header.DocCount);

    for Index := Low(DocumentIDs) to High(DocumentIDs) do
    begin
      ID := DocumentIDs[Index];
      if (ID >= 0) and (ID < Length(ActiveList)) then
        ActiveList[ID] := 0
      else
        raise ElvkIndexer.Create('Document ID is out of bounds');
    end;

    if Header.DocCount > 0 then
      OutputStream.WriteBuffer(ActiveList[0], Header.DocCount);
  end;

  procedure CopyRest;
  begin
    OutputStream.CopyFrom(InputStream, InputStream.Size - InputStream.Position);
  end;

begin
  if not IsIndexFile(InputFilename) then
    raise ElvkIndexer.CreateFmt('File %s is not a valid index file', [InputFilename]);

  InputStream := nil;
  OutputStream := nil;
  try
    InputStream := TFileStream.Create(InputFilename, fmOpenRead or
      fmShareDenyWrite);
    OutputStream := TFileStream.Create(OutputFilename, fmCreate);

    try
      CopyHeader;
      CopyRest;
    except
      FreeAndNil(OutputStream);
      DeleteFile(PChar(OutputFilename));
      raise;
    end;
  finally
    OutputStream.Free;
    InputStream.Free;
  end;
end;

procedure TIndexMaintenance.Pack(const InputFilename,
  OutputFilename: string; out PackedIDs: TDocumentIDArray);
type
  TByteArray = packed array of Byte;
var
  InputStream             : TFileStream;
  OutputStream            : TStream;
  Header                  : TIndexHeader;
  NewWordCount            : Integer;
  NewDocCount             : Integer;
  NewAssociatedDataOffset : Integer;
  OldActiveList           : TByteArray;

  procedure CopyHeader;
  var
    Index1      : Integer;
    Index2      : Integer;
    ActiveList  : TByteArray;
  begin
    InputStream.ReadBuffer(Header, SizeOf(Header));
    OutputStream.WriteBuffer(Header, SizeOf(Header));

    SetLength(ActiveList, Header.DocCount);
    if Header.DocCount > 0 then
      InputStream.ReadBuffer(ActiveList[0], Header.DocCount);

    SetLength(PackedIDs, Header.DocCount);
    Index1 := 0;
    for Index2 := 0 to Length(ActiveList)-1 do
    begin
      if ActiveList[Index2] <> 0 then
      begin
        PackedIDs[Index2] := Index1;
        Inc(Index1);
      end else
        PackedIDs[Index2] := INVALID_DOCUMENT_ID;
    end;

    OldActiveList := ActiveList;
    
    NewDocCount := Index1;
    SetLength(ActiveList, NewDocCount);
    for Index2 := 0 to NewDocCount-1 do
      ActiveList[Index2] := 1;
    if NewDocCount > 0 then
      OutputStream.WriteBuffer(ActiveList[0], NewDocCount);
  end;

  procedure CopyAndPackWords;
  var
    TempList            : ISafeMem;
    TempWords           : ISafeMem;
    WordIndex           : Integer;
    DocumentIndex       : Integer;
    WordLength          : UInt32;
    Word                : string;
    DocumentListLength  : UInt32;
    NewDocumentList     : TByteArray;
    DocumentList        : TByteArray;
    Position            : Integer;
    BasePosition        : Integer;

    function IsOldDocumentPresent(const DocumentIndex: Integer): Boolean;
    var
      b : Byte;
    begin
      Assert((DocumentIndex >= 0) and (DocumentIndex div 8 < Length(DocumentList)));

      b := DocumentList[DocumentIndex div 8];
      Result := (b and (1 shl (DocumentIndex mod 8))) <> 0;
    end;

    procedure SetNewDocumentPresent(const DocumentIndex: Integer);
    var
      b : Byte;
    begin
      Assert((DocumentIndex >= 0) and (DocumentIndex div 8 < Length(NewDocumentList)));

      b := NewDocumentList[DocumentIndex div 8];
      b := b or (1 shl (DocumentIndex mod 8));
      NewDocumentList[DocumentIndex div 8] := b;
    end;

  begin
    TempWords := AllocateSafeMem(0, True);
    TempList := AllocateSafeMem(0, True);

    {$WARNINGS OFF}
    InputStream.Position := InputStream.Position + SizeOf(UInt32) *
      Header.WordCount;
    {$WARNINGS ON}
    NewWordCount := 0;
    for WordIndex := 0 to Header.WordCount-1 do
    begin
      // Read word
      InputStream.ReadBuffer(WordLength, SizeOf(WordLength));
      SetLength(Word, WordLength);
      if WordLength > 0 then
        InputStream.ReadBuffer(Word[1], WordLength);

      // Read document list
      InputStream.ReadBuffer(DocumentListLength, SizeOf(DocumentListLength));
      SetLength(DocumentList, DocumentListLength);
      if DocumentListLength > 0 then
        InputStream.ReadBuffer(DocumentList[0], DocumentListLength);

      // Shrink document list
      if DocumentListLength > 0 then
      begin
        SetLength(NewDocumentList, DocumentListLength);
        ZeroMemory(@NewDocumentList[0], DocumentListLength);
        for DocumentIndex := 0 to Length(PackedIDs)-1 do
        begin
          if PackedIDs[DocumentIndex] <> INVALID_DOCUMENT_ID then
            if IsOldDocumentPresent(DocumentIndex) then
              SetNewDocumentPresent(PackedIDs[DocumentIndex]);
        end;

        // Reduce length of new documents
        DocumentIndex := DocumentListLength;
        while (DocumentIndex > 0) and (NewDocumentList[DocumentIndex-1] = 0) do
          Dec(DocumentIndex);
        SetLength(NewDocumentList, DocumentIndex);
      end;

      if Length(NewDocumentList) > 0 then
      begin
        Position := TempWords.Stream.Position;
        TempList.Stream.WriteBuffer(Position, SizeOf(Position));

        TempWords.Stream.WriteBuffer(WordLength, SizeOf(WordLength));
        if WordLength > 0 then
          TempWords.Stream.WriteBuffer(Word[1], WordLength);
        DocumentListLength := Length(NewDocumentList);
        TempWords.Stream.WriteBuffer(DocumentListLength, SizeOf(DocumentListLength));
        if DocumentListLength > 0 then
          TempWords.Stream.WriteBuffer(NewDocumentList[0], DocumentListLength);
        Inc(NewWordCount);
      end;
    end;

    TempList.Stream.Position := 0;
    BasePosition := OutputStream.Position + NewWordCount * SizeOf(UInt32);
    for WordIndex := 0 to NewWordCount-1 do
    begin
      TempList.Stream.ReadBuffer(Position, SizeOf(Position));
      Inc(Position, BasePosition);
      OutputStream.WriteBuffer(Position, SizeOf(Position));
    end;

    OutputStream.WriteBuffer(TempWords.Pointer^, TempWords.Size);
  end;

  procedure CopyAndPackAssociatedData;
  var
    Dummy       : Int32;
    Index       : Int32;
    NewDocIndex : Int32;
    OldPosition : Int32;
    NewPosition : Int32;
    DataSize    : Int32;
  begin
    NewAssociatedDataOffset := OutputStream.Size;
    Dummy := -1;
    OutputStream.Position := OutputStream.Size;
    for Index := 0 to NewDocCount-1 do
      OutputStream.WriteBuffer(Dummy, SizeOf(Dummy));
    NewDocIndex := 0;
    for Index := 0 to Length(OldActiveList)-1 do
    begin
      if OldActiveList[Index] = 1 then
      begin
        InputStream.Position := Header.AssociatedDataOffset + Index * SizeOf(UInt32);
        InputStream.ReadBuffer(OldPosition, SizeOf(OldPosition));

        if OldPosition >= 0 then
        begin
          OutputStream.Position := NewAssociatedDataOffset + NewDocIndex * SizeOf(UInt32);
          NewPosition := OutputStream.Size - NewAssociatedDataOffset - SizeOf(UInt32) * NewDocCount;
          OutputStream.WriteBuffer(NewPosition, SizeOf(NewPosition));
          OutputStream.Position := OutputStream.Size;

          InputStream.Position := Header.AssociatedDataOffset + Int32(Header.DocCount) * SizeOf(UInt32) + OldPosition;
          InputStream.ReadBuffer(DataSize, SizeOf(DataSize));
          OutputStream.WriteBuffer(DataSize, SizeOf(DataSize));
          OutputStream.CopyFrom(InputStream, DataSize);

          Inc(NewDocIndex);
        end;
      end;
    end;
  end;

  procedure UpdateHeader;
  begin
    Header.DocCount := NewDocCount;
    Header.AssociatedDataOffset := NewAssociatedDataOffset;
    Header.WordCount := NewWordCount;

    OutputStream.Position := 0;
    OutputStream.WriteBuffer(Header, SizeOf(Header));
  end;

begin
  if not IsIndexFile(InputFilename) then
    raise ElvkIndexer.CreateFmt('File %s is not a valid index file', [InputFilename]);

  InputStream := nil;
  OutputStream := nil;
  try
    InputStream := TFileStream.Create(InputFilename, fmOpenRead or
      fmShareDenyWrite);
    OutputStream := TFileStream.Create(OutputFilename, fmCreate);

    try
      CopyHeader;
      CopyAndPackWords;
      CopyAndPackAssociatedData;
      UpdateHeader;
    except
      FreeAndNil(OutputStream);
      DeleteFile(PChar(OutputFilename));
      raise;
    end;
  finally
    OutputStream.Free;
    InputStream.Free;
  end;
end;

end.
