{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the base classes for indexables, TBasicIndexable.
  See also:
    TBasicIndexable
}
unit lvkBasicIndexable;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBasicIndexable.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkIndexer, AxCtrls, ActiveX;

type
  { Description:
      This is the base class for indexables. You can descend new indexables
      from this class.
    See also:
      TBasicIndexableFactory
  }
  TBasicIndexable = class(TInterfacedObject, IIndexable, IIndexableFile,
    IIndexableMemory, IIndexableTStream, IIndexableIStream, IIndexableString)
  private
    FStream     : TStream;
    FOwnsStream : Boolean;

  protected
    { Description:
        You override this method if you want to provide a specialized method
        of obtaining raw text. This method should strip out all non-text
        tokens, like html tags, pdf formatting, etc.
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
      See also:
        Stream
    }
    function GetMoreText(var Buffer; const BufferSize: Integer; out TextSize: Integer): Boolean;

    procedure IIndexableFile.Open = OpenFile;
    // <ALIAS IIndexableFile.Open@string>
    procedure OpenFile(const Filename: string);

    procedure IIndexableTStream.Open = OpenTStream;
    // <ALIAS IIndexableTStream.Open@TStream@Boolean>
    procedure OpenTStream(const Stream: TStream; const OwnsStream: Boolean);

    procedure IIndexableIStream.Open = OpenIStream;
    // <ALIAS IIndexableIStream.Open@IStream>
    procedure OpenIStream(const Stream: IStream);

    procedure IIndexableString.Open = OpenString;
    // <ALIAS IIndexableString.Open@string>
    procedure OpenString(const Text: string);

    procedure IIndexableMemory.Open = OpenMemory;
    // <ALIAS IIndexableMemory.Open@Pointer@Integer>
    procedure OpenMemory(const Data: Pointer; const DataSize: Integer);

    { Description:
        This method is called internally when the object is destroyed, and
        closes the stream that the object uses for its text source.
    }
    procedure CloseStream;

    { Description:
        This property is for use internally by descendant classes to reach the
        stream that was opened through one of the IIndexable* interfaces.
    }
    property Stream: TStream read FStream;

  public
    { Description:
        This virtual constructor should be overridden in descendant classes
        if you want specialized construction for a new indexable class.
      See also:
        Destroy
    }
    constructor Create; virtual;

    { Description:
        Closes the stream and destroys the indexable object instance.
      See also:
        Create
    }
    destructor Destroy; override;
  end;

  { Description:
      This class type is used in calls to
      TBasicIndexableFactory.Create@TBasicIndexableClass
    See also:
      TBasicIndexableFactory.Create@TBasicIndexableClass
  }
  TBasicIndexableClass = class of TBasicIndexable;

  { Description:
      This is the base class for the class factory for indexables. You must
      provide one such factory class for each indexable class you create,
      and register it with the indexer for a file extension.

      Look at the example for how to register a file extension.

      Note: You can use the TBasicIndexableFactory if your new indexable
        doesn't require any specialized construction.

    Example:
<code>
  IndexablesFactory.RegisterIndexableFactory('.TXT',
    TBasicIndexableFactory.Create(TBasicIndexable));
</code>
    See also:
      IndexablesFactory, TBasicIndexable
  }
  TBasicIndexableFactory = class(TInterfacedObject, IIndexableFactory)
  private
    FBasicIndexableClass  : TBasicIndexableClass;

  protected
    { Description:
        This method is responsible for creating an instance of the indexable
        class for which this class factory is responsible. Simply create
        an instance and return it in the Indexable output parameter.

        In TBasicIndexableFactory, this method simply creates an instance of
        the class type that it has a reference to from the initial call to
        the constructor. This means that any indexable classes that doesn't
        require any specialized construction can use the standard
        TBasicIndexableFactory class instead of providing a new factory
        class.
      Parameters:
        Indexable - Return the new indexable object instance in this
          parameter.
      See also:
        Create@TBasicIndexableClass, TBasicIndexable, IIndexable
    }
    procedure CreateIndexable(out Indexable: IIndexable);

  public
    { Description:
        This method creates an instance of the class factory and stores a
        reference to the indexable class type that it is responsible for.

        This reference is later used in calls to CreateIndexable
      Parameters:
        BasicIndexableClass -
      See also:
        CreateIndexable, TBasicIndexableClass
    }
    constructor Create(const BasicIndexableClass: TBasicIndexableClass);
  end;

implementation

{ TBasicIndexable }

procedure TBasicIndexable.CloseStream;
begin
  if FOwnsStream then
    FStream.Free;

  FStream := nil;
  FOwnsStream := False;
end;

constructor TBasicIndexable.Create;
begin
  inherited;

  FStream := nil;
  FOwnsStream := False;
end;

destructor TBasicIndexable.Destroy;
begin
  CloseStream;

  inherited;
end;

function TBasicIndexable.GetMoreText(var Buffer;
  const BufferSize: Integer; out TextSize: Integer): Boolean;
begin
  TextSize := Stream.Read(Buffer, BufferSize);
  Result := (TextSize > 0);
end;

procedure TBasicIndexable.OpenFile(const Filename: string);
begin
  Assert(Filename <> '');

  CloseStream;

  FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  FOwnsStream := True;
end;

procedure TBasicIndexable.OpenIStream(const Stream: IStream);
begin
  Assert(Assigned(Stream));

  CloseStream;

  FStream := TOleStream.Create(Stream);
  FOwnsStream := True;
end;

procedure TBasicIndexable.OpenMemory(const Data: Pointer;
  const DataSize: Integer);
begin
  Assert(Assigned(Data));
  Assert(DataSize > 0);

  CloseStream;

  FStream := TMemoryStream.Create;
  FOwnsStream := True;
  TMemoryStream(FStream).Size := DataSize;
  Move(Data^, TMemoryStream(FStream).Memory^, DataSize);
end;

procedure TBasicIndexable.OpenString(const Text: string);
begin
  CloseStream;

  FStream := TStringStream.Create(Text);
  FOwnsStream := True;
end;

procedure TBasicIndexable.OpenTStream(const Stream: TStream;
  const OwnsStream: Boolean);
begin
  Assert(Assigned(Stream));

  CloseStream;

  FStream := Stream;
  FOwnsStream := OwnsStream;
end;

{ TBasicIndexableFactory }

constructor TBasicIndexableFactory.Create(
  const BasicIndexableClass: TBasicIndexableClass);
begin
  inherited Create;

  FBasicIndexableClass := BasicIndexableClass;
end;

procedure TBasicIndexableFactory.CreateIndexable(
  out Indexable: IIndexable);
begin
  Indexable := FBasicIndexableClass.Create as IIndexable;
end;

initialization
  PrepareIndexer;
  IndexablesFactory.RegisterIndexableFactory('.TXT', TBasicIndexableFactory.Create(TBasicIndexable));
end.
