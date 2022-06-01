{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains all the core interfaces related to the stream filter
    classes and code.
}
unit lvkStreamFilters;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkStreamFilters.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, lvkVersion;

type
{$IFNDEF DELPHI6UP}
  { Description:
      This type matches the TSeekOrigin type found in the Classes unit in
      Delphi 6.
  }
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
{$ENDIF}

  { Description:
      This is the base stream filter interface. All stream filters must support
      this interface.
  }
  IStreamFilter = interface
    ['{213348B1-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        The stream filter class will call this method in order to establish
        the sequence of filters to use. When the sequence is to be broken
        (during cleanup), this method will be called again with a nil
        parameter.
      Parameters:
        Filter - The next filter in the sequence. Refers to an existing
          filter interface, or is nil to signal that the sequence is being
          destroyed.
    }
    procedure SetNextFilter(const Filter: IStreamFilter);

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;

  { Description:
      All stream filters that support writing must support this interface.
  }
  IWriteableStreamFilter = interface
    ['{213348B2-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        This method will be called to write a block of data to the
        filter. The filter will have to decide what to do with the data
        according to how the filter works.
      Parameters:
        Buffer - The binary data to write.
        Count - The number of bytes in Buffer.
      Returns:
        The function must return the number of bytes written, or 0 if no
        bytes could be written.
    }
    function Write(const Buffer; const Count: Longint): Longint;

    { Description:
        This method is called to flush the output buffer. It is mainly used
        before cleaning up and destroying the filters, to allow all
        filters to purge their output buffers and make sure they are written
        to the next filter.
    }
    procedure Flush;
  end;

  { Description:
      All stream filters that support reading must support this interface.
  }
  IReadableStreamFilter = interface
    ['{213348B3-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        This method will be called to read a block of data from the
        filter. The filter will have to decide how to get or generate the
        data according to how the filter works.
      Parameters:
        Buffer - The buffer to fill with data.
        Count - The number of bytes to read into Buffer.
      Returns:
        The function must return the number of bytes actually read into Buffer,
        or 0 if no bytes could be read. The function must not read more than
        the specified number of bytes as that could crash the
        application.
    }
    function Read(var Buffer; const Count: Longint): Longint;
  end;

  { Description:
      All stream filters that support repositioning must support this interface.
  }
  ISeekableStreamFilter = interface
    ['{213348B4-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        This method will be called both to obtain the current stream
        position as well as reposition within the stream.
      Parameters:
        Offset - The position to seek to, relative to Origin.
        Origin - Where to start the seeking. Can be one of the following
          values:
            * soFromBeginning - Seek to position 0 + Offset
            * soFromCurrent - Seek to the current position + Offset
            * soFromEnd - Seek to the end + Offset (negative means back into
              the stream)
    }
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;
  end;

  { Description:
      All stream filters that support resizing must support this interface.
  }
  ISizeableStreamFilter = interface
    ['{213348B5-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        This method will be called whenever the Size property of the
        stream class is changed. It has to resize the underlying stream to
        match the specified value.
      Parameters:
        NewSize - The new size of the stream.
    }
    procedure SetSize(const NewSize: Int64);
  end;

  { Description:
      This is the core interface for all compression filters.
  }
  ICompressionStreamFilter = interface
    ['{213348B6-8AF9-11D5-B24D-0004761A6377}']

    { Description:
        This returns the number of bytes that has been sent into the
        compression/decompression engine.
      See also:
        TotalInput, GetTotalOutput, TotalOutput
    }
    function GetTotalInput: Cardinal;

    { Description:
        This returns the number of bytes that has been sent into the
        compression/decompression engine.
      See also:
        GetTotalInput, GetTotalOutput, TotalOutput
    }
    property TotalInput: Cardinal read GetTotalInput;

    { Description:
        This returns the number of bytes that has been output from the
        compression/decompression engine.
      See also:
        GetTotalInput, TotalInput, TotalOutput
    }
    function GetTotalOutput: Cardinal;

    { Description:
        This returns the number of bytes that has been output from the
        compression/decompression engine.
      See also:
        GetTotalInput, TotalInput, GetTotalOutput
    }
    property TotalOutput: Cardinal read GetTotalOutput;

    { Description:
        This returns the compression ratio as a floating point value. A value
        of 1.0 means there has been no significant compression. A value of 0.5
        means that the original, uncompressed, data has been compressed down
        to roughly half their size.
      See also:
        CompressionRatio
    }
    function GetCompressionRatio: Double;

    { Description:
        This returns the compression ratio as a floating point value. A value
        of 1.0 means there has been no significant compression. A value of 0.5
        means that the original, uncompressed, data has been compressed down
        to roughly half their size.
      See also:
        GetCompressionRatio
    }
    property CompressionRatio: Double read GetCompressionRatio;
  end;

  { Description:
      This is the core interface for all checksum/message digest filters.
  }
  IChecksumStreamFilter = interface
    ['{C6DFEAE1-92E3-11D5-B272-0004761A6377}']

    { Description:
        This returns the size of the checksum data in bytes. A 32-bit
        checksum returns 4 in this value. Calling code can use this to
        resize arrays or prepare to get the individual checksum bytes.
      See also:
        ChecksumSize
    }
    function GetChecksumSize: Integer;

    { Description:
        This returns the size of the checksum data in bytes. A 32-bit
        checksum returns 4 in this value. Calling code can use this to
        resize arrays or prepare to get the individual checksum bytes.
      See also:
        GetChecksumSize
    }
    property ChecksumSize: Integer read GetChecksumSize;

    { Description:
        This returns a given byte from the checksum.
      Parameters:
        Index - Which byte to return, 0-based. The first byte is the least
          significant in the checksum.
      See also:
        ChecksumBytes
    }
    function GetChecksumByte(const Index: Integer): Byte;

    { Description:
        This returns a given byte from the checksum.
      Parameters:
        Index - Which byte to return, 0-based. The first byte is the least
          significant in the checksum.
      See also:
        GetChecksumByte
    }
    property ChecksumBytes[const Index: Integer]: Byte read GetChecksumByte;

    { Description:
        This returns the checksum as a hexadecimal string. It consists of only
        uppercase characters, and will contain twice the number of characters
        as there are bytes in the checksum.
      See also:
        AsHexString
    }
    function GetAsHexString: string;

    { Description:
        This returns the checksum as a hexadecimal string. It consists of only
        uppercase characters, and will contain twice the number of characters
        as there are bytes in the checksum.
      See also:
        GetAsHexString
    }
    property AsHexString: string read GetAsHexString;
  end;

  { Description:
      This is the TStream class that uses the various filters. It inherits
      from TStream and can thus be used anywhere you can use a TStream.
  }
  TlvkFilteredStream = class(TStream)
  private
    FCoreStream     : TStream;
    FOwnsCoreStream : Boolean;
    FFilters        : array of IStreamFilter;
    FTopFilter      : IStreamFilter;

  protected
  {$IFDEF VER140}
    { Description:
        This implements the SetSize method as inherited from TStream.
    }
    procedure SetSize(NewSize: Longint); overload; override;

    { Description:
        This implements the SetSize method as inherited from TStream.
    }
    procedure SetSize(const NewSize: Int64); overload; override;
  {$ELSE}
    { Description:
        This implements the SetSize method as inherited from TStream.
    }
    procedure SetSize(NewSize: Longint); override;
  {$ENDIF}

    { Description:
        This property returns the topmost filter in the sequence. The class
        should only talk to this filter and let the filter talk to the next
        filter in the sequence, if necessary.
    }
    property TopFilter: IStreamFilter read FTopFilter;

  public
    { Description:
        This constructor creates and initializes the stream class, and sets up
        the filter sequence.

        The way it works is that a sequence of filters A, B and C will make
        all operations done on the stream be routed to the tomost filter, C.

        The C filter decides what to do according to what the filter is
        programmed to do, and optionally/eventually contacts the next filter
        in the sequence to write or read data. B in turn contacts A, and A
        in turn operates on the core stream. This way, data is passed down
        through the filters and back again.
      Parameters:
        CoreStream - The core TStream class to operate on. Data will
          eventually be written to this stream or read from it, depending
          on what kind of filters are used.
        OwnsCoreStream - If this parameters is set to True, then CoreStream
          will be automatically destroyed when this stream class is destroyed.
        Filters - Array of filters to use. The last filter in the list is
          the topmost filter. The first filter in the list is the filter
          closest to the core stream.
    }
    constructor Create(const CoreStream: TStream; const OwnsCoreStream: Boolean;
      const Filters: array of IStreamFilter);

    { Description:
        This destructor will clean up and flush any and all output buffers
        and remove the filter sequence from memory. If the filters are not
        referenced elsewhere, they will be cleaned up in the process.
    }
    destructor Destroy; override;

    { Description:
        This implements the Read method as inherited from TStream.
    }
    function Read(var Buffer; Count: Longint): Longint; override;

    { Description:
        This implements the Write method as inherited from TStream.
    }
    function Write(const Buffer; Count: Longint): Longint; override;
  {$IFDEF VER140}

    { Description:
        This implements the Seek method as inherited from TStream.
    }
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;

    { Description:
        This implements the Seek method as inherited from TStream.
    }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      overload; override;
  {$ELSE}

    { Description:
        This implements the Seek method as inherited from TStream.
    }
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  {$ENDIF}

    { Description:
        This method executes a manual flush of data pending in output
        buffers. Please note that unless you're at the very end of the data,
        many filters will not react well to being flushed mid-stream so to
        speak.
    }
    procedure FlushOutput; virtual;
  end;

  { Description:
      This exception class is the base class for all exceptions raised within
      the classes related to the stream filter code.
  }
  EStreamFilter = class(Exception);

  { Description:
      This exception class will be raised from within Read methods when
      there is a problem reading from the filter.
  }
  EStreamRead = class(EStreamFilter);

  { Description:
      This exception class will be raised from within Write methods when
      there is a problem writing to the filter.
  }
  EStreamWrite = class(EStreamFilter);

  { Description:
      This exception class will be raised from within Seek methods when
      there is a problem seeking on the filter.
  }
  EStreamSeek = class(EStreamFilter);

  { Description:
      This exception class will be raised from within Seek methods when
      there is a problem sizing the filter.
  }
  EStreamSize = class(EStreamFilter);

implementation

type
  TInternalCoreFilter = class(TInterfacedObject,
    IStreamFilter, IReadableStreamFilter, IWriteableStreamFilter,
    ISeekableStreamFilter, ISizeableStreamFilter)
  private
    FCoreStream : TStream;

  protected
    // IStreamFilter interface
    procedure SetNextFilter(const Filter: IStreamFilter);
    function GetPackageVersion: TPackageVersion;

    // IReadableStreamFilter interface
    function Read(var Buffer; const Count: Longint): Longint;

    // IWriteableStreamFilter interface
    function Write(const Buffer; const Count: Longint): Longint;
    procedure Flush;

    // ISeekableStreamFilter interface
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

    // ISizeableStreamFilter interface
    procedure SetSize(const NewSize: Int64);

  public
    constructor Create(const CoreStream: TStream);
  end;

{ TlvkFilteredStream }

constructor TlvkFilteredStream.Create(const CoreStream: TStream;
  const OwnsCoreStream: Boolean; const Filters: array of IStreamFilter);
var
  Index : Integer;
begin
  Assert(Assigned(CoreStream), 'No stream');
  Assert(Length(Filters)>0, 'No filters');

  inherited Create;

  FCoreStream := CoreStream;
  FOwnsCoreStream := OwnsCoreStream;
  SetLength(FFilters, Length(Filters)+1);
  FFilters[Low(FFilters)] := TInternalCoreFilter.Create(FCoreStream) as
    IStreamFilter;
  for Index := Low(FFilters)+1 to High(FFilters) do
    FFilters[Index] := Filters[Index-(Low(FFilters)+1)+Low(Filters)];

  for Index := Low(FFilters)+1 to High(FFilters) do
    FFilters[Index].SetNextFilter(FFilters[Index-1]);

  FTopFilter := FFilters[High(FFilters)];
end;

destructor TlvkFilteredStream.Destroy;
var
  Index : Integer;
begin
  FlushOutput;

  if Length(FFilters) > 0 then
  begin
    FTopFilter := nil;
    for Index := High(FFilters) downto Low(FFilters) do
      FFilters[Index].SetNextFilter(nil);
  end;
  SetLength(FFilters, 0);

  if FOwnsCoreStream then
    FCoreStream.Free;

  inherited;
end;

procedure TlvkFilteredStream.FlushOutput;
var
  Index : Integer;
  wsf   : IWriteableStreamFilter;
begin
  for Index := High(FFilters) downto Low(FFilters) do
    if FFilters[Index].QueryInterface(IWriteableStreamFilter, wsf) = S_OK then
      wsf.Flush;
end;

function TlvkFilteredStream.Read(var Buffer; Count: Integer): Longint;
var
  rsf : IReadableStreamFilter;
begin
  Assert(Assigned(TopFilter), 'No filters');

  if TopFilter.QueryInterface(IReadableStreamFilter, rsf) = S_OK then
    Result := rsf.Read(Buffer, Count)
  else
    raise EStreamRead.Create('Top filter is not readable in call to Read');
end;

function TlvkFilteredStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := Seek(Int64(Offset), Origin);
end;

{$IFDEF VER140}
function TlvkFilteredStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;
begin
  Assert(Assigned(TopFilter), 'No filters');

  if TopFilter.QueryInterface(ISeekableStreamFilter, ssf) = S_OK then
    Result := ssf.Seek(Offset, Origin)
  else
    raise EStreamSeek.Create('Top filter is not seekable in call to Seek');
end;
{$ENDIF}

{$IFDEF VER140}
procedure TlvkFilteredStream.SetSize(NewSize: Integer);
begin
  SetSize(Int64(NewSize));
end;

procedure TlvkFilteredStream.SetSize(const NewSize: Int64);
var
  ssf : ISizeableStreamFilter;
begin
  Assert(Assigned(TopFilter), 'No filters');

  if TopFilter.QueryInterface(ISizeableStreamFilter, ssf) = S_OK then
    ssf.SetSize(NewSize)
  else
    raise EStreamSize.Create('Top filter is not sizeable in call to SetSize');
end;
{$ELSE}
procedure TlvkFilteredStream.SetSize(NewSize: Integer);
var
  ssf : ISizeableStreamFilter;
begin
  Assert(Assigned(TopFilter), 'No filters');

  if TopFilter.QueryInterface(ISizeableStreamFilter, ssf) = S_OK then
    ssf.SetSize(NewSize)
  else
    raise EStreamSize.Create('Top filter is not sizeable in call to SetSize');
end;
{$ENDIF}

function TlvkFilteredStream.Write(const Buffer; Count: Integer): Longint;
var
  wsf : IWriteableStreamFilter;
begin
  Assert(Assigned(TopFilter), 'No filters');

  if TopFilter.QueryInterface(IWriteableStreamFilter, wsf) = S_OK then
    Result := wsf.Write(Buffer, Count)
  else
    raise EStreamWrite.Create('Top filter is not writeable in call to Write');
end;

{ TInternalCoreFilter }

constructor TInternalCoreFilter.Create(const CoreStream: TStream);
begin
  inherited Create;

  FCoreStream := CoreStream;
end;

procedure TInternalCoreFilter.Flush;
begin
  // Do nothing here
end;

function TInternalCoreFilter.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternalCoreFilter.Read(var Buffer;
  const Count: Integer): Longint;
begin
  Result := FCoreStream.Read(Buffer, Count);
end;

function TInternalCoreFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
const
  Origins : array[TSeekOrigin] of Word = (
    Classes.soFromBeginning,
    Classes.soFromCurrent,
    Classes.soFromEnd
  );
begin
  {$IFDEF VER140}
  Result := FCoreStream.Seek(Offset, Origin);
  {$ELSE}
  Result := FCoreStream.Seek(Offset, Origins[Origin]);
  {$ENDIF}
end;

procedure TInternalCoreFilter.SetNextFilter(const Filter: IStreamFilter);
begin
  // Do nothing here
end;

procedure TInternalCoreFilter.SetSize(const NewSize: Int64);
begin
  FCoreStream.Size := NewSize;
end;

function TInternalCoreFilter.Write(const Buffer;
  const Count: Integer): Longint;
begin
  Result := FCoreStream.Write(Buffer, Count);
end;

end.
