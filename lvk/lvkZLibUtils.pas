{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains easy to use functions for compression and decompression
    through the zLib compression engine.

    Note: This is not a ZIP compression package, it will not handle .zip files,
      although the compression used is compatible with the one used in .zip
      files, the framework around the compressed data is missing.
}
unit lvkZLibUtils;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 10 $
// $Archive: /Components/LVK/source/lvkZLibUtils.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkZLib, lvkZLibConsts, SysUtils, Classes, lvkSafeMem, lvkTypes,
  lvkBufferedStream;

const
  DEFAULT_BUFFER_SIZE       = 32*1024;
  DEFAULT_WRITE_HEADER      = True;
  DEFAULT_READ_HEADER       = DEFAULT_WRITE_HEADER;
  DEFAULT_COMPRESSION_LEVEL = Z_BEST_COMPRESSION;

{ Description:
    This function compresses a file and stores the resulting data into another
    file.
  Parameters:
    SourceFileName - Full path and name of the file containing data to compress.
    SourceStream - The stream to read the uncompressed data from.
    Input - The string or memory block to read the uncompressed data to.
    DestinationFileName - Full path and name of the file to store the
      compressed data into.
    DestinationStream - The stream to write the compressed data to.
    CompressionLevel - How much work to put into compressing the data. More
      work results in smaller files, but will take slightly longer to process.
      The value range from 0 (no compression) to 9 (best compression). Default
      is to use the best compression level (ie. 9).
    BufferSize - How large input and output buffers to process the data through.
      Using larger buffers means more work can be done before having to transfer
      more data to/from the files, but will of course mean a higher memory
      total on your application.
    WriteHeader - Leave it to True to prepend a small header with information
      about the compression level and algorithm used to use as guidelines for
      the decompressor. Many programs, like WinZip and web browsers, doesn't
      use this header though.
  Returns:
    For the string and memory variants, the function returns the compressed
    stream or memory block.
  See also:
    zLibDecompress
}
procedure zLibCompress(const SourceFileName, DestinationFileName: AnsiString;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER); overload;

{ Description:
    This function decompresses a compressed file and stores the resulting data
    into another file.
  Parameters:
    SourceFileName - Full path and name of the file containing data to
      decompress.
    SourceStream - The stream to read the compressed data from.
    Input - The input memory or string to read the compressed data from.
    DestinationFileName - Full path and name of the file to store the
      decompressed data into.
    DestinationStream - The stream to write the decompressed data to.
    BufferSize - How large input and output buffers to process the data through.
      Using larger buffers means more work can be done before having to transfer
      more data to/from the files, but will of course mean a higher memory
      total on your application.
    ReadHeader - Leave it to True to be compatible with the .ZIP file format
      compression, set it to False to be compatible with the Deflate algorithm
      used with web browsers, and with the gzip file format. Note that this
      parameter must have the same value as the WriteHeader parameter you used
      with zLibCompressXYZ.
  Returns:
    For the string and memory variants, the function returns the decompressed
    stream or memory block.
  See also:
    zLibCompress
}
procedure zLibDecompress(const SourceFileName, DestinationFileName: AnsiString;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER); overload;

// <COMBINE zLibCompress@string@string@Integer@Cardinal@Boolean>
procedure zLibCompress(const SourceStream, DestinationStream: TStream;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER); overload;

// <COMBINE zLibDecompress@string@string@Cardinal@Boolean>
procedure zLibDecompress(const SourceStream, DestinationStream: TStream;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER); overload;

// <COMBINE zLibCompress@string@string@Integer@Cardinal@Boolean>
function zLibCompress(const Input: AnsiString;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER): AnsiString; overload;

// <COMBINE zLibDecompress@string@string@Cardinal@Boolean>
function zLibDecompress(const Input: AnsiString;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER): AnsiString; overload;

// <COMBINE zLibCompress@string@string@Integer@Cardinal@Boolean>
function zLibCompress(const Input: ISafeMem;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER): ISafeMem; overload;

// <COMBINE zLibDecompress@string@string@Cardinal@Boolean>
function zLibDecompress(const Input: ISafeMem;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER): ISafeMem; overload;

{ Description:
    This function compresses a file and stores the resulting data into another
    file. It uses the gzip compression format.
  Parameters:
    SourceFileName - Full path and name of the file containing data to compress.
    SourceStream - The stream to read the uncompressed data from.
    Input - The string or memory block to read the uncompressed data to.
    DestinationFileName - Full path and name of the file to store the
      compressed data into.
    DestinationStream - The stream to write the compressed data to.
    CompressionLevel - How much work to put into compressing the data. More
      work results in smaller files, but will take slightly longer to process.
      The value range from 0 (no compression) to 9 (best compression). Default
      is to use the best compression level (ie. 9).
    BufferSize - How large input and output buffers to process the data through.
      Using larger buffers means more work can be done before having to transfer
      more data to/from the files, but will of course mean a higher memory
      total on your application.
  Returns:
    For the string and memory variants, the function returns the compressed
    stream or memory block.
  See also:
    gZipDecompress
}
procedure gZipCompress(const SourceFileName, DestinationFileName: AnsiString;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE); overload;

{ Description:
    This function decompresses a compressed file and stores the resulting data
    into another file. It uses the gzip compression format.
  Parameters:
    SourceFileName - Full path and name of the file containing data to
      decompress.
    SourceStream - The stream to read the compressed data from.
    Input - The input memory or string to read the compressed data from.
    DestinationFileName - Full path and name of the file to store the
      decompressed data into.
    DestinationStream - The stream to write the decompressed data to.
    BufferSize - How large input and output buffers to process the data through.
      Using larger buffers means more work can be done before having to transfer
      more data to/from the files, but will of course mean a higher memory
      total on your application.
  Returns:
    For the string and memory variants, the function returns the decompressed
    stream or memory block.
  See also:
    gZipCompress
}
procedure gZipDecompress(const SourceFileName, DestinationFileName: AnsiString;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE); overload;

// <COMBINE gZipCompress@string@string@Integer@Cardinal>
procedure gZipCompress(const SourceStream, DestinationStream: TStream;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE); overload;

// <COMBINE gZipDecompress@string@string@Cardinal>
procedure gZipDecompress(const SourceStream, DestinationStream: TStream;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE); overload;

// <COMBINE gZipCompress@string@string@Integer@Cardinal>
function gZipCompress(const Input: AnsiString;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE): AnsiString; overload;

// <COMBINE gZipDecompress@string@string@Cardinal>
function gZipDecompress(const Input: AnsiString;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE): AnsiString; overload;

// <COMBINE gZipCompress@string@string@Integer@Cardinal>
function gZipCompress(const Input: ISafeMem;
  const CompressionLevel: Integer=DEFAULT_COMPRESSION_LEVEL;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE): ISafeMem; overload;

// <COMBINE gZipDecompress@string@string@Cardinal>
function gZipDecompress(const Input: ISafeMem;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE): ISafeMem; overload;

implementation

uses
  lvkZLibTypes, Windows, lvkCRC, lvkChecksumUtils;

type
  TGZipHeader = packed record
    ID1_2 : UInt16;
    CM    : UInt8;
    FLG   : UInt8;
    MTIME : UInt32;
    XFL   : UInt8;
    OS    : UInt8;
  end;

  TGZipTrailer = packed record
    CRC32 : TCRC32;
    ISIZE : UInt32;
  end;

const
  GZIP_FLAG_FTEXT     = 1;
  GZIP_FLAG_FHCRC     = 2;
  GZIP_FLAG_FEXTRA    = 4;
  GZIP_FLAG_FNAME     = 8;
  GZIP_FLAG_FCOMMENT  = 16;

  GZIP_SIGNATURE      = $8B1F;
  GZIP_DEFLATED       = 8;

procedure CompressStream(const SourceStream, DestinationStream: TStream;
  const CompressionLevel: Integer; const BufferSize: Cardinal;
  const WriteHeader: Boolean);
var
  zs            : z_stream;
  InputBuffer   : ISafeMem;
  OutputBuffer  : ISafeMem;
  errorCode     : Integer;
  AtEnd         : Boolean;

  procedure Initialize;
  begin
    ZeroMemory(@zs, SizeOf(zs));

    zs.next_in := InputBuffer.Pointer;
    zs.next_out := OutputBuffer.Pointer;
    zs.avail_out := OutputBuffer.Size;

    if WriteHeader then
      zLibCheckCompress(deflateInit(zs, CompressionLevel))
    else
      zLibCheckCompress(deflateInit2(zs, CompressionLevel, Z_DEFLATED, -15, 9,
        Z_DEFAULT_STRATEGY));
  end;

  procedure Cleanup;
  begin
    zLibCheckCompress(deflateEnd(zs));
  end;

begin
  Assert(Assigned(SourceStream));
  Assert(Assigned(DestinationStream));
  Assert((CompressionLevel >= -1) and (CompressionLevel <= 9));
  Assert(BufferSize >= 2048);

  InputBuffer := AllocateSafeMem(BufferSize);
  OutputBuffer := InputBuffer.Clone(False);

  Initialize;
  try
    AtEnd := False;

    repeat
      if zs.avail_in = 0 then
      begin
        zs.next_in := InputBuffer.Pointer;
        zs.avail_in := SourceStream.Read(zs.next_in^, InputBuffer.Size);
        if zs.avail_in < BufferSize then
          AtEnd := True;
      end;

      if AtEnd then
        errorCode := deflate(zs, Z_FINISH)
      else
        errorCode := deflate(zs, Z_NO_FLUSH);

      if (errorCode <> Z_STREAM_END) and (errorCode <> Z_OK) then
        zLibCheckCompress(errorCode);

      if (zs.avail_out = 0) or (errorCode = Z_STREAM_END) then
      begin
        DestinationStream.WriteBuffer(OutputBuffer.Pointer^, zs.next_out -
          OutputBuffer.Pointer);
        zs.next_out := OutputBuffer.Pointer;
        zs.avail_out := OutputBuffer.Size;
      end;
    until errorCode = Z_STREAM_END;
  finally
    Cleanup;
  end;
end;


procedure DecompressStream(const SourceStream, DestinationStream: TStream;
  const BufferSize: Cardinal; const ReadHeader: Boolean);
var
  zs            : z_stream;
  InputBuffer   : ISafeMem;
  OutputBuffer  : ISafeMem;
  errorCode     : Integer;

  procedure Initialize;
  begin
    ZeroMemory(@zs, SizeOf(zs));

    zs.next_in := InputBuffer.Pointer;
    zs.next_out := OutputBuffer.Pointer;
    zs.avail_out := OutputBuffer.Size;

    if ReadHeader then
      zLibCheckDecompress(inflateInit(zs))
    else
      zLibCheckDecompress(inflateInit2(zs, -15));
  end;

  procedure Cleanup;
  begin
    zLibCheckDecompress(inflateEnd(zs));
  end;

begin
  Assert(BufferSize >= 2048);

  InputBuffer := AllocateSafeMem(BufferSize);
  OutputBuffer := InputBuffer.Clone(False);

  Initialize;
  try
    repeat
      if zs.avail_in = 0 then
      begin
        zs.next_in := InputBuffer.Pointer;
        zs.avail_in := SourceStream.Read(InputBuffer.Pointer^,
          InputBuffer.Size);
      end;

      errorCode := inflate(zs, Z_SYNC_FLUSH);

      if (errorCode <> Z_STREAM_END) and (errorCode <> Z_OK) then
        zLibCheckDecompress(errorCode);

      if (zs.avail_out = 0) or (errorCode = Z_STREAM_END) then
      begin
        DestinationStream.WriteBuffer(OutputBuffer.Pointer^, zs.next_out -
          OutputBuffer.Pointer);
        zs.next_out := OutputBuffer.Pointer;
        zs.avail_out := OutputBuffer.Size;
      end;
    until errorCode = Z_STREAM_END;
  finally
    Cleanup;
  end;
end;

procedure zLibCompress(const SourceFileName, DestinationFileName: AnsiString;
  const CompressionLevel: Integer; const BufferSize: Cardinal;
  const WriteHeader: Boolean);
var
  SourceStream      : TStream;
  DestinationStream : TStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or
      fmShareDenyWrite);
    DestinationStream := TFileStream.Create(DestinationFileName, fmCreate);

    zLibCompress(SourceStream, DestinationStream, CompressionLevel, BufferSize,
      WriteHeader);
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

procedure zLibDecompress(const SourceFileName, DestinationFileName: AnsiString;
  const BufferSize: Cardinal; const ReadHeader: Boolean);
var
  SourceStream      : TStream;
  DestinationStream : TStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or
      fmShareDenyWrite);
    DestinationStream := TFileStream.Create(DestinationFileName, fmCreate);

    zLibDecompress(SourceStream, DestinationStream, BufferSize, ReadHeader);
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

procedure zLibCompress(const SourceStream, DestinationStream: TStream;
  const CompressionLevel: Integer; const BufferSize: Cardinal;
  const WriteHeader: Boolean);
begin
  Assert(Assigned(SourceStream));
  Assert(Assigned(DestinationStream));

  SourceStream.Position := 0;
  CompressStream(SourceStream, DestinationStream, CompressionLevel,
    BufferSize, WriteHeader);
end;

procedure zLibDecompress(const SourceStream, DestinationStream: TStream;
  const BufferSize: Cardinal; const ReadHeader: Boolean);
begin
  Assert(Assigned(SourceStream));
  Assert(Assigned(DestinationStream));

  SourceStream.Position := 0;
  DecompressStream(SourceStream, DestinationStream, BufferSize, ReadHeader);
end;

function zLibCompress(const Input: AnsiString;
  const CompressionLevel: Integer; const BufferSize: Cardinal;
  const WriteHeader: Boolean): AnsiString;
var
  SourceStream      : TStringStream;
  DestinationStream : TStringStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TStringStream.Create(Input);
    DestinationStream := TStringStream.Create('');

    zLibCompress(SourceStream, DestinationStream, CompressionLevel, BufferSize,
      WriteHeader);

    Result := DestinationStream.DataString;
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

function zLibDecompress(const Input: AnsiString;
  const BufferSize: Cardinal; const ReadHeader: Boolean): AnsiString;
var
  SourceStream      : TStringStream;
  DestinationStream : TStringStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TStringStream.Create(Input);
    DestinationStream := TStringStream.Create('');

    zLibDecompress(SourceStream, DestinationStream, BufferSize, ReadHeader);

    Result := DestinationStream.DataString;
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

function zLibCompress(const Input: ISafeMem; const CompressionLevel: Integer;
  const BufferSize: Cardinal; const WriteHeader: Boolean): ISafeMem;
begin
  Assert(Assigned(Input));

  Result := AllocateSafeMem(0);
  zLibCompress(Input.Stream, Result.Stream, CompressionLevel, BufferSize,
    WriteHeader);
  Result.Stream.Position := 0;
end;

function zLibDecompress(const Input: ISafeMem;
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER): ISafeMem;
begin
  Assert(Assigned(Input));

  Result := AllocateSafeMem(0);
  zLibDecompress(Input.Stream, Result.Stream, BufferSize, ReadHeader);
  Result.Stream.Position := 0;
end;

procedure gZipCompress(const SourceFileName, DestinationFileName: AnsiString;
  const CompressionLevel: Integer; const BufferSize: Cardinal);
var
  SourceStream      : TStream;
  DestinationStream : TStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or
      fmShareDenyWrite);
    DestinationStream := TFileStream.Create(DestinationFileName, fmCreate);

    gZipCompress(SourceStream, DestinationStream, CompressionLevel, BufferSize);
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

procedure gZipDecompress(const SourceFileName, DestinationFileName: AnsiString;
  const BufferSize: Cardinal);
var
  SourceStream      : TStream;
  DestinationStream : TStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or
      fmShareDenyWrite);
    DestinationStream := TFileStream.Create(DestinationFileName, fmCreate);

    gZipDecompress(SourceStream, DestinationStream, BufferSize);
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

procedure gZipCompress(const SourceStream, DestinationStream: TStream;
  const CompressionLevel: Integer; const BufferSize: Cardinal);

  procedure WriteHeader;
  var
    Header  : TGZipHeader;
  begin
    ZeroMemory(@Header, SizeOf(Header));
    Header.ID1_2 := GZIP_SIGNATURE;
    Header.CM := GZIP_DEFLATED;

    DestinationStream.WriteBuffer(Header, SizeOf(Header));
  end;

  procedure WriteTrailer;
  var
    Trailer : TGZipTrailer;
  begin
    Trailer.CRC32 := CRC32Of(SourceStream);
    Trailer.ISIZE := SourceStream.Size;

    DestinationStream.WriteBuffer(Trailer, SizeOf(Trailer));
  end;

begin
  Assert(Assigned(SourceStream));
  Assert(Assigned(DestinationStream));

  SourceStream.Position := 0;

  WriteHeader;
  CompressStream(SourceStream, DestinationStream, CompressionLevel, BufferSize,
    False);
  WriteTrailer;
end;

procedure gZipDecompress(const SourceStream, DestinationStream: TStream;
  const BufferSize: Cardinal);
var
  Header        : TGZipHeader;
  SourceBuffer  : TStream;

  procedure SkipExtra;
  var
    XLEN  : UInt16;
  begin
    SourceBuffer.ReadBuffer(XLEN, SizeOf(XLEN));
    SourceBuffer.Position := SourceStream.Position + XLEN;
  end;

  procedure SkipASCIIZ;
  var
    b : UInt8;
  begin
    repeat
      SourceBuffer.ReadBuffer(b, 1);
    until b = 0;
  end;

  procedure SkipCRC16;
  var
    CRC16 : UInt16;
  begin
    SourceBuffer.ReadBuffer(CRC16, SizeOf(CRC16));
  end;

begin
  Assert(Assigned(SourceStream));
  Assert(Assigned(DestinationStream));

  SourceBuffer := TlvkBufferedStream.Create(SourceStream, False);
  try
    SourceBuffer.Position := 0;
    SourceBuffer.ReadBuffer(Header, SizeOf(Header));
    Assert(Header.ID1_2 = GZIP_SIGNATURE);
    Assert(Header.CM = GZIP_DEFLATED);
    if (Header.FLG and GZIP_FLAG_FEXTRA) <> 0 then
      SkipExtra;
    if (Header.FLG and GZIP_FLAG_FNAME) <> 0 then
      SkipASCIIZ;
    if (Header.FLG and GZIP_FLAG_FCOMMENT) <> 0 then
      SkipASCIIZ;
    if (Header.FLG and GZIP_FLAG_FHCRC) <> 0 then
      SkipCRC16;

    DecompressStream(SourceBuffer, DestinationStream, BufferSize, False);
  finally
    SourceBuffer.Free;
  end;
end;

function gZipCompress(const Input: AnsiString; const CompressionLevel: Integer;
  const BufferSize: Cardinal): AnsiString;
var
  SourceStream      : TStringStream;
  DestinationStream : TStringStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TStringStream.Create(Input);
    DestinationStream := TStringStream.Create('');

    gZipCompress(SourceStream, DestinationStream, CompressionLevel, BufferSize);

    Result := DestinationStream.DataString;
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

function gZipDecompress(const Input: AnsiString;
  const BufferSize: Cardinal): AnsiString;
var
  SourceStream      : TStringStream;
  DestinationStream : TStringStream;
begin
  SourceStream := nil;
  DestinationStream := nil;
  try
    SourceStream := TStringStream.Create(Input);
    DestinationStream := TStringStream.Create('');

    gZipDecompress(SourceStream, DestinationStream, BufferSize);

    Result := DestinationStream.DataString;
  finally
    DestinationStream.Free;
    SourceStream.Free;
  end;
end;

function gZipCompress(const Input: ISafeMem; const CompressionLevel: Integer;
  const BufferSize: Cardinal): ISafeMem;
begin
  Assert(Assigned(Input));

  Result := AllocateSafeMem(0);
  gZipCompress(Input.Stream, Result.Stream, CompressionLevel, BufferSize);
  Result.Stream.Position := 0;
end;

function gZipDecompress(const Input: ISafeMem;
  const BufferSize: Cardinal): ISafeMem;
begin
  Assert(Assigned(Input));

  Result := AllocateSafeMem(0);
  gZipDecompress(Input.Stream, Result.Stream, BufferSize);
  Result.Stream.Position := 0;
end;

end.
