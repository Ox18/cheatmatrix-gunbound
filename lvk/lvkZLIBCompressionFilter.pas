{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code for zLib compression as a stream filter.
}
unit lvkZLIBCompressionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkZLIBCompressionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, lvkStreamFilters;

type
	TCompressionLevel = (clNone, clBestSpeed, clBestCompression);

const
  DEFAULT_BUFFER_SIZE       = 32768;
  DEFAULT_WRITE_HEADER      = True;
  DEFAULT_READ_HEADER       = DEFAULT_WRITE_HEADER;
  DEFAULT_COMPRESSION_LEVEL = clBestCompression;

{ Description:
    This function returns a writeable filter that compresses data with the
    zLib compression engine. Write uncompressed data to this filter and
    compressed data will be written to the next filter.
  See also:
    NewReadableZLIBDecompressionFilter@Cardinal@Boolean,
    NewReadableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel,
    NewWriteableZLIBDecompressionFilter@Cardinal@Boolean
}
function NewWriteableZLIBCompressionFilter(
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER;
  const CompressionLevel: TCompressionLevel=
    DEFAULT_COMPRESSION_LEVEL): IStreamFilter;

{ Description:
    This function returns a readable filter that decompresses data with the
    zLib compression engine. This filter will read compressed data from the
    next filter, and return uncompressed data back.
  See also:
    NewWriteableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel,
    NewReadableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel,
    NewWriteableZLIBDecompressionFilter@Cardinal@Boolean
}
function NewReadableZLIBDecompressionFilter(
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER): IStreamFilter;

{ Description:
    This function returns a readable filter that compresses data with the zLib
    compression engine. This filter will read uncompressed data from the
    next filter, and return compressed data back.
  See also:
    NewWriteableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel,
    NewReadableZLIBDecompressionFilter@Cardinal@Boolean,
    NewWriteableZLIBDecompressionFilter@Cardinal@Boolean
}
function NewReadableZLIBCompressionFilter(
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const WriteHeader: Boolean=DEFAULT_WRITE_HEADER;
  const CompressionLevel: TCompressionLevel=
    DEFAULT_COMPRESSION_LEVEL): IStreamFilter;

{ Description:
    This function returns a writeable filter that decompresses data with the
    zLib compression engine. Write compressed data to this filter and it will
    write uncompressed data to the next filter.
  See also:
    NewWriteableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel,
    NewReadableZLIBDecompressionFilter@Cardinal@Boolean,
    NewReadableZLIBCompressionFilter@Cardinal@Boolean@TCompressionLevel
}
function NewWriteableZLIBDecompressionFilter(
  const BufferSize: Cardinal=DEFAULT_BUFFER_SIZE;
  const ReadHeader: Boolean=DEFAULT_READ_HEADER): IStreamFilter;

implementation

uses
  Math, lvkZLib, lvkZLibTypes, lvkZLibConsts, lvkBasicStreamFilters;

const
	zLibLevels	: array[ TCompressionLevel ] of Integer = (
		Z_NO_COMPRESSION,				// clNone
		Z_BEST_SPEED,						// clBestSpeed
		Z_BEST_COMPRESSION			// clBestCompression
	 );

type
  TBaseZLIBFilter = class(TlvkBaseStreamFilter)
  private
    FZS         : z_stream;
    FBufferSize : Cardinal;
    FPosition   : Integer;
    FInputBuf   : PChar;
    FOutputBuf  : PChar;

  protected
    // ICompressionStreamFilter filter
    function GetTotalInput: Cardinal; virtual;
    function GetTotalOutput: Cardinal; virtual;
    function GetCompressionRatio: Double; virtual; 

    // Internal
    procedure ResetFZS;

  public
    constructor Create(const BufferSize: Cardinal);
    destructor Destroy; override;
  end;

  TWriteableZLIBCompressionFilter = class(TBaseZLIBFilter,
    IWriteableStreamFilter, ISeekableStreamFilter)
  private
    FCompressionLevel : TCompressionLevel;
    FWriteHeader      : Boolean;

    function CompressBuffer(const Sync: Integer): Integer;
    procedure FlushEnd;
    procedure WriteOutput;
    procedure FixupInput;

  protected
    // IStreamFilter interface
    procedure SetNextFilter(const Filter: IStreamFilter); override;

    // IWriteableStreamFilter interface
    function Write(const Buffer; const Count: Longint): Longint; virtual;
    procedure Flush; virtual;

    // ISeekableStreamFilter interface
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64; virtual;

    // ICompressionStreamFilter filter
    function GetCompressionRatio: Double; override;

  public
    constructor Create(const BufferSize: Cardinal; const WriteHeader: Boolean; const CompressionLevel: TCompressionLevel);
    destructor Destroy; override;
  end;

  TReadableZLIBDecompressionFilter = class(TBaseZLIBFilter,
    IReadableStreamFilter, ISeekableStreamFilter)
  private
    FAtEnd      : Boolean;
    FReadHeader : Boolean;

    function DecompressData: Integer;

  protected
    // ICompressionStreamFilter filter
    function GetCompressionRatio: Double; override;

    // IReadableStreamFilter
    function Read(var Buffer; const Count: Longint): Longint;

    // ISeekableStreamFilter
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

  public
    constructor Create(const BufferSize: Cardinal; const ReadHeader: Boolean);
    destructor Destroy; override;
  end;

  TWriteableZLIBDecompressionFilter = class(TBaseZLIBFilter,
    IWriteableStreamFilter, ISeekableStreamFilter)
  private
    FReadHeader : Boolean;

    function DecompressBuffer(const Sync: Integer): Integer;
    procedure FlushEnd;
    procedure WriteOutput;
    procedure FixupInput;

  protected
    // IStreamFilter interface
    procedure SetNextFilter(const Filter: IStreamFilter); override;

    // IWriteableStreamFilter interface
    function Write(const Buffer; const Count: Longint): Longint; virtual;
    procedure Flush; virtual;

    // ISeekableStreamFilter interface
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64; virtual;

    // ICompressionStreamFilter filter
    function GetCompressionRatio: Double; override;

  public
    constructor Create(const BufferSize: Cardinal; const ReadHeader: Boolean);
    destructor Destroy; override;
  end;

  TReadableZLIBCompressionFilter = class(TBaseZLIBFilter,
    IReadableStreamFilter, ISeekableStreamFilter)
  private
    FAtEnd            : Boolean;
    FCompressionLevel : TCompressionLevel;
    FWriteHeader      : Boolean;

    function CompressData: Integer;

  protected
    // ICompressionStreamFilter filter
    function GetCompressionRatio: Double; override;

    // IReadableStreamFilter
    function Read(var Buffer; const Count: Longint): Longint;

    // ISeekableStreamFilter
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

  public
    constructor Create(const BufferSize: Cardinal; const WriteHeader: Boolean;
      const CompressionLevel: TCompressionLevel);
    destructor Destroy; override;
  end;

function NewWriteableZLIBCompressionFilter(const BufferSize: Cardinal; const WriteHeader: Boolean; const CompressionLevel: TCompressionLevel): IStreamFilter;
begin
  Result := TWriteableZLIBCompressionFilter.Create(BufferSize, WriteHeader, CompressionLevel) as IStreamFilter;
end;

function NewReadableZLIBDecompressionFilter(const BufferSize: Cardinal;
  const ReadHeader: Boolean): IStreamFilter;
begin
  Result := TReadableZLIBDecompressionFilter.Create(BufferSize, ReadHeader) as IStreamFilter;
end;

function NewWriteableZLIBDecompressionFilter(const BufferSize: Cardinal;
  const ReadHeader: Boolean): IStreamFilter;
begin
  Result := TWriteableZLIBDecompressionFilter.Create(BufferSize, ReadHeader) as IStreamFilter;
end;

function NewReadableZLIBCompressionFilter(const BufferSize: Cardinal;
  const WriteHeader: Boolean; const CompressionLevel: TCompressionLevel): IStreamFilter;
begin
  Result := TReadableZLIBCompressionFilter.Create(BufferSize, WriteHeader, CompressionLevel) as IStreamFilter;
end;

{ TWriteableZLIBCompressionFilter }

function TWriteableZLIBCompressionFilter.CompressBuffer(
  const Sync: Integer): Integer;
begin
  Result := deflate(FZS, Sync);

  FixupInput;
	if (Fzs.avail_out = 0) or (Result = Z_STREAM_END) then
    WriteOutput;
end;

constructor TWriteableZLIBCompressionFilter.Create(const BufferSize: Cardinal;
  const WriteHeader: Boolean; const CompressionLevel: TCompressionLevel);
begin
  inherited Create(BufferSize);

  FCompressionLevel := CompressionLevel;
  FWriteHeader := WriteHeader;

	if FWriteHeader then
    deflateInit(FZS, zLibLevels[CompressionLevel])
  else
    deflateInit2(FZS, zLibLevels[CompressionLevel], Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY);
end;

destructor TWriteableZLIBCompressionFilter.Destroy;
begin
  deflateEnd(FZS);

  inherited;
end;

procedure TWriteableZLIBCompressionFilter.FixupInput;
begin
  if FZS.avail_in > 0 then
    Move(FZS.next_in^, FInputBuf^, FZS.avail_in);

  FZS.next_in := FInputBuf;
end;

procedure TWriteableZLIBCompressionFilter.Flush;
begin
  if FZS.avail_out < FBufferSize then
    WriteOutput;
end;

procedure TWriteableZLIBCompressionFilter.FlushEnd;
var
  zrc : Integer;
begin
  repeat
    zrc := CompressBuffer(Z_FINISH);
  until (zrc <> Z_OK) or ((FZS.avail_in = 0) and (FZS.avail_out = FBufferSize));
end;

function TWriteableZLIBCompressionFilter.GetCompressionRatio: Double;
begin
  if GetTotalInput = 0 then
    Result := 1.0
  else
    Result := GetTotalOutput / GetTotalInput;
end;

function TWriteableZLIBCompressionFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromCurrent(const Offset: Int64): Int64; forward;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to end, current or beginning');

    Result := ssf.Seek(0, soBeginning);
    FPosition := 0;
    deflateEnd(FZS);
    ResetFZS;
    if FWriteHeader then
      deflateInit(FZS, zLibLevels[FCompressionLevel])
    else
      deflateInit2(FZS, zLibLevels[FCompressionLevel], Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY);

    if Offset > 0 then
      Result := SeekFromCurrent(Offset);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  begin
    Assert(Offset=0, 'Can only seek to end, current or beginning');

    Result := FPosition;
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to end, current or beginning');

    Result := FPosition;
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := FPosition;
  end;
end;

procedure TWriteableZLIBCompressionFilter.SetNextFilter(
  const Filter: IStreamFilter);
begin
  if not Assigned(Filter) then
    FlushEnd;

  inherited;
end;

function TWriteableZLIBCompressionFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  Left      : Integer;
  Room      : Integer;
  ToCopy    : Integer;
  zrc       : Integer;
begin
  Assert(@Buffer <> nil);

  BufferPtr := @Buffer;
  Left := Count;

  Result := 0;
  while Left > 0 do
  begin
    Room := FBufferSize - FZS.avail_in;
    ToCopy := Min(Room, Left);

    if ToCopy > 0 then
    begin
      Move(BufferPtr^, (FZS.next_in + FZS.avail_in)^, ToCopy);
      Inc(FZS.avail_in, ToCopy);
      Inc(BufferPtr, ToCopy);
      Inc(FPosition);
      Dec(Left, ToCopy);
      Inc(Result, ToCopy);
    end else begin
      zrc := CompressBuffer(0);
      if zrc <> Z_OK then
        Break;
    end;
  end;
end;

procedure TWriteableZLIBCompressionFilter.WriteOutput;
var
  wsf     : IWriteableStreamFilter;
  Written : Cardinal;
  ToWrite : Cardinal;
begin
  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) = S_OK then
  begin
    ToWrite := FBufferSize - FZS.avail_out;
    Written := wsf.Write(FOutputBuf^, ToWrite);
    if Written = ToWrite then
    begin
      FZS.next_out := FOutputBuf;
      FZS.avail_out := FBufferSize;
    end else begin
      Move((FOutputBuf + Written)^, FOutputBuf^, ToWrite-Written);
      FZS.next_out := FOutputBuf + (ToWrite-Written);
      FZS.avail_out := FBufferSize - (ToWrite-Written);
    end;
  end else
    raise EStreamWrite.Create('Next filter not writeable in call to WriteOutput');
end;

{ TBaseZLIBFilter }

constructor TBaseZLIBFilter.Create(const BufferSize: Cardinal);
begin
  Assert(BufferSize>0);

  inherited Create;

  FBufferSize := BufferSize;
  FPosition := 0;
  GetMem(FInputBuf, FBufferSize);
  GetMem(FOutputBuf, FBufferSize);

  ResetFZS;
end;

destructor TBaseZLIBFilter.Destroy;
begin
  FreeMem(FInputBuf);
  FreeMem(FOutputBuf);

  inherited;
end;

function TBaseZLIBFilter.GetCompressionRatio: Double;
begin
  // Dummy
  Result := 0.0;
end;

function TBaseZLIBFilter.GetTotalInput: Cardinal;
begin
  Result := FZS.total_in;
end;

function TBaseZLIBFilter.GetTotalOutput: Cardinal;
begin
  Result := FZS.total_out;
end;

procedure TBaseZLIBFilter.ResetFZS;
begin
	FillChar(FZS, SizeOf(FZS), #0);
	Fzs.next_in		:= FInputBuf;
	Fzs.next_out	:= FOutputBuf;
	Fzs.avail_out	:= FBufferSize;
end;

{ TReadableZLIBDecompressionFilter }

constructor TReadableZLIBDecompressionFilter.Create(
  const BufferSize: Cardinal; const ReadHeader: Boolean);
begin
  inherited Create(BufferSize);

  FReadHeader := ReadHeader;

  if ReadHeader then
    inflateInit(FZS)
  else
    inflateInit2(FZS, -15);
end;

function TReadableZLIBDecompressionFilter.DecompressData: Integer;
var
  Room  : Integer;
  Ptr   : Pointer;
  rsf   : IReadableStreamFilter;
begin
  if FZS.avail_in > 0 then
  begin
    if FZS.next_in > FInputBuf then
    begin
      Move(FZS.next_in^, FInputBuf^, FZS.avail_in);
      FZS.next_in := FInputBuf;
    end;

    Room := FBufferSize - FZS.avail_in;
    Ptr := FInputBuf + FZS.avail_in;
  end else begin
    Room := FBufferSize;
    Ptr := FInputBuf;
    FZS.next_in := FInputBuf;
  end;

  if NextFilter.QueryInterface(IReadableStreamFilter, rsf) <> S_OK then
    raise EStreamRead.Create('Next filter not readable in call to Seek');

  FZS.avail_in := FZS.avail_in + Cardinal(rsf.Read(Ptr^, Room));
  FZS.next_out := FOutputBuf;
  FZS.avail_out := FBufferSize;
  Result := inflate(FZS, 0);
end;

destructor TReadableZLIBDecompressionFilter.Destroy;
begin
  inflateEnd(FZS);
  
  inherited;
end;

function TReadableZLIBDecompressionFilter.GetCompressionRatio: Double;
begin
  if GetTotalOutput = 0 then
    Result := 1.0
  else
    Result := GetTotalInput / GetTotalOutput;
end;

function TReadableZLIBDecompressionFilter.Read(var Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  Left      : Integer;
  Available : Integer;
  ToCopy    : Integer;
  zrc       : Integer;
begin
  BufferPtr := @Buffer;
  Left := Count;

  Result := 0;
  while Left > 0 do
  begin
    Available := FBufferSize - FZS.avail_out;
    ToCopy := Min(Left, Available);

    if ToCopy > 0 then
    begin
      Move(FOutputBuf^, BufferPtr^, ToCopy);
      Inc(BufferPtr, ToCopy);
      Dec(Left, ToCopy);
      Inc(Result, ToCopy);
      Inc(FPosition, ToCopy);
      Inc(FZS.avail_out, ToCopy);
      if FZS.avail_out < FBufferSize then
      begin
        Move((FOutputBuf + ToCopy)^, FOutputBuf^, FBufferSize-FZS.avail_out);
        FZS.next_out := FOutputBuf + (FBufferSize-FZS.avail_out);
      end;
    end else begin
      if FAtEnd then
        Break;
      zrc := DecompressData;
      if zrc = Z_STREAM_END then
        FAtEnd := True
      else if zrc <> Z_OK then
        Break;
    end;
  end;
end;

function TReadableZLIBDecompressionFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromCurrent(const Offset: Int64): Int64; forward;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Result := ssf.Seek(0, soBeginning);
    FPosition := 0;
    FAtEnd := False;
    inflateEnd(FZS);
    ResetFZS;

    if FReadHeader then
      inflateInit(FZS)
    else
      inflateInit2(FZS, -15);

    if Offset > 0 then
      Result := SeekFromCurrent(Offset);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  var
    Buffer  : array[0..2047] of Byte;
    Left    : Int64;
  begin
    if Offset < 0 then
      Result := SeekFromBeginning(FPosition + Offset)
    else begin
      Left := Offset;
      while Left > 0 do
        Dec(Left, Read(Buffer, Min(SizeOf(Buffer), Left)));

      Result := FPosition;
    end;
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  var
    Buffer  : array[0..2047] of Byte;
  begin
    while Read(Buffer, SizeOf(Buffer)) > 0 do
      ;

    if Offset = 0 then
      Result := FPosition
    else
      Result := SeekFromBeginning(FPosition + Offset);
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := FPosition;
  end;
end;

{ TWriteableZLIBDecompressionFilter }

constructor TWriteableZLIBDecompressionFilter.Create(
  const BufferSize: Cardinal; const ReadHeader: Boolean);
begin
  inherited Create(BufferSize);

  FReadHeader := ReadHeader;

  if FReadHeader then
    inflateInit(FZS)
  else
    inflateInit2(FZS, -15);
end;

function TWriteableZLIBDecompressionFilter.DecompressBuffer(
  const Sync: Integer): Integer;
begin
  Result := inflate(FZS, Sync);

  FixupInput;
	if (Fzs.avail_out = 0) or (Result = Z_STREAM_END) then
    WriteOutput;
end;

destructor TWriteableZLIBDecompressionFilter.Destroy;
begin
  inflateEnd(FZS);

  inherited;
end;

procedure TWriteableZLIBDecompressionFilter.FixupInput;
begin
  if FZS.avail_in > 0 then
    Move(FZS.next_in^, FInputBuf^, FZS.avail_in);

  FZS.next_in := FInputBuf;
end;

procedure TWriteableZLIBDecompressionFilter.Flush;
begin
  if FZS.avail_out < FBufferSize then
    WriteOutput;
end;

procedure TWriteableZLIBDecompressionFilter.FlushEnd;
var
  zrc : Integer;
begin
  repeat
    zrc := DecompressBuffer(0);
  until (zrc <> Z_OK) or ((FZS.avail_in = 0) and (FZS.avail_out = FBufferSize));
end;

function TWriteableZLIBDecompressionFilter.GetCompressionRatio: Double;
begin
  if GetTotalOutput = 0 then
    Result := 1.0
  else
    Result := GetTotalInput / GetTotalOutput;
end;

function TWriteableZLIBDecompressionFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromCurrent(const Offset: Int64): Int64; forward;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to end, current or beginning');

    Result := ssf.Seek(0, soBeginning);
    FPosition := 0;
    inflateEnd(FZS);
    ResetFZS;
    if FReadHeader then
      inflateInit(FZS)
    else
      inflateInit2(FZS, -15);

    if Offset > 0 then
      Result := SeekFromCurrent(Offset);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  begin
    Assert(Offset=0, 'Can only seek to end, current or beginning');

    Result := FPosition;
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to end, current or beginning');

    Result := FPosition;
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := FPosition;
  end;
end;

procedure TWriteableZLIBDecompressionFilter.SetNextFilter(
  const Filter: IStreamFilter);
begin
  if not Assigned(Filter) then
    FlushEnd;

  inherited;
end;

function TWriteableZLIBDecompressionFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  Left      : Integer;
  Room      : Integer;
  ToCopy    : Integer;
  zrc       : Integer;
begin
  Assert(@Buffer <> nil);

  BufferPtr := @Buffer;
  Left := Count;

  Result := 0;
  while Left > 0 do
  begin
    Room := FBufferSize - FZS.avail_in;
    ToCopy := Min(Room, Left);

    if ToCopy > 0 then
    begin
      Move(BufferPtr^, (FZS.next_in + FZS.avail_in)^, ToCopy);
      Inc(FZS.avail_in, ToCopy);
      Inc(BufferPtr, ToCopy);
      Inc(FPosition);
      Dec(Left, ToCopy);
      Inc(Result, ToCopy);
    end else begin
      zrc := DecompressBuffer(0);
      if zrc <> Z_OK then
        Break;
    end;
  end;
end;

procedure TWriteableZLIBDecompressionFilter.WriteOutput;
var
  wsf     : IWriteableStreamFilter;
  Written : Cardinal;
  ToWrite : Cardinal;
begin
  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) = S_OK then
  begin
    ToWrite := FBufferSize - FZS.avail_out;
    Written := wsf.Write(FOutputBuf^, ToWrite);
    if Written = ToWrite then
    begin
      FZS.next_out := FOutputBuf;
      FZS.avail_out := FBufferSize;
    end else begin
      Move((FOutputBuf + Written)^, FOutputBuf^, ToWrite-Written);
      FZS.next_out := FOutputBuf + (ToWrite-Written);
      FZS.avail_out := FBufferSize - (ToWrite-Written);
    end;
  end else
    raise EStreamWrite.Create('Next filter not writeable in call to WriteOutput');
end;

{ TReadableZLIBCompressionFilter }

function TReadableZLIBCompressionFilter.CompressData: Integer;
var
  Room  : Cardinal;
  Ptr   : Pointer;
  rsf   : IReadableStreamFilter;
  More  : Cardinal;
begin
  if FZS.avail_in > 0 then
  begin
    if FZS.next_in > FInputBuf then
    begin
      Move(FZS.next_in^, FInputBuf^, FZS.avail_in);
      FZS.next_in := FInputBuf;
    end;

    Room := FBufferSize - FZS.avail_in;
    Ptr := FInputBuf + FZS.avail_in;
  end else begin
    Room := FBufferSize;
    Ptr := FInputBuf;
    FZS.next_in := FInputBuf;
  end;

  if NextFilter.QueryInterface(IReadableStreamFilter, rsf) <> S_OK then
    raise EStreamRead.Create('Next filter not readable in call to Seek');

  More := rsf.Read(Ptr^, Room);
  FZS.avail_in := FZS.avail_in + More;

  FZS.next_out := FOutputBuf;
  FZS.avail_out := FBufferSize;
  if More < Room then
    Result := deflate(FZS, Z_FINISH)
  else
    Result := deflate(FZS, 0);
end;

constructor TReadableZLIBCompressionFilter.Create(
  const BufferSize: Cardinal; const WriteHeader: Boolean;
  const CompressionLevel: TCompressionLevel);
begin
  inherited Create(BufferSize);

  FCompressionLevel := CompressionLevel;
  FWriteHeader := WriteHeader;

	if FWriteHeader then
    deflateInit(FZS, zLibLevels[CompressionLevel])
  else
    deflateInit2(FZS, zLibLevels[CompressionLevel], Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY);
end;

destructor TReadableZLIBCompressionFilter.Destroy;
begin
  deflateEnd(FZS);

  inherited;
end;

function TReadableZLIBCompressionFilter.GetCompressionRatio: Double;
begin
  if GetTotalInput = 0 then
    Result := 1.0
  else
    Result := GetTotalOutput / GetTotalInput;
end;

function TReadableZLIBCompressionFilter.Read(var Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  Left      : Integer;
  Available : Integer;
  ToCopy    : Integer;
  zrc       : Integer;
begin
  BufferPtr := @Buffer;
  Left := Count;

  Result := 0;
  while Left > 0 do
  begin
    Available := FBufferSize - FZS.avail_out;
    ToCopy := Min(Left, Available);

    if ToCopy > 0 then
    begin
      Move(FOutputBuf^, BufferPtr^, ToCopy);
      Inc(BufferPtr, ToCopy);
      Dec(Left, ToCopy);
      Inc(Result, ToCopy);
      Inc(FPosition, ToCopy);
      Inc(FZS.avail_out, ToCopy);
      if FZS.avail_out < FBufferSize then
      begin
        Move((FOutputBuf + ToCopy)^, FOutputBuf^, FBufferSize-FZS.avail_out);
        FZS.next_out := FOutputBuf + (FBufferSize-FZS.avail_out);
      end;
    end else begin
      if FAtEnd then
        Break;
      zrc := CompressData;
      if zrc = Z_STREAM_END then
        FAtEnd := True
      else if zrc <> Z_OK then
        Break;
    end;
  end;
end;

function TReadableZLIBCompressionFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromCurrent(const Offset: Int64): Int64; forward;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Result := ssf.Seek(0, soBeginning);
    FPosition := 0;
    FAtEnd := False;
    deflateEnd(FZS);
    ResetFZS;

    if FWriteHeader then
      deflateInit(FZS, zLibLevels[FCompressionLevel])
    else
      deflateInit2(FZS, zLibLevels[FCompressionLevel], Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY);

    if Offset > 0 then
      Result := SeekFromCurrent(Offset);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  var
    Buffer  : array[0..2047] of Byte;
    Left    : Int64;
  begin
    if Offset < 0 then
      Result := SeekFromBeginning(FPosition + Offset)
    else begin
      Left := Offset;
      while Left > 0 do
        Dec(Left, Read(Buffer, Min(SizeOf(Buffer), Left)));

      Result := FPosition;
    end;
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  var
    Buffer  : array[0..2047] of Byte;
  begin
    while Read(Buffer, SizeOf(Buffer)) > 0 do
      ;

    if Offset = 0 then
      Result := FPosition
    else
      Result := SeekFromBeginning(FPosition + Offset);
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := FPosition;
  end;
end;

end.
