{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains an implementation of the Burrows-Wheeler Transform.
}
unit lvkBWTFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBWTFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFNDEF DELPHI6UP}
  Windows,
  {$ENDIF}
  SysUtils, Classes, lvkStreamFilters;

const
  DEFAULT_BUFFER_SIZE = 16384;

{ Description:
    This function returns a writeable filter that implements the Burrows Wheeler
    transform. Write data to this and transformed data will be written to the
    next filter.
}
function NewWriteableBWTTransform(
  const BufferSize: Word=DEFAULT_BUFFER_SIZE): IStreamFilter;

implementation

uses
  Math, lvkBWT, lvkBasicStreamFilters;

type
  TBaseBWTFilter = class(TlvkBaseStreamFilter)
  private
    FInputBuffer  : PChar;
    FOutputBuffer : PChar;
    FBufferSize   : Word;
    FPosition     : Int64;

  public
    constructor Create(const BufferSize: Word);
    destructor Destroy; override;
  end;

  TWriteableBWTTransform = class(TBaseBWTFilter, IWriteableStreamFilter, ISeekableStreamFilter)
  private
    FNext : PChar;

    procedure TransformBuffer;

  protected
    // IWriteableStreamFilter
    function Write(const Buffer; const Count: Longint): Longint;
    procedure Flush;

    // ISeekableStreamFilter
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64;

  public
    constructor Create(const BufferSize: Word);
  end;

function NewWriteableBWTTransform(const BufferSize: Word): IStreamFilter;
begin
  Result := TWriteableBWTTransform.Create(BufferSize) as IStreamFilter;
end;

{ TBaseBWTFilter }

constructor TBaseBWTFilter.Create(const BufferSize: Word);
begin
  inherited Create;

  FBufferSize := BufferSize;
  GetMem(FInputBuffer, FBufferSize);
  GetMem(FOutputBuffer, FBufferSize);
  FPosition := 0;
end;

destructor TBaseBWTFilter.Destroy;
begin
  FreeMem(FInputBuffer);
  FreeMem(FOutputBuffer);

  inherited;
end;

{ TWriteableBWTTransform }

constructor TWriteableBWTTransform.Create(const BufferSize: Word);
begin
  inherited Create(BufferSize);

  FNext := FInputBuffer;
end;

procedure TWriteableBWTTransform.Flush;
begin
  if FNext > FInputBuffer then
    TransformBuffer;
end;

function TWriteableBWTTransform.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromCurrent(const Offset: Int64): Int64; forward;

  function SeekFromBeginning(const Offset: Int64): Int64;
  begin
    Assert(Offset = 0, 'Can only seek to end, current or beginning');

    Result := ssf.Seek(0, soBeginning);
    FPosition := 0;
    FNext := FInputBuffer;

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

procedure TWriteableBWTTransform.TransformBuffer;
var
  wsf     : IWriteableStreamFilter;
  Temp    : Word;

  procedure Write(const Data; const Size: Integer);
  var
    Written : Integer;
  begin
    Written := wsf.Write(Data, Size);
    if Written <> Size then
      raise EStreamWrite.Create('Unable to complete write in call to TransformBuffer');
  end;

begin
  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) <> S_OK then
    raise EStreamSeek.Create('Next filter not writeable in call to Seek');

  Temp := FNext-FInputBuffer;
  Write(Temp, SizeOf(Temp));

  Temp := BWT_Transform(FInputBuffer, FOutputBuffer, FNext-FInputBuffer);
  Write(Temp, SizeOf(Temp));

  BWT_MoveToFront(FOutputBuffer, FNext-FInputBuffer);

  Write(FOutputBuffer^, FNext-FInputBuffer);

  FNext := FInputBuffer;
end;

function TWriteableBWTTransform.Write(const Buffer;
  const Count: Integer): Longint;
var
  BufferPtr : PChar;
  Left      : Integer;
  Room      : Integer;
  ToCopy    : Integer;
begin
  BufferPtr := @Buffer;
  Left := Count;
  Result := 0;

  while Left > 0 do
  begin
    Room := FBufferSize - (FNext - FInputBuffer);
    ToCopy := Min(Left, Room);

    if ToCopy > 0 then
    begin
      Move(BufferPtr^, FNext^, ToCopy);
      Inc(FNext, ToCopy);
      Inc(BufferPtr, ToCopy);
      Dec(Left, ToCopy);
      Inc(FPosition, ToCopy);
      Inc(Result, ToCopy);
    end else
      TransformBuffer;
  end;
end;

end.
 