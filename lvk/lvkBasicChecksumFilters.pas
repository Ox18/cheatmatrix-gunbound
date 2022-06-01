{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the base code and classes for checksum filters.
    New checksum filter classes can inherit from one of the classes in this
    unit to get some of the functionality for free.
}
unit lvkBasicChecksumFilters;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBasicChecksumFilters.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, lvkStreamFilters, lvkBasicStreamFilters;

type
  { Description:
      When a checksum is to be calculated, it's stored in an array of bytes.
      This type is used to define parameters, function return types and
      variables to hold such checksums.
    See also:
      TlvkBaseChecksumFilter.CalculateChecksum
  }
  TChecksumBytes = array of Byte;

{ Description:
    This class is the base class for checksum filter classes. New checksum
    classes can inherit from this class and override ResetChecksum,
    Process and CalculateChecksum and get the rest of the necessary
    functionality for free from this base class.
}
  TlvkBaseChecksumFilter = class(TlvkBaseStreamFilter, IChecksumStreamFilter,
    IReadableStreamFilter, IWriteableStreamFilter, ISeekableStreamFilter)
  private
    FChecksum     : TChecksumBytes;
    FNeedsUpdate  : Boolean;

  protected
    // <ALIAS IChecksumStreamFilter.GetChecksumSize>
    function GetChecksumSize: Integer; virtual;

    // <ALIAS IChecksumStreamFilter.GetChecksumByte@Integer>
    function GetChecksumByte(const Index: Integer): Byte; virtual;

    // <ALIAS IChecksumStreamFilter.GetAsHexString>
    function GetAsHexString: string; virtual;

    // <ALIAS IReadableStreamFilter.Read@@Longint>
    function Read(var Buffer; const Count: Longint): Longint; virtual;

    // <ALIAS IWriteableStreamFilter.Write@@Longint>
    function Write(const Buffer; const Count: Longint): Longint; virtual;

    // <ALIAS IWriteableStreamFilter.Flush>
    procedure Flush; virtual;

    // <ALIAS ISeekableStreamFilter.Seek@Int64@TSeekOrigin>
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64; virtual;

    // Internal
    { Description:
        This method is called whenever the stream is seek'ed back to the
        beginning of the data in order to reset the internal checksum
        accumulators.

        Note: Descendant classes will most likely want to override this
          method.
    }
    procedure ResetChecksum; virtual;

    { Description:
        This method will be called in order to process a new block of data
        for inclusion in the checksum.

        Note: Descendant classes must override this method.
      Parameters:
        Data - The data to include in the checksum.
        DataSize - The number of bytes available in the Data parameter.
      See also:
        -
    }
    procedure Process(const Data; const DataSize: Integer); virtual; abstract;

    { Description:
        This method will be called to make sure that the checksum is up-to-date
        before outside code gets hold of the checksum value. If the checksum
        needs to be updated, it will call CalculateChecksum to get a new
        and current checksum.
    }
    procedure UpdateChecksum; virtual;

    { Description:
        This method will be called to calculate a new, current, checksum.

        Note: Descendant classes must override this method.

      Returns:
        The checksum as an array of bytes.
    }
    function CalculateChecksum: TChecksumBytes; virtual; abstract;

  public
    { Description:
        This constructor creates and initializes the checksum filter
        class.
    }
    constructor Create;
  end;

implementation

uses
  Math;

{ TlvkBaseChecksumFilter }

constructor TlvkBaseChecksumFilter.Create;
begin
  inherited Create;

  ResetChecksum;
  FNeedsUpdate := True;
end;

procedure TlvkBaseChecksumFilter.Flush;
begin
  // Do nothing
end;

function TlvkBaseChecksumFilter.GetAsHexString: string;
var
  Index : Integer;
  Value : Byte;
const
  HexChars  : array[0..15] of Char = '0123456789ABCDEF';
begin
  SetLength(Result, 2 * GetChecksumSize);
  for Index := 0 to GetChecksumSize-1 do
  begin
    Value := GetChecksumByte(Index);
    Result[Length(Result)-Index*2-1] := HexChars[Value shr 4];
    Result[Length(Result)-Index*2] := HexChars[Value and 15];
  end;
end;

function TlvkBaseChecksumFilter.GetChecksumByte(
  const Index: Integer): Byte;
begin
  UpdateChecksum;
  if (Index >= 0) and (Index < Length(FChecksum)) then
    Result := FChecksum[Index]
  else
    raise EStreamFilter.Create('Invalid index in call to GetChecksumByte');
end;

function TlvkBaseChecksumFilter.GetChecksumSize: Integer;
begin
  UpdateChecksum;
  Result := Length(FChecksum);
end;

function TlvkBaseChecksumFilter.Read(var Buffer;
  const Count: Integer): Longint;
var
  rsf : IReadableStreamFilter;
begin
  if NextFilter.QueryInterface(IReadableStreamFilter, rsf) <> S_OK then
    raise EStreamRead.Create('Next filter not readable in call to Read');

  Result := rsf.Read(Buffer, Count);
  if Result > 0 then
  begin
    Process(Buffer, Result);
    FNeedsUpdate := True;
  end;
end;

procedure TlvkBaseChecksumFilter.ResetChecksum;
begin
  // Do nothing
end;

function TlvkBaseChecksumFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;

  function SeekFromBeginning(const Offset: Int64): Int64;
  var
    Position  : Int64;
    Amount    : Integer;
    Buffer    : array[0..2047] of Char;
  begin
    ssf.Seek(0, soBeginning);
    ResetChecksum;
    FNeedsUpdate := True;

    Position := 0;
    while Position < Offset do
    begin
      Amount := Read(Buffer, Min(SizeOf(Buffer), Offset-Position));

      if Amount > 0 then
        Inc(Position, Amount)
      else
        Break;
    end;

    Result := Seek(0, soCurrent);
  end;

  function SeekFromCurrent(const Offset: Int64): Int64;
  begin
    if Offset = 0 then
      Result := ssf.Seek(0, soCurrent)
    else
      Result := SeekFromBeginning(ssf.Seek(0, soCurrent) + Offset);
  end;

  function SeekFromEnd(const Offset: Int64): Int64;
  var
    Size  : Integer;
  begin
    Size := ssf.Seek(0, soEnd);
    Result := SeekFromBeginning(Size + Offset);
  end;

begin
  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) <> S_OK then
    raise EStreamSeek.Create('Next filter not seekable in call to Seek');

  case Origin of
    soBeginning : Result := SeekFromBeginning(Offset);
    soCurrent   : Result := SeekFromCurrent(Offset);
    soEnd       : Result := SeekFromEnd(Offset);
  else
    Result := ssf.Seek(0, soCurrent);
  end;
end;

procedure TlvkBaseChecksumFilter.UpdateChecksum;
begin
  if FNeedsUpdate then
  begin
    FChecksum := CalculateChecksum;
    FNeedsUpdate := False;
  end;
end;

function TlvkBaseChecksumFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  wsf : IWriteableStreamFilter;
begin
  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) <> S_OK then
    raise EStreamWrite.Create('Next filter not writeable in call to Write');

  Result := wsf.Write(Buffer, Count);
  if Result > 0 then
  begin
    Process(Buffer, Result);
    FNeedsUpdate := True;
  end;
end;

end.
