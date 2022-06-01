{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains simple byte-sum checksum filters.
}
unit lvkByteSumChecksumFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkByteSumChecksumFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkStreamFilters;

{ Description:
    This method creates and returns a stream filter that implements a
    8-bit byte sum checksum filter. All bytes passing through this filter
    will be summed, modulo 8 bit.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    New16bitByteSumFilter,
    New32bitByteSumFilter,
    New64bitByteSumFilter
}
function New8bitByteSumFilter: IStreamFilter;

{ Description:
    This method creates and returns a stream filter that implements a
    16-bit byte sum checksum filter. All bytes passing through this filter
    will be summed, modulo 16 bit.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    New8bitByteSumFilter,
    New32bitByteSumFilter,
    New64bitByteSumFilter
}
function New16bitByteSumFilter: IStreamFilter;

{ Description:
    This method creates and returns a stream filter that implements a
    32-bit byte sum checksum filter. All bytes passing through this filter
    will be summed, modulo 32 bit.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    New8bitByteSumFilter,
    New16bitByteSumFilter,
    New64bitByteSumFilter
}
function New32bitByteSumFilter: IStreamFilter;

{ Description:
    This method creates and returns a stream filter that implements a
    64-bit byte sum checksum filter. All bytes passing through this filter
    will be summed, modulo 64 bit.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    New8bitByteSumFilter,
    New16bitByteSumFilter,
    New32bitByteSumFilter
}
function New64bitByteSumFilter: IStreamFilter;

implementation

uses
  lvkBasicChecksumFilters;
  
type
  TByteSumFilter = class(TlvkBaseChecksumFilter)
  private
    FMask : Int64;
    FSum  : Int64;
    FSize : Integer;

  protected
    // Internal
    procedure ResetChecksum; override;
    procedure Process(const Data; const DataSize: Integer); override;
    function CalculateChecksum: TChecksumBytes; override;

  public
    constructor Create(const Size: Integer; const Mask: Int64);
  end;

function New8bitByteSumFilter: IStreamFilter;
begin
  Result := TByteSumFilter.Create(1, $00000000000000FF);
end;

function New16bitByteSumFilter: IStreamFilter;
begin
  Result := TByteSumFilter.Create(2, $000000000000FFFF);
end;

function New32bitByteSumFilter: IStreamFilter;
begin
  Result := TByteSumFilter.Create(4, $00000000FFFFFFFF);
end;

function New64bitByteSumFilter: IStreamFilter;
begin
  Result := TByteSumFilter.Create(8, $FFFFFFFFFFFFFFFF);
end;

{ TByteSumFilter }

function TByteSumFilter.CalculateChecksum: TChecksumBytes;
begin
  SetLength(Result, FSize);
  Move(FSum, Result[0], FSize);
end;

constructor TByteSumFilter.Create(const Size: Integer; const Mask: Int64);
begin
  inherited Create;

  FMask := Mask;
  FSize := Size;
  FSum := 0;
end;

{$R-,Q-}
procedure TByteSumFilter.Process(const Data; const DataSize: Integer);
var
  Index   : Integer;
  DataPtr : PChar;
begin
  DataPtr := @Data;
  for Index := 0 to DataSize-1 do
    Inc(FSum, Ord(DataPtr[Index]));
end;
{$R+,Q+}

procedure TByteSumFilter.ResetChecksum;
begin
  FSum := 0;
end;

end.
