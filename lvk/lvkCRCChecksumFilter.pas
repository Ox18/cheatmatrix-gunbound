{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of CRC16 and CRC32 checksum
    filters.
}
unit lvkCRCChecksumFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCRCChecksumFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkStreamFilters;

{ Description:
    This function returns a stream filter that implements the CRC16
    checksum algorithm.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    NewCRC32ChecksumFilter, IChecksumStreamFilter
}
function NewCRC16ChecksumFilter: IStreamFilter;

{ Description:
    This function returns a stream filter that implements the CRC32
    checksum algorithm.

    The interface supports the IChecksumStreamFilter interface.
  See also:
    NewCRC16ChecksumFilter, IChecksumStreamFilter
}
function NewCRC32ChecksumFilter: IStreamFilter;

implementation

uses
  lvkBasicChecksumFilters, lvkCRC;

type
  TCRC16ChecksumFilter = class(TlvkBaseChecksumFilter,
    IReadableStreamFilter, IWriteableStreamFilter)
  private
    FChecksum : TCRC16;
    
  protected
    // Internal
    procedure Process(const Data; const DataSize: Integer); override;
    procedure ResetChecksum; override;
    function CalculateChecksum: TChecksumBytes; override;
  end;

  TCRC32ChecksumFilter = class(TlvkBaseChecksumFilter,
    IReadableStreamFilter, IWriteableStreamFilter)
  private
    FChecksum : TCRC32;

  protected
    // Internal
    procedure Process(const Data; const DataSize: Integer); override;
    procedure ResetChecksum; override;
    function CalculateChecksum: TChecksumBytes; override;
  end;
  
function NewCRC16ChecksumFilter: IStreamFilter;
begin
  Result := TCRC16ChecksumFilter.Create as IStreamFilter;
end;

function NewCRC32ChecksumFilter: IStreamFilter;
begin
  Result := TCRC32ChecksumFilter.Create as IStreamFilter;
end;

{ TCRC16ChecksumFilter }

function TCRC16ChecksumFilter.CalculateChecksum: TChecksumBytes;
var
  OutSum  : TCRC16;
begin
  OutSum := FCheckSum;
  CRC16Done(OutSum);
  
  SetLength(Result, 2);
  Result[0] := OutSum shr 8;
  Result[1] := OutSum and 255;
end;

procedure TCRC16ChecksumFilter.Process(const Data;
  const DataSize: Integer);
begin
  CRC16Process(FCheckSum, Data, DataSize);
end;

procedure TCRC16ChecksumFilter.ResetChecksum;
begin
  CRC16Init(FCheckSum);
end;

{ TCRC32ChecksumFilter }

function TCRC32ChecksumFilter.CalculateChecksum: TChecksumBytes;
var
  OutSum  : TCRC32;
begin
  OutSum := FCheckSum;
  CRC32Done(OutSum);

  SetLength(Result, 4);
  Result[0] := (OutSum shr 24) and $FF;
  Result[1] := (OutSum shr 16) and $FF;
  Result[2] := (OutSum shr 8) and $FF;
  Result[3] := OutSum and $FF;
end;

procedure TCRC32ChecksumFilter.Process(const Data;
  const DataSize: Integer);
begin
  CRC32Process(FCheckSum, Data, DataSize);
end;

procedure TCRC32ChecksumFilter.ResetChecksum;
begin
  CRC32Init(FCheckSum);
end;

end.
