{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code to calculate the CRC32 checksum of files, and
    other types of data.
}
unit lvkCRC32Utils;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/lvkCRC32Utils.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes,
  lvkTypes, lvkSafeMem, lvkCRC;

{ Description:
    Calculates the CRC32 checksum for the specified file.
  Parameters:
    FileName - The full path and name of the file to calculate the checksum
      for.
  See also:
    CRC32Of@TStream, CRC32Of@@UInt32, CRC32Of@ISafeMem
}
function CRC32Of(const FileName: string): TCRC32; overload;

{ Description:
    Calculates the CRC32 checksum for the specified stream.

    Note: The stream will be rewinded and the whole stream will be used
      to calculate the checksum.
  Parameters:
    Stream - The stream that holds the data to calculate the checksum for.
  See also:
    CRC32Of@string, CRC32Of@@UInt32, CRC32Of@ISafeMem
}
function CRC32Of(const Stream: TStream): TCRC32; overload;

{ Description:
    Calculates the CRC32 checksum for the given block of memory.
  Parameters:
    Data - Reference to the memory area that holds the data to calculate the
      checksum for.
    Size - The number of bytes to calculate the checksum for.
  See also:
    CRC32Of@string, CRC32Of@TStream, CRC32Of@ISafeMem
}
function CRC32Of(const Data; const Size: UInt32): TCRC32; overload;

{ Description:
    Calculates the CRC32 checksum for the safemem block.
  Parameters:
    SafeMem - The safemem block of data to calculate the checksum for.
  See also:
    CRC32Of@string, CRC32Of@TStream, CRC32Of@@UInt32
}
function CRC32Of(const SafeMem: ISafeMem): TCRC32; overload;

implementation

function CRC32Of(const FileName: string): TCRC32; overload;
var
  Stream  : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CRC32Of(Stream);
  finally
    Stream.Free;
  end;
end;

function CRC32Of(const Stream: TStream): TCRC32; overload;
var
  Buffer    : PChar;
  InBuffer  : Integer;
begin
  CRC32Init(Result);
  Stream.Position := 0;
  GetMem(Buffer, 32768);
  try
    repeat
      InBuffer := Stream.Read(Buffer^, 32768);
      if InBuffer > 0 then
        CRC32Process(Result, Buffer^, InBuffer);
    until InBuffer = 0;
  finally
    FreeMem(Buffer);
  end;
  CRC32Done(Result);
end;

function CRC32Of(const Data; const Size: UInt32): TCRC32; overload;
begin
  CRC32Init(Result);
  CRC32Process(Result, Data, Size);
  CRC32Done(Result);
end;

function CRC32Of(const SafeMem: ISafeMem): TCRC32; overload;
begin
  Result := CRC32Of(SafeMem.Stream);
end;

end.
