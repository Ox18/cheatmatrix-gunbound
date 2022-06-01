{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains CRC32 and MD5 checksum calculation functions.
}
unit lvkCheckSumUtils;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCheckSumUtils.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, lvkSafeMem, lvkTypes, lvkCRC;

{ Description:
    These functions all generate the CRC32 checksum for a block of data, be it
    a file, a stream, or data in memory.
  Parameters:
    FileName  - The name of the file to generate the CRC32 checksum for.
    Stream    - The stream that holds the data to generate the CRC32 checksum
      for.
    Data      - A reference to the memory area where the data to generate the
      CRC32 checksum for is stored.
    Size      - The amount of bytes to process in the Data memory area.
    SafeMem   - The safemem data to generate the CRC32 checksum for.
  See also:
    CRC32Init, CRC32Process, CRC32Done
}
function CRC32Of(const FileName: string): TCRC32; overload;
// <COMBING CRC32Of@string>
function CRC32Of(const Stream: TStream): TCRC32; overload;
// <COMBING CRC32Of@string>
function CRC32Of(const Data; const Size: UInt32): TCRC32; overload;
// <COMBING CRC32Of@string>
function CRC32Of(const SafeMem: ISafeMem): TCRC32; overload;

type
  TMD5Signature = array[0..15] of Byte;

function MD5Of(const FileName: string): TMD5Signature; overload;
function MD5Of(const Stream: TStream): TMD5Signature; overload;
function MD5Of(const SafeMem: ISafeMem): TMD5Signature; overload;

function SignatureToString(const MD5Signature: TMD5Signature): string; overload;

implementation

uses
  SysUtils, lvkStreamFilters, lvkMD5ChecksumFilter;

function SignatureToString(const MD5Signature: TMD5Signature): string; overload;
var
  Index : Integer;
begin
  Result := '';
  for Index := Low(MD5Signature) to High(MD5Signature) do
    Result := Result + IntToHex(MD5Signature[Index], 2);
end;

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

function MD5Of(const FileName: string): TMD5Signature;
var
  Stream  : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := MD5Of(Stream);
  finally
    Stream.Free;
  end;
end;

function MD5Of(const Stream: TStream): TMD5Signature;
var
  Buffer    : ISafeMem;
  Filter    : TlvkFilteredStream;
  MD5Filter : IStreamFilter;
  Index     : Integer;
begin
  Assert(Assigned(Stream));
  Stream.Position := 0;

  MD5Filter := NewMD5ChecksumFilter;
  Filter := TlvkFilteredStream.Create(Stream, False, [MD5Filter]);
  Buffer := AllocateSafeMem(32768);
  repeat
    Buffer.Grab(Filter);
  until Buffer.Size = 0;

  Assert((MD5Filter as IChecksumStreamFilter).ChecksumSize = SizeOf(Result));

  for Index := Low(Result) to High(Result) do
    Result[Index] := (MD5Filter as IChecksumStreamFilter).ChecksumBytes[Index];
end;

function MD5Of(const SafeMem: ISafeMem): TMD5Signature;
begin
  Result := MD5Of(SafeMem.Stream);
end;

end.
