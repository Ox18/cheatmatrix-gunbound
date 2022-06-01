{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code to read and write streams through ReadLn, meaning
    you can now read single lines of text from any TStream-descendant class.
}
unit lvkTextStream;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkTextStream.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes;

{ Description:
    This procedure assigns the stream to the file. After this you can
    Reset or Rewrite the file and start using ReadLn or WriteLn, along with
    Eof or similar functions to operate on the file.

    Be sure to call CloseFile as normal to flush any internal buffers and
    clean up the buffers.
  Parameters:
    F - The file variable to associate the stream with.
    Stream - The stream to associate the file variable with.
    BufferSize - The size of the buffer to use. If left at 0 (default), then
      the default buffer (260 bytes) will be used. A good buffer is an
      increment of 4kb, so 4096, 8192, or upwards will increase the speed.
}
procedure AssignStream(var F: TextFile; const Stream: TStream;
  const BufferSize: Cardinal=0);

implementation

uses
  Windows;

type
  TUserData = packed record
    Stream      : TStream;
    BufferSize  : Cardinal;
    Buffer      : PChar;
    Filler      : array[1..20] of Byte;
  end;

function ReadFromStream(var F: TTextRec): Integer;
var
  Stream  : TStream;
begin
  Stream := TUserData(F.UserData).Stream;

  F.BufEnd := Stream.Read(F.BufPtr^, F.BufSize);
  F.BufPos := 0;
  Result := 0;
end;

function WriteToStream(var F: TTextRec): Integer;
var
  Stream  : TStream;
begin
  Stream := TUserData(F.UserData).Stream;

  Stream.WriteBuffer(F.BufPtr^, F.BufPos);
  F.BufPos := 0;
  Result := 0;
end;

function OpenStream(var F: TTextRec): Integer;

  procedure FixupBuffer(var F: TTextRec);
  begin
    if TUserData(F.UserData).BufferSize > 0 then
    begin
      GetMem(TUserData(F.UserData).Buffer, TUserData(F.UserData).BufferSize);
      F.BufPos := 0;
      F.BufEnd := 0;
      F.BufPtr := TUserData(F.UserData).Buffer;
      F.BufSize := TUserData(F.UserData).BufferSize;
    end else begin
      F.BufPos := 0;
      F.BufEnd := 0;
      F.BufPtr := @F.Buffer;
      F.BufSize := SizeOf(F.Buffer);
    end;
  end;

begin
  case F.Mode of
    fmInput:
      begin
        FixupBuffer(F);
        F.InOutFunc := @ReadFromStream;
        F.FlushFunc := nil;
        TUserData(F.UserData).Stream.Position := 0;
        Result := 0;
      end;

    fmOutput:
      begin
        FixupBuffer(F);
        F.InOutFunc := @WriteToStream;
        F.FlushFunc := @WriteToStream;
        TUserData(F.UserData).Stream.Position := 0;
        TUserData(F.UserData).Stream.Size := 0;
        Result := 0;
      end;

  else
    Result := 1;
  end;
end;

function CloseStream(var F: TTextRec): Integer;
begin
  TUserData(F.UserData).Stream.Position := 0;
  F.InOutFunc := nil;
  F.FlushFunc := nil;
  Result := 0;

  if TUserData(F.UserData).BufferSize > 0 then
  begin
    FreeMem(TUserData(F.UserData).Buffer);
    TUserData(F.UserData).Buffer := nil;
  end;
end;

procedure AssignStream(var F: TextFile; const Stream: TStream;
  const BufferSize: Cardinal);
begin
  Assert(Assigned(Stream));

  ZeroMemory(@F, SizeOf(F));

  TUserData(TTextRec(F).UserData).Stream := Stream;
  TUserData(TTextRec(F).UserData).BufferSize := BufferSize;

  TTextRec(F).Mode := fmClosed;
  TTextRec(F).CloseFunc := @CloseStream;
  TTextRec(F).OpenFunc := @OpenStream;

  if BufferSize = 0 then
  begin
    TTextRec(F).BufPtr := @TTextRec(F).Buffer;
    TTextRec(F).BufSize := SizeOf(TTextRec(F).Buffer);
  end else
    TTextRec(F).BufSize := BufferSize;
end;

end.
