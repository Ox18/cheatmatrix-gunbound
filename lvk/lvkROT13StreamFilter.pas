{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of the simple ROT13
    "encryption" algorithm.
}
unit lvkROT13StreamFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkROT13StreamFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkStreamFilters;

{ Description:
    This function creates and returns a new streamfilter for the
    ROT13 "encryption" algorithm.

    Since the ROT13 algorithm is reversible, and uses no key (the value 13
    is the key), it doesn't matter if you read or write, encrypt or decrypt,
    the same filter applies in all cases.
}
function NewROT13Filter: IStreamFilter;

implementation

uses
  lvkBasicStreamFilters;
  
var
  ROT13Table  : array[Char] of Char;

type
  TlvkROT13StreamFilter = class(TlvkCompleteStreamFilter)
  private
    procedure ROT13(var Data; const Size: Longint);

  protected
    // IReadableStreamFilter interface
    function Read(var Buffer; const Count: Longint): Longint; override;

    // IWriteableStreamFilter interface
    function Write(const Buffer; const Count: Longint): Longint; override;
  end;

procedure InitializeROT13Table;
var
  c : Char;

  procedure Fixup(const s: string);
  var
    i       : Integer;
    c1, c2  : Char;
  begin
    Assert(Length(s) = 26);
    for i := 1 to 13 do
    begin
      c1 := s[i];
      c2 := s[i+13];

      ROT13Table[c1] := c2;
      ROT13Table[c2] := c1;
    end;
  end;

begin
  for c := #0 to #255 do
    ROT13Table[c] := c;

  Fixup('abcdefghijklmnopqrstuvwxyz');
  Fixup('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
end;

function NewROT13Filter: IStreamFilter;
begin
  Result := TlvkROT13StreamFilter.Create as IStreamFilter;
end;

{ TlvkROT13StreamFilter }

function TlvkROT13StreamFilter.Read(var Buffer;
  const Count: Integer): Longint;
begin
  Result := inherited Read(Buffer, Count);
  if Result > 0 then
    ROT13(Buffer, Count);
end;

procedure TlvkROT13StreamFilter.ROT13(var Data; const Size: Integer);
var
  Index   : Cardinal;
  DataPtr : PChar;
begin
  Assert(Size > 0);
  Assert(@Data <> nil);

  DataPtr := @Data;
  for Index := 0 to Pred(Size) do
    DataPtr[Index] := ROT13Table[DataPtr[Index]];
end;

function TlvkROT13StreamFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  Temp  : PChar;
begin
  if Count > 0 then
  begin
    GetMem(Temp, Count);
    try
      Move(Buffer, Temp^, Count);
      ROT13(Temp^, Count);

      Result := inherited Write(Temp^, Count);
    finally
      FreeMem(Temp);
    end;
  end else
    Result := 0;
end;

initialization
  InitializeROT13Table;
end.
