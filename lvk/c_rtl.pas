{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2003, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    Contains "C Runtime Library" functions for use with libraries written in
    C and linked to the component library.
}
unit c_rtl;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 10 $
// $Archive: /Components/LVK/source/c_rtl.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    malloc allocates a number of bytes on the heap and returns a pointer to
    the first byte.
  Parameters:
    size  - The number of bytes to allocate on the heap.
  Returns:
    Pointer to first byte allocated.
  See also:
    free, memset, memMove, strncmp
}
function malloc(size: Cardinal): Pointer; cdecl;
// <COMBINE malloc@Cardinal>
function _malloc(size: Cardinal): Pointer; cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    free returns the allocated block of memory back to the free blocks on the
    heap.
  Parameters:
    p - Pointer to the first byte in the block to return.
  See also:
    malloc, memset, memMove, strncmp
}
procedure free(p: Pointer); cdecl;
// <COMBINE free@Pointer>
procedure _free(p: Pointer); cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    memset sets the block pointer to by src to contain only characters with the
    value of c, for n bytes in total.
  Parameters:
    src - Pointer to the first byte to set.
    c   - The value to set the bytes to.
    n   - How many bytes to set.
  Returns:
    Returns src.
  See also:
    malloc, free, memMove, strncmp
}
function memset(src: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;
// <COMBINE memset@Pointer@Integer@Cardinal>
function _memset(src: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    memMove/memcpy copies a number of bytes from the memory pointed to by src, to
    the memory pointed to by dest.
  Parameters:
    dest  - Where to copy the bytes to.
    src   - Where to copy the bytes from.
    n     - Number of bytes to copy.
  Returns:
    Returns dest.
  See also:
    malloc, free, memset, memcpy, strncmp
}
function memMove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
// <COMBINE memMove@Pointer@Pointer@Cardinal>
function _memMove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;

// <ALIAS memMove>
function memcpy(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
// <COMBINE memcpy@Pointer@Pointer@Cardinal>
function _memcpy(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    strncmp compares bytes at two memory locations and returns an integer
    describing their relationship. It is functionally equivalent to a string
    comparison, up to at most MaxLen characters, and it will stop at the
    first #0 in either string.
  Parameters:
    s1      - The first string to compare.
    s2      - The string that s1 will be compared against.
    MaxLen  - Number of characters to compare at maximum.
  Returns:
    < 0 if s1 is less than s2.
    = 0 if s1 is equal to s2.
    > 0 if s1 is greater than s2.
  See also:
    malloc, free, memset, memMove, strcmp
}
function strncmp(const s1, s2: PChar; const MaxLen: Integer): Integer; cdecl;
// <COMBINE strncmp@PChar@PChar@Integer>
function _strncmp(const s1, s2: PChar; const MaxLen: Integer): Integer; cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    strncmp compares bytes at two memory locations and returns an integer
    describing their relationship. It is functionally equivalent to a string
    comparison and it will stop at the first #0 in either string.
  Parameters:
    s1 - The first string to compare.
    s2 - The string that s1 will be compared against.
  Returns:
    < 0 if s1 is less than s2.
    = 0 if s1 is equal to s2.
    > 0 if s1 is greater than s2.
  See also:
    strncmp
}
function strcmp(const s1, s2: PChar): Integer; cdecl;
// <COMBINE strcmp@PChar@PChar>
function _strcmp(const s1, s2: PChar): Integer; cdecl;

{ Description:
    Part of the "C Runtime Library" code for my components. The purpose of the
    functions in this unit is to provide some runtime functions for the two
    C libraries I've used, PCRE (regular expressions) and zLib (compression).

    memcmp compares bytes at two memory locations and returns an integer
    describing their relationship.
  Parameters:
    buf1  - Pointer to the first memory location.
    buf2  - Pointer to the second memory location.
    count - Number of bytes to compare in the two locations.
  See also:
    malloc, free, memset, memMove
}
function memcmp(const buf1, buf2: PChar; const count: Integer): Integer; cdecl;
// <COMBINE memcmp@PChar@PChar@Integer>
function _memcmp(const buf1, buf2: PChar; const count: Integer): Integer; cdecl;

implementation

uses
  SysUtils;

function _malloc(size: Cardinal): Pointer; cdecl;
begin
  Result := malloc(size);
end;

function malloc(size: Cardinal): Pointer; cdecl;
begin
  Result := AllocMem(size);
end;

procedure _free(p: Pointer); cdecl;
begin
  free(p);
end;

procedure free(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

function _memset(src: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;
begin
  Result := memset(src, c, n);
end;

function memset(src: Pointer; c: Integer; n: Cardinal): Pointer; cdecl;
begin
  fillchar(src^,n,c);
  Result := src;
end;

function _memMove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Result := memMove(dest, src, n);
end;

function memMove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Move(src^,dest^,n);
  Result := dest;
end;

function _memcpy(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Result := memcpy(dest, src, n);
end;

function memcpy(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  Move(src^,dest^,n);
  Result := dest;
end;

function _strncmp(const s1, s2: PChar; const MaxLen: Integer): Integer; cdecl;
begin
  Result := strncmp(s1, s2, MaxLen);
end;

function strncmp(const s1, s2: PChar; const MaxLen: Integer): Integer; cdecl;
begin
  Result := StrLComp(s1, s2, MaxLen);
end;

function memcmp(const buf1, buf2: PChar; const count: Integer): Integer; cdecl;
var
  i : Integer;
begin
  i := 0;
  Result := 0;
  while (i < count) and (Result = 0) do
  begin
    if buf1[i] < buf2[i] then
      Result := -1
    else if buf1[i] > buf2[i] then
      Result := +1;
    Inc(i);
  end;
end;

function _memcmp(const buf1, buf2: PChar; const count: Integer): Integer; cdecl;
begin
  Result := memcmp(buf1, buf2, count);
end;

function strcmp(const s1, s2: PChar): Integer; cdecl;
begin
  Result := StrComp(s1, s2);
end;

function _strcmp(const s1, s2: PChar): Integer; cdecl;
begin
  Result := StrComp(s1, s2);
end;

end.
