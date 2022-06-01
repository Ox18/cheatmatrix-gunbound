{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains simple functions for BWT support
    (Burrows-Wheeler Transform) which can be used together with certain forms
    of compression algorithms.
}
unit lvkBWT;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBWT.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    This function performs a Burrows-Wheeler Transform (BWT) on a block of data,
    copying the transformed data to the Output memory area.
  Parameters:
    Input   - Pointer to the data to transform.
    Output  - Pointer to where the transformed data are to be stored.
    Size    - Number of bytes to transform from Input to Output.
  See also:
    BWT_MoveToFront
}
function BWT_Transform(const Input, Output: PChar; const Size: Word): Word;

{ Description:
    This function replaces data in such a way that the most used byte values
    are replaced with values close to 0.
  Parameters:
    Buffer  - Pointer to the buffer to transform.
    Size    - Number of bytes in the buffer.
  See also:
    BWT_Transform
}
procedure BWT_MoveToFront(const Buffer: PChar; const Size: Word);

{ TODO 2 -oLVK -cSource : Write untransform functions as well }

implementation

type
  PPointerList = ^TPointerList;
  TPointerList = array[Word] of PChar;

const
  QSCutOff  = 15;

{function CompareMem(const p1, p2: PChar; const Size: Integer): Integer;
var
  sp1, sp2  : PChar;
  Left      : Integer;
begin
  sp1 := p1;
  sp2 := p2;
  Left := Size;

  while (Left > 0) and (sp1^ = sp2^) do
  begin
    Dec(Left);
    Inc(sp1);
    Inc(sp2);
  end;

  if Left = 0 then
    Result := 0
  else if sp1^ < sp2^ then
    Result := -1
  else
    Result := +1;
end;

procedure QuickSort(var PointerList: TPointerList; L, R, Size: Integer);
var
  I, J, P : Integer;
  Temp    : PChar;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while CompareMem(PointerList[I], PointerList[P], Size) < 0 do Inc(I);
      while CompareMem(PointerList[J], PointerList[P], Size) > 0 do Dec(J);
      if I <= J then
      begin
        Temp := PointerList[I];
        PointerList[I] := PointerList[J];
        PointerList[J] := Temp;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(PointerList, L, J, Size);
    L := I;
  until I >= R;
end; }

function CompareBlocks(aData1, aData2 : pointer;
                       aSize : integer) : integer;
var
  Data1 : PChar;
  Data2 : PChar;
  i     : integer;
begin
  Data1 := aData1;
  Data2 := aData2;
  i := aSize;
  while (i > 0) and (Data1^ = Data2^) do begin
    dec(i);
    inc(Data1);
    inc(Data2);
  end;
  if (i = 0) then
    Result := 0
  else if (Data1^ < Data2^) then
    Result := -1
  else
    Result := +1;
end;

procedure QSInsertionSort(aList    : PPointerList;
                          aFirst   : integer;
                          aLast    : integer;
                          aSize    : integer);
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  {find the smallest element in the first QSCutOff items and put it in
   the first position}
  IndexOfMin := aFirst;
  j := QSCutOff;
  if (j > aLast) then
    j := aLast;
  for i := succ(aFirst) to j do
    if (CompareBlocks(aList^[i], aList^[IndexOfMin], aSize) < 0) then
      IndexOfMin := i;
  if (aFirst <> IndexOfMin) then begin
    Temp := aList^[aFirst];
    aList^[aFirst] := aList^[IndexOfMin];
    aList^[IndexOfMin] := Temp;
  end;
  {now sort via fast insertion method}
  for i := aFirst+2 to aLast do begin
    Temp := aList^[i];
    j := i;
    while (CompareBlocks(Temp, aList^[j-1], aSize) < 0) do begin
      aList^[j] := aList^[j-1];
      dec(j);
    end;
    aList^[j] := Temp;
  end;
end;
{--------}
procedure QS(aList    : PPointerList;
             aFirst   : integer;
             aLast    : integer;
             aSize    : integer);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
begin
  while ((aLast - aFirst) > QSCutOff) do begin
    {sort the first, middle and last items, then set the pivot to the
     middle one - the median-of-3 method}
    R := (aFirst + aLast) div 2;
    if (CompareBlocks(aList^[aFirst], aList^[R], aSize) > 0) then begin
      Temp := aList^[aFirst];
      aList^[aFirst] := aList^[R];
      aList^[R] := Temp;
    end;
    if (CompareBlocks(aList^[aFirst], aList^[aLast], aSize) > 0) then begin
      Temp := aList^[aFirst];
      aList^[aFirst] := aList^[aLast];
      aList^[aLast] := Temp;
    end;
    if (CompareBlocks(aList^[R], aList^[aLast], aSize) > 0) then begin
      Temp := aList^[R];
      aList^[R] := aList^[aLast];
      aList^[aLast] := Temp;
    end;
    Pivot := aList^[R];
    {set indexes and partition}
    L := aFirst;
    R := aLast;
    while true do begin
      repeat dec(R); until (CompareBlocks(aList^[R], Pivot, aSize) <= 0);
      repeat inc(L); until (CompareBlocks(aList^[L], Pivot, aSize) >= 0);
      if (L >= R) then Break;
      Temp := aList^[L];
      aList^[L] := aList^[R];
      aList^[R] := Temp;
    end;
    {quicksort the first subfile}
    QS(aList, aFirst, R, aSize);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end;
{--------}
procedure Quicksort(aList    : PPointerList;
                    aFirst   : integer;
                    aLast    : integer;
                    aSize    : integer);
begin
  QS(aList, aFirst, aLast, aSize);
  QSInsertionSort(aList, aFirst, aLast, aSize);
end;

// Size intentionally set to Word to avoid larger buffers by mistake
function BWT_Transform(const Input, Output: PChar; const Size: Word): Word;
var
  TempInput   : PChar;
  PointerList : PPointerList;
  Index       : Integer;
begin
  Assert(Assigned(Input));
  Assert(Assigned(Output));
  Assert(Size>0);

  GetMem(PointerList, SizeOf(PChar) * Size);
  try
    GetMem(TempInput, Size*2);
    try
      Move(Input^, TempInput^, Size);
      Move(Input^, (TempInput+Size)^, Size);

      for Index := 0 to Pred(Size) do
        PointerList^[Index] := TempInput + Index;

      QuickSort(PointerList, 0, Pred(Size), Size);

      for Index := 0 to Pred(Size) do
        Output[Index] := PointerList^[Index][Pred(Size)];
      Result := 0;
    finally
      FreeMem(TempInput);
    end;
  finally
    FreeMem(PointerList);
  end;
end;

// Size intentionally set to Word to avoid larger buffers by mistake
procedure BWT_MoveToFront(const Buffer: PChar; const Size: Word);
type
  TCharArray = array[Char] of Char;
var
  Chars     : TCharArray;
  Ch1, Ch2  : Char;
  Index     : Integer;
begin
  for Ch1 := #0 to #255 do
    Chars[Ch1] := Ch1;

  for Index := 0 to Pred(Size) do
  begin
    Ch1 := Buffer[Index];
    for Ch2 := #0 to #255 do
    begin
      if Chars[Ch2] = Ch1 then
      begin
        Buffer[Index] := Ch2;
        if Ch2 > #0 then
        begin
          Move(Chars[#0], Chars[#1], Ord(Ch2));
          Chars[#0] := Ch1;
        end;
        Break;
      end;
    end;
  end;
end;

end.
