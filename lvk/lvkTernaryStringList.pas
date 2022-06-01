{ TODO 2 -oLVK -cSource : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the ternary stringlist class.
}
unit lvkTernaryStringList;

// $Author: Lasse V. Karlsen $
// $Revision: 3 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkTernaryStringList.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkHeap, SysUtils, Classes;

type
  TStringVisitMethod = procedure(const StringValue: PChar;
    const StringLength: Integer; const StringData: Integer;
    const TraverseData: Pointer) of object;

  IlvkTernaryStringList = interface
    ['{ABEFE963-E45D-4D6B-8A8A-BCFB7A53E028}']

    procedure Add(const Str: PChar; const Data: Integer=0);

    procedure Visit(const StringVisitMethod: TStringVisitMethod;
      const Data: Pointer=nil);

    function Exists(const Str: PChar): Boolean;
    function GetData(const Str: PChar; out Data: Integer): Boolean;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetSize: LongWord;
    property Size: LongWord read GetSize;

    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure Fill(const Strings: TStrings);

    procedure Balance;
  end;

function NewTernaryStringList(const MemoryBased: Boolean=True;
  const StoreData: Boolean=False;
  const InitialSize: Cardinal=65536;
  const GrowthFactor: Double=2.0): IlvkTernaryStringList;

implementation

uses
  lvkSafeMem;

const
  MAX_WORD_LENGTH = 256;

type
  PTreeNode = ^TTreeNode;
  TTreeNode = packed record
    Ch                : Char;
    WordStop          : Boolean;
    Left, Right, Same : LongWord;
    Data              : LongWord;
  end;

  PBalance = ^TBalance;
  TBalance = record
    Heap      : IlvkHeap;
    Size      : Integer;
    Position1 : Integer;
    Position2 : Integer;
  end;

  TTernaryStringList = class(TInterfacedObject, IlvkTernaryStringList)
  private
    FMemoryBased  : Boolean;
    FInitialSize  : Cardinal;

    FHeap         : IlvkHeap;
    FCount        : Integer;
    FStoreData    : Boolean;

    function NewNode(out Index: LongWord): PTreeNode;
    procedure AddToStrings(const StringValue: PChar;
      const StringLength: Integer; const StringData: Integer;
      const VisitData: Pointer);
    procedure SaveStringToStream(const StringValue: PChar;
      const StringLength: Integer; const StringData: Integer;
      const VisitData: Pointer);

    procedure BalanceSumSize(const StringValue: PChar;
      const StringLength: Integer; const StringData: Integer;
      const VisitData: Pointer);
    procedure BalanceAddWord(const StringValue: PChar;
      const StringLength: Integer; const StringData: Integer;
      const VisitData: Pointer);

  protected
    // IlvkTernaryStringList interface
    procedure Visit(const StringVisitMethod: TStringVisitMethod;
      const Data: Pointer=nil);
    procedure Add(const Str: PChar; const Data: Integer=0);
    function Exists(const Str: PChar): Boolean;
    function GetData(const Str: PChar; out Data: Integer): Boolean;
    function GetCount: Integer;
    function GetSize: LongWord;
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure Fill(const Strings: TStrings);
    procedure Balance;
    
  public
    constructor Create(const MemoryBased, StoreData: Boolean;
      const InitialSize: Cardinal; const GrowthFactor: Double);
  end;

function NewTernaryStringList(const MemoryBased, StoreData: Boolean;
  const InitialSize: Cardinal;
  const GrowthFactor: Double): IlvkTernaryStringList;
begin
  Result := TTernaryStringList.Create(MemoryBased, StoreData, InitialSize,
    GrowthFactor)
end;

{ TTernaryStringList }

procedure TTernaryStringList.Add(const Str: PChar; const Data: Integer);
var
  LastNode  : LongWord;

  function AddRest(const Str: PChar): LongWord;
  var
    NodePtr   : PTreeNode;
    NewOffset : LongWord;
  begin
    if Str^ = #0 then
      Result := 0
    else begin
      NodePtr := NewNode(Result);
      LastNode := Result;

      NodePtr^.Ch := Str^;
      NodePtr^.WordStop := Str[1] = #0;
      if NodePtr^.WordStop then
        Inc(FCount);
      NodePtr^.Left := 0;
      NodePtr^.Right := 0;
      NewOffset := AddRest(Str+1);
      NodePtr := PTreeNode(FHeap.Start + Result);
      NodePtr^.Same := NewOffset;
    end;
  end;

  procedure PlaceWord(Str: PChar);
  var
    NodeOffset  : LongWord;
    NewOffset   : LongWord;
    NodePtr     : PTreeNode;
    Ch          : Char;
  begin
    NodeOffset := 0;
    repeat
      LastNode := NodeOffset;
      NodePtr := PTreeNode(FHeap.Start + NodeOffset);

      Ch := Str^;
      if Ch < NodePtr^.Ch then
      begin
        if NodePtr^.Left = 0 then
        begin
          NewOffset := AddRest(Str);
          NodePtr := PTreeNode(FHeap.Start + NodeOffset);
          NodePtr^.Left := NewOffset;
          Exit;
        end else
          NodeOffset := NodePtr^.Left;
      end else if Ch > NodePtr^.Ch then
      begin
        if NodePtr^.Right = 0 then
        begin
          NewOffset := AddRest(Str);
          NodePtr := PTreeNode(FHeap.Start + NodeOffset);
          NodePtr^.Right := NewOffset;
          Exit;
        end else
          NodeOffset := NodePtr^.Right;
      end else begin
        if NodePtr^.Same = 0 then
        begin
          NewOffset := AddRest(Str+1);
          NodePtr := PTreeNode(FHeap.Start + NodeOffset);
          NodePtr^.Same := NewOffset;
          Exit;
        end;

        Inc(Str);
        NodeOffset := NodePtr^.Same;
      end;
    until Str^ = #0;

    if not NodePtr^.WordStop then
    begin
      NodePtr^.WordStop := True;
      Inc(FCount);
    end;
  end;

begin
  Assert(Assigned(Str) and (Str^ <> #0));

  if FCount = 0 then
    AddRest(Str)
  else
    PlaceWord(Str);

  if FStoreData then
    PTreeNode(FHeap.Start + LastNode)^.Data := Data;
end;

procedure TTernaryStringList.AddToStrings(const StringValue: PChar;
  const StringLength, StringData: Integer;
  const VisitData: Pointer);
begin
  TStrings(VisitData).Add(StringValue);
end;

procedure TTernaryStringList.Balance;
var
  Data  : TBalance;

  procedure AddCenter(const LowerIndex, UpperIndex: Integer);
  var
    CenterIndex : Integer;
    StringOfs   : Integer;
    StringPtr   : PChar;
    StringLen   : Integer;
    StringData  : Integer;
  begin
    CenterIndex := (LowerIndex + UpperIndex) div 2;
    Move((Data.Heap.Start + 4 * CenterIndex)^, StringOfs, 4);
    StringPtr := Data.Heap.Start + StringOfs;

    StringLen := StrLen(StringPtr);
    Move((StringPtr + StringLen + 1)^, StringData, 4);

    Add(StringPtr, StringData);
  end;

begin
  Data.Size := 4096; // satisfy block needs and cater for alignment
  Visit(BalanceSumSize, @Data);

  Data.Heap := NewHeap(True, Data.Size);
  try
    Data.Position1 := Data.Heap.Allocate(GetCount * 4);
    Data.Position2 := Data.Heap.Allocate(Data.Size);
    Visit(BalanceAddWord, @Data);

    FHeap := NewHeap(FMemoryBased, FInitialSize, FHeap.GrowthFactor);
    FCount := 0;
    AddCenter(0, GetCount-1);
  finally
    Data.Heap := nil;
  end;
end;

procedure TTernaryStringList.BalanceAddWord(const StringValue: PChar;
  const StringLength, StringData: Integer; const VisitData: Pointer);
var
  Data  : PBalance;
begin
  Data := VisitData;
  Move(Data^.Position2, (Data^.Heap.Start + Data.Position1)^, 4);
  Move(StringValue^, (Data^.Heap.Start + Data.Position2)^, StrLen(StringValue)+1);

  Inc(Data^.Position1, 4);
  Inc(Data^.Position2, StrLen(StringValue)+1);

  if FStoreData then
    Move(StringData, (Data^.Heap.Start + Data.Position2)^, 4);
end;

procedure TTernaryStringList.BalanceSumSize(const StringValue: PChar;
  const StringLength, StringData: Integer; const VisitData: Pointer);
var
  Data  : PBalance;
begin
  Data := VisitData;
  Inc(Data^.Size, StringLength + 1);
  if FStoreData then
    Inc(Data^.Size, 4);
end;

constructor TTernaryStringList.Create(const MemoryBased, StoreData: Boolean;
  const InitialSize: Cardinal; const GrowthFactor: Double);
begin
  inherited Create;

  FMemoryBased := MemoryBased;
  FInitialSize := InitialSize;

  FCount := 0;
  FStoreData := StoreData;
  FHeap := NewHeap(MemoryBased, InitialSize, GrowthFactor);
end;

function TTernaryStringList.Exists(const Str: PChar): Boolean;
var
  Dummy : Integer;
begin
  Result := GetData(Str, Dummy);
end;

procedure TTernaryStringList.Fill(const Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.BeginUpdate;
  try
    Strings.Clear;

    Visit(AddToStrings, Strings);
  finally
    Strings.EndUpdate;
  end;
end;

function TTernaryStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TTernaryStringList.GetData(const Str: PChar;
  out Data: Integer): Boolean;
var
  NodePtr : PTreeNode;
  Offset  : LongWord;
  Start   : PChar;
  TempStr : PChar;
  Ch      : Char;
begin
  Result := False;
  if (not Assigned(Str)) or (Str^ = #0) then
    Exit;

  if FCount = 0 then
    Exit;

  Offset := 0;
  TempStr := Str;
  Start := FHeap.Start;
  NodePtr := nil;
  repeat
    if TempStr^ = #0 then
      Break;

    NodePtr := PTreeNode(Start + Offset);
    Ch := TempStr^;

    if Ch = NodePtr^.Ch then
    begin
      Offset := NodePtr^.Same;
      Inc(TempStr);
    end else if Ch < NodePtr^.Ch then
      Offset := NodePtr^.Left
    else
      Offset := NodePtr^.Right;
  until (TempStr^ = #0) or (Offset=0);

  if (TempStr^ = #0) and Assigned(NodePtr) then
  begin
    if NodePtr^.WordStop then
    begin
      Result := True;
      if FStoreData then
        Data := NodePtr^.Data
      else
        Data := 0;
    end;
  end;
end;

function TTernaryStringList.GetSize: LongWord;
begin
  Result := FHeap.Allocated;
end;

procedure TTernaryStringList.LoadFromFile(const FileName: string);
var
  Stream  : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTernaryStringList.LoadFromStream(const Stream: TStream);
var
  Buffer  : ISafeMem;
  TempStr : string;
  p       : PChar;
  Index   : Integer;
begin
  FCount := 0;
  FHeap.Reset;

  Buffer := AllocateSafeMem(32768);

  TempStr := '';
  repeat
    Buffer.Grab(Stream);

    if Buffer.Size > 0 then
    begin
      p := Buffer.Pointer;
      for Index := 0 to Buffer.Size-1 do
      begin
        case p[Index] of
          #13:
            begin
              Add(PChar(TempStr), 0);
              TempStr := '';
            end;

          #10:
            ;

        else
          TempStr := TempStr + p[Index];
        end;
      end;
    end;
  until Buffer.Size = 0;

  Balance;
end;

function TTernaryStringList.NewNode(out Index: LongWord): PTreeNode;
begin
  if FStoreData then
    Index := FHeap.Allocate(SizeOf(TTreeNode))
  else
    Index := FHeap.Allocate(SizeOf(TTreeNode)-SizeOf(LongWord));
  Result := PTreeNode(FHeap.Start + Index);
end;

procedure TTernaryStringList.SaveStringToStream(const StringValue: PChar;
  const StringLength, StringData: Integer;
  const VisitData: Pointer);
var
  s : string;
begin
  s := string(StringValue) + #13#10;
  TStream(VisitData).WriteBuffer(s[1], Length(s));
end;

procedure TTernaryStringList.SaveToFile(const FileName: string);
var
  Stream  : TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTernaryStringList.SaveToStream(const Stream: TStream);
begin
  Assert(Assigned(Stream));
  Visit(SaveStringToStream, Stream);
end;

procedure TTernaryStringList.Visit(
  const StringVisitMethod: TStringVisitMethod; const Data: Pointer);
var
  CurrentWord : array[0..MAX_WORD_LENGTH] of Char;

  procedure VisitWord(const WordOffset: LongWord;
    const CurrentIndex: Integer);
  var
    NodePtr : PTreeNode;
  begin
    NodePtr := PTreeNode(FHeap.Start + WordOffset);

    if NodePtr^.Left > 0 then
      VisitWord(NodePtr^.Left, CurrentIndex);

    if NodePtr^.WordStop then
    begin
      CurrentWord[CurrentIndex] := NodePtr^.Ch;
      CurrentWord[CurrentIndex+1] := #0;
      if FStoreData then
        StringVisitMethod(CurrentWord, CurrentIndex+1, NodePtr^.Data, Data)
      else
        StringVisitMethod(CurrentWord, CurrentIndex+1, 0, Data);
    end;

    if NodePtr^.Same > 0 then
    begin
      CurrentWord[CurrentIndex] := NodePtr^.Ch;
      VisitWord(NodePtr^.Same, CurrentIndex+1);
    end;

    if NodePtr^.Right > 0 then
      VisitWord(NodePtr^.Right, CurrentIndex);
  end;

begin
  VisitWord(0, 0);
end;

end.
