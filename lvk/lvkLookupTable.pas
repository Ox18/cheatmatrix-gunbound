{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a class for a fast string->integer lookup table.
}
unit lvkLookupTable;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkLookupTable.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkSubAllocator;

type
  TTranslationTable = array[Char] of Char;

  { Description:
      This interface wraps a fast string->integer lookup table. It uses a bit
      more memory than a normal string array, but it's faster.
  }
  IlvkLookupTable = interface
    ['{0F4B0F8F-CC53-43F5-93D7-AD8144154E6A}']

    { Description:
        Adds the given string to the table, and sets the lookup value for it.
        If the string already exists, the value of that string will be changed.
      Parameters:
        s - The string to add.
        Value - The integer value to store for the string.
      See also:
        Lookup, Clear
    }
    procedure Add(const s: string; const Value: Integer);

    { Description:
        This method looks up the given string. If the string can be found,
        the method returns True and the Value output parameter will be set to
        the value of the string in the table. If the string cannot be found,
        the method returns False and the Value output parameter is undefined.
      Parameters:
        s - The string to lookup.
        Value - Output parameter that will be set to the value of the string
          in the table, if the string is found.
      Returns:
        True if the string was found, False if not.
      See also:
        Add, Clear
    }
    function Lookup(const s: string; out Value: Integer): Boolean;

    { Description:
        Removes all the strings from the table.
      See also:
        Add, Lookup
    }
    procedure Clear;
  end;

  { Description:
      Parameters to the constructor of the TlvkLookupTable class to dictate
      how it will handle different case sensitivity. The following values
      are available:
  }
  TCaseSensitive = (
    // The table will be case sensitive, all characters will be used as they
    // are provided.
    csYes,
    // English lower-case alphabet characters will be considered equal to
    // upper-case versions of the same characters (a-z only).
    csEnglish,
    // Locale-specific lower-case alphabet characters will be considered equal
    // to upper-case versions of the same characters.
    csLocale);

{ Description:
    This function returns a new, empty, lookuptable instance that uses normal
    english characters. You can specify wether the table should be case
    sensitive or not.
  Parameters:
    CaseSensitive - The case sensitivity of the table (see TCaseSensitive for
      more information.
  See also:
    NewLookupTable@TTranslationTable
}
function NewLookupTable(const CaseSensitive: TCaseSensitive=csYes): IlvkLookupTable; overload;

{ Description:
    This function returns a new, empty, lookuptable instance that uses the
    provided translation table. You can then implement or use whatever kind
    of case sensitivity scheme you want to.
  Parameters:
    TranslationTable  - The table to use. Each character in the table should
      hold a character that it will be translated to. For instance, if you
      consider A and Å to be the same two characters, you would want to
      map the Å character to A, so Table['Å'] := 'A';. All characters that you
      don't want to map should hold a copy of the same character, ie.
      Table['A'] := 'A';
  See also:
    NewLookupTable@TCaseSensitive
}
function NewLookupTable(const TranslationTable: TTranslationTable): IlvkLookupTable; overload;

implementation

uses
  Windows;

type
  PLookupNode = ^TLookupNode;
  TNodeArray = array[Char] of PLookupNode;
  TLookupNode = record
    Word  : Boolean;
    Value : Integer;
    Next  : TNodeArray;
  end;

  { Description:
      This is the lookup table class. You create an instance of this class
      and add and lookup strings in it.
  }
  TlvkLookupTable = class(TInterfacedObject, IlvkLookupTable)
  private
    FAllocator  : ISubAllocator;
    FRoot       : TLookupNode;
    FTable      : TTranslationTable;

    function AllocateNode: PLookupNode;

  public
    { Description:
        Creates an instance of the lookup table class with a very specific
        translation table to use. Look in the Create@TCaseSensitive constructor
        on how to configure such tables.
      Parameters:
        TranslationTable - The translation table to use for the lookup table.
      See also:
        Create@TCaseSensitive, Create
    }
    constructor Create(const TranslationTable: TTranslationTable); overload;

    { Description:
        Creates an instance of the lookup table class and configures it with
        a specific case sensitivity setting. This setting cannot be changed
        once the table has been created. See the TCasSensitive type for more
        information about the possibilities.
      Parameters:
        CaseSensitive - How to handle case sensitivity issues.
      See also:
        Create@TTranslationTable, Create
    }
    constructor Create(const CaseSensitive: TCaseSensitive); overload;

    { Description:
        Clears and destroys the table.
    }
    destructor Destroy; override;

    // <ALIAS IlvkLookupTable.Add@string@Integer>
    procedure Add(const s: string; const Value: Integer);
    // <ALIAS IlvkLookupTable.Lookup@string@Integer>
    function Lookup(const s: string; out Value: Integer): Boolean;
    // <ALIAS IlvkLookupTable.Clear>
    procedure Clear;
  end;

const
  BLOCK_SIZE  = (32768 div SizeOf(TLookupNode)) * SizeOf(TLookupNode);

function NewLookupTable(const CaseSensitive: TCaseSensitive): IlvkLookupTable;
begin
  Result := TlvkLookupTable.Create(CaseSensitive);
end;

function NewLookupTable(const TranslationTable: TTranslationTable): IlvkLookupTable;
begin
  Result := TlvkLookupTable.Create(TranslationTable);
end;

{ TlvkLookupTable }

procedure TlvkLookupTable.Add(const s: string; const Value: Integer);
var
  p     : PChar;
  c     : Char;
  Node  : PLookupNode;
begin
  Assert(s <> '');

  p := PChar(s);
  Node := @FRoot;

  while p^ <> #0 do
  begin
    c := FTable[p^];
    Inc(p);

    if not Assigned(Node^.Next[c]) then
      Node^.Next[c] := AllocateNode;

    Assert(Cardinal(Node^.Next[c]) > 255); 
    Node := Node^.Next[c];
  end;

  Assert(Assigned(Node));

  Node^.Word := True;
  Node^.Value := Value;
end;

constructor TlvkLookupTable.Create(
  const TranslationTable: TTranslationTable);
begin
  inherited Create;

  FTable := TranslationTable;
  FAllocator := NewSubAllocator(BLOCK_SIZE, al8);
end;

constructor TlvkLookupTable.Create(const CaseSensitive: TCaseSensitive);
var
  Table : TTranslationTable;
  c     : Char;
begin
  case CaseSensitive of
    csYes:
      begin
        for c := #0 to #255 do
          Table[c] := c;
      end;

    csEnglish:
      begin
        for c := #0 to #255 do
          Table[c] := c;
        for c := 'a' to 'z' do
          Table[c] := UpCase(c);
      end;

    csLocale:
      begin
        for c := #0 to #255 do
          Table[c] := c;
        for c := #0 to #255 do
          Table[c] := AnsiUpperCase(c)[1];
      end;
  end;

  Create(Table);
end;

procedure TlvkLookupTable.Clear;
begin
  FAllocator.ReleaseAll;
  ZeroMemory(@FRoot, SizeOf(FRoot));
end;

destructor TlvkLookupTable.Destroy;
begin
  Clear;

  inherited;
end;

function TlvkLookupTable.Lookup(const s: string;
  out Value: Integer): Boolean;
var
  p     : PChar;
  c     : Char;
  Node  : PLookupNode;
begin
  Assert(s <> '');
  Result := False;

  p := PChar(s);
  Node := @FRoot;
  while p^ <> #0 do
  begin
    c := FTable[p^];
    Inc(p);

    if not Assigned(Node^.Next[c]) then
      Break;

    Node := Node^.Next[c];
  end;

  if Assigned(Node) then
  begin
    Result := Node^.Word;
    Value := Node^.Value;
  end;
end;

function TlvkLookupTable.AllocateNode: PLookupNode;
begin
  Result := FAllocator.Grab(SizeOf(TLookupNode));
  ZeroMemory(Result, SizeOf(Result^));
end;

end.
