{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code to dissect a .MAP file and returning descriptive
    location information for a code address.
}
unit lvkMapFiles;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMapFiles.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  TlHelp32,
  Windows, SysUtils, Classes;

type
  PSegment = ^TSegment;
  TSegment = record
    Start   : LongWord;
    Length  : LongWord;
    Name    : string;
    Next    : PSegment;
  end;

  PPublic = ^TPublic;
  TPublic = record
    Start : LongWord;
    Name  : string;
    Next  : PPublic;
  end;

  PLine = ^TLine;
  TLine = record
    LineNo  : LongWord;
    Start   : LongWord;
    Next    : PLine;
  end;

  PFile = ^TFile;
  TFile = record
    Filename  : string;
    Lines     : PLine;
    Next      : PFile;
  end;

  TlvkMapFile = class
  private
    FModuleName   : string;
    FImageBase    : PChar;
    FImageSize    : Cardinal;

    FSegments     : PSegment;
    FPublics      : PPublic;
    FFiles        : PFile;

    procedure LoadMapFile;

  public
    constructor Create(const ModuleEntry: TModuleEntry32);
    destructor Destroy; override;

    property ModuleName: string read FModuleName;

    function CoversAddress(const p: Pointer): Boolean;
    function ModuleOfAddress(const Address: Pointer): string;
    function FilenameOfAddress(const Address: Pointer): string;
    function LineNumberOfAddress(const Address: Pointer): Integer;
    function ProcedureOfAddress(const Address: Pointer): string;
  end;

  TlvkMapFileCollection = class
  private
    FMapFiles : array of TlvkMapFile;

  protected
    function MapFileOfAddress(const Address: Pointer): TlvkMapFile;

  public
    constructor Create;
    destructor Destroy; override;

    function ModuleOfAddress(const Address: Pointer): string;
    function FilenameOfAddress(const Address: Pointer): string;
    function LineNumberOfAddress(const Address: Pointer): Integer;
    function ProcedureOfAddress(const Address: Pointer): string;
  end;

function __MODULE__(const Address: Pointer): string; overload;
function __FILENAME__(const Address: Pointer): string; overload;
function __PROCEDURE__(const Address: Pointer): string; overload;
function __LINE_NUMBER__(const Address: Pointer): Integer; overload;
function __DESCRIPTIVE_ADDRESS__(const Address: Pointer; const Short: Boolean=True): string; overload;

function CallStack(const Level: Integer): Pointer;

function __MODULE__(const Level: Integer=0): string; overload;
function __FILENAME__(const Level: Integer=0): string; overload;
function __PROCEDURE__(const Level: Integer=0): string; overload;
function __LINE_NUMBER__(const Level: Integer=0): Integer; overload;
function __DESCRIPTIVE_ADDRESS__(const Level: Integer=0; const Short: Boolean=True): string; overload;

implementation

var
  MapFiles  : TlvkMapFileCollection  = nil;

procedure LoadMapFiles;
begin
  if not Assigned(MapFiles) then
    MapFiles := TlvkMapFileCollection.Create;
end;

function __MODULE__(const Address: Pointer): string;
begin
  LoadMapFiles;
  Result := MapFiles.ModuleOfAddress(Address);
end;

function __FILENAME__(const Address: Pointer): string;
begin
  LoadMapFiles;
  Result := MapFiles.FilenameOfAddress(Address);
end;

function __PROCEDURE__(const Address: Pointer): string;
begin
  LoadMapFiles;
  Result := MapFiles.ProcedureOfAddress(Address);
end;

function __LINE_NUMBER__(const Address: Pointer): Integer;
begin
  LoadMapFiles;
  Result := MapFiles.LineNumberOfAddress(Address);
end;

function __DESCRIPTIVE_ADDRESS__(const Address: Pointer; const Short: Boolean): string;
var
  Module, ProcedureName, Filename : string;
  LineNumber                      : Integer;
begin
  Module := __MODULE__(Address);
  ProcedureName := __PROCEDURE__(Address);
  Filename := __FILENAME__(Address);
  LineNumber := __LINE_NUMBER__(Address);

  Result := '';

  if Short then
  begin
    Result := Filename + '#' + IntToStr(LineNumber);
  end else begin
    if ProcedureName <> '' then
      Result := 'procedure ' + ProcedureName;

    if Module <> '' then
    begin
      if Result <> '' then
        Result := Result + ' in ';

      Result := Result + 'module ' + Module;
    end;

    if FileName <> '' then
    begin
      if Result <> '' then
        Result := Result + ' in ';

      Result := Result + 'file ' + Filename;
    end;

    if LineNumber > 0 then
    begin
      if Result <> '' then
        Result := Result + ' at ';

      Result := Result + 'line ' + IntToStr(LineNumber);
    end;
  end;
end;

function CallStack(const Level: Integer): Pointer;
var
  Dummy : Integer;  // Dummy variable to force a stack frame
asm
  push  ecx
  inc   Dummy       // Reference the dummy variable so it won't be opitimized away

  mov   ecx, Level
  mov   eax, ebp

  jcxz  @@exit
@@loop:
  mov   eax, [eax]
  dec   ecx
  jnz   @@loop

@@exit:
  mov   eax, [eax+4]
  pop   ecx
end;

function __MODULE__(const Level: Integer): string;
begin
  Result := __MODULE__(CallStack(Level+1));
end;

function __FILENAME__(const Level: Integer): string;
begin
  Result := __FILENAME__(CallStack(Level+1));
end;

function __PROCEDURE__(const Level: Integer): string;
begin
  Result := __PROCEDURE__(CallStack(Level+1));
end;

function __LINE_NUMBER__(const Level: Integer): Integer;
begin
  Result := __LINE_NUMBER__(CallStack(Level+1));
end;

function __DESCRIPTIVE_ADDRESS__(const Level: Integer; const Short: Boolean): string;
begin
  Result := __DESCRIPTIVE_ADDRESS__(CallStack(Level+1), Short);
end;

{ TlvkMapFile }

function TlvkMapFile.CoversAddress(const p: Pointer): Boolean;
begin
  Result := (LongWord(p) >= LongWord(FImageBase)) and (LongWord(p) < LongWord(FImageBase)+FImageSize);
end;

constructor TlvkMapFile.Create(const ModuleEntry: TModuleEntry32);
begin
  inherited Create;

  FModuleName := ModuleEntry.szExePath;
  Assert(FileExists(ChangeFileExt(FModuleName, '.map')));

  FImageBase := PChar(ModuleEntry.modBaseAddr);
  FImageSize := ModuleEntry.modBaseSize;

  LoadMapFile;
end;

destructor TlvkMapFile.Destroy;
var
  SegmentPtr  : PSegment;
  PublicPtr   : PPublic;
  FilePtr     : PFile;
begin
  while Assigned(FSegments) do
  begin
    SegmentPtr := FSegments;
    FSegments := FSegments^.Next;

    Dispose(SegmentPtr);
  end;

  while Assigned(FPublics) do
  begin
    PublicPtr := FPublics;
    FPublics := FPublics^.Next;

    Dispose(PublicPtr);
  end;

  while Assigned(FFiles) do
  begin
    FilePtr := FFiles;
    FFiles := FFiles^.Next;

    Dispose(FilePtr);
  end;

  inherited;
end;

function TlvkMapFile.FilenameOfAddress(const Address: Pointer): string;
var
  Node    : PFile;
  Offset  : LongWord;
begin
  Offset := LongWord(Address) - $1000 - LongWord(FImageBase);

  Node := FFiles;
  Result := '';
  while Assigned(Node) do
  begin
    if Assigned(Node^.Lines) then
      if Offset >= Node^.Lines^.Start then
        Result := Node^.Filename
      else
        Break;

    Node := Node^.Next;
  end;
end;

function TlvkMapFile.LineNumberOfAddress(const Address: Pointer): Integer;
var
  Offset          : LongWord;
  FileNode        : PFile;
  LineNode        : PLine;
  HighestFileNode : PFile;
begin
  Offset := LongWord(Address) - $1000 - LongWord(FImageBase);

  Result := 0;

  FileNode := FFiles;
  HighestFileNode := nil;
  while Assigned(FileNode) do
  begin
    if Assigned(FileNode^.Lines) then
      if Offset >= FileNode^.Lines^.Start then
        HighestFileNode := FileNode
      else
        Break;

    FileNode := FileNode^.Next;
  end;

  if Assigned(HighestFileNode) then
  begin
    LineNode := HighestFileNode^.Lines;
    while Assigned(LineNode) do
    begin
      if Offset >= LineNode^.Start then
        Result := LineNode^.LineNo
      else
        Break;

      LineNode := LineNode^.Next;
    end;
  end;
end;

procedure TlvkMapFile.LoadMapFile;
type
  TSection = (sNone, sSections, sSegments, sPublicsByName, sPublicsByAddress, sFile);
var
  Stream        : TMemoryStream;
  Line          : string;
  Pos           : Integer;
  Index         : Integer;
  p             : PChar;

  Section       : TSection;
  LastFile      : PFile;
  LastLine      : PLine;
  LastSegment   : PSegment;
  LastPublic    : PPublic;

  procedure AddFile(const Line: string);
  var
    Name  : string;
  begin
    if not Assigned(FFiles) then
    begin
      New(FFiles);
      LastFile := FFiles;
    end else begin
      New(LastFile^.Next);
      LastFile := LastFile^.Next;
    end;

    Name := Copy(Line, System.Pos('(', Line)+1, Length(Line));
    Name := Copy(Name, 1, System.Pos(')', Name)-1);
    LastFile^.Filename := Name;
    LastFile^.Lines := nil;
    LastFile^.Next := nil;
  end;

  procedure AddLine(const Line: string);
  var
    x : Integer;
  begin
    Assert(Assigned(LastFile));

    x := 2;
    while Copy(Line, x+6, 5) = '0001:' do
    begin
      if not Assigned(LastFile^.Lines) then
      begin
        New(LastFile^.Lines);
        LastLine := LastFile^.Lines;
      end else begin
        New(LastLine^.Next);
        LastLine := LastLine^.Next;
      end;

      LastLine^.LineNo := StrToInt(Copy(Line, x, 5));
      LastLine^.Start := StrToInt('$' + Copy(Line, x+11, 8));
      LastLine^.Next := nil;

      Inc(x, 20);
    end;
  end;

  procedure ProcessLine;
  var
    Name  : string;
  begin
    case Section of
      sNone:
        begin
          if Copy(Line, 1, 54) = ' Start         Length     Name                   Class' then
            Section := sSections;
        end;

      sSections:
        begin
          if Copy(Line, 1, 24) = 'Detailed map of segments' then
            Section := sSegments;
        end;

      sSegments:
        begin
          if Copy(Line, 1, 33) = '  Address         Publics by Name' then
            Section := sPublicsByName
          else if Copy(Line, 36, 7) = 'S=.text' then
          begin
            if not Assigned(FSegments) then
            begin
              New(FSegments);
              LastSegment := FSegments;
            end else begin
              New(LastSegment^.Next);
              LastSegment := LastSegment^.Next;
            end;

            LastSegment^.Start := StrToInt('$' + Copy(Line, 7, 8));
            LastSegment^.Length := StrToInt('$' + Copy(Line, 16, 8));
            Name := Copy(Line, 60, Length(Line));
            Name := Copy(Name, 1, System.Pos(' ', Name)-1);
            LastSegment^.Name := Name;
          end
        end;

      sPublicsByName:
        begin
          if Copy(Line, 1, 34) = '  Address         Publics by Value' then
            Section := sPublicsByAddress;
        end;

      sPublicsByAddress:
        begin
          if Copy(Line, 1, 17) = 'Line numbers for ' then
          begin
            Section := sFile;
            AddFile(Line);
          end else if Copy(Line, 2, 5) = '0001:' then
          begin
            if not Assigned(FPublics) then
            begin
              New(FPublics);
              LastPublic := FPublics;
            end else begin
              New(LastPublic^.Next);
              LastPublic := LastPublic^.Next;
            end;

            LastPublic^.Start := StrToInt('$' + Copy(Line, 7, 8));
            LastPublic^.Name := Trim(Copy(Line, 22, Length(Line)));
          end;
        end;

      sFile:
        begin
          if Copy(Line, 1, 17) = 'Line numbers for ' then
          begin
            Section := sFile;
            AddFile(Line);
          end else if Copy(Line, 8, 5) = '0001:' then
            AddLine(Line);
        end;
    end;
  end;

begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(ChangeFileExt(FModuleName, '.map'));
    p := Stream.Memory;

    SetLength(Line, 255);
    FillChar(Line[1], 255, #32);

    Pos := 1;
    Section := sNone;
    for Index := 0 to Stream.Size-1 do
    begin
      case p^ of
        #13, #10:
          begin
            if Pos > 1 then
            begin
              ProcessLine;
              Pos := 1;
              FillChar(Line[1], 255, #32);
            end;
          end;
      else
        begin
          Line[Pos] := p^;
          Inc(Pos);

          if Pos = 256 then
            raise Exception.Create('Line too long');
        end;
      end;

      Inc(p);
    end;
  finally
    Stream.Free;
  end;
end;

function TlvkMapFile.ModuleOfAddress(const Address: Pointer): string;
var
  Offset  : LongWord;
  Segment : PSegment;
begin
  Offset := LongWord(Address) - $1000 - LongWord(FImageBase);

  Segment := FSegments;
  Result := '';
  while Assigned(Segment) do
  begin
    if (Offset >= Segment^.Start) and (Offset < Segment^.Start + Segment^.Length) then
    begin
      Result := Segment^.Name;
      Break;
    end;

    Segment := Segment^.Next;
  end;
end;

function TlvkMapFile.ProcedureOfAddress(const Address: Pointer): string;
var
  Offset    : LongWord;
  PublicPtr : PPublic;
begin
  Offset := LongWord(Address) - $1000 - LongWord(FImageBase);

  PublicPtr := FPublics;
  Result := '';
  while Assigned(PublicPtr) do
  begin
    if (Offset >= PublicPtr^.Start) then
      Result := PublicPtr^.Name
    else
      Break;

    PublicPtr := PublicPtr^.Next;
  end;
end;

{ TlvkMapFileCollection }

constructor TlvkMapFileCollection.Create;
var
  hSnapshot   : THandle;
  ModuleEntry : TModuleEntry32;
begin
  inherited Create;

  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPALL, GetCurrentProcessId);
  if hSnapshot = 0 then
    RaiseLastWin32Error;
  try
    SetLength(FMapFiles, 0);

    FillChar(ModuleEntry, SizeOf(ModuleEntry), #0);
    ModuleEntry.dwSize := SizeOf(ModuleEntry);
    
    if Module32First(hSnapshot, ModuleEntry) then
      repeat
        if FileExists(ChangeFileExt(ModuleEntry.szExePath, '.map')) then
        begin
          SetLength(FMapFiles, Length(FMapFiles)+1);
          FMapFiles[High(FMapFiles)] := TlvkMapFile.Create(ModuleEntry);
        end;
      until not Module32Next(hSnapshot, ModuleEntry)
    else
      RaiseLastWin32Error;
  finally
    CloseHandle(hSnapshot);
  end;
end;

destructor TlvkMapFileCollection.Destroy;
var
  Index : Integer;
begin
  for Index := Low(FMapFiles) to High(FMapFiles) do
    FMapFiles[Index].Free;

  inherited;
end;

function TlvkMapFileCollection.FilenameOfAddress(
  const Address: Pointer): string;
var
  MapFile : TlvkMapFile;
begin
  MapFile := MapFileOfAddress(Address);
  if Assigned(MapFile) then
    Result := MapFile.FilenameOfAddress(Address)
  else
    Result := '';
end;

function TlvkMapFileCollection.LineNumberOfAddress(
  const Address: Pointer): Integer;
var
  MapFile : TlvkMapFile;
begin
  MapFile := MapFileOfAddress(Address);
  if Assigned(MapFile) then
    Result := MapFile.LineNumberOfAddress(Address)
  else
    Result := 0;
end;

function TlvkMapFileCollection.MapFileOfAddress(
  const Address: Pointer): TlvkMapFile;
var
  Index : Integer;
begin
  Result := nil;

  for Index := Low(FMapFiles) to High(FMapFiles) do
    if FMapFiles[Index].CoversAddress(Address) then
    begin
      Result := FMapFiles[Index];
      Break;
    end;
end;

function TlvkMapFileCollection.ModuleOfAddress(
  const Address: Pointer): string;
var
  MapFile : TlvkMapFile;
begin
  MapFile := MapFileOfAddress(Address);
  if Assigned(MapFile) then
    Result := MapFile.ModuleOfAddress(Address)
  else
    Result := '';
end;

function TlvkMapFileCollection.ProcedureOfAddress(
  const Address: Pointer): string;
var
  MapFile : TlvkMapFile;
begin
  MapFile := MapFileOfAddress(Address);
  if Assigned(MapFile) then
    Result := MapFile.ProcedureOfAddress(Address)
  else
    Result := '';
end;

end.
