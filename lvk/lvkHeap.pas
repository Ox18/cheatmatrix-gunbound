{ TODO 2 -oLVK -cSource : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the heap object code.
}
unit lvkHeap;

// $Author: Lasse V. Karlsen $
// $Revision: 3 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkHeap.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes;

type
  IlvkHeap = interface
    ['{459E08FC-89AB-43CE-A390-E591AD5BDCA3}']

    function Allocate(const Amount: LongWord;
      const ZeroBlock: Boolean=False): LongWord;

    function GetStart: PChar;
    property Start: PChar read GetStart;

    function GetGrowthFactor: Double;
    procedure SetGrowthFactor(const Value: Double);
    property GrowthFactor: Double read GetGrowthFactor write SetGrowthFactor;

    function GetSize: LongWord;
    procedure SetSize(const Value: LongWord);
    property Size: LongWord read GetSize write SetSize;

    function GetAllocated: LongWord;
    property Allocated: LongWord read GetAllocated;

    procedure Reset;
  end;

  EHeap = class(Exception);

function NewHeap(const MemoryBased: Boolean; const InitialSize: Cardinal=65536;
  const GrowthFactor: Double=2): IlvkHeap; overload;
function NewHeap(const FileName: string; const InitialSize: Cardinal=65536;
  const GrowthFactor: Double=2): IlvkHeap; overload;

resourcestring
  ERR_TOO_SMALL_SIZE    = 'The new size of the heap must at least be the size of allocated space';
  ERR_NO_TEMP_DIRECTORY = 'Unable to determine temporary directory';
  ERR_NO_TEMP_FILENAME  = 'Unable to generate temporary filename';
  ERR_NO_TEMP_FILE      = 'Unable to create temporary file';
  ERR_NO_MAPPING        = 'Unable to create mapping of temporary file';
  ERR_NO_VIEW           = 'Unable to map view of temporary file';

implementation

uses
  Windows;

const
  FSCTL_SET_SPARSE  = $900C4;

type
  TBaseHeap = class(TInterfacedObject, IlvkHeap)
  private
    FAllocated    : LongWord;
    FGrowthFactor : Double;
    FStart        : PChar;
    FSize         : LongWord;

  protected
    procedure GrowHeap;
    procedure SetStart(const NewStart: PChar);
    procedure AdjustHeap; virtual; abstract;

    // IlvkHeap interface
    function Allocate(const Amount: LongWord;
      const ZeroBlock: Boolean): LongWord; virtual;
    function GetAllocated: LongWord; virtual;
    function GetGrowthFactor: Double; virtual;
    function GetSize: LongWord; virtual;
    function GetStart: PChar; virtual;
    procedure Reset; virtual;
    procedure SetGrowthFactor(const Value: Double); virtual;
    procedure SetSize(const Value: LongWord); virtual;

  public
    constructor Create(const InitialSize: Cardinal; const GrowthFactor: Double);
  end;

  TMemoryHeap = class(TBaseHeap)
  private
    FStream             : TMemoryStream;

  protected
    procedure AdjustHeap; override;
    
  public
    constructor Create(const InitialSize: Cardinal;
      const GrowthFactor: Double); overload;
    destructor Destroy; override;
  end;

  TDiskHeap = class(TBaseHeap)
  private
    FFileHandle         : THandle;
    FFileMappingHandle  : THandle;
    FMemoryMap          : PChar;

    procedure MapFile;
    procedure UnMapFile;

  protected
    procedure AdjustHeap; override;

  public
    constructor Create(const FileName: string; const InitialSize: Cardinal;
      const GrowthFactor: Double); overload;
    constructor Create(const InitialSize: Cardinal;
      const GrowthFactor: Double); overload;
    destructor Destroy; override;
  end;

function NewHeap(const MemoryBased: Boolean; const InitialSize: Cardinal;
  const GrowthFactor: Double): IlvkHeap;
begin
  if MemoryBased then
    Result := TMemoryHeap.Create(InitialSize, GrowthFactor)
  else
    Result := TDiskHeap.Create(InitialSize, GrowthFactor);
end;

function NewHeap(const FileName: string; const InitialSize: Cardinal;
  const GrowthFactor: Double): IlvkHeap;
begin
  Result := TDiskHeap.Create(FileName, InitialSize, GrowthFactor);
end;

{ TDiskHeap }

constructor TDiskHeap.Create(const FileName: string; const InitialSize: Cardinal;
  const GrowthFactor: Double);
var
  Dummy : Cardinal;
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  FFileMappingHandle := INVALID_HANDLE_VALUE;
  FMemoryMap := nil;

  inherited Create(InitialSize, GrowthFactor);

  FFileHandle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_TEMPORARY or
    FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_DELETE_ON_CLOSE, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EHeap.Create(ERR_NO_TEMP_FILE);

  DeviceIoControl(FFileHandle, FSCTL_SET_SPARSE, nil, 0, nil, 0, Dummy, nil);

  FSize := InitialSize;
  FGrowthFactor := GrowthFactor;
  FAllocated := 0;

  MapFile;
end;

procedure TDiskHeap.AdjustHeap;
begin
  UnmapFile;
  MapFile;
end;

constructor TDiskHeap.Create(const InitialSize: Cardinal;
  const GrowthFactor: Double);
var
  TempDirectory : array[0..MAX_PATH] of Char;
  TempFileName  : array[0..MAX_PATH] of Char;
  rc            : Integer;
begin
  rc := GetTempPath(SizeOf(TempDirectory), TempDirectory);
  if rc = 0 then
    raise EHeap.Create(ERR_NO_TEMP_DIRECTORY);

  rc := GetTempFileName(TempDirectory, nil, 0, TempFileName);
  if rc = 0 then
    raise EHeap.Create(ERR_NO_TEMP_FILENAME);

  Create(TempFileName, InitialSize, GrowthFactor);
end;

destructor TDiskHeap.Destroy;
begin
  UnMapFile;

  if FFileHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;

  inherited;
end;

procedure TDiskHeap.MapFile;
begin
  FSize := (FSize div 4096 + LongWord(Ord(FSize mod 4096 > 0)))*4096;

  FFileMappingHandle := CreateFileMapping(FFileHandle, nil, PAGE_READWRITE, 0,
    FSize, nil);
  if FFileMappingHandle = 0 then
    raise EHeap.Create(ERR_NO_MAPPING);

  FMemoryMap := MapViewOfFile(FFileMappingHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if FMemoryMap = nil then
    raise EHeap.Create(ERR_NO_VIEW);

  SetStart(FMemoryMap);
end;

procedure TDiskHeap.UnMapFile;
begin
  if Assigned(FStart) then
  begin
    UnmapViewOfFile(FStart);
    FStart := nil;
  end;

  if FFileMappingHandle <> 0 then
  begin
    CloseHandle(FFileMappingHandle);
    FFileMappingHandle := 0;
  end;
end;

{ TBaseHeap }

function TBaseHeap.Allocate(const Amount: Cardinal;
  const ZeroBlock: Boolean): Cardinal;
begin
  Result := FAllocated;

  if FAllocated + Amount > GetSize then
    GrowHeap;

  if ZeroBlock then
    ZeroMemory(FStart + FAllocated, Amount);
  Inc(FAllocated, Amount);
end;

constructor TBaseHeap.Create(const InitialSize: Cardinal;
  const GrowthFactor: Double);
begin
  inherited Create;

  Assert(GrowthFactor > 1.0, 'Must have a growth factor above 1.0');
  Assert(InitialSize >= 4096, 'Must have an initial size of at least 4KB');

  FGrowthFactor := GrowthFactor;
  FSize := InitialSize;
end;

function TBaseHeap.GetAllocated: Cardinal;
begin
  Result := FAllocated;
end;

function TBaseHeap.GetGrowthFactor: Double;
begin
  Result := FGrowthFactor;
end;

function TBaseHeap.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TBaseHeap.GetStart: PChar;
begin
  Result := FStart;
end;

procedure TBaseHeap.GrowHeap;
begin
  SetSize(Trunc(GetSize * FGrowthFactor));
end;

procedure TBaseHeap.Reset;
begin
  FAllocated := 0;
  FSize := 4096;

  AdjustHeap;
end;

procedure TBaseHeap.SetGrowthFactor(const Value: Double);
begin
  Assert(Value > 1.0, 'Must have a growth factor above 1.0');

  FGrowthFactor := Value;
end;

procedure TBaseHeap.SetSize(const Value: LongWord);
begin
  Assert(Value >= 4096, 'Must have a size of at least 4KB');

  if Value <> FSize then
  begin
    if Value < FAllocated then
      raise EHeap.Create(ERR_TOO_SMALL_SIZE);

    FSize := Value;
    AdjustHeap;
  end;
end;

procedure TBaseHeap.SetStart(const NewStart: PChar);
begin
  FStart := NewStart;
end;

{ TMemoryHeap }

procedure TMemoryHeap.AdjustHeap;
begin
  FStream.Size := GetSize;
  Setstart(FStream.Memory);
end;

constructor TMemoryHeap.Create(const InitialSize: Cardinal;
  const GrowthFactor: Double);
begin
  inherited Create(InitialSize, GrowthFactor);

  FStream := TMemoryStream.Create;
  FStream.Size := GetSize;

  SetStart(FStream.Memory);
end;

destructor TMemoryHeap.Destroy;
begin
  FStream.Free;

  inherited;
end;

end.
