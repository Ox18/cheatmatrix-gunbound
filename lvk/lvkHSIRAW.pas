{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a class for accessing HSI RAW image files.
}
unit lvkHSIRAW;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkHSIRAW.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Graphics, SysUtils, Classes, lvkLargeImage;

const
  iWidth        = 0;
  iHeight       = 1;
  iPaletteSize  = 2;
  iHorizDPI     = 3;
  iVertDPI      = 4;
  iGamma        = 5;
  iCompression  = 6;
  iAlphaChannel = 7;

type
  TMagicNumber  = packed array[0..5] of Byte;

  PHeader = ^THeader;
  THeader = packed record
    MagicNumber   : TMagicNumber;
    Version       : SmallInt;
    Width         : SmallInt;
    Height        : SmallInt;
    PaletteSize   : SmallInt;
    HorizDPI      : SmallInt;
    VertDPI       : SmallInt;
    Gamma         : SmallInt;
    Compression   : SmallInt;
    AlphaChannel  : SmallInt;
    Reserved1     : packed array[0..7] of Byte;
  end;

  TlvkHSIRAW = class(TlvkLargeImage)
  private
    FHeader : PHeader;

    procedure CreateFile(const Filename: string; const Width, Height: LongWord; const Paletted: Boolean);

    function GetInteger(const Index: Integer): SmallInt;
    procedure SetInteger(const Index: Integer; const Value: SmallInt);

  protected
    procedure ReadHeader(out Width, Height, LineSize, PixelSize, ImageStart: Integer); override;
    
  public
    constructor Create(const Filename: string; const Width, Height: LongWord; const Paletted: Boolean=False); overload;

    procedure Extract(const x1, y1, x2, y2: SmallInt; const DestinationBitmap: TBitmap); overload; override;

    property PaletteSize: SmallInt index iPaletteSize read GetInteger;
    property HorizontalDPI: SmallInt index iHorizDPI read GetInteger write SetInteger;
    property VerticalDPI: SmallInt index iVertDPI read GetInteger write SetInteger;
    property Gamma: SmallInt index iGamma read GetInteger write SetInteger;
  end;

  EHSIRAW = class(Exception);

implementation

const
  CMagicNumber  : TMagicNumber  = ($6d, $68, $77, $61, $6e, $68);

resourcestring
  SNotAccepted  = 'TlvkHSIRAW only accepts uncompressed version 4 HSI RAW image files without an alpha channel';

{ TlvkHSIRAW }

constructor TlvkHSIRAW.Create(const Filename: string; const Width,
  Height: LongWord; const Paletted: Boolean);
begin
  CreateFile(Filename, Width, Height, Paletted);
  Create(Filename);
end;

procedure TlvkHSIRAW.CreateFile(const Filename: string; const Width,
  Height: LongWord; const Paletted: Boolean);
begin
  { TODO 2 -oLVK -cSource : Implement CreateFile }
end;

procedure TlvkHSIRAW.Extract(const x1, y1, x2, y2: SmallInt;
  const DestinationBitmap: TBitmap);
var
  y         : Integer;
  CopyWidth : Integer;
  LeftEdge  : Integer;
  SourcePtr : PChar;
  DestPtr   : PChar;
begin
  Assert(x2 >= x1);
  Assert(y2 >= y1);
  Assert((x1 >= 0) and (x2 < Width));
  Assert((y1 >= 0) and (y2 < Height));

  { TODO 2 -oLVK -cSource : Handle paletted images }
  DestinationBitmap.Width := (x2-x1+1);
  DestinationBitmap.Height := (y2-y1+1);
  DestinationBitmap.PixelFormat := pf24bit;

  CopyWidth := (x2-x1+1);
  LeftEdge := x1*PixelSize;
  for y := y1 to y2 do
  begin
    DestPtr := DestinationBitmap.ScanLine[y-y1];
    SourcePtr := ImagePtr + y*LineSize + LeftEdge;
    Move24bitSwap(SourcePtr^, DestPtr^, CopyWidth);
  end;
end;

function TlvkHSIRAW.GetInteger(const Index: Integer): SmallInt;
begin
  case Index of
    iWidth        : Result := FHeader^.Width;
    iHeight       : Result := FHeader^.Height;
    iPaletteSize  : Result := FHeader^.PaletteSize;
    iHorizDPI     : Result := FHeader^.HorizDPI;
    iVertDPI      : Result := FHeader^.VertDPI;
    iGamma        : Result := FHeader^.Gamma;
  else
    raise Exception.Create('Internal error');
  end;

  Result := Swap(Result);
end;

procedure TlvkHSIRAW.ReadHeader(out Width, Height, LineSize, PixelSize, ImageStart: Integer);
begin
  if not CompareMem(FilePointer, @CMagicNumber, SizeOf(CMagicNumber)) then
    raise Exception.Create('Not a HSI RAW image file');

  FHeader := PHeader(FilePointer);
  if Swap(FHeader^.Version) <> $0004 then
    raise Exception.Create(SNotAccepted);

  if Swap(FHeader^.AlphaChannel) <> 0 then
    raise Exception.Create(SNotAccepted);
  if Swap(FHeader^.Compression) <> 0 then
    raise Exception.Create(SNotAccepted);

  ImageStart := SizeOf(THeader);
  if Swap(FHeader^.PaletteSize) > 0 then
  begin
    ImageStart := ImageStart + Swap(FHeader^.PaletteSize)*3;
    PixelSize := 1;
  end else
    PixelSize := 3;
  LineSize := Swap(FHeader^.Width) * PixelSize;
  Height := FHeader^.Height;
  Width := FHeader^.Width;
end;

procedure TlvkHSIRAW.SetInteger(const Index: Integer; const Value: SmallInt);
begin
  case Index of
    iHorizDPI     : FHeader^.HorizDPI := Swap(Value);
    iVertDPI      : FHeader^.VertDPI := Swap(Value);
    iGamma        : FHeader^.Gamma := Swap(Value);
  else
    raise Exception.Create('Internal error');
  end;
end;

end.
