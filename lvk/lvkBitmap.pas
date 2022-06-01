{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkBitmap class, which is used to read large,
    uncompressed, bitmap (.BMP files) files directly from disk, instead
    of loading the whole bitmap into memory.
  See also:
    TlvkBitmap, TlvkLargeImage
}
unit lvkBitmap;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBitmap.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Graphics, lvkLargeImage;

type
  { Description:
      The THeader and PHeader types are used internally by the TlvkBitmap
      class.
    See also:
      TlvkBitmap
  }
  PHeader = ^THeader;
  THeader = packed record
    Identifier      : array[0..1] of Char;
    FileSize        : LongWord;
    Reserved1       : LongWord;
    BitmapOffset    : LongWord;
    HeaderSize      : LongWord;
    Width           : LongWord;
    Height          : LongWord;
    Planes          : Word;
    BitsPerPixel    : Word;
    Compression     : LongWord;
    BitmapDataSize  : LongWord;
    HResolution     : LongWord;
    VResolution     : LongWord;
    Colors          : LongWord;
    ImportantColors : LongWord;
    // Palette = n*4 bytes
    // Bitmap data
  end;

  { Description:
      This class is a descendant from TlvkLargeImage, and is used to
      read and write uncompressed .BMP files directly on disk, instead of
      loading the whole bitmap into memory.
    See also:
      TlvkLargeImage
  }
  TlvkBitmap = class(TlvkLargeImage)
  private
    FHeader : PHeader;

  protected
    // <ALIAS TlvkLargeImage.ReadHeader@Integer@Integer@Integer@Integer@Integer>
    procedure ReadHeader(out Width, Height, LineSize, PixelSize, ImageStart: Integer); override;

  public
    // <ALIAS TlvkLargeImage.Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap>
    procedure Extract(const x1, y1, x2, y2: SmallInt; const DestinationBitmap: TBitmap); overload; override;
    // <ALIAS TlvkLargeImage.PixelPtr@Integer@Integer>
    function PixelPtr(const x, y: Integer): PChar; override;
  end;

implementation

{ TlvkBitmap }

procedure TlvkBitmap.Extract(const x1, y1, x2, y2: SmallInt;
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

  case PixelSize of
    2 : DestinationBitmap.PixelFormat := pf16bit;
    3 : DestinationBitmap.PixelFormat := pf24bit;
    4 : DestinationBitmap.PixelFormat := pf32bit;
  end;

  CopyWidth := (x2-x1+1);
  LeftEdge := x1*PixelSize;
  for y := y1 to y2 do
  begin
    DestPtr := DestinationBitmap.ScanLine[y-y1];
    SourcePtr := ImagePtr + y*LineSize + LeftEdge;

    case PixelSize of
      2 : Move(SourcePtr^, DestPtr^, CopyWidth*2);
      3 : Move24bitSwap(SourcePtr^, DestPtr^, CopyWidth);
      4 : Move32bitSwap(SourcePtr^, DestPtr^, CopyWidth);
    end;
  end;
end;

function TlvkBitmap.PixelPtr(const x, y: Integer): PChar;
begin
  Assert((x >= 0) and (x < Width));
  Assert((y >= 0) and (y < Height));
  Result := ImagePtr + LineSize * (Height-1-y) + PixelSize * x;
end;

procedure TlvkBitmap.ReadHeader(out Width, Height, LineSize, PixelSize,
  ImageStart: Integer);
begin
  FHeader := PHeader(FilePointer);

  Width := FHeader^.Width;
  Height := FHeader^.Height;

  case FHeader^.BitsPerPixel of
    16    : PixelSize := 2;
    24    : PixelSize := 3;
    32    : PixelSize := 4;
  else
    raise Exception.Create('TlvkBitmap handles only paletted, high color and true color images');
  end;
  LineSize := Width * PixelSize;
  ImageStart := FHeader^.BitmapOffset;
end;

end.
