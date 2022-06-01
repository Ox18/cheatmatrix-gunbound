{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the class for large bitmap support. With this class you
    can load large bitmaps in blocks and pixels without having to load the
    entire bitmap into memory in one go.
  See also:
    TlvkLargeImage, TlvkBitmap
}
unit lvkLargeImage;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkLargeImage.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  Windows, Classes, SysUtils, Graphics;

type
  { Description:
      This class is used to read large image files directly from disk. The image
      has to be stored in an uncompressed manner so that the program can
      read bits of the image as needed. Note that this is an abstract class
      so that you must create descendant classes of this instead of using the
      base class by itself. One such descendant is TlvkBitmap.

      Descendant classes must override the ReadHeader and the
      Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap methods.

      TlvkLargeImage.Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap.

      Note: The file will be mapped into memory. A pointer to the first byte in
        the file is available in the TlvkLargeImage.FilePointer property.
    See also:
      TlvkBitmap, TlvkLargeImage.FilePointer
  }
  TlvkLargeImage = class
  private
    FFilename     : string;
    FFile         : THandle;
    FFileMapping  : THandle;
    FFilePointer  : PChar;

    FWidth        : Integer;
    FHeight       : Integer;
    FPixelSize    : Integer;
    FLineSize     : Integer;
    FImageStart   : Integer;

  protected
    { Description:
        Descendant classes must override this method in order to provide the
        class with information about the image data in the file. It should also
        verify that the file that is being opened is indeed a valid file for
        that descendant class, and raise an exception if not.

        The class should set all the output parameters with appropriate values.
      Parameters:
        Width       - How many pixels wide is the image in the file.
        Height      - How many pixels high is the image in the file.
        LineSize    - How many bytes are stored for one single line of image
          data. This might simply be PixelSize * Width, but might also contain
          padding bytes appended to the end of the line.
        PixelSize   - How many bytes per pixel. A 24-bit image would require
          3 bytes per pixel.
        ImageStart  - The byte offset in the file of where the actual image data
          starts.
    }
    procedure ReadHeader(out Width, Height, LineSize, PixelSize, ImageStart: Integer); virtual; abstract;

    { Description:
        This internal method can be used by descendants to get a pointer to the
        memorymap provided with the file contents.
    }
    property FilePointer: PChar read FFilePointer;

  public
    { Description:
        This method creates an instance of the large image class and maps the
        file into memory. The last thing it does is calling ReadHeader to
        load header information about the image.
      Parameters:
        Filename        - The file to open, must contain full path.
        NeedWriteAccess - Set this to True if you need to modify the image. If
          not, the default is False and will map a readonly copy into memory.
      See also:
        Destroy, ReadHeader
    }
    constructor Create(const Filename: string; const NeedWriteAccess: Boolean=False);

    { Description:
        This destructor unmaps the memory for the image data, closes the file
        and then destroys the image object instance.
    }
    destructor Destroy; override;

    { Description:
        This method returns a pointer to where the pixel data starts in memory.
        If the image follows the most used way of storing data (top to bottom,
        left to right), the top left pixel will start at this pointer.
      Returns:
        Pointer to the first pixel data stored in the file.
      See also:
        PixelPtr, LinePtr
    }
    function ImagePtr: PChar; virtual;

    { Description:
        This method will give you a pointer reference to a specific pixel in
        the image.

        Note: If the file doesn't use top-bottom/left-right storage conventions,
          descendant classes must override this method to return a correct
          pointer.
      Parameters:
        x - The x coordinate, ranging from left (0) to right (Width-1).
        y - The y coordinate, ranging from top (0) to bottom (Height-1).
      Returns:
        Pointer to pixel data at the coordinates specified.
      See also:
        ImagePtr, LinePtr
    }
    function PixelPtr(const x, y: Integer): PChar; virtual;

    { Description:
        This method returns a pointer reference to the first pixel stored for
        a given line in the image.

        Note: If the file doesn't use top-bottom/left-right storage conventions,
          descendant classes must override this method to return a correct
          pointer.
      Parameters:
        y - The y coordinate of the line, ranging from top (0) to bottom
          (Height-1).
      See also:
        ImagePtr, PixelPtr
    }
    function LinePtr(const y: Integer): PChar; virtual;

    { Description:
        This method extracts a rectangular area of the image and copies it to
        a TBitmap instance.

        The area is defined by the Rect parameter. The method will internally
        call Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap to perform the
        image extraction.
      Parameters:
        Rect              - The rectangular area to extract.
        DestinationBitmap - The TBitmap instance to store the extracted data
          into.
      See also:
        Extract@SmallInt@SmallInt@TBitmap,
        Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap
    }
    procedure Extract(const Rect: TRect; const DestinationBitmap: TBitmap); overload; virtual;

    { Description:
        This method extracts a rectangular area of the image and copies it to
        a TBitmap instance.

        The area is defined by the x1, y1 parameters as well as the current
        width and height of the given DestinationBitmap parameter. The method
        will internally call Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap
        to perform the image extraction.
      Parameters:
        x1                - The left coordinate of the area to extract.
        y1                - The top coordinate of the area to extract.
        DestinationBitmap - The TBitmap instance to store the extracted data
          into. Width and height is used from the instance to find the size of
          the area to grab.
      See also:
        Extract@TRect@TBitmap,
        Extract@SmallInt@SmallInt@SmallInt@SmallInt@TBitmap
    }
    procedure Extract(const x1, y1: SmallInt; const DestinationBitmap: TBitmap); overload; virtual;

    { Description:
        This method extracts a rectangular area of the image and copies it to
        a TBitmap instance.

        Note: Descendant classes must override this method to provide a specific
          method to extract an area of pixels.
      Parameters:
        x1                - The left coordinate of the area to extract.
        y1                - The top coordinate of the area to extract.
        x2                - The right coordinate of the area to extract.
        y2                - The bottom coordinate of the area to extract.
        DestinationBitmap - The TBitmap instance to store the extracted data
          into.
      See also:
        Extract@TRect@TBitmap,
        Extract@SmallInt@SmallInt@TBitmap
    }
    procedure Extract(const x1, y1, x2, y2: SmallInt; const DestinationBitmap: TBitmap); overload; virtual; abstract;

    { Description:
        This property returns the width of the image in pixels.
      See also:
        Height, LineSize, PixelSize
    }
    property Width: Integer read FWidth;

    { Description:
        This property returns the height of the image in pixels.
      See also:
        Width, LineSize, PixelSize
    }
    property Height: Integer read FHeight;

    { Description:
        This method returns the width of a line in bytes. This might be as
        simple as PixelSize*Width, but might also contain any padding bytes
        appended to the end of a line of pixels.
      See also:
        Width, Height, PixelSize
    }
    property LineSize: Integer read FLineSize;

    { Description:
        This property returns the number of bytes needed to store a single
        pixel in the image. For a 24-bit image, this will be 3.
      See also:
        Width, Height, LineSize
    }
    property PixelSize: Integer read FPixelSize;
  end;

{ Description:
    This method is used internally to copy a set of 24-bit pixels from one
    memory area to another, swapping their byte order in the process.

    The swapping will be performed like this: ABC --> CBA.
  Parameters:
    Source  - The memory area to copy pixel data from.
    Dest    - The memory area to copy pixel data to.
    Count   - The number of pixels to copy.
  See also:
    Move32bitSwap
}
procedure Move24bitSwap(const Source; const Dest; const Count: Integer);

{ Description:
    This method is used internally to copy a set of 32-bit pixels from one
    memory area to another, swapping their byte order in the process.

    The swapping will be performed like this: ABCD --> CBAD.
  Parameters:
    Source  - The memory area to copy pixel data from.
    Dest    - The memory area to copy pixel data to.
    Count   - The number of pixels to copy.
  See also:
    Move24bitSwap
}
procedure Move32bitSwap(const Source; const Dest; const Count: Integer);

implementation

procedure Move24bitSwap(const Source; const Dest; const Count: Integer);
asm
  push  esi
  push  edi
  push  ecx
  push  eax

  mov   esi, Source
  mov   edi, Dest
  //mov   ecx, Count  // ecx is already count

  jcxz  @@exit
@@loop:
  mov   al, [esi]
  mov   [edi+2], al
  mov   al, [esi+1]
  mov   [edi+1], al
  mov   al, [esi+2]
  mov   [edi], al

  add   esi, 3
  add   edi, 3
  dec   cx
  jnz   @@loop

@@exit:
  pop   eax
  pop   ecx
  pop   edi
  pop   esi
end;

procedure Move32bitSwap(const Source; const Dest; const Count: Integer);
asm
  push  esi
  push  edi
  push  ecx
  push  eax

  mov   esi, Source
  mov   edi, Dest
  //mov   ecx, Count  // ecx is already count

  jcxz  @@exit
@@loop:
  mov   al, [esi]
  mov   [edi+2], al
  mov   al, [esi+1]
  mov   [edi+1], al
  mov   al, [esi+2]
  mov   [edi], al
  mov   al, [esi+3]
  mov   [edi+3], al

  add   esi, 4
  add   edi, 4
  dec   cx
  jnz   @@loop

@@exit:
  pop   eax
  pop   ecx
  pop   edi
  pop   esi
end;

{ TlvkLargeImage }

procedure TlvkLargeImage.Extract(const Rect: TRect;
  const DestinationBitmap: TBitmap);
begin
  Assert(Assigned(DestinationBitmap));
  Extract(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, DestinationBitmap);
end;

procedure TlvkLargeImage.Extract(const x1, y1: SmallInt;
  const DestinationBitmap: TBitmap);
begin
  Assert(Assigned(DestinationBitmap));
  Extract(x1, y1, x1+DestinationBitmap.Width-1, y1+DestinationBitmap.Height-1, DestinationBitmap);
end;

constructor TlvkLargeImage.Create(const Filename: string;
  const NeedWriteAccess: Boolean);
var
  Mode  : LongWord;
begin
  inherited Create;

  // Open file
  FFilename := Filename;
  Mode := GENERIC_READ;
  if NeedWriteAccess then
    Mode := Mode or GENERIC_WRITE;
  FFile := Windows.CreateFile(PChar(Filename), Mode,
    FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if FFile = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;

  // Create file mapping
  Mode := PAGE_READONLY;
  if NeedWriteAccess then
    Mode := PAGE_READWRITE;
  FFileMapping := CreateFileMapping(FFile, nil, Mode, 0, 0, nil);
  if FFileMapping = 0 then
    RaiseLastWin32Error;

  // Map view
  Mode := FILE_MAP_READ;
  if NeedWriteAccess then
    Mode := FILE_MAP_ALL_ACCESS;
  FFilePointer := MapViewOfFile(FFileMapping, Mode, 0, 0, 0);

  ReadHeader(FWidth, FHeight, FLineSize, FPixelSize, FImageStart);
end;

destructor TlvkLargeImage.Destroy;
begin
  UnmapViewOfFile(FFilePointer);
  CloseHandle(FFileMapping);
  CloseHandle(FFile);
  
  inherited;
end;

function TlvkLargeImage.LinePtr(const y: Integer): PChar;
begin
  Result := PixelPtr(0, y);
end;

function TlvkLargeImage.PixelPtr(const x, y: Integer): PChar;
begin
  Assert((x >= 0) and (x < FWidth));
  Assert((y >= 0) and (y < FHeight));
  Result := FFilePointer + FImageStart + FLineSize * y + FPixelSize * x;
end;

function TlvkLargeImage.ImagePtr: PChar;
begin
  Result := FFilePointer + FImageStart;
end;

end.
