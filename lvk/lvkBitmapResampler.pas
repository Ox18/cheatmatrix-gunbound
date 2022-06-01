{ Description:
    This unit contains resampling algorithms for TBitmap image objects. The
    resampling algorithms will produce much better results than a simple
    StretchDraw will at the cost of extra time spent in the resampling
    routine.

    A simple description of the algorithm is as follows:
    First the routines stretch the image in the Width direction and produces
    a temporary image that is as wide as the output image but as high as the
    input image. Then the routines stretch this temporary image to be as high
    as the output image. The result is the output of the resampling routines.

    The internal stretching routine works on a line-for-line basis and is
    as follows: First the routine calculates for each pixel in the output
    what pixels in the input that will contribute to it and how much. When
    that is done, the stretching is simply done by looping through all the
    output pixels, summing up all the contributing pixels and storing the
    result in the output.

    The routines in here was adapted from Jedi-VCL by Lasse Vågsæther Karlsen.
}
unit lvkBitmapResampler;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 5 $
// $Archive: /Components/LVK/source/lvkBitmapResampler.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
{$IFDEF LINUX}
  QGraphics;
{$ELSE}
  Graphics;
{$ENDIF}

type
  { Description:
      These algorithms decide how the input pixels contribute to each output
      pixel. Instead of describing each algorithm here (which I can't anyway
      since I don't know enough about them) I suggest you take a handful of
      images that are typical input images, resample them with the different
      algorithms to a size that is a typical output size and see which looks
      best or takes the least time.

      Note: The raBox algorithm is generally not good for anything but
        producing output images that are larger than the input image.
  }
  TResampleAlgorithm = (raSpline, raBox, raTriangle, raHermite, raBell,
    raLanczos3, raMitchell);

const
	// This is the default SampleWidth parameter for all the resampling algorithms. If you use
	// this parameter, the code will automatically use the correct default SampleWidth corresponding
	// to the algorithm you have chosen to use.
  DEFAULT_SAMPLE_WIDTH          = -1.0;

	// This is the default SampleWidth parameter for the Spline resampling algorithm.
  DEFAULT_SPLINE_SAMPLE_WIDTH   = 2.0;
	// This is the default SampleWidth parameter for the Box resampling algorithm.
  DEFAULT_BOX_SAMPLE_WIDTH      = 0.5;
	// This is the default SampleWidth parameter for the Triangle resampling algorithm.
  DEFAULT_TRIANGLE_SAMPLE_WIDTH = 1.0;
	// This is the default SampleWidth parameter for the Hermite resampling algorithm.
  DEFAULT_HERMITE_SAMPLE_WIDTH  = 1.0;
	// This is the default SampleWidth parameter for the Bell resampling algorithm.
  DEFAULT_BELL_SAMPLE_WIDTH     = 1.5;
	// This is the default SampleWidth parameter for the Lanczos3 resampling algorithm.
  DEFAULT_LANCZOS3_SAMPLE_WIDTH = 3.0;
	// This is the default SampleWidth parameter for the Mitchell resampling algorithm.
  DEFAULT_MITCHELL_SAMPLE_WIDTH = 2.0;

{ Description:
    ResampleWidth will resample an image in the Width direction.
    Both images must have a pixelformat of pf32bit.
  Parameters:
    Input - The object that holds the image to resample.
    Output - The object that will contain the resampled image. This object
      must be of the correct width (that the new image will be resampled to)
      and must be of the same height as the Input image.
    Algorithm - Which resampling algorithm to use.
    SampleWidth - All the algorithms use a samplewidth to determine how many
      pixels in the input image that will influence each pixel in the output
      image. For most uses, the default value here (DEFAULT_SAMPLE_WIDTH)
      is sufficient.
  See also:
    ResampleHeight, ResampleWidthHeight, Resample, TResampleAlgorithm
}
procedure ResampleWidth(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm=raSpline;
  const SampleWidth: Double=DEFAULT_SAMPLE_WIDTH);

{ Description:
    ResampleWidth will resample an image in the Height direction.
    Both images must have a pixelformat of pf32bit.
  Parameters:
    Input - The object that holds the image to resample.
    Output - The object that will contain the resampled image. This object
      must be of the correct height (that the new image will be resampled to)
      and must be of the same width as the Input image.
    Algorithm - Which resampling algorithm to use.
    SampleWidth - All the algorithms use a samplewidth to determine how many
      pixels in the input image that will influence each pixel in the output
      image. For most uses, the default value here (DEFAULT_SAMPLE_WIDTH)
      is sufficient.
  See also:
    ResampleWidth, ResampleWidthHeight, Resample, ResampleWithinConstraints,
    TResampleAlgorithm
}
procedure ResampleHeight(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm=raSpline;
  const SampleWidth: Double=DEFAULT_SAMPLE_WIDTH);

{ Description:
    ResampleWidthHeight will resample an image in both the Width and the
    Height direction. Both images must have a pixelformat of pf32bit.
  Parameters:
    Input - The object that holds the image to resample.
    Output - The object that will contain the resampled image. This object
      must be of the correct width and height (that the new image will be
      resampled to).
    Algorithm - Which resampling algorithm to use.
    SampleWidth - All the algorithms use a samplewidth to determine how many
      pixels in the input image that will influence each pixel in the output
      image. For most uses, the default value here (DEFAULT_SAMPLE_WIDTH)
      is sufficient.
  See also:
    ResampleWidth, ResampleHeight, ResampleWidthHeight,
    ResampleWithinConstraints, TResampleAlgorithm
}
procedure ResampleWidthHeight(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm=raSpline;
  const SampleWidth: Double=DEFAULT_SAMPLE_WIDTH);

{ Description:
    The Resample procedure will resample an image stored in the Input
    object parameter and store the result in the Output object parameter.
    Output must be a pre-created TBitmap object instance and must have
    a width and height. The resampled image will then be resampled to this
    width and height.

    Note: Internally, Resample will use ResampleWidth, ResampleHeight or
      ResampleWidthHeight depending on the sizes of the objects.
  Parameters:
    Input - The object that holds the image to resample.
    Output - The object that will contain the resampled image. This object
      must be of the correct width and height (that the new image will be
      resampled to).
    Algorithm - Which resampling algorithm to use.
    SampleWidth - All the algorithms use a samplewidth to determine how many
      pixels in the input image that will influence each pixel in the output
      image. For most uses, the default value here (DEFAULT_SAMPLE_WIDTH)
      is sufficient.
  See also:
    ResampleWidth, ResampleHeight, ResampleWidthHeight,
    ResampleWithinConstraints, TResampleAlgorithm
}
procedure Resample(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm=raSpline;
  const SampleWidth: Double=DEFAULT_SAMPLE_WIDTH);

{ Description:
    The ResampleWithinConstraints procedure will take the image in the Input
    parameter and resample it to be of a size less than or equal to MaxWidth
    and MaxHeight. The aspect ratio of the input image will be preserved.
    The new, resampled, image will be stored in the Output parameter which
    will have its contents replaced. Output must be a pre-created TBitmap object
    instance.
  Parameters:
    Input - The object that holds the image to resample.
    Output - The object that will contain the resampled image.
    MaxWidth - The maximum number of pixels in the Width direction that the
      new, resampled, image will contain.
    MaxHeight - The maximum number of pixels in the Height direction that the
      new, resampled, image will contain.
    Algorithm - Which resampling algorithm to use.
    SampleWidth - All the algorithms use a samplewidth to determine how many
      pixels in the input image that will influence each pixel in the output
      image. For most uses, the default value here (DEFAULT_SAMPLE_WIDTH)
      is sufficient.
  See also:
    ResampleWidth, ResampleHeight, ResampleWidthHeight, Resample,
    TResampleAlgorithm
}
procedure ResampleWithinConstraints(const Input, Output: TBitmap;
  const MaxWidth, MaxHeight: Integer;
  const Algorithm: TResampleAlgorithm=raSpline;
  const SampleWidth: Double=DEFAULT_SAMPLE_WIDTH);

implementation

uses
  Math;

type
  // Contributor for a pixel
  TContributor = record
    Pixel   : Integer;  // Source pixel
    Weight  : Integer;  // Pixel weight
  end;

  // List of source pixels contributing to a destination pixel
  PPixel = ^TPixel;
  TPixel = record
    PixelNum      : Integer;
    Contributors  : array of TContributor;
  end;

  TPixelContributionList = array of TPixel;

  TAlgorithm = function(const Value: Double): Double;

function SplineAlgorithm(const Value: Double): Double; forward;
function BoxAlgorithm(const Value: Double): Double; forward;
function TriangleAlgorithm(const Value: Double): Double; forward;
function HermiteAlgorithm(const Value: Double): Double; forward;
function BellAlgorithm(const Value: Double): Double; forward;
function Lanczos3Algorithm(const Value: Double): Double; forward;
function MitchellAlgorithm(const Value: Double): Double; forward;

const
  Algorithms  : array[TResampleAlgorithm] of TAlgorithm = (
    SplineAlgorithm, BoxAlgorithm, TriangleAlgorithm, HermiteAlgorithm,
    BellAlgorithm, Lanczos3Algorithm, MitchellAlgorithm
  );

procedure CalculateContributions(out Contrib: TPixelContributionList;
  const SourceLength, DestinationLength: Integer;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
var
  Scale           : Double;
  Width           : Double;
  FScale          : Double;
  Center          : Double;
  Weight          : Double;

  PixelNum        : Integer;
  UseSampleWidth  : Double;
  Left            : Integer;
  Right           : Integer;
  Index1          : Integer;
  Index2          : Integer;
  Index3          : Integer;

const
  DefaultSampleWidths : array[TResampleAlgorithm] of Double = (
    DEFAULT_SPLINE_SAMPLE_WIDTH,
    DEFAULT_BOX_SAMPLE_WIDTH,
    DEFAULT_TRIANGLE_SAMPLE_WIDTH,
    DEFAULT_HERMITE_SAMPLE_WIDTH,
    DEFAULT_BELL_SAMPLE_WIDTH,
    DEFAULT_LANCZOS3_SAMPLE_WIDTH,
    DEFAULT_MITCHELL_SAMPLE_WIDTH
  );
begin
  Assert((SampleWidth = DEFAULT_SAMPLE_WIDTH) or (SampleWidth > 0));
  if SampleWidth = DEFAULT_SAMPLE_WIDTH then
    UseSampleWidth := DefaultSampleWidths[Algorithm]
  else
    UseSampleWidth := SampleWidth;

  if SourceLength = 1 then
    Scale := DestinationLength / SourceLength
  else
    Scale := (DestinationLength-1) / (SourceLength-1);

  // Pre-calculate filter contributions
  SetLength(Contrib, DestinationLength);
  if Scale < 1.0 then
  begin
    // Scales from bigger to smaller width
    Width := UseSampleWidth / Scale;
    FScale := 1.0 / Scale;

    for Index1 := 0 to DestinationLength-1 do
    begin
      Contrib[Index1].PixelNum := 0;
      SetLength(Contrib[Index1].Contributors, Trunc(Width*2.0+2));
      Center := Index1 / Scale;
      Left := Floor(Center - Width);
      Right := Ceil(Center + Width);

      for Index2 := Left to Right do
      begin
        Weight := Algorithms[Algorithm]((Center-Index2) / FScale) / FScale;
        if Weight = 0.0 then
          Continue;
        if Index2 < 0 then
          PixelNum := -Index2
        else if Index2 >= Integer(SourceLength) then
          PixelNum := Integer(SourceLength) - Index2 + Integer(SourceLength) - 1
        else
          PixelNum := Index2;

        Index3 := Contrib[Index1].PixelNum;
        Contrib[Index1].PixelNum := Contrib[Index1].PixelNum + 1;
        Contrib[Index1].Contributors[Index3].Pixel := PixelNum;
        Contrib[Index1].Contributors[Index3].Weight := Trunc(Weight * 65536);
      end;
    end;
  end else begin
    // Scales from smaller to bigger width
    for Index1 := 0 to DestinationLength-1 do
    begin
      Contrib[Index1].PixelNum := 0;
      SetLength(Contrib[Index1].Contributors, Trunc(UseSampleWidth*2.0+3));
      Center := Index1 / Scale;
      Left := Floor(Center - UseSampleWidth);
      Right := Ceil(Center + UseSampleWidth);
      for Index2 := Left to Right do
      begin
        Weight := Algorithms[Algorithm](Center-Index2);
        if Weight = 0.0 then
          Continue;
        if Index2 < 0 then
          PixelNum := -Index2
        else if Index2 >= Integer(SourceLength) then
          PixelNum := Integer(SourceLength) - Index2 + Integer(SourceLength) - 1
        else
          PixelNum := Index2;
        Index3 := Contrib[Index1].PixelNum;
        Inc(Contrib[Index1].PixelNum);
        Contrib[Index1].Contributors[Index3].Pixel := PixelNum;
        Contrib[Index1].Contributors[Index3].Weight := Trunc(Weight * 65536);
      end;
    end;
  end;
end;

procedure ResampleLine(const InputLine, OutputLine: PChar;
  const InputWidth, OutputWidth: Integer;
  const PixelContributionList: TPixelContributionList);
var
  x                 : Integer;
  Color             : array[0..3] of Integer;
  ByteValue         : Byte;
  Pixel             : PPixel;
  Channel           : Integer;
  Weight            : Integer;
  PixelIndex        : Integer;
  ContributionIndex : Integer;
begin
  Assert(Assigned(InputLine) and Assigned(OutputLine));
  Assert((InputWidth > 0) and (OutputWidth > 0));
  Assert(Length(PixelContributionList) = OutputWidth);

  for x := 0 to OutputWidth-1 do
  begin
    Pixel := @PixelContributionList[x];
    for Channel := 0 to 3 do
      Color[Channel] := 0;

    for ContributionIndex := 0 to Pixel^.PixelNum-1 do
    begin
      Weight := Pixel^.Contributors[ContributionIndex].Weight;
      PixelIndex := Pixel^.Contributors[ContributionIndex].Pixel;

      for Channel := 0 to 3 do
        Color[Channel] := Color[Channel] + Ord(InputLine[PixelIndex*4 + Channel])*Weight;
    end;

    for Channel := 0 to 3 do
    begin
      if Color[Channel] < 0 then
        ByteValue := 0
      else if Color[Channel] > 255*65536 then
        ByteValue := 255
      else
        ByteValue := Trunc(Color[Channel]) shr 16;

      OutputLine[x*4 + Channel] := Char(ByteValue);
    end;
  end;
end;

procedure ResampleWidth(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
var
  y                   : Integer;
  PixelContributions  : TPixelContributionList;
begin
  // Validate input parameters
  Assert(Assigned(Input));
  Assert((Input.Width > 0) and (Input.Height > 0));
  Assert(Assigned(Output));
  Assert((Output.Width > 0) and (Output.Height > 0));
  Assert(Input.Height = Output.Height);
  Assert((Input.PixelFormat = pf32bit) and (Output.PixelFormat = pf32bit));

  // Resample the image
  if Input.Width = Output.Width then
    Output.Assign(Input)
  else begin
    CalculateContributions(PixelContributions, Input.Width, Output.Width,
      Algorithm, SampleWidth);

    for y := 0 to Input.Height-1 do
      ResampleLine(Input.ScanLine[y], Output.ScanLine[y], Input.Width,
        Output.Width, PixelContributions);
  end;
end;

procedure ResampleHeight(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
type
  PCardinal = ^Cardinal;
var
  x, y                : Integer;
  PixelContributions  : TPixelContributionList;
  InputBuffer         : PChar;
  OutputBuffer        : PChar;
  InputPtr            : PChar;
  OutputPtr           : PChar;
  Width4              : Integer;
begin
  // Validate input parameters
  Assert(Assigned(Input));
  Assert((Input.Width > 0) and (Input.Height > 0));
  Assert(Assigned(Output));
  Assert((Output.Width > 0) and (Output.Height > 0));
  Assert(Input.Width = Output.Width);
  Assert((Input.PixelFormat = pf32bit) and (Output.PixelFormat = pf32bit));

  // Resample the image
  if Input.Height = Output.Height then
    Output.Assign(Input)
  else begin
    CalculateContributions(PixelContributions, Input.Height, Output.Height,
      Algorithm, SampleWidth);

    InputBuffer := nil;
    OutputBuffer := nil;
    try
      GetMem(InputBuffer, Input.Height * 4);
      GetMem(OutputBuffer, Output.Height * 4);

      for x := 0 to Input.Width-1 do
      begin
        // First convert a single column of pixels into a continuous memory buffer
        if PChar(Input.ScanLine[0]) > PChar(Input.ScanLine[1]) then
          InputPtr := PChar(Input.ScanLine[Input.Height-1]) + x*4
        else
          InputPtr := PChar(Input.ScanLine[0]) + x*4;
        OutputPtr := InputBuffer;
        Width4 := Input.Width * 4;
        for y := 0 to Input.Height-1 do
        begin
          PCardinal(OutputPtr)^ := PCardinal(InputPtr)^;
          Inc(InputPtr, Width4);
          Inc(OutputPtr, 4);
        end;

        // Then resample this buffer
        ResampleLine(InputBuffer, OutputBuffer, Input.Height, Output.Height,
          PixelContributions);

        // Then convert the result buffer back into a column of pixels
        InputPtr := OutputBuffer;
        if PChar(Output.ScanLine[0]) > PChar(Output.ScanLine[1]) then
          OutputPtr := PChar(Output.ScanLine[Output.Height-1]) + x*4
        else
          OutputPtr := PChar(Output.ScanLine[0]) + x*4;
        Width4 := Output.Width * 4;
        for y := 0 to Output.Height-1 do
        begin
          PCardinal(OutputPtr)^ := PCardinal(InputPtr)^;
          Inc(InputPtr, 4);
          Inc(OutputPtr, Width4);
        end;
      end;
    finally
      if Assigned(InputBuffer) then
        FreeMem(InputBuffer);
      if Assigned(OutputBuffer) then
        FreeMem(OutputBuffer);
    end;
  end;
end;

procedure ResampleWidthHeight(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
var
  Inbetween : TBitmap;
begin
  // Validate input parameters
  Assert(Assigned(Input));
  Assert((Input.Width > 0) and (Input.Height > 0));
  Assert(Assigned(Output));
  Assert((Output.Width > 0) and (Output.Height > 0));
  Assert((Input.PixelFormat = pf32bit) and (Output.PixelFormat = pf32bit));

  // Resample the image
  if (Input.Width = Output.Width) and (Input.Height = Output.Height) then
    Output.Assign(Input)
  else begin
    Inbetween := TBitmap.Create;
    try
      Inbetween.Width := Input.Width;
      Inbetween.Height := Output.Height;
      Inbetween.PixelFormat := pf32bit;

      ResampleHeight(Input, Inbetween, Algorithm, SampleWidth);
      ResampleWidth(Inbetween, Output, Algorithm, SampleWidth);
    finally
      Inbetween.Free;
    end;
  end;
end;

procedure Resample(const Input, Output: TBitmap;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
begin
  // Validate input parameters
  Assert(Assigned(Input));
  Assert((Input.Width > 0) and (Input.Height > 0));
  Assert(Assigned(Output));
  Assert((Output.Width > 0) and (Output.Height > 0));

  // Decide which resampling function to use
  if (Input.Width = Output.Width) and (Input.Height = Output.Height) then
    Output.Assign(Input)
  else if (Input.Width = Output.Width) then
    ResampleHeight(Input, Output, Algorithm, SampleWidth)
  else if (Input.Height = Output.Height) then
    ResampleWidth(Input, Output, Algorithm, SampleWidth)
  else
    ResampleWidthHeight(Input, Output, Algorithm, SampleWidth);
end;

procedure ResampleWithinConstraints(const Input, Output: TBitmap;
  const MaxWidth, MaxHeight: Integer;
  const Algorithm: TResampleAlgorithm;
  const SampleWidth: Double);
var
  w, h  : Integer;
begin
  // Validate input parameters
  Assert(Assigned(Input));
  Assert((Input.Width > 0) and (Input.Height > 0));
  Assert(Assigned(Output));
  Assert((MaxWidth > 0) and (MaxHeight > 0));

  // Calculate new size within the constraints
  w := Input.Width;
  h := Input.Height;
  if w > MaxWidth then
  begin
    h := Trunc(h * (MaxWidth/w));
    w := MaxWidth;
  end;
  if h > MaxHeight then
  begin
    w := Trunc(w * (MaxHeight/h));
    h := MaxHeight;
  end;

  // Configure the output object and resample the image
  Output.Width := w;
  Output.Height := h;
  Output.PixelFormat := pf32bit;
  Resample(Input, Output, Algorithm, SampleWidth);
end;

// Algorithms

function SplineAlgorithm(const Value: Double): Double;
var
  TempValue : Double;
  Temp      : Double;
begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;

  if TempValue < 1.0 then
  begin
    Temp := Sqr(TempValue);
    Result := 0.5*Temp*TempValue - Temp + 2.0/3.0;
  end else if TempValue < 2.0 then
  begin
    TempValue := 2.0-TempValue;
    Result := 1.0/6.0 * Sqr(TempValue) * TempValue;
  end else
    Result := 0.0;
end;

function BoxAlgorithm(const Value: Double): Double;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

function TriangleAlgorithm(const Value: Double): Double;
var
  TempValue : Double;
begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;

  if TempValue < 1.0 then
    Result := 1.0 - TempValue
  else
    Result := 0.0;
end;

function HermiteAlgorithm(const Value: Double): Double;
var
  TempValue : Double;
begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;
  if TempValue < 1.0 then
    Result := (2.0 * TempValue - 3.0) * Sqr(TempValue) + 1.0
  else
    Result := 0.0;
end;

function BellAlgorithm(const Value: Double): Double;
var
  TempValue : Double;
begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;
  if TempValue < 0.5 then
    Result := 0.75 - Sqr(TempValue)
  else if TempValue < 1.5 then
  begin
    TempValue := TempValue - 1.5;
    Result := 0.5 * Sqr(TempValue);
  end else
    Result := 0.0;
end;

function Lanczos3Algorithm(const Value: Double): Double;
var
  TempValue : Double;

  function SinC(Value: Double): Double;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end else
      Result := 1.0;
  end;

begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;
  if TempValue < 3.0 then
    Result := SinC(TempValue) * SinC(TempValue / 3.0)
  else
    Result := 0.0;
end;

function MitchellAlgorithm(const Value: Double): Double;
const
  B		= (1.0 / 3.0);
  C		= (1.0 / 3.0);
var
  Temp      : Double;
  TempValue : Double;
begin
  if Value < 0.0 then
    TempValue := -Value
  else
    TempValue := Value;
  Temp := Sqr(TempValue);
  if (TempValue < 1.0) then
  begin
    TempValue := (((12.0 - 9.0 * B - 6.0 * C) * (TempValue * Temp))
      + ((-18.0 + 12.0 * B + 6.0 * C) * Temp)
      + (6.0 - 2 * B));
    Result := TempValue / 6.0;
  end else
  if (TempValue < 2.0) then
  begin
    TempValue := (((-1.0 * B - 6.0 * C) * (TempValue * Temp))
      + ((6.0 * B + 30.0 * C) * Temp)
      + ((-12.0 * B - 48.0 * C) * TempValue)
      + (8.0 * B + 24 * C));
    Result := TempValue / 6.0;
  end else
    Result := 0.0;
end;

end.
