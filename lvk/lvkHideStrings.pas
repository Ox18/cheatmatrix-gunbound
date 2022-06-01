{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the component TlvkHideStrings, a simple component
    for storing strings in a form which you don't want prying eyes to simply
    read out from the exe file with a hex editor or similar.
}
unit lvkHideStrings;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkHideStrings.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkComponents;

type
  { Description:
      This component allows you to store a set of strings in a form which isn't
      readable by a hex editor or similar tool.

      Note: The component employs a simple encoding scheme to hide the string
        contents so it isn't impossible to get hold of the text here, just a
        tad more difficult than just firing up the hex editor.
  }
  TlvkHideStrings = class(TlvkComponent)
  private
    FStrings  : TStrings;

    procedure SetStrings(const Value: TStrings);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure EncodeDecode(const Data: PChar; const Size: Integer);

  protected
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        This property allows you to add whatever text you want to be stored.
        You can use it as a block of text to be used elsewhere, or you can
        use the Name=Value system and use the .Values property of it in your
        programs.

        Note: This property is not saved as normal in the form, but instead
          encoded with a simple routine and saved as an internal property
          with the name Data.
    }
    property Strings: TStrings read FStrings write SetStrings stored False;
  end;

implementation

{ TlvkHideStrings }

constructor TlvkHideStrings.Create(AOwner: TComponent);
begin
  inherited;

  FStrings := TStringList.Create;
end;

procedure TlvkHideStrings.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Data', ReadData, WriteData, FStrings.Count > 0);
end;

destructor TlvkHideStrings.Destroy;
begin
  FStrings.Free;

  inherited;
end;

procedure TlvkHideStrings.EncodeDecode(const Data: PChar;
  const Size: Integer);
var
  Index : Integer;
begin
  for Index := 0 to Size-1 do
    Data[Index] := Char(Ord(Data[Index]) xor $AA xor (Size and $FF));
end;

procedure TlvkHideStrings.ReadData(Stream: TStream);
var
  Size  : Cardinal;
  Data  : PChar;
begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  if Size > 0 then
  begin
    GetMem(Data, Size);
    try
      Stream.ReadBuffer(Data^, Size);
      EncodeDecode(Data, Size);
      FStrings.Text := Data;
    finally
      FreeMem(Data);
    end;
  end;
end;

procedure TlvkHideStrings.SetStrings(const Value: TStrings);
begin
  if Assigned(Value) then
    FStrings.Assign(Value)
  else
    FStrings.Clear;
end;

procedure TlvkHideStrings.WriteData(Stream: TStream);
var
  Size  : Cardinal;
  Data  : PChar;
begin
  if FStrings.Count > 0 then
  begin
    Size := Length(FStrings.Text) + 1;
    GetMem(Data, Size);
    try
      StrPCopy(Data, FStrings.Text);
      EncodeDecode(Data, Size);

      Stream.WriteBuffer(Size, SizeOf(Size));
      Stream.WriteBuffer(Data^, Size);
    finally
      FreeMem(Data);
    end;
  end else begin
    Size := 0;
    Stream.WriteBuffer(Size, SizeOf(Size));
  end;
end;

end.
 