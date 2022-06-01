{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkFormBinaryFile component which can be used to
    attach binary data files to a form and have them saved as part of your
    program.
}
unit lvkFormBinaryFile;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFormBinaryFile.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkComponents;

const
  DEFAULT_AUTO_EXTRACT  = False;
  DEFAULT_AUTO_DELETE   = True;

type
  { Description:
      This is the item that is stored in the collection in the
      TlvkFormBinaryFileCollection component and each item contains one file.

      See the Filename property on how to load a file into this item.
    See also:
      TlvkFileItemCollection, TlvkFormBinaryFileCollection
  }
  TlvkFileItem = class(TCollectionItem)
  private
    FName               : string;
    FFilename           : string;
    FData               : TMemoryStream;
    FAutoExtract        : Boolean;
    FAutoDelete         : Boolean;
    FExtractedFilename  : string;

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure SetName(const Value: string);
    procedure SetFilename(const Value: string);
    procedure AutoExtractFile;

  protected
    function GetDisplayName: string; override;
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    { Description:
        This method is used by the property editor to load a new file into the
        item. Since at runtime, files are not saved back into the file, this
        method is almost useless for the end-user programmer.
    }
    procedure LoadFile(const Filename: string);

    { Description:
        The Data property is of type
        <EXTLINK borland://TMemoryStream_object>TMemoryStream</EXTLINK>
        and holds the data that
        have been loaded into the component.
      See also:
        Filename
    }
    property Data: TMemoryStream read FData;

    { Description:
        If you set the AutoExtract property to True, the file stored in the
        component is automatically extracted to the windows temp directory
        when the them is loaded into memory. This property, ExtractedFilename,
        contains the path and filename of the extracted file.
      See also:
        AutoExtract, AutoDelete
    }
    property ExtractedFilename: string read FExtractedFilename;

  published
    { Description:
        Each item can be given a name which you can later use in a call to the
        TlvkFileItemCollection.ItemsByName property to access a file item with
        a specified name.

        All items that are given a name must have a unique name within the
        component. A item can also be added without a name.

        Whenever you load a file into an item, if the item has no name at the
        moment, the name of the file without an extension will be used, if
        that name is not already taken.

        Note: Names are not case sensitive.
      See also:
        Filename
    }
    property Name: string read FName write SetName;

    { Description:
        This property holds the name of the file loaded into the component,
        except for the path information which was stripped out when the
        file was loaded into the component.

        In the object inspector you have a "..." button next to this property
        and you use this button to load a new file into the component.
      See also:
        Data
    }
    property Filename: string read FFilename write SetFilename stored False;

    { Description:
        Set this property to True and whenever this form is loaded, the file
        is extracted to the default windows temp directory. You can get the
        path and filename of the extracted file by reading the ExtractedFilename
        property.
      See also:
        ExtractedFilename, AutoDelete
    }
    property AutoExtract: Boolean read FAutoExtract write FAutoExtract
      default DEFAULT_AUTO_EXTRACT;

    { Description:
        If you set the AutoExtract property to True, the file stored in the
        component will be automatically extracted once the form is loaded.
        Set the AutoDelete property to True to have the file deleted when the
        form is unloaded from memory. If you set the property to False, the
        file will be left.
      See also:
        AutoExtract, ExtractedFilename
    }
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete
      default DEFAULT_AUTO_DELETE;
  end;

  { Description:
      This class is used as the collection type that stores the individual
      file items for TlvkFormBinaryFileCollection.
    See also:
      TlvkFormBinaryFileCollection, TlvkFileItem
  }
  TlvkFileItemCollection = class(TOwnedCollection)
  protected
    function GetItem(const Index: Integer): TlvkFileItem; virtual;
    procedure SetItem(const Index: Integer; const Value: TlvkFileItem); virtual;
    function GetItemsByName(const Name: string): TlvkFileItem; virtual;

  public
    constructor Create(AOwner: TPersistent);

    function Add: TlvkFileItem;
    function FindItemID(const ID: Integer): TlvkFileItem;
    function Insert(const Index: Integer): TlvkFileItem;
    property Items[const Index: Integer]: TlvkFileItem read GetItem
      write SetItem;

    { Description:
        This property gives access to the items stored in the collection by the
        name the programmer gave to them during design.

        Note: NIL will be returned if no item with the given name exists.
        Note: Names are not case sensitive.
      See also:
        TlvkFileItem.Name
    }
    property ItemsByName[const Name: string]: TlvkFileItem read GetItemsByName;
  end;

  { Description:
      This component is like TlvkFormBinaryFile, except that it allows you to
      store several files in a single component.
    Parameters:
      -
    See also:
      -
  }
  TlvkFormBinaryFileCollection = class(TlvkComponent)
  private
    FFiles  : TlvkFileItemCollection;

    procedure SetFiles(const Value: TlvkFileItemCollection);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        This property holds the collection of files stored in the component.
        It's a normal collection property so use the collection property editor
        to access the items stored in it.
      See also:
        TlvkFileItemCollection, TlvkFileItem
    }
    property Files: TlvkFileItemCollection read FFiles write SetFiles;
  end;

  { Description:
      This exception class will be used for all exceptions raised from the
      TlvkFormBinaryFile class.
    See also:
      TlvkFormBinaryFileCollection
  }
  ElvkFormBinaryFile = class(Exception);

const
  VERSION_1 = $0100;

procedure EncodeDecode(const Data: PChar; const Size: Integer);

implementation

uses
  Windows, lvkTempFile;

procedure EncodeDecode(const Data: PChar; const Size: Integer);
var
  Index : Integer;
begin
  for Index := 0 to Size-1 do
    Data[Index] := Char(Ord(Data[Index]) xor $AA xor (Size and $FF));
end;

{ TlvkFormBinaryFileCollection }

constructor TlvkFormBinaryFileCollection.Create(AOwner: TComponent);
begin
  inherited;

  FFiles := TlvkFileItemCollection.Create(Self);
end;

destructor TlvkFormBinaryFileCollection.Destroy;
begin
  FreeAndNil(FFiles);
  
  inherited;
end;

procedure TlvkFormBinaryFileCollection.SetFiles(
  const Value: TlvkFileItemCollection);
begin
  if Assigned(Value) then
    FFiles.Assign(Value)
  else
    FFiles.Clear;
end;

{ TlvkFileItem }

procedure TlvkFileItem.AutoExtractFile;
var
  TempPath  : array[0..MAX_PATH] of Char;
begin
  Win32Check(GetTempPath(SizeOf(TempPath), TempPath) > 0);

  FExtractedFilename := TempPath + Filename;
  FData.SaveToFile(FExtractedFilename);

  if FAutoDelete then
    lvkRegisterTempFile(FExtractedFilename);
end;

constructor TlvkFileItem.Create(Collection: TCollection);
begin
  inherited;

  FData := TMemoryStream.Create;
  FFilename := '';
  FAutoDelete := DEFAULT_AUTO_DELETE;
  FAutoExtract := DEFAULT_AUTO_EXTRACT;
end;

procedure TlvkFileItem.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Data', ReadData, WriteData, FFilename <> '');
end;

destructor TlvkFileItem.Destroy;
begin
  FreeAndNil(FData);

  inherited;
end;

function TlvkFileItem.GetDisplayName: string;
begin
  if FFilename <> '' then
    Result := FFilename + ': ' + IntToStr(FData.Size) + ' byte(s)'
  else
    Result := inherited GetDisplayName;
end;

procedure TlvkFileItem.LoadFile(const Filename: string);
begin
  FFilename := ExtractFilename(Filename);
  FData.LoadFromFile(Filename);

  if FName = '' then
  begin
    if not Assigned((Collection as TlvkFileItemCollection).ItemsByName[ChangeFileExt(FFilename, '')]) then
      FName := ChangeFileExt(FFilename, '');
  end;
end;

procedure TlvkFileItem.ReadData(Stream: TStream);
var
  Version : Word;

  procedure ReadVersion1(const Stream: TStream);
  var
    Size  : Cardinal;
  begin
    Stream.ReadBuffer(Size, SizeOf(Size));
    FData.Clear;

    if Size > 0 then
    begin
      FData.CopyFrom(Stream, Size);
      EncodeDecode(PChar(FData.Memory), Size);
    end;

    Stream.ReadBuffer(Size, SizeOf(Size));
    SetLength(FFilename, Size);
    if Size > 0 then
    begin
      Stream.ReadBuffer(FFilename[1], Size);
      EncodeDecode(@FFilename[1], Size);
    end;
  end;

begin
  Stream.ReadBuffer(Version, SizeOf(Version));

  if Version = VERSION_1 then
    ReadVersion1(Stream)
  else
    raise ElvkFormBinaryFile.Create('Invalid data format, unknown version tag');

  if not (csDesigning in ((Collection as TlvkFileItemCollection).GetOwner as TlvkFormBinaryFileCollection).ComponentState) then
  begin
    if FAutoExtract then
      AutoExtractFile;
  end;

  FData.Position := 0;
end;

procedure TlvkFileItem.SetFilename(const Value: string);
begin
  if Value = '' then
  begin
    FFilename := '';
    FData.Clear;
  end else begin
    if CompareText(ExtractFilename(Value), FFilename) <> 0 then
      LoadFile(Value);
  end;
end;

procedure TlvkFileItem.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    if Value <> '' then
      if Assigned((Collection as TlvkFileItemCollection).ItemsByName[Value]) then
        raise ElvkFormBinaryFile.Create('Another item by that name already exists');

    FName := Value;
  end;
end;

procedure TlvkFileItem.WriteData(Stream: TStream);
var
  Temp    : PChar;
  Size    : Cardinal;
  Version : Word;
begin
  Version := VERSION_1;
  Stream.WriteBuffer(Version, SizeOf(Version));

  Size := FData.Size;
  Stream.WriteBuffer(Size, SizeOf(Size));
  if Size > 0 then
  begin
    GetMem(Temp, Size);
    try
      Move(FData.Memory^, Temp^, Size);
      EncodeDecode(Temp, Size);
      Stream.WriteBuffer(Temp^, Size);
    finally
      FreeMem(Temp);
    end;
  end;

  Size := Length(FFilename);
  Stream.WriteBuffer(Size, SizeOf(Size));
  if Size > 0 then
  begin
    GetMem(Temp, Size);
    try
      Move(FFilename[1], Temp^, Size);
      EncodeDecode(Temp, Size);
      Stream.WriteBuffer(Temp^, Size);
    finally
      FreeMem(Temp);
    end;
  end;
end;

{ TlvkFileItemCollection }

function TlvkFileItemCollection.Add: TlvkFileItem;
begin
  Result := inherited Add as TlvkFileItem;
end;

constructor TlvkFileItemCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlvkFileItem);
end;

function TlvkFileItemCollection.FindItemID(const ID: Integer): TlvkFileItem;
begin
  Result := inherited FindItemID(ID) as TlvkFileItem;
end;

function TlvkFileItemCollection.GetItem(const Index: Integer): TlvkFileItem;
begin
  Result := inherited Items[Index] as TlvkFileItem;
end;

function TlvkFileItemCollection.GetItemsByName(
  const Name: string): TlvkFileItem;
var
  Index : Integer;
begin
  Result := nil;
  
  for Index := 0 to Count-1 do
  begin
    if CompareText(Items[Index].Name, Name) = 0 then
    begin
      Result := Items[Index];
      Break;
    end;
  end;
end;

function TlvkFileItemCollection.Insert(const Index: Integer): TlvkFileItem;
begin
  Result := inherited Insert(Index) as TlvkFileItem;
end;

procedure TlvkFileItemCollection.SetItem(const Index: Integer;
  const Value: TlvkFileItem);
begin
  inherited Items[Index] := Value;
end;

end.
