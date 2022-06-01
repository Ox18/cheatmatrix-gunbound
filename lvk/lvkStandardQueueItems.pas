{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains standard data items for use with the
    TlvkThreadCommunicationsQueue component. You can of course use these
    items anywhere else you want to store regular data in a interface
    variable.
}
unit lvkStandardQueueItems;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkStandardQueueItems.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkVersion;
  
type
{ Description:
    The IStringItem item contains a single string value. The programmer
    has read/write access to the value.
  See also:
    NewStringItem
}
  IStringItem = interface
    ['{F82A6CEB-47FB-436C-B1F6-DE5E6091303B}']

    { Description:
        Returns the value stored in the item.
      See also:
        SetValue, Value
    }
    function GetValue: string;

    { Description:
        Changes the value stored in the item.
      See also:
        GetValue, Value
    }
    procedure SetValue(const Value: string);

    { Description:
        This property gives read and write access to the value stored in the
        item.
      See also:
        GetValue, SetValue
    }
    property Value: string read GetValue write SetValue;
  end;

{ Description:
    The IIntegerItem item contains a single integer value. The programmer
    has read/write access to the value.
}
  IIntegerItem = interface
    ['{15DA6954-BDA3-4B45-B57F-C4D17F10E766}']

    { Description:
        Returns the value stored in the item.
      See also:
        SetValue, Value
    }
    function GetValue: Integer;

    { Description:
        Changes the value stored in the item.
      See also:
        GetValue, Value
    }
    procedure SetValue(const Value: Integer);

    { Description:
        This property gives read and write access to the value stored in the
        item.
      See also:
        GetValue, SetValue
    }
    property Value: Integer read GetValue write SetValue;
  end;

{ Description:
    The IBooleanItem item contains a single boolean value. The programmer
    has read/write access to the value.
}
  IBooleanItem = interface
    ['{2D68EB95-AA25-4C00-8706-0868A49EE734}']

    { Description:
        Returns the value stored in the item.
      See also:
        SetValue, Value
    }
    function GetValue: Boolean;

    { Description:
        Changes the value stored in the item.
      See also:
        GetValue, Value
    }
    procedure SetValue(const Value: Boolean);

    { Description:
        This property gives read and write access to the value stored in the
        item.
      See also:
        GetValue, SetValue
    }
    property Value: Boolean read GetValue write SetValue;
  end;

{ Description:
    The IDataItem item contains a block of memory. The programmer has
    read/write access to the value but can not point the item to a
    different block of memory or change the size of the block.
}
  IDataItem = interface
    ['{8923D630-33CF-4DF0-8410-C84F5E2122F9}']

    { Description:
        Returns the size of the block of memory in bytes.
      See also:
        GetSize, Size
    }
    function GetSize: Integer;

    // <ALIAS GetSize>
    property Size: Integer read GetSize;

    { Description:
        Returns a PChar pointer to the first byte in the block of memory.
      See also:
        GetData, Data
    }
    function GetData: PChar;

    // <ALIAS GetData>
    property Data: PChar read GetData;
  end;

{ Description:
    The IVariantItem item contains a single variant value. The programmer
    has read/write access to the value.
}
  IVariantItem = interface
    ['{A5E78FDC-370E-4F57-BB06-AEBBE282A371}']

    { Description:
        Returns the value stored in the item.
      See also:
        SetValue, Value
    }
    function GetValue: Variant;

    { Description:
        Changes the value stored in the item.
      See also:
        GetValue, Value
    }
    procedure SetValue(const Value: Variant);

    { Description:
        This property gives read and write access to the value stored in the
        item.
      See also:
        GetValue, SetValue
    }
    property Value: Variant read GetValue write SetValue;
  end;

{ Description:
    The IObjectItem contains a single object instance. The programmer has
    read/write access to the object but can not point the item to a
    different object instance.
}
  IObjectItem = interface
    ['{1391B6FB-F782-4ACF-924A-ED731B05AAFF}']

    { Description:
        This returns a reference to the object instance.
      See also:
        GetInstance, Instance
    }
    function GetInstance: TObject;

    // <ALIAS GetInstance>
    property Instance: TObject read GetInstance;

    { Description:
        This method returns the object instance, and detaches it from the
        IObjectItem interface. Detaching the object means that when the
        IObjectItem interface is destroyed, the object will still live. The
        job of destroying the object is transfered to the caller of the
        DetachInstance method
      See also:
        GetInstance
    }
    function DetachInstance: TObject;
  end;

{ Description:
    The IArrayItem item contains an array of other IUnknown items. The
    programmer has read/write access to the items but can not
    add or remove items from the array in any way.
}
  IArrayItem = interface
    ['{07868C0B-75F2-4DD4-A9EA-5D90EDAC8230}']

    { Description:
        This returns the number of items in the array.
      See also:
        GetCount, Count
    }
    function GetCount: Integer;

    // <COMBINE GetCount>
    property Count: Integer read GetCount;

    { Description:
        This returns a given item in the array.
      Parameters:
        Index - The index of the item to return. The first item has an index of
          0, and the last has an index of Count-1.
      See also:
        GetItems, Items
    }
    function GetItems(const Index: Integer): IUnknown;

    // <COMBINE GetItems@Integer>
    property Items[const Index: Integer]: IUnknown read GetItems; default;
  end;

  { Description:
      This enumerated data type is used in calls to NewDataItem to decide
      how to store the data specified. The possible values are:
        * rtReference - A simple reference to the data is stored. The data must
          not be freed or deallocated or in any other way made unavailable
          until after the item has been destroyed.
        * rtOwnedData - A simple reference to the data is stored and
          the item is also set to be the new owner of the data. This means
          that the item will do a FreeMem on the data when the item is
          destroyed. As for the rtReference value, the data must not
          be made unavailable by other means outside the item.
        * rtMakeCopy - A new, separate, copy of the data is made that is
          owned by the item. This is the safest way to use the data item as
          the data it referred to when it was created can be made unavailable
          independant of the item.
    See also:
      NewDataItem
  }
  TReferenceType = (rtReference, rtOwnedData, rtMakeCopy);

{ Description:
    This function returns a new string item for the string value given.
  See also:
    IStringItem
}
function NewStringItem(const Value: string): IStringItem; overload;

{ Description:
    This function returns a new string item. The value of the string item
    corresponds to the same value you would get by calling SysUtils.Format with
    the same parameters.
  See also:
    IStringItem
}
function NewStringItem(const Fmt: string; const Args: array of const): IStringItem; overload;

{ Description:
    This function returns a new integer item for the integer value given.
  See also:
    IIntegerItem
}
function NewIntegerItem(const Value: Integer): IIntegerItem;

{ Description:
    This function returns a new boolean item for the boolean value given.
  See also:
    IBooleanItem
}
function NewBooleanItem(const Value: Boolean): IBooleanItem;

{ Description:
    This function returns a new data item for the data given.
  See also:
    IDataItem
}
function NewDataItem(const Data: PChar; const Size: Integer; const ReferenceType: TReferenceType=rtMakeCopy): IDataItem;

{ Description:
    This function returns a new variant item for the variant value given.
  See also:
    IVariantItem
}
function NewVariantItem(const Value: Variant): IVariantItem;

{ Description:
    This function returns a new object item for the object instance given.
  See also:
    IObjectItem
}
function NewObjectItem(const Instance: TObject; const OwnsInstance: Boolean=True): IObjectItem;

{ Description:
    This function returns a array item for the items given.
  See also:
    IArrayItem
}
function NewArrayItem(const Items: array of IUnknown): IArrayItem;

implementation

uses
{$IFDEF HAS_VARIANTS_UNIT}
  Variants,
{$ENDIF}
  Windows, ActiveX, lvkPersistence, Classes, SysUtils;

const
  TStringItem_CLSID   : TGUID = '{C44ED633-79C0-11D5-B238-0004761A6377}';
  TIntegerItem_CLSID  : TGUID = '{C44ED634-79C0-11D5-B238-0004761A6377}';
  TBooleanItem_CLSID  : TGUID = '{C44ED635-79C0-11D5-B238-0004761A6377}';
  TDataItem_CLSID     : TGUID = '{C44ED636-79C0-11D5-B238-0004761A6377}';
  TObjectItem_CLSID   : TGUID = '{C44ED638-79C0-11D5-B238-0004761A6377}';
  TArrayItem_CLSID    : TGUID = '{C44ED639-79C0-11D5-B238-0004761A6377}';

type
  TStringItem = class(TInterfacedObject, IStringItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FValue  : string;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IStringItem interface
    function GetValue: string;
    procedure SetValue(const Value: string);

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Value: string);
  end;

  TStringItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewStringItem(const Value: string): IStringItem;
begin
  Result := TStringItem.Create(Value) as IStringItem;
end;

function NewStringItem(const Fmt: string; const Args: array of const): IStringItem;
begin
  Result := TStringItem.Create(Format(Fmt, Args)) as IStringItem;
end;

type
  TIntegerItem = class(TInterfacedObject, IIntegerItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FValue  : Integer;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IIntegerItem interface
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Value: Integer);
  end;

  TIntegerItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewIntegerItem(const Value: Integer): IIntegerItem;
begin
  Result := TIntegerItem.Create(Value) as IIntegerItem;
end;

type
  TBooleanItem = class(TInterfacedObject, IBooleanItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FValue  : Boolean;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IBooleanItem interface
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Value: Boolean);
  end;

  TBooleanItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewBooleanItem(const Value: Boolean): IBooleanItem;
begin
  Result := TBooleanItem.Create(Value) as IBooleanItem;
end;

type
  TDataItem = class(TInterfacedObject, IDataItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FData : PChar;
    FSize : Integer;
    FOwns : Boolean;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IDataItem interface
    function GetSize: Integer;
    function GetData: PChar;

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Data: PChar; const Size: Integer; const ReferenceType: TReferenceType);
    destructor Destroy; override;
  end;

  TDataItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewDataItem(const Data: PChar; const Size: Integer; const ReferenceType: TReferenceType): IDataItem;
begin
  Result := TDataItem.Create(Data, Size, ReferenceType);
end;

type
  TVariantItem = class(TInterfacedObject, IVariantItem, IPackageVersion)
  private
    FValue  : Variant;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IVariantItem interface
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);

  public
    constructor Create(const Value: Variant);
  end;

function NewVariantItem(const Value: Variant): IVariantItem;
begin
  Result := TVariantItem.Create(Value) as IVariantItem;
end;

type
  TObjectItem = class(TInterfacedObject, IObjectItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FInstance : TObject;
    FOwns     : Boolean;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IObjectItem interface
    function GetInstance: TObject;
    function DetachInstance: TObject;

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Instance: TObject; const OwnsInstance: Boolean=True);
    constructor CreateEmpty;
    destructor Destroy; override;
  end;

  TObjectItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewObjectItem(const Instance: TObject; const OwnsInstance: Boolean=True): IObjectItem;
begin
  Result := TObjectItem.Create(Instance, OwnsInstance) as IObjectItem;
end;

type
  TArrayItem = class(TInterfacedObject, IArrayItem, IPersist, IPersistStream,
    IPackageVersion)
  private
    FItems  : array of IUnknown;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IArrayItem interface
    function GetCount: Integer;
    function GetItems(const Index: Integer): IUnknown;

    // IPersist interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    // IPersistStream interface
    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

  public
    constructor Create(const Items: array of IUnknown);
    constructor CreateEmpty;
  end;

  TArrayItemFactory = class(TInterfacedObject, IlvkPersistenceClassFactory)
  protected
    // IlvkPersistenceClassFactory interface
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

function NewArrayItem(const Items: array of IUnknown): IArrayItem;
begin
  Result := TArrayItem.Create(Items);
end;

{ TStringItem }

constructor TStringItem.Create(const Value: string);
begin
  inherited Create;

  FValue := Value;
end;

function TStringItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TStringItem_CLSID;
  Result := S_OK;
end;

function TStringItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TStringItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStringItem.GetValue: string;
begin
  Result := FValue;
end;

function TStringItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TStringItem.Load(const stm: IStream): HResult;
var
  Temp      : LongInt;
  BytesRead : LongInt;
begin
  Result := stm.Read(@Temp, 4, @BytesRead);
  if Result <> 0 then
    Exit;
  if BytesRead < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;
  SetLength(FValue, Temp);
  if Temp > 0 then
  begin
    Result := stm.Read(@FValue[1], Temp, @BytesRead);
    if Result <> 0 then
      Exit;
    if BytesRead < Temp then
    begin
      Result := E_FAIL;
      Exit;
    end;
  end;

  Result := S_OK;
end;

function TStringItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  Temp          : LongInt;
  BytesWritten  : LongInt;
begin
  Temp := Length(FValue);
  Result := stm.Write(@Temp, 4, @BytesWritten);
  if Result <> 0 then
    Exit;
  if BytesWritten < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;
  if Temp > 0 then
  begin
    Result := stm.Write(@FValue[1], Temp, @BytesWritten);
    if Result <> 0 then
      Exit;
    if BytesWritten < Temp then
    begin
      Result := E_FAIL;
      Exit;
    end;
  end;

  Result := S_OK;
end;

procedure TStringItem.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TStringItemFactory }

function TStringItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
begin
  Instance := TStringItem.Create('') as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TStringItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TStringItem_CLSID;
  Result := S_OK;
end;

{ TIntegerItem }

constructor TIntegerItem.Create(const Value: Integer);
begin
  inherited Create;

  FValue := Value;
end;

function TIntegerItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TIntegerItem_CLSID;
  Result := S_OK;
end;

function TIntegerItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TIntegerItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TIntegerItem.GetValue: Integer;
begin
  Result := FValue;
end;

function TIntegerItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TIntegerItem.Load(const stm: IStream): HResult;
var
  BytesRead : LongInt;
begin
  Result := stm.Read(@FValue, 4, @BytesRead);
  if (Result = S_OK) and (BytesRead < 4) then
    Result := E_FAIL;
end;

function TIntegerItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  BytesWritten  : LongInt;
begin
  Result := stm.Write(@FValue, 4, @BytesWritten);
  if (Result = S_OK) and (BytesWritten < 4) then
    Result := E_FAIL;
end;

procedure TIntegerItem.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

{ TIntegerItemFactory }

function TIntegerItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
begin
  Instance := TIntegerItem.Create(0) as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TIntegerItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TIntegerItem_CLSID;
  Result := S_OK;
end;

{ TBooleanItem }

constructor TBooleanItem.Create(const Value: Boolean);
begin
  inherited Create;

  FValue := Value;
end;

function TBooleanItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TBooleanItem_CLSID;
  Result := S_OK;
end;

function TBooleanItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TBooleanItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TBooleanItem.GetValue: Boolean;
begin
  Result := FValue;
end;

function TBooleanItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TBooleanItem.Load(const stm: IStream): HResult;
var
  BytesRead : LongInt;
begin
  Result := stm.Read(@FValue, 1, @BytesRead);
  if (Result = S_OK) and (BytesRead < 1) then
    Result := E_FAIL;
end;

function TBooleanItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  BytesWritten  : LongInt;
begin
  Result := stm.Write(@FValue, 1, @BytesWritten);
  if (Result = S_OK) and (BytesWritten < 1) then
    Result := E_FAIL;
end;

procedure TBooleanItem.SetValue(const Value: Boolean);
begin
  FValue := Value;
end;

{ TBooleanItemFactory }

function TBooleanItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
begin
  Instance := TBooleanItem.Create(False) as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TBooleanItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TBooleanItem_CLSID;
  Result := S_OK;
end;

{ TVariantItem }

constructor TVariantItem.Create(const Value: Variant);
begin
  inherited Create;

  FValue := Value;
end;

function TVariantItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TVariantItem.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TVariantItem.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TDataItem }

constructor TDataItem.Create(const Data: PChar; const Size: Integer;
  const ReferenceType: TReferenceType);
begin
  inherited Create;

  case ReferenceType of
    rtReference : begin
      FData := Data;
      FSize := Size;
      FOwns := False;
    end;
    rtOwnedData : begin
      FData := Data;
      FSize := Size;
      FOwns := True;
    end;
    rtMakeCopy : begin
      FSize := Size;
      if Size > 0 then
      begin
        GetMem(FData, Size);
        Move(Data^, FData^, FSize);
      end;
      FOwns := True;
    end;
  end;
end;

destructor TDataItem.Destroy;
begin
  if FOwns then
    FreeMem(FData);

  inherited;
end;

function TDataItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TDataItem_CLSID;
  Result := S_OK;
end;

function TDataItem.GetData: PChar;
begin
  Result := FData;
end;

function TDataItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TDataItem.GetSize: Integer;
begin
  Result := FSize;
end;

function TDataItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDataItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TDataItem.Load(const stm: IStream): HResult;
var
  Temp      : LongInt;
  BytesRead : LongInt;
begin
  Result := stm.Read(@Temp, 4, @BytesRead);
  if Result <> 0 then
    Exit;

  if BytesRead < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if FOwns then
    FreeMem(FData);
  FSize := Temp;
  GetMem(FData, Temp);
  FOwns := True;

  if Temp > 0 then
  begin
    Result := stm.Read(FData, Temp, @BytesRead);
    if Result <> 0 then
      Exit;
    if BytesRead < Temp then
    begin
      Result := E_FAIL;
      Exit;
    end;
  end;

  Result := S_OK;
end;

function TDataItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  BytesWritten  : LongInt;
begin
  Result := stm.Write(@FSize, 4, @BytesWritten);
  if Result <> 0 then
    Exit;
  if BytesWritten < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;
  if FSize > 0 then
  begin
    Result := stm.Write(FData, FSize, @BytesWritten);
    if Result <> 0 then
      Exit;
    if BytesWritten < FSize then
    begin
      Result := E_FAIL;
      Exit;
    end;
  end;

  Result := S_OK;
end;

{ TDataItemFactory }

function TDataItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
var
  Dummy : Byte;
begin
  Instance := TDataItem.Create(@Dummy, 1, rtReference) as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TDataItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TDataItem_CLSID;
  Result := S_OK;
end;

{ TObjectItem }

constructor TObjectItem.Create(const Instance: TObject;
  const OwnsInstance: Boolean);
begin
  inherited Create;

  FInstance := Instance;
  FOwns := OwnsInstance;
end;

constructor TObjectItem.CreateEmpty;
begin
  inherited Create;
  
  FInstance := nil;
  FOwns := False;
end;

destructor TObjectItem.Destroy;
begin
  if FOwns then
    FInstance.Free;

  inherited;
end;

function TObjectItem.DetachInstance: TObject;
begin
  Result := FInstance;
  FOwns := False;
end;

function TObjectItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TObjectItem_CLSID;
  Result := S_OK;
end;

function TObjectItem.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TObjectItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TObjectItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TObjectItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TObjectItem.Load(const stm: IStream): HResult;
var
  TempStream    : TMemoryStream;
  Temp          : LongInt;
  BytesRead     : LongInt;
  Comp          : TComponent;
begin
  TempStream := TMemoryStream.Create;
  try
    Result := stm.Read(@Temp, 4, @BytesRead);
    if Result <> S_OK then
      Exit;
    if BytesRead < 4 then
    begin
      Result := E_FAIL;
      Exit;
    end;

    TempStream.SetSize(Temp);
    Result := stm.Read(TempStream.Memory, Temp, @BytesRead);
    if Result <> S_OK then
      Exit;
    if BytesRead < Temp then
    begin
      Result := E_FAIL;
      Exit;
    end;

    TempStream.Position := 0;
    Comp := TempStream.ReadComponent(nil);

    FInstance := Comp;
    FOwns := True;

    Result := S_OK;
  finally
    TempStream.Free;
  end;
end;

function TObjectItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  TempStream    : TMemoryStream;
  Temp          : LongInt;
  BytesWritten  : LongInt;
begin
  if not (FInstance is TComponent) then
    raise Exception.Create('Can only save descendants from TComponent');

  TempStream := TMemoryStream.Create;
  try
    TempStream.WriteComponent(FInstance as TComponent);
    TempStream.Position := 0;

    Temp := TempStream.Size;
    Result := stm.Write(@Temp, 4, @BytesWritten);
    if Result <> S_OK then
      Exit;
    if BytesWritten < 4 then
    begin
      Result := E_FAIL;
      Exit;
    end;

    Result := stm.Write(TempStream.Memory, Temp, @BytesWritten);
    if Result <> S_OK then
      Exit;
    if BytesWritten < Temp then
    begin
      Result := E_FAIL;
      Exit;
    end;

    Result := S_OK;
  finally
    TempStream.Free;
  end;
end;

{ TObjectItemFactory }

function TObjectItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
begin
  Instance := TObjectItem.CreateEmpty as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TObjectItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TObjectItem_CLSID;
  Result := S_OK;
end;

{ TArrayItem }

constructor TArrayItem.Create(const Items: array of IUnknown);
var
  Index : Integer;
begin
  inherited Create;

  SetLength(FItems, Length(Items));
  for Index := 0 to Length(Items)-1 do
    FItems[Low(FItems)+Index] := Items[Low(Items)+Index];
end;

constructor TArrayItem.CreateEmpty;
begin
  inherited Create;
  
  SetLength(FItems, 0);
end;

function TArrayItem.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TArrayItem_CLSID;
  Result := S_OK;
end;

function TArrayItem.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TArrayItem.GetItems(const Index: Integer): IUnknown;
begin
  Result := FItems[Index];
end;

function TArrayItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TArrayItem.GetSizeMax(out cbSize: Largeint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TArrayItem.IsDirty: HResult;
begin
  Result := S_FALSE;
end;

function TArrayItem.Load(const stm: IStream): HResult;
var
  BytesRead : LongInt;
  Temp      : LongInt;
  Index     : Integer;
  Instance  : IUnknown;
begin
  Result := stm.Read(@Temp, 4, @BytesRead);
  if Result <> S_OK then
    Exit;
  if BytesRead < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;

  SetLength(FItems, Temp);
  for Index := 0 to Temp-1 do
  begin
    Result := LoadFromStream(stm, Instance);
    if Result <> S_OK then
      Exit;
    FItems[Index] := Instance;
  end;

  Result := S_OK;
end;

function TArrayItem.Save(const stm: IStream; fClearDirty: BOOL): HResult;
var
  BytesWritten  : LongInt;
  Temp          : LongInt;
  Index         : Integer;
begin
  Temp := Length(FItems);
  Result := stm.Write(@Temp, 4, @BytesWritten);
  if Result <> S_OK then
    Exit;
  if BytesWritten < 4 then
  begin
    Result := E_FAIL;
    Exit;
  end;

  for Index := 0 to Temp-1 do
  begin
    Result := SaveToStream(stm, FItems[Index]);
    if Result <> S_OK then
      Exit;
  end;

  Result := S_OK;
end;

{ TArrayItemFactory }

function TArrayItemFactory.CreateFromStream(const Stream: IStream;
  out Instance: IUnknown): HResult;
begin
  Instance := TArrayItem.CreateEmpty as IUnknown;
  Result := (Instance as IPersistStream).Load(Stream);
end;

function TArrayItemFactory.GetClassID(out classID: TCLSID): HResult;
begin
  classID := TArrayItem_CLSID;
  Result := S_OK;
end;

initialization
  lvkPersistence.RegisterClass(TStringItemFactory.Create as IlvkPersistenceClassFactory);
  lvkPersistence.RegisterClass(TIntegerItemFactory.Create as IlvkPersistenceClassFactory);
  lvkPersistence.RegisterClass(TBooleanItemFactory.Create as IlvkPersistenceClassFactory);
  lvkPersistence.RegisterClass(TDataItemFactory.Create as IlvkPersistenceClassFactory);
  lvkPersistence.RegisterClass(TObjectItemFactory.Create as IlvkPersistenceClassFactory);
  lvkPersistence.RegisterClass(TArrayItemFactory.Create as IlvkPersistenceClassFactory);
end.
