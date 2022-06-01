{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a simple name-associative collection for use
    with scripts.
}
unit lvkCollection;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCollection.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Variants,
  {$ENDIF}
  ComObj, ActiveX, Windows, SysUtils, Classes;

type
  { Description:
      This is the main interface supported by the TlvkCollection object.

      Notes:
       * The object supports the IEnumVariant interface, which means you can use
         For Each in VBScript to iterate through all the names stored in the
         collection. Check the demo for more information.
       * In Scripts, the collection supports syntax like Collection(x)
         where x is either the name of a element, or the index of a element,
         1-based. Both of which are both readable and writeable.
       * In Scripts, the collection supports syntax like Collection.X
         where X is the name of an element in the collection, which is
         both readable and writeable.
  }
  IlvkCollection = interface
    ['{62972B1B-3263-472C-8B4A-5B2D87D783CC}']

    { Description:
        This function clears the collection by removing all the elements in
        it.
    }
    procedure Clear;

    function GetValueOfName(const Name: string): OleVariant;
    procedure SetValueOfName(const Name: string; const Value: OleVariant);

    { Description:
        This property gives access to the values of the collection by the
        name of the values. If you set the value of a non-existing name, the
        name will be created on-the-fly. This means that it's real easy to
        just add new data to the collection as you go.
    }
    property ValueOfNames[const Name: string]: OleVariant read GetValueOfName write SetValueOfName; default;

    { Description:
        This function will return True if the collection contains the name
        specified, False if not.
    }
    function HasName(const Name: string): Boolean;

    { Description:
        This function will return the number of elements stored in the
        container.
    }
    function Count: Integer;

    function GetNames(const Index: Integer): string;

    { Description:
        This property gives access to the names stored in the collection. From
        a script, use a 1-based index, from Delphi used a 0-based index. This
        is done in accordance with normal rules for VBScript collections.
    }
    property Names[const Index: Integer]: string read GetNames;

    { Description:
        This method will delete the element with the given name.
    }
    procedure Delete(const Name: string); overload;

    { Description:
        This method will delete the given element. In Delphi, use a 0-based
        index, in scripts used a 1-based index. This is done in accordance
        with normal rules for VBScript collections.
    }
    procedure Delete(const Index: Integer); overload;

    // <COMBINE ValueOfIndex>
    function GetValueOfIndex(const Index: Integer): OleVariant;
    // <COMBINE ValueOfIndex>
    procedure SetValueOfIndex(const Index: Integer; const Value: OleVariant);

    { Description:
        You can use this property to get access to the values stored in the
        collection by index. From a script, use a 1-based index, from Delphi
        used a 0-based index. This is done in accordance with normal rules for
        VBScript collections.
    }
    property ValueOfIndex[const Index: Integer]: OleVariant read GetValueOfIndex write SetValueOfIndex;

    { Description:
        This method will simply set the value of the name in the
        collection, in exactly the same manner as Collection[Name] := Value;
      Parameters:
        Name - The name to set the value for.
        Value - The new value of the name.
    }
    procedure Add(const Name: string; const Value: OleVariant);

    { Description:
        This method will rename an item from the old name to the new name.
        Functionality-wise, it's the same as:

          c.Add(NewName, c.ValueOfNames[OldName]);
          c.Delete(OldName);
      Returns:
        True if the name existed.
      Parameters:
        OldName - The existing name of the item.
        NewName - The new name to store the value as.
    }
    function Rename(const OldName, NewName: string): Boolean;
  end;

{ Description:
    This function will create and return a new Collection instance. The
    collection is initially empty.
}
function CreateCollection: IlvkCollection;

implementation

uses
  lvkRTTIDispatch;

const
  DISPATCH_BASE   = 1000;

  DISPID_HASNAME    = 1;
  DISPID_NAMES      = 2;
  DISPID_COUNT      = 3;
  DISPID_DELETE     = 4;
  DISPID_ADD        = 5;
  DISPID_RENAME     = 6;

type
  PValue  = ^OleVariant;

const
  DispatchMethods : array[1..6] of string = (
    'HasName',
    'Names',
    'Count',
    'Delete',
    'Add',
    'Rename'
  );

type
  TlvkCollection = class(TInterfacedObject, IlvkCollection, IDispatch)
  private
    FMembers  : TStringList;

  protected
    // IlvkCollection interface
    procedure Clear; virtual;
    function GetValueOfName(const Name: string): OleVariant; virtual;
    procedure SetValueOfName(const Name: string; const Value: OleVariant); virtual;
    function HasName(const Name: string): Boolean; virtual;
    function Count: Integer;
    function GetNames(const Index: Integer): string;
    procedure Delete(const Name: string); overload;
    procedure Delete(const Index: Integer); overload;
    function GetValueOfIndex(const Index: Integer): OleVariant;
    procedure SetValueOfIndex(const Index: Integer; const Value: OleVariant);
    procedure Add(const Name: string; const Value: OleVariant);
    function Rename(const OldName, NewName: string): Boolean;

    // IDispatch interface
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

    // Internal
    function GetDispIDOfName(const Name: string): TDispID; virtual;
    function GetValueOfDispID(const DispID: TDispID): OleVariant; virtual;
    procedure SetValueOfDispID(const DispID: TDispID; const Value: OleVariant); virtual;
    procedure DeleteByIndex(const Index: Integer);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TlvkCollectionEnumerator = class(TInterfacedObject, IEnumVariant)
  private
    FCollection : IlvkCollection;
    FIndex      : Integer;

    function GetNext(out Value: OleVariant): Boolean;

  protected
    // IEnumUnknown interface
    function Next(celt: LongWord; var rgvar : OleVariant;
      out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

  public
    constructor Create(const Collection: IlvkCollection; const Index: Integer=0);
  end;

function CreateCollection: IlvkCollection;
begin
  Result := TlvkCollection.Create as IlvkCollection;
end;

{ TlvkCollection }

procedure TlvkCollection.Add(const Name: string; const Value: OleVariant);
begin
  SetValueOfName(Name, Value);
end;

procedure TlvkCollection.Clear;
var
  Value : PValue;
  Index : Integer;
begin
  for Index := 0 to FMembers.Count-1 do
  begin
    Value := PValue(FMembers.Objects[Index]);
    if Assigned(Value) then
      Dispose(Value);
  end;

  FMembers.Clear;
end;

function TlvkCollection.Count: Integer;
begin
  Result := FMembers.Count;
end;

constructor TlvkCollection.Create;
begin
  inherited;

  FMembers := TStringList.Create;
  FMembers.Sorted := True;
end;

procedure TlvkCollection.Delete(const Name: string);
var
  Index : Integer;
begin
  if FMembers.Find(Name, Index) then
    DeleteByIndex(Index);
end;

procedure TlvkCollection.Delete(const Index: Integer);
begin
  DeleteByIndex(Index);
end;

procedure TlvkCollection.DeleteByIndex(const Index: Integer);
var
  Value : PValue;
begin
  Assert((Index >= 0) and (Index < FMembers.Count));
  Value := PValue(FMembers.Objects[Index]);
  if Assigned(Value) then
    Dispose(Value);
  FMembers.Delete(Index);
end;

destructor TlvkCollection.Destroy;
begin
  Clear;
  FMembers.Free;

  inherited;
end;

function TlvkCollection.GetDispIDOfName(const Name: string): TDispID;
var
  Index : Integer;
begin
  if not FMembers.Find(Name, Index) then
    Index := FMembers.AddObject(Name, nil);
    
  Result := DISPATCH_BASE + Index
end;

function TlvkCollection.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
type
  PPChar  = ^PWideChar;
var
  Index   : Integer;
  pName   : PPChar;
  pDispID : ^Integer;
  ID      : Integer;

  function GetDispIDOfMethod(const Name: string): TDispID;
  var
    Index : Integer;
  begin
    Result := DISPID_UNKNOWN;
    
    for Index := Low(DispatchMethods) to High(DispatchMethods) do
      if CompareText(DispatchMethods[Index], Name) = 0 then
      begin
        Result := Index;
        Break;
      end;
  end;

begin
  try
    pName := Names;
    pDispID := DispIDs;
    Result := S_OK;
    for Index := 1 to NameCount do
    begin
      ID := GetDispIDOfMethod(pName^);
      if ID = DISPID_UNKNOWN then
        ID := GetDispIDOfName(pName^);
      pDispID^ := ID;

      Inc(pDispID, 4);
      Inc(pName, 4);
    end;
  except
    { TODO 2 -oLVK -cQA : Add more specific exception handling }
    Result := E_FAIL;
  end;
end;

function TlvkCollection.GetNames(const Index: Integer): string;
begin
  Result := FMembers[Index];
end;

function TlvkCollection.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
  Exit;
end;

function TlvkCollection.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TlvkCollection.GetValueOfDispID(
  const DispID: TDispID): OleVariant;
var
  ValuePtr    : PValue;
  Collection  : IDispatch;
begin
  Assert((DispID >= DISPATCH_BASE) and (DispID < DISPATCH_BASE + FMembers.Count));

  ValuePtr := PValue(FMembers.Objects[DispID-DISPATCH_BASE]);
  if Assigned(ValuePtr) then
    Result := ValuePtr^
  else begin
    // Create a new collection on the fly and return it
    Collection := TlvkCollection.Create as IDispatch;
    New(ValuePtr);
    ValuePtr^ := Collection;
    FMembers.Objects[DispID-DISPATCH_BASE] := TObject(ValuePtr);
    Result := Collection;
  end;
end;

function TlvkCollection.GetValueOfIndex(const Index: Integer): OleVariant;
begin
  Assert((Index >= 0) and (Index < FMembers.Count));
  Result := GetValueOfDispID(DISPATCH_BASE + Index)
end;

function TlvkCollection.GetValueOfName(const Name: string): OleVariant;
var
  DispID  : TDispID;
begin
  DispID := GetDispIDOfName(Name);
  Result := GetValueOfDispID(DispID);
end;

function TlvkCollection.HasName(const Name: string): Boolean;
begin
  Result := FMembers.IndexOf(Name) >= 0;
end;

function TlvkCollection.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  NewParams : array of Variant;
  ParamsPtr : PDispParams;
  Index     : Integer;
  Res       : OleVariant;

  function IsInteger(const V: OleVariant): Boolean;
  begin
    case TVarData(V).VType of
      varSmallInt,
      varInteger,
      {$IFDEF DELPHI6UP}
      varShortInt,
      varWord,
      varLongWord,
      varInt64,
      {$ENDIF}
      varByte:
        Result := True;
    else
      Result := False;
    end;
  end;

begin
  try
    ParamsPtr := @Params;
    SetLength(NewParams, ParamsPtr^.cArgs);
    for Index := 0 to ParamsPtr^.cArgs - 1 do
      NewParams[High(NewParams) - Index] := Variant(ParamsPtr^.rgvarg[Index]);
    Res := Unassigned;

    if DispID = 0 then
    begin
      // DispID 0 is reserved for syntax like Collection(Name)
      if ((Flags and DISPATCH_PROPERTYGET)<>0) or ((Flags and DISPATCH_METHOD)<>0) then
      begin
        if Length(NewParams) = 1 then
        begin
          if IsInteger(NewParams[0]) then
            Res := GetValueOfDispID(Integer(NewParams[0]) - 1 + DISPATCH_BASE)
          else
            Res := GetValueOfName(VariantOf(NewParams[0]));
          Result := S_OK;
        end else
          Result := DISP_E_BADPARAMCOUNT;
      end else if (Flags and DISPATCH_PROPERTYPUT) <> 0 then
      begin
        if Length(NewParams) = 1 then
        begin
          if IsInteger(NewParams[0]) then
            SetValueOfDispID(Integer(NewParams[0]) - 1 + DISPATCH_BASE, NewParams[1])
          else
            SetValueOfName(NewParams[0], NewParams[1]);
          Result := S_OK;
        end else
          Result := DISP_E_BADPARAMCOUNT;
      end else
        Result := E_NOTIMPL;
    end else begin
      if ((Flags and DISPATCH_PROPERTYGET)<>0) or ((Flags and DISPATCH_METHOD)<>0) then
      begin
        if DispID < DISPATCH_BASE then
        begin
          case DispID of
            DISPID_NEWENUM:
              begin
                // Used by FOR EACH constructs in VBScript, amongst other things
                Res := TlvkCollectionEnumerator.Create(Self) as IEnumVariant;
                Result := S_OK;
              end;

            DISPID_HASNAME:
              if Length(NewParams) = 1 then
              begin
                Res := HasName(NewParams[0]);
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;

            DISPID_NAMES:
              if Length(NewParams) = 1 then
              begin
                Res := GetNames(Integer(Integer(NewParams[0])) - 1);
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;

            DISPID_COUNT:
              if Length(NewParams) = 0 then
              begin
                Res := Count;
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;

            DISPID_DELETE:
              if Length(NewParams) = 1 then
              begin
                if IsInteger(NewParams[0]) then
                  DeleteByIndex(Integer(NewParams[0]) -1)
                else
                  Delete(string(NewParams[0]));
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;

            DISPID_ADD:
              if Length(NewParams) = 2 then
              begin
                Add(NewParams[0], NewParams[1]);
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;

            DISPID_RENAME:
              if Length(NewParams) = 2 then
              begin
                Rename(NewParams[0], NewParams[1]);
                Result := S_OK;
              end else
                Result := DISP_E_BADPARAMCOUNT;
          else
            Result := DISP_E_MEMBERNOTFOUND;
          end;
        end else if Length(NewParams) = 0 then
        begin
          Res := GetValueOfDispID(DispID);
          Result := S_OK;
        end else
          Result := DISP_E_BADPARAMCOUNT;
      end else if (Flags and DISPATCH_PROPERTYPUT) <> 0 then
      begin
        if Length(NewParams) = 1 then
        begin
          SetValueOfDispID(DispID, NewParams[0]);
          Result := S_OK;
        end else
          Result := DISP_E_BADPARAMCOUNT;
      end else
        Result := E_NOTIMPL;
    end;

    if Assigned(VarResult) and (Result = S_OK) then
      OleVariant(VarResult^) := Res;
  except
    { TODO 2 -oLVK -cQA : Add more specific exception handling }
    Result := E_FAIL;
  end;
end;

function TlvkCollection.Rename(const OldName, NewName: string): Boolean;
var
  Value : Variant;
begin
  if HasName(OldName) then
  begin
    if CompareText(OldName, NewName) <> 0 then
    begin
      Value := GetValueOfName(OldName);
      SetValueOfName(NewName, Value);
      Delete(OldName);
    end;

    Result := True;
  end else
    Result := False;
end;

procedure TlvkCollection.SetValueOfDispID(const DispID: TDispID;
  const Value: OleVariant);
var
  ValuePtr  : PValue;
begin
  Assert((DispID >= DISPATCH_BASE) and (DispID < DISPATCH_BASE + FMembers.Count));

  ValuePtr := PValue(FMembers.Objects[DispID-DISPATCH_BASE]);
  if not Assigned(ValuePtr) then
  begin
    New(ValuePtr);
    FMembers.Objects[DispID-DISPATCH_BASE] := TObject(ValuePtr);
  end;

  ValuePtr^ := Value
end;

procedure TlvkCollection.SetValueOfIndex(const Index: Integer;
  const Value: OleVariant);
begin
  Assert((Index >= 0) and (Index < FMembers.Count));
  SetValueOfDispID(DISPATCH_BASE + Index, Value)
end;

procedure TlvkCollection.SetValueOfName(const Name: string;
  const Value: OleVariant);
var
  DispID  : TDispID;
begin
  DispID := GetDispIDOfName(Name);
  SetValueOfDispID(DispID, Value);
end;

{ TlvkCollectionEnumerator }

function TlvkCollectionEnumerator.Clone(out Enum: IEnumVariant): HResult;
begin
  try
    Enum := TlvkCollectionEnumerator.Create(FCollection, FIndex) as IEnumVariant;
    Result := S_OK;
  except
    Result := E_FAIL;
  end;
end;

constructor TlvkCollectionEnumerator.Create(
  const Collection: IlvkCollection; const Index: Integer);
begin
  inherited Create;

  FCollection := Collection;
  FIndex := Index;
end;

function TlvkCollectionEnumerator.GetNext(out Value: OleVariant): Boolean;
begin
  if (FIndex >= 0) and (FIndex < FCollection.Count) then
  begin
    Value := WideString(FCollection.Names[FIndex]);
    Result := True;
    Inc(FIndex);
  end else
    Result := False;
end;

function TlvkCollectionEnumerator.Next(celt: LongWord; var rgvar: OleVariant; 
  out pceltFetched: LongWord): HResult;
begin
  Result := S_OK;
  if celt > 0 then
  begin
    if GetNext(rgvar) then
    begin
      if Assigned(@pceltFetched) then
        pceltFetched := 1
      else
        Result := S_OK;
    end else begin
      if Assigned(@pceltFetched) then
        pceltFetched := 0;
      Result := S_FALSE;
    end;
  end;
end;

function TlvkCollectionEnumerator.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TlvkCollectionEnumerator.Skip(celt: LongWord): HResult;
var
  elt   : OleVariant;
  Index : Integer;
begin
  Result := S_OK;
  for Index := 1 to celt do
  begin
    if not GetNext(elt) then
    begin
      Result := E_FAIL;
      Break;
    end;
  end;
end;

end.
