{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code that gives IDispatch access to the properties and
    methods of a normal Delphi object.
  See also:
    lvkActiveScriptObject.pas
}
unit lvkRTTIDispatch;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkRTTIDispatch.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  TypInfo, ActiveX, ComObj, Windows, Classes;

type
{ Description:
    The TDispatchMethod defines how all published, virtual, methods of an
    object must look if they are to be used with TlvkRTTIDispatch.
  Parameters:
    Parameters  - The parameters passed to the method. Can be changed and
      returned back to client code if passed by reference.
    FunctionResult - Set to Unassigned for procedures, return a value if a
      function.
  Returns:
    A S_xxx value telling the client code wether the call was successful or not.
    Since IDispatch in these objects doesn't support early binding, the client
    code has no way of knowing if the number of parameters or their type is
    correct. Possible return values are:

      * DISP_E_TYPEMISMATCH
      * DISP_E_UNKNOWNNAME
      * DISP_E_NONAMEDARGS
      * DISP_E_BADVARTYPE
      * DISP_E_EXCEPTION
      * DISP_E_OVERFLOW
      * DISP_E_BADINDEX
      * DISP_E_BADPARAMCOUNT
      * DISP_E_PARAMNOTOPTIONAL
      * DISP_E_NOTACOLLECTION
}
  TDispatchMethod = function(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT of object;

{ Description:
    This class encapsulates a normal Delphi object and gives IDispatch access
    to its properties and methods. For an example, take a look at
    TlvkActiveScript.AddObject.

    If you publish methods, note that the method has to follow the syntax of
    TDispatchMethod, otherwise you'll get stack problems.
  See also:
    TlvkActiveScript.AddObject, TlvkRTTIBaseDispatch
}
  TlvkRTTIDispatch = class(TInterfacedObject, IDispatch)
  private
    FObject                   : TObject;
    FOwnsObject               : Boolean;

    FProperties               : TStrings;
    FMethods                  : TStrings;
    FConstants                : TStrings;

    FSupportDefaultEnumerator : Boolean;

  protected
    { Description:
        This internal method makes a copy of all the property names of the
        encapsulated object for easier reference later on.
      See also:
        InitializeMethods
    }
    procedure InitializeProperties;

    { Description:
        This internal method makes a copy of all the method names of the
        encapsulated object for easier reference later on.
      See also:
        InitializeProperties
    }
    procedure InitializeMethods;

    // IDispatch interface
    { Description:
        This internal method retrieves the number of typeinfo interfaces
        available from the object. For the moment, this is none.
      Parameters:
        Count - Output parameter to store the number of interfaces in.
      Return value:
        S_OK to signal success.
      See also:
        GetTypeInfo
    }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;

    { Description:
        This internal method would be used to retrieve a typeinfo interface to
        use for retrieving type information about the object. No such
        interfaces are supported at the moment.
      Parameters:
        Index     - Index of interface to retrieve.
        LocaleID  - Locale id to retrieve interface for.
        TypeInfo  - Output parameter to store the interface reference in.
      Return value:
        E_NOTIMPL to signal that this method is not implemented.
      See also:
        GetTypeInfo
    }
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;

    { Description:
        This method takes a list of names from the caller and returns a
        corresponding list of name id's. The id's correspond directly to the
        position in the internal list of properties or methods.
      Parameters:
        IID       - Reserved for future use; set to IID_NULL.
        Names     - Passed-in array of names to be mapped.
        NameCount - Count of the names to be mapped.
        LocaleID  - Locale context in which to interpret the names.
        DispIDs   - Caller-allocated array, each element of which contains an
          identifier that corresponds to one of the names passed in the Names
          array. The first element represents the member name. The subsequent
          elements represent each of the member’s parameters.
      Return value:
        DISP_E_UNKNOWNNAME to signal that one or more names was unknown,<P>
        S_OK to signal success.
      See also:
        Invoke
    }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;

    { Description:
        This internal method is called to get or set the value of a property
        or call a method.
      Parameters:
        DispID    - Identifies the member. Use GetIDsOfNames or the object’s
          documentation to obtain the dispatch identifier.
        IID       - Reserved for future use; set to IID_NULL.
        LocaleID  - The locale context in which to interpret parameters.
        Flags     - Flags describing the context of the Invoke call.
        Params    - Pointer to a structure that contains an array of
          parameters, an array of parameter DISPIDs for named parameters, and
          counts for the number of elements in the arrays.
        VarResult - Pointer to the location where the result is to be stored,
          or NULL if the caller expects no result.
        ExcepInfo - Pointer to a structure that contains exception information.
        ArgErr    - The index within rgvarg of the first parameter that has an
          error.
      Return value:
        E_FAIL to signal that it was unable to do the correct job,<P>
        S_OK to signal success.
      See also:
        GetIDsOfNames
    }
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall; 

    { Description:
        These three methods can be overriden in descendant classes to provide
        support for "Object(Index)" syntax directly without going through a
        property, like "Object.Items(Index)".

        If you do this:

          x = Object(10)

        then DefaultGet will be called, and if that returns E_NOTIMPL,
          DefaultCall will be called.

        If you do this:

          Object(10) = x

        then DefaultSet will be called.
      See also:
        TDispatchMethod
    }
    function DefaultGet(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    // <COMBINE DefaultGet>
    function DefaultSet(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    // <COMBINE DefaultGet>
    function DefaultCall(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;

    { Description:
        This function is called upon by the default enumerator to get an object.
        The first time the method is called, Index is 0, the next time it's 1
        and then it will go up by 1 for each call.

        The method should return the item in the Value out parameter, and return
        True. When an invalid index is passed (ie. no more items), the
        method should return False.

        The default implementation always returns False and descendant classes
        must implement this method to support the default enumerator.
      Parameters:
        Index - The index of the value to return, 0 is the first, 1 is the
          second, etc.
        Value - Out parameter that must contain the value when the method
          returns.
      Returns:
        True if Index was valid and a value is being returned. False if no
          more items.
      See also:
        CreateDefaultEnumerator
    }
    function EnumGetNext(const Index: Integer; out Value: OleVariant): Boolean; virtual;

    { Description:
        This function creates and returns an enumerator object for this
        object. By default this function returns nil, which means the object
        does not support enumeration. Descendant classes can override this
        and either call and return the result of CreateDefaultEnumerator, or
        implement their own type. You can optionally pass True in the
        last parameter to the constructor to support the default enumerator.
      See also:
        CreateDefaultEnumerator
    }
    function CreateEnumerator: IEnumVariant; virtual;

    { Description:
        This function creates a default enumerator for this object. The
        default enumerator uses the EnumGetNext method of the object to get
        the items of the enumerator.
      See also:
        CreateEnumerator
    }
    function CreateDefaultEnumerator: IEnumVariant; virtual;

  public
    { Description:
        Creates a new instance of the TlvkRTTIDispatch class, preparing it for
        use.
      Parameters:
        Obj         - The object to encapsulate.
        OwnsObject  - If this parameter is True, when the TlvkRTTIDispatch
          object instance is destroyed, the object referred to in Obj will also
          be destroyed.
      Return value:
        -
      See also:
        Destroy
    }
    constructor Create(const Obj: TObject; const OwnsObject: Boolean=False;
      const SupportDefaultEnumerator: Boolean=False);

    { Description:
        Destroys the instance of the TlvkRTTIDispatch class, possibly also
        destroying the encapsulated object, depending on the parameters passed
        to the constructor when the TlvkRTTIDispatch object was first created.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        You can call DefineConstant to make a named constant value available
        to the script code.
      Parameters:
        Name - The name of the constant to define.
        Value - The value of the constant to define.
      See also:
        UndefineConstant@string, ClearConstants
    }
    procedure DefineConstant(const Name: string; const Value: OleVariant);

    { Description:
        UndefineConstant will remove a constant definition from the object.
        The script engine caches information in some cases so even if you
        remove the constant after it has been read from the script once it
        might not revert to the Unassigned state.
      Parameters:
        Name - The name of the constant to remove.
      See also:
        DefineConstant@string@OleVariant, ClearConstants
    }
    procedure UndefineConstant(const Name: string);

    { Description:
        ClearConstants will remove all constants from the object. The same
        rule that were described for UndefineConstant applies to ClearConstants.
      See also:
        DefineConstant@string@OleVariant, UndefineConstant@string
    }
    procedure ClearConstants;
  end;

  {$M+}
  { Description:
      This class can be used as the base class for classes you want to pass
      to scripts or other procedures that takes IDispatch interfaces.

      Any published properties and methods (see the description of
      TlvkRTTIDispatch for more information) will be automatically published
      through the IDispatch interface.
    See also:
      TlvkRTTIDispatch
  }
  TlvkRTTIBaseDispatch = class(TlvkRTTIDispatch)
  public
    constructor Create(const SupportDefaultEnumerator: Boolean=False);
  end;
  {$M-}

  TlvkRTTIDispatchEnumerator = class(TInterfacedObject, IEnumVariant)
  private
    FObject : TlvkRTTIDispatch;
    FIndex  : Integer;

    function GetNext(out Value: OleVariant): Boolean;

  protected
    // IEnumUnknown interface
    function Next(celt: LongWord; var rgvar : OleVariant;
      out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

  public
    constructor Create(const Obj: TlvkRTTIDispatch; const Index: Integer=0);
  end;

{ Description:
    This function returns the variant contained in V. If V contains a reference
    to a variant, this function will dereference it and return the value of
    the variant.
  Parameters:
    V - The variant to return the real value of.
}
function VariantOf(const V: OleVariant): OleVariant; overload;
function VariantOf(const V: TVariantArg): OleVariant; overload;

procedure UpdateVariant(var V: OleVariant; const NewValue: OleVariant);

implementation

uses
{$IFDEF DELPHI6UP}
  Variants,
{$ENDIF}
  SysUtils;

const
  DISPID_DEFAULT  = 0;
  PROPERTY_BASE   = 10000;
  PROPERTY_END    = PROPERTY_BASE + 9999;

  METHOD_BASE     = PROPERTY_END + 1;
  METHOD_END      = METHOD_BASE + 9999;

  CONSTANT_BASE   = METHOD_END + 1;
  CONSTANT_END    = CONSTANT_BASE + 9999;

function VariantOf(const V: OleVariant): OleVariant;
begin
  if (TVarData(V).VType and varByRef) <> 0 then
  begin
    Result := OleVariant(TVarData(V).VPointer^);
  end else
    Result := V;
end;

function VariantOf(const V: TVariantArg): OleVariant;
begin
  case V.vt of
    VT_UI1:                  Result := V.bVal;
    VT_I2:                   Result := V.iVal;
    VT_I4:                   Result := V.lVal;
    VT_R4:                   Result := V.fltVal;
    VT_R8:                   Result := V.dblVal;
    VT_BOOL:                 Result := V.vbool;
    VT_ERROR:                Result := V.scode;
    VT_CY:                   Result := V.cyVal;
    VT_DATE:                 Result := V.date;
    VT_BSTR:                 Result := WideString(V.bstrVal);
    VT_UNKNOWN:              Result := IUnknown(V.punkVal);
    VT_DISPATCH:             Result := IDispatch(V.dispVal);
    VT_ARRAY:                Result := Unassigned; // V.parray: PSafeArray);
    VT_BYREF or VT_UI1:      Result := V.pbVal^;
    VT_BYREF or VT_I2:       Result := V.piVal^;
    VT_BYREF or VT_I4:       Result := V.plVal^;
    VT_BYREF or VT_R4:       Result := V.pfltVal^;
    VT_BYREF or VT_R8:       Result := V.pdblVal^;
    VT_BYREF or VT_BOOL:     Result := V.pbool^;
    VT_BYREF or VT_ERROR:    Result := V.pscode^;
    VT_BYREF or VT_CY:       Result := V.pcyVal^;
    VT_BYREF or VT_DATE:     Result := V.pdate^;
    VT_BYREF or VT_BSTR:     Result := V.pbstrVal^;
    VT_BYREF or VT_UNKNOWN:  Result := V.punkVal^;
    VT_BYREF or VT_DISPATCH: Result := V.pdispVal^;
    VT_BYREF or VT_ARRAY:    Result := Unassigned; // pparray: ^PSafeArray);
    VT_BYREF or VT_VARIANT:  Result := V.pvarVal^;
    VT_BYREF:                Result := Unassigned; // byRef: Pointer);
    VT_I1:                   Result := V.cVal;
    VT_UI2:                  Result := V.uiVal;
    {$IFDEF DELPHI6UP}
    VT_UI4:                  Result := V.ulVal;
    VT_UINT:                 Result := V.uintVal;
    VT_BYREF or VT_UINT:     Result := V.puintVal^;
    {$ELSE}
    VT_UI4:                  Result := 0.0 + V.ulVal;
    VT_UINT:                 Result := 0.0 + V.uintVal;
    VT_BYREF or VT_UINT:     Result := 0.0 + V.puintVal^;
    {$ENDIF}
    VT_INT:                  Result := V.intVal;
    VT_BYREF or VT_DECIMAL:  Result := Unassigned; // pdecVal^;
    VT_BYREF or VT_I1:       Result := V.pcVal^;
    VT_BYREF or VT_UI2:      Result := V.puiVal^;
    VT_BYREF or VT_UI4:      Result := V.pulVal^;
    VT_BYREF or VT_INT:      Result := V.pintVal^;
  end;
end;

procedure UpdateVariant(var V: OleVariant; const NewValue: OleVariant);
begin
  if (TVarData(V).VType and varByRef) <> 0 then
    OleVariant(TVarData(V).VPointer^) := NewValue;
end;

{ TlvkRTTIDispatch }

procedure TlvkRTTIDispatch.ClearConstants;
var
  Constant  : POleVariant;
  Index     : Integer;
begin
  for Index := 0 to FConstants.Count-1 do
  begin
    Constant := POleVariant(FConstants.Objects[Index]);
    Dispose(Constant);
  end;
  FConstants.Clear;
end;

constructor TlvkRTTIDispatch.Create(const Obj: TObject;
  const OwnsObject, SupportDefaultEnumerator: Boolean);
begin
  inherited Create;
  FObject := Obj;
  FOwnsObject := OwnsObject;
  FSupportDefaultEnumerator := SupportDefaultEnumerator;

  FMethods := TStringList.Create;
  TStringList(FMethods).Sorted := True;
  FProperties := TStringList.Create;
  TStringList(FProperties).Sorted := True;
  FConstants := TStringList.Create;
  TStringList(FConstants).Sorted := True;

  InitializeProperties;
  InitializeMethods;
end;

function TlvkRTTIDispatch.CreateDefaultEnumerator: IEnumVariant;
begin
  Result := TlvkRTTIDispatchEnumerator.Create(Self);
end;

function TlvkRTTIDispatch.CreateEnumerator: IEnumVariant;
begin
  if FSupportDefaultEnumerator then
    Result := CreateDefaultEnumerator
  else
    Result := nil;
end;

function TlvkRTTIDispatch.DefaultCall(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TlvkRTTIDispatch.DefaultGet(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TlvkRTTIDispatch.DefaultSet(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  Result := E_NOTIMPL;
end;

procedure TlvkRTTIDispatch.DefineConstant(const Name: string;
  const Value: OleVariant);
var
  Index     : Integer;
  Constant  : POleVariant;
begin
  Assert(Name <> '');

  Index := FConstants.IndexOf(Name);
  if Index >= 0 then
    Constant := POleVariant(FConstants.Objects[Index])
  else begin
    New(Constant);
    FConstants.AddObject(Name, TObject(Constant));
  end;

  Constant^ := Value;
end;

destructor TlvkRTTIDispatch.Destroy;
begin
  if FOwnsObject then
    FObject.Free;
  FMethods.Free;
  FProperties.Free;
  ClearConstants;
  FConstants.Free;

  inherited;
end;

function TlvkRTTIDispatch.EnumGetNext(const Index: Integer;
  out Value: OleVariant): Boolean;
begin
  // Descendant classes must override this method
  Result := False;
  Value := Unassigned;
end;

function TlvkRTTIDispatch.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
type
  PPChar  = ^PWideChar;
var
  i, j    : Integer;
  pName   : PPChar;
  pDispID : ^Integer;
  ID      : Integer;
begin
  pName := Names;
  pDispID := DispIDs;
  Result := S_OK;
  for i := 1 to NameCount do
  begin
    ID := DISPID_UNKNOWN;
    j := FProperties.IndexOf(pName^);
    if j >= 0 then
      ID := PROPERTY_BASE + j
    else begin
      j := FMethods.IndexOf(pName^);
      if j >= 0 then
        ID := METHOD_BASE + j
      else begin
        j := FConstants.IndexOf(pName^);
        if j >= 0 then
          ID := CONSTANT_BASE + j;
      end;
    end;

    if ID = DISPID_UNKNOWN then
      Result := DISP_E_UNKNOWNNAME;

    pDispID^ := ID;

    Inc(pDispID, 4);
    Inc(pName, 4);
  end;
end;

function TlvkRTTIDispatch.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkRTTIDispatch.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

procedure TlvkRTTIDispatch.InitializeMethods;
type
  TVMT = packed record
    Count : SmallInt;
    // [... methods ...]
  end;
var
  vmt     : ^TVMT;
  Name    : PShortString;
  Index   : Integer;

  function GetVMT(Obj: TObject): Pointer;
  asm
    mov   eax, [Obj]
    mov   eax, [eax].vmtMethodTable
  end;

begin
  vmt := GetVMT(FObject);
  if Assigned(vmt) then
  begin
    Name := Pointer(PChar(vmt)+8);

    for Index := 1 to vmt^.Count do
    begin
      if FMethods.IndexOf(Name^) < 0 then
        FMethods.Add(Name^);
      Name := Pointer(PChar(Name)+Length(Name^)+7);
    end;
  end;
end;

procedure TlvkRTTIDispatch.InitializeProperties;
var
  i         : Integer;
  PropList  : PPropList;
  Count     : Integer;
begin
  New(PropList);
  try
    Count := GetPropList(FObject.ClassInfo, [
      tkInteger, tkChar, tkEnumeration, tkFloat,
      tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
      tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ], PropList);
    for i := 0 to Count-1 do
      FProperties.Add(PropList[i]^.Name);
  finally
    Dispose(PropList);
  end;
end;

function TlvkRTTIDispatch.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  NewParams   : array of Variant;
  Res         : Variant;
  ParamsPtr   : PDispParams;
  i           : Integer;
  Method      : TDispatchMethod;
  PropInfo    : PPropInfo;
  Handled     : Boolean;
  Enumerator  : IEnumVariant;

  procedure AdjustByRef;
  var
    i : Integer;
  begin
    for i := 0 to ParamsPtr^.cArgs - 1 do
      UpdateVariant(OleVariant(ParamsPtr^.rgvarg[i]),
        NewParams[High(NewParams) - i]);
  end;

begin
  ParamsPtr := @Params;
  SetLength(NewParams, ParamsPtr^.cArgs);
  for i := 0 to ParamsPtr^.cArgs - 1 do
    NewParams[High(NewParams) - i] := VariantOf(ParamsPtr^.rgvarg[i]);

  Handled := False;
  Result := DISP_E_MEMBERNOTFOUND;

  // Check for "Read property" requests
  if (Flags and DISPATCH_PROPERTYGET) <> 0 then
  begin
    case DispID of
      DISPID_DEFAULT:
        begin
          Result := DefaultGet(NewParams, Res);
          if (Result = E_NOTIMPL) then
            Result := DefaultCall(NewParams, Res);
          AdjustByRef;
          Handled := (Result <> E_NOTIMPL);
        end;

      PROPERTY_BASE..PROPERTY_END:
        begin
          if Length(NewParams) > 0 then
            Result := DISP_E_BADPARAMCOUNT
          else begin
            PropInfo := GetPropInfo(FObject, FProperties[DispID - PROPERTY_BASE], []);
            Assert(Assigned(PropInfo));
            if Assigned(PropInfo^.GetProc) then
            begin
              Res := GetPropValue(FObject, FProperties[DispID - PROPERTY_BASE], False);
              Result := S_OK;
              Handled := True;
            end;
          end;
        end;

      CONSTANT_BASE..CONSTANT_END:
        begin
          if Length(NewParams) > 0 then
            Result := DISP_E_BADPARAMCOUNT
          else begin
            Res := POleVariant(FConstants.Objects[DispID-CONSTANT_BASE])^;
            Result := S_OK;
          end;
          Handled := True;
        end;
    end;
  end;

  // Check for "Write property" requests
  if (not Handled) and ((Flags and DISPATCH_PROPERTYPUT) <> 0) then
  begin
    case DispID of
      DISPID_DEFAULT:
        begin
          Result := DefaultSet(NewParams, Res);
          AdjustByRef;
          Handled := (Result <> E_NOTIMPL);
        end;

      PROPERTY_BASE..PROPERTY_END:
        begin
          if Length(NewParams) <> 1 then
            Result := DISP_E_BADPARAMCOUNT
          else begin
            PropInfo := GetPropInfo(FObject, FProperties[DispID - PROPERTY_BASE], []);
            Assert(Assigned(PropInfo));
            if Assigned(PropInfo^.SetProc) then
            begin
              SetPropValue(FObject, FProperties[DispID - PROPERTY_BASE], NewParams[0]);
              Result := S_OK;
              Handled := True;
            end;

            Res := 0;
          end;
        end;
    end;
  end;

  // Check for "Execute method" requests
  if (not Handled) and ((Flags and DISPATCH_METHOD) <> 0) then
  begin
    case DispID of
      DISPID_NEWENUM:
        begin
          Enumerator := CreateEnumerator;
          if Assigned(Enumerator) then
          begin
            Result := S_OK;
            Handled := True;
            Res := Enumerator;
          end;
        end;

      DISPID_DEFAULT:
        begin
          Result := DefaultCall(NewParams, Res);
          if (Result = E_NOTIMPL) then
            Result := DefaultGet(NewParams, Res);
          AdjustByRef;
          Handled := (Result <> E_NOTIMPL);
        end;

      METHOD_BASE..METHOD_END:
        begin
          TMethod(Method).Data := FObject;
          TMethod(Method).Code := FObject.MethodAddress(FMethods[DispID-METHOD_BASE]);

          Result := Method(NewParams, Res);
          AdjustByRef;
          Handled := True;
        end;
    end;
  end;

  if not Handled then
    Result := DISP_E_MEMBERNOTFOUND;

  if Assigned(VarResult) and (Result = S_OK) then
    OleVariant(VarResult^) := Res;
end;

procedure TlvkRTTIDispatch.UndefineConstant(const Name: string);
var
  Index     : Integer;
  Constant  : POleVariant;
begin
  Index := FConstants.IndexOf(Name);
  if Index >= 0 then
  begin
    Constant := POleVariant(FConstants.Objects[Index]);
    Dispose(Constant);
    FConstants.Delete(Index);
  end;
end;

{ TlvkRTTIBaseDispatch }

constructor TlvkRTTIBaseDispatch.Create(const SupportDefaultEnumerator: Boolean);
begin
  inherited Create(Self, False, SupportDefaultEnumerator);
end;

{ TlvkRTTIDispatchEnumerator }

function TlvkRTTIDispatchEnumerator.Clone(out Enum: IEnumVariant): HResult;
begin
  try
    Enum := TlvkRTTIDispatchEnumerator.Create(FObject, FIndex) as IEnumVariant;
    Result := S_OK;
  except
    Result := E_FAIL;
  end;
end;

constructor TlvkRTTIDispatchEnumerator.Create(const Obj: TlvkRTTIDispatch;
  const Index: Integer);
begin
  inherited Create;

  FObject := Obj;
  FIndex := Index;
end;

function TlvkRTTIDispatchEnumerator.GetNext(
  out Value: OleVariant): Boolean;
begin
  Result := FObject.EnumGetNext(FIndex, Value);
  if Result then
    Inc(FIndex);
end;

function TlvkRTTIDispatchEnumerator.Next(celt: LongWord;
  var rgvar: OleVariant; out pceltFetched: LongWord): HResult;
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

function TlvkRTTIDispatchEnumerator.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TlvkRTTIDispatchEnumerator.Skip(celt: LongWord): HResult;
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

