{ Description:
    This unit contains guard object functionality as seen in the Delphi-Jedi
    vcl system.

    A guard object is a way to reduce the number of try/finally levels
    needed in a method, function or procedure and trade it with a interface
    variable. For an example, look at the Guard functions.

    Slightly modified.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
}
unit lvkGuardObjects;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkGuardObjects.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

type
{ Description:
    This interface is the main guard interface. See one of the guard
    functions for example.

    A Guard object will automatically take care of destroying whatever it
    is guarding when the guard object is closed. Thus, by trading in a level
    of try/finally statements to destroy a object or free a block of memory,
    you can use a guard object. Since a guard object is represented by a
    interface variable, Delphi will inject code in the final parts of a
    procedure or function to automatically release the interface. Releasing
    the interface will automatically destroy the guard object and thus anything
    that the guard object is guarding.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
}
  IGuard = interface
    ['{DA1332E7-81BC-450F-869C-BCD032179B89}']

    { Description:
        Will the object or memory block that the object guards be destroyed
        when the guard object is destroyed. This function returns True
        or False according to that information. Note that all guard
        objects will pr. default destroy the object or memory block
        it guards, unless you Unlink it before the guard object is
        destroyed.
      See also:
        Unlink
    }
    function WillBeDestroyed: Boolean;

    { Description:
        If you for some reason wish to keep the object or memory block
        after the guard object has been destroyed, you can choose to
        unlink it. When you unlink it, the guard object will not do
        anything when it is destroyed.
      See also:
        WillBeDestroyed
    }
    procedure Unlink;
  end;

{ Description:
    This interface is supported by the class that takes care of guarding
    TObject-descendants. You can typecaste the IGuard interface to this
    interface in order to get hold of the object it guards.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
}
  IObjectGuard = interface(IGuard)
    ['{B36D3E99-CBD3-43CA-A363-E4ED2C6FDA84}']

    { Description:
        This method will return the object instance that the guard object
        is guarding.
    }
    function GetObject: TObject;
  end;

{ Description:
    This interface is supported by the class that takes care of guarding
    memory blocks. You can typecaste the IGuard interface to this
    interface in order to get hold of the memory block it guards.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
}
  IMemoryGuard = interface(IGuard)
    ['{36535ACD-FC7A-4B7B-AC33-522D63D935F3}']

    { Description:
        This method will return a pointer to the memory block that the
        guard object is guarding.
    }
    function GetMemory: Pointer;
  end;

{ Description:
    This function creates a guard object to guard a TObject-descendant class
    instance. When the guard object is destroyed, the object will be
    destroyed.
  Parameters:
    Obj - The object instance to guard, cannot be nil.
    GuardObject - Output parameter that will hold the guard object interface
      when the function returns.
  Returns:
    The object instance to guard. Use AS to typecast this back to the
    object type you want to use it as.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
  Example:
<CODE>
<B>procedure</B> TForm1.TestGuard;
<B>var</B>
  Form2   : TForm2;
  gForm2  : IGuard;
<B>begin</B>
  Form2 := Guard(TForm2.Create(Application), gForm2) <B>as</B> TForm2;
  Form2.ShowModal;
  // Notice no Form2.Free here
  // When gForm2 goes out of scope, it will be released, and thus the
  // object it guards, Form2, will be destroyed as well.
<B>end</B>;
</CODE>
}
function Guard(const Obj: TObject; out GuardObject: IGuard): TObject; overload;

{ Description:
    This function creates a guard object to guard a block of memory. When the
    guard object is destroyed, the memory-block will be freed.
  Parameters:
    P - The memory block to guard, cannot be nil.
    GuardObject - Output parameter that will hold the guard object interface
      when the function returns.
  Returns:
    The memory block to guard.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
  Example:
<CODE>
<B>procedure</B> TForm1.TestGuard;
<B>var</B>
  P   : PChar;
  gP  : IGuard;
<B>begin</B>
  GetMem(P, 32768);
  Guard(P, gP);
  StrCopy(P, 'Hello there');
  Caption := P;
  // Notice no FreeMem(P) here.
  // When gForm2 goes out of scope, it will be released, and thus the
  // memory block it guards, P, will be freed as well.
<B>end</B>;
</CODE>
}
function Guard(const P: Pointer; out GuardObject: IGuard): Pointer; overload;

{ Description:
    This function allocates a block of memory and creates a guard object to
    guard it. When the guard object is destroyed, the memory-block will be
    freed.
  Parameters:
    Size - How many bytes to allocate for the memory block.
    GuardObject - Output parameter that will hold the guard object interface
      when the function returns.
  Returns:
    The memory block that was allocated and is now guarded.
  See also:
    Guard@TObject@IGuard,
    Guard@Pointer@IGuard,
    GuardAlloc@Integer@IGuard
  Example:
<CODE>
<B>procedure</B> TForm1.TestGuard;
<B>var</B>
  P   : PChar;
  gP  : IGuard;
<B>begin</B>
  P := GuardAlloc(32768, gP);
  StrCopy(P, 'Hello there');
  Caption := P;
  // Notice no FreeMem(P) here.
  // When gForm2 goes out of scope, it will be released, and thus the
  // memory block it guards, P, will be freed as well.
<B>end</B>;
</CODE>
}
function GuardAlloc(const Size: Integer; out GuardObject: IGuard): Pointer; overload;

implementation

type
  TGuardTObject = class(TInterfacedObject, IGuard, IObjectGuard)
  private
    FObject     : TObject;
    FOwnsObject : Boolean;

  protected
    // IGuard interface
    procedure Unlink;
    function WillBeDestroyed: Boolean;

    // IObjectGuard interface
    function GetObject: TObject;

  public
    constructor Create(const Obj: TObject);
    destructor Destroy; override;
  end;

  TGuardMemory = class(TInterfacedObject, IGuard, IMemoryGuard)
  private
    FMemory     : Pointer;
    FOwnsMemory : Boolean;

  protected
    // IGuard interface
    procedure Unlink;
    function WillBeDestroyed: Boolean;

    // IMemoryGuard interface
    function GetMemory: Pointer;

  public
    constructor Create(const P: Pointer);
    destructor Destroy; override;
  end;


function Guard(const Obj: TObject; out GuardObject: IGuard): TObject;
begin
  Assert(Assigned(Obj));

  GuardObject := TGuardTObject.Create(Obj) as IGuard;
  Result := Obj;
end;

function Guard(const P: Pointer; out GuardObject: IGuard): Pointer;
begin
  Assert(Assigned(P));

  GuardObject := TGuardMemory.Create(P) as IGuard;
  Result := P;
end;

function GuardAlloc(const Size: Integer; out GuardObject: IGuard): Pointer; overload;
var
  P : Pointer;
begin
  GetMem(P, Size);
  Result := Guard(P, GuardObject);
end;

{ TGuardTObject }

constructor TGuardTObject.Create(const Obj: TObject);
begin
  inherited Create;

  FObject := Obj;
  FOwnsObject := True;
end;

destructor TGuardTObject.Destroy;
begin
  if FOwnsObject then
    FObject.Free;

  inherited;
end;

function TGuardTObject.GetObject: TObject;
begin
  Result := FObject;
end;

procedure TGuardTObject.Unlink;
begin
  FOwnsObject := False;
end;

function TGuardTObject.WillBeDestroyed: Boolean;
begin
  Result := FOwnsObject;
end;

{ TGuardMemory }

constructor TGuardMemory.Create(const P: Pointer);
begin
  inherited Create;

  FMemory := P;
  FOwnsMemory := True;
end;

destructor TGuardMemory.Destroy;
begin
  if FOwnsMemory then
    FreeMem(FMemory);

  inherited;
end;

function TGuardMemory.GetMemory: Pointer;
begin
  Result := FMemory;
end;

procedure TGuardMemory.Unlink;
begin
  FOwnsMemory := False;
end;

function TGuardMemory.WillBeDestroyed: Boolean;
begin
  Result := FOwnsMemory;
end;

end.
 