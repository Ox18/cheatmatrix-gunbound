{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the IlvkCleaner interface and the object it wraps.
}
unit lvkCleaner;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCleaner.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes;

type
  { Description:
      This interface wraps a reference counted object that can be used to
      automatically free a number of associated objects when it is destroyed.
      Thus, you can create an instance of the cleaner object as a local
      object in a procedure, add a number of object instances to the cleaner
      object and when the variable that refers to the cleaner object goes out
      of scope, all the objects will be destroyed.
    Parameters:
      -
    See also:
      -
  }
  IlvkCleaner = interface
    ['{7237DF01-C119-4305-B3E4-651FFA024173}']

    { Description:
        This method adds another object to the list of objects to destroy
        when the cleaner object is destroyed.
      See also:
        Remove@TObject
    }
    procedure Add(const Obj: TObject); overload;

    { Description:
        If you want to take over the destruction of the object again, call
        this method to remove it from the list that the cleaner object
        maintains.
      See also:
        Add@TObject
    }
    procedure Remove(const Obj: TObject); overload;
  end;

{ Description:
    This function returns a new cleaner object for use.
}
function NewCleaner: IlvkCleaner;

implementation

type
  TCleaner = class(TInterfacedObject, IlvkCleaner)
  private
    FObjects  : IInterfaceList;

  protected
    // IlvkCleaner interface
    procedure Add(const Obj: TObject); overload;
    procedure Remove(const Obj: TObject); overload;

  public
    constructor Create;
  end;

  ICleanItem = interface
    ['{0FEA01E4-1173-41E4-9CF2-4ABDB46DB746}']

    procedure Detach;
  end;

  IObjectCleaner = interface
    ['{1BC0EA59-8965-4C85-A5E5-491DC260F008}']

    function GetObject: TObject;
  end;

  TObjectCleaner = class(TInterfacedObject, ICleanItem, IObjectCleaner)
  private
    FObject : TObject;

  protected
    // ICleanItem interface
    procedure Detach;

    // IObjectCleaner interface
    function GetObject: TObject;

  public
    constructor Create(const Obj: TObject);
    destructor Destroy; override;
  end;

function NewCleaner: IlvkCleaner;
begin
  Result := TCleaner.Create;
end;

{ TCleaner }

procedure TCleaner.Add(const Obj: TObject);
begin
  FObjects.Add(TObjectCleaner.Create(Obj));
end;

constructor TCleaner.Create;
begin
  inherited;

  FObjects := TInterfaceList.Create;
end;

procedure TCleaner.Remove(const Obj: TObject);
var
  Index   : Integer;
  Cleaner : IObjectCleaner;
begin
  for Index := 0 to FObjects.Count-1 do
    if Supports(FObjects[Index], IObjectCleaner, Cleaner) then
      if Cleaner.GetObject = Obj then
      begin
        ICleanItem(FObjects[Index]).Detach;
        FObjects.Delete(Index);
        Break;
      end;
end;

{ TObjectCleaner }

constructor TObjectCleaner.Create(const Obj: TObject);
begin
  inherited Create;

  FObject := Obj;
end;

destructor TObjectCleaner.Destroy;
begin
  FObject.Free;

  inherited;
end;

procedure TObjectCleaner.Detach;
begin
  FObject := nil;
end;

function TObjectCleaner.GetObject: TObject;
begin
  Result := FObject;
end;

end.
