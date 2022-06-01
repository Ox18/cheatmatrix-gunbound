{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a guard object for dataset components.
  See also:
    lvkGuardObjects
}
unit lvkDBGuardObjects;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDBGuardObjects.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  DB, lvkGuardObjects;

type
  { Description:
      This interface is used for the TDataSet guard object.
    See also:
      IGuard
  }
  IOpenDataSetGuard = interface(IGuard)
    ['{E0C4CBDC-6F39-4823-92BA-C5ABB2E6E562}']

    { Description:
        This method retrieves a copy of the TDataSet component that the object
        guards.
    }
    function GetDataSet: TDataSet;
  end;

{ Description:
    This function creates an instance of the TDataSet guard object and returns
    that instance for the program to store.
  Parameters:
    DataSet - The dataset instance to guard.
    GuardObject - The guard object that guards the dataset. Store this object
      in a local variable of the code calling this function.
  Example:
  Returns:
    The dataset instance.
}
function GuardOpenDataSet(const DataSet: TDataSet; out GuardObject: IGuard): TDataSet; overload;

implementation

type
  TOpenDataSetGuard = class(TInterfacedObject, IGuard, IOpenDataSetGuard)
  private
    FDataSet      : TDataSet;
    FCloseDataSet : Boolean;

  protected
    // IGuard interface
    procedure Unlink;
    function WillBeDestroyed: Boolean;

    // IOpenDataSetGuard interface
    function GetDataSet: TDataSet;

  public
    constructor Create(const DataSet: TDataSet);
    destructor Destroy; override;
  end;

function GuardOpenDataSet(const DataSet: TDataSet; out GuardObject: IGuard): TDataSet; overload;
begin
  GuardObject := TOpenDataSetGuard.Create(DataSet) as IGuard;
  Result := DataSet;

  Result.Open;
end;

{ TOpenDataSetGuard }

constructor TOpenDataSetGuard.Create(const DataSet: TDataSet);
begin
  inherited Create;

  FDataSet := DataSet;
  FCloseDataSet := True;
end;

destructor TOpenDataSetGuard.Destroy;
begin
  if FCloseDataSet then
    FDataSet.Close;
    
  inherited;
end;

function TOpenDataSetGuard.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TOpenDataSetGuard.Unlink;
begin
  FCloseDataSet := False;
end;

function TOpenDataSetGuard.WillBeDestroyed: Boolean;
begin
  Result := False;
end;

end.
 