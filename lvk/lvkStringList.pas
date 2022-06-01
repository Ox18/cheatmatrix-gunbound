{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a basic design-time version of a TStringList
    class.
}
unit lvkStringList;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkStringList.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, lvkVersion;

const
  IDX_ONCHANGE    = 0;
  IDX_ONCHANGING  = 1;

type
  { Description:
      This is simply a design-time version of a TStringList. You can drop
      it on a form, enter text, set events. It does not introduce anything
      new compared to a stringlist you create in code.
  }
  TlvkStringList = class(TComponent)
  private
    FStrings  : TStrings;

    procedure SetStrings(const Value: TStrings);
    function GetEvent(const Index: Integer): TNotifyEvent;
    procedure SetEvent(const Index: Integer; const Value: TNotifyEvent);
    procedure SetPackageVersion(const Value: TPackageVersion);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        This property controls wether the stringlist will sort its contents
        or not.
    }
    property Sorted: Boolean read GetSorted write SetSorted;

    { Description:
        This property gives you access to the strings stored in the component.
    }
    property Strings: TStrings read FStrings write SetStrings;

    { Description:
        This event is fired whenever the contents of the stringlist component
        changes.
      See also:
        OnChanging
    }
    property OnChange: TNotifyEvent index IDX_ONCHANGE read GetEvent
      write SetEvent;

    { Description:
        This event is fired before any changes are applied to the strings
        stored in the component.
      See also:
        OnChange
    }
    property OnChanging: TNotifyEvent index IDX_ONCHANGING read GetEvent
      write SetEvent;
  end;

implementation

uses
  SysUtils;

{ TlvkStringList }

constructor TlvkStringList.Create(AOwner: TComponent);
begin
  inherited;

  FStrings := TStringList.Create;
end;

destructor TlvkStringList.Destroy;
begin
  FreeAndNil(FStrings);

  inherited;
end;

function TlvkStringList.GetEvent(const Index: Integer): TNotifyEvent;
begin
  case Index of
    IDX_ONCHANGE:
      Result := TStringList(FStrings).OnChange;

    IDX_ONCHANGING:
      Result := TStringList(FStrings).OnChanging;

  else
    raise Exception.Create('Internal error in TlvkStringList.GetEvent');
  end;
end;

function TlvkStringList.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkStringList.GetSorted: Boolean;
begin
  Result := TStringList(FStrings).Sorted;
end;

procedure TlvkStringList.SetEvent(const Index: Integer;
  const Value: TNotifyEvent);
begin
  case Index of
    IDX_ONCHANGE:
      TStringList(FStrings).OnChange := Value;

    IDX_ONCHANGING:
      TStringList(FStrings).OnChanging := Value;

  else
    raise Exception.Create('Internal error in TlvkStringList.SetEvent');
  end;
end;

procedure TlvkStringList.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkStringList.SetSorted(const Value: Boolean);
begin
  TStringList(FStrings).Sorted := Value;
end;

procedure TlvkStringList.SetStrings(const Value: TStrings);
begin
  if Assigned(Value) then
    FStrings.Assign(Value)
  else
    FStrings.Clear;
end;

end.
