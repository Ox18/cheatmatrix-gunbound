{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains basis classes for components, which all implement the
    PackageVersion property.
}
unit lvkComponents;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkComponents.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkVersion,
  Classes;

type
  { Description:
      This is the base class for all non-visual components in the LVK
      Components package. It provides the package version property and
      property editor.
  }
  TlvkComponent = class(TComponent, IPackageVersion)
  private
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored false;
  end;

implementation

{ TlvkComponent }

function TlvkComponent.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkComponent.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

end.
 