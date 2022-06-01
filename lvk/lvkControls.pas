{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains basis classes for controls, which all implement the
    PackageVersion property.
}
unit lvkControls;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkControls.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkVersion,
  Controls;

type
  { Description:
      This is the base class used for TGraphicControl descendant components
      in my component collection. It just adds the PackageVersion property.
  }
  TlvkGraphicControl = class(TGraphicControl, IPackageVersion)
  private
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

  { Description:
      This is the base class used for TCustomControl descendant components
      in my component collection. It just adds the PackageVersion property.
  }
  TlvkCustomControl = class(TCustomControl)
  private
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

implementation

{ TlvkGraphicControl }

function TlvkGraphicControl.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkGraphicControl.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

{ TlvkCustomControl }

function TlvkCustomControl.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomControl.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

end.
