{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the current LVK Components version number, as well
    as an interface that classes can implement.
}
unit lvkVersion;

// $Author: Lasse V. Karlsen $
// $Revision: 18 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkVersion.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

type
  TPackageVersion = type string;

// Automated build tagging
// const
//  PackageVersion  : TPackageVersion = '1.0.10.144';
{$I LVKVERSION.INC}

type
  { Description:
      Classes that are used through interface references can implement this
      interface so that the code can check which version of the LVK Components
      they're communicating with.
  }
  IPackageVersion = interface
    ['{6A617025-3E42-4DAB-8822-BB3B4386BC52}']

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;    

implementation

end.
 