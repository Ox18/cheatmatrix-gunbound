{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a storage component that saves the information to a
    registry key.
}
unit lvkRegistryStateStorage;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkRegistryStateStorage.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Registry, lvkState, lvkVersionInformation;

type
  { Description:
      This data type specifies the root key of the registry to place the
      data in. The following values are available:

        * rrCurrentUser - Beneath the HKEY_CURRENT_USER registry root. State
          information will only be used for the currently logged on user.
        * rrLocalMachine - Beneath the HKEY_LOCAL_MACHINE registry root. State
          information will be used for all users on this machine.
  }
  TlvkRegistryRoot = (rrCurrentUser, rrLocalMachine);

  { Description:
      This component handles saving state information to a registry key.
  }
  TlvkRegistryStateStorage = class(TlvkCustomStateStorage)
  private
    FRoot     : TlvkRegistryRoot;
    FKey      : string;
    FRegistry : TRegistry;

    function UseKey: string;

  protected
    function DoReadString(const SectionName, ID: String;
      const Default: String): String; override;
    procedure DoWriteString(const SectionName, ID: String; const Value: String);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // <ALIAS TlvkCustomStateStorage.BeginJob@Boolean>
    procedure BeginJob(const WriteMode: Boolean); override;
    // <ALIAS TlvkCustomStateStorage.DeleteValue@string@string>
    procedure DeleteValue(const SectionName, ID: string); override;
    // <ALIAS TlvkCustomStateStorage.EndJob>
    procedure EndJob; override;

  published
    { Description:
        This property specifies the root key to use for storing the data in.
        See the TlvkRegistryRoot data type for a description of the
        available values.
    }
    property Root: TlvkRegistryRoot read FRoot write FRoot;

    { Description:
        This property holds the name of the specific key in the registry to
        save the information in. The key will be created if it does not exist.

        You can use the \%EXENAME\% macro to specify the name of the exe file,
        without an extension.

        You can use the \%COMPANY\% macro to specify the company name, as given
        in the version information of the resulting exe file. Make sure you
        provide version information though, otherwise an exception will be
        raised.
    }
    property Key: string read FKey write FKey;
  end;

  ElvkRegistryStateStorage = class(ElvkStateStorage);

implementation

uses
  Windows, Forms;

{ TlvkRegistryStateStorage }

procedure TlvkRegistryStateStorage.BeginJob(const WriteMode: Boolean);
const
  Roots : array[TlvkRegistryRoot] of HKEY = (
    HKEY_CURRENT_USER,
    HKEY_LOCAL_MACHINE
  );
begin
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := Roots[FRoot];
  if not FRegistry.OpenKey(StringReplace(UseKey, '.', '\', [rfReplaceAll]), True) then
  try
    raise ElvkRegistryStateStorage.Create('Unable to open registry key');
  finally
    FRegistry.CloseKey;
  end;
end;

constructor TlvkRegistryStateStorage.Create(AOwner: TComponent);
begin
  inherited;

  FRoot := rrCurrentUser;
  FKey := 'Software\%EXENAME%\Settings';
end;

procedure TlvkRegistryStateStorage.DeleteValue(const SectionName,
  ID: string);
begin
  if FRegistry.OpenKey(StringReplace(UseKey + '\' + SectionName, '.', '\',
    [rfReplaceAll]), False) then
  try
    if FRegistry.ValueExists(ID) then
      FRegistry.DeleteValue(ID);
  finally
    FRegistry.CloseKey;
  end;
end;

destructor TlvkRegistryStateStorage.Destroy;
begin
  EndJob;

  inherited;
end;

function TlvkRegistryStateStorage.DoReadString(const SectionName, ID,
  Default: String): String;
begin
  if FRegistry.OpenKey(StringReplace(UseKey + '\' + SectionName, '.', '\',
    [rfReplaceAll]), False) then
  try
    if FRegistry.ValueExists(ID) then
      Result := SafeDecode(FRegistry.ReadString(ID))
    else
      Result := Default;
  finally
    FRegistry.CloseKey;
  end else
    Result := Default;
end;

procedure TlvkRegistryStateStorage.DoWriteString(const SectionName, ID,
  Value: String);
begin
  if FRegistry.OpenKey(StringReplace(UseKey + '\' + SectionName, '.', '\',
    [rfReplaceAll]), True) then
  try
    FRegistry.WriteString(ID, SafeEncode(Value));
  finally
    FRegistry.CloseKey;
  end else
    raise ElvkRegistryStateStorage.Create('Unable to open registry key');
end;

procedure TlvkRegistryStateStorage.EndJob;
begin
  FreeAndNil(FRegistry);
end;

function TlvkRegistryStateStorage.UseKey: string;
var
  VerInfo : TlvkVersionInformation;
begin
  Result := '\' + StringReplace(FKey, '%EXENAME%',
    ChangeFileExt(ExtractFileName(Application.ExeName), ''),
    [rfReplaceAll, rfIgnoreCase]);
  if Pos('%COMPANY%', UpperCase(Result)) > 0 then
  begin
    VerInfo := TlvkVersionInformation.Create(nil);
    try
      Result := '\' + StringReplace(FKey, '%COMPANY%',
        VerInfo.CompanyName, [rfReplaceAll, rfIgnoreCase]);
    finally
      VerInfo.Free;
    end;
  end;
end;

end.
