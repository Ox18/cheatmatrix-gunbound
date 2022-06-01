{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{* Portions contributed by Wim van der Vegt                                   *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a design-time component wrapper for TlvkActiveScript.
}
unit lvkActiveScriptComponent;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkActiveScriptComponent.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkActiveScript, ActiveX, lvkComponents;

type
{ Description:
    This component is a design-time component wrapper for TlvkActiveScript.
  See also:
    TlvkActiveScript
}
  TlvkActiveScriptWrapper = class(TlvkComponent)
  private
    FActiveScript       : IlvkActiveScript;
    FTypeLibraries      : TStrings;

    FOnBeforeCompile    : TNotifyEvent;
    FOnAfterCompile     : TNotifyEvent;
    FOnBeforeInvalidate : TNotifyEvent;
    FOnAfterInvalidate  : TNotifyEvent;

    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetScriptCode: TStrings;
    procedure SetScriptCode(const Value: TStrings);
    function GetScriptFilename: string;
    procedure SetScriptFilename(const Value: string);
    function GetDispatch: OleVariant;
    procedure SetTypeLibraries(const Value: TStrings);
    function GetModules: TlvkActiveScriptModuleCollection;
    procedure SetModules(const Value: TlvkActiveScriptModuleCollection);

    procedure DoBeforeInvalidate;
    procedure DoAfterInvalidate;
    procedure DoBeforeCompile;
    procedure DoAfterCompile;

    procedure BeforeInvalidate(Sender: TObject);
    procedure AfterInvalidate(Sender: TObject);
    procedure BeforeCompile(Sender: TObject);
    procedure AfterCompile(Sender: TObject);
    procedure TypeLibrariesChanged(Sender: TObject);
    function GetPreProcess: Boolean;
    procedure SetPreProcess(const Value: Boolean);

  public
    // <ALIAS TlvkActiveScript.Create@THandle>
    constructor Create(AOwner: TComponent); override;
    // <ALIAS TlvkActiveScript.Destroy>
    destructor Destroy; override;

    // <ALIAS TlvkActiveScript.Compile>
    procedure Compile;
    // <ALIAS TlvkActiveScript.Invalidate>
    procedure Invalidate;
    // <ALIAS TlvkActiveScript.RaiseLastScriptError@Exception>
    function RaiseLastScriptError(const ConvertException: Exception=nil): Boolean; virtual;
    // <ALIAS TlvkActiveScript.Intf>
    property Intf: OleVariant read GetDispatch;
    // <ALIAS IlvkActiveScript.Call@string@array of OleVariant>
    function Call(const FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@array of OleVariant>
    function Call(const FunctionName: string): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@array of OleVariant>
    function Call(const ModuleName, FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@array of OleVariant>
    function Call(const ModuleName, FunctionName: string): OleVariant; overload;
    // <ALIAS TlvkActiveScript.Eval@string>
    function Eval(const Expression: string): OleVariant;
    // <ALIAS IlvkActiveScript.AddObject@string@IDispatch@Boolean>
    procedure AddObject(const Name: string; const Instance: IDispatch; const Global: Boolean); virtual;
    // <ALIAS IlvkActiveScript.RemoveObjects>
    procedure RemoveObjects; virtual;
    // <ALIAS IlvkActiveScript.AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean>
    procedure AddTypeLibrary(const CLSID: TCLSID; const MajorVersion, MinorVersion: LongWord; const IsControl: Boolean=False); overload;
    // <ALIAS IlvkActiveScript.AddTypeLibrary@string>
    procedure AddTypeLibrary(const ProgID: string); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibrary@TCLSID>
    procedure RemoveTypeLibrary(const CLSID: TCLSID); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibrary@string>
    procedure RemoveTypeLibrary(const ProgID: string); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibraries>
    procedure RemoveTypeLibraries;
    // <ALIAS IlvkActiveScript.RemoveObject@string>
    procedure RemoveObject(const Name: string); virtual;
    // <ALIAS IlvkActiveScript.GetIdentifierList@TStrings@TGetIdentifierListIncludes>
    procedure GetIdentifierList(const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;
    // <ALIAS IlvkActiveScript.GetIdentifierList@TStrings@TGetIdentifierListIncludes>
    procedure GetIdentifierList(const ModuleName: string; const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;

    { Description:
        This property gives direct access to the wrapped TlvkActiveScript
        object, if needed.
      See also:
        TlvkActiveScript
    }
    property ScriptObject: IlvkActiveScript read FActiveScript;

  published
    { Description:
        All the ProgID's listed in the TypeLibraries property will be
        registered as type libraries with the activescript object when it
        is recompiled.
      See also:
        TlvkActiveScript.AddTypeLibrary@string
    }
    property TypeLibraries: TStrings read FTypeLibraries write SetTypeLibraries;

    // <ALIAS TlvkActiveScript.Modules>
    property Modules: TlvkActiveScriptModuleCollection read GetModules write SetModules;
    // <ALIAS TlvkActiveScript.Language>
    property Language: string read GetLanguage write SetLanguage;
    // <ALIAS TlvkActiveScript.ScriptCode>
    property ScriptCode: TStrings read GetScriptCode write SetScriptCode;
    // <ALIAS TlvkActiveScript.ScriptFilename>
    property ScriptFilename: string read GetScriptFilename write SetScriptFilename;
    // <ALIAS TlvkActiveScriptModule.PreProcess>
    property PreProcess: Boolean read GetPreProcess write SetPreProcess;
    // <ALIAS TlvkActiveScript.OnBeforeCompile>
    property OnBeforeCompile: TNotifyEvent read FOnBeforeCompile write FOnBeforeCompile;
    // <ALIAS TlvkActiveScript.OnAfterCompile>
    property OnAfterCompile: TNotifyEvent read FOnAfterCompile write FOnAfterCompile;
    // <ALIAS TlvkActiveScript.OnBeforeInvalidate>
    property OnBeforeInvalidate: TNotifyEvent read FOnBeforeInvalidate write FOnBeforeInvalidate;
    // <ALIAS TlvkActiveScript.OnAfterInvalidate>
    property OnAfterInvalidate: TNotifyEvent read FOnAfterInvalidate write FOnAfterInvalidate;
  end;

procedure Register;

implementation

uses
  Forms;

const
  DEFAULT_LANGUAGE  = 'VBScript';
  DEFAULT_CODE      = 'Option Explicit'#13#10#13#10''' Your code here';
  
procedure Register;
begin
  RegisterComponents('LVK ActiveScript', [TlvkActiveScriptWrapper]);
end;

{ TlvkActiveScriptWrapper }

procedure TlvkActiveScriptWrapper.AddObject(const Name: string;
  const Instance: IDispatch; const Global: Boolean);
begin
  FActiveScript.AddObject(Name, Instance, Global);
end;

procedure TlvkActiveScriptWrapper.AddTypeLibrary(const CLSID: TCLSID;
  const MajorVersion, MinorVersion: LongWord; const IsControl: Boolean);
begin
  FActiveScript.AddTypeLibrary(CLSID, MajorVersion, MinorVersion, IsControl);
end;

procedure TlvkActiveScriptWrapper.AddTypeLibrary(const ProgID: string);
begin
  FActiveScript.AddTypeLibrary(ProgID);
end;

procedure TlvkActiveScriptWrapper.AfterCompile(Sender: TObject);
begin
  DoAfterCompile;
end;

procedure TlvkActiveScriptWrapper.AfterInvalidate(Sender: TObject);
begin
  DoAfterInvalidate;
end;

procedure TlvkActiveScriptWrapper.BeforeCompile(Sender: TObject);
var
  i : Integer;
begin
  DoBeforeCompile;

  FActiveScript.RemoveTypeLibraries;
  for i := 0 to FTypeLibraries.Count-1 do
    FActiveScript.AddTypeLibrary(FTypeLibraries[i]);
end;

procedure TlvkActiveScriptWrapper.BeforeInvalidate(Sender: TObject);
begin
  DoBeforeInvalidate;
end;

function TlvkActiveScriptWrapper.Call(const ModuleName,
  FunctionName: string; const Args: array of OleVariant): OleVariant;
begin
  Result := FActiveScript.Call(ModuleName, FunctionName, Args);
end;

function TlvkActiveScriptWrapper.Call(
  const FunctionName: string): OleVariant;
begin
  Result := FActiveScript.Call(FunctionName);
end;

function TlvkActiveScriptWrapper.Call(const FunctionName: string;
  const Args: array of OleVariant): OleVariant;
begin
  Result := FActiveScript.Call(FunctionName, Args);
end;

function TlvkActiveScriptWrapper.Call(const ModuleName,
  FunctionName: string): OleVariant;
begin
  Result := FActiveScript.Call(ModuleName, FunctionName);
end;

procedure TlvkActiveScriptWrapper.Compile;
begin
  FActiveScript.Compile;
end;

constructor TlvkActiveScriptWrapper.Create(AOwner: TComponent);
begin
  inherited;

  FTypeLibraries := TStringList.Create;
  FActiveScript := TlvkActiveScript.Create(Application.Handle);

  TStringList(FTypeLibraries).OnChange := TypeLibrariesChanged;

  FActiveScript.OnBeforeCompile := BeforeCompile;
  FActiveScript.OnAfterCompile := AfterCompile;
  FActiveScript.OnBeforeInvalidate := BeforeInvalidate;
  FActiveScript.OnAfterInvalidate := AfterInvalidate;

  Language := DEFAULT_LANGUAGE;
  ScriptCode.Text := DEFAULT_CODE;
end;

destructor TlvkActiveScriptWrapper.Destroy;
begin
  FActiveScript := nil;
  FTypeLibraries.Free;

  inherited;
end;

procedure TlvkActiveScriptWrapper.DoAfterCompile;
begin
  if Assigned(FOnAfterCompile) then
    FOnAfterCompile(Self);
end;

procedure TlvkActiveScriptWrapper.DoAfterInvalidate;
begin
  if Assigned(FOnAfterInvalidate) then
    FOnAfterInvalidate(Self);
end;

procedure TlvkActiveScriptWrapper.DoBeforeCompile;
begin
  if Assigned(FOnBeforeCompile) then
    FOnBeforeCompile(Self);
end;

procedure TlvkActiveScriptWrapper.DoBeforeInvalidate;
begin
  if Assigned(FOnBeforeInvalidate) then
    FOnBeforeInvalidate(Self);
end;

function TlvkActiveScriptWrapper.Eval(
  const Expression: string): OleVariant;
begin
  Result := FActiveScript.Eval(Expression);
end;

function TlvkActiveScriptWrapper.GetDispatch: OleVariant;
begin
  Result := FActiveScript.Intf;
end;

procedure TlvkActiveScriptWrapper.GetIdentifierList(const List: TStrings;
  const IncludeIdentifiers: TGetIdentifierListIncludes);
begin
  FActiveScript.GetIdentifierList(List, IncludeIdentifiers);
end;

procedure TlvkActiveScriptWrapper.GetIdentifierList(
  const ModuleName: string; const List: TStrings;
  const IncludeIdentifiers: TGetIdentifierListIncludes);
begin
  FActiveScript.GetIdentifierList(ModuleName, List, IncludeIdentifiers);
end;

function TlvkActiveScriptWrapper.GetLanguage: string;
begin
  Result := FActiveScript.Language;
end;

function TlvkActiveScriptWrapper.GetModules: TlvkActiveScriptModuleCollection;
begin
  Result := FActiveScript.Modules;
end;

function TlvkActiveScriptWrapper.GetPreProcess: Boolean;
begin
  Result := FActiveScript.PreProcess;
end;

function TlvkActiveScriptWrapper.GetScriptCode: TStrings;
begin
  Result := FActiveScript.ScriptCode;
end;

function TlvkActiveScriptWrapper.GetScriptFilename: string;
begin
  Result := FActiveScript.ScriptFilename;
end;

procedure TlvkActiveScriptWrapper.Invalidate;
begin
  FActiveScript.Invalidate;
end;

function TlvkActiveScriptWrapper.RaiseLastScriptError(
  const ConvertException: Exception): Boolean;
begin
  Result := FActiveScript.RaiseLastScriptError(ConvertException);
end;

procedure TlvkActiveScriptWrapper.RemoveObject(const Name: string);
begin
  FActiveScript.RemoveObject(Name);
end;

procedure TlvkActiveScriptWrapper.RemoveObjects;
begin
  FActiveScript.RemoveObjects;
end;

procedure TlvkActiveScriptWrapper.RemoveTypeLibraries;
begin
  FActiveScript.RemoveTypeLibraries;
end;

procedure TlvkActiveScriptWrapper.RemoveTypeLibrary(const ProgID: string);
begin
  FActiveScript.RemoveTypeLibrary(ProgID);
end;

procedure TlvkActiveScriptWrapper.RemoveTypeLibrary(const CLSID: TCLSID);
begin
  FActiveScript.RemoveTypeLibrary(CLSID);
end;

procedure TlvkActiveScriptWrapper.SetLanguage(const Value: string);
begin
  FActiveScript.Language := Value;
end;

procedure TlvkActiveScriptWrapper.SetModules(
  const Value: TlvkActiveScriptModuleCollection);
begin
  FActiveScript.Modules := Value;
end;

procedure TlvkActiveScriptWrapper.SetPreProcess(const Value: Boolean);
begin
  FActiveScript.PreProcess := Value;
end;

procedure TlvkActiveScriptWrapper.SetScriptCode(const Value: TStrings);
begin
  FActiveScript.ScriptCode := Value;
end;

procedure TlvkActiveScriptWrapper.SetScriptFilename(const Value: string);
begin
  FActiveScript.ScriptFilename := Value;
end;

procedure TlvkActiveScriptWrapper.SetTypeLibraries(const Value: TStrings);
begin
  if Assigned(Value) then
    FTypeLibraries.Assign(Value)
  else
    FTypeLibraries.Clear;
end;

procedure TlvkActiveScriptWrapper.TypeLibrariesChanged(Sender: TObject);
begin
  Invalidate;
end;

end.

