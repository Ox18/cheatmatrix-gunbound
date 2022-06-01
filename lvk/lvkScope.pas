{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the scope object and associated functions, for taking
    care of scope-based memory allocation and class instances that should be
    automatically cleaned up.
}
unit lvkScope;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkScope.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, Graphics, Controls;

type
{$IFNDEF DELPHI6UP}
  PBoolean = ^Boolean;
{$ENDIF}

  IScoped = interface
    ['{C3266DAE-D875-4DB4-B046-534DF72EE555}']

    procedure Restore;
    procedure Forget;
  end;

  IScope = interface
    ['{34732F08-536D-490A-AD09-AE140C7C5807}']

    function DecimalSeparator(const NewDecimalSeparator: Char): IScoped;
    function DestroyObject(const ObjectInstance: TObject): IScoped; overload;
    function DestroyObject(const ObjectInstances: array of TObject): IScoped; overload;
    function ScreenCursor(const Cursor: TCursor): IScoped;
    function BooleanFlag(var FlagVariable: Boolean; const Value: Boolean): IScoped;
    function StringsUpdate(const Strings: TStrings): IScoped;
  end;

var
  Scope : IScope  = nil;

implementation

uses
  SysUtils, Forms;

type
  TScope = class(TInterfacedObject, IScope)
  protected
    // IScope interface
    function DecimalSeparator(const NewDecimalSeparator: Char): IScoped;
    function DestroyObject(const ObjectInstance: TObject): IScoped; overload;
    function DestroyObject(const ObjectInstances: array of TObject): IScoped; overload;
    function ScreenCursor(const Cursor: TCursor): IScoped;
    function BooleanFlag(var FlagVariable: Boolean; const Value: Boolean): IScoped;
    function StringsUpdate(const Strings: TStrings): IScoped;
  end;

  TBaseScoped = class(TInterfacedObject, IScoped)
  private
    FRestored : Boolean;

  protected
    // IScoped interface
    procedure Restore;
    procedure Forget;

    // Internal
    procedure DoRestore; virtual;
    procedure DoForget; virtual;

  public
    destructor Destroy; override;
  end;

  TDecimalSeparatorScoped = class(TBaseScoped)
  private
    FOldDecimalSeparator  : Char;

  protected
    // Internal
    procedure DoRestore; override;

  public
    constructor Create(const NewDecimalSeparator: Char);
  end;

  TDestroyObjectScoped = class(TBaseScoped)
  private
    FObjectInstances  : array of TObject;

  protected
    // Internal
    procedure DoRestore; override;

  public
    constructor Create(const ObjectInstance: TObject); overload;
    constructor Create(const ObjectInstances: array of TObject); overload;
  end;

  TScreenCursorScoped = class(TBaseScoped)
  private
    FOldCursor  : TCursor;

  protected
    // Internal
    procedure DoRestore; override;

  public
    constructor Create(const Cursor: TCursor);
  end;

  TBooleanFlagScoped = class(TBaseScoped)
  private
    FVariable : PBoolean;
    FOldValue : Boolean;

  protected
    // Internal
    procedure DoRestore; override;

  public
    constructor Create(var Variable: Boolean; const Value: Boolean);
  end;

  TStringsScoped = class(TBaseScoped)
  private
    FStrings  : TStrings;

  protected
    // Internal
    procedure DoRestore; override;

  public
    constructor Create(const Strings: TStrings);
  end;

{ TScope }

function TScope.BooleanFlag(var FlagVariable: Boolean;
  const Value: Boolean): IScoped;
begin
  if Value <> FlagVariable then
    Result := TBooleanFlagScoped.Create(FlagVariable, Value)
  else
    Result := nil;
end;

function TScope.DecimalSeparator(const NewDecimalSeparator: Char): IScoped;
begin
  if NewDecimalSeparator <> SysUtils.DecimalSeparator then
    Result := TDecimalSeparatorScoped.Create(NewDecimalSeparator)
  else
    Result := nil;
end;

function TScope.DestroyObject(const ObjectInstance: TObject): IScoped;
begin
  if Assigned(ObjectInstance) then
    Result := TDestroyObjectScoped.Create(ObjectInstance)
  else
    Result := nil;
end;

function TScope.DestroyObject(
  const ObjectInstances: array of TObject): IScoped;
begin
  if Length(ObjectInstances) > 0 then
    Result := TDestroyObjectScoped.Create(ObjectInstances)
  else
    Result := nil;
end;

function TScope.ScreenCursor(const Cursor: TCursor): IScoped;
begin
  if Cursor <> Screen.Cursor then
    Result := TScreenCursorScoped.Create(Cursor)
  else
    Result := nil;
end;

function TScope.StringsUpdate(const Strings: TStrings): IScoped;
begin
  if Assigned(Strings) then
    Result := TStringsScoped.Create(Strings)
  else
    Result := nil;
end;

{ TBaseScoped }

destructor TBaseScoped.Destroy;
begin
  Restore;

  inherited;
end;

procedure TBaseScoped.DoForget;
begin
  // Do nothing by default
end;

procedure TBaseScoped.DoRestore;
begin
  // Do nothing
end;

procedure TBaseScoped.Forget;
begin
  FRestored := True;
end;

procedure TBaseScoped.Restore;
begin
  if not FRestored then
  begin
    DoRestore;
    FRestored := True;
  end;
end;

{ TDecimalSeparatorScoped }

constructor TDecimalSeparatorScoped.Create(
  const NewDecimalSeparator: Char);
begin
  inherited Create;

  FOldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := NewDecimalSeparator;
end;

procedure TDecimalSeparatorScoped.DoRestore;
begin
  DecimalSeparator := FOldDecimalSeparator;
end;

{ TDestroyObjectScoped }

constructor TDestroyObjectScoped.Create(const ObjectInstance: TObject);
begin
  Create([ObjectInstance]);
end;

constructor TDestroyObjectScoped.Create(
  const ObjectInstances: array of TObject);
var
  Index : Integer;
begin
  inherited Create;

  SetLength(FObjectInstances, Length(ObjectInstances));
  for Index := Low(ObjectInstances) to High(ObjectInstances) do
    FObjectInstances[Index] := ObjectInstances[Index];
end;

procedure TDestroyObjectScoped.DoRestore;
var
  Index : Integer;
begin
  for Index := Low(FObjectInstances) to High(FObjectInstances) do
    if Assigned(FObjectInstances[Index]) then
      FObjectInstances[Index].Free;
end;

{ TScreenCursorScoped }

constructor TScreenCursorScoped.Create(const Cursor: TCursor);
begin
  inherited Create;

  FOldCursor := Screen.Cursor;
  Screen.Cursor := Cursor;
end;

procedure TScreenCursorScoped.DoRestore;
begin
  Screen.Cursor := FOldCursor;
end;

{ TBooleanFlagScoped }

constructor TBooleanFlagScoped.Create(var Variable: Boolean;
  const Value: Boolean);
begin
  inherited Create;

  FVariable := @Variable;
  FOldValue := FVariable^;

  FVariable^ := Value;
end;

procedure TBooleanFlagScoped.DoRestore;
begin
  FVariable^ := FOldValue;
end;

{ TStringsScoped }

constructor TStringsScoped.Create(const Strings: TStrings);
begin
  inherited Create;

  FStrings := Strings;
  FStrings.BeginUpdate;
end;

procedure TStringsScoped.DoRestore;
begin
  FStrings.EndUpdate;
end;

initialization
  Scope := TScope.Create;
end.
