{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a global hotkey component.
}
unit lvkGlobalHotKeys;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkGlobalHotKeys.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Messages, Windows, Menus, lvkComponents;

type
  { Description:
      This class holds a single keyboard hotkey for use with
      TlvkCustomGlobalHotKeys.
    See also:
      TlvkCustomGlobalHotKeys, THotKeyCollection
  }
  THotKeyItem = class(TCollectionItem)
  private
    FHotKey             : TShortCut;
    FOnExecute          : TNotifyEvent;
    FApplicationToFront : Boolean;
    FID                 : Integer;

    procedure Changed;

    procedure SetHotKey(const Value: TShortCut);
    procedure SetOnExecute(const Value: TNotifyEvent);
    procedure SetApplicationToFront(const Value: Boolean);

    procedure DoExecute;

  protected
    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

  published
    { Description:
        This property controls wether the application should be brought to
        front or not when the hotkey is invoked. Set it to False and the
        application will remain in the background.
    }
    property ApplicationToFront: Boolean read FApplicationToFront
      write SetApplicationToFront default True;

    { Description:
        This property holds the hotkey to use.
    }
    property HotKey: TShortCut read FHotKey write SetHotKey default 0;

    { Description:
        This event is executed once when the hotkey is pressed.
    }
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
  end;

  { Description:
      This collection class holds a list of THotKeyItem instances that
      each define a single keyboard hotkey.
    See also:
      TlvkCustomGlobalHotKeys, TlvkCustomGlobalHotKeys.HotKeys,
      THotKeyItem
  }
  THotKeyCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): THotKeyItem;
    procedure SetItem(Index: Integer; const Value: THotKeyItem);

  public
    constructor Create(AOwner: TPersistent);
    function Add: THotKeyItem;
    function FindItemID(ID: Integer): THotKeyItem;
    function Insert(Index: Integer): THotKeyItem;
    property Items[Index: Integer]: THotKeyItem read GetItem write SetItem;
      default;
  end;

  { Description:
      This component allows you to define keyboard hotkeys that will work
      even if the application is not the currently active one.
  }
  TlvkCustomGlobalHotKeys = class(TlvkComponent)
  private
    FHotKeys  : THotKeyCollection;
    FIDs      : array of Integer;
    FHandle   : THandle;
    FActive   : Boolean;

    procedure SetHotKeys(const Value: THotKeyCollection);
    procedure SetActive(const Value: Boolean);

    procedure WndProc(var Message: TMessage);

  protected
    procedure Changed;

    { Description:
        This property holds a list of all currently defined hotkeys. Look at
        THotKeyCollection for more information.
      See also:
        THotKeyCollection
    }
    property HotKeys: THotKeyCollection read FHotKeys write SetHotKeys;

    { Description:
        This property controls wether the hotkeys are active or not.,
    }
    property Active: Boolean read FActive write SetActive;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component is used to set up a system-wide
      hotkey that can be used to activate or execute functions
      in your application, even when the application is not the
      active one.

      Especially useful in conjunction with tray icon based
      applications.
  }
  TlvkGlobalHotKeys = class(TlvkCustomGlobalHotKeys)
  published
    // <ALIAS TlvkCustomGlobalHotKeys.HotKeys>
    property HotKeys;
    // <ALIAS TlvkCustomGlobalHotKeys.Active>
    property Active;
  end;

implementation

uses
  Forms;

type
  TIDManager = class
  private
    FIDs  : array of Integer;

  public
    function GetAvailableID: Integer;
    procedure ReleaseID(const ID: Integer);
  end;

var
  IDManager : TIDManager;

{ TIDManager }

function TIDManager.GetAvailableID: Integer;
var
  Ok    : Boolean;
  Index : Integer;
begin
  Result := $F000;

  repeat
    Ok := True;

    for Index := Low(FIDs) to High(FIDs) do
    begin
      if FIDs[Index] = Result then
      begin
        Inc(Result);
        Ok := False;
        Break;
      end;
    end;
  until Ok;

  SetLength(FIDs, Length(FIDs)+1);
  FIDs[High(FIDs)] := Result;
end;

procedure TIDManager.ReleaseID(const ID: Integer);
var
  Index : Integer;
begin
  for Index := Low(FIDs) to High(FIDs) do
  begin
    if FIDs[Index] = ID then
    begin
      if Index < High(FIDs) then
        FIDs[Index] := FIDs[High(FIDs)];

      SetLength(FIDs, Length(FIDs)-1);
      Break;
    end;
  end;
end;

{ THotKeyItem }

procedure THotKeyItem.Changed;
begin
  (THotKeyCollection(Collection).GetOwner as TlvkCustomGlobalHotKeys).Changed;
end;

constructor THotKeyItem.Create(Collection: TCollection);
begin
  inherited;

  FID := IDManager.GetAvailableID;
  FApplicationToFront := True;
end;

destructor THotKeyItem.Destroy;
begin
  IDManager.ReleaseID(FID);

  inherited;
end;

procedure THotKeyItem.DoExecute;
begin
  if FApplicationToFront then
    SetForegroundWindow(Application.Handle);
  if Assigned(FOnExecute) then
    FOnExecute(THotKeyCollection(Collection).GetOwner);
end;

function THotKeyItem.GetDisplayName: string;
begin
  if FHotKey <> 0 then
    Result := ShortCutToText(FHotKey)
  else
    Result := inherited GetDisplayName;
end;

procedure THotKeyItem.SetApplicationToFront(const Value: Boolean);
begin
  if Value <> FApplicationToFront then
  begin
    FApplicationToFront := Value;
    Changed;
  end;
end;

procedure THotKeyItem.SetHotKey(const Value: TShortCut);
begin
  if Value <> FHotKey then
  begin
    FHotKey := Value;
    Changed;
  end;
end;

procedure THotKeyItem.SetOnExecute(const Value: TNotifyEvent);
begin
  FOnExecute := Value;
  Changed;
end;

{ TlvkCustomGlobalHotKeys }

procedure TlvkCustomGlobalHotKeys.Changed;
var
  Index     : Integer;
  ShortCut  : TShortCut;
  Modifiers : Cardinal;
begin
  for Index := Low(FIDs) to High(FIDs) do
    UnregisterHotKey(FHandle, FIDs[Index]);
  SetLength(FIDs, 0);

  if FActive and (not (csDesigning in ComponentState)) then
  begin
    for Index := 0 to FHotKeys.Count-1 do
    begin
      if (FHotKeys[Index].HotKey <> 0) and
        (Assigned(FHotKeys[Index].OnExecute) or
        FHotKeys[Index].ApplicationToFront) then
      begin
        SetLength(FIDs, Length(FIDs)+1);
        FIDs[High(FIDs)] := FHotKeys[Index].FID;
        ShortCut := FHotKeys[Index].HotKey;

        Modifiers := 0;
        if (ShortCut and scShift) <> 0 then
        begin
          Modifiers := Modifiers or MOD_SHIFT;
          ShortCut := ShortCut and (not scShift);
        end;
        if (ShortCut and scCtrl) <> 0 then
        begin
          Modifiers := Modifiers or MOD_CONTROL;
          ShortCut := ShortCut and (not scCtrl);
        end;
        if (ShortCut and scAlt) <> 0 then
        begin
          Modifiers := Modifiers or MOD_ALT;
          ShortCut := ShortCut and (not scAlt);
        end;

        if not RegisterHotKey(FHandle, FIDs[High(FIDs)], Modifiers, ShortCut) then
        begin
          SetLength(FIDs, Length(FIDs)-1);
          RaiseLastWin32Error;
        end;
      end;
    end;
  end;
end;

constructor TlvkCustomGlobalHotKeys.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    FHandle := AllocateHWnd(WndProc);

  FActive := True;
  FHotKeys := THotKeyCollection.Create(Self);
end;

destructor TlvkCustomGlobalHotKeys.Destroy;
begin
  Active := False;
  FHotKeys.Free;

  if FHandle <> 0 then
    DeallocateHWnd(FHandle);

  inherited;
end;

procedure TlvkCustomGlobalHotKeys.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TlvkCustomGlobalHotKeys.SetHotKeys(
  const Value: THotKeyCollection);
begin
  FHotKeys.Assign(Value);
end;

procedure TlvkCustomGlobalHotKeys.WndProc(var Message: TMessage);
var
  Index : Integer;
begin
  if Message.Msg = WM_HOTKEY then
  begin
    for Index := 0 to FHotKeys.Count-1 do
      if Message.WParam = FHotKeys[Index].FID then
      FHotKeys[Index].DoExecute;
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam,
      Message.LParam);
end;

{ THotKeyCollection }

function THotKeyCollection.Add: THotKeyItem;
begin
  Result := inherited Add as THotKeyItem;
end;

constructor THotKeyCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, THotKeyItem);
end;

function THotKeyCollection.FindItemID(ID: Integer): THotKeyItem;
begin
  Result := inherited FindItemID(ID) as THotKeyItem;
end;

function THotKeyCollection.GetItem(Index: Integer): THotKeyItem;
begin
  Result := inherited Items[Index] as THotKeyItem;
end;

function THotKeyCollection.Insert(Index: Integer): THotKeyItem;
begin
  Result := inherited Insert(Index) as THotKeyItem;
end;

procedure THotKeyCollection.SetItem(Index: Integer;
  const Value: THotKeyItem);
begin
  inherited Items[Index] := Value;
end;

initialization
  IDManager := TIDManager.Create;
finalization
  IDManager.Free;
end.
