{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the directory selection dialog component.
}
unit lvkDirectoryDialog;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 4 $
// $Archive: /Components/LVK/source/lvkDirectoryDialog.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows, lvkComponents;

type
  TlvkDirectoryDialogOption = (ddoIncludeFiles, ddoNewFolderButton,
    ddoStatusText, ddoIncludeURLs, ddoDontGoBelowDomain,
    ddoOnlyFSAncestors, ddoOnlyFSDirectories, ddoShareable,
    ddoBrowseForPrinters, ddoBrowseForComputers);

  TlvkDirectoryDialogOptions = set of TlvkDirectoryDialogOption;

  TlvkDirectoryDialogStyle = (ddsOldStyle, ddsNewStyle, ddsNewUI);

  TOKButton = (obUnchanged, obEnable, obDisable);

const
  DEFAULT_OPTIONS       = [ddoOnlyFSDirectories, ddoNewFolderButton];
  DEFAULT_DIALOG_STYLE  = ddsNewUI;

type
  TlvkCustomDirectoryDialog = class;
  TSelectionChangeEvent = procedure(const Sender: TObject;
    const Selection: string; var OKButton: TOKButton) of object;

  TlvkCustomDirectoryDialog = class(TlvkComponent)
  private
    FSelection          : string;
    FRoot               : string;
    FCaption            : string;
    FOptions            : TlvkDirectoryDialogOptions;
    FDialogStyle        : TlvkDirectoryDialogStyle;
    FFlags              : Cardinal;
    FStatusText         : string;
    FHandle             : HWND;

    FOnSelectionChange  : TSelectionChangeEvent;

    procedure SetStatusText(const Value: string);
    procedure SetSelection(const Value: string);
    function StoreOptions: Boolean;

  protected
    property Root: string read FRoot write FRoot;
    property Selection: string read FSelection write SetSelection;
    property Caption: string read FCaption write FCaption;
    property Options: TlvkDirectoryDialogOptions read FOptions write FOptions
      stored StoreOptions;
    property Flags: Cardinal read FFlags write FFlags;
    property DialogStyle: TlvkDirectoryDialogStyle read FDialogStyle
      write FDialogStyle default DEFAULT_DIALOG_STYLE;

    property OnSelectionChange: TSelectionChangeEvent read FOnSelectionChange
      write FOnSelectionChange;
    property StatusText: string read FStatusText write SetStatusText;

  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean;
  end;

  TlvkDirectoryDialog = class(TlvkCustomDirectoryDialog)
  published
    property Selection;
    property Caption;
    property Root;
    property OnSelectionChange;
    property Options;
    property StatusText;
    property DialogStyle;
  end;

implementation

uses
  Messages, Forms, ShlObj, ActiveX;

const
  BIF_NONEWFOLDERBUTTON = $0200;

  BFFM_SETOKTEXT        = WM_USER + 105;

{ TlvkCustomDirectoryDialog }

function DirectoryDialogCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
var
  DirectoryDialog : TlvkCustomDirectoryDialog;
  OKButton        : TOKButton;
  ShellMalloc     : IMalloc;
  Buffer          : PChar;
  Rect            : TRect;
begin
  DirectoryDialog := TlvkCustomDirectoryDialog(lpData);

  if Assigned(DirectoryDialog) then
  begin                                            
    DirectoryDialog.FHandle := Wnd;
    try
      case uMsg of
        BFFM_INITIALIZED:
          begin
            if DirectoryDialog.Selection <> '' then
              SendMessage(Wnd, BFFM_SETSELECTION, Integer(True),
                Integer(PChar(DirectoryDialog.Selection)));

            GetWindowRect(Wnd, Rect);
            SetWindowPos(Wnd, 0,
              (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
              (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 2,
              0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
          end;

        BFFM_SELCHANGED:
          begin
            if Assigned(DirectoryDialog.OnSelectionChange) then
            begin
              if SHGetMalloc(ShellMalloc) = S_OK then
              begin
                OKButton := obUnchanged;
                Buffer := ShellMalloc.Alloc(MAX_PATH);
                try
                  SHGetPathFromIDList(PItemIDList(lParam), Buffer);
                  DirectoryDialog.OnSelectionChange(DirectoryDialog, Buffer,
                    OKButton);
                  if OKButton <> obUnchanged then
                    SendMessage(Wnd, BFFM_ENABLEOK, 0, Ord(OKButton = obEnable));
                finally
                  ShellMalloc.Free(Buffer);
                end;
              end;
            end;
          end;
      end;
    finally
      DirectoryDialog.FHandle := 0;
    end;
  end;

  Result := 0;
end;

constructor TlvkCustomDirectoryDialog.Create(AOwner: TComponent);
begin
  inherited;

  FOptions := DEFAULT_OPTIONS;
  FDialogStyle := DEFAULT_DIALOG_STYLE;
end;

function TlvkCustomDirectoryDialog.Execute: Boolean;
var
  BrowseInfo        : TBrowseInfo;
  ItemIDList        : PItemIDList;
  RootItemIDList    : PItemIDList;
  ErrorMode         : Cardinal;
  Buffer            : PChar;
  ShellMalloc       : IMalloc;
  WindowList        : Pointer;
  DesktopFolder     : IShellFolder;
  Flags             : Cardinal;
  Eaten             : LongWord;
  Option            : TlvkDirectoryDialogOption;
{$IFNDEF DELPHI7UP}
const
  BIF_BROWSEINCLUDEURLS  = $0080;
  BIF_SHAREABLE          = $8000;
  BIF_NEWDIALOGSTYLE     = $0040;
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
{$ENDIF}
const
  TrueOptionValues  : array[TlvkDirectoryDialogOption] of Cardinal = (
    BIF_BROWSEINCLUDEFILES,                 // ddoIncludeFiles
    0,                                      // ddoNewFolderButton,
    BIF_STATUSTEXT,                         // ddoStatusText
    BIF_BROWSEINCLUDEURLS,                  // ddoIncludeURLs
    BIF_DONTGOBELOWDOMAIN,                  // ddoDontGoBelowDomain
    BIF_RETURNFSANCESTORS,                  // ddoOnlyFSAncestors
    BIF_RETURNONLYFSDIRS,                   // ddoOnlyFSDirectories
    BIF_SHAREABLE,                          // ddoShareable
    BIF_BROWSEFORPRINTER,                   // ddoBrowseForPrinters
    BIF_BROWSEFORCOMPUTER                   // ddoBrowseForComputers
  );

  FalseOptionValues : array[TlvkDirectoryDialogOption] of Cardinal = (
    0,                                      // ddoIncludeFiles
    BIF_NONEWFOLDERBUTTON,                  // ddoNewFolderButton,
    0,                                      // ddoStatusText
    0,                                      // ddoIncludeURLs
    0,                                      // ddoDontGoBelowDomain
    0,                                      // ddoOnlyFSAncestors
    0,                                      // ddoOnlyFSDirectories
    0,                                      // ddoShareable
    0,                                      // ddoBrowseForPrinters
    0                                       // ddoBrowseForComputers
  );
  
  DialogStyleValues : array[TlvkDirectoryDialogStyle] of Cardinal = (
    0,                                      // ddsOldStyle
    BIF_NEWDIALOGSTYLE,                     // ddsNewStyle
    BIF_USENEWUI                            // ddsNewUI
  );
begin
  Result := False;

  if SHGetMalloc(ShellMalloc) = S_OK then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);
      BrowseInfo.hwndOwner := Application.Handle;
      BrowseInfo.lParam := Integer(Self);
      BrowseInfo.lpfn := DirectoryDialogCallback;
      BrowseInfo.lpszTitle := PChar(FCaption);
      BrowseInfo.pidlRoot := nil;
      BrowseInfo.pszDisplayName := Buffer;
      BrowseInfo.ulFlags := FFlags or DialogStyleValues[FDialogStyle];
      for Option := Low(TlvkDirectoryDialogOption) to High(TlvkDirectoryDialogOption) do
        if Option in FOptions then
          BrowseInfo.ulFlags := BrowseInfo.ulFlags or TrueOptionValues[Option]
        else
          BrowseInfo.ulFlags := BrowseInfo.ulFlags or FalseOptionValues[Option];

      RootItemIDList := nil;
      try
        if FRoot <> '' then
        begin
          SHGetDesktopFolder(DesktopFolder);
          DesktopFolder.ParseDisplayName(Application.Handle, nil,
            POleStr(WideString(FRoot)), Eaten, RootItemIDList, Flags);
          BrowseInfo.pidlRoot := RootItemIDList;
        end;

        WindowList := DisableTaskWindows(0);
        try
          ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            ItemIDList := ShBrowseForFolder(BrowseInfo);
          finally
            SetErrorMode(ErrorMode);
          end;
        finally
          EnableTaskWindows(WindowList);
        end;

        Result := Assigned(ItemIDList);
        if Result then
        try
          ShGetPathFromIDList(ItemIDList, Buffer);
          FSelection := Buffer;
        finally
          ShellMalloc.Free(ItemIDList);
        end;
      finally
        if Assigned(RootItemIDList) then
          ShellMalloc.Free(RootItemIDList);
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

procedure TlvkCustomDirectoryDialog.SetSelection(const Value: string);
begin
  if FSelection <> Value then
  begin
    FSelection := Value;
    if FHandle <> 0 then
      SendMessage(FHandle, BFFM_SETSELECTION, Integer(True), Integer(PChar(Value)));
  end;
end;

procedure TlvkCustomDirectoryDialog.SetStatusText(const Value: string);
begin
  if FStatusText <> Value then
  begin
    FStatusText := Value;
    if FHandle <> 0 then
      SendMessage(FHandle, BFFM_SETSTATUSTEXT, Integer(True), Integer(PChar(Value)));
  end;
end;

function TlvkCustomDirectoryDialog.StoreOptions: Boolean;
begin
  Result := FOptions <> DEFAULT_OPTIONS;
end;

end.
