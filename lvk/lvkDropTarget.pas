{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a more advanced drop target component that handles
    various types of data, not only files.
}
unit lvkDropTarget;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDropTarget.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  Windows, SysUtils, Classes, Controls, ActiveX, Graphics, lvkComponents,
  Messages, lvkSubClass;

type
  TDropEffect = (deNone, deCopy, deMove, deLink, deScroll);
  TDropEffects = set of TDropEffect;

  TDropTargetEnterEvent = procedure(Sender: TObject;
    const DataObject: IDataObject;
    const KeyState: Integer;
    const Point: TPoint;
    var Effects: TDropEffects) of object;
  TDropTargetDragEvent = procedure(Sender: TObject;
    const KeyState: Integer;
    const Point: TPoint;
    var Effects: TDropEffects) of object;
  TDropTargetDropEvent = procedure(Sender: TObject;
    const KeyState: Integer;
    const Point: TPoint;
    const Data: TStream; var Accept: Boolean;
    var Effects: TDropEffects) of object;

  TlvkDropTargetFormat = class(TCollectionItem)
  private
    FFormat   : TClipFormat;
    FActive   : Boolean;
    FOnEnter  : TDropTargetEnterEvent;
    FOnDrag   : TDropTargetDragEvent;
    FOnDrop   : TDropTargetDropEvent;
    FOnLeave  : TNotifyEvent;

    function GetUsable: Boolean;

  protected
    property Usable: Boolean read GetUsable;

  published
    property Format: TClipFormat read FFormat write FFormat;
    property Active: Boolean read FActive write FActive;

    property OnEnter: TDropTargetEnterEvent read FOnEnter write FOnEnter;
    property OnDrag: TDropTargetDragEvent read FOnDrag write FOnDrag;
    property OnDrop: TDropTargetDropEvent read FOnDrop write FOnDrop;
    property OnLeave: TNotifyEvent read FOnLeave write FOnLeave;
  end;

  TlvkDropTargetFormatCollection = class(TOwnedCollection)
  private
    function GetItem(const Index: Integer): TlvkDropTargetFormat;
    procedure SetItem(const Index: Integer;
      const Value: TlvkDropTargetFormat);

  public
    constructor Create(const AOwner: TPersistent);

    function Add: TlvkDropTargetFormat;
    function FindItemID(ID: Integer): TlvkDropTargetFormat;
    function Insert(Index: Integer): TlvkDropTargetFormat;
    property Items[const Index: Integer]: TlvkDropTargetFormat read GetItem
      write SetItem; default;
  end;

  TlvkCustomDropTarget = class(TlvkSubClass, IDropTarget)
  private
    FActive           : Boolean;
    FSetActive        : Boolean;
    FFormats          : TlvkDropTargetFormatCollection;
    FCurrentFormat    : TlvkDropTargetFormat;
    FCurrentFormatEtc : TFormatEtc;
    FCurrentEffects   : TDropEffects;

    procedure SetActive(const Value: Boolean);
    procedure DoActivate;
    procedure DoDeactivate;
    function GetTarget: TWinControl;
    procedure SetTarget(const Value: TWinControl);
    function TargetHandle: THandle;
    function GetTargetControl: TWinControl;
    procedure SetFormats(const Value: TlvkDropTargetFormatCollection);
    function EffectsToBitMask(const Effects: TDropEffects): Integer;

    function DoEnter(const dataObj: IDataObject; const grfKeyState: Integer;
      const pt: TPoint; var dwEffect: Integer): HRESULT;
    function DoDrag(const grfKeyState: Integer; const pt: TPoint;
      var dwEffect: Integer): HRESULT;
    function DoDrop(const dataObj: IDataObject; const grfKeyState: Integer;
      const pt: TPoint; var dwEffect: Integer): HRESULT;
    procedure DoLeave;
    function ScreenToClient(const Point: TPoint): TPoint;

    procedure GetData(const DataObject: IDataObject;
      const Stream: TStream);

  protected
    procedure BeforeTargetWindowProc(var Message: TMessage;
      var PassToTarget: Boolean); override;
    procedure AfterTargetWindowProc(var Message: TMessage); override;

    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(grfKeyState: Integer; pt: TPoint;
      var dwEffect: Integer): HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer): HRESULT; stdcall;

    property Active: Boolean read FActive write SetActive default False;
    property Target: TWinControl read GetTarget write SetTarget;
    property Formats: TlvkDropTargetFormatCollection read FFormats
      write SetFormats;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Activate;
    procedure Deactivate;
  end;

  ElvkCustomDropTarget = class(Exception);

  TlvkDropTarget = class(TlvkCustomDropTarget)
  published
    property Active;
    property Target;
    property Formats;
  end;

  TDropEnterEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; var AcceptDrop: Boolean; var Move: Boolean) of object;
  TDropDragEvent = TDropEnterEvent;

  TDropURLEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const URL: string; var Move: Boolean) of object;
  TDropFilesEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const FileNames: TStrings; var Move: Boolean) of object;
  TDropTextEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const Text: TStrings; var Move: Boolean) of object;
  TDropUnicodeTextEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const Text: WideString; var Move: Boolean) of object;
  TDropBitmapEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const Bitmap: TBitmap; var Move: Boolean) of object;
  TDropHTMLEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const SourceURL: string;
    const HTML: TStrings; var Move: Boolean) of object;
  TDropRTFEvent = procedure(Sender: TObject; const KeyState: Integer;
    const Point: TPoint; const RTF: TStrings; var Move: Boolean) of object;

  { Description:
      This is a component that allows your program to handle drag'n'drop
      of various types of data from other programs.

      The component currently handles files (from explorer or other programs),
      bitmap data, text (for instance from Internet Explorer), url's
      (from IE as well), html (from IE as well), rtf text (from IE and other
      programs, like Word).

      The component allows you to designate a specific control as the drop
      target, or use the whole form, as well as events for knowing where
      in the target the drop mouse cursor is currently positioned.
  }
  TlvkStandardDropTarget = class(TComponent)
  private
    FURLFormat          : TlvkDropTargetFormat;
    FTextFormat         : TlvkDropTargetFormat;
    FFilesFormat        : TlvkDropTargetFormat;
    FHTMLFormat         : TlvkDropTargetFormat;
    FBitmapFormat       : TlvkDropTargetFormat;
    FRTFFormat          : TlvkDropTargetFormat;
    FUnicodeTextFormat  : TlvkDropTargetFormat;
    FSetActive          : Boolean;

    FDropTarget         : TlvkDropTarget;
    FOnDropFiles        : TDropFilesEvent;
    FOnDropText         : TDropTextEvent;
    FOnDropURL          : TDropURLEvent;
    FOnDropHTML         : TDropHTMLEvent;
    FOnDropRTF          : TDropRTFEvent;
    FOnDropBitmap       : TDropBitmapEvent;
    FOnDropUnicodeText  : TDropUnicodeTextEvent;
    FOnDrag             : TDropDragEvent;
    FOnEnter            : TDropEnterEvent;
    FOnLeave            : TNotifyEvent;

    function GetActive: Boolean;
    function GetTarget: TWinControl;
    procedure SetActive(const Value: Boolean);
    procedure SetTarget(const Value: TWinControl);

    procedure DropEnter(Sender: TObject; const DataObject: IDataObject;
      const KeyState: Integer; const Point: TPoint; var Effects: TDropEffects);
    procedure DropDrag(Sender: TObject;
      const KeyState: Integer; const Point: TPoint; var Effects: TDropEffects);
    procedure DropLeave(Sender: TObject);

    procedure DropURL(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropText(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropFiles(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropHTML(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropBitmap(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropRTF(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);
    procedure DropUnicodeText(Sender: TObject; const KeyState: Integer;
      const Point: TPoint; const Data: TStream; var Accept: Boolean;
      var Effects: TDropEffects);

    function AddFormat(const Format: TClipFormat;
      const EventHandler: TDropTargetDropEvent): TlvkDropTargetFormat;

    procedure SetOnDropFiles(const Value: TDropFilesEvent);
    procedure SetOnDropText(const Value: TDropTextEvent);
    procedure SetOnDropURL(const Value: TDropURLEvent);
    procedure SetOnDropHTML(const Value: TDropHTMLEvent);
    procedure SetOnDropBitmap(const Value: TDropBitmapEvent);
    procedure SetOnDropRTF(const Value: TDropRTFEvent);
    procedure SetOnDropUnicodeText(const Value: TDropUnicodeTextEvent);

  protected
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        Before the component will start accepting dropped items, you must
        activate it. The default is True, so it will start out as active.
    }
    property Active: Boolean read GetActive write SetActive default False;

    { Description:
        This property controls what control will be the target that this
        component handles dropped items for. For instance, if you set it to
        a panel, when you drop an item on the panel, this component will
        handle it. The default is blank (nil) which means the owning form.
    }
    property Target: TWinControl read GetTarget write SetTarget;

    { Description:
        This event handler is called once when the drag'n'drop mouse cursor
        enters the target area. You can use it to control what to do.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        AcceptDrop - Set to False to not accept the drop. Mouse cursor will
          turn into a stop sign.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
      See also:
        OnDrag, OnLeave
    }
    property OnEnter: TDropEnterEvent read FOnEnter write FOnEnter;

    { Description:
        This event handler is called when the user drops files on the target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        FileNames - List of filenames of the files/directories dropped on the
          target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropFiles: TDropFilesEvent read FOnDropFiles
      write SetOnDropFiles;

    { Description:
        This event handler is called when the user drops html rendered text
        on the target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        SourceURL - The url of the page this html data was dragged from.
        HTML - The html code dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropHTML: TDropHTMLEvent read FOnDropHTML write SetOnDropHTML;

    { Description:
        This event handler is called when the user drops text on the target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        Text - The text dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropText: TDropTextEvent read FOnDropText write SetOnDropText;

    { Description:
        This event handler is called when the user drops unicode text on the
        target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        Text - The unicode text dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropUnicodeText: TDropUnicodeTextEvent read FOnDropUnicodeText
      write SetOnDropUnicodeText;

    { Description:
        This event handler is called when the user drops a internet hyperlink
        on the target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        URL - The internet hyperlink dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropURL: TDropURLEvent read FOnDropURL write SetOnDropURL;

    { Description:
        This event handler is called when the user drops a bitmap on the
        target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        Bitmap - The bitmap dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropBitmap: TDropBitmapEvent read FOnDropBitmap
      write SetOnDropBitmap;

    { Description:
        This event handler is called when the user drops Rich Text Formatted
        text on the target.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        RTF - The Rich Text Formatted text dropped on the target.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
    }
    property OnDropRTF: TDropRTFEvent read FOnDropRTF write SetOnDropRTF;

    { Description:
        This event handler is called whenever the mouse cursor (during
        drag'n'drop) is moved across the target area.
      Parameters:
        Sender - A reference to the TlvkStandardDropTarget component that
          calls the event handler.
        KeyState - The current state of the shift keys.
        Point - The mouse cursor position in the target.
        AcceptDrop - Set to False to not accept the drop. Mouse cursor will
          turn into a stop sign.
        Move - Set to True to use a Move cursor instead of a Copy cursor. Note
          that some programs will also use this to delete/cut the data from
          itself when you drop the item in a different program.
      See also:
        OnEnter, OnLeave
    }
    property OnDrag: TDropDragEvent read FOnDrag write FOnDrag;

    { Description:
        This event handler is called when the mouse cursor (during drag'n'drop)
        leaves the target area.
      Parameters:
        Sender
      See also:
        OnEnter, OnDrag
    }
    property OnLeave: TNotifyEvent read FOnLeave write FOnLeave;
  end;

implementation

uses
  ComObj, AxCtrls, ShlObj;

{ TlvkCustomDropTarget }

procedure TlvkCustomDropTarget.Activate;
begin
  Active := True;
end;

procedure TlvkCustomDropTarget.AfterTargetWindowProc(
  var Message: TMessage);
begin
  inherited;
  if Message.Msg = CM_RECREATEWND then
    if FActive then
      DoActivate;
end;

procedure TlvkCustomDropTarget.BeforeTargetWindowProc(
  var Message: TMessage; var PassToTarget: Boolean);
begin
  inherited;
  if Message.Msg = CM_RECREATEWND then
    if FActive then
      DoDeactivate;
end;

constructor TlvkCustomDropTarget.Create(AOwner: TComponent);
begin
  inherited;

  FActive := False;
  FFormats := TlvkDropTargetFormatCollection.Create(Self);
end;

procedure TlvkCustomDropTarget.Deactivate;
begin
  Active := False;
end;

destructor TlvkCustomDropTarget.Destroy;
begin
  Active := False;
  FFormats.Free;

  inherited;
end;

procedure TlvkCustomDropTarget.DoActivate;
var
  Handle  : THandle;
begin
  Handle := TargetHandle;
  if Handle = INVALID_HANDLE_VALUE then
    raise ElvkCustomDropTarget.Create('No valid target, cannot activate');

  OleCheck(RegisterDragDrop(Handle, Self));
end;

procedure TlvkCustomDropTarget.DoDeactivate;
var
  Handle  : THandle;
begin
  Handle := TargetHandle;
  if Handle <> INVALID_HANDLE_VALUE then
    OleCheck(RevokeDragDrop(Handle));
end;

function TlvkCustomDropTarget.DoDrag(const grfKeyState: Integer;
  const pt: TPoint; var dwEffect: Integer): HRESULT;
var
  Effects : TDropEffects;
begin
  if Assigned(FCurrentFormat) then
  begin
    if Assigned(FCurrentFormat.OnDrag) then
    begin
      Effects := [];
      FCurrentFormat.OnDrag(Self, grfKeyState, ScreenToClient(pt), Effects);
      if Effects <> [] then
      begin
        Result := S_OK;
        dwEffect := EffectsToBitMask(Effects);
      end else
        Result := E_UNEXPECTED;
    end else begin
      Result := S_OK;
      dwEffect := EffectsToBitMask(FCurrentEffects);
    end;
  end else
    Result := E_UNEXPECTED;
end;

function TlvkCustomDropTarget.DoDrop(const dataObj: IDataObject;
  const grfKeyState: Integer; const pt: TPoint;
  var dwEffect: Integer): HRESULT;
var
  FormatEtc : TFormatEtc;
  Stream    : TMemoryStream;
  Accept    : Boolean;
  Effects   : TDropEffects;
begin
  if Assigned(FCurrentFormat) and Assigned(FCurrentFormat.OnDrop) then
  begin
    FormatEtc := FCurrentFormatEtc;

    if dataObj.QueryGetData(FormatEtc) = S_OK then
    begin
      Stream := TMemoryStream.Create;
      try
        GetData(dataObj, Stream);
        Stream.Position := 0;

        Accept := False;
        Effects := [];
        FCurrentFormat.OnDrop(Self, grfKeyState, ScreenToClient(pt), Stream,
          Accept, Effects);

        if Accept then
          Result := S_OK
        else
          Result := E_UNEXPECTED;

        dwEffect := EffectsToBitMask(Effects);
      finally
        Stream.Free;
      end;
    end else
      Result := E_UNEXPECTED;
  end else
    Result := E_UNEXPECTED;
end;

function TlvkCustomDropTarget.DoEnter(const dataObj: IDataObject;
  const grfKeyState: Integer; const pt: TPoint; var dwEffect: Integer): HRESULT;
var
  Effects : TDropEffects;
begin
  dwEffect := DROPEFFECT_NONE;
  Result := E_UNEXPECTED;

  if Assigned(FCurrentFormat) and Assigned(FCurrentFormat.OnEnter) then
  begin
    Effects := [];
    FCurrentFormat.OnEnter(Self, dataObj, grfKeyState, ScreenToClient(pt),
      Effects);
    if Effects <> [] then
    begin
      dwEffect := EffectsToBitMask(Effects);
      FCurrentEffects := Effects;
      Result := S_OK;
    end;
  end;
end;

procedure TlvkCustomDropTarget.DoLeave;
begin
  if Assigned(FCurrentFormat) and Assigned(FCurrentFormat.OnLeave) then
    FCurrentFormat.OnLeave(Self);
end;

function TlvkCustomDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HRESULT;
var
  Enum          : IEnumFORMATETC;
  FormatEtc     : TFormatEtc;
  Index         : Integer;
  FoundFormats  : array of TFormatEtc;

  function FindFormat(const Format: TClipFormat; out FormatEtc: TFormatEtc): Boolean;
  var
    Index : Integer;
  begin
    Result := False;

    for Index := Low(FoundFormats) to High(FoundFormats) do
      if FoundFormats[Index].cfFormat = Format then
      begin
        FormatEtc := FoundFormats[Index];
        Result := True;
        Break;
      end;
  end;

begin
  Result := E_UNEXPECTED;

  OleCheck(dataObj.EnumFormatEtc(DATADIR_GET, Enum));

  SetLength(FoundFormats, 0);
  while Enum.Next(1, FormatEtc, nil) = S_OK do
  begin
    SetLength(FoundFormats, Length(FoundFormats)+1);
    FoundFormats[High(FoundFormats)] := FormatEtc;
  end;

  for Index := 0 to Formats.Count-1 do
  begin
    if FindFormat(Formats[Index].Format, FormatEtc) then
    begin
      if Formats[Index].Usable then
      begin
        FCurrentFormat := Formats[Index];
        FCurrentFormatEtc := FormatEtc;

        Result := DoEnter(dataObj, grfKeyState, pt, dwEffect);
        Break;
      end;
    end;

  end;
end;

function TlvkCustomDropTarget.DragLeave: HRESULT;
begin
  DoLeave;
  FCurrentFormat := nil;
  Result := S_OK;
end;

function TlvkCustomDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HRESULT;
begin
  Result := DoDrag(grfKeyState, pt, dwEffect);
end;

function TlvkCustomDropTarget.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HRESULT;
begin
  Result := DoDrop(dataObj, grfKeyState, pt, dwEffect);
end;

function TlvkCustomDropTarget.EffectsToBitMask(
  const Effects: TDropEffects): Integer;
const
  BitValues : array[TDropEffect] of Integer = (
    DROPEFFECT_NONE,
    DROPEFFECT_COPY,
    DROPEFFECT_MOVE,
    DROPEFFECT_LINK,
    Integer(DROPEFFECT_SCROLL)
  );
var
  Effect  : TDropEffect;
begin
  Result := 0;

  for Effect := Low(TDropEffect) to High(TDropEffect) do
    if Effect in Effects then
      Result := Result or BitValues[Effect];
end;

procedure TlvkCustomDropTarget.GetData(const DataObject: IDataObject;
  const Stream: TStream);

  procedure GetGlobal(const DataObject: IDataObject; const Stream: TStream);
  var
    Data    : Pointer;
    Medium  : TStgMedium;
  begin
    FCurrentFormatEtc.tymed := TYMED_HGLOBAL;
    OleCheck(DataObject.GetData(FCurrentFormatEtc, Medium));
    try
      Data := GlobalLock(Medium.hGlobal);
      try
        Stream.WriteBuffer(Data^, GlobalSize(Medium.hGlobal));
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
    finally
      if not Assigned(Medium.unkForRelease) then
        GlobalFree(Medium.hGlobal);
    end;
  end;

  procedure GetFile(const DataObject: IDataObject; const Stream: TStream);
  var
    Medium      : TStgMedium;
    FileStream  : TFileStream;
  begin
    FCurrentFormatEtc.tymed := TYMED_FILE;
    OleCheck(DataObject.GetData(FCurrentFormatEtc, Medium));
    try
      FileStream := TFileStream.Create(Medium.lpszFileName, fmOpenRead or fmShareDenyNone);
      try
        Stream.CopyFrom(FileStream, 0);
      finally
        FileStream.Free;
      end;
    finally
      if not Assigned(Medium.unkForRelease) then
        DeleteFile(PChar(string(Medium.lpszFileName)));
    end;
  end;

  procedure GetIStream(const DataObject: IDataObject; const Stream: TStream);
  var
    Medium      : TStgMedium;
    StreamIntf  : IStream;
    OleStream   : TOleStream;
  begin
    FCurrentFormatEtc.tymed := TYMED_ISTREAM;
    OleCheck(DataObject.GetData(FCurrentFormatEtc, Medium));
    try
      StreamIntf := IStream(Medium.stm);
      OleStream := TOleStream.Create(StreamIntf);
      try
        Stream.CopyFrom(OleStream, 0);
      finally
        OleStream.Free;
      end;
    finally
      if Assigned(Medium.unkForRelease) then
        Pointer(StreamIntf) := nil;
    end;
  end;

  procedure GetIStorage(const DataObject: IDataObject; const Stream: TStream);
  begin
    raise EOleSysError.Create('Drop target does not support IStorage', E_UNEXPECTED, 0);
  end;

  procedure GetGDI(const DataObject: IDataObject; const Stream: TStream);
  begin
    raise EOleSysError.Create('Drop target does not support GDI', E_UNEXPECTED, 0);
  end;

  procedure GetMFPict(const DataObject: IDataObject; const Stream: TStream);
  begin
    raise EOleSysError.Create('Drop target does not support MFPICT', E_UNEXPECTED, 0);
  end;

  procedure GetENHMF(const DataObject: IDataObject; const Stream: TStream);
  begin
    raise EOleSysError.Create('Drop target does not support ENHMF', E_UNEXPECTED, 0);
  end;

begin
  if FCurrentFormatEtc.tymed and TYMED_HGLOBAL <> 0 then
    GetGlobal(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_FILE <> 0 then
    GetFile(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_ISTREAM <> 0 then
    GetIStream(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_ISTORAGE <> 0 then
    GetIStorage(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_GDI <> 0 then
    GetGDI(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_MFPICT <> 0 then
    GetMFPict(DataObject, Stream)
  else if FCurrentFormatEtc.tymed and TYMED_ENHMF <> 0 then
    GetENHMF(DataObject, Stream);
end;

function TlvkCustomDropTarget.GetTarget: TWinControl;
begin
  Result := SubClassTarget;
end;

function TlvkCustomDropTarget.GetTargetControl: TWinControl;
var
  Component : TComponent;
begin
  if SubClassTarget <> nil then
    Result := SubClassTarget
  else begin
    Component := Owner;
    while Assigned(Component) and (not (Component is TWinControl)) do
      Component := Component.Owner;

    if Assigned(Component) then
      Result := Component as TWinControl
    else
      Result := nil;
  end;
end;

procedure TlvkCustomDropTarget.Loaded;
begin
  inherited;

  Active := FSetActive;
end;

procedure TlvkCustomDropTarget.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (csDestroying in ComponentState) then
    inherited
  else if (Operation = opRemove) and (AComponent = GetTarget) then
  begin
    if not (csDesigning in ComponentState) then
      Deactivate;
    SetTarget(nil);
  end else
    inherited;
end;

function TlvkCustomDropTarget.ScreenToClient(const Point: TPoint): TPoint;
var
  Control : TWinControl;
begin
  Control := GetTargetControl;
  if Assigned(Control) then
    Result := Control.ScreenToClient(Point)
  else
    Result := Point;
end;

procedure TlvkCustomDropTarget.SetActive(const Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FSetActive := Value
  else begin
    if Value <> FActive then
    begin
      if not (csDesigning in ComponentState) then
      begin
        if Value then
          DoActivate
        else
          DoDeactivate;
      end;

      FActive := Value;
    end;
  end;
end;

procedure TlvkCustomDropTarget.SetFormats(
  const Value: TlvkDropTargetFormatCollection);
begin
  if Assigned(Value) then
    FFormats.Assign(Value)
  else
    FFormats.Clear;
end;

procedure TlvkCustomDropTarget.SetTarget(const Value: TWinControl);
begin
  if SubClassTarget <> Value then
  begin
    if FActive and (not (csDesigning in ComponentState)) then
      DoDeactivate;

    SubClassed := False;
    SubClassTarget := Value;
    SubClassed := Assigned(Value);

    if FActive and (not (csDesigning in ComponentState)) then
      DoActivate;
  end;
end;

function TlvkCustomDropTarget.TargetHandle: THandle;
var
  Control : TWinControl;
begin
  Control := GetTargetControl;
  if Assigned(Control) then
  begin
    if csDestroying in Control.ComponentState then
      Result := INVALID_HANDLE_VALUE
    else
      Result := Control.Handle;
  end else
    Result := INVALID_HANDLE_VALUE;
end;

{ TlvkDropTargetFormatCollection }

function TlvkDropTargetFormatCollection.Add: TlvkDropTargetFormat;
begin
  Result := inherited Add as TlvkDropTargetFormat;
end;

constructor TlvkDropTargetFormatCollection.Create(const AOwner: TPersistent);
begin
  inherited Create(AOwner, TlvkDropTargetFormat);
end;

function TlvkDropTargetFormatCollection.FindItemID(
  ID: Integer): TlvkDropTargetFormat;
begin
  Result := inherited FindItemID(ID) as TlvkDropTargetFormat;
end;

function TlvkDropTargetFormatCollection.GetItem(
  const Index: Integer): TlvkDropTargetFormat;
begin
  Result := inherited Items[Index] as TlvkDropTargetFormat;
end;

function TlvkDropTargetFormatCollection.Insert(
  Index: Integer): TlvkDropTargetFormat;
begin
  Result := inherited Insert(Index) as TlvkDropTargetFormat;
end;

procedure TlvkDropTargetFormatCollection.SetItem(const Index: Integer;
  const Value: TlvkDropTargetFormat);
begin
  inherited Items[Index] := Value;
end;

{ TlvkDropTargetFormat }

function TlvkDropTargetFormat.GetUsable: Boolean;
begin
  Result := Assigned(FOnEnter) and Assigned(FOnDrop) and FActive;
end;

{ TlvkStandardDropTarget }

function TlvkStandardDropTarget.AddFormat(const Format: TClipFormat;
  const EventHandler: TDropTargetDropEvent): TlvkDropTargetFormat;
begin
  Result := FDropTarget.Formats.Add;
  Result.Format := Format;
  Result.OnEnter := DropEnter;
  Result.OnDrag := DropDrag;
  Result.OnDrop := EventHandler;
  Result.OnLeave := DropLeave;
end;

constructor TlvkStandardDropTarget.Create(AOwner: TComponent);
var
  TempFormat  : TClipFormat;
begin
  inherited;

  FDropTarget := TlvkDropTarget.Create(Self);
  FDropTarget.Active := False;

  TempFormat := RegisterClipboardFormat(CFSTR_SHELLURL);
  FURLFormat := AddFormat(TempFormat, DropURL);

  TempFormat := RegisterClipboardFormat('HTML Format');
  FHTMLFormat := AddFormat(TempFormat, DropHTML);

  FTextFormat := AddFormat(CF_TEXT, DropText);

  FFilesFormat := AddFormat(CF_HDROP, DropFiles);
  FBitmapFormat := AddFormat(CF_BITMAP, DropBitmap);

  TempFormat := RegisterClipboardFormat('Rich Text Format');
  FRTFFormat := AddFormat(TempFormat, DropRTF);

  FUnicodeTextFormat := AddFormat(CF_UNICODETEXT, DropUnicodeText);
end;

destructor TlvkStandardDropTarget.Destroy;
begin
  FDropTarget.Free;

  inherited;
end;

procedure TlvkStandardDropTarget.DropBitmap(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
begin
  if Assigned(FOnDropBitmap) then
  begin
    TMemoryStream(Data).SaveToFile('c:\test.txt');
    Accept := True;
  end;
end;

procedure TlvkStandardDropTarget.DropDrag(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; var Effects: TDropEffects);
var
  AcceptDrop  : Boolean;
  Move        : Boolean;
begin
  if Assigned(FOnDrag) then
  begin
    AcceptDrop := True;
    Move := False;
    FOnDrag(Self, KeyState, Point, AcceptDrop, Move);
    if AcceptDrop then
    begin
      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    end else
      Effects := [];
  end else
    Effects := [deCopy, deLink];
end;

procedure TlvkStandardDropTarget.DropEnter(Sender: TObject;
  const DataObject: IDataObject; const KeyState: Integer;
  const Point: TPoint; var Effects: TDropEffects);
var
  AcceptDrop  : Boolean;
  Move        : Boolean;
begin
  if Assigned(FOnEnter) then
  begin
    Move := False;
    FOnEnter(Self, KeyState, Point, AcceptDrop, Move);
    if AcceptDrop then
    begin
      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    end else
      Effects := [];
  end else
    Effects := [deCopy, deLink];
end;

procedure TlvkStandardDropTarget.DropFiles(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  df        : TDropFiles;
  Files     : TStrings;
  c         : Char;
  wc        : WideChar;
  FileName  : string;
  Move      : Boolean;
begin
  if Assigned(FOnDropFiles) then
  begin
    Data.ReadBuffer(df, SizeOf(df));
    Files := TStringList.Create;
    try
      FileName := '';

      if df.fWide then
      begin
        repeat
          Data.ReadBuffer(wc, 2);
          if wc = #0 then
          begin
            if FileName <> '' then
            begin
              Files.Add(FileName);
              FileName := '';
            end else
              Break;
          end else
            FileName := FileName + wc;
        until False;
      end else begin
        repeat
          Data.ReadBuffer(c, 1);
          if c = #0 then
          begin
            if FileName <> '' then
            begin
              Files.Add(FileName);
              FileName := '';
            end else
              Break;
          end else
            FileName := FileName + c;
        until False;
      end;

      Move := False;
      FOnDropFiles(Self, KeyState, Point, Files, Move);

      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    finally
      Files.Free;
    end;

    Accept := True;
  end;
end;

procedure TlvkStandardDropTarget.DropHTML(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  HTML      : TStrings;
  Temp      : string;
  Index     : Integer;
  SourceURL : string;
  Move      : Boolean;
begin
  if Assigned(FOnDropHTML) then
  begin
    SetLength(Temp, Data.Size);
    if Data.Size > 0 then
      Data.ReadBuffer(Temp[1], Data.Size);

    SourceURL := '';

    Index := Pos('SourceURL:', Temp);
    if Index > 0 then
    begin
      Delete(Temp, 1, Index+9);

      Index := Pos(#13, Temp);
      if Index > 0 then
      begin
        SourceURL := Copy(Temp, 1, Index-1);
        Delete(Temp, 1, Index);
      end;
    end;

    Index := Pos('<!--StartFragment-->', Temp);
    if Index > 0 then
      Delete(Temp, 1, Index + 19);

    Index := Pos('<!--EndFragment-->', Temp);
    if Index > 0 then
      Temp := Copy(Temp, 1, Index-1);

    HTML := TStringList.Create;
    try
      HTML.Text := Temp;

      Move := False;
      FOnDropHTML(Self, KeyState, Point, SourceURL, HTML, Move);

      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    finally
      HTML.Free;
    end;
    Accept := True;
  end;
end;

procedure TlvkStandardDropTarget.DropLeave(Sender: TObject);
begin
  if Assigned(FOnLeave) then
    FOnLeave(Self);
end;

procedure TlvkStandardDropTarget.DropRTF(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  Strings : TStrings;
  Move    : Boolean;
begin
  if Assigned(FOnDropRTF) then
  begin
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(Data);
      Move := False;
      FOnDropRTF(Self, KeyState, Point, Strings, Move);

      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    finally
      Strings.Free;
    end;

    Accept := True;
  end;
end;

procedure TlvkStandardDropTarget.DropText(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  Strings : TStrings;
  Move    : Boolean;
begin
  if Assigned(FOnDropText) then
  begin
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(Data);
      Move := False;
      FOnDropText(Self, KeyState, Point, Strings, Move);

      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];
    finally
      Strings.Free;
    end;

    Accept := True;
  end;
end;

procedure TlvkStandardDropTarget.DropUnicodeText(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  ps    : PWideChar;
  Move  : Boolean;
begin
  if Assigned(FOnDropUnicodeText) then
  begin
    GetMem(ps, Data.Size);
    try
      if Data.Size > 0 then
        Data.ReadBuffer(ps^, Data.Size);
      Move := False;
      FOnDropUnicodeText(Self, KeyState, Point, ps, Move);

      if Move then
        Effects := [deMove, deLink]
      else
        Effects := [deCopy, deLink];

      Accept := True;
    finally
      FreeMem(ps);
    end;
  end;
end;

procedure TlvkStandardDropTarget.DropURL(Sender: TObject;
  const KeyState: Integer; const Point: TPoint; const Data: TStream;
  var Accept: Boolean; var Effects: TDropEffects);
var
  Temp  : string;
  Move  : Boolean;
begin
  if Assigned(FOnDropURL) then
  begin
    SetLength(Temp, Data.Size);
    if Data.Size > 0 then
      Data.ReadBuffer(Temp[1], Data.Size);
    SetLength(Temp, StrLen(PChar(Temp)));

    Move := False;
    if Temp <> '' then
      FOnDropURL(Self, KeyState, Point, Temp, Move);

    if Move then
      Effects := [deMove, deLink]
    else
      Effects := [deCopy, deLink];

    Accept := True;
  end;
end;

function TlvkStandardDropTarget.GetActive: Boolean;
begin
  Result := FDropTarget.Active;
end;

function TlvkStandardDropTarget.GetTarget: TWinControl;
begin
  Result := FDropTarget.Target;
end;

procedure TlvkStandardDropTarget.Loaded;
begin
  inherited;

  if FSetActive then
    SetActive(True);
end;

procedure TlvkStandardDropTarget.SetActive(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FSetActive := Value
  else
    FDropTarget.Active := Value;
end;

procedure TlvkStandardDropTarget.SetOnDropBitmap(
  const Value: TDropBitmapEvent);
begin
  FOnDropBitmap := Value;
  FBitmapFormat.Active := Assigned(FOnDropBitmap);
end;

procedure TlvkStandardDropTarget.SetOnDropFiles(
  const Value: TDropFilesEvent);
begin
  FOnDropFiles := Value;
  FFilesFormat.Active := Assigned(FOnDropFiles);
end;

procedure TlvkStandardDropTarget.SetOnDropHTML(
  const Value: TDropHTMLEvent);
begin
  FOnDropHTML := Value;
  FHTMLFormat.Active := Assigned(FOnDropHTML);
end;

procedure TlvkStandardDropTarget.SetOnDropRTF(const Value: TDropRTFEvent);
begin
  FOnDropRTF := Value;
  FRTFFormat.Active := Assigned(FOnDropRTF);
end;

procedure TlvkStandardDropTarget.SetOnDropText(
  const Value: TDropTextEvent);
begin
  FOnDropText := Value;
  FTextFormat.Active := Assigned(FOnDropText);
end;

procedure TlvkStandardDropTarget.SetOnDropUnicodeText(
  const Value: TDropUnicodeTextEvent);
begin
  FOnDropUnicodeText := Value;
  FUnicodeTextFormat.Active := Assigned(FOnDropUnicodeText);
end;

procedure TlvkStandardDropTarget.SetOnDropURL(const Value: TDropURLEvent);
begin
  FOnDropURL := Value;
  FURLFormat.Active := Assigned(FOnDropURL);
end;

procedure TlvkStandardDropTarget.SetTarget(const Value: TWinControl);
begin
  FDropTarget.Target := Value;
end;

initialization
  OleCheck(OleInitialize(nil));
finalization
  OleUninitialize;
end.
