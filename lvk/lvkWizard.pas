{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvkWizard;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkWizard.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Forms, Controls, Classes, SysUtils, ExtCtrls, Graphics, lvkVersion;

const
  DEFAULT_ITEM_ALLOW_FINISH   = True;
  DEFAULT_PANEL_ALLOW_FINISH  = True;
  DEFAULT_SHOW_HEADER         = False;
  DEFAULT_SHOW_WATERMARK      = True;
  DEFAULT_SHOW_DESIGN_TOOLS   = False;
  DEFAULT_SHOW_CAPTION        = True;

type
  TlvkCustomWizard = class;

  { Description:
      This event is used when the wizard has determined that it's allowed to
      change to a page. The OnBeforeShow event will be called just before the
      page is shown.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TBeforeShowEvent  = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel) of object;

  { Description:
      This event is used when the wizard is trying to determine if it's allowed
      to enter a specific page. Only enabled pages will be checked if they can
      be entered. If the current page can be exited, and the next enabled page
      can be entered, the wizard changes to the next enabled page.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TAllowEnterEvent  = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel; var AllowEnter: Boolean) of object;

  { Description:
      This event is used when the wizard has determined that it's allowed to
      change to a page. The OnAfterShow event will be called just after the
      page has been shown.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TAfterShowEvent   = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel) of object;

  { Description:
      This event is used when the wizard has determined that it's allowed to
      change to a page. The OnBeforeHide event will be called just before the
      current page is hidden.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TBeforeHideEvent  = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel) of object;

  { Description:
      This event is used when the wizard is trying to determine if it's allowed
      to enter a specific page. If the current page can be exited, and the next
      enabled page can be entered, the wizard changes to the next enabled page.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TAllowNextEvent   = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel; var AllowNext: Boolean) of object;

  { Description:
      This event is used when the wizard has determined that it's allowe do
      change to a page. The OnAfterHide event will be called just after the
      current page has been hidden.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TAfterHideEvent   = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel) of object;

  { Description:
      This event is used when the wizard is trying to determine if the user can
      finish the wizard at the current page. If the current page can be exited
      and the OnAllowFinish event reports that it's allowed to finish, the
      user can finish the wizard.
    See also:
      TBeforeShowEvent, TAllowEnterEvent, TAfterShowEvent, TBeforeHideEvent,
      TAllowNextEvent, TAfterHideEvent, TAllowFinishEvent
  }
  TAllowFinishEvent = procedure(const Sender: TObject; const PageIndex: Integer;
    const Page: TPanel; var AllowFinish: Boolean) of object;

  // <COMBINE TlvkCustomWizard.ShowButtons>
  TShowButton = (sbBack, sbNext, sbCancel);
  // <COMBINE TlvkCustomWizard.ShowButtons>
  TShowButtons = set of TShowButton;

  { Description:
      This is the collection item that is used to keep track of the pages in
      the wizard.
  }
  TWizardPage = class(TCollectionItem)
  private
    FOnBeforeShow   : TBeforeShowEvent;
    FOnAllowEnter   : TAllowEnterEvent;
    FOnAfterShow    : TAfterShowEvent;

    FOnBeforeHide   : TBeforeHideEvent;
    FOnAllowNext    : TAllowNextEvent;
    FOnAfterHide    : TAfterHideEvent;

    FOnAllowFinish  : TAllowFinishEvent;

    FPage           : TPanel;
    FAllowFinish    : Boolean;

    FShowHeader     : Boolean;
    FShowWatermark  : Boolean;
    FPageTitle      : string;
    FPageSubtitle   : string;

  protected
    procedure DoBeforeShow(const Sender: TObject; const PageIndex: Integer);
    procedure DoAllowEnter(const Sender: TObject; const PageIndex: Integer;
      var AllowEnter: Boolean);
    procedure DoAfterShow(const Sender: TObject; const PageIndex: Integer);

    procedure DoBeforeHide(const Sender: TObject; const PageIndex: Integer);
    procedure DoAllowNext(const Sender: TObject; const PageIndex: Integer;
      var AllowNext: Boolean);
    procedure DoAfterHide(const Sender: TObject; const PageIndex: Integer);

    procedure DoAllowFinish(const Sender: TObject; const PageIndex: Integer;
      var AllowFinish: Boolean);

    function GetDisplayName: string; override;
    function GetUsePage: Boolean;

  public
    constructor Create(Collection: TCollection); override;

  published
    { Description:
        This is the main wizard page property. Each page in the wizard has to
        be implemented as a TPanel component with the controls of the wizard
        page on it.
    }
    property Page: TPanel read FPage write FPage;

    // <ALIAS TBeforeShowEvent>
    property OnBeforeShow: TBeforeShowEvent read FOnBeforeShow
      write FOnBeforeShow;
    // <ALIAS TAllowEnterEvent>
    property OnAllowEnter: TAllowEnterEvent read FOnAllowEnter
      write FOnAllowEnter;
    // <ALIAS TAfterShowEvent>
    property OnAfterShow: TAfterShowEvent read FOnAfterShow write FOnAfterShow;
    // <ALIAS TBeforeHideEvent>
    property OnBeforeHide: TBeforeHideEvent read FOnBeforeHide
      write FOnBeforeHide;
    // <ALIAS TAllowNextEvent>
    property OnAllowNext: TAllowNextEvent read FOnAllowNext write FOnAllowNext;
    // <ALIAS TAfterHideEvent>
    property OnAfterHide: TAfterHideEvent read FOnAfterHide write FOnAfterHide;
    // <ALIAS TAllowFinishEvent>
    property OnAllowFinish: TAllowFinishEvent read FOnAllowFinish
      write FOnAllowFinish;

    { Description:
        This property can be used to tell the wizard wether the use is allowed
        to finish the wizard on this page or not. You can override the
        setting by using the OnAllowFinish event.
    }
    property AllowFinish: Boolean read FAllowFinish write FAllowFinish
      default DEFAULT_ITEM_ALLOW_FINISH;

    { Description:
        This property describes the topmost title of the page.
      See also:
        PageSubTitle
    }
    property PageTitle: string read FPageTitle write FPageTitle;

    { Description:
        Each page can also have a secondary title, a sub-title. This property
        describes that subtitle.
      See also:
        PageTitle
    }
    property PageSubTitle: string read FPageSubTitle write FPageSubTitle;

    { Description:
        You can control wether you want to show the watermark on the current
        page with this property.
      See also:
        ShowHeader
    }
    property ShowWatermark: Boolean read FShowWatermark write FShowWatermark
      default DEFAULT_SHOW_WATERMARK;

    { Description:
        You can control wether you want to show the header on the current
        page with this property.
      See also:
        ShowWatermark
    }
    property ShowHeader: Boolean read FShowHeader write FShowHeader
      default DEFAULT_SHOW_HEADER;
  end;

  { Description:
      This is the class used for the Pages property of the TlvkCustomWizard
      component.
    See also:
      TlvkCustomWizard.Pages
  }
  TWizardPageCollection = class(TOwnedCollection)
  private
    function GetItem(const Index: Integer): TWizardPage;
    procedure SetItem(const Index: Integer; const Value: TWizardPage);

  public
    // <EXTLINK borland://TOwnerCollection_Create>
    constructor Create(const Owner: TlvkCustomWizard);

    // <EXTLINK borland://TOwnerCollection_Add>
    function Add: TWizardPage;
    // <EXTLINK borland://TOwnerCollection_FindItemID>
    function FindItemID(ID: Integer): TWizardPage;
    // <EXTLINK borland://TOwnerCollection_Insert>
    function Insert(Index: Integer): TWizardPage;
    // <EXTLINK borland://TOwnerCollection_Items>
    property Items[const Index: Integer]: TWizardPage read GetItem write SetItem;
  end;

  { Description:
      This is the custom version of the wizard component. It implements all the
      code necessary for the wizard component to do its work properly.
    See also:
      TlvkWizard
  }
  TlvkCustomWizard = class(TComponent)
  private
    FPages            : TWizardPageCollection;
    FFinishCaption    : string;
    FCancelCaption    : string;
    FBackCaption      : string;
    FNextCaption      : string;
    FCaption          : string;
    FOnShowWizard     : TNotifyEvent;
    FOnCloseWizard    : TNotifyEvent;
    FCurrentPageIndex : Integer;
    FCurrentPage      : TPanel;
    FShowCaption      : Boolean;
    FWizardForm       : TForm;

    FWatermark        : TPicture;
    FHeaderGraphic    : TPicture;
    FShowDesignTools  : Boolean;
    FShowButtons    : TShowButtons;

    procedure GetMaxSize(out MaxWidth, MaxHeight: Integer);
    procedure SetPages(const Value: TWizardPageCollection);
    procedure SetHeaderGraphic(const Value: TPicture);
    procedure SetWatermark(const Value: TPicture);
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    function IsUsingCustomCaptions: Boolean;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    { Description:
        This property gives access to the individual pages in the wizard. The
        order in which the pages appear in this collection determines the
        order in which the pages will appear in when the wizard is executed.

        Note: Disabled pages will not be shown.
    }
    property Pages: TWizardPageCollection read FPages write SetPages;

    { Description:
        This property holds the caption of the Finish button. You can change
        this to create localized wizards.
      See also:
        FinishCaption, CancelCaption, BackCaption, NextCaption, Caption
    }
    property FinishCaption: string read FFinishCaption write FFinishCaption
      stored IsUsingCustomCaptions;

    { Description:
        This property holds the caption of the Cancel button. You can change
        this to create localized wizards.
      See also:
        FinishCaption, CancelCaption, BackCaption, NextCaption, Caption
    }
    property CancelCaption: string read FCancelCaption write FCancelCaption
      stored IsUsingCustomCaptions;

    { Description:
        This property holds the caption of the Back button. You can change
        this to create localized wizards.
      See also:
        FinishCaption, CancelCaption, BackCaption, NextCaption, Caption
    }
    property BackCaption: string read FBackCaption write FBackCaption
      stored IsUsingCustomCaptions;

    { Description:
        This property holds the caption of the Next button. You can change
        this to create localized wizards.
      See also:
        FinishCaption, CancelCaption, BackCaption, NextCaption, Caption
    }
    property NextCaption: string read FNextCaption write FNextCaption
      stored IsUsingCustomCaptions;

    { Description:
        This property holds the caption of the wizard window. You can change
        this to create localized wizards.
      See also:
        FinishCaption, CancelCaption, BackCaption, NextCaption, Caption,
        ShowCaption
    }
    property Caption: string read FCaption write FCaption;

    { Description:
        This event is called just before the wizard is shown. Use this to
        prepare or collect data to be used in the wizard.
      See also:
        OnCloseWizard
    }
    property OnShowWizard: TNotifyEvent read FOnShowWizard write FOnShowWizard;

    { Description:
        This event is called just after the wizard is hidden. Use this to
        clean up after the wizard.
      See also:
        OnShowWizard
    }
    property OnCloseWizard: TNotifyEvent read FOnCloseWizard write FOnCloseWizard;

    { Description:
        This property determines if the caption of the wizard dialog should be
        visible or not.
      See also:
        Caption
    }
    property ShowCaption: Boolean read FShowCaption write FShowCaption
      default DEFAULT_SHOW_CAPTION;

    { Description:
        The watermark is the graphic shown to the left of the wizard panels.
        This property controls what graphic to show, if any.
      See also:
        HeaderGraphic
    }
    property Watermark: TPicture read FWatermark write SetWatermark;

    { Description:
        The header is the graphic shown to the right in the top of the of the
        wizard panels. This property controls what graphic to show, if any.
      See also:
        Watermark
    }
    property HeaderGraphic: TPicture read FHeaderGraphic write SetHeaderGraphic;

    { Description:
        Set this property to True to have the wizard show panel sizes when used
        so you can more easily set the correct size of your panels.
    }
    property ShowDesignTools: Boolean read FShowDesignTools
      write FShowDesignTools default DEFAULT_SHOW_DESIGN_TOOLS;

    { Description:
        This property controls what buttons to show in the wizard.
    }
    property ShowButtons: TShowButtons read FShowButtons write FShowButtons;

    procedure DoShowWizard;
    procedure DoCloseWizard;
    procedure SetCurrentPage(const PageIndex: Integer; const Page: TPanel);
    function GetNextPage(const Direction: Integer): Integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        Call this method to execute the wizard. The wizard will start by
        showing the wizard dialog and moving to the first enabled page in the
        page collection.
      Returns:
        True if the user finished or executed the wizard, False if the user
        cancelled the wizard.
    }
    function Execute: Boolean;

    { Description:
        This method returns the index of the current page shown in the
        wizard. Pages are 0-based.
      See also:
        CurrentPage
    }
    function CurrentPageIndex: Integer;

    { Description:
        This method returns the panel that is the current page shown in the
        wizard.
      See also:
        CurrentPageIndex
    }
    function CurrentPage: TPanel;

    { Description:
        This function determines if the page with the specified index is
        available to switch to. Note that even if a page is available it does
        not necessarily mean the wizard can switch to that page. The
        OnAllowNext and OnAllowEnter events of the page might override this.
      See also:
        PageAvailable@TPanel
    }
    function PageAvailable(const PageIndex: Integer): Boolean; overload;

    { Description:
        This function determines if the page with the specified panel is
        available to switch to. Note that even if a page is available it does
        not necessarily mean the wizard can switch to that page. The
        OnAllowNext and OnAllowEnter events of the page might override this.
      See also:
        PageAvailable@Integer
    }
    function PageAvailable(const Page: TPanel): Boolean; overload;

    { Description:
        This method moves to the first available page in the wizard.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function FirstPage: Boolean;

    { Description:
        This method moves to the last available page in the wizard.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function LastPage: Boolean;

    { Description:
        This method moves to the next available page in the wizard.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function Next: Boolean;

    { Description:
        This method moves to the previous available page in the wizard.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function Back: Boolean;

    { Description:
        This method moves to the page with the specified index.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function GotoPage(const PageIndex: Integer): Boolean; overload;

    { Description:
        This method moves to the page with the specified panel.
      Returns:
        True if the wizard switched to a different page, False if not.
      See also:
        FirstPage, LastPage, Next, Back, GotoPage@Integer,
        GotoPage@TPanel
    }
    function GotoPage(const Page: TPanel): Boolean; overload;

    { Description:
        This method finishes the wizard, if possible. If the Force parameter
        is set to True, the wizard is finished regardless of wether or not
        it's allowed to exit the current page and/or finish the wizard.
      Returns:
        True if the wizard was finished, False if not.
      See also:
        Cancel
    }
    function Finish(const Force: Boolean=False): Boolean;

    { Description:
        This method cancels the wizard. Cancel is available in all cases
        so this function will always return True.
      Returns:
        True
      See also:
        Finish
    }
    function Cancel: Boolean;
  end;

  { Description:
      This is the main wizard component. You use this component by dropping
      a set of panels on a form, dropping a TlvkWizard component on the form
      and add all the panels to the Pages collection.

      See the example on how to use the wizard.
    Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  WizardForm: TfmWizard;
<B>begin</B>
  WizardForm := TfmWizard.Create(Application);
  <B>try</B>
    <B>if</B> WizardForm.WizComponent.Execute <B>then</B>
      ShowMessage('Success')
    <B>else</B>
      ShowMessage('Failure');
  <B>finally</B>
    WizardForm.Free;
  <B>end</B>;
<B>end</B>;
</CODE>
  }
  TlvkWizard = class(TlvkCustomWizard)
  published
    // <ALIAS TlvkCustomWizard.Pages>
    property Pages;
    // <ALIAS TlvkCustomWizard.FinishCaption>
    property FinishCaption;
    // <ALIAS TlvkCustomWizard.CancelCaption>
    property CancelCaption;
    // <ALIAS TlvkCustomWizard.BackCaption>
    property BackCaption;
    // <ALIAS TlvkCustomWizard.NextCaption>
    property NextCaption;
    // <ALIAS TlvkCustomWizard.Caption>
    property Caption;
    // <ALIAS TlvkCustomWizard.OnShowWizard>
    property OnShowWizard;
    // <ALIAS TlvkCustomWizard.OnCloseWizard>
    property OnCloseWizard;
    // <ALIAS TlvkCustomWizard.ShowCaption>
    property ShowCaption;
    // <ALIAS TlvkCustomWizard.Watermark>
    property Watermark;
    // <ALIAS TlvkCustomWizard.HeaderGraphic>
    property HeaderGraphic;
    // <ALIAS TlvkCustomWizard.ShowDesignTools>
    property ShowDesignTools;
    // <ALIAS TlvkCustomWizard.ShowButtons>
    property ShowButtons;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      This component is used to prepare wizard pages as panels
      at design time.
  }
  TlvkWizardPanel = class(TPanel)
  private
    FOnBeforeShow   : TBeforeShowEvent;
    FOnAllowEnter   : TAllowEnterEvent;
    FOnAfterShow    : TAfterShowEvent;
    FOnBeforeHide   : TBeforeHideEvent;
    FOnAllowNext    : TAllowNextEvent;
    FOnAfterHide    : TAfterHideEvent;
    FOnAllowFinish  : TAllowFinishEvent;
    FAllowFinish    : Boolean;
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TBeforeShowEvent>
    property OnBeforeShow: TBeforeShowEvent read FOnBeforeShow
      write FOnBeforeShow;
    // <ALIAS TAllowEnterEvent>
    property OnAllowEnter: TAllowEnterEvent read FOnAllowEnter
      write FOnAllowEnter;
    // <ALIAS TAfterShowEvent>
    property OnAfterShow: TAfterShowEvent read FOnAfterShow write FOnAfterShow;
    // <ALIAS TBeforeHideEvent>
    property OnBeforeHide: TBeforeHideEvent read FOnBeforeHide
      write FOnBeforeHide;
    // <ALIAS TAllowNextEvent>
    property OnAllowNext: TAllowNextEvent read FOnAllowNext write FOnAllowNext;
    // <ALIAS TAfterHideEvent>
    property OnAfterHide: TAfterHideEvent read FOnAfterHide write FOnAfterHide;
    // <ALIAS TAllowFinishEvent>
    property OnAllowFinish: TAllowFinishEvent read FOnAllowFinish
      write FOnAllowFinish;

    { Description:
        This property can be used to tell the wizard wether the use is allowed
        to finish the wizard on this page or not. You can override the
        setting by using the OnAllowFinish event.
    }
    property AllowFinish: Boolean read FAllowFinish write FAllowFinish
      default DEFAULT_PANEL_ALLOW_FINISH;

    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

resourcestring
  // Default caption for the Cancel button
  LVKWIZARD_DEFAULT_CANCEL_CAPTION  = '&Cancel';
  // Default caption for the Finish button
  LVKWIZARD_DEFAULT_FINISH_CAPTION  = '&Finish';
  // Default caption for the Back button
  LVKWIZARD_DEFAULT_BACK_CAPTION    = '&Back';
  // Default caption for the Next button
  LVKWIZARD_DEFAULT_NEXT_CAPTION    = '&Next';
  // Default caption for wizard dialog
  LVKWIZARD_DEFAULT_WIZARD__CAPTION = 'Unnamed wizard';

implementation

uses
  Math, lvkWizardFM;

{ TlvkCustomWizard }

function TlvkCustomWizard.Back: Boolean;
var
  Temp  : Integer;
begin
  Temp := GetNextPage(-1);
  if Temp <> -1 then
    Result := GotoPage(Temp)
  else
    Result := False;
end;

function TlvkCustomWizard.Cancel: Boolean;
begin
  Result := False;

  if Assigned(FWizardForm) then
    if TfmlvkWizard(FWizardForm).acCancel.Enabled then
      Result := TfmlvkWizard(FWizardForm).acCancel.Execute;
end;

constructor TlvkCustomWizard.Create(AOwner: TComponent);
begin
  inherited;

  FHeaderGraphic := TPicture.Create;
  FWatermark := TPicture.Create;

  FShowDesignTools := DEFAULT_SHOW_DESIGN_TOOLS;
  FShowCaption := DEFAULT_SHOW_CAPTION;
  
  FPages := TWizardPageCollection.Create(Self);
  FCancelCaption := LVKWIZARD_DEFAULT_CANCEL_CAPTION;
  FFinishCaption := LVKWIZARD_DEFAULT_FINISH_CAPTION;
  FBackCaption := LVKWIZARD_DEFAULT_BACK_CAPTION;
  FNextCaption := LVKWIZARD_DEFAULT_NEXT_CAPTION;
  FCaption := LVKWIZARD_DEFAULT_WIZARD__CAPTION;
  FShowButtons := [Low(TShowButton)..High(TShowButton)];
end;

function TlvkCustomWizard.CurrentPage: TPanel;
begin
  Result := FCurrentPage;
end;

function TlvkCustomWizard.CurrentPageIndex: Integer;
begin
  Result := FCurrentPageIndex;
end;

destructor TlvkCustomWizard.Destroy;
begin
  FreeAndNil(FPages);

  inherited;
end;

procedure TlvkCustomWizard.DoCloseWizard;
begin
  if Assigned(FOnCloseWizard) then
    FOnCloseWizard(Self);
end;

procedure TlvkCustomWizard.DoShowWizard;
begin
  if Assigned(FOnCloseWizard) then
    FOnShowWizard(Self);
end;

function TlvkCustomWizard.Execute: Boolean;
var
  fm                  : TfmlvkWizard;
  MaxWidth, MaxHeight : Integer;
begin
  GetMaxSize(MaxWidth, MaxHeight);
  fm := TfmlvkWizard.Create(Self);
  try
    if not FShowCaption then
      fm.BorderStyle := bsNone
    else
      fm.WizardPanel.BevelOuter := bvNone;
    FWizardForm := fm;

    // Assign captions
    fm.acCancel.Caption := FCancelCaption;
    fm.acFinish.Caption := FFinishCaption;
    fm.acBack.Caption := FBackCaption;
    fm.acNext.Caption := FNextCaption;
    fm.Caption := FCaption;

    // Resize
    if MaxWidth > (fm.paWizardArea.Width + fm.paWatermark.Width) then
      fm.Width := fm.Width + (MaxWidth - (fm.paWizardArea.Width + fm.paWatermark.Width));
    if MaxHeight > (fm.paWizardArea.Height + fm.paWizardHeader.Height) then
      fm.Height := fm.Height + (MaxHeight - (fm.paWizardArea.Height + fm.paWizardHeader.Height));

    // Copy bitmaps
    fm.imgWatermark.Picture := FWatermark;
    fm.imgHeader.Picture := FHeaderGraphic;

    if FShowDesignTools then
    begin
      fm.stWatermark.Show;
      fm.stWizard.Show;
      fm.stWizardArea.Show;
    end;

    if not (sbBack in FShowButtons) then
    begin
      fm.btBack.Hide;
      fm.btNext.SetBounds(fm.btBack.Left, fm.btBack.Top,
        fm.btBack.Width, fm.btBack.Height);
    end;

    if not (sbNext in FShowButtons) then
      fm.btNext.Hide;
    if not (sbCancel in FShowButtons) then
    begin
      fm.btCancel.Hide;
      fm.btFinish.SetBounds(fm.btCancel.Left, fm.btCancel.Top,
        fm.btCancel.Width, fm.btCancel.Height);
    end;
    // Execute
    fm.Wizard := TlvkWizard(Self);
    Result := fm.ShowModal = mrOK;
  finally
    FWizardForm := nil;
    fm.Free;
  end;
end;

function TlvkCustomWizard.Finish(const Force: Boolean): Boolean;
begin
  Result := False;

  if Assigned(FWizardForm) then
    if TfmlvkWizard(FWizardForm).acFinish.Enabled or Force then
      Result := TfmlvkWizard(FWizardForm).acFinish.Execute;
end;

function TlvkCustomWizard.FirstPage: Boolean;
var
  OldPageIndex  : Integer;
  Temp          : Integer;
begin
  OldPageIndex := FCurrentPageIndex;

  FCurrentPageIndex := -1;
  try
    Temp := GetNextPage(+1);
  finally
    FCurrentPageIndex := OldPageIndex;
  end;
  if Temp <> -1 then
    Result := GotoPage(Temp)
  else
    Result := False;
end;

procedure TlvkCustomWizard.GetMaxSize(out MaxWidth, MaxHeight: Integer);
var
  Page  : TWizardPage;
  Index : Integer;
  w, h  : Integer;
begin
  MaxWidth := 0;
  MaxHeight := 0;

  for Index := 0 to FPages.Count-1 do
  begin
    Page := FPages.Items[Index] as TWizardPage;

    if Assigned(Page.Page) then
    begin
      w := Page.Page.Width;
      h := Page.Page.Height;

{      if Page.ShowHeader then
        Inc(h, 56);

      if Page.ShowWatermark then
        Inc(w, 96); }

      MaxWidth := Max(MaxWidth, w);
      MaxHeight := Max(MaxHeight, h);
    end;
  end;
end;

function TlvkCustomWizard.GetNextPage(const Direction: Integer): Integer;
var
  AllowEnter  : Boolean;
begin
  Result := FCurrentPageIndex + Direction;

  while (Result >= 0) and (Result < Pages.Count) do
  begin
    if Pages.Items[Result].GetUsePage then
    begin
      Pages.Items[Result].DoAllowEnter(Self, Result, AllowEnter);
      if not AllowEnter then
        Result := -1;
      Exit;
    end;

    Inc(Result, Direction);
  end;

  Result := -1;
end;

function TlvkCustomWizard.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkCustomWizard.GotoPage(const Page: TPanel): Boolean;
var
  Index : Integer;
begin
  Result := False;

  if Assigned(Page) then
  begin
    for Index := 0 to FPages.Count-1 do
      if FPages.Items[Index].Page = Page then
      begin
        Result := GotoPage(Index);
        Break;
      end;
  end;
end;

function TlvkCustomWizard.GotoPage(const PageIndex: Integer): Boolean;
begin
  Result := False;

  if PageAvailable(PageIndex) then
    if Assigned(FWizardForm) then
      if (PageIndex >= 0) and (PageIndex < FPages.Count) then
        Result := TfmlvkWizard(FWizardForm).SetCurrentPage(PageIndex);
end;

function TlvkCustomWizard.IsUsingCustomCaptions: Boolean;
begin
  Result := (FFinishCaption <> LVKWIZARD_DEFAULT_FINISH_CAPTION) or
    (FCancelCaption <> LVKWIZARD_DEFAULT_CANCEL_CAPTION) or
    (FNextCaption <> LVKWIZARD_DEFAULT_NEXT_CAPTION) or
    (FBackCaption <> LVKWIZARD_DEFAULT_BACK_CAPTION);
end;

function TlvkCustomWizard.LastPage: Boolean;
var
  OldPageIndex  : Integer;
  Temp          : Integer;
begin
  OldPageIndex := FCurrentPageIndex;

  FCurrentPageIndex := FPages.Count;
  try
    Temp := GetNextPage(-1);
  finally
    FCurrentPageIndex := OldPageIndex;
  end;
  if Temp <> -1 then
    Result := GotoPage(Temp)
  else
    Result := False;
end;

function TlvkCustomWizard.Next: Boolean;
var
  Temp  : Integer;
begin
  Temp := GetNextPage(+1);
  if Temp <> -1 then
    Result := GotoPage(Temp)
  else
    Result := False;
end;

procedure TlvkCustomWizard.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if not Assigned(FPages) then
    Exit;
    
  if Operation = opRemove then
  begin
    Index := 0;
    while Index < FPages.Count do
    begin
      if FPages.Items[Index].Page = AComponent then
        FPages.Items[Index].Page := nil
      else
        Inc(Index);
    end;
  end;
end;

function TlvkCustomWizard.PageAvailable(const Page: TPanel): Boolean;
var
  Index : Integer;
begin
  Result := False;

  if Assigned(Page) then
  begin
    for Index := 0 to FPages.Count-1 do
      if FPages.Items[Index].Page = Page then
      begin
        Result := PageAvailable(Index);
        Break;
      end;
  end;
end;

function TlvkCustomWizard.PageAvailable(const PageIndex: Integer): Boolean;
begin
  Result := False;

  if (PageIndex >= 0) and (PageIndex < FPages.Count) then
    if Assigned(FWizardForm) then
      Result := FPages.Items[PageIndex].GetUsePage;
end;

procedure TlvkCustomWizard.SetCurrentPage(const PageIndex: Integer; const Page: TPanel);
begin
  FCurrentPageIndex := PageIndex;
  FCurrentPage := Page;
end;

procedure TlvkCustomWizard.SetHeaderGraphic(const Value: TPicture);
begin
  FHeaderGraphic.Assign(Value);
end;

procedure TlvkCustomWizard.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomWizard.SetPages(const Value: TWizardPageCollection);
begin
  if Assigned(Value) then
    FPages.Assign(Value)
  else
    FPages.Clear;
end;

procedure TlvkCustomWizard.SetWatermark(const Value: TPicture);
begin
  FWatermark.Assign(Value);
end;

{ TWizardPageCollection }

function TWizardPageCollection.Add: TWizardPage;
begin
  Result := inherited Add as TWizardPage;
end;

constructor TWizardPageCollection.Create(const Owner: TlvkCustomWizard);
begin
  inherited Create(Owner, TWizardPage);
end;

function TWizardPageCollection.FindItemID(ID: Integer): TWizardPage;
begin
  Result := inherited FindItemID(ID) as TWizardPage;
end;

function TWizardPageCollection.GetItem(const Index: Integer): TWizardPage;
begin
  Result := inherited GetItem(Index) as TWizardPage;
end;

function TWizardPageCollection.Insert(Index: Integer): TWizardPage;
begin
  Result := inherited Insert(Index) as TWizardPage;
end;

procedure TWizardPageCollection.SetItem(const Index: Integer;
  const Value: TWizardPage);
begin
  inherited Items[Index] := Value;
end;

{ TWizardPage }

constructor TWizardPage.Create(Collection: TCollection);
begin
  inherited;

  FAllowFinish := True;
  FShowWatermark := (Collection.Count = 1);
  FShowHeader := not FShowWatermark;
  FPageTitle := 'No title';
  FPageSubtitle := 'No subtitle';
end;

procedure TWizardPage.DoAfterHide(const Sender: TObject; const PageIndex: Integer);
begin
  if Assigned(FOnAfterHide) then
    FOnAfterHide(Sender, PageIndex, FPage);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnAfterHide) then
    TlvkWizardPanel(FPage).FOnAfterHide(Sender, PageIndex, FPage);
end;

procedure TWizardPage.DoAfterShow(const Sender: TObject; const PageIndex: Integer);
begin
  if Assigned(FOnAfterShow) then
    FOnAfterShow(Sender, PageIndex, FPage);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnAfterShow) then
    TlvkWizardPanel(FPage).FOnAfterShow(Sender, PageIndex, FPage);
end;

procedure TWizardPage.DoAllowEnter(const Sender: TObject;
  const PageIndex: Integer; var AllowEnter: Boolean);
begin
  AllowEnter := True;

  if Assigned(FOnAllowEnter) then
    FOnAllowEnter(Sender, PageIndex, FPage, AllowEnter);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnAllowEnter) then
    TlvkWizardPanel(FPage).FOnAllowEnter(Sender, PageIndex, FPage, AllowEnter);
end;

procedure TWizardPage.DoAllowNext(const Sender: TObject;
  const PageIndex: Integer; var AllowNext: Boolean);
begin
  AllowNext := True;

  if Assigned(FOnAllowNext) then
    FOnAllowNext(Sender, PageIndex, FPage, AllowNext);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnAllowNext) then
    TlvkWizardPanel(FPage).FOnAllowNext(Sender, PageIndex, FPage, AllowNext);
end;

procedure TWizardPage.DoAllowFinish(const Sender: TObject;
  const PageIndex: Integer; var AllowFinish: Boolean);
begin
  AllowFinish := FAllowFinish;
  if Assigned(FPage) and (FPage is TlvkWizardPanel) then
    AllowFinish := AllowFinish and TlvkWizardPanel(FPage).AllowFinish;

  if Assigned(FOnAllowFinish) then
    FOnAllowFinish(Sender, PageIndex, FPage, AllowFinish);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnAllowFinish) then
    TlvkWizardPanel(FPage).FOnAllowFinish(Sender, PageIndex, FPage, AllowFinish);
end;

procedure TWizardPage.DoBeforeHide(const Sender: TObject;
  const PageIndex: Integer);
begin
  if Assigned(FOnBeforeHide) then
    FOnBeforeHide(Sender, PageIndex, FPage);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnBeforeHide) then
    TlvkWizardPanel(FPage).FOnBeforeHide(Sender, PageIndex, FPage);
end;

procedure TWizardPage.DoBeforeShow(const Sender: TObject; const PageIndex: Integer);
begin
  if Assigned(FOnBeforeShow) then
    FOnBeforeShow(Sender, PageIndex, FPage);
  if Assigned(FPage) and (FPage is TlvkWizardPanel) and Assigned(TlvkWizardPanel(FPage).FOnBeforeShow) then
    TlvkWizardPanel(FPage).FOnBeforeShow(Sender, PageIndex, FPage);
end;

function TWizardPage.GetDisplayName: string;
begin
  if Assigned(FPage) then
    Result := 'Page: ' + FPage.Name
  else
    Result := 'Page: <unassigned>';
end;

function TWizardPage.GetUsePage: Boolean;
begin
  Result := Assigned(FPage) and FPage.Enabled;
end;

{ TlvkWizardPanel }

constructor TlvkWizardPanel.Create(AOwner: TComponent);
begin
  inherited;

  FAllowFinish := DEFAULT_PANEL_ALLOW_FINISH;
end;

function TlvkWizardPanel.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkWizardPanel.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

end.
