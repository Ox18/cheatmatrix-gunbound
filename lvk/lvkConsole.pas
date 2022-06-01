{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{* Fixes contributed by Goodge                                                *}
{* Fixes contributed by various members of #Delphi on Efnet                   *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the lvkConsole component, for showing a running log of
    messages at runtime.
}
unit lvkConsole;

{ TODO 1 -oLVK -cEnhancement : Sub-word selection }
{ TODO 2 -oLVK -cEnhancement : Add named font and color styles, replace color with font }
{ TODO 2 -oLVK -cEnhancement : Add named glyphs }
{ TODO 3 -oLVK -cEnhancement : Better wrapping-algorithm for long words }
{ TODO 3 -oLVK -cEnhancement : New column types centred and right }

// $Author: Lasse V. Karlsen $
// $Revision: 23 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkConsole.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Classes, SysUtils, Graphics, Forms, Messages, Controls,
  StdCtrls, ExtCtrls, ImgList, Clipbrd, ShellApi, lvkControls;

const
  CM_INVALIDATE = WM_USER + 1;

type
  TPartType = (ptWord, ptAddFontStyle, ptRemoveFontStyle, ptAddIndent,
    ptRemoveIndent, ptLineBreak,
    ptAddLink, ptRemoveLink, ptWhitespace, ptImage, ptColumn,
    ptHorisontalLine, ptAddFont, ptRemoveFont, ptTimestamp,
    ptEnterPRE, ptLeavePRE);

  TEdgeType = (etNone, etClient, etStatic);

  TIndentType = (itNone, itHanging, itTimestamp);

  TLinkType = (ltURL, ltEvent);

const
  DEFAULT_SMOOTH_SCROLLING  = False;
  DEFAULT_MAX_SCROLLSPEED   = 20;
  DEFAULT_ACCELERATE_RANGE  = (DEFAULT_MAX_SCROLLSPEED *
    (DEFAULT_MAX_SCROLLSPEED+1)) div 2;
  DEFAULT_DECELERATE_RANGE  = DEFAULT_ACCELERATE_RANGE;
  DEFAULT_REMOVE_LINES      = False;

  DEFAULT_INDENT_TYPE       = itHanging;
  DEFAULT_INDENT_AMOUNT     = 10;

  DEFAULT_EDGE              = etClient;
  DEFAULT_LINE_SPACING      = 0;
  DEFAULT_LEFT_MARGIN       = 2;
  DEFAULT_RIGHT_MARGIN      = 2;
  DEFAULT_COLOR             = clWindow;
  DEFAULT_LINK_COLOR        = clBlue;
  DEFAULT_LIMIT             = 500;
  DEFAULT_BLINK_INTERVAL    = 500;

  DEFAULT_TIMESTAMP_EMPTY   = False;
  DEFAULT_TIMESTAMP_VISIBLE = False;
  DEFAULT_STORE_TEXT        = False;

type
  PPartNode = ^TPartNode;
  PMessageNode = ^TMessageNode;
  TPartNode = record
    Message   : PMessageNode;
    Next      : PPartNode;
    Blink     : Boolean;
    Selected  : Boolean;
    NoSelect  : Boolean;
    LineNo    : Integer;

    Width     : Integer;
    Height    : Integer;
  case PartType: TPartType of
    ptTimestamp:
      ();

    ptWhitespace:
      ();

    ptColumn:
      (ColumnSpot: Integer);

    ptHorisontalLine:
      (WidthPercent: Integer);

    ptWord:
      (Word: PChar);

    ptEnterPRE:
      ();

    ptLeavePRE:
      ();

    ptAddFontStyle:
      (AddFontStyle: TFontStyles);

    ptRemoveFontStyle:
      (RemoveFontStyle: TFontStyles);

    ptAddIndent, ptRemoveIndent:
      ();

    ptAddFont:
      (NewFontName: PChar;
       NewFontSize: Integer;
       NewFontDeltaSize: Integer;
       NewForegroundColor: TColor;
       NewBackgroundColor: TColor);

    ptRemoveFont:
      ();

    ptAddLink:
      (LinkName, LinkParams, LinkHint: PChar;
       LinkType: TLinkType;
       LinkEffect, LinkHoverEffect: Boolean);

    ptRemoveLink:
      ();

    ptImage:
      (ImageIndex: Integer);
  end;

  TMessageNode = record
    Text        : string;
    Timestamp   : TDateTime;
    Parts       : PPartNode;
    Width       : Integer;
    MaxWidth    : Integer;
    Height      : Integer;
    Next        : PMessageNode;
    Prev        : PMessageNode;
    LineHeights : array of Integer;
  end;

  PLinkNode = ^TLinkNode;
  TLinkNode = record
    Rect  : TRect;
    Part  : PPartNode;

    Next  : PLinkNode;
  end;

  TLinkInformation = record
    Valid   : Boolean;
    Name    : string;
    Params  : string;
    Hint    : string;
  end;

  TConsoleOption = (coBlinking, coSelection, coAutoCopy, coAutoOpenURLLinks);
  TConsoleOptions = set of TConsoleOption;

  TLinkEvent = procedure(const Sender: TObject; const Name, Params, Hint: string) of object;
  TSelectEvent = procedure(const Sender: TObject; const SelectedText: string) of object;
  TTextAddedEvent = procedure(const Sender: TObject; const LineCount: Integer;
    const ScrolledUp: Boolean) of object;

  TlvkCustomConsole = class;

  { Description:
      This class is used for the Scrolling property of the TlvkCustomConsole
      component.
    See also:
      TlvkCustomConsole, TlvkCustomConsole.Scrolling
  }
  TScrollingData = class(TPersistent)
  private
    FConsole  : TlvkCustomConsole;
    function GetAccelerationRange: Integer;
    function GetDecelerationRange: Integer;
    function GetMaxScrollSpeed: Integer;
    function GetRemoveLinesWhileScrolledUp: Boolean;
    function GetSmoothScrolling: Boolean;
    procedure SetAccelerationRange(const Value: Integer);
    procedure SetDecelerationRange(const Value: Integer);
    procedure SetMaxScrollSpeed(const Value: Integer);
    procedure SetRemoveLinesWhileScrolledUp(const Value: Boolean);
    procedure SetSmoothScrolling(const Value: Boolean);

  public
    constructor Create(const Console: TlvkCustomConsole);
    procedure Assign(Source: TPersistent); override;

  published
    { Description
        By setting the SmoothScrolling property to True, each line added to
        the console will scroll into view one pixel at a time instead of
        appearing as a whole new line in one go.
    }
    property SmoothScrolling: Boolean read GetSmoothScrolling
      write SetSmoothScrolling default DEFAULT_SMOOTH_SCROLLING;

    { Description:
        This property controls how fast the smooth scrolling can go. Set it to
        1 and you got no acceleration. If you set it above 1, the scrolling
        will accelerate from 1 pixel per frame, up to the given maximum.
      See also:
        AccelerationRange, DecelerationRange
    }
    property MaxScrollSpeed: Integer read GetMaxScrollSpeed
      write SetMaxScrollSpeed default DEFAULT_MAX_SCROLLSPEED;

    { Description:
        When you use smooth scrolling and acceleration, you can also control
        wether the scrolling should start out at max speed, or accelerate
        towards max speed. If the distance to scroll is more than
        AccelerationRange, it will start out at max speed, if it's less,
        it will start at 1 and accelerate.

        Setting AccelerationRange to 0 disables Acceleration and will always
        start out at max speed.
      See also:
        DecelerationRange, MaxScrollSpeed
    }
    property AccelerationRange: Integer read GetAccelerationRange
      write SetAccelerationRange default DEFAULT_ACCELERATE_RANGE;

    { Description:
        When you use smooth scrolling and acceleration, you can also control
        wether the scrolling should decelerate when it comes close to the
        scroll target. Set this property to the number of pixels that it
        should start decelerate within. If you set it to 100, it means that
        once the scrolling has 100 or less pixels to scroll, it will decrease
        the number of pixels it scrolls by per frame, one pixel per frame,
        down to the minimum of 1.
      See also:
        AccelerationRange, MaxScrollSpeed
    }
    property DecelerationRange: Integer read GetDecelerationRange
      write SetDecelerationRange default DEFAULT_DECELERATE_RANGE;

    { Description:
        While you have scrolled up in the console to show previous text, new
        text can be added to the bottom of the console. This should normally
        not disturb your view, unless the messages you have on your screen
        starts to fall over the limit on the number of messages to keep in
        the console. If you set this property to True, messages will not
        become deleted while you have scrolled up in the console. The limit
        will only be used while you have scrolled to the very bottom of the
        console.
      See also:
        TlvkCustomConsole.Limit
    }
    property RemoveLinesWhileScrolledUp: Boolean read GetRemoveLinesWhileScrolledUp write
      SetRemoveLinesWhileScrolledUp default DEFAULT_REMOVE_LINES;
  end;

  { Description:
      This class is used for the Link property of the TlvkCustomConsole
      component.
    See also:
      TlvkCustomConsole, TlvkCustomConsole.Links
  }
  TLinkData = class(TPersistent)
  private
    FConsole  : TlvkCustomConsole;
    function GetFont: TFont;
    function GetHoverFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetHoverFont(const Value: TFont);
    function GetAutoParseEffect: Boolean;
    function GetAutoParseHoverEffect: Boolean;
    procedure SetAutoParseEffect(const Value: Boolean);
    procedure SetAutoParseHoverEffect(const Value: Boolean);

  public
    constructor Create(const Console: TlvkCustomConsole);
    procedure Assign(Source: TPersistent); override;

  published
    { Description:
        Change this font property to change how links show up in the console.
      See also:
        HoverFont
    }
    property Font: TFont read GetFont write SetFont;

    { Description:
        Change this font property to change how links show up when you hover
        the mouse pointer above it.

        Note: If you select a different font from the LinkFont, make sure that
          the new font takes more space than the link font. Otherwise some
          "funny" problems will show up when you move the mouse pointer over
          the area near the borders of the link.
      See also:
        Font
    }
    property HoverFont: TFont read GetHoverFont write SetHoverFont;

    { Description:
        Set this to False if you don't want auto-parsed links to get their
        own effects, like colors and styles.
      See also:
        AutoParseHoverEffect
    }
    property AutoParseEffect: Boolean read GetAutoParseEffect
      write SetAutoParseEffect default True;

    { Description:
        Set this to False if you don't want auto-parsed links to change their
        appearance when the mouse hovers over it.
      See also:
        AutoParseEffect
    }
    property AutoParseHoverEffect: Boolean read GetAutoParseHoverEffect
      write SetAutoParseHoverEffect default True;
  end;

  TlvkConsoleWhitespace = (cwsCollapse, cwsCollapseOutsidePRE, cwsLeave);
  
  { Description:
      This is the custom version of the console component. It implements all the
      code necessary for the console component to work properly.
    See also:
      TlvkConsole
  }
  TlvkCustomConsole = class(TlvkCustomControl)
  private
    FStoreText                  : Boolean;
    FLineCount                  : Integer;
    FLineSpacing                : Integer;
    FScrollPos                  : Integer;
    FScrollMax                  : Integer;
    FUpdateLevel                : Integer;
    FUpdated                    : Boolean;
    FMessages                   : PMessageNode;
    FLastMessage                : PMessageNode;
    FLeftMargin                 : Integer;
    FRightMargin                : Integer;
    FPreviousHint               : string;
    FLinkFont                   : TFont;
    FHoverFont                  : TFont;
    FPreFont                    : TFont;

    FAutoParseEffect            : Boolean;
    FAutoParseHoverEffect       : Boolean;

    FTimestampVisible           : Boolean;
    FTimestampFormat            : string;
    FIndentType                 : TIndentType;
    FIndentAmount               : Integer;
    FTimestampEmptyLines        : Boolean;

    FCurrentLink                : PPartNode;
    FLinks                      : PLinkNode;
    FOnLink                     : TLinkEvent;
    FOnSelect                   : TSelectEvent;
    FBlinking                   : Boolean;
    FBlinkTimer                 : TTimer;
    FBlinkingVisible            : Boolean;
    FImageList                  : TImageList;

    FSelectMoved                : Boolean;
    FSelecting                  : Boolean;
    FSelectX1                   : Integer;
    FSelectY1                   : Integer;
    FSelectX2                   : Integer;
    FSelectY2                   : Integer;

    FSelectPart                 : Integer;
    FDesignText                 : TStringList;
    FOptions                    : TConsoleOptions;
    FEdge                       : TEdgeType;

    FLimit                      : Integer;
    FRemoveLinesWhileScrolledUp : Boolean;
    FCount                      : Integer;
    FSmoothScrolling            : Boolean;
    FScrollSpeed                : Integer;
    FMaxScrollSpeed             : Integer;
    FAccelerationRange          : Integer;
    FDecelerationRange          : Integer;
    FScrollDestination          : Integer;
    FOnTextAdded                : TTextAddedEvent;
    FScrollingData              : TScrollingData;
    FLinkData                   : TLinkData;
    FWhitespace                 : TlvkConsoleWhitespace;

    procedure DestroyParts(const Part: PPartNode);
    procedure RemoveOverLimit;
    procedure DoLink(const LinkNode: PPartNode);
    procedure DoTextAdded;
    function ParseText(const Text: string;
      const MessageNode: PMessageNode): PPartNode;
    function FormatTimestamp(const Timestamp: TDateTime): string;
    procedure RenderMessage(const Message: PMessageNode;
      const Y: Integer; const DoPaint: Boolean);
    procedure UpdateScrollBar;
    procedure SetOptions(const Value: TConsoleOptions);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);

    procedure CreateLink(const Rect: TRect; const Part: PPartNode);
    procedure ClearLinks;
    procedure SetCurrentLink(const Value: PPartNode);
    function GetBlinkInterval: Integer;
    procedure SetBlinkInterval(const Value: Integer);
    procedure OnBlink(Sender: TObject);
    procedure SetImageList(const Value: TImageList);
    procedure FontChange(Sender: TObject);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure StartSelect(const X, Y: Integer);
    procedure ContinueSelect(const X, Y: Integer);
    procedure EndSelect(const X, Y: Integer);
    procedure SelectText;
    procedure SetDesignText(const Value: TStringList);

    procedure DesignTextChange(Sender: TObject);
    procedure SetLimit(const Value: Integer);
    function GetLinkInformation: TLinkInformation;
    procedure SetLineSpacing(const Value: Integer);

    procedure OnInvalidate(var Msg: TMessage); message CM_INVALIDATE;
    procedure SetEdge(const Value: TEdgeType);
    procedure SetTimestampFormat(const Value: string);
    procedure SetIndentAmount(const Value: Integer);
    procedure SetIndentType(const Value: TIndentType);
    procedure SetTimestampEmptyLines(const Value: Boolean);
    procedure SetTimestampVisible(const Value: Boolean);
    procedure SetScrollPos(const Value: Integer);
    function GetMessageCount: Integer;
    function GetMessages(Index: Integer): PMessageNode;
    procedure SetScrollingData(const Value: TScrollingData);
    procedure SetLinkData(const Value: TLinkData);
    function GetScrollPos: Integer;
    procedure SetPreFont(const Value: TFont);

  protected
    procedure Reformat; virtual;
    property CurrentLink: PPartNode read FCurrentLink write SetCurrentLink;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;

    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
    procedure FontChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;

    { Description
        If you wish to add image symbols to the console, you need to link in
        a imagelist component that contains the images you wish to use.
    }
    property ImageList: TImageList read FImageList write SetImageList;

    { Description
        Whenever a clickable link is clicked, an OnClick event will be fired.
      Parameters:
        Name - The name attribute from the a tag.
        Params - The params attribute from the a tag.
        Hint - The hint attribute from the a tag.
    }
    property OnLink: TLinkEvent read FOnLink write FOnLink;

    { Description
        When you select a part of the text in the console, an OnSelect
        event will be fired with the selected text as a parameter.
      Parameters:
        SelectedText - The selected text.
    }
    property OnSelect: TSelectEvent read FOnSelect write FOnSelect;

    { Description:
        This event handler will be called if new text is added to the console
        while the user has scrolled the console upwards, so that new
        messages won't appear in the current view. You can use this event
        to notify the user (say, with a Beep), that new text has arrived.
    }
    property OnTextAdded: TTextAddedEvent
      read FOnTextAdded write FOnTextAdded;

    { Description
        How often the <blink>text</blink> text blocks should blink in
        milliseconds. You will also need to enable the blinking feature
        for this to work.
    }
    property BlinkInterval: Integer read GetBlinkInterval
      write SetBlinkInterval default DEFAULT_BLINK_INTERVAL;

    { Description:
        This property gives you the ability to select what options to use with
        the console. You can enable/disable the following options:

          * coBlinking - Enabling this will enable blinking text.
          * coSelection - Enabling this will allow the user to select text in
            the console.
          * coAutoCopy - If you select text from the console (selection must
            be enabled) and you have no event handler, the text will be
            copied to the clipboard if you specify this option.
    }
    property Options: TConsoleOptions read FOptions write SetOptions;

    { Description
        The Edge property determines if the console component should be drawn
        with a lowered edge or not.
    }
    property Edge: TEdgeType read FEdge write SetEdge default DEFAULT_EDGE;

    { Description:
        This property is only used design-time and allows you to add a lot of
        text to the console so you can see how your properties affect the
        console and its appearance.
    }
    property DesignText: TStringList read FDesignText write SetDesignText;

    { Description:
        This property determines how many pixels to use between each line
        in order to space them out. This property can only hold a positive
        value in addition to zero which means no extra space between the
        lines.
    }
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing
      default DEFAULT_LINE_SPACING;

    { Description:
        The LeftMargin property determines how many pixels will be added to the
        left of the console in order to push the console text out from the very
        edge.
    }
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin
      default DEFAULT_LEFT_MARGIN;

    { Description:
        The RightMargin property determines how many pixels will be added to the
        right of the console in order to avoid having the console text butting
        into the scrollbar.
    }
    property RightMargin: Integer read FRightMargin write SetRightMargin
      default DEFAULT_RIGHT_MARGIN;

    { Description:
        This property specifies how many lines of messages to store in the
        console component. A limit of 0 means no limit, any positive value
        means that once that number of messages has been added to the console,
        the oldest messages will be removed to keep the number of lines at or
        below the limit.
      See also:
        TScrollingData.RemoveLinesWhileScrolledUp
    }
    property Limit: Integer read FLimit write SetLimit default DEFAULT_LIMIT;

    { Description:
        The LinkInformation property returns information about the link that
        the cursor is hovering over. The Valid member of the returned record
        will have the value True if the mouse is hovering over a link, False if
        not.
    }
    property LinkInformation: TLinkInformation read GetLinkInformation;

    { Description:
        This method sets the timestamp format to use when adding a timestamp
        to console messages.

        See the FormatDateTime function in the Delphi help for information
        about the format for this property.
      See also:
        TimestampVisible, TimestampEmptyLines
    }
    property TimestampFormat: string read FTimestampFormat
      write SetTimestampFormat;

    { Description:
        This sets the type of indentation to use. This is only used for lines
        that wrap down to two or more lines, or if you use the <hr> or <br>
        tags.

        The available indentation types are:
          * itNone - No indentation, all lines will follow the left margin.
          * itHanging - Text will be indented to a fixed amount on the second
            and higher lines. The amount (number of pixels) to indent is
            stored in the IndentAmount property
          * itTimestamp - Text will be indented so that the timestamp will
            appear to be in a column by itself to the left.
      See also:
        IndentAmount
    }
    property IndentType: TIndentType read FIndentType write SetIndentType
      default DEFAULT_INDENT_TYPE;

    { Description:
        If you set the IndentType to itHanging, this property determines how
        many pixels to indent the second and higher line.
      See also:
        IndentType
    }
    property IndentAmount: Integer read FIndentAmount write SetIndentAmount
      default DEFAULT_INDENT_AMOUNT;

    { Description:
        If you opt to show a timestamp, you can choose wether a blank line
        (a blank string added to the console) should get a timestamp or not.
      See also:
        TimestampFormat, TimestampVisible
    }
    property TimestampEmptyLines: Boolean read FTimestampEmptyLines
      write SetTimestampEmptyLines default DEFAULT_TIMESTAMP_EMPTY;

    { Description:
        Set this property to True to auto-show timestamps for a message. Set it
        to False to hide it. The format for the timestamp text is stored in the
        TimestampFormat property.
      See also:
        TimestampFormat, TimestampVisible
    }
    property TimestampVisible: Boolean read FTimestampVisible
      write SetTimestampVisible default DEFAULT_TIMESTAMP_VISIBLE;

    { Description:
        Set the StoreText property to True, and the console will store all
        text added to it in a message node. You can reach this through the
        Text member of a message node.
    }
    property StoreText: Boolean read FStoreText write FStoreText
      default DEFAULT_STORE_TEXT;

    { Description:
        This property contains settings related to scrolling and how it
        behaves.
    }
    property Scrolling: TScrollingData read FScrollingData
      write SetScrollingData;

    { Description:
        This property contains settings related to links and how they are
        displayed.
    }
    property Links: TLinkData read FLinkData write SetLinkData;

    { Description:
        Change this property to change how the console handles whitespace
        characters. The available values are:

          * cwsCollapse - All whitespace will be collapsed down to just one
            space character. This includes multiple space characters and
            linebreak characters. This is the default setting to handle
            backwards compatibility.
          * cwsCollapseOutsidePRE - All whitespace not wrapped in \<pre\>
            tags will be collapsed as with cwsCollapse.
          * cwsLeave - All whitespace will be left as-is. This means linebreak
            characters will work as \<br\> and multiple spaces will all
            be added.
    }
    property Whitespace: TlvkConsoleWhitespace read FWhitespace
      write FWhitespace default cwsCollapseOutsidePRE;

    { Description:
        If you wrap text in the \<pre\> tag, it will be rendered with this
        font. Typically you will use a monospaced font here (default setting)
        so that text will line up properly.
    }
    property PreFont: TFont read FPreFont write SetPreFont;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description
        The Clear method clears the console and removes all the messages listed
        in it.
    }
    procedure Clear;

    { Description
        Call this method before you start adding a lot of messages to avoid
        updating the screen between each message. BeginUpdate/EndUpdate is
        cumulative so if you call BeginUpdate twice you'll need to call
        EndUpdate twice as well.
      See also:
        EndUpdate
    }
    procedure BeginUpdate;

    { Description
        Call this method when you have added all the messages you want to
        add in a single operation. See BeginUpdate for more information.
      See also:
        BeginUpdate
    }
    procedure EndUpdate;

    { Description
        This method adds a new line of text to the console. The text can
        contain html-like tags to specify the formatting of the text.

        Here are the tags available:

          * \<b\>Text\</b\> - Bold text.
          * \<i\>Text\</i\> - Italic text.
          * \<u\>Text\</u\> - Underlined text.
          * \<color name=clRed\>Text\</color\> - Colored text.
          * \<blink\>Text\</blink\> - Blinking text.
          * \<img index=0\> - Image from the imagelist component with the
            given index.
          * \<indent\>Text\</indent\> - All text in the indented block will begin
            at the same position out from the left margin. This is useful if
            you wish to create lists or similar.
          * \<noselect\>Text\</noselect\> - All the text in the noselect block
            will be left out when you select text. Useful for tagging
            timestamp columns or similar that is not part of the actual
            console messages.
          * \<a name="xxx" params="yyy" hint="zzz"\>Text\</a\> - The text in the
            a tag will be clickable. An OnClick event will be fired with the
            given parameters from the attributes. All the attributes are
            optional and will be substituted with blank strings if you don't
            specify them. The hint attribute will also be used as the
            hint property for the console control when you hover the mouse over
            the link. You also got two modifier attributes, "effect" and
            "hovereffect". These two have the value 1 or True by default, but
            you can set them to 0 by adding effect="0" and hovereffect="0".
            Basically, setting the effect to 0 means the link won't get the
            hyperlink color and font, and setting the hovereffect to 0 means
            that hovering the mouse over it won't change the font in any way.
          * \<column spot=xxx\> - Move to X coordinate xxx and continue
            the console text from that spot. If the X coordinate is already past
            the given spot, then the \<column\> tag will have no effect.
          * \<pre\>text\</pre\> - Render text in the PreFont property
            font settings. Also check out the Whitespace property for more
            information.

        The function will return a pointer to the message node added. You can
        use this pointer either to get information about the node, or use
        in calls to Replace and Delete.
      Parameters:
        Text - The text to add.
        Below - The existing message node to add this new message below. Pass
          nil (default) to add the new message to the bottom of the console.
        AutoParseURLs - Set this parameter to true to have the Add method
          automatically mark internet url's as links. These links will be of
          type URL, which means that if you use the AutoOpenURLs option,
          clicking on such a link will open the correct client program to
          deal with the url.
      Returns:
        Pointer to the newly added messagenode. You can use this pointer in
        calls to Replace, Add(below), Delete and similar.
    }
    function Add(const Text: string; const Below: PMessageNode=nil;
      const AutoParseURLs: Boolean=False): PMessageNode; overload; virtual;

    { Description
        This method adds a new line of text to the console. The Fmt and
        Args parameters works exactly like the Format function in the
        SysUtils unit.

        The function will return a pointer to the message node added. You can
        use this pointer either to get information about the node, or use
        in calls to Replace and Delete.
      Parameters:
        Fmt - Format string.
        Args - Parameters to the format string.
        Below - The existing message node to add this new message below. Pass
          nil (default) to add the new message to the bottom of the console.
        AutoParseURLs - Set this parameter to true to have the Add method
          automatically mark internet url's as links. These links will be of
          type URL, which means that if you use the AutoOpenURLs option,
          clicking on such a link will open the correct client program to
          deal with the url.
      Returns:
        Pointer to the newly added messagenode. You can use this pointer in
        calls to Replace, Add(below), Delete and similar.
      See also:
        Add@string@PMessageNode@Boolean,
        Add@string@array of const@PMessageNode@Boolean
    }
    function Add(const Fmt: string; const Args: array of const;
      const Below: PMessageNode=nil;
      const AutoParseURLs: Boolean=False): PMessageNode; overload; virtual;

    { Description:
        Use this method to replace the contents of a line of text in the
        console with a new line of text. You can use the same abilities as
        with the Add method.
      Parameters:
        Message - The message node to replace the text for.
        Text - The new text.
        UpdateTimestamp - Set this to True to replace the timestamp with the
          current time.
      See also:
        Add@string@PMessageNode@Boolean,
        Add@string@array of const@PMessageNode@Boolean
    }
    procedure Replace(const Message: PMessageNode; const Text: string;
      const UpdateTimestamp: Boolean=False); overload; virtual;

    { Description:
        Use this method to replace the contents of a line of text in the
        console with a new line of text. You can use the same abilities as
        with the Add method.
      Parameters:
        Message - The message node to replace the text for.
        Fmt - Format string.
        Args - Parameters to the format string.
        UpdateTimestamp - Set this to True to replace the timestamp with the
          current time.
      See also:
        Add@string@PMessageNode@Boolean,
        Add@string@array of const@PMessageNode@Boolean
    }
    procedure Replace(const Message: PMessageNode; const Fmt: string;
      const Args: array of const; const UpdateTimestamp: Boolean=False); overload; virtual;

    { Description:
        This method will remove a message node completely from the console.
    }
    procedure Delete(const Message: PMessageNode); virtual;

    { Description:
        This property returns the number of messages present in the console.
        Note that this might be less than the number of nodes you've added,
        either because the console has begun removing messages over the limit,
        or because you have deleted existing messages.
    }
    property MessageCount: Integer read GetMessageCount;

    { Description:
        This property will give you direct access to the message nodes in the
        console.

        Note: Due to the fact that the nodes are stored in a linked list,
          only Messages[0] will be retrieved fast, the rest will be in
          linear time depending on the index. If you need to quickly iterate
          through the nodes, retrieve the 0th message and use the Next
          member to move through the list until you get to the end.
    }
    property Messages[Index: Integer]: PMessageNode read GetMessages;

    { Description:
        Call this method to manually scroll the document up or down. Use
        a positive value to scroll up, and a negative value to scroll down.
        The value is the number of pixels to scroll.
    }
    procedure ScrollBy(const Delta: Integer);

    { Description:
        This property gives you access to the current position in the document.
        Set the property to scroll to a different position.
    }
    property ScrollPosition: Integer read GetScrollPos write SetScrollPos;

    { Description:
        This method will take the text given and return a text suitable for
        adding to the console. The text can contain IRC color codes and
        effects and it will be replaced with proper tags for the console.
        HTML tag delimiters \< and \> will be replaced with &lt; and &gt; so
        that they can be printed.

        Note: A url catcher/link creator will come in a future version.
      Parameters:
        IRCText - The IRC text to reformat to the console syntax.
    }
    function IRCTextToConsoleText(const IRCText: string): string;

    { Description:
        This method takes a single string literal and translates it into a text
        that is proper for the console. If the literal contains any special
        characters that the console parser uses, then they will be replaced
        by tags and quoted characters so they will appear correctly on the
        console. Basically, if you add the resulting text to the console, it
        will be added as it were, without parsing or reformatting.
      Parameters:
        Literal - The literal to translate.
      Returns:
        The translated text.
    }
    function LiteralTextToConsoleText(const Literal: string): string;
  end;

  { Description
      This component allows you to place a GUI console component on your form.
      A console component allows you to dump messages that are visible to the
      user, but with more formatting and options than a simple TMemo component
      or similar.

      This console component supports font styles, colors, images, clickable
      links, indentation, auto-word-wrapping, blinking text.
  }
  TlvkConsole = class(TlvkCustomConsole)
  published
    // <ALIAS TlvkCustomConsole.StoreText>
    property StoreText;
    // <ALIAS TlvkCustomConsole.ImageList>
    property ImageList;
    // <ALIAS TlvkCustomConsole.OnLink>
    property OnLink;
    // <ALIAS TlvkCustomConsole.OnSelect>
    property OnSelect;
    // <ALIAS TlvkCustomConsole.BlinkInterval>
    property BlinkInterval;
    // <ALIAS TlvkCustomConsole.Options>
    property Options;
    // <ALIAS TlvkCustomConsole.DesignText>
    property DesignText;
    // <ALIAS TlvkCustomConsole.LeftMargin>
    property LeftMargin;
    // <ALIAS TlvkCustomConsole.RightMargin>
    property RightMargin;
    // <ALIAS TlvkCustomConsole.Limit>
    property Limit;
    // <ALIAS TlvkCustomConsole.LineSpacing>
    property LineSpacing;
    // <ALIAS TlvkCustomConsole.Scrolling>
    property Scrolling;
    // <ALIAS TlvkCustomConsole.Edge>
    property Edge;
    // <ALIAS TlvkCustomConsole.Links>
    property LinkInformation;
    // <ALIAS TlvkCustomConsole.Links>
    property Links;
    // <ALIAS TlvkCustomConsole.OnTextAdded>
    property OnTextAdded;
    // <ALIAS TlvkCustomConsole.TimestampVisible>
    property TimestampVisible;
    // <ALIAS TlvkCustomConsole.TimestampFormat>
    property TimestampFormat;
    // <ALIAS TlvkCustomConsole.IndentType>
    property IndentType;
    // <ALIAS TlvkCustomConsole.IndentAmount>
    property IndentAmount;
    // <ALIAS TlvkCustomConsole.TimestampEmptyLines>
    property TimestampEmptyLines;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomConsole.Whitespace>
    property Whitespace;
    // <ALIAS TlvkCustomConsole.PreFont>
    property PreFont;

    property ShowHint;
    property ParentShowHint;
    property ParentFont default False;
    property Align;
    property Anchors;
    property Font;
    property Color;
    property PopupMenu;
    property OnDblClick;
    property OnClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

const
  MAX_LEFT_MARGIN       = 30;
  MAX_RIGHT_MARGIN      = 30;

  DEFAULT_HEIGHT        = 80;
  DEFAULT_WIDTH         = 100;

  DEFAULT_PROPERTY_NAME = '$$default$$';

implementation

uses
  lvkRegExp, Math;

{ TlvkCustomConsole }

function TlvkCustomConsole.Add(const Text: string; const Below: PMessageNode;
  const AutoParseURLs: Boolean): PMessageNode;

  procedure FindURLs(const Message: PMessageNode);
  type
    TURLType = (utNone, utHTTP, utMAILTO, utNEWS);
  var
    Part        : PPartNode;
    URLPart     : PPartNode;
    EndURLPart  : PPartNode;
    URL         : string;
    URLType     : TURLType;

    function NewPart(const PreviousPart: PPartNode): PPartNode;
    begin
      New(Result);
      FillChar(Result^, SizeOf(Result^), #0);
      Result^.Next := nil;
      Result^.Blink := PreviousPart^.Blink;
      Result^.LineNo := -1;
      Result^.NoSelect := PreviousPart^.NoSelect;
      Result^.Message := Message;
    end;

    function URLCheck(const Part: PPartNode): TURLType;
    begin
      if CompareText(Copy(Part^.Word, 1, 7), 'http://') = 0 then
        Result := utHTTP
      else if CompareText(Copy(Part^.Word, 1, 7), 'mailto:') = 0 then
        Result := utMAILTO
      else if CompareText(Copy(Part^.Word, 1, 7), 'news://') = 0 then
        Result := utNEWS
      else
        Result := utNone;
    end;

  begin
    Part := Message^.Parts;

    while Assigned(Part) do
    begin
      if Part^.PartType = ptWord then
      begin
        URLType := URLCheck(Part);
        if URLType <> utNone then
        begin
          URLPart := NewPart(Part);
          EndURLPart := NewPart(URLPart);

          URL := Part^.Word;
          StrDispose(Part^.Word);
          Part^.Word := nil;

          // Fixup middle part, the text to display
          URLPart^.PartType := ptWord;
          if URLType = utMAILTO then
            URLPart^.Word := StrNew(PChar(Copy(URL, 8, Length(URL)-7)))
          else
            URLPart^.Word := StrNew(PChar(URL));

          // Fixup end part, the end of the link
          EndURLPart^.PartType := ptRemoveLink;

          // Fixup start part, the start of the link
          Part^.PartType := ptAddLink;
          Part^.LinkParams := StrNew(PChar(URL));
          case URLType of
            utHTTP:
              Part^.LinkHint := StrNew(PChar('Open ' + URL + ' in default browser'));

            utMAILTO:
              Part^.LinkHint := StrNew(PChar('Send email to ' + URL + ' via default mail client'));

            utNEWS:
              Part^.LinkHint := StrNew(PChar('Read news from ' + URL + ' in default news client'));
          end;
          Part^.LinkName := StrNew('URL');
          Part^.LinkType := ltURL;
          Part^.LinkEffect := FAutoParseEffect;
          Part^.LinkHoverEffect := FAutoParseHoverEffect;

          // Link them all together
          EndURLPart^.Next := Part^.Next;
          URLPart^.Next := EndURLPart;
          Part^.Next := URLPart;
          Part := EndURLPart^.Next;
        end else
          Part := Part^.Next;
      end else
        Part := Part^.Next;
    end;
  end;

begin
  BeginUpdate;
  try
    if FMessages = nil then
      FScrollPos := FScrollMax;

    New(Result);
    if StoreText then
      Result^.Text := Text;
    Result^.Parts := ParseText(Text, Result);
    Result^.MaxWidth := 0;
    Result^.Width := 0;
    Result^.Height := 0;
    Result^.Timestamp := Now;
    Result^.LineHeights := nil;
    Result^.Prev := nil;

    if Assigned(Below) then
    begin
      Result^.Next := Below;
      Result^.Prev := Below^.Prev;

      Below^.Prev := Result;
      if Assigned(Result^.Prev) then
        Result^.Prev^.Next := Result
      else
        FMessages := Result;
    end else begin
      if FMessages = nil then
      begin
        FLastMessage := Result;
        FLastMessage^.Prev := nil;
      end;
      if Assigned(FMessages) then
        FMessages^.Prev := Result;
      Result^.Next := FMessages;
      FMessages := Result;
    end;
    FUpdated := True;

    if AutoParseURLs then
      FindURLs(Result);

    RenderMessage(Result, 0, False);
    if FScrollPos > 0 then
    begin
      if FScrollPos = FScrollDestination then
        Inc(FScrollDestination, Result^.Height + FLineSpacing);
      Inc(FScrollPos, Result^.Height + FLineSpacing);
      Inc(FScrollMax, Result^.Height + FLineSpacing);
      end
    else if FSmoothScrolling and (not (csDesigning in ComponentState)) then
    begin
      Inc(FScrollPos, Result^.Height + FLineSpacing);
      FScrollDestination := 0;
    end;

    if FScrollPos > FScrollMax then
      FScrollPos := FScrollMax;
    if FScrollDestination > FScrollMax then
      FScrollDestination := FScrollMax;

    Inc(FCount);
    RemoveOverLimit;
    Inc(FLineCount);
  finally
    EndUpdate;
  end;
end;

function TlvkCustomConsole.Add(const Fmt: string; const Args: array of const;
  const Below: PMessageNode; const AutoParseURLs: Boolean): PMessageNode;
begin
  Result := Add(Format(Fmt, Args), Below, AutoParseURLs);
end;

procedure TlvkCustomConsole.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TlvkCustomConsole.Clear;
var
  Message : PMessageNode;
begin
  BeginUpdate;
  try
    ClearLinks;
    while Assigned(FMessages) do
    begin
      Message := FMessages;
      FMessages := Message^.Next;
      DestroyParts(Message^.Parts);
      Dispose(Message);
      FUpdated := True;
    end;

    FCount := 0;
    FLastMessage := nil;
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomConsole.ClearLinks;
var
  Link  : PLinkNode;
begin
  while Assigned(FLinks) do
  begin
    Link := FLinks;
    FLinks := FLinks^.Next;

    Dispose(Link);
  end;
end;

procedure TlvkCustomConsole.Click;
begin
  inherited;

  if not (FSelecting and FSelectMoved) then
    if Assigned(CurrentLink) then
      DoLink(CurrentLink);
end;

procedure TlvkCustomConsole.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  CurrentLink := nil;
end;

procedure TlvkCustomConsole.ContinueSelect(const X, Y: Integer);
begin
  FSelectX2 := X;
  FSelectY2 := Y;

  Invalidate;
end;

constructor TlvkCustomConsole.Create(AOwner: TComponent);
begin
  inherited;

  FAutoParseHoverEffect := True;
  FAutoParseEffect := True;
  FWhitespace := cwsCollapseOutsidePRE;
  FScrollingData := TScrollingData.Create(Self);
  FLinkData := TLinkData.Create(Self);
  FMaxScrollSpeed := DEFAULT_MAX_SCROLLSPEED;
  FDecelerationRange := DEFAULT_ACCELERATE_RANGE;
  FAccelerationRange := DEFAULT_DECELERATE_RANGE;
  FRemoveLinesWhileScrolledUp := DEFAULT_REMOVE_LINES;
  FIndentType := DEFAULT_INDENT_TYPE;
  FIndentAmount := DEFAULT_INDENT_AMOUNT;
  FTimestampFormat := '';
  FEdge := DEFAULT_EDGE;
  FLineSpacing := DEFAULT_LINE_SPACING;
  FLeftMargin := DEFAULT_LEFT_MARGIN;
  FRightMargin := DEFAULT_RIGHT_MARGIN;
  ParentFont := False;
  ParentColor := False;
  Font.OnChange := FontChange;
  FUpdateLevel := 0;
  FMessages := nil;
  FUpdated := False;
  FImageList := nil;
  Color := DEFAULT_COLOR;
  Font.OnChange := FontChanged;
  FPreFont := TFont.Create;
  FPreFont.OnChange := FontChanged;
  FLinkFont := TFont.Create;
  FLinkFont.OnChange := FontChanged;
  FHoverFont := TFont.Create;
  FHoverFont.OnChange := FontChanged;

  FLinkFont.Color := DEFAULT_LINK_COLOR;
  FHoverFont.Assign(FLinkFont);
  FHoverFont.Style := [fsUnderline];

  FScrollPos := 0;
  FOptions := [coSelection, coBlinking, coAutoCopy, coAutoOpenURLLinks];
  FLimit := DEFAULT_LIMIT;

  Width := DEFAULT_WIDTH;
  Height := DEFAULT_HEIGHT;

  FBlinkTimer := TTimer.Create(Self);
  FBlinkTimer.Enabled := True;
  FBlinkTimer.OnTimer := OnBlink;
  FBlinkTimer.Interval := DEFAULT_BLINK_INTERVAL;

  FTimestampEmptyLines := DEFAULT_TIMESTAMP_EMPTY;
  FTimestampVisible := DEFAULT_TIMESTAMP_VISIBLE;

  FDesignText := TStringList.Create;
  TStringList(FDesignText).OnChange := DesignTextChange;
end;

procedure TlvkCustomConsole.CreateLink(const Rect: TRect; const Part: PPartNode);
var
  Link  : PLinkNode;
begin
  if (Rect.Top >= ClientHeight) or (Rect.Bottom < 0) then
    Exit;

  New(Link);
  Link^.Rect := Rect;
  Link^.Part := Part;
  Link^.Next := FLinks;
  FLinks := Link;
end;

procedure TlvkCustomConsole.CreateParams(var Params: TCreateParams);
const
  EdgeValues  : array[TEdgeType] of Cardinal = (
    0,                            // etNone
    WS_EX_CLIENTEDGE,             // etClient
    WS_EX_STATICEDGE              // etStatic
  );
begin
  inherited;

  Params.Style := (Params.Style or WS_VSCROLL) and (not WS_BORDER);
  Params.ExStyle := Params.ExStyle or EdgeValues[FEdge];
end;

procedure TlvkCustomConsole.Delete(const Message: PMessageNode);
var
  Prev, Next  : PMessageNode;
begin
  Assert(Assigned(Message));

  BeginUpdate;
  try
    Prev := Message^.Prev;
    Next := Message^.Next;
    DestroyParts(Message^.Parts);
    Dispose(Message);
    Dec(FCount);

    if Assigned(Next) and Assigned(Prev) then
    begin
      Next^.Prev := Prev;
      Prev^.Next := Next;
    end else if Assigned(Next) then
    begin
      FMessages := Next;
      Next^.Prev := nil;
    end else if Assigned(Prev) then
    begin
      Prev^.Next := nil;
      FLastMessage := Prev;
    end else begin
      FMessages := nil;
      FLastMessage := nil;
    end;
    FUpdated := True;
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomConsole.DesignTextChange(Sender: TObject);
var
  Index : Integer;
begin
  if csDesigning in ComponentState then
  begin
    BeginUpdate;
    try
      Clear;
      for Index := 0 to FDesignText.Count-1 do
        Add(FDesignText[Index]);
    finally
      FScrollPos := 0;
      EndUpdate;
    end;
  end;
end;

destructor TlvkCustomConsole.Destroy;
begin
  FUpdateLevel := 1;

  Clear;
  ClearLinks;

  FBlinkTimer.Free;
  FDesignText.Free;
  FLinkFont.Free;
  FPreFont.Free;
  FHoverFont.Free;
  FScrollingData.Free;
  FLinkData.Free;

  inherited;
end;

procedure TlvkCustomConsole.DestroyParts(const Part: PPartNode);
var
  CurPart   : PPartNode;
  NextPart  : PPartNode;
begin
  CurPart := Part;
  while Assigned(CurPart) do
  begin
    NextPart := CurPart^.Next;

    case CurPart^.PartType of
      ptWord:
        StrDispose(CurPart^.Word);

      ptAddFont:
        StrDispose(CurPart^.NewFontName);

      ptAddFontStyle, ptRemoveFontStyle:
        ;

      ptAddLink:
        begin
          StrDispose(CurPart^.LinkName);
          StrDispose(CurPart^.LinkParams);
          StrDispose(CurPart^.LinkHint);
        end;
    end;

    Dispose(CurPart);
    CurPart := NextPart;
  end;
end;

procedure TlvkCustomConsole.DoLink(const LinkNode: PPartNode);
begin
  if (coAutoOpenURLLinks in FOptions) and (LinkNode^.LinkType = ltURL) then
    ShellExecute(Application.Handle, 'OPEN', LinkNode^.LinkParams, nil, nil, SW_NORMAL);

  if Assigned(FOnLink) then
    FOnLink(Self, LinkNode^.LinkName, LinkNode^.LinkParams, LinkNode^.LinkHint);
end;

procedure TlvkCustomConsole.DoTextAdded;
begin
  if FLineCount > 0 then
  begin
    if Assigned(FOnTextAdded) then
    begin
      if FSmoothScrolling then
        FOnTextAdded(Self, FLineCount, FScrollDestination > 0)
      else
        FOnTextAdded(Self, FLineCount, ScrollPosition > 0);
    end;
    FLineCount := 0;
  end;
end;

procedure TlvkCustomConsole.EndSelect(const X, Y: Integer);
begin
  FSelecting := False;
  Invalidate;

  if FSelectMoved then
    SelectText;
  FSelectMoved := False;
end;

procedure TlvkCustomConsole.EndUpdate;
begin
  Dec(FUpdateLevel);

  if (FUpdateLevel = 0) and FUpdated then
  begin
    Invalidate;
    Paint;

    DoTextAdded;
  end;
end;

procedure TlvkCustomConsole.FontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TlvkCustomConsole.FontChanged(Sender: TObject);
begin
  Reformat;
  Invalidate;
end;

function TlvkCustomConsole.FormatTimestamp(
  const Timestamp: TDateTime): string;
begin
  Result := FormatDateTime(FTimestampFormat, Timestamp);
end;

function TlvkCustomConsole.GetBlinkInterval: Integer;
begin
  Result := FBlinkTimer.Interval;
end;

function TlvkCustomConsole.GetLinkInformation: TLinkInformation;
begin
  if Assigned(CurrentLink) then
  begin
    Result.Valid := True;
    Result.Name := CurrentLink^.LinkName;
    Result.Params := CurrentLink^.LinkParams;
    Result.Hint := CurrentLink^.LinkHint;
  end else begin
    Result.Valid := False;
    Result.Name := '';
    Result.Params := '';
    Result.Hint := '';
  end;
end;

function TlvkCustomConsole.GetMessageCount: Integer;
begin
  Result := FCount;
end;

function TlvkCustomConsole.GetMessages(Index: Integer): PMessageNode;
begin
  Result := FMessages;
  while (Index > 0) and Assigned(Result) do
  begin
    Result := Result^.Next;
    Dec(Index);
  end;

  if (Index < 0) or (Index > 0) then
    Result := nil;
end;

function TlvkCustomConsole.GetScrollPos: Integer;
begin
  if FSmoothScrolling then
    Result := FScrollDestination
  else
    Result := FScrollPos;
end;

function TlvkCustomConsole.IRCTextToConsoleText(
  const IRCText: string): string;
var
  Index       : Integer;
  Fore        : string;
  Back        : string;
  ColorTag    : string;
  BoldOn      : Boolean;
  UnderlineOn : Boolean;
const
  Colors  : array[0..15] of string = (
    '#ffffff',
    '#000000',
    '#00007f',
    '#009300',
    '#ff0000',
    '#7f0000',
    '#9c009c',
    '#fc7f00',
    '#ffff00',
    '#00fc00',
    '#009393',
    '#00ffff',
    '#0000fc',
    '#ff00ff',
    '#7f7f7f',
    '#d2d2d2'
  );
begin
  Result := IRCText;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  BoldOn := False;
  UnderlineOn := False;
  repeat
    Index := Pos(#3, Result);
    if Index > 0 then
    begin
      System.Delete(Result, Index, 1);
      Fore := '';
      Back := '';

      while (Index <= Length(Result)) and (Result[Index] in ['0'..'9']) and (Length(Fore) < 2) do
      begin
        Fore := Fore + Result[Index];
        System.Delete(Result, Index, 1);
      end;

      if (Index <= Length(Result)) and (Result[Index] = ',') then
      begin
        System.Delete(Result, Index, 1);
        while (Index <= Length(Result)) and (Result[Index] in ['0'..'9']) and (Length(Back) < 2) do
        begin
          Back := Back + Result[Index];
          System.Delete(Result, Index, 1);
        end;
      end;

      if Fore = '' then
        ColorTag := '</font>'
      else begin
        ColorTag := '<font foregroundcolor="' + Colors[StrToIntDef(Fore, 0) mod 16] + '"';
        if Back <> '' then
          ColorTag := ColorTag + ' backgroundcolor="' + Colors[StrToIntDef(Back, 1) mod 16] + '"';
        ColorTag := ColorTag + '>';
      end;
      Insert(ColorTag, Result, Index);
      Continue;
    end;

    Index := Pos(#2, Result);
    if Index > 0 then
    begin
      System.Delete(Result, Index, 1);
      if BoldOn then
        Insert('</b>', Result, Index)
      else
        Insert('<b>', Result, Index);
      BoldOn := not BoldOn;
      Continue;
    end;

    Index := Pos(#31, Result);
    if Index > 0 then
    begin
      System.Delete(Result, Index, 1);
      if UnderlineOn then
        Insert('</u>', Result, Index)
      else
        Insert('<u>', Result, Index);
      UnderlineOn := not UnderlineOn;
      Continue;
    end;
  until Index = 0;
end;

function TlvkCustomConsole.LiteralTextToConsoleText(
  const Literal: string): string;
var
  ResultLength  : Integer;
  Index         : Integer;
const
  TagBegin  : array[0..3] of Char = '&lt;';
  Ampersand : array[0..4] of Char = '&amp;';
  LineBreak : array[0..3] of Char = '<br>';
  Space     : array[0..5] of Char = '&nbsp;';
begin
  Index := 1;
  SetLength(Result, Length(Literal) * 6);
  ResultLength := 0;

  while Index <= Length(Literal) do
  begin
    case Literal[Index] of
      '<':
        begin
          Move(TagBegin, Result[ResultLength+1], SizeOf(TagBegin));
          Inc(ResultLength, SizeOf(TagBegin));
        end;

      '&':
        begin
          Move(Ampersand, Result[ResultLength+1], SizeOf(Ampersand));
          Inc(ResultLength, SizeOf(Ampersand));
        end;

      #13:
        begin
          Move(LineBreak, Result[ResultLength+1], SizeOf(LineBreak));
          Inc(ResultLength, SizeOf(LineBreak));
        end;

      #32:
        begin
          Move(Space, Result[ResultLength+1], SizeOf(Space));
          Inc(ResultLength, SizeOf(Space));
        end;
    else
      begin
        Inc(ResultLength);
        Result[ResultLength] := Literal[Index];
      end;
    end;

    Inc(Index);
  end;

  SetLength(Result, ResultLength);
end;

procedure TlvkCustomConsole.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (coSelection in FOptions) then
    StartSelect(X, Y);
end;

procedure TlvkCustomConsole.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Link  : PLinkNode;
begin
  inherited;

  if FSelecting then
  begin
    if FSelectMoved then
      ContinueSelect(X, Y)
    else if (Abs(X-FSelectX1) >= 5) or (Abs(Y-FSelectY1) >= 5) then
    begin
      FSelectMoved := True;
      ContinueSelect(X, Y);
    end;
  end else begin
    Link := FLinks;
    while Assigned(Link) do
    begin
      if (X >= Link^.Rect.Left) and (X < Link^.Rect.Right) and (Y >= Link^.Rect.Top) and (Y < Link^.Rect.Bottom) then
      begin
        if CurrentLink <> Link^.Part then
        begin
          CurrentLink := Link^.Part;
          Invalidate;
        end;
        Exit;
      end;

      Link := Link^.Next;
    end;

    if CurrentLink <> nil then
    begin
      CurrentLink := nil;
      Invalidate;
    end;
  end;
end;

procedure TlvkCustomConsole.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and FSelecting then
    EndSelect(X, Y);
end;

procedure TlvkCustomConsole.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FImageList then
    begin
      FImageList := nil;
      Invalidate;
    end;
  end;
end;

procedure TlvkCustomConsole.OnBlink(Sender: TObject);
begin
  FBlinking := not FBlinking;
  if FBlinkingVisible and (not (csDesigning in ComponentState)) then
    Invalidate;

  if Assigned(FCurrentLink) then
    Cursor := crHandPoint;
end;

procedure TlvkCustomConsole.OnInvalidate(var Msg: TMessage);
begin
  Invalidate;
end;

procedure TlvkCustomConsole.Paint;
var
  CurrentY      : Integer;
  Message       : PMessageNode;
  RenderWidth   : Integer;
begin
  if FScrollPos > FScrollMax then
    FScrollPos := FScrollMax;
  if FScrollDestination > FScrollMax then
    FScrollDestination := FScrollMax;

  CurrentY := ClientHeight + FScrollPos;
  Message := FMessages;
  FSelectPart := 0;
  FBlinkingVisible := False;

  ClearLinks;
  RenderWidth := ClientWidth - FRightMargin;
  while Assigned(Message) do
  begin
    if (Message^.Height = 0) or (Message^.Width <> RenderWidth) then
      RenderMessage(Message, CurrentY, False);

    Dec(CurrentY, Message^.Height);
    if (CurrentY < ClientHeight) and (CurrentY > -Message^.Height) then
      RenderMessage(Message, CurrentY, True);

    if FLineSpacing > 0 then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Dec(CurrentY, FLineSpacing);
      Canvas.Rectangle(0, CurrentY, ClientWidth, CurrentY + FLineSpacing);
    end;

    Message := Message^.Next;
  end;

  if CurrentY > 0 then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(0, 0, ClientWidth, CurrentY);
  end;

  FUpdated := False;

  UpdateScrollBar;

  if FMessages = nil then
  begin
    FCurrentLink := nil;
    Cursor := crDefault;
  end;

  if FSmoothScrolling and (FScrollDestination <> FScrollPos) then
  begin
    if FScrollPos < FScrollDestination then
    begin
      Inc(FScrollPos, FScrollSpeed);
      if FScrollPos > FScrollDestination then
      begin
        FScrollPos := FScrollDestination;
        FScrollSpeed := 1;
      end else begin
        if FScrollDestination - FScrollPos <= FDecelerationRange then
        begin
          if FScrollSpeed > 1 then
            Dec(FScrollSpeed);
        end else if FScrollSpeed < FMaxScrollSpeed then
        begin
          if FScrollDestination - FScrollPos >= FAccelerationRange then
            FScrollSpeed := FMaxScrollSpeed
          else
            Inc(FScrollSpeed);
        end;
      end;
    end else begin
      Dec(FScrollPos, FScrollSpeed);
      if FScrollPos < FScrollDestination then
      begin
        FScrollPos := FScrollDestination;
        FScrollSpeed := 1;
      end else begin
        if FScrollPos - FScrollDestination <= FDecelerationRange then
        begin
          if FScrollSpeed > 1 then
            Dec(FScrollSpeed);
        end else if FScrollSpeed < FMaxScrollSpeed then
        begin
          if FScrollPos - FScrollDestination >= FAccelerationRange then
            FScrollSpeed := FMaxScrollSpeed
          else
            Inc(FScrollSpeed);
        end;
      end;
    end;
    PostMessage(Handle, CM_INVALIDATE, 0, 0);
  end else
    FScrollSpeed := 1;
end;

function TlvkCustomConsole.ParseText(const Text: string;
  const MessageNode: PMessageNode): PPartNode;
var
  Parts         : PPartNode;
  Part          : PPartNode;
  Index         : Integer;
  InWhitespace  : Boolean;
  BlinkLevel    : Integer;
  NoSelectLevel : Integer;
  PreStyleLevel : Integer;
  Collapse      : Boolean;

  function NewPart: PPartNode;
  begin
    if Assigned(Parts) then
    begin
      New(Part^.Next);
      Part := Part^.Next;
    end else begin
      New(Parts);
      Part := Parts;
    end;

    FillChar(Part^, SizeOf(Part^), #0);
    Part^.Next := nil;
    Result := Part;
    Result^.Blink := (BlinkLevel > 0);
    Result^.LineNo := -1;
    Result^.NoSelect := (NoSelectLevel > 0);
    Result^.Message := MessageNode;
  end;

  procedure AddWordPart(const NewWord: string);
  var
    Temp  : PChar;
  begin
    if Assigned(Parts) and (Part^.PartType = ptWord) and (Part^.Blink = (BlinkLevel > 0)) then
    begin
      GetMem(Temp, StrLen(Part^.Word) + Cardinal(Length(NewWord)) + 1);
      try
        StrCopy(Temp, Part^.Word);
        StrCat(Temp, PChar(NewWord));

        StrDispose(Part^.Word);
        Part^.Word := StrNew(Temp);
      finally
        FreeMem(Temp);
      end;
    end else begin
      with NewPart^ do
      begin
        PartType := ptWord;
        Word := StrNew(PChar(NewWord));
      end;
    end;
  end;

  procedure AddFontStyle(const Remove: Boolean; const FontStyle: TFontStyle);
  begin
    with NewPart^ do
    begin
      if Remove then
      begin
        PartType := ptRemoveFontStyle;
        RemoveFontStyle := [FontStyle];
      end else begin
        PartType := ptAddFontStyle;
        AddFontStyle := [FontStyle];
      end;
    end;
  end;

  procedure AddIndent(const Remove: Boolean);
  begin
    with NewPart^ do
    begin
      if Remove then
        PartType := ptRemoveIndent
      else
        PartType := ptAddIndent;
    end;
  end;

  procedure AddLineBreak;
  begin
    with NewPart^ do
      PartType := ptLineBreak;
  end;

  procedure AddColor(const Remove: Boolean; const Attributes: TStrings);
  var
    ColorName : string;
  begin
    if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
      ColorName := Attributes.Values[DEFAULT_PROPERTY_NAME]
    else
      ColorName := Attributes.Values['Name'];

    with NewPart^ do
    begin
      if Remove then
      begin
        PartType := ptRemoveFont;
      end else begin
        PartType := ptAddFont;
        NewBackgroundColor := clNone;
        if Copy(ColorName, 1, 1) = '#' then
          NewForegroundColor := StringToColor('$' + Copy(ColorName, 6, 2) + Copy(ColorName, 4, 2) + Copy(ColorName, 2, 2))
        else
          NewForegroundColor := StringToColor(ColorName);
      end;
    end;
  end;

  procedure AddLink(const Remove: Boolean; const Attributes: TStrings);
  begin
    if Remove then
    begin
      with NewPart^ do
        PartType := ptRemoveLink;
    end else begin
      with NewPart^ do
      begin
        PartType := ptAddLink;
        LinkEffect := True;
        LinkHoverEffect := True;

        if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
        begin
          LinkName := StrNew(PChar(Attributes.Values[DEFAULT_PROPERTY_NAME]));
          LinkParams := nil;
          LinkHint := nil;
        end else begin
          LinkName := StrNew(PChar(Attributes.Values['name']));
          LinkParams := StrNew(PChar(Attributes.Values['params']));
          LinkHint := StrNew(PChar(Attributes.Values['hint']));
          if RegExpMatch('^0|off|false$', Attributes.Values['effect']) then
            LinkEffect := False;
          if RegExpMatch('^0|off|false$', Attributes.Values['hovereffect']) then
            LinkHoverEffect := False;

          if CompareText(Attributes.Values['type'], 'URL') = 0 then
            LinkType := ltURL
          else
            LinkType := ltEvent;
        end;
      end;
    end;
  end;

  procedure AddFont(const Remove: Boolean; const Attributes: TStrings);

    function ExpandedStringToColor(const s: string): TColor;
    begin
      if Copy(s, 1, 1) = '#' then
        Result := StringToColor('$' + Copy(s, 6, 2) + Copy(s, 4, 2) + Copy(s, 2, 2))
      else
        Result := StringToColor(s);
    end;

  begin
    if Remove then
    begin
      with NewPart^ do
        PartType := ptRemoveFont;
    end else begin
      with NewPart^ do
      begin
        PartType := ptAddFont;
        if Attributes.Values['name'] <> '' then
          NewFontName := StrNew(PChar(Attributes.Values['name']))
        else if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
          NewFontName := StrNew(PChar(Attributes.Values[DEFAULT_PROPERTY_NAME]))
        else
          NewFontName := nil;

        if Attributes.Values['foregroundcolor'] <> '' then
          NewForegroundColor := ExpandedStringToColor(Attributes.Values['foregroundcolor'])
        else if Attributes.Values['color'] <> '' then
          NewForegroundColor := ExpandedStringToColor(Attributes.Values['color'])
        else if Attributes.Values['foreground'] <> '' then
          NewForegroundColor := ExpandedStringToColor(Attributes.Values['foreground'])
        else
          NewForegroundColor := clNone;

        if Attributes.Values['backgroundcolor'] <> '' then
          NewBackgroundColor := ExpandedStringToColor(Attributes.Values['backgroundcolor'])
        else if Attributes.Values['background'] <> '' then
          NewBackgroundColor := ExpandedStringToColor(Attributes.Values['background'])
        else
          NewBackgroundColor := clNone;

        if Attributes.Values['size'] <> '' then
        begin
          NewFontSize := 0;
          NewFontDeltaSize := 0;

          if Attributes.Values['size'][1] in ['+', '-'] then
            NewFontDeltaSize := StrToIntDef(Attributes.Values['size'], 0)
          else
            NewFontSize := StrToIntDef(Attributes.Values['size'], 0);
        end else begin
          NewFontSize := 0;
          NewFontDeltaSize := 0;
        end;
      end;
    end;
  end;

  procedure AddImage(const Remove: Boolean; const Attributes: TStrings);
  begin
    if not Remove then
    begin
      with NewPart^ do
      begin
        PartType := ptImage;
        if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
          ImageIndex := StrToIntDef(Attributes.Values[DEFAULT_PROPERTY_NAME], 0)
        else
          ImageIndex := StrToIntDef(Attributes.Values['index'], 0);
      end;
    end;
  end;

  procedure AddColumn(const Attributes: TStrings);
  begin
    with NewPart^ do
    begin
      PartType := ptColumn;
      if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
        ColumnSpot := StrToIntDef(Attributes.Values[DEFAULT_PROPERTY_NAME], 0)
      else
        ColumnSpot := StrToIntDef(Attributes.Values['spot'], 0);
    end;
  end;

  procedure AddHorisontalLine(const Attributes: TStrings);
  begin
    with NewPart^ do
    begin
      PartType := ptHorisontalLine;
      if Attributes.Values[DEFAULT_PROPERTY_NAME] <> '' then
        WidthPercent := Min(100, Max(0, StrToIntDef(Attributes.Values[DEFAULT_PROPERTY_NAME], 100)))
      else
        WidthPercent := Min(100, Max(0, StrToIntDef(Attributes.Values['width'], 100)));
    end;
  end;

  procedure ParseWord;
  var
    Word  : string;
  begin
    Word := '';
    while (Index <= Length(Text)) and (not (Text[Index] in [#32, #9, #10, #13, '<', '&'])) do
    begin
      Word := Word + Text[Index];
      Inc(Index);
    end;

    AddWordPart(Word);
  end;

  procedure ParseQuoted;
  var
    Quoted  : string;
    i       : Integer;
  const
    QuotedList  : array[1..4] of string = (
      '< &lt',
      '> &gt',
      '  &nbsp',
      '& &amp'
    );
  begin
    Quoted := '';

    while (Index <= Length(Text)) and (Text[Index] <> ';') do
    begin
      Quoted := Quoted + Text[Index];
      Inc(Index);
    end;
    Inc(Index);

    for i := Low(QuotedList) to High(QuotedList) do
      if Copy(QuotedList[i], 3, Length(QuotedList[i])) = Quoted then
      begin
        AddWordPart(Copy(QuotedList[i], 1, 1));
        Exit;
      end;

    AddWordPart(Quoted);
  end;

  procedure EnterPRE;
  begin
    NewPart^.PartType := ptEnterPRE;
  end;

  procedure LeavePRE;
  begin
    NewPart^.PartType := ptLeavePRE;
  end;

  procedure ParseTag;
  var
    UnTag       : Boolean;
    TagName     : string;
    Attributes  : TStrings;
    Tag         : string;

    procedure SplitAttributes;
    var
      Index       : Integer;
      Quote       : Char;
      InQuote     : Boolean;
      StartIndex  : Integer;

      procedure AddAttribute(const Attr: string);
      var
        x     : Integer;
        Name  : string;
        Value : string;
      begin
        x := Pos('=', Attr);
        if x > 0 then
        begin
          Name := Copy(Attr, 1, x-1);
          Value := Copy(Attr, x+1, Length(Attr));
          if (Value <> '') and (Value[1] in ['"', '''']) and (Value[1] = Value[Length(Value)]) then
          begin
            System.Delete(Value, 1, 1);
            System.Delete(Value, Length(Value), 1);
          end;
        end else begin
          Name := DEFAULT_PROPERTY_NAME;
          Value := Attr;
        end;

        Attributes.Values[Name] := Value;
      end;

    begin
      Index := 1;
      StartIndex := -1;
      Quote := #0;
      InQuote := False;

      while Index <= Length(Tag) do
      begin
        if (not InQuote) and (Tag[Index] in [#32, #9, #10, #13]) then
        begin
          if StartIndex > 0 then
            AddAttribute(Copy(Tag, StartIndex, Index-StartIndex));
          StartIndex := -1;
        end else begin
          if StartIndex < 0 then
            StartIndex := Index;

          if InQuote then
          begin
            if Tag[Index] = Quote then
              InQuote := False;
          end else begin
            if Tag[Index] in ['"', ''''] then
            begin
              InQuote := True;
              Quote := Tag[Index];
            end;
          end;
        end;

        Inc(Index);
      end;

      if StartIndex > 0 then
        AddAttribute(Copy(Tag, StartIndex, Index-StartIndex));
    end;

    procedure AddPreStyle(const UnTag: Boolean);
    begin
      if UnTag then
      begin
        if PreStyleLevel > 0 then
        begin
          Dec(PreStyleLevel);
          if PreStyleLevel = 0 then
            LeavePRE;
        end;
      end else begin
        Inc(PreStyleLevel);
        if PreStyleLevel = 1 then
          EnterPRE;
      end;
    end;

  begin
    Attributes := TStringList.Create;
    try
      Tag := '';

      while (Index <= Length(Text)) and (Text[Index] <> '>') do
      begin
        if Text[Index] in [#13, #10, #9] then
          Tag := Tag + #32
        else
          Tag := Tag + Text[Index];
        Inc(Index);
      end;
      Inc(Index);

      System.Delete(Tag, 1, 1);

      if Tag[Length(Tag)] = '>' then
        System.Delete(Tag, Length(Tag), 1);

      if Copy(Tag, 1, 1) = '/' then
      begin
        System.Delete(Tag, 1, 1);
        UnTag := True;
      end else
        UnTag := False;

      if Pos(#32, Tag) = 0 then
      begin
        TagName := Tag;
        Tag := '';
      end else begin
        TagName := Copy(Tag, 1, Pos(#32, Tag)-1);
        Tag := Trim(Copy(Tag, Length(TagName)+1, Length(Tag)));
      end;

      SplitAttributes;

      if CompareText(TagName, 'PRE') = 0 then
        AddPreStyle(UnTag)
      else if CompareText(TagName, 'B') = 0 then
        AddFontStyle(UnTag, fsBold)
      else if CompareText(TagName, 'I') = 0 then
        AddFontStyle(UnTag, fsItalic)
      else if CompareText(TagName, 'U') = 0 then
        AddFontStyle(UnTag, fsUnderline)
      else if CompareText(TagName, 'COLOR') = 0 then
        AddColor(UnTag, Attributes)
      else if CompareText(TagName, 'INDENT') = 0 then
        AddIndent(UnTag)
      else if CompareText(TagName, 'BR') = 0 then
        AddLineBreak
      else if CompareText(TagName, 'A') = 0 then
        AddLink(UnTag, Attributes)
      else if CompareText(TagName, 'FONT') = 0 then
        AddFont(UnTag, Attributes)
      else if CompareText(TagName, 'IMG') = 0 then
        AddImage(UnTag, Attributes)
      else if CompareText(TagName, 'HR') = 0 then
        AddHorisontalLine(Attributes)
      else if CompareText(TagName, 'COLUMN') = 0 then
        AddColumn(Attributes)
      else if CompareText(TagName, 'NOSELECT') = 0 then
      begin
        if UnTag then
        begin
          if NoSelectLevel > 0 then
            Dec(NoSelectLevel);
        end else
          Inc(NoSelectLevel);
      end else if CompareText(TagName, 'BLINK') = 0 then
      begin
        if UnTag then
        begin
          if BlinkLevel > 0 then
            Dec(BlinkLevel);
        end else
          Inc(BlinkLevel);
      end;
    finally
      Attributes.Free;
    end;
  end;

begin
  Parts := nil;
  BlinkLevel := 0;
  NoSelectLevel := 0;
  PreStyleLevel := 0;

  Index := 1;
  InWhitespace := False;

  if (Text <> '') or FTimestampEmptyLines then
    with NewPart^ do
      PartType := ptTimestamp;

  while Index <= Length(Text) do
  begin
    case Text[Index] of
      #32, #9, #10, #13:
        begin
          case FWhitespace of
            cwsCollapse:
              Collapse := True;

            cwsLeave:
              Collapse := False;

            cwsCollapseOutsidePRE:
              Collapse := PreStyleLevel = 0;

          else
            Collapse := True;
          end;

          if Collapse then
          begin
            if not InWhitespace then
            begin
              InWhitespace := True;

              with NewPart^ do
                PartType := ptWhitespace;
            end;
          end else begin
            case Text[Index] of
              #32, #9:
                NewPart^.PartType := ptWhitespace;

              #13:
                NewPart^.PartType := ptLineBreak;

              #10:
                ; // ignore
            end;
          end;
          Inc(Index);
        end;

      '&':
        begin
          InWhitespace := False;
          ParseQuoted;
        end;

      '<':
        begin
          InWhitespace := False;
          ParseTag;
        end;

    else
      begin
        InWhitespace := False;
        ParseWord;
      end;
    end;
  end;

  Result := Parts;
end;

procedure TlvkCustomConsole.Reformat;
var
  MessageNode : PMessageNode;
  PartNode    : PPartNode;
begin
  MessageNode := FMessages;
  while Assigned(MessageNode) do
  begin
    MessageNode^.Width := 0;
    MessageNode^.Height := 0;

    PartNode := MessageNode^.Parts;
    while Assigned(PartNode) do
    begin
      PartNode^.Width := 0;
      PartNode^.Height := 0;

      PartNode := PartNode^.Next;
    end;

    MessageNode := MessageNode^.Next;
  end;
end;

procedure TlvkCustomConsole.RemoveOverLimit;
var
  Message : PMessageNode;
begin
  if (not FRemoveLinesWhileScrolledUp) and (FScrollDestination > 0) then
    Exit;

  while (FLimit > 0) and (FCount > FLimit) do
  begin
    Message := FLastMessage;

    FLastMessage := Message^.Prev;
    Message^.Prev^.Next := nil;
    DestroyParts(Message^.Parts);
    Dec(FScrollMax, Message^.Height);

    Dispose(Message);
    Dec(FCount);
  end;
end;

procedure TlvkCustomConsole.RenderMessage(const Message: PMessageNode;
  const Y: Integer; const DoPaint: Boolean);
type
  TFontRec = record
    Name            : string;
    Size            : Integer;
    ForegroundColor : TColor;
    BackgroundColor : TColor;
    FontStyles      : TFontStyles;
  end;
var
  Part            : PPartNode;
  CurX            : Integer;
  CurY            : Integer;
  WordWidth       : Integer;
  Indent          : Integer;
  Indents         : array of Integer;
  Links           : array of PPartNode;
  Fonts           : array of TFontRec;
  r               : TRect;
  Word            : string;
  NextPart        : PPartNode;
  DoDraw          : Boolean;
  PrevColor       : TColor;
  PrevBackground  : TColor;
  x1, x2          : Integer;
  MarginCleared   : Boolean;
  LineNo          : Integer;
  InPre           : Boolean;
  PreviousIndent  : Integer;
  TempColor       : TColor;

  procedure FixupLineHeights;
  begin
    while Length(Message ^.LineHeights) <= LineNo do
    begin
      SetLength(Message^.LineHeights, Length(Message^.LineHeights)+1);
      Message^.LineHeights[High(Message^.LineHeights)] := Canvas.TextHeight('[')+2;
    end;

    Assert(LineNo < Length(Message^.LineHeights));
  end;

  procedure AdjustSelect(const Part: PPartNode; const Rect: TRect);
  var
    x1, y1, x2, y2  : Integer;
    Temp            : Integer;
  begin
    if (not FSelecting) or (not FSelectMoved) then
      Exit;

    x1 := FSelectX1;
    y1 := FSelectY1;
    x2 := FSelectX2;
    y2 := FSelectY2;

    if (y1 >= Rect.Top) and (y1 < Rect.Bottom) and (y2 >= Rect.Top) and (y2 < Rect.Bottom) then
    begin
      if x1 > x2 then
      begin
        Temp := x1;
        x1 := x2;
        x2 := Temp;
      end;
    end else if y1 > y2 then
    begin
      Temp := x1;
      x1 := x2;
      x2 := Temp;

      Temp := y1;
      y1 := y2;
      y2 := Temp;
    end;

    if (y1 > Rect.Bottom) or (y2 < Rect.Top) then
      Exit
    else if (y1 < Rect.Top) and (y2 >= Rect.Bottom) then
      Part^.Selected := True
    else if (y1 < Rect.Bottom) and (y2 >= Rect.Bottom) then
    begin
      if x1 < Rect.Right then
        Part^.Selected := True;
    end else if (y1 < Rect.Top) and (y2 >= Rect.Top) then
    begin
      if x2 >= Rect.Left then
        Part^.Selected := True;
    end else if (y1 >= Rect.Top) and (y2 < Rect.Bottom) then
    begin
      if (x1 < Rect.Right) and (x2 >= Rect.Left) then
        Part^.Selected := True;
    end;

    if Part^.NoSelect then
      Part^.Selected := False;
  end;

  procedure ClearToEndOfLine;
  begin
    if (CurX < ClientWidth) and DoPaint then
    begin
      PrevBackground := Canvas.Brush.Color;

      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;

      if LineNo < Length(Message^.LineHeights) then
        Canvas.Rectangle(CurX, CurY, ClientWidth, CurY + Message^.LineHeights[LineNo]);

      Canvas.Brush.Color := PrevBackground;
      Canvas.Pen.Color := PrevBackground;
    end;
  end;

  procedure NewLine;
  begin
    if CurX > Message^.MaxWidth then
      Message^.MaxWidth := CurX;
    ClearToEndOfLine;
    FixupLineHeights;
    Inc(CurY, Message^.LineHeights[LineNo]);

    MarginCleared := False;

    if InPre then
      CurX := PreviousIndent
    else
      CurX := Indent;

    PreviousIndent := CurX;
    Inc(LineNo);
  end;

  procedure ClearMargin;
  begin
    if MarginCleared then
      Exit;

    if DoPaint then
    begin
      PrevBackground := Canvas.Brush.Color;

      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      FixupLineHeights;
      Canvas.Rectangle(0, CurY, Indent, CurY + Message^.LineHeights[LineNo]);

      Canvas.Brush.Color := PrevBackground;
      Canvas.Pen.Color := PrevBackground;
    end;
    MarginCleared := True;
  end;

  procedure AssignLineNo(const Part: PPartNode);
  begin
    Part^.LineNo := LineNo;
    while LineNo >= Length(Message^.LineHeights) do
    begin
      SetLength(Message^.LineHeights, Length(Message^.LineHeights)+1);
      Message^.LineHeights[High(Message^.LineHeights)] := Canvas.TextHeight('[')+2;
    end;

    FixupLineHeights;
    Message^.LineHeights[LineNo] := Max(Part^.Height, Message^.LineHeights[LineNo]);
  end;

  procedure RenderWordPart(const Word: string; var Part: PPartNode);
  begin
    if Part^.Width = 0 then
      Part^.Width := Canvas.TextWidth(Word);
    if Part^.Height = 0 then
      Part^.Height := Canvas.TextHeight(Word);
    WordWidth := Part^.Width;

    AssignLineNo(Part);

    if (CurX + WordWidth >= ClientWidth-FRightMargin) and (WordWidth < ClientWidth-FRightMargin) then
    begin
      NewLine;
      AssignLineNo(Part);
      if Part^.PartType = ptWhitespace then
      begin
        Part := NextPart;
        Exit;
      end;
    end;

    ClearMargin;

    if DoPaint then
    begin
      FixupLineHeights;
      r := Rect(CurX, CurY, CurX + WordWidth, CurY + Message^.LineHeights[LineNo]);
      if Length(Links) > 0 then
        CreateLink(r, Links[High(Links)]);

      DoDraw := True;

      if FBlinkTimer.Enabled and FBlinking and Part^.Blink then
        DoDraw := False;

      AdjustSelect(Part, r);

      PrevColor := clBlack;
      if Part^.Selected then
      begin
        PrevColor := Canvas.Font.Color;
        PrevBackground := Canvas.Brush.Color;

        Canvas.Font.Color := Color;
        Canvas.Brush.Color := clHighlight;
      end;

      Canvas.Pen.Color := Canvas.Brush.Color;

      FixupLineHeights;
      if Part^.Blink and (CurY + Message^.LineHeights[LineNo] >= 0) and (CurY < ClientHeight) then
        FBlinkingVisible := True;
      if DoDraw then
        Canvas.TextRect(r, CurX, CurY + Message^.LineHeights[LineNo] - Part^.Height, Word)
      else
        Canvas.Rectangle(r);

      if Part^.Selected then
      begin
        Canvas.Font.Color := PrevColor;
        Canvas.Brush.Color := PrevBackground;
        Canvas.Pen.Color := Canvas.Brush.Color;
      end;
    end;

    Inc(CurX, WordWidth);
  end;

  procedure PushFont;
  begin
    SetLength(Fonts, Length(Fonts)+1);
    Fonts[High(Fonts)].Name := Canvas.Font.Name;
    Fonts[High(Fonts)].Size := Canvas.Font.Size;
    Fonts[High(Fonts)].ForegroundColor := Canvas.Font.Color;
    Fonts[High(Fonts)].BackgroundColor := Canvas.Brush.Color;
    Fonts[High(Fonts)].FontStyles := Canvas.Font.Style;
  end;

  procedure PopFont;
  begin
    if Length(Fonts) > 0 then
    begin
      Canvas.Font.Name := Fonts[High(Fonts)].Name;
      Canvas.Font.Size := Fonts[High(Fonts)].Size;
      Canvas.Font.Color := Fonts[High(Fonts)].ForegroundColor;
      Canvas.Brush.Color := Fonts[High(Fonts)].BackgroundColor;
      Canvas.Font.Style := Fonts[High(Fonts)].FontStyles;
      SetLength(Fonts, Length(Fonts)-1);
    end;
  end;

begin
  SetLength(Indents, 0);
  SetLength(Links, 0);
  InPre := False;

  Part := Message^.Parts;
  Message^.MaxWidth := 0;

  if (not DoPaint) or (Length(Message^.LineHeights) = 0) then
  begin
    SetLength(Message^.LineHeights, 1);
    Message^.LineHeights[0] := 0; // Canvas.TextHeight('[')+2;
  end;
  LineNo := 0;

  CurY := Y;
  Indent := FLeftMargin;
  PreviousIndent := Indent;
  Canvas.Font.Assign(Font);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;

  MarginCleared := False;

  if FLeftMargin > 0 then
  begin
    CurX := FLeftMargin;

    if DoPaint then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Assert(LineNo < Length(Message^.LineHeights));
      Canvas.Rectangle(0, CurY, CurX, CurY + Message^.LineHeights[LineNo]);
    end;
  end else
    CurX := 0;
  if not Assigned(Part) then
    NewLine;

  if FIndentType = itHanging then
    Indent := FIndentAmount;

  MarginCleared := True;
  while Assigned(Part) do
  begin
    NextPart := Part^.Next;

    Part^.Selected := False;

    case Part^.PartType of
      ptEnterPre:
        begin
          PushFont;

          Canvas.Font := FPreFont;
          InPre := True;
        end;

      ptLeavePRE:
        begin
          PopFont;
          InPre := False;
        end;

      ptTimestamp:
        begin
          Word := FormatTimestamp(Message^.Timestamp) + ' ';
          if FTimestampVisible then
            RenderWordPart(Word, Part);

          if FIndentType = itTimestamp then
            Indent := CurX;
        end;

      ptWord, ptWhitespace:
        begin
          if Part^.PartType = ptWord then
            Word := Part^.Word
          else
            Word := ' ';

          RenderWordPart(Word, Part);
        end;

      ptAddFont:
        begin
          PushFont;

          if Part^.NewFontName <> nil then
            Canvas.Font.Name := Part^.NewFontName;
          if Part^.NewFontSize > 0 then
            Canvas.Font.Size := Part^.NewFontSize;
          if Part^.NewFontDeltaSize > 0 then
            Canvas.Font.Size := Canvas.Font.Size + Part^.NewFontDeltaSize;
          if Part^.NewForegroundColor <> clNone then
            Canvas.Font.Color := Part^.NewForegroundColor;
          if Part^.NewBackgroundColor <> clNone then
            Canvas.Brush.Color := Part^.NewBackgroundColor;
        end;

      ptRemoveFont:
        PopFont;

      ptAddFontStyle:
        Canvas.Font.Style := Canvas.Font.Style + Part^.AddFontStyle;

      ptRemoveFontStyle:
        Canvas.Font.Style := Canvas.Font.Style - Part^.RemoveFontStyle;

      ptAddIndent:
        begin
          SetLength(Indents, Length(Indents)+1);
          Indents[High(Indents)] := Indent;
          Indent := CurX;
        end;

      ptColumn:
        begin
          if CurX < Part^.ColumnSpot then
          begin
            Assert(LineNo < Length(Message^.LineHeights));
            AdjustSelect(Part, Rect(CurX, CurY, Part^.ColumnSpot, CurY + Message^.LineHeights[LineNo]));
            AssignLineNo(Part);
            if CurX > ClientWidth-FRightMargin then
              NewLine
            else begin
              if DoPaint then
              begin
                PrevColor := clBlack;
                if Part^.Selected then
                begin
                  PrevColor := Canvas.Font.Color;
                  Canvas.Font.Color := Color;
                  Canvas.Brush.Color := clHighlight;
                end; { else
                  Canvas.Brush.Color := Color; }

                Canvas.Pen.Color := Canvas.Brush.Color;
                Assert(LineNo < Length(Message^.LineHeights));
                Canvas.Rectangle(CurX, CurY, Part^.ColumnSpot, CurY + Message^.LineHeights[LineNo]);
                CurX := Part^.ColumnSpot;

                if Part^.Selected then
                begin
                  Canvas.Font.Color := PrevColor;
                  Canvas.Brush.Color := Color;
                  Canvas.Pen.Color := Canvas.Brush.Color;
                end;
              end;
            end;
          end;
        end;

      ptRemoveIndent:
        if Length(Indents) > 0 then
        begin
          Indent := Indents[High(Indents)];
          SetLength(Indents, Length(Indents)-1);
        end;

      ptLineBreak:
        NewLine;

      ptAddLink:
        begin
          SetLength(Links, Length(Links) + 1);
          Links[High(Links)] := Part;

          PushFont;
          { TODO 2 -oLVK -cSource :  }
          if Part = FCurrentLink then
          begin
            if Part^.LinkHoverEffect then
            begin
              if FHoverFont.Color = clNone then
              begin
                TempColor := Canvas.Font.Color;
                Canvas.Font.Assign(FHoverFont);
                Canvas.Font.Color := TempColor;
              end else
                Canvas.Font.Assign(FHoverFont);
            end;
          end else if Part^.LinkEffect then
          begin
            if FLinkFont.Color = clNone then
            begin
              TempColor := Canvas.Font.Color;
              Canvas.Font.Assign(FLinkFont);
              Canvas.Font.Color := TempColor;
            end else
              Canvas.Font.Assign(FLinkFont);
          end;
        end;

      ptHorisontalLine:
        begin
          if CurX > 0 then
          begin
            if DoPaint then
              ClearToEndOfLine;
            Assert(LineNo < Length(Message^.LineHeights));
            Inc(CurY, Message^.LineHeights[LineNo]);

            if (Indent > 0) and DoPaint then
            begin
              Canvas.Brush.Color := Color;
              Canvas.Pen.Color := Color;
              Canvas.Rectangle(0, CurY, Indent, CurY + 2);
            end;

            CurX := Indent;
          end;

          x1 := Indent;
          x2 := Indent + ((ClientWidth - FRightMargin - Indent) * Part^.WidthPercent) div 100;
          Part^.Height := 2;
          Inc(LineNo);
          AssignLineNo(Part);
          Assert(LineNo < Length(Message^.LineHeights));
          Message^.LineHeights[LineNo] := 2;
          Inc(LineNo);

          if DoPaint then
          begin
            Canvas.Pen.Color := Canvas.Font.Color;
            Canvas.MoveTo(x1, CurY);
            Canvas.LineTo(x2, CurY);

            Canvas.Pen.Color := Color;
            Canvas.MoveTo(0, CurY);
            Canvas.LineTo(x1, CurY);

            Canvas.MoveTo(0, CurY+1);
            Canvas.LineTo(ClientWidth, CurY+1);

            Canvas.MoveTo(x2, CurY);
            Canvas.LineTo(ClientWidth, CurY);
          end;

          CurX := Indent;
          Inc(CurY, Part^.Height);
          MarginCleared := False;
        end;

      ptImage:
        begin
          if Assigned(FImageList) then
          begin
            if (CurX + FImageList.Width >= ClientWidth-FRightMargin) and (FImageList.Width < ClientWidth-FRightMargin) then
              NewLine;

            Part^.Width := FImageList.Width;
            Part^.Height := FImageList.Height;

            AssignLineNo(Part);
            ClearMargin;
            FImageList.BkColor := Color;
            DoDraw := True;

            if FBlinkTimer.Enabled and FBlinking and Part^.Blink then
              DoDraw := False;

            if Length(Links) > 0 then
              CreateLink(Rect(CurX, CurY, CurX + FImageList.Width, CurY + FImageList.Height), Links[High(Links)]);

            Assert(LineNo < Length(Message^.LineHeights));
            AdjustSelect(Part, Rect(CurX, CurY, CurX + FImageList.Width, CurY + Message^.LineHeights[LineNo]));
            if DoPaint then
            begin
              PrevColor := clBlack;
              if Part^.Selected then
              begin
                PrevColor := Canvas.Font.Color;
                Canvas.Font.Color := Color;
                Canvas.Brush.Color := clHighlight;
              end else
                Canvas.Brush.Color := Color;
              Canvas.Pen.Color := Canvas.Brush.Color;

              Assert(LineNo < Length(Message^.LineHeights));
              if Part^.Blink and (CurY + Message^.LineHeights[LineNo] >= 0) and (CurY < ClientHeight) then
                FBlinkingVisible := True;
                
              if DoDraw then
              begin
                if Part^.Selected then
                  FImageList.BkColor := clHighlight
                else
                  FImageList.BkColor := Color;

                FImageList.Draw(Canvas, CurX, CurY, Part^.ImageIndex);
                Assert(LineNo < Length(Message^.LineHeights));
                if FImageList.Height < Message^.LineHeights[LineNo] then
                  Canvas.Rectangle(CurX, CurY + FImageList.Height, CurX + FImageList.Width, CurY + Message^.LineHeights[LineNo]);
              end else
                Canvas.Rectangle(CurX, CurY, CurX + FImageList.Width, CurY + Message^.LineHeights[LineNo]);

              if Part^.Selected then
              begin
                Canvas.Font.Color := PrevColor;
                Canvas.Brush.Color := Color;
                Canvas.Pen.Color := Canvas.Brush.Color;
              end;
            end;
            Inc(CurX, FImageList.Width);
          end;
        end;

      ptRemoveLink:
        if Length(Links) > 0 then
        begin
          PopFont;
          SetLength(Links, Length(Links)-1);
        end;
    end;

    Part := NextPart;
  end;

  if (CurX > FLeftMargin) and (CurX < ClientWidth - FRightMargin) then
  begin
    if DoPaint then
      ClearToEndOfLine;
    if LineNo >= Length(Message^.LineHeights) then
      Inc(CurY, Canvas.TextHeight('[')+2)
    else
      Inc(CurY, Message^.LineHeights[LineNo]);
  end;

  Message^.Width := ClientWidth - FRightMargin;
  Message^.Height := 0;
  for LineNo := Low(Message^.LineHeights) to High(Message^.LineHeights) do
    Message^.Height := Message^.Height + Message^.LineHeights[LineNo];
end;

procedure TlvkCustomConsole.Replace(const Message: PMessageNode;
  const Text: string; const UpdateTimestamp: Boolean);
begin
  Assert(Assigned(Message));

  BeginUpdate;
  try
    DestroyParts(Message^.Parts);

    if StoreText then
      Message^.Text := Text
    else
      Message^.Text := '';
    Message^.Parts := ParseText(Text, Message);
    Message^.MaxWidth := 0;
    Message^.Width := 0;
    Message^.Height := 0;
    if UpdateTimestamp then
      Message^.Timestamp := Now;
    FUpdated := True;

    RenderMessage(Message, 0, False);
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomConsole.Replace(const Message: PMessageNode;
  const Fmt: string; const Args: array of const; const UpdateTimestamp: Boolean);
begin
  Replace(Message, Format(Fmt, Args), UpdateTimestamp);
end;

procedure TlvkCustomConsole.ScrollBy(const Delta: Integer);
begin
  ScrollPosition := ScrollPosition + Delta;
end;

procedure TlvkCustomConsole.SelectText;
var
  Message : PMessageNode;
  Part    : PPartNode;
  Text    : TStringList;
  Line    : string;
  Index   : Integer;
  Any     : Boolean;
begin
  Text := TStringList.Create;
  try
    Message := FMessages;

    while Assigned(Message) do
    begin
      Index := 0;

      Line := '';
      Part := Message^.Parts;
      Any := False;
      while Assigned(Part) do
      begin
        if Part^.Selected then
        begin
          Any := True;
          case Part^.PartType of
            ptTimestamp:
              Line := Line + FormatTimestamp(Message^.Timestamp) + ' ';

            ptWord:
              Line := Line + Part^.Word;

            ptWhitespace:
              Line := Line + ' ';

            ptColumn:
              Line := Line + #9;
          end;
        end else if Part^.PartType = ptLineBreak then
        begin
          if Any then
          begin
            Text.Insert(Index, Line);
            Inc(Index);
          end;
          Line := '';
          Any := False;
        end;

        Part := Part^.Next;
      end;

      if Any then
        Text.Insert(Index, Line);

      Message := Message^.Next;
    end;

    // Strip of trailing linebreaks
    Line := Text.Text;
    while (Line <> '') and (Line[Length(Line)] in [#13, #10]) do
      System.Delete(Line, Length(Line), 1);

    if Line <> '' then
    begin
      if Assigned(FOnSelect) then
        FOnSelect(Self, Line)
      else if (coAutoCopy in FOptions) then
        Clipboard.AsText := Line;
    end;
  finally
    Text.Free;
  end;
end;

procedure TlvkCustomConsole.SetBlinkInterval(const Value: Integer);
begin
  FBlinkTimer.Interval := Value;
end;

procedure TlvkCustomConsole.SetCurrentLink(const Value: PPartNode);

  procedure FlagForRecalculation(const MessageNode: PMessageNode);
  var
    Part  : PPartNode;
  begin
    MessageNode^.Width := 0;
    MessageNode^.Height := 0;

    Part := MessageNode^.Parts;
    while Assigned(Part) do
    begin
      Part^.Height := 0;
      Part^.Width := 0;
      Part := Part^.Next;
    end;
  end;

begin
  if Value <> FCurrentLink then
  begin
    if Assigned(FCurrentLink) then
    begin
      Cursor := crDefault;
      Hint := FPreviousHint;
      FlagForRecalculation(CurrentLink^.Message);
      Invalidate;
    end;

    FCurrentLink := Value;

    if Assigned(FCurrentLink) then
    begin
      FlagForRecalculation(CurrentLink^.Message);
      Invalidate;
      Cursor := crHandPoint;
      FPreviousHint := Hint;
      if Assigned(FCurrentLink^.LinkHint) and (FCurrentLink^.LinkHint^ <> #0) then
        Hint := FCurrentLink^.LinkHint;
    end;
  end;
end;

procedure TlvkCustomConsole.SetDesignText(const Value: TStringList);
begin
  FDesignText.Assign(Value);
  Invalidate;
end;

procedure TlvkCustomConsole.SetEdge(const Value: TEdgeType);
begin
  if Value <> FEdge then
  begin
    FEdge := Value;
    RecreateWnd;
  end;
end;

procedure TlvkCustomConsole.SetImageList(const Value: TImageList);
begin
  if FImageList <> Value then
  begin
    FImageList := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetIndentAmount(const Value: Integer);
begin
  if FIndentAmount <> Value then
  begin
    FIndentAmount := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetIndentType(const Value: TIndentType);
begin
  if FIndentType <> Value then
  begin
    FIndentType := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetLeftMargin(const Value: Integer);
begin
  if Value <> FLeftMargin then
  begin
    if Value < 0 then
      FLeftMargin := 0
    else if Value > MAX_LEFT_MARGIN then
      FLeftMargin := MAX_LEFT_MARGIN
    else
      FLeftMargin := Value;

    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetLimit(const Value: Integer);
begin
  if FLimit <> Value then
  begin
    if Value < 0 then
      FLimit := 0
    else
      FLimit := Value;

    RemoveOverLimit;
  end;
end;

procedure TlvkCustomConsole.SetLineSpacing(const Value: Integer);
begin
  if Value <> FLineSpacing then
  begin
    if Value < 0 then
      FLineSpacing := 0
    else
    FLineSpacing := Value;

    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetLinkData(const Value: TLinkData);
begin
  FLinkData.Assign(Value);
end;

procedure TlvkCustomConsole.SetOptions(const Value: TConsoleOptions);

  procedure Unselect;
  var
    Message : PMessageNode;
    Part    : PPartNode;
  begin
    FSelecting := False;

    Message := FMessages;
    while Assigned(Message) do
    begin
      Part := Message^.Parts;
      while Assigned(Part) do
      begin
        Part^.Selected := False;
        Part := Part^.Next;
      end;
      Message := Message^.Next;
    end;
  end;

begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    FBlinkTimer.Enabled := (coBlinking in FOptions);

    if (not (coSelection in FOptions)) and FSelecting then
      Unselect;

    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetPreFont(const Value: TFont);
begin
  if Assigned(Value) then
    FPreFont.Assign(Value)
  else begin
    FPreFont.Free;
    FPreFont := TFont.Create;
    FPreFont.OnChange := FontChanged;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetRightMargin(const Value: Integer);
begin
  if Value <> FRightMargin then
  begin
    if Value < 0 then
      FRightMargin := 0
    else if Value > MAX_RIGHT_MARGIN then
      FRightMargin := MAX_RIGHT_MARGIN
    else
      FRightMargin := Value;

    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetScrollingData(const Value: TScrollingData);
begin
  FScrollingData := Value;
end;

procedure TlvkCustomConsole.SetScrollPos(const Value: Integer);
begin
  if FSmoothScrolling then
  begin
    if FScrollDestination <> Value then
    begin
      FScrollDestination := Value;
      if FScrollDestination < 0 then
        FScrollDestination := 0
      else if FScrollDestination > FScrollMax then
        FScrollDestination := FScrollMax;

      if Abs(FScrollDestination - FScrollPos) >= FAccelerationRange then
        FScrollSpeed := FMaxScrollSpeed;

      Invalidate;
    end;
  end else begin
    if FScrollPos <> Value then
    begin
      FScrollPos := Value;
      if FScrollPos < 0 then
        FScrollPos := 0
      else if FScrollPos > FScrollMax then
        FScrollPos := FScrollMax;
      Invalidate;
    end;
  end;
end;

procedure TlvkCustomConsole.SetTimestampEmptyLines(const Value: Boolean);
begin
  if TimestampEmptyLines <> Value then
  begin
    FTimestampEmptyLines := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetTimestampFormat(const Value: string);
var
  Message : PMessageNode;
begin
  if Value <> FTimestampFormat then
  begin
    FTimestampFormat := Value;
    Message := FMessages;
    while Assigned(Message) do
    begin
      if Assigned(Message^.Parts) and (Message^.Parts^.PartType = ptTimestamp) then
        Message^.Parts^.Height := 0;
      Message := Message^.Next;
    end;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.SetTimestampVisible(const Value: Boolean);
begin
  if FTimestampVisible <> Value then
  begin
    FTimestampVisible := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomConsole.StartSelect(const X, Y: Integer);
begin
  FSelecting := True;
  FSelectMoved := False;
  FSelectX1 := X;
  FSelectY1 := Y;
  FSelectX2 := X;
  FSelectY2 := Y;
  Invalidate;
end;

procedure TlvkCustomConsole.UpdateScrollBar;
var
  Sum         : Integer;
  Message     : PMessageNode;
  ScrollInfo  : TScrollInfo;
begin
  Sum := 0;
  Message := FMessages;
  while Assigned(Message) do
  begin
    Inc(Sum, Message^.Height + FLineSpacing);
    Message := Message^.Next;
  end;

  FScrollMax := Max(0, Sum - (Canvas.TextHeight('[') + 2));
  if FScrollPos > FScrollMax then
    FScrollPos := FScrollMax;
  if FScrollDestination > FScrollMax then
    FScrollDestination := FScrollMax;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_RANGE + SIF_PAGE + SIF_POS + SIF_DISABLENOSCROLL;
  if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) then
    Exit;

  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FScrollMax;
  if (FScrollPos <> FScrollDestination) and FSmoothScrolling then
    ScrollInfo.nPos := FScrollMax - FScrollDestination
  else
    ScrollInfo.nPos := FScrollMax - FScrollPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TlvkCustomConsole.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
  // Do nothing, avoid flicker
end;

procedure TlvkCustomConsole.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
end;

procedure TlvkCustomConsole.WMVScroll(var Message: TWMVScroll);
var
  ScrollInfo  : TScrollInfo;

  function Adjust(const Start, Delta: Integer): Integer;
  begin
    Result := Start + Delta;
    if Result < 0 then
      Result := 0
    else if Result > FScrollMax then
      Result := FScrollMax;
  end;

begin
  case Message.ScrollCode of
    SB_PAGEUP:
      ScrollPosition := Adjust(FScrollPos, ClientHeight);

    SB_PAGEDOWN:
      ScrollPosition := Adjust(FScrollPos, -ClientHeight);

    SB_THUMBTRACK:
      begin
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.fMask := SIF_TRACKPOS;
        if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) then
          Exit;

        FScrollPos := Adjust(FScrollMax - ScrollInfo.nTrackPos, 0);
        FScrollDestination := FScrollPos;
        Invalidate;
      end;

    SB_THUMBPOSITION:
      begin
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.fMask := SIF_TRACKPOS;
        if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) then
          Exit;

        ScrollPosition := Adjust(FScrollMax - ScrollInfo.nTrackPos, 0);
      end;

    SB_LINEUP:
      ScrollPosition := Adjust(FScrollPos, Canvas.TextHeight('[') + 2);

    SB_LINEDOWN:
      ScrollPosition := Adjust(FScrollPos, -(Canvas.TextHeight('[') + 2));

    SB_BOTTOM:
      ScrollPosition := 0;

    SB_TOP:
      ScrollPosition := FScrollMax;
  end;
end;

{ TScrollingData }

procedure TScrollingData.Assign(Source: TPersistent);
begin
  if Source is TScrollingData then
  begin
    SmoothScrolling := TScrollingData(Source).SmoothScrolling;
    MaxScrollSpeed := TScrollingData(Source).MaxScrollSpeed;
    AccelerationRange := TScrollingData(Source).AccelerationRange;
    DecelerationRange := TScrollingData(Source).DecelerationRange;
    RemoveLinesWhileScrolledUp := TScrollingData(Source).RemoveLinesWhileScrolledUp;
  end else if Source is TlvkCustomConsole then
    Assign(TlvkCustomConsole(Source).FScrollingData)
  else
    inherited;
end;

constructor TScrollingData.Create(const Console: TlvkCustomConsole);
begin
  inherited Create;

  FConsole := Console;
end;

function TScrollingData.GetAccelerationRange: Integer;
begin
  Result := FConsole.FAccelerationRange;
end;

function TScrollingData.GetDecelerationRange: Integer;
begin
  Result := FConsole.FDecelerationRange;
end;

function TScrollingData.GetMaxScrollSpeed: Integer;
begin
  Result := FConsole.FMaxScrollSpeed;
end;

function TScrollingData.GetRemoveLinesWhileScrolledUp: Boolean;
begin
  Result := FConsole.FRemoveLinesWhileScrolledUp;
end;

function TScrollingData.GetSmoothScrolling: Boolean;
begin
  Result := FConsole.FSmoothScrolling;
end;

procedure TScrollingData.SetAccelerationRange(const Value: Integer);
begin
  if Value < 0 then
    FConsole.FAccelerationRange := 0
  else
    FConsole.FAccelerationRange := Value;
end;

procedure TScrollingData.SetDecelerationRange(const Value: Integer);
begin
  if Value < 0 then
    FConsole.FDecelerationRange := 0
  else
    FConsole.FDecelerationRange := Value;
end;

procedure TScrollingData.SetMaxScrollSpeed(const Value: Integer);
begin
  if Value < 1 then
    FConsole.FMaxScrollSpeed := 1
  else
    FConsole.FMaxScrollSpeed := Value;
end;

procedure TScrollingData.SetRemoveLinesWhileScrolledUp(
  const Value: Boolean);
begin
  FConsole.FRemoveLinesWhileScrolledUp := Value;
end;

procedure TScrollingData.SetSmoothScrolling(const Value: Boolean);
begin
  FConsole.FSmoothScrolling := Value;
end;

{ TLinkData }

procedure TLinkData.Assign(Source: TPersistent);
begin
  if Source is TLinkData then
  begin
    Font := TLinkData(Source).Font;
    HoverFont := TLinkData(Source).HoverFont;
  end else if Source is TlvkCustomConsole then
    Assign(TlvkCustomConsole(Source).FLinkData)
  else
    inherited;
end;

constructor TLinkData.Create(const Console: TlvkCustomConsole);
begin
  inherited Create;

  FConsole := Console;
end;

function TLinkData.GetAutoParseEffect: Boolean;
begin
  Result := FConsole.FAutoParseEffect;
end;

function TLinkData.GetAutoParseHoverEffect: Boolean;
begin
  Result := FConsole.FAutoParseHoverEffect;
end;

function TLinkData.GetFont: TFont;
begin
  Result := FConsole.FLinkFont;
end;

function TLinkData.GetHoverFont: TFont;
begin
  Result := FConsole.FHoverFont;
end;

procedure TLinkData.SetAutoParseEffect(const Value: Boolean);
begin
  FConsole.FAutoParseEffect := Value;
end;

procedure TLinkData.SetAutoParseHoverEffect(const Value: Boolean);
begin
  FConsole.FAutoParseHoverEffect := Value;
end;

procedure TLinkData.SetFont(const Value: TFont);
begin
  FConsole.FLinkFont.Assign(Value);
end;

procedure TLinkData.SetHoverFont(const Value: TFont);
begin
  FConsole.FHoverFont.Assign(Value);
end;

end.
