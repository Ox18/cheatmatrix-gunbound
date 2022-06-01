{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component for using regular expressions in Delphi
    programs.

    Internally the component uses PCRE 4.1 for the regular expression matching.
}
unit lvkRegExp;

// $Author: Lasse V. Karlsen $
// $Revision: 23 $
// $Date: 28.04.03 0:28 $
// $Archive: /Components/LVK/Source/lvkRegExp.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkVersion, lvkSafeMem;

type
  { Description:
      This enumerated type contains the valid options that can be used with the
      TlvkRegExp component.

        * roGreedy - Always do a greedy match. With a greedy match, a .* will
          try to grab as much text as possible.
        * roCaseSensitive - Do a case sensitive match.
        * roMultiline - Text contains more than one line.
        * roDotAll - By default, the . pattern will not match newline
          characters. If you use the roDotAll option, the . pattern will
          include all characters.
        * roDollarEndOnly - The $ pattern matches end of line, but with
          this option, the $ pattern will match the end of the whole text.
        * roBol - Use when you pass partial text to the Text property. roBol
          should be used to tell the match engine that the start of the text
          is the start of a line.
        * roEol - Similar to roBol. Use to tell the engine that the end of
          the text is the end of a line.
        * roEmptyValid - Consider an empty match a valid match.
        * roAnchored - If this option is used, the match will only be
          valid if it starts at the start of the text.
  }
  TlvkRegExpOption  = (roGreedy, roCaseSensitive, roMultiline, roDotAll,
    roDollarEndOnly, roBol, roEol, roEmptyValid, roAnchored,
    roIgnoreWhitespace, roAutoCapture, roUTF8, roStudy);

  { Description:
      This type is used for the Option property of TlvkRegExp and is a
      set containing values from the TlvkRegExpOption enumerated type.
    See also:
      TlvkRegExp.Options, TlvkRegExpOption
  }
  TlvkRegExpOptions = set of TlvkRegExpOption;

const
  DEFAULT_OPTIONS       = [roGreedy, roBol, roEol, roAutoCapture];
  DEFAULT_OPTIONS_STUDY = DEFAULT_OPTIONS + [roStudy];

type
  { Description:
      This interface wraps the group data you can retrieve from the
      IRegExp.Groups property.
  }
  IRegExpGroup = interface
    ['{B238A392-6C4A-4EB4-B4C4-EC036E5F353A}']

    // <COMBINE StartPosition>
    function GetStartPosition: Integer;
    { Description:
        This property returns the starting position of the group in the
        text that was searched. Position 0 is the first character.
      See also:
        EndPosition
    }
    property StartPosition: Integer read GetStartPosition;

    // <COMBINE EndPosition>
    function GetEndPosition: Integer;
    { Description:
        This property returns the ending position of the group in the
        text that was searched. Position 0 is the first character.
      See also:
        StartPosition
    }
    property EndPosition: Integer read GetEndPosition;

    // <COMBINE Text>
    function GetText: string;
    { Description:
        This property returns the text that this group contains.
    }
    property Text: string read GetText;
  end;

  { Description:
      This interface wraps a reference counted regular expression object,
      tailored for use in methods and procedures where you don't really need
      the full designtime-configurable object. Use the
      NewRegExp@string@TlvkRegExpOptions function to create an instance
      of the object that this interface wraps.
    See also:
      NewRegExp@string@TlvkRegExpOptions
  }
  IRegExp = interface
    ['{3E25D89D-7E37-4B36-A19C-F0D3D3DB3548}']

    // <ALIAS TlvkRegExp.Pattern>
    function GetPattern: string;
    // <ALIAS TlvkRegExp.Pattern>
    procedure SetPattern(const NewValue: string);
    // <ALIAS TlvkRegExp.Pattern>
    property Pattern: string read GetPattern write SetPattern;

    { Description:
        This method is used to specify the text to search using the regular
        expression pattern.
      Parameters:
        Text - The text to search.
        MatchStart - The position to start matching at.
        MatchEnd - The position to end matching at.
      Returns:
        True if a match was found, False if not.
      See also:
        NextMatch
    }
    function MatchAgainst(const Text: string;
      const MatchStart: Integer=0; const MatchEnd: Integer=0): Boolean;

    // <ALIAS TlvkRegExp.NextMatch>
    function NextMatch: Boolean;

    // <ALIAS TlvkRegExp.MatchedGroupCount>
    function GetGroupCount: Integer;
    // <ALIAS TlvkRegExp.MatchedGroupCount>
    property GroupCount: Integer read GetGroupCount;

    // <COMBINE Groups>
    function GetGroups(const Index: Integer): IRegExpGroup;
    { Description:
        This property can be used to retrieve the sub-groups after a match
        has been found. Group 0 refers to the whole text that was matched,
        and group 1 and onwards refers to the (...) groups in the expression.
      See also:
        IRegExpGroup
    }
    property Groups[const Index: Integer]: IRegExpGroup read GetGroups; default;

    { Description:
        This function checks to see if the group index is valid, and that
        the group matched non-empty text, and then returns True. If the group
        index is invalid, or the group matched empty text, it will return False.
      Parameters:
        Index - The index of the group to check.
      See also:
        Groups
    }
    function HasGroupContents(const Index: Integer): Boolean;

    // <COMBINE NamedGroups>
    function GetNamedGroups(const Name: string): IRegExpGroup;
    { Description:
        This property returns groups by name. You can give a group a name by
        writing it like this: (?P&lt;name&gt;...) instead of just (...)

        Reading this property with an invalid or non-existant name will raise
        an exception. Use the HasNamedGroup function to check if the group
        exists before trying to read it.

        Note: A group which isn't actually used in the matching text might still
          "exist" in the list of groups but will contain no text.
      Parameters:
        Name - The name to get the group for.
      See also:
        HasNamedGroup, Groups, HasNamedGroupContents
    }
    property NamedGroups[const Name: string]: IRegExpGroup read GetNamedGroups;

    { Description:
        This function checks to see if there is a group by the specified name.

        Note: If a named group isn't actually used in the matching text, it
          might still exist in the list of groups and will thus "exist", but
          it will contain no text. This means you cannot use this function
          to check if a group matched any text.
      Parameters:
        Name - The name of the group to check for.
      See also:
        NamedGroups, HasNamedGroupContents
    }
    function HasNamedGroup(const Name: string): Boolean;

    { Description:
        This function is a expanded version of the HasNamedGroup function. In
        addition to checking if there is a group by the given name, it will
        also check to see that the group matched something. If the group isn't
        defined, wasn't used, or matched empty text, this function will return
        False, otherwise True.
      Parameters:
        Name - The name of the group to check for.
      See also:
        NamedGroups, HasNamedGroup
    }
    function HasNamedGroupContents(const Name: string): Boolean;

    { Description:
        This function returns the index number for a given group. It will
        raise an exception if there is no such group defined.
      Parameters:
        GroupName - The name of the group to return the index for.
      See also:
        NamedGroups, Groups
    }
    function GetNamedGroupIndex(const GroupName: string): Integer;
  end;

  { Description:
      This component encapsulates the PCRE 3.4 regular expression engine in a
      Delphi non-visual component.
  }
  TlvkRegExp  = class(TComponent)
  private
    FPattern        : string;
    Fpcre           : Pointer;
    Fextra          : Pointer;

    FError          : PChar;
    FErrorOffset    : Integer;

    FText           : string;
    FMatch          : Integer;
    FStart          : Integer;
    FEnd            : Integer;
    FOptions        : TlvkRegExpOptions;

    FOffsets        : array of Integer;

    function GetPackageVersion: TPackageVersion;
    procedure SetText(const Value: string);
    procedure SetPattern(const Value: string);
    procedure Compile;
    procedure SetMatchEnd(const Value: Integer);
    procedure SetMatchStart(const Value: Integer);
    function GetMatchedGroupCount: Integer;
    function GetMatchedText(const GroupIndex: Integer): string;
    function GetMatchedTextEnd(const GroupIndex: Integer): Integer;
    function GetMatchedTextStart(const GroupIndex: Integer): Integer;
    procedure SetOptions(const Value: TlvkRegExpOptions);
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This method restarts the match engine to the beginning of the text.
        Typically you will need to call this method after you have changed
        any of the options or the pattern property values.

        After calling Restart, call NextMatch to find the first match in the
        text.
      See also:
        NextMatch
    }
    procedure Restart;

    { Description:
        Use this method to find the next match in the text. You must call
        this method at least once to get the initial text, or find out that
        there is no possible match in the text.
      Returns:
        True if another match could be found, False if no match could be found.
      See also:
        TlvkRegExp.Restart
    }
    function NextMatch: Boolean;

    { Description:
        This property returns the starting position in the text of the
        current match. The first character in the text is considered the 0th
        character.

        Note: This property only gives usable results after a call to NextMatch
          with a result of True.
      Returns:
        The position in the text of the current match.
      See also:
        TlvkRegExp.MatchEnd
    }
    property MatchStart: Integer read FStart write SetMatchStart;

    { Description:
        This property returns the ending position in the text of the
        current match. The first character in the text is considered the 0th
        character.

        Note: This property only gives usable results after a call to NextMatch
          with a result of True.
      Returns:
        The position in the text of the last character in the current match.
      See also:
        TlvkRegExp.MatchStart
    }
    property MatchEnd: Integer read FEnd write SetMatchEnd;

    { Description:
        If you use groups in the regular expression pattern, then this
        property will return how many groups were matched in the current
        matched.

        Note: The 0th group matches the text that the whole regular expression
          pattern matched in the current match. As such, there will always be
          at least one group found.
      See also:
        TlvkRegExp.MatchedTextStart, TlvkRegExp.MatchedTextEnd,
        TlvkRegExp.MatchedText
    }
    property MatchedGroupCount: Integer read GetMatchedGroupCount;

    { Description:
        This property returns the starting position of the group specified.

        Note: The 0th group matches the text that the whole regular expression
          pattern matched in the current match. As such, MatchedTextStart[0]
          will return the same as MatchStart.
      Parameters:
        GroupIndex - The index of the group to return the start of.
      See also:
        TlvkRegExp.MatchedGroupCount, TlvkRegExp.MatchedTextEnd,
        TlvkRegExp.MatchedText
    }
    property MatchedTextStart[const GroupIndex: Integer]: Integer read GetMatchedTextStart;

    { Description:
        This property returns the ending position of the group specified.

        Note: The 0th group matches the text that the whole regular expression
          pattern matched in the current match. As such, MatchedTextEnd[0]
          will return the same as MatchEnd.
      Parameters:
        GroupIndex - The index of the group to return the end of.
      See also:
        TlvkRegExp.MatchedGroupCount, TlvkRegExp.MatchedTextStart,
        TlvkRegExp.MatchedText
    }
    property MatchedTextEnd[const GroupIndex: Integer]: Integer read GetMatchedTextEnd;

    { Description:
        This property returns the text of the group specified.

        Note: The 0th group matches the text that the whole regular expression
          pattern matched in the current match. As such, MatchedText[0]
          will return the text that the whole regular expression pattern
          matched in the current patch.
      Parameters:
        GroupIndex - The index of the group to return the text of.
      See also:
        TlvkRegExp.MatchedGroupCount, TlvkRegExp.MatchedTextStart,
        TlvkRegExp.MatchedTextEnd
    }
    property MatchedText[const GroupIndex: Integer]: string read GetMatchedText;


    { Description:
        This function checks to see if there is a named group in the matched
        text with the given name.
      Parameters:
        -
      See also:
        -
    }
    function HasNamedGroup(const GroupName: string): Boolean;

    // <COMBINE IRegExp.GetNamedGroupIndex@string>
    function GetNamedGroupIndex(const GroupName: string): Integer;

    // <ALIAS IRegExp.HasGroupContents@Integer>
    function HasGroupContents(const Index: Integer): Boolean;
    // <ALIAS IRegExp.HasNamedGroupContents@string>
    function HasNamedGroupContents(const GroupName: string): Boolean;

  published
    { Description:
        The PackageVersion property returns the version number for the LVK
        package. You can use this to compare against the version posted on the
        website, to see if you got the latest version or not.
    }
    property PackageVersion: TPackageVersion read GetPackageVersion write SetPackageVersion stored False;

    { Description:
        This property should be set to the text you wish to search for the
        pattern in.

        Note: Setting the Text property to a value will automatically call
        Restart, even if you set the Text property to the same value as it
        had before.
      See also:
        TlvkRegExp.Pattern, TlvkRegExp.Options
    }
    property Text: string read FText write SetText;

    { Description:
        This property holds the regular expression pattern to search for in
        the text.

        Note: Setting the Pattern property to a value will automatically call
        Restart, even if you set the Pattern property to the same value as it
        had before.
      See also:
        TlvkRegExp.Text, TlvkRegExp.Options
    }
    property Pattern: string read FPattern write SetPattern;

    { Description:
        The Options property is a set property that holds the different
        option values possible for the TlvkRegExp component.
      See also:
        TlvkRegExpOption, TlvkRegExpOptions
    }
    property Options: TlvkRegExpOptions read FOptions write SetOptions;
  end;

  { Description:
      This exception class is used when raising exceptions in the code related
      to TlvkRegExp.
    See also:
      TlvkRegExp
  }
  ElvkRegExp  = class(Exception)
  private
    FErrorCode  : Integer;

  public
    constructor Create(const ErrorCode: Integer; const Msg: string);
    constructor CreateFmt(const ErrorCode: Integer; const Fmt: string;
      const Args: array of const);

    property ErrorCode: Integer read FErrorCode;
  end;

  ElvkRegExpNamedGroup = class(ElvkRegExp);

const
  { Description:
      This text message is used when exceptions are raised due to a syntax
      error in the regular expression.
  }
  SCompilationError = 'Regular expression compilation error %s at position %d';

  { Description:
      This text message is used when exceptions are raised due to a syntax
      error in the regular expression.
  }
  SStudyError = 'Regular expression compilation study error %s';

{ Description:
    This function returns an object that supports the IRegExp interface, which
    can be used to do regular expression searches on text with.
  Parameters:
    Pattern - The regular expression to use for searching.
    Options - Various options that affect how the expression is used.
}
function NewRegExp(const Pattern: string;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS): IRegExp;

type
  PRegularExpression = ^TRegularExpression;
  TRegularExpression = record
    Disposable  : IDisposable;
    re          : Pointer;
    rex         : Pointer;
    Pattern     : string;
    Options     : TlvkRegExpOptions;
    Offsets     : array of Integer;
  end;

{ Description:
    This procedure prepares a regular expression for use with
    RegExpStringReplace and RegExpPos.
  Parameters:
    RegExp - The prepared regular expression that will be returned.
    RegularExpression - The regular expression pattern to prepare.
    Options - Options to use when compilign the regular expression.
}
procedure PrepareRegExp(out RegExp: TRegularExpression;
  const RegularExpression: string;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS); overload;

// <COMBINE RegExpStringReplace@string@TRegularExpression@string@Boolean@Boolean@Boolean>
function RegExpStringReplace(const S: string;
  const RegularExpression: string; const ReplacementPattern: string;
  const ReplaceAll: Boolean=True; const UseMacros: Boolean=False;
  const UsePosixTypeMacros: Boolean=False;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS): string; overload;

{ Description:
    These functions takes a string, S, as input parameter, locates each
    occurence of text that matches the regular expression, and replaces that
    text with the text given in the ReplacementPattern. You can select
    wether to replace only the first occurence or all the occurences,
    as well as wether the replacement pattern contains macros or not.

    A macro in the replacement pattern has the form of a integer, ranging from 0
    and upwards contained in curly braces . If you use groups in the regular
    expression, you can specify the contents of the group by index, and the
    macro will be replaced by the contents of that group.

    If you use macros, and want to add a curly brace in the replacement
    pattern, prefix it with a backslash \. A backslash must also be prefixed,
    and will thus look like this: \\.
  Parameters:
    S - The string to replace text in.
    RegularExpression - The regular expression to use.
    Options - Options to use when compiling the regular expression.
    RegExp - A prepared regular expression.
    ReplaceAll - Set to True to replace all occurences, set to False to
      replace just the first one.
    UseMacros - Set to True to use macros in the replacement string, set to
      False to insert the replacement pattern verbatim.
    UsePosixTypeMacros - If set to True, \123 macros should be used instead
      of the regular format, which uses the { character, but you would
      loose the format specifcations.
}
function RegExpStringReplace(const S: string;
  const RegExp: TRegularExpression; const ReplacementPattern: string;
  const ReplaceAll: Boolean=True; const UseMacros: Boolean=False;
  const UsePosixTypeMacros: Boolean=False): string; overload;

// <COMBINE RegExpPos@TRegularExpression@string@Integer>
function RegExpPos(const RegularExpression: string; const S: string;
  const Start: Integer=1;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS): Integer; overload;

{ Description:
    This is a regular expression version of the Pos function found in Delphi. It
    will return the position of the given expression, if found in the string S,
    or the value 0 if not found.
  Parameters:
    RegularExpression - The regular expression pattern.
    Options - Options to use when compiling the regular expression.
    RegExp - The prepared regular expression to use.
    S - The string to search through.
    Start - Where in the string to start searching.
}
function RegExpPos(const RegExp: TRegularExpression; const S: string;
  const Start: Integer=1): Integer; overload;

// <COMBINE RegExpMatch@TRegularExpression@string>
function RegExpMatch(const RegularExpression: string; const S: string;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS): Boolean; overload;

{ Description:
    This function checks to see if the regular expressions found in the
    S value or not. You use this to check if a given string matches a regular
    expression.
  Parameters:
    RegularExpression - The regular expression pattern.
    Options - Options to use when compiling the regular expression.
    RegExp - The prepared regular expression to use.
    S - The string to search through.
}
function RegExpMatch(const RegExp: TRegularExpression;
  const S: string): Boolean; overload;

type
  ERegExpScanf = class(Exception);

{ Description:
    This function is used to extract text from a string into individual
    variables.

    Note: Instead of giving a detailed explanation, see the example for more
      information.
    Note: The available data types are: longword, cardinal, shortint, single,
      double, extended, byte, word, integer, boolean, string, int64, pchar
  Parameters:
    RegExpFmt - The regular expression pattern used to match against the
      string.
    StringToScan - The string to scan, extracting found pieces into
      variables.
    Args - List of parameter types and variable pointers. See note and
      example. Two and two entries are used to specify the extraction for a
      single variable. The first entry is the index of the group to extract,
      along with the data type to extract to, and the second entry is a
      pointer to a variable of the correct type.
    Options - Options to use when matching.
  Example:
This example handles strings like "John is 20 years old", and extracts
'John' into a string variable, and 20 into an integer variable. It will
correctly handle "year" or "years", as well as different names and ages. It
will also handle different number of spaces between every word in the string.
<code>
<b>function</b> ExtractNameAge(<b>const</b> Input: <b>string</b>; <b>out</b> Name: <b>string</b>;
  <b>out</b> Age: Integer): Boolean;
<b>begin</b>
  Result := RegExpScanf('(.+)\s+is\s+([0-9]+)\s+years?\s+old',Input,
    ['1:STRING', @Name, '2:INTEGER', @Age);
<b>end</b>;
</code>
  Returns:
    True if it managed to extract data (a match was found), False if not.
}
function RegExpScanf(const RegExpFmt: string;
  const StringToScan: string;
  const Args: array of const;
  const Options: TlvkRegExpOptions=DEFAULT_OPTIONS;
  const IgnoreInvalidGroups: Boolean=False): Boolean; 

{ Description:
    This function translates a normal file mask with wildcards to a regular
    expression.

    For instance, "C:\*.txt" will be translated to "C:\\.*\.txt"
  Parameters:
    FileMaks - The filemask to convert.
}
function FileMaskToRegExp(const FileMask: string): string;

implementation

uses
  c_rtl, Math, Windows, pcre_intf;

type
  TlvkRegExpGroup = class(TInterfacedObject, IRegExpGroup)
  private
    FStartPosition  : Integer;
    FEndPosition    : Integer;
    FText           : string;

  protected
    // IRegExpGroup interface
    function GetStartPosition: Integer;
    function GetEndPosition: Integer;
    function GetText: string;

  public
    constructor Create(const StartPosition, EndPosition: Integer;
      const Text: string);
  end;

  TlvkRegExpIntfWrapper = class(TInterfacedObject, IRegExp, IPackageVersion)
  private
    FRegExp : TlvkRegExp;

  protected
    // IRegExp interface
    function GetPackageVersion: TPackageVersion;
    function GetPattern: string;
    procedure SetPattern(const NewValue: string);
    function MatchAgainst(const Text: string;
      const MatchStart: Integer=0; const MatchEnd: Integer=0): Boolean;
    function NextMatch: Boolean;
    function GetGroupCount: Integer;
    function GetGroups(const Index: Integer): IRegExpGroup;
    function GetNamedGroups(const Name: string): IRegExpGroup;
    function HasNamedGroup(const Name: string): Boolean;
    function HasGroupContents(const Index: Integer): Boolean;
    function HasNamedGroupContents(const Name: string): Boolean;
    function GetNamedGroupIndex(const GroupName: string): Integer;

  public
    constructor Create(const Pattern: string; const Options: TlvkRegExpOptions);
    destructor Destroy; override;
  end;

var
  UpperCaseTable  : array[Char] of Char;
  LowerCaseTable  : array[Char] of Char;

function NewRegExp(const Pattern: string;
  const Options: TlvkRegExpOptions): IRegExp;
begin
  Result := TlvkRegExpIntfWrapper.Create(Pattern, Options);
end;

{ TlvkRegExp }

const
  OptionValues    : array[TlvkRegExpOption, Boolean] of Integer = (
    (PCRE_UNGREEDY, 0),
    (PCRE_CASELESS, 0),
    (0, PCRE_MULTILINE),
    (0, PCRE_DOTALL),
    (0, PCRE_DOLLAR_ENDONLY),
    (PCRE_NOTBOL, 0),
    (PCRE_NOTEOL, 0),
    (PCRE_NOTEMPTY, 0),
    (0, PCRE_ANCHORED),
    (0, PCRE_EXTENDED),
    (PCRE_NO_AUTO_CAPTURE, 0),
    (0, PCRE_UTF8),
    (0, 0)
  );

  CompileOptions  = [roGreedy, roCaseSensitive, roMultiline, roAnchored, roDotAll, roAutoCapture, roIgnoreWhitespace];
  ExecOptions     = [roBol, roEol, roEmptyValid];

function GetCompileOptions(const Options: TlvkRegExpOptions): Integer;
var
  o : TlvkRegExpOption;
begin
  Result := 0;
  for o:=Low(o) to High(o) do
    if o in CompileOptions then
      Result := Result or OptionValues[o, o in Options];
end;

function GetExecOptions(const Options: TlvkRegExpOptions): Integer;
var
  o : TlvkRegExpOption;
begin
  Result := 0;
  for o:=Low(o) to High(o) do
    if o in ExecOptions then
      Result := Result or OptionValues[o, o in Options];
end;

procedure TlvkRegExp.Compile;
var
  rc            : Integer;
  OffsetsLength : Integer;
begin
  pcre_dispose(Fpcre, Fextra);

  Fpcre := nil;
  Fextra := nil;

  FOffsets := nil;

  Restart;

  if FPattern<>'' then
  begin
    Fpcre := pcre_compile(PChar(FPattern), GetCompileOptions(FOptions), @FError,
      @FErrorOffset, nil);
    if not Assigned(Fpcre) then
      raise ElvkRegExp.CreateFmt(PCRE_ERROR_COMPILE, SCompilationError, [string(FError), FErrorOffset]);

    if roStudy in FOptions then
      Fextra := pcre_study(Fpcre, 0, @FError);

    rc := pcre_fullinfo(Fpcre, Fextra, PCRE_INFO_CAPTURECOUNT, @OffsetsLength);
    if rc = 0 then
    begin
      OffsetsLength := (OffsetsLength + 1) * 3;
      SetLength(FOffsets, OffsetsLength);
    end else
      raise ElvkRegExp.Create(rc, 'Unable to retrieve number of capture groups');
  end;
end;

constructor TlvkRegExp.Create(AOwner: TComponent);
begin
  inherited;

  Pattern := '';
  FOptions := DEFAULT_OPTIONS;
end;

destructor TlvkRegExp.Destroy;
begin
  pcre_dispose(Fpcre, Fextra);

  inherited;
end;

function TlvkRegExp.GetMatchedGroupCount: Integer;
begin
  Result := FMatch;
end;

function TlvkRegExp.GetMatchedText(const GroupIndex: Integer): string;
var
  i1, i2  : Integer;
begin
  i1 := MatchedTextStart[GroupIndex]+1;
  i2 := MatchedTextEnd[GroupIndex]+1;

  Result := Copy(FText, i1, i2-i1 );
end;

function TlvkRegExp.GetMatchedTextEnd(const GroupIndex: Integer): Integer;
begin
  if (GroupIndex>=0) and (GroupIndex<FMatch) then
    Result := FOffsets[GroupIndex*2+1]
  else
    raise ElvkRegExp.Create(PCRE_ERROR_BOUNDS, 'GroupIndex parameter out of bounds in call to GetMatchedTextEnd');
end;

function TlvkRegExp.GetMatchedTextStart(
  const GroupIndex: Integer): Integer;
begin
  if (GroupIndex>=0) and (GroupIndex<FMatch) then
    Result := FOffsets[GroupIndex*2]
  else
    raise ElvkRegExp.Create(PCRE_ERROR_BOUNDS, 'GroupIndex parameter out of bounds in call to GetMatchedTextStart');
end;

function TlvkRegExp.GetNamedGroupIndex(const GroupName: string): Integer;
var
  rc  : Integer;
begin
  rc := pcre_get_stringnumber(Fpcre, PChar(GroupName));
  if rc = PCRE_ERROR_NOSUBSTRING then
    raise ElvkRegExpNamedGroup.CreateFmt(rc, 'No such named group: %s', [GroupName])
  else if rc > 0 then
    Result := rc
  else
    raise ElvkRegExp.CreateFmt(rc, 'Error %s when trying to find named group', [ErrorText(rc)]);
end;

function TlvkRegExp.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkRegExp.HasGroupContents(const Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < MatchedGroupCount) then
    Result := MatchedTextStart[Index] < MatchedTextEnd[Index]
  else
    Result := False;
end;

function TlvkRegExp.HasNamedGroup(const GroupName: string): Boolean;
var
  rc  : Integer;
begin
  rc := pcre_get_stringnumber(Fpcre, PChar(GroupName));
  if rc = PCRE_ERROR_NOSUBSTRING then
    Result := False
  else if rc > 0 then
    Result := True
  else
    raise ElvkRegExp.CreateFmt(rc, 'Error %s when trying to find named group', [ErrorText(rc)]);
end;

function TlvkRegExp.HasNamedGroupContents(
  const GroupName: string): Boolean;
var
  rc  : Integer;
begin
  rc := pcre_get_stringnumber(Fpcre, PChar(GroupName));
  if rc = PCRE_ERROR_NOSUBSTRING then
    Result := False
  else if rc > 0 then
    Result := HasGroupContents(rc)
  else
    raise ElvkRegExp.CreateFmt(rc, 'Error %s when trying to find named group', [ErrorText(rc)]);
end;

function TlvkRegExp.NextMatch: Boolean;
begin
  FStart := FEnd;

  FMatch := pcre_exec(Fpcre, Fextra, PChar(FText), Length(FText), FEnd,
    GetExecOptions(FOptions), @FOffsets[0], Length(FOffsets));

  Result := FMatch >= 0;
  if Result then
    FEnd := FOffsets[1];
end;

procedure TlvkRegExp.Restart;
begin
  FStart := 0;
  FEnd := 0;
end;

procedure TlvkRegExp.SetMatchEnd(const Value: Integer);
begin
  if FEnd<>Value then
  begin
    if Value<FStart then
      raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Cannot set MatchEnd to point before MatchStart');
    if Value>Length(FText) then
      raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Cannot set MatchEnd to point after end of text');
    if Value<0 then
      raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Cannot set MatchEnd to point before start of text');
    FEnd := Value;
  end;
end;

procedure TlvkRegExp.SetMatchStart(const Value: Integer);
begin
  if FStart<>Value then
  begin
    if Value<0 then
      raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Cannot set MatchStart to point before start of text');
    if Value>Length(FText) then
      raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Cannot set MatchStart to point after end of text');
    FStart := Value;
    if FEnd<FStart then
      FEnd := FStart;
  end;
end;

procedure TlvkRegExp.SetOptions(const Value: TlvkRegExpOptions);
begin
  if FOptions<>Value then
  begin
    FOptions := Value;
    Compile;
  end;
end;

procedure TlvkRegExp.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkRegExp.SetPattern(const Value: string);
begin
  if Value<>FPattern then
  begin
    FPattern := Value;
    Compile;
  end;
end;

procedure TlvkRegExp.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Restart;
  end;
end;

{ TlvkRegExpIntfWrapper }

constructor TlvkRegExpIntfWrapper.Create(const Pattern: string;
  const Options: TlvkRegExpOptions);
begin
  inherited Create;

  FRegExp := TlvkRegExp.Create(nil);
  FRegExp.Options := Options;
  FRegExp.Pattern := Pattern;
end;

destructor TlvkRegExpIntfWrapper.Destroy;
begin
  FRegExp.Free;

  inherited;
end;

function TlvkRegExpIntfWrapper.GetGroupCount: Integer;
begin
  Result := FRegExp.MatchedGroupCount;
end;

function TlvkRegExpIntfWrapper.GetGroups(
  const Index: Integer): IRegExpGroup;
begin
  if (Index < 0) or (Index >= FRegExp.MatchedGroupCount) then
    Result := nil
  else
    Result := TlvkRegExpGroup.Create(FRegExp.MatchedTextStart[Index],
      FRegExp.MatchedTextEnd[Index], FRegExp.MatchedText[Index]);
end;

function TlvkRegExpIntfWrapper.GetNamedGroupIndex(
  const GroupName: string): Integer;
begin
  Result := FRegExp.GetNamedGroupIndex(GroupName);
end;

function TlvkRegExpIntfWrapper.GetNamedGroups(
  const Name: string): IRegExpGroup;
begin
  Result := GetGroups(FRegExp.GetNamedGroupIndex(Name));
end;

function TlvkRegExpIntfWrapper.GetPackageVersion: TPackageVersion;
begin
  Result := FRegExp.PackageVersion;
end;

function TlvkRegExpIntfWrapper.GetPattern: string;
begin
  Result := FRegExp.Pattern;
end;

function TlvkRegExpIntfWrapper.HasGroupContents(
  const Index: Integer): Boolean;
begin
  Result := FRegExp.HasGroupContents(Index);
end;

function TlvkRegExpIntfWrapper.HasNamedGroup(const Name: string): Boolean;
begin
  Result := FRegExp.HasNamedGroup(Name);
end;

function TlvkRegExpIntfWrapper.HasNamedGroupContents(
  const Name: string): Boolean;
begin
  Result := FRegExp.HasNamedGroupContents(Name);
end;

function TlvkRegExpIntfWrapper.MatchAgainst(const Text: string;
  const MatchStart, MatchEnd: Integer): Boolean;
begin
  FRegExp.Text := Text;
  FRegExp.Restart;
  FRegExp.MatchStart := MatchStart;
  FRegExp.MatchEnd := MatchEnd;
  Result := NextMatch;
end;

function TlvkRegExpIntfWrapper.NextMatch: Boolean;
begin
  Result := FRegExp.NextMatch;
end;

procedure TlvkRegExpIntfWrapper.SetPattern(const NewValue: string);
begin
  FRegExp.Pattern := NewValue;
end;

{ TlvkRegExpGroup }

constructor TlvkRegExpGroup.Create(const StartPosition,
  EndPosition: Integer; const Text: string);
begin
  inherited Create;

  FStartPosition := StartPosition;
  FEndPosition := EndPosition;
  FText := Text;
end;

function TlvkRegExpGroup.GetEndPosition: Integer;
begin
  Result := FEndPosition;
end;

function TlvkRegExpGroup.GetStartPosition: Integer;
begin
  Result := FStartPosition;
end;

function TlvkRegExpGroup.GetText: string;
begin
  Result := FText;
end;

{ Support Functions }

function RegExpStringReplace(const S: string;
  const RegularExpression: string; const ReplacementPattern: string;
  const ReplaceAll, UseMacros: Boolean;
  const UsePosixTypeMacros: Boolean;
  const Options: TlvkRegExpOptions): string;
var
  RegExp  : TRegularExpression;
begin
  PrepareRegExp(RegExp, RegularExpression, Options);
  Result := RegExpStringReplace(S, RegExp, ReplacementPattern,
    ReplaceAll, UseMacros, UsePosixTypeMacros);
end;

function RegExpStringReplace(const S: string;
  const RegExp: TRegularExpression; const ReplacementPattern: string;
  const ReplaceAll, UseMacros, UsePosixTypeMacros: Boolean): string;
type
  PNode = ^TNode;
  TNode = record
    Buffer    : array[0..32767] of Char;
    InBuffer  : Integer;
    Next      : PNode;
  end;
var
  OptionsValue    : Integer;
  CurPtr          : PChar;
  Left            : Integer;
  Offsets         : array of Integer;
  OffsetsLength   : Integer;
  Any             : Boolean;
  Match           : Integer;
  More            : Boolean;
  ReplacementPtr  : PChar;
  ReplacementLen  : Integer;
  rc              : Integer;

  Head            : PNode;
  Tail            : PNode;
  Total           : Integer;

  procedure Output(const P: PChar; const Length: Integer); overload;
  var
    Left    : Integer;
    CurPtr  : PChar;
    Space   : Integer;
    ToCopy  : Integer;
  begin
    Left := Length;
    CurPtr := P;

    while Left > 0 do
    begin
      if Assigned(Head) then
      begin
        if Tail^.InBuffer = SizeOf(Tail^.Buffer) then
        begin
          New(Tail^.Next);
          Tail := Tail^.Next;
          Tail^.InBuffer := 0;
          Tail^.Next := nil;
        end;
      end else begin
        New(Head);
        Tail := Head;
        Head^.InBuffer := 0;
        Head^.Next := nil;
      end;

      Space := SizeOf(Tail^.Buffer) - Tail^.InBuffer;
      if Space > 0 then
      begin
        ToCopy := Min(Space, Left);
        Move(CurPtr^, Tail^.Buffer[Tail^.InBuffer], ToCopy);
        Inc(Tail^.InBuffer, ToCopy);
        Inc(CurPtr, ToCopy);
        Dec(Left, ToCopy);
        Inc(Total, ToCopy);
      end;
    end;
  end;

  procedure OutputCase(const P: PChar; const Length: Integer;
    const CaseTable: PChar); overload;
  var
    Index : Integer;
    a     : array[0..1] of Char;
  begin
    a[1] := #0;

    for Index := 0 to Length-1 do
    begin
      a[0] := CaseTable[Ord(P[Index])];
      Output(a, 1);
    end;
  end;

  procedure Output(const P: PChar); overload;
  begin
    Output(P, StrLen(P));
  end;

  procedure Output(const S: string); overload;
  begin
    Output(PChar(S), Length(S));
  end;

  procedure Output(const C: Char); overload;
  var
    a : array[0..1] of Char;
  begin
    a[0] := C;
    a[1] := #0;
    Output(a, 1);
  end;

  procedure OutputMacros;
  type
    TMacroFormat = (mfNone, mfUpperCase, mfLowerCase);
  var
    RepPtr  : PChar;
    Index   : Integer;
    Format  : TMacroFormat;
  begin
    RepPtr := ReplacementPtr;
    while RepPtr^ <> #0 do
    begin
      case RepPtr^ of
        '\':
          begin
            Inc(RepPtr);
            if UsePosixTypeMacros and (RepPtr^ in ['0'..'9']) then
            begin
              Index := 0;
              while RepPtr^ in ['0'..'9'] do
              begin
                Index := Index*10 + Ord(RepPtr^)-48;
                Inc(RepPtr);
              end;
              Output(CurPtr + Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2]);
            end else if RepPtr^ = #0 then
              Output(PChar('\ '), 1)
            else begin
              Output(RepPtr, 1);
              Inc(RepPtr);
            end;
          end;

        '{':
          begin
            if not UsePosixTypeMacros then
            begin
              Format := mfNone;
              Inc(RepPtr);
              Index := 0;
              while RepPtr^ in ['0'..'9'] do
              begin
                Index := Index*10 + Ord(RepPtr^)-48;
                Inc(RepPtr);
              end;

              if RepPtr^ = ':' then
              begin
                Inc(RepPtr);
                case RepPtr^ of
                  #0:
                    raise ElvkRegExp.CreateFmt(PCRE_ERROR_GENERIC, 'Missing macro format in replacement pattern {%d:}', [Index]);

                  'u', 'U':
                    Format := mfUpperCase;

                  'l', 'L':
                    Format := mfLowerCase;
                else
                  raise ElvkRegExp.CreateFmt(PCRE_ERROR_GENERIC, 'Invalid group macro format in replacement pattern {%d:%s}', [Index, RepPtr^]);
                end;
                Inc(RepPtr);
              end;

              if RepPtr^ = '}' then
                Inc(RepPtr)
              else
                raise ElvkRegExp.Create(PCRE_ERROR_GENERIC, 'Missing } in replacement pattern');


              if (Index >= 0) and (Index < Match) then
              begin
                case Format of
                  mfNone:
                    Output(CurPtr + Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2]);

                  mfUpperCase:
                    OutputCase(CurPtr + Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2], UpperCaseTable);

                  mfLowerCase:
                    OutputCase(CurPtr + Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2], LowerCaseTable);
                else
                  raise ElvkRegExp.CreateFmt(PCRE_ERROR_GENERIC, 'Invalid group identifier in replacement pattern {%d}', [Index]);
                end;
              end;
            end else begin
              Output(RepPtr, 1);
              Inc(RepPtr);
            end;
          end;
      else
        begin
          Output(RepPtr, 1);
          Inc(RepPtr);
        end;
      end;
    end;
  end;

  function GatherResult: string;
  var
    Node    : PNode;
    CurPtr  : PChar;
  begin
    SetLength(Result, Total);
    CurPtr := PChar(Result);

    while Assigned(Head) do
    begin
      Node := Head;
      Head := Head^.Next;

      Move(Node^.Buffer, CurPtr^, Node^.InBuffer);
      Inc(CurPtr, Node^.InBuffer);

      Dispose(Node);
    end;
  end;

begin
  if S = '' then
    Result := ''
  else if RegExp.Pattern = '' then
    Result := S
  else begin
    Head := nil;
    Total := 0;
    Any := False;
    try
      Assert(Assigned(RegExp.re));

      rc := pcre_fullinfo(RegExp.re, RegExp.rex, PCRE_INFO_CAPTURECOUNT, @OffsetsLength);
      if rc = 0 then
      begin
        OffsetsLength := (OffsetsLength + 1) * 3;
        SetLength(Offsets, OffsetsLength);
      end else
        raise ElvkRegExp.Create(rc, 'Unable to retrieve number of capture groups');

      ReplacementPtr := PChar(ReplacementPattern);
      ReplacementLen := Length(ReplacementPattern);

      CurPtr := PChar(S);
      Left := StrLen(CurPtr);
      OptionsValue := GetExecOptions(RegExp.Options);

      repeat
        Match := pcre_exec(RegExp.re, RegExp.rex, CurPtr, Left, 0, OptionsValue,
          @Offsets[0], Length(Offsets));
        More := Match >= 0;

        if More then
        begin
          Any := True;

          // First copy text that didn't match
          Output(CurPtr, Offsets[0]);

          // Then output replacement pattern
          if UseMacros then
            OutputMacros
          else
            Output(ReplacementPtr, ReplacementLen);

          // Then adjust for next match
          Inc(CurPtr, Offsets[1]);
          Dec(Left, Offsets[1]);

          if not ReplaceAll then
            More := False;
        end;
      until not More;

      if Any then
        Output(CurPtr);
    finally
      if Any then
        Result := GatherResult
      else
        Result := S;
    end;
  end;
end;

procedure UnPrepareRegExp(const Data: Pointer);
var
  RegExp  : PRegularExpression;
begin
  RegExp := PRegularExpression(Data);
  pcre_dispose(RegExp^.re, RegExp^.rex);
end;

procedure PrepareRegExp(out RegExp: TRegularExpression;
  const RegularExpression: string;
  const Options: TlvkRegExpOptions);
var
  Error         : PChar;
  ErrorOffset   : Integer;
  rc            : Integer;
  OffsetsLength : Integer;
begin
  RegExp.Pattern := RegularExpression;
  RegExp.Options := Options;
  RegExp.re := nil;
  RegExp.rex := nil;
  RegExp.Disposable := NewDisposable(@RegExp, UnprepareRegExp);
  RegExp.Offsets := nil;

  if RegExp.Pattern <> '' then
  begin
    RegExp.re := pcre_compile(PChar(RegularExpression), GetCompileOptions(Options),
      @Error, @ErrorOffset, nil);
    try
      if RegExp.re = nil then
        raise ElvkRegExp.CreateFmt(PCRE_ERROR_COMPILE, SCompilationError, [string(Error), ErrorOffset]);

      if roStudy in Options then
      begin
        RegExp.rex := pcre_study(RegExp.re, 0, @Error);
        if Error <> nil then
          raise ElvkRegExp.CreateFmt(PCRE_ERROR_STUDY, SStudyError, [string(Error)]);
      end;

      rc := pcre_fullinfo(RegExp.re, RegExp.rex, PCRE_INFO_CAPTURECOUNT, @OffsetsLength);
      if rc = 0 then
      begin
        OffsetsLength := (OffsetsLength + 1) * 3;
        SetLength(RegExp.Offsets, OffsetsLength);
      end else
        raise ElvkRegExp.Create(rc, 'Unable to retrieve number of capture groups');
    except
      free(RegExp.re);
      RegExp.re := nil;
      raise;
    end;
  end;
end;

function RegExpPos(const RegularExpression: string; const S: string;
  const Start: Integer; const Options: TlvkRegExpOptions): Integer; overload;
var
  RegExp  : TRegularExpression;
begin
  PrepareRegExp(RegExp, RegularExpression, Options);
  Result := RegExpPos(RegExp, S, Start);
end;

function RegExpPos(const RegExp: TRegularExpression; const S: string;
  const Start: Integer): Integer; overload;
begin
  if RegExp.Pattern = '' then
    Result := 0
  else begin
    Assert(Assigned(RegExp.re));
    Assert(Start >= 1);

    if pcre_exec(RegExp.re, RegExp.rex, PChar(S), Length(S), Start-1,
      GetExecOptions(RegExp.Options), @RegExp.Offsets[0], Length(RegExp.Offsets)) >= 0 then
    begin
      Result := RegExp.Offsets[0] + 1;
    end else
      Result := 0;
  end;
end;

function FileMaskToRegExp(const FileMask: string): string;
var
  Index : Integer;
  Expr  : string;

  procedure AppendExpr;
  begin
    if Expr <> '' then
    begin
      if Result <> '' then
        Result := Result + '|';
      Result := Result + Expr;
      Expr := '';
    end;
  end;

begin
  Result := '';

  if FileMask <> '' then
  begin
    Expr := '';
    
    for Index := 1 to Length(FileMask) do
    begin
      case FileMask[Index] of
        ';':
          AppendExpr;

        '.', '(', ')', '\':
          Expr := Expr + '\' + FileMask[Index];

        '?':
          Expr := Expr + '.';

        '*':
          Expr := Expr + '[^\\]*';

        ' ':
          if Expr <> '' then
            Expr := Expr + ' ';
            
      else
        Expr := Expr + FileMask[Index];
      end;
    end;
    AppendExpr;

    Result := '(^' + Result + '|\\' + Result + ')$';
  end;
end;

function RegExpScanf(const RegExpFmt: string;
  const StringToScan: string; const Args: array of const;
  const Options: TlvkRegExpOptions;
  const IgnoreInvalidGroups: Boolean): Boolean;
var
  RegExp        : IRegExp;
  ScanRegExp    : IRegExp;
  Index         : Integer;
  ScanSelector  : string;
  ValuePointer  : Pointer;
  LastGroup     : Integer;
  ScanGroup     : Integer;
  ScanGroupName : string;
  ScanType      : string;
  MaxSize       : Integer;
  ErrorPos      : Integer;

  function StrToBool(const s: string): Boolean;
  const
    TrueValues  : array[1..4] of string = (
      'TRUE', 'ON', '1', 'YES'
    );
    FalseValues : array[1..4] of string = (
      'FALSE', 'OFF', '0', 'NO'
    );
  var
    Index : Integer;
  begin
    for Index := Low(TrueValues) to High(TrueValues) do
    begin
      if CompareText(TrueValues[Index], s) = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
    for Index := Low(FalseValues) to High(FalseValues) do
    begin
      if CompareText(FalseValues[Index], s) = 0 then
      begin
        Result := False;
        Exit;
      end;
    end;

    raise Exception.CreateFmt('Unable to determine if BOOL value is True or False: %s', [s]);
  end;

{$IFNDEF DELPHI6UP}
type
  PCardinal = ^Cardinal;
  PBoolean = ^Boolean;
{$ENDIF}
begin
  RegExp := NewRegExp(RegExpFmt, Options);
  if RegExp.MatchAgainst(StringToScan) then
  begin
    ScanRegExp := NewRegExp('^(([a-zA-Z_0-9]{1,3}):)?((longword|cardinal|shortint|single|double|extended|byte|word|integer|boolean|string|int64|pchar|char)(\(([0-9]{1,5})\))?)$');

    Index := Low(Args);
    LastGroup := 0;

    while Index <= High(Args) do
    begin
      Assert((Args[Index].VType = vtPChar) or (Args[Index].VType = vtAnsiString));
      Assert(Index < High(Args));
      Assert(Args[Index+1].VType = vtPointer);

      case Args[Index].VType of
        vtPChar:
          ScanSelector := Args[Index].VPChar;

        vtAnsiString:
          ScanSelector := string(Args[Index].VAnsiString);
      end;

      ValuePointer := Args[Index+1].VPointer;

      if ScanRegExp.MatchAgainst(ScanSelector) then
      begin
        if ScanRegExp.GroupCount in [4..7] then
        begin
          if ScanRegExp.Groups[2].Text <> '' then
          begin
            ScanGroupName := ScanRegExp.Groups[2].Text;
            Val(ScanGroupName, ScanGroup, ErrorPos);
            if ErrorPos > 0 then
              ScanGroup := RegExp.GetNamedGroupIndex(ScanGroupName);
          end else
            ScanGroup := LastGroup + 1;

          ScanType := UpperCase(ScanRegExp.Groups[4].Text);

          if ScanRegExp.GroupCount = 7 then
            MaxSize := StrToInt(ScanRegExp.Groups[6].Text)
          else
            MaxSize := 0;
        end else
          raise ERegExpScanf.CreateFmt('Invalid scan selector: %s', [ScanSelector]);

        if ScanGroup < RegExp.GroupCount then
        begin
          if ScanType = 'INTEGER' then
            PInteger(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'INT64' then
            PInt64(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'LONGWORD' then
            PLongWord(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'CARDINAL' then
            PCardinal(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'SHORTINT' then
            PShortInt(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'STRING' then
          begin
            if MaxSize > 0 then
              PString(ValuePointer)^ := Copy(RegExp.Groups[ScanGroup].Text, 1, MaxSize)
            else
              PString(ValuePointer)^ := RegExp.Groups[ScanGroup].Text;
          end else if ScanType = 'BOOLEAN' then
            PBoolean(ValuePointer)^ := StrToBool(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'PCHAR' then
          begin
            if MaxSize > 0 then
              StrLCopy(ValuePointer, PChar(RegExp.Groups[ScanGroup].Text), MaxSize)
            else
              StrCopy(ValuePointer, PChar(RegExp.Groups[ScanGroup].Text))
          end else if ScanType = 'WORD' then
            PWord(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'BYTE' then
            PByte(ValuePointer)^ := StrToInt(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'DOUBLE' then
            PDouble(ValuePointer)^ := StrToFloat(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'SINGLE' then
            PSingle(ValuePointer)^ := StrToFloat(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'EXTENDED' then
            PExtended(ValuePointer)^ := StrToFloat(RegExp.Groups[ScanGroup].Text)
          else if ScanType = 'CHAR' then
            PChar(ValuePointer)^ := RegExp.Groups[ScanGroup].Text[1];
        end else if IgnoreInvalidGroups then
        begin
          if ScanType = 'INTEGER' then
            PInteger(ValuePointer)^ := 0
          else if ScanType = 'INT64' then
            PInt64(ValuePointer)^ := 0
          else if ScanType = 'LONGWORD' then
            PLongWord(ValuePointer)^ := 0
          else if ScanType = 'CARDINAL' then
            PCardinal(ValuePointer)^ := 0
          else if ScanType = 'SHORTINT' then
            PShortInt(ValuePointer)^ := 0
          else if ScanType = 'STRING' then
            PString(ValuePointer)^ := ''
          else if ScanType = 'BOOLEAN' then
            PBoolean(ValuePointer)^ := False
          else if ScanType = 'PCHAR' then
            PChar(ValuePointer)^ := #0
          else if ScanType = 'WORD' then
            PWord(ValuePointer)^ := 0
          else if ScanType = 'BYTE' then
            PByte(ValuePointer)^ := 0
          else if ScanType = 'DOUBLE' then
            PDouble(ValuePointer)^ := 0.0
          else if ScanType = 'SINGLE' then
            PSingle(ValuePointer)^ := 0.0
          else if ScanType = 'EXTENDED' then
            PExtended(ValuePointer)^ := 0.0
          else if ScanType = 'CHAR' then
            PChar(ValuePointer)^ := #0;
        end else
          raise ERegExpScanf.CreateFmt('Invalid scan group: %s', [ScanSelector]);

        LastGroup := ScanGroup;
      end else
        raise ERegExpScanf.CreateFmt('Invalid scan selector: %s', [ScanSelector]);

      Inc(Index, 2);
    end;

    Result := True;
  end else
    Result := False;
end;

function RegExpMatch(const RegularExpression: string; const S: string;
  const Options: TlvkRegExpOptions): Boolean;
begin
  Result := RegExpPos(RegularExpression, S, 1, Options) > 0;
end;

function RegExpMatch(const RegExp: TRegularExpression;
  const S: string): Boolean;
begin
  Result := RegExpPos(RegExp, S) > 0;
end;

procedure InitializeCaseTables;
var
  c : Char;
begin
  for c := #0 to #255 do
  begin
    LowerCaseTable[c] := AnsiLowerCase(c)[1];
    UpperCaseTable[c] := AnsiUpperCase(c)[1];
  end;
end;

{ ElvkRegExp }

constructor ElvkRegExp.Create(const ErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);

  FErrorCode := ErrorCode;
end;

constructor ElvkRegExp.CreateFmt(const ErrorCode: Integer;
  const Fmt: string; const Args: array of const);
begin
  inherited CreateFmt(Fmt, Args);

  FErrorCode := ErrorCode;
end;

initialization
  InitializeCaseTables;
end.

