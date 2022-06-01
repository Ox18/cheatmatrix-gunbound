{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains functions for using the CodeSite EnterMethod and
    ExitMethod methods through a cleaner interface, as well as loading initial
    CodeSite setup from a .ini file.

    The .ini file is named the same as the application .exe file, except with a
    .ini extension, and can have the following section:

    [CodeSite]
    UseDispatcher=...
    DestinationDetails=...
    Category=...
    CategoryColor=...
    DateTimeFormat=...
    HexNumPrefix=...
    HexViewWidth=...
    QuoteChar=...
    Enabled=...
    ClearOnStart=...
    UseLogFile=...
    LogFile=...
    MaxSize=...
    MaxParts=...
    KeepOpen=...
    UseTCPIP=...
    Host=...
    Port=...

    Here's a description of each:

    UseDispatcher=True|False
    If you set this to True, the normal CodeSite dispatcher is used, otherwise
    you must supply either UseLogFile=True or UseTCPIP=True (but not both,
    you'll need the dispatcher if you want to multicast the events).

    DestinationDetails=<details as in the normal codesite object>
    This is only used if you use the dispatcher. If you don't include this,
    or set it to blank, the viewer is used by default.

    Category=Categoryname
    Use this to specify the category name to mark all messages with. This is
    blank by default.

    CategoryColor=clXYZ
    Use this to specify the color to use, in normal Delphi syntax, clRed,
    clWhite, etc. clWindow is used by default if omitted.

    DateTimeFormat=format string
    Specifies the format of date/time data in the log. See the codesite object
    for details.

    HexNumPrefix=$
    Specify how to prefix hexadecimal values in the log file. $ is used by
    default if omitted.

    HexViewWidth=8
    Specify the number of bytes per line in a hexadecimal dump of a file or
    similar. 8 is used by default if omitted.

    QuoteChar='
    Specify what kind of character to quote strings with, ' is used by default
    if omitted.

    Enabled=True|False
    Use to enable or disable CodeSite logging from this application. This only
    affects the CodeSite object. If the application creates or manipulates this
    flag by itself then that overrides the setting in this file. Note also that
    if you elect to use the dispatcher, and you run the application on a machine
    without CodeSite installed, then no logging will take place. The default
    is True, provided you either use a file or tcp/ip or got the dispatcher
    installed.

    ClearOnStart=True|False
    Set to True to clear the log file/log window when the application starts.
    If you don't use the dispatcher and log directly to file, then the log
    files will be deleted when the application starts. The default is False,
    which will append to the log files/window.

    UseLogFile=True|False
    Set to True to log directly to file. This is only used if you elect to not
    use the dispatcher. Default is to log to a .csl file with the same base
    filename as the .exe filename, but you can override this with the LogFile
    ini file setting.

    LogFile=full path+filename
    Set this to override where to place the log files when you log directly to
    a file, and what to name the files. If you set MaxParts to a value above 1,
    then the filename will get a number appended to it to distinguish the
    parts from each other. The file with the lowest number is the latest log
    file and the file with the highest number is the oldest log file.

    MaxSize=1024
    Set to specify the maximum size of each part of a multi-part log file. The
    default is to use a size of 0 and only one logfile, which means it will
    grow indefinitely. This value is only used if you log directly to a file
    and elect to not use the dispatcher.

    MaxParts=3
    Set to specify how many log file parts to use when logging directly to
    a file. The default is 1, which means a single, unlimited size, log file
    will be used.

    KeepOpen=True|False
    Set to True if you want the log file to be kept open between log messages.
    This will improve performance since the file buffers doesn't have to be
    flushed each time, but it will also mean that if the application is
    killed or terminates a bit out of the ordinary then the last messages
    might not be saved to the log file. This value is only used if you log
    directly to a file and elect to not use the dispatcher.

    UseTCPIP=True|False
    Set to True if you want to log to a dispatcher application running on
    a remote server. You must also set the Host value to specify which remote
    host to use.

    Host=Servername
    Specifies which remote host to send the logmessages to. This value is only
    used if you log directly to a dispatcher on a remote host and elect to not
    use the dispatcher on the local machine.

    Port=3434
    Specifies which port to connect to on the remote host to send the
    logmessages to. Default value is 3434. This value is only used if you log
    directly to a dispatcher on a remote host and elect to not use the
    dispatcher on the local machine.
}
unit lvkCodeSiteUtils;

// $Author: Lasse V. Karlsen $
// $Revision: 3 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkCodeSiteUtils.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  CSIntf;

function AutoScope(const CSObj: TCodeSite; const ScopeName: string): IUnknown; overload;
function AutoScope(const CSObj: TCodeSite; const ScopeName, Fmt: string; const Args: array of const): IUnknown; overload;
function AutoScope(const CSObj: TCodeSite; const Obj: TObject; const MethodName: string): IUnknown; overload;
function AutoScope(const CSObj: TCodeSite; const Obj: TObject; const MethodName, Fmt: string; const Args: array of const): IUnknown; overload;

function AutoScope(const ScopeName: string): IUnknown; overload;
function AutoScope(const ScopeName, Fmt: string; const Args: array of const): IUnknown; overload;
function AutoScope(const Obj: TObject; const MethodName: string): IUnknown; overload;
function AutoScope(const Obj: TObject; const MethodName, Fmt: string; const Args: array of const): IUnknown; overload;

implementation

uses
  CSFileDirectIntf, CSTcpDirectIntf,
  lvkRegExp,
  Windows, SysUtils, IniFiles, Classes, Graphics;

type
  TAutoScope = class(TInterfacedObject)
  private
    FScopeName  : string;
    FCodeSite   : TCodeSite;

  public
    constructor Create(const CSObj: TCodeSite; const ScopeName: string); overload;
    constructor Create(const CSObj: TCodeSite; const ScopeName, Fmt: string; const Args: array of const); overload;
    destructor Destroy; override;
  end;

procedure SetupCodeSiteFromIniFile;
var
  IniFile : TIniFile;
const
  SECTION_NAME    = 'CodeSite';
  USE_DISPATCHER  = 'UseDispatcher';
  DESTINATION     = 'DestinationDetails';
  CATEGORY        = 'Category';
  CATEGORY_COLOR  = 'CategoryColor';
  DATETIMEFORMAT  = 'DateTimeFormat';
  HEXNUMPREFIX    = 'HexNumPrefix';
  HEXVIEWWIDTH    = 'HexViewWidth';
  QUOTECHAR       = 'QuoteChar';
  ENABLED         = 'Enabled';

  USE_LOGFILE     = 'UseLogFile';
  FILE_PATH       = 'LogFile';
  FILE_SIZE       = 'MaxSize';
  FILE_PARTS      = 'MaxParts';
  KEEP_OPEN       = 'KeepOpen';

  USE_TCPIP       = 'UseTCPIP';
  TCP_HOST        = 'Host';
  TCP_PORT        = 'Port';

  CLEAR_ON_START  = 'ClearOnStart';
  BoolStr         : array[Boolean] of string = ('False', 'True');
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.csi'));
  try
    if RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, USE_DISPATCHER, '')) then
    begin
      CodeSite.DestinationDetails := IniFile.ReadString(SECTION_NAME, DESTINATION, '');
    end else begin
      if RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, USE_TCPIP, '')) then
      begin
        CodeSite.Free;
        CodeSite := TCSTcpDirect.Create(nil);
        TCSTcpDirect(CodeSite).Host := IniFile.ReadString(SECTION_NAME, TCP_HOST, TCSTcpDirect(CodeSite).Host);
        TCSTcpDirect(CodeSite).Port := IniFile.ReadInteger(SECTION_NAME, TCP_PORT, TCSTcpDirect(CodeSite).Port);
      end else if RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, USE_LOGFILE, '')) then
      begin
        CodeSite.Free;
        CodeSite := TCSFileDirect.Create(nil);
        TCSFileDirect(CodeSite).Path := IniFile.ReadString(SECTION_NAME, FILE_PATH, ChangeFileExt(ParamStr(0), '.csl'));
        TCSFileDirect(CodeSite).MaxSize := IniFile.ReadInteger(SECTION_NAME, FILE_SIZE, TCSFileDirect(CodeSite).MaxSize);
        TCSFileDirect(CodeSite).MaxParts := IniFile.ReadInteger(SECTION_NAME, FILE_PARTS, TCSFileDirect(CodeSite).MaxParts);
        TCSFileDirect(CodeSite).KeepOpen := RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, KEEP_OPEN, ''));
      end;
    end;

    CodeSite.Category := IniFile.ReadString(SECTION_NAME, CATEGORY, CodeSite.Category);
    CodeSite.CategoryColor := StringToColor(IniFile.ReadString(SECTION_NAME, CATEGORY_COLOR, ColorToString(CodeSite.CategoryColor)));
    CodeSite.DateTimeFormat := IniFile.ReadString(SECTION_NAME, DATETIMEFORMAT, CodeSite.DateTimeFormat);
    CodeSite.HexNumPrefix := IniFile.ReadString(SECTION_NAME, HEXNUMPREFIX, CodeSite.HexNumPrefix);
    CodeSite.HexViewWidth := IniFile.ReadInteger(SECTION_NAME, HEXVIEWWIDTH, CodeSite.HexViewWidth);
    if IniFile.ReadString(SECTION_NAME, QUOTECHAR, CodeSite.QuoteChar) <> '' then
      CodeSite.QuoteChar := IniFile.ReadString(SECTION_NAME, QUOTECHAR, CodeSite.QuoteChar)[1];
    CodeSite.Enabled := RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, ENABLED, BoolStr[CodeSite.Enabled]));

    if CodeSite.Enabled and RegExpMatch('^(yes|1|true)$', IniFile.ReadString(SECTION_NAME, CLEAR_ON_START, '')) then
    begin
      CodeSite.Clear;
      if CodeSite is TCSFileDirect then
        TCSFileDirect(CodeSite).ClearLogFile;
    end;
  finally
    IniFile.Free;
  end;
end;

function AutoScope(const ScopeName: string): IUnknown;
begin
  if CodeSite.Enabled then
    Result := AutoScope(CodeSite, ScopeName)
  else
    Result := nil;
end;

function AutoScope(const ScopeName, Fmt: string; const Args: array of const): IUnknown;
begin
  if CodeSite.Enabled then
    Result := AutoScope(CodeSite, ScopeName, Fmt, Args)
  else
    Result := nil;
end;

function AutoScope(const Obj: TObject; const MethodName: string): IUnknown;
begin
  if CodeSite.Enabled then
    Result := AutoScope(CodeSite, Obj, MethodName)
  else
    Result := nil;
end;

function AutoScope(const Obj: TObject; const MethodName, Fmt: string; const Args: array of const): IUnknown;
begin
  if CodeSite.Enabled then
    Result := AutoScope(CodeSite, Obj, MethodName, Fmt, Args)
  else
    Result := nil;
end;

function AutoScope(const CSObj: TCodeSite; const ScopeName: string): IUnknown;
begin
  if Assigned(CSObj) and CSObj.Enabled then
    Result := TAutoScope.Create(CSObj, ScopeName)
  else
    Result := nil;
end;

function AutoScope(const CSObj: TCodeSite; const ScopeName, Fmt: string; const Args: array of const): IUnknown;
begin
  if Assigned(CSObj) and CSObj.Enabled then
    Result := TAutoScope.Create(CSObj, ScopeName, Fmt, Args)
  else
    Result := nil;
end;

function AutoScope(const CSObj: TCodeSite; const Obj: TObject; const MethodName: string): IUnknown;
begin
  if Assigned(CSObj) and CSObj.Enabled then
  begin
    if Assigned(Obj) then
      Result := AutoScope(CSObj, Format('%s.%s', [Obj.ClassName, MethodName]))
    else
      Result := AutoScope(CSObj, Format('(nil).%s', [MethodName]));
  end else
    Result := nil;
end;

function AutoScope(const CSObj: TCodeSite; const Obj: TObject; const MethodName, Fmt: string; const Args: array of const): IUnknown;
begin
  if Assigned(CSObj) and CSObj.Enabled then
  begin
    if Assigned(Obj) then
      Result := AutoScope(CSObj, Format('%s.%s', [Obj.ClassName, MethodName]), Fmt, Args)
    else
      Result := AutoScope(CSObj, Format('(nil).%s', [MethodName]), Fmt, Args);
  end else
    Result := nil;
end;

{ TAutoScope }

constructor TAutoScope.Create(const CSObj: TCodeSite;
  const ScopeName: string);
begin
  inherited Create;

  Assert(Assigned(CSObj));
  Assert(ScopeName <> '');

  FScopeName := ScopeName;
  FCodeSite := CSObj;
  FCodeSite.EnterMethod(ScopeName);
end;

constructor TAutoScope.Create(const CSObj: TCodeSite; const ScopeName,
  Fmt: string; const Args: array of const);
begin
  inherited Create;

  Assert(Assigned(CSObj));
  Assert(ScopeName <> '');

  FScopeName := ScopeName;
  FCodeSite := CSObj;
  FCodeSite.EnterMethod(ScopeName, Fmt, Args);
end;

destructor TAutoScope.Destroy;
begin
  FCodeSite.ExitMethod(FScopeName);

  inherited;
end;

{ Main Application Support Code }

function ParamString: string;
var
  Temp  : string;
  p     : PChar;
begin
  Temp := GetCommandLine;
  p := PChar(Temp);

  if Temp <> '' then
  begin
    if Temp[1] in ['"', ''''] then
      AnsiExtractQuotedStr(p, Temp[1]);

    Result := Trim(p);
  end else
    Result := '';

  if Result = '' then
    Result := '(blank)';
end;

initialization
  SetupCodeSiteFromIniFile;
  CodeSite.EnterMethod('Application Start', 'Filename=%s, Parameters=%s', [ParamStr(0), ParamString]);
finalization
  CodeSite.ExitMethod('Application Terminate', 'Filename=%s, Parameters=%s', [ParamStr(0), ParamString]);
end.
