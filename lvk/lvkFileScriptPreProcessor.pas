{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the File script preprocessor class.
}
unit lvkFileScriptPreProcessor;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFileScriptPreProcessor.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkActiveScriptPreProcessor;

type
  TFileScriptPreProcessor = class(TScriptPreProcessorHandler)
  private
    FLoops      : TStrings;
    FVariables  : TStrings;

    function PreProcessFileLine(const Language, Line: string): string;
    function ChangeLineVariable(const Language, Line: string): string;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Loops: TStrings read FLoops;
    property Variables: TStrings read FVariables;

    procedure Execute(const Language: String; const Code: TStrings); override;
    class function HandlesLanguage(const Language: String): Boolean; override;
  end;

  EFileScriptPreProcessor = class(Exception);

implementation

uses
  lvkRegExp;

const
  GlobalSQLObject = 'lvkSQLGlobal';

type
  TParameterList = array of string;
  TFileFunction = function(const PreProcessor: TFileScriptPreProcessor;
    const Language, Line: string): string;

(* Not in use ??
function MakeFunctionCall(
  const Language, MethodName: string; const Parameters: array of string;
  const IsFunction: Boolean=False): string;
var
  JavaScript  : Boolean;
  Index       : Integer;
begin
  Result := Format('%s.%s', [GlobalSQLObject, MethodName]);

  if RegExpMatch('^J(ava)?Script$', Language) then
  begin
    JavaScript := True;
    Result := Result + '(';
  end else begin
    JavaScript := False;
    if IsFunction then
      Result := Result + '('
    else
      Result := Result + ' ';
  end;

  for Index := Low(Parameters) to High(Parameters) do
  begin
    Result := Result + Parameters[Index];
    if Index < High(Parameters) then
      Result := Result + ', ';
  end;

  if JavaScript then
  begin
    if IsFunction then
      Result := Result + ')'
    else
      Result := Result + ');'
  end else if IsFunction then
    Result := Result + ')';
end;
*)

function QuoteString(const Language, Value: string): string;
begin
  if RegExpMatch('^J(ava)?Script$', Language) then
    Result := '''' + RegExpStringReplace(Value, '''', '\''') + ''''
  else
    Result := '"' + RegExpStringReplace(Value, '"', '""') + '"';
end;

function FixupFileVar(const s: string): string;
begin
  Assert(s[1] = '@');
  Result := 'lvkfile' + Copy(s, 2, Length(s)-1);
end;

function FixupLineVar(const s: string): string;
begin
  Assert(s[1] = '@');
  Result := 'lvkline' + Copy(s, 2, Length(s)-1);
end;

function FILE_FOREACH(const PreProcessor: TFileScriptPreProcessor;
  const Language, SQL: string): string;
var
  VariableName  : string;
  FileName      : string;
  Matching      : string;
  ForEach       : Boolean;
  Parameters    : TParameterList;
begin
  Result := '';
  Parameters := nil;
  FileName := '';
  Matching := '';

  if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\("([^"]+)"\)$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\(''([^'']+)''\)$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\("([^"]+)"\)\s+matching\s+"(.*)"$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\(''([^'']+)''\)\s+matching\s+"(.*)"$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\("([^"]+)"\)\s+matching\s+''(.*)''$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\(''([^'']+)''\)\s+matching\s+''(.*)''$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
  begin
    ForEach := True;
    FileName := QuoteString(Language, FileName);
  end else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\((.+)\)$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName]) then
    ForEach := True
  else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\((.+)\)\s+matching\s+"(.*)"$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
    ForEach := True
  else if RegExpScanf('^for\s+each\s+line\s+(@[a-zA-Z_0-9]+)\s+in\s+\((.+)\)\s+matching\s+''(.*)''$', SQL,
    ['STRING', @VariableName, 'STRING', @FileName, 'STRING', @Matching]) then
    ForEach := True
  else
    ForEach := False;

  if ForEach then
  begin
    if PreProcessor.Loops.IndexOf(VariableName) >= 0 then
      raise EFileScriptPreProcessor.Create('You must use different variable names when nesting # for each calls');

    if RegExpMatch('^J(ava)?Script$', Language) then
    begin
      Result := FixupFileVar(VariableName) + ' = ' +
        'CreateObject(''Scripting.FileSystemObject'').OpenTextFile(' +
        StringReplace(Filename, '\', '\\', [rfReplaceAll]) +
        ');'#13#10'while (!' + FixupFileVar(VariableName) +
        '.AtEndOfStream) {'#13#10'  ' + FixupLineVar(VariableName) + ' = ' +
        FixupFileVar(VariableName) + '.ReadLine();';

      if Matching <> '' then
        Result := Result + #13#10'  if (' + GlobalSQLObject +
          '.RegExpMatch(''' +
          StringReplace(Matching, '\', '\\', [rfReplaceAll]) + ''', ' +
          FixupLineVar(VariableName) + ')) {';
    end else begin
      Result := 'Set ' + FixupFileVar(VariableName) + ' = ' +
        'CreateObject("Scripting.FileSystemObject").OpenTextFile(' + Filename +
        ')'#13#10'Do While Not ' + FixupFileVar(VariableName) +
        '.AtEndOfStream'#13#10'  ' + FixupLineVar(VariableName) + ' = ' +
        FixupFileVar(VariableName) + '.ReadLine';

      if Matching <> '' then
        Result := Result + #13#10'  If ' + GlobalSQLObject +
          '.RegExpMatch(''' + Matching +''', ' +
          FixupLineVar(VariableName) + ') Then';
    end;

    PreProcessor.Variables.Add(FixupFileVar(VariableName));
    PreProcessor.Variables.Add(FixupLineVar(VariableName));

    if Matching <> '' then
      PreProcessor.Loops.Add('+' + VariableName)
    else
      PreProcessor.Loops.Add(VariableName);
  end;
end;

function FILE_NEXT(const PreProcessor: TFileScriptPreProcessor;
  const Language, SQL: string): string;
var
  LoopName  : string;
begin
  if RegExpMatch('^next\s+line$', SQL) then
  begin
    if PreProcessor.Loops.Count = 0 then
      raise EFileScriptPreProcessor.Create('# next line without # for each line');

    LoopName := PreProcessor.Loops[PreProcessor.Loops.Count-1];
    PreProcessor.Loops.Delete(PreProcessor.Loops.Count-1);

    if LoopName[1] = '+' then
    begin
      if RegExpMatch('^J(ava)?Script$', Language) then
        Result := '} }'
      else
        Result := 'End If'#13#10'Loop';
    end else begin
      if RegExpMatch('^J(ava)?Script$', Language) then
        Result := '}'
      else
        Result := 'Loop';
    end;
  end;
end;

const
  FileFunctions  : array[1..2] of TFileFunction = (
    FILE_FOREACH,
    FILE_NEXT
  );

{ TVBScriptPreProcessor }

function TFileScriptPreProcessor.ChangeLineVariable(const Language,
  Line: string): string;
begin
  Result := RegExpStringReplace(Line, '@([a-zA-Z_0-9]+)', 'lvkline{1}', True,
    True);
end;

constructor TFileScriptPreProcessor.Create;
begin
  inherited;

  FLoops := TStringList.Create;
  FVariables := TStringList.Create;
  TStringList(FVariables).Duplicates := dupIgnore;
  TStringList(FVariables).Sorted := True;
end;

destructor TFileScriptPreProcessor.Destroy;
begin
  FVariables.Free;
  FLoops.Free;

  inherited;
end;

procedure TFileScriptPreProcessor.Execute(const Language: String;
  const Code: TStrings);
var
  Index   : Integer;
  Index2  : Integer;
  Line    : string;

  procedure UpdateLine;
  var
    Lines   : TStrings;
    Prefix  : string;
    Index2  : Integer;
  begin
    if Line = '' then
    begin
      Inc(Index);
      Exit;
    end;

    Lines := TStringList.Create;
    try
      Lines.Text := Line;
      if Lines.Count = 0 then
        Code.Delete(Index)
      else begin
        Code[Index] := Lines[0];
        if not RegExpScanf('^(\s+)', Lines[0], ['STRING', @Prefix]) then
          Prefix := '';
        Inc(Index);

        for Index2 := 1 to Lines.Count-1 do
          Code.Insert(Index-1+Index2, Prefix + Lines[Index2]);
      end;
    finally
      Lines.Free;
    end;
  end;

begin
  Index := 0;
  while Index < Code.Count do
  begin
    Line := Code[Index];
    if RegExpMatch('^\s*#', Line) then
    begin
      Line := PreProcessFileLine(Language, Line);
      UpdateLine;
    end else if RegExpMatch('@[a-zA-Z_0-9]+', Line) then
    begin
      Line := ChangeLineVariable(Language, Line);
      UpdateLine;
    end else
      Inc(Index);
  end;

  if RegExpMatch('^J(ava)?Script$', Language) then
    for Index := 0 to FVariables.Count-1 do
      Code.Insert(0, 'var ' + FVariables[Index] + ';')
  else begin
    for Index := 0 to Code.Count-1 do
      if RegExpMatch('Option\s+Explicit', Code[Index]) then
      begin
        for Index2 := 0 to FVariables.Count-1 do
          Code.Insert(Index+1, 'Dim ' + FVariables[Index2]);
        Exit;
      end;

    for Index2 := 0 to FVariables.Count-1 do
      Code.Insert(0, 'Dim ' + FVariables[Index2]);
  end;
end;

class function TFileScriptPreProcessor.HandlesLanguage(
  const Language: String): Boolean;
begin
  Result := RegExpMatch('^(VBSCRIPT|J(AVA)?SCRIPT)$', Language);
end;

function TFileScriptPreProcessor.PreProcessFileLine(const Language,
  Line: string): string;
var
  Prefix    : string;
  Statement : string;
  Index     : Integer;
begin
  if not RegExpScanf('^(\s*)#\s*(.*)$', Line, ['STRING', @Prefix, 'STRING', @Statement]) then
    Exit;

  Statement := Trim(Statement);
  for Index := Low(FileFunctions) to High(FileFunctions) do
  begin
    Result := FileFunctions[Index](Self, Language, Statement);
    if Result <> '' then
    begin
      Result := Prefix + Result;
      Break;
    end;
  end;

  if Result = '' then
    Result := Line; // this will trigger a script code error
end;

initialization
  RegisterPreProcessorHandler(TFileScriptPreProcessor);
end.

