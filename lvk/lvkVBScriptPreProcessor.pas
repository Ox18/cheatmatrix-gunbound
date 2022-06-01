{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the basic VBScript preprocessor class.
}
unit lvkVBScriptPreProcessor;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkVBScriptPreProcessor.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkActiveScriptPreProcessor;

type
  { Description:
      This script preprocessor handles VBScript only, and only does one thing
      at the moment, handle a special case of Dim statements.

      In VBScript, you cannot declare a variable and give it a value at the
      same time, you got to split it into two lines, like this:

        Dim i
        i = 0

      With this preprocessor, you can combine this into one line:

        Dim i = 0

      It correctly handles forms like this:

        Dim i = 0, j = 1, x, y, z = 0

      This is actually the same as this:

        Dim i, j, x, y, z
        i = 0
        j = 1
        z = 0

      There is one exception though, objects. If you try to use this:

        Dim db = FunctionThatReturnsObject

      Then it will fail. The reason is that the code will look like this:

        Dim db
        db = FunctionThatReturnsObject

      but it should have looked like this:

        Dim db
        <B>Set</b> db = FunctionThatReturnsObject

      The preprocessor will detect calls to CreateObject, GetObject,
      WScript.CreateObject and WScript.GetObject, and add the Set command
      to make them work, but all other forms of object-producing function-calls
      will have to be split as normal.
  }
  TVBScriptPreProcessor = class(TScriptPreProcessorHandler)
  public
    procedure Execute(const Language: String; const Code: TStrings);
      override;
    class function HandlesLanguage(const Language: String): Boolean; override;
  end;

implementation

uses
  lvkRegExp;

{ TVBScriptPreProcessor }

procedure FixupVariables(var Line: string);
var
  Prefix  : string;
  List    : string;
  Index   : Integer;
  Vars    : TStrings;
  Quote   : Char;
  Start   : Integer;
  Level   : Integer;

  Dim     : string;
  Assign  : string;
  Name    : string;
  Value   : string;

  procedure AddVar(s: string);
  begin
    s := Trim(s);
    if s <> '' then
      Vars.Add(s);
  end;

  function CreatesAnObject(const Value: string): Boolean;
  const
    Patterns  : array[1..5] of string = (
      '^CreateObject\(',
      '^GetObject\(',
      '^WScript\.CreateObject\(',
      '^WScript\.GetObject\(',
      '^WScript\.CreateCollection\('
    );
  var
    Index : Integer;
  begin
    Result := False;
    for Index := Low(Patterns) to High(Patterns) do
      if RegExpMatch(Patterns[Index], Value) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if not RegExpScanf('^(\s*Dim\s+)(.*)\s*$', Line, ['STRING', @Prefix, 'STRING', @List]) then
    Exit;

  Index := 1;
  Level := 0;
  Vars := TStringList.Create;
  try
    Quote := #0;
    Start := 0;
    while Index <= Length(List) do
    begin
      case List[Index] of
        '"', '''':
          begin
            if Quote = #0 then
              Quote := List[Index]
            else if List[Index] = Quote then
              Quote := #0;
          end;

        ',':
          if (Start > 0) and (Quote = #0) and (Level = 0) then
          begin
            AddVar(Copy(List, Start, Index-Start));
            Start := 0;
          end;

        '(':
          if Quote = #0 then
            Inc(Level);

        ')':
          if Quote = #0 then
            Dec(Level);
      else
        if Start = 0 then
          Start := Index;
      end;

      Inc(Index);
    end;

    if Start > 0 then
      AddVar(Copy(List, Start, Index-Start));

    Dim := '';
    Assign := '';

    for Index := 0 to Vars.Count-1 do
    begin
      if RegExpScanf('^([a-zA-Z_0-9]+)\s*=\s*(.*)\s*$', Vars[Index],
        ['STRING', @Name, 'STRING', @Value]) then
      begin
        if Dim <> '' then
          Dim := Dim + ', ';
        Dim := Dim + Name;

        if Assign <> '' then
          Assign := Assign + ': ';

        if CreatesAnObject(Value) then
          Assign := Assign + 'Set ';
        Assign := Assign + Name + ' = ' + Value;
      end else begin
        if Dim <> '' then
          Dim := Dim + ', ';
        Dim := Dim + Vars[Index];
      end;
    end;

    Line := Prefix + Dim;
    if Assign <> '' then
      Line := Line + ': ' + Assign;
  finally
    Vars.Free;
  end;
end;

procedure TVBScriptPreProcessor.Execute(const Language: String;
  const Code: TStrings);
var
  Index : Integer;
  Line  : string;
begin
  for Index := 0 to Code.Count-1 do
  begin
    Line := Code[Index];

    if RegExpMatch('^\s*Dim\s+', Line) then
      FixupVariables(Line);

    Code[Index] := Line;
  end;
end;

class function TVBScriptPreProcessor.HandlesLanguage(
  const Language: String): Boolean;
begin
  Result := RegExpMatch('^(VBSCRIPT)$', Language);
end;

initialization
  RegisterPreProcessorHandler(TVBScriptPreProcessor);
end.
