{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code to execute script-based html in much the same
    manner that IIS and ASP pages are executed.
}
unit lvkScriptBasedHTML;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/lvkScriptBasedHTML.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Variants, SysUtils, Classes,
  lvkActiveScript;

type
  IlvkScriptBasedHTML = interface
    ['{3C2CC750-35DC-4C17-8CDC-AEB280F74BD0}']

    procedure Load(const FileName: string); overload;
    procedure Load(const Stream: TStream); overload;

    function Evaluate: string;

    function GetScript: IlvkActiveScript;
    property Script: IlvkActiveScript read GetScript;
  end;

  TlvkScriptBasedHTML = class(TInterfacedObject, IlvkScriptBasedHTML)
  private
    FHandle : THandle;
    FScript : IlvkActiveScript;
    FCode   : OleVariant;

  protected
    // IlvkScriptBasedHTML interface
    procedure Load(const FileName: string); overload;
    procedure Load(const Stream: TStream); overload;
    function GetScript: IlvkActiveScript;
    function Evaluate: string;

  public
    constructor Create(const WindowHandle: THandle=0);

  end;

implementation

uses
  lvkRegExp, lvkRTTIDispatch;

type
  TCodeObject = class(TlvkRTTIBaseDispatch)
  private
    FCode : string;

  published
    function Clear(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function Write(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function Output(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
  end;

{ TlvkScriptBasedHTML }

constructor TlvkScriptBasedHTML.Create(const WindowHandle: THandle);
begin
  inherited Create;

  FHandle := WindowHandle;
  FCode := TCodeObject.Create as IDispatch;
end;

function TlvkScriptBasedHTML.Evaluate: string;
begin
  Assert(Assigned(FScript));

  FCode.Clear;

  FScript.Invalidate;
  FScript.Compile;

  Result := FCode.Output;
end;

function TlvkScriptBasedHTML.GetScript: IlvkActiveScript;
begin
  Result := FScript;
end;

procedure TlvkScriptBasedHTML.Load(const Stream: TStream);
var
  re          : IRegExp;
  LastPos     : Integer;
  ScriptCode  : string;
  Language    : string;
  Compiled    : string;

  function FixupString(const s: string): string;
  var
    Index : Integer;
  begin
    Result := '';
    Index := 1;
    while Index <= Length(s) do
    begin
      case s[Index] of
        #13:
          begin
            if (Index < Length(s)) and (s[Index+1] = #10) then
            begin
              Inc(Index);
              if CompareText(Language, 'VBSCRIPT') = 0 then
                Result := Result + '" + vbCRLF + "'
              else
                Result := Result + '\r';
            end else begin
              if CompareText(Language, 'VBSCRIPT') = 0 then
                Result := Result + '" + vbCR + "'
              else
                Result := Result + '\r';
            end;
          end;

        #10:
          begin
            if CompareText(Language, 'VBSCRIPT') = 0 then
              Result := Result + '" + vbLF + "'
            else
              Result := Result + '\r';
          end;

        '"':
          begin
            if CompareText(Language, 'VBSCRIPT') = 0 then
              Result := Result + '""'
            else
              Result := Result + '\"';
          end;
      else
        Result := Result + s[Index];
      end;
      Inc(Index);
    end;
    Result := '"' + Result + '"';
  end;

  function OutputExpression(const Expression: string): string;
  begin
    if CompareText(Language, 'VBSCRIPT') = 0 then
      Result := Format('Code.Write %s'#13#10, [Trim(Expression)])
    else
      Result := Format('Cdoe.Write(%s);'#13#10, [Trim(Expression)]);
  end;

  function OutputString(const s: string): string;
  begin
    Result := OutputExpression(FixupString(s));
  end;

  procedure ProcessDirectives(const Directives: string);
  var
    Value     : string;

    procedure HandleLanguageDirective(const NewLanguage: string);
    begin
      if RegExpMatch('^(j(ava)?script|vbscript)$', NewLanguage) then
        Language := NewLanguage
      else
        raise Exception.CreateFmt('Invalid language "%s"', [NewLanguage]);
    end;

  begin
    if RegExpScanf('\bLANGUAGE="((""|[^"])*)"', Directives, ['string',
      @Value]) then
    begin
      HandleLanguageDirective(Value);
    end;
  end;

  function FixupCode(const Code: string): string;
  var
    Lines : TStrings;
    Index : Integer;
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Code;
      for Index := 0 to Lines.Count-1 do
        if (Copy(TrimLeft(Lines[Index]), 1, 2) = '<%') and
          (Copy(TrimLeft(Lines[Index]), 1, 3) <> '<%=') and
          (Copy(Lines[Index], 1, 1) <> '<') then
        begin
          Lines[Index] := TrimLeft(Lines[Index]);
        end;

      Result := Lines.Text;
    finally
      Lines.Free;
    end;
  end;

  function LoadCode: string;
  begin
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(PChar(Result)^, Stream.Size);

    Result := FixupCode(Result);
  end;

begin
  Compiled := '';
  re := NewRegExp('<%([=@]??)([\001-\255]*)%>\015??\012??', [roMultiline,
    roBol..roEol]);
  LastPos := 0;
  ScriptCode := LoadCode;
  if re.MatchAgainst(ScriptCode) then
  begin
    repeat
      if re.Groups[0].StartPosition > LastPos then
        Compiled := Compiled + OutputString(Copy(ScriptCode, LastPos+1,
          re.Groups[0].StartPosition - LastPos));

      if re.Groups[1].Text = '@' then
        ProcessDirectives(re.Groups[2].Text)
      else if re.Groups[1].Text = '=' then
        Compiled := Compiled + OutputExpression(re.Groups[2].Text)
      else
        Compiled := Compiled + re.Groups[2].Text + #13#10;
      LastPos := re.Groups[0].EndPosition;
    until not re.NextMatch;

    if LastPos < Length(ScriptCode) then
      Compiled := Compiled + OutputString(Copy(ScriptCode, LastPos+1,
        Length(ScriptCode)));
  end else
    Compiled := ScriptCode;

  FScript := TlvkActiveScript.Create(FHandle);
  FScript.Language := Language;
  FScript.ScriptCode.Text := Compiled;
  FScript.AddObject('Code', FCode, False);
end;

procedure TlvkScriptBasedHTML.Load(const FileName: string);
var
  Stream  : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Load(Stream);
  finally
    Stream.Free;
  end;
end;

{ TCodeObject }

function TCodeObject.Clear(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  FCode := '';

  FunctionResult := Unassigned;
  Result := S_OK;
end;

function TCodeObject.Output(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  FunctionResult := FCode;
  Result := S_OK;
end;

function TCodeObject.Write(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Index : Integer;
begin
  for Index := Low(Parameters) to High(Parameters) do
    if not VarIsNull(Parameters[Index]) then
      FCode := FCode + string(Parameters[Index]);

  FunctionResult := Unassigned;
  Result := S_OK;
end;

end.

