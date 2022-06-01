{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvkSettings;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSettings.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  TypInfo, Classes, IniFiles;

type
  TlvkSettings = class(TPersistent)
  private
    FSectionName  : string;
    FEncoded      : TStringList;

  protected
    function IsEncoded(const Prop: PPropInfo): Boolean;
    function EncodeString(const s: string): string;
    function DecodeString(const s: string): string;
    procedure Encode(const PropertyName: string);
    
    function OpenIni: TIniFile;
    property SectionName: string read FSectionName;

    procedure LoadInteger(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadChar(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadEnumeration(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadFloat(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadString(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadSet(const Ini: TIniFile; const Prop: PPropInfo);
    procedure LoadInt64(const Ini: TIniFile; const Prop: PPropInfo);

    procedure SaveInteger(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveChar(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveEnumeration(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveFloat(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveString(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveSet(const Ini: TIniFile; const Prop: PPropInfo);
    procedure SaveInt64(const Ini: TIniFile; const Prop: PPropInfo);

  public
    constructor Create(const SectionName: string);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Load;
    procedure Save;
    procedure Init; virtual;
  end;

implementation

uses
  SysUtils;

const
  TypeKinds = [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkWChar, tkLString, tkWString, tkInt64];

{ TlvkSettings }

procedure TlvkSettings.Assign(Source: TPersistent);
var
  SelfProps   : PPropList;
  OtherProps  : PPropList;
  SelfProp    : PPropInfo;
  OtherProp   : PPropInfo;
  SelfCount   : Integer;
  OtherCount  : Integer;
  Index       : Integer;
begin
  if Source.ClassInfo = ClassInfo then
  begin
    New(SelfProps);
    try
      New(OtherProps);
      try
        SelfCount := GetPropList(Self.ClassInfo, TypeKinds, SelfProps);
        OtherCount := GetPropList(Source.ClassInfo, TypeKinds, OtherProps);
        Assert(SelfCount = OtherCount);

        for Index := 0 to SelfCount-1 do
        begin
          SelfProp := SelfProps^[Index];
          OtherProp := OtherProps^[Index];

          Assert(SelfProp.Name = OtherProp.Name);
          Assert(SelfProp.PropType^.Kind = OtherProp.PropType^.Kind);

          case SelfProp^.PropType^.Kind of
            tkInteger, tkChar, tkWChar:
              SetOrdProp(Self, SelfProp, GetOrdProp(Source, OtherProp));

            tkEnumeration:
              SetEnumProp(Self, SelfProp, GetEnumProp(Source, OtherProp));

            tkFloat:
              SetFloatProp(Self, SelfProp, GetFloatProp(Source, OtherProp));

            tkString, tkLString, tkWString:
              SetStrProp(Self, SelfProp, GetStrProp(Source, OtherProp));

            tkSet:
              SetSetProp(Self, SelfProp, GetSetProp(Source, OtherProp));

            tkInt64:
              SetInt64Prop(Self, SelfProp, GetInt64Prop(Source, OtherProp));
          end;
        end;
      finally
        Dispose(OtherProps);
      end;
    finally
      Dispose(SelfProps);
    end;

  end else
    inherited Assign(Source);
end;

constructor TlvkSettings.Create(const SectionName: string);
begin
  inherited Create;

  FSectionName := SectionName;
  FEncoded := TStringList.Create;
  FEncoded.Sorted := True;
  Init;
  if FileExists(ChangeFileExt(ParamStr(0), '.Ini')) then
    Load;
end;

function TlvkSettings.DecodeString(const s: string): string;
var
  Index : Integer;
begin
  Result := '';
  Index := 1;
  while Index < Length(s) do
  begin
    Result := Result + Chr(StrToInt('$' + Copy(s, Index, 2)) xor $77 xor ((Index-1) div 2 + 1));
    Inc(Index, 2);
  end;
end;

destructor TlvkSettings.Destroy;
begin
  FEncoded.Free;

  inherited;
end;

procedure TlvkSettings.Encode(const PropertyName: string);
begin
  if FEncoded.IndexOf(PropertyName) < 0 then
    FEncoded.Add(PropertyName);
end;

function TlvkSettings.EncodeString(const s: string): string;
var
  Index : Integer;
begin
  Result := '';
  for Index := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[Index]) xor $77 xor Index, 2);
end;

procedure TlvkSettings.Init;
begin
  // Do nothing here
end;

function TlvkSettings.IsEncoded(const Prop: PPropInfo): Boolean;
begin
  Result := FEncoded.IndexOf(Prop^.Name) >= 0;
end;

procedure TlvkSettings.Load;
var
  Props : PPropList;
  Prop  : PPropInfo;
  Count : Integer;
  Index : Integer;
  Ini   : TIniFile;
begin
  Ini := OpenIni;
  try
    New(Props);
    try
      Count := GetPropList(Self.ClassInfo, TypeKinds, Props);
      for Index := 0 to Count-1 do
      begin
        Prop := Props^[Index];
        if Ini.ValueExists(SectionName, Prop^.Name) then
          case Prop^.PropType^.Kind of
            tkInteger     : LoadInteger(Ini, Prop);
            tkChar        : LoadChar(Ini, Prop);
            tkWChar       : LoadChar(Ini, Prop);
            tkEnumeration : LoadEnumeration(Ini, Prop);
            tkFloat       : LoadFloat(Ini, Prop);
            tkString      : LoadString(Ini, Prop);
            tkLString     : LoadString(Ini, Prop);
            tkWString     : LoadString(Ini, Prop);
            tkSet         : LoadSet(Ini, Prop);
            tkInt64       : LoadInt64(Ini, Prop);
          end;
      end;
    finally
      Dispose(Props);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TlvkSettings.LoadChar(const Ini: TIniFile; const Prop: PPropInfo);
begin
  SetOrdProp(Self, Prop, StrToInt(Copy(Ini.ReadString(SectionName, Prop^.Name, '#0'), 2, 255)));
end;

procedure TlvkSettings.LoadEnumeration(const Ini: TIniFile;
  const Prop: PPropInfo);
begin
  SetEnumProp(Self, Prop, Ini.ReadString(SectionName, Prop^.Name, ''));
end;

procedure TlvkSettings.LoadFloat(const Ini: TIniFile; const Prop: PPropInfo);
begin
  SetFloatProp(Self, Prop, Ini.ReadFloat(SectionName, Prop^.Name, 0.0));
end;

procedure TlvkSettings.LoadInt64(const Ini: TIniFile; const Prop: PPropInfo);
begin
  SetInt64Prop(Self, Prop, StrToInt(Ini.ReadString(SectionName, Prop^.Name, '0')));
end;

procedure TlvkSettings.LoadInteger(const Ini: TIniFile;
  const Prop: PPropInfo);
begin
  SetOrdProp(Self, Prop, Ini.ReadInteger(SectionName, Prop^.Name, 0));
end;

procedure TlvkSettings.LoadSet(const Ini: TIniFile; const Prop: PPropInfo);
begin
  SetSetProp(Self, Prop, Ini.ReadString(SectionName, Prop^.Name, ''));
end;

procedure TlvkSettings.LoadString(const Ini: TIniFile; const Prop: PPropInfo);
begin
  if IsEncoded(Prop) then
    SetStrProp(Self, Prop, DecodeString(Ini.ReadString(SectionName, Prop^.Name, '')))
  else
    SetStrProp(Self, Prop, Ini.ReadString(SectionName, Prop^.Name, ''));
end;

function TlvkSettings.OpenIni: TIniFile;
begin
  Result := TIniFile.Create(ChangeFileExt(ParamStr(0), '.Ini'));
end;

procedure TlvkSettings.Save;
var
  Props : PPropList;
  Prop  : PPropInfo;
  Count : Integer;
  Index : Integer;
  Ini   : TIniFile;
begin
  Ini := OpenIni;
  try
    New(Props);
    try
      Count := GetPropList(Self.ClassInfo, TypeKinds, Props);
      for Index := 0 to Count-1 do
      begin
        Prop := Props^[Index];
        case Prop^.PropType^.Kind of
          tkInteger     : SaveInteger(Ini, Prop);
          tkChar        : SaveChar(Ini, Prop);
          tkWChar       : SaveChar(Ini, Prop);
          tkEnumeration : SaveEnumeration(Ini, Prop);
          tkFloat       : SaveFloat(Ini, Prop);
          tkString      : SaveString(Ini, Prop);
          tkLString     : SaveString(Ini, Prop);
          tkWString     : SaveString(Ini, Prop);
          tkSet         : SaveSet(Ini, Prop);
          tkInt64       : SaveInt64(Ini, Prop);
        end;
      end;
    finally
      Dispose(Props);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TlvkSettings.SaveChar(const Ini: TIniFile; const Prop: PPropInfo);
begin
  Ini.WriteString(SectionName, Prop^.Name, '#' + IntToStr(GetOrdProp(Self, Prop)));
end;

procedure TlvkSettings.SaveEnumeration(const Ini: TIniFile;
  const Prop: PPropInfo);
begin
  Ini.WriteString(SectionName, Prop^.Name, GetEnumProp(Self, Prop));
end;

procedure TlvkSettings.SaveFloat(const Ini: TIniFile; const Prop: PPropInfo);
begin
  Ini.WriteFloat(SectionName, Prop^.Name, GetFloatProp(Self, Prop));
end;

procedure TlvkSettings.SaveInt64(const Ini: TIniFile; const Prop: PPropInfo);
begin
  Ini.WriteString(SectionName, Prop^.Name, IntToStr(GetInt64Prop(Self, Prop)));
end;

procedure TlvkSettings.SaveInteger(const Ini: TIniFile;
  const Prop: PPropInfo);
begin
  Ini.WriteInteger(SectionName, Prop^.Name, GetOrdProp(Self, Prop));
end;

procedure TlvkSettings.SaveSet(const Ini: TIniFile; const Prop: PPropInfo);
begin
  Ini.WriteString(SectionName, Prop^.Name, GetSetProp(Self, Prop, False));
end;

procedure TlvkSettings.SaveString(const Ini: TIniFile; const Prop: PPropInfo);
begin
  if IsEncoded(Prop) then
    Ini.WriteString(SectionName, Prop^.Name, EncodeString(GetStrProp(Self, Prop)))
  else
    Ini.WriteString(SectionName, Prop^.Name, GetStrProp(Self, Prop));
end;

end.
