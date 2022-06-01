unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MD5Unit, CMIni, XTEA, langcontrol, StdCtrls;

type
  TForm8 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Button4: TButton;
    Button5: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;
  CMLang: TCMLanguage;

implementation

{$R *.dfm}

Function isInt(valor: AnsiChar): boolean;
const ints = '0123456789';
begin
  if( pos(valor+'', ints) > 0 ) then
    result := true else result := false;
end;

Function SafeStrToInt(valor: AnsiString): integer;
var i: integer;  s: AnsiString;
begin
    s := '';
    result := 0;
    for i := 1 to length(valor) do
    begin
        if isInt(valor[i]) then
           s := s + valor[i];
    end;
    if s <> '' then
       result := StrToInt(s);
end;

function HexToInt(s: AnsiString): Longword;
var
  b: Byte;
  c: AnsiChar;
begin

//{$I VMProtectBegin.inc}

  Result := 0;
  s := UpperCase(s);
  for b := 1 to Length(s) do
  begin
    Result := Result * 16;
    c := s[b];
    case c of
      '0'..'9': Inc(Result, Ord(c) - Ord('0'));
      'A'..'F': Inc(Result, Ord(c) - Ord('A') + 10);
      else
        raise EConvertError.Create('No Hex-Number');
    end;
  end;
//{$I VMProtectEnd.inc}
end;

function FixString(valor: AnsiString):AnsiString;
var i: integer;
begin
  result := '';
  for i := 1 to length(valor) do
  begin
      if(ord(valor[i]) > 20) then
         result := result + valor[i]
      else
         result := result + '#'+inttohex(ord(valor[i]),2);
  end;
end;

procedure TForm8.Button1Click(Sender: TObject);
var i,j: integer;
    s: AnsiString;
    t: AnsiString;
    flag: boolean;
    InAspas: Boolean;
    index: integer;
    starting: boolean;
    hex: AnsiString;
begin
   memo2.Clear;
   t := '';
   InAspas := false;
   s := Memo1.Text;
   i := 1;
   starting := true;
   while i <= length(s) do
   begin

      //Verifica aspas
      if s[i] = '''' then
      begin
         if InAspas then
         begin
             if (s[i+1]) = '''' then
             begin
                t := t + '''';
                inc(i, 2);
                continue;
             end else
                InAspas := (not InAspas);
         end else
            InAspas := (not InAspas);
         inc(i);
         continue;
      end;

      if InAspas then
      begin
         if(ord(s[i]) >= $20) then
            t := t + s[i];
         inc(i);
         continue;
      end;

      if not InAspas then
      begin
          if (s[i] = '/') and (s[i+1] = '/') then
          begin
              while (i < length(s)) and (s[i] <> #13) do
                 inc(i);
              inc(i); //#10
              continue;
          end;

          if (s[i] = '#') then
          begin
             j := i+1;
             hex := '';
             while (j < length(s)) and (isInt(s[j])) do
             begin
                hex := hex+s[j];
                inc(j);
             end;
             t := t + AnsiChar(HexToInt(hex));
             inc(i, j-i);
             continue; 
          end;

          if (s[i] = ';') and (trim(t) <> '') and (index <> 0) then
          begin
              starting := true;
              CMLang.Item[index] := t;
              Memo2.Lines.Add('['+formatfloat('0000',index)+'] := "'+FixString(t)+'"');
              t := '';
          end;
      end;

      if starting then
      begin
        if s[i] <> ':' then
           t := t + s[i]
        else
        begin
           starting := false;
           index := SafeStrToInt(t);
           t := '';
           inc(i);
           continue;
        end;
      end;
         
      inc(i);
   end;
   if edit1.Text = '' then
      if SaveDialog1.Execute then
         edit1.Text := SaveDialog1.FileName;
   CMLang.SaveToFile(Edit1.Text);
end;

procedure TForm8.Button2Click(Sender: TObject);
begin
edit1.Clear;
end;

procedure TForm8.Button3Click(Sender: TObject);
var i: integer; a: AnsiString;
 b: string;
begin
if (Edit1.Text = '') or (not FileExists(Edit1.Text)) then
  if OpenDialog1.Execute then Edit1.Text := OpenDialog1.FileName;

  if FileExists(Edit1.Text) then
  begin
      CMLang.LoadFromFile(Edit1.Text);
      for i := 0 to CMLang.count-1 do
      begin
          a := IntToStr(CMLang.Items[i].Index);
          b := CMLang.Items[i].Valor;
          Memo2.Lines.Add('['+a+'] := "'+b+'"');
      end;
  end;
end;

procedure TForm8.Button4Click(Sender: TObject);
begin
   Memo2.Clear;
end;

procedure TForm8.Button5Click(Sender: TObject);
begin
memo1.Clear;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  CMLang := TCMLanguage.create;
  Edit1.Text := ExtractFilePath(ParamStr(0))+'lang.cmx';
  
end;

end.
