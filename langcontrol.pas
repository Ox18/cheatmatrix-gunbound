unit langcontrol;

interface

uses windows, sysutils, xtea, functions, classes;

type
PLangItem = ^TLangItem;
TLangItem = record
   Index: integer;
   Valor: AnsiString;
end;

TCMLanguage = class
  constructor create(arquivo: AnsiString = '');
  private
     Lista: TList;
     farq: AnsiString;
     Function GetValue(index: integer): AnsiString;
     Procedure SetValue(index: integer; valor: AnsiString);
     Function GetItem(index: integer): TLangItem;
     Procedure SetItem(index: integer; valor: TLangItem);
  public
     property Item[index: integer]: AnsiString read GetValue write SetValue;
     property Items[index: integer]: TLangItem read GetItem write SetItem;
     Function LoadFromFile(arquivo: AnsiString): boolean;
     Procedure SaveToFile(arquivo: AnsiString);
     procedure delete(index: integer);
     Function Load: boolean;
     Function count: integer;
     procedure Save;
     procedure Clear;
  end;

  var
    key: string = '';
implementation


{ TCMLanguage }

Function SafeStrToInt(valor: AnsiString): integer;
begin
    valor := trim(valor);

    result := 0;
    try
       result := StrToInt(valor);
    except
       on e:exception do
       begin
          result := 0;
          exit;
       end;
    end;
end;



{ TCMLanguage }

procedure TCMLanguage.Clear;
begin
   Lista.Clear;
end;

function TCMLanguage.count: integer;
begin
   result := Lista.Count;
end;

constructor TCMLanguage.create(arquivo: AnsiString = '');
begin
    Key := PAnsiChar('0w'+AnsiChar(83)+AnsiChar(32)+'mXX27fcn'+AnsiChar(16)+'2Df');
    farq := arquivo;
    Lista := TList.Create;
end;

procedure TCMLanguage.delete(index: integer);
var i: integer;
begin
   for i := 0 to Lista.Count-1 do
       if (PLangItem(Lista.Items[i])^.Index = index) then
       begin
           Lista.Delete(i);
           exit;
       end;
end;

function TCMLanguage.GetItem(index: integer): TLangItem;
begin
   result := PLangItem(Lista.Items[index])^;
end;

function TCMLanguage.GetValue(index: integer): AnsiString;
var i: integer;
begin
   for i := 0 to Lista.Count-1 do
       if (PLangItem(Lista.Items[i])^.Index = index) then
       begin
           result := PLangItem(Lista.Items[i])^.Valor;
           exit;
       end;
end;

Function TCMLanguage.Load: boolean;
begin
   result := LoadFromFile(farq);
end;

Function TCMLanguage.LoadFromFile(arquivo: AnsiString): boolean;
var
  i,j: integer;
  count: integer;
  arq: TFileStream;
  windex: integer;
  len: integer;
  s: AnsiString;
  e: AnsiString;
  p: PLangItem;
begin
  if not FileExists(arquivo) then
  begin
     result := false;
     exit;
  end;
  result := true;
  Lista.Clear;
  arq := TFileStream.Create(arquivo, fmOpenRead);
  arq.Seek(0, soFromBeginning);
  arq.ReadBuffer(count, 4);

  i := 0;
  while (i < 2) or (arq.Position < arq.Size) do
  begin
      p := new(PLangItem);
      arq.Read(windex, 4);
      arq.Read(len, 4);
      dec(windex, 94267);
      dec(len, 59983);
      SetLength(s, len);
      arq.Read(s[1], len);
      p^.Index := windex;
      e := trim(DecriptXTea(s, key));
      p^.Valor := e;
      Lista.Add(p);
      inc(i);
  end;
  arq.Free;
end;

procedure TCMLanguage.Save;
begin
   SaveToFile(farq);
end;

procedure TCMLanguage.SaveToFile(arquivo: AnsiString);
var
  i: integer;
  count: integer;
  arq: TFileStream;
  windex: integer;
  len: integer;
  s: AnsiString;
  n: TStrings;
begin
  if not FileExists(arquivo) then
  begin
      n := TStringList.Create;
      n.Text := ' ';
      n.SaveToFile(arquivo);
      n.Free;
  end;

  arq := TFileStream.Create(arquivo, fmCreate);
  count := Lista.Count;
  arq.WriteBuffer(count, 4);
  for i := 0 to count-1 do
  begin
     windex := PLangItem(Lista.Items[i])^.Index+94267;
     s := EncriptXTea(PLangItem(Lista.Items[i])^.Valor, Key);
     arq.WriteBuffer( windex, 4);
     len := length(s)+59983;
     arq.WriteBuffer( len, 4);
     len := len-59983;
     arq.WriteBuffer( s[1], len);
  end;
  arq.Free;
end;

procedure TCMLanguage.SetItem(index: integer; valor: TLangItem);
var p: PLangItem;
begin
  p := new(PLangItem);
  p^ := valor;
  Lista.Items[index] := p;
end;

procedure TCMLanguage.SetValue(index: integer; valor: AnsiString);
var i: integer;
    p: PLangItem;
begin
   for i := 0 to Lista.Count-1 do
       if (PLangItem(Lista.Items[i])^.Index = index) then
       begin
           PLangItem(Lista.Items[i])^.Valor := valor;
           exit;
       end;
   p := new(PLangItem);
   p^.Index := index;
   p^.Valor := valor;
   Lista.Add(p);
end;

end.

