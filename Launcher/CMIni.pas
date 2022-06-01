unit CMIni;

interface

uses windows, sysutils, classes, encription;

//{$DEFINE INIDEB}



type
  TCMIniResult = class
     constructor create(valor: AnsiString);
     private
        fvalue: AnsiString;
        Function SafeStrToInt(valor: AnsiString): integer;
        Function SafeStrToBool(valor: AnsiString): Boolean;
        Function SafeStrToFloat(valor: AnsiString): Double;
        Function SafeStrToInt64(valor: AnsiString): Int64;
        Function SafeStrToLong(valor: AnsiString): Dword;
     public
        Function AsString: AnsiString;
        Function AsInteger: integer;
        Function AsLong: dword;
        Function AsInt64: int64;
        Function AsBoolean: boolean;
        Function AsFloat: double;
     end;

  PCMIni = ^TCMIni;
  TCMIni = class
    constructor create(hd: AnsiString; arquivo: AnsiString);
    private
      fhd: AnsiString;
      farq: AnsiString;
      fcont: tstrings;
    public
      Procedure LoadFromText(valor: AnsiString);
      property filename: AnsiString read farq write farq;
      function assign(arquivo: AnsiString): boolean;
      function getvalue(chave: AnsiString; valor: AnsiString; default: AnsiString = ''): TCMIniResult; overload;
      function getvalue(chave: AnsiString; valor: AnsiString; default: integer): TCMIniResult; overload;
      Procedure setvalue(chave: AnsiString; valor: AnsiString; newvalue: AnsiString); overload;
      Procedure setvalue(chave: AnsiString; valor: AnsiString; newvalue: integer); overload;
      Procedure delete(chave: AnsiString); overload;
      Procedure delete(chave: AnsiString; valor: AnsiString); overload;
      procedure ListValues(chave: AnsiString; outPut: tstrings);
      procedure ListKeys(outPut: tstrings);
      function exists(chave: AnsiString): boolean; overload;
      function exists(chave: AnsiString; valor: AnsiString): boolean; overload;
      function ValueExists(valor: AnsiString): boolean; overload;
      function Show: AnsiString;
      function save: boolean;
      function close: boolean;
      function empty: boolean;
      function Text: AnsiString;
      function getHD: AnsiString;
      procedure RenameKey(Section: AnsiString; NewSection: AnsiString);
      procedure RenameValue(Section: AnsiString; Key: AnsiString; NewKey: AnsiString);
    end;

implementation

uses StrUtils;

{function formatChar(valor: char):string;
const nh = 'ghijklmnopqrsutvwxyz';
begin
   randomize;
   result := LowerCase(IntToHex(dword(valor),3));
   if(result[1] = '0') then result[1] := nh[random(19)+1];
   if(result[2] = '0') then result[2] := nh[random(19)+1];
   if(result[3] = '0') then result[3] := nh[random(19)+1]; 
end;

Function pack(valor: string):string;
var i: integer;
begin
   result := '';
   for i:=1 to length(valor) do
   begin
      result := result + formatChar(valor[i]);
   end;
end;

function HexToInt(s: string): Longword;
var
  b: Byte;
  c: Char;
begin
  Result := 0;
  s := trim(s);
  s := UpperCase(s);
  for b := 1 to Length(s) do
  begin
    Result := Result * 16;
    c := s[b];
    case c of
      '0'..'9': Inc(Result, Ord(c) - Ord('0'));
      'A'..'F': Inc(Result, Ord(c) - Ord('A') + 10);
      //else
       // raise EConvertError.Create('No Hex-Number');
    end;
  end;
end;

Function unpack(valor: string):string;
var i: integer; s:string;
const h = '0123456789abcdef';
begin

   valor := lowercase(valor);
   result := '';
   for i:=1 to length(valor) do
   begin
      if pos(valor[i], h) = 0 then
         s := s + '0'
      else
         s := s + valor[i];
   end;
   i := 1;
   result := '';
   while i < length(s) do
   begin
       result := result + char( HexToInt(s[i]+s[i+1]+s[i+2]) );
       i := i + 3;
   end;
   
end;}

{ TCMIni }

function TCMIni.assign(arquivo: AnsiString): boolean;
var t: textfile;
begin
   farq := arquivo;
   if trim(arquivo) = '' then
      exit;

   if not FileExists(arquivo) then
   begin
      AssignFile(t, farq);
      try
       Rewrite(t);
      finally
       CloseFile(t);
      end;
   end;

   fcont.LoadFromFile(farq);
   fcont.Text := {$IFNDEF INIDEB}MaxDecript{$ENDIF}(fcont.Text, fhd);
end;

function TCMIni.close: boolean;
var s: TStrings;
begin
  s := TStringList.Create;
  try
    try
     s.Text := {$IFNDEF INIDEB}MaxEncript{$ENDIF}(fcont.Text, fhd);
     s.SaveToFile(farq);
     result := true;
    except
      result := false;
    end;
  finally
    s.Free;
  end;
end;

constructor TCMIni.create(hd: AnsiString; arquivo: AnsiString);
var t: textfile;
begin
   fcont := TStringList.Create;
   farq := arquivo;

   fhd := hd;

   if trim(arquivo) = '' then
      exit;

   if not FileExists(arquivo) then
   begin
      AssignFile(t, farq);
      try
       Rewrite(t);
      finally
       CloseFile(t);
      end;
   end;

   fcont.LoadFromFile(farq);
   fcont.Text := {$IFNDEF INIDEB}MaxDecript{$ENDIF}(fcont.Text, fhd);
end;

procedure TCMIni.delete(chave, valor: AnsiString);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(key) = LowerCase(trim(chave)) ) and ( LowerCase(h) = LowerCase(trim(valor)) ) then
      begin
         fcont.Delete(i);
         break;
      end;
   end;
end;

function TCMIni.exists(chave: AnsiString): boolean;
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         if(LowerCase(key) = trim(lowercase(chave))) then
         begin
             result := true;
             exit;
         end;
      end;

   end;
end;

function TCMIni.empty: boolean;
begin
   result := (length(trim(fcont.Text)) = 0);
end;

function TCMIni.exists(chave, valor: AnsiString): boolean;
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
   key := '';

   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if (( LowerCase(key) = LowerCase(trim(chave)) ) or (LowerCase(trim(chave)) = '')) and ( LowerCase(h) = LowerCase(trim(valor)) ) then
      begin
          result := true;
          exit;
      end;
   end;
end;

function TCMIni.getvalue(chave, valor: AnsiString; default: integer): TCMIniResult;
begin
   result := getvalue(chave, valor, IntToStr(default));
end;

procedure TCMIni.delete(chave: AnsiString);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
res: TStrings;
flag: boolean;
flagIndex: integer;
q: integer;
begin
   flag := false;
   key := '';
   
   i := 0;
   q:= fcont.Count-1;
   while i < fcont.Count do
   begin
      s := trim(fcont[i]);

      if s = '' then
      begin
         inc(i);
         continue;
      end;

      if  (s[1] = '<') and (pos('>', s) > 0)  then
      begin
         key := trim( copy(s,2, length(s)-2) );
         if  LowerCase(key) = LowerCase(trim(chave))  then
         begin
            flag := true;
            flagIndex := i;
         end;
         inc(i);
         continue;
      end;

      if s[1] = '$' then
      begin
         inc(i);
         continue;
      end;

      if  LowerCase(key) = LowerCase(trim(chave))  then
      begin
         fcont.Delete(i);
         continue;
      end else
      if flag then
         break;

      inc(i);
   end;
   if flag then
      fcont.Delete(flagIndex);
end;

function TCMIni.getvalue(chave, valor: AnsiString; default: AnsiString = ''): TCMIniResult;
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
////{$I VMProtectBegin.inc} 
   key := '';
   result := TCMIniResult.create(default);
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(key) = LowerCase(trim(chave)) ) and ( LowerCase(h) = LowerCase(trim(valor)) ) then
      begin
          result := TCMIniResult.create(v);
          exit;
      end;
   end;
////{$I VMProtectEnd.inc}
end;

procedure TCMIni.ListKeys(outPut: tstrings);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
////{$I VMProtectBegin.inc}
   if not assigned(outPut) then
      exit;

   outPut.Clear;
    
   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         outPut.Add(key);
      end;

      continue;
   end;
////{$I VMProtectEnd.inc}
end;

procedure TCMIni.ListValues(chave: AnsiString; outPut: tstrings);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
////{$I VMProtectBegin.inc}
   if not assigned(outPut) then
      exit;

   outPut.Clear;

   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(key) = LowerCase(trim(chave)) ) then
      begin
          outPut.Add(h);
      end;
   end;
////{$I VMProtectEnd.inc}
end;

procedure TCMIni.LoadFromText(valor: AnsiString);
begin
   fcont.Text := {$IFNDEF INIDEB}MaxDecript{$ENDIF}(valor, fhd);
end;

procedure TCMIni.RenameKey(Section, NewSection: AnsiString);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
  flag: boolean;
begin
   flag := false;
   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         if(LowerCase(key) = trim(LowerCase(Section)))then
           fcont.Strings[i] := '<'+NewSection+'>';
         continue;
      end;
   end;
end;

procedure TCMIni.RenameValue(Section, Key, NewKey: AnsiString);
var i,j: integer; s:AnsiString; tmpkey: AnsiString; h,v: AnsiString; t: AnsiString;
  flag: boolean;
begin
   flag := false;
   tmpkey := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         tmpkey := trim( copy(s,2, length(s)-2) );
         if(LowerCase(tmpkey) = trim(LowerCase(Section)))then
           flag := true;
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(tmpkey) = LowerCase(trim(Section)) ) then
      begin
         if  LowerCase(h) = LowerCase(trim(key)) then
         begin
             t := copy(s, pos('~', s), length(s));
             fcont.Strings[i] := NewKey + t;
         end;
      end;
   end;
end;

function TCMIni.save: boolean;
var s:tstrings;
begin
////{$I VMProtectBegin.inc}
  s := TStringList.Create;
  try
    try
       s.Text := {$IFNDEF INIDEB}MaxEncript{$ENDIF}(fcont.Text, fhd);
       s.SaveToFile(farq);
       result := true;
    except
      result := false;
    end;
  finally
    s.Free;
  end;
////{$I VMProtectEnd.inc}
end;

procedure TCMIni.setvalue(chave, valor: AnsiString; newvalue: integer);
begin
   setvalue(chave, valor, IntToStr(newvalue));
end;

procedure TCMIni.setvalue(chave, valor: AnsiString; newvalue: AnsiString);
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
  flag: boolean;
begin
////{$I VMProtectBegin.inc}
   flag := false;
   key := '';
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         if(LowerCase(key) = trim(LowerCase(chave)))then
           flag := true;
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(key) = LowerCase(trim(chave)) ) then
      begin
         if  LowerCase(h) = LowerCase(trim(valor)) then
         begin
            v := newvalue;            
            fcont.Strings[i] := h+'~'+v;
            exit;
         end;
      end else
      if flag then
      begin
          fcont.Insert(i-1, valor+'~'+newvalue);
          exit;
      end;
   end;

   if not flag then
   begin
       fcont.Add('<'+trim(LowerCase(chave))+'>');
       fcont.Add(valor+'~'+newvalue);
   end else
      fcont.Add(valor+'~'+newvalue);
////{$I VMProtectEnd.inc}
end;

function TCMIni.Show: AnsiString;
begin
////{$I VMProtectBegin.inc}
    result := {$IFNDEF INIDEB}MaxEncript{$ENDIF}(fcont.Text, fhd);
////{$I VMProtectEnd.inc}
end;

function TCMIni.Text: AnsiString;
begin
    result := fcont.Text;
end;

function TCMIni.getHD: AnsiString;
begin
    result := fHD;
end;

function TCMIni.ValueExists(valor: AnsiString): boolean;
var i,j: integer; s:AnsiString; key: AnsiString; h,v: AnsiString;
begin
////{$I VMProtectBegin.inc} 
   for i := 0 to fcont.Count-1 do
   begin
      s := fcont.Strings[i];

      if trim(s) = '' then
         continue;

      if  (trim(s)[1] = '<') and (pos('>', s) > 0)  then
      begin
         s := trim(s);
         key := trim( copy(s,2, length(s)-2) );
         continue;
      end;

      if trim(s)[1] = '$' then
         continue;

      j := pos('~', s);
      if j > 0 then
      begin
          h := trim(Copy(s, 1, j-1));
          v := trim(Copy(s, j+1, length(s)));
      end;

      if ( LowerCase(trim(v)) = LowerCase(trim(valor)) ) then
      begin
          result := true;
          exit;
      end;
   end;
////{$I VMProtectEnd.inc}
end;

{ TCMIniResult }

function TCMIniResult.SafeStrToBool(valor: AnsiString): Boolean;
begin
////{$I VMProtectBegin.inc} 
     valor := Trim(valor);

     result := false;
     if valor = '' then
        exit;

     if LowerCase(Trim(valor)) = 'false' then
        exit
     else
     if LowerCase(Trim(valor)) = 'true' then
        result := true
     else

     try
        result := StrToBool(valor);
     except
        on e:exception do
          result := False;
     end;
////{$I VMProtectEnd.inc}
end;

function TCMIniResult.SafeStrToFloat(valor: AnsiString): Double;
begin
     valor := Trim(valor);

     result := 0;
     if valor = '' then
        exit;

     try
        result := StrToFloat(valor);
     except
        on e:exception do
          result := 0;
     end;
end;

Function TCMIniResult.SafeStrToInt(valor: AnsiString): integer;
begin
     result := 0;
     if trim(valor) = '' then
        exit;

     try
        result := StrToInt(valor);
     except
        on e:exception do
          result := 0;
     end;
end;

function TCMIniResult.SafeStrToInt64(valor: AnsiString): Int64;
begin
     valor := Trim(valor);

     result := 0;
     if valor = '' then
        exit;

     try
        result := StrToInt64(valor);
     except
        on e:exception do
          result := 0;
     end;
end;

function TCMIniResult.SafeStrToLong(valor: AnsiString): Dword;
begin
   result := SafeStrToInt(valor);
end;

function TCMIniResult.AsBoolean: boolean;
begin
   result := SafeStrToBool(fvalue);
end;

function TCMIniResult.AsFloat: double;
begin
  result := SafeStrToFloat(fvalue);
end;

function TCMIniResult.AsInt64: int64;
begin
  result := SafeStrToInt64(fvalue);
end;

function TCMIniResult.AsInteger: integer;
begin
  result := SafeStrToInt(fvalue);
end;

function TCMIniResult.AsLong: dword;
begin
  result := SafeStrToLong(fvalue);
end;

function TCMIniResult.AsString: AnsiString;
begin
  result := fvalue;
end;

constructor TCMIniResult.create(valor: AnsiString);
begin
  fvalue := valor;
end;

end.
|