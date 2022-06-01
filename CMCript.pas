unit CMCript;

interface

uses windows, cmSysUtils, Base64, functions;

Function IntToBin(n: integer): AnsiString;
function BinToInt(Value: AnsiString): Integer;
Function IsInt(s:AnsiString):boolean;

Function CM_Encrypt(valor: AnsiString): AnsiString;
Function CM_Decrypt(valor: AnsiString): AnsiString;

implementation

{uses strutils;  }

Function IsInt(s:AnsiString):boolean;
var i:integer;
begin
//{$I VMProtectBegin.inc}
   result:=true;
   for i:=1 to length(s) do
   begin
       if  (not (s[i] in ['-','0'..'9'])) or ((s[i] = '-') and (i <> 1)) then
       begin
           result:=false;
           exit;
       end;
   end;
//{$I VMProtectEnd.inc}
end;

Function IntToBin(n: integer): AnsiString;
{Converte um numero decimal em binário}
var
i,j: integer;
begin
//{$I VMProtectBegin.inc}
result:='';
i:=n;
  while true do
  begin
     j:=i mod 2;
     result:=CM_IntToStr(j)+result;
     i:=i div 2;
     if i=0 then
       break;
  end;
  while(length(result)<7) do
    result:='0'+result;
//{$I VMProtectEnd.inc}
end;

function BinToInt(Value: AnsiString): Integer;
var
  i, iValueSize: Integer;
begin
//{$I VMProtectBegin.inc}
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
    if Value[i] = '1' then Result := Result + (1 shl (iValueSize - i));
//{$I VMProtectEnd.inc}
end;


Function CharBin(valor: AnsiString):AnsiString;
var i,j:integer;
const
t : array [0..1] of AnsiChar = ('a','b');
begin
//{$I VMProtectBegin.inc}
   for i:=0 to 1 do begin
       for j:=1 to 6 do
       begin
           if CM_IntToStr(j)+t[i] = valor then
             result:=inttobin(i*6+j);
       end;
   end;
   result:=copy(result, length(result)-3, 4 );
//{$I VMProtectEnd.inc}
end;

//Transforma binário em 2 chars de encriptação - a ou b
Function BinChar(valor: AnsiChar):AnsiString;
var s:AnsiString;
v1,v2:AnsiString;
const
t : array [0..1] of AnsiChar = ('a','b');
begin
//{$I VMProtectBegin.inc}
   s:=inttobin(ord(valor));
   while length(s)<8 do
     s:='0'+s;
   v1:=copy(s,1,4);
   v2:=copy(s,5,4);
   if(bintoint(v1))<=6 then
      v1:=CM_IntToStr(bintoint(v1))+'a'
   else
      v1:=CM_IntToStr(bintoint(v1)-6)+'b';

   if(bintoint(v2))<=6 then
      v2:=CM_IntToStr(bintoint(v2))+'a'
   else
      v2:=CM_IntToStr(bintoint(v2)-6)+'b';

   result:=v1+v2;
//{$I VMProtectEnd.inc}
end;



//Comprime texto encryptado
Function CompressIt(valor: AnsiString):AnsiString;
var i:integer; s,s1,s2:AnsiString;
begin
//{$I VMProtectBegin.inc}
//o tanto de caracteres deve ser um numero par
if (length(valor) mod 2)>0 then
   valor:=copy(valor,1,length(valor)-1);
i:=1;
result:='';
//pega os caracteres de dois em dois
while i<=length(valor) do
begin
   s1:=''; s2:='';
   s:=copy(valor,i,2);
   i:=i+2;
   s1:=charbin(s);
   s:=copy(valor,i,2);
   i:=i+2;
   s2:= charbin(s);
   result:=result+AnsiChar(bintoint(s1+s2));
end;
//{$I VMProtectEnd.inc}
end;

//Decomprime texto comprimido
Function DecompressIt(valor: AnsiString):AnsiString;
var i:integer;
begin
//{$I VMProtectBegin.inc}
result:='';
for i:=1 to length(valor) do
   result:=result+binchar(valor[i]);
//{$I VMProtectEnd.inc}
end;

//Completa o numero com 10 digitos
Function FormatLen(valor:AnsiString):AnsiString;
var i,j:AnsiString;
begin
//{$I VMProtectBegin.inc}
   i:=CM_IntToStr(length(valor));
   j:=CM_IntToStr(length(i));
   while length(i)<9 do
     i:= i+CM_IntToStr(random(10));
   result:=j+i+valor;
//{$I VMProtectEnd.inc}
end;


Function CM_Encrypt(valor: AnsiString): AnsiString;
var
  i,k:integer;
  bin:AnsiString;
  s:AnsiString;
const
  a: array [0..1] of AnsiChar = ('a','b');  //a = 0; b = 1;
begin
//{$I VMProtectBegin.inc}
  for i:=1 to length(valor) do
  begin
      bin:=bin+ inttobin(ord(valor[i]));
  end;

  for k:=0 to 1 do
  begin
      for i:=6 downto 1 do
      begin
        //dublica k em string ivezes procurando repetiçoes
        s := CM_DupString(CM_IntToStr(k),i);
        if i = 1 then
           bin := CM_StrReplace(bin,s,'zz'+a[k])
        else
           bin := CM_StrReplace(bin,s,CM_IntToStr(i)+a[k]);
      end;
  end;
  bin := CM_StrReplace(bin,'zz', '1');
  bin := CompressIt(bin);
  bin := formatlen(Base64Encode(bin));
  result := bin;
//{$I VMProtectEnd.inc}
end;

Function RecBin(p: AnsiString):AnsiString;
const  a: array [0..1] of AnsiChar = ('a','b');
begin
//{$I VMProtectBegin.inc}
    result:='';
    if not (p[1] in ['0'..'9']) then
      exit;
    if p[2] = a[0] then
      result := CM_DupString('0',CM_StrToint(p[1]));
    if p[2] = a[1] then
      result := CM_DupString('1',CM_StrToint(p[1]));
//{$I VMProtectEnd.inc}
end;

Function CM_Decrypt(valor: AnsiString): AnsiString;
var
  i:integer;
  s:AnsiString;
  len:AnsiString;
  finalValue: AnsiString;
const a: array [0..1] of AnsiChar = ('a','b');
//=============================
      // Internal Function - Recupera o Binário
      
//=============================
begin
//{$I VMProtectBegin.inc}
finalValue:='';
if valor='' then exit;


while (valor<>'') and (valor[1]='0') do
  valor:=PAnsiChar(copy(valor,2,length(valor)));

try

      finalValue:='';
      if valor = '' then
       exit;

       
      if not IsInt(valor[1]) then
        exit;
      //
      len := copy(valor,2,CM_StrToint(valor[1]));
      if not IsInt(len) then
        exit;
      valor:=copy(valor,11,CM_StrToint(len));
      valor:=Base64Decode(CM_Trim(valor));
      valor:=DecompressIt(valor);
        if (length(valor) mod 2) > 0 then
           valor:=copy(valor,1,length(valor)-1);

        s:='';
        i:=1;
        
        while i<=length(valor) do
        begin
           s:=s+RecBin( copy(valor,i,2) );
           i:=i+2;
        end;
        valor:=s;
        

        i:=1;
        finalValue:='';

        while i<=length(valor) do
        begin
          finalValue := finalValue+AnsiChar(bintoint(copy(valor,i,7)));
          i:=i+7;
        end;
        result := finalValue;

except
  //on e:exception do
  begin
    finalValue := '';
    result := finalValue;
  end;
end;
//{$I VMProtectEnd.inc}
end;

end.
