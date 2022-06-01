unit CMSysUtils;

interface

//uses windows;

function CM_CopyMemory(destino: PByte; origem: PByte; qnt: integer): Boolean;
function CM_ZeroMemory(destino: PByte; qnt: integer): Boolean;
function CM_LowerCase(valor: AnsiString): AnsiString;
function CM_IntToStr(valor: integer): AnsiString;
function CM_StrToint(valor: AnsiString): integer;
function CM_StrReplace(texto: AnsiString; de: AnsiString; para: AnsiString; ignorecase: boolean = true): AnsiString;
function CM_DupString(valor: AnsiString; qnt: integer): AnsiString;
function CM_Trim(valor: AnsiString): AnsiString;

implementation

function CM_CopyMemory(destino: PByte; origem: PByte; qnt: integer): Boolean;
var i: integer;
begin
   //{$I VMProtectBegin.inc}
   result := false;
   for i := 0 to qnt-1 do
   begin
       PByte(cardinal(destino)+i)^ := PByte(cardinal(origem)+i)^;
   end;
   //{$I VMProtectEnd.inc}
end;

function CM_ZeroMemory(destino: PByte; qnt: integer): Boolean;
var i: integer;
begin
   //{$I VMProtectBegin.inc} 
   result := false;
   for i := 0 to qnt-1 do
   begin
       PByte(cardinal(destino)+i)^ := 0;
   end;
   //{$I VMProtectEnd.inc}
end;

function CM_LowerCase(valor: AnsiString): AnsiString;
var i: integer;
    c: byte;
begin
    //{$I VMProtectBegin.inc} 
    result := '';
    for i := 1 to length(valor) do
    begin
        result := result + valor[i];
        c := ord(valor[i]);
        if (c >= 65) and (c < 90) then
          result[i] := AnsiChar(ord(result[i]) + 32);
    end;
    //{$I VMProtectEnd.inc}
end;

function CM_IntToStr(valor: integer): AnsiString;
var i: integer;
begin
//{$I VMProtectBegin.inc} 
   result := '';
   while valor > 0 do
   begin
       i := (valor mod 10);
       valor := valor div 10;
       result := result + AnsiChar(48 + i);
   end;
   if result = '' then result := '0';
//{$I VMProtectEnd.inc}
end;

function CM_StrToint(valor: AnsiString): integer;
var i: integer;
    c: byte;
begin
//{$I VMProtectBegin.inc} 
   result := 0;
   for i := 1 to length(valor) do
   begin
       c := ord(valor[i]);
       if(c >= 48) and (c <= 57) then
          result := ((result * 10) + (c - 48))
       else
          break;
   end;
//{$I VMProtectEnd.inc}
end;

function CM_StrReplace(texto: AnsiString; de: AnsiString; para: AnsiString; ignorecase: boolean = true): AnsiString;
var i,j: integer;
    valor: AnsiString;
begin
//{$I VMProtectBegin.inc} 
   if (ignorecase) then
     valor := de
   else
     valor := CM_LowerCase(de);

   j := 0;
   result := texto;
   while( pos(de, texto) > 0 ) do
   begin
       i := pos(de, texto);
       j := j + i;

       delete(result, j, length(de));
       insert(para, result, j);

       texto := copy(texto, i+length(de), length(texto));

       j := j + length(para) - 1;
   end;
//{$I VMProtectEnd.inc}
end;

function CM_DupString(valor: AnsiString; qnt: integer): AnsiString;
var i: integer;
begin
//{$I VMProtectBegin.inc} 
    result := '';
    for i := 1 to qnt do
    begin
       result := result + valor;
    end;
//{$I VMProtectEnd.inc}
end;

function CM_Trim(valor: AnsiString): AnsiString;
var i: integer;
    flag: boolean;
begin
//{$I VMProtectBegin.inc} 
  result := '';
  flag := false;
  for i := 1 to length(valor) do
  begin
      if ord(valor[i]) < 32 then
         continue
      else
         flag := true;

      if flag then
      begin
         result := copy(valor, i, length(valor));
         break;
      end;
  end;

  flag := false;
  for i := length(valor) downto 1 do
  begin
      if ord(valor[i]) < 32 then
         continue
      else
         flag := true;

      if flag then
      begin
         result := copy(result, 1, i);
         break;
      end;
  end;
//{$I VMProtectEnd.inc}
end;

end.
