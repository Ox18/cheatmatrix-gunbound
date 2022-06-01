unit ConfigsLoader;

interface

uses windows, sysutils, IniFiles, common, constantes, classes;

type
   TCMConfig = class
        constructor Create(val: AnsiString);
     private
        valor: AnsiString;
     public
        Function AsString: AnsiString;
        Function AsFloat: double;
        Function AsBoolean: Boolean;
        Function AsInteger: Integer;
        Function AsLong: Int64;
        Function AsPAnsiChar: PAnsiChar;
   end;

   Function LoadConfig(Nome: AnsiString; Padrao: AnsiString = ''): TCMConfig;
   Procedure WriteConfig(Nome: AnsiString; Valor: AnsiString);
   Procedure InicializarConfig;

implementation

{ TCMConfig }

function TCMConfig.AsBoolean: Boolean;
begin
   if valor = '' then
     result := false
   else
   result := StrToBool(valor);
end;

function TCMConfig.AsFloat: double;
begin
   if valor = '' then
     result := 0
   else
   result := StrToFloat(valor);
end;

function TCMConfig.AsInteger: Integer;
begin
   if valor = '' then
     result := 0
   else
    result := StrToInt(valor);
end;

function TCMConfig.AsLong: Int64;
begin
   if valor = '' then
     result := 0
   else
   result := StrToInt64(valor);
end;

function TCMConfig.AsPAnsiChar: PAnsiChar;
begin
   if valor = '' then
     result := ''
   else
   result := PAnsiChar(valor);
end;

function TCMConfig.AsString: AnsiString;
begin
   result := valor;
end;

constructor TCMConfig.Create(val: AnsiString);
begin
   valor := val;
end;

Function LoadConfig(Nome: AnsiString; Padrao: AnsiString = ''): TCMConfig;
var ini: TIniFile;
    valor: AnsiString;
begin
   try
     ini := TIniFile.Create(CMDir+Constante(0066, false));
     valor := ini.ReadString('configs', LowerCase(Nome), Padrao);
     result := TCMConfig.Create(valor);
   finally
     ini.Free;
   end;
end;

Procedure InicializarConfig;
var ini: TIniFile;
    valor: AnsiString;
    lista: TStrings;
begin
  lista := TStringList.Create;
  valor := CMDir+Constante(0066, false);
   if not FileExists(valor) then
       lista.SaveToFile(valor);
end;

Procedure WriteConfig(Nome: AnsiString; Valor: AnsiString);
var ini: TIniFile;
begin
  try
    ini := TIniFile.Create(CMDir+Constante(0066, false));
    ini.WriteString('configs', LowerCase(Nome), valor);
  finally
    ini.Free;
  end;
end;

end.
