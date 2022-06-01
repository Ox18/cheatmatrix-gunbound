unit SkyFiles;

interface

uses windows,inifiles,sysutils;

Function WriteIni(Arquivo,Chave,Valor:AnsiString; Secao: AnsiString = 'Options'):boolean;
Function ReadIni(Arquivo,Chave:AnsiString; Secao: AnsiString = 'Options'):AnsiString;
Function BoolToIniBool(valor: boolean):AnsiString;
Function IniBoolToBool(valor: AnsiString):boolean;

implementation

Function BoolToIniBool(valor: boolean):AnsiString;
begin
  if valor then result := 'ON' else result := 'OFF';
end;

Function IniBoolToBool(valor: AnsiString):boolean;
begin
  if valor = 'ON' then result := true else result := false;
end;

Function WriteIni(Arquivo,Chave,Valor:AnsiString; Secao: AnsiString = 'Options'):boolean;
var ini:TIniFile;
begin
ini := TIniFile.Create(Arquivo);
ini.WriteString(Secao,Chave,Valor);
ini.Free;
end;

Function ReadIni(Arquivo,Chave:AnsiString; Secao: AnsiString = 'Options'):AnsiString;
var ini:TIniFile;
begin
ini := TIniFile.Create(Arquivo);
result := ini.ReadString(Secao,Chave,'');
ini.Free;
end;

end.
