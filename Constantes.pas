unit Constantes;

interface

uses windows, { common, } sysutils, encription, VMProtectSDK;

// {$DEFINE DEBUG_SERVER}

type
  NTStatus = cardinal;
  TIdioma = (NENHUM = -2, GERAL = -1, PT = 0, EN = 1);

const
  APICOUNT = 4;

CONST
  STATUS_SUCCESS = NTStatus($00000000);
  STATUS_ACCESS_DENIED = NTStatus($C0000022);
  STATUS_INFO_LENGTH_MISMATCH = NTStatus($C0000004);
  SEVERITY_ERROR = NTStatus($C0000000);

  // Controle de versão
  MatrizVersion = 1002;
  PostIndex = 1002;

  // Versão do driver - Para evitar crash
  CMDeviceVersion = 1011;

  hexes = '0123456789ABCDEF';
  nh = AnsiString('ghijklmnopqrsutvwxyz');

var
  Idioma: TIdioma = PT;
  UpdateFile: AnsiString;
  PluginsLiberados: array of AnsiString;
  NomesPacotes: array of AnsiString;
  TempoPacotes: array of AnsiString;
  OutrosValores: array of AnsiString;

  // --- Erros ----------------------------------------
Function Constante(index: integer; checkLanguage: boolean): AnsiString;
Function ConstanteX(index: integer; lpIdioma: TIdioma = NENHUM): PAnsiChar;
function makeString(valor: PAnsiChar): PAnsiChar;
Function CMPchar(s: AnsiString): PWideChar;

// const
// pt0009 = 'Falha na conexão com o servidor';

implementation

Function CMPchar(s: AnsiString): PWideChar;
begin
  result := AllocMem(length(s) + 1);
  StringToWideChar(s, result, length(s) + 2);
end;

function GetBuildInfo(full: boolean = false): AnsiString;
var
  V1, // Major Version
  V2, // Minor Version
  V3, // Release
  V4: Word; // Build Number
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  FileNameVar: String;
  a, b, c: AnsiString;
begin
  //
  FileNameVar := ParamStr(0);
  VerInfoSize := GetFileVersionInfoSize(PChar(FileNameVar), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(FileNameVar), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin

    V1 := dwFileVersionMS shr 16;
    V2 := dwFileVersionMS and $FFFF;
    V3 := dwFileVersionLS shr 16;
    V4 := dwFileVersionLS and $FFFF;
  end;

  a := '';
  b := '';
  c := '';

  if (not full) then
  begin

    a := '.' + IntToStr(V2);

    if (V4 <> 0) and (V3 <> 0) then
      b := '.' + IntToStr(V3);

    if V4 <> 0 then
      c := '.' + IntToStr(V4);

  end
  else
  begin
    a := '.' + IntToStr(V2);
    b := '.' + IntToStr(V3);
    if (V4 <> 0) then
      c := '.' + IntToStr(V4);
  end;

  result := IntToStr(V1) + a + b + c;
  FreeMem(VerInfo, VerInfoSize);
  //
end;

{
  Function GetName(index: integer): PAnsiChar;
  begin
  result := PAnsiChar(CMLanguage.Item[index]);
  end; }

var
  diferencial: integer = 0;
  patched: boolean;

procedure PatchConstantes;
var
  encontrado: boolean;
  valor: PAnsiChar;
  oldProtection: DWORD;
  i, j: integer;
begin
  Randomize;
  diferencial := Random(50) + 1;

  for j := 0001 to 0300 do
  begin
    try
      makeString(ConstanteX(j, PT));
      makeString(ConstanteX(j, EN));
      makeString(ConstanteX(j, GERAL));
    except
      on e: exception do
      begin

      end;
    end;
  end;
end;

function makeString(valor: PAnsiChar): PAnsiChar;
var
  encontrado: boolean;
  oldProtection: DWORD;
  i, j: integer;
begin
  if (valor <> nil) and (StrLen(valor) > 0) then
  begin
    VirtualProtect(valor, StrLen(valor), PAGE_EXECUTE_READWRITE,
      @oldProtection);
    for i := 0 to StrLen(valor) - 1 do
    begin
      valor[i] := AnsiChar(Word(Word(valor[i]) + Word(diferencial)) mod $FF);
    end;
    VirtualProtect(valor, StrLen(valor), oldProtection, @oldProtection);
  end;
  result := valor;
end;

Function ConstanteX(index: integer; lpIdioma: TIdioma = NENHUM): PAnsiChar;
var
  encontrado: boolean;
  idiomaAtual: TIdioma;
begin
  VMProtectBegin('cst');
  result := '';
  encontrado := true;

  if (lpIdioma = NENHUM) then
    idiomaAtual := Idioma;

  if (lpIdioma = NENHUM) or (lpIdioma = GERAL) then
  begin
    case index of
      // -- u/Yf0HZa6UzTXBLGz17CVQ==
      // 0020 : begin  result := PAnsiChar(maxDecript('u/Yf0HZa6UzTXBLGz17CVQ==',IntToStr(2009))); {cheatmatrix.net}   end;
      // -- rUsM4HhLPpGxcN8ivTu6Z7hZy2kUAQ0x
      0001: begin result := makeString(PAnsiChar(maxDecript('mYiAndai96lvA33mctoXyIo4IEDg1UKiYJ2ga6xACbrxcyaN2AjBhg==',IntToStr(2009)))); end; // \SYSTEM\CurrentControlSet\Services\
      0004: begin result := ' - by SkyW4rrior & Di4blo '; end;
      0005: begin result := 'localhost'; end;
      0006: begin result := 'ces'; end;
      0008: begin result := 'msvcrt36.dll'; end;
      0019: begin result := makeString (PAnsiChar(AnsiString(Constante(0002, false) + ' ' + AnsiString (GetBuildInfo(true)) + ' ® ' + AnsiString(FormatDateTime ('yyyy', now)) + Constante(0004, false))));end; // UnitMatriz.pas
      0002: begin result := 'Cheat Matrix X'; end;
{$IFDEF DEBUG_SERVER}
      0020: begin result := makeString(PAnsiChar(maxDecript('ILD164GoLoN7pE/v3eqIjw==', IntToStr(2009)))); { cheatmatrix.net } end;
{$ELSE}
      0020: begin result := makeString (PAnsiChar(maxDecript('u/Yf0HZa6UzTXBLGz17CVQ==', IntToStr(2009)))); { cheatmatrix.net } end;
{$ENDIF}
      0021: begin result := makeString (PAnsiChar(maxDecript('N8E6YR9IqR4=', IntToStr(2009)) + Constante (20, false) + maxDecript('AHjIicyuFhPnJ1IOVq1Ac+kENcF31xB8', IntToStr(2009)) + ' ')); { :8080/oldweb/Conector } end; // http://cheatmatrix.net/cmx/CMUpdates/search.php
      0025: begin result := 'Fix'; end;
      0026: begin result := 'connectionport'; end;
      0027: begin result := PAnsiChar('m1'); end;
      0028: begin result := PAnsiChar('m4'); { FixApis } end;
      0029: begin result := PAnsiChar('m2'); { FixOffsets } end;
      0030: begin result := PAnsiChar('m3'); { GetStatus } end;
      0040: begin result := 'Cheat Matrix  -  '; end;
      0041: begin result := 'other'; end;
      0043: begin result := 'testCode'; end;
      0061: begin result := 'Message'; end;
      0062: begin result := '0000'; end;
      0065: begin result := 'helpCode'; end;
      0066: begin result := 'configs.ini'; end;
      0067: begin result := 'configs'; end;
      0076: begin result := 'updates\'; end;
      0077: begin result := 'plugins\'; end;
      0081: begin result := '*.cmp'; end;
      0084: begin result := 'Main'; end;
      0086: begin result := 'Common\'; end;
      0087: begin result := 'Cod2'; end;
      0064: begin result := makeString(PAnsiChar(AnsiString(maxDecript('N8E6YR9IqR4=', IntToStr(2009)) + Constante(20, false) + ':' + IntToStr(8181) + ' '))); end; { 'http://cheatmatrix.net:8080'; }
      0088: begin result := makeString (PAnsiChar(maxDecript('9D2ib/tHaXxcK09p4nIdiQ==', IntToStr(2009)))); { gunbound } end;
      0100: begin result := 'documentos\'; end;
      0101: begin result := 'mdlcmmtx'; end;
      0102: begin result := '.exe'; end;
      0103: begin result := '.dll'; end;
      0104: begin result := 'bcdfghjklmnpqrstvwxz'; end;
      0105: begin result := 'aeiouy'; end;
      0106: begin result := 'cmw'; end;
      0107: begin result := 'msvsct'; end;
      0108: begin result := 'ddraw.dll'; end;
      0109: begin result := 'dmx.dll'; end;
    else
      encontrado := false;
    end;
  end
  else
  begin
    idiomaAtual := lpIdioma;
    encontrado := false;
  end;

  if (not encontrado) and (lpIdioma <> GERAL) then
  begin
    if idiomaAtual = PT then
    begin
      // Constantes são as melhores amigas do crackers... vamos botar todas em uma só função que retorna um PCHAR..
      // seria bom aplicar um pouco de matematica à este index para não ficar tão óbvio o switch, mas agora ja é tarde
      case index of
        0009: begin result := 'Falha na conexão com o servidor'; end;
        0010: begin result := 'Checando consistência dos dados...'; end;
        0011: begin result := 'Erro na conexão com o servidor! Reinicie o CM e tente novamente.'; end;
        0012: begin result := 'Verificando Conexão...'; end; // ny0RthvvVhwScIDfSSCWwaopzCn6rQQhJBxCLsCxOso=
        0031: begin result := 'Plugin inválido ('; end;
        0032: begin result := 'Falha ao carregar o plugin '; end;
        0042: begin result := 'Carregando Plugins...'; end;
        0044: begin result := 'Acesso negado!'; end;
        0052: begin result := 'Pressione F1 se precisar de ajuda'; end;
        0055: begin result := 'Erro ao carregar as configurações. Arquivo modificado.'; end;
        0080: begin result := 'Incompatibilidade de versão entre os arquivos'; end;
        0089: begin result := 'Servidor Indisponivel'; { gunbound } end;
        0091: begin result := 'dias'; end;
        0092: begin result := 'Erro! Execute como administrador.'; end;
        0167: begin result := 'Atualizando arquivos, aguarde... '; end;
        0174: begin result := 'Arquivo de download não encontrado!'; end;
        0175: begin result := 'Falha ao baixar o arquivo de atualização!'; end;
        0176: begin result := 'Falha ao salvar o arquivo de atualização!'; end;
        0177: begin result := 'Servidor sobrecarregado. Tente novamente mais tarde!'; end;
        0200: begin result := 'abrir '; end;
        0201: begin result := 'sair'; end;
        0202: begin result := 'Escolha o Jogo'; end;
        0203: begin result := 'Please wait...'; end;
        0204: begin result := 'Falha na configuração CMX. Certifique-se que o jogo (inclusive o launcher) esteja fechado e que esteja executando o CMX como ADMINISTRADOR. Para mais informações consulte o fórum em http://www.cheatmatrix.com.br/forum'; end;
        0205: begin result := 'CMX não configurado para o jogo selecionado'; end;
        0206: begin result := 'configurar'; end;
        0207: begin result := 'Jogo não encontrado!'; end;
        0208: begin result := 'Erro!'; end;
      end;
    end
    else if idiomaAtual = EN then
    begin
      case index of
        0009: begin result := 'Failed to connect to the server.'; end;
        0010: begin result := 'Checking data consistency ...'; end;
        0011: begin result := 'Error connecting to server! Restart the CMX and try again.'; end;
        0012: begin result := 'Checking Connection...'; end; // ny0RthvvVhwScIDfSSCWwaopzCn6rQQhJBxCLsCxOso=
        0031: begin result := 'Invalid Plugin ('; end;
        0032: begin result := 'Failed to load plugin '; end;
        0042: begin result := 'Loading Plugins...'; end;
        0044: begin result := 'Access denied!'; end;
        0052: begin result := 'Press F1 for help'; end;
        0055: begin result := 'Error loading settings. File corrupted.'; end;
        0080: begin result := 'Version mismatch between the files'; end;
        0089: begin result := 'Server Unavailable'; { gunbound } end;
        0091: begin result := 'days'; end;
        0092: begin result := 'Error!'; end;
        0167: begin result := 'Updating files, please wait... '; end;
        0174: begin result := 'Download file not found!'; end;
        0175: begin result := 'Failed to download the update file!'; end;
        0176: begin result := 'Failed to save the update file!'; end;
        0177: begin result := 'Server overloaded. Try again later!'; end;
        0200: begin result := 'open '; end;
        0201: begin result := 'exit'; end;
        0202: begin result := 'Select a Game'; end;
        0203: begin result := 'Please wait... '; end;
        0204: begin result := 'Failed to configure CMX. Be sure the game (and the launcher) are closed and that CMX is being executed as ADMINISTRATOR. For more information ask in the forum, at http://www.cheatmatrix.com.br/forum'; end;
        0205: begin result := 'CMX not configured for the selected game'; end;
        0206: begin result := 'configure'; end;
        0207: begin result := 'Game launcher not found!'; end;
        0208: begin result := 'Error!'; end;
      end;
    end;
  end;
  VMProtectEnd;
end;

Function Constante(index: integer; checkLanguage: boolean): AnsiString;
var
  valor, resultado: PAnsiChar;
  i: integer;
begin
  result := '';
  valor := ConstanteX(index);

  resultado := GetMemory(StrLen(valor) + 1);
  try
    CopyMemory(resultado, valor, StrLen(valor) + 1);

    for i := 0 to StrLen(valor) - 1 do
    begin
      result := result + AnsiChar
        (Word(Word(valor[i]) - Word(diferencial) + $FF) mod $FF);
    end;
  finally
    if resultado <> nil then
      FreeMemory(resultado);
  end;
end;

initialization

PatchConstantes;

end.
