unit Constantes;

interface

uses windows, common, MD5Unit, sysutils, encription, VMProtectSDK;

//{$DEFINE DEBUG_SERVER}

type
  NTStatus = cardinal;

const
   APICOUNT = 4;

   CONST
      STATUS_SUCCESS              = NTStatus($00000000);
      STATUS_ACCESS_DENIED        = NTStatus($C0000022);
      STATUS_INFO_LENGTH_MISMATCH = NTStatus($C0000004);
      SEVERITY_ERROR              = NTStatus($C0000000);

   {CM_NEW_SDT : array [1..APICOUNT] of string = (
   'ZWOpenProcess',  //DLL
   'ZWProtectVirtualMemory',
   'ZWReadVirtualMemory',
   'ZWWriteVirtualMemory');}

   //Controle de versão
   MatrizVersion = 1002;
   PostIndex = 1002;

   //Versão do driver - Para evitar crash
   CMDeviceVersion = 1011;

   hexes = '0123456789ABCDEF';
   nh = AnsiString('ghijklmnopqrsutvwxyz');

   //--- Erros ----------------------------------------
   Function Constante(index: integer; checkLanguage: boolean): PAnsiChar;

implementation

function GetBuildInfo(full: boolean = false): AnsiString;
var V1,       // Major Version
    V2,       // Minor Version
    V3,       // Release
    V4: Word; // Build Number
    VerInfoSize, VerValueSize, Dummy : DWORD;
    VerInfo : Pointer;
    VerValue : PVSFixedFileInfo;
    FileNameVar : String;
    a,b,c: AnsiString;
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

   if(not full) then
   begin

   a := '.' + IntToStr(V2);

   if (V4 <> 0) and (V3 <> 0) then
      b := '.' + IntToStr(V3);

   if V4 <> 0 then
      c := '.' + IntToStr(V4);

   end else
   begin
      a := '.' + IntToStr(V2);
      b := '.' + IntToStr(V3);
      if(v4 <> 0) then
      c := '.' + IntToStr(V4);
   end;

   Result :=  IntToStr(V1) + a + b + c ;
   FreeMem(VerInfo, VerInfoSize);
//
end;

{
Function GetName(index: integer): PAnsiChar;
begin
    result := PAnsiChar(CMLanguage.Item[index]);
end;}

Function Constante(index: integer; checkLanguage: boolean): PAnsiChar;
var encontrado: boolean;
begin
VMProtectBegin('cst');
   result := '';
   encontrado := true;
   case index of
       // -- u/Yf0HZa6UzTXBLGz17CVQ==
       //0020 : begin  result := PAnsiChar(maxDecript('u/Yf0HZa6UzTXBLGz17CVQ==',IntToStr(2009))); {cheatmatrix.net}   end;
       // -- rUsM4HhLPpGxcN8ivTu6Z7hZy2kUAQ0x
       0001 : begin  result := PAnsiChar(maxDecript('mYiAndai96lvA33mctoXyIo4IEDg1UKiYJ2ga6xACbrxcyaN2AjBhg==',IntToStr(2009))); end; //\SYSTEM\CurrentControlSet\Services\
       0019 : begin  result := PAnsiChar(AnsiString('Cheat Matrix X '+AnsiString(GetBuildInfo(true))+' ® '+AnsiString(FormatDateTime('yyyy', now))+' - by SkyW4rrior & Di4blo '));  end;//UnitMatriz.pas
       {$IFDEF DEBUG_SERVER}
       0020 : begin  result := PAnsiChar(maxDecript('ILD164GoLoN7pE/v3eqIjw==',IntToStr(2009))); {cheatmatrix.net}   end;
       {$ELSE}
       0020 : begin  result := PAnsiChar(maxDecript('u/Yf0HZa6UzTXBLGz17CVQ==',IntToStr(2009))); {cheatmatrix.net}   end;
       {$ENDIF}
       0021 : begin  result := PAnsiChar(maxDecript('N8E6YR9IqR4=',IntToStr(2009))+StrPas(Constante(20, false))+maxDecript('AHjIicyuFhPnJ1IOVq1Ac+kENcF31xB8',IntToStr(2009))+' '); {:8080/oldweb/Conector} end; //http://cheatmatrix.net/cmx/CMUpdates/search.php
       0025 : begin  result := 'Fix';  end;
       0026 : begin  result := 'connectionport';  end;
       0027 : begin  result := PAnsiChar('m1');  end;
       0028 : begin  result := PAnsiChar('m4'); {FixApis}  end;
       0029 : begin  result := PAnsiChar('m2'); {FixOffsets}  end;
       0030 : begin  result := PAnsiChar('m3'); {GetStatus}  end;
       0040 : begin  result := 'Cheat Matrix  -  '; end;
       0041 : begin  result := 'other'; end;
       0061 : begin  result := 'Message';  end;
       0062 : begin  result := '0000';    end;
       0065 : begin  result := 'helpCode';    end;
       0066 : begin  result := 'configs.ini';    end;
       0067 : begin  result := 'configs';    end;
       0076 : begin  result := 'updates\';    end;
       0077 : begin  result := 'plugins\';    end;
       0081 : begin  result := '*.cmp';    end;
       0084 : begin  result := 'Main';    end;
       0086 : begin  result := 'Common\';    end;
       0087 : begin  result := 'Cod2';    end;
       0064 : begin  result := PAnsiChar(AnsiString(maxDecript('N8E6YR9IqR4=',IntToStr(2009))+Constante(20, false)+':'+IntToStr(8181)+' ')); end;{'http://cheatmatrix.net:8080'; }
       0088 : begin  result := PAnsiChar(maxDecript('9D2ib/tHaXxcK09p4nIdiQ==',IntToStr(2009))); {gunbound}    end;
       else
           encontrado := false;
   end;

   if not encontrado then
   begin
     if Idioma = PT then
     begin
        //Constantes são as melhores amigas do crackers... vamos botar todas em uma só função que retorna um PCHAR..
        //seria bom aplicar um pouco de matematica à este index para não ficar tão óbvio o switch, mas agora ja é tarde
        case index of
           0009 : begin  result := 'Falha na conexão com o servidor'; end;
           0010 : begin  result := 'Checando consistência dos dados...'; end;
           0011 : begin  result := 'Erro na conexão com o servidor! Reinicie o CM e tente novamente.'; end;
           0012 : begin  result := 'Verificando Conexão...'; end; //                                                                                                              ny0RthvvVhwScIDfSSCWwaopzCn6rQQhJBxCLsCxOso=
           0031 : begin  result := 'Plugin inválido (';  end;
           0032 : begin  result := 'Falha ao carregar o plugin ';  end;
           0042 : begin  result := 'Carregando Plugins...'; end;
           0044 : begin  result := 'Acesso negado!'; end;
           0052 : begin  result := 'Pressione F1 se precisar de ajuda'; end;
           0055 : begin  result := 'Erro ao carregar as configurações. Arquivo modificado.'; end;
           0080 : begin  result := 'Incompatibilidade de versão entre os arquivos'; end;
           0089 : begin  result := 'Servidor Indisponivel'; {gunbound}    end;
           0091 : begin  result := 'dias';    end;
           0092 : begin  result := 'Erro!'; end;
           0167 : begin  result := 'Atualizando arquivos, aguarde... '; end;
           0174 : begin  result := 'Arquivo de download não encontrado!'; end;
           0175 : begin  result := 'Falha ao baixar o arquivo de atualização!'; end;
           0176 : begin  result := 'Falha ao salvar o arquivo de atualização!'; end;

           0200 : begin  result := 'abrir '; end;
           0201 : begin  result := 'sair'; end;
           0202 : begin  result := 'Escolha o Jogo'; end;
           0203 : begin  result := 'Please wait...'; end;
        end;
     end else if idioma = EN then 
     begin
        case index of
           0009 : begin  result := 'Failed to connect to the server.'; end;
           0010 : begin  result := 'Checking data consistency ...'; end;
           0011 : begin  result := 'Error connecting to server! Restart the CMX and try again.'; end;
           0012 : begin  result := 'Checking Connection...'; end; //                                                                                                            ny0RthvvVhwScIDfSSCWwaopzCn6rQQhJBxCLsCxOso=
           0031 : begin  result := 'Invalid Plugin (';  end;
           0032 : begin  result := 'Failed to load plugin ';  end;
           0042 : begin  result := 'Loading Plugins...'; end;
           0044 : begin  result := 'Access denied!'; end;
           0052 : begin  result := 'Press F1 for help'; end;
           0055 : begin  result := 'Error loading settings. File corrupted.'; end;
           0080 : begin  result := 'Version mismatch between the files'; end;
           0089 : begin  result := 'Server Unavailable'; {gunbound}    end;
           0091 : begin  result := 'days';    end;
           0092 : begin  result := 'Error!'; end;
           0167 : begin  result := 'Updating files, please wait... '; end;
           0174 : begin  result := 'Download file not found!'; end;
           0175 : begin  result := 'Failed to download the update file!'; end;
           0176 : begin  result := 'Failed to save the update file!'; end;

           0200 : begin  result := 'open '; end;
           0201 : begin  result := 'exit'; end;
           0202 : begin  result := 'Select a Game'; end;
           0203 : begin  result := 'Please wait...'; end;
        end;
     end;
   end;

  (*
       0001 : result := 'Falha ao Iniciar. Erro n° 30';
       0002 : result := 'Falha ao Iniciar. Erro n° 31';
       0003 : result := 'Falha ao Iniciar. Erro n° 32';
       0004 : result := 'Falha ao Iniciar. Erro n° 33';
       0005 : result := 'Falha ao Iniciar. Erro n° 34';
       0006 : result := 'Falha ao Iniciar. Erro n° 35';
       0007 : result := 'Falha ao Iniciar. Erro n° 36';
       0008 : result := 'Falha ao Iniciar. Erro n° 37';
       0013 : result := 'Falha ao Iniciar. Erro n° 38'; //ShareMemUnit.pas
       0014 : result := 'Falha ao Iniciar. Erro n° 39'; //CMClasses.pas

       0009 : result := 'Falha na conexão com o servidor';
       0010 : result := 'Checando consistência dos dados...';
       0011 : result := 'Erro na conexão com o servidor! Reinicie o CM e tente novamente.';
       0012 : result := 'Verificando Conexão...';

       0015 : result := 'Falha de proteção no setor 15-601. A versão do dump é diferente da atual.';      //PEUtils.pas
       0016 : result := 'Falha de proteção no setor 15-322. A proteção será desativada';                  //PEUtils.pas
       0017 : result := 'Algum programa tentou modificar a memória do CM. A modificação foi revertida.';  //PEUtils.pas
       0018 : result := 'Algum programa tentou modificar a memória do CM. A modificação foi não pôde ser revertida.';  //PEUtils.pas

                      //UnitMatriz.pas
       0021 : result := 'Falha de proteção no setor 15-321. A proteção será desativada';                  //PEUtils.pas
       0022 : result := 'Falha de proteção no setor 15-325. A proteção será desativada';                  //PEUtils.pas
       0023 : result := 'Falha ao Iniciar. Erro n° 62';
       0024 : result := 'Falha ao Iniciar. Erro n° 40';
       0025 : result := 'Falha ao Iniciar. Erro n° 41';
       0026 : result := 'Falha ao Iniciar. Erro n° 42';
       0027 : result := 'Falha ao Iniciar. Erro n° 64';

       0028 : result := PCHAR('Há um arquivo do CM faltando. Para recuperá-lo, o CM deve reiniciar o computador.'+
               #13#10+' Para reiniciar agora clique em SIM. Caso deseje reiniciar mais tarde clique NÃO.');

       0029 : result := PCHAR('Atenção! É necessário que qualquer anti-hack esteja desligado.'+#13#10+
              'Certifique-se de que nenhum anti-cheat foi aberto depois que você ligou o computador.'+
              #13#10+'Para prosseguir clique OK.');
       0030 : result := 'Falha ao Iniciar. Erro n° 43';
       0031 : result := 'Erro!';
       0032 : result := 'Atenção!';
       0033 : result := 'Falha ao Iniciar. Erro n° 44';
       0034 : result := 'Falha ao Iniciar. Erro n° 45';
       0035 : result := 'Falha ao Iniciar. Erro n° 60';
       0036 : result := 'Versão Inválida. Os arquivos não são compatíveis';
       0037 : result := 'Falha ao Iniciar. Erro n° 61';
       0038 : result := 'Falha ao Iniciar. Erro n° 63';
       0039 : result := 'Falha ao Iniciar. Erro n° 65';
       0040 : result := 'Cheat Matrix  -  ';
       0041 : result := 'Autenticação concluída! Carregando configurações...';
       0042 : result := 'Carregando Plugins...';
       0043 : result := 'Você não tem permissão para usar hacks do CM';
       0044 : result := 'Acesso negado!';
       0045 : result := 'Falha ao adiquirir lista de arquivos';
       0046 : result := PCHAR('   Você não escolheu o jogo para o qual deseja obter a lista de atualizações.'+
              #13#10+'   Leia o manual do CM para obter maiores informações');
       0047 : result := 'Atenção';
       0048 : result := 'Jogo inválido';
       0049 : result := 'Updates deletados';
       0050 : result := 'Seu tempo restante de uso do Cheat Matrix é de ';
       0051 : result := 'dias ';
       0052 : result := 'Pressione F1 se precisar de ajuda';
       0053 : result := 'Analizando lista de processos...';
       0054 : result := 'Acesso negado...';
       0055 : result := 'Erro ao carregar as configurações. Arquivo modificado.';
       0056 : result := PCHAR('Erro ao carregar o arquivo de configurações. O CMX será fechado'+#13#10+
              'Se o problema persistir contacte o administrador.');
       0057 : result := 'Verificando atualizações...';
       0058 : result := 'Um arquivo essencial do CMX não foi encontrado. Reinstale o aplicativo.';
       0059 : result := 'Arquivo não encontrado!';
       0060 : result := 'Seu computador está com a data incorreta!';
       0076 : result := 'Falha ao Iniciar. Erro n° 67';
       0077 : result := 'Falha ao Iniciar. Erro n° 68';
       0078 : result := 'Um arquivo essencial do CMX não foi encontrado. Por favor reinstale o software';
       0079 : result := 'Falha ao Iniciar. Erro n° 69';


       //---------- Principal
       0100 : result := 'Escolha o pacote';     //HackList Item[0]
       0101 : result := 'Escolha o processo:';  //Principal
       0102 : result := ' Ordem ';         //Principal
       0103 : result := ' Ordenar por ';  //Principal
       0104 : result := 'Crescente';      //Mostrador
       0105 : result := 'Descrescente';   //Mostrador
       0106 : result := 'Criação';  //Mostrador
       0107 : result := 'Nome';     //Mostrador
       0108 : result := 'PID';      //Mostrador

       //---------- FrameUpdates
       0120 : result := 'Status';      //Aba
       0121 : result := 'Plugins';     //Aba
       0122 : result := 'Controles';   //Aba

       0123 : result := 'Novo';     //Aba Status
       0124 : result := 'Normal';   //Aba Status
       0125 : result := 'Desatualizado';          //Aba Status
       0126 : result := 'Arquivos a instalar:';   //Aba Status
       0127 : result := 'Arquivos a baixar:';     //Aba Status
       0128 : result := 'Tamanho restante:';      //Aba Status
       0129 : result := 'Velocidade:';            //Aba Status
       0130 : result := 'Tempo restante:';        //Aba Status
       0131 : result := 'Verificar apenas disponíveis';     //Aba Status
       0132 : result := 'Listar plugins p/ todos os jogos'; //Aba Status
       0133 : result := 'Atualizar lista'; //Aba Status - Botão
       0134 : result := 'Atualizar'; //Aba Status - Botão
       0135 : result := 'Baixar';    //Aba Status - Botão
       0136 : result := 'Limpar';    //Aba Status - Botão
       0137 : result := 'Esconder';  //Aba Status - Botão
       0138 : result := 'Instalar';  //Aba Status - Botão
       0139 : result := 'Cancelar';  //Aba Status - Botão

       0140 : result := 'Todos';       //Aba Plugins - TabSheet3
       0141 : result := 'Disponíveis'; //Aba Plugins - TabSheet4

       0142 : result := 'ID';       //ListView
       0143 : result := 'Plugin';   //ListView
       0144 : result := 'Arquivo';  //ListView
       0145 : result := 'Jogo';     //ListView
       0146 : result := 'Tamanho';  //ListView
       0147 : result := 'Criação';  //ListView
       0148 : result := 'Autor';    //ListView
       0149 : result := 'Status';   //ListView

       //---------- FrameOptions
       0160 : result := ' Conexão ';
       0161 : result := ' Lista de Processos ';
       0162 : result := 'Usar Porta';
       0163 : result := 'Não mostrar processos ocultos e já finalizados';
       0164 : result := 'OK';
       0165 : result := 'Cancelar';
       0166 : result := 'Falha ao instalar o arquivo ';
       0167 : result := 'Atualizando arquivos, aguarde... ';
       0168 : result := 'Arquivos atualizados!';
       0169 : result := 'Houve uma falha na atualização anterior. Deseja prosseguir e tentar novamente?';
       0170 : result := 'O CMX será fechado. Se o problema consistir contecte o administrador';
       0171 : result := 'Atenção';
       0172 : result := 'Falha ao Iniciar. Erro n° 70';
       0173 : result := 'Falha ao Iniciar. Erro n° 71';

       0174 : result := 'Arquivo de download não encontrado!';
       0175 : result := 'Falha ao baixar o arquivo de atualização!';
       0176 : result := 'Falha ao salvar o arquivo de atualização!';

    *)
    VMProtectEnd;
end;



end.



