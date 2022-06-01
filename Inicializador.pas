unit Inicializador;

interface

//{$DEFINE NO_HASH_MATRIZ}

uses
  //-- Delphi --------------------------------------------------
  Windows, ExtCtrls, ImgList, Controls, StdCtrls, ComCtrls,
  Messages, sysutils, Graphics, dialogs,
  Classes, Forms, syncobjs, WinSvc, registry, ExtActns, Wininet,
  DateUtils, ComObj, variants, strutils, jpeg,
  Gauges, Buttons, OleCtrls, XPMan, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdBaseComponent, IdAntiFreezeBase, IdAntiFreeze, ButtonWithColor, Menus,
   inifiles, SkyIO, ProcListing, CMClasses, Encription, functions,
   cmini, ConfigsLoader, frmOptions, common, constantes, CmPlugins, Utils,
   SplashScreen, SkySql, VMProtectSDK,
  CMStatus, langcontrol, GIFImg;

  var
    UpIni: TCMIni;
    IniAutenticacao: TCMIni;
    IniKeys: TStrings;

    const
	versaoCMX = 4000;

  function inicializar: integer;
  function ativarCMX(usuario: AnsiString; senha: AnsiString; codigo: AnsiString): integer;

implementation

uses ShellAPI, Urlmon, UnitMatriz, base64;

procedure logar(valor: AnsiString);
var log: tstrings;
begin
{
    log := TStringList.Create;
    if FileExists( ExtractFilePath(ParamStr(0))+'log.txt') then
       log.LoadFromFile(ExtractFilePath(ParamStr(0))+'log.txt');

    log.Add(valor);
    log.SaveToFile(ExtractFilePath(ParamStr(0))+'log.txt'); }
end;

Function SafeStrToInt(valor: AnsiString): integer;
var i: integer;
    s: AnsiString;
begin
    valor := trim(valor);
    s := '';

    for i := 1 to length(valor) do
    begin
        if Pos(valor[i],'0123456789') > 0 then
           s := s+valor[i];
    end;
    valor := s;

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

function verificaErro(PostResult: AnsiString):integer;
var tmppos: integer;
begin
  tmppos := pos(md5(inttostr(1000)), PostResult);
  result := 0;

  if tmppos > 0 then
  begin
      if tmppos > 2 then
      begin
          result := SafeStrToInt(Copy(PostResult, 1, 2));
      end;

      PostResult := trim(Copy(PostResult, tmppos+32, length(PostResult)));
      if PostResult = '' then
         PostResult := Constante(0044, false)
      else
         PostResult := Base64Decode(PostResult);

         //ShowMessage(PostResult);
      if result = 1 then
      begin
         ChangeSplash(PostResult);
         telaAtivacao;
         exit;
      end;

      if result = 10 then
      begin
          ChangeSplash(PostResult);
          Delay(2000);
          exit;
      end;

      ChangeSplash(PostResult);
      Delay(3000);
      ExitCM;
  end else
  if trim(PostResult) = '' then
  begin
      logar('Nenhum resultado!');
      ChangeSplash(Constante(0009, false));
      Delay(3000);
      ExitCM;
  end;
end;

function ativarCMX(usuario: AnsiString; senha: AnsiString; codigo: AnsiString): integer;
var Post: TPost;
    HD: AnsiString;
    Ini: TCMIni;
    s: AnsiString;
    PostResult: AnsiString;
    i: integer;
    Keys: TStrings;
    tmppos: integer;
begin
    VMProtectBegin('prtatv');
    Randomize;
    result := Random(46)*Random(1000);
    if(result mod 10) = 0 then
    inc(result);
    HD := GetHDCode;
    Ini := TCMIni.Create(HD, '');

    Post := TPost.Create(false);
    Post.Port := 8080;
    Post.Host := trim(AnsiString(Constante(0021, false)));
    Post.Clear;
    s := MaxEncript(HD);
    Post.Add(IntToStr(9234+PostIndex), codigo);
    Post.Add(IntToStr(9235+PostIndex), s);
    Post.Add(IntToStr(9236+PostIndex), usuario);
    Post.Add(IntToStr(9237+PostIndex), senha);

    try
        PostResult := trim(Post.Execute);
        PostResult := AnsiReplaceStr(PostResult,#$D#$A,'');
        if verificaErro(PostResult) = 10 then
        begin
            telaAtivacao;
            Delay(2000);
            result := Inicializar;
            if ((result mod 10) = 0) then
            begin
                Application.CreateForm(TfrmMatriz, frmMatriz);
                Application.Run;
            end;
        end;
    except
        on e:Exception do
        begin
           ChangeSplash( Constante(0092, false)+' (code'+IntToStr(237)+')' );
           delay(1000);
           ExitCM;
        end;
    end;
    VMProtectEnd;
    {Ini.LoadFromText(PostResult);
    Keys := TStringList.Create;
    Ini.ListKeys(Keys);

    for i := 0 to Keys.Count-1 do
    begin
        if ( Keys.Strings[i][1]+Keys.Strings[i][2] = '03' ) then
        begin
            PostResult := Ini.getvalue(Keys.Strings[i], Constante(0061, false), Constante(0044, true)).AsString;
            ChangeSplash(PostResult);
            Delay(3000);
        end;
    end;

    if Ini.getvalue('Geral', 'Action', 0).AsInteger = 10 then
    begin
        Delay(6000);
        ExitCM;
    end; }
end;

Function GetDownloadSize(arq: AnsiString): int64;
var response:TMemoryStream;
    res: AnsiString;
begin
//    Splash.
end;

Procedure ReloadCM;
var StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  tempo : cardinal;
begin
    if( SafeStrToInt('0'+paramstr(1)) >= 1 ) then
    begin
        ChangeSplash( Constante(0080, false) );
        delay(3000);
        ExitCM;
    end;

    tempo := GetTickCount;
    while not CriaProcesso(ExtractFilePath(ParamStr(0)) + updatefile + ' ' + IntToStr(SafeStrToInt('0'+ParamStr(1))+1), 0) do
    begin
        Delay(200);
        if ( GetTickCount - tempo ) > 6000 then
           break;
    end;
    ExitCM;
end;

procedure atualizarDoServidor;
var
  arquivos: TStrings;
  i: integer;
  response:TMemoryStream;
  res: AnsiString;
  tamanho, tamanhoTotal: int64;
  tamanhos: array of int64;
  diretorio: AnsiString;
  upPath: AnsiString;
  upFullPath: AnsiString;
  UpdateNow: Boolean;
begin
    VMProtectBegin('prtatt');
  try
        Arquivos := TStringList.Create;
        response := TMemoryStream.Create;
        IniAutenticacao.ListValues(IntToStr(1159), arquivos);
        SetLength(tamanhos, arquivos.Count-1);
        diretorio := ExtractFilePath(ParamStr(0));
        UpdateNow := false;
        totalBaixados := 0;
        tamanhoTotal := 0;
        totalAtual := 0;

        ChangeSplash(Constante(0167, false));

        upPath := ExtractFilePath(ParamStr(0))+Constante(0076, false);

        for i := 0 to arquivos.Count -1 do
        begin
             if arquivos[i] = IntToStr(1159) then
             begin
                arquivos.Delete(i);
                break;
             end;
        end;

        // Pega o tamanho dos arquivos
        for i := 0 to arquivos.Count -1 do
        begin
             if arquivos[i] = IntToStr(1159) then
                continue;

             tamanho := SafeStrToInt(Splash.IdHTTP2.Get( trim(Constante(0021, false)) + '?' + md5(IntToStr(5123+PostIndex)) + '=' + UrlEncode( IniAutenticacao.getvalue(IntToStr(1159), arquivos[i]).AsString ) + '&' + md5(IntToStr(5124+PostIndex)) + '=' + UrlEncode( MD5(GetHDCode) ) ));
             tamanhoTotal := tamanhoTotal + tamanho;
             tamanhos[i] := tamanho;
        end;

        totalGeral := tamanhoTotal;

        // Baixa os arquivos
        for i := 0 to arquivos.Count -1 do
        begin
            if arquivos[i] = IntToStr(1159) then
                continue;

            try
                if tamanhos[i] = 0 then
                begin
                  MessageBox(0, CMPchar(Constante(0174, false)), CMPchar(Constante(0092, true)), 0);
                  exit;
                end;

                //Splash.Panel6.Caption := ExtractFileName(arquivos[i]);
                //Splash.Panel6.Update;
                //Application.ProcessMessages;

                //totalAtual :=  tamanhos[i];
                Splash.IdHTTP2.Get( Trim( Constante(0021, false) )+ '?' + md5(IntToStr(5113+PostIndex)) + '=' + UrlEncode( IniAutenticacao.getvalue(IntToStr(1159), arquivos[i]).AsString ) + '&' + md5(IntToStr(5124+PostIndex)) + '=' + UrlEncode( MD5(GetHDCode) ) ,response );

                if( response.size = 0) then
                begin
                      MessageBox(0, CMPchar(Constante(0175, false)), CMPchar(Constante(0092, false)), 0);
                      exit;
                end;

                //totalBaixados := totalBaixados + tamanhos[i];
            except
                on e:exception do
            end;

            try
                try
                    upFullPath := ExtractFilePath(upPath+arquivos[i]);

                    if FileExists(upPath+arquivos[i]) then
                    begin
                        try
                           DeleteFile(upPath+arquivos[i]);
                        except
                        end;
                    end;

                    ForceDirectories( upFullPath );
                    response.SaveToFile(upPath+arquivos[i]);
                    UpdateNow := true;

                except
                  on e:exception do
                  begin
                    MessageBox(0, CMPchar(Constante(0176, false)), CMPchar(Constante(0092, false)), 0);
                    exit;
                  end;
                end;
            finally
                response.Clear;
            end;
        end;

        if (FileExists(ExtractFilePath(ParamStr(0))+Constante(0076, false)+UpdateFile)) and (length(UpdateFile) > 0) then
        begin
           try
               MoveFileEx( CMPchar(ExtractFilePath(ParamStr(0))+Constante(0076, false)+UpdateFile), CMPChar(ExtractFilePath(ParamStr(0))+UpdateFile), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH or MOVEFILE_COPY_ALLOWED);
           except
              on e: Exception do
           end;
        end;

        if UpdateNow then
           ReloadCM;

  except
      on e:exception do
      begin
          MessageBox(0, CMPchar(Constante(0092, false)), CMPchar(Constante(0092, false)), 0);
          ExitCM;
      end;
  end;
  VMProtectEnd;
end;

function inicializar: integer;
  var Post: TPost;
      HD: AnsiString;
      fPath, sPath, s: AnsiString;
      smpstr: AnsiString;
      PostResult: AnsiString;
      tmppos: integer;
      i,j: integer;
      Local, UpdatePath: AnsiString;
     // LangArq: AnsiString;
      SearchResult: TSearchRec;
      Searcher: TSearcher;
      IniUpdates: TCMIni;
      IniFile: TStrings;
      CurDir: AnsiString;
      hackModules: TStrings;
      ini: TIniFile;
      debugString: String;
      lingua: String;
begin
VMProtectBegin('prtinc');
  try
      Randomize;
      result := Random(46)*Random(1000);
      if(result mod 10) = 0 then
        inc(result);

      hackModules := TStringList.Create;

      // O ini de atualização é encriptado com a chave de HD do usuário
      // (o mesmo é feito no servidor)
      Local := ExtractFilePath(ParamStr(0));
      UpdatePath := Local+Constante(0076, false);
      UpIni := TCMIni.create(GetHDCode,'');
      ini := TIniFile.Create(Local+Constante(0066, false));
      ini.EraseSection(Constante(0041, false));
      debugString := ini.ReadString(Constante(0067, false), Constante(0065, false), IntToStr(1));
      ini.Free;

      // Identifica o nome deste executavel e seu hash
      UpIni.setvalue( IntToStr(0091), IntToStr(0001), md5(LowerCase(ExtractFileName(ParamStr(0)))) );
      UpIni.setvalue( IntToStr(0091), IntToStr(0002), MD5File(ParamStr(0)) );
      //{$IFDEF NO_HASH_MATRIZ}
      UpIni.setvalue( IntToStr(0091), IntToStr(0004), debugString );
      UpIni.setvalue( IntToStr(0091), IntToStr(0005), debugString );
      UpIni.setvalue( IntToStr(0091), IntToStr(0007), versaoCMX);
      //{$ENDIF}

      {LangArq := ExtractFilePath(ParamStr(0)) + LoadConfig(StrPas(Constante(0079, false))).AsString;
       := TCMLanguage.create(LangArq);
      if not CMLanguage.Load then
      begin
          ChangeSplash(StrPas(Constante(0080, false)));
          delay(3000);
          ExitCM;
      end;  }

      {$IFNDEF NOAUT}
          ChangeSplash( Constante(0203, false) );
          HD := GetHDCode;
          IniAutenticacao := TCMIni.Create(HD, '');
          IniUpdates := TCMIni.Create(HD, '');

          try

              //Cria pasta de atualizações se não existir
              fPath := ExtractFilePath( ParamStr(0) )+'Updates\';
              if not DirectoryExists( fPath ) then
                  CreateDir(fPath);

              sPath := ExtractFilePath( ParamStr(0) )+'Screenshots\';
              if not DirectoryExists( sPath ) then
                  CreateDir(sPath);

              {$IFNDEF DEBUG6}
                  // Configura a conexão
                  Post := TPost.Create(false);
                  Post.Host := Trim(Constante(0021, false));
                  Post.Clear;
                  smpstr := MaxEncript(HD);

                  logar(Post.Host);

                  // Solicita a lista de arquivos necessários
                  Post.Add(IntToStr(1010+PostIndex), smpstr);
                  Post.Add(IntToStr(1157+PostIndex), smpstr);
                  Post.Add(IntToStr(1152+PostIndex), debugString);

                  try
                      PostResult := trim(Post.Execute);
                      PostResult := AnsiReplaceStr(PostResult,#$D#$A,'');
                      if verificaErro(PostResult) > 0 then
                         exit;
                      IniUpdates.LoadFromText(PostResult);
                      IniFile := TStringList.Create;
                      IniUpdates.ListValues(IntToStr(1157), IniFile);

                      lingua := IniUpdates.getvalue(IntToStr(1551), IntToStr(1551), 'PT_BR').AsString;
                      if lingua = 'EN_US' then
                         idioma := EN
                      else
                         idioma := PT;

                      CurDir := ExtractFilePath(ParamStr(0));
                      // Recebida a lista, devolve as caracteristicas de cada arquivos
                      // solicitado: nome e hash
                      for i := 0 to IniFile.Count - 1 do
                      begin
                          if FileExists(CurDir+IniFile.Strings[i]) then
                          begin
                              // Adiciona o HASH de cada arquivo solicitado pelo servidor
                              // na lista e envia
                              UpIni.setvalue( IntToStr(1158), (IniFile.Strings[i]), (MD5File(CurDir+IniFile.Strings[i])) );
                          end;
                      end;
                  except
                      on e:Exception do
                      begin
                         ChangeSplash( Constante(0009, false)+ ' - '+ e.ToString );
                         delay(3000);
                         ExitCM;
                      end;
                  end;

                  Post.Clear;
                  Post.Add(IntToStr(1010+PostIndex), smpstr);
                  Post.Add(IntToStr(8131+PostIndex), UpIni.Show);
                  Post.Add(IntToStr(1152+PostIndex), debugString);

                  try
                      // Dados enviados, se estiver tudo ok, o servidor retornará
                      // os dados para funcionamento, senão, retornará uma lista de
                      // arquivos a servem baixados
                      PostResult := trim(Post.Execute);
                      PostResult := AnsiReplaceStr(PostResult,#$D#$A,'');
                      if verificaErro(PostResult) > 0 then
                         exit;
                  except
                      on e:Exception do
                      begin
                         //ChangeSplash( StrPas(Constante(0092, false))+' (code'+IntToStr(237)+')' );
                         ChangeSplash( Constante(0009, false) + ' - '+ e.ToString );
                         delay(3000);
                         ExitCM;
                      end;
                  end;

                  IniAutenticacao.LoadFromText(PostResult);
                  IniKeys := TStringList.Create;
                  IniAutenticacao.ListKeys(IniKeys);

                  //- Updater
                  if IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9994),IntToStr(0)).AsString <> '0' then
                     UpdateFile := IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9994),IntToStr(0)).AsString;


                  // Se recebeu lista para download, baixa os arquivos
                  if IniAutenticacao.getvalue(IntToStr(1159), IntToStr(1159)).AsInteger = 1159 then
                  begin
                       atualizarDoServidor;
                       Exit;
                  end;

                  //--- Pega o nome dos módulos a serem carregados ---

                  //- Driver
                  //if IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9991),IntToStr(0)).AsString <> '0' then
                  //   ModuloSistema := IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9991),IntToStr(0)).AsString;

                  //- Wrapper
                  //if IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9992),IntToStr(0)).AsString <> '0' then
                  //   WrapperMS := IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9992),IntToStr(0)).AsString;

                  //- Wrapper
                  //if IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9993),IntToStr(0)).AsString <> '0' then
                  //   MutantPrefix := IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9993),IntToStr(0)).AsString;

                  IniFile := TStringList.Create;
                  IniAutenticacao.ListValues(IntToStr(6834), IniFile);
                  for i := 0 to IniFile.Count-1 do
                  begin
                      SetLength(NomesPacotes, length(NomesPacotes)+1);
                      SetLength(TempoPacotes, length(TempoPacotes)+1);

                      NomesPacotes[length(NomesPacotes)-1] := IniFile[i];
                      TempoPacotes[length(TempoPacotes)-1] := IniAutenticacao.getvalue(IntToStr(6834), IniFile[i], '0').AsString;
                  end;

                  IniFile.Free;
                  IniFile := TStringList.Create;
                  IniAutenticacao.ListValues(IntToStr(6835), IniFile);
                  for i := 0 to IniFile.Count-1 do
                  begin
                      SetLength(OutrosValores, length(OutrosValores)+1);
                      OutrosValores[length(OutrosValores)-1] := IniAutenticacao.getvalue(IntToStr(6835), IniFile[i], '').AsString;
                  end;
                  IniFile.Free;
                  // Coloca no array os plugins liberados para carregar
                  {IniAutenticacao.ListValues(IntToStr(5551), hackModules);
                  for i := 0 to hackModules.Count-1 do
                  begin
                    SetLength(PluginsLiberados, length(PluginsLiberados)+1);
                    PluginsLiberados[length(PluginsLiberados)-1] := hackModules[i];
                  end;     }

                  s := IniAutenticacao.Text;
                  if length(s) = 0 then
                    exit;

                  result := 1000*random(150);
              {$ENDIF}
          except
            on e:exception do
            begin
                ChangeSplash( Constante(0176, false)+' (code'+IntToStr(254)+')' );
                delay(3000);
                ExitCM
            end;
          end;
      {$ENDIF}
  except
      on e:exception do
      begin
          MessageBox(0, CMPchar(Constante(0092, false)), CMPchar(Constante(0092, false)), 0);
          ExitCM;
      end;
  end;
  VMProtectEnd;
end;

end.
