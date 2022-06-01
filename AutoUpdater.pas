unit AutoUpdater;

interface

uses windows, sysutils, ExtActns, Common, constantes, classes, CMIni, utils, unitjoiner,
splashscreen, SkySQL, DriverHandler;

//Function StartUpdate(Ini: TCMIni): boolean;
Procedure ShowMessages(Modo: integer; pIni: PCMIni = nil);

implementation

uses UnitMatriz, FrameUpdates, StrUtils, Inicializador;

var
Joiner: TJoiner;

function WindowsDirectory: AnsiString;
var
  WinDir: PChar;
begin
  WinDir := StrAlloc(MAX_PATH);
  GetWindowsDirectory(WinDir, MAX_PATH);
  Result := StrPas(WinDir);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  StrDispose(WinDir);
end;

function SystemDirectory: AnsiString;
var
  SysDir: PChar;
begin
  SysDir := StrAlloc(MAX_PATH);
  GetSystemDirectory(SysDir, MAX_PATH);
  Result := StrPas(SysDir);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  StrDispose(SysDir);
end;

function GetTempDir: AnsiString;
var
  TmpDir: PChar;
begin
  TmpDir := StrAlloc(MAX_PATH);
  GetTempPath(MAX_PATH, TmpDir);
  Result := StrPas(TmpDir);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  StrDispose(TmpDir);
end;

Function FixPath(valor: AnsiString; removeMask: boolean = false): AnsiString;
begin
    if ( Pos('<root>', valor) = 0 ) and
       ( Pos('<windows>', valor) = 0 ) and
       ( Pos('<system>', valor) = 0 ) and
       ( Pos('<temp>', valor) = 0 ) then
    begin
        if not removeMask then
           valor := AnsiString(ExtractFilePath(ParamStr(0))+valor)
    end else
    begin
        if not removeMask then
        begin
            valor := AnsiReplaceStr(valor, '<root>', ExtractFilePath(ParamStr(0)));
            valor := AnsiReplaceStr(valor, '<windows>', WindowsDirectory);
            valor := AnsiReplaceStr(valor, '<system>', SystemDirectory);
            valor := AnsiReplaceStr(valor, '<temp>', GetTempDir);
        end else
        begin
            valor := AnsiReplaceStr(valor, '<root>', '');
            valor := AnsiReplaceStr(valor, '<windows>', '');
            valor := AnsiReplaceStr(valor, '<system>', '');
            valor := AnsiReplaceStr(valor, '<temp>', '');
        end;
    end;
    result := valor;
end;

Procedure ShowMessages(Modo: integer; pIni: PCMIni = nil);
var //ini: PCMIni;
    UpdatePath: AnsiString;
    Local: AnsiString;
    Keys: TStrings;
    i, j, k: integer;
    Level: integer;

    Function TraduzIcone(valor: AnsiString): integer;
    begin
       result := 0;
       valor := trim(LowerCase(valor));
       if( valor <> '' ) then
       begin
           if valor[1] = 'w' then result := MB_ICONWARNING
           else
           if valor[1] = 'a' then result := MB_ICONASTERISK
           else
           if valor[1] = 'e' then result := MB_ICONERROR
           else
           if valor[1] = 'x' then result := MB_ICONEXCLAMATION
           else
           if valor[1] = 'q' then result := MB_ICONQUESTION;
       end;
    end;
begin
    Local := ExtractFilePath(ParamStr(0));
    UpdatePath := Local+'Updates\';

    Keys := TStringList.Create;

    if(pIni = nil) then
    begin
      pIni := new(PCMIni); 
      pIni^ := TCMIni.create(GetHDCode, UpdatePath+'update.cmx');
    end;
     
    pIni^.ListKeys(Keys);

    for i:= 0 to Keys.Count-1 do
    begin
        if( (keys[i][1]+keys[i][2]) <> '01' ) then
            continue;

        Level := pIni^.getvalue(Keys[i], 'Level', 0).AsInteger ;

        j := pIni^.getvalue(Keys[i], 'ShowTime', 0).AsInteger ;
        
        //Apenas para parte de Atualização
        if (j <> Modo) or (j >= 2) then
           continue;

        //Pega mensagem no arquivo de update
       j := pIni^.getvalue(Keys[i], 'Modo', '0').AsInteger;
       case j of
          0: MessageBox(0, PCHAR( trim(pIni^.getvalue(Keys[i], 'Message', '0').AsString ) ), PCHAR( trim(pIni^.getvalue(Keys[i], 'Titulo', '0').AsString )), TraduzIcone( pIni^.getvalue(Keys[i], 'Icone', '0').AsString ) );
          1: Mensagem.Add(Mensagem.GetFreeName, trim(pIni^.getvalue(Keys[i], 'Message', '0').AsString) );
       end;

       //Avisa o arquivo de upload que a mensagem foi mostrada
       k := UpIni.getvalue( Keys[i], 'qnt', '0' ).AsInteger;
       UpIni.setvalue(Keys[i], 'ultimo', FormatDateTime('yyyy-mm-dd h:m:s', now));
       UpIni.setvalue(Keys[i], 'level', Level);
       UpIni.setvalue(Keys[i], 'qnt', k+1 );
       UpIni.save;
       UpIni.close;

       //Deleta a mensagem do arquivo de update
       pIni^.delete(keys[i]);
       pIni^.save;
    end;
end;

Function FixVersion(valor: AnsiString): integer;
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

Function SafeStrToInt(valor: AnsiString): integer;
begin
    result :=  FixVersion(valor);
end;

Function IsUpdated(version: integer; arquivo: AnsiString):boolean;
var vers1: integer;
begin
    result := false;
    vers1 := FixVersion(Joiner.ExtractString(arquivo));

    if (vers1 = 0) or (vers1 < version) then
       exit;

    result := true;
end;

type
  TFileToDown = packed record
      arquivo: AnsiString;
      size: integer;
  end;

Function IsVersionCorrect(arquivo: AnsiString; atual: integer): Boolean;
var
    Version: AnsiString;
    iVersion: integer;
begin
    result := false;
    //Pega versão do arquivo
    Version := Joiner.ExtractString(arquivo, Constante(0063, false));
    iVersion := FixVersion(Version);

    if (atual = 0) or (iVersion = 0) or (iVersion < atual) then
       exit;

    result := true;
end;

Function CheckFileState(arq: AnsiString; data: TDateTime): integer;
var fData: TDateTime; cData: TDateTime;
    Aplicacao : THandle;
    FPath: AnsiString;
    Info1,Info2,Info3: TFileTime;
    Estrutura : SystemTime;
begin
    FPath := ExtractFilePath( ParamStr(0) )+'Plugins\'+ arq;
    if FileExists( FPath ) then
    begin
        try
             fData := data;//StrToDateTime( trim(data) );
        except
          on e:exception do
             fData := MinDateTime;
        end;

        try            //formatdatetime('dd/mm/yyyy hh:mm:ss', cdata)+'  -  '+formatdatetime('dd/mm/yyyy hh:mm:ss', fdata)
            Aplicacao := FileOpen(FPath, fmOpenRead or fmShareDenyNone);
            if Aplicacao > 0 then
            begin
                GetFileTime( Aplicacao, @Info1, @Info2, @Info3);
                if FileTimeToSystemTime(Info3, Estrutura) then
                begin
                  cData := SystemTimeToDateTime(Estrutura);
                end;
            end;
        finally
          FileClose(Aplicacao);
        end;

        if cData < fData then
           result := 1
        else
           result := 2;
    end else
    begin
        result := 0;
    end;
end;

Function GetDownloadSize(arq: AnsiString): int64;
var Post: TPost;
    res: AnsiString;
    param: AnsiString;
begin
    Post := TPost.Create(false);
    //if(frmMatriz.frameOptions1.edtConPort.Value = 0) then
    //  Post.Port := 80
    //else
    Post.Port := 8080;//frmMatriz.frameOptions1.edtConPort.Value;
    Post.Host := AnsiString(Constante(0064, false)) + AnsiString(Constante(0067, false));
    Post.Clear;
    param := AnsiReplaceStr(arq,'\', '/');
    Post.Add( IntToStr(9123+PostIndex), UrlEncode(param));
    
    res := trim(Post.Execute);
    try
      result := StrToInt(res);
    except
      on e:exception do
         result := 0;
    end;
end;

(*
Function StartUpdate(Ini: TCMIni): boolean;
var Files: TStrings;
    Values: TStrings;
    i,j: integer;
    Arquivo: AnsiString;
    Key: AnsiString;
    Local: AnsiString;
    UpdateNow: Boolean;
    Flag: Boolean;
    IniVersion: integer;
    UpdateFile: AnsiString;
    UpdatePath: AnsiString;
    FileName: AnsiString;
    NewIni: TCMIni;
    Donwloads: array of TFileToDown;
    Messages: TStrings;
    DataS: AnsiString;
    DataD: TDateTime;
    NewKeys: TStrings;
begin
    try
       ChangeSplash( StrPas(Constante(0167, true)) );
       Joiner := TJoiner.create;
       Local := ExtractFilePath(ParamStr(0));
       UpdatePath := Local+StrPas(Constante(0076, false));
       UpdateNow := false;

       NewIni := TCMIni.create(GetHDCode, UpdatePath+StrPas(Constante(0083, false)));

       //Deu erro na atualização anterior?
       if(NewIni.ValueExists('Erro')) then
       begin
         if MessageBox(0, PChar(Constante(0169, true)), PChar(Constante(0171, true)),MB_ICONQUESTION+MB_YESNO) <> IDYES then
         begin
            MessageBox(0, PChar(Constante(0170, true)), PChar(Constante(0171, true)), MB_ICONINFORMATION+MB_OK);
            ExitProcess(0);
         end;
            
         NewKeys := TStringList.Create;
         try
             NewIni.ListKeys(NewKeys);
             for i := 0 to NewKeys.Count-1 do
             begin
                 try
                    NewIni.delete(NewKeys[i], 'Erro');
                 except
                   on e:exception do
                 end;
             end
         finally
           NewKeys.Free;
         end;
       end;


       Files := TStringList.Create;
       Values := TStringList.Create;
       Messages := TStringList.Create;

       //Ini eh de dados na memória, vindos do servidor; então precisamos salvar
       //no disco no arquivo NewIni
       Ini.ListKeys( Files );
       Donwloads := nil;
   
       //Verifica todos os arquivos do INI
       for i := 0 to Files.Count-1 do
       begin
           Flag := false;

           Key := Trim(Files.Strings[i]);

           //Salva as mensagens
           if( (Key[1] + Key[2]) = '01' ) then
           begin
               Ini.ListValues(Key, Messages);
               for j := 0 to Messages.Count-1 do
                   NewIni.setvalue( Key, Messages[j], Ini.getvalue(Key, Messages[j]).AsString);
           end;
       
           //Se nao for key de atualização continua
           if( (Key[1] + Key[2]) <> '02' ) then
              continue;

           //Nome do arquivo a ser atualizado
           Arquivo := ini.getvalue(Files.Strings[i], 'Arquivo', '').AsString;
           FileName := ExtractFileName(FixPath(Arquivo));

           //Pega versão atual
           IniVersion := Ini.getvalue(Files.Strings[i], 'Version', 0).AsInteger;

           //Diretorio + Arquivo - Lembrando que o arquivo pode ser colocado
           //em qualquer parte do PC, nao apenas no diretorio local, usando masks.
           UpdateFile := FixPath(Arquivo);  //Local+Arquivo;

           //Se nao existir o arquivo no local certo ou na pasta updates, ou sea versão
           //dos dois não estiver correta, seta o flag
           if (not FileExists(UpdateFile)) or ( (FileExists(UpdateFile)) and ((not IsVersionCorrect(UpdateFile, IniVersion)) or (IniVersion = -1))  ) then
           begin
              //Tipo de arquivo que é atualizado dependendo da data de criação (versao = -1)
              //Informar data em campo 'Data'
              if(IniVersion = -1) then
              begin
                  DataS := Ini.getvalue(Files.Strings[i], 'Data', 0).AsString;
                  if Length(DataS) < 10 then
                     continue;
                  try
                     DataD := StrToDateTime(DataS);

                     if not FileExists(UpdateFile) then
                        Flag := true
                     else
                     begin
                       //Está desatualizado 
                       if CheckFileState(UpdateFile, DataD) = 1 then
                       begin
                          flag := true
                       end else
                       begin
                         //ini.delete
                         continue;
                         
                       end;

                     end;
                  except
                    on e:exception do
                      continue;
                  end;
              end else
              begin
                  //O arquivo existe na pasta de atualizações (ja foi feito o download)
                  if FileExists(UpdatePath+FixPath(Arquivo, true) ) then
                  begin
                     //Verifica se o Ini de update está completo, apesar de o arquivo estar ae...
                     //Mas tem doido pra mexe em tudo neh..
                     if not NewIni.ValueExists(Arquivo)  then
                     begin
                        //Ixi.. falei! Então agora vamos apenas restaurar o INI de update
                        Ini.ListValues(Files.Strings[i], Values);
                        for j := 0 to Values.Count-1 do
                        begin
                            NewIni.setvalue(Files.Strings[i], Values.Strings[j], Ini.getvalue(Files.Strings[i], Values.Strings[j]).AsString );
                            NewIni.save;
                        end;
                     end;
                     //Indica que o arquivo já existe
                     UpdateFile := UpdatePath+FixPath(Arquivo, true);
                     UpdateNow := true;
                  end else
                     Flag := true;
              end;
           end;

           if not Flag then
           begin
              //Se a versão for inválida ou antiga seta o flag
              if not IsVersionCorrect(UpdateFile, IniVersion) then
                 flag := true;
           end;

           //Donwloads := nil;
           //Se o Flag estiver setado grava o arquivo ini temporário
           if flag then
           begin
               UpdateNow := true;
               Ini.ListValues(Files.Strings[i], Values);

               for j := 0 to Values.Count-1 do
               begin
                  NewIni.setvalue( Files.Strings[i], Values.Strings[j], Ini.getvalue(Files.Strings[i], Values.Strings[j], '').AsString );
                  NewIni.save;
               end;

               SetLength(Donwloads, Length(Donwloads)+1);
               Donwloads[High(Donwloads)].arquivo := Arquivo;
               Donwloads[High(Donwloads)].size := {SafeStrToInt( Ini.getvalue(Files.Strings[i], 'Size', '0').AsString );}      GetDownloadSize(AnsiReplaceStr(FixPath(Arquivo, true), '\', '/'));//GetDownloadSize(Arquivo);
               pTotalAtualn := pTotalAtualn + Donwloads[High(Donwloads)].size;
           end;

       end;

       NewIni.save;

       //Tudo ok agora.. Vamos baixar os arquivos
       for i := 0 to length(Donwloads)-1 do
       begin
            Arquivo := AnsiReplaceStr(FixPath(Donwloads[i].arquivo, true), '\', '/');
            FileName := ExtractFileName(FixPath(Arquivo));
            //Cria arvore do diretório se não existir
            ForceDirectories( ExtractFilePath(UpdatePath+FixPath(Donwloads[i].arquivo, true)) );
            //Faz o download
            //j := frmMatriz.DownloadURL_NOCache(Constante(0064, false)+'/cmx/CMUpdates/'+Arquivo, Donwloads[i].size, UpdatePath+FixPath(Donwloads[i].arquivo, true));
            //frmMatriz.DownloadUpdateFile(Constante(0064, false)+'/cmx/CMUpdates/'+Arquivo, UpdatePath+FixPath(Donwloads[i].arquivo, true));
            frmMatriz.DownloadUpdateFile(Arquivo, UpdatePath+FixPath(Donwloads[i].arquivo, true));
       end;

       pTotalAtualn := 0;
       Files.Free;
       Values.Free;
       Messages.Free;


      if UpdateNow then
         //ExitCM;
         ReloadCM;
    except
      on e:exception do
      begin
        ChangeSplash( StrPas(Constante(0166, true)) );
        Delay2(3000);
        ExitCM;
      end;
    end;
end;
*)

end.
