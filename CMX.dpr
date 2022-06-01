program CMX;

// {$APPTYPE CONSOLE}
{$R 'icone.res' 'icone.rc'}

uses
    SysUtils,
    iniFiles,
    windows;

Function GetRandomName(Size: integer): AnsiString;
var
    i, k, l: integer;
    v: boolean;
const
    Consoantes = 'bcdfghjklmnpqrstvwxz';
    Vogais = 'aeiouy';
begin
    result := '';
    v := false;
    k := random(6) + 5;
    for i := 1 to Size do
    begin
        randomize;
        v := not v;
        l := random(3);
        if v then
            result := result + Consoantes[random(length(Consoantes) - 1) + 1]
        else
            result := result + Vogais[random(length(Vogais) - 1) + 1]
    end;
end;

function HexToInt(s: AnsiString): Longword;
var
    b: Byte;
    c: AnsiChar;
begin
    result := 0;
    s := UpperCase(s);
    for b := 1 to length(s) do
    begin
        result := result * 16;
        c := s[b];
        case c of
            '0' .. '9':
                Inc(result, Ord(c) - Ord('0'));
            'A' .. 'F':
                Inc(result, Ord(c) - Ord('A') + 10);
        else
            raise EConvertError.Create('No Hex-Number');
        end;
    end;
end;

Function HexToString(valor: AnsiString): AnsiString;
var
    i: integer;
    s: AnsiString;
begin
    result := '';
    s := '';
    if (length(valor) mod 2) > 0 then
        valor := '0' + valor;

    for i := 1 to length(valor) do
    begin
        s := s + valor[i];
        if length(s) = 2 then
        begin
            result := result + AnsiChar(HexToInt(s));
            s := '';
        end;
    end;
end;

Function CriarProcesso(Path: AnsiString; Parametro: PAnsiChar; showtype: cardinal): boolean;
var
    StartupInfo: STARTUPINFOA;
    ProcessInfo: PROCESS_INFORMATION;
    Diretorio: PAnsiChar;
begin
    // VMProtectBegin('cps');
    Diretorio := PAnsiChar(Path);
    //
    result := true;
    FillChar(StartupInfo, sizeof(StartupInfo), #0);
    FillChar(ProcessInfo, sizeof(ProcessInfo), #0);
    StartupInfo.cb := sizeof(StartupInfo);
    StartupInfo.cb := sizeof(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOWNORMAL;
    if not CreateProcessA(Diretorio, Parametro, nil, // pointer to process security attributes
      nil, // pointer to thread security attributes
      false, // handle inheritance flag
      NORMAL_PRIORITY_CLASS, // NORMAL_PRIORITY_CLASS,
      nil, // pointer to new environment block
      nil, // pointer to current directory name
      StartupInfo, // pointer to STARTUPINFO
      ProcessInfo) then
    begin
        result := false;
    end;
    closehandle(ProcessInfo.hProcess);
    closehandle(ProcessInfo.hThread);
    // VMProtectEnd;
end;

function FileSearch: boolean;
var
    Rec: TSearchRec;
    Path: AnsiString;
begin
    Path := ExtractFilePath(ParamStr(0));

    if FileExists(Path + 'matriz.exe') then
    begin
        result := true;
        exit;
    end;

    if FindFirst(Path + '*.exe', faAnyFile - faDirectory, Rec) = 0 then
    begin
        try
            repeat
                if LowerCase(Rec.Name) <> 'cmx.exe' then
                begin
                    if (RenameFile(Path + Rec.Name, Path + 'matriz.exe')) then
                    begin
                        result := true;
                        exit;
                    end;
                end;
            until FindNext(Rec) <> 0;
        finally
            SysUtils.FindClose(Rec);
        end;
    end;
end;

var
    exeName, exeStr, nomeMatriz, dirIni: AnsiString;
    iniConfig: TIniFile;
    encontrado: boolean;
    Parametro: AnsiString;

begin
    nomeMatriz := ExtractFilePath(ParamStr(0)) + 'matriz.exe';
    if not FileExists(nomeMatriz) then
    begin
        dirIni := ExtractFilePath(ParamStr(0)) + 'configs.ini';

        iniConfig := TIniFile.Create(dirIni);
        exeStr := iniConfig.ReadString('configs', 'des', '');
        exeName := HexToString(exeStr);

        encontrado := true;
        if (length(exeName) > 0) and (FileExists(ExtractFilePath(ParamStr(0)) + exeName + '.exe')) then
        begin
            if not(RenameFile(ExtractFilePath(ParamStr(0)) + exeName + '.exe', nomeMatriz)) then
                if not FileExists(nomeMatriz) then
                begin
                    encontrado := false;
                end;
        end
        else
        begin
            encontrado := false;
        end;

        if not encontrado then
        begin
            if not FileSearch then
            begin
                MessageBoxA(0, 'Matriz não encontrada! Reinstale o CMX', '', 0);
            end;
        end;
    end;

    Parametro := '';
    if ParamCount > 0 then
        Parametro := ParamStr(1);
    if not CriarProcesso(nomeMatriz, PAnsiChar(AnsiString(Parametro)), 0) then
        MessageBoxA(0, 'Falha ao iniciar o CMX!', '', 0)

end.
