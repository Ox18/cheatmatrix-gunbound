program CMX;
{$R 'icone.res' 'icone.rc'}

uses
	windows,
	sysutils, TlHelp32;//,
	//vmProtectSDK;

// {$R *.res}

Function GetRandomName(Size: integer): AnsiString;
var
	i, k, l: integer;
	v: boolean;
	Consoantes, Vogais: AnsiString;
begin
	Consoantes := 'bcdfghjklmnpqrstvwxz';
	Vogais := 'aeiouy';
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

Function CriarProcesso(Path: AnsiString; Parametro: PAnsiChar; showtype: cardinal): boolean;
var
	StartupInfo: STARTUPINFOA;
	ProcessInfo: PROCESS_INFORMATION;
	Diretorio: PAnsiChar;
begin
	//VMProtectBegin('cps');
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
	//VMProtectEnd;
end;

Function GetProcessIdFromProcname(procName: AnsiString): cardinal;
var
	pe: TProcessEntry32;
	thSnapshot: THandle;
	retval, ProcFound: boolean;
	total, j, n: integer;
	processosSistema: array [1 .. 10] of cardinal;
	filhoDoSistema: boolean;
	cbRet: cardinal;
	handle: cardinal;
begin
	try
		result := 0;
		total := 0;
		thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
		try
			if (thSnapshot = INVALID_HANDLE_VALUE) then
			begin
				result := 0;
				exit;
			end;
			pe.dwSize := sizeof(PROCESSENTRY32);
			retval := Process32First(thSnapshot, pe);
			n := 0;
			filhoDoSistema := false;
			while (retval) do
			begin
				if (LowerCase(trim(pe.szExeFile)) = LowerCase(trim(procName))) then
				begin
					ProcFound := true;
					inc(total);
					result := pe.th32ProcessID;
					exit;
				end;
				retval := Process32Next(thSnapshot, pe);
				pe.dwSize := sizeof(PROCESSENTRY32);
			end;
		finally
			closehandle(thSnapshot);
		end;
	except
		on e: exception do
		begin
			OutputDebugStringA('Erro 9292');
		end;
	end;
end;

procedure Deletefiles(APath, AFileSpec: string);
var
	lSearchRec: TSearchRec;
	lFind, id: integer;
	lPath: string;
    Processo: thandle;
begin
	lPath := IncludeTrailingPathDelimiter(APath);

	lFind := FindFirst(lPath + AFileSpec, faAnyFile, lSearchRec);
	while lFind = 0 do
	begin
		if pos('.exe', lSearchRec.Name) > 0 then
		begin
			id := GetProcessIdFromProcname(lSearchRec.Name);
			if (id > 0) then
			begin
				Processo := OpenProcess(PROCESS_TERMINATE, false, id);
				TerminateProcess(Processo, 0);
				closehandle(Processo);
			end;
		end;
		DeleteFile(lPath + lSearchRec.Name);
		lFind := sysutils.FindNext(lSearchRec);
	end;
	FindClose(lSearchRec);
end;

Procedure ExitCM;
var
    SelfProc: THandle;
begin
    try
        SelfProc := OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId);
        TerminateProcess(SelfProc, 0);
        closehandle(SelfProc);
    except
        on e: exception do
        begin
        end;
    end;
end;

var
	inicializado: integer = 0;
	Mutex: THandle;
	teste: AnsiString;
	arquivos: array [1 .. 5] of AnsiString;
	copiados: array [1 .. 5] of boolean;
	Path, docs: AnsiString;
	i: integer;

begin
	//VMProtectBegin('cpip');
	//Mutex := OpenMutex(MUTEX_ALL_ACCESS, false, 'mdlcmmtx');
	//if (Mutex = 0) then
	begin
		docs := ExtractFilePath(ParamStr(0)) + 'documentos\';
		ForceDirectories(docs);

		Path := ExtractFilePath(ParamStr(0));
		Deletefiles(docs, '*.exe');
        Deletefiles(docs, '*.*');

		randomize;
		arquivos[1] := GetRandomName(random(5) + 3) + '.dll';
		arquivos[2] := GetRandomName(random(5) + 3) + '.exe';
		arquivos[3] := GetRandomName(random(5) + 3) + '.dll';
		arquivos[4] := GetRandomName(random(5) + 3) + '.dll';

		copiados[1] := CopyFileA(PAnsiChar(Path + 'cma.dll'), PAnsiChar(docs + arquivos[1]), true);
		copiados[2] := CopyFileA(PAnsiChar(Path + 'cmb.dll'), PAnsiChar(docs + arquivos[2]), true);
		copiados[3] := CopyFileA(PAnsiChar(Path + 'cmc.dll'), PAnsiChar(docs + arquivos[3]), true);
		copiados[4] := CopyFileA(PAnsiChar(Path + 'cmm.dll'), PAnsiChar(docs + arquivos[4]), true);

		for i := 1 to 4 do
		begin
			if not copiados[i] then
			begin
				MessageBoxA(0, 'Arquivo em uso!', '', 0);
				ExitCM;
			end;
		end;

		if not CriarProcesso(AnsiString(docs + arquivos[2]), PAnsiChar('||' + Path + '|' + arquivos[1] + '|msvcrt36.dll|' + arquivos[4] + '|' + arquivos[3] + '|'), 0) then
			MessageBoxA(0, 'Falha ao iniciar o CMX!', '', 0)
			// else
			// OutputDebugStringA('Processo criado!');

	end;
	//else
	//	MessageBoxA(0, 'O processo já está aberto!', '', 0);
	//VMProtectEnd;

end.
