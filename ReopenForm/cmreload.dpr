{ KOL MCK } // Do not remove this line!
program cmreload;

uses
  windows, sysutils, TlHelp32, skyio;

  {$APPTYPE CONSOLE}
{$E dll}

//{$R *.res}

procedure Delay(dwMilliseconds: Longint);
var
	iStart, iStop: DWORD;
begin
	iStart := GetTickCount;
	repeat
		iStop := GetTickCount;
		//Application.ProcessMessages;
		Sleep(10); // addition from Christian Scheffler to avoid high CPU last
	until (iStop - iStart) >= dwMilliseconds;
end;

Procedure WriteLog(valor: string);
var
	t: TextFile;
	Arquivo: String;
begin
	Arquivo := ExtractFilePath(ParamStr(0)) + 'UpdateLog.txt';

	if not FileExists(Arquivo) then
		CreateFile(PCHAR(Arquivo), GENERIC_ALL, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);

	AssignFile(t, Arquivo);
	if not FileExists(Arquivo) then
		Rewrite(t);

	Writeln(t, valor);
	CloseFile(t);
end;


Function SafeStrToInt(valor: string): integer;
begin
	result := 0;
	if trim(valor) = '' then
		exit;

	try
		result := StrToInt(valor);
	except
		on e: exception do
		begin
			result := 0;
		end;
	end;
end;

Function CMPChar(s: AnsiString): PWideChar;
begin
	result := AllocMem(length(s) + 1);
	StringToWideChar(s, result, length(s) + 2);
end;

Function GetProcessIdFromProcname(procName: AnsiString): cardinal;
var
	pe: TProcessEntry32;
	thSnapshot: THANDLE;
	retval, ProcFound: boolean;
	total, j, n: integer;
	processosSistema: array [1 .. 10] of cardinal;
	filhoDoSistema: boolean;
	cbRet: cardinal;
	Handle: cardinal;
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

Function CriaProcesso(Path: AnsiString; showtype: cardinal): boolean;
var
	StartupInfo: TStartupInfo;
	ProcessInfo: TProcessInformation;
	Diretorio: String;
begin

	try
		Diretorio := Path;
		// {$I VMProtectBegin.inc}
		result := true;
		FillChar(StartupInfo, sizeof(StartupInfo), #0);
		StartupInfo.cb := sizeof(StartupInfo);
		StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
		StartupInfo.wShowWindow := SW_SHOWNORMAL;
		if not CreateProcess(nil, PCHAR(Diretorio), // pointer to command line AnsiString
			nil, // pointer to process security attributes
			nil, // pointer to thread security attributes
			false, // handle inheritance flag
			showtype or // creation flags
				HIGH_PRIORITY_CLASS, // NORMAL_PRIORITY_CLASS,
			nil, // pointer to new environment block
			nil, // pointer to current directory name
			StartupInfo, // pointer to STARTUPINFO
			ProcessInfo) then
		begin
			result := false;
		end;
	except
		on e: exception do
		begin

		end;
	end;

	// {$I VMProtectEnd.inc}
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

procedure IniciarRefresh;
var
	searcher: TSearcher;
	UpdatePath, Local, destino: AnsiString;
	i, id: integer;
	tempo: cardinal;
    processo: THANDLE;
begin
	try
	if (ParamStr(2) <> '') then
	begin
		if (FileExists(ParamStr(2))) then
		begin
			id := GetProcessIdFromProcname(ParamStr(2));
			if id > 0 then
			begin
				processo := OpenProcess(PROCESS_TERMINATE, false, id);
				TerminateProcess(processo, 0);
				closehandle(processo);
			end;
		end;
	end;
    except
    	on e: exception do
        begin
        end;
    end;

	try
//		Panel1.Caption := 'Aguarde...';
		OutputDebugStringA('Reloading...');
		searcher := TSearcher.Create;
		Local := ExtractFilePath(ParamStr(0));
		UpdatePath := Local + 'Updates\';

		searcher.Results.Clear;
		searcher.Dir := UpdatePath;
		searcher.Extensions.Clear;
		searcher.Extensions.Add('*.*');
		searcher.StartSearch;

//		ProgressBar1.MaxValue := searcher.Results.Count;

		for i := 0 to searcher.Results.Count - 1 do
		begin
			if LowerCase(ExtractFileName(searcher.Results.ResultValue[i].Path)) = LowerCase(ExtractFileName(ParamStr(0))) then
				continue;

			destino := ExtractFilePath(searcher.Results.ResultValue[i].Path);
			if (length(destino) >= length(UpdatePath)) then
			begin
				destino := Local + copy(destino, length(UpdatePath) + 1, length(destino));
				OutputDebugStringA(PAnsiChar(AnsiString('Destino: ' + destino)));
			end;

			ForceDirectories(destino);

			tempo := GetTickCount;

			try
				OutputDebugStringA(PAnsiChar(AnsiString('Movendo de ' + searcher.Results.ResultValue[i].Path + ' para ' + destino + ExtractFileName(searcher.Results.ResultValue[i].Path))));
				while not MoveFileEx(PCHAR(CMPChar(searcher.Results.ResultValue[i].Path)), PCHAR(CMPChar(destino + ExtractFileName(searcher.Results.ResultValue[i].Path))), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH or MOVEFILE_COPY_ALLOWED) do
				begin
					Delay(200);
					if (GetTickCount - tempo) > 20000 then
						break;
				end;
			except
				on e: exception do
				begin
					OutputDebugStringA('Falha ao mover o arquivo...');
					// WriteLog(e.ToString);
				end;
			end;

//			ProgressBar1.Progress := i + 1;
		end;

//		ProgressBar1.Progress := ProgressBar1.MaxValue;
		// WinExec(PAnsiChar(ExtractFilePath(ParamStr(0))+'Matriz.exe'), SW_SHOWNORMAL);

		tempo := GetTickCount;
		while not CriaProcesso(ExtractFilePath(ParamStr(0)) + 'CMX.exe ' + ParamStr(1), 0) do
		begin
			OutputDebugStringA('Reiniciando Matriz...');
			Delay(200);
			if (GetTickCount - tempo) > 6000 then
				break;
		end;
		ExitCM;
	except
		on e: exception do
		begin
			// WriteLog(e.ToString);
		end;
	end;
end;

begin
  //Application.Initialize;
  //Application.CreateForm(TForm1, Form1);
  //Application.Run;
end.


