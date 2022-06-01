unit Inicializador;

interface

// {$DEFINE NO_HASH_MATRIZ}

uses
	// -- Delphi --------------------------------------------------
	Windows, ExtCtrls, ImgList, Controls, StdCtrls, ComCtrls,
	sysutils, Classes, Forms, strutils, VMProtectSDK, inifiles,
	cmini, functions, constantes, SplashScreen,
	SkySql, Utils, Encription, SkyIO, base64, TlHelp32;

type
	PPluginMemoryDataRecord = ^TPluginMemoryDataRecord;

	TPluginMemoryDataRecord = record
		PluginID: cardinal;
		PacketID: integer;
		Ponteiro: cardinal;
		Offset: cardinal;
		Range: cardinal;
		Size: cardinal;
		Endereco: cardinal;
		ValorEx: TByteArray;
	end;

	TFileSubplugin = record
		PluginID: DWORD;
		idioma: byte;
		case integer of
			0:
				(Valor: PAnsiString);
			1:
				(ValorEx: TByteArray);
	end;

	PFileSubplugin = ^TFileSubplugin;

	TPacote = record
		len: WORD;
		tipo: WORD;
		case integer of
			0:
				(pBuffer: PByte);
			1:
				(buffer: TByteArray);
	end;

	PPacote = ^TPacote;

	TProcesso = record
		Nome: String;
		Indice: integer;
		Injetado: boolean;
	end;

	TReceptor = Procedure(Valor: PPacote);
	TTipoPacote = (TP_DISCONNECT, TP_MENSAGEM, TP_SUBPLUGIN, TP_PONTEIRO, TP_COMANDO, TP_ATALHO, TP_IDIOMA, TP_PROCESSO);

const
	versaoCMX = 4000;

var
	UpIni: TCMIni;
	IniAutenticacao: TCMIni;
	IniKeys: TStrings;
	arrayProcessos: array of TProcesso;
	diretorioPlugin: AnsiString; // , pathPlugin
	pDiretorioPlugin: PAnsiChar;
	Receptor: TReceptor = nil;

	loginUsuario: PAnsiChar;
	stringTempo: AnsiString;
	// clienteSocket: TIdHTTPMethod
	// plugins: array of TFileSubplugin;

function inicializar: integer;
// procedure IniciarMonitor;
function ativarCMX(usuario: AnsiString; senha: AnsiString; codigo: AnsiString): integer;
Function GetProcessIdFromProcname(procName: AnsiString): cardinal;
procedure Split(const Delimiter: char; Input: string; const Strings: TStrings);
Procedure enviar(tipo: TTipoPacote; buffer: PByte; len: integer);
Function CriarProcesso(Path: AnsiString; Parametro: PAnsiChar; showtype: cardinal): boolean;

implementation

Function InjectDLL(id: DWORD; Dll: AnsiString): boolean;
var
	processoH: THandle;
	TempHandle: THandle;
	AllocatedRegion: Pointer;
	Empty: DWORD;
	NumberOfBytesWritten: cardinal;
	RemoteString, LoadLibAddy: Pointer;
	nomeDll: PAnsiString;
begin
	try
		nomeDll := PAnsiString(Dll);
		if (id = 0) then
		begin
			result := false;
			exit;
		end;
		processoH := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, false, id);
		if (processoH > 0) then
		begin
			LoadLibAddy := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
			RemoteString := VirtualAllocEx(processoH, nil, length(Dll), MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
			Empty := 0;
			WriteProcessMemory(processoH, RemoteString, nomeDll, length(Dll), Empty);
			CreateRemoteThread(processoH, nil, 0, LoadLibAddy, RemoteString, 0, Empty);
			closehandle(processoH);
			result := true;
		end;
	except
		on e: exception do
		begin
			OutputDebugStringA('Erro 9293');
		end;
	end;
end;

procedure Split(const Delimiter: char; Input: string; const Strings: TStrings);
begin
	try
		Assert(Assigned(Strings));
		Strings.Clear;
		Strings.Delimiter := Delimiter;
		Strings.DelimitedText := Input;
	except
		on e: exception do
		begin
			OutputDebugStringA('Erro 9294');
		end;
	end;
end;

Procedure enviar(tipo: TTipoPacote; buffer: PByte; len: integer);
var
	bloco: TPacote;
	// bytes: TByteArray; // array [0..2048] of byte;
begin
	if @Receptor <> nil then
	begin
		try
			bloco.len := len;
			bloco.tipo := WORD(tipo);
			// SetLength(bytes, len+4);

			ZeroMemory(@bloco.buffer[0], len + 5);
			// CopyMemory(@bloco.buffer[0], @bloco.len, 2);
			// CopyMemory(@bloco.buffer[2], @bloco.tipo, 2);
			CopyMemory(@bloco.buffer[0], @buffer[0], len);
			// bloco.buffer := bytes;

			Receptor(@bloco);
		except
			on e: exception do
			begin
				OutputDebugStringA('Erro 9295');
			end;
		end;
	end;
end;

procedure enviarPacotes;
type
	TM3 = function(dirBase: PAnsiChar): PAnsiChar; stdcall;
var
	IniFile, entradasRegistro, campos: TStringList;
	Nome, s, s1, Hack, processo, indiceProcessoS, registro: AnsiString;
	i, w, Indice, k, n, l, j, len, Size, enviados, quantidade, indiceProcesso: integer;
	codigo: cardinal;
	RegisteredUser: boolean;
	iniConfig: TIniFile;
	pacote: PPluginMemoryDataRecord;
	plugin: TFileSubplugin;
	pacoteProcesso: TFileSubplugin;
	hRes: HRSRC;
	instancia: HMODULE;
	fname: PAnsiChar;
	f2: TM3;
	nomeMatriz: AnsiString;
	nomeBase: PAnsiChar;
begin
VMProtectBegin('evpc');
	stringTempo := '';
	try
		nomeMatriz := (AnsiString(diretorioPlugin + constante(0008, false) + constante(0103, false)));
		iniConfig := TIniFile.create(diretorioPlugin + (constante(0066, false)));
		entradasRegistro := TStringList.create;
		IniFile := TStringList.create;
		IniAutenticacao.ListValues(IntToStr(5115), IniFile);
		for i := 0 to IniFile.Count - 1 do
		begin
			Nome := trim(IniFile[i]);
			registro := IniAutenticacao.getvalue(IntToStr(5115), Nome, '').AsString;
			entradasRegistro.Values[Nome] := registro;
		end;

		for i := 0 to length(OutrosValores) - 1 do
		begin
			stringTempo := stringTempo + OutrosValores[i];
			stringTempo := stringTempo + #30;
		end;

		stringTempo := stringTempo + ' ' + #30;

		for i := 0 to length(NomesPacotes) - 1 do
		begin
			stringTempo := stringTempo + NomesPacotes[i] + ': ' + TempoPacotes[i] + ' ' + constante(0091, false);
			if (i < length(NomesPacotes) - 1) then
				stringTempo := stringTempo + #30;
		end;

		pacoteProcesso.PluginID := 0;
		pacoteProcesso.idioma := 1;
		CopyMemory(@pacoteProcesso.ValorEx[0], PAnsiChar(nomeMatriz), length(nomeMatriz) + 1);
		enviar(TP_PROCESSO, PByte(@pacoteProcesso), 8 + length(nomeMatriz) + 1);

		IniFile.Clear;
		IniAutenticacao.ListValues(IntToStr(6101), IniFile);
		for i := 0 to IniFile.Count - 1 do
		begin
			codigo := StrToInt(trim(IniFile[i]));
			s := IniAutenticacao.getvalue(IntToStr(6101), IntToStr(codigo), '').AsString;
			campos := TStringList.create;
			Split('|', s, campos);
			if (campos.Count < 5) then
				continue;

			Hack := campos.Strings[0];
			quantidade := StrToInt(trim('0' + campos.Strings[4]));
			processo := campos.Strings[1];

			indiceProcessoS := campos.Strings[2];
			indiceProcesso := StrToInt(indiceProcessoS);
			if (indiceProcesso = 0) then
				indiceProcesso := 1;

			pacoteProcesso.PluginID := codigo;
			pacoteProcesso.idioma := 0;
			PByte(@pacoteProcesso.ValorEx[0])^ := byte(indiceProcesso);
			CopyMemory(@pacoteProcesso.ValorEx[1], PAnsiChar(processo), length(processo) + 1);
			enviar(TP_PROCESSO, PByte(@pacoteProcesso), 8 + length(processo) + 1);

			SetLength(PluginsLiberados, length(PluginsLiberados) + 1);
			PluginsLiberados[length(PluginsLiberados) - 1] := campos.Strings[3];

			if FileExists(diretorioPlugin + PluginsLiberados[length(PluginsLiberados) - 1]) then
			begin
				instancia := LoadLibraryA(PAnsiChar(AnsiString(diretorioPlugin + PluginsLiberados[length(PluginsLiberados) - 1])));
				@f2 := GetProcAddress(instancia, 'f2');
				if @f2 <> nil then
				begin
					fname := f2(pDiretorioPlugin);
					if fname <> nil then
					begin

						plugin.PluginID := codigo;
						plugin.idioma := integer(idioma);
						CopyMemory(@plugin.ValorEx[0], fname, StrLen(fname) + 1);
						enviar(TP_SUBPLUGIN, PByte(@plugin), 8 + StrLen(fname) + 1);
						// OutputDebugStringA(PAnsiChar(AnsiString('Plugin enviado: ' + AnsiString(fname))));
					end;
				end;
			end;

			if (length(processo) > 0) then
			begin
				iniConfig.WriteString((constante(0041, false)), IntToStr(length(arrayProcessos)), md5(StrLower(PAnsiChar(AnsiString(processo + '-')))));
			end;

			for w := 0 to quantidade - 1 do
			begin
				Indice := (5 + (w * 6));
				if (5 + ((w + 1) * 6)) <= campos.Count then
				begin
					Size := StrToInt(trim('0' + campos.Strings[Indice + 4]));
					if (Size <= 4) then
						len := 32
					else
						len := (32 + Size - 4);

					pacote := GetMemory(len);
					pacote^.PluginID := codigo;
					pacote^.PacketID := StrToInt(trim('0' + campos.Strings[Indice + 5]));
					pacote^.Ponteiro := HexToInt('0' + campos.Strings[Indice + 0]);
					pacote^.Offset := HexToInt('0' + campos.Strings[Indice + 1]);
					pacote^.Endereco := 0;

					s := campos.Strings[Indice + 2];
					if (length(s) mod 2) > 0 then
						s := '0' + s;

					k := (length(s) div 2);

					// pacote^.Valor := new(PAnsiChar);
					// pacote^.Valor := GetMemory(k + 1);
					// n := 0;
					ZeroMemory(@pacote^.ValorEx[0], k + 1);

					l := 0;
					for j := k downto 1 do
					begin
						inc(l);
						s1 := Copy(s, 1, 2);
						s := Copy(s, 3, length(s));
						n := HexToInt(s1);
						pacote^.ValorEx[l - 1] := byte(n);
					end;

					pacote^.Range := StrToInt(trim('0' + campos.Strings[Indice + 3]));
					pacote^.Size := StrToInt(trim('0' + campos.Strings[Indice + 4]));

					// OutputDebugStringA(PAnsiChar(AnsiString('Ponteiro ' + IntToStr(pacote^.PacketID) + ' enviado...')));
					enviar(TP_PONTEIRO, PByte(pacote), len);
					Delay(5);
				end;
			end;
			// listaGrupoItemsMemoria.Add(HackPack^);
		end;
		IniFile.Free;
	except
		on e: exception do
		begin
			OutputDebugStringA('Erro 9290');
		end;
	end;
VMProtectEnd;
end;

Function CriarProcesso(Path: AnsiString; Parametro: PAnsiChar; showtype: cardinal): boolean;
var
	StartupInfo: STARTUPINFOA;
	ProcessInfo: PROCESS_INFORMATION;
	Diretorio: PAnsiChar;
begin
	Diretorio := PAnsiChar(Path);
	//
	result := true;
	FillChar(StartupInfo, sizeof(StartupInfo), #0);
	FillChar(ProcessInfo, sizeof(ProcessInfo), #0);
	StartupInfo.cb := sizeof(StartupInfo);
	StartupInfo.cb := sizeof(StartupInfo);
	StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
	StartupInfo.wShowWindow := SW_SHOWNORMAL;
	if not CreateProcessA(Diretorio, Parametro,
		// pointer to command line AnsiString
		nil, // pointer to process security attributes
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

	// InjectDLL(ProcessInfo.dwProcessId, ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll');
	// ResumeThread(ProcessInfo.hThread);
	// WaitForSingleObject( ProcessInfo.hProcess, INFINITE );
	closehandle(ProcessInfo.hProcess);
	closehandle(ProcessInfo.hThread);
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

procedure Delay(dwMilliseconds: Longint);
var
	iStart, iStop: DWORD;
begin
	iStart := GetTickCount;
	repeat
		iStop := GetTickCount;
		Application.ProcessMessages;
		Sleep(1); // addition from Christian Scheffler to avoid high CPU last
	until (iStop - iStart) >= dwMilliseconds;
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

Function SafeStrToInt(Valor: AnsiString): integer;
var
	i: integer;
	s: AnsiString;
begin
	Valor := trim(Valor);
	s := '';

	for i := 1 to length(Valor) do
	begin
		if Pos(Valor[i], '0123456789') > 0 then
			s := s + Valor[i];
	end;
	Valor := s;

	result := 0;
	try
		result := StrToInt(Valor);
	except
		on e: exception do
		begin
			result := 0;
			exit;
		end;
	end;
end;

function verificaErro(PostResult: AnsiString): integer;
var
	tmppos: integer;
begin
	tmppos := Pos(md5(IntToStr(1000)), PostResult);
	result := 0;

	if tmppos > 0 then
	begin
		if tmppos > 2 then
		begin
			result := SafeStrToInt(Copy(PostResult, 1, 2));
		end;

		PostResult := trim(Copy(PostResult, tmppos + 32, length(PostResult)));
		if PostResult = '' then
			PostResult := (constante(0044, false))
		else
			PostResult := Base64Decode(PostResult);

		// ShowMessage(PostResult);
		if result = 1 then
		begin
			ChangeSplash(PostResult);
			splash.visible := false;
			telaAtivacao;
			if splash.ShowModal = 1 then
				// result := 0;
				// splash.Show;
				// inicializar;
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
	end
	else if trim(PostResult) = '' then
	begin
		ChangeSplash(constante(0009, false));
		Delay(3000);
		ExitCM;
	end;
end;

function ativarCMX(usuario: AnsiString; senha: AnsiString; codigo: AnsiString): integer;
var
	Post: TPost;
	HD: AnsiString;
	Ini: TCMIni;
	s: AnsiString;
	PostResult: AnsiString;
	i: integer;
	Keys: TStrings;
	tmppos: integer;
begin
	VMProtectBegin('atv');
	Randomize;
	result := 0;
	HD := GetHDCode;
	Ini := TCMIni.create(HD, '');

	Post := TPost.create(false);
	Post.Port := 8080;
	Post.Host := trim(AnsiString(constante(0021, false)));
	Post.Clear;
	s := MaxEncript(HD);
	Post.Add(IntToStr(9234 + PostIndex), codigo);
	Post.Add(IntToStr(9235 + PostIndex), s);
	Post.Add(IntToStr(9236 + PostIndex), usuario);
	Post.Add(IntToStr(9237 + PostIndex), senha);

	try
		PostResult := trim(Post.Execute);
		PostResult := AnsiReplaceStr(PostResult, #$D#$A, '');
		if verificaErro(PostResult) = 10 then
		begin
			telaAtivacao;
			Delay(2000);
			splash.ModalResult := 1;
			// result := inicializar;
			// if (result) then
			// begin
			// Application.CreateForm(TfrmMatriz, frmMatriz);
			// Application.Run;
			// IniciarMonitor;
			// enviarPacotes;
			// end;
		end;
	except
		on e: exception do
		begin
			try
				ChangeSplash((constante(0092, false)) + ' (code' + IntToStr(237) + ')');
			except
				on e: exception do
				begin
				end;
			end;
			Delay(1000);
		end;
	end;
	// splash.ModalResult := 1;

	{ Ini.LoadFromText(PostResult);
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
	VMProtectEnd;
end;

Function GetDownloadSize(arq: AnsiString): int64;
var
	response: TMemoryStream;
	res: AnsiString;
begin
	// Splash.
end;

Procedure ReloadCM;
var
	StartInfo: TStartupInfo;
	ProcInfo: TProcessInformation;
	tempo: cardinal;
    nomeParametro: AnsiString;
begin
	if (SafeStrToInt('0' + ParamStr(1)) >= 1) then
	begin
		ChangeSplash((constante(0080, false)));
		Delay(3000);
		ExitCM;
	end;

	tempo := GetTickCount;
    nomeParametro := AnsiString(diretorioPlugin + updatefile + ' ' + IntToStr(SafeStrToInt('0' + ParamStr(1)) + 1) + ' ' + AnsiString(StringReplace(ParamStr(0),' ','|',[rfReplaceAll, rfIgnoreCase])));
	//OutputDebugStringA('teste');
    //OutputDebugStringA(PAnsiChar(AnsiString(nomeParametro)));
	while not CriaProcesso(nomeParametro , 0) do
	begin
		Delay(200);
		if (GetTickCount - tempo) > 6000 then
			break;
	end;
	ExitCM;
end;

procedure atualizarDoServidor;
var
	arquivos: TStrings;
	i: integer;
	response: TMemoryStream;
	res: AnsiString;
	tamanho, tamanhoTotal: int64;
	tamanhos: array of int64;
	Diretorio: AnsiString;
	upPath: AnsiString;
	upFullPath: AnsiString;
	UpdateNow: boolean;
	getStr: AnsiString;
begin
	//VMProtectBegin('atts');
	try
		arquivos := TStringList.create;
		response := TMemoryStream.create;
		IniAutenticacao.ListValues(IntToStr(1159), arquivos);
		SetLength(tamanhos, arquivos.Count - 1);
		Diretorio := diretorioPlugin;
		UpdateNow := false;
		totalBaixados := 0;
		tamanhoTotal := 0;
		totalAtual := 0;

		ChangeSplash(constante(0167, false));

		upPath := diretorioPlugin + (constante(0076, false));

		for i := 0 to arquivos.Count - 1 do
		begin
			if arquivos[i] = IntToStr(1159) then
			begin
				arquivos.Delete(i);
				break;
			end;
		end;

		// Pega o tamanho dos arquivos
		for i := 0 to arquivos.Count - 1 do
		begin
			if arquivos[i] = IntToStr(1159) then
				continue;
			try
				getStr := trim((constante(0021, false))) + '?' + md5(IntToStr(5123 + PostIndex)) + '=' + UrlEncode(IniAutenticacao.getvalue(IntToStr(1159), arquivos[i]).AsString) + '&' + md5(IntToStr(5124 + PostIndex)) + '=' + UrlEncode(md5(GetHDCode));
				//OutputDebugStringA(PAnsiChar(AnsiString( arquivos[i]+': '+trim((Constante(0021, false))) + '?' + md5(IntToStr(5123+PostIndex)) + '=' + UrlEncode( IniAutenticacao.getvalue(IntToStr(1159), arquivos[i]).AsString ) + '&' + md5(IntToStr(5124+PostIndex)) + '=' + UrlEncode( MD5(GetHDCode) ) )));
				tamanho := SafeStrToInt(splash.IdHTTP2.Get(getStr));
				if (tamanho = 33333) then
				begin
					ChangeSplash((constante(0177, false)));
					Delay(3000);
					ExitCM;
				end;
			except
				on e: exception do
				begin

				end;
			end;
			tamanhoTotal := tamanhoTotal + tamanho;
			tamanhos[i] := tamanho;
		end;

		totalGeral := tamanhoTotal;

		// Baixa os arquivos
		for i := 0 to arquivos.Count - 1 do
		begin
			if arquivos[i] = IntToStr(1159) then
			begin
				continue;
			end;

			try
				if tamanhos[i] = 0 then
				begin
					MessageBox(0, CMPchar(constante(0174, false)), CMPchar(constante(0092, true)), 0);
					exit;
				end;

				// Splash.Panel6.Caption := ExtractFileName(arquivos[i]);
				// Splash.Panel6.Update;
				// Application.ProcessMessages;

				// totalAtual :=  tamanhos[i];
				splash.IdHTTP2.Get(trim((constante(0021, false))) + '?' + md5(IntToStr(5113 + PostIndex)) + '=' + UrlEncode(IniAutenticacao.getvalue(IntToStr(1159), arquivos[i]).AsString) + '&' + md5(IntToStr(5124 + PostIndex)) + '=' + UrlEncode(md5(GetHDCode)), response);

				if (response.Size = 0) then
				begin
					MessageBox(0, CMPchar(constante(0175, false)), CMPchar(constante(0092, false)), 0);
					exit;
				end
				else
				begin
					if (response.Size = 5) then
					begin
						ChangeSplash((constante(0177, false)));
						Delay(3000);
						ExitCM;
					end;
				end;

				// totalBaixados := totalBaixados + tamanhos[i];
			except
				on e: exception do
				begin

				end;
			end;

			try
				try
					upFullPath := ExtractFilePath(upPath + arquivos[i]);

					if FileExists(upPath + arquivos[i]) then
					begin
						try
							DeleteFile(upPath + arquivos[i]);
						except
							on e: exception do
							begin

							end;
						end;
					end;

					ForceDirectories(upFullPath);
                    OutputDebugStringA(PAnsiChar(AnsiString('Baixando '+arquivos[i])));
					response.SaveToFile(upPath + arquivos[i]);
					UpdateNow := true;

				except
					on e: exception do
					begin
						MessageBox(0, CMPchar(constante(0176, false)), CMPchar(constante(0092, false)), 0);
						exit;
					end;
				end;
			finally
				response.Clear;
			end;
		end;

		if (FileExists(diretorioPlugin + constante(0076, false) + updatefile)) and (length(updatefile) > 0) then
		begin
			try
				MoveFileExA(PAnsiChar(AnsiString(diretorioPlugin + constante(0076, false) + updatefile)), PAnsiChar(AnsiString(diretorioPlugin + updatefile)), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH or MOVEFILE_COPY_ALLOWED);
			except
				on e: exception do
				begin

				end;
			end;
		end;

		if UpdateNow then
			ReloadCM;

	except
		on e: exception do
		begin
			MessageBox(0, CMPchar(constante(0092, false)), CMPchar(constante(0092, false)), 0);
			exit;
		end;
	end;
	//VMProtectEnd;
end;

function inicializar: integer;
var
	Post: TPost;
	HD: AnsiString;
	fPath, sPath, s: AnsiString;
	smpstr: AnsiString;
	PostResult: AnsiString;
	tmppos: integer;
	i, j: integer;
	Local, UpdatePath: AnsiString;
	// LangArq: AnsiString;
	SearchResult: TSearchRec;
	Searcher: TSearcher;
	IniUpdates: TCMIni;
	IniFile: TStrings;
	CurDir: AnsiString;
	hackModules: TStrings;
	Ini: TIniFile;
	debugString, testString: String;
	lingua: String;
	md5Str: String;
begin
    VMProtectBegin('incz');
	if splash <> nil then
	begin
		splash.Close;
		// Splash.FreeOnRelease;
		Application.RemovePopupForm(splash);
		// splash.Release;
		splash := nil; // .Free;
	end;

	if splash = nil then
	begin
		Application.CreateForm(TSplash, splash);
		// Splash := TSplash.Create(nil);
		splash.Show;
		splash.Refresh;
	end;

	try

		try
			// CurDir := Constante(0064, false);
			// listaMD5 := TStringList.Create;
			Randomize;
			result := 0;

			hackModules := TStringList.create;

			// O ini de atualização é encriptado com a chave de HD do usuário
			// (o mesmo é feito no servidor)
			Local := diretorioPlugin;
			UpdatePath := Local + (constante(0076, false));
			UpIni := TCMIni.create(GetHDCode, '');
			Ini := TIniFile.create(Local + (constante(0066, false)));
			Ini.EraseSection((constante(0041, false)));
			debugString := Ini.ReadString((constante(0067, false)), (constante(0065, false)), IntToStr(1));
			testString := Ini.ReadString((constante(0067, false)), (constante(0043, false)), IntToStr(1));
			Ini.Free;
			HD := GetHDCode;
			// Identifica o nome deste executavel e seu hash
			UpIni.setvalue(IntToStr(0091), IntToStr(0001), md5(LowerCase(diretorioPlugin)));
			// UpIni.setvalue(IntToStr(0091), IntToStr(0002), MD5File(ParamStr(0)));
			// MD5Print(MD5File(ParamStr(0))) );
			// {$IFDEF NO_HASH_MATRIZ}
			UpIni.setvalue(IntToStr(0091), IntToStr(0004), debugString);
			UpIni.setvalue(IntToStr(0091), IntToStr(0005), testString);
			UpIni.setvalue(IntToStr(0091), IntToStr(0007), versaoCMX);
{$IFNDEF NOAUT}
			ChangeSplash(constante(0203, false));
			HD := GetHDCode;
			IniAutenticacao := TCMIni.create(HD, '');
			IniUpdates := TCMIni.create(HD, '');

			try

				// Cria pasta de atualizações se não existir
				fPath := diretorioPlugin + 'Updates\';
				if not DirectoryExists(fPath) then
					CreateDir(fPath);

				sPath := diretorioPlugin + 'Screenshots\';
				if not DirectoryExists(sPath) then
					CreateDir(sPath);
{$IFNDEF DEBUG6}
				// Configura a conexão
				Post := TPost.create(false);
				Post.Host := trim((constante(0021, false)));
				Post.Clear;
				smpstr := MaxEncript(HD);

				// Solicita a lista de arquivos necessários
				Post.Add(IntToStr(1010 + PostIndex), smpstr);
				Post.Add(IntToStr(1157 + PostIndex), smpstr);
				Post.Add(IntToStr(1152 + PostIndex), testString);

				try
					PostResult := trim(Post.Execute);
					PostResult := AnsiReplaceStr(PostResult, #$D#$A, '');
					if verificaErro(PostResult) > 0 then
					begin
						// Application.RemovePopupForm(Splash);
						// Splash.Free;
						splash.ModalResult := 0;
						splash.Show;
						splash.Refresh;
						result := 2;
						exit;
					end;
					IniUpdates.LoadFromText(PostResult);
					IniFile := TStringList.create;
					IniUpdates.ListValues(IntToStr(1157), IniFile);

					lingua := IniUpdates.getvalue(IntToStr(1551), IntToStr(1551), 'PT_BR').AsString;
					if lingua = 'EN_US' then
						idioma := EN
					else
						idioma := PT;

					CurDir := diretorioPlugin;
					// Recebida a lista, devolve as caracteristicas de cada arquivos
					// solicitado: nome e hash
					for i := 0 to IniFile.Count - 1 do
					begin
						if FileExists(CurDir + IniFile.Strings[i]) then
						begin
							// Adiciona o HASH de cada arquivo solicitado pelo servidor
							// na lista e envia
							md5Str := MD5File(CurDir + IniFile.Strings[i]);
							// listaMD5.Add(IniFile.Strings[i]+': '+md5Str);
							UpIni.setvalue(IntToStr(1158), (IniFile.Strings[i]), md5Str);
						end;
					end;
				except
					on e: exception do
					begin
						ChangeSplash((constante(0009, false)) + ' - ' + e.ToString);
						Delay(3000);
						ExitCM;
					end;
				end;

				Post.Clear;
				Post.Add(IntToStr(1010 + PostIndex), smpstr);
				Post.Add(IntToStr(8131 + PostIndex), UpIni.Show);

				try
					// Dados enviados, se estiver tudo ok, o servidor retornará
					// os dados para funcionamento, senão, retornará uma lista de
					// arquivos a servem baixados
					PostResult := trim(Post.Execute);
					PostResult := AnsiReplaceStr(PostResult, #$D#$A, '');
					if verificaErro(PostResult) > 0 then
						exit;
				except
					on e: exception do
					begin
						// ChangeSplash( (Constante(0092, false))+' (code'+IntToStr(237)+')' );
						ChangeSplash((constante(0009, false)) + ' - ' + e.ToString);
						Delay(3000);
						result := 0;
						ExitCM;
					end;
				end;

				IniAutenticacao.LoadFromText(PostResult);
				IniKeys := TStringList.create;
				IniAutenticacao.ListKeys(IniKeys);

				// - Updater
				if IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9994), IntToStr(0)).AsString <> '0' then
					updatefile := IniAutenticacao.getvalue(IntToStr(5550), IntToStr(9994), IntToStr(0)).AsString;

				// Se recebeu lista para download, baixa os arquivos
				if IniAutenticacao.getvalue(IntToStr(1159), IntToStr(1159)).AsInteger = 1159 then
				begin
					atualizarDoServidor;
					exit;
				end;

				IniFile := TStringList.create;
				IniAutenticacao.ListValues(IntToStr(6834), IniFile);
				for i := 0 to IniFile.Count - 1 do
				begin
					SetLength(NomesPacotes, length(NomesPacotes) + 1);
					SetLength(TempoPacotes, length(TempoPacotes) + 1);

					NomesPacotes[length(NomesPacotes) - 1] := IniFile[i];
					TempoPacotes[length(TempoPacotes) - 1] := IniAutenticacao.getvalue(IntToStr(6834), IniFile[i], '0').AsString;
				end;

				IniFile.Free;
				IniFile := TStringList.create;
				IniAutenticacao.ListValues(IntToStr(6835), IniFile);
				for i := 0 to IniFile.Count - 1 do
				begin
					SetLength(OutrosValores, length(OutrosValores) + 1);
					OutrosValores[length(OutrosValores) - 1] := IniAutenticacao.getvalue(IntToStr(6835), IniFile[i], '').AsString;
				end;
				IniFile.Free;
				// Coloca no array os plugins liberados para carregar
				{ IniAutenticacao.ListValues(IntToStr(5551), hackModules);
				  for i := 0 to hackModules.Count-1 do
				  begin
				  SetLength(PluginsLiberados, length(PluginsLiberados)+1);
				  PluginsLiberados[length(PluginsLiberados)-1] := hackModules[i];
				  end; }

				s := IniAutenticacao.Text;
				if length(s) = 0 then
					exit;

				result := 1;
{$ENDIF}
			except
				on e: exception do
				begin
					ChangeSplash((constante(0176, false)) + ' (code' + IntToStr(254) + ')');
					Delay(3000);
					ExitCM
				end;
			end;
{$ENDIF}
		except
			on e: exception do
			begin
				MessageBox(0, CMPchar(constante(0092, false)), CMPchar(constante(0092, false)), 0);
				exit;
			end;
		end;
	finally
		if result = 1 then
			enviarPacotes;
		Application.RemovePopupForm(splash);
		splash.Free;
		// splash.Close;
	end;
	VMProtectEnd;
end;

end.
