unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TlHelp32, ExtCtrls;

type
  TForm3 = class(TForm)
    Edit3: TEdit;
    Button1: TButton;
    Edit4: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

Function GetProcessIdFromProcname(procName: AnsiString; indice: integer): cardinal;
var pe: TProcessEntry32;
    thSnapshot: THandle;
    retval,ProcFound: boolean;
    total, j, n: integer;
    processosSistema: array[1..10] of cardinal;
    filhoDoSistema: boolean;
    cbRet: cardinal;
    handle: cardinal;
begin

  if (indice = 0) then
    indice := 1;

    result := 0;
    total := 0;
   thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

   try
       if(thSnapshot = INVALID_HANDLE_VALUE) then
       begin
          result := 0;
          exit;
       end;

       pe.dwSize := sizeof(PROCESSENTRY32);

       retval := Process32First(thSnapshot, pe);

       n := 0;
       filhoDoSistema := false;
       while(retval) do
       begin
          if(LowerCase(Trim(pe.szExeFile)) = LowerCase(Trim(procName)) )  then
          begin
                ProcFound := true;
                inc(total);
                result := pe.th32ProcessID;
                break;
          end;

          retval    := Process32Next(thSnapshot, pe);
          pe.dwSize := sizeof(PROCESSENTRY32);
       end;

       if not ((ProcFound) and (total = indice)) then
         result := 0;
  finally
     CloseHandle(thSnapshot);
  end;
//{$I VMProtectEnd.inc}
end;

Function InjectDLL(id: dword; Dll: AnsiString): Boolean;
var
    processoH: THandle;
    TempHandle: THandle;
    AllocatedRegion: Pointer;
    Empty: dword;
    NumberOfBytesWritten: cardinal;
    RemoteString, LoadLibAddy: Pointer;
    nomeDll: PAnsiString;
begin
    nomeDll := PAnsiString(Dll);

    if (id = 0) then
    begin
        result := false;
        exit;
    end;
    //
    processoH := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, false, id);
    if (processoH > 0) then
    begin
        LoadLibAddy := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
        RemoteString := VirtualAllocEx(processoH, nil, length(Dll), MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
        // debugar('ID: '+IntToStr(id));
        Empty := 0;
        WriteProcessMemory(processoH, RemoteString, nomeDll, length(Dll), Empty);
        CreateRemoteThread(processoH, nil, 0, LoadLibAddy, RemoteString, 0, Empty);
        closehandle(processoH);
        result := true;
    end;
end;

procedure TForm3.Button1Click(Sender: TObject);
var id: integer;
    nomeDll: AnsiString;
begin
    //
    nomeDll := ExtractFilePath(ParamStr(0))+Edit3.Text+'.dll';
    id := GetProcessIdFromProcname(Edit4.Text,1);
	if(id > 0) then
    begin
        Panel2.Color := clGreen;

		if( not FileExists(nomeDll)) then
			Panel1.Color := clred
		else
        begin
			//LoadLibraryA(nomeDllC);
			InjectDLL(id, nomeDll );
            Panel1.Color := clGreen;
        end;

	end else
        Panel2.Color := clred
end;

end.
