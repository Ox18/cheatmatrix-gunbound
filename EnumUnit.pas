unit EnumUnit;

interface

uses windows, sysutils, TlHelp32, classes;

function EnumWindowsFunc(Handle: THandle; List: TStringList) : boolean ; stdcall;
Function CM_EnumWindows(Lista: TStrings): boolean;
Function CM_EnumProcesses(Lista: TStrings): boolean;

implementation

Function CM_EnumProcesses(Lista: TStrings): boolean;
var pe: TProcessEntry32;
    thSnapshot: THandle;
    retval,ProcFound: boolean;
begin
   result := false;

   thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

   if(thSnapshot = INVALID_HANDLE_VALUE) then
   begin
      exit;
   end;

   pe.dwSize := sizeof(PROCESSENTRY32);

   retval := Process32First(thSnapshot, pe);
   
   Lista.Clear;
   while(retval) do
   begin
      Lista.Add(FormatFloat('000000',pe.th32ProcessID)+' - '+Trim(pe.szExeFile));

      retval    := Process32Next(thSnapshot, pe);
      pe.dwSize := sizeof(PROCESSENTRY32);
   end;

   result := true;
end;

Function CM_EnumWindows(Lista: TStrings): boolean;
begin
    Lista.Clear;
    EnumWindows(@EnumWindowsFunc, LParam(Lista));
end;

function EnumWindowsFunc(Handle: THandle; List: TStringList) : boolean ; stdcall;
var Caption: array[0..256] of Char;
    pid: cardinal;
begin
    result := false;

    if GetWindowText(Handle, Caption, SizeOf(Caption)-1) <> 0 then
    begin
        GetWindowThreadProcessId(Handle,@pid);
        List.Add(FormatFloat('000000', pid)+' - '+ Caption);
    end;

    Result :=True;
end;

end.
