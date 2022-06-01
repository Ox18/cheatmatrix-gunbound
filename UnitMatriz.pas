unit UnitMatriz;

interface

// {$I Includes.inc}
// {$DEFINE NOUPDATE}
// {$DEFINE NOAUT}
// {$DEFINE GRAVAAUT}
// {$DEFINE DEBUG_FLAG}
// {$DEFINE DEBUG}

(* *****************************************************************************
  *
  *                              CHEAT MATRIX X
  *                              by SkyW4rrior
  *
  ****************************************************************************** *)

// {$DEFINE HIDEPANELS}

uses
    // -- Delphi --------------------------------------------------
    Windows, ExtCtrls, ImgList, Controls, StdCtrls, ComCtrls,
    Messages, sysutils, Graphics, dialogs,
    Classes, Forms, WinSvc, registry,
    ComObj, variants, strutils, jpeg,
    OleCtrls, IdComponent, IdTCPConnection, IdTCPClient,
    inifiles, TlHelp32, functions, VMProtectSDK,
    SkyIO, ProcListing, CMClasses, types,
    Encription, cmini, ConfigsLoader, common,
    constantes, CmPlugins, Utils, SplashScreen, SkySql,
    CMStatus, langcontrol, GIFImg, Menus, IdAntiFreezeBase, IdAntiFreeze,
    IdBaseComponent, IdHTTP, ScktComp;

Function HtmlHelp(hwndCaller: THandle; pszFile: PChar; uCommand: cardinal; dwData: longint): THandle; stdcall; external 'hhctrl.ocx' name 'HtmlHelpA';

const
    CMX_MESSAGE = WM_USER + 4941;

type
    QWord = Int64;
    PtrUInt = DWord;
    TIsWow64Process = function(processhandle: THandle; var isWow: BOOL): BOOL; stdcall;

Type
    TKeyInfo = record
        Handle: cardinal;
        Codigo: AnsiString;
    end;

    PKeyInfo = ^TKeyInfo;

    TMatrizKey = class
        constructor create;
    private
        fLastTime: Int64;
        fkey: AnsiString;
        keylist: TList;
    public
        Procedure Add(valor: char);
        Procedure RegisterKey(pluginHandle: cardinal; Codigo: AnsiString);
        Procedure DeleteKey(index: integer; plhandle: cardinal = 0);
        Property key: AnsiString read fkey;
        Procedure ClearRegisters;
        Procedure ClearKey;
        Property LastTime: Int64 read fLastTime write fLastTime;
        Procedure ProcessKey;
    end;

    _CMSYSTEMTIME = record
        Year: word;
        Month: word;
        Day: word;
        Hour: word;
        Minute: word;
        Second: word;
        Milliseconds: word;
        Weekday: word;
    end;

type
    TfrmMatriz = class(TForm)
        imgtImages: TImageList;
        AutoSelector: TTimer;
        TimerLauncher: TTimer;
        Mostrador: TPanel;
        GrupoProcs: TGroupBox;
        Label2: TLabel;
        ProcList: TComboBox;
        PanelOrdem: TPanel;
        GroupBox3: TGroupBox;
        Org_Dec: TRadioButton;
        Org_Cres: TRadioButton;
        GroupBox1: TGroupBox;
        Ord_Pid: TRadioButton;
        Ord_Nome: TRadioButton;
        Ord_Criacao: TRadioButton;
        GameList: TComboBox;
        Panel3: TPanel;
        Label1: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Edit1: TEdit;
        Edit2: TEdit;
        Edit3: TEdit;
        Edit4: TEdit;
        Edit5: TEdit;
        Edit6: TEdit;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Bevel1: TBevel;
        Shape1: TShape;
        Shape2: TShape;
        HackList: TListView;
        Bevel2: TBevel;
        Image1: TImage;
        Label8: TLabel;
        botaoConfig: TPanel;
        Panel1: TPanel;
        labelVersao: TLabel;
        Shape3: TShape;
        Shape4: TShape;
        Shape5: TShape;
        Label10: TLabel;
        Shape6: TShape;
        Shape7: TShape;
        Shape8: TShape;
        Panel2: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        CMXTray: TTrayIcon;
        PopupMenu1: TPopupMenu;
        restaurar1: TMenuItem;
        N1: TMenuItem;
        sair1: TMenuItem;
        GroupBox2: TGroupBox;
        listaTempo: TListBox;
        botaoAbrirJogo: TPanel;
        OpenDialog1: TOpenDialog;
        Label9: TLabel;
        Panel6: TPanel;
        ClientSocket1: TClientSocket;
        procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure MostradorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure Image4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure pingerTimer(Sender: TObject);
        procedure Sair1Click(Sender: TObject);
        procedure Minimizar1Click(Sender: TObject);
        procedure ShockwaveFlashEx1Click(Sender: TObject);
        procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure AutoSelectorTimer(Sender: TObject);
        procedure TrayIcon1DblClick(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure btnUnloadClick(Sender: TObject);
        procedure LabelCaptionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure BitBtnWithColor2Click(Sender: TObject);
        procedure BitBtnWithColor3Click(Sender: TObject);
        procedure ProcListDrawItem(Control: TWinControl; Index: integer; Rect: TRect; State: TOwnerDrawState);
        procedure DebugButtonClick(Sender: TObject);
        procedure ProcListChange(Sender: TObject);
        procedure ProcListDropDown(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure GameListChange(Sender: TObject);
        procedure HackListClick(Sender: TObject);
        procedure btnAtualizarClick(Sender: TObject);
        procedure btnSairClick(Sender: TObject);
        procedure ControlListClick(Sender: TObject);
        procedure TimerLauncherTimer(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure AtalhosTimerTimer(Sender: TObject);
        procedure Sair2Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure botaoConfigMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure botaoConfigMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure botaoConfigClick(Sender: TObject);
        procedure Panel1Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure restaurar1Click(Sender: TObject);
        procedure Panel4Click(Sender: TObject);
        procedure CMXTrayDblClick(Sender: TObject);
        procedure Panel5Click(Sender: TObject);
        procedure IdHTTP2WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
        procedure IdHTTP2Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
        procedure IdHTTP2WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
        procedure IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
        procedure IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
        procedure IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
        procedure botaoAbrirJogoClick(Sender: TObject);
        procedure Button8Click(Sender: TObject);
        procedure Panel6Click(Sender: TObject);
        procedure ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: integer);
    procedure ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
    private
        { Private declarations }
        // function CmHookProc(nCode: Integer; wp: wParam; lp: lParam): LongInt;
        // Procedure RecebeMensagens(var Msg: TMessage); message WM_COPYDATA;

        procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
        procedure MessageReceiver(var Msg: TMessage); message CMX_MESSAGE;
        procedure ChamaHelp(var Msg: TMsg; var Handled: Boolean);
    public
        // procedure WMNChitTest(var M: TWMNchitTest);

        Procedure AddHack(Nick: String; Tipo: THackType = HT_Hack; Status: TImageStatus = IS_Normal);
        Procedure RemoveFomList(Nome: AnsiString; Tipo: THackType = HT_Hack);
        Procedure OpenPlugin(Sender: TObject);
        Procedure ProcessRequests(var Msg: TWMCopyData); message WM_COPYDATA;
        Procedure LoadPlugins;
        // procedure LoadCmConfigs;
        Procedure CMOnConect;
        Procedure TraduzCMX;
        procedure verificarConfigDDraw;

        // procedure WMClose(var Msg : TWMClose); message WM_CLOSE;
        // function  DownloadURL_NOCache(const aUrl: string; fsize: int64; s: String): integer;
        // function DownloadURL_NOCache(const aUrlX: AnsiString; fsize: int64; sx: AnsiString): integer;
        { procedure URL_OnDownloadProgress (Sender: TDownLoadURL; Progress, ProgressMax:
          Cardinal; StatusCode: TURLDownloadStatus; StatusText: String; var Cancel:
          Boolean) ;
          function DoDownload(vURL, vArquivo: String; size: int64): Boolean; }
        // Function DownloadUpdateFile(arquivo: AnsiString; saveto: AnsiString): boolean;
    end;

var
    frmMatriz: TfrmMatriz;
    Mutex: THandle;
    Plugins: TPluginList;
    mBoxList: array of TMbox;
    mDigitalList: array of TMDigital;
    // LMsg: TLabel;
    THPainel: THandle;
    THPainelID: DWord;
    WaitBy: cardinal = 9;
    LastSelected: integer;
    MsgLeft: integer;
    MsgTop: cardinal;
    MsgText: AnsiString;
    MsgCanvas: TCanvas;
    AnimCanvas: TCanvas;
    LastProcSelected: DWord;
    HotKeys: TMatrizKey;
    VersaoJogoSelecionada: AnsiString;
    hdevice: THandle = INVALID_HANDLE_VALUE;
    apppath: PChar;
    servicename, sysfile: AnsiString;
    driverloc: AnsiString;
    sav: PAnsiChar;
    injectorNameDLL: AnsiString;
    // CMHotKeys: TCMHotkeys;

    // oldProc: Integer;

    hookl: cardinal;

const
    FILE_RW_ACCESS = FILE_READ_ACCESS or FILE_WRITE_ACCESS;

    IOCTL_CM_READMEMORY = (FILE_DEVICE_UNKNOWN shl 16) or ($0230 shl 2) or (METHOD_BUFFERED) or (FILE_RW_ACCESS shl 14);
    IOCTL_CM_WRITEMEMORY = (FILE_DEVICE_UNKNOWN shl 16) or ($0231 shl 2) or (METHOD_BUFFERED) or (FILE_RW_ACCESS shl 14);
    IOCTL_CM_OPENPROCESS = (FILE_DEVICE_UNKNOWN shl 16) or ($0232 shl 2) or (METHOD_BUFFERED) or (FILE_RW_ACCESS shl 14);

var
    mutant: cardinal;
    FormPacked: Boolean = false;
    FormPacking: Boolean = false;
    kernel32dll: THandle;
    IsWow64Process: TIsWow64Process;
    iswow64: BOOL;
    Successfullyloaded: Boolean;
    hscManager: THandle;
    hservicE: THandle;
    applicationPath: string;
    dataloc: string;
     injectorHandle: HMODULE;
     isconectado: function(porta: integer): boolean; stdcall;

    PingCount: word;
    PingValue: word;
    // dllMaeInject: Function(processID: DWORD): boolean; stdcall;
    // dllMae: DWORD;

    RetAtual, RetDest: integer;

Function InjectDLL(id: DWord; Dll: AnsiString): Boolean;
function lerConfig(sessao: AnsiString; key: AnsiString): AnsiString;
procedure gravarConfig(sessao: AnsiString; key: AnsiString; valor: AnsiString);
function getRegistroJogo: AnsiString;
function verificaInstallDDrawDLL(path: AnsiString): Boolean;
procedure Delay2(dwMilliseconds: longint);
Procedure ExitCM;
function InitializeDriver: Boolean;

function { ReadProcessMemory } rpm64(pid: integer; hProcess: THandle; lpBaseAddress: uint64; lpBuffer: pointer; nSize: DWord; var NumberOfBytesRead: DWord): BOOL; stdcall;
function { ReadProcessMemory64 } RPM(pid: integer; hProcess: THandle; lpBaseAddress: pointer; lpBuffer: pointer; nSize: DWord; var NumberOfBytesRead: DWord): BOOL; stdcall;
function { WriteProcessMemory } wpm64(pid: integer; hProcess: THandle; BaseAddress: QWord; lpBuffer: pointer; nSize: DWord; var NumberOfBytesWritten: DWord): BOOL; stdcall;
function { WriteProcessMemory } WPM(pid: integer; hProcess: THandle; lpBaseAddress: pointer; lpBuffer: pointer; nSize: DWord; var NumberOfBytesWritten: DWord): BOOL; stdcall;
function UnloadDriver: Boolean;

implementation

{$R *.dfm}

uses ShellAPI, Urlmon, inicializador;

// -----------------------------------------------------
function noIsWow64(processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
begin
    if @isWow <> nil then
        isWow := false;

    result := false;
end;

function isx64: Boolean;
var
    isw: LongBool;
begin
    @IsWow64Process := GetProcAddress(kernel32dll, 'IsWow64Process');
    if (@IsWow64Process = nil) then
        result := false
    else
    begin
        IsWow64Process(GetCurrentProcess, isw);
        result := isw;
    end;
end;

function StartService(Server: String; servicename: String): Boolean;
var
    SCH: SC_HANDLE;
    SvcSCH: SC_HANDLE;
    Arg: PChar;
    OldCur: TCursor;
begin
    result := false;
    OldCur := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
        SCH := OpenSCManagerA(PAnsiChar(Server), nil, SC_MANAGER_ALL_ACCESS);
        try
            if SCH = 0 then
            begin
                // Raise Exception.create('StartService OpenSCManager failed: ' + IntToStr(GetLastError));
                Exit;
            end;

            SvcSCH := OpenServiceA(SCH, PAnsiChar(servicename), SERVICE_ALL_ACCESS);
            if SvcSCH = 0 then
            begin
                // Raise Exception.create('StartService OpenService failed: ' + IntToStr(GetLastError));
                Exit;
            end;

            try
                Arg := nil;
                if not WinSvc.StartService(SvcSCH, 0, Arg) then
                begin
                    // Raise Exception.create('StartService failed: ' + IntToStr(GetLastError));
                    Exit;
                end;

                result := True;
            finally
                CloseServiceHandle(SvcSCH);
            end;
        finally
            CloseServiceHandle(SCH);
        end;
    finally
        Screen.Cursor := OldCur;
    end;
end;

function InitializeDriver: Boolean;
begin
    result := True;
    if hdevice = INVALID_HANDLE_VALUE then
    begin
        kernel32dll := loadlibrary('kernel32.dll');
        IsWow64Process := GetProcAddress(kernel32dll, 'IsWow64Process');
        if not assigned(IsWow64Process) then
            IsWow64Process := noIsWow64;

{$IFDEF cpu64}
        iswow64 := True;
{$ELSE}
        IsWow64Process(GetCurrentProcess, iswow64);
{$ENDIF}
        Successfullyloaded := false;
        apppath := nil;
        hscManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);

        try
            GetMem(apppath, 250);
            GetModuleFileName(0, apppath, 250);

            applicationPath := ExtractFilePath(apppath);

            // outputdebugstring('b');
            if not fileexists(dataloc) then
            begin
                // //outputdebugstring('b1');
                servicename := 'CReg';
                if iswow64 then
                    sysfile := 'cmg.dll'
                else
                    sysfile := 'cmg.dll';
            end;

            driverloc := ExtractFilePath(ParamStr(0)) + sysfile;
        finally
            freemem(apppath);
        end;

        if not fileexists(driverloc) then
        begin
            // //outputdebugstring('b2');
            // messagebox(0, 'You are missing the driver. Try reinstalling cheat engine, and try to disable your anti-virus before doing so.', 'Driver error', MB_ICONERROR or mb_ok);
            hdevice := INVALID_HANDLE_VALUE;
            result := false;
            Exit;
        end;

        // //outputdebugstring('b3');
        if hscManager <> 0 then
        begin
            // outputdebugStringA(PAnsiChar(servicename));
            // outputdebugStringA(PAnsiChar(driverloc));
            hservicE := OpenServiceA(hscManager, PAnsiChar(servicename), SERVICE_ALL_ACCESS);
            if hservicE = 0 then
            begin
                hservicE := CreateServiceA(hscManager, // SCManager database
                    PAnsiChar(servicename), // name of service
                    PAnsiChar(servicename), // name to display
                    SERVICE_ALL_ACCESS, // desired access
                    SERVICE_KERNEL_DRIVER, // service type
                    SERVICE_DEMAND_START, // start type
                    SERVICE_ERROR_NORMAL, // error control type
                    PAnsiChar(driverloc), // service's binary
                    nil, // no load ordering group
                    nil, // no tag identifier
                    nil, // no dependencies
                    nil, // LocalSystem account
                    nil // no password
                    );
                // //outputdebugstring('b4');
            end
            else
            begin
                // make sure the service points to the right file
                // //outputdebugstring('b5');
                result := false;
                ChangeServiceConfigA(hservicE, SERVICE_KERNEL_DRIVER, SERVICE_DEMAND_START, SERVICE_ERROR_NORMAL, PAnsiChar(driverloc), nil, nil, nil, nil, nil, PAnsiChar(servicename));
            end;

            if hservicE <> 0 then
            begin
                // sav := nil;
                // //outputdebugstring('b6');
                if not StartServiceA(hservicE, 0, sav) then
                begin
                    // outputdebugStringA(PAnsiChar(AnsiString('Erro D: ' + AnsiString(IntToStr(GetLastError)))));
                    if GetLastError > 0 then
                    begin
                        result := false;
                        // messagebox(0, 'Please reboot and press F8 during boot. Then choose "allow unsigned drivers". ' + #13#10 + 'Alternatively you could sign the driver yourself.' + #13#10 + 'Just buy yourself a class 3 business signing certificate and sign the driver. Then you''ll never have to reboot again to use this driver', 'DBK32 error', MB_ICONERROR or mb_ok);
                        // failedduetodriversigning := true;
                    end; // else could already be started
                end;

                CloseServiceHandle(hservicE);
            end
            else
            begin
                // messagebox(0, 'The service couldn''t get opened and also couldn''t get created.' + ' Check if you have the needed rights to create a service, or call your system admin (Who''ll probably beat you up for even trying this). Untill this is fixed you won''t be able to make use of the enhancements the driver gives you', 'DBK32 Error', MB_ICONERROR or mb_ok);
                hdevice := INVALID_HANDLE_VALUE;
                result := false;
                Exit;
            end;
            // //outputdebugstring('b7');
            hdevice := INVALID_HANDLE_VALUE;
            hdevice := CreateFileA(PAnsiChar(AnsiString('\\.\' + servicename)), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);

            CloseServiceHandle(hscManager);
        end;
    end;

end;

function UnloadDriver: Boolean;
var
    serviceStatus: _SERVICE_STATUS;
    SchSCManager, schService: SC_HANDLE;
begin
    SchSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    schService := OpenService(SchSCManager, 'CReg', SERVICE_ALL_ACCESS);
    ControlService(schService, SERVICE_CONTROL_STOP, &serviceStatus);
    DeleteService(schService);
    CloseServiceHandle(SchSCManager);
    CloseServiceHandle(schService);
    CloseHandle(hdevice);
end;

function { ReadProcessMemory64 } RPM(pid: integer; hProcess: THandle; lpBaseAddress: pointer; lpBuffer: pointer; nSize: DWord; var NumberOfBytesRead: DWord): BOOL; stdcall;
begin
    if (isx64) then
    begin
        result := ReadProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, NumberOfBytesRead);
    end
    else
        result := rpm64(pid, hProcess, uint64(PtrUInt(lpBaseAddress)), lpBuffer, nSize, NumberOfBytesRead);
end;

function { ReadProcessMemory } rpm64(pid: integer; hProcess: THandle; lpBaseAddress: uint64; lpBuffer: pointer; nSize: DWord; var NumberOfBytesRead: DWord): BOOL; stdcall;
type
    TInputstruct = packed record
        processid: uint64;
        startaddress: uint64;
        bytestoread: word;
    end;
var // ao: array [0..600] of byte; //give it some space
    input: TInputstruct;
    cc: DWord;

    i: integer;
    ok: Boolean;
    br: DWord;

    mempointer: QWord;
    bufpointer: PtrUInt;
    toread: DWord;
begin
    result := false;
    NumberOfBytesRead := 0;
    // find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

    if hdevice <> INVALID_HANDLE_VALUE then
    begin
        cc := IOCTL_CM_READMEMORY;
        mempointer := lpBaseAddress;
        bufpointer := PtrUInt(lpBuffer);

        ok := True;
        while ok do
        begin
            input.processid := pid;
            if (mempointer and $FFF) > 0 then // uneven
            begin
                toread := 4096 - (mempointer and $FFF);
                if toread > (nSize - NumberOfBytesRead) then
                    toread := nSize - NumberOfBytesRead;
            end
            else
            begin
                if nSize - NumberOfBytesRead >= 4096 then
                    toread := 4096
                else
                    toread := nSize - NumberOfBytesRead;
            end;

            input.bytestoread := toread;
            input.startaddress := mempointer;

            if not deviceiocontrol(hdevice, cc, @input, sizeof(input), pointer(bufpointer), toread, br, nil) then
                Exit;

            inc(mempointer, toread);
            inc(bufpointer, toread);
            inc(NumberOfBytesRead, toread);

            if NumberOfBytesRead = nSize then
            begin
                result := True;
                Exit;
            end;
        end;

        Exit;
    end
    else
        result := ReadProcessMemory(hProcess, pointer(lpBaseAddress), lpBuffer, nSize, NumberOfBytesRead);

    // not found so ....
    result := false; // Windows.ReadProcessMemory(hProcess, pointer(ptrUint(lpBaseAddress)), lpBuffer, nSize, NumberOfBytesRead);
end;

function { WriteProcessMemory } WPM(pid: integer; hProcess: THandle; lpBaseAddress: pointer; lpBuffer: pointer; nSize: DWord; var NumberOfBytesWritten: DWord): BOOL; stdcall;
begin
    if (isx64) then
    begin
        // //outputdebugStringA('w0');
        result := WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, NumberOfBytesWritten)
    end
    else
        result := wpm64(pid, hProcess, uint64(PtrUInt(lpBaseAddress)), lpBuffer, nSize, NumberOfBytesWritten);
end;

function { WriteProcessMemory } wpm64(pid: integer; hProcess: THandle; BaseAddress: QWord; lpBuffer: pointer; nSize: DWord; var NumberOfBytesWritten: DWord): BOOL; stdcall;
type
    TInputstruct = packed record
        processid: uint64;
        startaddress: uint64;
        bytestowrite: word;
    end;
var
    ao: array [0 .. 511] of byte;
    input: TInputstruct absolute ao[0];
    cc: DWord;

    i: integer;
    ok: Boolean;
    br: DWord;

    mempointer: QWord;
    bufpointer: PtrUInt;
    bufpointer2: pointer;
    towrite: DWord;
begin
    result := false;
    NumberOfBytesWritten := 0;
    // //outputdebugStringA('w1');
    // find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

    if hdevice <> INVALID_HANDLE_VALUE then
    begin
        // outputdebugStringA('w');
        cc := IOCTL_CM_WRITEMEMORY;
        mempointer := BaseAddress;
        bufpointer := PtrUInt(lpBuffer);

        ok := True;
        while ok do
        begin
            zeromemory(@ao[0], 512);
            // //outputdebugStringA('w3');
            input.processid := pid;
            if nSize - NumberOfBytesWritten >= (512 - sizeof(TInputstruct)) then
                towrite := (512 - sizeof(TInputstruct))
            else
                towrite := nSize - NumberOfBytesWritten;

            input.bytestowrite := towrite;
            input.startaddress := mempointer;

            bufpointer2 := pointer(bufpointer);
            copymemory(@ao[sizeof(TInputstruct)], bufpointer2, towrite);

            // //outputdebugString(pchar('sizeof(TInputstruct)='+inttostr(sizeof(TInputstruct))));
            // //outputdebugStringA(PAnsiChar(AnsiString('size: ' + AnsiString(IntToStr(cardinal(sizeof(TInputstruct)))))));
            // //outputdebugStringA(PAnsiChar(AnsiString('End: ' + AnsiString(IntToStr(cardinal(bufpointer2))) +' - '+AnsiString(IntToStr(cardinal(lpBuffer))))));
            // for i := 0 to 500 do
            // begin
            // //outputdebugStringA(PAnsiChar(AnsiString(AnsiString(IntToHex(ao[i],2)))));
            // end;

            if not deviceiocontrol(hdevice, cc, @ao[0], 512, @ao[0], 512, br, nil) then
                Exit;

            inc(mempointer, towrite);
            inc(bufpointer, towrite);
            inc(NumberOfBytesWritten, towrite);

            if NumberOfBytesWritten = nSize then
            begin
                result := True;
                Exit;
            end;
        end;

        Exit;
    end
    else
        result := WriteProcessMemory(hProcess, pointer(BaseAddress), lpBuffer, nSize, NumberOfBytesWritten)

end;

function { OpenProcess } OP(dwDesiredAccess: DWord; bInheritHandle: BOOL; dwProcessId: DWord): THandle; stdcall;
var
    valid: Boolean;
    processhandle: uint64;
    i: integer;
    cc, X: DWord;
begin
    valid := True;
    if dwProcessId = 0 then
    begin
        result := 0;
        Exit;
    end;

    if (hdevice <> INVALID_HANDLE_VALUE) and (not isx64) then
    begin
        cc := IOCTL_CM_OPENPROCESS;

        if deviceiocontrol(hdevice, cc, @dwProcessId, 4, @processhandle, 8, X, nil) then
        begin
            result := processhandle
        end
        else
            result := 0;
    end
    else
        result := OpenProcess(dwDesiredAccess, bInheritHandle, dwProcessId);
end;


// -----------------------------------------------------

procedure debugar(valor: AnsiString);
begin
    try
        outputdebugStringA(PAnsiChar(valor));
    except
        Exit;
    end;
end;

Procedure ExitCM;
var
    SelfProc: THandle;
    nNome: AnsiString;
    s: AnsiString;
begin

    try
        MatrizInfo.Ligado := false;
        exitprocess(0);
        SelfProc := OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId);
        TerminateProcess(SelfProc, 0);
        CloseHandle(SelfProc);
    except
        on e: Exception do
    end;
    //
end;

Function GetCMProcessList(valor: TStrings; showHidden: Boolean): Boolean;
type
    TPListRes = record
        pid: cardinal;
        Creation: _CMSYSTEMTIME;
        Nome: PAnsiChar;
    end;

type
    TOrdem = record
        Index: cardinal;
        Creation: TDateTime;
        pid: cardinal;
        Nome: AnsiString;
    end;

var
    Buf: array of TPListRes;
    bytes, ret: DWord;
    Nome: AnsiString;
    i, j, k: cardinal;
    Lista: array of TOrdem;
    qnt: cardinal;
    Reserva: TOrdem;
    temp: _SYSTEMTIME;
    s, t: AnsiString;
    cid: DWord;

    pe: TProcessEntry32;
    thSnapshot: THandle;
    retval, ProcFound: Boolean;
begin
    result := false;
    qnt := 0;

    thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

    if (thSnapshot = INVALID_HANDLE_VALUE) then
    begin
        Exit;
    end;

    pe.dwSize := sizeof(PROCESSENTRY32);

    retval := Process32First(thSnapshot, pe);

    while (retval) do
    begin
        setlength(Lista, length(Lista) + 1);
        Lista[length(Lista) - 1].pid := pe.th32ProcessID;
        Lista[length(Lista) - 1].Nome := pe.szExeFile;
        inc(qnt);
        retval := Process32Next(thSnapshot, pe);
        pe.dwSize := sizeof(PROCESSENTRY32);
    end;

    result := True;

    // Loop pela lista de processos
    for i := 0 to qnt - 1 do
    begin
        cid := (Lista[i].pid);
        t := FormatFloat( { '['+ inttostr(i)+ } '000000', cid) + '  -  ' + (Lista[i].Nome);
        // s := trim(CheckValidPID(cid));
        s := IntToStr(Lista[i].pid);
        if (Lista[i].pid = 0) then
            continue;

{$IFNDEF DEBUG6}
{$IFNDEF RESUMO}
        // Verifica se há algum processo na lista de permissoes de amostragem de
        // processo dos plugins
        if (CurrentPackIndex >= 0) then
            for k := 0 to length(PackProcList[CurrentPackIndex].Processos) - 1 do
            begin
                if (lowercase(trim((Lista[i].Nome))) = lowercase(trim(PackProcList[CurrentPackIndex].Processos[k]))) or (lowercase(trim(PackProcList[CurrentPackIndex].Processos[k])) = '***') and (lowercase(trim(PackProcList[CurrentPackIndex].Processos[k])) <> '') then
                begin
{$ENDIF}
{$ENDIF}
                    if { ( s <> trim(LowerCase(BypassString(Lista[i].Nome)))) or }
                        (s = '') then
                    begin
                        if not showHidden then
                            valor.Add('H' + t);
                    end
                    else
                        valor.Add('N' + t);
{$IFNDEF DEBUG6}
{$IFNDEF RESUMO}
                    if (lowercase(trim(PackProcList[CurrentPackIndex].Processos[k])) = '***') then
                        break;
                end;
            end;
{$ENDIF}
{$ENDIF}
    end;

    //
end;

{
  Function GetMutantAddress(module: HMODULE; valor: integer): Pointer;
  type TF1 = Function(valor: integer): cardinal; stdcall;
  var F1: TF1;
  begin
  @F1 := GetProcAddress(module, PAnsiChar(LoadConfig('Prefix').AsString+'1'));
  result := Pointer(F1(valor)-456789);
  end; }

Procedure Writelog(valor: AnsiString);
var
    s: TStrings;
    p: AnsiString;
begin
    /// /
    p := ExtractFilePath(ParamStr(0)) + 'log.txt';
    s := TStringList.create;
    if not fileexists(p) then
    begin
        s.Add(valor);
        s.SaveToFile(p);
    end
    else
    begin
        s.LoadFromFile(p);
        s.Add(valor);
        s.SaveToFile(p);
    end;
    s.Free;
    /// /
end;

Function SafeStrToInt(valor: AnsiString): integer;
begin
    valor := trim(valor);

    result := 0;
    try
        result := strtoint(valor);
    except
        on e: Exception do
        begin
            result := 0;
            Exit;
        end;
    end;
end;

var
    TotalDown: Int64;

var
    TotalArqsDown: Int64;
    ActualSize: Int64;

procedure Delay2(dwMilliseconds: longint);
var
    iStart, iStop: DWord;
begin
    iStart := GetTickCount;
    repeat
        iStop := GetTickCount;
        Application.ProcessMessages;
        Sleep(1); // addition from Christian Scheffler to avoid high CPU last
    until (iStop - iStart) >= dwMilliseconds;
end;

Function CheckFileState(arq: AnsiString; data: AnsiString): integer;
var
    fData: TDateTime;
    cData: TDateTime;
    Aplicacao: THandle;
    FPath: AnsiString;
    Info1, Info2, Info3: TFileTime;
    Estrutura: SystemTime;
begin
    FPath := ExtractFilePath(ParamStr(0)) + arq;
    if fileexists(FPath) then
    begin
        try
            fData := StrToDateTime(trim(data));
        except
            on e: Exception do
                fData := MinDateTime;
        end;

        try // formatdatetime('dd/mm/yyyy hh:mm:ss', cdata)+'  -  '+formatdatetime('dd/mm/yyyy hh:mm:ss', fdata)
            Aplicacao := FileOpen(FPath, fmOpenRead or fmShareDenyNone);
            if Aplicacao > 0 then
            begin
                GetFileTime(Aplicacao, @Info1, @Info2, @Info3);
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
    end
    else
    begin
        result := 0;
    end;
end;

procedure TfrmMatriz.WMHotKey(var Msg: TWMHotKey);
begin

end;

procedure TfrmMatriz.ChamaHelp(var Msg: TMsg; var Handled: Boolean);
begin

end;

procedure TfrmMatriz.ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: integer);
begin
    ErrorCode := 0;
end;

procedure TfrmMatriz.ClientSocket1Read(Sender: TObject;
  Socket: TCustomWinSocket);
begin
debugar('rr');
if socket.ReceiveLength = 0 then
    ClientSocket1.Active := false;
end;

// TfrmMatriz.
Procedure TfrmMatriz.CMOnConect;
begin
    ChangeSplash(Constante(0010, false));
end;

Procedure DecMsgPos(decIt: Boolean = True);
begin
    if decIt then
        dec(MsgLeft);
    MsgCanvas.TextOut(MsgLeft, MsgTop, '     ' + MsgText + '     ');
end;

Procedure TfrmMatriz.RemoveFomList(Nome: AnsiString; Tipo: THackType = HT_Hack);
var
    i: integer;
    p: pointer;
begin
    case integer(Tipo) of
        integer(HT_Hack):
            p := @frmMatriz.HackList;
        integer(HT_Control):
            p := @frmMatriz.HackList;
    end;

    for i := 0 to TListView(p^).Items.Count - 1 do
    begin
        if trim(lowercase(TListView(p^).Items.Item[i].Caption)) = trim(lowercase(Nome)) then
        begin
            TListView(p^).Items.Delete(i);
        end;
    end;
end;

procedure TfrmMatriz.restaurar1Click(Sender: TObject);
begin
    Application.Restore;
    Show;
    CMXTray.Visible := false;
end;

const
    SC_DragMove = $F012; { a magic number }

procedure TfrmMatriz.Sair1Click(Sender: TObject);
begin
    try
        CloseHandle(hMemFile);
    except

    end;
    if processhandle > 0 then
    begin
        try
            CloseHandle(processhandle);
        except

        end;
    end;

    try
        TerminateThread(THPainel, 0);
    except

    end;
    close;
    ExitCM;
end;

procedure TfrmMatriz.Sair2Click(Sender: TObject);
begin
    btnSairClick(self);
end;

procedure TfrmMatriz.ShockwaveFlashEx1Click(Sender: TObject);
var
    X, Y: integer;
begin

end;

procedure TfrmMatriz.TrayIcon1DblClick(Sender: TObject);
begin
    Application.Restore;
end;

Procedure TfrmMatriz.AddHack(Nick: String; Tipo: THackType = HT_Hack; Status: TImageStatus = IS_Normal);
var
    i: integer;
    p: pointer;
begin

    //

    case integer(Tipo) of
        integer(HT_Hack):
            p := @frmMatriz.HackList;
        integer(HT_Control):
            p := @frmMatriz.HackList;
    end;

    TListView(p^).Items.Add;
    TListView(p^).Items.Item[TListView(p^).Items.Count - 1].Caption := ' ' + Nick;
    case integer(Status) of
        integer(IS_Normal):
            i := 0;
        integer(IS_Main):
            i := 2;
        integer(IS_Open):
            i := 3;
        integer(IS_Running):
            i := 1;
    end;

    TListView(p^).Items.Item[TListView(p^).Items.Count - 1].ImageIndex := i;
    //
end;

procedure TfrmMatriz.GameListChange(Sender: TObject);
var
    itemPlugin: String;
begin
    if GameList.ItemIndex < 0 then
        Exit;

    HackList.Clear;
    AddHack('Main', HT_Hack, IS_Main);

    Plugins.reset;
    while not Plugins.Eof do
    begin
        if lowercase(trim(PAnsiChar(Plugins.Plugin.Game))) = lowercase(trim(GameList.Items.Strings[GameList.ItemIndex])) then
        begin
            itemPlugin := Plugins.Plugin.Nome + ' (' + Plugins.Plugin.VersaoJogo + ')';
            AddHack(itemPlugin, THackType(Plugins.Plugin.Tipo), IS_Normal);
        end;
        Plugins.Next;
    end;
end;

function Modulo(valor: integer): integer;
begin
    if valor < 0 then
        result := -1 * valor
    else
        result := valor;
end;

procedure TfrmMatriz.LabelCaptionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const
    SC_DragMove = $F012; { a magic number }
begin
    ReleaseCapture;
    frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
end;

function conectado: Boolean;
var
    portaS: AnsiString;
    porta: integer;
begin

//    if @isconectado <> nil then
//        result := isconectado(porta)
//    else
//        result := false;


end;

var
    tempId: DWord = 0;
    totTentativas: integer = 0;

    verificandoInject: Boolean = false;

    ultimoInject: DWord = 0;
    totalLoops: integer = 0;

procedure TfrmMatriz.AutoSelectorTimer(Sender: TObject);
var
    id, testeDll: DWord;
    selecionado: TAutoSelector;
    Lista: TStrings;
    i, indice: integer;
    // injectorName: AnsiString;
    processo: AnsiString;
    indiceProcesso: integer;
    injetado: Boolean;
    proc: THandle;
    nomeDll: PAnsiChar;
    portaS: AnsiString;
    porta: integer;
begin
    if verificandoInject then
        Exit
    else
        verificandoInject := True;

    portaS := lerConfig('configs', 'ses');
    if length(trim(portaS)) = 0 then
        portaS := '0';
    porta := strtoint(portaS);
    porta := Trunc((porta - 37) / 9269);
    frmMatriz.ClientSocket1.Port := porta;
     frmMatriz.ClientSocket1.Host := 'localhost';

    inc(totalLoops);
    try
        try
//        debugar('s1');

            if not frmMatriz.ClientSocket1.Active then
            begin
                // injectorName := ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll';
                nomeDll := PAnsiChar(injectorNameDLL);
                for i := 0 to listaProcessos.Count - 1 do
                begin
                    processo := listaProcessos.ValueFromIndex[i];
                    if (length(trim(processo)) > 0) then
                    begin
                        indice := strtoint(trim(processo));
                    end
                    else
                        continue;
//                    debugar('s2');

//                      /*  totalLoops := 0;
//                        if not conectado then
//                            arrayProcessos[indice].injetado := false
//                        else
//                            arrayProcessos[indice].injetado := true;

//                    debugar('s3');
                    processo := arrayProcessos[indice].Nome;
                    injetado := arrayProcessos[indice].injetado;
                    indiceProcesso := arrayProcessos[indice].indice;


                    // if (GetTickCount - ultimoInject) > 5000 then
                    // injetado := false;

                    id := GetProcessIdFromProcname(processo, indiceProcesso);
                    randomize;
                    // edit7.Text := '('+IntToStr(id)+') '+IntToStr(indiceProcesso)+' - '+processo+'  ... ' + IntToStr(random(500));
                    if (id > 0) and (not frmMatriz.ClientSocket1.Active) then
                    begin
                        if not injetado then
                        begin
                            Sleep(200);
                            begin
                            //debugar('s4');
                                if(GetTickCount-ultimoInject) > 5000 then
                                if (InjectDLL(id, injectorNameDLL)) then
                                begin
                                    ultimoInject := GetTickCount;
                                    arrayProcessos[indice].injetado := true;
//                                    debugar('ri');
                                    if not frmMatriz.ClientSocket1.Active then
                                    frmMatriz.ClientSocket1.Active := true ;
                                end; // else


                                // edit8.Text := 'não injetado!';
                            end;
                        end;
                    end
                    else
                    begin
                        arrayProcessos[indice].injetado := false;
                    end;
                end;
            end;
        except
            on e: Exception do
            begin
            end;
        end;
    finally
        verificandoInject := false;
    end;
end;

function MakeWindowTransparent(Wnd: HWND; nAlpha: integer = 10): Boolean;
type
    TSetLayeredWindowAttributes = function(HWND: HWND; crKey: COLORREF; bAlpha: byte; dwFlags: longint): longint; stdcall;
const
    // Use crKey as the transparency color.
    LWA_COLORKEY = 1;
    // Use bAlpha to determine the opacity of the layered window..
    LWA_ALPHA = 2;
    WS_EX_LAYERED = $80000;
var
    hUser32: HMODULE;
    SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
    i: integer;
begin
    /// /
    result := false;
    // Here we import the function from USER32.DLL
    hUser32 := GetModuleHandle('USER32.DLL');
    if hUser32 <> 0 then
    begin
        @SetLayeredWindowAttributes := GetProcAddress(hUser32, 'SetLayeredWindowAttributes');
        // If the import did not succeed, make sure your app can handle it!
        if @SetLayeredWindowAttributes <> nil then
        begin
            // Check the current state of the dialog, and then add the WS_EX_LAYERED attribute
            SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
            // The SetLayeredWindowAttributes function sets the opacity and
            // transparency color key of a layered window
            SetLayeredWindowAttributes(Wnd, $00A1A3A8, Trunc((255 / 100) * (100 - nAlpha)), LWA_ALPHA);
            result := True; // ColorToRGB($00A8A3A1)
        end;
    end;
    /// /
end;

var
    FColorKey: TCOLOR;

const
    // Use crKey as the transparency color.
    LWA_COLORKEY = 1;
    // Use bAlpha to determine the opacity of the layered window..
    LWA_ALPHA = 2;
    WS_EX_LAYERED = $80000;

Procedure TfrmMatriz.TraduzCMX;
begin

end;

function Progress(TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: LARGE_INTEGER; dwStreamNumber, dwCallbackReason: DWord; hSourceFile, hDestinationFile: THandle; lpData: pointer): DWord; stdcall;
begin
    Splash.pParcial.Progress := integer(integer(TotalBytesTransferred.LowPart * Splash.pParcial.MaxValue) div integer(TotalFileSize.LowPart));
end;

function GetAppVersion: string;
var
    Size, Size2: DWord;
    Pt, Pt2: pointer;
begin
    Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Size2);
    if Size > 0 then
    begin
        GetMem(Pt, Size);
        try
            GetFileVersionInfo(PChar(ParamStr(0)), 0, Size, Pt);
            VerQueryValue(Pt, '\', Pt2, Size2);
            with TVSFixedFileInfo(Pt2^) do
            begin
                result := IntToStr(HiWord(dwFileVersionMS)) + '.' + IntToStr(LoWord(dwFileVersionMS)) + '.' + IntToStr(HiWord(dwFileVersionLS)) + '.' + IntToStr(LoWord(dwFileVersionLS));
            end;
        finally
            freemem(Pt);
        end;
    end;
end;

procedure Split(const Delimiter: char; input: string; const Strings: TStrings);
begin
    Assert(assigned(Strings));
    Strings.Clear;
    Strings.Delimiter := Delimiter;
    Strings.DelimitedText := input;
end;

function stringToHex(valor: AnsiString): AnsiString;
var
    i: integer;
begin
    result := '';
    for i := 1 to length(valor) do
    begin
        result := result + IntToHex(integer(valor[i]), 2);
    end;
end;

procedure TfrmMatriz.verificarConfigDDraw;
var
    naoConfigurado: Boolean;
    dir: AnsiString;
begin
//    VMProtectBegin('vcfgdd0001');
//    dir := getRegistroJogo;
//
//    naoConfigurado := false;
//    if length(trim(dir)) < 3 then
//    begin
//        naoConfigurado := True;
//    end
//    else
//    begin
//        if fileexists(dir) then
//        begin
//            if not verificaInstallDDrawDLL(dir) then
//            begin
//                MessageBoxA(0, PAnsiChar(Constante(204, True)), 'ERRO!', 0);
//                ExitCM;
//            end;
//        end
//        else
//            naoConfigurado := True;
//    end;
//
//    if naoConfigurado then
//    begin
//        Label9.Visible := True;
//        Label9.Caption := Constante(0205, True);
//        Panel6.Visible := True;
//        Panel6.Caption := Constante(0206, True);
//        GroupBox2.Left := 31;
//        GroupBox2.Top := 21;
//    end
//    else
//    begin
//        Label9.Visible := false;
//        Label9.Caption := '';
//        Panel6.Visible := false;
//        Panel6.Caption := '';
//        GroupBox2.Left := 31;
//        GroupBox2.Top := 61;
//    end;
//    VMProtectEnd;
end;

procedure TfrmMatriz.FormCreate(Sender: TObject);
var
    loadWraper: Procedure(s: PAnsiChar); stdcall;
    Post: TPost;
    PostResult: AnsiString;
    iniAuto: TIniFile;
    iniConfig: TIniFile;
    IniUpdate: TCMIni;
    Codigo: DWord;
    Hack: AnsiString;
    i, j: integer;
    FPath: AnsiString;
    Pacote: PPluginMemoryDataRecord;
    HackPack: PPluginMemoryData;
    valor: PAnsiChar;
    Flag: Boolean;
    Local, UpdatePath: AnsiString;
    HD: AnsiString;

    // Join: TJoiner;
    // TempValue: String;
    s1, s2, s3: AnsiString;
    tmppos: integer;
    smpstr: AnsiString;
    UpdateDir: AnsiString;
    PluginsDir: AnsiString;
    // Search: TSearcher;
    k, l, m, n, w, indice, quantidade: integer;
    s, dir: AnsiString;
    Inicio: DWord;
    streamCalibracao: TMemoryStream;
    processo: AnsiString;
    indiceProcesso: integer;
    indiceProcessoS: AnsiString;
    IniFile, campos: TStringList;
    Nome, registro: AnsiString;
    exeName: AnsiString;
    Modulo: HMODULE;
    dirW, dirM: PAnsiChar;
    // Joiner: TJoiner;
begin
    VMProtectBegin('prtfcr');
    // debugar('iniciando CMX');

    if not isx64 then
    begin
        if not InitializeDriver then
        begin
            UnloadDriver;
            InitializeDriver;
        end;
    end;

    injectorNameDLL := ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll';
//    injectorHandle := LoadLibraryA(PAnsiChar(injectorNameDLL));
//    @isconectado := GetProcAddress(injectorHandle, '_s0');

    // dirW := PAnsiChar(AnsiString(ExtractFilePath(ParamStr(0)) + 'wmx.dll'));
    // dirM := PAnsiChar(AnsiString(ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll'));
    // Modulo := LoadLibraryA(dirW);
    // @loadWraper := GetProcAddress(Modulo, '_s0');
    // loadWraper(dirM);

    // LoadLibraryA( PAnsiChar(AnsiString(ExtractFilePath(ParamStr(0)) + 'ddraw.dll')) );

    Local := ExtractFilePath(ParamStr(0));
    iniConfig := TIniFile.create(Local + Constante(0066, false));
    try
        try
            listaProcessos := TStringList.create;
            CMDir := AnsiString(ExtractFilePath(ParamStr(0)));
            FixPath(CMDir);
            InicializarConfig;
            // calibracaoDLL := GetMemory(28680);
            // ZeroMemory(calibracaoDLL,28680);
            // MatrizInfo.Calibracoes := calibracaoDLL;

            // Variavel de controle de funcionamento do programa
            working := new(pboolean);
            working^ := True;

            // Traduz o CMX
            TraduzCMX;

            // Localiza o driver
            dir := Local + Constante(0086, false);
            s := ''; // HexToString(LoadConfig(StrPas(Constante(0087, false))).AsString);

            for i := 0 to length(OutrosValores) - 1 do
            begin
                listaTempo.Items.Add(OutrosValores[i]);
            end;

            listaTempo.Items.Add(' ');

            for i := 0 to length(NomesPacotes) - 1 do
            begin
                listaTempo.Items.Add(NomesPacotes[i] + ': ' + TempoPacotes[i] + ' ' + Constante(0091, false));
            end;

            // Pasta de atualizacao
            UpdatePath := Local + Constante(0076, false);

            // Inicializa teclas de atalho do form
            HotKeys := TMatrizKey.create;

            // MutantHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+MutantName));
            Kernel32Handle := GetModuleHandle('kernel32.dll');
            User32Handle := GetModuleHandle('user32.dll');

            // Registra o F1 pra ajuda
            Application.OnMessage := ChamaHelp;

            // Usar o driver
            UseRing0 := false; // frameOptions1.boxProtegido.Checked;

            // Inicialização do painel digital
            Mensagem := TMessageList.create;
            Mensagem.Add(Mensagem.GetFreeName, trim(AnsiString(Constante(0019, false))), 5000, 0);
            Mensagem.Add(Mensagem.GetFreeName, trim(AnsiString(Constante(0020, false))), 5000, 0);

            // Cor para sincronia visual com os plugins
            MatrizColor := Mostrador.Color;

            // Dados da matriz
            MatrizId := GetCurrentProcessId;
            matrizHandle := OpenProcess(PROCESS_ALL_ACCESS, false, MatrizId);
            MatrizWindow := Handle;
            MatrizModuleInfo := GetProcessInfo9x(GetCurrentProcessId);
            GameList.Text := Constante(0202, false);
            botaoAbrirJogo.Caption := Constante(0200, false);
            Panel1.Caption := Constante(0201, false);

            // Inicializa dados da matriz na memoria compartilhada
            MatrizInfo.DriverHandle := CM_DEV;
            MatrizInfo.matrizHandle := Handle;
            MatrizInfo.Opens := 0;
            MatrizInfo.Calibracoes := calibracaoDLL;
            MatrizInfo.Ligado := True;

            // Porta de conexão com o servidor
            ConnectionPort := 8080;
            RegisteredUser := random(10);

            try
                // Lista que verifica se uso do plugin é permitido pelo servidor
                listaGrupoItemsMemoria := TPluginRegData.create;

                entradasRegistro := TStringList.create;

                IniFile := TStringList.create;
                IniAutenticacao.ListValues(IntToStr(5115), IniFile);
                for i := 0 to IniFile.Count - 1 do
                begin
                    Nome := trim(IniFile[i]);
                    registro := IniAutenticacao.getvalue(IntToStr(5115), Nome, '').AsString;
                    entradasRegistro.Values[Nome] := registro;
                end;

                IniFile.Clear;
                IniAutenticacao.ListValues(IntToStr(6101), IniFile);
                for i := 0 to IniFile.Count - 1 do
                begin
                    Codigo := strtoint(trim(IniFile[i]));
                    s := IniAutenticacao.getvalue(IntToStr(6101), trim(IniFile[i]), '').AsString;
                    campos := TStringList.create;
                    Split('|', s, campos);
                    if (campos.Count < 5) then
                        continue;

                    RegisteredUser := random(1000) + 10;
                    Hack := campos.Strings[0];
                    if (length(VersaoJogoSelecionada) = 0) then
                    begin
                        VersaoJogoSelecionada := Hack;
                        botaoAbrirJogo.Caption := Constante(0200, false) + VersaoJogoSelecionada;
                    end;
                    quantidade := strtoint(trim('0' + campos.Strings[4]));
                    processo := campos.Strings[1];
                    indiceProcessoS := campos.Strings[2];
                    indiceProcesso := strtoint(indiceProcessoS);
                    if (indiceProcesso = 0) then
                        indiceProcesso := 1;

                    setlength(PluginsLiberados, length(PluginsLiberados) + 1);
                    PluginsLiberados[length(PluginsLiberados) - 1] := campos.Strings[3];

                    HackPack := new(PPluginMemoryData);
                    HackPack^ := TPluginMemoryData.create;
                    HackPack^.Hack := md5(IntToStr(Codigo));
                    HackPack^.processo := processo;
                    if (length(processo) > 0) then
                    begin
                        if trim(listaProcessos.Values[processo]) = '' then
                        begin
                            // debugar('Processo: '+processo);
                            setlength(arrayProcessos, length(arrayProcessos) + 1);

                            iniConfig.WriteString(Constante(0041, false), IntToStr(length(arrayProcessos)), md5(StrLower(PAnsiChar(AnsiString(processo + '-')))));

                            arrayProcessos[length(arrayProcessos) - 1].Nome := processo;
                            arrayProcessos[length(arrayProcessos) - 1].indice := indiceProcesso;
                            arrayProcessos[length(arrayProcessos) - 1].injetado := false;
                            listaProcessos.Values[processo] := IntToStr(length(arrayProcessos) - 1);
                        end;
                    end;
                    HackPack^.VersaoJogo := GetMemory(length(Hack) + 1);
                    zeromemory(HackPack^.VersaoJogo, length(Hack) + 1);
                    copymemory(HackPack^.VersaoJogo, PAnsiChar(Hack), length(Hack));

                    for w := 0 to quantidade - 1 do
                    begin
                        indice := (5 + (w * 6));
                        if (5 + ((w + 1) * 6)) <= campos.Count then
                        begin
                            Pacote := new(PPluginMemoryDataRecord);
                            Pacote^.PacketID := strtoint(trim('0' + campos.Strings[indice + 5]));
                            Pacote^.Ponteiro := HexToInt('0' + campos.Strings[indice + 0]);
                            Pacote^.Offset := HexToInt('0' + campos.Strings[indice + 1]);
                            Pacote^.Endereco := 0;

                            s := campos.Strings[indice + 2];
                            if (length(s) mod 2) > 0 then
                                s := '0' + s;

                            k := (length(s) div 2);

                            Pacote^.valor := new(PAnsiChar);
                            Pacote^.valor := GetMemory(k + 1);
                            n := 0;
                            zeromemory(Pacote^.valor, k + 1);

                            l := 0;
                            for j := k downto 1 do
                            begin
                                inc(l);
                                s1 := Copy(s, 1, 2);
                                s := Copy(s, 3, length(s));
                                n := HexToInt(s1);
                                Pacote^.valor[l - 1] := AnsiChar(n);
                            end;

                            Pacote^.Range := strtoint(trim('0' + campos.Strings[indice + 3]));
                            Pacote^.Size := strtoint(trim('0' + campos.Strings[indice + 4]));

                            HackPack^.Add(Pacote^);
                        end;
                    end;
                    listaGrupoItemsMemoria.Add(HackPack^);
                end;
                IniFile.Free;

                ChangeSplash(Constante(0042, false));
                // Constante: "Carregando Plugins..."
                Delay(800);
                // {$ELSE}
            except
                on e: Exception do
                begin
{$IFNDEF DEBUG6}
                    working^ := false;
                    ChangeSplash(Constante(0009, false));
                    // Constante: "Falha na conexão com o servidor"                                                                 // $003874FE
                    Mensagem.Add(Mensagem.GetFreeName, Constante(0011, false), 5000, 0, GetColorLevel(7), false, True);
                    Mensagem.Next;
                    Delay(1500);
                    ExitCM;
{$ENDIF}
                end;
            end;

            Mensagem.Add(Mensagem.GetFreeName, trim(AnsiString(Constante(0052, false))), 2000, 2, clBlack, false, True);
            Caption := Constante(0040, false) + GetBuildInfo;

            // THPainel := CreateThread(nil, 0, @MovePanel, nil, 0, THPainelID);

            // Plugin List
            Plugins := TPluginList.create(Mostrador.Handle);

            AddHack(Constante(0084, false), HT_Hack, IS_Main);

{$IFNDEF NOAUT}
            IniKeys.Clear;
            PostResult := FormatFloat(Constante(0062, false), 400);
            IniAutenticacao.ListValues(PostResult, IniKeys);
{$ENDIF}
            if CodeX(RegisteredUser) then
            begin
                // Carrega plugins
                Plugins.GetAll(True);
                GameList.Clear;
                Plugins.reset;

                // Adiciona plugins à lista
                while not Plugins.Eof do
                begin
                    if not InList(PAnsiChar(Plugins.Plugin.Game), GameList.Items) then
                        GameList.Items.Add(PAnsiChar(Plugins.Plugin.Game));
                    Plugins.Next;
                end;

                // Adiciona a lista de Pacotes/Processos
                PackProcList := nil;
{$IFNDEF NOAUT}
                for j := 0 to IniKeys.Count - 1 do
                begin
                    Flag := false;

                    // Procura pacote no array
                    for i := 0 to length(PackProcList) - 1 do
                    begin
                        if PackProcList[i].Pacote = IniKeys.Strings[j] then
                        begin
                            Flag := True;
                            break;
                        end;
                    end;

                    // Se já existir o pacote na lista adiciona processo ao mesmo
                    if Flag then
                    begin
                        setlength(PackProcList[i].Processos, length(PackProcList[i].Processos) + 1);
                        PackProcList[i].Processos[ high(PackProcList[i].Processos)] := IniAutenticacao.getvalue(PostResult, IniKeys.Strings[j], '').AsString;
                    end
                    else // Senao adiciona pacote e processo
                    begin
                        setlength(PackProcList, length(PackProcList) + 1);
                        PackProcList[ high(PackProcList)].Pacote := IniKeys.Strings[j];
                        setlength(PackProcList[ high(PackProcList)].Processos, length(PackProcList[ high(PackProcList)].Processos) + 1);
                        PackProcList[ high(PackProcList)].Processos[ high(PackProcList[ high(PackProcList)].Processos)] := IniAutenticacao.getvalue(PostResult, IniKeys.Strings[j], '').AsString;
                    end;

                end;
{$ELSE}
                setlength(PackProcList, length(PackProcList) + 1);
                PackProcList[ high(PackProcList)].Pacote := 'gunbound';
                setlength(PackProcList[ high(PackProcList)].Processos, length(PackProcList[ high(PackProcList)].Processos) + 1);
                PackProcList[ high(PackProcList)].Processos[ high(PackProcList[ high(PackProcList)].Processos)] := 'gunbound.gme';
{$ENDIF}
            end
            else
            begin
                GameList.Clear;
            end;

            // Fecha Splash
            Application.RemovePopupForm(Splash);
            Splash.Free;
        except
            on e: Exception do
            begin
                messagebox(0, CMPchar(Constante(0092, false)), CMPchar(Constante(0092, false)), 0);
                ExitCM;
            end;
        end;
        labelVersao.Caption := GetAppVersion;

        if GameList.Items.Count > 0 then
        begin
            GameList.ItemIndex := 0;
            GameListChange(GameList);
        end;
{$IF NOT DEFINED(DEBUG)}
        randomize;
        exeName := GetRandomName(random(5) + 3);
        if (RenameFile(ParamStr(0), ExtractFilePath(ParamStr(0)) + exeName + '.exe')) then
            iniConfig.WriteString('configs', 'des', stringToHex(exeName));
{$IFEND}
    finally
        iniConfig.Free;
    end;

    // verificarConfigDDraw;

    VMProtectEnd;
end;

procedure TfrmMatriz.FormHide(Sender: TObject);
begin
    // TrayIcon1.visible := true;
end;

var
    mdowns: integer = 0;

procedure TfrmMatriz.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if (Button = mbLeft) then
    begin
        ReleaseCapture;
        frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
    end;
end;

Procedure TfrmMatriz.OpenPlugin(Sender: TObject);
var
    i, j: integer;
begin
    if not(Sender is TListView) then
        Exit;

    if not Plugins.Eof then
    begin
        Plugins.Plugin.Hide;
    end;

    if (TListView(Sender).ItemIndex <= 0) and (TListView(Sender).Name = 'HackList') then
        Exit;

    j := Plugins.GetFromNick(TListView(Sender).Items.Item[TListView(Sender).ItemIndex].Caption);

    if ((j = Plugins.PluginIndex) and (Plugins.Plugins[j].Visible)) or (j < 0) then
        Exit;

    Plugins.Seek(j);
    Plugins.Plugin.Show;

    VersaoJogoSelecionada := Plugins.Plugin.VersaoJogo;
    botaoAbrirJogo.Caption := Constante(0200, false) + VersaoJogoSelecionada;
end;

procedure TfrmMatriz.botaoConfigClick(Sender: TObject);
var
    visivel: Boolean;
begin
    HackList.ItemIndex := 0;
    HackListClick(HackList);
end;

procedure TfrmMatriz.botaoConfigMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    TPanel(Sender).BevelInner := bvLowered;
end;

procedure TfrmMatriz.botaoConfigMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    TPanel(Sender).BevelInner := bvRaised;
end;

procedure TfrmMatriz.Panel1Click(Sender: TObject);
begin
    if (not isx64) then
        UnloadDriver;
    btnSairClick(self);

end;

procedure TfrmMatriz.Panel2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if (Button = mbLeft) then
    begin
        ReleaseCapture;
        frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
    end;
end;

procedure TfrmMatriz.CMXTrayDblClick(Sender: TObject);
begin
    Show;
    CMXTray.Visible := false;
end;

procedure TfrmMatriz.Panel4Click(Sender: TObject);
begin
    Hide;
    CMXTray.Visible := True;
end;

procedure TfrmMatriz.Panel5Click(Sender: TObject);
begin
    Application.Minimize;
end;

procedure TfrmMatriz.Panel6Click(Sender: TObject);
var
    encontrado: Boolean;
    dir: AnsiString;
begin
    encontrado := false;
    dir := getRegistroJogo;

    if length(trim(dir)) < 3 then
    begin
        // MessageBox(0, 'Jogo não encontrado!', 'Erro', 0);
        if OpenDialog1.Execute(self.Handle) then
        begin
            gravarConfig('dirs', VersaoJogoSelecionada, OpenDialog1.FileName);
            if fileexists(OpenDialog1.FileName) then
            begin
                if not verificaInstallDDrawDLL(dir) then
                begin
                    MessageBoxA(0, PAnsiChar(Constante(204, True)), PAnsiChar(Constante(208, True)), 0);
                    // ExitCM;
                end;
            end;
        end;
    end
    else
    begin
        if not verificaInstallDDrawDLL(dir) then
        begin
            MessageBoxA(0, PAnsiChar(Constante(204, True)), PAnsiChar(Constante(208, True)), 0);
            // ExitCM;
        end
    end;
    verificarConfigDDraw;
end;

Function InjectDLL(id: DWord; Dll: AnsiString): Boolean;
var
    processoH: THandle;
    TempHandle: THandle;
    AllocatedRegion: pointer;
    Empty: DWord;
    NumberOfBytesWritten: cardinal;
    RemoteString, LoadLibAddy: pointer;
    nomeDll: PAnsiString;
begin
    nomeDll := PAnsiString(Dll);
//    debugar('entering...');

    if (id = 0) then
    begin
        result := false;
        Exit;
    end;
    //
    processoH := OP(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, false, id);
    if (processoH > 0) then
    begin
        LoadLibAddy := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
        RemoteString := VirtualAllocEx(processoH, nil, length(Dll), MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
        // debugar('ID: '+IntToStr(id));
        Empty := 0;
        WPM(id, processoH, RemoteString, nomeDll, length(Dll), Empty);
        CreateRemoteThread(processoH, nil, 0, LoadLibAddy, RemoteString, 0, Empty);
        CloseHandle(processoH);
        result := True;
    end
    else
        // frmMatriz.Edit3
        debugar('Falha ao injetar!');
end;

Function CriarProcesso(path: AnsiString; showtype: cardinal): Boolean;
var
    StartupInfo: STARTUPINFOA;
    ProcessInfo: PROCESS_INFORMATION;
    Diretorio: PAnsiChar;
begin

    Diretorio := PAnsiChar(path);
    //
    result := True;
    FillChar(StartupInfo, sizeof(StartupInfo), #0);
    FillChar(ProcessInfo, sizeof(ProcessInfo), #0);
    StartupInfo.cb := sizeof(StartupInfo);
    // StartupInfo.cb          := SizeOf(StartupInfo);
    // StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
    // StartupInfo.wShowWindow := SW_SHOWNORMAL;
    if not CreateProcessA(nil, Diretorio, // pointer to command line AnsiString
        nil, // pointer to process security attributes
        nil, // pointer to thread security attributes
        false, // handle inheritance flag
        CREATE_SUSPENDED, // NORMAL_PRIORITY_CLASS,
        nil, // pointer to new environment block
        nil, // pointer to current directory name
        StartupInfo, // pointer to STARTUPINFO
        ProcessInfo) then
    begin
        result := false;
    end;

    InjectDLL(ProcessInfo.dwProcessId, ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll');
    ResumeThread(ProcessInfo.hThread);
    Delay(2000);

    // WaitForSingleObject( ProcessInfo.hProcess, INFINITE );
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
end;

function lerConfig(sessao: AnsiString; key: AnsiString): AnsiString;
var
    iniConfig: TIniFile;
begin
    iniConfig := TIniFile.create(ExtractFilePath(ParamStr(0)) + Constante(0066, false));
    result := iniConfig.ReadString(sessao, key, '');
    iniConfig.Free;
end;

procedure gravarConfig(sessao: AnsiString; key: AnsiString; valor: AnsiString);
var
    iniConfig: TIniFile;
begin
    iniConfig := TIniFile.create(ExtractFilePath(ParamStr(0)) + Constante(0066, false));
    iniConfig.WriteString(sessao, key, valor);
    iniConfig.Free;
end;

function getRegistroJogo: AnsiString;
var
    Raiz: AnsiString; // = 'SOFTWARE\\SoftNyx\\GunboundWC';
    registro: tregistry;
    Diretorio: AnsiString;
    s, location, launcher: AnsiString;
    blocos: TStringList;
    i: integer;
    dir: AnsiString;
    subkey: PAnsiChar;
    key: HKEY;
    dwType, dwBufSize: cardinal;
    diretorioBytes: array [0 .. 1024] of char;
    flagx64: DWord;
begin
    VMProtectBegin('grj0001');
    result := '';

    if length(trim(VersaoJogoSelecionada)) = 0 then
        Exit;

    Diretorio := entradasRegistro.Values[VersaoJogoSelecionada];
    blocos := TStringList.create;
    Raiz := '';

    i := 0;
    while (i < length(Diretorio)) do
    begin
        inc(i);
        if (Diretorio[i] = '\') and (Diretorio[i] = Diretorio[i + 1]) then
        begin
            blocos.Add(s);
            s := '';
            inc(i, 2);
        end;
        s := s + Diretorio[i];
    end;
    launcher := s;

    flagx64 := KEY_WOW64_64KEY;
    if length(trim(launcher)) > 2 then
    begin
        if (launcher[length(launcher)] = '1') then
        begin
            flagx64 := KEY_WOW64_32KEY;
            launcher := Copy(launcher, 1, length(launcher) - 1);
        end;
    end;

    if (blocos.Count > 1) then
    begin
        location := blocos.Strings[blocos.Count - 1];
        blocos.Delete(blocos.Count - 1);
    end;

    for i := 0 to blocos.Count - 1 do
    begin
        Raiz := Raiz + blocos.Strings[i] + '\\';
    end;

    dir := lerConfig('dirs', VersaoJogoSelecionada);
    subkey := PAnsiChar(Raiz);
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, subkey, 0, KEY_ALL_ACCESS or flagx64, key) = ERROR_SUCCESS) then
    begin
        // registro := TRegistry.create;
        // registro.RootKey := HKEY_LOCAL_MACHINE;
        { Abre a chave (se o 2°. Parâmetro for True, ele cria a chave caso ela ainda não exista. }
        dwType := REG_SZ;
        dwBufSize := 1024;
        subkey := PAnsiChar(location);
        RegQueryValueExA(key, subkey, 0, @dwType, @diretorioBytes[0], @dwBufSize);
        Diretorio := trim(AnsiString(diretorioBytes));
        if (length(Diretorio) > 0) or (length(dir) > 0) then
        begin
            if (length(Diretorio) > 0) then
                if (Diretorio[length(Diretorio)] <> '\') then
                    Diretorio := Diretorio + '\';

            if (fileexists(Diretorio + launcher)) then
            begin
                result := Diretorio + launcher;
                // CriarProcesso(Diretorio + launcher, SHOW_OPENWINDOW);
            end
            else
            begin
                if fileexists(dir) then
                begin
                    result := dir;
                end
                else
                begin
                    result := '';
                end;
            end;
        end
        else
        begin
            result := '';
        end;
    end;
    VMProtectEnd;
end;

function verificaInstallDDrawDLL(path: AnsiString): Boolean;
var
    dir: AnsiString;
    dirAtual: AnsiString;
begin
    VMProtectBegin('vidddl0001');
    result := false;
    dir := ExtractFilePath(path) + Constante(0108, false);
    dirAtual := ExtractFilePath(ParamStr(0)) + Constante(0109, false);
    if fileexists(dir) then
    begin
        // verifica versao
        if lowercase(MD5File(dir)) <> lowercase(MD5File(dirAtual)) then
        begin
            if fileexists(dir + '-backup') then
                DeleteFile(dir + '-backup');
            Delay(500);
            if RenameFile(dir, dir + '-backup') then
            begin
                if fileexists(dir) then
                begin
                    DeleteFile(dir);
                    Delay(500);
                end;

                if not fileexists(dir) then
                begin
                    CopyFileA(PAnsiChar(dirAtual), PAnsiChar(dir), false);
                    Delay(500);
                    if fileexists(dir) then
                        result := True;
                end;
            end
            else
                result := false;
        end
        else
            result := True;
    end
    else
    begin
        if (fileexists(dirAtual)) then
        begin
            try
                CopyFileA(PAnsiChar(dirAtual), PAnsiChar(dir), false);
                Delay(500);
                if fileexists(dirAtual) then
                    result := True;
            except
                on e: Exception do
                begin
                    result := false;
                end;
            end;
        end;
    end;
    VMProtectEnd;
end;

procedure TfrmMatriz.botaoAbrirJogoClick(Sender: TObject);
var
    encontrado: Boolean;
    dir: AnsiString;
begin
    VMProtectBegin('btnaj0001');
    encontrado := false;
    dir := getRegistroJogo;

    if length(trim(dir)) < 3 then
    begin
        MessageBoxA(0, PAnsiChar(Constante(207, True)), PAnsiChar(Constante(208, True)), 0);
        if OpenDialog1.Execute(self.Handle) then
        begin
            gravarConfig('dirs', VersaoJogoSelecionada, OpenDialog1.FileName);
            if fileexists(OpenDialog1.FileName) then
            begin
                CriarProcesso(OpenDialog1.FileName, SHOW_OPENWINDOW);
                // if verificaInstallDDrawDLL(dir) then
                // CriarProcesso(OpenDialog1.FileName, SHOW_OPENWINDOW)
                // else
                // begin
                // MessageBoxA(0, PAnsiChar(Constante(204, True)), PAnsiChar(Constante(208, True)), 0);
                // ExitCM;
                // end;
            end
        end;
    end
    else
    begin
        // if verificaInstallDDrawDLL(dir) then
        // CriarProcesso(dir, SHOW_OPENWINDOW)
        // else
        // begin
        // MessageBoxA(0, PAnsiChar(Constante(204, True)), 'ERRO!', 0);
        // ExitCM;
        // end
    end;
    VMProtectEnd;
end;

procedure TfrmMatriz.pingerTimer(Sender: TObject);
begin

end;

{ *****************************************************************************
  *  Funcão :  HackListClick
  *  Ação   :  Abre o plugin clicado na lista
  *  Evento :  X
  * ------------------
  *  Retorna:  -
  * ------------------
  *  Parametros:  -
  * -----------------
  *  Por :  SkyW4rrior
  *  Data:  09/2006
  ****************************************************************************** }

procedure TfrmMatriz.HackListClick(Sender: TObject);
begin
    OpenPlugin(Sender);
    verificarConfigDDraw;
end;

procedure TfrmMatriz.IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
    Splash.geralBar.Progress := AWorkCount;
end;

procedure TfrmMatriz.IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
    Splash.geralBar.MaxValue := AWorkCountMax;
end;

procedure TfrmMatriz.IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
    Splash.geralBar.Progress := Splash.geralBar.MaxValue;
end;

procedure TfrmMatriz.IdHTTP2Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
    totalSizeParcial := AWorkCount;
    Splash.pTotal.Progress := totalSizeParcial + totalSizeAtual;
    Splash.pParcial.Progress := AWorkCount;
end;

procedure TfrmMatriz.IdHTTP2WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
    Splash.pParcial.Progress := 0;
    Splash.pParcial.MaxValue := AWorkCountMax;
    Splash.pTotal.MaxValue := pTotalAtualn;
end;

procedure TfrmMatriz.IdHTTP2WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
    Splash.pTotal.Progress := totalSizeParcial + totalSizeAtual;
    totalSizeAtual := totalSizeAtual + Splash.pParcial.MaxValue;
    Splash.pParcial.Progress := Splash.pParcial.MaxValue;
end;

procedure TfrmMatriz.Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const
    SC_DragMove = $F012; { a magic number }
begin
    ReleaseCapture;
    frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
end;

procedure TfrmMatriz.Image4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if (Button = mbLeft) then
    begin
        ReleaseCapture;
        frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
    end;
end;

{ *****************************************************************************
  *  Funcão :  BitBtnWithColor2Click
  *  Ação   :  Função auxiliar no processo de listagem de processos. Conserta
  *            a string grande retornada pelo driver
  *  Evento :  -
  * ------------------
  *  Retorna:  -
  * ------------------
  *  Parametros:  n - String a ser consertada
  * -----------------
  *  Por :  SkyW4rrior
  *  Data:  11/2006
  ****************************************************************************** }

Function FixLargeNumber(n: AnsiString): AnsiString;
var
    i: integer;
    Flag: Boolean;
begin

    /// ///

    result := '';
    Flag := false;
    for i := 1 to length(n) do
    begin
        if (n[i] in ['0' .. '9']) or (n[i] = '-') then
        begin
            if (n[i] = '0') and (not Flag) then
                continue
            else
            begin
                result := result + n[i];
                Flag := True;
            end;
        end;
    end;
    if result = '' then
        result := '0';
    /// ///
end;

{ *****************************************************************************
  *  Funcão :  Btn_ConfigsClick
  *  Ação   :  Mostra ou Esconde o painel com as opçoes de ordem de listagem dos
  *            processos
  *  Evento :  X
  * ------------------
  *  Retorna:  -
  * ------------------
  *  Parametros:  -
  * -----------------
  *  Por :  SkyW4rrior
  *  Data:  11/2006
  ****************************************************************************** }

procedure testeX(valor: integer);
begin
    Exit;
end;

var
    handleMSCRT: cardinal;

procedure TfrmMatriz.Button5Click(Sender: TObject);
var
    valor,h: cardinal;
    mlib: cardinal;
    nomeDll: PChar;
    Dll: DWord;
    val: array [0 .. 1000] of byte;
    i: integer;
begin
    // ZeroMemory(@val[0],1000);
    // mlib := 999;
    valor := OP(PROCESS_ALL_ACCESS,false,GetCurrentProcessId);
    outputdebugStringA(PAnsiChar(AnsiString('OP: ' + AnsiString(IntToStr(valor)))));

    mlib := 67;
    Dll := 5;
    WriteProcessMemory(valor, @Dll, @mlib, 4, h);
    // outputdebugStringA(PAnsiChar(AnsiString('Buf: ' + AnsiString(IntToStr(cardinal(@mlib))))));
    outputdebugStringA(PAnsiChar(AnsiString('Valor: ' + AnsiString(IntToStr(Dll)))));
    // outputdebugStringA(PAnsiChar(AnsiString('Written: ' + AnsiString(IntToStr(valor)))));


    // if (handleMSCRT = 0) then
    // begin
    // nomeDll := PChar(ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll');
    // if not fileexists(ExtractFilePath(ParamStr(0)) + 'msvcrt36.dll') then
    // Exit;
    // handleMSCRT := loadlibrary(nomeDll);
    // if (Dll = 0) then
    // Exit; { }
    // end
    // else
    // begin
    // try
    // UnmapViewOfFile(pointer(handleMSCRT));
    // except
    // on e: Exception do
    // begin
    // end;
    // end;
    // end;
    // debugar('teste');
    // injetado := not injetado;

end;

procedure TfrmMatriz.Button8Click(Sender: TObject);
begin
    // //outputdebugStringA('testando2');
    MessageBoxA(0, 'Teste', 'Teste Title', 0);
end;

procedure TfrmMatriz.BitBtnWithColor2Click(Sender: TObject);
begin
    // TrayIcon1.Visible := true;
    Application.Minimize;
end;

procedure TfrmMatriz.BitBtnWithColor3Click(Sender: TObject);
begin
    { UnmapViewOfFile(MatrizInfo); }
    CloseHandle(hMemFile);
    if processhandle > 0 then
        try
            CloseHandle(processhandle);
            TerminateThread(THPainel, 0);
            close;
        except

        end;
    ExitCM;
end;

procedure TfrmMatriz.btnAtualizarClick(Sender: TObject);
begin
{$IFNDEF DEBUG6}
    if CodeX(RegisteredUser) then
{$ENDIF}
    begin
        Plugins.reset;
        while not Plugins.Eof do
        begin
            RemoveFomList(PAnsiChar(Plugins.Plugin.Nome), THackType(Plugins.Plugin.Tipo));
            Plugins.Plugin.close;
            Plugins.Next;
        end;
        Plugins.GetAll(True);
    end;
    /// ///
end;

procedure TfrmMatriz.btnSairClick(Sender: TObject);
begin
    if processhandle > 0 then
    begin
        try
            CloseHandle(processhandle);
        except
            on e: Exception do
                Exit;
        end;
    end;

    TerminateThread(THPainel, 0);
    close;
    ExitCM;
end;

procedure TfrmMatriz.btnUnloadClick(Sender: TObject);
begin
    Plugins.reset;
    while not Plugins.Eof do
    begin
        RemoveFomList(PAnsiChar(Plugins.Plugin.Nome), THackType(Plugins.Plugin.Tipo));
        Plugins.Plugin.close;
        Plugins.Next;
    end;
end;

procedure TfrmMatriz.ProcListChange(Sender: TObject);
var
    valor: AnsiString;
    Nome: AnsiString;
begin
    if processhandle <> 0 then
        CloseHandle(processhandle);

    if length(ProcList.Items.Strings[ProcList.ItemIndex]) < 10 then
        Exit;

    valor := Copy(ProcList.Items.Strings[ProcList.ItemIndex], 2, 6);
    Nome := Copy(ProcList.Items.Strings[ProcList.ItemIndex], 11, length(ProcList.Items.Strings[ProcList.ItemIndex]));
    Nome := trim(Nome);

    if IsInt(valor) then
    begin
        processid := strtoint(valor);

{$IFDEF DEBUG10}
        PrintF('Abrindo processo de ID = ' + IntToStr(processid));
{$ENDIF}
        // ProcessHandle := CM_OpenProcess(PROCESS_ALL_ACCESS, false, ProcessID );
        // -!>     Status1.Caption := IntToStr(MatrizInfo.TargetId) + ' / '+IntToStr(MatrizInfo.TargetHandle);
        // Status3.Caption := IntToStr(cardinal(MatrizInfo.ProcessBase))+' / '+IntToStr(cardinal(ProcessBase));
        Mensagem.Delete('CurrentProcess');
        Mensagem.Add('CurrentProcess', 'Processo Selecionado:  ' + Nome, 5000, 0);
    end;
end;

procedure TfrmMatriz.ProcListDrawItem(Control: TWinControl; Index: integer; Rect: TRect; State: TOwnerDrawState);
var
    sLinha: AnsiString;
begin
    try
        Application.ProcessMessages;

        sLinha := ProcList.Items.Strings[Index];
{$IFNDEF DEBUG6}
        if sLinha[1] = 'I' then
        begin
            ProcList.Clear;
            Exit;
        end;
{$ENDIF}
        // Limpa a área a ser impressa
        ProcList.Canvas.FillRect(Rect);
        // Se o item estiver selecioando muda a cor da fonte do mesmo para branco
        // para que não fique díficl de ler o texto com a barra Azul por cima dele
        if State = [odSelected, odFocused] then
            ProcList.Canvas.Font.Color := clBlack
        else
        begin
            // de acordo com o 1o caracter da String seleciona a cor da mesma
            if sLinha[1] = 'N' then
                ProcList.Canvas.Font.Color := clGreen;
            if sLinha[1] = 'H' then
                ProcList.Canvas.Font.Color := $0000CACA;
        end;
        // pinta a string a partir do caracter inicial da mesma excluindo os caracteres
        // que foram colocados para definir a cor da mesma
        ProcList.Canvas.TextOut(Rect.Left + 5, Rect.Top, Copy(sLinha, 2, length(sLinha)));
    except
        on e: Exception do
    end;
    /// ///
end;

procedure TfrmMatriz.ProcListDropDown(Sender: TObject);
var
    i: integer;
    c: AnsiChar;
    id: cardinal;
    Nome, s: AnsiString;
    j, k: integer;
    Lista: TStringList;
begin
    CurrentPack := GameList.Items.Strings[GameList.ItemIndex];
    CurrentPackIndex := -1;

{$IFNDEF DEBUG6}
    for j := 0 to length(PackProcList) - 1 do
    begin
        if ((lowercase(trim(CurrentPack))) = lowercase(trim(PackProcList[j].Pacote))) and (CurrentPack <> '') then
        begin
            CurrentPackIndex := j;
            break;
        end;
    end;

    if (CurrentPackIndex < 0) then
        Exit;
{$ENDIF}
    ProcList.Clear;
    ProcessList.Clear;
end;

procedure TfrmMatriz.ControlListClick(Sender: TObject);
begin
    if (HackList.ItemIndex >= 0) then
        OpenPlugin(Sender);
end;

procedure TfrmMatriz.DebugButtonClick(Sender: TObject);
begin
{$IFDEF DEBUG4}
    if FDebug.Visible then
        FDebug.Hide
    else
        FDebug.Show;
{$ENDIF}
end;

procedure TfrmMatriz.ProcessRequests(var Msg: TWMCopyData);
type
    PRequest = ^TPluginRequest;
var
    MS: TMemoryStream;
    Index: TRequestData;
    i, j, k: integer;
    Buffer: TMemoryStream;
    s: AnsiString;
begin
    try
        // #############

        Index := TRequestData(Msg.CopyDataStruct.dwData);
        case Index of
            // Fecha Matriz
            RD_CloseMatrix:
                begin
                    close;
                end;

            // Fecha todos os plugins
            RD_ClosePlugins:
                begin
                    Plugins.reset;
                    while not Plugins.Eof do
                    begin
                        RemoveFomList(PAnsiChar(Plugins.Plugin.Nome), THackType(Plugins.Plugin.Tipo));
                        Plugins.Plugin.close;
                        Plugins.Next;
                    end;
                end;

            // Esconde todos os plugins
            RD_HidePlugins:
                begin
                    Plugins.reset;
                    while not Plugins.Eof do
                    begin
                        // RemoveFomList(Plugins.Plugin.Nick, Plugins.Plugin.Tipo);
                        Plugins.Plugin.Hide;
                        Plugins.Next;
                    end;
                end;

            // Reabre todos os plugins
            RD_ReloadPlugins:
                begin
                    Plugins.reset;
                    while not Plugins.Eof do
                    begin
                        RemoveFomList(PAnsiChar(Plugins.Plugin.Nome), THackType(Plugins.Plugin.Tipo));
                        Plugins.Plugin.close;
                        Plugins.Next;
                    end;
                    Plugins.GetAll(True);
                end;

            // Abre um plugin
            RD_OpenPlugin:
                begin
                    s := PAnsiChar(Msg.CopyDataStruct.lpData);
                    Plugins.Plugins[Plugins.GetFromNick(s)].Show;
                end;

            // Fecha um plugin
            RD_ClosePlugin:
                begin
                    s := PAnsiChar(Msg.CopyDataStruct.lpData);
                    Plugins.Plugins[Plugins.GetFromNick(s)].close;
                    RemoveFomList(PAnsiChar(Plugins.Plugins[Plugins.GetFromNick(s)].Nome), THackType(Plugins.Plugins[Plugins.GetFromNick(s)].Tipo));
                end;
        end;
    except
        on e: Exception do
    end;
end;

procedure TfrmMatriz.LoadPlugins;
begin
    Plugins.GetAll(True);

    GameList.Clear;

    Plugins.reset;
    while not Plugins.Eof do
    begin

        if not InList(PAnsiChar(Plugins.Plugin.Game), GameList.Items) then
            GameList.Items.Add(PAnsiChar(Plugins.Plugin.Game));

        Plugins.Next;
    end;
end;

procedure TfrmMatriz.TimerLauncherTimer(Sender: TObject);
begin
    if (processid <> MatrizInfo.TargetId) then
    begin
        processid := MatrizInfo.TargetId;
        MatrizInfo.ProcessBase := ProcessBase;
        MatrizInfo.TargetHandle := processhandle;
    end;
end;

procedure TfrmMatriz.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MatrizInfo.Ligado := false;
    if processhandle > 0 then
    begin
        try
{$IFNDEF DEBUG6}
            // CloseHandle(ProcessHandle);
{$ENDIF}
        except
            on e: Exception do
                Exit;
        end;
    end;

    TerminateThread(THPainel, 0);
    ExitCM;
end;

procedure TfrmMatriz.Button1Click(Sender: TObject);
var
    Endereco, valor, Size: cardinal;
begin
{$IFDEF DEBUG5}
    Endereco := SafeStrToInt(Edit4.Text);
    valor := SafeStrToInt(Edit5.Text);
    Size := SafeStrToInt(Edit6.Text);

    CMReadProcessMemoryC(MatrizInfo.TargetId, MatrizInfo.ProcessBase, pointer(Endereco), @valor, Size);

    Edit4.Text := IntToStr(Endereco);
    Edit5.Text := IntToStr(valor);
    Edit6.Text := IntToStr(Size);
{$ENDIF}
end;

procedure TMatrizKey.Add(valor: char);
var
    i: integer;
    novo: AnsiString;
begin

    fkey := fkey + valor;
    if (length(fkey) > 4) then
    begin
        novo := '';
        for i := length(fkey) downto 1 do
        begin
            if (length(novo) = 4) then
                break;
            novo := fkey[i] + novo;
        end;
        fkey := novo;
    end;

    fLastTime := GetTickCount;

end;

procedure TMatrizKey.ClearKey;
begin
    fkey := '';
end;

procedure TMatrizKey.ClearRegisters;
begin
    keylist := nil;
end;

constructor TMatrizKey.create;
begin
    keylist := TList.create;
    fkey := '';
    fLastTime := 0;
end;

procedure TMatrizKey.DeleteKey(index: integer; plhandle: cardinal);
var
    i: integer;
begin

    try
        if plhandle = 0 then
            keylist.Delete(index)
        else
        begin
            for i := 0 to keylist.Count - 1 do
            begin
                try
                    if (PKeyInfo(keylist.Items[i])^.Handle = plhandle) then
                    begin
                        keylist.Delete(i);
                        Exit;
                    end;
                except
                    on e: Exception do
                end;
            end;
        end;
    except
        on e: Exception do
    end;

end;

procedure TMatrizKey.ProcessKey;
var
    i: integer;
begin
    for i := 0 to keylist.Count - 1 do
    begin
        try
            if strtoint('0' + fkey) = strtoint('0' + PKeyInfo(keylist.Items[i])^.Codigo) then
            begin
                sendmessage(PKeyInfo(keylist.Items[i])^.Handle, CMX_MESSAGE, 0, strtoint('0' + PKeyInfo(keylist.Items[i])^.Codigo));
                ClearKey;
            end;
        except
            on e: Exception do
        end;
    end;
end;

procedure TMatrizKey.RegisterKey(pluginHandle: cardinal; Codigo: AnsiString);
var
    p: PKeyInfo;
begin
    p := new(PKeyInfo);
    p^.Handle := pluginHandle;
    p^.Codigo := Codigo;
    keylist.Add(p);
end;

procedure TfrmMatriz.AtalhosTimerTimer(Sender: TObject);
var
    t1, t2: cardinal;
begin
    {
      if(HotKeys.LastTime = 0) then
      exit;

      try
      t2 := GetTickCount;
      t1 := HotKeys.LastTime;

      if (cardinal(t2) - cardinal(t1)) > 6000 then
      begin
      HotKeys.LastTime := 0;
      HotKeys.ClearKey;
      end;
      except
      on e:exception do
      end;
    }
end;

Function completeKey(valor: AnsiString): AnsiString;
var
    i, j: integer;
begin
    j := 4 - length(valor);
    for i := 1 to j do
    begin
        valor := ' ' + valor;
    end;
    result := valor;
end;

var
    showingS: Boolean;

procedure TfrmMatriz.MessageReceiver(var Msg: TMessage);
begin
    HotKeys.RegisterKey(Msg.WParam, IntToStr(Msg.LParam));
end;

procedure TfrmMatriz.Minimizar1Click(Sender: TObject);
begin
    Application.Minimize;
end;

procedure TfrmMatriz.MostradorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if (Button = mbLeft) then
    begin
        ReleaseCapture;
        frmMatriz.Perform(WM_SysCommand, SC_DragMove, 0);
    end;
end;

end.
