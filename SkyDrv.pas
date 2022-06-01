unit SkyDrv;

interface

uses windows,sysutils,winsvc,psapi,classes,types,registry;

const currentversion=3210005;

const FILE_ANY_ACCESS=0;
const FILE_SPECIAL_ACCESS=FILE_ANY_ACCESS;
const FILE_READ_ACCESS=$0001;
const FILE_WRITE_ACCESS=$0002;
const FILE_RW_ACCESS=FILE_READ_ACCESS or FILE_WRITE_ACCESS;

const METHOD_BUFFERED=    0;
const METHOD_IN_DIRECT=   1;
const METHOD_OUT_DIRECT=  2;
const METHOD_NEITHER=     3;
const FILE_DEVICE_UNKNOWN=$00000022;
const IOCTL_UNKNOWN_BASE=FILE_DEVICE_UNKNOWN;

const IOCTLSW_WORD_0034READMEMORY             = (IOCTL_UNKNOWN_BASE shl 16) or ($0800 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034WRITEMEMORY            = (IOCTL_UNKNOWN_BASE shl 16) or ($0801 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034OPENPROCESS    		  	=	(IOCTL_UNKNOWN_BASE shl 16) or ($0802 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034OPENTHREAD	    	   	  = (IOCTL_UNKNOWN_BASE shl 16) or ($0818 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034GETVERSION				      =	(IOCTL_UNKNOWN_BASE shl 16) or ($0816 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034INITIALIZE     		    = (IOCTL_UNKNOWN_BASE shl 16) or ($080d shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);
const IOCTLSW_WORD_0034HOOKINTS 					    = (IOCTL_UNKNOWN_BASE shl 16) or ($0810 shl 2) or (METHOD_BUFFERED ) or (FILE_RW_ACCESS shl 14);

type thandlelist=record
  processhandle: thandle;
  processid: dword;
  validhandle: boolean;
end;

type TClient_ID=record
  processid: thandle;
  threadid: thandle;
end;
type PClient_ID=^TClient_ID;

type THookIDTThread=class(tthread)
  public
    cpunr: byte;
    done: boolean;
    succeeded: boolean;
    procedure execute; override;
end;

type THookIDTConstantly=class(tthread)
  public
    procedure execute; override;
end;

function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
function IsValidHandle(hProcess:THandle):BOOL; stdcall;
Function {OpenProcess}SKY_OPEN(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
Function {OpenThread}SKY_OPENTHREAD(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
function {ReadProcessMemory}SKY_READ(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
function {WriteProcessMemory}SKY_WRITE(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
function GetDriverVersion: dword;
function GetWin32KAddress(var address:DWORD;var size:dworD):boolean;
function InitializeDriver(Address,size:dword):BOOL; stdcall;


var
  handlelist: array of thandlelist;
  hdevice: thandle;
  ioctl: boolean;
  kernel32dll: thandle;
  usealternatedebugmethod: boolean;
  Successfullyloaded: boolean;
  iamprotected: boolean;
  hooker: THookIDTConstantly;
  processeventname, threadeventname: string;
  SDTShadow: DWORD;
  driverloc: string;
  debugport,processname: dword;
  ThreadsProcess,ThreadListEntry:dword;
  processevent,threadevent:thandle;
  ownprocess: thandle; //needed for simple kernelmemory access

implementation

function IsValidHandle(hProcess:THandle):BOOL; stdcall;
var i: integer;
begin
  result:=false;
  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      result:=handlelist[i].validhandle;
      exit;
    end;
end;

function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

procedure THookIDTConstantly.execute;
var input:TInput;
    br,cc: dword;
    i:integer;
    cpunr,PA,SA:Dword;
    cpunr2:byte;
begin
  freeonterminate:=true;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTLSW_WORD_0034HOOKINTS;

    while not terminated do
    begin
//      outputdebugstring('writing the idt');
      GetProcessAffinityMask(getcurrentprocess,PA,SA);

      cpunr2:=0;
      cpunr:=1;
      while (cpunr<=PA) do
      begin
        if ((cpunr) and PA)>0 then
        begin
          SetProcessAffinityMask(getcurrentprocess,cpunr);
          //create a new thread. (Gues on what cpu it will run at...)

          with THookIDTThread.Create(true) do
          begin
            try
              cpunr:=cpunr2;
              resume;

              while not done do sleep(10); //the sleep should also cause a taskswitch but I'm not 100% sure
            finally
              free;
            end;
          end;

        end;
        if cpunr=$80000000 then break;
        inc(cpunr,cpunr);
        inc(cpunr2);
      end;

      SetProcessAffinityMask(getcurrentprocess,PA); //multi processors are so fun. It'd be a waste not to use it
      sleep(5000); //wait a while before rewriting
    end;
  end;
end;

procedure THookIDTThread.execute;
var cc,br: dword;
begin
  try
    cc:=IOCTLSW_WORD_0034HOOKINTS;
    succeeded:=deviceiocontrol(hdevice,cc,@cpunr,1,@cpunr,0,br,nil);
  finally
    done:=true;
  end;
end;

function InitializeDriver(Address,size:dword):BOOL; stdcall;
type tinput=record
  address: dword;
  size:dword;
  NtUserBuildHwndList_callnumber: Dword;
  NtUserQueryWindow_callnumber:dword;
  NtUserFindWindowEx_callnumber:DWORD;
  NtUserGetForegroundWindow_callnumber:DWORD;
  activelinkoffset: dword;
  processnameoffset:dword;
  debugportoffset:dword;
end;
var cc: dword;
    buf: tinput;
    res: dword absolute buf;
    x:dword;

    callnumberfile: tfilestream;
    windowsversion: TOSVersionInfo;
    majorversion,minorversion,buildnumber: dword;
    CSDVersion: array [0..127] of char;
    a: boolean;
    i: integer;
begin
  result:=false;
  sdtshadow:=0;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    processevent:=OpenEvent(SYNCHRONIZE,false,pchar(processeventname));
    threadevent:=OpenEvent(SYNCHRONIZE,false,pchar(threadeventname));

    zeromemory(@buf,sizeof(buf));
    buf.address:=address;
    buf.size:=size;
    buf.NtUserBuildHwndList_callnumber:=0;
    buf.NtUserQueryWindow_callnumber:=0;
    buf.NtUserFindWindowEx_callnumber:=0;
    buf.NtUserGetForegroundWindow_callnumber:=0;

    buf.activelinkoffset:=0;
    buf.processnameoffset:=0;
    buf.debugportoffset:=0;


    //check if there is a callnumber.txt file in the rootdir, and if so use it
    if fileexists(extractfilepath(driverloc)+'kerneldata.dat') then
    begin
      //read the file, first 4 bytes is the callnumber of NtUserBuildHwndList_callnumber
      try
        callnumberfile:=tfilestream.create(extractfilepath(driverloc)+'kerneldata.dat',fmOpenRead,fmShareDenyNone	);
        try
          windowsversion.dwOSVersionInfoSize:=sizeof(windowsversion);
          GetVersionEx(windowsversion);


          callnumberfile.ReadBuffer(MajorVersion,4);
          callnumberfile.ReadBuffer(MinorVersion,4);
          callnumberfile.ReadBuffer(BuildNumber,4);
          callnumberfile.ReadBuffer(CSDVersion,128);
        //  a:=comparemem(@CSDVersion[0],@windowsversion.szCSDVersion[0],128);

          a:=true;
          i:=0;
          while a and (i<128) and (windowsversion.szCSDVersion[i]<>#0) and (CSDVersion[i]<>#0) do
          begin
            a:=CSDVersion[i]=windowsversion.szCSDVersion[i];
            inc(i);
          end;

          if (not a) or (majorversion<>windowsversion.dwMajorVersion) or (MinorVersion<>windowsversion.dwMinorVersion) or (buildnumber<>windowsversion.dwBuildNumber) then
          begin
            //messagebox(0,'It is recommended to run the systemcallretriever since the kerneldata.dat you have is outdated and will not be used. Of course, if this is the systemcallretriever, ignore this message...','Outdated kerneldata.dat',mb_ok);
//not a valid kerneldata.dat file            
          end
          else
          begin
            callnumberfile.ReadBuffer(x,4);
            buf.NtUserBuildHwndList_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserQueryWindow_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserFindWindowEx_callnumber:=x;

            callnumberfile.ReadBuffer(x,4);
            buf.NtUserGetForegroundWindow_callnumber:=x;

            callnumberfile.ReadBuffer(buf.activelinkoffset,4);
            callnumberfile.ReadBuffer(buf.processnameoffset,4);
            callnumberfile.ReadBuffer(buf.debugportoffset,4);

            debugport:=buf.debugportoffset;
            processname:=buf.processnameoffset;

            //----------------Add this part to the file---------
            ThreadsProcess:=$220;
            ThreadListEntry:=$3c;
          end;
        finally
          callnumberfile.free;
        end;
      except

      end;
    end;

    cc:=IOCTLSW_WORD_0034INITIALIZE;
    if deviceiocontrol(hdevice,cc,@buf,sizeof(tinput),@buf,sizeof(tinput),x,nil) then
    begin
      result:=true;
      SDTShadow:=res;
    end;
    ownprocess:=SKY_OPEN(PROCESS_ALL_ACCESS,false,getcurrentprocessid);
  end;
end;


function GetWin32KAddress(var address:DWORD;var size:dworD):boolean;
var need:dword;
    p: pointer;
    oldx: dword;
    x: array of pointer;
    i,j: integer;
    count: integer;
    drivername: pchar;
    nearest: dword; //nearest other driver (AFTER win32k.sys)
begin
  result:=false;

  copymemory(@oldx,@x,4);

  EnumDevicedrivers(nil,0,need);
  count:=need div 4;
  getmem(p,need);
  try
    if enumDevicedrivers(p,need,need) then
    begin
      getmem(drivername,200);
      copymemory(@x,@p,4);
      try

        for i:=0 to count-1 do
        begin

          GetDevicedriverBaseName(x[i],drivername,200);
          if lowercase(drivername)='win32k.sys' then
          begin
            address:=dword(x[i]);

            nearest:=$ffffffff;
            for j:=0 to count-1 do
              if (dword(x[j])>dword(x[i])) and (dword(x[j])<nearest) then //it's bigger than winb32k.sys, but closer to it than the last closts I found
                nearest:=dword(x[j]);

            size:=nearest-address;

            result:=true;
            exit;
          end;
        end;


      finally
        copymemory(@x,@oldx,4);

        freemem(drivername);
      end;


    end;
  finally
    freemem(p);
  end;

end;

function GetDriverVersion:dword;
var x,res,cc,cc2,cc3:dword;
begin
  result:=0;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTLSW_WORD_0034GETVERSION;
    if cc<>cc2 then
      if cc<>cc3 then


    if deviceiocontrol(hdevice,cc,@res,4,@res,4,x,nil) then
      result:=res;

  end;
end;

function {ReadProcessMemory}SKY_READ(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesRead:DWORD):BOOL; stdcall;
type TInputstruct=record
  processid: dword;
  startaddress: dword;
  bytestoread: word;
end;
var ao: array [0..600] of byte; //give it some space
    input: TInputstruct absolute ao[0];
    cc:dword;

    i: integer;
    ok: boolean;
    br: dword;

    SW_WORD_0066: dword;
    bufpointer: dword;
    bufpointer2: pointer;
    toread: dword;
begin
  result:=false;
  numberofbytesread:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        cc:=IOCTLSW_WORD_0034READMEMORY;
        SW_WORD_0066:=dword(lpBaseAddress);
        bufpointer:=dword(lpbuffer);

        ok:=true;
        while ok do
        begin
          input.processid:=handlelist[i].processid;
          if nSize-numberofbytesread>=512 then
            toread:=512
          else
            toread:=nSize-numberofbytesread;

          input.bytestoread:=toread;
          input.startaddress:=SW_WORD_0066;

          if deviceiocontrol(hdevice,cc,@ao[0],512,@ao[0],512,br,nil) then
          begin
            bufpointer2:=pointer(bufpointer);
            copymemory(bufpointer2,@ao[0],toread);
            //no check if it works or try except, it's up to the (retarded) user to do it right
          end
          else
          begin
            exit;
          end;

          inc(SW_WORD_0066,toread);
          inc(bufpointer,toread);
          inc(numberofbytesread,toread);

          if numberofbytesread=nSize then
          begin
            result:=true;
            exit;
          end;
        end;

        exit;
      end else if not handlelist[i].validhandle then exit; //else use the normal method...
    end;

  //not found so ....
  result:=windows.ReadProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,NumberOfBytesRead);
end;

function {WriteProcessMemory}SKY_WRITE(hProcess:THANDLE;lpBaseAddress:pointer;lpBuffer:pointer;nSize:DWORD;var NumberOfBytesWritten:DWORD):BOOL; stdcall;
type TInputstruct=record
  processid: dword;
  startaddress: dword;
  bytestowrite: word;
end;
var ao: array [0..511] of byte;
    input: TInputstruct absolute ao[0];
    cc:dword;

    i: integer;
    ok: boolean;
    br: dword;

    SW_WORD_0066: dword;
    bufpointer: dword;
    bufpointer2: pointer;
    towrite: dword;
begin
  result:=false;
  NumberOfByteswritten:=0;
  //find the hprocess in the handlelist, if it isn't use the normal method (I could of course use NtQueryProcessInformation but it's undocumented and I'm too lazy to dig it up

  for i:=0 to length(handlelist)-1 do
    if handlelist[i].processhandle=hProcess then
    begin
      if hdevice<>INVALID_HANDLE_VALUE then
      begin
        cc:=IOCTLSW_WORD_0034WRITEMEMORY;
        SW_WORD_0066:=dword(lpBaseAddress);
        bufpointer:=dword(lpbuffer);

        ok:=true;
        while ok do
        begin
          zeromemory(@ao[0],512);

          input.processid:=handlelist[i].processid;
          if nSize-NumberOfByteswritten>=(512-sizeof(TInputstruct)) then
            towrite:=(512-sizeof(TInputstruct))
          else
            towrite:=nSize-NumberOfByteswritten;

          input.bytestowrite:=towrite;
          input.startaddress:=SW_WORD_0066;

          bufpointer2:=pointer(bufpointer);
          copymemory(@ao[sizeof(tinputstruct)],bufpointer2,towrite);

          if not deviceiocontrol(hdevice,cc,@ao[0],512,@ao[0],512,br,nil) then exit;

          inc(SW_WORD_0066,towrite);
          inc(bufpointer,towrite);
          inc(NumberOfByteswritten,towrite);

          if NumberOfByteswritten=nSize then
          begin
            result:=true;
            exit;
          end;
        end;

        exit;
      end else if not handlelist[i].validhandle then exit;
    end;

  //not found so ....
  result:=windows.writeProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize,NumberOfByteswritten);
end;

function {OpenThread}SKY_OPENTHREAD(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
var
  threadhandle: thandle;
  cc,x: dword;
begin
  result:=0;
  if dwThreadId=0 then exit;
  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTLSW_WORD_0034OPENTHREAD;
    threadhandle:=dwThreadId;
    if deviceiocontrol(hdevice,cc,@threadhandle,4,@threadhandle,4,x,nil) then
      result:=threadhandle
    else
      result:=0;
  end;
end;


function {OpenProcess}SKY_OPEN(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwProcessId:DWORD):THANDLE; stdcall;
var valid:boolean;
    Processhandle: thandle;
    i:integer;
    cc,x: dword;
begin
  valid:=true;
  if dwProcessId=0 then
  begin
    result:=0;
    exit;
  end;

  if hdevice<>INVALID_HANDLE_VALUE then
  begin
    cc:=IOCTLSW_WORD_0034OPENPROCESS;

    processhandle:=dwProcessId; //rest is ignored

    if deviceiocontrol(hdevice,cc,@processhandle,4,@processhandle,4,x,nil) then
    begin
      result:=processhandle
    end
    else
      result:=0;
  end else result:=windows.OpenProcess(dwDesiredAccess,bInheritHandle,dwProcessID);

  if result=0 then //you can still access memory using the low level stuff, just not normal stuff
  begin
    valid:=false;
    //openprocess isn't working
    if length(handlelist)=0 then result:=100+random(32000)
    else
      result:=handlelist[length(handlelist)-1].processhandle+1;
  end;

  //check for a duplicate handle and replace it (closehandle/openproces gets you the same handle)
  for i:=0 to length(handlelist)-1 do
  begin
    if handlelist[i].processhandle=result then
    begin
      handlelist[i].processid:=dwProcessID;
      handlelist[i].validhandle:=valid;
      exit;
    end;

  end;

  setlength(handlelist,length(handlelist)+1);
  handlelist[length(handlelist)-1].processhandle:=result;
  handlelist[length(handlelist)-1].processid:=dwProcessID;
  handlelist[length(handlelist)-1].validhandle:=valid;
end;

var hscManager: thandle;
    hservicE: thandle;

var sav: pchar;

    apppath: pchar;


    win32kaddress,win32size:dword;
    servicename,sysfile: string;
    dataloc: string;

    reg: tregistry;
    driverdat: textfile;

//    servicestatus: _service_status;
initialization
begin
  ioctl:=true;
  kernel32dll:=loadlibrary('kernel32.dll');

  usealternatedebugmethod:=false;
  Successfullyloaded:=false;
  iamprotected:=false;
  apppath:=nil;
  hooker:=nil;
  setlength(handlelist,0);
  hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
  try
    getmem(apppath,250);
    GetModuleFileName(0,apppath,250);

    //dataloc:=extractfilepath(apppath)+'driver.dat';
    if not fileexists(dataloc) then
    begin
      servicename:='CM_SYSTEM';
      processeventname:='CMList';
      threadeventname:='CMTList';
      sysfile:='Configs.cmp';
    end
    else
    begin
      assignfile(driverdat,dataloc);
      reset(driverdat);
      readln(driverdat,servicename);
      readln(driverdat,processeventname);
      readln(driverdat,threadeventname);
      readln(driverdat,sysfile);
      closefile(driverdat);      
    end;

    driverloc:=extractfilepath(apppath)+sysfile;
  finally
    freemem(apppath);
  end;

  if not fileexists(driverloc) then
  begin
    //messagebox(0,'You are missing the driver. Try reinstalling cheat engine, and try to disable your anti-virus before doing so.','Driver error',MB_ICONERROR or mb_ok);
    hDevice:=INVALID_HANDLE_VALUE;
    exit;
  end;



  if hscmanager<>0 then
  begin
    hService := OpenService(hSCManager, pchar(servicename), SERVICE_ALL_ACCESS);
    if hService=0 then
    begin
      hService:=CreateService(
         hSCManager,           // SCManager database
         pchar(servicename),   // name of service
         pchar(servicename),   // name to display
         SERVICE_ALL_ACCESS,   // desired access
         SERVICE_KERNEL_DRIVER,// service type
         SERVICE_DEMAND_START, // start type
         SERVICE_ERROR_NORMAL, // error control type
         pchar(driverloc),     // service's binary
         nil,                  // no load ordering group
         nil,                  // no tag identifier
         nil,                  // no dependencies
         nil,                  // LocalSystem account
         nil                   // no password
      );
    end
    else
    begin
      //make sure the service points to the right file
      ChangeServiceConfig(hservice,
                          SERVICE_KERNEL_DRIVER,
                          SERVICE_DEMAND_START,
                          SERVICE_ERROR_NORMAL,
                          pchar(driverloc),
                          nil,
                          nil,
                          nil,
                          nil,
                          nil,
                          pchar(servicename));


    end;

    if hservice<>0 then
    begin
      sav:=nil;

      //setup the configuration parameters before starting the driver
      reg:=tregistry.Create;
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      if not reg.OpenKey('\SYSTEM\CurrentControlSet\Services\'+servicename,false) then
      begin
        //messagebox(0,'Failure to configure the driver','Driver Error',MB_ICONERROR or mb_ok);
        hDevice:=INVALID_HANDLE_VALUE;
        exit;
      end;

      reg.WriteString('A','\Device\'+servicename);
      reg.WriteString('B','\DosDevices\'+servicename);
      reg.WriteString('C','\BaseNamedObjects\'+processeventname);
      reg.WriteString('D','\BaseNamedObjects\'+threadeventname);
      

      startservice(hservice,0,sav);
      closeservicehandle(hservice);
    end else
    begin
      //messagebox(0,'The service couldn''t get opened and also couldn''t get created.'+' Check if you have the needed rights to create a service, or call your system admin (Who''ll probably beat you up for even trying this). Untill this is fixed you won''t be able to make use of the enhancements the driver gives you','SW_WORD_0018 Error',MB_ICONERROR or mb_ok);
      hDevice:=INVALID_HANDLE_VALUE;
      exit;
    end;

    hdevice:=0;
    hDevice := CreateFile(pchar('\\.\'+servicename),
                  GENERIC_READ or GENERIC_WRITE,
                  FILE_SHARE_READ or FILE_SHARE_WRITE,
                  nil,
                  OPEN_EXISTING,
                  FILE_FLAG_OVERLAPPED,
                  0);


    if hdevice=INVALID_HANDLE_VALUE then
      //messagebox(0,'The driver couldn''t be opened! It''s not loaded or not responding. I recommend to reboot your system and try again','SW_WORD_0018.DLL Error',MB_ICONERROR or MB_OK)
    else
    begin
      //Get the address of win32k.sys
      if GetDriverVersion<>currentversion then
      begin
        closehandle(hdevice);
        //messagebox(0,'The driver that is currently loaded belongs to a previous version of Cheat Engine. Please unload this old driver or reboot.','SW_WORD_0018.dll',MB_ICONERROR or MB_OK);

        hdevice:=INVALID_HANDLE_VALUE;
      end
      else
      begin
        if GetWin32KAddress(win32kAddress,win32size) then
        begin
          if not InitializeDriver(win32kAddress,win32size) then
          begin
            //messagebox(0,'The driver failed to successfully initialize. Some functions may not completly work','SW_WORD_0018.dll',MB_ICONERROR or MB_OK);
          end;
        end
        else
          //messagebox(0,'There was an error while trying to find the win32k.sys device driver. This means that some functions will not work','SW_WORD_0018.dll',MB_ICONERROR or MB_OK);

        Successfullyloaded:=true;
      end;
    end;

    //successfully initialized, say goodbye to the init params
    reg.DeleteValue('A');
    reg.DeleteValue('B');
    reg.DeleteValue('C');
    reg.DeleteValue('D');


    closeservicehandle(hscmanager);
  end;
end;


finalization
begin
try
  closehandle(ownprocess);
  if hooker<>nil then hooker.Terminate;
  freelibrary(kernel32dll);
except
  on e:exception do
end;
end;
end.
