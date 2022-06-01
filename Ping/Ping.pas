unit Ping;

interface

uses
  Windows, SysUtils, Classes;

type
  TSunB = packed record
    s_b1, s_b2, s_b3, s_b4: byte;
  end;

  TSunW = packed record
    s_w1, s_w2: word;
  end;

  PIPAddr = ^TIPAddr;

  TIPAddr = record
    case integer of
      0:
        (S_un_b: TSunB);
      1:
        (S_un_w: TSunW);
      2:
        (S_addr: longword);
  end;

  IPAddr = TIPAddr;

function IcmpCreateFile: THandle; stdcall; external 'icmp.dll';
function IcmpCloseHandle(icmpHandle: THandle): boolean; stdcall;
external 'icmp.dll' function IcmpSendEcho(icmpHandle: THandle;
  DestinationAddress: IPAddr; RequestData: Pointer; RequestSize: Smallint;
  RequestOptions: Pointer; ReplyBuffer: Pointer; ReplySize: DWORD;
  Timeout: DWORD): DWORD; stdcall; external 'icmp.dll';

function Pingar(InetAddress: PAnsiChar): boolean;

implementation

uses
  WinSock;

function Fetch(var AInput: string; const ADelim: string = ' ';
  const ADelete: boolean = true): string;
var
  iPos: integer;
begin
  if ADelim = #0 then
  begin
    // AnsiPos does not work with #0
    iPos := Pos(ADelim, AInput);
  end
  else
  begin
    iPos := Pos(ADelim, AInput);
  end;
  if iPos = 0 then
  begin
    Result := AInput;
    if ADelete then
    begin
      AInput := '';
    end;
  end
  else
  begin
    Result := Copy(AInput, 1, iPos - 1);
    if ADelete then
    begin
      Delete(AInput, 1, iPos + Length(ADelim) - 1);
    end;
  end;
end;

procedure TranslateStringToTInAddr(AIP: PAnsiChar; var AInAddr);
var
  phe: PHostEnt;
  pac: PAnsiChar;
  GInitData: TWSAData;
begin
  WSAStartup($101, GInitData);
  try
    phe := GetHostByName((AIP));
    if Assigned(phe) then
    begin
      pac := phe^.h_addr_list^;
      if Assigned(pac) then
      begin
        with TIPAddr(AInAddr).S_un_b do
        begin
          s_b1 := byte(pac[0]);
          s_b2 := byte(pac[1]);
          s_b3 := byte(pac[2]);
          s_b4 := byte(pac[3]);
        end;
      end
      else
      begin
        //raise Exception.Create('Error getting IP from HostName');
        with TIPAddr(AInAddr).S_un_b do
        begin
          s_b1 := byte(0);
          s_b2 := byte(0);
          s_b3 := byte(0);
          s_b4 := byte(0);
        end;
      end;
    end
    else
    begin
      //raise Exception.Create('Error getting HostName');
        with TIPAddr(AInAddr).S_un_b do
        begin
          s_b1 := byte(0);
          s_b2 := byte(0);
          s_b3 := byte(0);
          s_b4 := byte(0);
        end;
    end;
  except
    FillChar(AInAddr, SizeOf(AInAddr), #0);
  end;
  WSACleanup;
end;

function Pingar(InetAddress: PAnsiChar): boolean;
var
  Handle: THandle;
  InAddr: IPAddr;
  DW: DWORD;
  rep: array [1 .. 128] of byte;
begin
  Result := false;
  Handle := IcmpCreateFile;
  if Handle = INVALID_HANDLE_VALUE then
    Exit;
  TranslateStringToTInAddr(InetAddress, InAddr);
  DW := IcmpSendEcho(Handle, InAddr, nil, 0, nil, @rep, 128, 0);
  Result := (DW <> 0);
  IcmpCloseHandle(Handle);
end;

end.
