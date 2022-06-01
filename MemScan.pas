unit MemScan;

interface

uses windows, sysutils, utils, common, constantes, ComCtrls, forms, classes, driverhandler;

Type

TMemoryRegion = record
  BaseAddress:  Dword;
  MemorySize:   Dword;
  IsChild:      boolean;
  startaddress: pointer;
end;

function AdjustPrivileges: boolean;
Function StartScan(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: string; Lista: TStrings): cardinal;
Function ScanArray(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: string; Lista: TStrings): cardinal;
Function ScanValue(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: int64; Lista: TStrings): cardinal;

var BufferSize: cardinal = 512;

implementation

function AdjustPrivileges: boolean;
var hToken: THandle;
    tp, oldtp: TOKEN_PRIVILEGES;
    dwSize: DWORD;
    LUID: Int64;
begin
	dwSize := sizeof(TOKEN_PRIVILEGES);

	if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
	begin
		if (GetLastError = ERROR_CALL_NOT_IMPLEMENTED) then
    begin
			result := true;
      exit;
    end;
		result := false;
    exit;
	end;

	if not LookupPrivilegeValue(nil, 'SeDebugPrivilege' , luid) then
	begin
		CloseHandle(hToken);
		result := false;
    exit;
	end;

	ZeroMemory(@tp, sizeof(tp));
	tp.PrivilegeCount := 1;
	tp.Privileges[0].Luid := luid;
	tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

	(* Adjust Token Privileges *)
	if not AdjustTokenPrivileges(hToken, FALSE, tp, sizeof(TOKEN_PRIVILEGES), oldtp, dwSize) then
	begin
		CloseHandle(hToken);
		result := false;
    exit;
	end;
	// close handles
  CloseHandle(hToken);

	result := true;
end;

type
   TByteInfo = record
      Valor: byte;
      Casa : byte;
      Pos: integer;
   end;

Function CompareSemiByte(Valor: byte; Rec: TByteInfo): boolean;
begin
   result := false;
   case Rec.Casa of
      1:
        begin
           if byte(Valor div $10) = byte(Rec.Valor) then
              result := true;
        end;
      2:
        begin
           if byte(Valor and $0F) = byte(Rec.Valor) then
              result := true;
        end;
   end;
end;

Function TrimArray(Valor: string): string;
var i:integer;
begin
   result := '';
   for i := 1 to length(valor) do
     if trim(valor[i]) <> '' then
        result := result + valor[i]; 
end;

Function ScanValue(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: int64; Lista: TStrings): cardinal;
begin
   result := StartScan(AStart, AStop, ReadOnly, ProgressBar, IntToByteArray(Valor), Lista);
end;

Function ScanArray(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: string; Lista: TStrings): cardinal;
begin
   result := StartScan(AStart, AStop, ReadOnly, ProgressBar, Valor, Lista);
end;

Function StartScan(AStart, AStop: Cardinal; ReadOnly: Boolean; ProgressBar: Pointer; Valor: string; Lista: TStrings): cardinal;
var mbi: MEMORY_BASIC_INFORMATION;
    Address, Size, TotalBytes: cardinal;
    TotalSize: int64;
    j,i, k, L: integer;
    ToSearch: array of TByteInfo;
    ValueSize: integer;
    Buffer: array of byte;
    Found: array of cardinal;
    Flag: boolean;
    Progresso: double;
    MemoryRegion: array of TMemoryRegion;
    FoundAddress: array of Cardinal;
    MemoryRegions: Cardinal;
begin
  Lista.Clear;
  FoundAddress := nil;
  FoundAddress := nil;
  MemoryRegion := nil;
  MemoryRegions := 0;
  
  Address := AStart;
  try
      //Pega regiões de memória acessível do processo
      while ( VirtualQueryEx(ProcessHandle, ptr(Address), mbi, sizeof(mbi)) <> 0) and (Address < AStop) and ( mbi.RegionSize > 0 ) do
      begin
          if (( mbi.Type_9 = MEM_PRIVATE ) or ( mbi.type_9 = MEM_IMAGE )) and ( mbi.State = MEM_COMMIT ) and (not boolean(mbi.Protect and PAGE_GUARD) ) and (not boolean(mbi.Protect and PAGE_NOACCESS) ) then
          begin
              if not ReadOnly then  
              begin
                  if( not boolean ((mbi.AllocationProtect) and (page_readonly or page_execute_read)) ) and ( not boolean((mbi.Protect) and (page_readonly or PAGE_EXECUTE_READ))) then 
                  begin
                    if (mbi.AllocationProtect and PAGE_NOCACHE)=PAGE_NOCACHE then
                    begin
                       Address := dword(mbi.BaseAddress)+mbi.RegionSize;
                       continue;
                    end;
                    setlength( MemoryRegion, MemoryRegions+1);
                    MemoryRegion[MemoryRegions].MemorySize  := mbi.RegionSize;
                    MemoryRegion[MemoryRegions].BaseAddress := dword(mbi.baseaddress);
                    inc(MemoryRegions);
                  end;
              end else  
              begin
                   if (mbi.AllocationProtect and PAGE_NOCACHE) = PAGE_NOCACHE then
                   begin
                       Address := dword(mbi.BaseAddress)+mbi.RegionSize;
                       continue;
                   end;
                   setlength(MemoryRegion ,MemoryRegions+1);
                   MemoryRegion[MemoryRegions].MemorySize := mbi.RegionSize;
                   MemoryRegion[MemoryRegions].BaseAddress := dword(mbi.baseaddress);
                   inc(MemoryRegions);
              end;
          end;

          Address := dword(mbi.baseaddress)+mbi.RegionSize;
      end;

      if MemoryRegions=0 then
      begin
        //closefile(memoryfile);
        //closefile(addressfile);
        //raise Exception.create('No readable memory found in the region you specified!');
        //MessageBox(0,'Falha ao ')
        exit;
      end;

      //Corrige o inicio da região de acordo com o endereço inicial ASTART
      if ( MemoryRegion[0].BaseAddress < AStart) and ( (MemoryRegion[0].BaseAddress + MemoryRegion[0].MemorySize - AStart) > 0 ) then
      begin
         MemoryRegion[0].MemorySize := (MemoryRegion[0].BaseAddress + MemoryRegion[0].MemorySize - AStart);
         MemoryRegion[0].BaseAddress := AStart;
      end;

      //Corrige o fim da região de acordo com o endereço inicial ASTOP
      if ( MemoryRegion[MemoryRegions-1].BaseAddress + MemoryRegion[MemoryRegions-1].MemorySize) > AStop then
           MemoryRegion[MemoryRegions-1].MemorySize := AStop - MemoryRegion[MemoryRegions-1].BaseAddress + 1;

      j := 0;
      Address := MemoryRegion[0].BaseAddress;
      Size := MemoryRegion[0].MemorySize;

      //Agrupa regiões contínuas
      for i:=1 to MemoryRegions-1 do
      begin
        if MemoryRegion[i].BaseAddress = (Address+Size) then
        begin
          inc(Size, MemoryRegion[i].MemorySize);
        end else
        begin
          MemoryRegion[j].BaseAddress := Address;
          MemoryRegion[j].MemorySize := Size;

          Address := MemoryRegion[i].BaseAddress;
          Size := MemoryRegion[i].MemorySize;
          inc(j);
        end;
      end;

      MemoryRegion[j].BaseAddress := Address;
      MemoryRegion[j].MemorySize := Size;

      MemoryRegions := j+1;
      setlength(MemoryRegion, MemoryRegions);

      //------------------------------------------------------------------------
          Valor := TrimArray(Valor);
          ValueSize := length(Valor) div 2;
          i := 1;
          j := 0;
          while i < length(valor) do
          begin
              if (valor[i] in ['0'..'9'] ) and (valor[i+1] in ['0'..'9'] ) then
              begin
                  inc(j);
                  setlength(ToSearch, Length(ToSearch)+1);
                  ToSearch[High(ToSearch)].Valor := byte(HexToInt(valor[i]+valor[i+1]));
                  ToSearch[High(ToSearch)].Casa  := 0;
                  ToSearch[High(ToSearch)].Pos   := j;
                  inc(i,2);
                  continue;
              end;

              if (valor[i] in ['0'..'9'] ) or (valor[i+1] in ['0'..'9'] ) then
              begin
                  setlength(ToSearch, Length(ToSearch)+1);
                  if (valor[i] in ['0'..'9'] ) then
                  begin
                     ToSearch[High(ToSearch)].Casa := 1;
                     ToSearch[High(ToSearch)].Valor := byte(HexToInt(valor[i]));
                  end else
                  begin
                     ToSearch[High(ToSearch)].Casa := 2;
                     ToSearch[High(ToSearch)].Valor := byte(HexToInt(valor[i+1]));
                  end;
                  inc(j);
                  ToSearch[High(ToSearch)].Pos := j;
              end;
              inc(j);
              inc(i,2);
          end;

      //------------------------------------------------------------------------
      TotalSize := 0;
      for i := 0 to MemoryRegions - 1 do
      begin
         inc(TotalSize, MemoryRegion[i].MemorySize);
      end;

      TProgressBar(ProgressBar^).Max := MemoryRegions;//TotalSize;
      //TProgressBar(ProgressBar^).Max := 60000;

      L := 0;
      for i := 0 to MemoryRegions - 1 do
      begin
          Application.ProcessMessages;
          setlength( Buffer, Memoryregion[i].MemorySize );
          CmReadProcessMemoryA(ProcessID, ProcessBase, ptr(Memoryregion[i].BaseAddress), @Buffer[0], Memoryregion[i].MemorySize);
          Inc(L, Memoryregion[i].MemorySize);
          if L > TotalSize then
             exit;
          j := 0;
          while j < length(Buffer)- 1 - ValueSize do
          begin
              Application.ProcessMessages;
              //Progresso := ;
              //TProgressBar(ProgressBar^).Position := Trunc(short(L*TProgressBar(ProgressBar^).Max) / short(TotalSize));

              Flag := true;

              if ToSearch[0].Casa > 0 then
                 Flag := CompareSemiByte(Buffer[i], ToSearch[0])
              else
                 Flag := (Buffer[j] = ToSearch[0].Valor);

              //Primeiro byte do array não confere.. incremente 1 e tente o proximo
              if not Flag then
              begin
                inc(j);
                continue;
              end else
              begin
              //Primeiro byte do array confere.. vamos testar o ultimo
                  if ToSearch[high(ToSearch)].Casa > 0 then
                    Flag := CompareSemiByte(Buffer[j + ToSearch[high(ToSearch)].Pos - 1], ToSearch[high(ToSearch)])
                  else
                    Flag := (Buffer[j + ToSearch[high(ToSearch)].Pos - 1] = ToSearch[high(ToSearch)].Valor);
              end;

              //Ok.. O Primeiro e o Ultimo conferem.. vamos testar o resto
              if Flag then
              begin
                  for k := 1 to length(ToSearch)- 2 do
                  begin
                      Flag := true;

                      if ToSearch[k].Casa > 0 then
                         Flag := CompareSemiByte(Buffer[ToSearch[k].Pos], ToSearch[k])
                      else
                         Flag := (Buffer[ToSearch[k].Pos] = ToSearch[k].Valor);

                      if not flag then
                         break;
                  end;

                  if Flag then
                  begin
                      setlength(Found, length(Found)+1);
                      Found[High(Found)] := Memoryregion[i].BaseAddress+j;
                      inc(j, ValueSize);
                      continue;
                  end;
              end;

              inc(j)
          end;
          TProgressBar(ProgressBar^).StepIt;


      end;

      if Length(Found) > 0 then
      begin
          for i:= 0 to length(found)-1 do
          begin
              Lista.add(IntToHex(Found[i],8));
          end;
      end;

      result := Length(Found);
      
  except
    on e:exception do
  end;


end;

end.
