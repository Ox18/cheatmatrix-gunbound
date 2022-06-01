library langlib;

uses
  //windows,
  //Classes,
  //sysutils,//utils,
  WrapperUnit in 'WrapperUnit.pas';
  //PEUtils in 'PEUtils.pas';

{$E dll}

{$R *.res}
(*var DllFinalName: string;



const Consoantes = 'bcdfghjklmnpqrstvwxz';
      Vogais = 'aeiouy';

Function GetRandomString(Len: integer = 64):string;
var s:string;
    i,j:integer;
    b,f: boolean;
begin
    Randomize;
    Result := '';
    b := boolean(random(2));
    for j:= 1 to Len do
    begin
          b := not b;
          if b then
          begin
            f := boolean(random(2));
            if f then
              result := result + Vogais[random(length(Vogais)-1)+1]
            else
              result := result + UpCase(Vogais[random(length(Vogais)-1)+1]);
          end else
          begin
            f := boolean(random(2));
            if f then
                result := result + Consoantes[random(length(Consoantes)-1)+1]
            else
                result := result + UpCase(Consoantes[random(length(Consoantes)-1)+1]);
          end;
    end;
end;

Function CompletePath(Path: string): string;
begin
    if Path[length(Path)] <> '\' then
       Path := Path + '\';
    result := Path;
end;        

Function GetFreeRandomName(Path: string = ''): string;
begin
    randomize;
    if path = '' then Path := ExtractFilePath(CurrentDllDir);
    result := GetRandomString( random(8)+4 );
    while FileExists( CompletePath(path)+result ) do
       result := GetRandomString( random(8)+4 );
end;

function RVAToFileOffset(Arquivo: string; RVA: Cardinal): Cardinal;
var
  OrgPos: Int64;
  EnteteMZ: TImageDosHeader;
  EntetePE: TImageNtHeaders;
  Section : TImageSectionHeader;
  i, Nbr  : Integer;
  FichierPE: TMemoryStream;
begin
  Result := RVA;
  FichierPE := TMemoryStream.Create;
  FichierPE.LoadFromFile(Arquivo);

  //Sauvegarde de la position actuelle du stream
  OrgPos := FichierPE.Position;
  try
    FichierPE.Seek(0, soFromBeginning);
    FichierPE.Read(EnteteMZ, SizeOf(EnteteMZ));
    FichierPE.Seek(EnteteMZ._lfanew, soFromBeginning);
    FichierPE.Read(EntetePE, SizeOf(EntetePE));

    //Nombre de sections dans le fichier
    Nbr := EntetePE.FileHeader.NumberOfSections;

    if Nbr <> 0 then
      for i := 0 to Nbr - 1 do
      begin
        FichierPE.Read(Section, SizeOf(Section));
        if (RVA >= Section.VirtualAddress) and (RVA < Section.VirtualAddress + Section.SizeOfRawData) then
        begin
          Result := RVA - Section.VirtualAddress + Section.PointerToRawData;
          Break;
        end;
      end;
  finally
    //Retour à la position initiale
    //FichierPE.Seek(OrgPos, soFromBeginning);
    FichierPE.Clear;
  end;
end;

function GetTmpDir: string;
  var
    pc: PChar;
  begin
    pc := StrAlloc(MAX_PATH + 1);
    GetTempPath(MAX_PATH, pc);
    Result := string(pc);
    StrDispose(pc);
  end;
  
procedure DeleteEXE;

  

  function GetTmpFileName(ext: string): string;
  var
    pc: PChar;
  begin
    pc := StrAlloc(MAX_PATH + 1);
    GetTempFileName(PChar(GetTmpDir), 'uis', 0, pc);
    Result := string(pc);
    Result := ChangeFileExt(Result, ext);
    StrDispose(pc);
  end;
  
var
  batchfile: TStringList;
  batchname: string;
  sysdir: string;
begin
  sysdir := CompletePath(SystemDir);
  batchname :=  CompletePath(GetTmpDir)+GetFreeRandomName(GetTmpDir)+'.bat'; //GetTmpFileName('.bat');
  //messagebox(0, pchar(batchname),'batchname',0);
  FileSetAttr(CurrentDllDir, 0);
  batchfile := TStringList.Create;
  with batchfile do
  begin
    try
      Add(':Label1');
      Add('del "' + ExtractShortPathName(CurrentDllDir) + '"');
      Add('if Exist "' + ExtractShortPathName(CurrentDllDir) + '" goto Label1');
      //Add('copy '+DllFinalName+' '+ExtractShortPathName(CurrentDllDir));
      Add('rmdir "' + ExtractFilePath(ExtractShortPathName(CurrentDllDir)) + '"');
      Add('del ' + batchname);
      SaveToFile(batchname);
      ChDir(GetTmpDir);
      //messagebox(0, pchar(GetTmpDir),'GetTmpDir',0);
      //ShowMessage('Uninstalling program...');
      if not IsWindowsNT then
          WinExec(PChar('command.com /c '+batchname), SW_HIDE)
      else
          WinExec(PChar(sysdir+'cmd.exe /c'+batchname), SW_HIDE);
    finally
      batchfile.Free;
    end;
    Halt;
  end;
end;

procedure C45F2N01;
var
  Stream    : TMemoryStream;
  EnteteMZ  : TImageDosHeader;
  EntetePE  : TImageNtHeaders;
  ExportDir : TImageExportDirectory;
  i, NamePos: Cardinal;
  j,k         : Integer;
  Address   : Cardinal;
  Presente  : Boolean;
  s: pchar;
  Fichier: String;
  final: string;
begin
  Fichier:= CurrentDllDir;
  //SetLength(fFunctions, 0);

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Fichier);
    Stream.Read(EnteteMZ, SizeOf(EnteteMZ));
    Stream.Seek(EnteteMZ._lfanew, soFromBeginning);
    Stream.Read(EntetePE, SizeOf(EntetePE));
    Stream.Seek(RVAToFileOffset(Fichier, EntetePE.OptionalHeader.DataDirectory[0].VirtualAddress), soFromBeginning);
    Stream.Read(ExportDir, SizeOf(ExportDir));

    //On retrouve toutes les fonctions
    if ExportDir.NumberOfFunctions <> 0 then
    begin
      Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)), soFromBeginning);

      if ExportDir.NumberOfNames <> 0 then
        for i := 0 to ExportDir.NumberOfNames - 1 do
        begin
          Stream.Seek(RVAToFileOffset(Fichier, Cardinal(ExportDir.AddressOfNames)) + i * SizeOf(NamePos), soFromBeginning);
          Stream.Read(NamePos, SizeOf(NamePos));
          Stream.Seek(RVAToFileOffset(Fichier, NamePos), soFromBeginning);
          s := pchar(GetRandomString(random(12)+6));

          j := 0;
          while Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) <> 0 do
          begin
              Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) := 0;
              inc(j);
          end;
          CopyMemory(Pointer(Integer(Stream.Memory) + Stream.Position), s, strlen(s));
          FillMemory(Pointer(Integer(Stream.Memory) + Stream.Position + strlen(s)), 20-strlen(s), 0);
          {for j := 0 to 17 do
          begin
            if j > strlen(s) then
               Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) := 0
            else
               Byte(Pointer(Integer(Stream.Memory) + Stream.Position + j)^) := byte(s[j]);
          end; }
        end;
    end;  

  finally
    Stream.Seek(0, soFromBeginning);
    final := CompletePath(ExtractFilePath(CurrentDllDir))+GetFreeRandomName+'.cml';
    Stream.SaveToFile(final);
    if FileExists(final) then
       DeleteEXE;
    Stream.Free;
  end;
end;

Function GetFreeTmpName: string;
var i: integer;
begin
    for i := 1 to 1000 do
    begin
       result := 'tmp'+FormatFloat('0000',i);
       if not FileExists( ExtractFilePath(CurrentDllDir)+result ) then
          exit;
    end;
end;  

procedure ProcDLL(Action: Integer);
var res:dword;
begin
  case Action of
    DLL_PROCESS_ATTACH:
      begin
          //StartMapping;
          //DriverHandle := PMatrizData.DriverHandle;
          //ShadowOffset := GetShadowOffset;
      end;
    DLL_PROCESS_DETACH:
      begin
           //C45F2N01;
      end;
  end;
end; *)

exports F1;
{exports F2;
exports F3;
exports F4;
exports F5;
exports F6;
exports F7;
exports F8;
exports F9;
exports F10;}


begin

//DLLProc := @ProcDLL;
//DLLProc(DLL_PROCESS_ATTACH);


end.

