unit CmPlugins;

interface

uses windows, classes, messages, Utils, controls, common,
    SplashScreen, graphics, functions, constantes;

const
    fSep = '|ØH®¼Ã|';
    vSep = '|©À½¥£|';

type
    PSwLine = ^TSwLine;

    TSwLine = packed record
        Field: AnsiString;
        Value: AnsiString;
    end;

    {
      TSwField = class
      private
      fValue: tSwLine;
      public
      Function AsString: AnsiString;
      Function AsPointer: PAnsiChar;
      Function AsInteger: Integer;
      Function AsBoolean: Boolean;
      end;
    }

    PPluginMemoryDataRecord = ^TPluginMemoryDataRecord;

    TPluginMemoryDataRecord = record
        PacketID: integer;
        Ponteiro: cardinal;
        Offset: cardinal;
        Range: cardinal;
        Size: cardinal;
        Endereco: cardinal;
        Valor: PAnsiChar;
    end;

    PPluginMemoryData = ^TPluginMemoryData;

    TPluginMemoryData = class(TList)
        constructor Create;
    private
        fcount: integer;
        function GetItem(index: integer): TPluginMemoryDataRecord;
        procedure SetItem(index: integer; const Value: TPluginMemoryDataRecord);
    public
        Hack: ShortString;
        Processo: ShortString;
        VersaoJogo: PAnsiChar;
        Property Item[index: integer]: TPluginMemoryDataRecord read GetItem write SetItem;
        Procedure Add(Valor: TPluginMemoryDataRecord);
    end;

    PPluginRegData = ^TPluginRegData;

    TPluginRegData = class(TList)
        constructor Create;
    private
        function GetItem(index: integer): TPluginMemoryData;
        procedure SetItem(index: integer; const Value: TPluginMemoryData);
    public
        Property Item[index: integer]: TPluginMemoryData read GetItem write SetItem;
        function Add(const Valor: TPluginMemoryData): integer; overload;
        Function IndexOf(Hack: ShortString): integer;
    end;


    //
    // Menssagem
    //

    { TSwMessage = Class
      constructor Create;
      private
      fValue: TList;
      public
      Procedure Clear;
      Procedure Add(Field: AnsiString; Value: AnsiString);
      Function ToText: AnsiString;
      Procedure LoadFrom(Value: AnsiString);
      Function FieldByName(Name: AnsiString): TSwField;
      end;


      TPluginMsgType = (MT_Close);

      TPluginMsg = packed record
      Tipo: TPluginMsgType;
      Valor: AnsiString;
      end; }

    //
    // Dados de um Plugin
    //
    TPgRec = record
        PutInList: Boolean;
        PluginHandle: HMODULE;
        WindowHandle: THandle;
        Registered: Boolean;
        Tipo: integer; // THackType;
        Status: integer; // TImageStatus;
        Offset: Pointer;
        Codigo: DWORD;
        Nome: PAnsiChar;
        Nick: PAnsiChar;
        Game: PAnsiChar;
        FileName: PAnsiChar;
        Key: PAnsiChar;
        NULO: PAnsiChar;
    end;

    PPgRec = ^TPgRec;

    PPlugin = ^TPlugin;

    TPlugin = class
        // private
    public
        // Vars: TPgRec;
        PutInList: Boolean;
        PluginHandle: HMODULE;
        WindowHandle: THandle;
        Registered: Boolean;
        Tipo: integer; // THackType;
        Status: integer; // TImageStatus;
        Offset: Pointer;
        Codigo: DWORD;
        Nome: PAnsiChar;
        VersaoJogo: PAnsiChar;
        Game: PAnsiChar;
        FileName: PAnsiChar;
        Key: PAnsiChar;
        Function SendMsg(Valor: integer): PChar;
        Procedure Show;
        Procedure Hide;
        Function Visible: Boolean;
        Procedure Close;
        constructor Create;
    end;

    //
    // Lista de Plugins
    //
    TPluginList = class
        constructor Create(AOwner: THandle);
    private
        fPlugins: TList;
        fIndex: integer;
        Function GetPlugin(Index: integer): TPlugin;
        Procedure SetPlugin(Index: integer; Plugin: TPlugin);
        Function LoadPlugin(PathAnsi: AnsiString): TPlugin;
        Procedure SetAtual(Plugin: TPlugin);
        function GetAtual: TPlugin;
    public
        Owner: THandle;
        Property PluginIndex: integer read fIndex;
        Property Plugins[index: integer]: TPlugin read GetPlugin write SetPlugin;
        Property Plugin: TPlugin read GetAtual write SetAtual;
        Function GetFromNick(Nick: AnsiString): integer;
        Function GetAll(ClearBefore: Boolean = false): integer;
        Function Count: integer;
        Procedure Add(Plugin: TPlugin);
        Procedure Delete(Index: integer);
        Procedure DeleteFull(Plugin: TPlugin);
        Function Seek(Index: integer): Boolean;
        Function Next: Boolean;
        Function Prev: Boolean;
        Function Eof: Boolean;
        Procedure Clear;
        Procedure Reset;
    end;

    TOffsetUnit = record
        Nome: AnsiString;
        Code: AnsiString;
        Data: TPluginMemoryDataRecord;
    end;

    TProcesso = record
        Nome: String;
        Indice: integer;
        Injetado: Boolean;
    end;

var
    listaGrupoItemsMemoria: TPluginRegData;
    arrayProcessos: array of TProcesso;
    entradasRegistro: TStringList;
    listaProcessos: TStringList;

    // OffsetList: array of TOffsetUnit;

Function CompleteNumber(Numero: AnsiString): AnsiString;

implementation

uses SysUtils;

{ TPlugin }

Function CompleteNumber(Numero: AnsiString): AnsiString;
var
    s: AnsiString;
    i, j: integer;
begin
    result := Numero;
    j := length(result);
    s := '';
    for i := 1 to (8 - j) do
        s := s + '0';

    result := result + s;
    { while length(IntToStr(Numero)) < 10 do
      Numero := Numero*10;
      while length(IntToStr(Numero)) > 10 do
      Numero := Numero div 10;
      result := Numero; }
end;

//
// Fecha o Plugin
//
procedure TPlugin.Close;
begin
    SendMsg(integer(MT_Close));
    try
        FreeLibrary(PluginHandle);
    except
        on e: exception do
    end;
end;

//
// Cria o Plugin
//

constructor TPlugin.Create;
begin
    //
end;

//
// Envia Msg para o Plugin
//

procedure TPlugin.Hide;
begin
    SendMsg(integer(MT_Hide));
end;

Function TPlugin.SendMsg(Valor: integer): PChar;
var
    Func: function(Valor: integer): PChar; stdcall;
    Address: Pointer;
    Ms: TMemoryStream;
begin
    //
    Address := GetProcAddress(PluginHandle, 'm5');
    if Address = nil then
        result := '';
    @Func := Address;
    result := Func(Valor);
    //
end;

{ TPluginList }

//
// Adiciona um Plugin
//

procedure TPluginList.Add(Plugin: TPlugin);
var
    P: PPlugin;
begin
    //
    P := new(PPlugin);
    P^ := Plugin;
    if fPlugins = nil then
        fPlugins := TList.Create;
    fPlugins.Add(P);
    //
end;

//
// Limpa a Lista de Plugins
//
procedure TPluginList.Clear;
begin
    fPlugins.Clear;
end;

//
// Cria a Lista de Plugins
//
function TPluginList.Count: integer;
begin
    result := fPlugins.Count;
end;

constructor TPluginList.Create(AOwner: THandle);
begin
    Owner := AOwner;
    fPlugins := TList.Create;
    fIndex := 0;
end;

//
// Deleta um Plugin
//
procedure TPluginList.Delete(Index: integer);
begin
    fPlugins.Delete(index);
end;

//
// Deleta um Plugin
//
procedure TPluginList.DeleteFull(Plugin: TPlugin);
var
    P: PPlugin;
begin
    P := new(PPlugin);
    P^ := Plugin;
    fPlugins.Remove(P);
end;

//
// Fim da lista de Plugins?
//
function TPluginList.Eof: Boolean;
begin
    result := false;
    if (fIndex >= fPlugins.Count) or (fIndex < 0) then
        result := true;
end;

//
// Retorna um Plugin da lista
//
Function TPluginList.GetAll(ClearBefore: Boolean = false): integer;
var
    i: integer;
    SearchResult: TSearchRec;
    Plugin: TPlugin;
    Dir: AnsiString;
    Indice: integer;
begin
    //
    if ClearBefore then
        Clear;
    for i := 0 to length(PluginsLiberados) - 1 do
    begin
        if not FileExists(ExtractFilePath(ParamStr(0)) + PluginsLiberados[i]) then
            continue;

        Plugin := LoadPlugin(ExtractFilePath(ParamStr(0)) + PluginsLiberados[i]);
        if Plugin <> nil then
            if trim(Plugin.Nome) <> '' then
                Indice := listaGrupoItemsMemoria.IndexOf(md5(IntToStr(Plugin.Codigo) { +trim(Plugin.Nome) } ));
        if Indice >= 0 { InList(  md5( trim(IntToStr(CompleteNumber(Plugin.Codigo)))+trim(Plugin.Nick))  , listaGrupoItemsMemoria) } then
        begin
            Plugin.VersaoJogo := listaGrupoItemsMemoria.GetItem(Indice).VersaoJogo;
            if Splash <> nil then
                if Splash.Visible then
                begin
                    ChangeSplash(AnsiString('Carregando ' + ExtractFileName(CMPchar(Plugin.FileName))));
                    Delay(10);
                end;
            Add(Plugin);
            Inc(result);
        end;
    end;
    //
end;

function TPluginList.GetPlugin(Index: integer): TPlugin;
begin
    result := TPlugin(fPlugins.Items[Index]^);
end;

//
// Plugin atual
//
function TPluginList.GetAtual: TPlugin;
begin
    result := Plugins[fIndex];
end;

//
// Carrega um Plugin
//
function TPluginList.LoadPlugin(PathAnsi: AnsiString): TPlugin;
type
    PStream = ^TStream;
var
    Loc: cardinal;
    Address, _Address: Pointer;
    Dll: Function(Chave: PAnsiChar; AOwner: THandle; Color: TColor): PPgRec; stdcall;
    m2: Procedure(Valor: TPluginMemoryDataRecord); stdcall;
    m3: Function: Boolean; stdcall;
    m4: Procedure(id: integer; Valor: cardinal); stdcall;
    Key: AnsiString;
    t: PPgRec;
    SID: ShortString;
    i, j, k, n: integer;
    b: Boolean;
    ttv: TPluginMemoryDataRecord;
    okey: AnsiString;
    Path: String;
    PathDLL: PChar;
    s: AnsiString;
    itemMemoria: TPluginMemoryDataRecord;
const
    nums = '0123456789';
begin
    Path := String(PathAnsi);
    PathDLL := PChar(Path);
    if Path = '' then
        Exit;
    try
        if (FileExists(Path)) then
            Loc := LoadLibrary(PathDLL);

        if Loc = 0 then
        begin
            MessageBox(0, CMPchar(Constante(0031, false) + ExtractFileName(Path) + ')'), '', 0);
            Exit;
        end;
        result := TPlugin.Create;

        Address := GetProcAddress(Loc, PAnsiChar(AnsiString(Constante(0027, false))));
        @m4 := GetProcAddress(Loc, PAnsiChar(AnsiString(Constante(0028, false))));
        @m2 := GetProcAddress(Loc, PAnsiChar(AnsiString(Constante(0029, false))));
        @m3 := GetProcAddress(Loc, PAnsiChar(AnsiString(Constante(0030, false))));

        if (Address = nil) or (@m2 = nil) or (@m4 = nil) or (@m3 = nil) then
        begin
            MessageBoxA(0, PAnsiChar(AnsiString(Constante(0032, false) + ExtractFileName(Path))), '', 0);
            Exit;
        end;

        @Dll := Address;
        Key := '';
        Randomize;
        for i := 1 to 10 do
        begin
            j := Random(10) + 1;
            okey := Key + nums[j];
            Key := Key + nums[j];
        end;

        t := Dll(PAnsiChar(Key), Owner, MatrizColor);
        result.PutInList := t.PutInList;
        result.Codigo := t.Codigo;
        result.PluginHandle := t.PluginHandle;
        result.WindowHandle := t.WindowHandle;
        result.Registered := t.Registered;
        result.Tipo := t.Tipo;
        result.Status := t.Status;
        result.Offset := t.Offset;
        result.Nome := t.Nick;
        // result.Nick := t.Nick;
        result.Game := t.Game;
        result.FileName := t.FileName;
        result.Key := t.Key;

        SetWindowPos(result.WindowHandle, 0, 0, 0, 570, 360, SWP_DRAWFRAME);

        result.PluginHandle := Loc;
        SID := '';
        for i := 1 to length(okey) do
        begin
            j := DWORD(okey[i]) - DWORD('0');
            SID := SID + AnsiChar($41 + j);
        end;

        for i := 0 to listaGrupoItemsMemoria.Count - 1 do
        begin
            s := md5(IntToStr(result.Codigo) { + trim(Result.Nome) } );
            if trim(listaGrupoItemsMemoria.Item[i].Hack) = s then
                for j := 0 to listaGrupoItemsMemoria.Item[i].Count - 1 do
                begin
                    try
                        itemMemoria := listaGrupoItemsMemoria.Item[i].Item[j];
                        n := itemMemoria.PacketID;
                        if n > 0 then
                            m2(listaGrupoItemsMemoria.Item[i].Item[j]);
                    except
                        on e: exception do
                    end;
                end;
        end;

        m4(1, cardinal(@PortaSubMatrix));
        m4(2, cardinal(@Idioma));
        // FixApis(2, cardinal(@CMWriteProcessMemoryB));
        // FixApis(3, cardinal(@CMOpenProcessB));
        // FixApis(4, cardinal(@CMGetDCB));
        // FixApis(5, cardinal(@MatrizInfo));
        // FixApis(6, cardinal(@CMVirtualProtectB));
        // FixApis(7, cardinal(@CMOpenFileMappingB));
        // FixApis(8, cardinal(@CMMapViewOfFileB));

        b := m3;

{$IFNDEF DEBUG6}
        if (trim(SID) <> trim(result.Key)) or (not b) then
        begin
            result := nil;
            Exit
        end;
{$ENDIF}
    except
        on e: exception do
        begin
            MessageBox(0, PChar(e.Message), '', 0);
            result := nil;
        end;
    end;
end;

//
// Proximo Plugin da Lista
//
function TPluginList.Next: Boolean;
begin
    Inc(fIndex);
    result := true;
    if fIndex >= fPlugins.Count then
        result := false;
end;

//
// Plugin Anterior
//
function TPluginList.Prev: Boolean;
begin
    if fIndex - 1 < 0 then
    begin
        result := false;
        Exit;
    end;
    result := true;
    dec(fIndex);
end;

//
// Primeiro Plugin
//
procedure TPluginList.Reset;
begin
    fIndex := 0;
end;

//
// Modifica um Plugin
//
procedure TPluginList.SetAtual(Plugin: TPlugin);
begin
    Plugins[fIndex] := Plugin;
end;

procedure TPluginList.SetPlugin(Index: integer; Plugin: TPlugin);
var
    P: PPlugin;
begin
    /// /
    P := new(PPlugin);
    P^ := Plugin;
    fPlugins.Items[Index] := P;
    /// /
end;

//
// TSwMessage
(*
  //
  //  Adiciona um campo na Mensagem
  //
  procedure TSwMessage.Add(Field, Value: AnsiString);
  var Msg: PSwLine;
  begin
  ////
  Msg := new(PSwLine);
  Msg^.Field := Field;
  Msg^.Value := Value;
  fValue.Add(Msg);
  ////

  end;

  //
  //     Inicializa Mensagem
  //
  procedure TSwMessage.Clear;
  begin
  fValue.Clear;
  end;

  constructor TSwMessage.Create;
  begin
  fValue := TList.Create;
  end;

  //
  //     Campo pelo nome
  //
  function TSwMessage.FieldByName(Name: AnsiString): TSwField;
  var i:integer;
  begin
  //
  result := TSwField.Create;

  for i:=0 to fValue.Count-1 do
  begin
  if LowerCase(TSwLine(fValue.Items[i]^).Field) = LowerCase(Name) then
  begin
  Result.fValue := TSwLine(fValue.Items[i]^);
  end;
  end;
  //
  end;

  //
  //    Carrega uma mensagem
  //
  procedure TSwMessage.LoadFrom(Value: AnsiString);
  var Texto: AnsiString;
  SepLen: integer;
  SepPos: integer;
  Field: AnsiString;
  Result: PSwLine;
  begin
  //
  Texto := Value;
  SepLen := Length(fSep);

  while pos(fSep, Texto)>0 do
  begin
  SepPos := pos(fSep, Texto);
  Texto := Copy(Texto, SepPos+SepLen, length(Texto)-(SepPos+SepLen)+1);
  if pos(vSep, Texto) > 0 then
  begin
  Field := copy(Texto, 1, pos(vSep,Texto)-1);
  Value := copy(Texto, pos(vSep,Texto)+SepLen , pos(fSep,Texto)-(pos(vSep,Texto)+SepLen));

  Result := new(PSwLine);
  Result^.Field := Field;
  Result^.Value := Value;

  fValue.Add(Result);
  end;
  end;
  //
  end;

  //
  //  Prepara mensagem para enviar
  //
  function TSwMessage.ToText: AnsiString;
  var i:integer;
  begin
  result := '';
  for i:=0 to fValue.Count-1 do
  begin
  result := result + fSep + TSwLine(fValue[i]^).Field + vSep + TSwLine(fValue[i]^).Value;
  end;
  result := result + fSep;
  end;

  //
  // TSwField

  function TSwField.AsBoolean: Boolean;
  begin
  result := StrToBool(fValue.Value);
  end;

  function TSwField.AsInteger: Integer;
  begin
  result := StrToInt(fValue.Value);
  end;

  function TSwField.AsPointer: PAnsiChar;
  begin
  result := PAnsiChar(fValue.Value);
  end;

  function TSwField.AsString: AnsiString;
  begin
  result := fValue.Value;
  end;
*)

function TPluginList.GetFromNick(Nick: AnsiString): integer;
var
    i: integer;
begin
    i := pos('(', Nick);
    if (i > 0) then
        Nick := trim(copy(Nick, 1, i - 1));

    result := -1;
    for i := 0 to Count - 1 do
    begin
        if trim(LowerCase(TPlugin(fPlugins.Items[i]^).Nome)) = trim(LowerCase(Nick)) then
        begin
            result := i;
            Exit;
        end;
    end;
    /// /
end;

procedure TPlugin.Show;
begin
    SendMsg(integer(MT_Show));
end;

function TPlugin.Visible: Boolean;
begin
    //
    result := Boolean(SendMsg(integer(MT_Visible)));
    //
end;

function TPluginList.Seek(Index: integer): Boolean;
begin
    if (Index < Count) and (Index >= 0) then
    begin
        fIndex := index;
        result := true;
    end
    else
        result := false;
end;

//
// TPluginMemoryData

procedure TPluginMemoryData.Add(Valor: TPluginMemoryDataRecord);
var
    P: PPluginMemoryDataRecord;
    x: TList;
begin
    P := new(PPluginMemoryDataRecord);
    P^ := Valor;
    inherited Add(P);
end;

constructor TPluginMemoryData.Create;
begin
    inherited Create;
end;

function TPluginMemoryData.GetItem(index: integer): TPluginMemoryDataRecord;
begin
    result := TPluginMemoryDataRecord(Items[index]^);
end;

procedure TPluginMemoryData.SetItem(index: integer; const Value: TPluginMemoryDataRecord);
begin
    TPluginMemoryDataRecord(Items[index]^) := Value;
end;

//
// TPluginRegData

function TPluginRegData.Add(const Valor: TPluginMemoryData): integer;
var
    P: PPluginMemoryData;
begin
    P := new(PPluginMemoryData);
    P^ := Valor;
    inherited Add(P);
end;

constructor TPluginRegData.Create;
begin
    inherited Create;
end;

function TPluginRegData.GetItem(index: integer): TPluginMemoryData;
begin
    result := TPluginMemoryData(Items[index]^);
end;

function TPluginRegData.IndexOf(Hack: ShortString): integer;
var
    i: integer;
begin
    result := -1;
    for i := 0 to Count - 1 do
    begin
        if LowerCase(trim(TPluginMemoryData(Items[i]^).Hack)) = LowerCase(trim(Hack)) then
        begin
            result := i;
            Exit;
        end;
    end;
end;

procedure TPluginRegData.SetItem(index: integer; const Value: TPluginMemoryData);
begin
    TPluginMemoryData(Items[index]^) := Value;
end;

end.
