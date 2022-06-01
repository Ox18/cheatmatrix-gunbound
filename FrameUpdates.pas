unit FrameUpdates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, CheckLst, Gauges, ButtonWithColor, ExtCtrls, SkySql, constantes,
  ComCtrls, utils, skyio,ExtActns;

type
  TframeUpd = class(TFrame)
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    PageControl3: TPageControl;
    TabSheet5: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    lblToInstall: TLabel;
    Label7: TLabel;
    lblToDown: TLabel;
    Label11: TLabel;
    lblSpeed: TLabel;
    Label13: TLabel;
    lblTime: TLabel;
    Label8: TLabel;
    lblTRestante: TLabel;
    verDisp: TCheckBox;
    ListAllGames: TCheckBox;
    pTotal: TGauge;
    pParcial: TGauge;
    TabSheet6: TTabSheet;
    GameList: TComboBox;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    lstPAll: TListView;
    TabSheet4: TTabSheet;
    lstPDisp: TListView;
    TabSheet1: TTabSheet;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    lstCAll: TListView;
    TabSheet7: TTabSheet;
    lstCDisp: TListView;
    Procedure CMUPDConnect;
    procedure lstCAllCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lstCAllCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure btnBaixarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnEsconderClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure BitBtnWithColor1Click(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    {procedure URL_OnDownloadProgress
        (Sender: TDownLoadURL;
         Progress, ProgressMax: Cardinal;
         StatusCode: TURLDownloadStatus;
         StatusText: String; var Cancel: Boolean) ;}
  public
    { Public declarations }
    Procedure UpdateList;
    Function SomeFileInDir:boolean;
    function DownloadURL_NOCache(const aUrl: AnsiString; fsize: cardinal; s: AnsiString): integer;
    Function CalculaTime: AnsiString;
    
  end;

    Function CheckFileState(arq: AnsiString; data: AnsiString): integer;
    
  type
    TDownList = record
      Arquivo: AnsiString;
      Size: cardinal;
    end;

  var 
      UpdateList: array of TDownList;
      DownList: array of TDownList;
      CancelNow: boolean = false;
      pTotalSize: cardinal;
      pTotalAtual: cardinal = 0;
      DownSTime: cardinal;
      baixados, abaixar: integer;
      totalToDown : integer = 0;

implementation

{$R *.dfm}

uses Common, DateUtils, UnitMatriz, Wininet;

Function TframeUpd.SomeFileInDir:boolean;
var s: TSearcher; 
begin
  s := TSearcher.Create;
  s.Dir := ExtractFilePath( ParamStr(0) )+'Updates\';
  s.Extensions.Add('*.cmp');
  s.Results.Clear;
  s.StartSearch;
  lblToInstall.Caption := inttostr(s.Results.Count);
  result := not s.Results.Eof;
  s.Free;
  
end;

{function DownloadFile(Source, Dest: string): Boolean;
begin
  try
     Result := UrlDownloadToFile(nil, PChar(source), PChar(Dest), 0, nil) = 0;
  except
     Result := False;
  end;
end; }

Function TframeUpd.CalculaTime: AnsiString;
var ms,s,m,h, t: cardinal; 
begin
    ms := GetTickCount - DownSTime;
    t := (pTotalSize * ms) div pTotalAtual;

    s := t div 1000;
    t := t mod 1000;

    m := s div 60;
    s := s mod 60;

    h := m div 60;
    h := h mod 60;

    result := FormatFloat('00',h)+':'+FormatFloat('00',m)+':'+FormatFloat('00',s);
end;

Function CalculaSpeed: AnsiString;
var ms,s,m,h, t: cardinal;
begin
    ms := GetTickCount - DownSTime;
    ms := ms div 60000;
    if ms = 0 then
       ms := 1;

    s := (pTotalAtual div 1024) div ms;
    result := FormatFloat('0.0',s)+' kb/s';
end;

Function FormatDownSize(valor: cardinal): AnsiString;
var b,k,m: double;
begin
    b := valor;
    k := b / 1024;
    m := k / 1024;

    if k < 1 then
       result := FormatFloat('0.00', b)+' bt'
    else if m < 1 then
       result := FormatFloat('0.00',k)+' kb'
    else
       result := FormatFloat('0.00',m)+' mb'
end;

(*procedure TframeUpd.URL_OnDownloadProgress(Sender: TDownLoadURL; Progress, ProgressMax: Cardinal;
         StatusCode: TURLDownloadStatus;  StatusText: String; var Cancel: Boolean);
var ind: integer;
    val, valor,i,j: integer;
    v: string;

begin
  if CancelNow then
  begin
      cancel := true;
      exit;
  end;

  pParcial.MaxValue := ProgressMax;
  pParcial.Progress := Progress;

  //inc(pTotalAtual, BytesRead);
  //inc(bTotal, BytesRead);

  try
      if totalToDown = 0 then totalToDown := 1;
      ind := 10000 div totalToDown;
      if ProgressMax > 0 then
         ProgressMax := 1;
      val := (Progress * ind) div ProgressMax;
      pTotal.Progress := ind * baixados + val;

      valor := 0;
      for i := 0 to baixados-1 do
          inc(valor,DownList[i].Size);
      j := DownList[baixados].Size;
      pTotalAtual := ((Progress*j) div ProgressMax) + valor;

      v := CalculaTime;
      lblTime.Caption := v;
      //v := CalculaSpeed;
      lblSpeed.Caption := v;
      lblTRestante.Caption := FormatDownSize(pTotalSize-pTotalAtual);
  except
    on  e:exception do
  end;
end;

function TframeUpd.DownloadURL_NOCache(const aUrl: string; fsize: cardinal; arq: String): integer;
//var Down: TDownloadURL;
begin
   //Down := TDownLoadURL.Create(self);
   result := 0;
   with TDownloadURL.Create(self) do
   try
     URL := aUrl;
     FileName := arq;
     OnDownloadProgress := URL_OnDownloadProgress;
     ExecuteTarget(nil) ;
     if CancelNow then
     begin
         CancelNow := false;
         result := 2;
         exit;
     end;
   finally
     Free;
   end;
   result := 1;
end; *)

var TotalDown: int64;
var TotalArqsDown: int64;

function TframeUpd.DownloadURL_NOCache(const aUrl: AnsiString; fsize: cardinal; s: AnsiString): integer;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  lpBuffer: array[0..1024] of byte;
  dwBytesRead: DWORD;
  btotal: cardinal;
  v: AnsiString;
  buf: TMemoryStream;
  localFile: File;
  f: byte;
begin
  if FileExists(s) then
     DeleteFile(s);
     
   TotalDown := 0;

  Result := 0;
  buf := TMemoryStream.Create;
  buf.Seek(0, soFromBeginning);
  pParcial.Progress := 0;
  // hSession := InternetOpen( 'MyApp', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  hSession := InternetOpen( PChar(Application.Title), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if Assigned(hSession) then
    begin                                                              //INTERNET_FLAG_RELOAD
      hService := InternetOpenUrl(hSession, PChar(aUrl), pchar(INTERNET_FLAG_DONT_CACHE), 0, 0, 0);
      if Assigned(hService) then
        try
          AssignFile(localFile,s);
          dwBytesRead := 1024;
          while dwBytesRead <> 0 do
          begin
              if CancelNow then
              begin
                  CancelNow := false;
                  result := 2;
                  exit;
              end;
              //dwBytesRead := 1024;
              InternetReadFile(hService,@lpBuffer, 1024,dwBytesRead);
              if dwBytesRead > 0 then
                 buf.WriteBuffer(lpBuffer, dwBytesRead);
              

              //inc(pTotalAtual, dwBytesRead);
              //inc(bTotal, dwBytesRead);
              inc(TotalDown, dwBytesRead);
              try
              if(fsize > 0) then
              begin
                  pParcial.Progress := (TotalDown*pParcial.MaxValue) div fsize;
                  pTotal.Progress := ((TotalArqsDown+TotalDown)*pTotal.MaxValue) div pTotalSize;
              end;
              //Splash.pParcial.Progress := (TotalDown*Splash.pParcial.MaxValue) div fsize;
              //Splash.pTotal.Progress := ((TotalArqsDown+TotalDown)*Splash.pTotal.MaxValue) div pTotalAtualn;

              v := CalculaTime;
              lblTime.Caption := v;
              //v := CalculaSpeed;
              lblSpeed.Caption := v;
              lblTRestante.Caption := FormatDownSize(pTotalSize-pTotalAtual);
              except
                on e:exception do
              end;
          end;
          f := 0;
          Result := 1;
        finally
          InternetCloseHandle(hService);
        end;
    end;
  finally
    pParcial.Progress := 10000;
    //pTotal.Progress := 10000;
    InternetCloseHandle(hSession);

    if FileExists(s) then
      begin
          try
             DeleteFile(s);
          except
          end;
    end;

    buf.Seek(0,soFromBeginning);
    buf.SaveToFile(s);
    TotalArqsDown := TotalArqsDown + fsize;
  end;
end;

{*************************************************************
*  0 - Não possui o arquivo
*  1 - O Arquivo está desatualizado
*  2 - OK
**************************************************************}

Function CheckFileState(arq: AnsiString; data: AnsiString): integer;
var fData: TDateTime; cData: TDateTime;
    Aplicacao : THandle;
    FPath: AnsiString;
    Info1,Info2,Info3: TFileTime;
    Estrutura : SystemTime;
begin
    FPath := ExtractFilePath( ParamStr(0) )+'Plugins\'+ arq;
    if FileExists( FPath ) then
    begin
        try
             fData := StrToDateTime( trim(data) );
        except
          on e:exception do
             fData := MinDateTime;
        end;

        try            //formatdatetime('dd/mm/yyyy hh:mm:ss', cdata)+'  -  '+formatdatetime('dd/mm/yyyy hh:mm:ss', fdata)
            Aplicacao := FileOpen(FPath, fmOpenRead or fmShareDenyNone);
            if Aplicacao > 0 then
            begin
                GetFileTime( Aplicacao, @Info1, @Info2, @Info3);
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
    end else
    begin
        result := 0;
    end;
end;

Procedure PutOnStatus(msg: AnsiString);
begin
    frmMatriz.Caption := msg;
end;

Procedure TframeUpd.UpdateList;
var flag: boolean;
    s: char;
    Data: TDateTime;
    i: integer;
    nom_, arq_, aut_, prm_, hid_, siz_, dat_, jog_:AnsiString;
    tip_, mod_: integer;
    Sock: TSwSQL;
    gid: AnsiString;
begin

  lstPDisp.Clear;
  lstPAll.Clear;
  lstCDisp.Clear;
  lstCAll.Clear;

  Sock := TSwSQL.Create;
try
  Sock.Clear;
  Sock.Port := ConnectionPort;
  Sock.Host := AnsiString(Constante(0064, false))+AnsiString(Constante(0069, false));
  Sock.OnExecute := CMUPDConnect;

  Sock.Parameters.Clear;
  i := 1;
  if verDisp.Checked then
     i := 2;

  Sock.Parameters.Add(inttostr(4338), GetHDCode, SE_MD5, SE_ENCRYPT);
  Sock.Parameters.Add(inttostr(4339), inttostr(i));

  if ListAllGames.Checked then
     i := 0
  else begin
      if GameList.ItemIndex = -1 then
      begin
          MessageBox(0, Pchar(Constante(0046, true)),PChar(Constante(0047, true)),MB_ICONWARNING);
          exit;
      end;
      gid := copy(GameList.Items.Strings[GameList.ItemIndex], 1, 5);
      if not IsInt(gid) then
      begin
          MessageBox(0, Pchar(Constante(0048, true)),PChar(Constante(0047, true)),MB_ICONWARNING);
          exit;
      end;
      i := strtoint(gid);
  end;

  Sock.Parameters.Add(inttostr(4340), intToStr(i), SE_MD5, SE_ENCRYPT);

  flag := Sock.Execute;
  if flag then
  begin

     while not Sock.EOF do
     begin
        nom_ := trim(Sock.FieldByName( inttostr(6021) ).AsString);
        arq_ := trim(Sock.FieldByName( inttostr(6022) ).AsString);
        aut_ := trim(Sock.FieldByName( inttostr(6024) ).AsString);
        prm_ := trim(Sock.FieldByName( inttostr(6025) ).AsString);
        mod_ := Sock.FieldByName( inttostr(6026) ).AsInteger;
        hid_ := trim(Sock.FieldByName( inttostr(6027) ).AsString);
        siz_ := trim(Sock.FieldByName( inttostr(6028) ).AsString);
        dat_ := trim(Sock.FieldByName( inttostr(6029) ).AsString);
        tip_ := Sock.FieldByName( inttostr(6030) ).AsInteger;
        jog_ := trim(Sock.FieldByName( inttostr(6031) ).AsString);

        i := CheckFileState(arq_, dat_);
        case i of
          0: s := 'N';
          1: s := 'D';
          2: s := 'O';
        end;

        if not verDisp.Checked then
        case tip_ of
           1: begin { Controle }
                  lstCAll.Items.Add;
                  try
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].Caption := hid_;
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(nom_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(arq_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(jog_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(siz_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(dat_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(aut_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(prm_);
                    lstCAll.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(s);
                  except
                      on e:exception do
                  end;
              end;
           0: begin  { Plugin }
                  lstPAll.Items.Add;
                  try
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].Caption := hid_;
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(nom_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(arq_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(jog_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(siz_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(dat_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(aut_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(prm_);
                    lstPAll.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(s);
                  except
                     on e:exception do
                  end;
              end;
        end;

        if mod_ = 2 then
        begin
            case tip_ of
               1: begin { Controle }
                      lstCDisp.Items.Add;
                      try
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].Caption := hid_;
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(nom_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(arq_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(jog_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(siz_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(dat_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(aut_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(prm_);
                        lstCDisp.Items.Item[lstCAll.Items.Count - 1].SubItems.Add(s);
                      except
                        on e:exception do
                      end;
                  end;
               0: begin  { Plugin }
                      lstPDisp.Items.Add;
                      try
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].Caption := hid_;
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(nom_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(arq_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(jog_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(siz_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(dat_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(aut_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(prm_);
                        lstPDisp.Items.Item[lstPAll.Items.Count - 1].SubItems.Add(s);
                      except
                        on e:exception do
                      end;
                  end;
            end;
        end;
         
        Sock.Next;
     end; 

  end else
     PutOnStatus(Constante(0045, true));
  finally
    Sock.Free;
  end;  
end;

procedure TframeUpd.CMUPDConnect;
begin
//
end;

procedure TframeUpd.lstCAllCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  sLinha:AnsiString;
  cor: TColor;
begin
  try
    if Item.SubItems.Count < 8 then
       exit; 
    sLinha := Item.SubItems.Strings[7]; 

    //Sender.Canvas.FillRect(Rect);
    if State = [cdsSelected, cdsFocused]  then
      Sender.Canvas.Font.Color := clBlack
    else
    begin
      if length(sLinha) > 0 then
      case sLinha[1] of
          'N': cor := clBlue;
          'D': cor := clRed;
          'O': cor := clBlack;
      end;
    end;

    Sender.Canvas.Font.Color := cor;
    //Sender.Canvas.TextOut(Sender.Left,Sender.Top,  FormatFloat('00000', strtoint(Copy(sLinha, 2, Length(sLinha)))) );
    //Item.Caption := copy( sLinha, 2, Length(sLinha) );
    //Sender.Canvas.TextOut( .Left+5,Rect.Top, Copy(sLinha, 2, Length(sLinha)));
  except
   on e:exception do
  end; 
end;

procedure TframeUpd.lstCAllCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  sLinha:AnsiString;
  subtext: AnsiString;
  cor: TColor;
begin
  try
    //sLinha := Item.Caption;
    if Item.SubItems.Count < 8 then
       exit;

    subtext := Item.SubItems.Strings[7];

    //Sender.Canvas.FillRect(Rect);
    if State = [cdsSelected, cdsFocused]  then
       Sender.Canvas.Font.Color := clBlack
    else
    begin
        if length(subtext) > 0 then
        case subtext[1] of
          'N': cor := clBlue;
          'D': cor := clRed;
          'O': cor := clBlack;
        end;
    end;

    Sender.Canvas.Font.Color := cor;
    //Sender.Canvas.TextOut(Rect.Left+5,Rect.Top, subtext);
  except
   on e:exception do
  end;
end;

Procedure FillDownList(list_: TListItems);
var i:integer;
begin
  for i := 0 to list_.Count-1 do
  begin
      if list_.Item[i].Checked then
      begin
          setlength(DownList, length(DownList)+1);
          DownList[high(DownList)].Arquivo := list_.Item[i].SubItems.Strings[1];
          DownList[high(DownList)].Size := strtoint(list_.Item[i].SubItems.Strings[3]);
      end;
  end;
end;

procedure TframeUpd.btnBaixarClick(Sender: TObject);
var i,j:integer;
    s: AnsiString;
    fPath: AnsiString;
begin
  baixados := 0;
  abaixar := 0;
  DownSTime := GetTickCount;
  pTotal.Progress := 0;
  pTotalAtual := 0;
  pTotalSize := 0;
  fPath := ExtractFilePath( ParamStr(0) )+'Updates\';
  DownList := nil;


  FillDownList(lstPDisp.Items);
  FillDownList(lstCDisp.Items);

  for i:=0 to length(DownList)-1 do
  begin
      inc(pTotalSize, DownList[i].Size);
  end;

  abaixar := length(DownList);
  totalToDown := length(DownList);
  for i:=0 to totalToDown-1 do
  begin
      s := '';

      if not DirectoryExists( fPath ) then
         CreateDir(fPath);
         
      j := DownloadURL_NOCache(Constante(0064, false)+'/cmx/CMUpdates/'+DownList[i].Arquivo, DownList[i].Size, fPath+DownList[i].Arquivo);

      if j = 2 then
         exit;
      if j = 1 then
        inc(baixados);
      if j = 0 then
        continue;

      lblToDown.Caption := inttostr(abaixar-baixados);
      lblToInstall.Caption := inttostr(baixados);

      //SaveDialog1.Files.Text := s;
      //SaveDialog1.Files.SaveToFile(fPath+DownList[i].Arquivo);
  end;
  pTotal.Progress := pTotal.MaxValue;

  btnBaixar.Enabled := true;
  btnAtualizar.Enabled := true;
  btnLimpar.Enabled := true;
  if SomeFileInDir then
     btnInstalar.Enabled := true;
  DownList := nil;
end;

procedure TframeUpd.btnCancelarClick(Sender: TObject);
begin
  CancelNow := true;
end;

procedure TframeUpd.btnEsconderClick(Sender: TObject);
begin
Hide;
end;

procedure TframeUpd.btnLimparClick(Sender: TObject);
var f: TSearcher;
begin
    f := TSearcher.Create;
    f.Extensions.Clear;
    f.Extensions.Add('*.cmp');
    f.Dir := ExtractFilePath( ParamStr(0) )+'Updates\';
    f.StartSearch;
    f.Results.Reset;
    while not f.Results.Eof do
    begin
       try
           DeleteFile(f.Results.CurrentResult.Path);
       except
       end;
       f.Results.Next;
    end;
    PutOnStatus(Constante(0049, true));
end;

procedure TframeUpd.BitBtnWithColor1Click(Sender: TObject);
var
    Sock: TSwSQL;
    gid: AnsiString;
begin
  GameList.Clear;
  Sock := TSwSQL.Create;
try
  Sock.Clear;
  Sock.Port := ConnectionPort;
  Sock.Host := AnsiString(Constante(0064, false))+AnsiString(Constante(0069, false));
  Sock.OnExecute := CMUPDConnect;
  sock.Parameters.Add(inttostr(5933),'---',SE_MD5, SE_MD5);
  sock.Execute;
  sock.Reset;
  while not sock.EOF do
  begin
      try
          GameList.Items.Add(formatfloat('00000', sock.fieldByName(inttostr(7010)).AsInteger)+' - '+sock.fieldByName(inttostr(7011)).AsString);
      except
        on e:exception do
      end;
      sock.Next;
  end;
except
  on e:exception do
end;
end;

procedure TframeUpd.btnAtualizarClick(Sender: TObject);
begin
UpdateList;
if SomeFileInDir then
     btnInstalar.Enabled := true;
end;

procedure TframeUpd.Timer1Timer(Sender: TObject);
begin
timer1.Enabled := false;
//if Visible then
  if SomeFileInDir then
     btnInstalar.Enabled := true;
end;

end.
