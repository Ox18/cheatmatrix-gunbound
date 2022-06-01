unit SckData;

interface

uses windows, sysutils, classes, ScktComp, SkySck, Contnrs;

Type
  PClientData = ^TClientData;
  TClientData = Packed Record
       Tipo: Integer;
       Color: integer;
       Texto: shortstring;
       Nick: shortstring;
       NovoNick: shortstring;
       Nome: shortstring;
       Mail: shortstring;
       RemoteIP: shortstring;
       LanIP: shortstring;
       LocalAddress: shortstring;
       Diretorio: shortstring;
       SOVersion: shortstring;
       SOCompilation: shortstring;
       SOAditional: shortstring;
       SO: shortstring;
       SOData: shortstring;
       Ping: cardinal;
  end;

  TServerData = Packed Record
       Tipo: Integer;
       Texto: ShortString;
       Valor: Integer;
       Hack: ShortString;
       Color: integer;
       ToReg: string;
       Conected: Array of shortstring;
  end;

type
       //TDeleteType = (S_Nick, S_Ip);
       TIpType = (IT_Remote, IT_Local, IT_Lan);

       PClientes = ^TClientes;
       TClientes = Class
       Constructor Create(Sock: Pointer);
       private
         FCliente: integer;
         fList: TList;
         fSock: Pointer;
         Function FClientCount: integer;
         Function GetCliente(index: integer): TClientData;
         Procedure SetCliente(index: integer; const cli: TClientData);
       public
         Procedure Add(Cliente: TClientData);
         Property Cliente[index: integer]:TClientData read GetCliente write SetCliente;
         Procedure Delete(index: integer);
         Procedure DeleteByNick(Nick: string);
         Procedure DeleteByIP(IP: string; Tipo: TIpType = IT_Remote);
         Function ClientByIp(Ip: string; Tipo: TIpType = IT_Remote):Integer;
         Function ClientByNick(Nick: String):Integer;
         Function ClientByHdCode(Code: String):Integer;
         Function GetConnection(index: integer): integer;
         Procedure ChangeNick(Novo,Anterior: shortstring);
         Procedure RefreshPing(IP: string);
       published
         Property ClientCount:integer read FClientCount;
  end;

  Procedure BufToClientData(Buf: TSkySck; var Data: TClientData);
  Procedure ClientDataToBuf(Data: TClientData; var Buf: TSkySck);

  Procedure BufToServerData(Buf: TSkySck; var Data: TServerData);
  Procedure ServerDataToBuf(Data: TServerData; var Buf: TSkySck);

implementation

  Procedure BufToServerData(Buf: TSkySck; var Data: TServerData);
  var i:integer;
  begin
      Data.Tipo        := strtoint(Buf.FieldByName('Tipo'));
      Data.Texto       := Buf.FieldByName('Texto');
      Data.Valor       := strtoint(Buf.FieldByName('Valor'));
      Data.Hack        := Buf.FieldByName('Hack');
      Data.Color       := strtoint(Buf.FieldByName('Color'));
      Data.ToReg       := Buf.FieldByName('ToReg');
      Data.Conected := nil;
      for i := 0 to Buf.Count-1 do
      begin
          if Buf.Fields[i].FieldName = 'Conected' then
          begin
            setlength(Data.Conected,length(Data.Conected)+1);
            Data.Conected[high(Data.Conected)] := Buf.Fields[i].Value;
          end;
      end;
  end;

  Procedure ServerDataToBuf(Data: TServerData; var Buf: TSkySck);
  var i:integer;
  begin
      Buf.Clear;
      Buf.Add('Tipo',  inttostr(Data.Tipo));
      Buf.Add('Valor', inttostr(Data.Valor));
      Buf.Add('Color', inttostr(Data.Color));
      Buf.Add('Texto', Data.Texto);
      Buf.Add('Hack',  Data.Hack);
      Buf.Add('ToReg', Data.ToReg);
      for i:=0 to length(Data.Conected)-1 do
      begin
          Buf.Add('Conected', Data.Conected[i]);
      end;
  end;

  Procedure BufToClientData(Buf: TSkySck; var Data: TClientData);
  begin
      Data.Tipo         := strtoint(Buf.FieldByName('Tipo'));
      Data.Color        := strtoint(Buf.FieldByName('Color'));
      Data.Texto        := Buf.FieldByName('Texto');
      Data.Nick         := Buf.FieldByName('Nick');
      Data.NovoNick     := Buf.FieldByName('NovoNick');
      Data.Nome         := Buf.FieldByName('Nome');
      Data.Mail         := Buf.FieldByName('Mail');
      Data.RemoteIP     := Buf.FieldByName('RemoteIP');
      Data.LanIP        := Buf.FieldByName('LanIP');
      Data.LocalAddress := Buf.FieldByName('LocalAddress');
      Data.SOVersion    := Buf.FieldByName('SOVersion');
      Data.SOCompilation:= Buf.FieldByName('SOCompilation');
      Data.SOAditional  := Buf.FieldByName('SOAditional');
      Data.SO           := Buf.FieldByName('SO');
      Data.SOData       := Buf.FieldByName('SOData');
  end;

  Procedure ClientDataToBuf(Data: TClientData; var Buf: TSkySck);
  begin
      Buf.Add('Tipo',         inttostr(Data.Tipo));
      Buf.Add('Color',        inttostr(Data.Color));
      Buf.Add('Texto',        (Data.Texto));
      Buf.Add('Nick',         (Data.Nick ));
      Buf.Add('NovoNick',     (Data.NovoNick));
      Buf.Add('Nome',         (Data.Nome ));
      Buf.Add('Mail',         (Data.Mail));
      Buf.Add('RemoteIP',     (Data.RemoteIP));
      Buf.Add('LanIP',        (Data.LanIP ));
      Buf.Add('LocalAddress', (Data.LocalAddress));
      Buf.Add('SOVersion',    (Data.SOVersion));
      Buf.Add('SOCompilation',(Data.SOCompilation));
      Buf.Add('SOAditional',  (Data.SOAditional));
      Buf.Add('SO',           (Data.SO ));
      Buf.Add('SOData',       (Data.SOData));
  end;

  { TCliente }




{ TClientes }

procedure TClientes.Add(Cliente: TClientData);
var p: PClientData;
begin
  p := new(PClientData);
  p^ := Cliente;
  fList.Add(p);
end;

procedure TClientes.ChangeNick(Novo, Anterior: shortstring);
begin
  TClientData(fList[ClientByNick(Anterior)]^).Nick := Novo;
end;

function TClientes.ClientByHdCode(Code: String): Integer;
var i:integer;
begin
   Result := -1;
   for i := 0 to fList.Count-1 do
   begin
      if TClientData(fList[i]^).SOData = Code then
      begin
          result := i;
          exit;
      end;
   end; 
end;

function TClientes.ClientByIp(Ip: string; Tipo: TIpType): Integer;
var i:integer;
begin
   Result := -1;
   for i := 0 to fList.Count-1 do
   begin
      case integer(Tipo) of
        integer(IT_Remote):
              if TClientData(fList[i]^).RemoteIP = Ip then
              begin
                  result := i;
                  exit;
              end;
        integer(IT_Local):
              if TClientData(fList[i]^).LocalAddress = Ip then
              begin
                  result := i;
                  exit;
              end;
        integer(IT_Lan):
              if TClientData(fList[i]^).LanIP = Ip then
              begin
                  result := i;
                  exit;
              end;
      end;
   end;
end;

function TClientes.ClientByNick(Nick: String): Integer;
var i:integer;
begin
   Result := -1;
   for i := 0 to fList.Count-1 do
   begin
      if TClientData(fList[i]^).Nick = Nick then
      begin
          result := i;
          exit;
      end;
   end;
end;

constructor TClientes.Create(Sock: Pointer);
begin
   fList := TList.Create;
   fSock := Sock
end;

procedure TClientes.Delete(index: integer);
begin
   fList.Delete(index);
end;

procedure TClientes.DeleteByIP(IP: string; Tipo: TIpType);
begin
   fList.Delete( ClientByIp(IP, Tipo) );
end;

procedure TClientes.DeleteByNick(Nick: string);
begin
   fList.Delete( ClientByNick(Nick) );
end;

function TClientes.FClientCount: integer;
begin
  Result := fList.Count;
end;

function TClientes.GetCliente(index: integer):TClientData ;
begin
  result := TClientData(fList[index]^);
end;

function TClientes.GetConnection(index: integer): integer;
var i:integer;
begin
    result := -1;
    for i:= 0 to TServerSocket(fSock^).Socket.ActiveConnections-1 do
    begin
        if TServerSocket(fSock^).Socket.Connections[i].RemoteAddress = Cliente[index].RemoteIP then
        begin
            result := i;
            exit;
        end;
    end;
end;

procedure TClientes.RefreshPing(IP: string);
begin
   TClientData(fList[ClientByIp(IP)]^).Ping := GetTickCount;
end;

procedure TClientes.SetCliente(index: integer; const cli: TClientData);
var p: PClientData;
begin
  p := new(PClientData);
  p^ := cli;
  fList[index] := p;
end;

end.
