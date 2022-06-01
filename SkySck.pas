unit SkySck;

interface

uses windows, sysutils, classes, ScktComp, CoreFunctions;
(*
const
          FSep = '{[(_!.+.!_)]}';
          VSep = '{[(_=.+.=_)]}';

type
   PMData = ^TMicroData;
   TMicroData = record
      FieldName: shortstring;
      Value: shortstring;
   end;

   SckMode = (SM_Server, SM_Client);

   TSkySck = class(TPersistent)
   constructor Create(Socket: Pointer; Modo: SckMode);
   private
      fMode: SckMode;
      fIndex: integer;
      SckList: TList;
      Sock: Pointer;
      Function fGetCount: integer;
      Function GetFields(Index: integer):TMicroData;
      Procedure SetFields(Index: integer; const Valor: TMicroData);
      Function GetField: TMicroData;
      Procedure SetField(const Valor: TMicroData);
   public
      Property Count: integer read fGetCount;
      Property Field: TMicroData read GetField write SetField;
      Property Fields[index: integer]: TMicroData read GetFields write SetFields;
      Function Receive(Data: string): boolean;
      Function Send: boolean;
      function SendEx(var _Sock: TCustomWinSocket): boolean;
      Function SendTo(sTo: integer):boolean;
      Procedure Add(FieldName,Valor: string);
      Procedure Clear;
      Function FieldbyName(Nome: string): string;
      Function Next: boolean;
      Function Back: boolean;
      Function EOF: boolean;
   end;  *)

implementation

{ TSkySck }
(*
procedure TSkySck.Add(FieldName, Valor: string);
var p: PMData;
begin
p := new(PMData);
p^.FieldName := FieldName;
p^.Value := Valor;
SckList.Add(p);
end;

function TSkySck.Back: boolean;
begin
  dec(fIndex);
  if fIndex+1 < 0 then
     result := false
  else
     result := true;
end;

procedure TSkySck.Clear;
begin
   SckList.Clear;
end;

constructor TSkySck.Create(Socket: Pointer; Modo: SckMode);
begin
  fMode:= Modo;
  Sock:= Socket;
  SckList := TList.Create;
  fIndex := -1;
end;

function TSkySck.EOF: boolean;
begin
  if fIndex >= SckList.Count then
    result:=true
  else
    result := false;
end;

function TSkySck.fGetCount: integer;
begin
  result := SckList.Count;
end;

function TSkySck.FieldbyName(Nome: string): string;
var i:integer;
begin
  result := '';
  for i := 0 to SckList.Count-1 do
  begin
      if TMicroData(SckList.Items[i]^).FieldName = Nome then
      begin
          result := TMicroData(SckList.Items[i]^).Value;
          exit;
      end;
  end;
end;

function TSkySck.GetField: TMicroData;
var n: TMicroData;
begin
  if (fIndex >=0) and (fIndex < SckList.Count) then
    result := TMicroData(SckList[fIndex]^)
  else
    result := n;
end;

function TSkySck.GetFields(Index: integer): TMicroData;
begin
   result := TMicroData(SckList.Items[index]^);
end;

function TSkySck.Next: boolean;
begin
  inc(fIndex);
  if fIndex <= SckList.Count-1 then
    result:=true
  else
    result:=false;
end;

function TSkySck.Receive(Data: string): boolean;
var i,j,k:integer; s,d:string;
    f,v:string; 
begin
   Data := DecryptValue(Data);
   result:=false;
   s := Data;
   k := length(fsep);
   while pos(fSep,s)>0 do
   begin
       j:=pos(fSep,s);                   //posiçao do marcador de 'field'
       s:=Copy(s,j+k,length(s)-(j+k)+1); //copia o restante depois do marcador de 'field'
       if pos(vsep,s) > 0 then           //se existir um marcador de 'value'
       begin
          f:=copy(s,1,pos(vsep,s)-1);
          v:=copy(s,pos(vsep,s)+k ,pos(fsep,s)-(pos(vsep,s)+k));
          Add(f,v);
          result := true;
       end;
   end;
end;

function TSkySck.Send: boolean;
var i:integer;
    buf: string;
begin
    buf := '';
    for i:=0 to SckList.Count-1 do
    begin
        buf := buf + FSep + TMicroData(SckList.Items[i]^).FieldName + VSep + TMicroData(SckList.Items[i]^).Value;
    end;
    buf := buf + FSep;
    buf := EncryptValue(buf);

    case integer(fMode) of
      integer(SM_Client):
      begin
        TClientSocket(Sock^).Socket.SendText(buf);
      end;

      integer(SM_Server):
      begin
          for i := 0 to TServerSocket(Sock^).Socket.ActiveConnections-1 do
          begin
              TServerSocket(Sock^).Socket.Connections[i].SendText(buf);
          end;
      end;
    end;
end;

function TSkySck.SendEx(var _Sock: TCustomWinSocket): boolean;
var i:integer;
    buf: string;
begin
    Result := true;
    buf := '';
    for i:=0 to SckList.Count-1 do
    begin
        buf := buf + FSep + TMicroData(SckList.Items[i]^).FieldName + VSep + TMicroData(SckList.Items[i]^).Value;
    end;
    buf := buf + FSep;
    buf := EncryptValue(buf);

    _Sock.SendText(buf);
end;

function TSkySck.SendTo(sTo: integer): boolean;
var i:integer;
    buf: string;
begin
    buf := '';
    for i:=0 to SckList.Count-1 do
    begin
        buf := buf + FSep + TMicroData(SckList.Items[i]^).FieldName + VSep + TMicroData(SckList.Items[i]^).Value;
    end;
    buf := buf + FSep;
    buf := EncryptValue(buf);

    case integer(fMode) of
      integer(SM_Client):
      begin
        TClientSocket(Sock^).Socket.SendText(buf);
      end;

      integer(SM_Server):
      begin
          TServerSocket(Sock^).Socket.Connections[sTo].SendText(buf);
      end;
    end;
end;

procedure TSkySck.SetField(const Valor: TMicroData);
var p:PMData;
begin
  p := new(PMData);
  p^:= valor;
  if (fIndex >= 0) and (fIndex < SckList.Count) then
    SckList[fIndex] := p;
end;

procedure TSkySck.SetFields(Index: integer; const Valor: TMicroData);
var p: PMData;
begin
  p := new(PMData);
  p^ := valor;
  SckList.Items[index] := p;
end;   *)

end.
 