unit CMStatus;

interface

uses windows, graphics, classes, sysutils;

type
  PStatusMessage = ^TStatusMessage;
  TStatusMessage = record
    Nome: shortstring;
    Menssage: shortstring;
    Tempo: cardinal;
    Decorrido: cardinal;
    Cor: TColor;
    Emergencia: Boolean;
    RunTimes: cardinal;
    Efeito: Boolean;
    TickHelper: Cardinal;
  end;

  type
TMessageList = Class
    constructor Create;
    private
      fList: TList;
      fIndex: integer;
      fLast: integer;
      fEmergency: boolean;
    public
      Function CurrentMessage: TStatusMessage;
      Procedure Add(Nome: AnsiString = ' '; Msg: AnsiString = ' '; Delay: Cardinal = 5000;
                RunTimes: integer = 1; Cor: TColor = clBlack; Piscar: Boolean = false; Emergencia: Boolean = false);
      Procedure Delete(Nome: AnsiString);
      Function GetFreeName: AnsiString;
      Function Tick(qnt: integer = 1): boolean;
      Procedure IncHelper(qnt: integer = 1);
      Function Next: TStatusMessage;
      Function Empty: Boolean;
    end;

implementation

{ **************************** TMessageList **********************************}

{ *********************** Mensagens para o Status ****************************}

{-----------------------------------------------------------------------------}
//          Adiciona Mensagem na Liosta de Espera para o Status
{-----------------------------------------------------------------------------}

procedure TMessageList.Add(Nome: AnsiString = ' '; Msg: AnsiString = ' '; Delay: Cardinal = 5000;
                RunTimes: integer = 1; Cor: TColor = clBlack; Piscar: Boolean = false; Emergencia: Boolean = false);
var
  P: PStatusMessage;
begin
  P := New(PStatusMessage);
  P^.Nome := Nome;
  P^.Tempo := Delay;
  P^.Menssage := Msg;
  P^.Cor := Cor;
  P^.Emergencia := Emergencia;
  P^.RunTimes := RunTimes;
  P^.Efeito := Piscar;
  fList.Add(p);
end;

{-----------------------------------------------------------------------------}
//                        Cria Lista de Espera
{-----------------------------------------------------------------------------}

constructor TMessageList.Create;
begin
   fList := TList.Create;
end;

{-----------------------------------------------------------------------------}
//                       Mensagem Atual
{-----------------------------------------------------------------------------}

function TMessageList.CurrentMessage: TStatusMessage;
begin
   Result := TStatusMessage(fList.Items[fIndex]^);
end;

{-----------------------------------------------------------------------------}
//                      Retira Mensagem da Lista
{-----------------------------------------------------------------------------}

procedure TMessageList.Delete(Nome: AnsiString);
var i:integer;
begin
   for i:=0 to fList.Count-1 do
   begin
      if LowerCase(trim(TStatusMessage(fList.Items[i]^).Nome)) = LowerCase(trim(Nome)) then
      begin
          fList.Delete(i);
          exit;
      end;
   end;
end;

{-----------------------------------------------------------------------------}
//           Verifica se a lista de espera do Status está vazia
{-----------------------------------------------------------------------------}

function TMessageList.Empty: Boolean;
begin
   Result := fList.Count = 0;
end;

{-----------------------------------------------------------------------------}
//             Incrementa o campo Helper (para efeito vaga-lume)
{-----------------------------------------------------------------------------}

function TMessageList.GetFreeName: AnsiString;
var i,j:integer;
    Flag: boolean;
    Nome: AnsiString;
begin
   j := 0;
   Flag := true;
   while flag do
   begin
       flag := false;
       Nome := 'Mensagem'+IntToStr(j);
       for i:=0 to fList.Count-1 do
       begin
          if LowerCase(trim(TStatusMessage(fList.Items[i]^).Nome)) = LowerCase(trim(Nome)) then
          begin
              flag := true;
          end;
       end;
       inc(j);
   end;
   result := nome;
end;

procedure TMessageList.IncHelper(qnt: integer);
begin
   TStatusMessage(fList.Items[fIndex]^).TickHelper :=  TStatusMessage(fList.Items[fIndex]^).TickHelper + qnt;
end;

function TMessageList.Next: TStatusMessage;
var i:integer;
    ExisteEmergencia: boolean;
begin
   TStatusMessage(fList.Items[fIndex]^).Decorrido := 0;
   TStatusMessage(fList.Items[fIndex]^).TickHelper := 0;
   //se o numero de amostragens atual for maior que zero ela não é infinita
   if TStatusMessage(fList.Items[fIndex]^).RunTimes > 0 then
   begin
      //Decrementa o numero de amostragens da mensagem atual
      dec(TStatusMessage(fList.Items[fIndex]^).RunTimes);
      //Se apos decrementar o numero de amostragens, este for 0, remove da lista
      if TStatusMessage(fList.Items[fIndex]^).RunTimes = 0 then
      begin
         fList.Delete(fIndex);

         //inc(fIndex);
         if fIndex+1 >= fList.Count then
         begin
             fIndex := 0;
             if fList.Count > 0 then
                result := TStatusMessage(fList.Items[fIndex]^);
             exit;
         end;
      end;
   end;

   //Se for emergencia salva a posição atual e Fura a Fila de mensagens
   ExisteEmergencia := false;
   for i := 0 to fList.Count-1 do
   begin
       if TStatusMessage(fList.Items[i]^).Emergencia then
       begin
           ExisteEmergencia := true;
           //se houver uma emergencia e a ultima msg não tiver sido uma,
           //salva a posição da fila de mensagens
           if not fEmergency then
              fLast := fIndex;

           findex := i;
           break;
       end;
   end;

   //Se não houver emergencias na lista
   if not ExisteEmergencia then
   begin
      //se ja tiver havido uma emergencia na ultima mostrada
      if fEmergency then
      begin
         fEmergency := false;
         fIndex := fLast;
      end else  // senão incrementa a posição na fila
         inc(fIndex);
   end;
   
   if fIndex >= fList.Count then
   begin
       fIndex := 0;
       if fList.Count > 0 then
          result := TStatusMessage(fList.Items[fIndex]^);
       exit;
   end;

   Result := TStatusMessage(fList.Items[fIndex]^);
end;

{-----------------------------------------------------------------------------}
//               Incrementa o Tempo de Amostragem
{-----------------------------------------------------------------------------}

Function TMessageList.Tick(qnt: integer = 1): boolean;
begin
   if TStatusMessage(fList.Items[fIndex]^).Decorrido >= TStatusMessage(fList.Items[fIndex]^).Tempo then
     result := false
   else begin
     inc(TStatusMessage(fList.Items[fIndex]^).Decorrido,qnt);
     result := true;
   end;
end;

end.
