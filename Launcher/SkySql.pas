unit SkySQL;

interface

uses
  Windows, SysUtils, Variants, Classes, IdMultipartFormData, graphics,
  idhttp, forms,StdCtrls, functions, encription,
  Contnrs, StrUtils , winsock, lvkZLibUtils;

var
          FSep :AnsiString ;
          VSep :AnsiString ;

type
  TSWItem = packed record
     FieldName: AnsiString;
     Value: AnsiString;
  end;

  TSwObjectList = class
     constructor Create;
     private
        Function GetO(Index: Integer):TObject;
        Procedure PutO(Index: Integer; const S: TObject);
     protected
        Objetos: array of TObject;
     public
        Procedure Add(O: TObject);
        Procedure Clear;
        Function Count:integer;
        Property Items[Index: Integer]: TObject read GetO write PutO; default;

  end;

  TSwSQLItem = class(TPersistent)
        constructor Create;
      private
        FText: AnsiString;
      protected
      public
        Valor:TSWItem;
        Function AsString: AnsiString;
        Function AsBoolean:Boolean;
        Function AsShortInt:Shortint;
        Function AsInteger:Integer;
        Function AsLong: int64;
        Function AsFloat:double;
      published
  end;

  PItemPost = ^TItemPost;
  TItemPost = record
    Nome: AnsiString;
    Valor: AnsiString;
  end;

  TSwEncryption = (SE_NONE, SE_MD5, SE_ENCRYPT);

   TPost = class(TPersistent)
   Constructor Create(UseEncryption: boolean = true; httpc: TIdHTTP = nil);
   private
     fPosts: TList; //array of TItemPost;
     fHost: AnsiString;
     fPort:cardinal;
     fDecrypt: boolean;
     fIndex: integer;
     Function GetCount:integer;
     Function GetItem(index: integer): TItemPost;
     Procedure SetItem(index: integer; const value: TItemPost );
     Function GetCurrentItem: TItemPost;
     Procedure SetCurrentItem( value: TItemPost );
   public
     Controler: TIdhttp;
     Compress: boolean;
     Property Count:integer read GetCount;
     Property Item[index: integer]: TItemPost read GetItem write SetItem;
     Property Parameter: TItemPost read GetCurrentItem write SetCurrentItem;
     Property Host: AnsiString read fHost write fHost;
     Property Port:cardinal read fPort write fPort;
     Procedure Add(Nome: AnsiString; Valor:AnsiString; FieldNameEncryption: TSwEncryption = SE_MD5; ValueEncryption:  TSwEncryption = SE_NONE);
     Function Execute: AnsiString;
     function ExecuteStream: TMemoryStream;
     Function Eof: boolean;
     Function Next: boolean;
     Function Prev: boolean;
     Procedure Clear;
   end;

  PSwRow = ^TSWRow;
  TSWRow = class(TObject)
      constructor Create;
    private
      FFields: array of TSWItem;
      Function Get(Index: Integer):TSWItem;
      Procedure Put(Index: Integer; const S: TSWItem);
      Function FToString: AnsiString;
    protected
      Procedure AddField(S: TSWItem);
    public
      Function FieldCount:integer;
      Property Fields[Index: Integer]: TSWItem read Get write Put; default;
      Property ToString: AnsiString read FToString;
    end;

  TSWRows = class
      constructor Create;
    private
      FRows: array of PSWRow;
      //procedure Add(s: TObject);
      Function GetRow(Index: Integer):TSWRow;
      Procedure PutRow(Index: Integer; const S: TSWRow);
    //protected
      //Procedure AddStringAsRow(s: string);
    public
      Procedure Clear;
      Procedure Add(s: TSWRow);
      Procedure AddString(s: AnsiString);
      Function RowCount:integer;
      Property Row[Index: Integer]: TSWRow read GetRow write PutRow;
    end;

  TSwList = class(TPersistent)
     Constructor Create;
     private
       FListText: TStrings;
       FSwRows: TSWRows;
    //procedure ADD(valor: string);//TStrings;
       Function FGetCount:integer;
       Function GetText:AnsiString;
       Function Get(Index: Integer):AnsiString;
       Procedure Put(Index: Integer; const S: AnsiString);
       Procedure SetText(value: AnsiString);
       //Procedure FPut(valor: string);
     public
       Property Count:integer read FGetCount;
       Procedure Clear;
     protected
       Procedure ADD(const valor: AnsiString); virtual;
       Property Rows: TSWRows read FSwRows write FSwRows;
       Property Strings[Index: Integer]: AnsiString read Get write Put; default;
     published

       Property Text:AnsiString read GetText write SetText;
  end;

  TCustomSwList = class(TSwList)
     constructor Create;
  published
     Property Rows;
  end;

  TCustomSwSQLList = class(TSwList)
  published
     Procedure ADD(const valor: AnsiString); override;
  public
     Property Strings;
  end;

  TQueryMode = (SW_SQL, SW_HOSTACTIVE, SW_OFFSET, SW_CUSTOM );
  TSWExecuteEvent = procedure of object;

  TSwSQL = class(TCustomSwList)
      constructor Create;
      private
        FHost: AnsiString;
        FPort: Cardinal;
        FList: TStringList;
        FExecuting: TSWExecuteEvent;
        FStopExecuting: TSWExecuteEvent;
        FPassword: AnsiString;
        FLoginID: integer;
        FProgress: Pointer;
        Function FLine:AnsiString;
        Function FNext:boolean;
        Function FPrev:boolean;
        Function FEOF:boolean;
        Function FEmpty:boolean;
        Function FFirst:AnsiString;
        Property Line:AnsiString read FLine;
        Function GetRow: TSWRow;
        Procedure SetRow(valor: TSWRow);
        
      public
        Controler: TIdhttp;
        Index:integer;
        Parameters: TPost;
        Property Row:TSWRow read GetRow write SetRow;
        Property First:AnsiString read FFirst;
        Property Password:AnsiString read FPassword write FPassword;
        Property Login:integer read FLoginID write FLoginID;
        Property Next:boolean read FNext;
        Property Prev:boolean read FPrev;
        Property EOF:boolean read FEOF;
        Property Empty:boolean read FEmpty;
        Function FieldByName(Field: AnsiString; SearchAllLines: boolean = false): TSwSQLItem;
        Function Execute: boolean;
        Procedure Reset;
        property OnExecute: TSWExecuteEvent read FExecuting write FExecuting;
        property OnStopExecuting: TSWExecuteEvent read FStopExecuting write FStopExecuting;
      published
        Property Host: AnsiString read FHost write FHost;
        Property Port: cardinal read FPort write FPort;
  end;

  function UrlEncode(const DecodedStr: AnsiString; Pluses: Boolean = false): AnsiString;
  function UrlDecode(const EncodedStr: AnsiString): AnsiString;

implementation

{ SQL }

function PadWithZeros(const str : AnsiString; size : integer) : AnsiString;
var
  origsize, i : integer;
begin
    try
        Result := str;
        origsize := Length(Result);
        if ((origsize mod size) <> 0) or (origsize = 0) then
        begin
            SetLength(Result,((origsize div size)+1)*size);
            for i := origsize+1 to Length(Result) do
                Result[i] := #0;
        end;
    except
        on e:Exception do
    end;
end;

constructor TSwSQL.Create;
begin
    FSep := md5('{[(_!.+.!_)]}');
    VSep := md5('{[(_=.+.=_)]}');
    Controler := TIdHTTP.Create(nil);
    Parameters := TPost.Create;
    inherited Create;
    self.Clear;
    Port:=80;
    Index:=0;
end;

function TSwSQL.FEmpty: boolean;
begin
    result:=(Self.Text='');
end;

function TSwSQL.FEOF: boolean;
begin
    if Index>=Self.Count then
          result:=true
    else
          result:=false;
end;

function TSwSQL.FFirst: AnsiString;
begin
    result := self.Strings[0];
end;

function TSwSQL.FieldByName(Field: AnsiString; SearchAllLines: boolean = false): TSwSQLItem;
var i,j:integer;  w:TSwSQLItem;
begin
    try
        w:=TSwSQLItem.Create;
        result:=w;
        if SearchAllLines then
        begin
            for i:= 0 to Rows.RowCount-1 do
            begin
                for j:=0 to Rows.Row[i].FieldCount-1 do
                begin
                  if LowerCase(Rows.Row[i].Fields[j].FieldName) = LowerCase(Field) then
                  begin
                     w.Valor:= Rows.Row[i].Fields[j];
                     result:=w;
                     exit;
                  end;
                end;
            end;
        end else
        begin
            for j:=0 to Row.FieldCount-1 do
            begin
              if LowerCase(Row.Fields[j].FieldName) = LowerCase(Field) then
              begin
                 w.Valor:= Row.Fields[j];
                 result:=w;
                 exit;
              end;
            end;
        end;
    except
        on e:Exception do
    end;
end;

function HexToInt(s: AnsiString): Longword;
var
  b: Byte;
  c: AnsiChar;
begin
    try
        Result := 0;
        s := UpperCase(s);
        for b := 1 to Length(s) do
        begin
            Result := Result * 16;
            c := s[b];
            case c of
                '0'..'9': Inc(Result, Ord(c) - Ord('0'));
                'A'..'F': Inc(Result, Ord(c) - Ord('A') + 10);
            else
                raise EConvertError.Create('No Hex-Number');
            end;
        end;
    except
        on e:Exception do
    end;
end;

function UrlEncode(const DecodedStr: AnsiString; Pluses: Boolean = false): AnsiString;
var
  I: Integer;
begin
    try
        Result := '';
        if Length(DecodedStr) > 0 then
            for I := 1 to Length(DecodedStr) do
            begin
                if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
                    Result := Result + '%' + IntToHex(Ord(DecodedStr[I]), 2)
                else
                if not (DecodedStr[I] = ' ') then
                    Result := Result + DecodedStr[I]
                else
                begin
                    if not Pluses then
                        Result := Result + '%20'
                    else
                        Result := Result + '+';
                end;
            end;
    except
        on e:Exception do
    end;
end;

function UrlDecode(const EncodedStr: AnsiString): AnsiString;
var
  I: Integer;
begin
    try
        Result := '';
        if Length(EncodedStr) > 0 then
        begin
            I := 1;
            while I <= Length(EncodedStr) do
            begin
                if EncodedStr[I] = '%' then
                begin
                    Result := Result + AnsiChar(HexToInt(EncodedStr[I+1] + EncodedStr[I+2]));
                    I := Succ(Succ(I));
                end else
                if EncodedStr[I] = '+' then
                    Result := Result + ' '
                else
                    Result := Result + EncodedStr[I];

                I := Succ(I);
            end;
        end;
    except
        on e:Exception do
    end;
end;

function TPost.Execute: AnsiString;
var
  data: TIdMultiPartFormDataStream;
  inStream, outStream: TMemoryStream;
  //IdHTTP1: TIdHTTP;
  lines:TStrings;
  i:integer;
  s:AnsiString;
begin

    //try
        result := '';

        Lines := TStringList.Create;
        //IdHTTP1 := TIdHTTP.Create(nil);
        Controler.URL.Host:=Host;

        if Compress then
            Controler.Request.AcceptEncoding := 'gzip';

        inStream := TMemoryStream.Create;
        data := TIdMultiPartFormDataStream.Create;
        try
            For i := 0 to Self.Count-1 do
            begin
                Application.ProcessMessages;
                Lines.Add(UrlEncode(Self.Item[i].Nome)+'='+UrlEncode(Self.Item[i].Valor));
                //data.AddFormField( UrlEncode(Self.Item[i].Nome) , UrlEncode(Self.Item[i].Valor) );
            end;

            Application.ProcessMessages;

            //try
                Controler.Post(Host, Lines, inStream);
            //except
            //on e:Exception do
            //    MessageBox(0, PChar(e.Message) , 'erro', 0);
            //end;

            outStream := TMemoryStream.Create;
            if Compress then
            begin
                try
                    if(inStream.size > 0) then
                    begin
                    gZipDecompress(inStream, outStream);
                    outStream.Position := 0;
                    //if(outStream<>nil)then
                    lines.LoadFromStream(outStream);
                    end;
                finally
                    outStream.Free;
                end;
            end else
            begin
                inStream.Position := 0;
                lines.LoadFromStream(inStream);
            end;

            result := lines.Text;

            //result := IdHTTP1.Post(Host, data);
        finally
            data.Free;
            //IdHTTP1.Free;
            lines.Free;
            inStream.Free;
        end;
    // except
      // on e:exception do
     //end;


end;


function TPost.ExecuteStream: TMemoryStream;
var
  data: TIdMultiPartFormDataStream;
  inStream, outStream: TMemoryStream;
  //IdHTTP1: TIdHTTP;
  i:integer;
  s:AnsiString;
begin

     try
        result := nil;

        //IdHTTP1 := TIdHTTP.Create(nil);
        Controler.URL.Host:=Host;

        if Compress then
          Controler.Request.AcceptEncoding := 'gzip';

        inStream := TMemoryStream.Create;
        data := TIdMultiPartFormDataStream.Create;
        try
          For i := 0 to Self.Count-1 do
          begin
              Application.ProcessMessages;
              data.AddFormField( UrlEncode(Self.Item[i].Nome) , UrlEncode(Self.Item[i].Valor) );
          end;

          Application.ProcessMessages;

          try
            Controler.Post(Host, data, inStream);
          except
            on e:Exception do
              //MessageBox(0, PChar(e.ToString) , 'erro', 0);
          end;

          outStream := TMemoryStream.Create;
          if Compress then
          begin
            try
              if(inStream.size > 0) then
              begin
                  gZipDecompress(inStream, outStream);
                  outStream.Position := 0;
                  //if(outStream<>nil)then
                  result := outStream;
                  if outStream.Size < 50 then
                     result := nil;
              end;
            finally
              outStream.Free;
            end;
          end else
          begin
            inStream.Position := 0;
            result := inStream;
            if inStream.Size < 50 then
               result := nil;
          end;

          //result := IdHTTP1.Post(Host, data);
        finally
          data.Free;
          if Compress then
            inStream.Free
          else
            outStream.Free;
          //IdHTTP1.Free;
          //inStream.Free;
        end;
     except
       on e:exception do
     end;


end;

function TSwSQL.Execute: boolean;
var
  data: TIdMultiPartFormDataStream;
  inStream, outStream: TMemoryStream;
  //IdHTTP1: TIdHTTP;
  lines:TStrings;
  i:integer;
  s:AnsiString;
begin


    try
      result := false;
      if Assigned(FExecuting) then
        FExecuting;
        Self.Clear;
        Rows.Clear;
        Lines:=TStringList.Create;
        //IdHTTP1 := TIdHTTP.Create(nil);
        Controler.URL.Host:=Host;
        Controler.Request.AcceptEncoding := 'gzip';

        inStream := TMemoryStream.Create;
        data := TIdMultiPartFormDataStream.Create;
        try
          For i := 0 to Parameters.Count-1 do
          begin
              Application.ProcessMessages;
              data.AddFormField( Parameters.Item[i].Nome , Parameters.Item[i].Valor );
          end;

          Application.ProcessMessages;
          Controler.Post(Host, data, inStream);
      
          outStream := TMemoryStream.Create;
          try
            gZipDecompress(inStream, outStream);
            outStream.Position := 0;
            //if(outStream<>nil)then
               lines.LoadFromStream(outStream);
          finally
            outStream.Free;
          end;

          for i:=0 to lines.Count-1 do
          begin
             Application.ProcessMessages; 
             if( length(lines.Strings[i]) > 10 ) then
             if MaxDecript(lines.Strings[i])<>'' then
             begin
               result := true;
               Self.add(MaxDecript(lines.Strings[i]));
               rows.AddString(MaxDecript(lines.Strings[i]));
             end;
          end;
        finally
          data.Free;
          //IdHTTP1.Free;
          lines.Free;
          inStream.Free;
        end;
      if Assigned(FStopExecuting) then
        FStopExecuting;
    except
       on e:exception do
    end;

end;

function TSwSQL.FLine: AnsiString;
begin
result:=Self.Strings[index];
end;

function TSwSQL.FNext: boolean;
begin
if index+1 > Self.Count-1 then
  result:=false
else
  result:=true;
inc(index);
end;

function TSwSQL.FPrev: boolean;
begin
if index-1 < 0 then
  result:=false
else
begin
    dec(index);
    result:=true;
end;
end;

function TSwSQL.GetRow: TSWRow;
begin
result:=rows.Row[index];
end;

procedure TSwSQL.Reset;
begin
   index:=0;
end;

procedure TSwSQL.SetRow(valor: TSWRow);
begin
 rows.Row[index]:=valor;
end;

{ TSwSQLItem }

function TSwSQLItem.AsBoolean: Boolean;
begin
if trim(valor.Value)='' then
begin
  result:=false;
  exit;
end;
  try
    result:=StrToBool(trim(valor.Value));
  except
    on e:exception do
  end;
end;

function TSwSQLItem.AsFloat: double;
begin
if trim(valor.Value)='' then
begin
  result:=0;
  exit;
end;
  try
    result:=StrToFloat(trim(valor.Value));
  except
    on e:exception do
  end;
end;

function TSwSQLItem.AsInteger: Integer;
begin
if trim(valor.Value)='' then
begin
  result:=0;
  exit;
end;
  try
    result:=StrToInt( trim(valor.Value) );
  except
    on e:exception do
  end;
end;

function TSwSQLItem.AsLong: int64;
begin
  try
    result:=int64(StrToInt64(trim(valor.Value)));
  except
    on e:exception do
  end;
end;

function TSwSQLItem.AsShortInt: Shortint;
begin
  try
    result:=Shortint(StrToInt(trim(valor.Value)));
  except
    on e:exception do
  end;
end;

function TSwSQLItem.AsString: AnsiString;
begin
if trim(valor.Value)='' then
begin
  result:='';
  exit;
end;
  try
    result:=trim(valor.Value);
  except
    on e:exception do
  end;
end;

constructor TSwSQLItem.Create;
begin
//
end;

{ TSWSQLList }

{ TSwList }

procedure TSwList.ADD(const valor: AnsiString);
begin
  FListText.Add(valor);
end;

procedure TSwList.Clear;
begin
  FListText.Clear
end;

constructor TSwList.Create;
begin
    FListText := TStringList.Create;
    FSwRows:=TSWRows.Create;
end;

function TSwList.FGetCount: integer;
begin
  Result:=FListText.Count;
end;

function TSwList.Get(Index: Integer): AnsiString;
begin
  result:= FListText.Strings[index];
end;

function TSwList.GetText: AnsiString;
var i:integer;
begin
  result:=FListText.Text;          
end;

procedure TSwList.Put(Index: Integer; const S: AnsiString);
begin
   FListText[index]:=s;
end;

procedure TSwList.SetText(value: AnsiString);
var i:integer; s:AnsiString;
begin
    FListText.Text:=value;
end;

{ TSWRow }

procedure TSWRow.AddField(S: TSWItem);
var p:pointer;
begin
    //p:=GetMemory(sizeof(s));
    //CopyMemory(p,@s,sizeof(s));
    setlength(FFields, length(FFields)+1);
    FFields[high(FFields)]:=TSWItem(s);//p^);
end;

constructor TSWRow.Create;
begin
    //
end;

function TSWRow.FieldCount: integer;
begin
    result:=length(FFields);
end;

function TSWRow.FToString: AnsiString;
var i:integer;
r:TSWItem;
begin
    i:=0;
    //r:=TSWItem.Create;
    result:='';
    for i:=0 to length(ffields) -1 do
    begin
       r:=ffields[i];
       if i<>0 then
         result:=result+' / '+ r.FieldName+': '+r.Value
       else
         result:=result+r.FieldName+': '+r.Value;
    end;
end;

function TSWRow.Get(Index: Integer): TSWItem;
begin
    result:= FFields[index];
end;

procedure TSWRow.Put(Index: Integer; const S: TSWItem);
begin
    FFields[index]:=s;
end;

{ TSWRows }

function GetFields(Line: AnsiString): TSWRow;
var i,j,k:integer; s,d:AnsiString;
    f,v:AnsiString; //field e value
    r:TSWItem;
    res: TSWRow;
    p:AnsiString;
    pr:^TSWItem;
begin

    try
       p:='';
       res:=TSWRow.Create;
       s:=Line;
       k:=length(fsep);
       while pos(fSep,s)>0 do
       begin
           j:=pos(fSep,s); //posiçao do marcador de 'field'
           s:=Copy(s,j+k,length(s)-(j+k)+1); //copia o restante depois do marcador de 'field'
           if pos(vsep,s) > 0 then //se existir um marcador de 'value'
           begin
              f:=copy(s,1,pos(vsep,s)-1);
              v:=copy(s,pos(vsep,s)+k ,pos(fsep,s)-(pos(vsep,s)+k));
              r.FieldName:=f;
              r.Value:=v;
              res.AddField(r);
              //r.Free;
              p:=res.ToString;
           end;
       end;
       result:=res;
    except
        on e:Exception do
    end;
end;

procedure TSWRows.Add(s: TSWRow);
var P:PSwRow;
begin
    setlength(FRows,length(FRows)+1);
    p:=new(PSwRow);
    move(s,p^,sizeof(s));
    //p^:=s;
    FRows[high(FRows)]:= p;
    //FRows[high(FRows)]^ := (s)
end;

procedure TSWRows.AddString(s: AnsiString);
var r:TSWRow; d:AnsiString;
begin
    d:='';
    r:=TSWRow.Create;
    r:=GetFields(s);
    Add(r);
    d:=d+inttostr(self.RowCount);
end;

procedure TSWRows.Clear;
begin
    setlength(frows,0);
end;

constructor TSWRows.Create;
begin
//
end;

function TSWRows.GetRow(Index: Integer): TSWRow;
begin
    result:=FRows[index]^;
end;

procedure TSWRows.PutRow(Index: Integer; const S: TSWRow);
begin
    FRows[index]^:=s;
end;

function TSWRows.RowCount: integer;
begin
    result:=length(fRows);
end;

{ TCustomSwSQLList }

procedure TCustomSwSQLList.ADD(const valor: AnsiString);
begin
    inherited;
end;

{ TCustomSwList }

constructor TCustomSwList.Create;
begin
    inherited Create;
end;

{ TSwObjectList }

procedure TSwObjectList.Add(O: TObject);
begin
    setlength(Objetos,length(Objetos)+1);
    objetos[high(Objetos)]:=o;
end;

procedure TSwObjectList.Clear;
begin
     setlength(Objetos,0);
end;

function TSwObjectList.Count: integer;
begin
    result:=length(Objetos);
end;

constructor TSwObjectList.Create;
begin
   //
end;

function TSwObjectList.GetO(Index: Integer): TObject;
begin
   result:=Objetos[index];
end;

procedure TSwObjectList.PutO(Index: Integer; const S: TObject);
begin
   Objetos[index]:=s;
end;

{ TPost }

procedure TPost.Add(Nome: AnsiString; Valor:AnsiString; FieldNameEncryption: TSwEncryption = SE_MD5; ValueEncryption:  TSwEncryption = SE_NONE);
var P: PItemPost;
begin
    P := new(PItemPost);

    case integer(FieldNameEncryption) of
         integer(SE_NONE):
              P^.Nome := Nome;
         integer(SE_MD5):
              P^.Nome := MD5(Nome);
         integer(SE_ENCRYPT):
              P^.Nome := MaxEncript(Nome);
    end;

    case integer(ValueEncryption) of
         integer(SE_NONE):
              P^.Valor := Valor;
         integer(SE_MD5):
              P^.Valor := MD5(Valor);
         integer(SE_ENCRYPT):
              P^.Valor := MaxEncript(Valor);
    end;

    fPosts.Add(P);
end;

procedure TPost.Clear;
begin
    fPosts.Clear;
end;

constructor TPost.Create(UseEncryption: boolean = true; httpc: TIdHTTP = nil);
begin
    Compress := true;
    if httpc = nil then
        Controler := TIdHTTP.Create(nil)
    else
        Controler := httpc;
    fPosts := TList.Create;
    fDecrypt := UseEncryption;
end;

function TPost.Eof: boolean;
begin
  result := (fIndex >= fPosts.Count) or (fIndex < 0);
end;

function TPost.GetCount: integer;
begin
  result := fPosts.Count;
end;

function TPost.GetCurrentItem: TItemPost;
begin
  result := Item[findex];
end;

function TPost.GetItem(index: integer): TItemPost;
begin
  result := TItemPost(fPosts[index]^);
end;

function TPost.Next: boolean;
begin
  inc(fIndex);
  result := Eof;
end;

function TPost.Prev: boolean;
begin
  dec(fIndex);
  result := Eof;
end;

procedure TPost.SetCurrentItem(value: TItemPost);
var P: PItemPost;
begin
    P := New(PItemPost);
    P^ := value;
    fPosts[fIndex] := P;

end;

procedure TPost.SetItem(index: integer; const value: TItemPost);
var P: PItemPost;
begin
    P := new(PItemPost);
    P^ := value;
    fPosts[index] := p; 
end;

end.
