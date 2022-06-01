unit Encription;

interface

uses Windows, TEA, CMCript, Base64, functions, sysutils;

Function MaxEncript(valor: AnsiString; hd: AnsiString = ''): AnsiString;
Function MaxDecript(valor: AnsiString; hd: AnsiString = ''): AnsiString;


implementation

{Function MD5(valor: string): string;
var Hash: TDCP_md5;
    Digest: array[0..15] of byte;
    i: integer;
    p: string;
    FinalValue: string;
begin
  FinalValue := '';
  try
     if valor <> '' then
     begin
        Hash:= TDCP_md5.Create(nil);        // create the hash
        Hash.Init;                           // initialize it
        Hash.UpdateStr(valor);                 // hash the stream contents
        Hash.Final(Digest);                  // produce the digest
        p:= '';
        for i:= 0 to 15 do p:= p + IntToHex(Digest[i],2);
     end;
     FinalValue := lowercase(p);
     Result := FinalValue;
  finally

  end;
end; }

Function MaxEncript(valor: AnsiString; hd: AnsiString = ''): AnsiString;
var s, key: AnsiString;
begin
//{$I VMProtectBegin.inc}
    result := '';

    if trim(valor) = '' then
       exit;

    //s := CM_Encrypt(valor);
    s := (valor);

    if hd = '' then
        //result := CM_Encrypt(s);
        //exit;
        hd := 'none'; 

    key := copy(md5(md5(hd+'cmx')),1, 16);

    s := EncriptTea(s, key);
    s := Base64Encode(s);
    result := s;
// {$I VMProtectEnd.inc}
end;

Function MaxDecript(valor: AnsiString; hd: AnsiString = ''): AnsiString;
var s, key: AnsiString;
begin
//{$I VMProtectBegin.inc}
    result := '';

    if trim(valor) = '' then
       exit;

    if hd = '' then
    begin
        //result := CM_Decrypt(valor);
        //exit;
        hd := 'none'; 
    end;

    key := copy(md5(md5(hd+'cmx')),1, 16);
    s := Base64Decode(trim(valor));
    s := Trim(DecriptTea(s, key));
    //s := CM_Decrypt(s);
    result := s;

// {$I VMProtectEnd.inc}
end;

end.
