unit TEA;

interface

uses Windows, SysUtils, functions;

Function EncriptTea(valor: AnsiString; key: AnsiString): AnsiString;
Function DecriptTea(valor: AnsiString; key: AnsiString): AnsiString;

  const
  Delta: longword = $9e3779b9;
  Lim32 = 2147483648; // MaxInt32 +1

type
  TByte4 = array[0..3] of byte;         //   32-bit =   4-byte
  TLong2 = array[0.. 1] of Longword;    //   64-bit =   8-byte
  TByte8 = array[0..7] of byte;         //   64-bit =   8-byte
  TTeaKey = array[0..3] of Longword;    //  128-bit =  16-byte
  TLong2x2 = array[0..1] of TLong2;     //  128-bit =  16-byte
  TByte16 = array[0..15] of byte;       //  128-bit =  16-byte
  TTeaData = array of Longword;         // n*32-bit = n*4-byte
  TLong2Data = array of TLong2;         // n*64-bit = n*8-byte

  var
    n: integer;
    //data: TTeaData;
    data2: TLong2Data;
    MasterKey: TTeaKey;
    fFileSize: Int64;
    fFileName: string;

implementation

type
PKey = ^TKey;
TKey = record
  k1: DWORD;
  k2: DWORD;
  k3: DWORD;
  k4: DWORD;
end;

PValor = ^TValor;
TValor = record
  v1: DWORD;
  v2: DWORD;
end;


Function ConvertStringToUInt(Input: AnsiString): DWORD;
begin
    try
        result := 0;
        result :=  result + (DWORD(Input[1]));
        result :=  result + (DWORD(Input[2]) shl 8);
        result :=  result + (DWORD(Input[3]) shl 16);
        result :=  result + (DWORD(Input[4]) shl 24);
    except
        on e:Exception do
    end;
end;

Function ConvertUIntToString(Input:DWORD):AnsiString;
begin
    try
        result := '';
        result :=  result + AnsiChar(Input and $FF);
        result :=  result + AnsiChar((Input shr 8) and $FF);
        result :=  result + AnsiChar((Input shr 16) and $FF);
        result :=  result + AnsiChar((Input shr 24) and $FF);
    except
        on e:Exception do
    end;
end;


Function FormatKey(valor: string): TKey;
begin
    result.k1 := ConvertStringToUInt(copy(valor, 1, 4) );
    result.k2 := ConvertStringToUInt(copy(valor, 5, 4) );
    result.k3 := ConvertStringToUInt(copy(valor, 9, 4) );
    result.k4 := ConvertStringToUInt(copy(valor, 13, 4) );
end;

Function PerformMath(membro: dword; sum: dword; key1: dword; key2: dword): integer;
var s1,s2,s3,s4: integer;
begin
    try
        s1 := (membro shl 4);
        s1 := (s1 + key1);
        s2 := (membro + sum);
        s3 := (membro shr 5);
        s3 := (s3 + key2);
        s4 := (s1 xor s2);
        s4 := (s4 xor s3);
        result := s4;
    except
        on e:Exception do
    end;
end;

procedure TeaEncrypt(var data: TLong2; const key: TTeaKey);
var
  y,z,sum: LongWord;
  a: byte;
  i: integer;
begin
    try
        y:=data[0];
        z:=data[1];
        sum := 0;

        for a := 0 to 31 do
        begin
            inc(sum, Delta);
            inc(y, ((z shl 4)+key[0]) xor (z+sum) xor ((z shr 5)+key[1]));
            inc(z, ((y shl 4)+key[2]) xor (y+sum) xor ((y shr 5)+key[3]));
        end;

        data[0] := y;
        data[1] := z
    except
        on e:Exception do
    end;
end;

procedure TeaDecrypt(var data: TLong2; const key: TTeaKey);
var
  y,z,sum: Longword;
  a: byte;
  i: integer;
begin
    try
        y := data[0];
        z := data[1];
        sum := delta shl 5;
        for a := 0 to 31 do
        begin
            dec(z, ((y shl 4)+key[2]) xor (y+sum) xor ((y shr 5)+key[3]));
            dec(y, ((z shl 4)+key[0]) xor (z+sum) xor ((z shr 5)+key[1]));
            dec(sum, Delta);
        end;
        data[0] := y;
        data[1] := z
    except
        on e:Exception do
    end;
end;

Function DataToStr(const valor: TTeaData): AnsiString;
var
  sa: AnsiString;
  i, n, m: integer;
  b: byte;
begin
    try
        n := Length(valor);
        SetLength(sa,n*4);
        for i := 0 to n-1 do
        for m := 0 to 3 do
        begin
            b := TByte4(valor[i])[m];
            sa[i*4+m+1] := ansiChar(b);
        end;
        result := (sa);
        sa := '';
    except
        on e:Exception do
    end;
end;

procedure StrToData(const s: AnsiString; var valor: TTeaData);
var
  sa: AnsiString;
  i, n, m: integer;
begin
    try
        sa := AnsiString(s);
        n := Length(sa) div 4;
        m := Length(sa) mod 4;
        if m <> 0 then
        begin
            inc(n);
            sa := sa + StringOfChar(' ',m);
        end;
        if n < 2 then  // n = 1
        begin
            n := 2;
            sa := sa + StringOfChar(' ',4);
        end;

        SetLength(valor,n);
        for i := 0 to n-1 do
            for m := 0 to 3 do
                TByte4(valor[i])[m] := ord(sa[i*4+m+1]);
        sa := '';
    except
        on e:Exception do
    end;
end;

procedure StrToKey(const s: AnsiString; var key: TTeaKey);
var
  sa, sb: AnsiString;
  i, n: integer;
begin
    try
        sa := AnsiString(s);
        sb := StringOfChar(' ',16);
        if Length(sa) < 16 then n := Length(sa) else n := 16;
        //n := min(Length(sa),16);
        for i := 1 to n do
            sb[i] := sa[i];
        for i := 1 to 16 do
            TByte16(key)[i-1] := ord(sb[i]);
        sa := '';
        sb := '';
    except
        on e:Exception do
    end;
end;

function KeyToStr(const key: TTeaKey): AnsiString;
var
  s: AnsiString;
  i: integer;
begin
    try
        SetLength(s,16);
        for i := 1 to 16 do
        begin
            s[i] := AnsiChar(TByte16(key)[i-1]);
        end;
        result := s;
    except
        on e:Exception do
    end;
end;

Function EncriptTea(valor: AnsiString; key: AnsiString): AnsiString;
var
  i,j,k,l,n: integer;
  dat: TLong2;
  s: AnsiString;
  TmpData: TTeaData;
begin
    try
        key := copy(md5(md5(key+'cmx')),1, 16);
        StrToKey(key, MasterKey);

        result := '';
        while length(valor) > 0 do
        begin
            //Copia 8 caracteres da string
            SetLength(s, 8);
            s := Copy(valor, 1, 8);

            if (length(valor) >= 8) then
                valor := Copy(valor, 9, length(valor))
            else
                valor := '';

            SetLength(TmpData, 2);

            n := length(s);
            for i := 1 to (8 - n) do
                s := s + #0;

            StrToData(s , TmpData);

            dat[0] := TmpData[0];
            dat[1] := TmpData[1];
        
            TeaEncrypt(dat, MasterKey);

            TmpData[0] := dat[0];
            TmpData[1] := dat[1];

            result := result + DataToStr(TmpData);
        end;
    except
        on e:Exception do
    end;
end;

Function DecriptTea(valor: AnsiString; key: AnsiString): AnsiString;
var
  i,j,k,l: integer;
  dat: TLong2;
  s: AnsiString;
  TmpData: TTeaData;
begin
    try
        key := copy(md5(md5(key+'cmx')),1, 16);
        StrToKey(key, MasterKey);

        result := '';
        while length(valor) > 0 do
        begin
            SetLength(s, 8);
            s := Copy(valor, 1, 8);
            if (length(valor) >= 8) then
                valor := Copy(valor, 9, length(valor))
            else
                valor := '';
            SetLength(TmpData, 2);
     
            n := length(s);
            for i := 1 to (8 - n) do
                s := s + #0;

            StrToData(s , TmpData);

            dat[0] := TmpData[0];
            dat[1] := TmpData[1];

            TeaDecrypt(dat, MasterKey);

            TmpData[0] := dat[0];
            TmpData[1] := dat[1];

            result := result + DataToStr(TmpData);
        end;
    except
        on e:Exception do
    end;
end;

end.
