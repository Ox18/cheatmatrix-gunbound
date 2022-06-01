unit functions;

interface

function MD5File(const fileName: AnsiString): AnsiString;
function MD5(const valor: AnsiString): AnsiString;
Function GetRandomName(Size: integer): AnsiString;

implementation

uses IdHashMessageDigest, idHash, classes, sysutils, windows, constantes;

Function GetRandomName(Size: integer): AnsiString;
var
	i, k, l: integer;
	v: boolean;
	Consoantes, Vogais: AnsiString;
begin
	Consoantes := constante(0104, false);
	Vogais := constante(0105, false);
	result := '';
	v := false;
	k := random(6) + 5;
	for i := 1 to Size do
	begin
		randomize;
		v := not v;
		l := random(3);
		if v then
			result := result + Consoantes[random(length(Consoantes) - 1) + 1]
		else
			result := result + Vogais[random(length(Vogais) - 1) + 1]
	end;
end;

function MD5(const valor: AnsiString): AnsiString;
var
	idmd5: TIdHashMessageDigest5;
	hash: T4x4LongWordRecord;
begin
	idmd5 := TIdHashMessageDigest5.Create;
	try
		result := AnsiLowerCase(idmd5.HashStringAsHex(valor));
	finally
		idmd5.Free;
	end;
end;

function MD5File(const fileName: AnsiString): AnsiString;
var
	idmd5: TIdHashMessageDigest5;
	fs: TFileStream;
	hash: T4x4LongWordRecord;
begin
	idmd5 := TIdHashMessageDigest5.Create;
	fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite);
	try
		result := AnsiLowerCase(idmd5.HashStreamAsHex(fs));
	finally
		fs.Free;
		idmd5.Free;
	end;
end;

end.
