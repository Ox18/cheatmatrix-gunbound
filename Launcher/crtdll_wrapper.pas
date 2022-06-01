unit crtdll_wrapper;
//  This file is a part of DiskID for Delphi
//  Original code of DiskID can be taken here:
//  http://www.winsim.com/diskid32/diskid32.html
//  The code was ported from C++ to Delphi by Victor Derevyanko, dvpublic0@gmail.com
//  If you find any error please send me bugreport by email. Thanks in advance.
//  The translation was donated by efaktum (http://www.efaktum.dk).

interface

function isspace(ch: AnsiChar): Boolean;
function isalpha(ch: AnsiChar): Boolean;
function tolower(ch: AnsiChar): AnsiChar;
function isprint(ch: AnsiChar): Boolean;
function isalnum(ch: AnsiChar): Boolean;


implementation

function crt_isspace(ch: Integer): Integer; cdecl; external 'crtdll.dll' name 'isspace';
function crt_isalpha(ch: Integer): Integer; cdecl; external 'crtdll.dll' name 'isalpha';
function crt_tolower(ch: Integer): Integer; cdecl; external 'crtdll.dll' name 'tolower';
function crt_isprint(ch: Integer): Integer; cdecl; external 'crtdll.dll' name 'isprint';
function crt_isalnum(ch: Integer): Integer; cdecl; external 'crtdll.dll' name 'isalnum';

function isspace(ch: AnsiChar): Boolean;
begin
  Result := crt_isspace(Ord(ch)) <> 0;
end;

function isalpha(ch: AnsiChar): Boolean;
begin
  Result := crt_isalpha(Ord(ch)) <> 0;
end;

function tolower(ch: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(crt_tolower(Ord(ch)));
end;

function isprint(ch: AnsiChar): Boolean;
begin
  Result := crt_isprint(Ord(ch)) <> 0;
end;

function isalnum(ch: AnsiChar): Boolean;
begin
  Result := crt_isalnum(Ord(ch)) <> 0;
end;
end.
