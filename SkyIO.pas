unit SkyIO;

interface

uses windows, classes, sysutils, forms;

type
  PFound = ^TFound;
  TFound = Record
    FileName: AnsiString;
    FileDir: AnsiString;
    FileExt: AnsiString;
    Path: AnsiString;
  end;

  TExtension = class
      constructor Create;
      private
        fList: TStrings;
        fIndex: integer;
        Function GetExtension(Index: integer): AnsiString;
        Procedure SetExtension(Index: integer; Extension: AnsiString);
      public
        Procedure Add(Ext: AnsiString);
        Function CurrentExt: AnsiString;
        Property Extension[index: integer]: AnsiString read GetExtension write SetExtension;
        Procedure Reset;
        Function Next: boolean;
        Function Prev: boolean;
        Function Count: integer;
        Procedure Clear;
        Function Eof: boolean;
  end;

  TSearchResult = class
     constructor Create;
     private
        fResults: TList;
        fIndex: integer;
        Function GetFound(Index: integer): TFound;
        Procedure SetFound(Index: integer; Found: TFound);
     public
        Property Index_:integer read fIndex;
        Procedure Add(Path: AnsiString);
        Function CurrentResult: TFound;
        Property ResultValue[index: integer]: TFound read GetFound write SetFound;
        Procedure Reset;
        Function Next: boolean;
        Function Prev: boolean;
        Function Count: integer;
        Procedure Clear;
        Function Eof: boolean;
     end;

  TSearcher = Class
    constructor Create;
    private
      fDir: AnsiString;
      fExt: TExtension;
      fRes: TSearchResult;
      fStop: boolean;
      fSub: boolean;
      procedure Search(Dir, Ext: AnsiString);
    public
    Property Dir: AnsiString read fDir write fDir;
    Property SearchOnSubDirs: boolean read fSub write fSub;
    Property Results: TSearchResult read fRes write fRes;
    Property Extensions: TExtension read fExt write fExt;
    Procedure StartSearch;
    Procedure StopSearch;
  end;

implementation

{ TExtension }

procedure TExtension.Add(Ext: AnsiString);
begin
  FList.Add(ext);
end;

procedure TExtension.Clear;
begin
  flist.Clear;
end;

function TExtension.Count: integer;
begin
  result := flist.Count;
end;

constructor TExtension.Create;
begin
  fList := TStringList.Create;
  fIndex := 0;
end;

function TExtension.CurrentExt: AnsiString;
begin
result := Extension[findex];
end;

function TExtension.Eof: boolean;
begin
  result := (fIndex >= fList.Count) or (fIndex<0);
end;

function TExtension.GetExtension(Index: integer): AnsiString;
begin
  result := flist[index];
end;

function TExtension.Next: boolean;
begin
  inc(fIndex);
  result := Eof;
end;

function TExtension.Prev: boolean;
begin
  dec(fIndex);
  result := Eof;
end;

procedure TExtension.Reset;
begin
  fIndex := 0;
end;

procedure TExtension.SetExtension(Index: integer; Extension: AnsiString);
begin
  flist[index] := Extension;
end;

{ TSearchResult }

procedure TSearchResult.Add(Path: AnsiString);
var P: PFound;
    S: AnsiString;
begin
  P := new(PFound);
  S := ExtractFileName(Path);
  P^.FileName := copy( S, 1, Length(S)-Length(ExtractFileExt(Path)));
  P^.FileExt := ExtractFileExt(Path);
  P^.Path := Path;
  P^.FileDir := ExtractFileDir(Path);
  fResults.Add(P);
end;

procedure TSearchResult.Clear;
begin
  fResults.Clear;
end;

function TSearchResult.Count: integer;
begin
  result := fResults.Count;
end;

constructor TSearchResult.Create;
begin
  fResults := TList.Create;
  fIndex := 0;
end;

function TSearchResult.CurrentResult: TFound;
begin
  result := ResultValue[findex];
end;

function TSearchResult.Eof: boolean;
begin
  result := (fIndex >= fResults.Count) or (fIndex < 0);
end;

function TSearchResult.GetFound(Index: integer): TFound;
begin
  result := TFound(fResults.Items[index]^);
end;

function TSearchResult.Next: boolean;
begin
  inc(fIndex);
  Result := Eof;
end;

function TSearchResult.Prev: boolean;
begin
  dec(fIndex);
  Result := Eof;
end;

procedure TSearchResult.Reset;
begin
  fIndex := 0;
end;

procedure TSearchResult.SetFound(Index: integer; Found: TFound);
var P:PFound;
begin
   P := new(PFound);
   P^ := Found;
   fResults.Items[index] := P;
end;

{ TSearcher }

constructor TSearcher.Create;
begin
  fExt := TExtension.Create;
  fRes := TSearchResult.Create;
  fStop := false;
  fSub := true;
end;

procedure TSearcher.Search(Dir, Ext: AnsiString);
var
  Diretorio,Nome : AnsiString;
  SearchRec : TSearchRec;
  ListaDirs : TStringList;
  Done : Integer;
  i : Integer;
begin
  Diretorio := Dir;
  Nome := Ext;
  Application.ProcessMessages;
  Done := FindFirst(Dir+ext,0,SearchRec);
  while Done = 0 do begin
    if fStop then
       exit;
    Application.ProcessMessages;
    Results.Add(Diretorio+SearchRec.Name);
    Done := FindNext(SearchRec);
  end;
  FindClose(SearchRec);

  ListaDirs := TStringList.Create;
  
  if fSub then
  try
    Done := FindFirst(Diretorio+'*.*', faDirectory,SearchRec);
    while Done = 0 do begin
      if fStop then
         exit;
      Application.ProcessMessages;
      if ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          ListaDirs.Add(Diretorio+SearchRec.Name+'\');
      Done := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
// pesquisa em todos diretorios encontrados    
    for i := 0 to ListaDirs.Count-1 do
    begin
      if fStop then
         exit;
      Application.ProcessMessages;
      Search(ListaDirs[i], Ext);
    end;
  finally
    ListaDirs.Free;
  end;
end;

procedure TSearcher.StartSearch;
begin
  fStop := false;
  
  if Dir[length(dir)] <> '\' then
     Dir := Dir + '\';

  Extensions.Reset;
  while not Extensions.Eof do
  begin
      Search(dir,Extensions.CurrentExt);
      Extensions.Next;
  end;
end;

procedure TSearcher.StopSearch;
begin
   fStop := true;
end;

end.
