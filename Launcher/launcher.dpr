program launcher;

uses
  Forms,
  windows,
  unit1 in 'unit1.pas';// { Form5 } ,
  //SplashScreen in 'SplashScreen.pas' { Splash } ;
{$R *.res}

  begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
  end.
       {
var
  inicializado: integer = 0;
  Mutex: THandle;

begin
  Mutex := CreateMutex(nil, True, 'CMMtx');
  if (Mutex <> 0) and (GetLastError = 0) then
  begin
    Application.Initialize;
    Application.CreateForm(TSplash, Splash);
    Application.CreateForm(TSplash, Splash);
    Splash.Show;
    Splash.Refresh;
    // inicializado := Inicializar;
    if Mutex <> 0 then
      CloseHandle(Mutex);
  end;

end.  }
