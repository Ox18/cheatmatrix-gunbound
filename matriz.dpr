program matriz;

uses
  Forms,
  windows,
  sysutils,
  SplashScreen in 'SplashScreen.pas',
  Utils in 'Utils.pas',
  CmPlugins in 'CmPlugins.pas',
  SkyFiles in 'SkyFiles.pas',
  SkySql in 'SkySql.pas',
  CMStatus in 'CMStatus.pas',
  Constantes in 'Constantes.pas',
  ConfigsLoader in 'ConfigsLoader.pas',
  Common in 'Common.pas',
  IOCTLs in 'IOCTLs.pas',
  CMClasses in 'CMClasses.pas',
  ProcListing in 'ProcListing.pas',
  CMHook in 'CMHook.pas',
  TEA in 'TEA.pas',
  Encription in 'Encription.pas',
  CMCript in 'CMCript.pas',
  langcontrol in 'langcontrol.pas',
  CMSysUtils in 'CMSysUtils.pas',
  UnitMatriz in 'UnitMatriz.pas' {frmMatriz},
  Inicializador in 'Inicializador.pas' {/VCLFixPack in 'fixes\VCLFixPack.pas',},
  functions in 'functions.pas';

{$R *.res}

var inicializado: integer = 0;
begin
Mutex := CreateMutex(nil, True, 'Cheat Matrix');
if (Mutex <> 0) and (GetLastError = 0) then
  begin
      Application.Initialize;
      Application.CreateForm(TSplash, Splash);
  antes := GetTickCount;
      Splash.Show;
      Splash.Refresh;
      {Splash := TSplash.Create(Application);
      antes := GetTickCount;
      Splash.Show;
      Splash.Refresh;}
      inicializado := Inicializar;
      if (inicializado mod 10) = 0 then
      begin
          Application.CreateForm(TfrmMatriz, frmMatriz);
          Application.Title := '';
          Application.Run;
      end else
      begin
          //Application.CreateForm(TfrmMatriz, frmMatriz);
          Application.Title := '';
          Application.Run;
      end;

      if Mutex <> 0 then
      CloseHandle(Mutex);
  end;
end.

