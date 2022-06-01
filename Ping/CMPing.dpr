program CMPing;

uses
  Forms,
  Unit9 in 'Unit9.pas' {Form9},
  Ping in 'Ping.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
