{ KOL MCK } // Do not remove this line!
program reload;

uses
  Unit1 in 'Unit1.pas' {Form1},
  forms;

{$E dll}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


