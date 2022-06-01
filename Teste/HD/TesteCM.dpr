program TesteCM;

uses
  Forms,
  Unit16 in 'Unit16.pas' {Form16},
  winioctl in '..\..\winioctl.pas',
  crtdll_wrapper in '..\..\crtdll_wrapper.pas',
  hwid_impl in '..\..\hwid_impl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
