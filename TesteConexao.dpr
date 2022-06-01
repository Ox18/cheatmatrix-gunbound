program TesteConexao;

uses
  Forms,
  unitTesteConexao in 'unitTesteConexao.pas' {Form12},
  SkySql in 'SkySql.pas',
  Constantes in 'Constantes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
