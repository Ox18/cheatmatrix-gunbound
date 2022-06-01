library cmc;

(* **********************************************
  *                                             *
  *  Modulo de configuração dos plugins         *
  *                                             *
  ********************************************** *)

uses
    SysUtils,
    Classes,
    windows,
    forms,
    UnitMatriz in '..\..\UnitMatriz.pas' {frmMatriz} ,
    VMProtectSDK in '..\..\VMProtectSDK.pas',
    CmPlugins in '..\..\CmPlugins.pas',
    Constantes in '..\..\Constantes.pas',
    functions in '..\functions.pas',
    encription in '..\encription.pas',
    Base64 in '..\Base64.pas',
    CMCript in '..\CMCript.pas',
    CMSysUtils in '..\CMSysUtils.pas',
    TEA in '..\TEA.pas',
    ConfigsLoader in '..\..\ConfigsLoader.pas',
    variaveis in 'variaveis.pas';

{$R *.res}

procedure f1(lista: PAnsiChar; lpDiretorioBase: PAnsiChar; lpTempoString: PAnsiChar); stdcall;
var
    s: AnsiString;
    len, i: integer;
    ss: ShortString;
begin
    if (lpDiretorioBase <> nil) then
    begin
        diretorioBase := AnsiString(lpDiretorioBase);
        tempoString := lpTempoString;

        if lista <> nil then
        begin
            i := 0;
            repeat
				if (lista[i] = '|') then
				begin
					setlength(PluginsLiberados, length(PluginsLiberados) + 1);
					PluginsLiberados[length(PluginsLiberados) - 1] := AnsiString(s);
					s := '';
					inc(i);
					continue;
				end;

				s := s + lista[i];
				inc(i);

				if lista[i] = #0 then
				begin
					setlength(PluginsLiberados, length(PluginsLiberados) + 1);
					PluginsLiberados[length(PluginsLiberados) - 1] := AnsiString(s);
					s := '';
					break;
				end;
			until (lista[i] = #0);

            // @UnloadMatriz := POINTER(valor);
            Application.CreateForm(TfrmMatriz, frmMatriz);
            frmMatriz := TfrmMatriz.Create(nil);
            frmMatriz.ShowModal;
            //for i := 0 to length(PluginsLiberados) do
             //   FreeAndNil(PluginsLiberados[i]);                            Application.RemovePopupForm(frmMatriz);
            FreeAndNil(frmMatriz);
            //frmMatriz.Free;
            //MessageBox(0,'teste','ok',0);
        end;
    end;
end;

exports f1;

begin

end.
