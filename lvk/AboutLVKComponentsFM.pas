{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the about box for my components.
}
unit AboutLVKComponentsFM;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 3 $
// $Archive: /Components/LVK/source/AboutLVKComponentsFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, lvkVersion, lvkConsole, lvkControls;

type
  TfmAboutLVKComponents = class(TForm)
    paBottom: TPanel;
    btOK: TButton;
    AboutConsole: TlvkConsole;
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAboutLVKComponents: TfmAboutLVKComponents;

implementation

{$R *.DFM}

procedure TfmAboutLVKComponents.btOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmAboutLVKComponents.FormCreate(Sender: TObject);
begin
  Caption := Format(Caption, [lvkVersion.PackageVersion]);
  btOK.Left := (paBottom.ClientWidth - btOK.Width) div 2;

  AboutConsole.Add('<font name="Verdana" size="32">LVK Components</font>');
  AboutConsole.Add('Installed version: %s', [lvkVersion.PackageVersion]);
  AboutConsole.Add('These components were designed and written by ' +
    '<a type="url" hint="Send email to Lasse Vågsæther Karlsen" params="mailto:lasse@vkarlsen.no">Lasse Vågsæther Karlsen</a>.');
  AboutConsole.Add('(C) 1999-2002, Lasse Vågsæther Karlsen');
  AboutConsole.Add('For more information, please visit the ' +
    '<a type="url" hint="Open webpage in default web browser" params="http://www.vkarlsen.no/">webpage of LVK Components</a>.');
end;

end.
