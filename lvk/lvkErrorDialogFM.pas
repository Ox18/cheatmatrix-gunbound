{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the form used from TlvkErrorDialog.
  See also:
    TlvkErrorDialog
}
unit lvkErrorDialogFM;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkErrorDialogFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ImgList, lvkErrorDialog, ShellAPI;

type
  TfmlvkErrorDialog = class(TForm)
    sbErrors: TScrollBox;
    paBottom: TPanel;
    btOk: TBitBtn;
    ilErrorDialog: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure URLEnter(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sbErrorsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure URLClick(Sender: TObject);
  private
    FY    : Integer;
    FURL  : TStaticText;

  public
    procedure Add(const ErrorType: TlvkErrorType; const ErrorMessage: string; const ExtraInformation: string; const URL: string);
  end;

var
  fmlvkErrorDialog: TfmlvkErrorDialog;

implementation

{$R *.DFM}

{ TfmlvkErrorDialog }

procedure TfmlvkErrorDialog.Add(const ErrorType: TlvkErrorType;
  const ErrorMessage, ExtraInformation, URL: string);
var
  img     : TImage;
  lbl     : TLabel;
  stt     : TStaticText;
  Bitmap  : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    ilErrorDialog.GetBitmap(Ord(ErrorType), Bitmap);
    img := TImage.Create(Self);
    img.AutoSize := True;
    img.Picture.Assign(Bitmap);
    img.Transparent := True;
    img.Parent := sbErrors;
    img.Left := 4;
    img.Top := FY;
  finally
    Bitmap.Free;
  end;

  lbl := TLabel.Create(Self);
  lbl.Caption := ErrorMessage;
  lbl.Parent := sbErrors;
  lbl.Left := 40;
  if (ExtraInformation = '') and (URL = '') then
    lbl.Top := FY + 8
  else
    lbl.Top := FY;
  lbl.Font.Style := [fsBold];

  if ExtraInformation <> '' then
  begin
    lbl := TLabel.Create(Self);
    lbl.Caption := ExtraInformation;
    lbl.Parent := sbErrors;
    lbl.AutoSize := False;
    lbl.Width := sbErrors.ClientWidth - 40 - 4;
    lbl.Anchors := [akLeft, akTop, akRight];
    lbl.Left := 40;
    lbl.Top := FY + 16;
  end;

  if URL <> '' then
  begin
    stt := TStaticText.Create(Self);
    stt.Caption := URL;
    stt.Parent := sbErrors;
    stt.Left := 40;
    stt.Font.Color := clBlue;
    stt.Cursor := crHandPoint;

    if ExtraInformation <> '' then
      stt.Top := FY + 32
    else
      stt.Top := FY + 16;

    stt.OnMouseMove := URLEnter;
    stt.OnClick := URLClick;
  end;

  Inc(FY, 40);
  if (URL <> '') and (ExtraInformation <> '') then
    Inc(FY, 16);
end;

procedure TfmlvkErrorDialog.FormCreate(Sender: TObject);
begin
  FY := 4;
end;

procedure TfmlvkErrorDialog.URLEnter(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  (Sender as TStaticText).Font.Style := [fsUnderline];
  FURL := (Sender as TStaticText);
end;

procedure TfmlvkErrorDialog.sbErrorsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FURL <> nil then
  begin
    FURL.Font.Style := [];
    FURL := nil;
  end;
end;

procedure TfmlvkErrorDialog.URLClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'OPEN', PChar((Sender as TStaticText).Caption), nil, nil, SW_NORMAL);
  sbErrorsMouseMove(Sender, [], 0, 0);
end;

end.
