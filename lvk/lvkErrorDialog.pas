{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component that is used to display error messages
    with.
}
unit lvkErrorDialog;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkErrorDialog.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Forms, SysUtils, Classes, lvkComponents;

const
  DEFAULT_AUTO_CLEAR  = False;

type
  TlvkErrorType = (etError, etWarning, etNote);

  TlvkErrorRec = record
    ErrorType         : TlvkErrorType;
    Message           : string;
    ExtraInformation  : string;
    URL               : string;
  end;

  TlvkErrorDialog = class(TlvkComponent)
  private
    FErrors     : array of TlvkErrorRec;
    FAutoClear  : Boolean;
    FCaption    : string;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;

    procedure AddError(const ErrorMessage: string;
      const ExtraInformation: string=''; const URL: string='');
    procedure AddWarning(const ErrorMessage: string;
      const ExtraInformation: string=''; const URL: string='');
    procedure AddNote(const ErrorMessage: string;
      const ExtraInformation: string=''; const URL: string='');
    procedure Add(const ErrorType: TlvkErrorType;
      const ErrorMessage: string; const ExtraInformation: string='';
      const URL: string='');

    function GotErrors: Boolean;
    function Show: Boolean;

  published
    property AutoClear: Boolean read FAutoClear write FAutoClear
      default DEFAULT_AUTO_CLEAR;
    property Caption: string read FCaption write FCaption;
  end;

resourcestring
  DEFAULT_LVKERRORDIALOG_CAPTION  = 'Errormessages';

implementation

uses
  lvkErrorDialogFM;

{ TlvkErrorDialog }

procedure TlvkErrorDialog.Add(const ErrorType: TlvkErrorType;
  const ErrorMessage, ExtraInformation, URL: string);
begin
  SetLength(FErrors, Length(FErrors)+1);
  FErrors[High(FErrors)].ErrorType := ErrorType;
  FErrors[High(FErrors)].Message := ErrorMessage;
  FErrors[High(FErrors)].ExtraInformation := ExtraInformation;
  FErrors[High(FErrors)].URL := URL;
end;

procedure TlvkErrorDialog.AddError(const ErrorMessage, ExtraInformation,
  URL: string);
begin
  Add(etError, ErrorMessage, ExtraInformation, URL);
end;

procedure TlvkErrorDialog.AddNote(const ErrorMessage, ExtraInformation,
  URL: string);
begin
  Add(etNote, ErrorMessage, ExtraInformation, URL);
end;

procedure TlvkErrorDialog.AddWarning(const ErrorMessage, ExtraInformation,
  URL: string);
begin
  Add(etWarning, ErrorMessage, ExtraInformation, URL);
end;

procedure TlvkErrorDialog.Clear;
begin
  SetLength(FErrors, 0);
end;

constructor TlvkErrorDialog.Create(AOwner: TComponent);
begin
  inherited;

  FCaption := DEFAULT_LVKERRORDIALOG_CAPTION;
  FAutoClear := DEFAULT_AUTO_CLEAR;
end;

function TlvkErrorDialog.GotErrors: Boolean;
begin
  Result := Length(FErrors) > 0;
end;

function TlvkErrorDialog.Show: Boolean;
var
  Index : Integer;
begin
  Result := False;

  if Length(FErrors) = 0 then
    Exit;

  with TfmlvkErrorDialog.Create(Application) do
  try
    Caption := FCaption;

    for Index := Low(FErrors) to High(FErrors) do
      Add(FErrors[Index].ErrorType, FErrors[Index].Message, FErrors[Index].ExtraInformation, FErrors[Index].URL);

    ShowModal;

    if FAutoClear then
      Clear;

    Result := True;
  finally
    Free;
  end;
end;

end.
 