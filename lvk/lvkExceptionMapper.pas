{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkExceptionMapper component.
}
unit lvkExceptionMapper;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkExceptionMapper.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Forms, Classes, SysUtils, lvkComponents;

type
  { Description
      This component, when dropped on a form, will handle all the exceptions
      raised in the application and show the exact filename and linenumber that
      the exception was raised at. This can be a great help when debugging
      or testing applications as the filename#line number information is a bit
      more useful then an address.

      Note that for this component to be able to do its job you will need to
      make the following changes to your project options:
        * Enable Stack Frames (Compiler tab)
        * Produce a detailed map file (Linker tab)

      When you distribute the application, you must also distribute the
      .map file that is produced with it.
  }
  TlvkExceptionMapper = class(TlvkComponent)
  private
    FLongFormat : Boolean;

    procedure ExceptionHandler(Sender: TObject; E: Exception);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Description:
        This property controls wether the exception message will contain the
        long format or the short format of the exception location.

        Short format:
          Unit1.pas#123

        Long format:
          procedure TForm1.Button1Click in module Unit1 in file Unit1.pas at line 123
    }
    property LongFormat: Boolean read FLongFormat write FLongFormat;
  end;

resourcestring
  EXCEPTION_MESSAGE = '%0:s: %1:s raised at %2:s';
  EXCEPTION_CAPTION = '%0:s';

implementation

uses
  Dialogs, Windows, lvkMapFiles;

{ TlvkExceptionMapper }

constructor TlvkExceptionMapper.Create(AOwner: TComponent);
begin
  inherited;

  FLongFormat := False;
  if not (csDesigning in ComponentState) then
    Application.OnException := ExceptionHandler;
end;

destructor TlvkExceptionMapper.Destroy;
begin
  if not (csDesigning in ComponentState) then
    Application.OnException := nil;

  inherited;
end;

procedure TlvkExceptionMapper.ExceptionHandler(Sender: TObject;
  E: Exception);
var
  Message : string;
  Caption : string;
begin
  Message := Format(EXCEPTION_MESSAGE, [E.ClassName, E.Message, __DESCRIPTIVE_ADDRESS__(PChar(ExceptAddr)-1, not FLongFormat)]);
  Caption := Format(EXCEPTION_CAPTION, [E.ClassName, E.Message, __DESCRIPTIVE_ADDRESS__(PChar(ExceptAddr)-1, not FLongFormat)]);

  Application.MessageBox(
    PChar(Message),
    PChar(Caption),
    MB_ICONERROR);
end;

end.
