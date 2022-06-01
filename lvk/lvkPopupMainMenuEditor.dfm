object fmEditPopupMainMenu: TfmEditPopupMainMenu
  Left = 490
  Top = 324
  Width = 408
  Height = 224
  Caption = 'fmEditPopupMainMenu'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = SelectMenu
  OldCreateOrder = False
  DesignSize = (
    400
    197)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 108
    Width = 389
    Height = 85
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Please select the correct menu by clicking on the first sub-item' +
      ' in the menu. If you want the contents of the File menu shown in' +
      ' your popup, click on the first item in the File menu.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 61
    Height = 13
    Caption = 'Root caption'
  end
  object eRootCaption: TEdit
    Left = 8
    Top = 68
    Width = 387
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object SelectMenu: TMainMenu
    Left = 8
    Top = 16
  end
end
