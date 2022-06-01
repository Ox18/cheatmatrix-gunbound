object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 464
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 514
    Height = 21
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 35
    Width = 576
    Height = 254
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 319
    Width = 576
    Height = 106
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 431
    Width = 185
    Height = 25
    Caption = 'Translate'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 528
    Top = 6
    Width = 56
    Height = 25
    Caption = 'Limpa'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 432
    Top = 431
    Width = 152
    Height = 25
    Caption = 'Load'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 199
    Top = 431
    Width = 227
    Height = 25
    Caption = 'Clear'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 295
    Width = 576
    Height = 18
    Caption = 'Clear'
    TabOrder = 7
    OnClick = Button5Click
  end
  object SaveDialog1: TSaveDialog
    Left = 384
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 40
  end
end
