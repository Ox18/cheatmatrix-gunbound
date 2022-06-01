object Form15: TForm15
  Left = 0
  Top = 0
  Caption = 'Servidor'
  ClientHeight = 332
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 335
    Top = 247
    Width = 83
    Height = 25
    Caption = 'Enviar'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 410
    Height = 201
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 247
    Width = 321
    Height = 21
    TabOrder = 2
  end
  object porta: TSpinEdit
    Left = 8
    Top = 8
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 1313
  end
  object SpinEdit1: TSpinEdit
    Left = 8
    Top = 296
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 1313
  end
  object SpinEdit2: TSpinEdit
    Left = 95
    Top = 296
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 1314
  end
  object SpinEdit3: TSpinEdit
    Left = 182
    Top = 296
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 1315
  end
  object SpinEdit4: TSpinEdit
    Left = 269
    Top = 294
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 1316
  end
end
