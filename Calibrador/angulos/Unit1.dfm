object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 524
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    642
    524)
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 8
    Top = 51
    Width = 626
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 8
    Top = 15
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 135
    Top = 15
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object Button1: TButton
    Left = 265
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object StringGrid2: TStringGrid
    Left = 8
    Top = 223
    Width = 626
    Height = 146
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object Button2: TButton
    Left = 559
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 5
    OnClick = Button2Click
  end
  object RadioButton1: TRadioButton
    Left = 368
    Top = 15
    Width = 65
    Height = 17
    Caption = 'sin'
    Checked = True
    TabOrder = 6
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 448
    Top = 15
    Width = 65
    Height = 17
    Caption = 'cos'
    TabOrder = 7
  end
  object Memo1: TMemo
    Left = 8
    Top = 375
    Width = 626
    Height = 141
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 8
  end
end
