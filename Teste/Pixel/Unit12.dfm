object Form12: TForm12
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Teste'
  ClientHeight = 205
  ClientWidth = 235
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
    Width = 219
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 8
    Top = 35
    Width = 219
    Height = 21
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 62
    Width = 49
    Height = 41
    ParentBackground = False
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 176
    Top = 62
    Width = 51
    Height = 41
    Color = clBlue
    ParentBackground = False
    TabOrder = 3
  end
  object Panel3: TPanel
    Left = 63
    Top = 62
    Width = 51
    Height = 41
    Color = clRed
    ParentBackground = False
    TabOrder = 4
  end
  object Panel4: TPanel
    Left = 120
    Top = 62
    Width = 51
    Height = 41
    Color = clGreen
    ParentBackground = False
    TabOrder = 5
  end
  object Edit3: TEdit
    Left = 8
    Top = 145
    Width = 219
    Height = 21
    TabOrder = 6
    Text = 'testedll'
  end
  object Button1: TButton
    Left = 72
    Top = 172
    Width = 75
    Height = 25
    Caption = 'injct'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Edit4: TEdit
    Left = 8
    Top = 118
    Width = 219
    Height = 21
    TabOrder = 8
    Text = 'notepad.exe'
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 80
    Top = 16
  end
end
