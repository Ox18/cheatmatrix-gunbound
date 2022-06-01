object Form12: TForm12
  Left = 0
  Top = 0
  Caption = 'Form12'
  ClientHeight = 597
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    812
    597)
  PixelsPerInch = 96
  TextHeight = 13
  object valorFVento: TLabel
    Left = 84
    Top = 8
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 62
    Height = 13
    Caption = 'For'#231'a Vento:'
  end
  object Label4: TLabel
    Left = 136
    Top = 8
    Width = 68
    Height = 13
    Caption = 'Angulo Vento:'
  end
  object valorAVento: TLabel
    Left = 210
    Top = 8
    Width = 6
    Height = 13
    Caption = '0'
  end
  object valorFBot: TLabel
    Left = 72
    Top = 56
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label3: TLabel
    Left = 16
    Top = 56
    Width = 50
    Height = 13
    Caption = 'For'#231'a Bot:'
  end
  object Label5: TLabel
    Left = 280
    Top = 57
    Width = 56
    Height = 13
    Caption = 'Angulo Bot:'
  end
  object valorABot: TLabel
    Left = 342
    Top = 57
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Shape1: TShape
    Left = 36
    Top = 520
    Width = 30
    Height = 41
    Shape = stCircle
  end
  object Label2: TLabel
    Left = 722
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Tiro:'
  end
  object valorTiro: TLabel
    Left = 750
    Top = 8
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label6: TLabel
    Left = 725
    Top = 56
    Width = 30
    Height = 13
    Caption = 'Fator:'
  end
  object valorFator: TLabel
    Left = 760
    Top = 56
    Width = 6
    Height = 13
    Caption = '0'
  end
  object trackFV: TTrackBar
    Left = 8
    Top = 24
    Width = 113
    Height = 27
    Max = 26
    Position = 10
    TabOrder = 0
    ThumbLength = 15
  end
  object trackAV: TTrackBar
    Left = 127
    Top = 21
    Width = 601
    Height = 30
    Anchors = [akLeft, akTop, akRight]
    Max = 360
    TabOrder = 1
    ThumbLength = 15
  end
  object trackFB: TTrackBar
    Left = 8
    Top = 70
    Width = 266
    Height = 27
    Max = 400
    Position = 200
    TabOrder = 2
    ThumbLength = 15
  end
  object trackAB: TTrackBar
    Left = 280
    Top = 69
    Width = 393
    Height = 30
    Anchors = [akLeft, akTop, akRight]
    Max = 360
    Position = 45
    TabOrder = 3
    ThumbLength = 15
  end
  object comboMobiles: TComboBox
    Left = 16
    Top = 114
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'Armor'
    Items.Strings = (
      'Armor'
      'Mage'
      'Nak'
      'Trico'
      'Big Foot'
      'Boomer'
      'Raon'
      'Lightning'
      'J.D'
      'A.Sate'
      'Turtle'
      'Ice'
      'Grub'
      'Aduka'
      'Kasilddon'
      'J.Frog'
      'Dragon'
      'Knight'
      'Phoenix'
      'Maya'
      'Wolf'
      'Tiburon'
      'BlueWhale'
      'Frank'
      'Random')
  end
  object trackTiro: TTrackBar
    Left = 714
    Top = 21
    Width = 70
    Height = 34
    Max = 3
    TabOrder = 5
    ThumbLength = 15
  end
  object SpinEdit1: TSpinEdit
    Left = 688
    Top = 75
    Width = 96
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 5
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 16
    Top = 160
  end
  object Timer2: TTimer
    Interval = 400
    OnTimer = Timer2Timer
    Left = 64
    Top = 160
  end
end
