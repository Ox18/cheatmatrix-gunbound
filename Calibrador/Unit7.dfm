object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 573
  ClientWidth = 793
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    793
    573)
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 0
    Height = 573
    ExplicitLeft = 376
    ExplicitTop = 160
    ExplicitHeight = 100
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 417
    Height = 573
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 423
    Top = 0
    Width = 186
    Height = 92
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 1
    OnEnter = Memo2Enter
  end
  object Memo3: TMemo
    Left = 608
    Top = 0
    Width = 185
    Height = 92
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 2
    OnEnter = Memo2Enter
  end
  object Memo4: TMemo
    Left = 423
    Top = 95
    Width = 185
    Height = 82
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 3
    OnEnter = Memo2Enter
  end
  object Memo5: TMemo
    Left = 607
    Top = 95
    Width = 186
    Height = 82
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 4
    OnEnter = Memo2Enter
  end
  object Memo6: TMemo
    Left = 423
    Top = 180
    Width = 370
    Height = 393
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 5
  end
end
