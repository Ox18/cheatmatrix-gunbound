object frmPresetRegularExpression: TfrmPresetRegularExpression
  Left = 0
  Top = 0
  Width = 380
  Height = 156
  Color = clWindow
  ParentColor = False
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 142
    Width = 246
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Select the preset regular expression you want to use'
  end
  object lbExpressions: TListBox
    Left = 0
    Top = 0
    Width = 380
    Height = 141
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbExpressionsDblClick
  end
  object lvkDoubleBuffered1: TlvkDoubleBuffered
    Left = 120
    Top = 56
  end
end
