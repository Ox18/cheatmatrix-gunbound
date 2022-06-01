object frmRegularExpressionOptions: TfrmRegularExpressionOptions
  Left = 0
  Top = 0
  Width = 190
  Height = 184
  Color = clWindow
  ParentColor = False
  TabOrder = 0
  object chkGreedy: TlvkCheckBox
    Left = 4
    Top = 4
    Width = 85
    Height = 17
    AutoSize = True
    Caption = 'Greedy match'
    TabOrder = 0
  end
  object chkCaseSensitive: TlvkCheckBox
    Left = 4
    Top = 24
    Width = 87
    Height = 17
    AutoSize = True
    Caption = 'Case sensitive'
    TabOrder = 1
  end
  object chkMultiLine: TlvkCheckBox
    Left = 4
    Top = 44
    Width = 60
    Height = 17
    AutoSize = True
    Caption = 'Multi-line'
    TabOrder = 2
  end
  object chkDotAll: TlvkCheckBox
    Left = 4
    Top = 64
    Width = 83
    Height = 17
    AutoSize = True
    Caption = 'Dot means all'
    TabOrder = 3
  end
  object chkDollarEndOnly: TlvkCheckBox
    Left = 4
    Top = 84
    Width = 111
    Height = 17
    AutoSize = True
    Caption = '$ only matches end'
    TabOrder = 4
  end
  object chkBOL: TlvkCheckBox
    Left = 4
    Top = 104
    Width = 177
    Height = 17
    AutoSize = True
    Caption = 'First character is the start of a line'
    TabOrder = 5
  end
  object chkEOL: TlvkCheckBox
    Left = 4
    Top = 124
    Width = 176
    Height = 17
    AutoSize = True
    Caption = 'Last character is the end of a line'
    TabOrder = 6
  end
  object chkEmptyValid: TlvkCheckBox
    Left = 4
    Top = 144
    Width = 115
    Height = 17
    AutoSize = True
    Caption = 'Empty match is valid'
    TabOrder = 7
  end
  object chkAnchored: TlvkCheckBox
    Left = 4
    Top = 164
    Width = 128
    Height = 17
    AutoSize = True
    Caption = 'Anchor to start of string'
    TabOrder = 8
  end
end
