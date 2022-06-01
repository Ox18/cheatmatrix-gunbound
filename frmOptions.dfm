object frameOptions: TframeOptions
  Left = 0
  Top = 0
  Width = 330
  Height = 246
  Color = 14737632
  ParentColor = False
  TabOrder = 0
  TabStop = True
  object GroupBox3: TGroupBox
    Left = 22
    Top = 224
    Width = 3
    Height = 9
    Caption = ' Atualiza'#231#245'es '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Visible = False
    object Label2: TLabel
      Left = 16
      Top = 46
      Width = 196
      Height = 13
      Caption = 'Quando houver atualiza'#231#245'es dispon'#237'veis:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object AutoGetUpd: TCheckBox
      Left = 16
      Top = 21
      Width = 227
      Height = 17
      Caption = 'Verificar atualiza'#231#245'es ao iniciar o CM'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = AutoGetUpdClick
    end
    object AutoUpd: TRadioButton
      Left = 32
      Top = 65
      Width = 211
      Height = 17
      Caption = 'Atualizar automaticamente'
      TabOrder = 1
      OnClick = AutoUpdClick
    end
    object NOUpd: TRadioButton
      Left = 29
      Top = 103
      Width = 211
      Height = 17
      Caption = 'Nada a fazer'
      TabOrder = 2
      OnClick = NOUpdClick
    end
    object ShowUpd: TRadioButton
      Left = 32
      Top = 79
      Width = 208
      Height = 25
      Caption = 'Apenas mostrar'
      Checked = True
      TabOrder = 3
      TabStop = True
      WordWrap = True
      OnClick = ShowUpdClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 22
    Top = 24
    Width = 91
    Height = 113
    Caption = ' Conex'#227'o '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label1: TLabel
      Left = 13
      Top = 24
      Width = 51
      Height = 13
      Caption = 'Usar Porta'
    end
    object edtConPort: TSpinEdit
      Left = 13
      Top = 40
      Width = 65
      Height = 22
      Color = clWhite
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 8080
    end
  end
  object GroupBox1: TGroupBox
    Left = 119
    Top = 24
    Width = 186
    Height = 113
    Caption = ' Lista de Processos '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object cbShowHidden: TCheckBox
      Left = 17
      Top = 20
      Width = 161
      Height = 33
      Caption = 'N'#227'o mostrar processos ocultos'
      TabOrder = 0
      WordWrap = True
      OnClick = AutoGetUpdClick
    end
    object boxSelectAuto: TCheckBox
      Left = 17
      Top = 59
      Width = 157
      Height = 17
      Caption = 'Selecionar automaticamente'
      Enabled = False
      TabOrder = 1
    end
    object boxProtegido: TCheckBox
      Left = 17
      Top = 82
      Width = 157
      Height = 17
      Caption = 'Modo Protegido'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 2
    end
  end
end
