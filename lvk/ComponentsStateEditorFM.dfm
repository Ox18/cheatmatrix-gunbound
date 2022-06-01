object fmComponentsStateEditor: TfmComponentsStateEditor
  Left = 416
  Top = 301
  Width = 470
  Height = 276
  Caption = 'fmComponentsStateProperties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 5
    Height = 216
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object paBottom: TPanel
    Left = 0
    Top = 216
    Width = 462
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      462
      33)
    object btOk: TBitBtn
      Left = 301
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btOkClick
      Kind = bkOK
    end
    object btCancel: TBitBtn
      Left = 381
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object paAvailableProperties: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 216
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      185
      216)
    object lblAvailableProperties: TLabel
      Left = 4
      Top = 52
      Width = 92
      Height = 13
      Caption = '&Available properties'
      FocusControl = lbAvailableProperties
    end
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 104
      Height = 13
      Caption = 'Available &components'
    end
    object lbAvailableProperties: TlvkListBox
      Left = 4
      Top = 68
      Width = 177
      Height = 144
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      PopupMenu = pmAvailableProperties
      Sorted = True
      TabOrder = 1
      OnDblClick = lbAvailablePropertiesDblClick
    end
    object cbComponents: TlvkComboBox
      Left = 4
      Top = 20
      Width = 177
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Text = 'cbComponents'
      OnChange = cbComponentsChange
    end
  end
  object paSavedProperties: TPanel
    Left = 190
    Top = 0
    Width = 272
    Height = 216
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      272
      216)
    object lblSavedProperties: TLabel
      Left = 84
      Top = 4
      Width = 47
      Height = 13
      Caption = '&Properties'
      FocusControl = lbSavedProperties
    end
    object lbSavedProperties: TlvkListBox
      Left = 84
      Top = 20
      Width = 181
      Height = 192
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      PopupMenu = pmSavedProperties
      TabOrder = 4
      OnDblClick = lbSavedPropertiesDblClick
    end
    object btAdd: TButton
      Left = 5
      Top = 20
      Width = 75
      Height = 25
      Action = acAdd
      TabOrder = 0
    end
    object btRemove: TButton
      Left = 5
      Top = 48
      Width = 75
      Height = 25
      Action = acRemove
      TabOrder = 1
    end
    object btMoveUp: TButton
      Left = 5
      Top = 160
      Width = 75
      Height = 25
      Action = acMoveUp
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object btMoveDown: TButton
      Left = 5
      Top = 188
      Width = 75
      Height = 25
      Action = acMoveDown
      Anchors = [akLeft, akBottom]
      TabOrder = 3
    end
  end
  object alComponentsStateEditor: TActionList
    OnUpdate = alComponentsStateEditorUpdate
    Left = 8
    Top = 72
    object acAdd: TAction
      Caption = '&Add -->'
      ShortCut = 16429
      OnExecute = acAddExecute
    end
    object acRemove: TAction
      Caption = '<-- &Remove'
      ShortCut = 16430
      OnExecute = acRemoveExecute
    end
    object acMoveUp: TAction
      Caption = 'Move &up'
      ShortCut = 16422
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move &down'
      ShortCut = 16424
      OnExecute = acMoveDownExecute
    end
    object acClear: TAction
      Caption = '&Clear'
      OnExecute = acClearExecute
    end
  end
  object stIniFile: TlvkINIFileStateStorage
    FileName = 'ComponentsStateProperties.ini'
    Left = 40
    Top = 72
  end
  object stForm: TlvkFormState
    SectionName = 'fmComponentsStateEditor.Form'
    Storage = stIniFile
    Options = [fsoPosition, fsoSize, fsoState, fsoActiveControl]
    Left = 72
    Top = 72
  end
  object stComponents: TlvkComponentsState
    SectionName = 'fmComponentsStateEditor.Components'
    Storage = stIniFile
    Properties.Strings = (
      'paAvailableProperties.Width')
    Left = 104
    Top = 72
  end
  object pmAvailableProperties: TPopupMenu
    Left = 8
    Top = 104
    object Add1: TMenuItem
      Action = acAdd
    end
  end
  object pmSavedProperties: TPopupMenu
    Left = 280
    Top = 24
    object Remove1: TMenuItem
      Action = acRemove
    end
    object Clear1: TMenuItem
      Action = acClear
    end
  end
  object PropertyEnabler: TlvkEventBasedEnabler
    Controls = <
      item
        Control = lbAvailableProperties
      end
      item
        Control = lblAvailableProperties
      end>
    OnUpdate = PropertyEnablerUpdate
    Left = 40
    Top = 104
  end
end
