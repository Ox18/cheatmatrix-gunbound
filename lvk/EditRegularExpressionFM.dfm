object fmEditRegularExpression: TfmEditRegularExpression
  Left = 496
  Top = 406
  Width = 430
  Height = 382
  Caption = 'fmEditRegularExpression'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    422
    355)
  PixelsPerInch = 96
  TextHeight = 13
  object lblRegExp: TLabel
    Left = 4
    Top = 4
    Width = 90
    Height = 13
    Caption = 'Regular e&xpression'
  end
  object lblOptions: TLabel
    Left = 4
    Top = 52
    Width = 36
    Height = 13
    Caption = 'Options'
  end
  object BitBtn1: TBitBtn
    Left = 4
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BitBtn2: TBitBtn
    Left = 84
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object eOptions: TlvkDropDownEdit
    Left = 4
    Top = 68
    Width = 413
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
    Text = 'eOptions'
    Button.Font.Charset = DEFAULT_CHARSET
    Button.Font.Color = clWindowText
    Button.Font.Height = -11
    Button.Font.Name = 'MS Sans Serif'
    Button.Font.Style = []
    Button.Caption = '...'
    Button.Kind = lbkComboBoxLookalike
    Options = [deoCloseOnEsc, deoCloseOnEnter]
    OnCreateDropDownFrame = eOptionsCreateDropDownFrame
    OnAfterDropDown = eOptionsAfterDropDown
    OnBeforeCloseUp = eOptionsBeforeCloseUp
    OnAfterCloseUp = eOptionsAfterCloseUp
  end
  object pcTest: TPageControl
    Left = 4
    Top = 96
    Width = 413
    Height = 225
    ActivePage = tsMatch
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tsMatch: TTabSheet
      Caption = 'Match'
      DesignSize = (
        405
        197)
      object lblMatch: TLabel
        Left = 8
        Top = 8
        Width = 67
        Height = 13
        Caption = 'Match against'
      end
      object lblMatchResult: TLabel
        Left = 8
        Top = 180
        Width = 85
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'lblMatchResult'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object eMatch: TMemo
        Left = 8
        Top = 24
        Width = 385
        Height = 153
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Sample input')
        TabOrder = 0
        OnChange = eMatchChange
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Find'
      ImageIndex = 1
      DesignSize = (
        405
        197)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Find in'
      end
      object lblFindResult: TLabel
        Left = 168
        Top = 176
        Width = 74
        Height = 13
        Caption = 'lblFindResult'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object eFind: TMemo
        Left = 8
        Top = 24
        Width = 385
        Height = 137
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Sample input')
        TabOrder = 0
      end
      object btFindFirst: TButton
        Left = 8
        Top = 168
        Width = 75
        Height = 25
        Action = acFindFirst
        TabOrder = 1
      end
      object btFindNext: TButton
        Left = 88
        Top = 168
        Width = 75
        Height = 25
        Action = acFindNext
        TabOrder = 2
      end
    end
  end
  object eRegExp: TlvkDropDownEdit
    Left = 4
    Top = 20
    Width = 413
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'eRegExp'
    OnExit = eRegExpExit
    Button.Font.Charset = DEFAULT_CHARSET
    Button.Font.Color = clWindowText
    Button.Font.Height = -11
    Button.Font.Name = 'MS Sans Serif'
    Button.Font.Style = []
    Button.Caption = '...'
    Button.Kind = lbkComboBoxLookalike
    Options = [deoCloseOnEsc, deoCloseOnEnter]
    OnCreateDropDownFrame = eRegExpCreateDropDownFrame
    SizeGrip = True
  end
  object alEditRegularExpression: TActionList
    OnUpdate = alEditRegularExpressionUpdate
    Left = 36
    Top = 184
    object acFindFirst: TAction
      Caption = 'First'
      OnExecute = acFindFirstExecute
    end
    object acFindNext: TAction
      Caption = 'Next'
      OnExecute = acFindNextExecute
    end
  end
  object lvkINIFileStateStorage1: TlvkINIFileStateStorage
    Left = 184
    Top = 72
  end
  object lvkFormState1: TlvkFormState
    SectionName = 'fmEditRegularExpression.Form'
    Storage = lvkINIFileStateStorage1
    Options = [fsoPosition, fsoSize, fsoState, fsoActiveControl]
    Left = 216
    Top = 72
  end
end
