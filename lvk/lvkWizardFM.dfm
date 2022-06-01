object fmlvkWizard: TfmlvkWizard
  Left = 273
  Top = 206
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'fmlvkWizard'
  ClientHeight = 288
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object WizardPanel: TPanel
    Left = 96
    Top = 0
    Width = 300
    Height = 256
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object paWizardArea: TPanel
      Left = 0
      Top = 56
      Width = 300
      Height = 200
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = paWizardAreaResize
    end
    object paWizardHeader: TPanel
      Left = 0
      Top = 0
      Width = 300
      Height = 56
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      DesignSize = (
        300
        56)
      object lblPageTitle: TLabel
        Left = 20
        Top = 12
        Width = 51
        Height = 13
        Caption = 'Pagetitle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblPageSubtitle: TLabel
        Left = 40
        Top = 28
        Width = 35
        Height = 13
        Caption = 'Subtitle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object imgHeader: TImage
        Left = 248
        Top = 4
        Width = 48
        Height = 48
        Anchors = [akTop, akRight]
      end
      object Bevel2: TBevel
        Left = 0
        Top = 6
        Width = 300
        Height = 50
        Align = alBottom
        Shape = bsBottomLine
        Style = bsRaised
      end
    end
  end
  object paWatermark: TPanel
    Left = 0
    Top = 0
    Width = 96
    Height = 256
    Align = alLeft
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    OnResize = paWatermarkResize
    object imgWatermark: TImage
      Left = 0
      Top = 0
      Width = 96
      Height = 256
      Align = alClient
      Stretch = True
    end
    object stWatermark: TStaticText
      Left = 4
      Top = 4
      Width = 64
      Height = 17
      BorderStyle = sbsSunken
      Caption = 'stWatermark'
      TabOrder = 0
      Visible = False
    end
  end
  object paWizardButtons: TPanel
    Left = 0
    Top = 256
    Width = 396
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      396
      32)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 396
      Height = 50
      Align = alTop
      Shape = bsTopLine
    end
    object btCancel: TBitBtn
      Left = 314
      Top = 5
      Width = 75
      Height = 25
      Action = acCancel
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 0
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
    object btFinish: TBitBtn
      Left = 234
      Top = 5
      Width = 75
      Height = 25
      Action = acFinish
      Anchors = [akRight, akBottom]
      Caption = '&Finish'
      Default = True
      TabOrder = 1
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
    object btNext: TBitBtn
      Left = 84
      Top = 5
      Width = 75
      Height = 25
      Action = acNext
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = '&Next'
      TabOrder = 2
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333FF3333333333333003333
        3333333333773FF3333333333309003333333333337F773FF333333333099900
        33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
        99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
        33333333337F3F77333333333309003333333333337F77333333333333003333
        3333333333773333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object btBack: TBitBtn
      Left = 4
      Top = 5
      Width = 75
      Height = 25
      Action = acBack
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = '&Back'
      TabOrder = 3
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333FF3333333333333003333333333333F77F33333333333009033
        333333333F7737F333333333009990333333333F773337FFFFFF330099999000
        00003F773333377777770099999999999990773FF33333FFFFF7330099999000
        000033773FF33777777733330099903333333333773FF7F33333333333009033
        33333333337737F3333333333333003333333333333377333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object stWizard: TStaticText
      Left = 160
      Top = 12
      Width = 73
      Height = 17
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'stWizard'
      TabOrder = 4
      Visible = False
    end
  end
  object stWizardArea: TStaticText
    Left = 97
    Top = 57
    Width = 67
    Height = 17
    BorderStyle = sbsSunken
    Caption = 'stWizardArea'
    TabOrder = 3
    Visible = False
  end
  object alWizard: TActionList
    OnUpdate = alWizardUpdate
    Left = 8
    Top = 44
    object acBack: TAction
      Caption = '&Back'
      OnExecute = acBackExecute
    end
    object acNext: TAction
      Caption = '&Next'
      OnExecute = acNextExecute
    end
    object acFinish: TAction
      Caption = '&Finish'
      OnExecute = acFinishExecute
    end
    object acCancel: TAction
      Caption = '&Cancel'
      OnExecute = acCancelExecute
    end
  end
end
