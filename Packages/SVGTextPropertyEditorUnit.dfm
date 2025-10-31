object SVGTextPropertyEditorForm: TSVGTextPropertyEditorForm
  Left = 916
  Top = 169
  Caption = 
    'SVGText Property Editor %s - Copyright (c) Ethea S.r.l. - Apache' +
    ' 2.0 Open Source License'
  ClientHeight = 256
  ClientWidth = 739
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 440
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 13
  object RightSplitter: TSplitter
    Left = 550
    Top = 0
    Width = 4
    Height = 223
    Align = alRight
    AutoSnap = False
    MinSize = 16
    ExplicitLeft = 582
    ExplicitHeight = 309
  end
  object paBottom: TPanel
    Left = 0
    Top = 223
    Width = 739
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object paButtons: TPanel
      Left = 146
      Top = 0
      Width = 593
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object CancelButton: TButton
        AlignWithMargins = True
        Left = 432
        Top = 3
        Width = 75
        Height = 27
        Align = alLeft
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 4
      end
      object OKButton: TButton
        AlignWithMargins = True
        Left = 351
        Top = 3
        Width = 75
        Height = 27
        Margins.Left = 10
        Align = alLeft
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 3
      end
      object HelpButton: TButton
        AlignWithMargins = True
        Left = 513
        Top = 3
        Width = 74
        Height = 27
        Align = alLeft
        Caption = '&Help'
        TabOrder = 5
        OnClick = HelpButtonClick
      end
      object LoadButton: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 74
        Height = 27
        Align = alLeft
        Caption = '&Load file...'
        TabOrder = 0
        OnClick = LoadButtonClick
      end
      object SaveButton: TButton
        AlignWithMargins = True
        Left = 163
        Top = 3
        Width = 74
        Height = 27
        Align = alLeft
        Caption = '&Save...'
        TabOrder = 1
        OnClick = SaveButtonClick
      end
      object ReformatXMLButton: TButton
        AlignWithMargins = True
        Left = 243
        Top = 3
        Width = 95
        Height = 27
        Align = alLeft
        Caption = 'Reformat &XML'
        TabOrder = 2
        OnClick = ReformatXMLButtonClick
      end
      object LoadWebButton: TButton
        AlignWithMargins = True
        Left = 83
        Top = 3
        Width = 74
        Height = 27
        Align = alLeft
        Caption = '&Load WEB...'
        TabOrder = 6
        OnClick = LoadWebButtonClick
      end
    end
  end
  object SVGTextMemo: TMemo
    Left = 0
    Top = 0
    Width = 550
    Height = 223
    Align = alClient
    BevelOuter = bvNone
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = SVGTextMemoChange
  end
  object paImage: TPanel
    Left = 554
    Top = 0
    Width = 185
    Height = 223
    Align = alRight
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 2
    OnResize = paImageResize
    object paTitle: TPanel
      Left = 2
      Top = 2
      Width = 181
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
    object ImagePanel: TPanel
      AlignWithMargins = True
      Left = 2
      Top = 26
      Width = 181
      Height = 171
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvLowered
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      object SVGIconImage: TSVGIconImage
        Left = 1
        Top = 1
        Width = 179
        Height = 169
        AutoSize = False
        Center = False
        ImageIndex = 0
        Align = alClient
      end
    end
    object BottomPanel: TPanel
      Left = 2
      Top = 197
      Width = 181
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object ProportionalCheckBox: TCheckBox
        Left = 8
        Top = 4
        Width = 161
        Height = 17
        Caption = 'Proportional'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ProportionalCheckBoxClick
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 400
    Top = 24
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'SVG files (*.svg)|*.svg'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 456
    Top = 24
  end
end
