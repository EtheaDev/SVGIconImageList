object SVGViewerForm: TSVGViewerForm
  Left = 0
  Top = 0
  Caption = 'SVG Preview & Engine Comparison'
  ClientHeight = 600
  ClientWidth = 800
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object FilesPanel: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 600
    Align = alLeft
    TabOrder = 0
    object ListBox: TListBox
      Left = 1
      Top = 42
      Width = 198
      Height = 557
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxClick
    end
    object OpenPanel: TPanel
      Left = 1
      Top = 1
      Width = 198
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object OpenButton: TButton
        Left = 4
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Open...'
        TabOrder = 0
        OnClick = OpenButtonClick
      end
      object SetPathButton: TButton
        Left = 85
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Set Path...'
        TabOrder = 1
        OnClick = SetPathButtonClick
      end
    end
  end
  object RightPanel: TPanel
    Left = 500
    Top = 0
    Width = 300
    Height = 600
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    inline FrameViewSkia: TFrameView
      Left = 0
      Top = 300
      Width = 300
      Height = 300
      Align = alClient
      TabOrder = 0
    end
    object ControlPanel: TPanel
      Left = 0
      Top = 0
      Width = 300
      Height = 300
      Align = alTop
      BevelOuter = bvNone
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      object ColorGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 294
        Height = 71
        Align = alTop
        Caption = 'Fixed Color'
        TabOrder = 0
        object FixedColorComboBox: TColorBox
          Left = 11
          Top = 17
          Width = 178
          Height = 22
          DefaultColorColor = clDefault
          NoneColorColor = clNone
          Selected = clDefault
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 0
          OnChange = FixedColorComboBoxChange
        end
        object ApplyToRootOnlyCheckBox: TCheckBox
          Left = 10
          Top = 45
          Width = 130
          Height = 17
          Caption = 'Apply To Root Only'
          TabOrder = 1
          OnClick = ApplyToRootOnlyCheckBoxClick
        end
      end
      object AspectGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 105
        Width = 294
        Height = 57
        Align = alTop
        Caption = 'Aspect'
        TabOrder = 1
        object KeepCheckBox: TCheckBox
          Left = 12
          Top = 16
          Width = 130
          Height = 17
          Caption = 'Keep aspect ratio'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = KeepCheckBoxClick
        end
        object GrayScaleCheckBox: TCheckBox
          Left = 12
          Top = 34
          Width = 130
          Height = 17
          Caption = 'GrayScale'
          TabOrder = 1
          OnClick = GrayScaleCheckBoxClick
        end
      end
      object OpacityGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 168
        Width = 294
        Height = 57
        Align = alTop
        Caption = 'Opacity:'
        TabOrder = 2
        object OpacityTrackBar: TTrackBar
          Left = 2
          Top = 16
          Width = 290
          Height = 39
          Align = alBottom
          Max = 100
          Frequency = 5
          Position = 100
          PositionToolTip = ptBottom
          TabOrder = 0
          OnChange = OpacityTrackBarChange
        end
      end
      object TitlePanel: TPanel
        AlignWithMargins = True
        Left = 1
        Top = 1
        Width = 298
        Height = 23
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Control Panel'
        Color = clHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlightText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 3
        StyleElements = [seBorder]
      end
    end
  end
  object ClientPanel: TPanel
    Left = 200
    Top = 0
    Width = 300
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    inline FrameViewerD2D: TFrameView
      Left = 0
      Top = 300
      Width = 300
      Height = 300
      Align = alClient
      TabOrder = 0
    end
    inline FrameViewImage32: TFrameView
      Left = 0
      Top = 0
      Width = 300
      Height = 300
      Align = alTop
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 456
    Top = 120
  end
end
