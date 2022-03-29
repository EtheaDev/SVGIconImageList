object SVGViewerForm: TSVGViewerForm
  Left = 0
  Top = 0
  Caption = 'SVG Preview & Engine Comparison'
  ClientHeight = 604
  ClientWidth = 823
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object RightPanel: TPanel
    Left = 624
    Top = 0
    Width = 199
    Height = 604
    Align = alRight
    TabOrder = 0
    object ListBox: TListBox
      Left = 1
      Top = 42
      Width = 197
      Height = 376
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxClick
      ExplicitHeight = 386
    end
    object OpenPanel: TPanel
      Left = 1
      Top = 1
      Width = 197
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
    object ColorGroupBox: TGroupBox
      Left = 1
      Top = 532
      Width = 197
      Height = 71
      Align = alBottom
      Caption = 'Fixed Color'
      TabOrder = 3
      ExplicitTop = 531
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
      Left = 1
      Top = 418
      Width = 197
      Height = 57
      Align = alBottom
      Caption = 'Aspect'
      TabOrder = 2
      ExplicitLeft = 3
      ExplicitTop = 390
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
      Left = 1
      Top = 475
      Width = 197
      Height = 57
      Align = alBottom
      Caption = 'Opacity:'
      TabOrder = 4
      ExplicitTop = 473
      object OpacityTrackBar: TTrackBar
        Left = 2
        Top = 16
        Width = 193
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
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 604
    Align = alLeft
    TabOrder = 1
    inline FrameViewSkia: TFrameView
      Left = 1
      Top = 301
      Width = 311
      Height = 302
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 301
      ExplicitWidth = 311
      ExplicitHeight = 302
      inherited ClientPanel: TPanel
        Width = 311
        Height = 302
        ExplicitWidth = 311
        ExplicitHeight = 302
        inherited SVGPaintBox: TPaintBox
          Width = 307
          Height = 275
          ExplicitWidth = 311
          ExplicitHeight = 273
        end
        inherited TitlePanel: TPanel
          Width = 307
          ExplicitWidth = 307
        end
      end
    end
    inline FrameViewTSVG: TFrameView
      Left = 1
      Top = 1
      Width = 311
      Height = 300
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 311
      ExplicitHeight = 300
      inherited ClientPanel: TPanel
        Width = 311
        Height = 300
        ExplicitWidth = 311
        ExplicitHeight = 300
        inherited SVGPaintBox: TPaintBox
          Width = 307
          Height = 273
          ExplicitWidth = 303
          ExplicitHeight = 275
        end
        inherited TitlePanel: TPanel
          Width = 307
          ExplicitWidth = 307
        end
      end
    end
  end
  object ClientPanel: TPanel
    Left = 313
    Top = 0
    Width = 311
    Height = 604
    Align = alClient
    TabOrder = 2
    inline FrameViewImage32: TFrameView
      Left = 1
      Top = 301
      Width = 309
      Height = 302
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 301
      ExplicitWidth = 309
      ExplicitHeight = 302
      inherited ClientPanel: TPanel
        Width = 309
        Height = 302
        ExplicitWidth = 309
        ExplicitHeight = 302
        inherited SVGPaintBox: TPaintBox
          Width = 305
          Height = 275
          ExplicitWidth = 303
          ExplicitHeight = 273
        end
        inherited TitlePanel: TPanel
          Width = 305
          ExplicitWidth = 305
        end
      end
    end
    inline FrameViewerD2D: TFrameView
      Left = 1
      Top = 1
      Width = 309
      Height = 300
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 309
      ExplicitHeight = 300
      inherited ClientPanel: TPanel
        Width = 309
        Height = 300
        ExplicitWidth = 309
        ExplicitHeight = 300
        inherited SVGPaintBox: TPaintBox
          Width = 305
          Height = 273
          ExplicitLeft = 0
          ExplicitTop = 23
          ExplicitWidth = 323
          ExplicitHeight = 273
        end
        inherited TitlePanel: TPanel
          Width = 305
          ExplicitWidth = 305
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 456
    Top = 120
  end
end
