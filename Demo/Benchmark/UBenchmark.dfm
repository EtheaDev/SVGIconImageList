object frmBenchmark: TfrmBenchmark
  Left = 0
  Top = 0
  Caption = 'Benchmark'
  ClientHeight = 361
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 460
    Top = 0
    Height = 321
    Align = alRight
    Beveled = True
    ExplicitLeft = 632
    ExplicitTop = 176
    ExplicitHeight = 100
  end
  object memOutput: TMemo
    Left = 0
    Top = 0
    Width = 460
    Height = 321
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object SVGIconImage: TSVGIconImage
    Left = 463
    Top = 0
    Width = 321
    Height = 321
    AutoSize = False
    ImageList = imlIcons
    Align = alRight
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 321
    Width = 784
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblLoops: TLabel
      AlignWithMargins = True
      Left = 550
      Top = 12
      Width = 28
      Height = 20
      Margins.Left = 5
      Margins.Top = 12
      Margins.Right = 5
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Loops'
      FocusControl = speLoops
      ExplicitHeight = 13
    end
    object btnClear: TButton
      AlignWithMargins = True
      Left = 109
      Top = 3
      Width = 100
      Height = 34
      Align = alLeft
      Caption = '&Clear'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnLoad: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 100
      Height = 34
      Align = alLeft
      Caption = 'L&oad Image'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnRunBenchmark: TButton
      AlignWithMargins = True
      Left = 679
      Top = 5
      Width = 100
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = '&Benchmark'
      TabOrder = 2
      OnClick = btnRunBenchmarkClick
    end
    object speLoops: TSpinEdit
      AlignWithMargins = True
      Left = 588
      Top = 8
      Width = 81
      Height = 24
      Margins.Left = 5
      Margins.Top = 8
      Margins.Right = 5
      Margins.Bottom = 8
      Align = alRight
      MaxValue = 999
      MinValue = 1
      TabOrder = 3
      Value = 50
    end
    object chkGrayScale: TCheckBox
      AlignWithMargins = True
      Left = 342
      Top = 3
      Width = 97
      Height = 34
      Align = alRight
      Caption = '&Grayscale'
      Checked = True
      State = cbChecked
      TabOrder = 4
      ExplicitLeft = 333
      ExplicitTop = 6
    end
    object chkFixedColor: TCheckBox
      AlignWithMargins = True
      Left = 445
      Top = 3
      Width = 97
      Height = 34
      Align = alRight
      Caption = '&Fixed Color'
      Checked = True
      State = cbChecked
      TabOrder = 5
      ExplicitLeft = 263
      ExplicitTop = 6
    end
  end
  object SVGIconImageCollection: TSVGIconImageCollection
    SVGIconItems = <>
    Left = 48
    Top = 40
  end
  object imlIcons: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <>
    ImageCollection = SVGIconImageCollection
    Left = 48
    Top = 88
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.svg'
    Left = 48
    Top = 136
  end
end
