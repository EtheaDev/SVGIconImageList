object SVGIconImageListEditor: TSVGIconImageListEditor
  Left = 392
  Top = 450
  Caption = 'SVG Icon ImageList Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 526
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TopSplitter: TSplitter
    Left = 0
    Top = 183
    Width = 679
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 170
    ExplicitLeft = 4
    ExplicitTop = 23
    ExplicitWidth = 675
  end
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 187
    Width = 679
    Height = 306
    Align = alClient
    Caption = '%d Icons of Imagelist'
    TabOrder = 1
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 675
      Height = 289
      Align = alClient
      Columns = <>
      DragMode = dmAutomatic
      FullDrag = True
      HideSelection = False
      IconOptions.AutoArrange = True
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnDragDrop = ImageViewDragDrop
      OnDragOver = ImageViewDragOver
      OnKeyDown = ImageViewKeyDown
      OnSelectItem = ImageViewSelectItem
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 679
    Height = 183
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 596
      Height = 183
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageListGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 596
        Height = 62
        Align = alTop
        Caption = 'Properties of ImageList'
        TabOrder = 0
        object SizeLabel: TLabel
          Left = 8
          Top = 15
          Width = 80
          Height = 13
          AutoSize = False
          Caption = 'Size (in pixel)'
          Transparent = True
        end
        object WidthLabel: TLabel
          Left = 94
          Top = 15
          Width = 80
          Height = 13
          AutoSize = False
          Caption = 'Width (in pixel)'
          Transparent = True
        end
        object HeightLabel: TLabel
          Left = 181
          Top = 15
          Width = 80
          Height = 13
          AutoSize = False
          Caption = 'Height (in pixel)'
          Transparent = True
        end
        object OpacityLabel: TLabel
          Left = 268
          Top = 15
          Width = 80
          Height = 13
          AutoSize = False
          Caption = 'Opacity (255-0)'
          Transparent = True
        end
        object FixedColorLabel: TLabel
          Left = 354
          Top = 15
          Width = 63
          Height = 13
          AutoSize = False
          Caption = 'Fixed Color'
          Transparent = True
        end
        object SizeSpinEdit: TSpinEdit
          Left = 8
          Top = 30
          Width = 81
          Height = 22
          Hint = 'Decimal value'
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = SizeSpinEditChange
        end
        object StoreAsTextCheckBox: TCheckBox
          Left = 505
          Top = 13
          Width = 85
          Height = 17
          Caption = 'StoreAsText'
          TabOrder = 5
          OnClick = StoreAsTextCheckBoxClick
        end
        object WidthSpinEdit: TSpinEdit
          Left = 94
          Top = 30
          Width = 81
          Height = 22
          Hint = 'Decimal value'
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = WidthSpinEditChange
        end
        object HeightSpinEdit: TSpinEdit
          Left = 181
          Top = 30
          Width = 81
          Height = 22
          Hint = 'Decimal value'
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = HeightSpinEditChange
        end
        object OpacitySpinEdit: TSpinEdit
          Left = 268
          Top = 30
          Width = 81
          Height = 22
          Hint = 'Decimal value'
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = OpacitySpinEditChange
        end
        object FixedColorComboBox: TColorBox
          Left = 354
          Top = 30
          Width = 136
          Height = 22
          DefaultColorColor = clDefault
          NoneColorColor = clNone
          Selected = clNone
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 4
          OnSelect = FixedColorComboBoxSelect
        end
        object GrayScaleCheckBox: TCheckBox
          Left = 505
          Top = 32
          Width = 85
          Height = 17
          Caption = 'GrayScale'
          TabOrder = 6
          OnClick = GrayScaleCheckBoxClick
        end
      end
      object ItemGroupBox: TGroupBox
        Left = 0
        Top = 62
        Width = 596
        Height = 121
        Align = alClient
        Caption = 'Properties of Selected Icon n.%d'
        TabOrder = 1
        DesignSize = (
          596
          121)
        object IconNameLabel: TLabel
          Left = 94
          Top = 19
          Width = 87
          Height = 13
          AutoSize = False
          Caption = 'IconName'
          Transparent = True
        end
        object Label1: TLabel
          Left = 94
          Top = 55
          Width = 63
          Height = 13
          AutoSize = False
          Caption = 'Fixed Color'
          Transparent = True
        end
        object IconPanel: TPanel
          Left = 10
          Top = 18
          Width = 78
          Height = 78
          BevelOuter = bvNone
          BorderWidth = 2
          BorderStyle = bsSingle
          Color = clWindow
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
          object IconImage: TSVGIconImage
            Left = 2
            Top = 2
            Width = 72
            Height = 72
            AutoSize = False
            Proportional = False
            Align = alClient
          end
        end
        object IconName: TEdit
          Left = 94
          Top = 34
          Width = 136
          Height = 21
          Hint = 'Icon Name'
          TabOrder = 1
          OnExit = IconNameExit
        end
        object SVGText: TMemo
          Left = 236
          Top = 10
          Width = 353
          Height = 106
          Hint = 'SVG Text'
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 4
          OnChange = SVGTextChange
        end
        object FixedColorItemComboBox: TColorBox
          Left = 94
          Top = 70
          Width = 136
          Height = 22
          NoneColorColor = clNone
          Selected = clDefault
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 2
          OnSelect = FixedColorItemComboBoxSelect
        end
        object GrayScaleItemCheckBox: TCheckBox
          Left = 94
          Top = 97
          Width = 85
          Height = 17
          Caption = 'GrayScale'
          TabOrder = 3
          OnClick = GrayScaleItemCheckBoxClick
        end
      end
    end
    object paButtons: TPanel
      Left = 596
      Top = 0
      Width = 83
      Height = 183
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object OKButton: TButton
        Left = 2
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = OkButtonClick
      end
      object CancelButton: TButton
        Left = 2
        Top = 34
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object HelpButton: TButton
        Left = 2
        Top = 94
        Width = 75
        Height = 25
        Caption = '&Help'
        TabOrder = 3
        OnClick = HelpButtonClick
      end
      object ApplyButton: TButton
        Left = 2
        Top = 68
        Width = 75
        Height = 25
        Caption = '&Apply'
        TabOrder = 2
        OnClick = ApplyButtonClick
      end
      object ReformatXMLButton: TButton
        Left = 2
        Top = 152
        Width = 75
        Height = 25
        Caption = 'Reformat &XML'
        TabOrder = 4
        OnClick = ReformatXMLButtonClick
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 493
    Width = 679
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object AddButton: TButton
      Left = 84
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Add...'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object DeleteButton: TButton
      Left = 246
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 3
      OnClick = DeleteButtonClick
    end
    object ClearAllButton: TButton
      Left = 327
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Clear all'
      Enabled = False
      TabOrder = 4
      OnClick = ClearAllButtonClick
    end
    object ExportButton: TButton
      Left = 408
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Export...'
      Enabled = False
      TabOrder = 5
      OnClick = ExportButtonClick
    end
    object ReplaceButton: TButton
      Left = 165
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Replace...'
      TabOrder = 2
      OnClick = ReplaceButtonClick
    end
    object NewButton: TButton
      Left = 2
      Top = 4
      Width = 75
      Height = 25
      Caption = '&New'
      TabOrder = 0
      OnClick = NewButtonClick
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 328
    Top = 96
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 392
    Top = 96
  end
end
