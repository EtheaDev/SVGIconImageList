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
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TopSplitter: TSplitter
    Left = 0
    Top = 170
    Width = 679
    Height = 4
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    MinSize = 170
    ExplicitLeft = 4
    ExplicitTop = 23
    ExplicitWidth = 675
  end
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 174
    Width = 679
    Height = 319
    Align = alClient
    Caption = ' Icons of Imagelist'
    TabOrder = 1
    ExplicitTop = 169
    ExplicitHeight = 324
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 675
      Height = 302
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
      OnSelectItem = ImageViewSelectItem
      ExplicitHeight = 307
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 679
    Height = 170
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 596
      Height = 170
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 183
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
          Left = 368
          Top = 32
          Width = 85
          Height = 17
          Caption = 'StoreAsText'
          TabOrder = 4
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
      end
      object ItemGroupBox: TGroupBox
        Left = 0
        Top = 62
        Width = 596
        Height = 108
        Align = alClient
        Caption = 'Properties of Selected Icon n.%d'
        TabOrder = 1
        ExplicitHeight = 104
        DesignSize = (
          596
          108)
        object IconNameLabel: TLabel
          Left = 94
          Top = 19
          Width = 87
          Height = 13
          AutoSize = False
          Caption = 'IconName'
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
            Center = True
            Proportional = False
            Stretch = True
            Opacity = 255
            Scale = 1.000000000000000000
            ImageIndex = -1
            Align = alClient
            Data = {00000000}
          end
        end
        object IconName: TEdit
          Left = 94
          Top = 34
          Width = 158
          Height = 21
          Hint = 'Icon Name'
          TabOrder = 1
          OnExit = IconNameExit
        end
        object SVGText: TMemo
          Left = 258
          Top = 10
          Width = 332
          Height = 93
          Hint = 'SVG Text'
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssBoth
          TabOrder = 2
          OnExit = SVGTextExit
          ExplicitHeight = 89
        end
      end
    end
    object paButtons: TPanel
      Left = 596
      Top = 0
      Width = 83
      Height = 170
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 169
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
    Left = 464
    Top = 16
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 528
    Top = 16
  end
end
