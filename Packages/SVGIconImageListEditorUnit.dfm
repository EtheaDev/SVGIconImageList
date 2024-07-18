object SVGIconImageListEditor: TSVGIconImageListEditor
  Left = 392
  Top = 450
  Caption = 'SVG Icon ImageList Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 618
  ClientWidth = 715
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object BottomSplitter: TSplitter
    Left = 0
    Top = 376
    Width = 715
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    MinSize = 200
    ExplicitTop = 398
    ExplicitWidth = 784
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 715
    Height = 376
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 715
      Height = 376
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageListGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 715
        Height = 66
        Align = alTop
        Caption = 'Properties of ImageList'
        TabOrder = 0
        object SizeLabel: TLabel
          Left = 8
          Top = 19
          Width = 80
          Height = 15
          AutoSize = False
          Caption = 'Size (in pixel)'
          Transparent = True
        end
        object WidthLabel: TLabel
          Left = 94
          Top = 19
          Width = 80
          Height = 15
          AutoSize = False
          Caption = 'Width (in pixel)'
          Transparent = True
        end
        object HeightLabel: TLabel
          Left = 181
          Top = 19
          Width = 84
          Height = 15
          AutoSize = False
          Caption = 'Height (in pixel)'
          Transparent = True
        end
        object SizeEdit: TEdit
          Left = 8
          Top = 36
          Width = 81
          Height = 23
          NumbersOnly = True
          TabOrder = 0
          OnExit = SizeEditChange
        end
        object WidthEdit: TEdit
          Left = 94
          Top = 36
          Width = 81
          Height = 23
          NumbersOnly = True
          TabOrder = 1
          OnExit = WidthEditChange
        end
        object HeightEdit: TEdit
          Left = 181
          Top = 36
          Width = 81
          Height = 23
          NumbersOnly = True
          TabOrder = 2
          OnExit = HeightEditChange
        end
      end
      object paImages: TPanel
        Left = 0
        Top = 66
        Width = 715
        Height = 310
        Align = alClient
        TabOrder = 1
        object CategorySplitter: TSplitter
          Left = 186
          Top = 1
          Width = 4
          Height = 308
          AutoSnap = False
          MinSize = 150
          ExplicitHeight = 136
        end
        object ImageListGroup: TGroupBox
          Left = 190
          Top = 1
          Width = 381
          Height = 308
          Align = alClient
          Caption = '%d Icons collection'
          TabOrder = 1
          object ImageView: TListView
            AlignWithMargins = True
            Left = 5
            Top = 23
            Width = 371
            Height = 280
            Margins.Top = 6
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
        object paButtons: TPanel
          Left = 571
          Top = 1
          Width = 143
          Height = 308
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object AddButton: TButton
            Left = 6
            Top = 17
            Width = 135
            Height = 25
            Caption = '&Add...'
            TabOrder = 0
            OnClick = AddButtonClick
          end
          object ReplaceButton: TButton
            Left = 4
            Top = 48
            Width = 135
            Height = 25
            Caption = '&Replace...'
            TabOrder = 1
            OnClick = ReplaceButtonClick
          end
          object ClearAllButton: TButton
            Left = 4
            Top = 107
            Width = 135
            Height = 25
            Caption = '&Clear all'
            Enabled = False
            TabOrder = 3
            OnClick = ClearAllButtonClick
          end
          object ExportButton: TButton
            Left = 6
            Top = 169
            Width = 135
            Height = 25
            Caption = 'Export &svg files...'
            Enabled = False
            TabOrder = 5
            OnClick = ExportButtonClick
          end
          object SetCategoriesButton: TButton
            Left = 4
            Top = 138
            Width = 135
            Height = 25
            Caption = '&Set Categories...'
            TabOrder = 4
            OnClick = SetCategoriesButtonClick
          end
          object DeleteAllButton: TButton
            Left = 4
            Top = 78
            Width = 135
            Height = 25
            Caption = 'D&elete'
            Enabled = False
            TabOrder = 2
            OnClick = DeleteAllButtonClick
          end
          object PngGroupBox: TGroupBox
            AlignWithMargins = True
            Left = 4
            Top = 196
            Width = 137
            Height = 110
            Caption = 'Png export'
            TabOrder = 6
            object PngHeightLabel: TLabel
              Left = 2
              Top = 51
              Width = 90
              Height = 15
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Height (in pixel)'
              Transparent = True
            end
            object PngWidthLabel: TLabel
              Left = 2
              Top = 24
              Width = 90
              Height = 15
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Width (in pixel)'
              Transparent = True
            end
            object PngWidthEdit: TEdit
              Left = 94
              Top = 21
              Width = 36
              Height = 23
              NumbersOnly = True
              TabOrder = 0
            end
            object PngHeightEdit: TEdit
              Left = 94
              Top = 49
              Width = 36
              Height = 23
              NumbersOnly = True
              TabOrder = 1
            end
            object ExportPngButton: TButton
              Left = 5
              Top = 77
              Width = 125
              Height = 25
              Caption = 'Export &png files...'
              Enabled = False
              TabOrder = 2
              OnClick = ExportPngButtonClick
            end
          end
        end
        object ImagesPanel: TPanel
          Left = 1
          Top = 1
          Width = 185
          Height = 308
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object CategoryGroupBox: TGroupBox
            Left = 0
            Top = 0
            Width = 185
            Height = 127
            Align = alClient
            Caption = 'Images/Categories'
            TabOrder = 0
            object CategoryListBox: TListBox
              AlignWithMargins = True
              Left = 5
              Top = 23
              Width = 175
              Height = 99
              Margins.Top = 6
              Align = alClient
              ItemHeight = 15
              TabOrder = 0
              OnClick = CategoryListBoxClick
            end
          end
          object PropertiesGroupBox: TGroupBox
            AlignWithMargins = True
            Left = 3
            Top = 130
            Width = 179
            Height = 175
            Align = alBottom
            Caption = 'Global properties'
            TabOrder = 1
            object FixedColorLabel: TLabel
              Left = 6
              Top = 19
              Width = 63
              Height = 15
              AutoSize = False
              Caption = 'Fixed Color'
              Transparent = True
            end
            object OpacityLabel: TLabel
              Left = 6
              Top = 127
              Width = 80
              Height = 15
              AutoSize = False
              Caption = 'Opacity (255-0)'
              Transparent = True
            end
            object AntiAliasColorLabel: TLabel
              Left = 6
              Top = 84
              Width = 99
              Height = 15
              AutoSize = False
              Caption = 'Antialias Color'
              Transparent = True
            end
            object FixedColorComboBox: TColorBox
              Left = 6
              Top = 36
              Width = 147
              Height = 22
              DefaultColorColor = clDefault
              NoneColorColor = clNone
              Selected = clNone
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 0
              OnSelect = FixedColorComboBoxSelect
            end
            object GrayScaleCheckBox: TCheckBox
              Left = 73
              Top = 149
              Width = 85
              Height = 17
              Caption = 'GrayScale'
              TabOrder = 4
              OnClick = GrayScaleCheckBoxClick
            end
            object OpacitySpinEdit: TSpinEdit
              Left = 5
              Top = 144
              Width = 64
              Height = 24
              MaxValue = 255
              MinValue = 0
              TabOrder = 3
              Value = 0
              OnChange = OpacitySpinEditChange
            end
            object AntialiasColorComboBox: TColorBox
              Left = 6
              Top = 101
              Width = 147
              Height = 22
              DefaultColorColor = clDefault
              NoneColorColor = clNone
              Selected = clNone
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 2
              OnSelect = AntialiasColorComboBoxSelect
            end
            object ApplyToRootOnlyCheckBox: TCheckBox
              Left = 6
              Top = 61
              Width = 147
              Height = 17
              Caption = 'Apply to Root only'
              TabOrder = 1
              OnClick = ApplyToRootOnlyCheckBoxClick
            end
          end
        end
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 580
    Width = 715
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      715
      38)
    object OKButton: TButton
      Left = 346
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object ApplyButton: TButton
      Left = 529
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      Left = 438
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 621
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object SVGErrorStaticText: TStaticText
      Left = 6
      Top = 3
      Width = 338
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 4
      Transparent = False
    end
  end
  object paIcon: TPanel
    Left = 0
    Top = 380
    Width = 715
    Height = 200
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object IconButtonsPanel: TPanel
      Left = 618
      Top = 0
      Width = 97
      Height = 200
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object NewButton: TButton
        Left = 6
        Top = 6
        Width = 85
        Height = 25
        Caption = '&New'
        TabOrder = 0
        OnClick = NewButtonClick
      end
      object ReformatXMLButton: TButton
        Left = 6
        Top = 161
        Width = 85
        Height = 25
        Caption = 'Reformat &XML'
        TabOrder = 2
        OnClick = ReformatXMLButtonClick
      end
      object DeleteButton: TButton
        Left = 6
        Top = 37
        Width = 85
        Height = 25
        Caption = '&Delete'
        TabOrder = 1
        OnClick = DeleteButtonClick
      end
    end
    object ItemGroupBox: TGroupBox
      Left = 0
      Top = 0
      Width = 618
      Height = 200
      Align = alClient
      Caption = 'Selected Icon'
      TabOrder = 0
      DesignSize = (
        618
        200)
      object IconNameLabel: TLabel
        Left = 53
        Top = 20
        Width = 140
        Height = 15
        AutoSize = False
        Caption = 'Name'
        Transparent = True
      end
      object IconFixedColorLabel: TLabel
        Left = 343
        Top = 20
        Width = 140
        Height = 15
        AutoSize = False
        Caption = 'Fixed Color'
        Transparent = True
      end
      object IconIndexLabel: TLabel
        Left = 8
        Top = 20
        Width = 38
        Height = 15
        AutoSize = False
        Caption = 'Index'
        Transparent = True
      end
      object CategoryLabel: TLabel
        Left = 198
        Top = 20
        Width = 140
        Height = 15
        AutoSize = False
        Caption = 'Category'
        Transparent = True
      end
      object IconPanel: TPanel
        Left = 7
        Top = 65
        Width = 124
        Height = 124
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        Color = clWindow
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
        object IconImage: TSVGIconImage
          AlignWithMargins = True
          Left = 2
          Top = 2
          Width = 118
          Height = 118
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          AutoSize = False
          Align = alClient
        end
      end
      object NameEdit: TEdit
        Left = 53
        Top = 37
        Width = 140
        Height = 23
        TabOrder = 1
        OnExit = NameEditExit
      end
      object SVGText: TMemo
        Left = 136
        Top = 65
        Width = 469
        Height = 124
        Hint = 'SVG Text content'
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 7
        OnChange = SVGTextChange
        OnEnter = SVGTextEnter
        OnExit = SVGTextExit
        OnKeyDown = SVGTextKeyDown
      end
      object FixedColorItemComboBox: TColorBox
        Left = 343
        Top = 37
        Width = 140
        Height = 22
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 3
        OnSelect = FixedColorItemComboBoxSelect
      end
      object GrayScaleItemCheckBox: TCheckBox
        Left = 487
        Top = 22
        Width = 88
        Height = 17
        Caption = 'GrayScale'
        TabOrder = 4
        OnClick = GrayScaleItemCheckBoxClick
      end
      object IconIndexEdit: TEdit
        Left = 8
        Top = 37
        Width = 40
        Height = 23
        Enabled = False
        TabOrder = 0
        OnExit = NameEditExit
      end
      object CategoryEdit: TEdit
        Left = 198
        Top = 37
        Width = 140
        Height = 23
        TabOrder = 2
        OnExit = CategoryEditExit
      end
      object ApplyToRootOnlyItemCheckBox: TCheckBox
        Left = 487
        Top = 40
        Width = 127
        Height = 17
        Caption = 'Apply to Root only'
        TabOrder = 5
        OnClick = ApplyToRootOnlyItemCheckBoxClick
      end
    end
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 392
    Top = 96
  end
end
