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
        Caption = 'Size of ImageList'
        TabOrder = 0
        object SizeLabel: TLabel
          Left = 8
          Top = 19
          Width = 60
          Height = 15
          AutoSize = False
          Caption = 'Size'
          Transparent = True
        end
        object WidthLabel: TLabel
          Left = 72
          Top = 19
          Width = 60
          Height = 15
          AutoSize = False
          Caption = 'Width'
          Transparent = True
        end
        object HeightLabel: TLabel
          Left = 136
          Top = 19
          Width = 60
          Height = 15
          AutoSize = False
          Caption = 'Height'
          Transparent = True
        end
        object SizeEdit: TSpinEdit
          Left = 8
          Top = 36
          Width = 60
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnExit = SizeEditChange
        end
        object WidthEdit: TSpinEdit
          Left = 72
          Top = 36
          Width = 60
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnExit = WidthEditChange
        end
        object HeightEdit: TSpinEdit
          Left = 136
          Top = 36
          Width = 60
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
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
          object DiskBevel: TBevel
            AlignWithMargins = True
            Left = 3
            Top = 70
            Width = 137
            Height = 2
            Align = alTop
          end
          object WebBevel: TBevel
            AlignWithMargins = True
            Left = 3
            Top = 145
            Width = 137
            Height = 2
            Align = alTop
          end
          object AddButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 8
            Width = 137
            Height = 25
            Margins.Top = 8
            Align = alTop
            Caption = '&Add from disk...'
            TabOrder = 0
            OnClick = AddButtonClick
          end
          object ReplaceButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 39
            Width = 137
            Height = 25
            Align = alTop
            Caption = '&Replace from Disk...'
            TabOrder = 1
            OnClick = ReplaceButtonClick
          end
          object ClearAllButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 184
            Width = 137
            Height = 25
            Align = alTop
            Caption = '&Clear all'
            Enabled = False
            TabOrder = 3
            OnClick = ClearAllButtonClick
          end
          object ExportButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 246
            Width = 137
            Height = 25
            Align = alTop
            Caption = 'Export &svg files...'
            Enabled = False
            TabOrder = 5
            OnClick = ExportButtonClick
          end
          object SetCategoriesButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 215
            Width = 137
            Height = 25
            Align = alTop
            Caption = '&Set Categories...'
            TabOrder = 4
            OnClick = SetCategoriesButtonClick
          end
          object DeleteAllButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 153
            Width = 137
            Height = 25
            Align = alTop
            Caption = 'D&elete'
            Enabled = False
            TabOrder = 2
            OnClick = DeleteAllButtonClick
          end
          object ExportPngButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 277
            Width = 137
            Height = 25
            Align = alTop
            Caption = 'Export &png files...'
            Enabled = False
            TabOrder = 6
            OnClick = ExportPngButtonClick
          end
          object AddWebButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 83
            Width = 137
            Height = 25
            Margins.Top = 8
            Align = alTop
            Caption = '&Add from WEB...'
            TabOrder = 7
            OnClick = AddWebButtonClick
          end
          object ReplaceWebButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 114
            Width = 137
            Height = 25
            Align = alTop
            Caption = '&Replace from WEB...'
            TabOrder = 8
            OnClick = ReplaceWebButtonClick
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
    object OKButton: TButton
      AlignWithMargins = True
      Left = 351
      Top = 6
      Width = 85
      Height = 26
      Margins.Top = 6
      Margins.Bottom = 6
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object ApplyButton: TButton
      AlignWithMargins = True
      Left = 533
      Top = 6
      Width = 85
      Height = 26
      Margins.Top = 6
      Margins.Bottom = 6
      Align = alRight
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      AlignWithMargins = True
      Left = 442
      Top = 6
      Width = 85
      Height = 26
      Margins.Top = 6
      Margins.Bottom = 6
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      AlignWithMargins = True
      Left = 624
      Top = 6
      Width = 85
      Height = 26
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alRight
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object SVGErrorStaticText: TStaticText
      AlignWithMargins = True
      Left = 3
      Top = 4
      Width = 342
      Height = 30
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alClient
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
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 91
        Height = 25
        Align = alTop
        Caption = '&New'
        TabOrder = 0
        OnClick = NewButtonClick
      end
      object ReformatXMLButton: TButton
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 91
        Height = 25
        Align = alTop
        Caption = 'Reformat &XML'
        TabOrder = 2
        OnClick = ReformatXMLButtonClick
      end
      object DeleteButton: TButton
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 91
        Height = 25
        Align = alTop
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
        Width = 437
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
