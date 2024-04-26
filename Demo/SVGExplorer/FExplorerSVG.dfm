object fmExplorerSVG: TfmExplorerSVG
  Left = 0
  Top = 0
  ClientHeight = 509
  ClientWidth = 914
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object spVertical: TSplitter
    Left = 185
    Top = 0
    Width = 4
    Height = 509
    AutoSnap = False
    MinSize = 120
    ExplicitHeight = 393
  end
  object spRight: TSplitter
    Left = 774
    Top = 0
    Width = 4
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 90
    ExplicitLeft = 701
    ExplicitHeight = 416
  end
  object paDir: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 509
    Align = alLeft
    TabOrder = 0
    object DirSelection: TDirectoryListBox
      Left = 1
      Top = 41
      Width = 183
      Height = 388
      Align = alClient
      TabOrder = 0
      OnChange = DirSelectionChange
    end
    object DrivePanel: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 40
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object DriveComboBox: TDriveComboBox
        Left = 1
        Top = 10
        Width = 178
        Height = 19
        DirList = DirSelection
        TabOrder = 0
      end
    end
    object PerformanceStatusBar: TStatusBar
      Left = 1
      Top = 489
      Width = 183
      Height = 19
      Panels = <>
      SimplePanel = True
    end
    object TrackBarPanel: TPanel
      Left = 1
      Top = 429
      Width = 183
      Height = 60
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object Label1: TLabel
        Left = 8
        Top = 5
        Width = 52
        Height = 13
        Caption = 'Icons size:'
      end
      object TrackBar: TTrackBar
        Left = 0
        Top = 21
        Width = 183
        Height = 39
        Align = alBottom
        Max = 128
        Min = 12
        Frequency = 8
        Position = 32
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = TrackBarChange
      end
    end
  end
  object PaList: TPanel
    Left = 189
    Top = 0
    Width = 585
    Height = 509
    Align = alClient
    TabOrder = 1
    object ImageListLabel: TLabel
      Left = 1
      Top = 42
      Width = 583
      Height = 16
      Align = alTop
      Alignment = taCenter
      Caption = 'SVG image'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 63
    end
    object spBottom: TSplitter
      Left = 1
      Top = 386
      Width = 583
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      MinSize = 100
      ExplicitTop = 58
      ExplicitWidth = 238
    end
    object paRicerca: TPanel
      Left = 1
      Top = 1
      Width = 583
      Height = 41
      Align = alTop
      TabOrder = 0
      DesignSize = (
        583
        41)
      object SearchBox: TSearchBox
        Left = 10
        Top = 10
        Width = 566
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        TextHint = 'Insert filter with wildcards to search icons by name...'
        OnInvokeSearch = SearchBoxInvokeSearch
      end
    end
    object ImageView: TListView
      Left = 1
      Top = 58
      Width = 583
      Height = 328
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      LargeImages = SVGIconImageList
      PopupMenu = PopupMenu
      SmallImages = SVGIconImageList
      TabOrder = 1
      OnKeyDown = ImageViewKeyDown
      OnSelectItem = ImageViewSelectItem
    end
    object StatusBar: TStatusBar
      Left = 1
      Top = 489
      Width = 583
      Height = 19
      Panels = <>
      SimplePanel = True
    end
    object paSVGText: TPanel
      Left = 1
      Top = 389
      Width = 583
      Height = 100
      Hint = 'SVG Text content'
      Align = alBottom
      TabOrder = 3
      object SVGMemo: TMemo
        Left = 1
        Top = 1
        Width = 581
        Height = 98
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object paPreview: TPanel
    Left = 778
    Top = 0
    Width = 136
    Height = 509
    Align = alRight
    TabOrder = 2
    OnResize = paPreviewResize
    DesignSize = (
      136
      509)
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 1
      Width = 134
      Height = 88
      Cursor = crSizeAll
      Hint = 'Left click to enlarge. Right click to shrink'
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      AutoSize = False
      ImageList = SVGIconImageList
      Align = alTop
      OnMouseDown = SVGIconImageMouseDown
    end
    object btDelete: TButton
      Left = 5
      Top = 478
      Width = 124
      Height = 25
      Action = DeleteAction
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
    end
    object BtRename: TButton
      Left = 5
      Top = 447
      Width = 124
      Height = 25
      Action = RenameAction
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
    end
    object ShowTextCheckBox: TCheckBox
      Left = 6
      Top = 420
      Width = 71
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show Text'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = ShowTextCheckBoxClick
    end
    object grpFactory: TRadioGroup
      Left = 1
      Top = 89
      Width = 134
      Height = 105
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alTop
      Caption = 'SVG Factory'
      TabOrder = 3
      OnClick = grpFactoryClick
    end
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 48
    SVGIconItems = <>
    Scaled = True
    Left = 264
    Top = 157
  end
  object PopupMenu: TPopupMenu
    Left = 480
    Top = 160
    object Rename1: TMenuItem
      Action = RenameAction
    end
    object Delete1: TMenuItem
      Action = DeleteAction
    end
  end
  object ActionList: TActionList
    Left = 480
    Top = 240
    object DeleteAction: TAction
      Caption = 'Delete...'
      OnExecute = DeleteActionExecute
      OnUpdate = ActionUpdate
    end
    object RenameAction: TAction
      Caption = 'Rename...'
      OnExecute = RenameActionExecute
      OnUpdate = ActionUpdate
    end
  end
end
