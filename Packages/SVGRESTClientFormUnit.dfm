object SVGRESTClientSearchForm: TSVGRESTClientSearchForm
  Left = 0
  Top = 0
  Caption = 'SVG REST Client Search %s - Copyright Ethea S.r.l.'
  ClientHeight = 530
  ClientWidth = 720
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object SearchGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 720
    Height = 55
    Align = alTop
    Caption = 'Search for Icons from the Web'
    TabOrder = 0
    object MaxLabel: TLabel
      AlignWithMargins = True
      Left = 480
      Top = 29
      Width = 60
      Height = 21
      Margins.Top = 12
      Align = alRight
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Max icons:'
      Transparent = True
    end
    object SearchEdit: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 25
      Width = 469
      Height = 24
      Hint = 'Insert Icon Name to search'
      Margins.Top = 8
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
      TextHint = 'Icon name'
      OnChange = SearchEditChange
      OnEnter = SearchEditEnter
      OnExit = SearchEditExit
      OnKeyDown = SearchEditKeyDown
    end
    object MaxIconsPanel: TPanel
      AlignWithMargins = True
      Left = 546
      Top = 25
      Width = 48
      Height = 24
      Margins.Top = 8
      Margins.Bottom = 4
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object MaxIconsEdit: TSpinEdit
        Left = 0
        Top = 0
        Width = 48
        Height = 24
        MaxValue = 255
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
    end
    object SearchButton: TButton
      AlignWithMargins = True
      Left = 600
      Top = 20
      Width = 115
      Height = 30
      Align = alRight
      Caption = 'Search...'
      Enabled = False
      TabOrder = 2
      OnClick = SearchButtonClick
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 492
    Width = 720
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object OKButton: TButton
      AlignWithMargins = True
      Left = 451
      Top = 6
      Width = 85
      Height = 26
      Margins.Left = 2
      Margins.Top = 6
      Margins.Right = 2
      Margins.Bottom = 6
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = OKButtonClick
    end
    object CancelButton: TButton
      AlignWithMargins = True
      Left = 540
      Top = 6
      Width = 85
      Height = 26
      Margins.Left = 2
      Margins.Top = 6
      Margins.Right = 2
      Margins.Bottom = 6
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object HelpButton: TButton
      AlignWithMargins = True
      Left = 629
      Top = 6
      Width = 85
      Height = 26
      Margins.Left = 2
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alRight
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object TrackBar: TTrackBar
      AlignWithMargins = True
      Left = 183
      Top = 3
      Width = 263
      Height = 32
      Hint = 'Icon Preview Size'
      Align = alClient
      Max = 128
      Min = 12
      Frequency = 8
      Position = 48
      PositionToolTip = ptBottom
      TabOrder = 0
      OnChange = TrackBarChange
    end
    object RemovePrefixCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 8
      Top = 3
      Width = 169
      Height = 32
      Hint = 'Remove Prefix from Name during selection'
      Margins.Left = 8
      Align = alLeft
      Caption = 'Remove Prefix from Name'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 55
    Width = 720
    Height = 437
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterView: TSplitter
      Left = 524
      Top = 0
      Width = 4
      Height = 437
      Align = alRight
    end
    object AvailGroupBox: TGroupBox
      Left = 0
      Top = 0
      Width = 524
      Height = 437
      Align = alClient
      Caption = 'Available Icons: double click to select or drag and drop'
      TabOrder = 0
      object SearchView: TListView
        AlignWithMargins = True
        Left = 5
        Top = 21
        Width = 514
        Height = 411
        Margins.Top = 4
        Align = alClient
        Columns = <>
        DragMode = dmAutomatic
        FullDrag = True
        HideSelection = False
        IconOptions.AutoArrange = True
        MultiSelect = True
        ReadOnly = True
        TabOrder = 0
        OnDblClick = SearchViewDblClick
        OnDragDrop = SelectedViewDragDrop
      end
    end
    object SelectedGroupBox: TGroupBox
      Left = 528
      Top = 0
      Width = 192
      Height = 437
      Align = alRight
      Caption = 'Selected Icons'
      TabOrder = 1
      object SelectedView: TListView
        AlignWithMargins = True
        Left = 5
        Top = 23
        Width = 182
        Height = 409
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
        OnDragDrop = SelectedViewDragDrop
        OnDragOver = SelectedViewDragOver
      end
    end
  end
end
