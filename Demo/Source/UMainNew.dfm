object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 547
  ClientWidth = 709
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter: TSplitter
    Left = 626
    Top = 38
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 80
    ExplicitLeft = 9
    ExplicitTop = 9
    ExplicitHeight = 427
  end
  object Panel1: TPanel
    Left = 0
    Top = 38
    Width = 202
    Height = 509
    Align = alLeft
    TabOrder = 0
    object SelectThemeRadioGroup: TRadioGroup
      Left = 1
      Top = 1
      Width = 200
      Height = 210
      Align = alClient
      Caption = 'Select Theme/Color'
      TabOrder = 0
      OnClick = SelectThemeRadioGroupClick
    end
    object LoadGroupBox: TGroupBox
      Left = 1
      Top = 211
      Width = 200
      Height = 59
      Align = alBottom
      Caption = 'Load SVG from disk'
      TabOrder = 1
      object BuildFromFilesButton: TButton
        Left = 3
        Top = 23
        Width = 189
        Height = 30
        Caption = 'Build from SVG files...'
        TabOrder = 0
        OnClick = BuildFromFilesButtonClick
      end
    end
    object SliderPanel: TPanel
      Left = 1
      Top = 373
      Width = 200
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object IconSizeLabel: TLabel
        Left = 8
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Icons size:'
      end
      object TrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 200
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
    object ButtonsPanel: TPanel
      Left = 1
      Top = 270
      Width = 200
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object ClearButton: TButton
        Left = 5
        Top = 5
        Width = 76
        Height = 30
        Caption = 'Clear Icons'
        TabOrder = 0
        OnClick = ClearButtonClick
      end
      object ShowImageEditorButton: TButton
        Left = 86
        Top = 5
        Width = 106
        Height = 30
        Caption = 'Show Image Editor'
        TabOrder = 1
        OnClick = ShowImageEditorButtonClick
      end
    end
    object ColorGroupBox: TGroupBox
      Left = 1
      Top = 435
      Width = 200
      Height = 73
      Align = alBottom
      Caption = 'Fixed color'
      TabOrder = 4
      object FixedColorComboBox: TColorBox
        Left = 10
        Top = 19
        Width = 178
        Height = 22
        DefaultColorColor = clDefault
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 0
        OnSelect = FixedColorComboBoxSelect
      end
      object GrayScaleCheckBox: TCheckBox
        Left = 124
        Top = 47
        Width = 71
        Height = 17
        Caption = 'GrayScale'
        TabOrder = 2
        OnClick = GrayScaleCheckBoxClick
      end
      object ApplyToRootOnlyCheckBox: TCheckBox
        Left = 10
        Top = 47
        Width = 110
        Height = 17
        Caption = 'ApplyToRootOnly'
        TabOrder = 1
        OnClick = ApplyToRootOnlyCheckBoxClick
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 311
      Width = 200
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object IconOpacityLabel: TLabel
        Left = 8
        Top = 3
        Width = 68
        Height = 13
        Caption = 'Icons opacity:'
      end
      object OpacityTrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 200
        Height = 39
        Align = alBottom
        Max = 255
        Min = 1
        Frequency = 8
        Position = 255
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = TrackBarChange
      end
    end
  end
  object TopToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 709
    Height = 38
    AutoSize = True
    ButtonHeight = 38
    ButtonWidth = 39
    Images = VirtualImageList
    TabOrder = 1
    Transparent = False
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = DisabledAction
      ImageIndex = 0
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      Action = DeleteIconAction
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 78
      Top = 0
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 117
      Top = 0
      ImageIndex = 3
    end
    object ToolButton5: TToolButton
      Left = 156
      Top = 0
      ImageIndex = 4
    end
    object ToolButton6: TToolButton
      Left = 195
      Top = 0
      Action = ChangeIconAction
      ImageIndex = 5
    end
    object ToolButton7: TToolButton
      Left = 234
      Top = 0
      Caption = 'Change Color'
      Enabled = False
      ImageIndex = 6
      OnClick = ChangeColorActionExecute
    end
  end
  object paButtons: TPanel
    Left = 629
    Top = 38
    Width = 80
    Height = 509
    Align = alRight
    TabOrder = 2
    OnResize = paButtonsResize
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 428
      Width = 78
      Height = 80
      Hint = 'Click left - right mouse button to change icon into SVGIconImage'
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      AutoSize = False
      ImageList = VirtualImageList
      ImageIndex = 100
      Align = alBottom
      OnDblClick = SVGIconImageDblClick
      OnMouseDown = SVGIconImageMouseDown
    end
    object DeleteButton: TButton
      Left = 3
      Top = 5
      Width = 73
      Height = 60
      Action = DeleteIconAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 0
    end
    object ChangeIconButton: TButton
      Left = 3
      Top = 71
      Width = 73
      Height = 60
      Action = ChangeIconAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 1
    end
    object NewFormButton: TButton
      Left = 3
      Top = 137
      Width = 73
      Height = 60
      Action = NewFormAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 2
    end
  end
  object ClientPanel: TPanel
    Left = 202
    Top = 38
    Width = 424
    Height = 509
    Align = alClient
    TabOrder = 3
    object ImageListLabel: TLabel
      Left = 1
      Top = 209
      Width = 422
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Image List Icons Preview'
      ExplicitWidth = 119
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 422
      Height = 208
      Align = alTop
      Images = VirtualImageList
      Indent = 35
      TabOrder = 0
      Items.NodeData = {
        0303000000240000000100000001000000FFFFFFFFFFFFFFFF00000000000000
        000100000001036F006E0065002C0000000400000004000000FFFFFFFFFFFFFF
        FF00000000000000000000000001076F006E0065002D006F006E006500240000
        000200000002000000FFFFFFFFFFFFFFFF000000000000000002000000010374
        0077006F002C0000000500000005000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107740077006F0020006F006E0065002C00000006000000070000
        0000000000FFFFFFFF0000000000000000000000000107740077006F00200074
        0077006F00280000000300000003000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010574006800720065006500}
    end
    object ImageView: TListView
      Left = 1
      Top = 222
      Width = 422
      Height = 286
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      Items.ItemData = {
        05CC0000000500000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        00034D0061006E0001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000
        0557006F006D0061006E0002000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
        00000008430061006C0065006E0064006100720003000000FFFFFFFFFFFFFFFF
        00000000FFFFFFFF000000000B49006E0066006F0072006D006100740069006F
        006E0004000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000A43006100
        6C00630075006C00610074006F007200}
      LargeImages = VirtualImageList
      SmallImages = VirtualImageList
      TabOrder = 1
      OnSelectItem = ImageViewSelectItem
    end
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 248
    Top = 424
    object ChangeIconAction: TAction
      Category = 'Edit'
      Caption = 'Change icon'
      ImageIndex = 255
      OnExecute = ChangeIconActionExecute
    end
    object DeleteIconAction: TAction
      Category = 'Edit'
      Caption = 'Delete Icon'
      ImageIndex = 39
      OnExecute = DeleteIconActionExecute
    end
    object DisabledAction: TAction
      Category = 'Edit'
      Caption = 'Disabled'
      Enabled = False
    end
    object NewFormAction: TAction
      Category = 'View'
      Caption = 'New Form'
      ImageIndex = 107
      OnExecute = NewFormActionExecute
    end
  end
  object ColorDialog: TColorDialog
    Left = 472
    Top = 136
  end
  object VirtualImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Business\businessman'
        Name = 'businessman'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Business\businesswoman'
        Name = 'businesswoman'
      end
      item
        CollectionIndex = 2
        CollectionName = 'calendar'
        Name = 'calendar'
      end
      item
        CollectionIndex = 3
        CollectionName = 'about'
        Name = 'about'
      end
      item
        CollectionIndex = 4
        CollectionName = 'calculator'
        Name = 'calculator'
      end
      item
        CollectionIndex = 5
        CollectionName = 'cell_phone'
        Name = 'cell_phone'
      end
      item
        CollectionIndex = 6
        CollectionName = 'contacts'
        Name = 'contacts'
      end
      item
        CollectionIndex = 7
        CollectionName = 'advertising'
        Name = 'advertising'
      end
      item
        CollectionIndex = 8
        CollectionName = 'alphabetical_sorting_az'
        Name = 'alphabetical_sorting_az'
      end
      item
        CollectionIndex = 9
        CollectionName = 'alphabetical_sorting_za'
        Name = 'alphabetical_sorting_za'
      end
      item
        CollectionIndex = 10
        CollectionName = 'android_os'
        Name = 'android_os'
      end
      item
        CollectionIndex = 11
        CollectionName = 'answers'
        Name = 'answers'
      end
      item
        CollectionIndex = 12
        CollectionName = 'approval'
        Name = 'approval'
      end
      item
        CollectionIndex = 13
        CollectionName = 'approve'
        Name = 'approve'
      end
      item
        CollectionIndex = 14
        CollectionName = 'area_chart'
        Name = 'area_chart'
      end
      item
        CollectionIndex = 15
        CollectionName = 'assistant'
        Name = 'assistant'
      end
      item
        CollectionIndex = 16
        CollectionName = 'audio_file'
        Name = 'audio_file'
      end
      item
        CollectionIndex = 17
        CollectionName = 'automatic'
        Name = 'automatic'
      end
      item
        CollectionIndex = 18
        CollectionName = 'automotive'
        Name = 'automotive'
      end
      item
        CollectionIndex = 19
        CollectionName = 'bad_decision'
        Name = 'bad_decision'
      end
      item
        CollectionIndex = 20
        CollectionName = 'bar_chart'
        Name = 'bar_chart'
      end
      item
        CollectionIndex = 21
        CollectionName = 'bearish'
        Name = 'bearish'
      end
      item
        CollectionIndex = 22
        CollectionName = 'binoculars'
        Name = 'binoculars'
      end
      item
        CollectionIndex = 23
        CollectionName = 'biohazard'
        Name = 'biohazard'
      end
      item
        CollectionIndex = 24
        CollectionName = 'biomass'
        Name = 'biomass'
      end
      item
        CollectionIndex = 25
        CollectionName = 'biotech'
        Name = 'biotech'
      end
      item
        CollectionIndex = 26
        CollectionName = 'bookmark'
        Name = 'bookmark'
      end
      item
        CollectionIndex = 27
        CollectionName = 'briefcase'
        Name = 'briefcase'
      end
      item
        CollectionIndex = 28
        CollectionName = 'bullish'
        Name = 'bullish'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Business\business'
        Name = 'business'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Business\business_contact'
        Name = 'business_contact'
      end
      item
        CollectionIndex = 31
        CollectionName = 'butting_in'
        Name = 'butting_in'
      end
      item
        CollectionIndex = 32
        CollectionName = 'cable_release'
        Name = 'cable_release'
      end
      item
        CollectionIndex = 33
        CollectionName = 'call_transfer'
        Name = 'call_transfer'
      end
      item
        CollectionIndex = 34
        CollectionName = 'callback'
        Name = 'callback'
      end
      item
        CollectionIndex = 35
        CollectionName = 'camcorder'
        Name = 'camcorder'
      end
      item
        CollectionIndex = 36
        CollectionName = 'camcorder_pro'
        Name = 'camcorder_pro'
      end
      item
        CollectionIndex = 37
        CollectionName = 'camera'
        Name = 'camera'
      end
      item
        CollectionIndex = 38
        CollectionName = 'camera_addon'
        Name = 'camera_addon'
      end
      item
        CollectionIndex = 39
        CollectionName = 'cancel'
        Name = 'cancel'
      end
      item
        CollectionIndex = 40
        CollectionName = 'candle_sticks'
        Name = 'candle_sticks'
      end
      item
        CollectionIndex = 41
        CollectionName = 'capacitor'
        Name = 'capacitor'
      end
      item
        CollectionIndex = 42
        CollectionName = 'cd_logo'
        Name = 'cd_logo'
      end
      item
        CollectionIndex = 43
        CollectionName = 'charge_battery'
        Name = 'charge_battery'
      end
      item
        CollectionIndex = 44
        CollectionName = 'checkmark'
        Name = 'checkmark'
      end
      item
        CollectionIndex = 45
        CollectionName = 'circuit'
        Name = 'circuit'
      end
      item
        CollectionIndex = 46
        CollectionName = 'clapperboard'
        Name = 'clapperboard'
      end
      item
        CollectionIndex = 47
        CollectionName = 'clear_filters'
        Name = 'clear_filters'
      end
      item
        CollectionIndex = 48
        CollectionName = 'close_up_mode'
        Name = 'close_up_mode'
      end
      item
        CollectionIndex = 49
        CollectionName = 'cloth'
        Name = 'cloth'
      end
      item
        CollectionIndex = 50
        CollectionName = 'collaboration'
        Name = 'collaboration'
      end
      item
        CollectionIndex = 51
        CollectionName = 'collect'
        Name = 'collect'
      end
      item
        CollectionIndex = 52
        CollectionName = 'combo_chart'
        Name = 'combo_chart'
      end
      item
        CollectionIndex = 53
        CollectionName = 'command_line'
        Name = 'command_line'
      end
      item
        CollectionIndex = 54
        CollectionName = 'comments'
        Name = 'comments'
      end
      item
        CollectionIndex = 55
        CollectionName = 'compact_camera'
        Name = 'compact_camera'
      end
      item
        CollectionIndex = 56
        CollectionName = 'conference_call'
        Name = 'conference_call'
      end
      item
        CollectionIndex = 57
        CollectionName = 'crystal_oscillator'
        Name = 'crystal_oscillator'
      end
      item
        CollectionIndex = 58
        CollectionName = 'currency_exchange'
        Name = 'currency_exchange'
      end
      item
        CollectionIndex = 59
        CollectionName = 'cursor'
        Name = 'cursor'
      end
      item
        CollectionIndex = 60
        CollectionName = 'customer_support'
        Name = 'customer_support'
      end
      item
        CollectionIndex = 61
        CollectionName = 'dam'
        Name = 'dam'
      end
      item
        CollectionIndex = 62
        CollectionName = 'data_sheet'
        Name = 'data_sheet'
      end
      item
        CollectionIndex = 63
        CollectionName = 'debt'
        Name = 'debt'
      end
      item
        CollectionIndex = 64
        CollectionName = 'department'
        Name = 'department'
      end
      item
        CollectionIndex = 65
        CollectionName = 'deployment'
        Name = 'deployment'
      end
      item
        CollectionIndex = 66
        CollectionName = 'diploma_1'
        Name = 'diploma_1'
      end
      item
        CollectionIndex = 67
        CollectionName = 'diploma_2'
        Name = 'diploma_2'
      end
      item
        CollectionIndex = 68
        CollectionName = 'display'
        Name = 'display'
      end
      item
        CollectionIndex = 69
        CollectionName = 'document'
        Name = 'document'
      end
      item
        CollectionIndex = 70
        CollectionName = 'donate'
        Name = 'donate'
      end
      item
        CollectionIndex = 71
        CollectionName = 'doughnut_chart'
        Name = 'doughnut_chart'
      end
      item
        CollectionIndex = 72
        CollectionName = 'down'
        Name = 'down'
      end
      item
        CollectionIndex = 73
        CollectionName = 'down_left'
        Name = 'down_left'
      end
      item
        CollectionIndex = 74
        CollectionName = 'down_right'
        Name = 'down_right'
      end
      item
        CollectionIndex = 75
        CollectionName = 'download'
        Name = 'download'
      end
      item
        CollectionIndex = 76
        CollectionName = 'dribbble'
        Name = 'dribbble'
      end
      item
        CollectionIndex = 77
        CollectionName = 'dvd_logo'
        Name = 'dvd_logo'
      end
      item
        CollectionIndex = 78
        CollectionName = 'electrical_sensor'
        Name = 'electrical_sensor'
      end
      item
        CollectionIndex = 79
        CollectionName = 'electrical_threshold'
        Name = 'electrical_threshold'
      end
      item
        CollectionIndex = 80
        CollectionName = 'electricity'
        Name = 'electricity'
      end
      item
        CollectionIndex = 81
        CollectionName = 'electro_devices'
        Name = 'electro_devices'
      end
      item
        CollectionIndex = 82
        CollectionName = 'electronics'
        Name = 'electronics'
      end
      item
        CollectionIndex = 83
        CollectionName = 'empty_battery'
        Name = 'empty_battery'
      end
      item
        CollectionIndex = 84
        CollectionName = 'empty_filter'
        Name = 'empty_filter'
      end
      item
        CollectionIndex = 85
        CollectionName = 'empty_trash'
        Name = 'empty_trash'
      end
      item
        CollectionIndex = 86
        CollectionName = 'end_call'
        Name = 'end_call'
      end
      item
        CollectionIndex = 87
        CollectionName = 'engineering'
        Name = 'engineering'
      end
      item
        CollectionIndex = 88
        CollectionName = 'entering_heaven_alive'
        Name = 'entering_heaven_alive'
      end
      item
        CollectionIndex = 89
        CollectionName = 'expand'
        Name = 'expand'
      end
      item
        CollectionIndex = 90
        CollectionName = 'export'
        Name = 'export'
      end
      item
        CollectionIndex = 91
        CollectionName = 'external'
        Name = 'external'
      end
      item
        CollectionIndex = 92
        CollectionName = 'factory'
        Name = 'factory'
      end
      item
        CollectionIndex = 93
        CollectionName = 'factory_breakdown'
        Name = 'factory_breakdown'
      end
      item
        CollectionIndex = 94
        CollectionName = 'faq'
        Name = 'faq'
      end
      item
        CollectionIndex = 95
        CollectionName = 'feed_in'
        Name = 'feed_in'
      end
      item
        CollectionIndex = 96
        CollectionName = 'feedback'
        Name = 'feedback'
      end
      item
        CollectionIndex = 97
        CollectionName = 'file'
        Name = 'file'
      end
      item
        CollectionIndex = 98
        CollectionName = 'filing_cabinet'
        Name = 'filing_cabinet'
      end
      item
        CollectionIndex = 99
        CollectionName = 'filled_filter'
        Name = 'filled_filter'
      end
      item
        CollectionIndex = 100
        CollectionName = 'Delphi_Product icon'
        Name = 'Delphi_Product icon'
      end
      item
        CollectionIndex = 101
        CollectionName = 'film'
        Name = 'film'
      end
      item
        CollectionIndex = 102
        CollectionName = 'film_reel'
        Name = 'film_reel'
      end
      item
        CollectionIndex = 103
        CollectionName = 'flash_auto'
        Name = 'flash_auto'
      end
      item
        CollectionIndex = 104
        CollectionName = 'flash_on'
        Name = 'flash_on'
      end
      item
        CollectionIndex = 105
        CollectionName = 'flow_chart'
        Name = 'flow_chart'
      end
      item
        CollectionIndex = 106
        CollectionName = 'folder'
        Name = 'folder'
      end
      item
        CollectionIndex = 107
        CollectionName = 'frame'
        Name = 'frame'
      end
      item
        CollectionIndex = 108
        CollectionName = 'full_battery'
        Name = 'full_battery'
      end
      item
        CollectionIndex = 109
        CollectionName = 'full_trash'
        Name = 'full_trash'
      end
      item
        CollectionIndex = 110
        CollectionName = 'gallery'
        Name = 'gallery'
      end
      item
        CollectionIndex = 111
        CollectionName = 'genealogy'
        Name = 'genealogy'
      end
      item
        CollectionIndex = 112
        CollectionName = 'generic_sorting_asc'
        Name = 'generic_sorting_asc'
      end
      item
        CollectionIndex = 113
        CollectionName = 'generic_sorting_desc'
        Name = 'generic_sorting_desc'
      end
      item
        CollectionIndex = 114
        CollectionName = 'globe'
        Name = 'globe'
      end
      item
        CollectionIndex = 115
        CollectionName = 'good_decision'
        Name = 'good_decision'
      end
      item
        CollectionIndex = 116
        CollectionName = 'google'
        Name = 'google'
      end
      item
        CollectionIndex = 117
        CollectionName = 'graduation_cap'
        Name = 'graduation_cap'
      end
      item
        CollectionIndex = 118
        CollectionName = 'grid'
        Name = 'grid'
      end
      item
        CollectionIndex = 119
        CollectionName = 'headset'
        Name = 'headset'
      end
      item
        CollectionIndex = 120
        CollectionName = 'heat_map'
        Name = 'heat_map'
      end
      item
        CollectionIndex = 121
        CollectionName = 'high_battery'
        Name = 'high_battery'
      end
      item
        CollectionIndex = 122
        CollectionName = 'high_priority'
        Name = 'high_priority'
      end
      item
        CollectionIndex = 123
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 124
        CollectionName = 'icons8_cup'
        Name = 'icons8_cup'
      end
      item
        CollectionIndex = 125
        CollectionName = 'idea'
        Name = 'idea'
      end
      item
        CollectionIndex = 126
        CollectionName = 'image_file'
        Name = 'image_file'
      end
      item
        CollectionIndex = 127
        CollectionName = 'import'
        Name = 'import'
      end
      item
        CollectionIndex = 128
        CollectionName = 'in_transit'
        Name = 'in_transit'
      end
      item
        CollectionIndex = 129
        CollectionName = 'info'
        Name = 'info'
      end
      item
        CollectionIndex = 130
        CollectionName = 'inspection'
        Name = 'inspection'
      end
      item
        CollectionIndex = 131
        CollectionName = 'integrated_webcam'
        Name = 'integrated_webcam'
      end
      item
        CollectionIndex = 132
        CollectionName = 'internal'
        Name = 'internal'
      end
      item
        CollectionIndex = 133
        CollectionName = 'invite'
        Name = 'invite'
      end
      item
        CollectionIndex = 134
        CollectionName = 'ipad'
        Name = 'ipad'
      end
      item
        CollectionIndex = 135
        CollectionName = 'iphone'
        Name = 'iphone'
      end
      item
        CollectionIndex = 136
        CollectionName = 'key'
        Name = 'key'
      end
      item
        CollectionIndex = 137
        CollectionName = 'kindle'
        Name = 'kindle'
      end
      item
        CollectionIndex = 138
        CollectionName = 'landscape'
        Name = 'landscape'
      end
      item
        CollectionIndex = 139
        CollectionName = 'leave'
        Name = 'leave'
      end
      item
        CollectionIndex = 140
        CollectionName = 'left'
        Name = 'left'
      end
      item
        CollectionIndex = 141
        CollectionName = 'left_down2'
        Name = 'left_down2'
      end
      item
        CollectionIndex = 142
        CollectionName = 'left_up2'
        Name = 'left_up2'
      end
      item
        CollectionIndex = 143
        CollectionName = 'library'
        Name = 'library'
      end
      item
        CollectionIndex = 144
        CollectionName = 'light_at_the_end_of_tunnel'
        Name = 'light_at_the_end_of_tunnel'
      end
      item
        CollectionIndex = 145
        CollectionName = 'like'
        Name = 'like'
      end
      item
        CollectionIndex = 146
        CollectionName = 'like_placeholder'
        Name = 'like_placeholder'
      end
      item
        CollectionIndex = 147
        CollectionName = 'line_chart'
        Name = 'line_chart'
      end
      item
        CollectionIndex = 148
        CollectionName = 'link'
        Name = 'link'
      end
      item
        CollectionIndex = 149
        CollectionName = 'linux'
        Name = 'linux'
      end
      item
        CollectionIndex = 150
        CollectionName = 'list'
        Name = 'list'
      end
      item
        CollectionIndex = 151
        CollectionName = 'lock'
        Name = 'lock'
      end
      item
        CollectionIndex = 152
        CollectionName = 'lock_landscape'
        Name = 'lock_landscape'
      end
      item
        CollectionIndex = 153
        CollectionName = 'lock_portrait'
        Name = 'lock_portrait'
      end
      item
        CollectionIndex = 154
        CollectionName = 'low_battery'
        Name = 'low_battery'
      end
      item
        CollectionIndex = 155
        CollectionName = 'low_priority'
        Name = 'low_priority'
      end
      item
        CollectionIndex = 156
        CollectionName = 'make_decision'
        Name = 'make_decision'
      end
      item
        CollectionIndex = 157
        CollectionName = 'manager'
        Name = 'manager'
      end
      item
        CollectionIndex = 158
        CollectionName = 'medium_priority'
        Name = 'medium_priority'
      end
      item
        CollectionIndex = 159
        CollectionName = 'menu'
        Name = 'menu'
      end
      item
        CollectionIndex = 160
        CollectionName = 'middle_battery'
        Name = 'middle_battery'
      end
      item
        CollectionIndex = 161
        CollectionName = 'mind_map'
        Name = 'mind_map'
      end
      item
        CollectionIndex = 162
        CollectionName = 'missed_call'
        Name = 'missed_call'
      end
      item
        CollectionIndex = 163
        CollectionName = 'mms'
        Name = 'mms'
      end
      item
        CollectionIndex = 164
        CollectionName = 'money_transfer'
        Name = 'money_transfer'
      end
      item
        CollectionIndex = 165
        CollectionName = 'multiple_cameras'
        Name = 'multiple_cameras'
      end
      item
        CollectionIndex = 166
        CollectionName = 'multiple_devices'
        Name = 'multiple_devices'
      end
      item
        CollectionIndex = 167
        CollectionName = 'multiple_inputs'
        Name = 'multiple_inputs'
      end
      item
        CollectionIndex = 168
        CollectionName = 'multiple_smartphones'
        Name = 'multiple_smartphones'
      end
      item
        CollectionIndex = 169
        CollectionName = 'music'
        Name = 'music'
      end
      item
        CollectionIndex = 170
        CollectionName = 'neutral_decision'
        Name = 'neutral_decision'
      end
      item
        CollectionIndex = 171
        CollectionName = 'neutral_trading'
        Name = 'neutral_trading'
      end
      item
        CollectionIndex = 172
        CollectionName = 'news'
        Name = 'news'
      end
      item
        CollectionIndex = 173
        CollectionName = 'next'
        Name = 'next'
      end
      item
        CollectionIndex = 174
        CollectionName = 'nfc_sign'
        Name = 'nfc_sign'
      end
      item
        CollectionIndex = 175
        CollectionName = 'night_landscape'
        Name = 'night_landscape'
      end
      item
        CollectionIndex = 176
        CollectionName = 'night_portrait'
        Name = 'night_portrait'
      end
      item
        CollectionIndex = 177
        CollectionName = 'no_idea'
        Name = 'no_idea'
      end
      item
        CollectionIndex = 178
        CollectionName = 'no_video'
        Name = 'no_video'
      end
      item
        CollectionIndex = 179
        CollectionName = 'nook'
        Name = 'nook'
      end
      item
        CollectionIndex = 180
        CollectionName = 'numerical_sorting_12'
        Name = 'numerical_sorting_12'
      end
      item
        CollectionIndex = 181
        CollectionName = 'numerical_sorting_21'
        Name = 'numerical_sorting_21'
      end
      item
        CollectionIndex = 182
        CollectionName = 'ok'
        Name = 'ok'
      end
      item
        CollectionIndex = 183
        CollectionName = 'old_time_camera'
        Name = 'old_time_camera'
      end
      item
        CollectionIndex = 184
        CollectionName = 'online_support'
        Name = 'online_support'
      end
      item
        CollectionIndex = 185
        CollectionName = 'opened_folder'
        Name = 'opened_folder'
      end
      item
        CollectionIndex = 186
        CollectionName = 'org_unit'
        Name = 'org_unit'
      end
      item
        CollectionIndex = 187
        CollectionName = 'organization'
        Name = 'organization'
      end
      item
        CollectionIndex = 188
        CollectionName = 'package'
        Name = 'package'
      end
      item
        CollectionIndex = 189
        CollectionName = 'paid'
        Name = 'paid'
      end
      item
        CollectionIndex = 190
        CollectionName = 'panorama'
        Name = 'panorama'
      end
      item
        CollectionIndex = 191
        CollectionName = 'parallel_tasks'
        Name = 'parallel_tasks'
      end
      item
        CollectionIndex = 192
        CollectionName = 'phone'
        Name = 'phone'
      end
      item
        CollectionIndex = 193
        CollectionName = 'phone_android'
        Name = 'phone_android'
      end
      item
        CollectionIndex = 194
        CollectionName = 'photo_reel'
        Name = 'photo_reel'
      end
      item
        CollectionIndex = 195
        CollectionName = 'picture'
        Name = 'picture'
      end
      item
        CollectionIndex = 196
        CollectionName = 'pie_chart'
        Name = 'pie_chart'
      end
      item
        CollectionIndex = 197
        CollectionName = 'planner'
        Name = 'planner'
      end
      item
        CollectionIndex = 198
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 199
        CollectionName = 'podium_with_audience'
        Name = 'podium_with_audience'
      end
      item
        CollectionIndex = 200
        CollectionName = 'podium_with_speaker'
        Name = 'podium_with_speaker'
      end
      item
        CollectionIndex = 201
        CollectionName = 'podium_without_speaker'
        Name = 'podium_without_speaker'
      end
      item
        CollectionIndex = 202
        CollectionName = 'portrait_mode'
        Name = 'portrait_mode'
      end
      item
        CollectionIndex = 203
        CollectionName = 'previous'
        Name = 'previous'
      end
      item
        CollectionIndex = 204
        CollectionName = 'print'
        Name = 'print'
      end
      item
        CollectionIndex = 205
        CollectionName = 'privacy'
        Name = 'privacy'
      end
      item
        CollectionIndex = 206
        CollectionName = 'process'
        Name = 'process'
      end
      item
        CollectionIndex = 207
        CollectionName = 'puzzle'
        Name = 'puzzle'
      end
      item
        CollectionIndex = 208
        CollectionName = 'questions'
        Name = 'questions'
      end
      item
        CollectionIndex = 209
        CollectionName = 'radar_plot'
        Name = 'radar_plot'
      end
      item
        CollectionIndex = 210
        CollectionName = 'rating'
        Name = 'rating'
      end
      item
        CollectionIndex = 211
        CollectionName = 'ratings'
        Name = 'ratings'
      end
      item
        CollectionIndex = 212
        CollectionName = 'reading'
        Name = 'reading'
      end
      item
        CollectionIndex = 213
        CollectionName = 'reading_ebook'
        Name = 'reading_ebook'
      end
      item
        CollectionIndex = 214
        CollectionName = 'reddit'
        Name = 'reddit'
      end
      item
        CollectionIndex = 215
        CollectionName = 'redo'
        Name = 'redo'
      end
      item
        CollectionIndex = 216
        CollectionName = 'refresh'
        Name = 'refresh'
      end
      item
        CollectionIndex = 217
        CollectionName = 'registered_trademark'
        Name = 'registered_trademark'
      end
      item
        CollectionIndex = 218
        CollectionName = 'remove_image'
        Name = 'remove_image'
      end
      item
        CollectionIndex = 219
        CollectionName = 'reuse'
        Name = 'reuse'
      end
      item
        CollectionIndex = 220
        CollectionName = 'right'
        Name = 'right'
      end
      item
        CollectionIndex = 221
        CollectionName = 'right_down2'
        Name = 'right_down2'
      end
      item
        CollectionIndex = 222
        CollectionName = 'right_up2'
        Name = 'right_up2'
      end
      item
        CollectionIndex = 223
        CollectionName = 'rotate_camera'
        Name = 'rotate_camera'
      end
      item
        CollectionIndex = 224
        CollectionName = 'rotate_to_landscape'
        Name = 'rotate_to_landscape'
      end
      item
        CollectionIndex = 225
        CollectionName = 'rotate_to_portrait'
        Name = 'rotate_to_portrait'
      end
      item
        CollectionIndex = 226
        CollectionName = 'rules'
        Name = 'rules'
      end
      item
        CollectionIndex = 227
        CollectionName = 'safe'
        Name = 'safe'
      end
      item
        CollectionIndex = 228
        CollectionName = 'sales_performance'
        Name = 'sales_performance'
      end
      item
        CollectionIndex = 229
        CollectionName = 'scatter_plot'
        Name = 'scatter_plot'
      end
      item
        CollectionIndex = 230
        CollectionName = 'search'
        Name = 'search'
      end
      item
        CollectionIndex = 231
        CollectionName = 'self_service_kiosk'
        Name = 'self_service_kiosk'
      end
      item
        CollectionIndex = 232
        CollectionName = 'selfie'
        Name = 'selfie'
      end
      item
        CollectionIndex = 233
        CollectionName = 'serial_tasks'
        Name = 'serial_tasks'
      end
      item
        CollectionIndex = 234
        CollectionName = 'service_mark'
        Name = 'service_mark'
      end
      item
        CollectionIndex = 235
        CollectionName = 'services'
        Name = 'services'
      end
      item
        CollectionIndex = 236
        CollectionName = 'settings'
        Name = 'settings'
      end
      item
        CollectionIndex = 237
        CollectionName = 'share'
        Name = 'share'
      end
      item
        CollectionIndex = 238
        CollectionName = 'shipped'
        Name = 'shipped'
      end
      item
        CollectionIndex = 239
        CollectionName = 'shop'
        Name = 'shop'
      end
      item
        CollectionIndex = 240
        CollectionName = 'sim_card'
        Name = 'sim_card'
      end
      item
        CollectionIndex = 241
        CollectionName = 'sim_card_chip'
        Name = 'sim_card_chip'
      end
      item
        CollectionIndex = 242
        CollectionName = 'slr_back_side'
        Name = 'slr_back_side'
      end
      item
        CollectionIndex = 243
        CollectionName = 'smartphone_tablet'
        Name = 'smartphone_tablet'
      end
      item
        CollectionIndex = 244
        CollectionName = 'sms'
        Name = 'sms'
      end
      item
        CollectionIndex = 245
        CollectionName = 'sound_recording_copyright'
        Name = 'sound_recording_copyright'
      end
      item
        CollectionIndex = 246
        CollectionName = 'speaker'
        Name = 'speaker'
      end
      item
        CollectionIndex = 247
        CollectionName = 'sports_mode'
        Name = 'sports_mode'
      end
      item
        CollectionIndex = 248
        CollectionName = 'stack_of_photos'
        Name = 'stack_of_photos'
      end
      item
        CollectionIndex = 249
        CollectionName = 'start'
        Name = 'start'
      end
      item
        CollectionIndex = 250
        CollectionName = 'steam'
        Name = 'steam'
      end
      item
        CollectionIndex = 251
        CollectionName = 'stumbleupon'
        Name = 'stumbleupon'
      end
      item
        CollectionIndex = 252
        CollectionName = 'support'
        Name = 'support'
      end
      item
        CollectionIndex = 253
        CollectionName = 'survey'
        Name = 'survey'
      end
      item
        CollectionIndex = 254
        CollectionName = 'switch_camera'
        Name = 'switch_camera'
      end
      item
        CollectionIndex = 255
        CollectionName = 'synchronize'
        Name = 'synchronize'
      end
      item
        CollectionIndex = 256
        CollectionName = 'tablet_android'
        Name = 'tablet_android'
      end
      item
        CollectionIndex = 257
        CollectionName = 'template'
        Name = 'template'
      end
      item
        CollectionIndex = 258
        CollectionName = 'timeline'
        Name = 'timeline'
      end
      item
        CollectionIndex = 259
        CollectionName = 'todo_list'
        Name = 'todo_list'
      end
      item
        CollectionIndex = 260
        CollectionName = 'touchscreen_smartphone'
        Name = 'touchscreen_smartphone'
      end
      item
        CollectionIndex = 261
        CollectionName = 'trademark'
        Name = 'trademark'
      end
      item
        CollectionIndex = 262
        CollectionName = 'tree_structure'
        Name = 'tree_structure'
      end
      item
        CollectionIndex = 263
        CollectionName = 'two_smartphones'
        Name = 'two_smartphones'
      end
      item
        CollectionIndex = 264
        CollectionName = 'undo'
        Name = 'undo'
      end
      item
        CollectionIndex = 265
        CollectionName = 'unlock'
        Name = 'unlock'
      end
      item
        CollectionIndex = 266
        CollectionName = 'up'
        Name = 'up'
      end
      item
        CollectionIndex = 267
        CollectionName = 'up_left'
        Name = 'up_left'
      end
      item
        CollectionIndex = 268
        CollectionName = 'up_right'
        Name = 'up_right'
      end
      item
        CollectionIndex = 269
        CollectionName = 'upload'
        Name = 'upload'
      end
      item
        CollectionIndex = 270
        CollectionName = 'usb'
        Name = 'usb'
      end
      item
        CollectionIndex = 271
        CollectionName = 'video_call'
        Name = 'video_call'
      end
      item
        CollectionIndex = 272
        CollectionName = 'video_file'
        Name = 'video_file'
      end
      item
        CollectionIndex = 273
        CollectionName = 'video_projector'
        Name = 'video_projector'
      end
      item
        CollectionIndex = 274
        CollectionName = 'view_details'
        Name = 'view_details'
      end
      item
        CollectionIndex = 275
        CollectionName = 'vip'
        Name = 'vip'
      end
      item
        CollectionIndex = 276
        CollectionName = 'vlc'
        Name = 'vlc'
      end
      item
        CollectionIndex = 277
        CollectionName = 'voice_presentation'
        Name = 'voice_presentation'
      end
      item
        CollectionIndex = 278
        CollectionName = 'voicemail'
        Name = 'voicemail'
      end
      item
        CollectionIndex = 279
        CollectionName = 'webcam'
        Name = 'webcam'
      end
      item
        CollectionIndex = 280
        CollectionName = 'wi-fi_logo'
        Name = 'wi-fi_logo'
      end
      item
        CollectionIndex = 281
        CollectionName = 'wikipedia'
        Name = 'wikipedia'
      end
      item
        CollectionIndex = 282
        CollectionName = 'workflow'
        Name = 'workflow'
      end
      item
        CollectionIndex = 283
        CollectionName = 'Pyton'
        Name = 'Pyton'
      end
      item
        CollectionIndex = 284
        CollectionName = 'monochrome'
        Name = 'monochrome'
      end>
    ImageCollection = ImageDataModule.SVGIconImageCollection
    Width = 32
    Height = 32
    Left = 424
    Top = 336
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 472
    Top = 72
  end
  object tmrTrackbar: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrTrackbarTimer
    Left = 250
    Top = 486
  end
end
