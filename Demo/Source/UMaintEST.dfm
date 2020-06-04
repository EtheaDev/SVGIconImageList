object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 234
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SVGIconImage: TSVGIconImage
    Left = 0
    Top = 0
    Width = 258
    Height = 234
    Hint = 'Click to show SVGText property editor'
    AutoSize = False
    Center = True
    Proportional = False
    Stretch = True
    Opacity = 255
    Scale = 1.000000000000000000
    ImageIndex = -1
    SVGText = 
      '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
      ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#673' +
      'AB7" d="M38,44H12V4h26c2.2,0,4,1.8,4,4v32C42,42.2,40.2,44,38,44z' +
      '"/>'#13#10'    <path fill="#311B92" d="M10,4h2v40h-2c-2.2,0-4-1.8-4-4V' +
      '8C6,5.8,7.8,4,10,4z"/>'#13#10'    <path fill="#fff" d="M36,24.2c-0.1,4' +
      '.8-3.1,6.9-5.3,6.7c-0.6-0.1-2.1-0.1-2.9-1.6c-0.8,1-1.8,1.6-3.1,1' +
      '.6c-2.6,0-3.3-2.5-3.4-3.1 c-0.1-0.7-0.2-1.4-0.1-2.2c0.1-1,1.1-6.' +
      '5,5.7-6.5c2.2,0,3.5,1.1,3.7,1.3L30,27.2c0,0.3-0.2,1.6,1.1,1.6c2.' +
      '1,0,2.4-3.9,2.4-4.6 c0.1-1.2,0.3-8.2-7-8.2c-6.9,0-7.9,7.4-8,9.2c' +
      '-0.5,8.5,6,8.5,7.2,8.5c1.7,0,3.7-0.7,3.9-0.8l0.4,2c-0.3,0.2-2,1.' +
      '1-4.4,1.1 c-2.2,0-10.1-0.4-9.8-10.8C16.1,23.1,17.4,14,26.6,14C35' +
      '.8,14,36,22.1,36,24.2z M24.1,25.5c-0.1,1,0,1.8,0.2,2.3 c0.2,0.5,' +
      '0.6,0.8,1.2,0.8c0.1,0,0.3,0,0.4-0.1c0.2-0.1,0.3-0.1,0.5-0.3c0.2-' +
      '0.1,0.3-0.3,0.5-0.6c0.2-0.2,0.3-0.6,0.4-1l0.5-5.4 c-0.2-0.1-0.5-' +
      '0.1-0.7-0.1c-0.5,0-0.9,0.1-1.2,0.3c-0.3,0.2-0.6,0.5-0.9,0.8c-0.2' +
      ',0.4-0.4,0.8-0.6,1.3S24.2,24.8,24.1,25.5z"/>'#13#10'</svg>'#13#10
    Align = alClient
    OnClick = SVGIconImageClick
  end
end
