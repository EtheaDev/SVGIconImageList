unit SVGIconImageListBase;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  Vcl.Graphics,
  WinApi.Windows,
  SVG,
  SVGColor;

const
  SVGIconImageListVersion = '1.6.0';
  DEFAULT_SIZE = 16;

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

type
  TSVGIconImageListBase = class(TDragImageList)
  private
    FStopDrawing: Integer;
    FOpacity: Byte;
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    FFixedColor: TSVGColor;
    FGrayScale: Boolean;
    FDisabledGrayScale: Boolean;
    FDisabledOpacity: Byte;
  protected
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;

    procedure SetSize(const Value: Integer);
    procedure SetFixedColor(const Value: TSVGColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetDisabledGrayScale(const Value: Boolean);
    procedure SetDisabledOpacity(const Value: Byte);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);


    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure Loaded; override;
    function GetCount: Integer;    {$IF CompilerVersion > 29} override; {$ENDIF}

    procedure StopDrawing(const AStop: Boolean);
    procedure Change; override;
    procedure RecreateBitmaps;

    function SVGToIcon(const SVG: TSVG): HICON;


  public
    property Count: Integer read GetCount;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property FixedColor: TSVGColor read FFixedColor write SetFixedColor default TSVGColor.inherit_color;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;

    {$IFDEF HiDPISupport}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
  end;

implementation

end.
