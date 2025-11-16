{**
 @abstract(@name provides a rasterizer to perform the painting of a Scalable Vector Graphics (SVG) image.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGRasterizer;

interface

uses System.SysUtils,
     System.Classes,
     System.Math,
     System.Generics.Collections,
     System.UITypes,
     Soap.EncdDecd,
     Vcl.Graphics,
     Winapi.Windows,
     UTWHelpers,
     UTWColor,
     UTWFillAndStroke,
     UTWVector,
     UTWMatrix,
     UTWRect,
     UTWGeometryTools,
     UTWDateTime,
     UTWSmartPointer,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGItems,
     UTWSVGAttribute,
     UTWSVGMeasure,
     UTWSVGGradients,
     UTWSVGFilters,
     UTWSVGAnimation,
     UTWSVGProperties,
     UTWSVGElements,
     UTWSVGStyle,
     UTWSVGParser,
     UTWSVG,
     UTWSVGAnimationDescriptor;

const
    //---------------------------------------------------------------------------
    // Global defines
    //---------------------------------------------------------------------------
    C_SVG_Default_Color:      TColor                             = clBlack;
    C_SVG_Default_Display:    TWSVGStyle.IPropDisplay.IEValue    = TWSVGStyle.IPropDisplay.IEValue.IE_V_Inline;
    C_SVG_Default_Visibility: TWSVGStyle.IPropVisibility.IEValue = TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible;
    //---------------------------------------------------------------------------

type
    {**
     Scalable Vector Graphics (SVG) rasterizer
     @br @bold(NOTE) See http://www.w3.org/TR/SVGCompositing/
    }
    TWSVGRasterizer = class
        public type
            {**
             Properties rules, determine how the prop item should be used in relation with its parent
             @value(IE_PR_Default The default value is used)
             @value(IE_PR_Inherit The value is inherited from the parent, set to default if no parent)
             @value(IE_PR_Combine The value is merged with the value contained in the parent)
            }
            IEPropRule =
            (
                IE_PR_Default,
                IE_PR_Inherit,
                IE_PR_Combine
            );

            {**
             Image type
             @value(IE_IT_Unknown image type is unknown)
             @value(IE_IT_PNG The image is a PNG image)
             @value(IE_IT_JPG The image is a JPG image)
             @value(IE_IT_SVG The image is a SVG image)
            }
            IEImageType =
            (
                IE_IT_Unknown,
                IE_IT_PNG,
                IE_IT_JPG,
                IE_IT_SVG
            );

            {**
             Text anchoring
             @value(IE_TA_Start Text is anchored on the left or top)
             @value(IE_TA_Middle Text is anchored on the center)
             @value(IE_TA_End Text is anchored on the right or bottom)
            }
            IETextAnchor =
            (
                IE_TA_Start,
                IE_TA_Middle,
                IE_TA_End
            );

            {**
             Text decoration
             @value(IE_TD_Normal Text without decoration)
             @value(IE_TD_Underline Underlined text)
             @value(IE_TD_LineThrough Line through text)
            }
            IETextDecoration =
            (
                IE_TD_Normal,
                IE_TD_Underline,
                IE_TD_LineThrough
            );

            {**
             Animation parameters structure. Custom data will be transmitted in OnAnimate callback
            }
            IAnimation = record
                m_Position:    Double;
                m_pCustomData: Pointer;
            end;

            {**
             Called while animation is running
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            ITfAnimateEvent = function (pAnimDesc: TWSVGAnimationDescriptor; pCustomData: Pointer): Boolean of object;

            {**
             Called while animation is running
             @param(pSender Event sender)
             @param(pStream Stream containing the image to read)
             @param(imageType The image type)
             @param(pGraphic The read image graphic)
             @returns(@true on success, otherwise @false)
            }
            ITfGetImageEvent = function (pSender: TObject; pStream: TMemoryStream; imageType: IEImageType;
                    var pGraphic: TGraphic): Boolean of object;

        protected type
            IValuesF = TWSVGAttribute<Single>.IValues;

            {**
             Property item
            }
            IPropItem = class
                private
                    m_Rule: IEPropRule;

                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; virtual;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); virtual;

                public
                    {**
                     Get or set the item rule
                    }
                    property Rule: IEPropRule read m_Rule write m_Rule;
            end;

            {**
             Boolean property item
            }
            IPropBoolItem = class(IPropItem)
                private
                    m_Value: Boolean;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: Boolean;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropBoolItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: Boolean read m_Value write m_Value;
            end;

            {**
             Float property item
            }
            IPropFloatItem = class(IPropItem)
                private
                    m_Value: Single;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: Single;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropFloatItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: Single read m_Value write m_Value;
            end;

            {**
             Color property item
            }
            IPropColorItem = class(IPropItem)
                public type
                {**
                 Color merge mode
                 @value(IE_MM_KeepThis The parent color will be ignored, the current color will be kept)
                 @value(IE_MM_KeepParent The parent color will replace completely the current one)
                 @value(IE_MM_ParentColor The color will be copied from parent, the current opacity is kept)
                 @value(IE_MM_ParentOpacity The opacity will be copied from parent, the current color is kept)
                }
                IEMergeMode =
                (
                    IE_MM_KeepThis,
                    IE_MM_KeepParent,
                    IE_MM_ParentColor,
                    IE_MM_ParentOpacity
                );

                private
                    m_Value:     TWColor;
                    m_MergeMode: IEMergeMode;

                protected
                    {**
                     Get value
                     @returns(Value)
                    }
                    function GetValue: PWColor; virtual;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(pValue Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(pValue: PWColor;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Set value
                     @param(pValue Value)
                    }
                    procedure SetValue(const pValue: PWColor); virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropColorItem); virtual;

                public
                    {**
                     Get value
                    }
                    property Value: PWColor read GetValue;

                    {**
                     Get or set the merge mode
                    }
                    property MergeMode: IEMergeMode read m_MergeMode write m_MergeMode;
            end;

            {**
             Matrix property item
            }
            IPropMatrixItem = class(IPropItem)
                private
                    m_Value: TWMatrix3x3;
                    m_Type:  TWSVGPropMatrix.IEType;

                protected
                    {**
                     Get value
                     @returns(Value)
                    }
                    function GetValue: PWMatrix3x3; virtual;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(pValue Item value)
                     @param(matrixType Matrix type (translate, ...))
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(const pValue: PWMatrix3x3; matrixType: TWSVGPropMatrix.IEType;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Set value
                     @param(pValue Value)
                    }
                    procedure SetValue(const pValue: PWMatrix3x3); virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropMatrixItem); virtual;

                public
                    {**
                     Get value
                    }
                    property Value: PWMatrix3x3 read GetValue;
            end;

            {**
             Fill rule property item
            }
            IPropFillRuleItem = class(IPropItem)
                private
                    m_Value: TWSVGFill.IERule;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGFill.IERule;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropFillRuleItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGFill.IERule read m_Value write m_Value;
            end;

            {**
             Dash pattern property item
            }
            IPropDashPatternItem = class(IPropItem)
                private
                    m_pValue: TWDashPattern;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(pValue: TWDashPattern;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropDashPatternItem); virtual;

                public
                    {**
                     Get the dash pattern
                    }
                    property Value: TWDashPattern read m_pValue;
            end;

            {**
             Stroke linecap property item
            }
            IPropStrokeLineCapItem = class(IPropItem)
                private
                    m_Value: TWSVGStroke.IELineCap;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGStroke.IELineCap;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropStrokeLineCapItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGStroke.IELineCap read m_Value write m_Value;
            end;

            {**
             Stroke line join property item
            }
            IPropStrokeLineJoinItem = class(IPropItem)
                private
                    m_Value: TWSVGStroke.IELineJoin;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGStroke.IELineJoin;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropStrokeLineJoinItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGStroke.IELineJoin read m_Value write m_Value;
            end;

            {**
             Display property item
            }
            IPropDisplayItem = class(IPropItem)
                private
                    m_Value: TWSVGStyle.IPropDisplay.IEValue;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGStyle.IPropDisplay.IEValue;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropDisplayItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGStyle.IPropDisplay.IEValue read m_Value write m_Value;
            end;

            {**
             Visibility property item
            }
            IPropVisibilityItem = class(IPropItem)
                private
                    m_Value: TWSVGStyle.IPropVisibility.IEValue;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGStyle.IPropVisibility.IEValue;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropVisibilityItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGStyle.IPropVisibility.IEValue read m_Value write m_Value;
            end;

            {**
             Gradient stop
            }
            IGradientStop = class
                private
                    m_Color:  TWColor;
                    m_Offset: Single;

                protected
                    {**
                     Get color
                     @returns(Color)
                    }
                    function GetColor: PWColor; virtual;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Set color
                     @param(pColor Color)
                    }
                    procedure SetColor(const pColor: PWColor); virtual;

                    {**
                     Clear the item content
                    }
                    procedure Clear; virtual;

                    {**
                     Assign (i.e. copy) the content from another gradient stop
                     @param(pOther Other gradient stop to copy from)
                    }
                    procedure Assign(const pOther: IGradientStop); virtual;

                public
                    {**
                     Get the color property
                    }
                    property Color: PWColor read GetColor;

                    {**
                     Get or set the offset property
                    }
                    property Offset: Single read m_Offset write m_Offset;
            end;

            IGradientStops = TObjectList<IGradientStop>;

            {**
             Gradient
             @br @bold(NOTE) The gradient unit also determines if the value should be read in percent
                             or as coordinates. If IE_GU_ObjectBoundingBox is declared, then the values
                             should be read in percent (between 0.0f and 1.0f). With IE_GU_UserSpaceOnUse,
                             the values contain real coordinates, in the same units as the viewport
            }
            IGradient = class(IPropItem)
                private
                    m_pGradientStops: IGradientStops;
                    m_Matrix:         TWMatrix3x3;
                    m_MatrixType:     TWSVGPropMatrix.IEType;
                    m_Unit:           TWSVGPropUnit.IEType;
                    m_SpreadMethod:   TWSVGGradient.IEGradientSpreadMethod;

                protected
                    {**
                     Get the matrix
                     @returns(The matrix)
                    }
                    function GetMatrix: PWMatrix3x3; virtual;

                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge properties with another properties set
                     @param(pOther Other properties set to merge with)
                    }
                    procedure Merge(const pOther: IGradient); virtual;

                public
                    {**
                     Get the gradient stops property
                    }
                    property GradientStops: IGradientStops read m_pGradientStops;

                    {**
                     Get the matrix property
                    }
                    property Matrix: PWMatrix3x3 read GetMatrix;

                    {**
                     Get or set the matrix type property
                    }
                    property MatrixType: TWSVGPropMatrix.IEType read m_MatrixType write m_MatrixType;

                    {**
                     Get or set the gradient unit property
                    }
                    property GradientUnit: TWSVGPropUnit.IEType read m_Unit write m_Unit;

                    {**
                     Get or set the spread method property
                    }
                    property SpreadMethod: TWSVGGradient.IEGradientSpreadMethod read m_SpreadMethod write m_SpreadMethod;
            end;

            {**
             Linear gradient vector, it's a vector, defined by start and end points, onto which the
             gradient stops are matched, and to which the gradient normal is perpendicular (before
             applying any transform)
            }
            ILinearGradientVector = record
                m_Start: TWVector2;
                m_End:   TWVector2;
            end;

            IPLinearGradientVector = ^ILinearGradientVector;

            {**
             Linear gradient
            }
            ILinearGradient = class(IGradient)
                private
                    m_Vector: ILinearGradientVector;

                protected
                    {**
                     Get the gradient vector
                     @returns(Gradient vector)
                    }
                    function GetVector: IPLinearGradientVector; virtual;

                    {**
                     Set the gradient vector
                     @param(pVector Gradient vector)
                    }
                    procedure SetVector(const pVector: IPLinearGradientVector); virtual;

                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IGradient); override;

                public
                    {**
                     Get or set the gradient vector
                    }
                    property Vector: IPLinearGradientVector read GetVector write SetVector;
            end;

            {**
             Radial gradient
            }
            IRadialGradient = class(IGradient)
                private
                    m_CX: Single;
                    m_CY: Single;
                    m_R:  Single;
                    m_FX: Single;
                    m_FY: Single;

                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IGradient); override;

                public
                    {**
                     Get or set the center x property
                    }
                    property CX: Single read m_CX write m_CX;

                    {**
                     Get or set the center y property
                    }
                    property CY: Single read m_CY write m_CY;

                    {**
                     Get or set the radius property
                    }
                    property R: Single read m_R write m_R;

                    {**
                     Get or set the focus x property
                    }
                    property FX: Single read m_FX write m_FX;

                    {**
                     Get or set the focus y property
                    }
                    property FY: Single read m_FY write m_FY;
            end;

            {**
             Brush, contains a solid color or a gradient to apply to a fill or stroke
            }
            IBrush = class
                private
                    m_pColor:          IPropColorItem;
                    m_pLinearGradient: ILinearGradient;
                    m_pRadialGradient: IRadialGradient;
                    m_Type:            EBrushType;
                    m_Rule:            IEPropRule;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge properties with another properties set
                     @param(pOther Other properties set to merge with)
                    }
                    procedure Merge(const pOther: IBrush); virtual;

                    {**
                     Check if brush is empty
                     @returns(@true if brush is empty, otherwise @false)
                    }
                    function IsEmpty: Boolean; inline;

                public
                    {**
                     Get the color property
                    }
                    property Color: IPropColorItem read m_pColor;

                    {**
                     Get the linear gradient property
                    }
                    property LinearGradient: ILinearGradient read m_pLinearGradient;

                    {**
                     Get the radial gradient property
                    }
                    property RadialGradient: IRadialGradient read m_pRadialGradient;

                    {**
                     Get or set the brush type property
                    }
                    property BrushType: EBrushType read m_Type write m_Type;

                    {**
                     Get or set the rule property
                    }
                    property Rule: IEPropRule read m_Rule write m_Rule;
            end;

            {**
             Fill parameters structure
            }
            IFill = class
                private
                    m_pBrush:  IBrush;
                    m_pRule:   IPropFillRuleItem;
                    m_pNoFill: IPropBoolItem;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IFill); virtual;

                    {**
                     Check if fill is empty
                     @returns(@true if fill is empty, otherwise @false)
                    }
                    function IsEmpty: Boolean; inline;

                public
                    {**
                     Get the brush property
                    }
                    property Brush: IBrush read m_pBrush;

                    {**
                     Get the fill rule property
                    }
                    property FillRule: IPropFillRuleItem read m_pRule;

                    {**
                     Get the no fill property
                    }
                    property NoFill: IPropBoolItem read m_pNoFill;
            end;

            {**
            * Stroke parameters structure
            }
            IStroke = class
                private
                    m_pBrush:       IBrush;
                    m_pDashPattern: IPropDashPatternItem;
                    m_pDashOffset:  IPropFloatItem;
                    m_pWidth:       IPropFloatItem;
                    m_pMiterLimit:  IPropFloatItem;
                    m_pLineCap:     IPropStrokeLineCapItem;
                    m_pLineJoin:    IPropStrokeLineJoinItem;
                    m_pNoStroke:    IPropBoolItem;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IStroke); virtual;

                    {**
                     Check if stroke is empty
                     @returns(@true if stroke is empty, otherwise @false)
                    }
                    function IsEmpty: Boolean; inline;

                public
                    {**
                     Get the brush property
                    }
                    property Brush: IBrush read m_pBrush;

                    {**
                     Get the dash pattern property
                    }
                    property DashPattern: IPropDashPatternItem read m_pDashPattern;

                    {**
                     Get the dash offset property
                    }
                    property DashOffset: IPropFloatItem read m_pDashOffset;

                    {**
                     Get the width property
                    }
                    property Width: IPropFloatItem read m_pWidth;

                    {**
                     Get the miter limit property
                    }
                    property MiterLimit: IPropFloatItem read m_pMiterLimit;

                    {**
                     Get the line cap property
                    }
                    property LineCap: IPropStrokeLineCapItem read m_pLineCap;

                    {**
                     Get the line join property
                    }
                    property LineJoin: IPropStrokeLineJoinItem read m_pLineJoin;

                    {**
                     Get the no stroke property
                    }
                    property NoStroke: IPropBoolItem read m_pNoStroke;
            end;

            {**
             Effect to add to a filter
            }
            IEffect = class(IPropItem)
                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual; abstract;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IEffect); virtual; abstract;
            end;

            IEffects = TObjectList<IEffect>;

            {**
             Gaussian blur effect
            }
            IGaussianBlur = class(IEffect)
                private
                    m_Deviation: TSize;

                public
                    {**
                     Constructor
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(rule: IEPropRule = IE_PR_Inherit); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IEffect); override;

                public
                    {**
                     Get the Gaussian blur deviation
                    }
                    property Deviation: TSize read m_Deviation;
            end;

            {**
             Filter, contains an effect list to apply
            }
            IFilter = class
                private
                    m_pX:       IPropFloatItem;
                    m_pY:       IPropFloatItem;
                    m_pWidth:   IPropFloatItem;
                    m_pHeight:  IPropFloatItem;
                    m_pPercent: IPropBoolItem;
                    m_pEffects: IEffects;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IFilter); virtual;
            end;

            {**
             Style parameters structure
            }
            IStyle = class
                private
                    m_pFill:        IFill;
                    m_pStroke:      IStroke;
                    m_pFilter:      IFilter;
                    m_pOpacity:     IPropFloatItem;
                    m_pDisplayMode: IPropDisplayItem;
                    m_pVisibility:  IPropVisibilityItem;
                    m_Width:        NativeInt;
                    m_Height:       NativeInt;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge properties with another properties set
                     @param(pOther Other properties set to merge with)
                    }
                    procedure Merge(const pOther: IStyle); virtual;

                    {**
                     Get fill color, considering global opacity
                     @returns(Fill color to apply)
                    }
                    function GetFillColor: TWColor; virtual;

                    {**
                     Get stroke color, considering global opacity
                     @returns(Stroke color to apply)
                    }
                    function GetStrokeColor: TWColor; virtual;

                public
                    {**
                     Get the fill properties
                    }
                    property Fill: IFill read m_pFill;

                    {**
                     Get the stroke properties
                    }
                    property Stroke: IStroke read m_pStroke;

                    {**
                     Get the opacity property
                    }
                    property Opacity: IPropFloatItem read m_pOpacity;

                    {**
                     Get the display mode property
                    }
                    property DisplayMode: IPropDisplayItem read m_pDisplayMode;

                    {**
                     Get the visibility property
                    }
                    property Visibility: IPropVisibilityItem read m_pVisibility;

                    {**
                     Get the width
                     @br @bold(NOTE) Altrough generally declared directly inside the header, the
                                     width may also sometimes appear in the header style
                    }
                    property Width: NativeInt read m_Width;

                    {**
                     Get the height
                     @br @bold(NOTE) Altrough generally declared directly inside the header, the
                                     height may also sometimes appear in the header style
                    }
                    property Height: NativeInt read m_Height;
            end;

            {**
             Aspect ratio type property item
            }
            IPropAspectRatioItem = class(IPropItem)
                private
                    m_Value: TWSVGPropAspectRatio.IEAspectRatio;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGPropAspectRatio.IEAspectRatio;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropAspectRatioItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGPropAspectRatio.IEAspectRatio read m_Value write m_Value;
            end;

            {**
             Aspect ratio reference property item
            }
            IPropAspectRatioRefItem = class(IPropItem)
                private
                    m_Value: TWSVGPropAspectRatio.IEReference;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Constructor
                     @param(value Item value)
                     @param(rule Rule to apply to item)
                    }
                    constructor Create(value: TWSVGPropAspectRatio.IEReference;
                            rule: IEPropRule = IE_PR_Inherit); reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the item content
                    }
                    procedure Clear; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: IPropItem); override;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IPropAspectRatioRefItem); virtual;

                public
                    {**
                     Get or set value
                    }
                    property Value: TWSVGPropAspectRatio.IEReference read m_Value write m_Value;
            end;

            {**
             Aspect ratio parameters structure
            }
            IAspectRatio = class(IPropItem)
                private
                    m_pAspectRatio: IPropAspectRatioItem;
                    m_pReference:   IPropAspectRatioRefItem;
                    m_pDefined:     IPropBoolItem;

                public
                    {**
                     Constructor
                    }
                    constructor Create; reintroduce; overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge property with another property
                     @param(pOther Other property to merge with)
                    }
                    procedure Merge(const pOther: IAspectRatio); virtual;

                public
                    {**
                     Get the aspect ratio property
                    }
                    property AspectRatio: IPropAspectRatioItem read m_pAspectRatio;

                    {**
                     Get the aspect ratio reference property
                    }
                    property Reference: IPropAspectRatioRefItem read m_pReference;

                    {**
                     Get if the aspect ratio was defined in the source file for this element (Should be ignored if not)
                    }
                    property Defined: IPropBoolItem read m_pDefined;
            end;

            {**
             Group, switch or object properties
            }
            IProperties = class
                private
                    m_pStyle:       IStyle;
                    m_pMatrix:      IPropMatrixItem;
                    m_pAspectRatio: IAspectRatio;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Load default properties
                    }
                    procedure Default; virtual;

                    {**
                     Merge properties with another properties set
                     @param(pOther Other properties set to merge with)
                    }
                    procedure Merge(const pOther: IProperties); virtual;

                public
                    {**
                     Get the style properties
                    }
                    property Style: IStyle read m_pStyle;

                    {**
                     Get the matrix property
                    }
                    property Matrix: IPropMatrixItem read m_pMatrix;

                    {**
                     Get the aspect ratio properties
                    }
                    property AspectRatio: IAspectRatio read m_pAspectRatio;
            end;

            {**
             Animation data structure
            }
            IAnimationData = class
                private
                    m_pSetAnims:     TList<TWSVGAnimation>;
                    m_pAttribAnims:  TList<TWSVGAnimation>;
                    m_pColorAnims:   TList<TWSVGAnimation>;
                    m_pMatrixAnims:  TList<TWSVGAnimation>;
                    m_pUnknownAnims: TList<TWSVGAnimation>;
                    m_Position:      Double;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set the position, in percent (between 0.0 and 1.0)
                    }
                    property Position: Double read m_Position write m_Position;

                    {**
                     Get set animations
                    }
                    property SetAnims: TList<TWSVGAnimation> read m_pSetAnims;

                    {**
                     Get set animations
                    }
                    property AttribAnims: TList<TWSVGAnimation> read m_pAttribAnims;

                    {**
                     Get set animations
                    }
                    property ColorAnims: TList<TWSVGAnimation> read m_pColorAnims;

                    {**
                     Get set animations
                    }
                    property MatrixAnims: TList<TWSVGAnimation> read m_pMatrixAnims;

                    {**
                     Get set animations
                    }
                    property UnknownAnims: TList<TWSVGAnimation> read m_pUnknownAnims;
            end;

        private type
            {**
             Animation cache item
            }
            IAnimCacheItem = class
                private
                    m_ClearingValue: Double;
                    m_LastPos:       Double;
                    m_Started:       Boolean;
                    m_Ended:         Boolean;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            IAnimCache = TObjectDictionary<TWSVGAnimation, IAnimCacheItem>;

            {**
             SVG cache item
            }
            ICacheItem = class
                private
                    m_pAnimCache:   IAnimCache;
                    m_AnimDuration: NativeUInt;
                    m_LastPos:      Double;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            ICache = TObjectDictionary<UnicodeString, ICacheItem>;

        private
            m_UUID:           UnicodeString;
            m_pCache:         ICache;
            m_fOnAnimate:     ITfAnimateEvent;
            m_fGetImageEvent: ITfGetImageEvent;

            {**
             Convert global animation position to sub-animation position
             @param(animDuration Animation duration)
             @param(pAnimItem Animation cache item)
             @param(animationData Animation data)
             @param(pAnimDesc Animation description)
             @return(Position in local sub-animation system, in percent (between 0.0 and 1.0))
            }
            function LocalPosToGlobalPos(animDuration: NativeUInt; pAnimItem: IAnimCacheItem;
                    const animationData: IAnimationData; const pAnimDesc: TWSVGAnimationDescriptor): Double;

            {**
             Get linked element
             @param(pLink Link containing the element identifier to find)
             @returns(Linked element, @nil if not found or on error)
            }
            function GetLinkedElement(const pLink: TWSVGPropLink): TWSVGElement;

        protected
            m_Animate: Boolean;

            {**
             Initialize SVG to rasterize
             @param(pSVG SVG)
            }
            procedure Initialize(const pSVG: TWSVG); virtual;

            {**
             Set the combine mode to use for a color with animated opacity
             @param(colorItem @bold([in, out]) Color item containing the properties to update)
            }
            procedure SetAnimatedOpacityCombineMode(pColorItem: IPropColorItem); virtual;

            {**
             Get size from header
             @param(pHeader Header to extract from)
             @returns(Size)
            }
            function GetSize(const pHeader: TWSVGParser.IHeader): TSize; overload; virtual;

            {**
             Get global svg view box from header
             @param(pHeader Header to extract from)
             @returns(View box)
            }
            function GetViewBox(const pHeader: TWSVGParser.IHeader): TWRectF; virtual;

            {**
             Get generic position and size properties
             @param(pElement SVG element to extract from)
             @param(x @bold([out]) Element x position)
             @param(y @bold([out]) Element y position)
             @param(width @bold([out]) Element width )
             @param(height @bold([out]) Element height)
             @param(viewBox @bold([out]) Viewbox to apply to element, should be combinated with aspect ratio)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetPosAndSizeProps(const pElement: TWSVGElement; out x: Single; out y: Single;
                    out width: Single; out height:Single; out viewBox: TWRectF; pAnimationData: IAnimationData;
                    pCustomData: Pointer): Boolean;

            {**
             Get rectangle properties
             @param(pRect SVG rect to extract from)
             @param(x @bold([out]) Rect x position)
             @param(y @bold([out]) Rect y position)
             @param(width @bold([out]) Rect width)
             @param(height @bold([out]) Rect height)
             @param(rx @bold([out]) x radius)
             @param(ry @bold([out]) y radius)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetRectProps(const pRect: TWSVGRect; out x: Single; out y: Single;
                    out width: Single; out height: Single; out rx: Single; out ry: Single;
                    pAnimationData: IAnimationData; pCustomData: Pointer): Boolean; virtual;

            {**
             Get circle properties
             @param(pCircle SVG circle to extract from)
             @param(x @bold([out]) Circle x position)
             @param(y @bold([out]) Circle y position)
             @param(radius @bold([out]) Circle radius)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetCircleProps(const pCircle: TWSVGCircle; out x: Single; out y: Single;
                    out radius: Single; pAnimationData: IAnimationData;
                    pCustomData: Pointer): Boolean; virtual;

            {**
             Get ellipse properties
             @param(pEllipse SVG ellipse to extract from)
             @param(x @bold([out]) Ellipse x position)
             @param(y @bold([out]) Ellipse y position)
             @param(rx @bold([out]) Ellipse x radius)
             @param(ry @bold([out]) Ellipse y radius)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetEllipseProps(const pEllipse: TWSVGEllipse; out x: Single; out y: Single;
                    out rx: Single; out ry: Single; pAnimationData: IAnimationData;
                    pCustomData: Pointer): Boolean; virtual;

            {**
             Get line properties
             @param(pLine SVG line to extract from)
             @param(x1 @bold([out]) Line start x position)
             @param(y1 @bold([out]) Line start y position)
             @param(x2 @bold([out]) Line end x position)
             @param(y2 @bold([out]) Line end y position)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetLineProps(const pLine: TWSVGLine; out x1: Single; out y1: Single; out x2: Single;
                    out y2: Single; pAnimationData: IAnimationData;
                    pCustomData: Pointer): Boolean; virtual;

            {**
             Get image properties
             @param(pImage SVG image to extract from)
             @param(x @bold([out]) Image start x position)
             @param(y @bold([out]) Image start y position)
             @param(width @bold([out]) Image width)
             @param(height @bold([out]) Image height)
             @param(viewBox @bold([out]) Viewbox to apply to image, should be combinated with aspect ratio)
             @param(imageType @bold([out]) Image type)
             @param(pImageData The stream which will contain the image data)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetImageProps(const pImage: TWSVGImage; out x: Single; out y: Single;
                    out width: Single; out height: Single; out viewBox: TWRectF; out imageType: IEImageType;
                    pImageData: TMemoryStream; pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;

            {**
             Get text properties
             @param(pText SVG text to extract from)
             @param(x @bold([out]) Text start x position)
             @param(y @bold([out]) Text start y position)
             @param(fontFamily @bold([out]) Text font family name to use)
             @param(fontSize @bold([out]) Text font size)
             @param(fontWeight @bold([out]) Text font weight)
             @param(bolder @bold([out]) If true, text font weight is bolder than current value)
             @param(lighter @bold([out]) If true, text font weight is lighter than current value)
             @param(fontStyle @bold[out]) Font style)
             @param(fontStyleAngle @bold[out]) Font style angle, in case font style is set to TWSVGText.IE_FS_Oblique)
             @param(anchor @bold([out]) Text anchor)
             @param(decoration @bold([out]) Text decoration)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetTextProps(const pText: TWSVGText; out x: Single; out y: Single;
                    out fontFamily: UnicodeString; out fontSize: Single; out fontWeight: Cardinal;
                    out bolder: Boolean; out lighter: Boolean; out fontStyle: TWSVGText.IEFontStyle;
                    out fontStyleAngle: Single; out anchor: IETextAnchor; out decoration: IETextDecoration;
                    pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;

            {**
             Get element properties
             @param(pElement Element to extract from)
             @param(pProperties Element properties to use)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetElementProps(const pElement: TWSVGElement; pProperties: IProperties;
                    pAnimationData: IAnimationData; pCustomData: Pointer): Boolean; virtual;

            {**
             Get style properties
             @param(pStyle Style to extract from)
             @param(pProperties Style properties to use)
             @param(pAnimationData Animation data)
             @param(pCustomData Custom data)
             @returns(@true on success, otherwise @false)
            }
            function GetStyleProps(const pStyle: TWSVGStyle; pProperties: IProperties;
                    pAnimationData: IAnimationData; pCustomData: Pointer): Boolean; virtual;

            {**
             Get use properties
             @param(pUse SVG use instruction to extract from)
             @param(pElement @bold([out]) Element to use)
             @returns(@true on success, otherwise @false)
            }
            function GetLinkedElementToUse(const pUse: TWSVGUse; out pElement: TWSVGElement): Boolean; virtual;

            {**
             Get the clip path linked to an element
             @param(pElement Element for which the clip path should be found)
             @param(pClipPath @bold([out]) The found clip path, @nil if not found or on error)
             @returns(@true on success, otherwise @false)
            }
            function GetClipPath(const pElement: TWSVGElement; out pClipPath: TWSVGClipPath): Boolean; virtual;

            {**
             Get a filter from a link
            @param(pLink Filter object link to parse)
            @param(pFilter Filter to populate)
            @returns(@true on success, otherwise @false)
            }
            function GetFilterFromLink(const pLink: TWSVGPropLink; pFilter: IFilter): Boolean; virtual;

            {**
             Get a color from a link
             @param(pLink Color object link to parse)
             @param(pBrush Brush to populate)
             @returns(@true on success, otherwise @false)
            }
            function GetColorFromLink(const pLink: TWSVGPropLink; pBrush: IBrush): Boolean; virtual;

            {**
             Get the gradient stops
             @param(pGradient SVG gradient containing the gradient stops to extract)
             @param(pGradientStops Gradient stop list to populate)
            }
            procedure GetGradientStops(const pGradient: TWSVGGradient; pGradientStops: IGradientStops); virtual;

            {**
             Get transform matrix from shape animation
             @param(pAnimationData Shape animation data)
             @param(pMatrix Matrix item)
             @param(pCustomData Custom data)
            }
            procedure GetTransformAnimMatrix(const pAnimationData: IAnimationData; pMatrixItem: IPropMatrixItem;
                    pCustomData: Pointer);

            {**
             Get animations from container
             @param(pContainer Container to extract from)
             @param(pAnimationData Animation data)
            }
            procedure GetAnimations(const pContainer: TWSVGContainer; pAnimationData: IAnimationData); virtual;

            {**
             Populate animation values
             @param(pAnimation Animation to extract from)
             @param(attribName @bold([out]) SVG attribute name to animate, if empty animation is global)
             @param(pAnimDesc Animation description to populate)
             @param(callOnAnimate If @true, OnAnimate will be called, if defined)
             @param(pCustomData Custom data)
             @returns(@true if animation is allowed to continue, otherwise @false)
            }
            function PopulateAnimation(const pAnimation: TWSVGAnimation; out attribName: UnicodeString;
                    pAnimDesc: TWSVGAnimationDescriptor; callOnAnimate: Boolean;
                    pCustomData: Pointer): Boolean; virtual;

            {**
             Get animation duration
             @param(pHeader SVG header to populate, populated header when function ends)
             @param(pElements Elements to search duration in)
             @param(switchMode If @true, function will return after first element is drawn (because reading switch statement))
             @param(useMode If @true, the current element to read is owned by an use link)
             @param(durationMax @bold([out]) Highest duration found in svg in milliseconds, 0 if duration is indefinite)
            }
            procedure GetAnimationDuration(pHeader: TWSVGParser.IHeader;
                    const pElements: TWSVGContainer.IElements; switchMode, useMode: Boolean;
                    out durationMax: NativeUInt); overload; virtual;

            {**
             Get animation duration
             @param(pAnimationData Animation data)
             @returns(Animation duration in milliseconds, 0 if duration is indefinite)
            }
            function GetAnimationDuration(const pAnimationData: IAnimationData): NativeUInt; overload; virtual;

            {**
             Measure the animation duration
             @param(pAnimation Animation to measure)
             @param(pAnimDesc Animation description)
             @param(duration @bold([in, out]) Animation duration, new measured duration on function ends)
            }
            procedure MeasureDuration(pAnimation: TWSVGAnimation; var duration: NativeUInt); overload; virtual;
            procedure MeasureDuration(const pAnimation: TWSVGAnimation; pAnimDesc: TWSVGAnimationDescriptor;
                    var duration: NativeUInt); overload; virtual;

            {**
             Transform point using matrix
             @param(pMatrix Matrix to apply)
             @param(point @bold([in, out]) Point to transform, transformed point on function ends)
             @returns(@true on success, otherwise @false)
            }
            function Transform(const pMatrix: TWSVGPropMatrix; var point: TWVector2): Boolean; virtual;

            {**
             Calculate at which percent the animation begins
             @param(pAnimDesc Animation description)
             @returns(Animation begin point in percent (between 0.0 and 1.0))
             @br @bold(NOTE) Returned value can be negative
            }
            function BeginAt(const pAnimDesc: TWSVGAnimationDescriptor): Double; virtual;

            {**
             Calculate at which percent the animation ends
             @param(animDesc Animation description)
             @returns(Animation end point in percent (between 0.0 and 1.0))
            }
            function EndAt(const pAnimDesc: TWSVGAnimationDescriptor): Double; virtual;

            {**
             Get animation position and check if animation is running
             @param(animationData Animation data)
             @param(animDesc Animation description)
             @param(position @bold([in, out]) Animation position in percent (between 0.0 and 1.0))
             @returns(@true if animation is running, otherwise @false)
            }
            function GetAnimPos(const pAnimationData: IAnimationData;
                    const pAnimDesc: TWSVGAnimationDescriptor; var position: Double): Boolean; virtual;

            {**
             Log the properties contained in an element
             @param(pElement The element for which the properties should be logged)
            }
            {$ifdef DEBUG}
                procedure LogProps(pElement: TWSVGElement); virtual;
            {$endif}

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Draw SVG on canvas
             @param(pSVG SVG to draw)
             @param(pos Draw position in pixels)
             @param(scale Scale factor)
             @param(antialiasing If @true, antialiasing should be used (if possible))
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pCanvas Canvas to draw on)
             @returns(@true on success, otherwise @false)
            }
            function Draw(const pSVG: TWSVG; const pos: TPoint; scale: Single; antialiasing: Boolean;
                    const animation: IAnimation; pCanvas: TCustomCanvas): Boolean; overload; virtual; abstract;

            {**
             Draw SVG on canvas
             @param(pSVG SVG to draw)
             @param(rect Rect in which svg will be drawn)
             @param(proportional If @true, svg proportions will be conserved)
             @param(antialiasing If @true, antialiasing should be used (if possible))
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pCanvas Canvas to draw on)
             @returns(@true on success, otherwise @false)
            }
            function Draw(const pSVG: TWSVG; const rect: TRect; proportional, antialiasing: Boolean;
                    const animation: IAnimation; pCanvas: TCustomCanvas): Boolean; overload; virtual; abstract;

            {**
             Get SVG size
             @param(pSVG SVG to measure)
             @returns(SVG size, empty size on error)
            }
            function GetSize(const pSVG: TWSVG): TSize; overload; virtual;

            {**
             Get SVG page style
             @param(pSVG SVG to get from)
             @param(pageColor @bold([out]) Page color (in other words it's the background color))
             @param(borderColor @bold([out]) Page border color)
             @param(borderOpacity @bold([out]) Page border opacity)
             @returns(@true on success, otherwise @false)
            }
            function GetPageStyle(const pSVG: TWSVG; out pageColor: TWColor; out borderColor: TWColor;
                    out borderOpacity: Single): Boolean; virtual;

            {**
             Get animation duration
             @param(pSVG SVG to get duration from)
             @returns(Animation duration in milliseconds)
            }
            function GetAnimationDuration(const pSVG: TWSVG): NativeUInt; overload; virtual;

            {**
             Enable or disable animation
             @param(value If @true, animation will be enabled, disabled otherwise)
            }
            procedure EnableAnimation(value: Boolean); virtual;

            {**
             Check if animation is enabled
             @returns(@true if animation is enabled, otherwise @false)
            }
            function IsAnimationEnabled: Boolean; virtual;

        public
            {**
             Get or set the OnAnimate event
            }
            property OnAnimate: ITfAnimateEvent read m_fOnAnimate write m_fOnAnimate;

            {**
             Get or set the OnGetImage event
            }
            property OnGetImage: ITfGetImageEvent read m_fGetImageEvent write m_fGetImageEvent;
    end;

implementation

{$if compilerversion > 24}
uses
  System.NetEncoding;
{$ifend}

//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropItem.Create(rule: IEPropRule);
begin
    inherited Create;

    m_Rule := rule;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropItem.Clear;
begin
    m_Rule := IE_PR_Inherit;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropItem.Assign(const pOther: IPropItem);
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    m_Rule := pOther.m_Rule;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropBoolItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropBoolItem.Create;
begin
    inherited Create;

    m_Value := False;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropBoolItem.Create(value: Boolean; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropBoolItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropBoolItem.Clear;
begin
    inherited Clear;

    m_Value := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropBoolItem.Assign(const pOther: IPropItem);
var
    pSource: IPropBoolItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropBoolItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropBoolItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropBoolItem.Merge(const pOther: IPropBoolItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            Rule    := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropFloatItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropFloatItem.Create;
begin
    inherited Create;

    m_Value := 0.0;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropFloatItem.Create(value: Single; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropFloatItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFloatItem.Clear;
begin
    inherited Clear;

    m_Value := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFloatItem.Assign(const pOther: IPropItem);
var
    pSource: IPropFloatItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropFloatItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropFloatItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFloatItem.Merge(const pOther: IPropFloatItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropColorItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropColorItem.Create;
begin
    inherited Create;

    m_Value     := TWColor.GetDefault;
    m_MergeMode := IE_MM_KeepThis;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropColorItem.Create(pValue: PWColor; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value.Assign(pValue^);
    m_MergeMode := IE_MM_KeepThis;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropColorItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropColorItem.Clear;
begin
    inherited Clear;

    m_Value     := TWColor.GetDefault;
    m_MergeMode := IE_MM_KeepThis;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropColorItem.Assign(const pOther: IPropItem);
var
    pSource: IPropColorItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropColorItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropColorItem;

    m_Value.Assign(pSource.m_Value);
    m_MergeMode := pSource.m_MergeMode;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IPropColorItem.GetValue: PWColor;
begin
    Result := @m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropColorItem.SetValue(const pValue: PWColor);
begin
    m_Value.Assign(pValue^);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropColorItem.Merge(const pOther: IPropColorItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value.Assign(pOther.m_Value);
            m_Rule := IE_PR_Default;
            Exit;
        end;

        IE_PR_Combine:
        begin
            case (m_MergeMode) of
                IE_MM_KeepParent:
                begin
                    m_Value := pOther.m_Value;
                    Exit;
                end;

                IE_MM_ParentColor:
                begin
                    // keep the parent color but retain the opacity
                    m_Value.SetColor(pOther.m_Value.GetRed, pOther.m_Value.GetGreen,
                            pOther.m_Value.GetBlue, m_Value.GetAlpha);
                    Exit;
                end;

                IE_MM_ParentOpacity:
                begin
                    // keep the parent opacity but retain the color
                    m_Value.SetColor(m_Value.GetRed, m_Value.GetGreen, m_Value.GetBlue,
                            pOther.m_Value.GetAlpha);
                    Exit;
                end;
            end;

            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropMatrixItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropMatrixItem.Create;
begin
    inherited Create;

    m_Value.SetIdentity;

    m_Type  := TWSVGPropMatrix.IEType.IE_Unknown;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropMatrixItem.Create(const pValue: PWMatrix3x3;
        matrixType: TWSVGPropMatrix.IEType; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value.Assign(pValue^);
    m_Type := matrixType;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropMatrixItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropMatrixItem.Clear;
begin
    inherited Clear;

    m_Type := TWSVGPropMatrix.IEType.IE_Unknown;
    m_Value.SetIdentity;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropMatrixItem.Assign(const pOther: IPropItem);
var
    pSource: IPropMatrixItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropMatrixItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropMatrixItem;
    m_Type  := pSource.m_Type;
    m_Value.Assign(pSource.m_Value);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IPropMatrixItem.GetValue: PWMatrix3x3;
begin
    Result := @m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropMatrixItem.SetValue(const pValue: PWMatrix3x3);
begin
    m_Value.Assign(pValue^);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropMatrixItem.Merge(const pOther: IPropMatrixItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value.Assign(pOther.m_Value);
            m_Rule := IE_PR_Default;
            Exit;
        end;

        IE_PR_Combine:
        begin
            m_Value := m_Value.Multiply(pOther.m_Value);
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropFillRuleItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropFillRuleItem.Create;
begin
    inherited Create;

    m_Value := TWSVGFill.IERule.IE_FR_Default;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropFillRuleItem.Create(value: TWSVGFill.IERule; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropFillRuleItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFillRuleItem.Clear;
begin
    inherited Clear;

    m_Value := TWSVGFill.IERule.IE_FR_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFillRuleItem.Assign(const pOther: IPropItem);
var
    pSource: IPropFillRuleItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropFillRuleItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropFillRuleItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropFillRuleItem.Merge(const pOther: IPropFillRuleItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropDashPatternItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropDashPatternItem.Create;
begin
    inherited Create;

    m_pValue := TWDashPattern.Create;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropDashPatternItem.Create(pValue: TWDashPattern; rule: IEPropRule);
begin
    inherited Create(rule);

    m_pValue := TWDashPattern.Create;

    // copy the whole items from the source
    m_pValue.InsertRange(0, pValue);
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropDashPatternItem.Destroy;
begin
    m_pValue.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDashPatternItem.Clear;
begin
    inherited Clear;

    m_pValue.Clear;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDashPatternItem.Assign(const pOther: IPropItem);
var
    pSource: IPropDashPatternItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropDashPatternItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropDashPatternItem;

    // clear previous content and copy the whole items from the source
    m_pValue.Clear;
    m_pValue.InsertRange(0, pSource.m_pValue);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDashPatternItem.Merge(const pOther: IPropDashPatternItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Rule := IE_PR_Default;

            // clear previous content and copy the whole items from the source
            m_pValue.Clear;
            m_pValue.InsertRange(0, pOther.m_pValue);
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropStrokeLineCapItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropStrokeLineCapItem.Create;
begin
    inherited Create;

    m_Value := TWSVGStroke.IELineCap.IE_LC_Default;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropStrokeLineCapItem.Create(value: TWSVGStroke.IELineCap;
        rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropStrokeLineCapItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineCapItem.Clear;
begin
    inherited Clear;

    m_Value := TWSVGStroke.IELineCap.IE_LC_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineCapItem.Assign(const pOther: IPropItem);
var
    pSource: IPropStrokeLineCapItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropStrokeLineCapItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropStrokeLineCapItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineCapItem.Merge(const pOther: IPropStrokeLineCapItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropStrokeLineJoinItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropStrokeLineJoinItem.Create;
begin
    inherited Create;

    m_Value := TWSVGStroke.IELineJoin.IE_LJ_Default;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropStrokeLineJoinItem.Create(value: TWSVGStroke.IELineJoin;
        rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropStrokeLineJoinItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineJoinItem.Clear;
begin
    inherited Clear;

    m_Value := TWSVGStroke.IELineJoin.IE_LJ_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineJoinItem.Assign(const pOther: IPropItem);
var
    pSource: IPropStrokeLineJoinItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropStrokeLineJoinItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropStrokeLineJoinItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropStrokeLineJoinItem.Merge(const pOther: IPropStrokeLineJoinItem);
begin
    case (m_Rule) of
        IE_PR_Default:
            Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropDisplayItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropDisplayItem.Create;
begin
    inherited Create;

    m_Value := C_SVG_Default_Display;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropDisplayItem.Create(value: TWSVGStyle.IPropDisplay.IEValue; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropDisplayItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDisplayItem.Clear;
begin
    inherited Clear;

    m_Value := C_SVG_Default_Display;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDisplayItem.Assign(const pOther: IPropItem);
var
    pSource: IPropDisplayItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropDisplayItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropDisplayItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropDisplayItem.Merge(const pOther: IPropDisplayItem);
begin
    case (m_Rule) of
        IE_PR_Default:
            Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropVisibilityItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropVisibilityItem.Create;
begin
    inherited Create;

    m_Value := C_SVG_Default_Visibility;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropVisibilityItem.Create(value: TWSVGStyle.IPropVisibility.IEValue; rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropVisibilityItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropVisibilityItem.Clear;
begin
    inherited Clear;

    m_Value := C_SVG_Default_Visibility;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropVisibilityItem.Assign(const pOther: IPropItem);
var
    pSource: IPropVisibilityItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropVisibilityItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropVisibilityItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropVisibilityItem.Merge(const pOther: IPropVisibilityItem);
begin
    case (m_Rule) of
        IE_PR_Default:
            Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IGradientStop
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IGradientStop.Create;
begin
    inherited Create;

    m_Color  := TWColor.GetDefault;
    m_Offset := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IGradientStop.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IGradientStop.GetColor: PWColor;
begin
    Result := @m_Color;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGradientStop.SetColor(const pColor: PWColor);
begin
    m_Color.Assign(pColor^);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGradientStop.Clear;
begin
    m_Color  := TWColor.GetDefault;
    m_Offset := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGradientStop.Assign(const pOther: IGradientStop);
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    m_Color.Assign(pOther.m_Color);
    m_Offset := pOther.m_Offset;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IGradient
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IGradient.Create(rule: IEPropRule);
begin
    inherited Create(rule);

    m_pGradientStops := IGradientStops.Create;
    m_Unit           := TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox;
    m_SpreadMethod   := TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad;
    m_Matrix         := TWMatrix3x3.GetDefault;
    m_MatrixType     := TWSVGPropMatrix.IEType.IE_Unknown;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IGradient.Destroy;
begin
    m_pGradientStops.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IGradient.GetMatrix: PWMatrix3x3;
begin
    Result := @m_Matrix;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGradient.Default;
begin
    // clear all gradient stops. NOTE as a TObjectList is used, the list will take care of freeing
    // all objects it contains, for that an explicit Free() on each item isn't required
    m_pGradientStops.Clear;

    m_Unit         := TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox;
    m_SpreadMethod := TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad;
    m_Matrix       := TWMatrix3x3.GetDefault;
    m_MatrixType   := TWSVGPropMatrix.IEType.IE_Unknown;
    m_Rule         := IE_PR_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGradient.Merge(const pOther: IGradient);
var
    pStop, pNewStop: IGradientStop;
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Unit         := pOther.m_Unit;
            m_SpreadMethod := pOther.m_SpreadMethod;
            m_MatrixType   := pOther.m_MatrixType;

            m_Matrix.Assign(pOther.m_Matrix);

            // clear all gradient stops. NOTE as a TObjectList is used, the list will take care of
            // freeing all objects it contains, for that an explicit Free() on each item isn't
            // required
            m_pGradientStops.Clear;

            // copy gradient stops from source
            for pStop in m_pGradientStops do
            begin
                pNewStop := nil;

                try
                    pNewStop := IGradientStop.Create;
                    pNewStop.Assign(pStop);

                    m_pGradientStops.Add(pNewStop);
                    pNewStop := nil;
                finally
                    pNewStop.Free;
                end;
            end;

            m_Rule := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.ILinearGradient
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.ILinearGradient.Create(rule: IEPropRule);
begin
    inherited Create(rule);

    m_Vector.m_Start.X := 0.0;
    m_Vector.m_Start.Y := 0.0;
    m_Vector.m_End.X   := 0.0;
    m_Vector.m_End.Y   := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.ILinearGradient.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.ILinearGradient.GetVector: IPLinearGradientVector;
begin
    Result := @m_Vector;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.ILinearGradient.SetVector(const pVector: IPLinearGradientVector);
begin
    if (not Assigned(pVector)) then
        Exit;

    m_Vector.m_Start.X := pVector.m_Start.X;
    m_Vector.m_Start.Y := pVector.m_Start.Y;
    m_Vector.m_End.X   := pVector.m_End.X;
    m_Vector.m_End.Y   := pVector.m_End.Y;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.ILinearGradient.Default;
begin
    inherited Default;

    m_Vector.m_Start.X := 0.0;
    m_Vector.m_Start.Y := 0.0;
    m_Vector.m_End.X   := 0.0;
    m_Vector.m_End.Y   := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.ILinearGradient.Merge(const pOther: IGradient);
var
    pSource: ILinearGradient;
begin
    if (not(pOther is ILinearGradient)) then
    begin
        Default;
        Exit;
    end;

    pSource := pOther as ILinearGradient;
    Assert(Assigned(pSource));

    inherited Merge(pOther);

    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Vector.m_Start.X := pSource.m_Vector.m_Start.X;
            m_Vector.m_Start.Y := pSource.m_Vector.m_Start.Y;
            m_Vector.m_End.X   := pSource.m_Vector.m_End.X;
            m_Vector.m_End.Y   := pSource.m_Vector.m_End.Y;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IRadialGradient
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IRadialGradient.Create(rule: IEPropRule);
begin
    inherited Create(rule);

    m_CX := 0.0;
    m_CY := 0.0;
    m_R  := 0.0;
    m_FX := 0.0;
    m_FY := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IRadialGradient.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IRadialGradient.Default;
begin
    inherited Default;

    m_CX := 0.0;
    m_CY := 0.0;
    m_R  := 0.0;
    m_FX := 0.0;
    m_FY := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IRadialGradient.Merge(const pOther: IGradient);
var
    pSource: IRadialGradient;
begin
    if (not(pOther is IRadialGradient)) then
    begin
        Default;
        Exit;
    end;

    pSource := pOther as IRadialGradient;
    Assert(Assigned(pSource));

    inherited Merge(pOther);

    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_CX := pSource.m_CX;
            m_CY := pSource.m_CY;
            m_R  := pSource.m_R;
            m_FX := pSource.m_FX;
            m_FY := pSource.m_FY;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IBrush
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IBrush.Create;
begin
    inherited Create;

    m_pColor          := IPropColorItem.Create;
    m_pLinearGradient := ILinearGradient.Create;
    m_pRadialGradient := IRadialGradient.Create;
    m_Type            := E_BT_Solid;
    m_Rule            := IE_PR_Inherit;

    // by default solid color is empty
    m_pColor.Value.Clear;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IBrush.Destroy;
begin
    m_pColor.Free;
    m_pLinearGradient.Free;
    m_pRadialGradient.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IBrush.Default;
var
    color: TWColor;
begin
    m_pColor.Free;

    m_pLinearGradient.Default;
    m_pRadialGradient.Default;

    color    := TWColor.Create(clBlack);
    m_pColor := IPropColorItem.Create(@color, IE_PR_Default);
    m_Type   := E_BT_Solid;
    m_Rule   := IE_PR_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IBrush.Merge(const pOther: IBrush);
begin
    case (m_Rule) of
        IE_PR_Inherit:
        begin
            m_Type := pOther.m_Type;

            case (m_Type) of
                E_BT_Solid:  m_pColor.Merge(pOther.Color);
                E_BT_Linear: m_pLinearGradient.Merge(pOther.LinearGradient);
                E_BT_Radial: m_pRadialGradient.Merge(pOther.RadialGradient);
            end;

            m_Rule := IE_PR_Default;
            Exit;
        end;

        IE_PR_Default: Exit;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IBrush.IsEmpty: Boolean;
var
    pStop: IGradientStop;
begin
    case m_Type of
        E_BT_Solid: Exit(m_pColor.m_Value.A = 0.0);

        E_BT_Linear:
        begin
            // check if at least one color is set
            for pStop in m_pLinearGradient.m_pGradientStops do
                if (pStop.m_Color.A > 0.0) then
                    Exit(False);

            Exit(True);
        end;

        E_BT_Radial:
        begin
            // check if at least one color is set
            for pStop in m_pRadialGradient.m_pGradientStops do
                if (pStop.m_Color.A > 0.0) then
                    Exit(False);

            Exit(True);
        end;
    else
        Result := True;
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IFill
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IFill.Create;
begin
    inherited Create;

    m_pBrush        := IBrush.Create;
    m_pRule         := IPropFillRuleItem.Create;
    m_pNoFill       := IPropBoolItem.Create;
    m_pNoFill.Value := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IFill.Destroy;
begin
    m_pBrush.Free;
    m_pRule.Free;
    m_pNoFill.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IFill.Default;
begin
    m_pRule.Free;
    m_pNoFill.Free;

    m_pRule   := IPropFillRuleItem.Create(TWSVGFill.IERule.IE_FR_NonZero, IE_PR_Default);
    m_pNoFill := IPropBoolItem.Create    (False,                          IE_PR_Default);

    m_pBrush.Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IFill.Merge(const pOther: IFill);
begin
    m_pBrush.Merge(pOther.m_pBrush);
    m_pRule.Merge(pOther.m_pRule);
    m_pNoFill.Merge(pOther.m_pNoFill);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IFill.IsEmpty: Boolean;
begin
    Result := m_pBrush.IsEmpty;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IStroke
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IStroke.Create;
begin
    inherited Create;

    m_pBrush          := IBrush.Create;
    m_pDashPattern    := IPropDashPatternItem.Create;
    m_pDashOffset     := IPropFloatItem.Create;
    m_pWidth          := IPropFloatItem.Create;
    m_pMiterLimit     := IPropFloatItem.Create;
    m_pLineCap        := IPropStrokeLineCapItem.Create;
    m_pLineJoin       := IPropStrokeLineJoinItem.Create;
    m_pNoStroke       := IPropBoolItem.Create;
    m_pNoStroke.Value := True;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IStroke.Destroy;
begin
    m_pBrush.Free;
    m_pDashPattern.Free;
    m_pDashOffset.Free;
    m_pWidth.Free;
    m_pMiterLimit.Free;
    m_pLineCap.Free;
    m_pLineJoin.Free;
    m_pNoStroke.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IStroke.Default;
begin
    m_pDashOffset.Free;
    m_pWidth.Free;
    m_pMiterLimit.Free;
    m_pLineCap.Free;
    m_pLineJoin.Free;
    m_pNoStroke.Free;

    m_pDashOffset := IPropFloatItem.Create         (0.0,                                IE_PR_Default);
    m_pWidth      := IPropFloatItem.Create         (1.0,                                IE_PR_Default);
    m_pMiterLimit := IPropFloatItem.Create         (4.0,                                IE_PR_Default);
    m_pLineCap    := IPropStrokeLineCapItem.Create (TWSVGStroke.IELineCap.IE_LC_Butt,   IE_PR_Default);
    m_pLineJoin   := IPropStrokeLineJoinItem.Create(TWSVGStroke.IELineJoin.IE_LJ_Miter, IE_PR_Default);
    m_pNoStroke   := IPropBoolItem.Create          (True,                               IE_PR_Default);

    m_pDashPattern.m_pValue.Clear;
    m_pDashPattern.m_Rule := IE_PR_Default;

    m_pBrush.Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IStroke.Merge(const pOther: IStroke);
begin
    m_pBrush.Merge(pOther.m_pBrush);
    m_pDashPattern.Merge(pOther.m_pDashPattern);
    m_pDashOffset.Merge(pOther.m_pDashOffset);
    m_pWidth.Merge(pOther.m_pWidth);
    m_pMiterLimit.Merge(pOther.m_pMiterLimit);
    m_pLineCap.Merge(pOther.m_pLineCap);
    m_pLineJoin.Merge(pOther.m_pLineJoin);
    m_pNoStroke.Merge(pOther.m_pNoStroke);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IStroke.IsEmpty: Boolean;
begin
    Result := m_pBrush.IsEmpty or (m_pWidth.m_Value = 0);
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IEffect
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IEffect.Create(rule: IEPropRule = IE_PR_Inherit);
begin
    inherited Create(rule);
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IEffect.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IGaussianBlur
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IGaussianBlur.Create(rule: IEPropRule = IE_PR_Inherit);
begin
    inherited Create(rule);

    m_Deviation := System.Default(TSize);
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IGaussianBlur.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGaussianBlur.Default;
begin
    m_Deviation := System.Default(TSize);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IGaussianBlur.Merge(const pOther: IEffect);
var
    pSource: IGaussianBlur;
begin
    if (not(pOther is IGaussianBlur)) then
    begin
        Default;
        Exit;
    end;

    pSource := pOther as IGaussianBlur;
    Assert(Assigned(pSource));

    case (m_Rule) of
        IE_PR_Inherit:
        begin
            m_Deviation := pSource.m_Deviation;
            Exit;
        end;

        IE_PR_Default: Exit;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IFilter
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IFilter.Create;
begin
    inherited Create;

    m_pX       := IPropFloatItem.Create;
    m_pY       := IPropFloatItem.Create;
    m_pWidth   := IPropFloatItem.Create;
    m_pHeight  := IPropFloatItem.Create;
    m_pPercent := IPropBoolItem.Create(False);
    m_pEffects := IEffects.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IFilter.Destroy;
begin
    m_pX.Free;
    m_pY.Free;
    m_pWidth.Free;
    m_pHeight.Free;
    m_pPercent.Free;

    // clear all effects. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pEffects.Clear;
    m_pEffects.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IFilter.Default;
var
    pEffect: IEffect;
begin
    m_pX.Free;
    m_pY.Free;
    m_pWidth.Free;
    m_pHeight.Free;
    m_pPercent.Free;

    // default pos and size are expressed in percent, and are 10% higher around the whole canvas
    m_pX       := IPropFloatItem.Create(-0.1,  IE_PR_Default);
    m_pY       := IPropFloatItem.Create(-0.1,  IE_PR_Default);
    m_pWidth   := IPropFloatItem.Create( 1.2,  IE_PR_Default);
    m_pHeight  := IPropFloatItem.Create( 1.2,  IE_PR_Default);
    m_pPercent := IPropBoolItem.Create ( True, IE_PR_Default);

    // reset the effects
    for pEffect in m_pEffects do
        pEffect.Default();
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IFilter.Merge(const pOther: IFilter);
{
var
    pEffect: IEffect;
}
begin
    m_pX.Merge(pOther.m_pX);
    m_pY.Merge(pOther.m_pY);
    m_pWidth.Merge(pOther.m_pWidth);
    m_pHeight.Merge(pOther.m_pHeight);
    m_pPercent.Merge(pOther.m_pPercent);

    // todo -cFeature -oJean: Don't know if effects should be merged, and if yes, how to do that
    //                        (because the source may have a completely different effect list)
    {
    // merge the effects
    for pEffect in m_pEffects do
        pEffect.Merge();
    }
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IStyle
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IStyle.Create;
begin
    inherited Create;

    m_pFill        := IFill.Create;
    m_pStroke      := IStroke.Create;
    m_pFilter      := IFilter.Create;
    m_pOpacity     := IPropFloatItem.Create;
    m_pDisplayMode := IPropDisplayItem.Create;
    m_pVisibility  := IPropVisibilityItem.Create;
    m_Width        := 0;
    m_Height       := 0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IStyle.Destroy;
begin
    m_pFill.Free;
    m_pStroke.Free;
    m_pFilter.Free;
    m_pOpacity.Free;
    m_pDisplayMode.Free;
    m_pVisibility.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IStyle.Default;
begin
    m_pFill.Default;
    m_pStroke.Default;
    m_pFilter.Default;

    m_pOpacity.Free;
    m_pOpacity := IPropFloatItem.Create(1.0, IE_PR_Default);

    m_pDisplayMode.Free;
    m_pDisplayMode := IPropDisplayItem.Create(C_SVG_Default_Display, IE_PR_Default);

    m_pVisibility.Free;
    m_pVisibility := IPropVisibilityItem.Create(C_SVG_Default_Visibility, IE_PR_Default);

    m_Width  := 0;
    m_Height := 0;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IStyle.Merge(const pOther: IStyle);
begin
    m_pFill.Merge(pOther.m_pFill);
    m_pStroke.Merge(pOther.m_pStroke);
    m_pFilter.Merge(pOther.m_pFilter);
    m_pOpacity.Merge(pOther.m_pOpacity);

    // the size should never change between groups, so simply copy it
    m_Width  := pOther.m_Width;
    m_Height := pOther.m_Height;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IStyle.GetFillColor: TWColor;
var
    alpha: Single;
begin
    if (m_pFill.m_pNoFill.m_Value) then
        Exit(System.Default(TWColor));

    alpha := m_pFill.m_pBrush.m_pColor.m_Value.GetAlpha();

    Result := TWColor.Create(m_pFill.m_pBrush.m_pColor.m_Value.GetRed,
            m_pFill.m_pBrush.m_pColor.m_Value.GetGreen,
            m_pFill.m_pBrush.m_pColor.m_Value.GetBlue,
            Min(255, Round(m_pOpacity.m_Value * alpha)));
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IStyle.GetStrokeColor: TWColor;
var
    alpha: Single;
begin
    if (m_pStroke.m_pNoStroke.m_Value) then
        Exit(System.Default(TWColor));

    alpha := m_pStroke.m_pBrush.m_pColor.m_Value.GetAlpha();

    Result := TWColor.Create(m_pStroke.m_pBrush.m_pColor.m_Value.GetRed,
            m_pStroke.m_pBrush.m_pColor.m_Value.GetGreen,
            m_pStroke.m_pBrush.m_pColor.m_Value.GetBlue,
            Min(255, Round(m_pOpacity.m_Value * alpha)));
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropAspectRatioItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropAspectRatioItem.Create;
begin
    inherited Create;

    m_Value := TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMid;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropAspectRatioItem.Create(value: TWSVGPropAspectRatio.IEAspectRatio;
        rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropAspectRatioItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioItem.Clear;
begin
    inherited Clear;

    m_Value := TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMid;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioItem.Assign(const pOther: IPropItem);
var
    pSource: IPropAspectRatioItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropAspectRatioItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropAspectRatioItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioItem.Merge(const pOther: IPropAspectRatioItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IPropAspectRatioRefItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropAspectRatioRefItem.Create;
begin
    inherited Create;

    m_Value := TWSVGPropAspectRatio.IEReference.IE_R_Meet;
end;
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IPropAspectRatioRefItem.Create(value: TWSVGPropAspectRatio.IEReference;
        rule: IEPropRule);
begin
    inherited Create(rule);

    m_Value := value;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IPropAspectRatioRefItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioRefItem.Clear;
begin
    inherited Clear;

    m_Value := TWSVGPropAspectRatio.IEReference.IE_R_Meet;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioRefItem.Assign(const pOther: IPropItem);
var
    pSource: IPropAspectRatioRefItem;
begin
    inherited Assign(pOther);

    if (not(pOther is IPropAspectRatioRefItem)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as IPropAspectRatioRefItem;
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IPropAspectRatioRefItem.Merge(const pOther: IPropAspectRatioRefItem);
begin
    case (m_Rule) of
        IE_PR_Default: Exit;

        IE_PR_Inherit:
        begin
            m_Value := pOther.m_Value;
            m_Rule  := IE_PR_Default;
            Exit;
        end;
    else
        raise Exception.CreateFmt('Merge items - unknown rule - %d', [Integer(m_Rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IAspectRatio
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IAspectRatio.Create;
begin
    inherited Create;

    m_pAspectRatio := IPropAspectRatioItem.Create;
    m_pReference   := IPropAspectRatioRefItem.Create;
    m_pDefined     := IPropBoolItem.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IAspectRatio.Destroy;
begin
    m_pAspectRatio.Free;
    m_pReference.Free;
    m_pDefined.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IAspectRatio.Default;
begin
    m_pAspectRatio.Free;
    m_pReference.Free;
    m_pDefined.Free;

    m_pAspectRatio := IPropAspectRatioItem.Create   (TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMid, IE_PR_Default);
    m_pReference   := IPropAspectRatioRefItem.Create(TWSVGPropAspectRatio.IEReference.IE_R_Meet,        IE_PR_Default);
    m_pDefined     := IPropBoolItem.Create          (False,                                             IE_PR_Default);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IAspectRatio.Merge(const pOther: IAspectRatio);
begin
    m_pAspectRatio.Merge(pOther.m_pAspectRatio);
    m_pReference.Merge(pOther.m_pReference);
    m_pDefined.Merge(pOther.m_pDefined);
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IProperties
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IProperties.Create;
begin
    inherited Create;

    m_pStyle       := IStyle.Create;
    m_pMatrix      := IPropMatrixItem.Create;
    m_pAspectRatio := IAspectRatio.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IProperties.Destroy;
begin
    m_pStyle.Free;
    m_pMatrix.Free;
    m_pAspectRatio.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IProperties.Default;
begin
    m_pStyle.Default;
    m_pAspectRatio.Default;

    m_pMatrix.m_Value.SetIdentity;
    m_pMatrix.m_Rule := IE_PR_Default;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.IProperties.Merge(const pOther: IProperties);
begin
    m_pStyle.Merge(pOther.m_pStyle);
    m_pMatrix.Merge(pOther.m_pMatrix);
    m_pAspectRatio.Merge(pOther.m_pAspectRatio);
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IAnimationData
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IAnimationData.Create;
begin
    inherited Create;

    m_pSetAnims     := TList<TWSVGAnimation>.Create;
    m_pAttribAnims  := TList<TWSVGAnimation>.Create;
    m_pColorAnims   := TList<TWSVGAnimation>.Create;
    m_pMatrixAnims  := TList<TWSVGAnimation>.Create;
    m_pUnknownAnims := TList<TWSVGAnimation>.Create;
    m_Position      := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IAnimationData.Destroy;
begin
    m_pSetAnims.Free;
    m_pAttribAnims.Free;
    m_pColorAnims.Free;
    m_pMatrixAnims.Free;
    m_pUnknownAnims.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.IAnimCacheItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.IAnimCacheItem.Create;
begin
    inherited Create;

    m_ClearingValue := 0.0;
    m_LastPos       := 0.0;
    m_Started       := False;
    m_Ended         := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.IAnimCacheItem.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer.ICacheItem
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.ICacheItem.Create;
begin
    inherited Create;

    m_pAnimCache   := IAnimCache.Create([doOwnsValues]);
    m_AnimDuration := 0;
    m_LastPos      := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.ICacheItem.Destroy;
begin
    m_pAnimCache.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGRasterizer
//---------------------------------------------------------------------------
constructor TWSVGRasterizer.Create;
begin
    inherited Create;

    m_pCache         := ICache.Create([doOwnsValues]);
    m_Animate        := True;
    m_fOnAnimate     := nil;
    m_fGetImageEvent := nil;
end;
//---------------------------------------------------------------------------
destructor TWSVGRasterizer.Destroy;
begin
    m_pCache.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.LocalPosToGlobalPos(animDuration: NativeUInt; pAnimItem: IAnimCacheItem;
        const animationData: IAnimationData; const pAnimDesc: TWSVGAnimationDescriptor): Double;
var
    localDuration:                                               NativeUInt;
    localPos, lastPos, finalPos, clearingVal, animDur, localDur: Double;
begin
    // is local duration indefinite?
    if (pAnimDesc.Duration.IsEmpty) then
        Exit(animationData.m_Position);

    // calculate local duration in milliseconds
    localDuration := pAnimDesc.Duration.ToMilliseconds;

    // is local duration equals to total duration?
    if (localDuration = animDuration) then
        Exit(animationData.m_Position);

    // convert animation and local duration to double values
    animDur  := animDuration;
    localDur := localDuration;

    // calculate local position (can be higher than 100%)
    localPos := ((animationData.m_Position * animDur) / localDur);

    // is local duration a multiple of total duration?
    if ((animDuration mod localDuration) = 0) then
        // can bound local duration between 0.0 and 1.0 without compensation in this case
        Exit(TWMathHelper.ExtMod(localPos, 1.0));

    // get cached values to use
    clearingVal := pAnimItem.m_ClearingValue;
    lastPos     := pAnimItem.m_LastPos;

    // calculate final position
    finalPos := clearingVal + localPos;

    // animation was looped? (NOTE rounded to 4 digits after the dot because rounding errors could
    // bias the calculation, and thus create jerks during the animation)
    if (RoundTo(finalPos, -4) < RoundTo(lastPos, -4)) then
    begin
        // recalculate new clearing value and final position
        clearingVal := TWMathHelper.ExtMod(clearingVal + (animDur / localDur), 1.0);
        finalPos    := clearingVal + localPos;
    end;

    // update values in cache
    pAnimItem.m_ClearingValue := clearingVal;
    pAnimItem.m_LastPos       := finalPos;

    Result := TWMathHelper.ExtMod(finalPos, 1.0);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetLinkedElement(const pLink: TWSVGPropLink): TWSVGElement;
var
    pDefsTable: TWSVGDefsTable;
    pItem:      TWSVGItem;
begin
    if (not Assigned(pLink)) then
        Exit(nil);

    pDefsTable := pLink.DefsTable;

    if (not Assigned(pDefsTable)) then
        Exit(nil);

    // each define should be unique in the map
    if (not pDefsTable.TryGetValue(pLink.Value, pItem)) then
    begin
        TWLogHelper.LogToCompiler('Get linked element - FAILED - not found - ' + pLink.Value);
        Exit(nil);
    end;

    if (not(pItem is TWSVGElement)) then
    begin
        TWLogHelper.LogToCompiler('Get linked element - FAILED - item is not an element - ' + pItem.ItemID);
        Exit(nil);
    end;

    Result := pItem as TWSVGElement
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.Initialize(const pSVG: TWSVG);
var
    pItem: ICacheItem;
begin
    // get current SVG UUID instance
    m_UUID := pSVG.GetUUID;

    // no instance?
    if (Length(m_UUID) = 0) then
        Exit;

    // is SVG already cached?
    if (m_pCache.ContainsKey(m_UUID)) then
        Exit;

    pItem := nil;

    try
        // create and populate new cache item
        pItem                := ICacheItem.Create;
        pItem.m_AnimDuration := GetAnimationDuration(pSVG);
        m_pCache.Add(m_UUID, pItem);
        pItem                := nil;
    finally
        pItem.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.SetAnimatedOpacityCombineMode(pColorItem: IPropColorItem);
begin
    case (pColorItem.m_Rule) of
        IE_PR_Inherit:
        begin
            // if the color is inherited, only the color should be copied from parent but NOT the opacity
            pColorItem.m_Rule      := IE_PR_Combine;
            pColorItem.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentColor;
            Exit;
        end;

        IE_PR_Combine:
        begin
            // if the color is inherited, search for combination mode that may erase the opacity, and
            // correct them to the appropriate value
            case (pColorItem.m_MergeMode) of
                TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_KeepThis,
                TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentColor:
                    Exit;

                TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_KeepParent:
                begin
                    pColorItem.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentColor;
                    Exit;
                end;

                TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentOpacity:
                begin
                    pColorItem.m_Rule      := IE_PR_Default;
                    pColorItem.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_KeepThis;
                    Exit;
                end;
            end;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetSize(const pHeader: TWSVGParser.IHeader): TSize;
var
    width, height:   Single;
    propCount, i:    NativeInt;
    pProperty:       TWSVGProperty;
    pWidth, pHeight: TWSVGMeasure<Single>;
    pStyle:          TWSVGStyle;
    pProperties:     IWSmartPointer<IProperties>;
    pAnimationData:  IWSmartPointer<IAnimationData>;
begin
    // initialize values in case header doesn't contain them
    width  := 0.0;
    height := 0.0;

    propCount := pHeader.Count;

    // iterate through header properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pHeader.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for header size
        if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get SVG width
            pWidth := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pWidth)) then
                continue;

            // set SVG width
            width := pWidth.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get SVG height
            pHeight := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pHeight)) then
                continue;

            // set SVG height
            height := pHeight.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Style) and (pProperty is TWSVGStyle)) then
        begin
            // get style
            pStyle := pProperty as TWSVGStyle;

            // found it?
            if (not Assigned(pStyle)) then
                continue;

            pProperties    := TWSmartPointer<IProperties>.Create();
            pAnimationData := TWSmartPointer<IAnimationData>.Create();

            // read properties from style
            if (GetStyleProps(pStyle, pProperties, pAnimationData, nil)) then
            begin
                if (pProperties.m_pStyle.m_Width <> 0) then
                    width := pProperties.m_pStyle.m_Width;

                if (pProperties.m_pStyle.m_Height <> 0) then
                    height := pProperties.m_pStyle.m_Height;
            end;
        end;
    end;

    // get source size
    Result := TSize.Create(Trunc(width), Trunc(height));
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetViewBox(const pHeader: TWSVGParser.IHeader): TWRectF;
var
    propCount, i: NativeInt;
    pProperty:    TWSVGProperty;
    pViewBox:     TWSVGPropRect;
    size:         TSize;
begin
    // no header?
    if (not Assigned(pHeader)) then
        Exit(Default(TWRectF));

    propCount := pHeader.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pHeader.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // is viewbox element?
        if ((pProperty.ItemName = C_SVG_Prop_ViewBox) and (pProperty is TWSVGPropRect)) then
        begin
            // get viewbox rect
            pViewBox := pProperty as TWSVGPropRect;

            // found it?
            if (not Assigned(pViewBox)) then
                Exit(Default(TWRectF));

            // viewbox exists but is empty?
            if ((pViewBox.X = 0.0) and (pViewBox.Y = 0.0) and (pViewBox.Width = 0.0)
                    and (pViewBox.Height = 0.0))
            then
            begin
                // use the SVG size instead
                size := GetSize(pHeader);

                Exit(TWRectF.Create(0, 0, size.Width, size.Height));
            end;

            // convert to GDI+ rect and return result
            Exit(TWRectF.Create(pViewBox.X, pViewBox.Y, pViewBox.X + pViewBox.Width,
                    pViewBox.Y + pViewBox.Height));
        end;
    end;

    // no viewbox defined, use the SVG size instead
    size := GetSize(pHeader);

    Result := TWRectF.Create(0, 0, size.Width, size.Height);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetPosAndSizeProps(const pElement: TWSVGElement; out x: Single; out y: Single;
        out width: Single; out height:Single; out viewBox: TWRectF; pAnimationData: IAnimationData;
        pCustomData: Pointer): Boolean;
var
    pProperty:               TWSVGProperty;
    pX, pY, pWidth, pHeight: TWSVGMeasure<Single>;
    pViewBox:                TWSVGPropRect;
    pAnimation:              TWSVGAnimation;
    pValueAnimDesc:          IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:              UnicodeString;
    propCount, i:            NativeInt;
    position, animPos:       Double;
begin
    if (not Assigned(pElement)) then
        Exit(False);

    // set default values (in case no matching value is found in rect)
    x       := 0.0;
    y       := 0.0;
    width   := 0.0;
    height  := 0.0;
    viewBox := Default(TWRectF);

    propCount := pElement.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pElement.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for property to get
        if ((pProperty.ItemName = C_SVG_Prop_X) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pX := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX)) then
                continue;

            // set x position
            x := pX.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pY := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY)) then
                continue;

            // set y position
            y := pY.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get width
            pWidth := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pWidth)) then
                continue;

            // set width
            width := pWidth.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get height
            pHeight := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pHeight)) then
                continue;

            // set height
            height := pHeight.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_ViewBox) and (pProperty is TWSVGPropRect)) then
        begin
            // get view box
            pViewBox := pProperty as TWSVGPropRect;

            // found it?
            if (not Assigned(pViewBox)) then
                continue;

            // set view box
            viewBox.Left   := pViewBox.X;
            viewBox.Top    := pViewBox.Y;
            viewBox.Right  := pViewBox.X + pViewBox.Width;
            viewBox.Bottom := pViewBox.Y + pViewBox.Height;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

     // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the rect x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the rect y position
                    y := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the rect width
                    width := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the rect height
                    height := pValueAnimDesc.ToList[0]
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the rect x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the rect y position
                    y := animPos
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the rect width
                    width := animPos
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the rect height
                    height := animPos
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetRectProps(const pRect: TWSVGRect; out x: Single; out y: Single;
        out width: Single; out height: Single; out rx: Single; out ry: Single;
        pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:                         TWSVGProperty;
    pX, pY, pWidth, pHeight, pRX, pRY: TWSVGMeasure<Single>;
    pAnimation:                        TWSVGAnimation;
    pValueAnimDesc:                    IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:                        UnicodeString;
    propCount, i:                      NativeInt;
    position, animPos:                 Double;
begin
    if (not Assigned(pRect)) then
        Exit(False);

    // set default values (in case no matching value is found in rect)
    x      := 0.0;
    y      := 0.0;
    width  := 0.0;
    height := 0.0;
    rx     := 0.0;
    ry     := 0.0;

    propCount := pRect.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pRect.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for rect property to get
        if ((pProperty.ItemName = C_SVG_Prop_X) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pX := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX)) then
                continue;

            // set x position
            x := pX.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pY := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY)) then
                continue;

            // set y position
            y := pY.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get width
            pWidth := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pWidth)) then
                continue;

            // set width
            width := pWidth.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get height
            pHeight := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pHeight)) then
                continue;

            // set height
            height := pHeight.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_RX) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x radius
            pRx := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pRx)) then
                continue;

            // set x radius
            rx := pRx.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_RY) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y radius
            pRy := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pRy)) then
                continue;

            // set y radius
            ry := pRy.Value.Value;
        end;
    end;

    // both values are equals if rx or ry is not set and other value is set
    if ((ry <> 0.0) and (rx = 0.0)) then
        rx := ry
    else
    if ((rx <> 0.0) and (ry = 0.0)) then
        ry := rx;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the rect x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the rect y position
                    y := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the rect width
                    width := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the rect height
                    height := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_RX) then
                    // do modify the rect corner x radius
                    rx := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_RY) then
                    // do modify the rect corner y radius
                    ry := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the rect x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the rect y position
                    y := animPos
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the rect width
                    width := animPos
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the rect height
                    height := animPos
                else
                if (attribName = C_SVG_Prop_RX) then
                    // do modify the rect corner x radius
                    rx := animPos
                else
                if (attribName = C_SVG_Prop_RY) then
                    // do modify the rect corner y radius
                    ry := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetCircleProps(const pCircle: TWSVGCircle; out x: Single; out y: Single;
        out radius: Single; pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:         TWSVGProperty;
    pCx, pCy, pR:      TWSVGMeasure<Single>;
    pAnimation:        TWSVGAnimation;
    pValueAnimDesc:    IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:        UnicodeString;
    propCount, i:      NativeInt;
    position, animPos: Double;
begin
    if (not Assigned(pCircle)) then
        Exit(False);

    // set default values (in case no matching value is found in circle)
    x      := 0.0;
    y      := 0.0;
    radius := 0.0;

    propCount := pCircle.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pCircle.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for circle property to get
        if ((pProperty.ItemName = C_SVG_Prop_CX) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pCx := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pCx)) then
                continue;

            // set x position
            x := pCx.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_CY) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pCy := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pCy)) then
                continue;

            // set y position
            y := pCy.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_R) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get radius
            pR := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pR)) then
                continue;

            // set radius
            radius := pR.Value.Value;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_CX) then
                    // do modify the circle x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_CY) then
                    // do modify the circle y position
                    y := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_R) then
                    // do modify the circle radius
                    radius := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_CX) then
                    // do modify the circle x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_CY) then
                    // do modify the circle y position
                    y := animPos
                else
                if (attribName = C_SVG_Prop_R) then
                    // do modify the circle radius
                    radius := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetEllipseProps(const pEllipse: TWSVGEllipse; out x: Single; out y: Single;
        out rx: Single; out ry: Single; pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:          TWSVGProperty;
    pCx, pCy, pRx, pRy: TWSVGMeasure<Single>;
    pAnimation:         TWSVGAnimation;
    pValueAnimDesc:     IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:         UnicodeString;
    propCount, i:       NativeInt;
    position, animPos:  Double;
begin
    if (not Assigned(pEllipse)) then
        Exit(False);

    // set default values (in case no matching value is found in ellipse)
    x  := 0.0;
    y  := 0.0;
    rx := 0.0;
    ry := 0.0;

    propCount := pEllipse.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pEllipse.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for ellipse property to get
        if ((pProperty.ItemName = C_SVG_Prop_CX) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pCx := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pCx)) then
                continue;

            // set x position
            x := pCx.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_CY) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pCy := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pCy)) then
                continue;

            // set y position
            y := pCy.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_RX) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x radius
            pRx := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pRx)) then
                continue;

            // set x radius
            rx := pRx.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_RY) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y radius
            pRy := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pRy)) then
                continue;

            // set y radius
            ry := pRy.Value.Value;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_CX) then
                    // do modify the ellipse x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_CY) then
                    // do modify the ellipse y position
                    y := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_RX) then
                    // do modify the ellipse x radius
                    rx := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_RY) then
                    // do modify the ellipse y radius
                    ry := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_CX) then
                    // do modify the ellipse x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_CY) then
                    // do modify the ellipse y position
                    y := animPos
                else
                if (attribName = C_SVG_Prop_RX) then
                    // do modify the ellipse x radius
                    rx := animPos
                else
                if (attribName = C_SVG_Prop_RY) then
                    // do modify the ellipse y radius
                    ry := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetLineProps(const pLine: TWSVGLine; out x1: Single; out y1: Single;
        out x2: Single; out y2: Single; pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:          TWSVGProperty;
    pX1, pY1, pX2, pY2: TWSVGMeasure<Single>;
    pAnimation:         TWSVGAnimation;
    pValueAnimDesc:     IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:         UnicodeString;
    propCount, i:       NativeInt;
    position, animPos:  Double;
begin
    if (not Assigned(pLine)) then
        Exit(False);

    // set default values (in case no matching value is found in line)
    x1 := 0.0;
    y1 := 0.0;
    x2 := 0.0;
    y2 := 0.0;

    propCount := pLine.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pLine.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for line property to get
        if ((pProperty.ItemName = C_SVG_Prop_X1) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get start x position
            pX1 := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX1)) then
                continue;

            // set start x position
            x1 := pX1.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y1) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get start y position
            pY1 := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY1)) then
                continue;

            // set start y position
            y1 := pY1.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_X2) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get end x position
            pX2 := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX2)) then
                continue;

            // set end x position
            x2 := pX2.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y2) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get end y position
            pY2 := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY2)) then
                continue;

            // set end y position
            y2 := pY2.Value.Value;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X1) then
                    // do modify the line x1 position
                    x1 := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y1) then
                    // do modify the line y1 position
                    y1 := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_X2) then
                    // do modify the line x2 position
                    x2 := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y2) then
                    // do modify the line y2 position
                    y2 := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X1) then
                    // do modify the line x1 position
                    x1 := animPos
                else
                if (attribName = C_SVG_Prop_Y1) then
                    // do modify the line y1 position
                    y1 := animPos
                else
                if (attribName = C_SVG_Prop_X2) then
                    // do modify the line x2 position
                    x2 := animPos
                else
                if (attribName = C_SVG_Prop_Y2) then
                    // do modify the line y2 position
                    y2 := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetImageProps(const pImage: TWSVGImage; out x: Single; out y: Single;
        out width: Single; out height: Single; out viewBox: TWRectF; out imageType: IEImageType;
        pImageData: TMemoryStream; pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:               TWSVGProperty;
    pX, pY, pWidth, pHeight: TWSVGMeasure<Single>;
    pViewBox:                TWSVGPropRect;
    pLink:                   TWSVGPropLink;
    pAnimation:              TWSVGAnimation;
    pValueAnimDesc:          IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:              UnicodeString;
    propCount, i:            NativeInt;
    position, animPos:       Double;
    pBytes:                  TBytes;
begin
    if (not Assigned(pImage)) then
        Exit(False);

    if (not Assigned(pImageData)) then
        Exit(False);

    // set default values (in case no matching value is found in text)
    x         := 0.0;
    y         := 0.0;
    width     := 0.0;
    height    := 0.0;
    viewBox   := Default(TWRectF);
    imageType := IE_IT_Unknown;

    propCount := pImage.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pImage.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for text property to get
        if ((pProperty.ItemName = C_SVG_Prop_X) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pX := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX)) then
                continue;

            // set x position
            x := pX.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pY := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY)) then
                continue;

            // set y position
            y := pY.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get image width
            pWidth := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pWidth)) then
                continue;

            // set image width
            width := pWidth.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get image height
            pHeight := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pHeight)) then
                continue;

            // set image height
            height := pHeight.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_ViewBox) and (pProperty is TWSVGPropRect)) then
        begin
            // get view box
            pViewBox := pProperty as TWSVGPropRect;

            // found it?
            if (not Assigned(pViewBox)) then
                continue;

            // set view box
            viewBox.Left   := pViewBox.X;
            viewBox.Top    := pViewBox.Y;
            viewBox.Right  := pViewBox.X + pViewBox.Width;
            viewBox.Bottom := pViewBox.Y + pViewBox.Height;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_XLink_HRef) and (pProperty is TWSVGPropLink)) then
        begin
            // get link
            pLink := pProperty as TWSVGPropLink;

            // found it?
            if (not Assigned(pLink)) then
                continue;

            // empty value?
            if (Length(pLink.Value) = 0) then
                continue;

            // search for encoding
            case pLink.Encoding of
                TWSVGPropLink.IEEncoding.IE_E_Base64:
                begin
                    pBytes := DecodeBase64(AnsiString(pLink.Value));

                    if (Assigned(pBytes)) then
                    begin
                        pImageData.Write(pBytes[0], Length(pBytes));
                        pImageData.Position := 0;
                    end;
                end;
            else
                TWLogHelper.LogToCompiler('Get image - unknown or unsupported encoding - ' +
                        IntToStr(Integer(pLink.Encoding)));
                continue;
            end;

            // search for encoding
            case pLink.DataType of
                TWSVGPropLink.IEDataType.IE_DT_PNG: imageType := IE_IT_PNG;
                TWSVGPropLink.IEDataType.IE_DT_JPG: imageType := IE_IT_JPG;
                TWSVGPropLink.IEDataType.IE_DT_SVG: imageType := IE_IT_SVG;
            else
                TWLogHelper.LogToCompiler('Get image - unknown image encodingtype - ' +
                        IntToStr(Integer(pLink.DataType)));
            end;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the image x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the image y position
                    y := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the image width
                    width := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the image height
                    height := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the image x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the image y position
                    y := animPos
                else
                if (attribName = C_SVG_Prop_Width) then
                    // do modify the image width
                    width := animPos
                else
                if (attribName = C_SVG_Prop_Height) then
                    // do modify the image height
                    height := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetTextProps(const pText: TWSVGText; out x: Single; out y: Single;
        out fontFamily: UnicodeString; out fontSize: Single; out fontWeight: Cardinal;
        out bolder: Boolean; out lighter: Boolean; out fontStyle: TWSVGText.IEFontStyle;
        out fontStyleAngle: Single; out anchor: IETextAnchor; out decoration: IETextDecoration;
        pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:         TWSVGProperty;
    pX, pY, pFontSize: TWSVGMeasure<Single>;
    pFontFamily:       TWSVGPropText;
    pFontWeight:       TWSVGText.IFontWeight;
    pFontStyle:        TWSVGText.IFontStyle;
    pAnchor:           TWSVGText.IAnchor;
    pDecoration:       TWSVGText.iDecoration;
    pAnimation:        TWSVGAnimation;
    pValueAnimDesc:    IWSmartPointer<TWSVGValueAnimDesc>;
    attribName:        UnicodeString;
    propCount, i:      NativeInt;
    position, animPos: Double;
begin
    if (not Assigned(pText)) then
        Exit(False);

    // set default values (in case no matching value is found in text)
    x              := 0.0;
    y              := 0.0;
    fontFamily     := 'Arial';
    fontSize       := 0.0;
    fontWeight     := 400;
    bolder         := False;
    lighter        := False;
    fontStyle      := TWSVGText.IEFontStyle.IE_FS_Normal;
    fontStyleAngle := 0.244; // 14°
    anchor         := IE_TA_Start;
    decoration     := IE_TD_Normal;

    propCount := pText.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pText.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for text property to get
        if ((pProperty.ItemName = C_SVG_Prop_X) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get x position
            pX := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pX)) then
                continue;

            // set x position
            x := pX.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Y) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get y position
            pY := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pY)) then
                continue;

            // set y position
            y := pY.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Font_Family) and (pProperty is TWSVGPropText)) then
        begin
            // get font family
            pFontFamily := pProperty as TWSVGPropText;

            // found it?
            if (not Assigned(pFontFamily)) then
                continue;

            // set font family
            fontFamily := pFontFamily.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Font_Size) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get font size
            pFontSize := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pFontSize)) then
                continue;

            // set font size
            fontSize := pFontSize.Value.Value;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Font_Weight) and (pProperty is TWSVGText.IFontWeight)) then
        begin
            // get font weight
            pFontWeight := pProperty as TWSVGText.IFontWeight;

            // found it?
            if (not Assigned(pFontWeight)) then
                continue;

            // set font weight properties
            fontWeight := pFontWeight.Value;
            bolder     := pFontWeight.Bolder;
            lighter    := pFontWeight.Lighter;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Font_Style) and (pProperty is TWSVGText.IFontStyle)) then
        begin
            // get font style
            pFontStyle := pProperty as TWSVGText.IFontStyle;

            // found it?
            if (not Assigned(pFontStyle)) then
                continue;

            // set font weight properties
            fontStyle      := pFontStyle.Style;
            fontStyleAngle := pFontStyle.Angle;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Text_Anchor) and (pProperty is TWSVGText.IAnchor)) then
        begin
            // get text anchor
            pAnchor := pProperty as TWSVGText.IAnchor;

            // found it?
            if (not Assigned(pAnchor)) then
                continue;

            // set text anchor
            case (pAnchor.Anchor) of
                TWSVGText.IEAnchor.IE_TA_Start:  anchor := IE_TA_Start;
                TWSVGText.IEAnchor.IE_TA_Middle: anchor := IE_TA_Middle;
                TWSVGText.IEAnchor.IE_TA_End:    anchor := IE_TA_End;
            else
                raise Exception.CreateFmt('Unknown text anchor value - %d', [Integer(pAnchor.Anchor)]);
            end;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Text_Decoration) and (pProperty is TWSVGText.IDecoration)) then
        begin
            // get text decoration
            pDecoration := pProperty as TWSVGText.IDecoration;

            // found it?
            if (not Assigned(pDecoration)) then
                continue;

            // set text decoration
            case (pDecoration.Value) of
                TWSVGText.IEDecoration.IE_D_Normal:      decoration := IE_TD_Normal;
                TWSVGText.IEDecoration.IE_D_Underline:   decoration := IE_TD_Underline;
                TWSVGText.IEDecoration.IE_D_LineThrough: decoration := IE_TD_LineThrough;
            else
                raise Exception.CreateFmt('Unknown text decoration value - %d', [Integer(pDecoration.Value)]);
            end;
        end;
    end;

    // do animate shape?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) = 0) then
                    continue;

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the text x position
                    x := pValueAnimDesc.ToList[0]
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the text y position
                    y := pValueAnimDesc.ToList[0];
            end;
        end;
    end;

    // iterate through attribute animations (i.e. animations linked to a particular shape attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
    begin
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_X) then
                    // do modify the text x position
                    x := animPos
                else
                if (attribName = C_SVG_Prop_Y) then
                    // do modify the text y position
                    y := animPos;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetElementProps(const pElement: TWSVGElement; pProperties: IProperties;
        pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:        TWSVGProperty;
    pStyle:           TWSVGStyle;
    pTransformMatrix: TWSVGPropMatrix;
    pPropMatrixItem:  IWSmartPointer<IPropMatrixItem>;
    pAspectRatio:     TWSVGPropAspectRatio;
    propCount, i:     NativeInt;
begin
    if (not Assigned(pElement)) then
        Exit(False);

    propCount := pElement.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pElement.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for element properties
        if ((pProperty.ItemName = C_SVG_Prop_Style) and (pProperty is TWSVGStyle)) then
        begin
            // get style
            pStyle := pProperty as TWSVGStyle;

            // found it?
            if (not Assigned(pStyle)) then
                continue;

            // read properties from style
            if (not GetStyleProps(pStyle, pProperties, pAnimationData, pCustomData)) then
                Exit(False);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Transform) and (pProperty is TWSVGPropMatrix)) then
        begin
            // get transform matrix
            pTransformMatrix := pProperty as TWSVGPropMatrix;

            // found it?
            if (not Assigned(pTransformMatrix)) then
                continue;

            // set matrix
            pPropMatrixItem := TWSmartPointer<IPropMatrixItem>.Create
                    (IPropMatrixItem.Create(pTransformMatrix.Matrix, pTransformMatrix.MatrixType,
                            IE_PR_Combine));
            pProperties.Matrix.Assign(pPropMatrixItem);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_PreserveAspectRatio) and (pProperty is TWSVGPropAspectRatio)) then
        begin
            // get aspect ratio
            pAspectRatio := pProperty as TWSVGPropAspectRatio;

            // found it?
            if (not Assigned(pAspectRatio)) then
                continue;

            // set image aspect ratio and reference
            pProperties.m_pAspectRatio.m_pAspectRatio.m_Value := pAspectRatio.AspectRatio;
            pProperties.m_pAspectRatio.m_pAspectRatio.m_Rule  := IE_PR_Default;
            pProperties.m_pAspectRatio.m_pReference.m_Value   := pAspectRatio.Reference;
            pProperties.m_pAspectRatio.m_pReference.m_Rule    := IE_PR_Default;
            pProperties.m_pAspectRatio.m_pDefined.m_Value     := True;
            pProperties.m_pAspectRatio.m_pDefined.m_Rule      := IE_PR_Default;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetStyleProps(const pStyle: TWSVGStyle; pProperties: IProperties;
        pAnimationData: IAnimationData; pCustomData: Pointer): Boolean;
var
    pProperty:                   TWSVGProperty;
    pMeasure:                    TWSVGMeasure<Single>;
    pValue:                      TWSVGAttribute<Single>;
    pColor:                      TWSVGPropColor;
    pLink:                       TWSVGPropLink;
    pDisplay:                    TWSVGStyle.IPropDisplay;
    pVisibility:                 TWSVGStyle.IPropVisibility;
    pFillRule:                   TWSVGFill.IPropRule;
    pLineCap:                    TWSVGStroke.IPropLineCap;
    pLineJoin:                   TWSVGStroke.IPropLineJoin;
    pAnimation:                  TWSVGAnimation;
    pValueAnimDesc:              IWSmartPointer<TWSVGValueAnimDesc>;
    pColorAnimDesc:              IWSmartPointer<TWSVGColorAnimDesc>;
    pEnumAnimDesc:               IWSmartPointer<TWSVGEnumAnimDesc>;
    attribName:                  UnicodeString;
    pPropFloatItem:              IWSmartPointer<IPropFloatItem>;
    pPropBoolItem:               IWSmartPointer<IPropBoolItem>;
    pPropFillRuleItem:           IWSmartPointer<IPropFillRuleItem>;
    color:                       TWColor;
    propCount, valueCount, i, j: NativeInt;
    position, animPos:           Double;
    foundColor, foundOpacity:    Boolean;
begin
    propCount := pStyle.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pStyle.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for style property to get
        if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get SVG width
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set SVG width
            pProperties.m_pStyle.m_Width := Round(pMeasure.Value.Value);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get SVG height
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set SVG height
            pProperties.m_pStyle.m_Height := Round(pMeasure.Value.Value);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Display) and (pProperty is TWSVGStyle.IPropDisplay)) then
        begin
            // get display mode
            pDisplay := pProperty as TWSVGStyle.IPropDisplay;

            // found it?
            if (not Assigned(pDisplay)) then
                continue;

            // set display mode
            if (pDisplay.Count = 0) then
            begin
                pProperties.m_pStyle.m_pDisplayMode.m_Value := C_SVG_Default_Display;
                pProperties.m_pStyle.m_pDisplayMode.m_Rule  := IE_PR_Default;
            end
            else
            begin
                // is explicitly tagged as inherited?
                if (pDisplay.Values[0] = Integer(TWSVGStyle.IPropDisplay.IEValue.IE_V_Inherit)) then
                    // do nothing (will be implicitly inherited in this case)
                    continue;

                pProperties.m_pStyle.m_pDisplayMode.m_Value := TWSVGStyle.IPropDisplay.IEValue(pDisplay.Values[0]);
                pProperties.m_pStyle.m_pDisplayMode.m_Rule  := IE_PR_Default;
            end
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Visibility) and (pProperty is TWSVGStyle.IPropVisibility)) then
        begin
            // get visibility
            pVisibility := pProperty as TWSVGStyle.IPropVisibility;

            // found it?
            if (not Assigned(pVisibility)) then
                continue;

            // set visibility
            if (pVisibility.Count = 0) then
            begin
                pProperties.m_pStyle.m_pVisibility.m_Value := C_SVG_Default_Visibility;
                pProperties.m_pStyle.m_pVisibility.m_Rule  := IE_PR_Default;
            end
            else
            begin
                pProperties.m_pStyle.m_pVisibility.m_Value := TWSVGStyle.IPropVisibility.IEValue(pVisibility.Values[0]);
                pProperties.m_pStyle.m_pVisibility.m_Rule  := IE_PR_Default;
            end
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Opacity) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get global opacity
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set global opacity
            pPropFloatItem := TWSmartPointer<IPropFloatItem>.Create
                    (IPropFloatItem.Create(pMeasure.Value.Value, IE_PR_Default));
            pProperties.m_pStyle.m_pOpacity.Assign(pPropFloatItem);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Filter) and (pProperty is TWSVGPropLink)) then
        begin
            // get filter link
            pLink := pProperty as TWSVGPropLink;

            // found it?
            if (not Assigned(pLink)) then
                continue;

            // get filter
            if (not GetFilterFromLink(pLink, pProperties.m_pStyle.m_pFilter)) then
                TWLogHelper.LogToCompiler('Get style props - invalid filter - id - ' + pLink.Value);
        end;
    end;

    // check if fill should be ignored completely
    if (pStyle.Fill.NoFill) then
    begin
        pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create
                (IPropBoolItem.Create(pStyle.Fill.NoFill, IE_PR_Default));
        pProperties.m_pStyle.m_pFill.m_pNoFill.Assign(pPropBoolItem);
        pProperties.m_pStyle.m_pFill.m_pBrush.m_Type := E_BT_Solid;
    end;

    foundColor   := False;
    foundOpacity := False;

    propCount := pStyle.Fill.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pStyle.Fill.Properties[i];

        // search for style property to get
        if (pProperty.ItemName = C_SVG_Prop_Fill) then
        begin
            // a fill color was found, reset the "no fill" flag
            pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False, IE_PR_Default));
            pProperties.m_pStyle.m_pFill.m_pNoFill.Assign(pPropBoolItem);

            // is a solid fill color?
            if (pProperty is TWSVGPropColor) then
            begin
                // get solid fill color
                pColor := pProperty as TWSVGPropColor;

                // found it?
                if (Assigned(pColor)) then
                begin
                    // color has no values?
                    if (pColor.Count = 0) then
                        color.SetColor(C_SVG_Default_Color)
                    else
                        // get fill color to apply
                        color.Assign(pColor.Values[0]^);

                    // copy only RGB values (opacity is defined by another property)
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetRed(color.GetRed);
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetGreen(color.GetGreen);
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetBlue(color.GetBlue);
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_Type          := E_BT_Solid;

                    foundColor := True;
                    continue;
                end;
            end;

            // is a link?
            if (pProperty is TWSVGPropLink) then
            begin
                pLink := pProperty as TWSVGPropLink;

                // fill may use a gradient or a pattern, get it
                if (GetColorFromLink(pLink, pProperties.m_pStyle.m_pFill.m_pBrush)) then
                    continue;

                if (Assigned(pLink)) then
                begin
                    TWLogHelper.LogToCompiler('Get style props - invalid fill color link - id - '
                            + pLink.Value);
                    continue;
                end;
            end;

            TWLogHelper.LogToCompiler('Get style props - unknown fill color type - skipped');
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Fill_Opacity) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // change the no fill property in the destination style only if the source isn't
            // explicitly tagged as "no fill". This is because several SVG contains an opacity value
            // despite of the fill itself is explicitly tagged as none
            if (not pStyle.Fill.NoFill) then
            begin
                // a fill opacity was found, reset the "no fill" flag
                pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False, IE_PR_Default));
                pProperties.m_pStyle.m_pFill.m_pNoFill.Assign(pPropBoolItem);
            end;

            // get fill opacity
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set fill opacity
            pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pMeasure.Value.Value * 100.0));
            pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule := IE_PR_Default;

            foundOpacity := True;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Fill_Rule) and (pProperty is TWSVGFill.IPropRule)) then
        begin
            // get fill rule
            pFillRule := pProperty as TWSVGFill.IPropRule;

            // found it?
            if (not Assigned(pFillRule)) then
                continue;

            // set fill rule
            if (pFillRule.Rule = TWSVGFill.IERule.IE_FR_Default) then
                pPropFillRuleItem := TWSmartPointer<IPropFillRuleItem>.Create
                        (IPropFillRuleItem.Create(pFillRule.Rule, IE_PR_Inherit))
            else
                pPropFillRuleItem := TWSmartPointer<IPropFillRuleItem>.Create
                        (IPropFillRuleItem.Create(pFillRule.Rule, IE_PR_Default));

            pProperties.m_pStyle.m_pFill.m_pRule.Assign(pPropFillRuleItem);
        end;
    end;

    // found a fill color in the style but no explicit opacity was defined (or vice-versa)?
    if (foundColor and not foundOpacity) then
    begin
        // make color opaque, the inheritance may change that later. NOTE required, because by
        // default a fill color is fully transparent (i.e. set to none and inheriting from parent)
        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetAlpha(255);

        // set the opacity to inherit from parent
        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule      := IE_PR_Combine;
        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentOpacity;
    end
    else
    if (not foundColor and foundOpacity) then
    begin
        // found an opacity but no color. In this case, the color should be inherited from parent,
        // but NOT the opacity
        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule      := IE_PR_Combine;
        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentColor;
    end;

    // check if stroke should be ignored completely
    if (pStyle.Stroke.NoStroke) then
    begin
        pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create
                (IPropBoolItem.Create(pStyle.Stroke.NoStroke, IE_PR_Default));
        pProperties.m_pStyle.m_pStroke.m_pNoStroke.Assign(pPropBoolItem);
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_Type := E_BT_Solid;
    end;

    foundColor   := False;
    foundOpacity := False;

    propCount := pStyle.Stroke.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pStyle.Stroke.Properties[i];

        // search for style stroke property to get
        if (pProperty.ItemName = C_SVG_Prop_Stroke) then
        begin
            // a stroke color was found, reset the "no stroke" flag
            pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False, IE_PR_Default));
            pProperties.m_pStyle.m_pStroke.m_pNoStroke.Assign(pPropBoolItem);

            // is a solid stroke color?
            if (pProperty is TWSVGPropColor) then
            begin
                // get solid stroke color
                pColor := pProperty as TWSVGPropColor;

                // found it?
                if (Assigned(pColor)) then
                begin
                    // color has no values?
                    if (pColor.Count = 0) then
                        color.SetColor(C_SVG_Default_Color)
                    else
                        // get fill color to apply
                        color.Assign(pColor.Values[0]^);

                    // copy only RGB values (opacity is defined by another property)
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetRed(color.GetRed);
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetGreen(color.GetGreen);
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetBlue(color.GetBlue);
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_Type          := E_BT_Solid;

                    foundColor := True;
                    continue;
                end;
            end;

            // is a link?
            if (pProperty is TWSVGPropLink) then
            begin
                pLink := pProperty as TWSVGPropLink;

                // stroke may use a gradient or a pattern, get it
                if (GetColorFromLink(pLink, pProperties.m_pStyle.m_pStroke.m_pBrush)) then
                    continue;

                if (Assigned(pLink)) then
                begin
                    TWLogHelper.LogToCompiler('Get style props - invalid stroke color link - id - '
                            + pLink.Value);
                    continue;
                end;
            end;

            TWLogHelper.LogToCompiler('Get style props - unknown stroke color type - skipped');
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_Opacity) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // change the no stroke property in the destination style only if the source isn't
            // explicitly tagged as "no stroke". This is because several SVG contains an opacity
            // value despite of the stroke itself is explicitly tagged as none
            if (not pStyle.Stroke.NoStroke) then
            begin
                // a stroke opacity was found, reset the "no stroke" flag
                pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False, IE_PR_Default));
                pProperties.m_pStyle.m_pStroke.m_pNoStroke.Assign(pPropBoolItem);
            end;

            // get stroke opacity
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set stroke opacity
            pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pMeasure.Value.Value * 100.0));
            pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule := IE_PR_Default;

            foundOpacity := True;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_Width) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get stroke width
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set stroke width
            pProperties.m_pStyle.m_pStroke.m_pWidth.m_Value := pMeasure.Value.Value;
            pProperties.m_pStyle.m_pStroke.m_pWidth.m_Rule  := IE_PR_Default;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_DashArray) and (pProperty is TWSVGAttribute<Single>)) then
        begin
            // get stroke dash array
            pValue := pProperty as TWSVGAttribute<Single>;

            // found it?
            if (not Assigned(pValue)) then
                continue;

            // get dash value count
            valueCount := pValue.Count;

            // iterate through dash values and copy pattern to apply
            for j := 0 to valueCount - 1 do
                pProperties.m_pStyle.m_pStroke.m_pDashPattern.m_pValue.Add(pValue.Values[j]);

            pProperties.m_pStyle.m_pStroke.m_pDashPattern.m_Rule := IE_PR_Default;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_DashOffset) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get stroke dash offset
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set stroke dash offset
            pProperties.m_pStyle.m_pStroke.m_pDashOffset.m_Value := pMeasure.Value.Value;
            pProperties.m_pStyle.m_pStroke.m_pDashOffset.m_Rule  := IE_PR_Default;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_LineCap) and (pProperty is TWSVGStroke.IPropLineCap)) then
        begin
            // get stroke dash linecap
            pLineCap := pProperty as TWSVGStroke.IPropLineCap;

            // found it?
            if (not Assigned(pLineCap)) then
                continue;

            // set stroke linecap
            pProperties.m_pStyle.m_pStroke.m_pLineCap.m_Value := pLineCap.LineCap;
            pProperties.m_pStyle.m_pStroke.m_pLineCap.m_Rule  := IE_PR_Default;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_LineJoin) and (pProperty is TWSVGStroke.IPropLineJoin)) then
        begin
            // get stroke dash line join
            pLineJoin := pProperty as TWSVGStroke.IPropLineJoin;

            // found it?
            if (not Assigned(pLineJoin)) then
                continue;

            // set stroke line join
            pProperties.m_pStyle.m_pStroke.m_pLineJoin.m_Value := pLineJoin.LineJoin;
            pProperties.m_pStyle.m_pStroke.m_pLineJoin.m_Rule  := IE_PR_Default;
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Stroke_MiterLimit) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get stroke dash offset
            pMeasure := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pMeasure)) then
                continue;

            // set stroke miter limit
            pProperties.m_pStyle.m_pStroke.m_pMiterLimit.m_Value := pMeasure.Value.Value;
            pProperties.m_pStyle.m_pStroke.m_pMiterLimit.m_Rule  := IE_PR_Default;
        end;
    end;

    // found a stroke color in the style but no explicit opacity was defined (or vice-versa)?
    if (foundColor and not foundOpacity) then
    begin
        // make color opaque by default, the inheritance may change that later. NOTE required,
        // because by default a stroke color is fully transparent (i.e. set to none and inheriting
        // from parent)
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetAlpha(255);

        // set the opacity to inherit from parent
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule      := IE_PR_Combine;
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentOpacity;
    end
    else
    if (not foundColor and foundOpacity) then
    begin
        // found an opacity but no color. In this case, the color should be inherited from parent,
        // but NOT the opacity
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule      := IE_PR_Combine;
        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_MergeMode := TWSVGRasterizer.IPropColorItem.IEMergeMode.IE_MM_ParentColor;
    end;

    // do animate style?
    if (not m_Animate) then
        Exit(True);

    // iterate through set animations (i.e. set a particular style attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pValueAnimDesc.ToList) > 0) then
                    // search for attribute to modify
                    if (attribName = C_SVG_Prop_Stroke_Width) then
                        // do modify the stroke width
                        pProperties.m_pStyle.m_pStroke.m_pWidth.m_Value := pValueAnimDesc.ToList[0]
                    else
                    if (attribName = C_SVG_Prop_Fill_Opacity) then
                    begin
                        // todo -cFeature -oJean: gradient colors are not supported, do support them
                        // do modify the fill opacity
                        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pValueAnimDesc.ToList[0] * 100.0));
                        SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor);
                    end
                    else
                    if (attribName = C_SVG_Prop_Stroke_Opacity) then
                    begin
                        // todo -cFeature -oJean: gradient colors are not supported, do support them
                        // do modify the stroke opacity
                        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pValueAnimDesc.ToList[0] * 100.0));
                        SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor);
                    end
                    else
                    if (attribName = C_SVG_Prop_Opacity) then
                    begin
                        // todo -cFeature -oJean: gradient colors are not supported, do support them
                        // do modify the fill and stroke opacity
                        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pValueAnimDesc.ToList[0] * 100.0));
                        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(pValueAnimDesc.ToList[0] * 100.0));

                        SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor);
                        SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor);
                    end
                    else
                    if (attribName = C_SVG_Prop_Stroke_DashOffset) then
                        // do modify the stroke dash offset
                        pProperties.m_pStyle.m_pStroke.m_pDashOffset.m_Value := pValueAnimDesc.ToList[0];
            end;

            TWSVGCommon.IEValueType.IE_VT_Color:
            begin
                // configure animation description
                pColorAnimDesc           := TWSmartPointer<TWSVGColorAnimDesc>.Create();
                pColorAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pColorAnimDesc, True, pCustomData)) then
                    continue;

                // do apply animation?
                if (pAnimationData.m_Position <> 1.0) then
                    continue;

                // is to value defined?
                if (Length(pColorAnimDesc.ToList) > 0) then
                    // search for attribute to modify
                    if (attribName = C_SVG_Prop_Fill) then
                    begin
                        // do modify the fill color
                        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.Assign(pColorAnimDesc.ToList[0]);

                        // was the fill previously hidden?
                        if (pProperties.m_pStyle.m_pFill.m_pNoFill.m_Value
                                and not pProperties.m_pStyle.m_pFill.IsEmpty)
                        then
                        begin
                            // turn it visible
                            pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False,
                                    IE_PR_Default));
                            pProperties.m_pStyle.m_pFill.m_pNoFill.Assign(pPropBoolItem);
                            pProperties.m_pStyle.m_pFill.m_pBrush.m_Type := E_BT_Solid;
                        end;

                        // if inherited, change the rule to default, because the animated color will be used
                        if (pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule = IE_PR_Inherit) then
                            pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                    end
                    else
                    if (attribName = C_SVG_Prop_Stroke) then
                    begin
                        // do modify the stroke color
                        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.Assign(pColorAnimDesc.ToList[0]);

                        // was the stroke previously hidden?
                        if (pProperties.m_pStyle.m_pStroke.m_pNoStroke.m_Value
                                and not pProperties.m_pStyle.m_pStroke.IsEmpty)
                        then
                        begin
                            // turn it visible
                            pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False,
                                    IE_PR_Default));
                            pProperties.m_pStyle.m_pStroke.m_pNoStroke.Assign(pPropBoolItem);
                            pProperties.m_pStyle.m_pStroke.m_pBrush.m_Type := E_BT_Solid;
                        end;

                        // if inherited, change the rule to default, because the animated color will be used
                        if (pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule = IE_PR_Inherit) then
                            pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                    end;
            end;
        end;

    // iterate through attribute animations (i.e. animations linked to a particular style attribute)
    for pAnimation in pAnimationData.m_pAttribAnims do
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Value:
            begin
                // configure animation description
                pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
                pValueAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pValueAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pValueAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pValueAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_Stroke_Width) then
                    // do modify the stroke width
                    pProperties.m_pStyle.m_pStroke.m_pWidth.m_Value := animPos
                else
                if (attribName = C_SVG_Prop_Fill_Opacity) then
                begin
                    // todo -cFeature -oJean: gradient colors are not supported, do support them
                    // do modify the fill opacity
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(animPos * 100.0));
                    SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor);
                end
                else
                if (attribName = C_SVG_Prop_Stroke_Opacity) then
                begin
                    // todo -cFeature -oJean: gradient colors are not supported, do support them
                    // do modify the stroke opacity
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(animPos * 100.0));
                    SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor);
                end
                else
                if (attribName = C_SVG_Prop_Opacity) then
                begin
                    // todo -cFeature -oJean: gradient colors are not supported, do support them
                    // do modify the fill and stroke opacity
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(animPos * 100.0));
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.SetOpacity(Trunc(animPos * 100.0));

                    SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor);
                    SetAnimatedOpacityCombineMode(pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor);
                end
                else
                if (attribName = C_SVG_Prop_Stroke_DashOffset) then
                    // do modify the stroke dash offset
                    pProperties.m_pStyle.m_pStroke.m_pDashOffset.m_Value := animPos;
            end;

            TWSVGCommon.IEValueType.IE_VT_Enum:
            begin
                // configure animation description
                pEnumAnimDesc           := TWSmartPointer<TWSVGEnumAnimDesc>.Create();
                pEnumAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pEnumAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pEnumAnimDesc, position)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pEnumAnimDesc, position)) then
                    continue;

                // calculate animation position
                animPos := pEnumAnimDesc.GetValueAt(position);

                // search for attribute to modify
                if (attribName = C_SVG_Prop_Display) then
                begin
                    // do modify the display attribute
                    pProperties.m_pStyle.m_pDisplayMode.m_Value := TWSVGStyle.IPropDisplay.IEValue(Trunc(animPos));
                    pProperties.m_pStyle.m_pDisplayMode.m_Rule  := IE_PR_Default;
                end
                else
                if (attribName = C_SVG_Prop_Visibility) then
                begin
                    // do modify the visibility attribute
                    pProperties.m_pStyle.m_pVisibility.m_Value := TWSVGStyle.IPropVisibility.IEValue(Trunc(animPos));
                    pProperties.m_pStyle.m_pVisibility.m_Rule  := IE_PR_Default;
                end;

                break;
            end;
        end;

    // iterate through color animations
    for pAnimation in pAnimationData.m_pColorAnims do
        // search for animation type to apply
        case (pAnimation.ValueType) of
            TWSVGCommon.IEValueType.IE_VT_Color:
            begin
                // configure animation description
                pColorAnimDesc           := TWSmartPointer<TWSVGColorAnimDesc>.Create();
                pColorAnimDesc.Animation := pAnimation;

                // populate animation description, and check if animation is allowed to continue
                if (not PopulateAnimation(pAnimation, attribName, pColorAnimDesc, True, pCustomData)) then
                    continue;

                // is animation running (i.e. between its start and end position)
                if (not GetAnimPos(pAnimationData, pColorAnimDesc, position)) then
                    continue;

                // calculate color to apply
                color.Assign(pColorAnimDesc.GetValueAt(position));

                // search for attribute to modify
                if (attribName = C_SVG_Prop_Fill) then
                begin
                    // do modify the fill color
                    pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Value.Assign(color);

                    // was the fill previously hidden?
                    if (pProperties.m_pStyle.m_pFill.m_pNoFill.m_Value
                            and not pProperties.m_pStyle.m_pFill.IsEmpty)
                    then
                    begin
                        // turn it visible
                        pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False,
                                IE_PR_Default));
                        pProperties.m_pStyle.m_pFill.m_pNoFill.Assign(pPropBoolItem);
                        pProperties.m_pStyle.m_pFill.m_pBrush.m_Type := E_BT_Solid;
                    end;

                    // if inherited, change the rule to default, because the animated color will be used
                    if (pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule = IE_PR_Inherit) then
                        pProperties.m_pStyle.m_pFill.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                end
                else
                if (attribName = C_SVG_Prop_Stroke) then
                begin
                    // do modify the fill color
                    pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Value.Assign(color);

                    // was the stroke previously hidden?
                    if (pProperties.m_pStyle.m_pStroke.m_pNoStroke.m_Value
                            and not pProperties.m_pStyle.m_pStroke.IsEmpty)
                    then
                    begin
                        // turn it visible
                        pPropBoolItem := TWSmartPointer<IPropBoolItem>.Create(IPropBoolItem.Create(False,
                                IE_PR_Default));
                        pProperties.m_pStyle.m_pStroke.m_pNoStroke.Assign(pPropBoolItem);
                        pProperties.m_pStyle.m_pStroke.m_pBrush.m_Type := E_BT_Solid;
                    end;

                    // if inherited, change the rule to default, because the animated color will be used
                    if (pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule = IE_PR_Inherit) then
                        pProperties.m_pStyle.m_pStroke.m_pBrush.m_pColor.m_Rule := IE_PR_Default;
                end;
            end;
        end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetLinkedElementToUse(const pUse: TWSVGUse; out pElement: TWSVGElement): Boolean;
var
    pProperty:    TWSVGProperty;
    pLink:        TWSVGPropLink;
    propCount, i: NativeInt;
begin
    if (not Assigned(pUse)) then
        Exit(False);

    // set default values (in case no linked element is found)
    pElement := nil;

    propCount := pUse.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pUse.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for use property to get
        if ((pProperty.ItemName = C_SVG_Prop_HRef) and (pProperty is TWSVGPropLink)) then
        begin
            // get link
            pLink := pProperty as TWSVGPropLink;

            // found it?
            if (not Assigned(pLink)) then
                continue;

            // get the linked element to use
            pElement := GetLinkedElement(pLink);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_XLink_HRef) and (pProperty is TWSVGPropLink)) then
        begin
            // get link
            pLink := pProperty as TWSVGPropLink;

            // found it?
            if (not Assigned(pLink)) then
                continue;

            // get the linked element to use
            pElement := GetLinkedElement(pLink);
        end;
    end;

    Result := Assigned(pElement);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetClipPath(const pElement: TWSVGElement; out pClipPath: TWSVGClipPath): Boolean;
var
    pProperty:      TWSVGProperty;
    pLink:          TWSVGPropLink;
    pLinkedElement: TWSVGElement;
    propCount, i:   NativeInt;
begin
    if (not Assigned(pElement)) then
        Exit(False);

    // set default values (in case no matching value is found in circle)
    pClipPath := nil;

    propCount := pElement.Count;

    // iterate through element properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pElement.Properties[i];

        if (not Assigned(pProperty)) then
            continue;

        // search for clip path property to get
        if (pProperty.ItemName =  C_SVG_Prop_ClipPath) then
        begin
            if (not(pProperty is TWSVGPropLink)) then
                continue;

            // get link
            pLink := pProperty as TWSVGPropLink;

            // found it?
            if (not Assigned(pLink)) then
                continue;

            // get the linked element containing the clip path
            pLinkedElement := GetLinkedElement(pLink);

            // get the clip path
            if (pLinkedElement is TWSVGClipPath) then
                pClipPath := pLinkedElement as TWSVGClipPath;
        end;
    end;

    Result := Assigned(pClipPath);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetFilterFromLink(const pLink: TWSVGPropLink; pFilter: IFilter): Boolean;
var
    pProperty:                                   TWSVGProperty;
    pLinkedElement:                              TWSVGElement;
    pSVGFilter:                                  TWSVGFilter;
    pSVGEffect:                                  TWSVGEffect;
    pSVGGaussianBlur:                            TWSVGGaussianBlur;
    pMeasure:                                    TWSVGMeasure<Single>;
    pAttribute:                                  TWSVGAttribute<Single>;
    pGaussianBlur:                               IGaussianBlur;
    propCount, effectCount, valueCount, i, j, k: NativeInt;
begin
    pLinkedElement := GetLinkedElement(pLink);

    if (not Assigned(pLinkedElement)) then
        Exit(False);

    // search for linked element type
    if ((pLinkedElement.ItemName = C_SVG_Tag_Filter) and (pLinkedElement is TWSVGFilter)) then
    begin
        // convert to filter
        pSVGFilter := pLinkedElement as TWSVGFilter;

        // failed?
        if (not Assigned(pSVGFilter)) then
            Exit(False);

        propCount := pSVGFilter.Count;

        // iterate through filter properties
        for i := 0 to propCount - 1 do
        begin
            pProperty := pSVGFilter.Properties[i];

            // get filter property
            if ((pProperty.ItemName = C_SVG_Prop_X) and (pProperty is TWSVGMeasure<Single>)) then
            begin
                // get x position
                pMeasure := pProperty as TWSVGMeasure<Single>;

                // found it?
                if (not Assigned(pMeasure)) then
                    continue;

                // set x position
                if (pMeasure.MeasureUnit = IEUnit.IE_UN_Percent) then
                begin
                    pFilter.m_pX.Value       := pMeasure.Value.Value * 0.01;
                    pFilter.m_pX.Rule        := IE_PR_Default;
                    pFilter.m_pPercent.Value := True;
                    pFilter.m_pPercent.Rule  := IE_PR_Default;
                end
                else
                begin
                    pFilter.m_pX.Value := pMeasure.Value.Value;
                    pFilter.m_pX.Rule  := IE_PR_Default;

                    if (pFilter.m_pPercent.Value) then
                        TWLogHelper.LogToCompiler('Get filter - mixed percent and pixel values - ' + pSVGFilter.ItemID);
                end;
            end
            else
            if ((pProperty.ItemName = C_SVG_Prop_Y) and (pProperty is TWSVGMeasure<Single>)) then
            begin
                // get y position
                pMeasure := pProperty as TWSVGMeasure<Single>;

                // found it?
                if (not Assigned(pMeasure)) then
                    continue;

                // set y position
                if (pMeasure.MeasureUnit = IEUnit.IE_UN_Percent) then
                begin
                    pFilter.m_pY.Value       := pMeasure.Value.Value * 0.01;
                    pFilter.m_pY.Rule        := IE_PR_Default;
                    pFilter.m_pPercent.Value := True;
                    pFilter.m_pPercent.Rule  := IE_PR_Default;
                end
                else
                begin
                    pFilter.m_pY.Value := pMeasure.Value.Value;
                    pFilter.m_pY.Rule  := IE_PR_Default;

                    if (pFilter.m_pPercent.Value) then
                        TWLogHelper.LogToCompiler('Get filter - mixed percent and pixel values - ' + pSVGFilter.ItemID);
                end;
            end
            else
            if ((pProperty.ItemName = C_SVG_Prop_Width) and (pProperty is TWSVGMeasure<Single>)) then
            begin
                // get width
                pMeasure := pProperty as TWSVGMeasure<Single>;

                // found it?
                if (not Assigned(pMeasure)) then
                    continue;

                // set width
                if (pMeasure.MeasureUnit = IEUnit.IE_UN_Percent) then
                begin
                    pFilter.m_pWidth.Value   := pMeasure.Value.Value * 0.01;
                    pFilter.m_pWidth.Rule    := IE_PR_Default;
                    pFilter.m_pPercent.Value := True;
                    pFilter.m_pPercent.Rule  := IE_PR_Default;
                end
                else
                begin
                    pFilter.m_pWidth.Value := pMeasure.Value.Value;
                    pFilter.m_pWidth.Rule  := IE_PR_Default;

                    if (pFilter.m_pPercent.Value) then
                        TWLogHelper.LogToCompiler('Get filter - mixed percent and pixel values - ' + pSVGFilter.ItemID);
                end;
            end
            else
            if ((pProperty.ItemName = C_SVG_Prop_Height) and (pProperty is TWSVGMeasure<Single>)) then
            begin
                // get height
                pMeasure := pProperty as TWSVGMeasure<Single>;

                // found it?
                if (not Assigned(pMeasure)) then
                    continue;

                // set height
                if (pMeasure.MeasureUnit = IEUnit.IE_UN_Percent) then
                begin
                    pFilter.m_pHeight.Value  := pMeasure.Value.Value * 0.01;
                    pFilter.m_pHeight.Rule   := IE_PR_Default;
                    pFilter.m_pPercent.Value := True;
                    pFilter.m_pPercent.Rule  := IE_PR_Default;
                end
                else
                begin
                    pFilter.m_pHeight.Value := pMeasure.Value.Value;
                    pFilter.m_pHeight.Rule  := IE_PR_Default;

                    if (pFilter.m_pPercent.Value) then
                        TWLogHelper.LogToCompiler('Get filter - mixed percent and pixel values - ' + pSVGFilter.ItemID);
                end;
            end;
        end;

        effectCount := pSVGFilter.EffectCount;

        // iterate through filter effects
        for i := 0 to effectCount - 1 do
        begin
            pSVGEffect := pSVGFilter.Effects[i];

            // search for effect to create
            if ((pSVGEffect.ItemName = C_SVG_Filter_Gaussian_Blur) and (pSVGEffect is TWSVGGaussianBlur)) then
            begin
                // get gaussian blur effect
                pSVGGaussianBlur := pSVGEffect as TWSVGGaussianBlur;

                if (not Assigned(pSVGGaussianBlur)) then
                    continue;

                pGaussianBlur := nil;

                try
                    // create a gaussian blur effect
                    pGaussianBlur := IGaussianBlur.Create;
                    propCount     := pSVGEffect.Count;

                    // iterate through blur properties
                    for j := 0 to propCount - 1 do
                    begin
                        pProperty := pSVGEffect.Properties[j];

                        // get blur property
                        if ((pProperty.ItemName = C_SVG_Filter_STD_Deviation) and (pProperty is TWSVGAttribute<Single>)) then
                        begin
                            // get deviation
                            pAttribute := pProperty as TWSVGAttribute<Single>;

                            if (not Assigned(pAttribute)) then
                                continue;

                            valueCount := pAttribute.Count;

                            // set deviation
                            for k := 0 to valueCount - 1 do
                                case (k) of
                                    0:  pGaussianBlur.m_Deviation.Width  := Round(pAttribute.Values[k]);
                                    1:  pGaussianBlur.m_Deviation.Height := Round(pAttribute.Values[k]);
                                else
                                    raise Exception.CreateFmt('Too many blur deviation values - %d', [valueCount]);
                                end;
                        end;
                    end;

                    // add gaussian blur effect to list
                    pFilter.m_pEffects.Add(pGaussianBlur);
                    pGaussianBlur := nil;
                finally
                    pGaussianBlur.Free;
                end;
            end;
        end;

        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetColorFromLink(const pLink: TWSVGPropLink; pBrush: IBrush): Boolean;
var
    pProperty:                                    TWSVGProperty;
    pLinkedElement:                               TWSVGElement;
    pLinkedBrush:                                 IBrush;
    pGradient:                                    TWSVGGradient;
    pGradientUnit:                                TWSVGPropUnit;
    pGradientSpreadMethod:                        TWSVGGradient.IGradientSpreadMethod;
    pMatrix:                                      TWSVGPropMatrix;
    pX1, pX2, pY1, pY2, pCX, pCY, pR, pFX, pFY:   TWSVGMeasure<Single>;
    pExternalLink:                                TWSVGPropLink;
    pGradientStops:                               IGradientStops;
    pNewStop:                                     IGradientStop;
    linkedBrushCount, propCount, stopCount, i, j: NativeInt;
    linkedBrushes:                                array of IBrush;
begin
    {$if CompilerVersion >= 32}
        // completely stupid, because all the paths below return a value, however
        // Tokyo and higher raise this warning every time a continue instruction is
        // found in the function code. Also see this post:
        // http://www.delphigroups.info/2/c9/526281.html
        Result := False;
    {$ifend}

    pLinkedElement := GetLinkedElement(pLink);

    if (not Assigned(pLinkedElement)) then
        Exit(False);

    // search for linked element type
    if ((pLinkedElement.ItemName = C_SVG_Tag_Linear_Gradient) and (pLinkedElement is TWSVGLinearGradient)) then
    begin
        // convert to linear gradient
        pGradient := pLinkedElement as TWSVGLinearGradient;

        // failed?
        if (not Assigned(pGradient)) then
            Exit(False);

        try
            propCount := pGradient.Count;

            // iterate through element properties
            for i := 0 to propCount - 1 do
            begin
                pProperty := pGradient.Properties[i];

                // get gradient property
                if (((pProperty.ItemName = C_SVG_Prop_HRef) or (pProperty.ItemName = C_SVG_Prop_XLink_HRef))
                        and (pProperty is TWSVGPropLink))
                then
                begin
                    // get external link
                    pExternalLink := pProperty as TWSVGPropLink;

                    // found it?
                    if (not Assigned(pExternalLink)) then
                        continue;

                    pLinkedBrush := nil;

                    try
                        pLinkedBrush := IBrush.Create;

                        // get the linked brush. For a linear gradient, the linked object should
                        // always be a kind of gradient (linear, radial, ...)
                        if (not GetColorFromLink(pExternalLink, pLinkedBrush)) then
                            Exit(False);

                        SetLength(linkedBrushes, Length(linkedBrushes) + 1);
                        linkedBrushes[Length(linkedBrushes) - 1] := pLinkedBrush;
                        pLinkedBrush                             := nil;
                    finally
                        pLinkedBrush.Free;
                    end;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Units) and (pProperty is TWSVGPropUnit)) then
                begin
                    // get gradient unit
                    pGradientUnit := pProperty as TWSVGPropUnit;

                    // found it?
                    if (not Assigned(pGradientUnit)) then
                        continue;

                    pBrush.m_pLinearGradient.m_Unit := pGradientUnit.UnitType;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Spread_Method)
                        and (pProperty is TWSVGGradient.IGradientSpreadMethod))
                then
                begin
                    // get gradient spread method
                    pGradientSpreadMethod := pProperty as TWSVGGradient.IGradientSpreadMethod;

                    // found it?
                    if (not Assigned(pGradientSpreadMethod)) then
                        continue;

                    pBrush.m_pLinearGradient.m_SpreadMethod := pGradientSpreadMethod.Method;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_X1) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get x1 position
                    pX1 := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pX1)) then
                        continue;

                    if (pX1.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pLinearGradient.m_Vector.m_Start.X := pX1.Value.Value / 100.0
                    else
                        pBrush.m_pLinearGradient.m_Vector.m_Start.X := pX1.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_X2) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get x2 position
                    pX2 := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pX2)) then
                        continue;

                    if (pX2.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pLinearGradient.m_Vector.m_End.X := pX2.Value.Value / 100.0
                    else
                        pBrush.m_pLinearGradient.m_Vector.m_End.X := pX2.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_Y1) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get y1 position
                    pY1 := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pY1)) then
                        continue;

                    if (pY1.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pLinearGradient.m_Vector.m_Start.Y := pY1.Value.Value / 100.0
                    else
                        pBrush.m_pLinearGradient.m_Vector.m_Start.Y := pY1.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_Y2) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get y2 position
                    pY2 := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pY2)) then
                        continue;

                    if (pY2.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pLinearGradient.m_Vector.m_End.Y := pY2.Value.Value / 100.0
                    else
                        pBrush.m_pLinearGradient.m_Vector.m_End.Y := pY2.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Transform) and (pProperty is TWSVGPropMatrix)) then
                begin
                    // get transform matrix
                    pMatrix := pProperty as TWSVGPropMatrix;

                    // found it?
                    if (not Assigned(pMatrix)) then
                        continue;

                    pBrush.m_pLinearGradient.m_Matrix.Assign(pMatrix.Matrix^);
                    pBrush.m_pLinearGradient.m_MatrixType := pMatrix.MatrixType;
                end;
            end;

            // populate the gradient stop list
            GetGradientStops(pGradient, pBrush.m_pLinearGradient.m_pGradientStops);

            linkedBrushCount := Length(linkedBrushes);

            // iterate through brushes to link. Each brush to link may contain any kind of gradient,
            // the properties should be copied in the target gradient only if they match, and if the
            // value is unset in the target gradient. See:
            // https://www.w3.org/TR/SVG11/pservers.html#LinearGradientElementHrefAttribute
            for i := 0 to linkedBrushCount - 1 do
            begin
                pGradientStops := nil;

                // dispatch gradient type
                case (linkedBrushes[i].m_Type) of
                    E_BT_Linear:
                    begin
                        // get the gradient stop list to copy (see below)
                        pGradientStops := linkedBrushes[i].m_pLinearGradient.m_pGradientStops;

                        // copy start x parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Vector.m_Start.X = 0.0) then
                            pBrush.m_pLinearGradient.m_Vector.m_Start.X :=
                                    linkedBrushes[i].m_pLinearGradient.m_Vector.m_Start.X;

                        // copy start y parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Vector.m_Start.Y = 0.0) then
                            pBrush.m_pLinearGradient.m_Vector.m_Start.Y :=
                                    linkedBrushes[i].m_pLinearGradient.m_Vector.m_Start.Y;

                        // copy end x parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Vector.m_End.X = 0.0) then
                            pBrush.m_pLinearGradient.m_Vector.m_End.X :=
                                    linkedBrushes[i].m_pLinearGradient.m_Vector.m_End.X;

                        // copy end y parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Vector.m_End.Y = 0.0) then
                            pBrush.m_pLinearGradient.m_Vector.m_End.Y :=
                                    linkedBrushes[i].m_pLinearGradient.m_Vector.m_End.Y;

                        // copy matrix parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Matrix.IsIdentity) then
                        begin
                            pBrush.m_pLinearGradient.m_Matrix.Assign(linkedBrushes[i].m_pLinearGradient.m_Matrix);
                            pBrush.m_pLinearGradient.m_MatrixType := linkedBrushes[i].m_pLinearGradient.m_MatrixType;
                        end;

                        // copy unit parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Unit = TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox) then
                            pBrush.m_pLinearGradient.m_Unit := linkedBrushes[i].m_pLinearGradient.m_Unit;

                        // copy spread method parameter if undefined
                        if (pBrush.m_pLinearGradient.m_SpreadMethod = TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad) then
                            pBrush.m_pLinearGradient.m_SpreadMethod := linkedBrushes[i].m_pLinearGradient.m_SpreadMethod;
                    end;

                    E_BT_Radial:
                    begin
                        // get the gradient stop list to copy (see below)
                        pGradientStops := linkedBrushes[i].m_pRadialGradient.m_pGradientStops;

                        // copy matrix parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Matrix.IsIdentity) then
                        begin
                            pBrush.m_pLinearGradient.m_Matrix.Assign(linkedBrushes[i].m_pRadialGradient.m_Matrix);
                            pBrush.m_pLinearGradient.m_MatrixType := linkedBrushes[i].m_pRadialGradient.m_MatrixType;
                        end;

                        // copy unit parameter if undefined
                        if (pBrush.m_pLinearGradient.m_Unit = TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox) then
                            pBrush.m_pLinearGradient.m_Unit := linkedBrushes[i].m_pRadialGradient.m_Unit;

                        // copy spread method parameter if undefined
                        if (pBrush.m_pLinearGradient.m_SpreadMethod = TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad) then
                            pBrush.m_pLinearGradient.m_SpreadMethod := linkedBrushes[i].m_pRadialGradient.m_SpreadMethod;
                    end;
                end;

                // the target brush contains no gradient stops?
                if (Assigned(pGradientStops)) then
                    if (pBrush.m_pLinearGradient.m_pGradientStops.Count = 0) then
                    begin
                        stopCount := pGradientStops.Count;

                        // if the target gradient has no defined gradient stops, and the
                        // referenced element does, then the gradient stops will be copied from
                        // the reference
                        for j := 0 to stopCount - 1 do
                        begin
                            pNewStop := nil;

                            try
                                pNewStop := IGradientStop.Create;
                                pNewStop.Assign(pGradientStops[j]);

                                pBrush.m_pLinearGradient.m_pGradientStops.Add(pNewStop);
                                pNewStop := nil;
                            finally
                                pNewStop.Free;
                            end;
                        end;
                    end;
            end;
        finally
            linkedBrushCount := Length(linkedBrushes);

            for i := 0 to linkedBrushCount - 1 do
                linkedBrushes[i].Free;

            SetLength(linkedBrushes, 0);
        end;

        pBrush.m_Type                   :=  E_BT_Linear;
        pBrush.m_Rule                   := IE_PR_Default;
        pBrush.m_pLinearGradient.m_Rule := IE_PR_Default;
        Exit(True);
    end
    else
    if ((pLinkedElement.ItemName = C_SVG_Tag_Radial_Gradient) and (pLinkedElement is TWSVGRadialGradient)) then
    begin
        // convert to linear gradient
        pGradient := pLinkedElement as TWSVGRadialGradient;

        // failed?
        if (not Assigned(pGradient)) then
            Exit(False);

        try
            propCount := pGradient.Count;

            // iterate through element properties
            for i := 0 to propCount - 1 do
            begin
                pProperty := pGradient.Properties[i];

                // get gradient property
                if (((pProperty.ItemName = C_SVG_Prop_HRef) or (pProperty.ItemName = C_SVG_Prop_XLink_HRef))
                        and (pProperty is TWSVGPropLink))
                then
                begin
                    // get external link
                    pExternalLink := pProperty as TWSVGPropLink;

                    // found it?
                    if (not Assigned(pExternalLink)) then
                        continue;

                    pLinkedBrush := nil;

                    try
                        pLinkedBrush := IBrush.Create;

                        // get the linked brush. For a radial gradient, the linked object should
                        // always be a kind of gradient (linear, radial, ...)
                        if (not GetColorFromLink(pExternalLink, pLinkedBrush)) then
                            Exit(False);

                        SetLength(linkedBrushes, Length(linkedBrushes) + 1);
                        linkedBrushes[Length(linkedBrushes) - 1] := pLinkedBrush;
                        pLinkedBrush                             := nil;
                    finally
                        pLinkedBrush.Free;
                    end;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Units) and (pProperty is TWSVGPropUnit)) then
                begin
                    // get gradient unit
                    pGradientUnit := pProperty as TWSVGPropUnit;

                    // found it?
                    if (not Assigned(pGradientUnit)) then
                        continue;

                    pBrush.m_pRadialGradient.m_Unit := pGradientUnit.UnitType;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Spread_Method)
                        and (pProperty is TWSVGGradient.IGradientSpreadMethod))
                then
                begin
                    // get gradient spread method
                    pGradientSpreadMethod := pProperty as TWSVGGradient.IGradientSpreadMethod;

                    // found it?
                    if (not Assigned(pGradientSpreadMethod)) then
                        continue;

                    pBrush.m_pRadialGradient.m_SpreadMethod := pGradientSpreadMethod.Method;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_CX) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get cx position
                    pCX := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pCX)) then
                        continue;

                    // read value, convert it between 0.0f and 1.0f if expressed in percent
                    if (pCX.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pRadialGradient.m_CX := pCX.Value.Value / 100.0
                    else
                        pBrush.m_pRadialGradient.m_CX := pCX.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_CY) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get cy position
                    pCY := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pCY)) then
                        continue;

                    // read value, convert it between 0.0f and 1.0f if expressed in percent
                    if (pCY.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pRadialGradient.m_CY := pCY.Value.Value / 100.0
                    else
                        pBrush.m_pRadialGradient.m_CY := pCY.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_R) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get radius
                    pR := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pR)) then
                        continue;

                    // read value, convert it between 0.0f and 1.0f if expressed in percent
                    if (pR.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pRadialGradient.m_R := pR.Value.Value / 100.0
                    else
                        pBrush.m_pRadialGradient.m_R := pR.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_FX) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get focus x position
                    pFX := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pFX)) then
                        continue;

                    // read value, convert it between 0.0f and 1.0f if expressed in percent
                    if (pFX.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pRadialGradient.m_FX := pFX.Value.Value / 100.0
                    else
                        pBrush.m_pRadialGradient.m_FX := pFX.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Prop_FY) and (pProperty is TWSVGMeasure<Single>)) then
                begin
                    // get focus y position
                    pFY := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pFY)) then
                        continue;

                    // read value, convert it between 0.0f and 1.0f if expressed in percent
                    if (pFY.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pBrush.m_pRadialGradient.m_FY := pFY.Value.Value / 100.0
                    else
                        pBrush.m_pRadialGradient.m_FY := pFY.Value.Value;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Transform) and (pProperty is TWSVGPropMatrix)) then
                begin
                    // get transform matrix
                    pMatrix := pProperty as TWSVGPropMatrix;

                    // found it?
                    if (not Assigned(pMatrix)) then
                        continue;

                    pBrush.m_pRadialGradient.m_Matrix.Assign(pMatrix.Matrix^);
                    pBrush.m_pRadialGradient.m_MatrixType := pMatrix.MatrixType;
                end;
            end;

            // populate the gradient stop list
            GetGradientStops(pGradient, pBrush.m_pRadialGradient.m_pGradientStops);

            linkedBrushCount := Length(linkedBrushes);

            // iterate through brushes to link. Each brush to link may contain any kind of gradient,
            // the properties should be copied in the target gradient only if they match, and if the
            // value is unset in the target gradient. See:
            // https://www.w3.org/TR/SVG11/pservers.html#RadialGradientElementHrefAttribute
            for i := 0 to linkedBrushCount - 1 do
            begin
                pGradientStops := nil;

                // dispatch gradient type
                case (linkedBrushes[i].m_Type) of
                    E_BT_Linear:
                    begin
                        // get the gradient stop list to copy (see below)
                        pGradientStops := linkedBrushes[i].m_pLinearGradient.m_pGradientStops;

                        // copy matrix parameter if undefined
                        if (pBrush.m_pRadialGradient.m_Matrix.IsIdentity) then
                        begin
                            pBrush.m_pRadialGradient.m_Matrix.Assign(linkedBrushes[i].m_pLinearGradient.m_Matrix);
                            pBrush.m_pRadialGradient.m_MatrixType := linkedBrushes[i].m_pLinearGradient.m_MatrixType;
                        end;

                        // copy unit parameter if undefined
                        if (pBrush.m_pRadialGradient.m_Unit = TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox) then
                            pBrush.m_pRadialGradient.m_Unit := linkedBrushes[i].m_pLinearGradient.m_Unit;

                        // copy spread method parameter if undefined
                        if (pBrush.m_pRadialGradient.m_SpreadMethod = TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad) then
                            pBrush.m_pRadialGradient.m_SpreadMethod := linkedBrushes[i].m_pLinearGradient.m_SpreadMethod;
                    end;

                    E_BT_Radial:
                    begin
                        // get the gradient stop list to copy (see below)
                        pGradientStops := linkedBrushes[i].m_pRadialGradient.m_pGradientStops;

                        // copy cx parameter if undefined
                        if (pBrush.m_pRadialGradient.m_CX = 0.0) then
                            pBrush.m_pRadialGradient.m_CX := linkedBrushes[i].m_pRadialGradient.m_CX;

                        // copy cy parameter if undefined
                        if (pBrush.m_pRadialGradient.m_CY = 0.0) then
                            pBrush.m_pRadialGradient.m_CY := linkedBrushes[i].m_pRadialGradient.m_CY;

                        // copy radius parameter if undefined
                        if (pBrush.m_pRadialGradient.m_R = 0.0) then
                            pBrush.m_pRadialGradient.m_R := linkedBrushes[i].m_pRadialGradient.m_R;

                        // copy fx parameter if undefined
                        if (pBrush.m_pRadialGradient.m_FX = 0.0) then
                            pBrush.m_pRadialGradient.m_FX := linkedBrushes[i].m_pRadialGradient.m_FX;

                        // copy fy parameter if undefined
                        if (pBrush.m_pRadialGradient.m_FY = 0.0) then
                            pBrush.m_pRadialGradient.m_FY := linkedBrushes[i].m_pRadialGradient.m_FY;

                        // copy matrix parameter if undefined
                        if (pBrush.m_pRadialGradient.m_Matrix.IsIdentity) then
                        begin
                            pBrush.m_pRadialGradient.m_Matrix.Assign(linkedBrushes[i].m_pRadialGradient.m_Matrix);
                            pBrush.m_pRadialGradient.m_MatrixType := linkedBrushes[i].m_pRadialGradient.m_MatrixType;
                        end;

                        // copy unit parameter if undefined
                        if (pBrush.m_pRadialGradient.m_Unit = TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox) then
                            pBrush.m_pRadialGradient.m_Unit := linkedBrushes[i].m_pRadialGradient.m_Unit;

                        // copy spread method parameter if undefined
                        if (pBrush.m_pRadialGradient.m_SpreadMethod = TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad) then
                            pBrush.m_pRadialGradient.m_SpreadMethod := linkedBrushes[i].m_pRadialGradient.m_SpreadMethod;
                    end;
                end;

                // the target brush contains no gradient stops?
                if (Assigned(pGradientStops)) then
                    if (pBrush.m_pRadialGradient.m_pGradientStops.Count = 0) then
                    begin
                        stopCount := pGradientStops.Count;

                        // if the target gradient has no defined gradient stops, and the
                        // referenced element does, then the gradient stops will be copied from
                        // the reference
                        for j := 0 to stopCount - 1 do
                        begin
                            pNewStop := nil;

                            try
                                pNewStop := IGradientStop.Create;
                                pNewStop.Assign(pGradientStops[j]);

                                pBrush.m_pRadialGradient.m_pGradientStops.Add(pNewStop);
                                pNewStop := nil;
                            finally
                                pNewStop.Free;
                            end;
                        end;
                    end;
            end;
        finally
            linkedBrushCount := Length(linkedBrushes);

            for i := 0 to linkedBrushCount - 1 do
                linkedBrushes[i].Free;

            SetLength(linkedBrushes, 0);
        end;

        pBrush.m_Type                   :=  E_BT_Radial;
        pBrush.m_Rule                   := IE_PR_Default;
        pBrush.m_pRadialGradient.m_Rule := IE_PR_Default;
        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.GetGradientStops(const pGradient: TWSVGGradient; pGradientStops: IGradientStops);
var
    pStyle:                                        TWSVGStyle;
    pProperty, pStyleProperty:                     TWSVGProperty;
    pMeasure, pOffset:                             TWSVGMeasure<Single>;
    pColor:                                        TWSVGPropColor;
    pSVGGradientStop:                              TWSVGGradientStop;
    pGradientStop:                                 IGradientStop;
    color:                                         TWColor;
    stopCount, propCount, stylePropCount, i, j, k: NativeInt;
begin
    stopCount               := pGradient.GradientStopCount;
    pGradientStops.Capacity := stopCount;

    // iterate through gradient stops to create
    for i := 0 to stopCount - 1 do
    begin
        pSVGGradientStop := pGradient.GradientStops[i];
        pGradientStop    := nil;

        try
            pGradientStop := IGradientStop.Create;
            propCount     := pSVGGradientStop.Count;

            // iterate through gradient stop properties
            for j := 0 to propCount - 1 do
            begin
                pProperty := pSVGGradientStop.Properties[j];

                if ((pProperty.ItemName = C_SVG_Prop_Style) and (pProperty is TWSVGStyle)) then
                begin
                    // get style
                    pStyle := pProperty as TWSVGStyle;

                    // found it?
                    if (not Assigned(pStyle)) then
                        continue;

                    stylePropCount := pStyle.Count;

                    // iterate through gradient stop properties
                    for k := 0 to stylePropCount - 1 do
                    begin
                        pStyleProperty := pStyle.Properties[k];

                        if ((pStyleProperty.ItemName = C_SVG_Gradient_Stop_Color)
                                and (pStyleProperty is TWSVGPropColor))
                        then
                        begin
                            // get stop color
                            pColor := pStyleProperty as TWSVGPropColor;

                            // found it?
                            if (Assigned(pColor)) then
                            begin
                                // color has no values?
                                if (pColor.Count = 0) then
                                    color.SetColor(C_SVG_Default_Color)
                                else
                                    // get fill color to apply
                                    color.Assign(pColor.Values[0]^);

                                // copy only RGB values (opacity is defined by another property)
                                pGradientStop.m_Color.SetRed(color.GetRed);
                                pGradientStop.m_Color.SetGreen(color.GetGreen);
                                pGradientStop.m_Color.SetBlue(color.GetBlue);
                            end;
                        end
                        else
                        if ((pStyleProperty.ItemName = C_SVG_Gradient_Stop_Opacity)
                                and (pStyleProperty is TWSVGMeasure<Single>))
                        then
                        begin
                            // get stop opacity
                            pMeasure := pStyleProperty as TWSVGMeasure<Single>;

                            // found it?
                            if (not Assigned(pMeasure)) then
                                continue;

                            // set stop opacity
                            pGradientStop.m_Color.SetOpacity(Trunc(pMeasure.Value.Value * 100.0));
                        end;
                    end;
                end
                else
                if ((pProperty.ItemName = C_SVG_Gradient_Stop_Offset)
                        and (pProperty is TWSVGMeasure<Single>))
                then
                begin
                    // get stop offset
                    pOffset := pProperty as TWSVGMeasure<Single>;

                    // found it?
                    if (not Assigned(pOffset)) then
                        continue;

                    if (pOffset.MeasureUnit = IEUnit.IE_UN_Percent) then
                        pGradientStop.m_Offset := pOffset.Value.Value / 100.0
                    else
                        pGradientStop.m_Offset := pOffset.Value.Value;
                end;
            end;

            pGradientStops.Add(pGradientStop);
            pGradientStop := nil;
        finally
            pGradientStop.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.GetTransformAnimMatrix(const pAnimationData: IAnimationData;
        pMatrixItem: IPropMatrixItem; pCustomData: Pointer);
var
    pAnimation: TWSVGAnimation;
    pAnimDesc:  IWSmartPointer<TWSVGMatrixAnimDesc>;
    animMatrix: TWMatrix3x3;
    attribName: UnicodeString;
    position:   Double;
begin
    if (not Assigned(pMatrixItem)) then
        Exit;

    // do animate shape?
    if (not m_Animate) then
        Exit;

    // matrix animations?
    if (pAnimationData.MatrixAnims.Count = 0) then
        Exit;

    // iterate through matrix animations
    for pAnimation in pAnimationData.MatrixAnims do
    begin
        // not a matrix animation type?
        if (pAnimation.ValueType <> TWSVGCommon.IEValueType.IE_VT_Matrix) then
            continue;

        // configure animation description
        pAnimDesc           := TWSmartPointer<TWSVGMatrixAnimDesc>.Create();
        pAnimDesc.Animation := pAnimation;

        // populate animation description, and check if animation is allowed to continue
        if (not PopulateAnimation(pAnimation, attribName, pAnimDesc, True, pCustomData)) then
            continue;

        // check if the property to modify is the transform one. NOTE do not confuse, transform
        // refers to the property name, which contains the matrix to apply, and NOT to the matrix
        // type (translate, rotate, ...)
        if (attribName <> C_SVG_Animation_Transform) then
            continue;

        // is animation running? (i.e between its start and end position)
        if (not GetAnimPos(pAnimationData, pAnimDesc, position)) then
            continue;

        animMatrix.SetIdentity;

        // get animation transformation matrix to apply
        pAnimDesc.Combine(position, animMatrix);

        // combine the animation matrix with the root matrix
        case (pAnimDesc.AdditiveMode) of
            IE_AT_Replace: pMatrixItem.m_Value.Assign(animMatrix);
            IE_AT_Sum:     pMatrixItem.m_Value.Assign(animMatrix.Multiply(pMatrixItem.m_Value));
        else
            raise Exception.CreateFmt('Unknown additive mode - %d', [Integer(pAnimDesc.AdditiveMode)]);
        end;
    end;

    // from now the local matrix should be combined with its parent
    pMatrixItem.Rule := IE_PR_Combine;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.GetAnimations(const pContainer: TWSVGContainer; pAnimationData: IAnimationData);
var
    animCount, i: NativeInt;
    pAnimation:   TWSVGAnimation;
begin
    // do animate shape?
    if (not m_Animate) then
        Exit;

    animCount := pContainer.AnimationCount;

    // iterate through animations
    for i := 0 to animCount - 1 do
    begin
        pAnimation := pContainer.Animations[i];

        // search for animation type
        if (pAnimation.ItemName = C_SVG_Tag_Set) then
            // add animation to list
            pAnimationData.m_pSetAnims.Add(pAnimation)
        else
        if (pAnimation.ItemName = C_SVG_Tag_Animate) then
            // add animation to list
            pAnimationData.m_pAttribAnims.Add(pAnimation)
        else
        if (pAnimation.ItemName = C_SVG_Tag_Animate_Color) then
            // add animation to list
            pAnimationData.m_pColorAnims.Add(pAnimation)
        else
        if (pAnimation.ItemName = C_SVG_Tag_Animate_Transform) then
            // add animation to list
            pAnimationData.m_pMatrixAnims.Add(pAnimation)
        else
        if ((pAnimation.ItemName = C_SVG_Tag_Set) or (pAnimation.ItemName = C_SVG_Tag_Animate_Motion)) then
            // add animation to list
            pAnimationData.m_pUnknownAnims.Add(pAnimation);
    end;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.PopulateAnimation(const pAnimation: TWSVGAnimation; out attribName: UnicodeString;
        pAnimDesc: TWSVGAnimationDescriptor; callOnAnimate: Boolean;
        pCustomData: Pointer): Boolean;
var
    pProperty:                                                        TWSVGProperty;
    pAttribName:                                                      TWSVGAnimation.IPropAttributeName;
    pRepeatCount:                                                     TWSVGAnimation.IPropRepeatCount;
    pAnimType:                                                        TWSVGAnimation.IPropAnimTransformType;
    pAdditiveMode:                                                    TWSVGAnimation.IPropAdditiveMode;
    pCalcMode:                                                        TWSVGAnimation.IPropCalcMode;
    pValueDesc:                                                       TWSVGValueAnimDesc;
    pMatrixDesc:                                                      TWSVGMatrixAnimDesc;
    pColorDesc:                                                       TWSVGColorAnimDesc;
    pEnumDesc:                                                        TWSVGEnumAnimDesc;
    pFrom, pTo, pBy, pValues:                                         TWSVGAttribute<Single>;
    pFromColor, pToColor, pByColor, pValuesColor:                     TWSVGPropColor;
    pFromDisplay, pToDisplay, pByDisplay, pValuesDisplay:             TWSVGStyle.IPropDisplay;
    pFromVisibility, pToVisibility, pByVisibility, pValuesVisibility: TWSVGStyle.IPropVisibility;
    pBegin, pEnd, pDuration:                                          TWSVGPropTime;
    propCount, fromCount, toCount, byCount, valueCount, i, j:         NativeInt;
begin
    attribName := '';

    propCount := pAnimation.Count;

    // iterate through animations attributes (i.e. animations linked to a particular shape attribute)
    for i := 0 to propCount - 1 do
    begin
        pProperty := pAnimation.Properties[i];

        // search for animation properties
        if ((pProperty.ItemName = C_SVG_Animation_Attribute_Name)
                and (pProperty is TWSVGAnimation.IPropAttributeName))
        then
        begin
            // get attribute name
            pAttribName := pProperty as TWSVGAnimation.IPropAttributeName;

            // found it?
            if (not Assigned(pAttribName)) then
                continue;

            // set attribute name
            attribName := pAttribName.AttributeName;
        end
        else
        if (pProperty.ItemName = C_SVG_Animation_From) then
        begin
            // search for animation value type (value, color, matrix, ...)
            case (pAnimation.ValueType) of
                TWSVGCommon.IEValueType.IE_VT_Value:
                begin
                    if (not (pAnimDesc is TWSVGValueAnimDesc)) then
                        continue;

                    // get animation set based on values
                    pValueDesc := pAnimDesc as TWSVGValueAnimDesc;

                    // found it?
                    if (not Assigned(pValueDesc)) then
                        continue;

                    // is from property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation from property
                        pFrom := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pFrom)) then
                            continue;

                        // get from value count
                        fromCount := pFrom.Count;

                        // iterate through all from values
                        for j := 0 to fromCount - 1 do
                            // set from value
                            pValueDesc.AddFrom(pFrom.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Matrix:
                begin
                    if (not (pAnimDesc is TWSVGMatrixAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pMatrixDesc := pAnimDesc as TWSVGMatrixAnimDesc;

                    // found it?
                    if (not Assigned(pMatrixDesc)) then
                        continue;

                    // is from property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation from property
                        pFrom := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pFrom)) then
                            continue;

                        // get from value count
                        fromCount := pFrom.Count;

                        // iterate through all from values
                        for j := 0 to fromCount - 1 do
                            // set from value
                            pMatrixDesc.AddFrom(pFrom.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Color:
                begin
                    if (not (pAnimDesc is TWSVGColorAnimDesc)) then
                        continue;

                    // get animation set based on colors
                    pColorDesc := pAnimDesc as TWSVGColorAnimDesc;

                    // found it?
                    if (not Assigned(pColorDesc)) then
                        continue;

                    // is from property a color?
                    if (pProperty is TWSVGPropColor) then
                    begin
                        // convert from property as color
                        pFromColor := pProperty as TWSVGPropColor;

                        // found it?
                        if (not Assigned(pFromColor)) then
                            continue;

                        // get from value count
                        fromCount := pFromColor.Count;

                        // iterate through all from values
                        for j := 0 to fromCount - 1 do
                            // set from value
                            pColorDesc.AddFrom(pFromColor.Values[j]);
                    end;
                end;

                TWSVGCommon.IEValueType.IE_VT_Enum:
                begin
                    if (not (pAnimDesc is TWSVGEnumAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pEnumDesc := pAnimDesc as TWSVGEnumAnimDesc;

                    // found it?
                    if (not Assigned(pEnumDesc)) then
                        continue;

                    // is from property a list of attributes?
                    if (pProperty is TWSVGStyle.IPropDisplay) then
                    begin
                        // get animation from property
                        pFromDisplay := pProperty as TWSVGStyle.IPropDisplay;

                        // found it?
                        if (not Assigned(pFromDisplay)) then
                            continue;

                        // get from value count
                        fromCount := pFromDisplay.Count;

                        // iterate through all from values
                        for j := 0 to fromCount - 1 do
                            // set from value
                            pEnumDesc.AddFrom(Integer(pFromDisplay.Values[j]));
                    end
                    else
                    if (pProperty is TWSVGStyle.IPropVisibility) then
                    begin
                        // get animation from property
                        pFromVisibility := pProperty as TWSVGStyle.IPropVisibility;

                        // found it?
                        if (not Assigned(pFromVisibility)) then
                            continue;

                        // get from value count
                        fromCount := pFromVisibility.Count;

                        // iterate through all from values
                        for j := 0 to fromCount - 1 do
                            // set from value
                            pEnumDesc.AddFrom(Integer(pFromVisibility.Values[j]));
                    end;
                end;
            else
                raise Exception.CreateFmt('Read animation - found unknown or unsupported data type - % d - attribute name - %s',
                        [Integer(pAnimation.ValueType), attribName]);
            end;
        end
        else
        if (pProperty.ItemName = C_SVG_Animation_To) then
        begin
            // search for animation value type (value, color, matrix, ...)
            case (pAnimation.ValueType) of
                TWSVGCommon.IEValueType.IE_VT_Value:
                begin
                    if (not (pAnimDesc is TWSVGValueAnimDesc)) then
                        continue;

                    // get animation set based on values
                    pValueDesc := pAnimDesc as TWSVGValueAnimDesc;

                    // found it?
                    if (not Assigned(pValueDesc)) then
                        continue;

                    // is to property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation to property
                        pTo := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pTo)) then
                            continue;

                        // get to value count
                        toCount := pTo.Count;

                        // iterate through all to values
                        for j := 0 to toCount - 1 do
                            // set to value
                            pValueDesc.AddTo(pTo.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Matrix:
                begin
                    if (not (pAnimDesc is TWSVGMatrixAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pMatrixDesc := pAnimDesc as TWSVGMatrixAnimDesc;

                    // found it?
                    if (not Assigned(pMatrixDesc)) then
                        continue;

                    // is to property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation to property
                        pTo := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pTo)) then
                            continue;

                        // get to value count
                        toCount := pTo.Count;

                        // iterate through all to values
                        for j := 0 to toCount - 1 do
                            // set to value
                            pMatrixDesc.AddTo(pTo.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Color:
                begin
                    if (not (pAnimDesc is TWSVGColorAnimDesc)) then
                        continue;

                    // get animation set based on colors
                    pColorDesc := pAnimDesc as TWSVGColorAnimDesc;

                    // found it?
                    if (not Assigned(pColorDesc)) then
                        continue;

                    // is to property a color?
                    if (pProperty is TWSVGPropColor) then
                    begin
                        // convert to property as color
                        pToColor := pProperty as TWSVGPropColor;

                        // found it?
                        if (not Assigned(pToColor)) then
                            continue;

                        // get to value count
                        toCount := pToColor.Count;

                        // iterate through all to values
                        for j := 0 to toCount - 1 do
                            // set to value
                            pColorDesc.AddTo(pToColor.Values[j]);
                    end;
                end;

                TWSVGCommon.IEValueType.IE_VT_Enum:
                begin
                    if (not (pAnimDesc is TWSVGEnumAnimDesc)) then
                        continue;

                    // get animation set based on values
                    pEnumDesc := pAnimDesc as TWSVGEnumAnimDesc;

                    // found it?
                    if (not Assigned(pEnumDesc)) then
                        continue;

                    // is to property a list of attributes?
                    if (pProperty is TWSVGStyle.IPropDisplay) then
                    begin
                        // get animation to property
                        pToDisplay := pProperty as TWSVGStyle.IPropDisplay;

                        // found it?
                        if (not Assigned(pToDisplay)) then
                            continue;

                        // get to value count
                        toCount := pToDisplay.Count;

                        // iterate through all to values
                        for j := 0 to toCount - 1 do
                            // set to value
                            pEnumDesc.AddTo(Integer(pToDisplay.Values[j]));
                    end
                    else
                    if (pProperty is TWSVGStyle.IPropVisibility) then
                    begin
                        // get animation to property
                        pToVisibility := pProperty as TWSVGStyle.IPropVisibility;

                        // found it?
                        if (not Assigned(pToVisibility)) then
                            continue;

                        // get to value count
                        toCount := pToVisibility.Count;

                        // iterate through all to values
                        for j := 0 to toCount - 1 do
                            // set to value
                            pEnumDesc.AddTo(Integer(pToVisibility.Values[j]));
                    end;
                end;
            else
                raise Exception.CreateFmt('Read animation - found unknown or unsupported data type - % d - attribute name - %s',
                        [Integer(pAnimation.ValueType), attribName]);
            end;
        end
        else
        if (pProperty.ItemName = C_SVG_Animation_By) then
        begin
            // search for animation value type (value, color, matrix, ...)
            case (pAnimation.ValueType) of
                TWSVGCommon.IEValueType.IE_VT_Value:
                begin
                    if (not (pAnimDesc is TWSVGValueAnimDesc)) then
                        continue;

                    // get animation set based on values
                    pValueDesc := pAnimDesc as TWSVGValueAnimDesc;

                    // found it?
                    if (not Assigned(pValueDesc)) then
                        continue;

                    // is by property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation by property
                        pBy := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pBy)) then
                            continue;

                        // get by value count
                        byCount := pBy.Count;

                        // iterate through all by values
                        for j := 0 to byCount - 1 do
                            // set by value
                            pValueDesc.AddBy(pBy.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Matrix:
                begin
                    if (not (pAnimDesc is TWSVGMatrixAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pMatrixDesc := pAnimDesc as TWSVGMatrixAnimDesc;

                    // found it?
                    if (not Assigned(pMatrixDesc)) then
                        continue;

                    // is by property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation by property
                        pBy := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pBy)) then
                            continue;

                        // get by value count
                        byCount := pBy.Count;

                        // iterate through all by values
                        for j := 0 to byCount - 1 do
                            // set by value
                            pMatrixDesc.AddBy(pBy.Values[j]);
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Color:
                begin
                    if (not (pAnimDesc is TWSVGColorAnimDesc)) then
                        continue;

                    // get animation set based on colors
                    pColorDesc := pAnimDesc as TWSVGColorAnimDesc;

                    // found it?
                    if (not Assigned(pColorDesc)) then
                        continue;

                    // is by property a color?
                    if (pProperty is TWSVGPropColor) then
                    begin
                        // convert by property as color
                        pByColor := pProperty as TWSVGPropColor;

                        // found it?
                        if (not Assigned(pByColor)) then
                            continue;

                        // get by value count
                        byCount := pByColor.Count;

                        // iterate through all by values
                        for j := 0 to byCount - 1 do
                            // set by value
                            pColorDesc.AddBy(pByColor.Values[j]);
                    end;
                end;

                TWSVGCommon.IEValueType.IE_VT_Enum:
                begin
                    if (not (pAnimDesc is TWSVGEnumAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pEnumDesc := pAnimDesc as TWSVGEnumAnimDesc;

                    // found it?
                    if (not Assigned(pEnumDesc)) then
                        continue;

                    // is by property a list of known enumerated values?
                    if (pProperty is TWSVGStyle.IPropDisplay) then
                    begin
                        // get animation by property
                        pByDisplay := pProperty as TWSVGStyle.IPropDisplay;

                        // found it?
                        if (not Assigned(pByDisplay)) then
                            continue;

                        // get by value count
                        byCount := pByDisplay.Count;

                        // iterate through all by values
                        for j := 0 to byCount - 1 do
                            // set by value
                            pEnumDesc.AddBy(Integer(pByDisplay.Values[j]));
                    end
                    else
                    if (pProperty is TWSVGStyle.IPropVisibility) then
                    begin
                        // get animation by property
                        pByVisibility := pProperty as TWSVGStyle.IPropVisibility;

                        // found it?
                        if (not Assigned(pByVisibility)) then
                            continue;

                        // get by value count
                        byCount := pByVisibility.Count;

                        // iterate through all by values
                        for j := 0 to byCount - 1 do
                            // set by value
                            pEnumDesc.AddBy(Integer(pByVisibility.Values[j]));
                    end;
                end;
            else
                raise Exception.CreateFmt('Read animation - found unknown or unsupported data type - % d - attribute name - %s',
                        [Integer(pAnimation.ValueType), attribName]);
            end;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Begin) and (pProperty is TWSVGPropTime)) then
        begin
            // get animation begin property
            pBegin := pProperty as TWSVGPropTime;

            // found it?
            if (not Assigned(pBegin)) then
                continue;

            // set begin property
            pAnimDesc.BeginTime.Assign(pBegin.Value);
            pAnimDesc.NegativeBegin := pBegin.Negative;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_End) and (pProperty is TWSVGPropTime)) then
        begin
            // get animation end property
            pEnd := pProperty as TWSVGPropTime;

            // found it?
            if (not Assigned(pEnd)) then
                continue;

            // set end property
            pAnimDesc.EndTime.Assign(pEnd.Value);
            pAnimDesc.NegativeEnd := pEnd.Negative;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Duration) and (pProperty is TWSVGPropTime)) then
        begin
            // get animation duration property
            pDuration := pProperty as TWSVGPropTime;

            // found it?
            if (not Assigned(pDuration)) then
                continue;

            // set duration property
            pAnimDesc.Duration.Assign(pDuration.Value);
            pAnimDesc.NegativeDuration := pDuration.Negative;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Repeat_Count)
                and (pProperty is TWSVGAnimation.IPropRepeatCount))
        then
        begin
            // get animation repeat count
            pRepeatCount := pProperty as TWSVGAnimation.IPropRepeatCount;

            // found it?
            if (not Assigned(pRepeatCount)) then
                continue;

            // is animation indefinite?
            if (pRepeatCount.Indefinite) then
            begin
                pAnimDesc.DoLoop := True;
                continue;
            end;

            // set from property
            pAnimDesc.RepeatCount  := pRepeatCount.Count;
            pAnimDesc.PartialCount := pRepeatCount.PartialCount;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Transform_Type)
                and (pProperty is TWSVGAnimation.IPropAnimTransformType))
        then
        begin
            // search for animation value type (value, color, matrix, ...)
            case (pAnimation.ValueType) of
                TWSVGCommon.IEValueType.IE_VT_Matrix:
                begin
                    if (not (pAnimDesc is TWSVGMatrixAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pMatrixDesc := pAnimDesc as TWSVGMatrixAnimDesc;

                    // found it?
                    if (not Assigned(pMatrixDesc)) then
                        continue;

                    // get animation type
                    pAnimType := pProperty as TWSVGAnimation.IPropAnimTransformType;

                    // found it?
                    if (not Assigned(pAnimType)) then
                        continue;

                    // set animation type
                    pMatrixDesc.TransformType := pAnimType.TransformType;
                end;
            end;
        end
        else
        if (pProperty.ItemName = C_SVG_Animation_Values) then
        begin
            // search for animation value type (value, color, matrix, ...)
            case (pAnimation.ValueType) of
                TWSVGCommon.IEValueType.IE_VT_Value:
                begin
                    if (not (pAnimDesc is TWSVGValueAnimDesc)) then
                        continue;

                    // get animation set based on values
                    pValueDesc := pAnimDesc as TWSVGValueAnimDesc;

                    // found it?
                    if (not Assigned(pValueDesc)) then
                        continue;

                    // is values property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation values property
                        pValues := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pValues)) then
                            continue;

                        // get values count
                        valueCount := pValues.Count;

                        // iterate through all values
                        for j := 0 to valueCount - 1 do
                            pValueDesc.AddValue(pValues.Values[j]);

                        // copy the group count and values per group count
                        pValueDesc.GroupCount         := pValues.GroupCount;
                        pValueDesc.ValuePerGroupCount := pValues.ValuePerGroupCount;
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Matrix:
                begin
                    if (not (pAnimDesc is TWSVGMatrixAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pMatrixDesc := pAnimDesc as TWSVGMatrixAnimDesc;

                    // found it?
                    if (not Assigned(pMatrixDesc)) then
                        continue;

                    // is values property a list of attributes?
                    if (pProperty is TWSVGAttribute<Single>) then
                    begin
                        // get animation values property
                        pValues := pProperty as TWSVGAttribute<Single>;

                        // found it?
                        if (not Assigned(pValues)) then
                            continue;

                        // get values count
                        valueCount := pValues.Count;

                        // iterate through all values
                        for j := 0 to valueCount - 1 do
                            pMatrixDesc.AddValue(pValues.Values[j]);

                        // copy the group count and values per group count
                        pMatrixDesc.GroupCount         := pValues.GroupCount;
                        pMatrixDesc.ValuePerGroupCount := pValues.ValuePerGroupCount;
                    end
                end;

                TWSVGCommon.IEValueType.IE_VT_Color:
                begin
                    if (not (pAnimDesc is TWSVGColorAnimDesc)) then
                        continue;

                    // get animation set based on colors
                    pColorDesc := pAnimDesc as TWSVGColorAnimDesc;

                    // found it?
                    if (not Assigned(pColorDesc)) then
                        continue;

                    // is values property a color?
                    if (pProperty is TWSVGPropColor) then
                    begin
                        // convert values property as color
                        pValuesColor := pProperty as TWSVGPropColor;

                        // found it?
                        if (not Assigned(pValuesColor)) then
                            continue;

                        // get values count
                        valueCount := pValuesColor.Count;

                        // iterate through all values
                        for j := 0 to valueCount - 1 do
                            pColorDesc.AddValue(pValuesColor.Values[j]);
                    end;
                end;

                TWSVGCommon.IEValueType.IE_VT_Enum:
                begin
                    if (not (pAnimDesc is TWSVGEnumAnimDesc)) then
                        continue;

                    // get animation set based on matrices
                    pEnumDesc := pAnimDesc as TWSVGEnumAnimDesc;

                    // found it?
                    if (not Assigned(pEnumDesc)) then
                        continue;

                    // is values property a known enumerator?
                    if (pProperty is TWSVGStyle.IPropDisplay) then
                    begin
                        // get animation values property
                        pValuesDisplay := pProperty as TWSVGStyle.IPropDisplay;

                        // found it?
                        if (not Assigned(pValuesDisplay)) then
                            continue;

                        // get values count
                        valueCount := pValuesDisplay.Count;

                        // iterate through all values
                        for j := 0 to valueCount - 1 do
                            pEnumDesc.AddValue(Integer(pValuesDisplay.Values[j]));
                    end
                    else
                    if (pProperty is TWSVGStyle.IPropVisibility) then
                    begin
                        // get animation values property
                        pValuesVisibility := pProperty as TWSVGStyle.IPropVisibility;

                        // found it?
                        if (not Assigned(pValuesVisibility)) then
                            continue;

                        // get values count
                        valueCount := pValuesVisibility.Count;

                        // iterate through all values
                        for j := 0 to valueCount - 1 do
                            pEnumDesc.AddValue(Integer(pValuesVisibility.Values[j]));
                    end;
                end;
            else
                raise Exception.CreateFmt('Read animation - found unknown or unsupported data type - % d - attribute name - %s',
                        [Integer(pAnimation.ValueType), attribName]);
            end;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Calc_Mode) and (pProperty is TWSVGAnimation.IPropCalcMode)) then
        begin
            // get animation calculation mode property
            pCalcMode := pProperty as TWSVGAnimation.IPropCalcMode;

            // found it?
            if (not Assigned(pCalcMode)) then
                continue;

            pAnimDesc.CalcMode := pCalcMode.CalcModeType;
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Key_Splines) and (pProperty is TWSVGAttribute<Single>)) then
        begin
            // get animation key splines property
            pValues := pProperty as TWSVGAttribute<Single>;

            // found it?
            if (not Assigned(pValues)) then
                continue;

            // get key splines count
            valueCount := pValues.Count;

            // iterate through all key splines
            for j := 0 to valueCount - 1 do
                // set key spline
                pAnimDesc.AddKeySpline(pValues.Values[j]);
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Key_Times) and (pProperty is TWSVGAttribute<Single>)) then
        begin
            // get animation key times property
            pValues := pProperty as TWSVGAttribute<Single>;

            // found it?
            if (not Assigned(pValues)) then
                continue;

            // get key times count
            valueCount := pValues.Count;

            // iterate through all key times
            for j := 0 to valueCount - 1 do
                // set key time
                pAnimDesc.AddKeyTime(pValues.Values[j]);
        end
        else
        if ((pProperty.ItemName = C_SVG_Animation_Additive) and (pProperty is TWSVGAnimation.IPropAdditiveMode)) then
        begin
            // get animation additive mode property
            pAdditiveMode := pProperty as TWSVGAnimation.IPropAdditiveMode;

            // found it?
            if (not Assigned(pAdditiveMode)) then
                continue;

            pAnimDesc.AdditiveMode := pAdditiveMode.AdditiveType;
        end;
    end;

    // notify that animation is running
    if (callOnAnimate and Assigned(m_fOnAnimate)) then
        Exit(m_fOnAnimate(pAnimDesc, pCustomData));

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.GetAnimationDuration(pHeader: TWSVGParser.IHeader;
        const pElements: TWSVGContainer.IElements; switchMode, useMode: Boolean;
        out durationMax: NativeUInt);
var
    pClonedElements:          IWSmartPointer<TWSVGContainer.IElements>;
    pElement, pLinkedElement: TWSVGElement;
    pClone:                   IWSmartPointer<TWSVGElement>;
    pGroup:                   TWSVGGroup;
    pSwitch:                  TWSVGSwitch;
    pAction:                  TWSVGAction;
    pUse:                     TWSVGUse;
    pSymbol:                  TWSVGSymbol;
    pEmbeddedSVG:             TWSVGSVG;
    pPath:                    TWSVGPath;
    pRect:                    TWSVGRect;
    pCircle:                  TWSVGCircle;
    pEllipse:                 TWSVGEllipse;
    pLine:                    TWSVGLine;
    pPolygon:                 TWSVGPolygon;
    pPolyline:                TWSVGPolyline;
    pText:                    TWSVGText;
    pAnimationData:           IWSmartPointer<IAnimationData>;
begin
    // iterate through SVG elements
    for pElement in pElements do
    begin
        // get svg header (should always be the first element, because header is contained inside
        // svg tag itself, that is the root tag). NOTE ignore the embedded SVG, because the header
        // was already defined in this case
        if ((pElement.ItemName = C_SVG_Tag_Name) and (not(pElement is TWSVGSVG))) then
        begin
            if (pElement is TWSVGParser.IHeader) then
                pHeader := pElement as TWSVGParser.IHeader
            else
                pHeader := nil;

            continue;
        end;

        // from now, svg header should be declared, otherwise svg data is malformed (NOTE svg header
        // element should exist even if svg tag contains nothing else than declaration)
        if (not Assigned(pHeader)) then
            raise Exception.Create('SVG is malformed');

        // is a group?
        if (pElement is TWSVGGroup) then
        begin
            // get group
            pGroup := pElement as TWSVGGroup;

            // found it?
            if (Assigned(pGroup)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pGroup, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // get animation duration in group subelements
                GetAnimationDuration(pHeader, pGroup.ElementList, False, useMode, durationMax);
                continue;
            end;
        end;

        // is a switch?
        if (pElement is TWSVGSwitch) then
        begin
            // get switch
            pSwitch := pElement as TWSVGSwitch;

            // found it?
            if (Assigned(pSwitch)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pSwitch, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // get animation duration in switch subelements
                GetAnimationDuration(pHeader, pSwitch.ElementList, True, useMode, durationMax);
                continue;
            end;
        end;

        // is an action?
        if (pElement is TWSVGAction) then
        begin
            // get action
            pAction := pElement as TWSVGAction;

            // found it?
            if (Assigned(pAction)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pAction, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // get animation duration in action subelements
                GetAnimationDuration(pHeader, pAction.ElementList, False, useMode, durationMax);
                continue;
            end;
        end;

        // is an use instruction?
        if (pElement is TWSVGUse) then
        begin
            // get use instruction
            pUse := pElement as TWSVGUse;

            // found it?
            if (Assigned(pUse)) then
            begin
                // get the linked element to use
                if (not GetLinkedElementToUse(pUse, pLinkedElement)) then
                    // don't care if link was not found, just continue with next element. It's not
                    // unusual that a SVG contains links pointing to nothing
                    continue;

                if (not(pLinkedElement is TWSVGContainer)) then
                    continue;

                // configure animation
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pUse, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // clone the source element. This is required because several additional properties
                // will be merged from the use instruction
                pClone := TWSmartPointer<TWSVGElement>.Create(pLinkedElement.CreateInstance(pLinkedElement.Parent));
                pClone.Assign(pLinkedElement);

                // create a pseudo element container, and add the cloned element to draw
                pClonedElements := TWSmartPointer<TWSVGContainer.IElements>.Create(TWSVGContainer.IElements.Create(False));
                pClonedElements.Add(pClone);

                // get animation duration in cloned subelements
                GetAnimationDuration(pHeader, pClonedElements, False, True, durationMax);
                continue;
            end;
        end;

        // is a symbol?
        if (pElement is TWSVGSymbol) then
        begin
            // not allowed to be drawn if not called from an use instruction
            if (not useMode) then
                continue;

            // get symbol
            pSymbol := pElement as TWSVGSymbol;

            // found it?
            if (Assigned(pSymbol)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pSymbol, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // get animation duration in symbol subelements
                GetAnimationDuration(pHeader, pSymbol.ElementList, False, useMode, durationMax);
                continue;
            end;
        end;

        // is an embedded SVG?
        if (pElement is TWSVGSVG) then
        begin
            // get embedded SVG
            pEmbeddedSVG := pElement as TWSVGSVG;

            // found it?
            if (Assigned(pEmbeddedSVG)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this container
                GetAnimations(pEmbeddedSVG, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // get animation duration in embedded SVG subelements
                GetAnimationDuration(pHeader, pEmbeddedSVG.ElementList, False, useMode, durationMax);
                continue;
            end;
        end;

        // is a path?
        if (pElement is TWSVGPath) then
        begin
            // get path
            pPath := pElement as TWSVGPath;

            // found it?
            if (Assigned(pPath)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pPath, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a rectangle?
        if (pElement is TWSVGRect) then
        begin
            // get rectangle
            pRect := pElement as TWSVGRect;

            // found it?
            if (Assigned(pRect)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pRect, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a circle?
        if (pElement is TWSVGCircle) then
        begin
            // get circle
            pCircle := pElement as TWSVGCircle;

            // found it?
            if (Assigned(pCircle)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pCircle, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is an ellipse?
        if (pElement is TWSVGEllipse) then
        begin
            // get ellipse
            pEllipse := pElement as TWSVGEllipse;

            // found it?
            if (Assigned(pEllipse)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pEllipse, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a line
        if (pElement is TWSVGLine) then
        begin
            // get line
            pLine := pElement as TWSVGLine;

            // found it?
            if (Assigned(pLine)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pLine, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a polygon?
        if (pElement is TWSVGPolygon) then
        begin
            // get polygon
            pPolygon := pElement as TWSVGPolygon;

            // found it?
            if (Assigned(pPolygon)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pPolygon, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a polyline?
        if (pElement is TWSVGPolyline) then
        begin
            // get polyline
            pPolyline := pElement as TWSVGPolyline;

            // found it?
            if (Assigned(pPolyline)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pPolyline, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;

        // is a text?
        if (pElement is TWSVGText) then
        begin
            // get text
            pText := pElement as TWSVGText;

            // found it?
            if (Assigned(pText)) then
            begin
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // configure animation
                pAnimationData.m_Position := 0.0;

                // get all animations linked to this shape
                GetAnimations(pText, pAnimationData);

                // keep highest animation duration
                durationMax := Max(GetAnimationDuration(pAnimationData), durationMax);

                // is switch mode enabled?
                if (switchMode) then
                    Exit;

                continue;
            end;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetAnimationDuration(const pAnimationData: IAnimationData): NativeUInt;
var
    pAnimation: TWSVGAnimation;
begin
    Result := 0;

    // iterate through set animations (i.e. set a particular shape attribute at elapsed time)
    for pAnimation in pAnimationData.m_pSetAnims do
        MeasureDuration(pAnimation, Result);

    // iterate through attribute animations
    for pAnimation in pAnimationData.m_pAttribAnims do
        MeasureDuration(pAnimation, Result);

    // iterate through color animations
    for pAnimation in pAnimationData.m_pColorAnims do
        MeasureDuration(pAnimation, Result);

    // iterate through matrix animations
    for pAnimation in pAnimationData.m_pMatrixAnims do
        MeasureDuration(pAnimation, Result);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.MeasureDuration(pAnimation: TWSVGAnimation; var duration: NativeUInt);
var
    pValueAnimDesc:  IWSmartPointer<TWSVGValueAnimDesc>;
    pMatrixAnimDesc: IWSmartPointer<TWSVGMatrixAnimDesc>;
    pColorAnimDesc:  IWSmartPointer<TWSVGColorAnimDesc>;
    pEnumAnimDesc:   IWSmartPointer<TWSVGEnumAnimDesc>;
begin
    pValueAnimDesc           := TWSmartPointer<TWSVGValueAnimDesc>.Create();
    pValueAnimDesc.Animation := pAnimation;
    MeasureDuration(pAnimation, pValueAnimDesc, duration);

    pMatrixAnimDesc           := TWSmartPointer<TWSVGMatrixAnimDesc>.Create();
    pMatrixAnimDesc.Animation := pAnimation;
    MeasureDuration(pAnimation, pMatrixAnimDesc, duration);

    pColorAnimDesc           := TWSmartPointer<TWSVGColorAnimDesc>.Create();
    pColorAnimDesc.Animation := pAnimation;
    MeasureDuration(pAnimation, pColorAnimDesc, duration);

    pEnumAnimDesc           := TWSmartPointer<TWSVGEnumAnimDesc>.Create();
    pEnumAnimDesc.Animation := pAnimation;
    MeasureDuration(pAnimation, pEnumAnimDesc, duration);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.MeasureDuration(const pAnimation: TWSVGAnimation; pAnimDesc: TWSVGAnimationDescriptor;
        var duration: NativeUInt);
var
    attribName: UnicodeString;
begin
    // populate animation description, and check if animation is allowed to continue
    if (not PopulateAnimation(pAnimation, attribName, pAnimDesc, false, nil)) then
        Exit;

    // keep highest animation duration
    duration := Max(pAnimDesc.Duration.ToMilliseconds, duration);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.Transform(const pMatrix: TWSVGPropMatrix; var point: TWVector2): Boolean;
begin
    // no matrix?
    if (not Assigned(pMatrix)) then
        Exit(False);

    // transform point using matrix
    point := pMatrix.Matrix.Transform(point);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.BeginAt(const pAnimDesc: TWSVGAnimationDescriptor): Double;
var
    offset, beginTime, durTime: Double;
begin
    // no begin time or duration?
    if (pAnimDesc.BeginTime.IsEmpty or pAnimDesc.Duration.IsEmpty) then
        Exit(0.0);

    // is begin value negative?
    if (pAnimDesc.NegativeBegin) then
        offset := -1.0
    else
        offset :=  1.0;

    beginTime := pAnimDesc.BeginTime.ToMilliseconds;
    durTime   := pAnimDesc.Duration.ToMilliseconds;

    // calculate absolute animation percent where animation begins
    Result := offset * (beginTime / durTime);
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.EndAt(const pAnimDesc: TWSVGAnimationDescriptor): Double;
var
    endTime, durTime: Double;
begin
    // no end time or duration?
    if (pAnimDesc.EndTime.IsEmpty or pAnimDesc.Duration.IsEmpty) then
        Exit(1.0);

    endTime := pAnimDesc.BeginTime.ToMilliseconds;
    durTime := pAnimDesc.Duration.ToMilliseconds;

    // calculate animation percent where animation ends
    Result := endTime / durTime;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetAnimPos(const pAnimationData: IAnimationData;
        const pAnimDesc: TWSVGAnimationDescriptor; var position: Double): Boolean;
var
    pCacheItem:           ICacheItem;
    pItem, pNewItem:      IAnimCacheItem;
    beginAtPos, endAtPos: Double;
    //cycleRestarted:  Boolean;
begin
    // is animation end or animation duration negative?
    if (pAnimDesc.NegativeEnd or pAnimDesc.NegativeDuration) then
    begin
        // animation cannot run because it was already stopped before starting
        position := 0.0;
        Exit(False);
    end;

    // get current cache to use. Cache item should always be created for the current running
    // animation, if it's not the case it's an error
    if (not m_pCache.TryGetValue(m_UUID, pCacheItem)) then
    begin
        position := 0.0;
        Exit(False);
    end;

    // check if cycle restarted
    //cycleRestarted := (position < pCacheItem.m_LastPos);

    // update last known position
    pCacheItem.m_LastPos := 0.0;//position;

    // get current animation item, create one if not found
    if (not pCacheItem.m_pAnimCache.TryGetValue(pAnimDesc.Animation, pItem)) then
    begin
        pNewItem := nil;

        try
            // create, populate and add new animation item
            pNewItem := IAnimCacheItem.Create;
            pCacheItem.m_pAnimCache.Add(pAnimDesc.Animation, pNewItem);
            pItem    := pNewItem;
            pNewItem := nil;
        finally
            pNewItem.Free;
        end;
    end;

    Assert(Assigned(pItem));

    // current position should be scaled to total position. By principle, the longest animation time
    // represents a global position of 100%. Because the total duration of each sub-animation may be
    // different than total time, sub-animation position should be scaled in relation to the total
    // position, e.g. a sub-animation of 2s will be repeated 3 times during a total animation of 6s
    position := LocalPosToGlobalPos(pCacheItem.m_AnimDuration, pItem, pAnimationData, pAnimDesc);

    // do loop animation?
    if (pAnimDesc.DoLoop) then
    begin
        // get begin at animation position
        beginAtPos := BeginAt(pAnimDesc);

        // theorically animations should begin only when position reaches the beginAt value. But in
        // case of looping animation, the result is really strange. It's why code below was disabled
        {
        // animation already started?
        if (not pItem.m_Started and (beginAtPos >= 0.0)) then
            // check if starting point was reached
            if ((beginAtPos <> 1.0) and (position < beginAtPos)) then
            begin
                position := 0.0;
                Exit(False);
            end
            else
            if ((beginAtPos = 1.0) and not cycleRestarted) then
            begin
                position := 0.0;
                Exit(False);
            end;
        }

        // calculate real position animation (begin at can shift the animation start) NOTE resulting
        // position value is limited between 0.0 and 1.0, thus it can never be out of bounds
        position        := TWMathHelper.ExtMod(position + (1.0 - beginAtPos), 1.0);
        pItem.m_Started := True;
        Exit(True);
    end;

    // get animation begin and end position
    beginAtPos := BeginAt(pAnimDesc);
    endAtPos   := EndAt(pAnimDesc);

    // nothing to check?
    if ((beginAtPos = 0.0) and (endAtPos = 1.0)) then
    begin
        pItem.m_Started := True;
        Exit(True);
    end;

    // animation started?
    if (position < beginAtPos) then
    begin
        position := 0.0;
        Exit(False);
    end;

    pItem.m_Started := True;

    // animation stopped?
    if (position > endAtPos) then
    begin
        position := beginAtPos + (position * (endAtPos - beginAtPos));
        Exit(False);
    end;

    // calculate real position
    position := beginAtPos + (position * (endAtPos - beginAtPos));
    Result   := True;
end;
//---------------------------------------------------------------------------
{$ifdef DEBUG}
    procedure TWSVGRasterizer.LogProps(pElement: TWSVGElement);
    var
        pProperty:    TWSVGProperty;
        propCount, i: NativeInt;
    begin
        if (not Assigned(pElement)) then
            Exit;

        TWLogHelper.LogToCompiler('Element - name - ' + pElement.ItemName + ' - id - ' + pElement.ItemID);

        propCount := pElement.Count;

        // iterate through element properties
        for i := 0 to propCount - 1 do
        begin
            pProperty := pElement.Properties[i];

            if (not Assigned(pProperty)) then
                continue;

            TWLogHelper.LogToCompiler('Property nb. ' + IntToStr(i) + ' - name - ' + pProperty.ItemName
                    + ' - id - ' + pProperty.ItemID);
        end;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetSize(const pSVG: TWSVG): TSize;
var
    pHeader:         TWSVGParser.IHeader;
    pElement:        TWSVGElement;
    elementCount, i: NativeInt;
    viewBox:         TWRectF;
begin
    pHeader      := nil;
    elementCount := pSVG.Parser.ElementCount;

    // iterate through SVG elements
    for i := 0 to elementCount - 1 do
    begin
        pElement := pSVG.Parser.Elements[i];

        // get svg header
        if ((pElement.ItemName = C_SVG_Tag_Name) and (pElement is TWSVGParser.IHeader)) then
        begin
            pHeader := pElement as TWSVGParser.IHeader;
            break;
        end;
    end;

    // found header?
    if (not Assigned(pHeader)) then
        Exit(Default(TSize));

    // get the size from viewbox. Anyway, if no viewbox is defined in the SVG, the width and height
    // properties will be used instead. NOTE although generally the width and height values are
    // identical as the viewbox values, they may differ in several SVG. In this case the viewport
    // represents the real size, because it will contain the whole SVG drawing, while the width and
    // height properties represent the size of the SVG drawing inside the viewport
    viewBox := GetViewBox(pHeader);
    Result  := TSize.Create(Round(viewBox.Width), Round(viewBox.Height));
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetPageStyle(const pSVG: TWSVG; out pageColor: TWColor; out borderColor: TWColor;
        out borderOpacity: Single): Boolean;
var
    pHeader:                    TWSVGParser.IHeader;
    pElement:                   TWSVGElement;
    pProperty:                  TWSVGProperty;
    pColor:                     TWSVGPropColor;
    pOpacity:                   TWSVGMeasure<Single>;
    elementCount, propCount, i: NativeInt;
begin
    pHeader      := nil;
    elementCount := pSVG.Parser.ElementCount;

    // iterate through SVG elements
    for i := 0 to elementCount - 1 do
    begin
        pElement := pSVG.Parser.Elements[i];

        // get svg header
        if ((pElement.ItemName = C_SVG_Tag_Name) and (pElement is TWSVGParser.IHeader)) then
        begin
            pHeader := pElement as TWSVGParser.IHeader;
            break;
        end;
    end;

    // found header?
    if (not Assigned(pHeader)) then
        Exit(False);

    // initialize values in case header doesn't contain them
    pageColor.Clear;
    borderColor.Clear;
    borderOpacity := 0.0;

    propCount := pHeader.Count;

    // iterate through header properties
    for i := 0 to propCount - 1 do
    begin
        pProperty := pHeader.Properties[i];

        // search for header style
        if ((pProperty.ItemName = C_SVG_Prop_Page_Color) and (pProperty is TWSVGPropColor)) then
        begin
            // get SVG page color
            pColor := pProperty as TWSVGPropColor;

            // found it?
            if (not Assigned(pColor)) then
                continue;

            // color has no values?
            if (pColor.Count = 0) then
                pageColor.SetColor(C_SVG_Default_Color)
            else
                // set SVG page color
                pageColor.Assign(pColor.Values[0]^);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Border_Color) and (pProperty is TWSVGPropColor)) then
        begin
            // get SVG border color
            pColor := pProperty as TWSVGPropColor;

            // found it?
            if (not Assigned(pColor)) then
                continue;

            // color has no values?
            if (pColor.Count = 0) then
                borderColor.SetColor(C_SVG_Default_Color)
            else
                // set SVG border color
                borderColor.Assign(pColor.Values[0]^);
        end
        else
        if ((pProperty.ItemName = C_SVG_Prop_Border_Opacity) and (pProperty is TWSVGMeasure<Single>)) then
        begin
            // get SVG border opacity
            pOpacity := pProperty as TWSVGMeasure<Single>;

            // found it?
            if (not Assigned(pOpacity)) then
                continue;

            // set SVG border opacity
            borderOpacity := pOpacity.Value.Value;
        end;
    end;

    // get source size
    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.GetAnimationDuration(const pSVG: TWSVG): NativeUInt;
var
    pHeader:             TWSVGParser.IHeader;
    switchMode, useMode: Boolean;
begin
    pHeader    := nil;
    switchMode := False;
    useMode    := False;
    Result     := 0;

    GetAnimationDuration(pHeader, pSVG.Parser.ElementList, switchMode, useMode, Result);
end;
//---------------------------------------------------------------------------
procedure TWSVGRasterizer.EnableAnimation(value: Boolean);
begin
    // nothing to do?
    if (m_Animate = value) then
        Exit;

    m_Animate := value;

    // reset cache if animation is enabled
    if (m_Animate) then
        m_pCache.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGRasterizer.IsAnimationEnabled: Boolean;
begin
    Result := m_Animate;
end;
//---------------------------------------------------------------------------

end.
