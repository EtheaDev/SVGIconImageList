{**
 @abstract(@name contains the Scalable Vector Graphics (SVG) animations. An animation describes how
           the SVG content should evolve in the time.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGAnimation;
{$I SVGMagic.inc}

interface

uses System.SysUtils,
     System.Math,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWMajorSettings,
     UTWHelpers,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGAttribute,
     UTWSVGItems,
     UTWSVGProperties,
     UTWSVGStyle;

type
    {**
     Scalable Vector Graphics (SVG) animation
    }
    TWSVGAnimation = class(TWSVGElement)
        public type
            {**
             Animation type enumeration
             @value(IE_AT_Unknown Unknown animation target)
             @value(IE_AT_Set Specifies a set of values that modify a target attribute over time)
             @value(IE_AT_Animate Specifies an animation for a single target attribute or property
                                  over time)
             @value(IE_AT_Animate_Color Specifies a color transformation over time)
             @value(IE_AT_Animate_Transform Specifies an animation for a transformation attribute on
                                            a target element, thereby allowing animations to control
                                            translation, scaling, rotation and/or skewing)
             @value(IE_AT_Animate_Motion Specifies a target element to move along a motion path)
            }
            IEAnimType =
            (
                IE_AT_Unknown,
                IE_AT_Set,
                IE_AT_Animate,
                IE_AT_Animate_Color,
                IE_AT_Animate_Transform,
                IE_AT_Animate_Motion
            );

            {**
             Linked attribute name property
            }
            IPropAttributeName = class(TWSVGProperty)
                private
                    m_AttributeName: UnicodeString;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                public
                    {**
                     Get or set the attribute name
                    }
                    property AttributeName: UnicodeString read m_AttributeName write m_AttributeName;
            end;

            {**
             Linked attribute type property
            }
            IPropAttributeType = class(TWSVGProperty)
                public type
                    {**
                     Attribute type enumeration
                     @value(IE_AT_Auto The implementation should match the attributeName to an
                                       attribute for the target element. User-agents first search
                                       through the list of CSS properties for a matching property
                                       name, and if none is found, search the default XML namespace
                                       for the element)
                     @value(IE_AT_CSS This specifies that the value of attributeName is the name of
                                      a CSS property defined as animatable)
                     @value(IE_AT_XML This specifies that the value of attributeName is the name of
                                      an XML attribute defined as animatable in the default XML
                                      namespace for the target element)
                    }
                    IEAttributeType =
                    (
                        IE_AT_Auto,
                        IE_AT_CSS,
                        IE_AT_XML
                    );

                private
                    m_Type: IEAttributeType;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert type to string
                     @param(attributeType Type to convert)
                     @returns(Converted string)
                    }
                    class function TypeToStr(attributeType: IEAttributeType): UnicodeString; static;

                    {**
                     Convert string to type
                     @param(str String containing type to convert)
                     @returns(Converted type)
                    }
                    class function StrToType(const str: UnicodeString): IEAttributeType; static;

                public
                    {**
                     Get or set the attribute type
                    }
                    property AttributeType: IEAttributeType read m_Type write m_Type;
            end;

            {**
             Animation calculation mode property
            }
            IPropCalcMode = class(TWSVGProperty)
                public type
                    {**
                     Calculation mode enumeration
                     @value(IE_CT_Discrete Specifies that the animation function will jump from one
                                           value to the next without any interpolation)
                     @value(IE_CT_Linear Specifies that a simple linear interpolation between values
                                         will be used to calculate the animation function)
                     @value(IE_CT_Paced Specifies that interpolation should produce an even pace of
                                        change across the animation)
                     @value(IE_CT_Spline Specifies that interpolation will be produced from one value
                                         in the <values> list to the next according to a time function
                                         defined by a cubic Bézier spline. The points of the spline
                                         are defined in the <keyTimes> attribute, and the control
                                         points for each interval are defined in the <keySplines>
                                         attribute)
                    }
                    IECalcModeType =
                    (
                        IE_CT_Discrete,
                        IE_CT_Linear,
                        IE_CT_Paced,
                        IE_CT_Spline
                    );

                private
                    m_Type: IECalcModeType;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert type to string
                     @param(calcModeType Type to convert)
                     @returns(Converted string)
                    }
                    class function TypeToStr(calcModeType: IECalcModeType): UnicodeString; static;

                    {**
                     Convert string to type
                     @param(str String containing type to convert)
                     @returns(Converted type)
                    }
                    class function StrToType(const str: UnicodeString): IECalcModeType; static;

                public
                    {**
                     Get or set the calculation mode
                    }
                    property CalcModeType: IECalcModeType read m_Type write m_Type;
            end;

            {**
             Animation fill mode property
            }
            IPropFillMode = class(TWSVGProperty)
                public type
                    {**
                     Fill mode enumeration
                     @value(IE_FM_Unknown The animation effect is unknown)
                     @value(IE_FM_Freeze The animation effect is defined to freeze the effect value
                                         at the last value of the active duration. The animation
                                         effect is "frozen" for the remainder of the document
                                         duration (or until the animation is restarted))
                     @value(IE_FM_Remove The animation effect is removed (no longer applied) when
                                         the active duration of the animation is over. After the
                                         active end of the animation, the animation no longer
                                         affects the target (unless the animation is restarted))
                    }
                    IEMode =
                    (
                        IE_FM_Unknown,
                        IE_FM_Freeze,
                        IE_FM_Remove
                    );

                private
                    m_Mode: IEMode;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert fill mode to string
                     @param(mode Mode to convert)
                     @returns(Converted string)
                     @raises(Exception on unknown mode)
                    }
                    class function ModeToStr(mode: IEMode): UnicodeString; static;

                    {**
                     Convert string to fill mode
                     @param(str String containing mode to convert)
                     @returns(Converted mode)
                     @raises(Exception on unknown mode)
                    }
                    class function StrToMode(const str: UnicodeString): IEMode; static;

                public
                    {**
                     Get or set the fill mode
                    }
                    property Mode: IEMode read m_Mode write m_Mode;
            end;

            {**
             Animation repeat count property, can be indefinite or partial, in this case partial
             count is exprimed in percent
            }
            IPropRepeatCount = class(TWSVGProperty)
                private
                    m_Count:        NativeUInt;
                    m_PartialCount: NativeUInt;
                    m_Indefinite:   Boolean;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                public
                    {**
                     Get or set the count
                    }
                    property Count: NativeUInt read m_Count write m_Count;

                    {**
                     Get or set the partial count
                    }
                    property PartialCount: NativeUInt read m_PartialCount write m_PartialCount;

                    {**
                     Get or set if animation is indefinite
                    }
                    property Indefinite: Boolean read m_Indefinite write m_Indefinite;
            end;

            {**
             Animation restart mode property
            }
            IPropRestart = class(TWSVGProperty)
                public type
                    {**
                     Restart type enumeration
                     @value(IE_RT_Unknown The animation restart behavior is unknown)
                     @value(IE_RT_Always The animation can be restarted at any time. This is the
                                         default value)
                     @value(IE_RT_WhenNotActive The animation can only be restarted when it is not
                                                active (i.e. after the active end). Attempts to
                                                restart the animation during its active duration are
                                                ignored)
                     @value(IE_RT_Never The element cannot be restarted for the remainder of the
                                        current simple duration of the parent time container. (In
                                        the case of SVG, since the parent time container is the SVG
                                        document fragment, then the animation cannot be restarted
                                        for the remainder of the document duration))
                    }
                    IERestartType =
                    (
                        IE_RT_Unknown,
                        IE_RT_Always,
                        IE_RT_WhenNotActive,
                        IE_RT_Never
                    );

                private
                    m_Type: IERestartType;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert type to string
                     @param(type Type to convert)
                     @returns(Converted string)
                    }
                    class function TypeToStr(restartType: IERestartType): UnicodeString; static;

                    {**
                     Convert string to type
                     @param(str String containing type to convert)
                     @returns(Converted type)
                    }
                    class function StrToType(const str: UnicodeString): IERestartType; static;

                public
                    {**
                     Get or set the restart type
                    }
                    property RestartType: IERestartType read m_Type write m_Type;
            end;

            {**
             Animate transform type property
            }
            IPropAnimTransformType = class(TWSVGProperty)
                public type
                    {**
                     Attribute type enumeration
                     @value(IE_TT_Unknown Unknown transformation)
                     @value(IE_TT_Translate Do apply a translation on the target)
                     @value(IE_TT_Scale Do apply a scaling on the target)
                     @value(IE_TT_Rotate Do apply a rotation on the target)
                     @value(IE_TT_SkewX Do apply a skew on the target x axis)
                     @value(IE_TT_SkewY Do apply a skew on the target y axis)
                    }
                    IETransformType =
                    (
                        IE_TT_Unknown,
                        IE_TT_Translate,
                        IE_TT_Scale,
                        IE_TT_Rotate,
                        IE_TT_SkewX,
                        IE_TT_SkewY
                    );

                private
                    m_Type: IETransformType;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert type to string
                     @param(type Type to convert)
                     @returns(Converted string)
                    }
                    class function TypeToStr(transformType: IETransformType): UnicodeString; static;

                    {**
                     Convert string to type
                     @param(str String containing type to convert)
                     @returns(Converted type)
                    }
                    class function StrToType(const str: UnicodeString): IETransformType; static;

                public
                    {**
                     Get or set the transformation type
                    }
                    property TransformType: IETransformType read m_Type write m_Type;
            end;

            {**
             Transformation combinations additive mode property
            }
            IPropAdditiveMode = class(TWSVGProperty)
                public type
                    {**
                     Additive mode enumeration
                     @value(IE_AT_Replace The first value should replace the second)
                     @value(IE_AT_Sum The first value should be added to the second)
                    }
                    IEType =
                    (
                        IE_AT_Replace,
                        IE_AT_Sum
                    );

                private
                    m_Type: IEType;

                public
                    {**
                     Constructor
                     @param(pParent Parent item, orphan or root if @nil)
                     @param(pOptions SVG options)
                    }
                    constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Assign (i.e. copy) content from another item
                     @param(pOther Other item to copy from)
                    }
                    procedure Assign(const pOther: TWSVGItem); override;

                    {**
                     Clear
                    }
                    procedure Clear; override;

                    {**
                     Create new element instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Element instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Log content
                     @param(margin Margin length in chars)
                    }
                    procedure Log(margin: Cardinal); override;

                    {**
                     Print content to string
                     @param(margin Margin length in chars)
                     @returns(Content)
                    }
                    function Print(margin: Cardinal): UnicodeString; override;

                    {**
                     Get xml formatted string
                     @returns(String)
                    }
                    function ToXml: UnicodeString; override;

                    {**
                     Convert type to string
                     @param(type Type to convert)
                     @returns(Converted string)
                    }
                    class function TypeToStr(transformType: IEType): UnicodeString; static;

                    {**
                     Convert string to type
                     @param(str String containing type to convert)
                     @returns(Converted type)
                    }
                    class function StrToType(const str: UnicodeString): IEType; static;

                public
                    {**
                     Get or set the transformation type
                    }
                    property AdditiveType: IEType read m_Type write m_Type;
            end;

        private
            m_Type:              IEAnimType;
            m_ValueType:         TWSVGCommon.IEValueType;
            m_ForcedToAnimColor: Boolean;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another item
             @param(pOther Other item to copy from)
            }
            procedure Assign(const pOther: TWSVGItem); override;

            {**
             Clear
            }
            procedure Clear; override;

            {**
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

            {**
             Read SVG element from xml
             @param(pNode Xml node containing SVG element to read)
             @returns(@true on success, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function Read(const pNode: TXMLNode): Boolean; override;
            {$else}
                function Read(const pNode: IXMLNode): Boolean; override;
            {$endif}

            {**
             Log content
             @param(margin Margin length in chars)
            }
            procedure Log(margin: Cardinal); override;

            {**
             Print content to string
             @param(margin Margin length in chars)
             @returns(Content)
            }
            function Print(margin: Cardinal): UnicodeString; override;

            {**
             Get xml formatted string
             @returns(String)
            }
            function ToXml: UnicodeString; override;

            {**
             Convert animation type to string
             @param(animType Type to convert)
             @returns(Converted string)
             @raises(Exception on unknown type)
            }
            class function TypeToStr(animType: IEAnimType): UnicodeString; static;

            {**
             Convert string to animation type
             @param(str String containing type to convert)
             @returns(Converted type)
            }
            class function StrToType(const str: UnicodeString): IEAnimType; static;

        public
            {**
             Get or set the animation type
            }
            property AnimationType: IEAnimType read m_Type write m_Type;

            {**
             Get or set the value type
            }
            property ValueType: TWSVGCommon.IEValueType read m_ValueType write m_ValueType;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropAttributeName
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropAttributeName.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropAttributeName.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeName.Assign(const pOther: TWSVGItem);
var
    pSource: IPropAttributeName;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropAttributeName)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropAttributeName;

    // copy data from source
    m_AttributeName := pSource.m_AttributeName;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeName.Clear;
begin
    inherited Clear;

    m_AttributeName := '';
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeName.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropAttributeName.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeName.Parse(const data: UnicodeString): Boolean;
begin
    m_AttributeName := data;
    Result          := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeName.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + m_AttributeName);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeName.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_AttributeName + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeName.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + m_AttributeName + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropAttributeType
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropAttributeType.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_AT_Auto;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropAttributeType.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeType.Assign(const pOther: TWSVGItem);
var
    pSource: IPropAttributeType;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropAttributeType)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropAttributeType;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeType.Clear;
begin
    inherited Clear;

    m_Type := IE_AT_Auto;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeType.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropAttributeType.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeType.Parse(const data: UnicodeString): Boolean;
begin
    m_Type := StrToType(data);
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAttributeType.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + TypeToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeType.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + TypeToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAttributeType.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + TypeToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAttributeType.TypeToStr(attributeType: IEAttributeType): UnicodeString;
begin
    case (attributeType) of
        IE_AT_CSS:  Exit(C_SVG_Animation_Attribute_Type_CSS);
        IE_AT_XML:  Exit(C_SVG_Animation_Attribute_Type_XML);
    else
        Exit(C_SVG_Animation_Attribute_Type_Auto);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAttributeType.StrToType(const str: UnicodeString): IEAttributeType;
begin
    if (str = C_SVG_Animation_Attribute_Type_CSS) then
        Exit(IE_AT_CSS)
    else
    if (str = C_SVG_Animation_Attribute_Type_XML) then
        Exit(IE_AT_XML)
    else
        Exit(IE_AT_Auto);
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropCalcMode
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropCalcMode.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_CT_Linear;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropCalcMode.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropCalcMode.Assign(const pOther: TWSVGItem);
var
    pSource: IPropCalcMode;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropCalcMode)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropCalcMode;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropCalcMode.Clear;
begin
    inherited Clear;

    m_Type := IE_CT_Linear;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropCalcMode.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropCalcMode.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropCalcMode.Parse(const data: UnicodeString): Boolean;
begin
    m_Type := StrToType(data);
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropCalcMode.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + TypeToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropCalcMode.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + TypeToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropCalcMode.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + TypeToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropCalcMode.TypeToStr(calcModeType: IECalcModeType): UnicodeString;
begin
    case (calcModeType) of
        IE_CT_Discrete: Exit(C_SVG_Animation_Calc_Mode_Discrete);
        IE_CT_Linear:   Exit(C_SVG_Animation_Calc_Mode_Linear);
        IE_CT_Paced:    Exit(C_SVG_Animation_Calc_Mode_Paced);
        IE_CT_Spline:   Exit(C_SVG_Animation_Calc_Mode_Spline);
    else
        raise Exception.CreateFmt('"Unknown calculation mode - %d', [Integer(calcModeType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropCalcMode.StrToType(const str: UnicodeString): IECalcModeType;
begin
    if (str = C_SVG_Animation_Calc_Mode_Discrete) then
        Exit(IE_CT_Discrete)
    else
    if (str = C_SVG_Animation_Calc_Mode_Linear) then
        Exit(IE_CT_Linear)
    else
    if (str = C_SVG_Animation_Calc_Mode_Paced) then
        Exit(IE_CT_Paced)
    else
    if (str = C_SVG_Animation_Calc_Mode_Spline) then
        Exit(IE_CT_Spline)
    else
    begin
        TWLogHelper.LogToCompiler('Calculation mode - string to type - unknown value - ' + str
                + ' - set to linear');
        Exit(IE_CT_Linear);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropFillMode
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropFillMode.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Mode := IE_FM_Unknown;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropFillMode.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropFillMode.Assign(const pOther: TWSVGItem);
var
    pSource: IPropFillMode;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropFillMode)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropFillMode;

    // copy data from source
    m_Mode := pSource.m_Mode;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropFillMode.Clear;
begin
    inherited Clear;

    m_Mode := IE_FM_Unknown;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropFillMode.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropFillMode.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropFillMode.Parse(const data: UnicodeString): Boolean;
begin
    m_Mode := StrToMode(data);
    Result := (m_Mode <> IE_FM_Unknown);
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropFillMode.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + ModeToStr(m_Mode));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropFillMode.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ModeToStr(m_Mode) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropFillMode.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ModeToStr(m_Mode) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropFillMode.ModeToStr(mode: IEMode): UnicodeString;
begin
    case (mode) of
        IE_FM_Freeze:  Exit(C_SVG_Animation_Fill_Mode_Freeze);
        IE_FM_Remove:  Exit(C_SVG_Animation_Fill_Mode_Remove);
    else
        raise Exception.CreateFmt('Unknown fill mode - %d', [Integer(mode)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropFillMode.StrToMode(const str: UnicodeString): IEMode;
begin
    if (str = C_SVG_Animation_Fill_Mode_Freeze) then
        Exit(IE_FM_Freeze)
    else
    if (str = C_SVG_Animation_Fill_Mode_Remove) then
        Exit(IE_FM_Remove)
    else
        Exit(IE_FM_Unknown)
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropRepeatCount
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropRepeatCount.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Count        := 0;
    m_PartialCount := 0;
    m_Indefinite   := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropRepeatCount.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRepeatCount.Assign(const pOther: TWSVGItem);
var
    pSource: IPropRepeatCount;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropRepeatCount)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropRepeatCount;

    // copy data from source
    m_Count        := pSource.m_Count;
    m_PartialCount := pSource.m_PartialCount;
    m_Indefinite   := pSource.m_Indefinite;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRepeatCount.Clear;
begin
    inherited Clear;

    m_Count        := 0;
    m_PartialCount := 0;
    m_Indefinite   := False;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRepeatCount.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropRepeatCount.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRepeatCount.Parse(const data: UnicodeString): Boolean;
var
    fraction: Single;
begin
    // is repeat count indefinite?
    if (data = C_SVG_Animation_Indefinite) then
    begin
        m_Count        := 0;
        m_PartialCount := 0;
        m_Indefinite   := True;
        Exit(True);
    end;

    // calculate complete and partial animation cycle
    fraction       := StrToFloat(data, g_InternationalFormatSettings);
    m_Count        := Floor(fraction);
    m_PartialCount := Trunc((fraction - m_Count) * 100.0);
    m_Indefinite   := False;
    Result         := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRepeatCount.Log(margin: Cardinal);
var
    fraction: Single;
begin
    // is repeat count indefinite?
    if (m_Indefinite) then
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + C_SVG_Animation_Indefinite)
    else
    // contains partial animation?
    if (m_PartialCount > 0) then
    begin
        // calculate complete and partial count fraction
        fraction := (m_Count + (m_PartialCount / 100.0));
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + FloatToStr(fraction));
    end
    else
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + IntToStr(m_Count));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRepeatCount.Print(margin: Cardinal): UnicodeString;
var
    fraction: Single;
begin
    // is repeat count indefinite?
    if (m_Indefinite) then
        Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + C_SVG_Animation_Indefinite
    else
    // contains partial animation?
    if (m_PartialCount > 0) then
    begin
        // calculate complete and partial count fraction
        fraction := (m_Count + (m_PartialCount / 100.0));
        Result   := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + FloatToStr(fraction);
    end
    else
        Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + IntToStr(m_Count);

    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRepeatCount.ToXml: UnicodeString;
var
    fraction: Single;
begin
    // is repeat count indefinite?
    if (m_Indefinite) then
        Result := ItemName + '=\"' + C_SVG_Animation_Indefinite + '\"'
    else
    // contains partial animation?
    if (m_PartialCount > 0) then
    begin
        // calculate complete and partial count fraction
        fraction := (m_Count + (m_PartialCount / 100.0));
        Result   := ItemName + '=\"' + FloatToStr(fraction) + '\"';
    end
    else
        Result := ItemName + '=\"' + IntToStr(m_Count) + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropRestart
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropRestart.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_RT_Unknown;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropRestart.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRestart.Assign(const pOther: TWSVGItem);
var
    pSource: IPropRestart;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropRestart)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropRestart;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRestart.Clear;
begin
    inherited Clear;

    m_Type := IE_RT_Unknown;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRestart.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropRestart.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRestart.Parse(const data: UnicodeString): Boolean;
begin
    m_Type := StrToType(data);
    Result := (m_Type <> IE_RT_Unknown);
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropRestart.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + TypeToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRestart.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + TypeToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropRestart.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + TypeToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropRestart.TypeToStr(restartType: IERestartType): UnicodeString;
begin
    case (restartType) of
        IE_RT_Always:        Exit(C_SVG_Animation_Restart_Always);
        IE_RT_WhenNotActive: Exit(C_SVG_Animation_Restart_When_Not_Active);
        IE_RT_Never:         Exit(C_SVG_Animation_Restart_Never);
    else
        raise Exception.CreateFmt('Unknown restart type - %d', [Integer(restartType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropRestart.StrToType(const str: UnicodeString): IERestartType;
begin
    if (str = C_SVG_Animation_Restart_Always) then
        Exit(IE_RT_Always)
    else
    if (str = C_SVG_Animation_Restart_When_Not_Active) then
        Exit(IE_RT_WhenNotActive)
    else
    if (str = C_SVG_Animation_Restart_Never) then
        Exit(IE_RT_Never)
    else
        Exit(IE_RT_Unknown);
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropAnimTransformType
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropAnimTransformType.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_TT_Unknown;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropAnimTransformType.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAnimTransformType.Assign(const pOther: TWSVGItem);
var
    pSource: IPropAnimTransformType;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropAnimTransformType)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropAnimTransformType;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAnimTransformType.Clear;
begin
    inherited Clear;

    m_Type := IE_TT_Unknown;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAnimTransformType.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropAnimTransformType.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAnimTransformType.Parse(const data: UnicodeString): Boolean;
begin
    m_Type := StrToType(data);
    Result := (m_Type <> IE_TT_Unknown);
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAnimTransformType.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + TypeToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAnimTransformType.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + TypeToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAnimTransformType.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + TypeToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAnimTransformType.TypeToStr(transformType: IETransformType): UnicodeString;
begin
    case (transformType) of
        IE_TT_Translate: Exit(C_SVG_Animation_Transform_Type_Translate);
        IE_TT_Scale:     Exit(C_SVG_Animation_Transform_Type_Scale);
        IE_TT_Rotate:    Exit(C_SVG_Animation_Transform_Type_Rotate);
        IE_TT_SkewX:     Exit(C_SVG_Animation_Transform_Type_SkewX);
        IE_TT_SkewY:     Exit(C_SVG_Animation_Transform_Type_SkewY);
    else
        raise Exception.CreateFmt('Unknown animate transform type - %d', [Integer(transformType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAnimTransformType.StrToType(const str: UnicodeString): IETransformType;
begin
    if (str = C_SVG_Animation_Transform_Type_Translate) then
        Exit(IE_TT_Translate)
    else
    if (str = C_SVG_Animation_Transform_Type_Scale) then
        Exit(IE_TT_Scale)
    else
    if (str = C_SVG_Animation_Transform_Type_Rotate) then
        Exit(IE_TT_Rotate)
    else
    if (str = C_SVG_Animation_Transform_Type_SkewX) then
        Exit(IE_TT_SkewX)
    else
    if (str = C_SVG_Animation_Transform_Type_SkewY) then
        Exit(IE_TT_SkewY)
    else
        Exit(IE_TT_Unknown);
end;
//---------------------------------------------------------------------------
// TWSVGAnimation.IPropAdditiveMode
//---------------------------------------------------------------------------
constructor TWSVGAnimation.IPropAdditiveMode.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_AT_Replace;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.IPropAdditiveMode.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAdditiveMode.Assign(const pOther: TWSVGItem);
var
    pSource: IPropAdditiveMode;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropAdditiveMode)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropAdditiveMode;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAdditiveMode.Clear;
begin
    inherited Clear;

    m_Type := IE_AT_Replace;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAdditiveMode.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropAdditiveMode.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAdditiveMode.Parse(const data: UnicodeString): Boolean;
begin
    m_Type := StrToType(data);
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.IPropAdditiveMode.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + TypeToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAdditiveMode.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + TypeToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.IPropAdditiveMode.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + TypeToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAdditiveMode.TypeToStr(transformType: IEType): UnicodeString;
begin
    case (transformType) of
        IE_AT_Replace: Exit(C_SVG_Animation_Additive_Replace);
        IE_AT_Sum:     Exit(C_SVG_Animation_Additive_Sum);
    else
        raise Exception.CreateFmt('Unknown animate addtive type - %d', [Integer(transformType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.IPropAdditiveMode.StrToType(const str: UnicodeString): IEType;
begin
    if (str = C_SVG_Animation_Additive_Sum) then
        Exit(IE_AT_Sum);

    Result := IE_AT_Replace;
end;
//---------------------------------------------------------------------------
// TWSVGAnimation
//---------------------------------------------------------------------------
constructor TWSVGAnimation.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type              := IE_AT_Unknown;
    m_ValueType         := TWSVGCommon.IEValueType.IE_VT_Unknown;
    m_ForcedToAnimColor := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimation.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGAnimation;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGAnimation)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGAnimation;

    // copy data from source
    m_Type      := pSource.m_Type;
    m_ValueType := pSource.m_ValueType;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.Clear;
begin
    inherited Clear;

    m_Type      := IE_AT_Unknown;
    m_ValueType := TWSVGCommon.IEValueType.IE_VT_Unknown;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGAnimation.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGAnimation.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGAnimation.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pAttribName:                                                 IPropAttributeName;
    pAttribType:                                                 IPropAttributeType;
    pCalcMode:                                                   IPropCalcMode;
    pRepeatCount:                                                IPropRepeatCount;
    pFillMode:                                                   IPropFillMode;
    pColorValues, pFromAsColor, pToAsColor:                      TWSVGPropColor;
    pEnumValues, pFromAsEnum, pToAsEnum:                         TWSVGPropEnum;
    pFromAsValues, pToAsValues, pValues, pKeySplines, pKeyTimes: TWSVGAttribute<Single>;
    pBegin, pEnd, pDuration, pMin, pMax, pRepeatDur:             TWSVGPropTime;
    pRestart:                                                    IPropRestart;
    pAnimTransformType:                                          IPropAnimTransformType;
    pAdditiveMode:                                               IPropAdditiveMode;
    attribName:                                                  UnicodeString;
    useValues:                                                   Boolean;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    // get animation type (it's the xml tag name too)
    {$ifdef USE_VERYSIMPLEXML}
        // trim all CRLF chars because very simple xml parser will not do that
        ItemName := StringReplace(StringReplace(pNode.NodeName, #10, '', [rfReplaceAll]), #13, '',
                [rfReplaceAll]);
    {$else}
        ItemName := pNode.NodeName;
    {$endif}
    m_Type       := StrToType(ItemName);

    // is element of type animation?
    if (m_Type = IE_AT_Unknown) then
        Exit(False);

    pAttribName := nil;

    try
        pAttribName := IPropAttributeName.Create(Self, m_pOptions);

        // read attribute name (if fails, stop reading because an animation should not be orphan)
        if (not pAttribName.Read(C_SVG_Animation_Attribute_Name, pNode)) then
            Exit(False);

        // get the attribute name
        attribName := pAttribName.AttributeName;

        // get the value type that the animation will process (read, color, matrix, ...)
        m_ValueType := TWSVGCommon.GetType(attribName);

        // add property to list
        m_pProperties.Add(pAttribName);
        pAttribName := nil;
    finally
        pAttribName.Free;
    end;

    case (m_ValueType) of
        TWSVGCommon.IEValueType.IE_VT_Value,
        TWSVGCommon.IEValueType.IE_VT_Matrix:
        begin
            useValues := False;
            pValues   := nil;

            try
                pValues := TWSVGAttribute<Single>.Create(Self, m_pOptions);

                // read animate transform values property (optional)
                if (pValues.Read(C_SVG_Animation_Values, pNode)) then
                begin
                    useValues := True;
                    m_pProperties.Add(pValues);
                    pValues   := nil;
                end;
            finally
                pValues.Free;
            end;

            // in the context of an animation, from, to and by properties should be ignored if a
            // value list is provided, see:
            // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values
            if (not useValues) then
            begin
                pFromAsValues := nil;

                try
                    pFromAsValues := TWSVGAttribute<Single>.Create(Self, m_pOptions);

                    // read from property (optional)
                    if (pFromAsValues.Read(C_SVG_Animation_From, pNode)) then
                    begin
                        m_pProperties.Add(pFromAsValues);
                        pFromAsValues := nil;
                    end;
                finally
                    pFromAsValues.Free;
                end;

                pToAsValues := nil;

                try
                    pToAsValues := TWSVGAttribute<Single>.Create(Self, m_pOptions);

                    // read to property (optional)
                    if (pToAsValues.Read(C_SVG_Animation_To, pNode)) then
                    begin
                        m_pProperties.Add(pToAsValues);
                        pToAsValues := nil;
                    end;
                finally
                    pToAsValues.Free;
                end;
            end;
        end;

        TWSVGCommon.IEValueType.IE_VT_Color:
        begin
            // check if the animation type is well defined to animate color. On the older standard
            // version, a colored animation was supposed to be declared with the animateColor tag,
            // but in newer version this tag was marked as obsolete and discarded since the 2.0 
            // standards version. So force the type to be an animate color if the animation contains
            // color values but the type isn't defined to animate color
            if (m_Type <> IE_AT_Animate_Color) then
            begin
                ItemName            := C_SVG_Tag_Animate_Color;
                m_Type              := IE_AT_Animate_Color;
                m_ForcedToAnimColor := True;
            end;

            useValues    := False;
            pColorValues := nil;

            try
                pColorValues := TWSVGPropColor.Create(Self, m_pOptions);

                // read animate transform values property (optional)
                if (pColorValues.Read(C_SVG_Animation_Values, pNode)) then
                begin
                    useValues    := True;
                    m_pProperties.Add(pColorValues);
                    pColorValues := nil;
                end;
            finally
                pColorValues.Free;
            end;

            // in the context of an animation, from, to and by properties should be ignored if a
            // value list is provided, see:
            // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values
            if (not useValues) then
            begin
                pFromAsColor := nil;

                try
                    pFromAsColor := TWSVGPropColor.Create(Self, m_pOptions);

                    // read from property (optional)
                    if (pFromAsColor.Read(C_SVG_Animation_From, pNode)) then
                    begin
                        m_pProperties.Add(pFromAsColor);
                        pFromAsColor := nil;
                    end;
                finally
                    pFromAsColor.Free;
                end;

                pToAsColor := nil;

                try
                    pToAsColor := TWSVGPropColor.Create(Self, m_pOptions);

                    // read to property (optional)
                    if (pToAsColor.Read(C_SVG_Animation_To, pNode)) then
                    begin
                        m_pProperties.Add(pToAsColor);
                        pToAsColor := nil;
                    end;
                finally
                    pToAsColor.Free;
                end;
            end;
        end;

        TWSVGCommon.IEValueType.IE_VT_Enum:
        begin
            useValues   := False;
            pEnumValues := nil;

            try
                if (attribName = C_SVG_Prop_Display) then
                    pEnumValues := TWSVGStyle.IPropDisplay.Create(Self, m_pOptions)
                else
                if (attribName = C_SVG_Prop_Visibility) then
                    pEnumValues := TWSVGStyle.IPropVisibility.Create(Self, m_pOptions);

                // read animate transform values property (optional)
                if (Assigned(pEnumValues) and pEnumValues.Read(C_SVG_Animation_Values, pNode)) then
                begin
                    useValues      := True;
                    m_pProperties.Add(pEnumValues);
                    pEnumValues := nil;
                end;
            finally
                pEnumValues.Free;
            end;

            // in the context of an animation, from, to and by properties should be ignored if a value list
            // is provided, see:
            // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values
            if (not useValues) then
            begin
                pFromAsEnum := nil;

                try
                    if (attribName = C_SVG_Prop_Display) then
                        pFromAsEnum := TWSVGStyle.IPropDisplay.Create(Self, m_pOptions)
                    else
                    if (attribName = C_SVG_Prop_Visibility) then
                        pFromAsEnum := TWSVGStyle.IPropVisibility.Create(Self, m_pOptions);

                    // read from property (optional)
                    if (Assigned(pFromAsEnum) and pFromAsEnum.Read(C_SVG_Animation_From, pNode)) then
                    begin
                        m_pProperties.Add(pFromAsEnum);
                        pFromAsEnum := nil;
                    end;
                finally
                    pFromAsEnum.Free;
                end;

                pToAsEnum := nil;

                try
                    if (attribName = C_SVG_Prop_Display) then
                        pToAsEnum := TWSVGStyle.IPropDisplay.Create(Self, m_pOptions)
                    else
                    if (attribName = C_SVG_Prop_Visibility) then
                        pToAsEnum := TWSVGStyle.IPropVisibility.Create(Self, m_pOptions);

                    // read to property (optional)
                    if (Assigned(pToAsEnum) and pToAsEnum.Read(C_SVG_Animation_To, pNode)) then
                    begin
                        m_pProperties.Add(pToAsEnum);
                        pToAsEnum := nil;
                    end;
                finally
                    pToAsEnum.Free;
                end;
            end;
        end;
    else
        {$ifdef DEBUG}
            TWLogHelper.LogToCompiler('Read animation - found unknown or unsupported data type - '
                    + IntToStr(Integer(m_ValueType)) + ' - attribute name - ' + attribName);
        {$endif}
    end;

    pAttribType := nil;

    try
        pAttribType := IPropAttributeType.Create(Self, m_pOptions);

        // read attribute type
        if (pAttribType.Read(C_SVG_Animation_Attribute_Type, pNode)) then
        begin
            m_pProperties.Add(pAttribType);
            pAttribType := nil;
        end;
    finally
        pAttribType.Free;
    end;

    pCalcMode := nil;

    try
        pCalcMode := IPropCalcMode.Create(Self, m_pOptions);

        // read calculation mode (optional)
        if (pCalcMode.Read(C_SVG_Animation_Calc_Mode, pNode)) then
        begin
            m_pProperties.Add(pCalcMode);
            pCalcMode := nil;
        end;
    finally
        pCalcMode.Free;
    end;

    pRepeatCount := nil;

    try
        pRepeatCount := IPropRepeatCount.Create(Self, m_pOptions);

        // read repeat count
        if (pRepeatCount.Read(C_SVG_Animation_Repeat_Count, pNode)) then
        begin
            m_pProperties.Add(pRepeatCount);
            pRepeatCount := nil;
        end;
    finally
        pRepeatCount.Free;
    end;

    pFillMode := nil;

    try
        pFillMode := IPropFillMode.Create(Self, m_pOptions);

        // read fill mode (optional)
        if (pFillMode.Read(C_SVG_Animation_Fill_Mode, pNode)) then
        begin
            m_pProperties.Add(pFillMode);
            pFillMode := nil;
        end;
    finally
        pFillMode.Free;
    end;

    pBegin := nil;

    try
        pBegin := TWSVGPropTime.Create(Self, m_pOptions);

        // read begin property (optional)
        if (pBegin.Read(C_SVG_Animation_Begin, pNode)) then
        begin
            m_pProperties.Add(pBegin);
            pBegin := nil;
        end;
    finally
        pBegin.Free;
    end;

    pEnd := nil;

    try
        pEnd := TWSVGPropTime.Create(Self, m_pOptions);

        // read end property (optional)
        if (pEnd.Read(C_SVG_Animation_End, pNode)) then
        begin
            m_pProperties.Add(pEnd);
            pEnd := nil;
        end;
    finally
        pEnd.Free;
    end;

    pDuration := nil;

    try
        pDuration := TWSVGPropTime.Create(Self, m_pOptions);

        // read duration property (optional)
        if (pDuration.Read(C_SVG_Animation_Duration, pNode)) then
        begin
            m_pProperties.Add(pDuration);
            pDuration := nil;
        end;
    finally
        pDuration.Free;
    end;

    pMin := nil;

    try
        pMin := TWSVGPropTime.Create(Self, m_pOptions);

        // read min property (optional)
        if (pMin.Read(C_SVG_Animation_Min, pNode)) then
        begin
            m_pProperties.Add(pMin);
            pMin := nil;
        end;
    finally
        pMin.Free;
    end;

    pMax := nil;

    try
        pMax := TWSVGPropTime.Create(Self, m_pOptions);

        // read max property (optional)
        if (pMax.Read(C_SVG_Animation_Max, pNode)) then
        begin
            m_pProperties.Add(pMax);
            pMax := nil;
        end;
    finally
        pMax.Free;
    end;

    pRepeatDur := nil;

    try
        pRepeatDur := TWSVGPropTime.Create(Self, m_pOptions);

        // read repeat duration property (optional)
        if (pRepeatDur.Read(C_SVG_Animation_Repeat_Duration, pNode)) then
        begin
            m_pProperties.Add(pRepeatDur);
            pRepeatDur := nil;
        end;
    finally
        pRepeatDur.Free;
    end;

    pRestart := nil;

    try
        pRestart := IPropRestart.Create(Self, m_pOptions);

        // read restart property (optional)
        if (pRestart.Read(C_SVG_Animation_Restart, pNode)) then
        begin
            m_pProperties.Add(pRestart);
            pRestart := nil;
        end;
    finally
        pRestart.Free;
    end;

    pAnimTransformType := nil;

    try
        pAnimTransformType := IPropAnimTransformType.Create(Self, m_pOptions);

        // read animate transform type property (optional)
        if (pAnimTransformType.Read(C_SVG_Animation_Transform_Type, pNode)) then
        begin
            m_pProperties.Add(pAnimTransformType);
            pAnimTransformType := nil;
        end;
    finally
        pAnimTransformType.Free;
    end;

    pKeySplines := nil;

    try
        pKeySplines := TWSVGAttribute<Single>.Create(Self, m_pOptions);

        // read key splines property (optional)
        if (pKeySplines.Read(C_SVG_Animation_Key_Splines, pNode)) then
        begin
            m_pProperties.Add(pKeySplines);
            pKeySplines := nil;
        end;
    finally
        pKeySplines.Free;
    end;

    pKeyTimes := nil;

    try
        pKeyTimes := TWSVGAttribute<Single>.Create(Self, m_pOptions);

        // read key times property (optional) NOTE key time is read as folating point list because times
        // are represented by a value in percent (between 0 and 1)
        if (pKeyTimes.Read(C_SVG_Animation_Key_Times, pNode)) then
        begin
            m_pProperties.Add(pKeyTimes);
            pKeyTimes := nil;
        end;
    finally
        pKeyTimes.Free;
    end;

    pAdditiveMode := nil;

    try
        pAdditiveMode := IPropAdditiveMode.Create(Self, m_pOptions);

        // read additive mode property (optional)
        if (pAdditiveMode.Read(C_SVG_Animation_Additive, pNode)) then
        begin
            m_pProperties.Add(pAdditiveMode);
            pAdditiveMode := nil;
        end;
    finally
        pAdditiveMode.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimation.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Animation ');
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight('type', margin, ' ') + ' - '
            + TypeToStr(m_Type));
    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Animation>' + #13 + #10;
    Result := Result + TWStringHelper.FillStrRight('type', margin, ' ');
    Result := Result + ' - ';
    Result := Result + TypeToStr(m_Type);
    Result := Result + #13 + #10;
    Result := Result + inherited Print(margin);
    Result := Result + '</Animation>' + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAnimation.ToXml: UnicodeString;
begin
    Result := '<' + ItemName + ' ' + inherited ToXml + '/>';
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.TypeToStr(animType: IEAnimType): UnicodeString;
begin
    case (animType) of
        IE_AT_Set:               Exit(C_SVG_Tag_Set);
        IE_AT_Animate:           Exit(C_SVG_Tag_Animate);
        IE_AT_Animate_Color:     Exit(C_SVG_Tag_Animate_Color);
        IE_AT_Animate_Transform: Exit(C_SVG_Tag_Animate_Transform);
        IE_AT_Animate_Motion:    Exit(C_SVG_Tag_Animate_Motion);
    else
        raise Exception.CreateFmt('Unknown animation type - %d', [Integer(animType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGAnimation.StrToType(const str: UnicodeString): IEAnimType;
begin
    if (str = C_SVG_Tag_Set) then
        Exit(IE_AT_Set)
    else
    if (str = C_SVG_Tag_Animate) then
        Exit(IE_AT_Animate)
    else
    if (str = C_SVG_Tag_Animate_Color) then
        Exit(IE_AT_Animate_Color)
    else
    if (str = C_SVG_Tag_Animate_Transform) then
        Exit(IE_AT_Animate_Transform)
    else
    if (str = C_SVG_Tag_Animate_Motion) then
        Exit(IE_AT_Animate_Motion)
    else
        Exit(IE_AT_Unknown);
end;
//---------------------------------------------------------------------------

end.
