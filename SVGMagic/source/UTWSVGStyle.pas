{**
 @abstract(@name provides the class to parse the SVG styles)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGStyle;
{$I SVGMagic.inc}

interface

uses System.SysUtils,
     Vcl.Graphics,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWHelpers,
     UTWColor,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGMeasure,
     UTWSVGAttribute,
     UTWSVGItems,
     UTWSVGProperties;

const
    //---------------------------------------------------------------------------
    // Global defines
    //---------------------------------------------------------------------------
    C_SVG_Default_Fill_Solid_Color:   TColor = clBlack;
    C_SVG_Default_Stroke_Solid_Color: TColor = clBlack;
    //---------------------------------------------------------------------------

type
    {**
     Style fill properties
     @br @bold(NOTE)A fill attribute may either contains a solid color value, or a link to a local
                    or global define. The property may also indicate explicitly that no fill must be
                    processed. Be careful, the color is a RGB color, the fill opacity level is
                    written in another property (called "fill-opacity")
    }
    TWSVGFill = class
        public type
            {**
             Fill rule enumeration
             @value(IE_FR_Default Default fill rule (i.e. nonzero) is used, or inherited from parent)
             @value(IE_FR_NonZero This value determines the "insideness" of a point in the shape by
                                  drawing a ray from that point to infinity in any direction and then
                                  examining the places where a segment of the shape crosses the ray.
                                  Starting with a count of zero, add one each time a path segment
                                  crosses the ray from left to right and subtract one each time a
                                  path segment crosses the ray from right to left. After counting
                                  the crossings, if the result is zero then the point is outside the
                                  path. Otherwise, it is inside)
             @value(IE_FR_EvenOdd This value determines the "insideness" of a point in the shape by
                                  drawing a ray from that point to infinity in any direction and
                                  counting the number of path segments from the given shape that the
                                  ray crosses. If this number is odd, the point is inside; if even,
                                  the point is outside)
            }
            IERule =
            (
                IE_FR_Default,
                IE_FR_NonZero,
                IE_FR_EvenOdd
            );

            {**
            * Fill rule property
            }
            IPropRule = class(TWSVGProperty)
                private
                    m_Rule: IERule;

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
                     Create new property instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Property instance)
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
                     Convert fill rule to string
                     @param(rule Fill rule to convert)
                     @returns(Fill rule as string)
                    }
                    class function ToStr(rule: IERule): UnicodeString; static;

                public
                    {**
                     Get or set the rule
                    }
                    property Rule: IERule read m_Rule write m_Rule;
            end;

        private
            m_pProperties: TWSVGElement.IProperties;
            m_pOptions:    PWSVGOptions;
            m_NoFill:      Boolean;

            {**
            * Delete and clear all data
            }
            procedure DelAndClear;

        protected
            {**
             Get property at index
             @param(index Property index)
             @returns(Property, @nil if not found or on error)
            }
            function GetProperty(index: Integer): TWSVGProperty; virtual;

            {**
             Get property count
             @returns(Property count)
            }
            function GetPropertyCount: Integer; virtual;

        public
            {**
             Constructor
             @param(pOptions SVG options)
            }
            constructor Create(pOptions: PWSVGOptions); virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another fill
             @param(pOther Other fill properties to copy from)
             @param(pParent Fill parent style)
            }
            procedure Assign(const pOther: TWSVGFill; pParent: TWSVGItem); virtual;

            {**
             Clear all fill properties
            }
            procedure Clear; virtual;

            {**
             Parse the property content
             @param(name Property name to parse)
             @param(value Property value)
             @param(pParent Parent item at which the new properties will belong)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const name, value: UnicodeString; pParent: TWSVGItem): Boolean; virtual;

            {**
             Log content
             @param(margin Margin length in chars)
            }
            procedure Log(margin: Cardinal); virtual;

            {**
             Print content to string
             @param(margin Margin length in chars)
             @returns(Content)
            }
            function Print(margin: Cardinal): UnicodeString; virtual;

            {**
             Get xml formatted string
             @param(count @bold([in, out]) Number of parsed properties)
             @returns(String)
            }
            function ToXml(var count: NativeUInt): UnicodeString; virtual;

        public
            {**
             Get or set if no fill should be processed
            }
            property NoFill: Boolean read m_NoFill write m_NoFill;

            {**
             Get the property at index. Example: property := Properties[0];
             @br @bold(NOTE) @nil will be returned if index is out of bounds
            }
            property Properties[index: Integer]: TWSVGProperty read GetProperty;

            {**
             Get the properties count
            }
            property Count: Integer read GetPropertyCount;
    end;

    {**
     Style stroke properties
     @br @bold(NOTE) A stroke attribute may either contains a solid color value, or a link to a
                     local or global define. The property may also indicate explicitly that no stroke
                     must be processed. Be careful, the color is a RGB color, the stroke opacity
                     level is written in another property (called "stroke-opacity")
    }
    TWSVGStroke = class
        public type
            {**
             Linecap enumeration
             @value(IE_LC_Default Use default line cap. @bold(NOTE) by default linecap is inherited from parent)
             @value(IE_LC_Butt Ends the line without termination shape)
             @value(IE_LC_Round Ends the line with a round)
             @value(IE_LC_Square Ends the line with a square)
            }
            IELineCap =
            (
                IE_LC_Default,
                IE_LC_Butt,
                IE_LC_Round,
                IE_LC_Square
            );

            {**
             Linecap property
            }
            IPropLineCap = class(TWSVGProperty)
                private
                    m_Type: IELineCap;

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
                     Create new property instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Property instance)
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
                     Convert line cap to string
                     @param(lineCap Line cap to convert)
                     @returns(Line cap as string)
                    }
                    class function ToStr(lineCap: IELineCap): UnicodeString; static;

                public
                    {**
                     Get or set the line cap
                    }
                    property LineCap: IELineCap read m_Type write m_Type;
            end;

            {**
             Line join enumeration
             @value(IE_LJ_Default Use default line join. @bold(NOTE) by default line join is inherited from parent)
             @value(IE_LJ_Miter Use square shape to stroke the lines)
             @value(IE_LJ_Round Use round shape to stroke the lines)
             @value(IE_LJ_Bevel Use no shape to stroke the lines)
            }
            IELineJoin =
            (
                IE_LJ_Default,
                IE_LJ_Miter,
                IE_LJ_Round,
                IE_LJ_Bevel
            );

            {**
             Line join property
            }
            IPropLineJoin = class(TWSVGProperty)
                private
                    m_Type: IELineJoin;

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
                     Create new property instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Property instance)
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
                     Convert line join to string
                     @param(lineJoin Line join to convert)
                     @returns(Line join as string)
                    }
                    class function ToStr(lineJoin: IELineJoin): UnicodeString; static;

                public
                    {**
                     Get or set the line join
                    }
                    property LineJoin: IELineJoin read m_Type write m_Type;
            end;

        private
            m_pProperties: TWSVGElement.IProperties;
            m_pOptions:    PWSVGOptions;
            m_NoStroke:    Boolean;

            {**
             Delete and clear all data
            }
            procedure DelAndClear;

        protected
            {**
             Get property at index
             @param(index Property index)
             @returns(Property, @nil if not found or on error)
            }
            function GetProperty(index: Integer): TWSVGProperty; virtual;

            {**
             Get property count
             @returns(Property count)
            }
            function GetPropertyCount: Integer; virtual;

        public
            {**
             Constructor
             @param(pOptions SVG options)
            }
            constructor Create(pOptions: PWSVGOptions); virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another stroke
             @param(pOther Other stroke properties to copy from)
             @param(pParent Stroke parent style)
            }
            procedure Assign(const pOther: TWSVGStroke; pParent: TWSVGItem); virtual;

            {**
             Clear all stroke properties
            }
            procedure Clear; virtual;

            {**
             Parse the property content
             @param(name Property name to parse)
             @param(value Property value)
             @param(pParent Parent item at which the new properties will belong)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const name, value: UnicodeString; pParent: TWSVGItem): Boolean; virtual;

            {**
             Log content
             @param(margin Margin length in chars)
            }
            procedure Log(margin: Cardinal); virtual;

            {**
             Print content to string
             @param(margin Margin length in chars)
             @returns(Content)
            }
            function Print(margin: Cardinal): UnicodeString; virtual;

            {**
             Get xml formatted string
             @param(count @bold([in, out]) Number of parsed properties)
             @returns(String)
            }
            function ToXml(var count: NativeUInt): UnicodeString; virtual;

        public
            {**
             Get or set if no stroke should be processed
             @br @bold(NOTE) BE CAREFUL, this will be set to true if, and only if the stroke
                             property is explicitly set to 'none'. This distinction is important
                             because a stroke property is also considered as 'none' by default,
                             but in this case the inheritance rules are applied, while the stroke
                             will be effectively applied with an empty color, and will not inherit
                             from its parent, if this property is set to true
            }
            property NoStroke: Boolean read m_NoStroke write m_NoStroke;

            {**
             Get the property at index. Example: property := Properties[0];
             @br @bold(NOTE) @nil will be returned if index is out of bounds
            }
            property Properties[index: Integer]: TWSVGProperty read GetProperty;

            {**
             Get the properties count
            }
            property Count: Integer read GetPropertyCount;
    end;

    {**
     A Scalable Vector Graphics (SVG) style is a set of properties that affect the manner how a
     drawing will be made. A style should at least contain a fill color and/or a stroke color, but
     may also contain several details about the drawing behavior, like e.g. the stroke miter limits,
     ... When an item isn't declared in the current style, its values are inherited from the closest
     parent style that exposes this item
    @br @bold(NOTE) The style may be exposed as a distinct property, e.g. <path style="fill:#8b4c4c;..." />,
                    or as a list of several properties, e.g. <path fill="#8b4c4c" ... />"
    }
    TWSVGStyle = class(TWSVGProperty)
        public type
            {**
             Display property
            }
            IPropDisplay = class(TWSVGPropEnum)
                public type
                    {**
                     Display enumeration
                     @value(IE_V_None Indicates that the element should not be rendered)
                     @value(IE_V_Inherit Indicates that the rendering of the element depends on the parent property)
                     @br @bold(NOTE) All other values means that the element should be rendered. These values
                                     may also affect the direct rendering in offscreen canvases
                    }
                    IEValue =
                    (
                        IE_V_Inline, // by default the display is inline
                        IE_V_Block,
                        IE_V_ListItem,
                        IE_V_RunIn,
                        IE_V_Compact,
                        IE_V_Marker,
                        IE_V_Table,
                        IE_V_InlineTable,
                        IE_V_TableRowGroup,
                        IE_V_TableHeaderGroup,
                        IE_V_TableFooterGroup,
                        IE_V_TableRow,
                        IE_V_TableColumnGroup,
                        IE_V_TableColumn,
                        IE_V_TableCell,
                        IE_V_TableCaption,
                        IE_V_None,
                        IE_V_Inherit
                    );

                protected
                    {**
                     Convert value to string
                     @param(value Value to convert)
                     @returns(Value as string)
                    }
                    function ValueToStr(value: Integer): UnicodeString; override;

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
                     Create new property instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Property instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Convert display value to string
                     @param(value Value to convert)
                     @returns(Value as string)
                    }
                    class function ToStr(value: IEValue): UnicodeString; static;
            end;

            {**
             Visibility property
            }
            IPropVisibility = class(TWSVGPropEnum)
                public type
                    {**
                     Value enumeration
                     @value(IE_V_Visible Indicates that the element will be painted)
                     @value(IE_V_Hidden Indicates that the element will not be painted)
                     @value(IE_V_Collapse This value is equal to hidden)
                    }
                    IEValue =
                    (
                        IE_V_Visible,
                        IE_V_Hidden,
                        IE_V_Collapse
                    );

                protected
                    {**
                     Convert value to string
                     @param(value Value to convert)
                     @returns(Value as string)
                    }
                    function ValueToStr(value: Integer): UnicodeString; override;

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
                     Create new property instance
                     @param(pParent Parent item, orphan or root if @nil)
                     @returns(Property instance)
                    }
                    function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

                    {**
                     Parse data
                     @param(data Data to parse)
                     @returns(@true on success, otherwise @false)
                    }
                    function Parse(const data: UnicodeString): Boolean; override;

                    {**
                     Convert visibility value to string
                     @param(value Value to convert)
                     @returns(Value as string)
                    }
                    class function ToStr(value: IEValue): UnicodeString; static;
            end;

        private
            m_pProperties: TWSVGElement.IProperties;
            m_pFill:       TWSVGFill;
            m_pStroke:     TWSVGStroke;

            {**
             Set value
             @param(name Value name)
             @param(value Value to set)
             @returns(@true on success, otherwise @false)
            }
            function SetValue(const name, value: UnicodeString): Boolean;

            {**
             Parse the global style properties content
             @param(name Property name to parse)
             @param(value Property value)
             @returns(@true on success, otherwise @false)
            }
            function ParseStyle(const name, value: UnicodeString): Boolean; overload;

            {**
             Delete and clear all data
            }
            procedure DelAndClear;

        protected
            {**
             Get property at index
             @param(index Property index)
             @returns(Property, @nil if not found or on error)
            }
            function GetProperty(index: Integer): TWSVGProperty; virtual;

            {**
             Get property count
             @returns(Property count)
            }
            function GetPropertyCount: Integer; virtual;

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
             Create new property instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Property instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

            {**
             Read property
             @param(name Xml tag name)
             @param(pNode Xml node containing property to read)
             @returns(@true on success, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function Read(const name: UnicodeString; const pNode: TXMLNode): Boolean; override;
            {$else}
                function Read(const name: UnicodeString; const pNode: IXMLNode): Boolean; override;
            {$endif}

            {**
             Parse data
             @param(data Data to parse)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const data: UnicodeString): Boolean; overload; override;

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
             Get the fill properties
            }
            property Fill: TWSVGFill read m_pFill;

            {**
             Get the stroke properties
            }
            property Stroke: TWSVGStroke read m_pStroke;

            {**
             Get the property at index. Example: property := Properties[0];
             @br @bold(NOTE) @nil will be returned if index is out of bounds
            }
            property Properties[index: Integer]: TWSVGProperty read GetProperty;

            {**
             Get the properties count
            }
            property Count: Integer read GetPropertyCount;
    end;

implementation

uses
  System.Classes;

//---------------------------------------------------------------------------
// TWSVGFill.IPropRule
//---------------------------------------------------------------------------
constructor TWSVGFill.IPropRule.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Rule := IE_FR_Default;
end;
//---------------------------------------------------------------------------
destructor TWSVGFill.IPropRule.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.IPropRule.Assign(const pOther: TWSVGItem);
var
    pSource: IPropRule;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropRule)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropRule;

    // copy data from source
    m_Rule := pSource.m_Rule;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.IPropRule.Clear;
begin
    inherited Clear;

    m_Rule := IE_FR_Default;
end;
//---------------------------------------------------------------------------
function TWSVGFill.IPropRule.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropRule.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGFill.IPropRule.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Value_Inherit) then
        m_Rule := IE_FR_Default
    else
    if (data = C_SVG_Value_NonZero) then
        m_Rule := IE_FR_NonZero
    else
    if (data = C_SVG_Value_EvenOdd) then
        m_Rule := IE_FR_EvenOdd
    else
    begin
        TWLogHelper.LogToCompiler('Parse fill rule - unknown rule - ' + data);
        m_Rule := IE_FR_Default;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.IPropRule.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Rule));
end;
//---------------------------------------------------------------------------
function TWSVGFill.IPropRule.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Rule) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGFill.IPropRule.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Rule) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGFill.IPropRule.ToStr(rule: IERule): UnicodeString;
begin
    case (rule) of
        IE_FR_Default: Exit(C_SVG_Value_Inherit);
        IE_FR_NonZero: Exit(C_SVG_Value_NonZero);
        IE_FR_EvenOdd: Exit(C_SVG_Value_EvenOdd);
    else
        raise Exception.CreateFmt('Unknown fill rule - %d', [Integer(rule)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGFill
//---------------------------------------------------------------------------
constructor TWSVGFill.Create(pOptions: PWSVGOptions);
begin
    inherited Create;

    m_pProperties := TWSVGElement.IProperties.Create;
    m_pOptions    := pOptions;
    m_NoFill      := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGFill.Destroy;
begin
    DelAndClear;

    m_pProperties.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.DelAndClear;
begin
    // clear all properties. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pProperties.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGFill.GetProperty(index: Integer): TWSVGProperty;
begin
    if (index >= m_pProperties.Count) then
        Exit(nil);

    Result := m_pProperties[index];
end;
//---------------------------------------------------------------------------
function TWSVGFill.GetPropertyCount: Integer;
begin
    Result := m_pProperties.Count;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.Assign(const pOther: TWSVGFill; pParent: TWSVGItem);
var
    pProperty, pNewProp: TWSVGProperty;
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // copy values
    m_NoFill := pOther.m_NoFill;

    // iterate through properties to copy
    for pProperty in pOther.m_pProperties do
    begin
        pNewProp := nil;

        try
            // copy property from source
            pNewProp := pProperty.CreateInstance(pParent);
            pNewProp.Assign(pProperty);
            m_pProperties.Add(pNewProp);
            pNewProp := nil;
        finally
            pNewProp.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.Clear;
begin
    m_NoFill := False;

    DelAndClear;
end;
//---------------------------------------------------------------------------
function TWSVGFill.Parse(const name, value: UnicodeString; pParent: TWSVGItem): Boolean;
var
    pLink:     TWSVGPropLink;
    pColor:    TWSVGPropColor;
    color:     TWColor;
    pOpacity:  TWSVGMeasure<Single>;
    pFillRule: TWSVGFill.IPropRule;
    data:      UnicodeString;
    pData:     PUnicodeString;
begin
    // if the value can be trusted, use it directly, otherwise clean it before
    if (m_pOptions.m_TrustSVGSyntax) then
        pData := @value
    else
    begin
        // prepare the value to be parsed
        data  := TWSVGCommon.PrepareStr(value);
        pData := @data;
    end;

    // search for property to parse
    if (name = C_SVG_Prop_Fill) then
    begin
        pLink  := nil;
        pColor := nil;

        try
            // create link property
            pLink := TWSVGPropLink.Create(pParent, m_pOptions);

            // first, read fill color as link
            if (pLink.Parse(pData^)) then
            begin
                pLink.ItemName := name;
                m_pProperties.Add(pLink);
                pLink := nil;
            end
            else
            begin
                // create color property and populate with fill color
                pColor          := TWSVGPropColor.Create(pParent, m_pOptions);
                pColor.ItemName := name;

                // no fill? (NOTE setting an empty color is not enough in this case, because the
                // opacity attribute is allowed to modify the fill color later. This may cause
                // situations where it's no more possible to determine if a fill should be processed
                // or simply skipped. As the both operations may cause different results, an
                // explicit distinction should be made. For that reason, if fill='none', it's better
                // to flag that fill is skipped and not register color at all)
                if (pData^ = C_SVG_Value_None) then
                    m_NoFill := True
                else
                begin
                    color.Assign(TWSVGPropColor.ParseColor(pData^));
                    pColor.Add(@color);
                    m_pProperties.Add(pColor);
                    pColor := nil;
                end;
            end;
        finally
            pLink.Free;
            pColor.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Fill_Opacity) then
    begin
        pOpacity := nil;

        try
            // create measure property and populate with fill opacity
            pOpacity          := TWSVGMeasure<Single>.Create(pParent, m_pOptions, True);
            pOpacity.ItemName := name;
            pOpacity.Parse(pData^);
            m_pProperties.Add(pOpacity);
            pOpacity := nil;
        finally
            pOpacity.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Fill_Rule) then
    begin
        pFillRule := nil;

        try
            // create and populate fill rule property
            pFillRule          := TWSVGFill.IPropRule.Create(pParent, m_pOptions);
            pFillRule.ItemName := name;
            pFillRule.Parse(pData^);
            m_pProperties.Add(pFillRule);
            pFillRule := nil;
        finally
            pFillRule.Free;
        end;

        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGFill.Log(margin: Cardinal);
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to log
    for pProperty in m_pProperties do
        pProperty.Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGFill.Print(margin: Cardinal): UnicodeString;
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to print
    for pProperty in m_pProperties do
        Result := Result + pProperty.Print(margin);
end;
//---------------------------------------------------------------------------
function TWSVGFill.ToXml(var count: NativeUInt): UnicodeString;
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to add
    for pProperty in m_pProperties do
    begin
        // is first property?
        if (count > 0) then
            // Add separator
            Result := Result + ';';

        // get property as string
        Result := Result + pProperty.ToXml;

        Inc(count);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStroke.IPropLineCap
//---------------------------------------------------------------------------
constructor TWSVGStroke.IPropLineCap.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_LC_Default;
end;
//---------------------------------------------------------------------------
destructor TWSVGStroke.IPropLineCap.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineCap.Assign(const pOther: TWSVGItem);
var
    pSource: IPropLineCap;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropLineCap)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropLineCap;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineCap.Clear;
begin
    inherited Clear;

    m_Type := IE_LC_Default;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineCap.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropLineCap.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineCap.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Value_Inherit) then
        m_Type := IE_LC_Default
    else
    if (data = C_SVG_Value_Butt) then
        m_Type := IE_LC_Butt
    else
    if (data = C_SVG_Value_Round) then
        m_Type := IE_LC_Round
    else
    if (data = C_SVG_Value_Square) then
        m_Type := IE_LC_Square
    else
    begin
        TWLogHelper.LogToCompiler('Parse dash linecap - unknown linecap - ' + data);
        m_Type := IE_LC_Default;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineCap.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineCap.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineCap.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGStroke.IPropLineCap.ToStr(lineCap: IELineCap): UnicodeString;
begin
    case (lineCap) of
        IE_LC_Default: Exit(C_SVG_Value_Inherit);
        IE_LC_Butt:    Exit(C_SVG_Value_Butt);
        IE_LC_Round:   Exit(C_SVG_Value_Round);
        IE_LC_Square:  Exit(C_SVG_Value_Square);
    else
        raise Exception.CreateFmt('Unknown linecap - %d', [Integer(lineCap)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStroke.IPropLineJoin
//---------------------------------------------------------------------------
constructor TWSVGStroke.IPropLineJoin.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_LJ_Default;
end;
//---------------------------------------------------------------------------
destructor TWSVGStroke.IPropLineJoin.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineJoin.Assign(const pOther: TWSVGItem);
var
    pSource: IPropLineJoin;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IPropLineJoin)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IPropLineJoin;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineJoin.Clear;
begin
    inherited Clear;

    m_Type := IE_LJ_Default;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineJoin.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropLineJoin.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineJoin.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Value_Inherit) then
        m_Type := IE_LJ_Default
    else
    if (data = C_SVG_Value_Miter) then
        m_Type := IE_LJ_Miter
    else
    if (data = C_SVG_Value_Round) then
        m_Type := IE_LJ_Round
    else
    if (data = C_SVG_Value_Bevel) then
        m_Type := IE_LJ_Bevel
    else
    begin
        TWLogHelper.LogToCompiler('Parse dash line join - unknown line join - ' + data);
        m_Type := IE_LJ_Default;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.IPropLineJoin.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Type));
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineJoin.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Type) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.IPropLineJoin.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Type) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGStroke.IPropLineJoin.ToStr(lineJoin: IELineJoin): UnicodeString;
begin
    case (lineJoin) of
        IE_LJ_Default: Exit(C_SVG_Value_Inherit);
        IE_LJ_Miter:   Exit(C_SVG_Value_Miter);
        IE_LJ_Round:   Exit(C_SVG_Value_Round);
        IE_LJ_Bevel:   Exit(C_SVG_Value_Bevel);
    else
        raise Exception.CreateFmt('Unknown line join - %d', [Integer(lineJoin)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStroke
//---------------------------------------------------------------------------
constructor TWSVGStroke.Create(pOptions: PWSVGOptions);
begin
    inherited Create;

    m_pProperties := TWSVGElement.IProperties.Create;
    m_pOptions    := pOptions;
    m_NoStroke    := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGStroke.Destroy;
begin
    DelAndClear;

    m_pProperties.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.DelAndClear;
begin
    // clear all properties. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pProperties.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.GetProperty(index: Integer): TWSVGProperty;
begin
    if (index >= m_pProperties.Count) then
        Exit(nil);

    Result := m_pProperties[index];
end;
//---------------------------------------------------------------------------
function TWSVGStroke.GetPropertyCount: Integer;
begin
    Result := m_pProperties.Count;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.Assign(const pOther: TWSVGStroke; pParent: TWSVGItem);
var
    pProperty, pNewProp: TWSVGProperty;
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // copy values
    m_NoStroke := pOther.m_NoStroke;

    // iterate through properties to copy
    for pProperty in pOther.m_pProperties do
    begin
        pNewProp := nil;

        try
            // copy property from source
            pNewProp := pProperty.CreateInstance(pParent);
            pNewProp.Assign(pProperty);
            m_pProperties.Add(pNewProp);
            pNewProp := nil;
        finally
            pNewProp.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.Clear;
begin
    m_NoStroke := False;

    DelAndClear;
end;
//---------------------------------------------------------------------------
function TWSVGStroke.Parse(const name, value: UnicodeString; pParent: TWSVGItem): Boolean;
var
    pLink:                                            TWSVGPropLink;
    pColor:                                           TWSVGPropColor;
    color:                                            TWColor;
    pOpacity, pWidth, pMiterLimit, pStrokeDashOffset: TWSVGMeasure<Single>;
    pStrokeDashArray:                                 TWSVGAttribute<Single>;
    pLineCap:                                         IPropLineCap;
    pLineJoin:                                        IPropLineJoin;
    data:                                             UnicodeString;
    pData:                                            PUnicodeString;
begin
    // if the value can be trusted, use it directly, otherwise clean it before
    if (m_pOptions.m_TrustSVGSyntax) then
        pData := @value
    else
    begin
        // prepare the value to be parsed
        data  := TWSVGCommon.PrepareStr(value);
        pData := @data;
    end;

    // search for property to parse
    if (name = C_SVG_Prop_Stroke) then
    begin
        pLink  := nil;
        pColor := nil;

        try
            // create link property
            pLink := TWSVGPropLink.Create(pParent, m_pOptions);

            // first, read fill color as link
            if (pLink.Parse(pData^)) then
            begin
                pLink.ItemName := name;
                m_pProperties.Add(pLink);
                pLink := nil;
            end
            else
            begin
                // create color property and populate with stroke color
                pColor          := TWSVGPropColor.Create(pParent, m_pOptions);
                pColor.ItemName := name;

                // no stroke? (NOTE setting an empty color is not enough in this case, because the
                // opacity attribute is allowed to modify the stroke color later. This may cause
                // situations where it's no more possible to determine if a stroke should be
                // processed or simply skipped. As the both operations may cause different results,
                // an explicit distinction should be made. For that reason, if stroke='none', it's
                // better to flag that stroke is skipped and not register color at all)
                if (pData^ = C_SVG_Value_None) then
                    m_NoStroke := True
                else
                begin
                    color.Assign(TWSVGPropColor.ParseColor(pData^));
                    pColor.Add(@color);
                    m_pProperties.Add(pColor);
                    pColor := nil;
                end;
            end;
        finally
            pLink.Free;
            pColor.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_Opacity) then
    begin
        pOpacity := nil;

        try
            // create measure property and populate with stroke opacity
            pOpacity          := TWSVGMeasure<Single>.Create(pParent, m_pOptions, True);
            pOpacity.ItemName := name;
            pOpacity.Parse(pData^);
            m_pProperties.Add(pOpacity);
            pOpacity := nil;
        finally
            pOpacity.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_Width) then
    begin
        pWidth := nil;

        try
            // create measure property and populate with stroke width (NOTE in Inkscape, stroke width
            // is always a float value exprimed in pixels)
            pWidth          := TWSVGMeasure<Single>.Create(pParent, m_pOptions, True);
            pWidth.ItemName := name;
            pWidth.Parse(pData^);
            m_pProperties.Add(pWidth);
            pWidth := nil;
        finally
            pWidth.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_LineCap) then
    begin
        pLineCap := nil;

        try
            // create and populate stroke linecap
            pLineCap          := IPropLineCap.Create(pParent, m_pOptions);
            pLineCap.ItemName := name;
            pLineCap.Parse(pData^);
            m_pProperties.Add(pLineCap);
            pLineCap := nil;
        finally
            pLineCap.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_LineJoin) then
    begin
        pLineJoin := nil;

        try
            // create and populate stroke line join
            pLineJoin          := IPropLineJoin.Create(pParent, m_pOptions);
            pLineJoin.ItemName := name;
            pLineJoin.Parse(pData^);
            m_pProperties.Add(pLineJoin);
            pLineJoin := nil;
        finally
            pLineJoin.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_MiterLimit) then
    begin
        pMiterLimit := nil;

        try
            // create and populate stroke miter limit
            pMiterLimit          := TWSVGMeasure<Single>.Create(pParent, m_pOptions, True);
            pMiterLimit.ItemName := name;
            pMiterLimit.Parse(pData^);
            m_pProperties.Add(pMiterLimit);
            pMiterLimit := nil;
        finally
            pMiterLimit.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_DashArray) then
    begin
        pStrokeDashArray := nil;

        try
            // create and populate stroke dash array
            pStrokeDashArray          := TWSVGAttribute<Single>.Create(pParent, m_pOptions);
            pStrokeDashArray.ItemName := name;
            pStrokeDashArray.Parse(pData^);
            m_pProperties.Add(pStrokeDashArray);
            pStrokeDashArray := nil;
        finally
            pStrokeDashArray.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Stroke_DashOffset) then
    begin
        pStrokeDashOffset := nil;

        try
            // create measure property and populate with stroke dash offset
            pStrokeDashOffset          := TWSVGMeasure<Single>.Create(pParent, m_pOptions, False);
            pStrokeDashOffset.ItemName := name;
            pStrokeDashOffset.Parse(pData^);
            m_pProperties.Add(pStrokeDashOffset);
            pStrokeDashOffset := nil;
        finally
            pStrokeDashOffset.Free;
        end;

        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGStroke.Log(margin: Cardinal);
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to log
    for pProperty in m_pProperties do
        pProperty.Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGStroke.Print(margin: Cardinal): UnicodeString;
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to print
    for pProperty in m_pProperties do
        Result := Result + pProperty.Print(margin);
end;
//---------------------------------------------------------------------------
function TWSVGStroke.ToXml(var count: NativeUInt): UnicodeString;
var
    pProperty: TWSVGProperty;
begin
    // iterate through properties to add
    for pProperty in m_pProperties do
    begin
        // is first property?
        if (count > 0) then
            // Add separator
            Result := Result + ';';

        // get property as string
        Result := Result + pProperty.ToXml;

        Inc(count);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStyle.IPropDisplay
//---------------------------------------------------------------------------
constructor TWSVGStyle.IPropDisplay.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Prop_Display;
end;
//---------------------------------------------------------------------------
destructor TWSVGStyle.IPropDisplay.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropDisplay.ValueToStr(value: Integer): UnicodeString;
begin
    Result := ToStr(IEValue(value));
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropDisplay.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropDisplay.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropDisplay.Parse(const data: UnicodeString): Boolean;
var
    pValues: TWSVGCommon.IValues;
    item:    UnicodeString;
begin
    pValues := nil;

    try
        pValues := TWSVGCommon.IValues.Create;

        // get all values to parse (in case of display list)
        TWSVGCommon.ExtractValues(data, 1, Length(data), pValues, not m_pOptions.m_TrustSVGSyntax);

        // iterate through values to parse
        for item in pValues do
            if (item = C_SVG_Value_Inline) then
                Add(Integer(IE_V_Inline))
            else
            if (item = C_SVG_Value_Block) then
                Add(Integer(IE_V_Block))
            else
            if (item = C_SVG_Value_List_Item) then
                Add(Integer(IE_V_ListItem))
            else
            if (item = C_SVG_Value_Run_In) then
                Add(Integer(IE_V_RunIn))
            else
            if (item = C_SVG_Value_Compact) then
                Add(Integer(IE_V_Compact))
            else
            if (item = C_SVG_Value_Marker) then
                Add(Integer(IE_V_Marker))
            else
            if (item = C_SVG_Value_Table) then
                Add(Integer(IE_V_Table))
            else
            if (item = C_SVG_Value_Inline_Table) then
                Add(Integer(IE_V_InlineTable))
            else
            if (item = C_SVG_Value_Table_Row_Group) then
                Add(Integer(IE_V_TableRowGroup))
            else
            if (item = C_SVG_Value_Table_Header_Group) then
                Add(Integer(IE_V_TableHeaderGroup))
            else
            if (item = C_SVG_Value_Table_Footer_Group) then
                Add(Integer(IE_V_TableFooterGroup))
            else
            if (item = C_SVG_Value_Table_Row) then
                Add(Integer(IE_V_TableRow))
            else
            if (item = C_SVG_Value_Table_Column_Group) then
                Add(Integer(IE_V_TableColumnGroup))
            else
            if (item = C_SVG_Value_Table_Column) then
                Add(Integer(IE_V_TableColumn))
            else
            if (item = C_SVG_Value_Table_Cell) then
                Add(Integer(IE_V_TableCell))
            else
            if (item = C_SVG_Value_Table_Caption) then
                Add(Integer(IE_V_TableCaption))
            else
            if (item = C_SVG_Value_None) then
                Add(Integer(IE_V_None))
            else
            if (item = C_SVG_Value_Inherit) then
                Add(Integer(IE_V_Inherit))
            else
            begin
                TWLogHelper.LogToCompiler('Parse display - unknown value - ' + item);
                Add(Integer(IE_V_Inline));
            end;
    finally
        pValues.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWSVGStyle.IPropDisplay.ToStr(value: IEValue): UnicodeString;
begin
    case (value) of
        IE_V_Inline:           Exit (C_SVG_Value_Inline);
        IE_V_Block:            Exit (C_SVG_Value_Block);
        IE_V_ListItem:         Exit (C_SVG_Value_List_Item);
        IE_V_RunIn:            Exit (C_SVG_Value_Run_In);
        IE_V_Compact:          Exit (C_SVG_Value_Compact);
        IE_V_Marker:           Exit (C_SVG_Value_Marker);
        IE_V_Table:            Exit (C_SVG_Value_Table);
        IE_V_InlineTable:      Exit (C_SVG_Value_Inline_Table);
        IE_V_TableRowGroup:    Exit (C_SVG_Value_Table_Row_Group);
        IE_V_TableHeaderGroup: Exit (C_SVG_Value_Table_Header_Group);
        IE_V_TableFooterGroup: Exit (C_SVG_Value_Table_Footer_Group);
        IE_V_TableRow:         Exit (C_SVG_Value_Table_Row);
        IE_V_TableColumnGroup: Exit (C_SVG_Value_Table_Column_Group);
        IE_V_TableColumn:      Exit (C_SVG_Value_Table_Column);
        IE_V_TableCell:        Exit (C_SVG_Value_Table_Cell);
        IE_V_TableCaption:     Exit (C_SVG_Value_Table_Caption);
        IE_V_None:             Exit (C_SVG_Value_None);
        IE_V_Inherit:          Exit (C_SVG_Value_Inherit);
    else
        raise Exception.CreateFmt('Unknown value - %d', [Integer(value)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStyle.IPropVisibility
//---------------------------------------------------------------------------
constructor TWSVGStyle.IPropVisibility.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Prop_Visibility;
end;
//---------------------------------------------------------------------------
destructor TWSVGStyle.IPropVisibility.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropVisibility.ValueToStr(value: Integer): UnicodeString;
begin
    Result := ToStr(IEValue(value));
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropVisibility.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IPropVisibility.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGStyle.IPropVisibility.Parse(const data: UnicodeString): Boolean;
var
    pValues: TWSVGCommon.IValues;
    item:    UnicodeString;
begin
    pValues := nil;

    try
        pValues := TWSVGCommon.IValues.Create;

        // get all values to parse (in case of visibility list)
        TWSVGCommon.ExtractValues(data, 1, Length(data), pValues, not m_pOptions.m_TrustSVGSyntax);

        // iterate through values to parse
        for item in pValues do
            if (item = C_SVG_Value_Visible) then
                Add(Integer(IE_V_Visible))
            else
            if (item = C_SVG_Value_Hidden) then
                Add(Integer(IE_V_Hidden))
            else
            if (item = C_SVG_Value_Collapse) then
                Add(Integer(IE_V_Collapse))
            else
            begin
                TWLogHelper.LogToCompiler('Parse visibility - unknown value - ' + item);
                Add(Integer(IE_V_Visible));
            end;
    finally
        pValues.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWSVGStyle.IPropVisibility.ToStr(value: IEValue): UnicodeString;
begin
    case (value) of
        IE_V_Visible:  Exit (C_SVG_Value_Visible);
        IE_V_Hidden:   Exit (C_SVG_Value_Hidden);
        IE_V_Collapse: Exit (C_SVG_Value_Collapse);
    else
        raise Exception.CreateFmt('Unknown value - %d', [Integer(value)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGStyle
//---------------------------------------------------------------------------
constructor TWSVGStyle.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_pProperties := TWSVGElement.IProperties.Create;
    m_pFill       := TWSVGFill.Create(pOptions);
    m_pStroke     := TWSVGStroke.Create(pOptions);

    // set style name by default. Be careful, some SVG files contain no style but distinct properties
    ItemName := C_SVG_Prop_Style;
end;
//---------------------------------------------------------------------------
destructor TWSVGStyle.Destroy;
begin
    DelAndClear;

    m_pProperties.Free;
    m_pFill.Free;
    m_pStroke.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.SetValue(const name, value: UnicodeString): Boolean;
begin
    // found last value?
    if (Length(value) = 0) then
        Exit(False);

    // parse property
    if ((name = C_SVG_Prop_Width) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Height) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Display) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Visibility) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Opacity) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Filter) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Gradient_Stop_Color) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Gradient_Stop_Opacity) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Enable_Background) and ParseStyle(name, value)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Fill) and m_pFill.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Fill_Opacity) and m_pFill.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Fill_Rule) and m_pFill.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_Opacity) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_Width) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_LineCap) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_LineJoin) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_MiterLimit) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_DashArray) and m_pStroke.Parse(name, value, Self)) then
        Exit(True)
    else
    if ((name = C_SVG_Prop_Stroke_DashOffset) and m_pStroke.Parse(name, value, Self)) then
        Exit(True);

    // log unknown value
    TWLogHelper.LogToCompiler('Style - found unknown property - name - ' + name + ' - value - ' + value);

    // always return true to skip the unknown value without put the parsing into failure
    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.ParseStyle(const name, value: UnicodeString): Boolean;
var
    pWidth, pHeight, pOpacity: TWSVGMeasure<Single>;
    pColor:                    TWSVGPropColor;
    pBg:                       TWSVGPropBackground;
    pDisplay:                  IPropDisplay;
    pVisibility:               IPropVisibility;
    pLink:                     TWSVGPropLink;
    color:                     TWColor;
    data:                      UnicodeString;
    pData:                     PUnicodeString;
begin
    // if the value can be trusted, use it directly, otherwise clean it before
    if (m_pOptions.m_TrustSVGSyntax) then
        pData := @value
    else
    begin
        // prepare the value to be parsed
        data  := TWSVGCommon.PrepareStr(value);
        pData := @data;
    end;

    // search for property to parse
    if (name = C_SVG_Prop_Width) then
    begin
        pWidth := nil;

        try
            // read width
            pWidth          := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
            pWidth.ItemName := name;
            pWidth.Parse(pData^);
            m_pProperties.Add(pWidth);
            pWidth := nil;
        finally
            pWidth.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Height) then
    begin
        pHeight := nil;

        try
            // read height
            pHeight          := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
            pHeight.ItemName := name;
            pHeight.Parse(pData^);
            m_pProperties.Add(pHeight);
            pHeight := nil;
        finally
            pHeight.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Display) then
    begin
        pDisplay := nil;

        try
            // read display property
            pDisplay          := IPropDisplay.Create(Self, m_pOptions);
            pDisplay.ItemName := name;
            pDisplay.Parse(pData^);
            m_pProperties.Add(pDisplay);
            pDisplay := nil;
        finally
            pDisplay.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Visibility) then
    begin
        pVisibility := nil;

        try
            // read visibility property
            pVisibility          := IPropVisibility.Create(Self, m_pOptions);
            pVisibility.ItemName := name;
            pVisibility.Parse(pData^);
            m_pProperties.Add(pVisibility);
            pVisibility := nil;
        finally
            pVisibility.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Opacity) then
    begin
        pOpacity := nil;

        try
            // create measure property and populate with opacity
            pOpacity          := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
            pOpacity.ItemName := name;
            pOpacity.Parse(pData^);
            m_pProperties.Add(pOpacity);
            pOpacity := nil;
        finally
            pOpacity.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Gradient_Stop_Color) then
    begin
        pColor := nil;

        try
            // create color property and populate with gradient stop color
            pColor          := TWSVGPropColor.Create(Self, m_pOptions);
            pColor.ItemName := name;

            color.Assign(TWSVGPropColor.ParseColor(pData^));
            pColor.Add(@color);
            m_pProperties.Add(pColor);
            pColor := nil;
        finally
            pColor.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Gradient_Stop_Opacity) then
    begin
        pOpacity := nil;

        try
            // create measure property and populate with fill opacity
            pOpacity          := TWSVGMeasure<Single>.Create(Self, m_pOptions, True);
            pOpacity.ItemName := name;
            pOpacity.Parse(pData^);
            m_pProperties.Add(pOpacity);
            pOpacity := nil;
        finally
            pOpacity.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Filter) then
    begin
        pLink := nil;

        try
            // create link property and populate with effect link
            pLink          := TWSVGPropLink.Create(Self, m_pOptions);
            pLink.ItemName := name;
            pLink.Parse(pData^);
            m_pProperties.Add(pLink);
            pLink := nil;
        finally
            pLink.Free;
        end;

        Exit(True);
    end
    else
    if (name = C_SVG_Prop_Enable_Background) then
    begin
        pBg := nil;

        try
            // create background property and populate with gradient stop color
            pBg          := TWSVGPropBackground.Create(Self, m_pOptions);
            pBg.ItemName := name;
            pBg.Parse(pData^);
            m_pProperties.Add(pBg);
            pBg := nil;
        finally
            pBg.Free;
        end;

        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGStyle.DelAndClear;
begin
    // clear all properties. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pProperties.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.GetProperty(index: Integer): TWSVGProperty;
begin
    if (index >= m_pProperties.Count) then
        Exit(nil);

    Result := m_pProperties[index];
end;
//---------------------------------------------------------------------------
function TWSVGStyle.GetPropertyCount: Integer;
begin
    Result := m_pProperties.Count;
end;
//---------------------------------------------------------------------------
procedure TWSVGStyle.Assign(const pOther: TWSVGItem);
var
    pSource:             TWSVGStyle;
    pProperty, pNewProp: TWSVGProperty;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGStyle)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGStyle;

    // copy fill and stroke
    m_pFill.Assign(pSource.m_pFill, Self);
    m_pStroke.Assign(pSource.m_pStroke, Self);

    // iterate through properties to copy
    for pProperty in pSource.m_pProperties do
    begin
        pNewProp := nil;

        try
            // copy property from source
            pNewProp := pProperty.CreateInstance(Self);
            pNewProp.Assign(pProperty);
            m_pProperties.Add(pNewProp);
            pNewProp := nil;
        finally
            pNewProp.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGStyle.Clear;
begin
    inherited Clear;

    DelAndClear;

    m_pFill.Clear;
    m_pStroke.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGStyle.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGStyle.Read(const name: UnicodeString; const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGStyle.Read(const name: UnicodeString; const pNode: IXMLNode): Boolean;
{$endif}
var
    display,
    visibility,
    opacity,
    gradientStopColor,
    gradientStopOpacity,
    filter,
    fillColor,
    fillOpacity,
    fillRule,
    strokeColor,
    strokeOpacity,
    strokeWidth,
    strokeLinecap,
    strokeLineJoin,
    strokeMiterLimit,
    strokeDashArray,
    strokeDashOffset: UnicodeString;
begin
    // read style as single property. NOTE styles can exist in 2 different ways: either in a style
    // property containing all the items, e.g. style="fill:#0d60ec;stroke:#000000", or in several
    // separated properties, e.g. fill="#FFF" stroke="#000". For that, the read function should
    // detect what kind of properties are read and use the correct way to do that)
    inherited Read(name, pNode);

    // get display mode
    display := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Display, C_SVG_Global_Error);
    if (display <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Prop_Display, display);

    // get visibility
    visibility := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Visibility, C_SVG_Global_Error);
    if (visibility <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Prop_Visibility, visibility);

    // get global opacity
    opacity := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Opacity, C_SVG_Global_Error);
    if (opacity <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Prop_Opacity, opacity);

    // get gradient stop color
    gradientStopColor := TWSVGCommon.GetAttribute(pNode, C_SVG_Gradient_Stop_Color, C_SVG_Global_Error);
    if (gradientStopColor <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Gradient_Stop_Color, gradientStopColor);

    // get gradient stop opacity
    gradientStopOpacity := TWSVGCommon.GetAttribute(pNode, C_SVG_Gradient_Stop_Opacity, C_SVG_Global_Error);
    if (gradientStopOpacity <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Gradient_Stop_Opacity, gradientStopOpacity);

    // get filter
    filter := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Filter, C_SVG_Global_Error);
    if (filter <> C_SVG_Global_Error) then
        ParseStyle(C_SVG_Prop_Filter, filter);

    // get fill color
    fillColor := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Fill, C_SVG_Global_Error);
    if (fillColor <> C_SVG_Global_Error) then
        m_pFill.Parse(C_SVG_Prop_Fill, fillColor, Self);

    // get fill opacity
    fillOpacity := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Fill_Opacity, C_SVG_Global_Error);
    if (fillOpacity <> C_SVG_Global_Error) then
        m_pFill.Parse(C_SVG_Prop_Fill_Opacity, fillOpacity, Self);

    // get fill rule
    fillRule := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Fill_Rule, C_SVG_Global_Error);
    if (fillRule <> C_SVG_Global_Error) then
        m_pFill.Parse(C_SVG_Prop_Fill_Rule, fillRule, Self);

    // get stroke color
    strokeColor := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke, C_SVG_Global_Error);
    if (strokeColor <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke, strokeColor, Self);

    // get stroke opacity
    strokeOpacity := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_Opacity, C_SVG_Global_Error);
    if (strokeOpacity <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_Opacity, strokeOpacity, Self);

    // get stroke width
    strokeWidth := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_Width, C_SVG_Global_Error);
    if (strokeWidth <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_Width, strokeWidth, Self);

    // get stroke linecap
    strokeLinecap := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_LineCap, C_SVG_Global_Error);
    if (strokeLinecap <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_LineCap, strokeLinecap, Self);

    // get stroke line join
    strokeLineJoin := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_LineJoin, C_SVG_Global_Error);
    if (strokeLineJoin <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_LineJoin, strokeLineJoin, Self);

    // get stroke miter limit
    strokeMiterLimit := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_MiterLimit, C_SVG_Global_Error);
    if (strokeMiterLimit <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_MiterLimit, strokeMiterLimit, Self);

    // get stroke dash array
    strokeDashArray := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_DashArray, C_SVG_Global_Error);
    if (strokeDashArray <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_DashArray, strokeDashArray, Self);

    // get stroke dash offset
    strokeDashOffset := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Stroke_DashOffset, C_SVG_Global_Error);
    if (strokeDashOffset <> C_SVG_Global_Error) then
        m_pStroke.Parse(C_SVG_Prop_Stroke_DashOffset, strokeDashOffset, Self);

    // the style is empty but it's not an error. The parent style will just be inherited while rasterization
    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.Parse(const data: UnicodeString): Boolean;
var
    name, value:  UnicodeString;
    c:            WideChar;
    lastValuePos: NativeInt;
    readValue:    Boolean;
begin
    readValue  := False;

    // iterate through data
    for c in data do
        // found name or value separator?
        if (c = ' ') then
        begin
            // blank spaces are important while a value is read
            if (readValue) then
                value := value + c;

            continue
        end
        else
        if (c = ':') then
            // found name separator, from now read value
            readValue := True
        else
        if (c = ';') then
        begin
            // set value
            if (not SetValue(name, value)) then
                Exit(False);

            // clear previous values
            name  := '';
            value := '';

            // next value to read is name
            readValue := False;
        end
        else
        if (readValue) then
            // update value
            value := value + c
        else
            // update name
            name := name + c;

    // check if last value is really a value to process, return without setting it if not, as this
    // value is always empty in this case
    if (TWSVGCommon.DoIgnoreLastValue(data, 1, Length(data), lastValuePos)) then
            Exit(True);

    // set last value
    Result := SetValue(name, value);
end;
//---------------------------------------------------------------------------
procedure TWSVGStyle.Log(margin: Cardinal);
var
    pProperty: TWSVGProperty;
begin
    TWLogHelper.LogToCompiler('Style - BEGIN');

    // iterate through properties to log
    for pProperty in m_pProperties do
        pProperty.Log(margin);

    m_pFill.Log(margin);
    m_pStroke.Log(margin);

    TWLogHelper.LogToCompiler('Style - END');
end;
//---------------------------------------------------------------------------
function TWSVGStyle.Print(margin: Cardinal): UnicodeString;
var
    pProperty: TWSVGProperty;
begin
    Result := '<Style>' + #13 + #10;

    // iterate through properties to print
    for pProperty in m_pProperties do
        Result := Result + pProperty.Print(margin);

    Result := Result + m_pFill.Print(margin);
    Result := Result + m_pStroke.Print(margin);

    Result := Result + '</Style>' + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGStyle.ToXml: UnicodeString;
var
    count:     NativeUInt;
    pProperty: TWSVGProperty;
begin
    // format string
    Result := ItemName + '=\"';

    count := 0;

    // iterate through properties to add
    for pProperty in m_pProperties do
    begin
        // is first property?
        if (count > 0) then
            // Add separator
            Result := Result + ';';

        // get property as string
        Result := Result + pProperty.ToXml;

        Inc(count);
    end;

    Result := Result + m_pFill.ToXml(count);
    Result := Result + m_pStroke.ToXml(count);

    // close string
    Result := Result + '\"';
end;
//---------------------------------------------------------------------------

end.
