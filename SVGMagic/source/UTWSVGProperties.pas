{**
 @abstract(@name provides the common properties that can be found in a SVG, like e.g. fill and
           strokes, matrices, ...)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGProperties;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     Vcl.Graphics,
     UTWMajorSettings,
     UTWColor,
     UTWStandardColor,
     UTWMatrix,
     UTWVersion,
     UTWDateTime,
     UTWHelpers,
     UTWGeometryTools,
     UTWSmartPointer,
     UTWSVGCommon,
     UTWSVGTags,
     UTWSVGItems;

type
    {**
     Enumerable property
    }
    TWSVGPropEnum = class(TWSVGProperty)
        public type
            IValues = array of Integer;

        protected
            m_Values: IValues;

            {**
             Get value at index
             @param(index Value index to get)
             @returns(Value at index)
             @raises(Exception if index is out of bounds)
            }
            function GetValue(index: Cardinal): Integer; virtual;

            {**
             Set value at index
             @param(index Value index)
             @param(value Value)
             @raises(Exception if index is out of bounds)
            }
            procedure SetValue(index: Cardinal; value: Integer); virtual;

            {**
             Get the value count
             @returns(Value count)
            }
            function GetValueCount: Cardinal; virtual;

            {**
             Convert value to string
             @param(value Value to convert)
             @returns(Value as string)
            }
            function ValueToStr(value: Integer): UnicodeString; virtual;

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
             Add a value
             @param(value Value to add)
            }
            procedure Add(value: Integer); virtual;

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
             Get or set the value at index. Example: value := Values[0];
             @br @bold(NOTE) An exception will be thrown if index is out of bounds
            }
            property Values[index: Cardinal]: Integer read GetValue write SetValue;

            {**
             Get the value count
            }
            property Count: Cardinal read GetValueCount;
    end;

    {**
     Aspect ratio property, used to align a graphic component inside its view box
    }
    TWSVGPropAspectRatio = class(TWSVGProperty)
        public type
            {**
             Image aspect ratio
             @value(IE_AR_None Do not force uniform scaling. Scale the graphic content of the given
                               element non-uniformly if necessary such that the element's bounding
                               box exactly matches the viewport rectangle. Note that if <align> is
                               none, then the optional <meetOrSlice> value is ignored)
             @value(IE_AR_XMinYMin Force uniform scaling. Align the <min-x> of the element's viewBox
                                   with the smallest X value of the viewport. Align the <min-y> of
                                   the element's viewBox with the smallest Y value of the viewport)
             @value(IE_AR_XMidYMin Force uniform scaling. Align the midpoint X value of the element's
                                   viewBox with the midpoint X value of the viewport. Align the
                                   <min-y> of the element's viewBox with the smallest Y value of the
                                   viewport)
             @value(IE_AR_XMaxYMin Force uniform scaling. Align the <min-x>+<width> of the element's
                                   viewBox with the maximum X value of the viewport. Align the <min-y>
                                   of the element's viewBox with the smallest Y value of the viewport)
             @value(IE_AR_XMinYMid Force uniform scaling. Align the <min-x> of the element's viewBox
                                   with the smallest X value of the viewport. Align the midpoint Y
                                   value of the element's viewBox with the midpoint Y value of the
                                   viewport)
             @value(IE_AR_XMidYMid Force uniform scaling. Align the midpoint X value of the element's
                                   viewBox with the midpoint X value of the viewport. Align the midpoint
                                   Y value of the element's viewBox with the midpoint Y value of the
                                   viewport)
             @value(IE_AR_XMaxYMid Force uniform scaling. Align the <min-x>+<width> of the element's
                                   viewBox with the maximum X value of the viewport. Align the midpoint
                                   Y value of the element's viewBox with the midpoint Y value of the
                                   viewport)
             @value(IE_AR_XMinYMax Force uniform scaling. Align the <min-x> of the element's viewBox
                                   with the smallest X value of the viewport. Align the <min-y>+<height>
                                   of the element's viewBox with the maximum Y value of the viewport)
             @value(IE_AR_XMidYMax Force uniform scaling. Align the midpoint X value of the element's
                                   viewBox with the midpoint X value of the viewport. Align the
                                   <min-y>+<height> of the element's viewBox with the maximum Y value
                                   of the viewport)
             @value(IE_AR_XMaxYMax Force uniform scaling. Align the <min-x>+<width> of the element's
                                   viewBox with the maximum X value of the viewport. Align the
                                   <min-y>+<height> of the element's viewBox with the maximum Y value
                                   of the viewport)
            }
            IEAspectRatio =
            (
                IE_AR_None,
                IE_AR_XMinYMin,
                IE_AR_XMidYMin,
                IE_AR_XMaxYMin,
                IE_AR_XMinYMid,
                IE_AR_XMidYMid,
                IE_AR_XMaxYMid,
                IE_AR_XMinYMax,
                IE_AR_XMidYMax,
                IE_AR_XMaxYMax
            );

            {**
             Image aspect ratio reference
             @value(IE_R_Meet Scale the graphic such that:
                              - aspect ratio is preserved
                              - the entire viewBox is visible within the viewport
                              - the viewBox is scaled up as much as possible, while still meeting
                                the other criteria
                              In this case, if the aspect ratio of the graphic does not match the
                              viewport, some of the viewport will extend beyond the bounds of the
                              viewBox (i.e., the area into which the viewBox will draw will be
                              smaller than the viewport))
             @value(IE_R_Slice Scale the graphic such that:
                               - aspect ratio is preserved
                               - the entire viewport is covered by the viewBox
                               - the viewBox is scaled down as much as possible, while still meeting
                                 the other criteria)
            }
            IEReference =
            (
                IE_R_Meet,
                IE_R_Slice
            );

        private
            m_AspectRatio: IEAspectRatio;
            m_Reference:   IEReference;

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
             Convert aspect ratio to string
             @param(aspectRatio Aspect ratio to convert)
             @returns(Aspect ratio as string)
            }
            class function ToStr(aspectRatio: IEAspectRatio): UnicodeString; overload; static;

            {**
             Convert aspect ratio reference to string
             @param(aspectRatio Aspect ratio reference to convert)
             @returns(Aspect ratio reference as string)
            }
            class function ToStr(reference: IEReference): UnicodeString; overload; static;

        public
            {**
             Get or set the image aspect ratio
            }
            property AspectRatio: IEAspectRatio read m_AspectRatio write m_AspectRatio;

            {**
             Get or set the image aspect ratio reference
            }
            property Reference: IEReference read m_Reference write m_Reference;
    end;

    {**
     Background property, used to determine how SVG background interacts with ancestor image. This
     property is rarely used (e.g. only recognized by IE10), and is useless except if filters are used
     too
    }
    // todo FIXME -cFeature -oJean: Move to style
    // todo FIXME -cFeature -oJean: and read better the https://www.w3.org/TR/SVG11/filters.html#AccessingBackgroundImage
    TWSVGPropBackground = class(TWSVGProperty)
        public type
            {**
             Background mode enumeration
             @value(IE_BM_Unknown Unknown background mode)
             @value(IE_BM_New Create a new background image canvas. All children of the current
                              container element can access the background, and they will be rendered
                              onto both the parent's background image canvas in addition to the target
                              device)
             @value(IE_BM_Accumulate If the ancestor container element has a property of new, then
                                     all graphics elements within the current container are rendered
                                     both on the parent's background image and onto the target)
            }
            IEBgMode =
            (
                IE_BM_Unknown,
                IE_BM_New,
                IE_BM_Accumulate
            );

        private
            m_BgMode: IEBgMode;
            m_X:      Single;
            m_Y:      Single;
            m_Width:  Single;
            m_Height: Single;

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
             Convert string to background mode
             @param(bgMode String containing the background mode to convert)
             @returns(Converted background mode, IE_BM_Unknown if unknown)
            }
            class function StrToBgMode(const bgMode: UnicodeString): IEBgMode; static;

            {**
             Convert background mode to string
             @param(bgMode Background mode)
             @param(defValue Default value to return if background mode is unknown)
             @returns(Converted string, default value if unknown)
            }
            class function BgModeToStr(bgMode: IEBgMode; const defValue: UnicodeString): UnicodeString; static;

        public
            {**
             Get or set the background mode
            }
            property Mode: IEBgMode read m_BgMode write m_BgMode;

            {**
             Get or set the background x value
            }
            property X: Single read m_X write m_X;

            {**
             Get or set the background y value
            }
            property Y: Single read m_Y write m_Y;

            {**
             Get or set the background width value
            }
            property Width: Single read m_Width write m_Width;

            {**
             Get or set the background bottom value
            }
            property Height: Single read m_Height write m_Height;
    end;

    {**
     Scalable Vector Graphics (SVG) color property
    }
    TWSVGPropColor = class(TWSVGProperty)
        public type
            IColors = array of TWColor;

        private
            m_Values: IColors;

        protected
            {**
             Get value at index
             @param(index Value index to get)
             @returns(Value at index)
             @raises(Exception if index is out of bounds)
            }
            function GetValue(index: Cardinal): PWColor; virtual;

            {**
             Set value at index
             @param(index Value index)
             @param(pValue Value)
             @raises(Exception if index is out of bounds)
            }
            procedure SetValue(index: Cardinal; pValue: PWColor); virtual;

            {**
             Get the value count
             @returns(Value count)
            }
            function GetValueCount: Cardinal; virtual;

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
             Add a color
             @param(pColor Color to add)
            }
            procedure Add(pColor: PWColor); virtual;

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
             Parse color
             @param(value String containing the color value to parse)
             @returns(The parsed color)
            }
            class function ParseColor(const value: UnicodeString): TWColor; static;

            {**
             Uncompress color
             @param(color Color to uncompress)
             @returns(Uncompressed color)
             @br @bold(NOTE) Color will not be modified if not compressed
            }
            class function UncompressColor(const color: UnicodeString): UnicodeString; static;

        public
            {**
             Get or set the value at index. Example: pColor := Values[0];
             @br @bold(NOTE) An exception will be raised if index is out of bounds
            }
            property Values[index: Cardinal]: PWColor read GetValue write SetValue;

            {**
             Get the value count
            }
            property Count: Cardinal read GetValueCount;
    end;

    {**
     Scalable Vector Graphics (SVG) link property
    }
    TWSVGPropLink = class(TWSVGProperty)
        public type
            {**
             Data type
             @value(IE_DT_URL The value contains a classic URL or link)
             @value(IE_DT_PNG The value contains a PNG image data)
             @value(IE_DT_JPG The value contains a JPG image data)
             @value(IE_DT_SVG The value contains a SVG image data)
             @value(IE_DT_Unknown The value is unknown)
            }
            IEDataType =
            (
                IE_DT_URL,
                IE_DT_PNG,
                IE_DT_JPG,
                IE_DT_SVG,
                IE_DT_Unknown
            );

            {**
             Encoding type
             @value(IE_E_None The value isn't encoded)
             @value(IE_E_Base64 The value is base64 encoded)
             @value(IE_E_Unknown The value encoding is unknown)
            }
            IEEncoding =
            (
                IE_E_None,
                IE_E_Base64,
                IE_E_Unknown
            );

        private
            m_Value:    UnicodeString;
            m_DataType: IEDataType;
            m_Encoding: IEEncoding;
            m_Local:    Boolean;

            {**
             Parse the link
             @param(data Data containing the link to parse)
             @returns(@true on success, otherwise @false)
            }
            function ParseLink(const data: UnicodeString): Boolean;

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

        public
            {**
             Get or set the link value
            }
            property Value: UnicodeString read m_Value write m_Value;

            {**
             Get or set the data type value
            }
            property DataType: IEDataType read m_DataType write m_DataType;

            {**
             Get or set the encoding value
            }
            property Encoding: IEEncoding read m_Encoding write m_Encoding;

            {**
             Get or set if the link is local
            }
            property IsLocal: Boolean read m_Local write m_Local;
    end;

    {**
     Scalable Vector Graphics (SVG) matrix property
     @br @bold(NOTE) The matrices are written in SVG in the form:
                     matrixType(a, b, c, d, e, f)

                     Here are how these values are distributed in the matrix:
                     [ a c e ]
                     [ b d f ]
                     [ 0 0 1 ]

                     The matrices may contain less values than above. In this case the repartition
                     depends on the matrix type. For example, a translation matrix may be written:
                     translate(x, y)

                     In this case the values are distributed as follow:
                     [ 1 0 x ]
                     [ 0 1 y ]
                     [ 0 0 1 ]

                     For further info, see: https://www.w3.org/TR/SVG/coords.html#TransformMatrixDefined
    }
    TWSVGPropMatrix = class(TWSVGProperty)
        public type
            {**
             Matrix type enumeration
             @value(IE_Unknown Unknown matrix type)
             @value(IE_Translate Matrix is a translation matrix)
             @value(IE_Scale Matrix is a scale matrix)
             @value(IE_Rotate Matrix is a rotation matrix)
             @value(IE_SkewX Matrix is a skew on x axis matrix)
             @value(IE_SkewY Matrix is a skew on y axis matrix)
             @value(IE_Custom Matrix is a custom matrix)
            }
            IEType =
            (
                IE_Unknown,
                IE_Translate,
                IE_Scale,
                IE_Rotate,
                IE_SkewX,
                IE_SkewY,
                IE_Custom
            );

        private
            m_Type:   IEType;
            m_Matrix: TWMatrix3x3;

            {**
             Set value
             @param(name Value name)
             @param(value Value to set)
             @returns(@true on success, otherwise @false)
            }
            function SetValue(const name, value: UnicodeString): Boolean;

            {**
             Read matrix values string as values
             @param(str String containing values)
             @param(values @bold([out]) values Read values)
             @returns(@true on success, otherwise @false)
            }
            function ReadValues<T>(const str: UnicodeString; var values: TWSVGArray<T>): Boolean;

        protected
            {**
             Get the matrix
             @returns(Matrix)
            }
            function GetMatrix: PWMatrix3x3; virtual;

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
             Set the matrix
             @param(pMatrix Matrix to set)
            }
            procedure SetMatrix(pMatrix: PWMatrix3x3); virtual;

            {**
             Parse data
             @param(data Data to parse)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const data: UnicodeString): Boolean; override;

            {**
             Parse data
             @param(data Data to parse)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) In this overload, the code clarity is favored on the execution speed.
                             The reason why this function was kept is that it can be used as strongly
                             tested alternative in case an unknown bug happen in the function used in
                             production
            }
            function Parse_Unoptimized(const data: UnicodeString): Boolean; virtual;

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
             Get the matrix
            }
            property Matrix: PWMatrix3x3 read GetMatrix;

            {**
             Get or set the matrix type
            }
            property MatrixType: IEType read m_Type write m_Type;
    end;

    {**
     Scalable Vector Graphics (SVG) rectangle property
    }
    TWSVGPropRect = class(TWSVGProperty)
        private
            m_X:      Single;
            m_Y:      Single;
            m_Width:  Single;
            m_Height: Single;

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

        public
            {**
             Get or set the viewbox x position
            }
            property X: Single read m_X write m_X;

            {**
             Get or set the viewbox y position
            }
            property Y: Single read m_Y write m_Y;

            {**
             Get or set the viewbox width
            }
            property Width: Single read m_Width write m_Width;

            {**
             Get or set the viewbox bottom value
            }
            property Height: Single read m_Height write m_Height;
    end;

    {**
     Scalable Vector Graphics (SVG) text property, can be e.g. an identifier, a url, ...
    }
    TWSVGPropText = class(TWSVGProperty)
        private
            m_Value: UnicodeString;

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

        public
            {**
             Get or set the text value
            }
            property Value: UnicodeString read m_Value write m_Value;
    end;

    {**
     Scalable Vector Graphics (SVG) date and time value
    }
    TWSVGPropTime = class(TWSVGProperty)
        private
            m_Value:      TWSimpleTime;
            m_Indefinite: Boolean;
            m_Negative:   Boolean;

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

        public
            {**
             Get or set the time value
            }
            property Value: TWSimpleTime read m_Value write m_Value;

            {**
             Get or set if the time is indefinite
            }
            property Indefinite: Boolean read m_Indefinite write m_Indefinite;

            {**
             Get or set if the time is negative
            }
            property Negative: Boolean read m_Negative write m_Negative;
    end;

    {**
     Scalable Vector Graphics (SVG) unit property
    }
    TWSVGPropUnit = class(TWSVGProperty)
        public type
            {**
             Unit type enumeration
             @value(IE_UT_ObjectBoundingBox All coordinates for the geometry properties refer to the
                                            user coordinate system as defined when the item was applied)
             @value(IE_UT_UserSpaceOnUse All coordinates for the geometry properties represent fractions
                                             or percentages of the item bounding box)
            }
            IEType =
            (
                IE_UT_ObjectBoundingBox,
                IE_UT_UserSpaceOnUse
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
             Convert string to unit type
             @param(unitType String containing the unit type to convert)
             @returns(Converted unit type, IE_UT_ObjectBoundingBox if unknown)
            }
            class function StrToUnitType(const unitType: UnicodeString): IEType; static;

            {**
             Convert unit type to string
             @param(unitType Unit type)
             @param(defValue Default value to return if unit type is unknown)
             @returns(Converted string, default value if unknown)
            }
            class function UnitTypeToStr(unitType: IEType; const defValue: UnicodeString): UnicodeString; static;

        public
            {**
             Get or set the unit type
            }
            property UnitType: IEType read m_Type write m_Type;
    end;

    {**
     Scalable Vector Graphics (SVG) version property
    }
    TWSVGPropVersion = class(TWSVGProperty)
        private
            m_Version: TWVersion;

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

        public
            {**
             Get or set the version value
            }
            property Value: TWVersion read m_Version write m_Version;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGPropEnum
//---------------------------------------------------------------------------
constructor TWSVGPropEnum.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGPropEnum.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGPropEnum.ValueToStr(value: Integer): UnicodeString;
begin
    Result := IntToStr(value);
end;
//---------------------------------------------------------------------------
function TWSVGPropEnum.GetValue(index: Cardinal): Integer;
begin
    if (Integer(index) >= Length(m_Values)) then
        raise Exception.Create('Index is out of bounds');

    Result := m_Values[index];
end;
//---------------------------------------------------------------------------
procedure TWSVGPropEnum.SetValue(index: Cardinal; value: Integer);
begin
    if (Integer(index) >= Length(m_Values)) then
        raise Exception.Create('Index is out of bounds');

    m_Values[index] := value;
end;
//---------------------------------------------------------------------------
function TWSVGPropEnum.GetValueCount: Cardinal;
begin
    Result := Length(m_Values);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropEnum.Assign(const pOther: TWSVGItem);
var
    pSource:  TWSVGPropEnum;
    count, i: Integer;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropEnum)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropEnum;

    // get the value count from source and prepare array to copy values
    count := pSource.Count;
    SetLength(m_Values, count);

    // copy values from source
    for i := 0 to count - 1 do
        m_Values[i] := pSource.m_Values[i];
end;
//---------------------------------------------------------------------------
procedure TWSVGPropEnum.Clear;
begin
    inherited Clear;

    SetLength(m_Values, 0);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropEnum.Add(value: Integer);
begin
    SetLength(m_Values, Length(m_Values) + 1);
    m_Values[Length(m_Values) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropEnum.Log(margin: Cardinal);
var
    str:      UnicodeString;
    count, i: Integer;
begin
    count := Length(m_Values);

    // no value to log?
    if (count = 0) then
        Exit;

    // iterate through values
    for i := 0 to count - 1 do
    begin
        // first value?
        if (i = 0) then
            str := str + ', ';

        str := str + ValueToStr(m_Values[i]);
    end;

    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + str);
end;
//---------------------------------------------------------------------------
function TWSVGPropEnum.Print(margin: Cardinal): UnicodeString;
var
    count, i: Integer;
begin
    count := Length(m_Values);

    // no value to print?
    if (count = 0) then
        Exit ('');

    // format property name
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ';

    // iterate through values
    for i := 0 to count - 1 do
    begin
        // first value?
        if (i = 0) then
            Result := Result + ', ';

        Result := Result + ValueToStr(m_Values[i]);
    end;

    // close the formatted string
    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropEnum.ToXml: UnicodeString;
var
    count, i: Integer;
begin
    count := Length(m_Values);

    // no value to export?
    if (count = 0) then
        Exit ('');

    // format property name
    Result := ItemName + '=\"';

    // iterate through values
    for i := 0 to count - 1 do
    begin
        // first value?
        if (i = 0) then
            Result := Result + ';';

        Result := Result + ValueToStr(m_Values[i]);
    end;

    // close the formatted string
    Result := Result + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGPropAspectRatio
//---------------------------------------------------------------------------
constructor TWSVGPropAspectRatio.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName      := C_SVG_Prop_PreserveAspectRatio;
    m_AspectRatio := IE_AR_XMidYMid;
    m_Reference   := IE_R_Meet;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropAspectRatio.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropAspectRatio.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropAspectRatio;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropAspectRatio)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropAspectRatio;

    // copy data from source
    m_AspectRatio := pSource.m_AspectRatio;
    m_Reference   := pSource.m_Reference;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropAspectRatio.Clear;
begin
    inherited Clear;

    m_AspectRatio := IE_AR_XMidYMid;
    m_Reference   := IE_R_Meet;
end;
//---------------------------------------------------------------------------
function TWSVGPropAspectRatio.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropAspectRatio.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropAspectRatio.Parse(const data: UnicodeString): Boolean;
var
    pValues: TWSVGCommon.IValues;
    index:   NativeUInt;
    item:    UnicodeString;
    doBreak: Boolean;
begin
    pValues := nil;

    try
        pValues := TWSVGCommon.IValues.Create;

        // get all values to parse (in case a reference is also defined)
        TWStringHelper.Split(' ', data, pValues);

        index   := 0;
        doBreak := False;

        // iterate through values to parse
        for item in pValues do
        begin
            case (index) of
                0:
                begin
                    if (item = C_SVG_Value_None) then
                        m_AspectRatio := IE_AR_None
                    else
                    if (item = C_SVG_Value_XMinYMin) then
                        m_AspectRatio := IE_AR_XMinYMin
                    else
                    if (item = C_SVG_Value_XMidYMin) then
                        m_AspectRatio := IE_AR_XMidYMin
                    else
                    if (item = C_SVG_Value_XMaxYMin) then
                        m_AspectRatio := IE_AR_XMaxYMin
                    else
                    if (item = C_SVG_Value_XMinYMid) then
                        m_AspectRatio := IE_AR_XMinYMid
                    else
                    if (item = C_SVG_Value_XMidYMid) then
                        m_AspectRatio := IE_AR_XMidYMid
                    else
                    if (item = C_SVG_Value_XMaxYMid) then
                        m_AspectRatio := IE_AR_XMaxYMid
                    else
                    if (item = C_SVG_Value_XMinYMax) then
                        m_AspectRatio := IE_AR_XMinYMax
                    else
                    if (item = C_SVG_Value_XMidYMax) then
                        m_AspectRatio := IE_AR_XMidYMax
                    else
                    if (item = C_SVG_Value_XMaxYMax) then
                        m_AspectRatio := IE_AR_XMaxYMax
                    else
                    begin
                        TWLogHelper.LogToCompiler('Aspect ratio - unknown aspect ratio - ' + data);
                        m_AspectRatio := IE_AR_XMidYMid;
                    end;
                end;

                1:
                begin
                    if (item = C_SVG_Value_Meet) then
                        m_Reference := IE_R_Meet
                    else
                    if (item = C_SVG_Value_Slice) then
                        m_Reference := IE_R_Slice
                    else
                    begin
                        TWLogHelper.LogToCompiler('Aspect ratio - unknown reference - ' + data);
                        m_Reference := IE_R_Meet;
                    end;
                end;
            else
                TWLogHelper.LogToCompiler('Aspect ratio - malformed value - ' + data);
                doBreak := True;
            end;

            if (doBreak) then
                break;

            Inc(index);
        end;
    finally
        pValues.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropAspectRatio.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + ToStr(m_AspectRatio) + ' - ' + ToStr(m_Reference));
end;
//---------------------------------------------------------------------------
function TWSVGPropAspectRatio.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_AspectRatio)
            + ' - ' + ToStr(m_Reference) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropAspectRatio.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_AspectRatio) + ' ' + ToStr(m_Reference) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGPropAspectRatio.ToStr(aspectRatio: IEAspectRatio): UnicodeString;
begin
    case (aspectRatio) of
        IE_AR_None:     Exit(C_SVG_Value_None);
        IE_AR_XMinYMin: Exit(C_SVG_Value_XMinYMin);
        IE_AR_XMidYMin: Exit(C_SVG_Value_XMidYMin);
        IE_AR_XMaxYMin: Exit(C_SVG_Value_XMaxYMin);
        IE_AR_XMinYMid: Exit(C_SVG_Value_XMinYMid);
        IE_AR_XMidYMid: Exit(C_SVG_Value_XMidYMid);
        IE_AR_XMaxYMid: Exit(C_SVG_Value_XMaxYMid);
        IE_AR_XMinYMax: Exit(C_SVG_Value_XMinYMax);
        IE_AR_XMidYMax: Exit(C_SVG_Value_XMidYMax);
        IE_AR_XMaxYMax: Exit(C_SVG_Value_XMaxYMax);
    else
        raise Exception.CreateFmt('Unknown aspect ratio value - %d', [Integer(aspectRatio)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGPropAspectRatio.ToStr(reference: IEReference): UnicodeString;
begin
    case (reference) of
        IE_R_Meet:  Exit(C_SVG_Value_Meet);
        IE_R_Slice: Exit(C_SVG_Value_Slice);
    else
        raise Exception.CreateFmt('Unknown aspect ratio reference value - %d', [Integer(reference)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGPropBackground
//---------------------------------------------------------------------------
constructor TWSVGPropBackground.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_BgMode := IE_BM_Unknown;
    m_X      := 0.0;
    m_Y      := 0.0;
    m_Width  := 0.0;
    m_Height := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropBackground.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropBackground.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropBackground;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropBackground)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropBackground;

    // copy data from source
    m_BgMode := pSource.m_BgMode;
    m_X      := pSource.m_X;
    m_Y      := pSource.m_Y;
    m_Width  := pSource.m_Width;
    m_Height := pSource.m_Height;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropBackground.Clear;
begin
    inherited Clear;

    m_BgMode := IE_BM_Unknown;
    m_X      := 0.0;
    m_Y      := 0.0;
    m_Width  := 0.0;
    m_Height := 0.0;
end;
//---------------------------------------------------------------------------
function TWSVGPropBackground.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropBackground.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropBackground.Parse(const data: UnicodeString): Boolean;
var
    pos, nextPos, i, dataLength: NativeUInt;
    rectData:                    UnicodeString;
    pStrings:                    IWSmartPointer<TStringList>;
begin
    pos        := 1;
    dataLength := Length(data);

    // iterate through data to read
    for i := 1 to dataLength do
        // is separator?
        if (data[i] = ' ') then
        begin
            pos := i;
            break;
        end;

    // calculate next position
    nextPos := pos + 1;

    // is position valid?
    if ((pos = 1) or (nextPos >= dataLength)) then
        Exit(False);

    // set background mode
    m_BgMode := StrToBgMode(Copy(data, 1, pos));

    // invalid background mode?
    if (m_BgMode = IE_BM_Unknown) then
        Exit(False);

    // get rect data (the part that contains x1, y1, x2 and y2)
    rectData := Copy(data, nextPos, dataLength - nextPos);

    pStrings := TWSmartPointer<TStringList>.Create();
    ExtractStrings([' '], [], PWideChar(rectData), pStrings);

    // full string must be read
    if (pStrings.Count <> 4) then
        Exit(False);

    m_X      := StrToFloat(pStrings[0], g_InternationalFormatSettings);
    m_Y      := StrToFloat(pStrings[1], g_InternationalFormatSettings);
    m_Width  := StrToFloat(pStrings[2], g_InternationalFormatSettings);
    m_Height := StrToFloat(pStrings[3], g_InternationalFormatSettings);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropBackground.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ('
            + FloatToStr(m_X) + ', ' + FloatToStr(m_Y) + ', ' + FloatToStr(m_Width) + ', '
            + FloatToStr(m_Height) + ') - mode - ' + BgModeToStr(m_BgMode, ''));
end;
//---------------------------------------------------------------------------
function TWSVGPropBackground.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - (' + FloatToStr(m_X) + ', '
            + FloatToStr(m_Y) + ', ' + FloatToStr(m_Width) + ', ' + FloatToStr(m_Height)
            + ') - mode - ' + BgModeToStr(m_BgMode, '') + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropBackground.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"' + BgModeToStr(m_BgMode, '') + ' ' + FloatToStr(m_X) + ' '
            + FloatToStr(m_Y) + ' ' + FloatToStr(m_Width) + ' ' + FloatToStr(m_Height) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGPropBackground.StrToBgMode(const bgMode: UnicodeString): IEBgMode;
begin
    // search for matching mode
    if (bgMode = C_SVG_Value_New) then
        Exit(IE_BM_New)
    else
    if (bgMode = C_SVG_Value_Accumulate) then
        Exit(IE_BM_Accumulate)
    else
        Exit(IE_BM_Unknown);
end;
//---------------------------------------------------------------------------
class function TWSVGPropBackground.BgModeToStr(bgMode: IEBgMode; const defValue: UnicodeString): UnicodeString;
begin
    // search for background mode
    case (bgMode) of
        IE_BM_New:        Exit(C_SVG_Value_New);
        IE_BM_Accumulate: Exit(C_SVG_Value_Accumulate);
        IE_BM_Unknown:
    else
        Exit(defValue);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGPropColor
//---------------------------------------------------------------------------
constructor TWSVGPropColor.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGPropColor.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.GetValue(index: Cardinal): PWColor;
begin
    if (Integer(index) >= Length(m_Values)) then
        raise Exception.Create('Index is out of bounds');

    Result := @m_Values[index];
end;
//---------------------------------------------------------------------------
procedure TWSVGPropColor.SetValue(index: Cardinal; pValue: PWColor);
begin
    if (Integer(index) >= Length(m_Values)) then
        raise Exception.Create('Index is out of bounds');

    m_Values[index].Assign(pValue^);
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.GetValueCount: Cardinal;
begin
    Result := Length(m_Values);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropColor.Assign(const pOther: TWSVGItem);
var
    pSource:       TWSVGPropColor;
    colorCount, i: Integer;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropColor)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropColor;

    // get the value count from source and prepare array to copy values
    colorCount := pSource.Count;
    SetLength(m_Values, colorCount);

    // copy colors from source
    for i := 0 to colorCount - 1 do
        m_Values[i].Assign(pSource.Values[i]^);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropColor.Clear;
begin
    inherited Clear;

    SetLength(m_Values, 0);
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropColor.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropColor.Add(pColor: PWColor);
begin
    SetLength(m_Values, Length(m_Values) + 1);
    m_Values[Length(m_Values) - 1].Assign(pColor^);
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.Parse(const data: UnicodeString): Boolean;
var
    pValues: TWSVGCommon.IValues;
    item:    UnicodeString;
    color:   TWColor;
begin
    pValues := nil;

    try
        pValues := TWSVGCommon.IValues.Create;

        // get all values to parse (in case of color list)
        TWSVGCommon.ExtractValues(data, 1, Length(data), pValues, not m_pOptions.m_TrustSVGSyntax);

        // iterate through values to parse
        for item in pValues do
        begin
            color.Assign(ParseColor(item));
            Add(@color);
        end;
    finally
        pValues.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropColor.Log(margin: Cardinal);
var
    str:           UnicodeString;
    colorCount, i: Integer;
begin
    colorCount := Length(m_Values);

    // no value to log?
    if (colorCount = 0) then
        Exit;

    // iterate through color values
    for i := 0 to colorCount - 1 do
    begin
        // first value?
        if (i = 0) then
            str := str + ', ';

        str := str + m_Values[i].ToHex(False);
    end;

    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + str);
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.Print(margin: Cardinal): UnicodeString;
var
    colorCount, i: Integer;
begin
    colorCount := Length(m_Values);

    // no value to print?
    if (colorCount = 0) then
        Exit ('');

    // format property name
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ';

    // iterate through color values
    for i := 0 to colorCount - 1 do
    begin
        // first value?
        if (i = 0) then
            Result := Result + ', ';

        Result := Result + m_Values[i].ToHex(False);
    end;

    // close the formatted string
    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropColor.ToXml: UnicodeString;
var
    colorCount, i: Integer;
begin
    colorCount := Length(m_Values);

    // no value to export?
    if (colorCount = 0) then
        Exit ('');

    // format property name
    Result := ItemName + '=\"';

    // iterate through color values
    for i := 0 to colorCount - 1 do
    begin
        // first value?
        if (i = 0) then
            Result := Result + ';';

        Result := Result + m_Values[i].ToHex(False);
    end;

    // close the formatted string
    Result := Result + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGPropColor.ParseColor(const value: UnicodeString): TWColor;
begin
    // is a standard color?
    if (TWStandardColor.GetInstance.Get(value, Result)) then
        Exit;

    // is a css function?
    if (Result.ValidateFunction(value)) then
    begin
        // parse css function
        Result.FromFunction(value);
        Exit;
    end;

    // convert hexadecimal color code
    Result.FromHex(UncompressColor(value), TWColor.IEHexFormat.IE_HF_RGB);
end;
//---------------------------------------------------------------------------
class function TWSVGPropColor.UncompressColor(const color: UnicodeString): UnicodeString;
var
    colorLength:       NativeUInt;
    validFormat:       Boolean;
    uncompressedColor: UnicodeString;
    c:                 WideChar;
begin
    // get color length
    colorLength := Length(color);

    // is color value compressed?
    if ((colorLength > 6) or ((colorLength = 6) and (color[1] <> '#'))) then
        Exit(color);

    // is compression format valid?
    validFormat := ((colorLength = 3) or ((colorLength = 4) and (color[1] = '#')));
    if (not validFormat) then
        // return black color (tested with some browsers, that seems be the general behavior in
        // this case)
        Exit('#000000');

    // iterate through color chars
    for c in color do
        // is hexadecimal value?
        if (((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or ((c >= 'A') and (c <= 'F'))) then
        begin
            // uncompress color component (simply double the last read value)
            uncompressedColor := uncompressedColor + c;
            uncompressedColor := uncompressedColor + c;
        end;

    Result := '#' + uncompressedColor;
end;
//---------------------------------------------------------------------------
// TWSVGPropLink
//---------------------------------------------------------------------------
constructor TWSVGPropLink.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_DataType := IE_DT_Unknown;
    m_Encoding := IE_E_Unknown;
    m_Local    := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropLink.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGPropLink.ParseLink(const data: UnicodeString): Boolean;
begin
    if (Length(data) = 0) then
        Exit(False);

    // first link char is a #? (this means that the link is local)
    if (data[1] = '#') then
    begin
        m_Value := TWStringHelper.Substr(data, 1, Length(data) - 1);
        m_Local := True;
    end
    else
    begin
        m_Value := data;
        m_Local := False;
    end;

    Result := (Length(m_Value) > 0);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropLink.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropLink;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropLink)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropLink;

    // copy data from source
    m_Value    := pSource.m_Value;
    m_DataType := pSource.m_DataType;
    m_Encoding := pSource.m_Encoding;
    m_Local    := pSource.m_Local;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropLink.Clear;
begin
    inherited Clear;

    m_Value    := '';
    m_DataType := IE_DT_Unknown;
    m_Encoding := IE_E_Unknown;
    m_Local    := False;
end;
//---------------------------------------------------------------------------
function TWSVGPropLink.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropLink.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropLink.Parse(const data: UnicodeString): Boolean;
var
    tagPos, typePos, encPos: Integer;
    url, dataType, encoding: UnicodeString;
    dataLength, i, index:    NativeUInt;
begin
    // search for data tag in data
    tagPos := System.Pos(C_SVG_Global_Data + ':', data);

    // check if data begins with data attribute and contains at least the "data" chars
    if ((tagPos = 1) and (Length(data) > 4)) then
    begin
        // search for the data type separator
        typePos  := System.Pos(';', data);

        // found it?
        if (typePos >= Length(data)) then
        begin
            TWLogHelper.LogToCompiler('Link - found unknown data - ' + data);
            m_DataType := IE_DT_Unknown;
            m_Encoding := IE_E_Unknown;
            m_Value    := data;
            Exit(True);
        end;

        // get the data type
        dataType := TWStringHelper.Substr(data, 5, typePos - 6);

        // found a valid data type?
        if (dataType = 'image/png') then
            m_DataType := IE_DT_PNG
        else
        if (dataType = 'image/jpeg') then
            m_DataType := IE_DT_JPG
        else
        if (dataType = 'image/svg+xml') then
            m_DataType := IE_DT_SVG
        else
        begin
            TWLogHelper.LogToCompiler('Link - found unknown data type - ' + dataType + ' - data - ' + data);
            m_DataType := IE_DT_Unknown;
            m_Encoding := IE_E_Unknown;
            m_Value    := data;
            Exit(True);
        end;

        // search for the encoding separator and extract the data type
        encPos := System.Pos(',', data);

        // found it?
        if (encPos < Length(data)) then
        begin
            // get the encoding
            encoding := TWStringHelper.Substr(data, typePos, encPos - typePos - 1);

            // found a valid encoding?
            if (encoding = 'base64') then
                m_Encoding := IE_E_Base64
            else
            begin
                TWLogHelper.LogToCompiler('Link - found unknown data encoding - ' + encoding + ' - data - ' + data);
                m_DataType := IE_DT_Unknown;
                m_Encoding := IE_E_Unknown;
                m_Value    := data;
                Exit(True);
            end;
        end
        else
            encPos := typePos;

        // extract the data itself
        m_Value := TWStringHelper.Substr(data, encPos, Length(data) - encPos);
        Exit(True);
    end;

    // standard url
    m_DataType := IE_DT_URL;
    m_Encoding := IE_E_None;

    // search for url tag in data
    tagPos := System.Pos(C_SVG_Link_URL, data);

    // check if data begins with url attribute and contains at least the "url()" chars
    if ((tagPos = 1) and (Length(data) > 5)) then
    begin
        // can trust the SVG syntax?
        if (m_pOptions.m_TrustSVGSyntax) then
            // strip the "url(...)" surrounding the value
            url := TWStringHelper.Substr(data, 4, Length(data) - 5)
        else
        begin
            // the url may contain wrong separators like url(#linearGradient3094) rgb(0, 0, 0); In
            // this case just stripping the parenthesis may cause the parser to deal with an invalid
            // value. The first closing parenthesis must be used as value end marker in this case
            dataLength := Length(data);
            index      := dataLength;

            // skip the opening "url(" marker and search for first closing parenthesis
            for i := 5 to dataLength do
                if (data[i] = ')') then
                begin
                    index := i;
                    break;
                end;

            // extract the value to parse
            url := TWStringHelper.Substr(data, 4, index - 5);
        end;

        // parse the url link
        Exit(ParseLink(url));
    end
    else
    if ((ItemName = C_SVG_Prop_HRef) or (ItemName = C_SVG_Prop_XLink_HRef)) then
        // should be a simple link, without attributes
        Exit(ParseLink(data));

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropLink.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_Value
            + ' - local - ' + TWStringHelper.BoolToStr(m_Local, True));
end;
//---------------------------------------------------------------------------
function TWSVGPropLink.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_Value + ' - local - '
            + TWStringHelper.BoolToStr(m_Local, True) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropLink.ToXml: UnicodeString;
begin
    // format string
    if (m_Local) then
        Result := ItemName + '=\"' + C_SVG_Link_URL + '(#' + m_Value + ')\"'
    else
        TWLogHelper.LogToCompiler('Log SVG link to XML - non-local values are not supported for now - '
                + m_Value);
end;
//---------------------------------------------------------------------------
// TWSVGPropMatrix
//---------------------------------------------------------------------------
constructor TWSVGPropMatrix.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_Translate;

    m_Matrix.SetIdentity;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropMatrix.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.SetValue(const name, value: UnicodeString): Boolean;
var
    matrixType:                                                     UnicodeString;
    pMatrixType:                                                    PUnicodeString;
    values:                                                         TWSVGArray<Single>;
    translate, invTranslate, scale, rotate, skew, combined, matrix: TWMatrix3x3;
    angle, c, s:                                                    Single;
begin
    // found last value?
    if (Length(value) = 0) then
        Exit(False);

    // if the value can be trusted, use it directly, otherwise clean it before
    if (m_pOptions.m_TrustSVGSyntax) then
        pMatrixType := @name
    else
    begin
        // prepare the value to be parsed
        matrixType  := TWSVGCommon.PrepareStr(name);
        pMatrixType := @matrixType;
    end;

    // search for matrix type
    if (pMatrixType^ = C_SVG_Matrix_Translate) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        translate.SetIdentity;

        // configure matrix
        case (Length(values)) of
            1:
            begin
                translate.Table[2, 0] := values[0];
                translate.Table[2, 1] := 0.0;
            end;

            2:
            begin
                translate.Table[2, 0] := values[0];
                translate.Table[2, 1] := values[1];
            end;
        end;

        // combine translation matrix with final matrix
        m_Matrix := translate.Multiply(m_Matrix);
        m_Type   := IE_Translate;
    end
    else
    if (pMatrixType^ = C_SVG_Matrix_Scale) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        scale.SetIdentity;

        // configure matrix
        case (Length(values)) of
            1:
            begin
                scale.Table[0, 0] := values[0];
                scale.Table[1, 1] := values[0];
            end;

            2:
            begin
                scale.Table[0, 0] := values[0];
                scale.Table[1, 1] := values[1];
            end;
        end;

        // combine scale matrix with final matrix
        m_Matrix := scale.Multiply(m_Matrix);
        m_Type   := IE_Scale;
    end
    else
    if (pMatrixType^ = C_SVG_Matrix_Rotate) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        Assert(Length(values) > 0);

        rotate.SetIdentity;
        translate.SetIdentity;
        invTranslate.SetIdentity;

        // get rotation angle in radians
        angle := TWGeometryTools.DegToRad(values[0]);

        // calculate sinus and cosinus values
        c := Cos(angle);
        s := Sin(angle);

        // configure matrix
        rotate.Table[0, 0] := c; rotate.Table[1, 0] := -s;
        rotate.Table[0, 1] := s; rotate.Table[1, 1] :=  c;

        // search for additional translation matrix to apply
        case (Length(values)) of
            2:
            begin
                // create a translation matrix
                translate.Table[2, 0] := values[1];
                translate.Table[2, 1] := 0.0;

                // create a translation inverse matrix
                invTranslate.Table[2, 0] := -values[1];
                invTranslate.Table[2, 1] :=  0.0;

                // build final matrix
                combined := rotate.Multiply(translate);
                combined := invTranslate.Multiply(combined);
                rotate   := combined;
            end;

            3:
            begin
                // create a translation matrix
                translate.Table[2, 0] := values[1];
                translate.Table[2, 1] := values[2];

                // create a translation inverse matrix
                invTranslate.Table[2, 0] := -values[1];
                invTranslate.Table[2, 1] := -values[2];

                // build final matrix
                combined := rotate.Multiply(translate);
                combined := invTranslate.Multiply(combined);
                rotate   := combined;
            end;
        end;

        // combine rotation matrix with final matrix
        m_Matrix := rotate.Multiply(m_Matrix);
        m_Type   := IE_Rotate;
    end
    else
    if (pMatrixType^ = C_SVG_Matrix_SkewX) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        Assert(Length(values) = 1);

        skew.SetIdentity;

        // configure matrix
        skew.Table[1, 0] := Tan(values[0]);

        // combine skew matrix with final matrix
        m_Matrix := skew.Multiply(m_Matrix);
        m_Type   := IE_SkewX;
    end
    else
    if (pMatrixType^ = C_SVG_Matrix_SkewY) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        Assert(Length(values) = 1);

        skew.SetIdentity;

        // configure matrix
        skew.Table[0, 1] := Tan(values[0]);

        // combine skew matrix with final matrix
        m_Matrix := skew.Multiply(m_Matrix);
        m_Type   := IE_SkewY;
    end
    else
    if (pMatrixType^ = C_SVG_Matrix_Matrix) then
    begin
        if (not ReadValues<Single>(value, values)) then
            Exit(False);

        Assert(Length(values) = 6);

        matrix.SetIdentity;

        // configure matrix
        matrix.Table[0, 0] := values[0];
        matrix.Table[0, 1] := values[1];
        matrix.Table[1, 0] := values[2];
        matrix.Table[1, 1] := values[3];
        matrix.Table[2, 0] := values[4];
        matrix.Table[2, 1] := values[5];

        // combine custom matrix with final matrix
        m_Matrix := matrix.Multiply(m_Matrix);
        m_Type   := IE_Custom;
    end
    else
    begin
        m_Type := IE_Unknown;

        // log unknown value
        TWLogHelper.LogToCompiler('Matrix - found unknown type - ' + name + ' - value - ' + value);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.ReadValues<T>(const str: UnicodeString; var values: TWSVGArray<T>): Boolean;
begin
    if (Length(str) = 0) then
        Exit(False);

    // get matrix table values
    Result := TWSVGCommon.ExtractValues<T>(str, values);
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.GetMatrix: PWMatrix3x3;
begin
    Result := @m_Matrix;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropMatrix.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropMatrix;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropMatrix)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropMatrix;

    // copy data from source
    m_Type := pSource.m_Type;
    m_Matrix.Assign(pSource.m_Matrix);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropMatrix.Clear;
begin
    inherited Clear;

    m_Type := IE_Translate;
    m_Matrix.SetIdentity;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropMatrix.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropMatrix.SetMatrix(pMatrix: PWMatrix3x3);
begin
    m_Matrix.Assign(pMatrix^);
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.Parse(const data: UnicodeString): Boolean;
var
    start, valuePos, offset: NativeInt;
    c:                       WideChar;
    readValue:               Boolean;
begin
    start     := 0;
    valuePos  := 0;
    offset    := 0;
    readValue := False;

    // iterate through data
    for c in data do
    begin
        case (c) of
            '(':
            begin
                // found separator, from now read value
                valuePos  := offset + 1;
                readValue := True;
            end;

            ')':
            begin
                // set value
                if (not SetValue(TWStringHelper.Substr(data, start, (valuePos - 1) - start),
                        TWStringHelper.Substr(data, valuePos, offset - valuePos)))
                then
                    Exit(False);

                // begin to read next value (if any)
                start     := offset + 1;
                valuePos  := offset + 1;
                readValue := False;
            end;

            ' ':
            begin
                // skip the spaces between the values
                if (not readValue) then
                begin
                    Inc(start);
                    Inc(valuePos);
                end;
            end;
        else
            readValue := True;
        end;

        Inc(offset);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.Parse_Unoptimized(const data: UnicodeString): Boolean;
var
    name, value: UnicodeString;
    c:           WideChar;
    readValue:   Boolean;
begin
    readValue := False;

    // iterate through data
    for c in data do
        // found start or end value separator, or char to add in name or value?
        if (c = '(') then
            // found separator, from now read value
            readValue := True
        else
        if (c = ')') then
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

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropMatrix.Log(margin: Cardinal);
begin
    case (m_Type) of
        IE_Translate:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_Translate
                    + ' - x - ' + FloatToStr(m_Matrix.Table[0, 2])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[1, 2]));

        IE_Scale:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_Scale
                    + ' - x - ' + FloatToStr(m_Matrix.Table[0, 0])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[1, 1]));

        IE_Rotate:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_Rotate
                    + ' - x - ' + FloatToStr(m_Matrix.Table[0, 0])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[0, 1]));

        IE_SkewX:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_SkewX
                    + ' - x - ' + FloatToStr(m_Matrix.Table[1, 0]));

        IE_SkewY:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_SkewY
                    + ' - y - ' + FloatToStr(m_Matrix.Table[0, 1]));

        IE_Custom:
            TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + C_SVG_Matrix_Matrix
                    + ' - a - ' + FloatToStr(m_Matrix.Table[0, 0]) + ' - b - ' + FloatToStr(m_Matrix.Table[0, 1])
                    + ' - c - ' + FloatToStr(m_Matrix.Table[1, 0]) + ' - d - ' + FloatToStr(m_Matrix.Table[1, 1])
                    + ' - e - ' + FloatToStr(m_Matrix.Table[2, 0]) + ' - f - ' + FloatToStr(m_Matrix.Table[2, 1]));
    else
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - unknown');
    end;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.Print(margin: Cardinal): UnicodeString;
begin
    case (m_Type) of
        IE_Translate:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' +
                    C_SVG_Matrix_Translate + ' - x - ' + FloatToStr(m_Matrix.Table[0, 2])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[1, 2]) + #13 + #10;

        IE_Scale:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' +
                    C_SVG_Matrix_Scale + ' - x - ' + FloatToStr(m_Matrix.Table[0, 0])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[1, 1]) + #13 + #10;

        IE_Rotate:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' +
                    C_SVG_Matrix_Rotate + ' - x - ' + FloatToStr(m_Matrix.Table[0, 0])
                    + ' - y - ' + FloatToStr(m_Matrix.Table[0, 1]) + #13 + #10;

        IE_SkewX:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' +
                    C_SVG_Matrix_SkewX + ' - x - ' + FloatToStr(m_Matrix.Table[1, 0]) + #13 + #10;

        IE_SkewY:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' +
                    C_SVG_Matrix_SkewY + ' - y - ' + FloatToStr(m_Matrix.Table[0, 1]) + #13 + #10;

        IE_Custom:
            Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - type - ' + C_SVG_Matrix_Matrix
                    + ' - a - ' + FloatToStr(m_Matrix.Table[0, 0]) + ' - b - ' + FloatToStr(m_Matrix.Table[0, 1])
                    + ' - c - ' + FloatToStr(m_Matrix.Table[1, 0]) + ' - d - ' + FloatToStr(m_Matrix.Table[1, 1])
                    + ' - e - ' + FloatToStr(m_Matrix.Table[2, 0]) + ' - f - ' + FloatToStr(m_Matrix.Table[2, 1])
                    + #13 + #10;
    else
        Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - unknown' + #13 + #10;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGPropMatrix.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"';

    case (m_Type) of
        IE_Translate:
            Result := Result + C_SVG_Matrix_Translate + '(' + FloatToStr(m_Matrix.Table[2, 0]) + ','
                    + FloatToStr(m_Matrix.Table[2, 1]) + ')';

        IE_Scale:
            Result := Result + C_SVG_Matrix_Scale + '(' + FloatToStr(m_Matrix.Table[0, 0]) + ','
                    + FloatToStr(m_Matrix.Table[1, 1]) + ')';

        IE_Rotate:
            Result := Result + C_SVG_Matrix_Rotate + '(' + FloatToStr(m_Matrix.Table[0, 0]) + ','
                    + FloatToStr(m_Matrix.Table[0, 1]) + ')';

        IE_SkewX: Result := Result + C_SVG_Matrix_SkewX + '(' + FloatToStr(m_Matrix.Table[1, 0]) + ')';
        IE_SkewY: Result := Result + C_SVG_Matrix_SkewY + '(' + FloatToStr(m_Matrix.Table[0, 1]) + ')';

        IE_Custom:
            Result := Result + C_SVG_Matrix_Matrix + '('
                    + FloatToStr(m_Matrix.Table[0, 0]) + ',' + FloatToStr(m_Matrix.Table[0, 1]) + ','
                    + FloatToStr(m_Matrix.Table[1, 0]) + ',' + FloatToStr(m_Matrix.Table[1, 1]) + ','
                    + FloatToStr(m_Matrix.Table[2, 0]) + ',' + FloatToStr(m_Matrix.Table[2, 1])
                    + ')';
    else
        raise Exception.CreateFmt('Unknown matrix type - %d', [Integer(m_Type)]);
    end;

    // close string
    Result := Result + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGPropRect
//---------------------------------------------------------------------------
constructor TWSVGPropRect.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_X      := 0.0;
    m_Y      := 0.0;
    m_Width  := 0.0;
    m_Height := 0.0;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropRect.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropRect.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropRect;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropRect)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropRect;

    // copy data from source
    m_X      := pSource.m_X;
    m_Y      := pSource.m_Y;
    m_Width  := pSource.m_Width;
    m_Height := pSource.m_Height;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropRect.Clear;
begin
    inherited Clear;

    m_X      := 0.0;
    m_Y      := 0.0;
    m_Width  := 0.0;
    m_Height := 0.0;
end;
//---------------------------------------------------------------------------
function TWSVGPropRect.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropRect.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropRect.Parse(const data: UnicodeString): Boolean;
var
    pStrings: IWSmartPointer<TStringList>;
begin
    pStrings := TWSmartPointer<TStringList>.Create();
    ExtractStrings([' '], [], PWideChar(data), pStrings);

    // full string must be read
    if (pStrings.Count <> 4) then
        Exit(False);

    m_X      := StrToFloat(pStrings[0], g_InternationalFormatSettings);
    m_Y      := StrToFloat(pStrings[1], g_InternationalFormatSettings);
    m_Width  := StrToFloat(pStrings[2], g_InternationalFormatSettings);
    m_Height := StrToFloat(pStrings[3], g_InternationalFormatSettings);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropRect.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - (' + FloatToStr(m_X)
            + ', ' + FloatToStr(m_Y) + ', ' + FloatToStr(m_Width) + ', ' + FloatToStr(m_Height) + ')');
end;
//---------------------------------------------------------------------------
function TWSVGPropRect.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - (' + FloatToStr(m_X) + ', '
            + FloatToStr(m_Y) + ', ' + FloatToStr(m_Width) + ', ' + FloatToStr(m_Height) + ')' + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropRect.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"' + FloatToStr(m_X) + ' ' + FloatToStr(m_Y) + ' ' + FloatToStr(m_Width)
            + ' ' + FloatToStr(m_Height) + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGPropText
//---------------------------------------------------------------------------
constructor TWSVGPropText.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGPropText.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropText.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropText;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropText)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropText;

    // copy data from source
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropText.Clear;
begin
    inherited Clear;

    m_Value := '';
end;
//---------------------------------------------------------------------------
function TWSVGPropText.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropText.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropText.Parse(const data: UnicodeString): Boolean;
begin
    m_Value := data;
    Result  := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropText.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_Value);
end;
//---------------------------------------------------------------------------
function TWSVGPropText.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_Value + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropText.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"' + m_Value + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGPropTime
//---------------------------------------------------------------------------
constructor TWSVGPropTime.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Value      := TWSimpleTime.Create;
    m_Indefinite := False;
    m_Negative   := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropTime.Destroy;
begin
    m_Value.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropTime.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropTime;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropTime)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropTime;

    // copy data from source
    m_Value.Assign(pSource.m_Value);
    m_Indefinite := pSource.m_Indefinite;
    m_Negative   := pSource.m_Negative;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropTime.Clear;
begin
    inherited Clear;

    m_Value.Clear;

    m_Indefinite := False;
    m_Negative   := False;
end;
//---------------------------------------------------------------------------
function TWSVGPropTime.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropTime.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropTime.Parse(const data: UnicodeString): Boolean;
begin
    // is time indefinite?
    if (data = C_SVG_Animation_Indefinite) then
    begin
        m_Value.Clear;
        m_Indefinite := True;
        Exit(True);
    end;

    m_Indefinite := False;

    // no value to parse?
    if (Length(data) = 0) then
    begin
        m_Value.Clear;
        Exit(True);
    end;

    // is a negative time?
    if (data[1] = '-') then
    begin
        m_Negative := True;

        // unfortunately TWSimpleTime is unable to deal with negative value, so skip it
        Exit(m_Value.FromSMIL(TWStringHelper.Substr(data, 1, Length(data) - 1)));
    end;

    Result := m_Value.FromSMIL(data);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropTime.Log(margin: Cardinal);
begin
    // is time indefinite?
    if (m_Indefinite) then
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + C_SVG_Animation_Indefinite)
    else
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + m_Value.ToSMIL);
end;
//---------------------------------------------------------------------------
function TWSVGPropTime.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ';

    // is time indefinite?
    if (m_Indefinite) then
        Result := Result + C_SVG_Animation_Indefinite
    else
        Result := Result + m_Value.ToSMIL;

    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropTime.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"';

    // is time indefinite?
    if (m_Indefinite) then
        Result := Result + C_SVG_Animation_Indefinite
    else
        Result := Result + m_Value.ToSMIL;

    Result := Result + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGPropUnit
//---------------------------------------------------------------------------
constructor TWSVGPropUnit.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_UT_ObjectBoundingBox;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropUnit.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropUnit.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropUnit;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropUnit)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropUnit;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropUnit.Clear;
begin
    inherited Clear;

    m_Type := IE_UT_ObjectBoundingBox;
end;
//---------------------------------------------------------------------------
function TWSVGPropUnit.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropUnit.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropUnit.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Unit_Object_Bounding_Box) then
        m_Type := IE_UT_ObjectBoundingBox
    else
    if (data = C_SVG_Unit_User_Space_On_Use) then
        m_Type := IE_UT_UserSpaceOnUse
    else
    begin
        TWLogHelper.LogToCompiler('Parse unit - unknown type - ' + data);
        m_Type := IE_UT_ObjectBoundingBox;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropUnit.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + UnitTypeToStr(m_Type, ''));
end;
//---------------------------------------------------------------------------
function TWSVGPropUnit.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + UnitTypeToStr(m_Type, '')
            + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropUnit.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"' + UnitTypeToStr(m_Type, '') + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGPropUnit.StrToUnitType(const unitType: UnicodeString): IEType;
begin
    // search for matching type
    if (unitType = C_SVG_Unit_User_Space_On_Use) then
        Exit(IE_UT_UserSpaceOnUse)
    else
        Exit(IE_UT_ObjectBoundingBox);
end;
//---------------------------------------------------------------------------
class function TWSVGPropUnit.UnitTypeToStr(unitType: IEType; const defValue: UnicodeString): UnicodeString;
begin
    // search for unit type
    case (unitType) of
        IE_UT_UserSpaceOnUse:    Exit(C_SVG_Unit_User_Space_On_Use);
        IE_UT_ObjectBoundingBox: Exit(C_SVG_Unit_Object_Bounding_Box);
    else
        Exit(defValue);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGPropVersion
//---------------------------------------------------------------------------
constructor TWSVGPropVersion.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Version := TWVersion.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGPropVersion.Destroy;
begin
    m_Version.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropVersion.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPropVersion;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPropVersion)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPropVersion;

    // copy data from source
    m_Version.Assign(pSource.m_Version);
end;
//---------------------------------------------------------------------------
procedure TWSVGPropVersion.Clear;
begin
    inherited Clear;

    m_Version.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGPropVersion.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGPropVersion.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGPropVersion.Parse(const data: UnicodeString): Boolean;
begin
    // no data?
    if (Length(data) = 0) then
        Exit(False);

    // get version
    m_Version.Convert(data);
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPropVersion.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + m_Version.ToStr(2));
end;
//---------------------------------------------------------------------------
function TWSVGPropVersion.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + m_Version.ToStr(2) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPropVersion.ToXml: UnicodeString;
begin
    // format string
    Result := ItemName + '=\"' + m_Version.ToStr(2) + '\"';
end;
//---------------------------------------------------------------------------

end.
