{**
 @abstract(@name contains the Scalable Vector Graphics (SVG) shapes. A SVG shape may be a path, a
           circle, a polygon, ...)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGElements;
{$I SVGMagic.inc}

interface

uses System.SysUtils,
     System.Generics.Collections,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWMajorSettings,
     UTWGenericNumber,
     UTWGeometryTools,
     UTWHelpers,
     UTWGraphicPath,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGMeasure,
     UTWSVGItems,
     UTWSVGProperties,
     UTWSVGStyle,
     UTWSVGFilters,
     UTWSVGAnimation,
     UTWSVGGradients;

type
    {**
     Scalable Vector Graphics (SVG) container, it's a SVG element that can contain other SVG elements
    }
    TWSVGContainer = class(TWSVGElement)
        public type
            IAnimations = TObjectList<TWSVGAnimation>;

        private
            {**
             Delete and clear all data
             @param(destroying If @True, the data are deleted in a destroying context)
            }
            procedure DelAndClear(destroying: Boolean);

        protected
            m_pElements:     TWSVGElement.IElements;
            m_pDefsElements: TWSVGElement.IElements;
            m_pAnimations:   IAnimations;

            {**
             Get element at index
             @param(index Index at which element should be get)
             @returns(Element)
            }
            function GetElement(index: Integer): TWSVGElement; virtual;

            {**
             Get element count
             @returns(Element count)
            }
            function GetElementCount: NativeUInt; virtual;

            {**
             Get animation at index
             @param(index Index at which animation should be get)
             @returns(Animation)
            }
            function GetAnimation(index: Integer): TWSVGAnimation; virtual;

            {**
             Get animation count
             @returns(Animation count)
            }
            function GetAnimationCount: NativeUInt; virtual;

            {**
             Read item
             @param(name Item name)
             @param(pNode Xml node containing item to read)
             @param(pElements @bold([out]) Elements list in which item will be added)
             @returns(@true if item was successfully read, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function ReadItem(const name: UnicodeString; const pNode: TXMLNode;
                        var pElements: TWSVGElement.IElements): Boolean; virtual;
            {$else}
                function ReadItem(const name: UnicodeString; const pNode: IXMLNode;
                        var pElements: TWSVGElement.IElements): Boolean; virtual;
            {$endif}

            {**
             Read linear gradient item and link it with defines table
             @param(pNode Xml node containing the linear gradient to read)
             @returns(@true if linear gradient item was successfully read, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function ReadLinearGradient(const pNode: TXMLNode): Boolean; virtual;
            {$else}
                function ReadLinearGradient(const pNode: IXMLNode): Boolean; virtual;
            {$endif}

            {**
             Read radial gradient item and link it with defines table
             @param(pNode Xml node containing the radial gradient to read)
             @returns(@true if radial gradient item was successfully read, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function ReadRadialGradient(const pNode: TXMLNode): Boolean; virtual;
            {$else}
                function ReadRadialGradient(const pNode: IXMLNode): Boolean; virtual;
            {$endif}

            {**
             Read filter item and link it with defines table
             @param(pNode Xml node containing the filter to read)
             @returns(@true if filter item was successfully read, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function ReadFilter(const pNode: TXMLNode): Boolean; virtual;
            {$else}
                function ReadFilter(const pNode: IXMLNode): Boolean; virtual;
            {$endif}

            {**
             Register a link to an element in the defines table
             @param(pElement Element to register)
             @returns(@true if element was successfully registered, otherwise @false)
            }
            function RegisterLink(pElement: TWSVGElement): Boolean; virtual;

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

        public
            {**
             Get element at index. Example: element := Elements[0];
             @br @bold(NOTE) @nil will be returned if index is out of bounds
            }
            property Elements[index: Integer]: TWSVGElement read GetElement;

            {**
             Get element count
            }
            property ElementCount: NativeUInt read GetElementCount;

            {**
             Get the element list
            }
            property ElementList: TWSVGElement.IElements read m_pElements;

            {**
             Get animation at index. Example: animation := Animations[0];
             @br @bold(NOTE) @nil will be returned if index is out of bounds
            }
            property Animations[index: Integer]: TWSVGAnimation read GetAnimation;

            {**
             Get animation count
            }
            property AnimationCount: NativeUInt read GetAnimationCount;
    end;

    {**
     Scalable Vector Graphics (SVG) shape, it's a base for all the kind of shapes a SVG may contain,
     e.g. rect, ellipse, path, ...
    }
    TWSVGShape = class(TWSVGContainer)
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
    end;

    {**
     Scalable Vector Graphics (SVG) group, it's a set of SVG items sharing several common properties
    }
    TWSVGGroup = class(TWSVGContainer)
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
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

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
    end;

    {**
     Scalable Vector Graphics (SVG) switch, it's a kind of group that contains e.g. shapes or images
     and contitions to evaluate, relative to system capabilities. If an object cannot be drawn by the
     current system, the next is tried, until an object could be drawn or all object were iterated
    }
    TWSVGSwitch = class(TWSVGContainer)
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
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

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
    end;

    {**
     Scalable Vector Graphics (SVG) action, it's a set of SVG items sharing several common properties,
     and that should respond to a given action (generally opening an hyperlink when the user clicks
     on them)
    }
    TWSVGAction = class(TWSVGContainer)
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
    end;

    {**
     Scalable Vector Graphics (SVG) symbol, it's a group only shown if referenced by an use element
    }
    TWSVGSymbol = class(TWSVGContainer)
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
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

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
    end;

    {**
     Scalable Vector Graphics (SVG) clip path, it's a special group owning a clipping path to apply
    }
    TWSVGClipPath = class(TWSVGContainer)
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
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

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
    end;

    {**
     Scalable Vector Graphics (SVG) embedded svg image
    }
    TWSVGSVG = class(TWSVGContainer)
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
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

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
    end;

    {**
     Scalable Vector Graphics (SVG) rectangle
    }
    TWSVGRect = class(TWSVGShape)
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
    end;

    {**
     Scalable Vector Graphics (SVG) circle
    }
    TWSVGCircle = class(TWSVGShape)
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
    end;

    {**
     Scalable Vector Graphics (SVG) ellipse
    }
    TWSVGEllipse = class(TWSVGShape)
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
    end;

    {**
     Scalable Vector Graphics (SVG) line
    }
    TWSVGLine = class(TWSVGShape)
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
    end;

    {**
     Scalable Vector Graphics (SVG) polygon
    }
    TWSVGPolygon = class(TWSVGShape)
        private
            m_Points: TWSVGArray<Single>;

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

        public
            {**
             Get points
            }
            property Points: TWSVGArray<Single> read m_Points;
    end;

    {**
     Scalable Vector Graphics (SVG) polyline
    }
    TWSVGPolyline = class(TWSVGShape)
        private
            m_Points: TWSVGArray<Single>;

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

        public
            {**
             Get points
            }
            property Points: TWSVGArray<Single> read m_Points;
    end;

    {**
     Scalable Vector Graphics (SVG) embedded image
    }
    TWSVGImage = class(TWSVGShape)
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
    end;

    {**
     Scalable Vector Graphics (SVG) text
    }
    TWSVGText = class(TWSVGShape)
        public type
            {**
             Text anchor enumeration
             @value(IE_TA_Start Text is anchored on the left or top)
             @value(IE_TA_Middle Text is anchored on the center)
             @value(IE_TA_End Text is anchored on the right or bottom)
            }
            IEAnchor =
            (
                IE_TA_Start,
                IE_TA_Middle,
                IE_TA_End
            );

            {**
             Text anchor property
            }
            IAnchor = class(TWSVGProperty)
                private
                    m_Anchor: IEAnchor;

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
                     Convert anchor to string
                     @param(anchor Anchor to convert)
                     @returns(Anchor as string)
                    }
                    class function ToStr(anchor: IEAnchor): UnicodeString; static;

                public
                    {**
                     Get or set the text anchor
                    }
                    property Anchor: IEAnchor read m_Anchor write m_Anchor;
            end;

            {**
             Text decoration enumeration
             @value(IE_D_Normal Text without decoration)
             @value(IE_D_Underline Underlined text)
             @value(IE_D_LineThrough Line through text)
            }
            IEDecoration =
            (
                IE_D_Normal,
                IE_D_Underline,
                IE_D_LineThrough
            );

            {**
             Text decoration property
            }
            IDecoration = class(TWSVGProperty)
                private
                    m_Value: IEDecoration;

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
                     Convert decoration to string
                     @param(value Decoration to convert)
                     @returns(Decoration as string)
                    }
                    class function ToStr(value: IEDecoration): UnicodeString; static;

                public
                    {**
                     Get or set the text decoration
                    }
                    property Value: IEDecoration read m_Value write m_Value;
            end;

            {**
             Text font weight property
            }
            IFontWeight = class(TWSVGProperty)
                private
                    m_Value:   Cardinal;
                    m_Bolder:  Boolean;
                    m_Lighter: Boolean;

                protected
                    {**
                     Set value
                     @param(value Weight value, between 0 and 1000)
                    }
                    procedure SetValue(value: Cardinal); virtual;

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
                     Get or set the text weight value
                    }
                    property Value: Cardinal read m_Value write SetValue;

                    {**
                     Get or set if the text weight should be bolder than the current value
                    }
                    property Bolder: Boolean read m_Bolder write m_Bolder;

                    {**
                     Get or set if the text weight should be lighter than the current value
                    }
                    property Lighter: Boolean read m_Lighter write m_Lighter;
            end;

            {**
             Text font style enumeration
             @value(IE_FS_Normal Normal font style)
             @value(IE_FS_Italic Italic font style)
             @value(IE_FS_Oblique Oblique font style, same as italic but with an optional angle value)
            }
            IEFontStyle =
            (
                IE_FS_Normal,
                IE_FS_Italic,
                IE_FS_Oblique
            );

            {**
             Text font style property
            }
            IFontStyle = class(TWSVGProperty)
                private
                    m_Style: IEFontStyle;
                    m_Angle: Single;

                protected
                    {**
                     Set angle
                     @param(value Angle in radians between -(PI / 2) and (PI / 2))
                    }
                    procedure SetAngle(value: Single); virtual;

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
                     Convert style to string
                     @param(style Style to convert)
                     @returns(Style as string)
                    }
                    class function ToStr(style: IEFontStyle): UnicodeString; static;

                public
                    {**
                     Get or set the font style
                    }
                    property Style: IEFontStyle read m_Style write m_Style;

                    {**
                     Get or set the angle in radians, if font style is oblique, between -(PI / 2) and (PI / 2)
                    }
                    property Angle: Single read m_Angle write SetAngle;
            end;

        private
            m_Text: UnicodeString;

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

        public
            {**
             Get text
            }
            property Text: UnicodeString read m_Text;
    end;

    {**
     A Scalable Vector Graphics (SVG) path command is an instruction composed by a command representing
     an action to be undertaken while the path is drawn, followed by the coordinates necessary to
     accomplish this command. E.g. an absolute "move to" path command will be written M18.285,1.5 in
     the SVG file
     @br @bold(NOTE) Several separators may be used to separate the coordinate values. Separators may
                     be a blank space, a negative '-' char or a native separator, as e.g. ',' And many
                     separators may be used in the same instruction to separate the values, e.g.
                     c0,1.76-1.572,3.39-3.615,3.39
    }
    TWSVGPathCmd = class(TWPathCmd)
        private
            m_Separator: WideChar;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(separator Default separator to use)
            }
            constructor Create(pParent: TWSVGItem; separator: WideChar); reintroduce; overload; virtual;

            {**
             Copy constructor
             @param(pOther Other command to copy from)
            }
            constructor Create(const pOther: TWSVGPathCmd); reintroduce; overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another path command
             @param(pOther Other path command)
            }
            procedure Assign(const pOther: TWPathCmd); override;

            {**
             Clear
            }
            procedure Clear; override;

            {**
             Parse data
             @param(data Data to parse)
             @param(start Start position where data will begin to be read in the string)
             @param(len Data length to read in the string)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const data: UnicodeString; start, len: NativeInt): Boolean; virtual;

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
             @returns(String)
            }
            function ToXml: UnicodeString; virtual;

            {**
             Convert SVG command char to type
             @param(cmdType Char containing SVG command type to convert)
             @param(relative @bold([out]) If @true, values owned by command are relative)
             @returns(SVG command type, IE_IT_Unknown if unknown)
            }
            class function SVGToType(const cmdType: WideChar; out relative: Boolean): TWPathCmd.IEType; static;

            {**
             Convert type to SVG command char
             @param(cmdType SVG command type to convert)
             @param(relative If @true, values owned by command are relative)
             @param(defValue Default value to return if type is unknown)
             @returns(SVG command as char, default value if unknown)
            }
            class function TypeToSVG(cmdType: TWPathCmd.IEType; relative: Boolean;
                    const defValue: WideChar): WideChar; static;
    end;

    {**
     Scalable Vector Graphics (SVG) path
    }
    TWSVGPath = class(TWSVGShape)
        private
            m_pCommands: TWPathCmds;

            {**
             Add command to list
             @param(data Data containing command)
             @param(startOffset Command start offset in data)
             @param(endOffset Command end offset in data)
             @returns(@true on success, otherwise @false)
            }
            function AddCmd(const data: UnicodeString; startOffset, endOffset: NativeUInt): Boolean;

            {**
            * Delete and clear all data
            }
            procedure DelAndClear;

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

        public
            {**
             Get path commands
            }
            property Commands: TWPathCmds read m_pCommands;
    end;

    {**
     List of scalable Vector Graphics (SVG) paths
    }
    TWSVGPaths = array of TWSVGPath;

    {**
     Scalable Vector Graphics (SVG) use instruction, which allows to reuse an existing geometry
    }
    TWSVGUse = class(TWSVGShape)
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
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGContainer
//---------------------------------------------------------------------------
constructor TWSVGContainer.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_pElements     := IElements.Create;
    m_pDefsElements := IElements.Create;
    m_pAnimations   := IAnimations.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGContainer.Destroy;
begin
    DelAndClear(True);

    m_pElements.Free;
    m_pDefsElements.Free;
    m_pAnimations.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGContainer.DelAndClear(destroying: Boolean);
var
    pDefsTable: TWSVGDefsTable;
    pElement:   TWSVGElement;
begin
    // unregister the links if performed in a non destroying context
    if (not destroying) then
    begin
        pDefsTable := GetDefsTable;

        if (Assigned(pDefsTable)) then
        begin
            // iterate through elements
            for pElement in m_pElements do
            begin
                // if element contains an identifier, removes it from the define table
                if (Assigned(pElement) and (not TWStringHelper.IsEmpty(pElement.ItemID))) then
                    pDefsTable.Remove(pElement.ItemID);
            end;

            // iterate through defines elements
            for pElement in m_pDefsElements do
            begin
                // if element contains an identifier, removes it from the define table
                if (Assigned(pElement) and (not TWStringHelper.IsEmpty(pElement.ItemID))) then
                    pDefsTable.Remove(pElement.ItemID);
            end;
        end;
    end;

    // clear all properties and defines. NOTE as TObjectList are used, the lists will take care
    // of freeing all objects they contain, for that an explicit Free() on each item isn't required
    m_pElements.Clear;
    m_pDefsElements.Clear;
    m_pAnimations.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGContainer.GetElement(index: Integer): TWSVGElement;
begin
    if (index >= m_pElements.Count) then
        Exit(nil);

    Result := m_pElements[index];
end;
//---------------------------------------------------------------------------
function TWSVGContainer.GetElementCount: NativeUInt;
begin
    Result := m_pElements.Count;
end;
//---------------------------------------------------------------------------
function TWSVGContainer.GetAnimation(index: Integer): TWSVGAnimation;
begin
    if (index >= m_pAnimations.Count) then
        Exit(nil);

    Result := m_pAnimations[index];
end;
//---------------------------------------------------------------------------
function TWSVGContainer.GetAnimationCount: NativeUInt;
begin
    Result := m_pAnimations.Count;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGContainer.ReadItem(const name: UnicodeString; const pNode: TXMLNode;
            var pElements: TWSVGElement.IElements): Boolean;
{$else}
    function TWSVGContainer.ReadItem(const name: UnicodeString; const pNode: IXMLNode;
            var pElements: TWSVGElement.IElements): Boolean;
{$endif}
var
    pGroup:       TWSVGGroup;
    pSwitch:      TWSVGSwitch;
    pAction:      TWSVGAction;
    pSymbol:      TWSVGSymbol;
    pClipPath:    TWSVGClipPath;
    pEmbeddedSVG: TWSVGSVG;
    pRect:        TWSVGRect;
    pCircle:      TWSVGCircle;
    pEllipse:     TWSVGEllipse;
    pLine:        TWSVGLine;
    pPolygon:     TWSVGPolygon;
    pPolyline:    TWSVGPolyline;
    pPath:        TWSVGPath;
    pImage:       TWSVGImage;
    pText:        TWSVGText;
    pUse:         TWSVGUse;
begin
    Result := True;

    // search for matching SVG element
    if (name = C_SVG_Tag_Group) then
    begin
        pGroup := nil;

        try
            // read group
            pGroup := TWSVGGroup.Create(Self, m_pOptions);
            Result := pGroup.Read(pNode) and Result;
            pElements.Add(pGroup);

            // register the link
            RegisterLink(pGroup);

            pGroup := nil;
        finally
            pGroup.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Switch) then
    begin
        pSwitch := nil;

        try
            // read switch
            pSwitch := TWSVGSwitch.Create(Self, m_pOptions);
            Result  := pSwitch.Read(pNode) and Result;
            pElements.Add(pSwitch);

            // register the link
            RegisterLink(pSwitch);

            pSwitch := nil;
        finally
            pSwitch.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Action) then
    begin
        pAction := nil;

        try
            // read action
            pAction := TWSVGAction.Create(Self, m_pOptions);
            Result  := pAction.Read(pNode) and Result;
            pElements.Add(pAction);

            // register the link
            RegisterLink(pAction);

            pAction := nil;
        finally
            pAction.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Symbol) then
    begin
        pSymbol := nil;

        try
            // read symbol
            pSymbol := TWSVGSymbol.Create(Self, m_pOptions);
            Result  := pSymbol.Read(pNode) and Result;
            pElements.Add(pSymbol);

            // register the link
            RegisterLink(pSymbol);

            pSymbol := nil;
        finally
            pSymbol.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_ClipPath) then
    begin
        pClipPath := nil;

        try
            // read clip path
            pClipPath := TWSVGClipPath.Create(Self, m_pOptions);
            result    := pClipPath.Read(pNode) and Result;
            pElements.Add(pClipPath);

            // register the link
            RegisterLink(pClipPath);

            pClipPath := nil;
        finally
            pClipPath.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_SVG) then
    begin
        pEmbeddedSVG := nil;

        try
            // read embedded SVG
            pEmbeddedSVG := TWSVGSVG.Create(Self, m_pOptions);
            Result := pEmbeddedSVG.Read(pNode) and Result;
            pElements.Add(pEmbeddedSVG);

            // register the link
            RegisterLink(pEmbeddedSVG);

            pEmbeddedSVG := nil;
        finally
            pEmbeddedSVG.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Rect) then
    begin
        pRect := nil;

        try
            // read rectangle
            pRect  := TWSVGRect.Create(Self, m_pOptions);
            Result := pRect.Read(pNode) and Result;
            pElements.Add(pRect);

            // register the link
            RegisterLink(pRect);

            pRect := nil;
        finally
            pRect.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Circle) then
    begin
        pCircle := nil;

        try
            // read circle
            pCircle := TWSVGCircle.Create(Self, m_pOptions);
            Result  := pCircle.Read(pNode) and Result;
            pElements.Add(pCircle);

            // register the link
            RegisterLink(pCircle);

            pCircle := nil;
        finally
            pCircle.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Ellipse) then
    begin
        pEllipse := nil;

        try
            // read ellipse
            pEllipse := TWSVGEllipse.Create(Self, m_pOptions);
            Result   := pEllipse.Read(pNode) and Result;
            pElements.Add(pEllipse);

            // register the link
            RegisterLink(pEllipse);

            pEllipse := nil;
        finally
            pEllipse.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Line) then
    begin
        pLine := nil;

        try
            // read line
            pLine  := TWSVGLine.Create(Self, m_pOptions);
            Result := pLine.Read(pNode) and Result;
            pElements.Add(pLine);

            // register the link
            RegisterLink(pLine);

            pLine := nil;
        finally
            pLine.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Polygon) then
    begin
        pPolygon := nil;

        try
            // read polygon
            pPolygon := TWSVGPolygon.Create(Self, m_pOptions);
            Result   := pPolygon.Read(pNode) and Result;
            pElements.Add(pPolygon);

            // register the link
            RegisterLink(pPolygon);

            pPolygon := nil;
        finally
            pPolygon.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Polyline) then
    begin
        pPolyline := nil;

        try
            // read polyline
            pPolyline := TWSVGPolyline.Create(Self, m_pOptions);
            Result    := pPolyline.Read(pNode) and Result;
            pElements.Add(pPolyline);

            // register the link
            RegisterLink(pPolyline);

            pPolyline := nil;
        finally
            pPolyline.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Path) then
    begin
        pPath := nil;

        try
            // read path
            pPath  := TWSVGPath.Create(Self, m_pOptions);
            Result := pPath.Read(pNode) and Result;
            pElements.Add(pPath);

            // register the link
            RegisterLink(pPath);

            pPath := nil;
        finally
            pPath.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Image) then
    begin
        pImage := nil;

        try
            // read image
            pImage := TWSVGImage.Create(Self, m_pOptions);
            Result := pImage.Read(pNode) and Result;
            pElements.Add(pImage);

            // register the link
            RegisterLink(pImage);

            pImage := nil;
        finally
            pImage.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Text) then
    begin
        pText := nil;

        try
            // read text
            pText  := TWSVGText.Create(Self, m_pOptions);
            Result := pText.Read(pNode) and Result;
            pElements.Add(pText);

            // register the link
            RegisterLink(pText);

            pText := nil;
        finally
            pText.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Use) then
    begin
        pUse := nil;

        try
            // read use instruction, which allows to reuse a geometry
            pUse   := TWSVGUse.Create(Self, m_pOptions);
            Result := pUse.Read(pNode) and Result;
            pElements.Add(pUse);

            // register the link
            RegisterLink(pUse);

            pUse := nil;
        finally
            pUse.Free;
        end;
    end
    else
    if (name = C_SVG_Tag_Linear_Gradient) then
        Result := ReadLinearGradient(pNode) and Result
    else
    if (name = C_SVG_Tag_Radial_Gradient) then
        Result := ReadRadialGradient(pNode) and Result
    else
    if (name = C_SVG_Tag_Filter) then
        result := ReadFilter(pNode) and Result;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGContainer.ReadLinearGradient(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGContainer.ReadLinearGradient(const pNode: IXMLNode): Boolean;
{$endif}
var
    pLinearGradient: TWSVGLinearGradient;
begin
    Result          := True;
    pLinearGradient := nil;

    try
        // read linear gradient
        pLinearGradient := TWSVGLinearGradient.Create(Self, m_pOptions);
        Result          := pLinearGradient.Read(pNode) and Result;
        m_pDefsElements.Add(pLinearGradient);

        // register the gradient link
        RegisterLink(pLinearGradient);

        pLinearGradient := nil;
    finally
        pLinearGradient.Free
    end;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGContainer.ReadRadialGradient(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGContainer.ReadRadialGradient(const pNode: IXMLNode): Boolean;
{$endif}
var
    pRadialGradient: TWSVGRadialGradient;
begin
    Result          := True;
    pRadialGradient := nil;

    try
        // read radial gradient
        pRadialGradient := TWSVGRadialGradient.Create(Self, m_pOptions);
        Result          := pRadialGradient.Read(pNode) and Result;
        m_pDefsElements.Add(pRadialGradient);

        // register the gradient link
        RegisterLink(pRadialGradient);

        pRadialGradient := nil;
    finally
        pRadialGradient.Free
    end;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGContainer.ReadFilter(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGContainer.ReadFilter(const pNode: IXMLNode): Boolean;
{$endif}
var
    pFilter: TWSVGFilter;
begin
    Result  := True;
    pFilter := nil;

    try
        // read radial gradient
        pFilter := TWSVGFilter.Create(Self, m_pOptions);
        Result  := pFilter.Read(pNode) and Result;
        m_pDefsElements.Add(pFilter);

        // register the filter link
        RegisterLink(pFilter);

        pFilter := nil;
    finally
        pFilter.Free
    end;
end;
//---------------------------------------------------------------------------
function TWSVGContainer.RegisterLink(pElement: TWSVGElement): Boolean;
var
    pDefsTable: TWSVGDefsTable;
    pItem:      TWSVGItem;
begin
    if (not Assigned(pElement)) then
        Exit(False);

    if (TWStringHelper.IsEmpty(pElement.ItemID)) then
        Exit(False);

    pDefsTable := pElement.DefsTable;

    if (not Assigned(pDefsTable)) then
        Exit(False);

    // each define should be unique in the map
    if (pDefsTable.TryGetValue(pElement.ItemID, pItem)) then
    begin
        TWLogHelper.LogToCompiler('Register link - FAILED - another item with same ID already exists - '
                + pElement.ItemID);
        Exit(False);
    end;

    // add the reference to the defines
    pDefsTable.Add(pElement.ItemID, pElement);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGContainer.Assign(const pOther: TWSVGItem);
var
    pSource:               TWSVGContainer;
    pAnim, pNewAnim:       TWSVGAnimation;
    pElement, pNewElement: TWSVGElement;
    shareDefsTable:        Boolean;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGContainer)) then
    begin
        Clear;
        Exit;
    end;

    // check if the both source and target items share the same parent. If yes, the target shouldn't
    // try to register its links, because they were already registered by the source item, and register
    // the same links from the target may generate conflicts
    shareDefsTable := (pOther.DefsTable = DefsTable);

    // get source object
    pSource := pOther as TWSVGContainer;

    // iterate through source elements
    for pElement in pSource.m_pElements do
    begin
        pNewElement := nil;

        try
            // create new element
            pNewElement := pElement.CreateInstance(Self);

            // copy element
            pNewElement.Assign(pElement);

            // add copied element to list
            m_pElements.Add(pNewElement);

            // register its link, if required
            if (not shareDefsTable) then
                RegisterLink(pNewElement);

            pNewElement := nil;
        finally
            pNewElement.Free;
        end;
    end;

    // iterate through source defines elements
    for pElement in pSource.m_pDefsElements do
    begin
        pNewElement := nil;

        try
            // create new element
            pNewElement := pElement.CreateInstance(Self);

            // copy element
            pNewElement.Assign(pElement);

            // add copied element to list
            m_pDefsElements.Add(pNewElement);

            // register its link, if required
            if (not shareDefsTable) then
                RegisterLink(pNewElement);

            pNewElement := nil;
        finally
            pNewElement.Free;
        end;
    end;

    // iterate through animations
    for pAnim in pSource.m_pAnimations do
    begin
        pNewAnim := nil;

        try
            // copy animation from source
            pNewAnim := TWSVGAnimation.Create(Self, m_pOptions);
            pNewAnim.Assign(pAnim);
            m_pAnimations.Add(pNewAnim);
            pNewAnim := nil;
        finally
            pNewAnim.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGContainer.Clear;
begin
    inherited Clear;

    DelAndClear(False);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGContainer.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGContainer.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pStyle:                  TWSVGStyle;
    pID:                     TWSVGPropText;
    pX, pY, pWidth, pHeight: TWSVGMeasure<Single>;
    pViewBox:                TWSVGPropRect;
    pMatrix:                 TWSVGPropMatrix;
    pClipPath:               TWSVGPropLink;
    pAspectRatio:            TWSVGPropAspectRatio;
    pAnimation:              TWSVGAnimation;
    count, i:                NativeInt;
    {$ifdef USE_VERYSIMPLEXML}
        pChildNode:          TXMLNode;
    {$else}
        pChildNode:          IXMLNode;
    {$endif}
    name:                    UnicodeString;
begin
    pStyle := nil;

    try
        pStyle := TWSVGStyle.Create(Self, m_pOptions);

        // read style
        if (pStyle.Read(C_SVG_Prop_Style, pNode)) then
        begin
            m_pProperties.Add(pStyle);
            pStyle := nil;
        end;
    finally
        pStyle.Free;
    end;

    pID := nil;

    try
        pID := TWSVGPropText.Create(Self, m_pOptions);

        // read identifier (optional)
        if (pID.Read(C_SVG_Prop_ID, pNode)) then
        begin
            // expose the identifier
            ItemID := pID.Value;

            m_pProperties.Add(pID);
            pID := nil;
        end;
    finally
        pID.Free;
    end;

    pX := nil;

    try
        pX := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read x position (optional)
        if (pX.Read(C_SVG_Prop_X, pNode)) then
        begin
            m_pProperties.Add(pX);
            pX := nil;
        end;
    finally
        pX.Free;
    end;

    pY := nil;

    try
        pY := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position (optional)
        if (pY.Read(C_SVG_Prop_Y, pNode)) then
        begin
            m_pProperties.Add(pY);
            pY := nil;
        end;
    finally
        pY.Free;
    end;

    pWidth := nil;

    try
        pWidth := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read width (optional)
        if (pWidth.Read(C_SVG_Prop_Width, pNode)) then
        begin
            m_pProperties.Add(pWidth);
            pWidth := nil;
        end;
    finally
        pWidth.Free;
    end;

    pHeight := nil;

    try
        pHeight := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read height
        if (pHeight.Read(C_SVG_Prop_Height, pNode)) then
        begin
            m_pProperties.Add(pHeight);
            pHeight := nil;
        end;
    finally
        pHeight.Free;
    end;

    pViewBox := nil;

    try
        pViewBox := TWSVGPropRect.Create(Self, m_pOptions);

        // read viewbox
        if (pViewBox.Read(C_SVG_Prop_ViewBox, pNode)) then
        begin
            m_pProperties.Add(pViewBox);
            pViewBox := nil;
        end;
    finally
        pViewBox.Free;
    end;

    pMatrix := nil;

    try
        pMatrix := TWSVGPropMatrix.Create(Self, m_pOptions);

        // read matrix (optional)
        if (pMatrix.Read(C_SVG_Prop_Transform, pNode)) then
        begin
            m_pProperties.Add(pMatrix);
            pMatrix := nil;
        end;
    finally
        pMatrix.Free;
    end;

    pClipPath := nil;

    try
        pClipPath := TWSVGPropLink.Create(Self, m_pOptions);

        // read clip path link (optional)
        if (pClipPath.Read(C_SVG_Prop_ClipPath, pNode)) then
        begin
            m_pProperties.Add(pClipPath);
            pClipPath := nil;
        end;
    finally
        pClipPath.Free;
    end;

    pAspectRatio := nil;

    try
        // read the aspect ratio (optional)
        pAspectRatio := TWSVGPropAspectRatio.Create(Self, m_pOptions);

        if (pAspectRatio.Read(C_SVG_Prop_PreserveAspectRatio, pNode)) then
        begin
            m_pProperties.Add(pAspectRatio);
            pAspectRatio := nil;
        end;
    finally
        pAspectRatio.Free;
    end;

    count  := pNode.ChildNodes.Count;
    Result := True;

    // iterate through all child elements
    for i := 0 to count - 1 do
    begin
        // get child node
        pChildNode := pNode.ChildNodes.Get(i);

        // found it?
        if (not Assigned(pChildNode)) then
            continue;

        // get child element name
        name := pChildNode.NodeName;

        {$ifdef USE_VERYSIMPLEXML}
            // trim all CRLF chars because very simple xml parser will not do that
            name := StringReplace(StringReplace(name, #10, '', [rfReplaceAll]), #13, '', [rfReplaceAll]);
        {$endif}

        // do skip blank text? (NOTE required because the XML parser consider each blank space
        // between the attributes as a generic #text attribute. For that unwished #text nodes may
        // appear while children are processed, and need to be ignored)
        if (name = C_SVG_Blank_Text_Attribute) then
            continue;

        // is an animation?
        if ((name = C_SVG_Tag_Animate) or (name = C_SVG_Tag_Animate_Color)
                or (name = C_SVG_Tag_Animate_Transform) or (name = C_SVG_Tag_Animate_Motion))
        then
        begin
            pAnimation := nil;

            try
                pAnimation := TWSVGAnimation.Create(Self, m_pOptions);

                // read child element as animation
                if (not pAnimation.Read(pChildNode)) then
                    continue;

                // add newly read animation to list
                m_pAnimations.Add(pAnimation);
                pAnimation := nil;
            finally
                pAnimation.Free;
            end;

            continue;
        end;

        // read next item
        Result := ReadItem(name, pChildNode, m_pElements) and Result;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGContainer.Log(margin: Cardinal);
var
    pElement: TWSVGElement;
    pAnim:    TWSVGAnimation;
begin
    inherited Log(margin);

    // iterate through defines elements to log
    for pElement in m_pDefsElements do
        // log child element
        pElement.Log(margin);

    // iterate through elements to log
    for pElement in m_pElements do
        // log child element
        pElement.Log(margin);

    // iterate through animations to log
    for pAnim in m_pAnimations do
        pAnim.Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGContainer.Print(margin: Cardinal): UnicodeString;
var
    pElement: TWSVGElement;
    pAnim:    TWSVGAnimation;
begin
    Result := inherited Print(margin);

    // iterate through defines elements to print
    for pElement in m_pDefsElements do
        // print child element
        Result := Result + pElement.Print(margin);

    // iterate through elements to print
    for pElement in m_pElements do
        // print child element
        Result := Result + pElement.Print(margin);

    // iterate through animations to print
    for pAnim in m_pAnimations do
        Result := Result + pAnim.Print(margin);
end;
//---------------------------------------------------------------------------
function TWSVGContainer.ToXml: UnicodeString;
var
    pElement: TWSVGElement;
    pAnim:    TWSVGAnimation;
begin
    Result := inherited ToXml;

    // iterate through defines elements to print
    for pElement in m_pDefsElements do
        // print child element
        Result := Result + pElement.ToXml;

    // iterate through elements to print
    for pElement in m_pElements do
        // print child element
        Result := Result + pElement.ToXml;

    // iterate through animations to convert to xml
    for pAnim in m_pAnimations do
        Result := Result + pAnim.ToXml;
end;
//---------------------------------------------------------------------------
// TWSVGShape
//---------------------------------------------------------------------------
constructor TWSVGShape.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGShape.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGGroup
//---------------------------------------------------------------------------
constructor TWSVGGroup.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Group;
end;
//---------------------------------------------------------------------------
destructor TWSVGGroup.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGGroup.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGGroup.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGGroup.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Group ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGGroup.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Group>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGSwitch
//---------------------------------------------------------------------------
constructor TWSVGSwitch.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Switch;
end;
//---------------------------------------------------------------------------
destructor TWSVGSwitch.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGSwitch.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGSwitch.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGSwitch.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Switch ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGSwitch.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Switch>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGAction
//---------------------------------------------------------------------------
constructor TWSVGAction.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Action;
end;
//---------------------------------------------------------------------------
destructor TWSVGAction.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGAction.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGAction.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGAction.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGAction.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pHRef, pXLinkHRef: TWSVGPropLink;
begin
    pHRef := nil;

    try
        pHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read action link (optional)
        if (pHRef.Read(C_SVG_Prop_HRef, pNode)) then
        begin
            m_pProperties.Add(pHRef);
            pHRef := nil;
        end;
    finally
        pHRef.Free;
    end;

    pXLinkHRef := nil;

    try
        pXLinkHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read old style action link (optional). NOTE this kind of links are deprecated in the SVG2
        // standards, but may still appear in several SVG files
        if (pXLinkHRef.Read(C_SVG_Prop_XLink_HRef, pNode)) then
        begin
            m_pProperties.Add(pXLinkHRef);
            pXLinkHRef := nil;
        end;
    finally
        pXLinkHRef.Free;
    end;

    Result := inherited Read(pNode);
end;
//---------------------------------------------------------------------------
procedure TWSVGAction.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Action ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGAction.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Action>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGSymbol
//---------------------------------------------------------------------------
constructor TWSVGSymbol.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Symbol;
end;
//---------------------------------------------------------------------------
destructor TWSVGSymbol.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGSymbol.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGSymbol.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGSymbol.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Symbol ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGSymbol.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Symbol>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGClipPath
//---------------------------------------------------------------------------
constructor TWSVGClipPath.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_ClipPath;
end;
//---------------------------------------------------------------------------
destructor TWSVGClipPath.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGClipPath.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGClipPath.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGClipPath.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Clip path ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGClipPath.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<clipPath>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGSVG
//---------------------------------------------------------------------------
constructor TWSVGSVG.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_SVG;
end;
//---------------------------------------------------------------------------
destructor TWSVGSVG.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGSVG.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGSVG.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
procedure TWSVGSVG.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' SVG ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGSVG.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<SVG>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGRect
//---------------------------------------------------------------------------
constructor TWSVGRect.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Rect;
end;
//---------------------------------------------------------------------------
destructor TWSVGRect.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRect.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGRect.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGRect.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGRect.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pRx, pRy: TWSVGMeasure<Single>;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pRx := nil;

    try
        pRx := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radius x value (optional)
        if (not pRx.Read(C_SVG_Prop_RX, pNode)) then
        begin
            // set default value if position was omitted
            pRx.ItemName    := C_SVG_Prop_RX;
            pRx.MeasureUnit := IEUnit.IE_UN_PX;
            pRx.Value       := 0.0;
        end;

        m_pProperties.Add(pRx);
        pRx := nil;
    finally
        pRx.Free;
    end;

    pRy := nil;

    try
        pRy := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radius y value
        if (not pRy.Read(C_SVG_Prop_RY, pNode)) then
        begin
            // set default value if position was omitted
            pRy.ItemName    := C_SVG_Prop_RY;
            pRy.MeasureUnit := IEUnit.IE_UN_PX;
            pRy.Value       := 0.0;
        end;

        m_pProperties.Add(pRy);
        pRy := nil;
    finally
        pRy.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGRect.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Rect ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGRect.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Rect>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGCircle
//---------------------------------------------------------------------------
constructor TWSVGCircle.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Circle;
end;
//---------------------------------------------------------------------------
destructor TWSVGCircle.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGCircle.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGCircle.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGCircle.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGCircle.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pX, pY, pRadius: TWSVGMeasure<Single>;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pX := nil;

    try
        pX := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read x position
        if (not pX.Read(C_SVG_Prop_CX, pNode)) then
        begin
            // set default value if position was omitted
            pX.ItemName    := C_SVG_Prop_CX;
            pX.MeasureUnit := IEUnit.IE_UN_PX;
            pX.Value       := 0.0;
        end;

        m_pProperties.Add(pX);
        pX := nil;
    finally
        pX.Free;
    end;

    pY := nil;

    try
        pY := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position
        if (not pY.Read(C_SVG_Prop_CY, pNode)) then
        begin
            // set default value if position was omitted
            pY.ItemName    := C_SVG_Prop_CY;
            pY.MeasureUnit := IEUnit.IE_UN_PX;
            pY.Value       := 0.0;
        end;

        m_pProperties.Add(pY);
        pY := nil;
    finally
        pY.Free;
    end;

    pRadius := nil;

    try
        // read radius
        pRadius := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result  := pRadius.Read(C_SVG_Prop_R, pNode) and Result;
        m_pProperties.Add(pRadius);
        pRadius := nil;
    finally
        pRadius.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGCircle.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Circle ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGCircle.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Circle>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGEllipse
//---------------------------------------------------------------------------
constructor TWSVGEllipse.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Ellipse;
end;
//---------------------------------------------------------------------------
destructor TWSVGEllipse.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGEllipse.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGEllipse.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGEllipse.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGEllipse.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pX, pY, pRx, pRy: TWSVGMeasure<Single>;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pX := nil;

    try
        pX := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read x position
        if (not pX.Read(C_SVG_Prop_CX, pNode)) then
        begin
            // set default value if position was omitted
            pX.ItemName    := C_SVG_Prop_CX;
            pX.MeasureUnit := IEUnit.IE_UN_PX;
            pX.Value       := 0.0;
        end;

        m_pProperties.Add(pX);
        pX := nil;
    finally
        pX.Free;
    end;

    pY := nil;

    try
        pY := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position
        if (not pY.Read(C_SVG_Prop_CY, pNode)) then
        begin
            // set default value if position was omitted
            pY.ItemName    := C_SVG_Prop_CY;
            pY.MeasureUnit := IEUnit.IE_UN_PX;
            pY.Value       := 0.0;
        end;

        m_pProperties.Add(pY);
        pY := nil;
    finally
        pY.Free;
    end;

    pRx := nil;

    try
        // read x radius
        pRx    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pRx.Read(C_SVG_Prop_RX, pNode) and Result;
        m_pProperties.Add(pRx);
        pRx    := nil;
    finally
        pRx.Free;
    end;

    pRy := nil;

    try
        // read y radius
        pRy    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pRy.Read(C_SVG_Prop_RY, pNode) and Result;
        m_pProperties.Add(pRy);
        pRy    := nil;
    finally
        pRy.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGEllipse.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Ellipse ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGEllipse.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Ellipse>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGLine
//---------------------------------------------------------------------------
constructor TWSVGLine.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Line;
end;
//---------------------------------------------------------------------------
destructor TWSVGLine.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGLine.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGLine.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGLine.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGLine.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pX1, pY1, pX2, pY2: TWSVGMeasure<Single>;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pX1 := nil;

    try
        // read line start x position
        pX1    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pX1.Read(C_SVG_Prop_X1, pNode) and Result;
        m_pProperties.Add(pX1);
        pX1    := nil;
    finally
        pX1.Free;
    end;

    pY1 := nil;

    try
        // read line start y position
        pY1    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pY1.Read(C_SVG_Prop_Y1, pNode) and Result;
        m_pProperties.Add(pY1);
        pY1    := nil;
    finally
        pY1.Free;
    end;

    pX2 := nil;

    try
        // read line end x position
        pX2    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pX2.Read(C_SVG_Prop_X2, pNode) and Result;
        m_pProperties.Add(pX2);
        pX2    := nil;
    finally
        pX2.Free;
    end;

    pY2 := nil;

    try
        // read line end y position
        pY2    := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);
        Result := pY2.Read(C_SVG_Prop_Y2, pNode) and Result;
        m_pProperties.Add(pY2);
        pY2    := nil;
    finally
        pY2.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGLine.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Line ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGLine.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Line>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGPolygon
//---------------------------------------------------------------------------
constructor TWSVGPolygon.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Polygon;
end;
//---------------------------------------------------------------------------
destructor TWSVGPolygon.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolygon.Assign(const pOther: TWSVGItem);
var
    pSource:  TWSVGPolygon;
    point:    Single;
    index:    NativeUInt;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPolygon)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPolygon;

    SetLength(m_Points, Length(pSource.m_Points));

    index := 0;

    // iterate through source points
    for point in pSource.m_Points do
    begin
        // copy point from source
        m_Points[index] := point;
        Inc(index);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolygon.Clear;
begin
    inherited Clear;

    // clear point list
    SetLength(m_Points, 0);
end;
//---------------------------------------------------------------------------
function TWSVGPolygon.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGPolygon.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGPolygon.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGPolygon.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    data: UnicodeString;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    // get points data
    data := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Points, C_SVG_Global_Error);

    // found them?
    if (data = C_SVG_Global_Error) then
        Exit(False);

    // get point list
    Result := TWSVGCommon.ExtractValues<Single>(data, m_Points) and Result;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolygon.Log(margin: Cardinal);
var
    point: Single;
begin
    TWLogHelper.LogBlockToCompiler(' Polygon ');

    inherited Log(margin);

    // iterate through source points
    for point in m_Points do
        // log point list
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight('Value', margin, ' ') + ' - '
                + FloatToStr(point));
end;
//---------------------------------------------------------------------------
function TWSVGPolygon.Print(margin: Cardinal): UnicodeString;
var
    point: Single;
begin
    Result := '<Polygon>' + #13 + #10 + inherited Print(margin);

    // iterate through source points
    for point in m_Points do
        // copy point from source
        Result := Result + TWStringHelper.FillStrRight('Value', margin, ' ' ) + ' - '
                + FloatToStr(point) + #13 + #10;
end;
//---------------------------------------------------------------------------
// TWSVGPolyline
//---------------------------------------------------------------------------
constructor TWSVGPolyline.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Polyline;
end;
//---------------------------------------------------------------------------
destructor TWSVGPolyline.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolyline.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGPolyline;
    point:   Single;
    index:   NativeUInt;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPolyline)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPolyline;

    SetLength(m_Points, Length(pSource.m_Points));

    index := 0;

    // iterate through source points
    for point in pSource.m_Points do
    begin
        // copy point from source
        m_Points[index] := point;
        Inc(index);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolyline.Clear;
begin
    inherited Clear;

    // clear point list
    SetLength(m_Points, 0);
end;
//---------------------------------------------------------------------------
function TWSVGPolyline.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGPolyline.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGPolyline.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGPolyline.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    data: UnicodeString;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    // get points data
    data := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Points, C_SVG_Global_Error);

    // found them?
    if (data = C_SVG_Global_Error) then
        Exit(False);

    // get point list
    Result := TWSVGCommon.ExtractValues<Single>(data, m_Points) and Result;
end;
//---------------------------------------------------------------------------
procedure TWSVGPolyline.Log(margin: Cardinal);
var
    point: Single;
begin
    TWLogHelper.LogBlockToCompiler(' Polyline ');

    inherited Log(margin);

    // iterate through source points
    for point in m_Points do
        // log point list
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight('Value', margin, ' ') + ' - '
                + FloatToStr(point));
end;
//---------------------------------------------------------------------------
function TWSVGPolyline.Print(margin: Cardinal): UnicodeString;
var
    point: Single;
begin
    Result := '<Polyline>' + #13 + #10;

    // iterate through source points
    for point in m_Points do
        // copy point from source
        Result := Result + TWStringHelper.FillStrRight('Value', margin, ' ' ) + ' - '
                + FloatToStr(point) + #13 + #10;
end;
//---------------------------------------------------------------------------
// TWSVGImage
//---------------------------------------------------------------------------
constructor TWSVGImage.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Image;
end;
//---------------------------------------------------------------------------
destructor TWSVGImage.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.Assign(const pOther: TWSVGItem);
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGImage)) then
    begin
        Clear;
        Exit;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.Clear;
begin
    inherited Clear;
end;
//---------------------------------------------------------------------------
function TWSVGImage.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGImage.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGImage.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGImage.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pHRef, pXLinkHRef: TWSVGPropLink;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pHRef := nil;

    try
        pHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read action link (optional)
        if (pHRef.Read(C_SVG_Prop_HRef, pNode)) then
        begin
            m_pProperties.Add(pHRef);
            pHRef := nil;
        end;
    finally
        pHRef.Free;
    end;

    pXLinkHRef := nil;

    try
        pXLinkHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read old style action link (optional). NOTE this kind of links are deprecated in the SVG2
        // standards, but may still appear in several SVG files
        if (pXLinkHRef.Read(C_SVG_Prop_XLink_HRef, pNode)) then
        begin
            m_pProperties.Add(pXLinkHRef);
            pXLinkHRef := nil;
        end;
    finally
        pXLinkHRef.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Image ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGImage.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Image>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGText.IAnchor
//---------------------------------------------------------------------------
constructor TWSVGText.IAnchor.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Prop_Text_Anchor;
    m_Anchor := IE_TA_Start;
end;
//---------------------------------------------------------------------------
destructor TWSVGText.IAnchor.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IAnchor.Assign(const pOther: TWSVGItem);
var
    pSource: IAnchor;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IAnchor)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IAnchor;

    // copy data from source
    m_Anchor := pSource.m_Anchor;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IAnchor.Clear;
begin
    inherited Clear;

    m_Anchor := IE_TA_Start;
end;
//---------------------------------------------------------------------------
function TWSVGText.IAnchor.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IAnchor.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGText.IAnchor.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Value_Start) then
        m_Anchor := IE_TA_Start
    else
    if (data = C_SVG_Value_Middle) then
        m_Anchor := IE_TA_Middle
    else
    if (data = C_SVG_Value_End) then
        m_Anchor := IE_TA_End
    else
    begin
        TWLogHelper.LogToCompiler('Parse text anchor - unknown value - ' + data);
        m_Anchor := IE_TA_Start;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IAnchor.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Anchor));
end;
//---------------------------------------------------------------------------
function TWSVGText.IAnchor.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Anchor) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGText.IAnchor.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Anchor) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGText.IAnchor.ToStr(anchor: IEAnchor): UnicodeString;
begin
    case (anchor) of
        IE_TA_Start:  Exit(C_SVG_Value_Start);
        IE_TA_Middle: Exit(C_SVG_Value_Middle);
        IE_TA_End:    Exit(C_SVG_Value_End);
    else
        raise Exception.CreateFmt('Unknown text anchor value - %d', [Integer(anchor)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGText.IDecoration
//---------------------------------------------------------------------------
constructor TWSVGText.IDecoration.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName  := C_SVG_Prop_Text_Decoration;
    m_Value   := IE_D_Normal;
end;
//---------------------------------------------------------------------------
destructor TWSVGText.IDecoration.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IDecoration.Assign(const pOther: TWSVGItem);
var
    pSource: IDecoration;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IDecoration)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IDecoration;

    // copy data from source
    m_Value := pSource.m_Value;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IDecoration.Clear;
begin
    inherited Clear;

    m_Value := IE_D_Normal;
end;
//---------------------------------------------------------------------------
function TWSVGText.IDecoration.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IDecoration.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGText.IDecoration.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Value_Text_Decoration_Normal) then
        m_Value := IE_D_Normal
    else
    if (data = C_SVG_Value_Text_Decoration_Underline) then
        m_Value := IE_D_Underline
    else
    if (data = C_SVG_Value_Text_Decoration_Line_Through) then
        m_Value := IE_D_LineThrough
    else
    begin
        TWLogHelper.LogToCompiler('Parse text decoration - unknown value - ' + data);
        m_Value := IE_D_Normal;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IDecoration.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Value));
end;
//---------------------------------------------------------------------------
function TWSVGText.IDecoration.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Value) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGText.IDecoration.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Value) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGText.IDecoration.ToStr(value: IEDecoration): UnicodeString;
begin
    case (value) of
        IE_D_Normal:      Exit(C_SVG_Value_Text_Decoration_Normal);
        IE_D_Underline:   Exit(C_SVG_Value_Text_Decoration_Underline);
        IE_D_LineThrough: Exit(C_SVG_Value_Text_Decoration_Line_Through);
    else
        raise Exception.CreateFmt('Unknown text decoration value - %d', [Integer(value)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGText.IFontWeight
//---------------------------------------------------------------------------
constructor TWSVGText.IFontWeight.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName  := C_SVG_Prop_Font_Weight;
    m_Value   := 400;
    m_Bolder  := False;
    m_Lighter := False;
end;
//---------------------------------------------------------------------------
destructor TWSVGText.IFontWeight.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontWeight.SetValue(value: Cardinal);
begin
    if (value > 1000) then
    begin
        m_Value := 1000;
        Exit();
    end;

    m_Value := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontWeight.Assign(const pOther: TWSVGItem);
var
    pSource: IFontWeight;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IFontWeight)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IFontWeight;

    // copy data from source
    m_Value   := pSource.m_Value;
    m_Bolder  := pSource.m_Bolder;
    m_Lighter := pSource.m_Lighter;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontWeight.Clear;
begin
    inherited Clear;

    m_Value   := 400;
    m_Bolder  := False;
    m_Lighter := False;
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontWeight.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IFontWeight.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontWeight.Parse(const data: UnicodeString): Boolean;
var
    c:         WideChar;
    isNumeric: Boolean;
begin
    if (data = C_SVG_Value_Text_Weight_Normal) then
        m_Value := 400
    else
    if (data = C_SVG_Value_Text_Weight_Bold) then
        m_Value := 700
    else
    if (data = C_SVG_Value_Text_Weight_Bolder) then
        m_Bolder := True
    else
    if (data = C_SVG_Value_Text_Weight_Lighter) then
        m_Lighter := True
    else
    begin
        isNumeric := True;

        // iterate through data to read and check if each value is a numeric one
        for c in data do
            if (not TWStringHelper.IsNumeric(c, True)) then
            begin
                isNumeric := False;
                break;
            end;

        // if value is numeric, convert it, otherwise log an error and reset the content
        if (isNumeric) then
            m_Value := StrToInt(data)
        else
        begin
            TWLogHelper.LogToCompiler('Parse text weight - invalid value - ' + data);
            m_Value   := 400;
            m_Bolder  := False;
            m_Lighter := False;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontWeight.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
            + IntToStr(m_Value) + ' - bolder - ' + TWStringHelper.BoolToStr(m_Bolder, True)
            + ' - lighter - ' + TWStringHelper.BoolToStr(m_Lighter, True));
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontWeight.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + IntToStr(m_Value)
            + ' - bolder - ' + TWStringHelper.BoolToStr(m_Bolder, True)
            + ' - lighter - ' + TWStringHelper.BoolToStr(m_Lighter, True) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontWeight.ToXml: UnicodeString;
begin
    if (m_Bolder) then
        Result := ItemName + '=\"' + C_SVG_Value_Text_Weight_Bolder + '\"'
    else
    if (m_Lighter) then
        Result := ItemName + '=\"' + C_SVG_Value_Text_Weight_Lighter + '\"'
    else
        Result := ItemName + '=\"' + IntToStr(m_Value) + '\"';
end;
//---------------------------------------------------------------------------
// TWSVGText.IFontStyle
//---------------------------------------------------------------------------
constructor TWSVGText.IFontStyle.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Prop_Font_Style;
    m_Style  := IE_FS_Normal;
    m_Angle  := 0.244; // 14°
end;
//---------------------------------------------------------------------------
destructor TWSVGText.IFontStyle.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontStyle.SetAngle(value: Single);
var
    halfPi: Single;
begin
    halfPi := PI / 2.0;

    // is angle too high?
    if (value > halfPi) then
    begin
        m_Angle := halfPi;
        Exit();
    end;

    // is angle too low?
    if (value < -halfPi) then
    begin
        m_Angle := -halfPi;
        Exit();
    end;

    m_Angle := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontStyle.Assign(const pOther: TWSVGItem);
var
    pSource: IFontStyle;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IFontStyle)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IFontStyle;

    // copy data from source
    m_Style := pSource.m_Style;
    m_Angle := pSource.m_Angle;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontStyle.Clear;
begin
    inherited Clear;

    m_Style := IE_FS_Normal;
    m_Angle := 0.244; // 14°
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontStyle.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IFontStyle.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontStyle.Parse(const data: UnicodeString): Boolean;
var
    style, value, valUnit: UnicodeString;
    c:                     WideChar;
    angle:                 Single;
    readValue, readUnit:   Boolean;
begin
    if (data = C_SVG_Value_Text_Font_Style_Normal) then
        m_Style := IE_FS_Normal
    else
    if (data = C_SVG_Value_Text_Font_Style_Italic) then
        m_Style := IE_FS_Italic
    else
    if (data = C_SVG_Value_Text_Font_Style_Oblique) then
        m_Style := IE_FS_Oblique
    else
    begin
        readValue := False;
        readUnit  := False;

        // iterate through the data chars
        for c in data do
        begin
            // dispatch current char
            case c of
                ' ':
                begin
                    // style still not read (meaning that there is something to trim)?
                    if (TWStringHelper.IsEmpty(style)) then
                        continue;

                    // from now the style value will be read
                    readValue := True;
                end;
            else
                // read the style or it's optional value
                if (readValue) then
                begin
                    // is reading the number unit?
                    if ((not readUnit) and (not TWStringHelper.IsNumeric(c, False))) then
                        readUnit := True;

                    // read the value and it's unit
                    if (readUnit) then
                        valUnit := valUnit + c
                    else
                        value := value + c;
                end
                else
                    // read the style name
                    style := style + c;
            end;
        end;

        // check again the style (after trimming)
        if (style = C_SVG_Value_Text_Font_Style_Normal) then
            m_Style := IE_FS_Normal
        else
        if (style = C_SVG_Value_Text_Font_Style_Italic) then
            m_Style := IE_FS_Italic
        else
        if (style = C_SVG_Value_Text_Font_Style_Oblique) then
        begin
            m_Style := IE_FS_Oblique;

            if (not TWStringHelper.IsEmpty(value)) then
            begin
                // read angle
                angle := StrToFloat(value, g_InternationalFormatSettings);

                // convert angle to radians
                if (not TWStringHelper.IsEmpty(valUnit)) then
                begin
                    if (valUnit = C_SVG_Value_Deg) then
                        m_Angle := TWGeometryTools.DegToRad(angle)
                    else
                    if (valUnit = C_SVG_Value_Rad) then
                        m_Angle := angle
                    else
                    if (valUnit = C_SVG_Value_Grad) then
                        m_Angle := TWGeometryTools.GradToRad(angle)
                    else
                    if (valUnit = C_SVG_Value_Turn) then
                        m_Angle := TWGeometryTools.TurnToRad(angle)
                    else
                    begin
                        m_Angle := angle;
                        TWLogHelper.LogToCompiler('Parse font style - invalid unit - assume value as in radians - '
                                + valUnit);
                    end;
                end
                else
                    // by default assume the value as in degrees
                    m_Angle := TWGeometryTools.DegToRad(angle);
            end;
        end
        else
        begin
            m_Style := IE_FS_Normal;
            m_Angle := 0.244; // 14°
            TWLogHelper.LogToCompiler('Parse font style - invalid value - ' + data);
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.IFontStyle.Log(margin: Cardinal);
begin
    if (m_Style = IE_FS_Oblique) then
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - '
                + ToStr(m_Style) + ' - angle - ' + FloatToStr(m_Angle, g_InternationalFormatSettings)
                + C_SVG_Value_Rad)
    else
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Style));
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontStyle.Print(margin: Cardinal): UnicodeString;
begin
    if (m_Style = IE_FS_Oblique) then
        Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Style)
                + ' - angle - ' + FloatToStr(m_Angle, g_InternationalFormatSettings)
                + C_SVG_Value_Rad + #13 + #10
    else
        Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Style) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGText.IFontStyle.ToXml: UnicodeString;
begin
    if (m_Style = IE_FS_Oblique) then
    begin
        if (m_Angle = 0.244) then
            Result := ItemName + '=\"' + ToStr(m_Style) + '\"'
        else
            Result := ItemName + '=\"' + ToStr(m_Style) + ' '
                    + FloatToStr(m_Angle, g_InternationalFormatSettings) + C_SVG_Value_Rad + '\"'
    end
    else
        Result := ItemName + '=\"' + ToStr(m_Style) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGText.IFontStyle.ToStr(style: IEFontStyle): UnicodeString;
begin
    case (style) of
        IE_FS_Normal:  Exit(C_SVG_Value_Text_Font_Style_Normal);
        IE_FS_Italic:  Exit(C_SVG_Value_Text_Font_Style_Italic);
        IE_FS_Oblique: Exit(C_SVG_Value_Text_Font_Style_Oblique);
    else
        raise Exception.CreateFmt('Unknown font style - %d', [Integer(style)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGText
//---------------------------------------------------------------------------
constructor TWSVGText.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Text;
end;
//---------------------------------------------------------------------------
destructor TWSVGText.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGText;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGText)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGText;

    // copy data from source
    m_Text := pSource.m_Text;
end;
//---------------------------------------------------------------------------
procedure TWSVGText.Clear;
begin
    inherited Clear;

    m_Text := '';
end;
//---------------------------------------------------------------------------
function TWSVGText.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGText.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGText.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGText.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pFontSize:   TWSVGMeasure<Single>;
    pFontFamily: TWSVGPropText;
    pFontWeight: IFontWeight;
    pFontStyle:  IFontStyle;
    pAnchor:     IAnchor;
    pDecoration: IDecoration;
    fontSize:    TWGenericNumber<Single>;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pFontSize := nil;

    try
        // read the font size (optional)
        pFontSize := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        if (not pFontSize.Read(C_SVG_Prop_Font_Size, pNode)) then
        begin
            fontSize.Value     := 16.0;
            pFontSize.ItemName := C_SVG_Prop_Font_Size;
            pFontSize.Value    := fontSize;
        end;

        m_pProperties.Add(pFontSize);
        pFontSize := nil;
    finally
        pFontSize.Free;
    end;

    pFontFamily := nil;

    try
        // read the font family (optional)
        pFontFamily := TWSVGPropText.Create(Self, m_pOptions);

        if (not pFontFamily.Read(C_SVG_Prop_Font_Family, pNode)) then
        begin
            pFontFamily.ItemName := C_SVG_Prop_Font_Family;
            pFontFamily.Value    := 'Arial';
        end;

        m_pProperties.Add(pFontFamily);
        pFontFamily := nil;
    finally
        pFontFamily.Free;
    end;

    pFontWeight := nil;

    try
        // read the font weight (optional)
        pFontWeight := IFontWeight.Create(Self, m_pOptions);

        if (not pFontWeight.Read(C_SVG_Prop_Font_Weight, pNode)) then
        begin
            pFontWeight.ItemName := C_SVG_Prop_Font_Weight;
            pFontWeight.Value    := 400;
        end;

        m_pProperties.Add(pFontWeight);
        pFontWeight := nil;
    finally
        pFontWeight.Free;
    end;

    pFontStyle := nil;

    try
        // read the font style (optional)
        pFontStyle := IFontStyle.Create(Self, m_pOptions);

        if (not pFontStyle.Read(C_SVG_Prop_Font_Style, pNode)) then
        begin
            pFontStyle.ItemName := C_SVG_Prop_Font_Style;
            pFontStyle.Style    := IE_FS_Normal;
        end;

        m_pProperties.Add(pFontStyle);
        pFontStyle := nil;
    finally
        pFontStyle.Free;
    end;

    pAnchor := nil;

    try
        // read the text anchor (optional)
        pAnchor := IAnchor.Create(Self, m_pOptions);

        if (not pAnchor.Read(C_SVG_Prop_Text_Anchor, pNode)) then
        begin
            pAnchor.ItemName := C_SVG_Prop_Text_Anchor;
            pAnchor.Anchor   := IE_TA_Start;
        end;

        m_pProperties.Add(pAnchor);
        pAnchor := nil;
    finally
        pAnchor.Free;
    end;

    pDecoration := nil;

    try
        // read the text decoration (optional)
        pDecoration := IDecoration.Create(Self, m_pOptions);

        if (not pDecoration.Read(C_SVG_Prop_Text_Decoration, pNode)) then
        begin
            pDecoration.ItemName := C_SVG_Prop_Text_Decoration;
            pDecoration.Value    := IE_D_Normal;
        end;

        m_pProperties.Add(pDecoration);
        pDecoration := nil;
    finally
        pDecoration.Free;
    end;

    // get text
    m_Text := TWStringHelper.Trim(TWSVGCommon.GetValue(pNode));
end;
//---------------------------------------------------------------------------
procedure TWSVGText.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Text ');

    inherited Log(margin);

    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight('Text', margin, ' ') + ' - ' + m_Text);
end;
//---------------------------------------------------------------------------
function TWSVGText.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Text>' + #13 + #10 + inherited Print(margin) + TWStringHelper.FillStrRight('Text', margin, ' ')
            + ' - ' + m_Text + #13 + #10;
end;
//---------------------------------------------------------------------------
// TWSVGPathCmd
//---------------------------------------------------------------------------
constructor TWSVGPathCmd.Create(pParent: TWSVGItem; separator: WideChar);
begin
    inherited Create;

    m_Separator := separator;
end;
//---------------------------------------------------------------------------
constructor TWSVGPathCmd.Create(const pOther: TWSVGPathCmd);
begin
    inherited Create;

    Assign(pOther);
end;
//---------------------------------------------------------------------------
destructor TWSVGPathCmd.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGPathCmd.Assign(const pOther: TWPathCmd);
begin
    inherited Assign(pOther);
end;
//---------------------------------------------------------------------------
procedure TWSVGPathCmd.Clear;
begin
    inherited Clear;
end;
//---------------------------------------------------------------------------
function TWSVGPathCmd.Parse(const data: UnicodeString; start, len: NativeInt): Boolean;
var
    isRelative: Boolean;
    points:     TWSVGArray<Single>;
    valLen:     NativeUInt;
begin
    // clear SVG
    Clear;

    // convert instruction to type and get relative or absolute flag
    Command  := SVGToType(data[start], isRelative);
    Relative := isRelative;

    // calculate the value length to extract
    if (len <= 1) then
        valLen := 0
    else
        valLen := len - 1;

    // get path points list. NOTE skip the first letter because it always should contain the path
    // command itself (e.g. m for a relative "move to", ...)
    if (not TWSVGCommon.ExtractValues<Single>(data, start + 1, valLen, points)) then
        Exit(False);

    // copy point list. Unfortunately cannot use m_Points as ParseValues() argument, because the
    // compiler does not recognize that IPoints and TWSVGArray are both array of Single, and thus
    // the same type, so it generates an error while compiling
    m_Points := IPoints(Copy(points, Low(points), Length(points)));
    Result   := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPathCmd.Log(margin: Cardinal);
var
    value: Single;
begin
    TWLogHelper.LogToCompiler(TypeToStr(Command) + ' - relative - '
            + TWStringHelper.BoolToStr(Relative, True));

    // iterate through points
    for value in m_Points do
        TWLogHelper.LogToCompiler(TWStringHElper.FillStrRight('Point ', margin, ' ') + FloatToStr(value));
end;
//---------------------------------------------------------------------------
function TWSVGPathCmd.Print(margin: Cardinal): UnicodeString;
var
    value: Single;
begin
    Result := TypeToStr(Command) + ' - relative - ' + TWStringHelper.BoolToStr(Relative, True)
            + #13 + #10;

    // iterate through points
    for value in m_Points do
        Result := Result + TWStringHelper.FillStrRight('Point ', margin, ' ') + FloatToStr(value)
                + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGPathCmd.ToXml: UnicodeString;
var
    first: Boolean;
    value: Single;
begin
    // format string
    Result := TypeToSVG(Command, Relative, '?');
    first  := True;

    // iterate through points
    for value in m_Points do
    begin
        // is first point or negative value? (in this case separator is negative symbol itself)
        if (not first and (value >= 0.0)) then
            // add separator
            Result := Result + m_Separator;

        // add value
        Result := Result + FloatToStr(value);
        First  := False;
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGPathCmd.SVGToType(const cmdType: WideChar; out relative: Boolean): TWPathCmd.IEType;
begin
    // search for matching type
    case (cmdType) of
        C_SVG_Path_MoveTo_Absolute:                        begin; relative := False; Exit(IE_IT_MoveTo);                          end;
        C_SVG_Path_MoveTo_Relative:                        begin; relative := True;  Exit(IE_IT_MoveTo);                          end;
        C_SVG_Path_LineTo_Absolute:                        begin; relative := False; Exit(IE_IT_LineTo);                          end;
        C_SVG_Path_LineTo_Relative:                        begin; relative := True;  Exit(IE_IT_LineTo);                          end;
        C_SVG_Path_HLineTo_Absolute:                       begin; relative := False; Exit(IE_IT_Horiz_LineTo);                    end;
        C_SVG_Path_HLineTo_Relative:                       begin; relative := True;  Exit(IE_IT_Horiz_LineTo);                    end;
        C_SVG_Path_VLineTo_Absolute:                       begin; relative := False; Exit(IE_IT_Vert_LineTo);                     end;
        C_SVG_Path_VLineTo_Relative:                       begin; relative := True;  Exit(IE_IT_Vert_LineTo);                     end;
        C_SVG_Path_CurveTo_Absolute:                       begin; relative := False; Exit(IE_IT_CurveTo);                         end;
        C_SVG_Path_CurveTo_Relative:                       begin; relative := True;  Exit(IE_IT_CurveTo);                         end;
        C_SVG_Path_SmoothCurveTo_Absolute:                 begin; relative := False; Exit(IE_IT_Smooth_CurveTo);                  end;
        C_SVG_Path_SmoothCurveTo_Relative:                 begin; relative := True;  Exit(IE_IT_Smooth_CurveTo);                  end;
        C_SVG_Path_QuadraticBezier_CurveTo_Absolute:       begin; relative := False; Exit(IE_IT_Quadratic_Bezier_CurveTo);        end;
        C_SVG_Path_QuadraticBezier_CurveTo_Relative:       begin; relative := True;  Exit(IE_IT_Quadratic_Bezier_CurveTo);        end;
        C_SVG_Path_SmoothQuadraticBezier_CurveTo_Absolute: begin; relative := False; Exit(IE_IT_Smooth_Quadratic_Bezier_CurveTo); end;
        C_SVG_Path_SmoothQuadraticBezier_CurveTo_Relative: begin; relative := True;  Exit(IE_IT_Smooth_Quadratic_Bezier_CurveTo); end;
        C_SVG_Path_Elliptical_Arc_Absolute:                begin; relative := False; Exit(IE_IT_Elliptical_Arc);                  end;
        C_SVG_Path_Elliptical_Arc_Relative:                begin; relative := True;  Exit(IE_IT_Elliptical_Arc);                  end;
        C_SVG_Path_Close_Upper,
        C_SVG_Path_Close:                                  begin; relative := False; Exit(IE_IT_ClosePath);                       end;
    else
        relative := False;
        Exit(IE_IT_Unknown);
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGPathCmd.TypeToSVG(cmdType: TWPathCmd.IEType; relative: Boolean;
        const defValue: WideChar): WideChar;
begin
    // search for type
    case (cmdType) of
        IE_IT_MoveTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_MoveTo_Relative);

            Exit(C_SVG_Path_MoveTo_Absolute);
        end;

        IE_IT_LineTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_LineTo_Relative);

            Exit(C_SVG_Path_LineTo_Absolute);
        end;

        IE_IT_Horiz_LineTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_HLineTo_Relative);

            Exit(C_SVG_Path_HLineTo_Absolute);
        end;

        IE_IT_Vert_LineTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_VLineTo_Relative);

            Exit(C_SVG_Path_VLineTo_Absolute);
        end;

        IE_IT_CurveTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_CurveTo_Relative);

            Exit(C_SVG_Path_CurveTo_Absolute);
        end;

        IE_IT_Smooth_CurveTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_SmoothCurveTo_Relative);

            Exit(C_SVG_Path_SmoothCurveTo_Absolute);
        end;

        IE_IT_Quadratic_Bezier_CurveTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_QuadraticBezier_CurveTo_Relative);

            Exit(C_SVG_Path_QuadraticBezier_CurveTo_Absolute);
        end;

        IE_IT_Smooth_Quadratic_Bezier_CurveTo:
        begin
            if (relative) then
                Exit(C_SVG_Path_SmoothQuadraticBezier_CurveTo_Relative);

            Exit(C_SVG_Path_SmoothQuadraticBezier_CurveTo_Absolute);
        end;

        IE_IT_Elliptical_Arc:
        begin
            if (relative) then
                Exit(C_SVG_Path_Elliptical_Arc_Relative);

            Exit(C_SVG_Path_Elliptical_Arc_Absolute);
        end;

        IE_IT_ClosePath:
            Exit(C_SVG_Path_Close);
    else
        Exit(defValue);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGPath
//---------------------------------------------------------------------------
constructor TWSVGPath.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_pCommands := TWPathCmds.Create;
    ItemName    := C_SVG_Tag_Path;
end;
//---------------------------------------------------------------------------
destructor TWSVGPath.Destroy;
begin
    DelAndClear;

    m_pCommands.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGPath.AddCmd(const data: UnicodeString; startOffset, endOffset: NativeUInt): Boolean;
var
    pPathCmd: TWSVGPathCmd;
begin
    pPathCmd := nil;

    try
        // create new command
        pPathCmd := TWSVGPathCmd.Create(Self, ',');

        // FIXME (WARNING previously commented, don't remove the comment)
        //TWLogHelper.LogToCompiler('SVG instructions - ' + M_LogHex(pPathCmd.get()));

        // parse data
        if (not pPathCmd.Parse(data, startOffset, endOffset - startOffset)) then
            Exit(False);

        // add new command to list
        m_pCommands.Add(pPathCmd);
        pPathCmd := nil;
    finally
        pPathCmd.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGPath.DelAndClear;
begin
    // clear all commands. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pCommands.Clear;
end;
//---------------------------------------------------------------------------
procedure TWSVGPath.Assign(const pOther: TWSVGItem);
var
    pSource:               TWSVGPath;
    pCommand, pNewCommand: TWPathCmd;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGPath)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGPath;

    // iterate through source commands
    for pCommand in pSource.m_pCommands do
    begin
        pNewCommand := nil;

        try
            // create new command
            pNewCommand := TWSVGPathCmd.Create(Self, ',');

            // copy command
            pNewCommand.Assign(pCommand);

            // add copied command to list
            m_pCommands.Add(pNewCommand);
            pNewCommand := nil;
        finally
            pNewCommand.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGPath.Clear;
begin
    inherited Clear;

    DelAndClear;
end;
//---------------------------------------------------------------------------
function TWSVGPath.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGPath.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGPath.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGPath.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    data:               UnicodeString;
    dataLength, pos, i: NativeUInt;
begin
    // no xml node?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    // get path data
    data := TWSVGCommon.GetAttribute(pNode, C_SVG_Prop_Path, C_SVG_Global_Error);

    // found it?
    if (data = C_SVG_Global_Error) then
        // not found, however several SVG may contain empty paths. Although it's a suspect situation,
        // return true here to prevent the complete SVG to fail just for an unique empty path
        Exit(True);

    // if the value can be trusted, use it directly, otherwise clean it before
    if (not m_pOptions.m_TrustSVGSyntax) then
        // prepare the data to be parsed
        data := TWSVGCommon.PrepareStr(data);

    dataLength := Length(data);
    pos        := 1;

    // iterate through data
    for i := 1 to dataLength do
        // found new instruction?
        if ((i > 1) and (((data[i] >= 'a') and (data[i] <= 'z')) or ((data[i] >= 'A') and (data[i] <= 'Z')))
                and (data[i] <> 'e') and (data[i] <> 'E'))
        then
        begin
            // read instruction and add it to list. NOTE the AddCmd function offsets are 0 based
            // (because in Delphi the UnicodeString are 1 based, but the SubString() function is 0
            // based), so the offsets must be decreased of 1
            Result := AddCmd(data, pos, i) and Result;

            // update next start position
            pos := i;
        end;

    // remaining data to read?
    if (pos <= dataLength) then
        // add last path command. NOTE the AddCmd function offsets are 0 based (because in Delphi
        // the UnicodeString are 1 based, but the SubString() function is 0 based), so the start
        // offset must be decreased of 1, but NOT the data length
        Result := AddCmd(data, pos, dataLength) and Result;
end;
//---------------------------------------------------------------------------
procedure TWSVGPath.Log(margin: Cardinal);
var
    pCommand: TWPathCmd;
    pPathCmd: TWSVGPathCmd;
begin
    TWLogHelper.LogBlockToCompiler(' Path ');

    inherited Log(margin);

    for pCommand in m_pCommands do
    begin
        if (not(pCommand is TWSVGPathCmd)) then
            continue;

        pPathCmd := pCommand as TWSVGPathCmd;

        if (not Assigned(pPathCmd)) then
        begin
            TWLogHelper.LogToCompiler('Log - FAILED - path command isn''t a SVG path command');
            continue;
        end;

        pPathCmd.Log(0);
    end;
end;
//---------------------------------------------------------------------------
function TWSVGPath.Print(margin: Cardinal): UnicodeString;
var
    pCommand: TWPathCmd;
    pPathCmd: TWSVGPathCmd;
begin
    Result := '<Path>' + #13 + #10 + inherited Print(margin);

    for pCommand in m_pCommands do
    begin
        if (not(pCommand is TWSVGPathCmd)) then
            continue;

        pPathCmd := pCommand as TWSVGPathCmd;

        if (not Assigned(pPathCmd)) then
        begin
            TWLogHelper.LogToCompiler('Print - FAILED - path command isn''t a SVG path command');
            continue;
        end;

        Result := Result + pPathCmd.Print(0);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGUse
//---------------------------------------------------------------------------
constructor TWSVGUse.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Use;
end;
//---------------------------------------------------------------------------
destructor TWSVGUse.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGUse.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGUse.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGUse.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGUse.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pHRef, pXLinkHRef: TWSVGPropLink;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    // read shape common properties
    Result := inherited Read(pNode);

    pHRef := nil;

    try
        pHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read external reference to another object (optional)
        if (pHRef.Read(C_SVG_Prop_HRef, pNode)) then
        begin
            m_pProperties.Add(pHRef);
            pHRef := nil;
        end;
    finally
        pHRef.Free;
    end;

    pXLinkHRef := nil;

    try
        pXLinkHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read external link/reference to another object (optional). NOTE this kind of links are
        // deprecated in the SVG2 standards, but may still appear in several SVG files
        if (pXLinkHRef.Read(C_SVG_Prop_XLink_HRef, pNode)) then
        begin
            m_pProperties.Add(pXLinkHRef);
            pXLinkHRef := nil;
        end;
    finally
        pXLinkHRef.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGUse.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Use ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGUse.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Use>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------

end.
