{**
 @abstract(@name contains the Scalable Vector Graphics (SVG) gradients. A gradient is a kind of
           color that can be applied to a fill or a stroke.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGGradients;
{$I SVGMagic.inc}

interface

uses System.SysUtils,
     System.Generics.Collections,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWHelpers,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGItems,
     UTWSVGMeasure,
     UTWSVGProperties,
     UTWSVGStyle;

type
    {**
     SVG gradient stop, it's a point where a gradient changes from one color to the next
    }
    TWSVGGradientStop = class(TWSVGElement)
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
     SVG gradient, describes a gradient to apply to a fill or stroke
    }
    TWSVGGradient = class(TWSVGElement)
        public type
            {**
             Gradient spread method enumeration
            }
            IEGradientSpreadMethod =
            (
                IE_GS_Pad,
                IE_GS_Reflect,
                IE_GS_Repeat
            );

            {**
             Gradient spread method property
            }
            IGradientSpreadMethod = class(TWSVGProperty)
                private
                    m_Method: IEGradientSpreadMethod;

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
                     Convert gradient spread method to string
                     @param(method Gradient spread method to convert)
                     @returns(Gradient spread method as string)
                    }
                    class function ToStr(method: IEGradientSpreadMethod): UnicodeString; static;

                public
                    {**
                     Get or set the gradient spread method
                    }
                    property Method: IEGradientSpreadMethod read m_Method write m_Method;
            end;

            IGradientStops = TObjectList<TWSVGGradientStop>;

        private
            m_pGradientStops: IGradientStops;

            {**
             Delete and clear all data
            }
            procedure DelAndClear;

        protected
            {**
             Get gradient stop at index
             @param(index Index at which gradient stop should be get)
             @returns(Gradient stop)
             @raises(Exception if index is out of bounds)
            }
            function GetGradientStop(index: Integer): TWSVGGradientStop; virtual;

            {**
             Get gradient stop count
             @returns(Gradient stop count)
            }
            function GetGradientStopCount: NativeUInt; virtual;

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

        public
            {**
             Get gradient stop at index. Example: gradientStop := GradientStops[0];
             @br @bold(NOTE) An exception will be raised if index is out of bounds
            }
            property GradientStops[index: Integer]: TWSVGGradientStop read GetGradientStop;

            {**
             Get gradient stop count
            }
            property GradientStopCount: NativeUInt read GetGradientStopCount;
    end;

    {**
     SVG linear gradient, describes a linear gradient to apply to a fill or stroke
    }
    TWSVGLinearGradient = class(TWSVGGradient)
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
     SVG radial gradient, describes a radial gradient to apply to a fill or stroke
    }
    TWSVGRadialGradient = class(TWSVGGradient)
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
// TWSVGGradientStop
//---------------------------------------------------------------------------
constructor TWSVGGradientStop.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);
end;
//---------------------------------------------------------------------------
destructor TWSVGGradientStop.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGGradientStop.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGGradientStop.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGGradientStop.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGGradientStop.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pStyle:  TWSVGStyle;
    pOffset: TWSVGMeasure<Single>;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    pStyle := nil;

    try
        // create new style (should always exist in gradient stop)
        pStyle := TWSVGStyle.Create(Self, m_pOptions);

        // read it
        if (not pStyle.Read(C_SVG_Prop_Style, pNode)) then
            Exit(False);

        m_pProperties.Add(pStyle);
        pStyle := nil;
    finally
        pStyle.Free;
    end;

    pOffset := nil;

    try
        pOffset := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read stop offset
        if (not pOffset.Read(C_SVG_Gradient_Stop_Offset, pNode)) then
        begin
            // set default value if position was omitted
            pOffset.ItemName    := C_SVG_Gradient_Stop_Offset;
            pOffset.MeasureUnit := IEUnit.IE_UN_None;
            pOffset.Value       := 0.0;
        end;

        m_pProperties.Add(pOffset);
        pOffset := nil;
    finally
        pOffset.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradientStop.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Gradient stop ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGGradientStop.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Gradient stop>' + #13 + #10 + inherited Print(margin) + '</Gradient stop>' + #13 + #10;
end;
//---------------------------------------------------------------------------
// TWSVGGradient.IGradientSpreadMethod
//---------------------------------------------------------------------------
constructor TWSVGGradient.IGradientSpreadMethod.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Method := IE_GS_Pad;
end;
//---------------------------------------------------------------------------
destructor TWSVGGradient.IGradientSpreadMethod.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.IGradientSpreadMethod.Assign(const pOther: TWSVGItem);
var
    pSource: IGradientSpreadMethod;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is IGradientSpreadMethod)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as IGradientSpreadMethod;

    // copy data from source
    m_Method := pSource.m_Method;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.IGradientSpreadMethod.Clear;
begin
    inherited Clear;

    m_Method := IE_GS_Pad;
end;
//---------------------------------------------------------------------------
function TWSVGGradient.IGradientSpreadMethod.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := IGradientSpreadMethod.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGGradient.IGradientSpreadMethod.Parse(const data: UnicodeString): Boolean;
begin
    if (data = C_SVG_Gradient_Spread_Method_Pad) then
        m_Method := IE_GS_Pad
    else
    if (data = C_SVG_Gradient_Spread_Method_Reflect) then
        m_Method := IE_GS_Reflect
    else
    if (data = C_SVG_Gradient_Spread_Method_Repeat) then
        m_Method := IE_GS_Repeat
    else
    begin
        TWLogHelper.LogToCompiler('Parse gradient spread method - unknown method - ' + data);
        m_Method := IE_GS_Pad;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.IGradientSpreadMethod.Log(margin: Cardinal);
begin
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Method));
end;
//---------------------------------------------------------------------------
function TWSVGGradient.IGradientSpreadMethod.Print(margin: Cardinal): UnicodeString;
begin
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + ToStr(m_Method) + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGGradient.IGradientSpreadMethod.ToXml: UnicodeString;
begin
    Result := ItemName + '=\"' + ToStr(m_Method) + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGGradient.IGradientSpreadMethod.ToStr(method: IEGradientSpreadMethod): UnicodeString;
begin
    case (method) of
        IE_GS_Pad:     Exit(C_SVG_Gradient_Spread_Method_Pad);
        IE_GS_Reflect: Exit(C_SVG_Gradient_Spread_Method_Reflect);
        IE_GS_Repeat:  Exit(C_SVG_Gradient_Spread_Method_Repeat);
    else
        raise Exception.CreateFmt('Unknown gradient spread method - %d', [Integer(method)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGGradient
//---------------------------------------------------------------------------
constructor TWSVGGradient.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_pGradientStops := IGradientStops.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGGradient.Destroy;
begin
    DelAndClear;

    m_pGradientStops.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGGradient.GetGradientStop(index: Integer): TWSVGGradientStop;
begin
    if (index >= m_pGradientStops.Count) then
        raise Exception.Create('Index is out of bounds');

    Result := m_pGradientStops[index];
end;
//---------------------------------------------------------------------------
function TWSVGGradient.GetGradientStopCount: NativeUInt;
begin
    Result := m_pGradientStops.Count;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.DelAndClear;
begin
    // clear all gradient stops. NOTE as a TObjectList is used, the list will take care of freeing
    // all objects it contains, for that an explicit Free() on each item isn't required
    m_pGradientStops.Clear;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.Assign(const pOther: TWSVGItem);
var
    pSource:         TWSVGGradient;
    pStop, pNewStop: TWSVGGradientStop;

begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGGradient)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGGradient;

    // iterate through gradient stops to copy
    for pStop in pSource.m_pGradientStops do
    begin
        pNewStop := nil;

        try
            // copy gradient stop from source
            pNewStop := TWSVGGradientStop.Create(Self, m_pOptions);
            pNewStop.Assign(pStop);
            m_pGradientStops.Add(pNewStop);
            pNewStop := nil;
        finally
            pNewStop.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.Clear;
begin
    inherited Clear;

    DelAndClear;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGGradient.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGGradient.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pID:               TWSVGPropText;
    pGradientUnit:     TWSVGPropUnit;
    pSpreadMethod:     IGradientSpreadMethod;
    pMatrix:           TWSVGPropMatrix;
    pHRef, pXLinkHRef: TWSVGPropLink;
    count, i:          NativeInt;
    {$ifdef USE_VERYSIMPLEXML}
        pChildNode:    TXMLNode;
    {$else}
        pChildNode:    IXMLNode;
    {$endif}
    pGradientStop:     TWSVGGradientStop;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    pID := nil;

    try
        pID := TWSVGPropText.Create(Self, m_pOptions);

        // read identifier
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

    // normally a filter should always contain an identifier, otherwise it cannot be get and used
    if (Length(ItemID) = 0) then
    begin
        TWLogHelper.LogToCompiler('Read filter - FAILED - identifier is empty');
        Exit(False);
    End;

    pGradientUnit := nil;

    try
        pGradientUnit := TWSVGPropUnit.Create(Self, m_pOptions);

        // read gradient unit (optional)
        if (not pGradientUnit.Read(C_SVG_Gradient_Units, pNode)) then
        begin
            // set default value if gradient unit was omitted
            pGradientUnit.ItemName := C_SVG_Gradient_Units;
            pGradientUnit.UnitType := TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox;
        end;

        m_pProperties.Add(pGradientUnit);
        pGradientUnit := nil;
    finally
        pGradientUnit.Free;
    end;

    pSpreadMethod := nil;

    try
        pSpreadMethod := IGradientSpreadMethod.Create(Self, m_pOptions);

        // read gradient spread method (optional)
        if (not pSpreadMethod.Read(C_SVG_Gradient_Spread_Method, pNode)) then
        begin
            // set default value if gradient spread method was omitted
            pSpreadMethod.ItemName := C_SVG_Gradient_Spread_Method;
            pSpreadMethod.Method   := IE_GS_Pad;
        end;

        m_pProperties.Add(pSpreadMethod);
        pSpreadMethod := nil;
    finally
        pSpreadMethod.Free;
    end;

    pMatrix := nil;

    try
        pMatrix := TWSVGPropMatrix.Create(Self, m_pOptions);

        // read gradient transform matrix (optional)
        if (pMatrix.Read(C_SVG_Gradient_Transform, pNode)) then
        begin
            m_pProperties.Add(pMatrix);
            pMatrix := nil;
        end;
    finally
        pMatrix.Free;
    end;

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

        // do skip blank text? (NOTE required because the XML parser consider each blank space
        // between the attributes as a generic #text attribute. For that unwished #text nodes may
        // appear while children are processed, and need to be ignored)
        {$ifndef USE_VERYSIMPLEXML}
            if (pChildNode.NodeName = C_SVG_Blank_Text_Attribute) then
                continue;
        {$endif}

        pGradientStop := nil;

        try
            pGradientStop := TWSVGGradientStop.Create(Self, m_pOptions);

            // read child element as animation
            if (not pGradientStop.Read(pChildNode)) then
                continue;

            // add newly read animation to list
            m_pGradientStops.Add(pGradientStop);
            pGradientStop := nil;
        finally
            pGradientStop.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGradient.Log(margin: Cardinal);
var
    pGradientStop: TWSVGGradientStop;
begin
    inherited Log(margin);
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight('ID', margin, ' ') + ' - ' + ItemID);

    // iterate through gradient stops to log
    for pGradientStop in m_pGradientStops do
        pGradientStop.Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGGradient.Print(margin: Cardinal): UnicodeString;
var
    pGradientStop: TWSVGGradientStop;
begin
    Result := inherited Print(margin) + TWStringHelper.FillStrRight('ID', margin, ' ') + ' - '
            + ItemID + #13 + #10;

    // iterate through gradient stops to print
    for pGradientStop in m_pGradientStops do
        Result := Result + pGradientStop.Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGLinearGradient
//---------------------------------------------------------------------------
constructor TWSVGLinearGradient.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Linear_Gradient;
end;
//---------------------------------------------------------------------------
destructor TWSVGLinearGradient.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGLinearGradient.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGLinearGradient.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGLinearGradient.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGLinearGradient.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pX1, pY1, pX2, pY2: TWSVGMeasure<Single>;
begin
    inherited Read(pNode);

    pX1 := nil;

    try
        pX1 := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read gradient start x position
        if (not pX1.Read(C_SVG_Prop_X1, pNode)) then
        begin
            // set default value if position was omitted
            pX1.ItemName    := C_SVG_Prop_X1;
            pX1.MeasureUnit := IEUnit.IE_UN_None;
            pX1.Value       := 0.0;
        end;

        m_pProperties.Add(pX1);
        pX1 := nil;
    finally
        pX1.Free;
    end;

    pY1 := nil;

    try
        pY1 := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position
        if (not pY1.Read(C_SVG_Prop_Y1, pNode)) then
        begin
            // set default value if position was omitted
            pY1.ItemName    := C_SVG_Prop_Y1;
            pY1.MeasureUnit := IEUnit.IE_UN_None;
            pY1.Value       := 0.0;
        end;

        m_pProperties.Add(pY1);
        pY1 := nil;
    finally
        pY1.Free;
    end;

    pX2 := nil;

    try
        pX2 := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read x position
        if (not pX2.Read(C_SVG_Prop_X2, pNode)) then
        begin
            // set default value if position was omitted
            pX2.ItemName    := C_SVG_Prop_X2;
            pX2.MeasureUnit := IEUnit.IE_UN_None;
            pX2.Value       := 0.0;
        end;

        m_pProperties.Add(pX2);
        pX2 := nil;
    finally
        pX2.Free;
    end;

    pY2 := nil;

    try
        pY2 := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position
        if (not pY2.Read(C_SVG_Prop_Y2, pNode)) then
        begin
            // set default value if position was omitted
            pY2.ItemName    := C_SVG_Prop_Y2;
            pY2.MeasureUnit := IEUnit.IE_UN_None;
            pY2.Value       := 0.0;
        end;

        m_pProperties.Add(pY2);
        pY2 := nil;
    finally
        pY2.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGLinearGradient.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Linear gradient ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGLinearGradient.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Linear gradient>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------
// TWSVGRadialGradient
//---------------------------------------------------------------------------
constructor TWSVGRadialGradient.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName := C_SVG_Tag_Radial_Gradient;
end;
//---------------------------------------------------------------------------
destructor TWSVGRadialGradient.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRadialGradient.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGRadialGradient.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGRadialGradient.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGRadialGradient.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pCx, pCy, pR, pFx, pFy: TWSVGMeasure<Single>;
begin
    inherited Read(pNode);

    pCx := nil;

    try
        pCx := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radial gradient center x position
        if (not pCx.Read(C_SVG_Prop_CX, pNode)) then
        begin
            // set default value if position was omitted
            pCx.ItemName    := C_SVG_Prop_CX;
            pCx.MeasureUnit := IEUnit.IE_UN_None;
            pCx.Value       := 0.0;
        end;

        m_pProperties.Add(pCx);
        pCx := nil;
    finally
        pCx.Free;
    end;

    pCy := nil;

    try
        pCy := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radial gradient center y position
        if (not pCy.Read(C_SVG_Prop_CY, pNode)) then
        begin
            // set default value if position was omitted
            pCy.ItemName    := C_SVG_Prop_CY;
            pCy.MeasureUnit := IEUnit.IE_UN_None;
            pCy.Value       := 0.0;
        end;

        m_pProperties.Add(pCy);
        pCy := nil;
    finally
        pCy.Free;
    end;

    pR := nil;

    try
        pR := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radial gradient radius position
        if (not pR.Read(C_SVG_Prop_R, pNode)) then
        begin
            // set default value if radius was omitted
            pR.ItemName    := C_SVG_Prop_R;
            pR.MeasureUnit := IEUnit.IE_UN_None;
            pR.Value       := 0.0;
        end;

        m_pProperties.Add(pR);
        pR := nil;
    finally
        pR.Free;
    end;

    pFx := nil;

    try
        pFx := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radial gradient focus x position (optional)
        if (pFx.Read(C_SVG_Prop_FX, pNode)) then
        begin
            m_pProperties.Add(pFx);
            pFx := nil;
        end;
    finally
        pFx.Free;
    end;

    pFy := nil;

    try
        pFy := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read radial gradient focus y position (optional)
        if (pFy.Read(C_SVG_Prop_FY, pNode)) then
        begin
            m_pProperties.Add(pFy);
            pFy := nil;
        end;
    finally
        pFy.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadialGradient.Log(margin: Cardinal);
begin
    TWLogHelper.LogBlockToCompiler(' Radial gradient ');

    inherited Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGRadialGradient.Print(margin: Cardinal): UnicodeString;
begin
    Result := '<Radial gradient>' + #13 + #10 + inherited Print(margin);
end;
//---------------------------------------------------------------------------

end.
