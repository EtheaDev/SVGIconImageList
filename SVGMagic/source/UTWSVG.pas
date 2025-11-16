{**
 @abstract(@name provides a class to open a Scalable Vector Graphics (SVG) file.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWSVG;
{$I SVGMagic.inc}

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     Winapi.msxml,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.xmldom,
         Xml.XMLConst,
         Xml.XMLIntf,
         Xml.XMLDoc,
         Xml.Win.msxmldom,
     {$endif}
     UTWHelpers,
     UTWSVGItems,
     UTWSVGParser;

type
    {**
     Sub-class for TMSXMLDOMDocumentFactory that allows to override the CreateDOMDocument() function
    }
    {$ifndef USE_VERYSIMPLEXML}
        TSVGDocumentFactory = class(TMSXMLDOMDocumentFactory)
            public
                class function CreateDOMDocument: IXMLDOMDocument; override;
        end;
    {$endif}

    {**
     Scalable Vector Graphics (SVG) manager
     @br @bold(NOTE) See http://www.w3.org/TR/SVGCompositing/
    }
    TWSVG = class
        private
            m_UUID:     UnicodeString;
            m_Encoding: string;
            m_pParser:  TWSVGParser;
            m_Options:  TWSVGOptions;

            {**
             Log all node content and hierarchy in the debugger output
             @param(pNode Node to log)
             @param(indent Indent to apply to next level, set to 0 by default)
            }
            {$ifdef DEBUG}
                {$ifdef USE_VERYSIMPLEXML}
                    procedure LogNodeContent(pNode: TXMLNode; indent: Cardinal = 0);
                {$else}
                    procedure LogNodeContent(pNode: IXMLNode; indent: Cardinal = 0);
                {$endif}
            {$endif}

        public
            {**
             Constructor
             @param(trustSVGSyntax If @true, the SVG syntax may be trusted)
            }
            constructor Create(trustSVGSyntax: Boolean = False); virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Clear all SVG data
            }
            procedure Clear; virtual;

            {**
             Assign (i.e. copy) content from another SVG
             @param(pOther Other SVG to copy from)
            }
            procedure Assign(const pOther: TWSVG); virtual;

            {**
             Check if SVG is empty
             @returns(@true if SVG is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

            {**
             Load SVG from file
             @param(fileName File name)
             @returns(@true on success, otherwise @false)
            }
            function LoadFromFile(const fileName: TFileName): Boolean; virtual;

            {**
             Load SVG from stream
             @param(pStream File stream)
             @returns(@true on success, otherwise @false)
            }
            function LoadFromStream(const pStream: TStream): Boolean; virtual;

            {**
             Load SVG from string
             @param(str String containing the XML data)
             @returns(@true on success, otherwise @false)
            }
            function LoadFromStr(const str: UnicodeString): Boolean; virtual;

            {**
             Log content
            }
            procedure Log; virtual;

            {**
             Print svg content to string
             @param(margin Margin length in chars)
             @returns(SVG content)
            }
            function Print(margin: Cardinal): UnicodeString; virtual;

            {**
             Get unique ID identifying the last opened SVG
             @return unique identifier, empty string no SVG was load or on error
            }
            function GetUUID: UnicodeString; virtual;

        public
            {**
             Get the SVG parser
            }
            property Parser: TWSVGParser read m_pParser;

            {**
             Get the SVG unique identifier
            }
            property UUID: UnicodeString read GetUUID;

            {**
             Get the SVG encoding
            }
            property Encoding: string read m_Encoding;
    end;

implementation
//---------------------------------------------------------------------------
// TSVGDocumentFactory
//---------------------------------------------------------------------------
{$ifndef USE_VERYSIMPLEXML}
    class function TSVGDocumentFactory.CreateDOMDocument: IXMLDOMDocument;
    begin
        Result := inherited;

        if not Assigned(Result) then
            raise DOMException.Create(SMSDOMNotInstalled);

        // set this property to False to disable the "DTD is prohibited" error while XML is loaded, in
        // case it contains a line like follow:
        // <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
        AddDOMProperty('ProhibitDTD', False);

        SetDOMProperties(Result as IXMLDOMDocument2);
    end;
{$endif}
//---------------------------------------------------------------------------
// TWSVG
//---------------------------------------------------------------------------
constructor TWSVG.Create(trustSVGSyntax: Boolean);
begin
    inherited Create;

    m_Options := Default(TWSVGOptions);
    m_pParser := TWSVGParser.Create(@m_Options);

    // configure the options
    m_Options.m_TrustSVGSyntax := trustSVGSyntax;
end;
//---------------------------------------------------------------------------
destructor TWSVG.Destroy;
begin
    m_pParser.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
{$ifdef DEBUG}
    {$ifdef USE_VERYSIMPLEXML}
        procedure TWSVG.LogNodeContent(pNode: TXMLNode; indent: Cardinal);
    {$else}
        procedure TWSVG.LogNodeContent(pNode: IXMLNode; indent: Cardinal);
    {$endif}
    var
        {$ifdef USE_VERYSIMPLEXML}
            pChild:        TXMLNode;
            pAttrs:        TXMLAttributeList;
            pAttr:         TXMLAttribute;
        {$else}
            pChild, pAttr: IXMLNode;
            pAttrs:        IXMLNodeList;
        {$endif}
        name:              UnicodeString;
        i, attrCount:      Integer;
    begin
        if (not Assigned(pNode)) then
            Exit;

        name := pNode.NodeName;

        {$ifdef USE_VERYSIMPLEXML}
            // trim all CRLF chars because very simple xml parser will not do that
            name := StringReplace(StringReplace(name, #10, '', [rfReplaceAll]), #13, '', [rfReplaceAll]);
        {$endif}

        TWLogHelper.LogToCompiler(StringOfChar(' ', indent) + 'Name - ' + name);
        TWLogHelper.LogToCompiler(StringOfChar(' ', indent) + 'Type - ' + IntToStr(Integer(pNode.NodeType)));

        if (pNode.IsTextElement) then
            TWLogHelper.LogToCompiler(StringOfChar(' ', indent) + 'Content - ' + pNode.NodeValue);

        {$ifdef USE_VERYSIMPLEXML}
            pAttrs := pNode.AttributeList;
        {$else}
            pAttrs := pNode.AttributeNodes;
        {$endif}

        if (Assigned(pAttrs)) then
        begin
            attrCount := pAttrs.Count;

            for i := 0 to attrCount - 1 do
            begin
                {$ifdef USE_VERYSIMPLEXML}
                    pAttr := pAttrs.Items[i];

                    // trim all CRLF chars because very simple xml parser will not do that
                    name := StringReplace(StringReplace(pAttr.Name, #10, '', [rfReplaceAll]), #13, '', [rfReplaceAll]);

                    TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Attribute - ' + name);
                    TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Type - '      + IntToStr(Integer(pAttr.AttributeType)));
                    TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Content - '   + pAttr.Value);
                {$else}
                    pAttr := pAttrs.Nodes[i];

                    TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Attribute - ' + pAttr.NodeName);
                    TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Type - '      + IntToStr(Integer(pAttr.NodeType)));

                    if (pAttr.NodeType = ntAttribute) then
                        TWLogHelper.LogToCompiler(StringOfChar(' ', indent + 2) + 'Content - ' + pAttr.NodeValue);
                {$endif}
            end;
        end;

        for i := 0 to pNode.ChildNodes.Count - 1 do
        begin
            pChild := pNode.ChildNodes.Get(i);
            LogNodeContent(pChild, indent + 4);
        end;
    end;
{$endif}
//---------------------------------------------------------------------------
procedure TWSVG.Clear;
begin
    m_UUID     := '';
    m_Encoding := '';
    m_pParser.Clear;
end;
//---------------------------------------------------------------------------
procedure TWSVG.Assign(const pOther: TWSVG);
begin
    Clear;

    if (not Assigned(pOther) or not(pOther is TWSVG)) Then
        Exit;

    m_pParser.Assign(pOther.m_pParser);
    m_UUID     := pOther.m_UUID;
    m_Encoding := pOther.m_Encoding;

    // update the options
    m_Options.m_TrustSVGSyntax := pOther.m_Options.m_TrustSVGSyntax;
end;
//---------------------------------------------------------------------------
function TWSVG.IsEmpty: Boolean;
begin
    Result := m_pParser.IsEmpty;
end;
//---------------------------------------------------------------------------
function TWSVG.LoadFromFile(const fileName: TFileName): Boolean;
var
    {$ifdef USE_VERYSIMPLEXML}
        pDocument: TXmlVerySimple;
    {$else}
        pDocument: IXMLDocument;
    {$endif}
    uid:       TGuid;
    hRes:      HResult;
begin
    {$ifdef USE_VERYSIMPLEXML}
        pDocument := nil;

        try
    {$endif}
            try
                m_UUID := '';

                if (not FileExists(fileName)) then
                begin
                    TWLogHelper.LogToCompiler('Load from file - FAILED - file does not exist - '
                            + fileName);
                    Exit(False);
                end;

                // load file
                {$ifdef USE_VERYSIMPLEXML}
                    pDocument := TXmlVerySimple.Create;
                    pDocument.LoadFromFile(fileName);
                {$else}
                    pDocument := LoadXMLDocument(fileName);
                {$endif}

                // get the document encoding
                m_Encoding := pDocument.Encoding;

                {$ifdef DEBUG}
                    //LogNodeContent(pDocument.DocumentElement);
                {$endif}

                if (Assigned(pDocument) and m_pParser.Load(pDocument)) then
                begin
                    hRes := CreateGuid(uid);

                    if (hRes <> S_OK) then
                    begin
                        TWLogHelper.LogToCompiler('Load from file - FAILED - could not create GUID');
                        Exit(False);
                    end;

                    // generate new unique identifier for this instance
                    m_UUID := GuidToString(uid);

                    Exit(True);
                end;
            except
                TWLogHelper.LogToCompiler('Load from file - FAILED - unexpected error');
                Exit(False);
            end;
    {$ifdef USE_VERYSIMPLEXML}
        finally
            pDocument.Free;
        end;
    {$endif}

    Result := False;
end;
//---------------------------------------------------------------------------
function TWSVG.LoadFromStream(const pStream: TStream): Boolean;
var
    {$ifdef USE_VERYSIMPLEXML}
        pDocument: TXmlVerySimple;
    {$else}
        pDocument: IXMLDocument;
    {$endif}
    uid:           TGuid;
    hRes:          HResult;
begin
    {$ifdef USE_VERYSIMPLEXML}
        pDocument := nil;

        try
    {$endif}
            try
                m_UUID := '';

                // load file
                {$ifdef USE_VERYSIMPLEXML}
                    pDocument := TXmlVerySimple.Create;
                {$else}
                    pDocument := TXMLDocument.Create(nil);
                {$endif}
                pDocument.LoadFromStream(pStream);

                // get the document encoding
                m_Encoding := pDocument.Encoding;

                {$ifdef DEBUG}
                    //LogNodeContent(pDocument.DocumentElement);
                {$endif}

                if (Assigned(pDocument) and m_pParser.Load(pDocument)) then
                begin
                    hRes := CreateGuid(uid);

                    if (hRes <> S_OK) then
                    begin
                        TWLogHelper.LogToCompiler('Load from stream - FAILED - could not create GUID');
                        Exit(False);
                    end;

                    // generate new unique identifier for this instance
                    m_UUID := GuidToString(uid);

                    Exit(True);
                end;
            except
                TWLogHelper.LogToCompiler('Load from stream - FAILED - unexpected error - see dump below');
                TWLogHelper.Dump(pStream, 500);
                Exit(False);
            end;
    {$ifdef USE_VERYSIMPLEXML}
        finally
            pDocument.Free;
        end;
    {$endif}

    TWLogHelper.LogToCompiler('Load from stream - FAILED - see dump below');
    TWLogHelper.Dump(pStream, 500);
    Result := False;
end;
//---------------------------------------------------------------------------
function TWSVG.LoadFromStr(const str: UnicodeString): Boolean;
var
    pStrStream: TStringStream;
begin
    pStrStream := nil;

    try
        // write the string in a stream
        pStrStream := TStringStream.Create;
        pStrStream.WriteString(str);

        // load the SVG from string stream
        Result := LoadFromStream(pStrStream);
    finally
        pStrStream.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVG.Log;
begin
    // log file content
    m_pParser.Log(17);
end;
//---------------------------------------------------------------------------
function TWSVG.Print(margin: Cardinal): UnicodeString;
begin
    Result := m_pParser.Print(margin);
end;
//---------------------------------------------------------------------------
function TWSVG.GetUUID: UnicodeString;
begin
    Result := m_UUID;
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
begin
    // replace the default MSXML DOM document factory by a SVG factory that disables the DTD checking
    {$ifndef USE_VERYSIMPLEXML}
        MSXMLDOMDocumentFactory := TSVGDocumentFactory;
    {$endif}
end;
//---------------------------------------------------------------------------

end.
