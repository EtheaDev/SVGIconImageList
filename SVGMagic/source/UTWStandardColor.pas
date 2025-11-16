{**
 @abstract(@name provides a standard color dictionary that allows to get a known color by naming it,
           e.g. Get('yellow') will return a TWColor set to yellow.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWStandardColor;

interface

uses System.SysUtils,
     System.Generics.Collections, Vcl.Forms,
     UTWColor;

type
    {**
     Standard color interface
    }
    IWStandardColor = interface['{07BB7AC3-52FC-4EDE-98FC-0C0785D023C7}']
        {**
         Check if color exists in dictionary
         @param(name Color name)
         @returns(@true if exists, otherwise @false)
        }
        function Exists(const name: UnicodeString): Boolean;

        {**
         Get color
         @param(name Color name)
         @param(color @bold([out]) Color if found, empty color (i.e. set to RGBA = 0) otherwise)
         @returns(@true if exists, otherwise @false)
        }
        function Get(const name: UnicodeString; out color: TWColor): Boolean;
    end;

    {**
     Standard predefined colors dictionary
    }
    TWStandardColor = class sealed (TInterfacedObject, IWStandardColor)
        private type
            IColorDictionary = TDictionary<UnicodeString, TWColor>;

        private
            class var m_pInstance:  TWStandardColor;
                      m_pColorDict: IColorDictionary;

        protected
            {**
             Create color dictionary
            }
            procedure CreateColorDictionary;

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
             Gets standard colors instance, creates one if still not created
             @returns(Standard colors instance)
            }
            class function GetInstance: IWStandardColor; static;

            {**
             Check if color exists in dictionary
             @param(name Color name)
             @returns(@true if exists, otherwise @false)
            }
            function Exists(const name: UnicodeString): Boolean; virtual;

            {**
             Get color
             @param(name Color name)
             @param(color @bold([out]) Color if found, empty color (e.g. set to RGBA = 0) otherwise)
             @returns(@true if exists, otherwise @false)
            }
            function Get(const name: UnicodeString; out color: TWColor): Boolean; virtual;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWStandardColor.Create;
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    inherited Create;

    m_pColorDict := IColorDictionary.Create;

    CreateColorDictionary;
end;
//---------------------------------------------------------------------------
destructor TWStandardColor.Destroy;
begin
    m_pColorDict.Free;

    inherited Destroy;

    m_pInstance := nil;
end;
//---------------------------------------------------------------------------
procedure TWStandardColor.CreateColorDictionary;
begin
    // populate color dictionary with standard colors
    m_pColorDict.Add('none',                 TWColor.Create(0,   0,   0,   0));
    m_pColorDict.Add('aliceblue',            TWColor.Create(240, 248, 255));
    m_pColorDict.Add('antiquewhite',         TWColor.Create(250, 235, 215));
    m_pColorDict.Add('aqua',                 TWColor.Create(0,   255, 255));
    m_pColorDict.Add('aquamarine',           TWColor.Create(127, 255, 212));
    m_pColorDict.Add('azure',                TWColor.Create(240, 255, 255));
    m_pColorDict.Add('beige',                TWColor.Create(245, 245, 220));
    m_pColorDict.Add('bisque',               TWColor.Create(255, 228, 196));
    m_pColorDict.Add('black',                TWColor.Create(0,   0,   0));
    m_pColorDict.Add('blanchedalmond',       TWColor.Create(255, 235, 205));
    m_pColorDict.Add('blue',                 TWColor.Create(0,   0,   255));
    m_pColorDict.Add('blueviolet',           TWColor.Create(138, 43,  226));
    m_pColorDict.Add('brown',                TWColor.Create(165, 42,  42));
    m_pColorDict.Add('burlywood',            TWColor.Create(222, 184, 135));
    m_pColorDict.Add('cadetblue',            TWColor.Create(95,  158, 160));
    m_pColorDict.Add('chartreuse',           TWColor.Create(127, 255, 0));
    m_pColorDict.Add('chocolate',            TWColor.Create(210, 105, 30));
    m_pColorDict.Add('coral',                TWColor.Create(255, 127, 80));
    m_pColorDict.Add('cornflowerblue',       TWColor.Create(100, 149, 237));
    m_pColorDict.Add('cornsilk',             TWColor.Create(255, 248, 220));
    m_pColorDict.Add('crimson',              TWColor.Create(220, 20,  60));
    m_pColorDict.Add('cyan',                 TWColor.Create(0,   255, 255));
    m_pColorDict.Add('darkblue',             TWColor.Create(0,   0,   139));
    m_pColorDict.Add('darkcyan',             TWColor.Create(0,   139, 139));
    m_pColorDict.Add('darkgoldenrod',        TWColor.Create(184, 134, 11));
    m_pColorDict.Add('darkgray',             TWColor.Create(169, 169, 169));
    m_pColorDict.Add('darkgreen',            TWColor.Create(0,   100, 0));
    m_pColorDict.Add('darkgrey',             TWColor.Create(169, 169, 169));
    m_pColorDict.Add('darkkhaki',            TWColor.Create(189, 183, 107));
    m_pColorDict.Add('darkmagenta',          TWColor.Create(139, 0,   139));
    m_pColorDict.Add('darkolivegreen',       TWColor.Create(85,  107, 47));
    m_pColorDict.Add('darkorange',           TWColor.Create(255, 140, 0));
    m_pColorDict.Add('darkorchid',           TWColor.Create(153, 50,  204));
    m_pColorDict.Add('darkred',              TWColor.Create(139, 0,   0));
    m_pColorDict.Add('darksalmon',           TWColor.Create(233, 150, 122));
    m_pColorDict.Add('darkseagreen',         TWColor.Create(143, 188, 143));
    m_pColorDict.Add('darkslateblue',        TWColor.Create(72,  61,  139));
    m_pColorDict.Add('darkslategray',        TWColor.Create(47,  79,  79));
    m_pColorDict.Add('darkslategrey',        TWColor.Create(47,  79,  79));
    m_pColorDict.Add('darkturquoise',        TWColor.Create(0,   206, 209));
    m_pColorDict.Add('darkviolet',           TWColor.Create(148, 0,   211));
    m_pColorDict.Add('deeppink',             TWColor.Create(255, 20,  147));
    m_pColorDict.Add('deepskyblue',          TWColor.Create(0,   191, 255));
    m_pColorDict.Add('dimgray',              TWColor.Create(105, 105, 105));
    m_pColorDict.Add('dimgrey',              TWColor.Create(105, 105, 105));
    m_pColorDict.Add('dodgerblue',           TWColor.Create(30,  144, 255));
    m_pColorDict.Add('firebrick',            TWColor.Create(178, 34,  34));
    m_pColorDict.Add('floralwhite',          TWColor.Create(255, 250, 240));
    m_pColorDict.Add('forestgreen',          TWColor.Create(34,  139, 34));
    m_pColorDict.Add('fuchsia',              TWColor.Create(255, 0,   255));
    m_pColorDict.Add('gainsboro',            TWColor.Create(220, 220, 220));
    m_pColorDict.Add('ghostwhite',           TWColor.Create(248, 248, 255));
    m_pColorDict.Add('gold',                 TWColor.Create(255, 215, 0));
    m_pColorDict.Add('goldenrod',            TWColor.Create(218, 165, 32));
    m_pColorDict.Add('gray',                 TWColor.Create(128, 128, 128));
    m_pColorDict.Add('grey',                 TWColor.Create(128, 128, 128));
    m_pColorDict.Add('green',                TWColor.Create(0,   128, 0));
    m_pColorDict.Add('greenyellow',          TWColor.Create(173, 255, 47));
    m_pColorDict.Add('honeydew',             TWColor.Create(240, 255, 240));
    m_pColorDict.Add('hotpink',              TWColor.Create(255, 105, 180));
    m_pColorDict.Add('indianred',            TWColor.Create(205, 92,  92));
    m_pColorDict.Add('indigo',               TWColor.Create(75,  0,   130));
    m_pColorDict.Add('ivory',                TWColor.Create(255, 255, 240));
    m_pColorDict.Add('khaki',                TWColor.Create(240, 230, 140));
    m_pColorDict.Add('lavender',             TWColor.Create(230, 230, 250));
    m_pColorDict.Add('lavenderblush',        TWColor.Create(255, 240, 245));
    m_pColorDict.Add('lawngreen',            TWColor.Create(124, 252, 0));
    m_pColorDict.Add('lemonchiffon',         TWColor.Create(255, 250, 205));
    m_pColorDict.Add('lightblue',            TWColor.Create(173, 216, 230));
    m_pColorDict.Add('lightcoral',           TWColor.Create(240, 128, 128));
    m_pColorDict.Add('lightcyan',            TWColor.Create(224, 255, 255));
    m_pColorDict.Add('lightgoldenrodyellow', TWColor.Create(250, 250, 210));
    m_pColorDict.Add('lightgray',            TWColor.Create(211, 211, 211));
    m_pColorDict.Add('lightgreen',           TWColor.Create(144, 238, 144));
    m_pColorDict.Add('lightgrey',            TWColor.Create(211, 211, 211));
    m_pColorDict.Add('lightpink',            TWColor.Create(255, 182, 193));
    m_pColorDict.Add('lightsalmon',          TWColor.Create(255, 160, 122));
    m_pColorDict.Add('lightseagreen',        TWColor.Create(32,  178, 170));
    m_pColorDict.Add('lightskyblue',         TWColor.Create(135, 206, 250));
    m_pColorDict.Add('lightslategray',       TWColor.Create(119, 136, 153));
    m_pColorDict.Add('lightslategrey',       TWColor.Create(119, 136, 153));
    m_pColorDict.Add('lightsteelblue',       TWColor.Create(176, 196, 222));
    m_pColorDict.Add('lightyellow',          TWColor.Create(255, 255, 224));
    m_pColorDict.Add('lime',                 TWColor.Create(0,   255, 0));
    m_pColorDict.Add('limegreen',            TWColor.Create(50,  205, 50));
    m_pColorDict.Add('linen',                TWColor.Create(250, 240, 230));
    m_pColorDict.Add('magenta',              TWColor.Create(255, 0,   255));
    m_pColorDict.Add('maroon',               TWColor.Create(128, 0,   0));
    m_pColorDict.Add('mediumaquamarine',     TWColor.Create(102, 205, 170));
    m_pColorDict.Add('mediumblue',           TWColor.Create(0,   0,   205));
    m_pColorDict.Add('mediumorchid',         TWColor.Create(186, 85,  211));
    m_pColorDict.Add('mediumpurple',         TWColor.Create(147, 112, 219));
    m_pColorDict.Add('mediumseagreen',       TWColor.Create(60,  179, 113));
    m_pColorDict.Add('mediumslateblue',      TWColor.Create(123, 104, 238));
    m_pColorDict.Add('mediumspringgreen',    TWColor.Create(0,   250, 154));
    m_pColorDict.Add('mediumturquoise',      TWColor.Create(72,  209, 204));
    m_pColorDict.Add('mediumvioletred',      TWColor.Create(199, 21,  133));
    m_pColorDict.Add('midnightblue',         TWColor.Create(25,  25,  112));
    m_pColorDict.Add('mintcream',            TWColor.Create(245, 255, 250));
    m_pColorDict.Add('mistyrose',            TWColor.Create(255, 228, 225));
    m_pColorDict.Add('moccasin',             TWColor.Create(255, 228, 181));
    m_pColorDict.Add('navajowhite',          TWColor.Create(255, 222, 173));
    m_pColorDict.Add('navy',                 TWColor.Create(0,   0,   128));
    m_pColorDict.Add('oldlace',              TWColor.Create(253, 245, 230));
    m_pColorDict.Add('olive',                TWColor.Create(128, 128, 0));
    m_pColorDict.Add('olivedrab',            TWColor.Create(107, 142, 35));
    m_pColorDict.Add('orange',               TWColor.Create(255, 165, 0));
    m_pColorDict.Add('orangered',            TWColor.Create(255, 69,  0));
    m_pColorDict.Add('orchid',               TWColor.Create(218, 112, 214));
    m_pColorDict.Add('palegoldenrod',        TWColor.Create(238, 232, 170));
    m_pColorDict.Add('palegreen',            TWColor.Create(152, 251, 152));
    m_pColorDict.Add('paleturquoise',        TWColor.Create(175, 238, 238));
    m_pColorDict.Add('palevioletred',        TWColor.Create(219, 112, 147));
    m_pColorDict.Add('papayawhip',           TWColor.Create(255, 239, 213));
    m_pColorDict.Add('peachpuff',            TWColor.Create(255, 218, 185));
    m_pColorDict.Add('peru',                 TWColor.Create(205, 133, 63));
    m_pColorDict.Add('pink',                 TWColor.Create(255, 192, 203));
    m_pColorDict.Add('plum',                 TWColor.Create(221, 160, 221));
    m_pColorDict.Add('powderblue',           TWColor.Create(176, 224, 230));
    m_pColorDict.Add('purple',               TWColor.Create(128, 0,   128));
    m_pColorDict.Add('red',                  TWColor.Create(255, 0,   0));
    m_pColorDict.Add('rosybrown',            TWColor.Create(188, 143, 143));
    m_pColorDict.Add('royalblue',            TWColor.Create(65,  105, 225));
    m_pColorDict.Add('saddlebrown',          TWColor.Create(139, 69,  19));
    m_pColorDict.Add('salmon',               TWColor.Create(250, 128, 114));
    m_pColorDict.Add('sandybrown',           TWColor.Create(244, 164, 96));
    m_pColorDict.Add('seagreen',             TWColor.Create(46,  139, 87));
    m_pColorDict.Add('seashell',             TWColor.Create(255, 245, 238));
    m_pColorDict.Add('sienna',               TWColor.Create(160, 82,  45));
    m_pColorDict.Add('silver',               TWColor.Create(192, 192, 192));
    m_pColorDict.Add('skyblue',              TWColor.Create(135, 206, 235));
    m_pColorDict.Add('slateblue',            TWColor.Create(106, 90,  205));
    m_pColorDict.Add('slategray',            TWColor.Create(112, 128, 144));
    m_pColorDict.Add('slategrey',            TWColor.Create(112, 128, 144));
    m_pColorDict.Add('snow',                 TWColor.Create(255, 250, 250));
    m_pColorDict.Add('springgreen',          TWColor.Create(0,   255, 127));
    m_pColorDict.Add('steelblue',            TWColor.Create(70,  130, 180));
    m_pColorDict.Add('tan',                  TWColor.Create(210, 180, 140));
    m_pColorDict.Add('teal',                 TWColor.Create(0,   128, 128));
    m_pColorDict.Add('thistle',              TWColor.Create(216, 191, 216));
    m_pColorDict.Add('tomato',               TWColor.Create(255, 99,  71));
    m_pColorDict.Add('turquoise',            TWColor.Create(64,  224, 208));
    m_pColorDict.Add('violet',               TWColor.Create(238, 130, 238));
    m_pColorDict.Add('wheat',                TWColor.Create(245, 222, 179));
    m_pColorDict.Add('white',                TWColor.Create(255, 255, 255));
    m_pColorDict.Add('whitesmoke',           TWColor.Create(245, 245, 245));
    m_pColorDict.Add('yellow',               TWColor.Create(255, 255, 0));
    m_pColorDict.Add('yellowgreen',          TWColor.Create(154, 205, 50));
end;
//--------------------------------------------------------------------------------------------------
class function TWStandardColor.GetInstance: IWStandardColor;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
        // get it
        Exit(m_pInstance);

    // create new singleton instance
    m_pInstance := TWStandardColor.Create;
    Result      := m_pInstance;
end;
//---------------------------------------------------------------------------
function TWStandardColor.Exists(const name: UnicodeString): Boolean;
begin
    // check if color exists in dictionary
    Result := m_pColorDict.ContainsKey(name);
end;
//---------------------------------------------------------------------------
function TWStandardColor.Get(const name: UnicodeString; out color: TWColor): Boolean;
begin
    // search for color
    if (not m_pColorDict.TryGetValue(name, color)) then
    begin
        color := TWColor.GetDefault;
        Exit(False);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------

end.
