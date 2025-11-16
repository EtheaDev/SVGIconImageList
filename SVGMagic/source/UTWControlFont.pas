{**
 @abstract(@name provides the classes and functions to add new custom fonts to the Windows session,
           or remove previously added custom fonts.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWControlFont;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     Vcl.Forms,
     Winapi.Messages,
     Winapi.Windows,
     UTWSmartPointer;

type
    // todo -cFeature -oJean: in order to register/unregister fonts in Windows session correctly,
    //                        this class should support threading, and each application should add a
    //                        lock when a local font is used. Session fonts should be deleted only
    //                        when all locks are removed
    {**
     Font helper, contains tools to manipulate fonts, add or remove fonts in memory or a Windows
     session, get font name from a TrueType file, ...
    }
    TWControlFont = class
        private type
            ITfGetFontResourceInfoW = function(name: PWideChar; var bufSize: Cardinal;
                    pBuffer: Pointer; infoType: Cardinal): LongBool; stdcall;

            ISessionCustomFonts = TDictionary<UnicodeString, UnicodeString>;
            ICustomFonts        = TDictionary<UnicodeString, THandle>;

        private
            class var m_pSessionCustomFonts:   ISessionCustomFonts;
            class var m_pCustomFonts:          ICustomFonts;
            class var m_hGetFontResourceInfoW: ITfGetFontResourceInfoW;

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
             Get font name from truetype font file (.ttf)
             @param(fileName File name)
             @returns(Font name)
            }
            class function GetFontNameFromFile(const fileName: UnicodeString): UnicodeString; static;

            {**
             Add new truetype font from file and make it available for application
             @param(name Font name)
             @param(fileName Font file name)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Thus added font will be available for ALL applications until the Windows
                             session is closed or the font is removed using RemoveFontFromSession or
                             RemoveFontsFromSession
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            class function AddFontToSession(const name, fileName: UnicodeString): Boolean; static;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(fileName Font file name)
             @returns(Font handle, @nil on error)
            }
            class function AddFont(const name, fileName: UnicodeString): THandle; overload; static;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(pStream Stream containing font data)
             @param(fontLength Font data length)
             @returns(Font handle, @nil on error)
            }
            class function AddFont(const name: UnicodeString; pStream: TStream; fontLength: Cardinal):
                    THandle; overload; static;

            {**
             Remove previously added font from Windows session
             @param(name Font name)
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            class procedure RemoveFontFromSession(const name: UnicodeString); static;

            {**
             Remove all previously added fonts from Windows session
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            class procedure RemoveFontsFromSession; static;

            {**
             Remove previously added font
             @param(name Font name)
            }
            class procedure RemoveFont(const name: UnicodeString); static;

            {**
             Remove all previously added fonts
            }
            class procedure RemoveFonts; static;

            {**
             Check if font exists in opened Windows session
             @param(name Font name)
             @returns(@true if font exists, otherwise @false)
            }
            class function FontExistsInSession(const name: UnicodeString): Boolean; static;

            {**
             Check if font exists
             @param(name Font name)
             @returns(@true if font exists, otherwise @false)
            }
            class function FontExists(const name: UnicodeString): Boolean; static;

            {**
             Get font
             @param(name Font name)
             @returns(font, @nil if not found)
             @br @bold(NOTE) Only font previously added with AddFont() can be get. Standard or
                             custom font belonging to Windows session are not included
            }
            class function GetFont(const name: UnicodeString): THandle; static;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWControlFont.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWControlFont.Destroy;
begin
    RemoveFonts;

    // as wished by Niki, fonts added in session are removed when application ends, even if other
    // apps can always use them (I informed Niki about this possible behavior)
    RemoveFontsFromSession;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
class function TWControlFont.GetFontNameFromFile(const fileName: UnicodeString): UnicodeString;
var
    fontFileName:  UnicodeString;
    addFontRes, i: Integer;
    logFont:       array of TLogFontW;
    lfsz:          Cardinal;
    hFnt:          HFONT;
begin
    if (not Assigned(m_hGetFontResourceInfoW)) then
    begin
        m_hGetFontResourceInfoW := GetProcAddress(GetModuleHandle('gdi32.dll'), 'GetFontResourceInfoW');

        if (not Assigned(m_hGetFontResourceInfoW)) then
            raise Exception.Create('GetFontResourceInfoW in gdi32.dll not found');
    end;

    fontFileName := fileName;

    if (LowerCase(ExtractFileExt(fontFileName)) = '.pfm') then
        fontFileName := fontFileName + '|' + ChangeFileExt(fontFileName, '.pfb');

    addFontRes := AddFontResourceW(PWideChar(fontFileName));

    try
        if (addFontRes > 0) then
        begin
            SetLength(logFont, addFontRes);
            lfsz := addFontRes * SizeOf(TLogFontW);

            if (not m_hGetFontResourceInfoW(PWideChar(fontFileName), lfsz, @logFont[0], 2)) then
                raise Exception.Create('GetFontResourceInfoW failed');

            addFontRes := lfsz div SizeOf(TLogFont);

            for i := 0 to addFontRes - 1 do
            begin
                hFnt := CreateFontIndirectW(logFont[i]);

                try
                    Result := logFont[i].lfFaceName;
                finally
                    DeleteObject(hFnt);
                end;
            end;
        end;
    finally
        RemoveFontResourceW(PWideChar(fontFileName));
    end;
end;
//---------------------------------------------------------------------------
class function TWControlFont.AddFontToSession(const name, fileName: UnicodeString): Boolean;
begin
    // no font name?
    if (Length(name) = 0) then
        Exit(False);

    // font already exists?
    if (FontExistsInSession(name)) then
        Exit(True);

    // add font in system
    if (AddFontResourceW(PWideChar(fileName)) = 0) then
        Exit(False);

    // add font in registered session custom list
    m_pSessionCustomFonts[name] := fileName;

    // notify all other applications and objects that font changed
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWControlFont.AddFont(const name, fileName: UnicodeString): THandle;
var
    pStream: IWSmartPointer<TMemoryStream>;
begin
    // load font file to stream and add font
    pStream := TWSmartPointer<TMemoryStream>.Create();
    pStream.LoadFromFile(fileName);
    pStream.Position := 0;
    Result := AddFont(name, pStream, pStream.Size);
end;
//---------------------------------------------------------------------------
class function TWControlFont.AddFont(const name: UnicodeString; pStream: TStream; fontLength: Cardinal): THandle;
var
    fontCount: DWORD;
    buffer:    array of AnsiChar;
begin
    // no font name?
    if (Length(name) = 0) then
        Exit(0);

    // no stream?
    if (not Assigned(pStream)) then
        Exit(0);

    // no length?
    if (fontLength = 0) then
        Exit(0);

    // get existing font
    Result := GetFont(name);

    // found it?
    if (Result <> 0) then
        Exit;

    try
        // read font data content from stream
        SetLength(buffer, fontLength);
        pStream.Read(buffer[0], fontLength * SizeOf(AnsiChar));

        // add font in system
        Result := AddFontMemResourceEx(PAnsiChar(buffer), fontLength, nil, @fontCount);
    finally
        SetLength(buffer, 0);
    end;

    // add font in custom registered list
    if (Result <> 0) then
        m_pCustomFonts[name] := Result;
end;
//---------------------------------------------------------------------------
class procedure TWControlFont.RemoveFontFromSession(const name: UnicodeString);
var
    value: UnicodeString;
begin
    // no font name?
    if (Length(name) = 0) then
        Exit;

    // search for registered session custom font
    if (not m_pSessionCustomFonts.TryGetValue(name, value)) then
        Exit;

    // remove font
    if (not RemoveFontResourceW(PWideChar(value))) then
        Exit;

    // remove font from list
    m_pSessionCustomFonts.Remove(name);

    // notify all other applications and objects that font changed
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
end;
//---------------------------------------------------------------------------
class procedure TWControlFont.RemoveFontsFromSession;
var
    fontChanged: Boolean;
    item:        TPair<UnicodeString, UnicodeString>;
begin
    fontChanged := False;

    // iterate through registered session custom fonts
    for item in m_pSessionCustomFonts do
        // remove font
        if (RemoveFontResourceW(PWideChar(item.Value))) then
            fontChanged := True;

    // at least 1 font was removed?
    if (fontChanged) then
        // notify all other applications and objects that font changed
        SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);

    // clear font container
    m_pSessionCustomFonts.Clear;
end;
//---------------------------------------------------------------------------
class procedure TWControlFont.RemoveFont(const name: UnicodeString);
var
    hFont: THandle;
begin
    // no font name?
    if (Length(name) = 0) then
        Exit;

    // search for registered custom font
    if (not m_pCustomFonts.TryGetValue(name, hFont)) then
        Exit;

    // remove font
    if (not RemoveFontMemResourceEx(hFont)) then
        Exit;

    // remove font from list
    m_pCustomFonts.Remove(name);
end;
//---------------------------------------------------------------------------
class procedure TWControlFont.RemoveFonts;
var
    item: TPair<UnicodeString, THandle>;
begin
    // iterate through registered custom fonts
    for item in m_pCustomFonts do
        // remove font
        RemoveFontMemResourceEx(item.Value);

    // clear font container
    m_pSessionCustomFonts.Clear;
end;
//---------------------------------------------------------------------------
class function TWControlFont.FontExistsInSession(const name: UnicodeString): Boolean;
begin
    // search for registered session custom font
    if (m_pSessionCustomFonts.ContainsKey(name)) then
        Result := True
    else
        // check if font exists in Windows session registered fonts and return result
        Result := (Screen.Fonts.IndexOf(name) <> -1);
end;
//---------------------------------------------------------------------------
class function TWControlFont.FontExists(const name: UnicodeString): Boolean;
begin
    // check if registered custom font exists in dictionary
    Result := m_pCustomFonts.ContainsKey(name);
end;
//---------------------------------------------------------------------------
class function TWControlFont.GetFont(const name: UnicodeString): THandle;
begin
    // search for registered custom font
    if (not m_pCustomFonts.TryGetValue(name, Result)) then
        Result := 0
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
// Global initialization procedure
//---------------------------------------------------------------------------
begin
    TWControlFont.m_pSessionCustomFonts   := TWControlFont.ISessionCustomFonts.Create;
    TWControlFont.m_pCustomFonts          := TWControlFont.ICustomFonts.Create;
    TWControlFont.m_hGetFontResourceInfoW := nil;
end;
//---------------------------------------------------------------------------

finalization
//---------------------------------------------------------------------------
// Global finalization procedure
//---------------------------------------------------------------------------
begin
    TWControlFont.m_pSessionCustomFonts.Free;
    TWControlFont.m_pCustomFonts.Free;
end;
//---------------------------------------------------------------------------

end.
