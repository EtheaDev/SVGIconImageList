{**
 @abstract(@name provides an utility class to count the number of times a cache was hit and to log
           the stats in an human readable style.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWCacheHit;

interface

uses System.SysUtils,
     Winapi.Windows;

type
    {**
     Count cache hit and miss, and log result in compiler event logs on destruction
    }
    TWCacheHit = class
        private
            m_Name: UnicodeString;
            m_Hit:  NativeUInt;
            m_Miss: NativeUInt;

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
             Log counters
            }
            procedure Log; virtual;

            {**
             Gets or sets the cache name
            }
            property Name: UnicodeString read m_Name write m_Name;

            {**
             Gets or sets the cache hit count
            }
            property Hit: NativeUInt read m_Hit write m_Hit;

            {**
             Gets or sets the cache miss count
            }
            property Miss: NativeUInt read m_Miss write m_Miss;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWCacheHit.Create;
begin
    inherited Create;

    m_Hit  := 0;
    m_Miss := 0;
end;
//---------------------------------------------------------------------------
destructor TWCacheHit.Destroy;
begin
    Log;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWCacheHit.Log;
begin
    OutputDebugString(PWideChar(m_Name + ' - hit - ' + IntToStr(m_Hit) + ' - miss - ' + IntToStr(m_Miss)));
end;
//---------------------------------------------------------------------------

end.
