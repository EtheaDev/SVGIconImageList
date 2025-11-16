{**
 @abstract(@name provides a generic padding structure.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWPadding;

interface

type
    {**
     Platform independent padding structure
    }
    TWPadding<T> = record
        private
            m_Left:   T;
            m_Top:    T;
            m_Right:  T;
            m_Bottom: T;

        public
            {**
             Constructor
             @param(left Left coordinate)
             @param(top Top coordinate)
             @param(right Right coordinate)
             @param(bottom Bottom coordinate)
            }
            constructor Create(const left, top, right, bottom: T);

        // Properties
        public
            {**
             Gets or sets the padding left edge value
            }
            property Left: T read m_Left write m_Left;

            {**
             Gets or sets the padding top edge value
            }
            property Top: T read m_Top write m_Top;

            {**
             Gets or sets the padding right edge value
            }
            property Right: T read m_Right write m_Right;

            {**
             Gets or sets the padding bottom edge value
            }
            property Bottom: T read m_Bottom write m_Bottom;
    end;

    TWPaddingI = TWPadding<Integer>;
    TWPaddingF = TWPadding<Single>;

    PWPaddingI = ^TWPaddingI;
    PWPaddingF = ^TWPaddingF;

implementation
//---------------------------------------------------------------------------
// WTPadding implementation
//---------------------------------------------------------------------------
constructor TWPadding<T>.Create(const left, top, right, bottom: T);
begin
    m_Left   := left;
    m_Top    := top;
    m_Right  := right;
    m_Bottom := bottom;
end;
//---------------------------------------------------------------------------

end.
