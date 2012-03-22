
function Add-ConsoleHelper()
{
    $code = @"
    using System;
    using System.Drawing;
    using System.Runtime.InteropServices;

    public static class ConsoleColorHelper
    {
        [DllImport( "kernel32.dll", SetLastError = true )]
        static extern bool SetConsoleScreenBufferInfoEx(
            IntPtr ConsoleOutput,
            ref CONSOLE_SCREEN_BUFFER_INFO_EX ConsoleScreenBufferInfoEx
            );

        [DllImport( "kernel32.dll", SetLastError = true )]
        static extern bool GetConsoleScreenBufferInfoEx(
            IntPtr hConsoleOutput,
            [In, Out] ref CONSOLE_SCREEN_BUFFER_INFO_EX ConsoleScreenBufferInfo
            );

        [DllImport( "kernel32.dll", SetLastError = true )]
        static extern IntPtr GetStdHandle(
            int nStdHandle
            );

        public static Color GetStandardConsoleColor( ConsoleColor color )
        {
            IntPtr hConsole = GetStdHandle( -11 );

            CONSOLE_SCREEN_BUFFER_INFO_EX info = new CONSOLE_SCREEN_BUFFER_INFO_EX();
            info.cbSize = Marshal.SizeOf( typeof( CONSOLE_SCREEN_BUFFER_INFO_EX ) );

            if ( !GetConsoleScreenBufferInfoEx( hConsole, ref info ) )
            {
                throw Marshal.GetExceptionForHR( Marshal.GetHRForLastWin32Error() );
            }

            return info.ColorTable[(int)color].GetColor();
        }

        public static void SetStandardConsoleColor( ConsoleColor color, Color newColor )
        {
            IntPtr hConsole = GetStdHandle( -11 );

            CONSOLE_SCREEN_BUFFER_INFO_EX info = new CONSOLE_SCREEN_BUFFER_INFO_EX();
            info.cbSize = Marshal.SizeOf( typeof( CONSOLE_SCREEN_BUFFER_INFO_EX ) );

            if ( !GetConsoleScreenBufferInfoEx( hConsole, ref info ) )
            {
                throw Marshal.GetExceptionForHR( Marshal.GetHRForLastWin32Error() );
            }

            info.ColorTable[(int)color] = new COLORREF( newColor );
            if ( !SetConsoleScreenBufferInfoEx( hConsole, ref info ) )
            {
                throw Marshal.GetExceptionForHR( Marshal.GetHRForLastWin32Error() );
            }
        }

        [StructLayout( LayoutKind.Sequential )]
        internal struct COORD
        {
            internal short X;
            internal short Y;
        }

        [StructLayout( LayoutKind.Sequential )]
        internal struct SMALL_RECT
        {
            internal short Left;
            internal short Top;
            internal short Right;
            internal short Bottom;
        }

        [StructLayout( LayoutKind.Sequential )]
        internal struct COLORREF
        {
            internal uint ColorDWORD;

            internal COLORREF( Color color )
            {
                ColorDWORD = (uint)color.R + ( ( (uint)color.G ) << 8 ) + ( ( (uint)color.B ) << 16 );
            }

            internal COLORREF( uint r, uint g, uint b )
            {
                ColorDWORD = r + ( g << 8 ) + ( b << 16 );
            }

            internal Color GetColor()
            {
                return Color.FromArgb( (int)( 0x000000FFU & ColorDWORD ),
                   (int)( 0x0000FF00U & ColorDWORD ) >> 8, (int)( 0x00FF0000U & ColorDWORD ) >> 16 );
            }

            internal void SetColor( Color color )
            {
                ColorDWORD = (uint)color.R + ( ( (uint)color.G ) << 8 ) + ( ( (uint)color.B ) << 16 );
            }
        }

        [StructLayout( LayoutKind.Sequential )]
        internal struct CONSOLE_SCREEN_BUFFER_INFO_EX
        {
            internal int cbSize;
            internal COORD dwSize;
            internal COORD dwCursorPosition;
            internal ushort wAttributes;
            internal SMALL_RECT srWindow;
            internal COORD dwMaximumWindowSize;
            internal ushort wPopupAttributes;

            [MarshalAs( UnmanagedType.Bool )]
            internal bool bFullscreenSupported;

            [MarshalAs( UnmanagedType.ByValArray, SizeConst = 16 )]
            internal COLORREF[] ColorTable;
        }
    }
"@

    Add-Type -TypeDefinition $code -ReferencedAssemblies @('System.Drawing')
}

# These are the 16 colors for our palette.
Add-Type -AssemblyName System.Drawing
$base03 = [System.Drawing.Color]::FromArgb(0x002b36)
$base02 = [System.Drawing.Color]::FromArgb(0x073642)
$base01 = [System.Drawing.Color]::FromArgb(0x586e75)
$base00 = [System.Drawing.Color]::FromArgb(0x657b83)
$base0 = [System.Drawing.Color]::FromArgb(0x839496)
$base1 = [System.Drawing.Color]::FromArgb(0x93a1a1)
$base2 = [System.Drawing.Color]::FromArgb(0xeee8d5)
$base3 = [System.Drawing.Color]::FromArgb(0xfdf6e3)
$yellow = [System.Drawing.Color]::FromArgb(0xb58900)
$orange = [System.Drawing.Color]::FromArgb(0xcb4b16)
$red = [System.Drawing.Color]::FromArgb(0xdc322f)
$magenta = [System.Drawing.Color]::FromArgb(0xd33682)
$violet = [System.Drawing.Color]::FromArgb(0x6c71c4)
$blue = [System.Drawing.Color]::FromArgb(0x268bd2)
$cyan = [System.Drawing.Color]::FromArgb(0x2aa198)
$green = [System.Drawing.Color]::FromArgb(0x859900)

function Set-SolarizedColors([switch]$Light, [switch]$Dark)
{
    # Redefine the standard console colors to have the Solarized-Light
    # palette.  
    #
    # Note that we're doing funny things here, like setting black to
    # white, because we are trying to make it so that programs that
    # don't redefine the palette still get a coherent scheme.
    #
    # We try to map with cognates, but it's not easy....
    #
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Black,       $base03)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkGray,    $base02)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Gray,        $base01)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkBlue,    $base00)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkGreen,   $base0)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkCyan,    $base1)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkRed,     $base2)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::White,       $base3)

    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Yellow,      $yellow)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkYellow,  $orange)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Red,         $red)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Magenta,     $magenta)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::DarkMagenta, $violet)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Blue,        $blue)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Cyan,        $cyan)
    [ConsoleColorHelper]::SetStandardConsoleColor([ConsoleColor]::Green,       $green)

    if ($Light)
    {
        $themefg = [ConsoleColor]::White    # $base3
        $themebg = [ConsoleColor]::DarkBlue # $base00
    }
    
    if ($Dark)
    {
        $themefg = [ConsoleColor]::DarkGreen # $base0
        $themebg = [ConsoleColor]::Black     # $base03
    }

    if ($Light -or $Dark)
    {
        (Get-Host).UI.RawUI.BackgroundColor = $themebg
        (Get-Host).UI.RawUI.ForegroundColor = $themefg
    }
}

Add-ConsoleHelper