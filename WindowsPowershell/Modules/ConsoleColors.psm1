
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

            // This appears to correct a bug in the implementation of
            // SetConsoleScreenBufferInfoEx.
            // 
            info.srWindow.Right++;
            info.srWindow.Bottom++;

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
            internal ushort Left;
            internal ushort Top;
            internal ushort Right;
            internal ushort Bottom;
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
            internal short wAttributes;
            internal SMALL_RECT srWindow;
            internal COORD dwMaximumWindowSize;
            internal short wPopupAttributes;

            [MarshalAs( UnmanagedType.Bool )]
            internal bool bFullscreenSupported;

            [MarshalAs( UnmanagedType.ByValArray, SizeConst = 16 )]
            internal COLORREF[] ColorTable;
        }
    }
"@

    Add-Type -TypeDefinition $code -ReferencedAssemblies @('System.Drawing')
}

function Get-SolarizedColorTable()
{
    # This table is mostly from http://ethanschoonover.com/solarized, with the console color mapping proposed here:
    # https://github.com/neilpa/cmd-colors-solarized
    #
    $source = @"
SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB         ConsoleColor
--------- ------- ---- -------  ----------- ---------- ----------- ----------- ------------
base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21 DarkGray
base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26 Black
base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46 DarkGreen
base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51 DarkYellow
base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59 DarkBlue
base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63 DarkCyan
base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93 Gray
base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99 White
yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71 Yellow
orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80 DarkRed
red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86 Red
magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83 Magenta
violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77 DarkMagenta
blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82 Blue
cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63 Cyan
green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60 Green
"@

    $table = @{}
    $lines = $source -split "`n"
    for($i = 2; $i -lt $lines.Length; $i++)
    {
        $tok = $lines[$i].Split(@(' '), "RemoveEmptyEntries")
        $val = New-Object PSObject -Property (@{
          "Solarized"=$tok[0];
          "Hex"=$tok[1];
          "TermCol"=$tok[3];
          "XTerm"=$tok[4];
          "XTermHex"=$tok[5];
          "L"=[int]$tok[6];
          "A"=[int]$tok[7];
          "B"=[int]$tok[8];
          "Red"=[int]$tok[9];
          "Green"=[int]$tok[10];
          "Blue"=[int]$tok[11];
          "Hue"=[int]$tok[12];
          "Saturation"=[int]$tok[13];
          "Black"=[int]$tok[14];
          "ConsoleColor"=[ConsoleColor]($tok[15])
        })

        $table[$val.Solarized]=$val
    }
    return $table
}

$SolarizedTable = Get-SolarizedColorTable

function ConvertFrom-SolarizedColor($solarized)
{
    return $SolarizedTable[$solarized].ConsoleColor
}

function Set-SolarizedColors([switch]$Light, [switch]$Dark)
{
    if ( (-not $Light) -and (-not $Dark) )
    {
        $Light = $true
    }

    if ($Host.Name -eq "ConsoleHost")
    {
        Add-Type -AssemblyName System.Drawing
        Add-ConsoleHelper

        # Redefine the standard console colors to have the Solarized palette.  
        #
        $table = Get-SolarizedColorTable
        foreach( $entry in $table.Values )
        {
            [ConsoleColorHelper]::SetStandardConsoleColor($entry.ConsoleColor, $entry.Hex)
        }

        if ($Light)
        {
            $Host.UI.RawUI.BackgroundColor            = ConvertFrom-SolarizedColor "base3"
            $Host.UI.RawUI.ForegroundColor            = ConvertFrom-SolarizedColor "base00"

            $Host.PrivateData.WarningForegroundColor  = ConvertFrom-SolarizedColor "base1"
            $Host.PrivateData.VerboseForegroundColor  = ConvertFrom-SolarizedColor "base01"
            $Host.PrivateData.DebugForegroundColor    = ConvertFrom-SolarizedColor "base01"

            $Host.PrivateData.ErrorBackgroundColor    = ConvertFrom-SolarizedColor "base2"
            $Host.PrivateData.WarningBackgroundColor  = ConvertFrom-SolarizedColor "base2"
        }
        else
        {
            $Host.UI.RawUI.BackgroundColor            = ConvertFrom-SolarizedColor "base03"
            $Host.UI.RawUI.ForegroundColor            = ConvertFrom-SolarizedColor "base0"

            $Host.PrivateData.WarningForegroundColor  = ConvertFrom-SolarizedColor "base01"
            $Host.PrivateData.VerboseForegroundColor  = ConvertFrom-SolarizedColor "base1"
            $Host.PrivateData.DebugForegroundColor    = ConvertFrom-SolarizedColor "base1"

            $Host.PrivateData.ErrorBackgroundColor    = ConvertFrom-SolarizedColor "base02"
            $Host.PrivateData.WarningBackgroundColor  = ConvertFrom-SolarizedColor "base02"
        }

        $Host.PrivateData.ErrorForegroundColor    = ConvertFrom-SolarizedColor "red"
        
        $Host.PrivateData.DebugBackgroundColor    =
        $Host.PrivateData.VerboseBackgroundColor  =
          $Host.UI.RawUI.BackgroundColor
    }
}
