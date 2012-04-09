# Make MSYS based things work correctly (most importantly, msysgit)
#
$env:TERM='msys'

# Load up the colors and set the dark solarized theme.
#
$profileDir = split-path -parent $Profile
Import-Module "$profileDir\Modules\ConsoleColors.psm1"
#Import-Module "$profileDir\Modules\PowerSolarized.dll"
Set-SolarizedColors -Dark

# These bits of profile courtesy of http://winterdom.com/
#
# http://winterdom.com/2008/01/modifyingthehomeinpowershell
# Set the $HOME variable for our use
# and make powershell recognize ~\ as $HOME
# in paths
#
set-variable -name HOME -value (resolve-path $env:Home) -force
(get-psprovider FileSystem).Home = $HOME

# Based on http://winterdom.com/2008/08/mypowershellprompt
function shorten-path([string] $path) 
{
    $loc = $path.Replace($HOME, '~') 
    # remove prefix for UNC paths
    $loc = $loc -replace '^[^:]+::', ''
    # make path shorter like tabs in Vim,
    # handle paths starting with \\ and . correctly
    return ($loc -replace '\\(\.?)([^\\]{3})[^\\]*(?=\\)','\$1$2')
}

function prompt 
{ 
    $ok = $?

    # Our "theme", as it were. Note that we assume the use of the
    # solarized colors.
    #
    $cdelim = [ConsoleColor]::DarkCyan 
    $chost = [ConsoleColor]::DarkGreen
    $cloc = $csym = [ConsoleColor]::DarkCyan
    if (-not $ok) { $csym = [ConsoleColor]::DarkRed; }
    
    write-host "$([char]0x0A7) " -n -f $csym
    write-host ([net.dns]::GetHostName()) -n -f $chost
    write-host ' {' -n -f $cdelim
    write-host (shorten-path (pwd).Path) -n -f $cloc
    write-host '}' -n -f $cdelim

    return ' '
}
