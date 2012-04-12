# Check to see if we're interactive or not.... this is a dirty, dirty
# hack.
$interactive = $true
$cmdLine = [System.Environment]::CommandLine
if ($cmdLine.Contains("powershell.exe") -and ($cmdLine.Contains(".ps1") -and !$cmdLine.Contains("-noexit")))
{
    # We're running non-interactively, don't do any of this stuff.
    $interactive = $false
}

if ($interactive)
{
    # Make MSYS based things work correctly (most importantly, msysgit)
    #
    $env:TERM='msys'
    
    # Load up the colors and set the dark solarized theme.
    #
    $profileDir = split-path -parent $Profile
    Import-Module "$profileDir\Modules\ConsoleColors.psm1"
    # Set-SolarizedColors -Dark

    # Ensure HOME is set properly
    #
    $env:Home = [Environment]::GetFolderPath("Personal")
    set-variable -name HOME -value (resolve-path $env:Home) -force
    (get-psprovider FileSystem).Home = $HOME
    
    # Add the right things to the path
    #
    $env:Path = "$env:windir\Microsoft.NET\Framework64;" + $env:Path
    $env:Path = "c:\debuggers;c:\tools\x86;c:\tools\x86\bin;c:\emacs\bin" + $env:Path
}

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

$global:SolarizedColors = $false

function prompt 
{ 
    $ok = $?

    if (!$global:SolarizedColors)
    {
        Set-SolarizedColors -Dark
        $global:SolarizedColors = $true
    }

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
