# These bits of profile courtesy of http://winterdom.com/
#
# http://winterdom.com/2008/01/modifyingthehomeinpowershell
# Set the $HOME variable for our use
# and make powershell recognize ~\ as $HOME
# in paths
#
set-variable -name HOME -value (resolve-path $env:Home) -force
(get-psprovider FileSystem).Home = $HOME

# http://winterdom.com/2008/08/mypowershellprompt
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
    # our theme
    $cdelim = [ConsoleColor]::DarkCyan 
    $chost = [ConsoleColor]::Green 
    $cloc = [ConsoleColor]::Cyan 
    
    write-host "$([char]0x0A7) " -n -f $cloc
    write-host ([net.dns]::GetHostName()) -n -f $chost
    write-host ' {' -n -f $cdelim
    write-host (shorten-path (pwd).Path) -n -f $cloc
    write-host '}' -n -f $cdelim
    return ' '
}

function Get-JDHelp(
    [switch] $Full,
[switch] $Detailed
    
)
{
}