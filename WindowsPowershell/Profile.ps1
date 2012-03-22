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
    # our theme
    $ok = $?

    $cdelim = [ConsoleColor]::DarkCyan 
    $chost = [ConsoleColor]::Green 
    $cloc = $csym = [ConsoleColor]::Cyan 
    if (-not $ok) { $csym = [ConsoleColor]::Red; }
    
    write-host "$([char]0x0A7) " -n -f $csym
    write-host ([net.dns]::GetHostName()) -n -f $chost
    write-host ' {' -n -f $cdelim
    write-host (shorten-path (pwd).Path) -n -f $cloc
    write-host '}' -n -f $cdelim

    return ' '
}

function Get-TfsWorkspace(
    [string] $path = "."
)
{
    [void][System.Reflection.Assembly]::LoadWithPartialName("Microsoft.TeamFoundation.Client")
    [void][System.Reflection.Assembly]::LoadWithPartialName("Microsoft.TeamFoundation.VersionControl.Client")

    function InitServerAndWorkspaceFromWSInfo($wsInfo)
    {
        $tfs = [Microsoft.TeamFoundation.Client.TeamFoundationServerFactory]::GetServer($wsInfo.ServerUri.AbsoluteUri)
        $vcs = $tfs.GetService([Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer])
        return $vcs.GetWorkspace($wsInfo)
    }

    # is there only 1 workspace in our cache file?  If so, use that one regardless of the hint
    $workspaceInfos = [Microsoft.TeamFoundation.VersionControl.Client.Workstation]::Current.GetAllLocalWorkspaceInfo()
    if ($workspaceInfos.Length -eq 1)
    {
        return InitServerAndWorkspaceFromWSInfo($workspaceInfos[0])
    }

    $workspaceInfos = [Microsoft.TeamFoundation.VersionControl.Client.Workstation]::Current.GetLocalWorkspaceInfoRecursively($path)
    if ($workspaceInfos.Length -gt 1)
    {
        throw 'More than one workspace matches the workspace hint "{0}": {1}' -f
            $path, [string]::join(', ', @($workspaceInfos | %{ $_.Name}))
    }
    elseif ($workspaceInfos.Length -eq 1)
    {
        return InitServerAndWorkspaceFromWSInfo($workspaceInfos[0])
    }
    else
    {
        throw "Could not figure out a workspace based on $path"
    }
}

function Get-TfsHistory(
    [string] $path,
    [switch] $raw
)
{
    $workspace = Get-TfsWorkspace $path
    $vcs = $workspace.VersionControlServer
    $serverPath = $workspace.GetServerItemForLocalItem($path)

    $changes = $vcs.QueryHistory($serverPath, [Microsoft.TeamFoundation.VersionControl.Client.VersionSpec]::Latest, 0, [Microsoft.TeamFoundation.VersionControl.Client.RecursionType]::None, $null, $null, $null, 100, $false, $false)
    if ($raw)
    {
        foreach($cs in $changes)
        {
            $cs
        }
    }
    else
    {
        foreach($cs in $changes)
        {
            new-object PSObject -Property @{
                Uri = $cs.ArtifactUri;
                Comment = $cs.Comment;
                Committer = $cs.Committer;
                Owner = $cs.Owner;
                Date = $cs.CreationDate;
                Id = $cs.ChangesetId;
            }
        }
    }
}

function get-directoryOfFileAbove($file, $start = $null)
{
    if (-not $start)
    {
        $start = resolve-path "."
    }

    while(($start) -and (-not (test-path "$start\$file")))
    {
        $start = split-path -Parent $start
    }
    return $start
}

function die($message)
{
    write-host -Foreground "Red" $message
    exit 1
}

function add-vhd($vhdfile) {
   $path = resolve-path $vhdfile
   $script = "SELECT VDISK FILE=`"$path`"`r`nATTACH VDISK"
   $script | diskpart
}

function remove-vhd($vhdfile) {
   $path = resolve-path $vhdfile
   $script = "SELECT VDISK FILE=`"$path`"`r`nDETACH VDISK"
   $script | diskpart
}