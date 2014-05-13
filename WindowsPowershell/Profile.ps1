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

    # Fun times with diffs and the like.
    #
    $env:SDPWDIFF = 'odd.exe'
    $env:SDVDIFF  = 'odd.exe -l!'
    $env:SDVCDIFF = 'odd.exe -ld'
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

function prompt 
{ 
    $ok = $?

    if ($global:SolarizedColors)
    {
        # Our "theme", as it were. Note that we assume the use of the
        # solarized colors.
        #
        $cdelim = ConvertFrom-SolarizedColor "base1"
        $chost = ConvertFrom-SolarizedColor "green"
        $cloc = $csym = ConvertFrom-SolarizedColor "base0"
        if (-not $ok) { $csym = ConvertFrom-SolarizedColor "red" }
    }
    else
    {
        $cdelim = [ConsoleColor]::Gray 
        $chost = [ConsoleColor]::Green
        $cloc = $csym = [ConsoleColor]::Gray
        if (-not $ok) { $csym = [ConsoleColor]::Red; }        
    }
   
    write-host "$([char]0x0A7) " -n -f $csym
    write-host ([net.dns]::GetHostName()) -n -f $chost
    write-host ' {' -n -f $cdelim
    write-host (shorten-path (pwd).Path) -n -f $cloc
    write-host '}' -n -f $cdelim

    return ' '
}


function Get-APFile($Path, $Cockpit="BLUINFDPXYIH802:80")
{
    $localFile = join-path $pwd (split-path -leaf $Path)
    $uri = "http://$Cockpit/files?cmd=get&path=$Path"
    Write-Host "Getting $Path to $localFile via $uri"
    (New-Object System.Net.Webclient).DownloadFile($uri, $localFile)
}

function Get-Url($Url)
{
    (New-Object System.Net.Webclient).DownloadString($Url)
}

function ConvertFrom-JWT($text)
{
    $text = $text.Trim()
    $part = $text.Split('.')[1]
    while($part.Length % 4) {
        $part = $part + '='
    }

    $json = [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($part))
    return ConvertFrom-Json $json
}

function ConvertFrom-KeyType($data)
{
    switch([BitConverter]::ToInt16($data,0)) {
        1 { "SessionRoot"; break; }
        2 { "SessionRootEscrow_AES"; break; }
        3 { "K_R_G"; break; }
        4 { "Live_RSA"; break; }
        5 { "ContentKey"; break; }
        6 { "DevContentKeyEscrow_RSA"; break; }
        7 { "ProdContentKeyEscrow_RSA"; break; }
        8 { "XboxAutoVaultSign_RSA"; break; }
        9 { "EscrowedDevContentKeyEKB_RSA"; break; }
        10 { "EscrowedProdContentKeyEKB_RSA"; break; }
        11 { "DevDurangoGlobalEscrow_RSA"; break; }
        12 { "ProdDurangoGlobalEscrow_RSA"; break; }
        13 { "DevKitConversionCert_RSA"; break; }
        default { "Unknown ($_)"; break; }
    }
}

function ConvertFrom-EKB($text)
{
    $ekb = @{}

    $bytes = [Convert]::FromBase64String($text)

    $stream = New-Object System.IO.MemoryStream
    $stream.Write($bytes,0,$bytes.Length)
    $stream.Position = 0

    $reader = New-Object System.IO.BinaryReader($stream)

    $stream.Seek(8, 'Current') | out-null
    while($stream.Position -ne $stream.Length) {
        $type = $reader.ReadInt16()
        $length = $reader.ReadInt32()
        $data = $reader.ReadBytes($length)
        
        switch($type){
            1 { 
                $ekb.EscrowedKey = ConvertFrom-KeyType($data) 
                break
            }
            2 {
                $ekb.KeyID = [System.Text.Encoding]::UTF8.GetString($data)
                if ($ekb.KeyID.Length -eq 32) {
                    $kid = $ekb.KeyID
                    $gah = @() 
                    while ($kid.Length) {
                        $gah += @([Byte]::Parse($kid.Substring(0,2), "HexNumber"))
                        $kid = $kid.Substring(2)
                    }
                    $ekb.KeyIDGuid = [Guid][byte[]]($gah)
                }
                break
            }
            3 { 
                $ekb.EscrowingKeyType = ConvertFrom-KeyType($data)
                break
            }
            4 { 
                $ekb.EscrowingKeyID = [System.Text.Encoding]::UTF8.GetString($data)
                break
            }
            5 { 
                $ekb.EscrowMethod = switch([BitConverter]::ToInt16($data, 0)){
                    1 { "RSAEncrypt_OAEP_SHA256"; break; }
                    2 { "AESKeyWrap"; break; }
                    3 { "RSAEncrypt_PKCS1_SHA256"; break; }
                    default { "Unknown ($_)"; break; }
                }
                break
            }
            6 { 
                $ekb.CustomData = $data
                break
            }
            7 { 
                $ekb.EscrowBlob = $data
                break
            }
        }
    }

    return $ekb
}

function Wrap-Text ($txt)
{
    $t = [string]$txt
    while($t.Length -gt 77) {
        $t.Substring(0, 77)
        $t = $t.Substring(77)
    }
    $t
}

function ConvertFrom-Base64UTF8($base64) {
    return [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($base64))
}


function Save-Change($ChangeNumber, $Description)
{
    if (!$ChangeNumber) { $ChangeNumber = "default" }
    if (!$Description) { $Description = "Change-$ChangeNumber" }

    $PackPath = Resolve-Path "~\dpk"
    if (!(Test-Path $PackPath)) 
    {
        mkdir $PackPath | out-null
    }

    # Get Unique number...    
    $Date = Get-Date
    $Prefix = "$($Date.Year).$($Date.Month.ToString('00')).$($Date.Day.ToString('00'))"

    $Index = "00"
    $items = @(Get-ChildItem $PackPath -Filter "$Prefix*.dpk" | Sort-Object -Property FullName -Descending)
    if ($items.Length -gt 0)
    {
        $idx = [int]($items[0].FullName.Split('.')[3]) + 1
        $Index = $idx.ToString('00')
    }
    
    $PackFile = "$PackPath\$Prefix.$Index.$Description.dpk"
    Write-Host "Packing change '$ChangeNumber' to '$PackFile'..."
    sdp pack $PackFile -c $ChangeNumber 
}

function Convert-HexNumberToGuid($hn)
{
    $gah = @() 
    while ($hn.Length) 
    {
        $gah += @([Byte]::Parse($hn.Substring(0,2), "HexNumber"))
        $hn = $hn.Substring(2)
    }
    return [Guid][byte[]]($gah)
}

function global:bld { build $args }
function global:bz  { build -PZM $args }
function global:bcz { build -cPZM $args }
