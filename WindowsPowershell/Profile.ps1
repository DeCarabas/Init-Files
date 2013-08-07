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

    if ($Host.Name -eq "ConsoleHost")
    {
        if (!$global:SolarizedColors)
        {
            Set-SolarizedColors -Dark
            $global:SolarizedColors = $true
        }

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

# XBOX HELPER FUNCTIONS

function Get-XboxDeployedPackageInfo() 
{
    xbrun /o reg query '\"HKU\S-1-5-21-2702878673-795188819-444038987-501\Software\Classes\Local Settings\Software\microsoft\windows\currentversion\appmodel\repository\packages"' /s
}

# function Get-XboxConnectedAddresses() 
# {
#     $addrs = @{}

#     $results = xbconnect
#     $reading = $false
#     foreach($line in $results) 
#     {
#         if ($line.IndexOf("XBTP connections") -ge 0) 
#         {
#             $reading = $true
#         }
#         elseif ($reading)
#         {
#             $parts = $line.Split(@(' '), 'RemoveEmptyEntries')
#             $key = $parts[0].Substring(0, $parts[0].Length - 1)
#             $value = $parts[1]
#             if ($value -eq 'Unreachable.')
#             {
#                 continue
#             }

#             $addrs[$key] = $value
#         }
#     }

#     return $addrs
# }

function Connect-XboxHostTelnet() 
{
    $addr = xbconnect -b 
    telnet $addr
}

function Invoke-XboxHost($command)
{
    xbrun /x/host /o $command
}

function Copy-XboxSystemFile($sourceFile, $targetFile)
{
    Write-Host "Unlinking Host OS and System OS..."
    Invoke-XboxHost "xvtool -w 1"

    Write-Host "Shutting down System OS..."
    Invoke-XboxHost "xvtool -s 2"

    Write-Host "Mounting System XVD..."
    Invoke-XboxHost "xvdutil -m f:\system.xvd"

    Write-Host "Copying file..."
    xbcp /x/host $sourceFile "Xg:$targetFile"

    Write-Host "Unmounting System XVD..."
    Invoke-XboxHost "xvdutil -umdn 3"

    Write-Host "Rebooting..."
    xbreboot /x/host
}

function Connect-XboxSystemKD()
{
    $addr = xbconnect -b
    windbg -k "net:port=50038,target=$addr"
}

function Connect-XboxGameKD()
{
    $addr = xbconnect -b
    windbg -k "net:port=50039,target=$addr"
}

function Connect-XboxHostKD()
{
    $addr = xbconnect -b
    windbg -k "net:port=,target=$addr"
}