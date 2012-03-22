function New-BootsImage {
#
#.Synopsis
#   Convert a PowerBoots element (WPF Visual) into an image
#.Description
#   Takes a WPF Visual element and captures it as an image, optionally saving it to file (otherwise, it's placed on the clipboard).
#.Parameter Content
#   A scriptblock that generates the element that you want to capture an image of
#.Example
#   StackPanel -Margin "10,5,10,5" -Background White $( 
#      Label "Please enter your name:"
#      StackPanel -Orientation Horizontal $(
#         TextBox -OutVariable global:textbox -Width 150 -On_KeyDown { 
#            if($_.Key -eq "Return") { 
#               Write-Output $textbox[0].Text
#               $BootsWindow.Close()
#            }
#         }
#         Button "Ok" -Padding "5,0,5,0" -Margin "2,0,0,0" -On_Click { 
#            Write-Output $textbox[0].Text
#            $BootsWindow.Close()
#         }
#      )
#   ) | Out-BootsImage Statistics.jpg
#   
#   Take a Screenshot of an input window. NOTE: this won't capture the window chrome.
#   
#.Example
#Chart -Width 200 -Height 150 -Theme Theme3 -Watermark $false -Animation $false (
#   DataSeries $(
#      1..(Get-Random -min 3 -max 6) | ForEach-Object {
#         DataPoint -YValue (Get-Random 100)
#      }
#   )
#) | Out-BootsImage Statistics.png  | ForEach-Object { 
#   Send-FTP HuddledMasses.org (Get-Credential) -LocalFile $_ -Remotefile "/public_html/$($_.Name)" 
#   [Windows.Clipboard]::SetText( "!http://HuddledMasses.org/$($_.Name)!" )
#}
#
#   Using the Visifire charting components, generate a random chart, convert it to a png image, upload it to my webserver using the NetCmdlets Send-FTP, and finally, send the new URL to the clipboard...
#   
#.Links 
#   http://HuddledMasses.org/powerboots-to-image
#.ReturnValue
#   The file path, if saved to file.
#.Notes
# AUTHOR:    Joel Bennett http://HuddledMasses.org
# LASTEDIT:  2009-01-07 11:35:23
#
#[CmdletBinding(DefaultParameterSetName="ToClipboard")]
Param(
   #[Parameter(Position=0, Mandatory=$false, ParameterSetName="ToFile")]
   [string]$FileName
,
   #[Parameter(Position=1, ValueFromPipeline=$true, Mandatory=$true)]
   [ScriptBlock]$Content
,
   [double]$dpiX = 96.0
,
   [double]$dpiY = 96.0
,
   [Windows.Media.PixelFormat]$pixelFormat = "Pbgra32"
)
PROCESS {
   [ScriptBlock]$global:export = iex @"
   { Param(`$window)
      `$null = Export-BootsImage '$FileName' `$Window '$dpiX,$dpiY' '$pixelFormat'
      `$Window.Close()
   }
"@
   $global:bmod = Get-BootsModule
   $global:Content = $Content

   $null = New-BootsWindow -Title "ScreenCapWindow" {
      GridPanel $global:Content -Background White
   } -On_ContentRendered {
      & $global:export $this
   }

   Remove-BootsWindow "ScreenCapWindow"
   $files = "{0}*{1}" -f [IO.Path]::GetFileNameWithoutExtension($FileName),
                         [IO.Path]::GetExtension($FileName)
   Write-Host $files
   Get-ChildItem $files
}
}

# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUIb53gkd3NESwLY9I4C5XKXt+
# L0OgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
# MQswCQYDVQQGEwJVUzERMA8GA1UECBMITmV3IFlvcmsxEjAQBgNVBAcTCVJvY2hl
# c3RlcjEhMB8GA1UEChMYaHR0cDovL0h1ZGRsZWRNYXNzZXMub3JnMSgwJgYDVQQL
# Ex9TY3JpcHRpbmcgQ2VydGlmaWNhdGUgQXV0aG9yaXR5MTcwNQYDVQQDEy5odHRw
# Oi8vSHVkZGxlZE1hc3Nlcy5vcmcgQ2VydGlmaWNhdGUgQXV0aG9yaXR5MScwJQYJ
# KoZIhvcNAQkBFhhKYXlrdWxASHVkZGxlZE1hc3Nlcy5vcmcwHhcNMDkwMzE1MTkx
# OTE5WhcNMTAwMzE1MTkxOTE5WjCBqzELMAkGA1UEBhMCVVMxETAPBgNVBAgTCE5l
# dyBZb3JrMRIwEAYDVQQHEwlSb2NoZXN0ZXIxITAfBgNVBAoTGGh0dHA6Ly9IdWRk
# bGVkTWFzc2VzLm9yZzESMBAGA1UECxMJU2NyaXB0aW5nMRUwEwYDVQQDEwxKb2Vs
# IEJlbm5ldHQxJzAlBgkqhkiG9w0BCQEWGEpheWt1bEBIdWRkbGVkTWFzc2VzLm9y
# ZzCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAPfqxOG9TQN+qZjZ6KfM
# +zBK0YpjeyPL/cFgiGBhiIdYWTBtkbZydFr3IiERKRsUJ0/SKFbhf0C3Bvd/neTJ
# qiZjH4D6xkrfdLlWMmmSXXqjSt48jZp+zfCAIaF8K84e9//7lMicdVFE6VcgoATZ
# /eMKQky4JvphJpzDHYPLxLJQrKd0pjDDwspjdX5RedWkzeZBG7VfBnebLWUzgnMX
# IxRQKfFCMryQDP8weceOnJjfJEf2FYmdpsEg5EKKKbuHsQCMVTxfteKdPvh1oh05
# 1GWyPsvEPh4auJUT8pAVvrdxq+/O9KW/UV01UxjRYM1vdklNw8g7mkJTrrHjSjl7
# tuugCnJjt5kN6v/OaUtRRMR68O85bSTVGOxJGCHUKlyuuTx9tnfIgy4siFYX1Ve8
# xwaAdN3haTon3UkWzncHOq3reCIVF0luwRZu7u+TnOAnz2BRlt+rcT0O73GN20Fx
# gyN2f5VGBbw1KuS7T8XZ0TFCspUdgwAcmTGuEVJKGhVcGAvNlLx+KPc5dba4qEfs
# VZ0MssC2rALC1z61qWuucb5psHYhuD2tw1SrztywuxihIirZD+1+yKE4LsjkM1zG
# fQwDO/DQJwkdByjfB2I64p6mk36OlZAFxVfRBpXSCzdzbgKpuPsbtjkb5lGvKjE1
# JFVls1SHLJ9q80jHz6yW7juBAgMBAAGjgcgwgcUwHQYDVR0OBBYEFO0wLZyg+qGH
# Z4WO8ucEGNIdU1T9MB8GA1UdIwQYMBaAFN2N42ZweJLF1mz0j70TMxePMcUHMAkG
# A1UdEwQCMAAwEQYJYIZIAYb4QgEBBAQDAgTwMCoGA1UdJQEB/wQgMB4GCCsGAQUF
# BwMBBggrBgEFBQcDAgYIKwYBBQUHAwMwCwYDVR0PBAQDAgTwMCwGCWCGSAGG+EIB
# DQQfFh1PcGVuU1NMIEdlbmVyYXRlZCBDZXJ0aWZpY2F0ZTANBgkqhkiG9w0BAQUF
# AAOCAgEAmKihxd6KYamLG0YLvs/unUTVJ+NW3jZP16R28PpmidY/kaBFOPhYyMl2
# bBGQABe7LA5rpHFAs0F56gYETNoFk0qREVvaoz9u18VfLb0Uwqtnq0P68L4c7p2q
# V3nKmWjeI6H7BAyFuogxmMH5TGDfiqrrVSuh1LtPbkV2Wtto0SAxP0Ndyts2J8Ha
# vu/2rt0Ic5AkyD+RblFPtzkCC/MLVwSNAiDSKGRPRrLaiGxntEzR59GRyf2vwhGg
# oAXUqcJ/CVeHCP6qdSTM39Ut3RmMZHXz5qY8bvLgNYL6MtcJAx+EeUhW497alzm1
# jInXdbikIh0d/peTSDyLbjS8CPFFtS6Z56TDGMf+ouTpEA16otcWIPA8Zfjq+7n7
# iBHjeuy7ONoJ2VDNgqn9B+ft8UWRwnJbyB85T83OAGf4vyhCPz3Kg8kWxY30Bhnp
# Fayc6zQKCpn5o5T0/a0BBHwAyMfr7Lhav+61GpzzG1KfAw58N2GV8KCPKNEd3Zdz
# y07aJadroVkW5R+35mSafKRJp5pz20GDRwZQllqGH1Y/UJFEiI0Bme9ecbl2vzNp
# JjHyl/jLVzNVrBI5Zwb0lCLsykApgNY0yrwEqaiqwcxq5nkXFDhDPQvbdulihSo0
# u33fJreCm2fFyGbTuvR61goSksAvLQhvijLAzcKqWKG+laOtYpAxggOTMIIDjwIB
# ATCB8TCB4zELMAkGA1UEBhMCVVMxETAPBgNVBAgTCE5ldyBZb3JrMRIwEAYDVQQH
# EwlSb2NoZXN0ZXIxITAfBgNVBAoTGGh0dHA6Ly9IdWRkbGVkTWFzc2VzLm9yZzEo
# MCYGA1UECxMfU2NyaXB0aW5nIENlcnRpZmljYXRlIEF1dGhvcml0eTE3MDUGA1UE
# AxMuaHR0cDovL0h1ZGRsZWRNYXNzZXMub3JnIENlcnRpZmljYXRlIEF1dGhvcml0
# eTEnMCUGCSqGSIb3DQEJARYYSmF5a3VsQEh1ZGRsZWRNYXNzZXMub3JnAgkAs+mo
# OP3CnvEwCQYFKw4DAhoFAKB4MBgGCisGAQQBgjcCAQwxCjAIoAKAAKECgAAwGQYJ
# KoZIhvcNAQkDMQwGCisGAQQBgjcCAQQwHAYKKwYBBAGCNwIBCzEOMAwGCisGAQQB
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFJFy9zhVKhFX5fp+uREOAh2/PVukMA0GCSqG
# SIb3DQEBAQUABIICAEM11WDkQ9KgbcWovHMMtb+mXdkke/A0X/0D3jbyWzYF2s+U
# y1rspn8NbNSTA4bTzqQeGlx0cCL9QIdoP0PM92q+96uFqMqAkmbR7FqKjrHmLKCz
# 2zcJCbOEetSuOKKXa9Y22s49DeS+Icb1gtHiWyf2dj8M4J6Qx7XQ/OsNcz9GunJJ
# lDWJsNijVloACLxPT6d39wDL+srefUVKYa581zicw9f3c9d/mIyCN7yvEpWVwfLj
# gm3tHFiJs9HhD3cJbRMSl4tRKmsfWzIfAqXddtGaW0erJSz0hoj4tJoMIkAbxorP
# zkBrbGgeaqa8TlDkDvFTurkDyo2ZdJ1r9HkeYZC2ScyZBSYcXy+BFiFkJ5akjasc
# vErtlVntcVxOb+DiCkfcpq0dfm+xTCFC+M+40eysjNqhoSDgvo2XRuDtoFpGI2Og
# DJCoaWxi5EYYRfHg5rx4NMZHAqLV7MAVs6PSEOPEYPtnfvu9DqpUql4sNla4oEtW
# 0utbKVFm9IKbZb+R22qfRJ3sd1aLVLueoo5eiBt89lIqVX2+egkVlVwAHPE5Woi/
# 2Gl3AC7+sVFVOq+ugo8jkyFlQdmC+8DH5d1b7eXuEWrEP4OTpQCsvyljfbdeXyFW
# +xuJ3sGMXSYZpqvPuQ7RI7hKQYaq9j3JCgPAtOe/o3KYiHg13P/HanoCxgFU
# SIG # End signature block
