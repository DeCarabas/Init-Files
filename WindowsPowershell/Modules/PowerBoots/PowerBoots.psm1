##Requires -Version 2.0
####################################################################################################
$PowerBootsPath = $PSScriptRoot
$ParameterHashCache = @{}
$DependencyProperties = @{}
if(Test-Path $PowerBootsPath\DependencyProperties.clixml) {
   $DependencyProperties = Import-CliXml $PowerBootsPath\DependencyProperties.clixml
}
$LoadedAssemblies = @(); 

$null = [Reflection.Assembly]::Load( "PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35" )

## Dot-Source all of these
. "$PowerBootsPath\UtilityFunctions.ps1"
. "$PowerBootsPath\ContentProperties.ps1"
. "$PowerBootsPath\Add-BootsFunction.ps1"
. "$PowerBootsPath\Set-DependencyProperty.ps1"
. "$PowerBootsPath\New-BootsImage.ps1"

# This is #Requires -STA
$IsSTA = [System.Threading.Thread]::CurrentThread.ApartmentState -eq "STA"
if( !$IsSTA ) { 
   Write-Warning "Out-BootsWindow disabled in MTA mode. (Must run PowerShell with -STA switch to enable)"
} else { # Requires -STA
   . "$PowerBootsPath\Out-BootsWindow.ps1"
}

## Put ths scripts into the path
[string[]]$path = ${Env:Path}.Split(";")
if($path -notcontains "$PowerBootsPath\Functions\") {
   $path += "$PowerBootsPath\Functions\"
   ${Env:Path} = [string]::Join(";", $path)
}

## Generate aliases for all the functions ....
$Scripts = Get-ChildItem "$PowerBootsPath\Functions\New-*.ps1" -ErrorAction 0

if($Scripts) {
   foreach($script in $Scripts) {
      $TypeName = $script.Name -replace 'New-(.*).ps1','$1'
      
      # Set-Alias -Name "New-$TypeName" $Script.FullName                     -EA "SilentlyContinue" -EV +ErrorList
      Set-Alias -Name "$($TypeName.Split('.')[-1])" "New-$TypeName"        -EA "SilentlyContinue" -EV +ErrorList
   }
} else {
   & "$PowerBootsPath\Reset-DefaultBoots.ps1"
   ## Generate aliases for all the functions ....
   foreach($script in Get-ChildItem "$PowerBootsPath\Functions\New-*.ps1" -ErrorAction 0) {
      $TypeName = $script.Name -replace 'New-(.*).ps1','$1'
      
      # Set-Alias -Name "New-$TypeName" $Script.FullName                     -EA "SilentlyContinue" -EV +ErrorList
      Set-Alias -Name "$($TypeName.Split('.')[-1])" "New-$TypeName"        -EA "SilentlyContinue" -EV +ErrorList
   }
}

## Extra aliases....
# A work around for the built-in "Grid" alias, call it GridPanel (which is what the class ought to be anyway?)
$errorList = @()
Set-Alias -Name GridPanel  -Value "New-System.Windows.Controls.Grid"   -EA "SilentlyContinue" -EV +ErrorList
if($ErrorList.Count) { Write-Warning """GridPanel"" alias not created, you must use New-System.Windows.Controls.Grid" }

$errorList = @()
Set-Alias -Name Boots      -Value "New-BootsWindow"         -EA "SilentlyContinue" -EV +ErrorList
if($ErrorList.Count) { Write-Warning "Boots alias not created, you must use the full New-BootsWindow function name!" }

$errorList = @()
Set-Alias -Name BootsImage -Value "Out-BootsImage"          -EA "SilentlyContinue" -EV +ErrorList
if($ErrorList.Count) { Write-Warning "BootsImage alias not created, you must use the full Out-BootsImage function name!" }

Set-Alias -Name obi    -Value "Out-BootsImage"          -EA "SilentlyContinue"
Set-Alias -Name sap    -Value "Set-DependencyProperty"  -EA "SilentlyContinue"
Set-Alias -Name gbw    -Value "Get-BootsWindow"         -EA "SilentlyContinue"
Set-Alias -Name rbw    -Value "Remove-BootsWindow"      -EA "SilentlyContinue"
                                                    
#  Write-Warning "======================================================================="
#  Write-Warning "== NOTICE: It is a known bug that the alias ""Grid"" will not be created."
#  Write-Warning "== We create ""GridPanel"" instead, but if there are other warnings above"
#  Write-Warning "== you should take note of them, as PowerBoots samples may not work."
#  Write-Warning "======================================================================="

$BootsFunctions = @("Add-BootsFunction", "Set-DependencyProperty", "New-*") +
                  @("Get-BootsModule", "Get-BootsAssemblies", "Get-Parameter", "Get-BootsParam" ) + 
                  @("Get-BootsContentProperty", "Add-BootsContentProperty", "Remove-BootsContentProperty") +
                  @("Get-BootsHelp", "Get-BootsCommand", "Out-BootsWindow", "New-BootsImage")
Export-ModuleMember -Function $BootsFunctions -Alias * -Variable IsSTA, PowerBootsPath
## Post CTP3 we have to export the submodule separately:
if($PSVersionTable.BuildVersion -ge "6.1.7100.0") {
   Export-ModuleMember -Cmdlet (Get-Command -Module PoshWpf)
}

# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUR7VEx7LrX2dfu0qSJ5DECU/S
# PHagggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFB92V5zsSwi1KlFOwTfUsPnD0RQUMA0GCSqG
# SIb3DQEBAQUABIICANZYi0/CbmXKwfAvtDz8Qu36EgoJf5aBcz6Zx6UNoOg4unXT
# 8N/0mErkJKUgOHGv3tpmhFlNMrWLBKPJXJBxxUqZg4IuKDWp96nTSatke1SfVu4U
# +aTuM3dlBQnXSr1wG1Gkk01jsXSl30i6whuzs3GclelLpSUp7BfVHh9H0n3YyqRu
# gPXlO8Oe6BpsQVpvAnUScnuf/2AAoAc0T8DM/Sll66Re9pxqAbCtA3h7/RNRpuQc
# dOmsyAULWkjqYoMtGRZfcw6QEBMoORFyeuS9CvWL8faRDGC2OFmFpydUmEglrfk4
# SwbV+7Lwy6QK8ZRSQldvlnVzEkFqW0k9erQdeNgI0D114qeWlAROaQ3jax6lUdMt
# KENMnXxX9FBsv1gNwdnnm+0o3JO0YwSQjcMWgEgkPrqrfK/f49Jh66vQPqlGbc48
# 9J3p0ktsXmPWlyf0FOQgsKETBC4YQTTfjn+9UTyPGf8D5i3KfV6AeYRc3xegMQcU
# 6ouepRDUzJ/vuEoF5vNJCNTfwfJRJM0Me3BP1a1ZnX5tJXuEJXty2TOJiSq0yaGh
# hux0Yn8z2FUgw2aKD2+2soObDyEB8jSaVKcbP+wMN/7s6rpcXCsSx5VXJih8ERuK
# fPhJZyQHKo0HeF//r1YzizVhz5bZFEwzWTpqB63GUg/apykOvMICqW2z5efI
# SIG # End signature block
