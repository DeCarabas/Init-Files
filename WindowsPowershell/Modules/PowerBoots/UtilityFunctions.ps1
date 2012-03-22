function Get-BootsModule { $executioncontext.sessionstate.module }
#  function Get-BootsModule { 
#  [CmdletBinding()]
#  Param()
#     $PSCmdlet.MyInvocation.MyCommand.Module 
#  }


function Get-BootsAssemblies {
#
#.Synopsis
#   Get a list of FullNames for the loaded assemblies
#.Description
#   Gets a list of assemblies, with a Location property added to the ones which are not in the GAC
#
   $assm =  [System.AppDomain]::CurrentDomain.GetAssemblies()
   ## Update the list if we need to...
   if($assm.Count -ne $LoadedAssemblies.Count) {
      $LoadedAssemblies = $assm | Sort FullName | ForEach-Object { 
         if($_.GlobalAssemblyCache) { 
            $_.FullName
         } else {
            Add-Member -input $_.FullName -Type NoteProperty -Name Location -Value $_.Location -Passthru
         }
      }
   }
   $LoadedAssemblies
}
#
#.Synopsis
#   Get a list of parameters for a command
#.Description
#   Get-Parameter gets a list of the parameters for a command
#   but only includes the "Common Parameters" when they are specifically requested
#
function Get-Parameter {
PARAM([string]$CommandName, [switch]$IncludeCommon)
   (New-Object System.Management.Automation.CommandMetaData @(Get-Command $CommandName)[0], $IncludeCommon).Parameters.GetEnumerator()
}

function Get-BootsParam {
#
#.Synopsis
#   Get information about the possible parameters for a specific WPF type
#.Parameter CommandName
#   The name of the command you want help for (eg: "Window" or "New-System.Windows.Window")
#.Parameter Parameter
#   An optional pattern for the name(s) of the parameter(s) you want help for.
#.Example
#   Get-BootsParam Window
#   
#   Returns the list of parameters (including Events) for the WIndow class.
#.Example
#   Get-BootsParam Window On_TextInput
#   
#   Returns the details about the On_TextInput, including the expected type, and the parameter attributes.
#
PARAM([string]$CommandName, [string]$Parameter)

   if($Parameter) {
      Get-Parameter $CommandName $false | ? { $_.Key -match $Parameter } | Sort-Object Key | Format-Wide Key
   } else {
      Get-Parameter $CommandName $false | Sort-Object Key | Format-Wide Key
   }
}

## Get a list of all the boots commands
function Get-BootsCommand {
#.Synopsis
#  Lists all the Boots aliases
   $commands = get-alias | ? { $_.Definition -like "New-ObjectFromAlias" } | %{$_.Name}
   Get-Alias |? { $commands -contains $_.Definition }
   Write-Warning "Get-BootsCommand is deprecated. Use: Get-Command -Module PowerBoots"
}

## Open an MSDN link for a boots element
function Get-BootsHelp {
PARAM([string]$TypeName)
   [Diagnostics.Process]::Start( "http://msdn.microsoft.com/library/$(Get-BootsType $TypeName)" )
}


# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUWZeQmPlvRQBpX3sgVD6QhTdR
# PQKgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFEhzDWgs9bK8XhkhpXW5FLUULly8MA0GCSqG
# SIb3DQEBAQUABIICAAUjivdA93YIoCZbIN13tRBD1w3ZQiqXOQFPIcqF5rauQK6d
# vxiYLj+agC2KI0HHokpdTYPFry2Cw/mDbT8d3WR5x9KEWHnQq8buei/wx9BC5xc+
# +clDModFxswHomExuz6qhWYLkysE0bYt8+hkvXdwtvK9WhNFGRbiS5NkVifp7aiT
# i1cvjuy6xftj6h35Wwu9v1FFUnzpU4bjBTQ6vhsSMhW46h8D6O9fNkyMZ7+UOLj6
# xcEAPXgp9+DflsPewoiMWcuk1xyJHwLcJKKmB587Mjy2CQES8CTdSq5M1WWmlwpR
# qJZVVANgYK2o+w+fTlsvxt9rYAFiWGtPtfWdNe9YUdfOSQBhLKA61CYREgwV0PO9
# uZj0Dq3CNdH2370HUzkP37RHWf5z9gzYghNxkoimMCqxqLWUcMwHngrueNC/VuEy
# U/ief8+qy8rRIKTwPbJRJDbCJ5L++zDECgjkVCDLpRckL2MGCoD2tx1YzAvGdps7
# or69mXU1244w0QTNaY3AOin4DQT9F/jBSwtD7fWS17lizFU/dXyKw4qKZqTnra5Z
# GlB0VUJisht5yngUA1jn1zVlXR45/bkgMuD1j87q3H/0UI1DS2E/L7Gims/hKert
# EW6eaDpxh818IJr/r2MXTGkZM3OlUNGRvwaPLCEo8+oD37Xf/vFPEqy3H5E2
# SIG # End signature block
