
function Set-DependencyProperty {
[CmdletBinding()]
PARAM(
   [Parameter(Position=0,Mandatory=$true)]
   $Property
,
   [Parameter(Mandatory=$true,ValueFromPipeline=$true)]
   $Element
,
   [Parameter()]
   [Switch]$Passthru
)
DYNAMICPARAM {
   $paramDictionary = new-object System.Management.Automation.RuntimeDefinedParameterDictionary
   $Param1 = new-object System.Management.Automation.RuntimeDefinedParameter
   $Param1.Name = "Value"
   # $Param1.Attributes.Add( (New-ParameterAttribute -Position 1) )
   $Param1.Attributes.Add( (New-Object System.Management.Automation.ParameterAttribute -Property @{ Position = 1 }) )
   ## We maybe don't need to keep this in memory?
   # $DependencyProperties = Import-CliXml $PowerBootsPath\DependencyProperties.clixml

   if( $Property ) {
      if($Property.GetType() -eq ([System.Windows.DependencyProperty]) -or
         $Property.GetType().IsSubclassOf(([System.Windows.DependencyProperty]))) 
      {
         $Param1.ParameterType = $Property.PropertyType
      } 
      elseif($Property -is [string] -and $Property.Contains(".")) {
         $Class,$Property = $Property.Split(".")
         if($DependencyProperties.ContainsKey($Property)){
            $type = $DependencyProperties[$Property] | ? { $_.DeclaringType -like "*$Class" }
            if($type) { 
               $Param1.ParameterType = [type]@($type)[0].PropertyType
            }
         }

      } elseif($DependencyProperties.ContainsKey($Property)){
         if($Element) {
            $type = $DependencyProperties[$Property] | ? { $_.DeclaringType -eq $element.GetType() }
            if($type) { 
               $Param1.ParameterType = [type]@($type)[0].PropertyType
            }
         } else {
            $Param1.ParameterType = [type]@($DependencyProperties[$Property])[0].PropertyType
         }
      }
      else 
      {
         $Param1.ParameterType = [PSObject]
      }
   }
   else 
   {
      $Param1.ParameterType = [PSObject]
   }
   $paramDictionary.Add("Value", $Param1)
   return $paramDictionary
}
PROCESS {   
   trap { 
      Write-Host "ERROR Setting Dependency Property" -Fore Red
      Write-Host "Trying to set $Property to $($Param1.Value)" -Fore Red
      continue
   }
   if($Property.GetType() -eq ([System.Windows.DependencyProperty]) -or
      $Property.GetType().IsSubclassOf(([System.Windows.DependencyProperty]))
   ){
      trap { 
         Write-Host "ERROR Setting Dependency Property" -Fore Red
         Write-Host "Trying to set $($Property.FullName) to $($Param1.Value)" -Fore Red
         continue
      }
      $Element.SetValue($Property, ($Param1.Value -as $Property.PropertyType))
   } else {
      $Class = ""
      if("$Property".Contains(".")) {
         $Class,$Property = "$Property".Split(".")
      }
         
      if($DependencyProperties.ContainsKey("$Property")){
         $fields = @($DependencyProperties[$Property] | ? { $_.DeclaringType -like "*$Class" -and ($Param1.Value -as ([type]$_.PropertyType))})
         if($fields.Count -eq 0 ) { 
            $fields = @($DependencyProperties[$Property] | ? { $_.DeclaringType -like "*$Class" })
         }
         if($fields.Count) {
            $success = $false
            foreach($field in $fields) {
               trap { 
                  Write-Host "ERROR Setting Dependency Property" -Fore Red
                  Write-Host "Trying to set $($field.DeclaringType)::$($field.Name) to $($Param1.Value) -as $($field.PropertyType)" -Fore Red
                  continue
               }
               $Element.SetValue( ([type]$field.DeclaringType)::"$($field.Name)", ($Param1.Value -as ([type]$field.PropertyType)))
               if($?) { $success = $true; break }
            }
            if(!$success) { throw "food" }
         } else {
            Write-Host "Couldn't find the right property: $Class.$Property on $($Element.GetType().Name) of type $($field.PropertyType)" -Fore Red      
         }
      } else {
         Write-Host "Unknown Dependency Property Key: $Property on $($Element.GetType().Name)" -Fore Red      
      }  
   }
   if( $Passthru ) {
      $Element
   }
}
}

# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUVrrwiQOQ3U08+Rn6NlRWqI6f
# BL6gggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFIvRijhW60hjUT58KPV2OIvAiChyMA0GCSqG
# SIb3DQEBAQUABIICALCo4WkEriSeLootrfkQ+3huLTHx3AqQQVEVSR9cJZnWqGt2
# ev57aGpjEufnhz5V6K633X5RAZ+/xIBfInEcf7E+X1aOlkUZU7XEH1s9cca9ujg5
# ge/o992C91iRO26ti2YSXihrsaiU0PT4NCM2QnJM15l6ILJYWF25juBZnxU4xlWF
# zFubzOTmpNH5XyjAtBg27N9UvAGbw1N568lM9VeDnW48SyoeDA5ZgpLPvyMjcCpY
# 6l8gaTcOYys0K6oZtHzqM9CYzhTR5H5af2ve9GY3YnT5uC5wUb2oZoEYDVrMjLBw
# G5ZCvlIp9+WhypuwcKB69Kfe6WuiWO2yAhbHR1xdQg+cJGvxSU2mJ5LuO78ILSdA
# R/up8nZy6U+WWjYJIWOtb6znnIpDHJ9qPlIOET/zKRnlF26J6Ki7pjZ2XY4NgzTI
# zeMNhlI5BO3w12W8kDeMoJBVShQBOciAbISgtwZs9Rriz+jdDxXQu/VsB4Dwx90X
# FXHlb2MP4G8BH6TukqQCcZWYQSWIb3bPXrLi5kiF4RMvftcpFbIq8jP/uoVIJ/nx
# akyaRf1g8bArUYLi/ug3Fg+2h0an/U5ZGpBSs6/KQs2UcX+fEUzbMstjpA3Anr1I
# rVW2D3ia8hg2a4yr8FtNTx7V9DwBh9hSXzLeiX3j6RQ1DlsUjKkwFZNcRixA
# SIG # End signature block
