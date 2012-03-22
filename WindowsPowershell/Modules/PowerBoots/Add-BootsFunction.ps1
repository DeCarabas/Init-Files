function Add-BootsFunction {
<#
.Synopsis
   Add support for a new class to Boots by creating the dynamic constructor function(s).
.Description
   Creates a New-Namespace.Type function for each type passed in, as well as a short form "Type" alias.

   Exposes all of the properties and events of the type as perameters to the function. 

   NOTE: The Type MUST have a default parameterless constructor.
.Parameter Type
   The type you want to create a constructor function for.  It must have a default parameterless constructor.
.Example
   Add-BootsFunction ([System.Windows.Controls.Button])
   
   Creates a new boots function for the Button control.

.Example
   [Reflection.Assembly]::LoadWithPartialName( "PresentationFramework" ).GetTypes() | Add-BootsFunction

   Will create boots functions for all the WPF components in the PresentationFramework assembly.  Note that you could also load that assembly using GetAssembly( "System.Windows.Controls.Button" ) or Load( "PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35" )

.Example
   Add-BootsFunction -Assembly PresentationFramework

   Will create boots functions for all the WPF components in the PresentationFramework assembly.

.Links 
   http://HuddledMasses.org/powerboots
.ReturnValue
   The name(s) of the function(s) created -- so you can export them, if necessary.
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  2009-01-13 16:35:23
#>
[CmdletBinding(DefaultParameterSetName="FromType")]
PARAM(
   [Parameter(Position=0,ValueFromPipeline=$true,ParameterSetName="FromType",Mandatory=$true)]
   [type[]]$type
,
   [Parameter(Position=0,ValueFromPipeline=$true,ParameterSetName="FromAssembly",Mandatory=$true)]
   [string[]]$Assembly
,
   [Parameter()]
   [switch]$Force
)
BEGIN {
   [Type[]]$Empty=@()
   if(!(Test-Path $PowerBootsPath\Functions)) {
      MkDir $PowerBootsPath\Functions
   }
}
END {
   Export-CliXml -Input $DependencyProperties -Path $PowerBootsPath\DependencyProperties.clixml
}
PROCESS {
   if($PSCmdlet.ParameterSetName -eq "FromAssembly") {
      [type[]]$type = @()
      foreach($lib in $Assembly) {
         $asm =  $null
         trap { continue }
         if(Test-Path $lib) {
            $asm =  [Reflection.Assembly]::LoadFrom( (Convert-Path (Resolve-Path $lib -EA "SilentlyContinue") -EA "SilentlyContinue") )
         }
         if(!$asm) {
            ## BUGBUG: LoadWithPartialName is "Obsolete" -- but it still works in 2.0/3.5
            $asm =  [Reflection.Assembly]::LoadWithPartialName( $lib )
         }
         if($asm) {
            $type += $asm.GetTypes() | ?{ $_.IsPublic    -and !$_.IsEnum      -and 
                                         !$_.IsAbstract  -and !$_.IsInterface -and 
                                         $_.GetConstructor( "Instance,Public", $Null, $Empty, @() )}
         } else {
            Write-Error "Can't find the assembly $lib, please check your spelling and try again"
         }
      }
   }

   $LoadedAssemblies = Get-BootsAssemblies 
   
   foreach($T in $type) {
      $TypeName = $T.FullName
      $ScriptPath = "$PowerBootsPath\Functions\New-$TypeName.ps1"
      Write-Verbose $TypeName

      ## Collect all dependency properties ....
      $T.GetFields() | 
         Where-Object { $_.FieldType -eq [System.Windows.DependencyProperty] } |
         Select-Object Name, @{n="DeclaringType";e={$_.DeclaringType.FullName}}, 
                  @{n="PropertyType";e={$_.DeclaringType::"$($_.Name)".PropertyType.FullName}},
                  @{n="Field";e={$_.DeclaringType::"$($_.Name)".Name}} |
         ForEach-Object { 
            if($DependencyProperties.ContainsKey( $_.Field )) {
               $DependencyProperties[$_.Field] = @($DependencyProperties[$_.Field]) + @($_)
            } else {
               $DependencyProperties[$_.Field] = $_
            }
         }
            
      if(!( Test-Path $ScriptPath ) -OR $Force) {
         $Pipelineable = @();
         ## Get (or generate) a set of parameters based on the the Type Name
         $Parameters = "[CmdletBinding(DefaultParameterSetName='Default')]`nPARAM(`n" + [String]::Join("`n,`n", @(
            ## Add all properties
            foreach ($p in $T.GetProperties("Public,Instance,FlattenHierarchy") | 
                              where {$_.CanWrite -Or $_.PropertyType.GetInterface([System.Collections.IList]) } | Sort Name -Unique)
            {
               if($p.Name -match "^$($BootsContentProperties -Join '$|^')`$") {
                  $Pipelineable += @(Add-Member -in $p.Name -Type NoteProperty -Name "IsCollection" -Value $($p.PropertyType.GetInterface([System.Collections.IList]) -ne $null) -Passthru)
                  "`t[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=`$true)]" +
                  "`n`t[Object[]]`$$($p.Name)"
               } elseif($p.PropertyType -eq [System.Boolean]) {
                  "`t[Parameter(ParameterSetName='Default')]"+
                  "`n`t[Switch]`$$($p.Name)"
               } else {
                  "`t[Parameter(ParameterSetName='Default')]"+
                  "`n`t[Object[]]`$$($p.Name)"
               }
            }
            
            ## Add all events
            foreach ($e in $T.GetEvents("Public,Instance,FlattenHierarchy"))
            {
               "`t[Parameter(ParameterSetName='Default')]" +
               "`n`t[PSObject]`$On_$($e.Name)"
            }
         )) + "`n,`n`t[Parameter(ValueFromRemainingArguments=`$true)]`n`t[string[]]`$DependencyProps`n)"

         $collectable = [bool]$(@(foreach($p in @($Pipelineable)){$p.IsCollection}) -contains $true)
         $ofs = "`n";

         #  Write-Host "Pipelineable Content Property for $TypeName: $($Pipelineable -ne $Null)" -Fore Cyan
         #  foreach($p in $Pipelineable) {write-host "$p is $(if(!$p.IsCollection) { "not " })a collection"}

### These three are "built in" to boots, so we don't need to write preloading for them
# PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35
# WindowsBase, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35
# PresentationCore, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35

$function = $(
"
$Parameters

## Preload the assembly if it's not already loaded


if( [Array]::BinarySearch(@(Get-BootsAssemblies), '$($T.Assembly.FullName)' ) -lt 0 ) {
$(
   $index = [Array]::BinarySearch($LoadedAssemblies, $T.Assembly.FullName)
   
   if( $index -gt 0 -and $LoadedAssemblies[$index].Location ) {
"  `$null = [Reflection.Assembly]::LoadFrom( '" + $LoadedAssemblies[$index].Location + "' ) "
   } else {
"  `$null = [Reflection.Assembly]::Load( '" + $($T.Assembly.FullName) + "' ) "

   }
)
}
if(`$ExecutionContext.SessionState.Module.Guid -ne (Get-BootsModule).Guid) {
	Write-Warning `"$($T.Name) not invoked in PowerBoots context. Attempting to reinvoke.`"
   `$scriptParam = `$PSBoundParameters
   return iex `"& (Get-BootsModule) '`$(`$MyInvocation.MyCommand.Path)' ```@PSBoundParameters`"
}
# Write-Host ""$($T.Name) in module `$(`$executioncontext.sessionstate.module) context!"" -fore Green


function Global:New-$TypeName {
<#
.Synopsis
   Create a new $($T.Name) object
.Description
   Generates a new $TypeName object, and allows setting all of it's properties
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  $(Get-Date)
#>
 
$Parameters
BEGIN {
   `$DObject = New-Object $TypeName
   `$All = Get-Parameter New-$TypeName | ForEach-Object { `$_.Key } | Sort
}
PROCESS {
"
if(!$collectable) {
"
   # The content of $TypeName is not a collection
   # So if we're in a pipeline, make a new $($T.Name) each time
   if(`$_) { 
      `$DObject = New-Object $TypeName
   }
"
}

@'
   foreach ($param in $PSBoundParameters.GetEnumerator() | ? { [Array]::BinarySearch($All,$_.Key) -ge 0 }) {
      ## TODO: HANDLE DEPENDENCY PROPERTIES
      if($param.Key -eq "DependencyProps") {
      ## HANDLE EVENTS ....
      } elseif ($param.Key.StartsWith("On_")) 
      {
         $EventName = "Add_" + $param.Key.SubString(3)
         $sb = $param.Value -as [ScriptBlock]
         if(!$sb) {
            $sb = (Get-Command $param.Value -CommandType Function,ExternalScript).ScriptBlock
         }
         Invoke-Expression "`$DObject.$EventName( {$($sb.GetNewClosure())} )"
      } ## HANDLE PROPERTIES ....
      else 
      { 
         try {
            ## TODO: File a BUG because Write-DEBUG and Write-VERBOSE die here.
            if($DebugPreference -ne "SilentlyContinue") {
               Write-Host "Setting $($param.Key) of $($DObject.GetType().Name) to $($param.Value)" -fore Yellow
            }
            if(@(foreach($sb in $param.Value) { $sb -is [ScriptBlock] }) -contains $true) {
               $Values = @()
               $bMod = Get-BootsModule
               foreach($sb in $param.Value) {
                  $Values += & $bMod $sb
               }
            } else {
               $Values = $param.Value
            }
      
            if ($DObject.$($param.Key) -is [System.Collections.IList]) {
               if($DebugPreference -ne "SilentlyContinue") { Write-Host "Parameter $($param.Name) is an IList" -fore Cyan}
               foreach ($value in @($Values)) {
                  try {
                     $null = $DObject.$($param.Key).Add($value)
                  }
                  catch [Exception]
                  {
                     # Write-Host "CAUGHT array problem" -fore Red
                     if($_.Exception.Message -match "Invalid cast from 'System.String' to 'System.Windows.UIElement'.") {
                        $null = $DObject.$($param.Key).Add( (TextBlock $value) )
                     } else { 
                       throw
                     }
                  }
               }
            }
            else {
               ## If they pass an array of 1 when we only want one, we just use the first value
               if($Values -is [System.Collections.IList] -and $Values.Count -eq 1) {
                  if($DebugPreference -ne "SilentlyContinue") { Write-Host "Value is an IList" -fore Cyan}
                  try {
                     $DObject.$($param.Key) = $Values[0]
                  }
                  catch [Exception]
                  {
                     # Write-Host "CAUGHT collection value problem" -fore Red
                     if($_.Exception.Message -match "Invalid cast from 'System.String' to 'System.Windows.UIElement'.") {
                        $null = $DObject.$($param.Key).Add( (TextBlock $Values[0]) )
                     }else { 
                        throw
                     }
                  }
               }
               else ## If they pass an array when we only want one, we try to use it, and failing that, cast it to strings
               {
                  if($DebugPreference -ne "SilentlyContinue") { Write-Host "Value is a just $Values" -fore Cyan}
                  try {
                     $DObject.$($param.Key) = $Values
                  } catch [Exception]
                  {
                     # Write-Host "CAUGHT value problem" -fore Red
                     if($_.Exception.Message -match "Invalid cast from 'System.String' to 'System.Windows.UIElement'.") {
                        $null = $DObject.$($param.Key).Add( (TextBlock $values) )
                     }else { 
                        throw
                     }
                  }
               }
            }
            if($DebugPreference -ne "SilentlyContinue") {
               if( $DObject.$($param.Key) -ne $null ) {
                  Write-Host $DObject.$($param.Key).GetType().FullName -fore Green
               }
            }
         }
         catch [Exception]
         {
            Write-Host "COUGHT AN EXCEPTION" -fore Red
            Write-Host $_ -fore Red
            Write-Host $this -fore DarkRed
         }
      }

      while($DependencyProps) {
         $name, $value, $DependencyProps = $DependencyProps
         $name = ([string]@($name)[0]).Trim("-")
         if($name -and $value) {
            Set-DependencyProperty -Element $DObject -Property $name -Value $Value
         }
      }
   }
'@
   if(!$collectable) {
@'
   Microsoft.PowerShell.Utility\Write-Output $DObject
} #Process
'@
   } else {
@'
} #Process
END {
   Microsoft.PowerShell.Utility\Write-Output $DObject
}
'@
   }
@"
}
                                                                        
New-$TypeName `@PSBoundParameters
"@
)

         Set-Content -Path $ScriptPath -Value $Function
      }
      New-Alias -Name $T.Name "New-$TypeName" -EA "SilentlyContinue"
   }                                                         
}#PROCESS
}#Add-BootsFunction
# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUHhtLLvsSLITfQ0Bj8OsPHlJI
# u8GgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFLc0+RGhSEPZ4FWvi4ZFzZ+O9lP0MA0GCSqG
# SIb3DQEBAQUABIICAC0niyf74gurVX3R+Jf4tKYgTLT8SkE+mfRpuDeY7ss1iNQq
# G3Tggsj66W9X5eRy+dKckb5yaymai0LQmiZUKk+/IvQ8LvdJykAqVXKHHlD97SH4
# mbL5vB3aM/jnar4zqxb7oCd96H+X21SnJosXImP/GQwi7u1q1TYA7rx1EnvRnnyB
# 61O03yF9k5kEhEmuANfzqpaKNRj+QjR1oYfpfou3lfrdoox/Y1JYfOFPvV3w4IU2
# LCAyp+IszIYhhvS/+ZvaagjS5cxkWmSb7KU4M1Vq0I67YVMsG6wzZM9Aj4jpdSHy
# OZIY6JYE3DnnNYcUAeOL/e3WPyTpN6cLcdMmXVB1cZACteVKIxUopOnyjQMamhT+
# kgEK5k3higM6Enz3RNIeVN7MQMd4IPiXmAIuJoRDDzsGt1I/9DarFKxUysk3HXbf
# cxLdYH5hF24L6tcnOtT6gZFXBIOnS8qCej+aC61mFbdeDo8dkCVbeeEDieqMdwaf
# zqmGTlNwHrdRROdj9EyB+QFoQK1Te+j18ry+NV7TJqjmOhtrKkEFjArU9sYizvO6
# Bkljz8n+/bXERGfx//r3fe3+2hywEdLa8N35yR33EWT1OckqmDlwjAX2tkjEPvtg
# rbuqMkVRQ6ORg/VH9bDLKszPWBGrkqtyDaKcIFmcY6pUXlgAaPyHsSf+ty3f
# SIG # End signature block
