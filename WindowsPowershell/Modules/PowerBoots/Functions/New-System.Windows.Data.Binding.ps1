
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Object[]]$AsyncState
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BindingGroupName
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$BindsDirectlyToSource
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Converter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ConverterCulture
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ConverterParameter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ElementName
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FallbackValue
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsAsync
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Mode
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnSourceUpdated
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnTargetUpdated
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnValidationError
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Path
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RelativeSource
,
	[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=$true)]
	[Object[]]$Source
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$StringFormat
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TargetNullValue
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$UpdateSourceExceptionFilter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$UpdateSourceTrigger
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ValidatesOnDataErrors
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ValidatesOnExceptions
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ValidationRules
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$XPath
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)

## Preload the assembly if it's not already loaded


if( [Array]::BinarySearch(@(Get-BootsAssemblies), 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) -lt 0 ) {
  $null = [Reflection.Assembly]::Load( 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) 
}
if($ExecutionContext.SessionState.Module.Guid -ne (Get-BootsModule).Guid) {
	Write-Warning "Binding not invoked in PowerBoots context. Attempting to reinvoke."
   $scriptParam = $PSBoundParameters
   return iex "& (Get-BootsModule) '$($MyInvocation.MyCommand.Path)' `@PSBoundParameters"
}
# Write-Host "Binding in module $($executioncontext.sessionstate.module) context!" -fore Green


function Global:New-System.Windows.Data.Binding {
<#
.Synopsis
   Create a new Binding object
.Description
   Generates a new System.Windows.Data.Binding object, and allows setting all of it's properties
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  07/23/2009 09:24:56
#>
 
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Object[]]$AsyncState
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BindingGroupName
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$BindsDirectlyToSource
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Converter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ConverterCulture
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ConverterParameter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ElementName
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FallbackValue
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsAsync
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Mode
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnSourceUpdated
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnTargetUpdated
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$NotifyOnValidationError
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Path
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RelativeSource
,
	[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=$true)]
	[Object[]]$Source
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$StringFormat
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TargetNullValue
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$UpdateSourceExceptionFilter
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$UpdateSourceTrigger
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ValidatesOnDataErrors
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ValidatesOnExceptions
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ValidationRules
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$XPath
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)
BEGIN {
   $DObject = New-Object System.Windows.Data.Binding
   $All = Get-Parameter New-System.Windows.Data.Binding | ForEach-Object { $_.Key } | Sort
}
PROCESS {


   # The content of System.Windows.Data.Binding is not a collection
   # So if we're in a pipeline, make a new Binding each time
   if($_) { 
      $DObject = New-Object System.Windows.Data.Binding
   }

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
   Microsoft.PowerShell.Utility\Write-Output $DObject
} #Process
}
                                                                        
New-System.Windows.Data.Binding @PSBoundParameters
