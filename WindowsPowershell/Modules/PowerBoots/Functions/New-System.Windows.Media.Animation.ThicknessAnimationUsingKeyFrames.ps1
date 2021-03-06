
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Object[]]$AccelerationRatio
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$AutoReverse
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BeginTime
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$DecelerationRatio
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Duration
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FillBehavior
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsAdditive
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsCumulative
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$KeyFrames
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Name
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RepeatBehavior
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$SpeedRatio
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentStateInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentTimeInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentGlobalSpeedInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Completed
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_RemoveRequested
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Changed
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)

## Preload the assembly if it's not already loaded


if( [Array]::BinarySearch(@(Get-BootsAssemblies), 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) -lt 0 ) {
  $null = [Reflection.Assembly]::Load( 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) 
}
if($ExecutionContext.SessionState.Module.Guid -ne (Get-BootsModule).Guid) {
	Write-Warning "ThicknessAnimationUsingKeyFrames not invoked in PowerBoots context. Attempting to reinvoke."
   $scriptParam = $PSBoundParameters
   return iex "& (Get-BootsModule) '$($MyInvocation.MyCommand.Path)' `@PSBoundParameters"
}
# Write-Host "ThicknessAnimationUsingKeyFrames in module $($executioncontext.sessionstate.module) context!" -fore Green


function Global:New-System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames {
<#
.Synopsis
   Create a new ThicknessAnimationUsingKeyFrames object
.Description
   Generates a new System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames object, and allows setting all of it's properties
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  07/23/2009 09:24:59
#>
 
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Object[]]$AccelerationRatio
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$AutoReverse
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BeginTime
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$DecelerationRatio
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Duration
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FillBehavior
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsAdditive
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsCumulative
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$KeyFrames
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Name
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RepeatBehavior
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$SpeedRatio
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentStateInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentTimeInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_CurrentGlobalSpeedInvalidated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Completed
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_RemoveRequested
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Changed
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)
BEGIN {
   $DObject = New-Object System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames
   $All = Get-Parameter New-System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames | ForEach-Object { $_.Key } | Sort
}
PROCESS {


   # The content of System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames is not a collection
   # So if we're in a pipeline, make a new ThicknessAnimationUsingKeyFrames each time
   if($_) { 
      $DObject = New-Object System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames
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
                                                                        
New-System.Windows.Media.Animation.ThicknessAnimationUsingKeyFrames @PSBoundParameters
