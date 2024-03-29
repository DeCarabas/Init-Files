
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Switch]$AllowDrop
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Background
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BaselineAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BindingGroup
,
	[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=$true)]
	[Object[]]$Blocks
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderBrush
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderThickness
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$CanDelayPlacement
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$CommandBindings
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ContextMenu
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Cursor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$DataContext
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FlowDirection
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$Focusable
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FocusVisualStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontFamily
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontSize
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontStretch
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontWeight
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ForceCursor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Foreground
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Height
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$HorizontalAnchor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$HorizontalOffset
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$InputBindings
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$InputScope
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsEnabled
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Language
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$LineHeight
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$LineStackingStrategy
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Margin
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Name
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$OverridesDefaultStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Padding
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Resources
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$SiblingInlines
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Style
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Tag
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextDecorations
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextEffects
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ToolTip
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalAnchor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalOffset
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Width
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$WrapDirection
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_TargetUpdated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_SourceUpdated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DataContextChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Initialized
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Loaded
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Unloaded
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ToolTipOpening
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ToolTipClosing
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ContextMenuOpening
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ContextMenuClosing
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsEnabledChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_FocusableChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_QueryCursor
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewKeyDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_KeyDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewKeyUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_KeyUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewGotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewLostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewTextInput
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_TextInput
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewQueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_QueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewGiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragOver
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragOver
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDrop
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Drop
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsKeyboardFocusWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsKeyboardFocusedChanged
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)

## Preload the assembly if it's not already loaded


if( [Array]::BinarySearch(@(Get-BootsAssemblies), 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) -lt 0 ) {
  $null = [Reflection.Assembly]::Load( 'PresentationFramework, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35' ) 
}
if($ExecutionContext.SessionState.Module.Guid -ne (Get-BootsModule).Guid) {
	Write-Warning "Figure not invoked in PowerBoots context. Attempting to reinvoke."
   $scriptParam = $PSBoundParameters
   return iex "& (Get-BootsModule) '$($MyInvocation.MyCommand.Path)' `@PSBoundParameters"
}
# Write-Host "Figure in module $($executioncontext.sessionstate.module) context!" -fore Green


function Global:New-System.Windows.Documents.Figure {
<#
.Synopsis
   Create a new Figure object
.Description
   Generates a new System.Windows.Documents.Figure object, and allows setting all of it's properties
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  07/23/2009 09:24:57
#>
 
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Switch]$AllowDrop
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Background
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BaselineAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BindingGroup
,
	[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=$true)]
	[Object[]]$Blocks
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderBrush
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderThickness
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$CanDelayPlacement
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$CommandBindings
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ContextMenu
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Cursor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$DataContext
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FlowDirection
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$Focusable
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FocusVisualStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontFamily
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontSize
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontStretch
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$FontWeight
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ForceCursor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Foreground
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Height
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$HorizontalAnchor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$HorizontalOffset
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$InputBindings
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$InputScope
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsEnabled
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Language
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$LineHeight
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$LineStackingStrategy
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Margin
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Name
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$OverridesDefaultStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Padding
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Resources
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$SiblingInlines
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Style
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Tag
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextDecorations
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TextEffects
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ToolTip
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalAnchor
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalOffset
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Width
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$WrapDirection
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_TargetUpdated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_SourceUpdated
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DataContextChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Initialized
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Loaded
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Unloaded
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ToolTipOpening
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ToolTipClosing
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ContextMenuOpening
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_ContextMenuClosing
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsEnabledChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_FocusableChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewMouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_MouseLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_QueryCursor
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_StylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewStylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewKeyDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_KeyDown
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewKeyUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_KeyUp
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewGotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewLostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_LostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewTextInput
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_TextInput
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewQueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_QueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewGiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_GiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragEnter
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragOver
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragOver
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDragLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_DragLeave
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_PreviewDrop
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_Drop
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsKeyboardFocusWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsMouseCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsStylusCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[PSObject]$On_IsKeyboardFocusedChanged
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)
BEGIN {
   $DObject = New-Object System.Windows.Documents.Figure
   $All = Get-Parameter New-System.Windows.Documents.Figure | ForEach-Object { $_.Key } | Sort
}
PROCESS {

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
} #Process
END {
   Microsoft.PowerShell.Utility\Write-Output $DObject
}
}
                                                                        
New-System.Windows.Documents.Figure @PSBoundParameters
