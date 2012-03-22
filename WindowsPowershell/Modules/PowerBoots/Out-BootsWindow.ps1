## The OLD display function
function Out-BootsWindow {
<#
.Synopsis
   Show Boots content in a window using ShowDialog()
.Description
   Will show a Visual Element in a simple WPF Window.  Out-Boots uses ShowDialog() so it is not Async -- execution will not continue until the window is closed.  

   If you need to return anything, you need to just set the $BootsOutput variable from within an event handler.
.Parameter Content
   The content that you want to display in a Wpf Window. 
.Parameter SizeToContent
   Controls the automatic resizing of the window to fit the contents, defaults to "WidthAndHeight"
.Parameter Title
      The Window Title.  Defaults to "Boots"
.Example
   "You need to know this" | Out-Boots
   
   The simplest possible way to do a popup dialog with some text on it.
.Example
   Button -Content "I can count!" -on_click {$BootsOutput += $BootsOutput.Count + 1} | Boots 
 
   Will output a series of numbers for the number of times you click the button. Notice that the output only happens AFTER the window is closed.
.Links 
   http://HuddledMasses.org/powerboots-tutorial-walkthrough
.ReturnValue
   The value of the $BootsOutput (which, by default is an array).
.Notes
 AUTHOR:    Joel Bennett http://HuddledMasses.org
 LASTEDIT:  2009-01-07 11:35:23
#>
[CmdletBinding(DefaultParameterSetName='Default')]
PARAM(
	[Parameter(ParameterSetName='Default')]
	[Switch]$AllowDrop
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$AllowsTransparency
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Background
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BindingGroup
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BitmapEffect
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BitmapEffectInput
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderBrush
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$BorderThickness
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Clip
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ClipToBounds
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$CommandBindings
,
	[Parameter(ParameterSetName='Default',Position=1,ValueFromPipeline=$true, Mandatory=$true)]
	[ScriptBlock]$Content
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ContentStringFormat
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ContentTemplate
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ContentTemplateSelector
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
	[Object[]]$DialogResult
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Effect
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
	[Object[]]$HorizontalAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$HorizontalContentAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Icon
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
	[Switch]$IsHitTestVisible
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$IsTabStop
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Language
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$LayoutTransform
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Left
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Margin
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$MaxHeight
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$MaxWidth
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$MinHeight
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$MinWidth
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Name
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Opacity
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$OpacityMask
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$OverridesDefaultStyle
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Owner
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Padding
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RenderSize
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RenderTransform
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$RenderTransformOrigin
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ResizeMode
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Resources
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ShowActivated
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$ShowInTaskbar
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$SizeToContent
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$SnapsToDevicePixels
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Style
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$TabIndex
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Tag
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Template
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Title
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$ToolTip
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Top
,
	[Parameter(ParameterSetName='Default')]
	[Switch]$Topmost
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Triggers
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Uid
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$VerticalContentAlignment
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Visibility
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$Width
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$WindowStartupLocation
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$WindowState
,
	[Parameter(ParameterSetName='Default')]
	[Object[]]$WindowStyle
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_SourceInitialized
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Activated
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Deactivated
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StateChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LocationChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Closing
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Closed
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_ContentRendered
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseDoubleClick
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseDoubleClick
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_TargetUpdated
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_SourceUpdated
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_DataContextChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_RequestBringIntoView
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_SizeChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Initialized
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Loaded
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Unloaded
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_ToolTipOpening
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_ToolTipClosing
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_ContextMenuOpening
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_ContextMenuClosing
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseLeftButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseLeftButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseRightButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseRightButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewMouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseWheel
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseEnter
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_MouseLeave
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_GotMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LostMouseCapture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_QueryCursor
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusInAirMove
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusEnter
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusLeave
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusInRange
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusOutOfRange
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusSystemGesture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_GotStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LostStylusCapture
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_StylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusButtonDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewStylusButtonUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewKeyDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_KeyDown
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewKeyUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_KeyUp
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewGotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_GotKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewLostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LostKeyboardFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewTextInput
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_TextInput
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewQueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_QueryContinueDrag
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewGiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_GiveFeedback
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewDragEnter
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_DragEnter
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewDragOver
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_DragOver
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewDragLeave
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_DragLeave
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_PreviewDrop
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_Drop
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsMouseDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsKeyboardFocusWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsMouseCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsMouseCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsStylusDirectlyOverChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsStylusCapturedChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsStylusCaptureWithinChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsKeyboardFocusedChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LayoutUpdated
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_GotFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_LostFocus
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsEnabledChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsHitTestVisibleChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_IsVisibleChanged
,
	[Parameter(ParameterSetName='Default')]
	[ScriptBlock]$On_FocusableChanged
,
	[Parameter(ValueFromRemainingArguments=$true)]
	[string[]]$DependencyProps
)

BEGIN {
   [Object[]]$Global:BootsOutput = @()
   ## Default value for SizeToContent 
   if(!$PSBoundParameters.ContainsKey("SizeToContent")) {
      if(!$PSBoundParameters.ContainsKey("Width") -and !$PSBoundParameters.ContainsKey("Height")) {
         $PSBoundParameters.Add("SizeToContent", "WidthAndHeight")
      } elseif($PSBoundParameters.ContainsKey("Width") -and !$PSBoundParameters.ContainsKey("Height")) { 
         $PSBoundParameters.Add("SizeToContent", "Height")
      } elseif(!$PSBoundParameters.ContainsKey("Width") -and $PSBoundParameters.ContainsKey("Height")) { 
         $PSBoundParameters.Add("SizeToContent", "Width")
      }
   }
   ## Default value for SizeToContent 
   if(!$PSBoundParameters.ContainsKey("Title")) {
      $PSBoundParameters.Add("Title", "Boots")
   }   
}
PROCESS {
   if( $PSBoundParameters.Content -is [System.Windows.Window] ) {
      $Global:BootsWindow = $PSBoundParameters.Content
   } 
   else
   {
      if($PSBoundParameters.Content -is [ScriptBlock]) 
      {
         Write-Host "PowerBoots"
         $bMod = Get-BootsModule
         $PSBoundParameters.Content = & $bMod $PSBoundParameters.Content
      }
      $Global:BootsWindow = Window @PSBoundParameters
   }
   $null = $Global:BootsWindow.ShowDialog()
   return $Global:BootsOutput
}   
}

# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQU6gQXcIT5MdK9GjwIG44flpmN
# WKGgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFPuJF+mLj90yDC3eDPh7+C/65ahqMA0GCSqG
# SIb3DQEBAQUABIICANaxXQ9PpqOWyxgn9heWau4OfId6avXIxhNqw3y3PlNhHLO6
# ClWnDGJYJXWn2lu+iDaVQjjzKb8jAWcCqKo5wsnRSLIHZbGihyOrc2zfc9d1FEpe
# aSLYcB4OpiHq2zkjhwFmejaVUL52bZB9Rl2YIv/ineiz74Ft0brF3eEr1dRfRWq6
# qXw3dDSCvv/rFHiNhPl63xU5IZmUPLKNIsFv6IvvsDdE9YlvJ+W4UzwB7s0OSYm7
# 6jszitLnYafC1lkwlFglw0giXWaugjFUsVv/b963EmnmX32Kwy0MGtXM2wPMHSUb
# ZDcnMTNVPKBfnDsxVLzmqHDfhmYaRLX7+WVsGWzZ0MdW5G33/QfbP+wB/GtKRC9m
# p3liZ3rgzN+wrvYg+O0s/ePfqctVFejWLoVrciklpynq2eRISnh5DhRf2V4SB9kt
# vxVMfcOWGdF7hg32824sy8E9dzOY03lupN+vai25IwXkDIJulwJuF/4EpUsslX1C
# tF+HzPiUbgbP4OdezEGYfuK8Bxtco2zZE0q0BKuq23XDUsTFzevy5C3BdT0iqjmj
# LTnFPgwt1PVEhb2uNohnW7hAjre4u+/DXN4FeixC+SG9eEfyWpR8AA8Iod47K8gu
# +T8sWeKzDbLUadcWs8bUC0Q5UL920VifLdgFbjpM2WKGXKBytZVTVmK8SRev
# SIG # End signature block
