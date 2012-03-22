Param([int]$which=0)

   if(!(get-command DataSeries -EA 0)){
      $null = [Reflection.Assembly]::LoadWIthPartialName("System.Windows.Forms")  # To get the Double-Click time
      # Add-BootsContentProperty 'DataPoints', 'Series'
      # Add-BootsFunction -Assembly "~\Documents\WindowsPowershell\Libraries\WPFVisifire.Charts.dll"
   }

   function global:New-GraphLabel {
      PARAM ( 
         [String]$Label = "Name", 
         [String]$Value = "Length", 
         [ScriptBlock]$DoubleClickAction = $null, 
         [Int]$max = $null, 
         [Int]$width = 200, 
         [double]$margin = 2,
         [Int]$DoubleClickTime = $([System.Windows.Forms.SystemInformation]::DoubleClickTime),
         $InputObject
      )
      BEGIN { $maxx = $max; $global:DoubleClickTime = $DoubleClickTime }
      PROCESS {
         if($_){ $InputObject = $_ }
         if(!$maxx){ $maxx=@($InputObject)[0].$Value }

         foreach($io in $InputObject) {
            ## This is the core part of the script ...
            ## For each input, generate a grid panel with a label and a rectangle in the background
         
            GridPanel -tag @{item=$io; action=$DoubleClickAction} -width $Width -margin $margin $( 
               Label $io.$Label 
               Rectangle -HorizontalAlignment Left -Fill "#9F00" `
                         -Width ($Width * ($io."$Value" / $maxx))
            ) -On_MouseLeftButtonDown {
               if($this.Tag.Action) { # They passed in a doubleclick action, so lets handle it
                  if($global:ClickTime -and 
                     ([DateTime]::Now - $ClickTime).TotalMilliseconds -lt $DoubleClickTime) {
                     # We invoke the scriptblock 
                     # and pass it the original input object 
                     # and the grid panel object
                     &$This.Tag.Action $this.Tag.Item $this
                  } else {
                     $global:ClickTime = [DateTime]::Now
                  }
               }
            }
         }
      }
   }
   function Get-BootsInput {
      Param([string]$Prompt = "Please enter your name:")
      
      Boots {
         Border -BorderThickness 4 -BorderBrush "#BE8" -Background "#EFC" (
            StackPanel -Margin 10  $( 
               Label $Prompt
               StackPanel -Orientation Horizontal $(
                  TextBox -Width 150 -On_KeyDown { 
                     if($_.Key -eq "Return") { 
                        Write-BootsOutput $global:textbox.Text
                        $BootsWindow.Close()
                     }
                  } | Tee -Variable global:textbox
                  Button "Ok" -On_Click { 
                     Write-BootsOutput $global:textbox.Text
                     $BootsWindow.Close()
                  }
               )
            )
         )
      } -On_Load { Invoke-BootsWindow $global:textbox { $global:textbox.Focus() } } `
      -WindowStyle None -AllowsTransparency `
      -On_PreviewMouseLeftButtonDown { 
         if($_.Source -notmatch ".*\.(TextBox|Button)") 
         {
            $BootsWindow.DragMove() 
         }
      }
   }

   Set-Alias GraphLabel New-GraphLabel -Scope Global

   if(!(gcm Microsoft.PowerShell.Utility\Get-Random -EA 0)) 
   { 
      $global:randor = new-object random
      function global:Get-Random([int]$min,[int]$max=$([int]::MaxValue)){
         if($min) {
            $global:randor.Next($min,$max)
         } else {
            $global:randor.Next($max)
         }
      }
   }
   
switch($which) {
0 { 
@"
This script just runs the various demo scripts I've written to test Boots.
You need to pass it a number (between 1 and 29) for the sample to run!
"@
}
1 {
   New-BootsWindow -SizeToContent WidthAndHeight -Content {
      Button -Content "Push Me" 
   }
}
2 {
   Boots { Button -Content "Push Me" }
}
3 { 
   Boots {
      StackPanel {
         Button "A bed of clams"
         Button "A coalition of cheetas"
         Button "A gulp of swallows"
      }
   }
}
4 {
   Boots { "A bed of clams", "A coalition of cheetas", "A gulp of swallows" | Button | StackPanel }
}
5 {
   Boots { "A bed of clams", "A coalition of cheetas", "A gulp of swallows" | StackPanel | Button }
}
6 {
   Boots {
      StackPanel -Margin 5 -Background Pink $(
         Button -Margin 2 "A bed of clams"
         Button -Margin 2 "A coalition of cheetas"
         Button -Margin 2 "A gulp of swallows"
      )
   }
}
7 {
   Boots { "A bed of clams", "A coalition of cheetas", "A gulp of swallows" | Button -Margin 2 | StackPanel -Margin 5 -Background Pink }
}
8 {
   Boots { Ellipse -Width 60 -Height 80 -Margin "20,10,60,20" -Fill Black }
}
9 {
   Boots {
      Canvas -Height 100 -Width 100 -Children $(
         Rectangle -Margin "10,10,0,0" -Width 45 -Height 45 -Stroke Purple -StrokeThickness 2 -Fill Red
         Polygon -Stroke Pink -StrokeThickness 2 -Fill DarkRed -Points "10,60", "50,60", "50,50", "65,65",
                                                                       "50,80", "50,70", "10,70", "10,60" 
      )
   }
}
10 {
   Boots {
      Image -Source http://data.powerisers.de/gallery/files/855/IMG_3298.jpg -MaxWidth 400 | 
   } -Title "Now those are some powerful boots!" -Async
}
11 {
   Boots {
      StackPanel -Margin 10 -Children $(
         TextBlock "A Question" -FontSize 42 -FontWeight Bold -Foreground "#FF0088" 
         TextBlock -FontSize 24 -Inlines $(
            Bold "Q. "
            "Are you starting to dig "
            Hyperlink "PowerBoots?" -NavigateUri http://huddledmasses.org/tag/powerboots/ `
                                    -On_RequestNavigate { [Diagnostics.Process]::Start( $this.NavigateUri ) }
         )
         TextBlock -FontSize 16 -Inlines $(
            Span -FontSize 24 -FontWeight Bold -Inlines "A. "
            "Leave me alone, I'm hacking here!"
         )
      )
   }
}
12 {
   Boots { 
      $global:Count = 0
      WrapPanel {
         Button "Push Me" -On_Click {
            $global:Count++
            $global:label.Content = "You clicked the button ${global:Count} times!"
         }
         Label "Nothing pushed so far" | Tee -variable global:label
      }
   } -Title "Test App" -On_Closing { $global:BootsOutput = $global:Count; rm variable:Count }
}
13 {
   Boots {
      WrapPanel -On_Load { $Count = 0 }  {
         Button "Push Me" -On_Click {
            Write-BootsOutput (++$count)
            $global:block.Inlines.Clear(); 
            $global:block.Inlines.Add("You clicked the button $count times!") 
         }
         TextBlock "Nothing pushed so far" -VerticalAlignment Center | Tee-Object -Variable global:block 
      }
   }
}
14 {
   ## This syntax only works in PowerSHell 2 running as MTA 
   ## because it requires making a RadialGradientBrush outside the boots thread...
   ## TODO: In a future release, make all the parameters take (optional) scriptblocks to be evaluated in-thread
   #  Boots -Background $(
      #  RadialGradientBrush {
         #  GradientStop -Offset 0 -Color "#F00"
         #  GradientStop -Offset 1 -Color "#F90"
      #  }
   #  ) {
      #  Label "Boots" -HorizontalAlignment Center `
                    #  -VerticalAlignment Center `
                    #  -Foreground White -Margin 80 `
                    #  -FontWeight Bold  -FontSize 40
   #  }
   
   Boots {
      Label "Boots" -HorizontalAlignment Center `
                    -VerticalAlignment Center `
                    -Foreground White -Margin 80 `
                    -FontWeight Bold  -FontSize 40
   } -async -passthru | Invoke-BootsWindow -Element {$_} {
      $BootsWindow.Background = RadialGradientBrush {
         GradientStop -Offset 0 -Color "#F00"
         GradientStop -Offset 1 -Color "#F90"
      }
   }
}
15 {
   Boots {
      TextBox -Width 220 
   } -Title "Enter your name" -On_Closing { 
         Write-BootsOutput $BootsWindow.Content.Text 
   }
}
16 {
   Get-BootsInput
}
17 {
   ## Example 1: list of processes with most RAM usage
   ## DoubleClickAction is `kill`
   Boots {
      ps | sort PM -Desc | Select -First 20 | 
         GraphLabel ProcessName PM { 
            Kill $Args[0].Id -WhatIf
            $global:panel.Children.Remove($Args[1])
         } | 
      StackPanel | Tee -Var global:panel
   }
}
18 {
   ## Example 2: list of images, with file size indicated
   ## DoubleClickAction is `open`
   Boots {
      ls ~/Pictures/ -recurse -Include *.jpg | 
      Select -First 10 | ## For the sake of the demo, just 10
      Sort Length -Desc |
      % {
         if(!$Max){$Max=$_.Length}

         StackPanel -Width 200 -Margin 5 $(
            Image -Source $_.FullName
            GraphLabel Name Length -Max $Max -IO $_ {
               [Diagnostics.Process]::Start( $args[0].FullName )
            }
         ) 
      } | WrapPanel 
   } -Width 800
}
19 {
   Boots {
      Chart -MinWidth 200 -MinHeight 150 -Theme Theme3 {
         DataSeries {
            DataPoint -YValue (Get-Random 100)
            DataPoint -YValue (Get-Random 100)
            DataPoint -YValue (Get-Random 100)
            DataPoint -YValue (Get-Random 100)
         }
      }
   } -Title "Sample, Theme 3"
}
20 {
   [int]$tk    = Invoke-Http get http://google.com/search -with @{q="TCL Tk"} |
                 Receive-Http Text "//div[@id='ssb']//b[3]"
   [int]$shoes = Invoke-Http get http://google.com/search -with @{q="Ruby Shoes"} |
                 Receive-Http Text "//div[@id='ssb']//b[3]"
   [int]$boots = Invoke-Http get http://google.com/search -with @{q="PowerShell PowerBoots"} |
                 Receive-Http Text "//div[@id='ssb']//b[3]"
   Boots {
      Chart -MinHeight 300 -MinWidth 400 {
         DataSeries -RenderAs Bar {
            DataPoint -YValue $tk    -AxisXLabel Tk    -Href http://google.com/search?q=TCL+Tk
            DataPoint -YValue $shoes -AxisXLabel Shoes -Href http://google.com/search?q=Ruby+Shoes
            DataPoint -YValue $boots -AxisXLabel Boots -Href http://google.com/search?q=PowerSHell+PowerBoots
         }
      }
   }
}
21 {
   $ad=New-Object DirectoryServices.DirectorySearcher [ADSI]''
   # Set a limit or TimeOut, PageSize lets us get more later
   $ad.PageSize = 200

   # ADSI field names are awful.
   # l = location, l=* returns only users with locations set
   $ad.Filter = "(&(objectClass=Person)(l=*))"  
   $results = $ad.FindAll().GetEnumerator() | ForEach { $_ }
   $users   = $results | ForEach { $_.GetDirectoryEntry() }

   # "l" is a PropertyValueCollection, use the first value
   $users | Group-Object {$_.l[0]}  | ForEach { 
      DataPoint -YValue ([int]$_.Count) -AxisXLabel $_.Name 
   }| DataSeries -RenderAs Doughnut | 
      Chart -Height 300 -Width 300  | 
      Boots -Title "AD Users by Location"
}
22 {
   Boots {
   ls | ForEach { 
      DataPoint -YValue ([DateTime]::Now - $_.LastWriteTime).TotalDays `
                -ZValue ($_.Length/1KB) `
                -AxisXLabel $_.Name -Tag $_ `
                -On_MouseLeftButtonUp { 
                  if($this.Tag) { 
                     Write-BootsOutput $this.Tag; 
                     $global:series.DataPoints.Remove($this)
                  }
               }
   } | DataSeries -RenderAs Bubble -ToolTipText "#AxisXLabel`nAge: #YValue days, Size: #ZValue Kb" | 
      Tee-Object -Variable global:series |
      Chart -MinHeight 350 -MinWidth 600 -Theme Theme3 
   } | Remove-Item -Confirm
}
23 {
   # Write-Host "We're going to ask for your password here, so we can upload an image via FTP"
   # $credential = Get-Credential

   if($PsVersionTable) {
      ## BUG BUG: Setting boolan properties isn't working in PowerShell 1
      Write-Host "Using PowerShell 2 Version" -Fore Cyan
      New-BootsImage VisiFire-BootsImage.jpg {
         Chart -Width 200 -Height 150 -Theme Theme3 -Watermark:$false -Animation:$false -Series {
            DataSeries {
               1..(Get-Random -min 3 -max 6) | ForEach-Object  {
                  DataPoint -YValue (Get-Random 100)
               }
            }
         }
      }   #| ForEach-Object { 
      #   Send-FTP HuddledMasses.org $credential -LocalFile $_ -Remotefile "$imgPath/$($_.Name)" 
      #   [Windows.Clipboard]::SetText( "!http://huddledmasses.org/images/PowerBoots/$($_.Name)!" )
      #}
   } else {
      Write-Host "Using PowerShell 1 Version" -Fore Cyan
      Boots -Title "ScreenCapWindow" {
         Chart -Width 200 -Height 150 -Theme Theme3 -Watermark:$false -Animation:$false -Series {
            DataSeries {
               1..(Get-Random -min 3 -max 6) | ForEach-Object  {
                  DataPoint -YValue (Get-Random 100)
               }
            }
         } | tee -var global:chart  
      } -Async
      sleep 5
      Export-BootsImage VisiFire-BootsImage.jpg $global:chart
      Remove-BootsWIndow "ScreenCapWindow" 
   }
}
24 {
# Write-Host "We're going to ask for your password here, so we can upload an image via FTP"
# $credential = Get-Credential
   New-BootsImage BootsImage-Screenshot.jpg {
      StackPanel -Margin "10,5,10,5" {
         Label "Please enter your name:"
         StackPanel -Orientation Horizontal {
            TextBox -Width 150 -On_KeyDown { 
               if($_.Key -eq "Return") { 
                  Write-BootsOutput $global:textbox.Text
                  $BootsWindow.Close()
               }
            } | Tee-Object -Variable global:textbox 
            Button "Ok" -Padding "5,0,5,0" -Margin "2,0,0,0" -On_Click { 
               Write-BootsOutput $textbox.Text
               $BootsWindow.Close()
            }
         }
      } 
   }      #| ForEach-Object { 
      #Send-FTP HuddledMasses.org $credential -LocalFile $_ -Remotefile "$imgPath/$($_.Name)" 
      #[Windows.Clipboard]::SetText( "!http://huddledmasses.org/images/PowerBoots/$($_.Name)!" )
   #}
}
25 {
   Boots -Async {
      StackPanel -Margin 10 {
         TextBlock "The Question" -FontSize 42 -FontWeight Bold -Foreground "#FF0088"
         TextBlock -FontSize 24 {
            Hyperlink {
               Bold "Q. "
               "Can PowerBoots do async threads?"
            } -NavigateUri " " -On_RequestNavigate {
               $global:Answer.Visibility = "Visible" 
            }
         }
         TextBlock -FontSize 16 {
            Span "A. " -FontSize 24 -FontWeight Bold 
            "Oh yes we can!"
         } -Visibility Collapsed | Tee -Variable global:Answer
      }
   } 
}
26 {
   # This works with just the PoshWpf snapin
   $global:Splash = New-BootsWindow -Async -Passthru -Content {$Args[0].Content} -On_MouseDown { $this.DragMove() } -SourceTemplate '<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation" WindowStyle="None" AllowsTransparency="True" Opacity="0.8" Topmost="True" SizeToContent="WidthAndHeight" WindowStartupLocation="CenterOwner" ShowInTaskbar="False"><Image Source="http://dilbert.com/dyn/str_strip/000000000/00000000/0000000/000000/40000/1000/200/41215/41215.strip.print.gif" Height="177" /></Window>' 
   # Imagine this is your script, working ...
   Start-Sleep 3
   # And now you're done, and want to close it
   Invoke-BootsWindow $Splash { $global:Splash.Close() }
}
27 {
   ## This requires PowerBoots which is (as of this writing) is still v2 only...
   $global:Splash = Boots -Async -Passthru -Content { 
      Image -Height 177 -Source http://dilbert.com/dyn/str_strip/000000000/00000000/0000000/000000/40000/1000/200/41215/41215.strip.print.gif
   } -WindowStyle None -AllowsTransparency -Opacity 0.8 -Topmost -WindowStartupLocation CenterOwner -ShowInTaskbar:$False -On_MouseDown { $this.DragMove() }

   # Imagine this is your script, working ...
   Start-Sleep 3
   # And now you're done, and want to close it
   Invoke-BootsWindow $Splash { $global:Splash.Close() }
}
28 {
   Write-Host "Initializing Performance Counters, please have patience" -fore Cyan
   ### Import PoshWpf module
   Import-Module PowerBoots
   ### Or, on v1:
   # Add-PSSnapin PoshWpf

   $global:cpu = new-object System.Diagnostics.PerformanceCounter "Processor", "% Processor Time", "_Total"
   $global:ram = new-object System.Diagnostics.PerformanceCounter "Memory", "Available KBytes"

   ## get initial values, because the counters don't work until the second call
   $null = $global:cpu.NextValue()
   $null = $global:ram.NextValue()
   $global:maxram = (gwmi Win32_OperatingSystem).TotalVisibleMemorySize

   Write-Host "Loading XAML window..." -fore Cyan
   ## Load the XAML and show the window. It won't be updating itself yet...
   $global:clock = New-BootsWindow -Async -Passthru -Content { $Args[0].Content } -FileTemplate "$PowerBootsPath\Samples\clock.xaml" 

   ## Create a script block which will update the UI by changing the Resources!
   $counter = 0;
   $global:updateBlock = {
      # Update the clock
      $global:clock.Resources["Time"] = [DateTime]::Now.ToString("hh:MM.ss")

      # We only want to update the counters at most once a second
      # Otherwise their values are invalid and ...
      # The CPU counter fluctuates from 0 to the real number
      if( $counter++ -eq 4 ) {
         $counter = 0
         # Update the CPU counter with the absolute value and the percentage
         $cu = $global:cpu.NextValue()
         $global:clock.Resources.CpuP = ($cu / 100)
         $global:clock.Resources.Cpu = "{0:0.0}%" -f $cu
         # Update the RAM counter with the absolute value and the percentage
         $rm = $global:ram.NextValue()
         $global:clock.Resources.RamP = ($rm / $global:maxram)
         $global:clock.Resources.Ram = "{0:0.00}Mb" -f ($rm/1MB)
         }
   }

   ## Now we need to call that scriptblock on a timer. That's easy, but it
   ## must be done on the window's thread, so we use Invoke-BootsWindow.
   ## Notice the first argument is the window we want to run the script in
   Invoke-BootsWindow $clock {
      ## We'll create a timer
      $global:timer = new-object System.Windows.Threading.DispatcherTimer
      ## Which will fire 4 times every second
      $timer.Interval = [TimeSpan]"0:0:0.25"
      ## And will invoke the $updateBlock
      $timer.Add_Tick( $global:updateBlock )
      ## Now start the timer running
      $timer.Start()
   }

   ## And just like that, the $UpdateBlock is running 4x a second
   ## and the clock is working.  Pretty cool, right?
   #
   #  ## If we wanted to, say, handle mouse events to let you drag the window or close it ...
   #  Invoke-BootsWindow $clock {
      #  $clock.Add_MouseLeftButtonDown( {
         #  $_.Handled = $true
         #  $clock.DragMove() # WPF Magic!
      #  } )
      #  $clock.Add_MouseRightButtonDown( {
         #  $_.Handled = $true
         #  $timer.Stop()  # we'd like to stop that timer now, thanks.
         #  $clock.Close() # and close the windows
      #  } )
   #  }

   Register-BootsEvent $clock -Event MouseLeftButtonDown -Action {
      $_.Handled = $true
      $clock.DragMove() # WPF Magic!
   }
   Register-BootsEvent $clock -Event MouseRightButtonDown -Action {
      $_.Handled = $true
      $timer.Stop()  # we'd like to stop that timer now, thanks.
      $clock.Close() # and close the windows
   }
}

}
# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUTDTaCJLXpXyxb6a8QFdj/vUQ
# fPCgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFKUABmyCqDvz845MGq7Vwjt2FqgcMA0GCSqG
# SIb3DQEBAQUABIICAAa0SfszAKnyNtUj8J4/QxAfciGNi19uZc6tXYiSmkz0AriS
# KtLq9hqRcfiL/ypHc1WNtaPEAaeUEcWnUT1KZO4XmI0I81iGf/O9OO2+Khav7vkJ
# 4OolvhA+JOZxsC8TDRY5YI49Q2QBZYw19uEsUrugCBk4lp0+YtCt33GjiQQmdMhi
# ClUyzb3bS8A2f9IvjDYWMyM8BLjIcldmk3qJQEOxjKdatutz4FVL1/LSDB4Qz1cR
# X9alxUohkm7pfp6FC/YJm8eQiRviDx1SleZVsH8bhIF+k4NjF+9+a+xTa5ZTm36W
# JEMA5grYoqrf8G9v+8QqqhEm+MlsP92aYmcXUkzz8c6q9A11PSASgJ/ohm/huqcj
# JSDVrrMSgUR9Y72Y/5t+D76XywYthX4oUHAcd4n59b1/uEr8YCxCnlu5NrbTnLq7
# iANVFOqZgmm+QOaTa86tWvfVxVBtdZcii4h1qvZWI9L8kGlXIN1dlhMcuHu+FatE
# sXpPzRg0SWtud5aE3AKWn8mvYOn4Pw7so9qrK2FI02w36G2FdsEisFRCzqxv5rmQ
# Gtw93mo5T2n7UC7n0ARaz7ClD0JKUeQILxvqJZhzMDbmcGH2okQ5eP9hMqr2xyvy
# MjaBfZbmkgMVdDK9j9s5nCvGwtVrEnqIgQjDKJ/C0NghQylKTbGdEzdp4jNA
# SIG # End signature block
