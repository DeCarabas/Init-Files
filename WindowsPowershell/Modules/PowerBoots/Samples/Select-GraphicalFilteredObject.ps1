## Select-GraphicalFilteredObject.ps1
## Use a graphical interface to select (and pass-through) pipeline objects
## by Lee Holmes (http://www.leeholmes.com/blog)

Import-Module PowerBoots

## Get the item as it would be displayed by Format-Table
function Get-StringRepresentation($object)
{
    $formatted = $object | Format-Table | Out-String
    $formatted -split '\r\n' | ? { $_ } | Select -Last 1
}

## Store the items that came from the pipeline
$GLOBAL:items = New-Object "System.Collections.ObjectModel.ObservableCollection``1[[System.Object, mscorlib]]"
$GLOBAL:originalItems = @{}
$GLOBAL:orderedItems = New-Object System.Collections.ArrayList

## Convert input to string representations
@($input) | % {
    $stringRepresentation = Get-StringRepresentation $_

    $GLOBAL:originalItems[$stringRepresentation] = $_
    $null = $orderedItems.Add($stringRepresentation)
    $GLOBAL:items.Add($stringRepresentation)
}

## Send the selected items down the pipeline
function global:OK_Click
{
    $selectedItems = Select-BootsElement "Object Filter" SelectedItems
    $source = $selectedItems.Items
    
    if($selectedItems.SelectedIndex -ge 0)
    {
        $source = $selectedItems.SelectedItems
    }

    $source | % { $GLOBAL:originalItems[$_] } | Write-BootsOutput
    $bootsWindow.Close()
}

## Send the selected items down the pipeline
function global:SelectedItems_DoubleClick
{
    $item = $args[1].OriginalSource.DataContext
    $GLOBAL:originalItems[$item] | Write-BootsOutput
    $bootsWindow.Close()
}

## Filter selected items to what's been typed
function global:SearchText_KeyUp
{
    if($this.Text)
    {
        $items.Clear()
        try
        {
            ## If this is a regex, do a regex match
            $orderedItems | ? { $_ -match $this.Text } | % { $items.Add($_) }
        }
        catch
        {
            ## If the regex threw, do simple text match
            $items.Clear()
            $orderedItems | 
                ? { $_ -like ("*" + [System.Management.Automation.WildcardPattern]::Escape($this.Text) + "*") } |
                    % { $items.Add($_) }
        }
    }
}

## Generate the window
Boots -Title "Object Filter" -MinWidth 400 -MaxWidth 1000 -Height 600 {
    GridPanel -Margin 5 -RowDefinitions @(
        RowDefinition -Height Auto
        RowDefinition -Height *
        RowDefinition -Height Auto
        RowDefinition -Height Auto
    ) {
        TextBlock -Margin 5 -Row 0 {
            "Type or click to search. Press Enter or click OK to pass the items down the pipeline." }
        
        ScrollViewer -Margin 5 -Row 1 {
            ListBox -SelectionMode Multiple -Name SelectedItems -FontFamily "Courier New" `
                -ItemsSource (,$items) -On_MouseDoubleClick  SelectedItems_DoubleClick
        } 
        
        TextBox -Margin 5 -Name SearchText -On_KeyUp SearchText_KeyUp -Row 2
        
        GridPanel -Margin 5 -HorizontalAlignment Right -ColumnDefinitions @(
            ColumnDefinition -Width 65
            ColumnDefinition -Width 10
            ColumnDefinition -Width 65
        ) {
            Button "OK" -IsDefault -Width 65 -On_Click OK_Click -"Grid.Column" 0
            Button "Cancel" -IsCancel -Width 65 -"Grid.Column" 2
        } -"Grid.Row" 3 -Passthru
        
   } -On_Loaded { (Select-BootsElement $this SearchText).Focus() }
}
Remove-BootsWindow "Object Filter"

# SIG # Begin signature block
# MIILCQYJKoZIhvcNAQcCoIIK+jCCCvYCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUPJz6PEWsj4tA+FFw0Zi/3a6d
# 4IqgggbgMIIG3DCCBMSgAwIBAgIJALPpqDj9wp7xMA0GCSqGSIb3DQEBBQUAMIHj
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
# gjcCARUwIwYJKoZIhvcNAQkEMRYEFEgDM/71S5/NomVzcgzQPihDM51nMA0GCSqG
# SIb3DQEBAQUABIICABrKzwBt3MY1rYBf1uyvGAZ6fGUXaFpZOYS/VVxGX10R2Jaa
# qk/v2e15Ed7uJRkIVYFPOmmx8Bv2BrhdJDExp1jVMOvsKs37Lp4rxJsj5fNxectz
# qQyxDanEvtd+6OD5xb8NpZdxXFIRR58VdZa22RJmZwgGdn6DDbtMd7HRmmwE65qU
# /ODQsTG8Qbr39w4qziOhKdG3b0f5xkgX/VwoPynAHTnOdKUK+mLArypCofwereMm
# QD9Fh2Iguxkm4LewadFG8c5MezRW7mrDbJDVRpvBr3psyHyrI9oayYuBE3IUIHwb
# mBTmVxbKM/LwG3V1UY9GCKrtXCy31xjOGRt92vWalxPvsA8feBLw1FakeG62GXRG
# IKskAZwRQ3brRpcsaN20CqXe+/xWvV57lw/1WcSrIyV3LwqvP3Ow9+1pDtkeOr6M
# HDcHivc5BAdAe3RjD6qMQYNXbTUmSTlUeQNIkO2CVfnSuV15oM8kkoHn4rjAWhWQ
# 0AFe/kMtwoOfHDxVQ0s8ypQK80PsRZMoYcbxAjq2pW09ZakxOoKNyVPccMjVqIQ2
# /HH1Wi1SCgpYUaifoZBfuAfjBOSInHSk97DIDd8hxgan0rTEycif6NpMldu9jAdv
# HTy6kwDfvU81noiWd8COKOuriv24sWj8PQ7ZLwtL9urqi3dQ5BiB90Vc2TGY
# SIG # End signature block
