$URL = "com.epicgames.launcher://apps/5e834b926aa547029903e0ae4f649a70%3Aa22fd963146146b1b0f6e0501be37693%3Ab1940ae77a284721957af7515b60308c?action=launch&silent=true"

Write-Output "Starting game..."
Start-Process $URL
Write-Output "Sleeping..."
sleep -seconds 30
Write-Output "Fetching process..."
$process = Get-Process $URL
Write-Output "Waiting for exit..."
$process.WaitForExit()