# Save the starting location
$startingDir = Get-Location

# Path to the shared file listing the repositories
$repoFile = "depends.txt"

# Read the file and process each line
Get-Content $repoFile | ForEach-Object {
    $repoInfo = $_ -split ","
    $repoUrl = $repoInfo[0]
    $targetDir = $repoInfo[1]

    if (Test-Path $targetDir) {
        Write-Host "Updating $targetDir..."
        Set-Location $targetDir
        git pull
    } else {
        Write-Host "Cloning $repoUrl into $targetDir..."
        git clone $repoUrl $targetDir
    }

    # Return to the starting directory
    Set-Location $startingDir
}
