[CmdletBinding()]
param(
    [string]$VcRuntimePath,
    [string]$SigningCertificate,
    [string]$TimestampServer = "http://timestamp.digicert.com",
    [string]$UpxPath,
    [switch]$SkipBuild,
    [switch]$SkipUpx
)

$ErrorActionPreference = "Stop"

$projectRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot ".."))
$releaseRoot = [System.IO.Path]::GetFullPath((Join-Path $projectRoot "dist\release"))
$releasePrefix = $releaseRoot.TrimEnd('\') + '\'
$stage = [System.IO.Path]::GetFullPath((Join-Path $releaseRoot "midivis-windows-x64"))
$archive = Join-Path $releaseRoot "midivis-windows-x64.zip"

if (-not $stage.StartsWith($releasePrefix, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Refusing to use a staging path outside dist\release: $stage"
}

function Find-VcRuntime {
    param([string]$ExplicitPath)

    if ($ExplicitPath) {
        $resolved = (Resolve-Path -LiteralPath $ExplicitPath -ErrorAction Stop).Path
        if (-not $resolved.EndsWith("\vcruntime140.dll", [System.StringComparison]::OrdinalIgnoreCase)) {
            throw "VcRuntimePath must point to vcruntime140.dll: $resolved"
        }
        return $resolved
    }

    $redistRoots = @()
    if ($env:VCToolsRedistDir) {
        $redistRoots += $env:VCToolsRedistDir
    }

    $vswhere = Join-Path ${env:ProgramFiles(x86)} "Microsoft Visual Studio\Installer\vswhere.exe"
    if (Test-Path -LiteralPath $vswhere) {
        $installations = & $vswhere -all -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
        foreach ($installation in $installations) {
            $redistRoots += Join-Path $installation "VC\Redist\MSVC"
        }
    }

    $candidates = foreach ($root in $redistRoots) {
        if (Test-Path -LiteralPath $root) {
            Get-ChildItem -LiteralPath $root -Recurse -File -Filter "vcruntime140.dll" |
                Where-Object { $_.FullName -match '\\x64\\Microsoft\.VC[^\\]*CRT\\vcruntime140\.dll$' }
        }
    }

    $runtime = $candidates | Sort-Object FullName -Descending | Select-Object -First 1
    if (-not $runtime) {
        throw "Could not locate the Microsoft x64 VC runtime. Install the Visual C++ build tools or pass -VcRuntimePath."
    }
    return $runtime.FullName
}

function Find-Upx {
    param([string]$ExplicitPath)

    if ($ExplicitPath) {
        return (Resolve-Path -LiteralPath $ExplicitPath -ErrorAction Stop).Path
    }
    if ($env:UPX_PATH) {
        return (Resolve-Path -LiteralPath $env:UPX_PATH -ErrorAction Stop).Path
    }

    $candidate = "I:\Data\upx-5.2.0-win64\upx.exe"
    if (Test-Path -LiteralPath $candidate) {
        return (Resolve-Path -LiteralPath $candidate).Path
    }

    $command = Get-Command upx -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($command) {
        return $command.Source
    }
    throw "Could not locate upx.exe. Pass -UpxPath or set UPX_PATH."
}

Push-Location $projectRoot
try {
    if (-not $SkipBuild) {
        $buildLog = Join-Path $projectRoot "dist-newstyle\package-build.log"
        cabal build "--build-log=$buildLog" exe:midivis
        if ($LASTEXITCODE -ne 0) {
            throw "cabal build exe:midivis failed with exit code $LASTEXITCODE"
        }
    }

    $exe = (& cabal list-bin exe:midivis | Select-Object -Last 1).Trim()
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $exe)) {
        throw "Could not locate the built midivis executable."
    }
    $exe = (Resolve-Path -LiteralPath $exe).Path
    $buildDir = Split-Path -Parent $exe

    $freeglut = Join-Path $buildDir "freeglut.dll"
    if (-not (Test-Path -LiteralPath $freeglut)) {
        $freeglut = Join-Path $projectRoot "freeglut\bin\x64\freeglut.dll"
    }
    if (-not (Test-Path -LiteralPath $freeglut)) {
        throw "Could not locate the x64 freeglut.dll runtime."
    }

    $vcRuntime = Find-VcRuntime -ExplicitPath $VcRuntimePath

    New-Item -ItemType Directory -Path $releaseRoot -Force | Out-Null
    if (Test-Path -LiteralPath $stage) {
        Remove-Item -LiteralPath $stage -Recurse -Force
    }
    New-Item -ItemType Directory -Path $stage | Out-Null

    $stagedExe = Join-Path $stage "midivis.exe"
    Copy-Item -LiteralPath $exe -Destination $stagedExe
    Copy-Item -LiteralPath $freeglut -Destination (Join-Path $stage "freeglut.dll")
    Copy-Item -LiteralPath $vcRuntime -Destination (Join-Path $stage "vcruntime140.dll")
    Copy-Item -LiteralPath (Join-Path $projectRoot "LICENSE") -Destination (Join-Path $stage "LICENSE")
    Copy-Item -LiteralPath (Join-Path $projectRoot "packaging\WINDOWS-README.txt") `
        -Destination (Join-Path $stage "README.txt")

    $uncompressedBytes = (Get-Item -LiteralPath $stagedExe).Length
    $upxUsed = $false
    if (-not $SkipUpx) {
        $upx = Find-Upx -ExplicitPath $UpxPath
        & $upx --best --lzma --no-color $stagedExe
        if ($LASTEXITCODE -ne 0) {
            throw "UPX compression failed with exit code $LASTEXITCODE"
        }
        & $upx -t $stagedExe | Out-Null
        if ($LASTEXITCODE -ne 0) {
            throw "UPX verification failed with exit code $LASTEXITCODE"
        }
        $upxUsed = $true
    }

    if ($SigningCertificate) {
        $signtool = Get-ChildItem "${env:ProgramFiles(x86)}\Windows Kits\10\bin" `
            -Recurse -File -Filter "signtool.exe" |
            Where-Object { $_.FullName -match '\\x64\\signtool\.exe$' } |
            Sort-Object FullName -Descending |
            Select-Object -First 1
        if (-not $signtool) {
            throw "signtool.exe was not found. Install the Windows SDK to sign the release."
        }
        & $signtool.FullName sign /sha1 $SigningCertificate /fd SHA256 /tr $TimestampServer /td SHA256 `
            (Join-Path $stage "midivis.exe")
        if ($LASTEXITCODE -ne 0) {
            throw "Authenticode signing failed with exit code $LASTEXITCODE"
        }
    }

    $files = @("midivis.exe", "freeglut.dll", "vcruntime140.dll", "LICENSE", "README.txt")
    foreach ($file in $files) {
        $path = Join-Path $stage $file
        if (-not (Test-Path -LiteralPath $path)) {
            throw "Release is missing required file: $file"
        }
    }

    $hashes = foreach ($file in $files) {
        $hash = Get-FileHash -LiteralPath (Join-Path $stage $file) -Algorithm SHA256
        "{0}  {1}" -f $hash.Hash.ToLowerInvariant(), $file
    }
    Set-Content -LiteralPath (Join-Path $stage "SHA256SUMS.txt") -Value $hashes -Encoding ascii

    if (Test-Path -LiteralPath $archive) {
        Remove-Item -LiteralPath $archive -Force
    }
    Compress-Archive -Path (Join-Path $stage "*") -DestinationPath $archive -CompressionLevel Optimal

    $signature = Get-AuthenticodeSignature -LiteralPath (Join-Path $stage "midivis.exe")
    [pscustomobject]@{
        Stage = $stage
        Archive = $archive
        UncompressedBytes = $uncompressedBytes
        ExecutableBytes = (Get-Item -LiteralPath (Join-Path $stage "midivis.exe")).Length
        UpxCompressed = $upxUsed
        ArchiveSha256 = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash.ToLowerInvariant()
        Signature = $signature.Status
    } | Format-List

    if ($signature.Status -ne "Valid") {
        Write-Warning "The executable is not Authenticode-signed. Pass -SigningCertificate for a production release."
    }
}
finally {
    Pop-Location
}
