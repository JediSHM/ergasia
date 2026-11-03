$ErrorActionPreference = "Stop"

$projectDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$rscriptPath = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
$taskScript = Join-Path $projectDirectory "0_Get data.R"
$progressScript = Join-Path $projectDirectory "checkpoint_progress.R"
$checkpointPath = Join-Path $projectDirectory "data_raw\crawl_checkpoint.rds"
$logDirectory = Join-Path $projectDirectory "data_raw"
$restartLimit = 100
$waitSeconds = 60
$staleMinutes = 15
$pollSeconds = 10

if (-not (Test-Path -LiteralPath $rscriptPath)) {
  throw "Rscript.exe was not found at $rscriptPath. Update rscriptPath in this file."
}
if (-not (Test-Path -LiteralPath $taskScript)) {
  throw "Could not find 0_Get data.R in $projectDirectory."
}
if (-not (Test-Path -LiteralPath $progressScript)) {
  throw "Could not find checkpoint_progress.R in $projectDirectory."
}

Set-Location -LiteralPath $projectDirectory
New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null

function Get-CheckpointProgress {
  if (-not (Test-Path -LiteralPath $checkpointPath)) { return 0 }
  $text = & $rscriptPath $progressScript $checkpointPath 2>&1
  if ($LASTEXITCODE -ne 0) { return -1 }
  $value = 0
  $resultText = (($text | Out-String).Trim())
  if ([int]::TryParse($resultText, [ref]$value)) { return $value }
  return -1
}

$lastProgress = Get-CheckpointProgress
$noProgressRestarts = 0

for ($attempt = 1; $attempt -le $restartLimit; $attempt++) {
  Write-Host "Starting Task 0 run $attempt of $restartLimit from the saved checkpoint..."
  $attemptTag = $attempt.ToString("000")
  $logPath = Join-Path $logDirectory ("supervisor_rscript_run_{0}.log" -f $attemptTag)
  $errorLogPath = $logPath + ".err"

  $process = Start-Process -FilePath $rscriptPath `
    -ArgumentList @('"' + $taskScript + '"') `
    -WorkingDirectory $projectDirectory `
    -RedirectStandardOutput $logPath `
    -RedirectStandardError $errorLogPath `
    -PassThru

  $lastCheckpointWrite = if (Test-Path -LiteralPath $checkpointPath) {
    (Get-Item -LiteralPath $checkpointPath).LastWriteTime
  } else { Get-Date }
  $lastDisplayedLineCount = 0
  $lastDisplayedErrorLineCount = 0
  $timedOut = $false

  while (-not $process.HasExited) {
    Start-Sleep -Seconds $pollSeconds
    $process.Refresh()

    if (Test-Path -LiteralPath $logPath) {
      $lines = @(Get-Content -LiteralPath $logPath -ErrorAction SilentlyContinue)
      if ($lines.Count -gt $lastDisplayedLineCount) {
        for ($i = $lastDisplayedLineCount; $i -lt $lines.Count; $i++) { Write-Host $lines[$i] }
        $lastDisplayedLineCount = $lines.Count
      }
    }
    if (Test-Path -LiteralPath $errorLogPath) {
      $errorLines = @(Get-Content -LiteralPath $errorLogPath -ErrorAction SilentlyContinue)
      if ($errorLines.Count -gt $lastDisplayedErrorLineCount) {
        for ($i = $lastDisplayedErrorLineCount; $i -lt $errorLines.Count; $i++) { Write-Host $errorLines[$i] }
        $lastDisplayedErrorLineCount = $errorLines.Count
      }
    }

    if (Test-Path -LiteralPath $checkpointPath) {
      $checkpointWrite = (Get-Item -LiteralPath $checkpointPath).LastWriteTime
      if ($checkpointWrite -gt $lastCheckpointWrite) { $lastCheckpointWrite = $checkpointWrite }
    }

    $minutesWithoutCheckpoint = ((Get-Date) - $lastCheckpointWrite).TotalMinutes
    if ($minutesWithoutCheckpoint -ge $staleMinutes) {
      Write-Host "No checkpoint update for $staleMinutes minutes. Stopping this R process and resuming from the last saved checkpoint."
      Stop-Process -Id $process.Id -Force
      $timedOut = $true
      break
    }
  }

  if (-not $timedOut) {
    $process.WaitForExit()
    if (Test-Path -LiteralPath $logPath) {
      $lines = @(Get-Content -LiteralPath $logPath -ErrorAction SilentlyContinue)
      if ($lines.Count -gt $lastDisplayedLineCount) {
        for ($i = $lastDisplayedLineCount; $i -lt $lines.Count; $i++) { Write-Host $lines[$i] }
      }
    }
    if (Test-Path -LiteralPath $errorLogPath) {
      $errorLines = @(Get-Content -LiteralPath $errorLogPath -ErrorAction SilentlyContinue)
      if ($errorLines.Count -gt $lastDisplayedErrorLineCount) {
        for ($i = $lastDisplayedErrorLineCount; $i -lt $errorLines.Count; $i++) { Write-Host $errorLines[$i] }
      }
    }
  }

  $exitCode = if ($timedOut) { 124 } else { $process.ExitCode }
  $captchaBlocked = $false
  if (Test-Path -LiteralPath $errorLogPath) {
    $captchaBlocked = Select-String -LiteralPath $errorLogPath `
      -Pattern "IMDb_CAPTCHA_BLOCKED" -Quiet
  }
  if ($captchaBlocked) {
    Write-Host "Stopped because IMDb requires a human CAPTCHA check. The checkpoint is saved and the current title remains pending."
    Write-Host "Do not restart the crawler until IMDb access is available again."
    break
  }

  if ($exitCode -eq 0) {
    Write-Host "Rscript exited normally. Review crawl_queue_raw.csv and member coverage."
    break
  }

  Write-Host "Rscript stopped with code $exitCode. Checking saved progress..."
  $currentProgress = Get-CheckpointProgress
  if ($currentProgress -gt $lastProgress) {
    $noProgressRestarts = 0
  } else {
    $noProgressRestarts++
  }
  $lastProgress = $currentProgress

  if ($noProgressRestarts -ge 3) {
    Write-Host "Stopped: three restarts in a row made no new checkpoint progress. Review the error and checkpoint."
    break
  }

  Write-Host "Waiting $waitSeconds seconds, then launching a fresh R process. Press Ctrl+C to stop."
  Start-Sleep -Seconds $waitSeconds
}

Write-Host "Supervisor finished. Press Enter to close this window."
[void](Read-Host)
