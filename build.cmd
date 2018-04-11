@echo off
cls

.paket\paket.exe restore
if errorlevel 1 (
  .paket\paket.bootstrapper.exe prerelease
  if errorlevel 1 (
    exit /b %errorlevel%
  )
  else (
    .paket\paket.exe restore
    if errorlevel 1 (
      exit /b %errorlevel%
    )
  )
)

del .fake\*_warnings.txt

packages\FAKE\tools\FAKE.exe --removeLegacyFakeWarning build.fsx %* encoding=utf-8
