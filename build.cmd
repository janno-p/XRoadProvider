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

IF NOT EXIST build.fsx (
  .paket\paket.exe update
  packages\FAKE\tools\FAKE.exe init.fsx
)

del .fake\*_warnings.txt

packages\FAKE\tools\FAKE.exe encoding=utf-8 build.fsx %*
