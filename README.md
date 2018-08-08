# XRoadProvider

A library containing collection of F# type providers to support easier integration with service providers over
[X-Road](http://x-road.eu) infrastructure.

[![NuGet](https://buildstats.info/nuget/XRoadProvider?includePreReleases=true)](https://www.nuget.org/packages/XRoadProvider/)

## Build Status

[![Mono build status](https://img.shields.io/travis/janno-p/XRoadProvider/master.svg?label=Mono%20build)](https://travis-ci.org/janno-p/XRoadProvider/)  
[![Windows build status](https://img.shields.io/appveyor/ci/janno-p/xroadprovider/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/janno-p/xroadprovider)  

## Documentation

Documentation and samples can be found at the [XRoadProvider home page](http://janno-p.github.com/XRoadProvider/).

## Prerequisites

* Restore 3rd party dependencies:

  **Mono**: Run `$ mono ./.paket/paket.exe restore`  
  **Windows**: Run: `> .paket\paket.exe restore`  

* Install FAKE dotnet SDK global tool:

  ```sh
  dotnet tool install fake-cli -g
  ```

## Building

```sh
fake run build.fsx
```

## Disclaimer

This is an alpha build and as such most likely has problems that are yet undetected. That means the solution is not suitable
for use in production environment. I will not hold responsibility for any damage caused by this software.

## Maintainer(s)

* [@janno-p](https://github.com/janno-p)
