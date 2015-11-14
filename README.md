# XRoadProvider

A library containing collection of F# type providers to support easier integration with service providers over
[X-Road](http://x-road.eu) infrastructure.

[![Mono build status](https://img.shields.io/travis/janno-p/XRoadProvider/master.svg?label=Mono build)]()  
[![Windows build status](https://img.shields.io/appveyor/ci/janno-p/xroadprovider/master.svg?label=Windows build)]()  
[![Version](http://img.shields.io/nuget/v/XRoadProvider.svg?label=Current version)](https://www.nuget.org/packages/XRoadProvider/)  
[![Downloads](https://img.shields.io/nuget/dt/XRoadProvider.svg?label=Downloads)]()

## Documentation

Documentation and samples can be found at the [XRoadProvider home page](http://janno-p.github.com/XRoadProvider/).

## Building

* Mono: Run `build.sh`
* Windows: Run `build.cmd`

## Known Issues

* Underlying `XmlSerializer` needs to have access to all generated types. Because of that, type provider definitions should
  be generated at namespace level (inside F# modules they will compile, but at the runtime you might get exceptions).

## Disclaimer

This is an alpha build and as such most likely has problems that are yet undetected. That means the solution is not suitable
for use in production environment. I will not hold responsibility for any damage caused by this software.

## Maintainer(s)

* [@janno-p](https://github.com/janno-p)
