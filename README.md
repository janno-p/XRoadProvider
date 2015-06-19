[![Issue Stats](http://issuestats.com/github/janno-p/XRoadProvider/badge/issue)](http://issuestats.com/github/janno-p/XRoadProvider)
[![Issue Stats](http://issuestats.com/github/janno-p/XRoadProvider/badge/pr)](http://issuestats.com/github/janno-p/XRoadProvider)

# XRoadProvider [![NuGet Status](http://img.shields.io/nuget/v/XRoadProvider.svg?style=flat)](https://www.nuget.org/packages/XRoadProvider/)

A library containing collection of F# type providers to support easier integration with service providers over
[X-Road](http://x-road.eu) infrastructure.

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
