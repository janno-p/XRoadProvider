![XRoadProvider](images/logo.png)

# XRoadProvider #

[![NuGet](https://buildstats.info/nuget/XRoadProvider?includePreReleases=true)](https://www.nuget.org/packages/XRoadProvider/)
[![Mono build status](https://img.shields.io/travis/janno-p/XRoadProvider/master.svg?label=Mono%20build)](https://travis-ci.org/janno-p/XRoadProvider/)
[![Windows build status](https://img.shields.io/appveyor/ci/janno-p/xroadprovider/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/janno-p/xroadprovider)

XRoadProvider is a .NET library which implements couple of [F# type providers](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/)
that offer easier integration with X-Road security servers and service providers.
Using minimal F# code (which looks like [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for X-Road services) it
is possible to use this library as tool for generating X-Road service interfaces
which can be referenced and used in projects implemented in other .NET languages
(C#, VB).


## More Info ##

Refer to [Developer Guide](articles/guide/index.md) for more information about using
this library.

Browse [API Reference](api/index.md) to find out specific internal details
about the library. Reference contains automatically generated documentation for
all types, modules and functions in the library.


## Contributing and Copyright ##

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [documentation][articles].

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file](articles/license.md) file. 

  [articles]: https://github.com/janno-p/XRoadProvider/tree/master/docs/articles
  [gh]: https://github.com/janno-p/XRoadProvider
  [issues]: https://github.com/janno-p/XRoadProvider/issues
