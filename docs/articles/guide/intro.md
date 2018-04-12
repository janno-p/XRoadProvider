# Introduction #

XRoadProvider library is a collection of type providers that make easier the integration process for
data exchange layer X-Road. Currently three different type providers are implemented:

* `XRoadProducer` is generated type provider which offers type signatures for types and method call
functionality for operations defined in producers service description.

* `XRoadServer` is erased type provider that discovers available producer names from specified
security server and offers their details inside development environment (legacy version that supports
older X-Road security servers).

* `XRoadServer6` is erased type provider that discovers available producer names from specified
security server and offers their details inside development environment (for X-Road security server
version 6).

XRoadProvider library can be [installed from NuGet](https://nuget.org/packages/XRoadProvider):

# [Package Manager](#tab/package-manager)

```powershell
PM> Install-Package XRoadProvider
```

# [.NET CLI](#tab/nuget)

```DOS
> dotnet add package XRoadProvider
```

# [Paket CLI](#tab/paket)

```DOS
> paket add XRoadProvider
```

***


## About Type Providers ##

Since type providers are language specific feature of F# programming language,
they can only be defined and set up in F# projects or scripts. Fortunately, it
takes minimal effort to set things up and ready to be imported into other dependant
projects that might already be in different .NET platform language, like C#.

A general definition for type providers is, that they work as F# compiler plugins,
which transform some source of outside information that is presented in well
defined form (like WSDL definition) to code that can be used by compiler for
two primary purposes. First, compiler uses that information to provide metadata
that can be used as intellisense in IDE-s and editors to provide signatures of
types, methods, etc.; provide code complete information and more features that
are common part of modern software development tools. Secondly, it uses that
same information to compile that metadata into assembly which can be used as
regular dependency that was interpreted from source information and written into
code by some unknown developer.

Using this feature of F# language, this package interprets two sources of
information to produce easier X-Road service development experience to software
developers:

* Using web service producer defined WSDL definition as source information it
provides service interfaces and client logic to execute these operations over
X-Road infrastucture. For that purpose the package defines
[XRoadProducer Type Provider](xroad-producer-provider.md).

* Using X-Road security server meta services as source information it provides
IDE experience for convenient browsing and exploration of services and producers
that are available on X-Road platform. Read about [XRoadServer6 Type Provider](xroad-server-provider.md) for more details and examples.


## Using the package ##

F# language has two options to write executable code. You can create regular
.NET assembly project (library, executable) compile it and the run it. Another,
more lightweight, but as powerful option is to write F# script file and execute
it in F# interactive.


### Creating .NET project ###

To use this package in regular MSBuild project, create new F# project (library
or executable, depending on needs). Then add reference to `XRoadProvider`
[NuGet package](https://www.nuget.org/packages/XRoadProvider/).

After adding a new source code file (with `.fs` file extension) or using the
default `Library1.fs` file, replace the existing contents with following
statement which imports the namespace that defines type provider types (`open`
basically corresponds to `using` statement in C#).

```fsharp
open XRoad.Providers
```

Next step would be using particular type providers, which is described in
previously referred guides.


### Writing F# script ###

Inside Visual Studio create new F# script file (it has `.fsx` extension). Unzip
this packages files in some known location for referencing from script. Replace
contents of the script with following statements:

```fsharp
#load "/path/to/known/location/of/package/lib/net461/XRoadProvider.fsx"
open XRoad.Providers
```

The first line imports bootstrapper script which adds neccessary dependencies
of the package, and second line, again, opens the namespace which has type
provider definitions.

As with .NET project approach, next step is to continue with other guides to
try out type providers. After completing the script, you can run it in F#
interactive which basically means you can try out X-Road services in real time
using the script you just wrote and modifying it as needed. This allows to try
out X-Road services in minutes, without the need to build complex client
application beforehand.
