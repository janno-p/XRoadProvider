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


## Example ##

This example demonstrates the use of the XRoadProvider:

```fsharp
// Reference the type provider assembly.
#load "packages/XRoadProvider/net461/XRoadProvider.fsx"

open XRoad
open XRoad.Providers

type Xrd6 = XRoadProducer<"/Work/XRoadProvider/tests/XRoadProvider.Tests/Wsdl/XRoadV6.wsdl.xml">

// Initialize service interface which provides access to operation methods.
let myport = Xrd6.producerPortService.getRandomPortSoap11("http://localhost:8001/")

// Assign X-Road header values.
let hdr = XRoadHeader()
hdr.Client <- XRoadMemberIdentifier("ee-dev", "GOV", "000000000", "sys")
hdr.Producer <- XRoadMemberIdentifier("ee-dev", "GOV", "00000000", "sys")
hdr.ProtocolVersion <- "4.0"
hdr.UserId <- "30101010007"

// Initialize request parameters.
let request = Xrd6.DefinedTypes.ProducerXRoadEu.getRandom_requestType()
request.seed <- (System.Guid.NewGuid()).ToString()

// Execute service request against specified adapter.
let response = myport.getRandom(hdr, request)

// Display results to console.
printfn "getRandom response: %s" response.response.content
```

As an alternative to asking service descriptions from security server, it's also possible to
exclude security server from development process by using local WSDL definitions instead.


## MIME/Multipart attachment support ##

X-Road method calls may require MIME/multipart message format in case of binary content. For
that purpose type provider defines special type `BinaryContent` which handles attachments according
to X-Road specification.

Usage example of `BinaryContent` type:

```fsharp
type Aktorstest = XRoadProducer<"/Work/XRoadProvider/tests/XRoadProvider.Tests/Wsdl/AktorstestService.wsdl.xml">

let service = Aktorstest.aktorstestService.Test()

let request2 = Aktorstest.DefinedTypes.aktorstest.fileUploadMTOM_requestType()
request2.filemtom <- BinaryContent.Create([| 0uy; 1uy; 2uy; 3uy |])
request2.fileName <- "file.bin"

let docHdr = XRoadDocHeader()
docHdr.UserName <- "toomas.dumpty"

let result = service.fileUploadMTOM(docHdr, request2)

printfn "%s" result.response.faultCode.BaseValue
printfn "%s" result.response.faultString.BaseValue
```


## Type provider for producer discovery ##

XRoadProvider package includes separate type provider to retrieve producer information from security
server. Resulting type contains details about all producers available and their WSDL URIs which may
be used as parameter to `XRoadProducer` provider to initialize service interfaces.

Example use of `XRoadServer` type provider:

```fsharp
open XRoadProvider

// Acquire list of producer from security server.
let [<Literal>] securityServerUrl = "http://your-security-server-here/"

type SecurityServer = XRoadServer6<securityServerUrl, "ee-dev", "COM", "12345678", "generic-consumer">
type AdsConfig = SecurityServer.Producers.GOV.``Maa-amet (70003098)``.``SUBSYSTEM:ads``
type Ads = XRoadProducer<AdsConfig.``SERVICE:ADSaadrmuudatused``>

let adsService = Ads.xroadeuService.xroadeuServicePort(securityServerUrl)

let adsHeader =
    XRoadHeader(
        Client = SecurityServer.Identifier,
        Producer = AdsConfig.Identifier,
        ProtocolVersion = "4.0"
    )

let adsResponse =
    adsService.ADSaadrmuudatused(
        adsHeader,
        muudetudAlates = some(NodaTime.LocalDate(2017, 11, 28))
    )

adsResponse.fault.MatchSome(fun f -> failwithf "Invalid service response. %s: %s" f.faultCode f.faultString)

let muudatused = adsResponse.muudatused.ValueOr([||])
printfn "Got %d changes in response message." muudatused.Length
```


## Samples & documentation ##

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the articles folder][articles]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.md) contains a further explanation of this sample library.

 * [API Reference](../api/index.md) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 

## Contributing and copyright ##

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][articles] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file](license.md) file. 

  [articles]: https://github.com/janno-p/XRoadProvider/tree/master/docs/articles
  [gh]: https://github.com/janno-p/XRoadProvider
  [issues]: https://github.com/janno-p/XRoadProvider/issues
  [readme]: https://github.com/janno-p/XRoadProvider/blob/master/README.md
