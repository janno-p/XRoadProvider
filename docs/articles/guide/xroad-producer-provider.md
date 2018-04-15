# XRoadProducer Type Provider #

`XRoadProducer` is the main working horse of the package. It takes X-Road web service WSDL
specification as input and returns a type which wraps all the types defined in the `types`
part of WSDL document and also service client types which implement `binding` part of
the WSDL document to provide service types for calling X-Road services over the network.

Although it looks like very dynamic code in IDE with all the code completion based on
WSDL definition, in the background the code is actually compiled into real assembly
which works as usual in .NET platform. So there is no runtime performance hit in
regards to resulting output of the type provider.


## XRoadProducer Type Provider in Action ##

Here is a small animation visualizing type provider usage in code editor or IDE with
proper F# language support (Visual Studio, Visual Studio Code with Ionide extension,
etc.).

![XRoadProducer](../../images/XRoadProducer.gif)


## Configuring XRoadProducer ##

Primary functionality of `XRoadProducer` type provider is executed with the following line:

```fsharp
type Xrd6 = XRoadProducer<"E:/Work/XRoadProvider/tests/XRoadProvider.Tests/Wsdl/XRoadV6.wsdl.xml">
```

This line executes `XRoadProducer` type provider which interprets given service description document
(WSDL) and returns new type which wraps required types from the schema which enable communication
with web service that follows the same WSDL. The resulting root type is saved into current namespace
with given alias (`Xrd6` in current sample fragment).

The type provider takes its parameters between angle brackets `'<'` and `'>'` and for `XRoadProducer`
type provider there are following configuration options available:

| Parameter name | Type | Required | Default value | Description |
|----------------|------|----------|---------------|-------------|
| `Uri` | `String` | Yes | - | WSDL document location (either local file or network resource). |
| `LanguageCode` | `String` | No | `"et"` | Specify language code that is extracted as documentation tooltips. |
| `Filter` | `String` | No | `""` | Comma separated list of operations which should be included in definitions. By default, all operations are included. |

It should be noted, that type provider parameters can only be compile time constant values and literals
which means you cannot "calculate" the parameters. You can hold parameter values in literal values
using `LiteralAttribute` attribute:

```fsharp
let [<Literal>] WsdlLocation = "http://someurl/and.wsdl"
```

For local files, it is sometimes convenient to provide relative locations, which can be achieved by using
special F# compiler predefined variable `__SOURCE_DIRECTORY__` which represents path of the directory
containing current script or source file:

```fsharp
let [<Literal>] WsdlLocation = __SOURCE_DIRECTORY__ + "/file.wsdl"
```

Using predefined values as type provider arguments:

```fsharp
type Xrd6 = XRoadProducer<WsdlLocation>
```


## Example ##

```fsharp
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

