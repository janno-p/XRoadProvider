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

