#r @"../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

open System.IO
open XRoadTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/AktorstestService.wsdl.xml"

type Aktorstest = XRoadTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test

type XteeHeader = Aktorstest.ServiceTypes.standardheader

let hdr = XteeHeader(// DNS-name of the institution
                     consumer="10239452",
                     // Service invocation nonce (unique identifier)
                     id="411d6755661409fed365ad8135f8210be07613da",
                     // DNS-name of the database
                     producer=testPort.producer,
                     // Name of the service to be invoked
                     service="land-cadastre.allowedMethods",
                     // ID code of the person invoking the service, preceded by a two letter country code (EE37702026518)
                     userId="EE:PIN:abc4567")

type Test = { T: string }

let fu = Aktorstest.ServiceTypes.fileUpload()
fu.body <- { T = "pizza" }
fu.file <- new MemoryStream()

printfn "%O" fu.body

printfn "%s" hdr.consumer
printfn "%s" testPort.address
printfn "%s" testPort.producer

testPort.Services.isikOtsing()
testPort.Services.changeAddress()
testPort.Services.fileDownload()
