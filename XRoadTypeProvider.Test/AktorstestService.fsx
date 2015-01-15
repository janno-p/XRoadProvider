#r @"../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

#r "System.Web.Services"

open System.IO
open XRoadTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/AktorstestService.wsdl.xml"

type Aktorstest = XRoadTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test
type XteeHeader = Aktorstest.ServiceTypes.standardheader

printfn "%s" testPort.DefaultAddress
printfn "%s" testPort.DefaultProducer
printfn "%A" testPort.BindingStyle

let service = testPort(Address="http://localhost:8001/")
printfn "Using port address: %s" service.Address
printfn "Using producer name: %s" service.Producer

let o1 = service.isikOtsing(box 1, box 2, box 3, box 4, box 5, box 6)
let o2 = service.changeAddress(box "body", box "service", box "id", box "userId", box "producer", box "consumer")

let resp = service.fileDownload(box "body", box "service", box "id", box "userId", box "producer", box "consumer")
let stream = resp.Attachments.[0]

(*
let o3, f3 = service.fileDownload(obj())

let hdr = XteeHeader(// DNS-name of the institution
                     consumer="10239452",
                     // Service invocation nonce (unique identifier)
                     id="411d6755661409fed365ad8135f8210be07613da",
                     // DNS-name of the database
                     producer=service.Producer,
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

type fileUploadRequest = { fileName: string }

let file = Runtime.AttachmentCollection()
file.Add(File.OpenRead("test.txt"))

//let result = testPort.Operations.fileUpload({ fileName = "test.txt" }, file)
*)
