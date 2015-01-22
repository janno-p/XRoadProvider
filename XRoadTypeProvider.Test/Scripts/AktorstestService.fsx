#r @"../../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

open System.IO
open XRoadTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test

type tns = Aktorstest.``http://aktorstest.x-road.ee/producer``

let service = testPort(Address="http://localhost:8001/")

// File upload service
let fup = tns.``fileUpload'``()
fup.request <- tns.``fileUpload'``.``request'``()
fup.request.fileName <- "test.txt"

let response = service.fileUpload(fup, obj(), obj(), obj(), obj(), obj(), obj())

printfn "%s" response.response.faultCode
printfn "%s" response.response.faultString

// ---

let adr = tns.aadress()
let fup = tns.fileUpload'()

let req = tns.``fileUpload'``.``request'``()
fup.request <- req

let hdr = Runtime.XRoadHeader()

printfn "%s" testPort.DefaultAddress
printfn "%s" testPort.DefaultProducer
printfn "%A" testPort.BindingStyle


printfn "Using port address: %s" service.Address
printfn "Using producer name: %s" service.Producer

let ml = service.listMethods(box 0)

let o1 = service.isikOtsing(tns.isikOtsing'(), box 2, box 3, box 4, box 5, box 6)
let o2 = service.changeAddress(tns.changeAddress'(), box "service", box "id", box "userId", box "producer", box "consumer")

let resp = service.fileDownload(tns.fileDownload'(), box "service", box "id", box "userId", box "producer", box "consumer")
let stream = resp.Attachments.[0]
let result = resp.Result

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
