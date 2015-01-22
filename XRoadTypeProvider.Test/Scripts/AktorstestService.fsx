#r @"../../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

open System.IO
open XRoadTypeProvider
open XRoadTypeProvider.Runtime

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test
type tns = Aktorstest.``http://aktorstest.x-road.ee/producer``

printfn "Default port address: %s" testPort.DefaultAddress
printfn "Default producer name: %s" testPort.DefaultProducer
printfn "SOAP message binding style: %A" testPort.BindingStyle

let service = testPort(Address="http://localhost:8001/")

printfn "Using port address: %s" service.Address
printfn "Using producer name: %s" service.Producer

let settings = XRoadHeader(Consumer=Some("10239452"),
                           UserId=Some("EE:PIN:abc4567"))

// File upload with multipart request

let fup = tns.``fileUpload'``()
fup.request <- tns.``fileUpload'``.``request'``()
fup.request.fileName <- "test.txt"

let fupResponse = service.fileUpload(fup, settings, None)

printfn "%s" fupResponse.response.faultCode
printfn "%s" fupResponse.response.faultString

// Change address service

let cad = tns.``changeAddress'``()
cad.request <- tns.``changeAddress'``.``request'``()
cad.request.aadress <- tns.aadress()
cad.request.aadress.korteriNr <- 13
cad.request.aadress.linnvald <- "Tallinn"
cad.request.aadress.maakond <- "Harju"
cad.request.aadress.majaNr <- "25A"
cad.request.aadress.tanav <- "Paldiski mnt."
cad.request.isikukood <- "30101010001"

let cadResponse = service.changeAddress(cad, settings)

printfn "%s" cadResponse.response.faultCode
printfn "%s" cadResponse.response.faultString

// List methods service

let methods = service.listMethods(obj(), settings)

// File download with multipart response

let fdInput = tns.``fileDownload'``()
fdInput.request <- tns.``fileDownload'``.``request'``()
fdInput.request.fileName <- "document.pdf"

let resp = service.fileDownload(fdInput, settings)
let stream = resp.Attachments.[0]
let result = resp.Result
