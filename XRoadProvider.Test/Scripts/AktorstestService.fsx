#r @"../../bin/XRoadProvider.dll"

open System.IO
open XRoad.Providers

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadProducer<wsdlPath>
type AktorstestDto = Aktorstest.DefinedTypes.aktorstest

let port = Aktorstest.aktorstestService.Test()

printfn "Default port address: %s" port.ProducerUri
printfn "Default producer name: %s" port.ProducerName
//printfn "XRoad request format: %A" testPort.RequestFormat

port.ProducerUri <- "http://localhost:8001/"

printfn "Using port address: %s" port.ProducerUri
printfn "Using producer name: %s" port.ProducerName

port.Consumer <- "10239452"
port.UserId <- "EE:PIN:abc4567"

// File upload with multipart request

let fup = AktorstestDto.fileUpload()
fup.request <- AktorstestDto.fileUpload.requestType()
fup.request.fileName <- "test.txt"

let fupr = port.fileUpload(fup)

printfn "%s" fupr.response.faultCode.BaseValue
printfn "%s" fupr.response.faultString.BaseValue

// Change address service

let cad = AktorstestDto.changeAddress()
cad.request <- AktorstestDto.changeAddress.requestType()
cad.request.aadress <- AktorstestDto.aadress()
cad.request.aadress.korteriNr <- 13I
cad.request.aadress.linnvald <- "Tallinn"
cad.request.aadress.maakond <- Aktorstest.DefinedTypes.xroad.maakond(BaseValue="Harju")
cad.request.aadress.majaNr <- "25A"
cad.request.aadress.tanav <- "Paldiski mnt."
cad.request.isikukood <- "30101010001"

let cadr = port.changeAddress(cad)

printfn "%s" cadr.response.faultCode.BaseValue
printfn "%s" cadr.response.faultString.BaseValue

// List methods service

let methods = port.listMethods(Aktorstest.DefinedTypes.xroad.listMethods())
methods.response.item |> Array.iter (printfn "%s")

// File download with multipart response

let fd = AktorstestDto.fileDownload()
fd.request <- AktorstestDto.fileDownload.requestType()
fd.request.fileName <- "document.pdf"

let fdr = port.fileDownload(fd)
fdr.response.file.href
let stream = resp.Attachments.[0]
let result = resp.Result
