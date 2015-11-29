#I @"../../../src/XRoadProvider/bin/Debug"

#r "XRoadProvider"
#r "XRoadSerializer"

open System.IO
open XRoad.Providers

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadProducer<wsdlPath>
type AktorstestDto = Aktorstest.DefinedTypes.aktorstest

let port = Aktorstest.aktorstestService.Test()

port.ProducerUri <- "http://localhost:8001/"
port.Consumer <- "10239452"
port.UserId <- "EE:PIN:abc4567"

(*
// File upload with multipart request

let fup = AktorstestDto.fileUpload()
fup.request <- AktorstestDto.fileUpload.requestType()
fup.request.fileName <- "test.txt"

let fupr = port.fileUpload(fup)

printfn "%s" fupr.response.faultCode.BaseValue
printfn "%s" fupr.response.faultString.BaseValue
//*)

//(*
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

printfn "%A" cadr
//printfn "%s" cadr.response.faultCode.BaseValue
//printfn "%s" cadr.response.faultString.BaseValue
//*)

(*
// List methods service

let methods = port.listMethods(Aktorstest.DefinedTypes.xroad.listMethods())
methods.response |> Array.iter (printfn "%s")
//*)

(*
// File download with multipart response

let fd = AktorstestDto.fileDownload()
fd.request <- AktorstestDto.fileDownload.requestType()
fd.request.fileName <- "document.pdf"

let fdr,attachments = port.fileDownload(fd)
fdr.response.file.href
let stream = attachments.[fdr.response.file.href]
//*)