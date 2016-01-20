#I @"../../../bin/XRoadProvider"

#r "../../../packages/Common.Logging/lib/net40/Common.Logging.dll"
#r "../../../packages/Common.Logging.Core/lib/net40/Common.Logging.Core.dll"
#r "XRoadProvider"

open System.IO
open XRoad.Providers

(*
let properties = Common.Logging.Configuration.NameValueCollection()
properties.["showDateTime"] <- "true"
properties.["level"] <- "TRACE"

Common.Logging.LogManager.Adapter <- Common.Logging.Simple.ConsoleOutLoggerFactoryAdapter(properties)
*)

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadProducer<wsdlPath>
type AktorstestDto = Aktorstest.DefinedTypes.aktorstest

let port = Aktorstest.aktorstestService.Test()

port.ProducerUri <- "http://localhost:8001/"
port.Consumer <- "10239452"
port.UserId <- "EE:PIN:abc4567"

(* Sample 1: changeAddress
let request1 = AktorstestDto.changeAddress()
request1.request <- AktorstestDto.changeAddress.requestType()
request1.request.aadress <- AktorstestDto.aadress()
request1.request.aadress.korteriNr <- 13I
request1.request.aadress.linnvald <- "Tallinn"
request1.request.aadress.maakond <- Aktorstest.DefinedTypes.xroad.maakond(BaseValue="Harju")
request1.request.aadress.majaNr <- "25A"
request1.request.aadress.tanav <- "Paldiski mnt."
request1.request.isikukood <- "30101010001"

let response1 = port.changeAddress(request1)
printfn "%A" response1.response.faultCode
printfn "%A" response1.response.faultString
//*)

//(* Sample 2: fileDownload
let request2 = AktorstestDto.fileDownload()
request2.request <- AktorstestDto.fileDownload.requestType()
request2.request.fileName <- "document.pdf"

let response2 = port.fileDownload(request2)
response2.response.file
//*)

(*
// File upload with multipart request

let fup = AktorstestDto.fileUpload()
fup.request <- AktorstestDto.fileUpload.requestType()
fup.request.fileName <- "test.txt"

let fupr = port.fileUpload(fup)

printfn "%s" fupr.response.faultCode.BaseValue
printfn "%s" fupr.response.faultString.BaseValue
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