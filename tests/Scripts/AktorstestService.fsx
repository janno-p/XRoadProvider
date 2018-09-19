#I @"../../../bin/XRoadProvider"

#r "../../../packages/Common.Logging/lib/net40/Common.Logging.dll"
#r "../../../packages/Common.Logging.Core/lib/net40/Common.Logging.Core.dll"
#r "XRoadProvider"
#r "System.Xml.Linq"

open System.IO
open XRoad
open XRoad.Providers

//(*
let properties = Common.Logging.Configuration.NameValueCollection()
properties.["showDateTime"] <- "true"
properties.["level"] <- "TRACE"

Common.Logging.LogManager.Adapter <- Common.Logging.Simple.ConsoleOutLoggerFactoryAdapter(properties)
//*)

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/AktorstestService.wsdl.xml"

type Aktorstest = XRoadProducer<wsdlPath>
type AktorstestDto = Aktorstest.DefinedTypes.aktorstest

let port = Aktorstest.aktorstestService.Test()

port.ProducerUri <- "http://localhost:8001/"

let header = XRoadDocHeader()
header.Consumer <- "10239452"
header.UserId <- "EE:PIN:abc4567"

(* Sample 1: changeAddress
let request1 = AktorstestDto.changeAddress_requestType()
request1.aadress <- AktorstestDto.aadress()
request1.aadress.korteriNr <- 13I
request1.aadress.linnvald <- "Tallinn"
request1.aadress.maakond <- Aktorstest.DefinedTypes.xroad.maakond("Harju")
request1.aadress.majaNr <- "25A"
request1.aadress.tanav <- "Paldiski mnt."
request1.isikukood <- "30101010001"

let response1 = port.changeAddress(header, request1)
printfn "%A" response1.response.faultCode
printfn "%A" response1.response.faultString
//*)

(* Sample 2: fileDownload
let request2 = AktorstestDto.fileDownload_requestType()
request2.fileName <- "document.pdf"

let response2 = port.fileDownload(header, request2)
response2.response.file
//*)

(*
// File upload with multipart request

let request3 = AktorstestDto.fileUpload_requestType()
request3.fileName <- "test.txt"

let response3 = port.fileUpload(header, request3)
printfn "%s" response3.response.faultCode.BaseValue
printfn "%s" response3.response.faultString.BaseValue
//*)

(*
// List methods service

let response4 = port.listMethods(header)
response4.response |> Array.iter (printfn "%s")
//*)

//(*
// File download with multipart response

let request5 = AktorstestDto.fileDownload_requestType()
request5.fileName <- "document.pdf"

let response5 = port.fileDownload(header, request5)
let stream = attachments.[response5.response.file.href]
//*)