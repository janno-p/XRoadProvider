#I @"../../../bin/XRoadProvider"

#r "../../../packages/Common.Logging/lib/net40/Common.Logging.dll"
#r "../../../packages/Common.Logging.Core/lib/net40/Common.Logging.Core.dll"
#r "XRoadProvider"

open System.IO
open XRoad
open XRoad.Providers

let properties = Common.Logging.Configuration.NameValueCollection()
properties.["showDateTime"] <- "true"
properties.["level"] <- "TRACE"

Common.Logging.LogManager.Adapter <- Common.Logging.Simple.ConsoleOutLoggerFactoryAdapter(properties)

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/XRoadV6.wsdl.xml"

type Xrv6 = XRoadProducer<wsdlPath>

let port = Xrv6.producerPortService.getRandomPortSoap11()

let header = XRoadHeader()

let request = Xrv6.DefinedTypes.ProducerXRoadEu.getRandom()
request.request <- Xrv6.DefinedTypes.ProducerXRoadEu.getRandom.requestType()
request.request.seed <- "@¤%&#"

let response = port.getRandom(header, request)

printfn "%s" response.response.content
