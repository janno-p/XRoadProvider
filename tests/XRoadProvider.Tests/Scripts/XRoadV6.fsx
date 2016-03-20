#I @"../../../bin/XRoadProvider"

#r "../../../packages/Common.Logging/lib/net40/Common.Logging.dll"
#r "../../../packages/Common.Logging.Core/lib/net40/Common.Logging.Core.dll"
#r "XRoadProvider"
#r "System.Xml.Linq"

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
type Types = Xrv6.DefinedTypes.ProducerXRoadEu

let port = Xrv6.producerPortService.getRandomPortSoap11()

let header = XRoadHeader()
header.Client <- XRoadMemberIdentifier("ee-dev", "GOV", "70000310")
header.Producer <- XRoadMemberIdentifier("ee-dev", "GOV", "70000310", "etoimik-arendus")
header.UserId <- "EE30101010001"

let request = Types.getRandom()
request.request <- Types.getRandom.requestType()
request.request.seed <- "123"

let response = port.getRandom(header, request)

printfn "%s" response.response.content
