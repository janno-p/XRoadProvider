﻿#I "../../../bin/XRoadProvider"

#r "Common.Logging"
#r "Common.Logging.Core"
#r "XRoadProvider"

#r "System.Xml.Linq"

open Common.Logging
open Common.Logging.Configuration
open Common.Logging.Simple
open System.IO
open XRoad
open XRoad.Providers

let properties = NameValueCollection()
properties.["showDateTime"] <- "true"
properties.["level"] <- "TRACE"

LogManager.Adapter <- ConsoleOutLoggerFactoryAdapter(properties)

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/XRoadV6.wsdl.xml"

type Xrv6 = XRoadProducer<wsdlPath>
type Types = Xrv6.DefinedTypes.ProducerXRoadEu

let car = Types.Car.Volkswagen
printfn "%s" car.BaseValue

let port = Xrv6.producerPortService.getRandomPortSoap11()

let header = XRoadHeader()
header.Client <- XRoadMemberIdentifier("ee-dev", "GOV", "70000310", "generic-consumer")
header.Producer <- XRoadMemberIdentifier("ee-dev", "GOV", "70000310", "etoimik-arendus")
header.UserId <- "EE30101010001"

let request = Types.getRandom_requestType()
request.seed <- "123"

let response = port.getRandom(header, request)
printfn "%s" response.response.content
