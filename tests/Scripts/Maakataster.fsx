﻿#I @"../../../bin/XRoadProvider"

#r "../../../packages/Common.Logging/lib/net40/Common.Logging.dll"
#r "../../../packages/Common.Logging.Core/lib/net40/Common.Logging.Core.dll"
#r "XRoadProvider"

open System.IO
open XRoad.Providers

//(*
let properties = Common.Logging.Configuration.NameValueCollection()
properties.["showDateTime"] <- "true"
properties.["level"] <- "TRACE"

Common.Logging.LogManager.Adapter <- Common.Logging.Simple.ConsoleOutLoggerFactoryAdapter(properties)
//*)

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/Maakataster.wsdl.xml"

type Maakataster = XRoadProducer<wsdlPath>
type myport = Maakataster.myservice.myport

let xp = Maakataster.DefinedTypes.xtee.hdrstd()
xp.andmekogu <- "maakataster"
xp.asutus <- "10239452"
xp.id <- "411d6755661409fed365ad8135f8210be07613da"
xp.isikukood <- "EE:PIN:abc4567"
xp.nimi <- "maakataster.uploadMime.v1"
xp.toimik <- "toimik"

let service = myport()
service.ProducerUri <- "http://localhost:8001/"

printfn "%s" service.ProducerUri
printfn "%s" service.ProducerName

(*
let req_a = Maakataster.DefinedTypes.maakataster.ky_paring()
req_a.katastritunnus <- "katastritunnus"
req_a.ky_max <- Maakataster.DefinedTypes.maakataster.t_ky_max(BaseValue="10")

let resp_a = service.ky(req_a)
//*)

let b = service.legacy1([| "array"; "of"; "strings" |])
let c = service.uploadMime(Maakataster.DefinedTypes.maakataster.mime_paring())
