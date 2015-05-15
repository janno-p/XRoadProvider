#r @"../../XRoadProvider/bin/Debug/XRoadProvider.dll"

open XRoad.Providers

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/../Wsdl/Maakataster.wsdl.xml"

type Maakataster = XRoadProvider<wsdlPath>
type myport = Maakataster.myservice.myport

type XteePäis = Maakataster.ServiceTypes.standardpais

let xp = XteePäis()
xp.andmekogu <- "maakataster"
xp.asutus <- "10239452"
xp.id <- "411d6755661409fed365ad8135f8210be07613da"
xp.isikukood <- "EE:PIN:abc4567"
xp.nimi <- "maakataster.uploadMime.v1"
xp.toimik <- "toimik"

printfn "%s" myport.DefaultAddress
printfn "%s" myport.DefaultProducer
printfn "%O" myport.BindingStyle

let service = myport()
let a = service.ky(obj())
let b = service.legacy1(obj())

//let c = service.uploadMime(obj(), Runtime.AttachmentCollection())
