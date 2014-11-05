#r @"../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

open XRoadTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/Maakataster.wsdl.xml"

type Maakataster = XRoadTypeProvider<wsdlPath>
type myport = Maakataster.myservice.myport

printfn "%s" myport.address
printfn "%s" myport.producer

myport.Services.ky()
myport.Services.legacy1()
myport.Services.uploadMime()
