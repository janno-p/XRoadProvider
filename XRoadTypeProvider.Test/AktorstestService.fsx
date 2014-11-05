#r @"../XRoadTypeProvider/bin/Debug/XRoadTypeProvider.dll"

open XRoadTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/AktorstestService.wsdl.xml"

type Aktorstest = XRoadTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test

printfn "%s" testPort.address
printfn "%s" testPort.producer

testPort.Services.isikOtsing()
testPort.Services.changeAddress()
testPort.Services.fileDownload()
