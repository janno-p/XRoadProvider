#r @"../XteeTypeProvider/bin/Debug/XteeTypeProvider.dll"

open XteeTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/AktorstestService.wsdl.xml"

type Aktorstest = XteeTypeProvider<wsdlPath>
type testPort = Aktorstest.aktorstestService.Test

printfn "%s" testPort.address
printfn "%s" testPort.producer
