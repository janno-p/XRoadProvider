#r @"../XteeTypeProvider/bin/Debug/XteeTypeProvider.dll"

open XteeTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/Maakataster.wsdl.xml"

type Maakataster = XteeTypeProvider<wsdlPath>
type myport = Maakataster.myservice.myport

printfn "%s" myport.address
printfn "%s" myport.producer

(*
type Maakataster = Provider.``http://producers.maakataster.xtee.riik.ee/producer/maakataster``

Provider.ky()
Provider.legacy1()
Provider.uploadMime()
*)
