#r @"../XteeTypeProvider/bin/Debug/XteeTypeProvider.dll"

open XteeTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/Maakataster.wsdl"

type Provider = XteeTypeProvider<wsdlPath>
type Maakataster = Provider.``http://producers.maakataster.xtee.riik.ee/producer/maakataster``

Provider.ky()
Provider.legacy1()
Provider.uploadMime()
