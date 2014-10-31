#r @"../XteeTypeProvider/bin/Debug/XteeTypeProvider.dll"

open XteeTypeProvider

[<Literal>]
let wsdlPath = __SOURCE_DIRECTORY__ + "/AktorstestService.wsdl.xml"

type Provider = XteeTypeProvider<wsdlPath>
type AktorstestService = Provider.``http://aktorstest.x-road.ee/producer``

Provider.fileUpload()
Provider.listMethods()
Provider.fileDownloadMTOM()
