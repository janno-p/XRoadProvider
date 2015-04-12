module XRoad5Sample

open XRoad.Providers

[<Literal>]
let path = __SOURCE_DIRECTORY__ + @"\Wsdl\xroad5.wsdl"

type XRoad5 = XRoadProducer<path>
