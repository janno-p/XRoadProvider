namespace XRoad

open System.Xml

type Serializer() =
    member __.Deserialize(_: XmlReader) : 'T =
        null
    member __.Serialize(_: XmlWriter, _: obj) =
        ()
