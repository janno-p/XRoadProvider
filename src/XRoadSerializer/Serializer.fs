namespace XRoad

open FSharp.Core
open System
open System.Collections.Concurrent
open System.Reflection.Emit
open System.Xml
open XRoad.Attributes

[<RequireQualifiedAccessAttribute>]
module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xrd = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

type SerializerDelegate = delegate of XmlWriter * obj -> unit
type TypeMap = { Serializer: SerializerDelegate }

type Serializer() =
    static let typeMaps = ConcurrentDictionary<Type, TypeMap>()

    static do
        typeMaps.TryAdd(typeof<string>, { Serializer = SerializerDelegate(fun wr v -> wr.WriteValue(v)) }) |> ignore
        typeMaps.TryAdd(typeof<int32>, { Serializer = SerializerDelegate(fun wr v -> wr.WriteValue(v)) }) |> ignore

    member __.Deserialize(_: XmlReader) : 'T =
        null
    member __.Serialize(writer: XmlWriter, value: obj, rootName: XmlQualifiedName) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let typ = value.GetType()
            let typeMap =
                match typeMaps.TryGetValue(typ) with
                | true, typeMap ->
                    typeMap
                | false, _ ->
                    let attr = typ.GetCustomAttributes(typeof<XRoadTypeAttribute>, false)
                               |> Array.map (fun a -> a :?> XRoadTypeAttribute)
                               |> Array.tryFind (fun _ -> true)
                    match attr with
                    | Some(_) ->
                        let f = DynamicMethod("DynamicSerialize", null, [| typeof<XmlWriter>; typeof<obj> |])
                        let il = f.GetILGenerator()
                        il.Emit(OpCodes.Ret)
                        let d = f.CreateDelegate(typeof<SerializerDelegate>) :?> SerializerDelegate
                        let tmap = { Serializer = d }
                        typeMaps.GetOrAdd(typ, tmap)
                    | None -> failwithf "Type `%s` is not serializable." typ.FullName
            typeMap.Serializer.Invoke(writer, value)
        writer.WriteEndElement()
