namespace XRoad

open FSharp.Core
open System.Xml
open XRoad.Attributes
open XRoad.DynamicMethods

type Serializer(isEncoded) as this =
    let rec skipRoot (depth: int) (reader: XmlReader) =
        if reader.Read() && reader.Depth > depth then
            if reader.NodeType = XmlNodeType.Element && reader.Depth = (depth + 1) then true
            else skipRoot depth reader
        else false

    member __.Deserialize<'T>(reader, context) : 'T =
        this.DeserializeObject(reader, typeof<'T>, context) |> Option.fold (fun _ x -> unbox x) Unchecked.defaultof<'T>

    member __.Deserialize(reader, typ, context) =
        this.DeserializeObject(reader, typ, context) |> Option.fold (fun _ x -> x) null

    member __.Serialize<'T>(writer: XmlWriter, value: 'T, rootName: XmlQualifiedName, context) =
        this.Serialize(writer, typeof<'T>, value, rootName, context)

    member __.Serialize(writer: XmlWriter, typ, value: obj, rootName: XmlQualifiedName, context) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, typ, value, context)
        writer.WriteEndElement()

    member private __.DeserializeObject(reader: XmlReader, typ, context) =
        match reader.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.map (fun x -> x.ToLower()) with
        | Some("true") | Some("1") -> None
        | _ ->
            let typeMap = typ |> getTypeMap isEncoded
            if typeMap.Layout = Some(LayoutKind.Choice) && not (skipRoot reader.Depth reader) then None
            else Some(typeMap.Deserialize(reader, context))

    member private __.SerializeObject(writer: XmlWriter, typ, value, context) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> (typ |> getTypeMap isEncoded).Serialize(writer, value, context) |> ignore
