namespace XRoad

open FSharp.Core
open System.Xml
open XRoad.Attributes
open XRoad.DynamicMethods

type Serializer() as this =
    let rec skipRoot (depth: int) (reader: XmlReader) =
        if reader.Read() && reader.Depth > depth then
            if reader.NodeType = XmlNodeType.Element && reader.Depth = (depth + 1) then true
            else skipRoot depth reader
        else false

    member __.Deserialize<'T>(reader: XmlReader) : 'T =
        this.DeserializeObject(reader, typeof<'T>) |> Option.fold (fun _ x -> unbox x) Unchecked.defaultof<'T>

    member __.Deserialize(reader: XmlReader, typ) =
        this.DeserializeObject(reader, typ) |> Option.fold (fun _ x -> x) null

    member __.Serialize(writer: XmlWriter, value: obj, rootName: XmlQualifiedName) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, value)
        writer.WriteEndElement()

    member private __.DeserializeObject(reader, typ) =
        match reader.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.map (fun x -> x.ToLower()) with
        | Some("true") | Some("1") -> None
        | _ ->
            let typeMap = getTypeMap(typ)
            if typeMap.Layout = Some(LayoutKind.Choice) && not (skipRoot reader.Depth reader) then None
            else Some(typeMap.Deserialize(reader))

    member private __.SerializeObject(writer, value) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> getTypeMap(value.GetType()).Serialize(writer, value) |> ignore
