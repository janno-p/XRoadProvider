namespace XRoad

open FSharp.Core
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open XRoad.Attributes
open XRoad.DynamicMethods

type Serializer() as this =
    member __.Deserialize<'T>(reader: XmlReader) : 'T =
        this.DeserializeObject<'T>(reader)

    member __.Serialize(writer: XmlWriter, value: obj, rootName: XmlQualifiedName) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, value)
        writer.WriteEndElement()

    member private __.DeserializeObject<'T>(reader: XmlReader) : 'T =
        match reader.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.map (fun x -> x.ToLower()) with
        | Some("true") | Some("1") -> Unchecked.defaultof<'T>
        | _ -> match getTypeMap(typeof<'T>).Deserialize(reader) with
               | null -> Unchecked.defaultof<'T>
               | value -> value |> unbox<'T>

    member private __.SerializeObject(writer: XmlWriter, value: obj) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> getTypeMap(value.GetType()).Serialize(writer, value) |> ignore
