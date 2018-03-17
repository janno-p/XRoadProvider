module XRoadProvider.Tests.CodeEmitterTests

open Expecto
open System
open System.IO
open System.Reflection.Emit
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Emitter
open XRoad.Emitter.EmitSerialization
open XRoad.Serialization.Attributes

[<XRoadType>]
type HasOptionalElements () =
    [<XRoadElement>]
    member val Value1 = Optional.Option.None<string>() with get, set
    [<XRoadElement>]
    member val Value2 = Optional.Option.None<int>() with get, set

let internal createTypeMap (typ: Type) =
    match typ with
    | NotSerializable ->
        failwithf "Type `%s` is not serializable." typ.FullName
    | Serializable(typeAttribute) ->
        let serialization, deserialization = typ |> Serialization.Create, typ |> Deserialization.Create
        let typeMap = TypeMap.Create(typ, deserialization, serialization, typ |> findBaseType false)
        match typeAttribute.Layout with
        | LayoutKind.Choice -> failwith "not implemented" // typeMap |> createChoiceTypeSerializers false
        | _ -> typeMap |> createTypeSerializers false
        typeMap

let serializeOptionalProperty (v: HasOptionalElements) =
    let serializeContent (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "content")
        il.Emit(OpCodes.Callvirt, typeof<XmlWriter>.GetMethod("WriteString"))
        il
    let propertyType = typeof<int>
    let propertyTypeMap = getTypeMap false propertyType
    let ownerType = typeof<HasOptionalElements>
    let ownerTypeMap = createTypeMap ownerType
    let p = ownerType.GetProperty("Value2")
    let property = Individual { TypeMap = propertyTypeMap
                                SimpleTypeName = getDefaultSystemTypeName (propertyType.FullName)
                                Element = Some(XName.Get("Value2"), false, true)
                                Wrapper = Type ownerTypeMap
                                GetMethod = Some(p.GetGetMethod())
                                SetMethod = Some(p.GetSetMethod(true))
                                HasValueMethod = Some(p.PropertyType.GetProperty("HasValue").GetGetMethod()) }
    let dynMethod = DynamicMethod("Serialize", null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
    let il = dynMethod.GetILGenerator()
    il |> emitOptionalFieldSerialization property serializeContent |> ignore
    il.Emit(OpCodes.Ret)
    let m = dynMethod.CreateDelegate(typeof<SerializerDelegate>) |> unbox<SerializerDelegate>
    use stream = new MemoryStream()
    let writer = XmlWriter.Create(stream)
    writer.WriteStartDocument()
    writer.WriteStartElement("Test")
    m.Invoke(writer, v, SerializerContext())
    writer.WriteEndElement()
    writer.WriteEndDocument();
    writer.Flush()
    stream.Position <- 0L
    use reader = new StreamReader(stream)
    reader.ReadToEnd()
    
let [<Tests>] tests =
    testList "serialization of optional elements" [
        test "doesn't serialize missing optional value" {
            let xml = HasOptionalElements() |> serializeOptionalProperty
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Test />" "invalid serialization result"
        }
        
        test "serializes existing optional value" {
            let xml =
                HasOptionalElements(Value2 = Optional.Option.Some<int>(15))
                |> serializeOptionalProperty
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Test>content</Test>" "invalid serialization result"
        }
    ]
