namespace XRoad

open FSharp.Core
open FSharp.Quotations
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
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
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xrd = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

module private IL =
    let generate (il: ILGenerator) (f: Expr<XmlWriter> -> Expr<obj> -> Expr) =
        let writer = Expr<XmlWriter>.GlobalVar("w")
        let value = Expr<obj>.GlobalVar("o")
        let expr = f writer value
        let rec genIL expr =
            match expr with
            | Call(Some(instExpr), mi, argsExpr) when instExpr = (upcast writer) ->
                il.Emit(OpCodes.Ldarg_0)
                argsExpr |> List.iter (fun argExpr ->
                    match argExpr with
                    | Value(value, typ) when typ = typeof<string> -> il.Emit(OpCodes.Ldstr, unbox<string> value)
                    | _ -> failwithf "Unimplemented expression: %A" argExpr)
                il.Emit(OpCodes.Callvirt, mi)
                il.Emit(OpCodes.Nop)
            | _ -> failwithf "Unimplemented expression: %A" expr
        genIL expr

type SerializerDelegate = delegate of XmlWriter * obj -> unit
type TypeMap = { Serializer: SerializerDelegate }

type Serializer() as this =
    static let typeMaps = ConcurrentDictionary<Type, TypeMap * DynamicMethod>()

    static do
        typeMaps.TryAdd(typeof<string>, ({ Serializer = SerializerDelegate(fun wr v -> wr.WriteValue(v)) }, null)) |> ignore
        typeMaps.TryAdd(typeof<int32>, ({ Serializer = SerializerDelegate(fun wr v -> wr.WriteValue(v)) }, null)) |> ignore

    static let xmlWriteValue = typeof<XmlWriter>.GetMethod("WriteValue", [| typeof<obj> |])

    member __.Deserialize(_: XmlReader) : 'T =
        null

    member __.Serialize(writer: XmlWriter, value: obj, rootName: XmlQualifiedName) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, value)
        writer.WriteEndElement()

    member private __.SerializeObject(writer: XmlWriter, value: obj) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> (this.GetTypeMap(value.GetType()) |> fst).Serializer.Invoke(writer, value)

    member private __.GetTypeMap(typ) =
        match typeMaps.TryGetValue(typ) with
        | true, typeMap -> typeMap
        | false, _ -> this.BuildTypeMap(typ)

    member private __.BuildTypeMap(typ: Type) =
        let attr = typ.GetCustomAttributes(typeof<XRoadTypeAttribute>, false)
                    |> Array.map (fun a -> a :?> XRoadTypeAttribute)
                    |> Array.tryFind (fun _ -> true)
        match attr with
        | Some(_) ->
            let methodName = sprintf "%s_DynamicSerialize" typ.FullName
            let f = DynamicMethod(methodName, null, [| typeof<XmlWriter>; typeof<obj> |])
            let il = f.GetILGenerator()

            typ.GetProperties()
            |> Array.choose (fun p ->
                if p.GetCustomAttribute<XRoadElementAttribute>() |> isNull && p.GetCustomAttribute<XRoadContentAttribute>() |> isNull
                then None
                else Some(p, attr))
            |> Array.iter (fun (p,_) -> this.BuildPropertySerialization(il, p))

            il.Emit(OpCodes.Ret)
            let d = f.CreateDelegate(typeof<SerializerDelegate>) :?> SerializerDelegate
            let tmap = { Serializer = d }
            typeMaps.GetOrAdd(typ, (tmap, f))
        | None -> failwithf "Type `%s` is not serializable." typ.FullName

    member private __.BuildPropertySerialization(il: ILGenerator, property: PropertyInfo) =
        let generator' = IL.generate il
        let contentAttribute = property.GetCustomAttribute<XRoadContentAttribute>() |> Option.ofObj
        let elementAttribute = property.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj
        let isContent = contentAttribute |> Option.fold (fun _ _ -> true) false
        let isNullable = elementAttribute |> Option.fold (fun _ x -> x.IsNullable) false
        il.Emit(OpCodes.Nop)
        if not isContent then
            generator' (fun writer _ -> let propertyName = property.Name
                                        <@@ (%writer).WriteStartElement(propertyName) @@>)
        let endLabel =
            if property.PropertyType.IsClass || Nullable.GetUnderlyingType(property.PropertyType) |> isNull |> not then
                let lbl1 = il.DefineLabel()
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Castclass, property.DeclaringType)
                il.Emit(OpCodes.Callvirt, property.GetGetMethod())
                if property.PropertyType.IsValueType then
                    il.Emit(OpCodes.Box, property.PropertyType)
                il.Emit(OpCodes.Ldnull)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brtrue_S, lbl1)
                if isNullable then
                    let lbl2 = il.DefineLabel()
                    il.Emit(OpCodes.Nop)
                    generator' (fun writer _ -> <@@ (%writer).WriteAttributeString("nil", XmlNamespace.Xsi, "true") @@>)
                    il.Emit(OpCodes.Nop)
                    il.Emit(OpCodes.Br_S, lbl2)
                    il.MarkLabel(lbl1)
                    il.Emit(OpCodes.Nop)
                    Some(lbl2)
                else
                    il.Emit(OpCodes.Ldstr, "Not nullable property `{0}` of type `{1}` has null value.")
                    il.Emit(OpCodes.Ldstr, property.Name)
                    il.Emit(OpCodes.Ldstr, property.DeclaringType.FullName)
                    il.Emit(OpCodes.Call, typeof<String>.GetMethod("Format", [| typeof<string>; typeof<obj>; typeof<obj> |]))
                    il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                    il.Emit(OpCodes.Throw)
                    il.MarkLabel(lbl1)
                    None
            else None
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Castclass, property.DeclaringType)
        il.Emit(OpCodes.Callvirt, property.GetGetMethod())
        if property.PropertyType.IsValueType then
            il.Emit(OpCodes.Box, property.PropertyType)
        if property.PropertyType.GetCustomAttribute<XRoadTypeAttribute>() |> isNull then
            if property.PropertyType = typeof<BigInteger> then
                il.Emit(OpCodes.Callvirt, typeof<obj>.GetMethod("ToString", [| |]))
            il.Emit(OpCodes.Callvirt, xmlWriteValue)
        else
            let tmap = this.GetTypeMap(property.PropertyType)
            il.Emit(OpCodes.Call, tmap |> snd)
        il.Emit(OpCodes.Nop)
        match endLabel with | Some(lbl) -> il.Emit(OpCodes.Nop); il.MarkLabel(lbl) | None -> ()
        if not isContent then
            generator' (fun writer _ -> <@@ (%writer).WriteEndElement() @@>)
