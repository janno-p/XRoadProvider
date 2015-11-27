module private XRoad.DynamicMethods

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open XRoad
open XRoad.Attributes

type DeserializerDelegate = delegate of XmlReader -> obj
type SerializerDelegate = delegate of XmlWriter * obj -> unit

type TypeMap =
    { Type: Type
      DeserializeDelegate: Lazy<DeserializerDelegate>
      Deserialization: MethodInfo * MethodInfo
      SerializeDelegate: Lazy<SerializerDelegate>
      Serialization: MethodInfo }
    member this.Serialize(writer: XmlWriter, value: obj) =
        this.SerializeDelegate.Value.Invoke(writer, value)
    member this.Deserialize(reader: XmlReader) =
        this.DeserializeDelegate.Value.Invoke(reader)
    static member Create(typ, deserialization, serialization) =
        { Type = typ
          Deserialization = deserialization
          DeserializeDelegate = lazy ((fst deserialization).CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.CreateDelegate(typeof<SerializerDelegate>) |> unbox) }

let typeMaps = ConcurrentDictionary<Type, TypeMap>()

let objGetType = typeof<obj>.GetMethod("GetType", [| |])
let typeGetFullName = typeof<Type>.GetProperty("FullName").GetGetMethod()
let stringEquals = typeof<String>.GetMethod("Equals", [| typeof<string> |])

let generate (il: ILGenerator) (argFunc: Expr -> int option) (expr: Expr<'a>) =
    let rec genIL expr =
        let genArg (argsExpr: Expr list) =
            let rec genSingleArg (argExpr: Expr) =
                if argFunc argExpr |> Option.isNone then
                    match argExpr with
                    | Value(null, _) -> il.Emit(OpCodes.Ldnull)
                    | Value(value, typ) when typ = typeof<string> -> il.Emit(OpCodes.Ldstr, unbox<string> value)
                    | Coerce(e, _) -> genSingleArg e
                    | Call(_)
                    | PropertyGet(_)
                    | NewObject(_) -> genIL argExpr
                    | _ -> failwithf "Unimplemented expression: %A (%A)" argExpr expr
            argsExpr |> List.iter (genSingleArg)
        if argFunc expr |> Option.isNone then
            match expr with
            | Call(Some(instExpr), mi, argsExpr) ->
                genArg (instExpr :: argsExpr)
                il.Emit(OpCodes.Callvirt, mi)
                il.Emit(OpCodes.Nop)
            | SpecificCall <@@ ignore @@> (_, _, [argsExpr]) ->
                genIL argsExpr
                il.Emit(OpCodes.Pop)
            | SpecificCall <@@ raise @@> (_, _, [argsExpr]) ->
                genIL argsExpr
                il.Emit(OpCodes.Throw)
            | SpecificCall <@@ (=) @@> (_, _, argsExpr) ->
                genArg argsExpr
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
            | Call(None, mi, argsExpr) ->
                genArg argsExpr
                il.Emit(OpCodes.Call, mi)
            | IfThenElse(condExpr, trueExpr, falseExpr) ->
                let lbl1 = il.DefineLabel()
                let lbl2 = il.DefineLabel()
                genIL condExpr
                il.Emit(OpCodes.Brtrue_S, lbl1)
                genIL trueExpr
                il.Emit(OpCodes.Br, lbl2)
                il.MarkLabel(lbl1)
                il.Emit(OpCodes.Nop)
                genIL falseExpr
                il.MarkLabel(lbl2)
                il.Emit(OpCodes.Nop)
            | NewObject(ci, [argsExpr]) ->
                genIL argsExpr
                il.Emit(OpCodes.Newobj, ci)
            | Sequential(expr1, expr2) ->
                genIL expr1
                genIL expr2
            | PropertyGet(Some(targetExpr), pi, argsExpr) ->
                genArg (targetExpr::argsExpr)
                il.Emit(OpCodes.Callvirt, pi.GetGetMethod())
                il.Emit(OpCodes.Nop)
            | Value(null,_) ->
                il.Emit(OpCodes.Nop)
            | _ -> failwithf "Unimplemented expression: %A" expr
    genIL expr

let getMethodInfo expr =
    match expr with
    | Call(_, mi, _) -> mi
    | PropertyGet(_, pi, _) -> pi.GetGetMethod()
    | _ -> failwith "Must be method call expression"

let createSerializerMethod (typ: Type) = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj> |])
let createDeserializerMethod (typ: Type) = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader> |])
let createDeserializeContentMethod (typ: Type) = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, null, [| typeof<XmlReader>; typeof<obj> |])

let createDeserializerMethodBody (il: ILGenerator) (typeMaps: TypeMap list) =
    let mainType = typeMaps.Head
    if mainType.Type.IsAbstract then
        // throw new Exception(string.Format("Cannot deserialize abstract type `{0}`.", typ.FullName));
        il.Emit(OpCodes.Ldstr, "Cannot deserialize abstract type `{0}`.")
        il.Emit(OpCodes.Ldstr, mainType.Type.FullName)
        il.Emit(OpCodes.Call, getMethodInfo <@ String.Format("", "") @>)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)
    else
        let markReturn = il.DefineLabel()

        // var nilValue = (reader.GetAttribute("nil", XmlNamespace.Xsi) ?? "").ToLower();
        let nilValue = il.DeclareLocal(typeof<string>)
        let markSkipNull = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "nil")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).GetAttribute("", "") @>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Brtrue_S, markSkipNull)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ldstr, "")
        il.MarkLabel(markSkipNull)
        il.Emit(OpCodes.Callvirt, getMethodInfo <@ "".ToLower() @>)
        il.Emit(OpCodes.Stloc, nilValue)

        // if (nilValue == "1" || nilValue == "true")
        //     return null;
        let lbl1 = il.DefineLabel()
        let lbl2 = il.DefineLabel()
        let lbl3 = il.DefineLabel()
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "1")
        il.Emit(OpCodes.Call, getMethodInfo <@ "" = "" @>)
        il.Emit(OpCodes.Brtrue_S, lbl1)
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "true")
        il.Emit(OpCodes.Call, getMethodInfo <@ "" = "" @>)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Br_S, lbl2)
        il.MarkLabel(lbl1)
        il.Emit(OpCodes.Ldc_I4_0)
        il.MarkLabel(lbl2)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Brtrue_S, lbl3)
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Br, markReturn)
        il.MarkLabel(lbl3)

        // var instance = new T();
        let instance = il.DeclareLocal(mainType.Type)
        il.Emit(OpCodes.Newobj, mainType.Type.GetConstructor([| |]))
        il.Emit(OpCodes.Stloc, instance)

        // TODO: deserialize attributes

        // Deserialize content
        let attr = mainType.Type.GetCustomAttribute<XRoadTypeAttribute>()
        match attr.Layout with
        | LayoutKind.Sequence ->
            typeMaps
            |> List.rev
            |> List.iter (fun typeMap ->
                let (_,mi) = typeMap.Deserialization
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldloc, instance)
                il.Emit(OpCodes.Call, mi)
                il.Emit(OpCodes.Nop))
        | _ -> failwith "Not implemented!"

        // return instance;
        il.Emit(OpCodes.Ldloc, instance)
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

let createDeserializeContentMethodBody (il: ILGenerator) (typeMap: TypeMap) (properties: (PropertyInfo * TypeMap) list) =
    // var varDepth = reader.Depth + 1;
    let varDepth = il.DeclareLocal(typeof<int>)
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).Depth @>)
    il.Emit(OpCodes.Ldc_I4_1)
    il.Emit(OpCodes.Add)
    il.Emit(OpCodes.Stloc, varDepth)

    let (|ContentProperty|_|) (properties: (PropertyInfo * TypeMap) list) =
        match properties with
        | [(prop,_) as x] ->
            match prop.GetCustomAttribute<XRoadContentAttribute>() with
            | null -> None
            | _ -> Some(x)
        | _ -> None

    let emitDeserialization (property: PropertyInfo) (propTypeMap: TypeMap) =
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Castclass, typeMap.Type)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, propTypeMap.Deserialization |> fst)
        if propTypeMap.Type.IsValueType
        then il.Emit(OpCodes.Unbox_Any, propTypeMap.Type)
        else il.Emit(OpCodes.Castclass, propTypeMap.Type)
        il.Emit(OpCodes.Callvirt, property.GetSetMethod())

    match properties with
    | ContentProperty(property,propTypeMap) ->
        emitDeserialization property propTypeMap
    | _ ->
        let attr = typeMap.Type.GetCustomAttribute<XRoadTypeAttribute>()
        match attr.Layout with
        | LayoutKind.Sequence ->
            properties
            |> List.iter (fun (property, propTypeMap) ->
                let markLoopStart = il.DefineLabel()
                let markSuccess = il.DefineLabel()

                // reader.Read() -> false exception
                il.MarkLabel(markLoopStart)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).Read() @>)
                il.Emit(OpCodes.Brtrue_S, markSuccess)

                // throw new Exception("Invalid message: could not parse xml.");
                il.Emit(OpCodes.Ldstr, "Invalid message: could not parse xml.")
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)
                il.MarkLabel(markSuccess)

                // reader.Depth != depth
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).Depth @>)
                il.Emit(OpCodes.Ldloc, varDepth)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse_S, markLoopStart)

                // reader.NodeType != XmlNodeType.Element
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).NodeType @>)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse_S, markLoopStart)

                // reader.LocalName != property.Name
                let markDeserialize = il.DefineLabel()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).LocalName @>)
                il.Emit(OpCodes.Ldstr, property.Name)
                il.Emit(OpCodes.Call, getMethodInfo <@ "" = "" @>)
                il.Emit(OpCodes.Brtrue_S, markDeserialize)
                il.Emit(OpCodes.Ldstr, "Unexpected element: found `{0}`, but was expecting to find `{1}`.")
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, getMethodInfo <@ (null: XmlReader).LocalName @>)
                il.Emit(OpCodes.Ldstr, property.Name)
                il.Emit(OpCodes.Call, getMethodInfo <@ String.Format("", "", "") @>)
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)

                // Deserialize property
                il.MarkLabel(markDeserialize)
                emitDeserialization property propTypeMap
                )
        | _ -> failwith "Not implemented"
    il.Emit(OpCodes.Ret)

let rec createTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> failwithf "Type `%s` is not serializable." typ.FullName
    | _ ->
        let deserializerMethod = createDeserializerMethod typ
        let deserializeContentMethod = createDeserializeContentMethod typ
        let serializerMethod = createSerializerMethod typ
        if typeMaps.TryAdd(typ, TypeMap.Create(typ, (upcast deserializerMethod, upcast deserializeContentMethod), serializerMethod)) then
            createSerializerMethodBody (serializerMethod.GetILGenerator()) typ
            createDeserializerMethodBody (deserializerMethod.GetILGenerator()) (findBaseTypes typ)
            let properties =
                typ.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> isNull |> not ||
                                          p.GetCustomAttribute<XRoadContentAttribute>() |> isNull |> not)
                |> Array.map (fun p -> (p, getTypeMap p.PropertyType))
                |> Array.sortBy (fun (p,_) -> p.MetadataToken)
                |> Array.toList
            createDeserializeContentMethodBody (deserializeContentMethod.GetILGenerator()) (getTypeMap typ) properties
        typeMaps.[typ]

and createSerializerMethodBody (il: ILGenerator) (typ: Type) =
    let writer: Expr<XmlWriter> = Expr.GlobalVar("w")
    let value: Expr<obj> = Expr.GlobalVar("v")

    generate il
             (fun arg -> match arg with
                         | e when e = (upcast value) -> il.Emit(OpCodes.Ldarg_1); Some(-1)
                         | e when e = (upcast writer) -> il.Emit(OpCodes.Ldarg_0); Some(-1)
                         | _ -> None)
             <@ if (%value) = null then (%writer).WriteAttributeString("nil", XmlNamespace.Xsi, "true") @>

    let emitPropertySerialization (il: ILGenerator) (property: PropertyInfo) =
        let contentAttribute = property.GetCustomAttribute<XRoadContentAttribute>() |> Option.ofObj
        let elementAttribute = property.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj
        let propertyName = property.Name
        let typeName = property.DeclaringType.FullName

        let serializer: Expr<unit> = Expr.GlobalVar("s")
        let attributes: Expr<unit> = Expr.GlobalVar("a")

        let contentExpr =
            if property.PropertyType.IsClass || Nullable.GetUnderlyingType(property.PropertyType) |> (isNull >> not) then
                if elementAttribute |> Option.fold (fun _ x -> x.IsNullable) false then
                    <@ (%attributes)
                       (%serializer) @>
                else
                    <@
                        if (%value) = null then
                            raise (Exception(String.Format("Not nullable property `{0}` of type `{1}` has null value.", propertyName, typeName)))
                        else
                            (%attributes)
                            (%serializer)
                        @>
            else <@ (%serializer) @>
        let expr =
            if contentAttribute |> Option.fold (fun _ _ -> true) false |> not then
                <@
                    (%writer).WriteStartElement(propertyName)
                    (%contentExpr)
                    (%writer).WriteEndElement()
                    @>
            else contentExpr

        let typeMap = getTypeMap property.PropertyType
        let subTypes = findSubTypes property.PropertyType

        let emitWriterParam() =
            il.Emit(OpCodes.Ldarg_0)
        let emitValueParam() =
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Castclass, property.DeclaringType)
            il.Emit(OpCodes.Callvirt, property.GetGetMethod())
            if property.PropertyType.IsValueType then
                il.Emit(OpCodes.Box, property.PropertyType)
        let emitSerializerParam() =
            match subTypes with
            | [] ->
                emitWriterParam()
                emitValueParam()
                il.Emit(OpCodes.Call, typeMap.Serialization)
                il.Emit(OpCodes.Nop)
            | _ ->
                let conditionEnd = il.DefineLabel()
                let rec genSubType (lbl: Label option) subTypes =
                    match subTypes with
                    | [] -> ()
                    | x::xs ->
                        lbl |> Option.iter (fun lbl -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop))
                        let lbl = match xs with [] -> conditionEnd | _ -> il.DefineLabel()
                        emitValueParam()
                        il.Emit(OpCodes.Call, objGetType)
                        il.Emit(OpCodes.Callvirt, typeGetFullName)
                        il.Emit(OpCodes.Ldstr, x.Type.FullName)
                        il.Emit(OpCodes.Callvirt, stringEquals)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                        il.Emit(OpCodes.Brtrue_S, lbl)
                        il.Emit(OpCodes.Nop)
                        emitWriterParam()
                        emitValueParam()
                        il.Emit(OpCodes.Call, x.Serialization)
                        il.Emit(OpCodes.Nop)
                        il.Emit(OpCodes.Nop)
                        il.Emit(OpCodes.Br, conditionEnd)
                        genSubType (Some lbl) xs
                subTypes |> genSubType None
                il.MarkLabel(conditionEnd)
        let rec emitAttributesParam() =
            match subTypes with
            | [] -> ()
            | _ ->
                let conditionEnd = il.DefineLabel()
                let rec genSubType (lbl: Label option) subTypes =
                    match subTypes with
                    | [] -> ()
                    | x::xs ->
                        match lbl with Some(lbl) -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop) | None -> ()
                        let lbl = match xs with [] -> conditionEnd | _ -> il.DefineLabel()
                        emitValueParam()
                        il.Emit(OpCodes.Call, objGetType)
                        il.Emit(OpCodes.Callvirt, typeGetFullName)
                        il.Emit(OpCodes.Ldstr, x.Type.FullName)
                        il.Emit(OpCodes.Callvirt, stringEquals)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                        il.Emit(OpCodes.Brtrue_S, lbl)
                        il.Emit(OpCodes.Nop)
                        generate' <@ (%writer).WriteStartAttribute("type", XmlNamespace.Xsi) @>
                        let attr = x.Type.GetCustomAttribute<XRoadTypeAttribute>()
                        let typeName = match attr.Name with null | "" -> x.Type.Name | name -> name
                        match attr.Namespace with
                        | null | "" -> generate' <@ (%writer).WriteString(typeName) @>
                        | ns -> generate' <@ (%writer).WriteQualifiedName(typeName, ns) @>
                        generate' <@ (%writer).WriteEndAttribute() @>
                        il.Emit(OpCodes.Br, conditionEnd)
                        genSubType (Some lbl) xs
                emitValueParam()
                il.Emit(OpCodes.Call, objGetType)
                il.Emit(OpCodes.Callvirt, typeGetFullName)
                il.Emit(OpCodes.Ldstr, property.PropertyType.FullName)
                il.Emit(OpCodes.Callvirt, stringEquals)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, conditionEnd)
                il.Emit(OpCodes.Nop)
                subTypes |> genSubType None
                il.MarkLabel(conditionEnd)
        and generate' x = generate il (fun arg -> match arg with
                                                  | e when e = (upcast value) -> emitValueParam(); Some(-1)
                                                  | e when e = (upcast writer) -> emitWriterParam(); Some(-1)
                                                  | e when e = (upcast serializer) -> emitSerializerParam(); Some(-1)
                                                  | e when e = (upcast attributes) -> emitAttributesParam(); Some(-1)
                                                  | _ -> None) x

        generate' expr
    let rec callBase (typ: Type) =
        match findTypeMap typ.BaseType with
        | Some(typeMap) ->
            callBase typ.BaseType
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Call, typeMap.Serialization)
            il.Emit(OpCodes.Nop)
        | _ -> ()
    callBase typ
    typ.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    |> Array.choose (fun p ->
        if p.GetCustomAttribute<XRoadElementAttribute>() |> isNull && p.GetCustomAttribute<XRoadContentAttribute>() |> isNull
        then None
        else Some(p))
    |> Array.iter (emitPropertySerialization il)
    il.Emit(OpCodes.Ret)

and getTypeMap(typ) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> createTypeMap typ

and findTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(getTypeMap typ)

and findSubTypes (typ: Type) =
    let subTypes =
        typ.Assembly.GetTypes()
        |> Array.filter (fun x -> x.IsSubclassOf(typ))
        |> List.ofArray
    let rec orderTypes (ordered: Type list) (unordered: Type list) =
        let next, rem = unordered |> List.partition (fun x -> ordered |> List.exists ((=) x.BaseType))
        let newOrdered = next @ ordered
        match rem with
        | [] -> newOrdered
        | _ -> orderTypes newOrdered rem
    match subTypes with
    | [] -> []
    | xs -> orderTypes [typ] xs |> List.choose (findTypeMap) |> List.filter (fun x -> not x.Type.IsAbstract)

and findBaseTypes (typ: Type) =
    typ
    |> List.unfold (fun typ -> if typ = typeof<obj> then None else Some(findTypeMap typ, typ.BaseType))
    |> List.choose (id)

let createSystemTypeMap<'T> (writerExpr: Expr<XmlWriter> -> Expr<obj> -> Expr<unit>) (readerExpr: Expr<XmlReader> -> Expr<obj>) =
    let serializerMethod = createSerializerMethod typeof<'T>
    let deserializerMethod = createDeserializerMethod typeof<'T>
    let reader: Expr<XmlReader> = Expr.GlobalVar("r")
    let writer: Expr<XmlWriter> = Expr.GlobalVar("w")
    let value: Expr<obj> = Expr.GlobalVar("v")
    let il = serializerMethod.GetILGenerator()
    generate il
             (fun e -> match e with
                       | e when e = (upcast value) -> il.Emit(OpCodes.Ldarg_1); Some(-1)
                       | e when e = (upcast writer) -> il.Emit(OpCodes.Ldarg_0); Some(-1)
                       | _ -> None)
             (writerExpr writer value)
    il.Emit(OpCodes.Ret)
    let il = deserializerMethod.GetILGenerator()
    generate il
             (fun e -> if e = (upcast reader) then il.Emit(OpCodes.Ldarg_0); Some(-1) else None)
             (readerExpr reader)
    il.Emit(OpCodes.Ret)
    typeMaps.TryAdd(typeof<'T>, TypeMap.Create(typeof<'T>, (upcast deserializerMethod, null), serializerMethod)) |> ignore

do
    createSystemTypeMap<bool>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ ignore((%r).Read()); box((%r).ReadContentAsBoolean()) @>)
    createSystemTypeMap<Nullable<bool>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue(%v) @>)
        (fun r -> <@ ignore((%r).Read()); box((%r).ReadContentAsBoolean()) @>)
    createSystemTypeMap<int32>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ ignore((%r).Read()); box((%r).ReadContentAsInt()) @>)
    createSystemTypeMap<Nullable<int32>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue(%v) @>)
        (fun r -> <@ ignore((%r).Read()); box((%r).ReadContentAsInt()) @>)
    createSystemTypeMap<BigInteger>
        (fun w v -> <@ (%w).WriteValue((%v).ToString()) @>)
        (fun r -> <@ ignore((%r).Read()); box(BigInteger((%r).ReadContentAsDecimal())) @>)
    createSystemTypeMap<Nullable<BigInteger>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue((%v).ToString()) @>)
        (fun r -> <@ ignore((%r).Read()); box(BigInteger((%r).ReadContentAsDecimal())) @>)
    createSystemTypeMap<string>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ ignore((%r).Read()); box((%r).ReadContentAsString()) @>)
