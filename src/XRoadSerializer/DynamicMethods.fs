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
      DeserializeMethod: MethodInfo
      SerializeDelegate: Lazy<SerializerDelegate>
      SerializeMethod: MethodInfo }
    member this.Serialize(writer: XmlWriter, value: obj) =
        this.SerializeDelegate.Value.Invoke(writer, value)
    member this.Deserialize(reader: XmlReader) =
        this.DeserializeDelegate.Value.Invoke(reader)
    static member Create(typ, deserializerMethod, serializerMethod) =
        { Type = typ
          DeserializeMethod = deserializerMethod
          DeserializeDelegate = lazy (deserializerMethod.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          SerializeMethod = serializerMethod
          SerializeDelegate = lazy (serializerMethod.CreateDelegate(typeof<SerializerDelegate>) |> unbox) }

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
                    | Call(_) -> genIL argExpr
                    | PropertyGet(_) -> genIL argExpr
                    | _ -> failwithf "Unimplemented expression: %A (%A)" argExpr expr
            argsExpr |> List.iter (genSingleArg)
        if argFunc expr |> Option.isNone then
            match expr with
            | Call(Some(instExpr), mi, argsExpr) ->
                genArg (instExpr :: argsExpr)
                il.Emit(OpCodes.Callvirt, mi)
                il.Emit(OpCodes.Nop)
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

let getMethodInfo expr = match expr with Call(_, mi, _) -> mi | _ -> failwith "Must be method call expression"
let createSerializerMethod (typ: Type) = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj> |])
let createDeserializerMethod (typ: Type) = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader> |])

let rec createTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> failwithf "Type `%s` is not serializable." typ.FullName
    | _ ->
        let deserializerMethod = createDeserializerMethod typ
        let serializerMethod = createSerializerMethod typ
        if typeMaps.TryAdd(typ, TypeMap.Create(typ, deserializerMethod, serializerMethod)) then
            createSerializerMethodBody (serializerMethod.GetILGenerator()) typ
            createDeserializerMethodBody (deserializerMethod.GetILGenerator()) typ
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
                il.Emit(OpCodes.Call, typeMap.SerializeMethod)
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
                        il.Emit(OpCodes.Call, x.SerializeMethod)
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
            il.Emit(OpCodes.Call, typeMap.SerializeMethod)
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

and createDeserializerMethodBody (il: ILGenerator) (typ: Type) =
    if typ.IsAbstract then
        // throw new Exception(string.Format("Cannot deserialize abstract type `{0}`.", typ.FullName));
        il.Emit(OpCodes.Ldstr, "Cannot deserialize abstract type `{0}`.")
        il.Emit(OpCodes.Ldstr, typ.FullName)
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
        let instance = il.DeclareLocal(typ)
        il.Emit(OpCodes.Newobj, typ.GetConstructor([| |]))
        il.Emit(OpCodes.Stloc, instance)

        // TODO: deserialize content

        // return instance;
        il.Emit(OpCodes.Ldloc, instance)
        il.MarkLabel(markReturn)
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
    typeMaps.TryAdd(typeof<'T>, TypeMap.Create(typeof<'T>, deserializerMethod, serializerMethod)) |> ignore

do
    createSystemTypeMap<bool>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ box(XmlConvert.ToBoolean((%r).ReadElementString())) @>)
    createSystemTypeMap<Nullable<bool>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue(%v) @>)
        (fun r -> <@ box(XmlConvert.ToBoolean((%r).ReadElementString())) @>)
    createSystemTypeMap<int32>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ box(XmlConvert.ToInt32((%r).ReadElementString())) @>)
    createSystemTypeMap<Nullable<int32>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue(%v) @>)
        (fun r -> <@ box(XmlConvert.ToInt32((%r).ReadElementString())) @>)
    createSystemTypeMap<BigInteger>
        (fun w v -> <@ (%w).WriteValue((%v).ToString()) @>)
        (fun r -> <@ box(BigInteger.Parse((%r).ReadElementString())) @>)
    createSystemTypeMap<Nullable<BigInteger>>
        (fun w v -> <@ if (%v) = null then (%w).WriteAttributeString("nil", XmlNamespace.Xsi, "true") else (%w).WriteValue((%v).ToString()) @>)
        (fun r -> <@ box(BigInteger.Parse((%r).ReadElementString())) @>)
    createSystemTypeMap<string>
        (fun w v -> <@ (%w).WriteValue(%v) @>)
        (fun r -> <@ box((%r).ReadElementString()) @>)
