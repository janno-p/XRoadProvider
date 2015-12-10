module private XRoad.DynamicMethods

open FSharp.Quotations
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

type Deserialization =
    { Root: MethodInfo
      Content: MethodInfo
      MatchType: MethodInfo }

type TypeMap =
    { Type: Type
      Name: string
      Namespace: string option
      Layout: LayoutKind option
      DeserializeDelegate: Lazy<DeserializerDelegate>
      Deserialization: Deserialization
      SerializeDelegate: Lazy<SerializerDelegate>
      Serialization: MethodInfo
      CanHaveNullAsValue: bool }
    member this.Serialize(writer: XmlWriter, value: obj) =
        this.SerializeDelegate.Value.Invoke(writer, value)
    member this.Deserialize(reader: XmlReader) =
        this.DeserializeDelegate.Value.Invoke(reader)
    member this.FullName =
        match this.Namespace with
        | Some(ns) -> sprintf "{%s}:{%s}" ns this.Name
        | None -> this.Name
    static member Create(typ: Type, deserialization, serialization) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>() |> Option.ofObj
        let layout = attr |> Option.map (fun attr -> attr.Layout)
        { Type = typ
          Name = attr |> Option.fold (fun name attr -> match attr.Name with null | "" -> name | x -> x) typ.Name
          Namespace = attr |> Option.bind (fun attr -> match attr.Namespace with null | "" -> None | x -> Some(x))
          Layout = layout
          Deserialization = deserialization
          DeserializeDelegate = lazy (deserialization.Root.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.CreateDelegate(typeof<SerializerDelegate>) |> unbox)
          CanHaveNullAsValue = (not (Nullable.GetUnderlyingType(typ) |> isNull)) || (typ.IsClass && layout <> Some(LayoutKind.Choice)) }

type PropertyMap =
    { TypeMap: TypeMap
      Element: (string * bool) option
      OwnerTypeMap: TypeMap
      GetMethod: MethodInfo
      SetMethod: MethodInfo }

type ArrayMap =
    { Type: Type
      Element: (string * bool) option
      ItemTypeMap: TypeMap
      ItemElement: (string * bool) option
      OwnerTypeMap: TypeMap
      GetMethod: MethodInfo
      SetMethod: MethodInfo }
    member this.GetItemPropertyMap() =
        { TypeMap = this.ItemTypeMap
          Element = this.ItemElement
          OwnerTypeMap = this.OwnerTypeMap
          GetMethod = null
          SetMethod = null }

type Property =
    | Individual of PropertyMap
    | Array of ArrayMap
    member this.Element with get() = this |> function Individual x -> x.Element | Array x -> x.Element
    member this.OwnerTypeMap with get() = this |> function Individual x -> x.OwnerTypeMap | Array x -> x.OwnerTypeMap
    member this.Type with get() = this |> function Individual x -> x.TypeMap.Type | Array x -> x.Type

let typeMaps = ConcurrentDictionary<Type, TypeMap>()

let (!@) expr =
    match expr with
    | Call(_, mi, _) -> mi
    | PropertyGet(_, pi, _) -> pi.GetGetMethod()
    | _ -> failwithf "Must be method call expression, but was `%A`." expr

let (!!@) expr =
    match expr with
    | NewObject(ci, _) -> ci
    | _ -> failwith "Must be constructor expression"

let createMatchTypeMethod (typ: Type) = DynamicMethod(sprintf "%s_MatchType" typ.FullName, typeof<bool>, [| typeof<XmlReader> |])
let createSerializerMethod (typ: Type) skipVisibility = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj> |], (skipVisibility: bool))
let createDeserializerMethod (typ: Type) = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader> |])
let createDeserializeContentMethod (typ: Type) = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, null, [| typeof<XmlReader>; typeof<obj>; typeof<bool> |])

let emitNullCheck (il: ILGenerator) (labelReturn: Label) =
    // var nilValue = (reader.GetAttribute("nil", XmlNamespace.Xsi) ?? "").ToLower();
    let nilValue = il.DeclareLocal(typeof<string>)
    let markSkipNull = il.DefineLabel()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldstr, "nil")
    il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).GetAttribute("", "") @>)
    il.Emit(OpCodes.Dup)
    il.Emit(OpCodes.Brtrue_S, markSkipNull)
    il.Emit(OpCodes.Pop)
    il.Emit(OpCodes.Ldstr, "")
    il.MarkLabel(markSkipNull)
    il.Emit(OpCodes.Callvirt, !@ <@ "".ToLower() @>)
    il.Emit(OpCodes.Stloc, nilValue)

    // if (nilValue == "1" || nilValue == "true")
    let lbl1 = il.DefineLabel()
    let lbl2 = il.DefineLabel()
    let lbl3 = il.DefineLabel()
    il.Emit(OpCodes.Ldloc, nilValue)
    il.Emit(OpCodes.Ldstr, "1")
    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
    il.Emit(OpCodes.Brtrue_S, lbl1)
    il.Emit(OpCodes.Ldloc, nilValue)
    il.Emit(OpCodes.Ldstr, "true")
    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
    il.Emit(OpCodes.Ldc_I4_0)
    il.Emit(OpCodes.Ceq)
    il.Emit(OpCodes.Br_S, lbl2)
    il.MarkLabel(lbl1)
    il.Emit(OpCodes.Ldc_I4_0)
    il.MarkLabel(lbl2)
    il.Emit(OpCodes.Nop)
    il.Emit(OpCodes.Brtrue_S, lbl3)

    // return null;
    il.Emit(OpCodes.Ldnull)
    il.Emit(OpCodes.Br, labelReturn)
    il.MarkLabel(lbl3)

let createDeserializerMethodBody (il: ILGenerator) (typeMap: TypeMap) =
    if typeMap.Type.IsAbstract then
        // throw new Exception(string.Format("Cannot deserialize abstract type `{0}`.", typ.FullName));
        il.Emit(OpCodes.Ldstr, "Cannot deserialize abstract type `{0}`.")
        il.Emit(OpCodes.Ldstr, typeMap.Type.FullName)
        il.Emit(OpCodes.Call, !@ <@ String.Format("", "") @>)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)
    else
        let markReturn = il.DefineLabel()

        emitNullCheck il markReturn

        // var instance = new T();
        let instance = il.DeclareLocal(typeMap.Type)
        il.Emit(OpCodes.Newobj, typeMap.Type.GetConstructor([| |]))
        il.Emit(OpCodes.Stloc, instance)

        // TODO: deserialize attributes

        // Deserialize content
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, instance)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
        il.Emit(OpCodes.Nop)

        // return instance;
        il.Emit(OpCodes.Ldloc, instance)
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

let rec emitTypeDeserialization (il: ILGenerator) (typeMap: TypeMap) =
    let emitDefault (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Root)
        match typeMap.Type.IsValueType with
        | true -> il.Emit(OpCodes.Unbox_Any, typeMap.Type)
        | _ -> il.Emit(OpCodes.Castclass, typeMap.Type)
    match findSubTypes typeMap.Type with
    | [] -> emitDefault typeMap
    | subTypes ->
        let conditionEnd = il.DefineLabel()
        let selfLabel = il.DefineLabel()
        let strVar = il.DeclareLocal(typeof<string>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "type")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).GetAttribute("", "") @>)
        il.Emit(OpCodes.Stloc, strVar)
        il.Emit(OpCodes.Ldloc, strVar)
        il.Emit(OpCodes.Brfalse, selfLabel)
        let chars = il.DeclareLocal(typeof<char[]>)
        let parts = il.DeclareLocal(typeof<string[]>)
        il.Emit(OpCodes.Ldloc, strVar)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Newarr, typeof<char>)
        il.Emit(OpCodes.Stloc, chars)
        il.Emit(OpCodes.Ldloc, chars)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldc_I4, int32 ':')
        il.Emit(OpCodes.Stelem_I2)
        il.Emit(OpCodes.Ldloc, chars)
        il.Emit(OpCodes.Ldc_I4_2)
        il.Emit(OpCodes.Callvirt, !@ <@ "".Split([| ':' |], 2) @>)
        il.Emit(OpCodes.Stloc, parts)
        let typeName = il.DeclareLocal(typeof<string>)
        let typeNamespace = il.DeclareLocal(typeof<string>)
        let label1 = il.DefineLabel()
        let label2 = il.DefineLabel()
        il.Emit(OpCodes.Ldloc, parts)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Conv_I4)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brtrue_S, label1)
        il.Emit(OpCodes.Ldloc, parts)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, parts)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Ldloc, typeNamespace)
        il.Emit(OpCodes.Brtrue_S, label2)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Br_S, label2)
        il.MarkLabel(label1)
        il.Emit(OpCodes.Ldloc, parts)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.MarkLabel(label2)
        il.Emit(OpCodes.Nop)
        let rec genSubType (lbl: Label option) (subTypes: TypeMap list) =
            lbl |> Option.iter (fun lbl -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop))
            match subTypes with
            | [] ->
                il.Emit(OpCodes.Ldstr, "Invalid message: unknown type `{0}:{1}`.")
                il.Emit(OpCodes.Ldloc, typeNamespace)
                il.Emit(OpCodes.Ldloc, typeName)
                il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)
            | x::xs ->
                let lbl = il.DefineLabel()
                il.Emit(OpCodes.Ldloc, typeName)
                il.Emit(OpCodes.Ldstr, x.Name)
                il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                il.Emit(OpCodes.Brfalse_S, lbl)
                il.Emit(OpCodes.Ldloc, typeNamespace)
                il.Emit(OpCodes.Ldstr, x.Namespace |> Option.fold (fun _ x -> x) "")
                il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                il.Emit(OpCodes.Brfalse_S, lbl)
                emitDefault x
                il.Emit(OpCodes.Br, conditionEnd)
                genSubType (Some lbl) xs
        subTypes |> genSubType None
        il.MarkLabel(selfLabel)
        emitDefault typeMap
        il.MarkLabel(conditionEnd)
        il.Emit(OpCodes.Nop)

and private createDeserializeContentMethodBody (il: ILGenerator) (typeMaps: TypeMap list) (properties: Property list) =
    let emitDeserialization (prop: Property) =
        match prop with
        | Individual propertyMap ->
            let x = il.DeclareLocal(propertyMap.TypeMap.Type)
            emitTypeDeserialization il propertyMap.TypeMap
            il.Emit(OpCodes.Stloc, x)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Castclass, typeMaps.Head.Type)
            il.Emit(OpCodes.Ldloc, x)
            il.Emit(OpCodes.Callvirt, propertyMap.SetMethod)
        | Array arrayMap ->
            failwith "not implemented array property deserialization"

    let (|Content|_|) (properties: Property list) =
        match properties with
        | [Individual(propertyMap) as prop] ->
            match propertyMap.Element with
            | None -> if propertyMap.TypeMap.Layout <> Some(LayoutKind.Choice) then Some(prop) else None
            | Some(_) -> None
        | _ -> None

    match properties with
    | Content(prop) -> emitDeserialization prop
    | _ ->
        let varDepth = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Stloc, varDepth)

        let label = il.DefineLabel()
        let skipLabel = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_2)
        il.Emit(OpCodes.Brfalse_S, label)
        il.Emit(OpCodes.Br, skipLabel)
        il.MarkLabel(label)
        il.Emit(OpCodes.Ldloc, varDepth)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stloc, varDepth)

        typeMaps.Tail
        |> List.rev
        |> List.iter (fun typeMap ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
            il.Emit(OpCodes.Nop))

        match typeMaps.Head.Layout.Value with
        | LayoutKind.Choice ->
            ()
        | LayoutKind.Sequence ->
            properties
            |> List.iteri (fun i prop ->
                let markLoopStart = il.DefineLabel()
                let markSuccess = il.DefineLabel()

                // reader.Read() -> false exception
                il.MarkLabel(markLoopStart)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Read() @>)
                il.Emit(OpCodes.Brtrue_S, markSuccess)

                // throw new Exception("Invalid message: could not parse xml.");
                match prop with
                | Individual { Element = Some(name,_) }
                | Array { Element = Some(name,_) }
                | Array { ItemElement = Some(name,_) } ->
                    il.Emit(OpCodes.Ldstr, sprintf "Invalid message: expected `%s`, but was end of file." name)
                | _ -> il.Emit(OpCodes.Ldstr, "Invalid message: unexpected end of file.")
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)
                il.MarkLabel(markSuccess)
                il.Emit(OpCodes.Nop)

                if i = 0 then il.MarkLabel(skipLabel)

                let markSuccess2 = il.DefineLabel()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
                il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.EndElement)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse_S, markSuccess2)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
                il.Emit(OpCodes.Ldloc, varDepth)
                il.Emit(OpCodes.Clt)
                il.Emit(OpCodes.Brfalse_S, markSuccess2)
                match prop with
                | Individual { Element = Some(name,_) }
                | Array { Element = Some(name,_) }
                | Array { ItemElement = Some(name,_) } ->
                    il.Emit(OpCodes.Ldstr, sprintf "Invalid message: expected `%s`, but was `</{0}>`." name)
                | _ -> il.Emit(OpCodes.Ldstr, "Invalid message: unexpected element `</{0}>`.")
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                il.Emit(OpCodes.Call, !@ <@ String.Format("", "") @>)
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)
                il.MarkLabel(markSuccess2)
                il.Emit(OpCodes.Nop)

                // reader.Depth != depth
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
                il.Emit(OpCodes.Ldloc, varDepth)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse_S, markLoopStart)

                // reader.NodeType != XmlNodeType.Element
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse_S, markLoopStart)

                match prop with
                | Individual { Element = Some(name,_) }
                | Array { Element = Some(name,_) } ->
                    // reader.LocalName != property.Name
                    let markDeserialize = il.DefineLabel()
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, name)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Brtrue_S, markDeserialize)
                    il.Emit(OpCodes.Ldstr, "Unexpected element: found `{0}`, but was expecting to find `{1}`.")
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, name)
                    il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
                    il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                    il.Emit(OpCodes.Throw)
                    il.MarkLabel(markDeserialize)
                | _ -> ()

                // Deserialize property
                emitDeserialization prop
                )
        | _ -> failwith "Not implemented"
    il.Emit(OpCodes.Ret)

and createTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> failwithf "Type `%s` is not serializable." typ.FullName
    | typeAttribute ->
        let deserializerMethod = createDeserializerMethod typ
        let deserializeContentMethod = createDeserializeContentMethod typ
        let serializerMethod = createSerializerMethod typ (typeAttribute.Layout = LayoutKind.Choice)
        let matchTypeMethod = createMatchTypeMethod typ
        let deserialization = { Root = deserializerMethod; Content = deserializeContentMethod; MatchType = matchTypeMethod }
        if typeMaps.TryAdd(typ, TypeMap.Create(typ, deserialization, serializerMethod)) then
            match typeAttribute.Layout with
            | LayoutKind.Choice ->
                createChoiceTypeSerializers (serializerMethod.GetILGenerator()) (deserializerMethod.GetILGenerator()) (deserializeContentMethod.GetILGenerator()) (matchTypeMethod.GetILGenerator()) typ
            | _ ->
                let typeMap = getTypeMap typ
                createSerializerMethodBody (serializerMethod.GetILGenerator()) typeMap
                createDeserializerMethodBody (deserializerMethod.GetILGenerator()) typeMap
                let propertyMaps = typeMap |> getPropertyMaps
                createDeserializeContentMethodBody (deserializeContentMethod.GetILGenerator()) (findBaseTypes typ) propertyMaps
                match propertyMaps with
                | [Individual { Element = None }]
                | [Array { Element = None; ItemElement = None }] -> ()
                | _ -> createMatchType (matchTypeMethod.GetILGenerator()) (propertyMaps |> List.tryHead)
        typeMaps.[typ]

and private createMatchType il property =
    match property with
    | Some(Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap })
    | Some(Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }) ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, typeMap.Deserialization.MatchType)
    | None
    | Some(Individual { Element = None })
    | Some(Array { Element = None; ItemElement = None }) ->
        il.Emit(OpCodes.Ldc_I4_0)
    | Some(Individual { Element = Some(name,_) })
    | Some(Array { Element = Some(name,_) })
    | Some(Array { Element = None; ItemElement = Some(name,_) }) ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
        il.Emit(OpCodes.Ldstr, name)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
    il.Emit(OpCodes.Ret)

and createChoiceTypeSerializers ilSer ilDeser ilDeserContent ilMatch choiceType =
    let idField =  match choiceType.GetField("__id", BindingFlags.Instance ||| BindingFlags.NonPublic) with
                   | null -> choiceType.GetField("__id@", BindingFlags.Instance ||| BindingFlags.NonPublic)
                   | x -> x
    let valueField = match choiceType.GetField("__value", BindingFlags.Instance ||| BindingFlags.NonPublic) with
                     | null -> choiceType.GetField("__value@", BindingFlags.Instance ||| BindingFlags.NonPublic)
                     | x -> x
    let conditionEnd = ilSer.DefineLabel()
    let rec genSerialization (label: Label option) (options: (XRoadChoiceOptionAttribute * Type * MethodInfo) list) =
        match options with
        | [] -> ()
        | (attr,typ,_)::xs ->
            label |> Option.iter (fun label -> ilSer.MarkLabel(label); ilSer.Emit(OpCodes.Nop))
            let label = match xs with [] -> conditionEnd | _ -> ilSer.DefineLabel()
            ilSer.Emit(OpCodes.Ldarg_1)
            ilSer.Emit(OpCodes.Castclass, choiceType)
            ilSer.Emit(OpCodes.Ldfld, idField)
            ilSer.Emit(OpCodes.Ldc_I4_S, attr.Id)
            ilSer.Emit(OpCodes.Ceq)
            ilSer.Emit(OpCodes.Brfalse, label)
            ilSer.Emit(OpCodes.Nop)
            let emitSerialization () =
                ilSer.Emit(OpCodes.Ldarg_0)
                ilSer.Emit(OpCodes.Ldarg_1)
                ilSer.Emit(OpCodes.Castclass, choiceType)
                ilSer.Emit(OpCodes.Ldfld, valueField)
                ilSer.Emit(OpCodes.Call, (getTypeMap typ).Serialization)
                ilSer.Emit(OpCodes.Nop)
            if attr.MergeContent then emitSerialization()
            else
                ilSer.Emit(OpCodes.Ldarg_0)
                ilSer.Emit(OpCodes.Ldstr, attr.Name)
                ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("") @>)
                match findSubTypes typ with
                | [] -> emitSerialization()
                | subTypes ->
                    let typeTestEnd = ilSer.DefineLabel()
                    let rec testSubType (label: Label option) (subTypes: TypeMap list) =
                        match subTypes with
                        | [] -> ()
                        | subType::subTypes ->
                            label |> Option.iter (fun label -> ilSer.MarkLabel(label); ilSer.Emit(OpCodes.Nop))
                            let label = match subTypes with [] -> typeTestEnd | _ -> ilSer.DefineLabel()
                            ilSer.Emit(OpCodes.Ldarg_1)
                            ilSer.Emit(OpCodes.Castclass, choiceType)
                            ilSer.Emit(OpCodes.Ldfld, valueField)
                            ilSer.Emit(OpCodes.Call, !@ <@ (null: obj).GetType() @>)
                            ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: Type).FullName @>)
                            ilSer.Emit(OpCodes.Ldstr, subType.Type.FullName)
                            ilSer.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                            ilSer.Emit(OpCodes.Brfalse_S, label)
                            ilSer.Emit(OpCodes.Ldarg_0)
                            ilSer.Emit(OpCodes.Ldstr, "type")
                            ilSer.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
                            ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartAttribute("", "") @>)
                            ilSer.Emit(OpCodes.Ldarg_0)
                            ilSer.Emit(OpCodes.Ldstr, subType.Name)
                            match subType.Namespace with
                            | Some(ns) ->
                                ilSer.Emit(OpCodes.Ldstr, ns)
                                ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteQualifiedName("", "") @>)
                            | None -> ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteString("") @>)
                            ilSer.Emit(OpCodes.Ldarg_0)
                            ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndAttribute() @>)
                            ilSer.Emit(OpCodes.Ldarg_0)
                            ilSer.Emit(OpCodes.Ldarg_1)
                            ilSer.Emit(OpCodes.Castclass, choiceType)
                            ilSer.Emit(OpCodes.Ldfld, valueField)
                            ilSer.Emit(OpCodes.Call, subType.Serialization)
                            ilSer.Emit(OpCodes.Nop)
                            ilSer.Emit(OpCodes.Nop)
                            ilSer.Emit(OpCodes.Br, typeTestEnd)
                            testSubType (Some label) subTypes
                    subTypes |> testSubType None
                    ilSer.MarkLabel(typeTestEnd)
                    ilSer.Emit(OpCodes.Nop)
            ilSer.Emit(OpCodes.Br_S, conditionEnd)
            if not <| attr.MergeContent then
                ilSer.Emit(OpCodes.Ldarg_0)
                ilSer.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndElement() @>)
            genSerialization (Some label) xs
    let genDeserialization options =
        let il = ilDeser
        let markReturn = il.DefineLabel()
        let rec generate (options: (XRoadChoiceOptionAttribute * Type * MethodInfo) list) =
            match options with
            | [] ->
                il.Emit(OpCodes.Ldnull)
                il.Emit(OpCodes.Br_S, markReturn)
            | (attr,typ,mi)::options ->
                let label = il.DefineLabel()
                let typeMap = getTypeMap typ
                if attr.MergeContent then
                    let instance = il.DeclareLocal(typeMap.Type)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.MatchType)
                    il.Emit(OpCodes.Brfalse_S, label)
                    il.Emit(OpCodes.Newobj, typeMap.Type.GetConstructor([| |]))
                    il.Emit(OpCodes.Stloc, instance)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldloc, instance)
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
                    il.Emit(OpCodes.Ldloc, instance)
                else
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, attr.Name)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Brfalse, label)
                    emitTypeDeserialization il typeMap
                il.Emit(OpCodes.Call, mi)
                il.Emit(OpCodes.Br_S, markReturn)
                il.MarkLabel(label)
                il.Emit(OpCodes.Nop)
                generate options
        generate options
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)
    let genMatch options =
        let il = ilMatch
        let markReturn = il.DefineLabel()
        let rec generate (options: (XRoadChoiceOptionAttribute * Type * MethodInfo) list) =
            match options with
            | [] ->
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Br_S, markReturn)
            | (attr,typ,_)::options ->
                let label = il.DefineLabel()
                let typeMap = getTypeMap typ
                if attr.MergeContent then
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.MatchType)
                else
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, attr.Name)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                il.Emit(OpCodes.Brfalse_S, label)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Br, markReturn)
                il.MarkLabel(label)
                il.Emit(OpCodes.Nop)
                generate options
        generate options
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)
    choiceType.GetCustomAttributes<XRoadChoiceOptionAttribute>()
    |> Seq.map (fun attr ->
        let (typ, mi) =
            let methodName = sprintf "New%s%s" (if Char.IsLower(attr.Name.[0]) then "_" else "") attr.Name
            match choiceType.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static) with
            | null -> failwithf "Type `%s` should define public static method `%s`." choiceType.FullName methodName
            | mi -> match mi.GetParameters() with
                    | [| pi |] -> (pi.ParameterType, mi)
                    | _ -> failwithf "Type `%s` method `New%s` should have exactly one argument." choiceType.FullName attr.Name
        (attr, typ, mi))
    |> Seq.toList
    |> (fun x -> genSerialization None x
                 genDeserialization x
                 genMatch x)
    ilSer.MarkLabel(conditionEnd)
    ilSer.Emit(OpCodes.Ret)
    ilDeserContent.Emit(OpCodes.Ret)

and emitPropertyContentSerialization (il: ILGenerator) (property: Property) (emitValue: Type -> unit) =
    let propertyType = property.Type
    let subTypes = match property with Array _ -> [] | _ -> findSubTypes propertyType

    let markReturn = il.DefineLabel()

    let emitTypeTest (typ: Type) =
        emitValue(typ)
        il.Emit(OpCodes.Call, !@ <@ (null: obj).GetType() @>)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: Type).FullName @>)
        il.Emit(OpCodes.Ldstr, typ.FullName)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)

    let emitTypeAttribute (typ: Type) =
        let markTypeTestEnd = il.DefineLabel()

        // if (value.GetType().FullName != propertyMap.Type.FullName) {
        emitTypeTest typ
        il.Emit(OpCodes.Brtrue, markTypeTestEnd)
        il.Emit(OpCodes.Nop)

        subTypes
        |> List.iter (fun subType ->
            let markNext = il.DefineLabel()

            // if (value.GetType().FullName == subType.Type.FullName) {
            emitTypeTest subType.Type
            il.Emit(OpCodes.Brfalse_S, markNext)

            // writer.WriteStartAttribute("type", XmlNamespace.Xsi);
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldstr, "type")
            il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartAttribute("", "") @>)

            // writer.WriteQualifiedName(subType.Name, subType.Namespace);
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldstr, subType.Name)
            match subType.Namespace with
            | Some(ns) ->
                il.Emit(OpCodes.Ldstr, ns)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteQualifiedName("", "") @>)
            | None ->
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteString("") @>)
            il.Emit(OpCodes.Nop)

            // writer.WriteEndAttribute()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndAttribute() @>)

            il.Emit(OpCodes.Br, markTypeTestEnd)

            // } else {
            il.MarkLabel(markNext)
            il.Emit(OpCodes.Nop)
            //}
            )

        if Nullable.GetUnderlyingType(typ) |> isNull then
            // throw new Exception(string.Format("Unexpected type `{0}`.", value.GetType().FullName));
            il.Emit(OpCodes.Ldstr, "Unexpected type `{0}`.")
            emitValue(typ)
            il.Emit(OpCodes.Call, !@ <@ (null: obj).GetType() @>)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: Type).FullName @>)
            il.Emit(OpCodes.Call, !@ <@ String.Format("", "") @>)
            il.Emit(OpCodes.Newobj, !!@ <@ Exception("") @>)
            il.Emit(OpCodes.Throw)

        // }
        il.MarkLabel(markTypeTestEnd)
        il.Emit(OpCodes.Nop)

    let emitAttributes (name: string, isNullable) =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            // if (value == null) {
            let markSuccess = il.DefineLabel()
            emitValue(propertyType)
            il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Ceq)
            il.Emit(OpCodes.Brfalse_S, markSuccess)
            if isNullable then
                // writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true");
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldstr, "nil")
                il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
                il.Emit(OpCodes.Ldstr, "true")
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteAttributeString("", "", "") @>)
                il.Emit(OpCodes.Nop)
                il.Emit(OpCodes.Br, markReturn)
            else
                // throw new Exception(string.Format("Not nullable property `{0}` of type `{1}` has null value.", "propertyName", "ownerTypeName"));
                il.Emit(OpCodes.Ldstr, "Not nullable property `{0}` of type `{1}` has null value.")
                il.Emit(OpCodes.Ldstr, name)
                il.Emit(OpCodes.Ldstr, property.OwnerTypeMap.FullName)
                il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
                il.Emit(OpCodes.Newobj, !!@ <@ Exception("") @>)
                il.Emit(OpCodes.Throw)
            // }
            il.MarkLabel(markSuccess)
            il.Emit(OpCodes.Nop)
            emitTypeAttribute property.Type
            // TODO: Other attributes
        | _ -> ()

    let emitContent (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldarg_0)
        emitValue(typeMap.Type)
        il.Emit(OpCodes.Call, typeMap.Serialization)
        il.Emit(OpCodes.Nop)

    match property.Element with
    | Some(name, isNullable) ->
        // writer.WriteStartElement("propertyName");
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, name)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("") @>)
        il.Emit(OpCodes.Nop)
        emitAttributes(name, isNullable)
    | None -> ()

    match property with
    | Individual propertyMap ->
        match subTypes with
        | [] ->
            emitContent(propertyMap.TypeMap)
        | _ ->
            let conditionEnd = il.DefineLabel()
            let rec genSubType (lbl: Label option) (subTypes: TypeMap list) =
                match subTypes with
                | [] -> ()
                | subTypeMap::subTypeMaps ->
                    lbl |> Option.iter (fun lbl -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop))
                    let lbl = match subTypeMaps with [] -> conditionEnd | _ -> il.DefineLabel()
                    emitTypeTest subTypeMap.Type
                    il.Emit(OpCodes.Brfalse_S, lbl)
                    il.Emit(OpCodes.Nop)
                    emitContent(subTypeMap)
                    il.Emit(OpCodes.Nop)
                    il.Emit(OpCodes.Br, conditionEnd)
                    genSubType (Some lbl) subTypeMaps
            subTypes |> genSubType None
            il.MarkLabel(conditionEnd)
    | Array arrayMap ->
        let arr = il.DeclareLocal(arrayMap.Type)
        emitValue(arrayMap.Type)
        il.Emit(OpCodes.Stloc, arr)
        let i = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Stloc, i)
        let markLoopCondition = il.DefineLabel()
        il.Emit(OpCodes.Br_S, markLoopCondition)
        let markLoopStart = il.DefineLabel()
        il.MarkLabel(markLoopStart)
        il.Emit(OpCodes.Nop)
        emitPropertyContentSerialization il (Individual (arrayMap.GetItemPropertyMap())) (fun typ ->
            il.Emit(OpCodes.Ldloc, arr)
            il.Emit(OpCodes.Ldloc, i)
            il.Emit(OpCodes.Ldelem, typ)
            if (typ.IsValueType) then
                il.Emit(OpCodes.Box, typ))
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stloc, i)
        il.MarkLabel(markLoopCondition)
        il.Emit(OpCodes.Ldloc, i)
        il.Emit(OpCodes.Ldloc, arr)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Conv_I4)
        il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Brtrue_S, markLoopStart)

    il.MarkLabel(markReturn)
    il.Emit(OpCodes.Nop)

    match property.Element with
    | Some(_) ->
        // writer.WriteEndElement();
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndElement() @>)
        il.Emit(OpCodes.Nop)
    | None -> ()

and private emitPropertySerialization (il: ILGenerator) (property: Property) =
    let emitValue =
        match property with
        | Individual propertyMap ->
            (fun (typ: Type) ->
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Castclass, propertyMap.OwnerTypeMap.Type)
                il.Emit(OpCodes.Callvirt, propertyMap.GetMethod)
                if typ.IsValueType then il.Emit(OpCodes.Box, typ))
        | Array arrayMap ->
            (fun _ ->
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Castclass, arrayMap.OwnerTypeMap.Type)
                il.Emit(OpCodes.Callvirt, arrayMap.GetMethod))
    emitPropertyContentSerialization il property emitValue

and private getPropertyMaps (typeMap: TypeMap) : Property list =
    typeMap.Type.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    |> List.ofArray
    |> List.sortBy (fun p -> p.MetadataToken)
    |> List.choose (fun p ->
        match p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj with
        | None -> None
        | Some(attr) ->
            let name = match attr.Name with null | "" -> p.Name | name -> name
            let element = if attr.MergeContent then None else Some(name, attr.IsNullable)
            match p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj with
            | Some(cattr) ->
                let itemTypeMap = getTypeMap (p.PropertyType.GetElementType())
                let itemName = match cattr.ItemName with null | "" -> "item" | name -> name
                let itemElement = if itemTypeMap.Layout <> Some(LayoutKind.Choice)
                                  then if cattr.MergeContent then None else Some(itemName, cattr.ItemIsNullable)
                                  else None
                Some(Array { Type = p.PropertyType
                             Element = element
                             ItemTypeMap = itemTypeMap
                             ItemElement = itemElement
                             OwnerTypeMap = typeMap
                             GetMethod = p.GetGetMethod()
                             SetMethod = p.GetSetMethod() })
            | None ->
                let propertyTypeMap = getTypeMap p.PropertyType
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Some(Individual { TypeMap = propertyTypeMap
                                  Element = element
                                  OwnerTypeMap = typeMap
                                  GetMethod = p.GetGetMethod()
                                  SetMethod = p.GetSetMethod() }))

and createSerializerMethodBody (il: ILGenerator) (typeMap: TypeMap) =
    let labelSerialize = il.DefineLabel()
    let labelReturn = il.DefineLabel()

    // if (value != null) {
    il.Emit(OpCodes.Ldarg_1)
    il.Emit(OpCodes.Ldnull)
    il.Emit(OpCodes.Ceq)
    il.Emit(OpCodes.Brfalse_S, labelSerialize)

    // writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true");
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldstr, "nil")
    il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
    il.Emit(OpCodes.Ldstr, "true")
    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteAttributeString("", "", "") @>)

    // return;
    il.Emit(OpCodes.Br, labelReturn)

    // }
    il.MarkLabel(labelSerialize)
    il.Emit(OpCodes.Nop)

    let rec callBase (typ: Type) =
        match findTypeMap typ.BaseType with
        | Some(typeMap) ->
            callBase typ.BaseType
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Call, typeMap.Serialization)
            il.Emit(OpCodes.Nop)
        | _ -> ()
    callBase typeMap.Type

    typeMap |> getPropertyMaps |> List.iter (emitPropertySerialization il)

    il.MarkLabel(labelReturn)
    il.Emit(OpCodes.Ret)

and getTypeMap(typ) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> createTypeMap typ

and findTypeMap (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(getTypeMap typ)

and findSubTypes (typ: Type) : TypeMap list =
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

let createSystemTypeMap<'X> (writeMethods: MemberInfo list) (readMethods: MemberInfo list) =
    let createTypeMap isNullable =
        let typ = if isNullable then typedefof<Nullable<_>>.MakeGenericType(typeof<'X>) else typeof<'X>
        let serializerMethod =
            let meth = createSerializerMethod typ false
            let il = meth.GetILGenerator()
            let labelEnd =
                if isNullable || typ.IsClass then
                    let label = il.DefineLabel()
                    let labelEnd = il.DefineLabel()
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldnull)
                    il.Emit(OpCodes.Ceq)
                    il.Emit(OpCodes.Brfalse_S, label)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldstr, "nil")
                    il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
                    il.Emit(OpCodes.Ldstr, "true")
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteAttributeString("", "", "") @>)
                    il.Emit(OpCodes.Br_S, labelEnd)
                    il.MarkLabel(label)
                    if typ = typeof<string> then
                        il.Emit(OpCodes.Ldarg_1)
                        il.Emit(OpCodes.Castclass, typ)
                        il.Emit(OpCodes.Ldstr, "")
                        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                        il.Emit(OpCodes.Brtrue_S, labelEnd)
                    Some(labelEnd)
                else None
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            writeMethods
            |> List.iter (fun mi ->
                match mi with
                | :? MethodInfo as mi -> il.Emit(OpCodes.Callvirt, mi)
                | _ -> failwith "not implemented"
                il.Emit(OpCodes.Nop))
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteValue(null: obj) @>)
            il.Emit(OpCodes.Nop)
            labelEnd |> Option.iter il.MarkLabel
            il.Emit(OpCodes.Ret)
            meth
        let deserializerMethod =
            let meth = createDeserializerMethod typ
            let il = meth.GetILGenerator()
            let labelRead = il.DefineLabel()
            let labelRet = il.DefineLabel()
            let labelCast = il.DefineLabel()
            if isNullable || typ.IsClass then
                emitNullCheck il labelRet
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).IsEmptyElement @>)
            il.Emit(OpCodes.Brfalse_S, labelRead)
            if typ = typeof<string> then
                il.Emit(OpCodes.Ldstr, "")
                il.Emit(OpCodes.Br_S, labelRet)
            else
                il.Emit(OpCodes.Ldtoken, typeof<'X>)
                il.Emit(OpCodes.Call, !@ <@ Type.GetTypeFromHandle(RuntimeTypeHandle()) @>)
                il.Emit(OpCodes.Call, !@ <@ Activator.CreateInstance(typeof<int>) @>)
                il.Emit(OpCodes.Unbox_Any, typeof<'X>)
                il.Emit(OpCodes.Br_S, labelCast)
            il.MarkLabel(labelRead)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Read() @>)
            il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Ldarg_0)
            readMethods
            |> List.iter (fun mi ->
                match mi with
                | :? MethodInfo as mi -> il.Emit(OpCodes.Callvirt, mi)
                | :? ConstructorInfo as ci -> il.Emit(OpCodes.Newobj, ci)
                | _ -> failwith "not implemented")
            il.MarkLabel(labelCast)
            if isNullable then il.Emit(OpCodes.Newobj, typ.GetConstructor([| typeof<'X> |]))
            if typ.IsValueType then il.Emit(OpCodes.Box, typ)
            il.MarkLabel(labelRet)
            il.Emit(OpCodes.Ret)
            meth
        let deserialization = { Root = deserializerMethod; Content = null; MatchType = null }
        typeMaps.TryAdd(typ, TypeMap.Create(typ, deserialization, serializerMethod)) |> ignore
    if typeof<'X>.IsValueType then createTypeMap true
    createTypeMap false

do
    createSystemTypeMap<bool>
        []
        [!@ <@ (null: XmlReader).ReadContentAsBoolean() @>]
    createSystemTypeMap<int32>
        []
        [!@ <@ (null: XmlReader).ReadContentAsInt() @>]
    createSystemTypeMap<int64>
        []
        [!@ <@ (null: XmlReader).ReadContentAsLong() @>]
    createSystemTypeMap<BigInteger>
        [!@ <@ (null: obj).ToString() @>]
        [!@ <@ (null: XmlReader).ReadContentAsDecimal() @>; !!@ <@ BigInteger(1M) @>]
    createSystemTypeMap<DateTime>
        []
        [!@ <@ (null: XmlReader).ReadContentAsDateTime() @>]
    createSystemTypeMap<string>
        []
        [!@ <@ (null: XmlReader).ReadContentAsString() @>]
